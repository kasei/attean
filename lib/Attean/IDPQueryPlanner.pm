use v5.14;
use warnings;

=head1 NAME

Attean::IDPQueryPlanner - Iterative dynamic programming query planner

=head1 VERSION

This document describes Attean::IDPQueryPlanner version 0.003

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $planner = Attean::IDPQueryPlanner->new();
  my $plan = $planner->plan_for_algebra( $algebra, $model, $dataset );
  my $e = Attean::QueryEvaluator->new();
  my $iter = $e->evaluate( $algebra, $active_graph );

=head1 DESCRIPTION

The Attean::IDPQueryPlanner class implements a query planner using the
iterative dynamic programming approach.

=head1 ATTRIBUTES

=over 4

=cut

use Attean::Algebra;
use Attean::Plan;
use Attean::Expression;

package Attean::IDPQueryPlanner 0.001 {
	use Moo;
	use Encode qw(encode);
	use Attean::RDF;
	use LWP::UserAgent;
	use Scalar::Util qw(blessed reftype);
	use List::Util qw(all any reduce);
	use Types::Standard qw(ConsumerOf InstanceOf);
	use URI::Escape;
	use Algorithm::Combinatorics qw(subsets);
	use List::Util qw(min);
	use namespace::clean;

	with 'Attean::API::CostPlanner';
	
=back

=head1 METHODS

=over 4

=item C<< plans_for_algebra( $algebra, $model, $active_graph ) >>

Returns L<Attean::API::Plan> objects representing alternate query plans for
evaluating the query C<< $algebra >> against the C<< $model >>, using
the supplied C<< $active_graph >>.

=cut

	sub plans_for_algebra {
		my $self			= shift;
		my $algebra			= shift;
		my $model			= shift;
		my $active_graph	= shift;
		
		if ($model->does('Attean::API::CostPlanner')) {
			my @plans	= $model->plans_for_algebra($algebra, $model, $active_graph, @_);
			if (@plans) {
				return @plans; # trust that the model knows better than us what plans are best
			}
		}
		
		Carp::confess "No algebra passed for evaluation" unless ($algebra);
		
		my @children	= @{ $algebra->children };
		my ($child)		= $children[0];
		if ($algebra->isa('Attean::Algebra::BGP')) {
			my @plans	= $self->_IDPJoin($model, $active_graph, map {
				[$self->access_plans($model, $active_graph, $_)]
			} @{ $algebra->triples });
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::Join')) {
			return $self->_IDPJoin($model, $active_graph, map {
				[$self->plans_for_algebra($_, $model, $active_graph, @_)]
			} @children);
		} elsif ($algebra->isa('Attean::Algebra::Distinct') or $algebra->isa('Attean::Algebra::Reduced')) {
			my @plans	= $self->plans_for_algebra($child, $model, $active_graph, @_);
			my @dist;
			foreach my $p (@plans) {
				if ($p->distinct) {
					push(@dist, $p);
				} else {
					# TODO: if the plan isn't distinct, but is ordered, we can use a batched implementation
					push(@dist, Attean::Plan::Distinct->new(children => [$p], distinct => 1, in_scope_variables => $p->in_scope_variables, ordered => $p->ordered));
				}
			}
			return @dist;
		} elsif ($algebra->isa('Attean::Algebra::Filter')) {
			# TODO: simple range relation filters can be handled differently if that filter operates on a variable that is part of the ordering
			my $expr	= $algebra->expression;
			my @plans	= map {
				Attean::Plan::Filter->new(children => [$_], expression => $expr, distinct => $_->distinct, in_scope_variables => $_->in_scope_variables, ordered => $_->ordered)
			} $self->plans_for_algebra($child, $model, $active_graph, @_);
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::OrderBy')) {
			# TODO: no-op if already ordered
			my @cmps	= map { Attean::Plan::Comparator->new(ascending => $_->ascending, expression => $_->expression) } @{ $algebra->comparators };
			my @plans	= map {
				Attean::Plan::OrderBy->new(children => [$_], comparators => \@cmps, distinct => $_->distinct, in_scope_variables => $_->in_scope_variables, ordered => \@cmps)
			} $self->plans_for_algebra($child, $model, $active_graph, @_);
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::LeftJoin')) {
			return $self->join_plans($model, $active_graph, @children[0,1], 1, 0, $algebra->expression);
		} elsif ($algebra->isa('Attean::Algebra::Minus')) {
			return $self->join_plans($model, $active_graph, @children[0,1], 0, 1);
		} elsif ($algebra->isa('Attean::Algebra::Project')) {
			my $vars	= $algebra->variables;
			my @vars	= map { $_->value } @{ $vars };
			my $vars_key	= join(' ', sort @vars);
			my @plans	= map {
				# TODO: compute the correct `ordered` array
				($vars_key eq join(' ', sort @{ $_->in_scope_variables }))
					? $_ # no-op if plan is already properly-projected
					: Attean::Plan::Project->new(children => [$_], variables => $vars, distinct => 0, in_scope_variables => \@vars, ordered => [])
			} $self->plans_for_algebra($child, $model, $active_graph, @_);
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::Graph')) {
			my $graph	= $algebra->graph;
			if ($graph->does('Attean::API::Term')) {
				return $self->plans_for_algebra($child, $model, $graph, @_);
			} else {
				die "handle GRAPH ?var query forms";
			}
		} elsif ($algebra->isa('Attean::Algebra::Extend')) {
		} elsif ($algebra->isa('Attean::Algebra::Service')) {
		} elsif ($algebra->isa('Attean::Algebra::Group')) {
		} elsif ($algebra->isa('Attean::Algebra::Path')) {
		} elsif ($algebra->isa('Attean::Algebra::Slice')) {
		} elsif ($algebra->isa('Attean::Algebra::Ask')) {
		} elsif ($algebra->isa('Attean::Algebra::Construct')) {
		} elsif ($algebra->isa('Attean::Algebra::Table')) {
		} elsif ($algebra->isa('Attean::Algebra::Union')) {
			# TODO: if both branches are similarly ordered, we can merge the branches and keep the ordering
		}
		die "Unimplemented algebra evaluation for: $algebra";
	}
	
	sub _IDPJoin {
		my $self			= shift;
		my $model			= shift;
		my $active_graph	= shift;
		my @args			= @_; # each $args[$i] here is an array reference containing alternate plans for element $i

		my $k				= 3; # this is the batch size over which to do full dynamic programming
		
		# initialize $optPlan{$i} to be a set of alternate plans for evaluating element $i
		my %optPlan;
		foreach my $i (0 .. $#args) {
			$optPlan{$i}	= [$self->prune_plans($model, @{ $args[$i] })];
		}
		
		my @todo	= (0 .. $#args); # initialize the todo list to all elements
		my $next_symbol	= 'a'; # when we start batching together sub-plans, we'll rename them with letters (e.g. elements 1, 2, and 4 might become 'a', and then 3, 5, and 'a' become 'b')
		
		# until we've joined all the elements in todo and are left with a set of plans for the join of all elements
		while (scalar(@todo) > 1) {
			$k	= ($k < scalar(@todo)) ? $k : scalar(@todo); # in case we're joining fewer than the batch size
			foreach my $i (2 .. $k) { # we've already initialized plans for evaluating single elements; now consider plans for groups of elements (with group sizes 2, 3, ..., $k)
				foreach my $s (subsets(\@todo, $i)) { # pick a subset of size $i of the elements that need to be planned
					my $s_key	= join('.', sort @$s);
					$optPlan{$s_key}	= [];
					foreach my $o (subsets($s)) { # partition the subset s into two (o and not_o)
						next if (scalar(@$o) == 0); # only consider proper, non-empty subsets
						next if (scalar(@$o) == scalar(@$s)); # only consider proper, non-empty subsets
						my $o_key	= join('.', sort @$o);
						my %o		= map { $_ => 1 } @$o;
						my $not_o_key	= join('.', sort grep { not exists $o{$_} } @$s);
						
						my $lhs		= $optPlan{$o_key}; # get the plans for evaluating o
						my $rhs		= $optPlan{$not_o_key}; # get the plans for evaluating not_o
						
						# compute and store all the possible ways to evaluate s (o ⋈ not_o)
						push(@{ $optPlan{$s_key} }, $self->join_plans($model, $active_graph, $lhs, $rhs, 0, 0));
						$optPlan{$s_key}	= [$self->prune_plans($model, @{ $optPlan{$s_key} })];
					}
				}
			}
			
			# find the minimum cost plan $p that computes the join over $k elements (the elements end up in @v)
			my %min_plans;
			foreach my $w (subsets(\@todo, $k)) {
				my $w_key	= join('.', sort @$w);
				my $plans	= $optPlan{$w_key};
				my %costs	= map { $self->cost_for_plan($_, $model) => [$_, $w] } @$plans;
				my $min		= min keys %costs;
				my $min_plan	= $costs{ $min };
				$min_plans{ $min }	= $min_plan;
			}
			my $min_cost	= min keys %min_plans;
			my $min_plan	= $min_plans{$min_cost};
			my ($p, $v)		= @$min_plan;
			my $v_key		= join('.', sort @$v);
# 			warn "Choosing join for $v_key\n";
			
			# generate a new symbol $t to stand in for $p, the join over the elements in @v
			my $t	= $next_symbol++;
			
			# remove elements in @v from the todo list, and replace them by the new composite element $t
			$optPlan{$t}	= [$p];
			my %v	= map { $_ => 1 } @$v;
			push(@todo, $t);
			@todo	= grep { not exists $v{$_} } @todo;
			
			# also remove subsets of @v from the optPlan hash as they are now covered by $optPlan{$t}
			foreach my $o (subsets($v)) {
				my $o_key	= join('.', sort @$o);
# 				warn "deleting $o_key\n";
				delete $optPlan{$o_key};
			}
		}
		
		my $final_key	= join('.', sort @todo);
		return $self->prune_plans($model, @{ $optPlan{$final_key} });
	}
	
	sub prune_plans {
		my $self	= shift;
		my $model	= shift;
		my @sorted	= map { $_->[1] } sort { $a->[0] <=> $b->[0] } map { [$self->cost_for_plan($_, $model), $_] } @_;
		return splice(@sorted, 0, 5);
	}
	
	# $pattern is a Attean::API::TripleOrQuadPattern object
	# Return a Attean::API::Plan object that represents the evaluation of $pattern.
	# e.g. different plans might represent different ways of producing the matches (table scan, index match, etc.)
	sub access_plans {
		my $self			= shift;
		my $model			= shift;
		my $active_graph	= shift;
		my $pattern			= shift;
		my @vars			= map { $_->value } grep { $_->does('Attean::API::Variable') } $pattern->values;
		my %vars;
		my $dup				= 0;
		foreach my $v (@vars) {
			$dup++ if ($vars{$v}++);
		}
		my $distinct		= not($dup);
		return Attean::Plan::Quad->new( quad => $pattern->as_quadpattern($active_graph), distinct => $distinct, in_scope_variables => \@vars, ordered => [] );
	}
	
	# $lhs and $rhs are both Attean::API::Plan objects
	# Return a Attean::API::Plan object that represents the evaluation of $lhs ⋈ $rhs.
	# The $left and $minus flags indicate the type of the join to be performed (⟕ and ▷, respectively).
	# e.g. different plans might represent different join algorithms (nested loop join, hash join, etc.) or different orderings ($lhs ⋈ $rhs or $rhs ⋈ $lhs)
	sub join_plans {
		my $self			= shift;
		my $model			= shift;
		my $active_graph	= shift;
		my $lplans			= shift;
		my $rplans			= shift;
		my $left			= shift;
		my $minus			= shift;
		my $expr			= shift;
		
		my @plans;
		Carp::confess unless (reftype($lplans) eq 'ARRAY');
		foreach my $lhs (@{ $lplans }) {
			foreach my $rhs (@{ $rplans }) {
				my @vars	= (@{ $lhs->in_scope_variables }, @{ $rhs->in_scope_variables });
				my %vars;
				foreach my $v (@vars) {
					$vars{$v}++;
				}
				my %join_vars;
				foreach my $l (@{ $lhs->in_scope_variables }) {
					foreach my $r (@{ $rhs->in_scope_variables }) {
						if ($l eq $r) {
							$join_vars{$l}++;
						}
					}
				}
				my @join_vars	= keys %join_vars;
		
				if ($left) {
					push(@plans, Attean::Plan::NestedLoopLeftJoin->new(children => [$lhs, $rhs], expression => $expr, join_variables => \@join_vars, distinct => 0, in_scope_variables => [keys %vars], ordered => $lhs->ordered));
					if (scalar(@join_vars) > 0) {
						push(@plans, Attean::Plan::HashLeftJoin->new(children => [$lhs, $rhs], expression => $expr, join_variables => \@join_vars, distinct => 0, in_scope_variables => [keys %vars], ordered => []));
					}
				} elsif ($minus) {
					push(@plans, Attean::Plan::NestedLoopAntiJoin->new(children => [$lhs, $rhs], join_variables => \@join_vars, distinct => 0, in_scope_variables => [keys %vars], ordered => $lhs->ordered));
					if (scalar(@join_vars) > 0) {
						push(@plans, Attean::Plan::HashAntiJoin->new(children => [$lhs, $rhs], join_variables => \@join_vars, join_variables => \@join_vars, distinct => 0, in_scope_variables => [keys %vars], ordered => []));
					}
				} else {
					# nested loop joins work in all cases
					push(@plans, Attean::Plan::NestedLoopJoin->new(children => [$lhs, $rhs], join_variables => \@join_vars, distinct => 0, in_scope_variables => [keys %vars], ordered => $lhs->ordered));
					push(@plans, Attean::Plan::NestedLoopJoin->new(children => [$rhs, $lhs], join_variables => \@join_vars, distinct => 0, in_scope_variables => [keys %vars], ordered => $rhs->ordered));
			
					if (scalar(@join_vars) > 0) {
						# if there's shared variables (hopefully), we can also use a hash join
						push(@plans, Attean::Plan::HashJoin->new(children => [$lhs, $rhs], join_variables => \@join_vars, distinct => 0, in_scope_variables => [keys %vars], ordered => []));
						push(@plans, Attean::Plan::HashJoin->new(children => [$rhs, $lhs], join_variables => \@join_vars, distinct => 0, in_scope_variables => [keys %vars], ordered => []));
					}
				}
			}
		}
		return @plans;
	}
	
	# Return a cost value for $plan. This value is basically opaque, except
	# that it will be used to sort plans by cost when determining which is the
	# cheapest plan to evaluate.
	sub cost_for_plan {
		my $self	= shift;
		my $plan	= shift;
		my $model	= shift;

		if ($plan->has_cost) {
			return $plan->cost;
		} else {
			if ($model->does('Attean::API::CostPlanner')) {
				if (defined(my $cost = $model->cost_for_plan($plan, $model))) {
					return $cost;
				}
			}

			my $cost	= 1;
			my @children	= @{ $plan->children };
			if ($plan->isa('Attean::Plan::Quad')) {
				my @vars	= map { $_->value } grep { $_->does('Attean::API::Variable') } $plan->quad->values;
				return 3 * scalar(@vars);
			} elsif (ref($plan) =~ /^Attean::Plan::NestedLoop\w*Join/) {
				my $jv			= $plan->join_variables;
				my $mult		= scalar(@$jv) ? 1 : 5;	# penalize cartesian joins
				my $lcost		= $self->cost_for_plan($children[0], $model);
				my $rcost		= $self->cost_for_plan($children[1], $model);
				$cost			= $mult * $lcost * $rcost;
			} elsif (ref($plan) =~ /^Attean::Plan::Hash\w*Join/) {
				my $jv			= $plan->join_variables;
				my $mult		= scalar(@$jv) ? 1 : 5;	# penalize cartesian joins
				my $lcost		= $self->cost_for_plan($children[0], $model);
				my $rcost		= $self->cost_for_plan($children[1], $model);
				$cost			= $mult * ($lcost + $rcost);
			} else {
				foreach my $c (@{ $plan->children }) {
					$cost	+= $self->cost_for_plan($c, $model);
				}
			}
			
			$plan->cost($cost);
			return $cost;
		}
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO

L<http://www.perlrdf.org/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
