use v5.14;
use warnings;

=head1 NAME

Attean::API::IDPJoinPlanner - Iterative dynamic programming query planning role

=head1 VERSION

This document describes Attean::API::IDPJoinPlanner version 0.023

=head1 SYNOPSIS

  extends 'Attean::QueryPlanner';
  with 'Attean::API::IDPJoinPlanner';

=head1 DESCRIPTION

The Attean::API::IDPJoinPlanner role provides a query planner the
C<< joins_for_plan_alternatives >> method, as well as the cost estimation
methods that consume the L<Attean::API::CostPlanner> role.

=head1 ATTRIBUTES

=over 4

=back

=head1 METHODS

=over 4

=cut

package Attean::API::QueryPlanner 0.023 {
	use Types::Standard qw(CodeRef);

	use Moo::Role;
	
	requires 'plan_for_algebra'; # plan_for_algebra($algebra, $model, \@default_graphs)
}

package Attean::API::CostPlanner 0.023 {
	use Scalar::Util qw(refaddr);
	use Types::Standard qw(CodeRef);

	use Moo::Role;
	use namespace::clean;
	with 'Attean::API::QueryPlanner';
	
	requires 'plans_for_algebra'; # plans_for_algebra($algebra, $model, \@active_graphs, \@default_graphs)
	requires 'cost_for_plan'; # cost_for_plan($plan, $model)
	
	before 'cost_for_plan' => sub {
		my $self	= shift;
		my $plan	= shift;
		my $model	= shift;
		
		if (refaddr($self) == refaddr($model)) {
			Carp::confess "Model and planner objects cannot be the same in call to cost_for_plan";
		} elsif ($self->does('Attean::API::Model') and $model->does('Attean::API::Model')) {
			Carp::confess "Model and planner objects cannot both consume Attean::API::Model in call to cost_for_plan";
		}
	};
	
	sub plan_for_algebra {
		my $self			= shift;
		my $algebra			= shift;
		my $model			= shift;
		my $default_graphs	= shift;
		my $active_graphs	= $default_graphs;
		my @plans			= sort { $self->cost_for_plan($a, $model) <=> $self->cost_for_plan($b, $model) } $self->plans_for_algebra($algebra, $model, $active_graphs, $default_graphs);
		my $plan			= shift(@plans);
		return $plan;
	}
}

package Attean::API::JoinPlanner 0.023 {
	use Moo::Role;
	requires 'joins_for_plan_alternatives';
}

package Attean::API::NaiveJoinPlanner 0.023 {
	use Math::Cartesian::Product;

	use Moo::Role;

	with 'Attean::API::JoinPlanner';
	with 'Attean::API::QueryPlanner';

	sub joins_for_plan_alternatives {
		my $self			= shift;
		my $model			= shift;
		my $active_graphs	= shift;
		my $default_graphs	= shift;
		my $interesting		= shift;
		my @args			= @_; # each $args[$i] here is an array reference containing alternate plans for element $i
		
		my $plans	= shift(@args);
		while (scalar(@args)) {
			my $next	= shift(@args);
			my @plans	= $self->join_plans($model, $active_graphs, $default_graphs, $plans, $next, 'inner');
			$plans		= \@plans;
		}
		
		my @plans	= @$plans;
		return @plans;
	}
}

package Attean::API::SimpleCostPlanner 0.023 {
	use Types::Standard qw(Int);
	use Scalar::Util qw(blessed);

	use Moo::Role;

	with 'Attean::API::CostPlanner';
	with 'MooX::Log::Any';

	has 'keep' => (is => 'ro', isa => Int, default => 5);
	
	around 'joins_for_plan_alternatives' => sub {
		my $orig	= shift;
		my $self	= shift;
		my $model			= shift;
		my @plans	= $orig->($self, $model, @_);
		return $self->prune_plans($model, [], \@plans);
	};
	
	sub prune_plans {
		my $self		= shift;
		my $model		= shift;
		my $interesting	= shift;
		my @plans		= @{ shift || [] };
		no  sort 'stable';
		my @sorted	= map { $_->[1] } sort { $a->[0] <=> $b->[0] } map { [$self->cost_for_plan($_, $model), $_] } @plans;
		
		return ($self->keep) ? splice(@sorted, 0, $self->keep) : @sorted;
	}
	
	sub cost_for_plan {
		my $self	= shift;
		my $plan	= shift;
		my $model	= shift;
		Carp::confess "No model given" unless (blessed($model) and $model->does('Attean::API::Model'));
		
		if ($plan->has_cost) {
			return $plan->cost;
		} else {
			if ($model->does('Attean::API::CostPlanner')) {
				if (defined(my $cost = $model->cost_for_plan($plan, $self))) {
					$plan->cost($cost);
					$self->log->info('Model \''.ref($model).'\' did cost planning for \''.ref($plan).'\' and got cost '.$cost);
					return $cost;
				}
			}

			my $cost	= 1;
			my @children	= @{ $plan->children };
			if ($plan->isa('Attean::Plan::Quad')) {
				my @vars	= map { $_->value } grep { blessed($_) and $_->does('Attean::API::Variable') } $plan->values;
				return scalar(@vars);
			} elsif ($plan->isa('Attean::Plan::Table')) {
				my $rows	= $plan->rows;
				$cost		= scalar(@$rows);
			} elsif ($plan->isa('Attean::Plan::NestedLoopJoin')) {
				my $lcost		= $self->cost_for_plan($children[0], $model);
				my $rcost		= $self->cost_for_plan($children[1], $model);
				if ($lcost == 0) {
					$cost	= $rcost;
				} elsif ($rcost == 0) {
					$cost	= $lcost;
				} else {
					$cost	= $lcost * $rcost;
				}
				
				# a cartesian nested loop join is bad, but the algorithm already
				# has to check for all possible joins, so it's not as bad as
				# a cartesian hash join (below)
				$cost	*= 10 unless ($plan->children_are_variable_connected);
			} elsif ($plan->isa('Attean::Plan::HashJoin')) {
				my $joined		= $plan->children_are_variable_connected;
				my $lcost		= $self->cost_for_plan($children[0], $model);
				my $rcost		= $self->cost_for_plan($children[1], $model);
				$cost	= ($lcost + $rcost);
				$cost += ($lcost < $rcost); # To let the plan with cheaper rhs win
				$cost	*= 100 unless ($plan->children_are_variable_connected);
			} elsif ($plan->isa('Attean::Plan::Service')) {
				my $scost	= 10;
				foreach my $c (@{ $plan->children }) {
					$scost	+= $self->cost_for_plan($c, $model);
				}
				$cost	= 5 * $scost;
			} elsif ($plan->isa('Attean::Plan::Unique')) {
				$cost	= 0; # consider a filter on the iterator (like unique) to be essentially free
				foreach my $c (@{ $plan->children }) {
					$cost	+= $self->cost_for_plan($c, $model);
				}
			} else {
				foreach my $c (@{ $plan->children }) {
					$cost	+= $self->cost_for_plan($c, $model);
				}
			}
			
			$plan->cost($cost);
			if ($self->log->is_trace) {
				$self->log->trace("Cost $cost estimated for\n".$plan->as_string);
			} else {
				$self->log->debug('Estimated cost for \''.ref($plan).'\' is '.$cost);
			}
			return $cost;
		}
	}
}

package Attean::API::IDPJoinPlanner 0.023 {
	use Encode qw(encode);
	use Attean::RDF;
	use LWP::UserAgent;
	use Scalar::Util qw(blessed reftype);
	use List::Util qw(reduce);
	use List::MoreUtils qw(all any);
	use Types::Standard qw(Int ConsumerOf InstanceOf);
	use URI::Escape;
	use Algorithm::Combinatorics qw(subsets);
	use List::Util qw(min);
	use Math::Cartesian::Product;

	use Moo::Role;

	with 'Attean::API::JoinPlanner';
	with 'Attean::API::SimpleCostPlanner';

	sub joins_for_plan_alternatives {
		my $self			= shift;
		my $model			= shift;
		my $active_graphs	= shift;
		my $default_graphs	= shift;
		my $interesting		= shift;
		my @args			= @_; # each $args[$i] here is an array reference containing alternate plans for element $i

		my $k				= 3; # this is the batch size over which to do full dynamic programming
		
		# initialize $optPlan{$i} to be a set of alternate plans for evaluating element $i
		my %optPlan;
		foreach my $i (0 .. $#args) {
			$optPlan{$i}	= [$self->prune_plans($model, $interesting, $args[$i])];
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
						push(@{ $optPlan{$s_key} }, $self->join_plans($model, $active_graphs, $default_graphs, $lhs, $rhs, 'inner'));
						$optPlan{$s_key}	= [$self->prune_plans($model, $interesting, $optPlan{$s_key})];
					}
				}
			}
			
			# find the minimum cost plan $p that computes the join over $k elements (the elements end up in @v)
			my %min_plans;
			foreach my $w (subsets(\@todo, $k)) {
				my $w_key		= join('.', sort @$w);
				my $plans		= $optPlan{$w_key};
				my @costs		= map { $self->cost_for_plan($_, $model) => [$_, $w] } @$plans;
				my %costs		= @costs;
				my $min			= min keys %costs;
				my @min_plans;
				while (my ($cost, $data) = splice(@costs, 0, 2)) {
					if ($cost == $min) {
						push(@min_plans, $data);
					}
				}
				$min_plans{ $min }	= \@min_plans;
			}
			my $min_cost	= min keys %min_plans;
			my $min_plans	= $min_plans{$min_cost};
			my @min_plans;
			my $min_key;
			foreach my $d (@$min_plans) {
				my ($p, $v)		= @$d;
				my $v_key		= join('.', sort @$v);
				if (not(defined($min_key)) or $min_key eq $v_key) {
					push(@min_plans, $p);
					$min_key	= $v_key;
				}
			}
# 			my ($p, $v)		= @$min_plan;
# 			my $v_key		= join('.', sort @$v);
# 			warn "Choosing join for $v_key\n";
			
			# generate a new symbol $t to stand in for $p, the join over the elements in @v
			my $t	= $next_symbol++;
			
			# remove elements in @v from the todo list, and replace them by the new composite element $t
			$optPlan{$t}	= [@min_plans];
			my %v	= map { $_ => 1 } split(/[.]/, $min_key);
			push(@todo, $t);
			@todo	= grep { not exists $v{$_} } @todo;
			
			# also remove subsets of @v from the optPlan hash as they are now covered by $optPlan{$t}
			foreach my $o (subsets([keys %v])) {
				my $o_key	= join('.', sort @$o);
# 				warn "deleting $o_key\n";
				delete $optPlan{$o_key};
			}
		}
		
		my $final_key	= join('.', sort @todo);
# 		use Data::Dumper;
# 		warn Dumper($optPlan{$final_key});
		return $self->prune_plans($model, $interesting, $optPlan{$final_key});
	}
	
	sub prune_plans {
		my $self		= shift;
		my $model		= shift;
		my $interesting	= shift;
		my @plans		= @{ shift || [] };
		no  sort 'stable';
		my @sorted	= map { $_->[1] } sort { $a->[0] <=> $b->[0] } map { [$self->cost_for_plan($_, $model), $_] } @plans;
		if ($self->log->is_trace) {
			$self->log->trace('============= Plan iteration separator ==============');
			foreach my $plan (@sorted){
				$self->log->trace("Cost: " . $self->cost_for_plan($plan, $model) . " for plan:\n". $plan->as_string);
			}
		}
		return splice(@sorted, 0, 5);
	}
	
	# Return a cost value for $plan. This value is basically opaque, except
	# that it will be used to sort plans by cost when determining which is the
	# cheapest plan to evaluate.
	sub cost_for_plan {
		my $self	= shift;
		my $plan	= shift;
		my $model	= shift;
		Carp::confess "No model given" unless (blessed($model) and $model->does('Attean::API::Model'));
		
		if ($plan->has_cost) {
			return $plan->cost;
		} else {
			if ($model->does('Attean::API::CostPlanner')) {
				if (defined(my $cost = $model->cost_for_plan($plan, $self))) {
					$plan->cost($cost);
					$self->log->info('Model \''.ref($model).'\' did cost planning for \''.ref($plan).'\' and got cost '.$cost);
					return $cost;
				}
			}

			my $cost	= 1;
			my @children	= @{ $plan->children };
			if ($plan->isa('Attean::Plan::Quad')) {
				my @vars	= map { $_->value } grep { blessed($_) and $_->does('Attean::API::Variable') } $plan->values;
				# This gives a cost increasing at a reasonable pace
				$cost	= $self->_hsp_heuristic_triple_sum($plan) * scalar(@vars);
			} elsif ($plan->isa('Attean::Plan::Table')) {
				my $rows	= $plan->rows;
				$cost		= scalar(@$rows);
			} elsif ($plan->isa('Attean::Plan::NestedLoopJoin')) {
				my $lcost		= $self->cost_for_plan($children[0], $model);
				my $rcost		= $self->cost_for_plan($children[1], $model);
				if ($lcost == 0) {
					$cost	= $rcost;
				} elsif ($rcost == 0) {
					$cost	= $lcost;
				} else {
					my $mult = $self->_penalize_joins($plan);
# 					warn "$mult * ($lcost * $rcost) [$children[0] $children[1]]";
					$cost	= $mult * $lcost * $rcost;
				}
			} elsif ($plan->isa('Attean::Plan::HashJoin')) {
				my $lcost		= $self->cost_for_plan($children[0], $model);
				my $rcost		= $self->cost_for_plan($children[1], $model);
				if ($lcost == 0) {
					$cost	= $rcost;
				} elsif ($rcost == 0) {
					$cost	= $lcost;
				} else {
					my $mult = $self->_penalize_joins($plan);
# 					warn "$mult * ($lcost + $rcost)";
					$cost	= $mult * ($lcost + $rcost);
					$cost += ($lcost < $rcost); # To let the plan with cheaper rhs win
				}
			} elsif ($plan->isa('Attean::Plan::Service')) {
				my $scost	= 10;
				foreach my $c (@{ $plan->children }) {
					$scost	+= $self->cost_for_plan($c, $model);
				}
				$cost	= 5 * $scost;
			} elsif ($plan->isa('Attean::Plan::Unique')) {
				$cost	= 0; # consider a filter on the iterator (like unique) to be essentially free
				foreach my $c (@{ $plan->children }) {
					$cost	+= $self->cost_for_plan($c, $model);
				}
			} else {
				foreach my $c (@{ $plan->children }) {
					$cost	+= $self->cost_for_plan($c, $model);
				}
			}
			
			# Costs must be integers for comparisons to work in the IDP planning algorithm
			$cost	= int($cost);
			$plan->cost($cost);
			return $cost;
		}
	}
	
	
# The below function finds a number to aid sorting
# It takes into account Heuristic 1 and 4 of the HSP paper, see REFERENCES
# as well as that it was noted in the text that rdf:type is usually less selective.

# By assigning the integers to nodes, depending on whether they are in
# triple (subject, predicate, object), variables, rdf:type and
# literals, and sum them, they may be sorted. See code for the actual
# values used.

# Denoting s for bound subject, p for bound predicate, a for rdf:type
# as predicate, o for bound object and l for literal object and ? for
# variable, we get the following order, most of which are identical to
# the HSP:

# spl: 6
# spo: 8
# sao: 10
# s?l: 14
# s?o: 16
# ?pl: 25
# ?po: 27
# ?ao: 29
# sp?: 30
# sa?: 32
# ??l: 33
# ??o: 35
# s??: 38
# ?p?: 49
# ?a?: 51
# ???: 57

# Note that this number is not intended as an estimate of selectivity,
# merely a sorting key, but further research may possibly create such
# numbers.

	sub _hsp_heuristic_triple_sum {
		my ($self, $t) = @_;
		my $sum = 0;
		if ($t->subject->does('Attean::API::Variable')) {
			$sum = 20;
		} else {
			$sum = 1;
		}
		if ($t->predicate->does('Attean::API::Variable')) {
			$sum += 10;
		} else {
			if ($t->predicate->equals(iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type'))) {
				$sum += 4;
			} else {
				$sum += 2;
			}
		}
		if ($t->object->does('Attean::API::Variable')) {
			$sum += 27;
		} elsif ($t->object->does('Attean::API::Literal')) {
			$sum += 3;
		} else {
			$sum += 5;
		}
		return $sum;
	}

	# The following method returns a factor used to penalize certain types of joins.
	# It penalizes cartesian joins heavily, but also uses HSP Heuristic 2 (see REFERENCES)
	sub _penalize_joins {
		my ($self, $plan) = @_;
		my $jv			= $plan->join_variables;
		my @children	= @{ $plan->children };
		my $mult		   = 1;
		if (scalar(@$jv)) {
			if ( all { $_->isa('Attean::Plan::Quad') } @children[0..1]) {
				my $var = ${$jv}[0]; # We will join on this
				my @lnodes = $children[0]->values;
				my @rnodes = $children[1]->values;
				# Now, find where the join variables are in the triple patterns
				my %joinpos;
				for (my $i = 0; $i <= 2; $i++) {
					if ($lnodes[$i]->does('Attean::API::Variable') && $lnodes[$i]->value eq $var) {
						$joinpos{l} = $i;
					}
					if ($rnodes[$i]->does('Attean::API::Variable') && $rnodes[$i]->value eq $var) {
						$joinpos{r} = $i;
					}
					last if scalar keys(%joinpos) >= 2; # Perhaps a bit premature optimization
				}
				my $joinpos = join("", sort values(%joinpos)); # We can now match on this string
				my %costs = ('12' => 1.1, # The penalty numbers come mostly out from thin air
								 '01' => 1.2,
								 '02' => 1.5,
								 '22' => 1.6,
								 '00' => 1.8,
								 '11' => 2);
				if (exists $costs{$joinpos}) {
					$mult = $costs{$joinpos};
				}
				#warn "Penalty: $mult for quads:\n" . $children[0]->as_string . $children[1]->as_string
			}
		} else {
			$mult = 5; # penalize cartesian joins
		}
		return $mult;
	}
}


1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 REFERENCES

The seminal reference for Iterative Dynamic Programming is "Iterative
dynamic programming: a new class of query optimization algorithms" by
D. Kossmann and K. Stocker, ACM Transactions on Database Systems
(2000).

The heuristics to order triple patterns in this module is
influenced by L<The ICS-FORTH Heuristics-based SPARQL Planner
(HSP)|http://www.ics.forth.gr/isl/index_main.php?l=e&c=645>.


=head1 SEE ALSO



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2019 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
