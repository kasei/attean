use v5.14;
use warnings;

=head1 NAME

Attean::QueryPlanner - Query planner

=head1 VERSION

This document describes Attean::QueryPlanner version 0.035

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $planner = Attean::QueryPlanner->new();
  my $default_graphs = [ Attean::IRI->new('http://example.org/') ];
  my $plan = $planner->plan_for_algebra( $algebra, $model, $default_graphs );
  my $iter = $plan->evaluate($model);
  while (my $result = $iter->next()) {
    say $result->as_string;
  }

=head1 DESCRIPTION

The Attean::QueryPlanner class is a base class implementing common behavior for
query planners. Subclasses will need to consume or compose the
L<Attean::API::JoinPlanner> role.

Trivial sub-classes may consume L<Attean::API::NaiveJoinPlanner>, while more
complex planners may choose to implement complex join planning (e.g.
L<Attean::IDPQueryPlanner>).

=head1 ATTRIBUTES

=over 4

=cut

use Attean::Algebra;
use Attean::Plan;
use Attean::Expression;

package Attean::QueryPlanner 0.035 {
	use Moo;
	use Encode qw(encode);
	use Attean::RDF;
	use Scalar::Util qw(blessed reftype);
	use List::Util qw(reduce);
	use List::Util qw(all any);
	use Types::Standard qw(Int ConsumerOf InstanceOf);
	use URI::Escape;
	use Algorithm::Combinatorics qw(subsets);
	use List::Util qw(min);
	use Math::Cartesian::Product;
	use namespace::clean;

	with 'Attean::API::QueryPlanner', 'MooX::Log::Any';
	has 'counter' => (is => 'rw', isa => Int, default => 0);
	has 'table_threshold'	=> (is => 'rw', isa => Int, default => 10);

=back

=head1 METHODS

=over 4

=item C<< new_temporary( $type ) >>

Returns a new unique (in the context of the query planner) ID string that may
be used for things like fresh (temporary) variables. The C<< $type >> string is
used in the generated name to aid in identifying different uses for the names.

=cut

	sub new_temporary {
		my $self	= shift;
		my $type	= shift;
		my $c		= $self->counter;
		$self->counter($c+1);
		return sprintf('.%s-%d', $type, $c);
	}

=item C<< plan_for_algebra( $algebra, $model, \@active_graphs, \@default_graphs ) >>

Returns the first plan returned from C<< plans_for_algebra >>.

=cut

	sub plan_for_algebra {
		my $self			= shift;
		my @plans			= $self->plans_for_algebra(@_);
		return shift(@plans);
	}
	
=item C<< plans_for_algebra( $algebra, $model, \@active_graphs, \@default_graphs ) >>

Returns L<Attean::API::Plan> objects representing alternate query plans for
evaluating the query C<< $algebra >> against the C<< $model >>, using
the supplied C<< $active_graph >>.

=cut

	sub plans_for_algebra {
		my $self			= shift;
		my $algebra			= shift;
		my $model			= shift;
		my $active_graphs	= shift;
		my $default_graphs	= shift;
		my %args			= @_;
		
		if ($model->does('Attean::API::CostPlanner')) {
			my @plans	= $model->plans_for_algebra($algebra, $self, $active_graphs, $default_graphs, %args);
			if (@plans) {
				return @plans; # trust that the model knows better than us what plans are best
			} else {
 				$self->log->info("*** Model did not provide plans: $model");
			}
		}
		
		Carp::confess "No algebra passed for evaluation" unless ($algebra);
		
		# TODO: propagate interesting orders
		my $interesting	= [];
		
		my @children	= @{ $algebra->children };
		my ($child)		= $children[0];
		if ($algebra->isa('Attean::Algebra::Query') or $algebra->isa('Attean::Algebra::Update')) {
			return $self->plans_for_algebra($algebra->child, $model, $active_graphs, $default_graphs, %args);
		} elsif ($algebra->isa('Attean::Algebra::BGP')) {
			my $triples	= $algebra->triples;
			my @triples	= @$triples;
			my %blanks;
			foreach my $i (0 .. $#triples) {
				my $t	= $triples[$i];
				my @nodes	= $t->values;
				my $changed	= 0;
				foreach (@nodes) {
					if ($_->does('Attean::API::Blank')) {
						$changed++;
						my $id	= $_->value;
						unless (exists $blanks{$id}) {
							$blanks{$id}	= Attean::Variable->new(value => $self->new_temporary('blank'));
						}
						$_	= $blanks{$id};
					}
				}
				
				if ($changed) {
					my $new	= Attean::TriplePattern->new(@nodes);
					$triples[$i]	= $new;
				}
			}
			my $bgp		= Attean::Algebra::BGP->new( triples => \@triples );
			my @plans	= $self->bgp_join_plans($bgp, $model, $active_graphs, $default_graphs, $interesting, map {
				[$self->access_plans($model, $active_graphs, $_)]
			} @triples);
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::Join')) {
			return $self->group_join_plans($model, $active_graphs, $default_graphs, $interesting, map {
				[$self->plans_for_algebra($_, $model, $active_graphs, $default_graphs, %args)]
			} @children);
		} elsif ($algebra->isa('Attean::Algebra::Distinct') or $algebra->isa('Attean::Algebra::Reduced')) {
			my @plans	= $self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, %args);
			my @dist;
			foreach my $p (@plans) {
				if ($p->distinct) {
					push(@dist, $p);
				} else {
					my @vars	= @{ $p->in_scope_variables };
					my $cmps	= $p->ordered;
					if ($self->_comparators_are_stable_and_cover_vars($cmps, @vars)) {
						# the plan has a stable ordering which covers all the variables, so we can just uniq the iterator
						push(@dist, Attean::Plan::Unique->new(children => [$p], distinct => 1, ordered => $p->ordered));
					} else {
						# TODO: if the plan isn't distinct, but is ordered, we can use a batched implementation
						push(@dist, Attean::Plan::HashDistinct->new(children => [$p], distinct => 1, ordered => $p->ordered));
					}
				}
			}
			return @dist;
		} elsif ($algebra->isa('Attean::Algebra::Filter')) {
			# TODO: simple range relation filters can be handled differently if that filter operates on a variable that is part of the ordering
			my $expr	= $algebra->expression;
			my $w	= Attean::TreeRewriter->new(types => ['Attean::API::DirectedAcyclicGraph']);
			$w->register_pre_handler(sub {
				my ($t, $parent, $thunk)	= @_;
				if ($t->isa('Attean::ExistsExpression')) {
					my $pattern	= $t->pattern;
					my $plan	= $self->plan_for_algebra($pattern, $model, $active_graphs, $default_graphs, @_);
					unless ($plan->does('Attean::API::BindingSubstitutionPlan')) {
						die 'Exists plan does not consume Attean::API::BindingSubstitutionPlan: ' . $plan->as_string;
					}
					my $new	= Attean::ExistsPlanExpression->new(
						plan => $plan,
					);
					return (1, 0, $new);
				}
				return (0, 1, $t);
			});
			my ($changed, $rewritten)	= $w->rewrite($expr, {});
			if ($changed) {
				$expr	= $rewritten;
			}
			
			my $var		= $self->new_temporary('filter');
			my %exprs	= ($var => $expr);
		
			my @plans;
			foreach my $plan ($self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, %args)) {
				my $distinct	= $plan->distinct;
				my $ordered		= $plan->ordered;
				if ($expr->isa('Attean::ValueExpression') and $expr->value->does('Attean::API::Variable')) {
					my $filtered	= Attean::Plan::EBVFilter->new(children => [$plan], variable => $expr->value->value, distinct => $distinct, ordered => $ordered);
					push(@plans, $filtered);
				} else {
					my @vars		= ($var);
					my @inscope		= ($var, @{ $plan->in_scope_variables });
					my @pvars		= map { Attean::Variable->new($_) } @{ $plan->in_scope_variables };
					my $extend		= Attean::Plan::Extend->new(children => [$plan], expressions => \%exprs, distinct => 0, ordered => $ordered, active_graphs => $active_graphs);
					my $filtered	= Attean::Plan::EBVFilter->new(children => [$extend], variable => $var, distinct => 0, ordered => $ordered);
					my $proj		= $self->new_projection($filtered, $distinct, @{ $plan->in_scope_variables });
					push(@plans, $proj);
				}
			}
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::OrderBy')) {
			# TODO: no-op if already ordered
			my @cmps	= @{ $algebra->comparators };
			my ($exprs, $ascending, $svars)	= $self->_order_by($algebra);
			my @plans;
			foreach my $plan ($self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, interesting_order => $algebra->comparators, %args)) {
				my $distinct	= $plan->distinct;

				if (scalar(@cmps) == 1 and $cmps[0]->expression->isa('Attean::ValueExpression') and $cmps[0]->expression->value->does('Attean::API::Variable')) {
					# TODO: extend this to handle more than one comparator, so long as they are *all* just variables (and not complex expressions)
					# If we're sorting by just a variable name, don't bother creating new variables for the sort expressions, use the underlying variable directy
					my @vars	= @{ $plan->in_scope_variables };
					my @pvars	= map { Attean::Variable->new($_) } @{ $plan->in_scope_variables };
					my $var		= $cmps[0]->expression->value->value;
					my $ascending	= { $var => $cmps[0]->ascending };
					my $ordered	= Attean::Plan::OrderBy->new(children => [$plan], variables => [$var], ascending => $ascending, distinct => $distinct, ordered => \@cmps);
					push(@plans, $ordered);
				} else {
					my @vars	= (@{ $plan->in_scope_variables }, keys %$exprs);
					my @pvars	= map { Attean::Variable->new($_) } @{ $plan->in_scope_variables };
					my $extend	= Attean::Plan::Extend->new(children => [$plan], expressions => $exprs, distinct => 0, ordered => $plan->ordered, active_graphs => $active_graphs);
					my $ordered	= Attean::Plan::OrderBy->new(children => [$extend], variables => $svars, ascending => $ascending, distinct => 0, ordered => \@cmps);
					my $proj	= $self->new_projection($ordered, $distinct, @{ $plan->in_scope_variables });
					push(@plans, $proj);
				}
			}
			
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::LeftJoin')) {
			my $l	= [$self->plans_for_algebra($children[0], $model, $active_graphs, $default_graphs, %args)];
			my $r	= [$self->plans_for_algebra($children[1], $model, $active_graphs, $default_graphs, %args)];
			return $self->join_plans($model, $active_graphs, $default_graphs, $l, $r, 'left', $algebra->expression);
		} elsif ($algebra->isa('Attean::Algebra::Minus')) {
			my $l	= [$self->plans_for_algebra($children[0], $model, $active_graphs, $default_graphs, %args)];
			my $r	= [$self->plans_for_algebra($children[1], $model, $active_graphs, $default_graphs, %args)];
			return $self->join_plans($model, $active_graphs, $default_graphs, $l, $r, 'minus');
		} elsif ($algebra->isa('Attean::Algebra::Project')) {
			my $vars	= $algebra->variables;
			my @vars	= map { $_->value } @{ $vars };
			my $vars_key	= join(' ', sort @vars);
			my $distinct	= 0;
			my @plans	= map {
				($vars_key eq join(' ', sort @{ $_->in_scope_variables }))
					? $_ # no-op if plan is already properly-projected
					: $self->new_projection($_, $distinct, @vars)
			} $self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, %args);
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::Graph')) {
			my $graph	= $algebra->graph;
			if ($graph->does('Attean::API::Term')) {
				if (my $available = $args{available_graphs}) {
					# the list of available graphs has been restricted, and this
					# graph is not available so return an empty table plan.
					unless (any { $_->equals($graph) } @$available) {
						my $plan	= Attean::Plan::Table->new( variables => [], rows => [], distinct => 0, ordered => [] );
						return $plan;
					}
				}
				return $self->plans_for_algebra($child, $model, [$graph], $default_graphs, %args);
			} else {
				my $gvar	= $graph->value;
				my $graphs	= $model->get_graphs;
				my @plans;
				my %vars		= map { $_ => 1 } $child->in_scope_variables;
				$vars{ $gvar }++;
				my @vars	= keys %vars;
				
				my %available;
				if (my $available = $args{available_graphs}) {
					foreach my $a (@$available) {
						$available{ $a->value }++;
					}
					$graphs	= $graphs->grep(sub { $available{ $_->value } });
				}
				
				my @branches;
				my %ignore	= map { $_->value => 1 } @$default_graphs;
				while (my $graph = $graphs->next) {
					next if $ignore{ $graph->value };
					my %exprs	= ($gvar => Attean::ValueExpression->new(value => $graph));
					# TODO: rewrite $child pattern here to replace any occurrences of the variable $gvar to $graph
					my @plans	= map {
						Attean::Plan::Extend->new(children => [$_], expressions => \%exprs, distinct => 0, ordered => $_->ordered, active_graphs => $active_graphs);
					} $self->plans_for_algebra($child, $model, [$graph], $default_graphs, %args);
					push(@branches, \@plans);
				}
				
				if (scalar(@branches) == 1) {
					@plans	= @{ shift(@branches) };
				} else {
					cartesian { push(@plans, Attean::Plan::Union->new(children => [@_], distinct => 0, ordered => [])) } @branches;
				}
				return @plans;
			}
		} elsif ($algebra->isa('Attean::Algebra::Table')) {
			my $rows	= $algebra->rows;
			my $vars	= $algebra->variables;
			my @vars	= map { $_->value } @{ $vars };
			
			if (scalar(@$rows) < $self->table_threshold) {
				return Attean::Plan::Table->new( variables => $vars, rows => $rows, distinct => 0, ordered => [] );
			} else {
				my $iter	= Attean::ListIterator->new(
					item_type => 'Attean::API::Result',
					variables => \@vars,
					values => $rows
				);
				return Attean::Plan::Iterator->new( iterator => $iter, distinct => 0, ordered => [] );
			}
		} elsif ($algebra->isa('Attean::Algebra::Service')) {
			my $endpoint	= $algebra->endpoint;
			my $silent		= $algebra->silent;
			my $sparql		= sprintf('SELECT * WHERE { %s }', $child->as_sparql);
			my @vars		= $child->in_scope_variables;
			my $plan		= Attean::Plan::Service->new(
				request_signer => $self->request_signer,
				endpoint => $endpoint,
				silent => $silent,
				sparql => $sparql,
				distinct => 0,
				in_scope_variables => \@vars,
				ordered => []
			);
			return $plan;
		} elsif ($algebra->isa('Attean::Algebra::Slice')) {
			my $limit	= $algebra->limit;
			my $offset	= $algebra->offset;
			my @plans;
			foreach my $plan ($self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, %args)) {
				my $vars	= $plan->in_scope_variables;
				push(@plans, Attean::Plan::Slice->new(children => [$plan], limit => $limit, offset => $offset, distinct => $plan->distinct, ordered => $plan->ordered));
			}
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::Union')) {
			# TODO: if both branches are similarly ordered, we can use Attean::Plan::Merge to keep the resulting plan ordered
			my @vars		= keys %{ { map { map { $_ => 1 } $_->in_scope_variables } @children } };
			my @plansets	= map { [$self->plans_for_algebra($_, $model, $active_graphs, $default_graphs, %args)] } @children;

			my @plans;
			cartesian {
				push(@plans, Attean::Plan::Union->new(children => \@_, distinct => 0, ordered => []))
			} @plansets;
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::Extend')) {
			my $var			= $algebra->variable->value;
			my $expr		= $algebra->expression;
			my %exprs		= ($var => $expr);
			my @vars		= $algebra->in_scope_variables;

			my @plans;
			foreach my $plan ($self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, %args)) {
				my $extend		= Attean::Plan::Extend->new(children => [$plan], expressions => \%exprs, distinct => 0, ordered => $plan->ordered, active_graphs => $active_graphs);
				push(@plans, $extend);
			}
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::Group')) {
			my $aggs	= $algebra->aggregates;
			my $groups	= $algebra->groupby;
			my %exprs;
			foreach my $expr (@$aggs) {
				my $var	= $expr->variable->value;
				$exprs{$var}	= $expr;
			}
			my @plans;
			foreach my $plan ($self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, %args)) {
				my $extend		= Attean::Plan::Aggregate->new(children => [$plan], aggregates => \%exprs, groups => $groups, distinct => 0, ordered => [], active_graphs => $active_graphs);
				push(@plans, $extend);
			}
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::Ask')) {
			my @plans;
			foreach my $plan ($self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, %args)) {
				return Attean::Plan::Exists->new(children => [$plan], distinct => 1, ordered => []);
			}
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::Path')) {
			my $s		= $algebra->subject;
			my $path	= $algebra->path;
			my $o		= $algebra->object;
			
			my @algebra	= $self->simplify_path($s, $path, $o);
			
			my @join;
			if (scalar(@algebra)) {
				my @triples;
				while (my $pa = shift(@algebra)) {
					if ($pa->isa('Attean::TriplePattern')) {
						push(@triples, $pa);
					} else {
						if (scalar(@triples)) {
							push(@join, Attean::Algebra::BGP->new( triples => [@triples] ));
							@triples	= ();
						}
						push(@join, $pa);
					}
				}
				if (scalar(@triples)) {
					push(@join, Attean::Algebra::BGP->new( triples => [@triples] ));
				}
				
				my @vars	= $algebra->in_scope_variables;
				
				my @joins	= $self->group_join_plans($model, $active_graphs, $default_graphs, $interesting, map {
					[$self->plans_for_algebra($_, $model, $active_graphs, $default_graphs, %args)]
				} @join);
				
				my @plans;
				foreach my $j (@joins) {
					push(@plans, Attean::Plan::Project->new(children => [$j], variables => [map { Attean::Variable->new($_) } @vars], distinct => 0, ordered => []));
				}
				return @plans;
				
			} elsif ($path->isa('Attean::Algebra::ZeroOrMorePath') or $path->isa('Attean::Algebra::OneOrMorePath')) {
				my $skip	= $path->isa('Attean::Algebra::OneOrMorePath') ? 1 : 0;
				my $begin	= Attean::Variable->new(value => $self->new_temporary('pp'));
				my $end		= Attean::Variable->new(value => $self->new_temporary('pp'));
				my $s_var	= $s->does('Attean::API::Variable');
				my $o_var	= $o->does('Attean::API::Variable');
				
				my $child	= $path->children->[0];
				my $a;
				if ($s_var and not($o_var)) {
					my $inv	= Attean::Algebra::InversePath->new( children => [$child] );
					$a		= Attean::Algebra::Path->new( subject => $end, path => $inv, object => $begin );
				} else {
					$a		= Attean::Algebra::Path->new( subject => $begin, path => $child, object => $end );
				}
				my @cplans	= $self->plans_for_algebra($a, $model, $active_graphs, $default_graphs, %args);
				my @plans;
				foreach my $cplan (@cplans) {
					my $plan	= Attean::Plan::ALPPath->new(
						subject => $s,
						children => [$cplan],
						object => $o,
						graph => $active_graphs,
						skip => $skip,
						step_begin => $begin,
						step_end => $end,
						distinct => 0,
						ordered => []
					);
					push(@plans, $plan);
				}
				return @plans;
			} elsif ($path->isa('Attean::Algebra::ZeroOrOnePath')) {
				my $a		= Attean::Algebra::Path->new( subject => $s, path => $path->children->[0], object => $o );
				my @children	= $self->plans_for_algebra($a, $model, $active_graphs, $default_graphs, %args);
				my @plans;
				foreach my $plan (@children) {
					push(@plans, Attean::Plan::ZeroOrOnePath->new(
						subject => $s,
						children => [$plan],
						object => $o,
						graph => $active_graphs,
						distinct => 0,
						ordered => []
					));
				}
				return @plans;
			} else {
				die "Cannot simplify property path $path: " . $algebra->as_string;
			}
		} elsif ($algebra->isa('Attean::Algebra::Construct')) {
			my @children	= $self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, %args);
			my @plans;
			foreach my $plan (@children) {
				push(@plans, Attean::Plan::Construct->new(triples => $algebra->triples, children => [$plan], distinct => 0, ordered => []));
			}
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::Describe')) {
			my @children	= $self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, %args);
			my @plans;
			foreach my $plan (@children) {
				push(@plans, Attean::Plan::Describe->new(terms => $algebra->terms, graph => $active_graphs, children => [$plan], distinct => 0, ordered => []));
			}
			return @plans;
		} elsif ($algebra->isa('Attean::Algebra::Clear')) {
			my $plan_class	= $algebra->drop ? 'Attean::Plan::Drop' : 'Attean::Plan::Clear';
			my $target	= $algebra->target;
			if ($target eq 'GRAPH') {
				return Attean::Plan::Clear->new(graphs => [$algebra->graph]);
			} else {
				my %default	= map { $_->value => 1 } @$active_graphs;
				my $graphs	= $model->get_graphs;
				my @graphs;
				while (my $graph = $graphs->next) {
					if ($target eq 'ALL') {
						push(@graphs, $graph);
					} else {
						if ($target eq 'DEFAULT' and $default{ $graph->value }) {
							push(@graphs, $graph);
						} elsif ($target eq 'NAMED' and not $default{ $graph->value }) {
							push(@graphs, $graph);
						}
					}
				}
				return $plan_class->new(graphs => \@graphs);
			}
		} elsif ($algebra->isa('Attean::Algebra::Add')) {
			my $triple	= triplepattern(variable('s'), variable('p'), variable('o'));
			my $child;
			my $default_source	= 0;
			if (my $from = $algebra->source) {
				($child)	= $self->access_plans( $model, $active_graphs, $triple->as_quad_pattern($from) );
			} else {
				$default_source++;
				my $bgp		= Attean::Algebra::BGP->new( triples => [$triple] );
				($child)	= $self->plans_for_algebra($bgp, $model, $active_graphs, $default_graphs, %args);
			}
			
			my $dest;
			my $default_dest	= 0;
			if (my $g = $algebra->destination) {
				$dest		= $triple->as_quad_pattern($g);
			} else {
				$default_dest++;
				$dest		= $triple->as_quad_pattern($default_graphs->[0]);
			}


			my @plans;
			my $run_update	= 1;
			if ($default_dest and $default_source) {
				$run_update	= 0;
			} elsif ($default_dest or $default_source) {
				#
			} elsif ($algebra->source->equals($algebra->destination)) {
				$run_update	= 0;
			}
			
			if ($run_update) {
				if ($algebra->drop_destination) {
					my @graphs	= $algebra->has_destination ? $algebra->destination : @$default_graphs;
					unshift(@plans, Attean::Plan::Clear->new(graphs => [@graphs]));
				}
			
				push(@plans, Attean::Plan::TripleTemplateToModelQuadMethod->new(
					graph		=> $default_graphs->[0],
					order		=> ['add_quad'],
					patterns	=> {'add_quad' => [$dest]},
					children 	=> [$child],
				));
			
				if ($algebra->drop_source) {
					my @graphs	= $algebra->has_source ? $algebra->source : @$default_graphs;
					push(@plans, Attean::Plan::Clear->new(graphs => [@graphs]));
				}
			}
			my $plan	= (scalar(@plans) == 1) ? shift(@plans) : Attean::Plan::Sequence->new( children => \@plans );
			return $plan;
		} elsif ($algebra->isa('Attean::Algebra::Modify')) {
			unless ($child) {
				# This is an INSERT/DELETE DATA algebra with ground data and no pattern
				$child	= Attean::Algebra::BGP->new( triples => [] );
			}
			
			my $dataset	= $algebra->dataset;
			my @default	= @{ $dataset->{default} || [] };
			my @named	= values %{ $dataset->{named} || {} };
			
			my @active_graphs	= @$active_graphs;
			my @default_graphs	= @$default_graphs;
			
			if (scalar(@default) or scalar(@named)) {
				# change the available named graphs
				# change the active graph(s)
				@active_graphs	= @default;
				@default_graphs	= @default;
				$args{ available_graphs }	= [@named];
			} else {
				# no custom dataset
			}
			
			my @children	= $self->plans_for_algebra($child, $model, \@active_graphs, \@default_graphs, %args);
			my $i	= $algebra->insert;
			my $d	= $algebra->delete;
			my %patterns;
			my @order;
			if (scalar(@$d)) {
				push(@order, 'remove_quad');
				$patterns{ 'remove_quad' }	= $d;
			}
			if (scalar(@$i)) {
				push(@order, 'add_quad');
				$patterns{ 'add_quad' }	= $i;
			}
			return map {
				Attean::Plan::TripleTemplateToModelQuadMethod->new(
					graph		=> $default_graphs->[0],
					order		=> \@order,
					patterns	=> \%patterns,
					children 	=> [$_],
				)
			} @children;
		} elsif ($algebra->isa('Attean::Algebra::Load')) {
			my $pattern		= triplepattern(variable('subject'), variable('predicate'), variable('object'));
			my $load		= Attean::Plan::Load->new( url => $algebra->url->value, silent => $algebra->silent );
			my $graph		= $algebra->has_graph ? $algebra->graph : $default_graphs->[0];
			my $plan		= Attean::Plan::TripleTemplateToModelQuadMethod->new(
				graph		=> $graph,
				order		=> ['add_quad'],
				patterns	=> {'add_quad' => [$pattern]},
				children 	=> [$load],
			);
			return $plan;
		} elsif ($algebra->isa('Attean::Algebra::Create')) {
			return Attean::Plan::Sequence->new( children => [] );
		} elsif ($algebra->isa('Attean::Algebra::Sequence')) {
			my @plans;
			foreach my $child (@{ $algebra->children }) {
				my ($plan)	= $self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, %args);
				push(@plans, $plan);
			}
			return Attean::Plan::Sequence->new( children => \@plans );
		} elsif ($algebra->isa('Attean::Algebra::Unfold')) {
			my @plans	= $self->plans_for_algebra($child, $model, $active_graphs, $default_graphs, %args);
			my @unfold;
			foreach my $p (@plans) {
				push(@unfold, Attean::Plan::Unfold->new( children => [$p], expression => $algebra->expression, variables => $algebra->variables, active_graphs => $active_graphs ));
			}
			return @unfold;
		}
		die "Unimplemented algebra evaluation for: " . $algebra->as_string;
	}
	
# 	sub plans_for_unbounded_path {
# 		my $self			= shift;
# 		my $algebra			= shift;
# 		my $model			= shift;
# 		my $active_graphs	= shift;
# 		my $default_graphs	= shift;
# 		my %args			= @_;
# 		
# 		my $s		= $algebra->subject;
# 		my $path	= $algebra->path;
# 		my $o		= $algebra->object;
# 		
# 		return Attean::Plan::ALPPath->new(distinct => 0, ordered => []);
# 	}
	
	sub _package {
		my $self	= shift;
		my @args	= @_;
		
		my @bgptriples	= map { @{ $_->triples } } grep { $_->isa('Attean::Algebra::BGP') } @args;
		my @triples		= grep { $_->isa('Attean::TriplePattern') } @args;
		my @rest		= grep { not $_->isa('Attean::Algebra::BGP') and not $_->isa('Attean::TriplePattern') } @args;
		if (scalar(@rest) == 0) {
			return Attean::Algebra::BGP->new( triples => [@bgptriples, @triples] );
		} else {
			my $p	= Attean::Algebra::BGP->new( triples => [@bgptriples, @triples] );
			while (scalar(@rest) > 0) {
				$p	= Attean::Algebra::Join->new( children => [$p, shift(@rest)] );
			}
			return $p;
		}
	}
	
=item C<< simplify_path( $subject, $path, $object ) >>

Return a simplified L<Attean::API::Algebra> object corresponding to the given
property path.

=cut

	sub simplify_path {
		my $self	= shift;
		my $s		= shift;
		my $path	= shift;
		my $o		= shift;
		if ($path->isa('Attean::Algebra::SequencePath')) {
			my $jvar		= Attean::Variable->new(value => $self->new_temporary('pp'));
			my ($lhs, $rhs)	= @{ $path->children };
			my @paths;
			push(@paths, $self->simplify_path($s, $lhs, $jvar));
			push(@paths, $self->simplify_path($jvar, $rhs, $o));
			return $self->_package(@paths);
		} elsif ($path->isa('Attean::Algebra::InversePath')) {
			my ($ipath)	= @{ $path->children };
			return $self->simplify_path($o, $ipath, $s);
		} elsif ($path->isa('Attean::Algebra::PredicatePath')) {
			my $pred	= $path->predicate;
			return Attean::TriplePattern->new($s, $pred, $o);
		} elsif ($path->isa('Attean::Algebra::AlternativePath')) {
			my ($l, $r)	= @{ $path->children };
			my $la	= $self->_package($self->simplify_path($s, $l, $o));
			my $ra	= $self->_package($self->simplify_path($s, $r, $o));
			return Attean::Algebra::Union->new( children => [$la, $ra] );
		} elsif ($path->isa('Attean::Algebra::NegatedPropertySet')) {
			my @branches;
			
			my @preds	= @{ $path->predicates };
			if (scalar(@preds)) {
				my $pvar	= Attean::Variable->new(value => $self->new_temporary('nps'));
				my $pvar_e	= Attean::ValueExpression->new( value => $pvar );
				my $t		= Attean::TriplePattern->new($s, $pvar, $o);
				my @vals	= map { Attean::ValueExpression->new( value => $_ ) } @preds;
				my $expr	= Attean::FunctionExpression->new( children => [$pvar_e, @vals], operator => 'notin' );
				my $bgp		= Attean::Algebra::BGP->new( triples => [$t] );
				my $f_fwd	= Attean::Algebra::Filter->new( children => [$bgp], expression => $expr );
				push(@branches, $f_fwd);
			}

			my @rev		= @{ $path->reversed };
			if (scalar(@rev)) {
				my $pvar	= Attean::Variable->new(value => $self->new_temporary('nps_rev'));
				my $pvar_e	= Attean::ValueExpression->new( value => $pvar );
				my $t		= Attean::TriplePattern->new($o, $pvar, $s);
				my @vals	= map { Attean::ValueExpression->new( value => $_ ) } @rev;
				my $expr	= Attean::FunctionExpression->new( children => [$pvar_e, @vals], operator => 'notin' );
				my $bgp		= Attean::Algebra::BGP->new( triples => [$t] );
				my $f_rev	= Attean::Algebra::Filter->new( children => [$bgp], expression => $expr );
				push(@branches, $f_rev);
			}

			if (scalar(@branches) == 1) {
				return shift(@branches);
			} else {
				return Attean::Algebra::Union->new( children => \@branches );
			}
		} else {
			return;
		}
	}
	
=item C<< new_projection( $plan, $distinct, @variable_names ) >>

Return a new L<< Attean::Plan::Project >> plan over C<< $plan >>, projecting
the named variables. C<< $disctinct >> should be true if the caller can
guarantee that the resulting plan will produce distinct results, false otherwise.

This method takes care of computing plan metadata such as the resulting ordering.

=cut

	sub new_projection {
		my $self		= shift;
		my $plan		= shift;
		my $distinct	= shift;
		my @vars		= @_;
		my $order		= $plan->ordered;
		my @pvars		= map { Attean::Variable->new($_) } @vars;
		
		my %pvars		= map { $_ => 1 } @vars;
		my @porder;
		CMP: foreach my $cmp (@{ $order }) {
			my @cmpvars	= $self->_comparator_referenced_variables($cmp);
			foreach my $v (@cmpvars) {
				unless ($pvars{ $v }) {
					# projection is dropping a variable used in this comparator
					# so we lose any remaining ordering that the sub-plan had.
					last CMP;
				}
			}
			
			# all the variables used by this comparator are available after
			# projection, so the resulting plan will continue to be ordered
			# by this comparator
			push(@porder, $cmp);
		}
		
		return Attean::Plan::Project->new(children => [$plan], variables => \@pvars, distinct => $distinct, ordered => \@porder);
	}

=item C<< bgp_join_plans( $bgp, $model, \@active_graphs, \@default_graphs, \@interesting_order, \@plansA, \@plansB, ... ) >>

Returns a list of alternative plans for the join of a set of triples.
The arguments C<@plansA>, C<@plansB>, etc. represent alternative plans for each
triple participating in the join.

=cut

	sub bgp_join_plans {
		my $self			= shift;
		my $bgp				= shift;
		my $model			= shift;
		my $active			= shift;
		my $default			= shift;
		my $interesting		= shift;
		my @triples			= @_;
		
		if (scalar(@triples)) {
			my @plans			= $self->joins_for_plan_alternatives($model, $active, $default, $interesting, @triples);
			my @triples			= @{ $bgp->triples };
		
			# If the BGP does not contain any blanks, then the results are
			# guaranteed to be distinct. Otherwise, we have to assume they're
			# not distinct.
			my $distinct		= 1;
			LOOP: foreach my $t (@triples) {
				foreach my $b ($t->values_consuming_role('Attean::API::Blank')) {
					$distinct	= 0;
					last LOOP;
				}
				foreach my $b ($t->values_consuming_role('Attean::API::Variable')) {
					if ($b->value =~ /^[.]/) {
						# variable names starting with a dot represent placeholders introduced during query planning (with C<new_temporary>)
						# they are not projectable, and so may cause an otherwise distinct result to become non-distinct
						$distinct	= 0;
						last LOOP;
					}
				}
			}
		
			# Set the distinct flag on each of the top-level join plans that
			# represents the entire BGP. (Sub-plans won't ever be marked as
			# distinct, but that shouldn't matter to the rest of the planning
			# process.)
			if ($distinct) {
				foreach my $p (@plans) {
					$p->distinct(1);
				}
			}

			return @plans;
		} else {
			# The empty BGP is a special case -- it results in a single join-identity result
			my $r		= Attean::Result->new( bindings => {} );
			my $plan	= Attean::Plan::Table->new( rows => [$r], variables => [], distinct => 1, ordered => [] );
			return $plan;
		}
	}

=item C<< group_join_plans( $model, \@active_graphs, \@default_graphs, \@interesting_order, \@plansA, \@plansB, ... ) >>

Returns a list of alternative plans for the join of a set of sub-plans.
The arguments C<@plansA>, C<@plansB>, etc. represent alternative plans for each
sub-plan participating in the join.

=cut

	sub group_join_plans {
		my $self			= shift;
		return $self->joins_for_plan_alternatives(@_);
	}
	
=item C<< joins_for_plan_alternatives( $model, \@active_graphs, \@default_graphs, $interesting, \@plan_A, \@plan_B, ... ) >>

Returns a list of alternative plans that may all be used to produce results
matching the join of C<< plan_A >>, C< plan_B >>, etc. Each plan array here
(e.g. C<< @plan_A >>) should contain equivalent plans.

=cut

	sub joins_for_plan_alternatives {
		my $self			= shift;
		my $model			= shift;
		my $active_graphs	= shift;
		my $default_graphs	= shift;
		my $interesting		= shift;
		my @args			= @_; # each $args[$i] here is an array reference containing alternate plans for element $i
		die "This query planner does not seem to consume a Attean::API::JoinPlanner role (which is necessary for query planning)";
	}
	
=item C<< access_plans( $model, $active_graphs, $pattern ) >>

Returns a list of alternative L<Attean::API::Plan> objects that may be used to
produce results matching the L<Attean::API::TripleOrQuadPattern> $pattern in
the context of C<< $active_graphs >>.

=cut

	# $pattern is a Attean::API::TripleOrQuadPattern object
	# Return a Attean::API::Plan object that represents the evaluation of $pattern.
	# e.g. different plans might represent different ways of producing the matches (table scan, index match, etc.)
	sub access_plans {
		my $self			= shift;
		my $model			= shift;
		my $active_graphs	= shift;
		my $pattern			= shift;
		my @vars			= map { $_->value } $pattern->values_consuming_role('Attean::API::Variable');
		my %vars;
		my $dup				= 0;
		foreach my $v (@vars) {
			$dup++ if ($vars{$v}++);
		}
		
		my $distinct		= 0; # TODO: is this pattern distinct? does it have blank nodes?
		
		my @nodes			= $pattern->values;
		unless ($nodes[3]) {
			$nodes[3]	= $active_graphs;
		}
		my $plan		= Attean::Plan::Quad->new(
			subject	=> $nodes[0],
			predicate	=> $nodes[1],
			object	=> $nodes[2],
			graph	=> $nodes[3],
			values => \@nodes,
			distinct => $distinct,
			ordered => [],
		);
		return $plan;
	}
	
=item C<< join_plans( $model, \@active_graphs, \@default_graphs, \@plan_left, \@plan_right, $type [, $expr] ) >>

Returns a list of alternative plans for the join of one plan from C<< @plan_left >>
and one plan from C<< @plan_right >>. The join C<< $type >> must be one of
C<< 'inner' >>, C<< 'left' >>, or C<< 'minus' >>, indicating the join algorithm
to be used. If C<< $type >> is C<< 'left' >>, then the optional C<< $expr >>
may be used to supply a filter expression that should be used by the SPARQL
left-join algorithm.

=cut

	# $lhs and $rhs are both Attean::API::Plan objects
	# Return a Attean::API::Plan object that represents the evaluation of $lhs ⋈ $rhs.
	# The $left and $minus flags indicate the type of the join to be performed (⟕ and ▷, respectively).
	# e.g. different plans might represent different join algorithms (nested loop join, hash join, etc.) or different orderings ($lhs ⋈ $rhs or $rhs ⋈ $lhs)
	sub join_plans {
		my $self			= shift;
		my $model			= shift;
		my $active_graphs	= shift;
		my $default_graphs	= shift;
		my $lplans			= shift;
		my $rplans			= shift;
		my $type			= shift;
		my $left			= ($type eq 'left');
		my $minus			= ($type eq 'minus');
		my $expr			= shift;
		
		my @plans;
		Carp::confess unless (reftype($lplans) eq 'ARRAY');
		foreach my $lhs (@{ $lplans }) {
			foreach my $rhs (@{ $rplans }) {
				my @vars	= (@{ $lhs->in_scope_variables }, @{ $rhs->in_scope_variables });
				my %vars;
				my %join_vars;
				foreach my $v (@vars) {
					if ($vars{$v}++) {
						$join_vars{$v}++;
					}
				}
				my @join_vars	= keys %join_vars;
				
				if ($left) {
					if (scalar(@join_vars) > 0) {
						push(@plans, Attean::Plan::HashJoin->new(children => [$lhs, $rhs], left => 1, expression => $expr, join_variables => \@join_vars, distinct => 0, ordered => []));
					}
					push(@plans, Attean::Plan::NestedLoopJoin->new(children => [$lhs, $rhs], left => 1, expression => $expr, join_variables => \@join_vars, distinct => 0, ordered => $lhs->ordered));
				} elsif ($minus) {
					# we can't use a hash join for MINUS queries, because of the definition of MINUS having a special case for compatible results that have disjoint domains
					push(@plans, Attean::Plan::NestedLoopJoin->new(children => [$lhs, $rhs], anti => 1, join_variables => \@join_vars, distinct => 0, ordered => $lhs->ordered));
				} else {
					if (scalar(@join_vars) > 0) {
						# if there's shared variables (hopefully), we can also use a hash join
						push(@plans, Attean::Plan::HashJoin->new(children => [$lhs, $rhs], join_variables => \@join_vars, distinct => 0, ordered => []));
						push(@plans, Attean::Plan::HashJoin->new(children => [$rhs, $lhs], join_variables => \@join_vars, distinct => 0, ordered => []));
# 					} else {
# 						warn "No join vars for $lhs ⋈ $rhs";
					}

					# nested loop joins work in all cases
					push(@plans, Attean::Plan::NestedLoopJoin->new(children => [$lhs, $rhs], join_variables => \@join_vars, distinct => 0, ordered => $lhs->ordered));
					push(@plans, Attean::Plan::NestedLoopJoin->new(children => [$rhs, $lhs], join_variables => \@join_vars, distinct => 0, ordered => $rhs->ordered));
				}
			}
		}
		
		return @plans;
	}
	
	
	sub _comparator_referenced_variables {
		my $self	= shift;
		my %vars;
		while (my $c = shift) {
			my $expr	= $c->expression;
			foreach my $v ($expr->in_scope_variables) {
				$vars{$v}++;
			}
		}
		return keys %vars;
	}
	
	sub _comparators_are_stable_and_cover_vars {
		my $self	= shift;
		my $cmps	= shift;
		my @vars	= @_;
		my %unseen	= map { $_ => 1 } @vars;
		foreach my $c (@$cmps) {
			return 0 unless ($c->expression->is_stable);
			foreach my $v ($self->_comparator_referenced_variables($c)) {
				delete $unseen{$v};
			}
		}
		my @keys	= keys %unseen;
		return (scalar(@keys) == 0);
	}
	
	sub _order_by {
		my $self	= shift;
		my $algebra	= shift;
		my ($exprs, $ascending, $svars);
		my @cmps	= @{ $algebra->comparators };
		my %ascending;
		my %exprs;
		my @svars;
		foreach my $i (0 .. $#cmps) {
			my $var	= $self->new_temporary('order');
			my $cmp	= $cmps[$i];
			push(@svars, $var);
			$ascending{$var}	= $cmp->ascending;
			$exprs{$var}		= $cmp->expression;
		}
		return (\%exprs, \%ascending, \@svars);
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2022 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
