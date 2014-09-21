use v5.14;
use warnings;

=head1 NAME

Attean::SimpleQueryEvaluator - Simple query evaluator

=head1 VERSION

This document describes Attean::SimpleQueryEvaluator version 0.001

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $e = Attean::SimpleQueryEvaluator->new( model => $model );
  my $iter = $e->evaluate( $algebra, $active_graph );

=head1 DESCRIPTION

The Attean::SimpleQueryEvaluator class implements a simple query evaluator that,
given an L<Attean::API::Algebra|Attean::API::Query> and a L<Attean::API::Model>
object, evaluates the query represented by the algebra using data from the
model, and returns a query result.

=cut

use Attean::Algebra;
use Attean::Expression;

package Attean::SimpleQueryEvaluator 0.001 {
	use Moo;
	use Attean::RDF;
	use Scalar::Util qw(blessed);
	use List::Util qw(reduce);
	use Types::Standard qw(ConsumerOf);
	use namespace::clean;

=item C<< model >>

The L<Attean::API::Model> object used for query evaluation.

=cut

	has 'model' => (is => 'ro', isa => ConsumerOf['Attean::API::Model'], required => 1);
	
=item C<< default_graph >>

The L<Attean::API::IRI> object representing the default graph in the C<< model >>.
The default graph will be excluded from enumeration of graph names for query
features such as C<< GRAPH ?g {} >>.

=cut

	has 'default_graph'	=> (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], required => 1);
	
=item C<< evaluate( $algebra, $active_graph ) >>

Returns an L<Attean::API::Iterator> object with results produced by evaluating
the query C<< $algebra >> against the evaluator's C<< model >>, using the
supplied C<< $active_graph >>.

=cut

	sub exists_pattern_handler_generator {
		my $self			= shift;
		my $active_graph	= shift;
		return sub {
			my $pattern		= shift;
			my ($r, %args)	= @_;
			my $table		= Attean::Algebra::Table->new( variables => [map { variable($_) } $r->variables], rows => [$r] );
			my $join		= Attean::Algebra::Join->new( children => [$table, $pattern] );
			# TODO: substitute variables at top-level of EXISTS pattern
			my $iter	= $self->evaluate($join, $active_graph);
			return ($iter->next) ? Attean::Literal->true : Attean::Literal->false;
		}
	}
	
	sub evaluate {
		my $self			= shift;
		my $algebra			= shift;
		my $active_graph	= shift || Carp::confess "No active-graph passed to Attean::SimpleQueryEvaluator->evaluate";
		
		Carp::confess "No algebra passed for evaluation" unless ($algebra);
		
		if ($algebra->isa('Attean::Algebra::BGP')) {
			my @triples	= @{ $algebra->triples };
			if (scalar(@triples) == 0) {
				my $b	= Attean::Result->new( bindings => {} );
				return Attean::ListIterator->new(values => [$b], item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
			} else {
				my @iters;
				my %vars;
				my %blanks;
				foreach my $t (@triples) {
					my $q		= $t->as_quad_pattern($active_graph);
					my @values;
					foreach my $v ($q->values) {
						if ($v->does('Attean::API::Blank')) {
							unless (exists $blanks{$v->value}) {
								$blanks{$v->value}	= Attean::Variable->new();
							}
							my $var	= $blanks{$v->value};
							push(@values, $var);
						} else {
							if ($v->does('Attean::API::Variable')) {
								$vars{ $v->value }++;
							}
							push(@values, $v);
						}
					}
					my $iter	= $self->model->get_bindings( @values );
					push(@iters, $iter);
				}
				while (scalar(@iters) > 1) {
					my ($lhs, $rhs)	= splice(@iters, 0, 2);
					my $iter		= $lhs->join($rhs);
					unshift(@iters, $iter);
				}
				return shift(@iters)->map(sub { shift->project(keys %vars) });
			}
		} elsif ($algebra->isa('Attean::Algebra::Distinct') or $algebra->isa('Attean::Algebra::Reduced')) {
			my %seen;
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate( $child, $active_graph );
			return $iter->grep(sub {
				my $r	= shift;
				return not($seen{ $r->as_string }++);
			});
		} elsif ($algebra->isa('Attean::Algebra::Extend')) {
			my $child	= $algebra;
			my @extends;
			my %extends;
			while ($child->isa('Attean::Algebra::Extend')) {
				my $expr			= $child->expression;
				my $var				= $child->variable->value;
				my $impl			= $expr->impl;
				$extends{ $var }	= $impl;
				unshift(@extends, $var);
				($child)			= @{ $child->children };
			}
			my $iter	= $self->evaluate( $child, $active_graph );
			return $iter->map(sub {
				my $r	= shift;
				my %extension;
				my %row_cache;
				foreach my $var (@extends) {
					my $impl	= $extends{ $var };
# 					warn "================> $var\n";
					my $eh	= $self->exists_pattern_handler_generator( $active_graph );
					my $val	= eval { $impl->($r, handle_exists => $eh, row_cache => \%row_cache) };
# 					warn "Extend error: $@" if ($@);
					if ($val) {
						$r	= Attean::Result->new( bindings => { $var => $val } )->join($r);
					}
				}
				return $r;
			});
		} elsif ($algebra->isa('Attean::Algebra::Filter')) {
			my $expr	= $algebra->expression;
			my $impl	= $expr->impl;
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate( $child, $active_graph );
			my $eh	= $self->exists_pattern_handler_generator( $active_graph );
			return $iter->grep(sub {
				my $r	= shift;
				say "FILTER Result: " . $r->as_string;
				my $t	= eval { $impl->($r, handle_exists => $eh, row_cache => {}) };
				if ($@) { warn "Filter evaluation: $@\n" };
# 				warn "Filter: " . ($t ? $t->as_string : '(undef)') . "\n";
# 				warn sprintf("--> %d\n", $t->ebv) if ($t);
				return ($t ? $t->ebv : 0);
			});
		} elsif ($algebra->isa('Attean::Algebra::OrderBy')) {
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate( $child, $active_graph );
			my @rows	= $iter->elements;
			my @cmps	= @{ $algebra->comparators };
			my @exprs	= map { $_->expression } @cmps;
			my @dirs	= map { $_->ascending } @cmps;
			my @sorted	= map { $_->[0] } sort {
				my ($ar, $avalues)	= @$a;
				my ($br, $bvalues)	= @$b;
				my $c	= 0;
				foreach my $i (0 .. $#cmps) {
					my $av	= $avalues->[$i];
					my $bv	= $bvalues->[$i];
					$c		= $av ? $av->compare($bv) : 1;
					if ($dirs[$i] == 0) {
						$c	*= -1;
					}
					last unless ($c == 0);
				}
				$c
			} map { my $r = $_; [$r, [map { $_->evaluate($r) } @exprs]] } @rows;
			return Attean::ListIterator->new( values => \@sorted, item_type => $iter->item_type);
		} elsif ($algebra->isa('Attean::Algebra::Graph')) {
			my $graph	= $algebra->graph;
			my ($child)	= @{ $algebra->children };
			if ($graph->does('Attean::API::Term')) {
				return $self->evaluate($child, $graph);
			} else {
				my $graphs	= $self->model->get_graphs();
				my @iters;
				while (my $g = $graphs->next) {
					next if ($g->value eq $self->default_graph->value);
					my $gr	= Attean::Result->new( bindings => { $graph->value => $g } );
					my $iter	= $self->evaluate($child, $g);
					push(@iters, $iter->map(sub {
						my $b	= shift;
						my $result	= $b->join($gr);
						return $result;
					}));
				}
				return Attean::IteratorSequence->new( iterators => \@iters, item_type => Type::Tiny::Role->new(role => 'Attean::API::Result') );
			}
		} elsif ($algebra->isa('Attean::Algebra::Group')) {
			my @groupby	= @{ $algebra->groupby };
			my @impls	= map { $_->impl } @groupby;
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate($child, $active_graph);
			my %groups;
			my $eh	= $self->exists_pattern_handler_generator( $active_graph );
			while (my $r = $iter->next) {
				my %vars;
				my %row_cache;
				my @group_terms	= map { eval { $_->($r, handle_exists => $eh, row_cache => \%row_cache) } } @impls;
				my $key			= join(' ', map { blessed($_) ? $_->as_string : '' } @group_terms);
				my %group_bindings;
				foreach my $i (0 .. $#group_terms) {
					my $v	= $groupby[$i];
					if (blessed($v) and $v->isa('Attean::ValueExpression') and $v->value->does('Attean::API::Variable')) {
						if (my $term = $group_terms[$i]) {
							$group_bindings{$v->value->value}	= $term;
						}
					}
				}
				my $gr			= Attean::Result->new( bindings => \%group_bindings );
				$groups{$key}	= [$gr, []] unless (exists($groups{$key}));
				push(@{ $groups{$key}[1] }, $r);
			}
			my @keys	= keys %groups;
			if (scalar(@keys) == 0) {
				push(@keys, '');
				my $gr			= Attean::Result->new( bindings => {} );
				$groups{''}	= [$gr, []];
			}
			my $aggs	= $algebra->aggregates;
			my @agg_impls	= map { $_->impl } @$aggs;
			my @results;
			foreach my $key (@keys) {
				my %row_cache;
				my ($binding, $rows)	= @{ $groups{$key} };
				my $count	= scalar(@$rows);
				my %bindings;
				foreach my $i (0 .. $#agg_impls) {
					my $agg	= $aggs->[$i];
					my $name	= $agg->variable->value;
					my $term	= eval { $agg_impls[$i]->( $rows, handle_exists => $eh, row_cache => \%row_cache ) };
# 					warn "AGGREGATE error: $@" if ($@);
					if ($term) {
						$bindings{ $name } = $term;
					}
				}
				my $r	= Attean::Result->new( bindings => \%bindings )->join($binding);
# 				say "GROUP Result: " . $r->as_string;
				push(@results, $r);
			}
			return Attean::ListIterator->new(values => \@results, item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
		} elsif ($algebra->isa('Attean::Algebra::Join')) {
			my ($lhs, $rhs)	= map { $self->evaluate($_, $active_graph) } @{ $algebra->children };
			return $lhs->join($rhs);
		} elsif ($algebra->isa('Attean::Algebra::LeftJoin')) {
			my $expr	= $algebra->expression;
			my ($lhs_iter, $rhs_iter)	= map { $self->evaluate($_, $active_graph) } @{ $algebra->children };
			my @rhs		= $rhs_iter->elements;
			my @results;
			while (my $lhs = $lhs_iter->next) {
				my $joined	= 0;
				foreach my $rhs (@rhs) {
					if (my $j = $lhs->join($rhs)) {
						if ($expr->evaluate($j)->ebv) {
							$joined++;
							push(@results, $j);
						}
					}
				}
				push(@results, $lhs) unless ($joined);
			}
			return Attean::ListIterator->new( values => \@results, item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
		} elsif ($algebra->isa('Attean::Algebra::Minus')) {
			my ($lhs, $rhs)	= map { $self->evaluate($_, $active_graph) } @{ $algebra->children };
			my @rhs		= $rhs->elements;
			my @results;
			while (my $lhs = $self->next) {
				my $joinable	= 0;
				foreach my $rhs (@rhs) {
					if (my $j = $lhs->join($rhs)) {
						$joinable++;
					} else {
						my $intersects	= 0;
						my %lhs_dom	= map { $_ => 1 } $lhs->variables;
						foreach my $rhs ($rhs->variables) {
							if (exists $lhs_dom{$rhs}) {
								$intersects++;
							}
						}
						if ($intersects) {
							$joinable++;
						}
					}
				}
				push(@results, $lhs) unless ($joinable);
			}
			return Attean::ListIterator->new( values => \@results, item_type => $self->item_type);
		} elsif ($algebra->isa('Attean::Algebra::Path')) {
			my $s		= $algebra->subject;
			my $path	= $algebra->path;
			my $o		= $algebra->object;
			if ($path->isa('Attean::Algebra::PredicatePath')) {
				return $self->model->get_bindings( $s, $path->predicate, $o, $active_graph );
			} elsif ($path->isa('Attean::Algebra::InversePath')) {
				my ($child)	= @{ $path->children };
				my $path	= Attean::Algebra::Path->new( subject => $o, path => $child, object => $s );
				return $self->evaluate( $path, $active_graph );
			} elsif ($path->isa('Attean::Algebra::AlternativePath')) {
				my @paths		= @{ $path->children };
				my @algebras	= map { Attean::Algebra::Path->new( subject => $s, path => $_, object => $o ) } @paths;
				my @iters		= map { $self->evaluate($_, $active_graph) } @algebras;
				return Attean::IteratorSequence->new( iterators => \@iters, item_type => $iters[0]->item_type );
			} elsif ($path->isa('Attean::Algebra::NegatedPropertySet')) {
				my ($child)	= @{ $path->children };
				my $preds	= $path->predicates;
				my %preds	= map { $_->value => 1 } @$preds;
				my $iter	= $self->model->get_quads($s, undef, $o, $active_graph);
				my $filter	= $iter->grep(sub {
					my $q	= shift;
					my $p	= $q->predicate;
					return not exists $preds{ $p->value };
				});
				my %vars;
				if ($s->does('Attean::API::Variable')) {
					$vars{subject}	= $s->value;
				}
				if ($o->does('Attean::API::Variable')) {
					$vars{object}	= $o->value;
				}
				return $filter->map(sub {
					my $q	= shift;
					return unless $q;
					my %bindings	= map { $vars{$_} => $q->$_() } (keys %vars);
					return Attean::Result->new( bindings => \%bindings );
				}, Type::Tiny::Role->new(role => 'Attean::API::Result'));
			} elsif ($path->isa('Attean::Algebra::SequencePath')) {
				my @seq		= @{ $path->children };
				if (scalar(@seq) == 1) {
					my $path	= Attean::Algebra::Path->new( subject => $s, path => $seq[0], object => $o );
					return $self->evaluate($path, $active_graph);
				} else {
					my @paths;
					my $first	= shift(@seq);
					my $join	= Attean::Variable->new();
					push(@paths, Attean::Algebra::Path->new( subject => $s, path => $first, object => $join ));
					foreach my $i (0 .. $#seq) {
						my $p	= $seq[$i];
						my $newjoin	= Attean::Variable->new();
						my $obj	= ($i == $#seq) ? $o : $newjoin;
						push(@paths, Attean::Algebra::Path->new( subject => $join, path => $p, object => $obj ));
						$join	= $newjoin;
					}
					
					while (scalar(@paths) > 1) {
						my ($l, $r)	= splice(@paths, 0, 2);
						unshift(@paths, Attean::Algebra::Join->new( children => [$l, $r] ));
					}
					return $self->evaluate(shift(@paths), $active_graph);
				}
			} elsif ($path->isa('Attean::Algebra::ZeroOrMorePath') or $path->isa('Attean::Algebra::OneOrMorePath')) {
				my ($child)	= @{ $path->children };
				if ($s->does('Attean::API::Term') and $o->does('Attean::API::Variable')) {
					my $v	= {};
					if ($path->isa('Attean::Algebra::ZeroOrMorePath')) {
						$self->_ALP($active_graph, $s, $child, $v);
					} else {
						my $iter	= $self->_eval($active_graph, $s, $child);
						while (my $n = $iter->next) {
							$self->_ALP($active_graph, $n, $child, $v);
						}
					}
					my @results;
					foreach my $v (values %$v) {
						my $r	= Attean::Result->new( bindings => { $o->value => $v } );
						push(@results, $r);
					}
					return Attean::ListIterator->new(values => \@results, item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
				} elsif ($s->does('Attean::API::Variable') and $o->does('Attean::API::Variable')) {
					my $nodes	= $self->model->graph_nodes( $active_graph );
					my @results;
					while (my $t = $nodes->next) {
						my $tr	= Attean::Result->new( bindings => { $s->value => $t } );
						my $p		= Attean::Algebra::Path->new( subject => $t, path => $path, object => $o );
						my $iter	= $self->evaluate($p, $active_graph);
						while (my $r = $iter->next) {
							push(@results, $r->join($tr));
						}
					}
					return Attean::ListIterator->new(values => \@results, item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
				} elsif ($s->does('Attean::API::Variable') and $o->does('Attean::API::Term')) {
					my $pp	= Attean::Algebra::InversePath->new( children => [$child] );
					my $p	= Attean::Algebra::Path->new( subject => $o, path => $pp, object => $s );
					return $self->evaluate($p, $active_graph);
				} else { # Term ZeroOrMorePath(path) Term
					my $v	= {};
					$self->_ALP($active_graph, $s, $child, $v);
					my @results;
					foreach my $v (values %$v) {
						if ($v->equals($o)) {
							return Attean::ListIterator->new(values => [Attean::Result->new()], item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
						}
					}
					return Attean::ListIterator->new(values => [], item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
				}
			} elsif ($path->isa('Attean::Algebra::ZeroOrOnePath')) {
				my ($child)	= @{ $path->children };
				my $path	= Attean::Algebra::Path->new( subject => $s, path => $child, object => $o );
				my @iters;
				push(@iters, $self->evaluate( $path, $active_graph ));
				push(@iters, $self->_zeroLengthPath($s, $o, $active_graph));
				return Attean::IteratorSequence->new( iterators => \@iters, item_type => Type::Tiny::Role->new(role => 'Attean::API::Result') );
			} else {
				die "Unexpected path type: $path";
			}
			die "Unimplemented path type: $path";
		} elsif ($algebra->isa('Attean::Algebra::Project')) {
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate( $child, $active_graph );
			my @vars	= map { $_->value } @{ $algebra->variables };
			return $iter->map(sub {
				my $r	= shift;
				my $b	= { map {
					my $t	= $r->value($_);
					$t	? ($_ => $t)
						: ()
				} @vars };
				return Attean::Result->new( bindings => $b );
			});
		} elsif ($algebra->isa('Attean::Algebra::Slice')) {
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate( $child, $active_graph );
			if ($algebra->offset > 0) {
				$iter	= $iter->offset($algebra->offset);
			}
			if ($algebra->limit >= 0) {
				$iter	= $iter->limit($algebra->limit);
			}
			return $iter->grep(sub {
				my $r	= shift;
				say "SLICE Result: " . $r->as_string;
				return 1;
			});
		} elsif ($algebra->isa('Attean::Algebra::Union')) {
			my @iters	= map { $self->evaluate($_, $active_graph) } @{ $algebra->children };
			return Attean::IteratorSequence->new( iterators => \@iters, item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
		} elsif ($algebra->isa('Attean::Algebra::Ask')) {
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate($child, $active_graph);
			my $result	= $iter->next;
			my $true	= Attean::Literal->true;
			my $false	= Attean::Literal->false;
			return Attean::ListIterator->new(values => [$result ? $true : $false], item_type => Type::Tiny::Role->new(role => 'Attean::API::Term'));
		} elsif ($algebra->isa('Attean::Algebra::Construct')) {
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate($child, $active_graph);
			my $triples	= $algebra->triples;
			my %seen;
			return Attean::CodeIterator->new(
				generator => sub {
					my $r	= $iter->next;
					return unless ($r);
					my %mapping			= map {
						my $t	= $r->value($_);
						$t ? ("?$_" => $t) : ();
					} ($r->variables);
					my $mapper	= Attean::TermMap->rewrite_map(\%mapping);
				
					my @triples;
					foreach my $pattern (@$triples) {
						my $triple	= Attean::Triple->new($pattern->apply_map($mapper)->values);
						push(@triples, $triple) unless ($triple->has_blanks);
					}
					return @triples;
				},
				item_type => Type::Tiny::Role->new(role => 'Attean::API::Triple')
			)->grep(sub {
				return not($seen{shift->as_string}++);
			});
		} elsif ($algebra->isa('Attean::Algebra::Table')) {
			return Attean::ListIterator->new(values => $algebra->rows, item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
		} else {
			die "Unexpected algebra type: $algebra";
		}

		die "Unimplemented algebra evaluation for: $algebra";
	}

	sub _ALP {
		my $self	= shift;
		my $graph	= shift;
		my $term	= shift;
		my $path	= shift;
		my $v		= shift;
		return if (exists $v->{ $term->as_string });
		$v->{ $term->as_string }	= $term;
		
		my $iter	= $self->_eval($graph, $term, $path);
		while (my $n = $iter->next) {
			$self->_ALP($graph, $n, $path, $v);
		}
	}
	
	sub _eval {
		my $self	= shift;
		my $graph	= shift;
		my $term	= shift;
		my $path	= shift;
		my $pp		= Attean::Algebra::Path->new( subject => $term, path => $path, object => variable('o') );
		my $iter	= $self->evaluate($pp, $graph);
		my $terms	= $iter->map(sub { shift->value('o') }, Type::Tiny::Role->new(role => 'Attean::API::Term'));
		my %seen;
		return $terms->grep(sub { not $seen{ shift->as_string }++ });
	}
	
	sub _zeroLengthPath {
		my $self	= shift;
		my $s		= shift;
		my $o		= shift;
		my $graph	= shift;
		my $s_term	= ($s->does('Attean::API::Term'));
		my $o_term	= ($o->does('Attean::API::Term'));
		if ($s_term and $o_term) {
			my @r;
			if ($s->equals($o)) {
				push(@r, Attean::Result->new());
			}
			return Attean::ListIterator->new(values => \@r, item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
		} elsif ($s_term) {
			my $name	= $o->value;
			my $r		= Attean::Result->new( bindings => { $name => $s } );
			return Attean::ListIterator->new(values => [$r], item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
		} elsif ($o_term) {
			my $name	= $s->value;
			my $r		= Attean::Result->new( bindings => { $name => $o } );
			return Attean::ListIterator->new(values => [$r], item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
		} else {
			my @vars	= map { $_->value } ($s, $o);
			my $nodes	= $self->model->graph_nodes( $graph );
			return $nodes->map(sub {
				my $term	= shift;
				Attean::Result->new( bindings => { map { $_ => $term } @vars } );
			}, Type::Tiny::Role->new(role => 'Attean::API::Result'));
		}
	}
}


1;

__END__

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
