use v5.14;
use warnings;

=head1 NAME

Attean::SimpleQueryEvaluator - Simple query evaluator

=head1 VERSION

This document describes Attean::SimpleQueryEvaluator version 0.032

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $algebra = Attean->get_parser('SPARQL')->parse('SELECT * WHERE { ... }');
  my $active_graph = Attean::IRI->new('http://example.org/');
  my $e = Attean::SimpleQueryEvaluator->new( model => $model );
  my $iter = $e->evaluate( $algebra, $active_graph );

=head1 DESCRIPTION

The Attean::SimpleQueryEvaluator class implements a simple query evaluator that,
given an L<Attean::API::Algebra|Attean::API::Query> and a L<Attean::API::Model>
object, evaluates the query represented by the algebra using data from the
model, and returns a query result.

=head1 ATTRIBUTES

=over 4

=cut

use Attean::Algebra;
use Attean::Expression;

package Attean::SimpleQueryEvaluator 0.032 {
	use Moo;
	use Encode qw(encode);
	use Attean::RDF;
	use AtteanX::Functions::CompositeLists;
	use AtteanX::Functions::CompositeMaps;
	use LWP::UserAgent;
	use Scalar::Util qw(blessed);
	use List::Util qw(all any reduce);
	use Types::Standard qw(ConsumerOf InstanceOf Bool Object);
	use URI::Escape;
	use Attean::SPARQLClient;
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

	has 'user_agent' => (is => 'rw', isa => InstanceOf['LWP::UserAgent'], default => sub { my $ua = LWP::UserAgent->new(); $ua->agent("Attean/$Attean::VERSION " . $ua->_agent); $ua });
	
=item C<< request_signer >>

If set, used to modify HTTP::Request objects used in evaluating SERVICE calls
before the request is made. This may be used to, for example, add cryptographic
signature headers to the request. The modification is performed by calling
C<< $request_signer->sign( $request ) >>.

=cut

	has 'request_signer' => (is => 'rw', isa => Object);
	
	has 'ground_blanks' => (is => 'rw', isa => Bool, default => 0);
	
=back

=head1 METHODS

=over 4

=item C<< evaluate( $algebra, $active_graph ) >>

Returns an L<Attean::API::Iterator> object with results produced by evaluating
the query C<< $algebra >> against the evaluator's C<< model >>, using the
supplied C<< $active_graph >>.

=cut

	sub evaluate {
		my $self			= shift;
		my $algebra			= shift;
		my $active_graph	= shift || Carp::confess "No active-graph passed to Attean::SimpleQueryEvaluator->evaluate";
		
		Carp::confess "No algebra passed for evaluation" unless ($algebra);
		
		my $expr_eval	= Attean::SimpleQueryEvaluator::ExpressionEvaluator->new( evaluator => $self );

		my @children	= @{ $algebra->children };
		my ($child)		= $children[0];
		if ($algebra->isa('Attean::Algebra::Query') or $algebra->isa('Attean::Algebra::Update')) {
			return $self->evaluate($algebra->child, $active_graph, @_);
		} elsif ($algebra->isa('Attean::Algebra::BGP')) {
			my @triples	= @{ $algebra->triples };
			if (scalar(@triples) == 0) {
				my $b	= Attean::Result->new( bindings => {} );
				return Attean::ListIterator->new(variables => [], values => [$b], item_type => 'Attean::API::Result');
			} else {
				my @iters;
				my @new_vars;
				my %blanks;
				foreach my $t (@triples) {
					push(@iters, $self->evaluate_pattern($t, $active_graph, \@new_vars, \%blanks));
				}
				while (scalar(@iters) > 1) {
					my ($lhs, $rhs)	= splice(@iters, 0, 2);
					unshift(@iters, $lhs->join($rhs));
				}
				return shift(@iters)->map(sub { shift->project_complement(@new_vars) });
			}
		} elsif ($algebra->isa('Attean::Algebra::Distinct') or $algebra->isa('Attean::Algebra::Reduced')) {
			my %seen;
			my $iter	= $self->evaluate( $child, $active_graph );
			return $iter->grep(sub {
				my $r	= shift;
				my $str	= $r->as_string;
				my $ok	= not($seen{ $str }) ? 1 : 0;
				$seen{ $str }++;
				return $ok;
			});
		} elsif ($algebra->isa('Attean::Algebra::Extend')) {
			my $child	= $algebra;
			my @extends;
			my %extends;
			while ($child->isa('Attean::Algebra::Extend')) {
				my $expr			= $child->expression;
				my $var				= $child->variable->value;
				$extends{ $var }	= $expr;
				unshift(@extends, $var);
				($child)			= @{ $child->children };
			}
			return $self->evaluate( $child, $active_graph )->map(sub {
				my $r	= shift;
				my %extension;
				my %row_cache;
				foreach my $var (@extends) {
					my $expr	= $extends{ $var };
					my $val		= $expr_eval->evaluate_expression( $expr, $r, $active_graph, \%row_cache );
					if (blessed($val) and $val->does('Attean::API::Binding')) {
						# patterns need to be made ground to be bound as values (e.g. TriplePattern -> Triple)
						$val	= $val->ground($r);
					}
# 					warn "Extend error: $@" if ($@);
					$r	= Attean::Result->new( bindings => { $var => $val } )->join($r) if ($val);
				}
				return $r;
			});
		} elsif ($algebra->isa('Attean::Algebra::Unfold')) {
			my $expr			= $algebra->expression;
			my @vars			= map { $_->value } @{ $algebra->variables };
			my ($first, $second);
    		$first	= $vars[0];
			if (scalar(@vars) == 2) {
				$second	= $vars[1];
			}

			my ($child)			= @{ $algebra->children };
			my $iter				= $self->evaluate( $child, $active_graph );
			my @results;
			while (my $r = $iter->next) {
				my %extension;
				my %row_cache;
				my $val		= $expr_eval->evaluate_expression( $expr, $r, $active_graph, \%row_cache );
				my $dt	= $val->datatype;
				if ($dt->value eq $AtteanX::Functions::CompositeLists::LIST_TYPE_IRI) {
					my @nodes	= AtteanX::Functions::CompositeLists::lex_to_list($val);
					foreach my $i (0 .. $#nodes) {
						my $val	= $nodes[$i];
						my %bindings;
						if (defined($val)) {
							if ($val->does('Attean::API::Binding')) {
								# patterns need to be made ground to be bound as values (e.g. TriplePattern -> Triple)
								$val	= $val->ground($r);
							}
			# 					warn "Unfold error: $@" if ($@);
							$bindings{$first}	= $val if ($val);
						}
					
						if (defined($second)) {
							$bindings{$second}	= Attean::Literal->integer(1+$i);
						}
						my $new	= Attean::Result->new( bindings => \%bindings )->join($r);
						push(@results, $new);
					}
				} elsif ($dt->value eq $AtteanX::Functions::CompositeMaps::MAP_TYPE_IRI) {
					my @nodes	= AtteanX::Functions::CompositeMaps::lex_to_map($val);
					# The namespace map here is used to correctly parse maps with boolean keys like `{true:1}`
					my $p		= AtteanX::Parser::Turtle->new();
					while (my ($key_string, $val) = splice(@nodes, 0, 2)) {
						my $key	= $p->parse_node($key_string); # TODO: this mistakes "true:4" as an attempted prefixname, not a key-value pair
						my %bindings;
						if (defined($first)) {
							if ($key->does('Attean::API::Binding')) {
								# patterns need to be made ground to be bound as values (e.g. TriplePattern -> Triple)
								$key	= $key->ground($r);
							}
			# 					warn "Unfold error: $@" if ($@);
							$bindings{$first}	= $key if ($key);
						}

						if (defined($second)) {
							if (blessed($val) and $val->does('Attean::API::Binding')) {
								# patterns need to be made ground to be bound as values (e.g. TriplePattern -> Triple)
								$val	= $val->ground($r);
							}
			# 					warn "Unfold error: $@" if ($@);
							$bindings{$second}	= $val if ($val);
						}
					
						my $new	= Attean::Result->new( bindings => \%bindings )->join($r);
						push(@results, $new);
					}
				}
			}
			my %vars = map { $_ => 1 } $iter->variables;
			$vars{$first}++;
			$vars{$second}++ if defined($second);
			return Attean::ListIterator->new(variables => [keys %vars], values => \@results, item_type => 'Attean::API::Result');
		} elsif ($algebra->isa('Attean::Algebra::Filter')) {
			# TODO: Merge adjacent filter evaluation so that they can share a row_cache hash (as is done for Extend above)
			my $expr	= $algebra->expression;
			my $iter	= $self->evaluate( $child, $active_graph );
			return $iter->grep(sub {
				my $t	= $expr_eval->evaluate_expression( $expr, shift, $active_graph, {} );
# 				if ($@) { warn "Filter evaluation: $@\n" };
				return ($t ? $t->ebv : 0);
			});
		} elsif ($algebra->isa('Attean::Algebra::OrderBy')) {
			local($Attean::API::Binding::ALLOW_IRI_COMPARISON)	= 1;
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
					my ($av, $bv)	= map { $_->[$i] } ($avalues, $bvalues);
					# Mirrors code in Attean::Plan::OrderBy->sort_rows
					if (blessed($av) and $av->does('Attean::API::Binding') and (not(defined($bv)) or not($bv->does('Attean::API::Binding')))) {
						$c	= 1;
					} elsif (blessed($bv) and $bv->does('Attean::API::Binding') and (not(defined($av)) or not($av->does('Attean::API::Binding')))) {
						$c	= -1;
					} else {
						$c		= eval { $av ? $av->compare($bv) : 1 };
						if ($@) {
							$c	= 1;
						}
					}
					$c		*= -1 if ($dirs[$i] == 0);
					last unless ($c == 0);
				}
				$c
			} map { my $r = $_; [$r, [map { $expr_eval->evaluate_expression( $_, $r, $active_graph, {} ) } @exprs]] } @rows;
			return Attean::ListIterator->new( values => \@sorted, item_type => $iter->item_type, variables => $iter->variables);
		} elsif ($algebra->isa('Attean::Algebra::Service')) {
			my $endpoint	= $algebra->endpoint->value;
			my ($pattern)	= @{ $algebra->children };
			my $sparql		= Attean::Algebra::Project->new( variables => [ map { variable($_) } $pattern->in_scope_variables ], children => [ $pattern ] )->as_sparql;
			my $silent		= $algebra->silent;
			my $client		= Attean::SPARQLClient->new(
				endpoint => $endpoint,
				silent => $silent,
				user_agent => $self->user_agent,
				request_signer => $self->request_signer,
			);
			return $client->query($sparql);
		} elsif ($algebra->isa('Attean::Algebra::Graph')) {
			my $graph	= $algebra->graph;
			return $self->evaluate($child, $graph) if ($graph->does('Attean::API::Term'));
			
			my @iters;
			my $graphs	= $self->model->get_graphs();
			my %vars;
			while (my $g = $graphs->next) {
				next if ($g->value eq $self->default_graph->value);
				my $gr	= Attean::Result->new( bindings => { $graph->value => $g } );
				my $iter	= $self->evaluate($child, $g)->map(sub { if (my $result = shift->join($gr)) { return $result } else { return } });
				foreach my $v (@{ $iter->variables }) {
					$vars{$v}++;
				}
				push(@iters, $iter);
			}
			return Attean::IteratorSequence->new( variables => [keys %vars], iterators => \@iters, item_type => 'Attean::API::Result' );
		} elsif ($algebra->isa('Attean::Algebra::Group')) {
			my @groupby	= @{ $algebra->groupby };
			my $iter	= $self->evaluate($child, $active_graph);
			my %groups;
			while (my $r = $iter->next) {
				my %vars;
				my %row_cache;
				my @group_terms	= map { $expr_eval->evaluate_expression( $_, $r, $active_graph, \%row_cache ) } @groupby;
				my $key			= join(' ', map { blessed($_) ? $_->as_string : '' } @group_terms);
				my %group_bindings;
				foreach my $i (0 .. $#group_terms) {
					my $v	= $groupby[$i];
					if (blessed($v) and $v->isa('Attean::ValueExpression') and $v->value->does('Attean::API::Variable') and $group_terms[$i]) {
						$group_bindings{$v->value->value}	= $group_terms[$i];
					}
				}
				$groups{$key}	= [Attean::Result->new( bindings => \%group_bindings ), []] unless (exists($groups{$key}));
				push(@{ $groups{$key}[1] }, $r);
			}
			my @keys	= keys %groups;
			$groups{''}	= [Attean::Result->new( bindings => {} ), []] if (scalar(@keys) == 0);
			my $aggs	= $algebra->aggregates;
			my @results;
			my %vars;
			foreach my $key (keys %groups) {
				my %row_cache;
				my ($binding, $rows)	= @{ $groups{$key} };
				my $count	= scalar(@$rows);
				my %bindings;
				foreach my $i (0 .. $#{ $aggs }) {
					my $name	= $aggs->[$i]->variable->value;
					my $term	= $expr_eval->evaluate_expression( $aggs->[$i], $rows, $active_graph, {} );
# 					warn "AGGREGATE error: $@" if ($@);
					$vars{$name}++;
					$bindings{ $name } = $term if ($term);
				}
				push(@results, Attean::Result->new( bindings => \%bindings )->join($binding));
			}
			return Attean::ListIterator->new(variables => [keys %vars], values => \@results, item_type => 'Attean::API::Result');
		} elsif ($algebra->isa('Attean::Algebra::Join')) {
			my ($lhs, $rhs)	= map { $self->evaluate($_, $active_graph) } @children;
			return $lhs->join($rhs);
		} elsif ($algebra->isa('Attean::Algebra::LeftJoin')) {
			my $expr	= $algebra->expression;
			my ($lhs_iter, $rhs_iter)	= map { $self->evaluate($_, $active_graph) } @children;
			my @rhs		= $rhs_iter->elements;
			my @results;
			my %vars	= map { $_ => 1 } (@{ $lhs_iter->variables }, @{ $rhs_iter->variables });
			while (my $lhs = $lhs_iter->next) {
				my $joined	= 0;
				foreach my $rhs (@rhs) {
					if (my $j = $lhs->join($rhs)) {
						if ($expr_eval->evaluate_expression( $expr, $j, $active_graph, {} )->ebv) {
							$joined++;
							push(@results, $j);
						}
					}
				}
				push(@results, $lhs) unless ($joined);
			}
			return Attean::ListIterator->new( variables => [keys %vars], values => \@results, item_type => 'Attean::API::Result');
		} elsif ($algebra->isa('Attean::Algebra::Minus')) {
			my ($lhsi, $rhs)	= map { $self->evaluate($_, $active_graph) } @children;
			my @rhs				= $rhs->elements;
			my @results;
			while (my $lhs = $lhsi->next) {
				my @compatible;
				my @disjoint;
				RHS: foreach my $rhs (@rhs) {
					if (my $j = $lhs->join($rhs)) {
						push(@compatible, 1);
					} else {
						push(@compatible, 0);
					}
					
					my $intersects	= 0;
					my %lhs_dom	= map { $_ => 1 } $lhs->variables;
					foreach my $rvar ($rhs->variables) {
						if (exists $lhs_dom{$rvar}) {
							$intersects	= 1;
						}
					}
					push(@disjoint, not($intersects));
				}
				
				my $count	= scalar(@rhs);
				my $keep	= 1;
				foreach my $i (0 .. $#rhs) {
					$keep	= 0 unless ($compatible[$i] == 0 or $disjoint[$i] == 1);
				}
				
				push(@results, $lhs) if ($keep);
			}
			return Attean::ListIterator->new( variables => $lhsi->variables, values => \@results, item_type => 'Attean::API::Result');
		} elsif ($algebra->isa('Attean::Algebra::Path')) {
			my $s			= $algebra->subject;
			my $path		= $algebra->path;
			my $o			= $algebra->object;
			my @children	= @{ $path->children };
			my ($child)		= $children[0];
			
			return $self->model->get_bindings( $s, $path->predicate, $o, $active_graph ) if ($path->isa('Attean::Algebra::PredicatePath'));
			if ($path->isa('Attean::Algebra::InversePath')) {
				my $path	= Attean::Algebra::Path->new( subject => $o, path => $child, object => $s );
				return $self->evaluate( $path, $active_graph );
			} elsif ($path->isa('Attean::Algebra::AlternativePath')) {
				my @children	= @{ $path->children };
				my @algebras	= map { Attean::Algebra::Path->new( subject => $s, path => $_, object => $o ) } @children;
				my @iters		= map { $self->evaluate($_, $active_graph) } @algebras;
				return Attean::IteratorSequence->new( iterators => \@iters, item_type => $iters[0]->item_type, variables => [$algebra->in_scope_variables] );
			} elsif ($path->isa('Attean::Algebra::NegatedPropertySet')) {
				my $preds	= $path->predicates;
				my %preds	= map { $_->value => 1 } @$preds;
				my $filter	= $self->model->get_quads($s, undef, $o, $active_graph)->grep(sub {
					my $q	= shift;
					my $p	= $q->predicate;
					return not exists $preds{ $p->value };
				});
				my %vars;
				$vars{subject}	= $s->value if ($s->does('Attean::API::Variable'));
				$vars{object}	= $o->value if ($o->does('Attean::API::Variable'));
				return $filter->map(sub {
					my $q	= shift;
					return unless $q;
					my %bindings	= map { $vars{$_} => $q->$_() } (keys %vars);
					return Attean::Result->new( bindings => \%bindings );
				}, 'Attean::API::Result', variables => [values %vars]);
			} elsif ($path->isa('Attean::Algebra::SequencePath')) {
				if (scalar(@children) == 1) {
					my $path	= Attean::Algebra::Path->new( subject => $s, path => $children[0], object => $o );
					return $self->evaluate($path, $active_graph);
				} else {
					my @paths;
					my $first		= shift(@children);
					my $join		= Attean::Variable->new();
					my @new_vars	= ($join->value);
					push(@paths, Attean::Algebra::Path->new( subject => $s, path => $first, object => $join ));
					foreach my $i (0 .. $#children) {
						my $newjoin	= Attean::Variable->new();
						my $obj		= ($i == $#children) ? $o : $newjoin;
						push(@new_vars, $newjoin->value);
						push(@paths, Attean::Algebra::Path->new( subject => $join, path => $children[$i], object => $obj ));
						$join	= $newjoin;
					}
					
					while (scalar(@paths) > 1) {
						my ($l, $r)	= splice(@paths, 0, 2);
						unshift(@paths, Attean::Algebra::Join->new( children => [$l, $r] ));
					}
					return $self->evaluate(shift(@paths), $active_graph)->map(sub { shift->project_complement(@new_vars) });
				}
			} elsif ($path->isa('Attean::Algebra::ZeroOrMorePath') or $path->isa('Attean::Algebra::OneOrMorePath')) {
				if ($s->does('Attean::API::TermOrTriple') and $o->does('Attean::API::Variable')) {
					my $v	= {};
					if ($path->isa('Attean::Algebra::ZeroOrMorePath')) {
						$self->_ALP($active_graph, $s, $child, $v);
					} else {
						my $iter	= $self->_eval($active_graph, $s, $child);
						while (my $n = $iter->next) {
							$self->_ALP($active_graph, $n, $child, $v);
						}
					}
					my @results	= map { Attean::Result->new( bindings => { $o->value => $_ } ) } (values %$v);
					return Attean::ListIterator->new(variables => [$o->value], values => \@results, item_type => 'Attean::API::Result');
				} elsif ($s->does('Attean::API::Variable') and $o->does('Attean::API::Variable')) {
					my $nodes	= $self->model->graph_nodes( $active_graph );
					my @results;
					while (my $t = $nodes->next) {
						my $tr		= Attean::Result->new( bindings => { $s->value => $t } );
						my $p		= Attean::Algebra::Path->new( subject => $t, path => $path, object => $o );
						my $iter	= $self->evaluate($p, $active_graph);
						while (my $r = $iter->next) {
							push(@results, $r->join($tr));
						}
					}
					my %vars	= map { $_ => 1 } ($s->value, $o->value);
					return Attean::ListIterator->new(variables => [keys %vars], values => \@results, item_type => 'Attean::API::Result');
				} elsif ($s->does('Attean::API::Variable') and $o->does('Attean::API::TermOrTriple')) {
					my $pp	= Attean::Algebra::InversePath->new( children => [$child] );
					my $p	= Attean::Algebra::Path->new( subject => $o, path => $pp, object => $s );
					return $self->evaluate($p, $active_graph);
				} else { # Term ZeroOrMorePath(path) Term
					my $v	= {};
					$self->_ALP($active_graph, $s, $child, $v);
					my @results;
					foreach my $v (values %$v) {
						return Attean::ListIterator->new(variables => [], values => [Attean::Result->new()], item_type => 'Attean::API::Result')
							if ($v->equals($o));
					}
					return Attean::ListIterator->new(variables => [], values => [], item_type => 'Attean::API::Result');
				}
			} elsif ($path->isa('Attean::Algebra::ZeroOrOnePath')) {
				my $path	= Attean::Algebra::Path->new( subject => $s, path => $child, object => $o );
				my @iters;
				my %seen;
				push(@iters, $self->evaluate( $path, $active_graph )->grep(sub { return not($seen{shift->as_string}++); }));
				push(@iters, $self->_zeroLengthPath($s, $o, $active_graph));
				my %vars;
				foreach my $iter (@iters) {
					$vars{$_}++ for (@{ $iter->variables });
				}
				return Attean::IteratorSequence->new( iterators => \@iters, item_type => 'Attean::API::Result', variables => [keys %vars] );
			}
			die "Unimplemented path type: $path";
		} elsif ($algebra->isa('Attean::Algebra::Project')) {
			my $iter	= $self->evaluate( $child, $active_graph );
			my @vars	= map { $_->value } @{ $algebra->variables };
			return $iter->map(sub {
				my $r	= shift;
				my $b	= { map { my $t	= $r->value($_); $t	? ($_ => $t) : () } @vars };
				return Attean::Result->new( bindings => $b );
			}, undef, variables => \@vars); #->debug('Project result');
		} elsif ($algebra->isa('Attean::Algebra::Slice')) {
			my $iter	= $self->evaluate( $child, $active_graph );
			$iter		= $iter->offset($algebra->offset) if ($algebra->offset > 0);
			$iter		= $iter->limit($algebra->limit) if ($algebra->limit >= 0);
			return $iter;
		} elsif ($algebra->isa('Attean::Algebra::Union')) {
			my ($lhs, $rhs)	= map { $self->evaluate($_, $active_graph) } @children;
			return Attean::IteratorSequence->new(
				iterators => [$lhs, $rhs],
				item_type => 'Attean::API::Result',
				variables => [$algebra->in_scope_variables]
			);
		} elsif ($algebra->isa('Attean::Algebra::Ask')) {
			my $iter	= $self->evaluate($child, $active_graph);
			my $result	= $iter->next;
			return Attean::ListIterator->new(values => [$result ? Attean::Literal->true : Attean::Literal->false], item_type => 'Attean::API::Term');
		} elsif ($algebra->isa('Attean::Algebra::Construct')) {
			my $iter		= $self->evaluate($child, $active_graph);
			my $patterns	= $algebra->triples;
			use Data::Dumper;
			my %seen;
			return Attean::CodeIterator->new(
				generator => sub {
					my $r	= $iter->next;
					return unless ($r);
					my %mapping	= map { my $t = $r->value($_); $t ? ("?$_" => $t) : (); } ($r->variables);
					my $mapper	= Attean::TermMap->rewrite_map(\%mapping);
					my @triples;
					PATTERN: foreach my $p (@$patterns) {
						my @terms	= map {
							($_->does('Attean::API::TriplePattern'))
								? $_->as_triple
								: $_
						} $p->apply_map($mapper)->values;
						unless (all { $_->does('Attean::API::TermOrTriple') } @terms) {
							next PATTERN;
						}
						push(@triples, Attean::Triple->new(@terms));
					}
					return @triples;
				},
				item_type => 'Attean::API::Triple'
			)->grep(sub { return not($seen{shift->as_string}++); });
		} elsif ($algebra->isa('Attean::Algebra::Table')) {
			my $vars	= [map { $_->value } @{ $algebra->variables }];
			return Attean::ListIterator->new(variables => $vars, values => $algebra->rows, item_type => 'Attean::API::Result');
		}
		die "Unimplemented simple algebra evaluation for: $algebra";
	}

	
=item C<< evaluate_pattern( $pattern, $active_graph, \@new_vars, \%blanks ) >>

Returns an L<Attean::API::Iterator> object with results produced by evaluating
the triple- or quad-pattern C<< $pattern >> against the evaluator's
C<< model >>, using the supplied C<< $active_graph >>.

If the C<< ground_blanks >> option is false, replaces blank nodes in the
pattern with fresh variables before evaluation, and populates C<< %blanks >>
with pairs ($variable_name => $variable_node). Each new variable is also
appended to C<< @new_vars >> as it is created.

=cut

	sub evaluate_pattern {
		my $self			= shift;
		my $t				= shift;
		my $active_graph	= shift || Carp::confess "No active-graph passed to Attean::SimpleQueryEvaluator->evaluate";
		my $new_vars		= shift;
		my $blanks			= shift;
		my $q				= $t->as_quad_pattern($active_graph);
		my @values;
		foreach my $v ($q->values) {
			if (not($self->ground_blanks) and $v->does('Attean::API::Blank')) {
				unless (exists $blanks->{$v->value}) {
					$blanks->{$v->value}	= Attean::Variable->new();
					push(@$new_vars, $blanks->{$v->value}->value);
				}
				push(@values, $blanks->{$v->value});
			} else {
				push(@values, $v);
			}
		}
		return $self->model->get_bindings( @values );
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
		my $terms	= $iter->map(sub { shift->value('o') }, 'Attean::API::Term');
		my %seen;
		return $terms->grep(sub { not $seen{ shift->as_string }++ });
	}
	
	sub _zeroLengthPath {
		my $self	= shift;
		my $s		= shift;
		my $o		= shift;
		my $graph	= shift;
		my $s_term	= ($s->does('Attean::API::TermOrTriple'));
		my $o_term	= ($o->does('Attean::API::TermOrTriple'));
		if ($s_term and $o_term) {
			my @r;
			push(@r, Attean::Result->new()) if ($s->equals($o));
			return Attean::ListIterator->new(variables => [], values => \@r, item_type => 'Attean::API::Result');
		} elsif ($s_term) {
			my $name	= $o->value;
			my $r		= Attean::Result->new( bindings => { $name => $s } );
			return Attean::ListIterator->new(variables => [$name], values => [$r], item_type => 'Attean::API::Result');
		} elsif ($o_term) {
			my $name	= $s->value;
			my $r		= Attean::Result->new( bindings => { $name => $o } );
			return Attean::ListIterator->new(variables => [$name], values => [$r], item_type => 'Attean::API::Result');
		} else {
			my @vars	= map { $_->value } ($s, $o);
			my $nodes	= $self->model->graph_nodes( $graph );
			return $nodes->map(
				sub {
					my $term	= shift;
					Attean::Result->new( bindings => { map { $_ => $term } @vars } );
				},
				'Attean::API::Result',
				variables => \@vars
			);
		}
	}
}

package Attean::SimpleQueryEvaluator::ExpressionEvaluator 0.032 {
	use Moo;
	use Attean::RDF;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(InstanceOf);
	use URI::Escape qw(uri_escape_utf8);
	use Encode qw(encode);
	use POSIX qw(ceil floor);
	use Digest;
	use UUID::Tiny ':std';
	use List::MoreUtils qw(zip);
	use DateTime::Format::W3CDTF;
	use I18N::LangTags;
	use namespace::clean;
	
	has 'evaluator'	=> (is => 'ro', isa => InstanceOf['Attean::SimpleQueryEvaluator']);
	sub evaluate_expression {
		my $self			= shift;
		my $expr			= shift;
		my $row				= shift;
		my $active_graph	= shift;
		my $row_cache		= shift || {};
		my $impl			= $self->impl($expr, $active_graph);
		my $result			= eval { $impl->($row, row_cache => $row_cache) };
		return $result;
	}
	
	sub impl {
		my $self			= shift;
		my $expr			= shift;
		my $active_graph	= shift;
		my $op		= $expr->operator;
		my $true	= Attean::Literal->true;
		my $false	= Attean::Literal->false;
		if ($expr->isa('Attean::ExistsExpression')) {
			my $pattern		= $expr->pattern;
			return sub {
				my $r			= shift;
				my $table		= Attean::Algebra::Table->new( variables => [map { variable($_) } $r->variables], rows => [$r] );
				my $join		= Attean::Algebra::Join->new( children => [$table, $pattern] );
				# TODO: substitute variables at top-level of EXISTS pattern
				my $iter	= $self->evaluator->evaluate($join, $active_graph);
				return ($iter->next) ? $true : $false;
			};
		} elsif ($expr->isa('Attean::ValueExpression')) {
			my $node	= $expr->value;
			if ($node->does('Attean::API::Variable')) {
				return sub { return shift->value($node->value); };
			} else {
				return sub { return $node };
			}
		} elsif ($expr->isa('Attean::UnaryExpression')) {
			my ($child)	= @{ $expr->children };
			my $impl	= $self->impl($child, $active_graph);
			if ($op eq '!') {
				return sub {
					my $term	= $impl->(@_);
					return ($term->ebv) ? $false : $true;
				}
			} elsif ($op eq '-' or $op eq '+') {
				return sub {
					my $term	= $impl->(@_);
					die "TypeError $op" unless (blessed($term) and $term->does('Attean::API::NumericLiteral'));
					my $v	= $term->numeric_value;
					return Attean::Literal->new( value => eval "$op$v", datatype => $term->datatype );
				};
			}
			die "Unimplemented UnaryExpression evaluation: " . $expr->operator;
		} elsif ($expr->isa('Attean::BinaryExpression')) {
			my ($lhs, $rhs)	= @{ $expr->children };
			my ($lhsi, $rhsi)	= map { $self->impl($_, $active_graph) } ($lhs, $rhs);
			if ($op eq '&&') {
				return sub {
					my ($r, %args)	= @_;
					my $lbv	= eval { $lhsi->($r, %args) };
					my $rbv	= eval { $rhsi->($r, %args) };
					die "TypeError $op" unless ($lbv or $rbv);
					return $false if (not($lbv) and not($rbv->ebv));
					return $false if (not($rbv) and not($lbv->ebv));
					die "TypeError $op" unless ($lbv and $rbv);
					return ($lbv->ebv && $rbv->ebv) ? $true : $false;
				}
			} elsif ($op eq '||') {
				return sub {
					my ($r, %args)	= @_;
					my $lbv	= eval { $lhsi->($r, %args) };
					return $true if ($lbv and $lbv->ebv);
					my $rbv	= eval { $rhsi->($r, %args) };
					die "TypeError $op" unless ($rbv);
					return $true if ($rbv->ebv);
					return $false if ($lbv);
					die "TypeError $op";
				}
			} elsif ($op =~ m#^(?:[-+*/])$#) { # numeric operators: - + * /
				return sub {
					my ($r, %args)	= @_;
					($lhs, $rhs)	= map { $_->($r, %args) } ($lhsi, $rhsi);
					for ($lhs, $rhs) { die "TypeError $op" unless (blessed($_) and $_->does('Attean::API::NumericLiteral')); }
					my $lv	= $lhs->numeric_value;
					my $rv	= $rhs->numeric_value;
					return Attean::Literal->new( value => eval "$lv $op $rv", datatype => $lhs->binary_promotion_type($rhs, $op) );
				};
			} elsif ($op =~ /^!?=$/) {
				return sub {
					my ($r, %args)	= @_;
					($lhs, $rhs)	= map { $_->($r, %args) } ($lhsi, $rhsi);
					for ($lhs, $rhs) { die "TypeError $op" unless (blessed($_) and $_->does('Attean::API::TermOrTriple')); }
					my $ok;
					if ($lhs->does('Attean::API::Binding')) {
						$ok	= $lhs->equals($rhs);
					} else {
						$ok	= $lhs->equals($rhs);
					}
					$ok		= not($ok) if ($op eq '!=');
					return $ok ? $true : $false;
				}
			} elsif ($op =~ /^[<>]=?$/) {
				return sub {
					my ($r, %args)	= @_;
					($lhs, $rhs)	= map { $_->($r, %args) } ($lhsi, $rhsi);
					for ($lhs, $rhs) {
						die "TypeError $op" unless $_->does('Attean::API::TermOrTriple');
						die "TypeError $op" if ($_->does('Attean::API::IRI')); # comparison of IRIs is only defined for `ORDER BY`, not for general expressions
					}
					
					my $c	= ($lhs->compare($rhs));
					return $true if (($c < 0 and ($op =~ /<=?/)) or ($c > 0 and ($op =~ />=?/)) or ($c == 0 and ($op =~ /=/)));
					return $false;
				}
			}
			die "Unexpected operator evaluation: $op";
		} elsif ($expr->isa('Attean::FunctionExpression')) {
			my $func			= $expr->operator;
			my @children		= map { $self->impl($_, $active_graph) } @{ $expr->children };
			my %type_roles		= qw(URI IRI IRI IRI BLANK Blank LITERAL Literal NUMERIC NumericLiteral TRIPLE Triple);
			my %type_classes	= qw(URI Attean::IRI IRI Attean::IRI STR Attean::Literal);
			return sub {
				my ($r, %args)	= @_;
				my $row_cache	= $args{row_cache} || {};
			
				if ($func eq 'IF') {
					my $term	= $children[0]->( $r, %args );
					my $ebv		= $term->ebv;
					return $ebv ? $children[1]->( $r, %args ) : $children[2]->( $r, %args );
				} elsif ($func eq 'IN' or $func eq 'NOTIN') {
					($true, $false)	= ($false, $true) if ($func eq 'NOTIN');
					my $child	= shift(@children);
					my $term	= $child->( $r, %args );
					foreach my $c (@children) {
						if (my $value = eval { $c->( $r, %args ) }) {
							return $true if ($term->equals($value));
						}
					}
					return $false;
				} elsif ($func eq 'COALESCE') {
					foreach my $c (@children) {
						my $t	= eval { $c->( $r, %args ) };
						next if ($@);
						return $t if $t;
					}
					return;
				}
			
				if ($func eq 'INVOKE') {
					my $furi	= shift(@children)->( $r, %args )->value;
					if (my $f = Attean->get_global_function($furi)) {
						my @operands	= map { $_->( $r, %args ) } @children;
						return $f->($self->evaluator->model, $active_graph, @operands);
					} elsif (my $fform = Attean->get_global_functional_form($furi)) {
						my @operands	= map { eval { $_->( $r, %args ) } || undef } @children;
						return $fform->($self->evaluator->model, $active_graph, @operands);
					} else {
						die "No extension registered for <$furi>";
					}
				} else {
					my @operands	= map { $_->( $r, %args ) } @children;
					if ($func =~ /^(STR)$/) {
						return $type_classes{$1}->new($operands[0]->value);
					} elsif ($func =~ /^(SUBJECT|PREDICATE|OBJECT)$/) {
						my $pos	= lc($func);
						my $term	= $operands[0]->$pos();
						return $term;
					} elsif ($func =~ /^([UI]RI)$/) {
						my $operand	= $operands[0];
						if ($operand->does('Attean::API::Literal')) {
							if ($operand->datatype->value ne 'http://www.w3.org/2001/XMLSchema#string') {
								die "TypeError: ${func} called with a datatyped-literal other than xsd:string";
							}
						}
						my @base	= $expr->has_base ? (base => $expr->base) : ();
						return $type_classes{$1}->new(value => $operands[0]->value, @base);
					} elsif ($func eq 'BNODE') {
						if (scalar(@operands)) {
							my $name	= $operands[0]->value;
							if (my $b = $row_cache->{bnodes}{$name}) {
								return $b;
							} else {
								my $b	= Attean::Blank->new();
								$row_cache->{bnodes}{$name}	= $b;
								return $b;
							}
						}
						return Attean::Blank->new();
					} elsif ($func eq 'LANG') {
						die "TypeError: LANG" unless ($operands[0]->does('Attean::API::Literal'));
						return Attean::Literal->new($operands[0]->language // '');
					} elsif ($func eq 'LANGMATCHES') {
						my ($lang, $match)	= map { $_->value } @operands;
						if ($match eq '*') {
							# """A language-range of "*" matches any non-empty language-tag string."""
							return ($lang ? $true : $false);
						} else {
							return (I18N::LangTags::is_dialect_of( $lang, $match )) ? $true : $false;
						}
					} elsif ($func eq 'DATATYPE') {
						return $operands[0]->datatype;
					} elsif ($func eq 'BOUND') {
						return $operands[0] ? $true : $false;
					} elsif ($func eq 'RAND') {
						return Attean::Literal->new( value => rand(), datatype => 'http://www.w3.org/2001/XMLSchema#double' );
					} elsif ($func eq 'ABS') {
						return Attean::Literal->new( value => abs($operands[0]->value), $operands[0]->construct_args );
					} elsif ($func =~ /^(?:CEIL|FLOOR)$/) {
						my $v	= $operands[0]->value;
						return Attean::Literal->new( value => (($func eq 'CEIL') ? ceil($v) : floor($v)), $operands[0]->construct_args );
					} elsif ($func eq 'ROUND') {
						return Attean::Literal->new( value => sprintf('%.0f', (0.000000000000001 + $operands[0]->numeric_value)), $operands[0]->construct_args );
					} elsif ($func eq 'CONCAT') {
						my $all_lang	= 1;
						my $all_str		= 1;
						my $lang;
						foreach my $n (@operands) {
							die "CONCAT called with a non-literal argument" unless ($n->does('Attean::API::Literal'));
							if ($n->datatype->value ne 'http://www.w3.org/2001/XMLSchema#string') {
								die "CONCAT called with a datatyped-literal other than xsd:string";
							} elsif ($n->language) {
								$all_str	= 0;
								if (defined($lang) and $lang ne $n->language) {
									$all_lang	= 0;
								} else {
									$lang	= $n->language;
								}
							} else {
								$all_lang	= 0;
								$all_str	= 0;
							}
						}
						my %strtype;
						if ($all_lang and $lang) {
							$strtype{language}	= $lang;
						} elsif ($all_str) {
							$strtype{datatype}	= 'http://www.w3.org/2001/XMLSchema#string'
						}
						return Attean::Literal->new( value => join('', map { $_->value } @operands), %strtype );
					} elsif ($func eq 'SUBSTR') {
						my $str		= shift(@operands);
						my @args	= map { $_->numeric_value } @operands;
						my $v		= scalar(@args == 1) ? substr($str->value, $args[0]-1) : substr($str->value, $args[0]-1, $args[1]);
						return Attean::Literal->new( value => $v, $str->construct_args );
					} elsif ($func eq 'STRLEN') {
						return Attean::Literal->integer(length($operands[0]->value));
					} elsif ($func eq 'REPLACE') {
						my ($node, $pat, $rep)	= @operands;
						die "TypeError: REPLACE called without a literal arg1 term" unless (blessed($node) and $node->does('Attean::API::Literal'));
						die "TypeError: REPLACE called without a literal arg2 term" unless (blessed($pat) and $pat->does('Attean::API::Literal'));
						die "TypeError: REPLACE called without a literal arg3 term" unless (blessed($rep) and $rep->does('Attean::API::Literal'));
						die "TypeError: REPLACE called with a datatyped (non-xsd:string) literal" if ($node->datatype and $node->datatype->value ne 'http://www.w3.org/2001/XMLSchema#string');
						my ($value, $pattern, $replace)	= map { $_->value } @operands;
						die "EvaluationError: REPLACE called with unsafe ?{} match pattern" if (index($pattern, '(?{') != -1 or index($pattern, '(??{') != -1);
						die "EvaluationError: REPLACE called with unsafe ?{} replace pattern" if (index($replace, '(?{') != -1 or index($replace, '(??{') != -1);
	
						$replace	=~ s/\\/\\\\/g;
						$replace	=~ s/\$(\d+)/\$$1/g;
						$replace	=~ s/"/\\"/g;
						$replace	= qq["$replace"];
						no warnings 'uninitialized';
						$value	=~ s/$pattern/"$replace"/eeg;
						return Attean::Literal->new(value => $value, $node->construct_args);
					} elsif ($func =~ /^[UL]CASE$/) {
						return Attean::Literal->new( value => ($func eq 'UCASE' ? uc($operands[0]->value) : lc($operands[0]->value) ), $operands[0]->construct_args );
					} elsif ($func eq 'ENCODE_FOR_URI') {
						return Attean::Literal->new( uri_escape_utf8($operands[0]->value) );
					} elsif ($func eq 'CONTAINS') {
						my ($node, $pat)	= @operands;
						my ($lit, $plit)	= map { $_->value } @operands;
						die "TypeError: CONTAINS" if ($node->language and $pat->language and $node->language ne $pat->language);
						return (index($lit, $plit) >= 0) ? $true : $false;
					} elsif ($func eq 'STRSTARTS' or $func eq 'STRENDS') {
						my ($lit, $plit)	= map { $_->value } @operands;
						if ($func eq 'STRENDS') {
							my $pos		= length($lit) - length($plit);
							return (rindex($lit, $plit) == $pos) ? $true : $false;
						} else {
							return (index($lit, $plit) == 0) ? $true : $false;
						}
					} elsif ($func eq 'STRBEFORE' or $func eq 'STRAFTER') {
						my ($node, $substr)	= @operands;

						die "$func called without a literal arg1 term" unless (blessed($node) and $node->does('Attean::API::Literal'));
						die "$func called without a literal arg2 term" unless (blessed($substr) and $substr->does('Attean::API::Literal'));
						die "$func called with a datatyped (non-xsd:string) literal" if ($node->datatype and $node->datatype->value ne 'http://www.w3.org/2001/XMLSchema#string');

						my $lhs_simple	= (not($node->language) and ($node->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string'));
						my $rhs_simple	= (not($substr->language) and ($substr->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string'));
						if ($lhs_simple and $rhs_simple) {
							# ok
						} elsif ($node->language and $substr->language and $node->language eq $substr->language) {
							# ok
						} elsif ($node->language and $rhs_simple) {
							# ok
						} else {
							die "$func called with literals that are not argument compatible";
						}
	
						my $value	= $node->value;
						my $match	= $substr->value;
						my $i		= index($value, $match, 0);
						if ($i < 0) {
							return Attean::Literal->new('');
						} else {
							if ($func eq 'STRBEFORE') {
								return Attean::Literal->new(value => substr($value, 0, $i), $node->construct_args);
							} else {
								return Attean::Literal->new(value => substr($value, $i+length($match)), $node->construct_args);
							}
						}
					} elsif ($func =~ /^(?:YEAR|MONTH|DAY|HOURS|MINUTES)$/) {
						my $method	= lc($func =~ s/^(HOUR|MINUTE)S$/$1/r);
						my $dt		= $operands[0]->datetime;
						return Attean::Literal->integer($dt->$method());
					} elsif ($func eq 'SECONDS') {
						my $dt	= $operands[0]->datetime;
						return Attean::Literal->decimal($dt->second());
					} elsif ($func eq 'TZ' or $func eq 'TIMEZONE') {
						my $dt	= $operands[0]->datetime;
						my $tz	= $dt->time_zone;
						if ($tz->is_floating) {
							return Attean::Literal->new('') if ($func eq 'TZ');
							die "TIMEZONE called with a dateTime without a timezone";
						}
						return Attean::Literal->new('Z') if ($func eq 'TZ' and $tz->is_utc);
						if ($tz) {
							my $offset	= $tz->offset_for_datetime( $dt );
							my $hours	= 0;
							my $minutes	= 0;
							my $minus	= ($func eq 'TZ') ? '+' : '';
							if ($offset < 0) {
								$minus	= '-';
								$offset	= -$offset;
							}

							my $duration	= "${minus}PT";
							if ($offset >= 60*60) {
								my $h	= int($offset / (60*60));
								$duration	.= "${h}H" if ($h > 0);
								$hours	= int($offset / (60*60));
								$offset	= $offset % (60*60);
							}
							if ($offset >= 60) {
								my $m	= int($offset / 60);
								$duration	.= "${m}M" if ($m > 0);
								$minutes	= int($offset / 60);
								$offset	= $offset % 60;
							}
							my $seconds	= int($offset);
							my $s		= int($offset);
							$duration	.= "${s}S" if ($s > 0 or $duration eq 'PT');
			
							return ($func eq 'TZ')
								? Attean::Literal->new(sprintf('%s%02d:%02d', $minus, $hours, $minutes))
								: Attean::Literal->new( value => $duration, datatype => "http://www.w3.org/2001/XMLSchema#dayTimeDuration");
						} else {
							return Attean::Literal->new('') if ($func eq 'TZ');
							die "TIMEZONE called without a valid dateTime";
						}
					} elsif ($func eq 'NOW') {
						my $value	= DateTime::Format::W3CDTF->new->format_datetime( DateTime->now );
						return Attean::Literal->new( value => $value, datatype => 'http://www.w3.org/2001/XMLSchema#dateTime' );
					} elsif ($func =~ /^(?:STR)?UUID$/) {
						return Attean::Literal->new(uc(uuid_to_string(create_uuid()))) if ($func eq 'STRUUID');
						return Attean::IRI->new('urn:uuid:' . uc(uuid_to_string(create_uuid())));
					} elsif ($func =~ /^(MD5|SHA1|SHA256|SHA384|SHA512)$/) {
						my $hash	= $func =~ s/SHA/SHA-/r;
						my $digest	= eval { Digest->new($hash)->add(encode('UTF-8', $operands[0]->value, Encode::FB_CROAK))->hexdigest };
						return Attean::Literal->new($digest);
					} elsif ($func eq 'STRLANG') {
						my ($str, $lang)	= @operands;
						my @values	= map { $_->value } @operands;
						die "TypeError: STRLANG must be called with two plain literals" unless (blessed($str) and $str->does('Attean::API::Literal') and blessed($lang) and $lang->does('Attean::API::Literal'));
						die "TypeError: STRLANG not called with a simple literal" unless ($str->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string' and not($str->language));
						return Attean::Literal->new( value => $values[0], language => $values[1] );
					} elsif ($func eq 'STRDT') {
						die "TypeError: STRDT" unless ($operands[0]->does('Attean::API::Literal') and not($operands[0]->language));
						if (my $dt = $operands[0]->datatype) {
							die "TypeError: STRDT" unless ($dt->value eq 'http://www.w3.org/2001/XMLSchema#string');
						}
						die "TypeError: STRDT" unless ($operands[1]->does('Attean::API::IRI'));
						my @values	= map { $_->value } @operands;
						return Attean::Literal->new( value => $values[0], datatype => $values[1] );
					} elsif ($func eq 'SAMETERM') {
						my ($a, $b)	= @operands;
						die "TypeError: SAMETERM" unless (blessed($operands[0]) and blessed($operands[1]));
						my $cmp = eval { $a->compare($b) };
						if (not($@) and $cmp) {
							return $false;
						}
						if ($a->does('Attean::API::Binding')) {
							my $ok	= ($a->sameTerms($b));
							return $ok ? $true : $false;
						} else {
							my $ok	= ($a->value eq $b->value);
							return $ok ? $true : $false;
						}
					} elsif ($func =~ /^IS([UI]RI|BLANK|LITERAL|NUMERIC|TRIPLE)$/) {
						return $operands[0]->does("Attean::API::$type_roles{$1}") ? $true : $false;
					} elsif ($func eq 'REGEX') {
						my ($value, $pattern)	= map { $_->value } @operands;
						return ($value =~ /$pattern/) ? $true : $false;
					}
					die "Unimplemented FunctionExpression evaluation: " . $expr->operator;
				}
			};
		} elsif ($expr->isa('Attean::AggregateExpression')) {
			my $agg		= $expr->operator;
			my ($child)	= @{ $expr->children };
			if ($agg eq 'COUNT') {
				if ($child) {
					my $impl	= $self->impl($child, $active_graph);
					return sub {
						my ($rows, %args)	= @_;
						my @terms	= grep { blessed($_) } map { $impl->($_, %args) } @{ $rows };
						if ($expr->distinct) {
							my %seen;
							@terms	= grep { not($seen{$_->as_string}++) } @terms;
						}
						return Attean::Literal->integer(scalar(@terms));
					};
				} else {
					return sub {
						my ($rows, %args)	= @_;
						return Attean::Literal->integer(scalar(@$rows));
					};
				}
			} elsif ($agg =~ /^(?:SAMPLE|MIN|MAX|SUM|AVG|GROUP_CONCAT)$/) {
				my $impl	= $self->impl($child, $active_graph);
				if ($agg eq 'SAMPLE') {
					return sub {
						my ($rows, %args)	= @_;
						return $impl->( shift(@$rows), %args )
					};
				} elsif ($agg eq 'MIN' or $agg eq 'MAX') {
					my $expect	= ($agg eq 'MIN') ? 1 : -1;
					return sub {
						my ($rows, %args)	= @_;
						my $extrema;
						foreach my $r (@$rows) {
							my $t	= $impl->( $r, %args );
							return if (not($t) and $agg eq 'MIN');	# unbound is always minimal
							next if (not($t));						# unbound need not be considered for MAX
							$extrema	= $t if (not($extrema) or $extrema->compare($t) == $expect);
						}
						return $extrema;
					};
				} elsif ($agg eq 'SUM' or $agg eq 'AVG') {
					return sub {
						my ($rows, %args)	= @_;
						my $count	= 0;
						my $sum		= Attean::Literal->integer(0);
						my %seen;
						foreach my $r (@$rows) {
							my $term	= $impl->( $r, %args );
							if ($expr->distinct) {
								next if ($seen{ $term->as_string }++);
							}
							if ($term->does('Attean::API::NumericLiteral')) {
								$count++;
								$sum	= Attean::Literal->new( value => ($sum->numeric_value + $term->numeric_value), datatype => $sum->binary_promotion_type($term, '+') );
							} else {
								die "TypeError: AVG";
							}
						}
						if ($agg eq 'AVG') {
							$sum	= not($count) ? undef : Attean::Literal->new( value => ($sum->numeric_value / $count), datatype => $sum->binary_promotion_type(Attean::Literal->integer($count), '/') );
						}
						return $sum;
					};
				} elsif ($agg eq 'GROUP_CONCAT') {
					my $sep	= $expr->scalar_vars->{ 'seperator' } // ' ';
					return sub {
						my ($rows, %args)	= @_;
						my %seen;
						my @strings;
						foreach my $r (@$rows) {
							my $term	= eval { $impl->( $r, %args ) };
							if ($expr->distinct) {
								next if ($seen{ blessed($term) ? $term->as_string : '' }++);
							}
							push(@strings, $term->value // '');
						}
						return Attean::Literal->new(join($sep, sort @strings));
					};
				}
			} elsif ($agg eq 'CUSTOM') {
				my $iri	= $expr->custom_iri;
				my $data	= Attean->get_global_aggregate($iri);
				unless ($data) {
					die "No extension aggregate registered for <$iri>";
				}
				my $start	= $data->{'start'};
				my $process	= $data->{'process'};
				my $finalize	= $data->{'finalize'};

				my $impl	= $self->impl($child, $active_graph);
				return sub {
					my ($rows, %args)	= @_;
					my $thunk	= $start->($self->evaluator->model, $active_graph);
					foreach my $r (@$rows) {
						my $t	= $impl->( $r, %args );
						$process->($thunk, $t);
					}
					return $finalize->($thunk);
				};
			}
			die "Unimplemented AggregateExpression evaluation: " . $expr->operator;
		} elsif ($expr->isa('Attean::CastExpression')) {
			my ($child)	= @{ $expr->children };
			my $impl	= $self->impl( $child, $active_graph );
			my $type	= $expr->datatype;
			return sub {
				my ($r, %args)	= @_;
				my $term	= $impl->($r, %args);
				# TODO: reformat syntax for xsd:double
				my $cast	= Attean::Literal->new( value => $term->value, datatype => $type );
				return $cast->canonicalized_term if ($cast->does('Attean::API::CanonicalizingLiteral'));
				return $cast;
			}
		} else {
			Carp::confess "No impl for expression " . $expr->as_string;
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



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2022 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
