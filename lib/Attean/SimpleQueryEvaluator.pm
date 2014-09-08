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
  my $result = $e->evaluate( $algebra );

=head1 DESCRIPTION

The Attean::SimpleQueryEvaluator class implements a simple query evaluator that,
given an L<Attean::API::Algebra|Attean::API::Query> and a L<Attean::API::Model>
object, evaluates the query represented by the algebra using data from the
model, and returns a query result.

=cut

package Attean::SimpleQueryEvaluator 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	has 'model' => (is => 'ro', isa => ConsumerOf['Attean::API::Model'], required => 1);
	
	sub evaluate {
		my $self			= shift;
		my $algebra			= shift;
		my $active_graph	= shift;
		
		if ($algebra->isa('Attean::Algebra::BGP')) {
			my @triples	= @{ $algebra->triples };
			if (scalar(@triples) == 0) {
				return Attean::ListIterator->new(values => [], item_type => Type::Tiny::Role->new(role => 'Attean::API::Result'));
			} else {
				my @iters;
				foreach my $t (@triples) {
					my $q	= $t->as_quad_pattern($active_graph);
					my $iter	= $self->model->get_bindings( $q->values );
					push(@iters, $iter);
				}
				while (scalar(@iters) > 1) {
					my ($lhs, $rhs)	= splice(@iters, 0, 2);
					my $iter		= $lhs->join($rhs);
					unshift(@iters, $iter);
				}
				return shift(@iters);
			}
		} elsif ($algebra->isa('Attean::Algebra::Distinct') or $algebra->isa('Attean::Algebra::Reduced')) {
			my %seen;
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate( $child );
			return $iter->grep(sub {
				my $r	= shift;
				return not($seen{ $r->as_string }++);
			});
		} elsif ($algebra->isa('Attean::Algebra::Extend')) {
			my $expr	= $algebra->expression;
			my $var		= $algebra->variable->value;
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate( $child );
			return $iter->map(sub {
				my $r	= shift;
				my %b	= map { $_ => $r->value($_) } $r->variables;
				$b{$var}	= $expr->evaluate($r);
				my $b	= Attean::Result->new( bindings => \%b );
			});
		} elsif ($algebra->isa('Attean::Algebra::Filter')) {
			my $expr	= $algebra->expression;
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate( $child, $active_graph );
			return $iter->grep(sub {
				my $r	= shift;
				return $expr->evaluate($r)->ebv;
			});
		} elsif ($algebra->isa('Attean::Algebra::Graph')) {
			my $graph	= $algebra->graph;
			my ($child)	= @{ $algebra->children };
			if ($graph->does('Attean::API::Term')) {
				return $self->evaluate($child, $graph);
			} else {
				my @graphs	= $self->model->get_graphs();
				foreach my $g (@graphs) {
					# TODO: handle case of GRAPH ?g {}
					return $self->evaluate($child, $g);
				}
			}
		} elsif ($algebra->isa('Attean::Algebra::Group')) {
			# TODO: implement group evaluation
		} elsif ($algebra->isa('Attean::Algebra::Join')) {
			my ($lhs, $rhs)	= map { $self->evaluate($_, $active_graph) } @{ $algebra->children };
			return $lhs->join($rhs);
		} elsif ($algebra->isa('Attean::Algebra::LeftJoin')) {
			my $expr	= $algebra->expression;
			my ($lhs, $rhs)	= map { $self->evaluate($_, $active_graph) } @{ $algebra->children };
			my @rhs		= $rhs->elements;
			my @results;
			while (my $lhs = $self->next) {
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
			return Attean::ListIterator->new( values => \@results, item_type => $self->item_type);
		} elsif ($algebra->isa('Attean::Algebra::Minus')) {
			my $expr	= $algebra->expression;
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
		} elsif ($algebra->isa('Attean::Algebra::OrderBy')) {
			# TODO: implement orderby evaluation
		} elsif ($algebra->isa('Attean::Algebra::Path')) {
			my $s		= $algebra->subject;
			my $path	= $algebra->path;
			my $o		= $algebra->object;
			if ($path->isa('Attean::Algebra::PredicatePath')) {
				return $self->model->get_bindings( $s, $path->predicate, $o, $active_graph );
			} elsif ($path->isa('Attean::Algebra::InversePath')) {
				my ($child)	= @{ $algebra->children };
				my $path	= Attean::Algebra::Path->new( subject => $o, path => $child, object => $s );
				return $self->evaluate( $path, $active_graph );
			} elsif ($path->isa('Attean::Algebra::AlternativePath')) {
				my @paths		= @{ $algebra->children };
				my @algebras	= map { Attean::Algebra::Path->new( subject => $s, path => $_, object => $o ) } @paths;
				my @iters		= map { $self->evaluate($_, $active_graph) } @algebras;
				return Attean::IteratorSequence->new( iterators => \@iters, item_type => $iters[0]->item_type );
			} elsif ($path->isa('Attean::Algebra::NegatedPropertySet')) {
				# TODO: implement NegatedPropertySet evaluation
			} elsif ($path->isa('Attean::Algebra::SequencePath')) {
				# TODO: implement SequencePath evaluation
			} elsif ($path->isa('Attean::Algebra::ZeroOrMorePath')) {
				# TODO: implement ZeroOrMorePath evaluation
			} elsif ($path->isa('Attean::Algebra::OneOrMorePath')) {
				# TODO: implement OneOrMorePath evaluation
			} elsif ($path->isa('Attean::Algebra::ZeroOrOnePath')) {
				# TODO: implement ZeroOrOnePath evaluation
			} else {
				# TODO: implement path evaluation
				die "Unexpected path type: $path";
			}
			die "Unimplemented path type: $path";
		} elsif ($algebra->isa('Attean::Algebra::Project')) {
			my ($child)	= @{ $algebra->children };
			my $iter	= $self->evaluate( $child, $active_graph );
			my @vars	= map { $_->value } @{ $algebra->variables };
			return $iter->map(sub {
				my $r	= shift;
				my $b	= { map { $_ => $r->value($_) } @vars };
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
			return $iter;
		} elsif ($algebra->isa('Attean::Algebra::Union')) {
			my @iters	= map { $self->evaluate($_, $active_graph) } @{ $algebra->children };
			return Attean::IteratorSequence->new( iterators => \@iters );
		} else {
			die "Unexpected algebra type: $algebra";
		}

		die "Unimplemented algebra evaluation for: $algebra";
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
