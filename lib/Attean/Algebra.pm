use v5.14;
use warnings;

=head1 NAME

Attean::Algebra - Representation of SPARQL algebra operators

=head1 VERSION

This document describes Attean::Algebra version 0.000

=head1 SYNOPSIS

  use v5.14;
  use Attean;

=head1 DESCRIPTION

This is a utility package that defines all the Attean query algebra classes
in the Attean::Algebra namespace:

=over 4

=cut

use Attean::API::Query;

=item * L<Attean::Algebra::Join>

=cut

package Attean::Algebra::Join 0.001 {
	use Moo;
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
	sub algebra_as_string { return 'Join' }
}

=item * L<Attean::Algebra::LeftJoin>

=cut

package Attean::Algebra::LeftJoin 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1, default => sub { Attean::ValueExpression->new( value => Attean::Literal->true ) });
	sub algebra_as_string {
		my $self	= shift;
		return sprintf('LeftJoin { %s }', $self->expression->as_string);
	}
}

=item * L<Attean::Algebra::Filter>

=cut

package Attean::Algebra::Filter 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
	sub algebra_as_string {
		my $self	= shift;
		return sprintf('Filter { %s }', $self->expression->as_string);
	}
}

=item * L<Attean::Algebra::Union>

=cut

package Attean::Algebra::Union 0.001 {
	use Moo;
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
	sub algebra_as_string { return 'Union' }
}

=item * L<Attean::Algebra::Graph>

=cut

package Attean::Algebra::Graph 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	sub in_scope_variables {
		my $self	= shift;
		my $graph	= $self->graph;
		my ($child)	= @{ $self->children };
		my @vars	= $child->in_scope_variables;
		if ($graph->does('Attean::API::Variable')) {
			return Set::Scalar->new(@vars, $graph->value)->members;
		} else {
			return @vars;
		}
	}
	sub algebra_as_string {
		my $self	= shift;
		return sprintf('Graph %s', $self->graph->as_string);
	}
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'graph' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
}

=item * L<Attean::Algebra::Extend>

=cut

package Attean::Algebra::Extend 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	sub in_scope_variables {
		my $self	= shift;
		my ($child)	= @{ $self->children };
		my @vars	= $child->in_scope_variables;
		return Set::Scalar->new(@vars, $self->variable->value)->members;
	}
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'variable' => (is => 'ro', isa => ConsumerOf['Attean::API::Variable'], required => 1);
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
	sub algebra_as_string {
		my $self	= shift;
		return sprintf('Extend { %s=%s }', $self->variable->as_string, $self->expression->as_string);
	}
	
}

=item * L<Attean::Algebra::Minus>

=cut

package Attean::Algebra::Minus 0.001 {
	use Moo;
	sub in_scope_variables {
		my $self	= shift;
		my ($child)	= @{ $self->children };
		return $child->in_scope_variables;
	}
	with 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
	sub algebra_as_string { return 'Minus' }
}

=item * L<Attean::Algebra::Distinct>

=cut

package Attean::Algebra::Distinct 0.001 {
	use Moo;
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	sub algebra_as_string { return 'Distinct' }
}

=item * L<Attean::Algebra::Reduced>

=cut

package Attean::Algebra::Reduced 0.001 {
	use Moo;
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	sub algebra_as_string { return 'Reduced' }
}

=item * L<Attean::Algebra::Slice>

=cut

package Attean::Algebra::Slice 0.001 {
	use Moo;
	use Types::Standard qw(Int);
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'limit' => (is => 'ro', isa => Int, default => -1);
	has 'offset' => (is => 'ro', isa => Int, default => 0);
	sub algebra_as_string {
		my $self	= shift;
		my @str	= ('Filter');
		push(@str, "Limit=" . $self->limit) if ($self->limit >= 0);
		push(@str, "Offset=" . $self->offset) if ($self->limit > 0);
		return join(' ', @str);
	}
}

=item * L<Attean::Algebra::Project>

=cut

package Attean::Algebra::Project 0.001 {
	use Moo;
	sub in_scope_variables {
		my $self	= shift;
		my ($child)	= @{ $self->children };
		my $set		= $child->in_scope_variables;
		my $proj	= Set::Scalar->new( map { $_->value } @{ $self->variables } );
		return $set->intersection($proj);
	}
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	use Types::Standard qw(ArrayRef ConsumerOf);
	has 'variables' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Variable']], required => 1);
	sub algebra_as_string {
		my $self	= shift;
		return sprintf('Project { %s }', join(' ', map { '?' . $_->value } @{ $self->variables }));
	}
}

=item * L<Attean::Algebra::Comparator>

=cut

package Attean::Algebra::Comparator 0.001 {
	use Moo;
	use Types::Standard qw(Bool ConsumerOf);
	has 'ascending' => (is => 'ro', isa => Bool, default => 1);
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
}

=item * L<Attean::Algebra::OrderBy>

=cut

package Attean::Algebra::OrderBy 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef InstanceOf);
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'comparators' => (is => 'ro', isa => ArrayRef[InstanceOf['Attean::Algebra::Comparator']], required => 1);
}

=item * L<Attean::Algebra::BGP>

=cut

package Attean::Algebra::BGP 0.001 {
	use Moo;
	use Attean::RDF;
	use Types::Standard qw(ArrayRef ConsumerOf);
	use namespace::clean;

	sub in_scope_variables {
		my $self	= shift;
		my $set		= Set::Scalar->new();
		foreach my $t (@{ $self->triples }) {
			my @vars	= map { $_->value } grep { $_->does('Attean::API::Variable') } $t->values;
			$set->insert(@vars);
		}
		return $set->members;
	}
	
	sub algebra_as_string {
		my $self	= shift;
		return 'BGP { ' . join(', ', map { $_->as_string } @{ $self->triples }) . ' }';
	}
	
	has 'triples' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::TriplePattern']], default => sub { [] });
	
	sub elements {
		my $self	= shift;
		return @{ $self->triples };
	}
	
	with 'Attean::API::Algebra', 'Attean::API::NullaryQueryTree', 'Attean::API::CanonicalizingBindingSet';

	sub canonicalize {
		my $self	= shift;
		my ($algebra, $mapping)	= $self->canonical_bgp_with_mapping();
		my @proj	= sort map { sprintf("(?v%03d AS $_)", $mapping->{$_}{id}) } grep { $mapping->{$_}{type} eq 'variable' } (keys %$mapping);
		foreach my $var (keys %$mapping) {
			$algebra	= Attean::Algebra::Extend->new(
				children	=> [$algebra],
				variable	=> variable($var),
				expression	=> Attean::ValueExpression->new( value => variable($mapping->{$var}{id}) ),
			);
		}
	}
	
	sub canonical_bgp_with_mapping {
		my $self	= shift;
		my ($triples, $mapping)	= $self->canonical_set_with_mapping();
		my $algebra	= Attean::Algebra::BGP->new( triples => $triples );
		return ($algebra, $mapping);
	}
}

=item * L<Attean::Algebra::Path>

=cut

package Attean::Algebra::Path 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	sub in_scope_variables {
		my $self	= shift;
		my @vars	= map { $_->value } grep { $_->does('Attean::API::Variable') } ($self->subject, $self->object);
		return Set::Scalar->new(@vars)->members;
	}
	with 'Attean::API::Algebra', 'Attean::API::NullaryQueryTree';
	has 'subject' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
	has 'path' => (is => 'ro', isa => ConsumerOf['Attean::API::PropertyPath'], required => 1);
	has 'object' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
}

=item * L<Attean::Algebra::Group>

=cut

package Attean::Algebra::Group 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	sub in_scope_variables {
		my $self	= shift;
		# TODO: implement Attean::Algebra::Group->in_scope_variables
		die "Attean::Algebra::Group->in_scope_variables unimplemented";
	}
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'groupby' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Expression']]);
	has 'aggregates' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::AggregateExpression']]);
}

=item * L<Attean::Algebra::NegatedPropertySet>

=cut

package Attean::Algebra::NegatedPropertySet 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	with 'Attean::API::PropertyPath';
	has 'predicates' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::IRI']], required => 1);
	sub as_string {
		my $self	= shift;
		return sprintf("!(%s)", join('|', map { $_->ntriples_string } @{ $self->predicates }));
	}
}

=item * L<Attean::Algebra::PredicatePath>

=cut

package Attean::Algebra::PredicatePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::PropertyPath';
	has 'predicate' => (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], required => 1);
	sub as_string {
		my $self	= shift;
		return $self->predicate->ntriples_string;
	}
}

=item * L<Attean::Algebra::InversePath>

=cut

package Attean::Algebra::InversePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub prefix_name { return "^" }
}

=item * L<Attean::Algebra::SequencePath>

=cut

package Attean::Algebra::SequencePath 0.001 {
	use Moo;
	with 'Attean::API::NaryPropertyPath';
	sub separator { return "/" }
}

=item * L<Attean::Algebra::AlternativePath>

=cut

package Attean::Algebra::AlternativePath 0.001 {
	use Moo;
	with 'Attean::API::NaryPropertyPath';
	sub separator { return "|" }
}

=item * L<Attean::Algebra::ZeroOrMorePath>

=cut

package Attean::Algebra::ZeroOrMorePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "*" }
}

=item * L<Attean::Algebra::OneOrMorePath>

=cut

package Attean::Algebra::OneOrMorePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "+" }
}

=item * L<Attean::Algebra::ZeroOrOnePath>

=cut

package Attean::Algebra::ZeroOrOnePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "?" }
}

=item * L<Attean::Algebra::Table>

=cut

package Attean::Algebra::Table 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	use namespace::clean;
	sub in_scope_variables {
		my $self	= shift;
		return map { $_->value } @{ $self->variables };
	}
	has variables => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Variable']]);
	has rows => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Result']]);
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
}

=item * L<Attean::Algebra::Ask>

=cut

package Attean::Algebra::Ask 0.001 {
	use Moo;
	sub in_scope_variables { return; }
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
}

=item * L<Attean::Algebra::Construct>

=cut

package Attean::Algebra::Construct 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	has 'triples' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::TriplePattern']]);
	sub in_scope_variables { return qw(subject predicate object); }
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
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
