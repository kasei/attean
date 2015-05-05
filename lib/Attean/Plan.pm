use v5.14;
use warnings;

=head1 NAME

Attean::Plan - Representation of SPARQL query plan operators

=head1 VERSION

This document describes Attean::Plan version 0.003

=head1 SYNOPSIS

  use v5.14;
  use Attean;

=head1 DESCRIPTION

This is a utility package that defines all the Attean query plan classes
in the Attean::Plan namespace:

=over 4

=cut

use Attean::API::Query;

=item * L<Attean::Plan::Quad>

=cut

package Attean::Plan::Quad 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::Plan', 'Attean::API::NullaryQueryTree';
	has 'quad' => (is => 'ro', isa => ConsumerOf['Attean::API::QuadPattern'], required => 1);
	sub plan_as_string {
		my $self	= shift;
		return sprintf('Quad { %s }', $self->quad->as_string);
	}
}

=item * L<Attean::Plan::NestedLoopJoin>

=cut

package Attean::Plan::NestedLoopJoin 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef Str);
	with 'Attean::API::Plan', 'Attean::API::BinaryQueryTree';
	has 'join_variables' => (is => 'ro', isa => ArrayRef[Str], required => 1);
	sub plan_as_string { return 'NestedLoopJoin' }
}

=item * L<Attean::Plan::NestedLoopJoin>

=cut

package Attean::Plan::HashJoin 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef Str);
	with 'Attean::API::Plan', 'Attean::API::BinaryQueryTree';
	has 'join_variables' => (is => 'ro', isa => ArrayRef[Str], required => 1);
	sub plan_as_string {
		my $self	= shift;
		return sprintf('HashJoin { %s }', join(', ', @{$self->join_variables}));
	}
}

=item * L<Attean::Plan::NestedLoopLeftJoin>

=cut

package Attean::Plan::NestedLoopLeftJoin 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf ArrayRef Str);
	with 'Attean::API::Plan', 'Attean::API::BinaryQueryTree';
	has 'join_variables' => (is => 'ro', isa => ArrayRef[Str], required => 1);
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1, default => sub { Attean::ValueExpression->new( value => Attean::Literal->true ) });
	sub plan_as_string {
		my $self	= shift;
		return sprintf('NestedLoopLeftJoin { %s }', $self->expression->as_string);
	}
	sub tree_attributes { return qw(expression) };
}

=item * L<Attean::Plan::Filter>

=cut

package Attean::Plan::Filter 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
	sub plan_as_string {
		my $self	= shift;
		return sprintf('Filter { %s }', $self->expression->as_string);
	}
	sub tree_attributes { return qw(expression) };
}

=item * L<Attean::Plan::Union>

=cut

package Attean::Plan::Union 0.001 {
	use Moo;
	with 'Attean::API::Plan', 'Attean::API::BinaryQueryTree';
	sub plan_as_string { return 'Union' }
}

=item * L<Attean::Plan::Graph>

=cut

package Attean::Plan::Graph 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	sub plan_as_string {
		my $self	= shift;
		return sprintf('Graph %s', $self->graph->as_string);
	}
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'graph' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
	sub tree_attributes { return qw(graph) };
}

=item * L<Attean::Plan::Extend>

=cut

package Attean::Plan::Extend 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'variable' => (is => 'ro', isa => ConsumerOf['Attean::API::Variable'], required => 1);
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
	sub plan_as_string {
		my $self	= shift;
		return sprintf('Extend { %s ← %s }', $self->variable->as_string, $self->expression->as_string);
	}
	sub tree_attributes { return qw(variable expression) };
}

=item * L<Attean::Plan::Minus>

=cut

package Attean::Plan::Minus 0.001 {
	use Moo;
	with 'Attean::API::Plan', 'Attean::API::BinaryQueryTree';
	sub plan_as_string { return 'Minus' }
}

=item * L<Attean::Plan::Distinct>

=cut

package Attean::Plan::Distinct 0.001 {
	use Moo;
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	sub plan_as_string { return 'Distinct' }
}

=item * L<Attean::Plan::Slice>

=cut

package Attean::Plan::Slice 0.001 {
	use Moo;
	use Types::Standard qw(Int);
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'limit' => (is => 'ro', isa => Int, default => -1);
	has 'offset' => (is => 'ro', isa => Int, default => 0);
	sub plan_as_string {
		my $self	= shift;
		my @str	= ('Slice');
		push(@str, "Limit=" . $self->limit) if ($self->limit >= 0);
		push(@str, "Offset=" . $self->offset) if ($self->offset > 0);
		return join(' ', @str);
	}
}

=item * L<Attean::Plan::Project>

=cut

package Attean::Plan::Project 0.001 {
	use Moo;
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	use Types::Standard qw(ArrayRef ConsumerOf);
	has 'variables' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Variable']], required => 1);
	sub plan_as_string {
		my $self	= shift;
		return sprintf('Project { %s }', join(' ', map { '?' . $_->value } @{ $self->variables }));
	}
	sub tree_attributes { return qw(variables) };
}

=item * L<Attean::Plan::Comparator>

=cut

package Attean::Plan::Comparator 0.001 {
	use Moo;
	use Types::Standard qw(Bool ConsumerOf);
	has 'ascending' => (is => 'ro', isa => Bool, default => 1);
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
	sub tree_attributes { return qw(expression) };
	sub as_string {
		my $self	= shift;
		if ($self->ascending) {
			return $self->expression->as_string;
		} else {
			return 'DESC(' . $self->expression->as_string . ')';
		}
	}
	
	sub satisfies_ordering {
		my $self	= shift;
		my $cmp		= shift;
		return ($self->as_string eq $cmp->as_string);
	}
}

=item * L<Attean::Plan::OrderBy>

=cut

package Attean::Plan::OrderBy 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef InstanceOf);
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'comparators' => (is => 'ro', isa => ArrayRef[InstanceOf['Attean::Plan::Comparator']], required => 1);
	sub tree_attributes { return qw(comparators) };
	sub plan_as_string {
		my $self	= shift;
		return sprintf('Order { %s }', join(', ', map { $_->as_string } @{ $self->comparators }));
	}
}

=item * L<Attean::Plan::Service>

=cut

package Attean::Plan::Service 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf Bool);
	sub plan_as_string {
		my $self	= shift;
		return sprintf('Service %s', $self->endpoint->as_string);
	}
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'endpoint' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
	has 'silent' => (is => 'ro', isa => Bool, default => 0);
	
	sub tree_attributes { return qw(endpoint) };
}

# =item * L<Attean::Algebra::Path>
# 
# =cut
# 
# package Attean::Algebra::Path 0.001 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	sub in_scope_variables {
# 		my $self	= shift;
# 		my @vars	= map { $_->value } grep { $_->does('Attean::API::Variable') } ($self->subject, $self->object);
# 		return Set::Scalar->new(@vars)->members;
# 	}
# 	with 'Attean::API::Algebra', 'Attean::API::NullaryQueryTree';
# 	has 'subject' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
# 	has 'path' => (is => 'ro', isa => ConsumerOf['Attean::API::PropertyPath'], required => 1);
# 	has 'object' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
# 	sub tree_attributes { return qw(subject path object) };
# 	sub as_sparql {
# 		my $self	= shift;
# 		my %args	= @_;
# 		my $level	= $args{level} // 0;
# 		my $sp		= $args{indent} // '    ';
# 		my $indent	= $sp x $level;
# 		
# 		return "${indent}"
# 			. $self->subject->as_sparql
# 			. ' '
# 			. $self->path->as_sparql
# 			. ' '
# 			. $self->object->as_sparql
# 			. "\n";
# 	}
# }
# 
# =item * L<Attean::Algebra::Group>
# 
# =cut
# 
# package Attean::Algebra::Group 0.001 {
# 	use Moo;
# 	use Types::Standard qw(ArrayRef ConsumerOf);
# 	sub in_scope_variables {
# 		my $self	= shift;
# 		my $aggs	= $self->aggregates // [];
# 		my @vars;
# 		foreach my $a (@$aggs) {
# 			push(@vars, $a->variable->value);
# 		}
# 		return @vars;
# 	}
# 	sub algebra_as_string {
# 		my $self	= shift;
# 		my @aggs;
# 		my $aggs	= $self->aggregates // [];
# 		my $groups	= $self->groupby // [];
# 		foreach my $a (@$aggs) {
# 			my $v	= $a->variable->as_sparql;
# 			my $op	= $a->operator;
# 			my $d	= $a->distinct ? "DISTINCT " : '';
# 			my ($e)	= map { $_->value->as_sparql } @{ $a->children };
# 			push(@aggs, "$v ← ${op}($d$e)");
# 		}
# 		return sprintf('Group { %s } aggregate { %s }', join(', ', map { $_->as_sparql() } @$groups), join(', ', @aggs));
# 	}
# 	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
# 	has 'groupby' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Expression']]);
# 	has 'aggregates' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::AggregateExpression']]);
# 	sub tree_attributes { return qw(groupby aggregates) };
# 	sub as_sparql {
# 		my $self	= shift;
# 		my %args	= @_;
# 		my $level	= $args{level} // 0;
# 		my $sp		= $args{indent} // '    ';
# 		my $indent	= $sp x $level;
# 		my $groups	= $self->groupby // [];
# 		my $aggs	= $self->aggregates // [];
# 		my %vmap	= %{ $args{ aggregate_variables } // {} };
# 		my @aggs;
# 		foreach my $a (@$aggs) {
# 			my $av	= $a->variable->value;
# 			my $v	= exists $vmap{$av} ? $vmap{$av} : $av;
# 			my $op	= $a->operator;
# 			my $d	= $a->distinct ? "DISTINCT " : '';
# 			my ($e)	= map { $_->value->as_sparql } @{ $a->children };
# 			push(@aggs, "(${op}($d$e) AS $v)");
# 		}
# 		
# 		warn "TODO: as_sparql serialization of GROUPing";
# 		my $sparql	= "${indent}SELECT " . join(' ', @aggs) . " WHERE {\n"
# 			. join('', map { $_->as_sparql( %args, level => $level+1 ) } @{ $self->children })
# 			. "${indent}}";
# 		if (scalar(@$groups)) {
# 			my @g	= map { $_->as_sparql() } @$groups;
# 			$sparql	.= " GROUP BY " . join(' ', @g);
# 		}
# 		$sparql	.= "\n";
# 		return $sparql;
# 	}
# }
# 
# =item * L<Attean::Algebra::NegatedPropertySet>
# 
# =cut
# 
# package Attean::Algebra::NegatedPropertySet 0.001 {
# 	use Moo;
# 	use Types::Standard qw(ArrayRef ConsumerOf);
# 	with 'Attean::API::PropertyPath';
# 	has 'predicates' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::IRI']], required => 1);
# 	sub as_string {
# 		my $self	= shift;
# 		return sprintf("!(%s)", join('|', map { $_->ntriples_string } @{ $self->predicates }));
# 	}
# 	sub algebra_as_string { return 'NPS' }
# 	sub tree_attributes { return qw(predicates) };
# 	sub as_sparql {
# 		my $self	= shift;
# 		return "!(" . join('|', map { $_->as_sparql } @{$self->predicates}) . ")";
# 	}
# }
# 
# =item * L<Attean::Algebra::PredicatePath>
# 
# =cut
# 
# package Attean::Algebra::PredicatePath 0.001 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	with 'Attean::API::PropertyPath';
# 	has 'predicate' => (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], required => 1);
# 	sub as_string {
# 		my $self	= shift;
# 		return $self->predicate->ntriples_string;
# 	}
# 	sub algebra_as_string {
# 		my $self	= shift;
# 		return 'Property Path ' . $self->as_string;
# 	}
# 	sub tree_attributes { return qw(predicate) };
# 	sub as_sparql {
# 		my $self	= shift;
# 		return $self->predicate->as_sparql;
# 	}
# }
# 
# =item * L<Attean::Algebra::InversePath>
# 
# =cut
# 
# package Attean::Algebra::InversePath 0.001 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	with 'Attean::API::UnaryPropertyPath';
# 	sub prefix_name { return "^" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my ($path)	= @{ $self->children };
# 		return '^' . $self->path->as_sparql;
# 	}
# }
# 
# =item * L<Attean::Algebra::SequencePath>
# 
# =cut
# 
# package Attean::Algebra::SequencePath 0.001 {
# 	use Moo;
# 	with 'Attean::API::NaryPropertyPath';
# 	sub separator { return "/" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my @paths	= @{ $self->children };
# 		return '(' . join('/', map { $_->as_sparql } @paths) . ')';
# 	}
# }
# 
# =item * L<Attean::Algebra::AlternativePath>
# 
# =cut
# 
# package Attean::Algebra::AlternativePath 0.001 {
# 	use Moo;
# 	with 'Attean::API::NaryPropertyPath';
# 	sub separator { return "|" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my @paths	= @{ $self->children };
# 		return '(' . join('|', map { $_->as_sparql } @paths) . ')';
# 	}
# }
# 
# =item * L<Attean::Algebra::ZeroOrMorePath>
# 
# =cut
# 
# package Attean::Algebra::ZeroOrMorePath 0.001 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	with 'Attean::API::UnaryPropertyPath';
# 	sub postfix_name { return "*" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my ($path)	= @{ $self->children };
# 		return $self->path->as_sparql . '*';
# 	}
# }
# 
# =item * L<Attean::Algebra::OneOrMorePath>
# 
# =cut
# 
# package Attean::Algebra::OneOrMorePath 0.001 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	with 'Attean::API::UnaryPropertyPath';
# 	sub postfix_name { return "+" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my ($path)	= @{ $self->children };
# 		return $self->path->as_sparql . '+';
# 	}
# }
# 
# =item * L<Attean::Algebra::ZeroOrOnePath>
# 
# =cut
# 
# package Attean::Algebra::ZeroOrOnePath 0.001 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	with 'Attean::API::UnaryPropertyPath';
# 	sub postfix_name { return "?" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my ($path)	= @{ $self->children };
# 		return $self->path->as_sparql . '?';
# 	}
# }
# 
# =item * L<Attean::Algebra::Table>
# 
# =cut
# 
# package Attean::Algebra::Table 0.001 {
# 	use Moo;
# 	use Types::Standard qw(ArrayRef ConsumerOf);
# 	use namespace::clean;
# 	sub in_scope_variables {
# 		my $self	= shift;
# 		return map { $_->value } @{ $self->variables };
# 	}
# 	has variables => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Variable']]);
# 	has rows => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Result']]);
# 	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
# 	sub tree_attributes { return qw(variables rows) };
# 	sub algebra_as_string { return 'Table' }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my %args	= @_;
# 		my $level	= $args{level} // 0;
# 		my $sp		= $args{indent} // '    ';
# 		my $indent	= $sp x $level;
# 		
# 		my $sparql	= "${indent}VALUES (" . join(' ', map { $_->as_sparql } @{ $self->variables }) . ") {\n";
# 		foreach my $row (@{ $self->rows }) {
# 			$sparql	.= "${indent}${sp}(" . join(' ', map { $_->as_sparql } $row->values) . ")\n";
# 		}
# 		$sparql		.= "${indent}}\n";
# 		return $sparql;
# 	}
# }
# 
# =item * L<Attean::Algebra::Ask>
# 
# =cut
# 
# package Attean::Algebra::Ask 0.001 {
# 	use Moo;
# 	sub in_scope_variables { return; }
# 	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
# 	sub algebra_as_string { return 'Ask' }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my %args	= @_;
# 		my $level	= $args{level} // 0;
# 		my $sp		= $args{indent} // '    ';
# 		my $indent	= $sp x $level;
# 		
# 		return "${indent}ASK {\n"
# 			. join('', map { $_->as_sparql( %args, level => $level+1 ) } @{ $self->children })
# 			. "${indent}}\n";
# 	}
# }
# 
# =item * L<Attean::Algebra::Construct>
# 
# =cut
# 
# package Attean::Algebra::Construct 0.001 {
# 	use Moo;
# 	use Types::Standard qw(ArrayRef ConsumerOf);
# 	has 'triples' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::TriplePattern']]);
# 	sub in_scope_variables { return qw(subject predicate object); }
# 	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
# 	sub tree_attributes { return qw(triples) };
# 	sub algebra_as_string { return 'Construct' }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my %args	= @_;
# 		my $level	= $args{level} // 0;
# 		my $sp		= $args{indent} // '    ';
# 		my $indent	= $sp x $level;
# 		
# 		return "${indent}CONSTRUCT {\n"
# 			. join('', map { $_->as_sparql( %args, level => $level+1 ) } @{ $self->triples })
# 			. "${indent}} WHERE {\n"
# 			. join('', map { $_->as_sparql( %args, level => $level+1 ) } @{ $self->children })
# 			. "${indent}}\n";
# 	}
# }


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
