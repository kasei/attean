use v5.14;
use warnings;

=head1 NAME

Attean::Algebra - Representation of SPARQL algebra operators

=head1 VERSION

This document describes Attean::Algebra version 0.005

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

package Attean::Algebra::Join 0.005 {
	use Moo;
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
	sub algebra_as_string { return 'Join' }
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		
		return join('', map { $_->as_sparql( %args ) } @{ $self->children });
	}
}

=item * L<Attean::Algebra::LeftJoin>

=cut

package Attean::Algebra::LeftJoin 0.005 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1, default => sub { Attean::ValueExpression->new( value => Attean::Literal->true ) });
	sub algebra_as_string {
		my $self	= shift;
		return sprintf('LeftJoin { %s }', $self->expression->as_string);
	}
	sub tree_attributes { return qw(expression) };
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		my ($lhs, $rhs)	= @{ $self->children };
		
		return "${indent}{\n"
			. $lhs->as_sparql( %args, level => $level+1 )
			. "${indent}} OPTIONAL {\n"
			. $rhs->as_sparql( %args, level => $level+1 )
			. "${indent}}\n";
	}
}

=item * L<Attean::Algebra::Filter>

=cut

package Attean::Algebra::Filter 0.005 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
	sub algebra_as_string {
		my $self	= shift;
		return sprintf('Filter { %s }', $self->expression->as_string);
	}
	sub tree_attributes { return qw(expression) };
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		my ($lhs, $rhs)	= @{ $self->children };
		
		my ($child)	= @{ $self->children };
		return $child->as_sparql( %args )
			. "${indent}FILTER(" . $self->expression->as_sparql . ")\n";
	}
}

=item * L<Attean::Algebra::Union>

=cut

package Attean::Algebra::Union 0.005 {
	use Moo;
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
	sub algebra_as_string { return 'Union' }
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		my ($lhs, $rhs)	= @{ $self->children };
		
		return "${indent}{\n"
			. $lhs->as_sparql( %args, level => $level+1 )
			. "${indent}} UNION {\n"
			. $rhs->as_sparql( %args, level => $level+1 )
			. "${indent}}\n";
	}
}

=item * L<Attean::Algebra::Graph>

=cut

package Attean::Algebra::Graph 0.005 {
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
	sub tree_attributes { return qw(graph) };
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		
		my ($child)	= @{ $self->children };
		my $g		= $self->graph->as_sparql;
		return "${indent}GRAPH $g {\n"
			. $child->as_sparql( %args, level => $level+1 )
			. "${indent}}\n";
	}
}

=item * L<Attean::Algebra::Extend>

=cut

package Attean::Algebra::Extend 0.005 {
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
		return sprintf('Extend { %s ← %s }', $self->variable->as_string, $self->expression->as_string);
	}
	sub tree_attributes { return qw(variable expression) };
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my %vmap	= %{ $args{ aggregate_variables } // {} };
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		my ($lhs, $rhs)	= @{ $self->children };
		
		my $expr	= $self->expression;
		my $var		= $self->variable;
		if ($expr->isa('Attean::ValueExpression')) {
			$vmap{ $expr->value->value }	= $var->as_sparql;
		}
		my ($child)	= @{ $self->children };

		my %in_scope	= map { $_ => 1 } $child->in_scope_variables;
		my $sparql;
		if ($child->isa('Attean::Algebra::Group')) {
			$sparql	= "${indent}{\n";
			$sparql	.= $child->as_sparql( %args, level => $level+1, aggregate_variables => \%vmap );
			$sparql	.= "${indent}}\n";
		} else {
			$sparql	= $child->as_sparql( %args, aggregate_variables => \%vmap );
		}
		my $evar	= $expr->isa('Attean::ValueExpression') ? $expr->value->value : $expr->as_sparql;
		unless (exists $in_scope{$evar}) {
			$sparql	.= "${indent}BIND(" . $expr->as_sparql . " AS " . $var->as_sparql . ")\n";
		}
		return $sparql;
	}
}

=item * L<Attean::Algebra::Minus>

=cut

package Attean::Algebra::Minus 0.005 {
	use Moo;
	sub in_scope_variables {
		my $self	= shift;
		my ($child)	= @{ $self->children };
		return $child->in_scope_variables;
	}
	with 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
	sub algebra_as_string { return 'Minus' }
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		my ($lhs, $rhs)	= @{ $self->children };
		
		return "${indent}{\n"
			. $lhs->as_sparql( %args, level => $level+1 )
			. "${indent}} MINUS {\n"
			. $rhs->as_sparql( %args, level => $level+1 )
			. "${indent}}\n";
	}
}

=item * L<Attean::Algebra::Distinct>

=cut

package Attean::Algebra::Distinct 0.005 {
	use Moo;
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	sub algebra_as_string { return 'Distinct' }
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		my ($child)	= @{ $self->children };
		
		if ($child->isa('Attean::Algebra::Project')) {
			return $child->as_sparql( %args, distinct => 1 );
		} else {
			return "${indent}SELECT DISTINCT * WHERE {\n"
				. $child->as_sparql( %args, level => $level+1 )
				. "${indent}}\n";
		}
	}
}

=item * L<Attean::Algebra::Reduced>

=cut

package Attean::Algebra::Reduced 0.005 {
	use Moo;
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	sub algebra_as_string { return 'Reduced' }
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		my ($child)	= @{ $self->children };
		
		if ($child->isa('Attean::Algebra::Project')) {
			return $child->as_sparql( %args, level => $level+1, reduced => 1 );
		} else {
			return "${indent}SELECT REDUCED * WHERE {\n"
				. $child->as_sparql( %args )
				. "${indent}}\n";
		}
	}
}

=item * L<Attean::Algebra::Slice>

=cut

package Attean::Algebra::Slice 0.005 {
	use Moo;
	use Types::Standard qw(Int);
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'limit' => (is => 'ro', isa => Int, default => -1);
	has 'offset' => (is => 'ro', isa => Int, default => 0);
	sub algebra_as_string {
		my $self	= shift;
		my @str	= ('Slice');
		push(@str, "Limit=" . $self->limit) if ($self->limit >= 0);
		push(@str, "Offset=" . $self->offset) if ($self->offset > 0);
		return join(' ', @str);
	}
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		my ($child)	= @{ $self->children };
		
		my $sparql;
		if ($child->isa('Attean::Algebra::Project')
				or $child->isa('Attean::Algebra::Distinct')
				or $child->isa('Attean::Algebra::Reduced')) {
			$sparql	= $child->as_sparql( %args );
		} else {
			$sparql	= "${indent}SELECT * WHERE {\n"
				. $child->as_sparql( %args, level => $level+1 )
				. "${indent}}\n";
		}
		$sparql	.= "${indent}LIMIT " . $self->limit . "\n" if ($self->limit >= 0);
		$sparql	.= "${indent}OFFSET " . $self->offset . "\n" if ($self->offset > 0);
		return $sparql;
	}
}

=item * L<Attean::Algebra::Project>

=cut

package Attean::Algebra::Project 0.005 {
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
	sub tree_attributes { return qw(variables) };
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		my ($child)	= @{ $self->children };
		my $order;
		if ($child->isa('Attean::Algebra::OrderBy')) {
			$order	= $child;
			($child)	= @{ $child->children };
		}
		
		my $modifier	= '';
		$modifier		= 'DISTINCT ' if ($args{distinct});
		$modifier		= 'REDUCED ' if ($args{reduced});
		my $pvars		= join(' ', map { $_->as_sparql } @{ $self->variables });
		$pvars			= '*' if ($pvars eq '');
		
		my @pvars	= sort map { $_->does('Attean::API::Variable') ? $_->value : $_->as_sparql } @{ $self->variables };
		my @vars	= sort $child->in_scope_variables;
		my $sparql;
		if (join(' ', @pvars) eq join(' ', @vars)) {
			$sparql	= $child->as_sparql( %args );
		} else {
			$sparql	= "${indent}SELECT $modifier$pvars WHERE {\n"
				. $child->as_sparql( %args, level => $level+1 )
				. "${indent}}\n";
		}
		
		if ($order) {
			$sparql	.= "${indent}ORDER BY " . join(' ', map { $_->as_sparql } @{ $order->comparators }) . "\n";
		}
		
		return $sparql;
	}
}

=item * L<Attean::Algebra::Comparator>

=cut

package Attean::Algebra::Comparator 0.005 {
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
	sub as_sparql {
		my $self	= shift;
		if ($self->ascending) {
			return $self->expression->as_sparql;
		} else {
			return 'DESC(' . $self->expression->as_sparql . ')';
		}
	}
}

=item * L<Attean::Algebra::OrderBy>

=cut

package Attean::Algebra::OrderBy 0.005 {
	use Moo;
	use Types::Standard qw(ArrayRef InstanceOf);
	with 'Attean::API::UnionScopeVariables', 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'comparators' => (is => 'ro', isa => ArrayRef[InstanceOf['Attean::Algebra::Comparator']], required => 1);
	sub tree_attributes { return qw(comparators) };
	sub algebra_as_string {
		my $self	= shift;
		return sprintf('Order { %s }', join(', ', map { $_->as_string } @{ $self->comparators }));
	}
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		my ($child)	= @{ $self->children };
		
		return "${indent}SELECT * WHERE {\n"
			. $child->as_sparql( %args, level => $level+1 )
			. "${indent}}\n"
			. "${indent}ORDER BY " . join(' ', map { $_->as_sparql } @{ $self->comparators }) . "\n";
	}
}

=item * L<Attean::Algebra::BGP>

=cut

package Attean::Algebra::BGP 0.005 {
	use Moo;
	use Attean::RDF;
	use Set::Scalar;
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
	
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		
		return "${indent}{\n"
			. join('', map { $indent . $sp . $_->as_sparql( %args, level => $level+1 ) } @{ $self->triples })
			. "${indent}}\n";
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
	sub tree_attributes { return qw(triples) };
}

=item * L<Attean::Algebra::Service>

=cut

package Attean::Algebra::Service 0.005 {
	use Moo;
	use Types::Standard qw(ConsumerOf Bool);
	sub algebra_as_string {
		my $self	= shift;
		return sprintf('Service %s', $self->endpoint->as_string);
	}
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree', 'Attean::API::UnionScopeVariables';
	has 'endpoint' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
	has 'silent' => (is => 'ro', isa => Bool, default => 0);
	
	sub tree_attributes { return qw(endpoint) };
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		
		my ($child)	= @{ $self->children };
		my $ep		= $self->endpoint->as_sparql;
		return "${indent}SERVICE $ep {\n"
			. $child->as_sparql( %args, level => $level+1 )
			. "${indent}}\n";
	}
}

=item * L<Attean::Algebra::Path>

=cut

package Attean::Algebra::Path 0.005 {
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
	sub tree_attributes { return qw(subject path object) };
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		
		return "${indent}"
			. $self->subject->as_sparql
			. ' '
			. $self->path->as_sparql
			. ' '
			. $self->object->as_sparql
			. "\n";
	}
}

=item * L<Attean::Algebra::Group>

=cut

package Attean::Algebra::Group 0.005 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	sub in_scope_variables {
		my $self	= shift;
		my $aggs	= $self->aggregates // [];
		my @vars;
		foreach my $a (@$aggs) {
			push(@vars, $a->variable->value);
		}
		return @vars;
	}
	sub algebra_as_string {
		my $self	= shift;
		my @aggs;
		my $aggs	= $self->aggregates // [];
		my $groups	= $self->groupby // [];
		foreach my $a (@$aggs) {
			my $v	= $a->variable->as_sparql;
			my $op	= $a->operator;
			my $d	= $a->distinct ? "DISTINCT " : '';
			my ($e)	= map { $_->value->as_sparql } @{ $a->children };
			push(@aggs, "$v ← ${op}($d$e)");
		}
		return sprintf('Group { %s } aggregate { %s }', join(', ', map { $_->as_sparql() } @$groups), join(', ', @aggs));
	}
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'groupby' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Expression']]);
	has 'aggregates' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::AggregateExpression']]);
	sub tree_attributes { return qw(groupby aggregates) };
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		my $groups	= $self->groupby // [];
		my $aggs	= $self->aggregates // [];
		my %vmap	= %{ $args{ aggregate_variables } // {} };
		my @aggs;
		foreach my $a (@$aggs) {
			my $av	= $a->variable->value;
			my $v	= exists $vmap{$av} ? $vmap{$av} : $av;
			my $op	= $a->operator;
			my $d	= $a->distinct ? "DISTINCT " : '';
			my ($e)	= map { $_->value->as_sparql } @{ $a->children };
			push(@aggs, "(${op}($d$e) AS $v)");
		}
		
		warn "TODO: as_sparql serialization of GROUPing";
		my $sparql	= "${indent}SELECT " . join(' ', @aggs) . " WHERE {\n"
			. join('', map { $_->as_sparql( %args, level => $level+1 ) } @{ $self->children })
			. "${indent}}";
		if (scalar(@$groups)) {
			my @g	= map { $_->as_sparql() } @$groups;
			$sparql	.= " GROUP BY " . join(' ', @g);
		}
		$sparql	.= "\n";
		return $sparql;
	}
}

=item * L<Attean::Algebra::NegatedPropertySet>

=cut

package Attean::Algebra::NegatedPropertySet 0.005 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	with 'Attean::API::PropertyPath';
	has 'predicates' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::IRI']], required => 1);
	sub as_string {
		my $self	= shift;
		return sprintf("!(%s)", join('|', map { $_->ntriples_string } @{ $self->predicates }));
	}
	sub algebra_as_string { return 'NPS' }
	sub tree_attributes { return qw(predicates) };
	sub as_sparql {
		my $self	= shift;
		return "!(" . join('|', map { $_->as_sparql } @{$self->predicates}) . ")";
	}
}

=item * L<Attean::Algebra::PredicatePath>

=cut

package Attean::Algebra::PredicatePath 0.005 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::PropertyPath';
	has 'predicate' => (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], required => 1);
	sub as_string {
		my $self	= shift;
		return $self->predicate->ntriples_string;
	}
	sub algebra_as_string {
		my $self	= shift;
		return 'Property Path ' . $self->as_string;
	}
	sub tree_attributes { return qw(predicate) };
	sub as_sparql {
		my $self	= shift;
		return $self->predicate->as_sparql;
	}
}

=item * L<Attean::Algebra::InversePath>

=cut

package Attean::Algebra::InversePath 0.005 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub prefix_name { return "^" }
	sub as_sparql {
		my $self	= shift;
		my ($path)	= @{ $self->children };
		return '^' . $self->path->as_sparql;
	}
}

=item * L<Attean::Algebra::SequencePath>

=cut

package Attean::Algebra::SequencePath 0.005 {
	use Moo;
	with 'Attean::API::NaryPropertyPath';
	sub separator { return "/" }
	sub as_sparql {
		my $self	= shift;
		my @paths	= @{ $self->children };
		return '(' . join('/', map { $_->as_sparql } @paths) . ')';
	}
}

=item * L<Attean::Algebra::AlternativePath>

=cut

package Attean::Algebra::AlternativePath 0.005 {
	use Moo;
	with 'Attean::API::NaryPropertyPath';
	sub separator { return "|" }
	sub as_sparql {
		my $self	= shift;
		my @paths	= @{ $self->children };
		return '(' . join('|', map { $_->as_sparql } @paths) . ')';
	}
}

=item * L<Attean::Algebra::ZeroOrMorePath>

=cut

package Attean::Algebra::ZeroOrMorePath 0.005 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "*" }
	sub as_sparql {
		my $self	= shift;
		my ($path)	= @{ $self->children };
		return $self->path->as_sparql . '*';
	}
}

=item * L<Attean::Algebra::OneOrMorePath>

=cut

package Attean::Algebra::OneOrMorePath 0.005 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "+" }
	sub as_sparql {
		my $self	= shift;
		my ($path)	= @{ $self->children };
		return $self->path->as_sparql . '+';
	}
}

=item * L<Attean::Algebra::ZeroOrOnePath>

=cut

package Attean::Algebra::ZeroOrOnePath 0.005 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "?" }
	sub as_sparql {
		my $self	= shift;
		my ($path)	= @{ $self->children };
		return $self->path->as_sparql . '?';
	}
}

=item * L<Attean::Algebra::Table>

=cut

package Attean::Algebra::Table 0.005 {
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
	sub tree_attributes { return qw(variables rows) };
	sub algebra_as_string { return 'Table' }
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		
		my $sparql	= "${indent}VALUES (" . join(' ', map { $_->as_sparql } @{ $self->variables }) . ") {\n";
		foreach my $row (@{ $self->rows }) {
			$sparql	.= "${indent}${sp}(" . join(' ', map { $_->as_sparql } $row->values) . ")\n";
		}
		$sparql		.= "${indent}}\n";
		return $sparql;
	}
}

=item * L<Attean::Algebra::Ask>

=cut

package Attean::Algebra::Ask 0.005 {
	use Moo;
	sub in_scope_variables { return; }
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	sub algebra_as_string { return 'Ask' }
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		
		return "${indent}ASK {\n"
			. join('', map { $_->as_sparql( %args, level => $level+1 ) } @{ $self->children })
			. "${indent}}\n";
	}
}

=item * L<Attean::Algebra::Construct>

=cut

package Attean::Algebra::Construct 0.005 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	has 'triples' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::TriplePattern']]);
	sub in_scope_variables { return qw(subject predicate object); }
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	sub tree_attributes { return qw(triples) };
	sub algebra_as_string { return 'Construct' }
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;
		
		return "${indent}CONSTRUCT {\n"
			. join('', map { $_->as_sparql( %args, level => $level+1 ) } @{ $self->triples })
			. "${indent}} WHERE {\n"
			. join('', map { $_->as_sparql( %args, level => $level+1 ) } @{ $self->children })
			. "${indent}}\n";
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
