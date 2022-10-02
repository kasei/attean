use v5.14;
use warnings;

=head1 NAME

Attean::API::Expression - SPARQL expressions

=head1 VERSION

This document describes Attean::API::Expression version 0.033

=head1 DESCRIPTION

The Attean::API::Expression role defines a common API for SPARQL expressions
consisting of logical, numeric, and function operators, constant terms, and
variables. Expressions may be evaluated in the context of a
L<Attean::API::Result> object, and either return a L<Attean::API::Term> object
or throw a type error exception.

=head1 ROLES

This role consumes the L<Attean::API::DirectedAcyclicGraph> role which provide the following methods:

=over 4

=item C<< is_leaf >>

=item C<< walk( prefix => \&pre_cb, postfix => \&pre_cb ) >>

=item C<< cover( prefix => \&pre_cb, postfix => \&pre_cb ) >>

=back

and the following attributes:

=over 4

=item C<< children >>

=back

=head1 ATTRIBUTES

The following attributes exist:

=over 4

=item C<< operator >>

A string indicating the expression operator (e.g. C<'+'> or C<'||'>).

=back

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Expression> role:

=over 4

=item C<< as_string >>

Returns a string serialization of the expression object.

=back

=cut

package Attean::API::Expression 0.033 {
	use Types::Standard qw(Str);

	use Moo::Role;

	with 'Attean::API::DirectedAcyclicGraph', 'Attean::API::UnionScopeVariables';
	
	has 'operator' => (is => 'ro', isa => Str, required => 1);
	requires 'is_stable'; # is stable for sorting (won't change across evaluations)
	requires 'unaggregated_variables';
	requires 'as_string';
	requires 'as_sparql';
	
	sub BUILD {}
	if ($ENV{ATTEAN_TYPECHECK}) {
		around 'BUILD' => sub {
			my $orig	= shift;
			my $self	= shift;
			$self->$orig(@_);
			my $name	= ref($self);
			$name		=~ s/^.*://;
			if ($self->can('arity')) {
				my $arity		= $self->arity;
				if (defined($arity)) {
					my $children	= $self->children;
					my $size		= scalar(@$children);
					unless ($size == $arity) {
						die "${name} expression construction with bad number of children (expected $arity, but got $size)";
					}
				}
			}
		}
	}
}

package Attean::API::UnaryExpression 0.033 {
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;

	use Moo::Role;
	
	with 'Attean::API::Expression', 'Attean::API::UnaryQueryTree';
	with 'Attean::API::SPARQLSerializable';
	
	sub as_string {
		my $self	= shift;
		my ($data)	= @{ $self->children };
		return sprintf("%s(%s)", $self->operator, $data->as_string);
	}

	my %ops	= (
		'!'	=> AtteanX::SPARQL::Token->fast_constructor( BANG, -1, -1, -1, -1, ['!'] ),
		'-'	=> AtteanX::SPARQL::Token->fast_constructor( MINUS, -1, -1, -1, -1, ['-'] ),
		'+'	=> AtteanX::SPARQL::Token->fast_constructor( PLUS, -1, -1, -1, -1, ['+'] ),
	);

	sub unaggregated_variables {
		my $self	= shift;
		my ($child)	= @{ $self->children };
		return $child->unaggregated_variables;
	}
	
	sub sparql_tokens {
		my $self	= shift;
		my $op		= $ops{$self->operator} // die "No operator found in Attean::API::UnaryExpression->sparql_tokens";

		my @tokens;
		push(@tokens, $op);
		foreach my $t (@{ $self->children }) {
			push(@tokens, $t->sparql_tokens->elements);
		}
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
}

package Attean::API::BinaryExpression 0.033 {
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;

	use Moo::Role;
	
	with 'Attean::API::Expression', 'Attean::API::BinaryQueryTree';
	with 'Attean::API::SPARQLSerializable';

	sub as_string {
		my $self	= shift;
		my ($lhs, $rhs)	= @{ $self->children };
		return sprintf("(%s %s %s)", $lhs->as_string, $self->operator, $rhs->as_string);
	}

	sub unaggregated_variables {
		my $self	= shift;
		return map { $_->unaggregated_variables } @{ $self->children };
	}
	
	my %ops	= (
		'-'		=> AtteanX::SPARQL::Token->fast_constructor( MINUS, -1, -1, -1, -1, ['-'] ),
		'+'		=> AtteanX::SPARQL::Token->fast_constructor( PLUS, -1, -1, -1, -1, ['+'] ),
		'*'		=> AtteanX::SPARQL::Token->fast_constructor( STAR, -1, -1, -1, -1, ['*'] ),
		'/'		=> AtteanX::SPARQL::Token->fast_constructor( SLASH, -1, -1, -1, -1, ['/'] ),
		'<'		=> AtteanX::SPARQL::Token->fast_constructor( LT, -1, -1, -1, -1, ['<'] ),
		'>'		=> AtteanX::SPARQL::Token->fast_constructor( GT, -1, -1, -1, -1, ['>'] ),
		'<='	=> AtteanX::SPARQL::Token->fast_constructor( LE, -1, -1, -1, -1, ['<='] ),
		'>='	=> AtteanX::SPARQL::Token->fast_constructor( GE, -1, -1, -1, -1, ['>='] ),
		'!='	=> AtteanX::SPARQL::Token->fast_constructor( NOTEQUALS, -1, -1, -1, -1, ['!='] ),
		'='		=> AtteanX::SPARQL::Token->fast_constructor( EQUALS, -1, -1, -1, -1, ['='] ),
		'&&'	=> AtteanX::SPARQL::Token->fast_constructor( ANDAND, -1, -1, -1, -1, ['&&'] ),
		'||'	=> AtteanX::SPARQL::Token->fast_constructor( OROR, -1, -1, -1, -1, ['||'] ),
	);
	
	sub sparql_tokens {
		my $self	= shift;
		my $op		= $ops{$self->operator} // die "No operator found in Attean::API::BinaryExpression->sparql_tokens";

		my @tokens;
		foreach my $t (@{ $self->children }) {
			push(@tokens, $t->sparql_tokens->elements);
			push(@tokens, $op);
		}
		pop(@tokens);
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
}

package Attean::API::NaryExpression 0.033 {
	use Moo::Role;
	with 'Attean::API::Expression', 'Attean::API::QueryTree';
	sub as_string {
		my $self	= shift;
		my @children	= map { $_->as_string } @{ $self->children };
		return sprintf("%s(%s)", $self->operator, join(', ', @children));
	}

	sub as_sparql {
		my $self	= shift;
		return $self->as_string;
	}

	sub unaggregated_variables {
		my $self	= shift;
		return map { $_->unaggregated_variables } @{ $self->children };
	}
}

package Attean::API::AggregateExpression 0.033 {
	use Moo::Role;
	requires 'operator';
	requires 'scalar_vars';
	with 'Attean::API::Expression', 'Attean::API::DirectedAcyclicGraph';

	sub as_string {
		my $self	= shift;
		my @children	= map { $_->as_string } @{ $self->children };
		return sprintf("%s(%s)", $self->operator, join(', ', @children));
	}

	sub as_sparql {
		my $self	= shift;
		return $self->as_string;
	}

	sub unaggregated_variables {
		return;
	}
}

1;

__END__

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
