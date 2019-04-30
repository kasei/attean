use v5.14;
use warnings;

=head1 NAME

Attean::Expression - SPARQL Expressions

=head1 VERSION

This document describes Attean::Expression version 0.023

=head1 SYNOPSIS

  use v5.14;
  use Attean;

  my $binding = Attean::Result->new();
  my $value = Attean::ValueExpression->new( value => Attean::Literal->integer(2) );
  my $plus = Attean::BinaryExpression->new( children => [$value, $value], operator => '+' );
  my $result = $plus->evaluate($binding);
  say $result->numeric_value; # 4

=head1 DESCRIPTION

This is a utility package that defines all the Attean SPARQL expression classes
consisting of logical, numeric, and function operators, constant terms, and
variables. Expressions may be evaluated in the context of a
L<Attean::API::Result> object, and either return a L<Attean::API::Term> object
or throw a type error exception.

The expression classes are:

=over 4

=cut

use Attean::API::Expression;

=item * L<Attean::ValueExpression>

=cut

package Attean::ValueExpression 0.023 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;
	use namespace::clean;

	with 'Attean::API::SPARQLSerializable';
	with 'Attean::API::Expression';

	has 'value' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable']);

	sub arity { return 0 }
	sub BUILDARGS {
		my $class	= shift;
		return $class->SUPER::BUILDARGS(@_, operator => '_value');
	}
	
	sub tree_attributes { return qw(operator) }

	sub is_stable {
		return 1;
	}
	
	sub as_string {
		my $self	= shift;
		my $str		= $self->value->ntriples_string;
		if ($str =~ m[^"(true|false)"\^\^<http://www[.]w3[.]org/2001/XMLSchema#boolean>$]) {
			return $1;
		} elsif ($str =~ m[^"(\d+)"\^\^<http://www[.]w3[.]org/2001/XMLSchema#integer>$]) {
			return $1
		}
		return $str;
	}
	
	sub in_scope_variables {
		my $self	= shift;
		if ($self->value->does('Attean::API::Variable')) {
			return $self->value->value;
		}
		return;
	}
	
	sub sparql_tokens {
		my $self	= shift;
		return $self->value->sparql_tokens;
	}

	sub unaggregated_variables {
		my $self	= shift;
		if ($self->value->does('Attean::API::Variable')) {
			return $self->value;
		}
		return;
	}
}

=item * L<Attean::UnaryExpression>

=cut

package Attean::UnaryExpression 0.023 {
	use Moo;
	use Types::Standard qw(Enum);
	use namespace::clean;

	with 'Attean::API::UnaryExpression', 'Attean::API::Expression', 'Attean::API::UnaryQueryTree';

	my %map	= ('NOT' => '!');
	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		my $args	= $class->$orig(@_);
		my $op		= $args->{operator};
		$args->{operator}	= $map{uc($op)} if (exists $map{uc($op)});
		return $args;
	};
	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(+ - !)];
		$type->assert_valid($self->operator);
	}
	
	sub tree_attributes { return qw(operator) }

	sub is_stable {
		my $self	= shift;
		foreach my $c (@{ $self->children }) {
			return 0 unless ($c->is_stable);
		}
		return 1;
	}
}

=item * L<Attean::BinaryExpression>

=cut

package Attean::BinaryExpression 0.023 {
	use Moo;
	use Types::Standard qw(Enum);
	use namespace::clean;

	with 'Attean::API::BinaryExpression';

	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(+ - * / < <= > >= != = && ||)];
		$type->assert_valid($self->operator);
	}
	
	sub tree_attributes { return qw(operator) }

	sub is_stable {
		my $self	= shift;
		foreach my $c (@{ $self->children }) {
			return 0 unless ($c->is_stable);
		}
		return 1;
	}
}

=item * L<Attean::FunctionExpression>

=cut

package Attean::FunctionExpression 0.023 {
	use Moo;
	use Types::Standard qw(Enum ConsumerOf HashRef);
	use Types::Common::String qw(UpperCaseStr);
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;
	use namespace::clean;

	has 'operator'		=> (is => 'ro', isa => UpperCaseStr, coerce => UpperCaseStr->coercion, required => 1);
	has 'base'			=> (is => 'rw', isa => ConsumerOf['Attean::IRI'], predicate => 'has_base');

	with 'Attean::API::NaryExpression';
	with 'Attean::API::SPARQLSerializable';


	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		my $args	= $class->$orig(@_);
		if ($args->{operator} eq 'ISURI') {
			$args->{operator}	= 'ISIRI';
		}
		$args->{operator}	= UpperCaseStr->coercion->($args->{operator});
		return $args;
	};
	
	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(INVOKE IN NOTIN STR LANG LANGMATCHES DATATYPE BOUND IRI URI BNODE RAND ABS CEIL FLOOR ROUND CONCAT SUBSTR STRLEN REPLACE UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS TIMEZONE TZ NOW UUID STRUUID MD5 SHA1 SHA256 SHA384 SHA512 COALESCE IF STRLANG STRDT SAMETERM ISIRI ISBLANK ISLITERAL ISNUMERIC REGEX)];
		$type->assert_valid($self->operator);
	}

	sub tree_attributes { return qw(operator) }

	sub is_stable {
		my $self	= shift;
		return 0 if ($self->operator =~ m/^(?:RAND|BNODE|UUID|STRUUID|NOW)$/);
		foreach my $c (@{ $self->children }) {
			return 0 unless ($c->is_stable);
		}
		return 1;
	}
	
	sub sparql_tokens {
		my $self	= shift;
		my $func	= AtteanX::SPARQL::Token->keyword($self->operator);
		my $lparen	= AtteanX::SPARQL::Token->lparen;
		my $rparen	= AtteanX::SPARQL::Token->rparen;
		my $comma	= AtteanX::SPARQL::Token->comma;

		my @tokens;
		push(@tokens, $func, $lparen);
		foreach my $t (@{ $self->children }) {
			push(@tokens, $t->sparql_tokens->elements);
			push(@tokens, $comma);
		}
		if (scalar(@tokens) > 2) {
			pop(@tokens);	# remove the last comma
		}
		push(@tokens, $rparen);
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
}

package Attean::AggregateExpression 0.023 {
	use Moo;
	use Types::Standard qw(Bool Enum Str HashRef ConsumerOf);
	use Types::Common::String qw(UpperCaseStr);
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;
	use namespace::clean;

	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		my $args	= $class->$orig(@_);
		$args->{operator}	= UpperCaseStr->coercion->($args->{operator});
		return $args;
	};
	sub BUILD {
		state $type	= Enum[qw(COUNT SUM MIN MAX AVG GROUP_CONCAT SAMPLE RANK)];
		$type->assert_valid(shift->operator);
	}
	has 'operator'		=> (is => 'ro', isa => UpperCaseStr, coerce => UpperCaseStr->coercion, required => 1);
	has 'scalar_vars'	=> (is => 'ro', isa => HashRef, default => sub { +{} });
	has 'distinct'		=> (is => 'ro', isa => Bool, default => 0);
	has 'variable'		=> (is => 'ro', isa => ConsumerOf['Attean::API::Variable'], required => 1);

	with 'Attean::API::AggregateExpression';
	with 'Attean::API::SPARQLSerializable';

	sub tree_attributes { return qw(operator scalar_vars variable) }

	sub is_stable {
		my $self	= shift;
		foreach my $expr (@{ $self->groups }, values %{ $self->aggregates }) {
			return 0 unless ($expr->is_stable);
		}
		return 1;
	}

	sub sparql_tokens {
		my $self	= shift;
		my $distinct	= AtteanX::SPARQL::Token->keyword('DISTINCT');
		my $func	= AtteanX::SPARQL::Token->keyword($self->operator);
		my $lparen	= AtteanX::SPARQL::Token->lparen;
		my $rparen	= AtteanX::SPARQL::Token->rparen;
		my $comma	= AtteanX::SPARQL::Token->comma;

		my @tokens;
		push(@tokens, $func);
		push(@tokens, $lparen);
		if ($self->distinct) {
			push(@tokens, $distinct);
		}
		foreach my $t (@{ $self->children }) {
			push(@tokens, $t->sparql_tokens->elements);
			push(@tokens, $comma);
		}
		if (scalar(@tokens) > 2) {
			pop(@tokens);	# remove the last comma
		}
		my $vars	= $self->scalar_vars;
		my @keys	= keys %$vars;
		if (scalar(@keys)) {
			die "TODO: Implement SPARQL serialization for aggregate scalar vars";
		}
		push(@tokens, $rparen);
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
}

package Attean::CastExpression 0.023 {
	use Moo;
	use Types::Standard qw(Enum ConsumerOf);
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;
	use namespace::clean;

	with 'Attean::API::SPARQLSerializable';
	with 'Attean::API::UnaryExpression', 'Attean::API::Expression', 'Attean::API::UnaryQueryTree';

	has 'datatype'	=> (is => 'ro', isa => ConsumerOf['Attean::API::IRI']);
	sub BUILDARGS {
		my $class	= shift;
		return $class->SUPER::BUILDARGS(@_, operator => '_cast');
	}
	sub BUILD {
		my $self	= shift;
		state $type	= Enum[map { "http://www.w3.org/2001/XMLSchema#$_" } qw(integer decimal float double string boolean dateTime)];
		$type->assert_valid($self->datatype->value);
	}
	
	sub tree_attributes { return qw(operator datatype) }

	sub is_stable {
		my $self	= shift;
		foreach my $c (@{ $self->children }) {
			return 0 unless ($c->is_stable);
		}
		return 1;
	}

	sub sparql_tokens {
		my $self	= shift;
		my $dt		= AtteanX::SPARQL::Token->fast_constructor( IRI, -1, -1, -1, -1, [$self->datatype->value] ),
		my $lparen	= AtteanX::SPARQL::Token->lparen;
		my $rparen	= AtteanX::SPARQL::Token->rparen;
		my $comma	= AtteanX::SPARQL::Token->comma;

		my @tokens;
		push(@tokens, $dt, $lparen);
		foreach my $t (@{ $self->children }) {
			push(@tokens, $t->sparql_tokens->elements);
			push(@tokens, $comma);
		}
		if (scalar(@tokens) > 2) {
			pop(@tokens);	# remove the last comma
		}
		push(@tokens, $rparen);
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
}

package Attean::ExistsExpression 0.023 {
	use Moo;
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;
	use Types::Standard qw(ConsumerOf);
	use namespace::clean;

	with 'Attean::API::SPARQLSerializable';
	with 'Attean::API::Expression';

	sub arity { return 0 }
	sub BUILDARGS {
		my $class	= shift;
		return $class->SUPER::BUILDARGS(@_, operator => '_exists');
	}
	has 'pattern' => (is => 'ro', isa => ConsumerOf['Attean::API::Algebra']);
	sub as_string {
		my $self	= shift;
		my $sparql	= $self->pattern->as_sparql;
		$sparql		=~ s/\s+/ /g;
		return "EXISTS { $sparql }";
	}

	sub tree_attributes { return qw(operator pattern) }

	sub is_stable {
		my $self	= shift;
		# TODO: need deep analysis of exists pattern to tell if this is stable
		# (there might be an unstable filter expression deep inside the pattern)
		return 0;
	}

	sub sparql_tokens {
		my $self	= shift;
		my $exists	= AtteanX::SPARQL::Token->keyword('EXISTS');
		my $lbrace	= AtteanX::SPARQL::Token->lbrace;
		my $rbrace	= AtteanX::SPARQL::Token->rbrace;
		my $child	= $self->pattern;
		
		my @tokens;
		push(@tokens, $exists, $lbrace);
		push(@tokens, $child->sparql_tokens->elements);
		push(@tokens, $rbrace);
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}

	sub unaggregated_variables {
		my $self	= shift;
		return map { Attean::Variable->new($_) } $self->pattern->in_scope_variables;
	}
}

package Attean::ExistsPlanExpression 0.023 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	use namespace::clean;

	with 'Attean::API::Expression';
	sub arity { return 0 }
	sub BUILDARGS {
		my $class	= shift;
		return $class->SUPER::BUILDARGS(@_, operator => '_existsplan');
	}
	has 'plan' => (is => 'ro', isa => ConsumerOf['Attean::API::BindingSubstitutionPlan']);
	sub as_string {
		my $self	= shift;
		# TODO: implement as_string for EXISTS patterns
		return "Attean::ExistsPlanExpression { ... }";
	}
	sub as_sparql {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{level} // 0;
		my $sp		= $args{indent} // '    ';
		my $indent	= $sp x $level;

		# TODO: implement as_string for EXISTS patterns
		return "EXISTS { " . $self->pattern->as_sparql( level => $level+1, indent => $sp ) . " }";
	}

	sub tree_attributes { return qw(operator plan) }

	sub is_stable {
		my $self	= shift;
		# TODO: need deep analysis of exists pattern to tell if this is stable
		# (there might be an unstable filter expression deep inside the pattern)
		return 0;
	}

	sub unaggregated_variables {
		my $self	= shift;
		die "unaggregated_variables cannot be called on Attean::ExistsPlanExpression";
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

Copyright (c) 2014--2019 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
