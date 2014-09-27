use v5.14;
use warnings;

=head1 NAME

Attean::Expression - SPARQL Expressions

=head1 VERSION

This document describes Attean::Expression version 0.001

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

package Attean::ValueExpression 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	use namespace::clean;

	with 'Attean::API::Expression';
	sub arity { return 0 }
	sub BUILDARGS {
		my $class	= shift;
		return $class->SUPER::BUILDARGS(@_, operator => '_value');
	}
	has 'value' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable']);
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
}

=item * L<Attean::UnaryExpression>

=cut

package Attean::UnaryExpression 0.001 {
	use Moo;
	use Types::Standard qw(Enum);
	use namespace::clean;

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
	
	with 'Attean::API::UnaryExpression', 'Attean::API::Expression', 'Attean::API::UnaryQueryTree';
}

=item * L<Attean::BinaryExpression>

=cut

package Attean::BinaryExpression 0.001 {
	use Moo;
	use Types::Standard qw(Enum);
	use namespace::clean;

	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(+ - * / < <= > >= != = && ||)];
		$type->assert_valid($self->operator);
	}
	
	with 'Attean::API::BinaryExpression';
}

=item * L<Attean::FunctionExpression>

=cut

package Attean::FunctionExpression 0.001 {
	use Moo;
	use Types::Standard qw(Enum ConsumerOf HashRef);
	use Types::Common::String qw(UpperCaseStr);
	use namespace::clean;

	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		my $args	= $class->$orig(@_);
		$args->{operator}	= UpperCaseStr->coercion->($args->{operator});
		return $args;
	};
	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(IN NOTIN STR LANG LANGMATCHES DATATYPE BOUND IRI URI BNODE RAND ABS CEIL FLOOR ROUND CONCAT SUBSTR STRLEN REPLACE UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS TIMEZONE TZ NOW UUID STRUUID MD5 SHA1 SHA256 SHA384 SHA512 COALESCE IF STRLANG STRDT SAMETERM ISIRI ISURI ISBLANK ISLITERAL ISNUMERIC REGEX)];
		$type->assert_valid($self->operator);
	}
	has 'operator'		=> (is => 'ro', isa => UpperCaseStr, coerce => UpperCaseStr->coercion, required => 1);
	has 'base'			=> (is => 'rw', isa => ConsumerOf['Attean::IRI'], predicate => 'has_base');
	with 'Attean::API::NaryExpression';
}

package Attean::AggregateExpression 0.001 {
	use Moo;
	use Types::Standard qw(Bool Enum Str HashRef ConsumerOf);
	use Types::Common::String qw(UpperCaseStr);
	use namespace::clean;

	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		my $args	= $class->$orig(@_);
		$args->{operator}	= UpperCaseStr->coercion->($args->{operator});
		return $args;
	};
	sub BUILD {
		state $type	= Enum[qw(COUNT SUM MIN MAX AVG GROUP_CONCAT SAMPLE)];
		$type->assert_valid(shift->operator);
	}
	has 'distinct'		=> (is => 'ro', isa => Bool, default => 0);
	has 'operator'		=> (is => 'ro', isa => UpperCaseStr, coerce => UpperCaseStr->coercion, required => 1);
	has 'scalar_vars'	=> (is => 'ro', isa => HashRef[Str], default => sub { +{} });
	has 'variable'		=> (is => 'ro', isa => ConsumerOf['Attean::API::Variable'], required => 1);
	with 'Attean::API::AggregateExpression';
}

package Attean::CastExpression 0.001 {
	use Moo;
	use Types::Standard qw(Enum ConsumerOf);
	use namespace::clean;

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
	
	with 'Attean::API::UnaryExpression', 'Attean::API::Expression', 'Attean::API::UnaryQueryTree';
}

package Attean::ExistsExpression 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	use namespace::clean;

	with 'Attean::API::Expression';
	sub arity { return 0 }
	sub BUILDARGS {
		my $class	= shift;
		return $class->SUPER::BUILDARGS(@_, operator => '_exists');
	}
	has 'pattern' => (is => 'ro', isa => ConsumerOf['Attean::API::Algebra']);
	sub as_string {
		my $self	= shift;
		# TODO: implement as_string for EXISTS patterns
		return "EXISTS { ... }";
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
