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

The Attean::Expression class represents a SPARQL expression consisting of
logical, numeric, and function operators, constant terms, and variables.
Expressions may be evaluated in the context of a L<Attean::API::Result> object,
and either return a L<Attean::API::Term> object or throw a type error exception.

=head1 METHODS

=over 4

=cut

use Attean::API::Expression;

package Attean::Expression 0.001 {}

package Attean::ValueExpression 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
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

=item C<< impl >>

Returns a CODE reference that when called with a L<Attean::API::Result>
argument, will evaluate the expression and return the resulting
L<Attean::API::Term> object (or throw a type error exception).

=cut

	sub impl {
		my $self	= shift;
		my $node	= $self->value;
		if ($node->does('Attean::API::Variable')) {
			return sub {
				my $binding	= shift;
				return $binding->value($node->value);
			};
		} else {
			return sub {
				return $node;
			};
		}
	}
}

package Attean::UnaryExpression 0.001 {
	use Moo;
	use Types::Standard qw(Enum);
	my %map	= ('NOT' => '!');
	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		my $args	= $class->$orig(@_);
		my $op		= $args->{operator};
		if (exists $map{uc($op)}) {
			$args->{operator}	= $map{uc($op)};
		}
		return $args;
	};
	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(+ - !)];
		$type->assert_valid($self->operator);
	}
	
	sub impl {
		my $self	= shift;
		return sub {
			die "Unimplemented UnaryExpression evaluation: " . $self->operator;
		};
	}
	
	with 'Attean::API::UnaryExpression';
	with 'Attean::API::Expression', 'Attean::API::UnaryQueryTree';
}

package Attean::BinaryExpression 0.001 {
	use Moo;
	use Try::Tiny;
	use Types::Standard qw(Enum);
	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(+ - * / < <= > >= != = && ||)];
		$type->assert_valid($self->operator);
	}
	
	sub impl {
		my $self		= shift;
		my ($lhs, $rhs)	= @{ $self->children };
		my $op			= $self->operator;
		my ($lhsi, $rhsi)	= map { $_->impl } ($lhs, $rhs);
		my $true	= Attean::Literal->true;
		my $false	= Attean::Literal->false;
		if ($op eq '&&') {
			return sub {
				my $lbv	= eval { $lhsi->(@_) };
				my $rbv	= eval { $rhsi->(@_) };
				die "TypeError" unless ($lbv or $rbv);
				return $false if (not($lbv) and not($rbv->ebv));
				return $false if (not($rbv) and not($lbv->ebv));
				die "TypeError" unless ($lbv and $rbv);
				return ($lbv->ebv && $rbv->ebv) ? $true : $false;
			}
		} elsif ($op eq '||') {
			return sub {
				my $lbv	= eval { $lhsi->(@_) };
				return $true if ($lbv and $lbv->ebv);
				my $rbv	= eval { $rhsi->(@_) };
				die "TypeError" unless ($rbv);
				return $true if ($rbv->ebv);
		
				if ($lbv) {
					return $false;
				} else {
					die "TypeError";
				}
			}
		} else { # numeric operators: - + * /
			return sub {
				($lhs, $rhs)	= map { $_->(@_) } ($lhsi, $rhsi);
				for ($lhs, $rhs) {
					die "TypeError" unless $_->does('Attean::API::NumericLiteral');
				}
				my $lv	= $lhs->numeric_value;
				my $rv	= $rhs->numeric_value;
				if ($op eq '+') {
					return Attean::Literal->new( value => ($lv + $rv), datatype => $lhs->binary_promotion_type($rhs, $op) );
				} elsif ($op eq '-') {
					return Attean::Literal->new( value => ($lv - $rv), datatype => $lhs->binary_promotion_type($rhs, $op) );
				} elsif ($op eq '*') {
					return Attean::Literal->new( value => ($lv * $rv), datatype => $lhs->binary_promotion_type($rhs, $op) );
				} elsif ($op eq '/') {
					return Attean::Literal->new( value => ($lv / $rv), datatype => $lhs->binary_promotion_type($rhs, $op) );
				} else {
					die "Unimplemented operator evaluation: $op";
				}
			};
		}
	}
	
	with 'Attean::API::BinaryExpression';
}

package Attean::FunctionExpression 0.001 {
	use Moo;
	use Types::Standard qw(Enum);
	use Types::Common::String qw(UpperCaseStr);
	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		my $args	= $class->$orig(@_);
		$args->{operator}	= UpperCaseStr->coercion->($args->{operator});
		return $args;
	};
	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(STR LANG LANGMATCHES DATATYPE BOUND IRI URI BNODE RAND ABS CEIL FLOOR ROUND CONCAT SUBSTR STRLEN REPLACE UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS TIMEZONE TZ NOW UUID STRUUID MD5 SHA1 SHA256 SHA384 SHA512 COALESCE IF STRLANG STRDT SAMETERM ISIRI ISURI ISBLANK ISLITERAL ISNUMERIC REGEX)];
		$type->assert_valid($self->operator);
	}
	has 'operator' => (is => 'ro', isa => UpperCaseStr, coerce => UpperCaseStr->coercion, required => 1);
	with 'Attean::API::NaryExpression';
	
	sub impl {
		my $self	= shift;
		return sub {
			die "Unimplemented FunctionExpression evaluation: " . $self->operator;
		};
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
