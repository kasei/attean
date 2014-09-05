use v5.14;
use warnings;

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
	has 'value' => (is => 'ro', isa => ConsumerOf['Attean::API::Term']);
	sub as_string {
		my $self	= shift;
		my $str		= $self->value->ntriples_string;
		if ($str =~ m[^"(true|false)"\^\^<http://www[.]w3[.]org/2001/XMLSchema#boolean>$]) {
			return $1;
		}
		return $str;
	}
	sub evaluate {
		my $self	= shift;
		my $node	= $self->value;
		if ($node->does('Attean::API::Variable')) {
			my $binding	= shift;
			return $binding->value($node->value);
		} else {
			return $node;
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
	sub evaluate {
		my $self	= shift;
		my ($child)	= @{ $self->children };
		my $value	= $child->evaluate( @_ );
		die;
	}
	with 'Attean::API::UnaryExpression';
	with 'Attean::API::Expression', 'Attean::API::UnaryQueryTree';
}

package Attean::BinaryExpression 0.001 {
	use Moo;
	use Types::Standard qw(Enum);
	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(+ - * / < <= > >= != = && ||)];
		$type->assert_valid($self->operator);
	}
	sub evaluate {
		my $self		= shift;
		my ($lhs, $rhs)	= @{ $self->children };
		my $op			= $self->operator;
		
		if ($op eq '&&') {
		} elsif ($op eq '||') {
		} else {
			($lhs, $rhs)	= map { $_->evaluate(@_) } ($lhs, $rhs);
			if ($op eq '+') {
				
			} elsif ($op eq '-') {
				
			} elsif ($op eq '*') {
				
			} elsif ($op eq '/') {
				
			} else {
				die "Unimplemented operator evaluation: $op";
			}
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
	sub evaluate {
		my $self	= shift;
		die "Unimplemented FunctionExpression evaluation: " . $self->operator;
	}
}

1;
