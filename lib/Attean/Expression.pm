use v5.14;
use warnings;

use Attean::API::Expression;

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
		return $self->value->ntriples_string;
	}
}

package Attean::UnaryExpression 0.001 {
	use Moo;
	with 'Attean::API::UnaryExpression';
	with 'Attean::API::Expression', 'Attean::API::UnaryQueryTree';
}

package Attean::BinaryExpression 0.001 {
	use Moo;
	with 'Attean::API::BinaryExpression';
}

package Attean::FunctionExpression 0.001 {
	use Moo;
	with 'Attean::API::NaryExpression';
}

1;
