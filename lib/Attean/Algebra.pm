use v5.14;
use warnings;

use Attean::API::Query;

package Attean::Algebra::Join 0.001 {
	use Moo;
	with 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
}

package Attean::Algebra::LeftJoin 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
}

package Attean::Algebra::Filter 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
}

package Attean::Algebra::Union 0.001 {
	use Moo;
	with 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
}

package Attean::Algebra::Graph 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'graph' => (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], required => 1);
}

package Attean::Algebra::Extend 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'variable' => (is => 'ro', isa => ConsumerOf['Attean::API::Variable'], required => 1);
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
}

package Attean::Algebra::Minus 0.001 {
	use Moo;
	with 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
}

package Attean::Algebra::Distinct 0.001 {
	use Moo;
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
}

package Attean::Algebra::Reduced 0.001 {
	use Moo;
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
}

package Attean::Algebra::Slice 0.001 {
	use Moo;
	use Types::Standard qw(Int);
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'limit' => (is => 'ro', isa => Int, default => -1);
	has 'offset' => (is => 'ro', isa => Int, default => 0);
}

package Attean::Algebra::Project 0.001 {
	use Moo;
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	use Types::Standard qw(ArrayRef ConsumerOf);
	has 'variables' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Variable']], required => 1);
}

package Attean::Algebra::_Comparator 0.001 {
	use Moo;
	use Types::Standard qw(Bool ConsumerOf);
	has 'ascending' => (is => 'ro', isa => Bool, default => 1);
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
}

package Attean::Algebra::OrderBy 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef InstanceOf);
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'expressions' => (is => 'ro', isa => ArrayRef[InstanceOf['_Comparator']], required => 1);
}

package Attean::Algebra::BGP 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	with 'Attean::API::Algebra', 'Attean::API::NullaryQueryTree';
	has 'triples' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Triple']], default => sub { [] });
}

package Attean::Algebra::Path 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::Algebra', 'Attean::API::NullaryQueryTree';
	has 'subject' => (is => 'ro', isa => ConsumerOf['Attean::API::BlankOrIRI'], required => 1);
	has 'path' => (is => 'ro', isa => ConsumerOf['Attean::API::PropertyPath'], required => 1);
	has 'object' => (is => 'ro', isa => ConsumerOf['Attean::API::Term'], required => 1);
}


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

package Attean::Algebra::InversePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub prefix_name { return "^" }
}

package Attean::Algebra::SequencePath 0.001 {
	use Moo;
	with 'Attean::API::NaryPropertyPath';
	sub separator { return "/" }
}

package Attean::Algebra::AlternativePath 0.001 {
	use Moo;
	with 'Attean::API::NaryPropertyPath';
	sub separator { return "|" }
}

package Attean::Algebra::ZeroOrMorePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "*" }
}

package Attean::Algebra::OneOrMorePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "+" }
}

package Attean::Algebra::ZeroOrOnePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "?" }
}

package Group 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'groupby' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Expression']]);
	has 'aggregates' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::AggregateExpression']]);
}


1;
