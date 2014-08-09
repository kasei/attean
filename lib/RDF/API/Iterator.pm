use v5.14;
use warnings;

package RDF::API::Iterator 0.001 {
	use Moose::Role;
	use Moose::Util::TypeConstraints;
	
	has 'item_type' => (is => 'ro', isa => 'Moose::Meta::TypeConstraint', required => 1);
	requires 'next';
}

1;
