use v5.14;
use warnings;

package RDF::API::Term 0.001 {
	use Moose::Role;
	
	with 'RDF::TermOrVariable';

	has 'value' => (is => 'ro', isa => 'Str', required => 1);
	has 'type'	=> (is => 'ro', isa => 'RDF::TermType', required => 1);
	requires 'ntriples_string';
}

package RDF::API::Literal 0.001 {
	use Moose::Role;
	
	with 'RDF::API::Term';
	
	requires 'datatype';
}

1;
