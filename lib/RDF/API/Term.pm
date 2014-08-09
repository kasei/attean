use v5.14;
use warnings;

package RDF::API::Term 0.001 {
	use Moose::Role;
	
	with 'RDF::TermOrVariable';

	has 'value' => (is => 'ro', isa => 'Str', required => 1);
	requires 'ntriples_string';
}

package RDF::API::Literal 0.001 {
	use Moose::Role;
	use IRI;
	use Moose::Util::TypeConstraints;
	
	with 'RDF::API::Term';
	coerce 'IRI' => from 'Str' => via { IRI->new(value => $_) };
	
	has 'language'			=> (is => 'ro', isa => 'Maybe[Str]', predicate => 'has_language');
	has 'datatype'			=> (is => 'ro', isa => 'IRI', required => 1, coerce => 1, default => sub { IRI->new(value => 'http://www.w3.org/2001/XMLSchema#string') });
}

1;
