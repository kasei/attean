use v5.14;
use warnings;

package RDF::BlankOrIRI 0.001 {
	use Moose::Role;
}

package RDF::TermOrVariable 0.001 {
	use Moose::Role;
}

package RDF::API 0.001 {
	use Moose::Util::TypeConstraints;

	use RDF::API::Term;
	use RDF::API::Store;
	use RDF::API::Model;
	use RDF::API::Iterator;
	use RDF::API::Parser;
	use RDF::API::Serializer;

	use RDF::Variable;
	use RDF::Blank;
	use RDF::IRI;
}

1;
