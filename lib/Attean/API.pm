use v5.14;
use warnings;

package Attean::API::BlankOrIRI 0.001 {
	use Moose::Role;
}

package Attean::TermOrVariable 0.001 {
	use Moose::Role;
}

package Attean::API 0.001 {
	use Moose::Util::TypeConstraints;

	use Attean::API::Term;
	use Attean::API::Store;
	use Attean::API::Model;
	use Attean::API::Iterator;
	use Attean::API::Parser;
	use Attean::API::Serializer;

	use Attean::Variable;
	use Attean::Blank;
	use Attean::IRI;
}

1;
