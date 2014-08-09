use v5.14;
use warnings;

package RDF::TermType 0.001 {
	use Moose;
	use IRI;
	use RDF::TermType::Blank;
	use RDF::TermType::DatatypeLiteral;
	use RDF::TermType::IRI;
	use RDF::TermType::Variable;
	use RDF::TermType::LanguageLiteral;
}

1;
