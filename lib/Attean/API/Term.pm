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
	
	has 'language'			=> (is => 'ro', isa => 'Maybe[Str]', predicate => 'has_language');
	has 'datatype'			=> (is => 'ro', isa => 'IRI', required => 1, coerce => 1, default => sub { IRI->new(value => 'http://www.w3.org/2001/XMLSchema#string') });
}

package RDF::API::IRI 0.001 {
	use Moose::Role;
	use IRI;
	use Moose::Util::TypeConstraints;
	
	with 'RDF::API::Term';
	
	sub _ntriples_string {
		my $self	= shift;
		my $iri		= $self->value;
		my @chars	= split(//, $iri);
		my $string	= '';
		while (scalar(@chars)) {
			my $c	= shift(@chars);
			my $o	= ord($c);
			if ($o < 0x8) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o == 0x9) {
				$string	.= "\\t";
			} elsif ($o == 0xA) {
				$string	.= "\\n";
			} elsif ($o < 0xC) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o == 0xD) {
				$string	.= "\\r";
			} elsif ($o < 0x1F) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o < 0x21) {
				$string	.= $c;
			} elsif ($o == 0x22) {
				$string	.= "\"";
			} elsif ($o < 0x5B) {
				$string	.= $c;
			} elsif ($o == 0x5C) {
				$string	.= "\\";
			} elsif ($o < 0x7E) {
				$string	.= $c;
			} elsif ($o < 0xFFFF) {
				$string	.= sprintf("\\u%04X", $o);
			} else {
				$string	.= sprintf("\\U%08X", $o);
			}
		}
		my $ntriples	= '<' . $string . '>';
		return $ntriples;
	}
}

1;
