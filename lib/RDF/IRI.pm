use v5.14;
use warnings;

package RDF::IRI 0.001 {
	use Moose;
	use IRI;
	
	extends 'IRI';
	
	has 'ntriples_string'	=> (is => 'ro', isa => 'Str', lazy => 1, builder => '_ntriples_string');

	with 'RDF::API::Term';
	with 'RDF::BlankOrIRI';

	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 1) {
			return $class->$orig(value => shift, type => RDF::TermType::IRI->new());
		}
		return $class->$orig(@_);
	};
	
	sub as_string {
		my $self	= shift;
		return $self->abs;
	}
	
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
