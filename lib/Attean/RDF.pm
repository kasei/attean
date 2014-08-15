# Exported shorthand functions for constructing RDF objects
# such as IRIs, Literals, Blanks, Triples, etc.

package Attean::RDF 0.001 {
	use Attean;
	use List::MoreUtils qw(zip);
	require Exporter;
	our @ISA	= qw(Exporter);
	@EXPORT	= qw(iri blank literal dtliteral langliteral variable triple);

	sub iri {
		return Attean::IRI->new(@_);
	}

	sub blank {
		return Attean::Blank->new(@_);
	}

	sub literal {
		return Attean::Literal->new(@_);
	}

	sub dtliteral {
		my @k	= qw(value datatype);
		return Attean::Literal->new(zip @k, @_);
	}

	sub langliteral {
		my @k	= qw(value language);
		return Attean::Literal->new(zip @k, @_);
	}
	
	sub triple {
		return Attean::Triple->new(@_);
	}
}

1;
