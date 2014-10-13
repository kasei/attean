=head1 NAME

Attean::RDF - Utility package for exporting shorthand functions for constructing RDF objects

=head1 VERSION

This document describes Attean::RDF version 0.001_01

=head1 SYNOPSIS

  use v5.14;
  use Attean::RDF;
  
  my $s = blank('b');
  my $p = iri('http://xmlns.com/foaf/0.1/name');
  my $o = langliteral("Eve", "en");
  my $triple = triple($s, $p, $o);
  say $triple->as_string; # _:b <http://xmlns.com/foaf/0.1/name> "Eve"@en .

=head1 DESCRIPTION

This is a utility package for exporting shorthand functions for constructing
RDF objects such as IRIs, Literals, Blanks, Triples, etc.

=head1 FUNCTIONS

All of the functions defined in this package may be exported (and are exported
by default).

=over 4

=cut

package Attean::RDF 0.001 {
	use v5.14;
	use warnings;

	use Attean;
	use List::MoreUtils qw(zip);
	require Exporter;
	use namespace::clean;

	our @ISA	= qw(Exporter);
	our @EXPORT	= qw(iri blank literal dtliteral langliteral variable triple quad triplepattern);

=item C<< variable( $value ) >>

C<< Attean::Variable->new($value) >>

=cut

	sub variable {
		return Attean::Variable->new(@_);
	}

=item C<< iri( $value ) >>

C<< Attean::IRI->new($value) >>

=cut

	sub iri {
		return Attean::IRI->new(@_);
	}

=item C<< blank( $value ) >>

C<< Attean::Blank->new($value) >>

=cut

	sub blank {
		return Attean::Blank->new(@_);
	}

=item C<< literal( $value ) >>

C<< Attean::Literal->new($value) >>

=cut

	sub literal {
		return Attean::Literal->new(@_);
	}

=item C<< dtliteral( $value, $dt ) >>

C<< Attean::Literal->new( value => $value, datatype => $dt ) >>

=cut

	sub dtliteral {
		my @k	= qw(value datatype);
		return Attean::Literal->new(zip @k, @_);
	}

=item C<< langliteral( $value, $lang ) >>

C<< Attean::Literal->new( value => $value, language => $lang ) >>

=cut

	sub langliteral {
		my @k	= qw(value language);
		return Attean::Literal->new(zip @k, @_);
	}
	
=item C<< triple( @terms ) >>

C<< Attean::Triple->new( @terms ) >>

=cut

	sub triple {
		return Attean::Triple->new(@_);
	}
	
=item C<< triplepattern( @terms ) >>

C<< Attean::TriplePattern->new( @terms ) >>

=cut

	sub triplepattern {
		return Attean::TriplePattern->new(@_);
	}
	
=item C<< quad( @terms ) >>

C<< Attean::Quad->new( @terms ) >>

=cut

	sub quad {
		return Attean::Quad->new(@_);
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO

L<IRI>

L<http://www.ietf.org/rfc/rfc3987.txt>

L<http://www.perlrdf.org/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
