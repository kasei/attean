# AtteanX::Parser::SPARQLXML
# -----------------------------------------------------------------------------

=head1 NAME

AtteanX::Parser::SPARQLXML - SPARQL XML Parser

=head1 VERSION

This document describes AtteanX::Parser::SPARQLXML version 0.001_01

=head1 SYNOPSIS

 use Attean;
 my $parser = Attean->get_parser('SPARQLXML')->new();
 $parser->parse_cb_from_io( $fh );

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Parser::SPARQLXML 0.001 {
	use XML::SAX::ParserFactory;
	use Attean;
	use Moo;
	use AtteanX::Parser::SPARQLXML::SAXHandler;
	
=item C<< canonical_media_type >>

Returns the canonical media type for SPARQL XML: application/sparql-results+xml.

=cut

	sub canonical_media_type { return "application/sparql-results+xml" }

=item C<< media_types >>

Returns a list of media types that may be parsed with the SPARQL XML parser:
application/sparql-results+xml.

=cut

	sub media_types {
		return [qw(application/sparql-results+xml)];
	}
	
=item C<< file_extensions >>

Returns a list of file extensions that may be parsed with the parser.

=cut

	sub file_extensions { return [qw(srx)] }

	with 'Attean::API::ResultOrTermParser';
	with 'Attean::API::PushParser';

=item C<< parse_cb_from_io( $fh ) >>

Calls the C<< $parser->handler >> function once for each
L<Attean::API::Binding> object that result from parsing
the data read from the L<IO::Handle> object C<< $fh >>.

=cut

	sub parse_cb_from_io {
		my $self	= shift;
		my $fh		= shift;
		my $handler	= AtteanX::Parser::SPARQLXML::SAXHandler->new($self->handler);
		my $p		= XML::SAX::ParserFactory->parser(Handler => $handler);
		$p->parse_file( $fh );
	}

=item C<< parse_cb_from_bytes( $data ) >>

Calls the C<< $parser->handler >> function once for each
L<Attean::API::Binding> object that result from parsing
the data read from the UTF-8 encoded byte string C<< $data >>.

=cut

	sub parse_cb_from_bytes {
		my $self	= shift;
		my $data	= shift;
	
		my $handler	= AtteanX::Parser::SPARQLXML::SAXHandler->new($self->handler);
		my $p		= XML::SAX::ParserFactory->parser(Handler => $handler);
		$p->parse_string( $data );
	}
}


1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/perlrdf/issues>.

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2006-2012 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
