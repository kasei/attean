use v5.14;
use warnings;

=head1 NAME

AtteanX::Parser::NQuads - N-Quads Parser

=head1 VERSION

This document describes AtteanX::Parser::NQuads version 0.035

=head1 SYNOPSIS

 use Attean;
 my $parser = Attean->get_parser('NQuads')->new();

 # Parse data from a file-handle and handle quads in the 'handler' callback
 $parser->parse_cb_from_io( $fh );
 
 # Parse the given byte-string, and return an iterator of quads
 my $iter = $parser->parse_iter_from_bytes('<http://example.org/subject> <tag:example.org:predicate> "object" <http://example.org/graph> .');
 while (my $quad = $iter->next) {
   print $quad->as_string;
 }

=head1 DESCRIPTION

This module implements a parser for the N-Quads format.

=head1 ROLES

This class consumes L<Attean::API::Parser>, L<Attean::API::PullParser>
and <Attean::API::MixedStatementParser>.

=head1 METHODS

=over 4

=item C<< parse_iter_from_io( $fh ) >>

Returns an L<Attean::API::Iterator> that result from parsing the data read from
the L<IO::Handle> object C<< $fh >>.

=item C<< parse_iter_from_bytes( $data ) >>

Returns an L<Attean::API::Iterator> that result from parsing the data read from
the UTF-8 encoded byte string C<< $data >>.

=cut

package AtteanX::Parser::NQuads 0.035 {
	use utf8;
	
	use Attean;
	use Moo;
	extends 'AtteanX::Parser::NTuples';
	
=item C<< canonical_media_type >>

Returns the canonical media type for N-Quads: application/n-quads.

=cut

	sub canonical_media_type { return "application/n-quads" }

=item C<< media_types >>

Returns a list of media types that may be parsed with the N-Triples parser:
application/n-quads.

=cut

	sub media_types {
		return [qw(application/n-quads)];
	}
	
=item C<< file_extensions >>

Returns a list of file extensions that may be parsed with the parser.

=cut

	sub file_extensions { return [qw(nq)] }
	
	with 'Attean::API::MixedStatementParser';
	with 'Attean::API::PullParser';
	with 'Attean::API::CDTBlankNodeMappingParser';

	sub _binding {
		my $self	= shift;
		my $nodes	= shift;
		my $lineno	= shift;
		if (scalar(@$nodes) == 3) {
			return Attean::Triple->new(@$nodes);
		} elsif (scalar(@$nodes) == 4) {
			return Attean::Quad->new(@$nodes);
		} else {
			die qq[Not valid N-Quads data at line $lineno];
		}
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

Copyright (c) 2014--2022 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
