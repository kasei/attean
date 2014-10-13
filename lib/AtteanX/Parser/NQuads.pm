use v5.14;
use warnings;

=head1 NAME

AtteanX::Parser::NQuads - N-Quads Parser

=head1 VERSION

This document describes AtteanX::Parser::NQuads version 0.001_01

=head1 SYNOPSIS

 use Attean;
 my $parser = Attean->get_parser('NQuads')->new();
 $parser->parse_cb_from_io( $fh );

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::NQuads 0.001 {
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

Copyright (c) 2006-2012 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
