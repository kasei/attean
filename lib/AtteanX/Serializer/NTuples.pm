=head1 NAME

AtteanX::Serializer::NTuples - Shared functionality for N-Triples and N-Quads serializers

=head1 VERSION

This document describes AtteanX::Serializer::NTuples version 0.012

=head1 SYNOPSIS

 use Attean;
 my $serializer = Attean->get_serializer('NTriples')->new();
 $serializer->serialize_iter_to_io( $io, $fh );

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Serializer::NTuples 0.012 {
	use Moo;
	use Encode qw(encode);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);
	use namespace::clean;
	
=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >>.

=cut

	sub serialize_iter_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= shift;
		while (my $t = $iter->next()) {
			my $str = $t->tuples_string . "\n";
			$io->print($str);
		}
		return;
	}
	
=item C<< serialize_iter_to_bytes( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >>
and returns the serialization as a UTF-8 encoded byte string.

=cut

	sub serialize_iter_to_bytes {
		my $self	= shift;
		my $iter	= shift;
		my $data	= '';
		while (my $t = $iter->next()) {
			my $str = $t->tuples_string;
			$data	.= $str . "\n";
		}
		return encode('UTF-8', $data);
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

Copyright (c) 2014--2016 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
