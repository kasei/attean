=head1 NAME

AtteanX::Serializer::CanonicalNTriples - Canonical N-Triples Serializer

=head1 VERSION

This document describes AtteanX::Serializer::CanonicalNTriples version 0.000

=head1 SYNOPSIS

 use Attean;
 my $serializer = Attean->get_serializer('NTriples')->new();
 $serializer->serialize_iter_to_io( $iter, $fh );

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Serializer::CanonicalNTriples 0.001 {
	use Moo;
	use Encode;
	extends 'AtteanX::Serializer::NTriples';
	use namespace::clean;
	
=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >>.

=cut

	sub serialize_iter_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= shift;
		my $bytes	= $self->serialize_iter_to_bytes($iter);
		$io->print(decode('UTF-8', $bytes, Encode::FB_CROAK));
		return;
	}
	
=item C<< serialize_iter_to_bytes( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >>
and returns the serialization as a UTF-8 encoded byte string.

=cut

	sub serialize_iter_to_bytes {
		my $self	= shift;
		my $iter	= shift;
		$iter		= $iter->materialize;
		my $triples	= $iter->canonical_set();
		
		my $data	= '';
		foreach my $t (@$triples) {
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

Copyright (c) 2006-2012 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
