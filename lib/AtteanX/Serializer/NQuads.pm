=head1 NAME

AtteanX::Serializer::NQuads - N-Triples Serializer

=head1 VERSION

This document describes AtteanX::Serializer::NQuads version 0.002

=head1 SYNOPSIS

 use Attean;
 my $serializer = Attean->get_serializer('NQuads')->new();
 $serializer->serialize_iter_to_io( $iter, $fh );

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

package AtteanX::Serializer::NQuads 0.001 {
	use Moo;
	use Types::Standard qw(Str ArrayRef);
	use Encode qw(encode);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);
	use namespace::clean;

	extends 'AtteanX::Serializer::NTuples';
	has 'canonical_media_type' => (is => 'ro', isa => Str, init_arg => undef, default => 'application/n-quads');

=item C<< media_types >>

Returns a list of media types that identify the format produced by this serializer.

=cut

	sub media_types {
		return [qw(application/n-quads)];
	}

	with 'Attean::API::MixedStatementSerializer';
	with 'Attean::API::AppendableSerializer';
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
