=head1 NAME

AtteanX::Serializer::NTriples - N-Triples Serializer

=head1 VERSION

This document describes AtteanX::Serializer::NTriples version 0.000

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

package AtteanX::Serializer::NTriples 0.001 {
	use Moo;
	use Types::Standard qw(Str ArrayRef);
	use Encode qw(encode);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);
	use namespace::clean;

	extends 'AtteanX::Serializer::NTuples';
	has 'canonical_media_type' => (is => 'ro', isa => Str, init_arg => undef, default => 'application/n-triples');
	has 'media_types' => (is => 'ro', isa => ArrayRef[Str], init_arg => undef, default => sub { ['application/n-triples'] });
	with 'Attean::API::TripleSerializer';
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
