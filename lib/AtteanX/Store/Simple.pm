=head1 NAME

AtteanX::Store::Simple - Simple, unindexed, in-memory RDF store

=head1 VERSION

This document describes AtteanX::Store::Simple version 0.027

=head1 SYNOPSIS

 use AtteanX::Store::Simple;

=head1 DESCRIPTION

AtteanX::Store::Simple provides an in-memory quad-store.

=cut

use v5.14;
use warnings;

package AtteanX::Store::Simple 0.027 {
	use Moo;
	use Type::Tiny::Role;
	use Types::Standard qw(Int ArrayRef HashRef ConsumerOf InstanceOf);
	use Encode;
	use Set::Scalar;
	use Digest::SHA;
	use List::Util qw(first);
	use Scalar::Util qw(refaddr reftype blessed);
	use namespace::clean;

	with 'Attean::API::QuadStore';

=head1 METHODS

Beyond the methods documented below, this class inherits methods from the
L<Attean::API::QuadStore> class.

=over 4

=item C<< new ( quads => \@quads ) >>

Returns a new memory-backed storage object.

=cut

	has quads => (is => 'rw', isa => ArrayRef[ConsumerOf['Attean::API::Quad']], default => sub { [] });

=item C<< get_quads ( $subject, $predicate, $object, $graph ) >>

Returns a stream object of all statements matching the specified subject,
predicate and objects. Any of the arguments may be undef to match any value.

=cut

	sub get_quads {
		my $self	= shift;
		my @nodes	= @_;
		my $quads	= $self->quads;
		my $iter	= Attean::ListIterator->new( values => $quads, item_type => 'Attean::API::Quad' );
		return $iter->matching_pattern(@nodes);
	}

}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/perlrdf2/issues>.

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2020 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
