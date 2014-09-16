use v5.14;
use warnings;

=head1 NAME

Attean::QuadModel - RDF model backed by a quad-store

=head1 VERSION

This document describes Attean::QuadModel version 0.001

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $model = Attean::QuadModel->new( store => $store );

=head1 DESCRIPTION

The Attean::QuadModel class represents a model that is backed by a single
L<Attean::API::QuadStore|Attean::API::Store> object.
It conforms to the L<Attean::API::Model> role.

The Attean::QuadModel constructor requires one named argument:

=over 4

=item store

A L<Attean::API::QuadStore|Attean::API::Store> object representing the backing
quad-store.

=back

=head1 METHODS

=over 4

=cut

package Attean::QuadModel 0.001 {
	use Moo;
	use Scalar::Util qw(reftype);
	use namespace::clean;

	has 'store'	=> (
		is => 'ro',
		does => 'Attean::API::QuadStore',
		required => 1,
		handles	=> [qw(size count_quads get_graphs)],
	);
	
=item C<< get_quads ( $subject, $predicate, $object, $graph ) >>

Returns an L<Attean::API::Iterator> for quads in the model that match the
supplied C<< $subject >>, C<< $predicate >>, C<< $object >>, and C<< $graph >>.
Any of these terms may be undefined or a L<Attean::API::Variable> object, in
which case that term will be considered as a wildcard for the purposes of
matching.

The returned iterator conforms to both L<Attean::API::Iterator> and
L<Attean::API::QuadIterator>.

=cut

	sub get_quads {
		my $self	= shift;
		my @nodes	= @_[0..3];
		foreach my $i (0..3) {
			my $t	= $nodes[$i];
			if (not(ref($t)) or reftype($t) ne 'ARRAY') {
				$nodes[$i]	= [$t];
			}
		}
		
		my @iters;
		foreach my $s (@{ $nodes[0] }) {
			foreach my $p (@{ $nodes[1] }) {
				foreach my $o (@{ $nodes[2] }) {
					foreach my $g (@{ $nodes[3] }) {
						push(@iters, $self->store->get_quads($s, $p, $o, $g));
					}
				}
			}
		}
		if (scalar(@iters) <= 1) {
			return shift(@iters);
		} else {
			return Attean::IteratorSequence->new( iterators => \@iters, item_type => $iters[0]->item_type );
		}
	}
	
	with 'Attean::API::Model';
}


package Attean::MutableQuadModel 0.001 {
	use Moo;
	extends 'Attean::QuadModel';
	
	has 'store'	=> (
		is => 'ro',
		does => 'Attean::API::MutableQuadStore',
		required => 1,
		handles	=> [qw(size count_quads add_quad remove_quad get_graphs create_graph drop_graph clear_graph add_iter)],
	);

	with 'Attean::API::MutableModel';
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO

L<http://www.perlrdf.org/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
