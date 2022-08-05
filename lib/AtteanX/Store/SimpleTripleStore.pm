=head1 NAME

AtteanX::Store::SimpleTripleStore - SimpleTripleStore, unindexed, in-memory RDF store

=head1 VERSION

This document describes AtteanX::Store::SimpleTripleStore version 0.031

=head1 SYNOPSIS

 use AtteanX::Store::SimpleTripleStore;

=head1 DESCRIPTION

AtteanX::Store::SimpleTripleStore provides an in-memory triple-store.

=cut

use v5.14;
use warnings;

package AtteanX::Store::SimpleTripleStore 0.031 {
	use Moo;
	use Type::Tiny::Role;
	use Types::Standard qw(Int ArrayRef HashRef ConsumerOf InstanceOf);
	use Encode;
	use Set::Scalar;
	use Digest::SHA;
	use List::Util qw(first);
	use Scalar::Util qw(refaddr reftype blessed);
	use namespace::clean;

	with 'Attean::API::MutableTripleStore';

	my @pos_names	= Attean::API::Quad->variables;

=head1 METHODS

Beyond the methods documented below, this class inherits methods from the
L<Attean::API::QuadStore> class.

=over 4

=item C<< new ( triples => \@triples ) >>

Returns a new memory-backed storage object.

=cut

	has triples => (is => 'rw', isa => ArrayRef[ConsumerOf['Attean::API::Triple']], default => sub { [] });

=item C<< get_triples ( $subject, $predicate, $object ) >>

Returns a stream object of all statements matching the specified subject,
predicate and objects. Any of the arguments may be undef to match any value.

=cut

	sub get_triples {
		my $self	= shift;
		my @nodes	= @_;
		my %bound;
		foreach my $pos (0 .. 2) {
			my $n	= $nodes[ $pos ];
			if (blessed($n) and $n->does('Attean::API::Variable')) {
				$n	= undef;
				$nodes[$pos]	= undef;
			}
			if (blessed($n)) {
				$bound{ $pos_names[$pos] }	= $n;
			}
		}
		
		my $triples	= $self->triples;
		my $iter	= Attean::ListIterator->new( values => $triples, item_type => 'Attean::API::Triple' );
		return $iter->grep(sub {
			my $q	= shift;
			foreach my $key (keys %bound) {
				my $term	= $q->$key();
				unless ($term->equals( $bound{$key} )) {
					return 0;
				}
			}
			return 1;
		});
		return $iter;
	}
	
=item C<< add_triple( $t ) >>

=cut

	sub add_triple {
		my $self	= shift;
		my $t		= shift;
		push(@{ $self->triples }, $t);
	}
	
=item C<< remove_triple( $t ) >>

=cut

	sub remove_triple {
		my $self	= shift;
		my $t		= shift;
		my @remove;
		my $triples	= $self->triples;
		foreach my $i (0 .. $#{ $triples }) {
			my $u	= $triples->[$i];
			if ($u->as_string eq $t->as_string) {
				push(@remove, $i);
			}
		}
		while (scalar(@remove)) {
			my $i	= pop(@remove);
			splice(@$triples, $i, 1, ());
		}
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

Copyright (c) 2014--2022 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
