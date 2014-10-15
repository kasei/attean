use v5.14;
use warnings;

=head1 NAME

Attean::IteratorSequence - Iterator implementation backed by zero or more sub-iterators

=head1 VERSION

This document describes Attean::IteratorSequence version 0.002

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $iter = Attean::IteratorSequence->new(iterators => [$iter1, $iter2]);

=head1 DESCRIPTION

The Attean::IteratorSequence class represents a typed iterator that is backed
by zero or more sub-iterators. When iterated over, it will return all the
elements of all of its sub-iterators, in order, before returning undef.
It conforms to the L<Attean::API::Iterator> role.

The Attean::IteratorSequence constructor requires two named arguments:

=over 4

=item iterators

An array reference containing zero or more L<Attean::API::Iterator> objects.

=item item_type

A string representing the type of the items that will be returned from the
iterator.

=back

=head1 METHODS

=over 4

=cut

package Attean::IteratorSequence 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	use namespace::clean;
	
	with 'Attean::API::Iterator';
	
	has iterators => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Iterator']], required => 1);
	
=item C<< next >>

Returns the iterator's next item, or undef upon reaching the end of iteration.

=cut

	sub next {
		my $self	= shift;
		my $list	= $self->iterators;
		
		while (1) {
			return unless (scalar(@$list));
			my $iter	= $list->[0];
			my $item	= $iter->next;
			unless (defined($item)) {
				shift(@$list);
				next;
			}
			return $item;
		}
	}
	
=item C<< push( $iterator ) >>

Adds the new C<< $iterator >> to the end of the array of sub-iterators.

After this call, C<< $iterator >> will be owned by the IteratorSequence,
so making any method calls on C<< $iterator >> after this point may produce
unexpected results.

=cut

	sub push {
		my $self	= shift;
		my $iter	= shift;
		push(@{ $self->iterators }, $iter);
		return;
	}
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
