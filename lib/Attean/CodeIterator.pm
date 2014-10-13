use v5.14;
use warnings;

=head1 NAME

Attean::CodeIterator - Iterator implementation backed by a generator function

=head1 VERSION

This document describes Attean::CodeIterator version 0.001_01

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $iter = Attean::CodeIterator->new(
    generator => sub {
      state $value = 0;
      Attean::Literal->new(++$value)
    },
    item_type => 'Attean::API::Term',
  );
  
  say $iter->next->value; # 1
  say $iter->next->value; # 2
  say $iter->next->value; # 3

=head1 DESCRIPTION

The Attean::CodeIterator class represents a typed iterator.
It conforms to the L<Attean::API::Iterator> role.

The Attean::CodeIterator constructor requires two named arguments:

=over 4

=item generator

A code reference that when called will return either the iterator's next item,
or undef upon reaching the end of iteration.

=item item_type

A L<Type::Tiny> object representing the type of the items
that will be returned from the iterator.

=back

=head1 METHODS

=over 4

=cut

package Attean::CodeIterator 0.001 {
	use Moo;
	use Type::Tiny::Role;
	use Types::Standard qw(CodeRef ArrayRef);
	use namespace::clean;
	
	with 'Attean::API::Iterator';
	
	has generator => (is => 'ro', isa => CodeRef, required => 1);
	has buffer => (is => 'ro', isa => ArrayRef, init_arg => undef, default => sub { [] });

=item C<< next >>

Returns the iterator's next item, or undef upon reaching the end of iteration.

=cut

	sub next {
		my $self	= shift;
		my $buffer	= $self->buffer;
		if (scalar(@$buffer)) {
			return shift(@$buffer);
		}
		my @items	= $self->generator->();
		my $item	= shift(@items);
		return unless defined($item);
		if (scalar(@items)) {
			push(@$buffer, @items);
		}
		my $role	= $self->item_type;
		if (Role::Tiny->is_role($role)) {
			die "CodeIterator item is not a $role" unless ($item->does($role));
		}
		return $item;
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
