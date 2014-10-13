use v5.14;
use warnings;

=head1 NAME

Attean::Blank - RDF blank nodes

=head1 VERSION

This document describes Attean::Blank version 0.001_01

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $term = Attean::Blank->new('b1');
  $term->ntriples_string; # _:b1

=head1 DESCRIPTION

The Attean::Blank class represents RDF blank nodes.
It conforms to the L<Attean::API::Blank|Attean::API::Term> role.

=head1 ROLES

This role consumes L<Attean::API::Blank>, which provides the following methods:

=over 4

=item C<< value >>

=back

=cut

package Attean::Blank 0.001 {
	use Moo;
	use Types::Standard qw(Str);
	use Data::UUID;
	use namespace::clean;
	
	has 'value' => (is => 'ro', isa => Str, required => 1);
	has 'ntriples_string'	=> (is => 'ro', isa => Str, lazy => 1, builder => '_ntriples_string');
	
	with 'Attean::API::Blank';

	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		
		if (scalar(@_) == 0) {
			my $uuid	= Data::UUID->new->create_hex;
			return $class->$orig(value => $uuid);
		} elsif (scalar(@_) == 1) {
			return $class->$orig(value => shift);
		}
		return $class->$orig(@_);
	};
	
	sub _ntriples_string {
		my $self	= shift;
		return '_:' . $self->value;
	}
}

1;

__END__

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
