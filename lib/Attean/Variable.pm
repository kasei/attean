use v5.14;
use warnings;

=head1 NAME

Attean::Variable - Pattern matching variables

=head1 VERSION

This document describes Attean::Variable version 0.000

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $term = Attean::Variable->new('name');
  $term->ntriples_string; # ?name

=head1 DESCRIPTION

The Attean::Variable class represents variables for use in pattern matching.
It conforms to the L<Attean::API::TermOrVariable|Attean::API> role.

=cut

package Attean::Variable 0.001 {
	use Moo;
	use Types::Standard qw(Str);
	use Data::UUID;
	use namespace::clean;
	
	has 'value' => (is => 'ro', isa => Str, required => 1);
	has 'ntriples_string'	=> (is => 'ro', isa => Str, lazy => 1, builder => '_ntriples_string');

	with 'Attean::API::Variable', 'Attean::API::TermOrVariable';
	
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
		return '?' . $self->value;
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
