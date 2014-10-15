use v5.14;
use warnings;

=head1 NAME

AtteanX::Parser::Turtle::Token - Token objects used for parsing of Turtle

=head1 VERSION

This document describes AtteanX::Parser::Turtle::Token version 0.002

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $term = Attean::Blank->new('b1');
  $term->ntriples_string; # _:b1

=head1 DESCRIPTION

The AtteanX::Parser::Turtle::Token class represents tokens produced and used
during parsing of Turtle.

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::Turtle::Token;

use Moo;
use Types::Standard qw(ArrayRef Str);
use List::MoreUtils qw(zip);
use namespace::clean;

has type => ( is => 'ro', );
has start_line => ( is => 'ro', );
has start_column => ( is => 'ro', );
has line => ( is => 'ro', );
has column => ( is => 'ro', );
has args => ( is => 'ro', isa => ArrayRef[Str]);

=item C<< value >>

Returns the token value.

=cut

sub value {
	my $self	= shift;
	my $args	= $self->args;
	return $args->[0];
}

=item C<< fast_constructor ( $type, $start_line, $start_col, $line, $col, \@args ) >>

Returns a new token object.

=cut

my @KEYS	= qw(type start_line start_column line column args);
sub fast_constructor {
	my $class = shift;
	return $class->new(
		zip @KEYS, @_
	);
}

__PACKAGE__->meta->make_immutable;

1;

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
