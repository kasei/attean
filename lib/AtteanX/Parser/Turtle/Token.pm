package AtteanX::Parser::Turtle::Token;

use v5.14;
use warnings;
use List::MoreUtils qw(zip);

use Moo;

has type => ( is => 'ro', );
has start_line => ( is => 'ro', );
has start_column => ( is => 'ro', );
has line => ( is => 'ro', );
has column => ( is => 'ro', );
has args => ( is => 'ro', );

=begin private

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

=end private
