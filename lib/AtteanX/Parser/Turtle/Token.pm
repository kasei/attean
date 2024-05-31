use v5.14;
use warnings;

=head1 NAME

AtteanX::Parser::Turtle::Token - Token objects used for parsing of Turtle

=head1 VERSION

This document describes AtteanX::Parser::Turtle::Token version 0.033

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $term = Attean::Blank->new('b1');
  $term->ntriples_string; # _:b1

=head1 DESCRIPTION

The AtteanX::Parser::Turtle::Token class represents tokens produced and used
during parsing of Turtle.

=head1 ATTRIBUTES

=over 4

=item C<< type >>

An integer indicating the token type, defined in L<AtteanX::Parser::Turtle::Constants>

=item C<< start_line >>

The line number in the source text that this token begins on.

=item C<< start_column >>

The column number in the source text that this token begins on.

=item C<< line >>

The line number in the source text that this token ends on.

=item C<< column >>

The column number in the source text that this token ends on.

=item C<< args >>

An array of values associated with the token (e.g. the integer value of an INT token).

=back

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::Turtle::Token;

use Moo;
use Types::Standard qw(ArrayRef Str);
use List::MoreUtils qw(zip);
use Sub::Util qw(set_subname);
use AtteanX::Parser::Turtle::Constants;
use Sub::Install;
use namespace::clean;

our $VERSION	= 0.033;

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

my %token_strings;
{
	my %tokens	= (
		a			=> [A, 'a'],
		prefix		=> [PREFIX, '@prefix'],
		base		=> [BASE, '@base'],
		lparen		=> [LPAREN, '('],
		rparen		=> [RPAREN, ')'],
		lbracket	=> [LBRACKET, '['],
		rbracket	=> [RBRACKET, ']'],
		dot			=> [DOT, '.'],
		comma		=> [COMMA, ','],
		semicolon	=> [SEMICOLON, ';'],
		hathat		=> [HATHAT, '^^'],
	);
	for my $name (keys %tokens) {
		my ($type, $value)	= @{ $tokens{ $name } };
		my $code	= sub {
			my $class	= shift;
			my $sl		= shift // -1;
			my $sc		= shift // -1;
			my $l		= shift // $sl;
			my $c		= shift // $sc;
			if ($sl > $l) { die '$start_line cannot be greater than $line in AtteanX::Parser::Turtle::Token constructor' }
			if ($sc > $c) { die '$start_line cannot be greater than $line in AtteanX::Parser::Turtle::Token constructor' }
			return $class->fast_constructor($type, $sl, $sc, $l, $c, [$value]);
		};
		Sub::Install::install_sub({
			code	=> set_subname($name, $code),
			as		=> $name
		});
		$token_strings{$type}	= $value;
	}
	
}

=item C<< token_as_string() >>

Returns a string version of the token (without escaping).

=cut

sub token_as_string {
	my $self	= shift;
	my $type	= $self->type;
	my @args	= @{ $self->args };
	my $value	= $args[0];
	if (defined(my $v = $token_strings{$type})) {
		return $v;
	} elsif ($type == STRING1D) {
		return qq["$value"];
	} elsif ($type == STRING1S) {
		return qq["$value"];
	} elsif ($type == STRING3D) {
		return qq["""$value"""];
	} elsif ($type == STRING3S) {
		return qq['''$value'''];
	} elsif ($type == IRI) {
		return qq[<$value>];
	} elsif ($type == BNODE) {
		return qq[_:$value]
	} elsif ($type == LANG) {
		return qq[\@$value]
	} else {
		join(', ', @args);
	}
}

=item C<< is_string() >>

Returns true if the token is one of the quoted string types (STRING1D, STRING3D,
STRING1S, or STRING3S), false otherwise.

=cut

sub is_string {
	my $self	= shift;
	my $type	= $self->type;
	return 1 if ($type == STRING1D);
	return 1 if ($type == STRING1S);
	return 1 if ($type == STRING3D);
	return 1 if ($type == STRING3S);
}

=item C<< as_string >>

Returns a string description of the token including the token type and any
associated values.

=cut

sub as_string {
	my $self	= shift;
	my $type	= decrypt_constant($self->type);
	my @args	= @{ $self->args };
	if (scalar(@args)) {
		return "$type(" . join(', ', @args) . ")";
	} else {
		return $type;
	}
}

__PACKAGE__->meta->make_immutable;

1;

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2022 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
