use v5.14;
use warnings;

=head1 NAME

AtteanX::SPARQL::Token - Token objects used for parsing and serializing SPARQL

=head1 VERSION

This document describes AtteanX::SPARQL::Token version 0.030

=head1 SYNOPSIS

  use v5.14;
  use Attean;

=head1 DESCRIPTION

The AtteanX::SPARQL::Token class represents tokens produced and used
during parsing and serializing of SPARQL.

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

package AtteanX::SPARQL::Token 0.030;

use Moo;
use Types::Standard qw(ArrayRef Str);
use List::MoreUtils qw(zip);
use Sub::Util qw(set_subname);
use AtteanX::SPARQL::Constants;
use namespace::clean;

has type => ( is => 'ro', );
has start_line => ( is => 'ro', );
has start_column => ( is => 'ro', );
has line => ( is => 'ro', );
has column => ( is => 'ro', );
has args => ( is => 'ro', isa => ArrayRef[Str]);

extends 'AtteanX::Parser::Turtle::Token';

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


		lbrace		=> [LBRACE, '{'],
		rbrace		=> [RBRACE, '}'],
		op_andand	=> [ANDAND, '&&'],
		anon		=> [ANON, '[]'],
		op_bang		=> [BANG, '!'],
		op_ge		=> [GE, '>='],
		op_gt		=> [GT, '>'],
		path_hat	=> [HAT, '^'],
		op_le		=> [LE, '<='],
		op_lt		=> [LT, '<'],
		minus		=> [MINUS, '-'],
		nil			=> [NIL, '()'],
		op_ne		=> [NOTEQUALS, '!='],
		path_or		=> [OR, '|'],
		op_oror		=> [OROR, '||'],
		op_plus		=> [PLUS, '+'],
		question	=> [QUESTION, '?'],
		slash		=> [SLASH, '/'],
		star		=> [STAR, '*'],
	);
	for my $name (keys %tokens) {
		my ($type, $value)	= @{ $tokens{ $name } };
		my $code	= sub {
			my $class	= shift;
			return $class->fast_constructor($type, -1, -1, -1, -1, [$value]);
		};
		Sub::Install::install_sub({
			code	=> set_subname($name, $code),
			as		=> $name
		});
	}
}

=item C<< keyword( $kw ) >>

Returns a new L<AtteanX::SPARQL::Token> object with the C<KEYWORD> type and
C<$kw> value.

=cut

sub keyword {
	my $class	= shift;
	my $kw		= shift;
	return $class->fast_constructor(KEYWORD, -1, -1, -1, -1, [uc($kw)]);
}

=item C<< integer( $value ) >>

Returns a new L<AtteanX::SPARQL::Token> object with the C<INTEGER> type and
the given C<$value>.

=cut

sub integer {
	my $class	= shift;
	my $value	= shift;
	return $class->fast_constructor(INTEGER, -1, -1, -1, -1, [+$value] );
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

Copyright (c) 2014--2020 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
