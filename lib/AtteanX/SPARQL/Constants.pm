# AtteanX::SPARQL::Constants
# -----------------------------------------------------------------------------

=head1 NAME

AtteanX::SPARQL::Constants - Constant definitions for use in parsing and serializing SPARQL

=head1 VERSION

This document describes AtteanX::SPARQL::Constants version 0.026

=head1 SYNOPSIS

 use AtteanX::SPARQL::Constants;

=head1 METHODS

=over 4

=cut

package AtteanX::SPARQL::Constants 0.026 {
	use v5.14;
	use warnings;
	use AtteanX::Parser::Turtle::Constants;
	
	our @EXPORT;
	our @LOCAL_TYPES;
	BEGIN {
		@LOCAL_TYPES	= qw(
			ANDAND
			ANON
			BANG
			GE
			GT
			HAT
			KEYWORD
			LE
			LT
			MINUS
			NIL
			NOTEQUALS
			OR
			OROR
			PLUS
			QUESTION
			SLASH
			STAR
			VAR
			decrypt_constant
		);
		@EXPORT = (@AtteanX::Parser::Turtle::Constants::EXPORT, @LOCAL_TYPES);
	};
	use base 'Exporter';

	{
		my %mapping;
		my %reverse;
		BEGIN {
			my $cx	= scalar(@AtteanX::Parser::Turtle::Constants::EXPORT) - 1;
			foreach my $name (grep { $_ ne 'decrypt_constant' } @LOCAL_TYPES) {
				my $value	= ++$cx;
				$reverse{ $value }	= $name;
				$mapping{ $name }	= $value;
			}
		}
		use constant +{ %mapping };

=item C<< decrypt_constant ( $type ) >>

Returns the token name for the given token type.

=cut

		no warnings 'redefine';
		sub decrypt_constant {
			my $num	= +shift;
			if (exists $reverse{$num}) {
				return $reverse{$num};
			} else {
				return AtteanX::Parser::Turtle::Constants::decrypt_constant($num);
			}
		}
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/perlrdf/issues>.

=head1 AUTHOR

Toby Inkster C<< <tobyink@cpan.org> >>

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2019 Toby Inkster. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
