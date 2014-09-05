use v5.14;
use warnings;

=head1 NAME

Attean::API::Term - RDF Terms

=head1 VERSION

This document describes Attean::API::Term version 0.001

=head1 DESCRIPTION

The Attean::API::Term role defines a common API for all RDF terms.

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Term> role:

=over 4

=item C<< value >>

=item C<< ntriples_string >>

=back

=head1 METHODS

The L<Attean::API::Term> role role provides default implementations of the
following methods:

=over 4

=item C<< as_string >>

=cut

package Attean::API::Term 0.001 {
	use Moo::Role;
	
	with 'Attean::API::TermOrVariable';
	
	requires 'value'; # => (is => 'ro', isa => 'Str', required => 1);
	requires 'ntriples_string';
	sub as_string {
		shift->ntriples_string();
	}
	sub ebv { return 0; }
	
	sub __ntriples_string {
		my $self	= shift;
		my $value	= $self->value;
		if ($value =~ m/^[\x20\x23-\x5a\x5d-\x7e]*$/o) {
			return $value;
		}
		
		my @chars	= split(//, $value);
		my $string	= '';
		while (scalar(@chars)) {
			my $c	= shift(@chars);
			my $o	= ord($c);
			if ($o < 0x8) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o == 0x9) {
				$string	.= "\\t";
			} elsif ($o == 0xA) {
				$string	.= "\\n";
			} elsif ($o < 0xC) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o == 0xD) {
				$string	.= "\\r";
			} elsif ($o < 0x1F) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o < 0x21) {
				$string	.= $c;
			} elsif ($o == 0x22) {
				$string	.= "\"";
			} elsif ($o < 0x5B) {
				$string	.= $c;
			} elsif ($o == 0x5C) {
				$string	.= "\\";
			} elsif ($o < 0x7E) {
				$string	.= $c;
			} elsif ($o < 0xFFFF) {
				$string	.= sprintf("\\u%04X", $o);
			} else {
				$string	.= sprintf("\\U%08X", $o);
			}
		}
		return $string;
	}
}

package Attean::API::Literal 0.001 {
	use Moo::Role;
	use IRI;
	
	with 'Attean::API::Term';
	
	requires 'language'; # => (is => 'ro', isa => 'Maybe[Str]', predicate => 'has_language');
	requires 'datatype'; # => (is => 'ro', isa => 'Attean::API::IRI', required => 1, coerce => 1, default => sub { IRI->new(value => 'http://www.w3.org/2001/XMLSchema#string') });
	sub ebv {
		my $self	= shift;
		my $value	= $self->value;
		my $dt		= $self->datatype->value;
		if ($dt eq 'http://www.w3.org/2001/XMLSchema#boolean') {
			return ($value eq 'true' or $value eq '1');
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#integer') {
			return ($value != 0);
# 			# TODO: handle other numeric types
		} else {
			return (length($value) > 0);
		}
	}
	
	if ($ENV{ATTEAN_TYPECHECK}) {
		use Types::Standard qw(Maybe Str ConsumerOf);
		my %map	= (
			language	=> Maybe[Str],
			datatype	=> ConsumerOf['Attean::API::IRI'],
		);
		foreach my $method (keys %map) {
			my $type	= $map{$method};
			around $method => sub {
				my $orig	= shift;
				my $self	= shift;
				my $class	= ref($self);
				my $value	= $self->$orig(@_);
				my $err		= $type->validate($value);
				if ($err) {
					my $name	= $type->display_name;
					die "${class}'s $method failed conformance check for $name: $value";
				}
				return $value;
			};
		}
	}
	
	sub _ntriples_string {
		my $self	= shift;
		return sprintf('"%s"', $self->__ntriples_string);
	}
}

package Attean::API::Blank 0.001 {
	use Moo::Role;
	
	sub ebv { return 0; }
	with 'Attean::API::Term';
	with 'Attean::API::BlankOrIRI';
}

package Attean::API::IRI 0.001 {
	use Moo::Role;
	use IRI;
	
	sub ebv { return 0; }
	with 'Attean::API::Term';
	with 'Attean::API::BlankOrIRI';
	
	sub _ntriples_string {
		my $self	= shift;
		return sprintf('<%s>', $self->__ntriples_string);
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
