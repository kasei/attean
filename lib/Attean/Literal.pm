use v5.14;
use warnings;

=head1 NAME

Attean::Literal - RDF Literals

=head1 VERSION

This document describes Attean::Literal version 0.001

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $langterm = Attean::Literal->new(value => 'foo', language => 'en-US');
  $langterm->ntriples_string; # "foo"@en-US

  my $typeterm = Attean::Literal->new(value => '123', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
  $langterm->ntriples_string; # "123"^^<http://www.w3.org/2001/XMLSchema#integer>

=head1 DESCRIPTION

The Attean::Literal class represents RDF literals.
It conforms to the L<Attean::API::Literal|Attean::API::Term> role.

=cut

package Attean::Literal 0.001 {
	use Attean::API::Term;
	use Moose;
	use IRI;
	
	my $XSD_STRING	= IRI->new(value => 'http://www.w3.org/2001/XMLSchema#string');
	has 'value' => (is => 'ro', isa => 'Str', required => 1);
	has 'language'			=> (is => 'ro', isa => 'Maybe[Str]', predicate => 'has_language');
	has 'datatype'			=> (is => 'ro', does => 'Attean::IRI', required => 1, coerce => 1, default => sub { $XSD_STRING });
	has 'ntriples_string'	=> (is => 'ro', isa => 'Str', lazy => 1, builder => '_ntriples_string');

	with 'Attean::API::Literal';

	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 1) {
			my $dt	= IRI->new('http://www.w3.org/2001/XMLSchema#string');
			return $class->$orig(value => shift, datatype => $dt);
		}
		
		my %args	= @_;
		return $class->$orig(%args);
	};
	
	sub BUILD {
		my $self	= shift;
		die unless ($self->has_language or length($self->datatype->as_string));
	}
	
	around 'datatype'	=> sub {
		my $orig	= shift;
		my $self	= shift;
		if ($self->has_language) {
			return Attean::IRI->new(value => 'http://www.w3.org/2001/XMLSchema#string');
		} else {
			return $self->$orig(@_);
		}
	};
	
	sub _ntriples_string {
		my $self	= shift;
		my $value	= $self->value;
		$value		=~ s/\\/\\\\/g;
		$value		=~ s/\n/\\n/g;
		$value		=~ s/\r/\\r/g;
		$value		=~ s/"/\\"/g;
		if ($self->has_language) {
			return sprintf('"%s"@%s', $value, $self->language);
		} else {
			my $dt	= $self->datatype->as_string;
			if ($dt eq 'http://www.w3.org/2001/XMLSchema#string') {
				return sprintf('"%s"', $self->value);
			} else {
				return sprintf('"%s"^^<%s>', $value, $dt);
			}
		}
	}

	no Moose;
	__PACKAGE__->meta->make_immutable;
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
