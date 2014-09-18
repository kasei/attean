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
	use Moo;
	use Types::Standard qw(Str Maybe InstanceOf);
	use Attean::API::Term;
	use IRI;
	use Sub::Install;
	use Sub::Name;
	use Scalar::Util qw(blessed);
	use namespace::clean;
	
	my $XSD_STRING	= IRI->new(value => 'http://www.w3.org/2001/XMLSchema#string');
	has 'value'				=> (is => 'ro', isa => Str, required => 1);
	has 'language'			=> (is => 'ro', isa => Maybe[Str], predicate => 'has_language');
	has 'datatype'			=> (
		is => 'ro',
		isa => InstanceOf['Attean::IRI'],
		required => 1,
		coerce => sub { blessed($_[0]) ? Attean::IRI->new($_[0]->as_string) : Attean::IRI->new($_[0]) },
		default => sub { $XSD_STRING }
	);
	has 'ntriples_string'	=> (is => 'ro', isa => Str, lazy => 1, builder => '_ntriples_string');

	with 'Attean::API::Literal';

	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		return $class->$orig(@_) if (scalar(@_) == 1 and ref($_[0]) eq "HASH");
		if (scalar(@_) == 1) {
			my $dt	= IRI->new('http://www.w3.org/2001/XMLSchema#string');
			return $class->$orig(value => shift, datatype => $dt);
		}
		return $class->$orig(@_);
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
				return sprintf('"%s"', $value);
			} else {
				return sprintf('"%s"^^<%s>', $value, $dt);
			}
		}
	}

	sub true {
		state $v	= Attean::Literal->new( value => 'true', datatype => 'http://www.w3.org/2001/XMLSchema#boolean' );
		return $v;
	}
	
	sub false {
		state $v	= Attean::Literal->new( value => 'false', datatype => 'http://www.w3.org/2001/XMLSchema#boolean' );
		return $v;
	}
	
	{
		for my $method (qw(integer decimal float double)) {
			my $code	= sub {
				my $class	= shift;
				return $class->new( value => shift, datatype => "http://www.w3.org/2001/XMLSchema#$method" );
			};
			Sub::Install::install_sub({
				code	=> subname("${method}", $code),
				as		=> "${method}"
			});
		}
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
