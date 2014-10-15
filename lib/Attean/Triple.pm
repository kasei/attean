use v5.14;
use warnings;

=head1 NAME

Attean::Triple - RDF Triples

=head1 VERSION

This document describes Attean::Triple version 0.002

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $triple = Attean::Triple->new( $s, $p, $o );

=head1 DESCRIPTION

The Attean::Triple class represents an RDF triple.
It conforms to the L<Attean::API::Triple|Attean::API::Binding> role.

=head1 ROLES

This role consumes L<Attean::API::Triple>.

=head1 METHODS

=over 4

=item C<< subject >>

=item C<< predicate >>

=item C<< object >>

=back

=cut

package Attean::TriplePattern 0.001 {
	use Moo;
	use Attean::API::Binding;
	
	has 'subject'	=> (is => 'ro', required => 1);
	has 'predicate'	=> (is => 'ro', required => 1);
	has 'object'	=> (is => 'ro', required => 1);
	
	with 'Attean::API::TriplePattern';
	
	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 3) {
			my %args;
			@args{ $class->variables }	= @_;
			return $class->$orig(%args);
		}
		return $class->$orig(@_);
	};
}

package Attean::Triple 0.001 {
	use Moo;
	use Attean::API::Binding;
	
	has 'subject'	=> (is => 'ro', does => 'Attean::API::BlankOrIRI', required => 1);
	has 'predicate'	=> (is => 'ro', does => 'Attean::API::IRI', required => 1);
	has 'object'	=> (is => 'ro', does => 'Attean::API::Term', required => 1);
	
	with 'Attean::API::Triple';
	
	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 3) {
			my %args;
			@args{ $class->variables }	= @_;
			return $class->$orig(%args);
		}
		return $class->$orig(@_);
	};
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
