use v5.14;
use warnings;

=head1 NAME

Attean::Quad - RDF Quads

=head1 VERSION

This document describes Attean::Quad version 0.002

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $quad = Attean::Quad->new( $s, $p, $o, $g );

=head1 DESCRIPTION

The Attean::Quad class represents an RDF quad.
It conforms to the L<Attean::API::Quad|Attean::API::Binding> role.

=head1 ROLES

This role consumes L<Attean::API::Quad>.

=head1 METHODS

=over 4

=item C<< subject >>

=item C<< predicate >>

=item C<< object >>

=item C<< graph >>

=back

=cut

package Attean::QuadPattern 0.001 {
	use Moo;
	use Attean::API;
	
	has 'subject'	=> (is => 'ro', required => 1);
	has 'predicate'	=> (is => 'ro', required => 1);
	has 'object'	=> (is => 'ro', required => 1);
	has 'graph'		=> (is => 'ro', required => 1);
	
	with 'Attean::API::QuadPattern';

	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 4) {
			my %args;
			@args{ $class->variables }	= @_;
			return $class->$orig(%args);
		}
		return $class->$orig(@_);
	};
}

package Attean::Quad 0.001 {
	use Moo;
	use Attean::API::Binding;
	
	has 'subject'	=> (is => 'ro', does => 'Attean::API::BlankOrIRI', required => 1);
	has 'predicate'	=> (is => 'ro', does => 'Attean::API::IRI', required => 1);
	has 'object'	=> (is => 'ro', does => 'Attean::API::Term', required => 1);
	has 'graph'		=> (is => 'ro', does => 'Attean::API::BlankOrIRI', required => 1);
	
	with 'Attean::API::Quad';
	
	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 4) {
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
