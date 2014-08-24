use v5.14;
use warnings;

=head1 NAME

Attean::Result - SPARQL Result

=head1 VERSION

This document describes Attean::Result version 0.001

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $result = Attean::Result->new( { name => $literal, homepage => $iri } );
  my @vars = $result->variables; # ('name', 'iri')
  my $term = $result->value('name'); # $term == $literal

=head1 DESCRIPTION

The Attean::Result class represents a SPARQL result (a set of bindings from
variable names to L<term|Attean::API::Term>s).
It conforms to the L<Attean::API::Result|Attean::API::Binding> role.

=head1 METHODS

=over 4

=cut

package Attean::Result 0.001 {
	use Moo;
	use MooX::Types::MooseLike::Base qw(HashRef);
	use Attean::API::Binding;
	
	with 'Attean::API::Result';
	has 'bindings' => (is => 'ro', isa => HashRef); # TODO: HashRef[Attean::API::Term]
	
=item C<< value( $name ) >>

Returns the term object bound to the C<< $name >>d variable, or undef if the
name does not map to a term.

=cut
	sub value {
		my $self	= shift;
		my $k		= shift;
		return $self->bindings->{$k};
	}
	
=item C<< variables >>

Returns a list of the variable names that are bound to terms in this result
object.

=cut

	sub variables {
		my $self	= shift;
		return keys %{ $self->bindings };
	}

	no Moose;
	__PACKAGE__->meta->make_immutable;
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
