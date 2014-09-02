use v5.14;
use warnings;

=head1 NAME

Attean::API - Utility package for loading all Attean role packages.

=head1 VERSION

This document describes Attean::API version 0.001

=head1 SYNOPSIS

  use v5.14;
  use Attean;

=head1 DESCRIPTION

This is a utility package that will load all the Attean-related Moose roles
in the Attean::API:: namespace.

=cut

package Attean::API::BlankOrIRI 0.001 {
	use Moo::Role;
}

package Attean::API::TermOrVariable 0.001 {
	use Moo::Role;
}

package Attean::Mapper 0.001 {
	use Moo::Role;
	requires 'map'; # my $that = $object->map($this)
}

package Attean::API::Variable 0.001 {
	use Moo::Role;
	with 'Attean::API::TermOrVariable';
}

package Attean::API 0.001 {
	use Attean::API::Term;
	use Attean::API::Store;
	use Attean::API::Model;
	use Attean::API::Iterator;
	use Attean::API::Parser;
	use Attean::API::Serializer;

	use Attean::Variable;
	use Attean::Blank;
	use Attean::IRI;
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
