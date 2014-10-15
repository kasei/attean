use v5.14;
use warnings;

=head1 NAME

Attean::API::Serializer - Serializer role

=head1 VERSION

This document describes Attean::API::Serializer version 0.002

=head1 DESCRIPTION

The Attean::API::Serializer role defines a common API for all serializers
of typed objects to data (either a byte string or printed to a filehandle).

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Serializer> role:

=over 4

=item C<< canonical_media_type >>

Returns the canonical media type string for the format of this serializer.

=item C<< media_types >>

Returns an ARRAY reference of media type strings that also identify the format
produced by this serializer.

=item C<< handled_type >>

Returns a L<Type::Tiny> object representing the type of items that are consumed
during serialization.

=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the elements from the L<Attean::API::Iterator> C<< $iterator >> to
the L<IO::Handle> object C<< $fh >>.

=item C<< serialize_iter_to_bytes( $fh, $iterator ) >>

Serializes the elements from the L<Attean::API::Iterator> C<< $iterator >>
and returns the serialization as a UTF-8 encoded byte string.

=back

=head1 METHODS

This role provides default implementations of the following methods:

=over 4

=item C<< serialize_list_to_io( $fh, @elements ) >>

Serializes the objects in C<< @elements >> to the L<IO::Handle> object
C<< $fh >>.

=item C<< serialize_list_to_bytes( @elements ) >>

Serializes the objects in C<< @elements >> and returns the serialization as a
UTF-8 encoded byte string.

=back

=cut

use Type::Tiny;

package Attean::API::Serializer 0.001 {
	use Moo::Role;
	
	requires 'canonical_media_type'; # => (is => 'ro', isa => 'Str', init_arg => undef);
	requires 'media_types'; # => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef);
	requires 'handled_type'; # => (is => 'ro', isa => 'Type::Tiny', init_arg => undef);
	
	requires 'serialize_iter_to_io';		# serialize_iter_to_io($io, $iter)
	requires 'serialize_iter_to_bytes';		# $data = serialize_iter_to_bytes($iter)

	sub serialize_list_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= Attean::ListIterator->new( values => [@_], item_type => $self->handled_type->role );
		return $self->serialize_iter_to_io($io, $iter);
	}

	sub serialize_list_to_bytes {
		my $self	= shift;
		my $iter	= Attean::ListIterator->new( values => [@_], item_type => $self->handled_type->role );
		return $self->serialize_iter_to_bytes($iter);
	}
}

package Attean::API::AbbreviatingSerializer 0.001 {
	# Serializer that can make use of a base IRI and/or prefix IRI mappings
	use Moo::Role;
	use Types::Standard qw(InstanceOf ConsumerOf Maybe);
	use namespace::clean;

	with 'Attean::API::Serializer';

	has base		=> (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], predicate => 'has_base');
	has namespaces	=> (is => 'ro', isa => Maybe[InstanceOf['URI::NamespaceMap']], predicate => 'has_namespaces');
}

package Attean::API::AppendableSerializer 0.001 {
	# Serializer for a format that allows multiple serialization calls to be appended and remain syntactically valid
	use Moo::Role;
	with 'Attean::API::Serializer';
}

package Attean::API::TermSerializer 0.001 {
	use Moo::Role;
	with 'Attean::API::Serializer';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Term');
		return $ITEM_TYPE;
	}
}

package Attean::API::TripleSerializer 0.001 {
	use Moo::Role;
	with 'Attean::API::Serializer';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Triple');
		return $ITEM_TYPE;
	}
}

package Attean::API::QuadSerializer 0.001 {
	use Moo::Role;
	with 'Attean::API::Serializer';
	
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Quad');
		return $ITEM_TYPE;
	}
}

package Attean::API::MixedStatementSerializer 0.001 {
	use Moo::Role;
	with 'Attean::API::Serializer';
	
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::TripleOrQuad');
		return $ITEM_TYPE;
	}
}

package Attean::API::ResultSerializer 0.001 {
	use Moo::Role;
	with 'Attean::API::Serializer';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Result');
		return $ITEM_TYPE;
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
