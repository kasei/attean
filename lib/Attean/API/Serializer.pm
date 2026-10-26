use v5.14;
use warnings;

=head1 NAME

Attean::API::Serializer - Serializer role

=head1 VERSION

This document describes Attean::API::Serializer version 0.040

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

=item C<< file_extensions >>

Returns an ARRAY reference of file extensions commonly associated with the
media types supported by the serializer (and returned by C<< media_types >>).
File extensions should NOT include a leading dot.

=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the elements from the L<Attean::API::Iterator> C<< $iterator >> to
the L<IO::Handle> object C<< $fh >>.

=item C<< serialize_iter_to_bytes( $fh ) >>

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

package Attean::API::Serializer 0.040 {
	use Moo::Role;
	use Carp qw(confess);
	
	requires 'canonical_media_type'; # => (is => 'ro', isa => 'Str', init_arg => undef);
	requires 'media_types'; # => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef);
	requires 'handled_type'; # => (is => 'ro', isa => 'Type::Tiny', init_arg => undef);
	requires 'file_extensions'; # => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef);

	requires 'serialize_iter_to_io';		# serialize_iter_to_io($io, $iter)
	requires 'serialize_iter_to_bytes';		# $data = serialize_iter_to_bytes($iter)

	before 'serialize_iter_to_io' => sub {
		my $self	= shift;
		my $io		= shift || confess "No filehandle passed to serialize_iter_to_io";
		my $iter	= shift || confess "No iterator passed to serialize_iter_to_io";
	};

	before 'serialize_iter_to_bytes' => sub {
		my $self	= shift;
		my $iter	= shift || confess "No iterator passed to serialize_iter_to_bytes";
	};


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
	
	sub file_extensions { return [] }
}

package Attean::API::AbbreviatingSerializer 0.040 {
	# Serializer that can make use of a base IRI and/or prefix IRI mappings
	use Types::Standard qw(InstanceOf ConsumerOf Maybe Bool);
	use Types::Namespace qw( NamespaceMap );

	use Moo::Role;

	with 'Attean::API::Serializer';

	has base		=> (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], predicate => 'has_base');
	has namespaces	=> (is => 'ro', isa => Maybe[NamespaceMap], predicate => 'has_namespaces');
	has omit_base => (is => 'ro', isa => Bool, default => 0);
}

package Attean::API::AppendableSerializer 0.040 {
	# Serializer for a format that allows multiple serialization calls to be appended and remain syntactically valid
	use Moo::Role;
	with 'Attean::API::Serializer';
}

package Attean::API::TermSerializer 0.040 {
	use Moo::Role;
	with 'Attean::API::Serializer';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Term');
		return $ITEM_TYPE;
	}
}

package Attean::API::TripleSerializer 0.040 {
	use Moo::Role;
	with 'Attean::API::Serializer';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Triple');
		return $ITEM_TYPE;
	}
}

package Attean::API::QuadSerializer 0.040 {
	use Moo::Role;
	with 'Attean::API::Serializer';
	
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Quad');
		return $ITEM_TYPE;
	}
}

package Attean::API::MixedStatementSerializer 0.040 {
	use Moo::Role;
	with 'Attean::API::Serializer';
	
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::TripleOrQuad');
		return $ITEM_TYPE;
	}
}

package Attean::API::ResultSerializer 0.040 {
	use Moo::Role;
	with 'Attean::API::Serializer';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Result');
		return $ITEM_TYPE;
	}
	
	around 'serialize_list_to_io' => sub {
		my $orig	= shift;
		my $self	= shift;
		my $io		= shift;
		my @vars;
		if (scalar(@_)) {
			@vars	= $_[0]->variables;
		}
		
		my $iter	= Attean::ListIterator->new( values => [@_], item_type => $self->handled_type->role, variables => \@vars );
		return $self->serialize_iter_to_io($io, $iter);
	};
	
	around 'serialize_list_to_bytes' => sub {
		my $orig	= shift;
		my $self	= shift;
		my @vars;
		if (scalar(@_)) {
			@vars	= $_[0]->variables;
		}
		my $iter	= Attean::ListIterator->new( values => [@_], item_type => $self->handled_type->role, variables => \@vars );
		return $self->serialize_iter_to_bytes($iter);
	};
}

1;

__END__

=head1 EXAMPLES

=head2 Using L<Attean::TermMap> to use short blank node labels

L<Attean::TermMap> may be used to modify the data being supplied to a serializer.
This may be useful for producing more concise serializations:

  my $ser = Attean->get_serializer('Turtle')->new();
  my $iter = $model->get_quads()->as_triples();
  my $short_iter = $iter->map( $bindings_mapper );
  my $bytes = $ser->serialize_iter_to_bytes($short_iter);
  my $turtle = decode_utf8($bytes);

Instead of blank nodes using UUID-style labels like this:

  _:b05968a01cb5011f1952eeac53c3f578c <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .

the output will shorten labels:

  _:a <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .

=head2 Serializing with IRI prefixes

If a serializer supports shorthand syntax for IRIs using namespaces (any serializer
conforming to L<Attean::API::AbbreviatingSerializer>), it may be supplied to the
serializer constructor:

  my $map = URI::NamespaceMap->new( { foaf => iri('http://xmlns.com/foaf/0.1/') } );
  my $ser = Attean->get_serializer('Turtle')->new(namespaces => $map);
  my $iter = $model->get_quads()->as_triples();
  my $bytes = $ser->serialize_iter_to_bytes($iter);
  my $turtle = decode_utf8($bytes);

Any namespace supplied in the namespace map, and appearing in the data being serialized
will cause a PREFIX declaration to be used, and a shortened syntax to be used in the
serialized content. Instead of:

  _:b05968a01cb5011f1952eeac53c3f578c <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .

the output will be:

  _:b05968a01cb5011f1952eeac53c3f578c <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> foaf:Person .

Note that in this case, because only foaf was supplied in the namespace map, the
rdf:type predicate appears in full. Supplying a second definition in the namespace map
declaration would allow the serialization:

  _:b05968a01cb5011f1952eeac53c3f578c rdf:type foaf:Person .

Namespace maps may be constructed manually, or be populated by parsing RDF.
For example, supplying a L<URI::NamespaceMap> to a turtle parser will cause the
map to be populated with any prefixes declared in the parsed data:

  my $map = URI::NamespaceMap->new();
  my $parser = Attean->get_parser('Turtle')->new( namespaces => $map );

This works for with any L<Attean::API::AbbreviatingParser>, which includes data
formats such as Turtle, TriG, and RDF/XML, as well as the SPARQL parser.

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2026 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
