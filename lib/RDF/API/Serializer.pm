use v5.14;
use warnings;

package RDF::API::Serializer 0.001 {
	use Moose::Role;
	
	has 'canonical_media_type' => (is => 'ro', isa => 'Str', init_arg => undef);
	has 'media_types' => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef);
	has 'handled_type' => (is => 'ro', isa => 'Moose::Meta::TypeConstraint', init_arg => undef);
	
	requires 'serialize_iter_to_io';		# serialize_iter_to_io($io, $iter)
	requires 'serialize_iter_to_bytes';		# $data = serialize_iter_to_bytes($iter)

	sub serialize_list_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= RDF::ListIterator->new( values => [@_], item_type => $self->handled_type );
		return $self->serialize_iter_to_io($io, $iter);
	}

	sub serialize_list_to_bytes {
		my $self	= shift;
		my $iter	= RDF::ListIterator->new( values => [@_], item_type => $self->handled_type );
		return $self->serialize_iter_to_bytes($iter);
	}
}

package RDF::API::AbbreviatingSerializer 0.001 {
	# Serializer that can make use of a base IRI and/or prefix IRI mappings
	use Moose::Role;
	with 'RDF::API::Serializer';

	has base		=> (is => 'ro', isa => 'IRI', predicate => 'has_base');
	has prefixes	=> (is => 'ro', isa => 'HashRef[IRI]', predicate => 'has_prefixes');
}

package RDF::API::TermSerializer 0.001 {
	# Serializes objects that conform to RDF::API::Term
	use Moose::Role;
	with 'RDF::API::Serializer';
}

package RDF::API::TripleSerializer 0.001 {
	# Serializes objects that conform to RDF::API::Triple
	use Moose::Role;
	with 'RDF::API::Serializer';
}

package RDF::API::QuadSerializer 0.001 {
	# Serializes objects that conform to RDF::API::Quad
	use Moose::Role;
	with 'RDF::API::Serializer';
	
	# TODO: add default implementations for serialize_model_to_io
	# TODO: add default implementations for serialize_model_to_bytes
}

package RDF::API::MixedStatementSerializer 0.001 {
	# Serializes objects that conform to either RDF::API::Triple or RDF::API::Quad
	use Moose::Role;
	with 'RDF::API::Serializer';
	
	# TODO: add default implementations for serialize_model_to_io_with_default_graph
	# TODO: add default implementations for serialize_model_to_bytes_with_default_graph
}

package RDF::API::ResultSerializer 0.001 {
	# Serializes objects that conform to RDF::API::Result
	use Moose::Role;
	with 'RDF::API::Serializer';
}

1;
