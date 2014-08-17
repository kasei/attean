use v5.14;
use warnings;

package Attean::API::Serializer 0.001 {
	use Moose::Role;
	
	requires 'canonical_media_type'; # => (is => 'ro', isa => 'Str', init_arg => undef);
	requires 'media_types'; # => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef);
	requires 'handled_type'; # => (is => 'ro', isa => 'Moose::Meta::TypeConstraint', init_arg => undef);
	
	requires 'serialize_iter_to_io';		# serialize_iter_to_io($io, $iter)
	requires 'serialize_iter_to_bytes';		# $data = serialize_iter_to_bytes($iter)

	sub serialize_list_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= Attean::ListIterator->new( values => [@_], item_type => $self->handled_type );
		return $self->serialize_iter_to_io($io, $iter);
	}

	sub serialize_list_to_bytes {
		my $self	= shift;
		my $iter	= Attean::ListIterator->new( values => [@_], item_type => $self->handled_type );
		return $self->serialize_iter_to_bytes($iter);
	}
}

package Attean::API::AbbreviatingSerializer 0.001 {
	# Serializer that can make use of a base IRI and/or prefix IRI mappings
	use Moose::Role;
	with 'Attean::API::Serializer';

	has base		=> (is => 'ro', isa => 'Attean::API::IRI', predicate => 'has_base');
	has namespaces	=> (is => 'ro', isa => 'Maybe[URI::NamespaceMap]', predicate => 'has_namespaces');
}

package Attean::API::TermSerializer 0.001 {
	use Moose::Role;
	with 'Attean::API::Serializer';
	sub handled_type {
		state $ITEM_TYPE = Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Term');
		return $ITEM_TYPE;
	}
}

package Attean::API::TripleSerializer 0.001 {
	use Moose::Role;
	with 'Attean::API::Serializer';
	sub handled_type {
		state $ITEM_TYPE = Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Triple');
		return $ITEM_TYPE;
	}
}

package Attean::API::QuadSerializer 0.001 {
	use Moose::Role;
	with 'Attean::API::Serializer';
	
	# TODO: add default implementations for serialize_model_to_io
	# TODO: add default implementations for serialize_model_to_bytes
	sub handled_type {
		state $ITEM_TYPE = Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Quad');
		return $ITEM_TYPE;
	}
}

package Attean::API::MixedStatementSerializer 0.001 {
	use Moose::Role;
	with 'Attean::API::Serializer';
	
	# TODO: add default implementations for serialize_model_to_io_with_default_graph
	# TODO: add default implementations for serialize_model_to_bytes_with_default_graph
	sub handled_type {
		state $ITEM_TYPE = Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::TripleOrQuad');
		return $ITEM_TYPE;
	}
}

package Attean::API::ResultSerializer 0.001 {
	use Moose::Role;
	with 'Attean::API::Serializer';
	sub handled_type {
		state $ITEM_TYPE = Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Result');
		return $ITEM_TYPE;
	}
}

1;
