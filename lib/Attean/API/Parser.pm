use v5.14;
use warnings;

package Attean::API::Parser 0.001 {
	use Moose::Role;
	
	has 'handler' => (is => 'rw', isa => 'CodeRef', default => sub { sub {} });
	
	requires 'canonical_media_type'; # => (is => 'ro', isa => 'Str', init_arg => undef);
	requires 'media_types'; # => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef);
	requires 'handled_type'; # => (is => 'ro', isa => 'Moose::Meta::TypeConstraint', init_arg => undef);
}

package Attean::API::Parser::AbbreviatingParser 0.001 {
	use Moose::Role;
	use URI::NamespaceMap;
	
	with 'Attean::API::Parser';
	has 'base' 		=> (is => 'rw', isa => 'IRI', coerce => 1, predicate => 'has_base');
	has 'namespaces'	=> (is => 'ro', isa => 'Maybe[URI::NamespaceMap]');
}

package Attean::API::PushParser 0.001 {
	use Moose::Role;
	with 'Attean::API::Parser';

	requires 'parse_cb_from_io';		# parse_cb_from_io($io)
	requires 'parse_cb_from_bytes';		# parse_cb_from_bytes($data)
	# TODO: add default implementations for pullparser methods
	# TODO: add default implementations for atonceparser methods
}

package Attean::API::PullParser 0.001 {
	use Moose::Role;
	with 'Attean::API::Parser';
	
	requires 'parse_iter_from_io';		# $iter = parse_iter_from_io($io)
	requires 'parse_iter_from_bytes';	# $iter = parse_iter_from_bytes($data)
	
	sub parse_cb_from_io {
		my $self	= shift;
		my $io		= shift;
		my $handler	= $self->handler;
		my $iter	= $self->parse_iter_from_io($io);
		while (my $item = $iter->next) { $handler->( $item ) }
	}
	
	sub parse_cb_from_bytes {
		my $self	= shift;
		my $data	= shift;
		my $handler	= $self->handler;
		my $iter	= $self->parse_iter_from_bytes($data);
		while (defined(my $item = $iter->next)) { $handler->( $item ) }
	}
	
	sub parse_list_from_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= $self->parse_iter_from_io($io);
		my @list;
		while (defined(my $item = $iter->next)) { push(@list, $item); }
		return @list;
	}
	
	sub parse_list_from_bytes {
		my $self	= shift;
		my $data	= shift;
		my $iter	= $self->parse_iter_from_bytes($data);
		my @list;
		while (defined(my $item = $iter->next)) { push(@list, $item); }
		return @list;
	}
}

package Attean::API::AtOnceParser 0.001 {
	use Moose::Role;
	with 'Attean::API::Parser';
	
	requires 'parse_list_from_io';		# @list = parse_list_from_io($io)
	requires 'parse_list_from_bytes';	# @list = parse_list_from_bytes($data)
	
	# TODO: add default implementations for pushparser methods
	# TODO: add default implementations for pullparser methods
}

package Attean::API::TermParser 0.001 {
	# Parser returns objects that conform to Attean::API::Term
	use Moose::Role;
	with 'Attean::API::Parser';
}

package Attean::API::TripleParser 0.001 {
	# Parser returns objects that conform to Attean::API::Triple
	use Moose::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Triple');
		return $ITEM_TYPE;
	}
}

package Attean::API::QuadParser 0.001 {
	# Parser returns objects that conform to Attean::API::Quad
	use Moose::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Quad');
		return $ITEM_TYPE;
	}
}

package Attean::API::MixedStatementParser 0.001 {
	# Parser returns objects that conform to either Attean::API::Triple or Attean::API::Quad
	use Moose::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::TripleOrQuad');
		return $ITEM_TYPE;
	}
}

package Attean::API::ResultParser 0.001 {
	# Parser returns objects that conform to Attean::API::Result
	use Moose::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Result');
		return $ITEM_TYPE;
	}
}

1;
