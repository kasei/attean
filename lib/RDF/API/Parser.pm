use v5.14;
use warnings;

package RDF::API::Parser 0.001 {
	use Moose::Role;
	
	has 'canonical_media_type' => (is => 'ro', isa => 'Str', init_arg => undef);
	has 'media_types' => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef);
}

package RDF::API::PushParser 0.001 {
	use Moose::Role;
	with 'RDF::API::Parser';

	requires 'parse_cb_from_io';		# parse_cb_from_io($io, $base, \&handler)
	requires 'parse_cb_from_bytes';		# parse_cb_from_bytes($data, $base, \&handler)
	# TODO: add default implementations for pullparser methods
	# TODO: add default implementations for atonceparser methods
}

package RDF::API::PullParser 0.001 {
	use Moose::Role;
	with 'RDF::API::Parser';
	
	requires 'parse_iter_from_io';		# $iter = parse_iter_from_io($io, $base)
	requires 'parse_iter_from_bytes';	# $iter = parse_iter_from_bytes($data, $base)
	
	# TODO: add default implementations for pushparser methods
	# TODO: add default implementations for atonceparser methods
}

package RDF::API::AtOnceParser 0.001 {
	use Moose::Role;
	with 'RDF::API::Parser';
	
	requires 'parse_list_from_io';		# @list = parse_list_from_io($io, $base)
	requires 'parse_list_from_bytes';	# @list = parse_list_from_bytes($data, $base)
	
	# TODO: add default implementations for pushparser methods
	# TODO: add default implementations for pullparser methods
}

package RDF::API::TermParser 0.001 {
	# Parser returns objects that conform to RDF::API::Term
	use Moose::Role;
	with 'RDF::API::Parser';
}

package RDF::API::TripleParser 0.001 {
	# Parser returns objects that conform to RDF::API::Triple
	use Moose::Role;
	with 'RDF::API::Parser';
}

package RDF::API::QuadParser 0.001 {
	# Parser returns objects that conform to RDF::API::Quad
	use Moose::Role;
	with 'RDF::API::Parser';
}

package RDF::API::MixedStatementParser 0.001 {
	# Parser returns objects that conform to either RDF::API::Triple or RDF::API::Quad
	use Moose::Role;
	with 'RDF::API::Parser';
}

package RDF::API::ResultParser 0.001 {
	# Parser returns objects that conform to RDF::API::Result
	use Moose::Role;
	with 'RDF::API::Parser';
}

1;
