use v5.14;
use warnings;

package Attean::MutableQuadModel 0.001 {
	use Moose;
	has 'store'	=> (
		is => 'ro',
		isa => 'Attean::API::MutableQuadStore',
		required => 1,
		handles	=> [qw(size get_quads count_quads add_quad remove_quad get_graphs create_graph drop_graph clear_graph)],
	);

	with 'Attean::API::MutableModel';
}

1;
