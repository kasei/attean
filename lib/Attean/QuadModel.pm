use v5.14;
use warnings;

package Attean::QuadModel 0.001 {
	use Moose;
	use Scalar::Util qw(reftype);
	has 'store'	=> (
		is => 'ro',
		isa => 'Attean::API::QuadStore',
		required => 1,
		handles	=> [qw(size count_quads get_graphs)],
	);
	
	sub get_quads {
		my $self	= shift;
		my @nodes	= @_[0..3];
		foreach my $i (0..3) {
			my $t	= $nodes[$i];
			if (not(ref($t)) or reftype($t) ne 'ARRAY') {
				$nodes[$i]	= [$t];
			}
		}
		
		my @iters;
		foreach my $s (@{ $nodes[0] }) {
			foreach my $p (@{ $nodes[1] }) {
				foreach my $o (@{ $nodes[2] }) {
					foreach my $g (@{ $nodes[3] }) {
						push(@iters, $self->store->get_quads($s, $p, $o, $g));
					}
				}
			}
		}
		if (scalar(@iters) <= 1) {
			return shift(@iters);
		} else {
			return Attean::IteratorSequence->new( iterators => \@iters, item_type => $iters[0]->item_type );
		}
	}
	
	with 'Attean::API::Model';
}


package Attean::MutableQuadModel 0.001 {
	use Moose;
	use Scalar::Util qw(reftype);
	extends 'Attean::QuadModel';
	
	has 'store'	=> (
		is => 'ro',
		isa => 'Attean::API::MutableQuadStore',
		required => 1,
		handles	=> [qw(size count_quads add_quad remove_quad get_graphs create_graph drop_graph clear_graph add_iter)],
	);

	with 'Attean::API::MutableModel';
}

1;
