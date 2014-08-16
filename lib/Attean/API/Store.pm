use v5.14;
use warnings;

package Attean::API::TripleStore 0.001 {
	use Moose::Role;
	
	requires 'get_triples';

	sub count_triples {
		my $self	= shift;
		my $iter	= $self->get_triples(@_);
		my $count	= 0;
		while (my $r = $iter->next) {
			$count++;
		}
		return $count;
	}
	sub size {
		my $self	= shift;
		return $self->count_triples();
	}
}

package Attean::API::MutableTripleStore 0.001 {
	use Moose::Role;
	with 'Attean::API::TripleStore';
	
	requires 'add_triple';
	requires 'remove_triple';
}

package Attean::API::CacheableTripleStore 0.001 {
	use Moose::Role;
	with 'Attean::API::TripleStore';
	
	requires 'last_modified_date_for_triples';
}

package Attean::API::QuadStore 0.001 {
	use Moose::Role;
	
	requires 'get_quads';

	sub count_quads {
		my $self	= shift;
		my $iter	= $self->get_quads(@_);
		my $count	= 0;
		while (my $r = $iter->next) {
			$count++;
		}
		return $count;
	}
	
	sub get_graphs {
		my $self	= shift;
		my $iter	= $self->get_quads(@_);
		my %graphs;
		while (my $r = $iter->next) {
			my $g	= $r->graph;
			$graphs{ $g->as_string }++;
		}
		return keys %graphs;
	}
	
	sub size {
		my $self	= shift;
		return $self->count_quads();
	}
}

package Attean::API::MutableQuadStore 0.001 {
	use Moose::Role;
	with 'Attean::API::QuadStore';
	
	requires 'add_quad';
	requires 'remove_quad';
	requires 'create_graph';
	requires 'drop_graph';
	requires 'clear_graph';
}

package Attean::API::CacheableQuadStore 0.001 {
	use Moose::Role;
	with 'Attean::API::QuadStore';
	
	requires 'etag_value_for_quads';
}

1;
