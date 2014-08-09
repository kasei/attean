use v5.14;
use warnings;

package RDF::API::TripleStore 0.001 {
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
}

package RDF::API::MutableTripleStore 0.001 {
	use Moose::Role;
	with 'RDF::API::TripleStore';
	
	requires 'add_triple';
	requires 'remove_triple';
}

package RDF::API::CacheableTripleStore 0.001 {
	use Moose::Role;
	with 'RDF::API::TripleStore';
	
	requires 'last_modified_date_for_triples';
}

package RDF::API::QuadStore 0.001 {
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
}

package RDF::API::MutableQuadStore 0.001 {
	use Moose::Role;
	with 'RDF::API::QuadStore';
	
	requires 'add_quad';
	requires 'remove_quad';
}

package RDF::API::CacheableQuadStore 0.001 {
	use Moose::Role;
	with 'RDF::API::QuadStore';
	
	requires 'last_modified_date_for_quads';
}

1;
