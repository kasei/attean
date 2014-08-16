use v5.14;
use warnings;

package Attean::API::Model 0.001 {
	use Moose::Role;
	use Scalar::Util qw(blessed);
	use List::MoreUtils qw(uniq);
	
	requires 'get_quads';
	
	sub get_bindings {
		my $self	= shift;
		my @vars	= uniq map { $_->value } grep { blessed($_) and $_->isa('Attean::Variable') } @_;
		my $iter	= $self->get_quads(@_);
		# TODO
		die 'unimplemented';
	}
	
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

package Attean::API::MutableModel 0.001 {
	use Moose::Role;
	
	requires 'add_quad';
	requires 'remove_quad';
	requires 'create_graph';
	requires 'drop_graph';
	requires 'clear_graph';
}

package Attean::API::CacheableModel 0.001 {
	use Moose::Role;
	
	requires 'last_modified_date_for_quads';
}

package Attean::API::BulkUpdatableModel 0.001 {
	use Moose::Role;
	
	with 'Attean::API::MutableModel';
	
	requires 'begin_bulk_updates';
	requires 'end_bulk_updates';
}

1;
