use v5.14;
use warnings;

=head1 NAME

Attean::TripleModel - RDF model backed by a set of triple-stores

=head1 VERSION

This document describes Attean::TripleModel version 0.034

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $model = Attean::TripleModel->new( stores => {
    'http://example.org/graph1' => $store1,
    'http://example.org/graph2' => $store2,
  } );

=head1 DESCRIPTION

The Attean::TripleModel class represents a model that is backed by a set of
L<Attean::API::TripleStore|Attean::API::Store> objects, identified by an IRI
string. It conforms to the L<Attean::API::Model> role.

The Attean::TripleModel constructor requires one named argument:

=over 4

=item stores

A hash mapping graph IRI values to L<Attean::API::TripleStore|Attean::API::Store>
objects representing the backing triple-store for that graph.

=back

=head1 METHODS

=over 4

=cut

package Attean::TripleModel 0.034 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf HashRef);
	use Scalar::Util qw(reftype blessed);
	use namespace::clean;

	with 'MooX::Log::Any';
	with 'Attean::API::Model';
	with 'Attean::API::CostPlanner';

	has 'stores' => (
		is => 'ro',
		isa => HashRef[ConsumerOf['Attean::API::TripleStore']],
		required => 1,
		default => sub { +{} },
	);
	
=item C<< size >>

=cut

	sub size {
		my $self	= shift;
		my $count	= 0;
		foreach my $store (values %{ $self->stores }) {
			$count	+= $store->size;
		}
		return $count;
	}
	

=item C<< count_quads >>

=cut

	sub count_quads {
		my $self	= shift;
		# TODO: don't materialize results here just to count them
		my $iter	= $self->get_quads( @_ );
		my $count	= 0;
		while (my $r = $iter->next) {
			$count++;
		}
		return $count;
	}

=item C<< count_quads_estimate >>

=cut

	sub count_quads_estimate {
		my $self	= shift;
		my ($s, $p, $o, $g)	= @_;
		if (blessed($g) and $g->does('Attean::API::IRI')) {
			if (my $store = $self->stores->{ $g->value }) {
				return $store->count_quads_estimate(@_);
			} else {
				return 0;
			}
		} else {
			return $self->count_quads(@_);
		}
	}
	
=item C<< holds >>

=cut

	sub holds {
		my $self	= shift;
		return ($self->count_quads_estimate(@_) > 0)
	}
	
=item C<< get_graphs >>

=cut

	sub get_graphs {
		my $self	= shift;
		my @graphs	= map { Attean::IRI->new($_) } keys %{ $self->stores };
		return Attean::ListIterator->new( values => \@graphs, item_type => 'Attean::API::Term' );
	}
	
=item C<< get_quads ( $subject, $predicate, $object, $graph ) >>

Returns an L<Attean::API::Iterator> for quads in the model that match the
supplied C<< $subject >>, C<< $predicate >>, C<< $object >>, and C<< $graph >>.
Any of these terms may be undefined or a L<Attean::API::Variable> object, in
which case that term will be considered as a wildcard for the purposes of
matching.

The returned iterator conforms to both L<Attean::API::Iterator> and
L<Attean::API::QuadIterator>.

=cut

	sub get_quads {
		my $self	= shift;
		my @nodes	= @_[0..3];
		foreach my $i (0..3) {
			my $t	= $nodes[$i] // Attean::Variable->new();
			if (not(ref($t)) or reftype($t) ne 'ARRAY') {
				$nodes[$i]	= [$t];
			}
		}
		
		my @iters;
		foreach my $s (@{ $nodes[0] }) {
			foreach my $p (@{ $nodes[1] }) {
				foreach my $o (@{ $nodes[2] }) {
					foreach my $g (@{ $nodes[3] }) {
						my $iter	= $self->_get_quads($s, $p, $o, $g);
						push(@iters, $iter);
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
	
	sub _get_quads {
		my $self	= shift;
		my $s		= shift;
		my $p		= shift;
		my $o		= shift;
		my $g		= shift;
		if (blessed($g) and $g->does('Attean::API::IRI')) {
			if (my $store = $self->stores->{ $g->value }) {
				my $iter	= $store->get_triples($s, $p, $o);
				return $iter->as_quads($g);
			}
		} elsif (blessed($g) and $g->does('Attean::API::Variable')) {
			my @iters;
			while (my ($g, $store) = each %{ $self->stores }) {
				my $iter	= $store->get_triples($s, $p, $o);
				my $graph	= Attean::IRI->new($g);
				my $quads	= $iter->map(sub { $_->as_quad($graph) }, 'Attean::API::Quad');
				push(@iters, $quads);
			}
			my $iter	= Attean::IteratorSequence->new( iterators => \@iters, item_type => $iters[0]->item_type );
			return $iter;
		} else {
			my $name	= (blessed($g) and $g->can('as_string')) ? $g->as_string : "$g";
			$self->log->warn("TripleModel cannot produce quads for non-IRI graph: $name");
		}
		return Attean::ListIterator->new( values => [], item_type => 'Attean::API::Quad' );
	}
	
=item C<< plans_for_algebra( $algebra, $planner, $active_graphs, $default_graphs ) >>

Delegates to an underlying store if the active graph is bound to the store,
and the store consumes Attean::API::CostPlanner.

=cut

	sub plans_for_algebra {
		my $self	= shift;
		my $algebra	= shift;
		my $planner	= shift;
		my $active_graphs	= shift;
		my $default_graphs	= shift;
		my @plans;
		if (scalar(@$active_graphs) == 1) {
			my $graph	= $active_graphs->[0];
			if (my $store = $self->stores->{ $graph->value }) {
				if ($store->does('Attean::API::CostPlanner')) {
					push(@plans, $store->plans_for_algebra($algebra, $planner, $active_graphs, $default_graphs));
				}
			}
		}
		return @plans;
	}
	
=item C<< cost_for_plan( $plan ) >>

Attempts to delegate to all the underlying stores if that store consumes Attean::API::CostPlanner.

=cut

	sub cost_for_plan {
		my $self	= shift;
		my $plan	= shift;
		foreach my $store (values %{ $self->stores }) {
			if ($store->does('Attean::API::CostPlanner')) {
				if (defined(my $cost = $store->cost_for_plan($plan, @_))) {
					return $cost;
				}
			}
		}
		return;
	}
}

package Attean::AddativeTripleModelRole 0.034 {
	use Scalar::Util qw(blessed);
	use Types::Standard qw(CodeRef);

	use Moo::Role;
	
	with 'Attean::API::Model';
	has 'store_constructor'	=> (is => 'ro', isa => CodeRef, required => 1);
	
=item C<< add_store( $graph => $store ) >>

Add the L<Attean::TripleStore> C<< $store >> object to the model using the
IRI string value C<< $graph >> as the graph name.

=cut

	sub add_store {
		my $self	= shift;
		my $graph	= shift;
		my $iri		= blessed($graph) ? $graph->value : $graph;
		my $store	= shift;
		
		die if exists $self->stores->{ $iri };
		$self->stores->{ $iri }	= $store;
	}

=item C<< create_graph( $graph ) >>

Create a new L<Attean::TripleStore> and add it to the model using the
L<Attean::API::IRI> C<< $graph >> as the graph name.

The store is constructed by using this object's C<< store_constructor >>
attribute:

  my $store = $self->store_constructor->($graph);

=cut

	sub create_graph {
		my $self	= shift;
		my $graph	= shift;
		my $iri		= $graph->value;
		return if exists $self->stores->{ $iri };

		my $store	= $self->store_constructor->($graph);
		$self->stores->{ $iri }	= $store;
	};

=item C<< drop_graph( $graph ) >>

Removes the store associated with the given C<< $graph >>.

=cut

	sub drop_graph {
		my $self	= shift;
		my $g		= shift;
		if ($g->does('Attean::API::IRI')) {
			delete $self->stores->{ $g->value };
		}
	}
}


package Attean::MutableTripleModel 0.034 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf HashRef);
	use Scalar::Util qw(reftype);
	use namespace::clean;

	extends 'Attean::TripleModel';
	with 'Attean::API::MutableModel';
	
	has 'stores'	=> (
		is => 'ro',
		isa => HashRef[ConsumerOf['Attean::API::MutableTripleStore']],
		required => 1,
		default => sub { +{} },
	);

=item C<< add_quad ( $quad ) >>

Adds the specified C<$quad> to the underlying model.

=cut

	sub add_quad {
		my $self	= shift;
		my $q		= shift;
		my $g		= $q->graph;
		die "Cannot add a quad whose graph is not an IRI" unless ($g->does('Attean::API::IRI'));
		my $v		= $g->value;
		if (my $store = $self->stores->{ $v }) {
			$store->add_triple( $q->as_triple );
		} else {
			Carp::confess "No such graph: $v";
		}
	}

=item C<< remove_quad ( $quad ) >>

Removes the specified C<< $quad >> from the underlying store.

=cut

	sub remove_quad {
		my $self	= shift;
		my $q		= shift;
		my $g		= $q->graph;
		if ($g->does('Attean::API::IRI')) {
			my $v		= $g->value;
			if (my $store = $self->stores->{ $v }) {
				$store->remove_triple( $q->as_triple );
			}
		}
	}
	
	sub create_graph { die; }

=item C<< drop_graph( $graph ) >>

Removes the store associated with the given C<< $graph >>.

=cut

	sub drop_graph {
		my $self	= shift;
		my $g		= shift;
		if ($g->does('Attean::API::IRI')) {
			delete $self->stores->{ $g->value };
		}
	}
	
=item C<< clear_graph( $graph ) >>

Removes all quads with the given C<< $graph >>.

=cut

	sub clear_graph {
		my $self	= shift;
		my $g		= shift;
		$self->drop_graph($g);
		$self->create_graph($g);
	}
}

package Attean::AddativeTripleModel 0.034 {
	use Moo;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(CodeRef);
	use namespace::clean;
	
	extends 'Attean::TripleModel';
	with 'Attean::AddativeTripleModelRole';
}

package Attean::AddativeMutableTripleModel 0.034 {
	use Moo;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(CodeRef);
	use namespace::clean;
	
	extends 'Attean::MutableTripleModel';
	with 'Attean::AddativeTripleModelRole';
}


1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2022 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
