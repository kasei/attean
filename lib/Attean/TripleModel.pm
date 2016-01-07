use v5.14;
use warnings;

=head1 NAME

Attean::QuadModel - RDF model backed by a quad-store

=head1 VERSION

This document describes Attean::QuadModel version 0.010

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $model = Attean::QuadModel->new( store => $store );

=head1 DESCRIPTION

The Attean::QuadModel class represents a model that is backed by a single
L<Attean::API::QuadStore|Attean::API::Store> object.
It conforms to the L<Attean::API::Model> role.

The Attean::QuadModel constructor requires one named argument:

=over 4

=item stores

A hash mapping graph IRI values to L<Attean::API::TripleStore|Attean::API::Store>
objects representing the backing triple-store for that graph.

=back

=head1 METHODS

=over 4

=cut

package Attean::TripleModel 0.010 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf HashRef);
	use Scalar::Util qw(reftype);
	use namespace::clean;

	with 'MooX::Log::Any';
	with 'Attean::API::Model';
	with 'Attean::API::CostPlanner';

	has 'stores'	=> (
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
						push(@iters, $self->_get_quads($s, $p, $o, $g));
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
		if ($g->does('Attean::API::IRI')) {
			if (my $store = $self->store->{ $g->value }) {
				my $iter	= $store->get_triples($s, $p, $o);
				return $iter->as_quads($g);
			}
		} else {
			my $name	= $g->can('as_string') ? $g->as_string : "$g";
			$self->log->warn("TripleModel cannot produce quads for non-IRI graph: $name");
		}
		return Attean::ListIterator->new( values => [], item_type => 'Attean::API::Quad' );
	}
	
=item C<< plans_for_algebra( $algebra, $model, $active_graphs, $default_graphs ) >>

Delegates to an underlying store if the active graph is bound to the store,
and the store consumes Attean::API::CostPlanner.

=cut

	sub plans_for_algebra {
		my $self	= shift;
		my $algebra	= shift;
		my $model	= shift;
		my $active_graphs	= shift;
		my $default_graphs	= shift;
		my @plans;
		if (scalar(@$active_graphs) == 1) {
			my $graph	= $active_graphs->[0];
			if (my $store = $self->stores->{ $graph->value }) {
				if ($store->does('Attean::API::CostPlanner')) {
					push(@plans, $store->plans_for_algebra($algebra, $model, $active_graphs, $default_graphs));
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

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO

L<http://www.perlrdf.org/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
