=head1 NAME

AtteanX::Store::Memory2 - Simple in-memory RDF store

=head1 VERSION

This document describes AtteanX::Store::Memory2 version 0.040

=head1 SYNOPSIS

 use AtteanX::Store::Memory2;

=head1 DESCRIPTION

AtteanX::Store::Memory2 provides an in-memory quad-store based on trie indexes.
It should be more efficient for querying than L<AtteanX::Store::Memory>, at the
cost of more expensive updates.

L<AtteanX::Store::Memory> is still useful for its implementation of the
L<Attean::API::ETagCacheableQuadStore> and L<Attean::API::TimeCacheableQuadStore>
roles.

=cut

use v5.14;
use warnings;

package AtteanX::Store::Memory2 0.040 {
use Moo;
use Type::Tiny::Role;
use Types::Standard qw(Int ArrayRef HashRef ConsumerOf InstanceOf);
use Encode;
use Data::Dumper;
use List::Util qw(first);
use Scalar::Util qw(refaddr reftype blessed);
use Math::Cartesian::Product;
use AtteanX::Store::Memory2::SPOGIndex;
use namespace::clean;

with 'Attean::API::RDFStarStore';
with 'Attean::API::MutableQuadStore';
with 'Attean::API::QuadStore';

my @pos_names	= Attean::API::Quad->variables;

=head1 ATTRIBUTES

=over 4

=item C<< spog >>

A trie index in the SPOG order.

=item C<< pogs >>

A trie index in the POGS order.

=item C<< gpso >>

A trie index in the GPSO order.

=back

=head1 METHODS

Beyond the methods documented below, this class inherits methods from the
L<Attean::API::QuadStore> class.

=over 4

=item C<< new () >>

Returns a new memory-backed storage object.

=cut

has spog		=> (is => 'rw', isa => InstanceOf['AtteanX::Store::Memory2::SPOGIndex']);
has pogs		=> (is => 'rw', isa => InstanceOf['AtteanX::Store::Memory2::SPOGIndex']);
has gpso		=> (is => 'rw', isa => InstanceOf['AtteanX::Store::Memory2::SPOGIndex']);

sub BUILD {
	my $self	= shift;
	$self->spog(AtteanX::Store::Memory2::SPOGIndex->new(order => [qw(subject predicate object graph)]));
	$self->pogs(AtteanX::Store::Memory2::SPOGIndex->new(order => [qw(predicate object graph subject)]));
	$self->gpso(AtteanX::Store::Memory2::SPOGIndex->new(order => [qw(graph predicate subject object)]));
}

sub indexes {
	my $self	= shift;
	my @i;
	push(@i, $self->spog);
	push(@i, $self->pogs);
	push(@i, $self->gpso);
	return @i;
}

=item C<< size >>

Returns the number of quads in the store.

=cut

sub size {
	my $self	= shift;
	my $count	= eval { $self->spog->size() };
	warn $@ if ($@);
	return $count;
}

=item C<< get_quads ( $subject, $predicate, $object, $graph ) >>

Returns a stream object of all statements matching the specified subject,
predicate and objects. Any of the arguments may be undef to match any value.

=cut

sub get_quads {
	my $self	= shift;
	my @nodes	= map { ref($_) eq 'ARRAY' ? $_ : [$_] } @_;
	my @iters;
	cartesian { push(@iters, $self->_get_quads(@_)) } @nodes;
	return Attean::IteratorSequence->new( iterators => \@iters, item_type => 'Attean::API::Quad' );
}

sub _get_quads {
	my $self	= shift;
	my @nodes	= @_;
	my $iter;
	my $s	= $nodes[0];
	my $g	= $nodes[3];
	
	# Simple heuristics to choose which index to use:
	# - SPOG if subject is bound
	# - GPSO if graph is bound
	# - POGS for any other pattern
	if (blessed($s) and $s->does('Attean::API::Term')) {
		$iter	= eval { $self->spog->match(@nodes) };
	} elsif (blessed($g) and $g->does('Attean::API::Term')) {
		$iter	= eval { $self->gpso->match(@nodes) };
	} else {
		$iter	= eval { $self->pogs->match(@nodes) };
	}
	warn $@ if ($@);
	return $iter;
}

=item C<< get_graphs >>

Returns an iterator over the Attean::API::Term objects comprising
the set of graphs of the stored quads.

=cut

sub get_graphs {
	my $self	= shift;
	if (my $gpso = $self->gpso) {
		return $gpso->head_values();
	} else {
		my %graphs;
		eval {
			my $iter	= $self->spog->match();
			while (my $q = $iter->next) {
				my $g	= $q->graph;
				$graphs{ $g->as_string }	= $g;
			}
		};
		warn $@ if ($@);
		return Attean::ListIterator->new( values => [values %graphs], item_type => 'Attean::API::Term' );
	}
}

=item C<< add_quad ( $quad ) >>

Adds the specified C<$quad> to the underlying model.

=cut

sub add_quad {
	my $self	= shift;
	my $st		= shift;
	eval {
		foreach my $i ($self->indexes) {
			$i->add_quad($st);
		}
	};
	warn $@ if ($@);
}

=item C<< remove_quad ( $statement ) >>

Removes the specified C<$statement> from the underlying model.

=cut

sub remove_quad {
	my $self	= shift;
	my $st		= shift;
	eval {
		foreach my $i ($self->indexes) {
			$i->remove_quad($st);
		}
	};
	warn $@ if ($@);
}

=item C<< remove_quads ( $subject, $predicate, $object, $graph ) >>

Removes the specified C<$statement> from the underlying model.

=cut

sub remove_quads {
	my $self	= shift;
	my @nodes	= map { ref($_) eq 'ARRAY' ? $_ : [$_] } @_;
	my @iters;
	cartesian { $self->_remove_quads(@_) } @nodes;
}

sub _remove_quads {
	my $self	= shift;
	my $subj	= shift;
	my $pred	= shift;
	my $obj		= shift;
	my $graph	= shift;
	my $iter	= $self->get_quads( $subj, $pred, $obj, $graph );
	while (my $st = $iter->next) {
		$self->remove_quad( $st );
	}
}

=item C<< create_graph( $graph ) >>

This is a no-op function for the memory quad-store.

=cut

sub create_graph {
	# no-op on a quad-store
}

=item C<< drop_graph( $graph ) >>

Removes all quads with the given C<< $graph >>.

=cut

sub drop_graph {
	my $self	= shift;
	return $self->clear_graph(@_);
}

=item C<< clear_graph( $graph ) >>

Removes all quads with the given C<< $graph >>.

=cut

sub clear_graph {
	my $self	= shift;
	my $g		= shift;
	my $iter	= $self->spog->match(undef, undef, undef, $g);
	eval {
		while (my $q = $iter->next) {
			foreach my $i ($self->indexes) {
				$i->remove_quad($q);
			}
		}
	};
	warn $@ if ($@);
}

=item C<< count_quads ( $subject, $predicate, $object, $graph ) >>

Returns a count of all the statements matching the specified subject,
predicate, object, and graph. Any of the arguments may be undef to match any
value.

=cut

sub count_quads {
	my $self	= shift;
	my @nodes	= map { ref($_) eq 'ARRAY' ? $_ : [$_] } @_;
	my $count	= 0;
	cartesian { $count += $self->_count_quads(@_) } @nodes;
	return $count;
}

sub _count_quads {
	my $self	= shift;
	my @nodes	= @_[0..3];
	my $iter	= $self->spog->match(@nodes);
	my $count	= 0;
	while (my $q = $iter->next) {
		$count++;
	}
	return $count;
}

}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/perlrdf2/issues>.

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2026 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
