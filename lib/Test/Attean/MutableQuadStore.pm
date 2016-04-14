package Test::Attean::MutableQuadStore;

use v5.14;
use warnings;
use Test::Roo::Role;
use Test::Moose;
use Attean;
use Attean::RDF;

requires 'create_store';      # create_store( quads => \@quads )
with 'Test::Attean::StoreCleanup';

test 'mutablequadstore add_quad' => sub {
    my $self	= shift;
    my $q1		= quad(iri('s'), iri('p'), iri('o'), iri('g'));
    my $q2		= quad(iri('x'), iri('y'), iri('z'), iri('g'));
    my $q3		= quad(iri('x'), iri('y'), iri('z'), iri('g2'));
    
    my $store	= $self->create_store(quads => []);
    my $size	= 0;
    for my $q ($q1, $q2, $q3) {
    	$store->add_quad($q);
    	is($store->size, ++$size, "size $size");
    }
	$self->cleanup_store($store);
};

test 'mutablequadstore remove_quad' => sub {
    my $self	= shift;
    my $q1		= quad(iri('s'), iri('p'), iri('o'), iri('g'));
    my $q2		= quad(iri('x'), iri('y'), iri('z'), iri('g'));
    my $q3		= quad(iri('x'), iri('y'), iri('z'), iri('g2'));
    
    my $store	= $self->create_store(quads => [$q3, $q2, $q1]);
    my $size	= 3;
    for my $q ($q1, $q2, $q3) {
    	is($store->size, $size, "size $size");
    	$store->remove_quad($q);
    	$size--;
    }
	$store->remove_quad($q2);
	is($store->size, 0, "size $size");
	$self->cleanup_store($store);
};

test 'mutablequadstore create_graph' => sub {
    my $self	= shift;
    my $store	= $self->create_store(quads => []);
    
    my $count	= 0;
    foreach my $g (iri('g1'), iri('g2'), iri('g3')) {
		$store->create_graph($g);
		my @graphs	= sort map { $_->value } $store->get_graphs->elements;
		my $graphs	= scalar(@graphs);
		ok($graphs == 0 or $graphs == ++$count);
    }

	$store->create_graph(iri('g2'));
	my @graphs	= sort map { $_->value } $store->get_graphs->elements;
	my $graphs	= scalar(@graphs);
	ok($graphs == 0 or $graphs == $count);
	$self->cleanup_store($store);
};

test 'mutablequadstore drop_graph' => sub {
	# drop_graph removes all the quads in a specific graph and removes the
	# graph from the list of graphs returned as an iterator from
	# $store->get_graphs
    my $self	= shift;
    my $q1		= quad(iri('s'), iri('p'), iri('o'), iri('g'));
    my $q2		= quad(iri('x'), iri('y'), iri('z'), iri('g'));
    my $q3		= quad(iri('x'), iri('y'), iri('z'), iri('g2'));
    
    {
		my $store	= $self->create_store(quads => [$q1, $q2, $q3]);
		$store->drop_graph(iri('g'));
		is($store->size, 1);
		my @graphs	= sort map { $_->value } $store->get_graphs->elements;
		is_deeply(\@graphs, ['g2']);
		$self->cleanup_store($store);
	}
    {
		my $store	= $self->create_store(quads => [$q1, $q2, $q3]);
		$store->drop_graph(iri('g2'));
		is($store->size, 2);
		my @graphs	= sort map { $_->value } $store->get_graphs->elements;
		is_deeply(\@graphs, ['g']);
		$self->cleanup_store($store);
	}
};

test 'mutablequadstore clear_graph' => sub {
	# clear_graph removes all the quads in a specific graph
	# depending on whether the implementation supports empty graphs,
	# the cleared graph may or may not disappear from the list of graphs
	# returned as an iterator from $store->get_graphs
    my $self	= shift;
    my $q1		= quad(iri('s'), iri('p'), iri('o'), iri('g'));
    my $q2		= quad(iri('x'), iri('y'), iri('z'), iri('g'));
    my $q3		= quad(iri('x'), iri('y'), iri('z'), iri('g2'));
    
    {
		my $store	= $self->create_store(quads => [$q1, $q2, $q3]);
		$store->clear_graph(iri('g'));
		is($store->size, 1);
		my @graphs	= sort map { $_->value } $store->get_graphs->elements;
		my $graphs	= scalar(@graphs);
		ok($graphs == 1 or $graphs == 2);
		$self->cleanup_store($store);
	}
    {
		my $store	= $self->create_store(quads => [$q1, $q2, $q3]);
		$store->clear_graph(iri('g2'));
		is($store->size, 2);
		my @graphs	= sort map { $_->value } $store->get_graphs->elements;
		my $graphs	= scalar(@graphs);
		ok($graphs == 1 or $graphs == 2);
		$self->cleanup_store($store);
	}
};

1;
