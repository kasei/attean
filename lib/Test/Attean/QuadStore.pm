package Test::Attean::QuadStore;

use v5.14;
use warnings;
use Test::Roo::Role;
use Test::Moose;
use Attean;
use Attean::RDF;

requires 'create_store';		# create_store( quads => \@quads )
with 'Test::Attean::StoreCleanup';

test 'quadstore roles' => sub {
	my $self	= shift;
	my $store	= $self->create_store(quads => []);
	ok $store->does('Attean::API::Store');
	ok $store->does('Attean::API::QuadStore');
	my $qiter	= $store->get_quads();
	ok $qiter->does('Attean::API::Iterator');
	is($qiter->item_type, 'Attean::API::Quad');

	my $giter	= $store->get_graphs;
	ok $giter->does('Attean::API::Iterator');
	is($giter->item_type, 'Attean::API::Term');
	$self->cleanup_store($store);
};

test 'quadstore get_quads empty' => sub {
	my $self	= shift;
	my $store	= $self->create_store(quads => []);
	{
		my $iter	= $store->get_quads();
		my @elements	= $iter->elements;
		is(scalar(@elements), 0);
	}
	{
		my $iter	= $store->get_quads(iri('s'), iri('p'));
		my @elements	= $iter->elements;
		is(scalar(@elements), 0);
	}
	$self->cleanup_store($store);
};

test 'quadstore get_quads with quads' => sub {
	my $self	= shift;
	my $q1		= quad(iri('s'), iri('p'), iri('o'), iri('g'));
	my $q2		= quad(iri('x'), iri('y'), iri('z'), iri('g'));
	my $store	= $self->create_store(quads => [$q1, $q2]);
	{
		my $iter	= $store->get_quads();
		my @elements	= $iter->elements;
		is(scalar(@elements), 2, '2 quads');
	}
	{
		my $iter	= $store->get_quads(iri('s'));
		my @elements	= $iter->elements;
		is(scalar(@elements), 1, '1 quad with <s> as subject');
	}
	{
		my $iter	= $store->get_quads(variable('s'), undef, undef, iri('g'));
		my @elements	= $iter->elements;
		is(scalar(@elements), 2, '2 quads with <g> as graph');
	}
	{
		my $iter	= $store->get_quads(iri('abc'));
		my @elements	= $iter->elements;
		is(scalar(@elements), 0, '0 quads with <abc> as subject');
	}
	$self->cleanup_store($store);
};

test 'count_quads' => sub {
	my $self	= shift;
	my $q1		= quad(iri('s'), iri('p'), iri('o'), iri('g'));
	my $q2		= quad(iri('x'), iri('y'), iri('z'), iri('g'));
	my $store	= $self->create_store(quads => [$q1, $q2]);
	is($store->count_quads(), 2, '2 quads');
	is($store->count_quads(iri('s')), 1, '1 quad with <s> as subject');
	is($store->count_quads(variable('s'), undef, undef, iri('g')), 2, '2 quads with <g> as graph');
	is($store->count_quads(iri('abc')), 0, '0 quads with <abc> as subject');
	cmp_ok($store->count_quads_estimate(iri('abc')), '>=', 0, 'count_quads_estimate');
	$self->cleanup_store($store);
};

# test 'count_quads_estimate' => sub {};

test 'size' => sub {
	my $self	= shift;
	my $q1		= quad(iri('s'), iri('p'), iri('o'), iri('g'));
	my $q2		= quad(iri('x'), iri('y'), iri('z'), iri('g'));
	my $q3		= quad(iri('x'), iri('y'), iri('z'), iri('g2'));
	my $store	= $self->create_store(quads => [$q1, $q2, $q3]);
	is($store->size(), 3);
	$self->cleanup_store($store);
};

test 'get_graphs' => sub {
	my $self	= shift;
	my $q1		= quad(iri('s'), iri('p'), iri('o'), iri('g'));
	my $q2		= quad(iri('x'), iri('y'), iri('z'), iri('g'));
	my $q3		= quad(iri('x'), iri('y'), iri('z'), iri('g2'));
	my $store	= $self->create_store(quads => [$q1, $q2, $q3]);
	my $iter	= $store->get_graphs;
	my @graphs	= sort map { $_->value } $iter->elements;
	is_deeply(\@graphs, ['g', 'g2']);
	$self->cleanup_store($store);
};

1;
