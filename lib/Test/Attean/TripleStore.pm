package Test::Attean::TripleStore;

use v5.14;
use warnings;
use Test::Roo::Role;
use Test::Moose;
use Attean;
use Attean::RDF;

requires 'create_store';	   # create_store( triples => \@triples )
sub cleanup_store {}			# cleanup_store( $store )

test 'get_triples' => sub {
	my $self	= shift;
	my $t1		= triple(iri('http://example.org/s'), iri('http://example.org/p'), iri('http://example.org/o'));
	my $t2		= triple(iri('http://example.org/x'), iri('http://example.org/y'), iri('http://example.org/z'));
	my @triples = ($t1, $t2);
	my $store	= $self->create_store(triples => \@triples);
	ok $store->does('Attean::API::Store');
	ok $store->does('Attean::API::TripleStore');
	$self->cleanup_store($store);
};

test 'count_triples' => sub {
	my $self	= shift;
	my @triples;
	foreach (1 .. 20) {
		push(@triples, triple(iri('http://example.org/s'), iri('http://example.org/p'), literal($_)));
	}
	foreach (1,10,20,50) {
		push(@triples, triple(iri('http://example.org/z'), iri('http://example.org/p'), literal($_)));
	}
	foreach (1 .. 20) {
		push(@triples, triple(iri('http://example.org/s'), iri('http://example.org/q'), blank("b$_")));
	}
	my $store	= $self->create_store(triples => \@triples);
	is($store->count_triples(iri('http://example.org/UNEXPECTED')), 0, 'unexpected IRI');
	is($store->count_triples(iri('http://example.org/s')), 40, 'expected subject');
	is($store->count_triples(undef, iri('http://example.org/q')), 20, 'expected predicate');
	is($store->count_triples(undef, undef, literal('7')), 1, 'expected object');
	is($store->count_triples(undef, undef, literal('10')), 2, 'expected object (2)');
	is($store->count_triples(iri('http://example.org/z'), undef, literal('10')), 1, 'expected subject/object');
	is($store->count_triples(variable('s'), iri('http://example.org/q')), 20, 'expected predicate with variable');
	is($store->count_triples(variable('s'), variable('p'), literal('7')), 1, 'expected object with variable');
	is($store->count_triples(variable('s'), variable('p'), literal('10')), 2, 'expected object (2) with variable');
	is($store->count_triples(iri('http://example.org/z'), variable('o'), literal('10')), 1, 'expected subject/object with variable');
	cmp_ok($store->count_triples_estimate(iri('http://example.org/z')), '>=', 0, 'count_triples_estimate');
	$self->cleanup_store($store);
};


# test 'count_triples_estimate' => sub {};

test 'size' => sub {
	my $self	= shift;
	foreach my $size (1, 10, 25, 57) {
		my @triples;
		foreach (1 .. $size) {
			push(@triples, triple(iri('http://example.org/s'), iri('http://example.org/p'), literal($_)));
		}
		my $store	= $self->create_store(triples => \@triples);
		is($store->size(), $size);
		$self->cleanup_store($store);
	}
};

1;
