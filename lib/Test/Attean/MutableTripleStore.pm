package Test::Attean::MutableTripleStore;

use v5.14;
use warnings;
use Test::Roo::Role;
use Test::Moose;
use Attean;
use Attean::RDF;

requires 'create_store';       # create_store( triples => \@triples )
with 'Test::Attean::StoreCleanup';

test 'mutabletriplestore add_triple' => sub {
    my $self	= shift;
    my $t1		= triple(iri('s'), iri('p'), iri('o'));
    my $t2		= triple(iri('x'), iri('y'), iri('z'));
    my $t3		= triple(iri('x'), iri('y'), literal('123'));
    
    my $store	= $self->create_store(triples => []);
    my $size	= 0;
    for my $t ($t1, $t2, $t3) {
    	$store->add_triple($t);
    	is($store->size, ++$size, "size $size");
    }
	$self->cleanup_store($store);
};

test 'mutabletriplestore remove_triple' => sub {
    my $self	= shift;
    my $t1		= triple(iri('s'), iri('p'), iri('o'));
    my $t2		= triple(iri('x'), iri('y'), iri('z'));
    my $t3		= triple(iri('x'), iri('y'), literal('123'));
    
    my $store	= $self->create_store(triples => [$t3, $t2, $t1]);
    my $size	= 3;
    for my $t ($t1, $t2, $t3) {
    	is($store->size, $size, "size $size");
    	$store->remove_triple($t);
    	$size--;
    }
	$store->remove_triple($t2);
	is($store->size, 0, "size $size");
	$self->cleanup_store($store);
};

1;
