package Test::Attean::MutableETagCacheableQuadStore;

use v5.14;
use warnings;
use Test::Roo::Role;
use Test::Moose;
use Attean;
use Attean::RDF;

requires 'create_store';       # create_store( quads => \@quads )
with 'Test::Attean::ETagCacheableQuadStore';

test 'mutable etagcacheablequadstore' => sub {
    my $self	= shift;
    my $q1		= quad(iri('s'), iri('p'), iri('o'), iri('g'));
    my $q2		= quad(iri('x'), iri('y'), iri('z'), iri('g'));
    my $store	= $self->create_store(quads => [$q1, $q2]);
    my $start	= $store->etag_value_for_quads();
    $store->remove_quad($q1);
    my $end		= $store->etag_value_for_quads();
    isnt($start, $end, "etag changed after update ($start => $end)");
	$self->cleanup_store($store);
};

1;
