package Test::Attean::MutableTimeCacheableQuadStore;

use v5.14;
use warnings;
use Test::Roo::Role;
use Test::Moose;
use Attean;
use Attean::RDF;

requires 'create_store';		# create_store( quads => \@quads )
with 'Test::Attean::StoreCleanup';
with 'Test::Attean::TimeCacheableQuadStore';

sub caching_sleep_time {
	return 30;
}

test 'mutable timecacheablequadstore' => sub {
    my $self	= shift;
    my $q1		= quad(iri('s'), iri('p'), iri('o'), iri('g'));
    my $q2		= quad(iri('x'), iri('y'), iri('z'), iri('g'));
    my $store	= $self->create_store(quads => [$q1, $q2]);
    my $start	= $store->mtime_for_quads();
    my $s		= $self->caching_sleep_time;
    note("Sleeping for $s seconds");
    sleep($s);
    $store->remove_quad($q1);
    my $end		= $store->mtime_for_quads();
    my $diff	= abs($end - $start);
    isnt($start, $end, "mtime changed after update (by $diff seconds)");
	$self->cleanup_store($store);
};

1;
