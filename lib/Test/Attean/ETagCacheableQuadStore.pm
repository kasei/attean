package Test::Attean::ETagCacheableQuadStore;

use v5.14;
use warnings;
use Test::Roo::Role;
use Test::Moose;
use Attean;
use Attean::RDF;

requires 'create_store';       # create_store( quads => \@quads )
with 'Test::Attean::StoreCleanup';

test 'etagcacheablequadstore' => sub {
    my $self	= shift;
    my $time	= time();
    my $store	= $self->create_store(quads => []);
    my $etag	= $store->etag_value_for_quads();
    ok(length($etag));
	$self->cleanup_store($store);
};

1;
