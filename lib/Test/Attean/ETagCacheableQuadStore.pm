package Test::Attean::ETagCacheableQuadStore;

use v5.18;
use Test::Roo::Role;
use Test::Moose;
use Attean;
use Attean::RDF;

requires 'create_store';       # create_store( quads => \@quads )

test 'etagcacheablequadstore' => sub {
    my $self	= shift;
    my $time	= time();
    my $store	= $self->create_store(quads => []);
    my $etag	= $store->etag_value_for_quads();
    ok(length($etag));
};

1;
