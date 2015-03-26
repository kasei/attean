package Test::Attean::TimeCacheableQuadStore;

use v5.18;
use Test::Roo::Role;
use Test::Moose;
use Attean;
use Attean::RDF;

requires 'create_store';       # create_store( quads => \@quads )

test 'timecacheablequadstore' => sub {
    my $self	= shift;
    my $time	= time();
    my $store	= $self->create_store(quads => []);
    my $mtime	= $store->mtime_for_quads();
    my $diff	= abs($mtime - $time);
    cmp_ok($diff, '<', 60 * 60 * 24, "mtime within delta ($diff seconds from expected)");
};

1;
