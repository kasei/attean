package Test::Attean::TimeCacheableQuadStore;

use v5.14;
use warnings;
use Test::Roo::Role;
use Test::Moose;
use Attean;
use Attean::RDF;

requires 'create_store';       # create_store( quads => \@quads )
sub acceptable_mtime_delta {
	return 60 * 60 * 24;
}

test 'timecacheablequadstore' => sub {
    my $self	= shift;
    my $time	= time();
    my $store	= $self->create_store(quads => []);
    my $mtime	= $store->mtime_for_quads();
    my $diff	= abs($mtime - $time);
    my $delta	= $self->acceptable_mtime_delta;
    cmp_ok($diff, '<', $delta, "mtime within delta ($diff seconds from expected)");
};

1;
