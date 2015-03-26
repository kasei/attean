package Test::Attean::TripleStore;

use v5.14;
use Test::Roo::Role;
use Test::Moose;
use Attean;

requires 'create_store';       # create_store( triples => \@triples )

test 'get_triples' => sub {
    my $self	= shift;
    my @triples;
    my $store	= $self->create_store(triples => \@triples);
    ok $store->does('Attean::API::Store');
    ok $store->does('Attean::API::TripleStore');
};

# test 'count_triples' => sub {};
# test 'count_triples_estimate' => sub {};
# test 'size' => sub {};

1;
