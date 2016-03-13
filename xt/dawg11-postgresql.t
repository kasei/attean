#!/usr/bin/perl

use utf8;
use v5.14;
use strict;
use warnings;
no warnings 'redefine';
no warnings 'once';

binmode(\*STDOUT, ':encoding(UTF-8)');
binmode(\*STDERR, ':encoding(UTF-8)');

use autodie;
use Test::Roo;
use List::MoreUtils qw(all);

with 'Test::Attean::SPARQLSuite';

my %args;
while (defined(my $opt = shift)) {
	if ($opt eq '-v') {
		$args{debug}++;
	} else {
		$args{pattern}	= $opt;
	}
}

sub test_model {
	unless (all { exists $ENV{$_} } qw(ATTEAN_STORE_PG_DATABASE ATTEAN_STORE_PG_HOST ATTEAN_STORE_PG_USER ATTEAN_STORE_PG_PASSWORD)) {
		plan skip_all => "Set the PostgreSQL environment variables to run these tests (ATTEAN_STORE_PG_DATABASE, ATTEAN_STORE_PG_HOST, ATTEAN_STORE_PG_PORT, ATTEAN_STORE_PG_USER, ATTEAN_STORE_PG_PASSWORD)";
		exit(0);
	}

	my @connect		= AtteanX::Store::DBI->dbi_connect_args(
		'postgresql',
		database	=> $ENV{ATTEAN_STORE_PG_DATABASE},
		host		=> $ENV{ATTEAN_STORE_PG_HOST},
		user		=> $ENV{ATTEAN_STORE_PG_USER},
		password	=> $ENV{ATTEAN_STORE_PG_PASSWORD},
	);
	my $dbh			= DBI->connect(@connect);
	my $store		= Attean->get_store('DBI')->new( dbh => $dbh );
	foreach my $g ($store->get_graphs->elements) {
		$store->drop_graph($g);
	}
	my $model	= Attean::MutableQuadModel->new( store => $store );
	return $model;
}

run_me(\%args);

done_testing;
