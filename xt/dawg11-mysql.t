#!/usr/bin/perl

use v5.14;
use strict;
use warnings;
no warnings 'redefine';
no warnings 'once';

use autodie;
use Test::Roo;
use List::MoreUtils qw(all);

with 'Test::Attean::SPARQLSuite';

run_me();

done_testing;

sub test_model {
	unless (all { exists $ENV{$_} } qw(ATTEAN_STORE_MYSQL_DATABASE ATTEAN_STORE_MYSQL_HOST ATTEAN_STORE_MYSQL_USER ATTEAN_STORE_MYSQL_PASSWORD)) {
		plan skip_all => "Set the MySQL environment variables to run these tests (ATTEAN_STORE_MYSQL_DATABASE, ATTEAN_STORE_MYSQL_HOST, ATTEAN_STORE_MYSQL_PORT, ATTEAN_STORE_MYSQL_USER, ATTEAN_STORE_MYSQL_PASSWORD)";
	}

	my @connect		= AtteanX::Store::DBI->dbi_connect_args(
		'mysql',
		database	=> $ENV{ATTEAN_STORE_MYSQL_DATABASE},
		host		=> $ENV{ATTEAN_STORE_MYSQL_HOST},
		user		=> $ENV{ATTEAN_STORE_MYSQL_USER},
		password	=> $ENV{ATTEAN_STORE_MYSQL_PASSWORD},
	);
	my $dbh			= DBI->connect(@connect);
	my $store		= Attean->get_store('DBI')->new( dbh => $dbh );
	foreach my $g ($store->get_graphs->elements) {
		$store->drop_graph($g);
	}
	my $model	= Attean::MutableQuadModel->new( store => $store );
	return $model;
}
