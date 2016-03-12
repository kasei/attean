use Test::Roo;
use Test::Modern;
use Test::Exception;
use List::MoreUtils qw(all);

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use AtteanX::Store::DBI;

sub create_store {
	my $self	= shift;
	my %args	= @_;
	my $quads	= $args{quads} // [];
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
	foreach my $q (@$quads) {
		$store->add_quad($q);
	}
	return $store;
}

unless (all { exists $ENV{$_} } qw(ATTEAN_STORE_MYSQL_DATABASE ATTEAN_STORE_MYSQL_HOST ATTEAN_STORE_MYSQL_USER ATTEAN_STORE_MYSQL_PASSWORD)) {
	plan skip_all => "Set the MySQL environment variables to run these tests (ATTEAN_STORE_MYSQL_DATABASE, ATTEAN_STORE_MYSQL_HOST, ATTEAN_STORE_MYSQL_PORT, ATTEAN_STORE_MYSQL_USER, ATTEAN_STORE_MYSQL_PASSWORD)";
}

with 'Test::Attean::QuadStore', 'Test::Attean::MutableQuadStore';
run_me; # run these Test::Attean tests

done_testing();
