#!/usr/bin/env perl

use utf8;
use v5.14;
use strict;
use warnings;
no warnings 'redefine';
no warnings 'once';

binmode(\*STDERR, ':encoding(utf8)');
binmode(\*STDOUT, ':encoding(utf8)');

use autodie;
use Test::Roo;
use List::Util qw(all);
use FindBin qw($Bin);
use Attean;
use Module::Load::Conditional qw(can_load);
use namespace::clean;

with 'Test::Attean::SPARQLSuite';

sub BUILD {
	my $self		= shift;
	my $env_path	= $ENV{'W3C_RDF_TESTS_PATH'};
	my $base		= (defined($env_path) and -d $env_path) ? $env_path : File::Spec->catfile( $Bin, 'data', 'rdf-tests' );
	my $path		= File::Spec->catfile( $base, 'sparql', 'sparql11' );
	$self->tests_dir($path);

	if (can_load( modules => { 'RDF::EARL' => 0.002 })) {
		my $v			= $Attean::VERSION;
		$v				=~ s/[.]/-/;
		my $subject		= "http://purl.org/NET/cpan-uri/dist/Attean/v_$v";
		my $earl		= RDF::EARL->new( subject => $subject );
		$self->earl($earl);
	}
}

my %args	= (use_idp_planner => 0, run_update_tests => 0);
while (defined(my $opt = shift)) {
	if ($opt eq '-v') {
		$args{debug}++;
		$args{results}++;
	} else {
		$args{pattern}	= $opt;
	}
}
run_me(\%args);

done_testing;

sub test_model {
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	return $model;
}
