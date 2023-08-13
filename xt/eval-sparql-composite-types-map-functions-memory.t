#!/usr/bin/env perl

use v5.14;
use strict;
use warnings;
no warnings 'redefine';
no warnings 'once';

binmode(\*STDERR, ':encoding(utf8)');
binmode(\*STDOUT, ':encoding(utf8)');

use autodie;
use Test::Roo;
use List::MoreUtils qw(all);
use FindBin qw($Bin);

with 'Test::Attean::SPARQLStarSuite';

##################
# use Devel::StackTrace;
# use ex::override GLOBAL_die => sub {
#     local *__ANON__ = "custom_die";
#     warn (
#         'Error: ', @_, "\n",
#         "Stack trace:\n",
#         Devel::StackTrace->new(no_refs => 1)->as_string, "\n",
#     );
#     exit 1;
# };
##################

use AtteanX::Functions::CompositeLists;
use AtteanX::Functions::CompositeMaps;
AtteanX::Functions::CompositeLists->register();
AtteanX::Functions::CompositeMaps->register();

sub BUILD {
	my $self	= shift;
	my $path	= File::Spec->catfile( $Bin, 'data', 'cdt', 'map-functions' );
	$self->tests_dir($path);
}

my %args	= (run_update_tests => 0, use_idp_planner => 0);
while (defined(my $opt = shift)) {
	if ($opt eq '-v') {
		$args{debug}++;
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
