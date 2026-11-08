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
use List::Util qw(all);
use FindBin qw($Bin);
use Module::Load::Conditional qw(can_load);

with 'Test::Attean::SPARQLSuite';

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

sub BUILD {
	my $self	= shift;
	my $path	= File::Spec->catfile( $Bin, 'data', 'cdt' );
	$self->tests_dir($path);
}

my $load_ok	= can_load(modules => { 'AtteanX::Functions::CompositeLists' => 0, 'AtteanX::Functions::CompositeMaps' => 0 });
if ($load_ok) {
	AtteanX::Functions::CompositeLists->register();
	AtteanX::Functions::CompositeMaps->register();
	my %args	= (run_update_tests => 0, use_idp_planner => 0);
	while (defined(my $opt = shift)) {
		if ($opt eq '-v') {
			$args{debug}++;
			$args{results}++;
		} elsif ($opt eq '-r') {
			$args{results}++;
		} elsif ($opt eq '-s') { # summary
			$args{summary}++;
		} else {
			$args{pattern}	= $opt;
		}
	}
	run_me(\%args);
}

done_testing;

sub test_model {
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	return $model;
}
