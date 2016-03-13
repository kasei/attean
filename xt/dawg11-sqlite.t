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
use File::Slurp;
use File::Temp qw(tempfile);
use List::MoreUtils qw(all);
use DBIx::MultiStatementDo;
use Types::Standard qw(Str ArrayRef);
use Class::Method::Modifiers;

with 'Test::Attean::SPARQLSuite';

has 'test_files'	=> (is => 'rw', isa => ArrayRef, default => sub { [] });
has 'file'			=> (is => 'rw', isa => Str);

my %args;
while (defined(my $opt = shift)) {
	if ($opt eq '-v') {
		$args{debug}++;
	} else {
		$args{pattern}	= $opt;
	}
}

around 'setup' => sub {
	my $orig	= shift;
	my $self	= shift;
	(undef, my $filename)	= tempfile();
# 	warn $filename;
	push(@{ $self->test_files }, $filename);
	$self->file($filename);

	my @connect		= AtteanX::Store::DBI->dbi_connect_args(
		'sqlite',
		database	=> $filename,
	);
	my $dbh		= DBI->connect(@connect);
	my $batch	= DBIx::MultiStatementDo->new( dbh => $dbh );
	my $store	= Attean->get_store('DBI')->new( dbh => $dbh );

	if (my $file = $store->create_schema_file) {
		my $sql	= read_file($file);
		$batch->do($sql);
	} else {
		plan skip_all => "No schema files available for SQLite";
		exit(0);
	}

	return $orig->($self, @_);
};

after 'teardown' => sub {
	my $self	= shift;
# 	unlink($self->file);
};

sub test_model {
	my $self	= shift;
	my @connect	= AtteanX::Store::DBI->dbi_connect_args('sqlite', database => $self->file);
	my $dbh		= DBI->connect(@connect);
	my $store	= Attean->get_store('DBI')->new( dbh => $dbh );
	foreach my $g ($store->get_graphs->elements) {
		$store->drop_graph($g);
	}
	my $model	= Attean::MutableQuadModel->new( store => $store );
	return $model;
}

run_me(\%args);

done_testing;

