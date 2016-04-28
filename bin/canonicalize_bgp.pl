#!/usr/bin/env perl

use v5.14;
use warnings;
no warnings 'once';
use autodie;
use File::Slurp;
use Scalar::Util qw(blessed);
use Attean;
use Attean::RDF;
use Attean::SimpleQueryEvaluator;
use Data::Dumper;
use Getopt::Long;
use Try::Tiny;
use Digest::SHA qw(sha1_hex);

if (scalar(@ARGV) < 1) {
	print STDERR <<"END";
Usage: $0 query.rq

Parses the supplied SPARQL query consisting of a simple BGP,
canonicalizes the BGP and emits a new query including a hash key for the
canonicalized query form and projection back to the original variable names.

END
	exit;
}

my $verbose	= 0;
my $debug	= 0;
my $result	= GetOptions ("verbose" => \$verbose, "debug" => \$debug);

my $qfile	= shift;

$Error::TypeTiny::StackTrace	= 1;
try {
	warn "Parsing query...\n" if ($verbose);
	my $sparql	= read_file($qfile);
	my $a		= Attean->get_parser('SPARQL')->parse($sparql);
	if ($debug) {
		warn "Walking algebra:\n";
		$a->walk( prefix => sub { my $a = shift; warn "- $a\n" });
	}
	
	my ($bgp)	= $a->subpatterns_of_type('Attean::Algebra::BGP');
	die "Query must be a simple BGP" unless ($bgp->isa('Attean::Algebra::BGP'));

	my ($canon, $mapping)	= $bgp->canonical_bgp_with_mapping();
	my $hash	= sha1_hex( join("\n", map { $_->tuples_string } (@{$canon->triples}) ) );
	my @proj	= sort map { sprintf("(?%s AS $_)", $mapping->{$_}{id}) } grep { $mapping->{$_}{type} eq 'variable' } (keys %$mapping);
	say "# Hash key: $hash";
	say "SELECT " . join(' ', @proj) . " WHERE {";
	foreach my $t (@{$canon->triples}) {
		say "\t" . $t->tuples_string;
	}
	say "}";
} catch {
	my $exception	= $_;
	warn "Caught error: $exception";
	warn $exception->stack_trace;
};
