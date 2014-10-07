#!/usr/bin/env perl

use v5.20;
use warnings;
no warnings 'once';
use autodie;
use File::Slurp;
use Scalar::Util qw(blessed);
use Attean;
use Attean::RDF;
use Attean::SimpleQueryEvaluator;
use AtteanX::RDFQueryTranslator;
use RDF::Query;
use Data::Dumper;
use Getopt::Long;
use Try::Tiny;
use Benchmark qw(timethese);

BEGIN { $Error::TypeTiny::StackTrace	= 1; }

if (scalar(@ARGV) < 2) {
	print STDERR <<"END";
Usage: $0 data.ttl query.rq

Uses RDF::Query to parse the supplied SPARQL query, translates the parsed query
form to an Attean::Algebra object, and executes it against a model containing
the RDF data parsed from the data file using Attean::SimpleQueryEvaluator.

END
	exit;
}

my $verbose	= 0;
my $debug	= 0;
my $benchmark	= 0;
my $result	= GetOptions ("verbose" => \$verbose, "debug" => \$debug, "benchmark" => \$benchmark);

my $data	= shift;
my $qfile	= shift;
my $base	= Attean::IRI->new('file://' . File::Spec->rel2abs($data));

open(my $fh, '<:encoding(UTF-8)', $data);

try {
	warn "Constructing model...\n" if ($verbose);
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= Attean::IRI->new('http://example.org/graph');

	{
		warn "Parsing data...\n" if ($verbose);
		my $pclass	= Attean->get_parser( filename => $data ) // 'AtteanX::Parser::Turtle';
		my $parser	= $pclass->new(base => $base);
		my $iter	= $parser->parse_iter_from_io($fh);
		my $quads	= $iter->as_quads($graph);
		$model->add_iter($quads);
	}

	if ($debug) {
		my $iter	= $model->get_quads();
		while (my $q = $iter->next) {
			say $q->as_string;
		}
	}

	warn "Parsing query...\n" if ($verbose);
	my $sparql	= read_file($qfile);
	my $query	= RDF::Query->new($sparql);
	unless ($query) {
		die RDF::Query->error;
	}

	warn "Translating query...\n" if ($verbose);
	my $t	= AtteanX::RDFQueryTranslator->new();
	my $a	= $t->translate_query($query);
	if ($debug) {
		warn "Walking algebra:\n";
		warn $a->as_string;
	}
	
	warn "Evaluating query...\n" if ($verbose);
	my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $graph );
	if ($benchmark) {
		timethese(5, {
			'baseline'	=> sub { my @e = $e->evaluate($a, $graph)->elements },
		});
	} else {
		my $iter	= $e->evaluate($a, $graph);
		my $count	= 1;
		while (my $r = $iter->next) {
			printf("%3d %s\n", $count++, $r->as_string);
		}
	}
} catch {
	my $exception	= $_;
	warn "Caught error: $exception";
	warn $exception->stack_trace;
};
