#!/usr/bin/env perl

use v5.20;
use warnings;
no warnings 'once';
use autodie;
use File::Slurp;
use Scalar::Util qw(blessed);
use Attean;
use Attean::RDF;
use RDF::Query;
use Data::Dumper;
use Getopt::Long;
use Try::Tiny;
use Benchmark qw(timethese);

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

open(my $fh, '<:encoding(UTF-8)', $data);

$Error::TypeTiny::StackTrace	= 1;
try {
	warn "Constructing model...\n" if ($verbose);
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= Attean::IRI->new('http://example.org/graph');

	{
		warn "Parsing data...\n" if ($verbose);
		my $parser	= Attean->get_parser('Turtle')->new();
		my $iter	= $parser->parse_iter_from_io($fh);
		my $quads	= $iter->as_quads($graph);
		$store->add_iter($quads);
		$store->add_quad(quad(iri('s'), iri('p'), iri('o'), iri('graph1')));
		$store->add_quad(quad(iri('s'), iri('p'), iri('o'), iri('graph2')));
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
	my $p		= $query->pattern;

	warn "Translating query...\n" if ($verbose);
	my $a		= translate($p);
	if ($debug) {
		warn "Walking algebra:\n";
		$a->walk( prefix => sub { my $a = shift; warn "- $a\n" });
	}
	
	if ($debug) {
		warn Dumper($a);
	}

	warn "Evaluating query...\n" if ($verbose);
	my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $graph );
	if ($benchmark) {
		timethese(5, {
			'baseline'	=> sub { local($ENV{ATTEAN_NO_MERGE_JOIN}) = 1; my @e = $e->evaluate($a, $graph)->elements },
			'mergejoin'	=> sub { my @e = $e->evaluate($a, $graph)->elements },
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

sub translate {
	my $a	= shift;
	die "Not a reference? " . Dumper($a) unless blessed($a);
	if ($a->isa('RDF::Query::Algebra::Project')) {
		my $p	= $a->pattern;
		my $v	= $a->vars;
		my @vars	= map { variable($_->name) } @$v;
		return Attean::Algebra::Project->new(
			children	=> [ translate($p) ],
			variables	=> [ @vars ],
		);
	} elsif ($a->isa('RDF::Query::Algebra::GroupGraphPattern')) {
		my @p	= map { translate($_) } $a->patterns;
		if (scalar(@p) == 0) {
			return Attean::Algebra::BGP->new();
		} else {
			while (scalar(@p) > 1) {
				my ($l, $r)	= splice(@p, 0, 2);
				unshift(@p, Attean::Algebra::Join->new( children => [$l, $r] ));
			}
			return shift(@p);
		}
	} elsif ($a->isa('RDF::Query::Algebra::BasicGraphPattern')) {
		my @p	= map { translate($_) } $a->triples;
		return Attean::Algebra::BGP->new( triples => \@p );
	} elsif ($a->isa('RDF::Query::Algebra::Triple')) {
		my @nodes	= map { translate($_) } $a->nodes;
		return Attean::TriplePattern->new(@nodes);
	} elsif ($a->isa('RDF::Query::Node::Variable')) {
		return variable($a->name);
	} elsif ($a->isa('RDF::Query::Node::Resource')) {
		return iri($a->uri_value);
	} elsif ($a->isa('RDF::Query::Algebra::Limit')) {
		my $child	= $a->pattern;
		if ($child->isa('RDF::Query::Algebra::Offset')) {
			my $p	= translate($child->pattern);
			return Attean::Algebra::Slice->new( children => [$p], limit => $a->limit, offset => $child->offset );
		} else {
			my $p	= translate($child);
			return Attean::Algebra::Slice->new( children => [$p], limit => $a->limit );
		}
	} elsif ($a->isa('RDF::Query::Algebra::Offset')) {
		my $p	= translate($a->pattern);
		return Attean::Algebra::Slice->new( children => [$p], offset => $a->offset );
	} elsif ($a->isa('RDF::Query::Algebra::Path')) {
		my $s		= translate($a->start);
		my $o		= translate($a->end);
		my $path	= translate_path($a->path);
		return Attean::Algebra::Path->new( subject => $s, path => $path, object => $o );
	} elsif ($a->isa('RDF::Query::Algebra::NamedGraph')) {
		my $graph	= translate($a->graph);
		my $p		= translate($a->pattern);
		return Attean::Algebra::Graph->new( children => [$p], graph => $graph );
	} elsif ($a->isa('')) {
	}
	die "Unrecognized algebra " . ref($a);
}

sub translate_path {
	my $path	= shift;
	my ($op, @args)	= @$path;
	if ($op eq '!') {
		my @nodes	= map { translate($_) } @args;
		return Attean::Algebra::NegatedPropertySet->new( predicates => \@nodes );
	} elsif ($op eq '/') {
		my @paths	= map { translate($_) } @args;
		foreach (@paths) {
			if ($_->does('Attean::API::IRI')) {
				$_	= Attean::Algebra::PredicatePath->new( predicate => $_ );
			}
		}
		return Attean::Algebra::SequencePath->new( children => \@paths );
	} elsif ($op eq '^') {
		my $path	= translate(shift(@args));
		if ($path->does('Attean::API::IRI')) {
			$path	= Attean::Algebra::PredicatePath->new( predicate => $path );
		}
		return Attean::Algebra::InversePath->new( children => [$path] );
	} elsif ($op eq '|') {
		my @paths	= map { translate($_) } @args;
		foreach (@paths) {
			if ($_->does('Attean::API::IRI')) {
				$_	= Attean::Algebra::PredicatePath->new( predicate => $_ );
			}
		}
		return Attean::Algebra::AlternativePath->new( children => \@paths );
	}
	die "Unrecognized path: $op";
}