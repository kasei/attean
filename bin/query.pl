#!/usr/bin/env perl

use v5.20;
use warnings;
use autodie;
use File::Slurp;
use Attean;
use Attean::RDF;
use RDF::Query;
use Data::Dumper;

if (scalar(@ARGV) != 2) {
	print STDERR <<"END";
Usage: $0 data.ttl query.rq

Uses RDF::Query to parse the supplied SPARQL query, translates the parsed query
form to an Attean::Algebra object, and executes it against a model containing
the RDF data parsed from the data file using Attean::SimpleQueryEvaluator.

END
	exit;
}

my $data	= shift;
my $qfile	= shift;
my $debug	= 0;

open(my $fh, '<:encoding(UTF-8)', $data);

my $store	= Attean->get_store('Memory')->new();
my $model	= Attean::MutableQuadModel->new( store => $store );
my $graph	= Attean::IRI->new('http://example.org/graph');

{
	my $parser	= Attean->get_parser('Turtle')->new();
	my $iter	= $parser->parse_iter_from_io($fh);
	my $quads	= $iter->as_quads($graph);
	$store->add_iter($quads);
}

if ($debug) {
	my $iter	= $model->get_quads();
	while (my $q = $iter->next) {
		say $q->as_string;
	}
}

my $sparql	= read_file($qfile);
my $query	= RDF::Query->new($sparql);
my $p		= $query->pattern;
my $a		= translate($p);

if ($debug) {
	warn Dumper($a);
}

my $e		= Attean::SimpleQueryEvaluator->new( model => $model );
my $iter	= $e->evaluate($a, $graph);
my $count	= 1;
while (my $r = $iter->next) {
	printf("%3d %s\n", $count++, $r->as_string);
}

sub translate {
	my $a	= shift;
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
		while (scalar(@p) > 1) {
			my ($l, $r)	= splice(@p, 0, 2);
			unshift(@p, Attean::Algebra::Join->new( children => [$l, $r] ));
		}
		return shift(@p);
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
	} elsif ($a->isa('')) {
	} elsif ($a->isa('')) {
	} elsif ($a->isa('')) {
	}
	die "Unrecognized algebra " . ref($a);
}