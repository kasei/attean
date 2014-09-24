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
use RDF::Query;
use Data::Dumper;
use Getopt::Long;
use Try::Tiny;
use Digest::SHA qw(sha1_hex);

if (scalar(@ARGV) < 1) {
	print STDERR <<"END";
Usage: $0 query.rq

Uses RDF::Query to parse the supplied SPARQL query consisting of a simple BGP,
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
	
	my ($bgp)	= @{ $a->children };
	die "Query must be a simple BGP" unless ($a->isa('Attean::Algebra::Project') and $bgp->isa('Attean::Algebra::BGP'));

	my ($canon, $mapping)	= $bgp->canonical_bgp_with_mapping();
	my $hash	= sha1_hex( join("\n", map { $_->tuples_string } (@{$canon->triples}) ) );
	my @proj	= sort map { sprintf("(?v_%03d AS $_)", $mapping->{$_}{id}) } grep { $mapping->{$_}{type} eq 'variable' } (keys %$mapping);
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
	} elsif ($a->isa('RDF::Query::Node::Blank')) {
		return blank($a->blank_identifier);
	} elsif ($a->isa('RDF::Query::Node::Literal')) {
		if ($a->has_language) {
			return langliteral($a->literal_value, $a->literal_value_language);
		} elsif ($a->has_datatype) {
			return dtliteral($a->literal_value, $a->literal_datatype);
		} else {
			return literal($a->literal_value);
		}
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
