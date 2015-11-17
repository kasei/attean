#!/usr/bin/perl

use strict;
use warnings;
no warnings 'redefine';

BEGIN {
	no warnings 'once';
	$Error::TypeTiny::StackTrace	= 1;
}

use v5.14;
use warnings;
no warnings 'once';
use autodie;
use AtteanX::Parser::SPARQL;
use Algorithm::Combinatorics qw(permutations);
use Benchmark qw(timethese);
use Data::Dumper;
use Encode qw(encode);
use File::Slurp;
use File::Temp qw(tempfile);
use Getopt::Long;
use LWP::MediaTypes qw(add_type);
use Regexp::Common qw /URI/;
use Scalar::Util qw(blessed reftype);
use Storable qw(dclone);
use Test::More;
use Text::CSV;
use Try::Tiny;
use URI::file;

add_type( 'application/rdf+xml' => qw(rdf xrdf rdfx) );
add_type( 'text/turtle' => qw(ttl) );
add_type( 'text/plain' => qw(nt) );
add_type( 'text/x-nquads' => qw(nq) );
add_type( 'text/json' => qw(json) );
add_type( 'text/html' => qw(html xhtml htm) );

use Attean;
use Attean::RDF;
use Attean::SimpleQueryEvaluator;
use AtteanX::RDFQueryTranslator;

use RDF::Query;
use RDF::Trine qw(statement);
use RDF::Trine::Graph;
use RDF::Trine::Namespace qw(rdf rdfs xsd);
use RDF::Trine::Iterator qw(smap);

use Carp;
use HTTP::Request;
use HTTP::Response;
use HTTP::Message::PSGI;

our $RUN_UPDATE_TESTS	= 0;
our $RUN_QUERY_TESTS	= 1;
our $debug				= 0;
our $STRICT_APPROVAL	= 0;
our $USE_IDP_PLANNER	= 1;

require XML::Simple;

my $PATTERN	= '';
my %args;

our $RUN_TESTS	= 1;
our $LIST_TESTS	= 0;

while (defined(my $opt = shift)) {
	if ($opt eq '-v') {
		$debug++;
	} elsif ($opt =~ /^-(.*)$/) {
		if ($1 eq 'list') {
			$RUN_TESTS	= 0;
			$LIST_TESTS	= 1;
		} elsif ($1 =~ 'stress=(\d+)') {
			$args{ 'stress' }	= $1;
		} else {
			$args{ $1 }	= 1;
		}
	} else {
		$PATTERN	= $opt;
	}
}

$ENV{RDFQUERY_THROW_ON_SERVICE}	= 1;

no warnings 'once';

if ($PATTERN) {
# 	$debug			= 1;
}

warn "PATTERN: ${PATTERN}\n" if ($PATTERN and $debug);

sub memory_model {
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	return $model;
}

my $model	= memory_model();
my @files;
if ($RUN_QUERY_TESTS) {
	push(@files, qw(
		aggregates
		bind
		bindings
		construct
		csv-tsv-res
		exists
		functions
		grouping
		json-res
		negation
		project-expression
		property-path
		subquery
	));
#		service
}
if ($RUN_UPDATE_TESTS) {
	push(@files, qw(
		add
		basic-update
		clear
		copy
		delete
		delete-data
		delete-insert
		delete-where
		drop
		move
		update-silent
	));
}
my @manifests	= grep { -r $_ } map { glob( "xt/dawg11/$_/manifest.ttl" ) } @files;
if (scalar(@manifests)) {
	plan qw(no_plan);
} else {
	plan skip_all => 'No manifest files found in xt/dawg11';
	exit(0);
}

my $class	= Attean->get_parser("turtle") || die "Failed to load parser for 'turtle'";
my $default_graph	= iri('http://graph/');
foreach my $file (@manifests) {
	my $path	= File::Spec->rel2abs($file);
	my $base	= iri("file://$path");
	my $parser	= $class->new( base => $base ) || die "Failed to construct parser for 'turtle'";
# 	warn "Parsing manifest $file\n" if $debug;
	open(my $fh, '<:utf8', $file);
	my $iter	= $parser->parse_iter_from_io($fh);
	my $quads	= $iter->as_quads($default_graph);
	$model->add_iter($quads);
}
warn "done parsing manifests" if $debug;

my $rs		= RDF::Trine::Namespace->new('http://www.w3.org/2001/sw/DataAccess/tests/result-set#');
my $mf		= RDF::Trine::Namespace->new('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#');
my $ut		= RDF::Trine::Namespace->new('http://www.w3.org/2009/sparql/tests/test-update#');
my $rq		= RDF::Trine::Namespace->new('http://www.w3.org/2001/sw/DataAccess/tests/test-query#');
my $dawgt	= RDF::Trine::Namespace->new('http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#');

{
	my @manifests	= $model->subjects( iri($rdf->type->uri_value), iri($mf->Manifest->uri_value) )->elements;
	foreach my $m (@manifests) {
# 		warn "Manifest: " . $m->as_string . "\n" if ($debug);
		my ($list)	= $model->objects( $m, iri($mf->entries->uri_value) )->elements;
		unless (blessed($list)) {
			warn "No mf:entries found for manifest " . $m->as_string . "\n";
		}
		my @tests	= $model->get_list( $default_graph, $list )->elements;
		foreach my $test (@tests) {
			unless ($test->value =~ /$PATTERN/) {
				next;
			}
			if ($LIST_TESTS) {
				say $test->value;
			}
			if ($RUN_TESTS) {
				if ($RUN_QUERY_TESTS) {
					my $et	= $model->count_quads($test, iri($rdf->type->uri_value), iri($mf->QueryEvaluationTest->uri_value));
					my $ct	= $model->count_quads($test, iri($rdf->type->uri_value), iri($mf->CSVResultFormatTest->uri_value));
					if ($et + $ct) {
						my ($name)	= $model->objects( $test, iri($mf->name->uri_value) )->elements;
						warn "### query eval test: " . $test->as_string . " >>> " . $name->value . "\n" if ($debug);
						my $count	= $args{ stress } || 1;
						query_eval_test( $model, $test, $count );
					}
				}
			
				if ($RUN_UPDATE_TESTS) {
					if ($model->count_quads($test, iri($rdf->type->uri_value), iri($ut->UpdateEvaluationTest->uri_value)) or $model->count_statements($test, iri($rdf->type->uri_value), iri($mf->UpdateEvaluationTest->uri_value))) {
						my ($name)	= $model->objects( $test, iri($mf->name->uri_value) );
						unless ($test->value =~ /$PATTERN/) {
							next;
						}
# 						warn "### update eval test: " . $test->as_string . " >>> " . $name->value . "\n" if ($debug);
# 						update_eval_test( $model, $test );
					}
				}
			}
		}
	}
}

sub query_eval_test {
	my $model		= shift;
	my $test		= shift;
	my $count		= shift || 1;
	
	my ($action)	= $model->objects( $test, iri($mf->action->uri_value) )->elements;
	my ($result)	= $model->objects( $test, iri($mf->result->uri_value) )->elements;
	my ($req)		= $model->objects( $test, iri($mf->requires->uri_value) )->elements;
	my ($approved)	= $model->objects( $test, iri($dawgt->approval->uri_value) )->elements;
	my ($queryd)	= $model->objects( $action, iri($rq->query->uri_value) )->elements;
	my ($data)		= $model->objects( $action, iri($rq->data->uri_value) )->elements;
	my @gdata		= $model->objects( $action, iri($rq->graphData->uri_value) )->elements;
	my @sdata		= $model->objects( $action, iri($rq->serviceData->uri_value) )->elements;
	
	if ($STRICT_APPROVAL) {
		unless ($approved) {
			warn "- skipping test because it isn't approved\n" if ($debug);
			return;
		}
		if ($approved->equal($dawgt->NotClassified)) {
			warn "- skipping test because its approval is dawgt:NotClassified\n" if ($debug);
			return;
		}
	}
	
	my $uri					= URI->new( $queryd->value );
	my $filename			= $uri->file;
	my (undef,$base,undef)	= File::Spec->splitpath( $filename );
	$base					= "file://${base}";
	warn "Loading SPARQL query from file $filename" if ($debug);
	my $sparql				= do { local($/) = undef; open(my $fh, '<', $filename) or do { warn("$!: $filename; " . $test->as_string); return }; binmode($fh, ':utf8'); <$fh> };
	
	my $q			= $sparql;
	$q				=~ s/\s+/ /g;
	if ($debug) {
		warn "### test     : " . $test->as_string . "\n";
		warn "# sparql     : $q\n";
		warn "# data       : " . ($data->as_string =~ s#file://##r) if (blessed($data));
		warn "# graph data : " . ($_->as_string =~ s#file://##r) for (@gdata);
		warn "# result     : " . ($result->as_string =~ s#file://##r);
		warn "# requires   : " . ($req->as_string =~ s#file://##r) if (blessed($req));
	}
	
STRESS:	foreach (1 .. $count) {
		print STDERR "constructing model... " if ($debug);
		my $test_model	= memory_model();
		try {
			if (blessed($data)) {
	# 			warn "*********************** " . Dumper($data->value);
				add_to_model( $test_model, $default_graph, $data->value );
			}
			foreach my $g (@gdata) {
	# 			warn "***** graph: " . $g->as_string . "\n";
				add_to_model( $test_model, $g, $g->value );
			}
		} catch {
			fail($test->as_string);
			record_result(0, $test->as_string);
			print "# died: " . $test->as_string . ": $_\n";
			next STRESS;
		};
		print STDERR "ok\n" if ($debug);
	
		my $resuri		= URI->new( $result->value );
		my $resfilename	= $resuri->file;
	
		TODO: {
			local($TODO)	= (blessed($req)) ? "requires " . $req->as_string : '';
			my $comment;
			my $ok	= try {
				if ($debug) {
					my $q	= $sparql;
					$q		=~ s/([\x{256}-\x{1000}])/'\x{' . sprintf('%x', ord($1)) . '}'/eg;
					warn $q;
				}
				
				my ($actual, $type);
				{
					local($::DEBUG)	= 1;
					print STDERR "getting actual results... " if ($debug);
					($actual, $type)		= get_actual_results( $test_model, $sparql, $base );
					print STDERR "ok\n" if ($debug);
				}
			
				print STDERR "getting expected results... " if ($debug);
				my $expected	= get_expected_results( $resfilename, $type );
				print STDERR "ok\n" if ($debug);
			
			#	warn "comparing results...";
				compare_results( $expected, $actual, $test->as_string, \$comment );
			} catch {
				if (ref($_)) {
					warn $_->message;
					warn $_->stack_trace;
				} else {
					warn $_;
				}
				fail($test->as_string);
				record_result(0, $test->as_string);
			};
			warn $@ if ($@);
			if ($ok) {
			} else {
				print "# failed: " . $test->as_string . "\n";
			}
		}
	}
}


exit;

######################################################################


sub add_to_model {
	my $model	= shift;
	my $graph	= shift;
	my @files	= @_;
	
	foreach my $file (@files) {
		$file	=~ s#^file://##;
		try {
			my $path	= File::Spec->rel2abs($file);
			my $base	= iri("file://$path");
			my $iter;
			open(my $fh, '<:utf8', $file) or die $!;
			my $format;
			$format	= 'turtle' if ($file =~ /[.]ttl$/);
			$format	= 'rdfxml' if ($file =~ /[.]rdf$/);
			$format	= 'ntriples' if ($file =~ /[.]nt$/);
			if ($format) {
				my $format	= ($file =~ /[.]ttl/) ? "turtle" : "rdfxml";
				my $class	= Attean->get_parser($format) || die "Failed to load parser for '$format'";
				my $parser	= $class->new( base => $base ) || die "Failed to construct parser for '$format'";
				$iter	= $parser->parse_iter_from_io($fh);
			} else {
				die "Unrecognized file extension: $file";
			}
			my $quads	= $iter->as_quads($graph);
			$model->add_iter($quads);
		} catch {
			warn "Failed to load $file into model: $_";
			if (ref($_)) {
				warn $_->stack_trace;
			}
		};
	}
}

sub get_actual_results {
	my $model		= shift;
	my $sparql		= shift;
	my $base		= shift;

	my $s 			= AtteanX::Parser::SPARQL->new(base => $base);
	my ($algebra)	= $s->parse_list_from_bytes($sparql);
	
	if ($debug) {
		warn "Walking algebra:\n";
		warn $algebra->as_string;
	}
	
	if ($debug) {
		my $iter	= $model->get_quads;
		warn "Dataset:\n-------------\n";
		while (my $q = $iter->next) {
			say $q->as_string;
		}
		warn "-------------\n";
	}
	
	my $testns	= 'http://example.com/test-results#';
	my $rmodel	= memory_model();

	my $results;
	if ($USE_IDP_PLANNER) {
		my $default_graphs	= [$default_graph];
		my $planner	= Attean::IDPQueryPlanner->new();
		my $plan	= $planner->plan_for_algebra($algebra, $model, $default_graphs);
		if ($debug) {
			warn "Walking plan:\n";
			warn $plan->as_string;
		}
		$results	= $plan->evaluate($model);
	} else {
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $default_graph );
		$results	= $e->evaluate($algebra, $default_graph);
	}
	my $count	= 1;
	
	$results	= $results->materialize;
	my $item	= $results->peek;
	
	my $type	= 'bindings';
	if ($item) {
		if ($item->does('Attean::API::Triple')) {
			$type	= 'graph';
		} elsif ($item->does('Attean::API::Term')) {
			$type	= 'boolean';
		}
	}
	
	print_results("Actual results", \$results) if ($args{ results });
	return ($results, $type);
	
	# TODO:
	if ($results->is_bindings) {
		return ($results, 'bindings');
	} elsif ($results->is_boolean) {
		$rmodel->add_statement( statement( iri("${testns}result"), iri("${testns}boolean"), literal(($results->get_boolean ? 'true' : 'false'), undef, $xsd->boolean) ) );
		return ($rmodel->get_statements, 'boolean');
	} elsif ($results->is_graph) {
		return ($results, 'graph');
	} else {
		warn "unknown result type: " . Dumper($results);
	}
}

sub print_results {
	my $name	= shift;
	my $results	= shift;
	$$results	= $$results->materialize;
	warn "$name:\n";
	my $count	= 1;
	while (my $r = $$results->next) {
		printf("%3d %s\n", $count++, $r->as_string);
	}
	$$results->reset;
}

sub get_expected_results {
	my $file		= shift;
	my $type		= shift;
	
	my $testns	= RDF::Trine::Namespace->new('http://example.com/test-results#');
	if ($type eq 'graph') {
		my $model	= memory_model();
		add_to_model($model, $default_graph, $file);
		my $results	= $model->get_quads->map(sub { shift->as_triple }, 'Attean::API::Triple');
		print_results("Expected results", \$results) if ($args{ results });
		return $results;
	} elsif ($file =~ /[.](srj|json)/) {
		my $model	= memory_model();
		open(my $fh, '<', $file) or die $!;
		my $parser	= Attean->get_parser('SPARQLJSON')->new();
		my $results	= $parser->parse_iter_from_io($fh)->materialize;
		my $item	= $results->peek;
		if ($item->does('Attean::API::Term')) {
			if ($args{ results }) {
				warn "Expected result: " . $item->as_string . "\n";
			}
			return $results;
		} else {
			print_results("Expected results", \$results) if ($args{ results });
			return $results;
		}
	} elsif ($file =~ /[.]srx/) {
		my $model	= memory_model();
		my $parser	= Attean->get_parser('sparqlxml')->new();
		open(my $fh, '<:encoding(UTF-8)', $file);
		my $results	= $parser->parse_iter_from_io($fh);
		
		print_results("Expected results", \$results) if ($args{ results });
		return $results;
	} elsif ($file =~ /[.]csv/) {
		my $csv	= Text::CSV->new({binary => 1});
		open( my $fh, "<:encoding(utf8)", $file ) or die $!;
		my $header	= $csv->getline($fh);
		my @vars	= @$header;
		my @data;
		while (my $row = $csv->getline($fh)) {
			my %result;
			foreach my $i (0 .. $#vars) {
				my $var		= $vars[$i];
				my $value	= $row->[ $i ];
				# XXX @@ heuristics that won't always work.
				# XXX @@ expected to work on the test suite, though
				if ($value =~ /^_:(\w+)$/) {
					$value	= blank($1);
				} elsif ($value =~ /$RE{URI}/) {
					$value	= iri($value);
				} elsif (defined($value) and length($value)) {
					$value	= literal($value);
				}
				if (ref($value)) {
					$result{ $var }	= $value;
				}
			}
			push(@data, Attean::Result->new( bindings => \%result ));
		}
		my $results	= Attean::ListIterator->new(values => \@data, item_type => 'Attean::API::Result');
		print_results("Expected results", \$results) if ($args{ results });
		return $results;
	} elsif ($file =~ /[.]tsv/) {
		my $parser	= Attean->get_parser('SPARQLTSV')->new();
		open( my $fh, "<:encoding(utf8)", $file ) or die $!;
		my $iter	= $parser->parse_iter_from_io($fh);
		return $iter;
	} elsif ($file =~ /[.](ttl|rdf|nt)/) {
		my $model	= memory_model();
		add_to_model($model, $default_graph, $file);
		my ($res)	= $model->subjects( iri($rdf->type->uri_value), iri($rs->ResultSet->uri_value) )->elements;
		if (my($b) = $model->objects( $res, iri($rs->boolean->uri_value) )->elements) {
			my $bool	= $b->value;
			my $term	= literal(value => $bool, datatype => $xsd->boolean->uri_value);
			if ($args{ results }) {
				warn "Expected result: " . $term->as_string . "\n";
			}
			return Attean::ListIterator->new(values => [$term], item_type => 'Attean::API::Term');
		} else {
			my @vars	= $model->objects( $res, iri($rs->resultVariable->uri_value) )->elements;
			my @sols	= $model->objects( $res, iri($rs->solution->uri_value) )->elements;
			my @names	= map { $_->value } @vars;
			my @bindings;
			foreach my $r (@sols) {
				my %data;
				my @b	= $model->objects( $r, iri($rs->binding->uri_value) );
				foreach my $b (@b) {
					my ($value)	= $model->objects( $b, iri($rs->value->uri_value) );
					my ($var)	= $model->objects( $b, iri($rs->variable->uri_value) );
					$data{ $var->value }	= $value;
				}
				push(@bindings, Attean::Result->new( bindings => \%data ));
			}
			my $results	= Attean::ListIterator->new(values => \@bindings, item_type => 'Attean::API::Result');
			print_results("Expected results", \$results) if ($args{ results });
			return $results;
		}
	} else {
		die "Unrecognized type of expected results: $file";
	}
}

sub compare_results {
	my $expected	= shift->canonicalize->materialize;
	my $actual		= shift->canonicalize->materialize;
	my $test		= shift;
	my $comment		= shift || do { my $foo; \$foo };
	my $TODO		= shift;
	
	if ($actual->does('Attean::API::ResultIterator') or $actual->does('Attean::API::TripleIterator')) {
		my $eqtest	= Attean::BindingEqualityTest->new();
		if ($test =~ /csv0/) {
			# CSV is a lossy format, so strip the languages and datatypes off of literals in the actual results (so that they'll match up with the (lossy) expected results
			my $mapper	= Attean::TermMap->new(mapper => sub {
				my $term	= shift;
				if ($term->does('Attean::API::Literal')) {
					return Attean::Literal->new(value => $term->value);
				}
				return $term;
			});
			$actual	= $actual->map($mapper->binding_mapper);
		}

		my $ok		= ok( $eqtest->equals( $actual, $expected ), $test ) or diag($eqtest->error);
		record_result($ok, $test);
		return $ok;
	} elsif ($actual->does('Attean::API::TermIterator')) {
		my $a	= $actual->next;
		my $e	= $expected->next;
		my $ok		= ok( $a->equals($e), sprintf("$test: %s == %s", $a->as_string, $e->as_string) );
		record_result($ok, $test);
		return $ok;
	} else {
		die "Unexpected result type $actual";
	}
}

{
	my @failures;
	sub record_result {
		my $ok		= shift;
		my $name	= shift;
		unless ($ok) {
			push(@failures, $name);
		}
	}
	END {
		if ($RUN_QUERY_TESTS and scalar(@failures)) {
			@failures	= sort @failures;
			my $msg	= "Failing tests: " . Dumper([@failures]);
			warn $msg;
			unless ($PATTERN) {
				open(my $fh, '>', sprintf('.sparql-test-suite-%d', scalar(time)));
				say $fh join("\n", @failures);
			}
		}
	}
}