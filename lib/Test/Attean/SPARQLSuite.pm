package Test::Attean::SPARQLSuite;

use v5.14;
use warnings;
use Test::Roo::Role;

use Attean;
use Attean::RDF;
use AtteanX::Parser::SPARQL;
use Attean::SimpleQueryEvaluator;
use Test::Attean::W3CManifestTestSuite;

use Carp;
use HTTP::Request;
use HTTP::Response;
use HTTP::Message::PSGI;
use Data::Dumper;
use Encode qw(encode encode_utf8);
use Getopt::Long;
use Module::Load::Conditional qw(can_load);
use Regexp::Common qw /URI/;
use Scalar::Util qw(blessed reftype);
use List::Util qw(all any);
use Test::More;
use Text::CSV;
use Try::Tiny;
use URI::file;
use File::Spec;
use Types::Standard qw(Str Bool ArrayRef HashRef InstanceOf ConsumerOf);
require XML::Simple;

my $XSD		= 'http://www.w3.org/2001/XMLSchema#';
my $RDF		= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
my $RDFS	= 'http://www.w3.org/2000/01/rdf-schema#';
my $RDFT	= 'http://www.w3.org/ns/rdftest#';
my $RS		= 'http://www.w3.org/2001/sw/DataAccess/tests/result-set#';
my $MF		= 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#';
my $UT		= 'http://www.w3.org/2009/sparql/tests/test-update#';
my $RQ		= 'http://www.w3.org/2001/sw/DataAccess/tests/test-query#';
my $DAWGT	= 'http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#';
my $FORMATS	= 'http://www.w3.org/ns/formats/';

my $direct			= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#DirectGraphIdentification');
my $indirect		= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#IndirectGraphIdentification');
my $post_creation	= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#POSTGraphCreation');

with 'Test::Attean::W3CManifestTestSuite';

sub manifest_paths {
	my $self	= shift;
	my $dir		= $self->tests_dir;
	unless (defined($dir)) {
		plan skip_all => "No manifest directory given";
		exit(0);
	}
	
	unless (-d $dir and -r $dir) {
		plan skip_all => "Manifest directory not readable: $dir";
		exit(1);
	}
	
	my $manifest	= File::Spec->catfile($dir, 'manifest.ttl');
	my $manifestall	= File::Spec->catfile($dir, 'manifest-all.ttl');
	my @manifests	= grep { -r $_ } ($manifest, $manifestall);
	unless (scalar(@manifests)) {
		die "No readable manifest: $dir\n";
	}
	return @manifests;
}

# Look up the test in the model, and determine:
# 
# * the test type (eval, syntax, prot, gsp, or sd)
# * whether the test is a query test
# * whether the test is an update test
# * whether the test is a negative test (implies a syntax test)
#
# If the test is not a recognized type, this method dies. Calling code is responsible
# for catching that error and properly recording a test failure.
sub query_test_type {
	my $self		= shift;
	my $model		= shift;
	my $test		= shift;
	
	my %types		= map { $_->value => $_ } $model->type_hierarchy( $test );
# 	warn $test->value . ' has types: ' . join(', ', keys %types);

	my $is_eval		= $types{ "${MF}TestEval" };
	my $is_syntax	= $types{ "${MF}TestSyntax" };
	my $is_prot		= $types{ "${MF}ProtocolTest" };
	my $is_gsp		= $types{ "${MF}GraphStoreProtocolTest" };
	my $is_sd		= $types{ "${MF}ServiceDescriptionTest" };
	
	my $is_query	= any { $types{ "${MF}$_" } or $types{ "${UT}$_" } } qw(QueryEvaluationTest PositiveSyntaxTest PositiveSyntaxTest11 NegativeSyntaxTest NegativeSyntaxTest11);
	my $is_update	= any { $types{ "${MF}$_" } or $types{ "${UT}$_" } } qw(UpdateEvaluationTest PositiveUpdateSyntaxTest PositiveUpdateSyntaxTest11 NegativeUpdateSyntaxTest NegativeUpdateSyntaxTest11);
	
	my $is_neg		= any { /Negative/ } keys %types;
	$is_syntax++	if ($is_neg); # to avoid having to do multiple subClassOf hops on these tests to find rdft:TextSyntax
	
	my $type;
	$type	= 'eval' if ($is_eval);
	$type	= 'syntax' if ($is_syntax);
	$type	= 'prot' if ($is_prot);
	$type	= 'gsp' if ($is_gsp);
	$type	= 'sd' if ($is_sd);

	unless (defined($type)) {
		my $types	= join(',', keys %types);
		die "Unhandled type for " . $test->value . " ; types=$types";
	}
	
	return ($type, $is_query, $is_update, $is_neg);
}

###############################################################################

Test::Roo::top_test 'SPARQL tests' => sub {
	my $self		= shift;
	my $PATTERN		= $self->pattern;
	my @manifests	= @{ $self->manifests };
	my $model		= $self->model;
	foreach my $m (@manifests) {
# 		warn "Manifest: " . $m->as_string . "\n" if ($self->debug);
		my $specVersion	= '1.1';
		my ($sv)	= $model->objects( $m, iri("${MF}specVersion") )->elements;
		if ($sv) {
			my $v	= $sv->value;
			if ($v =~ m<http://www.w3.org/ns/sparql#version-(.*)>) {
				$specVersion	= $1;
			} else {
				warn "*** Unexpected specVersion $v\n";
			}
		}
		my ($list)		= $model->objects( $m, iri("${MF}entries") )->elements;
		my @tests	= $model->get_list( $self->default_graph, $list )->elements;
		foreach my $test (@tests) {
			SKIP: {
				unless ($test->value =~ /$PATTERN/) {
					next;
				}
	
				my ($test_type, $is_query_test, $is_update_test, $is_neg)	= $self->query_test_type($model, $test);
				if ($is_query_test) {
					my ($name)	= $model->objects( $test, iri("${MF}name") )->elements;
					if ($self->run_query_tests) {
						if ($test_type eq 'eval') {
							# Evaluation Tests
							warn "### query eval test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
							$self->query_eval_test( $model, $test, specVersion => $specVersion );
						} elsif ($test_type eq 'syntax') {
							# Syntax Tests
							warn "### query syntax test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
							$self->sparql_syntax_test( 'query', $model, $test, specVersion => $specVersion );
						}
					}
					next;
				} elsif ($is_update_test) {
					my ($name)	= $model->objects( $test, iri("${MF}name") )->elements;
					if ($self->run_update_tests) {
						if ($test_type eq 'eval') {
							# Evaluation Tests
							warn "### update eval test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
							$self->update_eval_test( $model, $test, specVersion => $specVersion );
						} elsif ($test_type eq 'syntax') {
							# Syntax Tests
							warn "### query syntax test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
							$self->sparql_syntax_test( 'update', $model, $test, specVersion => $specVersion );
						}
					}
					next;
				} elsif ($test_type eq 'prot' or $test_type eq 'gsp') {
					unless (can_load( modules => { 'AtteanX::Endpoint' => 0, 'LWP::Protocol::PSGI' => 0 })) {
						$self->record_result('unhandled', 0, $test->as_string);
						my $type	= ($test_type eq 'prot') ? 'Protocol' : 'GSP';
						skip("${type} test is not handled (cannot load necessary modules; ensure AtteanX::Endpoint and LWP::Protocol::PSGI are available)");
						next;
					}
					
					if ($test_type eq 'gsp') {
						my (@requirements)		= $model->objects($test, iri("${MF}requires"))->elements;
						foreach my $r (@requirements) {
							skip("Not configured to run GSP direct-identification tests") if ($direct->equals($r) and not($self->run_gsp_direct));
							skip("Not configured to run GSP indirect-identification tests") if ($indirect->equals($r) and not($self->run_gsp_indirect));
							skip("Not configured to run GSP tests requiring POST to create fresh graph names") if ($post_creation->equals($r) and not($self->run_gsp_postcreate));
						}
					
					}
					
					subtest $test->value => sub {
						$self->sparql_protocol_test($test_type, $model, $test, specVersion => $specVersion);
					};
					next;
				} elsif ($test_type eq 'sd') {
					$self->record_result('unhandled', 0, $test->as_string);
					skip("Service Description test is not handled");
				}
	
				diag("Test is not handled: " . $test->value);
				$self->record_result('unhandled', 0, $test->as_string);
			}
		}
	}
};


1;
