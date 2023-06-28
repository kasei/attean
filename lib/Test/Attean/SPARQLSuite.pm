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
use Regexp::Common qw /URI/;
use Scalar::Util qw(blessed reftype);
use List::MoreUtils qw(all);
use Test::Modern;
use Text::CSV;
use Try::Tiny;
use URI::file;
use File::Spec;
use Types::Standard qw(Str Bool ArrayRef HashRef InstanceOf ConsumerOf);
require XML::Simple;

my $XSD		= 'http://www.w3.org/2001/XMLSchema#';
my $RDF		= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
my $RDFS	= 'http://www.w3.org/2000/01/rdf-schema#';
my $RS		= 'http://www.w3.org/2001/sw/DataAccess/tests/result-set#';
my $MF		= 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#';
my $UT		= 'http://www.w3.org/2009/sparql/tests/test-update#';
my $RQ		= 'http://www.w3.org/2001/sw/DataAccess/tests/test-query#';
my $DAWGT	= 'http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#';

with 'Test::Attean::W3CManifestTestSuite';

sub manifest_paths {
	my $self	= shift;
	my @files;
	if ($self->run_query_tests) {
		push(@files, qw(
			aggregates
			bind
			cast
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

		push(@files, qw(
			aggregates
			construct
			delete-insert
			grouping
			syntax-query
			syntax-fed
			syntax-update-1
			syntax-update-2
		));
	}
	if ($self->run_update_tests) {
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
	
	my $dir		= $self->tests_dir;
	unless (defined($dir)) {
		plan skip_all => "No manifest directory given";
		exit(0);
	}
	
	unless (-d $dir and -r $dir) {
		plan skip_all => "Manifest directory not readable: $dir";
		exit(0);
	}
	
	my @manifests	= grep { -r $_ } map { File::Spec->catfile($dir, $_, 'manifest.ttl') } @files;
}

###############################################################################


Test::Roo::top_test 'SPARQL 1.1 tests' => sub {
	my $self		= shift;
	my $PATTERN		= $self->pattern;
	my @manifests	= @{ $self->manifests };
	my $model		= $self->model;
	foreach my $m (@manifests) {
# 		warn "Manifest: " . $m->as_string . "\n" if ($self->debug);
		my ($list)	= $model->objects( $m, iri("${MF}entries") )->elements;
		unless (blessed($list)) {
			warn "No mf:entries found for manifest " . $m->as_string . "\n" if ($self->debug);
		}
		my @tests	= $model->get_list( $self->default_graph, $list )->elements;
		foreach my $test (@tests) {
			unless ($test->value =~ /$PATTERN/) {
				next;
			}
# 			if ($LIST_TESTS) {
# 				say $test->value;
# 			}
			if ($self->run_query_tests) {
				{
					# Evaluation Tests
					my $et	= $model->count_quads($test, iri("${RDF}type"), iri("${MF}QueryEvaluationTest"));
					my $ct	= $model->count_quads($test, iri("${RDF}type"), iri("${MF}CSVResultFormatTest"));
					if ($et + $ct) {
						my ($name)	= $model->objects( $test, iri("${MF}name") )->elements;
						warn "### query eval test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
						$self->query_eval_test( $model, $test );
					}
				}
				
				{
					# Syntax Tests
					my $total	= 0;
					foreach my $type (qw(PositiveSyntaxTest11 NegativeSyntaxTest11)) {
						$total	+= $model->count_quads($test, iri("${RDF}type"), iri("${MF}$type"));
					}

					if ($total) {
						my ($name)	= $model->objects( $test, iri("${MF}name") )->elements;
						warn "### query syntax test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
						$self->syntax_test( 'query', $model, $test );
					}
				}
			}
			
			if ($self->run_update_tests) {
				{
					# Evaluation Tests
					if ($model->count_quads($test, iri("${RDF}type"), iri("${UT}UpdateEvaluationTest")) or $model->count_quads($test, iri("${RDF}type"), iri("${MF}UpdateEvaluationTest"))) {
						my ($name)	= $model->objects( $test, iri("${MF}name") )->elements;
						unless ($test->value =~ /$PATTERN/) {
							next;
						}
						warn "### update eval test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
						$self->update_eval_test( $model, $test, );
					}
				}
				
				{
					# Syntax Tests
					my $total	= 0;
					foreach my $type (qw(PositiveUpdateSyntaxTest11 NegativeUpdateSyntaxTest11)) {
						$total	+= $model->count_quads($test, iri("${RDF}type"), iri("${MF}$type"));
					}

					if ($total) {
						my ($name)	= $model->objects( $test, iri("${MF}name") )->elements;
						warn "### query syntax test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
						$self->syntax_test( 'update', $model, $test );
					}
				}
			}
		}
	}
};


1;
