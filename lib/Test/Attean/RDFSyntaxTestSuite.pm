package Test::Attean::RDFSyntaxTestSuite;

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
my $RDFT	= 'http://www.w3.org/ns/rdftest#';
my $RS		= 'http://www.w3.org/2001/sw/DataAccess/tests/result-set#';
my $MF		= 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#';
my $UT		= 'http://www.w3.org/2009/sparql/tests/test-update#';
my $RQ		= 'http://www.w3.org/2001/sw/DataAccess/tests/test-query#';
my $DAWGT	= 'http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#';

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
		exit(0);
	}
	
	my $manifest	= File::Spec->catfile($dir, 'manifest.ttl');
	my $manifestall	= File::Spec->catfile($dir, 'manifest-all.ttl');
	return grep { -r $_ } ($manifest, $manifestall);
}


###############################################################################


Test::Roo::top_test 'RDF Concrete Syntax tests' => sub {
	my $self		= shift;
	my $PATTERN		= $self->pattern;
	my @manifests	= @{ $self->manifests };
	my $model		= $self->model;
	foreach my $m (@manifests) {
# 		warn "Manifest: " . $m->as_string . "\n" if ($self->debug);

# 		{
# 			my $iter	= $model->get_quads($m);
# 			my $ser		= Attean->get_serializer('NTriples');
# 			warn $ser->serialize_iter_to_bytes($iter);
# 		}

		my ($list)	= $model->objects( $m, iri("${MF}entries") )->elements;
		unless (blessed($list)) {
			warn "No mf:entries found for manifest " . $m->as_string . "\n" if ($self->debug);
		}
		my @tests	= $model->get_list( $self->default_graph, $list )->elements;
		foreach my $test (@tests) {
			unless ($test->value =~ /$PATTERN/) {
				next;
			}
# 			warn 'TEST: ' . $test->value;
# 			if ($LIST_TESTS) {
# 				say $test->value;
# 			}
			if ($self->run_syntax_tests) {
				my %syntax_eval_tests	= map { $_ => 1 } (
					"http://www.w3.org/ns/rdftest#TestTurtleEval",
				);
				my %syntax_tests	= map { $_ => 1 } (
					"http://www.w3.org/ns/rdftest#TestNTriplesPositiveSyntax",
					"http://www.w3.org/ns/rdftest#TestNTriplesNegativeSyntax",
				);
				{
					my @types	= $model->objects($test, , iri("${RDF}type"))->elements;
					my $eval_count	= 0;
					my $syntax_count	= 0;
					foreach my $t (@types) {
						if (exists $syntax_eval_tests{ $t->value }) {
							$eval_count++;
						}
						if (exists $syntax_tests{ $t->value }) {
							$syntax_count++;
						}
					}
# 					warn Dumper([map { $_->value } @types]);
					if ($eval_count) {
						my ($name)	= $model->objects( $test, iri("${MF}name") )->elements;
						warn "### RDF syntax test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
						$self->data_syntax_eval_test( $model, $test );
					}
					if ($syntax_count) {
						my ($name)	= $model->objects( $test, iri("${MF}name") )->elements;
						warn "### RDF syntax test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
						$self->data_syntax_eval_test( $model, $test );
					}
				}
			}
		}
	}
};


1;
