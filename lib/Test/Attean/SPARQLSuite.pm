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
use List::Util qw(all);
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
my $RS		= 'http://www.w3.org/2001/sw/DataAccess/tests/result-set#';
my $MF		= 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#';
my $UT		= 'http://www.w3.org/2009/sparql/tests/test-update#';
my $RQ		= 'http://www.w3.org/2001/sw/DataAccess/tests/test-query#';
my $DAWGT	= 'http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#';

with 'Test::Attean::W3CManifestTestSuite';

sub enable_earl {
	my $self	= shift;
	my %args	= @_;
	my $conf	= $args{ conf } || {};
	if (can_load( modules => { 'RDF::EARL' => 0.002 })) {
		my $v			= $Attean::VERSION;
		$v				=~ s/[.]/-/;
		my $def_subject	= "http://purl.org/NET/cpan-uri/dist/Attean/v_$v";
		my $subject		= $conf->{ subject } || $def_subject;
		my $assertor	= $conf->{ assertor } || "${def_subject}#Test-Attean-SPARQLSuite";
		$assertor		= iri($assertor) unless (ref($assertor));
		$subject		= iri($subject) unless (ref($subject));
		
		my $EARL		= 'http://www.w3.org/ns/earl#';
		my $foaf		= 'http://xmlns.com/foaf/0.1/';
		my $type		= iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
		my $partOf		= iri('http://purl.org/dc/terms/isPartOf');
		my $desc		= iri('http://purl.org/dc/elements/1.1/description');

		my @triples;
		my @config_notes;
		if (my $store = $conf->{store}) {
			push(@config_notes, "Configured with the ${store} quadstore");
		}
		if (my $eval = $conf->{'eval'}) {
			push(@config_notes, "Configured with ${eval}-based evaluation");
		}
		if (scalar(@config_notes)) {
			my $proj	= $subject;
			$subject	= iri('foo:bar');
			push(@triples, triple($proj, $type, iri("${EARL}Software")));
			push(@triples, triple($subject, $partOf, $proj));
			foreach my $note (@config_notes) {
				push(@triples, triple($subject, $desc, literal($note)));
			}
		}
		
		my $earl		= RDF::EARL->new( subject => $subject, assertor => $assertor );
		$self->earl($earl);

		my $graph		= $earl->graph;
		my $model		= $earl->model;
		foreach my $t (@triples) {
			warn "Adding metadata: " . $t->as_string . "\n";
			$model->add_quad($t->as_quad($graph));
		}
			
		return $earl;
	}
	return;
}

sub manifest_paths {
	my $self	= shift;

	my $dir		= $self->tests_dir;
	unless (defined($dir)) {
		plan skip_all => "No manifest directory given";
		exit(0);
	}
	
	my $manifest	= File::Spec->catfile($dir, 'manifest.ttl');
	if (-r $manifest) {
		return ($manifest);
	} else {
		die "Manifest is not readable: $manifest\n";
	}
}

sub recursively_find_manifest_files {
	my $self		= shift;
	my $manifest	= shift;

	my $model		= $self->memory_model();
	my $class		= Attean->get_parser("turtle") || die "Failed to load parser for 'turtle'";

	my %loaded;
	
	my %manifests;
	my %manifest_files	= ($manifest => 1);
	my @load	= map { iri("file://" . File::Spec->rel2abs($_)) } ($manifest);
	while (scalar(@load)) {
		foreach my $iri (@load) {
# 			warn "Loading " . $iri->value . "\n" if ($self->debug);
			$loaded{ $iri->value }++;
		}
		$model->load_urls_into_graph($self->default_graph, @load);
		@load	= ();

		my $subjects = $model->subjects( iri("${RDF}type"), iri("${MF}Manifest") );
		my @manifest_matches	= $subjects->elements;
		foreach my $m (@manifest_matches) {
			if (not exists $manifests{ $m->value }) {
				$manifests{ $m->value }	= $m;
				my @list_heads	= $model->objects( $m, iri("${MF}include") )->elements;
				my @elements	= map { $model->get_list(undef, $_)->elements() } @list_heads;
				my @new			= grep { not exists($loaded{$_->value}) } @elements;
				push(@load, @new);
				foreach my $n (@new) {
					my $url	= $n->value;
					$url	=~ s{file://}{};
					$manifest_files{ $url }++;
				}
			}
		}
	}
	
	return keys %manifest_files;
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
# 		unless (blessed($list)) {
# 			warn "No mf:entries found for manifest " . $m->as_string . "\n" if ($self->debug);
# 		}
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
						$self->query_eval_test( $model, $test, specVersion => $specVersion );
					}
				}
				
				{
					# Syntax Tests
					my $total	= 0;
					foreach my $type (qw(PositiveSyntaxTest PositiveSyntaxTest11 NegativeSyntaxTest NegativeSyntaxTest11)) {
						$total	+= $model->count_quads($test, iri("${RDF}type"), iri("${MF}$type"));
					}

					if ($total) {
						my ($name)	= $model->objects( $test, iri("${MF}name") )->elements;
						warn "### query syntax test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
						$self->sparql_syntax_test( 'query', $model, $test, specVersion => $specVersion );
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
						$self->update_eval_test( $model, $test, specVersion => $specVersion );
					}
				}
				
				{
					# Syntax Tests
					my $total	= 0;
					foreach my $type (qw(PositiveUpdateSyntaxTest PositiveUpdateSyntaxTest11 NegativeUpdateSyntaxTest NegativeUpdateSyntaxTest11)) {
						$total	+= $model->count_quads($test, iri("${RDF}type"), iri("${MF}$type"));
					}

					if ($total) {
						my ($name)	= $model->objects( $test, iri("${MF}name") )->elements;
						warn "### query syntax test: " . $test->as_string . " >>> " . $name->value . "\n" if ($self->debug);
						$self->sparql_syntax_test( 'update', $model, $test, specVersion => $specVersion );
					}
				}
			}
		}
	}
};


1;
