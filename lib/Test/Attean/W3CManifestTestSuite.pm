package Test::Attean::W3CManifestTestSuite;

use v5.14;
use warnings;

use Attean;
use Attean::RDF;
use AtteanX::Parser::SPARQL;
use Attean::SimpleQueryEvaluator;
use Test::Attean::TestSimpleQueryEvaluator;
use Test::Attean::TestIDPQueryPlanner;
use Module::Load::Conditional qw[can_load];

require XML::Simple;
use Carp;
use Data::Dumper;
use Encode qw(encode encode_utf8 decode_utf8);
use File::Slurp;
use File::Spec;
use FindBin qw($Bin);
use Getopt::Long;
use HTTP::Message::PSGI;
use HTTP::Request;
use HTTP::Response;
use HTTP::Status;
use JSON qw(decode_json);
use List::Util qw(all any);
use Regexp::Common qw /URI/;
use Scalar::Util qw(blessed reftype);
use Test::Modern;
use Test::Roo::Role;
use Text::CSV;
use Text::Table;
use Try::Tiny;
use Types::Standard qw(Str Bool Int ArrayRef HashRef InstanceOf ConsumerOf);
use URI::file;
use UUID::Tiny ':std';

my $XSD		= 'http://www.w3.org/2001/XMLSchema#';
my $RDF		= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
my $RDFS	= 'http://www.w3.org/2000/01/rdf-schema#';
my $RDFT	= 'http://www.w3.org/ns/rdftest#';
my $RS		= 'http://www.w3.org/2001/sw/DataAccess/tests/result-set#';
my $MF		= 'http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#';
my $UT		= 'http://www.w3.org/2009/sparql/tests/test-update#';
my $RQ		= 'http://www.w3.org/2001/sw/DataAccess/tests/test-query#';
my $SD		= 'http://www.w3.org/ns/sparql-service-description#';
my $DAWGT	= 'http://www.w3.org/2001/sw/DataAccess/tests/test-dawg#';
my $FORMATS	= 'http://www.w3.org/ns/formats/';

my $absolutePath	= iri('http://www.w3.org/2011/http#absolutePath');
my $authority		= iri('http://www.w3.org/2011/http#connectionAuthority');
my $cnt_chars		= iri('http://www.w3.org/2011/content#chars');
my $cnt_encoding	= iri('http://www.w3.org/2011/content#characterEncoding');
my $entries			= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#entries');
my $expectedBoolean	= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#expectedBoolean');
my $expectedFormat	= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#expectedFormat');
my $expectedLocation= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#expectedLocation');
my $expectedStatus	= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#expectedStatus');
my $ht_body			= iri('http://www.w3.org/2011/http#body');
my $ht_fieldName	= iri('http://www.w3.org/2011/http#fieldName');
my $ht_fieldValue	= iri('http://www.w3.org/2011/http#fieldValue');
my $ht_headers		= iri('http://www.w3.org/2011/http#headers');
my $ht_resp			= iri('http://www.w3.org/2011/http#resp');
my $Manifest		= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#Manifest');
my $methodName		= iri('http://www.w3.org/2011/http#methodName');
my $mf_action		= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#action');
my $mf_expectation	= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#expectation');
my $mf_name			= iri('http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name');
my $rdf_type		= iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
my $rdfs_label		= iri('http://www.w3.org/2000/01/rdf-schema#label');
my $requests		= iri('http://www.w3.org/2011/http#requests');
my $ut_graph		= iri('http://www.w3.org/2009/sparql/tests/test-update#graph');
my $ut_graphData	= iri('http://www.w3.org/2009/sparql/tests/test-update#graphData');

requires 'test_model';
requires 'manifest_paths';

has run_update_tests	=> (is => 'rw', isa => Bool, default => 1);
has run_query_tests		=> (is => 'rw', isa => Bool, default => 1);
has run_syntax_tests	=> (is => 'rw', isa => Bool, default => 1);
has run_gsp_direct		=> (is => 'rw', isa => Bool, default => 1);
has run_gsp_indirect	=> (is => 'rw', isa => Bool, default => 1);
has run_gsp_postcreate	=> (is => 'rw', isa => Bool, default => 1);



has debug				=> (is => 'rw', isa => Bool, default => 0);
has results				=> (is => 'rw', isa => Bool, default => 0);
has strict_approval		=> (is => 'rw', isa => Bool, default => 0);
has use_idp_planner		=> (is => 'rw', isa => Bool, default => 1);
has pattern				=> (is => 'rw', isa => Str, default => '');

has tests_dir			=> (is => 'rw', required => 1, default => sub { $ENV{ATTEAN_SPARQL_TESTS_DIR} });
has model				=> (is => 'rw', isa => ConsumerOf['Attean::API::Model'], init_arg => undef);
has manifests			=> (is => 'rw', isa => ArrayRef, init_arg => undef);
has default_graph		=> (is => 'rw');
has failures			=> (is => 'rw', isa => HashRef, default => sub { +{} });
has supported_extensions	=> (is => 'rw', isa => HashRef, default => sub { +{} });

has timestamp			=> (is => 'rw', isa => Int, default => sub { scalar(time) });
has earl				=> (is => 'rw', isa => InstanceOf['RDF::EARL']);
has earl_filename		=> (is => 'rw', isa => Str);

has load_ontology_files	=> (is => 'rw', isa => ArrayRef[Str]);

after 'BUILD' => sub {
	my $self	= shift;
	if ($self->pattern) {
		$self->results(1);
	}
	
	$self->earl_filename(sprintf('.sparql-test-suite-%d', $self->timestamp));
	my @files	= @{ $self->load_ontology_files || [] };
	unless (scalar(@files)) {
		my $env_path	= $ENV{'ATTEAN_LOAD_PATH'};
		my $base		= (defined($env_path) and -d $env_path) ? $env_path : File::Spec->catfile( $Bin, 'data', 'test-rdf' );
		push(@files, glob("${base}/*.ttl"));
		$self->load_ontology_files(\@files);
	}

};

sub memory_model {
	my $self	= shift;
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	return $model;
}

sub setup {
	my $self	= shift;
	$self->default_graph(iri('http://graph/'));

	warn 'setting up sparql test harness' if ($self->debug);
	my @manifests_iris	= $self->manifest_paths();
	unless (scalar(@manifests_iris)) {
		my $dir		= $self->tests_dir;
		plan skip_all => "No manifest files found in $dir";
		exit(0);
	}

	my $model	= $self->memory_model();
	foreach my $f (@{ $self->load_ontology_files }) {
		my $url	= "file://$f";
		try {
			warn "Loading $url\n" if ($self->debug);
			$model->load_urls_into_graph($self->default_graph, $url);
		} catch {
			warn "Failed to load $url\n";
		};
	}
	
	$self->model($model);
	my $class	= Attean->get_parser("turtle") || die "Failed to load parser for 'turtle'";

	my %loaded;
	
	my %manifests;
	my @load	= map { iri("file://" . File::Spec->rel2abs($_)) } @manifests_iris;
	while (scalar(@load)) {
		foreach my $iri (@load) {
			warn "Loading " . $iri->value . "\n" if ($self->debug);
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
				push(@load, grep { not exists($loaded{$_->value}) } @elements);
			}
		}
	}
	
	my @manifests	= values %manifests;
	
	if ($self->debug) {
		warn "done parsing manifests" if $self->debug;
		warn "setting manifests:\n";
		foreach my $m (@manifests) {
			warn "- " . $m->as_string . "\n";
		}
	}
	$self->manifests(\@manifests);
}

sub sparql_protocol_test {
	my $self		= shift;
	my $test_type	= shift;
	my $model		= shift;
	my $test		= shift;
	my %args		= @_;
	my $specVersion	= $args{'specVersion'} // '1.1';
	
	my $testname	= $test->value;
	my $config	= {
		store => 'Memory2',
		endpoint  => {
			service_description => {
				named_graphs => 1,
				default => 1,
			},
			load_data => 0,
			update => 1,
		}
	};
	my $end		= AtteanX::Endpoint->new( $config );
	my $app	= sub {
		my $env 	= shift;
		my $req 	= Plack::Request->new($env);
		my $resp	= $end->run( $req );
		return $resp->finalize;
	};

	my $host		= 'localhost:3000';
	LWP::Protocol::PSGI->register($app, host => $host);

	my $sparql_endpoint	= "http://${host}/sparql";
	my $gsp_endpoint	= "http://${host}/gsp";

	my $ua		= LWP::UserAgent->new();
	if ($test_type eq 'prot') {
		eval { $self->run_protocol_test($test_type, $ua, $model, $test, $sparql_endpoint) };
		if ($@) {
			warn "*** protocol test failed: $@";
			$self->record_result('unhandled', 0, $test->as_string);
			fail($testname);
		}
	} else {
		eval { $self->run_gsp_test($test_type, $ua, $model, $test, $sparql_endpoint, $gsp_endpoint) };
		if ($@) {
			warn "*** protocol test failed: $@";
			$self->record_result('unhandled', 0, $test->as_string);
			fail($testname);
		}
	}
}

sub setup_protocol_dataset {
	my $self		= shift;
	my $ua			= shift;
	my $endpoint	= shift;
	my $model		= shift;
	my $test		= shift;
	warn "Clearing dataset ...\n" if ($self->debug);
	warn "REQUEST: POST $endpoint update=DROP ALL\n" if ($self->debug);
	my $resp	= $ua->post($endpoint, {update => 'DROP ALL'});
	warn "RESPONSE:\n" . $resp->as_string . "\n" if ($self->debug);
	die $resp->status_line unless ($resp->is_success);

	my (@graphs)	= $model->objects($test, $ut_graphData)->elements;
	if (scalar(@graphs)) {
		foreach my $graph (@graphs) {
			# loading contents of $uri into graph named $name
			my ($name)	= map { $_->value } $model->objects($graph, $rdfs_label)->elements;
			my ($uri)	= map { $_->value } $model->objects($graph, $ut_graph)->elements;
			my $update	= "LOAD <$uri> INTO GRAPH <$name>";
			warn "Loading data <$name> ...\n" if ($self->debug);
			my $resp	= $ua->post($endpoint, {update => $update});
			die $resp->status_line unless ($resp->is_success);
		}
	}
}

sub run_gsp_test {
	my $self		= shift;
	my $test_type	= shift;
	my $ua			= shift;
	my $model		= shift;
	my $test		= shift;
	my $prot_endpoint	= shift;
	my $endpoint	= shift;
	say '# ' . $test->value if ($self->debug);
	my ($action)	= $model->objects($test, $mf_action)->elements;
	unless ($action) {
		return fail('no action data for test');
	}
	my ($reqs)		= $model->objects($action, $requests)->elements;
	my @reqs		= $model->get_list($self->default_graph, $reqs)->elements;
	my ($auth)		= $model->objects($action, $authority)->elements;
	my $auth_v		= $auth->value;
	my $uri			= new URI($endpoint);
	my $real_auth	= $uri->authority;
	
	$ua->get("$prot_endpoint?================================================================NEWTEST");
	$self->setup_protocol_dataset($ua, $prot_endpoint, $model, $test);
	
	say scalar(@reqs) . ' request(s) in test' if ($self->debug);
	my $req_number	= 1;
	my $templates	= {};
	foreach my $req (@reqs) {
		my $name	= "Request " . $req_number++;
		subtest $name => sub {
			say "--------------------------------- $name ---------------------------------" if ($self->debug);
			my $m	= $self->make_protocol_request($test_type, $endpoint, $model, $req, $auth_v, $real_auth, $templates);
			say "Request [[" . $m->method . " " . $m->uri . "]]\n" if ($self->debug);
	# 		next unless ($m->method eq 'GET');
			say "REQUEST:\n" . $m->as_string if ($self->debug);
			my $resp	= $ua->request($m);
			say "RESPONSE:\n" . $resp->as_string if ($self->debug);
			diag(sprintf('%s %s', $m->method, $m->uri));
			$self->validate_protocol_response($test_type, $endpoint, $model, $test, $req, $resp, $auth_v, $real_auth, $templates);
		};
		last;
	}
}

sub run_protocol_test {
	my $self		= shift;
	my $test_type	= shift;
	my $ua			= shift;
	my $model		= shift;
	my $test		= shift;
	my $prot_endpoint	= shift;
	my ($action)	= $model->objects($test, $mf_action)->elements;
	unless ($action) {
		return fail('no action data for test');
	}
	my ($reqs)		= $model->objects($action, $requests)->elements;
	my @reqs		= $model->get_list($self->default_graph, $reqs)->elements;
	my ($auth)		= $model->objects($action, $authority)->elements;
	my $auth_v		= $auth->value;
	my $uri			= new URI($prot_endpoint);
	my $real_auth	= $uri->authority;
	
	$self->setup_protocol_dataset($ua, $prot_endpoint, $model, $test);
	
	say scalar(@reqs) . ' request(s) in test' if ($self->debug);
	my $req_number	= 0;
	foreach my $req (@reqs) {
		$req_number++;
		my $m	= $self->make_protocol_request($test_type, $prot_endpoint, $model, $req, $auth_v, $real_auth);
# 		next unless ($m->method eq 'GET');
		warn "REQUEST:\n" . $m->as_string . "\n" if ($self->debug);
		my $resp	= $ua->request($m);
		warn "RESPONSE:\n" . $resp->as_string if ($self->debug);
		diag(sprintf('%s %s', $m->method, $m->uri));
		$self->validate_protocol_response($test_type, $prot_endpoint, $model, $test, $req, $resp, $auth_v, $real_auth);
		if ($self->debug) {
			warn "--------------------------------- after request:\n";
			$self->dump_protocol_dataset($prot_endpoint, $ua);
			warn "---------------------------------\n";
		}
	}
}

sub dump_protocol_dataset {
	my $self		= shift;
	my $endpoint	= shift;
	my $ua			= shift;
	my $resp		= $ua->post($endpoint, {query => 'SELECT * WHERE { { GRAPH ?g { ?s ?p ?o } } UNION { ?s ?p ?o } }'}, 'Accept' => 'application/sparql-results+json');
	my $body		= $resp->decoded_content;
	my $j			= decode_json($body);
	my $r			= $j->{results}{bindings};
	my $tb			= Text::Table->new(qw(s p o g));
	my @rows;
	foreach my $r (@$r) {
		push(@rows, [map { $r->{$_}{'value'} } qw(s p o g)]);
	}
	$tb->load(@rows);
	say $tb;
}

sub make_protocol_request {
	my $self		= shift;
	my $test_type	= shift;
	my $endpoint	= shift;
	my $model		= shift;
	my $req			= shift;
	my $auth_v		= shift;
	my $real_auth	= shift;
	my $templates	= shift;
	my ($path)		= $model->objects($req, $absolutePath)->elements;
	my ($method)	= $model->objects($req, $methodName)->elements;
	my ($body)		= $model->objects($req, $ht_body)->elements;
	my ($headers)	= $model->objects($req, $ht_headers)->elements;
	my $path_v		= $path->value;
	foreach my $t (keys %$templates) {
		my $v	= $templates->{$t};
		my $i	= index($path_v, $t);
		if ($i >= 0) {
			substr($path_v, $i, length($t), $v);
		}
	}
	
	my $pathuri		= new URI($path_v);

	my $m			= HTTP::Request->new();
	$m->method($method->value);
	my $uri	= new URI($endpoint);

	if ($test_type eq 'gsp') {
		my $test_path	= $pathuri->path;
		$test_path		=~ s#/gsp#$uri->path#e;
		$uri->path($test_path);
	}
	$uri->query($pathuri->query);
	
	my $uri_v	= $uri->as_string;
	
	# fixup
	$uri_v	=~ s/$auth_v/$real_auth/e;

	
	$m->uri(URI->new($uri_v));
	if ($body) {
		my ($content)		= $model->objects($body, $cnt_chars)->elements;
		my ($encoding)		= $model->objects($body, $cnt_encoding)->elements;

		# fixup
		$content	=~ s/$auth_v/$real_auth/ge;
		######### TODO CONTINUE HERE ##############
		
		my $bytes			= encode($encoding->value, $content->value);
		$m->content($bytes);
	}
	
	my @headers	= $model->get_list($self->default_graph, $headers)->elements;
	foreach my $h (@headers) {
		my ($key)		= $model->objects($h, $ht_fieldName)->elements;
		my ($value)		= $model->objects($h, $ht_fieldValue)->elements;
		$m->header($key->value => $value->value);
	}
	return $m;
}

sub validate_protocol_response {
	my $self		= shift;
	my $test_type	= shift;
	my $endpoint	= shift;
	my $model	= shift;
	my $test	= shift;
	my $name	= $test->value;
	my $req		= shift;
	my $m		= shift;
	my $auth_v		= shift;
	my $real_auth	= shift;
	my $templates	= shift;
	
	warn "Validating response..." if ($self->debug);
	
	my ($resp)	= $model->objects($req, $ht_resp)->elements;
	unless ($resp) {
		return fail('No expected response data for test');
	}

	my (@expected_location)	= $model->objects($resp, $expectedLocation)->elements;
	foreach my $loc (@expected_location) {
		my $header			= $m->header('Location');
		ok($header, 'Location header value');
		my $var				= $loc->value;
		$templates->{ $var }	= $header;
	}

	my (@expected_status)	= $model->objects($resp, $expectedStatus)->elements;
	if (scalar(@expected_status)) {
		my %ok				= map { $_ => 1 } map { $_->value } @expected_status;
		my $code			= $m->code;
		my $digit			= int($code / 100);
		my $actual_class_iri		= "http://www.w3.org/2011/http-statusCodes#StatusCode${digit}xx";
		my $message					= status_message($code);
		$message					=~ s/ //g;
		my $actual_iri				= ($test_type eq 'gsp')
									? "http://www.w3.org/2011/http-statusCodes#$message"
									: "http://www.w3.org/2011/http#$message";
		if (exists $ok{$actual_class_iri}) {
			pass("status code matches class ${digit}xx")
		} elsif (exists $ok{$actual_iri}) {
			pass("status code matches ($code)")
		} else {
			fail("got status code: " . $m->status_line . ' but expecting one of: ' . join(',', keys %ok))
		}
	}
	my ($expected_format)	= $model->objects($resp, $expectedFormat)->elements;
	if ($expected_format) {
		my $ct	= $m->header('Content-Type');
		my $expected	= $expected_format->value;
		if ($expected eq 'tabular') {
			like($ct, qr#^application/sparql-results[+](json|xml)|text/csv|text/tab-separated-values#, "expected tabular result but got $ct");
		} elsif ($expected eq 'boolean') {
			like($ct, qr#^application/sparql-results[+](json|xml)|text/plain#, "expected boolean result but got $ct");
		} elsif ($expected eq 'RDF') {
			like($ct, qr#^(application/(rdf-xml|n-triples))|text/((n-(triples|quads))|turtle)|application/ld[+]json#, "expected RDF result but got $ct")
		} else {
			die("Unexpected format value '$expected'");
		}
	}
	my ($expected_bool)		= $model->objects($resp, $expectedBoolean)->elements;
	if ($expected_bool) {
		my $ct	= $m->header('Content-Type');
		my $body	= $m->decoded_content;
		my $expected	= $expected_bool->value;
		if ($ct eq 'application/sparql-results+json') {
			like($body, qr#"boolean"\s*:\s*${expected}#sm, "expected boolean $expected");
		} elsif ($ct eq 'application/sparql-results+xml') {
			like($body, qr#<boolean>${expected}</boolean>#sm, "expected boolean $expected");
		} elsif ($ct =~ 'text/plain') {
			like($body, qr#${expected}#sm, "expected boolean $expected");
		} else {
			die("validate expected boolean for [$ct]: " . $expected_bool->value);
		}
	}

	my ($expected_headers)	= $model->objects($resp, $ht_headers)->elements;
	if ($expected_headers) {
		my @headers	= $model->get_list($self->default_graph, $expected_headers)->elements;
		foreach my $h (@headers) {
			my ($key)				= $model->objects($h, $ht_fieldName)->elements;
			my ($expected_value_l)	= $model->objects($h, $ht_fieldValue)->elements;
			my $expected_value		= $expected_value_l->value;
			my $actual_value		= $m->header($key->value);
			if ($expected_value =~ m<$actual_value; charset=utf-8>i) {
				pass('content-type with ignored charset=utf-8');
			} else {
				$actual_value	=~ s/ //g;
				$expected_value	=~ s/ //g;
				is($actual_value, $expected_value, "expected header: " . $key->value);
			}
		}
	}

	my ($expected_body)	= $model->objects($resp, $ht_body)->elements;
	if ($expected_body) {
		my $ct		= $m->header('Content-Type');
		if (not defined($ct)) {
			fail('No content-type in response');
			return;
		}
		$ct			=~ s/;.*//;
		
		warn "Body content-type: $ct\n";
		my $pclass		= Attean->get_parser( media_type => $ct );
		unless ($pclass) {
			fail("Unrecognized media type for response: $ct");
			return;
		}
		my $parser		= $pclass->new();
		my ($chars)		= $model->objects($expected_body, $cnt_chars)->elements;
		my $actual		= $m->decoded_content;
		my $expected	= $chars->value;
		
		# fixup
		$actual		=~ s/$auth_v/$real_auth/ge;
		$expected	=~ s/$auth_v/$real_auth/ge;

		warn "Actual response body: " . Dumper($actual) if ($self->debug);
		warn "Expected response body: " . Dumper($expected) if ($self->debug);
		
		my $actual_iter		= $parser->parse_iter_from_bytes(encode_utf8($actual));
		my $expected_iter	= $parser->parse_iter_from_bytes(encode_utf8($expected));
		my $eqtest	= Attean::BindingEqualityTest->new();
		my $ok		= eval { ok( $eqtest->equals( $actual_iter, $expected_iter ), 'expected triples' ) or diag($eqtest->error) };
# 		unless ($ok) {
# 			warn Dumper({expected => $expected, actual => $actual});
# 		}
		if ($@) {
			diag($@);
		}
		unless ($ok) {
			$self->record_result('protocol', $ok, $test->as_string);
			fail($test->value);
			return;
		}
	}

	my ($expectation)		= $model->objects($resp, $mf_expectation)->elements;
	if ($expectation) {
		$self->record_result('protocol', 0, $test->as_string);
		fail($test->value . " - validate expectation: " . $expectation->value);
		return;
	}
	pass($test->value);
}

sub sparql_syntax_test {
	my $self		= shift;
	my $test_type	= shift;
	my $model		= shift;
	my $test		= shift;
	my %args		= @_;
	my $specVersion	= $args{'specVersion'} // '1.1';

	my $handled		= 0;
	
	my $type		= iri( "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" );
	my $mfname		= iri( "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name" );
	my ($queryd)	= $model->objects( $test, iri("${MF}action") )->elements;
	my ($approved)	= $model->objects( $test, iri("${DAWGT}approval") )->elements;
	my ($name)		= $model->objects( $test, $mfname )->elements;
	my $testname	= $test->value;
	$testname		=~ s{^.*[/#]}{};
	
	if ($self->strict_approval) {
		unless ($approved) {
			warn "- skipping test because it isn't approved\n" if ($self->debug);
			return;
		}
		if ($approved->equal("${DAWGT}NotClassified")) {
			warn "- skipping test because its approval is dawgt:NotClassified\n" if ($self->debug);
			return;
		}
	}

	my $is_pos_query	= $model->count_quads($test, $type, iri("${MF}PositiveSyntaxTest")) + $model->count_quads($test, $type, iri("${MF}PositiveSyntaxTest11"));
	my $is_pos_update	= $model->count_quads($test, $type, iri("${MF}PositiveUpdateSyntaxTest")) + $model->count_quads($test, $type, iri("${MF}PositiveUpdateSyntaxTest11"));
	my $is_neg_query	= $model->count_quads($test, $type, iri("${MF}NegativeSyntaxTest")) + $model->count_quads($test, $type, iri("${MF}NegativeSyntaxTest11"));
	my $is_neg_update	= $model->count_quads($test, $type, iri("${MF}NegativeUpdateSyntaxTest")) + $model->count_quads($test, $type, iri("${MF}NegativeUpdateSyntaxTest11"));

	my $uri					= URI->new( $queryd->value );
	my $filename			= $uri->file;
	my (undef,$base,undef)	= File::Spec->splitpath( $filename );
	$base					= "file://${base}";
# 	warn "Loading SPARQL query from file $filename" if ($self->debug);
	my $sparql				= eval { read_file($filename, { binmode => ':utf8' }) };
	if ($@) {
		warn("$!: $filename; " . $test->as_string);
		$self->record_result('syntax', 0, $test->as_string, $TODO);
		return;
	}
	my $bytes	= encode_utf8($sparql);

	if ($self->debug) {
		my $q			= $sparql;
		$q				=~ s/\s+/ /g;
		warn "### test     : " . $test->as_string . "\n";
		warn "# sparql     : $q\n";
		warn "# file       : $filename\n";
	}
	
	my $pclass	= Attean->get_parser('SPARQL');
	my $parser	= $pclass->new(version => $specVersion);
	if ($test_type eq 'update') {
		$parser->update(1);
	}
	if ($is_pos_query or $is_pos_update) {
		my ($query)	= eval { $parser->parse_list_from_bytes($bytes) };
		my $ok	= blessed($query);
		$self->record_result('syntax', $ok, $test->as_string, $TODO);
		$handled++;
		if ($ok) {
			pass("$testname - $filename");
		} else {
			fail("$testname - $filename: $@");
		}
	} elsif ($is_neg_query or $is_neg_update) {
		my ($query)	= eval { $parser->parse_list_from_bytes($bytes) };
		my $ok	= $@ ? 1 : 0;
		$self->record_result('syntax', $ok, $test->as_string, $TODO);
		$handled++;
		if ($ok) {
			pass("$testname - $filename");
		} else {
			if ($self->debug) {
				warn $query->as_string;
			}
			fail("$testname - $filename (unexpected successful parse)");
		}
	}
	
	unless ($handled) {
		fail("Unhandled test: $testname");
	}
	
}

# Look up the test in the model, and determine:
# 
# * the media type of the syntax being tested
# * whether the test is an eval test (compared against a base nt/nq file)
# * whether the test is a syntax test
# * whether the test is a canonicalization test
# * whether the test is a negative test (implies a syntax test)
# * whether the syntax being tested is a triples-based format (being false implies that the format is a quad-based format)
#
# Not all of these pieces of information are derivable from the RDF data,
# so some of it is heuristics based on the naming of the test type.
#
# If the test is not a recognized type, this method dies. Calling code is responsible
# for catching that error and properly recording a test failure.
sub syntax_test_type {
	my $self		= shift;
	my $model		= shift;
	my $test		= shift;
	my @types		= $model->objects( $test, iri("${RDF}type") )->elements;
	my @formats		= $model->objects( \@types, iri("${RDFT}syntaxFormat") )->elements;
	my @mediatypes	= $model->objects( \@formats, iri("${FORMATS}media_type") )->elements;
	unless (scalar(@mediatypes) == 1) {
		my $types	= join(',', map { $_->value } @types);
		my $formats	= join(',', map { $_->value } @formats);
		die "No syntax test type for " . $test->value . " ; types=$types ; formats=$formats";
	}
	my $mediatype	= $mediatypes[0]->value;

	my $is_eval		= $model->count_quads( \@types, iri("${RDFS}subClassOf"), iri("${RDFT}TestEval") );
	my $is_syntax	= $model->count_quads( \@types, iri("${RDFS}subClassOf"), iri("${RDFT}TestSyntax") );
	my $is_c14n		= $model->count_quads( \@types, iri("${RDFS}subClassOf"), iri("${RDFT}TestC14N") );
	my $is_neg		= $model->count_quads( \@types, iri("${RDFS}subClassOf"), iri("${RDFT}TestNegativeSyntax") );
	$is_syntax++	if ($is_neg); # to avoid having to do multiple subClassOf hops on these tests to find rdft:TextSyntax
	
	my $triples		= 0;
	$triples++ if ($mediatype eq 'text/turtle');
	$triples++ if ($mediatype eq 'application/n-triples');
	$triples++ if ($mediatype eq 'application/rdf+xml');
	
	return ($mediatype, $is_eval, $is_syntax, $is_c14n, $is_neg, $triples);
}

sub data_syntax_eval_test {
	my $self		= shift;
	my $model		= shift;
	my $test		= shift;
	my $count		= shift // 1;

	my $type		= iri( "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" );
	my $mfname		= iri( "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name" );
	my ($queryd)	= $model->objects( $test, iri("${MF}action") )->elements;
	my ($resultd)	= $model->objects( $test, iri("${MF}result") )->elements;
	my ($approved)	= $model->objects( $test, iri("${DAWGT}approval") )->elements;
	my ($name)		= $model->objects( $test, $mfname )->elements;
	my $testname	= $test->value;
	$testname		=~ s{^.*[/#]([^/#]+[/#])}{$1};
	
	if ($self->strict_approval) {
		unless ($approved) {
			warn "- skipping test because it isn't approved\n" if ($self->debug);
			return;
		}
		if ($approved->equal("${DAWGT}NotClassified")) {
			warn "- skipping test because its approval is dawgt:NotClassified\n" if ($self->debug);
			return;
		}
	}

	my ($mediatype, $is_eval, $is_syntax, $is_c14n, $is_neg, $is_triples)	= eval { $self->syntax_test_type($model, $test) };
	if ($@) {
		$self->record_result('syntax', 0, $test->as_string, $TODO);
		fail("$testname - Unexpected format for test: $@");
		return;
	}
	my $is_quads	= not($is_triples);

	my $base;
	my ($result_filename, $expected_bytes, $nt_content);
	if ($is_eval) {
		my $result_uri			= URI->new( $resultd->value );
		$result_filename		= $result_uri->file;
		(undef,$base,undef)	= File::Spec->splitpath( $result_filename );
		$base					= "file://${base}";
		warn "Loading expected RDF data from file $result_filename\n" if ($self->debug);
		$nt_content				= eval { read_file($result_filename, { binmode => ':utf8' }) };
		if ($@) {
			warn("$!: $result_filename; " . $test->as_string);
			$self->record_result('evaluation', 0, $test->as_string, $TODO);
			return;
		}
		unless (defined($nt_content)) {
			warn "*** Undefined expected data from $result_filename...\n";
		}
		$expected_bytes	= encode_utf8($nt_content);
	}
	
	my $uri					= URI->new( $queryd->value );
	my $filename			= $uri->file;
	(undef,$base,undef)	= File::Spec->splitpath( $filename );
	$base					= "file://${base}";
	warn "Loading test RDF data from file $filename\n" if ($self->debug);
	my $content				= eval { read_file($filename, { binmode => ':utf8' }) };
	if ($@) {
		warn("$!: $filename; " . $test->as_string);
		$self->record_result('syntax', 0, $test->as_string, $TODO);
		return;
	}
	my $bytes				= encode_utf8($content);

	if ($self->debug) {
		my $q			= $content;
		$q				=~ s/\s+/ /g;
		
		warn "### test     : " . $test->as_string . "\n";
		warn "# file       : $filename\n";
		warn "# content    : $q\n";
		if ($is_eval) {
			my $expected	= $nt_content;
			$expected		=~ s/\s+/ /g;
			warn "# expected   : $result_filename\n";
			warn "# expected   : $expected\n";
		}
	}

	my $test_uri	= $uri->as_string;
	my $test_base	= $uri->as_string;
	$test_base		= $self->fix_local_to_w3c($test_base);
	warn "Test base: " . $test_base . "\n" if ($self->debug);
	
	my $pclass		= Attean->get_parser(media_type => $mediatype);
	unless ($pclass) {
		die "No parser found for type $mediatype";
	}
	my $test_parser	= $pclass->new(base => $test_base);
	my $nt_parser	= Attean->get_parser('NTriples')->new();
	my $nq_parser	= Attean->get_parser('NQuads')->new();
	SKIP: {
		if ($is_c14n) {
			skip("$testname - C14N tests not supported");
		} elsif ($is_neg) {
			my (@triples)	= eval { $test_parser->parse_list_from_bytes($bytes) };
			my $ok	= $@ ? 1 : 0;
			$self->record_result('syntax', $ok, $test->as_string, $TODO);
			if ($ok) {
				pass("$testname - $filename");
			} else {
				if ($self->debug) {
					warn "Unexpected successful parse of:\n" . $content;
				}
				fail("$testname - $filename (unexpected successful parse)");
			}
		} else {
			my (@actual_statements)	= eval { $test_parser->parse_iter_from_bytes($bytes)->uniq()->elements() };
			my $parse_error	= $@;
			my $ser			= Attean->get_serializer($is_triples ? 'NTriples' : 'NQuads')->new();
			my $nq_parser	= Attean->get_parser('NQuads')->new();
			my $expected_parser	= $is_triples ? $nt_parser : $nq_parser;
			my $role			= $is_triples ? 'Attean::API::Triple' : 'Attean::API::TripleOrQuad';
			my $eq_test	= Attean::BindingEqualityTest->new();
			if ($is_eval) {
				my @expected_statements	= eval { $expected_parser->parse_iter_from_bytes($expected_bytes)->uniq()->elements() };
				$parse_error			||= $@;
				if ($parse_error) {
					$self->record_result('evaluation', 0, $test->as_string, $TODO);
					fail("$testname - Failed to parse expected data from $result_filename: $parse_error");
					return;
				}
				my $expected_iter		= Attean::ListIterator->new( values => \@expected_statements, item_type => $role );

				my $actual_iter	= Attean::ListIterator->new( values => \@actual_statements, item_type => $role );
				my $ok		= $eq_test->equals($actual_iter, $expected_iter);
				$self->record_result('evaluation', $ok, $test->as_string, $TODO);
				ok($ok, "$testname - $filename");
				if (not $ok) {
					warn "BindingEqualityTest failures: " . $eq_test->error();
					warn "Expecting " . scalar(@expected_statements) . " statements:\n";
					if (scalar(@expected_statements)) {
						warn $ser->serialize_list_to_bytes(@expected_statements);
					}
					warn "But found " . scalar(@actual_statements) . " statements:\n";
					my $bytes	= $ser->serialize_list_to_bytes(@actual_statements);
					warn((length($bytes)) ? decode_utf8($bytes) : '(empty content)');
				}
			} else {
				my $ok	= scalar(not $parse_error);
				$self->record_result('syntax', $ok, $test->as_string, $TODO);
				if ($ok) {
					pass("$testname - $filename");
				} else {
					fail("$testname - $filename: $parse_error");
				}
			}
		}
	}
}

sub update_eval_test {
	my $self		= shift;
	my $model		= shift;
	my $test		= shift;
	my %args		= @_;
	my $specVersion	= $args{'specVersion'} // '1.1';

	my ($action)	= $model->objects( $test, iri("${MF}action") )->elements;
	my ($result)	= $model->objects( $test, iri("${MF}result") )->elements;
	my @reqs		= $model->objects( $test, iri("${MF}requires") )->elements;
	my ($approved)	= $model->objects( $test, iri("${DAWGT}approval") )->elements;
	my ($queryd)	= $model->objects( $action, iri("${UT}request") )->elements;
	my @data		= $model->objects( $action, iri("${UT}data") )->elements;
	my @gdata		= $model->objects( $action, iri("${UT}graphData") )->elements;
	
	if ($self->strict_approval) {
		unless ($approved) {
			warn "- skipping test because it isn't approved\n" if ($self->debug);
			return;
		}
		if ($approved->equal(iri("${DAWGT}NotClassified"))) {
			warn "- skipping test because its approval is dawgt:NotClassified\n" if ($self->debug);
			return;
		}
	}
	
	my $uri					= URI->new( $queryd->value );
	my $filename			= $uri->file;
	my (undef,$base,undef)	= File::Spec->splitpath( $filename );
	$base					= "file://${base}";
	warn "Loading SPARQL query from file $filename" if ($self->debug);
	my $sparql				= eval { read_file($filename, { binmode => ':utf8' }) };
	if ($@) {
		warn("$!: $filename; " . $test->as_string);
		$self->record_result('update', 0, $test->as_string, $TODO);
		return;
	}

	my $q			= $sparql;
	$q				=~ s/\s+/ /g;
	if ($self->debug) {
		warn "### test     : " . $test->value . "\n";
		warn "# sparql     : $q\n";
		warn "# file       : $filename\n";
		foreach my $data (@data) {
			warn "# data       : " . $data->value . "\n" if (blessed($data));
		}
		warn "# graph data : " . $_->value . "\n" for (@gdata);
		warn "# result     : " . $result->value . "\n";
		foreach my $req (@reqs) {
			warn "# requires   : " . $req->value . "\n" if (blessed($req));
		}
	}
	
	my $todo_because_of_test_req	= 0;
	my @unsupported_reqs;
	foreach my $req (@reqs) {
		unless (exists ${ $self->supported_extensions }{ $req->value }) {
			$todo_because_of_test_req++;
			push(@unsupported_reqs, $req->value);
		}
	}
	
	
	
	# TODO: set up remote endpoint mock
	TODO: {
		local($TODO)	= $todo_because_of_test_req ? "requires " . join(', ', @unsupported_reqs) : '';

		warn "constructing model...\n" if ($self->debug);
		my $test_model	= $self->test_model();
		foreach my $data (@data) {
			eval {
				if (blessed($data)) {
					$test_model->load_urls_into_graph($self->default_graph, $data);
				}
			};
			
			if ($self->debug) {
				warn "TEST MODEL AFTER DEFAULT GRAPH LOAD:\n";
				$self->debug_model($test_model);
			}
			
			if ($@) {
				fail($test->value);
				$self->record_result('update', 0, $test->value, $TODO);
				print "# died: " . $test->value . ": $@\n";
				return;
			}
		}
		foreach my $gdata (@gdata) {
			my ($data)	= ($model->objects( $gdata, iri("${UT}data") )->elements)[0] || ($model->objects( $gdata, iri("${UT}graph") )->elements)[0];
			my ($graph)	= $model->objects( $gdata, iri("${RDFS}label") )->elements;
			my $uri		= $graph->value;
			eval {
				$test_model->load_urls_into_graph(iri($uri), $data);
			};
			if ($@) {
				fail($test->as_string);
				$self->record_result('update', 0, $test->value, $TODO);
				print "# died: " . $test->value . ": $@\n";
				return;
			};
		}
		
		my ($result_status)	= $model->objects( $result, iri("${UT}result") )->elements;
		my @resgdata		= $model->objects( $result, iri("${UT}graphData") )->elements;
		my ($resdata)		= $model->objects( $result, iri("${UT}data") )->elements;
		my $expected_model	= memory_model;
		eval {
			if (blessed($resdata)) {
				$expected_model->load_urls_into_graph($self->default_graph, $resdata);
			}
		};
		if ($@) {
			fail($test->as_string);
			$self->record_result('update', 0, $test->value, $TODO);
			print "# died: " . $test->value . ": $@\n";
			return;
		};
		foreach my $gdata (@resgdata) {
			my ($data)	= ($model->objects( $gdata, iri("${UT}data") )->elements)[0] || ($model->objects( $gdata, iri("${UT}graph") )->elements)[0];
			my ($graph)	= $model->objects( $gdata, iri("${RDFS}label") )->elements;
			my $uri		= $graph->value;
			my $return	= 0;
			if ($data) {
				eval {
					$expected_model->load_urls_into_graph(iri($uri), $data);
				};
				if ($@) {
					fail($test->as_string);
					$self->record_result('update', 0, $test->value, $TODO);
					print "# died: " . $test->value . ": $@\n";
					$return	= 1;
				};
				return if ($return);
			}
		}
	
		if ($self->debug) {
			warn "Dataset before update operation:\n";
			warn $self->model_as_string($test_model);
		}
		
		my $ok	= 0;
		eval {
			my $algebra	= eval { Attean->get_parser('SPARQL')->parse_update($sparql) };
			if ($@) {
				warn "Failed to parse query $filename: $@";
				die $@;
			}
			unless ($algebra) {
				warn "No algebra generated for update\n";
				fail($test->value);
				return;
			}
			if ($self->debug) {
				warn "# Algebra:\n" . $algebra->as_string . "\n";
			}
		
			my $default_graphs	= [$self->default_graph];
			my $planner	= Attean::IDPQueryPlanner->new();
			my $plan	= $planner->plan_for_algebra($algebra, $test_model, $default_graphs);
			if ($self->debug) {
				warn "# Plan:\n" . $plan->as_string . "\n";
			}
	
			if ($self->debug) {
				warn "Running update...\n";
			}
			my $iter	= $plan->evaluate($test_model);
			$iter->elements;
			if ($self->debug) {
				warn "done.\n";
			}
		
			if ($self->debug) {
				warn "Comparing results...\n";
			}
			my $eqtest	= Attean::BindingEqualityTest->new();
			my $eq		= $eqtest->equals($test_model, $expected_model);
			if ($self->debug) {
				warn "done.\n";
			}
			$ok			= is( $eq, 1, $test->value );
			unless ($ok) {
				warn $eqtest->error;
				warn "Got model:\n" . $self->model_as_string($test_model);
				warn "Expected model:\n" . $self->model_as_string($expected_model);
			}
		};
		if ($@) {
			warn "Failed to execute update: $@";
			fail($test->value);
		}
		if (not($ok)) {
			print "# failed: " . $test->value . "\n";
		}
	
		$self->record_result('update', $ok, $test->value, $TODO);
		warn "ok\n" if ($self->debug);
	}
}

sub fix_local_to_w3c {
	my $self		= shift;
	my $file_url	= shift;
	$file_url		=~ s{^.*/rdf-tests/}{https://w3c.github.io/rdf-tests/}; # XXX hack
	return $file_url;
}

sub construct_data {
	my $self		= shift;
	my $model		= shift;
	my $action		= shift;
	my %args		= @_;
	my $specVersion	= $args{'specVersion'} // '1.1';

	my ($queryd)	= $model->objects( $action, iri("${RQ}query") )->elements;
	my @data		= $model->objects( $action, iri("${RQ}data") )->elements;
	my @gdata		= $model->objects( $action, iri("${RQ}graphData") )->elements;
	my @sdata		= $model->objects( $action, iri("${RQ}serviceData") )->elements;
	my @cdata		= $model->objects( $action, iri("${RQ}constructDataFile") )->elements;
	my ($fnode)		= $model->objects( $action, iri("${RQ}format") )->elements;

	my $format		= blessed($fnode) ? $fnode->value : 'text/turtle';

	my $uri					= URI->new( $queryd->value );
	my $filename			= $uri->file;
	my (undef,$base,undef)	= File::Spec->splitpath( $filename );
	$base					= "file://${base}";
	warn "Loading SPARQL query from file $filename" if ($self->debug);
	my $sparql				= eval { read_file($filename, { binmode => ':utf8' }) };
	if ($@) {
		warn "$!: $filename";
		return;
	}
	
	my $test_model	= $self->test_model();
	foreach my $data (@data) {
		if (blessed($data)) {
			$test_model->load_urls_into_graph($self->default_graph, $data);
		}
	}
	foreach my $g (@gdata) {
		my $start = $test_model->size;
		my $file_url	= $g->value;
		$file_url		= $self->fix_local_to_w3c($file_url);
		$test_model->load_urls_into_graph(iri($file_url), $g);
		my $end	= $test_model->size;
		unless ($start < $end) {
			warn "*** Loading file did not result in any new quads: " . $g;
		}
	}
	foreach my $n (@cdata) {
		my ($bytes, $format)	= $self->construct_data($model, $n, %args);
		my $p					= Attean->get_parser(media_type => $format)->new();
		my $iter				= $p->parse_iter_from_bytes($bytes);
		$test_model->add_iter($iter->as_quads($self->default_graph));
	}
	
	print STDERR "ok\n" if ($self->debug);

	if ($self->debug) {
		my $q	= $sparql;
		$q		=~ s/([\x{256}-\x{1000}])/'\x{' . sprintf('%x', ord($1)) . '}'/eg;
		warn $q;
	}
	
	my $test_base	= $base;
	$test_base		= $self->fix_local_to_w3c($test_base);

	my ($iter, $type)		= $self->get_actual_results( $filename, $test_model, $sparql, $test_base, $model, \@sdata, %args );
	my $sclass				= Attean->get_serializer(media_type => $format);
	my $s					= $sclass->new();
	my $bytes				= $s->serialize_iter_to_bytes($iter);
	return ($bytes, $format);
}

sub query_eval_test {
	my $self		= shift;
	my $model		= shift;
	my $test		= shift;
	my %args		= @_;
	my $specVersion	= $args{'specVersion'} // '1.1';
	my $testname	= $test->value;
	
	my ($action)	= $model->objects( $test, iri("${MF}action") )->elements;
	my ($result)	= $model->objects( $test, iri("${MF}result") )->elements;
	my ($req)		= $model->objects( $test, iri("${MF}requires") )->elements;
	my ($approved)	= $model->objects( $test, iri("${DAWGT}approval") )->elements;
	my ($res_card)	= $model->objects( $test, iri("${MF}resultCardinality") )->elements;
	my ($queryd)	= $model->objects( $action, iri("${RQ}query") )->elements;
	my @data		= $model->objects( $action, iri("${RQ}data") )->elements;
	my @gdata		= $model->objects( $action, iri("${RQ}graphData") )->elements;
	my @sdata		= $model->objects( $action, iri("${RQ}serviceData") )->elements;
	my @cdata		= $model->objects( $action, iri("${RQ}constructDataFile") )->elements;
	my @entailment	= $model->objects( $action, [iri("${SD}entailmentRegime"), iri("${SD}EntailmentProfile")] )->elements;
	my $reduced_mode	= (defined($res_card) and $res_card->value eq "${MF}LaxCardinality");
	
	if ($self->strict_approval) {
		unless ($approved) {
			warn "- skipping test because it isn't approved\n" if ($self->debug);
			return;
		}
		if ($approved->equal("${DAWGT}NotClassified")) {
			warn "- skipping test because its approval is dawgt:NotClassified\n" if ($self->debug);
			return;
		}
	}
	
	if (scalar(@entailment)) {
		$self->record_result('unhandled', 0, $test->as_string, $TODO);
		return;
	}

	my $uri					= URI->new( $queryd->value );
	my $filename			= $uri->file;
	my (undef,$base,undef)	= File::Spec->splitpath( $filename );
	$base					= "file://${base}";
	warn "Loading SPARQL query from file $filename\n" if ($self->debug);

	my $test_base	= $base;
	$test_base		= $self->fix_local_to_w3c($test_base);

	my $sparql				= eval { read_file($filename, { binmode => ':utf8' }) };
	if ($@) {
		warn("$!: $filename; " . $test->as_string);
		$self->record_result('evaluation', 0, $test->as_string, $TODO);
		return;
	}
	
	my $q			= $sparql;
	$q				=~ s/\s+/ /g;
	if ($self->debug) {
		warn "### test           : " . $test->value . "\n";
		warn "# sparql           : $q\n";
		warn "# file             : $filename\n";
		foreach my $data (@data) {
			warn "# data             : " . ($data->value =~ s#file://##r) . "\n" if (blessed($data));
		}
		warn "# graph data       : " . ($_->value =~ s#file://##r) . "\n" for (@gdata);
		warn "# constructed data : " . ($_->value =~ s#file://##r) . "\n" for (@cdata);
		warn "# result           : " . ($result->value =~ s#file://##r) . "\n";
		warn "# requires         : " . ($req->value =~ s#file://##r) . "\n" if (blessed($req));
	}
	
	print STDERR "constructing model...\n" if ($self->debug);
	my $test_model	= $self->test_model();
	TODO: {
		local($TODO)	= (blessed($req)) ? "requires " . $req->value : '';
		try {
			foreach my $data (@data) {
				my $file_url	= $data->value;
				$file_url		= $self->fix_local_to_w3c($file_url);
				if (blessed($data)) {
					$test_model->load_urls_into_graph_with_base($self->default_graph, [$data], $file_url);
				}
			}
			
			if ($self->debug) {
				warn "TEST MODEL AFTER DEFAULT GRAPH LOAD:\n";
				$self->debug_model($test_model);
			}

			foreach my $g (@gdata) {
				my $start = $test_model->size;
				my $file_url	= $g->value;
				$file_url		= $self->fix_local_to_w3c($file_url);
				$test_model->load_urls_into_graph_with_base(iri($file_url), [$g], $file_url);
				my $end	= $test_model->size;
				unless ($start < $end) {
					warn "*** Loading file did not result in any new quads: " . $g;
				}
			}
			foreach my $n (@cdata) {
				my ($bytes, $format)	= $self->construct_data($model, $n, %args);
				my $p					= Attean->get_parser(media_type => $format)->new();
				my $iter				= $p->parse_iter_from_bytes($bytes);
				$test_model->add_iter($iter->as_quads($self->default_graph));
			}
		} catch {
			fail($test->value);
			$self->record_result('evaluation', 0, $test->value, $TODO);
			print "# died: " . $test->value . ": $_\n";
			return;
		};
		print STDERR "ok\n" if ($self->debug);
	
		my $resuri		= URI->new( $result->value );
		my $resfilename	= $resuri->file;
	
		my $comment;
		eval {
			if ($self->debug) {
				my $q	= $sparql;
				$q		=~ s/([\x{256}-\x{1000}])/'\x{' . sprintf('%x', ord($1)) . '}'/eg;
				warn $q;
			}
			
			my ($actual, $type);
			{
				local($::DEBUG)	= 1;
				print STDERR "getting actual results...\n" if ($self->debug);
				($actual, $type)		= eval { $self->get_actual_results( $filename, $test_model, $sparql, $test_base, $model, \@sdata, %args ) };
				if ($@) {
					warn $@;
					fail($test->value);
					$self->record_result('evaluation', 0, $test->value, $TODO);
					return;
				}
				print STDERR "ok\n" if ($self->debug);
			}
		
			print STDERR "getting expected results...\n" if ($self->debug);
			my $expected	= $self->get_expected_results( $resfilename, $type );
			print STDERR "ok\n" if ($self->debug);
		
		#	warn "comparing results...";
			$self->compare_results( $expected, $actual, $test->value, \$comment, $reduced_mode, $TODO );
		};
		
		my $ok	= not($@);
		unless ($ok) {
			warn $@;
			fail($test->value);
			$self->record_result('evaluation', 0, $test->value, $TODO);
		};
		if ($ok) {
		} else {
			print "# failed: " . $test->value . "\n";
		}
	}
}

sub mock_endpoints {
	my $self			= shift;
	my $mock			= shift;
	my $model	= shift;
	my $sdata			= shift;
	
	foreach my $sd (@$sdata) {
		my ($e)		= $model->objects( $sd, iri("${RQ}endpoint") )->elements;
		my @data	= $model->objects( $sd, iri("${RQ}data") )->elements;
		my @gdata	= $model->objects( $sd, iri("${RQ}graphData") )->elements;

		my $endpoint	= $e->value;
		my $test_model	= $self->test_model();
		foreach my $data (@data) {
			if (blessed($data)) {
				$test_model->load_urls_into_graph($self->default_graph, $data);
			}
		}
		foreach my $g (@gdata) {
			my $start = $test_model->size;
			$test_model->load_urls_into_graph($g, $g);
			my $end	= $test_model->size;
			unless ($start < $end) {
				warn "*** Loading file did not result in any new quads: " . $g;
			}
		}
		$mock->register_test_endpoint($endpoint, [$test_model, $self->default_graph]);
	}
}

sub debug_model {
	my $self			= shift;
	my $model			= shift;
	if (shift || $self->debug) {
		my $iter	= $model->get_quads;
		warn "Dataset:\n-------------\n";
		my $s		= Attean->get_serializer('TextTable')->new();
		my $table	= decode_utf8($s->serialize_iter_to_bytes($iter));
		warn $table;
		warn "-------------\n";
	}
}

sub get_actual_results {
	my $self			= shift;
	my $filename		= shift;
	my $model			= shift;
	my $sparql			= shift;
	my $base			= shift;
	my $manifest_model	= shift;
	my $sdata			= shift;
	my %args			= @_;
	my $specVersion	= $args{'specVersion'} // '1.1';

	my $bytes	= encode_utf8($sparql);

	my $s 			= AtteanX::Parser::SPARQL->new(base => $base, version => $specVersion);
	my $algebra;
	eval {
		($algebra)	= $s->parse_list_from_bytes($bytes);
	};
	if ($@) {
		warn "Failed to parse query $filename: $@";
		warn "--------- [contents of $filename]\n";
		warn $bytes . "\n";
		warn "---------\n";
		die $@;
	}
	
	if ($self->debug) {
		warn "Walking algebra:\n";
		warn $algebra->as_string;
	}
	
	$self->debug_model($model);
	
	my $testns	= 'http://example.com/test-results#';
	my $rmodel	= memory_model();

	my $results;
	if ($self->use_idp_planner) {
		my $default_graphs	= [$self->default_graph];
		my $planner	= Test::Attean::TestIDPQueryPlanner->new();
		$self->mock_endpoints($planner, $manifest_model, $sdata);
		my $plan	= $planner->plan_for_algebra($algebra, $model, $default_graphs);
		if ($self->debug) {
			warn "Walking plan:\n";
			warn $plan->as_string;
		}
		$results	= eval { $plan->evaluate($model) };
		warn $@ if $@;
	} else {
		my $e		= Test::Attean::TestSimpleQueryEvaluator->new( model => $model, default_graph => $self->default_graph );
		$self->mock_endpoints($e, $manifest_model, $sdata);
		$results	= eval { $e->evaluate($algebra, $self->default_graph) };
		warn $@ if $@;
	}
	my $count	= 1;
	
	unless (blessed($results)) {
		die 'Failed to produce actual results';
	}
	
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

	$self->print_results("Actual results", \$results) if ($self->results);
	return ($results, $type);
	
	if ($results->is_bindings) {
		return ($results, 'bindings');
	} elsif ($results->is_boolean) {
		$rmodel->add_statement( triple( iri("${testns}result"), iri("${testns}boolean"), literal(($results->get_boolean ? 'true' : 'false'), undef, "${XSD}boolean") ) );
		return ($rmodel->get_statements, 'boolean');
	} elsif ($results->is_graph) {
		return ($results, 'graph');
	} else {
		warn "unknown result type: " . Dumper($results);
	}
}

sub print_results {
	my $self	= shift;
	my $name	= shift;
	my $results	= shift;
	$$results	= $$results->materialize;
	print "$name:\n";
	my $count	= 1;
	while (my $r = $$results->next) {
		printf("%3d %s\n", $count++, $r->as_string);
	}
	$$results->reset;
}

sub get_expected_results {
	my $self		= shift;
	my $file		= shift;
	my $type		= shift;
	
	if ($type eq 'graph') {
		my $model	= memory_model();
		$model->load_urls_into_graph($self->default_graph, iri("file://$file"));
		my $results	= $model->get_quads->map(sub { shift->as_triple }, 'Attean::API::Triple');
		$self->print_results("Expected results", \$results) if ($self->results);
		return $results;
	} elsif ($file =~ /[.](srj|json)/) {
		my $model	= memory_model();
		open(my $fh, '<', $file) or die $!;
		my $parser	= Attean->get_parser('SPARQLJSON')->new();
		my $results	= $parser->parse_iter_from_io($fh)->materialize;
		my $item	= $results->peek;
		if (blessed($item) and $item->does('Attean::API::Term')) {
			if ($self->results) {
				warn "Expected result: " . $item->as_string . "\n";
			}
			return $results;
		} else {
			$self->print_results("Expected results", \$results) if ($self->results);
			return $results;
		}
	} elsif ($file =~ /[.]srx/) {
		my $model	= memory_model();
		my $parser	= Attean->get_parser('sparqlxml')->new();
		open(my $fh, '<', $file);
		my $results	= $parser->parse_iter_from_io($fh);
		
		$self->print_results("Expected results", \$results) if ($self->results);
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
		my $results	= Attean::ListIterator->new(values => \@data, item_type => 'Attean::API::Result', variables => \@vars);
		$self->print_results("Expected results", \$results) if ($self->results);
		return $results;
	} elsif ($file =~ /[.]tsv/) {
		my $parser	= Attean->get_parser('SPARQLTSV')->new();
		open( my $fh, "<:encoding(utf8)", $file ) or die $!;
		my $iter	= $parser->parse_iter_from_io($fh);
		return $iter;
	} elsif ($file =~ /[.](ttl|rdf|nt)/) {
		my $model	= memory_model();
		my $file_url	= "file://$file";
		my $base		= $file_url;
		$base		= $self->fix_local_to_w3c($base);
		$model->load_urls_into_graph_with_base($self->default_graph, [iri($file_url)], iri($base));
		my ($res)	= $model->subjects( iri("${RDF}type"), iri("${RS}ResultSet") )->elements;
		if (my($b) = $model->objects( $res, iri("${RS}boolean") )->elements) {
			my $bool	= $b->value;
			my $term	= literal(value => $bool, datatype => "${XSD}boolean");
			if ($self->results) {
				warn "Expected result: " . $term->as_string . "\n";
			}
			return Attean::ListIterator->new(values => [$term], item_type => 'Attean::API::Term');
		} else {
			my @vars	= $model->objects( $res, iri("${RS}resultVariable") )->elements;
			my @sols	= $model->objects( $res, iri("${RS}solution") )->elements;
			my @names	= map { $_->value } @vars;
			my @bindings;
			my %vars;
			foreach my $r (@sols) {
				my %data;
				my @b	= $model->objects( $r, iri("${RS}binding") )->elements;
				foreach my $b (@b) {
					my ($value)	= $model->objects( $b, iri("${RS}value") )->elements;
					my ($var)	= $model->objects( $b, iri("${RS}variable") )->elements;
					$data{ $var->value }	= $value;
					$vars{ $var->value }++;
				}
				push(@bindings, Attean::Result->new( bindings => \%data ));
			}
			my $results	= Attean::ListIterator->new(values => \@bindings, item_type => 'Attean::API::Result', variables => [keys %vars]);
			$self->print_results("Expected results", \$results) if ($self->results);
			return $results;
		}
	} else {
		die "Unrecognized type of expected results: $file";
	}
}

sub compare_results {
	my $self		= shift;
	my $expected	= shift->canonicalize->materialize;
	my $actual		= shift->canonicalize->materialize;
	my $test		= shift;
	my $comment		= shift || do { my $foo; \$foo };
	my $reduced_mode	= shift;
	my $todo		= shift;
	if ($reduced_mode) {
		for ($actual, $expected) {
			$_	= $_->uniq();
		}
	}
	
	if ($reduced_mode and $self->debug) {
		warn "Comparing data using REDUCED cardinality semantics\n";
	}
	
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

		my $ok		= eval { ok( $eqtest->equals( $actual, $expected ), $test ) or diag($eqtest->error) };
		if ($@) {
			diag($@);
		}
		$self->record_result('evaluation', $ok, $test, $todo);
		return $ok;
	} elsif ($actual->does('Attean::API::TermIterator')) {
		my $a	= $actual->next;
		my $e	= $expected->next;
		my $name	= $self->debug ? sprintf("$test: %s == %s", $a->as_string, $e->as_string) : $test;
		my $eq	= $a->equals($e);
		my $ok	= ok( $eq, $name );
		$self->record_result('evaluation', $ok, $test, $todo);
		return $ok;
	} else {
		die "Unexpected result type $actual";
	}
}

sub record_result {
	my $self	= shift;
	my $type	= shift;
	my $ok		= shift;
	my $name	= shift;
	my $todo	= shift;
	$name		= $self->fix_local_to_w3c($name);
	unless ($ok) {
		if ($todo) {
			push(@{ $self->failures->{'TODO'}{$type} }, $name);
		} else {
			push(@{ $self->failures->{$type} }, $name);
		}
	}
	
	if (my $earl = $self->earl) {
		if ($ok) {
			$earl->pass($name);
		} else {
			$earl->fail($name);
		}
	}
}

sub model_as_string {
	my $self	= shift;
	my $model	= shift;
	my $ser		= Attean->get_serializer('nquads');
	my $sep		= ('####' x 25) . "\n";
	my $s		= sprintf("Model with %d quads:\n", $model->size);
	$s			.= $ser->serialize_iter_to_bytes($model->get_quads);
	return $sep . $s . $sep;
}

sub enable_earl {
	my $self	= shift;
	my %args	= @_;
	my $conf	= $args{ conf } || {};
	if (my $filename = $args{ filename }) {
		$self->earl_filename($filename);
	}
	
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
			my $uuid	= 'urn:uuid:' . uc(uuid_to_string(create_uuid()));
			$subject	= iri($uuid);
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
# 			warn "Adding metadata: " . $t->as_string . "\n";
			$model->add_quad($t->as_quad($graph));
		}
		foreach my $t ($self->earl_metadata($subject->value)) {
# 			warn "Adding metadata: " . $t->as_string . "\n";
			$model->add_quad($t->as_quad($graph));
		}
			
		return $earl;
	}
	return;
}

sub earl_metadata {
	my $self	= shift;
	my $subj	= shift;
	my $prefixes	= q[
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix doap: <http://usefulinc.com/ns/doap#>.
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
	];
	
	my $ver			= $Attean::VERSION;
	my $release		= 'urn:uuid:' . uc(uuid_to_string(create_uuid(UUID_V5, "${subj}-${ver}")));
	my $ttl		= $prefixes . <<"END";
<${subj}> a doap:Project ;
	doap:name "Attean" ;
	doap:homepage <https://github.com/kasei/attean> ;
    rdfs:seeAlso <irc://irc.perl.org/perlrdf> ;
    doap:license <http://opensource.org/licenses/Artistic>;
    doap:programming-language "Perl" ;
	doap:developer <http://kasei.us/about/#greg> ;
	doap:release <${release}> ;
	.

<${release}>
		a doap:Release ;
		doap:revision "${ver}" ;
	.

<http://kasei.us/about/#greg>
	a foaf:Person ;
    foaf:name "Gregory Todd Williams" ;
    foaf:mbox_sha1sum "f80a0f19d2a0897b89f48647b2fb5ca1f0bc1cb8" ;
    foaf:homepage <http://kasei.us/> ;
    rdfs:seeAlso <http://kasei.us/about/foaf.ttl> ;
    foaf:account [
        a foaf:OnlineAccount ;
        foaf:accountName "kasei" ;
        foaf:accountServiceHomepage <http://github.com/>
    ] ;
	.
END
	my $parser	= Attean->get_parser('Turtle')->new();
	my $iter = $parser->parse_iter_from_bytes(encode_utf8($ttl));
	return $iter->elements;
}

sub DESTROY {
	my $self	= shift;
	my $count	= 0;
	my $todo	= 0;
	while (my ($type, $failures) = each(%{ $self->failures })) {
		if ($type eq 'TODO') {
			while (my ($type, $failures) = each(%$failures)) {
				$todo	+= scalar(@$failures);
			}
			next;
		}
		$count	+= scalar(@$failures);
	}
	my $timestamp	= $self->timestamp;
	if ($self->run_query_tests and ($count or $todo)) {
		my $d	= Data::Dumper->new([$self->failures], [qw(tests)]);
		$d->Sortkeys(1)->Indent(2);
		my $msg	= "Failing and/or TODO tests: " . $d->Dump;
		warn $msg;
		unless ($self->pattern) {
			open(my $fh, '>', sprintf('.sparql-test-suite-%d', $timestamp)) or die $!;
			while (my ($type, $failures) = each(%{ $self->failures })) {
				if ($type eq 'TODO') {
					while (my ($type, $failures) = each(%$failures)) {
						say $fh "$type - TODO";
						say $fh join("\n", sort @$failures);
					}
					next;
				}
				say $fh $type;
				say $fh join("\n", sort @$failures);
			}
		}
	}
	if (my $earl = $self->earl and my $filename = $self->earl_filename) {
		open(my $fh, '>', $filename) or die $!;
		my $model	= $earl->model;
		my $iter	= $model->get_quads()->as_triples();
		my $rdf		= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
		my $EARL	= 'http://www.w3.org/ns/earl#';
		my $map		= URI::NamespaceMap->new( { earl => iri($EARL), rdf => iri($rdf) } );
		my $ser		= Attean->get_serializer('Turtle')->new(namespaces => $map);
		$ser->serialize_iter_to_io($fh, $iter);
	}
}

###############################################################################

1;
