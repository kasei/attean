package Test::Attean::SPARQLSuite;

use v5.14;
use warnings;
use Test::Roo::Role;

use Attean;
use Attean::RDF;
use AtteanX::Parser::SPARQL;
use Attean::SimpleQueryEvaluator;

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

requires 'test_model';

has run_update_tests	=> (is => 'rw', isa => Bool, default => 1);
has run_query_tests		=> (is => 'rw', isa => Bool, default => 1);

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

sub BUILD {
	my $self	= shift;
	if ($self->pattern) {
		$self->results(1);
	}
}

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
	unless (scalar(@manifests)) {
		plan skip_all => "No manifest files found in $dir";
		exit(0);
	}

	my $model	= $self->memory_model();
	my $class	= Attean->get_parser("turtle") || die "Failed to load parser for 'turtle'";

	my @load	= map { iri("file://" . File::Spec->rel2abs($_)) } @manifests;
	$model->load_urls_into_graph($self->default_graph, @load);

	warn "done parsing manifests" if $self->debug;
	$self->model($model);
	$self->manifests([$model->subjects( iri("${RDF}type"), iri("${MF}Manifest") )->elements]);
}

sub syntax_test {
	my $self		= shift;
	my $test_type	= shift;
	my $model		= shift;
	my $test		= shift;
	my $count		= shift // 1;
	
	my $type		= iri( "http://www.w3.org/1999/02/22-rdf-syntax-ns#type" );
	my $mfname		= iri( "http://www.w3.org/2001/sw/DataAccess/tests/test-manifest#name" );
	my ($queryd)	= $model->objects( $test, iri("${MF}action") )->elements;
	my ($approved)	= $model->objects( $test, iri("${DAWGT}approval") )->elements;
	my ($name)		= $model->objects( $test, $mfname )->elements;
	my $namevalue	= $name->value;
	
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

	my $is_pos_query	= $model->count_quads($test, $type, iri("${MF}PositiveSyntaxTest11"));
	my $is_pos_update	= $model->count_quads($test, $type, iri("${MF}PositiveUpdateSyntaxTest11"));
	my $is_neg_query	= $model->count_quads($test, $type, iri("${MF}NegativeSyntaxTest")) + $model->count_quads($test, $type, iri("${MF}NegativeSyntaxTest11"));
	my $is_neg_update	= $model->count_quads($test, $type, iri("${MF}NegativeUpdateSyntaxTest")) + $model->count_quads($test, $type, iri("${MF}NegativeUpdateSyntaxTest11"));
	
	my $uri					= URI->new( $queryd->value );
	my $filename			= $uri->file;
	my (undef,$base,undef)	= File::Spec->splitpath( $filename );
	$base					= "file://${base}";
	warn "Loading SPARQL query from file $filename" if ($self->debug);
	my $sparql				= do { local($/) = undef; open(my $fh, '<:utf8', $filename) or do { warn("$!: $filename; " . $test->as_string); return }; <$fh> };
	my $bytes	= encode_utf8($sparql);

	if ($self->debug) {
		my $q			= $sparql;
		$q				=~ s/\s+/ /g;
		warn "### test     : " . $test->as_string . "\n";
		warn "# file       : $filename\n";
		warn "# sparql     : $q\n";
	}
	
	my $pclass	= Attean->get_parser('SPARQL');
	my $parser	= $pclass->new();
	if ($test_type eq 'update') {
		$parser->update(1);
	}
	if ($is_pos_query or $is_pos_update) {
		my ($query)	= eval { $parser->parse_list_from_bytes($bytes) };
		my $ok	= blessed($query);
		$self->record_result('syntax', $ok, $test->as_string);
		if ($ok) {
			pass("syntax $namevalue: $filename");
		} else {
			fail("syntax $namevalue; $filename: $@");
		}
	} elsif ($is_neg_query or $is_neg_update) {
		my ($query)	= eval { $parser->parse_list_from_bytes($bytes) };
		my $ok	= $@ ? 1 : 0;
		$self->record_result('syntax', $ok, $test->as_string);
		if ($ok) {
			pass("syntax $namevalue: $filename");
		} else {
			if ($self->debug) {
				warn $query->as_string;
			}
			fail("syntax $namevalue; $filename (unexpected successful parse)");
		}
	}
}

sub update_eval_test {
	my $self		= shift;
	my $model		= shift;
	my $test		= shift;
	my $count		= shift // 1;
	
	my ($action)	= $model->objects( $test, iri("${MF}action") )->elements;
	my ($result)	= $model->objects( $test, iri("${MF}result") )->elements;
	my ($req)		= $model->objects( $test, iri("${MF}requires") )->elements;
	my ($approved)	= $model->objects( $test, iri("${DAWGT}approval") )->elements;
	my ($queryd)	= $model->objects( $action, iri("${UT}request") )->elements;
	my ($data)		= $model->objects( $action, iri("${UT}data") )->elements;
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
	my $sparql				= do { local($/) = undef; open(my $fh, '<', $filename) or do { fail("$!: $filename; " . $test->as_string); return }; binmode($fh, ':utf8'); <$fh> };

	my $q			= $sparql;
	$q				=~ s/\s+/ /g;
	if ($self->debug) {
		warn "### test     : " . $test->value . "\n";
		warn "# sparql     : $q\n";
		warn "# data       : " . $data->value . "\n" if (blessed($data));
		warn "# graph data : " . $_->value . "\n" for (@gdata);
		warn "# result     : " . $result->value . "\n";
		warn "# requires   : " . $req->value . "\n" if (blessed($req));
	}
	
	# TODO: set up remote endpoint mock

	warn "constructing model...\n" if ($self->debug);
	my $test_model	= $self->test_model();
	eval {
		if (blessed($data)) {
			$test_model->load_urls_into_graph($self->default_graph, $data);
		}
	};
	if ($@) {
		fail($test->value);
		print "# died: " . $test->value . ": $@\n";
		return;
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

	warn "ok\n" if ($self->debug);
}

sub query_eval_test {
	my $self		= shift;
	my $model		= shift;
	my $test		= shift;
	my $count		= shift // 1;
	
	my ($action)	= $model->objects( $test, iri("${MF}action") )->elements;
	my ($result)	= $model->objects( $test, iri("${MF}result") )->elements;
	my ($req)		= $model->objects( $test, iri("${MF}requires") )->elements;
	my ($approved)	= $model->objects( $test, iri("${DAWGT}approval") )->elements;
	my ($queryd)	= $model->objects( $action, iri("${RQ}query") )->elements;
	my ($data)		= $model->objects( $action, iri("${RQ}data") )->elements;
	my @gdata		= $model->objects( $action, iri("${RQ}graphData") )->elements;
	my @sdata		= $model->objects( $action, iri("${RQ}serviceData") )->elements;
	
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
	
	my $uri					= URI->new( $queryd->value );
	my $filename			= $uri->file;
	my (undef,$base,undef)	= File::Spec->splitpath( $filename );
	$base					= "file://${base}";
	warn "Loading SPARQL query from file $filename" if ($self->debug);
	my $sparql				= do { local($/) = undef; open(my $fh, '<', $filename) or do { warn("$!: $filename; " . $test->value); return }; binmode($fh, ':utf8'); <$fh> };
	
	my $q			= $sparql;
	$q				=~ s/\s+/ /g;
	if ($self->debug) {
		warn "### test     : " . $test->value . "\n";
		warn "# sparql     : $q\n";
		warn "# data       : " . ($data->value =~ s#file://##r) . "\n" if (blessed($data));
		warn "# graph data : " . ($_->value =~ s#file://##r) . "\n" for (@gdata);
		warn "# result     : " . ($result->value =~ s#file://##r) . "\n";
		warn "# requires   : " . ($req->value =~ s#file://##r) . "\n" if (blessed($req));
	}
	
STRESS:	foreach (1 .. $count) {
		print STDERR "constructing model... " if ($self->debug);
		my $test_model	= $self->test_model();
		try {
			if (blessed($data)) {
				$test_model->load_urls_into_graph($self->default_graph, $data);
			}
			foreach my $g (@gdata) {
				$test_model->load_urls_into_graph($g, $g);
			}
		} catch {
			fail($test->value);
			$self->record_result('evaluation', 0, $test->value);
			print "# died: " . $test->value . ": $_\n";
			next STRESS;
		};
		print STDERR "ok\n" if ($self->debug);
	
		my $resuri		= URI->new( $result->value );
		my $resfilename	= $resuri->file;
	
		TODO: {
			local($TODO)	= (blessed($req)) ? "requires " . $req->value : '';
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
					print STDERR "getting actual results... " if ($self->debug);
					($actual, $type)		= $self->get_actual_results( $filename, $test_model, $sparql, $base );
					print STDERR "ok\n" if ($self->debug);
				}
			
				print STDERR "getting expected results... " if ($self->debug);
				my $expected	= $self->get_expected_results( $resfilename, $type );
				print STDERR "ok\n" if ($self->debug);
			
			#	warn "comparing results...";
				$self->compare_results( $expected, $actual, $test->value, \$comment );
			};
			
			my $ok	= not($@);
			unless ($ok) {
				warn $@;
				fail($test->value);
				$self->record_result('evaluation', 0, $test->value);
			};
			if ($ok) {
			} else {
				print "# failed: " . $test->value . "\n";
			}
		}
	}
}

sub get_actual_results {
	my $self		= shift;
	my $filename	= shift;
	my $model		= shift;
	my $sparql		= shift;
	my $base		= shift;
	my $bytes	= encode_utf8($sparql);

	my $s 			= AtteanX::Parser::SPARQL->new(base => $base);
	my $algebra;
	eval {
		($algebra)	= $s->parse_list_from_bytes($bytes);
	};
	if ($@) {
		warn "Failed to parse query $filename: $@";
		die $@;
	}
	
	if ($self->debug) {
		warn "Walking algebra:\n";
		warn $algebra->as_string;
	}
	
	if ($self->debug) {
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
	if ($self->use_idp_planner) {
		my $default_graphs	= [$self->default_graph];
		my $planner	= Attean::IDPQueryPlanner->new();
		my $plan	= $planner->plan_for_algebra($algebra, $model, $default_graphs);
		if ($self->debug) {
			warn "Walking plan:\n";
			warn $plan->as_string;
		}
		$results	= $plan->evaluate($model);
	} else {
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $self->default_graph );
		$results	= $e->evaluate($algebra, $self->default_graph);
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
		$model->load_urls_into_graph($self->default_graph, iri("file://$file"));
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
		$self->record_result('evaluation', $ok, $test);
		return $ok;
	} elsif ($actual->does('Attean::API::TermIterator')) {
		my $a	= $actual->next;
		my $e	= $expected->next;
		my $name	= $self->debug ? sprintf("$test: %s == %s", $a->as_string, $e->as_string) : $test;
		my $ok		= ok( $a->equals($e), $name );
		$self->record_result('evaluation', $ok, $test);
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
	unless ($ok) {
		push(@{ $self->failures->{$type} }, $name);
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

sub DESTROY {
	my $self	= shift;
	my $count	= 0;
	while (my ($type, $failures) = each(%{ $self->failures })) {
		$count	+= scalar(@$failures);
	}
	if ($self->run_query_tests and $count) {
		my $d	= Data::Dumper->new([$self->failures], [qw(failures)]);
		$d->Sortkeys(1)->Indent(2);
		my $msg	= "Failing tests: " . $d->Dump;
		warn $msg;
		unless ($self->pattern) {
			open(my $fh, '>', sprintf('.sparql-test-suite-%d', scalar(time)));
			while (my ($type, $failures) = each(%{ $self->failures })) {
				say $fh $type;
				say $fh join("\n", sort @$failures);
			}
		}
	}
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
			warn "No mf:entries found for manifest " . $m->as_string . "\n";
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
