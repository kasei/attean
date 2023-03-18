use v5.14;
use autodie;
use utf8;
use Test::Modern;
use Test::Exception;
use FindBin qw($Bin);
use File::Glob qw(bsd_glob);
use File::Spec;

use Attean;
use Attean::RDF;
use AtteanX::SPARQL::Constants;
use Attean::SimpleQueryEvaluator;
use Type::Tiny::Role;
use AtteanX::Functions::CompositeLists;
use AtteanX::Functions::CompositeMaps;
AtteanX::Functions::CompositeLists->register();
AtteanX::Functions::CompositeMaps->register();

sub plan_eval {
	my $sparql			= shift;
	my $model			= shift;
	my $active_graph	= shift;
	my $s 				= Attean->get_parser('SPARQL')->new();
	my ($algebra)		= $s->parse($sparql);
	my $results			= $model->evaluate($algebra, $active_graph);
	return $results;
}

sub simple_eval {
	my $sparql			= shift;
	my $model			= shift;
	my $active_graph	= shift;
	my $e				= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $active_graph );
	my $s 				= Attean->get_parser('SPARQL')->new();
	my ($algebra)		= $s->parse($sparql);
	my $results			= $e->evaluate($algebra, $active_graph);
	return $results;
}

sub evaluations {
	return (\&simple_eval);
# 	return (\&simple_eval, \&plan_eval);
}

subtest 'UNFOLD value' => sub {
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $sparql	= <<"END";
PREFIX cdt: <http://example.org/cdt/>
SELECT * WHERE {
	UNFOLD(cdt:sequence(10) AS ?x)
}
END
	my $algebra	= $parser->parse($sparql);
	does_ok($algebra, 'Attean::API::Algebra');
	isa_ok($algebra, 'Attean::Algebra::Query');
	my ($e)		= $algebra->subpatterns_of_type('Attean::Algebra::Unfold');
	isa_ok($e, 'Attean::Algebra::Unfold');

	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= Attean::IRI->new('http://example.org/graph');
	foreach my $evalfunc (evaluations()) {
		note('starting test with new evaluation function...');
		my $results		= $evalfunc->($sparql, $model, $graph);
		my @rows		= $results->elements;
		is(scalar(@rows), 10, 'expected explode cardinality');
	}
	
};

subtest 'UNFOLD extension' => sub {
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $sparql	= <<"END";
PREFIX cdt: <http://example.org/cdt/>
SELECT * WHERE {
	BIND(cdt:sequence(10) AS ?list)
	UNFOLD(?list AS ?x)
}
END
	my $algebra	= $parser->parse($sparql);
	does_ok($algebra, 'Attean::API::Algebra');
	isa_ok($algebra, 'Attean::Algebra::Query');
	my ($e)		= $algebra->subpatterns_of_type('Attean::Algebra::Unfold');
	isa_ok($e, 'Attean::Algebra::Unfold');

	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= Attean::IRI->new('http://example.org/graph');
	foreach my $evalfunc (evaluations()) {
		note('starting test with new evaluation function...');
		my $results		= $evalfunc->($sparql, $model, $graph);
		my @rows		= $results->elements;
		is(scalar(@rows), 10, 'expected explode cardinality');
		my $row	= $rows[0];
		foreach my $expected (1 .. 10) {
			my $row	= shift(@rows);
			my $list	= $row->value('list');
			is($list->datatype->value, $AtteanX::Functions::CompositeLists::LIST_TYPE_IRI, 'list value is present');
			is($row->value('x')->numeric_value, $expected, "list element $expected exploded properly");
		}
	}
	
};

subtest 'UNFOLD extension zip pairs' => sub {
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $sparql	= <<"END";
PREFIX cdt: <http://example.org/cdt/>
SELECT ?x ?y WHERE {
	# correlate two sequences, returning matching elements as (?x, ?y)
	BIND(cdt:sequence(5) AS ?list_x)
	BIND(cdt:sequence(101, 105) AS ?list_y)
	BIND(cdt:zip(?list_x, ?list_y) AS ?zipped)
	UNFOLD(?zipped AS ?pair)
	BIND(cdt:get(?pair, 1) AS ?x)
	BIND(cdt:get(?pair, 2) AS ?y)
}
END
	my $algebra	= $parser->parse($sparql);
	does_ok($algebra, 'Attean::API::Algebra');
	isa_ok($algebra, 'Attean::Algebra::Query');

	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= Attean::IRI->new('http://example.org/graph');
	foreach my $evalfunc (evaluations()) {
		note('starting test with new evaluation function...');
		my $results		= $evalfunc->($sparql, $model, $graph);
		my @rows		= $results->elements;
		is(scalar(@rows), 5, 'expected explode cardinality');
		foreach my $i (0 .. $#rows) {
			my $row	= $rows[$i];
			my $x	= $row->value('x');
			my $y	= $row->value('y');
			is($x->numeric_value, $i+1, 'expected x value');
			is($y->numeric_value, $i+101, 'expected correlated y value');
		}
	}
	
};

done_testing();

sub expect {
	my $token	= shift;
	my $type	= shift;
	my $values	= shift;
	my $name	= shift // '';
	if (length($name)) {
		$name	= "${name}: ";
	}
	is($token->type, $type, "${name}token type");
	is_deeply($token->args, $values, "${name}token values");
}
