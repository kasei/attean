use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Test::LWP::UserAgent;
use Attean parsers => ['Turtle'];
use Attean::RDF;
use Attean::SimpleQueryEvaluator;
use AtteanX::Functions::CompositeLists;

AtteanX::Functions::CompositeLists->register();

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

my %eval_types	= (
	'simple eval' => \&simple_eval,
	'plan eval' => \&plan_eval,
);

subtest 'cdt:get' => sub {
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= Attean::IRI->new('http://example.org/graph');
	$model->load_triples('turtle', $graph, qq[_:a <p> "[1,'b',3]"^^<${AtteanX::Functions::CompositeLists::LIST_TYPE_IRI}> .]);
	
	my $sparql	= <<"END";
PREFIX cdt: <http://example.org/cdt/>
SELECT * WHERE {
	?s ?p ?o .
	BIND(cdt:get(?o, 0) AS ?e1)
	BIND(cdt:get(?o, 1) AS ?e2)
	BIND(cdt:get(?o, 2) AS ?e3)
}
END

	while (my ($name, $evalfunc) = each(%eval_types)) {
		note($name);
		my $results		= $evalfunc->($sparql, $model, $graph);
		my $row			= $results->next;
		is($row->value('e1')->numeric_value, 1);
		is($row->value('e2')->value, 'b');
		is($row->value('e3')->numeric_value, 3);
	}
};

subtest 'listCreate' => sub {
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= Attean::IRI->new('http://example.org/graph');
	
	my $sparql	= <<"END";
PREFIX cdt: <http://example.org/cdt/>
SELECT * WHERE {
	BIND(cdt:listCreate(1, 2, 'c', 4) AS ?list)
}
END
	while (my ($name, $evalfunc) = each(%eval_types)) {
		note($name);
		my $results		= $evalfunc->($sparql, $model, $graph);
		my $row			= $results->next;
		is($row->value('list')->value, '[1,2,"c",4]');
	}
};

subtest 'listAgg' => sub {
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= Attean::IRI->new('http://example.org/graph');
	
	my $sparql	= <<"END";
PREFIX cdt: <http://example.org/cdt/>
SELECT (cdt:listAgg(?v) AS ?aggList) WHERE {
	VALUES ?v { 1 2 'c' 4 }
}
END
	while (my ($name, $evalfunc) = each(%eval_types)) {
		note($name);
		my $results		= $evalfunc->($sparql, $model, $graph);
		my $row			= $results->next;
		is($row->value('aggList')->value, '[1,2,"c",4]');
	}
};

subtest 'sequence' => sub {
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= Attean::IRI->new('http://example.org/graph');
	
	my $sparql	= <<"END";
PREFIX cdt: <http://example.org/cdt/>
SELECT * WHERE {
	BIND(cdt:sequence(3) AS ?list_3)       # [1,3]
	BIND(cdt:sequence(3, 5) AS ?list_3_5)  # [3,5]
}
END
	while (my ($name, $evalfunc) = each(%eval_types)) {
		note($name);
		my $results		= $evalfunc->($sparql, $model, $graph);
		my $row			= $results->next;
		is($row->value('list_3')->value, '[1,2,3]');
		is($row->value('list_3_5')->value, '[3,4,5]');
	}
};

subtest 'list_from_head' => sub {
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= Attean::IRI->new('http://example.org/graph');
	$model->load_triples('turtle', $graph, qq[<http://example.org/s> <http://example.org/p> ("a" "b" "c") .]);
	
	my $sparql	= <<"END";
PREFIX cdt: <http://example.org/cdt/>
SELECT * WHERE {
	<http://example.org/s> <http://example.org/p> ?head .
	BIND(cdt:list_from_head(?head) AS ?list)
}
END
	while (my ($name, $evalfunc) = each(%eval_types)) {
		note($name);
		my $results		= $evalfunc->($sparql, $model, $graph);
		my $row			= $results->next;
		is($row->value('list')->value, '["a","b","c"]');
	}
};

done_testing();
