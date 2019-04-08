use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::RDF;
use Type::Tiny::Role;

{
  my $model = Attean->temporary_model;
  isa_ok($model, 'Attean::QuadModel');
  does_ok($model, 'Attean::API::MutableModel');
}


{
	my $store	= Attean->get_store('Memory')->new();
	isa_ok($store, 'AtteanX::Store::Memory');
	my $model	= Attean::MutableQuadModel->new( store => $store );
	isa_ok($model, 'Attean::MutableQuadModel');
	
	my $s	= Attean::Blank->new('x');
	my $p	= Attean::IRI->new('http://example.org/p1');
	my $o	= Attean::Literal->new(value => 'foo', language => 'en-US');
	my $g	= Attean::IRI->new('http://example.org/graph');
	my $q	= Attean::Quad->new($s, $p, $o, $g);
	does_ok($q, 'Attean::API::Quad');
	isa_ok($q, 'Attean::Quad');
	
	$model->add_quad($q);
	is($model->size, 1);
	
	{
		my $iter	= $model->get_quads($s);
		does_ok($iter, 'Attean::API::Iterator');
		my $q	= $iter->next;
		does_ok($q, 'Attean::API::Quad');
		my ($s, $p, $o, $g)	= $q->values;
		is($s->value, 'x');
		is($o->value, 'foo');
	}
	
	my $s2	= Attean::IRI->new('http://example.org/values');
	foreach my $value (1 .. 3) {
		my $o	= Attean::Literal->new(value => $value, datatype => 'http://www.w3.org/2001/XMLSchema#integer');
		my $p	= Attean::IRI->new("http://example.org/p$value");
		my $g	= Attean::IRI->new("http://example.org/graph" . ($value+1));
		my $q	= Attean::Quad->new($s2, $p, $o, $g);
		$model->add_quad($q);
	}
	is($model->size, 4);
	is($model->count_quads($s), 1);
	is($model->count_quads($s2), 3);
	is($model->count_quads(), 4);
	is($model->count_quads(undef, $p), 2);
	ok($model->holds($s2));
	ok(!$model->holds($s2, $g));

	{
		note('get_quads single-term matching with undef placeholders');
		my $iter	= $model->get_quads($s2);
		while (my $q = $iter->next()) {
			my $o	= $q->object->value;
			like($o, qr/^[123]$/, "Literal value: $o");
		}
	}
	
	{
		note('get_quads single-term matching with variable object placeholders');
		my @vars	= map { Attean::Variable->new($_) } qw(p o g);
		my $iter	= $model->get_quads($s2, @vars);
		does_ok($iter, 'Attean::API::Iterator');
		while (my $q = $iter->next()) {
			my $o	= $q->object->value;
			like($o, qr/^[123]$/, "Literal value: $o");
		}
	}
	
	{
		note('get_bindings single-term matching');
		my $v		= Attean::Variable->new('pred');
		my $iter	= $model->get_bindings($s2, $v);
		does_ok($iter, 'Attean::API::Iterator');
		my $count	= 0;
		while (my $b = $iter->next()) {
			$count++;
			does_ok($b, 'Attean::API::Result');
			is_deeply([$b->variables], [qw(pred)], 'expected binding variables');
			my $p	= $b->value('pred');
			my $v	= $p->value;
			does_ok($p, 'Attean::API::Term');
			like($v, qr<^http://example.org/p[123]$>, "Predicate value: $v");
		}
		is($count, 3, 'expected binding count');
	}
	
	{
		note('get_quads union-term matching');
		my $g2		= Attean::IRI->new("http://example.org/graph2");
		my $g3		= Attean::IRI->new("http://example.org/graph3");
		my $g4		= Attean::IRI->new("http://example.org/graph4");
		
		my $p1		= Attean::IRI->new("http://example.org/p1");
		my $p3		= Attean::IRI->new("http://example.org/p3");
		my $iter	= $model->get_quads(undef, [$p1, $p3], undef, [$g2, $g3, $g4]);
		my $count	= 0;
		while (my $q = $iter->next()) {
			$count++;
			my $o	= $q->object->value;
			like($o, qr/^[13]$/, "Literal value: $o");
		}
		is($count, 2);
	}
	
	note('removing data...');
	$model->remove_quad($q);
	is($model->size, 3);
	is($model->count_quads(undef, $p), 1);
	
	{
		note('objects() matching');
		my $objects	= $model->objects();
		does_ok($objects, 'Attean::API::Iterator');
		is($objects->item_type, 'Attean::API::Term', 'expected item_type');
		my $count	= 0;
		while (my $obj = $objects->next) {
			$count++;
			does_ok($obj, 'Attean::API::Literal');
			like($obj->value, qr/^[123]$/, "Literal value: $o");
		}
		is($count, 3);
	}
	
	{
		note('graphs() union-term matching');
		my $p1		= Attean::IRI->new("http://example.org/p1");
		my $p3		= Attean::IRI->new("http://example.org/p3");
		my $graphs	= $model->graphs(undef, [$p1, $p3]);
		does_ok($graphs, 'Attean::API::Iterator');
		is($graphs->item_type, 'Attean::API::Term', 'expected item_type');
		my $count	= 0;
		while (my $g = $graphs->next) {
			$count++;
			like($g->value, qr<^http://example.org/graph[24]$>, "Graph value: $g");
		}
		is($count, 2, 'expected graph count');
	}
}

subtest 'Model add_iter' => sub {
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	
	my $s	= Attean::Blank->new('x');
	my $p	= Attean::IRI->new('http://example.org/p1');
	my $o1	= Attean::Literal->new(value => 'foo', language => 'en-US');
	my $o2	= Attean::Literal->new(value => 'bar', language => 'en-GB');
	my $g	= Attean::IRI->new('http://example.org/graph');
	my $q1	= Attean::Quad->new($s, $p, $o1, $g);
	my $q2	= Attean::Quad->new($s, $p, $o2, $g);
	my $i	= Attean::ListIterator->new(values => [$q1, $q2], item_type => 'Attean::API::Quad');
	is($model->size, 0, 'size before add_iter');
	$model->add_iter($i);
	is($model->size, 2, 'size after add_iter');
};

subtest 'List helper methods' => sub {
	my $graph	= Attean::IRI->new('http://example.org/list-graph');
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	dies_ok { $model->add_list() } 'add_list with bad arguments';
	dies_ok { $model->get_list($graph) } 'get_list with bad arguments';
	my $head	= $model->add_list($graph, map { Attean::Literal->integer($_) } (1 .. 3));
	my $iter	= $model->get_quads;
# 	while (my $q  = $iter->next) { say $q->as_string }
	is($model->size, 6, 'expected add_list model size');
	my $list	= $model->get_list($graph, $head);
	does_ok($list, 'Attean::API::Iterator', 'get_list returned iterator');
	is_deeply([map { $_->value } $list->elements], [1,2,3], 'get_list elements');
};

subtest 'Sequence helper methods' => sub {
	my $graph	= Attean::IRI->new('http://example.org/list-graph');
	my $store	= Attean->get_store('Memory')->new();
	my $parser	= Attean->get_parser('ntriples')->new();
	my $data	= <<'END';
<http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/1999/02/22-rdf-syntax-ns#Seq> .
<http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_1> "banana" .
<http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_2> "apple" .
<http://example.org/favourite-fruit> <http://www.w3.org/1999/02/22-rdf-syntax-ns#_3> "pear" .
END
	my $iter	= $parser->parse_iter_from_bytes($data);
	my $quads	= $iter->as_quads($graph);
	$store->add_iter($quads);
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $seq		= $model->get_sequence($graph, iri('http://example.org/favourite-fruit'));
	does_ok($seq, 'Attean::API::Iterator', 'get_sequence returned iterator');
	is_deeply([map { $_->value } $seq->elements], [qw(banana apple pear)], 'get_sequence elements');
	
	$model->add_quad(quad(iri('http://example.org/favourite-fruit'), iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#_2'), literal('kiwi'), $graph));
	dies_ok { $model->get_sequence($graph, iri('http://example.org/favourite-fruit')) } 'get_sequence dies on invalid sequence data';
};

subtest 'holds and algebra_holds methods' => sub {
	my $graph	= Attean::IRI->new('http://example.org/graph');
	my $store	= Attean->get_store('Memory')->new();
	my $parser	= Attean->get_parser('turtle')->new();
	my $data	= <<'END';
@prefix : <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/> .
:alice a foaf:Person ; foaf:name "Alice" ; foaf:knows :bob .
:bob a foaf:Person ; foaf:name "Bob" ; foaf:knows :alice .
:eve a foaf:Person ; foaf:name "Eve" .
END
	my $iter	= $parser->parse_iter_from_bytes($data);
	my $quads	= $iter->as_quads($graph);
	$store->add_iter($quads);
	my $model	= Attean::MutableQuadModel->new( store => $store );
	
	ok($model->holds(iri('http://example.org/alice')), 'holds(subj)');
	ok($model->holds(iri('http://example.org/alice'), iri('http://xmlns.com/foaf/knows')), 'holds(subj, pred)');
	ok(!$model->holds(iri('http://example.org/eve'), iri('http://xmlns.com/foaf/knows')), '!holds(subj, pred)');
	ok($model->holds(triplepattern(iri('http://example.org/alice'), iri('http://xmlns.com/foaf/name'), variable('name'))), 'holds(triplepattern)');
	ok($model->algebra_holds(bgp(triplepattern(iri('http://example.org/alice'), iri('http://xmlns.com/foaf/name'), variable('name')), triplepattern(iri('http://example.org/alice'), iri('http://xmlns.com/foaf/knows'), variable('friend'))), $graph), 'algebra_holds(bgp)');
	ok(!$model->algebra_holds(bgp(triplepattern(iri('http://example.org/eve'), iri('http://xmlns.com/foaf/name'), variable('name')), triplepattern(iri('http://example.org/eve'), iri('http://xmlns.com/foaf/knows'), variable('friend'))), $graph), '!algebra_holds(bgp)');
};

package TruePlan {
	use Moo;
	extends 'Attean::Plan::Exists';

	sub plan_as_string { return 'AlwaysTrue' }
	sub impl {
		return sub {
			return Attean::ListIterator->new(values => [Attean::Literal->true], item_type => 'Attean::API::Term');
		}
	}
}

package AllAlgebrasHoldMemoryStore {
	use Moo;
	extends 'AtteanX::Store::Memory';
	with 'Attean::API::CostPlanner';

	sub plans_for_algebra {
		my $self	= shift;
		my $algebra	= shift;
		if ($algebra->isa('Attean::Algebra::Ask')) {
			return TruePlan->new();
		}
		return;
	}

	sub cost_for_plan {
		my $self	= shift;
		my $plan	= shift;
		if ($plan->isa('TruePlan')) { return 1 }
		return;
	}
}

subtest 'holds planning optimization' => sub {
	my $graph	= Attean::IRI->new('http://example.org/graph');
	my $store	= AllAlgebrasHoldMemoryStore->new();
	my $parser	= Attean->get_parser('turtle')->new();
	my $data	= <<'END';
@prefix : <http://example.org/> .
:x :p 1, 2, 3 .
END
	my $iter	= $parser->parse_iter_from_bytes($data);
	my $quads	= $iter->as_quads($graph);
	$store->add_iter($quads);
	my $model	= Attean::MutableQuadModel->new( store => $store );
	
	# holds() calls will fail because node of the matching data is in the store
	ok(!$model->holds(iri('http://example.org/alice')), 'holds(subj)');
	ok(!$model->holds(iri('http://example.org/alice'), iri('http://xmlns.com/foaf/knows')), 'holds(subj, pred)');
	ok(!$model->holds(iri('http://example.org/eve'), iri('http://xmlns.com/foaf/knows')), '!holds(subj, pred)');
	ok(!$model->holds(triplepattern(iri('http://example.org/alice'), iri('http://xmlns.com/foaf/name'), variable('name'))), 'holds(triplepattern)');

	# algebra_holds calls will pass because AllAlgebrasHoldMemoryStore will override query planning to return TruePlan query plans
	ok($model->algebra_holds(bgp(triplepattern(iri('http://example.org/alice'), iri('http://xmlns.com/foaf/name'), variable('name')), triplepattern(iri('http://example.org/alice'), iri('http://xmlns.com/foaf/knows'), variable('friend'))), $graph), 'algebra_holds(bgp)');
	ok($model->algebra_holds(bgp(triplepattern(iri('http://example.org/eve'), iri('http://xmlns.com/foaf/name'), variable('name')), triplepattern(iri('http://example.org/eve'), iri('http://xmlns.com/foaf/knows'), variable('friend'))), $graph), '!algebra_holds(bgp)');
};

done_testing();
