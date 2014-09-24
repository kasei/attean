use Test::More;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Type::Tiny::Role;

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

{
	note('List helper methods');
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
}

done_testing();


sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
