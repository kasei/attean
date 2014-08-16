use Test::More;
use Test::Exception;
use Test::Moose;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;

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
		my $q	= Attean::Quad->new($s2, $p, $o, $g);
		$model->add_quad($q);
	}
	is($model->size, 4);
	is($model->count_quads($s), 1);
	is($model->count_quads($s2), 3);
	is($model->count_quads(), 4);
	is($model->count_quads(undef, $p), 2);
	{
		my $iter	= $model->get_quads($s2);
		while (my $q = $iter->next()) {
			my $o	= $q->object->value;
			like($o, qr/^[123]$/, "Literal value: $o");
		}
	}
	
	$model->remove_quad($q);
	is($model->size, 3);
	is($model->count_quads(undef, $p), 1);
}

done_testing();
