use Test::Roo;
use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;

sub create_store {
	my $self	= shift;
	my %args	= @_;
	my $quads	= $args{quads} // [];
	my $store	= Attean->get_store('Memory')->new();
	foreach my $q (@$quads) {
		$store->add_quad($q);
	}
	return $store;
}

sub caching_sleep_time {
	return 2;
}

with 'Test::Attean::QuadStore', 'Test::Attean::MutableQuadStore';
with 'Test::Attean::MutableTimeCacheableQuadStore', 'Test::Attean::MutableETagCacheableQuadStore';
run_me; # run these Test::Attean tests

{
	my $store	= Attean->get_store('Memory')->new();
	isa_ok($store, 'AtteanX::Store::Memory');

	
	my $s	= Attean::Blank->new('x');
	my $p	= Attean::IRI->new('http://example.org/p1');
	my $o	= Attean::Literal->new(value => 'foo', language => 'en-US');
	my $g	= Attean::IRI->new('http://example.org/graph');
	my $q	= Attean::Quad->new($s, $p, $o, $g);
	does_ok($q, 'Attean::API::Quad');
	isa_ok($q, 'Attean::Quad');
	
	$store->add_quad($q);
	is($store->size, 1);
	
	{
		my $iter	= $store->get_quads($s);
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
		$store->add_quad($q);
	}
	is($store->size, 4);
	is($store->count_quads($s), 1);
	is($store->count_quads($s2), 3);
	is($store->count_quads(), 4);
	is($store->count_quads(undef, $p), 2);
	{
		my $iter	= $store->get_quads($s2);
		while (my $q = $iter->next()) {
			my $o	= $q->object->value;
			like($o, qr/^[123]$/, "Literal value: $o");
		}
	}
	
	$store->remove_quad($q);
	is($store->size, 3);
	is($store->count_quads(undef, $p), 1);
	
	$store->remove_quads(undef, iri('http://example.org/p2'));
	is($store->size, 2);

	$store->remove_quads(undef, [map { iri("http://example.org/p$_") } (1,3) ]);
	is($store->size, 0);
}

done_testing();
