use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::DatasetModel;
use Attean::RDF;
use Type::Tiny::Role;

{
  my $model = Attean::DatasetModel->new( graphs => [] );
  does_ok($model, 'Attean::API::Model');
}

my $s	= Attean::Blank->new('x');
my $s2	= Attean::IRI->new('http://example.org/values');
my $p	= Attean::IRI->new('http://example.org/p1');
my $o	= Attean::Literal->new(value => 'foo', language => 'en-US');
my $g1	= Attean::IRI->new('http://example.org/graph1');
my $g2	= Attean::IRI->new('http://example.org/graph2');
my $g3	= Attean::IRI->new('http://example.org/graph3');
my $g4	= Attean::IRI->new('http://example.org/graph4');
my $q	= Attean::Quad->new($s, $p, $o, $g1);

sub test_model {
	my $store	= Attean->get_store('Memory')->new();
	isa_ok($store, 'AtteanX::Store::Memory');
	my $model	= Attean::MutableQuadModel->new( store => $store );
	isa_ok($model, 'Attean::MutableQuadModel');
	
	does_ok($q, 'Attean::API::Quad');
	isa_ok($q, 'Attean::Quad');
	
	$model->add_quad($q);
	
	foreach my $value (1 .. 3) {
		my $o	= Attean::Literal->integer($value);
		my $p	= Attean::IRI->new("http://example.org/p$value");
		my $g	= Attean::IRI->new("http://example.org/graph" . ($value+1));
		my $q	= Attean::Quad->new($s2, $p, $o, $g);
		$model->add_quad($q);
	}
	return $model;
}

{
	my $model	= test_model();
	
	my $ds1	= Attean::DatasetModel->new( model => $model, graphs => [$g1]);
	is($ds1->count_quads(), 1);
	
	my $ds2	= Attean::DatasetModel->new( model => $model, graphs => [$g1, $g2]);
	is($ds2->count_quads(), 2);
	
	my $ds3	= Attean::DatasetModel->new( model => $model, graphs => [$g1, $g2, $g3]);
	is($ds3->count_quads(), 3);
	
	my $ds4	= Attean::DatasetModel->new( model => $model, graphs => [$g1, $g2, $g3, $g4]);
	is($ds4->count_quads(), 4);
	
	my $ds5	= Attean::DatasetModel->new( model => $model, graphs => [$s, $g2, $q, blank()]);
	is($ds5->count_quads(), 1);

# 	is($model->size, 4);
# 	is($model->count_quads($s), 1);
# 	is($model->count_quads($s2), 3);
# 	is($model->count_quads(), 4);
# 	is($model->count_quads(undef, $p), 2);
# 	ok($model->holds($s2));
# 	ok(!$model->holds($s2, $g1));
}

done_testing();
