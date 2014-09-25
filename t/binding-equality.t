use Test::More;
use Test::Exception;

use strict;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::RDF;
use Attean::BindingEqualityTest;

my $graph	= iri('http://example.org/');

note('Triples');

{
	my $foaf_a	= <<'END';
	@prefix foaf: <http://xmlns.com/foaf/0.1/> .
	<alice> foaf:knows <eve> .
END
	
	my $foaf_b	= <<'END';
	@prefix foaf: <http://xmlns.com/foaf/0.1/> .
	<alice> foaf:knows <eve> .
END
	
	my @models	= map { model_with_turtle($graph, $_) } ($foaf_a, $foaf_b);
	test_model_equality( @models, 1, 'equal graphs with no blank nodes' );
}

### 

{
	my $foaf_a	= <<'END';
	@prefix foaf: <http://xmlns.com/foaf/0.1/> .
	_:a a foaf:Person ; foaf:name "Alice" .
	_:b a foaf:Person ; foaf:name "Bob" .
	<x> <y> <z> .
END
	
	my $foaf_b	= <<'END';
	@prefix foaf: <http://xmlns.com/foaf/0.1/> .
	_:alice a foaf:Person ; foaf:name "Alice" .
	_:bob a foaf:Person ; foaf:name "Bob" .
	<x> <y> <z> .
END
	my @models	= map { model_with_turtle($graph, $_) } ($foaf_a, $foaf_b);
	test_model_equality( @models, 1, 'simple blank node map' );
	my $test	= Attean::BindingEqualityTest->new();
	my $map	= $test->injection_map(map { $_->get_quads } @models);
	is_deeply($map, {qw(a alice b bob)}, 'injection map');
}

{
	my $foaf_a	= <<'END';
	@prefix foaf: <http://xmlns.com/foaf/0.1/> .
	_:a foaf:knows _:eve .
END
	
	my $foaf_b	= <<'END';
	@prefix foaf: <http://xmlns.com/foaf/0.1/> .
	_:alice a foaf:Person ; foaf:knows _:b .
END
	my @models	= map { model_with_turtle($graph, $_) } ($foaf_a, $foaf_b);
	my $test	= Attean::BindingEqualityTest->new();
	my @iters	= map { $_->get_quads } @models;
	ok( $test->is_subgraph_of(@iters), "subgraph test with blank nodes" ) or diag($test->error);
	my $map	= $test->injection_map(map { $_->get_quads } @models);
	is_deeply($map, {qw(a alice eve b)}, 'injection map');
}

{
	my $foaf_a	= <<'END';
	@prefix foaf: <http://xmlns.com/foaf/0.1/> .
	[] a foaf:Person ; foaf:name "Alice" .
	<bob> a foaf:Person ; foaf:name "Bob" .
	<x> <y> <z> .
END
	
	my $foaf_b	= <<'END';
	@prefix foaf: <http://xmlns.com/foaf/0.1/> .
	_:alice a foaf:Person ; foaf:name "Alice" .
	_:bob a foaf:Person ; foaf:name "Bob" .
	<x> <y> <z> .
END
	my @models	= map { model_with_turtle($graph, $_) } ($foaf_a, $foaf_b);
	test_model_equality( @models, 0, 'blank node does not map to iri' );
}

{
	my $foaf_a	= "<x> <y> <z> .\n";
	my $foaf_b	= "<a> <b> <c> .\n";
	my @models	= map { model_with_turtle($graph, $_) } ($foaf_a, $foaf_b);
	test_model_equality( @models, 0, 'different non-blank statements' );
}

{
	my $foaf_a	= "_:a <knows> _:a .\n";
	my $foaf_b	= "_:a <knows> _:b .\n";
	my @models	= map { model_with_turtle($graph, $_) } ($foaf_a, $foaf_b);
	test_model_equality( @models, 0, 'different number of blank nodes' );
}

{
	my $foaf_a	= "_:a <knows> _:a .\n";
	my $foaf_b	= "_:a <knows> _:b, _:c.\n";
	my @models	= map { model_with_turtle($graph, $_) } ($foaf_a, $foaf_b);
	test_model_equality( @models, 0, 'different number of blank statements' );
}

note('Results');

{
	my $a	= Attean::Result->new();
	my $b	= Attean::Result->new();
	test_iter_equality(results_iter($a), results_iter($b), 1, 'empty results');
}

{
	my $a	= Attean::Result->new( bindings => { x => literal('x') } );
	my $b	= Attean::Result->new( bindings => { x => literal('y') } );
	test_iter_equality(results_iter($a), results_iter($b), 0, 'different IRIs results');
}

{
	my $a	= Attean::Result->new( bindings => { x => blank('x') } );
	my $b	= Attean::Result->new( bindings => { x => blank('y') } );
	test_iter_equality(results_iter($a), results_iter($b), 1, 'different blanks results');
}

{
	my $x	= blank('x');
	my $y	= blank('y');
	my $a	= Attean::Result->new( bindings => { foo => $x, bar => $y, baz => literal('1') } );
	my $b	= Attean::Result->new( bindings => { foo => $y, bar => $x, baz => literal('1') } );
	test_iter_equality(results_iter($a), results_iter($b), 1, 'multi-blank mapping results');
}

{
	my $x	= blank('x');
	my $y	= blank('y');
	my $a	= Attean::Result->new( bindings => { foo => $x, bar => $y, baz => literal('1') } );
	my $b	= Attean::Result->new( bindings => { foo => $y, bar => $x, baz => literal('1') } );
	my $test	= Attean::BindingEqualityTest->new();
	my $map	= $test->injection_map(results_iter($a), results_iter($b));
	is_deeply($map, {qw(x y y x)}, 'injection map');
}

done_testing();

sub model_with_turtle {
	my $graph	= shift;
	my $data	= shift;
	my $parser	= Attean->get_parser('Turtle')->new();
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $iter	= $parser->parse_iter_from_bytes($data);
	$store->add_iter($iter->as_quads($graph));
	return $model;
}

sub test_iter_equality {
	my $iter_a	= shift;
	my $iter_b	= shift;
	my $expect	= shift;
	my $name	= shift;
	my $test	= Attean::BindingEqualityTest->new();
	is( $test->equals( $iter_a, $iter_b ), $expect, $name ) or diag($test->error);
}

sub results_iter {
	my @results	= @_;
	return Attean::ListIterator->new(values => \@results, item_type => 'Attean::API::Result');
}

sub test_turtle_equality {
	my $rdf_a	= shift;
	my $rdf_b	= shift;
	
	my $model_a	= model_with_turtle(iri('http://example.org/'), $rdf_a);
	my $model_b	= model_with_turtle(iri('http://example.org/'), $rdf_b);

	return test_iter_equality($model_a->get_quads, $model_b->get_quads, @_);
}

sub test_model_equality {
	my ($model_a, $model_b)	= splice(@_, 0, 2);
	return test_iter_equality($model_a->get_quads, $model_b->get_quads, @_);
}
