use Test::More;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::RDF;

{
	note('Attean::SimpleEvaluator');
	my $parser	= Attean->get_parser('Turtle')->new();
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	
	my $graph	= Attean::IRI->new('http://example.org/graph');
	{
		my $data	= <<"END";
		_:a <b> _:a .
		<a> <b> <a> .
		<a> <c> 2, 3 .
END
		my $iter	= $parser->parse_iter_from_bytes($data);
		my $quads	= $iter->as_quads($graph);
		$store->add_iter($quads);
	}
	
	my $e	= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $graph );
	isa_ok($e, 'Attean::SimpleQueryEvaluator');
	
	my $active_graph	= $graph;

	{
		my $t	= Attean::TriplePattern->new(map { variable($_) } qw(s p o));
		my $bgp	= Attean::Algebra::BGP->new( triples => [$t] );
		does_ok($bgp, 'Attean::API::Algebra');
	
		my $iter	= $e->evaluate($bgp, $active_graph);
		my $count	= 0;
		while (my $r = $iter->next) {
			$count++;
			does_ok($r, 'Attean::API::Result');
			my $s	= $r->value('s');
			is($s->value, 'a');
			my $p	= $r->value('p');
			does_ok($p, 'Attean::API::IRI');
			like($p->value, qr/^[bc]$/);
		}
		is($count, 4);
	}

	{
		my $t1	= Attean::TriplePattern->new(iri('a'), iri('b'), variable('o1'));
		my $t2	= Attean::TriplePattern->new(iri('a'), iri('c'), variable('o2'));
		my $bgp	= Attean::Algebra::BGP->new( triples => [$t1, $t2] );
		does_ok($bgp, 'Attean::API::Algebra');
	
		my $iter	= $e->evaluate($bgp, $active_graph);
		my $count	= 0;
		while (my $r = $iter->next) {
			$count++;
			like($r->as_string, qr[{o1=<a>, o2="[23]"\^\^<http://www.w3.org/2001/XMLSchema#integer>}]);
		}
		is($count, 2);
	}
}

done_testing();


sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
