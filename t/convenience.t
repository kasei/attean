use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Test::LWP::UserAgent;
use Attean parsers => ['Turtle'];
use Attean::RDF;
use Attean::SimpleQueryEvaluator;

{
	my $data	= '<a> <b> 14 .';
	my $sparql	= 'SELECT ?o WHERE { ?s ?p ?o }';

	my $graph	= iri('http://example.org/');
	my $model	= Attean->temporary_model;
	$model->load_triples('turtle', $graph, $data);

	my $s 			= Attean->get_parser('SPARQL')->new();
	my ($algebra)	= $s->parse($sparql);
	my $results		= $model->evaluate($algebra, $graph);
	my $r			= $results->next;
	does_ok($r, 'Attean::API::Result');
	my $o			= $r->value('o');
	is($o->value, '14');
	does_ok($o, 'Attean::API::Literal');
}

done_testing();
