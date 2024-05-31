use v5.14;
use autodie;
use utf8;
use Test::Modern;

use Attean;
use Attean::RDF;

subtest 'Parser by file extension' => sub {
	is(Attean->get_parser('rq'), 'AtteanX::Parser::SPARQL', 'rq');
	is(Attean->get_parser('ru'), 'AtteanX::Parser::SPARQL', 'ru');
	is(Attean->get_parser('nt'), 'AtteanX::Parser::NTriples', 'nt');
	is(Attean->get_parser('srj'), 'AtteanX::Parser::SPARQLJSON', 'srj');
	is(Attean->get_parser('srx'), 'AtteanX::Parser::SPARQLXML', 'srx');
	is(Attean->get_parser('tsv'), 'AtteanX::Parser::SPARQLTSV', 'tsv');
	is(Attean->get_parser('ttl'), 'AtteanX::Parser::Turtle', 'ttl');
	is(Attean->get_parser('nq'), 'AtteanX::Parser::NQuads', 'nq');
	is(Attean->get_parser('rdf'), 'AtteanX::Parser::RDFXML', 'rdf');
	is(Attean->get_parser('xrdf'), 'AtteanX::Parser::RDFXML', 'xrdf');
	like(Attean->get_parser('rq'), qr'AtteanX::Parser::SPARQL', 'rq'); # may be SPARQL or SPARQLLex
};

subtest 'Serializer by file extension' => sub {
	is(Attean->get_serializer('rq'), 'AtteanX::Serializer::SPARQL', 'rq');
	is(Attean->get_serializer('ru'), 'AtteanX::Serializer::SPARQL', 'ru');
	is(Attean->get_serializer('txt'), 'AtteanX::Serializer::TextTable', 'txt');
	is(Attean->get_serializer('text'), 'AtteanX::Serializer::TextTable', 'text');
	like(Attean->get_serializer('nt'), qr'AtteanX::Serializer::\w*NTriples', 'nt'); # may be NTriples or CanonicalNTriples
	is(Attean->get_serializer('csv'), 'AtteanX::Serializer::SPARQLCSV', 'csv');
	is(Attean->get_serializer('srj'), 'AtteanX::Serializer::SPARQLJSON', 'srj');
	is(Attean->get_serializer('json'), 'AtteanX::Serializer::SPARQLJSON', 'json');
	is(Attean->get_serializer('srx'), 'AtteanX::Serializer::SPARQLXML', 'srx');
	like(Attean->get_serializer('xml'), qr'AtteanX::Serializer::(SPARQLXML|RDFXML)', 'xml');
	is(Attean->get_serializer('tsv'), 'AtteanX::Serializer::SPARQLTSV', 'tsv');
	is(Attean->get_serializer('nq'), 'AtteanX::Serializer::NQuads', 'nq');
	is(Attean->get_serializer('rdf'), 'AtteanX::Serializer::RDFXML', 'rdf');
	like(Attean->get_serializer('html'), qr'AtteanX::Serializer::[^:]*(HTML|RDFa)[^:]*', 'html'); # if AtteanX::Serializer::RDFa is installed, the html extension may map to either RDFa or SPARQLHTML
	like(Attean->get_serializer('ttl'), qr'AtteanX::Serializer::Turtle', 'ttl'); # may be Turtle or TurtleTokens
};

done_testing();

