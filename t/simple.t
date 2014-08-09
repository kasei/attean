use Test::More;
use Test::Exception;
use Test::Moose;

use v5.14;
use warnings;
no warnings 'redefine';

use RDF;

{
	note('RDF::Variable');
	my $a	= RDF::Variable->new('foo');
	does_ok($a, 'RDF::TermOrVariable');
	is($a->value, 'foo', 'value');
	is($a->ntriples_string, '?foo', 'ntriples_string');
}

{
	note('RDF::Blank');
	my $a	= RDF::Blank->new('foo');
	does_ok($a, 'RDF::API::Term');
	does_ok($a, 'RDF::TermOrVariable');
	is($a->value, 'foo', 'value');
	is($a->ntriples_string, '_:foo', 'ntriples_string');
}

{
	note('RDF::Literal (lang)');
	my $a	= RDF::Literal->new(value => 'foo', language => 'en-US');
	does_ok($a, 'RDF::API::Term');
	does_ok($a, 'RDF::API::Literal');
	does_ok($a, 'RDF::TermOrVariable');
	is($a->value, 'foo', 'value');
	is($a->language, 'en-US', 'language');
	isa_ok($a->datatype, 'IRI', 'datatype IRI');
	is($a->datatype->as_string, 'http://www.w3.org/2001/XMLSchema#string', 'language literal datatype is xsd:string');
	is($a->ntriples_string, '"foo"@en-US', 'ntriples_string');
}

{
	note('RDF::Literal (typed)');
	my $a	= RDF::Literal->new(value => '123', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
	does_ok($a, 'RDF::API::Term');
	does_ok($a, 'RDF::API::Literal');
	does_ok($a, 'RDF::TermOrVariable');
	is($a->value, '123', 'value');
	is($a->language, undef, 'no language method on typed literals');
	isa_ok($a->datatype, 'IRI', 'datatype IRI');
	is($a->datatype->as_string, 'http://www.w3.org/2001/XMLSchema#integer', 'language literal datatype is xsd:integer');
	is($a->ntriples_string, '"123"^^<http://www.w3.org/2001/XMLSchema#integer>', 'ntriples_string');
}

{
	note('RDF::IRI');
	my $a	= RDF::IRI->new('http://example.org/');
	does_ok($a, 'RDF::API::Term');
	is($a->value, 'http://example.org/', 'value');
	is($a->ntriples_string, '<http://example.org/>', 'ntriples_string');
}

{
	note('RDF::Triple');
	my $s	= RDF::Blank->new('x');
	my $p	= RDF::IRI->new('http://example.org/p');
	my $o	= RDF::Literal->new(value => 'foo', language => 'en-US');
	my $t	= RDF::Triple->new($s, $p, $o);
	does_ok($t, 'RDF::API::Triple');
	isa_ok($t, 'RDF::Triple');
	
	does_ok($t->subject, 'RDF::BlankOrIRI');
	isa_ok($t->predicate, 'RDF::IRI');
	does_ok($t->object, 'RDF::API::Term');
	
	is($t->tuples_string, '_:x <http://example.org/p> "foo"@en-US', 'tuples string');
}

done_testing();