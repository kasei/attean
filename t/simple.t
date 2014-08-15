use Test::More;
use Test::Exception;
use Test::Moose;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;

{
	note('Attean::Variable');
	my $a	= Attean::Variable->new('foo');
	does_ok($a, 'Attean::TermOrVariable');
	is($a->value, 'foo', 'value');
	is($a->ntriples_string, '?foo', 'ntriples_string');
}

{
	note('Attean::Blank');
	my $a	= Attean::Blank->new('foo');
	does_ok($a, 'Attean::API::Term');
	does_ok($a, 'Attean::TermOrVariable');
	is($a->value, 'foo', 'value');
	is($a->ntriples_string, '_:foo', 'ntriples_string');
}

{
	note('Attean::Literal (lang)');
	my $a	= Attean::Literal->new(value => 'foo', language => 'en-US');
	does_ok($a, 'Attean::API::Term');
	does_ok($a, 'Attean::API::Literal');
	does_ok($a, 'Attean::TermOrVariable');
	is($a->value, 'foo', 'value');
	is($a->language, 'en-US', 'language');
	isa_ok($a->datatype, 'IRI', 'datatype IRI');
	is($a->datatype->as_string, 'http://www.w3.org/2001/XMLSchema#string', 'language literal datatype is xsd:string');
	is($a->ntriples_string, '"foo"@en-US', 'ntriples_string');
}

{
	note('Attean::Literal (typed)');
	my $a	= Attean::Literal->new(value => '123', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
	does_ok($a, 'Attean::API::Term');
	does_ok($a, 'Attean::API::Literal');
	does_ok($a, 'Attean::TermOrVariable');
	is($a->value, '123', 'value');
	is($a->language, undef, 'no language method on typed literals');
	isa_ok($a->datatype, 'IRI', 'datatype IRI');
	is($a->datatype->as_string, 'http://www.w3.org/2001/XMLSchema#integer', 'language literal datatype is xsd:integer');
	is($a->ntriples_string, '"123"^^<http://www.w3.org/2001/XMLSchema#integer>', 'ntriples_string');
}

{
	note('Attean::IRI');
	my $a	= Attean::IRI->new('http://example.org/');
	does_ok($a, 'Attean::API::Term');
	is($a->value, 'http://example.org/', 'value');
	is($a->ntriples_string, '<http://example.org/>', 'ntriples_string');
}

{
	note('Attean::Triple');
	my $s	= Attean::Blank->new('x');
	my $p	= Attean::IRI->new('http://example.org/p');
	my $o	= Attean::Literal->new(value => 'foo', language => 'en-US');
	my $t	= Attean::Triple->new($s, $p, $o);
	does_ok($t, 'Attean::API::Triple');
	isa_ok($t, 'Attean::Triple');
	
	does_ok($t->subject, 'Attean::API::BlankOrIRI');
	isa_ok($t->predicate, 'Attean::IRI');
	does_ok($t->object, 'Attean::API::Term');
	
	is($t->tuples_string, '_:x <http://example.org/p> "foo"@en-US', 'tuples string');
}

done_testing();