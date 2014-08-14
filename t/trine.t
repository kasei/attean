use Test::More;
use Test::Exception;
use Test::Moose;

use strict;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::TrineCompatibility;

{
	note('Attean::Literal (lang)');
	my $a	= Attean::Literal->new(value => 'foo', language => 'en-US');
	is($a->literal_value, 'foo', 'value');
	is($a->literal_value_language, 'en-US', 'language');
	isa_ok($a->literal_datatype, 'IRI', 'datatype IRI');
	is($a->literal_datatype->as_string, 'http://www.w3.org/2001/XMLSchema#string', 'language literal datatype is xsd:string');
	is($a->as_ntriples, '"foo"@en-US', 'ntriples_string');
}

{
	note('Attean::Literal (typed)');
	my $a	= Attean::Literal->new(value => '123', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
	is($a->literal_value, '123', 'value');
	lives_ok { $a->language } 'typed literal has language method';
	is(eval { $a->language }, undef, 'language returns undef for typed literal');
	isa_ok($a->datatype, 'IRI', 'datatype IRI');
	is($a->datatype->abs, 'http://www.w3.org/2001/XMLSchema#integer', 'language literal datatype is xsd:integer');
	is($a->ntriples_string, '"123"^^<http://www.w3.org/2001/XMLSchema#integer>', 'ntriples_string');
}

done_testing();