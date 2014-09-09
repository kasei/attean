use v5.14;
use utf8;
use Data::Dumper;
use Test::More;
use Type::Tiny::Role;
use Attean::RDF;

is(iri('http://example.org/')->ntriples_string, '<http://example.org/>', 'IRI ntriples_string');
is(iri('http://example.org/✪')->ntriples_string, '<http://example.org/\u272A>', 'unicode IRI ntriples_string');
is(literal("🐶\\\n✪")->ntriples_string, qq["🐶\\\\\\n✪"], 'unicode literal ntriples_string');
is(literal('Eve')->ntriples_string, '"Eve"', 'literal ntriples_string');
is(langliteral('Eve', 'en')->ntriples_string, '"Eve"@en', 'lang-literal ntriples_string');
is(blank('eve')->ntriples_string, '_:eve', 'blank ntriples_string');

ok(Attean::Literal->integer(1)->ebv, '1 EBV');
ok(not(Attean::Literal->integer(0)->ebv), '0 EBV');
ok(not(literal('')->ebv), '"" EBV');
ok(literal('foo')->ebv, '"foo" EBV');
ok(blank('foo')->ebv, '_:foo EBV');
ok(iri('foo')->ebv, '<foo> EBV');

done_testing();
