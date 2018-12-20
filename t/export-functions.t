use Test::Modern;

use v5.14;
use warnings;

use Attean;
use Attean::RDF;

ok(my $vfoo = variable('foo'), 'Variable ?foo assignment OK');
isa_ok($vfoo, 'Attean::Variable');
does_ok($vfoo, 'Attean::API::Variable');
is($vfoo->value, 'foo', 'Variable ?foo has name');

ok(my $vbar = variable('bar'), 'Variable ?bar assignment OK');

ok(my $prop = iri('http://example.org/prop'), 'Variable iri prop assignment OK');
isa_ok($prop, 'Attean::IRI');
does_ok($prop, 'Attean::API::Term');
is($prop->value, 'http://example.org/prop', 'Variable iri prop has iri');

ok(my $t1 = triplepattern($vfoo, $prop, $vbar), 'Variable triplepattern Assignment OK');
isa_ok($t1, 'Attean::TriplePattern');
does_ok($t1, 'Attean::API::TriplePattern');
is($t1->as_string, '?foo <http://example.org/prop> ?bar .', 'Pattern string OK');


ok(my $lit = literal('Foobar'), 'Variable literal assignment OK');
isa_ok($lit, 'Attean::Literal');
does_ok($prop, 'Attean::API::Term');
is($lit->value, 'Foobar', 'Literal string OK');

ok(my $t2 = triplepattern($vbar, iri('http://example.org/prop2'), $lit), 'Variable triplepattern 2 assignment OK');
is($t2->as_string, '?bar <http://example.org/prop2> "Foobar" .', 'Pattern string OK');




ok(my $bgp = bgp($t1, $t2), 'Variable bgp assignment OK');
isa_ok($bgp, 'Attean::Algebra::BGP');
does_ok($bgp, 'Attean::API::Algebra');
is($bgp->as_string, "- BGP { ?foo <http://example.org/prop> ?bar ., ?bar <http://example.org/prop2> \"Foobar\" . }\n", 'Pattern string OK');


done_testing;
