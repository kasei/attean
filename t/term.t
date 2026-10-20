use v5.14;
use utf8;
use Data::Dumper;
use Test::Modern;
use Test::Exception;
use Type::Tiny::Role;
use Attean::RDF;

my $XSD	= "http://www.w3.org/2001/XMLSchema#";

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
dies_ok( sub { blank('foo')->ebv }, '_:foo EBV: TypeError');
dies_ok(sub { iri('foo')->ebv }, '<foo> EBV');

is(dtliteral('1', "${XSD}integer")->numeric_value, 1, 'integer numeric value');
is(dtliteral('1.5', "${XSD}float")->numeric_value, 1.5, 'float numeric value');
is(dtliteral('2.2e3', "${XSD}double")->numeric_value, 2200, 'double numeric value');
is(dtliteral('2.5', "${XSD}decimal")->numeric_value, 2.5, 'decimal numeric value');

subtest 'term type check methods' => sub {
	my $xl	= literal("🐶\\\n✪");
	my $dtl	= dtliteral('1', "${XSD}integer");
	my $ll	= langliteral('Eve', 'en');
	foreach my $l ($xl, $dtl, $ll) {
		ok($l->is_literal);
		foreach my $type (qw(variable blank resource iri)) {
			my $method	= "is_$type";
			ok(not($l->$method()));
		}
	}
};

{
	my $l1	= literal(7);
	my $l2	= literal(10);
	is($l1->compare($l2), 1, 'non-numeric literal sort');
}

{
	my $i1	= Attean::Literal->integer(7);
	my $i2	= Attean::Literal->integer(10);
	
	does_ok($i1, 'Attean::API::NumericLiteral');
	does_ok($i2, 'Attean::API::NumericLiteral');
	
	is($i1->compare($i2), -1, 'numeric literal sort');
}

subtest 'XSD type promotion' => sub {
	{
		my $a	= dtliteral('2.2', 'http://www.w3.org/2001/XMLSchema#decimal');
		my $b	= dtliteral('1', 'http://www.w3.org/2001/XMLSchema#integer');
		is($a->binary_promotion_type($b, '+'), 'http://www.w3.org/2001/XMLSchema#decimal', 'xsd:decimal + xsd:integer');
	}
	{
		my $a	= dtliteral('1', 'http://www.w3.org/2001/XMLSchema#integer');
		my $b	= dtliteral('2.2', 'http://www.w3.org/2001/XMLSchema#decimal');
		is($a->binary_promotion_type($b, '+'), 'http://www.w3.org/2001/XMLSchema#decimal', 'xsd:integer + xsd:decimal');
	}
	{
		my $a	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#long');
		my $b	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#short');
		is($a->binary_promotion_type($b, '+'), 'http://www.w3.org/2001/XMLSchema#integer', 'xsd:long + xsd:short');
	}
	{
		my $a	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#float');
		my $b	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#short');
		is($a->binary_promotion_type($b, '+'), 'http://www.w3.org/2001/XMLSchema#float', 'xsd:float + xsd:short');
	}
	{
		my $a	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#short');
		my $b	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#float');
		is($a->binary_promotion_type($b, '+'), 'http://www.w3.org/2001/XMLSchema#float', 'xsd:short + xsd:float');
	}
	{
		my $a	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#positiveInteger');
		my $b	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#unsignedByte');
		is($a->binary_promotion_type($b, '+'), 'http://www.w3.org/2001/XMLSchema#integer', 'xsd:positiveInteger + xsd:unsignedByte');
	}
	{
		my $a	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#positiveInteger');
		my $b	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#unsignedByte');
		is($a->binary_promotion_type($b, '/'), 'http://www.w3.org/2001/XMLSchema#decimal', 'xsd:positiveInteger / xsd:unsignedByte');
	}
	{
		my $a	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#positiveInteger');
		my $b	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#unsignedByte');
		is($a->binary_promotion_type($b, '/'), 'http://www.w3.org/2001/XMLSchema#decimal', 'xsd:positiveInteger / xsd:unsignedByte');
	}
	{
		my $a	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#float');
		my $b	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#float');
		is($a->binary_promotion_type($b, '*'), 'http://www.w3.org/2001/XMLSchema#float', 'xsd:float * xsd:float');
	}
	{
		my $a	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#float');
		my $b	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#double');
		is($a->binary_promotion_type($b, '*'), 'http://www.w3.org/2001/XMLSchema#double', 'xsd:float * xsd:double');
	}
	{
		my $a	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#float');
		my $b	= dtliteral('2', 'http://www.w3.org/2001/XMLSchema#decimal');
		is($a->binary_promotion_type($b, '+'), 'http://www.w3.org/2001/XMLSchema#float', 'xsd:float + xsd:decimal');
	}
	{
		my $a	= dtliteral('3', 'http://www.w3.org/2001/XMLSchema#float');
		my $b	= dtliteral('1', 'http://www.w3.org/2001/XMLSchema#decimal');
		is($a->binary_promotion_type($b, '/'), 'http://www.w3.org/2001/XMLSchema#float', 'xsd:float / xsd:decimal');
	}
};

subtest 'TermOrVariable apply_binding' => sub {
	my $i	= Attean::Literal->integer(350);
	my $unbound	= Attean::Variable->new(value => 'number');
	my $bound	= Attean::Variable->new(value => 'x');
	my $b	= Attean::Result->new( bindings => { x => literal('foo'), z => blank('bar') } );
	
	my $a_i	= $i->apply_binding($b);
	does_ok($a_i, 'Attean::API::Literal');
	
	my $a_unbound	= $unbound->apply_binding($b);
	does_ok($a_unbound, 'Attean::API::Variable');
	
	my $a_bound	= $bound->apply_binding($b);
	does_ok($a_bound, 'Attean::API::Literal');
	is($a_bound->value, 'foo');
};

subtest 'blank comparison' => sub {
	my $b1	= blank('foo');
	my $b2	= blank('bar');
	my $b3	= blank('foo');
	is($b1->compare($b3), 0, 'blank term equality comparison');
	is($b1->compare($b2), 1, 'blank term equality comparison');
	is($b2->compare($b3), -1, 'blank term equality comparison');
};

subtest 'dateTime terms' => sub {
	my $dtlit1	= Attean::Literal->new( value => '1999-12-31T23:59:59', datatype => 'http://www.w3.org/2001/XMLSchema#dateTime' );
	does_ok($dtlit1, 'Attean::API::DateTimeLiteral');
	my $dt1	= $dtlit1->datetime;
	isa_ok($dt1, 'DateTime');

	my $dtlit2	= Attean::Literal->new( value => '2000-01-01T00:00:00', datatype => 'http://www.w3.org/2001/XMLSchema#dateTime' );
	does_ok($dtlit2, 'Attean::API::DateTimeLiteral');
	my $dt2	= $dtlit2->datetime;
	isa_ok($dt2, 'DateTime');

	my $dtlit3	= Attean::Literal->new( value => '1999-12-31T24:00:00', datatype => 'http://www.w3.org/2001/XMLSchema#dateTime' );
	does_ok($dtlit3, 'Attean::API::DateTimeLiteral');
	my $dt3	= $dtlit3->datetime;
	isa_ok($dt3, 'DateTime');
	
	is(0, $dt2->compare($dt3), 'support for 24:00:00 lexical format');
};

done_testing();
