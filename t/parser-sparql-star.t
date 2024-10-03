use v5.14;
use autodie;
use utf8;
use Test::Modern;
use Test::Exception;
use FindBin qw($Bin);
use File::Glob qw(bsd_glob);
use File::Spec;

use Attean;
use Attean::RDF;
use AtteanX::SPARQL::Constants;
use Type::Tiny::Role;

subtest 'triple-pattern subject' => sub {
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $q		= $parser->parse("PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT * WHERE { << ?s a foaf:Person >> foaf:believedBy <http://kasei.us/about/#greg> }");
	does_ok($q, 'Attean::API::Algebra');
	isa_ok($q, 'Attean::Algebra::Query');
	my $p		= $q->child;
	isa_ok($p, 'Attean::Algebra::Project');
	my $s		= $p->child;
	isa_ok($s, 'Attean::Algebra::BGP');
	my $triples	= $s->triples();
	is(scalar(@$triples), 2, 'number of triples');
	my ($reification, $t)	= sort { $b->predicate->value eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies' } @$triples;
	isa_ok($t->subject, 'Attean::Blank');
	isa_ok($reification->object, 'Attean::TriplePattern');
	my $tp	= $reification->object;
	is($tp->as_string, '?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .');
	is($t->predicate->as_string, 'http://xmlns.com/foaf/0.1/believedBy');
	is($t->object->as_string, 'http://kasei.us/about/#greg');
};

subtest 'triple-pattern object' => sub {
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $q		= $parser->parse("PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT * WHERE { <http://kasei.us/about/#greg> foaf:believes << ?s a foaf:Person >> }");
	does_ok($q, 'Attean::API::Algebra');
	isa_ok($q, 'Attean::Algebra::Query');
	my $p		= $q->child;
	isa_ok($p, 'Attean::Algebra::Project');
	my $s		= $p->child;
	isa_ok($s, 'Attean::Algebra::BGP');
	my $triples	= $s->triples();
	is(scalar(@$triples), 2, 'number of triples');
	my ($reification, $t)	= sort { $b->predicate->value eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies' } @$triples;
	my $tp	= $reification->object;
	is($t->subject->as_string, 'http://kasei.us/about/#greg');
	is($t->predicate->as_string, 'http://xmlns.com/foaf/0.1/believes');
	isa_ok($reification->object, 'Attean::TriplePattern');
	is($reification->object->as_string, '?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .');
};

subtest 'triple-pattern bind' => sub {
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $q		= $parser->parse("PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT * WHERE { BIND(<<( ?s a foaf:Person )>> AS ?tp) }");
	does_ok($q, 'Attean::API::Algebra');
	isa_ok($q, 'Attean::Algebra::Query');
	my $p		= $q->child;
	isa_ok($p, 'Attean::Algebra::Project');
	my $e	= $p->child;
	isa_ok($e, 'Attean::Algebra::Extend');
	my $expr	= $e->expression;
	isa_ok($expr, 'Attean::ValueExpression');
	my $value	= $expr->value;
	isa_ok($value, 'Attean::TriplePattern');
};

subtest 'object annotation 1' => sub {
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $q		= $parser->parse("PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT * WHERE { ?s a foaf:Person {| foaf:believedBy <http://kasei.us/about/#greg> |} }");
	does_ok($q, 'Attean::API::Algebra');
	isa_ok($q, 'Attean::Algebra::Query');
	my $p		= $q->child;
	isa_ok($p, 'Attean::Algebra::Project');
	my $s		= $p->child;
	isa_ok($s, 'Attean::Algebra::BGP');
	my $triples	= $s->triples();
	is(scalar(@$triples), 3, 'number of triples');
	my ($reification, @ta)	= sort { $b->predicate->value eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies' } @$triples;
	my ($t, $a)				= sort { $b->predicate->value eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' } @$triples;
	isa_ok($reification->object, 'Attean::TriplePattern');
	is($reification->object->as_string, '?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .');
	is($a->predicate->as_string, 'http://xmlns.com/foaf/0.1/believedBy');
	is($a->object->as_string, 'http://kasei.us/about/#greg');
	foreach my $pos (qw(subject predicate object)) {
		is($reification->object->$pos()->as_string, $t->$pos()->as_string);
	}
};

subtest 'object annotation 2' => sub {
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $q		= $parser->parse("PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT * WHERE { ?s a foaf:Person {| foaf:believedBy <http://kasei.us/about/#greg> ; a <http://example.org/Assertion> |} }");
	does_ok($q, 'Attean::API::Algebra');
	isa_ok($q, 'Attean::Algebra::Query');
	my $p		= $q->child;
	isa_ok($p, 'Attean::Algebra::Project');
	my $s		= $p->child;
	isa_ok($s, 'Attean::Algebra::BGP');
	my $triples	= $s->triples();
	is(scalar(@$triples), 4, 'number of triples');
	my ($reification, @ta)	= sort { $b->predicate->value eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#reifies' } @$triples;
	my ($t, @a)		= sort { $b->object->as_string eq 'http://xmlns.com/foaf/0.1/Person' } @$triples;
	my ($a1, $a2)	= sort { $a->predicate->value eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type' } @a;
	isa_ok($reification->object, 'Attean::TriplePattern');
	my $reif	= $reification->subject;
	is($reification->object->as_string, '?s <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .');
	is($a1->subject->as_string, $reif->as_string);
	is($a2->subject->as_string, $reif->as_string);

	is($a1->predicate->as_string, 'http://xmlns.com/foaf/0.1/believedBy');
	is($a1->object->as_string, 'http://kasei.us/about/#greg');

	is($a2->predicate->as_string, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
	is($a2->object->as_string, 'http://example.org/Assertion');

	foreach my $pos (qw(subject predicate object)) {
		is($reification->object->$pos()->as_string, $t->$pos()->as_string);
		is($a1->subject->value, $reif->value);
		is($a2->subject->value, $reif->value);
	}
};

subtest 'sparql-star tokens' => sub {
	my $sparql	= "SELECT * { << ?s a <http://xmlns.com/foaf/0.1/Person> >> <http://xmlns.com/foaf/0.1/believedBy> <http://kasei.us/about/#greg> }";
	open(my $fh, '<:encoding(UTF-8)', \$sparql);
	my $l		= AtteanX::Parser::SPARQLLex->new();
	my $iter	= $l->parse_iter_from_io($fh);
	
	expect($iter->next, KEYWORD, ['SELECT']);
	expect($iter->next, STAR, ['*']);
	expect($iter->next, LBRACE, ['{'],);
	expect($iter->next, LTLT, ['<<'],);
	expect($iter->next, VAR, ['s'], 'subject');
	expect($iter->next, A, ['a'], 'rdf:type');
	expect($iter->next, IRI, ['http://xmlns.com/foaf/0.1/Person'], 'foaf:Person');
	expect($iter->next, GTGT, ['>>'],);
	expect($iter->next, IRI, ['http://xmlns.com/foaf/0.1/believedBy'], 'believedBy');
	expect($iter->next, IRI, ['http://kasei.us/about/#greg'], '#greg');
	expect($iter->next, RBRACE, ['}'], 'escaped closing brace');
};

done_testing();

sub expect {
	my $token	= shift;
	my $type	= shift;
	my $values	= shift;
	my $name	= shift // '';
	if (length($name)) {
		$name	= "${name}: ";
	}
	is($token->type, $type, "${name}token type (" . join(',', @$values) . ')');
	is_deeply($token->args, $values, "${name}token values");
}
