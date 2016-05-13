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

subtest 'parser construction and metadata' => sub {
	{
		my $parser	= Attean->get_parser('SPARQL')->new();
		isa_ok( $parser, 'AtteanX::Parser::SPARQL' );
		is($parser->canonical_media_type, 'application/sparql-query', 'canonical_media_type');
		my %extensions	= map { $_ => 1 } @{ $parser->file_extensions };
		ok(exists $extensions{'rq'}, 'file_extensions');
	}
	{
		my $parser	= Attean->get_parser('SPARQLLex')->new();
		isa_ok( $parser, 'AtteanX::Parser::SPARQLLex' );
		is($parser->canonical_media_type, 'application/x-sparql-query-tokens', 'canonical_media_type');
		my %extensions	= map { $_ => 1 } @{ $parser->file_extensions };
		ok(exists $extensions{'rq'}, 'file_extensions');
	}
};

{
	my $parser	= Attean->get_parser('SPARQL')->new();
	isa_ok($parser, 'AtteanX::Parser::SPARQL');
	my $type	= $parser->handled_type;
	can_ok($type, 'role');
	is($type->role, 'Attean::API::Algebra');
}

{
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $q		= $parser->parse("SELECT * { ?s <p> '''hello!''' OPTIONAL { ?s <q> ?x } FILTER(!BOUND(?x)) } LIMIT 5 OFFSET 5");
	does_ok($q, 'Attean::API::Algebra');
	isa_ok($q, 'Attean::Algebra::Query');
	my $s		= $q->child;
	isa_ok($s, 'Attean::Algebra::Slice');
}

{
	my $data	= "ASK { ?s ?p ?o FILTER(?o > -2.0 && ?o < +3e0 ) }";
	open(my $fh, '<', \$data);
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $iter	= $parser->parse_iter_from_io($fh);
	does_ok($iter, 'Attean::API::Iterator');
	my $q		= $iter->next;
	does_ok($q, 'Attean::API::Algebra');
	my $a		= $q->child;
	isa_ok($a, 'Attean::Algebra::Ask');
}

{
	my $map		= URI::NamespaceMap->new();
	my $parser	= Attean->get_parser('SPARQL')->new( namespaces => $map );
	my $content	= <<'END';
PREFIX ex: <http://example.org/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
SELECT * WHERE {
	?s a foaf:Person ; foaf:name 'Alice'
}
OFFSET 10
END
	my ($q)	= $parser->parse_list_from_bytes($content);
	is_deeply([sort $map->list_prefixes], [qw(ex foaf)]);
	my $foaf	= $map->namespace_uri('foaf');
	isa_ok($foaf, 'URI::Namespace');
	is($foaf->as_string, 'http://xmlns.com/foaf/0.1/');
	my $a	= $q->child;
	isa_ok($a, 'Attean::Algebra::Slice')
}

subtest 'escaping' => sub {
	my $sparql	= q[ASK { <s> ex:p "\\"", '\\'', '\\u706b\\U0000661F' \\u007d];
	open(my $fh, '<:encoding(UTF-8)', \$sparql);
	my $l		= AtteanX::Parser::SPARQLLex->new();
	my $iter	= $l->parse_iter_from_io($fh);
	
	expect($iter->next, KEYWORD, ['ASK']);
	expect($iter->next, LBRACE, ['{'],);
	expect($iter->next, IRI, ['s'], 'subject');
	expect($iter->next, PREFIXNAME, ['ex:', 'p'], 'predicate');
	expect($iter->next, STRING1D, ['"'], 'double quote');
	expect($iter->next, COMMA, [',']);
	expect($iter->next, STRING1S, ["'"], 'single quote');
	expect($iter->next, COMMA, [',']);
	expect($iter->next, STRING1S, ["火星"], 'unicode \\u and \\U escapes');
	expect($iter->next, RBRACE, ['}'], 'escaped closing brace');
};

subtest 'custom function' => sub {
	my $sparql	= q[PREFIX ex: <http://example.org/> SELECT * WHERE { ?s ?p ?o FILTER(ex:test(?o)) }];
	open(my $fh, '<:encoding(UTF-8)', \$sparql);
	my $parser	= AtteanX::Parser::SPARQL->new();
	my ($a)		= $parser->parse($sparql);
	my ($f)		= $a->subpatterns_of_type('Attean::Algebra::Filter');
	isa_ok($f, 'Attean::Algebra::Filter');
	my $expr	= $f->expression;
	isa_ok($expr, 'Attean::FunctionExpression');
	is($expr->operator, 'INVOKE');
	my ($iri, $term)	= map { $_->value } @{ $expr->children };
	does_ok($iri, 'Attean::API::IRI');
	is($iri->value, 'http://example.org/test');
	does_ok($term, 'Attean::API::Variable');
	is($term->value, 'o');
};

subtest 'syntax coverage: top-level filter custom function call' => sub {
	my $sparql	= q[PREFIX ex: <http://example.org/> SELECT * WHERE { ?s ?p ?o FILTER ex:test(?o) }];
	my $a	= AtteanX::Parser::SPARQL->parse($sparql);
	my ($f)		= $a->subpatterns_of_type('Attean::Algebra::Filter');
	isa_ok($f, 'Attean::Algebra::Filter');
	my $expr	= $f->expression;
	isa_ok($expr, 'Attean::FunctionExpression');
	is($expr->operator, 'INVOKE');
	my ($iri, $term)	= map { $_->value } @{ $expr->children };
	does_ok($iri, 'Attean::API::IRI');
	is($iri->value, 'http://example.org/test');
	does_ok($term, 'Attean::API::Variable');
	is($term->value, 'o');
};

subtest 'parse coverage: NIL' => sub {
	my $a	= AtteanX::Parser::SPARQL->parse('SELECT * WHERE { ?s ?p () }');
	does_ok($a, 'Attean::API::Algebra');
	my ($bgp)	= $a->subpatterns_of_type('Attean::Algebra::BGP');
	isa_ok($bgp, 'Attean::Algebra::BGP');
	my @t		= @{ $bgp->triples };
	is(scalar(@t), 1);
	my $t		= $t[0];
	does_ok($t, 'Attean::API::TriplePattern');
	my $nil		= $t->object;
	does_ok($nil, 'Attean::IRI');
	is($nil->value, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil');
};

subtest 'parse coverage: TriplesSameSubject' => sub {
	my $a	= AtteanX::Parser::SPARQL->parse('CONSTRUCT { ( ?s ?o ) } WHERE { ?s ?p ?o }');
	does_ok($a, 'Attean::API::Algebra');
};

subtest 'parse coverage: RANK' => sub {
	local($TODO)	= 'Fix RANK projection';
	my $sparql	= <<"END";
PREFIX : <http://example.org/>
SELECT ?age ?name ?school WHERE {
    ?p
    	:name ?name ;
        :school ?school ;
        :age ?age
        .
}
GROUP BY ?school
RANK(ASC(?age)) AS ?rank
HAVING (?rank < 2)
END
	my $a	= eval { AtteanX::Parser::SPARQL->parse($sparql) };
	does_ok($a, 'Attean::API::Algebra');
};

subtest 'parse error' => sub {
	dies_ok {
		my $a	= AtteanX::Parser::SPARQL->parse('* WHERE { ?s ?p ?o }');
	} 'missing method';
	
	dies_ok {
		my $a	= AtteanX::Parser::SPARQL->parse_update('INSERT { <s> <p> <o> }');
	} 'unexpected EOF in update';
	
};

subtest 'parse_cb_from_bytes' => sub {
	my $cb	= sub {
		my $a	= shift;
		does_ok($a, 'Attean::API::Algebra');
	};
	my $p	= Attean->get_parser('SPARQL')->new(handler => $cb);
	my $data	= "ASK { ?s ?p ?o }";
	open(my $fh, '<', \$data);
	$p->parse_cb_from_io($fh);
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
	is($token->type, $type, "${name}token type");
	is_deeply($token->args, $values, "${name}token values");
}
