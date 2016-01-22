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
	my $parser	= Attean->get_parser('SPARQL')->new();
	isa_ok( $parser, 'AtteanX::Parser::SPARQL' );
	is($parser->canonical_media_type, 'application/sparql-query', 'canonical_media_type');
	my %extensions	= map { $_ => 1 } @{ $parser->file_extensions };
	ok(exists $extensions{'rq'}, 'file_extensions');
};

{
	my $parser	= Attean->get_parser('SPARQL')->new();
	isa_ok($parser, 'AtteanX::Parser::SPARQL');
	my $type	= $parser->handled_type;
	can_ok($type, 'role');
	is($type->role, 'Attean::Algebra');
}

{
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $a		= $parser->parse("SELECT * { ?s <p> '''hello!''' OPTIONAL { ?s <q> ?x } FILTER(!BOUND(?x)) } LIMIT 5 OFFSET 5");
	does_ok($a, 'Attean::API::Algebra');
	isa_ok($a, 'Attean::Algebra::Ask');
}

{
	my $data	= "ASK { ?s ?p ?o FILTER(?o > -2.0 && ?o < +3e0 ) }";
	open(my $fh, '<', \$data);
	my $parser	= Attean->get_parser('SPARQL')->new();
	my $iter	= $parser->parse_iter_from_io($fh);
	does_ok($iter, 'Attean::API::Iterator');
	my $a		= $iter->next;
	does_ok($a, 'Attean::API::Algebra');
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
	my ($a)	= $parser->parse_list_from_bytes($content);
	is_deeply([sort $map->list_prefixes], [qw(ex foaf)]);
	my $foaf	= $map->namespace_uri('foaf');
	isa_ok($foaf, 'URI::Namespace');
	is($foaf->as_string, 'http://xmlns.com/foaf/0.1/');
	isa_ok($a, 'Attean::Algebra::Project')
}

subtest 'escaping' => sub {
	my $turtle	= q[ASK { <s> ex:p "\\"", '\\'', '\\u706b\\U0000661F' \\u007d];
	open(my $fh, '<:encoding(UTF-8)', \$turtle);
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
