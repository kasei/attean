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
use AtteanX::Parser::Turtle;
use AtteanX::Parser::Turtle::Constants;
use Type::Tiny::Role;

subtest 'parser construction and metadata' => sub {
	my $parser	= Attean->get_parser('Turtle')->new();
	isa_ok($parser, 'AtteanX::Parser::Turtle');
	is($parser->canonical_media_type, 'text/turtle', 'canonical_media_type');
	my %extensions	= map { $_ => 1 } @{ $parser->file_extensions };
	ok(exists $extensions{'ttl'}, 'file_extensions');
	my $type	= $parser->handled_type;
	can_ok($type, 'role');
	is($type->role, 'Attean::API::Triple');
};

{
	my $turtle	= "<s> <p> 1, 2 .\n";
	open(my $fh, '<', \$turtle);
	my $parser	= Attean->get_parser('Turtle')->new();
	my $iter	= $parser->parse_iter_from_io($fh);
	does_ok($iter, 'Attean::API::Iterator');
	is($iter->next->object->value, '1');
	is($iter->next->object->value, '2');
	is($iter->next, undef);
}

{
	my $map		= URI::NamespaceMap->new();
	my $parser	= Attean->get_parser('Turtle')->new( namespaces => $map );
	my $content	= <<'END';
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
_:x a foaf:Person .
END
	$parser->parse_cb_from_bytes($content, sub {});
	is_deeply([sort $map->list_prefixes], [qw(ex foaf)]);
	my $foaf	= $map->namespace_uri('foaf');
	isa_ok($foaf, 'URI::Namespace');
	is($foaf->as_string, 'http://xmlns.com/foaf/0.1/');
}

subtest 'escaping' => sub {
	my $turtle	= q[<s> ex:p "\\"", '\\'', '\\u706b\\U0000661F' .];
	open(my $fh, '<:encoding(UTF-8)', \$turtle);
	my $l	= AtteanX::Parser::Turtle::Lexer->new($fh);
	
	expect($l->get_token, IRI, ['s'], 'subject');
	expect($l->get_token, PREFIXNAME, ['ex:', 'p'], 'predicate');
	expect($l->get_token, STRING1D, ['"'], 'double quote');
	expect($l->get_token, COMMA, [',']);
	expect($l->get_token, STRING1S, ["'"], 'single quote');
	expect($l->get_token, COMMA, [',']);
	expect($l->get_token, STRING1S, ["火星"], 'unicode \\u and \\U escapes');
};

subtest 'parse_term_from_bytes' => sub {
	my $parser	= Attean->get_parser('Turtle')->new();
	my $turtle	= '"""hello"""@en';
	my $term	= $parser->parse_term_from_bytes($turtle);
	does_ok($term, 'Attean::API::Literal');
	is($term->value, 'hello');
	is($term->language, 'en');
};

subtest 'turtle numeric u escaping' => sub {
	open(my $fh, '<', \q['\\u006F']);
	my $l	= AtteanX::Parser::Turtle::Lexer->new($fh);
	my $t	= $l->get_token;
	is($t->value, 'o');
};

subtest 'turtle numeric U escaping' => sub {
	open(my $fh, '<', \q['\\U0000006F']);
	my $l	= AtteanX::Parser::Turtle::Lexer->new($fh);
	my $t	= $l->get_token;
	is($t->value, 'o');
};

subtest 'pre-defined base IRI' => sub {
	my $base	= iri('http://example.org/base/');
	my $parser	= Attean->get_parser('Turtle')->new( base => $base );

	my $resolved_term	= $parser->parse_term_from_bytes('<test>');
	is($resolved_term->value, 'http://example.org/base/test');

	my $absolute_term	= $parser->parse_term_from_bytes('<tag:test>');
	is($absolute_term->value, 'tag:test');
	
	my $iter	= $parser->parse_iter_from_bytes('<subj> </pred> <obj/value> .');
	does_ok($iter, 'Attean::API::Iterator');
	my $t = $iter->next;
	is($t->subject->value, 'http://example.org/base/subj');
	is($t->predicate->value, 'http://example.org/pred');
	is($t->object->value, 'http://example.org/base/obj/value');
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
