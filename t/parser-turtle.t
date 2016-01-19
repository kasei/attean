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

{
	my $parser	= Attean->get_parser('Turtle')->new();
	isa_ok($parser, 'AtteanX::Parser::Turtle');
	my $type	= $parser->handled_type;
	can_ok($type, 'role');
	is($type->role, 'Attean::API::Triple');
}

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
