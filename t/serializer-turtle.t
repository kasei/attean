use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::RDF;
use Type::Tiny::Role;
use AtteanX::Parser::Turtle::Constants;

subtest 'serializer construction and metadata' => sub {
	{
		my $ser	= Attean->get_serializer('Turtle')->new();
		does_ok($ser, 'Attean::API::Serializer');
		isa_ok($ser, 'AtteanX::Serializer::Turtle');
		is($ser->canonical_media_type, 'text/turtle', 'canonical_media_type');
		my %types	= map { $_ => 1 } @{ $ser->media_types };
		ok(exists $types{'text/turtle'}, 'media_types');
		my $type	= $ser->handled_type;
		can_ok($type, 'role');
		is($type->role, 'Attean::API::Triple');
		my %extensions	= map { $_ => 1 } @{ $ser->file_extensions };
		ok(exists $extensions{'ttl'}, 'file_extensions');
	}
	{
		my $ser	= Attean->get_serializer('TurtleTokens')->new();
		does_ok($ser, 'Attean::API::Serializer');
		isa_ok($ser, 'AtteanX::Serializer::TurtleTokens');
		is($ser->canonical_media_type, 'text/turtle', 'canonical_media_type');
		my %types	= map { $_ => 1 } @{ $ser->media_types };
		ok(exists $types{'text/turtle'}, 'media_types');
		my $type	= $ser->handled_type;
		can_ok($type, 'role');
		is($type->role, 'AtteanX::Parser::Turtle::Token');
		my %extensions	= map { $_ => 1 } @{ $ser->file_extensions };
		ok(exists $extensions{'ttl'}, 'file_extensions');
	}
};


my $constraint	= 'Attean::API::Triple';

my $s	= blank('x');
my $t	= blank('y');
my $p	= iri('http://example.org/p');
my $q	= iri('http://example.org/q');
my $r	= iri('http://example.org/r');
my $o1	= Attean::Literal->integer(1);
my $o2	= Attean::Literal->integer(2);
my $o3	= Attean::Literal->new(value => '3');
my $o4	= Attean::Literal->new(value => '火星', language => 'ja');

my $t1	= triple($s, $p, $o1);
my $t2	= triple($s, $p, $o2);
my $t3	= triple($s, $q, $o3);
my $t4	= triple($t, $r, $o4);

subtest 'turtle with object-list' => sub {
	my $ser	= Attean->get_serializer('Turtle')->new();
	does_ok($ser, 'Attean::API::Serializer');
	does_ok($ser, 'Attean::API::TripleSerializer');
	isa_ok($ser, 'AtteanX::Serializer::Turtle');

	my $expected	= <<"END";
_:x <http://example.org/p> 1 , 2 .
END
	
	{
		my $i	= Attean::ListIterator->new(values => [$t1, $t2], item_type => $constraint);
		my $data1	= $ser->serialize_iter_to_bytes($i);
		my $data2	= $ser->serialize_list_to_bytes($t1, $t2);
	
		is($data1, $expected, 'serialize_iter_to_bytes');
		is($data1, $data2, 'serialize_list_to_bytes');
	}

	{
		my $i	= Attean::ListIterator->new(values => [$t1, $t2], item_type => $constraint);
		my $data	= '';
		open(my $fh, '>', \$data);
		$ser->serialize_iter_to_io($fh, $i);
		close($fh);
	
		is($data, $expected, 'serialize_iter_to_io');
	}

	{
		my $i	= Attean::ListIterator->new(values => [$t1, $t2], item_type => $constraint);
		my $data	= '';
		open(my $fh, '>', \$data);
		$ser->serialize_list_to_io($fh, $t1, $t2);
		close($fh);
	
		is($data, $expected, 'serialize_iter_to_io');
	}
};

subtest 'turtle with predicate-object list' => sub {
	my $ser	= Attean->get_serializer('Turtle')->new();
	my $expected	= <<'END';
_:x <http://example.org/p> 1 , 2 ;
    <http://example.org/q> "3" .
_:y <http://example.org/r> "火星"@ja .
END
	
	my $i	= Attean::ListIterator->new(values => [$t1, $t2, $t3, $t4], item_type => $constraint);
	my $data1	= $ser->serialize_iter_to_bytes($i);
	my $data2	= $ser->serialize_list_to_bytes($t1, $t2, $t3, $t4);

	is($data1, $expected, 'serialize_iter_to_bytes');
	is($data1, $data2, 'serialize_list_to_bytes');
};

subtest 'turtle with prefix namespace declaration' => sub {
	my $map		= URI::NamespaceMap->new( { foaf => iri('http://xmlns.com/foaf/0.1/') } );
	my $ser = Attean->get_serializer('Turtle')->new( namespaces => $map );
	my $expected	= <<'END';
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
_:x <http://example.org/p> 1 , 2 .
END

	my $iter	= Attean::ListIterator->new(values => [$t1, $t2], item_type => 'Attean::API::Triple');
	my $turtle	= $ser->serialize_iter_to_bytes($iter);
	is($turtle, $expected, 'serialize_iter_to_bytes');
};

subtest 'turtle with prefix namespace declaration and use' => sub {
	my $map		= URI::NamespaceMap->new( { ex => iri('http://example.org/') } );
	my $ser = Attean->get_serializer('Turtle')->new( namespaces => $map );
	my $expected	= <<'END';
@prefix ex: <http://example.org/> .
_:x ex:p 1 , 2 .
END

	my $iter	= Attean::ListIterator->new(values => [$t1, $t2], item_type => 'Attean::API::Triple');
	my $turtle	= $ser->serialize_iter_to_bytes($iter);
	is($turtle, $expected, 'serialize_iter_to_bytes');
};

subtest 'escaping' => sub {
	my @tokens;
	my $dq	= literal('"');
	my $sq	= literal("'");
	my $bq	= literal(q["']);
	
	@tokens	= $dq->sparql_tokens->elements;
	expect(shift(@tokens), STRING1D, ['"'], 'double quote');

	@tokens	= $sq->sparql_tokens->elements;
	expect(shift(@tokens), STRING1D, ["'"], 'single quote');
	
	@tokens	= $bq->sparql_tokens->elements;
	expect(shift(@tokens), STRING1D, [q["']], 'double and single quotes');
	
	my $ser = Attean->get_serializer('Turtle')->new();
	my @triples	= map { triple(iri('s'), iri('p'), $_) } ($dq, $sq, $bq);
	my $iter	= Attean::ListIterator->new(values => \@triples, item_type => 'Attean::API::Triple');
	my $turtle	= $ser->serialize_iter_to_bytes($iter);
	my $expected	= qq[<s> <p> "\\"" , "'" , "\\"'" .\n];
	is($turtle, $expected, 'serialize_iter_to_bytes');
};

subtest 'token serialization' => sub {
	my $ser	= Attean->get_serializer('TurtleTokens')->new();
	my @tokens;
	push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(A, -1, -1, -1, -1, ['a']));
	push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(COMMENT, -1, -1, -1, -1, ['comment']));
	push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(STRING1S, -1, -1, -1, -1, ['xyz']));
	push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(COMMA, -1, -1, -1, -1, [',']));
	push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(STRING3S, -1, -1, -1, -1, ['hello']));
	push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(COMMA, -1, -1, -1, -1, [',']));
	push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(STRING3D, -1, -1, -1, -1, ['world']));
	push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(HATHAT, -1, -1, -1, -1, ['^^']));
	push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(PREFIXNAME, -1, -1, -1, -1, ['xsd:', 'string']));
	my $iter	= Attean::ListIterator->new(values => \@tokens, item_type => 'AtteanX::Parser::Turtle::Token');
	my $data	= $ser->serialize_iter_to_bytes($iter);
	like($data, qr/\ba\b/);
	like($data, qr/# comment/);
	like($data, qr/'xyz'(?!')/);
	like($data, qr/'''hello'''/);
	like($data, qr/"""world"""\^\^xsd:string/);
};

subtest 'AbbreviatingSerializer with explicit namespace map' => sub {
	my $map		= URI::NamespaceMap->new( { foaf => iri('http://xmlns.com/foaf/0.1/') } );
	my $p		= Attean->get_parser('Turtle')->new();
	my $iter	= $p->parse_iter_from_bytes('@prefix foaf: <http://xmlns.com/foaf/0.1/> . <http://example.org/people/alice> a foaf:Person ; foaf:name "Alice" .');
	my $s		= Attean->get_serializer('Turtle')->new( namespaces => $map );
	my $bytes	= $s->serialize_iter_to_bytes($iter);
	like($bytes, qr[prefix foaf: <http://xmlns.com/foaf/0.1/> .], 'serialization has prefix declaration');
	like($bytes, qr<http://example.org/people/alice>, 'serialization has IRI');
	like($bytes, qr/foaf:Person/, 'serialization has prefix name foaf:Person');
	like($bytes, qr/foaf:name "Alice"/, 'serialization has prefix name foaf:name');
};

subtest 'End-to-end AbbreviatingSerializer' => sub {
	my $map		= URI::NamespaceMap->new();
	my $p		= Attean->get_parser('Turtle')->new( namespaces => $map );
	my $iter	= $p->parse_iter_from_bytes('@prefix foaf: <http://xmlns.com/foaf/0.1/> . @prefix ex: <http://example.org/> . <http://example.org/people/alice> a foaf:Person ; foaf:name "Alice" .');
	my $s		= Attean->get_serializer('Turtle')->new( namespaces => $map );
	my $bytes	= $s->serialize_iter_to_bytes($iter);
	like($bytes, qr[prefix ex: <http://example.org/> .], 'serialization has prefix declaration ex:');
	like($bytes, qr[prefix foaf: <http://xmlns.com/foaf/0.1/> .], 'serialization has prefix declaration');
	like($bytes, qr<http://example.org/people/alice>, 'serialization has IRI');
	like($bytes, qr/foaf:Person/, 'serialization has prefix name foaf:Person');
	like($bytes, qr/foaf:name "Alice"/, 'serialization has prefix name foaf:name');
	is_deeply([sort $map->list_prefixes], [qw(ex foaf)]);
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
