use Test::Modern;
use Test::Exception;

use utf8;
use v5.14;
use warnings;
use Encode qw(decode);
no warnings 'redefine';

use Attean;
use Attean::RDF;
use Type::Tiny::Role;

subtest 'serializer construction and metadata' => sub {
	my $ser	= Attean->get_serializer('RDFXML')->new();
	does_ok($ser, 'Attean::API::Serializer');
	isa_ok($ser, 'AtteanX::Serializer::RDFXML');
	is($ser->canonical_media_type, 'application/rdf+xml', 'canonical_media_type');
	my %types	= map { $_ => 1 } @{ $ser->media_types };
	ok(exists $types{'application/rdf+xml'}, 'media_types');
	my $type	= $ser->handled_type;
	can_ok($type, 'role');
	is($type->role, 'Attean::API::Triple');
	my %extensions	= map { $_ => 1 } @{ $ser->file_extensions };
	ok(exists $extensions{'rdf'}, 'file_extensions');
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

subtest 'RDF/XML with object-list' => sub {
	my $ser	= Attean->get_serializer('RDFXML')->new();
	does_ok($ser, 'Attean::API::Serializer');
	does_ok($ser, 'Attean::API::TripleSerializer');
	isa_ok($ser, 'AtteanX::Serializer::RDFXML');

	my $expected	= <<'END';
<?xml version="1.0" encoding="utf-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<rdf:Description xmlns:ns1="http://example.org/" rdf:nodeID="bx">
	<ns1:p rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">1</ns1:p>
	<ns1:p rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">2</ns1:p>
</rdf:Description>
</rdf:RDF>
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

subtest 'RDF/XML with predicate-object list' => sub {
	my $ser	= Attean->get_serializer('RDFXML')->new();
	my $expected	= <<'END';
<?xml version="1.0" encoding="utf-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#">
<rdf:Description xmlns:ns1="http://example.org/" rdf:nodeID="bx">
	<ns1:p rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">1</ns1:p>
	<ns1:p rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">2</ns1:p>
	<ns1:q>3</ns1:q>
</rdf:Description>
<rdf:Description xmlns:ns1="http://example.org/" rdf:nodeID="by">
	<ns1:r xml:lang="ja">火星</ns1:r>
</rdf:Description>
</rdf:RDF>
END
	
	my $i	= Attean::ListIterator->new(values => [$t1, $t2, $t3, $t4], item_type => $constraint);
	my $data1	= $ser->serialize_iter_to_bytes($i);
	my $data2	= $ser->serialize_list_to_bytes($t1, $t2, $t3, $t4);
	
	my $string1	= decode('UTF-8', $data1, Encode::FB_CROAK);
	my $string2	= decode('UTF-8', $data2, Encode::FB_CROAK);
	is($string1, $expected, 'serialize_iter_to_bytes');
	is($string1, $string2, 'serialize_list_to_bytes');
};

subtest 'RDF/XML with prefix namespace declaration' => sub {
	my $map		= URI::NamespaceMap->new( { foaf => iri('http://xmlns.com/foaf/0.1/') } );
	my $ser = Attean->get_serializer('RDFXML')->new( namespaces => $map );
	my $expected	= <<'END';
<?xml version="1.0" encoding="utf-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:foaf="http://xmlns.com/foaf/0.1/">
<rdf:Description xmlns:ns1="http://example.org/" rdf:nodeID="bx">
	<ns1:p rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">1</ns1:p>
	<ns1:p rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">2</ns1:p>
</rdf:Description>
</rdf:RDF>
END

	my $iter	= Attean::ListIterator->new(values => [$t1, $t2], item_type => 'Attean::API::Triple');
	my $rdfxml	= $ser->serialize_iter_to_bytes($iter);
	is($rdfxml, $expected, 'serialize_iter_to_bytes');
};

subtest 'RDF/XML with prefix namespace declaration and use' => sub {
	my $map		= URI::NamespaceMap->new( { ex => iri('http://example.org/') } );
	my $ser 	= Attean->get_serializer('RDFXML')->new( namespaces => $map );
	my $expected	= <<'END';
<?xml version="1.0" encoding="utf-8"?>
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:ex="http://example.org/">
<rdf:Description rdf:nodeID="bx">
	<ex:p rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">1</ex:p>
	<ex:p rdf:datatype="http://www.w3.org/2001/XMLSchema#integer">2</ex:p>
</rdf:Description>
</rdf:RDF>
END

	my $iter	= Attean::ListIterator->new(values => [$t1, $t2], item_type => 'Attean::API::Triple');
	my $rdfxml	= $ser->serialize_iter_to_bytes($iter);
	is($rdfxml, $expected, 'serialize_iter_to_bytes');
};

subtest 'AbbreviatingSerializer with explicit namespace map' => sub {
	my $map		= URI::NamespaceMap->new( { foaf => iri('http://xmlns.com/foaf/0.1/') } );
	my $p		= Attean->get_parser('Turtle')->new();
	my $iter	= $p->parse_iter_from_bytes('@prefix foaf: <http://xmlns.com/foaf/0.1/> . <http://example.org/people/alice> a foaf:Person ; foaf:name "Alice" .');
	my $s		= Attean->get_serializer('RDFXML')->new( namespaces => $map );
	my $bytes	= $s->serialize_iter_to_bytes($iter);
	like($bytes, qr[xmlns:foaf="http://xmlns.com/foaf/0.1/"], 'serialization has prefix declaration');
	like($bytes, qr<http://example.org/people/alice>, 'serialization has IRI');
	like($bytes, qr/<foaf:name/, 'serialization has prefix name foaf:name');
};

subtest 'End-to-end AbbreviatingSerializer' => sub {
	my $map		= URI::NamespaceMap->new();
	my $p		= Attean->get_parser('Turtle')->new( namespaces => $map );
	my $iter	= $p->parse_iter_from_bytes('@prefix foaf: <http://xmlns.com/foaf/0.1/> . @prefix ex: <http://example.org/> . <http://example.org/people/alice> a foaf:Person ; foaf:name "Alice" .');
	my $s		= Attean->get_serializer('RDFXML')->new( namespaces => $map );
	my $bytes	= $s->serialize_iter_to_bytes($iter);
	like($bytes, qr[xmlns:ex="http://example.org/"], 'serialization has prefix declaration');
	like($bytes, qr[xmlns:foaf="http://xmlns.com/foaf/0.1/"], 'serialization has prefix declaration');
	like($bytes, qr<http://example.org/people/alice>, 'serialization has IRI');
	like($bytes, qr/<foaf:name/, 'serialization has prefix name foaf:name');
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
