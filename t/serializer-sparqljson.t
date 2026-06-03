use Test::Modern;
use Test::Exception;

use utf8;
use v5.14;
use warnings;
no warnings 'redefine';

use JSON qw(decode_json);
use Attean;
use Attean::RDF;
use Type::Tiny::Role;

my $bindings_constraint = 'Attean::API::Result';
my $boolean_constraint = 'Attean::API::Term';
my @vars = qw(subject predicate object);

my $s   = Attean::Blank->new('x');
my $p   = Attean::IRI->new('http://example.org/p');
my $o1  = Attean::Literal->new(value => '1', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
my $o2  = Attean::Literal->new(value => '火', language => 'en-US');

my $t1  = Attean::Result->new(bindings => { subject => $s, predicate => $p, object => $o1 });
my $t2  = Attean::Result->new(bindings => { subject => $s, predicate => $p, object => $o2 });
my $t3  = Attean::Result->new(bindings => { subject => iri('http://perlrdf.org/') });
my @triples  = ($t1, $t2, $t3);

is(Attean->get_serializer('sparqljson'), 'AtteanX::Serializer::SPARQLJSON', 'get serializer by name');
is(Attean->get_serializer(media_type => 'application/sparql-results+json'), 'AtteanX::Serializer::SPARQLJSON', 'get serializer by media type');

my $ser = Attean->get_serializer('SPARQLJSON')->new();

subtest 'Serializer roles' => sub {
	does_ok($ser, 'Attean::API::Serializer');
	does_ok($ser, 'Attean::API::AppendableSerializer');
	does_ok($ser, 'Attean::API::ResultSerializer');
	isa_ok($ser, 'AtteanX::Serializer::SPARQLJSON');
};

subtest 'Serializer media types' => sub {
	my @media_types = @{ $ser->media_types };
	is($media_types[0], $ser->canonical_media_type(), 'media_types');
};

my $expected_bindings = {"head" => {"vars" => ["object","predicate","subject"]},"results" => {"bindings" => [{"object" => {"type" => "literal","value" => "1"},"predicate" => {"type" => "uri","value" => "http://example.org/p"},"subject" => {"type" => "bnode","value" => "x"}},{"object" => {"type" => "literal","value" => "火"},"predicate" => {"type" => "uri","value" => "http://example.org/p"},"subject" => {"type" => "bnode","value" => "x"}},{"subject" => {"type" => "uri","value" => "http://perlrdf.org/"}}]}};
subtest 'bindings serialize_iter_to_bytes' => sub {
	my $i		= Attean::ListIterator->new(values => [@triples], item_type => $bindings_constraint, variables => [@vars]);
	my $b		= $ser->serialize_iter_to_bytes($i);
	my $data	= decode_json($b);
	is_deeply($data, $expected_bindings, 'serialize_iter_to_bytes');
};

subtest 'bindings serialize_iter_to_io' => sub {
	my $i = Attean::ListIterator->new(values => [@triples], item_type => $bindings_constraint, variables => [@vars]);
	my $bytes = '';
	open(my $fh, '>:encoding(UTF-8)', \$bytes);
	$ser->serialize_iter_to_io($fh, $i);
	close($fh);
	
	my $data	= decode_json($bytes);
	is_deeply($data, $expected_bindings, 'serialize_iter_to_io');
};

my $expected_boolean = {"head" => {},"boolean" => $JSON::true};
subtest 'boolean serialize_iter_to_bytes' => sub {
	my $i		= Attean::ListIterator->new(values => [Attean::Literal->true], item_type => $boolean_constraint);
	my $b		= $ser->serialize_iter_to_bytes($i);
	my $data	= decode_json($b);
	is_deeply($data, $expected_boolean, 'serialize_iter_to_bytes');
};

subtest 'boolean serialize_iter_to_io' => sub {
	my $i = Attean::ListIterator->new(values => [Attean::Literal->true], item_type => $boolean_constraint);
	my $bytes = '';
	open(my $fh, '>:encoding(UTF-8)', \$bytes);
	$ser->serialize_iter_to_io($fh, $i);
	close($fh);
	
	my $data	= decode_json($bytes);
	is_deeply($data, $expected_boolean, 'serialize_iter_to_io');
};

done_testing();
