use Test::More;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::RDF;
use Encode;
use Type::Tiny::Role;

my $constraint	= 'Attean::API::Triple';

my $p	= iri('http://example.org/p');

my $ser	= Attean->get_serializer('CanonicalNTriples')->new();
does_ok($ser, 'Attean::API::Serializer');
does_ok($ser, 'Attean::API::TripleSerializer');
isa_ok($ser, 'AtteanX::Serializer::NTriples');

{
	my $t1	= triple(blank('x'), $p, Attean::Literal->integer(1));
	my $t2	= triple(blank('x'), $p, Attean::Literal->integer(2));
	my @triples	= ($t1, $t2);

	my $expected	= <<"END";
_:v001 <http://example.org/p> "1"^^<http://www.w3.org/2001/XMLSchema#integer> .
_:v001 <http://example.org/p> "2"^^<http://www.w3.org/2001/XMLSchema#integer> .
END
	my $i		= Attean::ListIterator->new(values => [@triples], item_type => $constraint);
	my $bytes	= $ser->serialize_list_to_bytes(@triples);
	my $data	= decode('UTF-8', $bytes, Encode::FB_CROAK);
	is($data, $expected, 'canonical serialize_iter_to_bytes 1');
}

{
	my $x	= blank('x');
	my $y	= blank();
	my $t1	= triple($x, $p, Attean::Literal->integer(2));
	my $t2	= triple($x, $p, $y);
	my $t3	= triple($y, $p, $x);
	my $t4	= triple($y, $p, Attean::Literal->integer(7));
	my @triples	= ($t1, $t2, $t3, $t4);

	my $expected	= <<"END";
_:v001 <http://example.org/p> "2"^^<http://www.w3.org/2001/XMLSchema#integer> .
_:v001 <http://example.org/p> _:v002 .
_:v002 <http://example.org/p> "7"^^<http://www.w3.org/2001/XMLSchema#integer> .
_:v002 <http://example.org/p> _:v001 .
END
	my $i		= Attean::ListIterator->new(values => [@triples], item_type => $constraint);
	my $bytes	= $ser->serialize_list_to_bytes(@triples);
	my $data	= decode('UTF-8', $bytes, Encode::FB_CROAK);
	is($data, $expected, 'canonical serialize_iter_to_bytes 2');
}

done_testing();


sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
