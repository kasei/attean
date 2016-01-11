use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::RDF;
use Type::Tiny::Role;

my $constraint	= 'Attean::API::Triple';

my $s	= blank('x');
my $p	= iri('http://example.org/p');
my $o1	= Attean::Literal->integer(1);
my $o2	= Attean::Literal->integer(2);

my $t1	= triple($s, $p, $o1);
my $t2	= triple($s, $p, $o2);
my @triples	= ($t1, $t2);

{
	my $ser	= Attean->get_serializer('Turtle')->new();
	does_ok($ser, 'Attean::API::Serializer');
	does_ok($ser, 'Attean::API::TripleSerializer');
	isa_ok($ser, 'AtteanX::Serializer::Turtle');

	my $expected	= <<"END";
_:x <http://example.org/p> "1"^^<http://www.w3.org/2001/XMLSchema#integer> .
_:x <http://example.org/p> "2"^^<http://www.w3.org/2001/XMLSchema#integer> .
END
	
	{
		my $i	= Attean::ListIterator->new(values => [@triples], item_type => $constraint);
		my $data1	= $ser->serialize_iter_to_bytes($i);
		my $data2	= $ser->serialize_list_to_bytes(@triples);
	
		is($data1, $expected, 'serialize_iter_to_bytes');
		is($data1, $data2, 'serialize_list_to_bytes');
	}

	{
		my $i	= Attean::ListIterator->new(values => [@triples], item_type => $constraint);
		my $data	= '';
		open(my $fh, '>', \$data);
		$ser->serialize_iter_to_io($fh, $i);
		close($fh);
	
		is($data, $expected, 'serialize_iter_to_io');
	}

	{
		my $i	= Attean::ListIterator->new(values => [@triples], item_type => $constraint);
		my $data	= '';
		open(my $fh, '>', \$data);
		$ser->serialize_list_to_io($fh, @triples);
		close($fh);
	
		is($data, $expected, 'serialize_iter_to_io');
	}
}

{
	my $map		= URI::NamespaceMap->new( { foaf => iri('http://xmlns.com/foaf/0.1/') } );
	my $ser = Attean->get_serializer('Turtle')->new( namespaces => $map );

	my $iter	= Attean::ListIterator->new(values => [$t1, $t2], item_type => 'Attean::API::Triple');
	my $turtle	= $ser->serialize_iter_to_bytes($iter);
	like($turtle, qr{[@]prefix foaf: <http://xmlns.com/foaf/0.1/> .}, 'prefix definition');
}

done_testing();
