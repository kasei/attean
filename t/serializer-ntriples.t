use Test::More;
use Test::Exception;
use Test::Moose;

use v5.14;
use warnings;
no warnings 'redefine';

use RDF;

my $constraint	= Moose::Meta::TypeConstraint::Role->new(role => 'RDF::API::Triple');

my $s	= RDF::Blank->new('x');
my $p	= RDF::IRI->new('http://example.org/p');
my $o1	= RDF::Literal->new(value => '1', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
my $o2	= RDF::Literal->new(value => '2', datatype => 'http://www.w3.org/2001/XMLSchema#integer');

my $t1	= RDF::Triple->new($s, $p, $o1);
my $t2	= RDF::Triple->new($s, $p, $o2);
my @triples	= ($t1, $t2);

my $ser	= RDF->get_serializer('NTriples')->new();
does_ok($ser, 'RDF::API::Serializer');
does_ok($ser, 'RDF::API::TripleSerializer');
isa_ok($ser, 'RDF::X::Serializer::NTriples');

my $expected	= <<"END";
_:x <http://example.org/p> "1"^^<http://www.w3.org/2001/XMLSchema#integer> .
_:x <http://example.org/p> "2"^^<http://www.w3.org/2001/XMLSchema#integer> .
END
	
{
	my $i	= RDF::ListIterator->new(values => [@triples], item_type => $constraint);
	my $data1	= $ser->serialize_iter_to_bytes($i);
	my $data2	= $ser->serialize_list_to_bytes(@triples);
	
	is($data1, $expected, 'serialize_iter_to_bytes');
	is($data1, $data2, 'serialize_list_to_bytes');
}

{
	my $i	= RDF::ListIterator->new(values => [@triples], item_type => $constraint);
	my $data	= '';
	open(my $fh, '>', \$data);
	$ser->serialize_iter_to_io($fh, $i);
	close($fh);
	
	is($data, $expected, 'serialize_iter_to_io');
}

{
	my $i	= RDF::ListIterator->new(values => [@triples], item_type => $constraint);
	my $data	= '';
	open(my $fh, '>', \$data);
	$ser->serialize_list_to_io($fh, @triples);
	close($fh);
	
	is($data, $expected, 'serialize_iter_to_io');
}

done_testing();
