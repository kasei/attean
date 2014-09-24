use Test::More;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Type::Tiny::Role;

my $constraint	= 'Attean::API::Triple';

my $s	= Attean::Blank->new('x');
my $p	= Attean::IRI->new('http://example.org/p');
my $o1	= Attean::Literal->new(value => '1', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
my $o2	= Attean::Literal->new(value => '2', datatype => 'http://www.w3.org/2001/XMLSchema#integer');

my $t1	= Attean::Triple->new($s, $p, $o1);
my $t2	= Attean::Triple->new($s, $p, $o2);
my @triples	= ($t1, $t2);

{
	my $ser	= Attean->get_serializer('NTriples')->new();
	does_ok($ser, 'Attean::API::Serializer');
	does_ok($ser, 'Attean::API::TripleSerializer');
	isa_ok($ser, 'AtteanX::Serializer::NTriples');

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

done_testing();


sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
