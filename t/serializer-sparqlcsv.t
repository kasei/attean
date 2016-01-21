use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::RDF;
use Type::Tiny::Role;

my $constraint	= 'Attean::API::Result';

my $s	= Attean::Blank->new('x');
my $p	= Attean::IRI->new('http://example.org/p');
my $o1	= Attean::Literal->new(value => '1', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
my $o2	= Attean::Literal->new(value => '2', language => 'en-US');

my $t1	= Attean::Result->new(bindings => { subject => $s, predicate => $p, object => $o1 });
my $t2	= Attean::Result->new(bindings => { subject => $s, predicate => $p, object => $o2 });
my $t3	= Attean::Result->new(bindings => { subject => iri('http://perlrdf.org/') });
my @triples	= ($t1, $t2, $t3);

{
	my $ser	= Attean->get_serializer('SPARQLCSV')->new();
	does_ok($ser, 'Attean::API::Serializer');
	does_ok($ser, 'Attean::API::ResultSerializer');
	isa_ok($ser, 'AtteanX::Serializer::SPARQLCSV');

	my $expected	= <<'END';
subject,predicate,object
_:x,http://example.org/p,1
_:x,http://example.org/p,2
http://perlrdf.org/,,
END
	
	{
		my $i	= Attean::ListIterator->new(values => [@triples], item_type => $constraint, variables => [qw(subject predicate object)]);
		my $b	= $ser->serialize_iter_to_bytes($i);
	
		is($b, $expected, 'serialize_iter_to_bytes');
	}

	{
		my $i	= Attean::ListIterator->new(values => [@triples], item_type => $constraint, variables => [qw(subject predicate object)]);
		my $data	= '';
		open(my $fh, '>', \$data);
		$ser->serialize_iter_to_io($fh, $i);
		close($fh);
	
		is($data, $expected, 'serialize_iter_to_io');
	}

	{
		my $expected_reorder	= <<'END';
predicate,subject,object
http://example.org/p,_:x,1
http://example.org/p,_:x,2
,http://perlrdf.org/,
END
	
		my $i	= Attean::ListIterator->new(values => [@triples], item_type => $constraint, variables => [qw(predicate subject object)]);
		my $data	= '';
		open(my $fh, '>', \$data);
		$ser->serialize_iter_to_io($fh, $i);
		close($fh);
	
		is($data, $expected_reorder, 'variable order sensitivity');
	}
}

done_testing();
