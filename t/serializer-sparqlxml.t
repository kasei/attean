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
	my $ser	= Attean->get_serializer('SPARQLXML')->new();
	does_ok($ser, 'Attean::API::Serializer');
	does_ok($ser, 'Attean::API::ResultSerializer');
	isa_ok($ser, 'AtteanX::Serializer::SPARQLXML');

	my $expected	= <<'END';
<?xml version="1.0" encoding="utf-8"?>
<sparql xmlns="http://www.w3.org/2005/sparql-results#">
<head>
	<variable name="subject"/>
	<variable name="predicate"/>
	<variable name="object"/>
</head>
<results>
		<result>
			<binding name="subject"><bnode>x</bnode></binding>
			<binding name="predicate"><uri>http://example.org/p</uri></binding>
			<binding name="object"><literal datatype="http://www.w3.org/2001/XMLSchema#integer">1</literal></binding>
		</result>
		<result>
			<binding name="subject"><bnode>x</bnode></binding>
			<binding name="predicate"><uri>http://example.org/p</uri></binding>
			<binding name="object"><literal xml:lang="en-US">2</literal></binding>
		</result>
		<result>
			<binding name="subject"><uri>http://perlrdf.org/</uri></binding>
		</result>
</results>
</sparql>
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
<?xml version="1.0" encoding="utf-8"?>
<sparql xmlns="http://www.w3.org/2005/sparql-results#">
<head>
	<variable name="predicate"/>
	<variable name="subject"/>
	<variable name="object"/>
</head>
<results>
		<result>
			<binding name="predicate"><uri>http://example.org/p</uri></binding>
			<binding name="subject"><bnode>x</bnode></binding>
			<binding name="object"><literal datatype="http://www.w3.org/2001/XMLSchema#integer">1</literal></binding>
		</result>
		<result>
			<binding name="predicate"><uri>http://example.org/p</uri></binding>
			<binding name="subject"><bnode>x</bnode></binding>
			<binding name="object"><literal xml:lang="en-US">2</literal></binding>
		</result>
		<result>
			<binding name="subject"><uri>http://perlrdf.org/</uri></binding>
		</result>
</results>
</sparql>
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
