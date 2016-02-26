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
	my $ser	= Attean->get_serializer('SPARQLHTML')->new();
	does_ok($ser, 'Attean::API::Serializer');
	does_ok($ser, 'Attean::API::ResultSerializer');
	isa_ok($ser, 'AtteanX::Serializer::SPARQLHTML');

	my $expected	= <<'END';
?subject	?predicate	?object
_:x	<http://example.org/p>	"1"^^<http://www.w3.org/2001/XMLSchema#integer>
_:x	<http://example.org/p>	"2"@en-US
<http://perlrdf.org/>		
END
	
	subtest 'serialize_iter_to_bytes' => sub {
		my $i	= Attean::ListIterator->new(values => [@triples], item_type => $constraint, variables => [qw(subject predicate object)]);
		my $b	= $ser->serialize_iter_to_bytes($i);

		my @rows	= ($b =~ /(<tr)/g);
		cmp_ok(scalar(@rows), '>=', 4, 'at least 1 header row and 3 data rows');
		like($b, qr[<td>x</td>]);
		like($b, qr[<td><a href="http://example.org/p">http://example.org/p</a></td>]);
		like($b, qr[<td>1</td>]);
		like($b, qr[<td>x</td>]);
		like($b, qr[<td>2</td>]);
		like($b, qr[<td><a href="http://perlrdf.org/">http://perlrdf.org/</a></td>]);
	};

	subtest 'serialize_iter_to_io' => sub {
		my $i	= Attean::ListIterator->new(values => [@triples], item_type => $constraint, variables => [qw(subject predicate object)]);
		my $b	= '';
		open(my $fh, '>', \$b);
		$ser->serialize_iter_to_io($fh, $i);
		close($fh);
	
		my @rows	= ($b =~ /(<tr)/g);
		cmp_ok(scalar(@rows), '>=', 4, 'at least 1 header row and 3 data rows');
		like($b, qr[<td>x</td>]);
		like($b, qr[<td><a href="http://example.org/p">http://example.org/p</a></td>]);
		like($b, qr[<td>1</td>]);
		like($b, qr[<td>x</td>]);
		like($b, qr[<td>2</td>]);
		like($b, qr[<td><a href="http://perlrdf.org/">http://perlrdf.org/</a></td>]);
	};
}

{
	my $sclass = Attean->get_serializer('SPARQLHTML');
	my $map = URI::NamespaceMap->new( { 
										foaf => 'http://xmlns.com/foaf/0.1/'
									  });

	my $n1 = Attean::IRI->new('http://xmlns.com/foaf/0.1/Person');
	my $n2 = Attean::IRI->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');

	subtest 'serialize IRI with namespace map' => sub {
		my $s = $sclass->new(namespaces => $map);
		is ($s->node_as_html($n1), '<a href="http://xmlns.com/foaf/0.1/Person">foaf:Person</a>', 'Return HTML link for IRI with abbrev');
		is ($s->node_as_html($n2), '<a href="http://www.w3.org/1999/02/22-rdf-syntax-ns#type">http://www.w3.org/1999/02/22-rdf-syntax-ns#type</a>', 'Return HTML link for IRI - 1');
	};
	
	subtest 'serialize IRI without namespace map' => sub {
		my $s = $sclass->new();
		is ($s->node_as_html($n1), '<a href="http://xmlns.com/foaf/0.1/Person">http://xmlns.com/foaf/0.1/Person</a>', 'Return HTML link for IRI - 2');
		is ($s->node_as_html($n2), '<a href="http://www.w3.org/1999/02/22-rdf-syntax-ns#type">http://www.w3.org/1999/02/22-rdf-syntax-ns#type</a>', 'Return HTML link for IRI - 3');
	};
}

done_testing();
