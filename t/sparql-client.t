use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Test::LWP::UserAgent;
use Attean parsers => ['Turtle'];
use Attean::RDF;
use Attean::SPARQLClient;

package TestSigner {
	use Moo;
	sub sign {
		my $self	= shift;
		my $req		= shift;
		$req->header('Signed' => 'true');
		return $req;
	}
};

subtest 'SPARQLClient' => sub {
	my $ua		= Test::LWP::UserAgent->new();
	$ua->map_response(qr{example.org/sparql}, HTTP::Response->new('200', 'OK', ['Content-Type' => 'application/sparql-results+xml'], <<'XML'));
<?xml version="1.0"?>
<sparql xmlns="http://www.w3.org/2005/sparql-results#">
<head>
<variable name="s"/>
<variable name="p"/>
<variable name="o"/>
</head>
<results>
<result>
	<binding name="s"><uri>http://example.org/s4</uri></binding>
	<binding name="p"><uri>http://example.org/p</uri></binding>
	<binding name="o"><literal datatype="http://www.w3.org/2001/XMLSchema#integer">4</literal></binding>
</result>
<result>
	<binding name="s"><uri>http://example.org/s3</uri></binding>
	<binding name="p"><uri>http://example.org/p</uri></binding>
	<binding name="o"><literal datatype="http://www.w3.org/2001/XMLSchema#integer">3</literal></binding>
</result>
</results>
</sparql>
XML
	my $client	= Attean::SPARQLClient->new( endpoint => 'http://example.org/sparql', user_agent => $ua );
	my $sparql	= 'SELECT * WHERE { ?s ?p ?o }';
	my $iter	= $client->query($sparql);
	my @results	= $iter->elements;
	is(scalar(@results), 2, 'expected result count');
	my @objects	= sort { $a <=> $b } map { $_->value('o')->value } @results;
	is_deeply(\@objects, [3,4], 'expected values');
};

subtest 'SPARQLClient URL with extra query parameters' => sub {
	my $client	= Attean::SPARQLClient->new( endpoint => 'http://example.org/sparql?foo=bar' );
	my $sparql	= 'SELECT * WHERE { ?s ?p ?o }';
	my $req		= $client->query_request($sparql);
	my $uri		= URI->new($req->uri);
	my %params = $uri->query_form();
	is($params{'foo'}, 'bar');
	like($params{'query'}, qr/^SELECT/);
};

subtest 'SPARQLClient with request signer' => sub {
	my $signer	= TestSigner->new();
	my $client	= Attean::SPARQLClient->new( endpoint => 'http://example.org/sparql?foo=bar', request_signer => $signer );
	my $sparql	= 'SELECT * WHERE { ?s ?p ?o }';
	my $req		= $client->query_request($sparql);
	is($req->header('Signed'), 'true');
};

done_testing();
