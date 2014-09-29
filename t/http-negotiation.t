use v5.14;
use autodie;
use utf8;
use Test::More;
use Test::Exception;

use Attean;

{
	my %negotiate_expect	= (
		"text/plain"	=> ['NTriples', 'text/plain'],
# 		"application/rdf+xml"	=> ['RDFXML', 'application/rdf+xml'],
# 		"image/jpeg;q=1,application/rdf+xml;q=0.5"	=> ['RDFXML', 'application/rdf+xml'],
# 		"application/rdf+xml;q=1,text/plain"	=> ['RDFXML', 'application/rdf+xml'],
		"application/rdf+xml;q=0,text/plain;q=1"	=> ['NTriples', 'text/plain'],
# 		"application/rdf+xml;q=0.5,text/turtle;q=0.7,text/xml"	=> ['Turtle', 'text/turtle'],
# 		"application/x-turtle;q=1,text/turtle;q=0.7"	=> ['Turtle', 'application/x-turtle'],
	);
	
	while (my ($accept,$data) = each(%negotiate_expect)) {
		my ($sname, $etype)	= @$data;
		my $h	= new HTTP::Headers;
		$h->header(Accept => $accept);
		my ($type, $s)	= Attean->negotiate_serializer( request_headers => $h );
		is( $type, $etype, "expected media type for $sname serialization is $etype" );
		unless (is( $s, "AtteanX::Serializer::$sname", "HTTP negotiated $sname serializer" )) {
			warn "# $accept";
		}
	}
}

{
	my $h = new HTTP::Headers;
	$h->header(Accept=>"application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,*/*;0.5");
	my ($type, $s)	= Attean->negotiate_serializer( request_headers => $h );
	ok ( $type, "choose some serializer for Accept: */*: $type" );
}

{
	my $h = new HTTP::Headers;
	$h->header(Accept=>"application/rdf+xml;q=1,text/plain;q=0.7");
	my ($type, $s)	= Attean->negotiate_serializer( request_headers => $h, restrict => [ 'ntriples' ] );
	is ( $type, 'text/plain', 'choose less wanted serializer with restrict option' );
}

{
	my $h = new HTTP::Headers;
	$h->header(Accept=>"application/xhtml+xml;q=0.8,text/plain;q=0.9,text/turtle;q=0.7");
	my ($type, $s)	= Attean->negotiate_serializer(
		request_headers => $h,
		restrict => [ 'ntriples' ],
		extend => {
			'text/html'	=> 'html',
			'application/xhtml+xml' => 'xhtml',
		},
	);
	is( $type, 'application/xhtml+xml', "negotiation with both 'restrict' restriction and 'extend' custom type" );
	is( $s, 'xhtml', 'negotiation custom type thunk' );
}

{
	my $h = new HTTP::Headers;
	$h->header(Accept=>"application/rdf+xml;q=0.9,text/turtle;q=0.7");
	my ($type, $s)	= Attean->negotiate_serializer(
		request_headers => $h,
		extend => {
			'application/rdf+xml'	=> 'rdfxml',
		},
	);
	is($type, 'application/rdf+xml', 'extended negotiation with media type collision');
	is($s, 'rdfxml', 'extended negotiation with media type collision');
}


my %negotiate_fail	= (
	"image/jpeg" =>	undef,
	"application/rdf+xml" => ['turtle','rdfjson']
);

while (my ($accept,$restrict) = each(%negotiate_fail)) {
	dies_ok {
		my $h = new HTTP::Headers;
		$h->header(Accept => $accept);
		my ($type, $s)	= Attean->negotiate_serializer( request_headers => $h, restrict => $restrict );
	} "HTTP negotiated serialization throws on unknown/unwanted media type $accept";
}

{
	my ($sname, $etype)	= ();
	my $h	= new HTTP::Headers;
	$h->header(Accept => "");
	my ($type, $s)	= Attean->negotiate_serializer( request_headers => $h );
	use Data::Dumper;
	like( $type, qr'^((application/n-triples)|(text/plain))$', "expected media type with empty accept header" ) or die Dumper($type, $s);
	is( $s, "AtteanX::Serializer::NTriples", "HTTP negotiated empty accept header to proper serializer" );
}

done_testing();
