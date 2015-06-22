use v5.14;
use autodie;
use utf8;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use File::Glob qw(bsd_glob);
use File::Spec;

use Attean;

my $p	= Attean->get_parser('Turtle');
is($p, 'AtteanX::Parser::Turtle');

{
	note('all acceptable parsers');
	my $accept	= Attean->acceptable_parsers();
	ok(length($accept), 'got accept header value');
	
	# check to make sure some of the default parsers are present:
	like($accept, qr'text/turtle');
	like($accept, qr'application/rdf[+]xml');
	like($accept, qr'text/tab-separated-values');
}

{
	note(' acceptable PULL parsers');
	my $accept	= Attean->acceptable_parsers(prefer => q[pull]);
	
	# check to make sure some of the default parsers are present:
	like($accept, qr'application/n-quads');
	like($accept, qr'application/n-triples');
	like($accept, qr'text/tab-separated-values');
	
	unlike($accept, qr'application/rdf[+]xml');
}

{
	note(' acceptable PUSH parsers');
	my $accept	= Attean->acceptable_parsers(prefer => q[Attean::API::PushParser]);
	
	# check to make sure some of the default parsers are present:
	like($accept, qr'text/turtle');
	like($accept, qr'application/rdf[+]xml');
	like($accept, qr'application/sparql-results[+]xml');

	unlike($accept, qr'application/n-quads');
	unlike($accept, qr'application/n-triples');
	unlike($accept, qr'text/tab-separated-values');
}

{
	note(' acceptable ATONCE parsers');
	my $accept	= Attean->acceptable_parsers(prefer => q[AtOnce]);
	
	like($accept, qr'application/sparql-results[+]json');

	unlike($accept, qr'text/turtle');
	unlike($accept, qr'application/n-quads');
	unlike($accept, qr'application/n-triples');
}

{
	note(' acceptable SPARQL RESULT parsers');
	my $accept	= Attean->acceptable_parsers(handles => q[result]);
	
	like($accept, qr'application/sparql-results[+]json');
	like($accept, qr'application/sparql-results[+]xml');
	like($accept, qr'text/tab-separated-values');

	unlike($accept, qr'text/turtle');
	unlike($accept, qr'application/rdf[+]xml');
	unlike($accept, qr'application/n-quads');
	unlike($accept, qr'application/n-triples');
}

{
	note(' acceptable TRIPLE parsers');
	my $accept	= Attean->acceptable_parsers(handles => q[Attean::API::Triple]);
	
	like($accept, qr'application/n-quads');
	like($accept, qr'application/n-triples');
	like($accept, qr'application/octet-stream');
	like($accept, qr'application/x-turtle');
	like($accept, qr'application/turtle');
	like($accept, qr'text/turtle');
	like($accept, qr'application/rdf[+]xml');

	unlike($accept, qr'application/sparql-results[+]json');
	unlike($accept, qr'application/sparql-results[+]xml');
	unlike($accept, qr'text/tab-separated-values');
}

done_testing();
