use v5.14;
use autodie;
use utf8;
use Test::Modern;
use Test::Exception;
use FindBin qw($Bin);
use File::Glob qw(bsd_glob);
use File::Spec;

use Attean;

my $p	= Attean->get_parser('Turtle');
is($p, 'AtteanX::Parser::Turtle');

subtest 'all acceptable parsers' => sub {
	my $accept	= Attean->acceptable_parsers();
	ok(length($accept), 'got accept header value');
	
	# check to make sure some of the default parsers are present:
	like($accept, qr'text/turtle');
	like($accept, qr'application/rdf[+]xml');
	like($accept, qr'text/tab-separated-values');
};

subtest 'acceptable PULL parsers' => sub {
	my $accept	= Attean->acceptable_parsers(prefer => q[pull]);
	
	# check to make sure some of the default parsers are present:
	like($accept, qr'application/n-quads');
	like($accept, qr'application/n-triples');
	like($accept, qr'text/tab-separated-values');
	
	unlike($accept, qr'application/rdf[+]xml');
};

subtest 'acceptable PUSH parsers' => sub {
	my $accept	= Attean->acceptable_parsers(prefer => q[Attean::API::PushParser]);
	
	# check to make sure some of the default parsers are present:
	like($accept, qr'text/turtle');
	like($accept, qr'application/rdf[+]xml');
	like($accept, qr'application/sparql-results[+]xml');

	unlike($accept, qr'application/n-quads');
	unlike($accept, qr'application/n-triples');
	unlike($accept, qr'text/tab-separated-values');
};

subtest 'acceptable ATONCE parsers' => sub {
	my $accept	= Attean->acceptable_parsers(prefer => q[AtOnce]);
	
	like($accept, qr'application/sparql-results[+]json');

	unlike($accept, qr'text/turtle');
	unlike($accept, qr'application/n-quads');
	unlike($accept, qr'application/n-triples');
};

subtest 'acceptable SPARQL RESULT parsers' => sub {
	my $accept	= Attean->acceptable_parsers(handles => q[result]);
	
	like($accept, qr'application/sparql-results[+]json');
	like($accept, qr'application/sparql-results[+]xml');
	like($accept, qr'text/tab-separated-values');

	unlike($accept, qr'text/turtle');
	unlike($accept, qr'application/rdf[+]xml');
	unlike($accept, qr'application/n-quads');
	unlike($accept, qr'application/n-triples');
};

subtest 'acceptable TRIPLE parsers' => sub {
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
};

subtest 'parser access by filename' => sub {
	my $pclass	= Attean->get_parser(filename => 'foo.nt');
	is($pclass, 'AtteanX::Parser::NTriples');
};

subtest 'parser access by media type' => sub {
	my $pclass	= Attean->get_parser(media_type => 'application/n-triples');
	is($pclass, 'AtteanX::Parser::NTriples');
};

dies_ok {
	Attean->get_parser(foo => 'bar');
} 'bad get_parser argument dies';

done_testing();
