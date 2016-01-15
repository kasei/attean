use v5.14;
use warnings;
use autodie;
use Test::Modern;
use utf8;

use Attean;
use Attean::RDF;

subtest 'parser construction' => sub {
	my $parser	= Attean->get_parser('RDFXML')->new();
	isa_ok( $parser, 'AtteanX::Parser::RDFXML' );
};

subtest 'simple triple parse with namespaces' => sub {
	my $map		= URI::NamespaceMap->new();
	my $parser	= Attean->get_parser('RDFXML')->new( namespaces => $map );
	my $store		= Attean->get_store('Memory')->new();
	my $content	= <<"END";
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
         xmlns:eg="http://example.org/"
         xml:base="http://example.org/dir/file">

 <rdf:Description rdf:ID="frag" eg:value="v" />
</rdf:RDF>
END
	my @list	= $parser->parse_list_from_bytes($content);
	is(scalar(@list), 1);
	my ($t)		= @list;
	does_ok($t, 'Attean::API::Triple');
	is($t->as_string, '<http://example.org/dir/file#frag> <http://example.org/value> "v" .');

	is_deeply([sort $map->list_prefixes], [qw(eg rdf)]);
	my $rdf	= $map->namespace_uri('rdf');
	isa_ok($rdf, 'URI::Namespace');
	is($rdf->as_string, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');
};

done_testing();
