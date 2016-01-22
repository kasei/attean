use v5.14;
use warnings;
use autodie;
use Test::Modern;
use Test::Exception;
use utf8;

use Attean;
use Attean::RDF;

subtest 'parser construction and metadata' => sub {
	my $parser	= Attean->get_parser('RDFXML')->new();
	isa_ok( $parser, 'AtteanX::Parser::RDFXML' );
	is($parser->canonical_media_type, 'application/rdf+xml', 'canonical_media_type');
	my %extensions	= map { $_ => 1 } @{ $parser->file_extensions };
	ok(exists $extensions{'rdf'}, 'file_extensions');
};

subtest 'empty document' => sub {
	my $parser	= Attean->get_parser('RDFXML')->new();
	my @list	= $parser->parse_list_from_bytes('');
	is(scalar(@list), 0);
};

subtest 'invalid documents' => sub {
	my $parser	= Attean->get_parser('RDFXML')->new();
	dies_ok {
		my @list	= $parser->parse_list_from_bytes('<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"');
	}, 'invalid XML';
# 	dies_ok {
# 		my @list	= $parser->parse_list_from_bytes(<<"END");
# <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:ex="http://example.org/">
# 	<rdf:Description>
# 		<ex:p/>
# 	</rdf:Description>
# </rdf:RDF>
# END
# 		use Data::Dumper;
# 		warn Dumper(\@list);
# 	}, 'invalid RDF/XML';
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

subtest 'bnode prefix' => sub {
	my $parser	= Attean->get_parser('RDFXML')->new( bnode_prefix => 'foo' );
	my ($t)		= $parser->parse_list_from_bytes(<<"END");
<rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#" xmlns:ex="http://example.org/">
	<rdf:Description>
		<ex:p>Hello!</ex:p>
	</rdf:Description>
</rdf:RDF>
END
	my $subj	= $t->subject;
	does_ok($subj, 'Attean::API::Blank');
	like($subj->value, qr/^foo/, 'bnode prefix');
};

done_testing();
