use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::RDF;
use Encode;
use Type::Tiny::Role;

my $constraint	= 'Attean::API::Triple';

subtest 'serializer access' => sub {
	my $sclass	= Attean->get_serializer('NTriples');
	is($sclass, 'AtteanX::Serializer::NTriples');
};

subtest 'serializer access by name' => sub {
	my $sclass	= Attean->get_serializer(media_type => 'application/n-triples');
	like($sclass, qr'^AtteanX::Serializer::\w*NTriples$');
};

subtest 'serializer access by media type' => sub {
	my $sclass	= Attean->get_serializer(media_type => 'application/n-triples');
	like($sclass, qr'^AtteanX::Serializer::\w*NTriples$');
};

dies_ok {
	Attean->get_serializer(foo => 'bar');
} 'bad get_serializer argument dies';

done_testing();
