use v5.14;
use autodie;
use utf8;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use File::Glob qw(bsd_glob);
use File::Spec;

use Attean;

sub iri {
	my $value	= shift;
	return Attean::IRI->new(value => $value);
}

sub literal {
	my $value	= shift;
	my $lang	= shift;
	my $dt		= shift;
	my %args	= (value => $value);
	$args{language}	= $lang if (defined($lang));
	$args{datatype}	= $dt if (defined($dt));
	return Attean::IRI->new(%args);
}

{
	my $parser	= Attean->get_parser('Turtle')->new();
	isa_ok($parser, 'AtteanX::Parser::Turtle');
	my $type	= $parser->handled_type;
	isa_ok($type, 'Moose::Meta::TypeConstraint::Role');
	is($type->role, 'Attean::API::Triple');
}

{
	my $map		= URI::NamespaceMap->new();
	my $parser	= Attean->get_parser('Turtle')->new( namespaces => $map );
	my $content	= <<'END';
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
_:x a foaf:Person .
END
	$parser->parse_cb_from_bytes($content, sub {});
	is_deeply([sort $map->list_prefixes], [qw(ex foaf)]);
	my $foaf	= $map->namespace_uri('foaf');
	isa_ok($foaf, 'URI::Namespace');
	is($foaf->as_string, 'http://xmlns.com/foaf/0.1/');
}

my $path	= File::Spec->catfile( $Bin, 'data', 'turtle' );
my @good	= bsd_glob("${path}/test*.ttl");
my @bad		= bsd_glob("${path}/bad*.ttl");

foreach my $file (@good) {
	my $data	= do { open( my $fh, '<', $file ); local($/) = undef; <$fh> };
	my (undef, undef, $test)	= File::Spec->splitpath( $file );
	lives_ok {
		my $parser	= Attean->get_parser('Turtle')->new();
		open(my $fh, '<', $file);
		$parser->parse_cb_from_io($fh, sub{
			my $t	= shift;
# 			warn "parsed triple: " . $t->tuples_string . "\n";
		});
	} $test;
}

# warn "------------------------------------------------------\n";

foreach my $file (@bad) {
	my $data	= do { open( my $fh, '<', $file ); local($/) = undef; <$fh> };
	my (undef, undef, $test)	= File::Spec->splitpath( $file );
	dies_ok {
		my $parser	= Attean->get_parser('Turtle')->new();
		open(my $fh, '<', $file);
		my $url	= 'file://' . $file;
		$parser->parse_cb_from_io($fh);
	} $test;
}

done_testing();
