use v5.14;
use autodie;
use utf8;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use File::Glob qw(bsd_glob);
use File::Spec;

use Attean;
use Type::Tiny::Role;

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
	can_ok($type, 'role');
	is($type->role, 'Attean::API::Triple');
}

{
	my $turtle	= "<s> <p> 1, 2 .\n";
	open(my $fh, '<', \$turtle);
	my $parser	= Attean->get_parser('Turtle')->new();
	my $iter	= $parser->parse_iter_from_io($fh);
	does_ok($iter, 'Attean::API::Iterator');
	is($iter->next->object->value, '1');
	is($iter->next->object->value, '2');
	is($iter->next, undef);
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

done_testing();

sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
