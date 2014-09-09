use Test::More;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::RDF;

is_deeply([Attean::API::Triple->variables], [qw(subject predicate object)]);
is_deeply([Attean::API::Quad->variables], [qw(subject predicate object graph)]);

{
	note('Attean::Triple');
	my $b	= triple(blank('eve'), iri('http://xmlns.com/foaf/0.1/name'), literal('Eve'));
	dies_ok { $b->value('xxx') } 'bad binding key';
	does_ok($b, 'Attean::API::Binding');
	is_deeply([$b->variables], [qw(subject predicate object)], 'variables');
	is_deeply([$b->values], [blank('eve'), iri('http://xmlns.com/foaf/0.1/name'), literal('Eve')], 'values');
	my %m	= $b->mapping;
	is_deeply(\%m, { subject => blank('eve'), predicate => iri('http://xmlns.com/foaf/0.1/name'), object => literal('Eve') }, 'mapping');
	is_deeply($b->value('subject'), blank('eve'), 'value');
	my $qp	= $b->as_quad_pattern(variable('g'));
	my $q	= $b->as_quad(iri('graph'));
	does_ok($qp, 'Attean::API::Binding');
	does_ok($qp, 'Attean::API::QuadPattern');
	does_ok($q, 'Attean::API::Binding');
	does_ok($q, 'Attean::API::Quad');
	is_deeply($q, quad(blank('eve'), iri('http://xmlns.com/foaf/0.1/name'), literal('Eve'), iri('graph')));
}

{
	note('Attean::Quad');
	my $b	= quad(blank('eve'), iri('http://xmlns.com/foaf/0.1/name'), literal('Eve'), iri('graph'));
	dies_ok { $b->value('xxx') } 'bad binding key';
	does_ok($b, 'Attean::API::Binding');
	is_deeply([$b->variables], [qw(subject predicate object graph)], 'variables');
	is_deeply([$b->values], [blank('eve'), iri('http://xmlns.com/foaf/0.1/name'), literal('Eve'), iri('graph')], 'values');
	my %m	= $b->mapping;
	is_deeply(\%m, { subject => blank('eve'), predicate => iri('http://xmlns.com/foaf/0.1/name'), object => literal('Eve'), graph => iri('graph') }, 'mapping');
	is_deeply($b->value('subject'), blank('eve'), 'value');
}

{
	note('Attean::Result');
	my $b	= Attean::Result->new( bindings => { name => literal('Eve') } );
	does_ok($b, 'Attean::API::Binding');
	is_deeply([$b->variables], ['name'], 'variables');
	is_deeply([$b->values], [literal('Eve')], 'values');
	my %m	= $b->mapping;
	is_deeply(\%m, { name => literal('Eve') }, 'mapping');
	is_deeply($b->value('name'), literal('Eve'), 'value');
}

{
	note('Attean::Result joining');
	my $shared	= blank('eve');
	my $b1	= Attean::Result->new( bindings => { p => $shared, type => iri('http://xmlns.com/foaf/0.1/Person') } );
	my $b2	= Attean::Result->new( bindings => { p => blank('eve'), name => literal('Eve') } );
	my $b3	= Attean::Result->new( bindings => { p => blank('alice'), name => literal('Alice') } );
	my $b4	= Attean::Result->new( bindings => { x => literal('xxx') } );
	my $b5	= Attean::Result->new( bindings => { p => $shared, name => literal('Eve') } );
	is($b1->join($b3), undef, 'intersecting result non-join');
	is($b1->join($b4)->as_string, '{p=_:eve, type=<http://xmlns.com/foaf/0.1/Person>, x="xxx"}', 'disjoint result join');
	is($b1->join($b2)->as_string, '{name="Eve", p=_:eve, type=<http://xmlns.com/foaf/0.1/Person>}', 'intersecting result join');
	is($b1->join($b5)->as_string, '{name="Eve", p=_:eve, type=<http://xmlns.com/foaf/0.1/Person>}', 'intersecting result join using shared term object');
}


done_testing();


sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
