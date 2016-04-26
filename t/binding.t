use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;
use Attean::RDF;

is_deeply([Attean::API::Triple->variables], [qw(subject predicate object)]);
is_deeply([Attean::API::Quad->variables], [qw(subject predicate object graph)]);

subtest 'Attean::Triple' => sub {
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
};

subtest 'Attean::Quad' => sub {
	my $b	= quad(blank('eve'), iri('http://xmlns.com/foaf/0.1/name'), literal('Eve'), iri('graph'));
	dies_ok { $b->value('xxx') } 'bad binding key';
	does_ok($b, 'Attean::API::Binding');
	is_deeply([$b->variables], [qw(subject predicate object graph)], 'variables');
	is_deeply([$b->values], [blank('eve'), iri('http://xmlns.com/foaf/0.1/name'), literal('Eve'), iri('graph')], 'values');
	my %m	= $b->mapping;
	is_deeply(\%m, { subject => blank('eve'), predicate => iri('http://xmlns.com/foaf/0.1/name'), object => literal('Eve'), graph => iri('graph') }, 'mapping');
	is_deeply($b->value('subject'), blank('eve'), 'value');
};

subtest 'Attean::Result' => sub {
	my $b	= Attean::Result->new( bindings => { name => literal('Eve') } );
	does_ok($b, 'Attean::API::Binding');
	is_deeply([$b->variables], ['name'], 'variables');
	is_deeply([$b->values], [literal('Eve')], 'values');
	my %m	= $b->mapping;
	is_deeply(\%m, { name => literal('Eve') }, 'mapping');
	is_deeply($b->value('name'), literal('Eve'), 'value');
};

subtest 'Attean::Result joining' => sub {
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
};

subtest 'Attean::TriplePattern' => sub {
	my $b	= triplepattern(variable('eve'), iri('http://xmlns.com/foaf/0.1/name'), literal('Eve'));
	does_ok($b, 'Attean::API::Binding');
	my $qp	= $b->as_quadpattern(variable('g'));
	does_ok($b, 'Attean::API::Binding');
	isa_ok($qp, 'Attean::QuadPattern');
};

subtest 'statement application' => sub {
	{
		my $t	= triple(iri('s'), iri('p'), iri('o'));
		my $b	= triplepattern(variable('object'), iri('http://xmlns.com/foaf/0.1/name'), literal('Eve'));
		my $x	= $b->apply_triple($t);
	
		does_ok($x, 'Attean::API::Binding');
		is_deeply([$x->variables], ['subject']);
		my $o	= $x->value('subject');
		does_ok($o, 'Attean::API::IRI');
		is($o->value, 'o');
	}
	{
		my $q	= triple(iri('s'), iri('p'), iri('o'), iri('ggg'));
		my $b	= quadpattern(variable('object'), iri('http://xmlns.com/foaf/0.1/name'), variable('subject'), iri('http://example.org/graph'));
		my $x	= $b->apply_quad($q);
	
		does_ok($x, 'Attean::API::Binding');
		is_deeply([sort $x->variables], [qw(object subject)]);
		
		my $s	= $x->value('subject');
		does_ok($s, 'Attean::API::IRI');
		is($s->value, 'o');
		
		my $o	= $x->value('object');
		does_ok($o, 'Attean::API::IRI');
		is($o->value, 's');
	}
};

subtest 'binding projection' => sub {
	my $b	= Attean::Result->new(bindings => {
		subject => iri('s'),
		predicate => iri('http://xmlns.com/foaf/0.1/name'),
		object => literal('Hello!')
	});
	my $p	= $b->project(qw(predicate object));
	does_ok($p, 'Attean::API::Result');
	is_deeply([sort $b->variables], [qw(object predicate subject)]);
	is_deeply([sort $p->variables], [qw(object predicate)]);
};

subtest 'Attean::API::Binding convenience parse method' => sub {
	{
		my $t	= Attean::Triple->parse('<s> a <Class>');
		does_ok($t, 'Attean::API::Triple');
		is($t->predicate->value, 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', 'parsed A');
	}
	
	{
		my $map		= URI::NamespaceMap->new({ foaf => 'http://xmlns.com/foaf/0.1/' });
		my $t	= Attean::TriplePattern->parse('?s a foaf:Person', namespaces => $map);
		does_ok($t, 'Attean::API::TriplePattern');
		does_ok($t->subject, 'Attean::API::Variable');
		is($t->object->value, 'http://xmlns.com/foaf/0.1/Person', 'parsed prefixname');
	}
	
	{
		my $q	= Attean::Quad->parse('<s> <p> "foo"@en <http://example.org/graph/>');
		does_ok($q, 'Attean::API::Quad');
		does_ok($q->graph, 'Attean::API::IRI');
		is($q->graph->value, 'http://example.org/graph/', 'parsed quad graph');
	}
};

done_testing();
