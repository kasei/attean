use v5.14;
use Data::Dumper;
use Test::More;
use Type::Tiny::Role;

use Attean::RDF qw(iri blank literal dtliteral langliteral triple quad);
my $t = triple(blank('xxx'), iri('p'), literal('1'));
my $u = triple(blank('yyy'), iri('p'), literal('2'));
my $mapper = Attean::TermMap->short_blank_map;
my $bindings_mapper	= $mapper->binding_mapper;

{
	note('Mapping Iterator<Term>');
	my $iter	= Attean::ListIterator->new(values => [blank('a'), blank('zzz')], item_type => 'Attean::API::Term');
	my $mapped	= $iter->map( $mapper );
	my $a		= $mapped->next;
	my $b		= $mapped->next;
	is($a->ntriples_string, '_:a');
	is($b->ntriples_string, '_:b');
}

{
	note('Mapping Triples');
	my $iter	= Attean::ListIterator->new(values => [$t, $u], item_type => 'Attean::API::Triple');
	my $c		= $iter->next->apply_map($mapper);
	my $d		= $iter->next->apply_map($mapper);
	is($c->subject->ntriples_string, '_:c');
	is($d->subject->ntriples_string, '_:d');
}

{
	note('Mapping Iterator<Triple>');
	my $iter	= Attean::ListIterator->new(values => [$u, $t], item_type => 'Attean::API::Triple');
	my $mapped	= $iter->map( $bindings_mapper );
	my $d		= $mapped->next;
	my $c		= $mapped->next;
	is($c->subject->ntriples_string, '_:c');
	is($d->subject->ntriples_string, '_:d');
}

done_testing();
