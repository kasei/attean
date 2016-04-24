use v5.14;
use Data::Dumper;
use Test::More;
use Type::Tiny::Role;

use Attean::RDF qw(iri blank literal dtliteral langliteral triple quad);
my $t = triple(blank('xxx'), iri('p'), literal('1'));
my $u = triple(blank('yyy'), iri('p'), literal('2'));

subtest 'short blank node label mapping' => sub {
	my $mapper			= Attean::TermMap->short_blank_map;
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
};

subtest 'UUID blank node label mapping' => sub {
	my $mapper			= Attean::TermMap->uuid_blank_map;
	my $bindings_mapper	= $mapper->binding_mapper;
	my $uuid_blank		= qr/^_:0x[0-9A-Z]{32}$/;
	{
		note('Mapping Iterator<Term>');
		my $iter	= Attean::ListIterator->new(values => [blank('a'), blank('zzz')], item_type => 'Attean::API::Term');
		my $mapped	= $iter->map( $mapper );
		my $a		= $mapped->next;
		my $b		= $mapped->next;
		like($a->ntriples_string, $uuid_blank);
		like($b->ntriples_string, $uuid_blank);
	}

	{
		note('Mapping Triples');
		my $iter	= Attean::ListIterator->new(values => [$t, $u], item_type => 'Attean::API::Triple');
		my $c		= $iter->next->apply_map($mapper);
		my $d		= $iter->next->apply_map($mapper);
		like($c->subject->ntriples_string, $uuid_blank);
		like($d->subject->ntriples_string, $uuid_blank);
	}

	{
		note('Mapping Iterator<Triple>');
		my $iter	= Attean::ListIterator->new(values => [$u, $t], item_type => 'Attean::API::Triple');
		my $mapped	= $iter->map( $bindings_mapper );
		my $d		= $mapped->next;
		my $c		= $mapped->next;
		like($c->subject->ntriples_string, $uuid_blank);
		like($d->subject->ntriples_string, $uuid_blank);
	}
};

done_testing();
