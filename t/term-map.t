use v5.14;
use Data::Dumper;
use Test::More;
use Type::Tiny::Role;

use Attean::RDF qw(iri blank literal dtliteral langliteral triple quad);
my $t = triple(blank('xxx'), iri('p'), literal('1'));
my $u = triple(blank('yyy'), iri('p'), literal('2'));
my $v = triple(blank('yyy'), iri('p'), $t);

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
	my $uuid_blank		= qr/^_:b[0-9A-Za-z]{32}$/;
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
	
	{
		note('Mapping triple terms');
		my $iter	= Attean::ListIterator->new(values => [$t, $v], item_type => 'Attean::API::Triple');
		my $c		= $iter->next->apply_map($mapper);
		my $d		= $iter->next->apply_map($mapper);
		is($c->subject->ntriples_string, $d->object->subject->ntriples_string, 'blank node in triple and separate triple term');
	}
};

subtest 'canonicalize literal' => sub {
		my $i = Attean::Literal->integer('+12');
		my $m = Attean::TermMap->canonicalization_map;
		my $new_i = $m->map($i);

		is($new_i->ntriples_string, '"12"^^<http://www.w3.org/2001/XMLSchema#integer>');
};

subtest 'UUID blank node label mapping over CDTs' => sub {
		my $b	= Attean::Blank->new( value => 'xxx' );
		my $l	= Attean::Literal->new(value => '[_:abc, _:xxx, { "a": _:xxx }]', datatype => "http://w3id.org/awslabs/neptune/SPARQL-CDTs/List");
		my $m	= Attean::TermMap->uuid_blank_map;

		my $new_l	= $m->map($l);
		my $new_b	= $m->map($b);
		
		my $new_b_label	= $new_b->value;
		my $label		= quotemeta($new_b_label);
		like($new_l->value, qr/_:$label,.*_:$label\s*[}].*\]/);
		unlike($new_l->value, qr/_:abc\b/);
};

subtest 'short blank node label mapping over CDTs' => sub {
		my $b	= Attean::Blank->new( value => 'xxx' );
		my $l	= Attean::Literal->new(value => '[_:abc, _:xxx, { "a": _:xxx }]', datatype => "http://w3id.org/awslabs/neptune/SPARQL-CDTs/List");
		my $m			= Attean::TermMap->short_blank_map;

		my $new_l	= $m->map($l);
		my $new_b	= $m->map($b);

		my $new_b_label	= $new_b->value;
		my $label		= quotemeta($new_b_label);
		like($new_l->value, qr/_:$label,.*_:$label\s*[}].*\]/);
		unlike($new_l->value, qr/_:abc\b/);
};

subtest 'rewrite mapping over CDTs' => sub {
		my $b	= Attean::Blank->new( value => 'xxx' );
		my $bb	= Attean::Blank->new( value => 'newlabel' );
		my $l	= Attean::Literal->new(value => '[_:abc, _:xxx, { "a": _:xxx }]', datatype => "http://w3id.org/awslabs/neptune/SPARQL-CDTs/List");
		my $m			= Attean::TermMap->rewrite_map({ $b->as_string => $bb });

		my $new_l	= $m->map($l);
		my $new_b	= $m->map($b);

		my $new_b_label	= $new_b->value;
		my $label		= quotemeta($new_b_label);
		like($new_l->value, qr/_:$label,.*_:$label\s*[}].*\]/);
		like($new_l->value, qr/_:abc\b/);
};

subtest 'canonicalization mapping over CDTs' => sub {
		my $l	= Attean::Literal->new(value => '[_:abc, "+12"^^<http://www.w3.org/2001/XMLSchema#integer>, _:xxx]', datatype => "http://w3id.org/awslabs/neptune/SPARQL-CDTs/List");
		my $m			= Attean::TermMap->canonicalization_map;

		my $new_l	= $m->map($l);
		is($new_l->value, '[_:abc,12,_:xxx]');
};

done_testing();
