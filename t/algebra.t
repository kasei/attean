use v5.14;
use autodie;
use utf8;
use Test::Modern;
use Test::Exception;
use Digest::SHA qw(sha1_hex);

use Attean;
use Attean::RDF;

{
	my $b	= Attean::Algebra::BGP->new(triples => []);
	isa_ok($b, 'Attean::Algebra::BGP');
	ok($b->does('Attean::API::QueryTree'), 'bgp consumes QueryTree');
	ok($b->is_leaf, 'bgp is_leaf');
	is($b->arity, 0, 'bgp arity');
	ok(not($b->unary), 'BGP is not unary');
}

{
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	isa_ok($bgp, 'Attean::Algebra::BGP');
	ok($bgp->is_leaf, 'bgp is_leaf');
	
	my $dist	= Attean::Algebra::Distinct->new( children => [$bgp] );
	is($dist->arity, 1, 'Distinct arity');
	ok($dist->unary, 'Distinct is unary');
	isa_ok($dist, 'Attean::Algebra::Distinct');
	ok(not($dist->is_leaf), 'distinct not is_leaf');
	
	{
		my @prefix_seen;
		my @postfix_seen;
		my $prefix	= sub {
			my $node	= shift;
			my $name	= ref($node);
			$name	=~ s/^.*://;
			push(@prefix_seen, $name);
		};
		my $postfix	= sub {
			my $node	= shift;
			my $name	= ref($node);
			$name	=~ s/^.*://;
			push(@postfix_seen, $name);
		};
		$dist->walk( prefix => $prefix, postfix => $postfix );
		is_deeply(\@prefix_seen, [qw'Distinct BGP'], 'prefix walk order');
		is_deeply(\@postfix_seen, [qw'BGP Distinct'], 'postfix walk order');
	}
}

{
	my $t	= triplepattern(variable('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	ok($bgp->has_only_subtree_types('Attean::Algebra::BGP'));
	my $join	= Attean::Algebra::Join->new( children => [$bgp, $bgp] );
	ok(not $join->has_only_subtree_types('Attean::Algebra::BGP'));
	
	my @walk;
	$join->walk(prefix => sub { push(@walk, shift) });
	is(scalar(@walk), 3, 'expected walk count');
	
	my @cover;
	$join->cover(prefix => sub { push(@cover, shift) });
	is(scalar(@cover), 2, 'expected cover count');
}

{
	my $p1	= iri('p1');
	my $pp1	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
	ok($pp1->does('Attean::API::PropertyPath'), 'PredicatePath consumes PropertyPath');
	is($pp1->as_string, '<p1>', 'PredicatePath as_string');
	
	my $p2	= iri('p2');
	my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );

	my $nps	= Attean::Algebra::NegatedPropertySet->new( predicates => [$p1, $p2] );
	ok($nps->does('Attean::API::PropertyPath'), 'NegatedPropertySet consumes PropertyPath');
	is($nps->as_string, '!(<p1>|<p2>)', 'NegatedPropertySet as_string');
	
	my $seq1	= Attean::Algebra::SequencePath->new( children => [$pp2] );
	is($seq1->as_string, '<p2>', 'unary SequencePath as_string');

	my $seq	= Attean::Algebra::SequencePath->new( children => [$pp1, $pp2] );
	is($seq->as_string, '(<p1>/<p2>)', 'SequencePath as_string');

	my $alt1	= Attean::Algebra::AlternativePath->new( children => [$pp2] );
	is($alt1->as_string, '<p2>', 'unary AlternativePath as_string');

	my $alt	= Attean::Algebra::AlternativePath->new( children => [$pp1, $pp2] );
	is($alt->as_string, '(<p1>|<p2>)', 'AlternativePath as_string');

	my $inv1	= Attean::Algebra::InversePath->new( children => [$pp2] );
	is($inv1->as_string, '^<p2>', 'InversePath as_string');
	
	my $inv_seq	= Attean::Algebra::InversePath->new( children => [$seq] );
	is($inv_seq->as_string, '^(<p1>/<p2>)', 'complex InversePath as_string');
	
	my $inv_seq_star	= Attean::Algebra::ZeroOrMorePath->new( children => [$inv_seq] );
	is($inv_seq_star->as_string, '(^(<p1>/<p2>))*', 'complex ZeroOrMorePath as_string');
}

{
	note('BGP canonicalization');
	my $b		= blank('person');
	my $rdf_type	= iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
	my $foaf_name	= iri('http://xmlns.com/foaf/0.1/name');
	my $foaf_knows	= iri('http://xmlns.com/foaf/0.1/knows');
	my $foaf_Person	= iri('http://xmlns.com/foaf/0.1/Person');
	my $bgp1	= Attean::Algebra::BGP->new( triples => [
		triplepattern($b, $rdf_type, $foaf_Person),
		triplepattern($b, $foaf_name, variable('name')),
		triplepattern($b, $foaf_knows, variable('knows')),
	] );
	my $bgp2	= Attean::Algebra::BGP->new( triples => [
		triplepattern(blank('s'), $foaf_knows, variable('person')),
		triplepattern(blank('s'), $rdf_type, $foaf_Person),
		triplepattern(blank('s'), $foaf_name, variable('myname')),
	] );

	my $hash1	= sha1_hex( join("\n", map { $_->tuples_string } (@{$bgp1->triples}) ) );
	my $hash2	= sha1_hex( join("\n", map { $_->tuples_string } (@{$bgp2->triples}) ) );
	isnt($hash1, $hash2, 'non-matching pre-canonicalized BGP hashes');
	
	my ($cbgp1, $m1)	= $bgp1->canonical_bgp_with_mapping;
	my ($cbgp2, $m2)	= $bgp2->canonical_bgp_with_mapping;
	
	my $chash1	= sha1_hex( join("\n", map { $_->tuples_string } (@{$cbgp1->triples}) ) );
	my $chash2	= sha1_hex( join("\n", map { $_->tuples_string } (@{$cbgp2->triples}) ) );
	is($chash1, $chash2, 'matching canonicalized BGP hashes' );
	
	is_deeply($m1, { '?name' => { 'prefix' => '?', 'id' => 'v003', 'type' => 'variable' }, '?knows' => { 'id' => 'v002', 'prefix' => '?', 'type' => 'variable' }, '_:person' => { 'id' => 'v001', 'prefix' => '_:', 'type' => 'blank' } }, 'BGP1 mapping');
	is_deeply($m2, { '?person' => { 'prefix' => '?', 'id' => 'v002', 'type' => 'variable' }, '_:s' => { 'prefix' => '_:', 'id' => 'v001', 'type' => 'blank' }, '?myname' => { 'type' => 'variable', 'id' => 'v003', 'prefix' => '?' } }, 'BGP2 mapping');
}

subtest 'Triple canonicalization' => sub {
	my $t = triplepattern(variable('bar'), iri('p'), variable('foo'));
	my $u = triplepattern(variable('subject'), iri('p'), variable('object'));
	my $v = triplepattern(variable('foo'), iri('p'), variable('foo'));
	my $w = triplepattern(variable('x'), iri('p'), variable('x'));
	is($t->canonicalize->as_string, $u->canonicalize->as_string, 'Canonical strings match for 2-variable triple');
	isnt($t->canonicalize->as_string, $v->canonicalize->as_string, 'Canonical strings do not match for 2-variable triple');
	is($v->canonicalize->as_string, $w->canonicalize->as_string, 'Canonical strings match for 1 shared-variable triple');
};

subtest 'Quad canonicalization' => sub {
	my $t = quadpattern(variable('bar'), iri('p'), variable('foo'), iri('g'));
	my $u = quadpattern(variable('subject'), iri('p'), variable('object'), iri('g'));
	my $v = quadpattern(variable('foo'), iri('p'), literal('1'), variable('foo'));
	my $w = quadpattern(variable('x'), iri('p'), literal('1'), variable('x'));
	my $x = quadpattern(variable('x'), iri('p'), variable('x'), variable('x'));
	my $y = quadpattern(variable('x'), iri('p'), variable('x'), variable('x'));
	is($t->canonicalize->as_string, $u->canonicalize->as_string, 'Canonical strings match for 2-variable quad');
	isnt($t->canonicalize->as_string, $v->canonicalize->as_string, 'Canonical strings do not match for 2-variable quad');
	is($v->canonicalize->as_string, $w->canonicalize->as_string, 'Canonical strings match for 1 shared-variable quad');
	is($x->canonicalize->as_string, $y->canonicalize->as_string, 'Canonical strings match for 1 twice-shared-variable quad');
};

{
	my $t		= triplepattern(variable('s'), iri('p'), variable('o'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
	my @groups	= Attean::ValueExpression->new( value => variable('s') );
	my @aggs	= Attean::AggregateExpression->new(
		distinct	=> 0,
		operator	=> 'SUM',
		children	=> [Attean::ValueExpression->new( value => variable('s') )],
		scalar_vars	=> {},
		variable	=> variable("sum"),
	);
	my $agg		= Attean::Algebra::Group->new(
		children => [$bgp],
		groupby => \@groups,
		aggregates => \@aggs,
	);
	my $s	= $agg->as_string;
	like($s, qr/Group [{] [?]s [}] aggregate [{] [?]sum ← SUM\([?]s\) [}]/, 'aggregate serialization');
}

subtest 'Aggregation' => sub {
	my $t		= triplepattern(variable('s'), iri('p'), variable('o'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
	my @groups	= Attean::ValueExpression->new( value => variable('s') );
	my @aggs	= Attean::AggregateExpression->new(
		distinct	=> 0,
		operator	=> 'SUM',
		children	=> [Attean::ValueExpression->new( value => variable('s') )],
		scalar_vars	=> {},
		variable	=> variable("sum"),
	);
	my $agg		= Attean::Algebra::Group->new(
		children => [$bgp],
		groupby => \@groups,
		aggregates => \@aggs,
	);
	my $s	= $agg->as_string;
	like($s, qr/Group [{] [?]s [}] aggregate [{] [?]sum ← SUM\([?]s\) [}]/, 'aggregate serialization');
};

subtest 'Ranking' => sub {
	# RANKing example for 2 youngest students per school
	my $bgp		= Attean::Algebra::BGP->new(triples => [
		triplepattern(variable('p'), iri('ex:name'), variable('name')),
		triplepattern(variable('p'), iri('ex:school'), variable('school')),
		triplepattern(variable('p'), iri('ex:age'), variable('age')),
	]);
	my @groups	= Attean::ValueExpression->new( value => variable('school') );
	my $r_agg	= Attean::AggregateExpression->new(
		distinct	=> 0,
		operator	=> 'RANK',
		children	=> [Attean::ValueExpression->new( value => variable('age') )],
		scalar_vars	=> {},
		variable	=> variable("rank"),
	);
	my $agg		= Attean::Algebra::Group->new(
		children => [$bgp],
		groupby => \@groups,
		aggregates => [$r_agg],
	);
	my $rank	= Attean::Algebra::Filter->new(
		children	=> [$agg],
		expression	=> Attean::BinaryExpression->new(
			children => [
				Attean::ValueExpression->new( value => variable('rank') ),
				Attean::ValueExpression->new( value => Attean::Literal->integer('2') ),
			],
			operator => '<='
		),
	);
	my $s	= $rank->as_string;
	like($s, qr/Group [{] [?]school [}] aggregate [{] [?]rank ← RANK\([?]age\) [}]/, 'ranking serialization');
};

subtest 'Query Serialization' => sub {
	{
		my $a	= Attean->get_parser('SPARQL')->parse('SELECT * WHERE { ?s ?p 2 }');
		like($a->as_string, qr/Query.*Project.*BGP/s);
	}
	
	{
		my $a	= Attean->get_parser('SPARQL')->parse('SELECT REDUCED * WHERE { SERVICE <http://example.org/sparql> { ?s <p>*/<q> 2 } } ORDER BY ?s');
		like($a->as_string, qr/Query.*Reduced.*Project.*Order.*Service.*Path/s);
	}
	
	{
		my $a	= Attean->get_parser('SPARQL')->parse('SELECT * WHERE { { ?s <p> 1 . BIND(?s+1 AS ?x) } UNION { GRAPH <g> { ?s <p> 1 } } }');
		like($a->as_string, qr/Project.*Union.*Extend.*BGP.*Graph.*BGP/s);
	}

	{
		my $a	= Attean->get_parser('SPARQL')->parse('SELECT * WHERE { { ?s <p> 1 } MINUS { ?s <q> 2 } }');
		like($a->as_string, qr/Query.*Project.*Minus.*BGP.*BGP/s);
	}

	{
		my $a	= Attean->get_parser('SPARQL')->parse('SELECT * WHERE { ?s <q> 2 } VALUES (?z) { ("abc") ("def") }');
		like($a->as_string, qr/Query.*Project.*Join.*BGP.*Table/s);
	}

	{
		my $a	= Attean->get_parser('SPARQL')->parse('CONSTRUCT { ?s ?p 1 } WHERE { ?s ?p 2 }');
		like($a->as_string, qr/Query.*Construct.*BGP/s);
		like($a->as_string, qr/1/s);
		like($a->as_string, qr/2/s);
	}
};

subtest 'Modify' => sub {
	my $a	= Attean->get_parser('SPARQL')->parse_update('INSERT { ?s ?p 1 } WHERE { ?s ?p 2 }');
	is_deeply([$a->in_scope_variables], []);
	like($a->as_string, qr/Update.*Insert.*Data/s);
};

subtest 'Add' => sub {
	my $a	= Attean->get_parser('SPARQL')->parse_update('ADD GRAPH <g1> TO DEFAULT');
	is_deeply([$a->in_scope_variables], []);
	like($a->as_string, qr/Update.*Add/s);
};

subtest 'Update Sequence' => sub {
	my $a	= Attean->get_parser('SPARQL')->parse_update('ADD GRAPH <g1> TO DEFAULT; ADD GRAPH <g2> TO DEFAULT');
	like($a->as_string, qr/Update.*Add.*Add/s);
};

done_testing();
