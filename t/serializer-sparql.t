#!/usr/bin/env perl

use v5.14;
use autodie;
use utf8;
use Test::More;
use Test::Exception;
use Digest::SHA qw(sha1_hex);
use AtteanX::SPARQL::Constants;

use Attean;
use Attean::RDF;

subtest 'expected tokens: empty BGP tokens' => sub {
	my $bgp	= Attean::Algebra::BGP->new(triples => []);
	my $i	= $bgp->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	my @expect	= ();
	while (my $t = $i->next) {
		my $type	= AtteanX::SPARQL::Constants::decrypt_constant($t->type);
		is($t->type, shift(@expect), "expected token type $type");
	}
	is(scalar(@expect), 0);
};

subtest 'expected tokens: 1-triple BGP tokens' => sub {
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $i	= $bgp->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	my @expect	= (IRI, IRI, STRING1D, DOT);
	while (my $t = $i->next) {
		my $type	= AtteanX::SPARQL::Constants::decrypt_constant($t->type);
		is_token_of_type($t, shift(@expect));
	}
	is(scalar(@expect), 0);
};

subtest 'expected tokens: 2-BGP join tokens' => sub {
	my $t	= triple(variable('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $join	= Attean::Algebra::Join->new( children => [$bgp, $bgp] );
	my $i	= $join->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# { ?v <p> "1" . ?v <p> "1" . }
	my @expect	= (LBRACE, VAR, IRI, STRING1D, DOT, VAR, IRI, STRING1D, DOT, RBRACE);
	while (my $t = $i->next) {
		my $type	= AtteanX::SPARQL::Constants::decrypt_constant($t->type);
		is_token_of_type($t, shift(@expect));
	}
	is(scalar(@expect), 0);
};

subtest 'expected tokens: distinct/bgp' => sub {
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $dist	= Attean::Algebra::Distinct->new( children => [$bgp] );
	my $i	= $dist->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# SELECT DISTINCT * WHERE { <s> <p> "1" }
	my @expect	= (KEYWORD, KEYWORD, STAR, KEYWORD, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE);
	while (my $t = $i->next) {
		my $type	= AtteanX::SPARQL::Constants::decrypt_constant($t->type);
		is_token_of_type($t, shift(@expect));
	}
	is(scalar(@expect), 0);
};

subtest 'expected tokens: bgp/limit' => sub {
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $s	= Attean::Algebra::Slice->new( children => [$bgp], limit => 5 );
	my $i	= $s->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# SELECT * WHERE { <s> <p> "1" } LIMIT 5
	my @expect	= (KEYWORD, STAR, KEYWORD, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE, KEYWORD, INTEGER);
	while (my $t = $i->next) {
		my $type	= AtteanX::SPARQL::Constants::decrypt_constant($t->type);
		is_token_of_type($t, shift(@expect));
	}
	is(scalar(@expect), 0);
};

subtest 'expected tokens: bgp/slice' => sub {
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $s	= Attean::Algebra::Slice->new( children => [$bgp], limit => 5, offset => 5 );
	my $i	= $s->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# SELECT * WHERE { <s> <p> "1" } LIMIT 5 OFFSET 5
	my @expect	= (KEYWORD, STAR, KEYWORD, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE, KEYWORD, INTEGER, KEYWORD, INTEGER);
	while (my $t = $i->next) {
		my $type	= AtteanX::SPARQL::Constants::decrypt_constant($t->type);
		is_token_of_type($t, shift(@expect));
	}
	is(scalar(@expect), 0);
};

subtest 'expected tokens: distinct/bgp/slice' => sub {
	my $t		= triple(iri('s'), iri('p'), literal('1'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
	my $dist	= Attean::Algebra::Distinct->new( children => [$bgp] );
	my $s		= Attean::Algebra::Slice->new( children => [$dist], limit => 5, offset => 5 );
	my $i		= $s->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# SELECT DISTINCT * WHERE { <s> <p> "1" } LIMIT 5 OFFSET 5
	my @expect	= (KEYWORD, KEYWORD, STAR, KEYWORD, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE, KEYWORD, INTEGER, KEYWORD, INTEGER);
	while (my $t = $i->next) {
		my $type	= AtteanX::SPARQL::Constants::decrypt_constant($t->type);
		is_token_of_type($t, shift(@expect));
	}
	is(scalar(@expect), 0);
};

done_testing();
exit;

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

{
	my $t		= triple(variable('s'), iri('p'), variable('o'));
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
	like($s, qr/Group { [?]s } aggregate { [?]sum ← SUM\([?]s\) }/, 'aggregate serialization');
}

{
	note('Aggregation');
	my $t		= triple(variable('s'), iri('p'), variable('o'));
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
	like($s, qr/Group { [?]s } aggregate { [?]sum ← SUM\([?]s\) }/, 'aggregate serialization');
}

{
	note('Ranking');
	# RANKing example for 2 youngest students per school
	my $bgp		= Attean::Algebra::BGP->new(triples => [
		triple(variable('p'), iri('ex:name'), variable('name')),
		triple(variable('p'), iri('ex:school'), variable('school')),
		triple(variable('p'), iri('ex:age'), variable('age')),
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
	like($s, qr/Group { [?]school } aggregate { [?]rank ← RANK\([?]age\) }/, 'ranking serialization');
}

done_testing();


sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}

sub is_token_of_type {
	my $t			= shift;
	my $got			= $t->type;
	my $expect		= shift;
	if ($expect == A) {
		Carp::confess;
	}
	my $got_name	= AtteanX::SPARQL::Constants::decrypt_constant($got);
	my $expect_name	= AtteanX::SPARQL::Constants::decrypt_constant($expect);
	if ($got == $expect) {
		pass("Expected token type $got_name");
	} else {
		my $value	= $t->value;
		fail("Not expected token type (expected $expect_name, but got $got_name $value)");
	}
}
