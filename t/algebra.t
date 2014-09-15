use v5.14;
use autodie;
use utf8;
use Test::More;
use Test::Exception;
use Digest::SHA qw(sha1_hex);

use Attean;
use Attean::RDF;
use Attean::SimpleQueryEvaluator;

if ($ENV{ATTEAN_TYPECHECK}) {
	my $bgp	= Attean::Algebra::BGP->new();
	dies_ok { Attean::Algebra::Join->new( children => [] ); } 'bad join arity (0)';
	dies_ok { Attean::Algebra::Join->new( children => [$bgp] ); } 'bad join arity (1)';
	dies_ok { Attean::Algebra::Join->new( children => [$bgp, $bgp, $bgp] ); } 'bad join arity (3)';
}

{
	my $b	= Attean::Algebra::BGP->new(triples => []);
	isa_ok($b, 'Attean::Algebra::BGP');
	ok($b->does('Attean::API::QueryTree'), 'bgp consumes QueryTree');
	ok($b->is_leaf, 'bgp is_leaf');
}

{
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	isa_ok($bgp, 'Attean::Algebra::BGP');
	ok($bgp->is_leaf, 'bgp is_leaf');
	
	my $dist	= Attean::Algebra::Distinct->new( children => [$bgp] );
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
	my $t	= triple(variable('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $join	= Attean::Algebra::Join->new( children => [$bgp, $bgp] );
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

done_testing();
