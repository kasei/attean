use v5.14;
use autodie;
use utf8;
use Test::More;
use Test::Exception;
use Digest::SHA qw(sha1_hex);

use Attean;
use Attean::RDF;
use Attean::IDPQueryPlanner;
use AtteanX::Store::Memory;


# √ Attean::Plan::Quad
# Attean::Plan::NestedLoopJoin
# Attean::Plan::HashJoin
# Attean::Plan::EBVFilter
# Attean::Plan::Merge
# Attean::Plan::Union
# Attean::Plan::Extend
# Attean::Plan::HashDistinct
# Attean::Plan::Unique
# Attean::Plan::Slice
# Attean::Plan::Project
# Attean::Plan::OrderBy
# Attean::Plan::Service
# Attean::Plan::Table

my $p	= Attean::IDPQueryPlanner->new();
isa_ok($p, 'Attean::IDPQueryPlanner');
does_ok($p, 'Attean::API::CostPlanner');

{
	my $store	= TestStore->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= iri('http://example.org/');
	my $t		= triple(variable('s'), iri('p'), literal('1'));
	my $u		= triple(variable('s'), iri('p'), literal('2'));
	my $v		= triple(variable('s'), iri('q'), blank('xyz'));
	my $w		= triple(variable('a'), iri('b'), iri('c'));

	subtest 'Empty BGP' => sub {
		note("An empty BGP should produce the join identity table plan");
		my $bgp		= Attean::Algebra::BGP->new(triples => []);
		my $plan	= $p->plan_for_algebra($bgp, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan', 'Empty BGP');
		isa_ok($plan, 'Attean::Plan::Table');
		my $rows	= $plan->rows;
		is(scalar(@$rows), 1);
	};

	subtest '1-triple BGP' => sub {
		note("A 1-triple BGP should produce a single Attean::Plan::Quad plan object");
		my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
		my $plan	= $p->plan_for_algebra($bgp, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan', '1-triple BGP');
		isa_ok($plan, 'Attean::Plan::Quad');
	};

	subtest '2-triple BGP without join variable' => sub {
		note("A 2-triple BGP without a join variable should produce a distinct nested loop join");
		my $bgp		= Attean::Algebra::BGP->new(triples => [$t, $w]);
		my $plan	= $p->plan_for_algebra($bgp, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan', '2-triple BGP');
		isa_ok($plan, 'Attean::Plan::NestedLoopJoin');
		ok($plan->distinct);
	};

	subtest '2-triple BGP with join variable' => sub {
		note("A 2-triple BGP with a join variable and without any ordering should produce a distinct hash join");
		my $bgp		= Attean::Algebra::BGP->new(triples => [$t, $u]);
		my $plan	= $p->plan_for_algebra($bgp, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan', '2-triple BGP');
		isa_ok($plan, 'Attean::Plan::HashJoin');
		ok($plan->distinct);
	};

	subtest 'Distinct 2-triple BGP with join variable, no blank nodes' => sub {
		note("A 2-triple BGP with a join variable without any blank nodes is necessarily distinct, so a distinct operation should be a no-op, resulting in just a nested loop join");
		my $bgp		= Attean::Algebra::BGP->new(triples => [$t, $u]);
		my $dist	= Attean::Algebra::Distinct->new( children => [$bgp] );
		my $plan	= $p->plan_for_algebra($dist, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan', 'Distinct 2-triple BGP without blanks');
		isa_ok($plan, 'Attean::Plan::HashJoin');
		ok($plan->distinct);
	};

	subtest 'Distinct 3-triple BGP with join variable and blank nodes' => sub {
		note("A 3-triple BGP with a blank node isn't necessarily distinct, so a distinct operation should result in a HashDistinct plan");
		my $bgp		= Attean::Algebra::BGP->new(triples => [$t, $u, $v]);
		my $dist	= Attean::Algebra::Distinct->new( children => [$bgp] );
		my $plan	= $p->plan_for_algebra($dist, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan', 'Distinct 3-triple BGP with blanks');
		isa_ok($plan, 'Attean::Plan::HashDistinct');
		ok($plan->distinct);
	};
	
	subtest 'Sorted 1-triple BGP' => sub {
		note("A 1-triple BGP with ASC(?s) sorting should result in a Project(Order(Extend(Quad(....)))) pattern");
		my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
		my $sorted	= order_algebra_by_variables($bgp, 's');
		my $plan	= $p->plan_for_algebra($sorted, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan', 'Sorted 1-triple BGP'); # Sorting introduces a 
		isa_ok($plan, 'Attean::Plan::Project');
		ok($plan->distinct, 'Plan is distinct');
		
		my $order	= $plan->ordered;
		is(scalar(@$order), 1, 'Count of ordering comparators');
		my $cmp	= $order->[0];
		ok($cmp->ascending, 'Ordering is ascending');
		my $expr	= $cmp->expression;
		isa_ok($expr, 'Attean::ValueExpression');
		is($expr->value->value, 's');
	};
	
	subtest 'Join planning is equivalent to BGP planning' => sub {
		# A join between two 1-triple BGPs should result in the same plan as the equivalent 2-triple BGP
		my $plan1		= $p->plan_for_algebra(Attean::Algebra::BGP->new(triples => [$t, $u]), $model, [$graph]);
		my $bgp1		= Attean::Algebra::BGP->new(triples => [$t]);
		my $bgp2		= Attean::Algebra::BGP->new(triples => [$u]);
		my $join		= Attean::Algebra::Join->new(children => [$bgp1, $bgp2]);
		my $plan2		= $p->plan_for_algebra($join, $model, [$graph]);
		
		does_ok($_, 'Attean::API::Plan') for ($plan1, $plan2);
		isa_ok($_, 'Attean::Plan::HashJoin') for ($plan1, $plan2);
		
		# we don't do a single deep comparison on the plans here, because while they are equivalent plans,
		# BGP planning handles the annotating of the distinct flag on sub-plans differently than the
		# general join planning.
		foreach my $pos (0,1) {
			does_ok($_->children->[$pos], 'Attean::API::Plan') for ($plan1, $plan2);
			isa_ok($_->children->[$pos], 'Attean::Plan::Quad') for ($plan1, $plan2);
			is_deeply([$plan1->children->[$pos]->values], [$plan2->children->[$pos]->values]);
		}
	};
}


sub order_algebra_by_variables {
	my $algebra	= shift;
	my @vars	= @_;
	my @cmps;
	foreach my $var (@vars) {
		my $expr	= Attean::ValueExpression->new(value => variable($var));
		my $cmp		= Attean::Algebra::Comparator->new(ascending => 1, expression => $expr);
		push(@cmps, $cmp);
	}
	my $sorted	= Attean::Algebra::OrderBy->new( children => [$algebra], comparators => \@cmps );
	return $sorted;
}



# 
# {
# 	my $t	= triple(variable('s'), iri('p'), literal('1'));
# 	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
# 	my $join	= Attean::Algebra::Join->new( children => [$bgp, $bgp] );
# 	my @walk;
# 	$join->walk(prefix => sub { push(@walk, shift) });
# 	is(scalar(@walk), 3, 'expected walk count');
# 	
# 	my @cover;
# 	$join->cover(prefix => sub { push(@cover, shift) });
# 	is(scalar(@cover), 2, 'expected cover count');
# }
# 
# {
# 	my $p1	= iri('p1');
# 	my $pp1	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
# 	ok($pp1->does('Attean::API::PropertyPath'), 'PredicatePath consumes PropertyPath');
# 	is($pp1->as_string, '<p1>', 'PredicatePath as_string');
# 	
# 	my $p2	= iri('p2');
# 	my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );
# 
# 	my $nps	= Attean::Algebra::NegatedPropertySet->new( predicates => [$p1, $p2] );
# 	ok($nps->does('Attean::API::PropertyPath'), 'NegatedPropertySet consumes PropertyPath');
# 	is($nps->as_string, '!(<p1>|<p2>)', 'NegatedPropertySet as_string');
# 	
# 	my $seq1	= Attean::Algebra::SequencePath->new( children => [$pp2] );
# 	is($seq1->as_string, '<p2>', 'unary SequencePath as_string');
# 
# 	my $seq	= Attean::Algebra::SequencePath->new( children => [$pp1, $pp2] );
# 	is($seq->as_string, '(<p1>/<p2>)', 'SequencePath as_string');
# 
# 	my $alt1	= Attean::Algebra::AlternativePath->new( children => [$pp2] );
# 	is($alt1->as_string, '<p2>', 'unary AlternativePath as_string');
# 
# 	my $alt	= Attean::Algebra::AlternativePath->new( children => [$pp1, $pp2] );
# 	is($alt->as_string, '(<p1>|<p2>)', 'AlternativePath as_string');
# 
# 	my $inv1	= Attean::Algebra::InversePath->new( children => [$pp2] );
# 	is($inv1->as_string, '^<p2>', 'InversePath as_string');
# 	
# 	my $inv_seq	= Attean::Algebra::InversePath->new( children => [$seq] );
# 	is($inv_seq->as_string, '^(<p1>/<p2>)', 'complex InversePath as_string');
# 	
# 	my $inv_seq_star	= Attean::Algebra::ZeroOrMorePath->new( children => [$inv_seq] );
# 	is($inv_seq_star->as_string, '(^(<p1>/<p2>))*', 'complex ZeroOrMorePath as_string');
# }
# 
# {
# 	note('BGP canonicalization');
# 	my $b		= blank('person');
# 	my $rdf_type	= iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
# 	my $foaf_name	= iri('http://xmlns.com/foaf/0.1/name');
# 	my $foaf_knows	= iri('http://xmlns.com/foaf/0.1/knows');
# 	my $foaf_Person	= iri('http://xmlns.com/foaf/0.1/Person');
# 	my $bgp1	= Attean::Algebra::BGP->new( triples => [
# 		triplepattern($b, $rdf_type, $foaf_Person),
# 		triplepattern($b, $foaf_name, variable('name')),
# 		triplepattern($b, $foaf_knows, variable('knows')),
# 	] );
# 	my $bgp2	= Attean::Algebra::BGP->new( triples => [
# 		triplepattern(blank('s'), $foaf_knows, variable('person')),
# 		triplepattern(blank('s'), $rdf_type, $foaf_Person),
# 		triplepattern(blank('s'), $foaf_name, variable('myname')),
# 	] );
# 
# 	my $hash1	= sha1_hex( join("\n", map { $_->tuples_string } (@{$bgp1->triples}) ) );
# 	my $hash2	= sha1_hex( join("\n", map { $_->tuples_string } (@{$bgp2->triples}) ) );
# 	isnt($hash1, $hash2, 'non-matching pre-canonicalized BGP hashes');
# 	
# 	my ($cbgp1, $m1)	= $bgp1->canonical_bgp_with_mapping;
# 	my ($cbgp2, $m2)	= $bgp2->canonical_bgp_with_mapping;
# 	
# 	my $chash1	= sha1_hex( join("\n", map { $_->tuples_string } (@{$cbgp1->triples}) ) );
# 	my $chash2	= sha1_hex( join("\n", map { $_->tuples_string } (@{$cbgp2->triples}) ) );
# 	is($chash1, $chash2, 'matching canonicalized BGP hashes' );
# 	
# 	is_deeply($m1, { '?name' => { 'prefix' => '?', 'id' => 'v003', 'type' => 'variable' }, '?knows' => { 'id' => 'v002', 'prefix' => '?', 'type' => 'variable' }, '_:person' => { 'id' => 'v001', 'prefix' => '_:', 'type' => 'blank' } }, 'BGP1 mapping');
# 	is_deeply($m2, { '?person' => { 'prefix' => '?', 'id' => 'v002', 'type' => 'variable' }, '_:s' => { 'prefix' => '_:', 'id' => 'v001', 'type' => 'blank' }, '?myname' => { 'type' => 'variable', 'id' => 'v003', 'prefix' => '?' } }, 'BGP2 mapping');
# }

done_testing();


sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}


package TestStore {
	use Moo;
	extends 'AtteanX::Store::Memory';
	sub cost_for_plan {
		# we do this because the superclass would return a cost of 0 for quads when the store is empty
		# and if 0 was returned, there won't be any meaningful difference between the cost of different join algorithms 
		return 2;
	}
}
