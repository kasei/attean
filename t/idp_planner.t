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

package TestStore {
	use Moo;
	use namespace::clean;
	extends 'AtteanX::Store::Memory';
	
	sub cost_for_plan {
		# we do this because the superclass would return a cost of 0 for quads when the store is empty
		# and if 0 was returned, there won't be any meaningful difference between the cost of different join algorithms 
		my $self	= shift;
		my $plan	= shift;
		if ($plan->isa('Attean::Plan::Quad')) {
			return 3;
		}
		return;
	}
}


# Attean::Plan::Quad
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
	my $u		= triple(variable('s'), iri('p'), variable('o'));
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
		note("A join between two 1-triple BGPs should result in the same plan as the equivalent 2-triple BGP");
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
	
	subtest 'Variable Filter' => sub {
		note("FILTER(?o) should result in a EBVFilter(...) pattern");
		my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
		my $expr	= Attean::ValueExpression->new(value => variable('o'));
		my $filter	= Attean::Algebra::Filter->new(children => [$bgp], expression => $expr);
		my $plan	= $p->plan_for_algebra($filter, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan', 'Variable filter');
		isa_ok($plan, 'Attean::Plan::EBVFilter');
		is($plan->variable, 'o');
	};
	
	subtest 'Expression Filter' => sub {
		note("FILTER(?s && ?o) should result in a Project(EBVFilter(Extend(...))) pattern");
		my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
		my $expr1	= Attean::ValueExpression->new(value => variable('s'));
		my $expr2	= Attean::ValueExpression->new(value => variable('o'));
		my $expr	= Attean::BinaryExpression->new( operator => '&&', children => [$expr1, $expr2] );
		my $filter	= Attean::Algebra::Filter->new(children => [$bgp], expression => $expr);
		my $plan	= $p->plan_for_algebra($filter, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan', 'Expression filter');
		isa_ok($plan, 'Attean::Plan::Project');
		isa_ok($plan->children->[0], 'Attean::Plan::EBVFilter');
		isa_ok($plan->children->[0]->children->[0], 'Attean::Plan::Extend');
	};
	
	subtest 'IRI named graph' => sub {
		note("1-triple BGP restricted to an IRI-named graph should result in a Quad plan");
		my $ng		= iri('http://eample.org/named/');
		my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
		my $named	= Attean::Algebra::Graph->new(children => [$bgp], graph => $ng);
		my $plan	= $p->plan_for_algebra($named, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan', 'IRI-named graph');
		isa_ok($plan, 'Attean::Plan::Quad');
	};
	
	subtest 'Variable named graph (model with 0 named graphs)' => sub {
		note("1-triple BGP restricted to a variable-named graph should result in an empty Union plan");
		my $ng		= variable('g');
		my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
		my $named	= Attean::Algebra::Graph->new(children => [$bgp], graph => $ng);
		my $plan	= $p->plan_for_algebra($named, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan', 'IRI-named graph');
		isa_ok($plan, 'Attean::Plan::Union');
		is(scalar(@{ $plan->children }), 0);
	};
}

done_testing();



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

sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}


