use v5.14;
use Test::Modern;
use Attean;
use Attean::RDF;
use Attean::IDPQueryPlanner;


###############################################################################

package MyBGP {
	use Moo;
	use Scalar::Util qw(blessed reftype);
	use Types::Standard qw(ConsumerOf ArrayRef);
	use namespace::clean;
	with 'Attean::API::NullaryQueryTree', 'Attean::API::UnionScopeVariablesPlan';

	sub plan_as_string { return 'BGP' }
	sub impl { die "Unimplemented" }
}

package MyPlanner {
	use Moo;
	use namespace::clean;
	extends 'Attean::QueryPlanner';
	with 'Attean::API::NaiveJoinPlanner';
	with 'Attean::API::SimpleCostPlanner';
	with 'AtteanX::API::JoinRotatingPlanner';
	
	sub allow_join_rotation {
		my $self	= shift;
		my $join	= shift;
		# Inspect $join to conditionally allow/disallow join rotation
		return 1;
	}
	
	sub coalesce_rotated_join {
		my $self	= shift;
		my $join	= shift;
		my ($lhs, $rhs)	= @{ $join->children };
		if ($lhs->isa('Attean::Plan::Quad') and $rhs->isa('Attean::Plan::Quad')) {
			return MyBGP->new(children => [$lhs, $rhs], distinct => 0);
		} elsif ($lhs->isa('MyBGP') and $rhs->isa('Attean::Plan::Quad')) {
			my @quads	= (@{ $lhs->children }, $rhs);
			return MyBGP->new(children => \@quads, distinct => 0);
		} elsif ($rhs->isa('MyBGP') and $lhs->isa('Attean::Plan::Quad')) {
			my @quads	= ($lhs, @{ $rhs->children });
			return MyBGP->new(children => \@quads, distinct => 0);
		} elsif ($rhs->isa('MyBGP') and $lhs->isa('MyBGP')) {
			my @quads	= (@{ $lhs->children }, @{ $rhs->children });
			return MyBGP->new(children => \@quads, distinct => 0);
		}
		return $join;
	}
	
	around 'cost_for_plan' => sub {
		my $orig	= shift;
		my $self	= shift;
		my $plan	= shift;
		if ($plan->isa('MyBGP')) {
			# Force MyBGP objects to cost less than an equivalent join over Quad plans.
			return 1;
		}
		return $orig->($self, $plan, @_);
	}
}

package MyPlanner1 {
	# this planner uses the default allow_join_rotation()
	use Moo;
	use namespace::clean;
	extends 'Attean::QueryPlanner';
	with 'Attean::API::NaiveJoinPlanner';
	with 'Attean::API::SimpleCostPlanner';
	with 'AtteanX::API::JoinRotatingPlanner';
	
	sub coalesce_rotated_join {
		my $self	= shift;
		my $join	= shift;
		my ($lhs, $rhs)	= @{ $join->children };
		if ($lhs->isa('Attean::Plan::Quad') and $rhs->isa('Attean::Plan::Quad')) {
			return MyBGP->new(children => [$lhs, $rhs], distinct => 0);
		} elsif ($lhs->isa('MyBGP') and $rhs->isa('Attean::Plan::Quad')) {
			my @quads	= (@{ $lhs->children }, $rhs);
			return MyBGP->new(children => \@quads, distinct => 0);
		} elsif ($rhs->isa('MyBGP') and $lhs->isa('Attean::Plan::Quad')) {
			my @quads	= ($lhs, @{ $rhs->children });
			return MyBGP->new(children => \@quads, distinct => 0);
		} elsif ($rhs->isa('MyBGP') and $lhs->isa('MyBGP')) {
			my @quads	= (@{ $lhs->children }, @{ $rhs->children });
			return MyBGP->new(children => \@quads, distinct => 0);
		}
		return $join;
	}
	
	
	around 'cost_for_plan' => sub {
		my $orig	= shift;
		my $self	= shift;
		my $plan	= shift;
		if ($plan->isa('MyBGP')) {
			# Force MyBGP objects to cost less than an equivalent join over Quad plans.
			return 1;
		}
		return $orig->($self, $plan, @_);
	}
}

package MyPlanner2 {
	# this planner uses the default coalesce_rotated_join()
	use Moo;
	use namespace::clean;
	extends 'Attean::QueryPlanner';
	with 'Attean::API::NaiveJoinPlanner';
	with 'Attean::API::SimpleCostPlanner';
	with 'AtteanX::API::JoinRotatingPlanner';
	
	sub allow_join_rotation {
		my $self	= shift;
		my $join	= shift;
		# Inspect $join to conditionally allow/disallow join rotation
		return 1;
	}
	
	around 'cost_for_plan' => sub {
		my $orig	= shift;
		my $self	= shift;
		my $plan	= shift;
		if ($plan->isa('MyBGP')) {
			# Force MyBGP objects to cost less than an equivalent join over Quad plans.
			return 1;
		}
		return $orig->($self, $plan, @_);
	}
}

package MyTestStore {
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

###############################################################################

{
	my $store	= MyTestStore->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $graph	= iri('http://example.org/');
# 	my $t		= triplepattern(variable('s'), iri('p'), literal('1'));
	my $t		= triplepattern(variable('s'), iri('p'), variable('o'));
	my $v		= triplepattern(variable('s'), iri('q'), literal('xyz'));
	my $w		= triplepattern(variable('o'), iri('b'), iri('c'));

	my $bgp1	= Attean::Algebra::BGP->new(triples => [$t]);
	my $bgp2	= Attean::Algebra::BGP->new(triples => [$w]);
	my $service	= Attean::Algebra::Service->new(children => [$bgp2], endpoint => iri('http://endpoint.example.org/sparql'));
	my $bgp3	= Attean::Algebra::BGP->new(triples => [$v]);
	my $join1	= Attean::Algebra::Join->new(children => [$bgp1, $service]);

	# (t ⋈ Service(w)) ⋈ v
	my $join2	= Attean::Algebra::Join->new(children => [$join1, $bgp3]);

	subtest 'before BGP merging' => sub {
		# This tests the various possible plans that can be produced for this
		# algebra, allowing for join commutativity. Without join rotation or
		# coalescing, the resulting plan should have a top-level join, with
		# children being a quad, and another join of a quad and a service.
		# 
		# A possible plan for this algebra:
		# - Hash Join { s }
		# -   Quad { ?s, <q>, "xyz", <http://example.org/> } (distinct)
		# -   Hash Join { o }
		# -     Service <http://endpoint.example.org/sparql> SELECT * WHERE { { ?o <b> <c> . } }
		# -     Quad { ?s, <p>, ?o, <http://example.org/> } (distinct)

		my $p		= Attean::IDPQueryPlanner->new();
		my $plan	= $p->plan_for_algebra($join2, $model, [$graph]);
# 		warn $plan->as_string;

		does_ok($plan, 'Attean::API::Plan::Join');
		my ($lhs, $rhs)	= @{ $plan->children };
		my $join;
		if ($lhs->does('Attean::API::Plan::Join')) {
			does_ok($lhs, 'Attean::API::Plan::Join');
			isa_ok($rhs, 'Attean::Plan::Quad');
			$join	= $lhs;
		} else {
			does_ok($rhs, 'Attean::API::Plan::Join');
			isa_ok($lhs, 'Attean::Plan::Quad');
			$join	= $rhs;
		}
		
		my ($join_lhs, $join_rhs)	= @{ $join->children };
		if ($join_lhs->isa('Attean::Plan::Quad')) {
			isa_ok($join_lhs, 'Attean::Plan::Quad');
			isa_ok($join_rhs, 'Attean::Plan::Service');
		} else {
			isa_ok($join_rhs, 'Attean::Plan::Quad');
			isa_ok($join_lhs, 'Attean::Plan::Service');
		}
	};

	foreach my $planner_class (qw(MyPlanner MyPlanner1)) {
		subtest "after BGP merging ($planner_class)" => sub {
			# This test is similar, but requires that the resulting plan has
			# undergone join rotation and quad coalescing, and that the lowest
			# cost plan will be a join with children being a service and a BGP.
			# 
			# A possible plan for this algebra:
			# - NestedLoop Join
			# -   Service <http://endpoint.example.org/sparql> SELECT * WHERE { { ?o <b> <c> . } }
			# -   BGP
			# -     Quad { ?s, <p>, ?o, <http://example.org/> } (distinct)
			# -     Quad { ?s, <q>, "xyz", <http://example.org/> } (distinct)


			# (t ⋈ Service(w)) ⋈ v
			# should yield one of the following after rewriting:
			# - BGP(tv) ⋈ Service(w)
			# - Service(w) ⋈ BGP(tv)
			my $p		= $planner_class->new();
			my $plan	= $p->plan_for_algebra($join2, $model, [$graph]);
	# 		warn $plan->as_string;

			does_ok($plan, 'Attean::API::Plan::Join');
			my ($lhs, $rhs)	= @{ $plan->children };
			if ($lhs->isa('MyBGP')) {
				isa_ok($lhs, 'MyBGP');
				isa_ok($rhs, 'Attean::Plan::Service');
			} else {
				isa_ok($rhs, 'MyBGP');
				isa_ok($lhs, 'Attean::Plan::Service');
			}
		};
	}

	subtest "after BGP merging (MyPlanner2)" => sub {
		my $p		= MyPlanner2->new();
		my $plan	= $p->plan_for_algebra($join2, $model, [$graph]);
		does_ok($plan, 'Attean::API::Plan::Join');
	};
}

done_testing();
