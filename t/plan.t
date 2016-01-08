use v5.14;
use autodie;
use utf8;
use Test::Modern;
use Test::Exception;
use Digest::SHA qw(sha1_hex);

use Attean;
use Attean::RDF;
use AtteanX::Store::Memory;

package TestPlanner {
	use Moo;
	extends 'Attean::QueryPlanner';
	with 'Attean::API::NaiveJoinPlanner';
}

my $p	= TestPlanner->new();

my $store	= AtteanX::Store::Memory->new();
my $model	= Attean::MutableQuadModel->new( store => $store );

my $graph	= iri('http://example.org/');
my $t		= triple(variable('s'), iri('p'), literal('1'));
my $u		= triple(variable('s'), iri('p'), variable('o'));
my $v		= triple(variable('s'), iri('q'), blank('xyz'));
my $w		= triple(variable('a'), iri('b'), iri('c'));
my $x		= triple(variable('a'), variable('b'), iri('c'));

sub test_triples_for_connected_plan {
	my $triples		= shift;
	my $connected	= shift;
	my $note		= shift;
	my $bgp			= Attean::Algebra::BGP->new(triples => $triples);
	my $plan		= $p->plan_for_algebra($bgp, $model, [$graph]);
	my $ok			= $plan->subplans_of_type_are_variable_connected('Attean::Plan::Quad');
	$ok				= not($ok) unless ($connected);
	ok($ok, $note);
}


test_triples_for_connected_plan([], 1, 'Empty BGP');
test_triples_for_connected_plan([$t], 1, '1-triple BGP');
test_triples_for_connected_plan([$t, $u], 1, '2-triple BGP');
test_triples_for_connected_plan([$w, $x], 1, '2-triple BGP');
test_triples_for_connected_plan([$t, $u, $v], 1, '3-triple BGP');
test_triples_for_connected_plan([$t, $u, $v, $w], 0, '4-triple BGP');
test_triples_for_connected_plan([$x, $t, $u, $v, $w], 0, '5-triple BGP');

done_testing();
