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
my $t		= triplepattern(variable('s'), iri('p'), literal('1'));
my $u		= triplepattern(variable('s'), iri('p'), variable('o'));
my $v		= triplepattern(variable('s'), iri('q'), blank('xyz'));
my $w		= triplepattern(variable('a'), iri('b'), iri('c'));
my $x		= triplepattern(variable('a'), variable('b'), iri('c'));

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

subtest 'Construct' => sub {
	my $t	= Attean::Plan::Quad->new( subject => variable('s'), predicate => iri('p'), object => variable('o'), graph => iri('g'), distinct => 1, ordered => []);
	my $u	= triplepattern(variable('s'), iri('q'), variable('o'));
	my $c	= Attean::Plan::Construct->new(triples => [$u], children => [$t], distinct => 0, ordered => []);
	like($c->as_string, qr/Construct/s);
	like($c->as_string, qr/Quad.*[?]s[, ]*<p>[, ]*[?]o/, 'construct pattern');
	like($c->as_string, qr/[?]s <q> [?]o/, 'construct template');
};

done_testing();
