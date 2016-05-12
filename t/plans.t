=pod

=encoding utf-8

=head1 PURPOSE

Tests for various plans

=cut

use v5.14;
use autodie;
use utf8;
use feature "state";
use Test::Modern;
use Test::Exception;
use Digest::SHA qw(sha1_hex);

use Attean;
use Attean::RDF;

use Attean::Plan::Iterator;

my $ci = Attean::CodeIterator->new(
	generator => sub {
		state $i = 0;
		return undef if ($i > 2);
		return Attean::Result->new(bindings => { 'o' => literal($i++) });
	},
	item_type => 'Attean::API::Result',
	variables => ['o']
);

isa_ok($ci, 'Attean::CodeIterator');

my @values = map { Attean::Result->new(bindings => { 'o' => literal($_) }) } (1,2,3);
my $li = Attean::ListIterator->new(
	values => \@values,
	item_type => 'Attean::API::Result',
	variables => ['o']
);

isa_ok($li, 'Attean::ListIterator');

#subtest 'CodeIterator without size' => sub
{
	my $plan = Attean::Plan::Iterator->new(
		variables => [variable('o')],
		iterator => $ci,
		distinct => 0,
		ordered => []
	);
	isa_ok($plan, 'Attean::Plan::Iterator');
	does_ok($plan, 'Attean::API::Plan');
	can_ok($plan, 'iterator');
	ok(! $plan->has_size_estimate, 'Has no size estimate');
	is($plan->as_string, "- Iterator (?o)\n", 'Correct serialization');
};

#subtest 'CodeIterator with size' => sub
{
	my $plan = Attean::Plan::Iterator->new(
		variables => [variable('o')],
		iterator => $ci,
		distinct => 0,
		size_estimate => 2,
		ordered => []
	);
	isa_ok($plan, 'Attean::Plan::Iterator');
	does_ok($plan, 'Attean::API::Plan');
	can_ok($plan, 'iterator');
	ok($plan->has_size_estimate, 'Has size estimate');
	is($plan->size_estimate, 2, 'Correct returned estimate');
	is($plan->as_string, "- Iterator (?o with 2 elements)\n", 'Correct serialization');
};

{
	my $plan = Attean::Plan::Iterator->new(
		variables => [variable('o')],
		iterator => $li,
		distinct => 0,
		ordered => []
	);
	isa_ok($plan, 'Attean::Plan::Iterator');
	does_ok($plan, 'Attean::API::Plan');
	can_ok($plan, 'iterator');
	is($plan->size_estimate, 3, 'Correct returned estimate');
	ok($plan->has_size_estimate, 'Has size estimate for ListIterator');
	is($plan->as_string, "- Iterator (?o with 3 elements)\n", 'Correct serialization');
};

{
	my $plan = Attean::Plan::Iterator->new(
		variables => [variable('o')],
		iterator => $li,
		distinct => 0,
		size_estimate => 4,
		ordered => []
	);
	isa_ok($plan, 'Attean::Plan::Iterator');
	does_ok($plan, 'Attean::API::Plan');
	can_ok($plan, 'iterator');
	ok($plan->has_size_estimate, 'Has size estimate for ListIterator');
	is($plan->size_estimate, 4, 'Correct returned estimate when overriding');
	is($plan->as_string, "- Iterator (?o with 4 elements)\n", 'Correct serialization');
};

{
	my $li = Attean::ListIterator->new(
		values => \@values,
		item_type => 'Attean::API::Result',
		variables => ['o']
	);
	$li->next;
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	my $plan = Attean::Plan::Iterator->new(
		variables => [variable('o')],
		iterator => $li,
		distinct => 0,
		ordered => []
	);
	
	my $c	= $plan->impl($model);
	isa_ok($c, 'CODE');
	my $i	= $c->();
	does_ok($i, 'Attean::API::Iterator');
	my @r	= $i->elements;
	is(scalar(@r), 3);
};

done_testing;
