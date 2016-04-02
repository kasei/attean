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
												  return Attean::Result->new(bindings => { 'o' => literal($i) });
											  },
											  item_type => 'Attean::API::Result',
											  variables => ['o']
											 );

isa_ok($ci, 'Attean::CodeIterator');

#subtest 'CodeIterator without size' => sub 
{
	my $plan = Attean::Plan::Iterator->new( variables => [variable('o')],
														 iterator => $ci,
														 distinct => 0,
														 ordered => [] );
	isa_ok($plan, 'Attean::Plan::Iterator');
	does_ok($plan, 'Attean::API::Plan');
	can_ok($plan, 'iterator');
	ok(! $plan->has_size_estimate, 'Has no size estimate');
	is($plan->as_string, "- Iterator (?o)\n", 'Correct serialization');
};


done_testing;
