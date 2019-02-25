use Test::Modern;
use Test::Roo;
use Attean;

use v5.14;

sub create_store {
  my $self = shift;
  my %args = @_;
  my $store	= Attean->get_store('Memory')->new();
  $store->add_quad($args{quads}[0]);
  return $store;
}

with 'Test::Attean::MutableTransactionalQuadStore';

run_me;
done_testing;
