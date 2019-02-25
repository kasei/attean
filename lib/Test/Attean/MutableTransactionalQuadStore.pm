package Test::Attean::MutableTransactionalQuadStore;

use v5.14;
use warnings;
use Test::Roo::Role;
use Attean;
use Attean::RDF;
use Proc::Fork;
use Test::Modern;


requires 'create_store';      # create_store( quads => \@quads )
with 'Test::Attean::StoreCleanup';

test 'mutabletransactionalquadstore sequential' => sub {
  my $self	= shift;
  my $q1		= quad(iri('s'), iri('p'), dtliteral('35', iri('http://www.w3.org/2001/XMLSchema#integer')), iri('g'));
  my $store	= $self->create_store(quads => [$q1]);
  my $model = Attean::MutableQuadModel->new(store => $store);
  my $objects = $model->objects(iri('s'), iri('p'), iri('g'));
  does_ok($objects, 'Attean::API::Iterator');
  my $count1 = $objects->next->value;
  is($count1, 35, 'Correct initial count');
  $model->remove_quad($q1);
  is($model->size, 0);
  my $cout1 = $count1 + 100;
  my $q2 = quad(iri('s'), iri('p'), dtliteral($cout1, iri('http://www.w3.org/2001/XMLSchema#integer')), iri('g'));
  $model->add_quad($q2);
  is($model->size, 1);

  # Now next to come around
  my $object = $model->objects(iri('s'), iri('p'), iri('g'))->next;
  does_ok($object, 'Attean::API::Term');
  my $count2 = $object->value;
  is($count2, $cout1, 'Correct count read back');
  my $cout2 = $cout1 - 30;
  $model->remove_quad($q2);
  my $q3 = quad(iri('s'), iri('p'), dtliteral($cout2, iri('http://www.w3.org/2001/XMLSchema#integer')), iri('g'));
  $model->add_quad($q3);
  is($model->size, 1);
  is($model->objects(iri('s'), iri('p'), iri('g'))->next->value, $cout2, 'Correct count read back');


  $self->cleanup_store($store);
};


test 'mutabletransactionalquadstore lost update' => sub {
  my $self	= shift;
  my $q1		= quad(iri('s'), iri('p'), dtliteral('35', iri('http://www.w3.org/2001/XMLSchema#integer')), iri('g'));
  my $store	= $self->create_store(quads => [$q1]);
  my $model = Attean::MutableQuadModel->new(store => $store);

  run_fork {
	 child {
		my $objects = $model->objects(iri('s'), iri('p'), iri('g'));
		does_ok($objects, 'Attean::API::Iterator');
		my $count1 = $objects->next->value;
		is($count1, 35, 'Correct initial count');
		$model->remove_quad($q1);
		is($model->size, 0);
		my $cout1 = $count1 + 100;
		my $q2 = quad(iri('s'), iri('p'), dtliteral($cout1, iri('http://www.w3.org/2001/XMLSchema#integer')), iri('g'));
		$model->add_quad($q2);
		is($model->size, 1);
	 };
	 child {
		# Now next to come around
		my $object = $model->objects(iri('s'), iri('p'), iri('g'))->next;
		does_ok($object, 'Attean::API::Term');
		my $count2 = $object->value;
		my $cout1 = 135;
		is($count2, $cout1, 'Correct count read back');
		my $cout2 = $cout1 - 30;
		my $q2 = quad(iri('s'), iri('p'), dtliteral($cout1, iri('http://www.w3.org/2001/XMLSchema#integer')), iri('g'));
		$model->remove_quad($q2);
		my $q3 = quad(iri('s'), iri('p'), dtliteral($cout2, iri('http://www.w3.org/2001/XMLSchema#integer')), iri('g'));
		$model->add_quad($q3);
		is($model->size, 1);
		is($model->objects(iri('s'), iri('p'), iri('g'))->next->value, $cout2, 'Correct count read back');
	 }
  };
		
  $self->cleanup_store($store);
};

1;
