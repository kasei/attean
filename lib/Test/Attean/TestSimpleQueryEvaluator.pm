use v5.14;
use warnings;

use Attean::Algebra;
use Attean::Expression;
use Attean::SimpleQueryEvaluator;

package Test::Attean::MockClient 0.033 {
	use Moo;
	use Data::Dumper;
	use Types::Standard qw(ConsumerOf InstanceOf Bool Object HashRef);
	use Encode qw(encode encode_utf8 decode_utf8);
	use namespace::clean;

	has 'endpoint' => (is => 'rw');
	has 'mocked_endpoints' => (is => 'rw', isa => HashRef, default => sub { +{} });
	has 'default_graph'	=> (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], required => 1);
	has 'model' => (is => 'ro', isa => ConsumerOf['Attean::API::Model'], required => 1);
	has 'silent' => (is => 'ro', isa => Bool, default => 0);
	
	sub query {
		my $self		= shift;
		my $sparql		= shift;
		my $model		= $self->model;
		my $silent		= $self->silent;
		my $bytes		= encode_utf8($sparql);
		my $s 			= AtteanX::Parser::SPARQL->new();
		my ($algebra)	= $s->parse_list_from_bytes($bytes);
		my $e			= Test::Attean::TestSimpleQueryEvaluator->new( model => $model, default_graph => $self->default_graph, endpoints => $self->mocked_endpoints );
		my $results		= eval { $e->evaluate($algebra, $self->default_graph) };
		if ($@ and not $silent) {
			die $@;
		}
		return $results;
	}
}

package Test::Attean::TestSimpleQueryEvaluator 0.033 {
	use Moo;
	use Types::Standard qw(HashRef);

	extends 'Attean::SimpleQueryEvaluator';
	has 'endpoints' => (is => 'rw', isa => HashRef, default => sub { +{} });
	
	sub register_test_endpoint {
		my $self		= shift;
		my $endpoint	= shift;
		my $data		= shift;
		$self->endpoints->{$endpoint}	= $data;
	}
	
	sub new_service_client {
		my $self		= shift;
		my $endpoint	= shift;
		my $silent		= shift;
		if (my $data = $self->endpoints->{$endpoint}) {
			my ($model, $dg)	= @$data;
			return Test::Attean::MockClient->new(endpoint => $endpoint, model => $model, default_graph => $dg, silent => $silent, mocked_endpoints => $self->endpoints);
		}
		return $self->SUPER::new_service_client($endpoint, $silent, @_);
	}
}

1;

