use v5.14;
use warnings;

use Attean::Algebra;
use Attean::Expression;
use Attean::IDPQueryPlanner;

package Test::Attean::Plan::TestService 0.033 {
	use Moo;
	use Types::Standard qw(ConsumerOf Bool Str InstanceOf);
	use Encode qw(encode encode_utf8 decode_utf8);
	use Scalar::Util qw(blessed);
	use URI::Escape;
	use Attean::SPARQLClient;
	use namespace::clean;

	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';

	has 'plan' => (is => 'rw');
	has 'model' => (is => 'rw');
	has 'silent' => (is => 'ro', isa => Bool, default => 0);

	sub plan_as_string {
		my $self	= shift;
		my $s		= $self->plan->as_string;
		$s			=~ s/\n/ /g;
		return sprintf('TestService { ' . $s . ' }');
	}
	
	sub tree_attributes { return qw() };
	sub impl {
		my $self	= shift;
		shift; # model
		my $model	= $self->model;
		my $plan	= $self->plan;
		my $result	= eval { $plan->impl($model, @_) };
		if ($@ and not $self->silent) {
			die $@;
		}
		return $result;
	}
}


package Test::Attean::TestIDPQueryPlanner 0.033 {
	use Moo;
	use Data::Dumper;
	use Types::Standard qw(HashRef);
	use namespace::clean;
	
	extends 'Attean::IDPQueryPlanner';
	has 'endpoints' => (is => 'rw', isa => HashRef, default => sub { +{} });
	
	sub register_test_endpoint {
		my $self		= shift;
		my $endpoint	= shift;
		my $data		= shift;
		$self->endpoints->{$endpoint}	= $data;
	}
	
	sub plans_for_algebra {
		my $self			= shift;
		my $algebra			= shift;
		my $model			= shift;
		my $active_graphs	= shift;
		my $default_graphs	= shift;
		my %args			= @_;
		my @children	= @{ $algebra->children };
		my ($child)		= $children[0];

		if ($algebra->isa('Attean::Algebra::Service')) {
			my $endpoint	= $algebra->endpoint->value;
# 			warn "Looking for endpoint $endpoint ...\n";
			if (my $data = $self->endpoints->{$endpoint}) {
# 				warn "... found.\n";
				my ($mock_model, $dg)	= @$data;
				my @plans	= $self->plans_for_algebra($child, $mock_model, $active_graphs, $default_graphs, %args);
				my @vars		= $child->in_scope_variables;
				my $silent		= $algebra->silent;
				my $sparql		= sprintf('SELECT * WHERE { %s }', $child->as_sparql);
				my $plan		= Test::Attean::Plan::TestService->new(
					in_scope_variables => \@vars,
					plan	=> shift(@plans),
					model	=> $mock_model,
					silent	=> $silent,
				);
				return $plan;
			} else {
# 				warn "... not found.\n";
			}
		}
		
		return $self->SUPER::plans_for_algebra($algebra, $model, $active_graphs, $default_graphs, %args);
	}
}

1;

