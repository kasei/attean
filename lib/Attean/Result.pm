use v5.14;
use warnings;

package Attean::Result 0.001 {
	use Moose;
	use Attean::API::Binding;
	
	with 'Attean::API::Result';
	has 'bindings' => (is => 'ro', isa => 'HashRef[Attean::API::Term]');
	
	sub value {
		my $self	= shift;
		my $k		= shift;
		return $self->bindings->{$k};
	}
	
	sub variables {
		my $self	= shift;
		return keys %{ $self->bindings };
	}
}

1;
