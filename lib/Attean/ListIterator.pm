use v5.14;
use warnings;

package Attean::ListIterator 0.001 {
	use Moose;
	use Moose::Util::TypeConstraints;
	
	with 'Attean::API::RepeatableIterator';
	
	has values => (is => 'ro', isa => 'ArrayRef', required => 1);
	has current => (is => 'rw', isa => 'Int', init_arg => undef, default => 0);
	
	sub BUILD {
		my $self	 = shift;
		my $constraint	= $self->item_type;
		foreach my $item (@{ $self->values }) {
			$constraint->assert_valid($item);
		}
	}
	
	sub reset {
		my $self	= shift;
		$self->current(0);
	}
	
	sub next {
		my $self	= shift;
		my $list	= $self->values;
		my $index	= $self->current;
		my $item	= $list->[$index];
		return unless defined($item);
		$self->current(1+$index);
		return $item;
	}
}

1;
