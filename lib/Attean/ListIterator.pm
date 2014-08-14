use v5.14;
use warnings;

package Attean::ListIterator 0.001 {
	use Moose;
	use Moose::Util::TypeConstraints;
	
	with 'Attean::API::Iterator';
	
	has values => (is => 'ro', isa => 'ArrayRef', required => 1);
	
	sub BUILD {
		my $self	 = shift;
		my $constraint	= $self->item_type;
		foreach my $item (@{ $self->values }) {
			$constraint->assert_valid($item);
		}
	}
	
	sub next {
		my $self	= shift;
		my $list	= $self->values;
		return shift(@$list);
	}
}

1;
