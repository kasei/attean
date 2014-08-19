use v5.14;
use warnings;

package Attean::IteratorSequence 0.001 {
	use Moose;
	use Moose::Util::TypeConstraints;
	
	with 'Attean::API::Iterator';
	
	has iterators => (is => 'ro', isa => 'ArrayRef[Attean::API::Iterator]', required => 1);
	
	sub next {
		my $self	= shift;
		my $list	= $self->iterators;
		
		while (1) {
			return unless (scalar(@$list));
			my $iter	= $list->[0];
			my $item	= $iter->next;
			unless (defined($item)) {
				shift(@$list);
				next;
			}
			return $item;
		}
	}
	
	sub push {
		my $self	= shift;
		my $iter	= shift;
		push(@{ $self->iterators }, $iter);
		return;
	}
}

1;
