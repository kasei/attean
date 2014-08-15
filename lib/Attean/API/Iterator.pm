use v5.14;
use warnings;

package Attean::API::Iterator 0.001 {
	use Moose::Role;
	use Moose::Util::TypeConstraints;
	
	has 'item_type' => (is => 'ro', isa => 'Moose::Meta::TypeConstraint', required => 1);
	requires 'next';
	
	sub map {
		my $self	= shift;
		my $block	= shift;
		my $type	= shift || $self->item_type;
		
		Attean::CodeIterator->new(
			item_type => $type,
			generator => sub {
				my $item	= $self->next();
				return unless defined($item);
				local($_)	= $item;
				return $block->($item);
			}
		);
	}

	sub grep {
		my $self	= shift;
		my $block	= shift;
		my $type	= $self->item_type;
		
		Attean::CodeIterator->new(
			item_type => $type,
			generator => sub {
				while (1) {
					my $item	= $self->next();
					return unless defined($item);
					local($_)	= $item;
					if ($block->($item)) {
						return $item;
					}
				}
			}
		);
	}

}

package Attean::API::RepeatableIterator 0.001 {
	use Moose::Role;
	use Moose::Util::TypeConstraints;
	
	requires 'reset';
	with 'Attean::API::Iterator';
}

1;
