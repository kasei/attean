use v5.14;
use warnings;

package Attean::CodeIterator 0.001 {
	use Moose;
	use Moose::Util::TypeConstraints;
	
	with 'Attean::API::Iterator';
	
	has generator => (is => 'ro', isa => 'CodeRef', required => 1);
	
	sub next {
		my $self	= shift;
		my $item	= $self->generator->();
		return unless defined($item);
		
		my $constraint	= $self->item_type;
		$constraint->assert_valid($item);
		return $item;
	}
}

1;
