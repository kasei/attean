use v5.14;
use warnings;

package Attean::Variable 0.001 {
	use Moose;
	
	has 'value' => (is => 'ro', isa => 'Str', required => 1);
	has 'ntriples_string'	=> (is => 'ro', isa => 'Str', lazy => 1, builder => '_ntriples_string');

	with 'Attean::TermOrVariable';
	
	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 1) {
			return $class->$orig(value => shift);
		}
		return $class->$orig(@_);
	};
	
	sub _ntriples_string {
		my $self	= shift;
		return '?' . $self->value;
	}
}

1;
