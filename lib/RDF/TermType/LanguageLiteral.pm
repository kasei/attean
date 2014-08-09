use v5.14;
use warnings;

package RDF::TermType::LanguageLiteral 0.001 {
	use Moose;
	extends 'RDF::TermType';
	
	has language	=> (is => 'ro', isa => 'Str', required => 1);
	
	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 1) {
			return $class->$orig(language => shift);
		}
		return $class->$orig(@_);
	};
}

1;
