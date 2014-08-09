use v5.14;
use warnings;

package RDF::TermType::DatatypeLiteral 0.001 {
	use Moose;
	use Moose::Util::TypeConstraints;
	use IRI;
	extends 'RDF::TermType';
	
	coerce 'IRI' => from 'Str' => via { IRI->new( value => $_ ) };
	has datatype	=> (is => 'ro', isa => 'IRI', required => 1, coerce => 1);
	
	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 1) {
			return $class->$orig(datatype => shift);
		}
		return $class->$orig(@_);
	};
}

1;
