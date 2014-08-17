use v5.14;
use warnings;

package Attean::Quad 0.001 {
	use Moose;
	use Attean::API;
	
	has 'subject'	=> (is => 'ro', isa => 'Attean::API::BlankOrIRI', required => 1);
	has 'predicate'	=> (is => 'ro', isa => 'Attean::API::IRI', required => 1);
	has 'object'	=> (is => 'ro', isa => 'Attean::API::Term', required => 1);
	has 'graph'		=> (is => 'ro', isa => 'Attean::API::BlankOrIRI', required => 1);
	
	with 'Attean::API::Quad';
	
	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 4) {
			my %args;
			@args{qw(subject predicate object graph)}	= @_;
			return $class->$orig(%args);
		}
		return $class->$orig(@_);
	};
}

1;
