use v5.14;
use warnings;

package Attean::Triple 0.001 {
	use Moose;
	use Attean::API::Binding;
	
	has 'subject'	=> (is => 'ro', isa => 'Attean::API::BlankOrIRI', required => 1);
	has 'predicate'	=> (is => 'ro', isa => 'Attean::API::IRI', required => 1);
	has 'object'	=> (is => 'ro', isa => 'Attean::API::Term', required => 1);
	
	with 'Attean::API::Triple';
	
	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 3) {
			my %args;
			@args{qw(subject predicate object)}	= @_;
			return $class->$orig(%args);
		}
		return $class->$orig(@_);
	};
	
	sub as_string {
		my $self	= shift;
		my @terms	= map { $self->$_()->ntriples_string() } qw(subject predicate object);
		return join(' ', @terms) . ' .';
	}
}

1;
