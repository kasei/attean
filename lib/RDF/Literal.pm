use v5.14;
use warnings;

package RDF::Literal 0.001 {
	use RDF::API::Term;
	use Moose;
	use IRI;
	
	has 'type'	=> ( is => 'ro', isa => 'RDF::TermType', required => 1, handles => [qw(language)] );
	has 'ntriples_string'	=> (is => 'ro', isa => 'Str', lazy => 1, builder => '_ntriples_string');

	with 'RDF::API::Literal';

	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 1) {
			my $dt	= RDF::TermType::DatatypeLiteral->new('http://www.w3.org/2001/XMLSchema#string');
			return $class->$orig(value => shift, type => $dt);
		}
		
		my %args	= @_;
		if (exists($args{datatype})) {
			my $dt	= delete $args{datatype};
			$args{type}	= RDF::TermType::DatatypeLiteral->new($dt);
		} elsif (exists($args{language})) {
			my $lang	= delete $args{language};
			$args{type}	= RDF::TermType::LanguageLiteral->new($lang);
		}
		return $class->$orig(%args);
	};
	
	sub datatype {
		my $self	= shift;
		my $type	= $self->type;
		if ($type->isa('RDF::TermType::LanguageLiteral')) {
			return IRI->new('http://www.w3.org/2001/XMLSchema#string');
		} else {
			return $type->datatype;
		}
	}
	
	sub _ntriples_string {
		my $self	= shift;
		my $type	= $self->type;
		my $value	= $self->value;
		$value		=~ s/\\/\\\\/g;
		$value		=~ s/\n/\\n/g;
		$value		=~ s/\r/\\r/g;
		$value		=~ s/"/\\"/g;
		if ($type->isa('RDF::TermType::DatatypeLiteral')) {
			my $dt	= $type->datatype->as_string;
			if ($dt eq 'http://www.w3.org/2001/XMLSchema#string') {
				return sprintf('"%s"', $self->value);
			} else {
				return sprintf('"%s"^^<%s>', $value, $dt);
			}
		} else {
			my $lang	= $type->language;
			return sprintf('"%s"@%s', $value, $lang);
		}
	}
}

1;
