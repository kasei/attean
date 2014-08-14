use v5.14;
use warnings;

package Attean::API::Binding 0.001 {
	use Moose::Role;
	
	requires 'value';
	requires 'variables';
	
	sub tuples_string {
		my $self	= shift;
		my @terms	= map { $self->value($_) } $self->variables;
		my @strs	= map { $_->ntriples_string } @terms;
		return join(' ', @strs);
	}
}

package Attean::API::Triple 0.001 {
	use Moose::Role;
	
	with 'Attean::API::Binding';
	
	sub variables { return qw(subject predicate object) }
	sub value {
		my $self	= shift;
		my $key		= shift;
		if ($key =~ /^(subject|predicate|object)$/) {
			return $self->$key();
		} else {
			die "Unrecognized binding name '$key'";
		}
	}
	
	requires 'subject';		# TODO: type constrain to Attean::BlankOrIRI
	requires 'predicate';	# TODO: type constrain to Attean::IRI
	requires 'object';		# TODO: type constrain to Attean::API::Term
}

package Attean::API::Quad 0.001 {
	use Moose::Role;
	
	with 'Attean::API::Binding';
	
	sub variables { return qw(subject predicate object graph) }
	sub value {
		my $self	= shift;
		my $key		= shift;
		if ($key =~ /^(subject|predicate|object|graph)$/) {
			return $self->$key();
		} else {
			die "Unrecognized binding name '$key'";
		}
	}
	
	requires 'subject';		# TODO: type constrain to Attean::BlankOrIRI
	requires 'predicate';	# TODO: type constrain to Attean::IRI
	requires 'object';		# TODO: type constrain to Attean::API::Term
	requires 'graph';		# TODO: type constrain to Attean::IRI
}

package Attean::API::Result 0.001 {
	use Moose::Role;
	
	with 'Attean::API::Binding';
	
	sub join {
		my $self	= shift;
		my $class	= ref($self);
		my $rowb	= shift;
	
		my %keysa;
		my @keysa	= $self->variables;
		@keysa{ @keysa }	= (1) x scalar(@keysa);
		my @shared	= grep { exists $keysa{ $_ } } ($rowb->variables);
		foreach my $key (@shared) {
			my $val_a	= $self->value($key);
			my $val_b	= $rowb->value($key);
			next unless (defined($val_a) and defined($val_b));
			my $equal	= (refaddr($val_a) == refaddr($val_b)) || $val_a->equal( $val_b );
			unless ($equal) {
				return;
			}
		}
	
		my $row	= { (map { $_ => $self->value($_) } grep { defined($self->value($_)) } $self->variables), (map { $_ => $rowb->value($_) } grep { defined($rowb->value($_)) } $rowb->variables) };
		my $joined	= Attean::Result->new( $row );
		return $joined;
	}
}


1;
