use v5.14;
use warnings;

=head1 NAME

Attean::API::Binding - Name to term bindings

=head1 VERSION

This document describes Attean::API::Binding version 0.000

=head1 DESCRIPTION

The Attean::API::Binding role defines a common API for all objects that map
names to L<Attean::API::Term> objects. This includes triples, quads, and
SPARQL results (variable bindings).

=head1 REQUIRED METHODS

Classes consuming this role must provide the following methods:

=over 4

=item C<< value( $name ) >>

Returns the L<Attean::API::Term> object mapped to the variable named C<< $name >>,
or C<< undef >> if no such term is mapped.

=item C<< variables >>

Returns a list of the variable names mapped to L<Attean::API::Term> objects in
this mapping.

=item C<< apply_map( $mapper ) >>

Returns a new mapping object (of the same class as the referent) with term
objects rewritten using the supplied L<Attean::Mapper> object C<< $mapper >>.

=back

=head1 METHODS

This role provides default implementations of the following methods:

=over 4

=item C<< mapping >>

Returns a HASH mapping variable names to L<Attean::API::Term> objects.

=item C<< values >>

Returns a list of L<Attean::API::Term> objects corresponding to the variable
names returned by the referent's C<< variables >> method.

=item C<< tuples_string >>

Returns a string serialization of the L<Attean::API::Term> objects in the order
they are returned by the referent's C<< values >> method.

=item C<< as_string >>

Returns a string serialization of the variable bindings.

=item C<< has_blanks >>

Returns true if any variable is bound to an L<Attean::API::Blank> term, false
otherwise.

=cut

use Type::Tiny::Role;

package Attean::API::Binding 0.001 {
	use Moo::Role;
	use List::MoreUtils qw(zip);
	use namespace::clean;
	
	requires 'value';
	requires 'variables';
	requires 'apply_map';
	
	sub mapping {
		my $self	= shift;
		my @k		= $self->variables;
		my @v		= $self->values;
		return zip @k, @v;
	}
	
	sub values {
		my $self	= shift;
		return map { $self->value($_) } $self->variables;
	}
	
	sub tuples_string {
		my $self	= shift;
		my @strs	= map { $_->ntriples_string } $self->values;
		return join(' ', @strs) . ' .';
	}
	sub as_string {
		shift->tuples_string();
	}
	
	sub has_blanks {
		my $self	= shift;
		my @blanks	= grep { $_->does('Attean::API::Blank') } $self->values;
		return scalar(@blanks);
	}
}

package Attean::API::TripleOrQuadPattern 0.001 {
	use Moo::Role;
	sub apply_map {
		my $self	= shift;
		my $class	= ref($self);
		my $mapper	= shift;
		my %values	= map { $_ => $mapper->map($self->value($_)) } $self->variables;
		return $class->new( %values );
	}
}

package Attean::API::TripleOrQuad 0.001 {
	use Moo::Role;
	with 'Attean::API::TripleOrQuadPattern';
}

package Attean::API::TriplePattern 0.001 {
	use Moo::Role;
	use List::MoreUtils qw(zip);
	use namespace::clean;
	
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
	
	sub as_quad_pattern {
		my $self	= shift;
		my $graph	= shift;
		my @keys	= Attean::API::Quad->variables;
		my @values	= ($self->values, $graph);
		return Attean::QuadPattern->new(zip @keys, @values);
	}

	requires 'subject';
	requires 'predicate';
	requires 'object';

	with 'Attean::API::TripleOrQuadPattern', 'Attean::API::Binding';
}

package Attean::API::Triple 0.001 {
	use Moo::Role;
	
	if ($ENV{ATTEAN_TYPECHECK}) {
		my %map	= (
			subject		=> 'Attean::API::BlankOrIRI',
			predicate	=> 'Attean::API::IRI',
			object		=> 'Attean::API::Term'
		);
		foreach my $method (keys %map) {
			my $role	= $map{$method};
			around $method => sub {
				my $orig	= shift;
				my $self	= shift;
				my $class	= ref($self);
				my $term	= $self->$orig(@_);
				my $type	= Type::Tiny::Role->new( role => $role );
				my $err		= $type->validate($term);
				if ($err) {
					die "${class}'s $method failed conformance check for role $role";
				}
				return $term;
			};
		}
	}
	
	sub as_quad {
		my $self	= shift;
		my $graph	= shift;
		return Attean::Quad->new($self->values, $graph);
	}

	with 'Attean::API::TriplePattern', 'Attean::API::TripleOrQuad', 'Attean::API::Binding';
}

package Attean::API::QuadPattern 0.001 {
	use Moo::Role;
	
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
	
	requires 'subject';
	requires 'predicate';
	requires 'object';
	requires 'graph';

	with 'Attean::API::TripleOrQuadPattern', 'Attean::API::Binding';
}

package Attean::API::Quad 0.001 {
	use Moo::Role;
	
	if ($ENV{ATTEAN_TYPECHECK}) {
		my %map	= (
			# subject, predicate, and graph type checking is done by the Attean::API::Triple method modifiers
			graph		=> 'Attean::API::BlankOrIRI',
		);
		foreach my $method (keys %map) {
			my $role	= $map{$method};
			around $method => sub {
				my $orig	= shift;
				my $self	= shift;
				my $class	= ref($self);
				my $term	= $self->$orig(@_);
				my $type	= Type::Tiny::Role->new( role => $role );
				my $err		= $type->validate($term);
				if ($err) {
					die "${class}'s $method failed conformance check for role $role";
				}
				return $term;
			};
		}
	}

	sub as_triple {
		my $self	= shift;
		my @values	= $self->values;
		return Attean::Triple->new(@values[0..2]);
	}

	with 'Attean::API::QuadPattern';
	with 'Attean::API::TripleOrQuad', 'Attean::API::TripleOrQuadPattern', 'Attean::API::Triple';
}


package Attean::API::Result 0.001 {
	use Moo::Role;
	use Scalar::Util qw(refaddr);
	use namespace::clean;
	
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
			my $equal	= (refaddr($val_a) == refaddr($val_b)) || $val_a->equals( $val_b );
			unless ($equal) {
				return;
			}
		}
	
		my $row	= { (map { $_ => $self->value($_) } grep { defined($self->value($_)) } $self->variables), (map { $_ => $rowb->value($_) } grep { defined($rowb->value($_)) } $rowb->variables) };
		my $joined	= Attean::Result->new( bindings => $row );
		return $joined;
	}
	
	sub project {
		my $self	= shift;
		my @vars	= @_;
		my %bindings;
		foreach my $v (@vars) {
			my $term	= $self->value($v);
			if ($term) {
				$bindings{ $v }	= $term;
			}
		}
		return Attean::Result->new( bindings => \%bindings );
	}
	
	sub project_complement {
		my $self	= shift;
		my %vars	= map { $_ => 1 } @_;
		my %bindings;
		foreach my $v ($self->variables) {
			unless ($vars{$v}) {
				my $term	= $self->value($v);
				$bindings{ $v }	= $term;
			}
		}
		return Attean::Result->new( bindings => \%bindings );
	}
	
	sub apply_map {
		my $self	= shift;
		my $class	= ref($self);
		my $mapper	= shift;
		my %values	= map { $_ => $mapper->map($self->value($_)) } $self->variables;
		return $class->new( bindings => \%values );
	}

	with 'Attean::API::Binding', 'Attean::API::ResultOrTerm';
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO

L<http://www.perlrdf.org/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
