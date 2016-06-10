use v5.14;
use warnings;

=head1 NAME

Attean::API::Binding - Name to term bindings

=head1 VERSION

This document describes Attean::API::Binding version 0.017

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

package Attean::API::Binding 0.017 {
	use Moo::Role;
	use Scalar::Util qw(blessed);
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
	
=item C<< is_ground >>

Returns tue is all the bound values consume L<Attean::API::Term>, false otherwise.

=cut

	sub is_ground {
		my $self	= shift;
		my @bad	= grep { not($_->does('Attean::API::Term')) } $self->values;
		return (scalar(@bad) == 0);
	}
	
=item C<< values_consuming_role( $role ) >>

Returns the list of bound values that consume C<< $role >>.

=cut

	sub values_consuming_role {
		my $self	= shift;
		my $role	= shift;
		return grep { $_->does($role) } $self->values;
	}
	
=item C<< tree_attributes >>

Returns the variables which are bound in this object.

=cut

	sub tree_attributes {
		my $self	= shift;
		return $self->variables;
	}
	
=item C<< apply_bindings( $binding ) >>

Construct a new binding by replacing variables with their bound values from
C<< $binding >>.

=cut

	sub apply_bindings {
		my $self	= shift;
		my $class	= ref($self);
		my $bind	= shift;
		my %data;
		foreach my $k ($self->variables) {
			my $v	= $self->value($k);
			if ($v->does('Attean::API::Variable')) {
				my $name	= $v->value;
				my $replace	= $bind->value($name);
				if (defined($replace) and blessed($replace)) {
					$data{ $k }	= $replace;
				} else {
					$data{ $k }	= $v;
				}
			} else {
				$data{ $k }	= $v;
			}
		}
		return $class->new( bindings => \%data );
	}
}

package Attean::API::TripleOrQuadPattern 0.017 {
	use Encode qw(encode);
	use List::MoreUtils qw(zip);
	use Scalar::Util qw(blessed);
	use Attean::RDF;
	use Attean::API::Query;
	use Moo::Role;
	use namespace::clean;

	with 'Attean::API::SPARQLSerializable';
	
	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		my @args	= @_;
		if (scalar(@args) == 0 or not(defined($_[0])) or blessed($args[0])) {
			my @names	= $class->variables;
			foreach my $i (0 .. $#names) {
				my $k	= $names[$i];
				my $v	= $args[$i];
				unless (defined($v)) {
					$args[$i]	= variable($k);
				}
			}
			my %args;
			@args{ $class->variables }	= @args;
			return $class->$orig(%args);
		} elsif (scalar(@args) == 2) {
			if (defined($args[0]) and $args[0] eq 'bindings') {
				return $class->$orig(%{ $args[1] });
			}
		}
		
		my %args	= @_;
		foreach my $k ($class->variables) {
			if (not(exists $args{$k}) or not($args{$k})) {
				$args{$k}	= variable($k);
			}
		}
		
		return $class->$orig(%args);
	};

	sub apply_map {
		my $self	= shift;
		my $class	= ref($self);
		my $mapper	= shift;
		my %values	= map { $_ => $mapper->map($self->value($_)) } $self->variables;
		return $class->new( %values );
	}
	
	sub apply_statement {
		my $self	= shift;
		my $class	= ref($self);
		my $bind	= shift;
		my %data;
		foreach my $k ($self->variables) {
			my $v	= $self->value($k);
			if ($v->does('Attean::API::Variable')) {
				my $name	= $v->value;
				my $replace	= $bind->value($name);
				if (defined($replace) and blessed($replace)) {
					$data{ $k }	= $replace;
				} else {
					$data{ $k }	= $v;
				}
			}
		}
		return Attean::Result->new( bindings => \%data );
	}

	sub canonicalize {
		my $self	= shift;
		my $type	= ref($self);
		my $role	= $self->does('Attean::API::TriplePattern') ? 'Attean::API::TriplePattern' : 'Attean::API::QuadPattern';
		my $iter	= Attean::ListIterator->new( values => [$self], item_type => $role );
		my $triples	= $iter->canonical_set();
		my ($t)		= @$triples;
		return $t;
	}
	
=item C<< parse ( $string ) >>

Returns a triple or quad pattern object using the variables and/or terms
parsed from C<< $string >> in SPARQL syntax.

=cut

	sub parse {
		my $self	= shift;
		my $class	= ref($self) || $self;
		my $string	= shift;
		my $bytes	= encode('UTF-8', $string, Encode::FB_CROAK);
		my $parser	= Attean->get_parser('SPARQL')->new(@_);
		my @values	= $parser->parse_nodes($bytes);
		my @keys	= $self->variables;
		
		my $f	= scalar(@values);
		my $e	= scalar(@keys);
		unless ($e == $f) {
			die "${class}->parse found wrong number of nodes (found $f but expecting $e)";
		}
		return $self->new(zip @keys, @values);
	}
}

package Attean::API::TripleOrQuad 0.017 {
	use Moo::Role;
	use List::MoreUtils qw(any);
	use Carp;
	with 'Attean::API::TripleOrQuadPattern';

	sub BUILD {
		my $self = shift;
		if (any { $_->does('Attean::API::Variable') } $self->values) {
			croak 'Use a Pattern class to construct when using variables';
		}
	}
}

package Attean::API::TriplePattern 0.017 {
	use Moo::Role;
	use List::MoreUtils qw(zip);
	use Scalar::Util qw(blessed);
	use namespace::clean;
	
	sub variables { return qw(subject predicate object) }

	sub value {
		my $self	= shift;
		my $key		= shift;
		return $self->$key() if ($key =~ /^(subject|predicate|object)$/);
		die "Unrecognized binding name '$key'";
	}
	
	sub as_quad_pattern {
		my $self	= shift;
		my $graph	= shift;
		my @keys	= Attean::API::Quad->variables;
		my @values	= ($self->values, $graph);
		return Attean::QuadPattern->new(zip @keys, @values);
	}
	
	sub as_triple {
		my $self	= shift;
		unless ($self->is_ground) {
			die "Not a ground triple: " . $self->as_string;
		}
		return Attean::Triple->new($self->values);
	}
	
	sub apply_triple {
		my $self	= shift;
		return $self->apply_statement(@_);
	}

	sub sparql_tokens {
		my $self	= shift;
		my @tokens;
		foreach my $t ($self->values) {
			push(@tokens, $t->sparql_tokens->elements);
		}
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
	
	requires 'subject';
	requires 'predicate';
	requires 'object';

	with 'Attean::API::TripleOrQuadPattern', 'Attean::API::Binding';
}

package Attean::API::Triple 0.017 {
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

package Attean::API::QuadPattern 0.017 {
	use Scalar::Util qw(blessed);
	use List::MoreUtils qw(zip);
	use Moo::Role;
	use namespace::clean;
	
	sub variables { return qw(subject predicate object graph) }
	sub value {
		my $self	= shift;
		my $key		= shift;
		return $self->$key() if ($key =~ /^(subject|predicate|object|graph)$/);
		die "Unrecognized binding name '$key'";
	}
	
	sub as_quad {
		my $self	= shift;
		unless ($self->is_ground) {
			die "Not a ground quad: " . $self->as_string;
		}
		return Attean::Quad->new($self->values);
	}
	
	sub apply_quad {
		my $self	= shift;
		return $self->apply_statement(@_);
	}

	sub as_triple_pattern {
		my $self	= shift;
		my @keys	= Attean::API::Triple->variables;
		my @values	= $self->values;
		@values		= @values[0 .. scalar(@keys)-1];
		return Attean::TriplePattern->new(zip @keys, @values);
	}

	sub sparql_tokens {
		my $self	= shift;
		my @tokens;
		push(@tokens, AtteanX::SPARQL::Token->keyword('GRAPH'));
		push(@tokens, $self->graph->sparql_tokens->elements);
		push(@tokens, AtteanX::SPARQL::Token->lbrace());
		my @values	= ($self->values)[0..2];
		foreach my $t (@values) {
			push(@tokens, $t->sparql_tokens->elements);
		}
		push(@tokens, AtteanX::SPARQL::Token->rbrace());
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
	
	requires 'subject';
	requires 'predicate';
	requires 'object';
	requires 'graph';

	with 'Attean::API::TripleOrQuadPattern', 'Attean::API::Binding';
}

package Attean::API::Quad 0.017 {
	use Moo::Role;
	
	if ($ENV{ATTEAN_TYPECHECK}) {
		my $type	= Type::Tiny::Role->new( role => 'Attean::API::BlankOrIRI' );
		around 'graph' => sub {
			my $orig	= shift;
			my $self	= shift;
			my $class	= ref($self);
			my $term	= $self->$orig(@_);
			my $err		= $type->validate($term);
			die "${class}'s graph failed conformance check for role Attean::API::BlankOrIRI: $term" if ($err);
			return $term;
		};
	}

	sub as_triple {
		my $self	= shift;
		my @values	= $self->values;
		return Attean::Triple->new(@values[0..2]);
	}

	with 'Attean::API::QuadPattern';
	with 'Attean::API::TripleOrQuad', 'Attean::API::TripleOrQuadPattern', 'Attean::API::Triple';
}


package Attean::API::Result 0.017 {
	use Moo::Role;
	use Scalar::Util qw(refaddr);
	use Types::Standard qw(HashRef);
	use namespace::clean;
	
	has 'eval_stash' => (is => 'rw', isa => HashRef);

	sub BUILD {
		my $self	= shift;
		if (not $self->eval_stash) {
			$self->eval_stash({});
		}
	}
	
	sub shared_domain {
		my $self	= shift;
		my $class	= ref($self);
		my $rowb	= shift;
	
		my %keysa;
		my @keysa	= $self->variables;
		@keysa{ @keysa }	= (1) x scalar(@keysa);
		my @shared	= grep { exists $keysa{ $_ } } ($rowb->variables);
		return @shared;
	}
	
	sub join {
		my $self	= shift;
		my $class	= ref($self);
		my $rowb	= shift;
		my @shared	= $self->shared_domain($rowb);
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
	
=item C<< project( @keys ) >>

Returns a new L<Attean::Result> binding which contains variable-value mappings
from the invocant for every variable name in C<< @keys >>.

=cut

	sub project {
		my $self	= shift;
		my @vars	= @_;
		my %bindings;
		foreach my $v (@vars) {
			my $term	= $self->value($v);
			$bindings{ $v }	= $term if ($term);
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

Copyright (c) 2014--2016 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
