use v5.14;
use warnings;

=head1 NAME

Attean::API::Binding - Name to term bindings

=head1 VERSION

This document describes Attean::API::Binding version 0.034

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

package Attean::API::Binding 0.034 {
	use Scalar::Util qw(blessed);
	use List::MoreUtils qw(zip);

	use Moo::Role;
	
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
		foreach my $term ($self->values) {
			return 1 if ($term->does('Attean::API::Blank'));
			if ($term->does('Attean::API::Binding')) {
				return 1 if ($term->has_blanks);
			}
		}
		return 0;
	}
	
=item C<< sameTerms( $other ) >>

=cut

	sub sameTerms {
		my $self	= shift;
		my $other	= shift;
		return 0 unless ($other->does('Attean::API::Binding'));
		my @variables	= sort $self->variables;
		my @other_vars	= sort $other->variables;
		return 0 unless (scalar(@variables) == scalar(@other_vars));
		foreach my $i (0 .. $#variables) {
			return 0 unless $variables[$i] eq $other_vars[$i];
		}
		
		foreach my $v (@variables) {
			my $value		= $self->value($v);
			my $other_value	= $other->value($v);
			return 0 unless $value->sameTerms($other_value);
		}
		return 1;
	}
	
=item C<< equals( $other ) >>

=cut

	sub equals {
		my $self	= shift;
		my $other	= shift;
		return 0 unless ($other->does('Attean::API::Binding'));
		my @variables	= sort $self->variables;
		my @other_vars	= sort $other->variables;
		unless (scalar(@variables) == scalar(@other_vars)) {
			return 0;
		}
		foreach my $i (0 .. $#variables) {
			unless ($variables[$i] eq $other_vars[$i]) {
				return 0;
			}
		}
		
		foreach my $v (@variables) {
			my $value		= $self->value($v);
			my $other_value	= $other->value($v);
			if ($value->does('Attean::API::Binding')) {
				unless ($value->equals($other_value)) {
					return 0;
				}
			} else {
				unless (0 == $value->compare($other_value)) {
					return 0;
				}
			}
		}
		return 1;
	}
	
=item C<< blanks >>

Returns all the values in this mapping (recursively, if any values are embedded
bindings) that are blank nodes.

=cut

	sub blanks {
		my $self	= shift;
		my %nodes;
		foreach my $term ($self->values) {
			if ($term->does('Attean::API::Blank')) {
				$nodes{ $term->value }	= $term;
			}
			if ($term->does('Attean::API::Binding')) {
				foreach my $b ($term->blanks) {
					$nodes{ $b->value }	= $b;
				}
			}
		}
		return CORE::values %nodes;
	}
	
=item C<< referenced_variables >>

Returns a list of the names of any variable values that are referenced in this
binding (recursively, if any values are embedded bindings).

=cut

	sub referenced_variables {
		my $self	= shift;
		my %vars;
		foreach my $v ($self->values) {
			if ($v->does('Attean::API::Variable')) {
				$vars{$v->value}++;
			} elsif ($v->does('Attean::API::Binding')) {
				foreach my $name ($v->referenced_variables) {
					$vars{$name}++;
				}
			}
		}
		return keys %vars;
	}
	
=item C<< is_ground >>

Returns tue is all the bound values consume L<Attean::API::Term>, false otherwise.

=cut

	sub is_ground {
		my $self	= shift;
		my @non_terms	= grep { not($_->does('Attean::API::Term')) } $self->values;
		my @bad			= grep { not($_->does('Attean::API::Binding') and $_->is_ground) } @non_terms;
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
			if ($v->does('Attean::API::TriplePattern')) {
				my $replace	= $v->apply_bindings($bind);
				$data{ $k }	= $replace;
			} elsif ($v->does('Attean::API::Variable')) {
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

package Attean::API::TripleOrQuadPattern 0.034 {
	use Encode qw(encode);
	use List::MoreUtils qw(zip);
	use Scalar::Util qw(blessed);
	use Attean::RDF;
	use Attean::API::Query;

	use Moo::Role;

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
					$args[$i]	= Attean::RDF::variable($k);
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
		
		if (scalar(@_) % 2) {
			Carp::cluck;
		}
		my %args	= @_;
		foreach my $k ($class->variables) {
			if (not(exists $args{$k}) or not($args{$k})) {
				$args{$k}	= Attean::RDF::variable($k);
			}
		}
		
		return $class->$orig(%args);
	};

	sub apply_map {
		my $self	= shift;
		my $class	= ref($self);
		my $mapper	= shift;
		my %values;
		foreach my $pos ($self->variables) {
			my $value	= $self->value($pos);
			if ($value->does('Attean::API::Binding')) {
				$values{$pos}	= $value->apply_map($mapper);
			} else {
				$values{$pos}	= $mapper->map($value);
			}
		}
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

	sub ground {
		my $self	= shift;
		my $result	= shift;
		
		my %bindings;
		my @vars	= $self->variables();
		foreach my $pos (@vars) {
			my $pp	= $self->$pos();
			if ($pp->does('Attean::API::Variable')) {
				$bindings{ $pos }	= $result->value($pp->value);
			} elsif ($pp->does('Attean::API::TriplePattern')) {
				my $sub_ground	= $pp->ground($result);
				$bindings{ $pos }	= $sub_ground;
			} else {
				$bindings{ $pos }	= $pp;
			}
		}
		
		return scalar(@vars) == 3
			? Attean::Triple->new( %bindings )
			: Attean::Quad->new( %bindings );
	}
	
	sub unify {
		my $self	= shift;
		my $quad	= shift;
		my %binding;
		foreach my $pos ($self->variables) {
			my $pp	= $self->$pos();
			my $qp	= $quad->$pos();
			if ($pp->does('Attean::API::Variable')) {
				if (my $already = $binding{ $pp->value }) {
					return unless $already->equals($qp);
				}
				$binding{ $pp->value }	= $qp;
			} elsif ($pp->does('Attean::API::TriplePattern')) {
				return unless ($qp->does('Attean::API::Triple'));
				my $sub_binding	= $pp->unify($qp);
				return unless $sub_binding;
				my $bkeys	= Set::Scalar->new(keys %binding);
				my $sbkeys	= Set::Scalar->new($sub_binding->variables);
				my $i		= $bkeys->intersection($sbkeys);
				for my $key ($i->elements) {
					# variable bound in multiple places with different values
					return unless ($binding{$key}->equals($sub_binding->value($key)));
				}
				my $mapping	= {$sub_binding->mapping};
				@binding{ keys %$mapping }	= values %$mapping;
			} else {
				# bound position doesn't match
				use Data::Dumper;
				if ($pp->does('Attean::API::QuadPattern')) {
					Carp::cluck 'XXX unify: ' . Dumper($self);
				}
				
				return unless ($pp->equals($qp));
			}
		}
		
# 		warn 'final mapping: ' . Dumper(\%binding);
		return Attean::Result->new( bindings => \%binding );
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

package Attean::API::TripleOrQuad 0.034 {
	use List::MoreUtils qw(any);
	use Carp;

	use Moo::Role;

	with 'Attean::API::TripleOrQuadPattern';

	sub BUILD {
		my $self = shift;
		if (any { $_->does('Attean::API::Variable') } $self->values) {
			croak 'Use a Pattern class to construct when using variables';
		}
	}
}

package Attean::API::TriplePattern 0.034 {
	use List::MoreUtils qw(zip);
	use Scalar::Util qw(blessed);

	use Moo::Role;
	
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
		my @terms	= map { $_->does('Attean::API::TriplePattern') ? $_->as_triple : $_ } $self->values;
		return Attean::Triple->new(@terms);
	}
	
	sub apply_triple {
		my $self	= shift;
		return $self->apply_statement(@_);
	}

	sub sparql_tokens {
		my $self	= shift;
		my @tokens;
		foreach my $t ($self->values) {
			if ($t->does('Attean::API::TriplePattern')) {
				push(@tokens, AtteanX::SPARQL::Token->ltlt);
				push(@tokens, $t->sparql_tokens->elements);
				push(@tokens, AtteanX::SPARQL::Token->gtgt);
			} else {
				push(@tokens, $t->sparql_tokens->elements);
			}
		}
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
	
	requires 'subject';
	requires 'predicate';
	requires 'object';

	with 'Attean::API::TripleOrQuadPattern', 'Attean::API::Binding', 'Attean::API::TermOrVariableOrTriplePattern';
}

package Attean::API::Triple 0.034 {
	use Scalar::Util qw(blessed);
	use Moo::Role;
	
	if ($ENV{ATTEAN_TYPECHECK}) {
		my %map	= (
			subject		=> 'Attean::API::BlankOrIRIOrTriple',
			predicate	=> 'Attean::API::IRI',
			object		=> 'Attean::API::TermOrTriple'
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
					die "${class}'s $method failed conformance check for role $role: " . $term->as_string;
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

	sub ntriples_string {
		my $self	= shift;
		my @values	= $self->values;
		return join(' ', '<<', (map { $_->ntriples_string } @values), '>>');
	}

	sub order {
		my $self	= shift;
		return _compare('order', $self, @_);
	}
	
	sub compare {
		my $self	= shift;
		return _compare('compare', $self, @_);
	}
	
	sub _compare {
		my $cmp_method	= shift;
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		if (not $b->does('Attean::API::Triple')) {
			# this is a type-error for equality testing, but special handling is needed in calling code for ORDER BY in which Triples sort last (after literals)
			die "TypeError: cannot compare an RDF-star triple and a non-triple";
		}
		
		foreach my $pos ($a->variables) {
			my $at	= $a->$pos();
			my $bt	= $b->$pos();
			my $c	= $at->$cmp_method($bt);
			
			# If they are equal, continue. otherwise check if either term is an IRI.
			# This is because term equality is defined for IRIs, but < and > isn't.
			next unless ($c);
			
			unless ($Attean::API::Binding::ALLOW_IRI_COMPARISON) {
				for ($at, $bt) {
					if ($_->does('Attean::API::IRI')) {
	# 					Carp::cluck "TypeError comparison of IRI " . $at->ntriples_string . " <=> " . $bt->ntriples_string . "\n";
	# 					last;
						die "TypeError comparison of IRI" if ($_->does('Attean::API::IRI')); # comparison of IRIs is only defined for `ORDER BY`, not for general expressions
					}
				}
			}

			if ($c) {
				return $c;
			}
		}
		
		return 0;
# 		return $a->ntriples_string cmp $b->ntriples_string;
	}

	with 'Attean::API::TriplePattern', 'Attean::API::TripleOrQuad', 'Attean::API::Binding', 'Attean::API::TermOrVariableOrTriplePattern';
	with 'Attean::API::BlankOrIRIOrTriple';
	with 'Attean::API::TermOrTriple';
}

package Attean::API::QuadPattern 0.034 {
	use Scalar::Util qw(blessed);
	use List::MoreUtils qw(zip);

	use Moo::Role;
	
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

package Attean::API::Quad 0.034 {
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


package Attean::API::Result 0.034 {
	use Scalar::Util qw(refaddr);
	use Types::Standard qw(HashRef);

	use Moo::Role;
	
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
		my %values;
		foreach my $var ($self->variables) {
			my $value	= $self->value($var);
			if ($value->does('Attean::API::Binding')) {
				$values{$var}	= $value->apply_map($mapper);
			} else {
				my $term	= $mapper->map($value);
				if ($term) {
					$values{$var}	= $term;
				}
			}
		}
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



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2022 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
