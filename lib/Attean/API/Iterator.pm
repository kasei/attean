use v5.14;
use warnings;

=head1 NAME

Attean::API::Iterator - Typed iterator

=head1 VERSION

This document describes Attean::API::Iterator version 0.027

=head1 DESCRIPTION

The Attean::API::Iterator role defines a common API for typed iterators.
This package also defines several type-specific iterator roles:

=over 4

=item * L<Attean::API::TripleIterator>

=item * L<Attean::API::QuadIterator>

=item * L<Attean::API::MixedStatementIterator>

=item * L<Attean::API::ResultIterator>

=back

These roles will automatically be applied to iterators during construction when appropriate.

=head1 ATTRIBUTES

The following attributes exist:

=over 4

=item C<< item_type >>

A string indicating the type of elements returned by the iterator.

=back

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Iterator> role:

=over 4

=item C<< next >>

Returns the next element from the iterator, or C<< undef >> upon exhaustion.

=back

=head1 METHODS

The L<Attean::API::Iterator> role provides default implementations of the
following methods:

=over 4

=item C<< elements >>

Returns a list of all remaining elements in the iterator.

=item C<< map( \&mapper [, $result_type] ) >>

Returns a new L<Attean::API::Iterator> object with each element mapped using
the supplied C<< &mapper >> function. If the iterator elements are of the same
type as those in the referent iterator, only a mapping function is required.
Otherwise, the supplied L<Type::Tiny> C<< $result_type >> object must indicate
the new iterator's type information.

=item C<< grep( \&filter ) >>

Returns a new L<Attean::API::Iterator> object that filters elements from the
referent iterator based on whether calling C<< &filter( $element ) >> for each
C<< $element >> results in a true value.

=item C<< offset( $offset ) >>

Returns the L<Attean::API::Iterator> referent after skipping the first
C<< $offset >> elements.

=item C<< limit( $limit ) >>

Returns a new L<Attean::API::Iterator> object which returns the first
C<< $limit >> elements of the referent.

=item C<< materialize >>

Returns a new L<Attean::API::RepeatableIterator> object containing all the
elements from the referent.

=cut

package Attean::API::Iterator 0.027 {
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Str Object InstanceOf);
	use Carp qw(confess);

	use Moo::Role;
	
	has 'item_type' => (is => 'ro', isa => Str, required => 1);
	requires 'next';

	sub BUILD {}
	around 'BUILD' => sub {
		my $orig	= shift;
		my $self	= shift;
		my $args	= shift;
		$self->$orig($args);
		my $role	= $self->item_type;
		if (Moo::Role->is_role($role)) {
			my $check	= sub {
				my $check = shift;
				return ($role eq $check or Moo::Role::does_role($role, $check));
			};
			if ($check->('Attean::API::Quad')) {
				Moo::Role->apply_roles_to_object($self, 'Attean::API::QuadIterator');
			} elsif ($check->('Attean::API::Triple')) {
				Moo::Role->apply_roles_to_object($self, 'Attean::API::TripleIterator');
			} elsif ($check->('Attean::API::TripleOrQuad')) {
				Moo::Role->apply_roles_to_object($self, 'Attean::API::MixedStatementIterator');
			} elsif ($check->('Attean::API::Result')) {
				Moo::Role->apply_roles_to_object($self, 'Attean::API::ResultIterator');
				my $vars	= $args->{variables} // confess "Construction of a Attean::API::ResultIterator must include a variables list";
				$self->variables($vars);
			} elsif ($check->('Attean::API::Term')) {
				Moo::Role->apply_roles_to_object($self, 'Attean::API::TermIterator');
			} elsif ($check->('Attean::API::ResultOrTerm')) {
				Moo::Role->apply_roles_to_object($self, 'Attean::API::ResultOrTermIterator');
				$self->variables($args->{variables} || []);
			}

			if ($self->does('Attean::API::RepeatableIterator') and $check->('Attean::API::Binding')) {
				Moo::Role->apply_roles_to_object($self, 'Attean::API::CanonicalizingBindingSet');
			}
		}
	};
	
	if ($ENV{ATTEAN_TYPECHECK}) {
		around 'next' => sub {
			my $orig	= shift;
			my $self	= shift;
			my $type	= $self->item_type;
			my $class	= ref($self);
			my $term	= $self->$orig(@_);
			return unless defined($term);
			if (blessed($term)) {
				unless ($term->does($type) or $term->isa($type)) {
					die "${class} returned an element that failed conformance check for $type: $term";
				}
			}
			return $term;
		};
	}
	
	sub elements {
		my $self	= shift;
		my @elements;
		while (my $item = $self->next) { push(@elements, $item); }
		return @elements;
	}
	
	sub map {
		my $self	= shift;
		my $block	= shift;
		my $type	= shift || $self->item_type;
		
		my $generator;
		if (blessed($block) and $block->does('Attean::Mapper')) {
			$generator	= sub {
				my $item	= $self->next();
				return unless defined($item);
				my $new		= $block->map($item);
				return $new;
			}
		} else {
			my @buffer;
			$generator	= sub {
				while (1) {
					return shift(@buffer) if (scalar(@buffer));
					my $item	= $self->next();
					return unless defined($item);
					local($_)	= $item;
					push(@buffer, $block->($item));
				}
			}
		}
		
		# copy variables into new iterator if $self does ::ResultIterator or ::ResultOrTermIterator
		my %args	= @_;
		if ($self->can('variables') and not exists $args{variables}) {
			$args{variables}	= $self->variables;
		}
		
		return Attean::CodeIterator->new( %args, item_type => $type, generator => $generator );
	}

	sub grep {
		my $self	= shift;
		my $block	= shift;
		
		# copy variables into new iterator if $self does ::ResultIterator or ::ResultOrTermIterator
		my %args	= @_;
		if ($self->can('variables') and not exists $args{variables}) {
			$args{variables}	= $self->variables;
		}
		
		Attean::CodeIterator->new(
			%args,
			item_type => $self->item_type,
			generator => sub {
				while (1) {
					my $item	= $self->next();
					return unless defined($item);
					local($_)	= $item;
					return $item if ($block->($item));
				}
			}
		);
	}
	
	sub offset {
		my $self	= shift;
		my $offset	= shift;
		$self->next for (1 .. $offset);
		return $self;
	}
	
	sub limit {
		my $self	= shift;
		my $limit	= shift;
		
		# copy variables into new iterator if $self does ::ResultIterator or ::ResultOrTermIterator
		my %args	= @_;
		if ($self->can('variables') and not exists $args{variables}) {
			$args{variables}	= $self->variables;
		}
		
		Attean::CodeIterator->new(
			%args,
			item_type => $self->item_type,
			generator => sub {
				return unless $limit;
				my $item	= $self->next();
				return unless defined($item);
				$limit--;
				return $item;
			}
		);
	}
	
	sub materialize {
		my $self	= shift;
		my @data	= $self->elements;
		my %args	= @_;
		if ($self->can('variables') and not exists $args{variables}) {
			$args{variables}	= $self->variables;
		}
		
		return Attean::ListIterator->new( %args, values => \@data, item_type => $self->item_type );
	}
	
=item C<< debug( [$name] ) >>

Print each item as it is consumed (with the string generated by C<< as_string >>), prepended by C<< $name >>.

=cut

	sub debug {
		my $self	= shift;
		my $name	= shift // 'Iterator item';
		return $self->grep(sub { my $r = shift; say "$name: " . $r->as_string; return 1; });
	}
}

package Attean::API::RepeatableIterator 0.027 {
	use Moo::Role;
	requires 'reset';
	
	sub elements {
		my $self	= shift;
		my @elements;
		while (my $item = $self->next) { push(@elements, $item); }
		$self->reset;
		return @elements;
	}
	
	sub peek {
		my $self	= shift;
		my $item	= $self->next;
		$self->reset;
		return $item;
	}

	sub materialize {
		my $self	= shift;
		return $self;
	}

	with 'Attean::API::Iterator';
}

package Attean::API::CanonicalizingBindingIterator {
	use Moo::Role;
	sub canonicalize {
		my $self	= shift;
		my $mapper	= Attean::TermMap->canonicalization_map;
		return $self->map(sub { shift->apply_map( $mapper ) });
	}
}

package Attean::API::ResultOrTermIterator 0.027 {
	use Moo::Role;
	use Types::Standard qw(ArrayRef Str);
	has 'variables' => (is => 'rw', isa => ArrayRef[Str], default => sub { [] });
	sub canonicalize {
		my $self	= shift;
		my $mapper	= Attean::TermMap->canonicalization_map;
		return $self->map(sub{
			my $item	= shift;
			if ($item->does('Attean::API::Term')) {
				return $mapper->map($item);
			} else {
				my %values	= map { $_ => $mapper->map($item->value($_)) } $item->variables;
				return Attean::Result->new( bindings => \%values );
			}
		});
	}
	
	around 'grep' => sub {
		my $orig	= shift;
		my $self	= shift;
		my $block	= shift;
		my $iter	= $orig->($self, $block, @_);
		Attean::CodeIterator->new(
			item_type => $iter->item_type,
			generator => sub {
				while (1) {
					my $item	= $iter->next();
					return unless defined($item);
					local($_)	= $item;
					return $item if ($block->($item));
				}
			},
			variables => $self->variables,
		);
	};
}

package Attean::API::StatementIterator 0.027 {
	use Moo::Role;
	use Scalar::Util qw(blessed);
	requires 'variables';

	sub matching_pattern {
		my $self	= shift;
		my @nodes	= @_;

		my %bound;
		my @pos_names	= $self->variables;
		foreach my $pos (0 .. $#pos_names) {
			my $n	= $nodes[ $pos ];
			if (blessed($n) and $n->does('Attean::API::Variable')) {
				$n	= undef;
				$nodes[$pos]	= undef;
			}
			if (blessed($n)) {
				$bound{ $pos_names[$pos] }	= $n;
			}
		}
		
		return $self->grep(sub {
			my $q	= shift;
			foreach my $key (keys %bound) {
				my $term	= $q->$key();
				unless ($term->equals( $bound{$key} )) {
					return 0;
				}
			}
			return 1;
		});
	}
}

package Attean::API::TripleIterator 0.027 {
	use Moo::Role;
	with 'Attean::API::CanonicalizingBindingIterator';
	with 'Attean::API::StatementIterator';

	sub as_quads {
		my $self	= shift;
		my $graph	= shift;
		return $self->map(sub { $_->as_quad($graph) }, 'Attean::API::Quad');
	}

	sub variables {
		return qw(subject predicate object);
	}
}

package Attean::API::QuadIterator 0.027 {
	use Moo::Role;
	with 'Attean::API::CanonicalizingBindingIterator';
	with 'Attean::API::StatementIterator';
	
	sub variables {
		return qw(subject predicate object graph);
	}
}

package Attean::API::MixedStatementIterator 0.027 {
	use Moo::Role;
	with 'Attean::API::CanonicalizingBindingIterator';
	sub as_quads {
		my $self	= shift;
		my $graph	= shift;
		return $self->map(
			sub { $_->does('Attean::API::Quad') ? $_ : $_->as_quad($graph) },
			'Attean::API::Quad'
		);
	}
}

package Attean::API::ResultIterator 0.027 {
	use Types::Standard qw(Str ArrayRef);

	use Moo::Role;
	
	with 'Attean::API::CanonicalizingBindingIterator';
	has 'variables' => (is => 'rw', isa => ArrayRef[Str], required => 1);
	sub join {
		my $self	= shift;
		my $rhs		= shift;
		my @vars	= keys %{ { map { $_ => 1 } (@{ $self->variables }, @{ $rhs->variables }) } };
		
		my @rhs		= $rhs->elements;
		my @results;
		while (my $lhs = $self->next) {
			foreach my $rhs (@rhs) {
				if (my $j = $lhs->join($rhs)) {
					push(@results, $j);
				}
			}
		}
		return Attean::ListIterator->new( values => \@results, item_type => $self->item_type, variables => \@vars);
	}
}

package Attean::API::TermIterator 0.027 {
	use Moo::Role;
	sub canonicalize {
		my $self	= shift;
		my $mapper	= Attean::TermMap->canonicalization_map;
		return $self->map( $mapper );
	}
	with 'Attean::API::CanonicalizingBindingIterator';
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO

L<Attean::API::RepeatableIterator>



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2020 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
