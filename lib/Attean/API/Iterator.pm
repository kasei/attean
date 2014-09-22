use v5.14;
use warnings;

=head1 NAME

Attean::API::Iterator - Typed iterator

=head1 VERSION

This document describes Attean::API::Iterator version 0.001

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

A L<Type::Tiny> object indicating the type of elements returned by the iterator.

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

=back

=cut

use Type::Tiny::Role;

package Attean::API::Iterator 0.001 {
	use Moo::Role;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Object InstanceOf);
	use Role::Tiny;
	use namespace::clean;
	
	has 'item_type' => (is => 'ro', isa => InstanceOf['Type::Tiny'], required => 1);
	requires 'next';
	
	sub BUILD {}
	around 'BUILD' => sub {
		my $orig	= shift;
		my $self	= shift;
		$self->$orig(@_);
		my $type	= $self->item_type;
		if ($type->is_a_type_of(Type::Tiny::Role->new(role => 'Attean::API::Triple'))) {
			Role::Tiny->apply_roles_to_object($self, 'Attean::API::TripleIterator');
		} elsif ($type->is_a_type_of(Type::Tiny::Role->new(role => 'Attean::API::Quad'))) {
			Role::Tiny->apply_roles_to_object($self, 'Attean::API::QuadIterator');
		} elsif ($type->is_a_type_of(Type::Tiny::Role->new(role => 'Attean::API::TripleOrQuad'))) {
			Role::Tiny->apply_roles_to_object($self, 'Attean::API::MixedStatementIterator');
		} elsif ($type->is_a_type_of(Type::Tiny::Role->new(role => 'Attean::API::Result'))) {
			Role::Tiny->apply_roles_to_object($self, 'Attean::API::ResultIterator');
		} elsif ($type->is_a_type_of(Type::Tiny::Role->new(role => 'Attean::API::Term'))) {
			Role::Tiny->apply_roles_to_object($self, 'Attean::API::TermIterator');
		} elsif ($type->is_a_type_of(Type::Tiny::Role->new(role => 'Attean::API::ResultOrTerm'))) {
			Role::Tiny->apply_roles_to_object($self, 'Attean::API::ResultOrTermIterator');
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
			my $err		= $type->validate($term);
			if ($err) {
				my $name	= $type->name;
				if ($type->can('role')) {
					my $role	= $type->role;
					$name		= "role $role";
				}
				die "${class} returned an element that failed conformance check for $name";
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
		
		return Attean::CodeIterator->new(
			item_type => $type,
			generator => $generator,
		);
	}

	sub grep {
		my $self	= shift;
		my $block	= shift;
		
		Attean::CodeIterator->new(
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
		
		Attean::CodeIterator->new(
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
		return Attean::ListIterator->new( values => \@data, item_type => $self->item_type );
	}
	
	sub debug {
		my $self	= shift;
		my $name	= shift // 'Iterator item';
		return $self->grep(sub { my $r = shift; say "$name: " . $r->as_string; return 1; });
	}
}

package Attean::API::RepeatableIterator 0.001 {
	use Moo::Role;
	
	requires 'reset';
	with 'Attean::API::Iterator';
	
	sub peek {
		my $self	= shift;
		my $item	= $self->next;
		$self->reset;
		return $item;
	}
}

package Attean::API::CanonicalizingBindingIterator {
	use Moo::Role;
	sub canonicalize {
		my $self	= shift;
		my $mapper	= Attean::TermMap->canonicalization_map;
		return $self->map(sub { shift->apply_map( $mapper ) });
	}
}

package Attean::API::ResultOrTermIterator 0.001 {
	use Moo::Role;
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
}

package Attean::API::TripleIterator 0.001 {
	use Moo::Role;
	with 'Attean::API::CanonicalizingBindingIterator';
	sub as_quads {
		my $self	= shift;
		my $graph	= shift;
		return $self->map(sub { $_->as_quad($graph) }, Type::Tiny::Role->new(role => 'Attean::API::Quad'));
	}
}

package Attean::API::QuadIterator 0.001 {
	use Moo::Role;
	with 'Attean::API::CanonicalizingBindingIterator';
}

package Attean::API::MixedStatementIterator 0.001 {
	use Moo::Role;
	with 'Attean::API::CanonicalizingBindingIterator';
	sub as_quads {
		my $self	= shift;
		my $graph	= shift;
		return $self->map(
			sub { $_->does('Attean::API::Quad') ? $_ : $_->as_quad($graph) },
			Type::Tiny::Role->new(role => 'Attean::API::Quad')
		);
	}
}

package Attean::API::ResultIterator 0.001 {
	use Moo::Role;
	with 'Attean::API::CanonicalizingBindingIterator';
	sub join {
		my $self	= shift;
		my $rhs		= shift;
		my @rhs		= $rhs->elements;
		my @results;
		while (my $lhs = $self->next) {
			foreach my $rhs (@rhs) {
				if (my $j = $lhs->join($rhs)) {
					push(@results, $j);
				}
			}
		}
		return Attean::ListIterator->new( values => \@results, item_type => $self->item_type);
	}
}

package Attean::API::TermIterator 0.001 {
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

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO

L<Attean::API::RepeatableIterator>

L<http://www.perlrdf.org/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
