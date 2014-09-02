use v5.14;
use warnings;

=head1 NAME

Attean::API::Iterator - Typed iterator

=head1 VERSION

This document describes Attean::API::Iterator version 0.001

=head1 DESCRIPTION

The Attean::API::Iterator role defines a common API for typed iterators.

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Iterator> role:

=over 4

=item C<< next >>

=back

=head1 METHODS

The L<Attean::API::Iterator> role role provides default implementations of the
following methods:

=over 4

=item C<< elements >>

=item C<< map( \&mapper, $result_type ) >>

=item C<< grep( \&filter ) >>

=cut

use Type::Tiny::Role;

package Attean::API::Iterator 0.001 {
	use Moo::Role;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Object InstanceOf);
	use Moose::Util qw(apply_all_roles);
	
	has 'item_type' => (is => 'ro', isa => InstanceOf['Type::Tiny'], required => 1);
	requires 'next';
	
	sub BUILD {}
	around 'BUILD' => sub {
		my $orig	= shift;
		my $self	= shift;
		$self->$orig(@_);
		my $type	= $self->item_type;
		if ($type->can('role')) {	# TODO: simplify
			my $role	= $type->role;
			return unless defined($role);
			if ($role eq 'Attean::API::Triple') {
				apply_all_roles($self, 'Attean::API::TripleIterator');
			} elsif ($role eq 'Attean::API::Quad') {
				apply_all_roles($self, 'Attean::API::QuadIterator');
			} elsif ($role eq 'Attean::API::TripleOrQuad') {
				apply_all_roles($self, 'Attean::API::MixedStatementIterator');
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
			$generator	= sub {
				my $item	= $self->next();
				return unless defined($item);
				local($_)	= $item;
				return $block->($item);
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
		my $type	= $self->item_type;
		
		Attean::CodeIterator->new(
			item_type => $type,
			generator => sub {
				while (1) {
					my $item	= $self->next();
					return unless defined($item);
					local($_)	= $item;
					if ($block->($item)) {
						return $item;
					}
				}
			}
		);
	}

}

package Attean::API::RepeatableIterator 0.001 {
	use Moo::Role;
	
	requires 'reset';
	with 'Attean::API::Iterator';
}

package Attean::API::TripleIterator 0.001 {
	use Moo::Role;
	sub as_quads {
		my $self	= shift;
		my $graph	= shift;
		return $self->map(sub { $_->as_quad($graph) }, Type::Tiny::Role->new(role => 'Attean::API::Quad'));
	}
}

package Attean::API::QuadIterator 0.001 {
	use Moo::Role;
}

package Attean::API::MixedStatementIterator 0.001 {
	use Moo::Role;
	sub as_quads {
		my $self	= shift;
		my $graph	= shift;
		return $self->map(
			sub { $_->does('Attean::API::Quad') ? $_ : $_->as_quad($graph) },
			Type::Tiny::Role->new(role => 'Attean::API::Quad')
		);
	}
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
