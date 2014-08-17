use v5.14;
use warnings;

package Attean::API::Iterator 0.001 {
	use Moose::Role;
	use Moose::Util::TypeConstraints;
	use Moose::Util qw(apply_all_roles);
	
	has 'item_type' => (is => 'ro', isa => 'Moose::Meta::TypeConstraint', required => 1);
	requires 'next';
	
	sub BUILD {}
	around 'BUILD' => sub {
		my $orig	= shift;
		my $self	= shift;
		$self->$orig(@_);
		my $type	= $self->item_type;
		if ($type->isa('Moose::Meta::TypeConstraint::Role')) {
			my $role	= $type->role;
			if ($role eq 'Attean::API::Triple') {
				apply_all_roles($self, 'Attean::API::TripleIterator');
			} elsif ($role eq 'Attean::API::Quad') {
				apply_all_roles($self, 'Attean::API::QuadIterator');
			} elsif ($role eq 'Attean::API::TripleOrQuad') {
				apply_all_roles($self, 'Attean::API::MixedStatementIterator');
			}
		}
	};
	
	sub map {
		my $self	= shift;
		my $block	= shift;
		my $type	= shift || $self->item_type;
		
		return Attean::CodeIterator->new(
			item_type => $type,
			generator => sub {
				my $item	= $self->next();
				return unless defined($item);
				local($_)	= $item;
				return $block->($item);
			}
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
	use Moose::Role;
	use Moose::Util::TypeConstraints;
	
	requires 'reset';
	with 'Attean::API::Iterator';
}

package Attean::API::TripleIterator 0.001 {
	use Moose::Role;
	sub as_quads {
		my $self	= shift;
		my $graph	= shift;
		return $self->map(sub { $_->as_quad($graph) }, Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Quad'));
	}
}

package Attean::API::QuadIterator 0.001 {
	use Moose::Role;
}

package Attean::API::MixedStatementIterator 0.001 {
	use Moose::Role;
	sub as_quads {
		my $self	= shift;
		my $graph	= shift;
		return $self->map(
			sub { $_->does('Attean::API::Quad') ? $_ : $_->as_quad($graph) },
			Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Quad')
		);
	}
}

1;
