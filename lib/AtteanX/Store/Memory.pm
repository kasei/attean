=head1 NAME

AtteanX::Store::Memory - Simple in-memory RDF store

=head1 VERSION

This document describes AtteanX::Store::Memory version 1.009

=head1 SYNOPSIS

 use AtteanX::Store::Memory;

=head1 DESCRIPTION

AtteanX::Store::Memory provides an in-memory quad-store.

=cut

use v5.14;
use warnings;

package AtteanX::Store::Memory 0.001 {
use Moose;
with 'Attean::API::MutableQuadStore';
with 'Attean::API::QuadStore';

use Encode;
use Set::Scalar;
use Data::Dumper;
use Digest::SHA;
use List::Util qw(first);
use Scalar::Util qw(refaddr reftype blessed);

my @pos_names	= qw(subject predicate object graph);

=head1 METHODS

Beyond the methods documented below, this class inherits methods from the
L<Attean::API::QuadStore> class.

=over 4

=item C<< new () >>

Returns a new memory-backed storage object.

=cut

has size => (is => 'rw', isa => 'Int', init_arg => undef, default => 0);
has statements => (is => 'rw', isa => 'ArrayRef[Attean::API::Quad]', init_arg => undef, default => sub { [] });

has subject => (is => 'ro', isa => 'HashRef[Set::Scalar]', init_arg => undef, default => sub { +{} });
has predicate => (is => 'ro', isa => 'HashRef[Set::Scalar]', init_arg => undef, default => sub { +{} });
has object => (is => 'ro', isa => 'HashRef[Set::Scalar]', init_arg => undef, default => sub { +{} });
has graph => (is => 'ro', isa => 'HashRef[Set::Scalar]', init_arg => undef, default => sub { +{} });
has graph_nodes	=> (is => 'rw', isa => 'HashRef[Attean::API::IRI]', init_arg => undef, default => sub { +{} });
has hash		=> (is => 'ro', isa => 'Digest::SHA', default => sub { Digest::SHA->new });

=item C<< get_quads ( $subject, $predicate, $object, $graph ) >>

Returns a stream object of all statements matching the specified subject,
predicate and objects. Any of the arguments may be undef to match any value.

=cut

sub get_quads {
	my $self	= shift;
	my @nodes	= @_;
	my $bound	= 0;
	my %bound;
	
	foreach my $pos (0 .. 3) {
		my $n	= $nodes[ $pos ];
		if (blessed($n) and not($n->does('Attean::API::Variable'))) {
			$bound++;
			$bound{ $pos }	= $n;
		}
	}
	
	if ($bound == 0) {
		my $i	= 0;
		my $sub	= sub {
			return unless ($i <= $#{ $self->statements });
			my $st	= $self->statements->[ $i ];
			while (not(blessed($st)) and ($i <= $#{ $self->statements })) {
				$st	= $self->statements->[ ++$i ];
			}
			$i++;
			return $st;
		};
		return Attean::CodeIterator->new( generator => $sub, item_type => Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Quad') );
	}
	
	my $match_set;
	if ($bound == 1) {
		my ($pos)		= keys %bound;
		my $name		= $pos_names[ $pos ];
		my $node	= $bound{ $pos };
		my $string	= $node->as_string;
		$match_set	= $self->$name()->{ $string };
		unless (blessed($match_set)) {
			return Attean::ListIterator->new( values => [], item_type => Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Quad') );
		}
	} else {
		my @pos		= keys %bound;
		my @names	= @pos_names[ @pos ];
		
		my @sets;
		foreach my $i (0 .. $#pos) {
			my $pos	= $pos[ $i ];
			my $node	= $bound{ $pos };
			my $string	= $node->as_string;
			my $name	= $names[$i];
			my $hash	= $self->$name();
			my $set		= $hash->{ $string };
			push(@sets, $set);
		}
		
		foreach my $s (@sets) {
			unless (blessed($s)) {
				return Attean::ListIterator->new( values => [], item_type => Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Quad') );
			}
		}
		my $i	= shift(@sets);
		while (@sets) {
			my $s	= shift(@sets);
			$i	= $i->intersection($s);
		}
		$match_set	= $i;
	}
	
	my $open	= 1;
	my @e		= $match_set->elements;
	my $sub	= sub {
		unless (scalar(@e)) {
			$open	= 0;
			return;
		}
		my $e = shift(@e);
		
		my $st	= $self->statements->[ $e ];
		return $st;
	};
	return Attean::CodeIterator->new( generator => $sub, item_type => Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Quad') );
}

=item C<< get_graphs >>

Returns an iterator over the Attean::API::Term objects comprising
the set of graphs of the stored quads.

=cut

sub get_graphs {
	my $self	= shift;
	my @ctx		= values %{ $self->graph_nodes() };
	return Attean::ListIterator->new( values => \@ctx, item_type => Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Term') );
}

=item C<< add_quad ( $quad ) >>

Adds the specified C<$quad> to the underlying model.

=cut

sub add_quad {
	my $self	= shift;
	my $st		= shift;
	
	my $count	= $self->count_quads( $st->values );
	if ($count == 0) {
		$self->size($self->size + 1);
		my $id	= scalar(@{ $self->statements });
		$self->hash->add('+' . encode_utf8($st->as_string));
		push( @{ $self->statements }, $st );
		foreach my $pos (0 .. $#pos_names) {
			my $name	= $pos_names[ $pos ];
			my $node	= $st->$name();
			my $string	= $node->as_string;
			my $set	= $self->$name()->{ $string };
			unless (blessed($set)) {
				$set	= Set::Scalar->new();
				$self->$name()->{ $string }	= $set;
			}
			$set->insert( $id );
		}
		
		my $ctx	= $st->graph;
		my $str	= $ctx->as_string;
		unless (exists $self->graph_nodes->{ $str }) {
			$self->graph_nodes->{ $str }	= $ctx;
		}
	}
	return;
}

=item C<< remove_quad ( $statement ) >>

Removes the specified C<$statement> from the underlying model.

=cut

sub remove_quad {
	my $self	= shift;
	my $st		= shift;
	
	my @nodes	= $st->values;
	my $count	= $self->count_quads( @nodes[ 0..3 ] );
	if ($count > 0) {
		$self->size( $self->size - 1 );
		my $id	= $self->_statement_id( $st->values );
		$self->hash->add('-' . encode_utf8($st->as_string));
		$self->statements->[ $id ]	= undef;
		foreach my $pos (0 .. 3) {
			my $name	= $pos_names[ $pos ];
			my $node	= $st->$name();
			my $str		= $node->as_string;
			my $set		= $self->$name()->{ $str };
			$set->delete( $id );
			if ($set->size == 0) {
				if ($pos == 3) {
					delete $self->graph_nodes->{ $str };
				}
				delete $self->$name()->{ $str };
			}
		}
	}
	return;
}

=item C<< remove_quads ( $subject, $predicate, $object, $graph ) >>

Removes the specified C<$statement> from the underlying model.

=cut

sub remove_quads {
	my $self	= shift;
	my $subj	= shift;
	my $pred	= shift;
	my $obj		= shift;
	my $graph	= shift;
	my $iter	= $self->get_quads( $subj, $pred, $obj, $graph );
	while (my $st = $iter->next) {
		$self->remove_quad( $st );
	}
}

=item C<< count_quads ( $subject, $predicate, $object, $graph ) >>

Returns a count of all the statements matching the specified subject,
predicate, object, and graph. Any of the arguments may be undef to match any
value.

=cut

sub count_quads {
	my $self	= shift;
	my @nodes	= @_[0..3];
	my $bound	= 0;
	my %bound;
	
	foreach my $pos (0 .. 3) {
		my $n	= $nodes[ $pos ];
		if (blessed($n) and not($n->does('Attean::API::Variable'))) {
			$bound++;
			$bound{ $pos }	= $n;
		}
	}
	
	if ($bound == 0) {
		return $self->size;
	} elsif ($bound == 1) {
		my ($pos)	= keys %bound;
		my $name	= $pos_names[ $pos ];
		my $set		= $self->$name()->{ $bound{ $pos }->as_string };
		unless (blessed($set)) {
			return 0;
		}
		return $set->size;
	} else {
		my @pos		= keys %bound;
		my @names	= @pos_names[ @pos ];
		my @sets;
		foreach my $i (0 .. $#names) {
			my $pos		= $pos[ $i ];
			my $setname	= $names[ $i ];
			my $data	= $self->$setname();
			
			my $node	= $bound{ $pos };
			my $str		= $node->as_string;
			my $set		= $data->{ $str };
			push( @sets, $set );
		}
		foreach my $s (@sets) {
			unless (blessed($s)) {
				return 0;
			}
		}
		my $i	= shift(@sets);
		while (@sets) {
			my $s	= shift(@sets);
			$i	= $i->intersection($s);
		}
		return $i->size;
	}
}

=item C<< etag_value_for_quads >>

If the store has the capability and knowledge to support caching, returns a
persistent token that will remain consistent as long as the store's data doesn't
change. This token is acceptable for use as an HTTP ETag.

=cut

sub etag_value_for_quads {
	my $self	= shift;
	return $self->hash->b64digest;
}

sub _statement_id {
	my $self	= shift;
	my @nodes	= @_;
	my ($subj, $pred, $obj, $graph)	= @nodes;
	
	my @pos		= (0 .. 3);
	my @names	= @pos_names[ @pos ];
	my @sets;
	foreach my $i (0 .. $#names) {
		my $pos		= $pos[ $i ];
		my $setname	= $names[ $i ];
		my $data	= $self->$setname();
		my $node	= $nodes[ $pos ];
		my $str		= $node->as_string;
		my $set		= $data->{ $str };
		push( @sets, $set );
	}
	
	foreach my $s (@sets) {
		unless (blessed($s)) {
			return -1;
		}
	}
	my $i	= shift(@sets);
	while (@sets) {
		my $s	= shift(@sets);
		$i	= $i->intersection($s);
	}
	if ($i->size == 1) {
		my ($id)	= $i->members;
		return $id;
	} else {
		return -1;
	}
}

}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/perlrdf2/issues>.

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2006-2012 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
