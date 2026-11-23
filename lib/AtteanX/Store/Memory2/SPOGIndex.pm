=head1 NAME

AtteanX::Store::Memory2::SPOGIndex - A trie-based index for SPOG quad data

=head1 VERSION

This document describes AtteanX::Store::Memory2::SPOGIndex version 0.041

=head1 SYNOPSIS

 use AtteanX::Store::Memory2::SPOGIndex;

=head1 DESCRIPTION

AtteanX::Store::Memory2::SPOGIndex provides a trie-based index structure
that can be used to provide efficient access to indexed quad data.

=cut

use v5.14;
use warnings;

package AtteanX::Store::Memory2::SPOGIndex 0.041 {
use Moo;
use Type::Tiny::Role;
use Types::Standard qw(Str Int ArrayRef HashRef ConsumerOf InstanceOf);
use Encode;
use Set::Scalar;
use Digest::SHA;
use Data::Dumper;
use List::Util qw(first);
use Scalar::Util qw(refaddr reftype blessed);
use Math::Cartesian::Product;
use List::Util qw(mesh);
use Devel::STDERR::Indent qw/indent/;
use namespace::clean;

=head1 ATTRIBUTES

=over 4

=item C<< order >>

An 4-element array reference containing the order of the index. The elements
must be a permutation of C< ('subject', 'predicate', 'object', 'graph') >.

=item C<< pogs >>

A trie index in the POGS order.

=item C<< trie >>

A 4-level hash reference containing the indexed data.
The trie internal keys are the C<as_string> serializations of the terms
of an L<Attean::API::Quad> in the index order, leading to the
L<Attean::API::Quad> object at the fourth level:

  C<< $trie->{$a}{$b}{$c}{$d}	= $quad >>

=back

=head1 METHODS

=over 4

=cut

has order	=> (is => 'rw', isa => ArrayRef[Str], required => 1);
has trie	=> (is => 'rw', isa => HashRef, default => sub { +{} });

my %order	= ( subject => 0, predicate => 1, object => 2, graph => 3);

=item C<< size >>

Returns the number of quads in the index.

=cut

sub size {
	my $self	= shift;
	my $count	= 0;
	my $iter	= $self->match();
	while (my $q = $iter->next) {
		$count++;
	}
	return $count;
}

=item C<< map_to_index_order ( @values ) >>

Takes a list of values in quad (SPOG) order, and returns them in the order
appropriate for inserting into this index.

=cut

sub map_to_index_order {
	my $self	= shift;
	my @values	= @_;
	my @index	= @order{ @{ $self->order } };
	my @ordered	= @values[@index];
	pop(@ordered) while (scalar(@ordered) and not defined($ordered[-1]));
	return @ordered;
}

=item C<< map_to_quad_order ( @values ) >>

Takes a list of values in index C<order>, and returns them in quad (SPOG) order.

=cut

sub map_to_quad_order {
	my $self	= shift;
	my @index	= @_;
	my %mapping;
	@mapping{ @{ $self->order } }	= @index;
	my @ordered	= @mapping{Attean::API::Quad->variables};
	pop(@ordered) while (scalar(@ordered) and not defined($ordered[-1]));
	return @ordered;
}

=item C<< map_quad_to_index_order ( $quad ) >>

Takes a L<Attean::API::Quad>, returning its constituend values in index order.

=cut

sub map_quad_to_index_order {
	my $self	= shift;
	my $quad	= shift;
	my @nodes	= $quad->values;
	my @keys	= $quad->variables;
	my %nodes;
	@nodes{ @keys }	= @nodes;
	
	my @mapped	= @nodes{ @{ $self->order } };
	return @mapped;
}

=item C<< add_quad ( $quad ) >>

Adds the C<$quad> to the index. This method is idempotent.

=cut

sub add_quad {
	my $self	= shift;
	my $quad	= shift;
	my @nodes	= $self->map_quad_to_index_order( $quad );
	my @keys	= map { $_->as_string } @nodes;
	my ($a, $b, $c, $d)	= @keys;
	$self->trie->{$a}{$b}{$c}{$d}	= $quad;
}

=item C<< remove_quad ( $quad ) >>

Removes the C<$quad> from the index, if present. This method is idempotent.

=cut

sub remove_quad {
	my $self	= shift;
	my $quad	= shift;
	my @nodes	= $self->map_quad_to_index_order( $quad );
	my @keys	= map { $_->as_string } @nodes;
	my ($a, $b, $c, $d)	= @keys;
	my $trie	= $self->trie;
	delete $trie->{$a}{$b}{$c}{$d};
	delete $trie->{$a}{$b}{$c} unless (%{ $trie->{$a}{$b}{$c} });
	delete $trie->{$a}{$b} unless (%{ $trie->{$a}{$b} });
	delete $trie->{$a} unless (%{ $trie->{$a} });
}

=item C<< match ( $s, $p, $o, $g ) >>

Matches all quads in the index based on the pattern given in the arguments.
Each argument may be undefined, indicating a wildcard.

Returns an L<Attean::API::Iterator> of L<Attean::API::Quad> values.

=cut

sub match {
	my $self	= shift;
	my @nodes	= @_;
	my @ordered	= $self->map_to_index_order(@nodes);
	my $prefix	= 0;
	foreach my $t (@ordered) {
		unless (defined($t) or (blessed($t) and $t->does('Attean::API::Variable'))) {
			last;
		}
		$prefix++;
	}
	my @vars	= @{ $self->order };
	my $qp		= Attean::QuadPattern->new(mesh \@vars, \@ordered);
	@ordered	= @ordered[0..$prefix-1];
	my $iter	= $self->match_prefix(@ordered);
	return $iter->grep(sub {
		my $q = shift;
		return $qp->matches($q)
	});
}

=item C<< match_prefix ( @index_prefix ) >>

Matches all quads in the index based on the prefix of index-order terms given
in the arguments. All arguments must be defined. This method uses the prefix
values as a walk from the trie root, and then returns an iterator over
the values in the subtree reached at the walk ends.

Returns an L<Attean::API::Iterator> of L<Attean::API::Quad> values.

=cut

# NOTE: arguments are in index-order
sub match_prefix {
	my $self	= shift;
	my @prefix	= @_;
	my @keys;
	foreach my $t (@prefix) {
		if (blessed($t) and $t->does('Attean::API::Term')) {
			push(@keys, $t->as_string);
		} else {
			last;
		}
	}
# 	my @keys	= map { $_->as_string } @prefix;
	my $trie	= $self->trie;
	while (scalar(@keys)) {
		$trie	= $trie->{shift(@keys)};
		last unless ($trie);
	}
	if (scalar(@keys)) {
# 		warn "no data";
		return Attean::ListIterator->new( values => [], item_type => 'Attean::API::Quad' );
	}
	my @matched;
	$self->_populate_walk($trie, \@matched);
	return Attean::ListIterator->new( values => \@matched, item_type => 'Attean::API::Quad' );
}

sub _populate_walk {
	my $self	= shift;
	my $trie	= shift;
	my $list	= shift;
	if (blessed($trie) and $trie->does('Attean::API::Quad')) {
		push(@$list, $trie);
		return;
	}
	return unless (defined($trie));
	foreach my $k (sort keys %$trie) {
		my $o	= $trie->{$k};
		if (blessed($o) and $o->does('Attean::API::Quad')) {
			push(@$list, $o);
		} else {
			$self->_populate_walk($o, $list);
		}
	}
}

=item C<< remove_prefix ( @index_prefix ) >>

Removes all quads in the index matching the prefix of index-order terms given
in the arguments. All arguments must be defined. This method uses the prefix
values as a walk from the trie root, and then removes the subtree reached at
the walk ends.

Returns an L<Attean::API::Iterator> of L<Attean::API::Quad> values.

=cut

# NOTE: arguments are in index-order
sub remove_prefix {
	my $self	= shift;
	my @prefix	= @_;
	my @keys	= map { $_->as_string } @prefix;
	my $trie	= $self->trie;
	while (scalar(@keys)) {
		$trie	= $trie->{shift(@keys)};
		last unless ($trie);
	}
	if (scalar(@keys)) {
		warn "no data";
		return;
	}
	%{ $trie }	= ();
}

=item C<< head_values ( [ $trie ] ) >>

Returns an L<Attean::API::Iterator> of L<Attean::API::Term> elements
corresponding to the first element in index-order of each quad in the
index. For example, an index in GPSO order, this method will return an
iterator of graph values, while a POGS index order would result in predicate
values.

=cut

sub head_values {
	my $self	= shift;
	my $trie	= shift // $self->trie;
	my @values;
	foreach my $k (sort keys %$trie) {
		my $t		= $trie->{$k};
		my $q		= $self->get_first_quad($t);
		my @terms	= $self->map_quad_to_index_order($q);
		push(@values, $terms[0]);
	}
	return Attean::ListIterator->new( values => \@values, item_type => 'Attean::API::Term' );
}

=item C<< get_first_quad ( [ $trie ] ) >>

Returns the first quad in lexicographic order of the (sub-)trie.

=cut

sub get_first_quad {
	my $self	= shift;
	my $trie	= shift // $self->trie;
	if (ref($trie) eq 'HASH') {
		my ($k)	= sort keys %$trie;
		return $self->get_first_quad($trie->{$k});
	} else {
		return $trie;
	}
}

=item C<< debug ( [ $trie ] ) >>

Prints a serialization of the trie to STDERR.

=cut

sub debug {
	my $self	= shift;
	my $trie	= shift // $self->trie;
	my $h		= indent;
	if (ref($trie) eq 'HASH') {
		foreach my $k (sort keys %$trie) {
			warn "- $k\n";
			$self->debug($trie->{$k});
		}
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

Copyright (c) 2014--2026 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
