use v5.14;
use warnings;

package AtteanX::Store::Memory2::SPOGIndex 0.039 {
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

has order	=> (is => 'rw', isa => ArrayRef[Str], required => 1);
has trie	=> (is => 'rw', isa => HashRef, default => sub { +{} });

my %order	= ( subject => 0, predicate => 1, object => 2, graph => 3);

sub size {
	my $self	= shift;
	my $count	= 0;
	my $iter	= $self->match();
	while (my $q = $iter->next) {
		$count++;
	}
	return $count;
}

sub map_to_index_order {
	my $self	= shift;
	my @values	= @_;
	my @index	= @order{ @{ $self->order } };
	my @ordered	= @values[@index];
	pop(@ordered) while (scalar(@ordered) and not defined($ordered[-1]));
	return @ordered;
}

sub map_to_quad_order {
	my $self	= shift;
	my @index	= @_;
	my %mapping;
	@mapping{ @{ $self->order } }	= @index;
	my @ordered	= @mapping{Attean::API::Quad->variables};
	pop(@ordered) while (scalar(@ordered) and not defined($ordered[-1]));
	return @ordered;
}

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

sub add_quad {
	my $self	= shift;
	my $quad	= shift;
	my @nodes	= $self->map_quad_to_index_order( $quad );
	my @keys	= map { $_->as_string } @nodes;
	my ($a, $b, $c, $d)	= @keys;
	$self->trie->{$a}{$b}{$c}{$d}	= $quad;
}

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
