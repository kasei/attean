use v5.14;
use warnings;

=head1 NAME

Attean::API::Model - RDF Model

=head1 VERSION

This document describes Attean::API::Model version 0.001

=head1 DESCRIPTION

The Attean::API::Model role defines a common API for all RDF models to conform
to. It is provides a consistent interface for probing, counting, and retrieving
L<Attean::API::Quad|Attean::API::Binding>s matching a pattern, as well as
related functionality such as enumerating the graph names, and extracting
structured data from the models' quads.

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Model> role:

=over 4

=item C<< get_quads( $subject, $predicate, $object, $graph ) >>

=back

=head1 METHODS

The L<Attean::API::Model> role role provides default implementations of the
following methods:

=over 4

=item C<< get_bindings( $subject, $predicate, $object, $graph ) >>

=item C<< count_quads( $subject, $predicate, $object, $graph ) >>

=item C<< get_graphs >>

=item C<< get_list( $head ) >>

=item C<< get_sequence( $head ) >>

=item C<< subjects( $predicate, $object, $graph ) >>

=item C<< predicates( $subject, $object, $graph ) >>

=item C<< objects( $subject, $predicate, $graph ) >>

=item C<< graphs( $subject, $predicate, $object ) >>

=cut

package Attean::API::Model 0.001 {
	use Moose::Role;
	use URI::Namespace;
	use Scalar::Util qw(blessed);
	use List::MoreUtils qw(uniq);
	
	# get_quads($s, $p, $o, $g)
	# or:
	# get_quads([$s1, $s2, ...], \@p, \@o, \@g)
	requires 'get_quads';
	
	sub get_bindings {
		my $self	= shift;
		my @vars	= uniq map { $_->value } grep { blessed($_) and $_->isa('Attean::Variable') } @_;
		my $iter	= $self->get_quads(@_);
		# TODO
		die 'unimplemented';
	}
	
	sub count_quads {
		my $self	= shift;
		my $iter	= $self->get_quads(@_);
		my $count	= 0;
		while (my $r = $iter->next) {
			$count++;
		}
		return $count;
	}
	
	sub get_graphs {
		my $self	= shift;
		my $iter	= $self->get_quads(@_);
		my %graphs;
		while (my $r = $iter->next) {
			my $g	= $r->graph;
			$graphs{ $g->as_string }++;
		}
		return keys %graphs;
	}
	
	sub get_list {
		my $self	= shift;
		my $head	= shift;
		my $rdf		= URI::Namespace->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
		my @elements;
		my %seen;
		while (blessed($head) and not($head->does('Attean::API::IRI') and $head->value eq $rdf->nil->value)) {
			if ($seen{ $head->as_string }++) {
				die "Loop found during rdf:List traversal";
			}
			my @n		= $self->objects( $head, $rdf->first )->elements;
			if (scalar(@n) != 1) {
				die "Invalid structure found during rdf:List traversal";
			}
			push(@elements, @n);
			($head)	= $self->objects( $head, $rdf->rest )->elements;
		}
		return Attean::ListIterator->new(values => \@elements, item_type => Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Term') );
	}

	sub get_sequence {
		my $self	= shift;
		my $head	= shift;
		my $rdf		= URI::Namespace->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
		my @elements;
		my $i		= 1;
		while (1) {
			my $method	= '_' . $i;
			my @elem	= $self->objects( $head, $rdf->$method() )->elements;
			last unless (scalar(@elem));
			if (scalar(@elem) > 1) {
				my $count	= scalar(@elem);
				die "Invalid structure found during rdf:Seq access: $count elements found for element $i";
			}
			my $elem	= $elem[0];
			last unless (blessed($elem));
			push(@elements, $elem);
			$i++;
		}
		return Attean::ListIterator->new(values => \@elements, item_type => Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Term') );
	}

	{
		my $meta	= Moose::Util::find_meta(__PACKAGE__);
		my @pos		= qw(subject predicate object graph);
		my %pos		= map { $pos[$_] => $_ } (0 .. $#pos);
		for my $method (@pos) {
			$meta->add_method("${method}s", sub {
				my $self	= shift;
				my @nodes	= @_;
				$#nodes		= 3;
				splice(@nodes, $pos{$method}, 0, undef);
				my $iter	= $self->get_quads(@nodes);
				my $nodes	= $iter->map(
					sub { $_->$method() },
					Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Term'),
				);
				return $nodes;
			});
		}
	}
}

package Attean::API::MutableModel 0.001 {
	use Moose::Role;
	
	requires 'add_quad';
	requires 'remove_quad';
	requires 'create_graph';
	requires 'drop_graph';
	requires 'clear_graph';
	requires 'add_iter';
	
	with 'Attean::API::Model';
	
	sub add_list {
		my $self	= shift;
		my $graph	= shift;
		my @elements	= @_;
		my $rdf		= URI::Namespace->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
		if (scalar(@elements) == 0) {
			return $rdf->nil;
		} else {
			my $head		= Attean::Blank->new();
			my $node		= shift(@elements);
			my $rest		= $self->add_list( @elements );
			$self->add_quad( Attean::Quad->new($head, $rdf->first, $node, $graph) );
			$self->add_quad( Attean::Quad->new($head, $rdf->rest, $rest, $graph) );
			return $head;
		}
	}
}

package Attean::API::CacheableModel 0.001 {
	use Moose::Role;
	
	requires 'etag_value_for_quads';
}

package Attean::API::BulkUpdatableModel 0.001 {
	use Moose::Role;
	
	with 'Attean::API::MutableModel';
	
	requires 'begin_bulk_updates';
	requires 'end_bulk_updates';
	
	# End bulk updates the moment a read operation is performed...
	before [qw(get_quads get_bindings count_quads get_graphs subject predicate object graph)] => sub {
		my $self	= shift;
		$self->end_bulk_updates();
	};
}

1;
