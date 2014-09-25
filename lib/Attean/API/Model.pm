use v5.14;
use warnings;

=head1 NAME

Attean::API::Model - RDF Model

=head1 VERSION

This document describes Attean::API::Model version 0.000

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

Returns an L<Attean::API::Iterator> for quads in the model that match the
supplied C<< $subject >>, C<< $predicate >>, C<< $object >>, and C<< $graph >>.

Any of these terms may be:

* An L<Attean::API::Term> object, in which case matching is equality-based

* A L<Attean::API::Variable> object or C<< undef >>, in which case that term
will be considered as a wildcard for the purposes of matching

* An ARRAY reference of L<Attean::API::Term> objects, in which case the
matching will be equality-based on the disjunction of the supplied terms

The returned iterator conforms to both L<Attean::API::Iterator> and
L<Attean::API::QuadIterator>.

=item C<< count_quads( $subject, $predicate, $object, $graph ) >>

Returns the number of quads in the model matching the supplied pattern
(using the same matching semantics as C<< get_quads >>).

=item C<< get_graphs >>

Returns an L<Attean::API::Iterator> of distinct L<Attean::API::Term> objects
that are used in the graph position of quads in the model.

=back

=head1 METHODS

The L<Attean::API::Model> role provides default implementations of the
following methods:

=over 4

=item C<< get_bindings( $subject, $predicate, $object, $graph ) >>

Returns an L<Attean::API::Iterator> of L<Attean::API::Result> objects
corresponding to quads in the model matching the supplied pattern. For each
L<Attean::API::Variable> in the pattern list, a mapping will be present in the
corresponding result object. For example,
C<< $model->get_bindings( variable('s') ) >> will return an iterator of results
containing just a mapping from C<< 's' >> to subjects of all quads in the
model.

=item C<< get_list( $graph, $head ) >>

Returns an L<Attean::API::Iterator> of L<Attean::API::Term> objects that are
members of the rdf:List with the specified C<< $head >> (and matching
restricted to only the specified C<< $graph >>).

=item C<< get_sequence( $graph, $head ) >>

Returns an L<Attean::API::Iterator> of L<Attean::API::Term> objects that are
members of the rdf:Sequence with the specified C<< $head >> (and matching
restricted to only the specified C<< $graph >>).

=item C<< subjects( $predicate, $object, $graph ) >>

Returns an L<Attean::API::Iterator> of L<Attean::API::Term> objects of all
subjects of quads matching the supplied pattern (using the same matching
semantics as C<< get_quads >>).

=item C<< predicates( $subject, $object, $graph ) >>

Returns an L<Attean::API::Iterator> of L<Attean::API::Term> objects of all
predicates of quads matching the supplied pattern (using the same matching
semantics as C<< get_quads >> with an C<< undef >> predicate).

=item C<< objects( $subject, $predicate, $graph ) >>

Returns an L<Attean::API::Iterator> of L<Attean::API::Term> objects of all
objects of quads matching the supplied pattern (using the same matching
semantics as C<< get_quads >> with an C<< undef >> object).

=item C<< graphs( $subject, $predicate, $object ) >>

Returns an L<Attean::API::Iterator> of L<Attean::API::Term> objects of all
graphs of quads matching the supplied pattern (using the same matching
semantics as C<< get_quads >> with an C<< undef >> graph).

=item C<< graph_nodes( $graph ) >>

Returns an L<Attean::API::Iterator> of L<Attean::API::Term> objects of unique
subjects and objects present in the specified C<< $graph >>.

=cut

use Attean::API::Binding;

package Attean::API::Model 0.001 {
	use Moo::Role;
	use Sub::Install;
	use Sub::Name;
	use URI::Namespace;
	use Scalar::Util qw(blessed);
	use List::MoreUtils qw(uniq);
	use namespace::clean;
	
	# get_quads($s, $p, $o, $g)
	# or:
	# get_quads([$s1, $s2, ...], \@p, \@o, \@g)
	requires 'get_quads';
	
	sub get_bindings {
		my $self	= shift;
		my @nodes	= @_;
		my @pos		= Attean::API::Quad->variables;
		my %vars;
		foreach my $i (0 .. $#nodes) {
			my $n	= $nodes[$i];
			if (blessed($n) and $n->isa('Attean::Variable')) {
				my $name	= $n->value;
				$vars{ $pos[ $i ] }	= $name;
			}
		}
		my $quads		= $self->get_quads(@nodes);
		return $quads->map(sub {
			my $q			= shift;
			return unless blessed($q);
			my %bindings	= map { $vars{$_} => $q->$_() } (keys %vars);
			return Attean::Result->new( bindings => \%bindings );
		}, 'Attean::API::Result');
	}
	
	requires 'count_quads';
	requires 'get_graphs';
	
	sub get_list {
		my $self	= shift;
		die "get_list called without a graph name" unless (scalar(@_));
		my $graph	= shift;
		die "get_list called without a list head" unless (scalar(@_));
		my $head	= shift;
		my $rdf_first	= Attean::IRI->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#first');
		my $rdf_rest	= Attean::IRI->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#rest');
		my $rdf_nil		= Attean::IRI->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil');
		my @elements;
		my %seen;
		while (blessed($head) and not($head->does('Attean::API::IRI') and $head->value eq $rdf_nil->value)) {
			if ($seen{ $head->as_string }++) {
				die "Loop found during rdf:List traversal";
			}
			my @n		= $self->objects( $head, $rdf_first )->elements;
			if (scalar(@n) != 1) {
				die "Invalid structure found during rdf:List traversal";
			}
			push(@elements, @n);
			($head)	= $self->objects( $head, $rdf_rest )->elements;
		}
		return Attean::ListIterator->new(values => \@elements, item_type => 'Attean::API::Term' );
	}

	sub get_sequence {
		my $self	= shift;
		my $graph	= shift;
		my $head	= shift;
		my $rdf		= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
		my @elements;
		my $i		= 1;
		while (1) {
			my $term	= Attean::IRI->new("${rdf}_$i");
			my @elem	= $self->objects( $head, $term, $graph )->elements;
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
		return Attean::ListIterator->new(values => \@elements, item_type => 'Attean::API::Term' );
	}

	{
		my @pos		= Attean::API::Quad->variables;
		my %pos		= map { $pos[$_] => $_ } (0 .. $#pos);
		for my $method (@pos) {
			my $code	= sub {
				my $self	= shift;
				my @nodes	= @_;
				$#nodes		= 3;
				splice(@nodes, $pos{$method}, 0, undef);
				my $iter	= $self->get_quads(@nodes);
				my $nodes	= $iter->map(
					sub { $_->$method() },
					'Attean::API::Term',
				);
				return $nodes;
			};
			Sub::Install::install_sub({
				code	=> subname("${method}s", $code),
				as		=> "${method}s"
			});
		}
	}

	sub graph_nodes {
		my $self	= shift;
		my $graph	= shift;
		my $s		= $self->subjects(undef, undef, $graph);
		my $o		= $self->objects(undef, undef, $graph);
		my $union	= Attean::IteratorSequence->new( iterators => [$s, $o], item_type => 'Attean::API::Term' );
		my %seen;
		return $union->grep(sub {not($seen{shift->as_string}++)});
	}
}


package Attean::API::MutableModel 0.001 {
	use Moo::Role;
	use Encode qw(encode);
	use namespace::clean;
	
	requires 'add_quad';
	requires 'remove_quad';
	requires 'create_graph';
	requires 'drop_graph';
	requires 'clear_graph';
	requires 'add_iter';
	
	with 'Attean::API::Model';
	
	sub load_triples {
		my $self	= shift;
		my $format	= shift;
		my $class	= Attean->get_parser($format) || die "Failed to load parser for '$format'";
		my $parser	= $class->new() || die "Failed to construct parser for '$format'";
		while (scalar(@_)) {
			my ($graph, $string)	= splice(@_, 0, 2);
			my $iter	= $parser->parse_iter_from_bytes(encode('UTF-8', $string, Encode::FB_CROAK));
			my $quads	= $iter->as_quads($graph);
			$self->add_iter($quads);
		}
	}
	
	sub add_iter {
		my $self	= shift;
		my $iter	= shift;
		my $type	= $iter->item_type;
		die "Iterator type $type isn't quads" unless (Role::Tiny::does_role($type, 'Attean::API::Quad'));
		while (my $q = $iter->next) {
			$self->add_quad($q);
		}
	}

	sub add_list {
		my $self	= shift;
		die "add_list called without a graph name" unless (scalar(@_));
		my $graph	= shift;
		my @elements	= @_;
		my $rdf_first	= Attean::IRI->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#first');
		my $rdf_rest	= Attean::IRI->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#rest');
		my $rdf_nil		= Attean::IRI->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil');
		if (scalar(@elements) == 0) {
			return $rdf_nil;
		} else {
			my $head		= Attean::Blank->new();
			my $node		= shift(@elements);
			my $rest		= $self->add_list($graph, @elements);
			$self->add_quad( Attean::Quad->new($head, $rdf_first, $node, $graph) );
			$self->add_quad( Attean::Quad->new($head, $rdf_rest, $rest, $graph) );
			return $head;
		}
	}
}


package Attean::API::CacheableModel 0.001 {
	use Moo::Role;
	
	requires 'etag_value_for_quads';
}


package Attean::API::BulkUpdatableModel 0.001 {
	use Moo::Role;
	
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
