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

The L<Attean::API::Model> role provides default implementations of the
following methods:

=over 4

=item C<< get_bindings( $subject, $predicate, $object, $graph ) >>

=item C<< count_quads( $subject, $predicate, $object, $graph ) >>

=item C<< get_graphs >>

=item C<< get_list( $graph, $head ) >>

=item C<< get_sequence( $head ) >>

=item C<< subjects( $predicate, $object, $graph ) >>

=item C<< predicates( $subject, $object, $graph ) >>

=item C<< objects( $subject, $predicate, $graph ) >>

=item C<< graphs( $subject, $predicate, $object ) >>

=cut

use Attean::API::Binding;

package Attean::API::Model 0.001 {
	use Moo::Role;
	use Sub::Install;
	use Sub::Name;
	use Type::Tiny::Role;
	use URI::Namespace;
	use Scalar::Util qw(blessed);
	use List::MoreUtils qw(uniq);
	
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
		}, Type::Tiny::Role->new(role => 'Attean::API::Result'));
	}
	
	requires 'count_quads';
	requires 'get_graphs';
	
# 	sub count_quads {
# 		my $self	= shift;
# 		my $iter	= $self->get_quads(@_);
# 		my $count	= 0;
# 		while (my $r = $iter->next) {
# 			$count++;
# 		}
# 		return $count;
# 	}
# 	
# 	sub get_graphs {
# 		my $self	= shift;
# 		my $iter	= $self->get_quads(@_);
# 		my %graphs;
# 		return $self->graphs->grep(sub { not($graphs{ shift->as_string }++) });
# 	}
	
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
		return Attean::ListIterator->new(values => \@elements, item_type => Type::Tiny::Role->new(role => 'Attean::API::Term') );
	}

	sub get_sequence {
		my $self	= shift;
		my $head	= shift;
		my $rdf		= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
		my @elements;
		my $i		= 1;
		while (1) {
			my $term	= Attean::IRI->new("${rdf}_$i");
			my @elem	= $self->objects( $head, $term )->elements;
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
		return Attean::ListIterator->new(values => \@elements, item_type => Type::Tiny::Role->new(role => 'Attean::API::Term') );
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
					Type::Tiny::Role->new(role => 'Attean::API::Term'),
				);
				return $nodes;
			};
			Sub::Install::install_sub({
				code	=> subname("${method}s", $code),
				as		=> "${method}s"
			});
		}
	}
}


package Attean::API::MutableModel 0.001 {
	use Moo::Role;
	
	requires 'add_quad';
	requires 'remove_quad';
	requires 'create_graph';
	requires 'drop_graph';
	requires 'clear_graph';
	requires 'add_iter';
	
	with 'Attean::API::Model';
	
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
