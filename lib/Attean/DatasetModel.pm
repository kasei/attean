use v5.14;
use warnings;

=head1 NAME

Attean::DatasetModel - RDF model that restricts the dataset of a sub-model

=head1 VERSION

This document describes Attean::DatasetModel version 0.039

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $model = Attean::DatasetModel->new( model => $model, graphs => \@named_graphs );

=head1 DESCRIPTION

The Attean::DatasetModel class represents a model that is backed by a sub-model.
It restricts the available named graphs of the sub-model to only those
configured during construction.

This class conforms to the L<Attean::API::Model> role.

The Attean::DatasetModel constructor requires two named arguments:

=over 4

=item model

A L<Attean::API::Model> object representing the sub-model.

=item graphs

An array reference of L<Attean::API::IRI> objects, enumerating the acceptable
named graphs that can be queried.

=back

=head1 METHODS

=over 4

=cut

package Attean::DatasetModel 0.039 {
	use Moo;
	use Scalar::Util qw(blessed reftype);
	use Types::Standard qw(ConsumerOf HashRef);
	use namespace::clean;

	has 'model'	=> (
		is => 'ro',
		does => 'Attean::API::Model',
		required => 1,
		handles	=> [qw()],
	);
	
	has 'dataset_graphs' => (
		is => 'ro',
		isa => HashRef[ConsumerOf['Attean::API::IRI']],
		default => sub { +{} }
	);

	around 'BUILDARGS' => sub {
		my $orig 	= shift;
		my $class	= shift;
		my %args	= @_;
		
		my $graphs	= delete $args{ 'graphs' };
		my %graphs	= map { $_->value => $_ } grep { blessed($_) and $_->does('Attean::API::IRI') } @{ $graphs || [] };
		$args{ 'dataset_graphs' }	= \%graphs;
		
		unless (exists $args{ 'model' }) {
			$args{ 'model' }	= Attean->temporary_model;
		}
		
		return $class->$orig(%args);
	};
	
=item C<< get_quads ( $subject, $predicate, $object, $graph ) >>

Returns an L<Attean::API::Iterator> for quads in the model that match the
supplied C<< $subject >>, C<< $predicate >>, C<< $object >>, and C<< $graph >>.
Any of these terms may be undefined or a L<Attean::API::Variable> object, in
which case that term will be considered as a wildcard for the purposes of
matching.

The returned iterator conforms to both L<Attean::API::Iterator> and
L<Attean::API::QuadIterator>.

=cut

	sub get_quads {
		my $self	= shift;
		my @nodes	= @_[0..3];
		my $quad	= $nodes[3];
		if (defined($quad) and reftype($quad) eq 'ARRAY') {
			my @graphs	= @$quad;
			my @ok		= grep { exists $self->dataset_graphs->{$_->value} } @graphs;
			return $self->model->get_quads( @nodes[0..2], \@ok );
		} elsif (blessed($quad) and not($quad->isa('Attean::API::Variable'))) {
			if (exists($self->dataset_graphs->{ $quad->value })) {
				return $self->model->get_statements( @_ );
			} else {
				return Attean::ListIterator->new( values => [], item_type => 'Attean::API::Quad' );
			}
		} else {
			my @iters;
			if (not(defined($quad)) or $quad->isa('Attean::API::Variable')) {
				foreach my $g (values %{ $self->dataset_graphs }) {
					push(@iters, $self->model->get_quads( @nodes[0..2], $g ));
				}
			}
			my %seen;
			my $code	= sub {
				while (1) {
					return unless scalar(@iters);
					my $st	= $iters[0]->next;
					if ($st) {
						if ($seen{ $st->as_string }++) {
							next;
						}
						return $st;
					} else {
						shift(@iters);
					}
				}
			};
			return Attean::IteratorSequence->new( iterators => \@iters, item_type => 'Attean::API::Quad' );
		}
	}

	sub count_quads {
		my $self	= shift;
		my $iter	= $self->get_quads(@_);
		my $count	= 0;
		while ($iter->next) {
			$count++;
		}
		return $count;
	}
	
	sub count_quads_estimate {
		my $self	= shift;
		return $self->count_quads(@_);
	}
	
	sub get_graphs {
		my $self	= shift;
		my @graphs	= values %{ $self->dataset_graphs };
		return Attean::ListIterator->new( values => \@graphs, item_type => 'Attean::API::Term' );
	}
	
	sub size {
		my $self	= shift;
		return $self->count_quads();
	}
	
	with 'Attean::API::Model';
}

package Attean::MutableDatasetModel 0.039 {
	use Moo;
	use Scalar::Util qw(blessed reftype);
	use Types::Standard qw(ConsumerOf HashRef);
	use namespace::clean;
	
	extends 'Attean::DatasetModel';

	has 'model'	=> (
		is => 'ro',
		does => 'Attean::API::MutableModel',
		required => 1,
		handles	=> [qw(remove_quad)],
	);
	
	sub add_quad {
		my $self	= shift;
		my $quad	= shift;
		
		my $g		= $quad->graph;
		$self->model->add_quad($quad);
		$self->dataset_graphs->{ $g->value }	= $g;
	}
	
	sub create_graph {
		my $self	= shift;
		my $g		= shift;
		
		$self->dataset_graphs->{ $g->value }	= $g;
	}
	
	sub drop_graph {
		my $self	= shift;
		my $g		= shift;
		
		$self->model->drop_graph($g);
		delete $self->dataset_graphs->{ $g->value };
	}
	
	sub clear_graph {
		my $self	= shift;
		my $g		= shift;
		
		$self->model->clear_graph($g);
	}
	
	with 'Attean::API::MutableModel';
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2022 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
