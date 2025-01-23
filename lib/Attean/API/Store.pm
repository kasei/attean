use v5.14;
use warnings;

=head1 NAME

Attean::API::Store - Triple/quad store role

=head1 VERSION

This document describes Attean::Store version 0.035

=head1 DESCRIPTION

The Attean::Store role is an empty role that more specialized roles conform to:

=over 4

=item * L<Attean::API::TripleStore>

=item * L<Attean::API::MutableTripleStore>

=item * L<Attean::API::ETagCacheableTripleStore>

=item * L<Attean::API::TimeCacheableTripleStore>

=item * L<Attean::API::QuadStore>

=item * L<Attean::API::MutableQuadStore>

=item * L<Attean::API::ETagCacheableQuadStore>

=item * L<Attean::API::TimeCacheableQuadStore>

=back

=cut

package Attean::API::Store 0.035 {
	use Moo::Role;
}

package Attean::API::TripleStore 0.035 {
	use Scalar::Util qw(blessed);

	use Moo::Role;

	with 'Attean::API::Store';

	requires 'get_triples';

	before 'get_triples' => sub {
		if (scalar(@_) == 2 and blessed($_[1]) and not($_[1]->does('Attean::API::TermOrVariable'))) {
			my $type	= ref($_[0]);
			die "get_triples called with a single $type argument, but expecting a list of terms/variables";
		}
	};
	
	sub count_triples {
		my $self	= shift;
		my $iter	= $self->get_triples(@_);
		my $count	= 0;
		while (my $r = $iter->next) {
			$count++;
		}
		return $count;
	}
	
	sub count_triples_estimate {
		my $self	= shift;
		return $self->count_triples(@_);
	}
	
	sub size {
		my $self	= shift;
		return $self->count_triples();
	}

	sub holds {
		my $self = shift;
		return ($self->count_triples_estimate(@_) > 0)
	}

}

package Attean::API::MutableTripleStore 0.035 {
	use Moo::Role;
	with 'Attean::API::TripleStore';
	
	requires 'add_triple';
	requires 'remove_triple';

	before 'add_triple' => sub {
		my $self	= shift;
		my $quad	= shift;
		unless ($quad->is_ground) {
			die "Cannot add a non-ground triple (with variables) to a model";
		}
	};
}

package Attean::API::ETagCacheableTripleStore 0.035 {
	use Moo::Role;
	with 'Attean::API::TripleStore';
	
	requires 'etag_value_for_triples';
}

package Attean::API::TimeCacheableTripleStore 0.035 {
	use Moo::Role;
	with 'Attean::API::TripleStore';
	
	requires 'mtime_for_triples';
}

package Attean::API::QuadStore 0.035 {
	use Scalar::Util qw(blessed);

	use Moo::Role;

	with 'Attean::API::Store';

	requires 'get_quads';

	before 'get_quads' => sub {
		if (scalar(@_) == 2 and blessed($_[1]) and not($_[1]->does('Attean::API::TermOrVariable'))) {
			my $type	= ref($_[0]);
			die "get_quads called with a single $type argument, but expecting a list of terms/variables";
		}
	};
	
	sub count_quads {
		my $self	= shift;
		my $iter	= $self->get_quads(@_);
		my $count	= 0;
		while (my $r = $iter->next) {
			$count++;
		}
		return $count;
	}
	
	sub count_quads_estimate {
		my $self	= shift;
		return $self->count_quads(@_);
	}

	sub holds {
	  my $self = shift;
	  return ($self->count_quads_estimate(@_) > 0)
	}

	sub get_graphs {
		my $self	= shift;
		my $iter	= $self->get_quads(@_);
		my %graphs;
		while (my $r = $iter->next) {
			my $g	= $r->graph;
			$graphs{ $g->as_string }++;
		}
		return Attean::ListIterator->new( values => [map { Attean::IRI->new($_) } keys %graphs], item_type => 'Attean::API::Term' );
	}
	
	sub size {
		my $self	= shift;
		return $self->count_quads();
	}
}

package Attean::API::MutableQuadStore 0.035 {
	use Role::Tiny ();
	use Moo::Role;
	use Type::Tiny::Role;
	with 'Attean::API::QuadStore';
	
	requires 'add_quad';
	requires 'remove_quad';
	requires 'create_graph';
	requires 'drop_graph';
	requires 'clear_graph';

	before 'add_quad' => sub {
		my $self	= shift;
		my $quad	= shift;
		unless ($quad->is_ground) {
			die "Cannot add a non-ground quad (with variables) to a store";
		}
	};

	sub add_iter {
		my $self	= shift;
		my $iter	= shift;
		my $type	= $iter->item_type;
		use Data::Dumper;
		die "Iterator type $type isn't quads" unless (Role::Tiny::does_role($type, 'Attean::API::Quad'));
		while (my $q = $iter->next) {
			$self->add_quad($q);
		}
	}
}

package Attean::API::ETagCacheableQuadStore 0.035 {
	use Moo::Role;

	with 'Attean::API::QuadStore';
	
	requires 'etag_value_for_quads';
}

package Attean::API::TimeCacheableQuadStore 0.035 {
	use Moo::Role;

	with 'Attean::API::QuadStore';
	
	requires 'mtime_for_quads';
}

package Attean::API::BulkUpdatableStore 0.035 {
	use Moo::Role;
	
	requires 'begin_bulk_updates';
	requires 'end_bulk_updates';
}

package Attean::API::RDFStarStore 0.035 {
	use Moo::Role;
	
	with 'Attean::API::Store';
}

1;

__END__

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
