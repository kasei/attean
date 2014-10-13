use v5.14;
use warnings;

=head1 NAME

Attean::API::Store - Triple/quad store role

=head1 VERSION

This document describes Attean::Store version 0.001_01

=head1 DESCRIPTION

The Attean::Store role is an empty role that more specialized roles conform to:

=over 4

=item * L<Attean::API::TripleStore>

=item * L<Attean::API::MutableTripleStore>

=item * L<Attean::API::CacheableTripleStore>

=item * L<Attean::API::QuadStore>

=item * L<Attean::API::MutableQuadStore>

=item * L<Attean::API::CacheableQuadStore>

=back

=cut

package Attean::API::Store 0.001 {
	use Moo::Role;
}

package Attean::API::TripleStore 0.001 {
	use Moo::Role;
	with 'Attean::API::Store';

	requires 'get_triples';

	sub count_triples {
		my $self	= shift;
		my $iter	= $self->get_triples(@_);
		my $count	= 0;
		while (my $r = $iter->next) {
			$count++;
		}
		return $count;
	}
	
	sub size {
		my $self	= shift;
		return $self->count_triples();
	}
}

package Attean::API::MutableTripleStore 0.001 {
	use Moo::Role;
	with 'Attean::API::TripleStore';
	
	requires 'add_triple';
	requires 'remove_triple';
}

package Attean::API::CacheableTripleStore 0.001 {
	use Moo::Role;
	with 'Attean::API::TripleStore';
	
	requires 'etag_value_for_triples';
}

package Attean::API::QuadStore 0.001 {
	use Moo::Role;
	with 'Attean::API::Store';

	requires 'get_quads';

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
		return Attean::ListIterator->new( values => [map { Attean::IRI->new($_) } keys %graphs], item_type => 'Attean::API::Term' );
	}
	
	sub size {
		my $self	= shift;
		return $self->count_quads();
	}
}

package Attean::API::MutableQuadStore 0.001 {
	use Moo::Role;
	use Type::Tiny::Role;
	with 'Attean::API::QuadStore';
	
	requires 'add_quad';
	requires 'remove_quad';
	requires 'create_graph';
	requires 'drop_graph';
	requires 'clear_graph';
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

package Attean::API::CacheableQuadStore 0.001 {
	use Moo::Role;
	with 'Attean::API::QuadStore';
	
	requires 'etag_value_for_quads';
}

1;

__END__

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
