use v5.14;
use warnings;

=encoding utf8

=head1 NAME

AtteanX::API::JoinRotatingPlanner - Query planning role to produce alternative join plans

=head1 VERSION

This document describes AtteanX::API::JoinRotatingPlanner version 0.023

=head1 DESCRIPTION

The AtteanX::API::JoinRotatingPlanner role, when used with L<Attean::QueryPlanner>,
produces alternatives for join query plans. Specifically, joins of the form
(A⋈B)⋈C are rotated to A⋈(B⋈C), with the ability to coalesce B⋈C (e.g. for
adjacent BGPs).

=head1 REQUIRED METHODS

=over 4

=item C<< allow_join_rotation( $join_plan ) >>

Returns true if join rotation should be attempted on the given join plan.

=item C<< coalesce_rotated_join( $join_plan ) >>

Given a L<Attean::API::Plan::Join> plan C<< $join_plan >>, returns a list of
equivalent plans. This is useful when the join can be reduced to a more
fundamental plan type, such as merging two adjacent BGP plans into a single
plan.

=cut

package AtteanX::API::JoinRotatingPlanner 0.023 {
	# Rotate joins like (A⋈B)⋈C to A⋈(B⋈C), with the ability to coalesce B⋈C (e.g. for adjacent BGPs)
	use Attean;
	use Attean::RDF;

	use Moo::Role;

	requires 'coalesce_rotated_join';
	requires 'allow_join_rotation';
	
	sub allow_join_rotation {
		return 1;
	}
	
	sub coalesce_rotated_join {
		my $self	= shift;
		my $plan	= shift;
		return $plan;
	}
	
	around 'join_plans' => sub {
		my $orig			= shift;
		my $self			= shift;
		my $model			= shift;
		my $active_graphs	= shift;
		my $default_graphs	= shift;
		my $lplans			= shift;
		my $rplans			= shift;
		my $type			= shift;
		my @plans			= $orig->($self, $model, $active_graphs, $default_graphs, $lplans, $rplans, $type, @_);
		if ($type eq 'inner') {
			my @rotated;
			foreach my $p (@plans) {
				if ($self->allow_join_rotation($p)) {
					my ($lhs, $rhs)	= @{ $p->children };
					if ($lhs->does('Attean::API::Plan::Join')) {
						my ($a, $b)	= @{ $lhs->children };
						my $c		= $rhs;
						# (A⋈B)⋈C -> A⋈(B⋈C)
						foreach my $q ($orig->($self, $model, $active_graphs, $default_graphs, [$b], [$c], $type, @_)) {
							push(@rotated, $orig->($self, $model, $active_graphs, $default_graphs, [$a], [$self->coalesce_rotated_join($q)], $type, @_))
						}
					} elsif ($rhs->does('Attean::API::Plan::Join')) {
						my $a		= $lhs;
						my ($b, $c)	= @{ $rhs->children };
						# A⋈(B⋈C) -> (A⋈B)⋈C
						foreach my $q ($orig->($self, $model, $active_graphs, $default_graphs, [$a], [$b], $type, @_)) {
							push(@rotated, $orig->($self, $model, $active_graphs, $default_graphs, [$self->coalesce_rotated_join($q)], [$c], $type, @_));
						}
					}
				}
				push(@rotated, $p);
			}
			return @rotated;
		} else {
			return @plans;
		}
	};
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

Copyright (c) 2014--2019 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
