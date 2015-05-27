use v5.14;
use warnings;

=head1 NAME

Attean::API::Plan - Query plan

=head1 VERSION

This document describes Attean::API::Plan version 0.005

=head1 DESCRIPTION

The Attean::API::Plan role defines a common API for all query plans.

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Plan> role:

=over 4

=cut

use Type::Tiny::Role;

package Attean::API::Plan 0.005 {
	use Moo::Role;
	use Types::Standard qw(ArrayRef CodeRef Str Object InstanceOf Bool Num);
	use namespace::clean;
	
	has 'cost' => (is => 'rw', isa => Num, predicate => 'has_cost');
	has 'distinct' => (is => 'rw', isa => Bool, required => 1, default => 0);
	has 'item_type' => (is => 'ro', isa => Str, required => 1, default => 'Attean::API::Result');
	has 'in_scope_variables' => (is => 'ro', isa => ArrayRef[Str], required => 1);
	has 'ordered' => (is => 'ro', isa => ArrayRef, required => 1, default => sub { [] });
	
	requires 'impl';
	
	sub plan_as_string {
		my $self	= shift;
		return "$self";
	}
	
	sub as_string {
		my $self	= shift;
		my $string	= '';
		$self->walk( prefix => sub {
			my $a		= shift;
			my $level	= shift;
			my $parent	= shift;
			my $indent	= '  ' x $level;
			my @flags;
			push(@flags, 'distinct') if ($a->distinct);
			if (scalar(@{ $a->ordered })) {
				my @orders;
				foreach my $c (@{ $a->ordered }) {
					my $dir	= $c->ascending ? "↑" : "↓";
					my $s	= $dir . $c->expression->as_string;
					push(@orders, $s);
				}
				push(@flags, "order: " . join('; ', @orders));
			}
			$string	.= "-$indent " .  $a->plan_as_string;
			if (scalar(@flags)) {
				$string .= ' (' . join(' ', @flags) . ")";
			}
			$string	.= "\n";
		});
		return $string;
	}

	sub evaluate {
		my $self	= shift;
		my $impl	= $self->impl(@_);
		return $impl->();
	}
}

package Attean::API::Planner 0.005 {
	use Moo::Role;
	use Types::Standard qw(CodeRef);
	use namespace::clean;
	
	requires 'plan_for_algebra'; # plan_for_algebra($algebra, $model, \@default_graphs)
}

package Attean::API::CostPlanner 0.005 {
	use Moo::Role;
	use Types::Standard qw(CodeRef);
	use namespace::clean;
	with 'Attean::API::Planner';
	
	requires 'plans_for_algebra'; # plans_for_algebra($algebra, $model, \@active_graphs, \@default_graphs)
	requires 'cost_for_plan'; # cost_for_plan($plan, $model)
	
	sub plan_for_algebra {
		my $self			= shift;
		my $algebra			= shift;
		my $model			= shift;
		my $default_graphs	= shift;
		my $active_graphs	= $default_graphs;
		my @plans			= sort { $self->cost_for_plan($a, $model) <=> $self->cost_for_plan($b, $model) } $self->plans_for_algebra($algebra, $model, $active_graphs, $default_graphs);
		my $plan			= shift(@plans);
		return $plan;
	}
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
