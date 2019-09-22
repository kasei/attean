use v5.14;
use warnings;
use utf8;

=head1 NAME

Attean::API::Plan - Query plan

=head1 VERSION

This document describes Attean::API::Plan version 0.024

=head1 DESCRIPTION

The Attean::API::Plan role defines a common API for all query plans.

=head1 ATTRIBUTES

=over 4

=item C<< cost >>

=item C<< distinct >>

=item C<< item_type >>

=item C<< in_scope_variables >>

=item C<< ordered >>

=back

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Plan> role:

=over 4

=item C<< impl( $model ) >>

Returns a code reference that when called (without arguments), returns an
L<Attean::API::Iterator> object.

=back

=head1 METHODS

=over 4

=item C<< has_cost >>

=cut

use Type::Tiny::Role;

package Attean::API::Plan 0.024 {
	use Scalar::Util qw(blessed);
	use Types::Standard qw(ArrayRef CodeRef Str Object InstanceOf Bool Num Int);

	use Moo::Role;
	
	has 'cost' => (is => 'rw', isa => Int, predicate => 'has_cost');
	has 'distinct' => (is => 'rw', isa => Bool, required => 1, default => 0);
	has 'item_type' => (is => 'ro', isa => Str, required => 1, default => 'Attean::API::Result');
	has 'in_scope_variables' => (is => 'ro', isa => ArrayRef[Str], required => 1);
	has 'ordered' => (is => 'ro', isa => ArrayRef, required => 1, default => sub { [] });
	
	requires 'impl';
	requires 'plan_as_string';

=item C<< as_string >>

Returns a tree-structured string representation of this plan, including children.

=cut
	
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
			if (defined(my $cost = $a->cost)) {
				push(@flags, "cost: $cost");
			}
			$string	.= "-$indent " .  $a->plan_as_string($level);
			if (scalar(@flags)) {
				$string .= ' (' . join(' ', @flags) . ")";
			}
			$string	.= "\n";
		});
		return $string;
	}

=item C<< evaluate( $model ) >>

Evaluates this plan and returns the resulting iterator.

=cut

	sub evaluate {
		my $self	= shift;
		my $impl	= $self->impl(@_);
		return $impl->();
	}

=item C<< in_scope_variables_union( @plans ) >>

Returns the set union of C<< in_scope_variables >> of the given plan objects.

=cut

	sub in_scope_variables_union {
		my @plans	= grep { blessed($_) } @_;
		my %vars	= map { $_ => 1 } map { @{ $_->in_scope_variables } } @plans;
		return keys %vars;
	}

=item C<< subplans_of_type_are_variable_connected( $type ) >>

Returns true if the subpatterns of the given C<< $type >> are all connected
through their C<< in_scope_variables >>, false otherwise (implying a cartesian
product if the connecting plans perform some form of join.

=cut

	sub subplans_of_type_are_variable_connected {
		my $self	= shift;
		my @types	= @_;
		my @c		= $self->subpatterns_of_type(@types);
		return $self->_plans_are_variable_connected(@c);
	}
	
=item C<< children_are_variable_connected( $type ) >>

Returns true if the children of this plan are all connected
through their C<< in_scope_variables >>, false otherwise (implying a cartesian
product if this plan performs some form of join.

=cut

	sub children_are_variable_connected {
		my $self	= shift;
		my @c		= @{ $self->children };
		return $self->_plans_are_variable_connected(@c);
	}
	
	sub _plans_are_variable_connected {
		# TODO: In the worst case, this is going to run in O(n^2) in the number
		# of children. Better indexing of the children by variables can speed
		# this up.
		my $self	= shift;
		my @c		= @_;
# 		warn "===========================\n";
# 		foreach my $c (@c) {
# 			warn $c->as_string;
# 		}
		return 1 unless (scalar(@c));
		
		my %vars_by_child;
		foreach my $i (0 .. $#c) {
			my $c	= $c[$i];
			foreach my $var (@{ $c->in_scope_variables }) {
				$vars_by_child{$i}{$var}++;
			}
		}
		
		#
		my @remaining	= keys %vars_by_child;
		return 1 unless (scalar(@remaining));
		my $current		= shift(@remaining);
# 		warn 'Starting with ' . $c[$current]->as_string;
		my %seen_vars	= %{ $vars_by_child{$current} };
		LOOP: while (scalar(@remaining)) {
			foreach my $i (0 .. $#remaining) {
				my $candidate	= $remaining[$i];
				my @candidate_vars	= keys %{ $vars_by_child{$candidate} };
				foreach my $var (@candidate_vars) {
					if (exists $seen_vars{ $var }) {
						foreach my $var (@candidate_vars) {
							$seen_vars{$var}++;
						}
# 						warn "connected with $var: " . $c[$candidate]->as_string;
						splice(@remaining, $i, 1);
						next LOOP;
					}
				}
			}
# 			warn 'Not fully connected';
			return 0;
		}
# 		warn 'Fully connected';
		return 1;
	}
}

package Attean::API::BindingSubstitutionPlan 0.024 {
	use Moo::Role;
	with 'Attean::API::Plan';
	requires 'substitute_impl'; # $code = $plan->impl($model, $binding);
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my $b		= Attean::Result->new();
		return $self->substitute_impl($model, $b);
	}
}

package Attean::API::UnionScopeVariablesPlan 0.024 {
	use Moo::Role;

	with 'Attean::API::Plan';
	
	around 'BUILDARGS' => sub {
		my $orig		= shift;
		my $class		= shift;
		my %args		= @_;
		my @vars		=  Attean::API::Plan->in_scope_variables_union( @{ $args{children} } );
		
		if (exists $args{in_scope_variables}) {
			Carp::confess "in_scope_variables is computed automatically, and must not be specified in the $class constructor";
		}
		$args{in_scope_variables}	= [@vars];

		return $orig->( $class, %args );
	};
}

package Attean::API::Plan::Join 0.024 {
	use Types::Standard qw(CodeRef);
	use Types::Standard qw(ArrayRef Str ConsumerOf Bool);

	use Moo::Role;

	with 'Attean::API::Plan', 'Attean::API::BinaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';
	
	has 'join_variables' => (is => 'ro', isa => ArrayRef[Str], required => 1);
	has 'anti' => (is => 'ro', isa => Bool, default => 0);	# is this an anti-join
	has 'left' => (is => 'ro', isa => Bool, default => 0);	# is this a left, outer-join
	
	# if this is a left, outer-join, this is the filter expression that acts as part of the join operation (see the SPARQL semantics for LeftJoin for more details)
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 0, default => sub { Attean::ValueExpression->new( value => Attean::Literal->true ) });
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
