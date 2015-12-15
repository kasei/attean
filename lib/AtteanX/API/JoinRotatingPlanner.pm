use v5.14;
use Test::Modern;
use Attean;
use Attean::RDF;
use Attean::IDPQueryPlanner;

package AtteanX::API::JoinRotatingPlanner {
	# Rotate joins like (A⋈B)⋈C to A⋈(B⋈C), with the ability to coalesce B⋈C (e.g. for adjacent BGPs)
	use Moo::Role;
	use namespace::clean;
	
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
				push(@rotated, $p);
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
			}
			return @rotated;
		} else {
			return @plans;
		}
	};
}

1;
