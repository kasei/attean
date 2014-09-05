use v5.14;
use warnings;

package Attean::API::Expression 0.001 {
	use Moo::Role;
	use Types::Standard qw(Str);
	with 'Attean::API::DirectedAcyclicGraph';
	
	has 'operator' => (is => 'ro', isa => Str, required => 1);
	requires 'evaluate';
	requires 'as_string';
	
	sub BUILD {}
	if ($ENV{ATTEAN_TYPECHECK}) {
		around 'BUILD' => sub {
			my $orig	= shift;
			my $self	= shift;
			$self->$orig(@_);
			my $name	= ref($self);
			$name		=~ s/^.*://;
			if ($self->can('arity')) {
				my $arity		= $self->arity;
				if (defined($arity)) {
					my $children	= $self->children;
					my $size		= scalar(@$children);
					unless ($size == $arity) {
						die "${name} expression construction with bad number of children (expected $arity, but got $size)";
					}
				}
			}
		}
	}
}

package Attean::API::UnaryExpression 0.001 {
	use Moo::Role;
	with 'Attean::API::Expression', 'Attean::API::UnaryQueryTree';
	sub as_string {
		my $self	= shift;
		my ($data)	= @{ $self->children };
		return sprintf("%s(%s)", $self->operator, $data->as_string);
	}
}

package Attean::API::BinaryExpression 0.001 {
	use Moo::Role;
	with 'Attean::API::Expression', 'Attean::API::BinaryQueryTree';
	sub as_string {
		my $self	= shift;
		my ($lhs, $rhs)	= @{ $self->children };
		return sprintf("(%s %s %s)", $lhs->as_string, $self->operator, $rhs->as_string);
	}
}

package Attean::API::NaryExpression 0.001 {
	use Moo::Role;
	with 'Attean::API::Expression', 'Attean::API::QueryTree';
	sub as_string {
		my $self	= shift;
		my @children	= map { $_->as_string } @{ $self->children };
		return sprintf("%s(%s)", $self->operator, join(', ', @children));
	}
}

package Attean::API::AggregateExpression 0.001 {
	use Moo::Role;
	with 'Attean::API::DirectedAcyclicGraph';
}

1;
