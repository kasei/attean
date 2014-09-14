use v5.14;
use warnings;

package Attean::API::DirectedAcyclicGraph 0.001 {
	use Moo::Role;
	use Scalar::Util qw(refaddr);
	use Types::Standard qw(ArrayRef ConsumerOf);

=item C<< children >>

An ARRAY reference of L<Attean::API::DirectedAcyclicGraph> objects.

=back

=cut

	has 'children' => (
		is => 'ro',
		isa => ArrayRef[ConsumerOf['Attean::API::DirectedAcyclicGraph']],
		default => sub { [] },
	);
	
=item C<< is_leaf >>

Returns true if the referent has zero C<< children >>, false otherwise.

=cut

	sub is_leaf {
		my $self	= shift;
		return not(scalar(@{ $self->children }));
	}
	
=item C<< walk( prefix => \&pre_cb, postfix => \&pre_cb ) >>

Walks the graph rooted at the referent, calling C<< &pre_cb >> (if supplied)
before descending, and C<< &post_cb >> (if supplied) after descending. The
callback functions are passed the current graph walk node as the single
argument.

=cut

	sub walk {
		my $self	= shift;
		my %cb		= @_;
		if (my $cb = $cb{ prefix }) {
			$cb->( $self );
		}
		foreach my $c (@{ $self->children }) {
			$c->walk( %cb );
		}
		if (my $cb = $cb{ postfix }) {
			$cb->( $self );
		}
	}
	
=item C<< cover( prefix => \&pre_cb, postfix => \&pre_cb ) >>

Similar to C<< walk >>, walks the graph rooted at the referent, calling
C<< &pre_cb >> (if supplied) before descending, and C<< &post_cb >> (if
supplied) after descending. However, unlike C<< walk >>, each node in the graph
is visited only once.

=cut

	sub cover {
		my $self	= shift;
		return $self->_cover({}, @_);
	}
	
	sub _cover {
		my $self	= shift;
		my $seen	= shift;
		my %cb		= @_;
		return if ($seen->{refaddr($self)}++);
		if (my $cb = $cb{ prefix }) {
			$cb->( $self );
		}
		foreach my $c (@{ $self->children }) {
			$c->_cover( $seen, %cb );
		}
		if (my $cb = $cb{ postfix }) {
			$cb->( $self );
		}
	}
}

package Attean::API::Algebra 0.001 {
	use Moo::Role;

	requires 'in_scope_variables';			# variables that will be in-scope after this operation is evaluated
# TODO: require these algebra methods:
# 	requires 'necessarily_bound_variables';	# variables that will necessarily be bound to a term after this operation is evaluated
# 	requires 'required_variables';			# variables that must be bound before this operation for its evaluation to be successful
	
	sub BUILD {}
	if ($ENV{ATTEAN_TYPECHECK}) {
		around 'BUILD' => sub {
			my $orig	= shift;
			my $self	= shift;
			$self->$orig(@_);
			my $name	= ref($self);
			$name		=~ s/^.*://;
			if ($self->can('arity')) {
				my $arity	= $self->arity;
				my $children	= $self->children;
				my $size	= scalar(@$children);
				unless ($size == $arity) {
					die "${name} algebra construction with bad number of children (expected $arity, but got $size)";
				}
			}
		}
	}
}

package Attean::API::QueryTree 0.001 {
	use Moo::Role;
	with 'Attean::API::DirectedAcyclicGraph';
}

package Attean::API::NullaryQueryTree 0.001 {
	use Moo::Role;
	sub arity { return 0 }
	with 'Attean::API::QueryTree';
}

package Attean::API::UnaryQueryTree 0.001 {
	use Moo::Role;
	sub arity { return 1 }
	with 'Attean::API::QueryTree';
}

package Attean::API::BinaryQueryTree 0.001 {
	use Moo::Role;
	sub arity { return 2 }
	with 'Attean::API::QueryTree';
}

package Attean::API::PropertyPath 0.001 {
	use Moo::Role;
	with 'Attean::API::QueryTree';
	requires 'as_string';
}

package Attean::API::UnaryPropertyPath 0.001 {
	use Moo::Role;
	use Types::Standard qw(ConsumerOf);
	sub arity { return 1 }
# 	has 'path' => (is => 'ro', isa => ConsumerOf['Attean::API::PropertyPath'], required => 1);
	sub prefix_name { "" }
	sub postfix_name { "" }
	sub as_string {
		my $self	= shift;
		my ($path)	= @{ $self->children };
		my $pstr	= $path->as_string;
		if ($path->does('Attean::API::UnaryPropertyPath')) {
			$pstr	= "($pstr)";
		}
		my $str	= sprintf("%s%s%s", $self->prefix_name, $pstr, $self->postfix_name);
		return $str;
	}
	with 'Attean::API::PropertyPath', 'Attean::API::UnaryQueryTree';
}

package Attean::API::NaryPropertyPath 0.001 {
	use Moo::Role;
	use Types::Standard qw(ArrayRef ConsumerOf);
# 	has 'children' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::PropertyPath']], required => 1);
	requires 'separator';
	sub as_string {
		my $self	= shift;
		my @children	= @{ $self->children };
		if (scalar(@children) == 1) {
			return $children[0]->as_string;
		} else {
			return sprintf("(%s)", join($self->separator, map { $_->as_string } @children));
		}
	}
	with 'Attean::API::PropertyPath';
}

package Attean::API::UnionScopeVariables 0.001 {
	use Moo::Role;
	sub in_scope_variables {
		my $self	= shift;
		my $set		= Set::Scalar->new();
		foreach my $c (@{ $self->children }) {
			$set->insert( $c->in_scope_variables );
		}
		return $set->members;
	}
}

package Attean::API::IntersectionScopeVariables 0.001 {
	use Moo::Role;
	sub in_scope_variables {
		my $self	= shift;
		my @c		= @{ $self->children };
		return unless scalar(@c);
		my $set		= Set::Scalar->new(shift(@c)->in_scope_variables);
		foreach my $c (@c) {
			my $rhs	= Set::Scalar->new($c->in_scope_variables);
			$set	= $set->intersection($rhs);
		}
		return $set->members;
	}
}

1;
