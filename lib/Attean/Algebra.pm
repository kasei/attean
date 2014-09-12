use v5.14;
use warnings;

use Attean::API::Query;

package Attean::Algebra::Join 0.001 {
	use Moo;
	with 'Attean::API::UnionScopeVariables';
	with 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
}

package Attean::Algebra::LeftJoin 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnionScopeVariables';
	with 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
}

package Attean::Algebra::Filter 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnionScopeVariables';
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
}

package Attean::Algebra::Union 0.001 {
	use Moo;
	with 'Attean::API::UnionScopeVariables';
	with 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
}

package Attean::Algebra::Graph 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	sub in_scope_variables {
		my $self	= shift;
		my $graph	= $self->graph;
		my ($child)	= @{ $self->children };
		my @vars	= $child->in_scope_variables;
		if ($graph->does('Attean::API::Variable')) {
			return Set::Scalar->new(@vars, $graph->value)->members;
		} else {
			return @vars;
		}
	}
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'graph' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
}

package Attean::Algebra::Extend 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	sub in_scope_variables {
		my $self	= shift;
		my ($child)	= @{ $self->children };
		my @vars	= $child->in_scope_variables;
		return Set::Scalar->new(@vars, $self->variable->value)->members;
	}
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'variable' => (is => 'ro', isa => ConsumerOf['Attean::API::Variable'], required => 1);
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
}

package Attean::Algebra::Minus 0.001 {
	use Moo;
	sub in_scope_variables {
		my $self	= shift;
		my ($child)	= @{ $self->children };
		return $child->in_scope_variables;
	}
	with 'Attean::API::Algebra', 'Attean::API::BinaryQueryTree';
}

package Attean::Algebra::Distinct 0.001 {
	use Moo;
	with 'Attean::API::UnionScopeVariables';
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
}

package Attean::Algebra::Reduced 0.001 {
	use Moo;
	with 'Attean::API::UnionScopeVariables';
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
}

package Attean::Algebra::Slice 0.001 {
	use Moo;
	use Types::Standard qw(Int);
	with 'Attean::API::UnionScopeVariables';
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'limit' => (is => 'ro', isa => Int, default => -1);
	has 'offset' => (is => 'ro', isa => Int, default => 0);
}

package Attean::Algebra::Project 0.001 {
	use Moo;
	sub in_scope_variables {
		my $self	= shift;
		my ($child)	= @{ $self->children };
		my $set		= $child->in_scope_variables;
		my $proj	= Set::Scalar->new( map { $_->value } @{ $self->variables } );
		return $set->intersection($proj);
	}
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	use Types::Standard qw(ArrayRef ConsumerOf);
	has 'variables' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Variable']], required => 1);
}

package Attean::Algebra::_Comparator 0.001 {
	use Moo;
	use Types::Standard qw(Bool ConsumerOf);
	has 'ascending' => (is => 'ro', isa => Bool, default => 1);
	has 'expression' => (is => 'ro', isa => ConsumerOf['Attean::API::Expression'], required => 1);
}

package Attean::Algebra::OrderBy 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef InstanceOf);
	with 'Attean::API::UnionScopeVariables';
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'comparators' => (is => 'ro', isa => ArrayRef[InstanceOf['Attean::Algebra::_Comparator']], required => 1);
}

package Attean::Algebra::BGP 0.001 {
	use Moo;
	use Attean::RDF;
	use Types::Standard qw(ArrayRef ConsumerOf);
	sub in_scope_variables {
		my $self	= shift;
		my $set		= Set::Scalar->new();
		foreach my $t (@{ $self->triples }) {
			my @vars	= map { $_->value } grep { $_->does('Attean::API::Variable') } $t->values;
			$set->insert(@vars);
		}
		return $set->members;
	}
	with 'Attean::API::Algebra', 'Attean::API::NullaryQueryTree';
	has 'triples' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::TriplePattern']], default => sub { [] });
	
	sub canonicalize {
		my $self	= shift;
		my ($algebra, $mapping)	= $self->canonical_bgp_with_mapping();
		my @proj	= sort map { sprintf("(?v%03d AS $_)", $mapping->{$_}{id}) } grep { $mapping->{$_}{type} eq 'variable' } (keys %$mapping);
		foreach my $var (keys %$mapping) {
			$algebra	= Attean::Algebra::Extend->new(
				children	=> [$algebra],
				variable	=> variable($var),
				expression	=> Attean::ValueExpression->new( value => variable($mapping->{$var}{id}) ),
			);
		}
	}
	
	sub canonical_bgp_with_mapping {
		my $bgp		= shift;
		my @t		= @{ $bgp->triples };
		my @pairs	= map { [ $_->tuples_string, $_, {} ] } @t;
		my $replacements	= 0;
		foreach my $p (@pairs) {
			my ($str, $t)	= @$p;
			foreach my $pos (qw(subject predicate object)) {
				my $term	= $t->$pos();
				my $tstr	= $term->ntriples_string;
				if ($term->does('Attean::API::Blank') or $term->does('Attean::API::Variable')) {
					$str	=~ s/\Q$tstr\E/~/;
					$str	.= "#$tstr";
					$p->[2]{$pos}	= $tstr;
					$replacements++;
					$p->[0]	= $str;
				}
			}
		}
	
		@pairs	= sort { $a->[0] cmp $b->[0] } @pairs;
		my $counter	= 1;
		my %mapping;
		foreach my $i (0 .. $#pairs) {
			my $p		= $pairs[$i];
			my ($str, $t)	= @$p;
			my ($next, $last)	= ('')x2;
			$last	= $pairs[$i-1][0] if ($i > 0);
			$next	= $pairs[$i+1][0] if ($i < $#pairs);
			next if ($str eq $last or $str eq $next);
			foreach my $pos (qw(object predicate subject)) {
				if (defined(my $tstr = $p->[2]{$pos})) {
					$tstr	=~ /^([?]|_:)([^#]+)$/;
					my $prefix	= $1;
					my $name	= $2;
					my $key		= "$prefix$name";
					delete $p->[2]{$pos};
					my $id		= (exists($mapping{$key})) ? $mapping{$key}{id} : sprintf("v%03d", $counter++);
					my $type	= ($prefix eq '?' ? 'variable' : 'blank');
					$mapping{ $key }	= { id => $id, prefix => $prefix, type => $type };
					my %t		= $p->[1]->mapping;
					$t{ $pos }	= ($type eq 'blank') ? blank($id) : variable($id);
					my $t	= Attean::Triple->new( %t );
					$p->[1]	= $t;
					$p->[0]	= $t->tuples_string;
				}
			}
		}
	
		foreach my $p (@pairs) {
			my ($str, $t)	= @$p;
			foreach my $pos (qw(object predicate subject)) {
				if (defined(my $tstr = $p->[2]{$pos})) {
					$tstr	=~ /^([?]|_:)([^#]+)$/;
					my $prefix	= $1;
					my $name	= $2;
					my $key		= "$prefix$name";
					delete $p->[2]{$pos};
					unless (exists($mapping{$key})) {
						warn "Cannot canonicalize BGP";
						return;
					}
					my $id		= $mapping{$key}{id};
					my $type	= ($prefix eq '?' ? 'variable' : 'blank');
					$mapping{ $key }	= { id => $id, prefix => $prefix, type => $type };
					my %t		= $p->[1]->mapping;
					$t{ $pos }	= ($type eq 'blank') ? blank($id) : variable($id);
					my $t	= Attean::Triple->new( %t );
					$p->[1]	= $t;
					$p->[0]	= $t->tuples_string;
				}
			}
		}

		@pairs	= sort { $a->[0] cmp $b->[0] } @pairs;
		my $canon	= Attean::Algebra::BGP->new( triples => [ map { $_->[1] } @pairs ] );
		return ($canon, \%mapping);
	}	
}

package Attean::Algebra::Path 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	sub in_scope_variables {
		my $self	= shift;
		my @vars	= map { $_->value } grep { $_->does('Attean::API::Variable') } ($self->subject, $self->object);
		return Set::Scalar->new(@vars)->members;
	}
	with 'Attean::API::Algebra', 'Attean::API::NullaryQueryTree';
	has 'subject' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
	has 'path' => (is => 'ro', isa => ConsumerOf['Attean::API::PropertyPath'], required => 1);
	has 'object' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
}

package Attean::Algebra::Group 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	sub in_scope_variables {
		my $self	= shift;
		# TODO: implement Attean::Algebra::Group->in_scope_variables
		die "Attean::Algebra::Group->in_scope_variables unimplemented";
	}
	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
	has 'groupby' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Expression']]);
	has 'aggregates' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::AggregateExpression']]);
}

package Attean::Algebra::NegatedPropertySet 0.001 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	with 'Attean::API::PropertyPath';
	has 'predicates' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::IRI']], required => 1);
	sub as_string {
		my $self	= shift;
		return sprintf("!(%s)", join('|', map { $_->ntriples_string } @{ $self->predicates }));
	}
}

package Attean::Algebra::PredicatePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::PropertyPath';
	has 'predicate' => (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], required => 1);
	sub as_string {
		my $self	= shift;
		return $self->predicate->ntriples_string;
	}
}

package Attean::Algebra::InversePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub prefix_name { return "^" }
}

package Attean::Algebra::SequencePath 0.001 {
	use Moo;
	with 'Attean::API::NaryPropertyPath';
	sub separator { return "/" }
}

package Attean::Algebra::AlternativePath 0.001 {
	use Moo;
	with 'Attean::API::NaryPropertyPath';
	sub separator { return "|" }
}

package Attean::Algebra::ZeroOrMorePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "*" }
}

package Attean::Algebra::OneOrMorePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "+" }
}

package Attean::Algebra::ZeroOrOnePath 0.001 {
	use Moo;
	use Types::Standard qw(ConsumerOf);
	with 'Attean::API::UnaryPropertyPath';
	sub postfix_name { return "?" }
}


1;
