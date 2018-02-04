use v5.14;
use warnings;
use utf8;

=head1 NAME

Attean::Plan - Representation of SPARQL query plan operators

=head1 VERSION

This document describes Attean::Plan version 0.019

=head1 SYNOPSIS

  use v5.14;
  use Attean;

=head1 DESCRIPTION

This is a utility package that defines all the Attean query plan classes
in the Attean::Plan namespace:

=over 4

=cut

use Attean::API::Query;

=item * L<Attean::Plan::Quad>

Evaluates a quad pattern against the model.

=cut

package Attean::Plan::Quad 0.019 {
	use Moo;
	use Scalar::Util qw(blessed reftype);
	use Types::Standard qw(ConsumerOf ArrayRef);
	use namespace::clean;

	has 'subject'	=> (is => 'ro', required => 1);
	has 'predicate'	=> (is => 'ro', required => 1);
	has 'object'	=> (is => 'ro', required => 1);
	has 'graph'		=> (is => 'ro', required => 1);
	
	with 'Attean::API::BindingSubstitutionPlan', 'Attean::API::NullaryQueryTree';
	with 'Attean::API::QuadPattern';

	around 'BUILDARGS' => sub {
		my $orig		= shift;
		my $class		= shift;
		my $args		= $orig->( $class, @_ );
		if (exists $args->{in_scope_variables}) {
			Carp::confess "in_scope_variables is computed automatically, and must not be specified in the $class constructor";
		}
		
		my %vars;
		foreach my $pos (qw(subject predicate object graph)) {
			my $term	= $args->{$pos};
			if (blessed($term) and $term->does('Attean::API::Variable')) {
				$vars{$term->value}	= $term;
			}
		}
		
		my @vars	= keys %vars;
		$args->{in_scope_variables}	= [@vars];

		return $args;
	};

	sub plan_as_string {
		my $self	= shift;
		my @nodes	= $self->values;
		my @strings;
		foreach my $t (@nodes) {
			if (ref($t) eq 'ARRAY') {
				my @tstrings	= map { $_->ntriples_string } @$t;
				if (scalar(@tstrings) == 1) {
					push(@strings, @tstrings);
				} else {
					push(@strings, '[' . join(', ', @tstrings) . ']');
				}
			} elsif ($t->does('Attean::API::TermOrVariable')) {
				push(@strings, $t->ntriples_string);
			} else {
				use Data::Dumper;
				die "Unrecognized node in quad pattern: " . Dumper($t);
			}
		}
		return sprintf('Quad { %s }', join(', ', @strings));
	}
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my $b		= shift;
		my @values	= $self->values;
		foreach my $i (0 .. $#values) {
			my $value	= $values[$i];
			if (reftype($value) eq 'ARRAY') {
				my @values;
				foreach my $value (@{ $value }) {
					my $name	= $value->value;
					if (my $node = $b->value($name)) {
						push(@values, $node);
					} else {
						push(@values, $value);
					}
					$values[$i]	= \@values;
				}
			} elsif ($value->does('Attean::API::Variable')) {
				my $name	= $value->value;
				if (my $node = $b->value($name)) {
					$values[$i]	= $node;
				}
			}
		}
		
		return sub {
			return $model->get_bindings( @values );
		}
	}

	sub impl {
		my $self	= shift;
		my $model	= shift;
		my @values	= $self->values;
		return sub {
			return $model->get_bindings( @values );
		}
	}
}

=item * L<Attean::Plan::NestedLoopJoin>

Evaluates a join (natural-, anti-, or left-) using a nested loop.

=cut

package Attean::Plan::NestedLoopJoin 0.019 {
	use Moo;
	use List::MoreUtils qw(all);
	use namespace::clean;

	with 'Attean::API::BindingSubstitutionPlan';
	with 'Attean::API::Plan::Join';
	sub plan_as_string {
		my $self	= shift;
		if ($self->left) {
			return 'NestedLoop Left Join';
		} elsif ($self->anti) {
			return 'NestedLoop Anti Join';
		} else {
			return 'NestedLoop Join';
		}
	}

	sub impl {
		my $self	= shift;
		my $model	= shift;
		my @children	= map { $_->impl($model) } @{ $self->children };
		return $self->_impl($model, @children);
	}
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my $b		= shift;
		unless (all { $_->does('Attean::API::BindingSubstitutionPlan') } @{ $self->children }) {
			die "Plan children do not all consume BindingSubstitutionPlan role:\n" . $self->as_string;
		}
		
		my @children	= map { $_->substitute_impl($model, $b) } @{ $self->children };
		return $self->_impl($model, @children);
	}
	
	sub _impl {
		my $self		= shift;
		my $model		= shift;
		my @children	= @_;
		my $left		= $self->left;
		my $anti		= $self->anti;
		my $iter_variables	= $self->in_scope_variables;
		
		return sub {
			my ($lhs, $rhs)	= map { $_->() } @children;
			my @right	= $rhs->elements;
			my @results;
			while (my $l = $lhs->next) {
				my $seen	= 0;
				foreach my $r (@right) {
					my @shared	= $l->shared_domain($r);
					if ($anti and scalar(@shared) == 0) {
						# in a MINUS, two results that have disjoint domains are considered not to be joinable
						next;
					}
					if (my $j = $l->join($r)) {
						$seen++;
						if ($left) {
							# TODO: filter with expression
							push(@results, $j);
						} elsif ($anti) {
						} else {
							push(@results, $j);
						}
					}
				}
				if ($left and not($seen)) {
					push(@results, $l);
				} elsif ($anti and not($seen)) {
					push(@results, $l);
				}
			}
			return Attean::ListIterator->new(
				item_type => 'Attean::API::Result',
				variables => $iter_variables,
				values => \@results,
			);
		}
	}
}

=item * L<Attean::Plan::HashJoin>

Evaluates a join (natural-, anti-, or left-) using a hash join.

=cut

package Attean::Plan::HashJoin 0.019 {
	use Moo;
	use List::MoreUtils qw(all);
	use namespace::clean;
	
	sub BUILD {
		my $self	= shift;
		if ($self->anti) {
			die "Cannot use a HashJoin for anti-joins";
		}
	}
	
	with 'Attean::API::BindingSubstitutionPlan';
	with 'Attean::API::Plan::Join';
	sub plan_as_string {
		my $self	= shift;
		my $name;
		if ($self->left) {
			$name	= "Hash Left Join";
		} else {
			$name	= "Hash Join";
		}
		return sprintf('%s { %s }', $name, join(', ', @{$self->join_variables}));
	}

	sub impl {
		my $self	= shift;
		my $model	= shift;
		my @children	= map { $_->impl($model) } @{ $self->children };
		return $self->_impl($model, @children);
	}
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my $b		= shift;
		
		unless (all { $_->does('Attean::API::BindingSubstitutionPlan') } @{ $self->children }) {
			die "Plan children do not all consume BindingSubstitutionPlan role:\n" . $self->as_string;
		}
		
		my @children	= map { $_->substitute_impl($model, $b) } @{ $self->children };
		return $self->_impl($model, @children);
	}
	
	sub _impl {
		my $self		= shift;
		my $model		= shift;
		my @children	= @_;
		my $left	= $self->left;
		my $iter_variables	= $self->in_scope_variables;

		return sub {
			my %hash;
			my @vars	= @{ $self->join_variables };
			my $rhs		= $children[1]->();
			while (my $r = $rhs->next()) {
				my $has_unbound_right_join_var	= 0;
				my @values;
				foreach my $var (@vars) {
					my $value	= $r->value($var);
					unless (defined($value)) {
						$has_unbound_right_join_var++;
					}
					push(@values, $value);
				}
				if ($has_unbound_right_join_var) {
					# this is a RHS row that doesn't have a term bound to one of the join variables.
					# this will make it impossible to compute the proper hash key to access the row bucket,
					# so we add this row to the null bucket (hash key '') which we try to join all LHS rows
					# against.
					push(@{ $hash{''} }, $r);
				} else {
					my $key	= join(',', map { ref($_) ? $_->as_string : '' } @values);
					push(@{ $hash{$key} }, $r);
				}
			}
			
			my @results;
			my $lhs		= $children[0]->();
			while (my $l = $lhs->next()) {
				my $seen	= 0;
				my @values;
				my $has_unbound_left_join_var	= 0;
				foreach my $var (@vars) {
					my $value	= $l->value($var);
					unless (defined($value)) {
						$has_unbound_left_join_var++;
					}
					push(@values, $value);
				}
				
				my @buckets;
				if (my $b = $hash{''}) {
					push(@buckets, $b);
				}
				
				if ($has_unbound_left_join_var) {
					my $pattern	= join(',', map { ref($_) ? quotemeta($_->as_string) : '.*' } @values);
					foreach my $key (keys %hash) {
						if ($key =~ /^${pattern}$/) {
							push(@buckets, $hash{$key});
						}
					}
				} else {
					my $key		= join(',', map { ref($_) ? $_->as_string : '' } @values);
					if (my $rows = $hash{$key}) {
						push(@buckets, $rows);
					}
				}
				
				foreach my $rows (@buckets) {
					foreach my $r (@$rows) {
						if (my $j = $l->join($r)) {
							$seen++;
							if ($left) {
								# TODO: filter with expression
								push(@results, $j);
							} else {
								push(@results, $j);
							}
						}
					}
				}
				if ($left and not($seen)) {
					push(@results, $l);
				}
			}
			return Attean::ListIterator->new(
				item_type => 'Attean::API::Result',
				variables => $iter_variables,
				values => \@results
			);
		}
	}
}

=item * L<Attean::Plan::Construct>

=cut

package Attean::Plan::Construct 0.019 {
	use Moo;
	use List::MoreUtils qw(all);
	use Types::Standard qw(Str ArrayRef ConsumerOf InstanceOf);
	use namespace::clean;
	has 'triples' => (is => 'ro', 'isa' => ArrayRef[ConsumerOf['Attean::API::TripleOrQuadPattern']], required => 1);

	with 'Attean::API::BindingSubstitutionPlan', 'Attean::API::UnaryQueryTree';

	sub plan_as_string {
		my $self	= shift;
		my $triples	= $self->triples;
		return sprintf('Construct { %s }', join(' . ', map { $_->as_string } @$triples));
	}

	sub BUILDARGS {
		# TODO: this code is repeated in several plan classes; figure out a way to share it.
		my $class		= shift;
		my %args		= @_;
		my %vars		= map { $_ => 1 } map { @{ $_->in_scope_variables } } @{ $args{ children } };
		my @vars		= keys %vars;
		
		if (exists $args{in_scope_variables}) {
			Carp::confess "in_scope_variables is computed automatically, and must not be specified in the $class constructor";
		}
		$args{in_scope_variables}	= \@vars;

		return $class->SUPER::BUILDARGS(%args);
	}

	sub impl {
		my $self	= shift;
		my $model	= shift;
		my @children	= map { $_->impl($model) } @{ $self->children };
		return $self->_impl($model, @children);
	}
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my $b		= shift;
		unless (all { $_->does('Attean::API::BindingSubstitutionPlan') } @{ $self->children }) {
			die "Plan children do not all consume BindingSubstitutionPlan role:\n" . $self->as_string;
		}
		
		warn "TODO: fix substitute_impl to substitute construct triples";
		my @children	= map { $_->substitute_impl($model, $b) } @{ $self->children };
		return $self->_impl($model, @children);
	}
	
	sub _impl {
		my $self		= shift;
		my $model		= shift;
		my $child		= shift;
		
		my @triples		= @{ $self->triples };
		return sub {
			my $iter	= $child->();
			my @buffer;
			my %seen;
			return Attean::CodeIterator->new(
				item_type => 'Attean::API::Triple',
				generator => sub {
					if (scalar(@buffer)) {
						return shift(@buffer);
					}
					while (my $row = $iter->next) {
						foreach my $tp (@triples) {
							my $tp	= $tp->apply_bindings($row);
							my $t	= eval { $tp->as_triple };
							if ($t) {
								push(@buffer, $t);
							}
						}
						if (scalar(@buffer)) {
							my $t	= shift(@buffer);
							return $t;
						}
					}
				}
			)->grep(sub {
				return not $seen{$_->as_string}++;
			});
		}
	}
}

=item * L<Attean::Plan::Describe>

=cut

package Attean::Plan::Describe 0.019 {
	use Moo;
	use Attean::RDF;
	use List::MoreUtils qw(all);
	use Types::Standard qw(Str ArrayRef ConsumerOf InstanceOf);
	use namespace::clean;

	has 'graph'	=> (is => 'ro');
	has 'terms' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::TermOrVariable']]);

	with 'Attean::API::BindingSubstitutionPlan', 'Attean::API::UnaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	sub plan_as_string {
		my $self	= shift;
		my $terms	= $self->terms;
		return sprintf('Describe { %s }', join(' . ', map { $_->as_string } @$terms));
	}

	sub impl {
		my $self	= shift;
		my $model	= shift;
		my @children	= map { $_->impl($model) } @{ $self->children };
		return $self->_impl($model, @children);
	}
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my $b		= shift;
		unless (all { $_->does('Attean::API::BindingSubstitutionPlan') } @{ $self->children }) {
			die "Plan children do not all consume BindingSubstitutionPlan role:\n" . $self->as_string;
		}
		
		warn "TODO: fix substitute_impl to substitute describe terms";
		my @children	= map { $_->substitute_impl($model, $b) } @{ $self->children };
		return $self->_impl($model, @children);
	}
	
	sub _impl {
		my $self		= shift;
		my $model		= shift;
		my $child		= shift;
		
		my $graph		= $self->graph;
		my @terms		= @{ $self->terms };
		# TODO: Split @terms into ground terms and variables.
		#       Only call get_quads once for ground terms.
		#       For variable terms, call get_quads for each variable-result combination.
		return sub {
			my $iter	= $child->();
			my @buffer;
			my %seen;
			return Attean::CodeIterator->new(
				item_type => 'Attean::API::Triple',
				generator => sub {
					if (scalar(@buffer)) {
						return shift(@buffer);
					}
					while (my $row = $iter->next) {
						foreach my $term (@terms) {
							my $value	= $term->apply_binding($row);
							if ($value->does('Attean::API::Term')) {
								my $iter	= $model->get_quads( $value, variable('predicate'), variable('object'), $graph );
								push(@buffer, $iter->elements);
							}
							if (scalar(@buffer)) {
								return shift(@buffer);
							}
						}
					}
				}
			)->grep(sub {
				return not $seen{$_->as_string}++;
			});
		}
	}
}

=item * L<Attean::Plan::EBVFilter>

Filters results from a sub-plan based on the effective boolean value of a
named variable binding.

=cut

package Attean::Plan::EBVFilter 0.019 {
	use Moo;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Str ConsumerOf);
	use namespace::clean;
	
	with 'Attean::API::BindingSubstitutionPlan', 'Attean::API::UnaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	has 'variable' => (is => 'ro', isa => Str, required => 1);

	sub plan_as_string {
		my $self	= shift;
		return sprintf('EBVFilter { ?%s }', $self->variable);
	}
	sub tree_attributes { return qw(expression) };
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my $bind	= shift;
		my ($impl)	= map { $_->substitute_impl($model, $bind) } @{ $self->children };
		my $var		= $self->variable;

		return sub {
			my $iter	= $impl->();
			return $iter->grep(sub {
				my $r		= shift;
				my $term	= $r->value($var);
				return 0 unless (blessed($term) and $term->does('Attean::API::Term'));
				return $term->ebv;
			});
		};
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		my $var		= $self->variable;
		return sub {
			my $iter	= $impl->();
			return $iter->grep(sub {
				my $r		= shift;
				my $term	= $r->value($var);
				return 0 unless (blessed($term) and $term->does('Attean::API::Term'));
				return $term->ebv;
			});
		};
	}
}

=item * L<Attean::Plan::Merge>

Evaluates a set of sub-plans, returning the merged union of results, preserving
ordering.

=cut

package Attean::Plan::Merge 0.019 {
	use Moo;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Str ArrayRef ConsumerOf);
	use namespace::clean;
	
	with 'Attean::API::Plan', 'Attean::API::BinaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	has 'variables' => (is => 'ro', isa => ArrayRef[Str], required => 1);

	sub plan_as_string { return 'Merge' }
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my @children	= map { $_->impl($model) } @{ $self->children };
		return sub {
			die "Unimplemented";
		};
	}
}

=item * L<Attean::Plan::Union>

Evaluates a set of sub-plans, returning the union of results.

=cut

package Attean::Plan::Union 0.019 {
	use Moo;
	use Scalar::Util qw(blessed);
	use namespace::clean;
	
	with 'Attean::API::BindingSubstitutionPlan', 'Attean::API::BinaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	sub plan_as_string { return 'Union' }
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my @children	= map { $_->impl($model) } @{ $self->children };
		return $self->_impl($model, @children);
	}
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my $b		= shift;
		unless (all { $_->does('Attean::API::BindingSubstitutionPlan') } @{ $self->children }) {
			die "Plan children do not all consume BindingSubstitutionPlan role:\n" . $self->as_string;
		}
		
		my @children	= map { $_->substitute_impl($model, $b) } @{ $self->children };
		return $self->_impl($model, @children);
	}
	
	sub _impl {
		my $self		= shift;
		my $model		= shift;
		my @children	= @_;
		my $iter_variables	= $self->in_scope_variables;

		return sub {
			if (my $current	= shift(@children)) {
				my $iter	= $current->();
				return Attean::CodeIterator->new(
					item_type => 'Attean::API::Result',
					variables => $iter_variables,
					generator => sub {
						while (blessed($iter)) {
							my $row	= $iter->next();
							if ($row) {
								return $row;
							} else {
								$current	= shift(@children);
								if ($current) {
									$iter	= $current->();
								} else {
									undef $iter;
								}
							}
						}
					},
				);
			} else {
				return Attean::ListIterator->new( item_type => 'Attean::API::Result', variables => [], values => [], );
			}
		};
	}
}

=item * L<Attean::Plan::Extend>

Evaluates a sub-plan, and extends each result by evaluating a set of
expressions, binding the produced values to new variables.

=cut

package Attean::Plan::Extend 0.019 {
	use Moo;
	use Encode;
	use Data::UUID;
	use URI::Escape;
	use Data::Dumper;
	use I18N::LangTags;
	use POSIX qw(ceil floor);
	use Digest::SHA;
	use Digest::MD5 qw(md5_hex);
	use Scalar::Util qw(blessed looks_like_number);
	use List::MoreUtils qw(uniq all);
	use Types::Standard qw(ConsumerOf InstanceOf HashRef);
	use namespace::clean;

	with 'MooX::Log::Any';
	with 'Attean::API::BindingSubstitutionPlan', 'Attean::API::UnaryQueryTree';
	has 'expressions' => (is => 'ro', isa => HashRef[ConsumerOf['Attean::API::Expression']], required => 1);
	
	sub plan_as_string {
		my $self	= shift;
		my @strings	= map { sprintf('?%s ← %s', $_, $self->expressions->{$_}->as_string) } keys %{ $self->expressions };
		return sprintf('Extend { %s }', join(', ', @strings));
	}
	sub tree_attributes { return qw(variable expression) };
	
	sub BUILDARGS {
		my $class		= shift;
		my %args		= @_;
		my $exprs		= $args{ expressions };
		my @vars		= map { @{ $_->in_scope_variables } } @{ $args{ children } };
		my @evars		= (@vars, keys %$exprs);
		
		if (exists $args{in_scope_variables}) {
			Carp::confess "in_scope_variables is computed automatically, and must not be specified in the $class constructor";
		}
		$args{in_scope_variables}	= [@evars];
		return $class->SUPER::BUILDARGS(%args);
	}
	
	sub evaluate_expression {
		my $self	= shift;
		my $model	= shift;
		my $expr	= shift;
		my $r		= shift;
		Carp::confess unless ($expr->can('operator'));
		my $op		= $expr->operator;

		state $true			= Attean::Literal->true;
		state $false		= Attean::Literal->false;
		state $type_roles	= { qw(URI IRI IRI IRI BLANK Blank LITERAL Literal NUMERIC NumericLiteral) };
		state $type_classes	= { qw(URI Attean::IRI IRI Attean::IRI STR Attean::Literal) };
		
		if ($expr->isa('Attean::CastExpression')) {
			my $datatype	= $expr->datatype->value;
			my ($child)	= @{ $expr->children };
			my $term	= $self->evaluate_expression($model, $child, $r);
			die "TypeError $op" unless (blessed($term) and $term->does('Attean::API::Literal'));
			if ($datatype =~ m<^http://www.w3.org/2001/XMLSchema#(integer|float|double)>) {
				my $value	= $term->value;
				my $num;
				if ($datatype eq 'http://www.w3.org/2001/XMLSchema#integer') {
					if ($term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#boolean') {
						$value	= ($value eq 'true') ? '1' : '0';
					} elsif ($term->does('Attean::API::NumericLiteral')) {
						if ($term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#double' or (int($value) != $value)) {
							die "cannot cast to xsd:integer as precision would be lost";
						} else {
							$value	= int($value);
						}
					} elsif (looks_like_number($value)) {
						if ($value =~ /[eE]/) {	# double
							die "cannot to xsd:integer as precision would be lost";
						} elsif (int($value) != $value) {
							die "cannot to xsd:integer as precision would be lost";
						}
					}
					$num	= $value;
				} elsif ($datatype eq 'http://www.w3.org/2001/XMLSchema#decimal') {
					if ($term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#boolean') {
						$value	= ($value eq 'true') ? '1' : '0';
					} elsif ($term->does('Attean::API::NumericLiteral')) {
						if ($term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#double' or (int($value) != $value)) {
							die "cannot cast to xsd:decimal as precision would be lost";
						} else {
							$value	= $term->numeric_value;
						}
					} elsif (looks_like_number($value)) {
						if ($value =~ /[eE]/) {	# double
							die "cannot cast to xsd:decimal as precision would be lost";
						} elsif (int($value) != $value) {
							die "cannot cast to xsd:decimal as precision would be lost";
						}
					}
					$num	= $value;
				} elsif ($datatype =~ m<^http://www.w3.org/2001/XMLSchema#(float|double)$>) {
					my $typename	= $1;
					if ($term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#boolean') {
						$value	= ($value eq 'true') ? '1.0' : '0.0';
					} elsif ($term->does('Attean::API::NumericLiteral')) {
						# no-op
					} elsif (looks_like_number($value)) {
						$value	= +$value;
					} else {
						die "cannot cast unrecognized value '$value' to xsd:$typename";
					}
					$num	= sprintf("%e", $value);
				}
				return Attean::Literal->new(value => $num, datatype => $expr->datatype);
			}
			$self->log->warn("Cast expression unimplemented for $datatype: " . Dumper($expr));
		} elsif ($expr->isa('Attean::ValueExpression')) {
			my $node	= $expr->value;
			if ($node->does('Attean::API::Variable')) {
				return $r->value($node->value);
			} else {
				return $node;
			}
		} elsif ($expr->isa('Attean::UnaryExpression')) {
			my ($child)	= @{ $expr->children };
			my $term	= $self->evaluate_expression($model, $child, $r);
			if ($op eq '!') {
				return ($term->ebv) ? $false : $true;
			} elsif ($op eq '-' or $op eq '+') {
				die "TypeError $op" unless (blessed($term) and $term->does('Attean::API::NumericLiteral'));
				my $v	= $term->numeric_value;
				return Attean::Literal->new( value => eval "$op$v", datatype => $term->datatype );
			}
			die "Unimplemented UnaryExpression evaluation: " . $expr->operator;
		} elsif ($expr->isa('Attean::BinaryExpression')) {
			my $op	= $expr->operator;
			if ($op eq '&&') {
				foreach my $child (@{ $expr->children }) {
					my $term	= $self->evaluate_expression($model, $child, $r);
					unless ($term->ebv) {
						return $false;
					}
				}
				return $true;
			} elsif ($op eq '||') {
				foreach my $child (@{ $expr->children }) {
					my $term	= $self->evaluate_expression($model, $child, $r);
					if (blessed($term) and $term->ebv) {
						return $true;
					}
				}
				return $false;
			} elsif ($op eq '=') {
				my ($lhs, $rhs)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				return $lhs->equals($rhs) ? $true : $false; # TODO: this may not be using value-space comparision for numerics...
			} elsif ($op eq '!=') {
				my ($lhs, $rhs)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				return not($lhs->equals($rhs)) ? $true : $false; # TODO: this may not be using value-space comparision for numerics...
			} elsif ($op =~ m#[<>]=?#) {
				my ($lhs, $rhs)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $cmp	= $lhs->compare($rhs);
				if ($cmp < 0) {
					return ($op =~ /^<=?/) ? $true : $false;
				} elsif ($cmp > 0) {
					return ($op =~ /^>=?/) ? $true : $false;
				} else {
					return ($op =~ /=/) ? $true : $false;
				}
			} elsif ($op =~ m<^[-+*/]$>) {
				my ($lhs, $rhs)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				die "TypeError $op" unless all { blessed($_) and $_->does('Attean::API::NumericLiteral') } ($lhs, $rhs);
				my ($lv, $rv)	= map { $_->numeric_value } ($lhs, $rhs);
				my $type	= $lhs->binary_promotion_type($rhs, $op);
				if ($op eq '+') {
					return Attean::Literal->new(value => ($lv + $rv), datatype => $type);
				} elsif ($op eq '-') {
					return Attean::Literal->new(value => ($lv - $rv), datatype => $type);
				} elsif ($op eq '*') {
					return Attean::Literal->new(value => ($lv * $rv), datatype => $type);
				} elsif ($op eq '/') {
					return Attean::Literal->new(value => ($lv / $rv), datatype => $type);
				}
			}
			
			$self->log->warn("Binary operator $op expression evaluation unimplemented: " . Dumper($expr));
			die "Expression evaluation unimplemented: " . $expr->as_string;
		} elsif ($expr->isa('Attean::FunctionExpression')) {
			my $func	= $expr->operator;
			if ($func eq 'IF') {
				my ($check, @children)	= @{ $expr->children };
				my ($term)		= $self->evaluate_expression($model, $check, $r);
				$self->log->warn($@) if ($@);
				my $expr	= $children[ (blessed($term) and $term->ebv) ? 0 : 1 ];
				my $value	= $self->evaluate_expression($model, $expr, $r);
# 				warn '############# ' . $value->as_string;
				return $value;
			} elsif ($func eq 'COALESCE') {
# 				warn "COALESCE: . " . $r->as_string . "\n";
				foreach my $child (@{ $expr->children }) {
# 					warn '- ' . $child->as_string . "\n";
					my $term	= eval { $self->evaluate_expression($model, $child, $r) };
# 					warn $@ if $@;
					if (blessed($term)) {
# 						warn '    returning ' . $term->as_string . "\n";
						return $term;
					}
				}
# 				warn "    no value\n";
				return;
			}
			
			my @terms	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
			if ($func =~ /^IS([UI]RI|BLANK|LITERAL|NUMERIC)$/) {
				my $role	= "Attean::API::$type_roles->{$1}";
				my $t		= shift(@terms);
				my $ok		= (blessed($t) and $t->does($role));
				return $ok ? $true : $false;
			} elsif ($func eq 'REGEX') {
				my ($string, $pattern, $flags)	= @terms;
# 				my ($string, $pattern, $flags)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				# TODO: ensure that $string is a literal
				($string, $pattern, $flags)	= map { blessed($_) ? $_->value : '' } ($string, $pattern, $flags);
				my $re;
				if ($flags =~ /i/) {
					$re	= qr/$pattern/i;
				} else {
					$re	= qr/$pattern/;
				}
				return ($string =~ $re) ? $true : $false;
			} elsif ($func =~ /^(NOT)?IN$/) {
				my $ok			= ($func eq 'IN') ? $true : $false;
				my $notok		= ($func eq 'IN') ? $false : $true;
# 				my @children	= @{ $expr->children };
				my ($term, @children)	= @terms;
# 				my ($term)		= $self->evaluate_expression($model, shift(@children), $r);
# 				foreach my $child (@{ $expr->children }) {
				foreach my $value (@children) {
# 					my $value	= $self->evaluate_expression($model, $child, $r);
					if ($term->equals($value)) {
						return $ok;
					}
				}
				return $notok;
			} elsif ($func eq 'NOW') {
				my $dt		= DateTime->now;
				my $value	= DateTime::Format::W3CDTF->new->format_datetime( $dt );
				return Attean::Literal->new(value => $value, datatype => 'http://www.w3.org/2001/XMLSchema#dateTime');
			} elsif ($func eq 'STR') {
				my ($term)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				return Attean::Literal->new(value => $term->value);
			} elsif ($func =~ /^[UI]RI$/) { # IRI URI
				my ($term)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				return Attean::IRI->new(value => $term->value, base => $expr->base);
			} elsif ($func eq 'ABS') {
				my ($string)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $value		= abs($string->numeric_value);
				return Attean::Literal->new(value => $value, datatype => $string->datatype);
			} elsif ($func eq 'ROUND') {
				my ($string)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $value		= $string->numeric_value;
				my $mult	= 1;
				if ($value < 0) {
					$mult	= -1;
					$value	= -$value;
				}
				my $round	= $mult * POSIX::floor($value + 0.50000000000008);
				return Attean::Literal->new(value => $round, datatype => $string->datatype);
			} elsif ($func eq 'CEIL') {
				my ($string)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $value		= ceil($string->numeric_value);
				return Attean::Literal->new(value => $value, datatype => $string->datatype);
			} elsif ($func eq 'FLOOR') {
				my ($string)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $value		= floor($string->numeric_value);
				return Attean::Literal->new(value => $value, datatype => $string->datatype);
			} elsif ($func eq 'CONCAT') {
				my @strings		= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
# 				die "CONCAT called with terms that are not argument compatible" unless ($strings[0]->argument_compatible(@strings));
				my %args;
				if (my $l = $strings[0]->language) {
					$args{language}	= $l;
				} else {
					my $dt	= $strings[0]->datatype;
					if ($dt->value eq '') {
						$args{datatype}	= 'http://www.w3.org/2001/XMLSchema#string';
					}
				}
				foreach my $s (@strings) {
					die unless ($s->does('Attean::API::Literal'));
					die if ($s->datatype and not($s->datatype->value =~ m<http://www.w3.org/(1999/02/22-rdf-syntax-ns#langString|2001/XMLSchema#string)>));
					if (my $l2 = $s->language) {
						if (my $l1 = $args{language}) {
							if ($l1 ne $l2) {
								delete $args{language};
							}
						}
					} else {
						delete $args{language};
					}
				}
				my $c	= Attean::Literal->new(value => join('', map { $_->value } @strings), %args);
				return $c;
			} elsif ($func eq 'DATATYPE') {
				my ($string)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				die unless ($string->does('Attean::API::Literal'));
				return $string->datatype;
			} elsif ($func eq 'LANG') {
				my ($string)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				die unless ($string->does('Attean::API::Literal'));
				my $value		= $string->language // '';
				return Attean::Literal->new(value => $value);
			} elsif ($func eq 'LANGMATCHES') {
				my ($term, $pat)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $lang	= $term->value;
				my $match	= $pat->value;
				if ($match eq '*') {
					# """A language-range of "*" matches any non-empty language-tag string."""
					return $lang ? $true : $false;
				} else {
					return (I18N::LangTags::is_dialect_of( $lang, $match )) ? $true : $false;
				}
				
			} elsif ($func eq 'ENCODE_FOR_URI') {
				my ($string)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				return Attean::Literal->new(value => uri_escape_utf8($string->value));
			} elsif ($func =~ /^[LU]CASE$/) {
				my $term		= shift(@terms);
				my $value		= ($func eq 'LCASE') ? lc($term->value) : uc($term->value);
				return Attean::Literal->new(value => $value, $term->construct_args);
			} elsif ($func eq 'STRLANG') {
				my ($term, $lang)	= @terms;
				die unless ($term->does('Attean::API::Literal'));
				die unless ($term->datatype->value =~ m<http://www.w3.org/(1999/02/22-rdf-syntax-ns#langString|2001/XMLSchema#string)>);
				die if ($term->language);
				return Attean::Literal->new(value => $term->value, language => $lang->value);
			} elsif ($func eq 'STRDT') {
				my ($term, $dt)	= @terms;
				die unless ($term->does('Attean::API::Literal'));
				die unless ($term->datatype->value =~ m<http://www.w3.org/(1999/02/22-rdf-syntax-ns#langString|2001/XMLSchema#string)>);
				die if ($term->language);
# 				my ($term, $dt)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				return Attean::Literal->new(value => $term->value, datatype => $dt->value);
			} elsif ($func eq 'REPLACE') {
				my ($term, $pat, $rep)	= @terms;
				die unless ($term->does('Attean::API::Literal'));
				die unless ($term->language or $term->datatype->value =~ m<http://www.w3.org/(1999/02/22-rdf-syntax-ns#langString|2001/XMLSchema#string)>);
# 				my ($term, $pat, $rep)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $value	= $term->value;
				my $pattern	= $pat->value;
				my $replace	= $rep->value;
				die 'REPLACE() called with unsafe ?{} match pattern' if (index($pattern, '(?{') != -1 or index($pattern, '(??{') != -1);
				die 'REPLACE() called with unsafe ?{} replace pattern' if (index($replace, '(?{') != -1 or index($replace, '(??{') != -1);

				$replace	=~ s/\\/\\\\/g;
				$replace	=~ s/\$(\d+)/\$$1/g;
				$replace	=~ s/"/\\"/g;
				$replace	= qq["$replace"];
				no warnings 'uninitialized';
				$value	=~ s/$pattern/"$replace"/eeg;
			# 	warn "==> " . Dumper($value);
				return Attean::Literal->new(value => $value, $term->construct_args);
			} elsif ($func eq 'SUBSTR') {
				my ($term, @args)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $value	= $term->value;
				my @nums;
				foreach my $i (0 .. $#args) {
					my $argnum	= $i + 2;
					my $arg		= $args[ $i ];
					push(@nums, $arg->numeric_value);
				}
				$nums[0]--;
				my $substring	= (scalar(@nums) > 1) ? substr($value, $nums[0], $nums[1]) : substr($value, $nums[0]);
				return Attean::Literal->new(value => $substring, $term->construct_args);
			} elsif ($func eq 'CONTAINS') {
				my ($term, $pattern)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				if ($term->has_language and $pattern->has_language) {
					if ($term->literal_value_language ne $pattern->literal_value_language) {
						die "CONTAINS called with literals of different languages";
					}
				}
				my ($string, $pat)	= map { $_->value } ($term, $pattern);
				my $pos		= index($string, $pat);
				return ($pos >= 0) ? $true : $false;
			} elsif ($func eq 'STRSTARTS') {
				my (@terms)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my ($string, $pat)	= map { $_->value } @terms;
				return (substr($string, 0, length($pat)) eq $pat) ? $true : $false;
			} elsif ($func eq 'STRENDS') {
				my (@terms)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my ($string, $pat)	= map { $_->value } @terms;
				return (substr($string, length($string) - length($pat)) eq $pat) ? $true : $false;
			} elsif ($func eq 'STRAFTER') {
				my ($term, $pat)	= @terms;
				die "STRAFTER called without a literal" unless ($term->does('Attean::API::Literal'));
				die "STRAFTER called without a plain literal" unless ($term->language or $term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string');
				die "$func arguments are not term compatible: " . join(', ', map { $_->as_string } @terms) unless ($term->argument_compatible($pat));
				# TODO: check that the terms are argument compatible
				my $value	= $term->value;
				my $match	= $pat->value;
				my $i		= index($value, $match, 0);
				if ($i < 0) {
					return Attean::Literal->new(value => '');
				} else {
					return Attean::Literal->new(value => substr($value, $i+length($match)), $term->construct_args);
				}
			} elsif ($func eq 'STRBEFORE') {
				my ($term, $pat)	= @terms;
				die "STRBEFORE called without a literal" unless ($term->does('Attean::API::Literal'));
				die "STRBEFORE called without a plain literal" unless ($term->language or $term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string');
				die "$func arguments are not term compatible: " . join(', ', map { $_->as_string } @terms) unless ($term->argument_compatible($pat));
				# TODO: check that the terms are argument compatible
				my $value	= $term->value;
				my $match	= $pat->value;
				my $i		= index($value, $match, 0);
				if ($i < 0) {
					return Attean::Literal->new(value => '');
				} else {
					return Attean::Literal->new(value => substr($value, 0, $i), $term->construct_args);
				}
			} elsif ($func eq 'STRLEN') {
				my ($string)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				return Attean::Literal->new(value => length($string->value), datatype => 'http://www.w3.org/2001/XMLSchema#integer');
			} elsif ($func eq 'MD5') {
				my ($string)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $bytes		= encode('UTF-8', $string->value, Encode::FB_CROAK);
				return Attean::Literal->new(value => md5_hex($bytes));
			} elsif ($func =~ /^SHA(\d+)$/) {
				my $sha	= Digest::SHA->new($1);
				my ($string)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $bytes		= encode('UTF-8', $string->value, Encode::FB_CROAK);
				$sha->add($bytes);
				return Attean::Literal->new(value => $sha->hexdigest);
			} elsif ($func eq 'RAND') {
				return Attean::Literal->new(value => rand(), datatype => 'http://www.w3.org/2001/XMLSchema#double');
			} elsif ($func =~ /^(YEAR|MONTH|DAY|HOUR|MINUTE)S?$/) {
				my $method	= lc($1);
				my ($term)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $dt		= $term->datetime;
				return Attean::Literal->new(value => $dt->$method(), datatype => 'http://www.w3.org/2001/XMLSchema#integer');
			} elsif ($func eq 'SECONDS') {
				my ($term)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $dt		= $term->datetime;
				return Attean::Literal->new(value => $dt->second, datatype => 'http://www.w3.org/2001/XMLSchema#decimal');
			} elsif ($func eq 'TIMEZONE') {
				my ($term)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $dt		= $term->datetime;
				my $tz		= $dt->time_zone;
				die "TIMEZONE called with a dateTime without a timezone" if ($tz->is_floating);
				my $offset	= $tz->offset_for_datetime( $dt );
				my $minus	= '';
				if ($offset < 0) {
					$minus	= '-';
					$offset	= -$offset;
				}

				my $duration	= "${minus}PT";
				if ($offset >= 60*60) {
					my $h	= int($offset / (60*60));
					$duration	.= "${h}H" if ($h > 0);
					$offset	= $offset % (60*60);
				}
				if ($offset >= 60) {
					my $m	= int($offset / 60);
					$duration	.= "${m}M" if ($m > 0);
					$offset	= $offset % 60;
				}
				my $s	= int($offset);
				$duration	.= "${s}S" if ($s > 0 or $duration eq 'PT');
			
				return Attean::Literal->new(value => $duration, datatype => 'http://www.w3.org/2001/XMLSchema#dayTimeDuration');
			} elsif ($func eq 'TZ') {
				my ($term)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				my $dt		= $term->datetime;
				my $tz		= $dt->time_zone;
				return Attean::Literal->new(value =>'') if ($tz->is_floating);
				return Attean::Literal->new('Z') if ($tz->is_utc);
				my $offset	= $tz->offset_for_datetime( $dt );
				my $hours	= 0;
				my $minutes	= 0;
				my $minus	= '+';
				if ($offset < 0) {
					$minus	= '-';
					$offset	= -$offset;
				}

				if ($offset >= 60*60) {
					$hours	= int($offset / (60*60));
					$offset	= $offset % (60*60);
				}
				if ($offset >= 60) {
					$minutes	= int($offset / 60);
					$offset	= $offset % 60;
				}
				my $seconds	= int($offset);
				return Attean::Literal->new(value => sprintf('%s%02d:%02d', $minus, $hours, $minutes));
			} elsif ($func eq 'UUID') {
				my $u		= Data::UUID->new();
				my $uuid	= 'urn:uuid:' . $u->to_string( $u->create() );
				return Attean::IRI->new(value => $uuid);
			} elsif ($func eq 'STRUUID') {
				my $u		= Data::UUID->new();
				return Attean::Literal->new(value => $u->to_string( $u->create() ));
			} elsif ($func eq 'BNODE') {
				if (scalar(@{ $expr->children })) {
					my $string	= $self->evaluate_expression($model, $expr->children->[0], $r);
					my $value	= $string->value;
					my $b		= (exists $r->eval_stash->{'sparql:bnode'}{$value})
								? $r->eval_stash->{'sparql:bnode'}{$value}
								: Attean::Blank->new();
					$r->eval_stash->{'sparql:bnode'}{$value}	= $b;
					return $b;
				} else {
					return Attean::Blank->new();
				}
			} else {
				$self->log->warn("Expression evaluation unimplemented: " . $expr->as_string);
				die "Expression evaluation unimplemented: " . $expr->as_string;
			}
		} elsif ($expr->isa('Attean::ExistsPlanExpression')) {
			my $plan	= $expr->plan;
			my $impl	= $plan->substitute_impl($model, $r);
			my $iter	= $impl->();
			my $found	= 0;
			if (my $row = $iter->next) {
# 				warn "EXISTS found row: " . $row->as_string;
				$found++;
			}
			
			return $found ? Attean::Literal->true : Attean::Literal->false;
		} else {
			$self->log->warn("Expression evaluation unimplemented: " . $expr->as_string);
			die "Expression evaluation unimplemented: " . $expr->as_string;
		}
	}
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my $bind	= shift;
		my %exprs	= %{ $self->expressions };
		my ($impl)	= map { $_->substitute_impl($model, $bind) } @{ $self->children };
		# TODO: substitute variables in the expression
		return $self->_impl($model, $impl, %exprs);
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my %exprs	= %{ $self->expressions };
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		return $self->_impl($model, $impl, %exprs);
	}
	
	sub _impl {
		my $self	= shift;
		my $model	= shift;
		my $impl	= shift;
		my %exprs	= @_;
		my $iter_variables	= $self->in_scope_variables;

		return sub {
			my $iter	= $impl->();
			return Attean::CodeIterator->new(
				item_type => 'Attean::API::Result',
				variables => $iter_variables,
				generator => sub {
					ROW: while (my $r = $iter->next) {
# 						warn 'Extend Row -------------------------------> ' . $r->as_string;
						my %row	= map { $_ => $r->value($_) } $r->variables;
						foreach my $var (keys %exprs) {
							my $expr	= $exprs{$var};
# 							warn "-> $var => "  . $expr->as_string;
							my $term	= eval { $self->evaluate_expression($model, $expr, $r) };
# 							warn $@ if ($@);
							if (blessed($term)) {
# 								warn "===> " . $term->as_string;
								if ($row{ $var } and $term->as_string ne $row{ $var }->as_string) {
									next ROW;
								}
					
								$row{ $var }	= $term;
							}
						}
						return Attean::Result->new( bindings => \%row, eval_stash => $r->eval_stash );
					}
					return;
				}
			);
		};
	}
}

=item * L<Attean::Plan::HashDistinct>

Evaluates a sub-plan, and returns distinct results by checking a persistent
hash of already-seen results.

=cut

package Attean::Plan::HashDistinct 0.019 {
	use Moo;
	use namespace::clean;
	
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	sub plan_as_string { return 'HashDistinct' }
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		my %seen;
		return sub {
			my $iter	= $impl->();
			return $iter->grep(sub { return not($seen{ shift->as_string }++); });
		};
	}
}

=item * L<Attean::Plan::Unique>

Evaluates an already-ordered sub-plan, and returns distinct results by
filtering out sequential duplicates.

=cut

package Attean::Plan::Unique 0.019 {
	use Moo;
	use namespace::clean;
	
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	sub plan_as_string { return 'Unique' }
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		return sub {
			my $iter	= $impl->();
			my $last	= '';
			return $iter->grep(sub {
				my $r	= shift;
				my $s	= $r->as_string;
				my $ok	= $s ne $last;
				$last	= $s;
				return $ok;
			});
		};
	}
}

=item * L<Attean::Plan::Slice>

Evaluates a sub-plan, and returns the results after optionally skipping some
number of results ("offset") and limiting the total number of returned results
("limit").

=cut

package Attean::Plan::Slice 0.019 {
	use Moo;
	use Types::Standard qw(Int);
	use namespace::clean;
	
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	has 'limit' => (is => 'ro', isa => Int, default => -1);
	has 'offset' => (is => 'ro', isa => Int, default => 0);

	sub plan_as_string {
		my $self	= shift;
		my @str;
		push(@str, "Limit=" . $self->limit) if ($self->limit >= 0);
		push(@str, "Offset=" . $self->offset) if ($self->offset > 0);
		return sprintf('Slice { %s }', join(' ', @str));
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		my $offset	= $self->offset;
		my $limit	= $self->limit;
		return sub {
			my $iter	= $impl->();
			$iter		= $iter->offset($offset) if ($offset > 0);
			$iter		= $iter->limit($limit) if ($limit >= 0);
			return $iter;
		};
	}
}

=item * L<Attean::Plan::Project>

Evaluates a sub-plan and returns projected results by only keeping a fixed-set
of variable bindings in each result.

=cut

package Attean::Plan::Project 0.019 {
	use Moo;
	with 'Attean::API::BindingSubstitutionPlan', 'Attean::API::UnaryQueryTree';
	use Types::Standard qw(ArrayRef ConsumerOf);
	has 'variables' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Variable']], required => 1);

	sub BUILDARGS {
		my $class		= shift;
		my %args		= @_;
		my @vars		= map { $_->value } @{ $args{variables} };
		
		if (exists $args{in_scope_variables}) {
			Carp::confess "in_scope_variables is computed automatically, and must not be specified in the $class constructor";
		}
		$args{in_scope_variables}	= \@vars;

		return $class->SUPER::BUILDARGS(%args);
	}
	
# 	sub BUILD {
# 		my $self	= shift;
# 		my @vars	= map { $_->value } @{ $self->variables };
# 		unless (scalar(@vars)) {
# 			Carp::confess "No vars in project?";
# 		}
# 	}
	
	sub plan_as_string {
		my $self	= shift;
		return sprintf('Project { %s }', join(' ', map { '?' . $_->value } @{ $self->variables }));
	}
	sub tree_attributes { return qw(variables) };
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my $bind	= shift;
		my ($impl)	= map { $_->substitute_impl($model, $bind) } @{ $self->children };
		my @vars	= map { $_->value } @{ $self->variables };
		my $iter_variables	= $self->in_scope_variables;

		# TODO: substitute variables in the projection where appropriate
		return sub {
			my $iter	= $impl->();
			return $iter->map(sub {
				my $r	= shift;
				my $b	= { map { my $t	= $r->value($_); $t	? ($_ => $t) : () } @vars };
				return Attean::Result->new( bindings => $b );
			}, $iter->item_type, variables => $iter_variables);
		};
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		my @vars	= map { $_->value } @{ $self->variables };
		my $iter_variables	= $self->in_scope_variables;

		return sub {
			my $iter	= $impl->();
			return $iter->map(sub {
				my $r	= shift;
				my $b	= { map { my $t	= $r->value($_); $t	? ($_ => $t) : () } @vars };
				return Attean::Result->new( bindings => $b );
			}, $iter->item_type, variables => $iter_variables);
		};
	}
}

=item * L<Attean::Plan::OrderBy>

Evaluates a sub-plan and returns the results after fully materializing and
sorting is applied.

=cut

package Attean::Plan::OrderBy 0.019 {
	use Moo;
	use Types::Standard qw(HashRef ArrayRef InstanceOf Bool Str);
	use namespace::clean;
	
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	has 'variables' => (is => 'ro', isa => ArrayRef[Str], required => 1);
	has 'ascending' => (is => 'ro', isa => HashRef[Bool], required => 1);

	sub plan_as_string {
		my $self	= shift;
		my @vars	= @{ $self->variables };
		my $ascending	= $self->ascending;
		my @strings	= map { sprintf('%s(?%s)', ($ascending->{$_} ? 'ASC' : 'DESC'), $_) } @vars;
		return sprintf('Order { %s }', join(', ', @strings));
	}
	
	sub sort_rows {
		my $self		= shift;
		my $vars		= shift;
		my $ascending	= shift;
		my $rows		= shift;
		my @sorted		= map { $_->[0] } sort {
			my ($ar, $avalues)	= @$a;
			my ($br, $bvalues)	= @$b;
			my $c	= 0;
			foreach my $i (0 .. $#{ $vars }) {
				my $ascending	= $ascending->{ $vars->[$i] };
				my ($av, $bv)	= map { $_->[$i] } ($avalues, $bvalues);
				$c		= $av ? $av->compare($bv) : 1;
				$c		*= -1 unless ($ascending);
				last unless ($c == 0);
			}
			$c
		} map {
			my $r = $_;
			[$r, [map { $r->value($_) } @$vars]]
		} @$rows;
		return @sorted;
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my $vars	= $self->variables;
		my $ascending	= $self->ascending;
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		my $iter_variables	= $self->in_scope_variables;

		return sub {
			my $iter	= $impl->();
			my @rows	= $iter->elements;
			my @sorted	= $self->sort_rows($vars, $ascending, \@rows);
			return Attean::ListIterator->new(
				values => \@sorted,
				variables => $iter_variables,
				item_type => $iter->item_type
			);
		}
	}
}

=item * L<Attean::Plan::Service>

Evaluates a SPARQL query against a remove endpoint.

=cut

package Attean::Plan::Service 0.019 {
	use Moo;
	use Types::Standard qw(ConsumerOf Bool Str);
	use namespace::clean;

	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';

	has 'endpoint' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
	has 'silent' => (is => 'ro', isa => Bool, default => 0);
	has 'sparql' => (is => 'ro', isa => Str, required => 1);

	sub plan_as_string {
		my $self	= shift;
		my $sparql	= $self->sparql;
		$sparql		=~ s/\s+/ /g;
		return sprintf('Service <%s> %s', $self->endpoint->as_string, $sparql);
	}
	
	sub tree_attributes { return qw(endpoint) };
	sub impl {
		my $self	= shift;
		my $model	= shift;
		die __PACKAGE__ . " unimplemented";
	}
}

=item * L<Attean::Plan::Table>

Returns a constant set of results.

=cut

package Attean::Plan::Table 0.019 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	use namespace::clean;

	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';

	has variables => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Variable']]);
	has rows => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Result']]);

	sub tree_attributes { return qw(variables rows) };
	sub plan_as_string {
		my $self	= shift;
		my $level	= shift;
		my $indent	= '  ' x ($level + 1);
		my $vars	= join(', ', map { "?$_" } @{ $self->in_scope_variables });
		my $s		= "Table (" . $vars . ")";
		foreach my $row (@{ $self->rows }) {
			$s	.= "\n-${indent} " . $row->as_string;
		}
		return $s;
	}
	
	sub BUILDARGS {
		my $class		= shift;
		my %args		= @_;
		my @vars		= map { $_->value } @{ $args{variables} };
		
		if (exists $args{in_scope_variables}) {
			Carp::confess "in_scope_variables is computed automatically, and must not be specified in the $class constructor";
		}
		$args{in_scope_variables}	= \@vars;

		return $class->SUPER::BUILDARGS(%args);
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my $rows	= $self->rows;
		my $iter_variables	= $self->in_scope_variables;

		return sub {
			return Attean::ListIterator->new(
				item_type => 'Attean::API::Result',
				variables => $iter_variables,
				values => $rows
			);
		};
	}
}

=item * L<Attean::Plan::Iterator>

Returns a constant set of results.

Be aware that if the iterator being wrapped is not repeatable (consuming the
L<Attean::API::RepeatableIterator> role), then this plan may only be evaluated
once.

A size estimate may be given if it is available. If the iterator is an
L<Attean::ListIterator>, the size of that iterator will be used.

=cut

package Attean::Plan::Iterator 0.019 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf Int);
	use namespace::clean;

	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';

	has iterator		=> (is => 'ro', isa => ConsumerOf['Attean::API::ResultIterator']);
	has size_estimate	=> (is => 'lazy', isa => Int, predicate => 1);

	sub _build_size_estimate {
		my $self = shift;
		my $iter = $self->iterator;
		if ($iter->isa('Attean::ListIterator')) {
			return $iter->size;
		}
	}


	sub tree_attributes { return qw(iterator) };
	sub plan_as_string {
		my $self	= shift;
		my $level	= shift;
		my $indent	= '  ' x ($level + 1);
		my $string = 'Iterator (';
		$string	.= join(', ', map { "?$_" } @{ $self->in_scope_variables });
		if ($self->has_size_estimate) {
			$string .= ' with ' . $self->size_estimate . ' elements';
		}
		$string	.= ')';
		return $string;
	}
	
	sub BUILDARGS {
		my $class		= shift;
		my %args		= @_;
		my $vars		= $args{iterator}->variables;
		
		if (exists $args{in_scope_variables}) {
			Carp::confess "in_scope_variables is computed automatically, and must not be specified in the $class constructor";
		}
		$args{in_scope_variables}	= $vars;

		return $class->SUPER::BUILDARGS(%args);
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my $iter	= $self->iterator;

		return sub {
			if ($iter->does('Attean::API::RepeatableIterator')) {
				$iter->reset;
			}
			return $iter;
		};
	}
}

=item * L<Attean::Plan::ALPPath>

=cut

package Attean::Plan::ALPPath 0.019 {
	use Moo;
	use Attean::TreeRewriter;
	use Types::Standard qw(ArrayRef ConsumerOf);
	use namespace::clean;

	has 'subject'		=> (is => 'ro', required => 1);
	has 'object'		=> (is => 'ro', required => 1);
	has 'graph'			=> (is => 'ro', required => 1);
	has 'step_begin'	=> (is => 'ro', required => 1);
	has 'step_end'		=> (is => 'ro', required => 1);
	has 'skip'			=> (is => 'ro', required => 1, default => 0);
# 	has 'children'		=> (is => 'ro', isa => ConsumerOf['Attean::API::BindingSubstitutionPlan'], required => 1);
	
	with 'Attean::API::BindingSubstitutionPlan', 'Attean::API::NullaryQueryTree';

	sub tree_attributes { return qw(subject object graph) };
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	
	sub plan_as_string {
		my $self	= shift;
		my @strings;
		push(@strings, sprintf('%s ← %s', map { $_->ntriples_string } ($self->subject, $self->step_begin)));
		push(@strings, sprintf('%s ← %s', map { $_->ntriples_string } ($self->object, $self->step_end)));
		return sprintf('ALPPath %s', join(', ', @strings));
	}
	
	sub BUILDARGS {
		my $class		= shift;
		my %args		= @_;
		my @vars		= map { $_->value } grep { $_->does('Attean::API::Variable') } (@args{qw(subject object)});
		
		if (exists $args{in_scope_variables}) {
			Carp::confess "in_scope_variables is computed automatically, and must not be specified in the $class constructor";
		}
		$args{in_scope_variables}	= \@vars;

		return $class->SUPER::BUILDARGS(%args);
	}
	
	sub alp {
		my $model	= shift;
		my $graph	= shift;
		my $skip	= shift;
		my $x		= shift;
		my $path	= shift;
		my $v		= shift;
		my $start	= shift;
		my $end		= shift;
		my $bind	= shift;
		if (exists $v->{$x->as_string}) {
			return;
		}
		my $binding	= Attean::Result->new( bindings => { $start => $x } )->join($bind);
		unless ($binding) {
			return;
		}
		
		if ($skip) {
			$skip--;
		} else {
			$v->{$x->as_string}	= $x;
		}
		
		my $impl	= $path->substitute_impl($model, $binding);
		my $iter	= $impl->();
		while (my $row = $iter->next()) {
			my $n	= $row->value($end);
			alp($model, $graph, $skip, $n, $path, $v, $start, $end, $bind);
		}
	}
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my $bind	= shift;
		my $path	= $self->children->[0];
		my $subject	= $self->subject;
		my $object	= $self->object;
		my $graph	= $self->graph;
		my $start	= $self->step_begin->value;
		my $end		= $self->step_end->value;
		my $skip	= $self->skip;
		my $iter_variables	= $self->in_scope_variables;

		for ($subject, $object) {
			if ($_->does('Attean::API::Variable')) {
				my $name	= $_->value;
				if (my $node = $bind->value($name)) {
					$_	= $node;
				}
			}
		}
		
		my $s_var	= $subject->does('Attean::API::Variable');
		my $o_var	= $object->does('Attean::API::Variable');
		if ($s_var and $o_var) {
			return sub {
				my $nodes	= $model->graph_nodes($graph);
				my @rows;
				while (my $n = $nodes->next) {
					my %seen;
					alp($model, $graph, $skip, $n, $path, \%seen, $start, $end, $bind);
					foreach my $term (values %seen) {
						my $b	= Attean::Result->new( bindings => {
							$subject->value => $n,
							$object->value => $term,
						} );
						push(@rows, $b);
					}
				}
				return Attean::ListIterator->new(
					item_type => 'Attean::API::Result',
					variables => $iter_variables,
					values => \@rows,
				);
			};
		} elsif ($o_var) {
			return sub {
				my %seen;
				alp($model, $graph, $skip, $subject, $path, \%seen, $start, $end, $bind);
				my @rows	= map { Attean::Result->new( bindings => { $object->value => $_ } ) } (values %seen);
				return Attean::ListIterator->new(
					item_type => 'Attean::API::Result',
					variables => $iter_variables,
					values => \@rows,
				);
			};
		} elsif ($s_var) {
			die "ALP for FB should never occur in a plan (should be inversed during planning)";
		} else {
			return sub {
				my %seen;
				alp($model, $graph, $skip, $subject, $path, \%seen, $start, $end, $bind);
				if (exists $seen{ $object->as_string }) {
					return Attean::ListIterator->new(
						item_type => 'Attean::API::Result',
						variables => $iter_variables,
						values => [Attean::Result->new()]
					);
				} else {
					return Attean::ListIterator->new(
						item_type => 'Attean::API::Result',
						variables => $iter_variables,
						values => []
					);
				}
			};
		}
	}
}

package Attean::Plan::ZeroOrOnePath 0.019 {
	use Moo;
	use Attean::TreeRewriter;
	use Types::Standard qw(ArrayRef ConsumerOf);
	use namespace::clean;

	has 'subject'		=> (is => 'ro', required => 1);
	has 'object'		=> (is => 'ro', required => 1);
	has 'graph'			=> (is => 'ro', required => 1);
	
	with 'Attean::API::BindingSubstitutionPlan', 'Attean::API::NullaryQueryTree';

	sub BUILDARGS {
		my $class		= shift;
		my %args		= @_;
		my @vars		= map { $_->value } grep { $_->does('Attean::API::Variable') } (@args{qw(subject object)});
		
		if (exists $args{in_scope_variables}) {
			Carp::confess "in_scope_variables is computed automatically, and must not be specified in the $class constructor";
		}
		$args{in_scope_variables}	= \@vars;

		return $class->SUPER::BUILDARGS(%args);
	}
	
	sub tree_attributes { return qw(subject object) };
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	
	sub plan_as_string { return 'ZeroOrOnePath' }
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my $bind	= shift;
		my ($impl)	= map { $_->substitute_impl($model, $bind) } @{ $self->children };
		my $iter_variables	= $self->in_scope_variables;

		my $subject	= $self->subject;
		my $object	= $self->object;
		my $graph	= $self->graph;
		for ($subject, $object) {
			if ($_->does('Attean::API::Variable')) {
				my $name	= $_->value;
				if (my $node = $bind->value($name)) {
					$_	= $node;
				}
			}
		}

		my $s_var	= $subject->does('Attean::API::Variable');
		my $o_var	= $object->does('Attean::API::Variable');
		return sub {
			my @extra;
			if ($s_var and $o_var) {
				my $nodes	= $model->graph_nodes($graph);
				while (my $n = $nodes->next) {
					push(@extra, Attean::Result->new( bindings => { map { $_->value => $n } ($subject, $object) } ));
				}
			} elsif ($s_var) {
				push(@extra, Attean::Result->new( bindings => { $subject->value => $object } ));
			} elsif ($o_var) {
				push(@extra, Attean::Result->new( bindings => { $object->value => $subject } ));
			} else {
				if (0 == $subject->compare($object)) {
					push(@extra, Attean::Result->new( bindings => {} ));
				}
			}
			my $iter	= $impl->();
			my %seen;
			return Attean::CodeIterator->new(
				item_type => 'Attean::API::Result',
				variables => $iter_variables,
				generator => sub {
					while (scalar(@extra)) {
						my $r	= shift(@extra);
						unless ($seen{$r->as_string}++) {
							return $r;
						}
					}
					while (my $r = $iter->next()) {
						return unless ($r);
						if ($seen{$r->as_string}++) {
							next;
						}
						return $r;
					}
				}
			);
		};
	}
}

=item * L<Attean::Plan::Exists>

Returns an iterator containing a single boolean term indicating whether any
results were produced by evaluating the sub-plan.

=cut

package Attean::Plan::Exists 0.019 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	use namespace::clean;
	
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	has variables => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Variable']]);
	has rows => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Result']]);

	sub tree_attributes { return qw(variables rows) };
	sub plan_as_string { return 'Exists' }
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		return sub {
			my $iter	= $impl->();
			my $term	= ($iter->next) ? Attean::Literal->true : Attean::Literal->false;
			return Attean::ListIterator->new(values => [$term], item_type => 'Attean::API::Term');
		}
	}
}

=item * L<Attean::Plan::Aggregate>

=cut

package Attean::Plan::Aggregate 0.019 {
	use Moo;
	use Encode;
	use Data::UUID;
	use URI::Escape;
	use I18N::LangTags;
	use POSIX qw(ceil floor);
	use Digest::SHA;
	use Digest::MD5 qw(md5_hex);
	use Scalar::Util qw(blessed);
	use List::MoreUtils qw(uniq);
	use Types::Standard qw(ConsumerOf InstanceOf HashRef ArrayRef);
	use namespace::clean;

	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'aggregates' => (is => 'ro', isa => HashRef[ConsumerOf['Attean::API::Expression']], required => 1);
	has 'groups' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Expression']], required => 1);
	
	sub plan_as_string {
		my $self	= shift;
		my @astrings	= map { sprintf('?%s ← %s', $_, $self->aggregates->{$_}->as_string) } keys %{ $self->aggregates };
		my @gstrings	= map { sprintf('%s', $_->as_string) } @{ $self->groups };
		return sprintf('Aggregate { %s } Groups { %s }', join(', ', @astrings), join(', ', @gstrings));
	}
	sub tree_attributes { return qw(aggregates groups) };
	
	sub BUILDARGS {
		my $class		= shift;
		my %args		= @_;
		my $aggs		= $args{ aggregates };
		my @vars		= map { $_->value } grep { $_->does('Attean::API::Variable') } map { $_->value } @{ $args{groups} // [] };
		my @evars		= (@vars, keys %$aggs);
		if (exists $args{in_scope_variables}) {
			Carp::confess "in_scope_variables is computed automatically, and must not be specified in the $class constructor";
		}
		$args{in_scope_variables}	= [@evars];
		return $class->SUPER::BUILDARGS(%args);
	}
	
	sub evaluate_aggregate {
		my $self	= shift;
		my $model	= shift;
		my $expr	= shift;
		my $rows	= shift;
		my $op		= $expr->operator;
		my ($e)		= @{ $expr->children };
# 		my @children	= map { Attean::Plan::Extend->evaluate_expression($model, $_, $r) } @{ $expr->children };
# 		warn "$op — " . join(' ', map { $_->as_string } @children);
		if ($op eq 'COUNT') {
			my $count	= 0;
			foreach my $r (@$rows) {
				if ($e) {
					my $term	= Attean::Plan::Extend->evaluate_expression($model, $e, $r);
					if ($term) {
						$count++;
					}
				} else {
					# This is the special-case branch for COUNT(*)
					$count++;
				}
			}
			return Attean::Literal->new(value => $count, datatype => 'http://www.w3.org/2001/XMLSchema#integer');
		} elsif ($op eq 'SUM') {
			my @cmp;
			my @terms;
			foreach my $r (@$rows) {
				my $term	= Attean::Plan::Extend->evaluate_expression($model, $e, $r);
				if ($term->does('Attean::API::NumericLiteral')) {
					push(@terms, $term);
				}
			}
			my $lhs	= shift(@terms);
			while (my $rhs = shift(@terms)) {
				my $type	= $lhs->binary_promotion_type($rhs, '+');
				my ($lv, $rv)	= map { $_->numeric_value } ($lhs, $rhs);
				$lhs	= Attean::Literal->new(value => ($lv + $rv), datatype => $type);
			}
			return $lhs;
		} elsif ($op eq 'AVG') {
			my @cmp;
			my $count	= 0;
			my $all_ints	= 1;
			my @terms;
			foreach my $r (@$rows) {
				my $term	= Attean::Plan::Extend->evaluate_expression($model, $e, $r);
				die unless ($term->does('Attean::API::NumericLiteral'));
				push(@terms, $term);
				$count++;
			}
			my $lhs	= shift(@terms);
			while (my $rhs = shift(@terms)) {
				my $type	= $lhs->binary_promotion_type($rhs, '+');
				my ($lv, $rv)	= map { $_->numeric_value } ($lhs, $rhs);
				$lhs	= Attean::Literal->new(value => ($lv + $rv), datatype => $type);
			}
			
			my $rhs			= Attean::Literal->new(value => $count, datatype => 'http://www.w3.org/2001/XMLSchema#integer');
			my ($lv, $rv)	= map { $_->numeric_value } ($lhs, $rhs);
			my $type	= $lhs->binary_promotion_type($rhs, '/');
			return Attean::Literal->new(value => ($lv / $rv), datatype => $type);
		} elsif ($op eq 'SAMPLE') {
			foreach my $r (@$rows) {
				my $term	= Attean::Plan::Extend->evaluate_expression($model, $e, $r);
				return $term if (blessed($term));
			}
		} elsif ($op =~ /^(MIN|MAX)$/) {
			my @cmp;
			foreach my $r (@$rows) {
				my $term	= Attean::Plan::Extend->evaluate_expression($model, $e, $r);
				push(@cmp, $term);
			}
			@cmp	= sort { $a->compare($b) } @cmp;
			return ($op eq 'MIN') ? shift(@cmp) : pop(@cmp);
		} elsif ($op eq 'GROUP_CONCAT') {
			my $sep	= $expr->scalar_vars->{seperator} // ' ';
			my @values;
			foreach my $r (@$rows) {
				my $term	= Attean::Plan::Extend->evaluate_expression($model, $e, $r);
				push(@values, $term->value);
			}
			my $string	= join($sep, @values);
			return Attean::Literal->new(value => $string);
		}
		die "$op not implemented";
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my %aggs	= %{ $self->aggregates };
		my @groups	= @{ $self->groups };
		my $iter_variables	= $self->in_scope_variables;

		my $group_template_generator	= sub {
			my $r	= shift;
			my %components;
			foreach my $g (@groups) {
				if ($g->isa('Attean::ValueExpression')) {
					my $value	= $g->value;
					if ($value->isa('Attean::Variable')) {
						my $var	= $value->value;
						my $value	= eval { Attean::Plan::Extend->evaluate_expression($model, $g, $r) };
						if (blessed($value)) {
							$components{$var}	= $value;
						}
					}
				}
			}
			return %components;
		};
		my $group_key_generator	= sub {
			my $r	= shift;
			my @components;
			foreach my $g (@groups) {
				my $value	= eval { Attean::Plan::Extend->evaluate_expression($model, $g, $r) };
				my $key		= blessed($value) ? $value->as_string : '';
				push(@components, $key);
			}
			my $group	= join('|', @components);
			return $group;
		};
		
		my $rank;
		while (my($var, $agg) = each(%aggs)) {
			if ($agg->operator eq 'RANK') {
				$rank	= $var;
			}
		}
		
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		my %row_groups;
		my %group_templates;
		return sub {
			my $iter	= $impl->();
			while (my $r = $iter->next) {
				my $group_key	= $group_key_generator->($r);
				push(@{ $row_groups{ $group_key } }, $r);
				unless (exists $group_templates{ $group_key }) {
					$group_templates{ $group_key }	= { $group_template_generator->($r) };
				}
			}
			my @group_keys	= keys %row_groups;
			
			# SPARQL evaluation of aggregates over an empty input sequence should
			# result in an empty result <http://answers.semanticweb.com/questions/17410/semantics-of-sparql-aggregates>
			
			my @results;
			unless (scalar(@group_keys)) {
				push(@group_keys, '');
				$row_groups{''}			= [];
				$group_templates{''}	= {};
			}
			foreach my $group (@group_keys) {
				my %row	= %{ $group_templates{ $group } };
				my $rows	= $row_groups{$group};
				if (defined $rank) {
					my $agg			= $aggs{$rank};
					my $ascending	= $agg->scalar_vars->{ascending} // {};
					my $vars	= [map { $_->value->value } @{ $agg->children }];
					# TODO: support ordering by complex expressions in $vars, not just ValueExpressions with variables
					my @sorted	= Attean::Plan::OrderBy->sort_rows($vars, $ascending, $rows);
					my $ord		= 0;
					foreach my $row (@sorted) {
						my %b	= %{ $row->bindings };
						$b{ $rank }	= Attean::Literal->integer($ord++);
						my $r	= Attean::Result->new( bindings => \%b );
						push(@results, $r);
					}
				} else {
					foreach my $var (keys %aggs) {
						my $expr	= $aggs{$var};
						my $value	= eval { $self->evaluate_aggregate($model, $expr, $rows) };
						if ($value) {
							$row{$var}	= $value;
						}
					}
					my $result	= Attean::Result->new( bindings => \%row );
					push(@results, $result);
				}
			}
			return Attean::ListIterator->new(
				values => \@results,
				variables => $iter_variables,
				item_type => 'Attean::API::Result'
			);
		};
	}
}

package Attean::Plan::Sequence 0.019 {
	use Moo;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(ConsumerOf ArrayRef);
	use namespace::clean;
	
	with 'Attean::API::Plan', 'Attean::API::QueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	sub plan_as_string { return 'Sequence'; }
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my @children	= map { $_->impl($model) } @{ $self->children };
		return sub {
			foreach my $child (@children) {
				my $iter	= $child->();
				$iter->elements;
			}
			return Attean::ListIterator->new(values => [Attean::Literal->true], item_type => 'Attean::API::Term');
		};
	}
}

package Attean::Plan::Clear 0.019 {
	use Moo;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(ConsumerOf ArrayRef);
	use namespace::clean;
	
	with 'Attean::API::Plan', 'Attean::API::NullaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	has 'graphs' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Term']]);

	sub plan_as_string {
		my $self	= shift;
		my $level	= shift;
		my $indent	= '  ' x (1+$level);
		my $s		= sprintf("Clear { %d graphs }", scalar(@{ $self->graphs }));
		foreach my $g (@{ $self->graphs }) {
			my $name	= $g->as_sparql;
			chomp($name);
			$s	.= "\n-${indent} $name";
		}
		return $s;
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my $graphs	= $self->graphs;
		return sub {
			foreach my $g (@$graphs) {
				$model->clear_graph($g);
			}
			return Attean::ListIterator->new(values => [Attean::Literal->true], item_type => 'Attean::API::Term');
		};
	}
}

package Attean::Plan::Drop 0.019 {
	use Moo;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(ConsumerOf ArrayRef);
	use namespace::clean;
	
	with 'Attean::API::Plan', 'Attean::API::NullaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	has 'graphs' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Term']]);

	sub plan_as_string {
		my $self	= shift;
		my $level	= shift;
		my $indent	= '  ' x (1+$level);
		my $s		= sprintf("Drop { %d graphs }", scalar(@{ $self->graphs }));
		foreach my $g (@{ $self->graphs }) {
			$s	.= "\n-${indent} " . $g->as_sparql;
		}
		return $s;
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my $graphs	= $self->graphs;
		return sub {
			foreach my $g (@$graphs) {
				$model->drop_graph($g);
			}
			return Attean::ListIterator->new(values => [Attean::Literal->true], item_type => 'Attean::API::Term');
		};
	}
}

package Attean::Plan::TripleTemplateToModelQuadMethod 0.019 {
	use Moo;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(ConsumerOf Str ArrayRef HashRef);
	use namespace::clean;
	
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	has 'order' => (is => 'ro', isa => ArrayRef[Str], required => 1);
	has 'patterns' => (is => 'ro', isa => HashRef[ArrayRef[ConsumerOf['Attean::API::TripleOrQuadPattern']]], required => 1);
	has 'graph' => (is => 'ro', isa => ConsumerOf['Attean::API::Term']);

	sub plan_as_string {
		my $self	= shift;
		my $level	= shift;
		my $indent	= '  ' x (1+$level);
		my $s		= sprintf("Template-to-Model { Default graph: %s }", $self->graph->as_string);
		foreach my $method (@{ $self->order }) {
			my $pattern	= $self->patterns->{ $method };
			$s	.= "\n-${indent} Method: ${method}";
			foreach my $p (@$pattern) {
				$s	.= "\n-${indent}   " . $p->as_string;
			}
		}
		return $s;
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my $child	= $self->children->[0]->impl($model);
		
		my $graph	= $self->graph;
		my @order	= @{ $self->order };
		my $method	= shift(@order);
		my $pattern	= $self->patterns->{ $method };

		return sub {
			my $iter	= $child->();
			my @results;
			while (my $t = $iter->next) {
				if (scalar(@order)) {
					push(@results, $t);
				}
				foreach my $p (@$pattern) {
					my $q		= $p->apply_bindings($t);
					my $quad	= $q->does('Attean::API::QuadPattern') ? $q : $q->as_quad_pattern($graph);
					if ($quad->is_ground) {
						# warn "# $method: " . $quad->as_string . "\n";
						$model->$method($quad->as_quad);
					} else {
						# warn "not ground: " . $quad->as_string;
					}
				}
			}
			foreach my $method (@order) {
				my $pattern	= $self->patterns->{ $method };
				foreach my $t (@results) {
					foreach my $p (@$pattern) {
						my $q		= $p->apply_bindings($t);
						my $quad	= $q->does('Attean::API::QuadPattern') ? $q : $q->as_quad_pattern($graph);
						if ($quad->is_ground) {
							# warn "# $method: " . $quad->as_string . "\n";
							$model->$method($quad->as_quad);
						} else {
							# warn "not ground: " . $quad->as_string;
						}
					}
				}
			}
			return Attean::ListIterator->new(values => [Attean::Literal->integer($model->size)], item_type => 'Attean::API::Term');
		};
	}
}

package Attean::Plan::Load 0.019 {
	use Moo;
	use Encode;
	use LWP::UserAgent;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Bool Str);
	use namespace::clean;
	
	with 'Attean::API::Plan', 'Attean::API::NullaryQueryTree';
	with 'Attean::API::UnionScopeVariablesPlan';

	has 'silent' => (is => 'ro', isa => Bool, default => 0);
	has 'url' => (is => 'ro', isa => Str);

	sub plan_as_string {
		my $self	= shift;
		return sprintf("Load { %s }", $self->url);
	}
	
	sub impl {
		my $self	= shift;
		my $url		= $self->url;
		my $ua		= LWP::UserAgent->new();
		my $silent	= $self->silent;
		my $accept	= Attean->acceptable_parsers( handles => 'Attean::API::Triple' );
		$ua->default_headers->push_header( 'Accept' => $accept );
		return sub {
			my $resp	= $ua->get( $url );
			if ($resp->is_success) {
				my $ct		= $resp->header('Content-Type');
				if (my $pclass = Attean->get_parser( media_type => $ct )) {
					my $p		= $pclass->new();
					my $str		= $resp->decoded_content;
					my $bytes	= encode('UTF-8', $str, Encode::FB_CROAK);
					my $iter	= $p->parse_iter_from_bytes( $bytes );
					return $iter;
				}
			}
			
			if ($silent) {
				return Attean::ListIterator->new(values => [], item_type => 'Attean::API::Triple');
			} else {
				die "Failed to load url: " . $resp->status_line;
			}
		};
	}
}

# Create(iri)

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

Copyright (c) 2014--2018 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
