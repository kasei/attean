use v5.14;
use warnings;

=head1 NAME

Attean::API::Query - Utility package defining query-related roles

=head1 VERSION

This document describes Attean::API::Query version 0.031

=head1 SYNOPSIS

  use v5.14;
  use Attean;

=head1 DESCRIPTION

This is a utility package for defining query-related roles:

=over 4

=item * L<Attean::API::DirectedAcyclicGraph>

=cut

package Attean::API::DirectedAcyclicGraph 0.031 {
	use Scalar::Util qw(refaddr);
	use Types::Standard qw(ArrayRef ConsumerOf);

	use Moo::Role;

# =item C<< children >>
# 
# An ARRAY reference of L<Attean::API::DirectedAcyclicGraph> objects.
# 
# =back
# 
# =cut

	has 'children' => (
		is => 'ro',
		isa => ArrayRef[ConsumerOf['Attean::API::DirectedAcyclicGraph']],
		default => sub { [] },
	);
	
# =item C<< is_leaf >>
# 
# Returns true if the referent has zero C<< children >>, false otherwise.
# 
# =cut

	sub is_leaf {
		my $self	= shift;
		return not(scalar(@{ $self->children }));
	}
	
# =item C<< walk( prefix => \&pre_cb, postfix => \&pre_cb ) >>
# 
# Walks the graph rooted at the referent, calling C<< &pre_cb >> (if supplied)
# before descending, and C<< &post_cb >> (if supplied) after descending. The
# callback functions are passed the current graph walk node as the single
# argument.
# 
# =cut

	sub walk {
		my $self	= shift;
		my %args	= @_;
		my $level	= $args{ level } // 0;
		my $parent	= $args{ parent };
		if (my $cb = $args{ prefix }) {
			$cb->( $self, $level, $parent );
		}
		foreach my $c (@{ $self->children }) {
			$c->walk( %args, level => (1+$level), parent => $self );
		}
		if (my $cb = $args{ postfix }) {
			$cb->( $self, $level, $parent );
		}
	}
	
# =item C<< has_only_subtree_types( @classes ) >>
# 
# Returns true if the invocant and all of its sub-trees are instances of only
# the listed classes, false otherwise.
# 
# =cut

	sub has_only_subtree_types {
		my $self	= shift;
		my @types	= @_;
		my %types	= map { $_ => 1 } @types;
		return 0 unless (exists $types{ ref($self) });
		
		my %classes;
		$self->walk( prefix => sub {
			my $plan	= shift;
			$classes{ref($plan)}++;
		});
		foreach my $type (@types) {
			delete $classes{$type};
		}
		my @keys	= keys %classes;
		return (scalar(@keys) == 0) ? 1 : 0;
	}

# =item C<< cover( prefix => \&pre_cb, postfix => \&pre_cb ) >>
# 
# Similar to C<< walk >>, walks the graph rooted at the referent, calling
# C<< &pre_cb >> (if supplied) before descending, and C<< &post_cb >> (if
# supplied) after descending. However, unlike C<< walk >>, each node in the graph
# is visited only once.
# 
# =cut

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

	sub subpatterns_of_type {
		my $self	= shift;
		my @types	= @_;
		my @p;
		$self->walk( prefix => sub {
			my $a	= shift;
			foreach my $t (@types) {
				push(@p, $a) if ($a->isa($t) or $a->does($t));
			}
		});
		return @p;
	}
}

package Attean::API::SPARQLSerializable 0.031 {
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;
	use Encode qw(decode_utf8);
	use Attean::API::Iterator;
	use Attean::API::Serializer;
	use AtteanX::Serializer::SPARQL;

	use Moo::Role;

	requires 'sparql_tokens';
	
	sub as_sparql {
		my $self	= shift;
		my $s		= AtteanX::Serializer::SPARQL->new();
		my $i		= $self->sparql_tokens;
		my $bytes	= $s->serialize_iter_to_bytes($i);
		return decode_utf8($bytes);
	}
	
	sub sparql_subtokens {
		my $self	= shift;
		if ($self->does('Attean::API::SPARQLQuerySerializable')) {
			my $l	= AtteanX::SPARQL::Token->lbrace;
			my $r	= AtteanX::SPARQL::Token->rbrace;
			my @tokens;
			push(@tokens, $l);
			push(@tokens, $self->sparql_tokens->elements);
			push(@tokens, $r);
			return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
		} else {
			return $self->sparql_tokens;
		}
	}
	
	sub dataset_tokens {
		my $self	= shift;
		my $dataset	= shift;
		my @default	= @{ $dataset->{ default } || [] };
		my @named	= @{ $dataset->{ named } || [] };
		my $has_dataset	= (scalar(@default) + scalar(@named));
		my @tokens;
		if ($has_dataset) {
			my $from		= AtteanX::SPARQL::Token->keyword('FROM');
			my $named		= AtteanX::SPARQL::Token->keyword('NAMED');
			foreach my $i (sort { $a->as_string cmp $b->as_string } @default) {
				push(@tokens, $from);
				push(@tokens, $i->sparql_tokens->elements);
			}
			foreach my $i (sort { $a->as_string cmp $b->as_string } @named) {
				push(@tokens, $from);
				push(@tokens, $named);
				push(@tokens, $i->sparql_tokens->elements);
			}
		}
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
	
	sub query_tokens {
		my $self	= shift;
		my %args	= @_;
		my $dataset	= $args{dataset} || {};
		
		my $as		= AtteanX::SPARQL::Token->keyword('AS');
		my $lparen	= AtteanX::SPARQL::Token->lparen;
		my $rparen	= AtteanX::SPARQL::Token->rparen;
		
		my $algebra	= $self;
		
		my %modifiers;
		my $form	= 'SELECT';
		if ($algebra->isa('Attean::Algebra::Ask')) {
			$form	= 'ASK';
			($algebra)	= @{ $algebra->children };
		} elsif ($algebra->isa('Attean::Algebra::Describe')) {
			$form	= 'DESCRIBE';
			$modifiers{describe}	= $algebra->terms;
			($algebra)	= @{ $algebra->children };
		} elsif ($algebra->isa('Attean::Algebra::Construct')) {
			$form	= 'CONSTRUCT';
			$modifiers{construct}	= $algebra->triples;
			($algebra)	= @{ $algebra->children };
		}
		
		unless ($form eq 'CONSTRUCT' or $form eq 'DESCRIBE') {
			while ($algebra->isa('Attean::Algebra::Extend') or $algebra->isa('Attean::Algebra::Group') or $algebra->isa('Attean::Algebra::OrderBy') or $algebra->isa('Attean::Algebra::Distinct') or $algebra->isa('Attean::Algebra::Reduced') or $algebra->isa('Attean::Algebra::Slice') or $algebra->isa('Attean::Algebra::Project')) {
				# TODO: Handle HAVING
				# TODO: Error if Slice appears before distinct/reduced
				if ($algebra->isa('Attean::Algebra::Distinct')) {
					$modifiers{ distinct }	= 1;
				} elsif ($algebra->isa('Attean::Algebra::Reduced')) {
					$modifiers{ reduced }	= 1;
				} elsif ($algebra->isa('Attean::Algebra::Slice')) {
					if ($algebra->limit >= 0) {
						$modifiers{ limit }		= $algebra->limit;
					}
					if ($algebra->offset > 0) {
						$modifiers{ offset }	= $algebra->offset;
					}
				} elsif ($algebra->isa('Attean::Algebra::OrderBy')) {
					$modifiers{order}	= $algebra->comparators;
				} elsif ($algebra->isa('Attean::Algebra::Extend')) {
					my $v		= $algebra->variable;
					my $name	= $v->value;
					my $expr	= $algebra->expression;
					my @tokens;
					push(@tokens, $lparen);
					push(@tokens, $expr->sparql_tokens->elements);
					push(@tokens, $as);
					push(@tokens, $v->sparql_tokens->elements);
					push(@tokens, $rparen);
					$modifiers{project_expression_tokens}{$name}	= \@tokens;
				} elsif ($algebra->isa('Attean::Algebra::Project')) {
					my $vars	= $algebra->variables;
					my ($child)	= @{ $algebra->children };
					my @vars	= sort(map { $_->value } @$vars);
					my @subvars	= sort($child->in_scope_variables);
					if (scalar(@vars) == scalar(@subvars) and join('.', @vars) eq join('.', @subvars)) {
						# this is a SELECT * query
					} else {
						foreach my $v (@$vars) {
							my $name	= $v->value;
							unless ($modifiers{project_variables}{$name}++) {
								push(@{ $modifiers{project_variables_order} }, $name);
							}
						}
					}
				} elsif ($algebra->isa('Attean::Algebra::Group')) {
					my $aggs	= $algebra->aggregates;
					my $groups	= $algebra->groupby;
					foreach my $agg (@$aggs) {
						my $v		= $agg->variable;
						my $name	= $v->value;
						my @tokens;
						push(@tokens, $lparen);
						push(@tokens, $agg->sparql_tokens->elements);
						push(@tokens, $as);
						push(@tokens, $v->sparql_tokens->elements);
						push(@tokens, $rparen);
						unless ($modifiers{project_variables}{$name}++) {
							push(@{ $modifiers{project_variables_order} }, $name);
						}
						$modifiers{project_expression_tokens}{$name}	= \@tokens;
					}
					foreach my $group (@$groups) {
						push(@{ $modifiers{groups} }, $group->sparql_tokens->elements);
					}
				} else {
					die "Unexpected pattern type encountered in query_tokens: " . ref($algebra);
				}
				($algebra)	= @{ $algebra->children };
			}
		}
		
		my @tokens;

		my $where	= AtteanX::SPARQL::Token->keyword('WHERE');
		my $lbrace	= AtteanX::SPARQL::Token->lbrace;
		my $rbrace	= AtteanX::SPARQL::Token->rbrace;


		if ($form eq 'SELECT') {
			push(@tokens, AtteanX::SPARQL::Token->keyword('SELECT'));
			if ($modifiers{distinct}) {
				push(@tokens, AtteanX::SPARQL::Token->keyword('DISTINCT'));
			} elsif ($modifiers{reduced}) {
				push(@tokens, AtteanX::SPARQL::Token->keyword('REDUCED'));
			}
			
			if (my $p = $modifiers{project_variables_order}) {
				foreach my $name (@$p) {
					if (my $etokens = $modifiers{project_expression_tokens}{$name}) {
						push(@tokens, @$etokens);
					} else {
						my $v	= Attean::Variable->new( value => $name );
						push(@tokens, $v->sparql_tokens->elements);
					}
				}
			} else {
				push(@tokens, AtteanX::SPARQL::Token->star);
			}
			
			push(@tokens, $self->dataset_tokens($dataset)->elements);
			
			push(@tokens, $where);
			if ($algebra->isa('Attean::Algebra::Join')) {
				# don't emit extraneous braces at the top-level
				push(@tokens, $algebra->sparql_tokens->elements);
			} else {
				push(@tokens, $lbrace);
				push(@tokens, $algebra->sparql_tokens->elements);
				push(@tokens, $rbrace);
			}
			if (my $groups = $modifiers{groups}) {
				push(@tokens, AtteanX::SPARQL::Token->keyword('GROUP'));
				push(@tokens, AtteanX::SPARQL::Token->keyword('BY'));
				push(@tokens, @$groups);
			}
			if (my $expr = $modifiers{having}) {
				push(@tokens, AtteanX::SPARQL::Token->keyword('HAVING'));
				push(@tokens, $expr->sparql_tokens->elements);
			}
			if (my $comps = $modifiers{order}) {
				push(@tokens, AtteanX::SPARQL::Token->keyword('ORDER'));
				push(@tokens, AtteanX::SPARQL::Token->keyword('BY'));
				foreach my $c (@$comps) {
					push(@tokens, $c->sparql_tokens->elements);
				}
			}
			if (exists $modifiers{limit}) {
				push(@tokens, AtteanX::SPARQL::Token->keyword('LIMIT'));
				push(@tokens, AtteanX::SPARQL::Token->integer($modifiers{limit}));
			}
			if (exists $modifiers{offset}) {
				push(@tokens, AtteanX::SPARQL::Token->keyword('OFFSET'));
				push(@tokens, AtteanX::SPARQL::Token->integer($modifiers{offset}));
			}
		} elsif ($form eq 'DESCRIBE') {
			push(@tokens, AtteanX::SPARQL::Token->keyword('DESCRIBE'));
			foreach my $t (@{ $modifiers{describe} }) {
				push(@tokens, $t->sparql_tokens->elements);
			}
			push(@tokens, $self->dataset_tokens($dataset)->elements);
			push(@tokens, $where);
			push(@tokens, $lbrace);
			push(@tokens, $algebra->sparql_tokens->elements);
			push(@tokens, $rbrace);
		} elsif ($form eq 'CONSTRUCT') {
			push(@tokens, AtteanX::SPARQL::Token->keyword('CONSTRUCT'));
			push(@tokens, $lbrace);
			foreach my $t (@{ $modifiers{construct} }) {
				push(@tokens, $t->sparql_tokens->elements);
				push(@tokens, AtteanX::SPARQL::Token->dot);
			}
			push(@tokens, $rbrace);
			push(@tokens, $self->dataset_tokens($dataset)->elements);
			push(@tokens, $where);
			push(@tokens, $lbrace);
			push(@tokens, $algebra->sparql_tokens->elements);
			push(@tokens, $rbrace);
		} elsif ($form eq 'ASK') {
			push(@tokens, AtteanX::SPARQL::Token->keyword('ASK'));
			push(@tokens, $self->dataset_tokens($dataset)->elements);
			push(@tokens, $lbrace);
			push(@tokens, $algebra->sparql_tokens->elements);
			push(@tokens, $rbrace);
		} else {
			die "Unexpected query for '$form' in query_tokens";
		}
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
}

package Attean::API::SPARQLQuerySerializable 0.031 {
	use Moo::Role;
	use namespace::clean;
	with 'Attean::API::SPARQLSerializable';

	sub sparql_tokens {
		my $self	= shift;
		return $self->query_tokens;
	}
}

=item * L<Attean::API::Algebra>

=cut

package Attean::API::Algebra 0.031 {
	use Moo::Role;

	with 'Attean::API::SPARQLSerializable';
	
	requires 'as_sparql';
	requires 'in_scope_variables';			# variables that will be in-scope after this operation is evaluated
	
	sub unary {
		my $self	= shift;
		return unless (scalar(@{ $self->children }) == 1);
		return $self->children->[0];
	}
	
	sub algebra_as_string {
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
			$string	.= "-$indent " .  $a->algebra_as_string($level) . "\n";
		});
		return $string;
	}
	
	sub blank_nodes {
		my $self	= shift;
		my %blanks;
		$self->walk( prefix => sub {
			my $a	= shift;
			if ($a->isa('Attean::Algebra::BGP')) {
				my @triples	= @{ $a->triples };
				my @nodes	= grep { $_->does('Attean::API::Blank') } map { $_->values } @triples;
				foreach my $b (@nodes) {
					$blanks{ $b->value }	= $b;
				}
			} elsif ($a->isa('Attean::Algebra::Path')) {
				my @nodes	= grep { $_->does('Attean::API::Blank') } ($a->subject, $a->object);
				foreach my $b (@nodes) {
					$blanks{ $b->value }	= $b;
				}
			}
		});
		return values %blanks;
	}
	
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
					Carp::confess "${name} algebra construction with bad number of children (expected $arity, but got $size)";
				}
			}
		}
	}
}

=item * L<Attean::API::QueryTree>

=cut

package Attean::API::QueryTree 0.031 {
	use Moo::Role;
	with 'Attean::API::DirectedAcyclicGraph';
}

=item * L<Attean::API::NullaryQueryTree>

=cut

package Attean::API::NullaryQueryTree 0.031 {
	use Moo::Role;
	sub arity { return 0 }
	with 'Attean::API::QueryTree';
}

=item * L<Attean::API::UnaryQueryTree>

=cut

package Attean::API::UnaryQueryTree 0.031 {
	use Moo::Role;
	sub arity { return 1 }
	with 'Attean::API::QueryTree';
	
	sub child {
		my $self	= shift;
		return $self->children->[0];
	}
}

=item * L<Attean::API::BinaryQueryTree>

=cut

package Attean::API::BinaryQueryTree 0.031 {
	use Moo::Role;
	sub arity { return 2 }
	with 'Attean::API::QueryTree';
}

=item * L<Attean::API::PropertyPath>

=cut

package Attean::API::PropertyPath 0.031 {
	use Moo::Role;
	with 'Attean::API::QueryTree';
	requires 'as_string';
	requires 'as_sparql';
}

=item * L<Attean::API::UnaryPropertyPath>

=cut

package Attean::API::UnaryPropertyPath 0.031 {
	use Types::Standard qw(ConsumerOf);

	use Moo::Role;

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
	sub algebra_as_string {
		my $self	= shift;
		return "Property Path " . $self->prefix_name . $self->postfix_name;
	}
	with 'Attean::API::PropertyPath', 'Attean::API::UnaryQueryTree';
}

=item * L<Attean::API::NaryPropertyPath>

=cut

package Attean::API::NaryPropertyPath 0.031 {
	use Types::Standard qw(ArrayRef ConsumerOf);

	use Moo::Role;

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
	sub algebra_as_string {
		my $self	= shift;
		return "Property Path " . $self->separator;
	}
	with 'Attean::API::PropertyPath';
}

=item * L<Attean::API::UnionScopeVariables>

=cut

package Attean::API::UnionScopeVariables 0.031 {
	use Moo::Role;
	sub in_scope_variables {
		my $self	= shift;
		my $set		= Set::Scalar->new();
		foreach my $c (@{ $self->children }) {
			$set->insert( $c->in_scope_variables );
		}
		return $set->elements;
	}
}

=item * L<Attean::API::IntersectionScopeVariables>

=cut

package Attean::API::IntersectionScopeVariables 0.031 {
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
		return $set->elements;
	}
}

1;

=back

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
