use v5.14;
use warnings;

=head1 NAME

Attean::Plan - Representation of SPARQL query plan operators

=head1 VERSION

This document describes Attean::Plan version 0.008

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

package Attean::Plan::Quad 0.008 {
	use Moo;
	use Types::Standard qw(ConsumerOf ArrayRef);

	has 'subject'	=> (is => 'ro', required => 1);
	has 'predicate'	=> (is => 'ro', required => 1);
	has 'object'	=> (is => 'ro', required => 1);
	has 'graph'		=> (is => 'ro', required => 1);
	
	with 'Attean::API::Plan', 'Attean::API::NullaryQueryTree';
	with 'Attean::API::QuadPattern';

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

package Attean::Plan::NestedLoopJoin 0.008 {
	use Moo;
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
		my $left	= $self->left;
		my $anti	= $self->anti;
		my @children	= map { $_->impl($model) } @{ $self->children };
		return sub {
			my ($lhs, $rhs)	= map { $_->() } @children;
			my @right	= $rhs->elements;
			my @results;
			while (my $l = $lhs->next) {
				my $seen	= 0;
				foreach my $r (@right) {
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
				values => \@results
			);
		}
	}
}

=item * L<Attean::Plan::HashJoin>

Evaluates a join (natural-, anti-, or left-) using a hash join.

=cut

package Attean::Plan::HashJoin 0.008 {
	use Moo;
	with 'Attean::API::Plan::Join';
	sub plan_as_string {
		my $self	= shift;
		my $name;
		if ($self->left) {
			$name	= "Hash Left Join";
		} elsif ($self->anti) {
			$name	= "Hash Left Anti Join";
		} else {
			$name	= "Hash Join";
		}
		return sprintf('%s { %s }', $name, join(', ', @{$self->join_variables}));
	}

	sub impl {
		my $self	= shift;
		my $model	= shift;
		my @children	= map { $_->impl($model) } @{ $self->children };
		my $left	= $self->left;
		my $anti	= $self->anti;
		return sub {
			my %hash;
			my @vars	= @{ $self->join_variables };
			my $rhs		= $children[1]->();
			while (my $r = $rhs->next()) {
				my $key	= join(',', map { ref($_) ? $_->as_string : '' } map { $r->value($_) } @vars);
				push(@{ $hash{$key} }, $r);
			}
			
			my @results;
			my $lhs		= $children[0]->();
			while (my $l = $lhs->next()) {
				my $seen	= 0;
				my $key		= join(',', map { ref($_) ? $_->as_string : '' } map { $l->value($_) } @vars);
				if (my $rows = $hash{$key}) {
					foreach my $r (@$rows) {
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
				}
				if ($left and not($seen)) {
					push(@results, $l);
				} elsif ($anti and not($seen)) {
					push(@results, $l);
				}
			}
			return Attean::ListIterator->new(
				item_type => 'Attean::API::Result',
				values => \@results
			);
		}
	}
}

=item * L<Attean::Plan::EBVFilter>

Filters results from a sub-plan based on the effective boolean value of a
named variable binding.

=cut

package Attean::Plan::EBVFilter 0.008 {
	use Moo;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Str ConsumerOf);
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'variable' => (is => 'ro', isa => Str, required => 1);

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
	
	sub plan_as_string {
		my $self	= shift;
		return sprintf('EBVFilter { ?%s }', $self->variable);
	}
	sub tree_attributes { return qw(expression) };
	
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

package Attean::Plan::Merge 0.008 {
	use Moo;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Str ArrayRef ConsumerOf);
	with 'Attean::API::Plan', 'Attean::API::BinaryQueryTree';
	has 'variables' => (is => 'ro', isa => ArrayRef[Str], required => 1);
	sub plan_as_string { return 'Merge' }
	
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
		return sub {
			die "Unimplemented";
		};
	}
}

=item * L<Attean::Plan::Union>

Evaluates a set of sub-plans, returning the union of results.

=cut

package Attean::Plan::Union 0.008 {
	use Moo;
	use Scalar::Util qw(blessed);
	with 'Attean::API::Plan', 'Attean::API::BinaryQueryTree';
	sub plan_as_string { return 'Union' }
	
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
		return sub {
			my $current	= shift(@children);
			my $iter	= $current->();
			return Attean::CodeIterator->new(
				item_type => 'Attean::API::Result',
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
		};
	}
}

=item * L<Attean::Plan::Extend>

Evaluates a sub-plan, and extends each result by evaluating a set of
expressions, binding the produced values to new variables.

=cut

package Attean::Plan::Extend 0.008 {
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
	use Types::Standard qw(ConsumerOf InstanceOf HashRef);
	use namespace::clean;

	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
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
		my $op		= $expr->operator;

		state $true			= Attean::Literal->true;
		state $false		= Attean::Literal->false;
		state $type_roles	= { qw(URI IRI IRI IRI BLANK Blank LITERAL Literal NUMERIC NumericLiteral) };
		state $type_classes	= { qw(URI Attean::IRI IRI Attean::IRI STR Attean::Literal) };
		
		if ($expr->isa('Attean::ValueExpression')) {
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
			warn "Binary operator: " . $expr->operator;
			use Data::Dumper;
			warn Dumper($expr);
			die "Expression evaluation unimplemented: " . $expr->as_string;
		} elsif ($expr->isa('Attean::FunctionExpression')) {
			my $func	= $expr->operator;
			if ($func eq 'IF') {
				my ($check, @children)	= @{ $expr->children };
				my ($term)		= eval { $self->evaluate_expression($model, $check, $r) };
# 				warn $@ if $@;
				if (blessed($term) and $term->ebv) {
					return $self->evaluate_expression($model, $children[0], $r);
				} else {
					return $self->evaluate_expression($model, $children[1], $r);
				}
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
					die if ($s->datatype and not($s->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string'));
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
# 				my ($term, $lang)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				return Attean::Literal->new(value => $term->value, langauge => $lang->value);
			} elsif ($func eq 'STRDT') {
				my ($term, $dt)	= @terms;
				die unless ($term->does('Attean::API::Literal'));
				die unless ($term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string');
				die if ($term->language);
# 				my ($term, $dt)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				return Attean::Literal->new(value => $term->value, datatype => $dt->value);
			} elsif ($func eq 'REPLACE') {
				my ($term, $pat, $rep)	= @terms;
				die unless ($term->does('Attean::API::Literal'));
				die unless ($term->language or $term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string');
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
# 				my ($term, $pat)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				die unless ($term->does('Attean::API::Literal'));
				die unless ($term->language or $term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string');
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
# 				my ($term, $pat)	= map { $self->evaluate_expression($model, $_, $r) } @{ $expr->children };
				die unless ($term->does('Attean::API::Literal'));
				die unless ($term->language or $term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string');
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
			} else {
				die "Expression evaluation unimplemented: " . $expr->as_string;
			}
		} else {
			die "Expression evaluation unimplemented: " . $expr->as_string;
		}
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my %exprs	= %{ $self->expressions };
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		return sub {
			my $iter	= $impl->();
			return Attean::CodeIterator->new(
				item_type => 'Attean::API::Result',
				generator => sub {
					ROW: while (my $r = $iter->next) {
# 						warn 'ROW-------------------------------';
						my %row	= map { $_ => $r->value($_) } $r->variables;
						foreach my $var (keys %exprs) {
							my $expr	= $exprs{$var};
# 							warn "$var => "  . $expr->as_string;
							my $term	= eval { $self->evaluate_expression($model, $expr, $r) };
							warn $@ if ($@);
							if (blessed($term)) {
# 								warn "---> " . Dumper($term);
								if ($row{ $var } and $term->as_string ne $row{ $var }->as_string) {
									next ROW;
								}
					
								$row{ $var }	= $term;
							}
						}
						return Attean::Result->new( bindings => \%row );
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

package Attean::Plan::HashDistinct 0.008 {
	use Moo;
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	sub plan_as_string { return 'HashDistinct' }
	
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

package Attean::Plan::Unique 0.008 {
	use Moo;
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	sub plan_as_string { return 'Unique' }
	
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

package Attean::Plan::Slice 0.008 {
	use Moo;
	use Types::Standard qw(Int);
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'limit' => (is => 'ro', isa => Int, default => -1);
	has 'offset' => (is => 'ro', isa => Int, default => 0);

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

package Attean::Plan::Project 0.008 {
	use Moo;
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
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
	
	sub plan_as_string {
		my $self	= shift;
		return sprintf('Project { %s }', join(' ', map { '?' . $_->value } @{ $self->variables }));
	}
	sub tree_attributes { return qw(variables) };
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		my @vars	= map { $_->value } @{ $self->variables };
		return sub {
			my $iter	= $impl->();
			return $iter->map(sub {
				my $r	= shift;
				my $b	= { map { my $t	= $r->value($_); $t	? ($_ => $t) : () } @vars };
				return Attean::Result->new( bindings => $b );
			});
		};
	}
}

=item * L<Attean::Plan::OrderBy>

Evaluates a sub-plan and returns the results after fully materializing and
sorting is applied.

=cut

package Attean::Plan::OrderBy 0.008 {
	use Moo;
	use Types::Standard qw(HashRef ArrayRef InstanceOf Bool Str);
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'variables' => (is => 'ro', isa => ArrayRef[Str], required => 1);
	has 'ascending' => (is => 'ro', isa => HashRef[Bool], required => 1);
	sub plan_as_string {
		my $self	= shift;
		my @vars	= @{ $self->variables };
		my $ascending	= $self->ascending;
		my @strings	= map { sprintf('%s(?%s)', ($ascending->{$_} ? 'ASC' : 'DESC'), $_) } @vars;
		return sprintf('Order { %s }', join(', ', @strings));
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
		my $vars	= $self->variables;
		my $ascending	= $self->ascending;
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		return sub {
			my $iter	= $impl->();
			my @rows	= $iter->elements;
			my @sorted	= map { $_->[0] } sort {
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
			} @rows;
			return Attean::ListIterator->new( values => \@sorted, item_type => $iter->item_type);
		}
	}
}

package Attean::Plan::HeapSort 0.008 {
	use Moo;
	use Array::Heap;
	use Types::Standard qw(Int HashRef ArrayRef InstanceOf Bool Str);
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'limit' 	=> (is => 'ro', isa => Int, default => -1);
	has 'variables' => (is => 'ro', isa => ArrayRef[Str], required => 1);
	has 'ascending' => (is => 'ro', isa => HashRef[Bool], required => 1);

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
	
	sub plan_as_string {
		my $self		= shift;
		my @vars		= @{ $self->variables };
		my $ascending	= $self->ascending;
		my @cmpstrs	= map { sprintf('%s(?%s)', ($ascending->{$_} ? 'ASC' : 'DESC'), $_) } @vars;

		my @str;
		push(@str, "Limit=" . $self->limit);
		push(@str, "Order=" . join(', ', @cmpstrs));
		return sprintf('HeapSort { %s }', join(' ', @str));
	}
	
	sub impl {
		my $self	= shift;
		my $model	= shift;
		my $vars	= $self->variables;
		my $ascending	= $self->ascending;
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		my $limit	= $self->limit;
		my $cmp	= sub {
			my $a	= shift;
			my $b	= shift;
			my ($ar, $avalues)	= ($a, [map { $a->value($_) } @$vars]);
			my ($br, $bvalues)	= ($b, [map { $b->value($_) } @$vars]);
			my $c	= 0;
			foreach my $i (0 .. $#{ $vars }) {
				my $ascending	= $ascending->{ $vars->[$i] };
				my ($av, $bv)	= map { $_->[$i] } ($avalues, $bvalues);
				$c		= $av ? $av->compare($bv) : 1;
				$c		*= -1 unless ($ascending);
				last unless ($c == 0);
			}
			return -$c;	# negate the sort order so that we maintain the results we do want to keep in the heap
		};
		return sub {
			my $iter	= $impl->();
			my @heap;
			while (my $r = $iter->next) {
				push_heap_cmp(sub { $cmp->($a, $b) }, @heap, $r);
				if (scalar(@heap) > $limit) {
					pop_heap_cmp(sub { $cmp->($a, $b) }, @heap);
				}
			}
			my @rows;
			while (my $r = pop_heap_cmp(sub { $cmp->($a, $b) }, @heap)) {
				unshift(@rows, $r);
			}
			return Attean::ListIterator->new( values => \@rows, item_type => $iter->item_type);
		}
	}
}

=item * L<Attean::Plan::Service>

Evaluates a SPARQL query against a remove endpoint.

=cut

package Attean::Plan::Service 0.008 {
	use Moo;
	use Types::Standard qw(ConsumerOf Bool Str);
	sub plan_as_string {
		my $self	= shift;
		my $sparql	= $self->sparql;
		$sparql		=~ s/\s+/ /g;
		return sprintf('Service <%s> %s', $self->endpoint->as_string, $sparql);
	}
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	has 'endpoint' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
	has 'silent' => (is => 'ro', isa => Bool, default => 0);
	has 'sparql' => (is => 'ro', isa => Str, required => 1);
	
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

package Attean::Plan::Table 0.008 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	use namespace::clean;
	has variables => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Variable']]);
	has rows => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Result']]);
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	sub tree_attributes { return qw(variables rows) };
	sub plan_as_string { return 'Table' }
	
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
		return sub {
			return Attean::ListIterator->new(
				item_type => 'Attean::API::Result',
				values => $rows
			);
		};
	}
}

=item * L<Attean::Plan::Exists>

Returns an iterator containing a single boolean term indicating whether any
results were produced by evaluating the sub-plan.

=cut

package Attean::Plan::Exists 0.008 {
	use Moo;
	use Types::Standard qw(ArrayRef ConsumerOf);
	use namespace::clean;
	has variables => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Variable']]);
	has rows => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::Result']]);
	with 'Attean::API::Plan', 'Attean::API::UnaryQueryTree';
	sub tree_attributes { return qw(variables rows) };
	sub plan_as_string { return 'Exists' }
	
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
		my ($impl)	= map { $_->impl($model) } @{ $self->children };
		return sub {
			my $iter	= $impl->();
			my $term	= ($iter->next) ? Attean::Literal->true : Attean::Literal->false;
			return Attean::ListIterator->new(values => [$term], item_type => 'Attean::API::Term');
		}
	}
}

# =item * L<Attean::Algebra::Ask>
# 
# =cut
# 
# package Attean::Algebra::Ask 0.008 {
# 	use Moo;
# 	sub in_scope_variables { return; }
# 	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
# 	sub algebra_as_string { return 'Ask' }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my %args	= @_;
# 		my $level	= $args{level} // 0;
# 		my $sp		= $args{indent} // '    ';
# 		my $indent	= $sp x $level;
# 		
# 		return "${indent}ASK {\n"
# 			. join('', map { $_->as_sparql( %args, level => $level+1 ) } @{ $self->children })
# 			. "${indent}}\n";
# 	}
# }
# 
# =item * L<Attean::Algebra::Construct>
# 
# =cut
# 
# package Attean::Algebra::Construct 0.008 {
# 	use Moo;
# 	use Types::Standard qw(ArrayRef ConsumerOf);
# 	has 'triples' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::TriplePattern']]);
# 	sub in_scope_variables { return qw(subject predicate object); }
# 	with 'Attean::API::Algebra', 'Attean::API::UnaryQueryTree';
# 	sub tree_attributes { return qw(triples) };
# 	sub algebra_as_string { return 'Construct' }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my %args	= @_;
# 		my $level	= $args{level} // 0;
# 		my $sp		= $args{indent} // '    ';
# 		my $indent	= $sp x $level;
# 		
# 		return "${indent}CONSTRUCT {\n"
# 			. join('', map { $_->as_sparql( %args, level => $level+1 ) } @{ $self->triples })
# 			. "${indent}} WHERE {\n"
# 			. join('', map { $_->as_sparql( %args, level => $level+1 ) } @{ $self->children })
# 			. "${indent}}\n";
# 	}
# }
# 
# =item * L<Attean::Algebra::Path>
# 
# =cut
# 
# package Attean::Algebra::Path 0.008 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	sub in_scope_variables {
# 		my $self	= shift;
# 		my @vars	= map { $_->value } grep { $_->does('Attean::API::Variable') } ($self->subject, $self->object);
# 		return Set::Scalar->new(@vars)->members;
# 	}
# 	with 'Attean::API::Algebra', 'Attean::API::NullaryQueryTree';
# 	has 'subject' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
# 	has 'path' => (is => 'ro', isa => ConsumerOf['Attean::API::PropertyPath'], required => 1);
# 	has 'object' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable'], required => 1);
# 	sub tree_attributes { return qw(subject path object) };
# 	sub as_sparql {
# 		my $self	= shift;
# 		my %args	= @_;
# 		my $level	= $args{level} // 0;
# 		my $sp		= $args{indent} // '    ';
# 		my $indent	= $sp x $level;
# 		
# 		return "${indent}"
# 			. $self->subject->as_sparql
# 			. ' '
# 			. $self->path->as_sparql
# 			. ' '
# 			. $self->object->as_sparql
# 			. "\n";
# 	}
# }
# 
# =item * L<Attean::Algebra::Group>
# 
# =cut
# 
# =item * L<Attean::Algebra::NegatedPropertySet>
# 
# =cut
# 
# package Attean::Algebra::NegatedPropertySet 0.008 {
# 	use Moo;
# 	use Types::Standard qw(ArrayRef ConsumerOf);
# 	with 'Attean::API::PropertyPath';
# 	has 'predicates' => (is => 'ro', isa => ArrayRef[ConsumerOf['Attean::API::IRI']], required => 1);
# 	sub as_string {
# 		my $self	= shift;
# 		return sprintf("!(%s)", join('|', map { $_->ntriples_string } @{ $self->predicates }));
# 	}
# 	sub algebra_as_string { return 'NPS' }
# 	sub tree_attributes { return qw(predicates) };
# 	sub as_sparql {
# 		my $self	= shift;
# 		return "!(" . join('|', map { $_->as_sparql } @{$self->predicates}) . ")";
# 	}
# }
# 
# =item * L<Attean::Algebra::PredicatePath>
# 
# =cut
# 
# package Attean::Algebra::PredicatePath 0.008 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	with 'Attean::API::PropertyPath';
# 	has 'predicate' => (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], required => 1);
# 	sub as_string {
# 		my $self	= shift;
# 		return $self->predicate->ntriples_string;
# 	}
# 	sub algebra_as_string {
# 		my $self	= shift;
# 		return 'Property Path ' . $self->as_string;
# 	}
# 	sub tree_attributes { return qw(predicate) };
# 	sub as_sparql {
# 		my $self	= shift;
# 		return $self->predicate->as_sparql;
# 	}
# }
# 
# =item * L<Attean::Algebra::InversePath>
# 
# =cut
# 
# package Attean::Algebra::InversePath 0.008 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	with 'Attean::API::UnaryPropertyPath';
# 	sub prefix_name { return "^" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my ($path)	= @{ $self->children };
# 		return '^' . $self->path->as_sparql;
# 	}
# }
# 
# =item * L<Attean::Algebra::SequencePath>
# 
# =cut
# 
# package Attean::Algebra::SequencePath 0.008 {
# 	use Moo;
# 	with 'Attean::API::NaryPropertyPath';
# 	sub separator { return "/" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my @paths	= @{ $self->children };
# 		return '(' . join('/', map { $_->as_sparql } @paths) . ')';
# 	}
# }
# 
# =item * L<Attean::Algebra::AlternativePath>
# 
# =cut
# 
# package Attean::Algebra::AlternativePath 0.008 {
# 	use Moo;
# 	with 'Attean::API::NaryPropertyPath';
# 	sub separator { return "|" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my @paths	= @{ $self->children };
# 		return '(' . join('|', map { $_->as_sparql } @paths) . ')';
# 	}
# }
# 
# =item * L<Attean::Algebra::ZeroOrMorePath>
# 
# =cut
# 
# package Attean::Algebra::ZeroOrMorePath 0.008 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	with 'Attean::API::UnaryPropertyPath';
# 	sub postfix_name { return "*" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my ($path)	= @{ $self->children };
# 		return $self->path->as_sparql . '*';
# 	}
# }
# 
# =item * L<Attean::Algebra::OneOrMorePath>
# 
# =cut
# 
# package Attean::Algebra::OneOrMorePath 0.008 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	with 'Attean::API::UnaryPropertyPath';
# 	sub postfix_name { return "+" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my ($path)	= @{ $self->children };
# 		return $self->path->as_sparql . '+';
# 	}
# }
# 
# =item * L<Attean::Algebra::ZeroOrOnePath>
# 
# =cut
# 
# package Attean::Algebra::ZeroOrOnePath 0.008 {
# 	use Moo;
# 	use Types::Standard qw(ConsumerOf);
# 	with 'Attean::API::UnaryPropertyPath';
# 	sub postfix_name { return "?" }
# 	sub as_sparql {
# 		my $self	= shift;
# 		my ($path)	= @{ $self->children };
# 		return $self->path->as_sparql . '?';
# 	}
# }
# 


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
