use v5.14;
use warnings;

=head1 NAME

Attean::Expression - SPARQL Expressions

=head1 VERSION

This document describes Attean::Expression version 0.001

=head1 SYNOPSIS

  use v5.14;
  use Attean;

  my $binding = Attean::Result->new();
  my $value = Attean::ValueExpression->new( value => Attean::Literal->integer(2) );
  my $plus = Attean::BinaryExpression->new( children => [$value, $value], operator => '+' );
  my $result = $plus->evaluate($binding);
  say $result->numeric_value; # 4

=head1 DESCRIPTION

This is a utility package that defines all the Attean SPARQL expression classes
consisting of logical, numeric, and function operators, constant terms, and
variables. Expressions may be evaluated in the context of a
L<Attean::API::Result> object, and either return a L<Attean::API::Term> object
or throw a type error exception.

The expression classes are:

=over 4

=cut

use Attean::API::Expression;

=item * L<Attean::ValueExpression>

=cut

package Attean::ValueExpression 0.001 {
	use Moo;
	use utf8::all;
	use Types::Standard qw(ConsumerOf);
	use namespace::clean;

	with 'Attean::API::Expression';
	sub arity { return 0 }
	sub BUILDARGS {
		my $class	= shift;
		return $class->SUPER::BUILDARGS(@_, operator => '_value');
	}
	has 'value' => (is => 'ro', isa => ConsumerOf['Attean::API::TermOrVariable']);
	sub as_string {
		my $self	= shift;
		my $str		= $self->value->ntriples_string;
		if ($str =~ m[^"(true|false)"\^\^<http://www[.]w3[.]org/2001/XMLSchema#boolean>$]) {
			return $1;
		} elsif ($str =~ m[^"(\d+)"\^\^<http://www[.]w3[.]org/2001/XMLSchema#integer>$]) {
			return $1
		}
		return $str;
	}

# =item C<< impl >>
# 
# Returns a CODE reference that when called with a L<Attean::API::Result>
# argument, will evaluate the expression and return the resulting
# L<Attean::API::Term> object (or throw a type error exception).
# 
# =cut

	sub impl {
		my $self	= shift;
		my $node	= $self->value;
		if ($node->does('Attean::API::Variable')) {
			return sub {
				my ($r, @args)	= @_;
# 				warn "Evaluating ValueExpression for variable '" . $node->value . "' on binding " . $binding->as_string;
				my $term	= $r->value($node->value);
# 				warn "===> " . $term->as_string;
				return $term;
			};
		} else {
			return sub {
				return $node;
			};
		}
	}
}

=item * L<Attean::UnaryExpression>

=cut

package Attean::UnaryExpression 0.001 {
	use Moo;
	use Types::Standard qw(Enum);
	use namespace::clean;

	my %map	= ('NOT' => '!');
	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		my $args	= $class->$orig(@_);
		my $op		= $args->{operator};
		if (exists $map{uc($op)}) {
			$args->{operator}	= $map{uc($op)};
		}
		return $args;
	};
	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(+ - !)];
		$type->assert_valid($self->operator);
	}
	
	sub impl {
		my $self	= shift;
		my $op		= $self->operator;
		my ($child)	= @{ $self->children };
		my $impl	= $child->impl;
		if ($op eq '!') {
			return sub {
				my ($r, @args)	= @_;
				my $term	= $impl->($r, @args);
				return ($term->ebv) ? Attean::Literal->false : Attean::Literal->true;
			}
		}
		# TODO: implement UnaryExpression evaluation
		die "Unimplemented UnaryExpression evaluation: " . $self->operator;
	}
	
	with 'Attean::API::UnaryExpression', 'Attean::API::Expression', 'Attean::API::UnaryQueryTree';
}

=item * L<Attean::BinaryExpression>

=cut

package Attean::BinaryExpression 0.001 {
	use Moo;
	use Try::Tiny;
	use Types::Standard qw(Enum);
	use Scalar::Util qw(blessed);
	use namespace::clean;

	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(+ - * / < <= > >= != = && ||)];
		$type->assert_valid($self->operator);
	}
	
	sub impl {
		my $self		= shift;
		my ($lhs, $rhs)	= @{ $self->children };
		my $op			= $self->operator;
		my ($lhsi, $rhsi)	= map { $_->impl } ($lhs, $rhs);
		my $true	= Attean::Literal->true;
		my $false	= Attean::Literal->false;
		if ($op eq '&&') {
			return sub {
				my ($r, @args)	= @_;
				my $lbv	= eval { $lhsi->($r, @args) };
				my $rbv	= eval { $rhsi->($r, @args) };
				die "TypeError $op" unless ($lbv or $rbv);
				return $false if (not($lbv) and not($rbv->ebv));
				return $false if (not($rbv) and not($lbv->ebv));
				die "TypeError $op" unless ($lbv and $rbv);
				return ($lbv->ebv && $rbv->ebv) ? $true : $false;
			}
		} elsif ($op eq '||') {
			return sub {
				my ($r, @args)	= @_;
				my $lbv	= eval { $lhsi->($r, @args) };
				return $true if ($lbv and $lbv->ebv);
				my $rbv	= eval { $rhsi->($r, @args) };
				die "TypeError $op" unless ($rbv);
				return $true if ($rbv->ebv);
		
				return $false if ($lbv);
				die "TypeError $op";
			}
		} elsif ($op =~ m#^(?:[-+*/])$#) { # numeric operators: - + * /
			return sub {
				my ($r, @args)	= @_;
				($lhs, $rhs)	= map { $_->($r, @args) } ($lhsi, $rhsi);
				for ($lhs, $rhs) { die "TypeError $op" unless (blessed($_) and $_->does('Attean::API::NumericLiteral')); }
				my $lv	= $lhs->numeric_value;
				my $rv	= $rhs->numeric_value;
				return Attean::Literal->new( value => eval "$lv $op $rv", datatype => $lhs->binary_promotion_type($rhs, $op) );
			};
		} elsif ($op =~ /^!?=$/) {
			return sub {
				my ($r, @args)	= @_;
				($lhs, $rhs)	= map { $_->($r, @args) } ($lhsi, $rhsi);
				for ($lhs, $rhs) { die "TypeError $op" unless (blessed($_) and $_->does('Attean::API::Term')); }
				my $ok	= ($lhs->equals($rhs));
				$ok		= not($ok) if ($op eq '!=');
				return $ok ? Attean::Literal->true : Attean::Literal->false;
			}
		} elsif ($op =~ /^[<>]=?$/) {
			return sub {
				my ($r, @args)	= @_;
				($lhs, $rhs)	= map { $_->($r, @args) } ($lhsi, $rhsi);
				for ($lhs, $rhs) { die "TypeError $op" unless $_->does('Attean::API::Term'); }
				my $c	= ($lhs->compare($rhs));
				return Attean::Literal->true if (($c < 0 and ($op =~ /<=?/)) or ($c > 0 and ($op =~ />=?/)) or ($c == 0 and ($op =~ /=/)));
				return Attean::Literal->false;
			}
		}
		die "Unexpected operator evaluation: $op";
	}
	
	with 'Attean::API::BinaryExpression';
}

=item * L<Attean::FunctionExpression>

=cut

package Attean::FunctionExpression 0.001 {
	use Moo;
	use Types::Standard qw(Enum ConsumerOf HashRef);
	use Types::Common::String qw(UpperCaseStr);
	use URI::Escape qw(uri_escape_utf8);
	use Encode qw(encode);
	use POSIX qw(ceil floor);
	use Digest;
	use Data::UUID;
	use Scalar::Util qw(blessed);
	use List::MoreUtils qw(zip);
	use DateTime::Format::W3CDTF;
	use I18N::LangTags;
	use namespace::clean;

	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		my $args	= $class->$orig(@_);
		$args->{operator}	= UpperCaseStr->coercion->($args->{operator});
		return $args;
	};
	sub BUILD {
		my $self	= shift;
		state $type	= Enum[qw(IN NOTIN STR LANG LANGMATCHES DATATYPE BOUND IRI URI BNODE RAND ABS CEIL FLOOR ROUND CONCAT SUBSTR STRLEN REPLACE UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS TIMEZONE TZ NOW UUID STRUUID MD5 SHA1 SHA256 SHA384 SHA512 COALESCE IF STRLANG STRDT SAMETERM ISIRI ISURI ISBLANK ISLITERAL ISNUMERIC REGEX)];
		$type->assert_valid($self->operator);
	}
	has 'operator'		=> (is => 'ro', isa => UpperCaseStr, coerce => UpperCaseStr->coercion, required => 1);
	has 'base'			=> (is => 'rw', isa => ConsumerOf['Attean::IRI'], predicate => 'has_base');
	with 'Attean::API::NaryExpression';
	
	sub impl {
		my $self	= shift;
		my $func	= $self->operator;
		my @children	= map { $_->impl } @{ $self->children };
		my %type_roles		= qw(URI IRI IRI IRI BLANK Blank LITERAL Literal NUMERIC NumericLiteral);
		my %type_classes	= qw(URI Attean::IRI IRI Attean::IRI STR Attean::Literal);
		return sub {
			my ($r, @args)	= @_;
			my $row_cache	= shift(@args) || {};
			my $true		= Attean::Literal->true;
			my $false		= Attean::Literal->false;
			
			if ($func eq 'IF') {
				my $term	= $children[0]->( $r, @args );
				if ($term->ebv) {
					return $children[1]->( $r, @args );
				} else {
					return $children[2]->( $r, @args );
				}
			} elsif ($func eq 'IN' or $func eq 'NOTIN') {
				($true, $false)	= ($false, $true) if ($func eq 'NOTIN');
				my $child	= shift(@children);
				my $term	= $child->( $r, @args );
				foreach my $c (@children) {
					if (my $value = eval { $c->( $r, @args ) }) {
						return $true if ($term->equals($value));
					}
				}
				return $false;
			} elsif ($func eq 'COALESCE') {
				foreach my $c (@children) {
					my $t	= eval { $c->( $r, @args ) };
					return $t if $t;
				}
				return;
			}
			
			my @operands	= map { $_->( $r, @args ) } @children;
			if ($func =~ /^(STR)$/) {
				return $type_classes{$1}->new($operands[0]->value);
			} elsif ($func =~ /^([UI]RI)$/) {
				my @base	= $self->has_base ? (base => $self->base) : ();
				return $type_classes{$1}->new(value => $operands[0]->value, @base);
			} elsif ($func eq 'BNODE') {
				if (scalar(@operands)) {
					my $name	= $operands[0]->value;
					if (my $b = $row_cache->{bnodes}{$name}) {
						return $b;
					} else {
						my $b	= Attean::Blank->new();
						$row_cache->{bnodes}{$name}	= $b;
						return $b;
					}
				} else {
					return Attean::Blank->new();
				}
			} elsif ($func eq 'LANG') {
				die "TypeError: LANG" unless ($operands[0]->does('Attean::API::Literal'));
				return Attean::Literal->new($operands[0]->language // '');
			} elsif ($func eq 'LANGMATCHES') {
				my ($lang, $match)	= map { $_->value } @operands;
				if ($match eq '*') {
					# """A language-range of "*" matches any non-empty language-tag string."""
					return ($lang ? $true : $false);
				} else {
					my $ok	= (I18N::LangTags::is_dialect_of( $lang, $match ));
					return $ok ? $true : $false;
				}
				
			} elsif ($func eq 'DATATYPE') {
				return $operands[0]->datatype;
			} elsif ($func eq 'BOUND') {
				return $operands[0] ? Attean::Literal->true : Attean::Literal->false;
			} elsif ($func eq 'RAND') {
				return Attean::Literal->new( value => rand(), datatype => 'http://www.w3.org/2001/XMLSchema#double' );
			} elsif ($func eq 'ABS') {
				return Attean::Literal->new( value => abs($operands[0]->value), $operands[0]->construct_args );
			} elsif ($func =~ /^(?:CEIL|FLOOR)$/) {
				my $v	= $operands[0]->value;
				return Attean::Literal->new( value => (($func eq 'CEIL') ? ceil($v) : floor($v)), $operands[0]->construct_args );
			} elsif ($func eq 'ROUND') {
				return Attean::Literal->new( value => sprintf('%.0f', (0.000000000000001 + $operands[0]->numeric_value)), $operands[0]->construct_args );
			} elsif ($func eq 'CONCAT') {
				my $all_lang	= 1;
				my $all_str		= 1;
				my $lang;
				foreach my $n (@operands) {
					die "CONCAT called with a non-literal argument" unless ($n->does('Attean::API::Literal'));
					if ($n->datatype->value ne 'http://www.w3.org/2001/XMLSchema#string') {
						die "CONCAT called with a datatyped-literal other than xsd:string";
					} elsif ($n->language) {
						$all_str	= 0;
						if (defined($lang) and $lang ne $n->language) {
							$all_lang	= 0;
						} else {
							$lang	= $n->language;
						}
					} else {
						$all_lang	= 0;
						$all_str	= 0;
					}
				}
				my %strtype;
				if ($all_lang and $lang) {
					warn "=========> $lang";
					$strtype{language}	= $lang;
				} elsif ($all_str) {
					$strtype{datatype}	= 'http://www.w3.org/2001/XMLSchema#string'
				}
				return Attean::Literal->new( value => join('', map { $_->value } @operands), %strtype );
			} elsif ($func eq 'SUBSTR') {
				my $str	= shift(@operands);
				my @args	= map { $_->numeric_value } @operands;
				my $v	= scalar(@args == 1) ? substr($str->value, $args[0]) : substr($str->value, $args[0], $args[1]);
				return Attean::Literal->new( value => $v, $str->construct_args );
			} elsif ($func eq 'STRLEN') {
				return Attean::Literal->integer(length($operands[0]->value));
			} elsif ($func eq 'REPLACE') {
				my ($node, $pat, $rep)	= @operands;
				die "TypeError: REPLACE called without a literal arg1 term" unless (blessed($node) and $node->does('Attean::API::Literal'));
				die "TypeError: REPLACE called without a literal arg2 term" unless (blessed($pat) and $pat->does('Attean::API::Literal'));
				die "TypeError: REPLACE called without a literal arg3 term" unless (blessed($rep) and $rep->does('Attean::API::Literal'));
				die "TypeError: REPLACE called with a datatyped (non-xsd:string) literal" if ($node->datatype and $node->datatype->value ne 'http://www.w3.org/2001/XMLSchema#string');
				my ($value, $pattern, $replace)	= map { $_->value } @operands;
				die "EvaluationError: REPLACE called with unsafe ?{} match pattern" if (index($pattern, '(?{') != -1 or index($pattern, '(??{') != -1);
				die "EvaluationError: REPLACE called with unsafe ?{} replace pattern" if (index($replace, '(?{') != -1 or index($replace, '(??{') != -1);
	
				$replace	=~ s/\\/\\\\/g;
				$replace	=~ s/\$(\d+)/\$$1/g;
				$replace	=~ s/"/\\"/g;
				$replace	= qq["$replace"];
				no warnings 'uninitialized';
				$value	=~ s/$pattern/"$replace"/eeg;
				return Attean::Literal->new(value => $value, $node->construct_args);
			} elsif ($func =~ /^[UL]CASE$/) {
				return Attean::Literal->new( value => ($func eq 'UCASE' ? uc($operands[0]->value) : lc($operands[0]->value) ), $operands[0]->construct_args );
			} elsif ($func eq 'ENCODE_FOR_URI') {
				return Attean::Literal->new( uri_escape_utf8($operands[0]->value) );
			} elsif ($func eq 'CONTAINS') {
				my ($node, $pat)	= @operands;
				my ($lit, $plit)	= map { $_->value } @operands;
				# TODO: what should be returned if one or both arguments are typed as xsd:string?
				die "TypeError: CONTAINS" if ($node->language and $pat->language and $node->language ne $pat->language);
				my $pos		= index($lit, $plit);
				return ($pos >= 0) ? Attean::Literal->true : Attean::Literal->false;
			} elsif ($func eq 'STRSTARTS' or $func eq 'STRENDS') {
				my ($lit, $plit)	= map { $_->value } @operands;
				if ($func eq 'STRENDS') {
					my $pos		= length($lit) - length($plit);
					return (rindex($lit, $plit) == $pos) ? Attean::Literal->true : Attean::Literal->false;
				} else {
					return (index($lit, $plit) == 0) ? Attean::Literal->true : Attean::Literal->false;
				}
			} elsif ($func eq 'STRBEFORE' or $func eq 'STRAFTER') {
				my ($node, $substr)	= @operands;

				die "$func called without a literal arg1 term" unless (blessed($node) and $node->does('Attean::API::Literal'));
				die "$func called without a literal arg2 term" unless (blessed($substr) and $substr->does('Attean::API::Literal'));
				die "$func called with a datatyped (non-xsd:string) literal" if ($node->datatype and $node->datatype->value ne 'http://www.w3.org/2001/XMLSchema#string');

				my $lhs_simple	= (not($node->language) and ($node->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string'));
				my $rhs_simple	= (not($substr->language) and ($substr->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string'));
				if ($lhs_simple and $rhs_simple) {
					# ok
				} elsif ($node->language and $substr->language and $node->language eq $substr->language) {
					# ok
				} elsif ($node->language and $rhs_simple) {
					# ok
				} else {
					die "$func called with literals that are not argument compatible";
				}
	
				my $value	= $node->value;
				my $match	= $substr->value;
				my $i		= index($value, $match, 0);
				if ($i < 0) {
					return Attean::Literal->new('');
				} else {
					if ($func eq 'STRBEFORE') {
						return Attean::Literal->new(value => substr($value, 0, $i), $node->construct_args);
					} else {
						return Attean::Literal->new(value => substr($value, $i+length($match)), $node->construct_args);
					}
				}
			} elsif ($func =~ /^(?:YEAR|MONTH|DAY|HOURS|MINUTES)$/) {
				my $method	= lc($func =~ s/^(HOUR|MINUTE)S$/$1/r);
				my $dt	= $operands[0]->datetime;
				return Attean::Literal->integer($dt->$method());
			} elsif ($func eq 'SECONDS') {
				my $dt	= $operands[0]->datetime;
				return Attean::Literal->decimal($dt->second());
			} elsif ($func eq 'TZ' or $func eq 'TIMEZONE') {
				my $dt	= $operands[0]->datetime;
				my $tz		= $dt->time_zone;
				if ($tz->is_floating) {
					if ($func eq 'TZ') {
						return Attean::Literal->new('');
					}
					die "TIMEZONE called with a dateTime without a timezone";
				}
				if ($func eq 'TZ' and $tz->is_utc) {
					return Attean::Literal->new('Z');
				}
				if ($tz) {
					my $offset	= $tz->offset_for_datetime( $dt );
					my $hours	= 0;
					my $minutes	= 0;
					my $minus	= ($func eq 'TZ') ? '+' : '';
					if ($offset < 0) {
						$minus	= '-';
						$offset	= -$offset;
					}

					my $duration	= "${minus}PT";
					if ($offset >= 60*60) {
						my $h	= int($offset / (60*60));
						$duration	.= "${h}H" if ($h > 0);
						$hours	= int($offset / (60*60));
						$offset	= $offset % (60*60);
					}
					if ($offset >= 60) {
						my $m	= int($offset / 60);
						$duration	.= "${m}M" if ($m > 0);
						$minutes	= int($offset / 60);
						$offset	= $offset % 60;
					}
					my $seconds	= int($offset);
					my $s	= int($offset);
					$duration	.= "${s}S" if ($s > 0 or $duration eq 'PT');
			
					return ($func eq 'TZ')
						? Attean::Literal->new(sprintf('%s%02d:%02d', $minus, $hours, $minutes))
						: Attean::Literal->new( value => $duration, datatype => "http://www.w3.org/2001/XMLSchema#dayTimeDuration");
				} else {
					return Attean::Literal->new('') if ($func eq 'TZ');
					die "TIMEZONE called without a valid dateTime";
				}
			} elsif ($func eq 'NOW') {
				my $value	= DateTime::Format::W3CDTF->new->format_datetime( DateTime->now );
				return Attean::Literal->new( value => $value, datatype => 'http://www.w3.org/2001/XMLSchema#dateTime' );
			} elsif ($func =~ /^(?:STR)?UUID$/) {
				my $u		= Data::UUID->new();
				return Attean::Literal->new($u->to_string( $u->create() )) if ($func eq 'STRUUID');
				return Attean::IRI->new('urn:uuid:' . $u->to_string( $u->create() ));
			} elsif ($func =~ /^(MD5|SHA1|SHA256|SHA384|SHA512)$/) {
				my $hash	= $func =~ s/SHA/SHA-/r;
				my $digest	= eval { Digest->new($hash)->add(encode('UTF-8', $operands[0]->value, Encode::FB_CROAK))->hexdigest };
				return Attean::Literal->new($digest);
			} elsif ($func eq 'STRLANG') {
				my ($str, $lang)	= @operands;
				my @values	= map { $_->value } @operands;
				die "TypeError: STRLANG must be called with two plain literals" unless (blessed($str) and $str->does('Attean::API::Literal') and blessed($lang) and $lang->does('Attean::API::Literal'));
				die "TypeError: STRLANG not called with a simple literal" unless ($str->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string' and not($str->language));
				return Attean::Literal->new( value => $values[0], language => $values[1] );
			} elsif ($func eq 'STRDT') {
				die "TypeError: STRDT" unless ($operands[0]->does('Attean::API::Literal') and not($operands[0]->language));
				if (my $dt = $operands[0]->datatype) {
					die "TypeError: STRDT" unless ($dt->value eq 'http://www.w3.org/2001/XMLSchema#string');
				}
				die "TypeError: STRDT" unless ($operands[1]->does('Attean::API::IRI'));
				my @values	= map { $_->value } @operands;
				my $str	= Attean::Literal->new( value => $values[0], datatype => $values[1] );
				return $str;
			} elsif ($func eq 'SAMETERM') {
				my ($a, $b)	= @operands;
				die "TypeError: SAMETERM" unless (blessed($operands[0]) and blessed($operands[1]));
				return Attean::Literal->false if ($a->compare($b));
				return ($a->value eq $b->value) ? Attean::Literal->true : Attean::Literal->false;
			} elsif ($func =~ /^IS([UI]RI|BLANK|LITERAL|NUMERIC)$/) {
				return $operands[0]->does("Attean::API::$type_roles{$1}") ? Attean::Literal->true : Attean::Literal->false;
			} elsif ($func eq 'REGEX') {
				my ($value, $pattern)	= map { $_->value } @operands;
				return ($value =~ /$pattern/) ? Attean::Literal->true : Attean::Literal->false;
			}
			die "Unimplemented FunctionExpression evaluation: " . $self->operator;
		};
	}
}

package Attean::AggregateExpression 0.001 {
	use Moo;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Enum Str HashRef ConsumerOf);
	use Types::Common::String qw(UpperCaseStr);
	use namespace::clean;

	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		my $args	= $class->$orig(@_);
		$args->{operator}	= UpperCaseStr->coercion->($args->{operator});
		return $args;
	};
	sub BUILD {
		state $type	= Enum[qw(COUNT SUM MIN MAX AVG GROUP_CONCAT SAMPLE)];
		$type->assert_valid(shift->operator);
	}
	has 'operator' => (is => 'ro', isa => UpperCaseStr, coerce => UpperCaseStr->coercion, required => 1);
	has 'scalar_vars' => (is => 'ro', isa => HashRef[Str], default => sub { +{} });
	has 'variable' => (is => 'ro', isa => ConsumerOf['Attean::API::Variable'], required => 1);
	with 'Attean::API::AggregateExpression';
	
	sub impl {
		my $self	= shift;
		my $agg		= $self->operator;
		my ($child)	= @{ $self->children };
		if ($agg eq 'COUNT') {
			if ($child) {
				my $impl	= $child->impl;
				return sub {
					my @terms	= grep { blessed($_) } map { $impl->($_) } @_;
					return Attean::Literal->integer(scalar(@terms));
				};
			} else {
				return sub { return Attean::Literal->integer(scalar(@_)); };
			}
		} elsif ($agg =~ /^(?:SAMPLE|MIN|MAX|SUM|AVG|GROUP_CONCAT)$/) {
			my $impl	= $child->impl;
			if ($agg eq 'SAMPLE') {
				return sub { return $impl->( shift ) };
			} elsif ($agg eq 'MIN' or $agg eq 'MAX') {
				my $expect	= ($agg eq 'MIN') ? 1 : -1;
				return sub {
					my $extrema;
					foreach my $r (@_) {
						my $t	= $impl->( $r );
						return if (not($t) and $agg eq 'MIN');	# unbound is always minimal
						next if (not($t));						# unbound need not be considered for MAX
						$extrema	= $t if (not($extrema) or $extrema->compare($t) == $expect);
					}
					return $extrema;
				};
			} elsif ($agg eq 'SUM' or $agg eq 'AVG') {
				return sub {
					my $count	= 0;
					my $sum		= Attean::Literal->integer(0);
					foreach my $r (@_) {
						my $term	= $impl->( $r );
						if ($term->does('Attean::API::NumericLiteral')) {
							$count++;
							$sum	= Attean::Literal->new( value => ($sum->numeric_value + $term->numeric_value), datatype => $sum->binary_promotion_type($term, '+') );
						} else {
							die "TypeError: AVG";
						}
					}
					if ($agg eq 'AVG') {
						$sum	= not($count) ? undef : Attean::Literal->new( value => ($sum->numeric_value / $count), datatype => $sum->binary_promotion_type(Attean::Literal->integer($count), '/') );
					}
					return $sum;
				};
			} elsif ($agg eq 'GROUP_CONCAT') {
				my $sep	= $self->scalar_vars->{ 'seperator' } // ' ';
				return sub {
					my @strings	= map { eval { $impl->( shift )->value } // '' } @_;
					return Attean::Literal->new(join($sep, @strings));
				};
			}
		}
		die "Unimplemented AggregateExpression evaluation: " . $self->operator;
	}
}

package Attean::ExistsExpression 0.001 {
	use Moo;
	use utf8::all;
	use Types::Standard qw(ConsumerOf);
	use namespace::clean;

	with 'Attean::API::Expression';
	sub arity { return 0 }
	sub BUILDARGS {
		my $class	= shift;
		return $class->SUPER::BUILDARGS(@_, operator => '_exists');
	}
	has 'pattern' => (is => 'ro', isa => ConsumerOf['Attean::API::Algebra']);
	sub as_string {
		my $self	= shift;
		# TODO: implement as_string for EXISTS patterns
		return "EXISTS { ... }";
	}
	
	sub impl {
		my $self	= shift;
		my $algebra	= $self->pattern;
		die "Unimplemented ExistsExpression evaluation";
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
