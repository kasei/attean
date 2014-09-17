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
				my $binding	= shift;
# 				warn "Evaluating ValueExpression for variable '" . $node->value . "' on binding " . $binding->as_string;
				my $term	= $binding->value($node->value);
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
				my $lbv	= eval { $lhsi->(@_) };
				my $rbv	= eval { $rhsi->(@_) };
				die "TypeError $op" unless ($lbv or $rbv);
				return $false if (not($lbv) and not($rbv->ebv));
				return $false if (not($rbv) and not($lbv->ebv));
				die "TypeError $op" unless ($lbv and $rbv);
				return ($lbv->ebv && $rbv->ebv) ? $true : $false;
			}
		} elsif ($op eq '||') {
			return sub {
				my $lbv	= eval { $lhsi->(@_) };
				return $true if ($lbv and $lbv->ebv);
				my $rbv	= eval { $rhsi->(@_) };
				die "TypeError $op" unless ($rbv);
				return $true if ($rbv->ebv);
		
				return $false if ($lbv);
				die "TypeError $op";
			}
		} elsif ($op =~ m#^(?:[-+*/])$#) { # numeric operators: - + * /
			return sub {
				($lhs, $rhs)	= map { $_->(@_) } ($lhsi, $rhsi);
				for ($lhs, $rhs) { die "TypeError $op" unless $_->does('Attean::API::NumericLiteral'); }
				my $lv	= $lhs->numeric_value;
				my $rv	= $rhs->numeric_value;
				return Attean::Literal->new( value => eval "$lv $op $rv", datatype => $lhs->binary_promotion_type($rhs, $op) );
			};
		} elsif ($op =~ /^!?=$/) {
			return sub {
				($lhs, $rhs)	= map { $_->(@_) } ($lhsi, $rhsi);
				for ($lhs, $rhs) { die "TypeError $op" unless $_->does('Attean::API::Term'); }
				my $ok	= ($lhs->equals($rhs));
				$ok		= not($ok) if ($op eq '!=');
				return $ok ? Attean::Literal->true : Attean::Literal->false;
			}
		} elsif ($op =~ /^[<>]=?$/) {
			return sub {
				($lhs, $rhs)	= map { $_->(@_) } ($lhsi, $rhsi);
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
	use Types::Standard qw(Enum);
	use Types::Common::String qw(UpperCaseStr);
	use URI::Escape;
	use Encode qw(encode);
	use POSIX qw(ceil floor);
	use Digest;
	use DateTime::Format::W3CDTF;
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
		state $type	= Enum[qw(STR LANG LANGMATCHES DATATYPE BOUND IRI URI BNODE RAND ABS CEIL FLOOR ROUND CONCAT SUBSTR STRLEN REPLACE UCASE LCASE ENCODE_FOR_URI CONTAINS STRSTARTS STRENDS STRBEFORE STRAFTER YEAR MONTH DAY HOURS MINUTES SECONDS TIMEZONE TZ NOW UUID STRUUID MD5 SHA1 SHA256 SHA384 SHA512 COALESCE IF STRLANG STRDT SAMETERM ISIRI ISURI ISBLANK ISLITERAL ISNUMERIC REGEX)];
		$type->assert_valid($self->operator);
	}
	has 'operator' => (is => 'ro', isa => UpperCaseStr, coerce => UpperCaseStr->coercion, required => 1);
	with 'Attean::API::NaryExpression';
	
	sub impl {
		my $self	= shift;
		my $func	= $self->operator;
		my @children	= map { $_->impl } @{ $self->children };
		my %type_roles		= qw(URI IRI IRI IRI BLANK Blank LITERAL Literal NUMERIC NumericLiteral);
		my %type_classes	= qw(URI Attean::IRI IRI Attean::IRI STR Attean::Literal);
		return sub {
			my $r	= shift;
			
			if ($func eq 'IF') {
				# TODO:
			} elsif ($func eq 'COALESCE') {
				foreach my $c (@children) {
					my $t	= eval { $c->( $r ) };
					return $t if $t;
				}
				return;
			}
			
			my @operands	= map { $_->( $r ) } @children;
			if ($func =~ /^(STR|[UI]RI)$/) {
				return $type_classes{$1}->new($operands[0]->value);
			} elsif ($func eq 'BNODE') {
				# TODO: BNODE(literal)
				return Attean::Blank->new($operands[0]->value);
			} elsif ($func eq 'LANG') {
				return Attean::Literal->new($operands[0]->language);
			# LANGMATCHES
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
				return Attean::Literal->new( value => sprintf('%.0f', $operands[0]->numeric_value), $operands[0]->construct_args );
			} elsif ($func eq 'CONCAT') {
				return Attean::Literal->new( join('', map { $_->value } @operands) );
			} elsif ($func eq 'SUBSTR') {
				my $str	= shift(@operands);
				my @args	= map { $_->numeric_value } @operands;
				my $v	= scalar(@args == 1) ? substr($str->value, $args[0]) : substr($str->value, $args[0], $args[1]);
				return Attean::Literal->new( value => $v, $str->construct_args );
			} elsif ($func eq 'STRLEN') {
				return Attean::Literal->integer(length($operands[0]->value));
			# REPLACE
			} elsif ($func =~ /^[UL]CASE$/) {
				return Attean::Literal->new( ($func eq 'UCASE') ? uc($operands[0]->value) : lc($operands[0]->value) );
			} elsif ($func eq 'ENCODE_FOR_URI') {
				return Attean::Literal->new( uri_escape($operands[0]->value) );
			# CONTAINS
			# STRSTARTS
			# STRENDS
			# STRBEFORE
			# STRAFTER
			# YEAR
			# MONTH
			# DAY
			# HOURS
			# MINUTES
			# SECONDS
			# TIMEZONE
			# TZ
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
			# STRLANG
			# STRDT
			# SAMETERM
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
