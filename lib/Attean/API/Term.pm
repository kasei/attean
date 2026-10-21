use v5.14;
use warnings;

=head1 NAME

Attean::API::Term - RDF Terms

=head1 VERSION

This document describes Attean::API::Term version 0.039

=head1 DESCRIPTION

The Attean::API::Term role defines a common API for all RDF terms.

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Term> role:

=over 4

=item C<< value >>

Returns the term's value string.

=item C<< ntriples_string >>

Returns an N-Triples-compatible string serialization.

=back

=head1 METHODS

This role provides default implementations of the following methods:

=over 4

=item C<< as_string >>

Returns a string serialization of the term.

=cut

package Attean::API::Term 0.039 {
	use Moo::Role;
	
	with 'Attean::API::TermOrVariable', 'Attean::API::ResultOrTerm';
	with 'Attean::API::TermOrVariableOrTriplePattern';
	
	requires 'value'; # => (is => 'ro', isa => 'Str', required => 1);
	requires 'ntriples_string';
	sub as_string {
		shift->ntriples_string();
	}

=item C<< ebv >>

Returns true if the term has a true SPARQL "effective boolean value", false otherwise.

=cut

	requires 'ebv';
	requires 'compare';
	requires 'sameTerms';
	
=item C<< order ( $other ) >>

Similar to C<< compare >>, but provides the ordering semantics of ORDER BY.

=cut

	sub order {
		my $self	= shift;
		return $self->compare(@_);
	}
	
	sub __ntriples_string {
		my $self	= shift;
		my $value	= $self->value;
		if ($value =~ m/^[\x20\x23-\x5a\x5d-\x7e]*$/o) {
			return $value;
		}
		
		my @chars	= split(//, $value);
		my $string	= '';
		while (scalar(@chars)) {
			my $c	= shift(@chars);
			my $o	= ord($c);
			if ($o < 0x8) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o == 0x9) {
				$string	.= "\\t";
			} elsif ($o == 0xA) {
				$string	.= "\\n";
			} elsif ($o < 0xC) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o == 0xD) {
				$string	.= "\\r";
			} elsif ($o < 0x1F) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o < 0x21) {
				$string	.= $c;
			} elsif ($o == 0x22) {
				$string	.= "\"";
			} elsif ($o < 0x5B) {
				$string	.= $c;
			} elsif ($o == 0x5C) {
				$string	.= "\\";
			} elsif ($o < 0x7E) {
				$string	.= $c;
			} elsif ($o < 0xFFFF) {
				$string	.= sprintf("\\u%04X", $o);
			} else {
				$string	.= sprintf("\\U%08X", $o);
			}
		}
		return $string;
	}

	with 'Attean::API::TermOrTriple';
}

package Attean::API::Literal 0.039 {
	use IRI;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Maybe Str ConsumerOf);
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;
	use Attean::API::Query;

	use Moo::Role;
	
	with 'Attean::API::Term';
	with 'Attean::API::SPARQLSerializable';
	with 'Attean::API::TermOrVariableOrTriplePattern';
	
	requires 'language'; # => (is => 'ro', isa => 'Maybe[Str]', predicate => 'has_language');
	requires 'base_dir'; # => (is => 'ro', isa => 'Maybe[Str]');
	requires 'datatype'; # => (is => 'ro', isa => 'Attean::API::IRI', required => 1, coerce => 1, default => sub { IRI->new(value => 'http://www.w3.org/2001/XMLSchema#string') });
	
	sub BUILD {}
	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		my $args	= $class->$orig(@_);
		if (my $lang = $args->{language}) {
			my $oldlang	= $lang;
			# http://tools.ietf.org/html/bcp47#section-2.1.1
			# All subtags use lowercase letters
			$lang	= lc($lang);

			# with 2 exceptions: subtags that neither appear at the start of the tag nor occur after singletons
			# i.e. there's a subtag of length at least 2 preceding the exception; and a following subtag or end-of-tag

			# 1. two-letter subtags are all uppercase
			$lang	=~ s{(?<=\w\w-)(\w\w)(?=($|-))}{\U$1}g;

			# 2. four-letter subtags are titlecase
			$lang	=~ s{(?<=\w\w-)(\w\w\w\w)(?=($|-))}{\u\L$1}g;
			$args->{language}	= $lang;
		}
		return $args;
	};
	around 'BUILD' => sub {
		my $orig	= shift;
		my $self	= shift;
		$self->$orig(@_);
		if (my $dt = $self->datatype) {
			my $type	= $dt->value;
			if ($type =~ qr<^http://www[.]w3[.]org/2001/XMLSchema#(?:integer|decimal|float|double|non(?:Positive|Negative)Integer|(?:positive|negative)Integer|long|int|short|byte|unsigned(?:Long|Int|Short|Byte))$>) {
				Moo::Role->apply_roles_to_object($self, 'Attean::API::NumericLiteral');
			} elsif ($type eq 'http://www.w3.org/2001/XMLSchema#boolean') {
				Moo::Role->apply_roles_to_object($self, 'Attean::API::BooleanLiteral');
			} elsif ($type eq 'http://www.w3.org/2001/XMLSchema#dateTime') {
				Moo::Role->apply_roles_to_object($self, 'Attean::API::DateTimeLiteral');
			} elsif (my $role = Attean->get_datatype_role($type)) {
				Moo::Role->apply_roles_to_object($self, $role);
			}
		}
	};
	
	sub sparql_tokens {
		my $self	= shift;
		my @tokens;
		
		my $dt = $self->datatype;
		if ($self->does('Attean::API::NumericLiteral') and $dt->value eq 'http://www.w3.org/2001/XMLSchema#integer') {
			if ($self->value =~ /^\d+$/) {
				my $t	= AtteanX::SPARQL::Token->fast_constructor( INTEGER, -1, -1, -1, -1, [$self->value] );
				return Attean::ListIterator->new( values => [$t], item_type => 'AtteanX::SPARQL::Token' );
			}
		}
		
		my $qtype	= ($self->value =~ /\n/) ? STRING3D : STRING1D;
		my $t	= AtteanX::SPARQL::Token->fast_constructor( $qtype, -1, -1, -1, -1, [$self->value] );
		push(@tokens, $t);
		if (my $lang = $self->language) {
			my $lex	= "$lang";
			if (my $d = $self->base_dir) {
				$lex	.= '--' . $d;
			}
			my $l	= AtteanX::SPARQL::Token->fast_constructor( LANG, -1, -1, -1, -1, ["$lang"] );
			push(@tokens, $l);
		} else {
			if ($dt->value ne 'http://www.w3.org/2001/XMLSchema#string') {
				push(@tokens, AtteanX::SPARQL::Token->hathat);
				push(@tokens, $dt->sparql_tokens->elements);
			}
		}
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
	
	sub ebv {
		my $self	= shift;
		my $value	= $self->value;
		my $dt		= $self->datatype->value;
		if ($dt eq 'http://www.w3.org/2001/XMLSchema#boolean') {
			if ($value eq 'true' or $value eq '1') {
				return 1;
			} elsif ($value eq 'false' or $value eq '0') {
				return 0;
			} else {
				die "TypeError: EBV of an xsd:boolean with invalid lexical form";
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#string') {
			return (length($value) > 0);
		} elsif ($self->language) {
			die "TypeError: EBV of a language-tagged literal";
		} else {
			die "TypeError: EBV of a datatyped literal: $dt";
		}
	}
	
	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return 1 unless ($b->does('Attean::API::Literal') or $b->does('Attean::API::Binding'));
		return -1 if ($b->does('Attean::API::Binding'));
		my $c	= ((($a->language // '') cmp ($b->language // '')) || ($a->datatype->value cmp $b->datatype->value) || ($a->value cmp $b->value));
		return $c;
	}
	
=item C<< sameTerms( $other ) >>

=cut

	sub sameTerms {
		my $lhs	= shift;
		my $rhs	= shift;
		return 0 unless ($rhs->does('Attean::API::Literal'));
		return 0 unless ($lhs->value eq $rhs->value);
		return 0 unless ($lhs->datatype->value eq $rhs->datatype->value);
		return 1;
	}
	
	if ($ENV{ATTEAN_TYPECHECK}) {
		my %map	= (
			language	=> Maybe[Str],
			datatype	=> ConsumerOf['Attean::API::IRI'],
		);
		foreach my $method (keys %map) {
			my $type	= $map{$method};
			around $method => sub {
				my $orig	= shift;
				my $self	= shift;
				my $class	= ref($self);
				my $value	= $self->$orig(@_);
				my $err		= $type->validate($value);
				if ($err) {
					my $name	= $type->display_name;
					die "${class}'s $method failed conformance check for $name: $value";
				}
				return $value;
			};
		}
	}
	
	sub construct_args {
		my $self	= shift;
		my %args;
		$args{language}	= $self->language if ($self->language);
		$args{datatype}	= $self->datatype if ($self->datatype);
		return %args;
	}
	
	sub argument_compatible {
		my $self	= shift;
		my @terms	= @_;
		
		if (my $l = $self->language) {
			foreach my $t (@terms) {
				return 0 unless ($t->does('Attean::API::Literal'));
				if ($t->language) {
					return 0 unless (defined($t->language));
					return 0 unless ($t->language eq $l);
				} else {
					return 0 unless ($t->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string');
				}
			}
			return 1;
		} elsif ($self->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string') {
			foreach my $t (@terms) {
				return 0 unless ($t->does('Attean::API::Literal'));
				return 0 if ($t->language);
				return 0 unless (blessed($t->datatype));
				return 0 unless ($t->datatype->value eq 'http://www.w3.org/2001/XMLSchema#string');
			}
			return 1;
		}
		
		return 0;
	}
	
	sub _ntriples_string {
		my $self	= shift;
		my $str		= sprintf('"%s"', $self->__ntriples_string);
		if (my $l = $self->language) {
			if (my $d = $self->base_dir) {
				return join('', $str, '@', $l, '--', $d);
			} else {
				return join('@', $str, $l);
			}
		} else {
			my $dt	= $self->datatype;
			if ($dt->value eq 'http://www.w3.org/2001/XMLSchema#string') {
				return $str;
			} else {
				return join('^^', $str, $dt->ntriples_string);
			}
		}
	}

	around as_sparql => sub {
		my $orig	= shift;
		my $self	= shift;
		my $s		= $self->$orig(@_);
		
		if ($s =~ m[^"(true|false)"\^\^<http://www[.]w3[.]org/2001/XMLSchema#boolean>$]) {
			return $1;
		}
		
		return $s;
	};

	sub has_base_dir {
		my $self	= shift;
		my $base_dir	= $self->base_dir;
		return (defined($base_dir) and length($base_dir) > 0);
	}

	sub lang_dir {
		my $self	= shift;
		my $lang_key	= '';
		if (my $l = $self->language) {
			$lang_key	= $l;
			if (my $d = $self->base_dir) {
				$lang_key	.= '--' . $d;
			}
		}
		return $lang_key;
	}

}

package Attean::API::DateTimeLiteral 0.039 {
	use DateTime::Format::W3CDTF;

	use Moo::Role;

	sub datetime {
		my $self	= shift;
		my $w3c	= DateTime::Format::W3CDTF->new;
		my $lex	= $self->value;
		my $add	= 0;
		if ($lex =~ /T24:00:00/) {
			$add++;
			$lex	=~ s/24:00:00/00:00:00/;
		}
		my $dt	= $w3c->parse_datetime( $lex );
		if ($add) {
			$dt->add( days => 1 );
		}
		return $dt;
	}
	
	sub equals {
		my $self	= shift;
		my $other	= shift;
		return 0 unless ($other->does('Attean::API::DateTimeLiteral'));
		my $dt	= $self->datetime;
		my $odt	= $other->datetime;
		return 0 if ($dt->time_zone->is_floating != $odt->time_zone->is_floating);
		return (0 == $dt->compare($odt));
	}
}

package Attean::API::CanonicalizingLiteral 0.039 {
	use Moo::Role;
	requires 'canonicalized_term';
	requires 'canonicalized_term_strict';
}

package Attean::API::BooleanLiteral 0.039 {
	use Scalar::Util qw(blessed looks_like_number);

	use Moo::Role;

	sub canonicalized_term_strict {
		my $self	= shift;
		my $value	= $self->value;
		if ($value =~ m/^(true|false|0|1)$/) {
			return ($value eq 'true' or $value eq '1')
				? Attean::Literal->true
				: Attean::Literal->false;
		} else {
			die "Bad lexical form for xsd:boolean: '$value'";
		}
	}

	sub canonicalized_term {
		my $self	= shift;
		my $value	= $self->value;
		if ($value =~ m/^(true|false|0|1)$/) {
			return ($value eq 'true' or $value eq '1')
				? Attean::Literal->true
				: Attean::Literal->false;
		} else {
			return $self;
		}
	}

	sub equals {
		my ($a, $b)	= @_;
		if ($b->does('Attean::API::BooleanLiteral')) {
			my $l	= $a->canonicalized_term;
			my $r	= $b->canonicalized_term;
			return $l->value eq $r->value;
		}
		return $a->SUPER::equals($b);
	}

	with 'Attean::API::Literal', 'Attean::API::CanonicalizingLiteral';
}

package Attean::API::NumericLiteral 0.039 {
	use Scalar::Util qw(blessed looks_like_number);

	use Moo::Role;

	sub equals {
		my ($a, $b)	= @_;
		return 0 unless ($b->does('Attean::API::NumericLiteral'));
		return 0 unless (defined $a->numeric_value);
		return 0 unless (defined $b->numeric_value);
		return $a->numeric_value == $b->numeric_value;
	}

	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return 1 unless ($b->does('Attean::API::Literal') or $b->does('Attean::API::Binding'));
		return -1 if ($b->does('Attean::API::Binding'));
		if ($b->does('Attean::API::NumericLiteral')) {
			return $a->numeric_value <=> $b->numeric_value;
		} else {
			return 1;
# 			Attean::API::Literal::compare($a, $b);
		}
	}

	sub canonicalized_term_strict {
		my $self	= shift;
		return $self->_canonicalized_term(1, @_);
	}
	
	sub canonicalized_term {
		my $self	= shift;
		return $self->_canonicalized_term(0, @_);
	}
	
	sub _canonicalized_term {
		my $self	= shift;
		my $strict	= shift;
		my $value	= $self->value;
		my $type	= $self->datatype->value;
		$type		=~ s/^.*#//;
		if ($type eq 'integer') {
			if ($value =~ m/^([-+])?(\d+)$/) {
				my $sign	= $1 || '';
				my $num		= $2;
				$sign		= '' if ($sign eq '+');
				$num		=~ s/^0+(\d)/$1/;
				return Attean::Literal->integer("${sign}${num}");
			} else {
 				die "Bad lexical form for xsd:integer: '$value'" if ($strict);
				return $self;
			}
		} elsif ($type eq 'negativeInteger') {
			if ($value =~ m/^-(\d+)$/) {
				my $num		= $1;
				$num		=~ s/^0+(\d)/$1/;
				return Attean::Literal->new(value => "-${num}", datatype => 'http://www.w3.org/2001/XMLSchema#negativeInteger');
			} else {
				die "Bad lexical form for xsd:integer: '$value'" if ($strict);
				return $self;
			}
		} elsif ($type eq 'decimal') {
			if ($value =~ m/^([-+])?((\d+)([.]\d*)?)$/) {
				my $sign	= $1 || '';
				my $num		= $2;
				my $int		= $3;
				my $frac	= $4;
				$sign		= '' if ($sign eq '+');
				$num		=~ s/^0+(\d)/$1/;
				$num		=~ s/[.](\d+)0+$/.$1/;
				if ($num =~ /^[.]/) {
					$num	= "0$num";
				}
				if ($num !~ /[.]/) {
					$num	= "${num}.0";
				}
				return Attean::Literal->decimal("${sign}${num}");
			} elsif ($value =~ m/^([-+])?([.]\d+)$/) {
				my $sign	= $1 || '';
				my $num		= $2;
				$sign		= '' if ($sign eq '+');
				$num		=~ s/^0+(.)/$1/;
				return Attean::Literal->decimal("${sign}${num}");
			} else {
				die "Bad lexical form for xsd:deciaml: '$value'" if ($strict);
				return $self;
			}
		} elsif ($type eq 'float') {
			if ($value =~ m/^(?:([-+])?(?:(\d+(?:\.\d*)?|\.\d+)([Ee][-+]?\d+)?|(INF)))|(NaN)$/) {
				my $sign	= $1;
				my $inf		= $4;
				my $nan		= $5;
				no warnings 'uninitialized';
				$sign		= '' if ($sign eq '+');
				return Attean::Literal->float("${sign}$inf") if ($inf);
				return Attean::Literal->float($nan) if ($nan);

				no warnings 'numeric';
				$value		= sprintf('%E', $value);
				$value 		=~ m/^(?:([-+])?(?:(\d+(?:\.\d*)?|\.\d+)([Ee][-+]?\d+)?|(INF)))|(NaN)$/;
				$sign		= $1;
				$inf		= $4;
				$nan		= $5;
				my $num		= $2;
				my $exp		= $3;
				$num		=~ s/[.](\d+?)0+/.$1/;
				$exp	=~ tr/e/E/;
				$exp	=~ s/E[+]/E/;
				$exp	=~ s/E(-?)0+([1-9])$/E$1$2/;
				$exp	=~ s/E(-?)0+$/E${1}0/;
				return Attean::Literal->float("${sign}${num}${exp}");
			} else {
				die "Bad lexical form for xsd:float: '$value'" if ($strict);
				return $self;
			}
		} elsif ($type eq 'boolean') {
			if ($value =~ m/^(true|false|0|1)$/) {
				return ($value eq 'true' or $value eq '1')
					? Attean::Literal->true
					: Attean::Literal->false;
			} else {
				die "Bad lexical form for xsd:boolean: '$value'" if ($strict);
				return $self;
			}
		} elsif ($type eq 'double') {
			if ($value =~ m/^(?:([-+])?(?:(\d+(?:\.\d*)?|\.\d+)([Ee][-+]?\d+)?|(INF)))|(NaN)$/) {
				my $sign	= $1;
				my $inf		= $4;
				my $nan		= $5;
				no warnings 'uninitialized';
				$sign		= '' if ($sign eq '+');
				return Attean::Literal->double("${sign}$inf") if ($inf);
				return Attean::Literal->double($nan) if ($nan);

				no warnings 'numeric';
				$value		= sprintf('%E', $value);
				$value 		=~ m/^(?:([-+])?(?:(\d+(?:\.\d*)?|\.\d+)([Ee][-+]?\d+)?|(INF)))|(NaN)$/;
				$sign		= $1;
				$inf		= $4;
				$nan		= $5;
				my $num		= $2;
				my $exp		= $3;
				$num		=~ s/[.](\d+?)0+/.$1/;
				$exp	=~ tr/e/E/;
				$exp	=~ s/E[+]/E/;
				$exp	=~ s/E(-?)0+([1-9])$/E$1$2/;
				$exp	=~ s/E(-?)0+$/E${1}0/;
				return Attean::Literal->double("${sign}${num}${exp}");
			} else {
				die "Bad lexical form for xsd:double: '$value'" if ($strict);
				return $self;
			}
		} else {
			warn "No canonicalization for type $type";
		}
		return $self;
	}
	
	sub is_integer_type {
		my $self	= shift;
		my $type	= $self->datatype->value;
		return scalar($type =~ qr<^http://www[.]w3[.]org/2001/XMLSchema#(?:integer|non(?:Positive|Negative)Integer|(?:positive|negative)Integer|long|int|short|byte|unsigned(?:Long|Int|Short|Byte))$>);
	}
	
	sub ebv {
		my $self	= shift;
		return ($self->numeric_value != 0);
	}

	sub numeric_value {
		my $self	= shift;
		my $v		= $self->value;
		return (looks_like_number($v)) ? eval $v : undef;
	}

	{
		my %type_hierarchy	= (
			'integer'				=> 'decimal',
			'nonPositiveInteger'	=> 'integer',
			'negativeInteger'		=> 'nonPositiveInteger',
			'long'					=> 'integer',
			'int'					=> 'long',
			'short'					=> 'int',
			'byte'					=> 'short',
			'nonNegativeInteger'	=> 'integer',
			'unsignedLong'			=> 'nonNegativeInteger',
			'unsignedInt'			=> 'unsignedLong',
			'unsignedShort'			=> 'unsignedInt',
			'unsignedByte'			=> 'unsignedShort',
			'positiveInteger'		=> 'nonNegativeInteger',
		);
		sub is_base_type {
			my $type	= shift;
			return ($type =~ m/\b(integer|decimal|float|double)$/);
		}
		sub _lowest_base_type {
			my ($lhs, $rhs)	= @_;
			for ($lhs, $rhs) {
				s/^.*#//;
			}
			if ($lhs eq $rhs) {
				if (is_base_type($lhs)) {
					return "http://www.w3.org/2001/XMLSchema#$lhs";
				}
			}
			my $cur	= $lhs;
			my %ancestors;
			do {
				$ancestors{$cur}++;
				if ($cur eq $rhs) {
					if (is_base_type($cur)) {
						return "http://www.w3.org/2001/XMLSchema#$cur";
					}
				}
			} while ($cur = $type_hierarchy{$cur});

			$cur	= $rhs;
			my $seen_float		= 0;
			my $seen_decimal	= 0;
			
			while ($cur = $type_hierarchy{$cur}) {
				if (exists $ancestors{$cur}) {
					if (is_base_type($cur)) {
						return "http://www.w3.org/2001/XMLSchema#$cur";
					}
				}
				if ($cur eq 'float') {
					$seen_float++;
					if (my $r = _lowest_base_type($lhs, 'double')) {
						return $r;
					}
				}
				if ($cur eq 'decimal') {
					$seen_decimal++;
					if (my $r = _lowest_base_type($lhs, 'float')) {
						return $r;
					}
					if (my $r = _lowest_base_type($lhs, 'double')) {
						return $r;
					}
				}
			}
			if ($ancestors{'float'}) {
				# seen a float type on the lhs
				return "http://www.w3.org/2001/XMLSchema#double" if ($rhs eq 'float');
			} elsif ($seen_float) {
				# seen a float type on the rhs
				return "http://www.w3.org/2001/XMLSchema#double" if (exists $ancestors{'float'});
			}
			if ($ancestors{'decimal'}) {
				# seen a decimal type on the lhs
				return "http://www.w3.org/2001/XMLSchema#float" if ($rhs eq "float");
				return "http://www.w3.org/2001/XMLSchema#double" if ($rhs eq "double");
			} elsif ($seen_decimal) {
				# seen a decimal type on the rhs
				return "http://www.w3.org/2001/XMLSchema#float" if (exists $ancestors{"float"});
				return "http://www.w3.org/2001/XMLSchema#double" if (exists $ancestors{"double"});
			}
			if ($rhs eq 'float') {
				$seen_float++;
				if (my $r = _lowest_base_type($lhs, 'double')) {
					return $r;
				}
			} elsif ($rhs eq 'decimal') {
				$seen_decimal++;
				if (my $r = _lowest_base_type($lhs, 'float')) {
					return $r;
				}
				if (my $r = _lowest_base_type($lhs, 'double')) {
					return $r;
				}
			}
			return;
		}
		sub _lca {
			my ($lhs, $rhs)	= @_;
			for ($lhs, $rhs) {
				s/^.*#//;
			}
			return "http://www.w3.org/2001/XMLSchema#$lhs" if ($lhs eq $rhs);
			my $cur	= $lhs;
			my %ancestors	= ($cur => 1);
			while ($cur = $type_hierarchy{$cur}) {
				$ancestors{$cur}++;
				return "http://www.w3.org/2001/XMLSchema#$cur" if ($cur eq $rhs);
			}
			$cur	= $rhs;
			my $seen_float		= 0;
			my $seen_decimal	= 0;
			while ($cur = $type_hierarchy{$cur}) {
				$seen_float++ if ($cur eq 'float');
				$seen_decimal++ if ($cur eq 'decimal');
				return "http://www.w3.org/2001/XMLSchema#$cur" if exists $ancestors{$cur};
			}
			if ($ancestors{'decimal'}) {
				# seen decimal on the lhs
				return "http://www.w3.org/2001/XMLSchema#float" if ($rhs eq "float");
				return "http://www.w3.org/2001/XMLSchema#double" if ($rhs eq "double");
			} elsif ($seen_decimal) {
				# seen decimal on the rhs
				return "http://www.w3.org/2001/XMLSchema#float" if (exists $ancestors{"float"});
				return "http://www.w3.org/2001/XMLSchema#double" if (exists $ancestors{"double"});
			}
			return;
		}
		sub binary_promotion_type {
			my $self	= shift;
			my $rhs		= shift;
			my $op		= shift;
			
			if ($op =~ m<^[-+*]$>) {
				# return common numeric type
				if (my $lbtype = _lowest_base_type($self->datatype->value, $rhs->datatype->value)) {
					return $lbtype;
				} elsif (my $lctype = _lca($self->datatype->value, $rhs->datatype->value)) {
					return $lctype;
				}
				return 'http://www.w3.org/2001/XMLSchema#double';
			} elsif ($op eq '/') {
				if ($self->is_integer_type and $rhs->is_integer_type) {
					# return xsd:decimal if both operands are integers
					return 'http://www.w3.org/2001/XMLSchema#decimal';
				}
				if (my $type = _lca($self->datatype->value, $rhs->datatype->value)) {
					return $type;
				} else {
					my $lt	= $self->datatype->value;
					my $rt	= $rhs->datatype->value;
					if ($lt =~ /#double$/ or $rt =~ /#double$/) {
						return "http://www.w3.org/2001/XMLSchema#double";
					} elsif ($lt =~ /#float$/ or $rt =~ /#float$/) {
						return "http://www.w3.org/2001/XMLSchema#float";
					}
				}
				return 'http://www.w3.org/2001/XMLSchema#double';
			}
			die "Unexpected numeric operation in binary_promotion_type: $op";
		}
	}
	with 'Attean::API::Literal', 'Attean::API::CanonicalizingLiteral';
}

package Attean::API::Blank 0.039 {
	use Scalar::Util qw(blessed);
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;
	use Attean::API::Query;

	use Moo::Role;
	
	sub ebv {
		die "TypeError: EBV of a blank";
	 }
	with 'Attean::API::Term', 'Attean::API::BlankOrIRI', 'Attean::API::BlankOrIRIOrTriple';
;
	with 'Attean::API::SPARQLSerializable';
	
	sub sparql_tokens {
		my $self	= shift;
		my $t	= AtteanX::SPARQL::Token->fast_constructor( BNODE, -1, -1, -1, -1, [$self->value] );
		return Attean::ListIterator->new( values => [$t], item_type => 'AtteanX::SPARQL::Token' );
	}
	
	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return -1 unless ($b->does('Attean::API::Blank'));
		return ($a->value cmp $b->value);
	}

=item C<< sameTerms( $other ) >>

=cut

	sub sameTerms {
		my $lhs	= shift;
		my $rhs	= shift;
		return 0 unless ($rhs->does('Attean::API::Blank'));
		return 0 unless ($lhs->value eq $rhs->value);
		return 1;
	}
}

package Attean::API::IRI 0.039 {
	use IRI;
	use Scalar::Util qw(blessed);
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;
	use Attean::API::Query;

	use Moo::Role;
	
	sub ebv {
		die "TypeError: EBV of a blank";
	}
	with 'Attean::API::Term', 'Attean::API::BlankOrIRI', 'Attean::API::BlankOrIRIOrTriple';
	with 'Attean::API::SPARQLSerializable';
	
	sub sparql_tokens {
		my $self	= shift;
		my @tokens;
		if ($self->value eq '') {
			push(@tokens, AtteanX::Parser::Turtle::Token->a);
		} else {
			push(@tokens, AtteanX::SPARQL::Token->fast_constructor( IRI, -1, -1, -1, -1, [$self->value] ));
		}
		return Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::SPARQL::Token' );
	}
	
	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return -1 if ($b->does('Attean::API::Literal') or $b->does('Attean::API::Binding'));
		return 1 unless ($b->does('Attean::API::IRI'));
		return ($a->value cmp $b->value);
	}
	
=item C<< sameTerms( $other ) >>

=cut

	sub sameTerms {
		my $lhs	= shift;
		my $rhs	= shift;
		return 0 unless ($rhs->does('Attean::API::IRI'));
		return 0 unless ($lhs->value eq $rhs->value);
		return 1;
	}

	sub _ntriples_string {
		my $self	= shift;
		return sprintf('<%s>', $self->__ntriples_string);
	}

	around as_sparql => sub {
		my $orig	= shift;
		my $self	= shift;
		
		if ($self->value eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') {
			return 'a';
		}

		return $self->$orig(@_);
	};
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

Copyright (c) 2014--2026 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
