use v5.14;
use warnings;

=head1 NAME

Attean::API::Term - RDF Terms

=head1 VERSION

This document describes Attean::API::Term version 0.000

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

=back

=cut

package Attean::API::Term 0.001 {
	use Moo::Role;
	
	with 'Attean::API::TermOrVariable', 'Attean::API::ResultOrTerm';
	
	requires 'value'; # => (is => 'ro', isa => 'Str', required => 1);
	requires 'ntriples_string';
	sub as_string {
		shift->ntriples_string();
	}
	sub ebv { return 0; }
	
	requires 'compare';
	
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
}

package Attean::API::Literal 0.001 {
	use Moo::Role;
	use IRI;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Maybe Str ConsumerOf);
	use namespace::clean;
	
	with 'Attean::API::Term';
	
	requires 'language'; # => (is => 'ro', isa => 'Maybe[Str]', predicate => 'has_language');
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
				Role::Tiny->apply_roles_to_object($self, 'Attean::API::NumericLiteral');
			} elsif ($type eq 'http://www.w3.org/2001/XMLSchema#dateTime') {
				Role::Tiny->apply_roles_to_object($self, 'Attean::API::DateTimeLiteral');
			}
		}
	};
	
	sub ebv {
		my $self	= shift;
		my $value	= $self->value;
		my $dt		= $self->datatype->value;
		if ($dt eq 'http://www.w3.org/2001/XMLSchema#boolean') {
			return ($value eq 'true' or $value eq '1');
		} else {
			return (length($value) > 0);
		}
	}
	
	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return 1 unless ($b->does('Attean::API::Literal'));
		my $c	= ((($a->language // '') cmp ($b->language // '')) || ($a->datatype->value cmp $b->datatype->value) || ($a->value cmp $b->value));
		return $c;
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
	
	sub _ntriples_string {
		my $self	= shift;
		my $str		= sprintf('"%s"', $self->__ntriples_string);
		if (my $l = $self->language) {
			return join('@', $str, $l);
		} else {
			my $dt	= $self->datatype;
			if ($dt->value eq 'http://www.w3.org/2001/XMLSchema#string') {
				return $str;
			} else {
				return join('^^', $str, $dt->ntriples_string);
			}
		}
	}
}

package Attean::API::DateTimeLiteral 0.001 {
	use Moo::Role;
	use DateTime::Format::W3CDTF;
	use namespace::clean;
	sub datetime {
		my $self	= shift;
		my $w3c	= DateTime::Format::W3CDTF->new;
		return $w3c->parse_datetime( $self->value );
	}
}

package Attean::API::CanonicalizingLiteral 0.001 {
	use Moo::Role;
	requires 'canonicalized_term';
}

package Attean::API::NumericLiteral 0.001 {
	use Moo::Role;
	use Scalar::Util qw(blessed looks_like_number);
	use namespace::clean;
	
	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return 1 unless ($b->does('Attean::API::Literal'));
		if ($b->does('Attean::API::NumericLiteral')) {
			return $a->numeric_value <=> $b->numeric_value;
		} else {
			return 1;
# 			Attean::API::Literal::compare($a, $b);
		}
	}
	
	sub canonicalized_term {
		my $self	= shift;
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
				die "Bad lexical form for xsd:integer: '$value'";
			}
		} elsif ($type eq 'decimal') {
			if ($value =~ m/^([-+])?((\d+)([.]\d*)?)$/) {
				my $sign	= $1 || '';
				my $num		= $2;
				my $int		= $3;
				my $frac	= $4;
				$sign		= '' if ($sign eq '+');
				$num		=~ s/^0+(.)/$1/;
				$num		=~ s/[.](\d)0+$/.$1/;
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
				die "Bad lexical form for xsd:deciaml: '$value'";
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
				die "Bad lexical form for xsd:float: '$value'";
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
				die "Bad lexical form for xsd:double: '$value'";
			}
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
			while ($cur = $type_hierarchy{$cur}) {
				return "http://www.w3.org/2001/XMLSchema#$cur" if exists $ancestors{$cur};
			}
			return;
		}
		sub binary_promotion_type {
			my $self	= shift;
			my $rhs		= shift;
			my $op		= shift;
			
			if ($op =~ m<^[-+*]$>) {
				# return common numeric type
				if (my $type = _lca($self->datatype->value, $rhs->datatype->value)) {
					return $type;
				}
				return 'http://www.w3.org/2001/XMLSchema#double';
			} elsif ($op eq '/') {
				if ($self->is_integer_type and $rhs->is_integer_type) {
					# return xsd:decimal if both operands are integers
					return 'http://www.w3.org/2001/XMLSchema#decimal';
				}
				if (my $type = _lca($self->datatype->value, $rhs->datatype->value)) {
					return $type;
				}
				return 'http://www.w3.org/2001/XMLSchema#double';
			}
			die "Unexpected numeric operation in binary_promotion_type: $op";
		}
	}
	with 'Attean::API::Literal', 'Attean::API::CanonicalizingLiteral';
}

package Attean::API::Blank 0.001 {
	use Moo::Role;
	use Scalar::Util qw(blessed);
	use namespace::clean;
	
	sub ebv { return 1; }
	with 'Attean::API::Term', 'Attean::API::BlankOrIRI';

	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return -1 unless ($b->does('Attean::API::Blank'));
		return ($a->value cmp $b->value);
	}
}

package Attean::API::IRI 0.001 {
	use Moo::Role;
	use IRI;
	use Scalar::Util qw(blessed);
	use namespace::clean;
	
	sub ebv { return 1; }
	with 'Attean::API::Term', 'Attean::API::BlankOrIRI';
	
	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return -1 if ($b->does('Attean::API::Literal'));
		return 1 unless ($b->does('Attean::API::IRI'));
		return ($a->value cmp $b->value);
	}
	
	sub _ntriples_string {
		my $self	= shift;
		return sprintf('<%s>', $self->__ntriples_string);
	}
}

1;

__END__

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
