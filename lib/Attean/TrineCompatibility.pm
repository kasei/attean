use v5.14;
use warnings;

package Attean::TrineCompatibility::Literal 0.001 {
	use Moose::Role;
	use MooseX::Aliases;
	
	with 'Attean::API::Literal';

	# This seems to be required to allow the aliasing below
	sub value {}
	sub language {}
	sub datatype {}
	sub ntriples_string {}
	
	alias 'literal_value' => 'value';
	alias 'literal_value_language' => 'language';
	alias 'literal_datatype' => 'datatype';
	alias 'as_string' => 'ntriples_string';
	alias 'as_ntriples' => 'ntriples_string';
	
	around 'language' => sub {
		my $orig	= shift;
		my $self	= shift;
		my $value	= eval { $self->$orig(@_) };
		return if ($@);
		return $value;
	}; 
	sub sse { die "Unimplemented" }
	sub equal { die "Unimplemented" }
	sub _compare { die "Unimplemented" }
	sub type { return 'LITERAL'; }
	sub has_language {
		my $self	= shift;
		my $l		= $self->language;
		return (defined($l) and length($l));
	}
	sub has_datatype {
		my $self	= shift;
		my $dt		= $self->datatype;
		return (defined($dt) and length($dt));
	}

=item C<< canonicalize >>

Returns a new literal node object whose value is in canonical form (where applicable).

=cut

	sub canonicalize {
		my $self	= shift;
		my $class	= ref($self);
		my $dt		= $self->literal_datatype;
		my $lang	= $self->literal_value_language;
		my $value	= $self->value;
		if (defined $dt) {
			$value	= RDF::Trine::Node::Literal->canonicalize_literal_value( $value, $dt, 1 );
		}
		return $class->new($value, $lang, $dt);
	}

=item C<< canonicalize_literal_value ( $string, $datatype, $warn ) >>

If C<< $datatype >> is a recognized datatype, returns the canonical lexical
representation of the value C<< $string >>. Otherwise returns C<< $string >>.

Currently, xsd:integer, xsd:decimal, and xsd:boolean are canonicalized.
Additionally, invalid lexical forms for xsd:float, xsd:double, and xsd:dateTime
will trigger a warning.

=cut

	sub canonicalize_literal_value {
		my $self	= shift;
		my $value	= shift;
		my $dt		= shift;
		my $warn	= shift;
	
		if ($dt eq 'http://www.w3.org/2001/XMLSchema#integer') {
			if ($value =~ m/^([-+])?(\d+)$/) {
				my $sign	= $1 || '';
				my $num		= $2;
				$sign		= '' if ($sign eq '+');
				$num		=~ s/^0+(\d)/$1/;
				return "${sign}${num}";
			} else {
				warn "Bad lexical form for xsd:integer: '$value'" if ($warn);
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#decimal') {
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
				return "${sign}${num}";
			} elsif ($value =~ m/^([-+])?([.]\d+)$/) {
				my $sign	= $1 || '';
				my $num		= $2;
				$sign		= '' if ($sign eq '+');
				$num		=~ s/^0+(.)/$1/;
				return "${sign}${num}";
			} else {
				warn "Bad lexical form for xsd:deciaml: '$value'" if ($warn);
				$value		= sprintf('%f', $value);
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#float') {
			if ($value =~ m/^(?:([-+])?(?:(\d+(?:\.\d*)?|\.\d+)([Ee][-+]?\d+)?|(INF)))|(NaN)$/) {
				my $sign	= $1;
				my $inf		= $4;
				my $nan		= $5;
				no warnings 'uninitialized';
				$sign		= '' if ($sign eq '+');
				return "${sign}$inf" if ($inf);
				return $nan if ($nan);

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
				return "${sign}${num}${exp}";
			} else {
				warn "Bad lexical form for xsd:float: '$value'" if ($warn);
				$value	= sprintf('%E', $value);
				$value	=~ s/E[+]/E/;
				$value	=~ s/E0+(\d)/E$1/;
				$value	=~ s/(\d)0+E/$1E/;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#double') {
			if ($value =~ m/^(?:([-+])?(?:(\d+(?:\.\d*)?|\.\d+)([Ee][-+]?\d+)?|(INF)))|(NaN)$/) {
				my $sign	= $1;
				my $inf		= $4;
				my $nan		= $5;
				no warnings 'uninitialized';
				$sign		= '' if ($sign eq '+');
				return "${sign}$inf" if ($inf);
				return $nan if ($nan);

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
				return "${sign}${num}${exp}";
			} else {
				warn "Bad lexical form for xsd:double: '$value'" if ($warn);
				$value	= sprintf('%E', $value);
				$value	=~ s/E[+]/E/;
				$value	=~ s/E0+(\d)/E$1/;
				$value	=~ s/(\d)0+E/$1E/;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#boolean') {
			if ($value =~ m/^(true|false|0|1)$/) {
				$value	= 'true' if ($value eq '1');
				$value	= 'false' if ($value eq '0');
				return $value;
			} else {
				warn "Bad lexical form for xsd:boolean: '$value'" if ($warn);
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#dateTime') {
			if ($value =~ m/^-?([1-9]\d{3,}|0\d{3})-(0[1-9]|1[0-2])-(0[1-9]|[12]\d|3[01])T(([01]\d|2[0-3]):[0-5]\d:[0-5]\d(\.\d+)?|(24:00:00(\.0+)?))(Z|(\+|-)((0\d|1[0-3]):[0-5]\d|14:00))?$/) {
				# XXX need to canonicalize the dateTime
				return $value;
			} else {
				warn "Bad lexical form for xsd:boolean: '$value'" if ($warn);
			}
		}
		return $value;
	}

=item C<< is_canonical_lexical_form >>

=cut

	sub is_canonical_lexical_form {
		my $self	= shift;
		my $value	= $self->literal_value;
		my $dt		= $self->literal_datatype;
	
		unless ($dt =~ qr<^http://www.w3.org/2001/XMLSchema#(integer|decimal|float|double|boolean|dateTime|non(Positive|Negative)Integer|(positive|negative)Integer|long|int|short|byte|unsigned(Long|Int|Short|Byte))>) {
			return '0E0';	# zero but true (it's probably ok, but we don't recognize the datatype)
		}
	
		if ($dt =~ m<http://www.w3.org/2001/XMLSchema#(integer|non(Positive|Negative)Integer|(positive|negative)Integer|long|int|short|byte|unsigned(Long|Int|Short|Byte))>) {
			if ($value =~ m/^([-+])?(\d+)$/) {
				return 1;
			} else {
				return 0;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#decimal') {
			if ($value =~ m/^([-+])?((\d+)[.]\d+)$/) {
				return 1;
			} elsif ($value =~ m/^([-+])?([.]\d+)$/) {
				return 1;
			} else {
				return 0;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#float') {
			if ($value =~ m/^[-+]?(\d+\.\d*|\.\d+)([Ee][-+]?\d+)?|[-+]?INF|NaN$/) {
				return 1;
			} elsif ($value =~ m/^[-+]?(\d+(\.\d*)?|\.\d+)([Ee][-+]?\d+)|[-+]?INF|NaN$/) {
				return 1;
			} else {
				return 0;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#double') {
			if ($value =~ m/^[-+]?((\d+(\.\d*))|(\.\d+))([Ee][-+]?\d+)?|[-+]?INF|NaN$/) {
				return 1;
			} elsif ($value =~ m/^[-+]?((\d+(\.\d*)?)|(\.\d+))([Ee][-+]?\d+)|[-+]?INF|NaN$/) {
				return 1;
			} else {
				return 0;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#boolean') {
			if ($value =~ m/^(true|false)$/) {
				return 1;
			} else {
				return 0;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#dateTime') {
			if ($value =~ m/^-?([1-9]\d{3,}|0\d{3})-(0[1-9]|1[0-2])-(0[1-9]|[12]\d|3[01])T(([01]\d|2[0-3]):[0-5]\d:[0-5]\d(\.\d+)?|(24:00:00(\.0+)?))(Z|(\+|-)((0\d|1[0-3]):[0-5]\d|14:00))?$/) {
				return 1;
			} else {
				return 0;
			}
		}
		return 0;
	}

=item C<< is_valid_lexical_form >>

Returns true if the node is of a recognized datatype and has a valid lexical form
for that datatype. If the lexical form is invalid, returns false. If the datatype
is unrecognized, returns zero-but-true.

=cut

	sub is_valid_lexical_form {
		my $self	= shift;
		my $value	= $self->literal_value;
		my $dt		= $self->literal_datatype;
	
		unless ($dt =~ qr<^http://www.w3.org/2001/XMLSchema#(integer|decimal|float|double|boolean|dateTime|non(Positive|Negative)Integer|(positive|negative)Integer|long|int|short|byte|unsigned(Long|Int|Short|Byte))>) {
			return '0E0';	# zero but true (it's probably ok, but we don't recognize the datatype)
		}
	
		if ($dt =~ m<http://www.w3.org/2001/XMLSchema#(integer|non(Positive|Negative)Integer|(positive|negative)Integer|long|int|short|byte|unsigned(Long|Int|Short|Byte))>) {
			if ($value =~ m/^([-+])?(\d+)$/) {
				return 1;
			} else {
				return 0;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#decimal') {
			if ($value =~ m/^([-+])?((\d+)([.]\d*)?)$/) {
				return 1;
			} elsif ($value =~ m/^([-+])?([.]\d+)$/) {
				return 1;
			} else {
				return 0;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#float') {
			if ($value =~ m/^[-+]?(\d+(\.\d*)?|\.\d+)([Ee][-+]?\d+)?|[-+]?INF|NaN$/) {
				return 1;
			} else {
				return 0;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#double') {
			if ($value =~ m/^[-+]?((\d+(\.\d*)?)|(\.\d+))([Ee][-+]?\d+)?|[-+]?INF|NaN$/) {
				return 1;
			} else {
				return 0;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#boolean') {
			if ($value =~ m/^(true|false|0|1)$/) {
				return 1;
			} else {
				return 0;
			}
		} elsif ($dt eq 'http://www.w3.org/2001/XMLSchema#dateTime') {
			if ($value =~ m/^-?([1-9]\d{3,}|0\d{3})-(0[1-9]|1[0-2])-(0[1-9]|[12]\d|3[01])T(([01]\d|2[0-3]):[0-5]\d:[0-5]\d(\.\d+)?|(24:00:00(\.0+)?))(Z|(\+|-)((0\d|1[0-3]):[0-5]\d|14:00))?$/) {
				return 1;
			} else {
				return 0;
			}
		}
		return 0;
	}

=item C<< is_numeric_type >>

Returns true if the literal is a known (xsd) numeric type.

=cut

	sub is_numeric_type {
		my $self	= shift;
		return 0 unless ($self->has_datatype);
		my $type	= $self->literal_datatype;
		if ($type =~ qr<^http://www.w3.org/2001/XMLSchema#(integer|decimal|float|double|non(Positive|Negative)Integer|(positive|negative)Integer|long|int|short|byte|unsigned(Long|Int|Short|Byte))>) {
			return 1;
		} else {
			return 0;
		}
	}

=item C<< numeric_value >>

Returns the numeric value of the literal (even if the literal isn't a known numeric type.

=cut

	sub numeric_value {
		my $self	= shift;
		if ($self->is_numeric_type) {
			my $value	= $self->literal_value;
			if (looks_like_number($value)) {
				my $v	= 0 + eval "$value";	## no critic (ProhibitStringyEval)
				return $v;
			} else {
				throw RDF::Query::Error::TypeError -text => "Literal with numeric type does not appear to have numeric value.";
			}
		} elsif (not $self->has_datatype) {
			if (looks_like_number($self->literal_value)) {
				return 0+$self->literal_value;
			} else {
				return;
			}
		} elsif ($self->literal_datatype eq 'http://www.w3.org/2001/XMLSchema#boolean') {
			return ($self->literal_value eq 'true') ? 1 : 0;
		} else {
			return;
		}
	}
}

package Attean::TrineCompatibility 0.001 {
	use Moose::Util qw(apply_all_roles);
	sub import {
		warn "Making literals Trine-compatible...";
		apply_all_roles('Attean::Literal', 'Attean::TrineCompatibility::Literal');
	}
}


1;
