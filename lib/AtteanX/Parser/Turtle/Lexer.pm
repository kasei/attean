# AtteanX::Parser::Turtle::Lexer
# -----------------------------------------------------------------------------

=head1 NAME

AtteanX::Parser::Turtle::Lexer - Tokenizer for parsing Turtle, TriG, and N-Triples

=head1 VERSION

This document describes AtteanX::Parser::Turtle::Lexer version 0.000

=head1 SYNOPSIS

 use AtteanX::Parser::Turtle::Lexer;
 my $l = AtteanX::Parser::Turtle::Lexer->new( file => $fh );
 while (my $t = $l->get_token) {
   ...
 }

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::Turtle::Lexer 0.001 {
	use AtteanX::Parser::Turtle::Constants;
	use v5.14;
	use strict;
	use warnings;
	use Data::Dumper;
	use Moo;
	use Types::Standard qw(FileHandle Ref Str Int ArrayRef HashRef ConsumerOf InstanceOf);
	use namespace::clean;

	my $r_nameChar_extra		= qr'[-0-9\x{B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]'o;
	my $r_nameStartChar_minus_underscore	= qr'[A-Za-z\x{00C0}-\x{00D6}\x{00D8}-\x{00F6}\x{00F8}-\x{02FF}\x{0370}-\x{037D}\x{037F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{00010000}-\x{000EFFFF}]'o;
	my $r_nameStartChar			= qr/[A-Za-z_\x{00C0}-\x{00D6}\x{00D8}-\x{00F6}\x{00F8}-\x{02FF}\x{0370}-\x{037D}\x{037F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]/o;
	my $r_nameChar				= qr/${r_nameStartChar}|[-0-9\x{b7}\x{0300}-\x{036f}\x{203F}-\x{2040}]/o;
	my $r_prefixName			= qr/(?:(?!_)${r_nameStartChar})(?:$r_nameChar)*/o;
	my $r_nameChar_test			= qr"(?:$r_nameStartChar|$r_nameChar_extra)"o;
	my $r_double				= qr'[+-]?([0-9]+\.[0-9]*[eE][+-]?[0-9]+|\.[0-9]+[eE][+-]?[0-9]+|[0-9]+[eE][+-]?[0-9]+)'o;
	my $r_decimal				= qr'[+-]?(([0-9]+\.[0-9]+)|\.([0-9])+)'o;
	my $r_integer				= qr'[+-]?[0-9]+'o;
	my $r_PN_CHARS_U			= qr/[_A-Za-z_\x{00C0}-\x{00D6}\x{00D8}-\x{00F6}\x{00F8}-\x{02FF}\x{0370}-\x{037D}\x{037F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]/o;
	my $r_PN_CHARS				= qr"${r_PN_CHARS_U}|[-0-9\x{00B7}\x{0300}-\x{036F}\x{203F}-\x{2040}]"o;
	my $r_bnode_id				= qr"(?:${r_PN_CHARS_U}|[0-9])((${r_PN_CHARS}|[.])*${r_PN_CHARS})?"o;

	my $r_PN_CHARS_BASE			= qr/([A-Z]|[a-z]|[\x{00C0}-\x{00D6}]|[\x{00D8}-\x{00F6}]|[\x{00F8}-\x{02FF}]|[\x{0370}-\x{037D}]|[\x{037F}-\x{1FFF}]|[\x{200C}-\x{200D}]|[\x{2070}-\x{218F}]|[\x{2C00}-\x{2FEF}]|[\x{3001}-\x{D7FF}]|[\x{F900}-\x{FDCF}]|[\x{FDF0}-\x{FFFD}]|[\x{10000}-\x{EFFFF}])/o;
	my $r_PN_PREFIX				= qr/(${r_PN_CHARS_BASE}((${r_PN_CHARS}|[.])*${r_PN_CHARS})?)/o;
	my $r_PN_LOCAL_ESCAPED		= qr{(\\([-~.!&'()*+,;=/?#@%_\$]))|%[0-9A-Fa-f]{2}}o;
	my $r_PN_LOCAL				= qr/((${r_PN_CHARS_U}|[:0-9]|${r_PN_LOCAL_ESCAPED})((${r_PN_CHARS}|${r_PN_LOCAL_ESCAPED}|[:.])*(${r_PN_CHARS}|[:]|${r_PN_LOCAL_ESCAPED}))?)/o;
	my $r_PN_LOCAL_BNODE		= qr/((${r_PN_CHARS_U}|[0-9])((${r_PN_CHARS}|[.])*${r_PN_CHARS})?)/o;
	my $r_PNAME_NS				= qr/((${r_PN_PREFIX})?:)/o;
	my $r_PNAME_LN				= qr/(${r_PNAME_NS}${r_PN_LOCAL})/o;

	has file			=> ( is => 'ro', isa => FileHandle, required => 1, );
	has linebuffer		=> ( is => 'rw', isa => Str, default => '', );
	has line			=> ( is => 'rw', isa => Int, default => 1, );
	has column			=> ( is => 'rw', isa => Int, default => 1, );
	has buffer			=> ( is => 'rw', isa => Str, default => '', );
	has start_column	=> ( is => 'rw', isa => Int, default => -1, );
	has start_line		=> ( is => 'rw', isa => Int, default => -1, );
	sub BUILDARGS {
		my $class	= shift;
		return { file => shift } if (scalar(@_) == 1);
		return $class->SUPER::BUILDARGS(@_);
	}

=item C<< new_token ( $type, $start_line, $start_column, @values ) >>

Returns a new token with the given type and optional values, capturing the
current line and column of the input data.

=cut

	sub new_token {
		my $self		= shift;
		my $type		= shift;
		my $start_line	= shift;
		my $start_col	= shift;
		my $line		= $self->line;
		my $col			= $self->column;
		return AtteanX::Parser::Turtle::Token->fast_constructor( $type, $start_line, $start_col, $line, $col, \@_ );
	}

	my %CHAR_TOKEN	= (
		'.'	=> DOT,
		';'	=> SEMICOLON,
		'['	=> LBRACKET,
		']'	=> RBRACKET,
		'('	=> LPAREN,
		')'	=> RPAREN,
		'{'	=> LBRACE,
		'}'	=> RBRACE,
		','	=> COMMA,
		'='	=> EQUALS,
	);

	my %METHOD_TOKEN	= (
	# 	q[#]	=> '_get_comment',
		q[@]	=> '_get_keyword',
		q[<]	=> '_get_iriref',
		q[_]	=> '_get_bnode',
		q[']	=> '_get_single_literal',
		q["]	=> '_get_double_literal',
		q[:]	=> '_get_pname',
		(map {$_ => '_get_number'} (0 .. 9, '-', '+'))
	);

=item C<< get_token >>

Returns the next token present in the input.

=cut

	sub get_token {
		my $self	= shift;
		while (1) {
			$self->fill_buffer unless (length($self->{buffer}));

			if ($self->{buffer} =~ /^[ \r\n\t]+/o) {
				$self->_read_length($+[0]);
				# we're ignoring whitespace tokens, but we could return them here instead of falling through to the 'next':
	# 			return $self->new_token(WS);
				next;
			}

			my $c	= $self->_peek_char();
			return unless (defined($c));

			if ($c eq '#') {
				# we're ignoring comment tokens, but we could return them here instead of falling through to the 'next':
				$self->_get_comment();
				next;
			}
		
			my $start_column	= $self->{column};
			my $start_line		= $self->{line};
		
			$self->start_column( $start_column );
			$self->start_line( $start_line );
		
			if ($c eq '.' and $self->{buffer} =~ /^$r_decimal/) {
				return $self->_get_number();
			}
		
			if (defined(my $name = $CHAR_TOKEN{$c})) { $self->_get_char; return $self->new_token($name, $start_line, $start_column); }
			elsif (defined(my $method = $METHOD_TOKEN{$c})) { return $self->$method() }
			elsif ($c =~ /[A-Za-z\x{00C0}-\x{00D6}\x{00D8}-\x{00F6}\x{00F8}-\x{02FF}\x{0370}-\x{037D}\x{037F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]/o) {
				if ($self->{buffer} =~ /^a(?!:)\s/o) {
					$self->_get_char;
					return $self->new_token(A, $start_line, $start_column);
				} elsif ($self->{buffer} =~ /^(?:true|false)(?!:)\b/o) {
					my $bool	= $self->_read_length($+[0]);
					return $self->new_token(BOOLEAN, $start_line, $start_column, $bool);
				} elsif ($self->{buffer} =~ /^BASE(?!:)\b/oi) {
					$self->_read_length(4);
					return $self->new_token(SPARQLBASE, $start_line, $start_column);
				} elsif ($self->{buffer} =~ /^PREFIX(?!:)\b/io) {
					$self->_read_length(6);
					return $self->new_token(SPARQLPREFIX, $start_line, $start_column);
				} else {
					return $self->_get_pname;
				}
			}
			elsif ($c eq '^') {
				$self->_read_word('^^'); return $self->new_token(HATHAT, $start_line, $start_column); }
			else {
	# 			Carp::cluck sprintf("Unexpected byte '$c' (0x%02x)", ord($c));
				return $self->_throw_error(sprintf("Unexpected byte '%s' (0x%02x)", $c, ord($c)));
			}
			warn sprintf('byte: 0x%x', ord($c));
		}
	}

=begin private

=cut

=item C<< fill_buffer >>

Fills the internal parse buffer with a new line from the input source.

=cut

	sub fill_buffer {
		my $self	= shift;
		unless (length($self->buffer)) {
			my $line	= $self->file->getline;
			$self->{buffer}	.= $line if (defined($line));
		}
	}

=item C<< check_for_bom >>

Checks the input buffer for a Unicode BOM, and consumes it if it is present.

=cut

	sub check_for_bom {
		my $self	= shift;
		my $c		= $self->_peek_char();
		$self->_get_char if (defined($c) and $c eq "\x{FEFF}");
	}

	sub _get_char_safe {
		my $self	= shift;
		my $char	= shift;
		my $c		= $self->_get_char;
		$self->_throw_error("Expected '$char' but got '$c'") if ($c ne $char);
		return $c;
	}

	sub _get_char_fill_buffer {
		my $self	= shift;
		if (length($self->{buffer}) == 0) {
			$self->fill_buffer;
			return if (length($self->{buffer}) == 0);
		}
		my $c		= substr($self->{buffer}, 0, 1, '');
		if ($c eq "\n") {
			$self->{line}	= 1+$self->{line};
			$self->{column}	= 1;
		} else {
			$self->{column}	= 1+$self->{column};
		}
		return $c;
	}

	sub _get_char {
		my $self	= shift;
		my $c		= substr($self->{buffer}, 0, 1, '');
		if ($c eq "\n") {
	# 		$self->{linebuffer}	= '';
			$self->{line}	= 1+$self->{line};
			$self->{column}	= 1;
		} else {
	# 		$self->{linebuffer}	.= $c;
			$self->{column}	= 1+$self->{column};
		}
		return $c;
	}

	sub _peek_char {
		my $self	= shift;
		if (length($self->{buffer}) == 0) {
			$self->fill_buffer;
			return if (length($self->{buffer}) == 0);
		}
		return substr($self->{buffer}, 0, 1);
	}

	sub _read_word {
		my $self	= shift;
		my $word	= shift;
		$self->fill_buffer while (length($self->{buffer}) < length($word));
		$self->_throw_error("Expected '$word'") if (substr($self->{buffer}, 0, length($word)) ne $word);
	
		my $lines	= ($word =~ tr/\n//);
		my $lastnl	= rindex($word, "\n");
		my $cols	= length($word) - $lastnl - 1;
		$self->{lines}	+= $lines;
		if ($lines) {
			$self->{column}	= $cols;
		} else {
			$self->{column}	+= $cols;
		}
		substr($self->{buffer}, 0, length($word), '');
	}

	sub _read_length {
		my $self	= shift;
		my $len		= shift;
		while (length($self->{buffer}) < $len) {
			$self->fill_buffer;
		}
	
		my $word	= substr($self->{buffer}, 0, $len, '');
		my $lines	= ($word =~ tr/\n//);
		my $lastnl	= rindex($word, "\n");
		my $cols	= length($word) - $lastnl - 1;
		$self->{lines}	+= $lines;
		if ($lines) {
			$self->{column}	= $cols;
		} else {
			$self->{column}	+= $cols;
		}
		return $word;
	}

	sub _get_pname {
		my $self	= shift;
		my $prefix	= '';
	
		if ($self->{buffer} =~ /^$r_PNAME_LN/o) {
			my $ln	= $self->_read_length($+[0]);
			my ($ns, $local)	= ($ln =~ /^([^:]*:)(.*)$/);
			no warnings 'uninitialized';
			$local	=~ s{\\([-~.!&'()*+,;=:/?#@%_\$])}{$1}g;
			return $self->new_token(PREFIXNAME, $self->{start_line}, $self->{start_column}, $ns, $local);
		} else {
			$self->{buffer} =~ $r_PNAME_NS;
			my $ns	= $self->_read_length($+[0]);
			return $self->new_token(PREFIXNAME, $self->{start_line}, $self->{start_column}, $ns);
		}
	}

	sub _get_iriref {
		my $self	= shift;
		$self->_get_char_safe(q[<]);
		if ($self->{buffer} =~ m/^[\x23-\x3d\x3f-\x5a\x5d-\x7e]*>/o) {
			my $iri	.= $self->_read_length($+[0]);
			chop($iri);
			return $self->new_token(IRI, $self->{start_line}, $self->{start_column}, $iri);
		}
	
		my $iri	= '';
		while (1) {
			if (length($self->{buffer}) == 0) {
				my $c	= $self->_peek_char;
				last unless defined($c);
			}
			if (substr($self->{buffer}, 0, 1) eq '\\') {
				$self->_get_char_safe('\\');
				my $esc	= $self->_get_char;
				if ($esc eq '\\') {
					$iri	.= "\\";
				} elsif ($esc eq 'U') {
					my $codepoint	= $self->_read_length(8);
					$self->_throw_error("Bad unicode escape codepoint '$codepoint'") unless ($codepoint =~ /^[0-9A-Fa-f]+$/o);
					$iri .= chr(hex($codepoint));
				} elsif ($esc eq 'u') {
					my $codepoint	= $self->_read_length(4);
					$self->_throw_error("Bad unicode escape codepoint '$codepoint'") unless ($codepoint =~ /^[0-9A-Fa-f]+$/o);
					my $char	= chr(hex($codepoint));
					if ($char =~ /[<>" {}|\\^`]/o) {
						$self->_throw_error(sprintf("Bad IRI character: '%s' (0x%x)", $char, ord($char)));
					}
					$iri .= $char;
				} else {
					$self->_throw_error("Unrecognized iri escape '$esc'");
				}
			} elsif ($self->{buffer} =~ /^[^<>\x00-\x20\\"{}|^`]+/o) {
				$iri	.= $self->_read_length($+[0]);
			} elsif (substr($self->{buffer}, 0, 1) eq '>') {
				last;
			} else {
				my $c	= $self->_peek_char;
				$self->_throw_error("Got '$c' while expecting IRI character");
			}
		}
		$self->_get_char_safe(q[>]);
		return $self->new_token(IRI, $self->{start_line}, $self->{start_column}, $iri);
	}

	sub _get_bnode {
		my $self	= shift;
		$self->_read_word('_:');
		$self->_throw_error("Expected: name") unless ($self->{buffer} =~ /^${r_bnode_id}/o);
		my $name	= substr($self->{buffer}, 0, $+[0]);
		$self->_read_word($name);
		return $self->new_token(BNODE, $self->{start_line}, $self->{start_column}, $name);
	}

	sub _get_number {
		my $self	= shift;
		if ($self->{buffer} =~ /^${r_double}/o) {
			return $self->new_token(DOUBLE, $self->{start_line}, $self->{start_column}, $self->_read_length($+[0]));
		} elsif ($self->{buffer} =~ /^${r_decimal}/o) {
			return $self->new_token(DECIMAL, $self->{start_line}, $self->{start_column}, $self->_read_length($+[0]));
		} elsif ($self->{buffer} =~ /^${r_integer}/o) {
			return $self->new_token(INTEGER, $self->{start_line}, $self->{start_column}, $self->_read_length($+[0]));
		}
		$self->_throw_error("Expected number");
	}

	sub _get_comment {
		my $self	= shift;
		$self->_get_char_safe('#');
		my $comment	= '';
		my $c		= $self->_peek_char;
		while (length($c) and $c !~ /[\r\n]/o) {
			$comment	.= $self->_get_char;
			$c			= $self->_peek_char;
		}
		if (length($c) and $c =~ /[\r\n]/o) {
			$self->_get_char;
		}
		return $self->new_token(COMMENT, $self->{start_line}, $self->{start_column}, $comment);
	}

	sub _get_double_literal {
		my $self	= shift;
	# 	my $c		= $self->_peek_char();
		$self->_get_char_safe(q["]);
		if (substr($self->{buffer}, 0, 2) eq q[""]) {
			# #x22 #x22 #x22 lcharacter* #x22 #x22 #x22
			$self->_read_word(q[""]);
		
			my $quote_count	= 0;
			my $string	= '';
			while (1) {
				if (length($self->{buffer}) == 0) {
					$self->fill_buffer;
					$self->_throw_error("Found EOF in string literal") if (length($self->{buffer}) == 0);
				}
				if (substr($self->{buffer}, 0, 1) eq '"') {
					my $c	= $self->_get_char;
					$quote_count++;
					last if ($quote_count == 3);
				} else {
					if ($quote_count) {
						$string	.= '"' foreach (1..$quote_count);
						$quote_count	= 0;
					}
					if (substr($self->{buffer}, 0, 1) eq '\\') {
						$string	.= $self->_get_escaped_char();
					} else {
						$self->{buffer}	=~ /^[^"\\]+/;
						$string	.= $self->_read_length($+[0]);
					}
				}
			}
			return $self->new_token(STRING3D, $self->{start_line}, $self->{start_column}, $string);
		} else {
			### #x22 scharacter* #x22
			my $string	= '';
			while (1) {
				if (substr($self->{buffer}, 0, 1) eq '\\') {
					$string	.= $self->_get_escaped_char();
				} elsif ($self->{buffer} =~ /^[^"\\]+/o) {
					$string	.= $self->_read_length($+[0]);
				} elsif (substr($self->{buffer}, 0, 1) eq '"') {
					last;
				} else {
					my $c	= $self->_peek_char;
					$self->_throw_error("Got '$c' while expecting string character");
				}
			}
			$self->_get_char_safe(q["]);
			return $self->new_token(STRING1D, $self->{start_line}, $self->{start_column}, $string);
		}
	}

	sub _get_single_literal {
		my $self	= shift;
		$self->_get_char_safe("'");
		if (substr($self->{buffer}, 0, 2) eq q['']) {
			# #x22 #x22 #x22 lcharacter* #x22 #x22 #x22
			$self->_read_word(q['']);
		
			my $quote_count	= 0;
			my $string	= '';
			while (1) {
				if (length($self->{buffer}) == 0) {
					$self->fill_buffer;
					$self->_throw_error("Found EOF in string literal") if (length($self->{buffer}) == 0);
				}
				if (substr($self->{buffer}, 0, 1) eq "'") {
					my $c	= $self->_get_char;
					$quote_count++;
					last if ($quote_count == 3);
				} else {
					if ($quote_count) {
						$string	.= "'" foreach (1..$quote_count);
						$quote_count	= 0;
					}
					if (substr($self->{buffer}, 0, 1) eq '\\') {
						$string	.= $self->_get_escaped_char();
					} else {
						$self->{buffer}	=~ /^[^'\\]+/;
						$string	.= $self->_read_length($+[0]);
					}
				}
			}
			return $self->new_token(STRING3S, $self->{start_line}, $self->{start_column}, $string);
		} else {
			### #x22 scharacter* #x22
			my $string	= '';
			while (1) {
				if (substr($self->{buffer}, 0, 1) eq '\\') {
					$string	.= $self->_get_escaped_char();
				} elsif ($self->{buffer} =~ /^[^'\\]+/o) {
					$string	.= $self->_read_length($+[0]);
				} elsif (substr($self->{buffer}, 0, 1) eq "'") {
					last;
				} else {
					my $c		= $self->_peek_char();
					$self->_throw_error("Got '$c' while expecting string character");
				}
			}
			$self->_get_char_safe(q[']);
			return $self->new_token(STRING1S, $self->{start_line}, $self->{start_column}, $string);
		}
	}

	sub _get_escaped_char {
		my $self	= shift;
		my $c	= $self->_peek_char;
		$self->_get_char_safe('\\');
		my $esc	= $self->_get_char;
		if ($esc eq '\\') { return "\\" }
		elsif ($esc =~ /^['">]$/) { return $esc }
		elsif ($esc eq 'r') { return "\r" }
		elsif ($esc eq 't') { return "\t" }
		elsif ($esc eq 'n') { return "\n" }
		elsif ($esc eq 'b') { return "\b" }
		elsif ($esc eq 'f') { return "\f" }
		elsif ($esc eq 'U') {
			my $codepoint	= $self->_read_length(8);
			$self->_throw_error("Bad unicode escape codepoint '$codepoint'") unless ($codepoint =~ /^[0-9A-Fa-f]+$/o);
			return chr(hex($codepoint));
		} elsif ($esc eq 'u'){
			my $codepoint	= $self->_read_length(4);
			$self->_throw_error("Bad unicode escape codepoint '$codepoint'") unless ($codepoint =~ /^[0-9A-Fa-f]+$/o);
			return chr(hex($codepoint));
		}
		$self->_throw_error("Unrecognized string escape '$esc'");
	}
	
	sub _get_keyword {
		my $self	= shift;
		$self->_get_char_safe('@');
		if ($self->{buffer} =~ /^base/o) {
			$self->_read_word('base');
			return $self->new_token(BASE, $self->{start_line}, $self->{start_column});
		} elsif ($self->{buffer} =~ /^prefix/o) {
			$self->_read_word('prefix');
			return $self->new_token(PREFIX, $self->{start_line}, $self->{start_column});
		} else {
			if ($self->{buffer} =~ /^[a-zA-Z]+(-[a-zA-Z0-9]+)*\b/o) {
				my $lang	= $self->_read_length($+[0]);
				return $self->new_token(LANG, $self->{start_line}, $self->{start_column}, $lang);
			}
			$self->_throw_error("Expected keyword or language tag");
		}
	}

	sub _throw_error {
		my $self	= shift;
		my $error	= shift;
		my $line	= $self->line;
		my $col		= $self->column;
		Carp::confess "$error at $line:$col with buffer: " . Dumper($self->{buffer});
	}
}

1;

__END__

=end private

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/perlrdf/issues>.

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2006-2012 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
