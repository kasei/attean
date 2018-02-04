use v5.14;
use warnings;

=head1 NAME

AtteanX::Parser::SPARQLLex - SPARQL Lexer

=head1 VERSION

This document describes AtteanX::Parser::SPARQLLex version 0.019

=head1 SYNOPSIS

 use Attean;

=head1 DESCRIPTION

...

=head1 ATTRIBUTES

=over 4

=item C<< canonical_media_type >>

=item C<< media_types >>

=item C<< file_extensions >>

=item C<< handled_type >>

=item C<< extend >>

=back

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::SPARQLLex 0.019 {
	use utf8;
	use Moo;
	use Attean;
	use Encode;
	use Encode qw(decode);
	use List::MoreUtils qw(zip);
	use Types::Standard qw(ArrayRef);
	use namespace::clean;

	sub canonical_media_type { return "application/x-sparql-query-tokens" }

	# these pass through to the lexer iterator
	has extend		=> ( is => 'ro', isa => ArrayRef, default => sub { [] } );

	sub media_types {
		return [qw(application/x-sparql-query-tokens)];
	}
	
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'AtteanX::SPARQL::Token');
		return $ITEM_TYPE;
	}

=item C<< file_extensions >>

Returns a list of file extensions that may be parsed with the parser.

=cut

	sub file_extensions { return [qw(rq)] }

	with 'Attean::API::PullParser', 'Attean::API::Parser';

=item C<< parse_iter_from_bytes( $data ) >>

Returns an iterator of SPARQL tokens that result from parsing
the SPARQL query/update read from the UTF-8 encoded byte string C<< $data >>.

=cut

	sub parse_iter_from_bytes {
		my $self	= shift;
		my $data	= shift;
		open(my $fh, '<:encoding(UTF-8)', \$data);
		return $self->parse_iter_from_io($fh);
	}

=item C<< parse_iter_from_io( $fh ) >>

Returns an iterator of SPARQL tokens that result from parsing
the SPARQL query/update read from the L<IO::Handle> object C<< $fh >>.

=cut

	sub parse_iter_from_io {
		my $self	= shift;
		my $fh		= shift;
		return AtteanX::Parser::SPARQLLex::Iterator->new(
			extend => $self->extend,
			file => $fh,
		);
	}
}

package AtteanX::Parser::SPARQLLex::Iterator 0.019 {
	use utf8;
	use Moo;
	use Attean;
	use Encode;
	use Encode qw(decode);
	use List::MoreUtils qw(zip);
	use AtteanX::SPARQL::Token;
	use AtteanX::SPARQL::Constants;
	use Types::Standard qw(FileHandle Ref Str Int ArrayRef HashRef ConsumerOf InstanceOf);
	use namespace::clean;
	
	has lookahead_methods		=> ( is => 'ro', isa => HashRef, default => sub { +{} } );
	has lookahead_tokens		=> ( is => 'ro', isa => HashRef, default => sub { +{} } );
	has extend					=> ( is => 'ro', isa => ArrayRef, default => sub { [] } );
	has token_buffer			=> ( is => 'ro', isa => ArrayRef, default => sub { [] } );
	
	with 'AtteanX::API::Lexer';
	
	my $r_ECHAR					= qr/\\([tbnrf\\"'])/o;
	my $r_STRING_LITERAL1		= qr/'(([^\x{27}\x{5C}\x{0A}\x{0D}])|${r_ECHAR})*'/o;
	my $r_STRING_LITERAL2		= qr/"(([^\x{22}\x{5C}\x{0A}\x{0D}])|${r_ECHAR})*"/o;
	my $r_STRING_LITERAL_LONG1	= qr/'''(('|'')?([^'\\]|${r_ECHAR}))*'''/o;
	my $r_STRING_LITERAL_LONG2	= qr/"""(("|"")?([^"\\]|${r_ECHAR}))*"""/o;
	my $r_LANGTAG				= qr/@[a-zA-Z]+(-[a-zA-Z0-9]+)*/o;
	my $r_IRI_REF				= qr/<([^<>"{}|^`\\\x{00}-\x{20}])*>/o;
	my $r_PN_CHARS_BASE			= qr/([A-Z]|[a-z]|[\x{00C0}-\x{00D6}]|[\x{00D8}-\x{00F6}]|[\x{00F8}-\x{02FF}]|[\x{0370}-\x{037D}]|[\x{037F}-\x{1FFF}]|[\x{200C}-\x{200D}]|[\x{2070}-\x{218F}]|[\x{2C00}-\x{2FEF}]|[\x{3001}-\x{D7FF}]|[\x{F900}-\x{FDCF}]|[\x{FDF0}-\x{FFFD}]|[\x{10000}-\x{EFFFF}])/o;
	my $r_PN_CHARS_U			= qr/([_]|${r_PN_CHARS_BASE})/o;
	my $r_VARNAME				= qr/((${r_PN_CHARS_U}|[0-9])(${r_PN_CHARS_U}|[0-9]|\x{00B7}|[\x{0300}-\x{036F}]|[\x{203F}-\x{2040}])*)/o;
	my $r_VAR1					= qr/[?]${r_VARNAME}/o;
	my $r_VAR2					= qr/[\$]${r_VARNAME}/o;
	my $r_PN_CHARS				= qr/${r_PN_CHARS_U}|-|[0-9]|\x{00B7}|[\x{0300}-\x{036F}]|[\x{203F}-\x{2040}]/o;
	my $r_PN_PREFIX				= qr/(${r_PN_CHARS_BASE}((${r_PN_CHARS}|[.])*${r_PN_CHARS})?)/o;
	my $r_PN_LOCAL_ESCAPED		= qr{(\\([-~.!&'()*+,;=/?#@%_\$]))|%[0-9A-Fa-f]{2}}o;
	my $r_PN_LOCAL				= qr/((${r_PN_CHARS_U}|[:0-9]|${r_PN_LOCAL_ESCAPED})((${r_PN_CHARS}|${r_PN_LOCAL_ESCAPED}|[:.])*(${r_PN_CHARS}|[:]|${r_PN_LOCAL_ESCAPED}))?)/o;
	my $r_PN_LOCAL_BNODE		= qr/((${r_PN_CHARS_U}|[0-9])((${r_PN_CHARS}|[.])*${r_PN_CHARS})?)/o;
	my $r_PNAME_NS				= qr/((${r_PN_PREFIX})?:)/o;
	my $r_PNAME_LN				= qr/(${r_PNAME_NS}${r_PN_LOCAL})/o;
	my $r_EXPONENT				= qr/[eE][-+]?\d+/o;
	my $r_DOUBLE				= qr/\d+[.]\d*${r_EXPONENT}|[.]\d+${r_EXPONENT}|\d+${r_EXPONENT}/o;
	my $r_DECIMAL				= qr/(\d+[.]\d*)|([.]\d+)/o;
	my $r_INTEGER				= qr/\d+/o;
	my $r_BLANK_NODE_LABEL		= qr/_:${r_PN_LOCAL_BNODE}/o;
	my $r_ANON					= qr/\[[\t\r\n ]*\]/o;
	my $r_NIL					= qr/\([\n\r\t ]*\)/o;
	my $r_KEYWORDS				= qr/(ABS|ADD|ALL|ASC|ASK|AS|AVG|BASE|BIND|BNODE|BOUND|BY|CEIL|CLEAR|COALESCE|CONCAT|CONSTRUCT|CONTAINS|COPY|COUNT|CREATE|DATATYPE|DAY|DEFAULT|DELETE|DELETE WHERE|DESCRIBE|DESC|DISTINCT|DISTINCT|DROP|ENCODE_FOR_URI|EXISTS|FILTER|FLOOR|FROM|GRAPH|GROUP_CONCAT|GROUP|HAVING|HOURS|IF|INSERT|INSERT|DATA|INTO|IN|IRI|ISBLANK|ISIRI|ISLITERAL|ISNUMERIC|ISURI|LANGMATCHES|LANG|LCASE|LIMIT|LOAD|MAX|MD5|MINUS|MINUTES|MIN|MONTH|MOVE|NAMED|NOT|NOW|OFFSET|OPTIONAL|ORDER|PREFIX|RAND|REDUCED|REGEX|REPLACE|ROUND|SAMETERM|SAMPLE|SECONDS|SELECT|SEPARATOR|SERVICE|SHA1|SHA256|SHA384|SHA512|SILENT|STRAFTER|STRBEFORE|STRDT|STRENDS|STRLANG|STRLEN|STRSTARTS|STRUUID|STR|SUBSTR|SUM|TIMEZONE|TO|TZ|UCASE|UNDEF|UNION|URI|USING|UUID|VALUES|WHERE|WITH|YEAR)(?!:)\b/io;

	sub BUILD {
		my $self	= shift;
		my %METHOD_TOKEN	= (
		# 	q[#]	=> '_get_comment',
			q[@]	=> '_get_lang',
			q[<]	=> '_get_iriref_or_relational',
			q[_]	=> '_get_bnode',
			q[']	=> '_get_single_literal',
			q["]	=> '_get_double_literal',
			q[:]	=> '_get_pname',
			q[?]	=> '_get_variable',
			q[!]	=> '_get_bang',
			q[>]	=> '_get_iriref_or_relational',
			q([)	=> '_get_lbracket_or_anon',
			q[(]	=> '_get_lparen_or_nil',
			(map {$_ => '_get_number'} (0 .. 9, '-', '+'))
		);
		while (my ($k,$v) = each(%METHOD_TOKEN)) {
			if (length($k) != 1) {
				die "Cannot set a lookahead token handler method with lookahead > 1 char";
			}
			$self->lookahead_methods->{$k}	//= $v;
		}

		my %CHAR_TOKEN	= (
			','	=> COMMA,
			'.'	=> DOT,
			'='	=> EQUALS,
			'{'	=> LBRACE,
			'}'	=> RBRACE,
			']'	=> RBRACKET,
			')'	=> RPAREN,
			'-'	=> MINUS,
			'+'	=> PLUS,
			';'	=> SEMICOLON,
			'/'	=> SLASH,
			'*'	=> STAR,
		);
		while (my ($k,$v) = each(%CHAR_TOKEN)) {
			if (length($k) != 1) {
				die "Cannot set a lookahead token with lookahead > 1 char";
			}
			$self->lookahead_tokens->{$k}	//= $v;
		}
		
		$self->add_regex_rule( $r_KEYWORDS, KEYWORD, sub { return uc(shift) } );
	}
	
	sub peek {
		my $self	= shift;
		my $b		= $self->token_buffer;
		my $t		= $self->next;
		return unless ($t);
		push(@$b, $t);
		return $t;
	}
	
	sub next {
		my $self	= shift;
		my $b		= $self->token_buffer;
		if (scalar(@$b)) {
			return shift(@$b);
		} else {
			return $self->get_token();
		}
	}
	
	sub fill_buffer {
		my $self	= shift;
		unless (length($self->buffer)) {
			my $line	= $self->file->getline;
			if (defined($line)) {
				$line		=~ s/\\u([0-9A-Fa-f]{4})/chr(hex($1))/ge;
				$line		=~ s/\\U([0-9A-Fa-f]{8})/chr(hex($1))/ge;
				$self->{buffer}	.= $line;
			}
		}
	}
	
	sub new_token {
		my $self		= shift;
		my $type		= shift;
		my $start_line	= shift;
		my $start_col	= shift;
		my $line		= $self->line;
		my $col			= $self->column;
		return AtteanX::SPARQL::Token->fast_constructor( $type, $start_line, $start_col, $line, $col, \@_ );
	}

	sub add_regex_rule {
		my $self	= shift;
		my $r		= shift;
		my $ttype	= shift;
		my $convert	= shift;
		my $extend	= $self->extend;
		push(@$extend, sub {
			my $l	= shift;
			
			if ($l->buffer =~ /^$r\b/) {
				my $value	= $self->read_length($+[0]);
				my $c		= $convert ? $convert->($value) : $value;
				return $l->new_token($ttype, $l->start_line, $l->start_column, $c);
			}
		});
	}
	
	sub get_token {
		my $self	= shift;
		while (1) {
			$self->fill_buffer unless (length($self->buffer));

			if ($self->buffer =~ /^[ \r\n\t]+/o) {
				$self->read_length($+[0]);
				# we're ignoring whitespace tokens, but we could return them here instead of falling through to the 'next':
	# 			return $self->new_token(WS);
				next;
			}

			my $c	= $self->peek_char();
			return unless (defined($c));

			if ($c eq '#') {
				# we're ignoring comment tokens, but we could return them here instead of falling through to the 'next':
				$self->_get_comment();
				next;
			}
		
			my $start_column	= $self->column;
			my $start_line		= $self->line;
		
			$self->start_column( $start_column );
			$self->start_line( $start_line );
		
			foreach my $e (@{ $self->extend }) {
				if (my $t = $e->( $self )) {
					return $t;
				}
			}
			
			if ($c eq '.' and $self->buffer =~ /^$r_DECIMAL/) {
				return $self->_get_number();
			}
			
			if (defined(my $name = $self->lookahead_tokens->{$c})) { $self->get_char; return $self->new_token($name, $start_line, $start_column, $c); }
			elsif (defined(my $method = $self->lookahead_methods->{$c})) { return $self->$method() }
			elsif ($c =~ /[A-Za-z\x{00C0}-\x{00D6}\x{00D8}-\x{00F6}\x{00F8}-\x{02FF}\x{0370}-\x{037D}\x{037F}-\x{1FFF}\x{200C}-\x{200D}\x{2070}-\x{218F}\x{2C00}-\x{2FEF}\x{3001}-\x{D7FF}\x{F900}-\x{FDCF}\x{FDF0}-\x{FFFD}\x{10000}-\x{EFFFF}]/o) {
				if ($self->buffer =~ /^a(?!:)\s/o) {
					$self->get_char;
					return $self->new_token(A, $start_line, $start_column, 'a');
				} elsif ($self->buffer =~ /^(?:true|false)(?!:)\b/o) {
					my $bool	= $self->read_length($+[0]);
					return $self->new_token(BOOLEAN, $start_line, $start_column, $bool);
# 				} elsif ($self->buffer =~ /^$r_KEYWORDS/) {
# 					my $bool	= $self->read_length($+[0]);
# 					return $self->new_token(KEYWORD, $start_line, $start_column, $bool);
				} elsif ($self->buffer =~ /^BASE(?!:)\b/oi) {
					$self->read_length(4);
					return $self->new_token(BASE, $start_line, $start_column, 'BASE');
				} elsif ($self->buffer =~ /^PREFIX(?!:)\b/io) {
					$self->read_length(6);
					return $self->new_token(PREFIX, $start_line, $start_column, 'PREFIX');
				} else {
					return $self->_get_pname;
				}
			} elsif ($c eq '^') {
				if ($self->buffer =~ /^\^\^/) {
					$self->read_word('^^');
					return $self->new_token(HATHAT, $start_line, $start_column, '^^');
				} else {
					$self->read_word('^');
					return $self->new_token(HAT, $start_line, $start_column, '^');
				}
			} elsif ($c eq '|') {
				if ($self->buffer =~ /^\|\|/) {
					$self->read_word('||');
					return $self->new_token(OROR, $start_line, $start_column, '||');
				} else {
					$self->read_word('|');
					return $self->new_token(OR, $start_line, $start_column, '|');
				}
			} elsif ($c eq '&') {
				$self->read_word('&&');
				return $self->new_token(ANDAND, $start_line, $start_column, '&&');
			} else {
	# 			Carp::cluck sprintf("Unexpected byte '$c' (0x%02x)", ord($c));
				return $self->_throw_error(sprintf("Unexpected byte '%s' (0x%02x)", $c, ord($c)));
			}
			warn sprintf('byte: 0x%x', ord($c));
		}
	}

	sub _get_pname {
		my $self	= shift;
		my $prefix	= '';
	
		if ($self->buffer =~ /^$r_PNAME_LN/o) {
			my $ln	= $self->read_length($+[0]);
			my ($ns, $local)	= ($ln =~ /^([^:]*:)(.*)$/);
			no warnings 'uninitialized';
			$local	=~ s{\\([-~.!&'()*+,;=:/?#@%_\$])}{$1}g;
			return $self->new_token(PREFIXNAME, $self->start_line, $self->start_column, $ns, $local);
		} elsif ($self->buffer =~ $r_PNAME_NS) {
			my $ns	= $self->read_length($+[0]);
			return $self->new_token(PREFIXNAME, $self->start_line, $self->start_column, $ns);
		} else {
			$self->_throw_error("Expected PNAME");
		}
	}

	sub _get_variable {
		my $self	= shift;
		$self->get_char_safe('?');
		if ($self->buffer =~ /^$r_VARNAME/) {
			my $name	= $self->read_length($+[0]);
			return $self->new_token(VAR, $self->start_line, $self->start_column, $name);
		} else {
			return $self->new_token(QUESTION, $self->start_line, $self->start_column, '?');
		}
	}
	
	sub _get_iriref_or_relational {
		my $self	= shift;
		my $buffer	= $self->buffer;
		if ($buffer =~ m/^<([^<>"{}|^`\x00-\x20])*>/) {
			$self->get_char_safe(q[<]);
			if ($self->buffer =~ m/^[\x23-\x3d\x3f-\x5a\x5d-\x7e]*>/o) {
				my $iri	.= $self->read_length($+[0]);
				chop($iri);
				return $self->new_token(IRI, $self->start_line, $self->start_column, $iri);
			}
	
			my $iri	= '';
			while (1) {
				if (length($self->buffer) == 0) {
					my $c	= $self->peek_char;
					last unless defined($c);
				}
				if (substr($self->buffer, 0, 1) eq '\\') {
					$self->get_char_safe('\\');
					my $esc	= $self->get_char;
					if ($esc eq '\\') {
						$iri	.= "\\";
					} elsif ($esc eq 'U') {
						my $codepoint	= $self->read_length(8);
						$self->_throw_error("Bad unicode escape codepoint '$codepoint'") unless ($codepoint =~ /^[0-9A-Fa-f]+$/o);
						$iri .= chr(hex($codepoint));
					} elsif ($esc eq 'u') {
						my $codepoint	= $self->read_length(4);
						$self->_throw_error("Bad unicode escape codepoint '$codepoint'") unless ($codepoint =~ /^[0-9A-Fa-f]+$/o);
						my $char	= chr(hex($codepoint));
						if ($char =~ /[<>" {}|\\^`]/o) {
							$self->_throw_error(sprintf("Bad IRI character: '%s' (0x%x)", $char, ord($char)));
						}
						$iri .= $char;
					} else {
						$self->_throw_error("Unrecognized iri escape '$esc'");
					}
				} elsif ($self->buffer =~ /^[^<>\x00-\x20\\"{}|^`]+/o) {
					$iri	.= $self->read_length($+[0]);
				} elsif (substr($self->buffer, 0, 1) eq '>') {
					last;
				} else {
					my $c	= $self->peek_char;
					$self->_throw_error("Got '$c' while expecting IRI character");
				}
			}
			$self->get_char_safe(q[>]);
			return $self->new_token(IRI, $self->start_line, $self->start_column, $iri);
		} elsif (substr($buffer, 0, 2) eq '<=') {
			$self->read_length(2);
			return $self->new_token(LE, $self->start_line, $self->start_column, '<=');
		} elsif (substr($buffer, 0, 2) eq '>=') {
			$self->read_length(2);
			return $self->new_token(GE, $self->start_line, $self->start_column, '>=');
		} elsif (substr($buffer, 0, 1) eq '>') {
			$self->get_char;
			return $self->new_token(GT, $self->start_line, $self->start_column, '>');
		} elsif (substr($buffer, 0, 1) eq '<') {
			$self->get_char;
			return $self->new_token(LT, $self->start_line, $self->start_column, '<');
		} else {
			die "Unrecognized relational op near '$buffer'";
		}
	}

	sub _get_bang {
		my $self	= shift;
		if ($self->buffer =~ /^!=/) {
			$self->read_length(2);
			return $self->new_token(NOTEQUALS, $self->start_line, $self->start_column, '!=');
		} else {
			$self->get_char;
			return $self->new_token(BANG, $self->start_line, $self->start_column, '!');
		}
	}
	
	sub _get_bnode {
		my $self	= shift;
		unless ($self->buffer =~ /^$r_BLANK_NODE_LABEL/o) {
			$self->_throw_error("Expected: name");
		}
		my $ln	= $self->read_length($+[0]);
		my $name	= substr($ln, 2);
		return $self->new_token(BNODE, $self->start_line, $self->start_column, $name);
	}

	sub _get_number {
		my $self	= shift;
		if ($self->buffer =~ /^${r_DOUBLE}/o) {
			return $self->new_token(DOUBLE, $self->start_line, $self->start_column, $self->read_length($+[0]));
		} elsif ($self->buffer =~ /^${r_DECIMAL}/o) {
			return $self->new_token(DECIMAL, $self->start_line, $self->start_column, $self->read_length($+[0]));
		} elsif ($self->buffer =~ /^${r_INTEGER}/o) {
			return $self->new_token(INTEGER, $self->start_line, $self->start_column, $self->read_length($+[0]));
		}
		$self->_throw_error("Expected number");
	}

	sub _get_lparen_or_nil {
		my $self	= shift;
		if ($self->buffer =~ /^$r_NIL/) {
			$self->read_length($+[0]);
			return $self->new_token(NIL, $self->start_line, $self->start_column, '()');
		} else {
			$self->get_char_safe('(');
			return $self->new_token(LPAREN, $self->start_line, $self->start_column, '(');
		}
	}
	
	sub _get_lbracket_or_anon {
		my $self	= shift;
		if ($self->buffer =~ /^$r_ANON/) {
			$self->read_length($+[0]);
			return $self->new_token(ANON, $self->start_line, $self->start_column, '[]');
		} else {
			$self->get_char_safe('[');
			return $self->new_token(LBRACKET, $self->start_line, $self->start_column, '[');
		}
	}
	
	sub _get_comment {
		my $self	= shift;
		$self->get_char_safe('#');
		my $comment	= '';
		my $c		= $self->peek_char;
		while (length($c) and $c !~ /[\r\n]/o) {
			$comment	.= $self->get_char;
			$c			= $self->peek_char;
		}
		if (length($c) and $c =~ /[\r\n]/o) {
			$self->get_char;
		}
		return $self->new_token(COMMENT, $self->start_line, $self->start_column, $comment);
	}

	sub _get_lang {
		my $self	= shift;
		$self->get_char_safe('@');
		if ($self->buffer =~ /^[a-zA-Z]+(-[a-zA-Z0-9]+)*\b/o) {
			my $lang	= $self->read_length($+[0]);
			return $self->new_token(LANG, $self->start_line, $self->start_column, $lang);
		}
		$self->_throw_error("Expected keyword or language tag");
	}
	
	sub _get_double_literal {
		my $self	= shift;
	# 	my $c		= $self->peek_char();
		$self->get_char_safe(q["]);
		if (substr($self->buffer, 0, 2) eq q[""]) {
			# #x22 #x22 #x22 lcharacter* #x22 #x22 #x22
			$self->read_word(q[""]);
		
			my $quote_count	= 0;
			my $string	= '';
			while (1) {
				if (length($self->buffer) == 0) {
					$self->fill_buffer;
					$self->_throw_error("Found EOF in string literal") if (length($self->buffer) == 0);
				}
				if (substr($self->buffer, 0, 1) eq '"') {
					my $c	= $self->get_char;
					$quote_count++;
					last if ($quote_count == 3);
				} else {
					if ($quote_count) {
						$string	.= '"' foreach (1..$quote_count);
						$quote_count	= 0;
					}
					if (substr($self->buffer, 0, 1) eq '\\') {
						$string	.= $self->_get_escaped_char();
					} else {
						$self->buffer	=~ /^[^"\\]+/;
						$string	.= $self->read_length($+[0]);
					}
				}
			}
			return $self->new_token(STRING3D, $self->start_line, $self->start_column, $string);
		} else {
			### #x22 scharacter* #x22
			my $string	= '';
			while (1) {
				if (substr($self->buffer, 0, 1) eq '\\') {
					$string	.= $self->_get_escaped_char();
				} elsif ($self->buffer =~ /^[^"\\]+/o) {
					$string	.= $self->read_length($+[0]);
				} elsif (substr($self->buffer, 0, 1) eq '"') {
					last;
				} else {
					my $c	= $self->peek_char;
					$self->_throw_error("Got '$c' while expecting string character");
				}
			}
			$self->get_char_safe(q["]);
			return $self->new_token(STRING1D, $self->start_line, $self->start_column, $string);
		}
	}

	sub _get_single_literal {
		my $self	= shift;
		$self->get_char_safe("'");
		if (substr($self->buffer, 0, 2) eq q['']) {
			# #x22 #x22 #x22 lcharacter* #x22 #x22 #x22
			$self->read_word(q['']);
		
			my $quote_count	= 0;
			my $string	= '';
			while (1) {
				if (length($self->buffer) == 0) {
					$self->fill_buffer;
					$self->_throw_error("Found EOF in string literal") if (length($self->buffer) == 0);
				}
				if (substr($self->buffer, 0, 1) eq "'") {
					my $c	= $self->get_char;
					$quote_count++;
					last if ($quote_count == 3);
				} else {
					if ($quote_count) {
						$string	.= "'" foreach (1..$quote_count);
						$quote_count	= 0;
					}
					if (substr($self->buffer, 0, 1) eq '\\') {
						$string	.= $self->_get_escaped_char();
					} else {
						$self->buffer	=~ /^[^'\\]+/;
						$string	.= $self->read_length($+[0]);
					}
				}
			}
			return $self->new_token(STRING3S, $self->start_line, $self->start_column, $string);
		} else {
			### #x22 scharacter* #x22
			my $string	= '';
			while (1) {
				if (substr($self->buffer, 0, 1) eq '\\') {
					$string	.= $self->_get_escaped_char();
				} elsif ($self->buffer =~ /^[^'\\]+/o) {
					$string	.= $self->read_length($+[0]);
				} elsif (substr($self->buffer, 0, 1) eq "'") {
					last;
				} else {
					my $c		= $self->peek_char();
					$self->_throw_error("Got '$c' while expecting string character");
				}
			}
			$self->get_char_safe(q[']);
			return $self->new_token(STRING1S, $self->start_line, $self->start_column, $string);
		}
	}

	sub _get_escaped_char {
		my $self	= shift;
		my $c	= $self->peek_char;
		$self->get_char_safe('\\');
		my $esc	= $self->get_char;
		if ($esc eq '\\') { return "\\" }
		elsif ($esc =~ /^['">]$/) { return $esc }
		elsif ($esc eq 'r') { return "\r" }
		elsif ($esc eq 't') { return "\t" }
		elsif ($esc eq 'n') { return "\n" }
		elsif ($esc eq 'b') { return "\b" }
		elsif ($esc eq 'f') { return "\f" }
		elsif ($esc eq 'U') {
			my $codepoint	= $self->read_length(8);
			$self->_throw_error("Bad unicode escape codepoint '$codepoint'") unless ($codepoint =~ /^[0-9A-Fa-f]+$/o);
			return chr(hex($codepoint));
		} elsif ($esc eq 'u'){
			my $codepoint	= $self->read_length(4);
			$self->_throw_error("Bad unicode escape codepoint '$codepoint'") unless ($codepoint =~ /^[0-9A-Fa-f]+$/o);
			return chr(hex($codepoint));
		}
		$self->_throw_error("Unrecognized string escape '$esc'");
	}
	
	sub _throw_error {
		my $self	= shift;
		my $error	= shift;
		my $line	= $self->line;
		my $col		= $self->column;
		use Data::Dumper;
		Carp::confess "$error at $line:$col with buffer: " . Dumper($self->buffer);
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/perlrdf/issues>.

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2018 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
