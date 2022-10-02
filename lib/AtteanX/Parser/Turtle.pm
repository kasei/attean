use v5.14;
use warnings;

# AtteanX::Parser::Turtle
# -----------------------------------------------------------------------------

=head1 NAME

AtteanX::Parser::Turtle - Turtle RDF Parser

=head1 VERSION

This document describes AtteanX::Parser::Turtle version 0.033

=head1 SYNOPSIS

 use Attean;
 my $parser	= AtteanX::Parser::Turtle->new( handler => sub {...}, base => $base_iri );
 
 # Parse data from a file-handle and handle triples in the 'handler' callback
 $parser->parse_cb_from_io( $fh );
 
 # Parse the given byte-string, and return an iterator of triples
 my $iter = $parser->parse_iter_from_bytes('<s> <p> 1, 2, 3 .');
 while (my $triple = $iter->next) {
   print $triple->as_string;
 }

=head1 DESCRIPTION

This module implements a parser for the Turtle RDF format.

=head1 ROLES

This class consumes L<Attean::API::Parser>, L<Attean::API::PushParser>,
<Attean::API::AbbreviatingParser>, and <Attean::API::TripleParser>.

=head1 ATTRIBUTES

=over 4

=item C<< canonical_media_type >>

=item C<< media_types >>

=item C<< file_extensions >>

=item C<< canonicalize >>

A boolean indicating whether term values should be canonicalized during parsing.

=back

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::Turtle 0.033 {
	use Moo;
	use Types::Standard qw(Bool ArrayRef HashRef Str Maybe InstanceOf);
	use Types::Namespace qw( NamespaceMap );
	use utf8;
	use Carp qw(carp);
	use Encode qw(encode);
	use Scalar::Util qw(blessed);
	use AtteanX::Parser::Turtle::Constants;
	use AtteanX::Parser::Turtle::Lexer;
	use AtteanX::Parser::Turtle::Token;
	use Attean::API::Parser;
	use namespace::clean;

	sub canonical_media_type { return "text/turtle" }

	sub media_types {
		return [qw(application/x-turtle application/turtle text/turtle)];
	}

	sub file_extensions { return [qw(ttl)] }

	has 'canonicalize'	=> (is => 'rw', isa => Bool, default => 0);
	has '_map' => (is => 'ro', isa => HashRef[Str], default => sub { +{} });

=item C<< has_namespaces >>

Returns true if the parser has a namespace map, false otherwise.

=cut

	has 'namespaces' => (is => 'rw', isa => Maybe[NamespaceMap], predicate => 'has_namespaces');
	has	'_stack'	=> (
		is => 'ro',
		isa => ArrayRef,
		default => sub { [] },
		init_arg => undef,
	);
	
	with 'Attean::API::TripleParser';
	with 'Attean::API::AbbreviatingParser';
	with 'Attean::API::PushParser';
	
	my $RDF	= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
	my $XSD	= 'http://www.w3.org/2001/XMLSchema#';

=item C<< parse_cb_from_io( $fh ) >>

Calls the C<< $parser->handler >> function once for each
L<Attean::API::Binding> object that result from parsing
the data read from the L<IO::Handle> object C<< $fh >>.

=cut

	sub parse_cb_from_io {
		my $self	= shift;
		my $fh		= shift;

		unless (ref($fh)) {
			my $filename	= $fh;
			undef $fh;
			open( $fh, '<', $filename ) or die $!;
		}
	
		my $l	= AtteanX::Parser::Turtle::Lexer->new($fh);
		$self->_parse($l);
	}

=item C<< parse_cb_from_bytes( $data ) >>

Calls the C<< $parser->handler >> function once for each
L<Attean::API::Binding> object that result from parsing
the data read from the UTF-8 encoded byte string C<< $data >>.

=cut

	sub parse_cb_from_bytes {
		my $self	= shift;
		my $data	= shift;
	
		open(my $fh, '<:encoding(UTF-8)', \$data);
		my $l	= AtteanX::Parser::Turtle::Lexer->new($fh);
		$self->_parse($l);
	}

=item C<< parse_term_from_bytes ( $bytes ) >>

=item C<< parse_node ( $bytes ) >>

Returns the Attean::API::Term object corresponding to the node whose N-Triples
serialization is found at the beginning of C<< $bytes >>.

=cut

	sub parse_term_from_bytes {
		my $self	= shift;
		unless (ref($self)) {
			$self	= $self->new();
		}
		return $self->parse_node(@_);
	}
	
	sub parse_node {
		my $self	= shift;
		my $string	= shift;
		my %args	= @_;
	
		open(my $fh, '<:encoding(UTF-8)', \$string);
		my $l	= AtteanX::Parser::Turtle::Lexer->new(file => $fh, %args);
		my $t = $self->_next_nonws($l);
		my $node	= $self->_object($l, $t);
		return $node;
	}

	sub _parse {
		my $self	= shift;
		my $l		= shift;
		$l->check_for_bom;
		while (my $t = $self->_next_nonws($l)) {
			$self->_statement($l, $t);
		}
	}

	################################################################################

	sub _unget_token {
		my $self	= shift;
		my $t		= shift;
		push(@{ $self->_stack }, $t);
# 		push(@{ $self->{ stack } }, $t);
	}

	sub _next_nonws {
		my $self	= shift;
		if (scalar(@{ $self->_stack })) {
			return pop(@{ $self->_stack });
		}
		my $l		= shift;
		while (1) {
			my $t	= $l->get_token;
			return unless ($t);
	# 		my $type = $t->type;
	# 		next if ($type == WS or $type == COMMENT);
	# 		warn decrypt_constant($type) . "\n";
			return $t;
		}
	}

	sub _get_token_type {
		my $self	= shift;
		my $l		= shift;
		my $type	= shift;
		my $t		= $self->_next_nonws($l);
		unless ($t) {
			$l->_throw_error(sprintf("Expecting %s but got EOF", decrypt_constant($type)));
			return;
		}
		unless ($t->type eq $type) {
			$self->_throw_error(sprintf("Expecting %s but got %s", decrypt_constant($type), decrypt_constant($t->type)), $t, $l);
		}
		return $t;
	}

	sub _statement {
		my $self	= shift;
		my $l		= shift;
		my $t		= shift;
		my $type	= $t->type;
	# 		when (WS) {}
		if ($type == TURTLEPREFIX or $type == PREFIX) {
			$t	= $self->_get_token_type($l, PREFIXNAME);
			use Data::Dumper;
			unless (defined($t->value)) {
				my $tname	= AtteanX::Parser::Turtle::Constants::decrypt_constant($t->type);
				Carp::confess "undefined $tname token value: " . Dumper($t);
			}
			my $name	= $t->value;
			chop($name) if (substr($name, -1) eq ':');
# 			$name		=~ s/:$//;
			$t	= $self->_get_token_type($l, IRI);
			my %args	= (value => $t->value);
			if ($self->has_base) {
				$args{base}	= $self->base;
			}
			my $r	= $self->new_iri(%args);
			my $iri	= $r->as_string;
			if ($type == TURTLEPREFIX) {
				$t	= $self->_get_token_type($l, DOT);
	# 			$t	= $self->_next_nonws($l);
	# 			if ($t and $t->type != DOT) {
	# 				$self->_unget_token($t);
	# 			}
			}
			$self->_map->{$name}	= $iri;
			if ($self->has_namespaces) {
				my $ns	= $self->namespaces;
				unless ($ns->namespace_uri($name)) {
					$ns->add_mapping($name, $iri);
				}
			}
		}
		elsif ($type == TURTLEBASE or $type == BASE) {
			$t	= $self->_get_token_type($l, IRI);
			my %args	= (value => $t->value);
			if ($self->has_base) {
				$args{base}	= $self->base;
			}
			my $r	= $self->new_iri(%args);
			my $iri	= $r->as_string;
			if ($type == TURTLEBASE) {
				$t	= $self->_get_token_type($l, DOT);
	# 			$t	= $self->_next_nonws($l);
	# 			if ($t and $t->type != DOT) {
	# 				$self->_unget_token($t);
	# 			}
			}
			$self->base($iri);
		}
		else {
			$self->_triple( $l, $t );
			$t	= $self->_get_token_type($l, DOT);
		}
	# 	}
	}

	sub _triple {
		my $self	= shift;
		my $l		= shift;
		my $t		= shift;
		my $type	= $t->type;
		# subject
		my $subj;
		my $bnode_plist	= 0;
		if ($type == LTLT) {
			$subj	= $self->_quotedTriple($l);
		} elsif ($type == LBRACKET) {
			$bnode_plist	= 1;
			$subj	= Attean::Blank->new();
			my $t	= $self->_next_nonws($l);
			if ($t->type != RBRACKET) {
				$self->_unget_token($t);
				$self->_predicateObjectList( $l, $subj );
				$t	= $self->_get_token_type($l, RBRACKET);
			}
		} elsif ($type == LPAREN) {
			my $t	= $self->_next_nonws($l);
			if ($t->type == RPAREN) {
				$subj	= Attean::IRI->new(value => "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil", lazy => 1);
			} else {
				$subj	= Attean::Blank->new();
				my @objects	= $self->_object($l, $t);
			
				while (1) {
					my $t	= $self->_next_nonws($l);
					if ($t->type == RPAREN) {
						last;
					} else {
						push(@objects, $self->_object($l, $t));
					}
				}
				$self->_assert_list($subj, @objects);
			}
		} elsif (not($type==IRI or $type==PREFIXNAME or $type==BNODE)) {
			$self->_throw_error("Expecting resource or bnode but got " . decrypt_constant($type), $t, $l);
		} else {
			$subj	= $self->_token_to_node($t);
		}
	# 	warn "Subject: $subj\n";	# XXX
	
		if ($bnode_plist) {
			#predicateObjectList?
			$t	= $self->_next_nonws($l);
			$self->_unget_token($t);
			if ($t->type != DOT) {
				$self->_predicateObjectList($l, $subj);
			}
		} else {
			#predicateObjectList
			$self->_predicateObjectList($l, $subj);
		}
	}

	sub _quotedTriple {
		my $self	= shift;
		my $l		= shift;
		my $subj	= $self->_qtSubject($l);

		my $t		= $self->_next_nonws($l);
		my $type	= $t->type;
		unless ($type==IRI or $type==PREFIXNAME or $type==A) {
			$self->_throw_error("Expecting verb but got " . decrypt_constant($type), $t, $l);
		}
		my $pred	= $self->_token_to_node($t);
		my $obj		= $self->_qtObject($l, $self->_next_nonws($l));
		$self->_get_token_type($l, GTGT);
		my $triple	= Attean::Triple->new($subj, $pred, $obj);
		return $triple;
	}
	
	sub _qtSubject {
		my $self	= shift;
		my $l		= shift;
		my $t		= $self->_next_nonws($l);
		my $type	= $t->type;

		my $subj;
		if ($type == LTLT) {
			$subj	= $self->_quotedTriple($l);
		} elsif ($type == LBRACKET) {
			$self->_get_token_type($l, RBRACKET);
			return Attean::Blank->new();
		} elsif (not($type==IRI or $type==PREFIXNAME or $type==BNODE)) {
			$self->_throw_error("Expecting resource or bnode but got " . decrypt_constant($type), $t, $l);
		} else {
			$subj	= $self->_token_to_node($t);
		}
		return $subj;
	}

	sub _qtObject {
		my $self	= shift;
		my $l		= shift;
		my $t		= shift;
		my $tcopy	= $t;
		my $obj;
		my $type	= $t->type;
		if ($type == LTLT) {
			$obj	= $self->_quotedTriple($l);
		} elsif ($type == LBRACKET) {
			$self->_get_token_type($l, RBRACKET);
			return Attean::Blank->new();
		} elsif (not($type==IRI or $type==PREFIXNAME or $type==STRING1D or $type==STRING3D or $type==STRING1S or $type==STRING3S or $type==BNODE or $type==INTEGER or $type==DECIMAL or $type==DOUBLE or $type==BOOLEAN)) {
			$self->_throw_error("Expecting object but got " . decrypt_constant($type), $t, $l);
		} else {
			if ($type==STRING1D or $type==STRING3D or $type==STRING1S or $type==STRING3S) {
				my $value	= $t->value;
				my $t		= $self->_next_nonws($l);
				my $dt;
				my $lang;
				if ($t) {
					if ($t->type == HATHAT) {
						my $t		= $self->_next_nonws($l);
						if ($t->type == IRI or $t->type == PREFIXNAME) {
							$dt	= $self->_token_to_node($t);
						}
					} elsif ($t->type == LANG) {
						$lang	= $t->value;
					} else {
						$self->_unget_token($t);
					}
				}
				my %args	= (value => $value);
				$args{language}	= $lang if (defined($lang));
				$args{datatype}	= $dt if (defined($dt));
				$obj	= Attean::Literal->new(%args);
			} else {
				$obj	= $self->_token_to_node($t, $type);
			}
		}
		return $obj;
	}
	
	sub _assert_list {
		my $self	= shift;
		my $subj	= shift;
		my @objects	= @_;
		my $head	= $subj;
		while (@objects) {
			my $obj	= shift(@objects);
			$self->_assert_triple($head, Attean::IRI->new(value => "${RDF}first", lazy => 1), $obj);
			my $next	= scalar(@objects) ? Attean::Blank->new() : Attean::IRI->new(value => "${RDF}nil", lazy => 1);
			$self->_assert_triple($head, Attean::IRI->new(value => "${RDF}rest", lazy => 1), $next);
			$head		= $next;
		}
	}

	sub _predicateObjectList {
		my $self	= shift;
		my $l		= shift;
		my $subj	= shift;
		my $t		= $self->_next_nonws($l);
		while (1) {
			my $type = $t->type;
			unless ($type==IRI or $type==PREFIXNAME or $type==A) {
				$self->_throw_error("Expecting verb but got " . decrypt_constant($type), $t, $l);
			}
			my $pred	= $self->_token_to_node($t);
			$self->_objectList($l, $subj, $pred);
		
			$t		= $self->_next_nonws($l);
			last unless ($t);
			if ($t->type == SEMICOLON) {
				my $sc	= $t;
	SEMICOLON_REPEAT:			
				$t		= $self->_next_nonws($l);
				unless ($t) {
					$l->_throw_error("Expecting token after semicolon, but got EOF");
				}
				goto SEMICOLON_REPEAT if ($t->type == SEMICOLON);
				if ($t->type == IRI or $t->type == PREFIXNAME or $t->type == A) {
					next;
				} else {
					$self->_unget_token($t);
					return;
				}
			} else {
				$self->_unget_token($t);
				return;
			}
		}
	}

	sub _objectList {
		my $self	= shift;
		my $l		= shift;
		my $subj	= shift;
		my $pred	= shift;
		while (1) {
			my $t		= $self->_next_nonws($l);
			last unless ($t);
			my $obj		= $self->_object($l, $t);
			$self->_assert_triple_with_optional_annotation($l, $subj, $pred, $obj);
		
			$t	= $self->_next_nonws($l);
			if ($t and $t->type == COMMA) {
				next;
			} else {
				$self->_unget_token($t);
				return;
			}
		}
	}

	sub _assert_triple_with_optional_annotation {
		my $self	= shift;
		my $l		= shift;
		my $subj	= shift;
		my $pred	= shift;
		my $obj		= shift;
		my $qt		= $self->_assert_triple($subj, $pred, $obj);
		
		my $t	= $self->_next_nonws($l);
		if ($t->type != LANNOT) {
			$self->_unget_token($t);
			return;
		}
		
		$self->_predicateObjectList( $l, $qt );
		$self->_get_token_type($l, RANNOT);
	}
	
	sub _assert_triple {
		my $self	= shift;
		my $subj	= shift;
		my $pred	= shift;
		my $obj		= shift;
		if ($self->canonicalize and blessed($obj) and $obj->does('Attean::API::Literal')) {
			$obj	= $obj->canonicalize;
		}
	
		my $t		= Attean::Triple->new($subj, $pred, $obj);
		$self->handler->($t);
		return $t;
	}


	sub _object {
		my $self	= shift;
		my $l		= shift;
		my $t		= shift;
		my $tcopy	= $t;
		my $obj;
		my $type	= $t->type;
		if ($type==LTLT) {
			return $self->_quotedTriple($l);
		} elsif ($type==LBRACKET) {
			$obj	= Attean::Blank->new();
			my $t	= $self->_next_nonws($l);
			unless ($t) {
				$self->_throw_error("Expecting object but got only opening bracket", $tcopy, $l);
			}
			if ($t->type != RBRACKET) {
				$self->_unget_token($t);
				$self->_predicateObjectList( $l, $obj );
				$t	= $self->_get_token_type($l, RBRACKET);
			}
		} elsif ($type == LPAREN) {
			my $t	= $self->_next_nonws($l);
			unless ($t) {
				$self->_throw_error("Expecting object but got only opening paren", $tcopy, $l);
			}
			if ($t->type == RPAREN) {
				$obj	= Attean::IRI->new(value => "http://www.w3.org/1999/02/22-rdf-syntax-ns#nil", lazy => 1);
			} else {
				$obj	= Attean::Blank->new();
				my @objects	= $self->_object($l, $t);
			
				while (1) {
					my $t	= $self->_next_nonws($l);
					if ($t->type == RPAREN) {
						last;
					} else {
						push(@objects, $self->_object($l, $t));
					}
				}
				$self->_assert_list($obj, @objects);
			}
		} elsif (not($type==IRI or $type==PREFIXNAME or $type==STRING1D or $type==STRING3D or $type==STRING1S or $type==STRING3S or $type==BNODE or $type==INTEGER or $type==DECIMAL or $type==DOUBLE or $type==BOOLEAN)) {
			$self->_throw_error("Expecting object but got " . decrypt_constant($type), $t, $l);
		} else {
			if ($type==STRING1D or $type==STRING3D or $type==STRING1S or $type==STRING3S) {
				my $value	= $t->value;
				my $t		= $self->_next_nonws($l);
				my $dt;
				my $lang;
				if ($t) {
					if ($t->type == HATHAT) {
						my $t		= $self->_next_nonws($l);
						if ($t->type == IRI or $t->type == PREFIXNAME) {
							$dt	= $self->_token_to_node($t);
						}
					} elsif ($t->type == LANG) {
						$lang	= $t->value;
					} else {
						$self->_unget_token($t);
					}
				}
				my %args	= (value => $value);
				$args{language}	= $lang if (defined($lang));
				$args{datatype}	= $dt if (defined($dt));
				$obj	= Attean::Literal->new(%args);
			} else {
				$obj	= $self->_token_to_node($t, $type);
			}
		}
		return $obj;
	}

	sub _token_to_node {
		my $self	= shift;
		my $t		= shift;
		my $type	= shift || $t->type;
		if ($type eq A) {
			state $rdftype	= Attean::IRI->new(value => "${RDF}type", lazy => 1);
			return $rdftype;
		}
		elsif ($type eq IRI) {
			my $value	= $t->value;
			my %args	= (value => $value);
			my $iri;
			if ($self->has_base) {
				$args{base}	= $self->base;
				my $iri	= $self->new_iri(%args);
				return $iri;
			}
			
			state %cache;
			 if (my $n = $cache{$value}) {
				return $n;
			} else {
				my $iri	= $self->new_iri(%args);
				if (rand() < 0.02) {
					# clear out the cache roughly every 50 IRIs
					%cache	= ();
				}
				$cache{$value}	= $iri;
				return $iri;
			}
		}
		elsif ($type eq INTEGER) {
			return Attean::Literal->new(value => $t->value, datatype => Attean::IRI->new(value => "${XSD}integer", lazy => 1));
		}
		elsif ($type eq DECIMAL) {
			return Attean::Literal->new(value => $t->value, datatype => Attean::IRI->new(value => "${XSD}decimal", lazy => 1));
		}
		elsif ($type eq DOUBLE) {
			return Attean::Literal->new(value => $t->value, datatype => Attean::IRI->new(value => "${XSD}double", lazy => 1));
		}
		elsif ($type eq BOOLEAN) {
			return Attean::Literal->new(value => $t->value, datatype => Attean::IRI->new(value => "${XSD}boolean", lazy => 1));
		}
		elsif ($type eq PREFIXNAME) {
			my ($ns, $local)	= @{ $t->args };
			$ns		=~ s/:$//;
			unless (exists $self->_map->{$ns}) {
				$self->_throw_error("Use of undeclared prefix '$ns'", $t);
			}
			my $prefix			= $self->_map->{$ns};
			no warnings 'uninitialized';
			my $iri				= $self->new_iri("${prefix}${local}");
			return $iri;
		}
		elsif ($type eq BNODE) {
			return Attean::Blank->new($t->value);
		}
		elsif ($type eq STRING1D) {
			return Attean::Literal->new($t->value);
		}
		elsif ($type eq STRING1S) {
			return Attean::Literal->new($t->value);
		}
		else {
			$self->_throw_error("Converting $type to node not implemented", $t);
		}
	}

	sub _throw_error {
		my $self	= shift;
		my $message	= shift;
		my $t		= shift;
		my $l		= shift;
		my $line	= $t->start_line;
		my $col		= $t->start_column;
	# 	Carp::cluck "$message at $line:$col";
		my $text	= "$message at $line:$col";
		if (defined($t->value)) {
			$text	.= " (near '" . $t->value . "')";
		}
		die $text;
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

Copyright (c) 2014--2022 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
