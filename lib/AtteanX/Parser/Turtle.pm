use v5.14;
use warnings;

# AtteanX::Parser::Turtle
# -----------------------------------------------------------------------------

=head1 NAME

AtteanX::Parser::Turtle - Turtle RDF Parser

=head1 VERSION

This document describes AtteanX::Parser::Turtle version 1.007

=head1 SYNOPSIS

 use AtteanX::Parser::Turtle;
 my $parser	= AtteanX::Parser::Turtle->new( handler => sub {...} );
 $parser->parse_cb_from_io( $fh, $base_uri );

=head1 DESCRIPTION

This module implements a parser for the Turtle RDF format.

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::Turtle 0.001 {
	use utf8;
	use Encode qw(encode);
	use Scalar::Util qw(blessed);
	use Data::Dumper;
	use AtteanX::Parser::Turtle::Constants;
	use AtteanX::Parser::Turtle::Lexer;
	use AtteanX::Parser::Turtle::Token;
	use Attean::API::Parser;
	
	use Moose;
	my $ITEM_TYPE = Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Triple');
	
	sub canonical_media_type { return "text/turtle" }
	sub media_types {
		return [qw(application/x-turtle application/turtle text/turtle)];
	}
	has 'handled_type' => (
		is => 'ro',
		isa => 'Moose::Meta::TypeConstraint',
		init_arg => undef,
		default => sub { $ITEM_TYPE },
	);

	has 'canonicalize'	=> (is => 'rw', isa => 'Bool', default => 0);
	has 'map' => (is => 'ro', isa => 'HashRef[Str]', default => sub { +{} });
	has 'namespaces' => (is => 'rw', isa => 'Maybe[URI::NamespaceMap]', predicate => 'has_namespaces');
	has	'stack'	=> (
		is => 'ro',
		isa => 'ArrayRef',
		default => sub { [] },
		init_arg => undef,
		traits  => ['Array'],
		handles => {
			stack_size	=> 'count',
			stack_push	=> 'push',
			stack_pop	=> 'pop',
		}
	);

	with 'Attean::API::Parser::AbbreviatingParser';
	with 'Attean::API::PushParser';
	
	# TODO: Remove all references to RDF::Trine code (e.g. RDF::Trine::Error)

	our $VERSION;
	BEGIN {
		$VERSION				= '1.007';
	# 	foreach my $ext (qw(ttl)) {
	# 		$RDF::Trine::Parser::file_extensions{ $ext }	= __PACKAGE__;
	# 	}
	# 	$RDF::Trine::Parser::parser_names{ 'turtle' }	= __PACKAGE__;
	# 	my $class										= __PACKAGE__;
	# 	$RDF::Trine::Parser::encodings{ $class }		= 'utf8';
	# 	$RDF::Trine::Parser::format_uris{ 'http://www.w3.org/ns/formats/Turtle' }	= __PACKAGE__;
	# 	$RDF::Trine::Parser::canonical_media_types{ $class }	= 'text/turtle';
	# 	foreach my $type (qw(application/x-turtle application/turtle text/turtle)) {
	# 		$RDF::Trine::Parser::media_types{ $type }	= __PACKAGE__;
	# 	}
	}

	my $RDF	= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
	my $XSD	= 'http://www.w3.org/2001/XMLSchema#';

	sub parse_cb_from_io {
		my $self	= shift;
		my $fh		= shift;

		unless (ref($fh)) {
			my $filename	= $fh;
			undef $fh;
			open( $fh, '<', $filename ) or throw RDF::Trine::Error::ParserError -text => $!;
		}
	
		binmode($fh, ':encoding(UTF-8)');
		my $l	= AtteanX::Parser::Turtle::Lexer->new($fh);
		$self->_parse($l);
	}

	sub parse_cb_from_bytes {
		my $self	= shift;
		my $data	= shift;
	
		$data	= Encode::encode("utf-8", $data);
		open(my $fh, '<:encoding(UTF-8)', \$data);
		my $l	= AtteanX::Parser::Turtle::Lexer->new($fh);
		$self->_parse($l);
	}

=item C<< parse_node ( $string, $base ) >>

Returns the Attean::API::Term object corresponding to the node whose N-Triples
serialization is found at the beginning of C<< $string >>.

=cut

	sub parse_node {
		my $self	= shift;
		my $string	= shift;
		my %args	= @_;
	
		open(my $fh, '<:encoding(UTF-8)', \$string);
		my $l	= AtteanX::Parser::Turtle::Lexer->new($fh);
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
		$self->stack_push($t);
# 		push(@{ $self->{ stack } }, $t);
	}

	sub _next_nonws {
		my $self	= shift;
		my $l		= shift;
		if ($self->stack_size) {
# 		if (scalar(@{ $self->{ stack } })) {
			return $self->stack_pop;
# 			return pop(@{ $self->{ stack } });
		}
		while (1) {
			my $t	= $l->get_token;
			return unless ($t);
			my $type = $t->type;
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
		if ($type == PREFIX or $type == SPARQLPREFIX) {
			$t	= $self->_get_token_type($l, PREFIXNAME);
			my $name	= $t->value;
			$name		=~ s/:$//;
			$t	= $self->_get_token_type($l, IRI);
			my %args	= (value => $t->value);
			if ($self->has_base) {
				$args{base}	= $self->base;
			}
			my $r	= Attean::IRI->new(%args);
			my $iri	= $r->as_string;
			if ($type == PREFIX) {
				$t	= $self->_get_token_type($l, DOT);
	# 			$t	= $self->_next_nonws($l);
	# 			if ($t and $t->type != DOT) {
	# 				$self->_unget_token($t);
	# 			}
			}
			$self->map->{$name}	= $iri;
			if ($self->has_namespaces) {
				my $ns	= $self->namespaces;
				unless ($ns->namespace_uri($name)) {
					$ns->add_mapping($name, $iri);
				}
			}
		}
		elsif ($type == BASE or $type == SPARQLBASE) {
			$t	= $self->_get_token_type($l, IRI);
			my %args	= (value => $t->value);
			if ($self->has_base) {
				$args{base}	= $self->base;
			}
			my $r	= Attean::IRI->new(%args);
			my $iri	= $r->as_string;
			if ($type == BASE) {
				$t	= $self->_get_token_type($l, DOT);
	# 			$t	= $self->_next_nonws($l);
	# 			if ($t and $t->type != DOT) {
	# 				$self->_unget_token($t);
	# 			}
			}
			$self->set_base($iri);
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
		if ($type == LBRACKET) {
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
				$subj	= Attean::IRI->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil');
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

	sub _assert_list {
		my $self	= shift;
		my $subj	= shift;
		my @objects	= @_;
		my $head	= $subj;
		while (@objects) {
			my $obj	= shift(@objects);
			$self->_assert_triple($head, Attean::IRI->new("${RDF}first"), $obj);
			my $next	= scalar(@objects) ? Attean::Blank->new() : Attean::IRI->new("${RDF}nil");
			$self->_assert_triple($head, Attean::IRI->new("${RDF}rest"), $next);
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
	# 	warn "objectList: " . Dumper($subj, $pred);	# XXX
		while (1) {
			my $t		= $self->_next_nonws($l);
			last unless ($t);
			my $obj		= $self->_object($l, $t);
			$self->_assert_triple($subj, $pred, $obj);
		
			$t	= $self->_next_nonws($l);
			if ($t and $t->type == COMMA) {
				next;
			} else {
				$self->_unget_token($t);
				return;
			}
		}
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
	}


	sub _object {
		my $self	= shift;
		my $l		= shift;
		my $t		= shift;
		my $tcopy	= $t;
		my $obj;
		my $type	= $t->type;
		if ($type==LBRACKET) {
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
				$obj	= Attean::IRI->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#nil');
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
			return Attean::IRI->new("${RDF}type");
		}
		elsif ($type eq IRI) {
			my %args	= (value => $t->value);
			if ($self->has_base) {
				$args{base}	= $self->base;
			}
			return Attean::IRI->new(%args);
		}
		elsif ($type eq INTEGER) {
			return Attean::Literal->new(value => $t->value, datatype => Attean::IRI->new("${XSD}integer"));
		}
		elsif ($type eq DECIMAL) {
			return Attean::Literal->new(value => $t->value, datatype => Attean::IRI->new("${XSD}decimal"));
		}
		elsif ($type eq DOUBLE) {
			return Attean::Literal->new(value => $t->value, datatype => Attean::IRI->new("${XSD}double"));
		}
		elsif ($type eq BOOLEAN) {
			return Attean::Literal->new(value => $t->value, datatype => Attean::IRI->new("${XSD}boolean"));
		}
		elsif ($type eq PREFIXNAME) {
			my ($ns, $local)	= @{ $t->args };
			$ns		=~ s/:$//;
			unless (exists $self->map->{$ns}) {
				$self->_throw_error("Use of undeclared prefix '$ns'", $t);
			}
			my $prefix			= $self->map->{$ns};
			no warnings 'uninitialized';
			my $iri				= Attean::IRI->new("${prefix}${local}");
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
		RDF::Trine::Error::ParserError::Tokenized->throw(
			-text => $text,
			-object => $t,
		);
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

Copyright (c) 2006-2012 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
