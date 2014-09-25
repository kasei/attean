use v5.14;
use warnings;

package AtteanX::Parser::RDFXML 0.001 {
	use Moo;
	use Types::Standard qw(Str Object);
	use Attean;
	use Attean::RDF;
	
	use Carp;
	use Encode;
	use XML::SAX;
	use Data::Dumper;
	use Scalar::Util qw(blessed);
	use Module::Load::Conditional qw[can_load];

=head1 METHODS

=over 4

=item C<< canonical_media_type >>

Returns the canonical media type for SPARQL XML: application/sparql-results+json.

=cut

	sub canonical_media_type { return "application/rdf+xml" }

=item C<< media_types >>

Returns a list of media types that may be parsed with the SPARQL XML parser:
application/sparql-results+json.

=cut

	sub media_types { return [qw(application/rdf+xml application/octet-stream)]; }
	
=item C<< file_extensions >>

Returns a list of file extensions that may be parsed with the parser.

=cut

	sub file_extensions { return [qw(rdf xrdf)] }

	with 'Attean::API::TripleParser', 'Attean::API::AbbreviatingParser';
	with 'Attean::API::PushParser';

	has 'bnode_prefix'	=> (is => 'ro', isa => Str, default => '');
	
=item C<< parse_cb_from_io( $fh ) >>

Calls the C<< $parser->handler >> function once for each
L<Attean::API::Binding> object that result from parsing
the data read from the L<IO::Handle> object C<< $fh >>.

=cut

	sub parse_cb_from_io {
		my $self	= shift;
		$self->_parse(@_);
	}
	
=item C<< parse_cb_from_bytes( $data ) >>

Calls the C<< $parser->handler >> function once for each
L<Attean::API::Binding> object that result from parsing
the data read from the UTF-8 encoded byte string C<< $data >>.

=cut

	sub parse_cb_from_bytes {
		my $self	= shift;
		$self->_parse(@_);
	}

	sub _parse {
		my $self	= shift;
		my $data	= shift;

		my $saxhandler	= AtteanX::Parser::RDFXML::SAXHandler->new( bnode_prefix => $self->bnode_prefix, handler => $self->handler );
		my $p			= XML::SAX::ParserFactory->parser(Handler => $saxhandler);
		$saxhandler->push_base( $self->base ) if ($self->has_base);
		if (ref($data)) {
			$p->parse_file($data);
		} else {
			$p->parse_string($data);
		}
		my $nodes	= $saxhandler->{nodes};
		if ($nodes and scalar(@$nodes)) {
			die "RDFXML parser node stack isn't empty after parse: " . Dumper($nodes);
		}
		my $expect	= $saxhandler->{expect};
		if ($expect and scalar(@$expect) > 2) {
			die "RDFXML parser expect stack isn't empty after parse:" . Dumper($expect);
		}
	}
}

package AtteanX::Parser::RDFXML::SAXHandler;

use v5.14;
use warnings;
use base qw(XML::SAX::Base);
use List::Util qw(first);
use Module::Load::Conditional qw[can_load];

use Attean::RDF;
use Data::Dumper;
use Scalar::Util qw(blessed);

use constant	NIL			=> 0x00;
use constant	SUBJECT		=> 0x01;
use constant	PREDICATE	=> 0x02;
use constant	OBJECT		=> 0x04;
use constant	LITERAL		=> 0x08;
use constant	COLLECTION	=> 0x16;

my $HAS_XML_LIBXML	= can_load( modules => { 'XML::LibXML'	=> 1.70, } );

sub new {
	my $class	= shift;
	my %args	= @_;
	my $prefix	= $args{ bnode_prefix } // '';
	my $self	= bless( {
					expect			=> [ SUBJECT, NIL ],
					base			=> [],
					depth			=> 0,
					characters		=> '',
					prefix			=> $prefix,
					counter			=> 0,
					nodes			=> [],
					chars_ok		=> 0,
					sthandler		=> $args{handler},
					named_bnodes	=> {},
				}, $class );
	if (my $ns = $args{ namespaces }) {
		$self->{namespaces}	= $ns;
	}
	if (my $base = $args{ base }) {
		$self->push_base( $base );
	}
	return $self;
}

sub new_expect {
	my $self	= shift;
	unshift( @{ $self->{expect} }, shift );
}

sub old_expect {
	shift( @{ shift->{expect} } );
}

sub expect {
	return shift->{expect}[0];
}

sub peek_expect {
	return shift->{expect}[1];
}


sub start_element {
	my $self	= shift;
	my $el		= shift;

	$self->{depth}++;
	$self->handle_scoped_values( $el ) unless ($self->expect == LITERAL);
	if ($self->{depth} == 1 and $el->{NamespaceURI} eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#' and $el->{LocalName} eq 'RDF') {
		# ignore the wrapping rdf:RDF element
	} else {
		my $prefix	= $el->{Prefix};
		my $expect	= $self->expect;
		$self->new_expect( $expect = SUBJECT ) if ($expect == NIL);
	
		if ($expect == SUBJECT or $expect == OBJECT) {
			my $ns		= $self->get_namespace( $prefix );
			my $local	= $el->{LocalName};
			my $uri		= join('', $ns, $local);
			my $node	= $self->new_resource( $uri );
			if ($self->expect == OBJECT) {
				if (defined($self->{characters}) and length(my $string = $self->{characters})) {
					die "character data found before object element" if ($string =~ /\S/);
				}
				delete($self->{characters});	# get rid of any whitespace we saw before the element
			}
			my $node_id	= $self->node_id( $el );
		
			if ($self->peek_expect == COLLECTION) {
				my $list	= $self->new_bnode;
				if (my $last = $self->{ collection_last }[0]) {
					my $st		= Attean::Triple->new( $last, iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), $list );
					$self->assert( $st );
				}
				$self->{ collection_last }[0]	= $list;
				my $st		= Attean::Triple->new( $list, iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#first"), $node_id );
				$self->assert( $st );
				$self->{ collection_head }[0]	||= $list;
			} elsif ($self->expect == OBJECT) {
				my $nodes	= $self->{nodes};
				my $st		= Attean::Triple->new( @{ $nodes }[ $#{$nodes} - 1, $#{$nodes} ], $node_id );
				$self->assert( $st );
			}
		
			if ($uri ne 'http://www.w3.org/1999/02/22-rdf-syntax-ns#Description') {
				my $type	= $node;
				$self->assert( Attean::Triple->new( $node_id, iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"), $node ) );
			}
			push( @{ $self->{nodes} }, $node_id );
		
			$self->parse_literal_property_attributes( $el, $node_id );
			$self->new_expect( PREDICATE );
			unshift(@{ $self->{seqs} }, 0);
		} elsif ($self->expect == COLLECTION) {
		} elsif ($self->expect == PREDICATE) {
			my $ns		= $self->get_namespace( $prefix );
			my $local	= $el->{LocalName};
			my $uri		= join('', $ns, $local);
			my $node	= $self->new_resource( $uri );
		
			if ($node->value eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#li') {
				my $id	= ++(${ $self }{seqs}[0]);
				$node	= $self->new_resource( 'http://www.w3.org/1999/02/22-rdf-syntax-ns#_' . $id );
			}
		
			push( @{ $self->{nodes} }, $node );
			if (my $data = $el->{Attributes}{'{http://www.w3.org/1999/02/22-rdf-syntax-ns#}datatype'}) {
				$self->{datatype}		= $data->{Value};
			}
		
			if (my $data = $el->{Attributes}{'{http://www.w3.org/1999/02/22-rdf-syntax-ns#}ID'}) {
				my $id	= $data->{Value};
				unshift(@{ $self->{reify_id} }, $id);
			} else {
				unshift(@{ $self->{reify_id} }, undef);
			}
		
			if (my $pt = $el->{Attributes}{'{http://www.w3.org/1999/02/22-rdf-syntax-ns#}parseType'}) {
				if ($pt->{Value} eq 'Resource') {
					# fake an enclosing object scope
					my $node	= $self->new_bnode;
					my $nodes	= $self->{nodes};
					push( @$nodes, $node );
					$self->assert( Attean::Triple->new( @{ $nodes }[ $#{$nodes} - 2 .. $#{$nodes} ] ) );
				
					$self->new_expect( PREDICATE );
				} elsif ($pt->{Value} eq 'Literal') {
					$self->{datatype}		= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral';
					my $depth				= $self->{depth};
					$self->{literal_depth}	= $depth - 1;
					$self->new_expect( LITERAL );
				} elsif ($pt->{Value} eq 'Collection') {
					my $depth				= $self->{depth};
					unshift( @{ $self->{ collection_head } }, undef );
					unshift( @{ $self->{ collection_last } }, undef );
					$self->new_expect( COLLECTION );
					$self->new_expect( OBJECT );
				}
			} elsif (my $data = $el->{Attributes}{'{http://www.w3.org/1999/02/22-rdf-syntax-ns#}resource'}) {
				# stash the uri away so that we can use it when we get the end_element call for this predicate
				my $uri	= $self->new_resource( $data->{Value} );
				$self->parse_literal_property_attributes( $el, $uri );
				$self->{'rdf:resource'}	= $uri;
				$self->new_expect( OBJECT );
				$self->{chars_ok}	= 1;
			} elsif (my $ndata = $el->{Attributes}{'{http://www.w3.org/1999/02/22-rdf-syntax-ns#}nodeID'}) {
				my $node_name	= $ndata->{Value};
				# stash the bnode away so that we can use it when we get the end_element call for this predicate
				my $bnode	= $self->get_named_bnode( $node_name );
				$self->parse_literal_property_attributes( $el, $uri );
				$self->{'rdf:resource'}	= $bnode;	# the key 'rdf:resource' is a bit misused here, but both rdf:resource and rdf:nodeID use it for the same purpose, so...
				$self->new_expect( OBJECT );
				$self->{chars_ok}	= 1;
			} elsif (my $node = $self->parse_literal_property_attributes( $el )) {
				# fake an enclosing object scope
				my $nodes	= $self->{nodes};
				push( @$nodes, $node );
				$self->assert( Attean::Triple->new( @{ $nodes }[ $#{$nodes} - 2 .. $#{$nodes} ] ) );
				$self->new_expect( PREDICATE );
			} else {
				$self->new_expect( OBJECT );
				$self->{chars_ok}	= 1;
			}
		} elsif ($self->expect == LITERAL) {
			my $tag;
			if ($el->{Prefix}) {
				$tag	= join(':', @{ $el }{qw(Prefix LocalName)});
			} else {
				$tag	= $el->{LocalName};
			}
			$self->{characters}	.= '<' . $tag;
			my $attr	= $el->{Attributes};
		
			if (my $ns = $el->{NamespaceURI}) {
				my $abbr = $el->{Prefix};
				unless ($self->{defined_literal_namespaces}{$abbr}{$ns}) {
					$self->{characters}	.= ' xmlns';
					if (length($abbr)) {
						$self->{characters}	.= ':' . $abbr;
					}
					$self->{characters}	.= '="' . $ns . '"';
					$self->{defined_literal_namespaces}{$abbr}{$ns}++;
				}
			}
			if (%$attr) {
				foreach my $k (keys %$attr) {
					$self->{characters}	.= ' ';
					my $el	= $attr->{ $k };
					my $prop;
					if ($el->{Prefix}) {
						$prop	= join(':', @{ $el }{qw(Prefix LocalName)});
					} else {
						$prop	= $el->{LocalName};
					}
					$self->{characters}	.= $prop . '="' . $el->{Value} . '"';
				}
			}
			$self->{characters}	.= '>';
		} else {
			die "not sure what type of token is expected";
		}
	}
}

sub end_element {
	my $self	= shift;
	my $el		= shift;
	$self->{depth}--;

	my $cleanup	= 0;
	my $expect	= $self->expect;
	if ($expect == SUBJECT) {
		$self->old_expect;
		$cleanup	= 1;
		$self->{chars_ok}	= 0;
		shift(@{ $self->{reify_id} });
	} elsif ($expect == PREDICATE) {
		$self->old_expect;
		if ($self->expect == PREDICATE) {
			# we're closing a parseType=Resource block, so take off the extra implicit node.
			pop( @{ $self->{nodes} } );
		} else {
			shift(@{ $self->{seqs} });
		}
		$cleanup	= 1;
		$self->{chars_ok}	= 0;
	} elsif ($expect == OBJECT or ($expect == LITERAL and $self->{literal_depth} == $self->{depth})) {
		if (exists $self->{'rdf:resource'}) {
			my $uri	= delete $self->{'rdf:resource'};
			my $nodes	= $self->{nodes};
			delete $self->{characters};
			$self->assert( Attean::Triple->new( @{ $nodes }[ $#{$nodes} - 1, $#{$nodes} ], $uri ) );
		}
		$self->old_expect;
		if (defined($self->{characters})) {
			my $string	= $self->{characters};
			my $literal	= $self->new_literal( $string );
			my $nodes	= $self->{nodes};
			$self->assert( Attean::Triple->new( @{ $nodes }[ $#{$nodes} - 1, $#{$nodes} ], $literal ) );
			delete($self->{characters});
			delete $self->{datatype};
			delete $self->{defined_literal_namespaces};
		}
	
		if ($self->expect == COLLECTION) {
			# We were expecting an object, but got an end_element instead.
			# after poping the OBJECT expectation, we see we were expecting objects in a COLLECTION.
			# so we're ending the COLLECTION here:
			$self->old_expect;
			my $nodes	= $self->{nodes};
			my $head	= $self->{ collection_head }[0] || iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil");
			my @nodes	= (@{ $nodes }[ $#{$nodes} - 1, $#{$nodes} ], $head);
			my $st		= Attean::Triple->new( @nodes );
			$self->assert( $st );
		
			if (my $last = $self->{ collection_last }[0]) {
				my @nodes	= ( $last, iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#rest"), iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#nil") );
				my $st		= Attean::Triple->new( @nodes );
				$self->assert( $st );
			}
		
			shift( @{ $self->{ collection_last } } );
			shift( @{ $self->{ collection_head } } );
		}
	
		$cleanup	= 1;
		$self->{chars_ok}	= 0;
		shift(@{ $self->{reify_id} });
	} elsif ($expect == COLLECTION) {
		shift( @{ $self->{collections} } );
		$self->old_expect;
	} elsif ($expect == LITERAL) {
		my $tag;
		if ($el->{Prefix}) {
			$tag	= join(':', @{ $el }{qw(Prefix LocalName)});
		} else {
			$tag	= $el->{LocalName};
		}
		$self->{characters}	.= '</' . $tag . '>';
		$cleanup	= 0;
	} else {
		die "how did we get here?";
	}

	if ($cleanup) {
		pop( @{ $self->{nodes} } );
		$self->pop_namespace_pad();
		$self->pop_language();
		$self->pop_base();
	}
}

sub characters {
	my $self	= shift;
	my $data	= shift;
	my $expect	= $self->expect;
	if ($expect == LITERAL or ($expect == OBJECT and $self->{chars_ok})) {
		my $chars	= $data->{Data};
		$self->{characters}	.= $chars;
	}
}

sub parse_literal_property_attributes {			
	my $self	= shift;
	my $el		= shift;
	my $node_id	= shift || $self->new_bnode;
	my @keys	= grep { not(m<[{][}](xmlns|about)>) }
					grep { not(m<[{]http://www.w3.org/1999/02/22-rdf-syntax-ns#[}](resource|about|ID|datatype|nodeID)>) }
					grep { not(m<[{]http://www.w3.org/XML/1998/namespace[}](base|lang)>) }
					keys %{ $el->{Attributes} };
	my $asserted	= 0;

	unshift(@{ $self->{reify_id} }, undef);	# don't reify any of these triples
	foreach my $k (@keys) {
		my $data = $el->{Attributes}{ $k };
		my $ns		= $data->{NamespaceURI};
		unless ($ns) {
			my $prefix	= $data->{Prefix};
			next unless (length($ns));
			$ns			= $self->get_namespace( $prefix );
		}
		next if ($ns eq 'http://www.w3.org/XML/1998/namespace');
		next if ($ns eq 'http://www.w3.org/2000/xmlns/');
		my $local	= $data->{LocalName};
		my $uri		= join('', $ns, $local);
		my $value	= $data->{Value};
		my $pred	= $self->new_resource( $uri );
		my $term	= ($uri eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#type') ? $self->new_resource( $value ) : $self->new_literal( $value );
		$self->assert( Attean::Triple->new( $node_id, $pred, $term ) );
		$asserted++;
	}
	shift(@{ $self->{reify_id} });
	return ($asserted ? $node_id : 0);
}

sub assert {
	my $self	= shift;
	my $st		= shift;

	if ($self->{sthandler}) {
		$self->{sthandler}->( $st );
		if (defined(my $id = $self->{reify_id}[0])) {
			my $stid	= $self->new_resource( "#$id" );
		
			my $tst	= Attean::Triple->new( $stid, iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"), iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#Statement") );
			my $sst	= Attean::Triple->new( $stid, iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#subject"), $st->subject );
			my $pst	= Attean::Triple->new( $stid, iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#predicate"), $st->predicate );
			my $ost	= Attean::Triple->new( $stid, iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#object"), $st->object );
			foreach ($tst, $sst, $pst, $ost) {
				$self->{sthandler}->( $_ );
			}
			$self->{reify_id}[0]	= undef;	# now that we've used this reify ID, get rid of it (because we don't want it used again)
		}
	}
}

sub node_id {
	my $self	= shift;
	my $el		= shift;

	if ($el->{Attributes}{'{http://www.w3.org/1999/02/22-rdf-syntax-ns#}about'}) {
		my $uri	= $el->{Attributes}{'{http://www.w3.org/1999/02/22-rdf-syntax-ns#}about'}{Value};
		return $self->new_resource( $uri );
	} elsif ($el->{Attributes}{'{http://www.w3.org/1999/02/22-rdf-syntax-ns#}ID'}) {
		my $uri	= $el->{Attributes}{'{http://www.w3.org/1999/02/22-rdf-syntax-ns#}ID'}{Value};
		return $self->new_resource( '#' . $uri );
	} elsif ($el->{Attributes}{'{http://www.w3.org/1999/02/22-rdf-syntax-ns#}nodeID'}) {
		my $name	= $el->{Attributes}{'{http://www.w3.org/1999/02/22-rdf-syntax-ns#}nodeID'}{Value};
		return $self->get_named_bnode( $name );
	} else {
		return $self->new_bnode;
	}
}

sub handle_scoped_values {
	my $self	= shift;
	my $el		= shift;
	my %new;

	{
		# xml:base
		my $base	= '';
		if (exists($el->{Attributes}{'{http://www.w3.org/XML/1998/namespace}base'})) {
			my $uri	= $el->{Attributes}{'{http://www.w3.org/XML/1998/namespace}base'}{Value};
			$base	= $self->new_resource( $uri );
		}
		$self->push_base( $base );
	}

	{
		# language
		my $lang	= '';
		if (exists($el->{Attributes}{'{http://www.w3.org/XML/1998/namespace}lang'})) {
			$lang	= $el->{Attributes}{'{http://www.w3.org/XML/1998/namespace}lang'}{Value};
		}
		$self->push_language( $lang );
	}

	{
		# namespaces
		my @ns		= grep { m<^[{]http://www.w3.org/2000/xmlns/[}]> } (keys %{ $el->{Attributes} });
		foreach my $n (@ns) {
			my ($prefix)	= substr($n, 31);
			my $value		= $el->{Attributes}{$n}{Value};
			$new{ $prefix }	= $value;
			if (blessed(my $ns = $self->{namespaces})) {
				unless ($ns->namespace_uri($prefix)) {
					$ns->add_mapping( $prefix => $value );
				}
			}
		}
	
		if (exists($el->{Attributes}{'{}xmlns'})) {
			my $value		= $el->{Attributes}{'{}xmlns'}{Value};
			$new{ '' }		= $value;
		}
	
		$self->push_namespace_pad( \%new );
	}
}

sub push_base {
	my $self	= shift;
	my $base	= shift;
	if ($base) {
		my $uri		= (blessed($base) and $base->isa('URI')) ? $base : URI->new($base->value );
		$uri->fragment( undef );
		$base	= iri( "$uri" );
	}
	unshift( @{ $self->{base} }, $base );
}

sub pop_base {
	my $self	= shift;
	shift( @{ $self->{base} } );
}

sub get_base {
	my $self	= shift;
	return first { length($_) } @{ $self->{base} };
}

sub push_language {
	my $self	= shift;
	my $lang	= shift;
	unshift( @{ $self->{language} }, $lang );
}

sub pop_language {
	my $self	= shift;
	shift( @{ $self->{language} } );
}

sub get_language {
	my $self	= shift;
	my $lang	= first { length($_) } @{ $self->{language} };
	return $lang // '';
}

sub push_namespace_pad {
	my $self	= shift;
	my $pad		= shift;
	unshift( @{ $self->{_namespaces} }, $pad );
}

sub pop_namespace_pad {
	my $self	= shift;
	shift( @{ $self->{_namespaces} } );
}

sub get_namespace {
	my $self	= shift;
	my $prefix	= shift;
	foreach my $level (0 .. $#{ $self->{_namespaces} }) {
		my $pad		= $self->{_namespaces}[ $level ];
		if (exists($pad->{ $prefix })) {
			my $uri		= $pad->{ $prefix };
			return $uri;
		}
	}
	die "Unknown namespace: $prefix";
}

sub new_bnode {
	my $self	= shift;
	if (my $prefix = $self->{prefix}) {
		my $id	= $prefix . ++$self->{counter};
		return Attean::Blank->new( $id );
	} else {
		return Attean::Blank->new();
	}
}

sub new_literal {
	my $self	= shift;
	my $string	= shift;
	my %args;
	if (my $dt = $self->{datatype}) {	# datatype
		$args{datatype}	= $dt;
		if ($dt eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#XMLLiteral') {
			if ($HAS_XML_LIBXML) {
				eval {
					if ($string =~ m/^</) {
						my $doc 	= XML::LibXML->load_xml(string => $string);
						my $canon	= $doc->toStringEC14N(1);
						$string	= $canon;
					}
				};
				if ($@) {
					warn "Cannot canonicalize XMLLiteral: $@" . Dumper($string);
				}
			}
		}
	} elsif (my $lang = $self->get_language) {
		$args{language}	= $lang;
	}
	my $literal	= Attean::Literal->new( value => $string, %args );
}

sub new_resource {
	my $self	= shift;
	my $uri		= shift;
	my ($base)	= $self->get_base;
	return Attean::IRI->new( value => $uri, $base ? (base => $base) : () );
}

sub get_named_bnode {
	my $self	= shift;
	my $name	= shift;
	return ($self->{named_bnodes}{ $name } ||= $self->new_bnode);
}

1;

__END__

=back

=cut
