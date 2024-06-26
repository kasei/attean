use v5.14;
use warnings;
use utf8;

=head1 NAME

AtteanX::Functions::CompositeLists - Functions and aggregates to work with composite lists

=head1 VERSION

This document describes AtteanX::Functions::CompositeLists version 0.034

=head1 SYNOPSIS

  use v5.14;
  use Attean;

=head1 DESCRIPTION

This is a utility package that defines functions and aggregates to work with
composite list datatypes.

=over 4

=cut

package AtteanX::Functions::CompositeLists::TurtleLexerWithNull {
	use Moo;
	use AtteanX::Parser::Turtle;
	use AtteanX::SPARQL::Constants;
	extends 'AtteanX::Parser::Turtle::Lexer';

	sub get_token {
		my $self	= shift;
		while (1) {
			$self->fill_buffer unless (length($self->buffer));

			if ($self->buffer =~ /^[ \r\n\t]+/o) {
				my $start_column	= $self->column;
				my $start_line		= $self->line;
				my $ws	= $self->read_length($+[0]);
				# we're ignoring whitespace tokens, but we could return them here instead of falling through to the 'next':
				unless ($self->ignore_whitespace) {
		 			return $self->new_token(WS, $start_line, $start_column, $ws);
				}
				next;
			}

			my $c	= $self->peek_char();
			return unless (defined($c));
			if ($c eq ':') {
				$self->read_length(1);
				return AtteanX::Parser::Turtle::Token->fast_constructor(PREFIXNAME, -1, -1, -1, -1, [':']);
			}
			if ($self->buffer =~ /^null\b/) {
				$self->read_length($+[0]);
				return 1;
			}
			return $self->SUPER::get_token();
		}
	}
}

package AtteanX::Functions::CompositeLists 0.034 {

	use Attean;
	use Attean::RDF;
	use Encode qw(decode_utf8);
	use Scalar::Util qw(blessed);
	use Digest::SHA qw(sha1_hex);
	use AtteanX::Serializer::TurtleTokens;
	use AtteanX::Parser::Turtle;
	use AtteanX::SPARQL::Constants;
	use AtteanX::Functions::CompositeMaps;
	
	our $CDT_BASE		= 'http://w3id.org/awslabs/neptune/SPARQL-CDTs/';
	our $LIST_TYPE_IRI	= "${CDT_BASE}List";

=item C<< lex_to_list($literal) >>

=cut
	sub lex_to_list {
		my $l	= shift;
		die 'TypeError: Cannot parse non-literal to cdt:List' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError: not a datatype literal' unless ($dt);
		die 'TypeError: Expecting a List but found '  . $dt->value unless ($dt->value eq $LIST_TYPE_IRI);
		my $lex	= $l->value;
		$lex	=~ s/^\s*//g;
		$lex	=~ s/\s*$//g;
		
		unless ($lex =~ m<^\[(.*)\]$>s) {
			die 'TypeError: Invalid lexical form for cdt:List literal: '  . $dt->value;
		}
		
		open(my $fh, '<:encoding(UTF-8)', \$lex);
		my $p		= AtteanX::Parser::Turtle->new();
		local($p->{enable_cdt_rewriting})	= 0;
		
		my $lexer	= AtteanX::Functions::CompositeLists::TurtleLexerWithNull->new(file => $fh);
		my @nodes;
		eval {
			my $t = $p->_next_nonws($lexer);
			if ($t->type == LBRACKET) {
				push(@nodes, @{AtteanX::Functions::CompositeMaps::_recursive_lexer_parse_cdt($p, $lexer)});
			}
# 			while (my $t = $p->_next_nonws($lexer)) {
# 				
# # 				warn "TOKEN: $t\n";
# 				if ($t and not blessed($t)) {
# 					# this is the special value returned from our lexer subclass that indicates a null values
# 					push(@nodes, undef);
# 				} else {
# 					next if ($t->type == COMMA);
# 					push(@nodes, $p->_object($lexer, $t));
# 				}
# 			}
		};

		return @nodes;
		
		# this would create fresh bnodes on each access:
# 		my $mapper			= Attean::TermMap->uuid_blank_map;
# 		return map { blessed($_) ? $mapper->map($_) : $_ } @nodes;
	}

=item C<< list_to_lex(@terms) >>

=cut
	sub list_to_lex {
		my @terms	= @_;
		my $s		= AtteanX::Serializer::TurtleTokens->new( suppress_whitespace => 1 );
		my $bytes	= '';
		open(my $io, '>', \$bytes);
		my $first	= 1;
		foreach my $t (@terms) {
			my @tokens;
			unless ($first) {
				push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(COMMA, -1, -1, -1, -1, [',']));
			}
			$first	= 0;
			if (blessed($t)) {
				push(@tokens, $t->sparql_tokens->elements);
				my $iter	= Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::Parser::Turtle::Token' );
				$s->serialize_iter_to_io($io, $iter);
			} else {
				my $iter	= Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::Parser::Turtle::Token' );
				$s->serialize_iter_to_io($io, $iter);
				print {$io} "null";
			}
		}
		close($io);
		my $str		= decode_utf8($bytes);
		chomp($str);
		return dtliteral("[${str}]", $LIST_TYPE_IRI);
	}







=item C<< stringSplit($string, $pattern) >>

=cut
	sub stringSplit {
		my $model			= shift;
		my $active_graph	= shift;
		my $string	= shift;
		my $pattern	= shift;
		my @parts	= map { literal($_) } split(quotemeta($pattern->value), $string->value);
		return list_to_lex(@parts);
	}
	
=item C<< listCreate(@list) >>

=cut
	sub listCreate {
		my $model			= shift;
		my $active_graph	= shift;
		my $literal			= eval { list_to_lex(@_) };
		warn "cdt:List constructor error: $@" if $@;
		return $literal;
	}

=item C<< ctGet($ct, $key) >>

=cut
	sub ctGet {
		my $model			= shift;
		my $active_graph	= shift;
		my $ct				= shift;
		my $pos				= shift;
		die 'TypeError: Cannot interpret non-literal as CDT type' unless ($ct->does('Attean::API::Literal'));
		my $dt	= $ct->datatype;
		if ($dt->value eq $LIST_TYPE_IRI) {
			return listGet($model, $active_graph, $ct, $pos);
		} elsif ($dt->value eq $AtteanX::Functions::CompositeMaps::MAP_TYPE_IRI) {
			return AtteanX::Functions::CompositeMaps::mapGet($model, $active_graph, $ct, $pos);
		} else {
			die 'TypeError: Unexpected non-CDT type: ' . $dt->value;
		}
	}

=item C<< listGet($list, $pos) >>

=cut
	sub listGet {
		my $model			= shift;
		my $active_graph	= shift;
		my $l				= shift;
		my $pos				= shift;
		die 'TypeError: Cannot interpret non-literal as cdt:List' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError: Unexpected non-List type: ' . $dt->value unless ($dt->value eq $LIST_TYPE_IRI);
		my @nodes	= lex_to_list($l);
		die 'TypeError' unless ($pos->does('Attean::API::NumericLiteral') and $pos->datatype->value eq 'http://www.w3.org/2001/XMLSchema#integer');
		my $i		= int($pos->value) - 1; # cdt:get is 1-based, while the array index below is 0-based
		die 'Unexpected non-positive get index' unless ($i >= 0);
		return $nodes[$i];
	}

=item C<< listSubseq($list, $pos, $len) >>

=cut
	sub listSubseq {
		my $model			= shift;
		my $active_graph	= shift;
		my $l				= shift;
		foreach my $term (@_) {
			die 'TypeError' unless ($term->does('Attean::API::NumericLiteral') and $term->datatype->value eq 'http://www.w3.org/2001/XMLSchema#integer');
		}
		my $pos				= shift;
		my @len				= @_;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError' unless ($dt->value eq $LIST_TYPE_IRI);
		my @nodes		= lex_to_list($l);
		my $start		= int($pos->value);
		die 'Unexpected non-positive subseq start argument' unless ($start > 0);

		my @length		= map { int($_->value) } @len;
		if ($start == (1+scalar(@nodes))) {
			die 'Unexpected subseq start argument at end of list with non-zero length' unless ((scalar(@length) and $length[0] == 0) or not(scalar(@length)))
		} else {
			die 'Unexpected subseq start argument past end of list' unless ($start < (1+scalar(@nodes)));
		}
		if (scalar(@length)) {
			my $end	= $start + $length[0];
			die 'Subseq start+length is beyond the end of the array' if ($end > (1+scalar(@nodes)));
		}
		my @orig		= @nodes;
		
		my $from	= $start-1;
		my $to		= scalar(@length) ? ($from + $length[0] - 1) : $#nodes;
		my @seq		= @nodes[$from .. $to];
		return list_to_lex(@seq);
	}

=item C<< listConcat(@lists) >>

=cut
	sub listConcat {
		my $model			= shift;
		my $active_graph	= shift;
		my @lists			= @_;
		my @nodes;
		foreach my $l (@lists) {
			die 'TypeError' unless ($l->does('Attean::API::Literal'));
			my $dt	= $l->datatype;
			die 'TypeError' unless ($dt->value eq $LIST_TYPE_IRI);
			push(@nodes, lex_to_list($l));
		}
		
		return list_to_lex(@nodes);
	}

=item C<< listReverse($list, $pos) >>

=cut
	sub listReverse {
		my $model			= shift;
		my $active_graph	= shift;
		my $l				= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError' unless ($dt->value eq $LIST_TYPE_IRI);
		my @nodes	= lex_to_list($l);
		return list_to_lex(reverse @nodes);
	}

=item C<< listHead($list, $pos) >>

=cut
	sub listHead {
		my $model			= shift;
		my $active_graph	= shift;
		my $l				= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError' unless ($dt->value eq $LIST_TYPE_IRI);
		my @nodes	= lex_to_list($l);
		return shift(@nodes);
	}

=item C<< listTail($list, $pos) >>

=cut
	sub listTail {
		my $model			= shift;
		my $active_graph	= shift;
		my $l				= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError' unless ($dt->value eq $LIST_TYPE_IRI);
		my @nodes	= lex_to_list($l);
		unless (scalar(@nodes)) {
			die 'cdt:tail called on an empty list';
		}
		shift(@nodes);
		return list_to_lex(@nodes);
	}

=item C<< listContains($list, $term) >>

=cut
	sub listContains {
		my $model			= shift;
		my $active_graph	= shift;
		my $l				= shift;
		my $term			= shift;
		die 'TypeError: Not a literal' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError: Not a cdt:List' unless ($dt->value eq $LIST_TYPE_IRI);
		my @nodes	= lex_to_list($l);
		
		foreach my $n (@nodes) {
			next unless (defined($n)); # null list elements cannot be tested for with CONTAINS
			my $equals	= eval { $n->equals($term) };
# 			warn $@ if ($@);
			return Attean::Literal->true if ($equals);
		}
		return Attean::Literal->false;
	}

=item C<< listContainsTerm($list, $term) >>

=cut
	sub listContainsTerm {
		my $model			= shift;
		my $active_graph	= shift;
		my $l				= shift;
		my $term			= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError' unless ($dt->value eq $LIST_TYPE_IRI);
		my @nodes	= lex_to_list($l);
		
		foreach my $n (@nodes) {
			next unless (defined($n)); # null list elements cannot be tested for with CONTAINSTERM
			if ($n->compare($term)) {
				next;
			}
			if ($n->does('Attean::API::Binding')) {
				return Attean::Literal->true if ($n->sameTerms($term));
			} else {
				return Attean::Literal->true if ($n->value eq $term->value);
			}
		}
		return Attean::Literal->false;
	}

=item C<< ctSize($ct) >>

=cut
	sub ctSize {
		my $model			= shift;
		my $active_graph	= shift;
		my $ct				= shift;
		my $pos				= shift;
		die 'TypeError' unless ($ct->does('Attean::API::Literal'));
		my $dt	= $ct->datatype;
		if ($dt->value eq $LIST_TYPE_IRI) {
			return listSize($model, $active_graph, $ct, $pos);
		} elsif ($dt->value eq $AtteanX::Functions::CompositeMaps::MAP_TYPE_IRI) {
			return AtteanX::Functions::CompositeMaps::mapSize($model, $active_graph, $ct, $pos);
		} else {
			die 'TypeError';
		}
	}

=item C<< listSize($list) >>

=cut
	sub listSize {
		my $model			= shift;
		my $active_graph	= shift;
		my $l		= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError' unless ($dt->value eq $LIST_TYPE_IRI);
		my @nodes	= lex_to_list($l);
		return Attean::Literal->integer(scalar(@nodes));
	}

=item C<< sequence($start, $end) >>

=cut
	sub sequence  {
		my $model			= shift;
		my $active_graph	= shift;
		my $start	= 1;
		my $end		= 1;
		if (scalar(@_) == 2) {
			$start		= shift->numeric_value;
			$end		= shift->numeric_value;
		} else {
			$end		= shift->numeric_value;
		}
		my @values	= ($start .. $end);
		return list_to_lex(map { Attean::Literal->integer($_) } @values);
	}

=item C<< zip($list, $list) >>

=cut
	sub zip {
		my $model			= shift;
		my $active_graph	= shift;
		my $lhs				= shift;
		my $rhs				= shift;
		my @lhs_nodes		= lex_to_list($lhs);
		my @rhs_nodes		= lex_to_list($rhs);
		die 'zip operands are not the same length' unless (scalar(@lhs_nodes) == scalar(@rhs_nodes));

		my @elements;
		while (scalar(@lhs_nodes)) {
			my @list	= (shift(@lhs_nodes), shift(@rhs_nodes));
			my $l		= list_to_lex(@list);
			push(@elements, $l);
		}
		return list_to_lex(@elements);
	}

=item C<< listCreate_agg_start() >>

=cut
	sub listCreate_agg_start {
		my $model			= shift;
		my $active_graph	= shift;
		return {
			values => []
		};
	}

=item C<< listCreate_agg_process($thunk, $term) >>

=cut
	sub listCreate_agg_process {
		my $thunk	= shift;
		my ($term)	= @_;
		push(@{ $thunk->{'values' }}, $term);
	}

=item C<< listCreate_agg_finalize($thunk) >>

=cut
	sub listCreate_agg_finalize {
		my $thunk	= shift;
		my @terms	= @{ $thunk->{'values' }};
		return list_to_lex(@terms);
	}

=item C<< list_from_head($head) >>

=cut
	sub list_from_head {
		my $model			= shift;
		my $active_graph	= shift;
		my $head			= shift;
		my $list			= $model->get_list($active_graph, $head);
		return list_to_lex($list->elements);
	}

=item C<< rewrite_lexical( $literal, \%bnode_map, $parse_id ) >>

=cut

	# lexer-based rewrite that will preserve the lexical representation of the literal for everything but the blank nodes
	sub rewrite_lexical {
		my $term		= shift;
		my $bnode_map	= shift;
		my $parse_id	= shift;
		my %bnode_map	= %{ $bnode_map };
		my $rewrite_tokens	= sub {
			my $l	= shift;
			my $t	= shift;
			return $t unless (blessed($t));
			if ($t->type == BNODE) {
				my $v	= $t->value;
				if (my $b = $bnode_map{$v}) {
					return $l->new_token(BNODE, $t->start_line, $t->start_column, $b->value);
				} else {
					my $value	= 'b' . sha1_hex($parse_id . $v);
					my $b		= Attean::Blank->new(value => $value);
					$bnode_map{$value}	= $b;
					return $l->new_token(BNODE, $t->start_line, $t->start_column, $b->value);
				}
			} else {
				return $t;
			}
		};
		my $r			= eval {
			my %seen_hathat;
			my %seen_cdt_iri;
			my %seen_cdt_list_iri;
			my %seen_cdt_map_iri;
			my $lex			= $term->value;
			open(my $fh, '<:encoding(UTF-8)', \$lex);
			my $lexer	= AtteanX::Functions::CompositeLists::TurtleLexerWithNull->new(file => $fh, ignore_whitespace => 0);
			my $p		= AtteanX::Parser::Turtle->new();
			my @rewritten_tokens;
			my $i	= 0;
			while (my $t = $rewrite_tokens->($lexer, $lexer->get_token())) {
				if (blessed($t)) {
					if ($t->type == HATHAT) {
						$seen_hathat{$i-1}++;
					} elsif ($t->type == IRI) {
						if ($t->value eq 'http://w3id.org/awslabs/neptune/SPARQL-CDTs/Map') {
							$seen_cdt_map_iri{$i-2}++;
							$seen_cdt_iri{$i-2}++;
						} elsif ($t->value eq 'http://w3id.org/awslabs/neptune/SPARQL-CDTs/List') {
							$seen_cdt_list_iri{$i-2}++;
							$seen_cdt_iri{$i-2}++;
						}
					}
				}
				push(@rewritten_tokens, $t);
				$i++;
			}
			
			my @rewritten;
			my $j	= 0;
			my $s		= AtteanX::Serializer::SPARQL->new();
			foreach my $t (@rewritten_tokens) {
				if (blessed($t) and $t->is_string and $seen_hathat{$j} and $seen_cdt_iri{$j}) {
					my $ct_type	= $seen_cdt_list_iri{$j}
								? 'http://w3id.org/awslabs/neptune/SPARQL-CDTs/List'
								: 'http://w3id.org/awslabs/neptune/SPARQL-CDTs/Map';
					my $literal	= Attean::Literal->new(value => $t->value, datatype => $ct_type);
					my $rewritten	= AtteanX::Functions::CompositeLists::rewrite_lexical($literal, $bnode_map, $parse_id);
					my ($t)	= $rewritten->sparql_tokens->elements;
					my $i	= Attean::ListIterator->new( values => [$t], item_type => 'AtteanX::SPARQL::Token' );
					my $str	= decode_utf8($s->serialize_iter_to_bytes($i));
					chomp($str);
					push(@rewritten, $str);
					$j++;
					next;
				}
				push(@rewritten, blessed($t) ? $t->token_as_string() : "null");
				$j++;
			}
			my $rewritten	= join('', @rewritten);
			Attean::Literal->new(value => $rewritten, datatype => $term->datatype);
		};
		warn $@ if ($@);
		return $r || $term;
	}

=item C<< register() >>

=cut
	sub register {
		Attean->register_datatype_role(
			"${CDT_BASE}List"	=> 'AtteanX::Functions::CompositeLists::ListLiteral'
		);
		
		Attean->register_global_functional_form(
			"${CDT_BASE}List" => \&listCreate,
			"${CDT_BASE}listCreate" => \&listCreate,
		);
		Attean->register_global_function(
			"${CDT_BASE}get" => \&ctGet,
			"${CDT_BASE}listGet" => \&listGet,
			"${CDT_BASE}subseq" => \&listSubseq,
			"${CDT_BASE}size" => \&ctSize,
			"${CDT_BASE}listSize" => \&listSize,
			"${CDT_BASE}reverse" => \&listReverse,
			"${CDT_BASE}head" => \&listHead,
			"${CDT_BASE}tail" => \&listTail,
			"${CDT_BASE}contains" => \&listContains,
			"${CDT_BASE}concat" => \&listConcat,
			"${CDT_BASE}containsTerm" => \&listContainsTerm,
			"${CDT_BASE}sequence" => \&sequence,
			"${CDT_BASE}zip" => \&zip,
			"${CDT_BASE}split" => \&stringSplit,
			"${CDT_BASE}list_from_head" => \&list_from_head,
		);

		Attean->register_global_aggregate(
			"${CDT_BASE}listAgg" => {
				start		=> \&listCreate_agg_start,
				process		=> \&listCreate_agg_process,
				finalize	=> \&listCreate_agg_finalize,
			},
		);
	}
}

package AtteanX::Functions::CompositeLists::ListLiteral {
	use Scalar::Util qw(blessed looks_like_number);

	use Moo::Role;
	use List::Util qw(min);

	sub equals {
		my $lhs	= shift;
		my $rhs	= shift;
# 		warn "LIST EQUALS?";
# 		warn "- " . $lhs->as_string . "\n";
# 		warn "- " . $rhs->as_string . "\n";
		return 0 unless ($rhs->does('Attean::API::Literal') and $rhs->datatype->value eq $AtteanX::Functions::CompositeLists::LIST_TYPE_IRI);
		my $lhs_size	= eval { AtteanX::Functions::CompositeLists::listSize(undef, undef, $lhs)->value };
		return 0 if ($@);
 		my $rhs_size	= eval { AtteanX::Functions::CompositeLists::listSize(undef, undef, $rhs)->value };
		return 0 if ($@);
		return 0 unless ($lhs_size == $rhs_size);
		
		my $seen_error	= 0;
		foreach my $i (0 .. $lhs_size-1) {
			my $li	= AtteanX::Functions::CompositeLists::listGet(undef, undef, $lhs, Attean::Literal->integer($i+1));
			my $ri	= AtteanX::Functions::CompositeLists::listGet(undef, undef, $rhs, Attean::Literal->integer($i+1));
			if (not blessed($li) and not blessed($ri)) {
				# both null
				next;
			} elsif (not blessed($li) or not blessed($ri)) {
				return 0;
			}

			if ($li->does('Attean::API::Blank') and $ri->does('Attean::API::Blank')) {
				if ($li->value eq $ri->value) {
					next;
				} else {
					$seen_error++;
					next;
				}
			}
			return 0 unless ($li->equals($ri));
		}
		if ($seen_error) {
			die 'TypeError: Cannot compare cdt:List values with blank nodes';
		}
		return 1;
	}
	
	sub order {
		my $self	= shift;
		return _compare('order', $self, @_);
	}
	
	sub compare {
		my $self	= shift;
		return _compare('compare', $self, @_);
	}
	
	sub _compare {
		my $cmp_method	= shift;
		my $lhs	= shift;
		my $rhs	= shift;
# 		warn "LIST-LESS-THAN?";
# 		warn "- " . $lhs->as_string . "\n";
# 		warn "- " . $rhs->as_string . "\n";
		die 'TypeError' unless (blessed($rhs) and $rhs->does('Attean::API::Literal') and $rhs->datatype->value eq $AtteanX::Functions::CompositeLists::LIST_TYPE_IRI);
		
		my @lhs	= AtteanX::Functions::CompositeLists::lex_to_list($lhs);
		my @rhs	= AtteanX::Functions::CompositeLists::lex_to_list($rhs);
		
		my $lhs_size	= scalar(@lhs);
 		my $rhs_size	= scalar(@rhs);

		my $seen_error	= 0;
		my $length	= min($lhs_size, $rhs_size);
		foreach my $i (0 .. $length-1) {
			my $li	= AtteanX::Functions::CompositeLists::listGet(undef, undef, $lhs, Attean::Literal->integer($i+1));
			my $ri	= AtteanX::Functions::CompositeLists::listGet(undef, undef, $rhs, Attean::Literal->integer($i+1));
			
			if (not blessed($li) and not blessed($ri)) {
				# both null
				next;
			} elsif (not blessed($li)) {
				if ($cmp_method eq 'order') {
					return -1;
				} else {
					die 'TypeError';
					$seen_error++;
					next;
				}
			} elsif (not blessed($ri)) {
				if ($cmp_method eq 'order') {
					return 1;
				} else {
					die 'TypeError';
					$seen_error++;
					next;
				}
			}

			if ($li->does('Attean::API::Blank') and $ri->does('Attean::API::Blank')) {
				die 'TypeError';
				$seen_error++;
				next;
			}
			
			my $icmp	= $li->$cmp_method($ri);
			next if ($icmp == 0);
			return $icmp;
		}
		
		if ($seen_error) {
			die 'TypeError';
		}
		
		if ($lhs_size == $rhs_size) {
			return 0;
		} else {
			return ($lhs_size > $rhs_size) ? 1 : -1;
		}
	}

	sub canonicalized_term {
		my $self	= shift;
		return $self->canonicalized_term_strict();
	}

	sub canonicalized_term_strict {
		my $self	= shift;
		my @values	= AtteanX::Functions::CompositeLists::lex_to_list($self);
		return AtteanX::Functions::CompositeLists::list_to_lex(@values);
	}

	with 'Attean::API::Literal';
	with 'Attean::API::CanonicalizingLiteral';
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

Copyright (c) 2014--2022 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
