use v5.14;
use warnings;
use utf8;

=head1 NAME

AtteanX::Functions::CompositeMaps - Functions and aggregates to work with composite maps

=head1 VERSION

This document describes AtteanX::Functions::CompositeMaps version 0.032

=head1 SYNOPSIS

  use v5.14;
  use Attean;

=head1 DESCRIPTION

This is a utility package that defines functions and aggregates to work with
composite map datatypes.

=over 4

=cut

package AtteanX::Functions::CompositeMaps::TurtleLexerWithNull {
	use Moo;
	use AtteanX::Serializer::TurtleTokens;
	use AtteanX::Parser::Turtle;
	use AtteanX::SPARQL::Constants;
	extends 'AtteanX::Parser::Turtle::Lexer';

	sub get_token {
		my $self	= shift;
		while (1) {
			$self->fill_buffer unless (length($self->buffer));

			my $start_column	= $self->column;
			my $start_line		= $self->line;

			if ($self->buffer =~ /^[ \r\n\t]+/o) {
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
			} elsif ($self->buffer =~ /^(true|false)\b/) {
				my $bool	= $self->read_length($+[0]);
				return $self->new_token(BOOLEAN, $start_line, $start_column, $bool);
			}
			return $self->SUPER::get_token();
		}
	}
}

package AtteanX::Functions::CompositeMaps 0.032 {

	use Attean;
	use Attean::RDF;
	use Encode qw(decode_utf8);
	use Scalar::Util qw(blessed);
	use Digest::SHA qw(sha1_hex);
	use AtteanX::Serializer::TurtleTokens;
	use AtteanX::Parser::Turtle;
	use AtteanX::SPARQL::Constants;
	use AtteanX::Functions::CompositeLists;
	
	our $CDT_BASE		= 'http://w3id.org/awslabs/neptune/SPARQL-CDTs/';
	our $MAP_TYPE_IRI	= "${CDT_BASE}Map";

	# Assume the opening token of the cdt has already been consumed.
	# Return either a HASH or ARRAY reference, depending on the closing token.
	# Does not validate the lexical form with respect to balanced cdt tokens.
	sub _recursive_lexer_parse_cdt {
		my $p		= shift;
		my $lexer	= shift;
		my @nodes;
		my $s		= AtteanX::Serializer::TurtleTokens->new( suppress_whitespace => 1 );
		while (my $t = $p->_next_nonws($lexer)) {
			if ($t and not blessed($t)) {
				# this is the special value returned from our lexer subclass that indicates a null values
				push(@nodes, undef);
			} else {
				next if ($t->type == COMMA);
				next if ($t->type == PREFIXNAME and $t->value eq ':'); # COLON
				
				if ($t->type == LBRACE) {
					my $hash		= _recursive_lexer_parse_cdt($p, $lexer);
					push(@nodes, AtteanX::Functions::CompositeMaps::map_to_lex(%$hash));
				} elsif ($t->type == RBRACE) {
					my %hash;
					die "odd number of map elements" unless (scalar(@nodes) % 2 == 0);
					while (my ($k, $v) = splice(@nodes, 0, 2)) {
						my @tokens;
						push(@tokens, $k->sparql_tokens->elements);
						my $iter	= Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::Parser::Turtle::Token' );
						my $bytes	= $s->serialize_iter_to_bytes($iter);
						my $key_string	= decode_utf8($bytes);
						
						$hash{ $key_string }	= $v;
					}
					return \%hash;
				} elsif ($t->type == LBRACKET) {
					my $subnodes	= _recursive_lexer_parse_cdt($p, $lexer);
					push(@nodes, AtteanX::Functions::CompositeLists::list_to_lex(@$subnodes));
				} elsif ($t->type == RBRACKET) {
					return \@nodes;
				} else {
					my $t	= $p->_object($lexer, $t);
					push(@nodes, $t);
				}
			}
		}

		die 'unexpected end of map literal lexical form';
	}

=item C<< lex_to_map($literal) >>

Parses $literal as a cdt:Map value and returns a hash of stringified keys and
term object values. Use C<< lex_to_maplist >> to get a list of key-value pairs
in which the keys are also term objects.

=cut
	sub lex_to_map {
		my $l	= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError: not a datatype literal' unless ($dt);
		die 'TypeError: Expecting a Map but found '  . $dt->value unless ($dt->value eq $MAP_TYPE_IRI);
		my $lex	= $l->value;
		$lex	=~ s/^\s*//g;
		$lex	=~ s/\s*$//g;
		
		unless ($lex =~ m<^\{(.*)\}$>) {
			die 'TypeError: Invalid lexical form for cdt:Map literal: '  . $dt->value;
		}
		
		open(my $fh, '<:encoding(UTF-8)', \$lex);
		my $p		= AtteanX::Parser::Turtle->new();
		local($p->{enable_cdt_rewriting})	= 0;

# 		$p->_map->{''}	= Attean::IRI->new($MAP_TYPE_IRI);
		my $lexer	= AtteanX::Functions::CompositeMaps::TurtleLexerWithNull->new(file => $fh);
		my @nodes;
		my $t = $p->_next_nonws($lexer);
		if ($t->type == LBRACE) {
			my $hash	= _recursive_lexer_parse_cdt($p, $lexer);
			push(@nodes, %$hash);
		}

		return @nodes;
	}

=item C<< map_to_lex(@terms) >>

=cut
	sub map_to_lex {

		my @terms	= @_;
		my $s		= AtteanX::Serializer::TurtleTokens->new( suppress_whitespace => 1 );
		my $bytes	= '';
		open(my $io, '>', \$bytes);
		my $first	= 1;
		
		my $p		= AtteanX::Parser::Turtle->new();
		local($p->{enable_cdt_rewriting})	= 0;

		while (my ($key_string, $value) = splice(@terms, 0, 2)) {
			unless ($first) {
				my @tokens;
				push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(COMMA, -1, -1, -1, -1, [',']));
				push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(WS, -1, -1, -1, -1, [' ']));
				my $iter	= Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::Parser::Turtle::Token' );
				$s->serialize_iter_to_io($io, $iter);
			}
			$first	= 0;

			my @tokens;
			my $key	= $p->parse_node($key_string);

			push(@tokens, $key->sparql_tokens->elements);
			push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(WS, -1, -1, -1, -1, [' ']));

    		push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(PREFIXNAME, -1, -1, -1, -1, [':']));
			if (blessed($value)) {
				push(@tokens, $value->sparql_tokens->elements);
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
		return dtliteral("{${str}}", $MAP_TYPE_IRI);
	}

=item C<< mapCreate(@list) >>

=cut
	sub mapCreate {
		my $model			= shift;
		my $active_graph	= shift;
		my @map;
		my $s		= AtteanX::Serializer::TurtleTokens->new( suppress_whitespace => 1 );
		while (my ($key, $value) = splice(@_, 0, 2)) {
			next unless (is_valid_map_key($key));
			my @tokens	= $key->sparql_tokens->elements;
			my $iter	= Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::Parser::Turtle::Token' );
			my $bytes	= $s->serialize_iter_to_bytes($iter);
			my $key_string	= decode_utf8($bytes);
			push(@map, $key_string, $value);
# 			$map{$key_string}	= $value;
		}
		my $literal			= eval { map_to_lex(@map) };
		warn "cdt:Map constructor error: $@" if $@;
		return $literal;
	}

=item C<< is_valid_map_key( $value ) >>

Returns true if $value is a valid map key (is an IRI or a literal), false otherwise.

=cut
	sub is_valid_map_key {
		my $key	= shift;
		return 0 unless (blessed($key));
		return 1 if ($key->does('Attean::API::IRI'));
		return 1 if ($key->does('Attean::API::Literal'));
		return 0;
	}

	sub _map_key_string {
		my $key		= shift;
		my $s		= AtteanX::Serializer::TurtleTokens->new( suppress_whitespace => 1 );
		my @tokens	= $key->sparql_tokens->elements;
		my $iter	= Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::Parser::Turtle::Token' );
		my $bytes	= $s->serialize_iter_to_bytes($iter);
		my $key_string	= decode_utf8($bytes);
		return $key_string;
	}

=item C<< mapGet($list, $key) >>

=cut
	sub mapGet {
		my $model			= shift;
		my $active_graph	= shift;
		my $l				= shift;
		my $key				= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError' unless ($dt->value eq $MAP_TYPE_IRI);
		my %nodes	= lex_to_map($l);

		my $key_string	= _map_key_string($key);
		my $value	= $nodes{$key_string};
		return $value;
	}

=item C<< mapKeys($map) >>

=cut
	sub mapKeys {
		my $model			= shift;
		my $active_graph	= shift;
		my $map				= shift;
		die 'TypeError' unless ($map->does('Attean::API::Literal'));
		my $dt	= $map->datatype;
		die 'TypeError' unless ($dt->value eq $MAP_TYPE_IRI);
		my %nodes	= lex_to_map($map);
		my @key_strings	= keys %nodes;

		my $p		= AtteanX::Parser::Turtle->new();
		local($p->{enable_cdt_rewriting})	= 0;

# 		$p->_map->{''}	= Attean::IRI->new($MAP_TYPE_IRI);
		my @nodes	= map {
			open(my $fh, '<:encoding(UTF-8)', \$_);
			my $lexer	= AtteanX::Functions::CompositeMaps::TurtleLexerWithNull->new(file => $fh);
			my $token	= $p->_next_nonws($lexer);
			$p->_object($lexer, $token);
		} @key_strings;
		AtteanX::Functions::CompositeLists::list_to_lex(@nodes);
	}

=item C<< map_key_to_term( @keys ) >>

Converts each argument from the stringified version of map keys used as hash
keys to a list of term objects.

=cut
	sub map_key_to_term {
		my @keys	= @_;
		my $p		= AtteanX::Parser::Turtle->new();
		local($p->{enable_cdt_rewriting})	= 0;

		my @terms	= map {
			open(my $fh, '<:encoding(UTF-8)', \$_);
			my $lexer	= AtteanX::Functions::CompositeMaps::TurtleLexerWithNull->new(file => $fh);
			my $token	= $p->_next_nonws($lexer);
			$p->_object($lexer, $token);
		} @keys;
		
		return wantarray ? @terms : $terms[0];
	}
	
=item C<< lex_to_maplist() >>

Parses $literal as a cdt:Map value and returns a (flattened) list of key-value
pairs of term values.

=cut
	sub lex_to_maplist {
		my %map		= lex_to_map(@_);
		my @key_strings	= keys %map;
		my @values		= values %map;
		my $p		= AtteanX::Parser::Turtle->new();
		local($p->{enable_cdt_rewriting})	= 0;

		my @keys	= map {
			open(my $fh, '<:encoding(UTF-8)', \$_);
			my $lexer	= AtteanX::Functions::CompositeMaps::TurtleLexerWithNull->new(file => $fh);
			my $token	= $p->_next_nonws($lexer);
			$p->_object($lexer, $token);
		} @key_strings;
		my @list;
		foreach my $i (0 .. $#keys) {
			push(@list, $keys[$i]);
			push(@list, $values[$i]);
		}
		return @list;
	}

=item C<< mapPut($map, $key, $value) >>

=cut
	sub mapPut {
		my $model			= shift;
		my $active_graph	= shift;
		my $map				= shift;
		my $key				= shift;
		die 'TypeError' unless (is_valid_map_key($key));
		my $value			= shift;
		die 'TypeError' unless (blessed($map) and $map->does('Attean::API::Literal'));
		my $dt	= $map->datatype;
		die 'TypeError' unless ($dt->value eq $MAP_TYPE_IRI);
		my %nodes	= lex_to_map($map);
		my @key_strings	= keys %nodes;

		my $key_string	= _map_key_string($key);
		$nodes{ $key_string }	= $value;

		return map_to_lex(%nodes);
	}

=item C<< mapRemove($map, $key) >>

=cut
	sub mapRemove {
		my $model			= shift;
		my $active_graph	= shift;
		my $map				= shift;
		my $key				= shift;
		die 'TypeError' unless (blessed($map) and $map->does('Attean::API::Literal'));
		my $dt	= $map->datatype;
		die 'TypeError' unless ($dt->value eq $MAP_TYPE_IRI);
		my %nodes	= lex_to_map($map);
		my @key_strings	= keys %nodes;

		unless (is_valid_map_key($key)) {
			return $map;
		}

		my $key_string	= _map_key_string($key);
		delete $nodes{ $key_string };

		return map_to_lex(%nodes);
	}

=item C<< mapSize($list) >>

=cut
	sub mapSize {
		my $model			= shift;
		my $active_graph	= shift;
		my $l		= shift;
		die 'TypeError' unless (blessed($l) and $l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError' unless ($dt->value eq $MAP_TYPE_IRI);
		my %nodes	= lex_to_map($l);
		my @keys	= keys(%nodes);
		return Attean::Literal->integer(scalar(@keys));
	}

=item C<< mapContains($map, $term) >>

=cut
	sub mapContains {
		my $model			= shift;
		my $active_graph	= shift;
		my $l				= shift;
		my $term			= shift;
		die 'TypeError: Not a literal' unless (blessed($l) and $l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError: Not a cdt:List' unless ($dt->value eq $MAP_TYPE_IRI);
		my %nodes	= lex_to_map($l);

		my $s		= AtteanX::Serializer::TurtleTokens->new( suppress_whitespace => 1 );
		my @tokens	= $term->sparql_tokens->elements;
		my $iter	= Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::Parser::Turtle::Token' );
		my $bytes	= $s->serialize_iter_to_bytes($iter);
		my $key_string	= decode_utf8($bytes);
		
		return (exists $nodes{ $key_string }) ? Attean::Literal->true : Attean::Literal->false;
	}

=item C<< mapMerge($map1, $map2) >>

=cut
	sub mapMerge {
		my $model			= shift;
		my $active_graph	= shift;
		my $map1			= shift;
		my $map2			= shift;
		my %nodes1	= lex_to_map($map1);
		my %nodes2	= lex_to_map($map2);
		my %merged	= (%nodes2, %nodes1);
		my $merged	= map_to_lex(%merged);
		return $merged;
	}

=item C<< mapCreate_agg_start() >>

=cut
	sub mapCreate_agg_start {
		my $model			= shift;
		my $active_graph	= shift;
		return {
			values => {}
		};
	}

=item C<< mapCreate_agg_process($thunk, $key, $value) >>

=cut
	sub mapCreate_agg_process {
		my $thunk	= shift;
		my ($key)	= shift;
		my $value	= shift;
		$thunk->{'values'}{$key->value}	= $value;
	}

=item C<< mapCreate_agg_finalize($thunk) >>

=cut
	sub mapCreate_agg_finalize {
		my $thunk	= shift;
		my %terms	= @{ $thunk->{'values' }};
		return map_to_lex(%terms);
	}

=item C<< register() >>

=cut
	sub register {
		Attean->register_datatype_role(
			$MAP_TYPE_IRI	=> 'AtteanX::Functions::CompositeMaps::MapLiteral'
		);
		
		Attean->register_global_functional_form(
			"${CDT_BASE}Map" => \&mapCreate,
			"${CDT_BASE}mapCreate" => \&mapCreate,
			"${CDT_BASE}put" => \&mapPut,
		);
		Attean->register_global_function(
			"${CDT_BASE}mapGet" => \&mapGet,
			"${CDT_BASE}mapSize" => \&mapSize,
			"${CDT_BASE}keys" => \&mapKeys,
			"${CDT_BASE}remove" => \&mapRemove,
			"${CDT_BASE}containsKey" => \&mapContains,
			"${CDT_BASE}merge" => \&mapMerge,
		);

		Attean->register_global_aggregate(
			"${CDT_BASE}mapAgg" => {
				start		=> \&mapCreate_agg_start,
				process		=> \&mapCreate_agg_process,
				finalize	=> \&mapCreate_agg_finalize,
			},
		);
	}
}

package AtteanX::Functions::CompositeMaps::MapLiteral {
	use Scalar::Util qw(blessed looks_like_number);
	use List::Util qw(min);

	use Moo::Role;

	sub equals {
		my $lhs	= shift;
		my $rhs	= shift;
# 		warn "MAP EQUALS?";
# 		warn "- " . $lhs->as_string . "\n";
# 		warn "- " . $rhs->as_string . "\n";
		return 0 unless ($rhs->does('Attean::API::Literal') and $rhs->datatype->value eq $AtteanX::Functions::CompositeMaps::MAP_TYPE_IRI);
		my $lhs_size	= eval { AtteanX::Functions::CompositeMaps::mapSize(undef, undef, $lhs)->value };
		return 0 if ($@);
 		my $rhs_size	= eval { AtteanX::Functions::CompositeMaps::mapSize(undef, undef, $rhs)->value };
		return 0 if ($@);
		return 0 unless ($lhs_size == $rhs_size);
		
		my %lhs_map	= AtteanX::Functions::CompositeMaps::lex_to_map($lhs);
		my %rhs_map	= AtteanX::Functions::CompositeMaps::lex_to_map($rhs);
		
		my @lhs_keys	= sort(keys %lhs_map);
		my @rhs_keys	= sort(keys %rhs_map);
		
		# TODO: handle differing lexical forms for map keys here:

		my $seen_error	= 0;
		foreach my $i (0 .. $lhs_size-1) {
			return 0 unless ($lhs_keys[$i] eq $rhs_keys[$i]);
			my $key	= $lhs_keys[$i];
			my $lv	= $lhs_map{$key};
			my $rv	= $rhs_map{$key};
			if (not blessed($lv) and not blessed($rv)) {
				# both null
				next;
			} elsif (not blessed($lv) or not blessed($rv)) {
				return 0;
			}

			if ($lv->does('Attean::API::Blank') and $rv->does('Attean::API::Blank')) {
				if ($lv->value eq $rv->value) {
					next;
				} else {
					$seen_error++;
					next;
				}
			}

			return 0 unless ($lv->equals($rv));
		}
		if ($seen_error) {
			die 'TypeError';
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
# 		warn "MAP-LESS-THAN?";
# 		warn "- " . $lhs->as_string . "\n";
# 		warn "- " . $rhs->as_string . "\n";
		die 'TypeError' unless (blessed($rhs) and $rhs->does('Attean::API::Literal') and $rhs->datatype->value eq $AtteanX::Functions::CompositeMaps::MAP_TYPE_IRI);
		
		my %lhs	= AtteanX::Functions::CompositeMaps::lex_to_map($lhs);
		my %rhs	= AtteanX::Functions::CompositeMaps::lex_to_map($rhs);
		
		my $lhs_size	= scalar(%lhs);
 		my $rhs_size	= scalar(%rhs);
 		
 		if (not($lhs_size) and not($rhs_size)) {
 			return 0; # empty maps are trivially eq
 		}
 		
 		
 		if (scalar(%lhs) == 0 and scalar(%rhs) == 0) {
 			return 0;
 		}

 		my @lhs_keys	= sort_map_keys(keys %lhs);
 		my @rhs_keys	= sort_map_keys(keys %rhs);
 		
		my $length	= min($lhs_size, $rhs_size);
		foreach my $i (0 .. $length-1) {
			my $k1	= AtteanX::Functions::CompositeMaps::map_key_to_term($lhs_keys[$i]);
			my $k2	= AtteanX::Functions::CompositeMaps::map_key_to_term($rhs_keys[$i]);
			
			my $same	= sameterm($k1, $k2);
			if (not($same)) {
				# the keys are not the same
				my @sorted	= sort_map_keys($lhs_keys[$i], $rhs_keys[$i]);
				if ($sorted[0] eq $lhs_keys[$i]) { # k1 is ordered before k2 according tot he map key sorting
					return -1; # less than
				} else {
					return 1; # greater than
				}
			}
			
			my $v1	= $lhs{$lhs_keys[$i]};
			my $v2	= $rhs{$rhs_keys[$i]};

			if (not blessed($v1) and not blessed($v2)) {
				# both null
				next;
			} elsif (not blessed($v1)) {
				if ($cmp_method eq 'order') {
					return -1;
				} else {
					die 'TypeError';
				}
			} elsif (not blessed($v2)) {
				if ($cmp_method eq 'order') {
					return 1;
				} else {
					die 'TypeError';
				}
			}
			
			if ($cmp_method eq 'compare') {
				foreach my $v ($v1, $v2) {
					if ($v->does('Attean::API::IRI')) {
						die 'TypeError'; # IRIs as map values cannot be compared
					}
				}
			}
			
			my $v_cmp	= $v1->$cmp_method($v2); # may throw an error
			if ($v_cmp) {
				return $v_cmp;
			}
		}
		
		return ($lhs_size - $rhs_size); # sort smaller maps ahead of larger maps
	}
	
	sub sameterm {
		# shared code with SAMETERM handling in SimpleQueryEvaluator. Consider refactoring.
		my $a	= shift;
		my $b	= shift;
		my $cmp = eval { $a->compare($b) };
		if (not($@) and $cmp) {
			return 0;
		}
		if ($a->does('Attean::API::Binding')) {
			my $ok	= ($a->sameTerms($b));
			return $ok;
		} else {
			my $ok	= ($a->value eq $b->value);
			return $ok;
		}
	}
	
	sub sort_map_keys {
		my @keys	= @_;
		my @terms	= AtteanX::Functions::CompositeMaps::map_key_to_term(@keys);
		my @iri_i;
		my @other_i;
		foreach my $i (0 .. $#terms) {
			if ($terms[$i]->does('Attean::API::IRI')) {
				push(@iri_i, $i);
			} else {
				push(@other_i, $i);
			}
		}
		@iri_i	= sort {
			my $at		= $terms[$a];
			my $bt		= $terms[$b];
			$at->compare($bt)
		} @iri_i;
		@other_i	= sort {
			my $at		= $terms[$a];
			my $bt		= $terms[$b];
			my $a_value	= $at->value;
			my $a_dt	= $at->datatype->value;
			my $a_lang	= $at->language // '';
			my $b_value	= $bt->value;
			my $b_dt	= $bt->datatype->value;
			my $b_lang	= $bt->language // '';
			
			if (my $cdt = ($a_dt cmp $b_dt)) {
				return $cdt;
			}
			if (my $cval = ($a_value cmp $b_value)) {
				return $cval;
			}
			return $a_lang cmp $b_lang;
		} @other_i;
		
		return map { $keys[$_] } (@iri_i, @other_i);
	}

	sub canonicalized_term {
		my $self	= shift;
		return $self->canonicalized_term_strict();
	}

	sub canonicalized_term_strict {
		my $self	= shift;
		my %values	= AtteanX::Functions::CompositeMaps::lex_to_map($self);
		my @keys	= sort keys %values;
		my @values	= map { $_ => $values{$_} } @keys;
		return AtteanX::Functions::CompositeMaps::map_to_lex(@values);
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
