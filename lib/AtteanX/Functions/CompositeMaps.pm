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

			if ($self->buffer =~ /^[ \r\n\t]+/o) {
				$self->read_length($+[0]);
				# we're ignoring whitespace tokens, but we could return them here instead of falling through to the 'next':
	# 			return $self->new_token(WS);
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

package AtteanX::Functions::CompositeMaps 0.032 {

	use Attean;
	use Attean::RDF;
	use Encode qw(decode_utf8);
	use Scalar::Util qw(blessed);
	use AtteanX::Serializer::TurtleTokens;
	use AtteanX::Parser::Turtle;
	use AtteanX::SPARQL::Constants;
	
	our $CDT_BASE		= 'http://example.org/cdt/';
	our $MAP_TYPE_IRI	= "${CDT_BASE}Map";

	sub _recursive_lexer_map_parse {
		my $p		= shift;
		my $lexer	= shift;
		my @nodes;
		while (my $t = $p->_next_nonws($lexer)) {
			if ($t and not blessed($t)) {
				# this is the special value returned from our lexer subclass that indicates a null values
				push(@nodes, undef);
			} else {
				next if ($t->type == COMMA);
				next if ($t->type == PREFIXNAME and $t->value eq ':'); # COLON
				
				if ($t->type == LBRACE) {
					my $hash		= _recursive_lexer_map_parse($p, $lexer);
					push(@nodes, AtteanX::Functions::CompositeMaps::map_to_lex(%$hash));
				} elsif ($t->type == RBRACE) {
					my %hash;
					while (my ($k, $v) = splice(@nodes, 0, 2)) {
						$hash{ $k->as_string }	= $v;
					}
					return \%hash;
				} elsif ($t->type == LBRACKET) {
					my $subnodes	= _recursive_lexer_map_parse($p, $lexer);
					push(@nodes, [AtteanX::Functions::CompositeLists::list_to_lex(@$subnodes)]);
				} elsif ($t->type == RBRACKET) {
					return \@nodes;
				} else {
					push(@nodes, $p->_object($lexer, $t));
				}
			}
		}

		die 'unexpected end of map literal lexical form';
	}

=item C<< lex_to_map($literal) >>

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
# 		$p->_map->{''}	= Attean::IRI->new($MAP_TYPE_IRI);
		my $lexer	= AtteanX::Functions::CompositeMaps::TurtleLexerWithNull->new(file => $fh);
		my @nodes;
		eval {
			my $t = $p->_next_nonws($lexer);
			if ($t->type == LBRACE) {
				my $hash	= _recursive_lexer_map_parse($p, $lexer);
    			push(@nodes, %$hash);
			}
		};
		warn $@ if ($@);

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
		
		while (my ($key, $value) = splice(@terms, 0, 2)) {
			unless ($first) {
				my @tokens;
				push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(COMMA, -1, -1, -1, -1, [',']));
				push(@tokens, AtteanX::Parser::Turtle::Token->fast_constructor(WS, -1, -1, -1, -1, [' ']));
				my $iter	= Attean::ListIterator->new( values => \@tokens, item_type => 'AtteanX::Parser::Turtle::Token' );
				$s->serialize_iter_to_io($io, $iter);
			}
			$first	= 0;
			print {$io} $key;
			
			my @tokens;
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
		my $literal			= eval { map_to_lex(@_) };
		warn "cdt:Map constructor error: $@" if $@;
		return $literal;
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
		my $value	= $nodes{$key->as_string};
		return $value;
	}

=item C<< mapSize($list) >>

=cut
	sub mapSize {
		my $model			= shift;
		my $active_graph	= shift;
		my $l		= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError' unless ($dt->value eq $MAP_TYPE_IRI);
		my %nodes	= lex_to_map($l);
		my @keys	= keys(%nodes);
		return Attean::Literal->integer(scalar(@keys));
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
		);
		Attean->register_global_function(
			"${CDT_BASE}mapGet" => \&mapGet,
			"${CDT_BASE}mapSize" => \&mapSize,
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
		
		foreach my $i (0 .. $lhs_size-1) {
			return 0 unless ($lhs_keys[$i] eq $rhs_keys[$i]);
			my $key	= $lhs_keys[$i];
			my $lv	= $lhs_map{$key};
			my $rv	= $rhs_map{$key};
			unless (blessed($lv) and blessed($rv)) {
				die 'TypeError';
			}
			return 0 unless ($lv->equals($rv));
		}
		return 1;
	}
	
# 	sub compare {
# 		my ($a, $b)	= @_;
# 		return 1 unless blessed($b);
# 		return 1 unless ($b->does('Attean::API::Literal') or $b->does('Attean::API::Binding'));
# 		return -1 if ($b->does('Attean::API::Binding'));
# 		if ($b->does('Attean::API::NumericLiteral')) {
# 			return $a->numeric_value <=> $b->numeric_value;
# 		} else {
# 			return 1;
# # 			Attean::API::Literal::compare($a, $b);
# 		}
# 	}
# 
# 	sub canonicalized_term {
# 		my $self	= shift;
# 		my $value	= $self->value;
# 		if ($value =~ m/^(true|false|0|1)$/) {
# 			return ($value eq 'true' or $value eq '1')
# 				? Attean::Literal->true
# 				: Attean::Literal->false;
# 		} else {
# 			die "Bad lexical form for xsd:boolean: '$value'";
# 		}
# 	}
	with 'Attean::API::Literal';
# 	with 'Attean::API::CanonicalizingLiteral';
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
