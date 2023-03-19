use v5.14;
use warnings;
use utf8;

=head1 NAME

AtteanX::Functions::CompositeLists - Functions and aggregates to work with composite lists

=head1 VERSION

This document describes AtteanX::Functions::CompositeLists version 0.032

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

package AtteanX::Functions::CompositeLists 0.032 {

	use Attean;
	use Attean::RDF;
	use Encode qw(decode_utf8);
	use Scalar::Util qw(blessed);
	use AtteanX::Serializer::TurtleTokens;
	use AtteanX::Parser::Turtle;
	use AtteanX::SPARQL::Constants;
	use AtteanX::Functions::CompositeMaps;
	
	our $CDT_BASE		= 'http://example.org/cdt/';
	our $LIST_TYPE_IRI	= "${CDT_BASE}List";

=item C<< lex_to_list($literal) >>

=cut
	sub lex_to_list {
		my $l	= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError: not a datatype literal' unless ($dt);
		die 'TypeError: Expecting a List but found '  . $dt->value unless ($dt->value eq $LIST_TYPE_IRI);
		my $lex	= $l->value;
		$lex	=~ s/^\s*//g;
		$lex	=~ s/\s*$//g;
		
		unless ($lex =~ m<^\[(.*)\]$>) {
			die 'TypeError: Invalid lexical form for cdt:List literal: '  . $dt->value;
		}
		
		open(my $fh, '<:encoding(UTF-8)', \$lex);
		my $p		= AtteanX::Parser::Turtle->new();
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

=item C<< listGet($list, $pos) >>

=cut
	sub listGet {
		my $model			= shift;
		my $active_graph	= shift;
		my $l				= shift;
		my $pos				= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError' unless ($dt->value eq $LIST_TYPE_IRI);
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
			"${CDT_BASE}get" => \&listGet,
			"${CDT_BASE}subseq" => \&listSubseq,
			"${CDT_BASE}size" => \&listSize,
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
			unless (blessed($li) and blessed($ri)) {
				$seen_error++;
				next;
			}
			return 0 unless ($li->equals($ri));
		}
		if ($seen_error) {
			die 'TypeError';
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
