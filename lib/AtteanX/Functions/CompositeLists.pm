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

package AtteanX::Functions::CompositeLists 0.032 {

	use Attean;
	use Attean::RDF;
	
	our $LIST_TYPE_IRI	= 'http://example.org/List';

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
		return list_to_lex(@_);
	}

=item C<< listGet($list, $pos) >>

=cut
	sub listGet {
		my $model			= shift;
		my $active_graph	= shift;
		my $l		= shift;
		my $pos		= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError' unless ($dt->value eq $LIST_TYPE_IRI);
		my $lex	= $l->value;
		substr($lex, 0, 1, '');
		substr($lex, -1, 1, '');
		my $p	= Attean->get_parser('SPARQL')->new();
		my @nodes	= $p->parse_nodes($lex, commas => 1);
		my $i		= int($pos->value);
		return $nodes[$i];
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
		my @terms	= map { Attean::Literal->integer($_) } ($start .. $end);
		my $lex	= '(' . join(',', map { $_->ntriples_string } @terms) . ')';
		return dtliteral($lex, $LIST_TYPE_IRI);
	}

=item C<< lex_to_list($literal) >>

=cut
	sub lex_to_list {
		my $l	= shift;
		die 'TypeError' unless ($l->does('Attean::API::Literal'));
		my $dt	= $l->datatype;
		die 'TypeError: not a datatype literal' unless ($dt);
		die 'TypeError: Expecting a List but found '  . $dt->value unless ($dt->value eq $LIST_TYPE_IRI);
		my $lex	= $l->value;
		substr($lex, 0, 1, '');
		substr($lex, -1, 1, '');
		my $p	= Attean->get_parser('SPARQL')->new();
		my @nodes	= $p->parse_nodes($lex, commas => 1);
		return @nodes;
	}

=item C<< list_to_lex(@terms) >>

=cut
	sub list_to_lex {
		my @terms	= @_;
		my $lex	= '(' . join(',', map { $_->ntriples_string } @terms) . ')';
		return dtliteral($lex, $LIST_TYPE_IRI);
	}

=item C<< zip($list, $list) >>

=cut
	sub zip {
		my $model			= shift;
		my $active_graph	= shift;
		my $lhs		= shift;
		my $rhs		= shift;
		my @lhs_nodes	= lex_to_list($lhs);
		my @rhs_nodes	= lex_to_list($rhs);
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
		my $lex	= '(' . join(',', map { $_->ntriples_string } @terms) . ')';
		return dtliteral($lex, $LIST_TYPE_IRI);
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
		Attean->register_global_function(
			'http://example.org/listGet' => \&listGet,
			'http://example.org/listCreate' => \&listCreate,
			'http://example.org/sequence' => \&sequence,
			'http://example.org/zip' => \&zip,
			'http://example.org/split' => \&stringSplit,
			'http://example.org/list_from_head' => \&list_from_head,
		);

		Attean->register_global_aggregate(
			'http://example.org/listAgg' => {
				start		=> \&listCreate_agg_start,
				process		=> \&listCreate_agg_process,
				finalize	=> \&listCreate_agg_finalize,
			},
		);
	}
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
