use v5.14;
use warnings;

=head1 NAME

Attean::BindingEqualityTest - Test for equality of binding sets with bnode isomorphism

=head1 VERSION

This document describes Attean::BindingEqualityTest version 0.033

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $test	= Attean::BindingEqualityTest->new();
  if ($test->equals($iter_a, $iter_b)) {
    say "Iterators contain equivalent bindings";
  }

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

package Attean::BindingEqualityTest::_Iter {
	sub new {
		my $class	= shift;
		my @iters	= @_;
		my @values	= $class->_materialize([], @iters);
		return bless(\@values, $class);
	}
	
	sub _materialize {
		my $class	= shift;
		my $v		= shift;
		my @iters	= @_;
		if (scalar(@iters)) {
			my $i	= shift(@iters);
			my @values;
			while (my $vv = $i->next) {
				my $prefix	= [@$v, @$vv];
				push(@values, $class->_materialize($prefix, @iters));
			}
			return @values;
		} else {
			return $v;
		}
	}
	
	sub next {
		my $self	= shift;
		return shift(@$self);
	}
}

package Attean::BindingEqualityTest 0.033 {
	use v5.14;
	use warnings;
	use Moo;
	use Types::Standard qw(CodeRef ConsumerOf Str);
	use Data::Dumper;
	use Algorithm::Combinatorics qw(permutations);
	use Scalar::Util qw(blessed);
	use List::Util qw(shuffle);
	use Attean::RDF;
	use Digest::MD5 qw(md5_hex);
	use namespace::clean;
	
	with 'MooX::Log::Any';
	
	has error => (is => 'rw', isa => Str, init_arg => undef);
	
	sub _coerce {
		my $o	= shift;
		if ($o->does('Attean::API::Model')) {
			return $o->get_quads;
		} elsif ($o->does('Attean::API::Iterator')) {
			return $o;
		}
		return;
	}

=item C<< equals ( $graph1, $graph2 ) >>

Returns true if the invocant and $graph represent two equal RDF graphs (e.g.
there exists a bijection between the RDF statements of the invocant and $graph).

=cut

	sub equals {
		my $self  	= shift;
		$self->error('');
		return $self->_check_equality(@_) ? 1 : 0;
	}

	sub _check_equality {
		my $self	= shift;
		my ($a, $b)	= map { _coerce($_) } @_;
		
		my @graphs	= ($a, $b);
		my ($ba, $nba)	= $self->split_blank_statements($a);
		my ($bb, $nbb)	= $self->split_blank_statements($b);
		if (scalar(@$nba) != scalar(@$nbb)) {
			my $nbac	= scalar(@$nba);
			my $nbbc	= scalar(@$nbb);
			
# 			warn "====================================================\n";
# 			warn "BindingEqualityTest count of non-blank statements didn't match:\n";
# 			warn "-------- a\n";
# 			foreach my $t (@$nba) {
# 				warn $t->as_string . "\n";
# 			}
# 			warn "-------- b\n";
# 			foreach my $t (@$nbb) {
# 				warn $t->as_string . "\n";
# 			}
			$self->error("count of non-blank statements didn't match ($nbac != $nbbc)");
			return 0;
		}
		my $bac	= scalar(@$ba);
		my $bbc	= scalar(@$bb);
		if ($bac != $bbc) {
			$self->error("count of blank statements didn't match ($bac != $bbc)");
			return 0;
		}
		
		for ($nba, $nbb) {
			@$_	= sort map { $_->as_string } @$_;
		}
		
		foreach my $i (0 .. $#{ $nba }) {
			unless ($nba->[$i] eq $nbb->[$i]) {
# 				warn "====================================================\n";
# 				warn "BindingEqualityTest non-blank statements didn't match:\n";
# 				warn "-------- a\n";
# 				foreach my $t (@$nba) {
# 					warn $t . "\n";
# 				}
# 				warn "-------- b\n";
# 				foreach my $t (@$nbb) {
# 					warn $t . "\n";
# 				}
				$self->error("non-blank triples don't match:\n" . Dumper($nba->[$i], $nbb->[$i]));
				return 0;
			}
		}
	
		return _find_mapping($self, $ba, $bb, 1);
	}

=item C<< is_subgraph_of ( $graph1, $graph2 ) >>

Returns true if the invocant is a subgraph of $graph. (i.e. there exists an
injection of RDF statements from the invocant to $graph.)

=cut

	sub is_subgraph_of {
		my $self  = shift;
		$self->error('');
		return $self->_check_subgraph(@_) ? 1 : 0;
	}

=item C<< injection_map ( $graph1, $graph2 ) >>

If the invocant is a subgraph of $graph, returns a mapping of blank node
identifiers from the invocant graph to $graph as a hashref. Otherwise
returns false. The solution is not always unique; where there exist multiple
solutions, the solution returned is arbitrary.

=cut

	sub injection_map {
		my $self  = shift;
		$self->error('');
		my $map   = $self->_check_subgraph(@_);
		return $map if $map;
		return;
	}

	sub _check_subgraph {
		my $self	= shift;
		my ($a, $b)	= map { _coerce($_) } @_;
	
		my @graphs	= ($a, $b);
		my ($ba, $nba)	= $self->split_blank_statements($a);
		my ($bb, $nbb)	= $self->split_blank_statements($b);
	
		if (scalar(@$nba) > scalar(@$nbb)) {
			$self->error("invocant had too many blank node statements to be a subgraph of argument");
			return 0;
		} elsif (scalar(@$ba) > scalar(@$bb)) {
			$self->error("invocant had too many non-blank node statements to be a subgraph of argument");
			return 0;
		}

		my %NBB = map { $_->as_string => 1 } @$nbb;
	
		foreach my $st (@$nba) {
			unless ($NBB{ $st->as_string }) {
				return 0;
			}
		}
	
		return _find_mapping($self, $ba, $bb);
	}

	sub _statement_blank_irisets {
		my $self	= shift;
		my @st		= @_;
		my %blank_ids_b_iris;
		foreach my $st (@st) {
			my @iris	= map { $_->value } grep { $_->does('Attean::API::IRI') } $st->values;
			unless (scalar(@iris)) {
				push(@iris, '_');
			}
			foreach my $n (grep { $_->does('Attean::API::Blank') } $st->values) {
				foreach my $i (@iris) {
					$blank_ids_b_iris{$n->value}{$i}++;
				}
			}
		}
		
		my %iri_blanks;
		foreach my $bid (sort keys %blank_ids_b_iris) {
			my $d	= Digest::MD5->new();
			foreach my $iri (sort keys %{ $blank_ids_b_iris{$bid} }) {
				$d->add($iri);
			}
			$iri_blanks{$d->hexdigest}{$bid}++;
		}
		return \%iri_blanks;
	}
	
	sub _find_mapping {
		my $self	= shift;
		my $ba		= shift;
		my $bb		= shift;
		my $equal	= shift || 0;

# 		warn "########### _find_mapping:\n";
# 		warn "============ A\n";
# 		foreach my $t (@$ba) {
# 			warn $t->as_string . "\n";
# 		}
# 		warn "============ B\n";
# 		foreach my $t (@$bb) {
# 			warn $t->as_string . "\n";
# 		}

		if (scalar(@$ba) == 0) {
			return {};
		}
	
		my %blank_ids_a;
		foreach my $st (@$ba) {
			foreach my $n ($st->blanks) {
				$blank_ids_a{ $n->value }++;
			}
		}

		my %blank_ids_b;
		foreach my $st (@$bb) {
			foreach my $n ($st->blanks) {
				$blank_ids_b{ $n->value }++;
			}
		}
		
		
		my (@ka, @kb);
		my $kbp;
# 		if ($equal) {
# 			# if we're testing for equality, and not just finding an injection mapping,
# 			# we can avoid unnecessary work by restricting mappings to those where each
# 			# permutation only maps blank nodes to other blank nodes that appear in
# 			# similar bindings (in this case they appear with all the same IRIs)
# 			my $ba_iri_blanks	= $self->_statement_blank_irisets(@$ba);
# 		
# 			my $bb_iri_blanks	= $self->_statement_blank_irisets(@$bb);
# 		
# 			my $ba_keys	= join('|', sort keys %$ba_iri_blanks);
# 			my $bb_keys	= join('|', sort keys %$bb_iri_blanks);
# 			unless ($ba_keys eq $bb_keys) {
# 				$self->error("didn't find blank node mapping\n");
# 				return 0;
# 			}
# 			
# 			my @iters;
# 			foreach my $k (sort keys %$ba_iri_blanks) {
# 				unless (scalar(@{[keys %{ $ba_iri_blanks->{$k} }]}) == scalar(@{[keys %{ $bb_iri_blanks->{$k} }]})) {
# 					$self->error("didn't find blank node mapping\n");
# 					return 0;
# 				}
# 				push(@ka, keys %{ $ba_iri_blanks->{$k} });
# 				push(@kb, keys %{ $bb_iri_blanks->{$k} });
# 				my $i		= permutations([keys %{ $bb_iri_blanks->{$k} }]);
# 				push(@iters, $i);
# 			}
# 			
# 			if (scalar(@iters) == 1) {
# 				$kbp	= shift(@iters);
# 			} else {
# 				$kbp	= Attean::BindingEqualityTest::_Iter->new(@iters);
# 			}
# 		} else {
			@ka		= keys %blank_ids_a;
			@kb		= keys %blank_ids_b;
			$kbp	= permutations( [shuffle @kb] );
# 		}
		
		my $canon_map	= Attean::TermMap->canonicalization_map;
		my %bb_master	= map { $_->apply_map($canon_map)->as_string => 1 } @$bb;
	
		my $count	= 0;
		MAPPING: while (my $mapping = $kbp->next) {
			my %mapping_str;
			@mapping_str{ @ka }	= @$mapping;
			my %mapping			= map {
				Attean::Blank->new($_)->as_string => Attean::Blank->new($mapping_str{$_})
			} (keys %mapping_str);
			my $mapper	= Attean::TermMap->rewrite_map(\%mapping);
			$self->log->trace("trying mapping: " . Dumper($mapping));
		
			my %bb	= %bb_master;
			foreach my $st (@$ba) {
				my $mapped_st	= $st->apply_map($mapper)->as_string;
# 				warn ">>>>>>>\n";
# 				warn "-> " . $st->as_string . "\n";
# 				warn "-> " . $mapped_st . "\n";
				$self->log->trace("checking for '$mapped_st' in " . Dumper(\%bb));
				if ($bb{ $mapped_st }) {
					$self->log->trace("Found mapping for binding: " . Dumper($mapped_st));
					delete $bb{ $mapped_st };
				} else {
					$self->log->trace("No mapping found for binding: " . Dumper($mapped_st));
# 					warn "No mapping found for binding: " . Dumper($mapped_st);
# 					warn Dumper(\%bb);
					next MAPPING;
				}
			}
			$self->error("found mapping: " . Dumper(\%mapping_str));
			return \%mapping_str;
		}
		
# 		warn "didn't find blank node mapping:\n";
# 		warn "============ A\n";
# 		foreach my $t (@$ba) {
# 			warn $t->as_string . "\n";
# 		}
# 		warn "============ B\n";
# 		foreach my $t (@$bb) {
# 			warn $t->as_string . "\n";
# 		}
		$self->error("didn't find blank node mapping\n");
		return 0;
	}

=item C<< split_blank_statements( $iter ) >>

Returns two array refs containing bindings from C<< $iter >>, with bindings
containing blank nodes and bindings without any blank nodes, respectively.

=cut

	sub split_blank_statements {
		my $self	= shift;
		my $iter	= shift;
		my (@blanks, @nonblanks);
		while (my $st = $iter->next) {
			if ($st->has_blanks) {
				push(@blanks, $st);
			} else {
				push(@nonblanks, $st);
			}
		}
		return (\@blanks, \@nonblanks);
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
