=head1 NAME

AtteanX::Store::DBI - Database quad-store

=head1 VERSION

This document describes AtteanX::Store::DBI version 0.012

=head1 SYNOPSIS

 use AtteanX::Store::DBI;

=head1 DESCRIPTION

AtteanX::Store::DBI provides a quad-store backed by a relational database.

=cut

use v5.14;
use warnings;

package AtteanX::Store::API::DBI 0.012 {
	use Moo::Role;
	use Type::Tiny::Role;
	use Types::Standard qw(Int Str ArrayRef HashRef ConsumerOf InstanceOf);
	use Encode;
	use Cache::LRU;
	use Set::Scalar;
	use Digest::SHA;
	use List::Util qw(any first);
	use List::MoreUtils qw(zip);
	use Scalar::Util qw(refaddr reftype blessed);
	use Math::Cartesian::Product;
	use namespace::clean;

	with 'Attean::API::MutableQuadStore';
	with 'Attean::API::QuadStore';

	requires '_last_insert_id';
	
	my @pos_names	= Attean::API::Quad->variables;

=head1 ATTRIBUTES

=over 4

=item C<< subject >>

=item C<< predicate >>

=item C<< object >>

=item C<< graph >>

=back

=head1 METHODS

Beyond the methods documented below, this class inherits methods from the
L<Attean::API::QuadStore> class.

=over 4

=item C<< new () >>

Returns a new memory-backed storage object.

=cut

	has dbh => (is => 'ro', isa => InstanceOf['DBI::db'], required => 1);
	has quads_table => (is => 'ro', isa => Str, default => 'quads');
	has _i2t_cache => (is => 'ro', default => sub { Cache::LRU->new( size => 256 ) });
	has _t2i_cache => (is => 'ro', default => sub { Cache::LRU->new( size => 256 ) });

	sub _get_term {
		my $self	= shift;
		my $id		= shift;
		if (my $term = $self->_i2t_cache->get($id)) {
			return $term;
		}
		my $sth		= $self->dbh->prepare('SELECT type, iri.value AS iri_value, blank.value AS blank_value, literal.value AS literal_value, dtiri.value AS datatype, language FROM term LEFT JOIN iri USING (iri_id) LEFT JOIN literal USING (literal_id) LEFT JOIN iri dtiri ON (datatype_id = dtiri.iri_id) LEFT JOIN blank USING (blank_id) WHERE term_id = ?');
		$sth->execute($id);
		my $row		= $sth->fetchrow_hashref;
		my $type	= $row->{type};
		my $term;
		if ($type eq 'iri') {
			$term	= Attean::IRI->new( value => $row->{iri_value} );
		} elsif ($type eq 'blank') {
			$term	= Attean::Blank->new( value => $row->{blank_value} );
		} elsif ($type eq 'literal') {
			my %args	= (value => $row->{literal_value}, datatype => Attean::IRI->new(value => $row->{datatype}));
			if (my $lang = $args{language}) {
				$args{language} = $lang;
			}
			$term	= Attean::Literal->new( %args );
		}
		if ($term) {
			$self->_i2t_cache->set($id => $term);
			return $term;
		}
		die;
	}
	
	sub _get_term_id {
		my $self		= shift;
		my $term		= shift;
		if (my $id = $self->_t2i_cache->get($term->as_string)) {
			return $id;
		}
		my $dbh			= $self->dbh;
		my $tid;
		if ($term->does('Attean::API::IRI')) {
			my $sth	= $dbh->prepare('SELECT iri_id FROM iri WHERE value = ?');
			my $value	= $term->value;
			$sth->execute($value);
			my ($id) = $sth->fetchrow_array;
			if (defined($id)) {
				my $sth	= $dbh->prepare('SELECT term_id FROM term WHERE iri_id = ?');
				$sth->execute($id);
				($tid)	= $sth->fetchrow_array;
			}
		} elsif ($term->does('Attean::API::Blank')) {
			my $sth	= $dbh->prepare('SELECT blank_id FROM blank WHERE value = ?');
			my $value	= $term->value;
			$sth->execute($value);
			my ($id) = $sth->fetchrow_array;
			if (defined($id)) {
				my $sth	= $dbh->prepare('SELECT term_id FROM term WHERE blank_id = ?');
				$sth->execute($id);
				($tid)	= $sth->fetchrow_array;
			}
		} elsif ($term->does('Attean::API::Literal')) {
			my $dtid	= $self->_get_or_create_term_id($term->datatype);
			my $sth	= $dbh->prepare('SELECT literal_id FROM literal WHERE value = ? AND datatype_id = ? AND language = ?');
			my $value	= $term->value;
			my $lang	= $term->language;
			$sth->execute($value, $dtid, $lang);
			my ($id) = $sth->fetchrow_array;
			if (defined($id)) {
				my $sth	= $dbh->prepare('SELECT term_id FROM term WHERE literal_id = ?');
				$sth->execute($id);
				($tid)	= $sth->fetchrow_array;
			}
		}
		if (defined($tid)) {
			$self->_t2i_cache->set($term->as_string => $tid);
			return $tid;
		}
		return;
	}
	
	sub _get_or_create_term_id {
		my $self		= shift;
		my $term		= shift;
		if (my $id = $self->_t2i_cache->get($term->as_string)) {
			return $id;
		}
		my $dbh			= $self->dbh;
		my $tid;
		my $insert_term_sth	= $dbh->prepare('INSERT INTO term (type, iri_id, literal_id, blank_id) VALUES (?, ?, ?, ?)');
		if ($term->does('Attean::API::IRI')) {
			my $sth	= $dbh->prepare('SELECT iri_id FROM iri WHERE value = ?');
			my $value	= $term->value;
			$sth->execute($value);
			my ($id) = $sth->fetchrow_array;
			if (defined($id)) {
				my $sth	= $dbh->prepare('SELECT term_id FROM term WHERE iri_id = ?');
				$sth->execute($id);
				($tid)	= $sth->fetchrow_array;
			} else {
				my $sth	= $dbh->prepare('INSERT INTO iri (value) VALUES (?)');
				$sth->execute($value);
				$id	= $self->_last_insert_id('iri');
				$insert_term_sth->execute('iri', $id, undef, undef);
				$tid	= $self->_last_insert_id('term');
			}
		} elsif ($term->does('Attean::API::Blank')) {
			my $sth	= $dbh->prepare('SELECT blank_id FROM blank WHERE value = ?');
			my $value	= $term->value;
			$sth->execute($value);
			my ($id) = $sth->fetchrow_array;
			if (defined($id)) {
				my $sth	= $dbh->prepare('SELECT term_id FROM term WHERE blank_id = ?');
				$sth->execute($id);
				($tid)	= $sth->fetchrow_array;
			} else {
				my $sth	= $dbh->prepare('INSERT INTO blank (value) VALUES (?)');
				$sth->execute($value);
				$id	= $self->_last_insert_id('blank');
				$insert_term_sth->execute('blank', undef, undef, $id);
				$tid	= $self->_last_insert_id('term');
			}
		} elsif ($term->does('Attean::API::Literal')) {
			my $dttid	= $self->_get_or_create_term_id($term->datatype);
			my $tsth	= $dbh->prepare('SELECT iri_id FROM term WHERE term_id = ?');
			$tsth->execute($dttid);
			my ($dtid)	= $tsth->fetchrow_array;
			my $sql		= 'SELECT literal_id FROM literal WHERE value = ? AND datatype_id = ?';
			my $value	= $term->value;
			my @bind	= ($value, $dtid);
			my $lang	= $term->language;
			if ($lang) {
				$sql	.= ' AND language = ?';
				push(@bind, $lang);
			}
			my $sth	= $dbh->prepare($sql);
			$sth->execute(@bind);
			my ($id) = $sth->fetchrow_array;
			if (defined($id)) {
				my $sth	= $dbh->prepare('SELECT term_id FROM term WHERE literal_id = ?');
				$sth->execute($id);
				($tid)	= $sth->fetchrow_array;
			} else {
				my $sth	= $dbh->prepare('INSERT INTO literal (value, datatype_id, language) VALUES (?, ?, ?)');
# 				use Data::Dumper;
# 				warn Dumper([$value, $dtid, $lang]);
				$sth->execute($value, $dtid, $lang);
# 				warn "<<<\n";
				$id	= $self->_last_insert_id('literal');
				$insert_term_sth->execute('literal', undef, $id, undef);
				$tid	= $self->_last_insert_id('term');
			}
		} else {
			die "Failed to get ID for term: " . $term->as_string;
		}
		
		if (defined($tid)) {
			$self->_t2i_cache->set($term->as_string => $tid);
			return $tid;
		}
		die;
	}

	# =item C<< size >>
	# 
	# Returns the number of quads in the store.
	# 
	# =cut
	# 
	# sub size {
	# 	die;
	# }

=item C<< get_quads ( $subject, $predicate, $object, $graph ) >>

Returns a stream object of all statements matching the specified subject,
predicate and objects. Any of the arguments may be undef to match any value.

=cut

	sub get_quads {
		my $self	= shift;
		my @nodes	= map { ref($_) eq 'ARRAY' ? $_ : [$_] } @_;
		my @iters;
		cartesian {
			my @where;
			my @bind;
			foreach my $i (0 .. 3) {
				my $name	= $pos_names[$i];
				if (defined(my $term = $_[$i])) {
					my $id	= $self->_get_term_id($term);
					push(@where, "$name = ?");
					push(@bind, $id);
				}
			}
			my $sql		= 'SELECT subject, predicate, object, graph FROM quad';
			if (scalar(@where)) {
				$sql	.= ' WHERE ' . join(' AND ', @where);
			}
			my $sth	= $self->dbh->prepare($sql);
			$sth->execute(@bind);
			my $sub	= sub {
				my $row	= $sth->fetchrow_arrayref;
				return unless $row;
				my @terms	= map { $self->_get_term($_) } @$row;
				return Attean::Quad->new(zip @pos_names, @terms);
			};
			my $iter	= Attean::CodeIterator->new( generator => $sub, item_type => 'Attean::API::Quad' );
			push(@iters, $iter);
		} @nodes;
		return Attean::IteratorSequence->new( iterators => \@iters, item_type => 'Attean::API::Quad' );
	}

	# sub count_quads {
	# 	my $self	= shift;
	# 	my @nodes	= map { ref($_) eq 'ARRAY' ? $_ : [$_] } @_;
	# 	die;
	# }

=item C<< get_graphs >>

Returns an iterator over the Attean::API::Term objects comprising
the set of graphs of the stored quads.

=cut

	sub get_graphs {
		my $self	= shift;
		my $sth		= $self->dbh->prepare('SELECT DISTINCT value FROM quad JOIN term ON (quad.graph = term.term_id) JOIN iri ON (term.iri_id = iri.iri_id)');
		$sth->execute;
		my $sub	= sub {
			my $row	= $sth->fetchrow_arrayref;
			return unless ref($row);
			my ($value)	= @$row;
			return Attean::IRI->new(value => $value);
		};
		return Attean::CodeIterator->new( generator => $sub, item_type => 'Attean::API::Term' );
	}

	# -----------------------------------------------------------------------------

=item C<< add_quad ( $quad ) >>

Adds the specified C<$quad> to the underlying model.

=cut
	
	sub add_quad {
		my $self	= shift;
		my $st		= shift;
		my @ids		= map { $self->_get_or_create_term_id($_) } $st->values;
		if (any { not defined($_) } @ids) {
			return;
		}
		my $sth		= $self->dbh->prepare('INSERT INTO quad (subject, predicate, object, graph) VALUES (?, ?, ?, ?)');
		$sth->execute(@ids);
		return;
	}

=item C<< remove_quad ( $statement ) >>

Removes the specified C<$statement> from the underlying model.

=cut

	sub remove_quad {
		my $self	= shift;
		my $st		= shift;
		my @ids		= map { $self->_get_term_id($_) } $st->values;
		if (any { not defined($_) } @ids) {
			return;
		}
		my $sth		= $self->dbh->prepare('DELETE FROM quad WHERE subject = ? AND predicate = ? AND object = ? AND graph = ?');
		$sth->execute(@ids);
		return;
	}

=item C<< create_graph( $graph ) >>

This is a no-op function for the memory quad-store.

=cut

	sub create_graph {
		# no-op on a quad-store
	}

=item C<< drop_graph( $graph ) >>

Removes all quads with the given C<< $graph >>.

=cut

	sub drop_graph {
		my $self	= shift;
		return $self->clear_graph(@_);
	}

=item C<< clear_graph( $graph ) >>

Removes all quads with the given C<< $graph >>.

=cut

	sub clear_graph {
		my $self	= shift;
		my $graph	= shift;
		my $gid		= $self->_get_term_id($graph);
		return unless defined($gid);
		my $sth		= $self->dbh->prepare('DELETE FROM quad WHERE graph = ?');
		$sth->execute($gid);
		return;
	}
}

package AtteanX::Store::DBI::mysql 0.012 {
	use Moo;
	use List::Util qw(any);
	use namespace::clean;
	
	with 'AtteanX::Store::API::DBI';
	
	sub _last_insert_id {
		my $self	= shift;
		my $table	= shift;
		my $dbh		= $self->dbh;
		return $dbh->{'mysql_insertid'};
	}

	sub add_quad {
		my $self	= shift;
		my $st		= shift;
		my @ids		= map { $self->_get_or_create_term_id($_) } $st->values;
		if (any { not defined($_) } @ids) {
			return;
		}
		my $sth		= $self->dbh->prepare('INSERT IGNORE INTO quad (subject, predicate, object, graph) VALUES (?, ?, ?, ?)');
		$sth->execute(@ids);
		return;
	}
}

package AtteanX::Store::DBI::Pg 0.012 {
	use Moo;
	use List::Util qw(any);
	use namespace::clean;
	
	with 'AtteanX::Store::API::DBI';
	
	sub _last_insert_id {
		my $self	= shift;
		my $table	= shift;
		my $dbh		= $self->dbh;
		return $dbh->last_insert_id(undef, undef, $table, undef);
	}

	sub add_quad {
		my $self	= shift;
		my $st		= shift;
		my @ids		= map { $self->_get_or_create_term_id($_) } $st->values;
		if (any { not defined($_) } @ids) {
			return;
		}
		my $check	= $self->dbh->prepare('SELECT COUNT(*) FROM quad WHERE subject = ? AND predicate = ? AND object = ? AND graph = ?');
		$check->execute(@ids);
		my ($count)	= $check->fetchrow_array;
		unless ($count) {
			my $sth		= $self->dbh->prepare('INSERT INTO quad (subject, predicate, object, graph) VALUES (?, ?, ?, ?)');
			$sth->execute(@ids);
		}
		return;
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/perlrdf2/issues>.

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2016 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
