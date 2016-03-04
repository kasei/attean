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

package AtteanX::Store::DBI 0.012 {
	use Moo;
	use DBI;
	use DBI::Const::GetInfoType;
	use Type::Tiny::Role;
	use Types::Standard qw(Int Str ArrayRef HashRef ConsumerOf InstanceOf);
	use Encode;
	use Cache::LRU;
	use Set::Scalar;
	use Digest::SHA;
	use List::MoreUtils qw(zip);
	use List::Util qw(any first);
	use File::ShareDir qw(dist_dir dist_file);
	use Scalar::Util qw(refaddr reftype blessed);
	use Math::Cartesian::Product;
	use namespace::clean;

	with 'Attean::API::MutableQuadStore', 'Attean::API::BulkUpdatableStore';
	with 'Attean::API::QuadStore';

	my @pos_names	= Attean::API::Quad->variables;

=head1 ROLES

This class consumes L<Attean::API::QuadStore>,
L<Attean::API::MutableQuadStore>, and L<Attean::API::BulkUpdatableStore>.

=head1 METHODS

=over 4

=item C<< new ( dbh => $dbh ) >>

Returns a new quad-store object backed by the database referenced by the
supplied database handle.

=cut

	has _dbh => (is => 'ro', isa => InstanceOf['DBI::db'], required => 1);
	has _i2t_cache => (is => 'ro', default => sub { Cache::LRU->new( size => 256 ) });
	has _t2i_cache => (is => 'ro', default => sub { Cache::LRU->new( size => 256 ) });

	sub _last_insert_id {
		my $self	= shift;
		my $table	= shift;
		my $dbh		= $self->_dbh;
		return $dbh->last_insert_id(undef, undef, $table, undef);
	}

	sub _get_term {
		my $self	= shift;
		my $id		= shift;
		if (my $term = $self->_i2t_cache->get($id)) {
			return $term;
		}
		my $sth		= $self->_dbh->prepare('SELECT term.type, term.value, dtterm.value AS datatype, term.language FROM term LEFT JOIN term dtterm ON (term.datatype_id = dtterm.term_id) WHERE term.term_id = ?');
		$sth->execute($id);
		my $row		= $sth->fetchrow_hashref;
		my $type	= $row->{type};
		my $term;
		my $value		= $row->{value};
		my $datatype	= $row->{datatype};
		my $lang		= $row->{language};
		if ($type eq 'iri') {
			$term	= Attean::IRI->new( value => $value );
		} elsif ($type eq 'blank') {
			$term	= Attean::Blank->new( value => $value );
		} elsif ($type eq 'literal') {
			my %args	= (value => $value, datatype => Attean::IRI->new(value => $datatype));
			if ($lang) {
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
		my $dbh			= $self->_dbh;
		my $tid;
		if ($term->does('Attean::API::IRI')) {
			my $sth	= $dbh->prepare('SELECT term_id FROM term WHERE type = ? AND value = ?');
			my $value	= $term->value;
			$sth->execute('iri', $value);
			($tid) = $sth->fetchrow_array;
		} elsif ($term->does('Attean::API::Blank')) {
			my $sth	= $dbh->prepare('SELECT term_id FROM term WHERE type = ? AND value = ?');
			my $value	= $term->value;
			$sth->execute('blank', $value);
			($tid) = $sth->fetchrow_array;
		} elsif ($term->does('Attean::API::Literal')) {
			my $dtid	= $self->_get_or_create_term_id($term->datatype);
			my $sth	= $dbh->prepare('SELECT term_id FROM term WHERE type = ? AND value = ? AND datatype_id = ? AND language = ?');
			my $value	= $term->value;
			my $lang	= $term->language;
			$sth->execute('literal', $value, $dtid, $lang);
			($tid) = $sth->fetchrow_array;
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
		my $dbh			= $self->_dbh;
		my $tid;
		my $insert_term_sth	= $dbh->prepare('INSERT INTO term (type, value, datatype_id, language) VALUES (?, ?, ?, ?)');
		if ($term->does('Attean::API::IRI')) {
			my $sth	= $dbh->prepare('SELECT term_id FROM term WHERE type = ? AND value = ?');
			my $value	= $term->value;
			$sth->execute('iri', $value);
			($tid) = $sth->fetchrow_array;
			unless (defined($tid)) {
				$insert_term_sth->execute('iri', $value, undef, undef);
				$tid	= $self->_last_insert_id('term');
			}
		} elsif ($term->does('Attean::API::Blank')) {
			my $sth	= $dbh->prepare('SELECT term_id FROM term WHERE type = ? AND value = ?');
			my $value	= $term->value;
			$sth->execute('blank', $value);
			($tid) = $sth->fetchrow_array;
			unless (defined($tid)) {
				$insert_term_sth->execute('blank', $value, undef, undef);
				$tid	= $self->_last_insert_id('term');
			}
		} elsif ($term->does('Attean::API::Literal')) {
			my $dtid	= $self->_get_or_create_term_id($term->datatype);
			my $sql		= 'SELECT term_id FROM term WHERE type = ? AND value = ? AND datatype_id = ?';
			my $value	= $term->value;
			my @bind	= ('literal', $value, $dtid);
			my $lang	= $term->language;
			if ($lang) {
				$sql	.= ' AND language = ?';
				push(@bind, $lang);
			}
			my $sth	= $dbh->prepare($sql);
			$sth->execute(@bind);
			($tid) = $sth->fetchrow_array;
			unless (defined($tid)) {
				$insert_term_sth->execute('literal', $value, $dtid, $lang);
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
			my $sth	= $self->_dbh->prepare($sql);
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
		my $sth		= $self->_dbh->prepare('SELECT DISTINCT value FROM quad JOIN term ON (quad.graph = term.term_id)');
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
		
		my $type	= $self->database_type;
		my @bind	= @ids;
		my $sql		= 'INSERT INTO quad (subject, predicate, object, graph) VALUES (?, ?, ?, ?)';
		if ($type eq 'sqlite') {
			$sql	= 'INSERT OR IGNORE INTO quad (subject, predicate, object, graph) VALUES (?, ?, ?, ?)';
		} elsif ($type eq 'mysql') {
			$sql	= 'INSERT IGNORE INTO quad (subject, predicate, object, graph) VALUES (?, ?, ?, ?)';
		} elsif ($type eq 'postgresql') {
			$sql	= 'INSERT INTO quad (subject, predicate, object, graph) SELECT ?, ?, ?, ? WHERE NOT EXISTS (SELECT 1 FROM quad WHERE subject = ? AND predicate = ? AND object = ? AND graph = ?)';
			push(@bind, @ids);
		}
		my $sth		= $self->_dbh->prepare($sql);
		$sth->execute(@bind);
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
		my $sth		= $self->_dbh->prepare('DELETE FROM quad WHERE subject = ? AND predicate = ? AND object = ? AND graph = ?');
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
		my $sth		= $self->_dbh->prepare('DELETE FROM quad WHERE graph = ?');
		$sth->execute($gid);
		return;
	}
	
=item C<< begin_bulk_updates >>

Begin a database transaction.

=cut

	sub begin_bulk_updates {
		my $self	= shift;
		$self->_dbh->begin_work;
	}
	
=item C<< end_bulk_updates >>

Commit the current database transaction.

=cut

	sub end_bulk_updates {
		my $self	= shift;
		$self->_dbh->commit;
	}
	
=item C<< database_type >>

Returns the database type name as a string (e.g. 'mysql', 'sqlite', or 'postgresql').

=cut

	sub database_type {
		my $self	= shift;
		my $dbh		= $self->_dbh;
		my $type	= lc($dbh->get_info($GetInfoType{SQL_DBMS_NAME}));
		return $type;
	}
	
=item C<< schema_file >>

Returns the path to the file containing the database schema DDL for the current
database type if available, undef otherwise.

=cut

	sub schema_file {
		my $self	= shift;
		my $type	= $self->database_type;
		my $dir		= $ENV{RDF_ENDPOINT_SHAREDIR} || eval { dist_dir('Attean') } || 'share';
		my $file	= File::Spec->catfile($dir, 'database-schema', sprintf('dbi-schema-%s.sql', $type));
		if (-r $file) {
			return $file;
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
