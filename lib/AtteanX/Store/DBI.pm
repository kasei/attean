=head1 NAME

AtteanX::Store::DBI - Database quad-store

=head1 VERSION

This document describes AtteanX::Store::DBI version 0.012

=head1 SYNOPSIS

 use AtteanX::Store::DBI;

=head1 DESCRIPTION

AtteanX::Store::DBI provides a quad-store backed by a relational database.

=head1 ATTRIBUTES

=over 4

=item C<< dbh >>

=back

=cut

use utf8;
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
	use List::MoreUtils qw(zip);
	use List::Util qw(any all first);
	use File::ShareDir qw(dist_dir dist_file);
	use Scalar::Util qw(refaddr reftype blessed);
	use namespace::clean;

	with 'Attean::API::MutableQuadStore', 'Attean::API::BulkUpdatableStore';
	with 'Attean::API::QuadStore';
	with 'Attean::API::CostPlanner';

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

	has dbh => (is => 'ro', isa => InstanceOf['DBI::db'], required => 1);
	has _i2t_cache => (is => 'ro', default => sub { Cache::LRU->new( size => 256 ) });
	has _t2i_cache => (is => 'ro', default => sub { Cache::LRU->new( size => 256 ) });

	sub _last_insert_id {
		my $self	= shift;
		my $table	= shift;
		my $dbh		= $self->dbh;
		return $dbh->last_insert_id(undef, undef, $table, undef);
	}

	sub _get_term {
		my $self	= shift;
		my $id		= shift;
		if (my $term = $self->_i2t_cache->get($id)) {
			return $term;
		}
		my $sth		= $self->dbh->prepare('SELECT term.type, term.value, dtterm.value AS datatype, term.language FROM term LEFT JOIN term dtterm ON (term.datatype_id = dtterm.term_id) WHERE term.term_id = ?');
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
		Carp::confess "Failed to load term values for bad ID " . Dumper($id);
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
		my $dbh			= $self->dbh;
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

=item C<< get_quads ( $subject, $predicate, $object, $graph ) >>

Returns a stream object of all statements matching the specified subject,
predicate and objects. Any of the arguments may be undef to match any value,
or an ARRAY reference of terms that are allowable in the respective quad
position.

=cut

	sub get_quads {
		my $self	= shift;
		my @nodes	= map { ref($_) eq 'ARRAY' ? $_ : [$_] } @_;
		my @where;
		my @bind;
		foreach my $i (0 .. 3) {
			my $name	= $pos_names[$i];
			my $terms = $nodes[$i];
			if (defined($terms)) {
				unless (scalar(@$terms) == 1 and not defined($terms->[0])) {
					unless (any { $_->does('Attean::API::Variable') } @$terms) {
						my @ids	= map { $self->_get_term_id($_) } @$terms;
						unless (scalar(@ids)) {
							return Attean::ListIterator->new( values => [], item_type => 'Attean::API::Quad' );
						}
						push(@where, "$name IN (" . join(', ', ('?') x scalar(@ids)) . ")");
						push(@bind, @ids);
					}
				}
			}
		}
		my $sql		= 'SELECT subject, predicate, object, graph FROM quad';
		if (scalar(@where)) {
			$sql	.= ' WHERE ' . join(' AND ', @where);
		}
		my $sth	= $self->dbh->prepare($sql);
		$sth->execute(@bind);
		my $ok	= 1;
		my $sub	= sub {
			return unless ($ok);
			if (my $row	= $sth->fetchrow_arrayref) {
				my @terms	= map { $self->_get_term($_) } @$row;
				my $quad	= Attean::Quad->new(zip @pos_names, @terms);
				return $quad;
			}
			$ok	= 0;
			return;
		};
		my $iter	= Attean::CodeIterator->new( generator => $sub, item_type => 'Attean::API::Quad' );
		return $iter;
	}

=item C<< count_quads ( $subject, $predicate, $object, $graph ) >>

Returns the count of all statements matching the specified subject,
predicate and objects. Any of the arguments may be undef to match any value,
or an ARRAY reference of terms that are allowable in the respective quad
position.

=cut

	sub count_quads {
		my $self	= shift;
		my @nodes	= map { ref($_) eq 'ARRAY' ? $_ : [$_] } @_;
		my @where;
		my @bind;
		foreach my $i (0 .. 3) {
			my $name	= $pos_names[$i];
			my $terms = $nodes[$i];
			if (defined($terms)) {
				unless (scalar(@$terms) == 1 and not defined($terms->[0])) {
					unless (any { $_->does('Attean::API::Variable') } @$terms) {
						my @ids	= map { $self->_get_term_id($_) } @$terms;
						return 0 unless scalar(@ids);
						push(@where, "$name IN (" . join(', ', ('?') x scalar(@ids)) . ")");
						push(@bind, @ids);
					}
				}
			}
		}
		my $sql		= 'SELECT COUNT(*) FROM quad';
		if (scalar(@where)) {
			$sql	.= ' WHERE ' . join(' AND ', @where);
		}
		my $sth	= $self->dbh->prepare($sql);
		$sth->execute(@bind);
		my ($count)	= $sth->fetchrow_array;
		return $count;
	}

=item C<< get_graphs >>

Returns an iterator over the Attean::API::Term objects comprising
the set of graphs of the stored quads.

=cut

	sub get_graphs {
		my $self	= shift;
		my $sth		= $self->dbh->prepare('SELECT DISTINCT value FROM quad JOIN term ON (quad.graph = term.term_id)');
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
		my $sth		= $self->dbh->prepare($sql);
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
		unless (scalar(@ids) == 4) {
			return;
		}
		unless (all { defined($_) } @ids) {
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
	
=item C<< begin_bulk_updates >>

Begin a database transaction.

=cut

	sub begin_bulk_updates {
		my $self	= shift;
		$self->dbh->begin_work;
	}
	
=item C<< end_bulk_updates >>

Commit the current database transaction.

=cut

	sub end_bulk_updates {
		my $self	= shift;
		$self->dbh->commit;
	}
	
=item C<< database_type >>

Returns the database type name as a string (e.g. 'mysql', 'sqlite', or 'postgresql').

=cut

	sub database_type {
		my $self	= shift;
		my $dbh		= $self->dbh;
# 		warn $dbh->get_info($GetInfoType{SQL_DRIVER_NAME});
		my $type	= lc($dbh->get_info($GetInfoType{SQL_DBMS_NAME}));
		return $type;
	}
	
=item C<< create_schema_file >>

Returns the path to the file containing the database DDL for quadstore creation
for the current database type if available, undef otherwise.

=cut

	sub create_schema_file {
		my $self	= shift;
		my $type	= $self->database_type;
		my $dir		= $ENV{ATTEAN_SHAREDIR} || eval { dist_dir('Attean') } || 'share';
		my $file	= File::Spec->catfile($dir, 'database-schema', sprintf('%s-create.sql', $type));
		if (-r $file) {
			return $file;
		}
		return;
	}

=item C<< drop_schema_file >>

Returns the path to the file containing the database DDL for quadstore deletion
for the current database type if available, undef otherwise.

=cut

	sub drop_schema_file {
		my $self	= shift;
		my $type	= $self->database_type;
		my $dir		= $ENV{ATTEAN_SHAREDIR} || eval { dist_dir('Attean') } || 'share';
		my $file	= File::Spec->catfile($dir, 'database-schema', sprintf('%s-drop.sql', $type));
		if (-r $file) {
			return $file;
		}
		return;
	}
	
=item C<< available_database_types >>

Returns the names of the database types for which the system has schemas
available to create and drop quadstore tables.

=cut

	sub available_database_types {
		my $self	= shift;
		my $dir		= $ENV{ATTEAN_SHAREDIR} || eval { dist_dir('Attean') } || 'share';
		my $pat		= File::Spec->catfile($dir, 'database-schema', '*-create.sql');
		my @files	= glob($pat);
		my @types	= map { /(\w+)-create.sql/ } @files;
		return @types;
	}
	
=item C<< dbi_connect_args ( $type, %args ) >>

=item C<< dbi_connect_args ( %args ) >>

Returns a quad C<< $dsn, $user, $password, \%connect_args >> suitable for
passing to C<< DBI->connect >> to obtain a database handle to be used in
constructing a C<< AtteanX::Store::DBI >> quadstore.

C<< %args >> must contain a value for the C<< database >> key. It may also
contain values for the optional keys: C<< user >>, C<< password >>,
C<< host >>, and C<< port >>.

If invoked as a class method, the C<< $type >> parameter is required, and must
be one of the database types returned by C<< available_database_types >>.

If invoked as an object method, the C<< $type >> parameter must not be
included; this information will be obtained directly from the
C<< AtteanX::Store::DBI >> object.

=cut

	sub dbi_connect_args {
		my $self		= shift;
		my $type		= blessed($self) ? $self->database_type : shift;
		my %args		= @_;
		my $database	= $args{database};
		my $user		= $args{user};
		my $password	= $args{password};
		my $host		= $args{host};
		my $port		= $args{port};
		my $dsn;
		my %connect_args;
		$connect_args{RaiseError}	= 1;

		if ($type eq 'mysql') {
			$dsn		= "DBI:mysql:database=${database}";
			if (defined($host)) {
				$dsn	.= ";host=$host";
			}
			if (defined($port)) {
				$dsn	.= ";port=$port";
			}
			$connect_args{mysql_enable_utf8}	= 1;
		} elsif ($type eq 'postgresql') {
			$dsn		= "DBI:Pg:dbname=${database}";
			if (defined($host)) {
				$dsn	.= ";host=$host";
			}
			if (defined($port)) {
				$dsn	.= ";port=$port";
			}
		} elsif ($type eq 'sqlite') {
			$dsn		= "DBI:SQLite:dbname=${database}";
			$connect_args{sqlite_unicode}	= 1;
		}
		
		return ($dsn, $user, $password, \%connect_args);
	}

=item C<< plans_for_algebra( $algebra, $model, $active_graphs, $default_graphs ) >>

For BGP algebras, returns a DBI-specific L<Attean::API::Plan> object, otherwise
returns undef.

=cut

	sub plans_for_algebra {
		my $self			= shift;
		my $algebra			= shift;
		my $model			= shift;
		my $active_graphs	= shift;
		my $default_graphs	= shift;
		
		my %args			= @_;
		return unless ($algebra);
		
		if ($algebra->isa('Attean::Algebra::BGP') and scalar(@{ $algebra->triples }) > 0) {
			my @vars	= $algebra->in_scope_variables;
			
			my @triples	= @{ $algebra->triples };
			my @select;
			my @where_joins;
			my %seen_vars;
			my %source_table_for_var;
			my %blanks;
			my $tcounter	= 0;
			my $bcounter	= 0;
			my @tables;
			
			my %rename_mapping;
			my $rename_proj	= sub {
				my $name	= shift;
				if ($name =~ /[-._]|\W/) {
					my $old	= $name;
					$name	=~ s/_/__/g;
					$name	=~ s/([-.]|\W)/_d/g;
					$name	=~ s/([-.]|\W)/'_x' . sprintf('%02x', ord($1))/e;
					$rename_mapping{$old}	= $name;
				}
				
				return $name;
			};
			
			Carp::confess Dumper($active_graphs) unless (ref($active_graphs) eq 'ARRAY');
			
			my @graph_ids	= map { $self->_get_term_id($_) } @{ $active_graphs };
			if (any { not defined($_) } @graph_ids) {
				return;
			}
		
			my @bind;
			my $graph	= Attean::Variable->new(value => '___g');
			$seen_vars{ $graph->value }++;
			my $graph_values	= sprintf('(%s)', join(', ', ('?') x scalar(@graph_ids)));
			push(@bind, @graph_ids);
			my @where	= ("t0.graph IN $graph_values");
			
			foreach my $t (@triples) {
				my $table	= 't' . $tcounter++;
				push(@tables, $table);

				my @vars;
				my $q		= $t->as_quadpattern($graph);
				my @nodes	= $q->values;
				foreach my $i (0 .. $#nodes) {
					my $node	= $nodes[$i];
					my $name	= $pos_names[$i];
					if ($node->does('Attean::API::Variable')) {
						my $var	= $node;
						push(@vars, [$var, $name]);
					} elsif ($node->does('Attean::API::Blank')) {
						my $id	= $node->value;
						unless (exists $blanks{$id}) {
							my $bname		= sprintf('.%s_%d', 'blank', $bcounter++);
							$blanks{$id}	= Attean::Variable->new(value => $bname);
						}
						my $var	= $blanks{$id};
						push(@vars, [$var, $name]);
						$seen_vars{ $var->value }++;
					} else {
						my $id	= $self->_get_term_id($node);
						return unless defined($id);
						push(@where, sprintf('%s.%s = ?', $table, $name));
						push(@bind, $id);
					}
				}
				
				foreach my $vdata (@vars) {
					my ($var, $name)	= @$vdata;
					my $var_name		= $rename_proj->( $var->value );
					push(@select, [$table, $name, $var_name]) unless ($seen_vars{$var->value}++);
					if (my $tt = $source_table_for_var{ $var->value }) {
						push(@where_joins, ['=', $tt, [$table, $name]]);
					} else {
						$source_table_for_var{ $var->value }	= [$table, $name];
					}
				}
			}
			
			foreach my $w (@where_joins) {
				my ($op, $a, $b)	= @$w;
				my ($as, $bs)		= map { sprintf('%s.%s', @$_) } ($a, $b);
				push(@where, join(' ', $as, $op, $bs));
			}
			
			return AtteanX::Store::DBI::Plan->new(
				store => $self,
				select => \@select,
				where => \@where,
				tables => \@tables,
				in_scope_variables => [@vars],
				rename_mapping => \%rename_mapping,
				bindings => \@bind,
				variables => \%source_table_for_var,
			);
		}

		return;
	}

=item C<< cost_for_plan( $plan ) >>

Returns the estimated cost for a DBI-specific query plan, undef otherwise.

=cut

	sub cost_for_plan {
		my $self	= shift;
		my $plan	= shift;
		if ($plan->isa('AtteanX::Store::DBI::Plan')) {
			return 1; # TODO: actually estimate cost here
		}
		return;
	}

}

package AtteanX::Store::DBI::Plan 0.012 {
	use Moo;
	use Type::Tiny::Role;
	use Types::Standard qw(HashRef ArrayRef InstanceOf Str);
	use namespace::clean;
	
	has store			=> (is => 'ro', isa => InstanceOf['AtteanX::Store::DBI'], required => 1);
	has rename_mapping	=> (is => 'ro', isa => HashRef[Str], default => sub { +{} });
	has variables		=> (is => 'ro', isa => HashRef, required => 1);
	has bindings		=> (is => 'ro', isa => ArrayRef, required => 1);
	has select			=> (is => 'ro', isa => ArrayRef, required => 1);
	has where			=> (is => 'ro', isa => ArrayRef, required => 1);
	has tables			=> (is => 'ro', isa => ArrayRef[Str], required => 1);
	
	with 'Attean::API::BindingSubstitutionPlan', 'Attean::API::NullaryQueryTree';
	
	sub plan_as_string {
		my $self	= shift;
		my ($sql, @bind)	= $self->sql();
		return sprintf('DBI BGP { %s ← (%s) }', $sql, join(', ', @bind));
	}
	
	sub sql {
		my $self	= shift;
		my $bind	= shift;

		my $store	= $self->store;
		my $dbh		= $store->dbh;
		my @bind	= @{ $self->bindings };
		my @where	= @{ $self->where };
		if ($bind) {
			foreach my $var ($bind->variables) {
				my $id	= $store->_get_term_id($bind->value($var));
				return unless defined($id);
				if (my $cdata = $self->variables->{ $var }) {
					my ($table, $col)	= @$cdata;
					push(@where, sprintf("%s.%s = ?", $table, $col));
					push(@bind, $id);
				}
			}
		}
		
		my @select	= map { sprintf("%s.%s AS %s", map { $dbh->quote_identifier( $_ ) } @$_) } @{ $self->select };
		unless (scalar(@select)) {
			push(@select, '1');
		}
		
		
		my @sql;
		push(@sql, 'SELECT');
		push(@sql, join(', ', @select));

		push(@sql, 'FROM');
		push(@sql, join(', ', map { sprintf("quad %s", $_) } @{ $self->tables }));

		if (scalar(@where)) {
			push(@sql, 'WHERE');
			push(@sql, join(' AND ', map { "($_)" } @where));
		}
		
		my $sql	= join(" ", @sql);
		return ($sql, @bind);
	}
	
	sub substitute_impl {
		my $self	= shift;
		my $model	= shift;
		my ($sql, @bind)	= $self->sql(@_);
		my $store	= $self->store;
		my $dbh		= $store->dbh;

# 		warn "TODO: generatee SQL for BGP: $sql\n";
# 		warn "======================================================================\n";
# 		warn "$sql\n";
# 		warn "======================================================================\n";

		my $sth		= $dbh->prepare($sql);
		my $vars	= $self->in_scope_variables;
		return sub {
			my $rv	= $sth->execute(@bind);
			my $sub	= sub {
				if (my $row = $sth->fetchrow_hashref) {
					my %bindings;
					foreach my $k (@$vars) {
						my $key			= $self->rename_mapping->{$k} // $k;
						my $term		= $store->_get_term($row->{$key});
						$bindings{$k}	= $term;
					}
					my $r	= Attean::Result->new( bindings => \%bindings );
					return $r;
				}
				return;
			};
			return Attean::CodeIterator->new( generator => $sub, item_type => 'Attean::API::Result', variables => $vars );
		};
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
