use v5.14;
use warnings;

=head1 NAME

AtteanX::RDFQueryTranslator - Translate RDF::Query objects to Attean::API::Algebra objects.

=head1 VERSION

This document describes AtteanX::RDFQueryTranslator version 0.013

=head1 DESCRIPTION

The AtteanX::RDFQueryTranslator class translates RDF::Query objects into an
L<Attean::API::Algebra> tree.

=head1 ATTRIBUTES

=over 4

=item C<< base >>

=back

=head1 METHODS

=over 4

=item C<< has_base >>

=cut

package AtteanX::RDFQueryTranslator 0.013 {
	use v5.14;
	use warnings;
	use Moo;
	use Data::Dumper;
	use namespace::clean;
	
	use Attean::Algebra;
	use Attean::Expression;
	use Attean::RDF;
	use Scalar::Util qw(blessed);
	use Types::Standard qw(Bool ConsumerOf HashRef);
	has '_in_expr' => (is => 'rw', isa => Bool, default => 0);
	has 'base' => (is => 'rw', isa => ConsumerOf['Attean::IRI'], predicate => 'has_base');
	
=item C<< translate_query( $query ) >>

Returns an algebra object for the given L<RDF::Query> object.

=cut

	sub translate_query {
		my $class	= shift;
		my $query	= shift;
# 		unless ($self->has_base) {
# 			$self->base(Attean::IRI->new(value => $query->{base_uri}->uri_value));
# 		}
		return unless blessed($query);
		my $parsed	= $query->{parsed};
		my $t		= $class->new();
		if (exists $parsed->{base} and my $base = $t->translate($parsed->{base})) {
			$t->base($base);
		}
		my $method	= $parsed->{method};
		my $algebra	= $t->translate($query->pattern);
		if (my $b = $parsed->{bindings}) {
			my @vars	= map { $t->translate($_) } @{ $b->{vars} };
			my $terms	= $b->{terms};
			my @bindings;
			foreach my $row (@$terms) {
				my %binding;
				foreach my $i (0 .. $#vars) {
					if (my $term = $row->[$i]) {
						$binding{ $vars[$i]->value }	= $t->translate($term);
					}
				}
				push(@bindings, Attean::Result->new( bindings => \%binding ));
			}
			my $table	= Attean::Algebra::Table->new( rows => \@bindings, variables => \@vars );
			$algebra	= Attean::Algebra::Join->new( children => [$algebra, $table] );
		}
		if ($method eq 'ASK') {
			$algebra	= Attean::Algebra::Ask->new( children => [$algebra] );
		}
		return $algebra;
	}
	
=item C<< translate( $algebra ) >>

Returns an algebra object for the given L<RDF::Query::Algebra>,
L<RDF::Query::Expression>, or L<RDF::Query::Node> object.

=cut

	sub translate {
		my $self	= shift;
		my $a		= shift;
		Carp::confess "Not a reference? " . Dumper($a) unless blessed($a);
		if ($a->isa('RDF::Query::Algebra::Construct')) {
			my $p		= $self->translate($a->pattern);
			my @triples	= @{ $a->triples || [] };
			if (scalar(@triples) == 1 and $triples[0]->isa('RDF::Query::Algebra::BasicGraphPattern')) {
				@triples	= $triples[0]->triples;
			}
			return Attean::Algebra::Construct->new( children => [$p], triples => [map { $self->translate($_) } @triples] );
		} elsif ($a->isa('RDF::Query::Algebra::Project')) {
			my $p	= $a->pattern;
			my $v	= $a->vars;
			my @vars	= map { variable($_->name) } @$v;
			return Attean::Algebra::Project->new(
				children	=> [ $self->translate($p) ],
				variables	=> [ @vars ],
			);
		} elsif ($a->isa('RDF::Query::Algebra::GroupGraphPattern')) {
			my @p	= map { $self->translate($_) } $a->patterns;
			if (scalar(@p) == 0) {
				return Attean::Algebra::BGP->new();
			} else {
				while (scalar(@p) > 1) {
					my ($l, $r)	= splice(@p, 0, 2);
					unshift(@p, Attean::Algebra::Join->new( children => [$l, $r] ));
				}
				return shift(@p);
			}
		} elsif ($a->isa('RDF::Query::Algebra::BasicGraphPattern')) {
			my @p	= map { $self->translate($_) } $a->triples;
			return Attean::Algebra::BGP->new( triples => \@p );
		} elsif ($a->isa('RDF::Query::Algebra::Triple')) {
			my @nodes	= map { $self->translate($_) } $a->nodes;
			return Attean::TriplePattern->new(@nodes);
		} elsif ($a->isa('RDF::Query::Node::Variable')) {
			my $value	= variable($a->isa("RDF::Query::Node::Variable::ExpressionProxy") ? ("." . $a->name) : $a->name);
			$value		= Attean::ValueExpression->new(value => $value) if ($self->_in_expr);
			return $value;
		} elsif ($a->isa('RDF::Query::Node::Resource')) {
			my $value	= iri($a->uri_value);
			$value		= Attean::ValueExpression->new(value => $value) if ($self->_in_expr);
			return $value;
		} elsif ($a->isa('RDF::Query::Node::Blank')) {
			my $value	= blank($a->blank_identifier);
			$value		= Attean::ValueExpression->new(value => $value) if ($self->_in_expr);
			return $value;
		} elsif ($a->isa('RDF::Query::Node::Literal')) {
			my $value;
			if ($a->has_language) {
				$value	= langliteral($a->literal_value, $a->literal_value_language);
			} elsif ($a->has_datatype) {
				$value	= dtliteral($a->literal_value, $a->literal_datatype);
			} else {
				$value	= literal($a->literal_value);
			}
			$value	= Attean::ValueExpression->new(value => $value) if ($self->_in_expr);
			return $value;
		} elsif ($a->isa('RDF::Query::Algebra::Limit')) {
			my $child	= $a->pattern;
			if ($child->isa('RDF::Query::Algebra::Offset')) {
				my $p	= $self->translate($child->pattern);
				return Attean::Algebra::Slice->new( children => [$p], limit => $a->limit, offset => $child->offset );
			} else {
				my $p	= $self->translate($child);
				return Attean::Algebra::Slice->new( children => [$p], limit => $a->limit );
			}
		} elsif ($a->isa('RDF::Query::Algebra::Offset')) {
			my $p	= $self->translate($a->pattern);
			return Attean::Algebra::Slice->new( children => [$p], offset => $a->offset );
		} elsif ($a->isa('RDF::Query::Algebra::Path')) {
			my $s		= $self->translate($a->start);
			my $o		= $self->translate($a->end);
			my $path	= $self->translate_path($a->path);
			return Attean::Algebra::Path->new( subject => $s, path => $path, object => $o );
		} elsif ($a->isa('RDF::Query::Algebra::Service')) {
			my $endpoint	= $self->translate($a->endpoint);
			my $p			= $self->translate($a->pattern);
			return Attean::Algebra::Service->new( children => [$p], endpoint => $endpoint );
		} elsif ($a->isa('RDF::Query::Algebra::NamedGraph')) {
			my $graph	= $self->translate($a->graph);
			my $p		= $self->translate($a->pattern);
			return Attean::Algebra::Graph->new( children => [$p], graph => $graph );
		} elsif ($a->isa('RDF::Query::Algebra::Filter')) {
			my $p		= $self->translate($a->pattern);
			my $expr	= $self->translate_expr($a->expr);
			return Attean::Algebra::Filter->new( children => [$p], expression => $expr );
		} elsif ($a->isa('RDF::Query::Expression::Binary')) {
			my $op	= $a->op;
			$op		= '=' if ($op eq '==');
			my @ops	= $a->operands;
			my @operands	= map { $self->translate($_) } @ops;
			my $expr	= Attean::BinaryExpression->new( operator => $op, children => \@operands );
			return $expr;
		} elsif ($a->isa('RDF::Query::Expression::Unary')) {
			my $op	= $a->op;
			$op		= '=' if ($op eq '==');
			my ($child)	= $a->operands;
			my $expr	= Attean::UnaryExpression->new( operator => $op, children => [$self->translate($child)] );
			return $expr;
		} elsif ($a->isa('RDF::Query::Algebra::Extend')) {
			my $p		= $self->translate($a->pattern);
			my $vars	= $a->vars;
			foreach my $v (@$vars) {
				if ($v->isa('RDF::Query::Expression::Alias')) {
					my $var		= variable($v->name);
					my $expr	= $v->expression;
					$p	= Attean::Algebra::Extend->new( children => [$p], variable => $var, expression => $self->translate_expr( $expr ) );
				} else {
					die "Unexpected extend expression: " . Dumper($v);
				}
			}
			return $p;
		} elsif ($a->isa('RDF::Query::VariableBindings')) {
			my %bindings;
			foreach my $v ($a->variables) {
				if (my $term = $a->{ $v }) {
					$bindings{ $v }	= $self->translate( $term );
				}
			}
			return Attean::Result->new( bindings => \%bindings );
		} elsif ($a->isa('RDF::Query::Algebra::Table')) {
			my @vars	= map { variable($_) } $a->variables;
			my @rows	= map { $self->translate($_) } $a->rows;
			return Attean::Algebra::Table->new( variables => \@vars, rows => \@rows );
		} elsif ($a->isa('RDF::Query::Algebra::Aggregate')) {
			my $p		= $self->translate($a->pattern);
			my @group;
			foreach my $g ($a->groupby) {
				if ($g->isa('RDF::Query::Expression::Alias')) {
					my $var		= $self->translate($g->alias);
					my $varexpr	= $self->translate_expr($g->alias);
					push(@group, $varexpr);
					my $expr	= $self->translate_expr( $g->expression );
					$p	= Attean::Algebra::Extend->new( children => [$p], variable => $var, expression => $expr );
				} else {
					push(@group, $self->translate_expr($g));
				}
			}
			my @ops		= $a->ops;
			
			my @aggs;
			foreach my $o (@ops) {
				my ($str, $op, $scalar_vars, @vars)	= @$o;
				my $operands	= [map { $self->translate_expr($_) } grep { blessed($_) } @vars];
				my $distinct	= ($op =~ /-DISTINCT$/);
				$op				=~ s/-DISTINCT$//;
				my $expr	= Attean::AggregateExpression->new(
					distinct	=> $distinct,
					operator	=> $op,
					children	=> $operands,
					scalar_vars	=> $scalar_vars,
					variable	=> variable(".$str"),
				);
				push(@aggs, $expr);
			}
			return Attean::Algebra::Group->new(
				children	=> [$p],
				groupby		=> \@group,
				aggregates	=> \@aggs,
			);
		} elsif ($a->isa('RDF::Query::Algebra::Sort')) {
			my $p		= $self->translate($a->pattern);
			my @order	= $a->orderby;
			my @cmps;
			foreach my $o (@order) {
				my ($dir, $e)	= @$o;
				my $asc				= ($dir eq 'ASC');
				my $expr			= $self->translate_expr($e);
				push(@cmps, Attean::Algebra::Comparator->new(ascending => $asc, expression => $expr));
			}
			return Attean::Algebra::OrderBy->new( children => [$p], comparators => \@cmps );
		} elsif ($a->isa('RDF::Query::Algebra::Distinct')) {
			my $p		= $self->translate($a->pattern);
			return Attean::Algebra::Distinct->new( children => [$p] );
		} elsif ($a->isa('RDF::Query::Algebra::Minus')) {
			my $p		= $self->translate($a->pattern);
			my $m		= $self->translate($a->minus);
			return Attean::Algebra::Minus->new( children => [$p, $m] );
		} elsif ($a->isa('RDF::Query::Algebra::Union')) {
			my @p		= map { $self->translate($_) } $a->patterns;
			return Attean::Algebra::Union->new( children => \@p );
		} elsif ($a->isa('RDF::Query::Algebra::Optional')) {
			my $p		= $self->translate($a->pattern);
			my $o		= $self->translate($a->optional);
			return Attean::Algebra::LeftJoin->new( children => [$p, $o] );
		} elsif ($a->isa('RDF::Query::Algebra::SubSelect')) {
			my $q	= $a->query;
			my $p	= $self->translate_query($q);
			return $p;
		} elsif ($a->isa('RDF::Query::Expression::Function')) {
			my $uri		= $a->uri->uri_value;
			my @args	= map { $self->translate_expr($_) } $a->arguments;
			if ($uri eq 'sparql:logical-and') {
				my $algebra	= Attean::BinaryExpression->new( operator => '&&', children => [splice(@args, 0, 2)] );
				while (scalar(@args)) {
					$algebra	= Attean::BinaryExpression->new( operator => '&&', children => [$algebra, shift(@args)] );
				}
				return $algebra;
			} elsif ($uri eq 'sparql:logical-or') {
				my $algebra	= Attean::BinaryExpression->new( operator => '||', children => [splice(@args, 0, 2)] );
				while (scalar(@args)) {
					$algebra	= Attean::BinaryExpression->new( operator => '||', children => [$algebra, shift(@args)] );
				}
				return $algebra;
			} elsif ($uri =~ /^sparql:(.+)$/) {
				if ($1 eq 'exists') {
					# re-translate the pattern as a pattern, not an expression:
					my ($p)	= map { $self->translate_pattern($_) } $a->arguments;
					return Attean::ExistsExpression->new( pattern => $p );
				} else {
					return Attean::FunctionExpression->new( children => \@args, operator => $1, ($self->has_base ? (base => $self->base) : ()) );
				}
			} elsif ($uri =~ m<^http://www[.]w3[.]org/2001/XMLSchema#(?<cast>integer|decimal|float|double|string|boolean|dateTime)$>) {
				my $cast	= $+{cast};
				if ($cast =~ /^(?:integer|decimal|float|double)$/) {
					return Attean::CastExpression->new( children => \@args, datatype => iri($uri) );
				} elsif ($cast eq 'string') {
					return Attean::FunctionExpression->new( children => \@args, operator => 'STR', ($self->has_base ? (base => $self->base) : ()) );
				} elsif ($cast eq 'boolean') {
				
				} elsif ($cast eq 'dateTime') {
				
				}
			}
			warn "Unrecognized function: " . Dumper($uri, \@args);
		}
		Carp::confess "Unrecognized algebra " . ref($a);
	}

=item C<< translate_expr( $algebra ) >>

Returns an expression object for the given L<RDF::Query::Expression> object.

=cut

	sub translate_expr {
		my $self	= shift;
		my $a		= shift;
		my $prev	= $self->_in_expr;
		$self->_in_expr(1);
		my $expr	= $self->translate($a);
		$self->_in_expr($prev);
		return $expr;
	}
	
=item C<< translate_pattern( $algebra ) >>

Translate expression, algebra, or node in a non-expression context.

=cut

	sub translate_pattern {
		my $self	= shift;
		my $a		= shift;
		my $prev	= $self->_in_expr;
		$self->_in_expr(0);
		my $expr	= $self->translate($a);
		$self->_in_expr($prev);
		return $expr;
	}
	
=item C<< translate_path( $path ) >>

Translate a node or property path into an equivalent property path algebra.

=cut

	sub translate_path {
		my $self	= shift;
		my $path	= shift;
		if (blessed($path) and $path->isa('RDF::Query::Node::Resource')) {
			return Attean::Algebra::PredicatePath->new( predicate => $self->translate($path) );
		}
		
		my ($op, @args)	= @$path;
		if ($op eq '!') {
			my @nodes	= map { $self->translate($_) } @args;
			return Attean::Algebra::NegatedPropertySet->new( predicates => \@nodes );
		} elsif ($op eq '/') {
			my @paths	= map { $self->translate_path($_) } @args;
			foreach (@paths) {
				if ($_->does('Attean::API::IRI')) {
					$_	= Attean::Algebra::PredicatePath->new( predicate => $_ );
				}
			}
			return Attean::Algebra::SequencePath->new( children => \@paths );
		} elsif ($op eq '?') {
			my $path	= $self->translate_path(shift(@args));
			return Attean::Algebra::ZeroOrOnePath->new( children => [$path] );
		} elsif ($op eq '*') {
			my $path	= $self->translate_path(shift(@args));
			return Attean::Algebra::ZeroOrMorePath->new( children => [$path] );
		} elsif ($op eq '+') {
			my $path	= $self->translate_path(shift(@args));
			return Attean::Algebra::OneOrMorePath->new( children => [$path] );
		} elsif ($op eq '^') {
			my $path	= $self->translate_path(shift(@args));
			if ($path->does('Attean::API::IRI')) {
				$path	= Attean::Algebra::PredicatePath->new( predicate => $path );
			}
			return Attean::Algebra::InversePath->new( children => [$path] );
		} elsif ($op eq '|') {
			my @paths	= map { $self->translate_path($_) } @args;
			foreach (@paths) {
				if ($_->does('Attean::API::IRI')) {
					$_	= Attean::Algebra::PredicatePath->new( predicate => $_ );
				}
			}
			return Attean::Algebra::AlternativePath->new( children => \@paths );
		}
		die "Unrecognized path: $op";
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO

L<http://www.perlrdf.org/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2016 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
