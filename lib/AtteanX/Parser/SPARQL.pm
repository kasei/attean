use v5.14;
use warnings;

=head1 NAME

AtteanX::Parser::SPARQL - SPARQL 1.1 Parser.

=head1 VERSION

This document describes AtteanX::Parser::SPARQL version 0.017.

=head1 SYNOPSIS

 use AtteanX::Parser::SPARQL;
 my $algbrea = AtteanX::Parser::SPARQL->parse($sparql, $base_uri);
 # or:
 my $parser	= AtteanX::Parser::SPARQL->new();
 my ($algebra) = $parser->parse_list_from_bytes($sparql, $base_uri);
 
 # or to allow parsing of SPARQL 1.1 Updates:
 
 my $algbrea = AtteanX::Parser::SPARQL->parse_update($sparql, $base_uri);
 # or:
 my $parser = AtteanX::Parser::SPARQL->new(update => 1);
 my ($algebra) = $parser->parse_list_from_bytes($sparql, $base_uri);
 
=head1 DESCRIPTION

This module implements a recursive-descent parser for SPARQL 1.1 using the
L<AtteanX::Parser::SPARQLLex> tokenizer. Successful parsing results in an
object whose type is one of: L<Attean::Algebra::Query>,
L<Attean::Algebra::Update>, or L<Attean::Algebra::Sequence>.

=head1 ROLES

This class consumes L<Attean::API::Parser>, L<Attean::API::AtOnceParser>, and
L<Attean::API::AbbreviatingParser>.

=head1 ATTRIBUTES

=over 4

=item C<< canonical_media_type >>

=item C<< media_types >>

=item C<< file_extensions >>

=item C<< handled_type >>

=item C<< lexer >>

=item C<< args >>

=item C<< build >>

=item C<< update >>

=item C<< namespaces >>

=item C<< baseURI >>

=item C<< filters >>

=back

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::SPARQL 0.017;

use strict;
use warnings;
no warnings 'redefine';
use Carp qw(cluck confess croak);

use Attean;
use Data::Dumper;
use URI::NamespaceMap;
use List::MoreUtils qw(zip);
use AtteanX::Parser::SPARQLLex;
use AtteanX::SPARQL::Constants;
use Types::Standard qw(InstanceOf HashRef ArrayRef Bool Str Int);
use Scalar::Util qw(blessed looks_like_number reftype refaddr);

######################################################################

use Moo;

has 'lexer' 		=> (is => 'rw', isa => InstanceOf['AtteanX::Parser::SPARQLLex::Iterator']);
has 'args'			=> (is => 'ro', isa => HashRef);
has 'build'			=> (is => 'rw', isa => HashRef);
has 'update'		=> (is => 'rw', isa => Bool);
has 'baseURI'		=> (is => 'rw');
has '_stack'		=> (is => 'rw', isa => ArrayRef);
has 'filters'		=> (is => 'rw', isa => ArrayRef);
has 'counter'		=> (is => 'rw', isa => Int, default => 0);
has '_pattern_container_stack'	=> (is => 'rw', isa => ArrayRef);

sub file_extensions { return [qw(rq ru)] }

sub canonical_media_type { return "application/sparql-query" }

sub media_types {
	return [qw(application/sparql-query application/sparql-update)];
}

sub handled_type {
	state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Algebra');
	return $ITEM_TYPE;
}

with 'Attean::API::AtOnceParser', 'Attean::API::Parser', 'Attean::API::AbbreviatingParser';
with 'MooX::Log::Any';

sub BUILDARGS {
	my $class	= shift;
	my %args	= @_;
	my $ns		= delete $args{namespaces} // 	URI::NamespaceMap->new();
	my %a		= (args => \%args, namespaces => $ns);
	if (my $handler	= delete $args{handler}) {
		$a{handler}	= $handler;
	}
	return \%a;
}

################################################################################

sub _configure_lexer {
	my $self	= shift;
	my $l		= shift;
	$l->add_regex_rule( qr/RANK/, KEYWORD, sub { return uc(shift) } );
	return $l;
}

=item C<< parse ( $sparql ) >>

Parse the C<< $sparql >> query string and return the resulting
L<Attean::API::Algebra> object.

=cut

sub parse {
	my $self	= shift;
	my $parser	= ref($self) ? $self : $self->new();
	my ($algebra) = $parser->parse_list_from_bytes(@_);
	return $algebra;
}

=item C<< parse_update ( $sparql ) >>

Parse the C<< $sparql >> update string and return the resulting
L<Attean::API::Algebra> object.

=cut

sub parse_update {
	my $self	= shift;
	my $parser	= ref($self) ? $self : $self->new();
	$parser->update(1);
	my ($algebra) = $parser->parse_list_from_bytes(@_);
	return $algebra;
}

=item C<< parse_list_from_io( $fh ) >>

=cut

sub parse_list_from_io {
	my $self	= shift;
	my $p 		= AtteanX::Parser::SPARQLLex->new();
	my $l		= $self->_configure_lexer( $p->parse_iter_from_io(@_) );
	$self->lexer($l);
	$self->baseURI($self->{args}{base});
	my $q		= $self->_parse();
	return unless (ref($q));
	my $a	= $q->{triples}[0];
	return unless (ref($a));
	return $a;
}

=item C<< parse_list_from_bytes( $bytes ) >>

=cut

sub parse_list_from_bytes {
	my $self	= shift;
	my $p 		= AtteanX::Parser::SPARQLLex->new();
	my $l		= $self->_configure_lexer( $p->parse_iter_from_bytes(@_) );
	$self->lexer($l);
	$self->baseURI($self->{args}{base});
	my $q		= $self->_parse();
	return unless (ref($q));
	my $a	= $q->{triples}[0];
	return unless (ref($a));
	return $a;
}

=item C<< parse_nodes ( $string ) >>

Returns a list of L<Attean::API::Term> or L<Attean::API::Variable> objects,
parsed in SPARQL syntax from the supplied C<< $string >>. Parsing is ended
either upon seeing a DOT, or reaching the end of the string.

=cut

sub parse_nodes {
	my $self	= shift;
	my $p 		= AtteanX::Parser::SPARQLLex->new();
	my $l		= $self->_configure_lexer( $p->parse_iter_from_bytes(@_) );
	$self->lexer($l);
	$self->baseURI($self->{args}{base});
	
	my @nodes;
	while ($self->_peek_token) {
		if ($self->_Verb_test) {
			$self->_Verb;
		} else {
			$self->_GraphNode;
		}
		push(@nodes, splice(@{ $self->{_stack} }));
		if ($self->_test_token(DOT)) {
			$self->log->notice('DOT seen in string, stopping here');
			last;
		}
	}
	
	return @nodes;
}

sub _parse {
	my $self	= shift;
	
	unless ($self->update) {
		my $t		= $self->lexer->peek;
		unless (defined($t)) {
			confess "No query string found to parse";
		}
	}

	$self->_stack([]);
	$self->filters([]);
	$self->_pattern_container_stack([]);
	my $triples								= $self->_push_pattern_container();
	my $build								= { sources => [], triples => $triples };
	$self->build($build);
	if ($self->baseURI) {
		$build->{base}	= $self->baseURI;
	}

	$self->_RW_Query();
	delete $build->{star};
	my $data								= $build;
#	$data->{triples}						= $self->_pop_pattern_container();
	return $data;
}

################################################################################


# [1] Query ::= Prologue ( SelectQuery | ConstructQuery | DescribeQuery | AskQuery | LoadUpdate )
sub _RW_Query {
	my $self	= shift;
	$self->_Prologue;

	my $read_query	= 0;
	my $update		= 0;
	while (1) {
		if ($self->_optional_token(KEYWORD, 'SELECT')) {
			$self->_SelectQuery();
			$read_query++;
		} elsif ($self->_optional_token(KEYWORD, 'CONSTRUCT')) {
			$self->_ConstructQuery();
			$read_query++;
		} elsif ($self->_optional_token(KEYWORD, 'DESCRIBE')) {
			$self->_DescribeQuery();
			$read_query++;
		} elsif ($self->_optional_token(KEYWORD, 'ASK')) {
			$self->_AskQuery();
			$read_query++;
		} elsif ($self->_test_token(KEYWORD, 'CREATE')) {
			unless ($self->update) {
				croak "CREATE GRAPH update forbidden in read-only queries";
			}
			$update++;
			$self->_CreateGraph();
		} elsif ($self->_test_token(KEYWORD, 'DROP')) {
			unless ($self->update) {
				croak "DROP GRAPH update forbidden in read-only queries";
			}
			$update++;
			$self->_DropGraph();
		} elsif ($self->_test_token(KEYWORD, 'LOAD')) {
			unless ($self->update) {
				croak "LOAD update forbidden in read-only queries"
			}
			$update++;
			$self->_LoadUpdate();
		} elsif ($self->_test_token(KEYWORD, 'CLEAR')) {
			unless ($self->update) {
				croak "CLEAR GRAPH update forbidden in read-only queries";
			}
			$update++;
			$self->_ClearGraphUpdate();
		} elsif ($self->_test_token(KEYWORD, qr/^(WITH|INSERT|DELETE)/)) {
			unless ($self->update) {
				croak "INSERT/DELETE update forbidden in read-only queries";
			}
			$update++;
			my ($graph);
			if ($self->_optional_token(KEYWORD, 'WITH')) {
				$self->{build}{custom_update_dataset}	= 1;
				$self->_IRIref;
				($graph)	= splice( @{ $self->{_stack} } );
			}
			if ($self->_optional_token(KEYWORD, 'INSERT')) {
				if ($self->_optional_token(KEYWORD, 'DATA')) {
					unless ($self->update) {
						croak "INSERT DATA update forbidden in read-only queries";
					}
					$self->_InsertDataUpdate();
				} else {
					$self->_InsertUpdate($graph);
				}
			} elsif ($self->_optional_token(KEYWORD, 'DELETE')) {
				if ($self->_optional_token(KEYWORD, 'DATA')) {
					unless ($self->update) {
						croak "DELETE DATA update forbidden in read-only queries";
					}
					$self->_DeleteDataUpdate();
				} else {
					$self->_DeleteUpdate($graph);
				}
			}
		} elsif ($self->_test_token(KEYWORD, 'COPY')) {
			$update++;
			$self->_AddCopyMoveUpdate('COPY');
		} elsif ($self->_test_token(KEYWORD, 'MOVE')) {
			$update++;
			$self->_AddCopyMoveUpdate('MOVE');
		} elsif ($self->_test_token(KEYWORD, 'ADD')) {
			$update++;
			$self->_AddCopyMoveUpdate('ADD');
		} elsif ($self->_test_token(SEMICOLON)) {
			$self->_expected_token(SEMICOLON);
			next if ($self->_Query_test);
			last;
		} else {
			if ($self->update and not $self->_peek_token) {
				last;
			}
			
			my $t		= $self->_peek_token;
			return $self->_token_error($t, 'Expected query type');
		}

		last if ($read_query);
		if ($self->_optional_token(SEMICOLON)) {
			if ($self->_Query_test) {
				next;
			}
		}
		last;
	}
	my $count	= scalar(@{ $self->{build}{triples} });
	
	my $t	= $self->_peek_token;
	if ($t) {
		my $type	= AtteanX::SPARQL::Constants::decrypt_constant($t->type);
		croak "Syntax error: Remaining input after query: $type " . Dumper($t->args);
	}

	if ($count == 0 or $count > 1) {
		my @patterns	= splice(@{ $self->{build}{triples} });
		my %seen;
		foreach my $p (@patterns) {
			my @blanks	= $p->blank_nodes;
			foreach my $b (@blanks) {
				if ($seen{$b->value}++) {
					croak "Cannot re-use a blank node label in multiple update operations in a single request";
				}
			}
		}
		my $pattern		= Attean::Algebra::Sequence->new( children => \@patterns );
		$self->_check_duplicate_blanks($pattern);
		$self->{build}{triples}	= [ $pattern ];
	}
	
	my %dataset;
	foreach my $s (@{ $self->{build}{sources} }) {
		my ($iri, $group)	= @$s;
		if ($group eq 'NAMED') {
			push(@{ $dataset{named} }, $iri );
		} else {
			push(@{ $dataset{default} }, $iri );
		}
	}

	my $algebra	= $self->{build}{triples}[0];
	
	if ($update) {
		$self->{build}{triples}[0]	= Attean::Algebra::Update->new( children => [$algebra] );
	} else {
		$self->{build}{triples}[0]	= Attean::Algebra::Query->new( children => [$algebra], dataset => \%dataset );
	}
}

sub _Query_test {
	my $self	= shift;
	return ($self->_test_token(KEYWORD, qr/^(SELECT|CONSTRUCT|DESCRIBE|ASK|LOAD|CLEAR|DROP|ADD|MOVE|COPY|CREATE|INSERT|DELETE|WITH)/i));
}

# [2] Prologue ::= BaseDecl? PrefixDecl*
# [3] BaseDecl ::= 'BASE' IRI_REF
# [4] PrefixDecl ::= 'PREFIX' PNAME_NS IRI_REF
sub _Prologue {
	my $self	= shift;

	my $base;
	my @base;
	if ($self->_optional_token(KEYWORD, 'BASE')) {
		my $iriref	= $self->_expected_token(IRI);
		my $iri		= $iriref->value;
		$base		= $self->new_iri( value => $iri );
		@base		= $base;
		$self->{base}	= $base;
	}

	my %namespaces;
	while ($self->_optional_token(KEYWORD, 'PREFIX')) {
		my $prefix	= $self->_expected_token(PREFIXNAME);
		my @args	= @{ $prefix->args };
		if (scalar(@args) > 1) {
			croak "Syntax error: PREFIX namespace used a full PNAME_LN, not a PNAME_NS";
		}
		my $ns		= substr($prefix->value, 0, length($prefix->value) - 1);
		my $iriref	= $self->_expected_token(IRI);
		my $iri		= $iriref->value;
		if (@base) {
			my $r	= $self->new_iri( value => $iri, base => shift(@base) );
			$iri	= $r->value;
		}
		$namespaces{ $ns }	= $iri;
		$self->namespaces->add_mapping($ns, $iri);
	}

	$self->{build}{namespaces}	= \%namespaces;
	$self->{build}{base}		= $base if (defined($base));

# 	push(@data, (base => $base)) if (defined($base));
# 	return @data;
}

sub _InsertDataUpdate {
	my $self	= shift;
	$self->_expected_token(LBRACE);
	local($self->{__data_pattern})	= 1;
	my @triples	= $self->_ModifyTemplate();
	$self->_expected_token(RBRACE);

	my $insert	= Attean::Algebra::Modify->new(insert => \@triples);
	$self->_add_patterns( $insert );
	$self->{build}{method}		= 'UPDATE';
}

sub _DeleteDataUpdate {
	my $self	= shift;
	$self->_expected_token(LBRACE);
	local($self->{__data_pattern})	= 1;
	local($self->{__no_bnodes})		= "DELETE DATA block";
	my @triples	= $self->_ModifyTemplate();
	$self->_expected_token(RBRACE);
	
	my $delete	= Attean::Algebra::Modify->new(delete => \@triples);
	$self->_add_patterns( $delete );
	$self->{build}{method}		= 'UPDATE';
}

sub _InsertUpdate {
	my $self	= shift;
	my $graph	= shift;
	$self->_expected_token(LBRACE);
	my @triples	= $self->_ModifyTemplate();
	$self->_expected_token(RBRACE);
	
	if ($graph) {
		@triples	= map { $_->as_quad_pattern($graph) } @triples;
	}

	my %dataset;
	while ($self->_optional_token(KEYWORD, 'USING')) {
		$self->{build}{custom_update_dataset}	= 1;
		my $named	= 0;
		if ($self->_optional_token(KEYWORD, 'NAMED')) {
			$named	= 1;
		}
		$self->_IRIref;
		my ($iri)	= splice( @{ $self->{_stack} } );
		if ($named) {
			$dataset{named}{$iri->value}	= $iri;
		} else {
			push(@{ $dataset{default} }, $iri );
		}
	}

	$self->_expected_token(KEYWORD, 'WHERE');
	if ($graph) {
		$self->_GroupGraphPattern;
		my $ggp	= $self->_remove_pattern;
		$ggp	= Attean::Algebra::Graph->new( children => [$ggp], graph => $graph );
		$self->_add_patterns( $ggp );
	} else {
		$self->_GroupGraphPattern;
	}

	my $ggp	= $self->_remove_pattern;

	my @triples_with_fresh_bnodes	= $self->_statements_with_fresh_bnodes(@triples);
	my $insert	= Attean::Algebra::Modify->new( children => [$ggp], insert => \@triples_with_fresh_bnodes, dataset => \%dataset );
	$self->_add_patterns( $insert );
	$self->{build}{method}		= 'UPDATE';
}

sub _statements_with_fresh_bnodes {
	my $self	= shift;
	my @triples	= @_;
	
	my %fresh_blank_map;
	my @triples_with_fresh_bnodes;
	foreach my $t (@triples) {
		my @pos		= ref($t)->variables;
		if ($t->has_blanks) {
			my @terms;
			foreach my $term ($t->values) {
				if ($term->does('Attean::API::Blank')) {
					if (my $b = $fresh_blank_map{$term->value}) {
						push(@terms, $b);
					} else {
						my $id		= $self->counter;
						$self->counter($id+1);
						my $name	= ".b-$id";
						my $b		= Attean::Blank->new($name);
						push(@terms, $b);
						$fresh_blank_map{$term->value}	= $b;
					}
				} else {
					push(@terms, $term);
				}
			}
			push(@triples_with_fresh_bnodes, ref($t)->new(zip @pos, @terms));
		} else {
			push(@triples_with_fresh_bnodes, $t);
		}
	}
	return @triples_with_fresh_bnodes;
}

sub _DeleteUpdate {
	my $self	= shift;
	my $graph	= shift;
	
	my %dataset;
	if ($self->_optional_token(KEYWORD, 'WHERE')) {
		if ($graph) {
			croak "Syntax error: WITH clause cannot be used with DELETE WHERE operations";
		}
		$self->_expected_token(LBRACE);
		my @st	= $self->_ModifyTemplate();
		$self->_expected_token(RBRACE);
		my @patterns;
		my @triples;
		my @quads;
		my @blanks	= grep { $_->does('Attean::API::Blank') } map { $_->values } @st;
		if (scalar(@blanks) > 0) {
			croak "Cannot use blank nodes in a DELETE pattern";
		}
		foreach my $s (@st) {
			if ($s->does('Attean::API::QuadPattern')) {
				push(@quads, $s);
				my $tp	= $s->as_triple_pattern;
				my $bgp	= Attean::Algebra::BGP->new( triples => [$tp] );
				push(@patterns, Attean::Algebra::Graph->new( graph => $s->graph, children => [$bgp] ));
			} else {
				push(@triples, $s);
			}
		}
		push(@patterns, Attean::Algebra::BGP->new( triples => \@triples ));
		my $ggp	= Attean::Algebra::Join->new( children => \@patterns );
		my $update	= Attean::Algebra::Modify->new( children => [$ggp], delete => [@st]);
		$self->_add_patterns( $update );
		$self->{build}{method}		= 'UPDATE';
		return;
	} else {
		my @delete_triples;
		{
			local($self->{__no_bnodes})		= "DELETE block";
			$self->_expected_token(LBRACE);
			@delete_triples	= $self->_ModifyTemplate( $graph );
			$self->_expected_token(RBRACE);
		}
		
		my @insert_triples;
		if ($self->_optional_token(KEYWORD, 'INSERT')) {
			$self->_expected_token(LBRACE);
			@insert_triples	= $self->_ModifyTemplate( $graph );
			@insert_triples	= $self->_statements_with_fresh_bnodes(@insert_triples);
			$self->_expected_token(RBRACE);
		}
		
		if ($graph) {
			@insert_triples	= map { $_->does('Attean::API::QuadPattern') ? $_ : $_->as_quad_pattern($graph) } @insert_triples;
			@delete_triples	= map { $_->does('Attean::API::QuadPattern') ? $_ : $_->as_quad_pattern($graph) } @delete_triples;
		}

		while ($self->_optional_token(KEYWORD, 'USING')) {
			$self->{build}{custom_update_dataset}	= 1;
			my $named	= 0;
			if ($self->_optional_token(KEYWORD, 'NAMED')) {
				$named	= 1;
			}
			$self->_IRIref;
			my ($iri)	= splice( @{ $self->{_stack} } );
			if ($named) {
				$dataset{named}{$iri->value}	= $iri;
			} else {
				push(@{ $dataset{default} }, $iri );
			}
		}
		
		$self->_expected_token(KEYWORD, 'WHERE');
	
		if ($graph) {
			$self->_GroupGraphPattern;
			delete $self->{__no_bnodes};
			my $ggp	= $self->_remove_pattern;
			$ggp	= Attean::Algebra::Graph->new( children => [$ggp], graph => $graph );
			$self->_add_patterns( $ggp );
		} else {
			$self->_GroupGraphPattern;
			delete $self->{__no_bnodes};
		}

		my $ggp	= $self->_remove_pattern;

		my %args	= (children => [$ggp], dataset => \%dataset);
		if (scalar(@insert_triples)) {
			$args{insert}	= \@insert_triples;
		}
		if (scalar(@delete_triples)) {
			$args{delete}	= \@delete_triples;
			my @blanks	= grep { $_->does('Attean::API::Blank') } map { $_->values } @delete_triples;
			if (scalar(@blanks) > 0) {
				croak "Cannot use blank nodes in a DELETE pattern";
			}
		}
		my $update	= Attean::Algebra::Modify->new( %args );
		$self->_add_patterns( $update );
		$self->{build}{method}		= 'UPDATE';
	}
}

sub _ModifyTemplate_test {
	my $self	= shift;
	return 1 if ($self->_TriplesBlock_test);
	return 1 if ($self->_test_token(KEYWORD, 'GRAPH'));
	return 0;
}

sub _ModifyTemplate {
	my $self	= shift;
	my $graph	= shift;
	
	my @triples;
	while ($self->_ModifyTemplate_test) {
		push(@triples, $self->__ModifyTemplate( $graph ));
	}
	
	return @triples;
}

sub __ModifyTemplate {
	my $self	= shift;
	my $graph	= shift;
	local($self->{_modify_template})	= 1;
	if ($self->_TriplesBlock_test) {
		$self->_push_pattern_container;
		$self->_TriplesBlock;
		my ($bgp)	= @{ $self->_pop_pattern_container };
		my @triples	= @{ $bgp->triples };
		if ($graph) {
			@triples	= map { $_->as_quad_pattern($graph) } @triples;
		}
		
		return @triples;
	} else {
		$self->_GraphGraphPattern;
		
		{
			my (@d)	= splice(@{ $self->{_stack} });
			$self->__handle_GraphPatternNotTriples( @d );
		}
		
		my $data	= $self->_remove_pattern;
		my $graph	= $data->graph;
		my @bgps	= $data->subpatterns_of_type('Attean::Algebra::BGP');
		my @triples	= map { $_->as_quad_pattern($graph) } map { @{ $_->triples } } @bgps;
		return @triples;
	}
}

sub _LoadUpdate {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'LOAD');
	my $silent	= $self->_optional_token(KEYWORD, 'SILENT') ? 1 : 0;
	$self->_IRIref;
	my ($iri)	= splice( @{ $self->{_stack} } );
	if ($self->_optional_token(KEYWORD, 'INTO')) {
		$self->_expected_token(KEYWORD, 'GRAPH');
		$self->_IRIref;
		my ($graph)	= splice( @{ $self->{_stack} } );
		my $pat	= Attean::Algebra::Load->new( silent => $silent, url => $iri, graph => $graph );
		$self->_add_patterns( $pat );
	} else {
		my $pat	= Attean::Algebra::Load->new( silent => $silent, url => $iri );
		$self->_add_patterns( $pat );
	}
	$self->{build}{method}		= 'LOAD';
}

sub _CreateGraph {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'CREATE');
	my $silent	= $self->_optional_token(KEYWORD, 'SILENT') ? 1 : 0;
	$self->_expected_token(KEYWORD, 'GRAPH');
	$self->_IRIref;
	my ($graph)	= splice( @{ $self->{_stack} } );
	my $pat	= Attean::Algebra::Create->new( silent => $silent, graph => $graph );
	$self->_add_patterns( $pat );
	$self->{build}{method}		= 'CREATE';
}

sub _ClearGraphUpdate {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'CLEAR');
	my $silent	= $self->_optional_token(KEYWORD, 'SILENT') ? 1 : 0;
	if ($self->_optional_token(KEYWORD, 'GRAPH')) {
		$self->_IRIref;
		my ($graph)	= splice( @{ $self->{_stack} } );
		my $pat	= Attean::Algebra::Clear->new(silent => $silent, target => 'GRAPH', graph => $graph);
		$self->_add_patterns( $pat );
	} elsif ($self->_optional_token(KEYWORD, 'DEFAULT')) {
		my $pat	= Attean::Algebra::Clear->new(silent => $silent, target => 'DEFAULT');
		$self->_add_patterns( $pat );
	} elsif ($self->_optional_token(KEYWORD, 'NAMED')) {
		my $pat	= Attean::Algebra::Clear->new(silent => $silent, target => 'NAMED');
		$self->_add_patterns( $pat );
	} elsif ($self->_optional_token(KEYWORD, 'ALL')) {
		my $pat	= Attean::Algebra::Clear->new(silent => $silent, target => 'ALL');
		$self->_add_patterns( $pat );
	}
	$self->{build}{method}		= 'CLEAR';
}

sub _DropGraph {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'DROP');
	my $silent	= $self->_optional_token(KEYWORD, 'SILENT') ? 1 : 0;
	if ($self->_optional_token(KEYWORD, 'GRAPH')) {
		$self->_IRIref;
		my ($graph)	= splice( @{ $self->{_stack} } );
		my $pat	= Attean::Algebra::Clear->new(drop => 1, silent => $silent, target => 'GRAPH', graph => $graph);
		$self->_add_patterns( $pat );
	} elsif ($self->_optional_token(KEYWORD, 'DEFAULT')) {
		my $pat	= Attean::Algebra::Clear->new(drop => 1, silent => $silent, target => 'DEFAULT');
		$self->_add_patterns( $pat );
	} elsif ($self->_optional_token(KEYWORD, 'NAMED')) {
		my $pat	= Attean::Algebra::Clear->new(drop => 1, silent => $silent, target => 'NAMED');
		$self->_add_patterns( $pat );
	} elsif ($self->_optional_token(KEYWORD, 'ALL')) {
		my $pat	= Attean::Algebra::Clear->new(drop => 1, silent => $silent, target => 'ALL');
		$self->_add_patterns( $pat );
	}
	$self->{build}{method}		= 'CLEAR';
}

sub __graph {
	my $self	= shift;
	if ($self->_optional_token(KEYWORD, 'DEFAULT')) {
		return;
	} else {
		$self->_optional_token(KEYWORD, 'GRAPH');
		$self->_IRIref;
		my ($g)	= splice( @{ $self->{_stack} } );
		return $g;
	}
}

sub _AddCopyMoveUpdate {
	my $self	= shift;
	my $op		= shift;
	$self->_expected_token(KEYWORD, $op);
	my $silent	= $self->_optional_token(KEYWORD, 'SILENT') ? 1 : 0;
	
	my %args	= (silent => $silent);
	if ($op eq 'COPY') {
		$args{drop_destination}	=1;
	} elsif ($op eq 'MOVE') {
		$args{drop_destination}	= 1;
		$args{drop_source}		= 1;
	}
	if (my $from = $self->__graph()) {
		$args{source}	= $from;
	}
	$self->_expected_token(KEYWORD, 'TO');
	if (my $to = $self->__graph()) {
		$args{destination}	= $to;
	}
	my $pattern	= Attean::Algebra::Add->new( %args );
	$self->_add_patterns( $pattern );
	$self->{build}{method}		= 'UPDATE';
}

# [5] SelectQuery ::= 'SELECT' ( 'DISTINCT' | 'REDUCED' )? ( Var+ | '*' ) DatasetClause* WhereClause SolutionModifier
sub _SelectQuery {
	my $self	= shift;
	if ($self->_optional_token(KEYWORD, qr/^(DISTINCT)/)) {
		$self->{build}{options}{distinct}	= 1;
	} elsif ($self->_optional_token(KEYWORD, qr/^(REDUCED)/)) {
		$self->{build}{options}{distinct}	= 2;
	}
	
	my ($star, $exprs, $vars)	= $self->__SelectVars;
	my @exprs	= @$exprs;
	
	$self->_DatasetClause();
	
	$self->_WhereClause;
	$self->_SolutionModifier($vars);
	
	if ($self->_optional_token(KEYWORD, 'VALUES')) {
		my @vars;
# 		$self->_Var;
# 		push( @vars, splice(@{ $self->{_stack} }));
		my $parens	= 0;
		if ($self->_optional_token(NIL)) {
			$parens	= 1;
		} else {
			if ($self->_optional_token(LPAREN)) {
				$parens	= 1;
			}
			while ($self->_test_token(VAR)) {
				$self->_Var;
				push( @vars, splice(@{ $self->{_stack} }));
			}
			if ($parens) {
				$self->_expected_token(RPAREN);
			}
		}
		
		my $count	= scalar(@vars);
		if (not($parens) and $count == 0) {
			croak "Syntax error: Expected VAR in inline data declaration";
		} elsif (not($parens) and $count > 1) {
			croak "Syntax error: Inline data declaration can only have one variable when parens are omitted";
		}
		
		my $short	= (not($parens) and $count == 1);
		$self->_expected_token(LBRACE);
		if ($self->_optional_token(NIL)) {
		
		} else {
			if (not($short) or ($short and $self->_test_token(LPAREN))) {
				while ($self->_test_token(LPAREN)) {
					my $terms	= $self->_Binding($count);
					push( @{ $self->{build}{bindings}{terms} }, $terms );
				}
			} else {
				while ($self->_BindingValue_test) {
					$self->_BindingValue;
					my ($term)	= splice(@{ $self->{_stack} });
					push( @{ $self->{build}{bindings}{terms} }, [$term] );
				}
			}
		}
		
		$self->_expected_token(RBRACE);

		my $bindings	= delete $self->{build}{bindings};
		my @rows	= @{ $bindings->{terms} || [] };
		my @vbs;
		foreach my $r (@rows) {
			my %d;
			foreach my $i (0 .. $#{ $r }) {
				if (blessed($r->[$i])) {
					$d{ $vars[$i]->value }	= $r->[$i];
				}
			}
			my $r	= Attean::Result->new(bindings => \%d);
			push(@vbs, $r);
		}
		my $table	= Attean::Algebra::Table->new( variables => \@vars, rows => \@vbs );
		my $pattern	= pop(@{ $self->{build}{triples} });
		push(@{ $self->{build}{triples} }, $self->_new_join($pattern, $table));
	}
	
	my %projected	= map { $_ => 1 } $self->__solution_modifiers( $star, @exprs );
	delete $self->{build}{options};
	$self->{build}{method}		= 'SELECT';
}

sub __SelectVars {
	my $self	= shift;
	my $star	= 0;
	my @vars;
	my $count	= 0;
	my @exprs;
	while ($self->_test_token(STAR) or $self->__SelectVar_test) {
		if ($self->_test_token(STAR)) {
			$self->{build}{star}++;
			$self->_expected_token(STAR);
			$star	= 1;
			$count++;
			last;
		} else {
			my @s	= $self->__SelectVar;
			if (scalar(@s) > 1) {
				my ($var, $expr)	= @s;
				push(@exprs, $var->value, $expr);
			} else {
				my $var	= $s[0];
				push(@exprs, $var->value, $var);
			}
			push(@vars, shift(@s));
			$count++;
		}
	}
	
	my %seen;
	foreach my $v (@vars) {
		if ($v->does('Attean::API::Variable')) {
			my $name	= $v->value;
			if ($seen{ $name }++) {
				croak "Syntax error: Repeated variable ($name) used in projection list";
			}
		}
	}
	
	$self->{build}{variables}	= \@vars;
	if ($count == 0) {
		croak "Syntax error: No select variable or expression specified";
	}
	return $star, \@exprs, \@vars;
}

sub _BrackettedAliasExpression {
	my $self	= shift;
	$self->_expected_token(LPAREN);
	$self->_Expression;
	my ($expr)	= splice(@{ $self->{_stack} });
	$self->_expected_token(KEYWORD, 'AS');
	$self->_Var;
	my ($var)	= splice(@{ $self->{_stack} });
	$self->_expected_token(RPAREN);
	
	return ($var, $expr);
}

sub __SelectVar_test {
	my $self	= shift;
	local($self->{__aggregate_call_ok})	= 1;
#	return 1 if $self->_BuiltInCall_test;
	return 1 if $self->_test_token(LPAREN);
	return $self->_test_token(VAR);
}

sub __SelectVar {
	my $self	= shift;
	local($self->{__aggregate_call_ok})	= 1;
	if ($self->_test_token(LPAREN)) {
		my ($var, $expr)	= $self->_BrackettedAliasExpression;
		return ($var, $expr);
	} else {
		$self->_Var;
		my ($var)	= splice(@{ $self->{_stack} });
		return $var;
	}
}

# [6] ConstructQuery ::= 'CONSTRUCT' ConstructTemplate DatasetClause* WhereClause SolutionModifier
sub _ConstructQuery {
	my $self	= shift;
	my $shortcut	= 1;
	if ($self->_test_token(LBRACE)) {
		$shortcut	= 0;
		$self->_ConstructTemplate;
	}
	$self->_DatasetClause();
	if ($shortcut) {
		$self->_TriplesWhereClause;
	} else {
		$self->_WhereClause;
	}
	
	$self->_SolutionModifier();
	
	my $pattern		= $self->{build}{triples}[0];
	my $triples		= delete $self->{build}{construct_triples};
	if (blessed($triples) and $triples->isa('Attean::Algebra::BGP')) {
		$triples	= $triples->triples;
	}
# 	my @triples;
# 	warn $triples;
# 	foreach my $t (@{ $triples // [] }) {
# 		if ($t->isa('Attean::Algebra::BGP')) {
# 			push(@triples, @{ $t->triples });
# 		} else {
# 			push(@triples, $t);
# 		}
# 	}
	my $construct	= Attean::Algebra::Construct->new( children => [$pattern], triples => $triples );
	$self->{build}{triples}[0]	= $construct;
	$self->{build}{method}		= 'CONSTRUCT';
}

# [7] DescribeQuery ::= 'DESCRIBE' ( VarOrIRIref+ | '*' ) DatasetClause* WhereClause? SolutionModifier
sub _DescribeQuery {
	my $self	= shift;
	
	my $star	= 0;
	if ($self->_optional_token(STAR)) {
		$star	= 1;
		$self->{build}{variables}	= ['*'];
	} else {
		$self->_VarOrIRIref;
		while ($self->_VarOrIRIref_test) {
			$self->_VarOrIRIref;
		}
		$self->{build}{variables}	= [ splice(@{ $self->{_stack} }) ];
	}
	
	$self->_DatasetClause();
	
	if ($self->_WhereClause_test) {
		$self->_WhereClause;
	} else {
		my $pattern	= Attean::Algebra::BGP->new();
		$self->_add_patterns( $pattern );
	}
	
	$self->_SolutionModifier();
	$self->{build}{method}		= 'DESCRIBE';

	my $pattern	= $self->{build}{triples}[0];
	my $terms	= $star ? [map { Attean::Variable->new($_) } $pattern->in_scope_variables] : $self->{build}{variables};
	$self->{build}{triples}[0]	= Attean::Algebra::Describe->new( terms => $terms, children => [$pattern] );
}

# [8] AskQuery ::= 'ASK' DatasetClause* WhereClause
sub _AskQuery {
	my $self	= shift;
	
	$self->_DatasetClause();
	
	$self->_WhereClause;
	
	$self->{build}{variables}	= [];
	$self->{build}{method}		= 'ASK';
	
	
	my $pattern	= $self->{build}{triples}[0];
	$self->{build}{triples}[0]	= Attean::Algebra::Ask->new( children => [$pattern] );
}

# sub _DatasetClause_test {
# 	my $self	= shift;
# 	return $self->_test_token(KEYWORD, 'FROM');
# }

# [9] DatasetClause ::= 'FROM' ( DefaultGraphClause | NamedGraphClause )
sub _DatasetClause {
	my $self	= shift;
	
# 	my @dataset;
 	$self->{build}{sources}	= [];
 	while ($self->_optional_token(KEYWORD, 'FROM')) {
 		if ($self->_test_token(KEYWORD, 'NAMED')) {
			$self->_NamedGraphClause;
		} else {
			$self->_DefaultGraphClause;
		}
	}
}

# [10] DefaultGraphClause ::= SourceSelector
sub _DefaultGraphClause {
	my $self	= shift;
	$self->_SourceSelector;
	my ($source)	= splice(@{ $self->{_stack} });
	push( @{ $self->{build}{sources} }, [$source, 'DEFAULT'] );
}

# [11] NamedGraphClause ::= 'NAMED' SourceSelector
sub _NamedGraphClause {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'NAMED');
	$self->_SourceSelector;
	my ($source)	= splice(@{ $self->{_stack} });
	push( @{ $self->{build}{sources} }, [$source, 'NAMED'] );
}

# [12] SourceSelector ::= IRIref
sub _SourceSelector {
	my $self	= shift;
	$self->_IRIref;
}

# [13] WhereClause ::= 'WHERE'? GroupGraphPattern
sub _WhereClause_test {
	my $self	= shift;
	return 1 if ($self->_test_token(KEYWORD, 'WHERE'));
	return 1 if ($self->_test_token(LBRACE));
	return 0;
}
sub _WhereClause {
	my $self	= shift;
	$self->_optional_token(KEYWORD, 'WHERE');
	$self->_GroupGraphPattern;
	
	my $ggp	= $self->_peek_pattern;
	$self->_check_duplicate_blanks($ggp);
}

sub _check_duplicate_blanks {
	my $self	= shift;
	my $p		= shift;
# 	warn 'TODO: $ggp->_check_duplicate_blanks'; # XXXXXXXX
# 	my @children	= @{ $ggp->children };
# 	my %seen;
# 	foreach my $c (@{ $ggp->children }) {
# 		my @blanks		= $c->blank_nodes;
# 		foreach my $b (@blanks) {
# 			my $id	= $b->value;
# 			if ($seen{ $id }++) {
# 				warn $ggp->as_string;
# 				croak "Same blank node identifier ($id) used in more than one BasicGraphPattern.";
# 			}
# 		}
# 	}
	return 1;
}

sub _TriplesWhereClause {
	my $self	= shift;
	$self->_push_pattern_container;
	
	$self->_expected_token(KEYWORD, 'WHERE');
	$self->_expected_token(LBRACE);
	if ($self->_TriplesBlock_test) {
		$self->_TriplesBlock;
	}
	$self->_expected_token(RBRACE);
	
	my $cont		= $self->_pop_pattern_container;
	$self->{build}{construct_triples}	= $cont->[0];
	
	my $pattern	= $self->_new_join(@$cont);
	$self->_add_patterns( $pattern );
}

# sub _Binding_test {
# 	my $self	= shift;
# 	return $self->_test_token(LPAREN);
# }

sub _Binding {
	my $self	= shift;
	my $count	= shift;
	
	$self->_expected_token(LPAREN);
	
	my @terms;
	foreach my $i (1..$count) {
		unless ($self->_BindingValue_test) {
			my $found	= $i-1;
			croak "Syntax error: Expected $count BindingValues but only found $found";
		}
		$self->_BindingValue;
		push( @terms, splice(@{ $self->{_stack} }));
	}
	$self->_expected_token(RPAREN);
	return \@terms;
}

sub _BindingValue_test {
	my $self	= shift;
	return 1 if ($self->_IRIref_test);
	return 1 if ($self->_test_token(KEYWORD, 'UNDEF'));
	return 1 if ($self->_test_literal_token);
	return 1 if ($self->_IRIref_test);
	return 1 if ($self->_test_token(BNODE));
	return 1 if ($self->_test_token(NIL));
	return 0;
}

sub _BindingValue {
	my $self	= shift;
	if ($self->_optional_token(KEYWORD, 'UNDEF')) {
		push(@{ $self->{_stack} }, undef);
	} else {
		$self->_GraphTerm;
	}
}

# [20]  	GroupCondition	  ::=  	( BuiltInCall | FunctionCall | '(' Expression ( 'AS' Var )? ')' | Var )
sub __GroupByVar_test {
	my $self	= shift;
	return 1 if ($self->_BuiltInCall_test);
	return 1 if ($self->_IRIref_test);
	return 1 if ($self->_test_token(LPAREN));
	return 1 if ($self->_test_token(VAR));
	return 0;
}

sub __GroupByVar {
	my $self	= shift;
	if ($self->_optional_token(LPAREN)) {
		$self->_Expression;
		my ($expr)	= splice(@{ $self->{_stack} });
		if ($self->_optional_token(KEYWORD, 'AS')) {
			$self->_Var;
			my ($var)	= splice(@{ $self->{_stack} });
			push(@{ $self->{build}{__group_vars} }, [$var, $expr]);
			my $vexpr	= Attean::ValueExpression->new( value => $var );
			$self->_add_stack( $vexpr );
		} else {
			$self->_add_stack( $expr );
		}
		$self->_expected_token(RPAREN);
		
	} elsif ($self->_IRIref_test) {
		$$self->_FunctionCall;
	} elsif ($self->_BuiltInCall_test) {
		$self->_BuiltInCall;
	} else {
		$self->_Var;
		my $var		= pop(@{ $self->{_stack} });
		my $expr	= Attean::ValueExpression->new(value => $var);
		$self->_add_stack($expr);
	}
}

# [14] SolutionModifier ::= OrderClause? LimitOffsetClauses?
sub _SolutionModifier {
	my $self	= shift;
	my $vars	= shift // [];
	
	if ($self->_test_token(KEYWORD, 'GROUP')) {
		$self->_GroupClause($vars);
	}
	
	if ($self->_test_token(KEYWORD, 'RANK')) {
		$self->_RankClause;
	}
	
	if ($self->_test_token(KEYWORD, 'HAVING')) {
		$self->_HavingClause;
	}
	
	if ($self->_OrderClause_test) {
		$self->_OrderClause;
	}
	
	if ($self->_LimitOffsetClauses_test) {
		$self->_LimitOffsetClauses;
	}
}

sub _GroupClause {
	my $self	= shift;
	my $vars	= shift;
	$self->_expected_token(KEYWORD, 'GROUP');
	$self->_expected_token(KEYWORD, 'BY');
	
	if ($self->{build}{star}) {
		croak "Syntax error: SELECT * cannot be used with aggregate grouping";
	}
	
	$self->{build}{__aggregate}	||= {};
	my @vars;
	$self->__GroupByVar;
	my ($v)	= splice(@{ $self->{_stack} });
	push( @vars, $v );
	while ($self->__GroupByVar_test) {
		$self->__GroupByVar;
		my ($v)	= splice(@{ $self->{_stack} });
		push( @vars, $v );
	}

	my %seen;
	foreach my $v (@vars) {
		my $var	= $v->value;
		if ($var->does('Attean::API::Variable')) {
			my $name	= $var->value;
			$seen{ $name }++;
		}
	}
	
# 	warn 'TODO: verify that projection only includes aggregates and grouping variables'; # XXXXX
# 	foreach my $v (@$vars) {
# 		if ($v->does('Attean::API::Variable')) {
# 			my $name	= $v->value;
# 			unless ($seen{ $name }) {
# 				croak "Syntax error: Variable used in projection but not present in aggregate grouping ($name)";
# #				throw ::Error::ParseError -text => "Syntax error: Variable used in projection but not present in aggregate grouping ($name)";
# 			}
# 		}
# 	}
	
	$self->{build}{__group_by}	= \@vars;
}

sub _RankClause {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'RANK');
	$self->_expected_token(LPAREN);
	$self->_OrderCondition;
	my @order;
	push(@order, splice(@{ $self->{_stack} }));
	while ($self->_OrderCondition_test) {
		$self->_OrderCondition;
		push(@order, splice(@{ $self->{_stack} }));
	}
	$self->_expected_token(RPAREN);
	$self->_expected_token(KEYWORD, 'AS');
	$self->_Var;
	my ($var)	= splice(@{ $self->{_stack} });
	
	my @exprs;
	my %ascending;
	foreach my $o (@order) {
		my ($dir, $expr)	= @$o;
		push(@exprs, $expr);
		$ascending{ $expr->value->value }	= ($dir eq 'ASC') ? 1 : 0; # TODO: support ranking by complex expressions, not just variables
	}
	my $r	= Attean::AggregateExpression->new(
		distinct	=> 0,
		operator	=> 'RANK',
		children	=> \@exprs,
		scalar_vars	=> {
			ascending	=> \%ascending,
		},
		variable	=> $var,
	);
	
	$self->{build}{__aggregate}{ $var->value }	= [ $var, $r ];
}

sub _HavingClause {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'HAVING');
	$self->{build}{__aggregate}	||= {};
	local($self->{__aggregate_call_ok})	= 1;
	$self->_Constraint;
	my ($expr) = splice(@{ $self->{_stack} });
	$self->{build}{__having}	= $expr;
}

# [15] LimitOffsetClauses ::= ( LimitClause OffsetClause? | OffsetClause LimitClause? )
sub _LimitOffsetClauses_test {
	my $self	= shift;
	return 1 if ($self->_test_token(KEYWORD, 'LIMIT'));
	return 1 if ($self->_test_token(KEYWORD, 'OFFSET'));
	return 0;
}

sub _LimitOffsetClauses {
	my $self	= shift;
	if ($self->_LimitClause_test) {
		$self->_LimitClause;
		if ($self->_OffsetClause_test) {
			$self->_OffsetClause;
		}
	} else {
		$self->_OffsetClause;
		if ($self->_LimitClause_test) {
			$self->_LimitClause;
		}
	}
}

# [16] OrderClause ::= 'ORDER' 'BY' OrderCondition+
sub _OrderClause_test {
	my $self	= shift;
	return 1 if ($self->_test_token(KEYWORD, 'ORDER'));
	return 0;
}

sub _OrderClause {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'ORDER');
	$self->_expected_token(KEYWORD, 'BY');
	my @order;
	$self->{build}{__aggregate}	||= {};
	local($self->{__aggregate_call_ok})	= 1;
	$self->_OrderCondition;
	push(@order, splice(@{ $self->{_stack} }));
	while ($self->_OrderCondition_test) {
		$self->_OrderCondition;
		push(@order, splice(@{ $self->{_stack} }));
	}
	$self->{build}{options}{orderby}	= \@order;
}

# [17] OrderCondition ::= ( ( 'ASC' | 'DESC' ) BrackettedExpression ) | ( Constraint | Var )
sub _OrderCondition_test {
	my $self	= shift;
	return 1 if ($self->_test_token(KEYWORD, 'ASC'));
	return 1 if ($self->_test_token(KEYWORD, 'DESC'));
	return 1 if ($self->_test_token(VAR));
	return 1 if $self->_Constraint_test;
	return 0;
}

sub _OrderCondition {
	my $self	= shift;
	my $dir	= 'ASC';
	if (my $t = $self->_optional_token(KEYWORD, qr/^(ASC|DESC)/)) {
		$dir	= $t->value;
		$self->_BrackettedExpression;
	} elsif ($self->_test_token(VAR)) {
		$self->_Var;
		my $var		= pop(@{ $self->{_stack} });
		my $expr	= Attean::ValueExpression->new(value => $var);
		$self->_add_stack($expr);
	} else {
		$self->_Constraint;
	}
	my ($expr)	= splice(@{ $self->{_stack} });
	$self->_add_stack( [ $dir, $expr ] );
}

# [18] LimitClause ::= 'LIMIT' INTEGER
sub _LimitClause_test {
	my $self	= shift;
	return ($self->_test_token(KEYWORD, 'LIMIT'));
}

sub _LimitClause {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'LIMIT');
	my $t		= $self->_expected_token(INTEGER);
	$self->{build}{options}{limit}	= $t->value;
}

# [19] OffsetClause ::= 'OFFSET' INTEGER
sub _OffsetClause_test {
	my $self	= shift;
	return ($self->_test_token(KEYWORD, 'OFFSET'));
}

sub _OffsetClause {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'OFFSET');
	my $t		= $self->_expected_token(INTEGER);
	$self->{build}{options}{offset}	= $t->value;
}

# [20] GroupGraphPattern ::= '{' TriplesBlock? ( ( GraphPatternNotTriples | Filter ) '.'? TriplesBlock? )* '}'
sub _GroupGraphPattern {
	my $self	= shift;
	
	$self->_expected_token(LBRACE);
	
	if ($self->_SubSelect_test) {
		$self->_SubSelect;
	} else {
		$self->_GroupGraphPatternSub;
	}

	$self->_expected_token(RBRACE);
}

sub _GroupGraphPatternSub {
	my $self	= shift;
	$self->_push_pattern_container;
	
	my $got_pattern	= 0;
	my $need_dot	= 0;
	if ($self->_TriplesBlock_test) {
		$need_dot	= 1;
		$got_pattern++;
		$self->_TriplesBlock;
	}
	
	while (not $self->_test_token(RBRACE)) {
		my $cur	= $self->_peek_token;
		if ($self->_GraphPatternNotTriples_test) {
			$need_dot	= 0;
			$got_pattern++;
			$self->_GraphPatternNotTriples;
			my (@data)	= splice(@{ $self->{_stack} });
			$self->__handle_GraphPatternNotTriples( @data );
		} elsif ($self->_test_token(KEYWORD, 'FILTER')) {
			$got_pattern++;
			$need_dot	= 0;
			$self->_Filter;
		}
		
		if ($need_dot or $self->_test_token(DOT)) {
			$self->_expected_token(DOT);
			if ($got_pattern) {
				$need_dot		= 0;
				$got_pattern	= 0;
			} else {
				croak "Syntax error: Extra dot found without preceding pattern";
			}
		}
		
		if ($self->_TriplesBlock_test) {
			my $peek	= $self->_peek_pattern;
			if (blessed($peek) and $peek->isa('Attean::Algebra::BGP')) {
				$self->_TriplesBlock;
				my $rhs		= $self->_remove_pattern;
				my $lhs		= $self->_remove_pattern;
				if ($rhs->isa('Attean::Algebra::BGP')) {
					my $merged	= $self->__new_bgp( map { @{ $_->triples } } ($lhs, $rhs) );
					$self->_add_patterns( $merged );
				} else {
					my $merged	= $self->_new_join($lhs, $rhs);
					$self->_add_patterns( $merged );
				}
			} else {
				$self->_TriplesBlock;
			}
		}

		my $t	= $self->_peek_token;
		last if (refaddr($t) == refaddr($cur));
	}
	my $cont		= $self->_pop_pattern_container;

	my @filters		= splice(@{ $self->{filters} });
	my @patterns;
	my $pattern		= $self->_new_join(@$cont);
	if (@filters) {
		while (my $f = shift @filters) {
			$pattern	= Attean::Algebra::Filter->new( children => [$pattern], expression => $f );
		}
	}
	$self->_add_patterns( $pattern );
}

sub __handle_GraphPatternNotTriples {
	my $self	= shift;
	my $data	= shift;
	my ($class, @args)	= @$data;
	if ($class =~ /^Attean::Algebra::(LeftJoin|Minus)$/) {
		my $cont	= $self->_pop_pattern_container;
		my $ggp		= $self->_new_join(@$cont);
		$self->_push_pattern_container;
		# my $ggp	= $self->_remove_pattern();
		unless ($ggp) {
			$ggp	= Attean::Algebra::BGP->new();
		}
		
		my $opt	= $class->new( children => [$ggp, @args] );
		$self->_add_patterns( $opt );
	} elsif ($class eq 'Attean::Algebra::Table') {
 		my ($table)	= @args;
		$self->_add_patterns( $table );
	} elsif ($class eq 'Attean::Algebra::Extend') {
		my $cont	= $self->_pop_pattern_container;
		my $ggp		= $self->_new_join(@$cont);
		$self->_push_pattern_container;
		# my $ggp	= $self->_remove_pattern();
		unless ($ggp) {
			$ggp	= Attean::Algebra::BGP->new();
		}
		my ($var, $expr)	= @args;
		my %in_scope	= map { $_ => 1 } $ggp->in_scope_variables;
		if (exists $in_scope{ $var->value }) {
			croak "Syntax error: BIND used with variable already in scope";
		}
		my $bind	= Attean::Algebra::Extend->new( children => [$ggp], variable => $var, expression => $expr );
		$self->_add_patterns( $bind );
	} elsif ($class eq 'Attean::Algebra::Service') {
		my ($endpoint, $pattern, $silent)	= @args;
		if ($endpoint->does('Attean::API::Variable')) {
			# SERVICE ?var
			croak "SERVICE ?var not implemented";
		} else {
			# SERVICE <endpoint>
			# no-op
			my $service	= Attean::Algebra::Service->new( children => [$pattern], endpoint => $endpoint, silent => $silent );
			$self->_add_patterns( $service );
		}
	} elsif ($class =~ /Attean::Algebra::(Union|Graph|Join)$/) {
		# no-op
	} else {
		croak 'Unrecognized GraphPattern: ' . $class;
	}
}

sub _SubSelect_test {
	my $self	= shift;
	return $self->_test_token(KEYWORD, 'SELECT');
}

sub _SubSelect {
	my $self	= shift;
	my $pattern;
	{
		local($self->{namespaces})				= $self->{namespaces};
		local($self->{_stack})					= [];
		local($self->{filters})					= [];
		local($self->{_pattern_container_stack})	= [];

		my $triples								= $self->_push_pattern_container();
		local($self->{build})					= { triples => $triples};
		if ($self->{baseURI}) {
			$self->{build}{base}	= $self->{baseURI};
		}
		
		$self->_expected_token(KEYWORD, 'SELECT');
		if (my $t = $self->_optional_token(KEYWORD, qr/^(DISTINCT|REDUCED)/)) {
			my $mod	= $t->value;
			$self->{build}{options}{lc($mod)}	= 1;
		}
		
		my ($star, $exprs, $vars)	= $self->__SelectVars;
		my @exprs	= @$exprs;
		
		$self->_WhereClause;
		$self->_SolutionModifier($vars);
		
		if ($self->{build}{options}{orderby}) {
			my $order	= delete $self->{build}{options}{orderby};
			my $pattern	= pop(@{ $self->{build}{triples} });
			
			my @order	= @$order;
			my @cmps;
			foreach my $o (@order) {
				my ($dir, $expr)	= @$o;
				my $asc				= ($dir eq 'ASC');
				push(@cmps, Attean::Algebra::Comparator->new(ascending => $asc, expression => $expr));
			}
			my $sort	= Attean::Algebra::OrderBy->new( children => [$pattern], comparators => \@cmps );
			push(@{ $self->{build}{triples} }, $sort);
		}
		
		if ($self->_optional_token(KEYWORD, 'VALUES')) {
			my @vars;
			my $parens	= 0;
			if ($self->_optional_token(LPAREN)) {
				$parens	= 1;
			}
			while ($self->_test_token(VAR)) {
				$self->_Var;
				push( @vars, splice(@{ $self->{_stack} }));
			}
			if ($parens) {
				$self->_expected_token(RPAREN);
			}
			my $count	= scalar(@vars);
			if (not($parens) and $count == 0) {
				croak "Syntax error: Expected VAR in inline data declaration";
			} elsif (not($parens) and $count > 1) {
				croak "Syntax error: Inline data declaration can only have one variable when parens are omitted";
			}
			
			my $short	= (not($parens) and $count == 1);
			$self->_expected_token(LBRACE);
			if (not($short) or ($short and $self->_test_token(LPAREN))) {
				while ($self->_test_token(LPAREN)) {
					my $terms	= $self->_Binding($count);
					push( @{ $self->{build}{bindings}{terms} }, $terms );
				}
			} else {
				while ($self->_BindingValue_test) {
					$self->_BindingValue;
					my ($term)	= splice(@{ $self->{_stack} });
					push( @{ $self->{build}{bindings}{terms} }, [$term] );
				}
			}
			
			$self->_expected_token(RBRACE);
			$self->{build}{bindings}{vars}	= \@vars;
			
			my $bindings	= delete $self->{build}{bindings};
			my @rows	= @{ $bindings->{terms} };
			my @vbs;
			foreach my $r (@rows) {
				my %d;
				foreach my $i (0 .. $#{ $r }) {
					if (blessed($r->[$i])) {
						$d{ $vars[$i]->value }	= $r->[$i];
					}
				}
				my $r	= Attean::Result->new(bindings => \%d);
				push(@vbs, $r);
			}
			my $table	= Attean::Algebra::Table->new( variables => \@vars, rows => \@vbs );
			my $pattern	= pop(@{ $self->{build}{triples} });
			push(@{ $self->{build}{triples} }, $self->_new_join($pattern, $table));
		}
		
		$self->__solution_modifiers( $star, @exprs );
		
		delete $self->{build}{options};
		my $data	= delete $self->{build};
		$pattern	= $data->{triples}[0];
		$pattern	= Attean::Algebra::Query->new( children => [$pattern], subquery => 1 );
	}
	
	$self->_add_patterns( $pattern );
}

# [21] TriplesBlock ::= TriplesSameSubject ( '.' TriplesBlock? )?
sub _TriplesBlock_test {
	my $self	= shift;
	# VarOrTerm | TriplesNode -> (Var | GraphTerm) | (Collection | BlankNodePropertyList) -> Var | IRIref | RDFLiteral | NumericLiteral | BooleanLiteral | BlankNode | NIL | Collection | BlankNodePropertyList
	# but since a triple can't start with a literal, this is reduced to:
	# Var | IRIref | BlankNode | NIL
	return 1 if ($self->_test_token(VAR));
	return 1 if ($self->_test_token(NIL));
	return 1 if ($self->_test_token(ANON));
	return 1 if ($self->_test_token(BNODE));
	return 1 if ($self->_test_token(LPAREN));
	return 1 if ($self->_test_token(LBRACKET));
	return 1 if ($self->_IRIref_test);
	return 1 if ($self->_test_literal_token);
	return 0;
}

sub _test_literal_token {
	my $self	= shift;
	return 1 if ($self->_test_token(STRING1D));
	return 1 if ($self->_test_token(STRING3D));
	return 1 if ($self->_test_token(STRING1S));
	return 1 if ($self->_test_token(STRING3S));
	return 1 if ($self->_test_token(DECIMAL));
	return 1 if ($self->_test_token(DOUBLE));
	return 1 if ($self->_test_token(INTEGER));
	return 1 if ($self->_test_token(BOOLEAN));
	return 0;
}


sub _TriplesBlock {
	my $self	= shift;
	$self->_push_pattern_container;
	$self->__TriplesBlock;
	my $triples		= $self->_pop_pattern_container;
	my $bgp			= $self->__new_bgp( @$triples );
	$self->_add_patterns( $bgp );
}

## this one (with two underscores) doesn't pop patterns off the stack and make a BGP.
## instead, things are left on the stack so we can recurse without doing the wrong thing.
## the one with one underscore (_TriplesBlock) will pop everything off and make the BGP.
sub __TriplesBlock {
	my $self	= shift;
	my $got_dot	= 0;
TRIPLESBLOCKLOOP:
	$self->_TriplesSameSubjectPath;
	while ($self->_test_token(DOT)) {
		if ($got_dot) {
			croak "Syntax error: found extra DOT after TriplesBlock";
		}
		$self->_expected_token(DOT);
		$got_dot++;
		if ($self->_TriplesBlock_test) {
			$got_dot	= 0;
			goto TRIPLESBLOCKLOOP;
		}
	}
}

# [22] GraphPatternNotTriples ::= OptionalGraphPattern | GroupOrUnionGraphPattern | GraphGraphPattern
sub _GraphPatternNotTriples_test {
	my $self	= shift;
	return 1 if ($self->_test_token(LBRACE));
	my $t	= $self->_peek_token;
	return unless ($t);
	return 0 unless ($t->type == KEYWORD);
	return ($t->value =~ qr/^(VALUES|BIND|SERVICE|MINUS|OPTIONAL|GRAPH)$/i);
}

sub _GraphPatternNotTriples {
	my $self	= shift;
	if ($self->_test_token(KEYWORD, 'VALUES')) {
		$self->_InlineDataClause;
	} elsif ($self->_test_token(KEYWORD, 'SERVICE')) {
		$self->_ServiceGraphPattern;
	} elsif ($self->_test_token(KEYWORD, 'MINUS')) {
		$self->_MinusGraphPattern;
	} elsif ($self->_test_token(KEYWORD, 'BIND')) {
		$self->_Bind;
	} elsif ($self->_test_token(KEYWORD, 'OPTIONAL')) {
		$self->_OptionalGraphPattern;
	} elsif ($self->_test_token(LBRACE)) {
		$self->_GroupOrUnionGraphPattern;
	} else {
		$self->_GraphGraphPattern;
	}
}

sub _InlineDataClause {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'VALUES');
	my @vars;
	
	my $parens	= 0;
	if ($self->_optional_token(LPAREN)) {
		$parens	= 1;
	}
	while ($self->_test_token(VAR)) {
		$self->_Var;
		push( @vars, splice(@{ $self->{_stack} }));
	}
	if ($parens) {
		$self->_expected_token(RPAREN);
	}
	
	my $count	= scalar(@vars);
	if (not($parens) and $count == 0) {
		croak "Syntax error: Expected VAR in inline data declaration";
	} elsif (not($parens) and $count > 1) {
		croak "Syntax error: Inline data declaration can only have one variable when parens are omitted";
	}
	
	my $short	= (not($parens) and $count == 1);
	$self->_expected_token(LBRACE);
	my @rows;
	if (not($short) or ($short and $self->_test_token(LPAREN))) {
		# { (term) (term) }
		while ($self->_test_token(LPAREN)) {
			my $terms	= $self->_Binding($count);
			push( @rows, $terms );
		}
	} else {
		# { term term }
		while ($self->_BindingValue_test) {
			$self->_BindingValue;
			my ($term)	= splice(@{ $self->{_stack} });
			push( @rows, [$term] );
		}
	}
	
	$self->_expected_token(RBRACE);
	
	my @vbs		= map { my %d; @d{ map { $_->value } @vars } = @$_; Attean::Result->new(bindings => \%d) } @rows;
	my $table	= Attean::Algebra::Table->new( variables => \@vars, rows => \@vbs );
	$self->_add_stack( ['Attean::Algebra::Table', $table] );
	
}

sub _Bind {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'BIND');
	my ($var, $expr)	= $self->_BrackettedAliasExpression;
	$self->_add_stack( ['Attean::Algebra::Extend', $var, $expr] );
}

sub _ServiceGraphPattern {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'SERVICE');
	my $silent	= $self->_optional_token(KEYWORD, 'SILENT') ? 1 : 0;
	$self->__close_bgp_with_filters;
	if ($self->_test_token(VAR)) {
		$self->_Var;
	} else {
		$self->_IRIref;
	}
	my ($endpoint)	= splice( @{ $self->{_stack} } );
	$self->_GroupGraphPattern;
	my $ggp	= $self->_remove_pattern;
	
	my $opt		= ['Attean::Algebra::Service', $endpoint, $ggp, ($silent ? 1 : 0)];
	$self->_add_stack( $opt );
}

# [23] OptionalGraphPattern ::= 'OPTIONAL' GroupGraphPattern
# sub _OptionalGraphPattern_test {
# 	my $self	= shift;
# 	return $self->_test_token(KEYWORD, 'OPTIONAL');
# }

sub __close_bgp_with_filters {
	my $self	= shift;
	my @filters		= splice(@{ $self->{filters} });
	if (@filters) {
		my $cont	= $self->_pop_pattern_container;
		my $ggp		= $self->_new_join(@$cont);
		$self->_push_pattern_container;
		# my $ggp	= $self->_remove_pattern();
		unless ($ggp) {
			$ggp	= Attean::Algebra::BGP->new();
		}
		while (my $f = shift @filters) {
			$ggp	= Attean::Algebra::Filter->new( children => [$ggp], expression => $f );
		}
		$self->_add_patterns($ggp);
	}
}

sub _OptionalGraphPattern {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'OPTIONAL');
	$self->__close_bgp_with_filters;
	
	$self->_GroupGraphPattern;
	my $ggp	= $self->_remove_pattern;
	my $opt		= ['Attean::Algebra::LeftJoin', $ggp];
	$self->_add_stack( $opt );
}

sub _MinusGraphPattern {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'MINUS');
	$self->__close_bgp_with_filters;
	
	$self->_GroupGraphPattern;
	my $ggp	= $self->_remove_pattern;
	my $opt		= ['Attean::Algebra::Minus', $ggp];
	$self->_add_stack( $opt );
}

# [24] GraphGraphPattern ::= 'GRAPH' VarOrIRIref GroupGraphPattern
sub _GraphGraphPattern {
	my $self	= shift;
	if ($self->{__data_pattern}) {
		if ($self->{__graph_nesting_level}++) {
			croak "Syntax error: Nested named GRAPH blocks not allowed in data template.";
		}
	}
	
	$self->_expected_token(KEYWORD, 'GRAPH');
	$self->_VarOrIRIref;
	my ($graph)	= splice(@{ $self->{_stack} });
	if ($graph->does('Attean::API::IRI')) {
		$self->_GroupGraphPattern;
	} else {
		$self->_GroupGraphPattern;
	}

	if ($self->{__data_pattern}) {
		$self->{__graph_nesting_level}--;
	}
	
	my $ggp	= $self->_remove_pattern;
	my $pattern	= Attean::Algebra::Graph->new( children => [$ggp], graph => $graph );
	$self->_add_patterns( $pattern );
	$self->_add_stack( [ 'Attean::Algebra::Graph' ] );
}

# [25] GroupOrUnionGraphPattern ::= GroupGraphPattern ( 'UNION' GroupGraphPattern )*
# sub _GroupOrUnionGraphPattern_test {
# 	my $self	= shift;
# 	return $self->_test_token(LBRACE);
# }

sub _GroupOrUnionGraphPattern {
	my $self	= shift;
	$self->_GroupGraphPattern;
	my $ggp	= $self->_remove_pattern;
	if ($self->_test_token(KEYWORD, 'UNION')) {
		while ($self->_optional_token(KEYWORD, 'UNION')) {
			$self->_GroupGraphPattern;
			my $rhs	= $self->_remove_pattern;
			$ggp	= Attean::Algebra::Union->new( children => [$ggp, $rhs] );
		}
		$self->_add_patterns( $ggp );
		$self->_add_stack( [ 'Attean::Algebra::Union' ] );
	} else {
		$self->_add_patterns( $ggp );
		$self->_add_stack( [ 'Attean::Algebra::Join' ] );
	}
}

# [26] Filter ::= 'FILTER' Constraint
sub _Filter {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'FILTER');
	$self->_Constraint;
	my ($expr) = splice(@{ $self->{_stack} });
	$self->_add_filter( $expr );
}

# [27] Constraint ::= BrackettedExpression | BuiltInCall | FunctionCall
sub _Constraint_test {
	my $self	= shift;
	return 1 if ($self->_test_token(LPAREN));
	return 1 if $self->_BuiltInCall_test;
	return 1 if $self->_IRIref_test;
	return 0;
}

sub _Constraint {
	my $self	= shift;
	if ($self->_test_token(LPAREN)) {
		$self->_BrackettedExpression();
	} elsif ($self->_BuiltInCall_test) {
		$self->_BuiltInCall();
	} else {
		$self->_FunctionCall();
	}
}

# [28] FunctionCall ::= IRIref ArgList
# sub _FunctionCall_test {
# 	my $self	= shift;
# 	return $self->_IRIref_test;
# }

sub _FunctionCall {
	my $self	= shift;
	$self->_IRIref;
	my ($iri)	= splice(@{ $self->{_stack} });
	
	my @args	= $self->_ArgList;

	if ($iri->value =~ m<^http://www[.]w3[.]org/2001/XMLSchema#(?:integer|decimal|float|double)$>) {
		my $expr	= Attean::CastExpression->new( children => \@args, datatype => $iri );
		$self->_add_stack( $expr );
	} else {
		my $func	= Attean::ValueExpression->new( value => $iri );
		my $expr	= $self->new_function_expression( 'INVOKE', $func, @args );
		$self->_add_stack( $expr );
	}
}

# [29] ArgList ::= ( NIL | '(' Expression ( ',' Expression )* ')' )
sub _ArgList_test {
	my $self	= shift;
	return 1 if $self->_test_token(NIL);
	return $self->_test_token(LPAREN);
}

sub _ArgList {
	my $self	= shift;
	if ($self->_optional_token(NIL)) {
		return;
	} else {
		$self->_expected_token(LPAREN);
		my @args;
		unless ($self->_test_token(RPAREN)) {
			$self->_Expression;
			push( @args, splice(@{ $self->{_stack} }) );
			while ($self->_optional_token(COMMA)) {
				$self->_Expression;
				push( @args, splice(@{ $self->{_stack} }) );
			}
		}
		$self->_expected_token(RPAREN);
		return @args;
	}
}

# [30] ConstructTemplate ::= '{' ConstructTriples? '}'
sub _ConstructTemplate {
	my $self	= shift;
	$self->_push_pattern_container;
	$self->_expected_token(LBRACE);
	
	if ($self->_ConstructTriples_test) {
		$self->_ConstructTriples;
	}

	$self->_expected_token(RBRACE);
	my $cont	= $self->_pop_pattern_container;
	$self->{build}{construct_triples}	= $cont;
}

# [31] ConstructTriples ::= TriplesSameSubject ( '.' ConstructTriples? )?
sub _ConstructTriples_test {
	my $self	= shift;
	return $self->_TriplesBlock_test;
}

sub _ConstructTriples {
	my $self	= shift;
	$self->_TriplesSameSubject;
	while ($self->_optional_token(DOT)) {
		if ($self->_ConstructTriples_test) {
			$self->_TriplesSameSubject;
		}
	}
}

# [32] TriplesSameSubject ::= VarOrTerm PropertyListNotEmpty | TriplesNode PropertyList
sub _TriplesSameSubject {
	my $self	= shift;
	my @triples;
	if ($self->_TriplesNode_test) {
		$self->_TriplesNode;
		my ($s)	= splice(@{ $self->{_stack} });
		$self->_PropertyList;
		my @list	= splice(@{ $self->{_stack} });
		foreach my $data (@list) {
			push(@triples, $self->__new_statement( $s, @$data ));
		}
	} else {
		$self->_VarOrTerm;
		my ($s)	= splice(@{ $self->{_stack} });

		$self->_PropertyListNotEmpty;
		my (@list)	= splice(@{ $self->{_stack} });
		foreach my $data (@list) {
			push(@triples, $self->__new_statement( $s, @$data ));
		}
	}
	
	$self->_add_patterns( @triples );
#	return @triples;
}

# TriplesSameSubjectPath ::= VarOrTerm PropertyListNotEmptyPath | TriplesNode PropertyListPath
sub _TriplesSameSubjectPath {
	my $self	= shift;
	my @triples;
	if ($self->_TriplesNode_test) {
		$self->_TriplesNode;
		my ($s)	= splice(@{ $self->{_stack} });
		$self->_PropertyListPath;
		my @list	= splice(@{ $self->{_stack} });
		foreach my $data (@list) {
			push(@triples, $self->__new_statement( $s, @$data ));
		}
	} else {
		$self->_VarOrTerm;
		my ($s)	= splice(@{ $self->{_stack} });
		$self->_PropertyListNotEmptyPath;
		my (@list)	= splice(@{ $self->{_stack} });
		foreach my $data (@list) {
			push(@triples, $self->__new_statement( $s, @$data ));
		}
	}
	$self->_add_patterns( @triples );
#	return @triples;
}

# [33] PropertyListNotEmpty ::= Verb ObjectList ( ';' ( Verb ObjectList )? )*
sub _PropertyListNotEmpty {
	my $self	= shift;
	$self->_Verb;
	my ($v)	= splice(@{ $self->{_stack} });
	$self->_ObjectList;
	my @l	= splice(@{ $self->{_stack} });
	my @props		= map { [$v, $_] } @l;
	while ($self->_optional_token(SEMICOLON)) {
		if ($self->_Verb_test) {
			$self->_Verb;
			my ($v)	= splice(@{ $self->{_stack} });
			$self->_ObjectList;
			my @l	= splice(@{ $self->{_stack} });
			push(@props, map { [$v, $_] } @l);
		}
	}
	$self->_add_stack( @props );
}

# [34] PropertyList ::= PropertyListNotEmpty?
sub _PropertyList {
	my $self	= shift;
	if ($self->_Verb_test) {
		$self->_PropertyListNotEmpty;
	}
}

# [33] PropertyListNotEmptyPath ::= (VerbPath | VerbSimple) ObjectList ( ';' ( (VerbPath | VerbSimple) ObjectList )? )*
sub _PropertyListNotEmptyPath {
	my $self	= shift;
	if ($self->_VerbPath_test) {
		$self->_VerbPath;
	} else {
		$self->_VerbSimple;
	}
	my ($v)	= splice(@{ $self->{_stack} });
	$self->_ObjectList;
	my @l	= splice(@{ $self->{_stack} });
	my @props		= map { [$v, $_] } @l;
	while ($self->_optional_token(SEMICOLON)) {
		if ($self->_VerbPath_test or $self->_test_token(VAR)) {
			if ($self->_VerbPath_test) {
				$self->_VerbPath;
			} else {
				$self->_VerbSimple;
			}
			my ($v)	= splice(@{ $self->{_stack} });
			$self->_ObjectList;
			my @l	= splice(@{ $self->{_stack} });
			push(@props, map { [$v, $_] } @l);
		}
	}
	$self->_add_stack( @props );
}

# [34] PropertyListPath ::= PropertyListNotEmptyPath?
sub _PropertyListPath {
	my $self	= shift;
	if ($self->_Verb_test) {
		$self->_PropertyListNotEmptyPath;
	}
}

# [35] ObjectList ::= Object ( ',' Object )*
sub _ObjectList {
	my $self	= shift;
	
	my @list;
	$self->_Object;
	push(@list, splice(@{ $self->{_stack} }));
	
	while ($self->_optional_token(COMMA)) {
		$self->_Object;
		push(@list, splice(@{ $self->{_stack} }));
	}
	$self->_add_stack( @list );
}

# [36] Object ::= GraphNode
sub _Object {
	my $self	= shift;
	$self->_GraphNode;
}

# [37] Verb ::= VarOrIRIref | 'a'
sub _Verb_test {
	my $self	= shift;
	return 1 if ($self->_test_token(A));
	return 1 if ($self->_test_token(VAR));
	return 1 if ($self->_IRIref_test);
	return 0;
}

sub _Verb {
	my $self	= shift;
	if ($self->_optional_token(A)) {
		my $type	= Attean::IRI->new(value =>  'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', lazy => 1);
		$self->_add_stack( $type );
	} else {
		$self->_VarOrIRIref;
	}
}

# VerbSimple ::= Var
# sub _VerbSimple_test {
# 	my $self	= shift;
# 	return ($self->_test_token(VAR));
# }

sub _VerbSimple {
	my $self	= shift;
	$self->_Var;
}

# VerbPath ::= Path
sub _VerbPath_test {
	my $self	= shift;
	return 1 if ($self->_IRIref_test);
	return 1 if ($self->_test_token(HAT));
	return 1 if ($self->_test_token(OR));
	return 1 if ($self->_test_token(BANG));
	return 1 if ($self->_test_token(LPAREN));
	return 1 if ($self->_test_token(A));
	return 0;
}

sub _VerbPath {
	my $self	= shift;
	$self->_Path
}

# [74]  	Path	  ::=  	PathAlternative
sub _Path {
	my $self	= shift;
	$self->_PathAlternative;
}

################################################################################

# [75]  	PathAlternative	  ::=  	PathSequence ( '|' PathSequence )*
sub _PathAlternative {
	my $self	= shift;
	$self->_PathSequence;
	while ($self->_optional_token(OR)) {
		my ($lhs)	= splice(@{ $self->{_stack} });
#		$self->_PathOneInPropertyClass;
		$self->_PathSequence;
		my ($rhs)	= splice(@{ $self->{_stack} });
		$self->_add_stack( ['PATH', '|', $lhs, $rhs] );
	}
}

# [76]  	PathSequence	  ::=  	PathEltOrInverse ( '/' PathEltOrInverse | '^' PathElt )*
sub _PathSequence {
	my $self	= shift;
	$self->_PathEltOrInverse;
	while ($self->_test_token(SLASH) or $self->_test_token(HAT)) {
		my $op;
		my ($lhs)	= splice(@{ $self->{_stack} });
		if ($self->_optional_token(SLASH)) {
			$op	= '/';
			$self->_PathEltOrInverse;
		} else {
			$op	= '^';
			$self->_expected_token(HAT);
			$self->_PathElt;
		}
		my ($rhs)	= splice(@{ $self->{_stack} });
		$self->_add_stack( ['PATH', $op, $lhs, $rhs] );
	}
}

# [77]  	PathElt	  ::=  	PathPrimary PathMod?
sub _PathElt {
	my $self	= shift;
	$self->_PathPrimary;
#	$self->__consume_ws_opt;
	if ($self->_PathMod_test) {
		my @path	= splice(@{ $self->{_stack} });
		$self->_PathMod;
		my ($mod)	= splice(@{ $self->{_stack} });
		if (defined($mod)) {
			$self->_add_stack( ['PATH', $mod, @path] );
		} else {
			# this might happen if we descend into _PathMod by mistaking a + as
			# a path modifier, but _PathMod figures out it's actually part of a
			# signed numeric object that follows the path
			$self->_add_stack( @path );
		}
	}
}

# [78]  	PathEltOrInverse	  ::=  	PathElt | '^' PathElt
sub _PathEltOrInverse {
	my $self	= shift;
	if ($self->_optional_token(HAT)) {
		$self->_PathElt;
		my @props	= splice(@{ $self->{_stack} });
		$self->_add_stack( [ 'PATH', '^', @props ] );
	} else {
		$self->_PathElt;
	}
}

# [79]  	PathMod	  ::=  	( '*' | '?' | '+' | '{' ( Integer ( ',' ( '}' | Integer '}' ) | '}' ) ) )
sub _PathMod_test {
	my $self	= shift;
	return 1 if ($self->_test_token(STAR));
	return 1 if ($self->_test_token(QUESTION));
	return 1 if ($self->_test_token(PLUS));
	return 1 if ($self->_test_token(LBRACE));
	return 0;
}

sub _PathMod {
	my $self	= shift;
	if ($self->_test_token(STAR) or $self->_test_token(QUESTION) or $self->_test_token(PLUS)) {
		my $t	= $self->_next_token;
		my $op;
		if ($t->type == STAR) {
			$op	= '*';
		} elsif ($t->type == QUESTION) {
			$op	= '?';
		} else {
			$op	= '+';
		}
		$self->_add_stack($op);
### path repetition range syntax :path{n,m}; removed from 1.1 Query 2LC
# 	} else {
# 		$self->_eat(qr/{/);
# 		$self->__consume_ws_opt;
# 		my $value	= 0;
# 		if ($self->_test(qr/}/)) {
# 			throw ::Error::ParseError -text => "Syntax error: Empty Path Modifier";
# 		}
# 		if ($self->_test($r_INTEGER)) {
# 			$value	= $self->_eat( $r_INTEGER );
# 			$self->__consume_ws_opt;
# 		}
# 		if ($self->_test(qr/,/)) {
# 			$self->_eat(qr/,/);
# 			$self->__consume_ws_opt;
# 			if ($self->_test(qr/}/)) {
# 				$self->_eat(qr/}/);
# 				$self->_add_stack( "$value-" );
# 			} else {
# 				my $end	= $self->_eat( $r_INTEGER );
# 				$self->__consume_ws_opt;
# 				$self->_eat(qr/}/);
# 				$self->_add_stack( "$value-$end" );
# 			}
# 		} else {
# 			$self->_eat(qr/}/);
# 			$self->_add_stack( "$value" );
# 		}
	}
}

# [80]  	PathPrimary	  ::=  	( IRIref | 'a' | '!' PathNegatedPropertyClass | '(' Path ')' )
sub _PathPrimary {
	my $self	= shift;
	if ($self->_IRIref_test) {
		$self->_IRIref;
	} elsif ($self->_optional_token(A)) {
		my $type	= Attean::IRI->new(value =>  'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', lazy => 1);
		$self->_add_stack( $type );
	} elsif ($self->_optional_token(BANG)) {
		$self->_PathNegatedPropertyClass;
		my (@path)	= splice(@{ $self->{_stack} });
		$self->_add_stack( ['PATH', '!', @path] );
	} else {
		$self->_expected_token(LPAREN);
		$self->_Path;
		$self->_expected_token(RPAREN);
	}
}

# [81]  	PathNegatedPropertyClass	  ::=  	( PathOneInPropertyClass | '(' ( PathOneInPropertyClass ( '|' PathOneInPropertyClass )* )? ')' )
sub _PathNegatedPropertyClass {
	my $self	= shift;
	if ($self->_optional_token(LPAREN)) {
		
		my @nodes;
		if ($self->_PathOneInPropertyClass_test) {
			$self->_PathOneInPropertyClass;
			push(@nodes, splice(@{ $self->{_stack} }));
			while ($self->_optional_token(OR)) {
				$self->_PathOneInPropertyClass;
				push(@nodes, splice(@{ $self->{_stack} }));
#				$self->_add_stack( ['PATH', '|', $lhs, $rhs] );
			}
		}
		$self->_expected_token(RPAREN);
		$self->_add_stack( @nodes );
	} else {
		$self->_PathOneInPropertyClass;
	}
}

# [82]  	PathOneInPropertyClass	  ::=  	IRIref | 'a'
sub _PathOneInPropertyClass_test {
	my $self	= shift;
	return 1 if $self->_IRIref_test;
	return 1 if ($self->_test_token(A));
	return 1 if ($self->_test_token(HAT));
	return 0;
}

sub _PathOneInPropertyClass {
	my $self	= shift;
	my $rev		= 0;
	if ($self->_optional_token(HAT)) {
		$rev	= 1;
	}
	if ($self->_optional_token(A)) {
		my $type	= Attean::IRI->new(value =>  'http://www.w3.org/1999/02/22-rdf-syntax-ns#type', lazy => 1);
		if ($rev) {
			$self->_add_stack( [ 'PATH', '^', $type ] );
		} else {
			$self->_add_stack( $type );
		}
	} else {
		$self->_IRIref;
		if ($rev) {
			my ($path)	= splice(@{ $self->{_stack} });
			$self->_add_stack( [ 'PATH', '^', $path ] );
		}
	}
}

################################################################################

# [38] TriplesNode ::= Collection | BlankNodePropertyList
sub _TriplesNode_test {
	my $self	= shift;
	return 1 if $self->_test_token(LPAREN);
	return 1 if $self->_test_token(LBRACKET);
	return 0;
}

sub _TriplesNode {
	my $self	= shift;
	if ($self->_test_token(LPAREN)) {
		$self->_Collection;
	} else {
		$self->_BlankNodePropertyList;
	}
}

# [39] BlankNodePropertyList ::= '[' PropertyListNotEmpty ']'
sub _BlankNodePropertyList {
	my $self	= shift;
	if (my $where = $self->{__no_bnodes}) {
		croak "Syntax error: Blank nodes not allowed in $where";
	}
	$self->_expected_token(LBRACKET);
#	$self->_PropertyListNotEmpty;
	$self->_PropertyListNotEmptyPath;
	$self->_expected_token(RBRACKET);
	
	my @props	= splice(@{ $self->{_stack} });
	my $subj	= Attean::Blank->new();
	my @triples	= map { $self->__new_statement( $subj, @$_ ) } @props;
	$self->_add_patterns( @triples );
	$self->_add_stack( $subj );
}

# [40] Collection ::= '(' GraphNode+ ')'
sub _Collection {
	my $self	= shift;
	$self->_expected_token(LPAREN);
	$self->_GraphNode;
	my @nodes;
	push(@nodes, splice(@{ $self->{_stack} }));
	
	while ($self->_GraphNode_test) {
		$self->_GraphNode;
		push(@nodes, splice(@{ $self->{_stack} }));
	}
	
	$self->_expected_token(RPAREN);
	
	my $subj	= Attean::Blank->new();
	my $cur		= $subj;
	my $last;

	my $first	= Attean::IRI->new(value => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#first', lazy => 1);
	my $rest	= Attean::IRI->new(value => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#rest', lazy => 1);
	my $nil		= Attean::IRI->new(value => 'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil', lazy => 1);

	
	my @triples;
	foreach my $node (@nodes) {
		push(@triples, $self->__new_statement( $cur, $first, $node ) );
		my $new	= Attean::Blank->new();
		push(@triples, $self->__new_statement( $cur, $rest, $new ) );
		$last	= $cur;
		$cur	= $new;
	}
	pop(@triples);
	push(@triples, $self->__new_statement( $last, $rest, $nil ));
	$self->_add_patterns( @triples );
	
	$self->_add_stack( $subj );
}

# [41] GraphNode ::= VarOrTerm | TriplesNode
sub _GraphNode_test {
	my $self	= shift;
	# VarOrTerm | TriplesNode -> (Var | GraphTerm) | (Collection | BlankNodePropertyList) -> Var | IRIref | RDFLiteral | NumericLiteral | BooleanLiteral | BlankNode | NIL | Collection | BlankNodePropertyList
	# but since a triple can't start with a literal, this is reduced to:
	# Var | IRIref | BlankNode | NIL
	return 1 if ($self->_test_token(VAR));
	return 1 if ($self->_IRIref_test);
	return 1 if ($self->_test_token(BNODE));
	return 1 if ($self->_test_token(LBRACKET));
	return 1 if ($self->_test_token(LPAREN));
	return 1 if ($self->_test_token(ANON));
	return 1 if ($self->_test_token(NIL));
	return 0;
}

sub _GraphNode {
	my $self	= shift;
	if ($self->_TriplesNode_test) {
		$self->_TriplesNode;
	} else {
		$self->_VarOrTerm;
	}
}

# [42] VarOrTerm ::= Var | GraphTerm
# sub _VarOrTerm_test {
# 	my $self	= shift;
# 	return 1 if ($self->_peek_token(VAR));
# 	return 1 if ($self->_IRIref_test);
# 	return 1 if ($self->_peek_token(BOOLEAN));
# 	return 1 if ($self->_test_literal_token);
# 	return 1 if ($self->_peek_token(BNODE));
# 	return 1 if ($self->_peek_token(NIL));
# 	return 0;
# }

sub _VarOrTerm {
	my $self	= shift;
	if ($self->_test_token(VAR)) {
		$self->_Var;
	} else {
		$self->_GraphTerm;
	}
}

# [43] VarOrIRIref ::= Var | IRIref
sub _VarOrIRIref_test {
	my $self	= shift;
	return 1 if ($self->_IRIref_test);
	return 1 if ($self->_test_token(VAR));
	return 0;
}

sub _VarOrIRIref {
	my $self	= shift;
	if ($self->_test_token(VAR)) {
		$self->_Var;
	} else {
		$self->_IRIref;
	}
}

# [44] Var ::= VAR1 | VAR2
sub _Var {
	my $self	= shift;
	if ($self->{__data_pattern}) {
		croak "Syntax error: Variable found where Term expected";
	}

	my $var		= $self->_expected_token(VAR);
	$self->_add_stack( Attean::Variable->new( $var->value ) );
}

# [45] GraphTerm ::= IRIref | RDFLiteral | NumericLiteral | BooleanLiteral | BlankNode | NIL
sub _GraphTerm {
	my $self	= shift;
	if ($self->_test_token(BOOLEAN)) {
		my $b	= $self->_BooleanLiteral;
		$self->_add_stack( $b );
	} elsif ($self->_test_token(NIL)) {
		my $n	= $self->_NIL;
		$self->_add_stack( $n );
	} elsif ($self->_test_token(ANON) or $self->_test_token(BNODE)) {
		my $b	= $self->_BlankNode;
		$self->_add_stack( $b );
	} elsif ($self->_test_token(INTEGER) or $self->_test_token(DECIMAL) or $self->_test_token(DOUBLE) or $self->_test_token(MINUS) or $self->_test_token(PLUS)) {
		my $l	= $self->_NumericLiteral;
		$self->_add_stack( $l );
	} elsif ($self->_test_literal_token) {
		my $l	= $self->_RDFLiteral;
		$self->_add_stack( $l );
	} else {
		$self->_IRIref;
	}
}

# [46] Expression ::= ConditionalOrExpression
sub _Expression {
	my $self	= shift;
	$self->_ConditionalOrExpression;
}

# [47] ConditionalOrExpression ::= ConditionalAndExpression ( '||' ConditionalAndExpression )*
sub _ConditionalOrExpression {
	my $self	= shift;
	my @list;
	
	$self->_ConditionalAndExpression;
	push(@list, splice(@{ $self->{_stack} }));
	
	while ($self->_test_token(OROR)) {
		$self->_expected_token(OROR);
		$self->_ConditionalAndExpression;
		push(@list, splice(@{ $self->{_stack} }));
	}
	
	if (scalar(@list) > 1) {
		my $algebra	= Attean::BinaryExpression->new( operator => '||', children => [splice(@list, 0, 2)] );
		while (scalar(@list)) {
			$algebra	= Attean::BinaryExpression->new( operator => '||', children => [$algebra, shift(@list)] );
		}
		$self->_add_stack($algebra);
	} else {
		$self->_add_stack( @list );
	}
	if (scalar(@{ $self->{_stack} }) == 0) {
		my $t	= $self->_peek_token;
		$self->_token_error($t, "Missing conditional expression");
	}
}

# [48] ConditionalAndExpression ::= ValueLogical ( '&&' ValueLogical )*
sub _ConditionalAndExpression {
	my $self	= shift;
	$self->_ValueLogical;
	my @list	= splice(@{ $self->{_stack} });
	
	while ($self->_test_token(ANDAND)) {
		$self->_expected_token(ANDAND);
		$self->_ValueLogical;
		push(@list, splice(@{ $self->{_stack} }));
	}
	
	if (scalar(@list) > 1) {
		my $algebra	= Attean::BinaryExpression->new( operator => '&&', children => [splice(@list, 0, 2)] );
		while (scalar(@list)) {
			$algebra	= Attean::BinaryExpression->new( operator => '&&', children => [$algebra, shift(@list)] );
		}
		$self->_add_stack($algebra);
	} else {
		$self->_add_stack( @list );
	}
}

# [49] ValueLogical ::= RelationalExpression
sub _ValueLogical {
	my $self	= shift;
	$self->_RelationalExpression;
}

# [50] RelationalExpression ::= NumericExpression ( '=' NumericExpression | '!=' NumericExpression | '<' NumericExpression | '>' NumericExpression | '<=' NumericExpression | '>=' NumericExpression )?
sub _RelationalExpression {
	my $self	= shift;
	$self->_NumericExpression;
	
	my $t		= $self->_peek_token;
	my $type	= $t->type;
	if ($type == EQUALS or $type == NOTEQUALS or $type == LE or $type == GE or $type == LT or $type == GT) {
		$self->_next_token;
		my @list	= splice(@{ $self->{_stack} });
		my $op	= $t->value;
		$self->_NumericExpression;
		push(@list, splice(@{ $self->{_stack} }));
		$self->_add_stack( $self->new_binary_expression( $op, @list ) );
	} elsif ($self->_test_token(KEYWORD, qr/^(NOT|IN)/)) {
		my @list	= splice(@{ $self->{_stack} });
		my $not		= $self->_optional_token(KEYWORD, 'NOT');
		$self->_expected_token(KEYWORD, 'IN');
		my $op		= $not ? 'NOTIN' : 'IN';
		$self->_ExpressionList();
		push(@list, splice(@{ $self->{_stack} }));
		my $p	= $self->new_function_expression( $op, @list );
		$self->_add_stack($p);
	}
}

sub _ExpressionList {
	my $self	= shift;
	if ($self->_optional_token(NIL)) {
		return;
	} else {
		$self->_expected_token(LPAREN);
		my @args;
		unless ($self->_test_token(RPAREN)) {
			$self->_Expression;
			push( @args, splice(@{ $self->{_stack} }) );
			while ($self->_optional_token(COMMA)) {
				$self->_Expression;
				push( @args, splice(@{ $self->{_stack} }) );
			}
		}
		$self->_expected_token(RPAREN);
		$self->_add_stack( @args );
	}
}

# [51] NumericExpression ::= AdditiveExpression
sub _NumericExpression {
	my $self	= shift;
	$self->_AdditiveExpression;
}

# [52] AdditiveExpression ::= MultiplicativeExpression ( '+' MultiplicativeExpression | '-' MultiplicativeExpression | NumericLiteralPositive | NumericLiteralNegative )*
sub _AdditiveExpression {
	my $self	= shift;
	$self->_MultiplicativeExpression;
	my ($expr)	= splice(@{ $self->{_stack} });
	
	while ($self->_test_token(MINUS) or $self->_test_token(PLUS)) {
		my $t	= $self->_next_token;
		my $op	= ($t->type == MINUS) ? '-' : '+';
		$self->_MultiplicativeExpression;
		my ($rhs)	= splice(@{ $self->{_stack} });
		$expr	= $self->new_binary_expression( $op, $expr, $rhs );
	}
	$self->_add_stack( $expr );
}

# [53] MultiplicativeExpression ::= UnaryExpression ( '*' UnaryExpression | '/' UnaryExpression )*
sub _MultiplicativeExpression {
	my $self	= shift;
	$self->_UnaryExpression;
	my ($expr)	= splice(@{ $self->{_stack} });
	
	while ($self->_test_token(STAR) or $self->_test_token(SLASH)) {
		my $t	= $self->_next_token;
		my $op	= ($t->type == STAR) ? '*' : '/';
		$self->_UnaryExpression;
		my ($rhs)	= splice(@{ $self->{_stack} });
		$expr	= $self->new_binary_expression( $op, $expr, $rhs );
	}
	$self->_add_stack( $expr );
}

# [54] UnaryExpression ::= '!' PrimaryExpression  | '+' PrimaryExpression  | '-' PrimaryExpression  | PrimaryExpression
sub _UnaryExpression {
	my $self	= shift;
	if ($self->_optional_token(BANG)) {
		$self->_PrimaryExpression;
		my ($expr)	= splice(@{ $self->{_stack} });
		my $not		= Attean::UnaryExpression->new( operator => '!', children => [$expr] );
		$self->_add_stack( $not );
	} elsif ($self->_optional_token(PLUS)) {
		$self->_PrimaryExpression;
		my ($expr)	= splice(@{ $self->{_stack} });
		
		### if it's just a literal, force the positive down into the literal
		if (blessed($expr) and $expr->isa('Attean::ValueExpression') and $expr->value->does('Attean::API::NumericLiteral')) {
			my $value	= '+' . $expr->value->value;
			my $l		= Attean::Literal->new( value => $value, datatype => $expr->value->datatype );
			my $lexpr	= Attean::ValueExpression->new( value => $l );
			$self->_add_stack( $lexpr );
		} else {
			my $lexpr	= Attean::ValueExpression->new( value => $expr );
			$self->_add_stack( $lexpr );
		}
	} elsif ($self->_optional_token(MINUS)) {
		$self->_PrimaryExpression;
		my ($expr)	= splice(@{ $self->{_stack} });
		
		### if it's just a literal, force the negative down into the literal instead of make an unnecessary multiplication.
		if (blessed($expr) and $expr->isa('Attean::ValueExpression') and $expr->value->does('Attean::API::NumericLiteral')) {
			my $value	= -1 * $expr->value->value;
			my $l		= Attean::Literal->new( value => $value, datatype => $expr->value->datatype );
			my $lexpr	= Attean::ValueExpression->new( value => $l );
			$self->_add_stack( $lexpr );
		} else {
			my $int		= 'http://www.w3.org/2001/XMLSchema#integer';
			my $l		= Attean::Literal->new( value => '-1', datatype => $int );
			my $neg		= $self->new_binary_expression( '*', Attean::ValueExpression->new( value => $l ), $expr );
			my $lexpr	= Attean::ValueExpression->new( value => $neg );
			$self->_add_stack( $lexpr );
		}
	} else {
		$self->_PrimaryExpression;
	}
}

# [55] PrimaryExpression ::= BrackettedExpression | BuiltInCall | IRIrefOrFunction | RDFLiteral | NumericLiteral | BooleanLiteral | Var
sub _PrimaryExpression {
	my $self	= shift;
	my $t	= $self->_peek_token;
	if ($self->_test_token(LPAREN)) {
		$self->_BrackettedExpression;
	} elsif ($self->_BuiltInCall_test) {
		$self->_BuiltInCall;
	} elsif ($self->_IRIref_test) {
		$self->_IRIrefOrFunction;
		my $v		= pop(@{ $self->{_stack} });
		if ($v->does('Attean::API::IRI')) {
			$v	= Attean::ValueExpression->new(value => $v);
		}
		$self->_add_stack($v);
	} elsif ($self->_test_token(VAR)) {
		$self->_Var;
		my $var		= pop(@{ $self->{_stack} });
		my $expr	= Attean::ValueExpression->new(value => $var);
		$self->_add_stack($expr);
	} elsif ($self->_test_token(BOOLEAN)) {
		my $b		= $self->_BooleanLiteral;
		my $expr	= Attean::ValueExpression->new(value => $b);
		$self->_add_stack($expr);
	} elsif ($self->_test_token(INTEGER) or $self->_test_token(DECIMAL) or $self->_test_token(DOUBLE) or $self->_test_token(PLUS) or $self->_test_token(MINUS)) {
		my $l		= $self->_NumericLiteral;
		my $expr	= Attean::ValueExpression->new(value => $l);
		$self->_add_stack($expr);
	} else {
		my $value	= $self->_RDFLiteral;
		my $expr	= Attean::ValueExpression->new(value => $value);
		$self->_add_stack($expr);
	}
}

# [56] BrackettedExpression ::= '(' Expression ')'
# sub _BrackettedExpression_test {
# 	my $self	= shift;
# 	return $self->_test_token(LPAREN);
# }

sub _BrackettedExpression {
	my $self	= shift;
	$self->_expected_token(LPAREN);
	$self->_Expression;
	$self->_expected_token(RPAREN);
}

sub _Aggregate {
	my $self	= shift;
	my $t		= $self->_expected_token(KEYWORD);
	my $op		= $t->value;
	$self->_expected_token(LPAREN);
	my $distinct	= 0;
	if ($self->_optional_token(KEYWORD, 'DISTINCT')) {
		$distinct	= 1;
	}
	
	my $star	= 0;
	my (@expr, %options);
	if ($self->_optional_token(STAR)) {
		$star	= 1;
	} else {
		$self->_Expression;
		push(@expr, splice(@{ $self->{_stack} }));
		if ($op eq 'GROUP_CONCAT') {
			while ($self->_optional_token(COMMA)) {
				$self->_Expression;
				push(@expr, splice(@{ $self->{_stack} }));
			}
			if ($self->_optional_token(SEMICOLON)) {
				$self->_expected_token(KEYWORD, 'SEPARATOR');
				$self->_expected_token(EQUALS);
				my $sep		= $self->_String;
				$options{ seperator }	= $sep;
			}
		}
	}
	my $arg	= join(',', map { blessed($_) ? $_->as_string : $_ } @expr);
	if ($distinct) {
		$arg	= 'DISTINCT ' . $arg;
	}
	my $name	= sprintf('%s(%s)', $op, $arg);
	$self->_expected_token(RPAREN);
	
	my $var		= Attean::Variable->new( value => ".$name");
	my $agg		= Attean::AggregateExpression->new(
					distinct	=> $distinct,
					operator	=> $op,
					children	=> [@expr],
					scalar_vars	=> \%options,
					variable	=> $var,
				);
	$self->{build}{__aggregate}{ $name }	= [ $var, $agg ];
	my $expr	= Attean::ValueExpression->new(value => $var);
	$self->_add_stack($expr);
}

# [57] BuiltInCall ::= 'STR' '(' Expression ')'  | 'LANG' '(' Expression ')'  | 'LANGMATCHES' '(' Expression ',' Expression ')'  | 'DATATYPE' '(' Expression ')'  | 'BOUND' '(' Var ')'  | 'sameTerm' '(' Expression ',' Expression ')'  | 'isIRI' '(' Expression ')'  | 'isURI' '(' Expression ')'  | 'isBLANK' '(' Expression ')'  | 'isLITERAL' '(' Expression ')'  | RegexExpression
sub _BuiltInCall_test {
	my $self	= shift;
	my $t		= $self->_peek_token;
	return unless ($t);
	if ($self->{__aggregate_call_ok}) {
		return 1 if ($self->_test_token(KEYWORD, qr/^(MIN|MAX|COUNT|AVG|SUM|SAMPLE|GROUP_CONCAT)$/io));
	}
	return 1 if ($self->_test_token(KEYWORD, 'NOT'));
	return 1 if ($self->_test_token(KEYWORD, 'EXISTS'));
	return 1 if ($self->_test_token(KEYWORD, qr/^(ABS|CEIL|FLOOR|ROUND|CONCAT|SUBSTR|STRLEN|UCASE|LCASE|ENCODE_FOR_URI|CONTAINS|STRSTARTS|STRENDS|RAND|MD5|SHA1|SHA224|SHA256|SHA384|SHA512|HOURS|MINUTES|SECONDS|DAY|MONTH|YEAR|TIMEZONE|TZ|NOW)$/i));
	return ($self->_test_token(KEYWORD, qr/^(COALESCE|UUID|STRUUID|STR|STRDT|STRLANG|STRBEFORE|STRAFTER|REPLACE|BNODE|IRI|URI|LANG|LANGMATCHES|DATATYPE|BOUND|sameTerm|isIRI|isURI|isBLANK|isLITERAL|REGEX|IF|isNumeric)$/i));
}

sub _BuiltInCall {
	my $self	= shift;
	my $t		= $self->_peek_token;
	if ($self->{__aggregate_call_ok} and $self->_test_token(KEYWORD, qr/^(MIN|MAX|COUNT|AVG|SUM|SAMPLE|GROUP_CONCAT)\b/io)) {
		$self->_Aggregate;
	} elsif ($self->_test_token(KEYWORD, qr/^(NOT|EXISTS)/)) {
		my $not	= $self->_optional_token(KEYWORD, 'NOT');
		$self->_expected_token(KEYWORD, 'EXISTS');
		local($self->{filters})					= [];
		$self->_GroupGraphPattern;
		my $cont	= $self->_remove_pattern;
		my $p		= Attean::ExistsExpression->new( pattern => $cont );
		if ($not) {
			$p	= Attean::UnaryExpression->new( operator => '!', children => [$p] );
		}
		$self->_add_stack($p);
	} elsif ($self->_test_token(KEYWORD, qr/^(COALESCE|BNODE|CONCAT|SUBSTR|RAND|NOW)/i)) {
		# n-arg functions that take expressions
		my $t		= $self->_next_token;
		my $op		= $t->value;
		my @args	= $self->_ArgList;
		my $func	= $self->new_function_expression( $op, @args );
		$self->_add_stack( $func );
	} elsif ($self->_test_token(KEYWORD, 'REGEX')) {
		$self->_RegexExpression;
	} else {
		my $t		= $self->_next_token;
		my $op		= $t->value;
		if ($op =~ /^(STR)?UUID$/i) {
			# no-arg functions
			$self->_expected_token(NIL);
			$self->_add_stack( $self->new_function_expression($op) );
		} elsif ($op =~ /^(STR|URI|IRI|LANG|DATATYPE|isIRI|isURI|isBLANK|isLITERAL|isNumeric|ABS|CEIL|FLOOR|ROUND|STRLEN|UCASE|LCASE|ENCODE_FOR_URI|MD5|SHA1|SHA224|SHA256|SHA384|SHA512|HOURS|MINUTES|SECONDS|DAY|MONTH|YEAR|TIMEZONE|TZ)$/i) {
			### one-arg functions that take an expression
			$self->_expected_token(LPAREN);
			$self->_Expression;
			my ($expr)	= splice(@{ $self->{_stack} });
			$self->_add_stack( $self->new_function_expression($op, $expr) );
			$self->_expected_token(RPAREN);
		} elsif ($op =~ /^(STRDT|STRLANG|LANGMATCHES|sameTerm|CONTAINS|STRSTARTS|STRENDS|STRBEFORE|STRAFTER)$/i) {
			### two-arg functions that take expressions
			$self->_expected_token(LPAREN);
			$self->_Expression;
			my ($arg1)	= splice(@{ $self->{_stack} });
			$self->_expected_token(COMMA);
			$self->_Expression;
			my ($arg2)	= splice(@{ $self->{_stack} });
			$self->_add_stack( $self->new_function_expression($op, $arg1, $arg2) );
			$self->_expected_token(RPAREN);
		} elsif ($op =~ /^(IF|REPLACE)$/i) {
			### three-arg functions that take expressions
			$self->_expected_token(LPAREN);
			$self->_Expression;
			my ($arg1)	= splice(@{ $self->{_stack} });
			$self->_expected_token(COMMA);
			$self->_Expression;
			my ($arg2)	= splice(@{ $self->{_stack} });
			$self->_expected_token(COMMA);
			$self->_Expression;
			my ($arg3)	= splice(@{ $self->{_stack} });
			$self->_add_stack( $self->new_function_expression($op, $arg1, $arg2, $arg3) );
			$self->_expected_token(RPAREN);
		} else {
			### BOUND(Var)
			$self->_expected_token(LPAREN);
			$self->_Var;
			my $var		= pop(@{ $self->{_stack} });
			my $expr	= Attean::ValueExpression->new(value => $var);
			$self->_add_stack( $self->new_function_expression($op, $expr) );
			$self->_expected_token(RPAREN);
		}
	}
}

# [58] RegexExpression ::= 'REGEX' '(' Expression ',' Expression ( ',' Expression )? ')'
# sub _RegexExpression_test {
# 	my $self	= shift;
# 	return $self->_test_token(KEYWORD, 'REGEX');
# }

sub _RegexExpression {
	my $self	= shift;
	$self->_expected_token(KEYWORD, 'REGEX');
	$self->_expected_token(LPAREN);
	$self->_Expression;
	my $string	= splice(@{ $self->{_stack} });
	
	$self->_expected_token(COMMA);
	$self->_Expression;
	my $pattern	= splice(@{ $self->{_stack} });
	
	my @args	= ($string, $pattern);
	if ($self->_optional_token(COMMA)) {
		$self->_Expression;
		push(@args, splice(@{ $self->{_stack} }));
	}
	
	$self->_expected_token(RPAREN);
	$self->_add_stack( $self->new_function_expression( 'REGEX', @args ) );
}

# [59] IRIrefOrFunction ::= IRIref ArgList?
# sub _IRIrefOrFunction_test {
# 	my $self	= shift;
# 	$self->_IRIref_test;
# }

sub _IRIrefOrFunction {
	my $self	= shift;
	$self->_IRIref;
	if ($self->_ArgList_test) {
		my ($iri)	= splice(@{ $self->{_stack} });
		my @args	= $self->_ArgList;
		if ($iri->value =~ m<^http://www[.]w3[.]org/2001/XMLSchema#(?:integer|decimal|float|double)$>) {
			my $expr	= Attean::CastExpression->new( children => \@args, datatype => $iri );
			$self->_add_stack( $expr );
		} else {
			my $func	= Attean::ValueExpression->new( value => $iri );
			my $expr	= $self->new_function_expression( 'INVOKE', $func, @args );
			$self->_add_stack( $expr );
		}
	}
}

# [60] RDFLiteral ::= String ( LANGTAG | ( '^^' IRIref ) )?
sub _RDFLiteral {
	my $self	= shift;
	my $value	= $self->_String;
	
	my $obj;
	if ($self->_test_token(LANG)) {
		my $t	= $self->_expected_token(LANG);
		my $lang	= $t->value;
		$obj	= Attean::Literal->new( value => $value, language => $lang );
	} elsif ($self->_test_token(HATHAT)) {
		$self->_expected_token(HATHAT);
		$self->_IRIref;
		my ($iri)	= splice(@{ $self->{_stack} });
		$obj	= Attean::Literal->new( value => $value, datatype => $iri );
	} else {
		$obj	= Attean::Literal->new( value => $value );
	}
	
	return $obj;
}

# [61] NumericLiteral ::= NumericLiteralUnsigned | NumericLiteralPositive | NumericLiteralNegative
# [62] NumericLiteralUnsigned ::= INTEGER | DECIMAL | DOUBLE
# [63] NumericLiteralPositive ::= INTEGER_POSITIVE | DECIMAL_POSITIVE | DOUBLE_POSITIVE
# [64] NumericLiteralNegative ::= INTEGER_NEGATIVE | DECIMAL_NEGATIVE | DOUBLE_NEGATIVE
sub _NumericLiteral {
	my $self	= shift;
	my $sign	= 0;
	if ($self->_optional_token(PLUS)) {
		$sign	= '+';
	} elsif ($self->_optional_token(MINUS)) {
		$sign	= '-';
	}
	
	my $value;
	my $type;
	if (my $db = $self->_optional_token(DOUBLE)) {
		$value	= $db->value;
		$type	= Attean::IRI->new(value =>  'http://www.w3.org/2001/XMLSchema#double', lazy => 1);
	} elsif (my $dc = $self->_optional_token(DECIMAL)) {
		$value	= $dc->value;
		$type	= Attean::IRI->new(value =>  'http://www.w3.org/2001/XMLSchema#decimal', lazy => 1);
	} else {
		my $i	= $self->_expected_token(INTEGER);
		$value	= $i->value;
		$type	= Attean::IRI->new(value =>  'http://www.w3.org/2001/XMLSchema#integer', lazy => 1);
	}
	
	if ($sign) {
		$value	= $sign . $value;
	}
	
	my $obj	= Attean::Literal->new( value => $value, datatype => $type );
# 	if ($self->{args}{canonicalize} and blessed($obj) and $obj->isa('RDF::Trine::Node::Literal')) {
# 		$obj	= $obj->canonicalize;
# 	}

	return $obj;
}

# [65] BooleanLiteral ::= 'true' | 'false'
sub _BooleanLiteral {
	my $self	= shift;
	my $t		= $self->_expected_token(BOOLEAN);
	my $bool	= $t->value;

	my $obj	= Attean::Literal->new( value => $bool, datatype => 'http://www.w3.org/2001/XMLSchema#boolean' );
# 	if ($self->{args}{canonicalize} and blessed($obj) and $obj->isa('RDF::Trine::Node::Literal')) {
# 		$obj	= $obj->canonicalize;
# 	}
	return $obj;
}

# [66] String ::= STRING_LITERAL1 | STRING_LITERAL2 | STRING_LITERAL_LONG1 | STRING_LITERAL_LONG2
sub _String {
	my $self	= shift;
	my $value;
	my $string;
	my $t	= $self->_peek_token;
	if ($string = $self->_optional_token(STRING1D)) {
		$value	= $string->value;
	} elsif ($string = $self->_optional_token(STRING1S)) {
		$value	= $string->value;
	} elsif ($string = $self->_optional_token(STRING3S)) {
		$value	= $string->value;
	} elsif ($string = $self->_optional_token(STRING3D)) {
		$value	= $string->value;
	} else {
		my $got	= AtteanX::SPARQL::Constants::decrypt_constant($t->type);
		my $value	= $t->value;
		croak "Expecting string literal but found $got '$value'";
	}

	$value	=~ s/\\t/\t/g;
	$value	=~ s/\\b/\n/g;
	$value	=~ s/\\n/\n/g;
	$value	=~ s/\\r/\x08/g;
	$value	=~ s/\\"/"/g;
	$value	=~ s/\\'/'/g;
	$value	=~ s/\\\\/\\/g;	# backslash must come last, so it doesn't accidentally create a new escape
	return $value;
}

# [67] IRIref ::= IRI_REF | PrefixedName
sub _IRIref_test {
	my $self	= shift;
	return 1 if ($self->_test_token(IRI));
	return 1 if ($self->_test_token(PREFIXNAME));
	return 0;
}


sub _IRIref {
	my $self	= shift;
	if (my $t = $self->_optional_token(IRI)) {
		my $iri	= $t->value;
		my $base	= $self->__base;
		my $node	= $self->new_iri( value => $iri, $base ? (base => $base) : () );
		$self->_add_stack( $node );
	} else {
		my $p	= $self->_PrefixedName;
		$self->_add_stack( $p );
	}
}

# [68] PrefixedName ::= PNAME_LN | PNAME_NS
sub _PrefixedName {
	my $self	= shift;
	my $t		= $self->_expected_token(PREFIXNAME);
	my ($ns, $local)	= @{ $t->args };
	chop($ns);
# 		$local	=~ s{\\([-~.!&'()*+,;=:/?#@%_\$])}{$1}g;
	
	unless ($self->namespaces->namespace_uri($ns)) {
		croak "Syntax error: Use of undefined namespace '$ns'";
	}
	
	my $iri		= $self->namespaces->namespace_uri($ns)->iri($local);
	my $base	= $self->__base;
	my $p		= $self->new_iri( value => $iri->value, $base ? (base => $base) : () );
	return $p;
}

# [69] BlankNode ::= BLANK_NODE_LABEL | ANON
sub _BlankNode {
	my $self	= shift;
	if (my $where = $self->{__no_bnodes}) {
		croak "Syntax error: Blank nodes not allowed in $where";
	}
	if (my $b = $self->_optional_token(BNODE)) {
		my $label	= $b->value;
		return Attean::Blank->new($label);
	} else {
		$self->_expected_token(ANON);
		return Attean::Blank->new();
	}
}

sub _NIL {
	my $self	= shift;
	$self->_expected_token(NIL);
	return Attean::IRI->new(value =>  'http://www.w3.org/1999/02/22-rdf-syntax-ns#nil', lazy => 1);
}

sub __solution_modifiers {
	my $self	= shift;
	my $star	= shift;
	my @exprs	= @_;
	
	if (my $computed_group_vars = delete( $self->{build}{__group_vars} )) {
		my $pattern	= $self->{build}{triples}[0];
		foreach my $data (@$computed_group_vars) {
			my ($var, $expr)	= @$data;
			$pattern	= Attean::Algebra::Extend->new( children => [$pattern], variable => $var, expression => $expr );
		}
		$self->{build}{triples}[0]	= $pattern;
	}
	
	my $has_aggregation	= 0;
	my $having_expr;
	my $aggdata	= delete( $self->{build}{__aggregate} );
	my $groupby	= delete( $self->{build}{__group_by} ) || [];
	my @aggkeys	= keys %{ $aggdata || {} };
	if (scalar(@aggkeys) or scalar(@$groupby)) {
		$has_aggregation++;
		my @aggs;
		foreach my $k (@aggkeys) {
			my ($var, $expr)	= @{ $aggdata->{$k} };
			push(@aggs, $expr);
		}
		
		my $pattern	= $self->{build}{triples};
		my $ggp		= shift(@$pattern);
		if (my $having = delete( $self->{build}{__having} )) {
			$having_expr	= $having;
		}
		my $agg		= Attean::Algebra::Group->new( children => [$ggp], groupby => $groupby, aggregates => \@aggs );
		push(@{ $self->{build}{triples} }, $agg);
	}
	
	my %group_vars;
	my %agg_vars;
	if ($has_aggregation) {
		foreach my $agg_var (map { $_->[0] } values %$aggdata) {
			$agg_vars{ $agg_var->value }++;
		}
		foreach my $g (@$groupby) {
			if ($g->isa('Attean::ValueExpression') and $g->value->does('Attean::API::Variable')) {
				$group_vars{ $g->value->value }++;
			} else {
				$self->log->trace("Remaining GROUP BY clauses:\n" . Dumper($g));
				croak 'Unrecognized GROUP BY clauses, see trace log for details.';
			}
		}
	}
	
	my @project;
	my @vars;
	my @extend;
	if ($star) {
		my $pattern	= ${ $self->{build}{triples} }[-1];
		push(@project, $pattern->in_scope_variables);
		if ($has_aggregation) {
			croak "Cannot SELECT * in an aggregate query";
		}
	} else {
		for (my $i = 0; $i < $#exprs; $i += 2) {
			my $k	= $exprs[$i];
			my $v	= $exprs[$i+1];
			if ($has_aggregation) {
				my @vars	= $v->does('Attean::API::Variable') ? $v : $v->unaggregated_variables;
				foreach my $var (@vars) {
					my $name	= $var->value;
					unless (exists $agg_vars{$name} or exists $group_vars{$name}) {
						croak "Cannot project variable ?$name that is not aggregated or used in grouping";
					}
				}
			}
			
			push(@project, $k);
			if ($v->does('Attean::API::Variable')) {
				push(@vars, $v);
			} else {
				push(@extend, $k, $v);
			}
		}
	}

	{
		my $pattern	= pop(@{ $self->{build}{triples} });
		my %in_scope	= map { $_ => 1 } $pattern->in_scope_variables;
		while (my($name, $expr) = splice(@extend, 0, 2)) {
			if (exists $in_scope{$name}) {
				croak "Syntax error: Already-bound variable ($name) used in project expression";
			}
			my $var		= Attean::Variable->new( value => $name );
			$pattern	= Attean::Algebra::Extend->new(children => [$pattern], variable => $var, expression => $expr);
		}
		push(@{ $self->{build}{triples} }, $pattern);
	}
	
	if ($having_expr) {
		my $pattern	= pop(@{ $self->{build}{triples} });
		my $filter	= Attean::Algebra::Filter->new( children => [$pattern], expression => $having_expr );
		push(@{ $self->{build}{triples} }, $filter);
	}
	
	if ($self->{build}{options}{orderby}) {
		my $order	= delete $self->{build}{options}{orderby};
		my $pattern	= pop(@{ $self->{build}{triples} });
		my @order	= @$order;
		my @cmps;
		foreach my $o (@order) {
			my ($dir, $expr)	= @$o;
			my $asc				= ($dir eq 'ASC');
			push(@cmps, Attean::Algebra::Comparator->new(ascending => $asc, expression => $expr));
		}
		my $sort	= Attean::Algebra::OrderBy->new( children => [$pattern], comparators => \@cmps );
		push(@{ $self->{build}{triples} }, $sort);
	}

	{
		my $pattern	= pop(@{ $self->{build}{triples} });
		my $vars	= [map { Attean::Variable->new(value => $_) } @project];
		if (scalar(@$vars)) {
			$pattern	= Attean::Algebra::Project->new( children => [$pattern], variables => $vars);
		}
		push(@{ $self->{build}{triples} }, $pattern);
	}
	
	if (my $level = $self->{build}{options}{distinct}) {
		delete $self->{build}{options}{distinct};
		my $pattern	= pop(@{ $self->{build}{triples} });
		my $sort	= ($level == 1)
					? Attean::Algebra::Distinct->new( children => [$pattern] )
					: Attean::Algebra::Reduced->new( children => [$pattern] );
		push(@{ $self->{build}{triples} }, $sort);
	}
	
	if (exists $self->{build}{options}{offset} and exists $self->{build}{options}{limit}) {
		my $limit	= delete $self->{build}{options}{limit};
		my $offset	= delete $self->{build}{options}{offset};
		my $pattern	= pop(@{ $self->{build}{triples} });
		my $sliced	= Attean::Algebra::Slice->new( children => [$pattern], limit => $limit, offset => $offset );
		push(@{ $self->{build}{triples} }, $sliced);
	} elsif (exists $self->{build}{options}{offset}) {
		my $offset		= delete $self->{build}{options}{offset};
		my $pattern		= pop(@{ $self->{build}{triples} });
		my $sliced	= Attean::Algebra::Slice->new( children => [$pattern], offset => $offset );
		push(@{ $self->{build}{triples} }, $sliced);
	} elsif (exists $self->{build}{options}{limit}) {
		my $limit	= delete $self->{build}{options}{limit};
		my $pattern	= pop(@{ $self->{build}{triples} });
		my $sliced	= Attean::Algebra::Slice->new( children => [$pattern], limit => $limit );
		push(@{ $self->{build}{triples} }, $sliced);
	}
	
	return @project;
}

################################################################################

=item C<< error >>

Returns the error encountered during the last parse.

=cut

sub _add_patterns {
	my $self	= shift;
	my @triples	= @_;
	my $container	= $self->{ _pattern_container_stack }[0];
	push( @{ $container }, @triples );
}

sub _remove_pattern {
	my $self	= shift;
	my $container	= $self->{ _pattern_container_stack }[0];
	my $pattern		= pop( @{ $container } );
	return $pattern;
}

sub _peek_pattern {
	my $self	= shift;
	my $container	= $self->{ _pattern_container_stack }[0];
	my $pattern		= $container->[-1];
	return $pattern;
}

sub _push_pattern_container {
	my $self	= shift;
	my $cont	= [];
	unshift( @{ $self->{ _pattern_container_stack } }, $cont );
	return $cont;
}

sub _pop_pattern_container {
	my $self	= shift;
	my $cont	= shift( @{ $self->{ _pattern_container_stack } } );
	return $cont;
}

sub _add_stack {
	my $self	= shift;
	my @items	= @_;
	push( @{ $self->{_stack} }, @items );
}

sub _add_filter {
	my $self	= shift;
	my @filters	= shift;
	push( @{ $self->{filters} }, @filters );
}

sub __base {
	my $self	= shift;
	my $build	= $self->{build};
	if (blessed($build->{base})) {
		return $build->{base};
	} elsif (defined($build->{base})) {
		return $self->new_iri($build->{base});
	} else {
		return;
	}
}

sub __new_statement {
	my $self	= shift;
	my @nodes	= @_;
	return Attean::TriplePattern->new(@nodes);
}

sub __new_path {
	my $self	= shift;
	my $start	= shift;
	my $pdata	= shift;
	my $end		= shift;
	(undef, my $op, my @nodes)	= @$pdata;
	my $path	= $self->__new_path_pred($op, @nodes);
	return Attean::Algebra::Path->new( subject => $start, path => $path, object => $end );
}

sub __new_path_pred {
	my $self	= shift;
	my $op		= shift;
	my @nodes	= @_;

	if ($op eq '!') {
		return Attean::Algebra::NegatedPropertySet->new( predicates => \@nodes );
	}
	
	foreach my $i (0 .. $#nodes) {
		if (ref($nodes[$i]) eq 'ARRAY') {
			(undef, my @data) = @{ $nodes[$i] };
			$nodes[$i]	= $self->__new_path_pred(@data);
		} elsif ($nodes[$i]->does('Attean::API::IRI')) {
			$nodes[$i]	= Attean::Algebra::PredicatePath->new( predicate => $nodes[$i] );
		}
	}
	
	if ($op eq '*') {
		return Attean::Algebra::ZeroOrMorePath->new( children => [@nodes] );
	} elsif ($op eq '+') {
		return Attean::Algebra::OneOrMorePath->new( children => [@nodes] );
	} elsif ($op eq '?') {
		return Attean::Algebra::ZeroOrOnePath->new( children => [@nodes] );
	} elsif ($op eq '^') {
		return Attean::Algebra::InversePath->new( children => [@nodes] );
	} elsif ($op eq '/') {
		return Attean::Algebra::SequencePath->new( children => [@nodes] );
	} elsif ($op eq '|') {
		return Attean::Algebra::AlternativePath->new( children => [@nodes] );
	} else {
		$self->log->debug("Path $op:\n". Dumper(\@nodes));
		confess "Error in path $op. See debug log for details." 
	}
}

sub __new_bgp {
	# fix up BGPs that might actually have property paths in them. split those
	# out as their own path algebra objects, and join them with the bgp with a
	# ggp if necessary
	my $self		= shift;
	my @patterns	= @_;
	my @paths		= grep { reftype($_->predicate) eq 'ARRAY' and $_->predicate->[0] eq 'PATH' } @patterns;
	my @triples		= grep { blessed($_->predicate) } @patterns;
	if ($self->log->is_trace && (scalar(@patterns) > scalar(@paths) + scalar(@triples))) {
		$self->log->warn('More than just triples and paths passed to __new_bgp');
		$self->log->trace("Arguments to __new_bgp:\n" .Dumper(\@patterns));
	}
	
	my $bgp			= Attean::Algebra::BGP->new( triples => \@triples );
	if (@paths) {
		my @p;
		foreach my $p (@paths) {
			my $start	= $p->subject;
			my $end		= $p->object;
			my $pdata	= $p->predicate;
			push(@p, $self->__new_path( $start, $pdata, $end ));
		}
		if (scalar(@triples)) {
			return $self->_new_join($bgp, @p);
		} else {
			return $self->_new_join(@p);
		}
	} else {
		return $bgp;
	}
}

=item C<new_binary_expression ( $operator, @operands )>

Returns a new binary expression structure.

=cut

sub new_binary_expression {
	my $self		= shift;
	my $op			= shift;
	my @operands	= @_[0,1];
	return Attean::BinaryExpression->new( operator => $op, children => \@operands );
}

=item C<new_function_expression ( $function, @operands )>

Returns a new function expression structure.

=cut

sub new_function_expression {
	my $self		= shift;
	my $function	= shift;
	my @operands	= @_;
	my $base		= $self->__base;
	return Attean::FunctionExpression->new( operator => $function, children => \@operands, $base ? (base => $base) : () );
}

sub _new_join {
	my $self	= shift;
	my @parts	= @_;
	if (0 == scalar(@parts)) {
		return Attean::Algebra::BGP->new();
	} elsif (1 == scalar(@parts)) {
		return shift(@parts);
	} else {
		return Attean::Algebra::Join->new( children => \@parts );
	}
}

sub _peek_token {
	my $self	= shift;
	my $l		= $self->lexer;
	my $t		= $l->peek;
	return unless ($t);
	while ($t == COMMENT) {
		$t		= $l->peek;
		return unless ($t);
	}
	return $t;
}

sub _test_token {
	my $self	= shift;
	my $type	= shift;
	my $t		= $self->_peek_token;
	return unless ($t);
	return if ($t->type != $type);
	if (@_) {
		my $value	= shift;
		if (ref($value) eq 'Regexp') {
			return unless ($t->value =~ $value);
		} else {
			return unless ($t->value eq $value);
		}
	}
	return 1;
}

sub _optional_token {
	my $self	= shift;
	if ($self->_test_token(@_)) {
		return $self->_next_token;
	}
	return;
}

sub _next_token {
	my $self	= shift;
	my $l		= $self->lexer;
	my $t		= $l->next;
	while ($t->type == COMMENT) {
		$t		= $l->peek;
		return unless ($t);
	}
	return $t;
}

sub _expected_token {
	my $self	= shift;
	my $type	= shift;
	if ($self->_test_token($type, @_)) {
		return $self->_next_token;
	} else {
		my $t			= $self->_peek_token;
		my $expecting	= AtteanX::SPARQL::Constants::decrypt_constant($type);
		my $got			= blessed($t) ? AtteanX::SPARQL::Constants::decrypt_constant($t->type) : '(undef)';
		if (@_) {
			my $value	= shift;
			if ($t) {
				my $value2	= $t->value;
				confess "Expecting $expecting '$value' but got $got '$value2' before " . $self->lexer->buffer;
			} else {
				confess "Expecting $expecting '$value' but found EOF";
			}
		} else {
			confess "Expecting $expecting but found $got before " . $self->lexer->buffer;
		}
	}
}

sub _token_error {
	my $self	= shift;
	my $t		= shift;
	my $note	= shift;
	my $got		= blessed($t) ? AtteanX::SPARQL::Constants::decrypt_constant($t->type) : '(undef)';
	my $message	= "$note but got $got";
	if ($t and $t->start_line > 0) {
		my $l	= $t->start_line;
		my $c	= $t->start_column;
		$message	.= " at $l:$c";
	} else {
		my $n 		= $self->lexer->buffer;
		$n			=~ s/\s+/ /g;
		$n			=~ s/\s*$//;
		if ($n) {
			$message	.= " near '$n'";
		}
	}
	croak $message;
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
