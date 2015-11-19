use v5.14;
use warnings;

=head1 NAME

AtteanX::Parser::SPARQL - SPARQL 1.1 Parser.

=head1 VERSION

This document describes AtteanX::Parser::SPARQL version 2.916.

=head1 SYNOPSIS

 use AtteanX::Parser::SPARQL;
 my $parser	= AtteanX::Parser::SPARQL->new();
 my $iterator = $parser->parse( $query, $base_uri );

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::SPARQL;

use strict;
use warnings;
no warnings 'redefine';

use Attean;
use RDF::Query;
use URI;
use URI::NamespaceMap;
use Data::Dumper;
use RDF::Query::Parser;
use RDF::Query::Algebra;
use AtteanX::Parser::SPARQLLex;
use AtteanX::Parser::SPARQL::Constants;
use RDF::Trine::Namespace qw(rdf);
use Types::Standard qw(InstanceOf HashRef ArrayRef Bool Str);
use Scalar::Util qw(blessed looks_like_number reftype);

######################################################################

our ($VERSION);
BEGIN {
	$VERSION	= '2.916';
}

######################################################################

my $rdf			= RDF::Trine::Namespace->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#');
my $xsd			= RDF::Trine::Namespace->new('http://www.w3.org/2001/XMLSchema#');

our $r_ECHAR				= qr/\\([tbnrf\\"'])/o;
our $r_STRING_LITERAL1		= qr/'(([^\x{27}\x{5C}\x{0A}\x{0D}])|${r_ECHAR})*'/o;
our $r_STRING_LITERAL2		= qr/"(([^\x{22}\x{5C}\x{0A}\x{0D}])|${r_ECHAR})*"/o;
our $r_STRING_LITERAL_LONG1	= qr/'''(('|'')?([^'\\]|${r_ECHAR}))*'''/o;
our $r_STRING_LITERAL_LONG2	= qr/"""(("|"")?([^"\\]|${r_ECHAR}))*"""/o;
our $r_LANGTAG				= qr/@[a-zA-Z]+(-[a-zA-Z0-9]+)*/o;
our $r_IRI_REF				= qr/<([^<>"{}|^`\\\x{00}-\x{20}])*>/o;
our $r_PN_CHARS_BASE		= qr/([A-Z]|[a-z]|[\x{00C0}-\x{00D6}]|[\x{00D8}-\x{00F6}]|[\x{00F8}-\x{02FF}]|[\x{0370}-\x{037D}]|[\x{037F}-\x{1FFF}]|[\x{200C}-\x{200D}]|[\x{2070}-\x{218F}]|[\x{2C00}-\x{2FEF}]|[\x{3001}-\x{D7FF}]|[\x{F900}-\x{FDCF}]|[\x{FDF0}-\x{FFFD}]|[\x{10000}-\x{EFFFF}])/o;
our $r_PN_CHARS_U			= qr/([_]|${r_PN_CHARS_BASE})/o;
our $r_VARNAME				= qr/((${r_PN_CHARS_U}|[0-9])(${r_PN_CHARS_U}|[0-9]|\x{00B7}|[\x{0300}-\x{036F}]|[\x{203F}-\x{2040}])*)/o;
our $r_VAR1					= qr/[?]${r_VARNAME}/o;
our $r_VAR2					= qr/[\$]${r_VARNAME}/o;
our $r_PN_CHARS				= qr/${r_PN_CHARS_U}|-|[0-9]|\x{00B7}|[\x{0300}-\x{036F}]|[\x{203F}-\x{2040}]/o;
our $r_PN_PREFIX			= qr/(${r_PN_CHARS_BASE}((${r_PN_CHARS}|[.])*${r_PN_CHARS})?)/o;
our $r_PN_LOCAL_ESCAPED		= qr{(\\([-~.!&'()*+,;=/?#@%_\$]))|%[0-9A-Fa-f]{2}}o;
our $r_PN_LOCAL				= qr/((${r_PN_CHARS_U}|[:0-9]|${r_PN_LOCAL_ESCAPED})((${r_PN_CHARS}|${r_PN_LOCAL_ESCAPED}|[:.])*(${r_PN_CHARS}|[:]|${r_PN_LOCAL_ESCAPED}))?)/o;
our $r_PN_LOCAL_BNODE		= qr/((${r_PN_CHARS_U}|[0-9])((${r_PN_CHARS}|[.])*${r_PN_CHARS})?)/o;
our $r_PNAME_NS				= qr/((${r_PN_PREFIX})?:)/o;
our $r_PNAME_LN				= qr/(${r_PNAME_NS}${r_PN_LOCAL})/o;
our $r_EXPONENT				= qr/[eE][-+]?\d+/o;
our $r_DOUBLE				= qr/\d+[.]\d*${r_EXPONENT}|[.]\d+${r_EXPONENT}|\d+${r_EXPONENT}/o;
our $r_DECIMAL				= qr/(\d+[.]\d*)|([.]\d+)/o;
our $r_INTEGER				= qr/\d+/o;
our $r_BLANK_NODE_LABEL		= qr/_:${r_PN_LOCAL_BNODE}/o;
our $r_ANON					= qr/\[[\t\r\n ]*\]/o;
our $r_NIL					= qr/\([\n\r\t ]*\)/o;
our $r_AGGREGATE_CALL		= qr/(MIN|MAX|COUNT|AVG|SUM|SAMPLE|GROUP_CONCAT)\b/io;

=item C<< new >>

Returns a new SPARQL 1.1 parser object.

=cut

use Moo;

has 'lexer' 		=> (is => 'rw', isa => InstanceOf['AtteanX::Parser::SPARQLLex::Iterator']);
has 'args'			=> (is => 'ro', isa => HashRef);
has 'build'			=> (is => 'rw', isa => HashRef);
has 'update'		=> (is => 'rw', isa => Bool);
has 'namespaces'	=> (is => 'rw', isa => InstanceOf['URI::NamespaceMap'], default => sub { URI::NamespaceMap->new() });
has 'baseURI'		=> (is => 'rw');
has 'stack'			=> (is => 'rw', isa => ArrayRef);
has 'filters'		=> (is => 'rw', isa => ArrayRef);
has 'pattern_container_stack'	=> (is => 'rw', isa => ArrayRef);

sub file_extensions { return [qw(rq ru)] }

sub canonical_media_type { return "application/sparql-query" }

sub media_types {
	return [qw(application/sparql-query application/sparql-update)];
}

sub handled_type {
	state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::Algebra');
	return $ITEM_TYPE;
}

with 'Attean::API::AtOnceParser', 'Attean::API::Parser', 'Attean::API::AbbreviatingParser';

sub BUILDARGS {
	my $class	= shift;
	my %args	= @_;
	my $ns		= delete $args{namespaces} // 	URI::NamespaceMap->new();
	return { args => \%args, namespaces => $ns };
}

################################################################################

sub parse_list_from_io {
	my $self	= shift;
	my $p 		= AtteanX::Parser::SPARQLLex->new();
	my $l		= $p->parse_iter_from_io(@_);
	$self->lexer($l);
	my $q		= $self->parse($self->{args}{base});
	return unless (ref($q));
	my $a	= $q->{triples}[0];
	return unless (ref($a));
	return $a;
}

sub parse_list_from_bytes {
	my $self	= shift;
	my $p 		= AtteanX::Parser::SPARQLLex->new();
	my $l		= $p->parse_iter_from_bytes(@_);
	$self->lexer($l);
	my $q		= $self->parse($self->{args}{base});
	return unless (ref($q));
	my $a	= $q->{triples}[0];
	return unless (ref($a));
	return $a;
}

=item C<< parse ( $query, $base_uri, $update_flag ) >>

Parses the C<< $query >>, using the given C<< $base_uri >>.
If C<< $update_flag >> is true, the query will be parsed allowing
SPARQL 1.1 Update statements.

=cut

sub parse {
	my $self	= shift;
	
	my $t		= $self->lexer->peek;
	unless (defined($t)) {
		die "No query string found to parse";
	}

	my $baseuri	= shift;
	my $update	= shift || 0;

	$self->baseURI($baseuri);
	$self->stack([]);
	$self->filters([]);
	$self->pattern_container_stack([]);
	$self->update($update);
	my $triples								= $self->_push_pattern_container();
	my $build								= { sources => [], triples => $triples };
	$self->build($build);
	if ($baseuri) {
		$build->{base}	= $baseuri;
	}

# 	try {
		$self->_RW_Query();
# 	} catch RDF::Query::Error with {
# 		my $e	= shift;
# 		$self->{build}	= undef;
# 		$build			= undef;
# 	} otherwise {
# 		my $e	= shift;
# 		$self->{build}	= undef;
# 		$build			= undef;
# 	};
	
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
	while (1) {
		if ($self->test_token(KEYWORD, 'SELECT')) {
			$self->_SelectQuery();
			$read_query++;
		} elsif ($self->test_token(KEYWORD, 'CONSTRUCT')) {
			$self->_ConstructQuery();
			$read_query++;
		} elsif ($self->test_token(KEYWORD, 'DESCRIBE')) {
			$self->_DescribeQuery();
			$read_query++;
		} elsif ($self->test_token(KEYWORD, 'ASK')) {
			$self->_AskQuery();
			$read_query++;
		} elsif ($self->test_token(KEYWORD, 'CREATE')) {
			unless ($self->{update}) {
				die "CREATE GRAPH update forbidden in read-only queries";
			}
			$self->_CreateGraph();
		} elsif ($self->test_token(KEYWORD, 'DROP')) {
			unless ($self->{update}) {
				die "DROP GRAPH update forbidden in read-only queries";
			}
			$self->_DropGraph();
		} elsif ($self->test_token(KEYWORD, 'LOAD')) {
			unless ($self->{update}) {
				die "LOAD update forbidden in read-only queries"
			}
			$self->_LoadUpdate();
		} elsif ($self->test_token(KEYWORD, 'CLEAR')) {
			unless ($self->{update}) {
				die "CLEAR GRAPH update forbidden in read-only queries";
			}
			$self->_ClearGraphUpdate();
		} elsif ($self->test_token(KEYWORD, 'WITH') or $self->test_token(KEYWORD, 'INSERT') or $self->test_token(KEYWORD, 'DELETE')) {
			unless ($self->{update}) {
				die "INSERT/DELETE update forbidden in read-only queries";
			}
			my ($graph);
			if ($self->_test(qr/WITH/)) {
				$self->{build}{custom_update_dataset}	= 1;
				$self->_eat_with_ws(qr/WITH/i);
				$self->_IRIref;
				($graph)	= splice( @{ $self->{stack} } );
			}
			if ($self->_test(qr/INSERT/ims)) {
				$self->_eat_with_ws(qr/INSERT/i);
				if ($self->_test(qr/DATA/i)) {
					unless ($self->{update}) {
						die "INSERT DATA update forbidden in read-only queries";
					}
					$self->_eat_with_ws(qr/DATA/i);
					$self->_InsertDataUpdate();
				} else {
					$self->_InsertUpdate($graph);
				}
			} elsif ($self->_test(qr/DELETE/ims)) {
				$self->_eat_with_ws(qr/DELETE/i);
				if ($self->_test(qr/DATA/i)) {
					unless ($self->{update}) {
						die "DELETE DATA update forbidden in read-only queries";
					}
					$self->_eat_with_ws(qr/DATA/i);
					$self->_DeleteDataUpdate();
				} else {
					$self->_DeleteUpdate($graph);
				}
			}
		} elsif ($self->test_token(KEYWORD, 'COPY')) {
			$self->_CopyUpdate();
		} elsif ($self->test_token(KEYWORD, 'MOVE')) {
			$self->_MoveUpdate();
		} elsif ($self->test_token(KEYWORD, 'ADD')) {
			$self->_AddUpdate();
		} elsif ($self->test_token(SEMICOLON)) {
			$self->expected_token(SEMICOLON);
			next if ($self->_Query_test);
			last;
		} else {
			my $l		= Log::Log4perl->get_logger("rdf.query");
			if ($l->is_debug) {
				$l->logcluck("Syntax error: Expected query type with input <<$self->{tokens}>>");
			}
			die 'Syntax error: Expected query type';
		}

		last if ($read_query);
		if ($self->optional_token(SEMICOLON)) {
			if ($self->_Query_test) {
				next;
			}
		}
		last;
	}
#	$self->_eat(qr/;/) if ($self->_test(qr/;/));

	my $count	= scalar(@{ $self->{build}{triples} });
	
	my $t	= $self->peek_token;
	if ($t) {
		my $type	= AtteanX::Parser::SPARQL::Constants::decrypt_constant($t->type);
		die "Syntax error: Remaining input after query: $type";
	}

	if ($count == 0 or $count > 1) {
		my @patterns	= splice(@{ $self->{build}{triples} });
		my $pattern		= Attean::Algebra::Sequence->new( children => \@patterns );
		warn 'TODO: $pattern->check_duplicate_blanks';
		$self->{build}{triples}	= [ $pattern ];
	}

# 	my %query	= (%p, %body);
# 	return \%query;
}

sub _Query_test {
	my $self	= shift;
	return 1 if ($self->_test(qr/SELECT|CONSTRUCT|DESCRIBE|ASK|LOAD|CLEAR|DROP|ADD|MOVE|COPY|CREATE|INSERT|DELETE|WITH/i));
	return 0;
}

# [2] Prologue ::= BaseDecl? PrefixDecl*
# [3] BaseDecl ::= 'BASE' IRI_REF
# [4] PrefixDecl ::= 'PREFIX' PNAME_NS IRI_REF
sub _Prologue {
	my $self	= shift;

	my $base;
	my @base;
	if ($self->optional_token(KEYWORD, 'BASE')) {
		my $iriref	= $self->expected_token(IRI);
		my $iri		= $iriref->value;
		$base		= Attean::IRI->new( value => $iri );
		@base		= $base;
		$self->{base}	= $base;
	}

	my %namespaces;
	while ($self->optional_token(KEYWORD, 'PREFIX')) {
		my $prefix	= $self->expected_token(PREFIXNAME);
		my $ns		= substr($prefix->value, 0, length($prefix->value) - 1);
		my $iriref	= $self->expected_token(IRI);
		my $iri		= $iriref->value;
		if (@base) {
			my $r	= Attean::IRI->new( value => $iri, base => shift(@base) );
			$iri	= $r->value;
		}
		$namespaces{ $ns }	= $iri;
		$self->{namespaces}{$ns}	= $iri;
	}

	$self->{build}{namespaces}	= \%namespaces;
	$self->{build}{base}		= $base if (defined($base));

# 	push(@data, (base => $base)) if (defined($base));
# 	return @data;
}

sub _InsertDataUpdate {
	my $self	= shift;
	$self->expected_token(LBRACE);
	local($self->{__data_pattern})	= 1;
	$self->_ModifyTemplate();
	my $data	= $self->_remove_pattern;
	$self->expected_token(RBRACE);
	
	my $empty	= Attean::Algebra::BGP->new();
	my $insert	= RDF::Query::Algebra::Update->new(undef, $data, $empty, undef, 1);
	$self->_add_patterns( $insert );
	$self->{build}{method}		= 'UPDATE';
}

sub _DeleteDataUpdate {
	my $self	= shift;
	$self->expected_token(LBRACE);
	local($self->{__data_pattern})	= 1;
	local($self->{__no_bnodes})		= "DELETE DATA block";
	$self->_ModifyTemplate();
	my $data	= $self->_remove_pattern;
	$self->expected_token(RBRACE);
	
	my $empty	= Attean::Algebra::BGP->new();
	my $delete	= RDF::Query::Algebra::Update->new($data, undef, $empty, undef, 1);
	$self->_add_patterns( $delete );
	$self->{build}{method}		= 'UPDATE';
}

sub _InsertUpdate {
	my $self	= shift;
	my $graph	= shift;
	$self->expected_token(LBRACE);
	$self->_ModifyTemplate();
	my $data	= $self->_remove_pattern;
	$self->expected_token(RBRACE);
	if ($graph) {
		$data	= Attean::Algebra::Graph->new( children => [$data], graph => $graph );
	}


	my %dataset;
	while ($self->_test(qr/USING/i)) {
		$self->{build}{custom_update_dataset}	= 1;
		$self->_eat_with_ws(qr/USING/i);
		my $named	= 0;
		if ($self->_test(qr/NAMED/i)) {
			$self->_eat_with_ws(qr/NAMED/i);
			$named	= 1;
		}
		$self->_IRIref;
		my ($iri)	= splice( @{ $self->{stack} } );
		if ($named) {
			$dataset{named}{$iri->uri_value}	= $iri;
		} else {
			push(@{ $dataset{default} }, $iri );
		}
	}

	$self->_eat_with_ws(qr/WHERE/i);
	if ($graph) {
		$self->_GroupGraphPattern;
		my $ggp	= $self->_remove_pattern;
		$ggp	= Attean::Algebra::Graph->new( children => [$ggp], graph => $graph );
		$self->_add_patterns( $ggp );
	} else {
		$self->_GroupGraphPattern;
	}

	my $ggp	= $self->_remove_pattern;

	my @ds_keys	= keys %dataset;
	unless (@ds_keys) {
		$dataset{ default }	= [$graph || ()];
	}
	
	my $insert	= RDF::Query::Algebra::Update->new(undef, $data, $ggp, \%dataset, 0);
	$self->_add_patterns( $insert );
	$self->{build}{method}		= 'UPDATE';
}

sub _DeleteUpdate {
	my $self	= shift;
	my $graph	= shift;
	my ($delete_data, $insert_data);
	
	my %dataset;
	my $delete_where	= 0;
	if ($self->_test(qr/WHERE/i)) {
		if ($graph) {
			die "Syntax error: WITH clause cannot be used with DELETE WHERE operations";
		}
		$delete_where	= 1;
	} else {
		{
			local($self->{__no_bnodes})		= "DELETE block";
			$self->_eat_with_ws('{');
			$self->_ModifyTemplate( $graph );
			$self->_eat('}');
		}
		$delete_data	= $self->_remove_pattern;
		
		if ($self->_test(qr/INSERT/i)) {
			$self->_eat_with_ws(qr/INSERT/i);
			$self->_eat_with_ws('{');
			$self->_ModifyTemplate( $graph );
			$self->_eat_with_ws('}');
			$insert_data	= $self->_remove_pattern;
		}
		
		while ($self->_test(qr/USING/i)) {
			$self->{build}{custom_update_dataset}	= 1;
			$self->_eat_with_ws(qr/USING/i);
			my $named	= 0;
			if ($self->_test(qr/NAMED/i)) {
				$self->_eat_with_ws(qr/NAMED/i);
				$named	= 1;
			}
			$self->_IRIref;
			my ($iri)	= splice( @{ $self->{stack} } );
			if ($named) {
				$dataset{named}{$iri->uri_value}	= $iri;
			} else {
				push(@{ $dataset{default} }, $iri );
			}
		}
	}
	
	$self->_eat_with_ws(qr/WHERE/i);
	if ($graph) {
		$self->{__no_bnodes}	= "DELETE WHERE block" if ($delete_where);
		$self->_GroupGraphPattern;
		delete $self->{__no_bnodes};
		my $ggp	= $self->_remove_pattern;
		$ggp	= Attean::Algebra::Graph->new( children => [$ggp], graph => $graph );
		$self->_add_patterns( $ggp );
	} else {
		$self->{__no_bnodes}	= "DELETE WHERE block" if ($delete_where);
		$self->_GroupGraphPattern;
		delete $self->{__no_bnodes};
	}

	my $ggp	= $self->_remove_pattern;

	if ($delete_where) {
		$delete_data	= $ggp;
	}
	
	my @ds_keys	= keys %dataset;
	if ($graph and not(scalar(@ds_keys))) {
		$dataset{ default }	= [$graph || ()];
	}
	
	my $insert	= RDF::Query::Algebra::Update->new($delete_data, $insert_data, $ggp, \%dataset, 0);
	$self->_add_patterns( $insert );
	$self->{build}{method}		= 'UPDATE';
}

sub _ModifyTemplate_test {
	my $self	= shift;
	return 1 if ($self->_TriplesBlock_test);
	return 1 if ($self->_test(qr/GRAPH/i));
	return 0;
}

sub _ModifyTemplate {
	my $self	= shift;
	my $graph	= shift;
	
	local($self->{named_graph});
	if ($graph) {
		$self->{named_graph}	= $graph;
	}
	
	my $data;
	while ($self->_ModifyTemplate_test) {
		$self->__ModifyTemplate( $graph );
		my $d			= $self->_remove_pattern;
		my @patterns	= blessed($data) ? $data->patterns : ();
		$data			= $self->new_join(@patterns, $d);
	}
	$data	= Attean::Algebra::BGP->new() unless (blessed($data));
	$self->_add_patterns( $data );
}

sub __ModifyTemplate {
	my $self	= shift;
	my $graph	= shift;
	local($self->{_modify_template})	= 1;
	if ($self->_TriplesBlock_test) {
		my $data;
		$self->_push_pattern_container;
		$self->_TriplesBlock;
		($data)	= @{ $self->_pop_pattern_container };
		if ($graph) {
			$data	= Attean::Algebra::Graph->new( children => $data, graph => $graph );
		}
		$self->_add_patterns( $data );
	} else {
		$self->_GraphGraphPattern;
		
		{
			my (@d)	= splice(@{ $self->{stack} });
			$self->__handle_GraphPatternNotTriples( @d );
		}
	}
}

sub _LoadUpdate {
	my $self	= shift;
	my $op		= $self->_eat(qr/LOAD\s+(SILENT\s+)?/i);
	my $silent	= ($op =~ /SILENT/);
	$self->_IRIref;
	my ($iri)	= splice( @{ $self->{stack} } );
	if ($self->_test(qr/INTO GRAPH/i)) {
		$self->_eat(qr/INTO GRAPH/i);
		$self->_ws;
		$self->_IRIref;
		my ($graph)	= splice( @{ $self->{stack} } );
		my $pat	= RDF::Query::Algebra::Load->new( $iri, $graph, $silent );
		$self->_add_patterns( $pat );
	} else {
		my $pat	= RDF::Query::Algebra::Load->new( $iri, undef, $silent );
		$self->_add_patterns( $pat );
	}
	$self->{build}{method}		= 'LOAD';
}

sub _CreateGraph {
	my $self	= shift;
	my $op		= $self->_eat(qr/CREATE\s+(SILENT\s+)?GRAPH/i);
	my $silent	= ($op =~ /SILENT/i);
	$self->_ws;
	$self->_IRIref;
	my ($graph)	= splice( @{ $self->{stack} } );
	my $pat	= RDF::Query::Algebra::Create->new( $graph );
	$self->_add_patterns( $pat );
	$self->{build}{method}		= 'CREATE';
}

sub _ClearGraphUpdate {
	my $self	= shift;
	my $op		= $self->_eat(qr/CLEAR(\s+SILENT)?/i);
	my $silent	= ($op =~ /SILENT/i);
	$self->_ws;
	if ($self->_test(qr/GRAPH/i)) {
		$self->_eat(qr/GRAPH/i);
		$self->_ws;
		$self->_IRIref;
		my ($graph)	= splice( @{ $self->{stack} } );
		my $pat	= RDF::Query::Algebra::Clear->new( $graph );
		$self->_add_patterns( $pat );
	} elsif ($self->_test(qr/DEFAULT/i)) {
		$self->_eat(qr/DEFAULT/i);
		my $pat	= RDF::Query::Algebra::Clear->new( RDF::Trine::Node::Nil->new );
		$self->_add_patterns( $pat );
	} elsif ($self->_test(qr/NAMED/i)) {
		$self->_eat(qr/NAMED/i);
		my $pat	= RDF::Query::Algebra::Clear->new( Attean::IRI->new(value => 'tag:gwilliams@cpan.org,2010-01-01:RT:NAMED') );
		$self->_add_patterns( $pat );
	} elsif ($self->_test(qr/ALL/i)) {
		$self->_eat(qr/ALL/i);
		my $pat	= RDF::Query::Algebra::Clear->new( Attean::IRI->new(value => 'tag:gwilliams@cpan.org,2010-01-01:RT:ALL') );
		$self->_add_patterns( $pat );
	}
	$self->{build}{method}		= 'CLEAR';
}

sub _DropGraph {
	my $self	= shift;
	my $op		= $self->_eat(qr/DROP(\s+SILENT)?/i);
	my $silent	= ($op =~ /SILENT/i);
	$self->_ws;
	if ($self->_test(qr/GRAPH/i)) {
		$self->_eat(qr/GRAPH/i);
		$self->_ws;
		$self->_IRIref;
		my ($graph)	= splice( @{ $self->{stack} } );
		my $pat	= RDF::Query::Algebra::Clear->new( $graph );
		$self->_add_patterns( $pat );
	} elsif ($self->_test(qr/DEFAULT/i)) {
		$self->_eat(qr/DEFAULT/i);
		my $pat	= RDF::Query::Algebra::Clear->new( RDF::Trine::Node::Nil->new );
		$self->_add_patterns( $pat );
	} elsif ($self->_test(qr/NAMED/i)) {
		$self->_eat(qr/NAMED/i);
		my $pat	= RDF::Query::Algebra::Clear->new( Attean::IRI->new(value => 'tag:gwilliams@cpan.org,2010-01-01:RT:NAMED') );
		$self->_add_patterns( $pat );
	} elsif ($self->_test(qr/ALL/i)) {
		$self->_eat(qr/ALL/i);
		my $pat	= RDF::Query::Algebra::Clear->new( Attean::IRI->new(value => 'tag:gwilliams@cpan.org,2010-01-01:RT:ALL') );
		$self->_add_patterns( $pat );
	}
	$self->{build}{method}		= 'CLEAR';
}

sub __graph {
	my $self	= shift;
	if ($self->_test(qr/DEFAULT/i)) {
		$self->_eat(qr/DEFAULT/i);
		return RDF::Trine::Node::Nil->new();
	} else {
		if ($self->_test(qr/GRAPH/)) {
			$self->_eat_with_ws(qr/GRAPH/i);
		}
		$self->_IRIref;
		my ($g)	= splice( @{ $self->{stack} } );
		return $g;
	}
}

sub _CopyUpdate {
	my $self	= shift;
	my $op		= $self->_eat(qr/COPY(\s+SILENT)?/i);
	my $silent	= ($op =~ /SILENT/i);
	$self->_ws;
	my $from	= $self->__graph();
	$self->_ws;
	$self->_eat(qr/TO/i);
	$self->_ws;
	my $to	= $self->__graph();
	my $pattern	= RDF::Query::Algebra::Copy->new( $from, $to, $silent );
	$self->_add_patterns( $pattern );
	$self->{build}{method}		= 'UPDATE';
}

sub _MoveUpdate {
	my $self	= shift;
	my $op		= $self->_eat(qr/MOVE(\s+SILENT)?/i);
	my $silent	= ($op =~ /SILENT/i);
	$self->_ws;
	my $from	= $self->__graph();
	$self->_ws;
	$self->_eat(qr/TO/i);
	$self->_ws;
	my $to	= $self->__graph();
	my $pattern	= RDF::Query::Algebra::Move->new( $from, $to, $silent );
	$self->_add_patterns( $pattern );
	$self->{build}{method}		= 'UPDATE';
}

sub _AddUpdate {
	my $self	= shift;
	my $op		= $self->_eat(qr/ADD(\s+SILENT)?/i);
	my $silent	= ($op =~ /SILENT/i);
	$self->_ws;
	return $self->__UpdateShortcuts( 'ADD', $silent );
}

sub __UpdateShortcuts {
	my $self	= shift;
	my $op		= shift;
	my $silent	= shift;
	my ($from, $to);
	if ($self->_test(qr/DEFAULT/i)) {
		$self->_eat(qr/DEFAULT/i);
	} else {
		if ($self->_test(qr/GRAPH/)) {
			$self->_eat_with_ws(qr/GRAPH/i);
		}
		$self->_IRIref;
		($from)	= splice( @{ $self->{stack} } );
	}
	$self->_ws;
	$self->_eat(qr/TO/i);
	$self->_ws;
	if ($self->_test(qr/DEFAULT/i)) {
		$self->_eat(qr/DEFAULT/i);
	} else {
		if ($self->_test(qr/GRAPH/)) {
			$self->_eat_with_ws(qr/GRAPH/i);
		}
		$self->_IRIref;
		($to)	= splice( @{ $self->{stack} } );
	}
	
	my $from_pattern	= RDF::Query::Algebra::GroupGraphPattern->new(
							RDF::Query::Algebra::BasicGraphPattern->new(
								RDF::Query::Algebra::Triple->new(
									map { Attean::Variable->new( $_ ) } qw(s p o)
								)
							)
						);
	if (defined($from)) {
		$from_pattern	= RDF::Query::Algebra::NamedGraph->new( $from, $from_pattern );
	}

	my $to_pattern	= RDF::Query::Algebra::GroupGraphPattern->new(
							RDF::Query::Algebra::BasicGraphPattern->new(
								RDF::Query::Algebra::Triple->new(
									map { Attean::Variable->new( $_ ) } qw(s p o)
								)
							)
						);
	if (defined($to)) {
		$to_pattern	= RDF::Query::Algebra::NamedGraph->new( $to, $to_pattern );
	}
	
	my $to_graph	= $to || RDF::Trine::Node::Nil->new;
	my $from_graph	= $from || RDF::Trine::Node::Nil->new;
	my $drop_to		= RDF::Query::Algebra::Clear->new( $to_graph, $silent );
	my $update		= RDF::Query::Algebra::Update->new( undef, $to_pattern, $from_pattern, undef, 0 );
	my $drop_from	= RDF::Query::Algebra::Clear->new( $from_graph );
	my $pattern;
	if ($op eq 'MOVE') {
		$pattern		= Attean::Algebra::Sequence->new( children => [$drop_to, $update, $drop_from] );
	} elsif ($op eq 'COPY') {
		$pattern		= Attean::Algebra::Sequence->new( children => [$drop_to, $update] );
	} else {
		$pattern		= $update;
	}
	$self->_add_patterns( $pattern );
	$self->{build}{method}		= 'UPDATE';
}

# [5] SelectQuery ::= 'SELECT' ( 'DISTINCT' | 'REDUCED' )? ( Var+ | '*' ) DatasetClause* WhereClause SolutionModifier
sub _SelectQuery {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'SELECT');
	if ($self->optional_token(KEYWORD, 'DISTINCT') or $self->optional_token(KEYWORD, 'REDUCED')) {
		$self->{build}{options}{distinct}	= 1;
	}
	
	my ($star, @exprs)	= $self->__SelectVars;
	
	$self->_DatasetClause();
	
	$self->_WhereClause;

	$self->_SolutionModifier();
	
	if ($self->optional_token(KEYWORD, 'VALUES')) {
		my @vars;
# 		$self->_Var;
# 		push( @vars, splice(@{ $self->{stack} }));
		my $parens	= 0;
		if ($self->optional_token(LPAREN)) {
			$parens	= 1;
		}
		while ($self->test_token(VAR)) {
			$self->_Var;
			push( @vars, splice(@{ $self->{stack} }));
		}
		if ($parens) {
			$self->expected_token(RPAREN);
		}
		
		my $count	= scalar(@vars);
		if (not($parens) and $count == 0) {
			die "Syntax error: Expected VAR in inline data declaration";
		} elsif (not($parens) and $count > 1) {
			die "Syntax error: Inline data declaration can only have one variable when parens are omitted";
		}
		
		my $short	= (not($parens) and $count == 1);
		$self->expected_token(LBRACE);
		if (not($short) or ($short and $self->_Binding_test)) {
			while ($self->_Binding_test) {
				my $terms	= $self->_Binding($count);
				push( @{ $self->{build}{bindings}{terms} }, $terms );
			}
		} else {
			while ($self->_BindingValue_test) {
				$self->_BindingValue;
				my ($term)	= splice(@{ $self->{stack} });
				push( @{ $self->{build}{bindings}{terms} }, [$term] );
			}
		}
		
		$self->expected_token(RBRACE);

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
		push(@{ $self->{build}{triples} }, $self->new_join($pattern, $table));
	}
	
	$self->__solution_modifiers( $star, @exprs );
	my $pattern	= $self->{build}{triples}[0];
	warn 'TODO: check for aggregates';
# 	my @agg		= $pattern->subpatterns_of_type( 'RDF::Query::Algebra::Aggregate', 'RDF::Query::Algebra::SubSelect' );
# 	if (@agg) {
# 		my ($agg)	= @agg;
# 		my @gvars	= $agg->groupby;
# 		if (scalar(@gvars) == 0) {
# 			# aggregate query with no explicit group keys
# 			foreach my $v (@{ $self->{build}{variables} }) {
# 				if ($v->isa('RDF::Query::Node::Variable')) {
# 					my $name	= $v->name;
# 					throw RDF::Query::Error::ParseError -text => "Syntax error: Variable used in projection but not present in aggregate grouping ($name)";
# 				}
# 			}
# 		}
# 	}
	
	delete $self->{build}{options};
	$self->{build}{method}		= 'SELECT';
}

sub __SelectVars {
	my $self	= shift;
	my $star	= 0;
	my @vars;
	my $count	= 0;
	my @exprs;
	while ($self->test_token(STAR) or $self->__SelectVar_test) {
		if ($self->test_token(STAR)) {
			$self->{build}{star}++;
			$self->expected_token(STAR);
			$star	= 1;
			$count++;
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
	warn 'TODO: verify projection does not contain repeated variables';
# 	foreach my $v (@vars) {
# 		if ($v->isa('RDF::Query::Node::Variable') or $v->isa('RDF::Query::Expression::Alias')) {
# 			my $name	= $v->name;
# 			if ($v->isa('RDF::Query::Expression::Alias')) {
# 				if ($seen{ $name }) {
# 					die "Syntax error: Repeated variable ($name) used in projection list";
# #					throw RDF::Query::Error::ParseError -text => "Syntax error: Repeated variable ($name) used in projection list";
# 				}
# 			}
# 			$seen{ $name }++;
# 		}
# 	}
	
	$self->{build}{variables}	= \@vars;
	if ($count == 0) {
		die "Syntax error: No select variable or expression specified";
	}
	return $star, @exprs;
}

sub _BrackettedAliasExpression {
	my $self	= shift;
	$self->expected_token(LPAREN);
	$self->_Expression;
	my ($expr)	= splice(@{ $self->{stack} });
	$self->expected_token(KEYWORD, 'AS');
	$self->_Var;
	my ($var)	= splice(@{ $self->{stack} });
	$self->expected_token(RPAREN);
	
	return ($var, $expr);
}

sub __SelectVar_test {
	my $self	= shift;
	local($self->{__aggregate_call_ok})	= 1;
#	return 1 if $self->_BuiltInCall_test;
	return 1 if $self->test_token(LPAREN);
	return $self->test_token(VAR);
}

sub __SelectVar {
	my $self	= shift;
	local($self->{__aggregate_call_ok})	= 1;
	if ($self->test_token(LPAREN)) {
		my ($var, $expr)	= $self->_BrackettedAliasExpression;
		return ($var, $expr);
	} else {
		$self->_Var;
		my ($var)	= splice(@{ $self->{stack} });
		return $var;
	}
}

# [6] ConstructQuery ::= 'CONSTRUCT' ConstructTemplate DatasetClause* WhereClause SolutionModifier
sub _ConstructQuery {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'CONSTRUCT');
	my $shortcut	= 1;
	if ($self->test_token(LBRACE)) {
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
	$self->expected_token(KEYWORD, 'DESCRIBE');
	
	if ($self->optional_token(STAR)) {
		$self->{build}{variables}	= ['*'];
	} else {
		$self->_VarOrIRIref;
		while ($self->_VarOrIRIref_test) {
			$self->_VarOrIRIref;
		}
		$self->{build}{variables}	= [ splice(@{ $self->{stack} }) ];
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
}

# [8] AskQuery ::= 'ASK' DatasetClause* WhereClause
sub _AskQuery {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'ASK');
	
	$self->_DatasetClause();
	
	$self->_WhereClause;
	
	$self->{build}{variables}	= [];
	$self->{build}{method}		= 'ASK';
	
	
	my $pattern	= $self->{build}{triples}[0];
	$self->{build}{triples}[0]	= Attean::Algebra::Ask->new( children => [$pattern] );
}

sub _DatasetClause_test {
	my $self	= shift;
	return $self->test_token(KEYWORD, 'FROM');
}

# [9] DatasetClause ::= 'FROM' ( DefaultGraphClause | NamedGraphClause )
sub _DatasetClause {
	my $self	= shift;
	
# 	my @dataset;
 	$self->{build}{sources}	= [];
 	while ($self->optional_token(KEYWORD, 'FROM')) {
 		if ($self->test_token(KEYWORD, 'NAMED')) {
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
	my ($source)	= splice(@{ $self->{stack} });
	push( @{ $self->{build}{sources} }, [$source] );
}

# [11] NamedGraphClause ::= 'NAMED' SourceSelector
sub _NamedGraphClause {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'NAMED');
	$self->_SourceSelector;
	my ($source)	= splice(@{ $self->{stack} });
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
	return $self->_test( qr/WHERE|{/i );
}
sub _WhereClause {
	my $self	= shift;
	$self->optional_token(KEYWORD, 'WHERE');
	$self->_GroupGraphPattern;
	
	my $ggp	= $self->_peek_pattern;
	warn 'TODO: $ggp->check_duplicate_blanks';
}

sub _TriplesWhereClause {
	my $self	= shift;
	$self->_push_pattern_container;
	
	$self->expected_token(KEYWORD, 'WHERE');
	$self->expected_token(LBRACE);
	if ($self->_TriplesBlock_test) {
		$self->_TriplesBlock;
	}
	$self->expected_token(RBRACE);
	
	my $cont		= $self->_pop_pattern_container;
	$self->{build}{construct_triples}	= $cont->[0];
	
	my $pattern	= $self->new_join(@$cont);
	$self->_add_patterns( $pattern );
}

sub _Binding_test {
	my $self	= shift;
	return $self->test_token(LPAREN);
}

sub _Binding {
	my $self	= shift;
	my $count	= shift;
	
	$self->expected_token(LPAREN);
	
	my @terms;
	foreach my $i (1..$count) {
		unless ($self->_BindingValue_test) {
			my $found	= $i-1;
			die "Syntax error: Expected $count BindingValues but only found $found";
		}
		$self->_BindingValue;
		push( @terms, splice(@{ $self->{stack} }));
	}
	$self->expected_token(RPAREN);
	return \@terms;
}

sub _BindingValue_test {
	my $self	= shift;
	return 1 if ($self->_IRIref_test);
	return 1 if ($self->test_token(KEYWORD, 'UNDEF'));
	return 1 if ($self->test_token(BOOLEAN));
	return 1 if ($self->test_token(INTEGER));
	return 1 if ($self->test_token(DECIMAL));
	return 1 if ($self->test_token(DOUBLE));
	return 1 if ($self->test_token(IRI));
	return 1 if ($self->test_token(STRING1S));
	return 1 if ($self->test_token(STRING1D));
	return 1 if ($self->test_token(STRING3S));
	return 1 if ($self->test_token(STRING3D));
	return 1 if ($self->test_token(PREFIXNAME));
	return 1 if ($self->test_token(BNODE));
	return 1 if ($self->test_token(NIL));
	return 0;
# 	return 1 if ($self->_test(qr/UNDEF|[<'".0-9]|(true|false)\b|_:|\([\n\r\t ]*\)/));
# 	return 0;
}

sub _BindingValue {
	my $self	= shift;
	if ($self->optional_token(KEYWORD, 'UNDEF')) {
		push(@{ $self->{stack} }, undef);
	} else {
		$self->_GraphTerm;
	}
}

# [20]  	GroupCondition	  ::=  	( BuiltInCall | FunctionCall | '(' Expression ( 'AS' Var )? ')' | Var )
sub __GroupByVar_test {
	my $self	= shift;
	return 1 if ($self->_BuiltInCall_test);
	return 1 if ($self->_IRIref_test);
	return 1 if ($self->test_token(LPAREN));
	return 1 if ($self->test_token(VAR));
	return 0;
}

sub __GroupByVar {
	my $self	= shift;
	if ($self->optional_token(LPAREN)) {
		$self->_Expression;
		my ($expr)	= splice(@{ $self->{stack} });
		if ($self->optional_token(KEYWORD, 'AS')) {
			$self->_Var;
			my ($var)	= splice(@{ $self->{stack} });
			push(@{ $self->{build}{__group_vars} }, [$var, $expr]);
			my $vexpr	= Attean::ValueExpression->new( value => $var );
			$self->_add_stack( $vexpr );
		} else {
			$self->_add_stack( $expr );
		}
		$self->expected_token(RPAREN);
		
	} elsif ($self->_IRIref_test) {
		$$self->_FunctionCall;
	} elsif ($self->_BuiltInCall_test) {
		$self->_BuiltInCall;
	} else {
		$self->_Var;
		my $var		= pop(@{ $self->{stack} });
		my $expr	= Attean::ValueExpression->new(value => $var);
		$self->_add_stack($expr);
	}
}

# [14] SolutionModifier ::= OrderClause? LimitOffsetClauses?
sub _SolutionModifier {
	my $self	= shift;
	
	if ($self->test_token(KEYWORD, 'GROUP')) {
		$self->_GroupClause;
	}
	
	if ($self->test_token(KEYWORD, 'HAVING')) {
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
	$self->expected_token(KEYWORD, 'GROUP');
	$self->expected_token(KEYWORD, 'BY');
	
	if ($self->{build}{star}) {
		die "Syntax error: SELECT * cannot be used with aggregate grouping";
	}
	
	$self->{build}{__aggregate}	||= {};
	my @vars;
	$self->__GroupByVar;
	my ($v)	= splice(@{ $self->{stack} });
	push( @vars, $v );
	while ($self->__GroupByVar_test) {
		$self->__GroupByVar;
		my ($v)	= splice(@{ $self->{stack} });
		push( @vars, $v );
	}

	my %seen;
	foreach my $v (@vars) {
		if ($v->isa('RDF::Query::Node::Variable') or $v->isa('RDF::Query::Expression::Alias')) {
			die;
		} elsif ($v->does('Attean::API::Variable')) {
			my $name	= $v->value;
			$seen{ $name }++;
		}
	}
	
	warn 'TODO: verify that projection only includes aggregates and grouping variables';
# 	foreach my $v (@{ $self->{build}{variables} }) {
# 		if ($v->does('Attean::API::Variable')) {
# 			my $name	= $v->value;
# 			unless ($seen{ $name }) {
# 				die "Syntax error: Variable used in projection but not present in aggregate grouping ($name)";
# #				throw RDF::Query::Error::ParseError -text => "Syntax error: Variable used in projection but not present in aggregate grouping ($name)";
# 			}
# 		} elsif ($v->isa('RDF::Query::Node::Variable')) {
# 			my $name	= $v->name;
# 			unless ($seen{ $name }) {
# 				die "Syntax error: Variable used in projection but not present in aggregate grouping ($name)";
# #				throw RDF::Query::Error::ParseError -text => "Syntax error: Variable used in projection but not present in aggregate grouping ($name)";
# 			}
# 		} elsif ($v->isa('RDF::Query::Expression::Alias')) {
# 			my $expr	= $v->expression;
# # 			warn 'expression: ' . Dumper($expr);
# 			if ($expr->isa('RDF::Query::Node::Variable::ExpressionProxy')) {
# 				# RDF::Query::Node::Variable::ExpressionProxy is used for aggregate operations.
# 				# we can ignore these because any variable used in an aggreate is valid, even if it's not mentioned in the grouping keys
# 			} elsif ($expr->isa('RDF::Query::Expression')) {
# 				my @vars	= $expr->nonaggregated_referenced_variables;
# 				foreach my $name (@vars) {
# 					unless ($seen{ $name }) {
# 						die "Syntax error: Variable used in projection but not present in aggregate grouping ($name)";
# #						throw RDF::Query::Error::ParseError -text => "Syntax error: Variable used in projection but not present in aggregate grouping ($name)";
# 					}
# 				}
# 			}
# 		}
# 	}
	
	$self->{build}{__group_by}	= \@vars;
}

sub _HavingClause {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'HAVING');
	$self->{build}{__aggregate}	||= {};
	local($self->{__aggregate_call_ok})	= 1;
	$self->_Constraint;
	my ($expr) = splice(@{ $self->{stack} });
	$self->{build}{__having}	= $expr;
}

# [15] LimitOffsetClauses ::= ( LimitClause OffsetClause? | OffsetClause LimitClause? )
sub _LimitOffsetClauses_test {
	my $self	= shift;
	return 1 if ($self->test_token(KEYWORD, 'LIMIT'));
	return 1 if ($self->test_token(KEYWORD, 'OFFSET'));
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
	return 1 if ($self->test_token(KEYWORD, 'ORDER'));
	return 0;
}

sub _OrderClause {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'ORDER');
	$self->expected_token(KEYWORD, 'BY');
	my @order;
	$self->{build}{__aggregate}	||= {};
	local($self->{__aggregate_call_ok})	= 1;
	$self->_OrderCondition;
	push(@order, splice(@{ $self->{stack} }));
	while ($self->_OrderCondition_test) {
		$self->_OrderCondition;
		push(@order, splice(@{ $self->{stack} }));
	}
	$self->{build}{options}{orderby}	= \@order;
}

# [17] OrderCondition ::= ( ( 'ASC' | 'DESC' ) BrackettedExpression ) | ( Constraint | Var )
sub _OrderCondition_test {
	my $self	= shift;
	return 1 if ($self->test_token(KEYWORD, 'ASC'));
	return 1 if ($self->test_token(KEYWORD, 'DESC'));
	return 1 if ($self->test_token(VAR));
	return 1 if $self->_Constraint_test;
	return 0;
}

sub _OrderCondition {
	my $self	= shift;
	my $dir	= 'ASC';
	if ($self->test_token(KEYWORD, 'ASC') or $self->test_token(KEYWORD, 'DESC')) {
		my $dir;
		if ($self->optional_token(KEYWORD, 'ASC')) {
			$dir	= 'ASC';
		} else {
			$self->next_token;
			$dir	= 'DESC';
		}
		$self->_BrackettedExpression;
	} elsif ($self->test_token(VAR)) {
		$self->_Var;
		my $var		= pop(@{ $self->{stack} });
		my $expr	= Attean::ValueExpression->new(value => $var);
		$self->_add_stack($expr);
	} else {
		$self->_Constraint;
	}
	my ($expr)	= splice(@{ $self->{stack} });
	$self->_add_stack( [ $dir, $expr ] );
}

# [18] LimitClause ::= 'LIMIT' INTEGER
sub _LimitClause_test {
	my $self	= shift;
	return ($self->test_token(KEYWORD, 'LIMIT'));
}

sub _LimitClause {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'LIMIT');
	my $t		= $self->expected_token(INTEGER);
	$self->{build}{options}{limit}	= $t->value;
}

# [19] OffsetClause ::= 'OFFSET' INTEGER
sub _OffsetClause_test {
	my $self	= shift;
	return ($self->test_token(KEYWORD, 'OFFSET'));
}

sub _OffsetClause {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'OFFSET');
	my $t		= $self->expected_token(INTEGER);
	$self->{build}{options}{offset}	= $t->value;
}

# [20] GroupGraphPattern ::= '{' TriplesBlock? ( ( GraphPatternNotTriples | Filter ) '.'? TriplesBlock? )* '}'
sub _GroupGraphPattern {
	my $self	= shift;
	
	$self->expected_token(LBRACE);
	
	if ($self->_SubSelect_test) {
		$self->_SubSelect;
	} else {
		$self->_GroupGraphPatternSub;
	}

	$self->expected_token(RBRACE);
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
	
# 	my $pos	= length($self->{tokens});
	while (not $self->test_token(RBRACE)) {
		if ($self->_GraphPatternNotTriples_test) {
			$need_dot	= 0;
			$got_pattern++;
			$self->_GraphPatternNotTriples;
			my (@data)	= splice(@{ $self->{stack} });
			$self->__handle_GraphPatternNotTriples( @data );
		} elsif ($self->test_token(KEYWORD, 'FILTER')) {
			$got_pattern++;
			$need_dot	= 0;
			$self->_Filter;
		}
		
		if ($need_dot or $self->test_token(DOT)) {
			$self->expected_token(DOT);
			if ($got_pattern) {
				$need_dot		= 0;
				$got_pattern	= 0;
			} else {
				die "Syntax error: Extra dot found without preceding pattern";
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
					my $merged	= $self->new_join($lhs, $rhs);
					$self->_add_patterns( $merged );
				}
			} else {
				$self->_TriplesBlock;
			}
		}

		my $t	= $self->peek_token;
# 		
# 		last unless ($self->_test( qr/\S/ ));
# 		
# 		my $new	= length($self->{tokens});
# 		if ($pos == $new) {
# 			# we haven't progressed, and so would infinite loop if we don't break out and throw an error.
# 			$self->_syntax_error('');
# 		} else {
# 			$pos	= $new;
# 		}
	}
	
	my $cont		= $self->_pop_pattern_container;

	my @filters		= splice(@{ $self->{filters} });
	my @patterns;
	my $pattern		= $self->new_join(@$cont);
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
		my $ggp		= $self->new_join(@$cont);
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
		my $ggp		= $self->new_join(@$cont);
		$self->_push_pattern_container;
		# my $ggp	= $self->_remove_pattern();
		unless ($ggp) {
			$ggp	= Attean::Algebra::BGP->new();
		}

		my ($var, $expr)	= @args;
		warn 'TODO: check in-scope variables before adding BIND';
# 		my %in_scope	= map { $_ => 1 } $ggp->potentially_bound();
# 		if (exists $in_scope{ $var }) {
# 			die "Syntax error: BIND used with variable already in scope";
# #			throw RDF::Query::Error::QueryPatternError -text => "Syntax error: BIND used with variable already in scope";
# 		}
		my $bind	= Attean::Algebra::Extend->new( children => [$ggp], variable => $var, expression => $expr );
		$self->_add_patterns( $bind );
	} elsif ($class eq 'Attean::Algebra::Service') {
		my ($endpoint, $pattern, $silent)	= @args;
		if ($endpoint->does('Attean::API::Variable')) {
			# SERVICE ?var
			die "SERVICE ?var not implemented";
		} else {
			# SERVICE <endpoint>
			# no-op
			my $service	= Attean::Algebra::Service->new( children => [$pattern], endpoint => $endpoint, silent => $silent );
			$self->_add_patterns( $service );
		}
	} elsif ($class =~ /Attean::Algebra::(Union|Graph|Join)$/) {
		# no-op
	} else {
		die 'Unrecognized GraphPattern: ' . $class;
	}
}

sub _SubSelect_test {
	my $self	= shift;
	return $self->test_token(KEYWORD, 'SELECT');
}

sub _SubSelect {
	my $self	= shift;
	my $pattern;
	{
		local($self->{namespaces})				= $self->{namespaces};
		local($self->{stack})					= [];
		local($self->{filters})					= [];
		local($self->{pattern_container_stack})	= [];
		my $triples								= $self->_push_pattern_container();
		local($self->{build})					= { triples => $triples};
		if ($self->{baseURI}) {
			$self->{build}{base}	= $self->{baseURI};
		}
		
		$self->expected_token(KEYWORD, 'SELECT');
		if ($self->test_token(KEYWORD, 'DISTINCT') or $self->test_token(KEYWORD, 'REDUCED')) {
			my $t	= $self->next_token;
			my $mod	= $t->value;
			$self->{build}{options}{lc($mod)}	= 1;
		}
		
		my ($star, @exprs)	= $self->__SelectVars;
		
		$self->_WhereClause;
		
		$self->_SolutionModifier();
		
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
		
		if ($self->optional_token(KEYWORD, 'VALUES')) {
			my @vars;
			my $parens	= 0;
			if ($self->optional_token(LPAREN)) {
				$parens	= 1;
			}
			while ($self->test_token(VAR)) {
				$self->_Var;
				push( @vars, splice(@{ $self->{stack} }));
			}
			if ($parens) {
				$self->expected_token(RPAREN);
			}
			my $count	= scalar(@vars);
			if (not($parens) and $count == 0) {
				die "Syntax error: Expected VAR in inline data declaration";
			} elsif (not($parens) and $count > 1) {
				die "Syntax error: Inline data declaration can only have one variable when parens are omitted";
			}
			
			my $short	= (not($parens) and $count == 1);
			$self->expected_token(LBRACE);
			if (not($short) or ($short and $self->_Binding_test)) {
				while ($self->_Binding_test) {
					my $terms	= $self->_Binding($count);
					push( @{ $self->{build}{bindings}{terms} }, $terms );
				}
			} else {
				while ($self->_BindingValue_test) {
					$self->_BindingValue;
					my ($term)	= splice(@{ $self->{stack} });
					push( @{ $self->{build}{bindings}{terms} }, [$term] );
				}
			}
			
			$self->expected_token(RBRACE);
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
			push(@{ $self->{build}{triples} }, $self->new_join($pattern, $table));
		}
		
		$self->__solution_modifiers( $star, @exprs );
		
		delete $self->{build}{options};
		my $data	= delete $self->{build};
		$pattern	= $data->{triples}[0];
	}
	
	$self->_add_patterns( $pattern );
}

# [21] TriplesBlock ::= TriplesSameSubject ( '.' TriplesBlock? )?
sub _TriplesBlock_test {
	my $self	= shift;
	# VarOrTerm | TriplesNode -> (Var | GraphTerm) | (Collection | BlankNodePropertyList) -> Var | IRIref | RDFLiteral | NumericLiteral | BooleanLiteral | BlankNode | NIL | Collection | BlankNodePropertyList
	# but since a triple can't start with a literal, this is reduced to:
	# Var | IRIref | BlankNode | NIL
	return 1 if ($self->test_token(VAR));
	return 1 if ($self->test_token(IRI));
	return 1 if ($self->test_token(NIL));
	return 1 if ($self->test_token(ANON));
	return 1 if ($self->test_token(BNODE));
	return 1 if ($self->test_token(STRING1D));
	return 1 if ($self->test_token(STRING3D));
	return 1 if ($self->test_token(STRING1S));
	return 1 if ($self->test_token(STRING3S));
	return 1 if ($self->test_token(PREFIXNAME));
	return 1 if ($self->test_token(LPAREN));
	return 1 if ($self->test_token(LBRACKET));
	
	return 0;
# 	return $self->_test(qr/[\$?]|<|_:|\[[\n\r\t ]*\]|\([\n\r\t ]*\)|\[|[[(]|${r_PNAME_NS}/);
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
	while ($self->test_token(DOT)) {
		if ($got_dot) {
			die "Syntax error: found extra DOT after TriplesBlock";
		}
		$self->expected_token(DOT);
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
	return 1 if ($self->test_token(LBRACE));
	my $t	= $self->peek_token;
	return unless ($t);
	return 0 unless ($t->type == KEYWORD);
	return ($t->value =~ qr/VALUES|BIND|SERVICE|MINUS|OPTIONAL|{|GRAPH/i);
}

sub _GraphPatternNotTriples {
	my $self	= shift;
	if ($self->test_token(KEYWORD, 'VALUES')) {
		$self->_InlineDataClause;
	} elsif ($self->test_token(KEYWORD, 'SERVICE')) {
		$self->_ServiceGraphPattern;
	} elsif ($self->test_token(KEYWORD, 'MINUS')) {
		$self->_MinusGraphPattern;
	} elsif ($self->test_token(KEYWORD, 'BIND')) {
		$self->_Bind;
	} elsif ($self->_OptionalGraphPattern_test) {
		$self->_OptionalGraphPattern;
	} elsif ($self->_GroupOrUnionGraphPattern_test) {
		$self->_GroupOrUnionGraphPattern;
	} else {
		$self->_GraphGraphPattern;
	}
}

sub _InlineDataClause {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'VALUES');
	my @vars;
	
	my $parens	= 0;
	if ($self->optional_token(LPAREN)) {
		$parens	= 1;
	}
	while ($self->test_token(VAR)) {
		$self->_Var;
		push( @vars, splice(@{ $self->{stack} }));
	}
	if ($parens) {
		$self->expected_token(RPAREN);
	}
	
	my $count	= scalar(@vars);
	if (not($parens) and $count == 0) {
		die "Syntax error: Expected VAR in inline data declaration";
	} elsif (not($parens) and $count > 1) {
		die "Syntax error: Inline data declaration can only have one variable when parens are omitted";
	}
	
	my $short	= (not($parens) and $count == 1);
	$self->expected_token(LBRACE);
	my @rows;
	if (not($short) or ($short and $self->_Binding_test)) {
		# { (term) (term) }
		while ($self->_Binding_test) {
			my $terms	= $self->_Binding($count);
			push( @rows, $terms );
		}
	} else {
		# { term term }
		while ($self->_BindingValue_test) {
			$self->_BindingValue;
			my ($term)	= splice(@{ $self->{stack} });
			push( @rows, [$term] );
		}
	}
	
	$self->expected_token(RBRACE);
	
	my @vbs		= map { my %d; @d{ map { $_->value } @vars } = @$_; Attean::Result->new(bindings => \%d) } @rows;
	my $table	= Attean::Algebra::Table->new( variables => \@vars, rows => \@vbs );
	$self->_add_stack( ['Attean::Algebra::Table', $table] );
	
}

sub _Bind {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'BIND');
	my ($var, $expr)	= $self->_BrackettedAliasExpression;
	$self->_add_stack( ['Attean::Algebra::Extend', $var, $expr] );
}

sub _ServiceGraphPattern {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'SERVICE');
	my $silent	= $self->optional_token(KEYWORD, 'SILENT');
	$self->__close_bgp_with_filters;
	if ($self->test_token(VAR)) {
		$self->_Var;
	} else {
		$self->_IRIref;
	}
	my ($endpoint)	= splice( @{ $self->{stack} } );
	$self->_GroupGraphPattern;
	my $ggp	= $self->_remove_pattern;
	
	my $opt		= ['Attean::Algebra::Service', $endpoint, $ggp, ($silent ? 1 : 0)];
	$self->_add_stack( $opt );
}

# [23] OptionalGraphPattern ::= 'OPTIONAL' GroupGraphPattern
sub _OptionalGraphPattern_test {
	my $self	= shift;
	return $self->test_token(KEYWORD, 'OPTIONAL');
}

sub __close_bgp_with_filters {
	my $self	= shift;
	my @filters		= splice(@{ $self->{filters} });
	if (@filters) {
		my $cont	= $self->_pop_pattern_container;
		my $ggp		= $self->new_join(@$cont);
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
	$self->expected_token(KEYWORD, 'OPTIONAL');
	$self->__close_bgp_with_filters;
	
	$self->_GroupGraphPattern;
	my $ggp	= $self->_remove_pattern;
	my $opt		= ['Attean::Algebra::LeftJoin', $ggp];
	$self->_add_stack( $opt );
}

sub _MinusGraphPattern {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'MINUS');
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
			die "Syntax error: Nested named GRAPH blocks not allowed in data template.";
		}
	}
	
	$self->expected_token(KEYWORD, 'GRAPH');
	$self->_VarOrIRIref;
	my ($graph)	= splice(@{ $self->{stack} });
	if ($graph->does('Attean::API::IRI')) {
		local($self->{named_graph})	= $graph;
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
sub _GroupOrUnionGraphPattern_test {
	my $self	= shift;
	return $self->test_token(LBRACE);
}

sub _GroupOrUnionGraphPattern {
	my $self	= shift;
	$self->_GroupGraphPattern;
	my $ggp	= $self->_remove_pattern;
	if ($self->test_token(KEYWORD, 'UNION')) {
		while ($self->optional_token(KEYWORD, 'UNION')) {
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
	$self->expected_token(KEYWORD, 'FILTER');
	$self->_Constraint;
	my ($expr) = splice(@{ $self->{stack} });
	$self->_add_filter( $expr );
}

# [27] Constraint ::= BrackettedExpression | BuiltInCall | FunctionCall
sub _Constraint_test {
	my $self	= shift;
	return 1 if ($self->test_token(LPAREN));
	return 1 if $self->_BuiltInCall_test;
	return 1 if $self->_FunctionCall_test;
	return 0;
}

sub _Constraint {
	my $self	= shift;
	if ($self->_BrackettedExpression_test) {
		$self->_BrackettedExpression();
	} elsif ($self->_BuiltInCall_test) {
		$self->_BuiltInCall();
	} else {
		$self->_FunctionCall();
	}
}

# [28] FunctionCall ::= IRIref ArgList
sub _FunctionCall_test {
	my $self	= shift;
	return $self->_IRIref_test;
}

sub _FunctionCall {
	my $self	= shift;
	$self->_IRIref;
	my ($iri)	= splice(@{ $self->{stack} });
	
	$self->_ArgList;
	my @args	= splice(@{ $self->{stack} });

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
	return 1 if $self->test_token(NIL);
	return $self->test_token(LPAREN);
}

sub _ArgList {
	my $self	= shift;
	if ($self->optional_token(NIL)) {
		return;
	} else {
		$self->expected_token(LPAREN);
		my @args;
		unless ($self->test_token(RPAREN)) {
			$self->_Expression;
			push( @args, splice(@{ $self->{stack} }) );
			while ($self->optional_token(COMMA)) {
				$self->_Expression;
				push( @args, splice(@{ $self->{stack} }) );
			}
		}
		$self->expected_token(RPAREN);
		$self->_add_stack( @args );
	}
}

# [30] ConstructTemplate ::= '{' ConstructTriples? '}'
sub _ConstructTemplate {
	my $self	= shift;
	$self->_push_pattern_container;
	$self->expected_token(LBRACE);
	
	if ($self->_ConstructTriples_test) {
		$self->_ConstructTriples;
	}

	$self->expected_token(RBRACE);
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
	while ($self->optional_token(DOT)) {
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
		my ($s)	= splice(@{ $self->{stack} });
		$self->_PropertyList;
		my @list	= splice(@{ $self->{stack} });
		foreach my $data (@list) {
			push(@triples, $self->__new_statement( $s, @$data ));
		}
	} else {
		$self->_VarOrTerm;
		my ($s)	= splice(@{ $self->{stack} });

		$self->_PropertyListNotEmpty;
		my (@list)	= splice(@{ $self->{stack} });
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
		my ($s)	= splice(@{ $self->{stack} });
		$self->_PropertyListPath;
		my @list	= splice(@{ $self->{stack} });
		foreach my $data (@list) {
			push(@triples, $self->__new_statement( $s, @$data ));
		}
	} else {
		$self->_VarOrTerm;
		my ($s)	= splice(@{ $self->{stack} });
		$self->_PropertyListNotEmptyPath;
		my (@list)	= splice(@{ $self->{stack} });
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
	my ($v)	= splice(@{ $self->{stack} });
	$self->_ObjectList;
	my @l	= splice(@{ $self->{stack} });
	my @props		= map { [$v, $_] } @l;
	while ($self->optional_token(SEMICOLON)) {
		if ($self->_Verb_test) {
			$self->_Verb;
			my ($v)	= splice(@{ $self->{stack} });
			$self->_ObjectList;
			my @l	= splice(@{ $self->{stack} });
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
	my ($v)	= splice(@{ $self->{stack} });
	$self->_ObjectList;
	my @l	= splice(@{ $self->{stack} });
	my @props		= map { [$v, $_] } @l;
	while ($self->optional_token(SEMICOLON)) {
		if ($self->_VerbPath_test or $self->_VerbSimple_test) {
			if ($self->_VerbPath_test) {
				$self->_VerbPath;
			} else {
				$self->_VerbSimple;
			}
			my ($v)	= splice(@{ $self->{stack} });
			$self->_ObjectList;
			my @l	= splice(@{ $self->{stack} });
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
	push(@list, splice(@{ $self->{stack} }));
	
	while ($self->optional_token(COMMA)) {
		$self->_Object;
		push(@list, splice(@{ $self->{stack} }));
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
	return 1 if ($self->test_token(A));
	return 1 if ($self->test_token(VAR));
	return 1 if ($self->test_token(IRI));
	return 1 if ($self->test_token(PREFIXNAME));
	return 0;
# 	return $self->_test( qr/a[\n\t\r <]|[?\$]|<|${r_PNAME_LN}|${r_PNAME_NS}/ );
}

sub _Verb {
	my $self	= shift;
	if ($self->optional_token(A)) {
		my $type	= Attean::IRI->new(value =>  $rdf->type->uri_value);
		$self->_add_stack( $type );
	} else {
		$self->_VarOrIRIref;
	}
}

# VerbSimple ::= Var
sub _VerbSimple_test {
	my $self	= shift;
	return ($self->test_token(VAR));
}

sub _VerbSimple {
	my $self	= shift;
	$self->_Var;
}

# VerbPath ::= Path
sub _VerbPath_test {
	my $self	= shift;
	return 1 if ($self->test_token(IRI));
	return 1 if ($self->test_token(PREFIXNAME));
	return 1 if ($self->test_token(HAT));
	return 1 if ($self->test_token(OR));
	return 1 if ($self->test_token(BANG));
	return 1 if ($self->test_token(LPAREN));
	return 1 if ($self->test_token(A));
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
	while ($self->optional_token(OR)) {
		my ($lhs)	= splice(@{ $self->{stack} });
#		$self->_PathOneInPropertyClass;
		$self->_PathSequence;
		my ($rhs)	= splice(@{ $self->{stack} });
		$self->_add_stack( ['PATH', '|', $lhs, $rhs] );
	}
}

# [76]  	PathSequence	  ::=  	PathEltOrInverse ( '/' PathEltOrInverse | '^' PathElt )*
sub _PathSequence {
	my $self	= shift;
	$self->_PathEltOrInverse;
	while ($self->test_token(SLASH) or $self->test_token(HAT)) {
		my $op;
		my ($lhs)	= splice(@{ $self->{stack} });
		if ($self->optional_token(SLASH)) {
			$op	= '/';
			$self->_PathEltOrInverse;
		} else {
			$op	= '^';
			$self->expected_token(HAT);
			$self->_PathElt;
		}
		my ($rhs)	= splice(@{ $self->{stack} });
		$self->_add_stack( ['PATH', $op, $lhs, $rhs] );
	}
}

# [77]  	PathElt	  ::=  	PathPrimary PathMod?
sub _PathElt {
	my $self	= shift;
	$self->_PathPrimary;
#	$self->__consume_ws_opt;
	if ($self->_PathMod_test) {
		my @path	= splice(@{ $self->{stack} });
		$self->_PathMod;
		my ($mod)	= splice(@{ $self->{stack} });
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
	if ($self->optional_token(HAT)) {
		$self->_PathElt;
		my @props	= splice(@{ $self->{stack} });
		$self->_add_stack( [ 'PATH', '^', @props ] );
	} else {
		$self->_PathElt;
	}
}

# [79]  	PathMod	  ::=  	( '*' | '?' | '+' | '{' ( Integer ( ',' ( '}' | Integer '}' ) | '}' ) ) )
sub _PathMod_test {
	my $self	= shift;
	return 1 if ($self->test_token(STAR));
	return 1 if ($self->test_token(QUESTION));
	return 1 if ($self->test_token(PLUS));
	return 1 if ($self->test_token(LBRACE));
	return 0;
# 	return 1 if ($self->_test(qr/[*?+{]/));
}

sub _PathMod {
	my $self	= shift;
	if ($self->test_token(STAR) or $self->test_token(QUESTION) or $self->test_token(PLUS)) {
		my $t	= $self->next_token;
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
# 			throw RDF::Query::Error::ParseError -text => "Syntax error: Empty Path Modifier";
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
	} elsif ($self->optional_token(A)) {
		my $type	= Attean::IRI->new(value =>  $rdf->type->uri_value);
		$self->_add_stack( $type );
	} elsif ($self->optional_token(BANG)) {
		$self->_PathNegatedPropertyClass;
		my (@path)	= splice(@{ $self->{stack} });
		$self->_add_stack( ['PATH', '!', @path] );
	} else {
		$self->expected_token(LPAREN);
		$self->_Path;
		$self->expected_token(RPAREN);
	}
}

# [81]  	PathNegatedPropertyClass	  ::=  	( PathOneInPropertyClass | '(' ( PathOneInPropertyClass ( '|' PathOneInPropertyClass )* )? ')' )
sub _PathNegatedPropertyClass {
	my $self	= shift;
	if ($self->optional_token(LPAREN)) {
		
		my @nodes;
		if ($self->_PathOneInPropertyClass_test) {
			$self->_PathOneInPropertyClass;
			push(@nodes, splice(@{ $self->{stack} }));
			while ($self->optional_token(OR)) {
				$self->_PathOneInPropertyClass;
				push(@nodes, splice(@{ $self->{stack} }));
#				$self->_add_stack( ['PATH', '|', $lhs, $rhs] );
			}
		}
		$self->expected_token(RPAREN);
		$self->_add_stack( @nodes );
	} else {
		$self->_PathOneInPropertyClass;
	}
}

# [82]  	PathOneInPropertyClass	  ::=  	IRIref | 'a'
sub _PathOneInPropertyClass_test {
	my $self	= shift;
	return 1 if $self->_IRIref_test;
	return 1 if ($self->test_token(A));
	return 1 if ($self->test_token(HAT));
	return 0;
}

sub _PathOneInPropertyClass {
	my $self	= shift;
	my $rev		= 0;
	if ($self->optional_token(HAT)) {
		$rev	= 1;
	}
	if ($self->optional_token(A)) {
		my $type	= Attean::IRI->new(value =>  $rdf->type->uri_value);
		if ($rev) {
			$self->_add_stack( [ 'PATH', '^', $type ] );
		} else {
			$self->_add_stack( $type );
		}
	} else {
		$self->_IRIref;
		if ($rev) {
			my ($path)	= splice(@{ $self->{stack} });
			$self->_add_stack( [ 'PATH', '^', $path ] );
		}
	}
}

################################################################################

# [38] TriplesNode ::= Collection | BlankNodePropertyList
sub _TriplesNode_test {
	my $self	= shift;
	return 1 if $self->test_token(LPAREN);
	return 1 if $self->test_token(LBRACKET);
# 	return 1 if $self->test_token(ANON);
# 	return 1 if $self->test_token(NIL);
	return 0;
# 	return $self->_test(qr/[[(](?![\n\r\t ]*\])(?![\n\r\t ]*\))/);
}

sub _TriplesNode {
	my $self	= shift;
	if ($self->test_token(LPAREN)) {
		$self->_Collection;
	} else {
		$self->_BlankNodePropertyList;
	}
}

# [39] BlankNodePropertyList ::= '[' PropertyListNotEmpty ']'
sub _BlankNodePropertyList {
	my $self	= shift;
	if (my $where = $self->{__no_bnodes}) {
		die "Syntax error: Blank nodes not allowed in $where";
	}
	$self->expected_token(LBRACKET);
#	$self->_PropertyListNotEmpty;
	$self->_PropertyListNotEmptyPath;
	$self->expected_token(RBRACKET);
	
	my @props	= splice(@{ $self->{stack} });
	my $subj	= Attean::Blank->new();
	my @triples	= map { $self->__new_statement( $subj, @$_ ) } @props;
	$self->_add_patterns( @triples );
	$self->_add_stack( $subj );
}

# [40] Collection ::= '(' GraphNode+ ')'
sub _Collection {
	my $self	= shift;
	$self->expected_token(LPAREN);
	$self->_GraphNode;
	my @nodes;
	push(@nodes, splice(@{ $self->{stack} }));
	
	while ($self->_GraphNode_test) {
		$self->_GraphNode;
		push(@nodes, splice(@{ $self->{stack} }));
	}
	
	$self->expected_token(RPAREN);
	
	my $subj	= Attean::Blank->new();
	my $cur		= $subj;
	my $last;

	my $first	= Attean::IRI->new(value =>  $rdf->first->uri_value);
	my $rest	= Attean::IRI->new(value =>  $rdf->rest->uri_value);
	my $nil		= Attean::IRI->new(value =>  $rdf->nil->uri_value);

	
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
	return $self->_test(qr/[\$?]|<|['"]|(true\b|false\b)|([+-]?\d)|_:|${r_ANON}|${r_NIL}|\[|[[(]/);
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
sub _VarOrTerm_test {
	my $self	= shift;
	return 1 if ($self->peek_token(VAR));
	return 1 if ($self->_IRIref_test);
	return 1 if ($self->peek_token(BOOLEAN));
	return 1 if ($self->peek_token(STRING1D));
	return 1 if ($self->peek_token(STRING1S));
	return 1 if ($self->peek_token(STRING3D));
	return 1 if ($self->peek_token(STRING3S));
	return 1 if ($self->peek_token(INTEGER));
	return 1 if ($self->peek_token(DECIMAL));
	return 1 if ($self->peek_token(DOUBLE));
	return 1 if ($self->peek_token(BNODE));
	return 1 if ($self->peek_token(NIL));
	return 0;
# 	return 1 if ($self->_test(qr/[<'".0-9]|(true|false)\b|_:|\([\n\r\t ]*\)/));
}

sub _VarOrTerm {
	my $self	= shift;
	if ($self->test_token(VAR)) {
		$self->_Var;
	} else {
		$self->_GraphTerm;
	}
}

# [43] VarOrIRIref ::= Var | IRIref
sub _VarOrIRIref_test {
	my $self	= shift;
	return 1 if ($self->test_token(IRI));
	return 1 if ($self->test_token(VAR));
	return 1 if ($self->test_token(PREFIXNAME));
	return 0;
# 	return $self->_test(qr/[\$?]|<|${r_PNAME_LN}|${r_PNAME_NS}/);
}

sub _VarOrIRIref {
	my $self	= shift;
	if ($self->test_token(VAR)) {
		$self->_Var;
	} else {
		$self->_IRIref;
	}
}

# [44] Var ::= VAR1 | VAR2
sub _Var {
	my $self	= shift;
	if ($self->{__data_pattern}) {
		die "Syntax error: Variable found where Term expected";
	}

	my $var		= $self->expected_token(VAR);
	$self->_add_stack( Attean::Variable->new( $var->value ) );
}

# [45] GraphTerm ::= IRIref | RDFLiteral | NumericLiteral | BooleanLiteral | BlankNode | NIL
sub _GraphTerm {
	my $self	= shift;
	if ($self->test_token(BOOLEAN)) {
		$self->_BooleanLiteral;
	} elsif ($self->test_token(LPAREN)) {
		$self->_NIL;
	} elsif ($self->test_token(ANON) or $self->test_token(BNODE)) {
		$self->_BlankNode;
	} elsif ($self->test_token(INTEGER) or $self->test_token(DECIMAL) or $self->test_token(DOUBLE) or $self->test_token(MINUS) or $self->test_token(PLUS)) {
		$self->_NumericLiteral;
	} elsif ($self->test_token(STRING1S) or $self->test_token(STRING1D) or $self->test_token(STRING1D) or $self->test_token(STRING3D)) {
		$self->_RDFLiteral;
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
	push(@list, splice(@{ $self->{stack} }));
	
	while ($self->test_token(OROR)) {
		$self->expected_token(OROR);
		$self->_ConditionalAndExpression;
		push(@list, splice(@{ $self->{stack} }));
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
	Carp::confess $self->{tokens} if (scalar(@{ $self->{stack} }) == 0);
}

# [48] ConditionalAndExpression ::= ValueLogical ( '&&' ValueLogical )*
sub _ConditionalAndExpression {
	my $self	= shift;
	$self->_ValueLogical;
	my @list	= splice(@{ $self->{stack} });
	
	while ($self->test_token(ANDAND)) {
		$self->expected_token(ANDAND);
		$self->_ValueLogical;
		push(@list, splice(@{ $self->{stack} }));
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
	
	my $t		= $self->peek_token;
	my $type	= $t->type;
	if ($type == EQUALS or $type == NOTEQUALS or $type == LE or $type == GE or $type == LT or $type == GT) {
		$self->next_token;
		my @list	= splice(@{ $self->{stack} });
		my $op	= $t->value;
		$self->_NumericExpression;
		push(@list, splice(@{ $self->{stack} }));
		$self->_add_stack( $self->new_binary_expression( $op, @list ) );
	} elsif ($self->test_token(KEYWORD, 'NOT') or $self->test_token(KEYWORD, 'IN')) {
		my @list	= splice(@{ $self->{stack} });
		my $not		= $self->optional_token(KEYWORD, 'NOT');
		$self->expected_token(KEYWORD, 'IN');
		my $op		= $not ? 'NOTIN' : 'IN';
		$self->_ExpressionList();
		push(@list, splice(@{ $self->{stack} }));
		my $p	= $self->new_function_expression( $op, @list );
		$self->_add_stack($p);
	}
}

sub _ExpressionList {
	my $self	= shift;
	if ($self->optional_token(NIL)) {
		return;
	} else {
		$self->expected_token(LPAREN);
		my @args;
		unless ($self->test_token(RPAREN)) {
			$self->_Expression;
			push( @args, splice(@{ $self->{stack} }) );
			while ($self->optional_token(COMMA)) {
				$self->_Expression;
				push( @args, splice(@{ $self->{stack} }) );
			}
		}
		$self->expected_token(RPAREN);
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
	my ($expr)	= splice(@{ $self->{stack} });
	
	while ($self->test_token(MINUS) or $self->test_token(PLUS)) {
		my $t	= $self->next_token;
		my $op	= ($t->type == MINUS) ? '-' : '+';
		$self->_MultiplicativeExpression;
		my ($rhs)	= splice(@{ $self->{stack} });
		$expr	= $self->new_binary_expression( $op, $expr, $rhs );
	}
	$self->_add_stack( $expr );
}

# [53] MultiplicativeExpression ::= UnaryExpression ( '*' UnaryExpression | '/' UnaryExpression )*
sub _MultiplicativeExpression {
	my $self	= shift;
	$self->_UnaryExpression;
	my ($expr)	= splice(@{ $self->{stack} });
	
	while ($self->test_token(STAR) or $self->test_token(SLASH)) {
		my $t	= $self->next_token;
		my $op	= ($t->type == STAR) ? '*' : '/';
		$self->_UnaryExpression;
		my ($rhs)	= splice(@{ $self->{stack} });
		$expr	= $self->new_binary_expression( $op, $expr, $rhs );
	}
	$self->_add_stack( $expr );
}

# [54] UnaryExpression ::= '!' PrimaryExpression  | '+' PrimaryExpression  | '-' PrimaryExpression  | PrimaryExpression
sub _UnaryExpression {
	my $self	= shift;
	if ($self->optional_token(BANG)) {
		$self->_PrimaryExpression;
		my ($expr)	= splice(@{ $self->{stack} });
		my $not		= Attean::UnaryExpression->new( operator => '!', children => [$expr] );
		$self->_add_stack( $not );
	} elsif ($self->optional_token(PLUS)) {
		$self->_PrimaryExpression;
		my ($expr)	= splice(@{ $self->{stack} });
		
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
	} elsif ($self->optional_token(MINUS)) {
		$self->_PrimaryExpression;
		my ($expr)	= splice(@{ $self->{stack} });
		
		### if it's just a literal, force the negative down into the literal instead of make an unnecessary multiplication.
		if (blessed($expr) and $expr->isa('Attean::ValueExpression') and $expr->value->does('Attean::API::NumericLiteral')) {
			my $value	= -1 * $expr->value->value;
			my $l		= Attean::Literal->new( value => $value, datatype => $expr->value->datatype );
			my $lexpr	= Attean::ValueExpression->new( value => $l );
			$self->_add_stack( $lexpr );
		} else {
			my $int		= $xsd->integer->uri_value;
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
	my $t	= $self->peek_token;
	warn 'primary expression: ' . $t->value;
	if ($self->_BrackettedExpression_test) {
		$self->_BrackettedExpression;
	} elsif ($self->_BuiltInCall_test) {
		$self->_BuiltInCall;
	} elsif ($self->_IRIref_test) {
		$self->_IRIrefOrFunction;
		my $v		= pop(@{ $self->{stack} });
		if ($v->does('Attean::API::IRI')) {
			$v	= Attean::ValueExpression->new(value => $v);
		}
		$self->_add_stack($v);
	} elsif ($self->test_token(VAR)) {
		$self->_Var;
		my $var		= pop(@{ $self->{stack} });
		my $expr	= Attean::ValueExpression->new(value => $var);
		$self->_add_stack($expr);
	} elsif ($self->test_token(BOOLEAN)) {
		$self->_BooleanLiteral;
		my $b		= pop(@{ $self->{stack} });
		my $expr	= Attean::ValueExpression->new(value => $b);
		$self->_add_stack($expr);
	} elsif ($self->test_token(INTEGER) or $self->test_token(DECIMAL) or $self->test_token(DOUBLE) or $self->test_token(PLUS) or $self->test_token(MINUS)) {
		$self->_NumericLiteral;
		my $l		= pop(@{ $self->{stack} });
		my $expr	= Attean::ValueExpression->new(value => $l);
		$self->_add_stack($expr);
	} else {	# if ($self->_test(qr/['"]/)) {
		$self->_RDFLiteral;
		my $value	= pop(@{ $self->{stack} });
		my $expr	= Attean::ValueExpression->new(value => $value);
		$self->_add_stack($expr);
	}
}

# [56] BrackettedExpression ::= '(' Expression ')'
sub _BrackettedExpression_test {
	my $self	= shift;
	return $self->test_token(LPAREN);
}

sub _BrackettedExpression {
	my $self	= shift;
	$self->expected_token(LPAREN);
	$self->_Expression;
	$self->expected_token(RPAREN);
}

sub _Aggregate {
	my $self	= shift;
	my $t		= $self->expected_token(KEYWORD);
	my $op		= $t->value;
	$self->expected_token(LPAREN);
	my $distinct	= 0;
	if ($self->optional_token(KEYWORD, 'DISTINCT')) {
		$distinct	= 1;
	}
	
	my $star	= 0;
	my (@expr, %options);
	if ($self->optional_token(STAR)) {
		$star	= 1;
	} else {
		$self->_Expression;
		push(@expr, splice(@{ $self->{stack} }));
		if ($op eq 'GROUP_CONCAT') {
			while ($self->optional_token(COMMA)) {
				$self->_Expression;
				push(@expr, splice(@{ $self->{stack} }));
			}
			if ($self->optional_token(SEMICOLON)) {
				$self->expected_token(KEYWORD, 'SEPARATOR');
				$self->expected_token(EQUALS);
				$self->_String;
				my ($sep)	= splice(@{ $self->{stack} });
				$options{ seperator }	= $sep;
			}
		}
	}
	my $arg	= join(',', map { blessed($_) ? $_->as_sparql : $_ } @expr);
	if ($distinct) {
		$arg	= 'DISTINCT ' . $arg;
	}
	my $name	= sprintf('%s(%s)', $op, $arg);
	$self->expected_token(RPAREN);
	
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
	my $t		= $self->peek_token;
	return unless ($t);
	if ($self->{__aggregate_call_ok}) {
		return 1 if ($t->type == KEYWORD and $t->value =~ qr/^(MIN|MAX|COUNT|AVG|SUM|SAMPLE|GROUP_CONCAT)$/io);
	}
	return 1 if ($self->test_token(KEYWORD, 'NOT'));
	return 1 if ($self->test_token(KEYWORD, 'EXISTS'));
	return 1 if ($t->type == KEYWORD and $t->value =~ qr/^(ABS|CEIL|FLOOR|ROUND|CONCAT|SUBSTR|STRLEN|UCASE|LCASE|ENCODE_FOR_URI|CONTAINS|STRSTARTS|STRENDS|RAND|MD5|SHA1|SHA224|SHA256|SHA384|SHA512|HOURS|MINUTES|SECONDS|DAY|MONTH|YEAR|TIMEZONE|TZ|NOW)$/i);
	return ($t->type == KEYWORD and $t->value =~ qr/^(COALESCE|UUID|STRUUID|STR|STRDT|STRLANG|STRBEFORE|STRAFTER|REPLACE|BNODE|IRI|URI|LANG|LANGMATCHES|DATATYPE|BOUND|sameTerm|isIRI|isURI|isBLANK|isLITERAL|REGEX|IF|isNumeric)$/i);
}

sub _BuiltInCall {
	my $self	= shift;
	my $t		= $self->peek_token;
	if ($self->{__aggregate_call_ok} and $t->type == KEYWORD and $t->value =~ qr/(MIN|MAX|COUNT|AVG|SUM|SAMPLE|GROUP_CONCAT)\b/io) {
		$self->_Aggregate;
	} elsif ($self->optional_token(KEYWORD, 'NOT')) {
		$self->expected_token(KEYWORD, 'EXISTS');
		local($self->{filters})					= [];
		$self->_GroupGraphPattern;
		my $cont	= $self->_remove_pattern;
		my $p		= Attean::ExistsExpression->new( pattern => $cont );
		$p	= Attean::UnaryExpression->new( operator => '!', children => [$p] );
		$self->_add_stack($p);
	} elsif ($self->optional_token(KEYWORD, 'EXISTS')) {
		local($self->{filters})					= [];
		$self->_GroupGraphPattern;
		my $cont	= $self->_remove_pattern;
		my $p		= Attean::ExistsExpression->new( pattern => $cont );
		$self->_add_stack($p);
	} elsif ($t->type == KEYWORD and $t->value =~ qr/(COALESCE|BNODE|CONCAT|SUBSTR|RAND|NOW)/i) {
		# n-arg functions that take expressions
		my $t	= $self->next_token;
		my $op	= $t->value;
		$self->_ArgList;
		my @args	= splice(@{ $self->{stack} });
		my $func	= $self->new_function_expression( $op, @args );
		$self->_add_stack( $func );
	} elsif ($self->_RegexExpression_test) {
		$self->_RegexExpression;
	} else {
		my $t		= $self->next_token;
		my $op		= $t->value;
		if ($op =~ /^(STR)?UUID$/i) {
			# no-arg functions
			$self->expected_token(NIL);
			$self->_add_stack( $self->new_function_expression($op) );
		} elsif ($op =~ /^(STR|URI|IRI|LANG|DATATYPE|isIRI|isURI|isBLANK|isLITERAL|isNumeric|ABS|CEIL|FLOOR|ROUND|STRLEN|UCASE|LCASE|ENCODE_FOR_URI|MD5|SHA1|SHA224|SHA256|SHA384|SHA512|HOURS|MINUTES|SECONDS|DAY|MONTH|YEAR|TIMEZONE|TZ)$/i) {
			### one-arg functions that take an expression
			$self->expected_token(LPAREN);
			$self->_Expression;
			my ($expr)	= splice(@{ $self->{stack} });
			$self->_add_stack( $self->new_function_expression($op, $expr) );
			$self->expected_token(RPAREN);
		} elsif ($op =~ /^(STRDT|STRLANG|LANGMATCHES|sameTerm|CONTAINS|STRSTARTS|STRENDS|STRBEFORE|STRAFTER)$/i) {
			### two-arg functions that take expressions
			$self->expected_token(LPAREN);
			$self->_Expression;
			my ($arg1)	= splice(@{ $self->{stack} });
			$self->expected_token(COMMA);
			$self->_Expression;
			my ($arg2)	= splice(@{ $self->{stack} });
			$self->_add_stack( $self->new_function_expression($op, $arg1, $arg2) );
			$self->expected_token(RPAREN);
		} elsif ($op =~ /^(IF|REPLACE)$/i) {
			### three-arg functions that take expressions
			$self->expected_token(LPAREN);
			$self->_Expression;
			my ($arg1)	= splice(@{ $self->{stack} });
			$self->expected_token(COMMA);
			$self->_Expression;
			my ($arg2)	= splice(@{ $self->{stack} });
			$self->expected_token(COMMA);
			$self->_Expression;
			my ($arg3)	= splice(@{ $self->{stack} });
			$self->_add_stack( $self->new_function_expression($op, $arg1, $arg2, $arg3) );
			$self->expected_token(RPAREN);
		} else {
			### BOUND(Var)
			$self->expected_token(LPAREN);
			$self->_Var;
			my $var		= pop(@{ $self->{stack} });
			my $expr	= Attean::ValueExpression->new(value => $var);
			$self->_add_stack( $self->new_function_expression($op, $expr) );
			$self->expected_token(RPAREN);
		}
	}
}

# [58] RegexExpression ::= 'REGEX' '(' Expression ',' Expression ( ',' Expression )? ')'
sub _RegexExpression_test {
	my $self	= shift;
	return $self->test_token(KEYWORD, 'REGEX');
}

sub _RegexExpression {
	my $self	= shift;
	$self->expected_token(KEYWORD, 'REGEX');
	$self->expected_token(LPAREN);
	$self->_Expression;
	my $string	= splice(@{ $self->{stack} });
	
	$self->expected_token(COMMA);
	$self->_Expression;
	my $pattern	= splice(@{ $self->{stack} });
	
	my @args	= ($string, $pattern);
	if ($self->optional_token(COMMA)) {
		$self->_Expression;
		push(@args, splice(@{ $self->{stack} }));
	}
	
	$self->expected_token(RPAREN);
	
	$self->_add_stack( $self->new_function_expression( 'REGEX', @args ) );
}

# [59] IRIrefOrFunction ::= IRIref ArgList?
sub _IRIrefOrFunction_test {
	my $self	= shift;
	$self->_IRIref_test;
}

sub _IRIrefOrFunction {
	my $self	= shift;
	$self->_IRIref;
	if ($self->_ArgList_test) {
		my ($iri)	= splice(@{ $self->{stack} });
		$self->_ArgList;
		my @args	= splice(@{ $self->{stack} });
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
	$self->_String;
	my ($value)	= splice(@{ $self->{stack} });
	
	my $obj;
	if ($self->test_token(LANG)) {
		my $t	= $self->expected_token(LANG);
		my $lang	= $t->value;
		$obj	= Attean::Literal->new( value => $value, language => $lang );
	} elsif ($self->test_token(HATHAT)) {
		$self->expected_token(HATHAT);
		$self->_IRIref;
		my ($iri)	= splice(@{ $self->{stack} });
		$obj	= Attean::Literal->new( value => $value, datatype => $iri );
	} else {
		$obj	= Attean::Literal->new( value => $value );
	}
	
	$self->_add_stack( $obj );
}

# [61] NumericLiteral ::= NumericLiteralUnsigned | NumericLiteralPositive | NumericLiteralNegative
# [62] NumericLiteralUnsigned ::= INTEGER | DECIMAL | DOUBLE
# [63] NumericLiteralPositive ::= INTEGER_POSITIVE | DECIMAL_POSITIVE | DOUBLE_POSITIVE
# [64] NumericLiteralNegative ::= INTEGER_NEGATIVE | DECIMAL_NEGATIVE | DOUBLE_NEGATIVE
sub _NumericLiteral {
	my $self	= shift;
	my $sign	= 0;
	if ($self->optional_token(PLUS)) {
		$sign	= '+';
	} elsif ($self->optional_token(MINUS)) {
		$sign	= '-';
	}
	
	my $value;
	my $type;
	if (my $db = $self->optional_token(DOUBLE)) {
		$value	= $db->value;
		$type	= Attean::IRI->new(value =>  $xsd->double->uri_value );
	} elsif (my $dc = $self->optional_token(DECIMAL)) {
		$value	= $dc->value;
		$type	= Attean::IRI->new(value =>  $xsd->decimal->uri_value );
	} else {
		my $i	= $self->expected_token(INTEGER);
		$value	= $i->value;
		$type	= Attean::IRI->new(value =>  $xsd->integer->uri_value );
	}
	
	if ($sign) {
		$value	= $sign . $value;
	}
	
	my $obj	= Attean::Literal->new( value => $value, datatype => $type );
# 	if ($self->{args}{canonicalize} and blessed($obj) and $obj->isa('RDF::Trine::Node::Literal')) {
# 		$obj	= $obj->canonicalize;
# 	}
	$self->_add_stack( $obj );
}

# [65] BooleanLiteral ::= 'true' | 'false'
sub _BooleanLiteral {
	my $self	= shift;
	my $t		= $self->expected_token(BOOLEAN);
	my $bool	= $t->value;

	my $obj	= Attean::Literal->new( value => $bool, datatype => $xsd->boolean->uri_value );
# 	if ($self->{args}{canonicalize} and blessed($obj) and $obj->isa('RDF::Trine::Node::Literal')) {
# 		$obj	= $obj->canonicalize;
# 	}
	$self->_add_stack( $obj );
}

# [66] String ::= STRING_LITERAL1 | STRING_LITERAL2 | STRING_LITERAL_LONG1 | STRING_LITERAL_LONG2
sub _String {
	my $self	= shift;
	my $value;
	my $string;
	my $t	= $self->peek_token;
	if ($string = $self->optional_token(STRING1D)) {
		$value	= $string->value;
	} elsif ($string = $self->optional_token(STRING1S)) {
		$value	= $string->value;
	} elsif ($string = $self->optional_token(STRING3S)) {
		$value	= $string->value;
	} elsif ($string = $self->optional_token(STRING3D)) {
		$value	= $string->value;
	} else {
		my $got	= AtteanX::Parser::SPARQL::Constants::decrypt_constant($t->type);
		my $value	= $t->value;
		die "Expecting string literal but found $got '$value'";
	}

#	$value	=~ s/(${r_ECHAR})/"$1"/ge;
	$value	=~ s/\\t/\t/g;
	$value	=~ s/\\b/\n/g;
	$value	=~ s/\\n/\n/g;
	$value	=~ s/\\r/\x08/g;
	$value	=~ s/\\"/"/g;
	$value	=~ s/\\'/'/g;
	$value	=~ s/\\\\/\\/g;	# backslash must come last, so it doesn't accidentally create a new escape
	$self->_add_stack( $value );
}

# [67] IRIref ::= IRI_REF | PrefixedName
sub _IRIref_test {
	my $self	= shift;
	return 1 if ($self->test_token(IRI));
	return 1 if ($self->test_token(PREFIXNAME));
	return 0;
}

sub _IRIref {
	my $self	= shift;
	if (my $t = $self->optional_token(IRI)) {
		my $iri	= $t->value;
		my $base	= $self->__base;
		my $node	= Attean::IRI->new( value => $iri, $base ? (base => $base) : () );
		$self->_add_stack( $node );
	} else {
		$self->_PrefixedName;
	}
}

# [68] PrefixedName ::= PNAME_LN | PNAME_NS
sub _PrefixedName {
	my $self	= shift;
	my $t		= $self->expected_token(PREFIXNAME);
	my ($ns, $local)	= @{ $t->args };
	chop($ns);
# 		$local	=~ s{\\([-~.!&'()*+,;=:/?#@%_\$])}{$1}g;
	
	unless (exists $self->{namespaces}{$ns}) {
		die "Syntax error: Use of undefined namespace '$ns'";
	}
	
	my $iri		= $self->{namespaces}{$ns} . $local;
	my $base	= $self->__base;
	$self->_add_stack( Attean::IRI->new( value => $iri, $base ? (base => $base) : () ) );
}

# [69] BlankNode ::= BLANK_NODE_LABEL | ANON
sub _BlankNode {
	my $self	= shift;
	if (my $where = $self->{__no_bnodes}) {
		die "Syntax error: Blank nodes not allowed in $where";
	}
	if (my $b = $self->optional_token(BNODE)) {
		my $label	= $b->value;
		$self->_add_stack( Attean::Blank->new($label) );
	} else {
		$self->expected_token(ANON);
		$self->_add_stack( Attean::Blank->new() );
	}
}

sub _NIL {
	my $self	= shift;
	$self->expected_token(NIL);
	my $nil	= Attean::IRI->new(value =>  $rdf->nil->uri_value );
	$self->_add_stack( $nil );
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
	
	
	
	
	my $having_expr;
	my $aggdata	= delete( $self->{build}{__aggregate} );
	my @aggkeys	= keys %{ $aggdata || {} };
	if (scalar(@aggkeys)) {
		my @aggs;
		foreach my $k (@aggkeys) {
			my ($var, $expr)	= @{ $aggdata->{$k} };
			push(@aggs, $expr);
		}
		
		my $groupby	= delete( $self->{build}{__group_by} ) || [];
		my $pattern	= $self->{build}{triples};
		my $ggp		= shift(@$pattern);
		if (my $having = delete( $self->{build}{__having} )) {
			$having_expr	= $having;
		}
		
		my $agg		= Attean::Algebra::Group->new( children => [$ggp], groupby => $groupby, aggregates => \@aggs );
		push(@{ $self->{build}{triples} }, $agg);
	}
	
	my @project;
	my @vars;
	my @extend;
	for (my $i = 0; $i < $#exprs; $i += 2) {
		my $k	= $exprs[$i];
		my $v	= $exprs[$i+1];
		push(@project, $k);
		if ($v->does('Attean::API::Variable')) {
			push(@vars, $v);
		} else {
			push(@extend, $k, $v);
		}
	}

	{
# 		my @vars	= grep { $_->isa('RDF::Query::Expression::Alias') } @$vars;
# 		if (scalar(@vars)) {
# 			my $pattern	= pop(@{ $self->{build}{triples} });
# 			my @bound	= $pattern->potentially_bound;
# 			my %bound	= map { $_ => 1 } @bound;
# 			foreach my $v (@vars) {
# 				my $name	= $v->name;
# 				if ($bound{ $name }) {
# 					throw RDF::Query::Error::ParseError -text => "Syntax error: Already-bound variable ($name) used in project expression";
# 				}
# 			}
# 			
# 			my $proj	= RDF::Query::Algebra::Extend->new( $pattern, $vars );
# 			push(@{ $self->{build}{triples} }, $proj);
# 		}
		
		my $pattern	= pop(@{ $self->{build}{triples} });
		while (my($name, $expr) = splice(@extend, 0, 2)) {
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
	
	if ($self->{build}{options}{distinct}) {
		delete $self->{build}{options}{distinct};
		my $pattern	= pop(@{ $self->{build}{triples} });
		my $sort	= Attean::Algebra::Distinct->new( children => [$pattern] );
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
}

################################################################################

=item C<< error >>

Returns the error encountered during the last parse.

=cut

sub _add_patterns {
	my $self	= shift;
	my @triples	= @_;
	my $container	= $self->{ pattern_container_stack }[0];
	push( @{ $container }, @triples );
}

sub _remove_pattern {
	my $self	= shift;
	my $container	= $self->{ pattern_container_stack }[0];
	my $pattern		= pop( @{ $container } );
	return $pattern;
}

sub _peek_pattern {
	my $self	= shift;
	my $container	= $self->{ pattern_container_stack }[0];
	my $pattern		= $container->[-1];
	return $pattern;
}

sub _push_pattern_container {
	my $self	= shift;
	my $cont	= [];
	unshift( @{ $self->{ pattern_container_stack } }, $cont );
	return $cont;
}

sub _pop_pattern_container {
	my $self	= shift;
	my $cont	= shift( @{ $self->{ pattern_container_stack } } );
	return $cont;
}

sub _add_stack {
	my $self	= shift;
	my @items	= @_;
	push( @{ $self->{stack} }, @items );
}

sub _add_filter {
	my $self	= shift;
	my @filters	= shift;
	push( @{ $self->{filters} }, @filters );
}

# sub _eat_with_ws {
# 	my $self	= shift;
# 	my $r		= $self->_eat(@_);
# 	$self->__consume_ws_opt;
# 	return $r;
# }
# 
# sub _eat {
# 	my $self	= shift;
# 	my $thing	= shift;
# 	if (not(length($self->{tokens}))) {
# 		$self->_syntax_error("No tokens left");
# 	}
# 
# # 	if (substr($self->{tokens}, 0, 1) eq '^') {
# # 		Carp::cluck( "eating $thing with input $self->{tokens}" );
# # 	}
# 
# 	if (ref($thing) and $thing->isa('Regexp')) {
# 		if ($self->{tokens} =~ /^($thing)/) {
# 			my $match	= $1;
# 			substr($self->{tokens}, 0, length($match))	= '';
# 			return $match;
# 		}
# 
# 		$self->_syntax_error( "Expected $thing" );
# 	} elsif (looks_like_number( $thing )) {
# 		my ($token)	= substr( $self->{tokens}, 0, $thing, '' );
# 		return $token
# 	} else {
# 		### thing is a string
# 		if (substr($self->{tokens}, 0, length($thing)) eq $thing) {
# 			substr($self->{tokens}, 0, length($thing))	= '';
# 			return $thing;
# 		} else {
# 			$self->_syntax_error( "Expected $thing" );
# 		}
# 	}
# 	print $thing;
# 	die;
# }

sub _syntax_error {
	my $self	= shift;
	my $thing	= shift;
	my $expect	= $thing;

	my $level	= 2;
	while (my $sub = (caller($level++))[3]) {
		if ($sub =~ m/::_([A-Z]\w*)$/) {
			$expect	= $1;
			last;
		}
	}

	my $l		= Log::Log4perl->get_logger("rdf.query.parser.sparql");
	if ($l->is_debug) {
		$l->logcluck("Syntax error eating $thing with input <<$self->{tokens}>>");
	}

	my $near	= "'" . substr($self->{tokens}, 0, 20) . "...'";
	$near		=~ s/[\r\n ]+/ /g;
	if ($thing) {
# 		Carp::cluck Dumper($self->{tokens});	# XXX
		die "Syntax error: $thing in $expect near $near";
	} else {
		die "Syntax error: Expected $expect near $near";
	}
}

# sub _test {
# 	my $self	= shift;
# 	my $thing	= shift;
# 	if (blessed($thing) and $thing->isa('Regexp')) {
# 		if ($self->{tokens} =~ m/^$thing/) {
# 			return 1;
# 		} else {
# 			return 0;
# 		}
# 	} else {
# 		if (substr($self->{tokens}, 0, length($thing)) eq $thing) {
# 			return 1;
# 		} else {
# 			return 0;
# 		}
# 	}
# }
# 
# sub _ws_test {
# 	my $self	= shift;
# 	unless (length($self->{tokens})) {
# 		return 0;
# 	}
# 
# 	if ($self->{tokens} =~ m/^[\t\r\n #]/) {
# 		return 1;
# 	} else {
# 		return 0;
# 	}
# }
# 
# sub _ws {
# 	my $self	= shift;
# 	### #x9 | #xA | #xD | #x20 | comment
# 	if ($self->_test('#')) {
# 		$self->_eat(qr/#[^\x0d\x0a]*.?/);
# 	} else {
# 		$self->_eat(qr/[\n\r\t ]/);
# 	}
# }
# 
# sub __consume_ws_opt {
# 	my $self	= shift;
# 	if ($self->_ws_test) {
# 		$self->__consume_ws;
# 	}
# }
# 
# sub __consume_ws {
# 	my $self	= shift;
# 	$self->_ws;
# 	while ($self->_ws_test()) {
# 		$self->_ws()
# 	}
# }

sub __base {
	my $self	= shift;
	my $build	= $self->{build};
	if (blessed($build->{base})) {
		return $build->{base};
	} elsif (defined($build->{base})) {
		return Attean::IRI->new($build->{base});
	} else {
		return;
	}
}

sub __new_statement {
	my $self	= shift;
	my @nodes	= @_;
	if ($self->{_modify_template} and my $graph = $self->{named_graph} and $self->{named_graph}->does('Attean::API::IRI')) {
		return Attean::QuadPattern->new(@nodes, $graph);
	} else {
		return Attean::TriplePattern->new(@nodes);
	}
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
		Carp::confess "Path $op: " . Dumper(\@nodes);
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
	if (scalar(@patterns) > scalar(@paths) + scalar(@triples)) {
		Carp::cluck "more than just triples and paths passed to __new_bgp: " . Dumper(\@patterns);
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
			return Attean::Algebra::Join->new( children => [$bgp, @p] );
		} else {
			return $self->new_join(@p);
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

sub new_join {
	my $self	= shift;
	my @parts	= @_;
	unless (scalar(@parts)) {
		return Attean::Algebra::BGP->new();
	}
	
	if (scalar(@parts) == 1) {
		return shift(@parts);
	}
	
	return Attean::Algebra::Join->new( children => \@parts );
}

sub peek_token {
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

sub test_token {
	my $self	= shift;
	my $type	= shift;
	my $t		= $self->peek_token;
	return unless ($t);
	return if ($t->type != $type);
	if (@_) {
		my $value	= shift;
		return unless ($t->value eq $value);
	}
	return 1;
}

sub optional_token {
	my $self	= shift;
	if ($self->test_token(@_)) {
		return $self->next_token;
	}
	return;
}

sub next_token {
	my $self	= shift;
	my $l		= $self->lexer;
	my $t		= $l->next;
	while ($t->type == COMMENT) {
		$t		= $l->peek;
		return unless ($t);
	}
	return $t;
}

sub expected_token {
	my $self	= shift;
	my $type	= shift;
	if ($self->test_token($type, @_)) {
		return $self->next_token;
	} else {
		my $t			= $self->peek_token;
		my $expecting	= AtteanX::Parser::SPARQL::Constants::decrypt_constant($type);
		my $got			= AtteanX::Parser::SPARQL::Constants::decrypt_constant($t->type);
		if (@_) {
			my $value	= shift;
			my $value2	= $t->value;
			Carp::confess "Expecting $expecting '$value' but got $got '$value2' before " . $self->lexer->buffer;
		} else {
			Carp::confess "Expecting $expecting but found $got before " . $self->lexer->buffer;
		}
	}
}

1;

__END__

=back

=cut
