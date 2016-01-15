#!/usr/bin/env perl

use v5.14;
use autodie;
use utf8;
use Test::Modern;
use Test::Exception;
use Digest::SHA qw(sha1_hex);
use AtteanX::SPARQL::Constants;

use Attean;
use Attean::RDF;

subtest 'expected tokens: empty BGP tokens' => sub {
	my $a	= Attean::Algebra::BGP->new(triples => []);
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	expect_token_stream($i, []);
	ws_is($a->as_sparql, '');
};

subtest 'expected tokens: 1-triple BGP tokens' => sub {
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $a	= Attean::Algebra::BGP->new(triples => [$t]);
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	expect_token_stream($i, [IRI, IRI, STRING1D, DOT]);
	ws_is($a->as_sparql, '<s> <p> "1" .');
};

subtest 'expected tokens: 2-BGP join tokens' => sub {
	my $t	= triple(variable('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $a	= Attean::Algebra::Join->new( children => [$bgp, $bgp] );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# { ?s <p> "1" . ?s <p> "1" . }
	expect_token_stream($i, [LBRACE, VAR, IRI, STRING1D, DOT, VAR, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, '{ ?s <p> "1" . ?s <p> "1" . }');
};

subtest 'expected tokens: distinct/bgp' => sub {
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $a	= Attean::Algebra::Distinct->new( children => [$bgp] );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# SELECT DISTINCT * WHERE { <s> <p> "1" }
	expect_token_stream($i, [KEYWORD, KEYWORD, STAR, KEYWORD, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, 'SELECT DISTINCT * WHERE { <s> <p> "1" . }');
};

subtest 'expected tokens: reduced/bgp' => sub {
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $a	= Attean::Algebra::Reduced->new( children => [$bgp] );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# SELECT REDUCED * WHERE { <s> <p> "1" }
	expect_token_stream($i, [KEYWORD, KEYWORD, STAR, KEYWORD, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, 'SELECT REDUCED * WHERE { <s> <p> "1" . }');
};

subtest 'expected tokens: bgp/limit' => sub {
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $a	= Attean::Algebra::Slice->new( children => [$bgp], limit => 5 );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# SELECT * WHERE { <s> <p> "1" } LIMIT 5
	expect_token_stream($i, [KEYWORD, STAR, KEYWORD, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE, KEYWORD, INTEGER]);
	ws_is($a->as_sparql, 'SELECT * WHERE { <s> <p> "1" . } LIMIT 5');
};

subtest 'expected tokens: bgp/slice' => sub {
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $a	= Attean::Algebra::Slice->new( children => [$bgp], limit => 5, offset => 5 );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# SELECT * WHERE { <s> <p> "1" } LIMIT 5 OFFSET 5
	expect_token_stream($i, [KEYWORD, STAR, KEYWORD, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE, KEYWORD, INTEGER, KEYWORD, INTEGER]);
	ws_is($a->as_sparql, 'SELECT * WHERE { <s> <p> "1" . } LIMIT 5 OFFSET 5');
};

subtest 'expected tokens: distinct/bgp/slice' => sub {
	my $t		= triple(iri('s'), iri('p'), literal('1'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
	my $dist	= Attean::Algebra::Distinct->new( children => [$bgp] );
	my $a		= Attean::Algebra::Slice->new( children => [$dist], limit => 5, offset => 5 );
	my $i		= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# SELECT DISTINCT * WHERE { <s> <p> "1" } LIMIT 5 OFFSET 5
	expect_token_stream($i, [KEYWORD, KEYWORD, STAR, KEYWORD, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE, KEYWORD, INTEGER, KEYWORD, INTEGER]);
	ws_is($a->as_sparql, 'SELECT DISTINCT * WHERE { <s> <p> "1" . } LIMIT 5 OFFSET 5');
};

subtest 'property paths' => sub {
	subtest 'expected tokens: predicate path' => sub {
		my $p1	= iri('p1');
		my $a	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
		my $i		= $a->sparql_tokens;
		expect_token_stream($i, [IRI]);
		
		subtest 'predicate path' => sub {
			my $p	= Attean::Algebra::Path->new( path => $a, subject => iri('s'), object => iri('o') );
			ws_is($p->as_sparql, '<s> <p1> <o>');
		}
	};
	
	subtest 'expected tokens: nps path' => sub {
		my $p1	= iri('p1');
		my $p2	= iri('p2');
		my $a	= Attean::Algebra::NegatedPropertySet->new( predicates => [$p1, $p2] );
		my $i		= $a->sparql_tokens;
		# !(<p1>|<p2>)
		expect_token_stream($i, [BANG, LPAREN, IRI, OR, IRI, RPAREN]);
		
		subtest 'nps path' => sub {
			my $a	= Attean::Algebra::Path->new( path => $a, subject => iri('s'), object => iri('o') );
			ws_is($a->as_sparql, '<s> !(<p1>|<p2>) <o>');
		}
	};
	
	subtest 'expected tokens: 1-IRI sequence path' => sub {
		my $p2	= iri('p2');
		my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );
		my $a	= Attean::Algebra::SequencePath->new( children => [$pp2] );
		my $i	= $a->sparql_tokens;
		expect_token_stream($i, [IRI]);
		
		subtest 'sequence path' => sub {
			my $p	= Attean::Algebra::Path->new( path => $a, subject => iri('s'), object => iri('o') );
			ws_is($p->as_sparql, '<s> <p2> <o>');
		}
	};
	
	subtest 'expected tokens: 2-IRI sequence path' => sub {
		my $p1	= iri('p1');
		my $p2	= iri('p2');
		my $pp1	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
		my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );
		my $a	= Attean::Algebra::SequencePath->new( children => [$pp1, $pp2] );
		my $i	= $a->sparql_tokens;
		expect_token_stream($i, [IRI, SLASH, IRI]);
		
		my $p	= Attean::Algebra::Path->new( path => $a, subject => iri('s'), object => iri('o') );
		ws_is($p->as_sparql, '<s> <p1>/<p2> <o>');
	};
	
	subtest 'expected tokens: 1-IRI alternative path' => sub {
		my $p2	= iri('p2');
		my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );
		my $a	= Attean::Algebra::AlternativePath->new( children => [$pp2] );
		my $i	= $a->sparql_tokens;
		expect_token_stream($i, [IRI]);
		
		subtest 'alternative path' => sub {
			my $p	= Attean::Algebra::Path->new( path => $a, subject => iri('s'), object => iri('o') );
			ws_is($p->as_sparql, '<s> <p2> <o>');
		}
	};
	
	subtest 'expected tokens: 2-IRI alternative path' => sub {
		my $p1	= iri('p1');
		my $p2	= iri('p2');
		my $pp1	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
		my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );
		my $a	= Attean::Algebra::AlternativePath->new( children => [$pp1, $pp2] );
		my $i	= $a->sparql_tokens;
		# <p1>|<p2>
		expect_token_stream($i, [IRI, OR, IRI]);
		
		subtest 'alternative path' => sub {
			my $p	= Attean::Algebra::Path->new( path => $a, subject => iri('s'), object => iri('o') );
			ws_is($p->as_sparql, '<s> <p1>|<p2> <o>');
		}
	};
	
	subtest 'expected tokens: 1-IRI inverse path' => sub {
		my $p2	= iri('p2');
		my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );
		my $a	= Attean::Algebra::InversePath->new( children => [$pp2] );
		my $i	= $a->sparql_tokens;
		# ^<p1>
		expect_token_stream($i, [HAT, IRI]);
		
		subtest 'inverse path' => sub {
			my $p	= Attean::Algebra::Path->new( path => $a, subject => iri('s'), object => iri('o') );
			ws_is($p->as_sparql, '<s> ^<p2> <o>');
		}
	};
	
	subtest 'expected tokens: 2-IRI inverse path' => sub {
		my $p1	= iri('p1');
		my $p2	= iri('p2');
		my $pp1	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
		my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );
		my $seq	= Attean::Algebra::AlternativePath->new( children => [$pp1, $pp2] );
		my $a	= Attean::Algebra::InversePath->new( children => [$seq] );
		my $i	= $a->sparql_tokens;
		# ^(<p1>|<p2>)
		expect_token_stream($i, [HAT, LPAREN, IRI, OR, IRI, RPAREN]);
		
		subtest 'inverse path' => sub {
			my $p	= Attean::Algebra::Path->new( path => $a, subject => iri('s'), object => iri('o') );
			ws_is($p->as_sparql, '<s> ^(<p1>|<p2>) <o>');
		}
	};
	
	subtest 'expected tokens: zero or more 2-IRI inverse path' => sub {
		my $p1	= iri('p1');
		my $p2	= iri('p2');
		my $pp1	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
		my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );
		my $seq	= Attean::Algebra::AlternativePath->new( children => [$pp1, $pp2] );
		my $inv	= Attean::Algebra::InversePath->new( children => [$seq] );
		my $a	= Attean::Algebra::ZeroOrMorePath->new( children => [$inv] );
		my $i	= $a->sparql_tokens;
		# (^(<p1>/<p2>))*
		expect_token_stream($i, [LPAREN, HAT, LPAREN, IRI, OR, IRI, RPAREN, RPAREN, STAR]);

		subtest '* path' => sub {
			my $p	= Attean::Algebra::Path->new( path => $a, subject => iri('s'), object => iri('o') );
			ws_is($p->as_sparql, '<s> (^(<p1>|<p2>))* <o>');
		}
	};
	
	subtest 'expected tokens: one or more 2-IRI inverse path' => sub {
		my $p1	= iri('p1');
		my $p2	= iri('p2');
		my $pp1	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
		my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );
		my $seq	= Attean::Algebra::AlternativePath->new( children => [$pp1, $pp2] );
		my $inv	= Attean::Algebra::InversePath->new( children => [$seq] );
		my $a	= Attean::Algebra::OneOrMorePath->new( children => [$inv] );
		my $i	= $a->sparql_tokens;
		# (^(<p1>/<p2>))+
		expect_token_stream($i, [LPAREN, HAT, LPAREN, IRI, OR, IRI, RPAREN, RPAREN, PLUS]);

		subtest '+ path' => sub {
			my $p	= Attean::Algebra::Path->new( path => $a, subject => iri('s'), object => iri('o') );
			ws_is($p->as_sparql, '<s> (^(<p1>|<p2>))+ <o>');
		}
	};
	
	subtest 'expected tokens: zero or one 2-IRI inverse path' => sub {
		my $p1	= iri('p1');
		my $p2	= iri('p2');
		my $pp1	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
		my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );
		my $seq	= Attean::Algebra::AlternativePath->new( children => [$pp1, $pp2] );
		my $inv	= Attean::Algebra::InversePath->new( children => [$seq] );
		my $a	= Attean::Algebra::ZeroOrOnePath->new( children => [$inv] );
		my $i	= $a->sparql_tokens;
		# (^(<p1>/<p2>))+
		expect_token_stream($i, [LPAREN, HAT, LPAREN, IRI, OR, IRI, RPAREN, RPAREN, QUESTION]);

		subtest '? path' => sub {
			my $p	= Attean::Algebra::Path->new( path => $a, subject => iri('s'), object => iri('o') );
			ws_is($p->as_sparql, '<s> (^(<p1>|<p2>))? <o>');
		}
	};

	subtest 'expected tokens: 2-IRI sequence path triple' => sub {
		my $p1	= iri('p1');
		my $p2	= iri('p2');
		my $pp1	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
		my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );
		my $seq	= Attean::Algebra::SequencePath->new( children => [$pp1, $pp2] );
		my $a	= Attean::Algebra::Path->new( path => $seq, subject => iri('s'), object => iri('o') );
		my $i	= $a->sparql_tokens;
		expect_token_stream($i, [IRI, IRI, SLASH, IRI, IRI]);
		ws_is($a->as_sparql, '<s> <p1>/<p2> <o>');
	};
};

subtest 'expected tokens: named graph tokens' => sub {
	my $bgp	= Attean::Algebra::BGP->new(triples => [triple(iri('s'), iri('p'), literal('1'))]);
	my $g	= iri('graphname');
	my $a	= Attean::Algebra::Graph->new( children => [$bgp], graph => $g );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	# GRAPH <graphname> { <s> <p> "1" . }
	expect_token_stream($i, [KEYWORD, IRI, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, 'GRAPH <graphname> { <s> <p> "1" . }');
};

subtest 'expected tokens: service tokens' => sub {
	my $bgp	= Attean::Algebra::BGP->new(triples => [triple(iri('s'), iri('p'), literal('1'))]);
	my $g	= iri('http://example.org/sparql');
	my $a	= Attean::Algebra::Service->new( children => [$bgp], endpoint => $g );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	# SERVICE <http://example.org/sparql> { <s> <p> "1" . }
	expect_token_stream($i, [KEYWORD, IRI, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, 'SERVICE <http://example.org/sparql> { <s> <p> "1" . }');
};

subtest 'expected tokens: union tokens' => sub {
	my $lhs	= Attean::Algebra::BGP->new(triples => [triple(iri('s'), iri('p'), literal('1'))]);
	my $rhs	= Attean::Algebra::BGP->new(triples => [triple(iri('s'), iri('p'), literal('2'))]);
	my $a	= Attean::Algebra::Union->new( children => [$lhs, $rhs] );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	expect_token_stream($i, [LBRACE, IRI, IRI, STRING1D, DOT, RBRACE, KEYWORD, LBRACE, IRI, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, '{ <s> <p> "1" . } UNION { <s> <p> "2" . }');
};

subtest 'expected tokens: minus tokens' => sub {
	my $lhs	= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('1'))]);
	my $rhs	= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('2'))]);
	my $a	= Attean::Algebra::Minus->new( children => [$lhs, $rhs] );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	# { ?s <p> "1" . } MINUS { ?s <p> "2" . }
	expect_token_stream($i, [LBRACE, VAR, IRI, STRING1D, DOT, RBRACE, KEYWORD, LBRACE, VAR, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, '{ ?s <p> "1" . } MINUS { ?s <p> "2" . }');
};

subtest 'expected tokens: optional tokens' => sub {
	my $lhs	= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('1'))]);
	my $rhs	= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('2'))]);
	my $a	= Attean::Algebra::LeftJoin->new( children => [$lhs, $rhs] );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	# { ?s <p> "1" . } OPTIONAL { ?s <p> "1" . }
	expect_token_stream($i, [LBRACE, VAR, IRI, STRING1D, DOT, RBRACE, KEYWORD, LBRACE, VAR, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, '{ ?s <p> "1" . } OPTIONAL { ?s <p> "2" . }');
};

subtest 'expected tokens: table tokens' => sub {
	my @rows;
	push(@rows, Attean::Result->new( bindings => { 's' => iri('http://example.org/') }));
	push(@rows, Attean::Result->new( bindings => { 's' => literal('sparql') }));
	my $a	= Attean::Algebra::Table->new(variables => [variable('s')], rows => \@rows);
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# VALUES (?s) { (<http://example.org>) ("sparql") }
	expect_token_stream($i, [KEYWORD, LPAREN, VAR, RPAREN, LBRACE, LPAREN, IRI, RPAREN, LPAREN, STRING1D, RPAREN, RBRACE]);
	ws_is($a->as_sparql, 'VALUES (?s) { (<http://example.org/>) ("sparql") }');
};

subtest 'expected tokens: optional+filter tokens' => sub {
	my $lhs	= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('1'))]);
	my $rhs	= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('2'))]);
	my $e		= Attean::ValueExpression->new( value => variable('s') );
	my $expr	= Attean::FunctionExpression->new( operator => 'ISIRI', children => [$e] );
	my $a	= Attean::Algebra::LeftJoin->new( children => [$lhs, $rhs], expression => $expr );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	# { ?s <p> "1" . } OPTIONAL { ?s <p> "1" . FILTER(ISIRI(?s)) }
	expect_token_stream($i, [LBRACE, VAR, IRI, STRING1D, DOT, RBRACE, KEYWORD, LBRACE, VAR, IRI, STRING1D, DOT, KEYWORD, LPAREN, KEYWORD, LPAREN, VAR, RPAREN, RPAREN, RBRACE]);
	ws_is($a->as_sparql, '{ ?s <p> "1" . } OPTIONAL { ?s <p> "2" . FILTER(ISIRI(?s)) }');
};

subtest 'expected tokens: project' => sub {
	my $bgp		= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('1'))]);
	my $a		= Attean::Algebra::Project->new( children => [$bgp], variables => [variable('p')] );
	my $i		= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# SELECT ?p WHERE { ?s <p> "1" . }
	expect_token_stream($i, [KEYWORD, VAR, KEYWORD, LBRACE, VAR, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, 'SELECT ?p WHERE { ?s <p> "1" . }');
};

subtest 'expected tokens: comparator tokens' => sub {
	my $bgp		= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('1'))]);
	my $expr	= Attean::ValueExpression->new( value => variable('s') );
	my $a		= Attean::Algebra::Comparator->new(ascending => 0, expression => $expr);
	my $i		= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# DESC(?s)
	expect_token_stream($i, [KEYWORD, LPAREN, VAR, RPAREN]);
};

subtest 'expected tokens: comparator tokens' => sub {
	my $bgp		= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('1'))]);
	my $expr	= Attean::ValueExpression->new( value => variable('s') );
	my $cmp		= Attean::Algebra::Comparator->new(ascending => 0, expression => $expr);
	my $a		= Attean::Algebra::OrderBy->new( children => [$bgp], comparators => [$cmp] );
	my $i		= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# SELECT * WHERE { ?s <p> "1" . } ORDER BY DESC(?s)
	expect_token_stream($i, [KEYWORD, STAR, KEYWORD, LBRACE, VAR, IRI, STRING1D, DOT, RBRACE, KEYWORD, KEYWORD, KEYWORD, LPAREN, VAR, RPAREN]);
};

subtest 'expected tokens: ASK tokens' => sub {
	my $bgp		= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('1'))]);
	my $a		= Attean::Algebra::Ask->new( children => [$bgp] );
	my $i		= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# ASK { ?s <p> "1" . }
	expect_token_stream($i, [KEYWORD, LBRACE, VAR, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, 'ASK { ?s <p> "1" . }');
};

subtest 'expected tokens: CONSTRUCT tokens' => sub {
	my $bgp	= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('1'))]);
	my $t	= triple(variable('s'), iri('q'), literal('2'));
	my $a	= Attean::Algebra::Construct->new( children => [$bgp], triples => [$t] );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# CONSTRUCT { ?s <q> "2" } WHERE { ?s <p> "1" . }
	expect_token_stream($i, [KEYWORD, LBRACE, VAR, IRI, STRING1D, DOT, RBRACE, KEYWORD, LBRACE, VAR, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, 'CONSTRUCT { ?s <q> "2" . } WHERE { ?s <p> "1" . }');
};

subtest 'expected tokens: DESCRIBE tokens' => sub {
	my $bgp	= Attean::Algebra::BGP->new(triples => [triple(variable('s'), iri('p'), literal('1'))]);
	my $a	= Attean::Algebra::Describe->new( children => [$bgp], terms => [variable('s'), iri('q')] );
	my $i	= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	
	# DESCRIBE ?s <q> WHERE { ?s <p> "1" . }
	expect_token_stream($i, [KEYWORD, VAR, IRI, KEYWORD, LBRACE, VAR, IRI, STRING1D, DOT, RBRACE]);
	ws_is($a->as_sparql, 'DESCRIBE ?s <q> WHERE { ?s <p> "1" . }');
};

subtest 'expected tokens: project expressions tokens' => sub {
	my $t1		= triple(variable('s'), iri('p'), variable('o1'));
	my $t2		= triple(variable('s'), iri('q'), variable('o2'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t1, $t2]);
	my $e1		= Attean::ValueExpression->new( value => variable('o1') );
	my $e2		= Attean::ValueExpression->new( value => variable('o2') );
	my $expr	= Attean::BinaryExpression->new( operator => '+', children => [$e1, $e2] );
	my $extend	= Attean::Algebra::Extend->new(children => [$bgp], variable => variable('sum'), expression => $expr);
	subtest 'project ordering 1' => sub {
		my $a		= Attean::Algebra::Project->new( children => [$extend], variables => [variable('s'), variable('sum')] );
		my $i		= $a->sparql_tokens;
		does_ok($i, 'Attean::API::Iterator');
		# SELECT ?s (?o1 + ?o2 AS ?sum) WHERE { ?s <p> ?o1 . ?s <q> ?o2 . }
		expect_token_stream($i, [KEYWORD, VAR, LPAREN, VAR, PLUS, VAR, KEYWORD, VAR, RPAREN, KEYWORD, LBRACE, VAR, IRI, VAR, DOT, VAR, IRI, VAR, DOT, RBRACE]);
		ws_is($a->as_sparql, 'SELECT ?s (?o1 + ?o2 AS ?sum) WHERE { ?s <p> ?o1 . ?s <q> ?o2 . }');
	};
	subtest 'project ordering 2' => sub {
		my $a		= Attean::Algebra::Project->new( children => [$extend], variables => [variable('sum'), variable('s')] );
		my $i		= $a->sparql_tokens;
		does_ok($i, 'Attean::API::Iterator');
		# SELECT (?o1 + ?o2 AS ?sum) ?s WHERE { ?s <p> ?o1 . ?s <q> ?o2 . }
		expect_token_stream($i, [KEYWORD, LPAREN, VAR, PLUS, VAR, KEYWORD, VAR, RPAREN, VAR, KEYWORD, LBRACE, VAR, IRI, VAR, DOT, VAR, IRI, VAR, DOT, RBRACE]);
		ws_is($a->as_sparql, 'SELECT (?o1 + ?o2 AS ?sum) ?s WHERE { ?s <p> ?o1 . ?s <q> ?o2 . }');
	};
};

subtest 'expected tokens: binary filter tokens' => sub {
	my $t1		= triple(variable('s'), iri('p'), variable('o1'));
	my $t2		= triple(variable('s'), iri('q'), variable('o2'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t1, $t2]);
	my $e1		= Attean::ValueExpression->new( value => variable('o1') );
	my $e2		= Attean::ValueExpression->new( value => variable('o2') );
	my $expr	= Attean::BinaryExpression->new( operator => '>', children => [$e1, $e2] );
	my $a		= Attean::Algebra::Filter->new(children => [$bgp], expression => $expr);
	my $i		= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	# ?s <p> ?o1 . ?s <q> ?o2 . FILTER(?o1 > ?o2)
	expect_token_stream($i, [VAR, IRI, VAR, DOT, VAR, IRI, VAR, DOT, KEYWORD, LPAREN, VAR, GT, VAR, RPAREN]);
};

subtest 'expected tokens: function filter tokens' => sub {
	my $t		= triple(variable('s'), iri('p'), variable('o'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
	my $e		= Attean::ValueExpression->new( value => variable('o') );
	my $expr	= Attean::FunctionExpression->new( operator => 'ISIRI', children => [$e] );
	my $a		= Attean::Algebra::Filter->new(children => [$bgp], expression => $expr);
	my $i		= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	# ?s <p> ?o . FILTER(ISIRI(?o))
	expect_token_stream($i, [VAR, IRI, VAR, DOT, KEYWORD, LPAREN, KEYWORD, LPAREN, VAR, RPAREN, RPAREN]);
};

subtest 'expected tokens: cast filter tokens' => sub {
	my $t		= triple(variable('s'), iri('p'), variable('o'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
	my $e		= Attean::ValueExpression->new( value => variable('o') );
	my $expr	= Attean::CastExpression->new( datatype => iri('http://www.w3.org/2001/XMLSchema#integer'), children => [$e] );
	my $a		= Attean::Algebra::Filter->new(children => [$bgp], expression => $expr);
	my $i		= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	# ?s <p> ?o . FILTER(<http://www.w3.org/2001/XMLSchema#integer>(?o))
	expect_token_stream($i, [VAR, IRI, VAR, DOT, KEYWORD, LPAREN, IRI, LPAREN, VAR, RPAREN, RPAREN]);
};

subtest 'expected tokens: exists filter tokens' => sub {
	my $t		= triple(variable('s'), iri('p'), variable('o'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
	my $u		= triple(variable('s'), iri('q'), literal('1'));
	my $expr	= Attean::ExistsExpression->new( pattern => Attean::Algebra::BGP->new(triples => [$u]) );
	my $a		= Attean::Algebra::Filter->new(children => [$bgp], expression => $expr);
	my $i		= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	# ?s <p> ?o . FILTER( EXISTS { ?s <q> "1" } )
	expect_token_stream($i, [VAR, IRI, VAR, DOT, KEYWORD, LPAREN, KEYWORD, LBRACE, VAR, IRI, STRING1D, DOT, RBRACE, RPAREN]);
};

subtest 'expected tokens: non-projected extend tokens' => sub {
	my $t1		= triple(variable('s'), iri('p'), variable('o1'));
	my $t2		= triple(variable('s'), iri('q'), variable('o2'));
	my $bgp1	= Attean::Algebra::BGP->new(triples => [$t1, $t2]);
	my $e1		= Attean::ValueExpression->new( value => variable('o1') );
	my $e2		= Attean::ValueExpression->new( value => variable('o2') );
	my $expr	= Attean::BinaryExpression->new( operator => '+', children => [$e1, $e2] );
	my $extend	= Attean::Algebra::Extend->new(children => [$bgp1], variable => variable('sum'), expression => $expr);
	subtest 'bare extend' => sub {
		my $a		= $extend;
		my $i		= $a->sparql_tokens;
		does_ok($i, 'Attean::API::Iterator');
		# ?s <p> ?o1 . ?s <q> ?o2 . BIND(?o1 + ?o2 AS ?sum)
		expect_token_stream($i, [VAR, IRI, VAR, DOT, VAR, IRI, VAR, DOT, KEYWORD, LPAREN, VAR, PLUS, VAR, KEYWORD, VAR, RPAREN]);
	};
	
	subtest 'extend within projection' => sub {
		my $t3		= triple(variable('s'), iri('r'), variable('o3'));
		my $bgp2		= Attean::Algebra::BGP->new(triples => [$t3]);

		my $join	= Attean::Algebra::Join->new( children => [$extend, $bgp2] );
		my $a		= Attean::Algebra::Project->new( children => [$join], variables => [variable('s'), variable('o3'), variable('sum')] );

		my $i		= $a->sparql_tokens;
		does_ok($i, 'Attean::API::Iterator');
		# SELECT ?s ?o3 ?sum WHERE { ?s <p> ?o1 . ?s <q> ?o2 . BIND(?o1 + ?o2 AS ?sum) ?s <r> ?o3 }
		expect_token_stream($i, [KEYWORD, VAR, VAR, VAR, KEYWORD, LBRACE, VAR, IRI, VAR, DOT, VAR, IRI, VAR, DOT, KEYWORD, LPAREN, VAR, PLUS, VAR, KEYWORD, VAR, RPAREN, VAR, IRI, VAR, DOT, RBRACE]);
	};
};

subtest 'expected tokens: aggregation' => sub {
	my $t		= triple(variable('s'), iri('p'), variable('o'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
	my @groups	= Attean::ValueExpression->new( value => variable('s') );
	my @aggs	= Attean::AggregateExpression->new(
		distinct	=> 0,
		operator	=> 'SUM',
		children	=> [Attean::ValueExpression->new( value => variable('o') )],
		scalar_vars	=> {},
		variable	=> variable("sum"),
	);
	my $a		= Attean::Algebra::Group->new(
		children => [$bgp],
		groupby => \@groups,
		aggregates => \@aggs,
	);
	my $i		= $a->sparql_tokens;
	does_ok($i, 'Attean::API::Iterator');
	# SELECT (SUM(?o) AS ?sum) WHERE { ?s <p> ?o . } GROUP BY ?s
	expect_token_stream($i, [KEYWORD, LPAREN, KEYWORD, LPAREN, VAR, RPAREN, KEYWORD, VAR, RPAREN, KEYWORD, LBRACE, VAR, IRI, VAR, DOT, RBRACE, KEYWORD, KEYWORD, VAR]);
};

# Attean::Algebra::Construct
# Attean::Algebra::Extend
# Attean::Algebra::Sequence

subtest 'BGP with blank' => sub {
	my $b			= blank('person');
	my $rdf_type	= iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
	my $foaf_name	= iri('http://xmlns.com/foaf/0.1/name');
	my $foaf_knows	= iri('http://xmlns.com/foaf/0.1/knows');
	my $foaf_Person	= iri('http://xmlns.com/foaf/0.1/Person');
	my $bgp1		= Attean::Algebra::BGP->new( triples => [
		triplepattern($b, $rdf_type, $foaf_Person),
		triplepattern($b, $foaf_name, variable('name')),
		triplepattern($b, $foaf_knows, variable('knows')),
	] );
	lives_ok {
		my $string = $bgp1->as_sparql;
		is($string, <<"END", 'expected serialization');
_:person <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://xmlns.com/foaf/0.1/Person> .
_:person <http://xmlns.com/foaf/0.1/name> ?name .
_:person <http://xmlns.com/foaf/0.1/knows> ?knows .
END
	} 'as_sparql returns a string on blank';
};

done_testing();
exit;


{
	note('BGP canonicalization');
	my $b		= blank('person');
	my $rdf_type	= iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
	my $foaf_name	= iri('http://xmlns.com/foaf/0.1/name');
	my $foaf_knows	= iri('http://xmlns.com/foaf/0.1/knows');
	my $foaf_Person	= iri('http://xmlns.com/foaf/0.1/Person');
	my $bgp1	= Attean::Algebra::BGP->new( triples => [
		triplepattern($b, $rdf_type, $foaf_Person),
		triplepattern($b, $foaf_name, variable('name')),
		triplepattern($b, $foaf_knows, variable('knows')),
	] );
	my $bgp2	= Attean::Algebra::BGP->new( triples => [
		triplepattern(blank('s'), $foaf_knows, variable('person')),
		triplepattern(blank('s'), $rdf_type, $foaf_Person),
		triplepattern(blank('s'), $foaf_name, variable('myname')),
	] );

	my $hash1	= sha1_hex( join("\n", map { $_->tuples_string } (@{$bgp1->triples}) ) );
	my $hash2	= sha1_hex( join("\n", map { $_->tuples_string } (@{$bgp2->triples}) ) );
	isnt($hash1, $hash2, 'non-matching pre-canonicalized BGP hashes');
	
	my ($cbgp1, $m1)	= $bgp1->canonical_bgp_with_mapping;
	my ($cbgp2, $m2)	= $bgp2->canonical_bgp_with_mapping;
	
	my $chash1	= sha1_hex( join("\n", map { $_->tuples_string } (@{$cbgp1->triples}) ) );
	my $chash2	= sha1_hex( join("\n", map { $_->tuples_string } (@{$cbgp2->triples}) ) );
	is($chash1, $chash2, 'matching canonicalized BGP hashes' );
	
	is_deeply($m1, { '?name' => { 'prefix' => '?', 'id' => 'v003', 'type' => 'variable' }, '?knows' => { 'id' => 'v002', 'prefix' => '?', 'type' => 'variable' }, '_:person' => { 'id' => 'v001', 'prefix' => '_:', 'type' => 'blank' } }, 'BGP1 mapping');
	is_deeply($m2, { '?person' => { 'prefix' => '?', 'id' => 'v002', 'type' => 'variable' }, '_:s' => { 'prefix' => '_:', 'id' => 'v001', 'type' => 'blank' }, '?myname' => { 'type' => 'variable', 'id' => 'v003', 'prefix' => '?' } }, 'BGP2 mapping');
}

{
	my $t		= triple(variable('s'), iri('p'), variable('o'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
	my @groups	= Attean::ValueExpression->new( value => variable('s') );
	my @aggs	= Attean::AggregateExpression->new(
		distinct	=> 0,
		operator	=> 'SUM',
		children	=> [Attean::ValueExpression->new( value => variable('s') )],
		scalar_vars	=> {},
		variable	=> variable("sum"),
	);
	my $agg		= Attean::Algebra::Group->new(
		children => [$bgp],
		groupby => \@groups,
		aggregates => \@aggs,
	);
	my $s	= $agg->as_string;
	like($s, qr/Group [{] [?]s [}] aggregate [{] [?]sum ← SUM\([?]s\) [}]/, 'aggregate serialization');
}

{
	note('Aggregation');
	my $t		= triple(variable('s'), iri('p'), variable('o'));
	my $bgp		= Attean::Algebra::BGP->new(triples => [$t]);
	my @groups	= Attean::ValueExpression->new( value => variable('s') );
	my @aggs	= Attean::AggregateExpression->new(
		distinct	=> 0,
		operator	=> 'SUM',
		children	=> [Attean::ValueExpression->new( value => variable('s') )],
		scalar_vars	=> {},
		variable	=> variable("sum"),
	);
	my $agg		= Attean::Algebra::Group->new(
		children => [$bgp],
		groupby => \@groups,
		aggregates => \@aggs,
	);
	my $s	= $agg->as_string;
	like($s, qr/Group [{] [?]s [}] aggregate [{] [?]sum ← SUM\([?]s\) [}]/, 'aggregate serialization');
}

{
	note('Ranking');
	# RANKing example for 2 youngest students per school
	my $bgp		= Attean::Algebra::BGP->new(triples => [
		triple(variable('p'), iri('ex:name'), variable('name')),
		triple(variable('p'), iri('ex:school'), variable('school')),
		triple(variable('p'), iri('ex:age'), variable('age')),
	]);
	my @groups	= Attean::ValueExpression->new( value => variable('school') );
	my $r_agg	= Attean::AggregateExpression->new(
		distinct	=> 0,
		operator	=> 'RANK',
		children	=> [Attean::ValueExpression->new( value => variable('age') )],
		scalar_vars	=> {},
		variable	=> variable("rank"),
	);
	my $agg		= Attean::Algebra::Group->new(
		children => [$bgp],
		groupby => \@groups,
		aggregates => [$r_agg],
	);
	my $rank	= Attean::Algebra::Filter->new(
		children	=> [$agg],
		expression	=> Attean::BinaryExpression->new(
			children => [
				Attean::ValueExpression->new( value => variable('rank') ),
				Attean::ValueExpression->new( value => Attean::Literal->integer('2') ),
			],
			operator => '<='
		),
	);
	my $s	= $rank->as_string;
	like($s, qr/Group [{] [?]school [}] aggregate [{] [?]rank ← RANK\([?]age\) [}]/, 'ranking serialization');
}

done_testing();

sub warn_token_stream {
	my $i		= shift;
	while (my $t = $i->next) {
		my $type	= AtteanX::SPARQL::Constants::decrypt_constant($t->type);
		my $value	= $t->value;
		warn sprintf("%-16s: %s\n", $type, $value);
	}
}

sub expect_token_stream {
	my $i		= shift;
	my $expect	= shift;
	while (my $t = $i->next) {
		my $type	= AtteanX::SPARQL::Constants::decrypt_constant($t->type);
		is_token_of_type($t, shift(@$expect));
	}
	is(scalar(@$expect), 0);
}

sub is_token_of_type {
	my $t			= shift;
	my $got			= $t->type;
	my $expect		= shift;
	if ($expect == A) {
		Carp::confess;
	}
	my $got_name	= AtteanX::SPARQL::Constants::decrypt_constant($got);
	my $expect_name	= AtteanX::SPARQL::Constants::decrypt_constant($expect);
	if ($got == $expect) {
		pass("Expected token type $got_name");
	} else {
		my $value	= $t->value;
		fail("Not expected token type (expected $expect_name, but got $got_name $value)");
	}
}

sub ws_is {
	my $got		= shift;
	my $expect	= shift;
	for ($got, $expect) {
		chomp;
		s/\s+//sg;
	}
	is($got, $expect);
}
