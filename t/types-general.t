#!/usr/bin/env perl

use strict;
use warnings;
use Test::More;
use Test::TypeTiny;
use Attean;
use Test::Requires { 'Attean::IRI' => '0.023' };
use Attean::RDF qw(
	iri
	blank
	literal
	triple
	quad
);
use Types::Attean qw(
        AtteanIRI
	AtteanBlank
        AtteanLiteral
        AtteanSubject AtteanPredicate AtteanObject
        AtteanGraph
        AtteanTriple
        AtteanQuad
);

my $iri     = iri('http://www.example.net/');
my $blank   = blank('b0');
my $literal = literal('foo');

my $triple = triple( $blank, $iri, $literal );
my $quad   =   quad( $blank, $iri, $literal, blank('g0') );

should_pass( $iri    , AtteanIRI     );
should_pass( $blank  , AtteanBlank   );
should_pass( $literal, AtteanLiteral );

note 'IRI can be in any position';
should_pass( $iri    , AtteanSubject   );
should_pass( $iri    , AtteanPredicate );
should_pass( $iri    , AtteanObject    );
should_pass( $iri    , AtteanGraph     );

should_pass( $blank  , AtteanSubject   );
should_fail( $blank  , AtteanPredicate , 'blank can not be a predicate');
should_pass( $blank  , AtteanObject    );
should_pass( $blank  , AtteanGraph     );

should_fail( $literal, AtteanSubject   );
should_fail( $literal, AtteanPredicate );
should_pass( $literal, AtteanObject    , 'literal can only be an object');
should_fail( $literal, AtteanGraph     );

should_pass( $triple , AtteanTriple );
should_fail( $triple , AtteanQuad   , 'triple is not a quad');

should_pass( $quad   , AtteanTriple , 'quad is also a triple');
should_pass( $quad   , AtteanQuad   );

done_testing;
