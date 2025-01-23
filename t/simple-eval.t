use Test::Modern;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Test::LWP::UserAgent;
use Attean parsers => ['Turtle'];
use Attean::RDF;
use Attean::SimpleQueryEvaluator;

{
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	
	my $graph	= Attean::IRI->new('http://example.org/graph');
	{
		my $data	= <<"END";
		_:a <b> _:a .
		<a> <b> <a> .
		<a> <c> 2, 3 .
END
		$model->load_triples('turtle', $graph, $data);
	}
	
	my $e	= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $graph );
	isa_ok($e, 'Attean::SimpleQueryEvaluator');
	
	my $active_graph	= $graph;

	{
		my $t	= Attean::TriplePattern->new(map { variable($_) } qw(s p o));
		my $bgp	= Attean::Algebra::BGP->new( triples => [$t] );
		does_ok($bgp, 'Attean::API::Algebra');
	
		my $iter	= $e->evaluate($bgp, $active_graph);
		my $count	= 0;
		while (my $r = $iter->next) {
			$count++;
			does_ok($r, 'Attean::API::Result');
			my $s	= $r->value('s');
			is($s->value, 'a');
			my $p	= $r->value('p');
			does_ok($p, 'Attean::API::IRI');
			like($p->value, qr/^[bc]$/);
		}
		is($count, 4);
	}

	{
		my $t1	= Attean::TriplePattern->new(iri('a'), iri('b'), variable('o1'));
		my $t2	= Attean::TriplePattern->new(iri('a'), iri('c'), variable('o2'));
		my $bgp	= Attean::Algebra::BGP->new( triples => [$t1, $t2] );
		does_ok($bgp, 'Attean::API::Algebra');
	
		my $iter	= $e->evaluate($bgp, $active_graph);
		my $count	= 0;
		while (my $r = $iter->next) {
			$count++;
			like($r->as_string, qr[{o1=<a>, o2="[23]"\^\^<http://www.w3.org/2001/XMLSchema#integer>}]);
		}
		is($count, 2);
	}
}

{
	my $g		= iri('tag:g');
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	{
		my $data	= <<"END";
		<tag:a> <tag:p> <tag:b> <tag:g> .
		<tag:b> <tag:p> <tag:c> <tag:g> .
		<tag:c> <tag:p> <tag:d> <tag:g> .
		<tag:c> <tag:q> <tag:e> <tag:g> .
		
		<tag:b> <tag:values> "0"^^<http://www.w3.org/2001/XMLSchema#integer> <tag:ints> .
		<tag:b> <tag:values> "1"^^<http://www.w3.org/2001/XMLSchema#integer> <tag:ints> .
		<tag:b> <tag:values> "2"^^<http://www.w3.org/2001/XMLSchema#integer> <tag:ints> .
		<tag:b> <tag:values> "07"^^<http://www.w3.org/2001/XMLSchema#integer> <tag:ints> .
END
		$model->load_triples('nquads', $g, $data);
	}
	
	{
		note('Project');
		my $t		= triplepattern(variable('s'), iri('tag:q'), variable('o'));
		my $b		= Attean::Algebra::BGP->new( triples => [$t] );
		my $p		= Attean::Algebra::Project->new( children => [$b], variables => [variable('s')] );
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		my $iter	= $e->evaluate($p, $g);
		my @subj	= $iter->elements;
		is(scalar(@subj), 1, 'expected project count');
		my ($r)		= @subj;
		does_ok($r, 'Attean::API::Result');
		is_deeply([$r->variables], ['s'], 'expected projection variable');
	}
	
	{
		note('Distinct');
		my $t		= triplepattern(variable('s'), variable('p'), variable('o'));
		my $b		= Attean::Algebra::BGP->new( triples => [$t] );
		my $p		= Attean::Algebra::Project->new( children => [$b], variables => [variable('p')] );
		my $d		= Attean::Algebra::Distinct->new( children => [$p] );
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );

		my $proj	= $e->evaluate($p, $g);
		my @ppreds	= $proj->elements;
		is(scalar(@ppreds), 4, 'pre-distinct projected count');
		
		my $dist	= $e->evaluate($d, $g);
		my @dpreds	= $dist->elements;
		is(scalar(@dpreds), 2, 'post-distinct projected count');

		my %preds	= map { $_->value('p')->value => 1 } @dpreds;
		is_deeply(\%preds, { 'tag:p' => 1, 'tag:q' => 1 });
	}

	{
		note('Filter');
		my $t		= triplepattern(variable('s'), variable('p'), variable('o'));
		my $bgp		= Attean::Algebra::BGP->new( triples => [$t] );
		my $expr	= Attean::ValueExpression->new( value => variable('o') );
		my $f		= Attean::Algebra::Filter->new( children => [$bgp], expression => $expr );

		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		my $iter	= $e->evaluate($f, iri('tag:ints'));
		my @quads	= $iter->elements;
		is(scalar(@quads), 3, 'filter count');
		
		my @values	= sort { $a <=> $b } map { 0+($_->value('o')->value) } @quads;
		is_deeply(\@values, [1, 2, 7]);
	}

	{
		note('IRI Graph');
		my $t		= triplepattern(variable('s'), iri('tag:values'), variable('o'));
		my $bgp		= Attean::Algebra::BGP->new( triples => [$t] );
		my $graph	= Attean::Algebra::Graph->new( children => [$bgp], graph => iri('tag:ints') );

		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		my $iter	= $e->evaluate($graph, $g);
		my @quads	= $iter->elements;
		is(scalar(@quads), 4, 'graph count');
		
		my @values	= sort { $a <=> $b } map { 0+($_->value('o')->value) } @quads;
		is_deeply(\@values, [0, 1, 2, 7]);
	}

	{
		note('Variable Graph');
		my $t		= triplepattern(variable('s'), iri('tag:values'), variable('o'));
		my $bgp		= Attean::Algebra::BGP->new( triples => [$t] );
		my $graph	= Attean::Algebra::Graph->new( children => [$bgp], graph => variable('graph') );

		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		my $iter	= $e->evaluate($graph, $g);
		my @quads	= $iter->elements;
		is(scalar(@quads), 4, 'graph count');
		
		my ($r)		= @quads;
		does_ok($r, 'Attean::API::Result');
		my $gt		= $r->value('graph');
		does_ok($gt, 'Attean::API::Term');
		is($gt->value, 'tag:ints');
	}

	{
		note('Join');
		my $t1		= triplepattern(iri('tag:a'), iri('tag:p'), variable('o'));
		my $bgp1	= Attean::Algebra::BGP->new( triples => [$t1] );

		my $t2		= triplepattern(variable('o'), iri('tag:p'), iri('tag:c'));
		my $bgp2	= Attean::Algebra::BGP->new( triples => [$t2] );
		
		my $j		= Attean::Algebra::Join->new( children => [$bgp1, $bgp2] );
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		my $iter	= $e->evaluate($j, $g);
		my @results	= $iter->elements;
		is(scalar(@results), 1, 'expected result count');
		my ($r)		= @results;
		does_ok($r, 'Attean::API::Result');
		my $term	= $r->value('o');
		is($term->value, 'tag:b');
	}

	{
		note('Slice');
		my $t		= triplepattern(variable('s'), variable('p'), variable('o'));
		my $b		= Attean::Algebra::BGP->new( triples => [$t] );
		my $s_o		= Attean::Algebra::Slice->new( children => [$b], offset => 1 );
		my $s_l		= Attean::Algebra::Slice->new( children => [$b], limit => 1 );
		my $s_ol	= Attean::Algebra::Slice->new( children => [$b], limit => 1, offset => 1 );
		
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		my @r_o		= $e->evaluate($s_o, $g)->elements;
		my @r_l		= $e->evaluate($s_l, $g)->elements;
		my @r_ol	= $e->evaluate($s_ol, $g)->elements;
		is(scalar(@r_o), 3, 'offset count');
		is(scalar(@r_l), 1, 'limit count');
		is(scalar(@r_ol), 1, 'offset/limit count');
	}

	{
		note('Order');
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		my $sort_by	= sub {
			my $algebra		= shift;
			my @cmps;
			while (scalar(@_)) {
				my ($variable, $asc)	= splice(@_, 0, 2);
				my $expr	= Attean::ValueExpression->new( value => variable($variable) );
				my $cmp		= Attean::Algebra::Comparator->new( expression => $expr, ascending => $asc );
				push(@cmps, $cmp);
			}
			return Attean::Algebra::OrderBy->new( children => [$algebra], comparators => \@cmps );
		};
		
		my $b		= Attean::Algebra::BGP->new( triples => [triplepattern(variable('s'), variable('p'), variable('o'))] );
		
		{
			my $order_o	= $sort_by->( $b, 'o', 1 );
			my @rows_o	= $e->evaluate($order_o, $g)->elements;
			my @values_o	= map { $_->value('o')->value } @rows_o;
			is_deeply(\@values_o, [qw(tag:b tag:c tag:d tag:e)], 'ORDER ascending');
		}
		
		{
			my $order_o	= $sort_by->( $b, 'o', 0 );
			my @rows_o	= $e->evaluate($order_o, $g)->elements;
			my @values_o	= map { $_->value('o')->value } @rows_o;
			is_deeply(\@values_o, [qw(tag:e tag:d tag:c tag:b)], 'ORDER descending');
		}
		
		{
			my $order_so	= $sort_by->( $b, 's' => 1, 'o' => 0 );
			my @rows_so	= $e->evaluate($order_so, $g)->elements;
			my @values_so	= map { [$_->value('s')->value, $_->value('o')->value] } @rows_so;
			is_deeply(\@values_so, [[qw(tag:a tag:b)], [qw(tag:b tag:c)], [qw(tag:c tag:e)], [qw(tag:c tag:d)]], 'ORDER mixed');
# 			foreach my $r (@rows_so) { say $r->as_string }
		}
	}
	
	{
		note('ZeroOrOnePath');
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		
		{
			# <a> <q>? ?o
			my $pred	= Attean::Algebra::PredicatePath->new( predicate => iri('tag:q') );
			my $pp		= Attean::Algebra::ZeroOrOnePath->new( children => [ $pred ] );
			my $path	= Attean::Algebra::Path->new( subject => iri('tag:a'), path => $pp, object => variable('o') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 1);
			is($rows[0]->value('o')->value, 'tag:a');
		}
		
		{
			# ?s <q>? <c>
			my $pred	= Attean::Algebra::PredicatePath->new( predicate => iri('tag:q') );
			my $pp		= Attean::Algebra::ZeroOrOnePath->new( children => [ $pred ] );
			my $path	= Attean::Algebra::Path->new( subject => variable('s'), path => $pp, object => iri('tag:c') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 1);
			is($rows[0]->value('s')->value, 'tag:c');
		}
		
		{
			# <c> <q>? <c>
			my $pred	= Attean::Algebra::PredicatePath->new( predicate => iri('tag:q') );
			my $pp		= Attean::Algebra::ZeroOrOnePath->new( children => [ $pred ] );
			my $path	= Attean::Algebra::Path->new( subject => iri('tag:c'), path => $pp, object => iri('tag:c') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 1);
			is_deeply([$rows[0]->variables], []);
		}
		
		{
			# <c> <q>? <d>
			my $pred	= Attean::Algebra::PredicatePath->new( predicate => iri('tag:q') );
			my $pp		= Attean::Algebra::ZeroOrOnePath->new( children => [ $pred ] );
			my $path	= Attean::Algebra::Path->new( subject => iri('tag:c'), path => $pp, object => iri('tag:d') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 0);
		}
		
		{
			# ?s <q>? ?o
			my $pred	= Attean::Algebra::PredicatePath->new( predicate => iri('tag:q') );
			my $pp		= Attean::Algebra::ZeroOrOnePath->new( children => [ $pred ] );
			my $path	= Attean::Algebra::Path->new( subject => variable('s'), path => $pp, object => variable('o') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 6);
		}
	}

	{
		note('NegatedPropertySet');
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		
		{
			# <c> !<p> ?o
			my $pp		= Attean::Algebra::NegatedPropertySet->new( predicates => [iri('tag:p')] );
			my $path	= Attean::Algebra::Path->new( subject => iri('tag:c'), path => $pp, object => variable('o') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 1);
			is($rows[0]->value('o')->value, 'tag:e');
		}
	}

	{
		note('Sequence Path');
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		
		{
			# <a> <p>/<values> ?o
			my $p1		= Attean::Algebra::PredicatePath->new( predicate => iri('tag:p') );
			my $p2		= Attean::Algebra::PredicatePath->new( predicate => iri('tag:q') );
			my $pp		= Attean::Algebra::SequencePath->new( children => [ $p1, $p2 ] );
			my $path	= Attean::Algebra::Path->new( subject => iri('tag:b'), path => $pp, object => variable('o') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 1);
			is($rows[0]->value('o')->value, 'tag:e');
		}
	}

	{
		note('BIND');
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		
		{
			my $t		= triplepattern(variable('s'), variable('p'), variable('o'));
			my $b		= Attean::Algebra::BGP->new( triples => [$t] );
			my $expr	= Attean::ValueExpression->new( value => variable('o') );
			my $extend	= Attean::Algebra::Extend->new(children => [$b], variable => variable('x'), expression => $expr);
			my $iter	= $e->evaluate($extend, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 4);
			like($rows[0]->value('x')->value, qr'^tag:[bcde]$');
		}
	}
	
	{
		note('CONSTRUCT');
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		
		{
			my $t		= triplepattern(variable('s'), iri('tag:q'), variable('o'));
			my $u		= triplepattern(variable('o'), iri('tag:qqq'), variable('s'));
			my $b		= Attean::Algebra::BGP->new( triples => [$t] );
			my $c		= Attean::Algebra::Construct->new( children => [$b], triples => [$u] );
			my $iter	= $e->evaluate($c, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 1);
			is($rows[0]->as_string, '<tag:e> <tag:qqq> <tag:c> .');
		}
	}
	
	{
		note('CAST');
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		
		{
			my $t		= triplepattern(variable('s'), iri('tag:values'), variable('o'));
			my $bgp		= Attean::Algebra::BGP->new( triples => [$t] );
			my $graph	= Attean::Algebra::Graph->new( children => [$bgp], graph => iri('tag:ints') );
			my $var		= Attean::ValueExpression->new( value => variable('o') );
			my $expr	= Attean::CastExpression->new( children => [$var], datatype => iri('http://www.w3.org/2001/XMLSchema#decimal') );
			my $extend	= Attean::Algebra::Extend->new(children => [$graph], variable => variable('x'), expression => $expr);
			my $iter	= $e->evaluate($extend, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 4);
			foreach my $r (@rows) {
				is($r->value('x')->datatype->value, 'http://www.w3.org/2001/XMLSchema#decimal', 'decimal datatype');
				like($r->value('x')->value, qr/^[0127]\.0$/, 'decimal value');
			}
		}
	}
}

{
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	{
		my $data	= <<'END';
@prefix ex:	<http://www.example.org/schema#>.
@prefix in:	<http://www.example.org/instance#>.

in:a ex:p1 in:b .
in:b ex:p2 in:c .
in:a ex:p1 in:d .
in:d ex:p2 in:c .
END
		$model->load_triples('turtle', iri('pp11'), $data);
	}
	{
		my $data	= <<'END';
@prefix : <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

:a foaf:knows :b .
:b foaf:knows :c .
END
		$model->load_triples('turtle', iri('pp14'), $data);
	}
	
	{
		# pp14
		my $p1	= Attean::Algebra::PredicatePath->new( predicate => iri('http://xmlns.com/foaf/0.1/knows') );
		my $pp	= Attean::Algebra::ZeroOrMorePath->new( children => [$p1] );
		my $path	= Attean::Algebra::Path->new( subject => variable('X'), path => $pp, object => variable('Y') );
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => iri('pp14') );
		my $iter	= $e->evaluate($path, iri('pp14'));
		my @rows	= $iter->elements;
		is(scalar(@rows), 6);
		my @expected	= (
			q(a a),
			q(a b),
			q(a c),
			q(b b),
			q(b c),
			q(c c),
		);
		my @got;
		foreach my $r (@rows) {
			my $str	= join(' ', map { $r->value($_)->value } qw(X Y));
			$str	=~ s#http://example.org/##g;
			push(@got, $str);
		}
		is_deeply([sort @got], \@expected);
# 		while (my $q = $iter->next) { say $q->as_string }
	}
	
# 	{
# 		# pp12
# 		my $p1	= Attean::Algebra::PredicatePath->new( predicate => iri('http://www.example.org/schema#p1') );
# 		my $p2	= Attean::Algebra::PredicatePath->new( predicate => iri('http://www.example.org/schema#p2') );
# 		my $seq	= Attean::Algebra::SequencePath->new( children => [$p1, $p2] );
# 		my $pp	= Attean::Algebra::OneOrMorePath->new( children => [$seq] );
# 		my $path	= Attean::Algebra::Path->new( subject => iri('http://www.example.org/instance#a'), path => $pp, object => variable('x') );
# 		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => iri('pp11') );
# 		my $iter	= $e->evaluate($path, iri('pp11'));
# 		while (my $q = $iter->next) { say $q->as_string }
# 	}

	{
		note('Service');
		my $ua		= Test::LWP::UserAgent->new();
		$ua->map_response(qr{example.org/sparql}, HTTP::Response->new('200', 'OK', ['Content-Type' => 'application/sparql-results+xml'], <<'XML'));
<?xml version="1.0"?>
<sparql xmlns="http://www.w3.org/2005/sparql-results#">
  <head>
	<variable name="s"/>
	<variable name="p"/>
	<variable name="o"/>
  </head>
  <results>
	<result>
		<binding name="s"><uri>http://example.org/s4</uri></binding>
		<binding name="p"><uri>http://example.org/p</uri></binding>
		<binding name="o"><literal datatype="http://www.w3.org/2001/XMLSchema#integer">4</literal></binding>
	</result>
	<result>
		<binding name="s"><uri>http://example.org/s3</uri></binding>
		<binding name="p"><uri>http://example.org/p</uri></binding>
		<binding name="o"><literal datatype="http://www.w3.org/2001/XMLSchema#integer">3</literal></binding>
	</result>
  </results>
</sparql>
XML
		my $g		= iri('g');
		my $ep		= iri('http://example.org/sparql');
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g, user_agent => $ua );
		my $t		= triplepattern(variable('s'), variable('p'), variable('o'));
		my $bgp		= Attean::Algebra::BGP->new( triples => [$t] );
		my $algebra	= Attean::Algebra::Service->new(
			endpoint => $ep,
			children => [$bgp],
		);
		my $iter	= $e->evaluate($algebra, $g);
		my @results	= $iter->elements;
		is(scalar(@results), 2, 'expected result count');
		my @objects	= sort { $a <=> $b } map { $_->value('o')->value } @results;
		is_deeply(\@objects, [3,4], 'expected values');
	}
}

{
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	
	my $graph	= Attean::IRI->new('http://example.org/graph');
	{
		my $data	= <<'END';
@prefix test: <http://ontologi.es/doap-tests#> .
@prefix deps: <http://ontologi.es/doap-deps#>.
@prefix httph:<http://www.w3.org/2007/ont/httph#> .
@prefix http: <http://www.w3.org/2007/ont/http#> .
@prefix nfo:  <http://www.semanticdesktop.org/ontologies/2007/03/22/nfo#> .
@prefix :     <http://example.org/test#> .


:test_list a test:FixtureTable ;
    test:fixtures :public_writeread_unauthn_alt .


:public_writeread_unauthn_alt a test:AutomatedTest ;
    test:purpose "More elaborate HTTP vocab for PUT then GET test"@en ;
    test:test_script <http://example.org/httplist#http_req_res_list_unauthenticated> ;
    test:params [
        test:steps (
            [
                test:request :public_writeread_unauthn_alt_put_req ;
                test:response_assertion :public_writeread_unauthn_alt_put_res
            ]
            [
                test:request :public_writeread_unauthn_alt_get_req ;
                test:response_assertion :public_writeread_unauthn_alt_get_res
            ]
        )
    ] .
END
		$model->load_triples('turtle', $graph, $data);
	}
	
	my $active_graph	= $graph;
	my $test	= URI::Namespace->new('http://ontologi.es/doap-tests#');
	my $b	= $model->objects(undef, iri('http://www.w3.org/1999/02/22-rdf-syntax-ns#first'))->next();
	my $t1	= triplepattern($b, iri($test->request->as_string), variable('request'));
	my $t2	= triplepattern($b, iri($test->response_assertion->as_string), variable('response_assertion'));
	my $bgp	= bgp($t1, $t2);

	{
		my $e	= Attean::SimpleQueryEvaluator->new(
			model => $model,
			default_graph => $graph,
			ground_blanks => 0
		);
		my $iter = $e->evaluate($bgp, $graph);
		my @v	= $iter->elements;
		is(scalar(@v), 2);
	}

	{
		my $e	= Attean::SimpleQueryEvaluator->new(
			model => $model,
			default_graph => $graph,
			ground_blanks => 1
		);
		my $iter = $e->evaluate($bgp, $graph);
		my @v	= $iter->elements;
		is(scalar(@v), 1);
	}
}

done_testing();
