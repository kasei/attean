use Test::More;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

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
	my $g		= iri('g');
	my $store	= Attean->get_store('Memory')->new();
	my $model	= Attean::MutableQuadModel->new( store => $store );
	{
		my $data	= <<"END";
		<a> <p> <b> <g> .
		<b> <p> <c> <g> .
		<c> <p> <d> <g> .
		<c> <q> <e> <g> .
		
		<b> <values> "0"^^<http://www.w3.org/2001/XMLSchema#integer> <ints> .
		<b> <values> "1"^^<http://www.w3.org/2001/XMLSchema#integer> <ints> .
		<b> <values> "2"^^<http://www.w3.org/2001/XMLSchema#integer> <ints> .
		<b> <values> "07"^^<http://www.w3.org/2001/XMLSchema#integer> <ints> .
END
		$model->load_triples('nquads', $g, $data);
	}
	
	{
		note('Project');
		my $t		= triplepattern(variable('s'), iri('q'), variable('o'));
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
		is_deeply(\%preds, { 'p' => 1, 'q' => 1 });
	}

	{
		note('Filter');
		my $t		= triplepattern(variable('s'), variable('p'), variable('o'));
		my $bgp		= Attean::Algebra::BGP->new( triples => [$t] );
		my $expr	= Attean::ValueExpression->new( value => variable('o') );
		my $f		= Attean::Algebra::Filter->new( children => [$bgp], expression => $expr );

		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		my $iter	= $e->evaluate($f, iri('ints'));
		my @quads	= $iter->elements;
		is(scalar(@quads), 3, 'filter count');
		
		my @values	= sort { $a <=> $b } map { 0+($_->value('o')->value) } @quads;
		is_deeply(\@values, [1, 2, 7]);
	}

	{
		note('IRI Graph');
		my $t		= triplepattern(variable('s'), iri('values'), variable('o'));
		my $bgp		= Attean::Algebra::BGP->new( triples => [$t] );
		my $graph	= Attean::Algebra::Graph->new( children => [$bgp], graph => iri('ints') );

		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		my $iter	= $e->evaluate($graph, $g);
		my @quads	= $iter->elements;
		is(scalar(@quads), 4, 'graph count');
		
		my @values	= sort { $a <=> $b } map { 0+($_->value('o')->value) } @quads;
		is_deeply(\@values, [0, 1, 2, 7]);
	}

	{
		note('Variable Graph');
		my $t		= triplepattern(variable('s'), iri('values'), variable('o'));
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
		is($gt->value, 'ints');
	}

	{
		note('Join');
		my $t1		= triplepattern(iri('a'), iri('p'), variable('o'));
		my $bgp1	= Attean::Algebra::BGP->new( triples => [$t1] );

		my $t2		= triplepattern(variable('o'), iri('p'), iri('c'));
		my $bgp2	= Attean::Algebra::BGP->new( triples => [$t2] );
		
		my $j		= Attean::Algebra::Join->new( children => [$bgp1, $bgp2] );
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		my $iter	= $e->evaluate($j, $g);
		my @results	= $iter->elements;
		is(scalar(@results), 1, 'expected result count');
		my ($r)		= @results;
		does_ok($r, 'Attean::API::Result');
		my $term	= $r->value('o');
		is($term->value, 'b');
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
			is_deeply(\@values_o, [qw(b c d e)], 'ORDER ascending');
		}
		
		{
			my $order_o	= $sort_by->( $b, 'o', 0 );
			my @rows_o	= $e->evaluate($order_o, $g)->elements;
			my @values_o	= map { $_->value('o')->value } @rows_o;
			is_deeply(\@values_o, [qw(e d c b)], 'ORDER descending');
		}
		
		{
			my $order_so	= $sort_by->( $b, 's' => 1, 'o' => 0 );
			my @rows_so	= $e->evaluate($order_so, $g)->elements;
			my @values_so	= map { [$_->value('s')->value, $_->value('o')->value] } @rows_so;
			is_deeply(\@values_so, [[qw(a b)], [qw(b c)], [qw(c e)], [qw(c d)]], 'ORDER mixed');
# 			foreach my $r (@rows_so) { say $r->as_string }
		}
	}
	
	{
		note('ZeroOrOnePath');
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => $g );
		
		{
			# <a> <q>? ?o
			my $pred	= Attean::Algebra::PredicatePath->new( predicate => iri('q') );
			my $pp		= Attean::Algebra::ZeroOrOnePath->new( children => [ $pred ] );
			my $path	= Attean::Algebra::Path->new( subject => iri('a'), path => $pp, object => variable('o') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 1);
			is($rows[0]->value('o')->value, 'a');
		}
		
		{
			# ?s <q>? <c>
			my $pred	= Attean::Algebra::PredicatePath->new( predicate => iri('q') );
			my $pp		= Attean::Algebra::ZeroOrOnePath->new( children => [ $pred ] );
			my $path	= Attean::Algebra::Path->new( subject => variable('s'), path => $pp, object => iri('c') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 1);
			is($rows[0]->value('s')->value, 'c');
		}
		
		{
			# <c> <q>? <c>
			my $pred	= Attean::Algebra::PredicatePath->new( predicate => iri('q') );
			my $pp		= Attean::Algebra::ZeroOrOnePath->new( children => [ $pred ] );
			my $path	= Attean::Algebra::Path->new( subject => iri('c'), path => $pp, object => iri('c') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 1);
			is_deeply([$rows[0]->variables], []);
		}
		
		{
			# <c> <q>? <d>
			my $pred	= Attean::Algebra::PredicatePath->new( predicate => iri('q') );
			my $pp		= Attean::Algebra::ZeroOrOnePath->new( children => [ $pred ] );
			my $path	= Attean::Algebra::Path->new( subject => iri('c'), path => $pp, object => iri('d') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 0);
		}
		
		{
			# ?s <q>? ?o
			my $pred	= Attean::Algebra::PredicatePath->new( predicate => iri('q') );
			my $pp		= Attean::Algebra::ZeroOrOnePath->new( children => [ $pred ] );
			my $path	= Attean::Algebra::Path->new( subject => variable('s'), path => $pp, object => variable('o') );
			my $iter	= $e->evaluate($path, $g);
			my @rows	= $iter->elements;
			is(scalar(@rows), 6);
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
		while (my $q = $iter->next) { say $q->as_string }
	}
	
	if (0) {
		# pp12
		my $p1	= Attean::Algebra::PredicatePath->new( predicate => iri('http://www.example.org/schema#p1') );
		my $p2	= Attean::Algebra::PredicatePath->new( predicate => iri('http://www.example.org/schema#p2') );
		my $seq	= Attean::Algebra::SequencePath->new( children => [$p1, $p2] );
		my $pp	= Attean::Algebra::OneOrMorePath->new( children => [$seq] );
		my $path	= Attean::Algebra::Path->new( subject => iri('http://www.example.org/instance#a'), path => $pp, object => variable('x') );
		my $e		= Attean::SimpleQueryEvaluator->new( model => $model, default_graph => iri('pp11') );
		my $iter	= $e->evaluate($path, iri('pp11'));
		while (my $q = $iter->next) { say $q->as_string }
	}
}

done_testing();

sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
