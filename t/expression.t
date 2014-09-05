use v5.14;
use autodie;
use utf8;
use Test::More;
use Test::Exception;

use Attean;
use Attean::RDF;

{
	my $t	= Attean::Literal->true;
	isa_ok($t, 'Attean::Literal');
	is($t->value, 'true');
}

my $true	= Attean::Literal->true;
my $false	= Attean::Literal->false;
my $t	= Attean::ValueExpression->new( value => $true );
my $f	= Attean::ValueExpression->new( value => $false );

dies_ok { Attean::BinaryExpression->new( children => [$t, $f], operator => '***' ) } 'Bad BinaryExpression operator';

{
	my $tt	= $t->evaluate();
	is_deeply($tt, $true, 'ValueExpression evaluate');
}

{
	my $e	= Attean::BinaryExpression->new( children => [$t, $f], operator => '&&' );
	ok($e->does('Attean::API::Expression'));
	is($e->as_string, '(true && false)', 'binary &&');
}

{
	my $e	= Attean::UnaryExpression->new( children => [$f], operator => '!' );
	ok($e->does('Attean::API::Expression'));
	is($e->as_string, '!(false)', 'unary not');
}

{
	my $e	= Attean::UnaryExpression->new( children => [$f], operator => 'not' );
	ok($e->does('Attean::API::Expression'));
	is($e->as_string, '!(false)', 'unary not');
}

{
	my $e	= Attean::FunctionExpression->new( children => [$f, $t], operator => 'coalesce' );
	ok($e->does('Attean::API::Expression'));
	is($e->operator, 'COALESCE');
	is($e->as_string, 'COALESCE(false, true)', 'function coalesce');
}

done_testing();

__END__

{
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	isa_ok($bgp, 'Attean::Algebra::BGP');
	ok($bgp->is_leaf, 'bgp is_leaf');
	
	my $dist	= Attean::Algebra::Distinct->new( children => [$bgp] );
	isa_ok($dist, 'Attean::Algebra::Distinct');
	ok(not($dist->is_leaf), 'distinct not is_leaf');
	
	{
		my @prefix_seen;
		my @postfix_seen;
		my $prefix	= sub {
			my $node	= shift;
			my $name	= ref($node);
			$name	=~ s/^.*://;
			push(@prefix_seen, $name);
		};
		my $postfix	= sub {
			my $node	= shift;
			my $name	= ref($node);
			$name	=~ s/^.*://;
			push(@postfix_seen, $name);
		};
		$dist->walk( prefix => $prefix, postfix => $postfix );
		is_deeply(\@prefix_seen, [qw'Distinct BGP'], 'prefix walk order');
		is_deeply(\@postfix_seen, [qw'BGP Distinct'], 'postfix walk order');
	}
}

{
	my $t	= triple(variable('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	my $join	= Attean::Algebra::Join->new( children => [$bgp, $bgp] );
	my @walk;
	$join->walk(prefix => sub { push(@walk, shift) });
	is(scalar(@walk), 3, 'expected walk count');
	
	my @cover;
	$join->cover(prefix => sub { push(@cover, shift) });
	is(scalar(@cover), 2, 'expected cover count');
}

{
	my $p1	= iri('p1');
	my $pp1	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
	ok($pp1->does('Attean::API::PropertyPath'), 'PredicatePath consumes PropertyPath');
	is($pp1->as_string, '<p1>', 'PredicatePath as_string');
	
	my $p2	= iri('p2');
	my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );

	my $nps	= Attean::Algebra::NegatedPropertySet->new( predicates => [$p1, $p2] );
	ok($nps->does('Attean::API::PropertyPath'), 'NegatedPropertySet consumes PropertyPath');
	is($nps->as_string, '!(<p1>|<p2>)', 'NegatedPropertySet as_string');
	
	my $seq1	= Attean::Algebra::SequencePath->new( children => [$pp2] );
	is($seq1->as_string, '<p2>', 'unary SequencePath as_string');

	my $seq	= Attean::Algebra::SequencePath->new( children => [$pp1, $pp2] );
	is($seq->as_string, '(<p1>/<p2>)', 'SequencePath as_string');

	my $alt1	= Attean::Algebra::AlternativePath->new( children => [$pp2] );
	is($alt1->as_string, '<p2>', 'unary AlternativePath as_string');

	my $alt	= Attean::Algebra::AlternativePath->new( children => [$pp1, $pp2] );
	is($alt->as_string, '(<p1>|<p2>)', 'AlternativePath as_string');

	my $inv1	= Attean::Algebra::InversePath->new( children => [$pp2] );
	is($inv1->as_string, '^<p2>', 'InversePath as_string');
	
	my $inv_seq	= Attean::Algebra::InversePath->new( children => [$seq] );
	is($inv_seq->as_string, '^(<p1>/<p2>)', 'complex InversePath as_string');
	
	my $inv_seq_star	= Attean::Algebra::ZeroOrMorePath->new( children => [$inv_seq] );
	is($inv_seq_star->as_string, '(^(<p1>/<p2>))*', 'complex ZeroOrMorePath as_string');
}

