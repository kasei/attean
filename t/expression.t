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
