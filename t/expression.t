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
my $two	= Attean::ValueExpression->new( value => Attean::Literal->integer(2) );
my $foo	= Attean::ValueExpression->new( value => literal('foo') );

dies_ok { Attean::BinaryExpression->new( children => [$t, $f], operator => '***' ) } 'Bad BinaryExpression operator';
is($foo->as_string, '"foo"');
is($two->as_string, '2');
is($two->arity, 0);

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
	is($e->arity, 1);
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

{
	my $expr	= Attean::ValueExpression->new( value => variable('foo') );
	my $b		= Attean::Result->new( bindings => { foo => literal('bar'), baz => iri('quux') } );
	my $foo		= $expr->evaluate( $b );
	does_ok($foo, 'Attean::API::Literal');
	is($foo->value, 'bar');
}

done_testing();

sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
