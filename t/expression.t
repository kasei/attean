use v5.14;
use autodie;
use utf8;
use Test::More;
use Test::Exception;

use Attean;
use Attean::RDF;
use Attean::Expression;
use Attean::SimpleQueryEvaluator;

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

my $eval	= Attean::SimpleQueryEvaluator::ExpressionEvaluator->new();

{
	my $tt	= $eval->evaluate_expression($t);
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
	my $foo		= $eval->evaluate_expression($expr, $b);
	does_ok($foo, 'Attean::API::Literal');
	is($foo->value, 'bar');
}

note('Expression evaluation');

my $ident	= Attean::Result->new();
{
	my $a		= integer(2);
	my $b		= integer(4);
	my $error	= Attean::BinaryExpression->new( children => [integer(1), integer(0)], operator => '/' );
	
	{
		my $plus	= Attean::BinaryExpression->new( children => [$a, $b], operator => '+' );
		my $v		= $eval->evaluate_expression($plus, $ident);
		does_ok($v, 'Attean::API::NumericLiteral');
		is($v->numeric_value, 6, 'numeric +');
		is($v->datatype->value, 'http://www.w3.org/2001/XMLSchema#integer', 'expected result datatype');
	}
	{
		my $plus	= Attean::BinaryExpression->new( children => [$a, $b], operator => '-' );
		my $v		= $eval->evaluate_expression($plus, $ident);
		does_ok($v, 'Attean::API::NumericLiteral');
		is($v->numeric_value, -2, 'numeric -');
		is($v->datatype->value, 'http://www.w3.org/2001/XMLSchema#integer', 'expected result datatype');
	}
	{
		my $plus	= Attean::BinaryExpression->new( children => [$a, $b], operator => '*' );
		my $v		= $eval->evaluate_expression($plus, $ident);
		does_ok($v, 'Attean::API::NumericLiteral');
		is($v->numeric_value, 8, 'numeric *');
		is($v->datatype->value, 'http://www.w3.org/2001/XMLSchema#integer', 'expected result datatype');
	}
	{
		my $plus	= Attean::BinaryExpression->new( children => [$a, $b], operator => '/' );
		my $v		= $eval->evaluate_expression($plus, $ident);
		does_ok($v, 'Attean::API::NumericLiteral');
		is($v->numeric_value, 0.5, 'numeric /');
		is($v->datatype->value, 'http://www.w3.org/2001/XMLSchema#decimal', 'expected result datatype');
	}
	
	{
		my $iri		= Attean::ValueExpression->new( value => iri('http://example.org/') );
		my $plus	= Attean::BinaryExpression->new( children => [$a, $iri], operator => '+' );
		is($eval->evaluate_expression($plus, $ident), undef, 'TypeError on bad operand numeric op');
	}
	
	{
		# The SPARQL 1.1 logical truth table from <http://www.w3.org/TR/sparql11-query/#evaluation>
		my %values		= ('T' => $t, 'F' => $f, 'E' => $error);
		my %expected;
		$expected{qw(T T)}	= { '||' => 'T', '&&' => 'T' };
		$expected{qw(T F)}	= { '||' => 'T', '&&' => 'F' };
		$expected{qw(F T)}	= { '||' => 'T', '&&' => 'F' };
		$expected{qw(F F)}	= { '||' => 'F', '&&' => 'F' };
		$expected{qw(T E)}	= { '||' => 'T', '&&' => 'E' };
		$expected{qw(E T)}	= { '||' => 'T', '&&' => 'E' };
		$expected{qw(F E)}	= { '||' => 'E', '&&' => 'F' };
		$expected{qw(E F)}	= { '||' => 'E', '&&' => 'F' };
		$expected{qw(E E)}	= { '||' => 'E', '&&' => 'E' };
		
		foreach my $op (qw(|| &&)) {
			foreach my $l (qw(T F E)) {
				foreach my $r (qw(T F E)) {
					my $lhs		= $values{$l};
					my $rhs		= $values{$r};
					my $expr	= Attean::BinaryExpression->new( children => [$lhs, $rhs], operator => $op );
					my $expect	= $expected{$l, $r}{$op};
					if ($expect eq 'E') {
						my $term	= $eval->evaluate_expression($expr, $ident);
						is($term, undef, "$l $op $r => $expect");
					} else {
						my $value	= ($expect eq 'T') ? 'true' : 'false';
						my $term	= $eval->evaluate_expression($expr, $ident);
						is($term->value, $value, "$l $op $r => $expect");
					}
				}
			}
		}
	}
}



done_testing();

sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}

sub integer {
	my $value	= shift;
	return Attean::ValueExpression->new( value => Attean::Literal->integer($value) );
}
