use v5.14;
use warnings;
use autodie;
use Test::More;
use utf8;

use Attean;

sub iri { Attean::IRI->new(shift) }
sub blank { Attean::Blank->new(shift) }
sub literal {
	my ($value, $lang, $dt)	= @_;
	if ($lang) {
		return Attean::Literal->new(value => $value, language => $lang);
	} elsif ($dt) {
		return Attean::Literal->new(value => $value, datatype => $dt);
	} else {
		return Attean::Literal->new($value);
	}
}

my $parser	= Attean->get_parser('NTriples')->new();
isa_ok( $parser, 'AtteanX::Parser::NTriples' );

{
	my $store		= Attean->get_store('Memory')->new();
	my $ntriples	= <<"END";
	_:a <b> <a> .
	<a> <b> _:a .
END
	my @list	= $parser->parse_list_from_bytes($ntriples);
	is(scalar(@list), 2);
	my ($t1, $t2)	= @list;
	does_ok($t1, 'Attean::API::Triple');
	does_ok($t2, 'Attean::API::Triple');
	is($t1->subject->value, 'a');
	is($t2->subject->value, 'a');

	is($t1->predicate->value, 'b');
	is($t2->predicate->value, 'b');

	is($t1->object->value, 'a');
	is($t2->object->value, 'a');
}

{
	my $store	= Attean->get_store('Memory')->new();
	my $ntriples	= <<"END";
	_:a <b> <a> .
	<a> <b> _:a .
END
	my $iter	= $parser->parse_iter_from_bytes($ntriples);
	my $graph	= Attean::IRI->new('http://example.org/graph');
	my $quads	= $iter->as_quads($graph);
	$store->add_iter($quads);
	
	is( $store->size, 2, 'expected model size after ntriples parse' );
	is( $store->count_quads(blank('a')), 1, 'expected 1 count bff' );
	is( $store->count_quads(iri('a')), 1, 'expected 1 count bff' );
	is( $store->count_quads(iri('b')), 0, 'expected 0 count bff' );
	is( $store->count_quads(undef, iri('b')), 2, 'expected 2 count fbf' );
}

{
	my $ntriples	= qq[_:eve <http://example.com/resum\\u00E9> <http://example.com/resume.html> .\n];
	my @list		= $parser->parse_list_from_bytes($ntriples);
	is( scalar(@list), 1, 'expected model size after ntriples parse' );
	is($list[0]->predicate->value, 'http://example.com/resumé', 'expected 1 count fbf with unicode escaping' );
}

{
	my $ntriples	= qq[_:eve <http://example.com/resum\\u00E9> "Resume" .\n];
	my @list		= $parser->parse_list_from_bytes($ntriples);
	is( scalar(@list), 1, 'expected model size after ntriples parse' );
	is($list[0]->object->value, 'Resume', 'expected 1 count fbf with unicode escaping' );
}

{
	my %got;
	my $handler	= sub {
		my $st	= shift;
		my $o	= $st->object;
		$got{ $o->ntriples_string }++
	};
	my $ntriples	= <<"END";
_:anon <http://example.org/property> <http://example.org/resource2> .
# comment
<http://example.org/resource14> <http://example.org/property> "x" .
<http://example.org/resource16> <http://example.org/property> "\\u00E9" .

<http://example.org/resource21> <http://example.org/property> "<p/>"^^<http://www.w3.org/2000/01/rdf-schema#XMLLiteral> .
<http://example.org/resource30> <http://example.org/property> "chat"\@fr .
END
	$parser->handler($handler);
	$parser->parse_cb_from_bytes($ntriples);
	my %expect	= (
		q["é"]	=> 1,
		q["chat"@fr]	=> 1,
		q["x"]	=> 1,
		q["<p/>"^^<http://www.w3.org/2000/01/rdf-schema#XMLLiteral>]	=> 1,
		q[<http://example.org/resource2>]	=> 1,
	);
	is_deeply( \%got, \%expect, 'expected statement object parsing' );
	$parser->handler(sub {});
}

{
	# escaping tests
	{
		my $ntriples	= qq[_:a <http://example.com/string> "0\\t1" .\n];
		my ($st)		= $parser->parse_list_from_bytes($ntriples);
		is($st->object->value, "0\t1", 'expected plain literal with tab-encoding' );
	}
	{
		my $ntriples	= qq[_:a <http://example.com/string> "0\\n1" .\n];
		my ($st)		= $parser->parse_list_from_bytes($ntriples);
		is($st->object->value, "0\n1", 'expected plain literal with newline-encoding' );
	}
	{
		my $ntriples	= qq[_:a <http://example.com/string> "0\\"\\\\1" .\n];
		my ($st)		= $parser->parse_list_from_bytes($ntriples);
		is($st->object->value, qq[0"\\1], 'expected plain literal with quote and backslash-encoding' );
	}
	{
		my $ntriples	= qq[_:a <http://example.com/string> "0\\U000000611" .\n];
		my ($st)		= $parser->parse_list_from_bytes($ntriples);
		is($st->object->value, qq[0a1], 'expected plain literal with U-encoding' );
	}
}

done_testing();


sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
