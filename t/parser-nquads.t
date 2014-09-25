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

my $parser	= Attean->get_parser('NQuads')->new();
isa_ok( $parser, 'AtteanX::Parser::NQuads' );

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
	my $nquads	= <<"END";
	_:a <b> <a> .
	<a> <b> _:a <g> .
	<a> <b> _:a _:graph .
END
	my $iter	= $parser->parse_iter_from_bytes($nquads);
	my $graph	= Attean::IRI->new('http://example.org/default');
	my $quads	= $iter->map(
		sub { $_->does('Attean::API::Quad') ? $_ : $_->as_quad($graph) },
		'Attean::API::Quad'
	);
	$store->add_iter($quads);
	
	is( $store->size, 3, 'expected model size after ntriples parse' );
	is( $store->count_quads(blank('a')), 1, 'expected 1 count bfff' );
	is( $store->count_quads(iri('a')), 2, 'expected 2 count bfff' );
	is( $store->count_quads(iri('b')), 0, 'expected 0 count bfff' );
	is( $store->count_quads(undef, iri('b')), 3, 'expected 2 count fbff' );
	is( $store->count_quads(undef, undef, undef, iri('g')), 1, 'expected 1 count fffb' );
	is( $store->count_quads(undef, undef, undef, blank('graph')), 1, 'expected 1 count fffb' );
	is( $store->count_quads(undef, undef, undef, iri('http://example.org/default')), 1, 'expected 1 count fffb' );
}

{
	my $store	= Attean->get_store('Memory')->new();
	my $nquads	= <<"END";
	_:a <b> <a> <g1> .
	<a> <b> _:a <g2> .
END
	open(my $fh, '<', \$nquads);
	my @list	= $parser->parse_list_from_io($fh);
	is(scalar(@list), 2);
	my ($t1, $t2)	= @list;
	does_ok($t1, 'Attean::API::Quad');
	does_ok($t2, 'Attean::API::Quad');
	is($t1->subject->value, 'a');
	is($t2->subject->value, 'a');

	is($t1->predicate->value, 'b');
	is($t2->predicate->value, 'b');

	is($t1->object->value, 'a');
	is($t2->object->value, 'a');

	is($t1->graph->value, 'g1');
	is($t2->graph->value, 'g2');
}

{
	my $store	= Attean->get_store('Memory')->new();
	my $nquads	= <<"END";
	_:a <b> <a> <g1> .
	<a> <b> _:a <g2> .
END
	open(my $fh, '<', \$nquads);
	my $counter	= 0;
	my $parser	= Attean->get_parser('NQuads')->new(handler => sub {
		$counter++;
		my $q	= shift;
		does_ok($q, 'Attean::API::Quad');
	});
	$parser->parse_cb_from_io($fh);
	is($counter, 2);
}

done_testing();

sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
