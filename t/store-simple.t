use Test::More;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;

# use Try::Tiny;
# $Error::TypeTiny::StackTrace	= 1;
# try {
{
	my $s	= Attean::Blank->new('x');
	my $p	= Attean::IRI->new('http://example.org/p1');
	my $o	= Attean::Literal->new(value => 'foo', language => 'en-US');
	my $g	= Attean::IRI->new('http://example.org/graph');
	my $q	= Attean::Quad->new($s, $p, $o, $g);

	my @quads;
	push(@quads, $q);
	
	my $s2	= Attean::IRI->new('http://example.org/values');
	foreach my $value (1 .. 3) {
		my $o	= Attean::Literal->new(value => $value, datatype => 'http://www.w3.org/2001/XMLSchema#integer');
		my $p	= Attean::IRI->new("http://example.org/p$value");
		my $q	= Attean::Quad->new($s2, $p, $o, $g);
		push(@quads, $q);
	}
	
	my $store	= Attean->get_store('Simple')->new( quads => \@quads );
	isa_ok($store, 'AtteanX::Store::Simple');

	is($store->size, 4);
	is($store->count_quads($s), 1);
	is($store->count_quads($s2), 3);
	is($store->count_quads(), 4);
	is($store->count_quads(undef, $p), 2);
	{
		my $iter	= $store->get_quads($s2);
		while (my $q = $iter->next()) {
			my $o	= $q->object->value;
			like($o, qr/^[123]$/, "Literal value: $o");
		}
	}
	
	my $iter	= $store->get_graphs;
	my @graphs	= $iter->elements;
	is(scalar(@graphs), 1);
	is($graphs[0]->value, 'http://example.org/graph');
}
# catch {
# 	my $exception	= $_;
# 	warn "Caught error: $exception";
# 	warn $exception->stack_trace;
# };

done_testing();


sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
