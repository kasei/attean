use Test::Modern;
use Test::Roo;
use Test::Exception;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;

sub create_store {
	my $self	= shift;
	return Attean->get_store('SimpleTripleStore')->new(@_);
}

with 'Test::Attean::TripleStore';
run_me; # run these Test::Attean tests

# use Try::Tiny;
# $Error::TypeTiny::StackTrace	= 1;
# try {
{
	my $s	= Attean::Blank->new('x');
	my $p	= Attean::IRI->new('http://example.org/p1');
	my $o	= Attean::Literal->new(value => 'foo', language => 'en-US');
	my $t	= Attean::Triple->new($s, $p, $o);

	my @triples;
	push(@triples, $t);
	
	my $s2	= Attean::IRI->new('http://example.org/values');
	foreach my $value (1 .. 3) {
		my $o	= Attean::Literal->new(value => $value, datatype => 'http://www.w3.org/2001/XMLSchema#integer');
		my $p	= Attean::IRI->new("http://example.org/p$value");
		my $t	= Attean::Triple->new($s2, $p, $o);
		push(@triples, $t);
	}
	
	my $store	= Attean->get_store('SimpleTripleStore')->new( triples => \@triples );
	isa_ok($store, 'AtteanX::Store::SimpleTripleStore');

	is($store->size, 4);
	is($store->count_triples($s), 1);
	is($store->count_triples($s2), 3);
	is($store->count_triples(), 4);
	is($store->count_triples(undef, $p), 2);
	{
		my $iter	= $store->get_triples($s2);
		while (my $t = $iter->next()) {
			my $o	= $t->object->value;
			like($o, qr/^[123]$/, "Literal value: $o");
		}
	}
}
# catch {
# 	my $exception	= $_;
# 	warn "Caught error: $exception";
# 	warn $exception->stack_trace;
# };

done_testing();
