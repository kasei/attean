use Test::More;
use Test::Exception;
use Test::Moose;

use v5.14;
use warnings;
no warnings 'redefine';

use RDF;

{
	note('ListIterator[RDF::Triple]');
	my $s	= RDF::Blank->new('x');
	my $p	= RDF::IRI->new('http://example.org/p');
	my $o1	= RDF::Literal->new(value => '1', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
	my $o2	= RDF::Literal->new(value => '2', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
	
	my $t1	= RDF::Triple->new($s, $p, $o1);
	my $t2	= RDF::Triple->new($s, $p, $o2);
	my $i	= RDF::ListIterator->new(values => [$t1, $t2], item_type => Moose::Meta::TypeConstraint::Role->new(role => 'RDF::API::Triple'));
	does_ok($i, 'RDF::API::Iterator');
	isa_ok($i, 'RDF::ListIterator');
	
	my $x1	= $i->next;
	does_ok($x1, 'RDF::API::Triple');
	
	my $x2	= $i->next;
	does_ok($x2, 'RDF::API::Triple');
	
	is($i->next, undef, 'eof');
}

{
	note('ListIterator[Quad != Triple]');
	my $s	= RDF::Blank->new('x');
	my $p	= RDF::IRI->new('http://example.org/p');
	my $o	= RDF::Literal->new(value => '1', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
	my $g	= RDF::IRI->new('http://example.org/g');
	my $q	= RDF::Quad->new($s, $p, $o, $g);
	dies_ok {
		my $i	= RDF::ListIterator->new(values => [$q], item_type => Moose::Meta::TypeConstraint::Role->new(role => 'RDF::API::Triple'));
	};
}

{
	note('ListIterator[Int != Triple]');
	dies_ok {
		my $i	= RDF::ListIterator->new(values => [1, 2, 3], item_type => Moose::Meta::TypeConstraint::Role->new(role => 'RDF::API::Triple'));
	};
}



done_testing();