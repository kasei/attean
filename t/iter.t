use Test::More;
use Test::Exception;
use Test::Moose;

use v5.14;
use warnings;
no warnings 'redefine';

use Attean;

{
	note('ListIterator[Attean::Triple]');
	my $s	= Attean::Blank->new('x');
	my $p	= Attean::IRI->new('http://example.org/p');
	my $o1	= Attean::Literal->new(value => '1', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
	my $o2	= Attean::Literal->new(value => '2', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
	
	my $t1	= Attean::Triple->new($s, $p, $o1);
	my $t2	= Attean::Triple->new($s, $p, $o2);
	my $i	= Attean::ListIterator->new(values => [$t1, $t2], item_type => Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Triple'));
	does_ok($i, 'Attean::API::Iterator');
	isa_ok($i, 'Attean::ListIterator');
	
	my $x1	= $i->next;
	does_ok($x1, 'Attean::API::Triple');
	
	my $x2	= $i->next;
	does_ok($x2, 'Attean::API::Triple');
	
	is($i->next, undef, 'eof');
}

{
	note('ListIterator[Quad != Triple]');
	my $s	= Attean::Blank->new('x');
	my $p	= Attean::IRI->new('http://example.org/p');
	my $o	= Attean::Literal->new(value => '1', datatype => 'http://www.w3.org/2001/XMLSchema#integer');
	my $g	= Attean::IRI->new('http://example.org/g');
	my $q	= Attean::Quad->new($s, $p, $o, $g);
	dies_ok {
		my $i	= Attean::ListIterator->new(values => [$q], item_type => Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Triple'));
	};
}

{
	note('ListIterator[Int != Triple]');
	dies_ok {
		my $i	= Attean::ListIterator->new(values => [1, 2, 3], item_type => Moose::Meta::TypeConstraint::Role->new(role => 'Attean::API::Triple'));
	};
}



done_testing();