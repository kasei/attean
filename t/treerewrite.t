use v5.14;
use autodie;
use utf8;
use Test::More;
use Test::Exception;
use Data::Dumper;

use Attean;
use Attean::RDF;
use Attean::TreeRewriter;

{
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);
	
	{
		my $w	= Attean::TreeRewriter->new();
		$w->register_pre_handler(sub { return (0, 1, shift); });
		isa_ok($w, 'Attean::TreeRewriter');
		my ($changed, $rewritten)	= $w->rewrite($bgp, {});
		ok(not($changed), 'not changed');
	}
	
	{
		my $w	= Attean::TreeRewriter->new();
		$w->register_pre_handler(sub {
			my ($t, $parent, $thunk)	= @_;
			return (0, 1, shift);
		});
		isa_ok($w, 'Attean::TreeRewriter');
		my ($changed, $rewritten)	= $w->rewrite($bgp, {});
		ok(not($changed), 'not changed');
	}
	
	{
		my $w	= Attean::TreeRewriter->new(types => []);
		my $seen	= 0;
		$w->register_pre_handler(sub {
			my ($t, $parent, $thunk)	= @_;
			$seen++;
			return (0, 1, shift);
		});
		isa_ok($w, 'Attean::TreeRewriter');
		my ($changed, $rewritten)	= $w->rewrite($bgp, {});
		ok(not($changed), 'not changed');
		ok(not($seen), 'tree not walked');
	}
	
	{
		my $w	= Attean::TreeRewriter->new(types => ['Attean::API::DirectedAcyclicGraph', 'Attean::API::Binding']);
		$w->register_pre_handler(sub {
			my ($t, $parent, $thunk)	= @_;
			if ($t->isa('Attean::Triple')) {
				my $s	= $t->subject;
				if ($s->value =~ /s$/) {
					my $new	= triple(iri('x'), iri('y'), iri('z'));
					return (1, 0, $new);
				}
			}
			return (0, 1, shift);
		});
		isa_ok($w, 'Attean::TreeRewriter');
		my ($changed, $rewritten)	= $w->rewrite($bgp, {});
		ok($changed, 'changed');
		isa_ok($rewritten, 'Attean::Algebra::BGP');
		my @triples	= @{ $rewritten->triples };
		is(scalar(@triples), 1, 'triple count');
		my ($t)	= @triples;
		isa_ok($t, 'Attean::Triple');
		is($t->as_string, '<x> <y> <z> .');
	}
	
	{
		my $w	= Attean::TreeRewriter->new(types => ['Attean::API::DirectedAcyclicGraph', 'Attean::API::Binding', 'Attean::API::Literal']);
		$w->register_pre_handler(sub {
			my ($t, $parent, $thunk)	= @_;
			if ($t->isa('Attean::Literal')) {
				my $value	= 1 + $t->value;
				my $new		= literal($value);
				return (1, 0, $new);
			}
			return (0, 1, $t);
		});
		isa_ok($w, 'Attean::TreeRewriter');
		my ($changed, $rewritten)	= $w->rewrite($bgp, {});
		ok($changed, 'changed');
		isa_ok($rewritten, 'Attean::Algebra::BGP');
		my @triples	= @{ $rewritten->triples };
		is(scalar(@triples), 1, 'triple count');
		my ($t)	= @triples;
		isa_ok($t, 'Attean::Triple');
		is($t->as_string, '<s> <p> "2" .');
	}
}

{
	# rewrite iris s/^p/Z/ (e.g. <p1> -> <Z1>; <p> -> <Z>)
	my $p1	= iri('p1');
	my $pp1	= Attean::Algebra::PredicatePath->new( predicate => $p1 );
	my $p2	= iri('p2');
	my $pp2	= Attean::Algebra::PredicatePath->new( predicate => $p2 );

	my $nps	= Attean::Algebra::NegatedPropertySet->new( predicates => [$p1, $p2] );
	
	my $seq	= Attean::Algebra::SequencePath->new( children => [$pp1, $pp2] );

	my $alt	= Attean::Algebra::AlternativePath->new( children => [$pp1, $pp2] );

	my $inv_seq	= Attean::Algebra::InversePath->new( children => [$seq] );
	my $inv_seq_star	= Attean::Algebra::ZeroOrMorePath->new( children => [$inv_seq] );
	
	my $t	= triple(iri('s'), iri('p'), literal('1'));
	my $bgp	= Attean::Algebra::BGP->new(triples => [$t]);

	my $join	= Attean::Algebra::Join->new( children => [$bgp, $inv_seq_star, $alt, $nps] );
	my $dist	= Attean::Algebra::Distinct->new( children => [$join] );
	
	my $w	= Attean::TreeRewriter->new(types => ['Attean::API::DirectedAcyclicGraph', 'Attean::API::Binding', 'Attean::API::TermOrVariable']);
	$w->register_pre_handler(sub {
		my ($t, $parent, $thunk)	= @_;
		if ($t->isa('Attean::IRI')) {
			if ($t->value =~ /^p(.*)$/) {
				my $value	= $t->value;
				my $new		= iri("Z$1");
				return (1, 0, $new);
			}
		}
		return (0, 1, shift);
	});
	my ($changed, $rewritten)	= $w->rewrite($dist, {});
	ok($changed, 'changed');
	my $string	= $rewritten->as_string;
	like($string, qr/<s> <Z> "1"/);
	like($string, qr/Property Path <Z1>/);
	like($string, qr/Property Path <Z2>/);
}

done_testing();
