#!/usr/bin/env perl

use strict;
use warnings;
use Test::More;
use Attean;
use Test::Requires { 'Attean::IRI' => '0.023' };
use Types::URI qw( to_Uri to_Iri );
use Types::Namespace qw( to_Namespace );
use Types::Attean qw(to_AtteanIRI);
use Attean::IRI;

my $atteaniri = Attean::IRI->new('http://www.example.net/');

{
  my $uri = to_Uri($atteaniri);
  isa_ok($uri, 'URI');
  is("$uri", 'http://www.example.net/', "Correct string URI to Uri");
  
  my $iri = to_Iri($atteaniri);
  isa_ok($iri, 'IRI');
  is($iri->as_string, 'http://www.example.net/', "Correct string URI to Iri");
  
  my $nsuri = to_Namespace($atteaniri);
  isa_ok($nsuri, 'URI::Namespace');
  is($nsuri->as_string, 'http://www.example.net/', "Correct string URI to Namespace");
}

_test_to_attean(URI->new('http://www.example.net/'));

_test_to_attean(IRI->new('http://www.example.net/'));

_test_to_attean(URI::Namespace->new('http://www.example.net/'));

_test_to_attean('http://www.example.net/');

sub _test_to_attean {
  my $uri = shift;
  my $airi = to_AtteanIRI($uri);
  isa_ok($airi, 'Attean::IRI');
  is($airi->as_string, 'http://www.example.net/', 'Correct string URI from ' . ref($uri));
  ok($airi->equals($atteaniri), 'Is the same URI');

  # TODO: Something like this should work too?
  # my $aciri = Attean::IRI->new($uri); 
  # isa_ok($aciri, 'Attean::IRI');
  # is($aciri->as_string, 'http://www.example.net/', 'Correct string URI from ' . ref($uri));
  # ok($aciri->equals($atteaniri), 'Is the same URI');
}
  
done_testing;
