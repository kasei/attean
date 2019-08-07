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

{
  my $uri = URI->new('http://www.example.net/');
  my $airi = to_AtteanIRI($uri);
  isa_ok($airi, 'Attean::IRI');
  is("$uri", 'http://www.example.net/', "Correct string URI from Uri");
  ok($airi->equals($atteaniri), 'Is the same URI');
}

{
  my $iri = IRI->new('http://www.example.net/');
  my $airi = to_AtteanIRI($iri);
  isa_ok($airi, 'Attean::IRI');
  is($airi->as_string, 'http://www.example.net/', "Correct string IRI from Iri");
  ok($airi->equals($atteaniri), 'Is the same IRI');
}

{
  my $nsuri = URI::Namespace->new('http://www.example.net/');
  my $airi = to_AtteanIRI($nsuri);
  isa_ok($airi, 'Attean::IRI');
  is($airi->as_string, 'http://www.example.net/', "Correct string URI from Namespace");
  ok($airi->equals($atteaniri), 'Is the same URI');
}

done_testing;
