#!/usr/bin/env perl

use strict;
use warnings;
use Test::More;
use Attean;
#use Test::Requires { 'Attean::IRI' => '0.023' };
use Types::URI qw( to_Uri to_Iri );
use Types::Namespace qw( to_Namespace );
#use Types::Attean qw(to_AtteanIri);
use Attean::IRI;

subtest 'Coercions to other URIs' => sub {
  my $atteaniri = Attean::IRI->new('http://www.example.net/');
  
  my $uri = to_Uri($atteaniri);
  isa_ok($uri, 'URI');
  is("$uri", 'http://www.example.net/', "Correct string URI");
 
  my $iri = to_Iri($atteaniri);
  isa_ok($iri, 'IRI');
  is($iri->as_string, 'http://www.example.net/', "Correct string URI");
  
  my $nsuri = to_Namespace($atteaniri);
  isa_ok($nsuri, 'Namespace');
  is($nsuri->as_string, 'http://www.example.net/', "Correct string URI");
};

done_testing;
