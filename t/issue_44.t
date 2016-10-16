use strict;
use warnings;

use Test::More tests => 3;

use v5.14;
use Attean;

my $ns  = {foaf => 'http://xmlns.com/foaf/0.1/'};
my $map = URI::NamespaceMap->new($ns);

my $value = Attean::ValueExpression->new(value => Attean::IRI->new('http://xmlns.com/foaf/0.1/person'));

is ($value->as_sparql(namespaces => $map), <<IRI, 'as_sparql with URI::NamespaceMap object passed as namespace value');
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
foaf:person
IRI

is ($value->as_sparql(namespaces => $map), <<IRI, 'as_sparql with HASH ref passed as namespace value');
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
foaf:person
IRI

is ($value->as_sparql(),<<IRI, 'as_sparql without arguments');
<http://xmlns.com/foaf/0.1/person>
IRI

