use strict;
use warnings;

use Test::More tests => 4;
use Attean;
use Attean::IRI;

my $sclass = Attean->get_serializer('SPARQLHTML');

my $map = URI::NamespaceMap->new( { 
                                    foaf => 'http://xmlns.com/foaf/0.1/'
                                  });

my $n1 = Attean::IRI->new('http://xmlns.com/foaf/0.1/Person');
my $n2 = Attean::IRI->new('http://www.w3.org/1999/02/22-rdf-syntax-ns#type');
	
NAMESPACEMAP:{

	my $s = $sclass->new(namespaces => $map);

	is ($s->node_as_html($n1), '<a href="http://xmlns.com/foaf/0.1/Person">foaf:Person</a>', 'Return HTML link for IRI with abbrev');
	is ($s->node_as_html($n2), '<a href="http://www.w3.org/1999/02/22-rdf-syntax-ns#type">http://www.w3.org/1999/02/22-rdf-syntax-ns#type</a>', 'Return HTML link for IRI - 1');

}

NO_NAMESPACEMAP:{

	my $s = $sclass->new();
	
	is ($s->node_as_html($n1), '<a href="http://xmlns.com/foaf/0.1/Person">http://xmlns.com/foaf/0.1/Person</a>', 'Return HTML link for IRI - 2');
	is ($s->node_as_html($n2), '<a href="http://www.w3.org/1999/02/22-rdf-syntax-ns#type">http://www.w3.org/1999/02/22-rdf-syntax-ns#type</a>', 'Return HTML link for IRI - 3');

}

