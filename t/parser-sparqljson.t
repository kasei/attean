use v5.14;
use warnings;
use autodie;
use Test::Modern;
use utf8;

use Attean;

sub iri { Attean::IRI->new(shift) }
sub blank { Attean::Blank->new(shift) }
sub literal {
	my ($value, $lang, $dt)	= @_;
	if ($lang) {
		return Attean::Literal->new(value => $value, language => $lang);
	} elsif ($dt) {
		return Attean::Literal->new(value => $value, datatype => $dt);
	} else {
		return Attean::Literal->new($value);
	}
}

subtest 'parser construction and metadata' => sub {
	my $parser	= Attean->get_parser('SPARQLJSON')->new();
	isa_ok($parser, 'AtteanX::Parser::SPARQLJSON');
	is($parser->canonical_media_type, 'application/sparql-results+json', 'canonical_media_type');
	my %extensions	= map { $_ => 1 } @{ $parser->file_extensions };
	ok(exists $extensions{'srj'}, 'file_extensions');
	my $type	= $parser->handled_type;
	can_ok($type, 'role');
	is($type->role, 'Attean::API::ResultOrTerm');
};

{
	my $json	= <<'END';
{
  "head": { "vars": [ "x", "hpage", "name", "age", "mbox", "friend" ]
  } ,
  "results": { 
    "bindings": [
      {
        "x": { "type": "bnode" , "value": "r2" } ,
        "hpage": { "type": "uri" , "value": "http://work.example.org/bob/" } ,
        "name": { "type": "literal" , "value": "Bob", "xml:lang": "en" } ,
        "age": { "type": "literal" , "value": "30", "datatype": "http://www.w3.org/2001/XMLSchema#integer" } ,
        "mbox": { "type": "uri" , "value": "mailto:bob@work.example.org" }
      }
    ]
  }
}
END
	my $counter	= 0;
	my $parser	= Attean->get_parser('SPARQLJSON')->new(handler => sub {
		$counter++;
		my $result	= shift;
		does_ok($result, 'Attean::API::Result');
		my @vars	= $result->variables;
		is_deeply([sort @vars], [qw(age hpage mbox name x)]);
		my $x	= $result->value('x');
		does_ok($x, 'Attean::API::Blank');
		is($x->value, 'r2');
		
		my $age	= $result->value('age');
		does_ok($age, 'Attean::API::Literal');
		is($age->value, '30');
		is($age->datatype->value, 'http://www.w3.org/2001/XMLSchema#integer');
		
		my $hpage	= $result->value('hpage');
		does_ok($hpage, 'Attean::API::IRI');
		is($hpage->value, 'http://work.example.org/bob/');
	});
	$parser->parse_cb_from_bytes($json);
}

{
	my $tsv	= <<'END';
{
  "head": { "vars": [ "x", "name" ]
  } ,
  "results": { 
    "bindings": [
      {
        "x": { "type": "bnode" , "value": "r2" } ,
        "name": { "type": "literal" , "value": "Bob", "xml:lang": "en" }
      },
      {
        "x": { "type": "uri" , "value": "http://example.org/eve" } ,
        "name": { "type": "literal" , "value": "Eve" }
      }
    ]
  }
}
END
	open(my $fh, '<', \$tsv);
	my $counter	= 0;
	my $parser	= Attean->get_parser('SPARQLJSON')->new(handler => sub {});
	my @results	= $parser->parse_list_from_io($fh);
	is(scalar(@results), 2);
}

done_testing();
