use v5.14;
use warnings;
use autodie;
use Test::More;
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

{
	my $parser	= Attean->get_parser('SPARQLXML')->new();
	isa_ok( $parser, 'AtteanX::Parser::SPARQLXML' );
}

{
	my $xml	= <<'END';
<?xml version="1.0"?>
<sparql xmlns="http://www.w3.org/2005/sparql-results#">
  <head>
    <variable name="x"/>
    <variable name="hpage"/>
    <variable name="name"/>
    <variable name="age"/>
    <variable name="mbox"/>
    <variable name="friend"/>
  </head>
  <results>
    <result>
      <binding name="x">
	<bnode>r2</bnode>
      </binding>
      <binding name="hpage">
	<uri>http://work.example.org/bob/</uri>
      </binding>
      <binding name="name">
	<literal xml:lang="en">Bob</literal>
      </binding>
      <binding name="age">
	<literal datatype="http://www.w3.org/2001/XMLSchema#integer">30</literal>
      </binding>
      <binding name="mbox">
	<uri>mailto:bob@work.example.org</uri>
      </binding>
    </result>
  </results>
</sparql>
END
	my $counter	= 0;
	my $parser	= Attean->get_parser('SPARQLXML')->new(handler => sub {
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
	$parser->parse_cb_from_bytes($xml);
}

{
	my $xml	= <<'END';
<?xml version="1.0"?>
<sparql xmlns="http://www.w3.org/2005/sparql-results#">
	<head>
		<variable name="x"/>
		<variable name="name"/>
	</head>
	<results>
		<result>
			<binding name="x"><bnode>r2</bnode></binding>
			<binding name="name"><literal xml:lang="en">Bob</literal></binding>
		</result>
		<result>
			<binding name="x"><uri>http://example.org/eve</uri></binding>
			<binding name="name"><literal>Eve</literal></binding>
		</result>
	</results>
</sparql>
END
	open(my $fh, '<', \$xml);
	my $counter	= 0;
	my $parser	= Attean->get_parser('SPARQLXML')->new(handler => sub {});
	my @results	= $parser->parse_list_from_io($fh);
	is(scalar(@results), 2);
}

done_testing();


sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
