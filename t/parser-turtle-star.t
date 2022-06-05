use v5.14;
use autodie;
use utf8;
use Test::Modern;
use Test::Exception;
use FindBin qw($Bin);
use File::Glob qw(bsd_glob);
use File::Spec;

use Attean;
use Attean::RDF;
use AtteanX::Parser::Turtle;
use AtteanX::Parser::Turtle::Constants;
use Type::Tiny::Role;

subtest 'Turtle-star quoted triples' => sub {
	my $turtle = <<"END";
BASE <http://example.org/>
PREFIX : <#> 
_:a :name "Alice" .
<< _:a :name "Alice" >> :statedBy :bob .
END
	open(my $fh, '<', \$turtle);
	my $parser	= Attean->get_parser('Turtle')->new();
	my $iter	= $parser->parse_iter_from_io($fh);
	does_ok($iter, 'Attean::API::Iterator');
	
	my $t1	= $iter->next;
	my $t2	= $iter->next;

	is($t1->object->value, 'Alice');
	is($t2->object->value, 'http://example.org/#bob');

	my $qt	= $t2->subject;
	ok($qt->does('Attean::API::Triple'));
	is($qt->object->value, 'Alice');
};

subtest 'Turtle-star annotated triples' => sub {
	my $turtle = <<"END";
PREFIX : <http://example/>

:s :p :o {| :r :z |} .
END
	open(my $fh, '<', \$turtle);
	my $parser	= Attean->get_parser('Turtle')->new();
	my $iter	= $parser->parse_iter_from_io($fh);
	does_ok($iter, 'Attean::API::Iterator');
	
	my $t1	= $iter->next;
	my $t2	= $iter->next;

	is($t1->object->value, 'http://example/o');
	is($t2->object->value, 'http://example/z');

	my $qt	= $t2->subject;
	ok($qt->does('Attean::API::Triple'));
	is($qt->object->value, 'http://example/o');
};

done_testing();

sub expect {
	my $token	= shift;
	my $type	= shift;
	my $values	= shift;
	my $name	= shift // '';
	if (length($name)) {
		$name	= "${name}: ";
	}
	is($token->type, $type, "${name}token type");
	is_deeply($token->args, $values, "${name}token values");
}
