use strict;
use warnings;
no warnings 'uninitialized';
use Test::Modern;
use Test::Exception;
use FindBin qw($Bin);
use File::Spec;
use File::Find qw(find);

use Attean;
use Attean::BindingEqualityTest;

my $ok_regex	= (@ARGV) ? shift : '';

my %XML_EXEMPTIONS	= map { $_ => 1 } qw(
/rdfms-empty-property-elements/test003.rdf
/rdfms-empty-property-elements/test009.rdf
);

our %PREFIXES	= (
	'/rdf-containers-syntax-vs-schema/test007.rdf'	=> 'd',
);

my $path	= File::Spec->catfile( $Bin, 'data', 'rdfxml-w3c' );
my @good;
my %ok	= map { $_ => 1 } qw(
/amp-in-url/test001.rdf
/datatypes/test001.rdf
/datatypes/test002.rdf
/rdf-charmod-literals/test001.rdf
/rdf-charmod-uris/test001.rdf
/rdf-charmod-uris/test002.rdf
/rdf-containers-syntax-vs-schema/test001.rdf
/rdf-containers-syntax-vs-schema/test002.rdf
/rdf-containers-syntax-vs-schema/test003.rdf
/rdf-containers-syntax-vs-schema/test004.rdf
/rdf-containers-syntax-vs-schema/test005.rdf
/rdf-containers-syntax-vs-schema/test006.rdf
/rdf-containers-syntax-vs-schema/test007.rdf
/rdf-containers-syntax-vs-schema/test008.rdf
/rdf-element-not-mandatory/test001.rdf
/rdf-ns-prefix-confusion/test0001.rdf
/rdf-ns-prefix-confusion/test0002.rdf
/rdf-ns-prefix-confusion/test0003.rdf
/rdf-ns-prefix-confusion/test0004.rdf
/rdf-ns-prefix-confusion/test0005.rdf
/rdf-ns-prefix-confusion/test0006.rdf
/rdf-ns-prefix-confusion/test0007.rdf
/rdf-ns-prefix-confusion/test0008.rdf
/rdf-ns-prefix-confusion/test0009.rdf
/rdf-ns-prefix-confusion/test0010.rdf
/rdf-ns-prefix-confusion/test0011.rdf
/rdf-ns-prefix-confusion/test0012.rdf
/rdf-ns-prefix-confusion/test0013.rdf
/rdf-ns-prefix-confusion/test0014.rdf
/rdfms-difference-between-ID-and-about/test1.rdf
/rdfms-difference-between-ID-and-about/test2.rdf
/rdfms-difference-between-ID-and-about/test3.rdf
/rdfms-duplicate-member-props/test001.rdf
/rdfms-empty-property-elements/test001.rdf
/rdfms-empty-property-elements/test002.rdf
/rdfms-empty-property-elements/test003.rdf
/rdfms-empty-property-elements/test004.rdf
/rdfms-empty-property-elements/test005.rdf
/rdfms-empty-property-elements/test006.rdf
/rdfms-empty-property-elements/test007.rdf
/rdfms-empty-property-elements/test008.rdf
/rdfms-empty-property-elements/test009.rdf
/rdfms-empty-property-elements/test010.rdf
/rdfms-empty-property-elements/test011.rdf
/rdfms-empty-property-elements/test012.rdf
/rdfms-empty-property-elements/test013.rdf
/rdfms-empty-property-elements/test014.rdf
/rdfms-empty-property-elements/test015.rdf
/rdfms-empty-property-elements/test016.rdf
/rdfms-empty-property-elements/test017.rdf
/rdfms-identity-anon-resources/test001.rdf
/rdfms-identity-anon-resources/test002.rdf
/rdfms-identity-anon-resources/test003.rdf
/rdfms-identity-anon-resources/test004.rdf
/rdfms-identity-anon-resources/test005.rdf
/rdfms-literal-is-xml-structure/test001.rdf
/rdfms-literal-is-xml-structure/test002.rdf
/rdfms-literal-is-xml-structure/test003.rdf
/rdfms-literal-is-xml-structure/test004.rdf
/rdfms-literal-is-xml-structure/test005.rdf
/rdfms-nested-bagIDs/test001.rdf
/rdfms-nested-bagIDs/test002.rdf
/rdfms-nested-bagIDs/test003.rdf
/rdfms-nested-bagIDs/test004.rdf
/rdfms-nested-bagIDs/test005.rdf
/rdfms-nested-bagIDs/test006.rdf
/rdfms-nested-bagIDs/test008.rdf
/rdfms-nested-bagIDs/test009.rdf
/rdfms-nested-bagIDs/test010a.rdf
/rdfms-nested-bagIDs/test010b.rdf
/rdfms-nested-bagIDs/test011a.rdf
/rdfms-nested-bagIDs/test011b.rdf
/rdfms-nested-bagIDs/test012a.rdf
/rdfms-nested-bagIDs/test012b.rdf
/rdfms-not-id-and-resource-attr/test001.rdf
/rdfms-not-id-and-resource-attr/test002.rdf
/rdfms-not-id-and-resource-attr/test003.rdf
/rdfms-not-id-and-resource-attr/test004.rdf
/rdfms-not-id-and-resource-attr/test005.rdf
/rdfms-para196/test001.rdf
/rdfms-rdf-names-use/test-001.rdf
/rdfms-rdf-names-use/test-002.rdf
/rdfms-rdf-names-use/test-003.rdf
/rdfms-rdf-names-use/test-004.rdf
/rdfms-rdf-names-use/test-005.rdf
/rdfms-rdf-names-use/test-006.rdf
/rdfms-rdf-names-use/test-007.rdf
/rdfms-rdf-names-use/test-008.rdf
/rdfms-rdf-names-use/test-009.rdf
/rdfms-rdf-names-use/test-010.rdf
/rdfms-rdf-names-use/test-011.rdf
/rdfms-rdf-names-use/test-012.rdf
/rdfms-rdf-names-use/test-013.rdf
/rdfms-rdf-names-use/test-014.rdf
/rdfms-rdf-names-use/test-015.rdf
/rdfms-rdf-names-use/test-016.rdf
/rdfms-rdf-names-use/test-017.rdf
/rdfms-rdf-names-use/test-018.rdf
/rdfms-rdf-names-use/test-019.rdf
/rdfms-rdf-names-use/test-020.rdf
/rdfms-rdf-names-use/test-021.rdf
/rdfms-rdf-names-use/test-022.rdf
/rdfms-rdf-names-use/test-023.rdf
/rdfms-rdf-names-use/test-024.rdf
/rdfms-rdf-names-use/test-025.rdf
/rdfms-rdf-names-use/test-026.rdf
/rdfms-rdf-names-use/test-027.rdf
/rdfms-rdf-names-use/test-028.rdf
/rdfms-rdf-names-use/test-029.rdf
/rdfms-rdf-names-use/test-030.rdf
/rdfms-rdf-names-use/test-031.rdf
/rdfms-rdf-names-use/test-032.rdf
/rdfms-rdf-names-use/test-033.rdf
/rdfms-rdf-names-use/test-034.rdf
/rdfms-rdf-names-use/test-035.rdf
/rdfms-rdf-names-use/test-036.rdf
/rdfms-rdf-names-use/test-037.rdf
/rdfms-rdf-names-use/warn-001.rdf
/rdfms-rdf-names-use/warn-002.rdf
/rdfms-rdf-names-use/warn-003.rdf
/rdfms-reification-required/test001.rdf
/rdfms-seq-representation/test001.rdf
/rdfms-syntax-incomplete/test001.rdf
/rdfms-syntax-incomplete/test002.rdf
/rdfms-syntax-incomplete/test003.rdf
/rdfms-syntax-incomplete/test004.rdf
/rdfms-uri-substructure/test001.rdf
/rdfms-xml-literal-namespaces/test001.rdf
/rdfms-xml-literal-namespaces/test002.rdf
/rdfms-xmllang/test001.rdf
/rdfms-xmllang/test002.rdf
/rdfms-xmllang/test003.rdf
/rdfms-xmllang/test004.rdf
/rdfms-xmllang/test005.rdf
/rdfms-xmllang/test006.rdf
/rdfs-domain-and-range/test001.rdf
/rdfs-domain-and-range/test002.rdf
/rdfs-domain-and-range/test003.rdf
/rdfs-domain-and-range/test004.rdf
/unrecognised-xml-attributes/test001.rdf
/unrecognised-xml-attributes/test002.rdf
/xml-canon/test001.rdf
/xmlbase/test001.rdf
/xmlbase/test002.rdf
/xmlbase/test003.rdf
/xmlbase/test004.rdf
/xmlbase/test005.rdf
/xmlbase/test006.rdf
/xmlbase/test007.rdf
/xmlbase/test008.rdf
/xmlbase/test009.rdf
/xmlbase/test010.rdf
/xmlbase/test011.rdf
/xmlbase/test012.rdf
/xmlbase/test013.rdf
/xmlbase/test014.rdf
/xmlbase/test015.rdf
/xmlbase/test016.rdf
);
find( sub {
	if ($File::Find::name =~ /^(.*)[.]rdf/) {
		my $prefix	= $1;
		my ($file)	= $File::Find::name =~ m/rdfxml-w3c(.*)$/;
		if ($ok{ $file }) {
			my $expect	= "${prefix}.nt";
			if (-r $expect) {
				push(@good, $File::Find::name);
			}
		}
	}
}, $path );

my $s	= Attean->get_serializer('NTriples')->new();
foreach my $file (@good) {
	TODO: {
		my (undef, undef, $filename)	= File::Spec->splitpath( $file );
		if ($file =~ $ok_regex) {
			local($TODO)	= 'rdf/xml parser is currently broken' if ($file =~ m/ex-(19|37|45|46|53|58)/);
			my ($name)	= $file =~ m<^.*rdfxml-w3c(.*)[.]rdf>;
			note($name);
			my (undef, undef, $test)	= File::Spec->splitpath( $file );
			{
				open( my $fh, '<:', $file ) or die $!;
				my $iter;
				lives_ok {
					my ($filename)	= $file	=~ m/rdfxml-w3c(.*)$/;
					my $url	= 'http://www.w3.org/2000/10/rdf-tests/rdfcore' . $filename;
					my $prefix	= $PREFIXES{ $filename } || 'genid';

					my $parser = Attean->get_parser('RDFXML')->new(base => $url);
					$iter		= $parser->parse_iter_from_io($fh);
				} "parsing $name with parse_iter_from_io lives";
				
				compare( $iter, $file );
			}
		}
	}
}

done_testing();

sub compare {
	my $iter		= shift;
	my $file		= shift;
	my ($filename)	= $file	=~ m/rdfxml-w3c(.*)$/;
	my ($name)		= $file =~ m<^.*rdfxml-w3c(.*)[.]rdf>;
	$file			=~ s/[.]rdf$/.nt/;
	my $parser		= Attean->get_parser('NTriples')->new();
	open( my $fh, '<', $file ) or die $!;
	my $nt_iter		= $parser->parse_iter_from_io($fh);

	my $test	= Attean::BindingEqualityTest->new();
	ok( $test->equals( $iter, $nt_iter ), "graph equality: $file" ) or diag($test->error);
}
