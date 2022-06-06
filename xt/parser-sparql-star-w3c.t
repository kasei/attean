use strict;
use warnings;
no warnings 'uninitialized';
use Test::Modern;
use Test::Exception;
use FindBin qw($Bin);
use File::Spec;
use File::Find qw(find);
use Data::Dumper;

use Attean;
use Attean::BindingEqualityTest;

my $ok_regex	= (@ARGV) ? shift : '';

my $path	= File::Spec->catfile( $Bin, 'data', 'rdf-star', 'sparql', 'syntax' );
my @good;
my @bad;
find( sub {
	return if ($File::Find::name =~ /^manifest[.]rq/);
	if ($File::Find::name =~ /^(.*)[.]rq/) {
		my $prefix	= $1;
		my ($file)	= $File::Find::name =~ m{rdf-star/sparql/syntax(.*)$};
		if ($file =~ /\bbad\b/) {
			push(@bad, $File::Find::name);
		} else {
			push(@good, $File::Find::name);
		}
	}
}, $path );

foreach my $file (@good) {
	TODO: {
		my (undef, undef, $filename)	= File::Spec->splitpath( $file );
		if ($file =~ $ok_regex) {
			my ($name)	= $filename;
			note($name);
			my (undef, undef, $test)	= File::Spec->splitpath( $file );
			{
				open( my $fh, '<:', $file ) or die $!;
				my $iter;
				lives_ok {
					my ($filename)	= $file;
					my $url	= 'https://github.com/w3c/rdf-star/blob/main/tests/sparql/syntax/' . $filename;

					my $parser = Attean->get_parser('SPARQL')->new(base => $url);
					$iter		= $parser->parse_iter_from_io($fh);
				} "parsing $name with parse_iter_from_io lives";
			}
		}
	}
}

foreach my $file (@bad) {
	TODO: {
		my (undef, undef, $filename)	= File::Spec->splitpath( $file );
		if ($file =~ $ok_regex) {
			my ($name)	= $filename;
			note($name);
			my (undef, undef, $test)	= File::Spec->splitpath( $file );
			{
				open( my $fh, '<:', $file ) or die $!;
				my $iter;
				dies_ok {
					my ($filename)	= $file;
					my $url	= 'https://github.com/w3c/rdf-star/blob/main/tests/sparql/syntax/' . $filename;

					my $parser = Attean->get_parser('SPARQL')->new(base => $url);
					$iter		= $parser->parse_iter_from_io($fh);
				} "parsing $name with parse_iter_from_io dies";
			}
		}
	}
}

done_testing();

# sub compare {
# 	my $iter		= shift;
# 	my $file		= shift;
# 	my ($filename)	= $file	=~ m/rdfxml-w3c(.*)$/;
# 	my ($name)		= $file =~ m<^.*rdfxml-w3c(.*)[.]rdf>;
# 	$file			=~ s/[.]rdf$/.nt/;
# 	my $parser		= Attean->get_parser('NTriples')->new();
# 	open( my $fh, '<', $file ) or die $!;
# 	my $nt_iter		= $parser->parse_iter_from_io($fh);
# 
# 	my $test	= Attean::BindingEqualityTest->new();
# 	ok( $test->equals( $iter, $nt_iter ), "graph equality: $file" ) or diag($test->error);
# }
