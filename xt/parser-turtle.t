use v5.14;
use autodie;
use utf8;
use Test::More;
use Test::Exception;
use FindBin qw($Bin);
use File::Glob qw(bsd_glob);
use File::Spec;

use Attean;
use Type::Tiny::Role;

my $path	= File::Spec->catfile( $Bin, 'data', 'turtle' );
my @good	= bsd_glob("${path}/test*.ttl");
my @bad		= bsd_glob("${path}/bad*.ttl");

foreach my $file (@good) {
	my $data	= do { open( my $fh, '<', $file ); local($/) = undef; <$fh> };
	my (undef, undef, $test)	= File::Spec->splitpath( $file );
	lives_ok {
		my $parser	= Attean->get_parser('Turtle')->new( handler => sub {
			my $t	= shift;
# 			warn "parsed triple: " . $t->tuples_string . "\n";
		} );
		open(my $fh, '<', $file);
		$parser->parse_cb_from_io($fh);
	} $test;
}

# warn "------------------------------------------------------\n";

foreach my $file (@bad) {
	my $data	= do { open( my $fh, '<', $file ); local($/) = undef; <$fh> };
	my (undef, undef, $test)	= File::Spec->splitpath( $file );
	dies_ok {
		my $parser	= Attean->get_parser('Turtle')->new();
		open(my $fh, '<', $file);
		my $url	= 'file://' . $file;
		$parser->parse_cb_from_io($fh);
	} $test;
}

done_testing();

sub does_ok {
    my ($class_or_obj, $does, $message) = @_;
    $message ||= "The object does $does";
    ok(eval { $class_or_obj->does($does) }, $message);
}
