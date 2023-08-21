use strict;
use warnings;

use lib '.';
use inc::Module::Install;

name			'Attean';
all_from		'lib/Attean.pm';
author			'Gregory Todd Williams <gwilliams@cpan.org>';
license			'perl';

test_requires	'HTTP::Message::PSGI'		=> 0;
test_requires	'Regexp::Common'			=> 0;
test_requires	'Test::Exception'			=> 0;
test_requires	'Test::Requires'			=> 0;
test_requires	'Test::LWP::UserAgent'		=> 0;
test_requires	'Test::More'				=> 0.88;
test_requires	'XML::Simple'				=> 0;

perl_version	'5.014';

requires		'Algorithm::Combinatorics'	=> 0;
requires		'UUID::Tiny'				=> 0;
requires		'DateTime::Format::W3CDTF'	=> 0;
requires		'Exporter::Tiny'			=> 1.000000;
requires		'File::Slurp'				=> 0;
requires		'HTTP::Negotiate'			=> 0;
requires		'IRI'						=> 0.005;
requires		'JSON'						=> 0;
requires		'List::MoreUtils'			=> 0;
requires		'LWP::UserAgent'			=> 0;
requires		'Math::Cartesian::Product'	=> 1.008;
requires		'Module::Pluggable'			=> 0;
requires		'Moo'						=> 2.000002;
requires		'MooX::Log::Any'			=> 0;
requires		'namespace::clean'			=> 0;
requires		'PerlIO::Layers'			=> 0;
requires		'Role::Tiny'				=> 2.000003;
requires		'Set::Scalar'				=> 0;
requires		'Sub::Install'				=> 0;
requires		'Sub::Util'					=> 1.40;
requires		'Test::Modern'				=> 0.012;
requires		'Test::Moose'				=> 0;
requires		'Test::Roo'					=> 0;
requires		'Text::CSV'					=> 0;
requires		'Text::Table'				=> 0;
requires		'Try::Tiny'					=> 0;
requires		'Type::Tiny'				=> 0;
requires		'URI::Escape'				=> 1.36;
requires		'URI::NamespaceMap'			=> 0.12;
requires		'XML::SAX'					=> 0;

resources(
	'homepage'		=> "https://metacpan.org/release/Attean",
	'repository'	=> "https://github.com/kasei/attean/",
	'bugtracker'	=> "https://github.com/kasei/attean/issues",
	'IRC'			=> "irc://irc.perl.org/#perlrdf",
);

author_tests('xt');

install_script glob('bin/attean_*');

write_doap_changes "meta/changes.ttl", "Changes", "turtle";

sign if ! exists $ENV{CI};
WriteAll;
