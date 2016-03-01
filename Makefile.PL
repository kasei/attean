use strict;
use warnings;

use inc::Module::Install;

name			'Attean';
all_from		'lib/Attean.pm';
author			'Gregory Todd Williams <gwilliams@cpan.org>';
license			'perl';

test_requires	'Test::Exception'			=> 0;
test_requires	'Test::LWP::UserAgent'		=> 0;
test_requires	'Test::Modern'				=> 0;
test_requires	'Test::Moose'				=> 0;
test_requires	'Test::More'				=> 0.88;
test_requires	'Test::Roo'					=> 0;

### This is required for running xt/dawg11.t, which also requires manual
### downloading of the SPARQL test suite, so will not be relevant to automated
### tooling:
# test_requires	'XML::Simple'				=> 0;

perl_version	'5.014';

requires		'Algorithm::Combinatorics'	=> 0;
requires		'Data::UUID'				=> 0;
requires		'DateTime::Format::W3CDTF'	=> 0;
requires		'HTTP::Negotiate'			=> 0;
requires		'IRI'						=> 0.004;
requires		'JSON'						=> 0;
requires		'LWP::UserAgent'			=> 0;
requires		'List::MoreUtils'			=> 0;
requires		'Math::Cartesian::Product'	=> 1.008;
requires		'Module::Pluggable'			=> 0;
requires		'Moo'						=> 1.006000;
requires		'MooX::Log::Any'			=> 0;
requires		'namespace::clean'			=> 0;
requires		'Set::Scalar'				=> 0;
requires		'Sub::Install'				=> 0;
requires		'Sub::Util'					=> 1.40;
requires		'Text::CSV'					=> 0;
requires		'Try::Tiny'					=> 0;
requires		'Type::Tiny'				=> 0;
requires		'URI::Escape'				=> 1.36;
requires		'URI::NamespaceMap'			=> 0.12;
requires		'XML::SAX'					=> 0;
requires		'File::Slurp'				=> 0;
requires		'File::ShareDir'			=> 0;
requires		'RDF::Query'				=> 2.910;

resources(
	'homepage'   => "http://search.cpan.org/dist/Attean/",
	'repository' => "http://github.com/kasei/attean/",
	'bugtracker' => "https://github.com/kasei/attean/issues",
	'IRC'        => "irc://irc.perl.org/#perlrdf",
);

author_tests('xt');

install_share;
install_script glob('bin/attean_parse');
install_script glob('bin/attean_query');

write_doap_changes "meta/changes.ttl", "Changes", "turtle";

sign;
WriteAll;
