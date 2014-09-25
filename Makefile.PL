use strict;
use warnings;

use inc::Module::Install;

name			'Attean';
all_from		'lib/Attean.pm';
author			'Gregory Todd Williams <gwilliams@cpan.org>';
license			'perl';

build_requires	'Test::More'				=> 0.88;
build_requires	'Test::Exception'			=> 0;

perl_version	'5.014';

requires		'IRI'						=> 0.004;
requires		'URI::Namespace'			=> 0;
requires		'Digest::SHA'				=> 0;
requires		'Encode'					=> 0;
requires		'List::Util'				=> 0;
requires		'Scalar::Util'				=> 0;
requires		'Set::Scalar'				=> 0;
requires		'Type::Tiny'				=> 0;
requires		'Moo'						=> 1.006000;
requires		'Sub::Name'					=> 0;
requires		'Sub::Install'				=> 0;
requires		'Set::Scalar'				=> 0;
requires		'namespace::clean'			=> 0;
requires		'Data::UUID'				=> 0;
requires		'DateTime::Format::W3CDTF'	=> 0;
requires		'I18N::LangTags'			=> 0;
requires		'URI::Escape'				=> 1.36;
requires		'RDF::Query'				=> 2.910;
requires		'Try::Tiny'					=> 0;

resources(
	'homepage'   => "http://search.cpan.org/dist/Attean/",
	'repository' => "http://github.com/kasei/attean/",
	'bugtracker' => "https://github.com/kasei/attean/issues",
	'IRC'			=> "irc://irc.perl.org/#perlrdf",
);

author_tests('xt');

install_script glob('bin/attean_parse');

sign;

WriteAll;
