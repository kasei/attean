=head1 NAME

Attean - A Semantic Web Framework

=head1 VERSION

This document describes Attean version 0.001_01

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  use Attean::RDF qw(iri);
  my $store = Attean->get_store('Memory')->new();
  my $parser = Attean->get_parser('NTriples')->new();
  
  # iterator of triples and quads
  my $iter = $parser->parse_iter_from_io(\*STDIN);
  
  # add a graph name to all triples
  my $quads = $iter->as_quads(iri('http://graph-name/'));
  
  $store->add_iter($quads);
  my $model = Attean::QuadModel->new( store => $store );
  my $iter = $model->get_quads();
  while (my $quad = $iter->next) {
    say $quad->object->ntriples_string;
  }

=head1 DESCRIPTION

Attean provides APIs for parsing, storing, querying, and serializing
Semantic Web (RDF and SPARQL) data.

=head1 METHODS

=over 4

=cut

package Attean {
	use v5.14;
	use warnings;
	our $VERSION	= '0.001_01';
	use Attean::API;
	
	use Attean::Blank;
	use Attean::Literal;
	use Attean::Variable;
	use Attean::IRI;
	
	use Attean::Triple;
	use Attean::Quad;
	use Attean::Result;
	
	use Attean::QuadModel;
	use Attean::BindingEqualityTest;
	
	use Attean::CodeIterator;
	use Attean::ListIterator;
	use Attean::IteratorSequence;
	
	use Attean::TermMap;
	
	use HTTP::Negotiate qw(choose);
	use List::MoreUtils qw(any all);
	use Module::Load::Conditional qw(can_load);
	use Sub::Util qw(set_subname);
	use namespace::clean;
	
	use Module::Pluggable search_path => 'AtteanX::Parser', sub_name => 'parsers', max_depth => 3;
	use Module::Pluggable search_path => 'AtteanX::Serializer', sub_name => 'serializers', max_depth => 3;
	use Module::Pluggable search_path => 'AtteanX::Store', sub_name => 'stores', max_depth => 3;
	
	sub import {
		my $class	= shift;
		if (scalar(@_)) {
			my %args	= @_;
			foreach my $p (@{ $args{parsers} || [] }) {
# 				warn "Loading $p parser...";
				$class->get_parser($p) || die "Failed to load parser: $p";
			}
			foreach my $s (@{ $args{serializers} || [] }) {
# 				warn "Loading $s serializer...";
				$class->get_serializer($s) || die "Failed to load serializer: $s";
			}
			foreach my $s (@{ $args{stores} || [] }) {
# 				warn "Loading $s store...";
				$class->get_store($s) || die "Failed to load store: $s";
			}
		}
	}
	
=item C<< get_store( $NAME ) >>

Attempts to find a L<Attean::API::Store> implementation with the
given C<< $NAME >>. This is done using L<Module::Pluggable> and will generally
be searching for class names C<< AtteanX::Store::$NAME >>.

Returns the full class name if a matching implementation is found, otherwise
returns undef.

=cut

	sub get_store {
		my $self	= shift;
		return $self->_get_plugin('stores', shift);
	}
	
=item C<< get_serializer( $NAME ) >>

=item C<< get_serializer( media_type => $MEDIA_TYPE ) >>

Attempts to find a L<Attean::API::Serializer> serializer class with the given
C<< $NAME >>, or that can serialize files with the C<< $MEDIA_TYPE >> media
type.

Returns the full class name if a matching implementation is found, otherwise
returns undef.

=cut

	sub get_serializer {
		my $self	= shift;
		my $role	= 'Attean::API::Serializer';
		return $self->_get_plugin('serializers', shift, $role) if (scalar(@_) == 1);
		my $type	= shift;
		my %method	= (media_type => 'media_types');
		if (my $method = $method{ $type }) {
			my $value	= shift;
			foreach my $p ($self->serializers()) {
				if (can_load( modules => { $p => 0 })) {
					next unless ($p->does($role));
					my @exts	= @{ $p->$method() };
					return $p if (any { $value eq $_ } @exts);
				}
			}
			return;
		} else {
			die "Not a valid constraint in get_serializer call: $type";
		}
	}
	
=item C<< get_parser( $NAME ) >>

=item C<< get_parser( filename => $FILENAME ) >>

=item C<< get_parser( media_type => $MEDIA_TYPE ) >>

Attempts to find a L<Attean::API::Parser> parser class with the given
C<< $NAME >>, or that can parse files with the same extension as
C<< $FILENAME >>, or that can parse files with the C<< $MEDIA_TYPE >> media
type.

Returns the full class name if a matching implementation is found, otherwise
returns undef.

=cut

	sub get_parser {
		my $self	= shift;
		my $role	= 'Attean::API::Parser';
		return $self->_get_plugin('parsers', shift, $role) if (scalar(@_) == 1);
		my $type	= shift;
		my %method	= (filename => 'file_extensions', media_type => 'media_types');
		if (my $method = $method{ $type }) {
			my $value	= shift;
			$value	=~ s/^.*[.]// if ($type eq 'filename');
			foreach my $p ($self->parsers()) {
				if (can_load( modules => { $p => 0 })) {
					next unless ($p->does($role));
					my @exts	= @{ $p->$method() };
					return $p if (any { $value eq $_ } @exts);
				}
			}
			return;
		} else {
			die "Not a valid constraint in get_parser call: $type";
		}
	}
	
	{
		my %roles	= (
			serializers	=> 'Attean::API::Serializer',
			parsers		=> 'Attean::API::Parser',
			stores		=> 'Attean::API::Store',
		);
		for my $method (keys %roles) {
			my $role	= $roles{$method};
			my $code	= sub {
				my $self	= shift;
				my @classes;
				foreach my $class ($self->$method()) {
					next unless (can_load( modules => { $class => 0 }));
					push(@classes, $class) if ($class->does($role));
				}
				return @classes;
			};
			Sub::Install::install_sub({
				code	=> set_subname("list_${method}", $code),
				as		=> "list_${method}"
			});
		}
	}
	
	sub _get_plugin {
		my $self	= shift;
		my $type	= shift;
		my $name	= shift;
		my @roles	= @_;
		foreach my $p ($self->$type()) {
			if (lc(substr($p, -(length($name)+2))) eq lc("::$name")) {
				unless (can_load( modules => { $p => 0 })) {
					warn $Module::Load::Conditional::ERROR;
					return;
				}
				
				foreach (@roles) {
					unless ($p->does($_)) {
						die ucfirst($type) . " class $p failed validation for role $_";
					}
				}
				return $p;
			}
		}
	}
	
=item C<< negotiate_serializer ( request_headers => $request_headers, restrict => \@serializer_names, extend => \%media_types ) >>

Returns a two-element list containing an appropriate media type and
L<Attean::API::Serializer> class as decided by L<HTTP::Negotiate>.  If the
C<< 'request_headers' >> key-value is supplied, the C<< $request_headers >> is
passed to C<< HTTP::Negotiate::choose >>.  The option C<< 'restrict' >>, set to
a list of serializer names, can be used to limit the serializers to choose from.
Finally, an C<<'extend'>> option can be set to a hashref that contains
MIME-types as keys and a custom variant as value. This will enable the user to
use this negotiator to return a type that isn't supported by any serializers.
The subsequent code will have to find out how to return a representation.

=cut

	sub negotiate_serializer {
		my $class		= shift;
		my %options		= @_;
		my $headers		= delete $options{ 'request_headers' };
		my $restrict	= delete $options{ 'restrict' };
		my $extend		= delete $options{ 'extend' } || {};
		my %sclasses;
		my %serializer_names;
		my %media_types;
		foreach my $sclass ($class->list_serializers) {
			my $name	= $sclass =~ s/^.*://r;
			$serializer_names{lc($name)}	= $sclass;
			$media_types{$_}	= $sclass for (@{ $sclass->media_types });
		}
		if (ref($restrict) && ref($restrict) eq 'ARRAY') {
			foreach (@$restrict) {
				if (my $sclass = $serializer_names{lc($_)}) {
					$sclasses{ $sclass } = 1;
				}
			}
		} else {
			%sclasses = reverse %serializer_names;
		}
		my @default_variants;
		while (my($type, $sclass) = each(%media_types)) {
			next unless $sclasses{$sclass};
			my $qv;
			# slightly prefer turtle as a readable format to others
			# try hard to avoid using ntriples as 'text/plain' isn't very useful for conneg
			if ($type eq 'application/n-triples') {
				$qv	= 1.0;
			} elsif ($type eq 'text/plain') {
				$qv	= 0.2;
			} else {
				$qv	= 0.99;
				$qv		-= 0.01 if ($type =~ m#/x-#);				# prefer non experimental media types
				$qv		-= 0.01 if ($type =~ m#^application/(?!rdf[+]xml)#);	# prefer standard rdf/xml to other application/* formats
			}
			push(@default_variants, [$type, $qv, $type]);
		}
	
		my %custom_thunks;
		my @custom_variants;
		while (my($type,$thunk) = each(%$extend)) {
			push(@custom_variants, [$thunk, 1.0, $type]);
			$custom_thunks{ $thunk }	= [$type, $thunk];
		}
	
		# remove variants with media types that are in custom_variants from @variants
		my @variants	= grep { not exists $extend->{ $_->[2] } } @default_variants;
		push(@variants, @custom_variants);
		
		my $stype	= choose( \@variants, $headers );
		if (defined($stype) and $custom_thunks{ $stype }) {
			my $thunk	= $stype;
			my $type	= $custom_thunks{ $stype }[0];
			return ($type, $thunk);
		}
	
		if (defined($stype) and my $sclass = $media_types{ $stype }) {
			return ($stype, $sclass);
		} else {
			die "No appropriate serializer found for content-negotiation";
		}
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO

L<http://www.perlrdf.org/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
