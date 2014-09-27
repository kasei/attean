=head1 NAME

Attean - A Semantic Web Framework

=head1 VERSION

This document describes Attean version 0.001

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $parser = Attean->get_parser('NQuads')->new();
  my $iter = $parser->parse_iter_from_io(\*STDIN);
  my $store = Attean->get_store('Memory')->new();
  $store->add_iter($iter->as_quads);
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
	our $VERSION	= '0.001';
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
	
	use List::MoreUtils qw(any all);
	use Module::Load::Conditional qw(can_load);
	use Sub::Install;
	use Sub::Name;
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

Attempts to find a store (L<Attean::API::Store>) implementation with the
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

Attempts to find a store (L<Attean::API::Serializer>) implementation with the
given C<< $NAME >>. This is done using L<Module::Pluggable> and will generally
be searching for class names C<< AtteanX::Serializer::$NAME >>.

Returns the full class name if a matching implementation is found, otherwise
returns undef.

=cut

	sub get_serializer {
		my $self	= shift;
		return $self->_get_plugin('serializers', shift, 'Attean::API::Serializer');
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
				code	=> subname("list_${method}", $code),
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
