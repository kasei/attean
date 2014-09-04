use v5.14;
use warnings;

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

package Attean 0.001 {
	use Attean::API;
	
	use Attean::Blank;
	use Attean::Literal;
	use Attean::Variable;
	use Attean::IRI;
	
	use Attean::Triple;
	use Attean::Quad;
	use Attean::Result;
	
	use Attean::QuadModel;
	
	use Attean::CodeIterator;
	use Attean::ListIterator;
	use Attean::IteratorSequence;
	
	use Attean::TermMap;
	
	use Attean::Algebra;
	use Attean::Expression;
	
	use List::MoreUtils qw(any all);
	use Module::Load::Conditional qw(can_load);
	use Module::Pluggable search_path => 'AtteanX::Parser', sub_name => 'parsers', max_depth => 3;
	use Module::Pluggable search_path => 'AtteanX::Serializer', sub_name => 'serializers', max_depth => 3;
	use Module::Pluggable search_path => 'AtteanX::Store', sub_name => 'stores', max_depth => 3;
	
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

Attempts to find a store (L<Attean::API::Parser>) implementation with the
given C<< $NAME >>. This is done using L<Module::Pluggable> and will generally
be searching for class names C<< AtteanX::Parser::$NAME >>.

Returns the full class name if a matching implementation is found, otherwise
returns undef.

=cut

	sub get_parser {
		my $self	= shift;
		return $self->_get_plugin('parsers', shift, 'Attean::API::Parser');
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
