use v5.14;
use warnings;

=head1 NAME

Attean::API::Parser - Parser role

=head1 VERSION

This document describes Attean::API::Parser version 0.034

=head1 DESCRIPTION

The Attean::API::Parser role defines a common API for all parsers of typed
objects from data (either a byte string or a filehandle).

=head1 ATTRIBUTES

The following attributes exist:

=over 4

=item C<< handler >>

A code reference that will be called during callback-variant parsing  methods.
This attribute has a default (no-op function), so specifying it is not
necessary if using iterator- or list-variant parsing methods.

=back

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Parser> role:

=over 4

=item C<< canonical_media_type >>

Returns the canonical media type string for the format of this parser.

=item C<< media_types >>

Returns an ARRAY reference of media type strings that are acceptable as input
to this parser.

=item C<< handled_type >>

Returns a L<Type::Tiny> object representing the type of items that result from
parsing.

=item C<< file_extensions >>

Returns an ARRAY reference of file extensions commonly associated with the
media types supported by the parser (and returned by C<< media_types >>).
File extensions should NOT include a leading dot.

=cut

use Type::Tiny::Role;

package Attean::API::Parser 0.034 {
	use Types::Standard qw(CodeRef Bool);

	use Moo::Role;
	use Scalar::Util qw(blessed);
	use namespace::clean;
	
	has 'handler' => (is => 'rw', isa => CodeRef, default => sub { sub {} });
	has 'lazy_iris' => (is => 'rw', isa => Bool, default => 0);
	
	requires 'canonical_media_type'; # => (is => 'ro', isa => 'Str', init_arg => undef);
	requires 'media_types'; # => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef);
	requires 'handled_type'; # => (is => 'ro', isa => 'Type::Tiny', init_arg => undef);
	requires 'file_extensions'; # => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef);
	
=item C<< new_iri( value => $value ) >>

Constructs and returns a new L<Attean::IRI> object, respecting the parser's
C<lazy_iris> attribute.
=cut

	sub new_iri {
		my $self	= shift;
		my %args;
		if ($self->lazy_iris) {
			$args{lazy}	= 1;
		} else {
			$args{lazy}	= 0;
		}
		if (scalar(@_) == 1) {
			$args{value}	= shift;
		} else {
			%args	= (%args, @_);
		}
		return Attean::IRI->new(%args);
	}

=item C<< new_literal( value => $value, [ datatype => $dt, ] [ language => $lang ])	>>

Constructs and returns a new L<Attean::Literal> object.

=cut

	sub new_literal {
		my $self	= shift;
		my %args	= @_;
		
		return Attean::Literal->new(%args);
	}
	
	sub file_extensions { return [] }
}

package Attean::API::AbbreviatingParser 0.034 {
	use Types::Standard qw(ConsumerOf InstanceOf Maybe);
	use Types::Namespace qw( NamespaceMap );
	use Scalar::Util qw(blessed);

	use Moo::Role;
	
	with 'Attean::API::Parser';
	has 'base' 			=> (is => 'rw', isa => ConsumerOf['Attean::API::IRI'], coerce => sub { blessed($_[0]) ? Attean::IRI->new($_[0]->as_string) : Attean::IRI->new($_[0]) }, predicate => 'has_base');
	has 'namespaces'	=> (is => 'ro', isa => Maybe[NamespaceMap]);
}

package Attean::API::CDTBlankNodeMappingParser 0.034 {
	use Types::Standard qw(HashRef ConsumerOf InstanceOf Maybe Bool);
	use Scalar::Util qw(blessed);
	use UUID::Tiny ':std';
	use Data::Dumper;
	use Moo::Role;
	
	with 'Attean::API::Parser';
	has 'blank_nodes'	=> (is => 'ro', isa => Maybe[HashRef[ConsumerOf['Attean::API::Blank']]]);
	has 'parse_id'		=> (is => 'rw', default => sub { unpack('H*', create_uuid()) });
	has 'enable_cdt_rewriting' => (is => 'rw', isa => Bool, default => 1);

	foreach my $method (qw(parse_iter_from_io parse_iter_from_bytes parse_cb_from_io parse_cb_from_bytes)) {
		around $method => sub {
			my $orig	= shift;
			my $self	= $_[0];

			$self->parse_id(unpack('H*', create_uuid()));
			my $term	= $orig->(@_);
			return $term;
		};
	}
	
	around 'new_literal' => sub {
		my $orig	= shift;
		my $self	= $_[0];
		my $term	= $orig->(@_);
		if ($self->enable_cdt_rewriting) {
			my $dt 		= $term->datatype();
			if (blessed($dt) and ($dt->value eq 'http://w3id.org/awslabs/neptune/SPARQL-CDTs/Map' or $dt->value eq 'http://w3id.org/awslabs/neptune/SPARQL-CDTs/List')) {
				return AtteanX::Functions::CompositeLists::rewrite_lexical($term, $self->blank_nodes, $self->parse_id);
			}
		}
		return $term;
	};
}

package Attean::API::PushParser 0.034 {
	use Moo::Role;
	with 'Attean::API::Parser';

	requires 'parse_cb_from_io';		# parse_cb_from_io($io)
	requires 'parse_cb_from_bytes';		# parse_cb_from_bytes($data)
	
	sub parse_iter_from_io {
		my $self	= shift;
		my @values	= $self->parse_list_from_io(@_);
		if ($self->does('Attean::API::ResultParser') or $self->does('Attean::API::ResultOrTermParser')) {
			my %vars;
			foreach my $r (@values) {
				if ($r->does('Attean::API::Result')) {
					foreach my $v ($r->variables) {
						$vars{$v}++;
					}
				}
			}
			return Attean::ListIterator->new( variables => [keys %vars], values => \@values, item_type => $self->handled_type->role, );
		} else {
			return Attean::ListIterator->new( values => \@values, item_type => $self->handled_type->role, );
		}
	}
	
	sub parse_iter_from_bytes {
		my $self	= shift;
		my @values	= $self->parse_list_from_bytes(@_);
		if ($self->does('Attean::API::ResultParser') or $self->does('Attean::API::ResultOrTermParser')) {
			my %vars;
			foreach my $r (@values) {
				if ($r->does('Attean::API::Result')) {
					foreach my $v ($r->variables) {
						$vars{$v}++;
					}
				}
			}
			return Attean::ListIterator->new( variables => [keys %vars], values => \@values, item_type => $self->handled_type->role, );
		} else {
			return Attean::ListIterator->new( values => \@values, item_type => $self->handled_type->role, );
		}
	}
	
	sub parse_list_from_io {
		my $self	= shift;
		my @values;
		$self->handler(sub {
			push(@values, shift);
		});
		$self->parse_cb_from_io(@_);
		return @values;
	}
	
	sub parse_list_from_bytes {
		my $self	= shift;
		my @values;
		$self->handler(sub {
			push(@values, shift);
		});
		$self->parse_cb_from_bytes(@_);
		return @values;
	}
}

package Attean::API::PullParser 0.034 {
	use Moo::Role;
	with 'Attean::API::Parser';
	
	requires 'parse_iter_from_io';		# $iter = parse_iter_from_io($io)
	requires 'parse_iter_from_bytes';	# $iter = parse_iter_from_bytes($data)
	
	sub parse_cb_from_io {
		my $self	= shift;
		my $io		= shift;
		my $handler	= $self->handler;
		my $iter	= $self->parse_iter_from_io($io);
		while (my $item = $iter->next) { $handler->( $item ) }
	}
	
	sub parse_cb_from_bytes {
		my $self	= shift;
		my $data	= shift;
		my $handler	= $self->handler;
		my $iter	= $self->parse_iter_from_bytes($data);
		while (defined(my $item = $iter->next)) { $handler->( $item ) }
	}
	
	sub parse_list_from_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= $self->parse_iter_from_io($io);
		my @list;
		while (defined(my $item = $iter->next)) { push(@list, $item); }
		return @list;
	}
	
	sub parse_list_from_bytes {
		my $self	= shift;
		my $data	= shift;
		my $iter	= $self->parse_iter_from_bytes($data);
		my @list;
		while (defined(my $item = $iter->next)) { push(@list, $item); }
		return @list;
	}
}

package Attean::API::AtOnceParser 0.034 {
	use Moo::Role;
	with 'Attean::API::Parser';
	
	requires 'parse_list_from_io';		# @list = parse_list_from_io($io)
	requires 'parse_list_from_bytes';	# @list = parse_list_from_bytes($data)
	
	sub parse_cb_from_io {
		my $self	= shift;
		my $io		= shift;
		my $handler	= $self->handler;
		my $iter	= $self->parse_iter_from_io($io);
		while (my $item = $iter->next) { $handler->( $item ) }
	}
	
	sub parse_cb_from_bytes {
		my $self	= shift;
		my $data	= shift;
		my $handler	= $self->handler;
		my $iter	= $self->parse_iter_from_bytes($data);
		while (defined(my $item = $iter->next)) { $handler->( $item ) }
	}
	
	sub parse_iter_from_io {
		my $self	= shift;
		my @values	= $self->parse_list_from_io(@_);
		if ($self->does('Attean::API::ResultParser') or $self->does('Attean::API::ResultOrTermParser')) {
			my %vars;
			foreach my $r (@values) {
				if ($r->does('Attean::API::Result')) {
					foreach my $v ($r->variables) {
						$vars{$v}++;
					}
				}
			}
			return Attean::ListIterator->new( variables => [keys %vars], values => \@values, item_type => $self->handled_type->role, );
		} else {
			return Attean::ListIterator->new( values => \@values, item_type => $self->handled_type->role, );
		}
	}
	
	sub parse_iter_from_bytes {
		my $self	= shift;
		my @values	= $self->parse_list_from_bytes(@_);
		if ($self->does('Attean::API::ResultParser') or $self->does('Attean::API::ResultOrTermParser')) {
			my %vars;
			foreach my $r (@values) {
				if ($r->does('Attean::API::Result')) {
					foreach my $v ($r->variables) {
						$vars{$v}++;
					}
				}
			}
			return Attean::ListIterator->new( variables => [keys %vars], values => \@values, item_type => $self->handled_type->role, );
		} else {
			return Attean::ListIterator->new( values => \@values, item_type => $self->handled_type->role, );
		}
	}
	
}

package Attean::API::TermParser 0.034 {
	# Parser returns objects that conform to Attean::API::Term
	use Moo::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Term');
		return $ITEM_TYPE;
	}
}

package Attean::API::TripleParser 0.034 {
	# Parser returns objects that conform to Attean::API::Triple
	use Moo::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Triple');
		return $ITEM_TYPE;
	}
}

package Attean::API::QuadParser 0.034 {
	# Parser returns objects that conform to Attean::API::Quad
	use Moo::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Quad');
		return $ITEM_TYPE;
	}
}

package Attean::API::MixedStatementParser 0.034 {
	# Parser returns objects that conform to either Attean::API::Triple or Attean::API::Quad
	use Moo::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::TripleOrQuad');
		return $ITEM_TYPE;
	}
}

package Attean::API::ResultOrTermParser 0.034 {
	# Parser returns objects that conform to either Attean::API::Result or Attean::API::Term
	use Moo::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::ResultOrTerm');
		return $ITEM_TYPE;
	}
}

package Attean::API::ResultParser 0.034 {
	# Parser returns objects that conform to Attean::API::Result
	use Moo::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Result');
		return $ITEM_TYPE;
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2022 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
