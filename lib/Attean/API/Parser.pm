use v5.14;
use warnings;

=head1 NAME

Attean::API::Parser - Parser role

=head1 VERSION

This document describes Attean::API::Parser version 0.001

=head1 DESCRIPTION

The Attean::API::Parser role defines a common API for all parsers of typed
objects from data (either a byte string or a filehandle).

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Serializer> role:

=over 4

=item C<< canonical_media_type >>

=item C<< media_types >>

=item C<< handled_type >>

=cut

use Type::Tiny::Role;

package Attean::API::Parser 0.001 {
	use Moo::Role;
	use Types::Standard qw(CodeRef);
	
	has 'handler' => (is => 'rw', isa => CodeRef, default => sub { sub {} });
	
	requires 'canonical_media_type'; # => (is => 'ro', isa => 'Str', init_arg => undef);
	requires 'media_types'; # => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef);
	requires 'handled_type'; # => (is => 'ro', isa => 'Moose::Meta::TypeConstraint', init_arg => undef);
}

package Attean::API::Parser::AbbreviatingParser 0.001 {
	use Moo::Role;
	use Types::Standard qw(InstanceOf Maybe);
	use URI::NamespaceMap;
	use Scalar::Util qw(blessed);
	
	with 'Attean::API::Parser';
	has 'base' 		=> (is => 'rw', isa => InstanceOf['IRI'], coerce => sub { blessed($_[0]) ? IRI->new($_[0]->as_string) : IRI->new($_[0]) }, predicate => 'has_base');
	has 'namespaces'	=> (is => 'ro', isa => Maybe[InstanceOf['URI::NamespaceMap']]);
}

package Attean::API::PushParser 0.001 {
	use Moo::Role;
	with 'Attean::API::Parser';

	requires 'parse_cb_from_io';		# parse_cb_from_io($io)
	requires 'parse_cb_from_bytes';		# parse_cb_from_bytes($data)
	
	sub parse_iter_from_io {
		my $self	= shift;
		my @values	= $self->parse_list_from_io(@_);
		return Attean::ListIterator->new( values => \@values, item_type => $self->handled_type, );
	}
	
	sub parse_iter_from_bytes {
		my $self	= shift;
		my @values	= $self->parse_list_from_bytes(@_);
		return Attean::ListIterator->new( values => \@values, item_type => $self->handled_type, );
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

package Attean::API::PullParser 0.001 {
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

package Attean::API::AtOnceParser 0.001 {
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
		return Attean::ListIterator->new( values => \@values, item_type => $self->handled_type, );
	}
	
	sub parse_iter_from_bytes {
		my $self	= shift;
		my @values	= $self->parse_list_from_bytes(@_);
		return Attean::ListIterator->new( values => \@values, item_type => $self->handled_type, );
	}
	
}

package Attean::API::TermParser 0.001 {
	# Parser returns objects that conform to Attean::API::Term
	use Moo::Role;
	with 'Attean::API::Parser';
}

package Attean::API::TripleParser 0.001 {
	# Parser returns objects that conform to Attean::API::Triple
	use Moo::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Triple');
		return $ITEM_TYPE;
	}
}

package Attean::API::QuadParser 0.001 {
	# Parser returns objects that conform to Attean::API::Quad
	use Moo::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Quad');
		return $ITEM_TYPE;
	}
}

package Attean::API::MixedStatementParser 0.001 {
	# Parser returns objects that conform to either Attean::API::Triple or Attean::API::Quad
	use Moo::Role;
	with 'Attean::API::Parser';
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::TripleOrQuad');
		return $ITEM_TYPE;
	}
}

package Attean::API::ResultParser 0.001 {
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

L<http://www.perlrdf.org/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
