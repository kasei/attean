use v5.14;
use warnings;

=head1 NAME

Attean::TermMap - Mapping terms to new terms

=head1 VERSION

This document describes Attean::TermMap version 0.000

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $m = Attean::TermMap->short_blank_map;
  my $new_blank = $m->map( Attean::Blank->new('abcdefg') );
  say $new_blank->ntriples_string; # _:a

=head1 DESCRIPTION

The Attean::TermMap class represents a one-way mapping process from and to
L<Attean::API::Term> objects. This mapping may rename the blank identifiers,
skolemize nodes, or map the nodes in some other, custom way.

It conforms to the L<Attean::API::Mapper> role.

=head1 CLASS METHODS

=over 4

=cut

package Attean::TermMap 0.001 {
	use Moo;
	use Types::Standard qw(CodeRef);
	use Attean::API::Binding;
	use Data::UUID;
	use namespace::clean;
	
	with 'Attean::Mapper';
	has 'mapper'	=> (is => 'ro', isa => CodeRef, default => sub { shift }, required => 1);
	
	around BUILDARGS => sub {
		my $orig 	= shift;
		my $class	= shift;
		if (scalar(@_) == 1) {
			return $class->$orig(mapper => shift);
		}
		return $class->$orig(@_);
	};
	
=item C<< canonicalization_map >>

Returns a new L<Attean::TermMap> that canonicalizes recognized typed
L<Attean::API::Literal> values.

=cut

	sub canonicalization_map {
		my $class	= shift;
		my %map;
		return $class->new(mapper => sub {
			my $term	= shift;
			return $term unless ($term->does('Attean::API::Literal') and $term->datatype);
			
			if ($term->does('Attean::API::NumericLiteral')) {
				return $term->canonicalized_term;
			}
			
			return $term;
		});
	}
	
=item C<< uuid_blank_map >>

Returns a new L<Attean::TermMap> that renames blank nodes with UUID values.

=cut

	sub uuid_blank_map {
		my $class	= shift;
		my %map;
		return $class->new(mapper => sub {
			my $term	= shift;
			unless ($term->does('Attean::API::Blank')) {
				return $term;
			}
			my $id		= $term->value;
			if (defined(my $t = $map{$id})) {
				return $t;
			} else {
				my $uuid	= Data::UUID->new->create_hex;
				my $new		= Attean::Blank->new( $uuid );
				$map{$id}	= $new;
				return $new;
			}
		});
	}
	
=item C<< short_blank_map >>

Returns a new L<Attean::TermMap> that renames blank nodes with short
alphabetic names (e.g. _:a, _:b).

=cut

	sub short_blank_map {
		my $class	= shift;
		my %map;
		my $next	= 'a';
		return $class->new(mapper => sub {
			my $term	= shift;
			unless ($term->does('Attean::API::Blank')) {
				return $term;
			}
			my $id		= $term->value;
			if (defined(my $t = $map{$id})) {
				return $t;
			} else {
				my $new		= Attean::Blank->new( $next++ );
				$map{$id}	= $new;
				return $new;
			}
		});
	}

	sub rewrite_map {
		my $class	= shift;
		my $map		= shift;
		use Data::Dumper;
# 		warn "Rewrite map: " . Dumper($map);
		return $class->new(mapper => sub {
			my $term	= shift;
			if (exists $map->{ $term->as_string }) {
				my $new	= $map->{ $term->as_string };
# 				warn "Rewriting " . $term->as_string . ": " . $new->as_string;
				return $new;
			} else {
# 				warn "Passing through: " . $term->as_string;
				return $term;
			}
		});
	}

=back

=head1 METHODS

=over 4
	
=item C<< map( $term ) >>

Returns the term that is mapped to by the supplied C<< $term >>.

=cut

	sub map {
		my $self	= shift;
		my $term	= shift;
		return $self->mapper->( $term );
	}
	
=item C<< binding_mapper >>

Returns a mapping function reference that maps L<Attean::API::Binding>
objects by mapping their constituent mapped L<Attean::API::Term> objects.

=cut

	sub binding_mapper {
		my $self	= shift;
		return sub {
			my $binding	= shift;
			return $binding->apply_map($self);
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
