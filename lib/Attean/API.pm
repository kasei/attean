use v5.14;
use warnings;

=head1 NAME

Attean::API - Utility package for loading all Attean role packages.

=head1 VERSION

This document describes Attean::API version 0.028

=head1 SYNOPSIS

  use v5.14;
  use Attean;

=head1 DESCRIPTION

This is a utility package that will load all the Attean-related Moo roles
in the Attean::API namespace.

=head1 METHODS

=over 4

=cut

package Attean::API::ResultOrTerm 0.028 {
	use Moo::Role;
}

package Attean::API::BlankOrIRI 0.028 {
	use Moo::Role;
}

package Attean::API::TermOrVariable 0.028 {
	use Scalar::Util qw(blessed);
	use Sub::Install;
	use Sub::Util qw(set_subname);

	use Moo::Role;

	with 'Attean::API::SPARQLSerializable';

	sub equals {
		my ($a, $b)	= @_;
		return ($a->as_string eq $b->as_string);
	}

	sub is_bound {
	  my $self = shift;
	  return (! $self->does('Attean::API::Variable'));
	}
	
	sub apply_binding {
		my $self	= shift;
		my $class	= ref($self);
		my $bind	= shift;
		if ($self->does('Attean::API::Variable')) {
			my $name	= $self->value;
			my $replace	= $bind->value($name);
			if (defined($replace) and blessed($replace)) {
				return $replace;
			} else {
				return $self;
			}
		} else {
			return $self;
		}
	}

	BEGIN {
		my %types	= (
			variable	=> 'Variable',
			blank		=> 'Blank',
			literal		=> 'Literal',
			resource	=> 'IRI',
			iri			=> 'IRI',
		);
		while (my ($name, $role) = each(%types)) {
			my $method	= "is_$name";
			my $code	= sub { return shift->does("Attean::API::$role") };
			Sub::Install::install_sub({
				code	=> set_subname($method, $code),
				as		=> $method
			});
		}
	}
}

package Attean::Mapper 0.028 {
	use Moo::Role;
	requires 'map'; # my $that = $object->map($this)
}

package Attean::API::Variable 0.028 {
	use AtteanX::SPARQL::Constants;
	use AtteanX::SPARQL::Token;

	use Moo::Role;

	with 'Attean::API::TermOrVariable';

=item C<< as_string >>

Returns a string representation of the variable.'

=cut

	sub as_string {
		my $self	= shift;
		return '?' . $self->value;
	}

	sub sparql_tokens {
		my $self	= shift;
		my $t	= AtteanX::SPARQL::Token->fast_constructor( VAR, -1, -1, -1, -1, [$self->value] );
		return Attean::ListIterator->new( values => [$t], item_type => 'AtteanX::SPARQL::Token' );
	}
	
}

package Attean::API::CanonicalizingBindingSet 0.028 {
	use Attean::RDF;

	use Moo::Role;
	use namespace::clean;

	with 'MooX::Log::Any';
	requires 'elements';

	sub canonical_set {
		my $self	= shift;
		my ($set)	= $self->canonical_set_with_mapping;
		return $set;
	}
	
	sub canonical_set_with_mapping {
		my $self	= shift;
		my @t		= $self->elements;
		my @tuples	= map { [ $_->tuples_string, $_, {} ] } @t;
		my $replacements	= 0;
		foreach my $p (@tuples) {
			my ($str, $t)	= @$p;
			foreach my $pos ($t->variables) {
				my $term	= $t->value($pos);
				my $tstr	= $term->ntriples_string;
				if ($term->does('Attean::API::Blank') or $term->does('Attean::API::Variable')) {
					$str	=~ s/\Q$tstr\E/~/;
					$str	.= "#$tstr";
					$p->[2]{$pos}	= $tstr;
					$replacements++;
					$p->[0]	= $str;
				}
			}
		}
	
		@tuples	= sort { $a->[0] cmp $b->[0] } @tuples;
		my $counter	= 1;
		my %mapping;
		foreach my $i (0 .. $#tuples) {
			my $p		= $tuples[$i];
			my ($str, $t)	= @$p;
			my $item_class	= ref($t);
			my ($next, $last)	= ('')x2;
			$last	= $tuples[$i-1][0] if ($i > 0);
			$next	= $tuples[$i+1][0] if ($i < $#tuples);
			next if ($str eq $last or $str eq $next);
			foreach my $pos (reverse $t->variables) {
				if (defined(my $tstr = $p->[2]{$pos})) {
					$tstr	=~ /^([?]|_:)([^#]+)$/;
					my $prefix	= $1;
					my $name	= $2;
					my $key		= "$prefix$name";
					delete $p->[2]{$pos};
					my $id		= (exists($mapping{$key})) ? $mapping{$key}{id} : sprintf("v%03d", $counter++);
					my $type	= ($prefix eq '?' ? 'variable' : 'blank');
					$mapping{ $key }	= { id => $id, prefix => $prefix, type => $type };
					my %t		= $p->[1]->mapping;
					$t{ $pos }	= ($type eq 'blank') ? Attean::Blank->new($id) : Attean::Variable->new($id);
					my $t	= $item_class->new( %t );
					$p->[1]	= $t;
					$p->[0]	= $t->tuples_string;
				}
			}
		}
	
		foreach my $p (@tuples) {
			my ($str, $t)	= @$p;
			my $item_class	= ref($t);
			foreach my $pos (reverse $t->variables) {
				if (defined(my $tstr = $p->[2]{$pos})) {
					$tstr	=~ /^([?]|_:)([^#]+)$/;
					my $prefix	= $1;
					my $name	= $2;
					my $key		= "$prefix$name";
					delete $p->[2]{$pos};
					unless (exists($mapping{$key})) {
						$self->error("Cannot canonicalize binding set");
						return;
					}
					my $id		= $mapping{$key}{id};
					my $type	= ($prefix eq '?' ? 'variable' : 'blank');
					$mapping{ $key }	= { id => $id, prefix => $prefix, type => $type };
					my %t		= $p->[1]->mapping;
					$t{ $pos }	= ($type eq 'blank') ? Attean::Blank->new($id) : Attean::Variable->new($id);
					my $t	= $item_class->new( %t );
					$p->[1]	= $t;
					$p->[0]	= $t->tuples_string;
				}
			}
		}

		@tuples	= sort { $a->[0] cmp $b->[0] } @tuples;
		my $elements	= [ map { $_->[1] } @tuples ];
		return ($elements, \%mapping);
	}
}

package Attean::API 0.028 {
	use Attean::API::Term;
	use Attean::API::Store;
	use Attean::API::Model;
	use Attean::API::Iterator;
	use Attean::API::Parser;
	use Attean::API::Serializer;
	use Attean::API::Query;
	use Attean::API::Expression;
	use Attean::API::Plan;
	use Attean::API::QueryPlanner;
	
	use Attean::Variable;
	use Attean::Blank;
	use Attean::IRI;
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

Copyright (c) 2014--2020 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
