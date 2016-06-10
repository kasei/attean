=head1 NAME

AtteanX::Serializer::Turtle - Turtle Serializer

=head1 VERSION

This document describes AtteanX::Serializer::Turtle version 0.017

=head1 SYNOPSIS

 use Attean;
 my $serializer = Attean->get_serializer('Turtle')->new();
 $serializer->serialize_iter_to_io( $io, $fh );

=head1 DESCRIPTION

...

=head1 ATTRIBUTES

=over 4

=item C<< canonical_media_type >>

=item C<< media_types >>

=item C<< handled_type >>

=item C<< file_extensions >>

=back

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Serializer::Turtle 0.017 {
	use Moo;
	use Data::Dumper;
	use Encode qw(encode);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);
	use AtteanX::Parser::Turtle::Constants;
	use AtteanX::Parser::Turtle::Token;
	use AtteanX::Serializer::TurtleTokens;
	use Types::Standard qw(InstanceOf HashRef ArrayRef Bool Str);
	use namespace::clean;
	with 'Attean::API::AbbreviatingSerializer';
	with 'Attean::API::AppendableSerializer';
	with 'Attean::API::TripleSerializer';
	
	sub canonical_media_type { return "text/turtle" }

	sub media_types {
		return [qw(text/turtle)];
	}
	
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'Attean::API::Triple');
		return $ITEM_TYPE;
	}

	sub file_extensions { return [qw(ttl)] }

	has 'serializer' => (is => 'rw', isa => InstanceOf['AtteanX::Serializer::TurtleTokens']);
	
	sub BUILD {
		my $self	= shift;
		my $s		= $self->serializer;
		unless ($s) {
			my @args;
			if (my $map = $self->namespaces) {
				push(@args, namespaces => $map);
			}
			$s		= AtteanX::Serializer::TurtleTokens->new( @args );
			$self->serializer($s);
		}
	}
	
=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the Turtle token objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >>.

=cut

	sub serialize_iter_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= shift;
		my @buffer;
		
		# TODO: look for shared subject-predicate in repeated triples, and emit COMMA syntax
		# TODO: look for shared subject in repeated triples, and emit SEMICOLON syntax
		
		my $dot		= AtteanX::Parser::Turtle::Token->dot;
		my $comma	= AtteanX::Parser::Turtle::Token->comma;
		my $semi	= AtteanX::Parser::Turtle::Token->semicolon;
		if (my $map = $self->namespaces) {
			my $prefix	= AtteanX::Parser::Turtle::Token->prefix;
			foreach my $ns (sort $map->list_prefixes) {
				my $uri		= Attean::IRI->new( value => $map->namespace_uri($ns)->as_string );
				my $name	= AtteanX::Parser::Turtle::Token->fast_constructor( PREFIXNAME, -1, -1, -1, -1, ["${ns}:"] );
				my $iri		= AtteanX::Parser::Turtle::Token->fast_constructor( IRI, -1, -1, -1, -1, [$uri->value] );
				push(@buffer, $prefix);
				push(@buffer, $name);
				push(@buffer, $iri);
				push(@buffer, $dot);
			}
		}
		
		my $last_subj;
		my $last_pred;
		my $sub	= sub {
			if (scalar(@buffer)) {
				return shift(@buffer);
			}
			if (my $t = $iter->next) {
				my ($subj, $pred, $obj)	= $t->values;
				if (defined($last_subj) and $subj->equals($last_subj)) {
					if (defined($last_pred) and $pred->equals($last_pred)) {
						push(@buffer, $comma);
						push(@buffer, $obj->sparql_tokens->elements);
					} else {
						push(@buffer, $semi);
						push(@buffer, $pred->sparql_tokens->elements);
						push(@buffer, $obj->sparql_tokens->elements);
					}
				} else {
					if (defined($last_pred)) {
						push(@buffer, $dot);
					}
					foreach my $term ($subj, $pred, $obj) {
						push(@buffer, $term->sparql_tokens->elements);
					}
				}
				
				$last_subj	= $subj;
				$last_pred	= $pred;
				return shift(@buffer);
			}

			if (defined($last_subj)) {
				push(@buffer, $dot);
				$last_subj	= undef;
				$last_pred	= undef;
				return shift(@buffer);
			}
			
			return;
		};
		
		my $titer	= Attean::CodeIterator->new( generator => $sub, item_type => 'AtteanX::Parser::Turtle::Token' );
		return $self->serializer->serialize_iter_to_io($io, $titer);
	}
	
=item C<< serialize_iter_to_bytes( $iterator ) >>

Serializes the Turtle token objects from C<< $iterator >>
and returns the serialization as a UTF-8 encoded byte string.

=cut

	sub serialize_iter_to_bytes {
		my $self	= shift;
		my $iter	= shift;
		my $data	= encode('UTF-8', '');
		open(my $fh, '>', \$data);
		$self->serialize_iter_to_io($fh, $iter);
		close($fh);
		return $data;
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/perlrdf/issues>.

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2016 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
