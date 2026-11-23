=head1 NAME

AtteanX::Serializer::TextTable - SPARQL Results TSV Serializer

=head1 VERSION

This document describes AtteanX::Serializer::TextTable version 0.041

=head1 SYNOPSIS

 use Attean;
 my $s = Attean->get_serializer('TextTable')->new();
 $s->serialize_iter_to_io( $fh, $iter );

=head1 DESCRIPTION

...

=head1 ATTRIBUTES

=over 4

=item C<< canonical_media_type >>

=item C<< file_extensions >>

=back

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Serializer::TextTable 0.041 {
	use Moo;
	use Types::Standard qw(Str Bool ArrayRef);
	use Encode qw(encode);
	use Scalar::Util qw(blessed);
	use Attean::ListIterator;
	use List::Util qw(any);
	use Text::Table;
	use namespace::clean;

	my @rule			= qw(- +);
	has 'canonical_media_type' => (is => 'ro', isa => Str, init_arg => undef, default => 'text/plain');
	has 'number_rows' => (is => 'rw', isa => Bool, default => 0);

=item C<< media_types >>

Returns a list of media types that identify the format produced by this serializer.

=cut

	sub media_types {
		return [qw(text/plain)];
	}

=item C<< file_extensions >>

Returns a list of file extensions associated with the serialized format.

=cut

	sub file_extensions { return [qw(txt text)] };
	
=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >>.

=cut

	sub serialize_iter_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= shift;
		
		my @vars;
		if ($iter->does('Attean::API::ResultOrTermIterator')) {
			@vars	= @{ $iter->variables };
		} elsif ($iter->does('Attean::API::TripleIterator')) {
			@vars	= qw(subject predicate object);
		} else {
			@vars	= qw(subject predicate object graph);
		}

		my @header_names	= @vars;
		if ($self->number_rows) {
			unshift(@header_names, '#');
		}

		my @headers			= (\q"| ");
		push(@headers, map { $_ => \q" | " } @header_names);
		pop	@headers;
		push @headers => (\q" |");

		my $table = Text::Table->new(@headers);

		my @rule			= qw(- +);
		my @rows;
		my $row	= 1;
		while (my $t = $iter->next()) {
			my @strings	= map { $self->_serialize_value($_) } map { eval { $t->value($_) } } @vars;
			if ($self->number_rows) {
				unshift(@strings, $row++);
			}
			push(@rows, \@strings);
		}
		$table->load(@rows);
		
		print {$io} join('',
				$table->rule(@rule),
				$table->title,
				$table->rule(@rule),
				map({ $table->body($_) } 0 .. @rows),
				$table->rule(@rule)
			);
	}

	sub _serialize_value {
		my $self	= shift;
		my $value	= shift;
		return '' unless (blessed($value));
		my $lex		= $value->value;
		if ($value->does('Attean::API::IRI')) {
			# this shortening to something like a prefixed name is a heuristic
			# suitable for texttable, but not for cases where the value must
			# be able to round-tripl through a parser
			if (my $ns = $self->namespaces) {
				my $qname;
				my $len;
				while (my ($prefix, $nsURI) = $ns->each_map) {
					my $base	= $nsURI->iri->value;
					if (substr($lex, 0, length($base)) eq $base) {
						my $local	= substr($lex, length($base));
						if (not(defined($qname)) or length($local) < $len) {
							$qname	= join(':', $prefix, $local);
							$len	= length($local);
						}
					}
				}
				return $qname if (defined($qname));
			}
			return $value->ntriples_string;
		} elsif ($value->does('Attean::API::Literal') and my $dt = $value->datatype) {
			if ($dt->value eq 'http://www.w3.org/2001/XMLSchema#string') {
				return qq["$lex"];
			}
			my $dts	= $self->_serialize_value($dt);
			return '"' . $lex . '"^^' . $dts;
		}
		return $value->ntriples_string;
	}
	
=item C<< serialize_iter_to_bytes( $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >>
and returns the serialization as a UTF-8 encoded byte string.

=cut

	sub serialize_iter_to_bytes {
		my $self	= shift;
		my $iter	= shift;
		
		my @vars	= ($iter->does('Attean::API::TripleIterator') or $iter->does('Attean::API::QuadIterator')) ? $iter->variables : @{ $iter->variables };
		my @header_names	= @vars;
		if ($self->number_rows) {
			unshift(@header_names, '#');
		}

		my @headers			= (\q"| ");
		push(@headers, map { $_ => \q" | " } @header_names);
		pop	@headers;
		push @headers => (\q" |");

		my $table = Text::Table->new(@headers);
		
		my @rows;
		my $row	= 1;
		while (my $t = $iter->next()) {
			my @strings	= map { $self->_serialize_value($_) } map { $t->value($_) } @vars;
			if ($self->number_rows) {
				unshift(@strings, $row++);
			}
			push(@rows, \@strings);
		}
		$table->load(@rows);

		my $data	= join('',
				$table->rule(@rule),
				$table->title,
				$table->rule(@rule),
				map({ $table->body($_) } 0 .. @rows),
				$table->rule(@rule)
			);
		return encode('UTF-8', $data);
	}

	with 'Attean::API::ResultSerializer';
	with 'Attean::API::AbbreviatingSerializer';
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

Copyright (c) 2014--2026 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
