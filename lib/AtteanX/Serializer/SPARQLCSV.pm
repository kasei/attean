=head1 NAME

AtteanX::Serializer::SPARQLCSV - SPARQL Results CSV Serializer

=head1 VERSION

This document describes AtteanX::Serializer::SPARQLCSV version 0.034

=head1 SYNOPSIS

 use Attean;
 my $s = Attean->get_serializer('SPARQLCSV')->new();
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

package AtteanX::Serializer::SPARQLCSV 0.034 {
	use Moo;
	use Types::Standard qw(Str ArrayRef);
	use Encode qw(encode);
	use Scalar::Util qw(blessed);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);
	use Text::CSV;
	use namespace::clean;

	has 'canonical_media_type' => (is => 'ro', isa => Str, init_arg => undef, default => 'text/csv');

=item C<< media_types >>

Returns a list of media types that identify the format produced by this serializer.

=cut

	sub media_types {
		return [qw(text/csv)];
	}

=item C<< file_extensions >>

Returns a list of file extensions associated with the serialized format.

=cut

	sub file_extensions { return [qw(csv)] };
	
=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >>.

=cut

	sub serialize_iter_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= shift;
		my $csv = Text::CSV->new ( { binary => 1 } );

		my @vars	= @{ $iter->variables };
		$csv->print($io, \@vars);
		print $io "\n";
		
		while (my $t = $iter->next()) {
			my @strings;
			foreach my $var (@vars) {
				my $term	= $t->value($var);
				if (blessed($term)) {
					if ($term->does('Attean::API::Blank')) {
						push(@strings, $term->ntriples_string);
					} else {
						push(@strings, $term->value);
					}
				} else {
					push(@strings, '');
				}
			}
			$csv->print($io, [@strings]);
			print $io "\n";
		}
		return;
	}
	
=item C<< serialize_iter_to_bytes( $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >>
and returns the serialization as a UTF-8 encoded byte string.

=cut

	sub serialize_iter_to_bytes {
		my $self	= shift;
		my $iter	= shift;
		my $data	= encode('UTF-8', '');
		my $csv = Text::CSV->new ( { binary => 1 } );

		my @vars	= @{ $iter->variables };
		$csv->combine(map { encode('UTF-8', $_) } @vars);
		$data		.= $csv->string . "\n";

		while (my $t = $iter->next()) {
			my @strings;
			foreach my $var (@vars) {
				my $term	= $t->value($var);
				if (blessed($term)) {
					if ($term->does('Attean::API::Blank')) {
						push(@strings, $term->ntriples_string);
					} else {
						push(@strings, $term->value);
					}
				} else {
					push(@strings, '');
				}
			}
			if ($csv->combine(map { encode('UTF-8', $_) } @strings)) {
				$data	.= $csv->string . "\n";
			}
		}
		return $data;
	}

	with 'Attean::API::ResultSerializer', 'Attean::API::AppendableSerializer';
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

Copyright (c) 2014--2022 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
