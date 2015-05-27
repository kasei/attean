=head1 NAME

AtteanX::Serializer::SPARQLCSV - SPARQL Results CSV Serializer

=head1 VERSION

This document describes AtteanX::Serializer::SPARQLCSV version 0.005

=head1 SYNOPSIS

 use Attean;
 my $s = Attean->get_serializer('SPARQLCSV')->new();
 $s->serialize_iter_to_io( $fh, $iter );

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Serializer::SPARQLCSV 0.005 {
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
	
=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >>.

=cut

	sub serialize_iter_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= shift;
		my $first	= 1;
		my $csv = Text::CSV->new ( { binary => 1 } );
		while (my $t = $iter->next()) {
			if ($first) {
				$csv->print($io, [$t->variables]);
				print $io "\n";
				$first	= 0;
			}
			my @strings;
			foreach my $term ($t->values) {
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
	
=item C<< serialize_iter_to_bytes( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >>
and returns the serialization as a UTF-8 encoded byte string.

=cut

	sub serialize_iter_to_bytes {
		my $self	= shift;
		my $iter	= shift;
		my $data	= encode('UTF-8', '');
		my $first	= 1;
		my $csv = Text::CSV->new ( { binary => 1 } );
		while (my $t = $iter->next()) {
			if ($first) {
				if ($csv->combine(map { encode('UTF-8', $_) } $t->variables)) {
					$data	.= $csv->string . "\n";
				}
				$first	= 0;
			}
			my @strings;
			foreach my $term ($t->values) {
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

Copyright (c) 2006-2012 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
