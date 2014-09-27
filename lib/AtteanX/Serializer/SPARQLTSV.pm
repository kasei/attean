=head1 NAME

AtteanX::Serializer::SPARQLTSV - SPARQL Results TSV Serializer

=head1 VERSION

This document describes AtteanX::Serializer::SPARQLTSV version 0.001

=head1 SYNOPSIS

 use Attean;
 my $s = Attean->get_serializer('SPARQLTSV')->new();
 $s->serialize_iter_to_io( $fh, $iter );

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Serializer::SPARQLTSV 0.001 {
	use Moo;
	use Types::Standard qw(Str ArrayRef);
	use Encode qw(encode);
	use Scalar::Util qw(blessed);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);
	use namespace::clean;

	has 'canonical_media_type' => (is => 'ro', isa => Str, init_arg => undef, default => 'text/csv');
	has 'media_types' => (is => 'ro', isa => ArrayRef[Str], init_arg => undef, default => sub { ['text/csv'] });
	
=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >>.

=cut

	sub serialize_iter_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= shift;
		my $first	= 1;
		while (my $t = $iter->next()) {
			if ($first) {
				$io->print(join("\t", map { "?$_" } $t->variables) . "\n");
				$first	= 0;
			}
			my @strings	= map { blessed($_) ? $_->ntriples_string : '' } $t->values;
			$io->print(join("\t", @strings) . "\n");
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
		my $data	= '';
		while (my $t = $iter->next()) {
			my @strings	= map { blessed($_) ? $_->ntriples_string : '' } $t->values;
			my $str		= join("\t", @strings);
			$data		.= $str . "\n";
		}
		return encode('UTF-8', $data);
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
