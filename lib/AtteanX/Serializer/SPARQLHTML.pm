=head1 NAME

AtteanX::Serializer::SPARQLHTML - SPARQL Results HTML Serializer

=head1 VERSION

This document describes AtteanX::Serializer::SPARQLHTML version 0.023

=head1 SYNOPSIS

 use Attean;
 my $s = Attean->get_serializer('SPARQLHTML')->new();
 $s->serialize_iter_to_io( $fh, $iter );

=head1 DESCRIPTION

...

=head1 ATTRIBUTES

=over 4

=item C<< canonical_media_type >>

=back

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Serializer::SPARQLHTML 0.023 {
	use Moo;
	use Types::Standard qw(Str Bool ArrayRef);
	use Encode qw(encode);
	use Scalar::Util qw(blessed);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);
	use namespace::clean;

	has 'full_document' => (is => 'rw', isa => Bool, default => 1);
	has 'canonical_media_type' => (is => 'ro', isa => Str, init_arg => undef, default => 'text/html');

=item C<< media_types >>

Returns a list of media types that identify the format produced by this serializer.

=cut

	sub media_types {
		return [qw(text/html)];
	}
	
=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >>.

=cut

	sub serialize_iter_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= shift;
		if ($self->full_document) {
			$io->print(<<"END");
<!DOCTYPE html>
<html><head><title>SPARQL Results</title></head><body>
<div id="result">
<h2>Results</h2>
END
		}
		my @names;
		my $count	= 0;
		my $first	= 1;
		while (my $t = $iter->next()) {
			$count++;
			if ($first) {
				$io->print("<table class='sparqlresults'>\n<thead><tr>\n");
				@names	= $t->variables;
				foreach my $name (@names) {
					$io->print("\t<th>" . $name . "</th>\n");
				}
				$io->print("</tr></thead>\n");
				$first	= 0;
			}
			
			$io->print("<tr>\n");
			foreach my $k (@names) {
				my $term	= $t->value($k);
				my $value	= $self->node_as_html($term) // '';
				$io->print("\t<td>$value</td>\n");
			}
			$io->print("</tr>\n");
		}
		unless ($first) {
			my $columns	= scalar(@names);
			$io->print("<tfoot><tr><th colspan=\"$columns\">Total: $count</th></tr></tfoot>\n</table>\n");
		}
		if ($self->full_document) {
			$io->print("</div>\n</body></html>\n");
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
		my $data	= '';
		open(my $fh, '>:utf8', \$data);
		$self->serialize_iter_to_io($fh, $iter);
		close($fh);
		return $data;
	}

=item C<< node_as_html($node) >>

Serializes the L<Attean::API::Term> object as HTML.

=cut

	sub node_as_html {
		my $self	= shift;
		my $node	= shift;
		return '' unless (blessed($node));
		if ($node->does('Attean::API::IRI')) {
			my $uri	= $node->value;
			for ($uri) {
				s/&/&amp;/g;
				s/</&lt;/g;
			}
			my $html = qq[<a href="${uri}">$uri</a>];

			if (my $map = $self->namespaces) {
				my $abr = $map->abbreviate($uri);

				if ($abr) {
					return qq[<a href="${uri}">$abr</a>];
				} else {
					return $html;
				}

			} else {
			
				return $html;
			}


# 			if ($link) {
# 				$html	= qq[<a href="${uri}">$html</a>];
# 			}
		} elsif ($node->does('Attean::API::Literal')) {
			my $html	= $node->value;
			for ($html) {
				s/&/&amp;/g;
				s/</&lt;/g;
			}
			return $html;
		} else {
			my $html	= $node->value;
			for ($html) {
				s/&/&amp;/g;
				s/</&lt;/g;
			}
			return $html;
		}
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

Copyright (c) 2014--2019 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
