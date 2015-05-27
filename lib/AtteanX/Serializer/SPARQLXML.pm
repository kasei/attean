=head1 NAME

AtteanX::Serializer::SPARQLXML - SPARQL Results XML Serializer

=head1 VERSION

This document describes AtteanX::Serializer::SPARQLXML version 0.005

=head1 SYNOPSIS

 use Attean;
 my $s = Attean->get_serializer('SPARQLXML')->new();
 $s->serialize_iter_to_io( $fh, $iter );

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Serializer::SPARQLXML 0.005 {
	use Moo;
	use Types::Standard qw(Str ArrayRef);
	use Encode qw(encode);
	use Scalar::Util qw(blessed);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);
	use namespace::clean;

	has 'canonical_media_type' => (is => 'ro', isa => Str, init_arg => undef, default => 'application/sparql-results+xml');

=item C<< media_types >>

Returns a list of media types that identify the format produced by this serializer.

=cut

	sub media_types {
		return [qw(application/sparql-results+xml)];
	}
	
=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >>.

=cut

	sub serialize_iter_to_io {
		my $self	= shift;
		my $fh		= shift;
		my $iter	= shift;
		print {$fh} <<"END";
<?xml version="1.0" encoding="utf-8"?>
<sparql xmlns="http://www.w3.org/2005/sparql-results#">
<head>
END
		if ($iter->does('Attean::API::ResultIterator')) {
			foreach my $v (@{ $iter->variables }) {
				print $fh qq(\t<variable name="$v"/>\n);
			}
		}

		print {$fh} <<"END";
</head>
<results>
END
		
		while (my $t = $iter->next()) {
			print $fh "\t\t<result>\n";
			foreach my $name ($t->variables) {
				my $term	= $t->value($name);
				if (blessed($term)) {
					if ($term->does('Attean::API::IRI')) {
						my $label	= $term->value;
						$label	=~ s/&/&amp;/g;
						$label	=~ s/</&lt;/g;
						$label	=~ s/"/&quot;/g;
						print $fh qq(\t\t\t<binding name="${name}"><uri>${label}</uri></binding>\n);
					} elsif ($term->does('Attean::API::Literal')) {
						my $label	= $term->value;
						$label	=~ s/&/&amp;/g;
						$label	=~ s/</&lt;/g;
						$label	=~ s/"/&quot;/g;
						if (my $lang = $term->language) {
							$label	= qq(<literal xml:lang="${lang}">${label}</literal>);
						} elsif (my $dt = $term->datatype) {
							$label	= qq(<literal datatype=") . $dt->value . qq(">${label}</literal>);
						} else {
							$label	= qq(<literal>${label}</literal>);
						}
						print $fh qq(\t\t\t<binding name="${name}">${label}</binding>\n);
					} elsif ($term->does('Attean::API::Blank')) {
						my $label	= $term->value;
						$label	=~ s/&/&amp;/g;
						$label	=~ s/</&lt;/g;
						$label	=~ s/"/&quot;/g;
						print $fh qq(\t\t\t<binding name="${name}"><bnode>${label}</bnode></binding>\n);
					} else {
						die;
					}
				}
			}
			print $fh "\t\t</result>\n";
		}
		print {$fh} "</results>\n";
		print {$fh} "</sparql>\n";
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
		open(my $fh, '>', \$data);
		$self->serialize_iter_to_io($fh, $iter);
		close($fh);
		use Devel::Peek;
		Dump($data);
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
