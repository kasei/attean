=head1 NAME

AtteanX::Serializer::SPARQLJSON - SPARQL Results JSON Serializer

=head1 VERSION

This document describes AtteanX::Serializer::SPARQLJSON version 0.016

=head1 SYNOPSIS

 use Attean;
 my $s = Attean->get_serializer('SPARQLJSON')->new();
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

package AtteanX::Serializer::SPARQLJSON 0.016 {
        use Moo;
        use Types::Standard qw(Bool Str);
        use Encode qw(encode);
        use Scalar::Util qw(blessed);
        use Attean::ListIterator;
        use List::MoreUtils qw(any);
	use JSON -support_by_pp;
        use namespace::clean;

        has 'distinct'             => (is => 'ro', isa => Bool, init_arg => undef, default => 0 );
        has 'ordered'              => (is => 'ro', isa => Bool, init_arg => undef, default => 0 );
        has 'canonical_media_type' => (is => 'ro', isa => Str,  init_arg => undef, default => 'application/sparql-results+json');

=item C<< media_types >>

Returns a list of media types that identify the format produced by this serializer.

=cut

	sub media_types {
		return [qw(application/sparql-results+json)];
	}

=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >>.

=cut

	sub serialize_iter_to_io {
		my $self        = shift;
		my $fh          = shift;
		my $iter        = shift;

                my @vars        = @{ $iter->variables };
                my $distinct    = $self->distinct ? JSON::true : JSON::false;
                my $ordered     = $self->ordered  ? JSON::true : JSON::false;

                my $data        = {
				        head    => { vars => \@vars },
				        results => { ordered => $ordered, distinct => $distinct, bindings => [] },
				  };

                while (my $t = $iter->next()) {
                        foreach my $name ($t->variables) {
                                my $term = $t->value($name);
                                if (blessed($term)) {
                                        my $type;
                                        if ($term->does('Attean::API::IRI')) {
                                                $type = 'uri';
                                        } elsif ($term->does('Attean::API::Literal')) {
                	                        $type = 'literal';
        	                        } elsif ($term->does('Attean::API::Blank')) {
                                                $type = 'bnode';
                                        } else {
                                                die 'Term object has an unrecognized type: ' . ref($term);
                                        }
                                push(@{ $data->{results}{bindings} }, $name => { type => $type, value => $term->value } );
                                }
                        }
                }

		print {$fh} JSON->new->utf8(1)->canonical(1)->pretty(0)->encode($data);
		return;
	}

=item C<< serialize_iter_to_bytes( $fh, $iterator ) >>

Serializes the L<Attean::API::Binding> objects from C<< $iterator >>
and returns the serialization as a UTF-8 encoded byte string.

=cut

	sub serialize_iter_to_bytes {
		my $self        = shift;
		my $iter        = shift;
		my $data        = encode('UTF-8', '');
		open(my $fh, '>', \$data);
		$self->serialize_iter_to_io($fh, $iter);
		close($fh);
		return $data;
	}

	with 'Attean::API::ResultSerializer', 'Attean::API::AppendableSerializer';
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2006-2016 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
