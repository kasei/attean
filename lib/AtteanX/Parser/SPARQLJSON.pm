=head1 NAME

AtteanX::Parser::SPARQLJSON - SPARQL JSON Parser

=head1 VERSION

This document describes AtteanX::Parser::SPARQLJSON version 0.002

=head1 SYNOPSIS

 use Attean;
 my $parser = Attean->get_parser('SPARQLJSON')->new();
 $parser->parse_list_from_io( $fh );

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Parser::SPARQLJSON 0.001 {
	use Attean;
	use Moo;
	use JSON;
	use Encode qw(decode);
	
=item C<< canonical_media_type >>

Returns the canonical media type for SPARQL XML: application/sparql-results+json.

=cut

	sub canonical_media_type { return "application/sparql-results+json" }

=item C<< media_types >>

Returns a list of media types that may be parsed with the SPARQL XML parser:
application/sparql-results+json.

=cut

	sub media_types {
		return [qw(application/sparql-results+json)];
	}
	
=item C<< file_extensions >>

Returns a list of file extensions that may be parsed with the parser.

=cut

	sub file_extensions { return [qw(srj)] }

	with 'Attean::API::ResultOrTermParser';
	with 'Attean::API::AtOnceParser';

	sub parse_list_from_io {
		my $self	= shift;
		my $io		= shift;
		my $data	= do { local($/) = undef; <$io> };
		return $self->parse_list_from_bytes($data);
	}
	
	sub parse_list_from_bytes {
		my $self	= shift;
		my $octets	= shift;
		my $json	= decode('UTF-8', $octets, Encode::FB_CROAK);
		my $data	= from_json($json, {utf8 => 1});
		my $head	= $data->{head};
		my $vars	= $head->{vars};
		my $res		= $data->{results};
		if (defined(my $bool = $data->{boolean})) {
			return ($bool) ? Attean::Literal->true : Attean::Literal->false;
		} elsif (my $binds = $res->{bindings}) {
			my @results;
			foreach my $b (@$binds) {
				my %data;
				foreach my $v (@$vars) {
					if (defined(my $value = $b->{ $v })) {
						my $type	= $value->{type};
						if ($type eq 'uri') {
							my $data	= $value->{value};
							$data{ $v }	= Attean::IRI->new( $data );
						} elsif ($type eq 'bnode') {
							my $data	= $value->{value};
							$data{ $v }	= Attean::Blank->new( $data );
						} elsif ($type eq 'literal') {
							my $data	= $value->{value};
							if (my $lang = $value->{'xml:lang'}) {
								$data{ $v }	= Attean::Literal->new( value => $data, language => $lang );
							} else {
								$data{ $v }	= Attean::Literal->new( $data );
							}
						} elsif ($type eq 'typed-literal') {
							my $data	= $value->{value};
							my $dt		= $value->{datatype};
							$data{ $v }	= Attean::Literal->new( value => $data, datatype => $dt );
						} else {
							warn Dumper($data, $b);
							die "Unknown node type $type during parsing of SPARQL JSON Results";
						}
					}
				}
				push(@results, Attean::Result->new( bindings => \%data ));
			}
			return @results;
		}
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

Copyright (c) 2006-2012 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
