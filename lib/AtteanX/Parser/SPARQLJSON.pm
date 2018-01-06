=head1 NAME

AtteanX::Parser::SPARQLJSON - SPARQL JSON Parser

=head1 VERSION

This document describes AtteanX::Parser::SPARQLJSON version 0.018

=head1 SYNOPSIS

 use Attean;
 my $parser = Attean->get_parser('SPARQLJSON')->new();
 $parser->parse_list_from_io( $fh );

=head1 DESCRIPTION

...

=head1 ATTRIBUTES

=over 4

=item C<< canonical_media_type >>

=item C<< media_types >>

=item C<< file_extensions >>

=back

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Parser::SPARQLJSON 0.018 {
	use Attean;
	use Moo;
	use JSON;
	use Encode qw(decode);
	
	sub canonical_media_type { return "application/sparql-results+json" }

	sub media_types {
		return [qw(application/sparql-results+json)];
	}
	
	sub file_extensions { return [qw(srj)] }

	with 'Attean::API::ResultOrTermParser';
	with 'Attean::API::Parser';
	with 'Attean::API::AtOnceParser';

=item C<< parse_list_from_io( $fh ) >>

=cut

	sub parse_list_from_io {
		my $self	= shift;
		my $io		= shift;
		my $data	= do { local($/) = undef; <$io> };
		return $self->parse_list_from_bytes($data);
	}
	
=item C<< parse_list_from_bytes( $bytes ) >>

=cut

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
							$data{ $v }	= $self->new_iri( value => $data );
						} elsif ($type eq 'bnode') {
							my $data	= $value->{value};
							$data{ $v }	= Attean::Blank->new( $data );
						} elsif ($type eq 'literal') {
							my $data	= $value->{value};
							if (my $lang = $value->{'xml:lang'}) {
								$data{ $v }	= Attean::Literal->new( value => $data, language => $lang );
							} elsif (my $dt = $value->{'datatype'}) {
								my $iri		= $self->new_iri(value => $dt);
								$data{ $v }	= Attean::Literal->new( value => $data, datatype => $iri );
							} else {
								$data{ $v }	= Attean::Literal->new( $data );
							}
						} elsif ($type eq 'typed-literal') {
							my $data	= $value->{value};
							my $dt		= $value->{datatype};
							my $iri		= $self->new_iri(value => $dt);
							$data{ $v }	= Attean::Literal->new( value => $data, datatype => $iri );
						} else {
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

Copyright (c) 2014--2018 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
