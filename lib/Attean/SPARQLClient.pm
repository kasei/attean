use v5.14;
use warnings;

=head1 NAME

Attean::SPARQLClient - RDF blank nodes

=head1 VERSION

This document describes Attean::SPARQLClient version 0.035

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $client = Attean::SPARQLClient->new(endpoint => 'http://example.org/sparql');
  my $results = $client->query('SELECT * WHERE { ?s ?p ?o }');
  while (my $r = $results->next) {
    say $r->as_string;
  }

=head1 DESCRIPTION

The Attean::SPARQLClient class provides an API to execute SPARQL queries
against a remote SPARQL Protocol endpoint.

=head1 ATTRIBUTES

The following attributes exist:

=over 4

=item C<< endpoint >>

A URL of the remote service implementing the SPARQL 1.1 Protocol. This value
is a L<Attean::API::IRI>, but can be coerced from a string.

=item C<< silent >>

=item << user_agent >>

=item C<< request_signer >>

=back

=head1 METHODS

=over 4

=cut

package Attean::SPARQLClient 0.035 {
	use Moo;
	use Types::Standard qw(ConsumerOf Bool Str InstanceOf);
	use Encode qw(encode);
	use Scalar::Util qw(blessed);
	use URI::Escape;
	use Attean::RDF qw(iri);
	use namespace::clean;

	has 'endpoint' => (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], coerce => sub { iri(shift) }, required => 1);
	has 'silent' => (is => 'ro', isa => Bool, default => 0);
	has 'user_agent' => (is => 'rw', isa => InstanceOf['LWP::UserAgent'], default => sub { my $ua = LWP::UserAgent->new(); $ua->agent("Attean/$Attean::VERSION " . $ua->_agent); $ua });
	has 'request_signer' => (is => 'rw');

=item C<< query_request( $sparql ) >>

Returns an HTTP::Request object for the given SPARQL query string.

=cut

	sub query_request {
		my $self	= shift;
		my $sparql	= shift;
		my $endpoint	= $self->endpoint->value;
		my $uri			= URI->new($endpoint);
		my %params		= $uri->query_form;
		$params{'query'} = $sparql;
		$uri->query_form(%params);
		my $url			= $uri->as_string;
		my $req			= HTTP::Request->new('GET', $url);
		if (my $signer = $self->request_signer) {
			$signer->sign($req);
		}
		return $req;
	}

=item C<< query( $sparql ) >>

Executes the given SPARQL query string at the remote endpoint. If execution is
successful, returns an Attean::API::Iterator object with the results. If
execution fails but the client C<< silent >> flag is true, returns an empty
iterator. Otherwise raises an error via C<< die >>.

=cut

	sub query {
		my $self	= shift;
		my $sparql	= shift;
		my $req		= $self->query_request($sparql);
		my $silent	= $self->silent;
		my $ua		= $self->user_agent;
		
		my $response	= $ua->request($req);
		if (blessed($response) and $response->is_success) {
			my $type	= $response->header('Content-Type');
			my $pclass	= eval { Attean->get_parser(media_type => $type) or die "No parser for media type: $type" };
			if ($@) {
				if ($silent) {
					my $b	= Attean::Result->new( bindings => {} );
					return Attean::ListIterator->new(variables => [], values => [$b], item_type => 'Attean::API::Result');
				} else {
					die $@;
				}
			}
			if (not($pclass)) {
				if ($silent) {
					my $b	= Attean::Result->new( bindings => {} );
					return Attean::ListIterator->new(variables => [], values => [$b], item_type => 'Attean::API::Result');
				} else {
					die "No parser found for type [$type]";
				}
			}
			my $parser	= $pclass->new();
			my $xml		= $response->decoded_content;
			my $bytes	= encode('UTF-8', $xml, Encode::FB_CROAK);
			return $parser->parse_iter_from_bytes($bytes);
		} elsif ($silent) {
			my $b	= Attean::Result->new( bindings => {} );
			return Attean::ListIterator->new(variables => [], values => [$b], item_type => 'Attean::API::Result');
		} else {
			die "SPARQL Protocol error: " . $response->status_line;
		}
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO

L<SPARQL 1.1 Protocol|https://www.w3.org/TR/sparql11-protocol/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2022 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
