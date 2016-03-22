# TODO: If the model supports caching roles, add headers and check for http 304
# TODO: Implement and support Accept-Language models (port from RDF::Trine::Store::LanguagePreference)
# TODO: Add next/prev link headers if query is paged
# TODO: Add configuration and link headers to indicate LDF/SPARQL mirrors

use v5.14;
use warnings;

package Attean::Error {
	use Moo;
	use Types::Standard qw(Str HashRef);
	use namespace::clean;

	has 'message'	=> (is => 'ro', isa => Str, required => 1);
	has 'details'	=> (is => 'ro', isa => HashRef, default => sub { +{} });
	has 'uri'		=> (is => 'ro', isa => Str);
}

package Attean::Endpoint::Error {
	use Moo;
	extends 'Attean::Error';
	use Types::Standard qw(Int);
	use namespace::clean;
	has 'code' => (is => 'ro', isa => Int, required => 1);
}

package Attean::Endpoint::ClientError {
	use Moo;
	extends 'Attean::Endpoint::Error';
	use Types::Standard qw(Int);
	use namespace::clean;

	has 'code' => (is => 'ro', isa => Int, default => 400);
}

package Attean::Endpoint::ServerError {
	use Moo;
	extends 'Attean::Endpoint::Error';
	use Types::Standard qw(Int);
	use namespace::clean;

	has 'code' => (is => 'ro', isa => Int, default => 500);
}

=head1 NAME

Attean::Endpoint - SPARQL Protocol Endpoint

=head1 VERSION

This document describes Attean::Endpoint version 0.010

=head1 SYNOPSIS

  use v5.14;
  use Attean;

=head1 DESCRIPTION

The Attean::Endpoint class implements a PSGI SPARQL Protocol endpoint.

=head1 METHODS

=over 4

=cut

package Plack::App::Attean::Endpoint 0.010 {
	use parent qw(Plack::Component);
	use Plack::Request;
	
	sub configure {
		my $self	= shift;
		$self->{config}	= shift;
		return $self;
	}
	
	sub prepare_app {
		my $self = shift;
		my $config = $self->{config};
		$self->{endpoint} = eval { Attean::Endpoint->new( $config ) };
		if ($@) {
			warn $@;
		}
	}

	sub call {
		my($self, $env) = @_;
		my $req	= Plack::Request->new($env);
		unless ($req->method =~ /^(GET|HEAD|POST)$/) {
			return [ 405, [ 'Content-type', 'text/plain' ], [ 'Method not allowed' ] ];
		}

		my $ep		= $self->{endpoint};
		my $resp	= $ep->run( $req );
		return $resp->finalize;
	}
}

package Attean::Endpoint 0.010 {
	use Moo;
	use Attean;
	use TryCatch;
	use JSON;
	use Encode;
	use Plack::Request;
	use Plack::Response;
	use Scalar::Util qw(blessed refaddr);
	use List::MoreUtils qw(any);
	use File::ShareDir qw(dist_dir);
	use HTTP::Negotiate qw(choose);
	use IO::Compress::Gzip qw(gzip);
	use HTML::HTML5::Parser;
	use HTML::HTML5::Writer qw(DOCTYPE_XHTML_RDFA);
	use Carp qw(croak);
	use Types::Standard qw(ConsumerOf CodeRef HashRef ArrayRef Str Int);
	use AtteanX::RDFQueryTranslator;
# 	use IO::Handle;
# 	use Digest::MD5 qw(md5_base64);
# 	use XML::LibXML 1.70;
# 	use RDF::RDFa::Generator 0.102;
# 	use Hash::Merge::Simple qw/ merge /;
# 	use Fcntl qw(:flock SEEK_END);
	use namespace::clean;

	with 'MooX::Log::Any';

	has 'planner' => (
		is => 'ro',
		isa => ConsumerOf['Attean::API::QueryPlanner'],
		required => 1,
		default => sub {
			Attean::IDPQueryPlanner->new();
		}
	);
	has 'model' => (is => 'ro', isa => ConsumerOf['Attean::API::Model'], required => 1);
	has 'conf'	=> (is => 'ro', isa => HashRef, required => 1);
	has 'graph'	=> (is => 'ro', isa => ConsumerOf['Attean::API::IRI'], required => 1);
	
	sub BUILDARGS {
		my $class		= shift;
		my @params = @_;
		my %args;
		if (blessed($params[0]) and $params[0]->does('Attean::API::Model')) {
			# ->new( $model, \%conf )
			$args{ model }	= shift @params;
			$args{ conf }	= shift @params;
			$args{ graph }	= Attean::IRI->new('http://example.org/graph');
		} elsif (any { blessed($_) && $_->does('Attean::API::Model') } @params) {
			# Assume the buildargs can be taken directly
			return $class->SUPER::BUILDARGS(@params);
		} else {
			# ->new( \%conf )
			my $conf		= shift @params;
			my $store_conf	= $conf->{store};
			my ($name, $file)	= split(';', $store_conf, 2);
			my $sclass	= Attean->get_store($name)->new();
			my $store	= $sclass->new();
			my $model	= Attean::MutableQuadModel->new( store => $store );
			
			my $graph	= Attean::IRI->new('http://example.org/graph');
			if (defined($file) and length($file)) {
				$graph		= Attean::IRI->new('file://' . File::Spec->rel2abs($file));
				open(my $fh, '<:encoding(UTF-8)', $file) or die $!;
 				#$self->log->debug("Parsing data from $file...");
				my $pclass	= Attean->get_parser( filename => $file ) // 'AtteanX::Parser::Turtle';
				my $parser	= $pclass->new(base => $graph);
				my $iter	= $parser->parse_iter_from_io($fh);
				my $quads	= $iter->as_quads($graph);
				$model->add_iter($quads);
			}
			
			$args{ model }	= $model;
			$args{ conf }	= $conf;
			$args{ graph }	= $graph;
		}
		
		return $class->SUPER::BUILDARGS(%args);
	}

=item C<< run ( $request ) >>

Run the SPARQL request contained in the given C<< $request >> object and return
a response object.

=cut
	
	sub run {
		my $self	= shift;
		my $req		= shift;
		try {
			return $self->_run($req, @_);
		}
		catch (Attean::Endpoint::Error $e) {
			my $resp	= Plack::Response->new;
			my $code	= $e->code;
			my $status	= $e->message;
			my $error	= {
				title		=> $status,
				describedby	=> $e->uri,
			};
			if (my $d = $e->details) {
				$error->{details}		= $d;
			}
			my @variants	= (
				['text/plain', 0.98, 'text/plain'],
				['application/json-problem', 0.99, 'application/json-problem'],
			);
			my $headers	= $req->headers;
			my $stype	= choose( \@variants, $headers ) || 'text/plain';
			if ($stype eq 'application/json-problem') {
				$resp->headers->content_type( 'application/json-problem' );
				$resp->status($code);
				my $content	= encode_json($error);
				$resp->body($content);
			} else {
				$resp->headers->content_type( 'text/plain' );
				$resp->status($code);
				my @messages	= grep { defined($_) } @{ $error }{ qw(title detail) };
				my $content		= join("\n\n", $status, @messages);
				$resp->body($content);
			}
			return $resp;
		}
	}
	
	sub _run {
		my $self	= shift;
		my $req		= shift;
		
		my $config	= $self->{conf};
		my $endpoint_path = $config->{endpoint}{endpoint_path} || '/sparql';
		my $model	= $self->{model};
		
		my $response	= Plack::Response->new;

		our $VERSION;
		my $server = "Attean::Endpoint/$VERSION";
		$server .= " " . $response->headers->header('Server') if defined($response->headers->header('Server'));
		$response->headers->header('Server' => $server);

		unless ($req->path eq $endpoint_path) {
			my $content;
			my $path	= $req->path_info;
			$path		=~ s#^/##;
			my $dir		= $ENV{ATTEAN_ENDPOINT_SHAREDIR} || File::Spec->catdir((eval { dist_dir('Attean-Endpoint') } || 'share'), 'endpoint');
			my $abs		= File::Spec->rel2abs($dir);
			my $file	= File::Spec->catfile($abs, 'www', $path);
			if (-r $file) {
				open( my $fh, '<', $file ) or croak $!;
				$response->status(200);
				$content	= $fh;
			} else {
				my $path	= $req->path;
				$response->status(404);
				$content	= <<"END";
	<!DOCTYPE HTML PUBLIC "-//IETF//DTD HTML 2.0//EN">\n<html><head>\n<title>404 Not Found</title>\n</head><body>\n
	<h1>Not Found</h1>\n<p>The requested URL $path was not found on this server.</p>\n</body></html>
END
			}
			$response->body($content);
			return $response;
		}
	
		my $headers	= $req->headers;
		my $type	= $headers->header('Accept') || 'application/sparql-results+xml';
		if (my $t = $req->param('media-type')) {
			$type	= $t;
			$headers->header('Accept' => $type);
		}
	
		my $ae		= $req->headers->header('Accept-Encoding') || '';
	
		my $sparql;
		my $content;
		my $ct	= $req->header('Content-type');
		if ($req->method !~ /^(GET|POST)$/i) {
			my $method	= uc($req->method);
			$content	= "Unexpected method $method (expecting GET or POST)";
			$self->log_error( $req, $content );
			$response->header('Allow' => 'GET, POST');
			die Attean::Endpoint::ClientError->new(code => 405, message => 'Method not allowed', uri => 'http://id.kasei.us/rdf-endpoint/error/bad_http_method');
		} elsif (defined($ct) and $ct eq 'application/sparql-query') {
			$sparql	= $req->content;
		} elsif (defined($ct) and $ct eq 'application/sparql-update') {
			if ($config->{endpoint}{update} and $req->method eq 'POST') {
				$sparql	= $req->content;
			}
		} elsif ($req->param('query')) {
			my @sparql	= $req->param('query');
			if (scalar(@sparql) > 1) {
				$content	= "More than one query string submitted";
				$self->log_error( $req, $content );
				die Attean::Endpoint::ClientError->new(code => 400, message => 'Multiple query strings not allowed', uri => 'http://id.kasei.us/rdf-endpoint/error/multiple_queries');
			} else {
				$sparql = $sparql[0];
			}
		} elsif ($req->param('update')) {
			my @sparql	= $req->param('update');
			if (scalar(@sparql) > 1) {
				$content	= "More than one update string submitted";
				$self->log_error( $req, $content );
				die Attean::Endpoint::ClientError->new(code => 400, message => 'Multiple update strings not allowed', uri => 'http://id.kasei.us/rdf-endpoint/error/multiple_updates');
			}
		
			if ($config->{endpoint}{update} and $req->method eq 'POST') {
				$sparql = $sparql[0];
			} elsif ($req->method ne 'POST') {
				my $method	= $req->method;
				$content	= "Update operations must use POST";
				$self->log_error( $req, $content );
				$response->header('Allow' => 'POST');
				die Attean::Endpoint::ClientError->new(code => 405, message => "$method Not Allowed for Update Operation", uri => 'http://id.kasei.us/rdf-endpoint/error/bad_http_method_update');
			}
		}
	
		if ($sparql) {
			my %args;
			$args{ update }		= 1 if ($config->{endpoint}{update} and $req->method eq 'POST');
			$args{ load_data }	= 1 if ($config->{endpoint}{load_data});
		
			my $protocol_specifies_update_dataset	= 0;
			{
				my @default	= $req->param('default-graph-uri');
				my @named	= $req->param('named-graph-uri');
				if (scalar(@default) or scalar(@named)) {
					delete $args{ load_data };
					# TODO: handle custom-dataset
					$self->log->warn('custom query datasets not supported yet');
# 					$model	= Attean::MutableQuadModel->new( store => Attean->get_store('Memory')->new() );
# 					foreach my $url (@named) {
# 						RDF::Trine::Parser->parse_url_into_model( $url, $model, context => iri($url) );
# 					}
# 					foreach my $url (@default) {
# 						RDF::Trine::Parser->parse_url_into_model( $url, $model );
# 					}
				}
			}
		
			{
				my @default	= $req->param('using-graph-uri');
				my @named	= $req->param('using-named-graph-uri');
				if (scalar(@named) or scalar(@default)) {
					$protocol_specifies_update_dataset	= 1;
					# TODO: handle custom-dataset
					$self->log->warn('custom update datasets not supported yet');
# 					$model	= RDF::Trine::Model::Dataset->new( $model );
# 					$model->push_dataset( default => \@default, named => \@named );
				}
			}
		
# 			my $match	= $headers->header('if-none-match') || '';
# 			my $etag	= md5_base64( join('#', $self->run_tag, $model->etag, $type, $ae, $sparql) );
# 			if (length($match)) {
# 				if (defined($etag) and ($etag eq $match)) {
# 					$response->status(304);
# 					return $response;
# 				}
# 			}
		
			my $base	= $req->base;
			my $parser	= Attean->get_parser('SPARQL')->new(base => $base);
			$parser->update(1) if ($args{update});
			my ($algebra)	= eval { $args{update} ? $parser->parse_update($sparql, base => $base) : $parser->parse($sparql, base => $base) };
			if ($@ or not($algebra)) {
				my $error	= $@ || 'Internal error';
				$self->log_error( $req, $error );
				my $eclass	= ($error =~ /Syntax/) ? 'Attean::Endpoint::ClientError' : 'Attean::Endpoint::ServerError';
				if ($req->method ne 'POST' and $error =~ /read-only queries/sm) {
					$error	= 'Updates must use a HTTP POST request.';
					die $eclass->new(message => 'Updates must use a HTTP POST request', uri => 'http://id.kasei.us/rdf-endpoint/error/bad_http_method_update');
				} else {
					die $eclass->new(message => 'SPARQL query/update parse error', uri => 'http://id.kasei.us/rdf-endpoint/error/parse_error', details => { error => $error, sparql => $sparql });
				}
			} else {
				$self->log_query( $req, $sparql );
				# TODO: handle case where query specifies update dataset
# 				if ($protocol_specifies_update_dataset and $query->specifies_update_dataset) {
# 					my $method	= $req->method;
# 					$content	= "Update operations cannot specify a dataset in both the query and with protocol parameters";
# 					$self->log_error( $req, $content );
# 					die Attean::Endpoint::ClientError->new(code => 400, message => 'Multiple datasets specified for update', uri => 'http://id.kasei.us/rdf-endpoint/error/update_specifies_multiple_datasets');
# 				}

				if ($self->log->is_trace) {
					$self->log->trace("Algebra:\n" . $algebra->as_string);
				}
				my $graph	= $self->graph;
				my $default_graphs	= [$graph];
				my $planner	= $self->planner;
				if ($self->log->is_trace) {
					$self->log->debug('Planning with default graphs:');
					foreach my $g (@$default_graphs) {
						$self->log->trace($g->as_string);
					}
				}
				my $plan	= $planner->plan_for_algebra($algebra, $model, $default_graphs);
				if ($self->log->is_debug) {
					$self->log->debug("Plan:\n" . $plan->as_string);
				}
				eval {
					my $iter	= $plan->evaluate($model);
					$response->status(200);
					my $sclass	= Attean->negotiate_serializer(request_headers => $headers) // Attean->get_serializer('sparqlxml');
					$self->log->debug("Serializer class: $sclass");
					my $s		= $sclass->new();
					$content	= $s->serialize_iter_to_bytes($iter);
					my $stype	= $s->canonical_media_type;
					$response->headers->content_type($stype);
				};
				if ($@) {
					my $error	= $@;
					$self->log->fatal($error);
					die Attean::Endpoint::ServerError->new(code => 500, message => 'SPARQL query/update execution error', uri => 'http://id.kasei.us/rdf-endpoint/error/execution_error', details => { error => $@, sparql => $sparql });
				}
			}
		} elsif ($req->method eq 'POST') {
			$content	= "POST without recognized query or update";
			$self->log_error( $req, $content );
			die Attean::Endpoint::ClientError->new(message => 'Missing SPARQL Query/Update String', uri => 'http://id.kasei.us/rdf-endpoint/error/missing_sparql_string');
		} else {
			my $stype		= 'text/html';
			my $dir			= $ENV{ATTEAN_ENDPOINT_SHAREDIR} || File::Spec->catdir((eval { dist_dir('Attean-Endpoint') } || 'share'), 'endpoint');
			my $template	= File::Spec->catfile($dir, 'index.html');
			my $parser		= HTML::HTML5::Parser->new;
			my $doc			= $parser->parse_file( $template );
# 			my $gen			= RDF::RDFa::Generator->new( style => 'HTML::Head');
# 			$gen->inject_document($doc, $sdmodel);
		
			my $writer	= HTML::HTML5::Writer->new( markup => 'xhtml', doctype => DOCTYPE_XHTML_RDFA );
			$content	= encode_utf8( $writer->document($doc) );
			$response->status(200);
			$response->headers->content_type('text/html');
		}
	
		$content	= $response->body || $content;
		my $length	= 0;
		my %ae		= map { $_ => 1 } split(/\s*,\s*/, $ae);
		if ($ae{'gzip'}) {
			my $orig	= length($content);
			my ($rh, $wh);
			pipe($rh, $wh);
			if (ref($content)) {
				gzip $content => $wh;
			} else {
				gzip \$content => $wh;
			}
			close($wh);
			my $body	= do { local($/) = undef; <$rh> };
			$self->log->info("Compressed $orig bytes to " . length($body) . " bytes");
			$length		= bytes::length($body);
			$response->headers->header('Content-Encoding' => 'gzip');
			$response->headers->header('Content-Length' => $length);
			$response->body( $body ) unless ($req->method eq 'HEAD');
		} else {
			local($/)	= undef;
			my $body	= ref($content) ? <$content> : $content;
			$length		= bytes::length($body);
			$response->headers->header('Content-Length' => $length);
			$response->body( $body ) unless ($req->method eq 'HEAD');
		}
		return $response;
	}
	
=item C<< log_query ( $message ) >>

=cut

	sub log_query {
		my $self	= shift;
		my $req		= shift;
		my $message	= shift;
		$self->log->info($message);
		$self->_log( $req, { level => 'info', message => $message } );
	}

=item C<< log_error ( $message ) >>

=cut

	sub log_error {
		my $self	= shift;
		my $req		= shift;
		my $message	= shift;
		$self->log->error($message);
		$self->_log( $req, { level => 'error', message => $message } );
	}

	sub _log {
		my $self	= shift;
		my $req		= shift;
		my $data	= shift;
		my $logger	= $req->logger || sub {};
	
		$logger->($data);
	}

	sub _set_response_error {
		my $self	= shift;
		my $req		= shift;
		my $resp	= shift;
		my $code	= shift;
		my $error	= shift;
		my @variants	= (
			['text/plain', 1.0, 'text/plain'],
			['application/json-problem', 0.99, 'application/json-problem'],
		);
		my $headers	= $req->headers;
		my $stype	= choose( \@variants, $headers ) || 'text/plain';
		if ($stype eq 'application/json-problem') {
			$resp->headers->content_type( 'application/json-problem' );
			$resp->status($code);
			my $content	= encode_json($error);
			$resp->body($content);
		} else {
			$resp->headers->content_type( 'text/plain' );
			$resp->status($code);
			my @messages	= grep { defined($_) } @{ $error }{ qw(title detail) };
			my $content		= join("\n\n", @messages);
			$resp->body($content);
		}
		return;
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO

L<http://www.perlrdf.org/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
