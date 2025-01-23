use v5.14;
use warnings;

# AtteanX::Parser::Trig
# -----------------------------------------------------------------------------

=head1 NAME

AtteanX::Parser::Trig - Trig RDF Parser

=head1 VERSION

This document describes AtteanX::Parser::Trig version 0.035

=head1 SYNOPSIS

 use Attean;
 my $parser	= AtteanX::Parser::Trig->new( handler => sub {...}, base => $base_iri );
 
 # Parse data from a file-handle and handle triples in the 'handler' callback
 $parser->parse_cb_from_io( $fh );
 
 # Parse the given byte-string, and return an iterator of triples
 my $iter = $parser->parse_iter_from_bytes('<s> <p> 1, 2, 3 .');
 while (my $triple = $iter->next) {
   print $triple->as_string;
 }

=head1 DESCRIPTION

This module implements a parser for the Trig RDF format.

=head1 ROLES

This class consumes L<Attean::API::Parser>, L<Attean::API::PushParser>,
<Attean::API::AbbreviatingParser>, and <Attean::API::TripleParser>.

=head1 ATTRIBUTES

=over 4

=item C<< canonical_media_type >>

=item C<< media_types >>

=item C<< file_extensions >>

=item C<< canonicalize >>

A boolean indicating whether term values should be canonicalized during parsing.

=back

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::Trig 0.035 {
	use Moo;
	use Types::Standard qw(Bool ArrayRef HashRef Str Maybe InstanceOf);
	use Types::Namespace qw( NamespaceMap );
	use utf8;
	use Carp qw(carp);
	use Encode qw(encode);
	use Scalar::Util qw(blessed);
	use Attean::API::Parser;
	use AtteanX::Parser::Turtle;
	use AtteanX::Parser::Turtle::Constants;
	use namespace::clean;

	my $RDF	= 'http://www.w3.org/1999/02/22-rdf-syntax-ns#';
	my $XSD	= 'http://www.w3.org/2001/XMLSchema#';
	
	extends 'AtteanX::Parser::Turtle';

	sub canonical_media_type { return "text/trig" }

	sub media_types {
		return [qw(text/trig)];
	}

	sub file_extensions { return [qw(trig)] }

	has 'canonicalize'	=> (is => 'rw', isa => Bool, default => 0);
	has '_map' => (is => 'ro', isa => HashRef[Str], default => sub { +{} });

	with 'Attean::API::MixedStatementParser';
	
	################################################################################

	# this is the entry point where we change the rules from Turtle to Trig
	sub _parse {
		my $self	= shift;
		my $l		= shift;
		$l->check_for_bom;
		while (my $t = $self->_next_nonws($l)) {
			$self->_trigDoc($l, $t);
		}
	}

	sub _trigDoc {
		my $self	= shift;
		my $l		= shift;
		my $t		= shift;
		my $type	= $t->type;
		if ($type == TURTLEPREFIX or $type == PREFIX) {
			$t	= $self->_get_token_type($l, PREFIXNAME);
			use Data::Dumper;
			unless (defined($t->value)) {
				my $tname	= AtteanX::Parser::Turtle::Constants::decrypt_constant($t->type);
				Carp::confess "undefined $tname token value: " . Dumper($t);
			}
			my $name	= $t->value;
			chop($name) if (substr($name, -1) eq ':');
# 			$name		=~ s/:$//;
			$t	= $self->_get_token_type($l, IRI);
			my %args	= (value => $t->value);
			if ($self->has_base) {
				$args{base}	= $self->base;
			}
			my $r	= $self->new_iri(%args);
			my $iri	= $r->as_string;
			if ($type == TURTLEPREFIX) {
				$t	= $self->_get_token_type($l, DOT);
	# 			$t	= $self->_next_nonws($l);
	# 			if ($t and $t->type != DOT) {
	# 				$self->_unget_token($t);
	# 			}
			}
			$self->_map->{$name}	= $iri;
			if ($self->has_namespaces) {
				my $ns	= $self->namespaces;
				unless ($ns->namespace_uri($name)) {
					$ns->add_mapping($name, $iri);
				}
			}
		}
		elsif ($type == TURTLEBASE or $type == BASE) {
			$t	= $self->_get_token_type($l, IRI);
			my %args	= (value => $t->value);
			if ($self->has_base) {
				$args{base}	= $self->base;
			}
			my $r	= $self->new_iri(%args);
			my $iri	= $r->as_string;
			if ($type == TURTLEBASE) {
				$t	= $self->_get_token_type($l, DOT);
	# 			$t	= $self->_next_nonws($l);
	# 			if ($t and $t->type != DOT) {
	# 				$self->_unget_token($t);
	# 			}
			}
			$self->base($iri);
		}
		else {
			$self->_block( $l, $t );
		}
	# 	}
	}

	sub _block {
		my $self	= shift;
		my $l		= shift;
		my $t		= shift;
		my $type	= $t->type;
		if ($type == GRAPH) {
			# "GRAPH" labelOrSubject wrappedGraph
			my $graph	= $self->_labelOrSubject($l);
			local($self->{graph})	= $graph;
			$t	= $self->_get_token_type($l, LBRACE);
			$self->_block($l, $t);
		} elsif ($type == LBRACE) {
			$t		= $self->_next_nonws($l);
			$type	= $t->type;
			while ($type != RBRACE) {
				$self->_triple($l, $t);
				$t		= $self->_next_nonws($l);
				$type	= $t->type;
				unless ($type == RBRACE or $type == DOT) {
					carp "Expected DOT or closing brace";
				}
				if ($type == DOT) {
					$t		= $self->_next_nonws($l);
					$type	= $t->type;
				}
			}
		} else {
			$self->_triple($l, $t);
			$t	= $self->_get_token_type($l, DOT);
		}
	}
	
	sub _labelOrSubject {
		my $self	= shift;
		my $l		= shift;
		my $t		= $self->_next_nonws($l);
		if ($t->type == IRI or $t->type == PREFIXNAME or $t->type == BNODE) {
			return $self->_token_to_node($t);
		} else {
			$self->_throw_error(sprintf("Expecting graph name but got %s", decrypt_constant($t->type)), $t, $l);
		}
	}
	
	sub _assert_triple {
		my $self	= shift;
		my $subj	= shift;
		my $pred	= shift;
		my $obj		= shift;
		if ($self->canonicalize and blessed($obj) and $obj->does('Attean::API::Literal')) {
			$obj	= $obj->canonicalize;
		}
	
		my $graph = $self->{graph};
		my $t		= (defined($graph))
					? Attean::Quad->new($subj, $pred, $obj, $graph)
					: Attean::Triple->new($subj, $pred, $obj);
		$self->handler->($t);
		return $t;
	}

	sub _throw_error {
		my $self	= shift;
		my $message	= shift;
		my $t		= shift;
		my $l		= shift;
		my $line	= $t->start_line;
		my $col		= $t->start_column;
	# 	Carp::cluck "$message at $line:$col";
		my $text	= "$message at $line:$col";
		if (defined($t->value)) {
			$text	.= " (near '" . $t->value . "')";
		}
		Carp::cluck "TriG parser error";
		die $text;
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

Copyright (c) 2014--2022 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
