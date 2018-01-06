=head1 NAME

AtteanX::Serializer::RDFXML - RDF/XML Serializer

=head1 VERSION

This document describes AtteanX::Serializer::RDFXML version 0.018

=head1 SYNOPSIS

 use Attean;
 my $s = Attean->get_serializer('RDFXML')->new();
 $s->serialize_iter_to_io( $fh, $iter );

=head1 DESCRIPTION

...

=head1 ATTRIBUTES

=over 4

=item C<< canonical_media_type >>

=item C<< scoped_namespaces >>

=item C<< file_extensions >>

=back

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Serializer::RDFXML 0.018 {
	use Moo;
	use Types::Standard qw(Str ArrayRef HashRef);
	use Encode qw(encode);
	use Scalar::Util qw(blessed);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);
	use namespace::clean;

	has 'canonical_media_type' => (is => 'ro', isa => Str, init_arg => undef, default => 'application/rdf+xml');
	has '_rev' => (is => 'rw', isa => HashRef, init_arg => undef, default => sub { +{} });
	has 'scoped_namespaces' => (is => 'rw', init_arg => undef);

	sub file_extensions { return [qw(rdf xml)] }
	
=item C<< media_types >>

Returns a list of media types that identify the format produced by this serializer.

=cut

	sub media_types {
		return [qw(application/rdf+xml)];
	}
	
=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the L<Attean::API::Triple> objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >> (which SHOULD be open with the UTF-8 encoding).

=cut

	sub serialize_iter_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= shift;
		
		my $ns		= $self->_top_xmlns();
		my $base_uri	= '';
		if ($self->{base_uri}) {
			$base_uri = "xml:base=\"$self->{base_uri}\" ";
		}
		print {$io} qq[<?xml version="1.0" encoding="utf-8"?>\n<rdf:RDF ${base_uri}xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"$ns>\n];
		
		my $st			= $iter->next;
		my @statements;
		push(@statements, $st) if blessed($st);
		while (@statements) {
			my $st	= shift(@statements);
			my @samesubj;
			push(@samesubj, $st);
			my $subj	= $st->subject;
			while (my $row = $iter->next) {
				if ($row->subject->equals( $subj )) {
					push(@samesubj, $row);
				} else {
					push(@statements, $row);
					last;
				}
			}
		
			print {$io} $self->_statements_same_subject_as_string( @samesubj );
		}
	
		print {$io} qq[</rdf:RDF>\n];
		return;
	}
	
=item C<< serialize_iter_to_bytes( $iterator ) >>

Serializes the L<Attean::API::Triple> objects from C<< $iterator >>
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

	sub _statements_same_subject_as_string {
		my $self		= shift;
		my @statements	= @_;
		my $s			= $statements[0]->subject;
	
		my $id;
		if ($s->does('Attean::API::Blank')) {
			my $b	= 'b' . $s->value;
			$id	= qq[rdf:nodeID="$b"];
		} else {
			my $i	= $s->abs;
			for ($i) {
				s/&/&amp;/g;
				s/</&lt;/g;
				s/"/&quot;/g;
			}
			$id	= qq[rdf:about="$i"];
		}
	
		my $counter	= 1;
		my %namespaces	= %{ $self->_rev };
		my $string	= '';
		foreach my $st (@statements) {
			my (undef, $p, $o)	= $st->values;
			my %used_namespaces;
			my ($ns, $ln);
			eval {
				($ns,$ln)	= $self->_qname($p);
			};
			if ($@) {
				my $uri	= $p->abs;
				die "Can't turn predicate $uri into a QName.";
			}
			$used_namespaces{ $ns }++;
			unless (exists $namespaces{ $ns }) {
				$namespaces{ $ns }	= 'ns' . $counter++;
			}
			
			my $prefix	= $namespaces{ $ns };
			my $nsdecl	= '';
			if ($self->scoped_namespaces) {
				$nsdecl	= qq[ xmlns:$prefix="$ns"];
			}
			if ($o->does('Attean::API::Literal')) {
				my $lv		= $o->value;
				for ($lv) {
					s/&/&amp;/g;
					s/</&lt;/g;
					s/"/&quot;/g;
				}
				my $lang	= $o->language;
				my $dt		= $o->datatype->value;
				my $tag	= join(':', $prefix, $ln);
			
				if ($lang) {
					$string	.= qq[\t<${tag}${nsdecl} xml:lang="${lang}">${lv}</${tag}>\n];
				} elsif ($dt) {
					if ($dt eq 'http://www.w3.org/2001/XMLSchema#string') {
						$string	.= qq[\t<${tag}${nsdecl}>${lv}</${tag}>\n];
					} else {
						$string	.= qq[\t<${tag}${nsdecl} rdf:datatype="${dt}">${lv}</${tag}>\n];
					}
				} else {
					$string	.= qq[\t<${tag}${nsdecl}>${lv}</${tag}>\n];
				}
			} elsif ($o->does('Attean::API::Blank')) {
				my $b	= 'b' . $o->value;
				for ($b) {
					s/&/&amp;/g;
					s/</&lt;/g;
					s/"/&quot;/g;
				}
				$string	.= qq[\t<${prefix}:$ln${nsdecl} rdf:nodeID="$b"/>\n];
			} else {
				my $u	= $o->abs;
				for ($u) {
					s/&/&amp;/g;
					s/</&lt;/g;
					s/"/&quot;/g;
				}
				$string	.= qq[\t<${prefix}:$ln${nsdecl} rdf:resource="$u"/>\n];
			}
		}
	
		$string	.= qq[</rdf:Description>\n];
	
		# rdf namespace is already defined in the <rdf:RDF> tag, so ignore it here
		my %seen	= %{ $self->_rev };
		my @ns;
		foreach my $uri (sort { $namespaces{$a} cmp $namespaces{$b} } grep { not($seen{$_}) } (keys %namespaces)) {
			my $ns	= $namespaces{$uri};
			my $str	= ($ns eq '') ? qq[xmlns="$uri"] : qq[xmlns:${ns}="$uri"];
			push(@ns, $str);
		}
		my $ns	= join(' ', @ns);
		if ($ns) {
			return qq[<rdf:Description ${ns} $id>\n] . $string;
		} else {
			return qq[<rdf:Description $id>\n] . $string;
		}
	}

	sub _qname {
		my $self	= shift;
		my $p		= shift;
		my $uri		= $p->abs;

		state $r_PN_CHARS_BASE		= qr/([A-Z]|[a-z]|[\x{00C0}-\x{00D6}]|[\x{00D8}-\x{00F6}]|[\x{00F8}-\x{02FF}]|[\x{0370}-\x{037D}]|[\x{037F}-\x{1FFF}]|[\x{200C}-\x{200D}]|[\x{2070}-\x{218F}]|[\x{2C00}-\x{2FEF}]|[\x{3001}-\x{D7FF}]|[\x{F900}-\x{FDCF}]|[\x{FDF0}-\x{FFFD}]|[\x{10000}-\x{EFFFF}])/o;
		state $r_PN_CHARS_U			= qr/(_|${r_PN_CHARS_BASE})/o;
		state $r_PN_CHARS			= qr/${r_PN_CHARS_U}|-|[0-9]|\x{00B7}|[\x{0300}-\x{036F}]|[\x{203F}-\x{2040}]/o;
		state $r_PN_LOCAL			= qr/((${r_PN_CHARS_U})((${r_PN_CHARS}|[.])*${r_PN_CHARS})?)/o;
		if ($uri =~ m/${r_PN_LOCAL}$/o) {
			my $ln	= $1;
			my $ns	= substr($uri, 0, length($uri)-length($ln));
			return ($ns, $ln);
		} else {
			die "Can't turn IRI $uri into a QName.";
		}
	}

	sub _top_xmlns {
		my $self		= shift;
		my $namespaces	= $self->namespaces;
		return '' if ($self->scoped_namespaces);
	
		my @ns;
		my @prefixes	= $namespaces ? $namespaces->list_prefixes : ();
		foreach my $k (sort { $a cmp $b } @prefixes) {
			my $v	= $namespaces->namespace_uri($k)->as_string;
			$self->_rev->{$v}	= $k;
			next if ($v eq 'http://www.w3.org/1999/02/22-rdf-syntax-ns#');
			my $str	= ($k eq '') ? qq[xmlns="$v"] : qq[xmlns:$k="$v"];
			push(@ns, $str);
		}
		my $ns		= join(' ', @ns);
		if (length($ns)) {
			$ns	= " $ns";
		}
		return $ns;
	}

	with 'Attean::API::TripleSerializer';
	with 'Attean::API::AbbreviatingSerializer';
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/perlrdf/issues>.

=head1 SEE ALSO

L<http://www.w3.org/TR/rdf-syntax-grammar/>

=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2018 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
