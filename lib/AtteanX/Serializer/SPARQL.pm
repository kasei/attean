=head1 NAME

AtteanX::Serializer::SPARQL - SPARQL Serializer

=head1 VERSION

This document describes AtteanX::Serializer::SPARQL version 0.028

=head1 SYNOPSIS

 use Attean;
 my $serializer = Attean->get_serializer('SPARQL')->new();
 $serializer->serialize_iter_to_io( $io, $fh );

=head1 DESCRIPTION

...

=head1 ATTRIBUTES

=over 4

=item C<< canonical_media_type >>

=item C<< media_types >>

=item C<< handled_type >>

=item C<< file_extensions >>

=back

=head1 METHODS

=over 4

=cut

use v5.14;
use warnings;

package AtteanX::Serializer::SPARQL 0.028 {
	use Moo;
	use Data::Dumper;
	use Encode qw(encode);
	use Attean::ListIterator;
	use Scalar::Util qw(blessed);
	use List::MoreUtils qw(any);
	use AtteanX::SPARQL::Constants;
	use namespace::clean;
	with 'Attean::API::AbbreviatingSerializer';
	
	sub canonical_media_type { return "application/sparql-query" }

	sub media_types {
		return [qw(application/sparql-query)];
	}
	
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'AtteanX::SPARQL::Token');
		return $ITEM_TYPE;
	}

	sub file_extensions { return [qw(rq)] }

=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the SPARQL token objects from C<< $iterator >> to the
L<IO::Handle> object C<< $fh >>.

=cut

	sub serialize_iter_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= shift;
		my $indent		= 0;
		my $newline		= 1;
		my $semicolon	= 0;
		my $need_space	= 0;
		my $ns 			= $self->namespaces;
		my $parser		= Attean->get_parser('SPARQLLex')->new();
		
		if ($ns) {
			NSLOOP: foreach my $p (sort $ns->list_prefixes) {
				my $prefix	= $ns->namespace_uri($p)->as_string;
				$io->print("PREFIX $p: <$prefix>\n");
			}
		}
		
		my $last;
		
		while (my $t = $iter->next()) {
			my $type	= $t->type;
			
			if ($type == LANG or $type == HATHAT) {
				$need_space= 0;
			}
			
			unless ($newline) {
				if ($type == RBRACE) {
					$io->print("\n");
					$newline	= 1;
				} elsif ($type == KEYWORD and $t->value =~ /^(BASE|PREFIX|SELECT|ASK|CONSTRUCT|DESCRIBE|USING|FROM)$/) {
					$io->print("\n");
					$newline	= 1;
				} elsif ($type == KEYWORD and $t->value eq 'WHERE' and blessed($last) and ($last->type == PREFIXNAME or $last->type == IRI)) {
					# this captures "USING <g> WHERE" and "USING NAMED <g> WHERE", forcing a newline before the "WHERE"
					$io->print("\n");
					$newline	= 1;
				}
			}
			
			if ($type == RBRACE) {
				$indent--;
			}
			
			if ($semicolon and $type == KEYWORD and $t->value =~ /^(BASE|PREFIX|SELECT|ADD|COPY|MOVE|USING|LOAD|DELETE|INSERT|WITH|CLEAR|DROP|CREATE)$/) {
				# SPARQL Update use of a semicolon is different from its use in a Query;
				# In queries, semicolon affects indentation. In updates, it's just a separator.
				# So back out the indentation if it's being used as a separator here.
				$semicolon	= 0;
				$indent--;
			}
			
			if ($newline) {
				$io->print('    ' x $indent);
				$newline	= 0;
			} elsif ($need_space) {
				$io->print(' ');
				$need_space	= 0;
			}
			
			if ($type == KEYWORD) {
				$io->print($t->value);
				$need_space++;
			} elsif ($type == IRI) {
				my $value	= $t->value;
				my $ser		= '<' . $value . '>';
				if ($ns) {
					NSLOOP: foreach my $p ($ns->list_prefixes) {
						my $prefix	= $ns->namespace_uri($p)->as_string;
						if (substr($value, 0, length($prefix)) eq $prefix) {
							# now verify that the prefixname is valid SPARQL syntax by re-parsing it
							my $pname	= join(':', $p, substr($value, length($prefix)));
							my $b		= $pname;
							$b			= encode('UTF-8', $b, Encode::FB_CROAK);
							my ($pnt)	= eval { $parser->parse_list_from_bytes($b) };
							if (blessed($pnt) and $pnt->type == PREFIXNAME) {
								$ser	= $pname;
							}
							last NSLOOP;
						}
					}
				}
				
				# TODO: escape
				$io->print($ser);
				$need_space++;
			} elsif ($type == PREFIXNAME) {
				my $args	= $t->args;
				$io->print(join('', @$args));
				$need_space++;
			} elsif ($type == BNODE) {
				$io->print('_:');
				$io->print($t->value);
				$need_space++;
			} elsif ($type == LANG) {
				$io->print('@');
				$io->print($t->value);
				$need_space++;
			} elsif ($type == STRING1S) {
				my $value	= $t->value;
				$value		=~ s/'/\\'/g;
				$io->print("'");
				$io->print($value);
				$io->print("'");
				$need_space++;
			} elsif ($type == STRING1D) {
				my $value	= $t->value;
				$value		=~ s/"/\\"/g;
				$io->print('"');
				$io->print($value);
				$io->print('"');
				$need_space++;
			} elsif ($type == STRING3S) {
				my $value	= $t->value;
				$value		=~ s/'''/''\\'/g;
				$io->print("'''");
				$io->print($value);
				$io->print("'''");
				$need_space++;
			} elsif ($type == STRING3D) {
				my $value	= $t->value;
				$value		=~ s/"""/""\\"/g;
				$io->print('"""');
				$io->print($value);
				$io->print('"""');
				$need_space++;
			} elsif ($type == VAR) {
				$io->print('?');
				$io->print($t->value);
				$need_space++;
			} elsif ($type == A) {
				$io->print('a');
				$need_space++;
			} elsif ($type == WS) {
			} elsif ($type == COMMENT) {
				$io->print('# ');
				$io->print($t->value);
				$io->print("\n");
			} elsif ($type == HATHAT) {
				$io->print($t->value);
			} else {
				$io->print($t->value);
				$need_space++;
			}
			
			if ($type == DOT) {
				if ($semicolon) {
					$indent--;
					$semicolon	= 0;
				}
				$need_space	= 0;
				$io->print("\n");
				$newline	= 1;
			} elsif ($type == LBRACE) {
				$io->print("\n");
				$need_space	= 0;
				$newline	= 1;
				$indent++;
			} elsif ($type == SEMICOLON) {
				$io->print("\n");
				$need_space	= 0;
				$newline	= 1;
				unless ($semicolon) {
					$indent++;
				}
				$semicolon	= 1;
			}
			$last	= $t;
		}
		unless ($newline) {
			$io->print("\n");
		}
		return;
	}
	
=item C<< serialize_iter_to_bytes( $iterator ) >>

Serializes the SPARQL token objects from C<< $iterator >>
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

Copyright (c) 2014--2020 Gregory Todd Williams. This
program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
