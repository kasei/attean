=head1 NAME

AtteanX::Serializer::TurtleTokens - Turtle Serializer

=head1 VERSION

This document describes AtteanX::Serializer::TurtleTokens version 0.028

=head1 SYNOPSIS

 use Attean;
 my $serializer = Attean->get_serializer('Turtle')->new();
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

package AtteanX::Serializer::TurtleTokens 0.028 {
	use Moo;
	use Data::Dumper;
	use Encode qw(encode);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);
	use AtteanX::Parser::Turtle::Constants;
	use AtteanX::Parser::Turtle::Lexer;
	use namespace::clean;
	with 'Attean::API::AbbreviatingSerializer';
	with 'Attean::API::AppendableSerializer';
	
	sub canonical_media_type { return "text/turtle" }

	sub media_types {
		return [qw(text/turtle)];
	}
	
	sub handled_type {
		state $ITEM_TYPE = Type::Tiny::Role->new(role => 'AtteanX::Parser::Turtle::Token');
		return $ITEM_TYPE;
	}

	sub file_extensions { return [qw(ttl)] }

=item C<< serialize_iter_to_io( $fh, $iterator ) >>

Serializes the Turtle token objects from C<< $iterator >> to the
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

		my $map = $self->namespaces;
		my %namespace_map;
		if ($map) {
			foreach my $p ($map->list_prefixes) {
				my $prefix	= $map->namespace_uri($p)->as_string;
				$namespace_map{$prefix}	= $p;
			}
		}
		
		while (my $t = $iter->next()) {
			my $type	= $t->type;
			
			if ($map) {
				if ($type == IRI) {
					my $value	= $t->value;
					if ($value =~ /^(?<namespace>.*?)(?<local>$AtteanX::Parser::Turtle::Lexer::r_PN_LOCAL)$/) {
						if (my $ns = $namespace_map{$+{namespace}}) {
							$type	= PREFIXNAME;
							$t		= AtteanX::SPARQL::Token->fast_constructor( $type, $t->start_line, $t->start_column, $t->line, $t->column, ["${ns}:", $+{local}] );
						}
					}
				}
			}
			
			if ($type == LANG or $type == HATHAT) {
				$need_space= 0;
			}
			
			unless ($newline) {
				if ($type == BASE or $type == PREFIX or $type == TURTLEBASE or $type == TURTLEPREFIX) {
					$io->print("\n");
					$newline	= 1;
				}
			}
			
			if ($newline) {
				$io->print('    ' x $indent);
				$newline	= 0;
			} elsif ($need_space) {
				$io->print(' ');
				$need_space	= 0;
			}
			
			if ($type == PREFIX or $type == TURTLEPREFIX) {
				# If we're serializing a PREFIX, also serialize the PREFIXNAME
				# and IRI that must follow it so that we don't accidentally
				# shorten the prefix IRI with its own namespace. For example,
				# if we didn't serialize the PREFIXNAME and IRI here, we might
				# end up with this:
				# 
				#    @prefix foaf: foaf:
				# 
				# instead of:
				# 
				#    @prefix foaf: <http://xmlns.com/foaf/0.1/>
				$io->print($t->value);
				$io->print(' ');
				my $pname	= $iter->next();
				unless ($pname->type == PREFIXNAME) {
					die "PREFIX namespace not found during Turtle serialization";
				}
				my $args	= $pname->args;
				$io->print(join('', @$args));
				$io->print(' ');
				
				my $iri = $iter->next();
				unless ($iri->type == IRI) {
					die "PREFIX IRI not found during Turtle serialization";
				}
				$io->print('<');
				$io->print($iri->value);
				$io->print('>');
				$need_space++;
			} elsif ($type == PREFIXNAME) {
				my $args	= $t->args;
				$io->print(join('', @$args));
				$need_space++;
			} elsif ($type == BNODE) {
				$io->print('_:');
				$io->print($t->value);
				$need_space++;
			} elsif ($type == IRI) {
				# TODO: escape
				$io->print('<');
				$io->print($t->value);
				$io->print('>');
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
			} elsif ($type == A) {
				$io->print('a');
				$need_space++;
			} elsif ($type == WS) {
			} elsif ($type == COMMENT) {
				if ($t->value =~ /\n/) {
					die "Unexpected newline found in Turtle comment token";
				}
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
			} elsif ($type == SEMICOLON) {
				$io->print("\n");
				$need_space	= 0;
				$newline	= 1;
				unless ($semicolon) {
					$indent++;
				}
				$semicolon	= 1;
			}
		}
		unless ($newline) {
			$io->print("\n");
		}
		return;
	}
	
=item C<< serialize_iter_to_bytes( $iterator ) >>

Serializes the Turtle token objects from C<< $iterator >>
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
