use v5.14;
use warnings;

=head1 NAME

AtteanX::Parser::NTuples - Shared functionality for N-Triples and N-Quads parsers

=head1 VERSION

This document describes AtteanX::Parser::NTuples version 0.001

=head1 SYNOPSIS

 use Attean;

=head1 DESCRIPTION

...

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::NTuples 0.001 {
	use utf8;
	use Moo;
	use Attean;
	use Encode qw(decode);
	use namespace::clean;
	
	sub parse_term_from_string {
		my $self	= shift;
		my $string	= shift;
		my $n = $self->_eat_node( 0, $string );
		return $n;
	}
	
=item C<< parse_iter_from_bytes( $data ) >>

Returns an iterator of L<Attean::API::Binding> objects that result from parsing
the data read from the UTF-8 encoded byte string C<< $data >>.

=cut

	sub parse_iter_from_bytes {
		my $self	= shift;
		my $data	= shift;
	
		$data	= Encode::encode("utf-8", $data);
		open(my $fh, '<:encoding(UTF-8)', \$data);
		return $self->parse_iter_from_io($fh);
	}

=item C<< parse_iter_from_io( $fh ) >>

Returns an iterator of L<Attean::API::Binding> objects that result from parsing
the data read from the L<IO::Handle> object C<< $fh >>.

=cut

	sub parse_iter_from_io {
		my $self	= shift;
		my $fh		= shift;
		
		my $lineno	= 0;
		my $line;
		my $gen		= sub {
			while (defined($line = <$fh>)) {
				($line, my @extra)	= split(/\r\n|\r|\n/, $line, 2);
				$lineno++;
		
				next unless (defined($line) and length($line));
				next unless ($line =~ /\S/);
				chomp($line);
				$line	=~ s/^\s*//;
				$line	=~ s/\s*$//;
				next if ($line =~ /^#/);
		
				my @nodes	= ();
				while (my $n = $self->_eat_node( $lineno, $line )) {
					push(@nodes, $n);
					$line	=~ s/^\s*//;
				}
				$line	=~ s/^\s//g;
				unless ($line eq '.') {
					die "Missing expected '.' at line $lineno";
				}
		
				my $binding	= $self->_binding( \@nodes, $lineno );
				if (@extra and $extra[0] ne '') {
					$line	= shift(@extra);
					goto LINE;
				}
				return $binding;
			}
			return;
		};
		return Attean::CodeIterator->new(
			generator => $gen,
			item_type => $self->handled_type->role,
		);
	}

	sub _eat_node {
		my $self	= shift;
		my $lineno	= shift;
		$_[0]	=~ s/^\s*//;
		return unless length($_[0]);
		my $char	= substr($_[0], 0, 1);
		return if ($char eq '.');
	
		if ($char eq '<') {
			my ($uri)	= $_[0] =~ m/^<([^>]*)>/;
			substr($_[0], 0, length($uri)+2)	= '';
			state %cache;
			if (my $i = $cache{$uri}) {
				return $i;
			} else {
				if (rand() < 0.02) {
					# clear out the cache roughly every 50 IRIs
					%cache	= ();
				}
				my $iri	= Attean::IRI->new( _unescape($uri, $lineno) );
				$cache{$uri}	= $iri;
				return $iri;
			}
		} elsif ($char eq '_') {
			my ($name)	= $_[0] =~ m/^_:([A-Za-z][A-Za-z0-9]*)/;
			substr($_[0], 0, length($name)+2)	= '';
			return Attean::Blank->new( $name );
		} elsif ($char eq '"') {
			substr($_[0], 0, 1)	= '';
			my $value	= decode('utf8', '');
			while (length($_[0]) and substr($_[0], 0, 1) ne '"') {
				while ($_[0] =~ m/^([^"\\]+)/) {
					$value	.= $1;
					substr($_[0],0,length($1))	= '';
				}
				if (substr($_[0],0,1) eq '\\') {
					while ($_[0] =~ m/^\\(.)/) {
						if ($1 eq 't') {
							$value	.= "\t";
							substr($_[0],0,2)	= '';
						} elsif ($1 eq 'r') {
							$value	.= "\r";
							substr($_[0],0,2)	= '';
						} elsif ($1 eq 'n') {
							$value	.= "\n";
							substr($_[0],0,2)	= '';
						} elsif ($1 eq '"') {
							$value	.= '"';
							substr($_[0],0,2)	= '';
						} elsif ($1 eq '\\') {
							$value	.= "\\";
							substr($_[0],0,2)	= '';
						} elsif ($1 eq 'u') {
							$_[0] =~ m/^\\u([0-9A-Fa-f]{4})/ or die qq[Bad N-Triples \\u escape at line $lineno, near "$_[0]"];
							$value	.= chr(oct('0x' . $1));
							substr($_[0],0,6)	= '';
						} elsif ($1 eq 'U') {
							$_[0] =~ m/^\\U([0-9A-Fa-f]{8})/ or die qq[Bad N-Triples \\U escape at line $lineno, near "$_[0]"];
							$value	.= chr(oct('0x' . $1));
							substr($_[0],0,10)	= '';
						} else {
							die qq[Not valid N-Triples escape character '\\$1' at line $lineno, near "$_[0]"];
						}
					}
				}
			}
			if (substr($_[0],0,1) eq '"') {
				substr($_[0],0,1)	= '';
			} else {
				die qq[Ending double quote not found at line $lineno];
			}
		
			if ($_[0] =~ m/^@([a-z]+(-[a-zA-Z0-9]+)*)/) {
				my $lang	= $1;
				substr($_[0],0,1+length($lang))	= '';
				return Attean::Literal->new( value => $value, language => $lang );
			} elsif (substr($_[0],0,3) eq '^^<') {
				substr($_[0],0,3)	= '';
				my ($uri)	= $_[0] =~ m/^([^>]*)>/;
				substr($_[0], 0, length($uri)+1)	= '';
				return Attean::Literal->new( value => $value, datatype => $uri);
			} else {
				return Attean::Literal->new($value);
			}
		} else {
			die qq[Not valid N-Triples node start character '$char' at line $lineno, near "$_[0]"];
		}
	}

	sub _unescape {
		my $string	= shift;
		my $lineno	= shift;
		my $value	= '';
		while (length($string)) {
			while ($string =~ m/^([^\\]+)/) {
				$value	.= $1;
				substr($string,0,length($1))	= '';
			}
			if (length($string)) {
				if ($string eq '\\') {
					die qq[Backslash in N-Triples node without escaped character at line $lineno];
				}
				if ($string =~ m/^\\([tbnrf"'uU])/) {
					while ($string =~ m/^\\([tbnrf"'uU])/) {
						if ($1 eq 't') {
							$value	.= "\t";
							substr($string,0,2)	= '';
						} elsif ($1 eq 'b') {
							$value	.= "\b";
							substr($string,0,2)	= '';
						} elsif ($1 eq 'n') {
							$value	.= "\n";
							substr($string,0,2)	= '';
						} elsif ($1 eq 'r') {
							$value	.= "\r";
							substr($string,0,2)	= '';
						} elsif ($1 eq 'f') {
							$value	.= "\f";
							substr($string,0,2)	= '';
						} elsif ($1 eq '"') {
							$value	.= '"';
							substr($string,0,2)	= '';
						} elsif ($1 eq '\\') {
							$value	.= "\\";
							substr($string,0,2)	= '';
						} elsif ($1 eq 'u') {
							$string =~ m/^\\u([0-9A-F]{4})/ or die qq[Bad N-Triples \\u escape at line $lineno, near "$string"];
							$value	.= chr(oct('0x' . $1));
							substr($string,0,6)	= '';
						} elsif ($1 eq 'U') {
							$string =~ m/^\\U([0-9A-F]{8})/ or die qq[Bad N-Triples \\U escape at line $lineno, near "$string"];
							$value	.= chr(oct('0x' . $1));
							substr($string,0,10)	= '';
						}
					}
				} else {
					my $esc	= substr($string, 0, 2);
					die qq[Not a valid N-Triples escape sequence '$esc' at line $lineno, near "$string"];
				}
			}
		}
		return $value;
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
