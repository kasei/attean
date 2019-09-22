use v5.14;
use warnings;

=head1 NAME

AtteanX::API::Lexer - Role defining common functionality for lexers.

=head1 VERSION

This document describes AtteanX::API::Lexer version 0.024

=head1 DESCRIPTION

The AtteanX::API::Lexer role provides a common interface and implementation
for lexer implementations, allowing line-based buffer filling, and consuming
of characters, constant strings, and fixed-length buffers.

=head1 ATTRIBUTES

=over 4

=item C<< file >>

=item C<< linebuffer >>

=item C<< line >>

=item C<< column >>

=item C<< buffer >>

=item C<< start_column >>

=item C<< start_line >>

=back

=head1 METHODS

=over 4

=cut

package AtteanX::API::Lexer 0.024 {
	use strict;
	use Types::Standard qw(FileHandle Ref Str Int ArrayRef HashRef ConsumerOf InstanceOf);

	use Moo::Role;

	has file			=> ( is => 'ro', isa => FileHandle, required => 1, );
	has linebuffer		=> ( is => 'rw', isa => Str, default => '', );
	has line			=> ( is => 'rw', isa => Int, default => 1, );
	has column			=> ( is => 'rw', isa => Int, default => 1, );
	has buffer			=> ( is => 'rw', isa => Str, default => '', );
	has start_column	=> ( is => 'rw', isa => Int, default => -1, );
	has start_line		=> ( is => 'rw', isa => Int, default => -1, );

	around 'BUILDARGS' => sub {
		my $orig	= shift;
		my $class	= shift;
		return { file => shift } if (scalar(@_) == 1);
		return $orig->( $class, @_ );
	};

=item C<< fill_buffer >>

Fills the buffer with a new line from the underlying filehandle.

=cut

	sub fill_buffer {
		my $self	= shift;
		unless (length($self->buffer)) {
			my $line	= $self->file->getline;
			$self->{buffer}	.= $line if (defined($line));
		}
	}

=item C<< check_for_bom >>

Remove a BOM character if one appears at the start of the buffer.

=cut

	sub check_for_bom {
		my $self	= shift;
		my $c		= $self->peek_char();
		$self->get_char if (defined($c) and $c eq "\x{FEFF}");
	}

=item C<< get_char_safe( $char ) >>

Consume the single character C<< $char >> from the buffer.
Throw an error if C<< $char >> is not at the start of the buffer.

=cut

	sub get_char_safe {
		my $self	= shift;
		my $char	= shift;
		my $c		= $self->get_char;
		$self->_throw_error("Expected '$char' but got '$c'") if ($c ne $char);
		return $c;
	}

=item C<< get_char( $char ) >>

Consume and return a single character from the buffer.

=cut

	sub get_char {
		my $self	= shift;
		my $c		= substr($self->{buffer}, 0, 1, '');
		if ($c eq "\n") {
	# 		$self->{linebuffer}	= '';
			$self->{line}	= 1+$self->{line};
			$self->{column}	= 1;
		} else {
	# 		$self->{linebuffer}	.= $c;
			$self->{column}	= 1+$self->{column};
		}
		return $c;
	}

=item C<< peek_char( $char ) >>

Return a single character from the start of the buffer.

=cut

	sub peek_char {
		my $self	= shift;
		if (length($self->{buffer}) == 0) {
			$self->fill_buffer;
			return if (length($self->{buffer}) == 0);
		}
		return substr($self->{buffer}, 0, 1);
	}

=item C<< read_word( $word ) >>

Consume the string C<< $word >> from the start of the buffer.
Throw an error if C<< $word >> is not at the start of the buffer.

=cut

	sub read_word {
		my $self	= shift;
		my $word	= shift;
		$self->fill_buffer while (length($self->{buffer}) < length($word));
		$self->_throw_error("Expected '$word'") if (substr($self->{buffer}, 0, length($word)) ne $word);
	
		my $lines	= ($word =~ tr/\n//);
		my $lastnl	= rindex($word, "\n");
		my $cols	= length($word) - $lastnl - 1;
		$self->{lines}	+= $lines;
		if ($lines) {
			$self->{column}	= $cols;
		} else {
			$self->{column}	+= $cols;
		}
		substr($self->{buffer}, 0, length($word), '');
	}

=item C<< read_length( $length ) >>

Consume and return C<< $length >> characters  from the start of the buffer.

=cut

	sub read_length {
		my $self	= shift;
		my $len		= shift;
		while (length($self->{buffer}) < $len) {
			my $curlen	= length($self->{buffer});
			$self->fill_buffer;
			last if (length($self->{buffer}) == $curlen);
		}
	
		my $word	= substr($self->{buffer}, 0, $len, '');
		my $lines	= ($word =~ tr/\n//);
		my $lastnl	= rindex($word, "\n");
		my $cols	= length($word) - $lastnl - 1;
		$self->{lines}	+= $lines;
		if ($lines) {
			$self->{column}	= $cols;
		} else {
			$self->{column}	+= $cols;
		}
		return $word;
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2019 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
