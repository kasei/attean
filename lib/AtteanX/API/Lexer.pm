package AtteanX::API::Lexer 0.010 {
	use v5.14;
	use strict;
	use warnings;
	use Moo::Role;
	use Types::Standard qw(FileHandle Ref Str Int ArrayRef HashRef ConsumerOf InstanceOf);
	use namespace::clean;

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

	sub fill_buffer {
		my $self	= shift;
		unless (length($self->buffer)) {
			my $line	= $self->file->getline;
			$self->{buffer}	.= $line if (defined($line));
		}
	}

	sub check_for_bom {
		my $self	= shift;
		my $c		= $self->peek_char();
		$self->get_char if (defined($c) and $c eq "\x{FEFF}");
	}

	sub get_char_safe {
		my $self	= shift;
		my $char	= shift;
		my $c		= $self->get_char;
		$self->_throw_error("Expected '$char' but got '$c'") if ($c ne $char);
		return $c;
	}

	sub get_char_fill_buffer {
		my $self	= shift;
		if (length($self->{buffer}) == 0) {
			$self->fill_buffer;
			return if (length($self->{buffer}) == 0);
		}
		my $c		= substr($self->{buffer}, 0, 1, '');
		if ($c eq "\n") {
			$self->{line}	= 1+$self->{line};
			$self->{column}	= 1;
		} else {
			$self->{column}	= 1+$self->{column};
		}
		return $c;
	}

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

	sub peek_char {
		my $self	= shift;
		if (length($self->{buffer}) == 0) {
			$self->fill_buffer;
			return if (length($self->{buffer}) == 0);
		}
		return substr($self->{buffer}, 0, 1);
	}

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

	sub read_length {
		my $self	= shift;
		my $len		= shift;
		while (length($self->{buffer}) < $len) {
			$self->fill_buffer;
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
