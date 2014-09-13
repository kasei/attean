use v5.14;
use warnings;

=head1 NAME

Attean::API::Term - RDF Terms

=head1 VERSION

This document describes Attean::API::Term version 0.001

=head1 DESCRIPTION

The Attean::API::Term role defines a common API for all RDF terms.

=head1 REQUIRED METHODS

The following methods are required by the L<Attean::API::Term> role:

=over 4

=item C<< value >>

=item C<< ntriples_string >>

=back

=head1 METHODS

The L<Attean::API::Term> role role provides default implementations of the
following methods:

=over 4

=item C<< as_string >>

=cut

package Attean::API::Term 0.001 {
	use Moo::Role;
	
	with 'Attean::API::TermOrVariable';
	
	requires 'value'; # => (is => 'ro', isa => 'Str', required => 1);
	requires 'ntriples_string';
	sub as_string {
		shift->ntriples_string();
	}
	sub ebv { return 0; }
	
	requires 'compare';
	
	sub __ntriples_string {
		my $self	= shift;
		my $value	= $self->value;
		if ($value =~ m/^[\x20\x23-\x5a\x5d-\x7e]*$/o) {
			return $value;
		}
		
		my @chars	= split(//, $value);
		my $string	= '';
		while (scalar(@chars)) {
			my $c	= shift(@chars);
			my $o	= ord($c);
			if ($o < 0x8) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o == 0x9) {
				$string	.= "\\t";
			} elsif ($o == 0xA) {
				$string	.= "\\n";
			} elsif ($o < 0xC) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o == 0xD) {
				$string	.= "\\r";
			} elsif ($o < 0x1F) {
				$string	.= sprintf("\\u%04X", $o);
			} elsif ($o < 0x21) {
				$string	.= $c;
			} elsif ($o == 0x22) {
				$string	.= "\"";
			} elsif ($o < 0x5B) {
				$string	.= $c;
			} elsif ($o == 0x5C) {
				$string	.= "\\";
			} elsif ($o < 0x7E) {
				$string	.= $c;
			} elsif ($o < 0xFFFF) {
				$string	.= sprintf("\\u%04X", $o);
			} else {
				$string	.= sprintf("\\U%08X", $o);
			}
		}
		return $string;
	}
}

package Attean::API::Literal 0.001 {
	use Moo::Role;
	use IRI;
	use Scalar::Util qw(blessed);
	
	with 'Attean::API::Term';
	
	requires 'language'; # => (is => 'ro', isa => 'Maybe[Str]', predicate => 'has_language');
	requires 'datatype'; # => (is => 'ro', isa => 'Attean::API::IRI', required => 1, coerce => 1, default => sub { IRI->new(value => 'http://www.w3.org/2001/XMLSchema#string') });
	
	sub BUILD {}
	around 'BUILD' => sub {
		my $orig	= shift;
		my $self	= shift;
		$self->$orig(@_);
		if (my $dt = $self->datatype) {
			my $type	= $dt->value;
			if ($type =~ qr<^http://www[.]w3[.]org/2001/XMLSchema#(?:integer|decimal|float|double|non(?:Positive|Negative)Integer|(?:positive|negative)Integer|long|int|short|byte|unsigned(?:Long|Int|Short|Byte))$>) {
				Role::Tiny->apply_roles_to_object($self, 'Attean::API::NumericLiteral');
			}
		}
	};
	
	sub ebv {
		my $self	= shift;
		my $value	= $self->value;
		my $dt		= $self->datatype->value;
		if ($dt eq 'http://www.w3.org/2001/XMLSchema#boolean') {
			return ($value eq 'true' or $value eq '1');
		} else {
			return (length($value) > 0);
		}
	}
	
	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return 1 unless ($b->does('Attean::API::Literal'));
		return ((($a->language // '') cmp ($b->language // '')) || ($a->datatype->value cmp $b->datatype->value) || ($a->value cmp $b->value));
	}
	
	if ($ENV{ATTEAN_TYPECHECK}) {
		use Types::Standard qw(Maybe Str ConsumerOf);
		my %map	= (
			language	=> Maybe[Str],
			datatype	=> ConsumerOf['Attean::API::IRI'],
		);
		foreach my $method (keys %map) {
			my $type	= $map{$method};
			around $method => sub {
				my $orig	= shift;
				my $self	= shift;
				my $class	= ref($self);
				my $value	= $self->$orig(@_);
				my $err		= $type->validate($value);
				if ($err) {
					my $name	= $type->display_name;
					die "${class}'s $method failed conformance check for $name: $value";
				}
				return $value;
			};
		}
	}
	
	sub _ntriples_string {
		my $self	= shift;
		my $str		= sprintf('"%s"', $self->__ntriples_string);
		if (my $l = $self->language) {
			return join('@', $str, $l);
		} else {
			my $dt	= $self->datatype;
			if ($dt->value eq 'http://www.w3.org/2001/XMLSchema#string') {
				return $str;
			} else {
				return join('^^', $str, $dt->ntriples_string);
			}
		}
	}
}

package Attean::API::NumericLiteral 0.001 {
	use Moo::Role;
	use Scalar::Util qw(blessed);
	
	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return 1 unless ($b->does('Attean::API::Literal'));
		if ($b->does('Attean::API::NumericLiteral')) {
			return $a->numeric_value <=> $b->numeric_value;
		} else {
			Attean::API::Literal::compare($a, $b);
		}
	}
	
	sub is_integer_type {
		my $self	= shift;
		my $type	= $self->datatype->value;
		return scalar($type =~ qr<^http://www[.]w3[.]org/2001/XMLSchema#(?:integer|non(?:Positive|Negative)Integer|(?:positive|negative)Integer|long|int|short|byte|unsigned(?:Long|Int|Short|Byte))$>);
	}
	
	sub ebv {
		my $self	= shift;
		return ($self->numeric_value != 0);
	}

	sub numeric_value {
		my $self	= shift;
		# TODO: parse numeric values that have lexical forms that perl doesn't recognize
		return 0+$self->value;

	}

	{
		my %type_hierarchy	= (
			'integer'				=> 'decimal',
			'nonPositiveInteger'	=> 'integer',
			'negativeInteger'		=> 'nonPositiveInteger',
			'long'					=> 'integer',
			'int'					=> 'long',
			'short'					=> 'int',
			'byte'					=> 'short',
			'nonNegativeInteger'	=> 'integer',
			'unsignedLong'			=> 'nonNegativeInteger',
			'unsignedInt'			=> 'unsignedLong',
			'unsignedShort'			=> 'unsignedInt',
			'unsignedByte'			=> 'unsignedShort',
			'positiveInteger'		=> 'nonNegativeInteger',
		);
		sub _lca {
			my ($lhs, $rhs)	= @_;
			for ($lhs, $rhs) {
				s/^.*#//;
			}
# 			warn "LCA ($lhs $rhs)";
			return "http://www.w3.org/2001/XMLSchema#$lhs" if ($lhs eq $rhs);
			my $cur	= $lhs;
			my %ancestors	= ($cur => 1);
			while ($cur = $type_hierarchy{$cur}) {
				$ancestors{$cur}++;
				return "http://www.w3.org/2001/XMLSchema#$cur" if ($cur eq $rhs);
			}
# 			use Data::Dumper;
# 			warn Dumper(\%ancestors);
			$cur	= $rhs;
			while ($cur = $type_hierarchy{$cur}) {
				return "http://www.w3.org/2001/XMLSchema#$cur" if exists $ancestors{$cur};
			}
			return;
		}
		sub binary_promotion_type {
			my $self	= shift;
			my $rhs		= shift;
			my $op		= shift;
			
			if ($op =~ m<^[-+*]$>) {
				# return common numeric type
				if (my $type = _lca($self->datatype->value, $rhs->datatype->value)) {
					return $type;
				}
				return 'http://www.w3.org/2001/XMLSchema#double';
			} elsif ($op eq '/') {
				if ($self->is_integer_type and $rhs->is_integer_type) {
					# return xsd:decimal if both operands are integers
					return 'http://www.w3.org/2001/XMLSchema#decimal';
				}
				if (my $type = _lca($self->datatype->value, $rhs->datatype->value)) {
					return $type;
				}
				return 'http://www.w3.org/2001/XMLSchema#double';
			} else {
				die "Unexpected numeric operation in binary_promotion_type: $op";
			}
		}
	}
	with 'Attean::API::Literal';
}

package Attean::API::Blank 0.001 {
	use Moo::Role;
	use Scalar::Util qw(blessed);
	
	sub ebv { return 1; }
	with 'Attean::API::Term';
	with 'Attean::API::BlankOrIRI';

	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return -1 unless ($b->does('Attean::API::Blank'));
		return ($a->value cmp $b->value);
	}
}

package Attean::API::IRI 0.001 {
	use Moo::Role;
	use IRI;
	use Scalar::Util qw(blessed);
	
	sub ebv { return 1; }
	with 'Attean::API::Term';
	with 'Attean::API::BlankOrIRI';
	
	sub compare {
		my ($a, $b)	= @_;
		return 1 unless blessed($b);
		return -1 if ($b->does('Attean::API::Literal'));
		return 1 unless ($b->does('Attean::API::IRI'));
		return ($a->value cmp $b->value);
	}
	
	sub _ntriples_string {
		my $self	= shift;
		return sprintf('<%s>', $self->__ntriples_string);
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
