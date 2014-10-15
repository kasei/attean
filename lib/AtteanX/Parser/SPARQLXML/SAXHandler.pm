# AtteanX::Parser::SPARQLXML::SAXHandler
# -----------------------------------------------------------------------------

=head1 NAME

AtteanX::Parser::SPARQLXML::SAXHandler - XML parser for SPARQL XML Results format

=head1 VERSION

This document describes AtteanX::Parser::SPARQLXML::SAXHandler version 0.002

=head1 STATUS

This module's API and functionality should be considered unstable.
In the future, this module may change in backwards-incompatible ways,
or be removed entirely. If you need functionality that this module provides,
please L<get in touch|http://www.perlrdf.org/>.

=head1 SYNOPSIS

 use AtteanX::Parser::SPARQLXML::SAXHandler;

=head1 METHODS

=over 4

=cut

package AtteanX::Parser::SPARQLXML::SAXHandler 0.001;

use v5.14;
use warnings;

use Attean;
use Scalar::Util qw(refaddr);
use base qw(XML::SAX::Base);
use Attean;
use namespace::clean;

my %strings;
my %tagstack;
my %results;
my %values;
my %bindings;
my %booleans;
my %variables;
my %has_head;
my %has_end;
my %result_count;
my %result_handlers;
my %config;

my %expecting_string	= map { $_ => 1 } qw(boolean bnode uri literal);

=item C<< new ( [ \&handler ] ) >>

Returns a new XML::SAX handler object. If C<< &handler >> is supplied, it will
be called with a variable bindings object as each is parsed, bypassing the
normal process of collecting the results for retrieval via an iterator object.

=cut

sub new {
	my $class	= shift;
	my $self	= $class->SUPER::new();
	if (@_) {
		my $addr	= refaddr( $self );
		my $code	= shift;
		my $args	= shift || {};
		$result_handlers{ $addr }	= $code;
		$config{ $addr }			= { %$args };
	}
	return $self;
}

=begin private

=item C<< start_element >>

=cut

sub start_element {
	my $self	= shift;
	my $el		= shift;
	my $tag		= $el->{LocalName};
	my $addr	= refaddr( $self );
	
	unshift( @{ $tagstack{ $addr } }, [$tag, $el] );
	if ($expecting_string{ $tag }) {
		$strings{ $addr }	= '';
	}
}

=item C<< end_element >>

=cut

sub end_element {
	my $self	= shift;
	my $class	= ref($self);
	my $eel		= shift;
	my $addr	= refaddr( $self );
	my $string	= $strings{ $addr };
	my $taginfo	= shift( @{ $tagstack{ $addr } } );
	my ($tag, $el)	= @$taginfo;
	
	if ($tag eq 'head') {
		$has_head{ $addr }	= 1;
		if (my $code = $result_handlers{ $addr }) {
			if ($config{ $addr }{ variables }) {
				$code->( $variables{ $addr } );
			}
		}
	} elsif ($tag eq 'sparql') {
		$has_end{ $addr }	= 1;
	} elsif ($tag eq 'variable') {
		push( @{ $variables{ $addr } }, $el->{Attributes}{'{}name'}{Value});
	} elsif ($tag eq 'boolean') {
		$booleans{ $addr }	= ($string eq 'true') ? 1 : 0;
		if ($string =~ /^(?:true|false)$/ and my $code = $result_handlers{ $addr }) {
			$code->( Attean::Literal->$string() );
		}
	} elsif ($tag eq 'binding') {
		my $name	= $el->{Attributes}{'{}name'}{Value};
		my $value	= delete( $values{ $addr } );
		$bindings{ $addr }{ $name }	= $value;
	} elsif ($tag eq 'result') {
		my $result	= delete( $bindings{ $addr } ) || {};
		$result_count{ $addr }++;
		my $vb	= Attean::Result->new( bindings => $result );
		if (my $code = $result_handlers{ $addr }) {
			$code->( $vb );
		} else {
			push( @{ $results{ $addr } }, $vb );
		}
	} elsif ($tag eq 'bnode') {
		$values{ $addr }	= Attean::Blank->new( $string );
	} elsif ($tag eq 'uri') {
		$values{ $addr }	= Attean::IRI->new( $string );
	} elsif ($tag eq 'literal') {
		my ($lang, $dt);
		if (my $dtinf = $el->{Attributes}{'{}datatype'}) {
			$dt		= $dtinf->{Value};
			$values{ $addr }	= Attean::Literal->new( value => $string, datatype => $dt );
		} elsif (my $langinf = $el->{Attributes}{'{http://www.w3.org/XML/1998/namespace}lang'}) {
			$lang	= $langinf->{Value};
			$values{ $addr }	= Attean::Literal->new( value => $string, language => $lang );
		} else {
			$values{ $addr }	= Attean::Literal->new( value => $string );
		}
	}
}

=item C<< characters >>

=cut

sub characters {
	my $self	= shift;
	my $data	= shift;
	my $addr	= refaddr( $self );
	
	my $tag		= $self->_current_tag;
	if ($expecting_string{ $tag }) {
		my $chars	= $data->{Data};
		$strings{ $addr }	.= $chars;
	}
}

sub _current_tag {
	my $self	= shift;
	my $addr	= refaddr( $self );
	return $tagstack{ $addr }[0][0];
}

sub DESTROY {
	my $self	= shift;
	my $addr	= refaddr( $self );
	delete $strings{ $addr };
	delete $results{ $addr };
	delete $tagstack{ $addr };
	delete $values{ $addr };
	delete $bindings{ $addr };
	delete $booleans{ $addr };
	delete $variables{ $addr };
	delete $has_head{ $addr };
	delete $has_end{ $addr };
	delete $result_count{ $addr };
	delete $result_handlers{ $addr };
	delete $config{ $addr };
}


1;

__END__

=end private

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
