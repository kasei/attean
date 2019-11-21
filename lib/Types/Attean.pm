package Types::Attean;
use strict;
use warnings;

use Type::Library -base, -declare => qw( AtteanIRI );
use Types::Standard qw( Str InstanceOf ScalarRef );
use Types::URI qw( Uri Iri );
use Types::Namespace qw( Namespace );

my $TrineNode = InstanceOf['RDF::Trine::Node::Resource'];
my $TrineNS   = InstanceOf['RDF::Trine::Namespace'];
my $XmlNS     = InstanceOf['XML::Namespace'];


our $VERSION = '0.025';

=head1 NAME

Types::Attean - Type constraints for dealing with Attean classes

=head1 SYNOPSIS

TODO
  package IRI::Counter {
    use Moo;  # or Moose
    use Types::Attean qw( AtteanIRI );

    has iri => (
      is => "ro",
      isa => AtteanIRI,
      required => 1,
    );

    sub count_uses_in_document { ... }
  }

=head1 DESCRIPTION

Types::Attean is a type constraint library suitable for use with
L<Moo>/L<Moose> attributes, L<Kavorka> sub signatures, and so
forth. It builds on L<Types::URI>.

=head1 TYPES

=over

=item C<< AtteanIri >>

A class type for L<Attean::IRI>.

Can coerce from L<URI>, L<IRI>, L<URI::Namespace> and strings.

=back

=head1 OTHER COERCIONS

This library can also coerce from C<Attean::IRI> to the C<Namespace> type defined in L<URI::Namespace>.

=cut

__PACKAGE__->add_type(
	name       => AtteanIRI,
	parent     => InstanceOf['Attean::IRI']
);




AtteanIRI->coercion->add_type_coercions(
         Str         ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_) } },
#         HashRef     ,=> q{ do { require Attean::IRI; "Attean::IRI"->new(URI::FromHash::uri(%$_)) } }, # TODO: Perhaps use for a shortcut to populate rather than parse?
         Namespace   ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->as_string) } },
         Uri         ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->as_string) } },
         Iri         ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->as_string) } },
         ScalarRef   ,=> q{ do { require Attean::IRI; my $u = "URI"->new("data:"); $u->data($$_); "Attean::IRI"->new($u->as_string) } },
         $TrineNode  ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->uri_value) } },
         $TrineNS    ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->uri->uri_value) } },
         $XmlNS      ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->uri) } },
);


require Attean::IRI;


1;
