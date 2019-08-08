package Types::Attean;
use strict;
use warnings;

use Type::Library -base, -declare => qw( AtteanIRI );
use Types::Standard qw( Str InstanceOf );
use Types::URI qw( Uri Iri );
use Types::Namespace qw( Namespace );

our $VERSION = '0.024';

=head1 NAME

Types::Attean - type constraints for dealing with Attean classes

=head1 SYNOPSIS

TODO
  package Namespace::Counter {
    use Moo;  # or Moose
    use Types::Namespace qw( Namespace );

    has ns => (
      is => "ro",
      isa => Namespace,
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

Can coerce from L<URI>, L<IRI>, L<URI::Namespace>, L<Path::Tiny>, and strings.

=item C<< NamespaceMap >>

A class type for L<URI::NamespaceMap>.

Can coerce from a hashref of C<< prefix => URI >> pairs.

=item C<< Uri >>, C<< Iri >>

These namespaces are re-exported from L<Types::URI>, but with an
additional coercion from the C<< Namespace >> type.

=back

=head1 FURTHER DETAILS

See L<URI::NamespaceMap> for further details about authors, license, etc.

=cut

__PACKAGE__->add_type(
	name       => AtteanIRI,
	parent     => InstanceOf['Attean::IRI']
);




AtteanIRI->coercion->add_type_coercions(
#         Uuid        ,=> q{ do { require Attean::IRI; "Attean::IRI"->new("urn:uuid:$_") } },
         Str         ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_) } },
#         Path        ,=> q{ do { require Attean::IRI; my $u = "URI::file"->new($_); "Attean::IRI"->new($u->as_string) } },
#         ScalarRef   ,=> q{ do { require Attean::IRI; my $u = "URI"->new("data:"); $u->data($$_); "Attean::IRI"->new($u->as_string) } },
#         HashRef     ,=> q{ do { require Attean::IRI; "Attean::IRI"->new(URI::FromHash::uri(%$_)) } },
#         $TrineNode  ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->uri_value) } },
#         $TrineNS    ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->uri->uri_value) } },
#         $XmlNS      ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->uri) } },
         Namespace   ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->as_string) } },
         Uri         ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->as_string) } },
         Iri         ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->as_string) } },
);

Namespace->coercion->add_type_coercions(
  AtteanIRI ,=> q{ do { require URI::Namespace; "URI::Namespace"->new($_->as_string) } },
);

require Attean::IRI;


1;
