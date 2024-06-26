package Types::Attean;
use strict;
use warnings;

use Type::Library -base, -declare => qw(
        AtteanIRI
        AtteanBlank
        AtteanLiteral
        AtteanSubject AtteanPredicate AtteanObject
        AtteanGraph
        AtteanTriple
        AtteanQuad
);
use Types::Standard qw( Str InstanceOf ConsumerOf ScalarRef );
use Types::URI qw( Uri Iri );
use Types::Namespace qw( Namespace );
use Types::Path::Tiny  qw( Path );
use Types::UUID        qw( Uuid );

my $TrineNode = InstanceOf['RDF::Trine::Node::Resource'];
my $TrineNS   = InstanceOf['RDF::Trine::Namespace'];
my $XmlNS     = InstanceOf['XML::Namespace'];


our $VERSION = '0.034';

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

=item C<< AtteanIRI >>

A class type for L<Attean::IRI>.

Can coerce from L<URI>, L<IRI>, L<URI::Namespace>,
L<RDF::Trine::Node::Resource>, L<RDF::Trine::Namespace>,
L<XML::Namespace> and strings.

Additionally, a C<ScalarRef> can be coerced into a C<data> URI.

=item C<< AtteanBlank >>

A role type for L<Attean::API::Blank>.

=item C<< AtteanLiteral >>

A role type for L<Attean::API::Literal>.

=item C<< AtteanSubject >>

A role type for a term that can be used as a subject
in a triple or quad
(i.e., L<Attean::API::BlankOrIRI>).

=item C<< AtteanPredicate >>

A role type for a term that can be used as a predicate
in a triple or quad
(i.e., L<Attean::API::IRI>).

=item C<< AtteanObject >>

A role type for a term that can be used as an object
in a triple or quad
(i.e., L<Attean::API::Term>).

=item C<< AtteanGraph >>

A role type for a term that can be used as a graph
in a quad
(i.e., L<Attean::API::BlankOrIRI>).

=item C<< AtteanTriple >>

A role type for L<Attean::API::Triple>.

=item C<< AtteanQuad >>

A role type for L<Attean::API::Quad>.

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
         Uuid        ,=> q{ do { require Attean::IRI; "Attean::IRI"->new("urn:uuid:$_") } },
         Path        ,=> q{ do { require Attean::IRI; my $u = "URI::file"->new($_); "Attean::IRI"->new($u->as_string) } },
         ScalarRef   ,=> q{ do { require Attean::IRI; my $u = "URI"->new("data:"); $u->data($$_); "Attean::IRI"->new($u->as_string) } },
         $TrineNode  ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->uri_value) } },
         $TrineNS    ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->uri->uri_value) } },
         $XmlNS      ,=> q{ do { require Attean::IRI; "Attean::IRI"->new($_->uri) } },
);


require Attean::IRI;

__PACKAGE__->add_type(
	name       => AtteanBlank,
	parent     => ConsumerOf['Attean::API::Blank']
);

__PACKAGE__->add_type(
	name       => AtteanLiteral,
	parent     => ConsumerOf['Attean::API::Literal']
);

__PACKAGE__->add_type(
	name       => AtteanSubject,
	parent     => ConsumerOf['Attean::API::BlankOrIRI']
);

__PACKAGE__->add_type(
	name       => AtteanPredicate,
	parent     => ConsumerOf['Attean::API::IRI']
);

__PACKAGE__->add_type(
	name       => AtteanObject,
	parent     => ConsumerOf['Attean::API::Term']
);

__PACKAGE__->add_type(
	name       => AtteanGraph,
	parent     => ConsumerOf['Attean::API::BlankOrIRI']
);

__PACKAGE__->add_type(
	name       => AtteanTriple,
	parent     => ConsumerOf['Attean::API::Triple']
);

__PACKAGE__->add_type(
	name       => AtteanQuad,
	parent     => ConsumerOf['Attean::API::Quad']
);

1;
