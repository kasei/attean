use v5.14;
use warnings;

package RDF::X::Serializer::NTriples 0.001 {
	use Moose;
	use Encode qw(encode);
	use RDF::ListIterator;
	use List::MoreUtils qw(any);
	
	my $ITEM_TYPE = Moose::Meta::TypeConstraint::Role->new(role => 'RDF::API::Triple');
	
	with 'RDF::API::TripleSerializer';
	has 'canonical_media_type' => (is => 'ro', isa => 'Str', init_arg => undef, default => 'application/n-triples');
	has 'media_types' => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef, default => sub { ['application/n-triples'] });
	has 'handled_type' => (
		is => 'ro',
		isa => 'Moose::Meta::TypeConstraint',
		init_arg => undef,
		default => sub { $ITEM_TYPE },
	);
	
	sub serialize_iter_to_io {
		my $self	= shift;
		my $io		= shift;
		my $iter	= shift;
		my $type	= $iter->item_type;
		while (my $t = $iter->next()) {
			my $str = $t->tuples_string;
			$io->print($str, " .\n");
		}
		return;
	}
	
	sub serialize_iter_to_bytes {
		my $self	= shift;
		my $iter	= shift;
		my $data	= '';
		while (my $t = $iter->next()) {
			my $str = $t->tuples_string;
			$data	.= $str;
			$data	.= " .\n";
		}
		return encode('UTF-8', $data);
	}
}

1;
