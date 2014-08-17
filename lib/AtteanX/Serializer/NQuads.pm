use v5.14;
use warnings;

package AtteanX::Serializer::NQuads 0.001 {
	use Encode qw(encode);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);

	use Moose;
	extends 'AtteanX::Serializer::NTuples';
	has 'canonical_media_type' => (is => 'ro', isa => 'Str', init_arg => undef, default => 'application/n-quads');
	has 'media_types' => (is => 'ro', isa => 'ArrayRef[Str]', init_arg => undef, default => sub { ['application/n-quads'] });
	with 'Attean::API::MixedStatementSerializer';
}

1;
