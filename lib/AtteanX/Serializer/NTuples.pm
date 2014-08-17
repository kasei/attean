use v5.14;
use warnings;

package AtteanX::Serializer::NTuples 0.001 {
	use Moose;
	use Encode qw(encode);
	use Attean::ListIterator;
	use List::MoreUtils qw(any);
	
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
