use v5.14;
use warnings;

package RDF 0.001 {
	use Attean::API;
	
	use Attean::Blank;
	use Attean::Literal;
	use Attean::Variable;
	use Attean::IRI;
	
	use Attean::Triple;
	use Attean::Quad;
	
	use Attean::ListIterator;
	
	use List::MoreUtils qw(any all);
	use Module::Load::Conditional qw(can_load);
	use Module::Pluggable search_path => 'AtteanX::Parser', sub_name => 'parsers', max_depth => 3;
	use Module::Pluggable search_path => 'AtteanX::Serializer', sub_name => 'serializers', max_depth => 3;
	
	sub get_serializer {
		my $self	= shift;
		return $self->get_plugin('serializers', shift, 'Attean::API::Serializer');
	}
	
	sub get_parser {
		my $self	= shift;
		return $self->get_plugin('parsers', shift, 'Attean::API::Parser');
	}
	
	sub get_plugin {
		my $self	= shift;
		my $type	= shift;
		my $name	= shift;
		my @roles	= @_;
		foreach my $p ($self->$type()) {
			if (substr($p, -(length($name)+2)) eq "::$name") {
				unless (can_load( modules => { $p => 0 })) {
					warn $Module::Load::Conditional::ERROR;
					return;
				}
				
				foreach (@roles) {
					my $err	= Moose::Meta::TypeConstraint::Role->new(role => $_)->validate($p);
					die  ucfirst($type) . " class $p failed validation for role $_" if ($err);
				}
				return $p;
			}
		}
	}
}

1;

__END__
