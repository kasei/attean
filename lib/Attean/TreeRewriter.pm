use v5.14;
use warnings;

=head1 NAME

Attean::TreeRewriter - Walk and rewrite subtrees

=head1 VERSION

This document describes Attean::TreeRewriter version 0.023

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $w = Attean::TreeRewriter->new();
  my ($rewritten, $tree) = $w->rewrite($tree, $thunk);
  if ($rewritten) {
	...
  }

=head1 DESCRIPTION

The Attean::TreeRewriter class walks the nodes of query trees and rewrites
sub-trees based on handlers that have been registered prior to rewriting.

=head1 ROLES

None.

=head1 METHODS

=over 4

=cut

package Attean::TreeRewriter 0.023 {
	use Moo;
	use Types::Standard qw(CodeRef ArrayRef Str);
	use Data::UUID;
	use Scalar::Util qw(blessed refaddr);
	use namespace::clean;
	with 'MooX::Log::Any';

	has types => (is => 'rw', isa => ArrayRef[Str], default => sub { ['Attean::API::DirectedAcyclicGraph'] });
	has pre_handlers => (is => 'rw', isa => ArrayRef[CodeRef], default => sub { [] });
	
=item C<< register_pre_handler( \&code ) >>

Register a handler that will be called for each sub-tree during tree rewriting.

The function will be called as C<< &code( $tree, $parent_node, $thunk ) >> where
C<< $thunk >> is an opaque value passed to C<< rewrite >>.

The function must return a list C<< ($handled, $descend, $rewritten) >>.
C<< $handled >> is a boolean indicating whether the handler function rewrote
the sub-tree, which is returned as C<< $rewritten >>. The C<< $descend >>
boolean value indicates whether the the tree rewriting should continue downwards
in the tree.

=cut

	sub register_pre_handler {
		my $self	= shift;
		my $code	= shift;
		push(@{ $self->pre_handlers }, $code);
	}
	
	sub _fire_pre_handlers {
		my $self	= shift;
		my ($t, $parent, $thunk)	= @_;
		my $main_descend	= 0;
		foreach my $cb (@{ $self->pre_handlers }) {
			my ($handled, $descend, $rewritten) = $cb->($t, $parent, $thunk);
            unless (defined($descend)) {
                $descend = 1;
            }
			if ($handled) {
				return ($descend, $rewritten);
			} elsif ($descend) {
				$main_descend	= 1;
			}
		}
		return ($main_descend, undef);
	}

=item C<< rewrite( $tree, $thunk, \%seen, $parent ) >>

Rewrites the given C<< $tree >> using the registered handler functions.
C<< $thunk >> is passed through to each handler function.
C<< %seen >> is currently unused.
C<< $parent >> is passed through to the handler functions as the value of the
pseudo-parent tree node for C<< $tree >>.

Returns a list C<< ($handled, $tree) >> with C<< $handled >> indicating whether
rewriting was performed, with the corresponding rewritten C<< $tree >>.

=cut

	sub rewrite {
		my $self	= shift;
		my $tree	= shift;
		my $thunk	= shift;
		my $seen	= shift || {};
		my $parent	= shift;
		my $ok		= 0;
# 		if ($seen->{ refaddr($tree) }++) {
# 			return (0, $tree);
# 		}
		foreach my $type (@{ $self->types }) {
			if (blessed($tree) and $tree->does($type)) {
				$ok++;
			}
		}
		unless ($ok) {
 			$self->log->debug(ref($tree) . ' does not conform to any rewrite roles');
			return (0, $tree);
		}
		
		my ($descend, $rewritten) = $self->_fire_pre_handlers($tree, $parent, $thunk);
		if ($rewritten) {
			if (refaddr($rewritten) == refaddr($tree)) {
				return (0, $tree);
			}
			if ($descend) {
				(undef, my $rewritten2) = $self->rewrite($rewritten, $thunk, $seen, $parent);
				my $changed	= (refaddr($rewritten) != refaddr($rewritten2));
				return ($changed, $rewritten2);
			} else {
				return (1, $rewritten);
			}
		}
		if ($descend) {
			my @children;
			my %attributes;
			my $changed = 0;
			if ($tree->does('Attean::API::DirectedAcyclicGraph')) {
				my @c	= @{ $tree->children };
				foreach my $i (0 .. $#c) {
					my $p	= $c[$i];
					my ($childchanged, $child) = $self->rewrite($p, $thunk, $seen, $tree);
					push(@children, $childchanged ? $child : $p);
					if ($childchanged) {
 						$self->log->debug("Child $p changed for parent $tree");
						$changed	 = 1;
					}
				}
			}
			
			if ($tree->can('tree_attributes')) {
				foreach my $attr ($tree->tree_attributes) {
					my $p	= $tree->$attr();
					if (ref($p) eq 'ARRAY') {
						my @patterns;
						foreach my $pp (@$p) {
# 							warn "- $attr: $pp\n";
							my ($childchanged, $child) = $self->rewrite($pp, $thunk, $seen, $tree);
							if ($childchanged) {
								$changed	 = 1;
							}
							push(@patterns, $child);
						}
						$attributes{$attr}	= \@patterns;
					} else {
# 						warn "- $attr: $p\n";
						my ($childchanged, $child) = $self->rewrite($p, $thunk, $seen, $tree);
						$attributes{$attr}	= $child;
						if ($childchanged) {
							$changed	 = 1;
						}
					}
				}
			}
			if ($changed) {
				my $class	= ref($tree);
				$rewritten	= $class->new( %attributes, children => \@children );
# 				(undef, $rewritten) = $self->rewrite($rewritten, $thunk, $seen, $parent);
				return (1, $rewritten);
			}
		}
		return (0, $tree);
	}
}

1;

__END__

=back

=head1 BUGS

Please report any bugs or feature requests to through the GitHub web interface
at L<https://github.com/kasei/attean/issues>.

=head1 SEE ALSO



=head1 AUTHOR

Gregory Todd Williams  C<< <gwilliams@cpan.org> >>

=head1 COPYRIGHT

Copyright (c) 2014--2019 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
