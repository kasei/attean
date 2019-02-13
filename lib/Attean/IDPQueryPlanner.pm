use v5.14;
use warnings;

=head1 NAME

Attean::IDPQueryPlanner - Iterative dynamic programming query planner

=head1 VERSION

This document describes Attean::IDPQueryPlanner version 0.021

=head1 SYNOPSIS

  use v5.14;
  use Attean;
  my $planner = Attean::IDPQueryPlanner->new();
  my $default_graphs = [ Attean::IRI->new('http://example.org/') ];
  my $plan = $planner->plan_for_algebra( $algebra, $model, $default_graphs );
  my $iter = $plan->evaluate($model);
  my $iter = $e->evaluate( $model );

=head1 DESCRIPTION

The Attean::IDPQueryPlanner class implements a query planner using the
iterative dynamic programming approach.

=head1 ATTRIBUTES

=over 4

=back

=head1 METHODS

=over 4

=cut

package Attean::IDPQueryPlanner 0.021 {
	use Moo;
	use namespace::clean;
	
	extends 'Attean::QueryPlanner';
	with 'Attean::API::IDPJoinPlanner';
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

Copyright (c) 2014--2018 Gregory Todd Williams.
This program is free software; you can redistribute it and/or modify it under
the same terms as Perl itself.

=cut
