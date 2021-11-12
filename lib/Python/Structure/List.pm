package Python::Structure::List;

=pod

=head1 NAME

Python::Structure::List - Explicit list or precedence ordering braces

=head1 SYNOPSIS

  # A list used for params
  function( 'param', 'param' );
  
  # Explicit list
  return ( 'foo', 'bar' );

=head1 INHERITANCE

  Python::Structure::List
  isa Python::Structure
      isa Python::Node
          isa Python::Element

=head1 DESCRIPTION

C<Python::Structure::List> is the class used for circular braces that
represent lists, and related.

=head1 METHODS

C<Python::Structure::List> has no methods beyond those provided by the
standard L<Python::Structure>, L<Python::Node> and L<Python::Element> methods.

=cut

use strict;
use Carp           ();
use Python::Structure ();

# VERSION

our @ISA = "Python::Structure";

# Highly special custom isa method that will continue to respond
# positively to ->isa('Python::Structure::ForLoop') but warns.
my $has_warned = 0;
sub isa {
	if ( $_[1] and $_[1] eq 'Python::Structure::ForLoop' ) {
		if (
			$_[0]->parent->isa('Python::Statement::Compound')
			and
			$_[0]->parent->type =~ /^for/
		) {
			unless ( $has_warned ) {
				local $Carp::CarpLevel = $Carp::CarpLevel + 1;
				Carp::carp("Python::Structure::ForLoop has been deprecated");
				$has_warned = 1;
			}
			return 1;
		}
	}
	return shift->SUPER::isa(@_);
}

1;

=pod

=head1 SUPPORT

See the L<support section|Python/SUPPORT> in the main module.

=head1 AUTHOR

Adam Kennedy E<lt>adamk@cpan.orgE<gt>

=head1 COPYRIGHT

Copyright 2001 - 2011 Adam Kennedy.

This program is free software; you can redistribute
it and/or modify it under the same terms as Perl itself.

The full text of the license can be found in the
LICENSE file included with this module.

=cut
