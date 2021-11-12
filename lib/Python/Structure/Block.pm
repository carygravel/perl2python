package Python::Structure::Block;

=pod

=head1 NAME

Python::Structure::Block - Curly braces representing a code block

=head1 SYNOPSIS

  sub foo { ... }
  
  grep { ... } @list;
  
  if ( condition ) {
      ...
  }
  
  LABEL: {
      ...
  }

=head1 INHERITANCE

  Python::Structure::Block
  isa Python::Structure
      isa Python::Node
          isa Python::Element

=head1 DESCRIPTION

C<Python::Structure::Block> is the class used for all curly braces that
represent code blocks. This includes subroutines, compound statements
and any other block braces.

=head1 METHODS

C<Python::Structure::Block> has no methods beyond those provided by the
standard L<Python::Structure>, L<Python::Node> and L<Python::Element> methods.

=cut

use strict;
use Python::Structure ();

# VERSION

our @ISA = "Python::Structure";





#####################################################################
# Python::Element Methods

# This is a scope boundary
sub scope() { 1 }

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
