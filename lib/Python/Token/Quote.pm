package Python::Token::Quote;

=pod

=head1 NAME

Python::Token::Quote - String quote abstract base class

=head1 INHERITANCE

  Python::Token::Quote
  isa Python::Token
      isa Python::Element

=head1 DESCRIPTION

The C<Python::Token::Quote> class is never instantiated, and simply
provides a common abstract base class for the four quote classes.
In Python, a "quote" is limited to only the quote-like things that
themselves directly represent a string. (although this includes
double quotes with interpolated elements inside them, note that
L<String::InterpolatedVariables> allows to extract them).

The subclasses of C<Python::Token::Quote> are:

=over 2

=item C<''> - L<Python::Token::Quote::Simple>

=item C<''''''> - L<Python::Token::Quote::Docstring>

=item C<f''> - L<Python::Token::Quote::Formatted>

=back

=head1 METHODS

=cut

use strict;
use Python::Token ();

# VERSION

our @ISA = "Python::Token";





#####################################################################
# Python::Token::Quote Methods

=pod

=head2 string

The C<string> method is provided by all four ::Quote classes. It won't
get you the actual literal Perl value, but it will strip off the wrapping
of the quotes.

  # The following all return foo from the ->string method
  'foo'
  "foo"
  q{foo}
  qq <foo>

=cut

#sub string {
#	my $class = ref $_[0] || $_[0];
#	die "$class does not implement method ->string";
#}

=pod

=head2 literal

The C<literal> method is provided by ::Quote::Literal and
::Quote::Single.  This returns the value of the string as Perl sees
it: without the quote marks and with C<\\> and C<\'> resolved to C<\>
and C<'>.

The C<literal> method is not implemented by ::Quote::Double or
::Quote::Interpolate yet.

=cut

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
