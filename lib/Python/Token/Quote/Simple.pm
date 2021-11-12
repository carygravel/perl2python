package Python::Token::Quote::Simple;

=pod

=head1 NAME

Python::Token::Quote::Simple - A 'simple quote' token

=head1 INHERITANCE

  Python::Token::Quote::Simple
  isa Python::Token::Quote
      isa Python::Token
          isa Python::Element

=head1 SYNOPSIS

  'This is a simple quote'
  
  q{This is a literal, but NOT a simple quote}

=head1 DESCRIPTION

A C<Python::Token::Quote::Simple> object represents a simple quoted string
literal. 

=head1 METHODS

There are no methods available for C<Python::Token::Quote::Simple> beyond
those provided by the parent L<Python::Token::Quote>, L<Python::Token> and
L<Python::Element> classes.

=cut

use strict;
use Python::Token::Quote ();
use Python::Token::_QuoteEngine::Simple ();

# VERSION

our @ISA = qw{
	Python::Token::_QuoteEngine::Simple
	Python::Token::Quote
};





#####################################################################
# Python::Token::Quote Methods

sub string {
	my $str = $_[0]->{content};
	substr( $str, 1, length($str) - 2 );
}


my %UNESCAPE = (
	"\\'"  => "'",
	"\\\\" => "\\",
);

sub literal {
	# Unescape \\ and \' ONLY
	my $str = $_[0]->string;
	$str =~ s/(\\.)/$UNESCAPE{$1} || $1/ge;
	return $str;
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
