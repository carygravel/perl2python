package Python::Token;

=pod

=head1 NAME

Python::Token - A single token of Perl source code

=head1 INHERITANCE

  Python::Token
  isa Python::Element

=head1 DESCRIPTION

C<Python::Token> is the abstract base class for all Tokens. In Python terms, a "Token" is
a L<Python::Element> that directly represents bytes of source code.

=head1 METHODS

=cut

use strict;
use Params::Util   qw{_INSTANCE};
use Python::Element   ();
use Python::Exception ();

# VERSION

our @ISA = 'Python::Element';

# We don't load the abstracts, they are loaded
# as part of the inheritance process.

# Load the token classes
use Python::Token::BOM                   ();
use Python::Token::Whitespace            ();
use Python::Token::Comment               ();
#use Python::Token::Pod                   ();
#use Python::Token::Number                ();
#use Python::Token::Number::Binary        ();
#use Python::Token::Number::Octal         ();
#use Python::Token::Number::Hex           ();
#use Python::Token::Number::Float         ();
#use Python::Token::Number::Exp           ();
#use Python::Token::Number::Version       ();
#use Python::Token::Word                  ();
#use Python::Token::DashedWord            ();
#use Python::Token::Symbol                ();
#use Python::Token::ArrayIndex            ();
#use Python::Token::Magic                 ();
#use Python::Token::Quote::Single         ();
#use Python::Token::Quote::Double         ();
#use Python::Token::Quote::Literal        ();
#use Python::Token::Quote::Interpolate    ();
#use Python::Token::QuoteLike::Backtick   ();
#use Python::Token::QuoteLike::Command    ();
#use Python::Token::QuoteLike::Regexp     ();
#use Python::Token::QuoteLike::Words      ();
#use Python::Token::QuoteLike::Readline   ();
#use Python::Token::Regexp::Match         ();
#use Python::Token::Regexp::Substitute    ();
#use Python::Token::Regexp::Transliterate ();
#use Python::Token::Operator              ();
#use Python::Token::Cast                  ();
use Python::Token::Structure             ();
#use Python::Token::Label                 ();
#use Python::Token::HereDoc               ();
#use Python::Token::Separator             ();
#use Python::Token::Data                  ();
#use Python::Token::End                   ();
#use Python::Token::Prototype             ();
#use Python::Token::Attribute             ();
#use Python::Token::Unknown               ();





#####################################################################
# Constructor and Related

sub new {
	bless { content => (defined $_[1] ? "$_[1]" : '') }, $_[0];
}

sub set_class {
	my $self  = shift;
	# @_ or throw Exception("No arguments to set_class");
	my $class = substr( $_[0], 0, 12 ) eq 'Python::Token::' ? shift : 'Python::Token::' . shift;

	# Find out if the current and new classes are complex
	my $old_quote = (ref($self) =~ /\b(?:Quote|Regex)\b/o) ? 1 : 0;
	my $new_quote = ($class =~ /\b(?:Quote|Regex)\b/o)     ? 1 : 0;

	# No matter what happens, we will have to rebless
	bless $self, $class;

	# If we are changing to or from a Quote style token, we
	# can't just rebless and need to do some extra thing
	# Otherwise, we have done enough
	return $class if ($old_quote - $new_quote) == 0;

	# Make a new token from the old content, and overwrite the current
	# token's attributes with the new token's attributes.
	my $token = $class->new( $self->{content} );
	%$self = %$token;

	# Return the class as a convenience
	return $class;
}





#####################################################################
# Python::Token Methods

=pod

=head2 set_content $string

The C<set_content> method allows you to set/change the string that the
C<Python::Token> object represents.

Returns the string you set the Token to

=cut

sub set_content {
	$_[0]->{content} = $_[1];
}

=pod

=head2 add_content $string

The C<add_content> method allows you to add additional bytes of code
to the end of the Token.

Returns the new full string after the bytes have been added.

=cut

sub add_content { $_[0]->{content} .= $_[1] }

=pod

=head2 length

The C<length> method returns the length of the string in a Token.

=cut

sub length { CORE::length($_[0]->{content}) }





#####################################################################
# Overloaded Python::Element methods

sub content {
	$_[0]->{content};
}

# You can insert either a statement, or a non-significant token.
sub insert_before {
	my $self    = shift;
	my $Element = _INSTANCE(shift, 'Python::Element')  or return undef;
	if ( $Element->isa('Python::Structure') ) {
		return $self->__insert_before($Element);
	} elsif ( $Element->isa('Python::Token') ) {
		return $self->__insert_before($Element);
	}
	'';
}

# As above, you can insert a statement, or a non-significant token
sub insert_after {
	my $self    = shift;
	my $Element = _INSTANCE(shift, 'Python::Element') or return undef;
	if ( $Element->isa('Python::Structure') ) {
		return $self->__insert_after($Element);
	} elsif ( $Element->isa('Python::Token') ) {
		return $self->__insert_after($Element);
	}
	'';
}





#####################################################################
# Tokenizer Methods

sub __TOKENIZER__on_line_start() { 1 }
sub __TOKENIZER__on_line_end()   { 1 }
sub __TOKENIZER__on_char()       { 'Unknown' }





#####################################################################
# Lexer Methods

sub __LEXER__opens {
	ref($_[0]) eq 'Python::Token::Structure'
	and
	$_[0]->{content} =~ /(?:\(|\[|\{)/
}

sub __LEXER__closes {
	ref($_[0]) eq 'Python::Token::Structure'
	and
	$_[0]->{content} =~ /(?:\)|\]|\})/
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
