package Python::Statement::Def;

=pod

=head1 NAME

Python::Statement::Def - Defroutine declaration

=head1 INHERITANCE

  Python::Statement::Def
  isa Python::Statement
      isa Python::Node
          isa Python::Element

=head1 DESCRIPTION

Except for the special BEGIN, CHECK, UNITCHECK, INIT, and END subroutines
(which are part of L<Python::Statement::Scheduled>) all subroutine declarations
are lexed as a Python::Statement::Def object.

Primarily, this means all of the various C<sub foo {}> statements, but also
forward declarations such as C<sub foo;> or C<sub foo($);>. It B<does not>
include anonymous subroutines, as these are merely part of a normal statement.

=head1 METHODS

C<Python::Statement::Def> has a number of methods in addition to the standard
L<Python::Statement>, L<Python::Node> and L<Python::Element> methods.

=cut

use strict;
use List::Util     ();
use Params::Util   qw{_INSTANCE};
use Python::Statement ();

# VERSION

our @ISA = "Python::Statement";

# Lexer clue
sub __LEXER__normal() { '' }

sub _complete {
	my $child = $_[0]->schild(-1);
	return !! (
		defined $child
		and
		$child->isa('Python::Structure::Block')
		and
		$child->complete
	);
}





#####################################################################
# Python::Statement::Def Methods

=pod

=head2 name

The C<name> method returns the name of the subroutine being declared.

In some rare cases such as a naked C<sub> at the end of the file, this may return
false.

=cut

sub name {
	my ($self) = @_;

	# Usually the second token is the name.
	my $token = $self->schild(1);
	return $token->content
	  if defined $token and $token->isa('Python::Token::Word');

	# In the case of special subs whose 'sub' can be omitted (AUTOLOAD
	# or DESTROY), the name will be the first token.
	$token = $self->schild(0);
	return $token->content
	  if defined $token and $token->isa('Python::Token::Word');
	return '';
}

=pod

=head2 prototype

If it has one, the C<prototype> method returns the subroutine's prototype.
It is returned in the same format as L<Python::Token::Prototype/prototype>,
cleaned and removed from its brackets.

Returns the subroutine's prototype, or undef if the subroutine does not
define one. Note that when the sub has an empty prototype (C<()>) the
return is an empty string.

=cut

sub prototype {
	my $self      = shift;
	my $Prototype = List::Util::first {
		_INSTANCE($_, 'Python::Token::Prototype')
	} $self->children;
	defined($Prototype) ? $Prototype->prototype : undef;
}

=pod

=head2 block

With its name and implementation shared with L<Python::Statement::Scheduled>,
the C<block> method finds and returns the actual Structure object of the
code block for this subroutine.

Returns false if this is a forward declaration, or otherwise does not have a
code block.

=cut

sub block {
	my $self = shift;
	my $lastchild = $self->schild(-1) or return '';
	$lastchild->isa('Python::Structure::Block') and $lastchild;
}

=pod

=head2 forward

The C<forward> method returns true if the subroutine declaration is a
forward declaration.

That is, it returns false if the subroutine has a code block, or true
if it does not.

=cut

sub forward {
	! shift->block;
}

=pod

=head2 reserved

The C<reserved> method provides a convenience method for checking to see
if this is a special reserved subroutine. It does not check against any
particular list of reserved sub names, but just returns true if the name
is all uppercase, as defined in L<perlsub>.

Note that in the case of BEGIN, CHECK, UNITCHECK, INIT and END, these will be
defined as L<Python::Statement::Scheduled> objects, not subroutines.

Returns true if it is a special reserved subroutine, or false if not.

=cut

sub reserved {
	my $self = shift;
	my $name = $self->name or return '';
	# perlsub is silent on whether reserveds can contain:
	# - underscores;
	# we allow them due to existing practice like CLONE_SKIP and __SUB__.
	# - numbers; we allow them by Python tradition.
	$name eq uc $name;
}

=pod

=head2 type

The C<type> method checks and returns the declaration type of the statement,
which will be one of 'my', 'our', or 'state'.

Returns a string of the type, or C<undef> if the type is not declared.

=cut

sub type {
	my $self = shift;

	# Get the first significant child
	my @schild = grep { $_->significant } $self->children;

	# Ignore labels
	shift @schild if _INSTANCE($schild[0], 'Python::Token::Label');

	# Get the type
	(_INSTANCE($schild[0], 'Python::Token::Word') and $schild[0]->content =~ /^(my|our|state)$/)
		? $schild[0]->content
		: undef;
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
