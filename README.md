# NAME

Perl2Python - Tool to transcribe Perl 5 to Python 3

# VERSION

1

# SYNOPSIS

This module attempts to transcribe Perl 5 code to Python 3. This will not be
successful for all but the simplest code, but it should do most of the legwork.

# DESCRIPTION

You will probably be left with the following problems to solve:

- Python is much stricter about types than Perl. Perl will happily convert a
string with a number to a number for comparison purposes. Python requires this
to be explicit, so you may well have to add casts.
- Similarly, in Perl, you can create a scalar and turn it into a hashref or an
arrayref by using hash or array functions on it. In Python, you have to
explicitly initialise the dict or list first, so these statements will have to
be added.
- Perl is fairly flexible about the contents of strings. Python strictly separates
bytes and strings. Python code will often require extra encode() or decode()
calls to convert from one to the other.
- Perl has a very loose concept of instance variables in classes. In Python, these
must previously declared.
- Perl does not distinguish between instance variables in classes and hash keys.
perl2python makes a guess that any keys for objects called "self" are instance
variables, and anything else are dict keys. This will often be wrong.
- All Perl modules that are in the list of directory to be searched are
automatically identified. Python modules are identified partly by their path,
and thus either the import statements or their calls will often have to be
adjusted.
- Capture groups in Perl substitutions cannot trivially be transcribed, as they
typically contain modifiers, e.g.:

        s/(^\w)/\U$1/xsmg

    will need something like

        re.sub(r"(^\w)",lambda x: x.group(1).upper(),string_to_match)

- Unless prototypes are used when defined Perl subs, Perl does not care about the
number of arguments. Python will throw an error if the number of arguments is
unexpected. Thus Perl code handling default argument values will need to be
manually adjusted, as checking this is non-trivial.

# SUBROUTINES/METHODS

## map\_document

# DIAGNOSTICS

# CONFIGURATION AND ENVIRONMENT

# DEPENDENCIES

## Runtime

## Build

## Test

# INCOMPATIBILITIES

# BUGS AND LIMITATIONS

# SEE ALSO

# AUTHOR

Jeffrey Ratcliffe, <jffry@posteo.net>

# LICENSE AND COPYRIGHT

Copyright (C) 2021-2022 by Jeffrey Ratcliffe

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.5 or,
at your option, any later version of Perl 5 you may have available.
