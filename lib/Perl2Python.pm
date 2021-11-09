package Perl2Python;

use warnings;
use strict;
use feature 'switch';
no if $] >= 5.018, warnings => 'experimental::smartmatch';
use PPI;
use Python;
use Exporter ();
use base qw(Exporter);
use Carp;

our @EXPORT_OK = qw(parse_document parse_file);   # symbols to export on request

our $VERSION = 1;

sub parse_document {
    my ($string) = @_;
    return map_element( PPI::Document->new($string), Python::Document->new );
}

sub parse_file {
    my ($file) = @_;
    return map_element( PPI::Document::File->new($file), Python::Document->new; );
}

sub map_element {
    my ( $pl_element, $py_element ) = @_;
    my $out = q{};
    if ( $nest_level > 0 ) {
        $out .= '    ' x $nest_level;
    }
    given ( ref $pl_element ) {
        when (/PPI::Token::Comment/xsm) {
            my $comment = $pl_element->content;
            if ( $pl_element->line and $comment =~ /^([#]!.*)perl/xsm ) {
                $comment = $1 . "python3\n";
            }
            $py_element->add_element( Python::Token::Comment->new($comment) );
        }
        when (/PPI::Statement::Include/xsm) {
            my $module = $pl_element->module;
            if ( $module !~ /^(warnings|strict)$/xsm )              {
                $module =~ s/::/./gsm;
                $py_element->add_element( Python::Statement::Include->new($module));
            }
        }
        when (/PPI::Statement::Sub/xsm) {
            use Data::Dumper;
            print STDERR Dumper($pl_element);
            my $iter  = next_non_whitespace($pl_element);
            my $child = $iter->();
            if ( $child->{content} ne 'sub' ) {
                croak "Error parsing sub statement: $pl_element";
            }
            $child = $iter->();
            $out .= "def $child->{content}(";
            my $block = $iter->();
            $iter  = next_non_whitespace($block);
            $child = $iter->();
            if ( $child->isa('PPI::Statement::Variable')
                and last_non_whitespace($child)->{content} eq '@_' )
            {
                print STDERR "Looking for variables\n";
                my @variables;
                my $variter = next_non_whitespace($child);
                while ( my $var = $variter->() ) {
                    print STDERR "var $var\n";
                    if ( $var->isa('PPI::Token::Word') ) {
                        next;
                    }
                    if (   $var->isa('PPI::Structure::List')
                        or $var->isa('PPI::Statement::Expression') )
                    {
                        print STDERR "list $var\n";
                        $variter = next_non_whitespace($var);
                        next;
                    }
                    if ( $var->isa('PPI::Token::Symbol') ) {
                        my $varname = $var->{content};
                        $varname =~ s/^[$@%]//sm;
                        push @variables, $varname;
                    }
                }
                $out .= join ', ', @variables;
            }
            $out .= "):\n";
            $out .= map_element( $block, $nest_level + 1 );
            return $out;
        }
        when (/PPI::Statement/xsm) {
            my ( @statement, $suppress_trailing_nl );
            my $iter = next_non_whitespace($pl_element);
            while ( my $child = $iter->() ) {
                if ( $child->isa('PPI::Token::Word') ) {
                    if ( $child->{content} =~ /^(?:print|return)/ ) {
                        push @statement, "$child->{content}(";
                    }
                }
                elsif ( $child->isa('PPI::Token::Quote') ) {
                    push @statement, $child->{content};
                    $suppress_trailing_nl =
                      (       $child->isa('PPI::Token::Quote::Double')
                          and $child->{content} =~ /\\n.$/xsm );
                }
            }
            if (@statement) {
                if ( $statement[0] eq 'print(' ) {
                    if ($suppress_trailing_nl) {
                        $statement[-1] = substr(
                            $statement[-1], 0,
                            0 - 2    # length \n
                              - 1    # length quote
                        ) . substr $statement[-1], 0, 1;
                    }
                }
                if ( $statement[0] =~ /[(]$/xsm ) { push @statement, ')' }
                $out .= join( q{ }, @statement ) . "\n";
            }
            return $out;
        }
        when (/PPI::Token::Whitespace/xsm) {
        }
    }
    if ( exists $pl_element->{children} ) {
        my $iter = next_non_whitespace($pl_element);
        while ( my $child = $iter->() ) {
            $out .= map_element( $child, $nest_level );
        }
    }
    return $out;
}

# an iterator for parsing PPI tree
# iterator returns the next element that is not whitespace
# my $iter = next_non_whitespace($parent);
# while (my $element = $iter->()) {}

sub next_non_whitespace {
    my ($parent) = @_;
    my $iter = 0;
    return sub {
        my $next;
        while ( exists $parent->{children}[$iter]
            and $parent->{children}[$iter]->isa('PPI::Token::Whitespace') )
        {
            $iter++;
        }
        return $parent->{children}[ $iter++ ];
    };
}

sub last_non_whitespace {
    my ($parent) = @_;
    my $iter = $#{ $parent->{children} };
    while ($parent->{children}[$iter]->isa('PPI::Token::Whitespace')
        or $parent->{children}[$iter]->isa('PPI::Token::Structure') )
    {
        $iter--;
    }
    print STDERR "last_non_whitespace returning $parent->{children}[$iter]\n";
    return $parent->{children}[$iter];
}

1;
__END__

=head1 NAME

Perl2Python - Tool to transcribe Perl 5 to Python 3

=head1 VERSION

1

=head1 SYNOPSIS

=head1 DESCRIPTION

=for readme stop

=head1 SUBROUTINES/METHODS

=head2 parse_document

=for readme continue

=head1 DIAGNOSTICS

=head1 CONFIGURATION AND ENVIRONMENT

=head1 DEPENDENCIES

=head2 Runtime

=head2 Build

=head2 Test

=head1 INCOMPATIBILITIES

=head1 BUGS AND LIMITATIONS

=head1 SEE ALSO

=head1 AUTHOR

Jeffrey Ratcliffe, E<lt>jffry@posteo.netE<gt>

=head1 LICENSE AND COPYRIGHT

Copyright (C) 2021 by Jeffrey Ratcliffe

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.5 or,
at your option, any later version of Perl 5 you may have available.

=cut
