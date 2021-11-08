package Perl2Python;

use warnings;
use strict;
use feature 'switch';
no if $] >= 5.018, warnings => 'experimental::smartmatch';
use PPI;
use Exporter ();
use base qw(Exporter);

our @EXPORT_OK = qw(parse_document parse_file);   # symbols to export on request

our $VERSION = 1;

sub parse_document {
    my ($string) = @_;
    return parse_element( PPI::Document->new($string) );
}

sub parse_file {
    my ($file) = @_;
    return parse_element( PPI::Document::File->new($file) );
}

sub parse_element {
    my ($element) = @_;
    my $out = q{};
    given ( ref $element ) {
        when (/PPI::Token::Comment/xsm) {
            if ( $element->{content} =~ /^([#]!.*)perl/xsm ) {
                $out .= $1 . "python3\n";
            }
            else {
                $out .= $element->{content};
            }
            return $out;
        }
        when (/PPI::Statement::Include/xsm) {
            my @imports;
            for my $child ( @{ $element->{children} } ) {
                if ( $child->isa('PPI::Token::Word') ) {
                    if ( $child->{content} =~
                        /(use|warnings|strict|no|feature|if)/xsm )
                    {
                        next;
                    }
                    else {
                        my $module = $child->{content};
                        $module =~ s/::/./gsm;
                        push @imports, $module;
                    }
                }
            }
            if (@imports) {
                $out .= 'import ' . join( q{ }, @imports ) . "\n";
            }
            return $out;
        }
        when (/PPI::Statement/xsm) {
            my ( @statement, $suppress_trailing_nl );
            for my $child ( @{ $element->{children} } ) {
                if ( $child->isa('PPI::Token::Word') ) {
                    if ( $child->{content} eq 'print' ) {
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
            if ( @statement and $statement[0] eq 'print(' ) {
                if ($suppress_trailing_nl) {
                    $statement[-1] = substr(
                        $statement[-1], 0,
                        0 - 2    # length \n
                          - 1    # length quote
                    ) . substr $statement[-1], 0, 1;
                }
            }
            if ( $statement[0] =~ /[(]$/xsm ) { push @statement, ')' }
            if (@statement) {
                $out .= join( q{ }, @statement ) . "\n";
            }
            return $out;
        }
        when (/PPI::Token::Whitespace/xsm) {
        }
    }
    if ( exists $element->{children} ) {
        for my $child ( @{ $element->{children} } ) {
            $out .= parse_element($child);
        }
    }
    return $out;
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
