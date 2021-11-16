package Perl2Python;

use warnings;
use strict;
use feature 'switch';
no if $] >= 5.018, warnings => 'experimental::smartmatch';
use PPI;
use Exporter ();
use base qw(Exporter);
use Carp;
use Readonly;
Readonly my $LAST => -1;

our @EXPORT_OK = qw(parse_document parse_file);   # symbols to export on request

our $VERSION = 1;

sub parse_document {
    my ($string) = @_;
    my $doc = PPI::Document->new($string);
    map_element($doc);
    return $doc;
}

sub parse_file {
    my ($file) = @_;
    my $doc = PPI::Document::File->new($file);
    map_element($doc);
    return $doc;
}

sub map_element {
    my ($element) = @_;
    given ( ref $element ) {
        when (/PPI::Document/xsm) {

            #            use Data::Dumper;
            #            print STDERR Dumper($element);
        }
        when (/PPI::Token::Comment/xsm) {
            my $comment = $element->content;
            if ( $element->line and $comment =~ /^([#]!.*)perl/xsm ) {
                $element->{content} = $1 . "python3\n";
            }
        }
        when (/PPI::Statement::Include/xsm) {
            my $module = $element->module;
            if ( $module =~ /^(warnings|strict|feature|if)$/xsm ) {
                my $whitespace = $element->next_sibling;
                if (    defined $whitespace
                    and $whitespace->isa('PPI::Token::Whitespace')
                    and $whitespace =~ /\n/xsm )
                {
                    $whitespace->delete;
                }
                $element->delete;
                return;
            }
            $module =~ s/::/./gsm;
            my $import = $element->schild(0);
            if ( $import ne 'use' ) {
                croak "Unrecognised include $element\n";
            }
            $import->{content} = 'import';
            $import            = $import->snext_sibling;
            $import->{content} = $module;
            remove_trailing_semicolon($element);
        }
        when (/PPI::Statement::Sub/xsm) {
            my $name = $element->name;
            if ( not defined $name or $name eq q{} ) {
                croak "Anonymous subs not yet supported\n";
            }
            my $child = $element->schild(0);
            if ( $child ne 'sub' ) {
                croak "Unknown sub: $element\n";
            }
            $child->{content} = 'def';
            $child = $child->snext_sibling;
            my $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $list->{finish} = PPI::Token::Structure->new(')');
            $element->__insert_after_child( $child, $list );
            my $block = $element->find_first('PPI::Structure::Block');
            if ( not defined $block ) {
                croak "sub without block not supported\n";
            }
            $block->{start}->{content}  = q{:};
            $block->{finish}->{content} = q{};
        }
        when (/PPI::Statement::Variable/xsm) {
            my $magic = $element->find_first('PPI::Token::Magic');
            if ($magic) {
                my $source_list = $element->find_first('PPI::Structure::List');
                my $dest_list =
                  $element->parent->parent->find_first('PPI::Structure::List');
                for my $child ( $source_list->children ) {
                    $dest_list->add_element( $child->remove );
                }
                $element->delete;
                map_element($dest_list);
            }
        }
        when (/PPI::Statement/xsm) {
            map_statement($element)
        }
        when (/PPI::Token::Symbol/xsm) {
            $element->{content} =~ s/^[\$@%]//smx;
        }
    }
    if ( exists $element->{children} ) {
        for my $child ( $element->children ) {
            map_element($child);
        }
    }
    return;
}

sub map_statement {
    my ($element) = @_;
    my $child = $element->schild(0);
    if ( $child eq 'print' ) {
        my $list = $child->snext_sibling;
        if ( $list ne '(' ) {
            $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $list->{finish} = PPI::Token::Structure->new(')');
            my $print = $child;
            my @children;
            $child = $child->next_sibling;
            while ( $child
                and not $child->isa('PPI::Token::Structure') )
            {
                push @children, $child;
                $child = $child->next_sibling;
            }
            for my $child (@children) {
                $list->add_element( $child->remove );
            }
            $element->__insert_after_child( $print, $list );
        }
        my $quote = $list->schild($LAST);
        if ( $quote->isa('PPI::Token::Quote::Double') ) {
            $quote->{content} =~ s/\\n"$/"/gsmx;
        }
    }
    remove_trailing_semicolon($element);
    return;
}

sub nest_level {
    my ($element) = @_;
    my $level = 0;
    while ( my $parent = $element->parent ) {
        if ( $element eq $parent ) {
            return $level;
        }
        if ( $element->isa('PPI::Structure::Block') ) {
            $level++;
        }
        elsif ( $element->isa('PPI::Document') ) {
            return $level;
        }
        $element = $parent;
    }
    return $level;
}

sub remove_trailing_semicolon {
    my ($parent) = @_;
    my $child = $parent->schild($LAST);
    if ( $child eq q{;} ) {
        $child->delete;
    }
    return;
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
