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
        }
        when (/PPI::Statement::Compound/xsm) {
            if ( $element->child(0) eq 'if' ) {
                my $condition =
                  $element->find_first('PPI::Structure::Condition');
                $element->__insert_after_child( $condition,
                    $element->find_first('PPI::Statement::Expression')
                      ->remove );
                $condition->delete;
            }
        }
        when (/PPI::Statement::Variable/xsm) {
            map_variable($element);
        }
        when (/PPI::Statement/xsm) {
            map_statement($element)
        }
        when (/PPI::Structure::Block/xsm) {
            $element->{start}->{content}  = q{:};
            $element->{finish}->{content} = q{};
        }
        when (/PPI::Token::Regexp::Match/xsm) {
            map_regex_match($element)
        }
        when (/PPI::Token::Symbol/xsm) {
            $element->{content} =~ s/^[\$@%]//smx;
        }
        when (/PPI::Token::Word/xsm) {
            if ( $element eq 'defined' ) {
                my $parent = $element->parent;
                $element->{content} = 'is';
                $parent->__insert_after_child(
                    $element->snext_sibling,
                    PPI::Token::Whitespace->new(q{ }),
                    $element->remove,
                    PPI::Token::Whitespace->new(q{ }),
                    PPI::Token::Word->new('not'),
                    PPI::Token::Whitespace->new(q{ }),
                    PPI::Token::Word->new('None')
                );
            }
            elsif ( $element eq 'shift' ) {
                my $parent = $element->parent;
                $element->{content} = '.pop(0)';
                $parent->__insert_after_child( $element->snext_sibling,
                    $element->remove );
            }
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

sub map_variable {
    my ($element) = @_;
    if ( $element->child(0) eq 'my' ) {
        $element->child(0)->delete;    # my
        $element->child(0)->delete;    # whitespace
    }

    # magic defined in sub
    my $magic = $element->find_first('PPI::Token::Magic');
    if ( $magic eq '@_' ) {    ## no critic (RequireInterpolationOfMetachars)
        my $source_list = $element->find_first('PPI::Structure::List');
        my $dest_list =
          $element->parent->parent->find_first('PPI::Structure::List');
        for my $child ( $source_list->children ) {
            $dest_list->add_element( $child->remove );
        }
        $element->delete;
        map_element($dest_list);
        return;
    }

    # magic defined in regex capture, move capture out of condition
    # and use it to fetch group
    elsif ( $magic =~ /^\$(\d+)$/xsm ) {
        my $block     = $element->parent;
        my $compound  = $block->parent;
        my $parent    = $compound->parent;
        my $condition = $block->sprevious_sibling;
        my $search    = $condition->find_first(
            sub {
                $_[1]->isa('PPI::Token::Word')
                  and $_[1]->content eq 're.search';
            }
        );
        my $list      = $search->snext_sibling;
        my $regex_var = PPI::Statement::Variable->new;
        $regex_var->add_element( PPI::Token::Symbol->new('regex') );
        $regex_var->add_element( PPI::Token::Operator->new(q{=}) );
        $parent->__insert_before_child( $compound, $regex_var );
        $condition->__insert_before_child( $search,
            PPI::Token::Symbol->new('regex') );
        $regex_var->add_element( $search->remove );
        $regex_var->add_element( $list->remove );
        $compound->__insert_before_child( $compound->child(0),
            PPI::Token::Whitespace->new("\n") );

        # replace the magic with the regex group
        $element->__insert_before_child( $magic,
            PPI::Token::Word->new("regex.group($1)") );
        $magic->delete;
    }
    my $shift = $element->find_first(
        sub {
            $_[1]->isa('PPI::Token::Word')
              and $_[1]->content eq 'shift'
              and $_[1]->snext_sibling->isa('PPI::Token::Structure');
        }
    );
    if ($shift) {
        my $source = $element->find_first('PPI::Token::Symbol');
        my $dest_list =
          $element->parent->parent->find_first('PPI::Structure::List');
        if ( $dest_list->children ) {
            $dest_list->add_element( PPI::Token::Operator->new(q{,}) );
        }
        $dest_list->add_element( $source->remove );
        $element->delete;
        map_element($dest_list);
        return;
    }
    remove_trailing_semicolon($element);
    return;
}

sub map_regex_match {
    my ($element) = @_;

    # in perl, the parent is a PPI::Statement::Expression, which we now
    # turn into re.search(regex, string)
    my $expression = $element->parent;
    $expression->add_element( PPI::Token::Word->new('re.search') );
    my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
    $list->{finish} = PPI::Token::Structure->new(')');
    $expression->add_element($list);
    my $operator = $element->sprevious_sibling;
    if ( not $operator->isa('PPI::Token::Operator') ) {
        croak "Expected operator before regex match. Found '$operator'\n";
    }
    my $string = $operator->sprevious_sibling;
    if ( not $string->isa('PPI::Token::Symbol') ) {
        croak "Expected symbol before operator. Found '$string'\n";
    }
    $list->add_element( $element->remove );

    if ( $operator eq q{=~} ) {
    }
    elsif ( $operator eq q{!~} ) {
        $expression->__insert_before_child( $element,
            PPI::Token::Word->new('not') );
        $expression->__insert_before_child( $element,
            PPI::Token::Whitespace->new(q{ }) );
    }
    else {
        croak "Unknown operator '$operator'\n";
    }
    $operator->delete;
    $list->add_element( PPI::Token::Operator->new(q{,}) );
    $list->add_element( $string->remove );

    # remove the flags convert the separator to Python string terminators
    my $separator = $element->{separator};
    $element->{content} =~ s{^$separator}{r'}xsm;
    $element->{content} =~ s{$separator[xsmg]+$}{'}xsm;

    # ensure we have import re
    my $document = $element->top;
    if (
        not $document->find_first(
            sub {
                $_[1]->isa('PPI::Statement::Include')
                  and $_[1]->content eq 'import re';
            }
        )
      )
    {
        my $statement = PPI::Statement::Include->new;
        $statement->add_element( PPI::Token::Word->new('import') );
        $statement->add_element( PPI::Token::Whitespace->new(q{ }) );
        $statement->add_element( PPI::Token::Word->new('re') );
        $document->__insert_before_child( $document->child(0),
            PPI::Token::Whitespace->new("\n") );
        $document->__insert_before_child( $document->child(0), $statement );
    }
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
