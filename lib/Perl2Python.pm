package Perl2Python;

use warnings;
use strict;
use feature 'switch';
no if $] >= 5.018, warnings => 'experimental::smartmatch';
use PPI;
use Python::Document;
use Python::Statement::Include;
use Python::Statement::Def;
use Python::Structure::Block;
use Python::Token::Comment;
use Python::Token::Word;
use Python::Token::Quote;
use Exporter ();
use base qw(Exporter);
use Carp;

our @EXPORT_OK = qw(parse_document parse_file);   # symbols to export on request

our $VERSION = 1;

sub parse_document {
    my ($string) = @_;
    return map_element( PPI::Document->new($string) );
}

sub parse_file {
    my ($file) = @_;
    return map_element( PPI::Document::File->new($file) );
}

sub map_element {
    my ( $pl_element ) = @_;
    given ( ref $pl_element ) {
        when (/PPI::Document/xsm) {
            my $doc = Python::Document->new;
            for my $child ($pl_element->children) {
                my $py_element = map_element( $child );
                if (defined $py_element) {
                    $doc->add_element( $py_element );
                }
            }
            return $doc
        }
        when (/PPI::Token::Comment/xsm) {
            my $comment = $pl_element->content;
            if ( $pl_element->line and $comment =~ /^([#]!.*)perl/xsm ) {
                $comment = $1 . "python3\n";
            }
            return Python::Token::Comment->new($comment)
        }
        when (/PPI::Token::Word/xsm) {
            my $word = $pl_element->content;
            return Python::Token::Word->new($word)
        }
        when (/PPI::Token::Quote/xsm) {
            my $quote = $pl_element->content;
            return Python::Token::Quote->new($quote)
        }
        when (/PPI::Statement::Include/xsm) {
            my $module = $pl_element->module;
            if ( $module =~ /^(warnings|strict|feature|if)$/xsm )              {return}
            $module =~ s/::/./gsm;
            my $include = Python::Statement::Include->new();
            $include->add_element( Python::Token::Word->new('import') );
            $include->add_element( Python::Token::Whitespace->new(q{ }) );
            $include->add_element( Python::Token::Word->new($module) );
            $include->add_element( Python::Token::Whitespace->new("\n") );
            return $include;
        }
        when (/PPI::Statement::Sub/xsm) {
            use Data::Dumper;
            print STDERR Dumper($pl_element);
            my $name = $pl_element->name;
            if (not defined $name or $name eq q{}) {
                croak "Anonymous subs not yet supported\n";
            }
            my $def = Python::Statement::Def->new();
            $def->add_element( Python::Token::Word->new('def') );
            $def->add_element( Python::Token::Whitespace->new(q{ }) );
            $def->add_element( Python::Token::Word->new($name) );
            my $list = Python::Structure::List->new;
            $list->{start} = Python::Token::Structure->new('(');
            $list->{finish} = Python::Token::Structure->new(')');
            $def->add_element( $list );

            my $pl_block = $pl_element->find_first('PPI::Structure::Block');
            if (not defined $pl_block) {
                croak "sub without block not supported\n";
            }
            $def->add_element( Python::Token::Structure->new(':') );
            $def->add_element( Python::Token::Whitespace->new("\n") );
            my $block = Python::Structure::Block->new;
            $def->add_element($block);
            for my $child ($pl_block->children) {
                my $py_element = map_element( $child );
                if (defined $py_element) {
                    $block->add_element( $py_element );
                }
            }
            return $def;


            my $iter  = next_non_whitespace($pl_element);
            my $child = $iter->();
            if ( $child->{content} ne 'sub' ) {
                croak "Error parsing sub statement: $pl_element";
            }
            $child = $iter->();
            #$out .= "def $child->{content}(";
            #my $block = $iter->();
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
                #$out .= join ', ', @variables;
            }
            #$out .= "):\n";
            #$out .= map_element( $block, $nest_level + 1 );
            #return $out;
        }
        when (/PPI::Statement/xsm) {
            my $statement = Python::Statement->new;
            my $nest_level = nest_level($pl_element);
            if ($nest_level > 0) {
                $statement->add_element( Python::Token::Whitespace->new('    'x$nest_level) );
            }

            my $container = $statement;
            for my $child ($pl_element->children) {
                my $py_element = map_element( $child );
                if (defined $py_element) {
                    $container->add_element( $py_element );
                }
                if ($child eq 'print' and $child->snext_sibling ne '(') {
                    $container = Python::Structure::List->new;
                    $container->{start} = Python::Token::Structure->new('(');
                    $container->{finish} = Python::Token::Structure->new(')');
                    $statement->add_element( $container );
                }
            }
            if ($pl_element->schild(0) eq 'print') {
                my $pl_quote = $pl_element->schild(-2);
                if ($pl_quote->isa('PPI::Token::Quote::Double') and $pl_quote =~ /\\n"$/xsm) {
                    my $py_quote = $container->child(-1);
                    $py_quote->{content} =~ s/\\n"$/"/gsm;
                }
            }
            $statement->add_element( Python::Token::Whitespace->new("\n") );
            return $statement;
        }
        when (/PPI::Token::Whitespace/xsm) {
        }
    }
    if ( exists $pl_element->{children} ) {
        my $iter = next_non_whitespace($pl_element);
        while ( my $child = $iter->() ) {
            #$out .= map_element( $child, $nest_level );
        }
    }
    #return $out;
}

  sub nest_level {
      my ($element) = @_;
      my $level = 0;
      while (my $parent = $element->parent) {
          if ($element eq $parent) {
              return $level
          }
          if ($element->isa('PPI::Structure::Block')) {
              $level++
          }
          elsif ($element->isa('PPI::Document')) {
              return $level
          }
          $element = $parent
      }
      return $level
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
