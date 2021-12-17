package Perl2Python;

use warnings;
use strict;
use feature 'switch';
no if $] >= 5.018, warnings => 'experimental::smartmatch';
use PPI;
use Exporter ();
use base qw(Exporter);
use Carp;
use File::Spec;
use Readonly;
Readonly my $LAST          => -1;
Readonly my $INDENT_LENGTH => 4;
Readonly my $INDENT        => q{ } x $INDENT_LENGTH;

# symbols to export on request
our @EXPORT_OK = qw(map_directory map_document map_file map_path);

our $VERSION = 1;

our $DEBUG;

# https://perldoc.perl.org/perlop#Operator-Precedence-and-Associativity
my %PRECENDENCE = (
    q{or}  => 0,
    q{xor} => 0,
    q{and} => 1,
    q{not} => 2,
    q{,}   => 4,
    q{=}   => 5,
    q{+=}  => 5,
    q{-=}  => 5,
    q{*=}  => 5,
    q{/=}  => 5,
    q{..}  => 6,
    q{...} => 6,
    q{||}  => 7,
    q{//}  => 7,
    q{&&}  => 8,
    q{|}   => 9,
    q{|.}  => 9,
    q{^}   => 9,
    q{^.}  => 9,
    q{&}   => 10,
    q{&.}  => 10,
    q{==}  => 11,
    q{!=}  => 11,
    q{eq}  => 11,
    q{ne}  => 11,
    q{<=>} => 11,
    q{cmp} => 11,
    q{~~}  => 11,
    q{<}   => 12,
    q{>}   => 12,
    q{<=}  => 12,
    q{>=}  => 12,
    q{lt}  => 12,
    q{gt}  => 12,
    q{le}  => 12,
    q{ge}  => 12,
    q{<<}  => 15,
    q{>>}  => 15,
    q{+}   => 16,
    q{-}   => 16,
    q{.}   => 16,
    q{*}   => 17,
    q{/}   => 17,
    q{%}   => 17,
    q{x}   => 17,
    q{=~}  => 18,
    q{!~}  => 18,
    q{!}   => 19,
    q{~}   => 19,
    q{~.}  => 19,
    q{\\}  => 19,
    q{**}  => 20,
    q{++}  => 21,
    q{--}  => 21,
    q{->}  => 22,
);

sub map_built_in {
    my ($element) = @_;
    my $list = $element->snext_sibling;
    if ( not $list->isa('PPI::Structure::List') ) {
        $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
        $list->{finish} = PPI::Token::Structure->new(')');
        my @children;
        my $child = $element->next_sibling;
        while ( $child
            and not( $child->isa('PPI::Token::Structure') or $child eq 'or' ) )
        {
            push @children, $child;
            $child = $child->next_sibling;
        }
        for my $child (@children) {
            $list->add_element( $child->remove );
        }
        $element->insert_after($list);

        # deal with return value from built-in
        $child = $list->next_sibling;
        if ( $child eq 'or' ) {
            $child->delete;
            my $statement = $element->parent;
            my $try       = PPI::Statement::Compound->new;
            my $parent    = $statement->parent;
            $parent->__insert_before_child( $statement, $try );
            $try->add_element( PPI::Token::Word->new('try') );
            my $block =
              PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
            $block->{start}->{content} = q{:};
            $try->add_element($block);
            $block->add_element( PPI::Token::Whitespace->new("\n") );
            $block->add_element( $statement->remove );

            my $except = PPI::Statement::Compound->new;
            $parent->__insert_after_child( $try,
                PPI::Token::Whitespace->new("\n"), $except );
            $except->add_element( PPI::Token::Word->new('except') );
            $block =
              PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
            $block->{start}->{content} = q{:};
            $except->add_element($block);
            $block->add_element( PPI::Token::Whitespace->new("\n") );
            $statement = PPI::Statement->new;
            $block->add_element($statement);

            while ( my $rest = $list->next_sibling ) {
                $statement->add_element( $rest->remove );
            }

            # indent the added code explicitly
            indent_element($try);
            indent_element($except);
            indent_element($statement);
        }
    }
    return $list;
}

sub map_compound {
    my ($element) = @_;
    my $conditions = $element->find('PPI::Structure::Condition');
    if ($conditions) {
        for my $condition ( @{$conditions} ) {
            if ( not $condition or $condition->parent ne $element ) { next }
            my $expression =
              $condition->find_first('PPI::Statement::Expression');

            # variable assignments can only occur with for var in iterator
            my $assignment = $expression->find_first(
                sub {
                    $_[1]->isa('PPI::Token::Operator')
                      and $_[1]->content eq q{=};
                }
            );
            if ($assignment) {
                $assignment->{content} = 'in';
                my $word = $element->schild(0);
                if ( $word eq 'while' ) {
                    $word->{content} = 'for';
                }
            }
            $element->__insert_after_child( $condition, $expression->remove );
            $condition->delete;
        }
    }
    my $word = $element->schild(0);
    my $list = $element->find_first('PPI::Structure::List');
    if ( $word =~ /(?:while|for)/xsm and $list and $list->parent eq $element ) {
        for my $child ( $list->children ) {
            $element->__insert_after_child( $list, $child->remove );
        }
        $element->__insert_before_child(
            $list,
            PPI::Token::Operator->new('in'),
            PPI::Token::Whitespace->new(q{ })
        );
        $list->delete;
    }
    return;
}

sub map_directory {
    my ($dir) = @_;
    if ( -d $dir ) {
        for my $file ( glob( $dir . q{/*} ) ) {
            map_directory($file);
        }
    }
    else {
        map_file($dir);
    }
    return;
}

sub map_document {
    my ($string) = @_;
    my $doc = PPI::Document->new($string);
    map_element($doc);
    return $doc;
}

sub map_element {
    my ($element) = @_;
    logger($element);
    remove_trailing_semicolon($element);
    given ( ref $element ) {
        when (/PPI::Token::Comment/xsm) {
            if ( $element->line and $element->content =~ /^([#]!.*)perl/xsm ) {
                $element->{content} = $1 . "python3\n";
            }
        }
        when (/PPI::Statement::End/xsm) {
            while ( my $sibling = $element->next_sibling ) {
                $sibling->delete;
            }
            $element->delete;
        }
        when (/PPI::Statement::Include/xsm) {
            map_include($element);
        }
        when (/PPI::Statement::Package/xsm) {
            map_package($element);
        }
        when (/PPI::Statement::Scheduled/xsm) {
            my $block  = $element->find_first('PPI::Structure::Block');
            my $parent = $element->parent;
            for my $child ( $block->children ) {
                $parent->__insert_before_child( $element, $child->remove );
                map_element($child);
            }
            $element->delete
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
            $child->insert_after($list);
        }
        when (/PPI::Statement::Compound/xsm) {
            map_compound($element);
        }
        when (/PPI::Statement::Variable/xsm) {
            map_magic($element);
        }
        when (/PPI::Statement/xsm) {
            if ( not $element->isa('PPI::Statement::Expression')
                and $element eq '1' )
            {
                $element->delete;
                return;
            }
            map_magic($element);
        }
        when (/PPI::Structure::Block/xsm) {
            $element->{start}->{content}  = q{:};
            $element->{finish}->{content} = q{};
        }
        when (/PPI::Token::Operator/xsm) {
            map_operator($element);
        }

        # deal with $line = <$fh>
        when (/PPI::Token::QuoteLike::Readline/xsm) {
            if ( $element =~ /<\$(\w+)>/xsm ) {
                my $parent = $element->parent;

                # on a separate line, so map to "line = fh.readline()"
                if ( $parent->isa('PPI::Statement::Variable') ) {
                    $parent->__insert_before_child( $element,
                        PPI::Token::Symbol->new("$1.readline()") );
                    $element->delete;
                }

                # we are in a loop, so map to "for line in fh":
                else {
                    $element->{content} = $1;
                }
            }
            else {
                croak "Error parsing '$element\n";
            }
        }
        when (/PPI::Token::QuoteLike::Words/xsm) {
            my $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('[') );
            $list->{finish} = PPI::Token::Structure->new(']');
            my $string = substr $element->content,
              $element->{sections}[0]{position}, $element->{sections}[0]{size};
            for ( split q{ }, $string ) {
                if ( $list->children ) {
                    $list->add_element( PPI::Token::Operator->new(q{,}) );
                }
                $list->add_element(
                    PPI::Token::Quote::Double->new( q{"} . $_ . q{"} ) );
            }
            $element->insert_after($list);
            $element->delete;
        }
        when (/PPI::Token::Regexp::Match/xsm) {
            map_regex_match($element)
        }
        when (/PPI::Token::Symbol/xsm) {
            map_symbol($element);
        }
        when (/PPI::Token::Word/xsm) {
            map_word($element);
        }
    }
    if ( exists $element->{children} ) {
        for my $child ( $element->children ) {
            map_element($child);
        }
    }
    indent_element($element);
    return;
}

sub map_file {
    my ($file) = @_;
    my $doc = PPI::Document::File->new($file);
    print "Reading from $file\n" or croak 'Error printing to STDOUT';
    map_element($doc);
    my $outfile = map_path($file);
    print "Writing to $outfile\n" or croak 'Error printing to STDOUT';
    open my $fh, '>', $outfile or croak "Error opening $outfile";
    print {$fh} $doc or croak "Error writing to $outfile";
    close $fh or croak "Error closing $outfile";
    return;
}

sub map_include {
    my ($element) = @_;
    my $module = $element->module;
    if ( $module =~
        /^(warnings|strict|feature|if|Readonly|IPC::System::Simple)$/xsm )
    {
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
    elsif ( $module eq 'Test::More' ) {
        my $def = PPI::Statement::Sub->new;
        $def->add_element( PPI::Token::Word->new('def') );
        $def->add_element( PPI::Token::Whitespace->new(q{ }) );
        $def->add_element( PPI::Token::Word->new('test_1') );
        my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
        $list->{finish} = PPI::Token::Structure->new(')');
        $def->add_element($list);
        $element->insert_after($def);
        $element->delete;

        # Add a block to scope and indent things properly
        my $block =
          PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
        $block->{start}->{content} = q{:};
        $def->add_element($block);
        while ( my $next = $def->next_sibling ) {
            $block->add_element( $next->remove );
        }
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
    return;
}

sub map_magic {
    my ($element) = @_;

    # magic defined in sub - add to def arguments
    my $magic = $element->find_first('PPI::Token::Magic');
    if ( $magic eq '@_' ) {    ## no critic (RequireInterpolationOfMetachars)
        my $source_list = $element->find_first('PPI::Structure::List');
        my $sub         = $element->parent->parent;
        if ( not $sub ) {
            return;
        }
        my $dest_list = $sub->find_first('PPI::Structure::List');
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
        my $block = $element->parent;
        while ( not $block->isa('PPI::Structure::Block') ) {
            $block = $block->parent;
        }
        my $compound  = $block->parent;
        my $parent    = $compound->parent;
        my $condition = $block->sprevious_sibling;
        my $search    = $condition->find_first(
            sub {
                $_[1]->isa('PPI::Token::Word')
                  and $_[1]->content eq 're.search';
            }
        );
        if ($search) {
            my $list      = $search->snext_sibling;
            my $regex_var = PPI::Statement::Variable->new;
            $regex_var->add_element( PPI::Token::Symbol->new('regex') );
            $regex_var->add_element( PPI::Token::Operator->new(q{=}) );
            $compound->insert_before($regex_var);
            $search->insert_before( PPI::Token::Symbol->new('regex') );
            $regex_var->add_element( $search->remove );
            $regex_var->add_element( $list->remove );
            $compound->insert_before( PPI::Token::Whitespace->new("\n") );
            indent_element($regex_var);
        }

        # replace the magic with the regex group
        $magic->insert_before( PPI::Token::Word->new("regex.group($1)") );
        $magic->delete;
    }

    # deal with implied @_ in shift
    if (
        $element->find_first(
            sub {
                $_[1]->isa('PPI::Token::Word')
                  and $_[1]->content eq 'shift'
                  and not $_[1]->snext_sibling;
            }
        )
      )
    {
        my $source = $element->find_first('PPI::Token::Symbol');
        my $dest_list =
          $element->parent->parent->find_first('PPI::Structure::List');
        if ( $dest_list->children ) {
            $dest_list->add_element( PPI::Token::Operator->new(q{,}) );
        }
        $dest_list->add_element( $source->remove );
        $element->delete;
        map_element($dest_list);
    }
    return;
}

sub map_operator {
    my ($element) = @_;
    $element->{originally} = $element->{content};
    given ("$element") {
        when (q{?}) {
            my $conditional = $element->sprevious_sibling;
            my $true        = $element->snext_sibling;
            my $operator    = $true->snext_sibling;
            my $false       = $operator->snext_sibling;
            if ( $operator ne q{:} or not( $true and $false ) ) {
                croak
"Error parsing conditional operator: '$conditional $element $true $operator $false'\n";
            }
            $element->{content}  = 'if';
            $operator->{content} = 'else';
            my $parent = $element->parent;
            $parent->__insert_after_child( $element,
                PPI::Token::Whitespace->new(q{ }),
                $conditional->remove );
            $parent->__insert_before_child( $element, $true->remove,
                PPI::Token::Whitespace->new(q{ }) );
        }
        when (q{.=}) {
            $element->{content} = q{+=};
        }
        when (q{->}) {
            $element->{content} = q{.};
        }
        when (q{-s}) {
            add_import( $element, 'os' );
            my $parent = $element->parent;
            my $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $list->{finish} = PPI::Token::Structure->new(')');
            $list->add_element( $element->snext_sibling->remove );
            $parent->__insert_after_child( $element,
                PPI::Token::Word->new('os.path.getsize'), $list );
            $element->delete;
        }
        when (q{!}) {
            $element->{content} = q{not};
        }
    }
    return;
}

sub map_package {
    my ($element) = @_;
    my $package = $element->schild(0);
    $package->{content} = 'class';
    my $name = $element->schild(1);
    $name->{content} =~ s/.*:://xsm;

    # convert to a sub, which is similar and has a block
    my $class = PPI::Statement::Sub->new;
    for my $child ( $element->children ) {
        $class->add_element( $child->remove );
    }
    $element->insert_after($class);
    $element->delete;
    my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
    $list->{finish} = PPI::Token::Structure->new(')');
    $name->insert_after($list);

    # Add a block to scope and indent things properly
    my $block = PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
    $block->{start}->{content} = q{:};
    $class->add_element($block);
    my $next;
    while ( ( $next = $class->next_sibling )
        and not $next->isa('PPI::Statement::Package') )
    {
        $block->add_element( $next->remove );
    }
    return;
}

#https://docs.python-guide.org/writing/structure/ i.e.:
#  bin -> bin
#  lib/package -> package
#  t -> tests, then ensure the contents start with test_
#  Makefile.PL -> setup.py + Makefile
sub map_path {
    my ($path) = @_;
    my ( $volume, $directories, $file ) = File::Spec->splitpath($path);
    my @dirs    = File::Spec->splitdir($directories);
    my $outfile = "$file.py";
    if ( $file =~ /(.+?)(:?[.]t)$/xsm ) {
        my $pref = $1;
        if ( $pref =~ /^test/xsm ) {
            $outfile = "$pref.py";
        }
        else {
            $outfile = "test_$pref.py";
        }
    }
    elsif ( $file =~ /(.+?)(:?[.]p[lm])$/xsm ) {
        $outfile = "$1.py";
    }
    my @outdirs;
    for (@dirs) {
        if ( $_ eq 't' ) {
            push @outdirs, 'tests';
        }
        elsif ( $_ ne 'lib' ) {
            push @outdirs, $_;
        }
    }
    return File::Spec->catpath( $volume, File::Spec->catdir(@outdirs),
        $outfile );
}

sub map_regex_match {
    my ($element) = @_;

    # in perl, the parent is a PPI::Statement::Expression, which we now
    # turn into re.search(regex, string)
    my $expression = $element->parent;
    if ( not $expression ) { return }
    $expression->add_element( PPI::Token::Word->new('re.search') );
    my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
    $list->{finish} = PPI::Token::Structure->new(')');
    $expression->add_element($list);
    my $operator = $element->sprevious_sibling;

    if ( not $operator or not $operator->isa('PPI::Token::Operator') ) {
        croak "Expected operator before regex match. Found '$operator'\n";
    }
    my @argument = get_argument_for_operator( $operator, 0 );
    if ( not @argument or not $argument[0] ) {
        croak "Argument for operator '$operator' not found\n";
    }
    $list->add_element( $element->remove );

    if ( $operator eq q{=~} ) {
    }
    elsif ( $operator eq q{!~} ) {
        $element->insert_before( PPI::Token::Word->new('not'),
            PPI::Token::Whitespace->new(q{ }) );
    }
    else {
        croak "Unknown operator '$operator'\n";
    }
    $operator->delete;
    $list->add_element( PPI::Token::Operator->new(q{,}) );
    for my $argument (@argument) {
        $list->add_element( $argument->remove );
    }

    # remove the flags convert the separator to Python string terminators
    my $separator = $element->{separator};
    $element->{content} =~ s{^$separator}{r'}xsm;
    $element->{content} =~ s{$separator(?:[xsmg]+)?$}{'}xsm;

    # ensure we have import re
    add_import( $element, 're' );
    return;
}

sub map_symbol {
    my ($element) = @_;
    if (
        $element eq '$SIG'    ## no critic (RequireInterpolationOfMetachars)
        and $element->snext_sibling eq '{__WARN__}'
      )
    {
        add_import( $element, 'logging' );
        my $statement = $element->parent;
        for my $child ( $statement->children ) {
            $child->delete;
        }
        $statement->add_element(
            PPI::Token::Word->new('logging.captureWarnings(True)') );

    }
    else {
        $element->{content} =~ s/^[\$@%]//smx;
    }
    return;
}

sub map_word {
    my ($element) = @_;
    $element->{content} =~ s/::/./gsm;
    given ("$element") {
        when ('Readonly') {
            my $operator = $element->parent->find_first('PPI::Token::Operator');
            if ( $operator ne '=>' ) {
                croak "Unexpected operator '$operator'\n";
            }
            $operator->{content} = q{=};
            $element->delete;
        }
        when ('close') {
            my $list = map_built_in($element);
            my $fh   = $list->schild(0);
            map_element($fh);
            if ( $fh->isa('PPI::Statement::Expression') ) {
                $fh = $fh->schild(0);
            }
            $fh->{content} .= q{.};
            $element->insert_before( $fh->remove );
        }
        when ('defined') {
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
        when ('elsif') {
            $element->{content} = 'elif';
        }
        when ('grep') {
            my $expression = $element->snext_sibling;
            my $array      = $expression->snext_sibling;
            while ( $expression->isa('PPI::Structure::Block') ) {
                $expression = $expression->schild(0);
            }
            my $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('[') );
            $list->{finish} = PPI::Token::Structure->new(']');
            $element->insert_before($list);
            $list->add_element( PPI::Token::Symbol->new('x') );
            $list->add_element( PPI::Token::Whitespace->new(q{ }) );
            $list->add_element( PPI::Token::Word->new('for') );
            $list->add_element( PPI::Token::Whitespace->new(q{ }) );
            $list->add_element( PPI::Token::Symbol->new('x') );
            $list->add_element( PPI::Token::Whitespace->new(q{ }) );
            $list->add_element( PPI::Token::Operator->new('in') );
            $list->add_element( PPI::Token::Whitespace->new(q{ }) );
            $list->add_element( $array->remove );
            $list->add_element( PPI::Token::Whitespace->new(q{ }) );
            $list->add_element( PPI::Token::Operator->new('if') );
            $list->add_element( PPI::Token::Whitespace->new(q{ }) );
            my $operator = $expression->find_first('PPI::Token::Operator');

            if ($operator) {
                $list->add_element( $operator->remove );
                $list->add_element( PPI::Token::Whitespace->new(q{ }) );
                map_operator($operator);
            }
            my $regex = $expression->find_first('PPI::Token::Regexp::Match');
            if ($regex) {
                add_import( $element, 're' );
                $list->add_element( PPI::Token::Word->new('re.search') );
                my $list2 =
                  PPI::Structure::List->new( PPI::Token::Structure->new('(') );
                $list2->{finish} = PPI::Token::Structure->new(')');
                $list->add_element($list2);
                $list2->add_element(
                    PPI::Token::Quote::Double->new(
                        q{r"}
                          . substr(
                            $regex->content,
                            $regex->{sections}[0]{position},
                            $regex->{sections}[0]{size}
                          )
                          . q{"}
                    )
                );
                $list2->add_element( PPI::Token::Operator->new(q{,}) );
                $list2->add_element( PPI::Token::Symbol->new('x') );
            }
            while ($element) {
                my $next = $element->next_sibling;
                $element->delete;
                $element = $next;
            }
        }
        when ('is') {

            # ignore 'is' created by previously mapping defined
            if ( $element->snext_sibling =~ /(None|not)/xsm ) {
                return;
            }

            # we've got a unit test - map to assert
            $element->{content} = 'assert';
            my $statement = $element->parent;
            my $operators = $statement->find(
                sub {
                    $_[1]->isa('PPI::Token::Operator')
                      and $_[1]->content eq q{,};
                }
            );
            $operators->[0]->{content} = q{==};
            my $comment = PPI::Token::Comment->new(' # ');
            $statement->__insert_after_child( $operators->[1], $comment );
            $operators->[1]->delete;
        }
        when ('length') {
            my $list = map_built_in($element);
            $element->{content} = 'len';
        }
        when (/^(?:my|our)$/xsm) {
            $element->delete;
        }
        when ('open') {
            my $list     = map_built_in($element);
            my $fh       = $list->find_first('PPI::Token::Symbol');
            my $operator = $fh->snext_sibling;
            if ( not $operator->isa('PPI::Token::Operator') ) {
                croak "Expected operator, found '$operator'\n";
            }
            $operator->{content} = q{=};
            $element->insert_before( $fh->remove );
            $element->insert_before( $operator->remove );
            my $mode = $list->find_first('PPI::Token::Quote');
            if ( $mode eq "$mode->{separator}<$mode->{separator}" ) {
                $mode->{content} = "mode=$mode->{separator}r$mode->{separator}";
                $operator = $mode->snext_sibling;
                my $fname = $operator->snext_sibling;
                $fname->insert_after( $operator->remove );
                $operator->insert_after( $mode->remove );
            }
        }
        when ('print') {
            my $list  = map_built_in($element);
            my $quote = $list->schild($LAST);
            if ( $quote->isa('PPI::Token::Quote::Double') ) {
                $quote->{content} =~ s/\\n"$/"/gsmx;
            }
        }
        when ('shift') {
            my $parent = $element->parent;
            $element->{content} = '.pop(0)';
            $element->snext_sibling->insert_after( $element->remove );
        }
        when ('split') {
            my $list = map_built_in($element);
            my $sep  = $list->schild(0);
            my $op   = $sep->snext_sibling;
            my $str  = $op->snext_sibling;
            if ($sep) {
                map_element($str);
                if ( $sep->isa('PPI::Token::Quote') ) {
                    $element->{content} = "$str.$element->{content}";
                    $op->delete;
                    $str->delete;
                }
                elsif ( $sep->isa('PPI::Token::Regexp::Match') ) {
                    add_import( $element, 're' );
                    $element->{content} = "re.$element->{content}";
                    $sep->insert_after(
                        PPI::Token::Quote::Double->new(
                            q{r"}
                              . substr(
                                $sep->content,
                                $sep->{sections}[0]{position},
                                $sep->{sections}[0]{size}
                              )
                              . q{"}
                        )
                    );
                    $sep->delete;
                }
            }
        }
        when ('system') {
            add_import( $element, 'subprocess' );
            $element->{content} = 'subprocess.run';
        }
        when ('unlink') {
            add_import( $element, 'os' );
            $element->{content} = 'os.remove';
            my $list = map_built_in($element);
        }
        when ('use_ok') {
            $element->{content} = 'import';
            my $parent = $element->parent;
            my $list   = $parent->find_first('PPI::Structure::List');
            if ($list) {
                $parent->__insert_before_child( $list,
                    PPI::Token::Whitespace->new(q{ }) );
                for my $child ( $list->children ) {
                    $parent->__insert_before_child( $list, $child->remove );
                }
                $list->delete;
            }
            my $quote = $parent->find_first('PPI::Token::Quote');
            if ($quote) {
                my $separator = $quote->{separator};
                $quote->{content} =~ s{^$separator}{}xsm;
                $quote->{content} =~ s{$separator$}{}xsm;
                $quote->{content} =~ s/::/./gsm;
            }
        }
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
    if ( $parent->isa('PPI::Token') ) { return }
    my $child = $parent->schild($LAST);
    if ( $child and $child eq q{;} ) {
        $child->delete;
    }
    return;
}

# Having mapped the code structure, clean up the whitespace enough so that
# Python can parse it.
sub indent_element {
    my ($element) = @_;
    if (
        (
            $element->isa('PPI::Statement')
            and not $element->isa('PPI::Statement::Expression')
        )
        or $element->isa('PPI::Statement::Variable')
      )
    {

        # trim leading whitespace inside statement
        my $child = $element->child(0);
        while ( defined $child and $child->isa('PPI::Token::Whitespace') ) {
            $child->delete;
            $child = $element->child(0);
        }

        # trim newlines inside statement
        for my $child ( $element->children ) {
            if ( $child->isa('PPI::Token::Whitespace') and $child =~ /\n/xsm ) {
                $child->{content} = q{ };
            }
        }

        # fixup whitespace before statement
        indent_subelement($element);

        # and inside compounds statements
        if ( $element->isa('PPI::Statement::Compound') ) {
            my $substatements = $element->find(
                sub {
                    $_[1]->isa('PPI::Token::Word')
                      and $_[1]->content =~ /(?:elif|else)/xsm;
                }
            );
            if ($substatements) {
                for my $child ( @{$substatements} ) {
                    indent_subelement($child);
                }
            }
        }
    }
    return;
}

sub indent_subelement {
    my ($element)           = @_;
    my $nest_level          = nest_level($element);
    my $required_whitespace = $INDENT x $nest_level;
    my $whitespace          = $element->previous_sibling;
    if ( $nest_level > 0 ) {
        if (   not $whitespace
            or not $whitespace->isa('PPI::Token::Whitespace')
            or $whitespace eq "\n" )
        {
            my $indent = PPI::Token::Whitespace->new($required_whitespace);
            $element->insert_before($indent);
            if ( $whitespace ne "\n" ) {
                $indent->insert_before( PPI::Token::Whitespace->new("\n") );
            }
        }
        elsif ( $whitespace->isa('PPI::Token::Whitespace') ) {
            $whitespace->{content} = $required_whitespace;
            my $newline = $whitespace->previous_sibling;
            if ($newline) {
                if ( $newline ne "\n" ) {
                    $newline->insert_after( PPI::Token::Whitespace->new("\n") );
                }
            }
            else {
                $whitespace->insert_before( PPI::Token::Whitespace->new("\n") );
            }
        }
    }
    else {
        if (    $whitespace
            and $whitespace->isa('PPI::Token::Whitespace')
            and $whitespace ne "\n" )
        {
            $whitespace->delete;
            $whitespace = $element->previous_sibling;
            if ( $whitespace ne "\n" ) {
                $element->insert_before( PPI::Token::Whitespace->new("\n") );
            }
        }
    }
    return;
}

sub add_import {
    my ( $element, $module ) = @_;
    my $document = $element->top;
    if (
        not $document->find_first(
            sub {
                $_[1]->isa('PPI::Statement::Include')
                  and $_[1]->content eq "import $module";
            }
        )
      )
    {
        my $statement = PPI::Statement::Include->new;
        $statement->add_element( PPI::Token::Word->new('import') );
        $statement->add_element( PPI::Token::Whitespace->new(q{ }) );
        $statement->add_element( PPI::Token::Word->new($module) );
        $document->schild(0)->insert_before($statement);
        $statement->insert_after( PPI::Token::Whitespace->new("\n") );
    }
    return;
}

sub logger {
    my ($element) = @_;
    if ($DEBUG) {
        my $message = ref $element;
        if ( defined $element->{content} ) {
            $message .= ': ' . $element->{content};
            $message =~ s/\n.*//xsm;
        }
        print {*STDERR} "$message\n" or croak;
    }
    return;
}

sub get_argument_for_operator {
    my ( $element, $n ) = @_;
    my @sibling;
    my $next = $n == 0 ? $element->sprevious_sibling : $element->snext_sibling;
    while (
        $next
        and ( not $next->isa('PPI::Token::Operator')
            or has_higher_precedence_than( $element, $next ) )
      )
    {
        if ( $n == 0 ) {
            unshift @sibling, $next;
        }
        else {
            push @sibling, $next;
        }
        $next = $n == 0 ? $next->sprevious_sibling : $next->snext_sibling;
    }
    return @sibling;
}

sub has_higher_precedence_than {
    my ( $op1, $op2 ) = @_;
    return $PRECENDENCE{ $op1->{originally} } <
      $PRECENDENCE{ $op2->{originally} };
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

=head2 map_document

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
