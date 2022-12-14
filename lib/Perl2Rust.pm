package Perl2Rust;

use warnings;
use strict;
use feature 'switch';
no if $] >= 5.018, warnings => 'experimental::smartmatch';
use PPI;
use Exporter ();
use base qw(Exporter);
use Carp;
use File::Spec;
use IPC::Open3 'open3';
use Try::Tiny;
use Readonly;
Readonly my $LAST          => -1;
Readonly my $INDENT_LENGTH => 4;
Readonly my $INDENT        => q{ } x $INDENT_LENGTH;

# symbols to export on request
our @EXPORT_OK = qw(map_directory map_document map_file map_path);

our $VERSION = 1;
our $LINENUMBER;
our $DEBUG;
my $REGEX;

# https://perldoc.perl.org/perlop#Operator-Precedence-and-Associativity
my @PRECEDENCE = (
    [ 'left',     qw{or xor} ],
    [ 'left',     qw{and} ],
    [ 'right',    qw{not} ],
    [ 'nonassoc', 'list operator (rightward)' ],
    [ 'left',     q{,}, q{=>} ],
    [ 'right',    qw{= += -= *= /=} ],
    [ 'right',    qw{? :} ],
    [ 'nonassoc', qw{.. ...} ],
    [ 'left',     qw{|| //} ],
    [ 'left',     qw{&&} ],
    [ 'left',     qw{| |. ^ ^.} ],
    [ 'left',     qw{& &.} ],
    [ 'chain/na', qw{== != eq ne <=> cmp ~~} ],
    [ 'chained',  qw{< > <= >= lt gt le ge} ],
    [
        'nonassoc',
        qw{-r -w -x -o -R -W -X -O -e -z -s -f -d -l -p -S -b -c -t -u -g -k -T -B -M -A -C}
    ],
    [ 'left',     qw{<< >>} ],
    [ 'left',     qw{+ - .} ],
    [ 'left',     qw{* / % x} ],
    [ 'left',     qw{=~ !~} ],
    [ 'right',    qw{! ~ ~. \\} ],
    [ 'right',    qw{**} ],
    [ 'nonassoc', qw{++ --} ],
    [ 'left',     qw{->} ],
    [ 'left', 'term', 'list operator (leftward)' ],
);
my %PRECEDENCE    = ();
my %ASSOCIATIVITY = ();
for my $i ( 0 .. $#PRECEDENCE ) {
    my $ass = shift @{ $PRECEDENCE[$i] };
    for my $op ( @{ $PRECEDENCE[$i] } ) {
        $PRECEDENCE{$op}    = $i;
        $ASSOCIATIVITY{$op} = $ass;
    }
}

# https://perldoc.perl.org/functions
my @BUILTINS = qw(defined eval);
my ( %BUILTINS, %LIST_OPERATORS );
for my $op (@BUILTINS) {
    $BUILTINS{$op} = 1;
}
my @LIST_OPERATORS =
  qw(chmod chown close grep join length map next pack open print printf push return reverse say sort split sprintf unlink unshift);
for my $op (@LIST_OPERATORS) {
    $BUILTINS{$op}       = 1;
    $LIST_OPERATORS{$op} = 1;
}

my $IGNORED_INCLUDES =
q/^(?:warnings|strict|feature|if|Carp|English|Exporter|File::Copy|IPC::System::Simple|POSIX|Proc::Killfam|Readonly|Try::Tiny)$/;

my @RESERVED_WORDS = qw(class def print break);

sub check_operator {
    my ( $element, $direction ) = @_;
    if ( defined $element->{originally} ) {
        $element = $element->{originally};
    }
    if ( not defined $PRECEDENCE{$element} ) {
        if ( defined $LIST_OPERATORS{$element} ) {
            if ( $direction > 0 ) {
                return 'list operator (rightward)';
            }
            return 'list operator (leftward)';
        }
        else {
            return 'term';
        }
    }
    return $element;
}

sub element_should_be_indented {
    my ($element) = @_;
    my $parent = $element->parent;
    return (
        (
            $element->isa('PPI::Statement')
              and not $element->isa('PPI::Statement::Expression')
        )
          or (  $element->isa('PPI::Statement::Variable')
            and $parent
            and not $parent->isa('PPI::Structure::List') )
    );
}
sub get_argument_for_operator {
    my ( $element, $n ) = @_;

    if ( $element eq q{?} ) {
        return get_argument_for_ternary( $element, $n );
    }
    my @sibling;
    my $iter = $element;
    while ( $iter = next_sibling( $iter, $n ) and $iter ne q{;} ) {
        if ( $n > 0 ) {
            if (    not @sibling
                and $iter->isa('PPI::Structure::List')
                and defined $BUILTINS{$element} )
            {
                return get_argument_from_list( $iter, $n );
            }

            # most built-ins have to have an argument, so grab one
            my $next = next_sibling( $iter, $n );
            if (    defined $BUILTINS{$iter}
                and $next
                and not $next->isa('PPI::Structure::List') )
            {
                push @sibling, $iter, get_argument_for_operator( $iter, $n );
                $iter = pop @sibling;
            }
        }
        if (
            not( has_higher_precedence_than( $element, $iter, $n )
                or ( @sibling == 0 and has_rh_associativity($iter) ) )
          )
        {

            # ensure we have at least 1 argument
            if ( @sibling == 0 or has_rh_associativity( $sibling[-1] ) ) {
                push @sibling, $iter;
                while ( ( $iter = next_sibling( $iter, $n ) )
                    and has_rh_associativity2( $iter, $sibling[-1] ) )
                {
                    push @sibling, $iter;
                }
            }
            last;
        }

        if ( $n == 0 ) {
            unshift @sibling, $iter;
        }
        else {
            push @sibling, $iter;
        }
    }
    return @sibling;
}

sub get_argument_for_ternary {
    my ( $element, $n ) = @_;
    my @sibling;
    my $iter = $element;
    while ( $iter = next_sibling( $iter, $n ) ) {
        if ( $iter eq q{?} ) {
            push @sibling, get_argument_for_operator( $iter, 1 );
            $iter = $sibling[-1];
        }
        else {
            if ( $n == 0 ) {
                if ( not has_higher_precedence_than( $element, $iter, $n ) ) {
                    last;
                }
                push @sibling, $iter;
            }
            else {
                if ( $iter eq q{:} ) { last }
                push @sibling, $iter;
            }
        }
    }
    return @sibling;
}

sub get_argument_from_list {
    my ( $element, $n ) = @_;
    my @sibling;
    $element = $element->schild(0);
    if ( $element->isa('PPI::Statement::Expression') ) {
        $element = $element->schild(0);
    }
    while ( $element and $element ne q{,} ) {
        push @sibling, $element;
        $element = $element->snext_sibling;
    }
    return @sibling;
}

sub has_rh_associativity {
    my ($iter) = @_;
    return (
        $iter->isa('PPI::Token::Cast')
          or
          ( defined $ASSOCIATIVITY{$iter} and $ASSOCIATIVITY{$iter} eq 'right' )
    );
}

sub has_rh_associativity2 {
    my ( $iter, $prev ) = @_;
    return (
        $iter
          and (
               $iter->isa('PPI::Structure::Subscript')
            or $iter eq '->'
            or ( defined $iter->{originally} and $iter->{originally} eq '->' )
            or $prev eq '->'
            or ( defined $prev->{originally} and $prev->{originally} eq '->' )
            or (    $prev->isa('PPI::Token::Word')
                and $iter->isa('PPI::Structure::List') )
          )
    );
}

sub has_higher_precedence_than {
    my ( $op1, $op2, $direction ) = @_;

    # don't swallow built-ins looking left for arguments
    if ( $direction == 0 and defined $BUILTINS{$op2} ) { return }

    my $res = check_operator( $op1, $direction );
    if (    not defined $BUILTINS{$op1}
        and $op1->isa('PPI::Token::Word')
        and $res eq 'term' )
    {
        if ( $direction > 0 ) {
            $res = 'list operator (rightward)';
        }
        else {
            $res = 'list operator (leftward)';
        }
    }
    $op1 = $res;
    $op2 = check_operator( $op2, $direction );
    return $PRECEDENCE{$op1} < $PRECEDENCE{$op2};
}

# Having mapped the code structure, clean up the whitespace enough so that
# Python can parse it.
sub indent_element {
    my ($element) = @_;
    if ( not $element or not defined $element or ref($element) eq 'HASH' ) {
        return;
    }
    if ( element_should_be_indented($element) ) {

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

        # and inside compound statements
        my $substatements = $element->find(
            sub {
                $_[1]->isa('PPI::Token::Word')
                  and $_[1]->content =~ /(?:elif|else|except)/xsm;
            }
        );
        if ($substatements) {
            for my $child ( @{$substatements} ) {
                indent_subelement($child);
            }
        }
    }

    # remove trailing whitespace in blocks
    elsif ( $element->isa('PPI::Structure::Block') ) {
        my $child = $element->child($LAST);
        if ( not $child ) { return }
        if ( $child->isa('PPI::Token::Whitespace') and $child =~ /[ ]+/xsm ) {
            $child->delete;
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
        if ($whitespace) {
            if (    $whitespace->isa('PPI::Token::Whitespace')
                and $whitespace ne "\n" )
            {
                $whitespace->delete;
                $whitespace = $element->previous_sibling;
            }
            if ( $whitespace ne "\n" ) {
                $element->insert_before( PPI::Token::Whitespace->new("\n") );
            }
        }
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
    $LINENUMBER = $element->line_number;
    return;
}

sub map_built_in {
    my ( $element, @args ) = @_;
    my $statement = $element->parent;
    if ( not $statement ) { return }
    my $list = $element->snext_sibling;
    if ( not $list or not $list->isa('PPI::Structure::List') ) {
        $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
        $list->{finish} = PPI::Token::Structure->new(')');

        if ( not @args ) {
            @args = get_argument_for_operator( $element, 1 );
        }
        for my $child (@args) {
            $list->add_element( $child->remove );
        }
        $element->insert_after($list);
    }

    # deal with return value from built-in
    my $child = $list->snext_sibling;
    if ( $child eq 'or' ) {
        my $try    = PPI::Statement::Compound->new;
        my $parent = $statement->parent;
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
        $block = PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
        $block->{start}->{content} = q{:};
        $except->add_element($block);
        $block->add_element( PPI::Token::Whitespace->new("\n") );
        $statement = PPI::Statement->new;
        $block->add_element($statement);

        while ( my $rest = $child->next_sibling ) {
            $statement->add_element( $rest->remove );
        }
        $child->delete;

        # indent the added code explicitly
        indent_element($try);
        indent_element($except);
        indent_element($statement);
    }
    return $list;
}

sub map_document {
    my ($string) = @_;
    my $doc = PPI::Document->new($string);
    map_element($doc);
    return $doc;
}

sub map_element {
    my ($element) = @_;
    if ( defined $element->{mapped} ) { return }
    $element->{mapped} = 1;
    logger($element);
    my $map_children = 1;
    given ( ref $element ) {
        when (/PPI::Token::Comment/xsm) {
            if ( $element->line and $element->content =~ /^([#]!.*)perl/xsm ) {
                $element->delete;
            }
        }
        when (/PPI::Statement::End/xsm) {
            while ( my $sibling = $element->next_sibling ) {
                $sibling->delete;
            }
            $element->delete;
        }
        when (/PPI::Statement::Expression/xsm) {
            map_expression($element);
        }
        when (/PPI::Statement::Given/xsm) {
            map_given($element);
        }
        when (/PPI::Statement::Include/xsm) {
            map_include($element);
            $map_children = 0;
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
            map_sub($element);
        }
        when (/PPI::Statement::Compound/xsm) {
            map_compound($element);
        }
        when (/PPI::Statement::Variable/xsm) {
            map_variable($element);
        }
        when (/PPI::Statement/xsm) {
            if (
                $element =~ /Log::Log4perl->easy_init/xsm
                or ( not $element->isa('PPI::Statement::Expression')
                    and $element eq '1' )
              )
            {
                $element->delete;
                return;
            }
        }
        when (/PPI::Structure::Block/xsm) {
            $element->{start}->{content}  = q{:};
            $element->{finish}->{content} = q{};
        }
        when (/PPI::Structure::Subscript/xsm) {
            map_subscript($element);
        }

        # $# -> len()
        when (/PPI::Token::ArrayIndex/xsm) {
            $element->insert_before( PPI::Token::Word->new('len') );
            my $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $list->{finish} = PPI::Token::Structure->new(')');
            $element->insert_before($list);
            $list->add_element(
                PPI::Token::Word->new(
                    substr $element->{content},
                    2,
                    length $element->{content}
                )
            );
            $list->insert_after( PPI::Token::Number->new(1) );
            $list->insert_after( PPI::Token::Operator->new(q{-}) );
            $element->delete;
        }
        when (/PPI::Token::Cast/xsm) {
            map_cast($element);
        }
        when (/PPI::Token::Operator/xsm) {
            map_operator($element);
        }
        when (/PPI::Token::HereDoc/xsm) {
            $element->insert_before(
                PPI::Token::Quote::Double->new(
                    '"""' . join( q{}, @{ $element->{_heredoc} } ) . '"""'
                )
            );
            $element->delete;
        }
        when (/PPI::Token::Quote::Double/xsm) {
            map_interpreted_string($element);
        }
        when (/PPI::Token::Quote::Literal/xsm) {
            my $content = substr
              $element->content,
              $element->{sections}[0]{position},
              $element->{sections}[0]{size};
            $content =~ s/"/\\"/xsm;
            $element->insert_after(
                PPI::Token::Quote::Double->new( q{"} . $content . q{"} ) );
            $element->delete;
        }
        when (/PPI::Token::QuoteLike::Readline/xsm) {
            map_readline($element);
        }
        when (/PPI::Token::QuoteLike::Regexp/xsm) {
            if ( $element->sprevious_sibling eq q{=~} ) {
                map_regex_match($element);
            }
            else {
                my $content = substr
                  $element->content,
                  $element->{sections}[0]{position},
                  $element->{sections}[0]{size};
                $content =~ s/"/\\"/xsm;
                $element->insert_after(
                    PPI::Token::Quote::Double->new( q{r"} . $content . q{"} ) );
                $element->delete;
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
        when (/PPI::Token::Regexp::Substitute/xsm) {
            map_regex_substitute($element);
        }
        when (/PPI::Token::Symbol/xsm) {
            map_symbol($element);
        }
        when (/PPI::Token::Word/xsm) {
            map_word($element);
        }
        when (/PPI::Token::Magic/xsm) {
            $element = map_magic($element);
        }
        when (/PPI::Token::Number/xsm) {
            if ( $element->{content} =~ /^0+(\d+)$/xsm ) {
                $element->{content} = $1;
            }
        }
    }
    if ( exists $element->{children} and $map_children ) {
        for my $child ( $element->children ) {
#            try {
                map_element($child);
#            }
#            catch {
#                my $mess =
#                  "Error mapping: '$child' at line number $LINENUMBER\n";
#                if ($DEBUG) {
#                    croak $mess;
#                }
#                else {
#                    warn $mess;
#                }
#            };
        }
    }
    indent_element($element);
    return $element;
}

sub map_include {
    my ($element) = @_;
    my $module    = $element->module;
    my $import    = $element->schild(0);
    my $top       = $element->top;
    given ($module) {

        # empty string matches cases like "use 5.008005;"
        when (/(?:$IGNORED_INCLUDES|^$)/xsm) {
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
        when ('Glib::Object::Subclass') {
            map_gobject_subclass( $element, $import );
            return;
        }
        when ('Test::More') {
            my $def = PPI::Statement::Sub->new;
            $def->add_element( PPI::Token::Word->new("\ndef") );
            $def->add_element( PPI::Token::Whitespace->new(q{ }) );
            $def->add_element( PPI::Token::Word->new('test_1') );
            my $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
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
        default {
            ensure_include_is_top_level( $element, $top );
        }
    }
    $module =~ s/::/./gsm;

    if ( $import ne 'use' ) {
        croak "Unrecognised include $element\n";
    }
    my $path    = $import->snext_sibling;
    my $symbols = $path->snext_sibling;
    given ("$path") {
        when ('Data::UUID') {
            $module = 'uuid';
        }
        when ('Date::Calc') {
            $module = 'datetime';
            $symbols->delete;
            undef $symbols;
        }
        when ('Encode') {
            $element->delete;
        }
        when ('File::Temp') {
            $module = 'tempfile';
        }
        when ('File::stat') {
            $module = 'os';
        }
        when ('Glib') {
            if ($symbols) {
                $element->delete;
            }
            else {
                $element->add_element( PPI::Token::Whitespace->new(q{ }) );
                $symbols = PPI::Token::Quote::Double->new('"GObject"');
                $element->add_element($symbols);
                $module = 'gi.repository';
            }
        }
        when ('Gtk3') {
            add_import( $element, 'gi' );
            my $parent    = $element->parent;
            my $statement = PPI::Statement->new;
            $statement->add_element(
                PPI::Token::Word->new('gi.require_version("Gtk", "3.0")') );
            $parent->__insert_before_child( $element, $statement );
            $parent->__insert_before_child( $element,
                PPI::Token::Whitespace->new("\n") );
            $element->add_element( PPI::Token::Whitespace->new(q{ }) );
            delete_everything_after($path);
            $element->add_element( PPI::Token::Whitespace->new(q{ }) );
            $symbols = PPI::Token::Quote::Double->new('"Gtk"');
            $element->add_element($symbols);
            $module = 'gi.repository';
            indent_element($statement);
        }
        when ('Image::Magick') {
            $module = 'PythonMagick';
        }
        when ('Image::Sane') {
            delete_everything_after($path);
            $module = 'sane';
            $symbols->delete;
            undef $symbols;
        }
        when ('Locale::gettext') {
            delete_everything_after($path);
            $module = 'gettext';
            $symbols->delete;
            undef $symbols;
        }
        when ('Log::Log4perl') {
            delete_everything_after($path);
            $module = 'logging';
            $symbols->delete;
            undef $symbols;
        }
        when ('Set::IntSpan') {
            delete_everything_after($path);
            $element->add_element( PPI::Token::Whitespace->new(q{ }) );
            $symbols = PPI::Token::Quote::Double->new('"intspan"');
            $element->add_element($symbols);
            $module = 'intspan';
        }
        when ('Storable') {
            if ( $symbols->{content} =~ s/dclone/deepcopy/xsm ) {
                $symbols->{sections}[0]{size} += 2;
                $module = 'copy';
            }
        }
        when ('Thread::Queue') {
            $module = 'queue';
        }
        when ('base') {
            my $class = $top->find_first(
                sub {
                    $_[1]->isa('PPI::Statement::Sub')
                      and $_[1]->schild(0) eq 'class';
                }
            );
            if ( not $class ) {
                $element->delete;
                return;
            }
            my $list = $class->schild(0)->snext_sibling->snext_sibling;
            my @symbols =
              map_import_symbols( $import, $path, $module, $symbols );
            if ( $symbols[0] eq 'Exporter' ) {
                shift @symbols;
                if ( @symbols > 0 ) {
                    shift @symbols;
                }
            }
            for my $symbol (@symbols) {
                map_element($symbol);
                $list->add_element( $symbol->remove );
            }
            $element->delete;
            return;
        }
        when ('threads') {
            $module = 'threading';
        }
        when ('threads::shared') {
            $element->delete;
        }
    }
    if ( $symbols and $symbols->isa('PPI::Token::Number') ) {
        $symbols->delete;
        undef $symbols;
    }
    if ( $symbols and defined $symbols->{content} ) {
        map_import_symbols( $import, $path, $module, $symbols );
    }
    else {
        $import->{content} = 'import';
        $path->{content}   = $module;
    }
    return;
}

sub map_interpreted_string {
    my ($element) = @_;
    my $formatted;
    while ( defined $element->{content}
        and $element =~ /^(.*)\$(\w+)(.*)$/xsm )
    {
        $formatted = 1;
        my $pre_content  = $1;
        my $varname      = $2;
        my $post_content = $3;
        if ( $varname =~ /^\d+$/xsm ) {
            my $magic = PPI::Token::Magic->new("\$$varname");
            $element->insert_after($magic);
            my $post = PPI::Token::Quote::Double->new("}$post_content");
            $magic->insert_after($post);
            map_regex_group( $magic, $varname );
            $element->{content} = $pre_content . '{';
        }
        else {
            $element->{content} =~ s/\$(\w+)/{$1}/xsmg;
        }
    }
    if ($formatted) {
        $element->{content} = "f$element->{content}";
    }
    return;
}

sub map_print {
    my ($element) = @_;
    my $list      = map_built_in($element);
    my $block     = $list->schild(0);
    if ( $block->isa('PPI::Structure::Block') ) {
        my $statement = $block->schild(0);
        my $fh        = $statement->schild(0);
        map_element($fh);
        $fh->{content} .= q{.};
        $element->insert_before( $fh->remove );
    }
    else {
        undef $block;
    }

    if ( $element eq 'printf' ) {

        # add an extra set of parens for the tuple
        my $tuple =
          PPI::Structure::List->new( PPI::Token::Structure->new('(') );
        $tuple->{finish} = PPI::Token::Structure->new(')');

        $element->insert_after($tuple);
        $tuple->add_element( $list->remove );

        my @format;
        if ($block) {
            @format = get_argument_for_operator( $block, 1 );
        }
        else {
            @format = get_argument_for_operator( $element, 1 );
        }
        map_format_string( $list, @format );
    }
    else {
        $element->{content} = 'println!';
    }

    if ($block) {
        $element->{content} = 'write';
        $block->delete;
    }

    my $quote = $list->schild($LAST);
    if ( $quote->isa('PPI::Token::Quote::Double') ) {
        $quote->{content} =~ s/\\n"$/"/gsmx;
        if ( $quote eq q{""} ) {
            my $operator = $quote->sprevious_sibling;
            $quote->delete;
            if ($operator) {
                $operator->delete;
            }
        }
    }
    return;
}

sub map_word {
    my ($element) = @_;
    if ( not $element->{content} ) { return }
    $element->{content} =~ s/::/./gsm;
    given ("$element") {
        when ('FALSE') {    # special-case common bare words
            $element->{content} = 'False';
        }
        when ('Data.UUID') {
            map_data_uuid($element);
        }
        when ('File.Temp') {
            map_file_temp($element);
        }
        when ('Glib.Type') {
            $element->{content} = 'GObject.TypeModule';
        }
        when (/Gtk\d[.]Gdk/xsm) {
            $element->{content} =~ s/Gtk\d[.]//gsmx;
            add_import( $element, 'gi.repository', 'Gdk' );
        }
        when (/Gtk\d[.]EVENT/xsm) {
            $element->{content} =~ s/Gtk\d/Gdk/gsmx;
            add_import( $element, 'gi.repository', 'Gdk' );
        }
        when (/Gtk\d/xsm) {
            $element->{content} =~ s/Gtk\d/Gtk/gsmx;
        }
        when ('Image.Magick') {
            map_image_magick($element);
        }
        when ('Locale.gettext') {
            $element->{content} = 'gettext';
            my $operator = $element->snext_sibling;
            my $method   = $operator->snext_sibling;
            if ( $method eq 'domain' ) {
                $method->{content} = 'translation';
            }
        }
        when ('POSIX.strftime') {
            map_posix_strftime($element);
        }
        when ('Readonly') {
            my $operator = $element->parent->find_first('PPI::Token::Operator');
            if ( $operator ne '=>' ) {
                croak "Unexpected operator '$operator'\n";
            }
            $operator->{content} = q{=};
            $element->delete;
        }
        when (/^SANE_/xsm) {
            map_sane_enums($element);
        }
        when ('TRUE') {
            $element->{content} = 'True';
        }
        when ('Time_to_Date') {
            $element->{content} = 'datetime.datetime.fromtimestamp';
        }
        when ('bless') {
            $element->parent->delete;
        }
        when ('catch') {
            $element->{content} = 'except';
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
        when ('create_str') {
            $element->{content} = 'str';
            my $operator = $element->sprevious_sibling;
            my $object   = $operator->sprevious_sibling;
            my $list     = map_built_in($element);
            $list->add_element( $object->remove );
            my $list2 =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $list2->{finish} = PPI::Token::Structure->new(')');
            $list->add_element($list2);
            $operator->delete;
        }
        when (/^(?:croak|die)$/xsm) {
            $element->{content} = 'raise';
        }
        when ('dclone') {
            $element->{content} = 'deepcopy';
        }
        when ('decode_utf8') {
            my $list       = map_built_in($element);
            my $expression = $list->schild(0);
            my $str        = $expression->schild(0);
            $element->insert_before( $str->remove );
            $element->{content} = '.decode';
            $list->add_element( PPI::Token::Quote::Double->new('"utf8"') )
        }
        when ('defined') {
            map_defined($element);
        }
        when ('delete') {
            if ( not $element->parent->isa('PPI::Statement::Expression') ) {
                map_built_in($element);
                $element->{content} = 'del';
            }
        }
        when ('do') {
            map_do($element);
        }
        when ('elsif') {
            $element->{content} = 'elif';
        }
        when ('eval') {
            map_eval($element);
        }
        when ('grep') {
            map_grep($element);
        }
        when ('hex') {
            map_built_in($element);
        }
        when ('if') {
            map_postfix_if($element);
        }
        when (/^is(?:_deeply)?$/xsm) {
            map_is($element);
        }
        when ('isa_ok') {
            $element->insert_before( PPI::Token::Word->new('assert') );
            $element->insert_before( PPI::Token::Whitespace->new(q{ }) );
            $element->{content} = 'isinstance';
            my $type = $element->snext_sibling->schild(0)->schild($LAST);
            $type->{content} =~ s/::/./gsm;
            $type->{content} =~ s/["']//gsmx;
        }
        when (/^(?:keys|values)$/xsm) {
            my $list = map_built_in($element);
            for my $child ( $list->children ) {
                $element->insert_before( $child->remove );
            }
            $element->insert_before( PPI::Token::Operator->new(q{.}) );
        }
        when ('keyval') {
            my $list     = map_built_in($element);
            my @children = $list->children;
            if (@children) {
                $list->insert_before( PPI::Token::Operator->new(q{=}) );
                for my $child (@children) {
                    map_element($child);
                    $element->parent->__insert_after_child( $list,
                        $child->remove );
                }
            }
            $list->delete;
        }
        when ('killfam') {
            add_import( $element, 'os' );
            $element->{content} = 'os.killpg';
            my $list   = map_built_in($element);
            my $sig    = $list->schild(0);
            my $op     = $list->schild(1);
            my $pid    = $list->schild(2);
            my $method = PPI::Token::Word->new('os.getpgid');
            $pid->insert_before($method);
            map_built_in($method);
            $list->add_element( $op->remove );

            if ( $sig =~ /KILL/xsm ) {
                $sig->{content} = 'signal.SIGKILL';
            }
            $list->add_element( $sig->remove );
        }
        when ('last') {
            $element->{content} = 'break';
        }
        when ('length') {
            my $list = map_built_in($element);
            $element->{content} = 'len';
        }
        when ('local') {    # no equivalent in python
            $element->delete;
        }
        when ('log') {
            add_import( $element, 'math' );
            $element->{content} = 'math.log';
        }
        when (/^(?:copy|move)$/xsm) {
            map_move_copy($element);
        }
        when (/^(?:my|our)$/xsm) {
            my $symbol = $element->snext_sibling;
            if ( $symbol eq
                '$self' )    ## no critic (RequireInterpolationOfMetachars)
            {
                $element->parent->delete;
            }
            else {
                $element->delete;
            }
        }
        when ('next') {
            map_built_in($element);
        }

        when ('new') {
            map_new($element);
        }
        when ('ok') {
            map_ok($element);
        }
        when ('open') {
            map_open($element);
        }
        when ('pass') {
            $element->{content} = 'assert True';
            if ( $element->snext_sibling ) {
                $element->insert_after( PPI::Token::Operator->new(q{,}) );
            }
        }
        when (/^printf?$/xsm) {
            map_print($element);
        }
        when (/^(?:push|unshift)$/xsm) {
            map_push($element);
        }
        when ('ref') {
            map_ref($element);
        }
        when ('rmdir') {
            my $list = map_built_in($element);
            add_import( $element, 'os' );
            $element->{content} = 'os.rmdir';
        }
        when ('scalar') {
            map_scalar($element);
        }
        when ('setlocale') {
            map_setlocale($element);
        }
        when ('share') {
            my $list = map_built_in($element);
            $list->insert_after( PPI::Token::Symbol->new('queue.Queue()') );
            $list->insert_after( PPI::Token::Operator->new(q{=}) );
            for my $child ( $list->children ) {
                $list->insert_before( $child->remove );
            }
            $element->delete;
            $list->delete;
        }
        when ('shift') {
            map_shift($element);
        }
        when (/^signal_(connect|connect_after|emit)$/xsm) {
            $element->{content} = $1;
            if ( $element->{content} =~ /connect/xsm ) {
                my $list = map_built_in($element);
                map_element($list);
                my $exp   = $list->schild(0);
                my $event = $exp->schild(0);
                $event->{content} =~ s/_/-/gxsm;
                $event->{content} = "'$event->{content}'";
                my $op = $exp->schild(1);
                $op->{content} = q{,};
            }
        }
        when ('sort') {
            my $list = map_built_in($element);
            $element->{content} = 'sorted';
        }
        when ('splice') {
            map_splice($element);
        }
        when ('split') {
            map_split($element);
        }
        when ('sprintf') {
            map_sprintf($element);
        }
        when ('stat') {
            $element->{content} = 'os.stat';
        }
        when ('sub') {
            map_anonymous_sub($element);
        }
        when ('system') {
            map_system($element);
        }
        when ('threads') {
            map_threads($element);
        }
        when ('undef') {
            map_undef($element);
        }
        when ('unlink') {
            my $list = map_built_in($element);
            add_import( $element, 'os' );
            $element->{content} = 'os.remove';
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
        when ('waitpid') {
            add_import( $element, 'os' );
            my $list = map_built_in($element);
            $element->{content} = 'os.waitpid';
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

sub next_sibling {
    my ( $element, $n ) = @_;
    return $n == 0 ? $element->sprevious_sibling : $element->snext_sibling;
}

1;
