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
    [ 'left',     'term', 'list operator (leftward)' ],
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

my $ANONYMOUS = 0;

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

sub ensure_include_is_top_level {
    my ( $element, $top ) = @_;
    if ( $element->parent ne $top ) {

        # find all top-level includes
        my $includes = $top->find(
            sub {
                $_[1]->isa('PPI::Statement::Include')
                  and $_[1]->parent eq $top;
            }
        );
        if ($includes) {
            $includes->[-1]->insert_after( PPI::Token::Whitespace->new("\n") );
            $includes->[-1]->insert_after( $element->remove );
        }
        else {
            my $first_child = $top->child(0);
            $top->__insert_before_child( $first_child, $element->remove );
            $top->__insert_before_child( $first_child,
                PPI::Token::Whitespace->new("\n") );
        }
    }
    return;
}

sub delete_everything_after {
    my ($element) = @_;
    while ( my $iter = $element->next_sibling ) {
        if ( $iter eq q{;} ) {
            last;
        }
        else {
            $iter->delete;
        }
    }
    return;
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

sub map_arrow_operator {
    my ($element) = @_;
    my @prev      = get_argument_for_operator( $element, 0 );
    my $next      = $element->snext_sibling;

    # closure -> iterator
    if (
        $prev[0] eq 'iter' and $next    # hard-coded to iter. Fragile.
        and $next          and $next->isa('PPI::Structure::List')
      )
    {
        $element->{content} = q{.};
        $element->parent->__insert_before_child( $element, $prev[0]->remove );
        $element->insert_after( PPI::Token::Word->new('next') );

      # FIXME: lose any arguments the closure takes, as next can't process them.
        for my $child ( $next->children ) {
            $child->delete;
        }
    }
    elsif (    # hashref
        $next and ( $next->isa('PPI::Structure::Subscript')
            or $next->isa('PPI::Structure::List') )
        and $prev[0] ne 'self'
      )
    {
        $element->delete;
        map_subscript($next);
    }
    else {     # class method call
        if ( $prev[-1]->isa('PPI::Token::Word') ) {
            $element->{content} = q{::};
        }
        else { $element->{content} = q{.} }
        if (    $prev[0] eq 'self'
            and $next->isa('PPI::Structure::Subscript')
            and not $next->find_first('PPI::Token::Symbol') )
        {
            my $child = $next->schild(0);
            $element->parent->__insert_after_child( $element, $child->remove );
            $next->delete;
        }

        # methods require parens
        elsif ( $next->isa('PPI::Token::Word') ) {
            my $parens = $next->snext_sibling;
            if ( not $parens or not $parens->isa('PPI::Structure::List') ) {
                my $list =
                  PPI::Structure::List->new( PPI::Token::Structure->new('(') );
                $list->{finish} = PPI::Token::Structure->new(')');
                $next->insert_after($list);
            }
        }
        else {
            $element->delete;
        }
    }
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
        $block->{finish} = PPI::Token::Structure->new("}\n");
        $try->add_element($block);
        $block->add_element( PPI::Token::Whitespace->new("\n") );
        $block->add_element( $statement->remove );

        my $except = PPI::Statement::Compound->new;
        $parent->__insert_after_child( $try,
            PPI::Token::Whitespace->new("\n"), $except );
        $except->add_element( PPI::Token::Word->new('except') );
        $block = PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
        $block->{finish} = PPI::Token::Structure->new("}\n");
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

sub map_cast {
    my ($element) = @_;

    # implicit cast from array to scalar -> len()
    my $operator = $element->sprevious_sibling;
    my $parent   = $element->parent;
    my $block    = $element->snext_sibling;
    if (   $element eq q{@}
        or $element eq q{$#} )    ## no critic (RequireInterpolationOfMetachars)
    {

        # array cast in list context
        if ( not $operator
            and $element ne
            q{$#} )               ## no critic (RequireInterpolationOfMetachars)
        {
            remove_cast( $element, $block, $parent );
        }

        # array cast in scalar context -> len()
        elsif ( defined $PRECEDENCE{$operator}
            or $element eq
            q{$#} )               ## no critic (RequireInterpolationOfMetachars)
        {
            my $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $list->{finish} = PPI::Token::Structure->new(')');
            $parent->__insert_after_child( $element,
                PPI::Token::Word->new('len'), $list );
            if ( not $block->isa('PPI::Structure::Block') ) {
                croak "Expected block, found '$block'\n";
            }
            for my $child ( $block->children ) {
                map_element($child);
                $list->add_element( $child->remove );
            }
            if ( $element eq
                q{$#} )    ## no critic (RequireInterpolationOfMetachars)
            {
                $list->insert_after( PPI::Token::Number->new(1) );
                $list->insert_after( PPI::Token::Operator->new(q{-}) );
            }
            $element->delete;
            $block->delete;
        }
    }

    # cast from hashref to hash or scalar ref to scalar -> remove cast
    elsif ( $element =~ /^[\$%]$/xsm and $block->isa('PPI::Structure::Block') )
    {
        remove_cast( $element, $block, $parent );
    }

    # cast to ref -> remove cast
    elsif ( $element eq q{\\} ) {
        $element->delete;
    }
    return;
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

        # if no variable assignment, map magic
        my $var = $list->sprevious_sibling;
        if ( $var eq $word ) {
            $element->__insert_before_child(
                $list,
                PPI::Token::Symbol->new('_'),
                PPI::Token::Whitespace->new(q{ })
            );
        }

        for my $child ( $list->children ) {
            $element->__insert_after_child( $list, $child->remove );
        }
        $element->__insert_after_child(
            $list,
            PPI::Token::Operator->new('in'),
            PPI::Token::Whitespace->new(q{ })
        );
        $list->delete;
    }
    return;
}

sub map_directory {
    my ( $dir, @xpaths ) = @_;
    for my $path (@xpaths) {
        if ( $path eq $dir ) {
            warn "Ignoring $dir, as in exclude list\n";
            return;
        }
    }
    if ( -d $dir ) {
        for my $file ( glob( $dir . q{/*} ) ) {
            map_directory( $file, @xpaths );
        }
    }
    else {
        $dir =~ /.*[.](\w+)$/xsm;
        if ( not defined $1 or $1 =~ /(p[lm]|t)$/xsmi ) {
            map_file($dir);
        }
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

sub map_eval {
    my ($element) = @_;

    # map
    #   if ( not eval { require Package; } ) {plan(skip_all)}
    # or
    #   eval "use Package";
    #   plan skip_all => "Package required" if $@;
    # -> match MyPackage::new() { Ok(package) => {}, Err(_) => {} }
    my $arg = $element->snext_sibling;
    my ( $module, $expression, $compound, $statement1, $statement2 );
    if (
        $arg->isa('PPI::Structure::Block')
        and my $import = $arg->find_first(
            sub {
                $_[1]->isa('PPI::Token::Word')
                  and $_[1]->content =~ /^(?:require|use)/xsm;
            }
        )
      )
    {
        $module = $import->snext_sibling;
    }
    elsif ( $arg->isa('PPI::Token::Quote')
        and $arg =~ /^["'](?:require|use)\s+([\w:]+)/xsm )
    {
        $module = $1;
    }
    if ($module) {
        if (
                $expression = $element->parent
            and $expression->isa('PPI::Statement::Expression')
            and $compound = $expression->parent
            and $compound->isa('PPI::Statement::Compound')
            and $compound->find_first(
                sub {
                    $_[1]->isa('PPI::Token::Word')
                      and $_[1]->content eq 'skip_all';
                }
            )
          )
        {
            my $message = $compound->find_first('PPI::Token::Quote');
            $compound->parent->__insert_before_child(
                $compound,
                PPI::Token::Word->new(
                        "match $module"
                      . "::new() { Ok(package) => {}, Err(_) => { println!($message); } }"
                )
            );
            $compound->delete;
            return;
        }
        elsif (
                $statement1 = $element->parent
            and $statement1->isa('PPI::Statement')
            and $statement2 = $statement1->snext_sibling
            and $statement2->isa('PPI::Statement')
            and $statement2->find_first(
                sub {
                    $_[1]->isa('PPI::Token::Word')
                      and $_[1]->content eq 'skip_all';
                }
            )
          )
        {
            my $message = $statement2->find_first('PPI::Token::Quote');
            $statement1->parent->__insert_before_child(
                $statement1,
                PPI::Token::Word->new(
                        "match $module"
                      . "::new() { Ok(package) => {}, Err(_) => { println!($message); } }"
                )
            );
            $statement1->delete;
            $statement2->delete;
            return;
        }
    }

    map_built_in($element);
    return;
}

sub map_expression {
    my ($element) = @_;

    # remove any newlines that aren't enclosed by parens
    my $newlines = $element->find(
        sub {
            $_[1]->isa('PPI::Token::Whitespace')
              and $_[1]->content =~ /\n/xsm;
        }
    );
    if ($newlines) {
        for my $newline ( @{$newlines} ) {
            my $list;
            my $parent = $newline;
            while ( $parent = $parent->parent ) {
                if (   $parent->isa('PPI::Structure::List')
                    or $parent->isa('PPI::Structure::Constructor') )
                {
                    $list = $parent;
                    break;
                }
            }
            if ( not $list ) { $newline->delete }
        }
    }
    return;
}

sub map_file {
    my ($file) = @_;
    $ANONYMOUS = 0;
    my $pid = open3( undef, my $chld_out, undef, 'file', $file );
    waitpid $pid, 0;
    my $magic = <$chld_out>;
    if ( $magic !~ /Perl/xsm and $file !~ /[.]t$/xsm ) {
        warn "Ignoring $file, as not Perl code\n";
        return;
    }
    my $doc = PPI::Document::File->new($file);
    warn "Reading from $file\n";
    map_element($doc);
    my $outfile = map_path($file);
    warn "Writing to $outfile\n";
    my ( undef, $directories, undef ) = File::Spec->splitpath($outfile);
    if ( not -d $directories ) {
        mkdir $directories;
    }
    open my $fh, '>', $outfile or croak "Error opening $outfile";
    print {$fh} $doc or croak "Error writing to $outfile";
    close $fh        or croak "Error closing $outfile";
    return;
}

sub map_import_symbols {
    my ( $import, $path, $module, $symbols ) = @_;
    my $whitespace = $symbols->previous_sibling;
    if ($whitespace) { $whitespace->{content} = q{::} }
    my @symbols;
    if ( $symbols->isa('PPI::Token::Quote') ) {
        $symbols->{content} = quote2content($symbols);
        if ( $symbols eq ':all' ) {
            $symbols->{content} = q{*};
        }
        push @symbols, $symbols;
    }
    else {
        my $string = substr $symbols->content,
          $symbols->{sections}[0]{position}, $symbols->{sections}[0]{size};
        my $i    = 0;
        my $list = PPI::Structure::List->new( PPI::Token::Structure->new('{') );
        $list->{finish} = PPI::Token::Structure->new('}');
        $symbols->insert_before($list);
        for ( split q{ }, $string ) {
            if ( $i++ ) {
                push @symbols, PPI::Token::Whitespace->new(q{,});
                $list->add_element( $symbols[-1] );
            }
            push @symbols, PPI::Token::Word->new($_);
            $list->add_element( $symbols[-1] );
        }
        $symbols->delete;
    }
    return @symbols;
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
            $def->add_element( PPI::Token::Word->new("\nfn") );
            $def->add_element( PPI::Token::Whitespace->new(q{ }) );
            $def->add_element( PPI::Token::Word->new('test_1') );
            my $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $list->{finish} = PPI::Token::Structure->new(')');
            $def->add_element($list);
            $element->insert_after($def);
            $element->insert_before( PPI::Token::Comment->new('#[test]') );
            $element->delete;

            # Add a block to scope and indent things properly
            my $block =
              PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
            $block->{finish} = PPI::Token::Structure->new("}\n");
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

    if ( $import ne 'use' ) {
        croak "Unrecognised include $element\n";
    }
    my $path    = $import->snext_sibling;
    my $symbols = $path->snext_sibling;
    if ( $symbols eq q{;} ) { undef $symbols }
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
                $module = 'glib';
                delete_everything_after($path);
            }
        }
        when ('Gtk3') {
            $module = 'gtk';
            delete_everything_after($path);
            $symbols->delete;
            undef $symbols;
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
        $path->{content} = $module;
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

sub map_is {
    my ($element) = @_;
    map_built_in($element);
    $element->{content} = 'assert_eq!';
    return;
}

sub map_fat_comma {    # =>
    my ($element)  = @_;
    my $expression = $element->parent;
    my $parent     = $expression->parent;
    my $prev       = $parent->sprevious_sibling;
    my @largument  = get_argument_for_operator( $element, 0 );
    my @rargument  = get_argument_for_operator( $element, 1 );

    # function call - map -> name arguments
    if (    $expression->isa('PPI::Statement')
        and $parent->isa('PPI::Structure::List')
        and $prev->isa('PPI::Token::Word') )
    {
        $element->{content} = q{=};
        my $property = $element->sprevious_sibling;

        # remove quotes from properties, mapping hyphens to underscores
        if ( $property->isa('PPI::Token::Quote') ) {
            my $word = PPI::Token::Word->new( substr $property->{content},
                1, length( $property->{content} ) - 2 );
            $word->{content} =~ s/-/_/gxsm;
            $property->insert_after($word);
            $property->delete;
        }
        return;
    }
    if ( @largument == 1 and $largument[0]->isa('PPI::Token::Word') ) {
        my $key = PPI::Token::Quote::Double->new( q{"} . $largument[0] . q{"} );
        $expression->__insert_before_child( $largument[0], $key );
        $largument[0]->delete;
    }
    if ( not $parent->isa('PPI::Structure::Constructor') ) {
        if ( $parent->isa('PPI::Structure::List')
            and not $prev->isa('PPI::Token::Word') )
        {
            if ( $parent->{start} ne '{' ) {
                $parent->{start}{content}  = '{';
                $parent->{finish}{content} = '}';
            }
        }
        else {
            my $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('{') );
            $list->{finish} = PPI::Token::Structure->new('}');
            $parent->__insert_before_child( $expression, $list );
            $list->add_element( $expression->remove );
        }
    }
    $element->{content} = q{:};
    return;
}

sub map_ok {
    my ($element) = @_;
    map_built_in($element);
    $element->{content} = 'assert!';
    return;
}

sub map_operator {
    my ($element) = @_;
    if ( not $element->{content} ) { return }
    $element->{originally} = $element->{content};
    given ("$element") {
        when (q{?}) {
            my @conditional = get_argument_for_operator( $element, 0 );
            my @true        = get_argument_for_operator( $element, 1 );

            # Work around https://github.com/Perl-Critic/PPI/issues/262
            if ( $true[0]->isa('PPI::Token::Label') ) {
                my $word = $true[0]->content;
                $word =~ s/[ ]*:$//xsm;
                $word = PPI::Token::Word->new($word);
                $true[0]->insert_before($word);
                map_word($word);
                $true[0]->insert_after( PPI::Token::Operator->new(q{:}) );
                $true[0]->delete;
                @true = get_argument_for_operator( $element, 1 );
            }

            my $operator = $true[-1]->snext_sibling;
            my @false    = get_argument_for_operator( $operator, 1 );

            # map true/false expressions before mapping ternary in case we
            # change the precedence in doing so
            if ( defined $BUILTINS{ $true[0]->content } ) {
                my $list = map_built_in(@true);
                @true = ( $true[0], $list );
            }
            if ( defined $BUILTINS{ $false[0]->content } ) {
                my $list = map_built_in(@false);
                @false = ( $false[0], $list );
            }

            if ( $operator ne q{:} or not( @true and @false ) ) {
                croak
"Error parsing conditional operator: '@conditional $element @true $operator @false'\n";
            }
            $element->{content}  = 'if';
            $operator->{content} = 'else';
            my $parent = $element->parent;
            for my $conditional (@conditional) {
                $parent->__insert_after_child( $element, $conditional->remove );
            }
            $parent->__insert_after_child( $element,
                PPI::Token::Whitespace->new(q{ }) );
            for my $true (@true) {
                $parent->__insert_before_child( $element, $true->remove );
            }
            $parent->__insert_before_child( $element,
                PPI::Token::Whitespace->new(q{ }) );
        }
        when (q{.=}) {
            $element->{content} = q{+=};
        }
        when (q{++}) {
            $element->{content} = q{+=};
            my @next = get_argument_for_operator( $element, 1 );
            if (@next) {
                for my $next (@next) {
                    $element->insert_before( $next->remove );
                }
            }
            $element->insert_after( PPI::Token::Number->new(1) );
        }
        when (q{.}) {
            $element->{content} = q{+};
        }
        when (q{..}) {
            my @start = get_argument_for_operator( $element, 0 );
            my @stop  = get_argument_for_operator( $element, 1 );
            $start[0]->insert_before( PPI::Token::Word->new('range') );
            my $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $list->{finish} = PPI::Token::Structure->new(')');
            $start[0]->insert_before($list);
            if ( $start[0] eq '0' ) {
                $start[0]->delete;
            }
            else {
                for my $child (@start) {
                    $list->add_element( $child->remove );
                }
                $list->add_element( PPI::Token::Operator->new(q{,}) );
            }
            for my $child (@stop) {
                $list->add_element( $child->remove );
            }
            $list->add_element( PPI::Token::Operator->new(q{+}) );
            $list->add_element( PPI::Token::Number->new(1) );
            $element->delete;
        }
        when (q{!}) {
            $element->{content} = q{not};
        }
        when (q{=>}) {
            map_fat_comma($element);
        }
        when (q{->}) {
            map_arrow_operator($element);
        }
        when (/^-[fs]$/xsm) {
            add_import( $element, 'os' );
            my $parent = $element->parent;
            my $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $list->{finish} = PPI::Token::Structure->new(')');
            my @args = get_argument_for_operator( $element, 1 );
            for (@args) {
                $list->add_element( $_->remove );
            }
            my $method;
            if ( $element eq '-f' ) {
                $method = 'isfile';
            }
            elsif ( $element eq '-s' ) {
                $method = 'getsize';
            }
            $parent->__insert_after_child( $element,
                PPI::Token::Word->new("os.path.$method"), $list );
            $element->delete;
        }
        when ('eq') {
            $element->{content} = q{==};
        }
        when ('ge') {
            $element->{content} = q{>=};
        }
        when ('gt') {
            $element->{content} = q{>};
        }
        when ('le') {
            $element->{content} = q{<=};
        }
        when ('lt') {
            $element->{content} = q{<};
        }
        when ('ne') {
            $element->{content} = q{!=};
        }
        when ('xor') {
            $element->{content} = q{^};
        }
    }
    return;
}

# FIXME:
#https://docs.python-guide.org/writing/structure/ i.e.:
#  bin -> bin
#  lib/package -> package
#  t -> tests, then ensure the contents start with test_
#  Makefile.PL -> setup.py + Makefile
sub map_path {
    my ($path) = @_;
    my ( $volume, $directories, $file ) = File::Spec->splitpath($path);
    my @dirs    = File::Spec->splitdir($directories);
    my $outfile = "$file.rs";
    if ( $file =~ /^(.+)[.](:?(:?p[lm])|t)$/xsm ) {
        $outfile = "$1.rs";
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

sub map_postfix_if {
    my ($element) = @_;
    my $prev = $element->sprevious_sibling;
    if ( not $prev ) { return }
    my $cstatement = PPI::Statement::Compound->new;
    my $ostatement = $element->parent;
    my $condition  = $element->snext_sibling;
    my $expression = $condition->schild(0);
    $ostatement->insert_before($cstatement);
    my $block = PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
    $block->{start}->{content} = q{:};
    $cstatement->add_element( $element->remove );
    $cstatement->add_element( PPI::Token::Whitespace->new(q{ }) );
    $cstatement->add_element( $condition->remove );
    $condition->insert_after($block);
    $block->add_element( $ostatement->remove );
    indent_element($cstatement);
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

sub map_sub {
    my ($element) = @_;
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

    # new -> __init__ for classes
    if ( $child eq 'new' ) {
        my $parent = $child;
        while ( $parent = $parent->parent ) {
            if (    $parent->isa('PPI::Statement::Sub')
                and $parent->schild(0) eq 'class' )
            {
                $child->{content} = '__init__';
                last;
            }
        }

    }

    my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
    $list->{finish} = PPI::Token::Structure->new(')');
    $child->insert_after($list);

    my (@docstring);
    my $prev = $element;
    while (
        $prev = $prev->previous_sibling
        and (  $prev->isa('PPI::Token::Comment')
            or $prev->isa('PPI::Token::Whitespace') )
      )
    {
        unshift @docstring, $prev;
    }

    # trim docstring
    while ( @docstring and $docstring[0]->isa('PPI::Token::Whitespace') ) {
        shift @docstring;
    }
    while ( @docstring and $docstring[-1]->isa('PPI::Token::Whitespace') ) {
        pop @docstring;
    }

    # convert to string
    if (@docstring) {
        my $docstring = PPI::Token::Quote::Single->new(q{"""});
        my $statement = PPI::Statement->new;
        $statement->add_element($docstring);
        for my $item (@docstring) {
            $item->{content} =~ s/^[#]\s*//xsm;
            $docstring->{content} .= $item->{content};
            $item->delete;
        }
        $docstring->{content} .= q{"""};
        my $block = $element->find_first('PPI::Structure::Block');
        $block->__insert_after_child( $block->child(0), $statement );
    }
    return;
}

sub map_subscript {
    my ($element) = @_;
    $element->{start}->{originally}  = $element->{start}->{content};
    $element->{start}->{content}     = q{.get(};
    $element->{finish}->{originally} = $element->{finish}->{content};
    $element->{finish}->{content}    = q{)};
    my $expression = $element->schild(0);
    if (    $element->{start}->{originally}
        and $element->{start}->{originally} eq '{'
        and $expression )
    {
        my $key = $expression->schild(0);
        if ( $key->isa('PPI::Token::Word') ) {
            if ( $key ne 'scalar' ) {
                $key->{content} = '&String::from("' . $key . '")';
            }
        }
    }
    return;
}

sub map_symbol {
    my ($element) = @_;
    if ( not $element->{content} ) { return }
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
        if ( $element->{content} =~ /^@/smx ) {
            my $operator = $element->snext_sibling;
            my $method   = $element->sprevious_sibling;
            if ($operator) {
                if ( $operator eq q{=} ) {
                    my $list = $operator->snext_sibling;
                    if ($list) {
                        $list->{start}->{content}  = q{[};
                        $list->{finish}->{content} = q{]};
                    }
                }

                # map scalar @array -> len(array)
                elsif ( $operator =~ /(?:==|<=|>=|!=|[><+-])/xsm
                    and ( not $method or $method ne 'assert' ) )
                {
                    my $list =
                      PPI::Structure::List->new(
                        PPI::Token::Structure->new('(') );
                    $list->{finish} = PPI::Token::Structure->new(')');
                    $element->insert_before( PPI::Token::Word->new('len') );
                    $element->insert_before($list);
                    $list->add_element( $element->remove );
                }
            }
        }
        $element->{content} =~ s/^[\$@%&]//smx;
        if ( $element->{content} ~~ @RESERVED_WORDS ) {
            $element->{content} = "_$element->{content}";
        }
    }
    return;
}

sub map_variable {
    my ($element) = @_;
    my $operator = $element->find_first('PPI::Token::Operator');
    if ( $operator eq 'in' ) {
        my $iter = $operator->snext_sibling;
        if ($iter) {
            my $operator2 = $iter->snext_sibling;
            if ( $operator2 eq '->' ) {
                my $list = $operator2->snext_sibling;
                if ( $list eq '()' ) {
                    $list->delete;
                    $operator2->delete;
                }
            }
        }
    }

    # ensure global variables appear before class definitions
    elsif (
        $element->find_first(
            sub {
                $_[1]->isa('PPI::Token::Operator')
                  and $_[1]->content eq q{=};
            }
        )
      )
    {
        my $symbol = $element->find_first('PPI::Token::Symbol');
        if ( $symbol =~ /^[\$@%][[:upper:]\d_]+$/xsm ) {
            my $parent = $element->parent;
            if ( not $parent->isa('PPI::Document') ) {
                my $gparent = $parent->parent;
                $gparent->insert_before( $element->remove );
                $gparent->insert_before( PPI::Token::Whitespace->new("\n") );
            }
        }
    }
    else {
        $element->add_element( PPI::Token::Operator->new(q{=}) );

        # map my ($var1, @var2, %var3) -> (var1, var2) = (None, [], {})
        if ( my $list = $element->find_first('PPI::Structure::List') ) {
            my $dest_list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $dest_list->{finish} = PPI::Token::Structure->new(')');
            $element->add_element($dest_list);
            for my $child ( $list->schild(0)->children ) {
                if ( $child->isa('PPI::Token::Symbol') ) {
                    $dest_list->add_element(
                        create_variable_definition($child) );
                }
                elsif ( $child eq q{,} ) {
                    $dest_list->add_element( PPI::Token::Operator->new(q{,}) );
                }
            }
        }

        # map my $var1 -> var1 = None
        else {
            $element->add_element(
                create_variable_definition(
                    $element->find_first('PPI::Token::Symbol')
                )
            );
        }
    }
    return;
}

sub map_word {
    my ($element) = @_;
    if ( not $element->{content} ) { return }
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
        when (/Gtk\d::Gdk/xsm) {
            $element->{content} =~ s/Gtk\d::Gdk/gdk/gsmx;
        }
        when (/Gtk\d::EVENT/xsm) {
            $element->{content} =~ s/Gtk\d/gdk/gsmx;
        }
        when (/Gtk\d/xsm) {
            $element->{content} =~ s/Gtk\d/gtk/gsmx;
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
            my $list = map_built_in($element);
            $element->{content} = 'assert!';
            my $operator = $element->snext_sibling->schild(0)->schild(1);
            $operator->{content} = ' is';
            my $type = $element->snext_sibling->schild(0)->schild($LAST);
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
                $list->delete;
            }
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
                $element->{content} = 'let';
            }
        }
        when ('next') {
            map_built_in($element);
        }
        when ('ok') {
            map_ok($element);
        }
        when ('open') {
            map_open($element);
        }
        when ('pass') {
            my $list = map_built_in($element);
            $element->{content} = 'assert!';
            my $comment = $list->schild(0);
            if ($comment) {
                $comment->insert_before( PPI::Token::Word->new('true') );
                $comment->insert_before( PPI::Token::Operator->new(q{,}) );
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
                $event->{content} = '"' . $event->{content} . '"';
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

sub quote2content {
    my ($element) = @_;
    return substr $element->{content}, 1, length( $element->{content} ) - 2;
}

sub remove_cast {
    my ( $element, $block, $parent ) = @_;
    my $child = $block->schild(0);
    map_element($child);
    $parent->__insert_after_child( $block, $child->remove );
    $element->delete;
    $block->delete;
    return;
}

sub remove_parens_map_children {
    my ($element) = @_;

    # map and move in separate step to keep context
    for my $child ( $element->schild(0)->children ) {
        map_element($child);
    }
    for my $child ( $element->schild(0)->children ) {
        $element->insert_before( $child->remove );
    }
    $element->delete;
    return;
}

1;
