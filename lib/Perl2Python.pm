package Perl2Python;

use warnings;
use strict;
use feature 'switch';
use PPI;
use Exporter ();
use base qw(Exporter);
use Carp;
use File::Spec;
use IPC::Open3 'open3';
use Try::Tiny;
use File::Path qw(make_path);
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
my @BUILTINS = qw(chomp defined eval keys length scalar);
my ( %BUILTINS, %LIST_OPERATORS );
for my $op (@BUILTINS) {
    $BUILTINS{$op} = 1;
}
my @LIST_OPERATORS =
  qw(chmod chown close grep join map next pack open print printf push return reverse say sort split sprintf unlink unshift);
for my $op (@LIST_OPERATORS) {
    $BUILTINS{$op}       = 1;
    $LIST_OPERATORS{$op} = 1;
}

my %REGEX_MODIFIERS = (
    i => 're.IGNORECASE',
    m => 're.MULTILINE',
    o => q{},               # no equivalent
    s => 're.DOTALL',
    x => 're.VERBOSE',
);

my $IGNORED_INCLUDES =
q/^(?:warnings|strict|feature|if|Carp|English|Exporter|File::Copy|IPC::System::Simple|List::Util|POSIX|Proc::Killfam|Readonly|Scalar::Util|Try::Tiny)$/;
my $SIGNAL_PREFIX = 'signal_(?:handler_)?';
my $SIGNAL_SUFFIX =
  '(connect|connect_after|disconnect|emit|handler_block|handler_unblock)';

my @RESERVED_WORDS = qw(class def print break);

my $ANONYMOUS = 0;

sub add_anonymous_method {
    my ( $statement, $block, @args ) = @_;
    my $name = sprintf 'anonymous_%02d', ++$ANONYMOUS;
    my $sub  = PPI::Statement::Sub->new;
    $sub->add_element( PPI::Token::Word->new('sub') );
    $sub->add_element( PPI::Token::Whitespace->new(q{ }) );
    $sub->add_element( PPI::Token::Word->new($name) );
    $sub->add_element($block);
    my $parent = $statement->parent;

    while (
        (
               not $statement->isa('PPI::Statement')
            or $statement->isa('PPI::Statement::Expression')
            or $parent->isa('PPI::Structure::List')
            or $parent->isa('PPI::Statement::Expression')
        )
        and not $parent->isa('PPI::Document')
      )
    {
        $statement = $parent;
        $parent    = $statement->parent;
    }
    $parent->__insert_before_child( $statement, $sub );
    $parent->__insert_before_child( $statement,
        PPI::Token::Whitespace->new("\n") );
    $parent->__insert_before_child( $statement,
        PPI::Token::Whitespace->new("\n") );
    map_element($sub);
    if (@args) {
        my $list = $block->sprevious_sibling;
        for my $child (@args) {
            $list->add_element($child);
        }
    }
    return $name;
}

sub add_import {
    my ( $element, $from, $module ) = @_;
    my $document = $element->top;
    my $search   = "import $from";
    if ($module) {
        $search = "from $from import $module";
    }
    if (
        not $document->find_first(
            sub {
                $_[1]->isa('PPI::Statement::Include')
                  and $_[1]->content eq $search;
            }
        )
      )
    {
        my $statement = PPI::Statement::Include->new;
        if ($module) {
            $statement->add_element( PPI::Token::Word->new('from') );
            $statement->add_element( PPI::Token::Whitespace->new(q{ }) );
            $statement->add_element( PPI::Token::Word->new($from) );
            $statement->add_element( PPI::Token::Whitespace->new(q{ }) );
        }
        else {
            $module = $from;
        }
        $statement->add_element( PPI::Token::Word->new('import') );
        $statement->add_element( PPI::Token::Whitespace->new(q{ }) );
        $statement->add_element( PPI::Token::Word->new($module) );
        $document->__insert_before_child( $document->schild(0),
            $statement, PPI::Token::Whitespace->new("\n") );
    }
    return;
}

sub delete_everything_after {
    my ($element) = @_;
    while ( my $iter = $element->next_sibling ) {
        $iter->delete;
    }
    return;
}

sub find_first_isa_content {
    my ( $element, $isa, $content ) = @_;
    return $element->find_first(
        sub {
            $_[1]->isa($isa)
              and $_[1]->content eq $content;
        }
    );
}

sub get_argument_for_operator {
    my ( $element, $n ) = @_;

    if ( $element eq q{?} ) {
        return get_argument_for_ternary( $element, $n );
    }
    my @sibling;
    my $iter = $element;
    while ( $iter = next_sibling( $iter, $n ) ) {
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

sub map_anonymous_sub {
    my ($element) = @_;
    if ( not $element->parent ) { return }
    my $block = $element->snext_sibling;
    if ( not $block->schild(0) ) {
        my $statement = PPI::Statement->new;
        my $word      = PPI::Token::Word->new('pass');
        $word->{mapped} = 1;    # don't map this again.
        $statement->add_element($word);
        $block->add_element($statement);
    }
    my $name = add_anonymous_method( $element, $block->remove );
    $element->{content} = $name;
    return;
}

sub map_arrow_operator {
    my ($element) = @_;
    my @prev      = get_argument_for_operator( $element, 0 );
    my $next      = $element->snext_sibling;
    if ( not $next ) {
        $element->delete;
        return;
    }

    # closure -> iterator
    if (
        $prev[0] eq 'iter'    # hard-coded to iter. Fragile.
        and $next->isa('PPI::Structure::List')
      )
    {
        $element->insert_before( PPI::Token::Word->new('next') );

      # FIXME: lose any arguments the closure takes, as next can't process them.
        for my $child ( $next->children ) {
            $child->delete;
        }
        for my $prev (@prev) {
            $next->add_element( $prev->remove );
        }
        $element->delete;
    }
    elsif (
        (
               $next->isa('PPI::Structure::Subscript')
            or $next->isa('PPI::Structure::List')
        )
        and $prev[0] ne 'self'
      )
    {
        $element->delete;
    }
    else {
        $element->{content} = q{.};
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
            map_built_in($next);
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

        if ( not @args and $element->snext_sibling !~ /^(?:,|->|==)$/xsm ) {
            @args = get_argument_for_operator( $element, 1 );
        }
        for my $child (@args) {
            $list->add_element( $child->remove );
        }
        $element->insert_after($list);
    }

    # deal with return value from built-in
    my $child = $list->snext_sibling;
    my $next;
    if ($child) {
        $next = $child->snext_sibling;
    }
    if ( $child eq 'or' and $next and $next->isa('PPI::Token::Word') ) {
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

sub map_cast {
    my ($element) = @_;

    # implicit cast from array to scalar -> len()
    my $operator = $element->sprevious_sibling;
    my $parent   = $element->parent;
    my $block    = $element->snext_sibling;
    try {
        if (   $element eq q{@}
            or $element eq
            q{$#} )    ## no critic (RequireInterpolationOfMetachars)
        {

            # array cast in list context
            if (
                ( not $operator
                    and $element ne
                    q{$#} )    ## no critic (RequireInterpolationOfMetachars)
                or ( $operator eq q{=} and $element eq q{@} )
              )
            {
                remove_cast( $element, $block, $parent );
            }

            # array cast in scalar context -> len()
            elsif ( defined $PRECEDENCE{$operator}
                or $element eq
                q{$#} )    ## no critic (RequireInterpolationOfMetachars)
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
        elsif ( $element =~ /^[\$%]$/xsm
            and $block->isa('PPI::Structure::Block') )
        {
            remove_cast( $element, $block, $parent );
        }

        # cast to ref -> remove cast
        elsif ( $element eq q{\\} ) {
            $element->delete;
        }
    }
    catch { return };
    return;
}

sub map_chomp {
    my ($element) = @_;
    my $list = map_built_in($element);
    map_element($list);
    my @var = $list->children;
    if ( $var[0]->isa('PPI::Statement::Expression') ) {
        @var = $var[0]->children;
    }
    my $operator = $element->sprevious_sibling;
    if ( not defined $operator or $operator eq q{} ) {
        $element->insert_before( PPI::Token::Operator->new("@var=") );
    }
    $element->{content} = '.rstrip';
    for my $child (@var) {
        $element->insert_before( $child->remove );
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

sub map_defined {
    my ($element) = @_;
    my $parent    = $element->parent;
    my @args      = get_argument_for_operator( $element, 1 );
    if ( not @args ) {
        my $magic = PPI::Token::Symbol->new('_');
        $element->insert_after($magic);
        push @args, $magic;
    }
    if ( $args[0]->isa('PPI::Token::Cast') ) {
        @args = remove_cast( $args[0], $args[1], $parent );
    }
    elsif ( $args[0]->isa('PPI::Token::Magic') ) {
        $args[0] = map_magic( $args[0] );
    }
    my $list = $args[-1]->parent;
    if ( $list ne $parent ) {
        if ( $list->isa('PPI::Statement::Expression') ) {
            $list = $list->parent;
        }
        my @args2;
        for my $child (@args) {
            push @args2, $child->remove;
        }
        $parent->__insert_after_child( $list, @args2 );
        $list->delete;
        @args = @args2;
    }

    my $not = $element->sprevious_sibling;
    if ( $args[-1]->isa('PPI::Structure::Subscript') ) {
        map_subscript( $args[-1] );
        $element->{content} = 'in';
        my $insert = map_element( $args[0] );
        if ( $not eq 'not' or $not eq q{!} ) {
            $insert = $not;
        }
        for my $child ( $args[-1]->children ) {
            $parent->__insert_before_child( $insert, $child->remove,
                PPI::Token::Whitespace->new(q{ }) );
        }
        $args[-1]->delete;
        $parent->__insert_before_child(
            $args[0],         PPI::Token::Whitespace->new(q{ }),
            $element->remove, PPI::Token::Whitespace->new(q{ }),
        );
        if ( $args[ -(2) ] eq '->' ) {
            $args[ -(2) ]->delete;
        }
        return;
    }

    for my $child (@args) {

        # mapping one child can remove siblings, so check it still exists
        if ($child) {
            map_element($child);
        }
    }

    # pack in parens to ensure we don't change the precendence
    my $parens = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
    $parens->{finish} = PPI::Token::Structure->new(')');
    $parent->__insert_after_child( $args[-1], $parens );
    for my $child (@args) {
        $parens->add_element( $child->remove );
    }
    $parens->add_element( PPI::Token::Whitespace->new(q{ }) );
    $element->{content} = 'is';
    $parens->add_element( $element->remove );
    $parens->add_element( PPI::Token::Whitespace->new(q{ }) );
    if ( $not eq 'not' or $not eq q{!} ) {
        $not->delete;
    }
    else {
        $parens->add_element( PPI::Token::Word->new('not') );
        $parens->add_element( PPI::Token::Whitespace->new(q{ }) );
    }
    $parens->add_element( PPI::Token::Word->new('None') );
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

sub map_do {
    my ($element) = @_;

    # map common slurp one-liner
    my $block    = $element->snext_sibling;
    my $operator = $element->sprevious_sibling;
    my $local =
      'local\s*[(]\s*\@ARGV,\s*\$/\s*[)]' ## no critic (RequireInterpolationOfMetachars)
      ;
    if (    $block =~ /[{]\s*$local\s*=\s*.+\s*;\s*<>\s*[}]/xsm
        and $operator eq q{=} )
    {
        my $argument = $operator->sprevious_sibling;
        my $op2      = $block->find_first(
            sub {
                $_[1]->isa('PPI::Token::Operator')
                  and $_[1]->content eq q{=};
            }
        );
        my $file = $op2->snext_sibling;
        my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
        $list->{finish} = PPI::Token::Structure->new(')');
        $list->add_element( $file->remove );
        map_element($file);
        $list->add_element( PPI::Token::Operator->new(q{,}) );
        $list->add_element( PPI::Token::Quote::Double->new('"r"') );
        $argument->insert_before( PPI::Token::Word->new('with') );
        $argument->insert_before( PPI::Token::Whitespace->new(q{ }) );
        $argument->insert_before( PPI::Token::Word->new('open') );
        $argument->insert_before($list);
        $argument->insert_before( PPI::Token::Whitespace->new(q{ }) );
        $argument->insert_before( PPI::Token::Word->new('as') );
        $argument->insert_before( PPI::Token::Whitespace->new(q{ }) );
        $element->{content} = 'fd';

        for my $child ( $block->children ) {
            $child->delete;
        }

        # wrap the contents of the block in a PPI::Statement so that is
        # correctly indented
        my $statement = PPI::Statement->new;
        $block->add_element($statement);
        $statement->add_element( $argument->remove );
        $statement->add_element( $operator->remove );
        $statement->add_element( PPI::Token::Word->new('fd.read()') );
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
    remove_trailing_semicolon($element);
    my $map_children = 1;
    my $ref          = ref $element;
    if ( $ref =~ /PPI::Token::Comment/xsm ) {
        if ( $element->line and $element->content =~ /^([#]!.*)perl/xsm ) {
            $element->{content} = $1 . "python3\n";
        }
    }
    elsif ( $ref =~ /PPI::Statement::End/xsm ) {
        while ( my $sibling = $element->next_sibling ) {
            $sibling->delete;
        }
        $element->delete;
    }
    elsif ( $ref =~ /PPI::Statement::Expression/xsm ) {
        map_expression($element);
    }
    elsif ( $ref =~ /PPI::Statement::Given/xsm ) {
        map_given($element);
    }
    elsif ( $ref =~ /PPI::Statement::Include/xsm ) {
        map_include($element);
        $map_children = 0;
    }
    elsif ( $ref =~ /PPI::Statement::Package/xsm ) {
        map_package($element);
    }
    elsif ( $ref =~ /PPI::Statement::Scheduled/xsm ) {
        my $block  = $element->find_first('PPI::Structure::Block');
        my $parent = $element->parent;
        for my $child ( $block->children ) {
            $parent->__insert_before_child( $element, $child->remove );
            map_element($child);
        }
        $element->delete;
    }
    elsif ( $ref =~ /PPI::Statement::Sub/xsm ) {
        map_sub($element);
    }
    elsif ( $ref =~ /PPI::Statement::Compound/xsm ) {
        map_compound($element);
    }
    elsif ( $ref =~ /PPI::Statement::Variable/xsm ) {
        map_variable($element);
    }
    elsif ( $ref =~ /PPI::Statement/xsm ) {
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
    elsif ( $ref =~ /PPI::Structure::Block/xsm ) {
        $element->{start}->{content}  = q{:};
        $element->{finish}->{content} = q{};
    }
    elsif ( $ref =~ /PPI::Structure::Subscript/xsm ) {
        map_subscript($element);
    }

    # $# -> len()
    elsif ( $ref =~ /PPI::Token::ArrayIndex/xsm ) {
        $element->insert_before( PPI::Token::Word->new('len') );
        my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
        $list->{finish} = PPI::Token::Structure->new(')');
        $element->insert_before($list);
        $list->add_element(
            PPI::Token::Word->new(
                substr $element->{content},
                2, length $element->{content}
            )
        );
        $list->insert_after( PPI::Token::Number->new(1) );
        $list->insert_after( PPI::Token::Operator->new(q{-}) );
        $element->delete;
    }
    elsif ( $ref =~ /PPI::Token::Cast/xsm ) {
        map_cast($element);
    }
    elsif ( $ref =~ /PPI::Token::Operator/xsm ) {
        map_operator($element);
    }
    elsif ( $ref =~ /PPI::Token::HereDoc/xsm ) {
        $element->insert_before(
            PPI::Token::Quote::Double->new(
                '"""' . join( q{}, @{ $element->{_heredoc} } ) . '"""'
            )
        );
        $element->delete;
    }
    elsif ( $ref =~ /PPI::Token::Label/xsm ) {
        map_label($element);
    }
    elsif ( $ref =~ /PPI::Token::Quote::Double/xsm ) {
        map_interpreted_string($element);
    }
    elsif ( $ref =~ /PPI::Token::Quote::Literal/xsm ) {
        map_quote($element);
    }
    elsif ( $ref =~ /PPI::Token::QuoteLike::Readline/xsm ) {
        map_readline($element);
    }
    elsif ( $ref =~ /PPI::Token::QuoteLike::Regexp/xsm ) {
        if ( $element->sprevious_sibling eq q{=~} ) {
            map_regex_match($element);
        }
        else {
            map_quote( $element, 'r' );
        }
    }
    elsif ( $ref =~ /PPI::Token::QuoteLike::Words/xsm ) {
        my $list = PPI::Structure::List->new( PPI::Token::Structure->new('[') );
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
    elsif ( $ref =~ /PPI::Token::Regexp::Match/xsm ) {
        map_regex_match($element);
    }
    elsif ( $ref =~ /PPI::Token::Regexp::Substitute/xsm ) {
        map_regex_substitute($element);
    }
    elsif ( $ref =~ /PPI::Token::Symbol/xsm ) {
        map_symbol($element);
    }
    elsif ( $ref =~ /PPI::Token::Word/xsm ) {
        map_word($element);
    }
    elsif ( $ref =~ /PPI::Token::Magic/xsm ) {
        $element = map_magic($element);
    }
    elsif ( $ref =~ /PPI::Token::Number/xsm ) {
        if ( $element->{content} =~ /^0+(\d+)$/xsm ) {
            $element->{content} = $1;
        }
    }
    if ( exists $element->{children} and $map_children ) {
        for my $child ( $element->children ) {
            try {
                map_element($child);
            }
            catch {
                my $mess =
                  "Error mapping: '$child' at line number $LINENUMBER\n";
                if ($DEBUG) {
                    croak $mess;
                }
                else {
                    warn $mess;
                }
            };
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
    # -> pytest.importorskip('Package')
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
            $compound->parent->__insert_before_child( $compound,
                PPI::Token::Word->new("pytest.importorskip('$module')") );
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
            $statement1->parent->__insert_before_child( $statement1,
                PPI::Token::Word->new("pytest.importorskip('$module')") );
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
                }
            }
            if ( not $list ) { $newline->delete }
        }
    }
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
    if (
            $expression->isa('PPI::Statement')
        and $parent->isa('PPI::Structure::List')
        and
        ( $prev->isa('PPI::Token::Word') or $prev->isa('PPI::Token::Symbol') )
      )
    {
        $element->{content} = q{=};
        my $property = $element->sprevious_sibling;

        # remove quotes from properties, mapping hyphens to underscores
        if ( $property->isa('PPI::Token::Quote') ) {
            my $word = PPI::Token::Word->new( quote2content($property) );
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
        make_path($directories);
    }
    open my $fh, '>', $outfile or croak "Error opening $outfile";
    print {$fh} $doc or croak "Error writing to $outfile";
    close $fh        or croak "Error closing $outfile";
    return;
}

sub map_file_temp {
    my ($element) = @_;
    $element->{content} = 'tempfile';
    my $method = $element->snext_sibling->snext_sibling;
    if ( $method eq 'new' ) {
        my $list = $method->snext_sibling;
        my ( $unlink, $dir, $suffix );
        if ($list) {
            $dir = find_first_isa_content( $list, 'PPI::Token::Word', 'DIR' );
            $suffix =
              find_first_isa_content( $list, 'PPI::Token::Word', 'SUFFIX' );
            $unlink =
              find_first_isa_content( $list, 'PPI::Token::Word', 'UNLINK' );
        }
        else {
            $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $list->{finish} = PPI::Token::Structure->new(')');
            $method->insert_after($list);
        }
        if ($unlink) {
            $method->{content} = 'NamedTemporaryFile';
            $unlink->{content} = 'delete';
        }
        else {
            $method->{content} = 'TemporaryFile';
        }
        if ($dir) {
            $dir->{content} = 'dir';
        }
        if ($suffix) {
            $suffix->{content} = 'suffix';
        }
    }
    return;
}

sub map_first_index {
    my ($element) = @_;
    $element->{content} = '.index';
    my $list   = map_built_in($element);
    my $method = $list->schild(0);
    my @magic  = $method->find('PPI::Token::Magic');
    if ( @magic and $magic[0] ne q{} ) {
        for ( @{ $magic[0] } ) {
            $_->delete;
        }
    }
    my @operators = $method->find('PPI::Token::Operator');
    if ( @operators and $operators[0] ne q{} ) {
        for ( @{ $operators[0] } ) {
            $_->delete;
        }
    }
    my $array = $list->schild(1);
    if ( $array eq q{@} and $list->schild(2)->isa('PPI::Structure::Block') ) {
        remove_cast( $array, $list->schild(2), $array->parent );
        $array = $list->schild(1);
    }

    $element->parent->__insert_before_child( $element, $array->remove );
    for ( $method->children ) {
        map_element($_);
        $list->add_element( $_->remove );
    }
    $method->delete;
    return;
}

# map getter to property
sub map_get {
    my ($element) = @_;
    my $list      = map_built_in($element);
    my $prev      = $element->sprevious_sibling;
    my $name      = $list->find_first('PPI::Token::Quote');
    if (    ( $prev eq '->' or $prev->{originally} eq '->' )
        and $name
        and $name->parent->children == 1 )
    {
        $name->{content} =~ s/["']//gsmx;    # remove the quotes
        $name->{content} =~ s/-/_/gsmx;      # dash to underscore
        $element->insert_before( $name->remove );
        $element->delete;
        $list->delete;
    }
    return;
}

sub map_given {
    my ($element) = @_;
    my $given     = $element->find_first('PPI::Structure::Given');
    my $block     = $element->find_first('PPI::Structure::Block');
    my $compound  = PPI::Statement::Compound->new;
    for my $when ( $block->children ) {
        if ( not $when->isa('PPI::Statement::When') ) { next }
        my $structure = $when->find_first('PPI::Structure::When');
        my $regex     = $when->find_first('PPI::Token::Regexp::Match');
        if ( not $structure ) {
            $compound->add_element( PPI::Token::Word->new('else') );
        }
        elsif ( $compound->children ) {
            $compound->add_element( PPI::Token::Word->new('elsif') );
        }
        else {
            $compound->add_element( PPI::Token::Word->new('if') );
        }
        $compound->add_element( PPI::Token::Whitespace->new(q{ }) );
        if ( $regex and $regex->parent->parent->parent eq $when ) {
            my @out = regex2search( $regex, $given->children );
            for my $out (@out) {
                $compound->add_element($out);
            }
        }
        elsif ($structure) {
            map_given_structure( $when, $given, $compound );
        }
        my $whenblock = $when->find_first('PPI::Structure::Block');
        if ( not $whenblock ) { return }
        $compound->add_element( $whenblock->remove );
    }
    $element->insert_before($compound);
    $element->delete;
    map_element($compound);
    return;
}

sub map_given_structure {
    my ( $when, $given, $compound ) = @_;
    my $expression = $when->find_first('PPI::Statement::Expression');
    my $list       = $when->find_first('PPI::Structure::Constructor');
    if ( $list and $list->parent->parent->sprevious_sibling eq 'when' ) {
        for my $match ( $given->children ) {
            $compound->add_element( $match->clone );
        }
        $compound->add_element( PPI::Token::Operator->new('in ') );
        $compound->add_element( $list->remove );
    }
    elsif ( $expression
        and $expression->find_first('PPI::Token::Operator') )
    {
        my @when_magic = $when->find(
            sub {
                $_[1]->isa('PPI::Token::Magic')
                  and $_[1]->content eq
                  '$_'    ## no critic (RequireInterpolationOfMetachars)
                  ;
            }
        );
        if ( @when_magic and $when_magic[0] ne q{} ) {
            for my $magic ( @{ $when_magic[0] } ) {
                for my $match ( $given->children ) {
                    $magic->parent->__insert_before_child( $magic,
                        $match->clone );
                }
                $magic->delete;
            }
        }
        $compound->add_element( $expression->remove );
    }
    else {
        for my $match ( $given->children ) {
            $compound->add_element( $match->clone );
        }
        $compound->add_element( PPI::Token::Operator->new(q{==}) );
        if ($expression) {
            $compound->add_element( $expression->remove );
        }
    }
    return;
}

sub map_gobject_signals {
    my ( $object, $first_child ) = @_;
    my $signals_def = $object->snext_sibling->snext_sibling;
    my $statement   = PPI::Statement::Variable->new;
    $first_child->parent->__insert_before_child( $first_child, $statement );
    $statement->add_element( PPI::Token::Word->new('__gsignals__') );
    $statement->add_element( PPI::Token::Operator->new(q{=}) );
    my $dict = PPI::Structure::List->new( PPI::Token::Structure->new('{') );
    $dict->{finish} = PPI::Token::Structure->new('}');
    $statement->add_element($dict);
    my $signals = $signals_def->schild(0);

    my @connections;
    while ($signals) {
        my $name = $signals->schild( my $isignal = 0 );
        if ( not $name ) { last }
        if ( not $name->isa('PPI::Token::Quote') ) {
            my $new_name =
              PPI::Token::Quote::Double->new( q{"} . $name . q{"} );
            $name->insert_before($new_name);
            $name->delete;
            $name = $new_name;
        }
        my $op1 = $signals->schild( ++$isignal );
        my $def = $signals->schild( ++$isignal );
        my $op2 = $signals->schild( ++$isignal );
        if ( $def eq q{\\} ) {
            my $connection = PPI::Statement->new;
            push @connections, $connection;
            $connection->add_element( PPI::Token::Word->new('self.connect') );
            my $connect_tuple =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $connect_tuple->{finish} = PPI::Token::Structure->new(')');
            $connection->add_element($connect_tuple);
            $connect_tuple->add_element( $name->remove );
            $connect_tuple->add_element( PPI::Token::Operator->new(q{,}) );
            $connect_tuple->add_element( $op2->remove );
            $op2->{content} =~ s/^&//xsm;
            $op2 = $def->snext_sibling;
            $op1->delete;
            $def->delete;

            if ($op2) {
                $op2->delete;
            }
            next;
        }
        $dict->add_element( $name->remove );
        $dict->add_element( PPI::Token::Operator->new(q{:}) );
        my $tuple =
          PPI::Structure::List->new( PPI::Token::Structure->new('(') );
        $tuple->{finish} = PPI::Token::Structure->new(')');
        $dict->add_element($tuple);
        $tuple->add_element(
            PPI::Token::Word->new('GObject.SignalFlags.RUN_FIRST') );
        $tuple->add_element( PPI::Token::Operator->new(q{,}) );
        $tuple->add_element( PPI::Token::Word->new('None') );
        $tuple->add_element( PPI::Token::Operator->new(q{,}) );
        $dict->add_element($tuple);
        my $def_expression = $def->schild(0);

        my $type_expression;
        while ($def_expression) {
            my $key = $def_expression->schild( my $idef = 0 );
            if ( not $key ) { last }
            my $op3        = $def_expression->schild( ++$idef );
            my $type_tuple = $def_expression->schild( ++$idef );
            my $op4        = $def_expression->schild( ++$idef );
            if ( $key eq 'param_types' ) {
                $type_expression = $type_tuple->schild(0);
                if ($type_expression) {
                    $type_tuple->{start}  = PPI::Token::Structure->new('(');
                    $type_tuple->{finish} = PPI::Token::Structure->new(')');
                    $tuple->add_element( $type_tuple->remove );
                    for my $type ( $type_expression->children ) {
                        map_gobject_signal_type($type);
                    }
                }
            }
            $key->delete;
            for my $op ( $op3, $op4 ) {
                if ($op) {
                    $op->delete;
                }
            }
        }
        if ( not $type_expression ) {
            $tuple->add_element( PPI::Token::Word->new('()') );
        }
        $op1->delete;
        $def->delete;
        if ($op2) {
            $op2->delete;
        }
        $dict->add_element( PPI::Token::Operator->new(q{,}) );
    }
    indent_element($statement);
    return $signals_def->snext_sibling, @connections;
}

sub map_gobject_signal_type {
    my ($type) = @_;
    if ( $type->isa('PPI::Token::Quote') ) {
        $type->{content} =~ s/Glib:://xsm;
        $type->{content} =~ s/UInt/Int/xsm;
        $type->{content} =~ s/String/str/xsm;
        $type->{content} =~ s/Scalar/object/xsm;
        $type->{content} = lc $type;
        $type->insert_before( PPI::Token::Word->new( quote2content($type) ) );
        $type->insert_before( PPI::Token::Operator->new(q{,}) );
    }
    $type->delete;
    return;
}

sub list_add_elements {
    my ( $list, @elements ) = @_;
    if (@elements) {
        for (@elements) {
            map_element($_);
            $list->add_element( $_->remove );
        }
    }
    return;
}

sub map_gobject_subclass {
    my ( $element, $import ) = @_;
    my $parent_package = $import->snext_sibling->snext_sibling;
    my $operator       = $parent_package->snext_sibling;
    $parent_package->{content} =~ s/::$//sm;
    $parent_package->{content} =~ s/::/./gsm;
    $parent_package->{content} =~ s/Glib/GObject/gsm;
    $parent_package->{content} =~ s/Gtk\d/Gtk/gsmx;
    my $document = $element->top;
    my $class    = $document->find_first(
        sub {
            $_[1]->isa('PPI::Token::Word')
              and $_[1]->content eq 'class';
        }
    );
    my $class_list = $class->snext_sibling->snext_sibling;
    $class_list->add_element( $parent_package->remove );
    my $block       = $class_list->snext_sibling;
    my $first_child = $block->schild(0);
    if ( not $first_child ) {
        $first_child = $block->child(0);
    }
    add_import( $element, 'gi.repository', 'GObject' );
    my @connections;
    if ($operator) {
        my $object = $operator->snext_sibling;
        while ($object) {
            if ( $object eq 'signals' ) {
                ( $object, @connections ) =
                  map_gobject_signals( $object, $first_child );
            }
            elsif ( $object eq 'properties' ) {
                my $prop_list = $object->snext_sibling->snext_sibling;
                for my $spec (
                    @{
                        $prop_list->find(
                            sub {
                                $_[1]->isa('PPI::Token::Word')
                                  and $_[1]->content eq 'Glib::ParamSpec';
                            }
                        )
                    }
                  )
                {
                    my $op    = $spec->snext_sibling;
                    my $type  = $op->snext_sibling;
                    my $struc = $type->snext_sibling;
                    my $expre = $struc->schild(0);
                    my $name  = $expre->schild(0);
                    my $nick  = $name->snext_sibling->snext_sibling;
                    my $blurb = $nick->snext_sibling->snext_sibling;
                    my ( @min, @max, @default );

                    my $content = $type->{content};
                    if ( $content eq 'boolean' ) {
                        @default =
                          get_argument_for_operator( $blurb->snext_sibling, 1 );
                        $type->{content} = 'bool';
                    }
                    elsif ( $content eq 'enum' ) {
                        my $enum = $blurb->snext_sibling->snext_sibling;
                        @default =
                          get_argument_for_operator( $enum->snext_sibling, 1 );
                        $type->{content} = 'GObject.GEnum';
                    }
                    elsif ( $content eq 'float' or $content eq 'int' ) {
                        @min =
                          get_argument_for_operator( $blurb->snext_sibling, 1 );
                        @max =
                          get_argument_for_operator( $min[-1]->snext_sibling,
                            1 );
                        @default =
                          get_argument_for_operator( $max[-1]->snext_sibling,
                            1 );
                    }
                    elsif ( $content eq 'scalar' ) {
                        $type->{content} = 'object';
                    }
                    elsif ( $content eq 'string' ) {
                        @default =
                          get_argument_for_operator( $blurb->snext_sibling, 1 );
                        $type->{content} = 'str';
                    }
                    my $statement = PPI::Statement::Variable->new;
                    $first_child->parent->__insert_before_child( $first_child,
                        $statement );
                    $name = quote2content($name);
                    $name =~ s/-/_/gxms;
                    $statement->add_element( PPI::Token::Word->new($name) );
                    $statement->add_element( PPI::Token::Operator->new(q{=}) );
                    $statement->add_element(
                        PPI::Token::Word->new('GObject.Property') );
                    my $list =
                      PPI::Structure::List->new(
                        PPI::Token::Structure->new('(') );
                    $list->{finish} = PPI::Token::Structure->new(')');
                    $statement->add_element($list);
                    $list->add_element( PPI::Token::Word->new('type=') );
                    $list->add_element( $type->remove );

                    if (@min) {
                        list_add_elements(
                            $list,
                            PPI::Token::Operator->new(q{,}),
                            PPI::Token::Word->new('minimum='), @min
                        );
                    }
                    if (@max) {
                        list_add_elements(
                            $list,
                            PPI::Token::Operator->new(q{,}),
                            PPI::Token::Word->new('maximum='), @max
                        );
                    }
                    if (@default) {
                        list_add_elements(
                            $list,
                            PPI::Token::Operator->new(q{,}),
                            PPI::Token::Word->new('default='), @default
                        );
                    }
                    if ( length $nick > 2 ) {
                        list_add_elements(
                            $list,
                            PPI::Token::Operator->new(q{,}),
                            PPI::Token::Word->new('nick='), $nick
                        );
                    }
                    if ( length $blurb > 2 ) {
                        list_add_elements(
                            $list,
                            PPI::Token::Operator->new(q{,}),
                            PPI::Token::Word->new('blurb='), $blurb
                        );
                    }
                    indent_element($statement);
                }
            }
            $object = $object->snext_sibling;
        }
    }
    my $init = PPI::Statement::Sub->new;
    $block->__insert_before_child( $first_child, $init );
    $init->add_element( PPI::Token::Word->new('def') );
    $init->add_element( PPI::Token::Whitespace->new(q{ }) );
    $init->add_element( PPI::Token::Word->new('__init__') );
    my $init_list =
      PPI::Structure::List->new( PPI::Token::Structure->new('(') );
    $init_list->{finish} = PPI::Token::Structure->new(')');
    $init->add_element($init_list);
    $init_list->add_element( PPI::Token::Word->new('self, *args, **kwargs') );
    my $init_block =
      PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
    $init_block->{start}->{content} = q{:};
    $init->add_element($init_block);
    my $statement = PPI::Statement->new;
    $init_block->add_element($statement);
    $statement->add_element(
        PPI::Token::Word->new('super().__init__(*args, **kwargs)') );
    indent_element($init);
    indent_element($statement);
    $element->delete;
    indent_element( $init->snext_sibling );

    for my $connection (@connections) {
        $init_block->add_element($connection);
        indent_element($connection);
    }
    return;
}

sub map_grep {
    my ($element)  = @_;
    my $expression = $element->snext_sibling;
    my $array      = $expression->snext_sibling;
    while ( $expression->isa('PPI::Structure::Block') ) {
        $expression = $expression->schild(0);
    }
    my $list = PPI::Structure::List->new( PPI::Token::Structure->new('[') );
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
        my @out = regex2search( $regex, PPI::Token::Symbol->new('x') );
        for my $out (@out) {
            $list->add_element($out);
        }
    }
    while ($element) {
        my $next = $element->next_sibling;
        $element->delete;
        $element = $next;
    }
    return;
}

sub map_image_magick {
    my ($element) = @_;
    $element->{content} = 'PythonMagick';
    my $method = $element->snext_sibling->snext_sibling;
    if ( $method eq 'new' ) {
        $method->{content} = 'Image';
        my $list = $method->snext_sibling;
        if ( not $list ) {
            $list =
              PPI::Structure::List->new( PPI::Token::Structure->new('(') );
            $list->{finish} = PPI::Token::Structure->new(')');
            $method->insert_after($list);
        }
    }
    return;
}

sub map_import_symbols {
    my ( $import, $path, $module, $symbols ) = @_;
    $import->{content} = 'from';
    $path->{content}   = $module;
    $symbols->insert_before( PPI::Token::Word->new('import') );
    $symbols->insert_before( PPI::Token::Whitespace->new(q{ }) );
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
        my $i = 0;
        for ( split q{ }, $string ) {
            if ( $i++ ) {
                push @symbols, PPI::Token::Whitespace->new(q{,});
                $symbols->insert_before( $symbols[-1] );
            }
            push @symbols, PPI::Token::Word->new($_);
            $symbols->insert_before( $symbols[-1] );
        }
        $symbols->delete;
    }
    return @symbols;
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

sub map_include {
    my ($element) = @_;
    my $module    = $element->module;
    my $import    = $element->schild(0);
    my $top       = $element->top;
    if ( $module =~ /(?:$IGNORED_INCLUDES|^$)/xsm ) {
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
    elsif ( $module eq 'Glib::Object::Subclass' ) {
        map_gobject_subclass( $element, $import );
        return;
    }
    elsif ( $module eq 'Test::More' ) {
        my $def = PPI::Statement::Sub->new;
        $def->add_element( PPI::Token::Word->new("\ndef") );
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
    else {
        ensure_include_is_top_level( $element, $top );
    }
    $module =~ s/::/./gsm;

    if ( $import ne 'use' ) {
        croak "Unrecognised include $element\n";
    }
    my $path    = $import->snext_sibling;
    my $symbols = $path->snext_sibling;
    my $pathstr = "$path";
    if ( $pathstr eq 'Cairo' ) {
        $module = 'cairo';
    }
    elsif ( $pathstr eq 'Data::UUID' ) {
        $module = 'uuid';
    }
    elsif ( $pathstr eq 'Date::Calc' ) {
        $module = 'datetime';
        $symbols->delete;
        undef $symbols;
    }
    elsif ( $pathstr eq 'Encode' ) {
        $element->delete;
    }
    elsif ( $pathstr eq 'File::Temp' ) {
        $module = 'tempfile';
    }
    elsif ( $pathstr eq 'File::stat' ) {
        $module = 'os';
    }
    elsif ( $pathstr eq 'Glib' ) {
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
    elsif ( $pathstr eq 'Gtk3' ) {
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
    elsif ( $pathstr eq 'Image::Magick' ) {
        $module = 'PythonMagick';
    }
    elsif ( $pathstr eq 'Image::Sane' ) {
        delete_everything_after($path);
        $module = 'sane';
        $symbols->delete;
        undef $symbols;
    }
    elsif ( $pathstr eq 'Locale::gettext' ) {
        delete_everything_after($path);
        $module = 'gettext';
        if ($symbols) { $symbols->delete }
        undef $symbols;
    }
    elsif ( $pathstr eq 'Log::Log4perl' ) {
        delete_everything_after($path);
        $module = 'logging';
        $symbols->delete;
        undef $symbols;
    }
    elsif ( $pathstr eq 'Set::IntSpan' ) {
        delete_everything_after($path);
        $element->add_element( PPI::Token::Whitespace->new(q{ }) );
        $symbols = PPI::Token::Quote::Double->new('"intspan"');
        $element->add_element($symbols);
        $module = 'intspan';
    }
    elsif ( $pathstr eq 'Storable' ) {
        if ( $symbols->{content} =~ s/dclone/deepcopy/xsm ) {
            $symbols->{sections}[0]{size} += 2;
            $module = 'copy';
        }
    }
    elsif ( $pathstr eq 'Thread::Queue' ) {
        $module = 'queue';
    }
    elsif ( $pathstr eq 'base' ) {
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
        my $list    = $class->schild(0)->snext_sibling->snext_sibling;
        my @symbols = map_import_symbols( $import, $path, $module, $symbols );
        if ( $symbols[0] eq 'Exporter' ) {
            shift @symbols;
            if ( @symbols > 0 ) {
                shift @symbols;
            }
        }
        for my $symbol (@symbols) {
            $symbol->{content} =~ s/.*:://xsm;
            map_element($symbol);
            $list->add_element( $symbol->remove );
        }
        $element->delete;
        return;
    }
    elsif ( $pathstr eq 'threads' ) {
        $module = 'threading';
    }
    elsif ( $pathstr eq 'threads::shared' ) {
        $element->delete;
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

sub map_inc_dec {
    my ($element) = @_;
    my @previous  = get_argument_for_operator( $element, 0 );
    my @next      = get_argument_for_operator( $element, 1 );
    if ( $element eq q{++} ) {
        $element->{content} = q{+=};
    }
    else {
        $element->{content} = q{-=};
    }
    my $parent  = $element->parent;
    my $gparent = $parent;
    while ( $gparent and not $gparent->isa('PPI::Statement') ) {
        $gparent = $gparent->parent;
    }
    my @operand;
    if (@previous) {
        if ( not $previous[-1]->isa('PPI::Token::Operator') ) {
            @operand = @previous;
        }
        else {
            @operand = $element;
        }
    }
    if ( "$gparent" ne "$parent"
        or ( @operand and $operand[0]->sprevious_sibling ) )
    {
        my $statement = PPI::Statement->new;
        $gparent->insert_after($statement);
        for my $item ( @previous, @next ) {
            $statement->add_element( $item->clone );
        }
        $statement->add_element( $element->remove );
        $statement->add_element( PPI::Token::Number->new(1) );
        indent_element($statement);
        return;
    }
    if (@next) {
        for my $next (@next) {
            $element->insert_before( $next->remove );
        }
    }
    $element->insert_after( PPI::Token::Number->new(1) );
    return;
}

sub map_is {
    my ($element) = @_;
    my $next = $element->snext_sibling;

    # assert doesn't use parens, so remove any
    if ( $next->isa('PPI::Structure::List') ) {
        remove_parens_map_children($next);
        $next = $element->snext_sibling;
        $element->insert_after( PPI::Token::Whitespace->new(q{ }) );
    }

    # ignore 'is' created by previously mapping defined
    if ( $next =~ /(None|not)/xsm ) {
        return;
    }

    # we've got a unit test - map to assert
    my @result = get_argument_for_operator( $element, 1 );
    my $operator;
    for (@result) {
        if ( $_ eq q{,} ) {
            $operator = $_;
            last;
        }
    }
    if ( not defined $operator ) {
        $operator = $result[-1]->snext_sibling;
    }
    if ( $element eq 'isnt' ) {
        $operator->{content} = q{!=};
    }
    else {
        $operator->{content} = q{==};
    }
    $element->{content} = 'assert';
    return;
}

sub map_isa {
    my ($element) = @_;
    my $list      = map_built_in($element);
    my $prev      = $element->sprevious_sibling;
    my $obj       = $prev;
    if ( $prev eq '->' or $prev->{originally} eq '->' ) {
        $obj = $prev->sprevious_sibling;
        $prev->delete;
    }
    $element->{content} = 'issubclass';
    my $class = $list->find_first('PPI::Token::Quote');
    $class->{content} =~ s/["']//gsmx;    # remove the quotes
    $class->{content} =~ s/^.*:://smx;    # remove the class hierachy
    $list->__insert_before_child( $class, PPI::Token::Operator->new(q{,}) );
    $list->__insert_before_child( $class, $obj->remove );
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
    if ( defined $element->{content} and $element =~ /\n/xsm ) {
        $element->{content} = q{""} . $element->{content} . q{""};
    }
    if ($formatted) {
        $element->{content} = "f$element->{content}";
    }
    return;
}

sub map_join {
    my ($element) = @_;
    my $prev = $element->sprevious_sibling;
    if ( $prev eq q{.} ) { return }
    $element->{content} = '.join';
    my $list   = map_built_in($element);
    my $string = $list->schild(0);
    if ( $string->isa('PPI::Statement::Expression') ) {
        $string = $string->schild(0);
        map_element($string);
    }
    my $operator = $string->snext_sibling;
    $element->insert_before( $string->remove );
    $operator->delete;
    return;
}

sub map_label {
    my ($element) = @_;
    if ( $element eq 'SKIP:' ) {
        my $block         = $element->snext_sibling;
        my $skipstatement = $block->schild(0);
        my $skip          = $skipstatement->schild(0);
        my $skipmessage   = $skip->snext_sibling;
        my $opr           = $skipmessage->snext_sibling;
        my $count         = $opr->snext_sibling;
        my $ifu           = $count->snext_sibling;
        if ( $ifu =~ /(?:if|unless)/xsm ) {
            my @expression;
            while ( my $token = $ifu->snext_sibling ) {
                if ( $token eq q{;} ) {
                    last;
                }
                else {
                    push @expression, $token->remove;
                }

            }
            $element->insert_before( $ifu->remove );
            $element->insert_before( PPI::Token::Word->new(' not ') );
            if ( @expression == 1 ) {
                $element->insert_before( $expression[0] );
            }
            else {
                my $list =
                  PPI::Structure::List->new( PPI::Token::Structure->new('(') );
                $list->{finish} = PPI::Token::Structure->new(')');
                for my $token (@expression) {
                    $list->add_element($token);
                }
                $element->insert_before($list);
            }
            for my $token (@expression) {
                map_element($token);
            }
        }
        else {
            $element->insert_before( PPI::Token::Word->new('if False') );
        }
        $element->delete;
        $skipstatement->delete;
    }
    return;
}

sub map_like {
    my ($element) = @_;
    add_import( $element, 're' );
    $element->{content} = 'assert re.search';
    my $list = map_built_in($element);
    $list->insert_after( PPI::Token::Word->new(' is not None') );
    my $comma = $list->find(
        sub {
            $_[1]->isa('PPI::Token::Operator')
              and $_[1]->content eq q{,};
        }
    );
    if ( @{$comma} > 1 ) {
        my $regex  = $comma->[1]->previous_sibling;
        my $parent = $element->parent;
        while ( my $item = $regex->next_sibling ) {
            $parent->add_element( $item->remove );
        }
    }
    $list->add_element( PPI::Token::Operator->new(q{,}) );
    while ( my $item = $comma->[0]->previous_sibling ) {
        $list->add_element( $item->remove );
    }
    $comma->[0]->delete;
    return;
}

sub map_magic {
    my ($element) = @_;

    # magic defined in sub - add to def arguments
    if ( $element eq '@_' ) {    ## no critic (RequireInterpolationOfMetachars)
        my $expression = $element->parent;
        my $sub        = $expression->parent;
        while ( $sub and not $sub->isa('PPI::Statement::Sub') ) {
            $sub = $sub->parent;
        }
        if ( not $sub ) {
            warn "Error parsing magic in $sub\n";
            return;
        }
        my $dest_list = $sub->find_first('PPI::Structure::List');

        # sub argument definition
        if (    $expression->isa('PPI::Statement')
            and $sub->isa('PPI::Statement::Sub') )
        {
            my $source_list = $expression->find_first('PPI::Structure::List');
            if ($source_list) {

                # new() subs pick up the class name as the first argument.
                # python requires here self
                my $subname = $sub->schild(1);
                if ( $subname eq '__init__' or $subname =~ /^new/xsm ) {
                    my $class = $source_list->schild(0)->schild(0);
                    $class->{content} = 'self';
                }

                for my $child ( $source_list->children ) {
                    $dest_list->add_element( $child->remove );
                }
                $expression->delete;
                map_element($dest_list);
                return;
            }
            my $operator = $expression->find_first('PPI::Token::Operator');
            if ( $operator eq q{=} ) {
                my @array = get_argument_for_operator( $operator, 0 );
                for my $item (@array) {
                    $dest_list->add_element( $item->remove );
                }
                $expression->delete;
                return;
            }
        }

        # sub argument usage
        $element->{content} = '*argv';
        $dest_list->add_element( PPI::Token::Symbol->new('*argv') );
    }

    # magic defined in regex capture, move capture out of condition
    # and use it to fetch group
    elsif ( $element =~ /^\$(\d+)$/xsm ) {
        $element = map_regex_group( $element, $1 );
    }
    else {
        map_symbol($element);
    }
    return $element;
}

sub map_map {
    my ($element) = @_;
    my $list      = map_built_in($element);
    my $block     = $list->schild(0);
    if ( $block->isa('PPI::Structure::Block') ) {
        $block->insert_before( PPI::Token::Word->new('lambda x: ') );
        $block->insert_after( PPI::Token::Operator->new(q{,}) );
        my $magic = $block->find('PPI::Token::Magic');
        if ($magic) {
            for ( @{$magic} ) {
                $_->{content} = 'x';
            }
        }
        for my $child ( $block->children ) {
            if ( $child->isa('PPI::Statement') ) {
                for my $gchild ( $child->children ) {
                    $list->__insert_before_child( $block, $gchild->remove );
                }
            }
            else {
                $list->__insert_before_child( $block, $child->remove );
            }
        }
        map_element($list);
        $block->delete;
    }
    return;
}

sub map_modifiers {
    my ($element) = @_;
    my @flags;
    for my $modifier ( sort keys %{ $element->{modifiers} } ) {
        if ( defined $REGEX_MODIFIERS{$modifier} ) {
            if ( $REGEX_MODIFIERS{$modifier} ) {
                push @flags, $REGEX_MODIFIERS{$modifier};
            }
        }
        else {
            carp "Unknown regex modifier '$modifier'";
        }
    }
    if (@flags) {
        return PPI::Token::Symbol->new( join q{|}, @flags );
    }
    return;
}

sub map_move_copy {
    my ($element) = @_;
    my $list      = map_built_in($element);
    my $module    = 'os';
    my $method    = 'os.rename';
    if ( $element eq 'copy' ) {
        $module = 'shutil';
        $method = 'shutil.copy2';
    }
    add_import( $element, $module );
    $element->{content} = $method;
    if ( not $list->find_first('PPI::Token::Operator') ) {
        my $op   = $list->snext_sibling;
        my @dest = get_argument_for_operator( $op, 1 );
        $list->add_element( $op->remove );
        for my $arg (@dest) {
            $list->add_element( $arg->remove );
        }

        # repeat map_built_in call to catch return value
        $list = map_built_in($element);
    }
    return;
}

# map class->new() -> class()
sub map_new {
    my ($element) = @_;
    map_built_in($element);
    my $op    = $element->sprevious_sibling;
    my $class = $op->sprevious_sibling;
    if ( $class eq '_class' ) {
        $class->{content} = '__class__';
    }
    $op->delete;
    $element->delete;
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

sub map_ok {
    my ($element) = @_;
    my $next = $element->snext_sibling;

    # assert doesn't use parens, so remove any
    if ( $next->isa('PPI::Structure::List') ) {
        remove_parens_map_children($next);
        $next = $element->snext_sibling;
        $element->insert_after( PPI::Token::Whitespace->new(q{ }) );
    }

    # we've got a unit test - map to assert
    $element->{content} = 'assert';
    return;
}

sub map_open {
    my ($element) = @_;
    my $list      = map_built_in($element);
    my $previous  = $element->sprevious_sibling;
    if ( $previous and $previous eq q{.} ) { return }    # not built-in
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
    return;
}

sub map_operator {
    my ($element) = @_;
    if ( not $element->{content} ) { return }
    $element->{originally} = $element->{content};
    my $elementstr = "$element";
    if ( $elementstr eq q{?} ) {
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
    elsif ( $elementstr eq q{||} ) {
        map_or($element);
    }
    elsif ( $elementstr eq q{&&} ) {
        $element->{content} = 'and';
    }
    elsif ( $elementstr eq q{.=} ) {
        $element->{content} = q{+=};
    }
    elsif ( $elementstr =~ /^(?:[+][+]|--)$/xsm ) {
        map_inc_dec($element);
    }
    elsif ( $elementstr eq q{.} ) {
        $element->{content} = q{+};
    }
    elsif ( $elementstr eq q{..} ) {
        my @start = get_argument_for_operator( $element, 0 );
        my @stop  = get_argument_for_operator( $element, 1 );
        $start[0]->insert_before( PPI::Token::Word->new('range') );
        my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
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
    elsif ( $elementstr eq q{!} ) {
        $element->{content} = q{not};
    }
    elsif ( $elementstr eq q{=>} ) {
        map_fat_comma($element);
    }
    elsif ( $elementstr =~ /^[=!]~$/xsm ) {

        # Most of the logic depends on the regex, so handle it there
        my $next = $element->snext_sibling;
        if ( $next->isa('PPI::Token::Symbol') ) {
            map_regex_match($next);
        }
    }
    elsif ( $elementstr eq q{->} ) {
        map_arrow_operator($element);
    }
    elsif ( $elementstr =~ /^-[efrs]$/xsm ) {
        my $parent = $element->parent;
        my $list   = map_built_in($element);
        if ( $element eq '-e' ) {
            add_import( $element, 'pathlib' );
            $element->{content} = 'pathlib.Path';
            $list->insert_after( PPI::Token::Word->new('.exists()') );
        }
        if ( $element eq '-f' ) {
            add_import( $element, 'os' );
            $element->{content} = 'os.path.isfile';
        }
        elsif ( $element eq '-r' ) {
            add_import( $element, 'os' );
            $element->{content} = 'os.access';
            $list->add_element( PPI::Token::Operator->new(q{,}) );
            $list->add_element( PPI::Token::Word->new('os.R_OK') );
        }
        elsif ( $element eq '-s' ) {
            add_import( $element, 'os' );
            $element->{content} = 'os.path.getsize';
        }
    }
    elsif ( $elementstr eq 'eq' ) {
        $element->{content} = q{==};
    }
    elsif ( $elementstr eq 'ge' ) {
        $element->{content} = q{>=};
    }
    elsif ( $elementstr eq 'gt' ) {
        $element->{content} = q{>};
    }
    elsif ( $elementstr eq 'le' ) {
        $element->{content} = q{<=};
    }
    elsif ( $elementstr eq 'lt' ) {
        $element->{content} = q{<};
    }
    elsif ( $elementstr eq 'ne' ) {
        $element->{content} = q{!=};
    }
    elsif ( $elementstr eq 'xor' ) {
        $element->{content} = q{^};
    }
    return;
}

sub map_or {
    my ($element) = @_;
    my $parent = $element->parent;

    # defined-or
    if (
        $parent->find_first(
            sub {
                $_[1]->isa('PPI::Token::Operator')
                  and $_[1]->content eq q{=};
            }
        )
      )
    {
        my @defined = get_argument_for_operator( $element, 0 );
        $element->{content} = "if @defined is not None else";
    }
    else {
        $element->{content} = 'or';
    }
    return;
}

sub map_package {
    my ($element) = @_;
    my $new;

    # Look for use Glib::Object::Subclass
    if (
        $element->top->find_first(
            sub {
                $_[1]->isa('PPI::Statement::Include')
                  and $_[1]->content =~ /Glib::Object::Subclass/xsm;
            }
        )
      )
    {
        $new = 1;
    }
    elsif (
        my $base = $element->top->find_first(
            sub {
                $_[1]->isa('PPI::Statement::Include')
                  and $_[1]->content =~ /base/xsm
                  and $_[1]->content !~ /[(]Exporter[)]/xsm;
            }
        )
      )
    {
        $new = 1;
    }
    else {

        # Look for new or INIT_INSTANCE methods
        my $subs = $element->parent->find('PPI::Statement::Sub');
        if ($subs) {
            for my $sub ( @{$subs} ) {
                if ( $sub->schild(1) =~ /(?:new|INIT_INSTANCE)/xsm ) {
                    $new = 1;
                    last;
                }
            }
        }
    }

    if ($new) {
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
        my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
        $list->{finish} = PPI::Token::Structure->new(')');
        $name->insert_after($list);

        # Add a block to scope and indent things properly
        my $block =
          PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
        $block->{start}->{content} = q{:};
        $class->add_element($block);
        my $next;
        while ( ( $next = $class->next_sibling )
            and not $next->isa('PPI::Statement::Package') )
        {
            if (
                not $next->isa('PPI::Statement::Sub') or $next->find_first(
                    sub {
                        $_[1]->isa('PPI::Token::Symbol')
                          and $_[1]->content =~
                          '^[$](?:self|class)$' ## no critic (RequireInterpolationOfMetachars)
                          ;
                    }
                )
              )
            {
                $block->add_element( $next->remove );
            }
            else {
                $class = $next;
            }
        }
    }
    $element->delete;
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

sub map_push {
    my ($element) = @_;
    my $list = map_built_in($element);
    map_element($list);
    my @argument = get_argument_from_list( $list, 0 );
    my $parent   = $element->parent;
    for my $arg (@argument) {
        $parent->__insert_before_child( $element, $arg->remove );    # array
    }
    my $operator = $list->schild(0);
    $operator->{content} = q{.};
    $element->insert_before( $operator->remove );
    if ( $element eq 'push' ) {
        $element->{content} = 'append';
    }
    else {
        $element->{content} = 'insert';
        my $child = $list->schild(0);
        $child->insert_before( PPI::Token::Number->new(0) );
        $child->insert_before( PPI::Token::Operator->new(q{,}) );
    }
    return;
}

sub map_quote {
    my ( $element, $prefix ) = @_;
    if ( not defined $prefix ) { $prefix = q{} }
    my $content = substr
      $element->content,
      $element->{sections}[0]{position},
      $element->{sections}[0]{size};
    $content =~ s/"/\\"/xsmg;
    my $quote = q{"};
    if ( $content =~ /\n/xsm ) {
        $quote = '"""';
    }
    $element->insert_after(
        PPI::Token::Quote::Double->new( $prefix . $quote . $content . $quote )
    );
    $element->delete;
    return;
}

sub map_readline {
    my ($element) = @_;
    my $parent = $element->parent;

    # glob
    if ( $element =~ /<.*[*].*>/xsm ) {
        my $prev_op = $element->sprevious_sibling;
        my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
        $list->{finish} = PPI::Token::Structure->new(')');
        $parent->__insert_before_child( $element,
            PPI::Token::Word->new('glob.glob'), $list );
        my $string = substr $element->content,
          $element->{sections}[0]{position},
          $element->{sections}[0]{size};
        my $quote = q{"};
        if ( $string =~ s/\$(\w+)/{$1}/xsm ) {
            $quote = 'f"';
        }
        $list->add_element(
            PPI::Token::Quote::Double->new( $quote . $string . q{"} ) );
        if ( $prev_op and $prev_op eq q{,} ) {
            my $prev_arg = $prev_op->sprevious_sibling;
            my $arg_list =
              PPI::Structure::List->new( PPI::Token::Structure->new('[') );
            $arg_list->{finish} = PPI::Token::Structure->new(']');
            $prev_arg->insert_before($arg_list);
            $arg_list->add_element( $prev_arg->remove );
            $prev_op->{content} = q{+};
        }
        add_import( $element, 'glob' );
        $element->delete;
    }

    # $line = <$fh>
    elsif ( $element =~ /<\$(\w+)>/xsm ) {

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
    return;
}

sub map_regex_group {
    my ( $element, $group ) = @_;
    my $scope = $element->parent;

    # split + regex capture group test otherwise falls over here
    if ( not $scope ) { return }

    my $search;
    while (
        not $search = $scope->find_first(
            sub {
                $_[1]->isa('PPI::Token::Word')
                  and $_[1]->content eq 're.search';
            }
        )
      )
    {
        # if we are in a block belonging to a if/then/else or while,
        # then before trying the parent, look for the condition
        if (    $scope->isa('PPI::Structure::Block')
            and $scope->parent->isa('PPI::Statement::Compound') )
        {
            $scope = $scope->sprevious_sibling;
            if ( $scope eq 'else' ) {
                $scope = $scope->sprevious_sibling->sprevious_sibling;
            }
        }
        else {
            $scope = $scope->parent;
        }
    }
    my $i = q{};
    if (    $search
        and $scope->isa('PPI::Statement::Expression')
        and $scope->sprevious_sibling =~ /(?:if|elif|while)/xsm )
    {
        my $compound  = $scope->parent;
        my $list      = $search->snext_sibling;
        my $regex_var = PPI::Statement::Variable->new;
        my $parent    = $scope->parent;
        while (
            $parent->find_first(
                sub {
                    $_[1]->isa('PPI::Token::Symbol')
                      and $_[1]->content eq "regex$i";
                }
            )
          )
        {
            if ( $i eq q{} ) {
                $i = 1;
            }
            $i++;
        }
        $REGEX = $i;
        $regex_var->add_element( PPI::Token::Symbol->new("regex$i") );
        $regex_var->add_element( PPI::Token::Operator->new(q{=}) );
        $compound->insert_before($regex_var);
        $search->insert_before( PPI::Token::Symbol->new("regex$i") );
        $regex_var->add_element( $search->remove );
        $regex_var->add_element( $list->remove );
        $compound->insert_before( PPI::Token::Whitespace->new("\n") );
        indent_element($regex_var);
    }
    elsif ( defined $REGEX ) {
        $i = $REGEX;
    }

    # replace the magic with the regex group
    my $new = PPI::Token::Word->new("regex$i.group($group)");
    $element->insert_before($new);
    $element->delete;
    return $new;
}

sub map_regex_match {
    my ($element) = @_;

    # in perl, the parent is a PPI::Statement::Expression, which we now
    # turn into re.search(regex, string, flags)
    if ( not $element->parent ) { return }
    my @argument;
    my $method   = defined $element->{modifiers}{g} ? 'findall' : 'search';
    my $operator = $element->sprevious_sibling;
    my $list     = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
    $list->{finish} = PPI::Token::Structure->new(')');
    $element->insert_after($list);
    $element->insert_after( PPI::Token::Word->new("re.$method") );

    if ( not $operator ) {
        $operator = PPI::Token::Operator->new(q{=~});
        push @argument, PPI::Token::Symbol->new('_');
    }
    elsif ( not $operator->isa('PPI::Token::Operator') ) {
        carp
"Expected operator before '$element' regex match. Found '$operator'\n";
        return;
    }
    else {
        @argument = get_argument_for_operator( $operator, 0 );
        if ( not @argument or not $argument[0] ) {
            croak "Argument for operator '$operator' not found\n";
        }
    }

    if ( $element->isa('PPI::Token::Symbol') ) {
        my $new = $element->clone;
        map_element($new);
        $list->add_element($new);
    }
    else {
        $list->add_element( regex2quote($element) );
    }

    if ( $operator eq q{!~} ) {
        $element->insert_before( PPI::Token::Word->new('not'),
            PPI::Token::Whitespace->new(q{ }) );
    }
    elsif ( $operator ne q{=~} ) {
        $operator = PPI::Token::Operator->new(q{=~});
        @argument = ( PPI::Token::Symbol->new('_') );
    }
    my $expression_operator = $argument[0]->sprevious_sibling;
    $operator->delete;
    $list->add_element( PPI::Token::Operator->new(q{,}) );
    for my $argument (@argument) {
        $list->add_element( $argument->remove );
    }

    # map () = -> len()
    if ( defined $element->{modifiers}{g} ) {
        my $expression_arg;
        if ( $expression_operator eq q{=} ) {
            $expression_arg = $expression_operator->sprevious_sibling;
        }
        if ( $expression_operator eq q{=} ) {
            if ( $expression_arg and $expression_arg =~ /[(]\s*[)]/xsm ) {
                my $lenlist =
                  PPI::Structure::List->new( PPI::Token::Structure->new('(') );
                $lenlist->{finish} = PPI::Token::Structure->new(')');
                $element->insert_before( PPI::Token::Word->new('len') );
                $element->insert_before($lenlist);
                while ( my $child = $element->snext_sibling ) {
                    $lenlist->add_element( $child->remove );
                }
                $expression_operator->delete;
                $expression_arg->delete;
            }
        }
        delete $element->{modifiers}{g};
    }
    my $flags = map_modifiers($element);
    if ($flags) {
        $list->add_element( PPI::Token::Operator->new(q{,}) );
        $list->add_element($flags);
    }

    # ensure we have import re
    add_import( $element, 're' );
    $element->delete;
    return;
}

sub map_regex_substitute {
    my ($element) = @_;
    add_import( $element, 're' );
    my $operator = $element->sprevious_sibling;
    my @target;
    if ( not $operator ) {
        $operator = PPI::Token::Operator->new(q{=~});
        $element->insert_before($operator);
        push @target, PPI::Token::Symbol->new('_');
        $operator->insert_before( $target[0] );
    }
    elsif ( $operator ne q{=~} ) {
        warn
          "Unexpected operator '$operator'. Expected '=~' for substitution\n";
        return;
    }
    else {
        @target = get_argument_for_operator( $operator, 0 );
    }
    $operator->{content} = q{=};
    $element->insert_before( PPI::Token::Word->new('re.sub') );
    my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
    $list->{finish} = PPI::Token::Structure->new(')');
    $element->insert_after($list);
    $list->add_element( regex2quote( $element, 0 ) );
    $list->add_element( PPI::Token::Operator->new(q{,}) );

    if ( defined $element->{modifiers}{e} ) {
        my $block =
          PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
        $block->{finish} = PPI::Token::Structure->new('}');
        my $section = 'return ' . substr $element->content,
          $element->{sections}[1]{position}, $element->{sections}[1]{size};
        my $subdocument = PPI::Document->new( \$section );

        my $magic = $subdocument->find('PPI::Token::Magic');
        if ($magic) {
            for my $child ( @{$magic} ) {
                if ( $child =~ /^\$(\d+)$/xsm ) {
                    my $parent        = $child->parent;
                    my $magicstring   = "\$match{$1}";
                    my $magicdocument = PPI::Document->new( \$magicstring );
                    $parent->__insert_before_child( $child,
                        $magicdocument->schild(0)->remove );
                    $child->delete;
                }
            }
        }

        $block->add_element( $subdocument->schild(0)->remove );
        my $name = add_anonymous_method( $element, $block,
            PPI::Token::Word->new('match') );
        $list->add_element( PPI::Token::Word->new($name) );
        delete $element->{modifiers}{e};
    }
    else {
        $list->add_element( regex2quote( $element, 1 ) );
    }
    $list->add_element( PPI::Token::Operator->new(q{,}) );
    for my $target (@target) {
        $list->add_element( PPI::Token::Word->new("$target") )
          ;    #stringify to get all children
    }
    if ( defined $element->{modifiers}{g} ) {
        delete $element->{modifiers}{g};
    }
    else {
        $list->add_element( PPI::Token::Operator->new(q{,}) );
        $list->add_element( PPI::Token::Word->new('count=1') );
    }
    my $flags = map_modifiers($element);
    if ($flags) {
        $list->add_element( PPI::Token::Operator->new(q{,}) );
        $list->add_element( PPI::Token::Word->new('flags=') );
        $list->add_element($flags);
    }
    $element->delete;
    return;
}

sub map_scalar {
    my ($element) = @_;
    my $list      = map_built_in($element);
    my $arg       = $list->schild(0);
    my $parent    = $element->parent;
    if ( $arg->isa('PPI::Statement::Expression') ) {
        $parent->__insert_before_child( $element, $arg->remove );
        map_element($arg);
        $element->delete;
        $list->delete;
    }
    else {
        $element->{content} = 'len';
    }
    return;
}

sub map_setlocale {
    my ($element) = @_;
    add_import( $element, 'locale' );
    $element->{content} = 'locale.' . $element->{content};
    my $list       = map_built_in($element);
    my $expression = $list->schild(0);
    if ($expression) {
        my $arg = $expression->schild(0);
        if ( $expression and $expression =~ /^LC_/xsm ) {
            $arg->{content} = 'locale.' . $arg->{content};
        }
    }
    return;
}

sub map_splice {
    my ($element) = @_;
    $element->{content} = 'del';
    my $list = map_built_in($element);
    map_element($list);
    my $subscript =
      PPI::Structure::Subscript->new( PPI::Token::Structure->new('[') );
    $subscript->{finish} = PPI::Token::Structure->new(']');
    my @commas = @{
        $list->find(
            sub {
                $_[1]->isa('PPI::Token::Operator')
                  and $_[1]->content eq q{,};
            }
        )
    };
    $commas[0]->insert_before($subscript);
    my @start = get_argument_for_operator( $commas[0], 1 );
    $commas[0]->delete;
    for my $child (@start) {
        $subscript->add_element( $child->remove );
    }
    my @count         = get_argument_for_operator( $commas[1], 1 );
    my $count_numeric = 1;
    for (@count) {
        if ( not $_->isa('PPI::Token::Number') ) { $count_numeric = 0 }
    }
    if ( @commas > 1 ) {
        if ( not $count_numeric or int("@count") > 1 ) {
            $subscript->add_element( $commas[1]->remove );
            $commas[1]->{content} = q{:};
            for my $child (@start) {
                $subscript->add_element( $child->clone );
            }
            $subscript->add_element( PPI::Token::Operator->new(q{+}) );
            for my $child (@count) {
                $subscript->add_element( $child->remove );
            }
        }
        else {
            $commas[1]->delete;
            for my $child (@count) {
                $child->delete;
            }
        }
    }
    if ( @commas > 2 ) {
        my @replacement = get_argument_for_operator( $commas[2], 1 );
        my $parent      = $element->parent;
        my ( @array, $item );
        my $i = 0;
        $item = $list->child($i);
        while ( $item and not $item->isa('PPI::Structure::Subscript') ) {
            push @array, $item->clone;
            $item = $list->child( ++$i );
        }
        my $insert = PPI::Statement->new;
        $parent->insert_after($insert);
        $parent->insert_after( PPI::Token::Whitespace->new("\n") );
        for my $child (@array) {
            $insert->add_element( $child->clone );
        }
        $insert->add_element( PPI::Token::Word->new('.insert') );
        my $list2 =
          PPI::Structure::List->new( PPI::Token::Structure->new('(') );
        $list2->{finish} = PPI::Token::Structure->new(')');
        $insert->add_element($list2);
        for my $child (@start) {
            $list2->add_element( $child->clone );
        }
        $list2->add_element( $commas[2]->remove );
        for my $child (@replacement) {
            $list2->add_element( $child->remove );
        }
        indent_element($insert);
    }

    # if we have an operator, then simply pass the string slice
    if ( $element->sprevious_sibling ) {
        for my $child ( $list->children ) {
            $list->insert_before( $child->remove );
        }
        $element->delete;
        $list->delete;
    }
    return;
}

sub map_split {
    my ($element) = @_;
    my $list      = map_built_in($element);
    my $sep       = $list->schild(0);
    if ($sep) {
        my $str;
        my $op = $sep->snext_sibling;
        if ($op) {
            $str = $op->snext_sibling;
            map_element($str);
        }
        else {
            $op  = PPI::Token::Operator->new(q{,});
            $str = PPI::Token::Symbol->new('_');
            $list->__insert_after_child( $sep, $op, $str );
        }
        if ( $sep->isa('PPI::Token::Quote') ) {
            $element->{content} = "$str.$element->{content}";
            $op->delete;
            $str->delete;
        }
        elsif ( $sep->isa('PPI::Token::Regexp::Match') ) {
            add_import( $element, 're' );
            $element->{content} = "re.$element->{content}";
            $sep->insert_after( regex2quote($sep) );
            $sep->delete;
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
    $element->{start}->{content}     = q{[};
    $element->{finish}->{originally} = $element->{finish}->{content};
    $element->{finish}->{content}    = q{]};
    my $expression = $element->schild(0);
    if (    $element->{start}->{originally}
        and $element->{start}->{originally} eq '{'
        and $expression )
    {
        my $key = $expression->schild(0);
        if ( $expression->schildren == 1 and $key->isa('PPI::Token::Word') ) {
            $key->{content} = q{"} . $key->{content} . q{"};
            return;
        }
        if ( $key eq 'scalar' ) {
            $key->delete;
            return;
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
            if ( $element eq
                '@args' )    ## no critic (RequireInterpolationOfMetachars)
            {
                $element->{content} = '*args';
            }
            if ($operator) {
                if ( $operator eq q{=} ) {
                    my $list = $operator->snext_sibling;
                    if ($list) {
                        $list->{start}->{content}  = q{[};
                        $list->{finish}->{content} = q{]};
                    }
                }

                # map scalar @array -> len(array)
                elsif ( $operator =~ m{(?:==|<=|>=|!=|[><+*/-])}xsm
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
        $element->{content} =~ s/::/./gsmx;
        if ( grep { $_ eq $element->{content} } @RESERVED_WORDS ) {
            $element->{content} = "_$element->{content}";
        }
    }
    return;
}

sub map_system {
    my ($element) = @_;
    add_import( $element, 'subprocess' );
    $element->{content} = 'subprocess.run';
    my $args = map_built_in($element);
    map_element($args);
    my $list = PPI::Structure::List->new( PPI::Token::Structure->new('[') );
    $list->{finish} = PPI::Token::Structure->new(']');
    my @children = $args->children;

    while (@children) {
        my $child = shift @children;
        if (   $child->isa('PPI::Structure::List')
            or $child->isa('PPI::Statement::Expression') )
        {
            my @gchildren = $child->children;
            if (@gchildren) {
                unshift @children, @gchildren;
            }
        }
        else {
            my $parent = $child->parent;

            # run() takes a string or list of strings
            if (   $child->isa('PPI::Token::Quote')
                or $child->isa('PPI::Token::Operator')
                or $child->isa('PPI::Token::Whitespace') )
            {
                $list->add_element( $child->remove );
            }
            else {
                $list->add_element( PPI::Token::Word->new('str') );
                my $strlist =
                  PPI::Structure::List->new( PPI::Token::Structure->new('(') );
                $strlist->{finish} = PPI::Token::Structure->new(')');
                $list->add_element($strlist);
                $strlist->add_element( $child->remove );
            }
            if ( $parent eq '[]' ) {
                $parent->delete;
            }
        }
    }
    $args->add_element($list);
    return;
}

sub map_data_uuid {
    my ($element) = @_;
    $element->{content} = 'uuid';
    my $method = $element->snext_sibling->snext_sibling;
    if ( $method eq 'new' ) {
        $method->{content} = 'uuid1';
        my $list = $method->snext_sibling;
        if ($list) { $list->delete }
    }
    return;
}

sub map_myour {
    my ($element) = @_;
    my $symbol    = $element->snext_sibling;
    my $parent    = $element->parent;
    if (
        $symbol eq '$self'    ## no critic (RequireInterpolationOfMetachars)
        and $parent !~ /shift/xsm
      )
    {
        $parent->delete;
    }
    else {
        $element->delete;
    }
    return;
}

sub map_pass_fail {
    my ($element) = @_;
    if ( $element->{content} eq 'pass' ) {
        $element->{content} = 'assert True';
    }
    else {
        $element->{content} = 'assert False';
    }
    if ( $element->snext_sibling ) {
        $element->insert_after( PPI::Token::Operator->new(q{,}) );
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

sub map_posix_strftime {
    my ($element) = @_;
    add_import( $element, 'datetime' );
    my $list = map_built_in($element);
    $element->{content} = 'datetime.datetime.date';
    my $op = PPI::Token::Operator->new(q{.});
    $list->insert_after($op);
    my $strftime = PPI::Token::Word->new('strftime');
    $op->insert_after($strftime);
    my $list2 = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
    $list2->{finish} = PPI::Token::Structure->new(')');
    $strftime->insert_after($list2);
    my $xpression = $list->schild(0);
    $list2->add_element( $xpression->schild(0)->remove );    #template
    map_element($list2);
    $xpression->schild(0)->delete;                           # comma

    for my $arg ( reverse $xpression->children ) {
        $xpression->add_element( $arg->remove );
    }
    return;
}

sub map_postfix_if {
    my ($element) = @_;
    my $prev = $element->sprevious_sibling;
    if ( not $prev ) { return }
    if ( $element eq 'unless' ) {
        $element->{content} = 'if not';
    }
    my $cstatement = PPI::Statement::Compound->new;
    my $ostatement = $element->parent;

    # could be multiple items, especially if the condition is not surrounded by
    # parens.
    my @condition = get_argument_for_operator( $element, 1 );
    $ostatement->insert_before($cstatement);
    my $block = PPI::Structure::Block->new( PPI::Token::Structure->new('{') );
    $block->{start}->{content} = q{:};
    $cstatement->add_element( $element->remove );
    $cstatement->add_element( PPI::Token::Whitespace->new(q{ }) );
    for my $child (@condition) {
        $cstatement->add_element( $child->remove );
    }
    $condition[-1]->insert_after($block);
    $block->add_element( $ostatement->remove );
    indent_element($cstatement);
    return;
}

sub map_ref {
    my ($element) = @_;
    my $list      = map_built_in($element);
    my $operator  = $list->snext_sibling;
    my $string;
    if ($operator) {
        $string = $operator->snext_sibling;
    }
    if (    $operator
        and $operator =~ /(?:eq|ne)/xms
        and $string
        and $string->isa('PPI::Token::Quote')
        and quote2content( $string, 0 ) eq 'ARRAY' )
    {
        $element->{content} = 'isinstance';
        $list->add_element( PPI::Token::Operator->new(q{,}) );
        $list->add_element( PPI::Token::Word->new('list') );
        $operator->delete;
        $string->delete;
    }
    else {
        $element->{content} = 'type';
    }
    return;
}

sub map_sane_enums {
    my ($element) = @_;
    $element->{content} =~ s/^SANE_//xsm;
    if ( $element->{content} =~ s/^NAME_//xsm ) {
        $element->{content} =~ s/^SCAN_//xsm;
        $element->{content} =~ s/_/-/xsm;
        $element->{content} = lc $element->{content};
    }
    elsif ( $element->{content} =~ /^(:?TRUE|FALSE)$/xsm ) {
        map_word($element);
        return;
    }
    $element->{content} = q{"} . $element->{content} . q{"};
    return;
}

# map setter to property
sub map_set {
    my ($element) = @_;
    my $list      = map_built_in($element);
    my $prev      = $element->sprevious_sibling;
    my $name      = $list->schild(0);
    if ( $name->isa('PPI::Statement::Expression') ) { $name = $name->schild(0) }
    if ( ( $prev eq '->' or $prev->{originally} eq '->' ) and $name ) {
        my @value = get_argument_for_operator( $name->snext_sibling, 1 );
        for my $value (@value) {
            map_element($value);
        }

        # refetch value incase mapping has changed things
        @value = get_argument_for_operator( $name->snext_sibling, 1 );
        $name->{content} =~ s/["']//gsmx;    # remove the quotes
        $name->{content} =~ s/-/_/gsmx;      # dash to underscore
        $element->insert_before( $name->remove );
        $element->insert_before( PPI::Token::Operator->new(q{=}) );

        # loop twice to retain context whilst mapping
        for my $value (@value) {
            $element->insert_before( $value->remove );
        }
        $element->delete;
        $list->delete;
    }
    return;
}

sub map_shift {
    my ($element) = @_;
    my $argument = $element->snext_sibling;

    # deal with implied @_ in shift
    if ( not $argument ) {
        my $parent = $element->parent;
        my $source = $parent->find_first('PPI::Token::Symbol');
        if ( not $source ) {    # argument is discarded in Perl->replace with _
            $source = PPI::Token::Symbol->new(q{_});
        }
        my $dest_list =
          $parent->parent->parent->find_first('PPI::Structure::List');
        if ( $dest_list->children ) {
            $dest_list->add_element( PPI::Token::Operator->new(q{,}) );
        }
        $dest_list->add_element( $source->remove );
        $parent->delete;
        map_element($dest_list);
    }
    else {
        $element->{content} = '.pop(0)';
        $element->snext_sibling->insert_after( $element->remove );
    }
    return;
}

sub map_signals {
    my ( $element, $name ) = @_;
    $element->{content} = $name;
    my $prev = $element->sprevious_sibling;
    if ( $prev ne 'def' and $element->{content} =~ /connect/xsm ) {
        my $list = map_built_in($element);
        map_element($list);
        my $exp   = $list->schild(0);
        my $event = $exp->schild(0);
        if ( $event ne '*args' and $element->{content} !~ /dis/xsm ) {
            $event->{content} =~ s/_/-/gxsm;
            $event->{content} = "'$event->{content}'";
            my $op = $exp->schild(1);
            $op->{content} = q{,};
        }
    }
    return;
}

sub map_sort {
    my ($element) = @_;
    my $method = $element->snext_sibling;
    if ( $method->isa('PPI::Structure::Block') ) {
        $method->delete;
    }
    my $list = map_built_in($element);
    $element->{content} = 'sorted';
    return;
}

sub map_sprintf {
    my ($element) = @_;
    my $list = map_built_in($element);
    if ( not $list ) { return }
    my @format = get_argument_for_operator( $element, 1 );
    map_format_string( $element, @format );
    $element->delete;
    return;
}

sub map_format_string {
    my ( $element, @format ) = @_;
    my $operator = $format[-1]->snext_sibling;
    $operator->{content} = q{%};
    for ( 0 .. $#format ) {
        $format[$_] = $format[$_]->remove;
    }
    my $parent = $element->parent;
    $parent->__insert_before_child( $element, @format,
        PPI::Token::Whitespace->new(q{ }),
        $operator->remove, PPI::Token::Whitespace->new(q{ }) );
    return;
}

sub map_threads {
    my ($element) = @_;
    $element->{content} = 'threading.Thread';
    my $op = $element->snext_sibling;
    if ($op) {
        my $method = $op->snext_sibling;
        my $list   = map_built_in($method);
        my $expr   = $list->schild(0);
        my $target = $expr->schild(0);
        $op   = $expr->schild(2);
        $list = map_built_in($op);
        if ( $list->children == 1 ) {
            $list->add_element( PPI::Token::Operator->new(q{,}) );
        }
        $target->insert_before( PPI::Token::Word->new('target=') );
        $list->insert_before( PPI::Token::Word->new('args=') );
    }
    return;
}

sub map_undef {
    my ($element) = @_;
    my $prev      = $element->sprevious_sibling;
    my $next      = $element->snext_sibling;
    my $gparent   = $element->parent->parent;
    if ( $gparent and $gparent->isa('PPI::Structure::List') ) {
        if ( $gparent->snext_sibling eq q{=} ) {
            $element->{content} = '_';
        }
        else {
            $element->{content} = 'None';
        }
    }
    elsif ( $prev or ( $next and $next->isa('PPI::Token::Operator') ) ) {
        $element->{content} = 'None';
    }
    else {
        my @arg = get_argument_for_operator( $element, 1 );
        $arg[-1]->insert_after( PPI::Token::Word->new('None') );
        $arg[-1]->insert_after( PPI::Token::Operator->new(q{=}) );
        $element->delete;
    }
    return;
}

sub map_unpack {
    my ($element) = @_;
    add_import( $element, 'struct' );
    my $list = map_built_in($element);
    $element->{content} = 'struct.unpack';
    if ( $list->schildren < 2 ) {
        $list->add_element( PPI::Token::Symbol->new(',_') );
    }
    return;
}

sub map_use_ok {
    my ($element) = @_;
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
    return;
}

sub map_variable {
    my ($element) = @_;

    # don't define variables in the header of a compound
    if ( $element->parent->isa('PPI::Statement::Compound') ) { return }
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

sub create_variable_definition {
    my ($element) = @_;
    my $type      = substr $element->{content}, 0, 1;
    if ( $type eq q{$} ) {
        return PPI::Token::Word->new('None');
    }
    elsif ( $type eq q{@} ) {
        my $arraydef =
          PPI::Structure::List->new( PPI::Token::Structure->new('[') );
        $arraydef->{finish} = PPI::Token::Structure->new(']');
        return $arraydef;
    }
    elsif ( $type eq q{%} ) {
        my $hashdef =
          PPI::Structure::List->new( PPI::Token::Structure->new('{') );
        $hashdef->{finish} = PPI::Token::Structure->new('}');
        return $hashdef;
    }
    return;
}

sub map_word {
    my ($element) = @_;
    if ( not $element->{content} ) { return }
    $element->{content} =~ s/::/./gsm;
    my $elementstr = "$element";
    if ( $elementstr eq 'FALSE' ) {    # special-case common bare words
        $element->{content} = 'False';
    }
    elsif ( $elementstr eq 'Data.UUID' ) {
        map_data_uuid($element);
    }
    elsif ( $elementstr eq 'File.Temp' ) {
        map_file_temp($element);
    }
    elsif ( $elementstr eq 'Glib.Type' ) {
        $element->{content} = 'GObject.TypeModule';
    }
    elsif ( $elementstr =~ /Gtk\d[.]Gdk/xsm ) {
        $element->{content} =~ s/Gtk\d[.]//gsmx;
        add_import( $element, 'gi.repository', 'Gdk' );
    }
    elsif ( $elementstr =~ /Gtk\d[.]EVENT/xsm ) {
        $element->{content} =~ s/Gtk\d/Gdk/gsmx;
        add_import( $element, 'gi.repository', 'Gdk' );
    }
    elsif ( $elementstr =~ /Gtk\d/xsm ) {
        $element->{content} =~ s/Gtk\d/Gtk/gsmx;
    }
    elsif ( $elementstr eq 'Image.Magick' ) {
        map_image_magick($element);
    }
    elsif ( $elementstr eq 'Locale.gettext' ) {
        $element->{content} = 'gettext';
        my $operator = $element->snext_sibling;
        my $method   = $operator->snext_sibling;
        if ( $method eq 'domain' ) {
            $method->{content} = 'translation';
        }
    }
    elsif ( $elementstr eq 'POSIX.strftime' ) {
        map_posix_strftime($element);
    }
    elsif ( $elementstr eq 'Readonly' ) {
        my $operator = $element->parent->find_first('PPI::Token::Operator');
        if ( $operator ne '=>' ) {
            croak "Unexpected operator '$operator'\n";
        }
        $operator->{content} = q{=};
        $element->delete;
    }
    elsif ( $elementstr =~ /^SANE_/xsm ) {
        map_sane_enums($element);
    }
    elsif ( $elementstr eq 'TRUE' ) {
        $element->{content} = 'True';
    }
    elsif ( $elementstr eq 'Time_to_Date' ) {
        $element->{content} = 'datetime.datetime.fromtimestamp';
    }
    elsif ( $elementstr =~ /^(?:abs|chr|int|ord)$/xsm ) {
        map_built_in($element);
    }
    elsif ( $elementstr eq 'bless' ) {
        $element->parent->delete;
    }
    elsif ( $elementstr eq 'blessed' ) {    # no equivalent for blessed
        my $list     = map_built_in($element);
        my $operator = $list->snext_sibling;
        $element->delete;
        $list->delete;
        if ( $operator->isa('PPI::Token::Operator') ) {
            $operator->delete;
        }
    }
    elsif ( $elementstr eq 'can_ok' ) {
        my $statement = $element->parent;
        my $block     = $statement->parent;
        my $list      = map_built_in($element);
        my $exp       = $list->schild(0);
        my $obj       = $exp->schild(0);
        map_element($obj);
        my $methods = $exp->schild(2);
        my $comment = $exp->schild( 2 + 2 );
        my $string  = substr $methods->content,
          $methods->{sections}[0]{position}, $methods->{sections}[0]{size};

        for ( split q{ }, $string ) {
            my $nstatement = PPI::Statement->new();
            my $str        = "assert hasattr($obj,'$_') and callable($obj.$_)";
            if ( defined $comment ) { $str .= ", $comment" }
            $nstatement->add_element( PPI::Token::Word->new($str) );
            $block->__insert_before_child( $statement, $nstatement );
            indent_element($nstatement);
        }
        $statement->delete;
    }
    elsif ( $elementstr eq 'carp' ) {
        map_built_in($element);
        $element->{content} = 'warn';
    }
    elsif ( $elementstr eq 'catch' ) {
        $element->{content} = 'except';
    }
    elsif ( $elementstr eq 'chomp' ) {
        map_chomp($element);
    }
    elsif ( $elementstr eq 'close' ) {
        my $list = map_built_in($element);
        my $fh   = $list->schild(0);
        map_element($fh);
        if ( $fh->isa('PPI::Statement::Expression') ) {
            $fh = $fh->schild(0);
        }
        $fh->{content} .= q{.};
        $element->insert_before( $fh->remove );
    }
    elsif ( $elementstr eq 'cmp_ok' ) {
        $element->{content} = 'assert';
        my $list = map_built_in($element);
        map_element($list);
        my $exp  = $list->schild(0);
        my $com1 = $exp->schild(1);
        my $op   = $com1->snext_sibling;
        my $com2 = $op->snext_sibling;
        $com1->delete;
        $com2->delete;
        $op->{content} = quote2content($op);
        $list->parent->__insert_after_child( $list, $exp->remove );
        $list->delete;
        $element->insert_after( PPI::Token::Whitespace->new(q{ }) );
    }
    elsif ( $elementstr eq 'create_str' ) {
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
    elsif ( $elementstr =~ /^(?:croak|die)$/xsm ) {
        $element->{content} = 'raise';
    }
    elsif ( $elementstr eq 'dclone' ) {
        $element->{content} = 'deepcopy';
    }
    elsif ( $elementstr eq 'decode_utf8' ) {
        my $list       = map_built_in($element);
        my $expression = $list->schild(0);
        my $str        = $expression->schild(0);
        $element->insert_before( $str->remove );
        $element->{content} = '.decode';
        $list->add_element( PPI::Token::Quote::Double->new('"utf8"') );
    }
    elsif ( $elementstr eq 'defined' ) {
        map_defined($element);
    }
    elsif ( $elementstr eq 'delete' ) {
        if ( not $element->parent->isa('PPI::Statement::Expression') ) {
            map_built_in($element);
            $element->{content} = 'del';
        }
    }
    elsif ( $elementstr eq 'do' ) {
        map_do($element);
    }
    elsif ( $elementstr eq 'each' ) {
        my $list = map_built_in($element);
        $element->{content} = '.items';
        for my $child ( $list->children ) {
            $element->insert_before( $child->remove );
        }
    }
    elsif ( $elementstr eq 'elsif' ) {
        $element->{content} = 'elif';
    }
    elsif ( $elementstr eq 'eval' ) {
        map_eval($element);
    }
    elsif ( $elementstr eq 'first_index' ) {
        map_first_index($element);
    }
    elsif ( $elementstr eq 'foreach' ) {
        $element->{content} = 'for';
    }
    elsif ( $elementstr eq 'get' ) {
        map_get($element);
    }
    elsif ( $elementstr eq 'glob' ) {
        add_import( $element, 'glob' );
        map_built_in($element);
        $element->{content} = 'glob.glob';
    }
    elsif ( $elementstr eq 'grep' ) {
        map_grep($element);
    }
    elsif ( $elementstr eq 'hex' ) {
        map_built_in($element);
    }
    elsif ( $elementstr =~ /^(?:if|unless)$/xsm ) {
        map_postfix_if($element);
    }
    elsif ( $elementstr =~ /^(?:is|cmp)(?:_deeply|nt)?$/xsm ) {
        map_is($element);
    }
    elsif ( $elementstr eq 'isa' ) {
        map_isa($element);
    }
    elsif ( $elementstr eq 'isa_ok' ) {
        my $parent = $element->parent;
        my $list   = map_built_in($element);
        $element->insert_before( PPI::Token::Word->new('assert') );
        $element->insert_before( PPI::Token::Whitespace->new(q{ }) );
        $element->{content} = 'isinstance';
        my $type = $list->find_first('PPI::Token::Quote');
        $type->{content} =~ s/::/./gsm;
        $type->{content} =~ s/["']//gsmx;

        while ( my $token = $type->next_sibling ) {
            $parent->add_element( $token->remove );
        }
    }
    elsif ( $elementstr eq 'join' ) {
        map_join($element);
    }
    elsif ( $elementstr =~ /^(?:keys|values)$/xsm ) {
        my $list = map_built_in($element);
        for my $child ( $list->children ) {
            $element->insert_before( $child->remove );
        }
        $element->insert_before( PPI::Token::Operator->new(q{.}) );
    }
    elsif ( $elementstr eq 'keyval' ) {
        my $list     = map_built_in($element);
        my @children = $list->children;
        if (@children) {
            $list->insert_before( PPI::Token::Operator->new(q{=}) );
            for my $child (@children) {
                map_element($child);
                $element->parent->__insert_after_child( $list, $child->remove );
            }
        }
        $list->delete;
    }
    elsif ( $elementstr eq 'killfam' ) {
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
    elsif ( $elementstr eq 'last' ) {
        $element->{content} = 'break';
    }
    elsif ( $elementstr eq 'length' ) {
        my $list = map_built_in($element);
        $element->{content} = 'len';
    }
    elsif ( $elementstr eq 'like' ) {
        map_like($element);
    }
    elsif ( $elementstr eq 'local' ) {    # no equivalent in python
        $element->delete;
    }
    elsif ( $elementstr eq 'log' ) {
        add_import( $element, 'math' );
        $element->{content} = 'math.log';
    }
    elsif ( $elementstr eq 'map' ) {
        map_map($element);
    }
    elsif ( $elementstr =~ /^(?:copy|move)$/xsm ) {
        map_move_copy($element);
    }
    elsif ( $elementstr =~ /^(?:my|our)$/xsm ) {
        map_myour($element);
    }
    elsif ( $elementstr eq 'next' ) {
        $element->{content} = 'continue';
    }
    elsif ( $elementstr eq 'new' ) {
        map_new($element);
    }
    elsif ( $elementstr eq 'num' ) {
        my $list = map_built_in($element);
        add_import( $element, 'pytest' );
        $element->{content} = 'pytest.approx';
    }
    elsif ( $elementstr eq 'ok' ) {
        map_ok($element);
    }
    elsif ( $elementstr eq 'open' ) {
        map_open($element);
    }
    elsif ( $elementstr eq 'pack' ) {
        add_import( $element, 'struct' );
        map_built_in($element);
        $element->{content} = 'struct.pack';
    }
    elsif ( $elementstr =~ /^(?:pass|fail)$/xsm ) {
        map_pass_fail($element);
    }
    elsif ( $elementstr eq 'pop' ) {
        my $list = map_built_in($element);
        $element->{content} = '.pop';
        for my $child ( $list->children ) {
            $element->insert_before( $child->remove );
        }
    }
    elsif ( $elementstr =~ /^printf?$/xsm ) {
        map_print($element);
    }
    elsif ( $elementstr =~ /^(?:push|unshift)$/xsm ) {
        map_push($element);
    }
    elsif ( $elementstr eq 'ref' ) {
        map_ref($element);
    }
    elsif ( $elementstr eq 'reverse' ) {
        map_built_in($element);
        $element->{content} = 'reversed';
    }
    elsif ( $elementstr eq 'rmdir' ) {
        my $list = map_built_in($element);
        add_import( $element, 'os' );
        $element->{content} = 'os.rmdir';
    }
    elsif ( $elementstr eq 'scalar' ) {
        map_scalar($element);
    }
    elsif ( $elementstr eq 'set' ) {
        map_set($element);
    }
    elsif ( $elementstr eq 'setlocale' ) {
        map_setlocale($element);
    }
    elsif ( $elementstr eq 'share' ) {
        my $list = map_built_in($element);
        $list->insert_after( PPI::Token::Symbol->new('queue.Queue()') );
        $list->insert_after( PPI::Token::Operator->new(q{=}) );
        for my $child ( $list->children ) {
            $list->insert_before( $child->remove );
        }
        $element->delete;
        $list->delete;
    }
    elsif ( $elementstr eq 'shift' ) {
        map_shift($element);
    }
    elsif ( $elementstr =~ /^$SIGNAL_PREFIX$SIGNAL_SUFFIX$/xsm ) {
        map_signals( $element, $1 );
    }
    elsif ( $elementstr eq 'sort' ) {
        map_sort($element);
    }
    elsif ( $elementstr eq 'splice' ) {
        map_splice($element);
    }
    elsif ( $elementstr eq 'split' ) {
        map_split($element);
    }
    elsif ( $elementstr eq 'sprintf' ) {
        map_sprintf($element);
    }
    elsif ( $elementstr eq 'stat' ) {
        $element->{content} = 'os.stat';
    }
    elsif ( $elementstr eq 'sub' ) {
        map_anonymous_sub($element);
    }
    elsif ( $elementstr eq 'substr' ) {
        my $list = map_built_in($element);
        $list->{start}->{content}  = q{[};
        $list->{finish}->{content} = q{]};
        $list->insert_before( $list->schild(0)->remove );
        $list->schild(0)->delete;
        $element->delete;
        my $comma = $list->find_first('PPI::Token::Operator');
        $comma->{content} = q{:};
        $comma->insert_after( PPI::Token::Operator->new(q{+}) );
        my $offset = $list->schild(0)->clone;
        map_element($offset);
        $comma->insert_after($offset);
    }
    elsif ( $elementstr eq 'system' ) {
        map_system($element);
    }
    elsif ( $elementstr eq 'threads' ) {
        map_threads($element);
    }
    elsif ( $elementstr eq 'ucfirst' ) {
        my $list = map_built_in($element);
        $element->{content} = '.capitalize';
        for my $child ( $list->children ) {
            map_element($child);
            $element->insert_before( $child->remove );
        }
    }
    elsif ( $elementstr eq 'undef' ) {
        map_undef($element);
    }
    elsif ( $elementstr eq 'unlink' ) {
        my $list = map_built_in($element);
        add_import( $element, 'os' );
        $element->{content} = 'os.remove';
    }
    elsif ( $elementstr eq 'unpack' ) {
        map_unpack($element);
    }
    elsif ( $elementstr eq 'use_ok' ) {
        map_use_ok($element);
    }
    elsif ( $elementstr eq 'utime' ) {
        add_import( $element, 'os' );
        $element->{content} = 'os.utime';
        my $list = map_built_in($element);
        my $tuple =
          PPI::Structure::List->new( PPI::Token::Structure->new('(') );
        $tuple->{finish} = PPI::Token::Structure->new(')');
        for ( 0 .. 2 ) {
            $tuple->add_element( $list->schild(0)->remove );
        }
        $list->add_element( $list->schild(0)->remove );
        $list->add_element($tuple);
    }
    elsif ( $elementstr eq 'waitpid' ) {
        add_import( $element, 'os' );
        my $list = map_built_in($element);
        $element->{content} = 'os.waitpid';
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

sub next_sibling {
    my ( $element, $n ) = @_;
    return $n == 0 ? $element->sprevious_sibling : $element->snext_sibling;
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

sub has_higher_precedence_than {
    my ( $op1, $op2, $direction ) = @_;

    # don't swallow built-ins looking left for arguments
    if ( $direction == 0 and defined $BUILTINS{$op2} ) { return }

    # don't swallow postfix if/unless
    if ( $direction == 1 and $op2 =~ /^(?:if|unless)$/xsm ) { return }

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

sub quote2content {
    my ($element) = @_;
    return substr $element->{content}, 1, length( $element->{content} ) - 2;
}

sub regex2search {
    my ( $regex, @string_expression ) = @_;
    add_import( $regex, 're' );
    my $list = PPI::Structure::List->new( PPI::Token::Structure->new('(') );
    $list->{finish} = PPI::Token::Structure->new(')');
    $list->add_element( regex2quote($regex) );
    $list->add_element( PPI::Token::Operator->new(q{,}) );
    for my $string (@string_expression) {
        $list->add_element( $string->clone );
    }
    return PPI::Token::Word->new('re.search'), $list;
}

sub regex2quote {    # FIXME: duplicate code with map_quote()
    my ( $element, $section ) = @_;
    if ( not defined $section ) {
        $section = 0;
    }
    my $content = substr $element->content,
      $element->{sections}[$section]{position},
      $element->{sections}[$section]{size};
    if ( $content =~ /^\$(\w+)$/xsm ) {
        return PPI::Token::Symbol->new($1);
    }
    $content =~ s/\[\[:alpha:\]\]/[A-Za-z]/xsm;
    $content =~ s/"/\\"/xsmg;
    my $quote = q{"};
    my $type  = q{r};
    if ( $content =~ /\$(\w+)/xsm ) {
        $type = q{fr};
        $content =~ s/{/{{/xsm;
        $content =~ s/}/}}/xsm;
        $content =~ s/\$(\w+)/{$1}/xsmg;
    }
    if ( $element->{content} =~ /\n/xsm ) {
        $quote = q{"""};
    }
    return PPI::Token::Quote::Double->new("$type$quote$content$quote");
}

sub remove_cast {
    my ( $element, $block, $parent ) = @_;
    if ( not $block ) { return }
    my $child;
    if ( $block->isa('PPI::Token::Magic') ) {
        $child = $block->clone;
    }
    else {
        $child = $block->schild(0);
    }
    map_element($child);
    $parent->__insert_after_child( $block, $child->remove );
    $element->delete;
    $block->delete;
    return $child;
}

1;
__END__

=head1 NAME

Perl2Python - Tool to transcribe Perl 5 to Python 3

=head1 VERSION

1

=head1 SYNOPSIS

This module attempts to transcribe Perl 5 code to Python 3. This will not be
successful for all but the simplest code, but it should do most of the legwork.

=head1 DESCRIPTION

You will probably be left with the following problems to solve:

=over

=item *
Python is much stricter about types than Perl. Perl will happily convert a
string with a number to a number for comparison purposes. Python requires this
to be explicit, so you may well have to add casts.

=item *
Similarly, in Perl, you can create a scalar and turn it into a hashref or an
arrayref by using hash or array functions on it. In Python, you have to
explicitly initialise the dict or list first, so these statements will have to
be added.

=item *
Perl is fairly flexible about the contents of strings. Python strictly separates
bytes and strings. Python code will often require extra encode() or decode()
calls to convert from one to the other.

=item *
Perl has a very loose concept of instance variables in classes. In Python, these
must previously declared.

=item *
Perl does not distinguish between instance variables in classes and hash keys.
perl2python makes a guess that any keys for objects called "self" are instance
variables, and anything else are dict keys. This will often be wrong.

=item *
All Perl modules that are in the list of directory to be searched are
automatically identified. Python modules are identified partly by their path,
and thus either the import statements or their calls will often have to be
adjusted.

=item *
Capture groups in Perl substitutions cannot trivially be transcribed, as they
typically contain modifiers, e.g.:

 s/(^\w)/\U$1/xsmg

will need something like

 re.sub(r"(^\w)",lambda x: x.group(1).upper(),string_to_match)

=item *
Unless prototypes are used when defined Perl subs, Perl does not care about the
number of arguments. Python will throw an error if the number of arguments is
unexpected. Thus Perl code handling default argument values will need to be
manually adjusted, as checking this is non-trivial.

=back

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

Copyright (C) 2021-2022 by Jeffrey Ratcliffe

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.5 or,
at your option, any later version of Perl 5 you may have available.

=cut
