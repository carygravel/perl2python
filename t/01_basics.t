use warnings;
use strict;
use English qw( -no_match_vars );    # for $INPUT_RECORD_SEPARATOR
use Perl2Python qw(map_document map_path);
use Test::More tests => 126;

sub slurp {
    my ($file) = @_;
    open my $fh, '<', $file;
    local $INPUT_RECORD_SEPARATOR = undef;
    my $text = <$fh>;
    close $fh;
    return $text;
}

#########################

my $script = <<'EOS';
#!/usr/bin/perl
use warnings;
use strict;
print "Hello world!\n";
EOS

my $expected = <<'EOS';
#!/usr/bin/python3

print("Hello world!") 
EOS

is map_document( \$script ), $expected, "print()";

my $in  = 'test.pl';
my $out = 'test.py';
open my $fh, '>', $in;
print $fh $script;
close $fh;
system("perl bin/perl2python $in");
is slurp($out), $expected, "Hello world";
unlink $in, $out;

#########################

is map_path('lib/package/module.pm'), 'package/module.py', "map_path lib";
is map_path('base/lib/package/module.pm'), 'base/package/module.py',
  "map_path lib2";
is map_path('t/01_basics.t'), 'tests/test_01_basics.py', "map_path t";

#########################

$script = <<'EOS';
use Test::More tests => 14;
use Gtk3 -init;
is $result, $expected, "comment";
is_deeply \@result, \@expected, "comment";
            is_deeply [ 1, 2, 3 ],
              [ 4, 5, 6 ],
              "comment";
method_with_is();
is_deeply(\@result, \@expected, "comment");
is_deeply( $hashref->{array}, \@that, 'comment' );
is( MyClass->method(), 'return value', 'comment' );
is( MyClass->method, 'return value', 'comment' );
is( $iter->(), 'return value', 'comment' );
isa_ok( $object, 'My::Class' );
pass 'comment';
ok( $dialog->get('property') == 'value', 'comment' );
EOS

$expected = <<'EOS';
import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk

def test_1():

    assert result== expected, "comment"
    assert result== expected, "comment"
    assert [
    1, 2, 3 ]==               [
    4, 5, 6 ],               "comment"
    method_with_is()
    assert result== expected, "comment"
    assert hashref["array"]== that, 'comment'
    assert MyClass.method()== 'return value', 'comment'
    assert MyClass.method()== 'return value', 'comment'
    assert next(iter)== 'return value', 'comment'
    assert isinstance( object, My.Class )
    assert True, 'comment'
    assert dialog.get('property') == 'value', 'comment'
EOS

$in  = 'test.t';
$out = 'test.py';
open $fh, '>', $in;
print $fh $script;
close $fh;
system("perl bin/perl2python $in");
is slurp($out), $expected, "Basic test";
unlink $in, $out;

$script = <<'EOS';
if ( not eval { require MyPackage; } ) {
    plan( skip_all => "MyPackage required to run tests" );
}
EOS

$expected = <<'EOS';
pytest.importorskip('MyPackage')
EOS

is map_document( \$script ), $expected, "conditionally skip tests";

$script = <<'EOS';
eval "use MyPackage";
plan skip_all => "MyPackage required" if $@;
EOS

$expected = <<'EOS';
pytest.importorskip('MyPackage')

EOS

is map_document( \$script ), $expected, "conditionally skip more tests";

#########################

$script = <<'EOS';
use 5.008005;
use feature 'switch';
no if $] >= 5.018, warnings => 'experimental::smartmatch';
use English qw( -no_match_vars );
use Exporter ();
use Carp;
use MyModule::MySubModule::MySubSubModule;
MyModule::MySubModule::MySubSubModule::my_method();
my $var = $MyModule::MySubModule::MODULE_CONSTANT;
EOS

$expected = <<'EOS';
import MyModule.MySubModule.MySubSubModule
MyModule.MySubModule.MySubSubModule.my_method()
var = MyModule.MySubModule.MODULE_CONSTANT
EOS

is map_document( \$script ), $expected, "import";

$script = <<'EOS';
use MyModule::MySubModule::MySubSubModule 2.40;
EOS

$expected = <<'EOS';
import MyModule.MySubModule.MySubSubModule 
EOS

is map_document( \$script ), $expected, "import ignore version";

$script = <<'EOS';
use MyModule 'symbol';
use MyModule ':all';
use MyModule qw(symbol1 symbol2);
use Glib;
use Glib qw(TRUE FALSE);    # To get TRUE and FALSE
EOS

$expected = <<'EOS';
from MyModule import symbol
from MyModule import *
from MyModule import symbol1,symbol2
from gi.repository import GObject
    # To get TRUE and FALSE
EOS

is map_document( \$script ), $expected,
  "map use with symbol, special casing import Glib";

$script = <<'EOS';
use Gtk3 0.028 -init;
EOS

$expected = <<'EOS';
import gi
gi.require_version("Gtk", "3.0")
from gi.repository import Gtk
EOS

is map_document( \$script ), $expected, "special case import Gtk3";

$script = <<'EOS';
package My::Object;
use warnings;
use Glib::Object::Subclass Parent::Object::;
EOS

$expected = <<'EOS';
from gi.repository import GObject
class Object(Parent.Object):
    def __init__(self):
        GObject.GObject.__init__(self)
EOS

is map_document( \$script ), $expected, "subclass basic GObject";

$script = <<'EOS';
package My::Package;
use Some::Other::Package;
use Glib::Object::Subclass Glib::Object::;
EOS

$expected = <<'EOS';
from gi.repository import GObject
import Some.Other.Package
class Package(GObject.Object):

    def __init__(self):
        GObject.GObject.__init__(self)
EOS

is map_document( \$script ), $expected, "subclass basic GObject #2";

$script = <<'EOS';
package My::Object;
use Glib::Object::Subclass Gtk3::Object::, signals => {
    'signal_with_float' => {
        param_types => ['Glib::Float'],
    },
    'signal_with_ints' => {
        param_types => [ 'Glib::Int', 'Glib::Int' ],
    },
    'signal_without_value' => { param_types => [], },
    'signal_without_param_types' => {},
    show                  => \&show,
  },
  properties => [
    Glib::ParamSpec->scalar('name1', 'Nick1', 'Blurb1', G_PARAM_READWRITE),
    Glib::ParamSpec->string('name2','Nick2','Blurb2','default',G_PARAM_READWRITE),
    Glib::ParamSpec->int('name-3','Nick3','Blurb3', 1, 999, 1, [qw/readable writable/]),
  ];
EOS

$expected = <<'EOS';
from gi.repository import GObject
class Object(Gtk.Object):
    __gsignals__={'signal_with_float':(GObject.SIGNAL_RUN_FIRST,None,(float,)),'signal_with_ints':(GObject.SIGNAL_RUN_FIRST,None,( int,int, )),'signal_without_value':(GObject.SIGNAL_RUN_FIRST,None,None),'signal_without_param_types':(GObject.SIGNAL_RUN_FIRST,None,None),}
    name1=GObject.Property(type=object,nick='Nick1',blurb='Blurb1')
    name2=GObject.Property(type=str,default='default',nick='Nick2',blurb='Blurb2')
    name_3=GObject.Property(type=int,min=1,max=999,default=1,nick='Nick3',blurb='Blurb3')
    def __init__(self):
        GObject.GObject.__init__(self)
        self.connect("show",show)
EOS

is map_document( \$script ), $expected,
  "subclass GObject with signals and properties";

$script = <<'EOS';
use Set::IntSpan 1.10;          # For size method for page numbering issues
EOS

$expected = <<'EOS';
from intspan import intspan          # For size method for page numbering issues
EOS

is map_document( \$script ), $expected, "special case import Set::IntSpan";

$script = <<'EOS';
use Image::Sane ':all';
$unit = SANE_UNIT_NONE;
$unit = SANE_UNIT_PIXEL;
$type = SANE_TYPE_BOOL;
$type = SANE_TYPE_FIXED;

$cap = SANE_CAP_SOFT_DETECT;
$cap = SANE_CAP_SOFT_SELECT;
$cap = SANE_CAP_INACTIVE;

# not in the Python sane class
$val = SANE_TRUE;
$val = SANE_FALSE;

$name = SANE_NAME_PAGE_HEIGHT;
$name = SANE_NAME_PAGE_WIDTH;
$name = SANE_NAME_SCAN_TL_X;
$name = SANE_NAME_SCAN_BR_X;
$name = SANE_NAME_SCAN_TL_Y;
$name = SANE_NAME_SCAN_BR_Y;

$constraint_type = SANE_CONSTRAINT_NONE;
$constraint_type = SANE_CONSTRAINT_RANGE;

@array = ( [ scalar(SANE_NAME_PAGE_HEIGHT), 'pageheight' ] )
EOS

$expected = <<'EOS';
import sane
unit = "UNIT_NONE"
unit = "UNIT_PIXEL"
type = "TYPE_BOOL"
type = "TYPE_FIXED"

cap = "CAP_SOFT_DETECT"
cap = "CAP_SOFT_SELECT"
cap = "CAP_INACTIVE"

# not in the Python sane class

val = True
val = False

name = "page-height"
name = "page-width"
name = "tl-x"
name = "br-x"
name = "tl-y"
name = "br-y"

constraint_type = "CONSTRAINT_NONE"
constraint_type = "CONSTRAINT_RANGE"

array = [[
"page-height", 'pageheight' ] ]
EOS

is map_document( \$script ), $expected, "special case import Image::Sane";

#########################

$script = <<'EOS';
use Readonly;
Readonly my $VARIABLE          => 4;
EOS

$expected = <<'EOS';
VARIABLE          = 4
EOS

is map_document( \$script ), $expected, "readonly";

$script = <<'EOS';
my $EMPTY         = q{};
my $DOUBLE_QUOTES = q{"};
my $regex = qr{(\d{4})-(\d\d)-(\d\d)}xsm;
my $var_with_var = "$var1 $var2";
my $var_with_var = '$var1 $var2';
EOS

$expected = <<'EOS';
EMPTY         = ""
DOUBLE_QUOTES = "\""
regex = r"(\d{4})-(\d\d)-(\d\d)"
var_with_var = f"{var1} {var2}"
var_with_var = '$var1 $var2'
EOS

is map_document( \$script ), $expected, "more quotes";

#########################

$script = <<'EOS';
# a

# comment which should end up as a docstring
# $no interpolation to f-string

sub function {
    my ( $x, $y, $t ) = @_;
    return $x, $y, $t;
}
EOS

$expected = <<'EOS';

def function( x, y, t ) :
    """a

comment which should end up as a docstring
$no interpolation to f-string
"""    
    return x, y, t

EOS

is map_document( \$script ), $expected, "sub";

$script = <<'EOS';
sub function {
    my $x = shift;
    my $y = shift;
    my $t = shift;
    my $f = shift @d;
    return $x, $y, $t;
}
EOS

$expected = <<'EOS';
def function(x,y,t) :
    
    
    
    f =  d.pop(0)
    return x, y, t

EOS

is map_document( \$script ), $expected, "sub + shift";

$script = <<'EOS';
sub function {
    my @array = @_;
    return @array;
}
EOS

$expected = <<'EOS';
def function(array) :
    
    return array

EOS

is map_document( \$script ), $expected, "sub + array";

#########################

$script = <<'EOS';
my ($var1, @var2, %var3);
my $var3;
my ( $var1, $var2 ) = $self->some_method();
EOS

$expected = <<'EOS';
(var1, var2, var3)=(None,[],{})
var3=None
( var1, var2 ) = self.some_method()
EOS

is map_document( \$script ), $expected, "declare variables";

#########################

$script = <<'EOS';
if ( $line =~ /(\d+)\n/ ) {
    my $maxval = $1;
    $maxval = "$1";
    $maxval = "$var1 $1";
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r"(\d+)\n",line)
if   regex :
    maxval = regex.group(1)
    maxval = f"{regex.group(1)}"
    maxval = f"{var1} {regex.group(1)}"

EOS

is map_document( \$script ), $expected, "if + capture from regex";

$script = <<'EOS';
while ( $line =~ /(\d+)\n/ ) {
    $maxval .= $1;
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r"(\d+)\n",line)
while   regex :
    maxval += regex.group(1)

EOS

is map_document( \$script ), $expected, "while + capture from regex";

$script = <<'EOS';
if ( $line =~ /(\d+)\n/ ) {
    my $maxval = $1;
}
elsif ( $line =~ /(\w+)\n/ ) {
    my $maxval = $1;
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r"(\d+)\n",line)
regex2=re.search(r"(\w+)\n",line)
if   regex :
    maxval = regex.group(1)

elif   regex2 :
    maxval = regex2.group(1)

EOS

is map_document( \$script ), $expected, "if + elsif + capture from regex";

$script = <<'EOS';
if ( defined $line and $line =~ /(\d+)\n/ ) {
    my $maxval = $1;
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r"(\d+)\n",line)
if  (line is not None) and   regex :
    maxval = regex.group(1)

EOS

is map_document( \$script ), $expected, "if + regex + other conditions";

$script = <<'EOS';
if ( $line =~ /(\d+)\n/ ) {
    $maxval = $1;
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r"(\d+)\n",line)
if   regex :
    maxval = regex.group(1)

EOS

is map_document( \$script ), $expected, "if + capture from regex - no my/our";

$script = <<'EOS';
if ( $line =~ /(\d*)[ ](\d*)\n/ ) {
    ( $vara, $varb ) = ( $1, $2 );
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r"(\d*)[ ](\d*)\n",line)
if   regex :
    ( vara, varb ) = ( regex.group(1), regex.group(2) )

EOS

is map_document( \$script ), $expected, "multiple capture groups";

$script = <<'EOS';
if ( $line =~ /(\d*)[ ](\d*)\n/ ) {
    if ( defined $1 ) {
        print $1, "\n";
    }
}
elsif ( $line =~ /(\w*)[ ](\w*)\n/ ) {
    if ( defined $2 ) {
        print $2, "\n";
    }
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r"(\d*)[ ](\d*)\n",line)
regex2=re.search(r"(\w*)[ ](\w*)\n",line)
if   regex :
    if  (regex.group(1) is not None) :
        print(regex.group(1))  


elif   regex2 :
    if  (regex2.group(2) is not None) :
        print(regex2.group(2))  


EOS

is map_document( \$script ), $expected, "capture group + defined";

$script = <<'EOS';
if ( $line =~ /(\d+)\n/ ) {
    my @values = split /\s+/sm, $1;
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r"(\d+)\n",line)
if   regex :
    values = re.split(r"\s+",regex.group(1))  

EOS

is map_document( \$script ), $expected, "split + regex capture group";

$script = <<'EOS';
if ( $var1 =~ /[.]/ or $var2 =~ /[.]/ ) {
    return;
}
EOS

$expected = <<'EOS';
import re
if   re.search(r"[.]",var1) or   re.search(r"[.]",var2) :
    return

EOS

is map_document( \$script ), $expected, "multiple regex matches";

$script = <<'EOS';
if ( $line =~ /(\d+)\n/ ) {
    try {
        some_function_that_can_die( $1 );
    }
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r"(\d+)\n",line)
if   regex :
    try :
        some_function_that_can_die( regex.group(1) )


EOS

is map_document( \$script ), $expected, "regex capture within subblock";

$script = <<'EOS';
if ( $var1 =~ /^[.]{2}$value1 $value2$/ ) {
    return;
}
EOS

$expected = <<'EOS';
import re
if   re.search(fr"^[.]{{2}}{value1} {value2}$",var1) :
    return

EOS

is map_document( \$script ), $expected, "regex with interpreted variable";

$script = <<'EOS';
if ( $var1 =~ qr{ (-?\d+[.]?\d*) }xsm ) {
    $var2 = $1;
    if ( $var2 =~ /[.]/xsm or $var3 =~ /[.]/xsm ) {
        return $var2;
    }
}
elsif ( $var1 =~ /^<(\w+)>($list)?$/xsm ) {
    if ( $1 eq 'float' ) {
        return $1;
    }
    elsif ( $1 eq 'string' ) {
        return $1;
    }
    if ( defined $2 ) {
        if ( $2 eq 'a string' ) {
            return 0;
        }
        else {
            return $2;
        }
        return $2;
    }
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r" (-?\d+[.]?\d*) ",var1,re.MULTILINE|re.DOTALL|re.VERBOSE)
regex2=re.search(fr"^<(\w+)>({list})?$",var1,re.MULTILINE|re.DOTALL|re.VERBOSE)
if   regex :
    var2 = regex.group(1)
    if   re.search(r"[.]",var2,re.MULTILINE|re.DOTALL|re.VERBOSE) or   re.search(r"[.]",var3,re.MULTILINE|re.DOTALL|re.VERBOSE) :
        return var2


elif   regex2 :
    if regex2.group(1) == 'float' :
        return regex2.group(1)
 
    elif regex2.group(1) == 'string' :
        return regex2.group(1)

    if  (regex2.group(2) is not None) :
        if regex2.group(2) == 'a string' :
            return 0
 
        else :
            return regex2.group(2)

        return regex2.group(2)


EOS

is map_document( \$script ), $expected, "nested ifs with regex";

$script = <<'EOS';
if (  $line =~ qr/(\d+)\n/ ) {
    $maxval = $1;
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r"(\d+)\n",line)
if   regex :
    maxval = regex.group(1)

EOS

is map_document( \$script ), $expected, "regex match with qr//";

$script = <<'EOS';
if (
    $line =~ m{[(]\s+".*" # comment
[)]}xsmo
          )
        {
    $vara = 2;
}
EOS

$expected = <<'EOS';
import re
if   re.search(r"""[(]\s+".*" # comment
[)]""",line,re.MULTILINE|re.DOTALL|re.VERBOSE)         :
    vara = 2

EOS

is map_document( \$script ), $expected, "regex flags";

$script = <<'EOS';
if (  $line =~ /[[:alpha:]]/ ) {
    return;
}
EOS

$expected = <<'EOS';
import re
if   re.search(r"[A-Za-z]",line) :
    return

EOS

is map_document( \$script ), $expected, "regex with character classes";

$script = <<'EOS';
$data =~ s/in/out/s;
EOS

$expected = <<'EOS';
import re
data = re.sub(r"in",r"out",data,count=1,flags=re.DOTALL)
EOS

is map_document( \$script ), $expected, "regex substitute";

$script = <<'EOS';
$data =~ s/in/out/g;
$ahash{key} =~ s/in/out/g;
EOS

$expected = <<'EOS';
import re
data = re.sub(r"in",r"out",data)
ahash["key"] = re.sub(r"in",r"out",ahash["key"])
EOS

is map_document( \$script ), $expected, "regex substitute + global flag";

$script = <<'EOS';
$data =~ s/$in/$out/g;
EOS

$expected = <<'EOS';
import re
data = re.sub(in,out,data)
EOS

is map_document( \$script ), $expected, "regex substitute with variables";

$script = <<'EOS';
for ( @{$array} ) {
    s{in}{out}g;
}
EOS

$expected = <<'EOS';
import re
for _ in  array  :
    _=re.sub(r"in",r"out",_)

EOS

is map_document( \$script ), $expected, "regex substitute + magic variables";

$script = <<'EOS';
$data =~ s{\\(?:([0-7]{1,3})|(.))} {defined($1) ? chr(oct($1)) : $2}eg;
EOS

$expected = <<'EOS';
import re
def anonymous_01(match):
    return       chr(oct(match[1])) if (match[1] is not None)  else     match[2]

data = re.sub(r"\\(?:([0-7]{1,3})|(.))",anonymous_01,data)
EOS

is map_document( \$script ), $expected, "substitution with expression";

#########################

$script = <<'EOS';
if (1) {
if ( $line =~ /(\d+)\n/ ) {
    my $maxval = $1;
}}
EOS

$expected = <<'EOS';
import re
if 1 :
    regex=re.search(r"(\d+)\n",line)
    if   regex :
        maxval = regex.group(1)

EOS

is map_document( \$script ), $expected, "indenting of new lines";

$script = <<'EOS';
if ( $cond1
     and $cond2 ) {
    print "true\n";
}
EOS

$expected = <<'EOS';
if cond1     and cond2 :
    print("true") 

EOS

is map_document( \$script ), $expected,
  "remove new lines that aren't enclosed by parens";

$script = <<'EOS';
if ( $obj->Get('version') =~ /(\d+)\n/ ) { return }
EOS

$expected = <<'EOS';
import re
if   re.search(r"(\d+)\n",obj.Get('version')) :
    return
EOS

is map_document( \$script ), $expected, "support precedence";

#########################

$script = <<'EOS';
@new = grep { /^0$/xsm } @old;
EOS

$expected = <<'EOS';
import re
new = [x for x in old if re.search(r"^0$",x)]
EOS

is map_document( \$script ), $expected, "map grep->list comprehension";

$script = <<'EOS';
@new = grep { !/^0$/xsm } @old;
EOS

$expected = <<'EOS';
import re
new = [x for x in old if not re.search(r"^0$",x)]
EOS

is map_document( \$script ), $expected, "map !->not, more precedence";

$script = <<'EOS';
my @paths = split ':', $path;
EOS

$expected = <<'EOS';
paths = path.split(':')  
EOS

is map_document( \$script ), $expected, "split on string";

$script = <<'EOS';
my @paths = split /:/xsm, $path;
EOS

$expected = <<'EOS';
import re
paths = re.split(r":",path)  
EOS

is map_document( \$script ), $expected, "split on regex";

$script = <<'EOS';
for ( @array ) {
    my @paths = split /:/xsm;
}
EOS

$expected = <<'EOS';
import re
for _ in  array  :
    paths = re.split(r":",_) 

EOS

is map_document( \$script ), $expected, "split magic on regex";

$script = <<'EOS';
for my $i ( 0 .. $#{ $self->{array} } ) {
    print $i,"\n";
}
EOS

$expected = <<'EOS';
for  i in  range(len( self.array )-1+1)    :
    print(i) 

EOS

is map_document( \$script ), $expected, "map magic array length";

#########################

$script = <<'EOS';
package MyModule::MyPackage;
$CLASS_VAR          = 4;
our $VERSION = 1;
sub random_sub {
    return
}
1;
__END__
EOS

$expected = <<'EOS';

CLASS_VAR          = 4
VERSION = 1
def random_sub() :
    return


EOS

is map_document( \$script ), $expected, "package -> collection of defs";

$script = <<'EOS';
package MyModule::MyPackage;
$class_var          = 4;
our $GLOBAL_VAR = 1;
sub new {
    my ( $class, %options ) = @_;
    my $self = {};
    bless $self, $class;
    return
}
sub non_class_method {
    my ( $arg ) = @_;
    return;
}
sub class_method {
    my ( $self, $arg ) = @_;
    return;
}
1;
__END__
EOS

$expected = <<'EOS';
GLOBAL_VAR = 1
class MyPackage():
    class_var          = 4

    def __init__( self, options ) :
    
    
    
        return


    def class_method( self, arg ) :
    
        return



def non_class_method( arg ) :
    
    return
EOS

is map_document( \$script ), $expected, "package -> class";

$script = <<'EOS';
package MyModule::MyPackage;
$class_var          = 4;
our $VERSION = 1;
sub new_from_data {
    my ( $class, $data ) = @_;
    return $class->new();
}
1;
__END__
EOS

$expected = <<'EOS';
VERSION = 1
class MyPackage():
    class_var          = 4

    def new_from_data( self, data ) :
    
        return __class__()


EOS

is map_document( \$script ), $expected,
  "package -> class with more types of new()";

$script = <<'EOS';
package MyModule::MyPackage;
$class_var          = 4;
our $VERSION = 1;
use Exporter ();
use base qw(Exporter My::ParentPackage);
sub new {
    my ( $class, %options ) = @_;
    my $self = {};
    bless $self, $class;
    return
}
1;
__END__
EOS

$expected = <<'EOS';

VERSION = 1
class MyPackage(My.ParentPackage):
    class_var          = 4


    def __init__( self, options ) :
    
    
    
        return


EOS

is map_document( \$script ), $expected, "package -> class+subclass";

$script = <<'EOS';
package MyModule::MyPackage;
use Exporter ();
use base qw(Exporter);
sub some_method {
    return;
}
EOS

$expected = <<'EOS';


def some_method() :
    return

EOS

is map_document( \$script ), $expected, "package -> no class+no parent";

#########################

$script = <<'EOS';
$line =
    run_function($filename);
EOS

$expected = <<'EOS';
line =     run_function(filename)
EOS

is map_document( \$script ), $expected, "remove linebreaks inside statements";

#########################

$script = <<'EOS';
$result = $result_of_expression ? $result_if_true : $result_if_false;
EOS

$expected = <<'EOS';
result =  result_if_true if result_of_expression  else result_if_false
EOS

is map_document( \$script ), $expected, "ternary operator";

$script = <<'EOS';
$result = $result_of_expression ? $ahash{'key with space'} : $ahash{'key with another space'};
EOS

$expected = <<'EOS';
result =  ahash['key with space'] if result_of_expression  else ahash['key with another space']
EOS

is map_document( \$script ), $expected,
  "ternary operator with more complex arguments";

$script = <<'EOS';
$result = $result_of_expression ? TRUE : FALSE;
EOS

$expected = <<'EOS';
result =  True if result_of_expression else False
EOS

# https://github.com/Perl-Critic/PPI/issues/262
is map_document( \$script ), $expected, "workaround PPI bug #262";

$script = <<'EOS';
return $result_of_expression
  ? sprintf $FORMAT1, @args1
  : sprintf $FORMAT2, @args2;
EOS

$expected = <<'EOS';
return    FORMAT1 % (args1) if result_of_expression      else FORMAT2 % (args2)  
EOS

is map_document( \$script ), $expected, "ternary + built-in";

$script = <<'EOS';
return $var1 eq $val ? 0 : sprintf( '%.1g', $var1 ),
      $var2 eq $val ? 0 : sprintf '%.1g', $var2;
EOS

$expected = <<'EOS';
return    0 if var1==val  else '%.1g' % (  var1 ),          0 if var2==val  else '%.1g' % (var2)  
EOS

is map_document( \$script ), $expected, "list of ternaries + built-ins";

$script = <<'EOS';
while ( $step > 0 ? ( $i <= $e and $i < @{ $self->{data} } ) : $i >= $e ) {
    return $i;
}
EOS

$expected = <<'EOS';
while    ( i <= e and i < len( self.data ) ) if step>0  else i >= e :
    return i

EOS

is map_document( \$script ), $expected, "parens in ternary";

$script = <<'EOS';
my %ahash = ( sentinel => \$sentinel, ( $data ? %{$data} : () ) );
$last_index = $#array;
$last_index = $#{$array};
$last_index = $#{ $self->{array} };
if (@array == 2 or @array > 2 or @array < 2) {
    print @array, "\n";
}
return scalar @{ $self->{array} };
EOS

$expected = <<'EOS';
ahash = { "sentinel" : sentinel, (  data if data  else () ) }
last_index = len(array)-1
last_index = len(array)-1
last_index = len( self.array )-1
if len(array) == 2 or len(array) > 2 or len(array) < 2 :
    print(array)  

return len(self.array) 
EOS

is map_document( \$script ), $expected, "various casts";

$script = <<'EOS';
my @results = split $var1, sprintf $hash{key}, $var2, $var3;
EOS

$expected = <<'EOS';
results = split(var1,hash["key"] % (var2,var3))     
EOS

is map_document( \$script ), $expected, "sprintf with complex arguments";

$script = <<'EOS';
sprintf __('i %d'), $i;
EOS

$expected = <<'EOS';
__('i %d') % (i)  
EOS

is map_document( \$script ), $expected, "sprintf with complex arguments #2";

$script = <<'EOS';
override(
    'set_option' => sub {
        return defined $var ? sprintf( '%d', $var ) : 'false'
    }
);
EOS

$expected = <<'EOS';
def anonymous_02():
    return   '%d' % (  var ) if (var is not None)  else 'false'


override(
    set_option = anonymous_02 
)
EOS

is map_document( \$script ), $expected,
  "anonymous sub/defined/sprintf/ternary combination";

$script = <<'EOS';
if ( defined $self->by_name('source') ) {
    $self->{source} = $self->by_name('source');
}
EOS

$expected = <<'EOS';
if  (self.by_name('source') is not None) :
    self.source = self.by_name('source')

EOS

is map_document( \$script ), $expected,
  "precendency/associativity defined + oo method call";

$script = <<'EOS';
sub source_defined {
    my ($self) = @_;
    return ( defined $self->{source} );
}
EOS

$expected = <<'EOS';
def source_defined(self) :
    
    return (  (self.source is not None) )

EOS

is map_document( \$script ), $expected, "defined + class variable";

#########################

$script = <<'EOS';
open my $fh, '<', $filename;
my $line = <$fh>;
close $fh;
open my $fh, '<', $filename or return;
close($fh);
l = length $line;
unlink $filename;
last;
push @my_array, $item;
push @{ $self->{data} }, $item;
unshift @my_array, $item;
splice @{ $self->{data} }, $i - 1, 1;
delete $ahash{key};
undef $ahash{key};
$ahash{key} = undef;
$HEX_FF = hex 'ff';
ref($object);
$ahash{key}++;
EOS

$expected = <<'EOS';
import os
fh=open(filename,mode='r')    
line = fh.readline()
fh.close() 
try:
    fh=open(filename,mode='r')
except:
    return
fh.close()
l = len(line) 
os.remove(filename) 
break
my_array.append(item)  
self.data.append(item)  
my_array.insert(0,item)  
del(self.data[i-1])     
del(ahash["key"]) 
ahash["key"]=None
ahash["key"] = None
HEX_FF = hex('ff') 
type(object)
ahash["key"]+=1
EOS

is map_document( \$script ), $expected, "more built-ins";

$script = <<'EOS';
eval "1 + 1"
EOS

$expected = <<'EOS';
eval("1 + 1") 
EOS

is map_document( \$script ), $expected, "eval";

$script = <<'EOS';
my $pdfobj = PDF::Builder->open( $options{info}{path} );
EOS

$expected = <<'EOS';
pdfobj = PDF.Builder.open( options["info"]["path"] )
EOS

is map_document( \$script ), $expected, "looks like a built-in but isn't";

$script = <<'EOS';
while ( $line = <$fh> ) {
    print line
}
EOS

$expected = <<'EOS';
for line in fh :
    print(line) 

EOS

is map_document( \$script ), $expected, "map while reading a line at a time";

$script = <<'EOS';
unlink $filename, <$dir/*>;
EOS

$expected = <<'EOS';
import glob
import os
os.remove([filename]+glob.glob(f"{dir}/*"))  
EOS

is map_document( \$script ), $expected, "glob + formatted string";

#########################

$script = <<'EOS';
if (1) {return} elsif (1) {return} else {return}
EOS

$expected = <<'EOS';
if 1 :
    return
elif 1 :
    return
else :
    return
EOS

is map_document( \$script ), $expected, "indent elif/else";

$script = <<'EOS';
if (1) {if (1) {return} else {return}}
EOS

$expected = <<'EOS';
if 1 :
    if 1 :
        return
    else :
        return
EOS

is map_document( \$script ), $expected, "indent else2";

$script = <<'EOS';
return $result if ( defined $result );
EOS

$expected = <<'EOS';
if (  (result is not None) ):
    return result  
EOS

is map_document( \$script ), $expected, "postfix if";

$script = <<'EOS';
package Gscan2pdf::Scanner::Options;
use Glib::Object::Subclass Glib::Object::;
sub by_title {
    my ( $self, $title ) = @_;
    for ( @{ $self->{array} } ) {
        return $_ if ( defined $_->{title}  );
    }
    return;
}
EOS

$expected = <<'EOS';
from gi.repository import GObject
class Options(GObject.Object):
    def __init__(self):
        GObject.GObject.__init__(self)
    def by_title( self, title ) :
    
        for _ in          self.array  :
            if (  "title"  in _  ):
                return _  

        return

EOS

is map_document( \$script ), $expected, "postfix if/subclass combination";

$script = <<'EOS';
if ( defined $ahash{key} ) {do_something()}
EOS

$expected = <<'EOS';
if  "key"  in ahash :
    do_something()
EOS

is map_document( \$script ), $expected, "map hash key defined";

$script = <<'EOS';
if ( defined $ahash->{key} ) {do_something()}
EOS

$expected = <<'EOS';
if  "key"  in ahash :
    do_something()
EOS

is map_document( \$script ), $expected, "map hashref key defined";

$script = <<'EOS';
if ( not defined( $ahash->{key} ) ) {do_something()}
EOS

$expected = <<'EOS';
if "key" not  in ahash :
    do_something()
EOS

is map_document( \$script ), $expected, "map hashref key not defined";

$script = <<'EOS';
if ( defined( $ahash->{scalar IMPORTED_SYMBOL} ) ) {do_something()}
EOS

$expected = <<'EOS';
if  IMPORTED_SYMBOL  in ahash :
    do_something()
EOS

is map_document( \$script ), $expected, "map imported symbol as hashref key";

$script = <<'EOS';
if ( defined $alist[i] ) {do_something()}
EOS

$expected = <<'EOS';
if  (alist[i] is not None) :
    do_something()
EOS

is map_document( \$script ), $expected, "map list element defined";

#########################

$script = <<'EOS';
given ( $result ) {
    when (/a/xsm) {
        return 0
    }
    when ('b') {
        return 1
    }
    when ($_ eq 'c' or $_ eq 'd') {
        return 3
    }
    default {
        return 2
    }
}
return;
EOS

$expected = <<'EOS';
import re
if re.search(r"a", result ):
    return 0

elif  result =='b':
    return 1

elif  result  == 'c' or  result  == 'd':
    return 3

else :
    return 2

return
EOS

is map_document( \$script ), $expected, "given/when->if/elif/else";

$script = <<'EOS';
given ( $vara ) {
    when ('S') {
        given ( $varb ) {
            when (/regex/xsm) {
                $data = 'data';
            }
        }
    }
}
EOS

$expected = <<'EOS';
import re
if  vara =='S':
    if re.search(r"regex", varb ):
        data = 'data'


EOS

is map_document( \$script ), $expected, "nested given";

#########################

$script = <<'EOS';
$line .= 'string';
$line = $line . 'string';
$line = 'string' . $line;
print $var1 ne $var2, "\n";
print $var1 eq $var2, "\n";
print $var1 ge $var2, "\n";
print $var1 le $var2, "\n";
print $var1 lt $var2, "\n";
print $var1 gt $var2, "\n";
return ($var1 xor $var2);
EOS

$expected = <<'EOS';
line += 'string'
line = line + 'string'
line = 'string' + line
print(var1!=var2)    
print(var1==var2)    
print(var1>=var2)    
print(var1<=var2)    
print(var1<var2)    
print(var1>var2)    
return (var1 ^ var2)
EOS

is map_document( \$script ), $expected, "operators";

#########################

$script = <<'EOS';
use IPC::System::Simple qw(system);
system( qw(ls -l) );
system( 'ls', '-l' );
system( qw(ls -l), 'file with a space' );
system( qw(ls -l), $variable );
EOS

$expected = <<'EOS';
import subprocess
subprocess.run([ "ls","-l" ])
subprocess.run([ 'ls', '-l' ])
subprocess.run([ "ls","-l", 'file with a space' ])
subprocess.run([ "ls","-l", str(variable) ])
EOS

is map_document( \$script ), $expected, "subprocess";

#########################

$script = <<'EOS';
BEGIN {
    use_ok('MyModule::MySubModule::MySubSubModule');
}
EOS

$expected = <<'EOS';

import MyModule.MySubModule.MySubSubModule

EOS

is map_document( \$script ), $expected, "scheduled blocks";

#########################

$script = <<'EOS';
for my $type (qw(pbm pgm ppm)) {
    print $type
}
EOS

$expected = <<'EOS';
for  type in ["pbm","pgm","ppm"] :
    print(type) 

EOS

is map_document( \$script ), $expected, "iterator over array";

$script = <<'EOS';
for my $type (qw(pbm pgm ppm)) {
    if ( $type =~ /(\w)/ ) {
        print $type
    }
}
EOS

$expected = <<'EOS';
import re
for  type in ["pbm","pgm","ppm"] :
    if   re.search(r"(\w)",type) :
        print(type) 


EOS

is map_document( \$script ), $expected, "iterator over array + regex";

$script = <<'EOS';
for (qw(pbm pgm ppm)) {
    if ( /(\w)/ ) {
        print $_
    }
}
EOS

$expected = <<'EOS';
import re
for _ in ["pbm","pgm","ppm"] :
    if re.search(r"(\w)",_) :
        print(_) 


EOS

is map_document( \$script ), $expected, "magic + regex";

$script = <<'EOS';
for (qw(pbm pgm ppm)) {
    if ( defined($something) and /(\w)/ ) {
        print $_
    }
}
EOS

$expected = <<'EOS';
import re
for _ in ["pbm","pgm","ppm"] :
    if (something is not None) and re.search(r"(\w)",_) :
        print(_) 


EOS

is map_document( \$script ), $expected, "magic + regex2";

$script = <<'EOS';
for (@my_list) {
    if ( not defined ) {
        next
    }
}
EOS

$expected = <<'EOS';
for _ in my_list :
    if  (_ is None) :
        next()


EOS

is map_document( \$script ), $expected, "defined + magic";

$script = <<'EOS';
$count = () = $data =~ /\w/g;
EOS

$expected = <<'EOS';
import re
count =     len(re.findall(r"\w",data))
EOS

is map_document( \$script ), $expected, "regex count matches";

#########################

$script = <<'EOS';
my $size = -s $file;
EOS

$expected = <<'EOS';
import os
size = os.path.getsize(file) 
EOS

is map_document( \$script ), $expected, "-s -> getsize()";

#########################

$script = <<'EOS';
my $test = -f $file;
EOS

$expected = <<'EOS';
import os
test = os.path.isfile(file) 
EOS

is map_document( \$script ), $expected, "-f -> isfile()";

#########################

$script = <<'EOS';
local $SIG{__WARN__} = sub {
    $logger->warn(@_);
};
EOS

$expected = <<'EOS';
import logging
logging.captureWarnings(True)
EOS

is map_document( \$script ), $expected, "log warnings";

#########################

$script = <<'EOS';
%my_hash = {
    key  => "value",
    key2 => "value2",
}
EOS

$expected = <<'EOS';
my_hash = {
    "key"  : "value",
    "key2" : "value2",
}
EOS

is map_document( \$script ), $expected, "map hash->dict";

$script = <<'EOS';
@my_array = (1, 2, 3);
EOS

$expected = <<'EOS';
my_array = [1, 2, 3]
EOS

is map_document( \$script ), $expected, "map array->list";

$script = <<'EOS';
$my_array = [undef, {}];
EOS

$expected = <<'EOS';
my_array = [None, {}]
EOS

is map_document( \$script ), $expected, "map arrayref->list";

$script = <<'EOS';
for ( keys %options ) {
    print $_;
}
for ( keys %{$options} ) {
    print $_;
}
for ( values %options ) {
    print $_;
}
for ( sort %my_list ) {
    print $_;
}
for ( sort keys %options ) {
    print $_;
}
EOS

$expected = <<'EOS';
for _ in  options.keys()   :
    print(_) 

for _ in  options.keys()   :
    print(_) 

for _ in  options.values()   :
    print(_) 

for _ in  sorted(my_list)   :
    print(_) 

for _ in  sorted(options.keys())    :
    print(_) 

EOS

is map_document( \$script ), $expected, "sort(), keys() & values()";

$script = <<'EOS';
add_column_type('hstring', type => 'Glib::Scalar', attr => 'hidden', 'property-with-hyphen' => 'value');
EOS

$expected = <<'EOS';
add_column_type('hstring', type = 'Glib::Scalar', attr = 'hidden', property_with_hyphen = 'value')
EOS

is map_document( \$script ), $expected, "hash -> named arguments";

$script = <<'EOS';
method_call( [ $list_item ], key1 => "value1", key2 => "value2" );
EOS

$expected = <<'EOS';
method_call([
list_item ], key1 = "value1", key2 = "value2" )
EOS

is map_document( \$script ), $expected, "hash -> named arguments #2";

$script = <<'EOS';
function_with_callback( callback => sub { return "result" } );
EOS

$expected = <<'EOS';
def anonymous_03():
    return "result"

function_with_callback( callback = anonymous_03  )
EOS

is map_document( \$script ), $expected, "name anonymous subs";

$script = <<'EOS';
function_with_callback( callback => sub { return update_something(@_) } );
EOS

$expected = <<'EOS';
def anonymous_04(*argv):
    return update_something(*argv)

function_with_callback( callback = anonymous_04  )
EOS

is map_document( \$script ), $expected, "magic in anonymous subs";

$script = <<'EOS';
cmd(
    callback     => sub {
        my ($line) = @_;
        given ($line) {
            when ($mess) {
                return;
            }
        }
    }
);
EOS

$expected = <<'EOS';
def anonymous_05(line):
        
    if line==mess:
        return



cmd(
    callback     = anonymous_05 
)
EOS

is map_document( \$script ), $expected, "given in anonymous sub";

$script = <<'EOS';
sub iterator {
    my ( $option ) = @_;
    my $iter;
    return sub {
        my ($option2) = @_;
        return $iter;
    };
}
EOS

$expected = <<'EOS';
def iterator( option ) :
    
    iter=None
    def anonymous_06(option2):
        
        return iter


    return anonymous_06 

EOS

is map_document( \$script ), $expected, "more magic in anonymous subs/closure";

$script = <<'EOS';
function_with_callback( callback => sub {  } );
EOS

$expected = <<'EOS';
def anonymous_07():
    pass

function_with_callback( callback = anonymous_07  )
EOS

is map_document( \$script ), $expected, "empty anonymous sub";

#########################

$script = <<'EOS';
my $iter = method_returning_iterator();
while ( my $item = $iter->() ) {
    print $iter, "\n";
}
EOS

$expected = <<'EOS';
iter = method_returning_iterator()
for item in iter :
    print(iter)  

EOS

is map_document( \$script ), $expected, "map iter";

#########################

$script = <<'EOS';
my $output = do { local ( @ARGV, $/ ) = $filename; <> };
EOS

$expected = <<'EOS';
with open(filename,"r") as   fd :
    output=fd.read()
EOS

is map_document( \$script ), $expected, "slurp file";

#########################

$script = <<'EOS';
log($variable);
EOS

$expected = <<'EOS';
import math
math.log(variable)
EOS

is map_document( \$script ), $expected, "maths functions";

#########################

$script = <<'EOS';
use Log::Log4perl qw(:easy);
Log::Log4perl->easy_init($ERROR);
EOS

$expected = <<'EOS';
import logger

EOS

is map_document( \$script ), $expected, "map log4perl -> logger";

#########################

$script = <<'EOS';
croak 'Error: filename not supplied'
EOS

$expected = <<'EOS';
raise 'Error: filename not supplied'
EOS

is map_document( \$script ), $expected, "map croak -> raise";

#########################

$script = <<'EOS';
use Try::Tiny;
    try {
        some_method_that_can_fail();
    }
    catch {
        print "failed\n";
    };
EOS

$expected = <<'EOS';

try :
    some_method_that_can_fail()
 
except :
    print("failed") 

EOS

is map_document( \$script ), $expected, "map try/catch->try/except";

#########################

$script = <<'EOS';
use File::Copy;
move $old, $new or return;
move($old, $new);
copy($old, $new);
EOS

$expected = <<'EOS';
import shutil
import os
try:
    os.rename(old,new)
except:
    return
os.rename(old, new)
shutil.copy2(old, new)
EOS

is map_document( \$script ), $expected, "map File::Copy";

#########################

$script = <<'EOS';
use File::Temp;
$filename = File::Temp->new;
$filename2 = File::Temp->new(
    DIR    => $dir,
    SUFFIX => $suffix,
    UNLINK => FALSE,
);
EOS

$expected = <<'EOS';
import tempfile
filename = tempfile.TemporaryFile()
filename2 = tempfile.NamedTemporaryFile(
    dir    = dir,
    suffix = suffix,
    delete = False,
)
EOS

is map_document( \$script ), $expected, "map File::Temp->tempfile";

#########################

$script = <<'EOS';
use Image::Magick;
my $image  = Image::Magick->new;
EOS

$expected = <<'EOS';
import PythonMagick
image  = PythonMagick.Image()
EOS

is map_document( \$script ), $expected, "map Image::Magick->PythonMagick";

#########################

$script = <<'EOS';
use POSIX qw(locale_h);
setlocale( LC_NUMERIC, 'C' );
EOS

$expected = <<'EOS';
import locale
locale.setlocale( locale.LC_NUMERIC, 'C' )
EOS

is map_document( \$script ), $expected, "map locale";

$script = <<'EOS';
use POSIX qw(strftime);
my $result = POSIX::strftime($template, $sec, $min, $hour, $day, $month, $year);
EOS

$expected = <<'EOS';
import datetime
result = datetime.datetime.date(year ,month ,day ,hour ,min ,sec ).strftime(template)
EOS

is map_document( \$script ), $expected, "map POSIX::strftime()->datetime";

$script = <<'EOS';
use POSIX qw(:sys_wait_h);
waitpid $pid, 0;
EOS

$expected = <<'EOS';
import os
os.waitpid(pid,0)  
EOS

is map_document( \$script ), $expected, "map waitpid";

#########################

$script = <<'EOS';
use Data::UUID;
my $uuid_obj = Data::UUID->new;
$uuid_str = $uuid_obj->create_str();
EOS

$expected = <<'EOS';
import uuid
uuid_obj = uuid.uuid1()
uuid_str = str(uuid_obj())
EOS

is map_document( \$script ), $expected, "map Data::UUID->uuid";

#########################

$script = <<'EOS';
$multilinestring = <<'END';
line1
line2
END
EOS

$expected = <<'EOS';
multilinestring = """line1
line2
"""
EOS

is map_document( \$script ), $expected, "map heredoc->multiline string";

#########################

$script = <<'EOS';
use Storable qw(dclone);
my $clone = dclone( $symbol );
EOS

$expected = <<'EOS';
from copy import deepcopy
clone = deepcopy( symbol )
EOS

is map_document( \$script ), $expected, "dclone -> copy.deepcopy";

#########################

$script = <<'EOS';
if ( ref($my_object) eq 'ARRAY' ) {
    do_something();
}
EOS

$expected = <<'EOS';
if isinstance(my_object,list)   :
    do_something()

EOS

is map_document( \$script ), $expected, "map ref() eq ... -> isinstance()";

#########################

$script = <<'EOS';
my $next_value = $iter->();
$next_value = $iter->(0);
EOS

$expected = <<'EOS';
next_value = next(iter)
next_value = next(iter)
EOS

is map_document( \$script ), $expected, "closure -> iterator";

#########################

__END__
