use warnings;
use strict;
use English qw( -no_match_vars );    # for $INPUT_RECORD_SEPARATOR
use Perl2Python qw(map_document map_path);
use Test::More tests => 39;

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

print( "Hello world!")
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

$script = <<'EOS';
use Test::More tests => 14;
is $result, $expected, "comment";
EOS

$expected = <<'EOS';
def test_1():
    assert result== expected #  "comment"
EOS

$in  = 'test.t';
$out = 'test.py';
open $fh, '>', $in;
print $fh $script;
close $fh;
system("perl bin/perl2python $in");
is slurp($out), $expected, "Basic test";
unlink $in, $out;

#########################

$script = <<'EOS';
use feature 'switch';
no if $] >= 5.018, warnings => 'experimental::smartmatch';
use MyModule::MySubModule::MySubSubModule;
MyModule::MySubModule::MySubSubModule::my_method();
EOS

$expected = <<'EOS';
import MyModule.MySubModule.MySubSubModule
MyModule.MySubModule.MySubSubModule.my_method()
EOS

is map_document( \$script ), $expected, "import";

#########################

$script = <<'EOS';
use Readonly;
Readonly my $VARIABLE          => 4;
EOS

$expected = <<'EOS';
VARIABLE          = 4
EOS

is map_document( \$script ), $expected, "readonly";

#########################

$script = <<'EOS';
sub function {
    my ( $x, $y, $t ) = @_;
    return $x, $y, $t;
}
EOS

$expected = <<'EOS';
def function( x, y, t ) :
    
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

#########################

$script = <<'EOS';
if ( $line =~ /(\d+)\n/xsm ) {
    my $maxval = $1;
}
if ( defined $line and $line =~ /(\d+)\n/xsm ) {
    my $maxval = $1;
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r'(\d+)\n',line)
if   regex :
    maxval = regex.group(1)

regex=re.search(r'(\d+)\n',line)
if  line is not None and   regex :
    maxval = regex.group(1)

EOS

is map_document( \$script ), $expected, "if + capture from regex";

$script = <<'EOS';
if ( $line =~ /(\d+)\n/xsm ) {
    $maxval = $1;
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r'(\d+)\n',line)
if   regex :
    maxval = regex.group(1)

EOS

is map_document( \$script ), $expected, "if + capture from regex - no my/our";

$script = <<'EOS';
if ( $line =~ /(\d*)[ ](\d*)\n/xsm ) {
    ( $width, $height ) = ( $1, $2 );
}
EOS

$expected = <<'EOS';
import re
regex=re.search(r'(\d*)[ ](\d*)\n',line)
if   regex :
    ( width, height ) = ( regex.group(1), regex.group(2) )

EOS

is map_document( \$script ), $expected, "multiple capture groups";

$script = <<'EOS';
if (1) {
if ( $line =~ /(\d+)\n/xsm ) {
    my $maxval = $1;
}}
EOS

$expected = <<'EOS';
import re
if 1 :
    regex=re.search(r'(\d+)\n',line)
    if   regex :
        maxval = regex.group(1)

EOS

is map_document( \$script ), $expected, "indenting of new lines";

$script = <<'EOS';
if ( $obj->Get('version') =~ /(\d+)\n/xsm ) { return }
EOS

$expected = <<'EOS';
import re
if   re.search(r'(\d+)\n',obj.Get('version')) :
    return
EOS

is map_document( \$script ), $expected, "support precedence";

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
paths = path.split( ':' )
EOS

is map_document( \$script ), $expected, "split on string";

$script = <<'EOS';
my @paths = split /:/xsm, $path;
EOS

$expected = <<'EOS';
import re
paths = re.split( r":", path)
EOS

is map_document( \$script ), $expected, "split on regex";

#########################

$script = <<'EOS';
package MyModule::MyPackage;
$CLASS_VAR          = 4;
our $VERSION = 1;
1;
__END__
EOS

$expected = <<'EOS';
class MyPackage():
    CLASS_VAR          = 4
    VERSION = 1

EOS

is map_document( \$script ), $expected, "package";

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

#########################

$script = <<'EOS';
open my $fh, '<', $filename;
my $line = <$fh>;
close $fh;
open my $fh, '<', $filename or return;
close($fh);
l = length $line;
unlink $filename;
EOS

$expected = <<'EOS';
import os
fh=open(    filename,mode='r')
line = fh.readline()
fh.close( )
try:
    fh=open(    filename,mode='r' )
except:
    return
fh.close()
l = len( line)
os.remove( filename)
EOS

is map_document( \$script ), $expected, "more built-ins";

$script = <<'EOS';
while ( $line = <$fh> ) {
    print line
}
EOS

$expected = <<'EOS';
for line in fh :
    print( line)

EOS

is map_document( \$script ), $expected, "map while reading a line at a time";

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
if ( defined $ahash{key} ) {do_something()}
EOS

$expected = <<'EOS';
if  'key' in ahash :
    do_something()
EOS

is map_document( \$script ), $expected, "map key defined";

#########################

$script = <<'EOS';
given ( $result ) {
    when (/a/xsm) {
        return 0
    }
    when ('b') {
        return 1
    }
    default {
        return 2
    }
}
EOS

$expected = <<'EOS';
import re


if re.search(r"a", result ):
    return 0

elif  result =='b':
    return 1

else :
    return 2
EOS

is map_document( \$script ), $expected, "given/when->if/elif/else";

#########################

$script = <<'EOS';
$line .= 'string';
EOS

$expected = <<'EOS';
line += 'string'
EOS

is map_document( \$script ), $expected, "operators";

#########################

$script = <<'EOS';
use IPC::System::Simple qw(system);
system( qw(ls -l) );
EOS

$expected = <<'EOS';
import subprocess
subprocess.run( ["ls","-l"] )
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
    print( type)

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
    if   re.search(r'(\w)',type) :
        print( type)


EOS

is map_document( \$script ), $expected, "iterator over array + regex";

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
function_with_callback( callback => sub { return "result" } );
EOS

$expected = <<'EOS';
def anonymous_01():
    return "result"

function_with_callback( {"callback" : anonymous_01 } )
EOS

is map_document( \$script ), $expected, "name anonymous subs";

$script = <<'EOS';
function_with_callback( callback => sub { return update_something(@_) } );
EOS

$expected = <<'EOS';
def anonymous_02(*argv):
    return update_something(*argv)

function_with_callback( {"callback" : anonymous_02 } )
EOS

is map_document( \$script ), $expected, "magic in anonymous subs";

#########################

is map_path('lib/package/module.pm'), 'package/module.py', "map_path lib";
is map_path('base/lib/package/module.pm'), 'base/package/module.py',
  "map_path lib2";
is map_path('t/01_basics.t'), 'tests/test_01_basics.py', "map_path t";

#########################

__END__
