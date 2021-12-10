use warnings;
use strict;
use English qw( -no_match_vars );    # for $INPUT_RECORD_SEPARATOR
use Perl2Python qw(parse_document);
use Test::More tests => 23;

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

is parse_document( \$script ), $expected, "print()";

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

is parse_document( \$script ), $expected, "import";

#########################

$script = <<'EOS';
use Readonly;
Readonly my $VARIABLE          => 4;
EOS

$expected = <<'EOS';
VARIABLE          = 4
EOS

is parse_document( \$script ), $expected, "readonly";

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

is parse_document( \$script ), $expected, "sub";

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

is parse_document( \$script ), $expected, "sub + shift";

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

is parse_document( \$script ), $expected, "if + capture from regex";

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

is parse_document( \$script ), $expected, "if + capture from regex - no my/our";

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

is parse_document( \$script ), $expected, "multiple capture groups";

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

is parse_document( \$script ), $expected, "indenting of new lines";

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

is parse_document( \$script ), $expected, "package";

#########################

$script = <<'EOS';
$line =
    run_function($filename);
EOS

$expected = <<'EOS';
line =     run_function(filename)
EOS

is parse_document( \$script ), $expected, "remove linebreaks inside statements";

#########################

$script = <<'EOS';
$result = $result_of_expression ? $result_if_true : $result_if_false;
EOS

$expected = <<'EOS';
result =  result_if_true if result_of_expression  else result_if_false
EOS

is parse_document( \$script ), $expected, "ternary operator";

#########################

$script = <<'EOS';
open my $fh, '<', $filename;
my $line = <$fh>;
close $fh;
open my $fh, '<', $filename or return;
close($fh);
l = length $line
EOS

$expected = <<'EOS';
fh=open(    filename,mode='r')
line = fh.readline()
fh.close( )
try:
    fh=open(    filename,mode='r' )
except:
    return
fh.close()
l = len( line)
EOS

is parse_document( \$script ), $expected, "more built-ins";

$script = <<'EOS';
while ( $line = <$fh> ) {
    print line
}
EOS

$expected = <<'EOS';
for line in fh :
    print( line)

EOS

is parse_document( \$script ), $expected, "map while reading a line at a time";

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

is parse_document( \$script ), $expected, "indent elif/else";

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

is parse_document( \$script ), $expected, "indent else2";

#########################

$script = <<'EOS';
$line .= 'string';
EOS

$expected = <<'EOS';
line += 'string'
EOS

is parse_document( \$script ), $expected, "operators";

#########################

$script = <<'EOS';
use IPC::System::Simple qw(system);
system( qw(ls -l) );
EOS

$expected = <<'EOS';
import subprocess
subprocess.run( ["ls","-l"] )
EOS

is parse_document( \$script ), $expected, "subprocess";

#########################

$script = <<'EOS';
BEGIN {
    use_ok('MyModule::MySubModule::MySubSubModule');
}
EOS

$expected = <<'EOS';

import MyModule.MySubModule.MySubSubModule

EOS

is parse_document( \$script ), $expected, "scheduled blocks";

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

is parse_document( \$script ), $expected, "iterator over array";

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

is parse_document( \$script ), $expected, "iterator over array + regex";

#########################

__END__
