use warnings;
use strict;
use English qw( -no_match_vars );    # for $INPUT_RECORD_SEPARATOR
use Perl2Python qw(parse_document);
use Test::More tests => 11;

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

#########################

$script = <<'EOS';
use feature 'switch';
no if $] >= 5.018, warnings => 'experimental::smartmatch';
use MyModule::MySubModule::MySubSubModule;
EOS

$expected = <<'EOS';
import MyModule.MySubModule.MySubSubModule
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

#########################

$script = <<'EOS';
package MyModule::MyPackage;
$CLASS_VAR          = 4;
our $VERSION = 1
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
EOS

$expected = <<'EOS';
fh=open(    filename,mode='r')
EOS

is parse_document( \$script ), $expected, "more built-ins";

#########################

unlink $in, $out;

__END__
