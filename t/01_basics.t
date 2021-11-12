use warnings;
use strict;
use English qw( -no_match_vars );    # for $INPUT_RECORD_SEPARATOR
use Perl2Python qw(parse_document);
use Test::More tests => 4;

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
sub function {
    my ( $x, $y, $t ) = @_;
    return $x, $y, $t;
}
EOS

$expected = <<'EOS';
def function($x, $y, $t):
    return($x, $y, $t)
EOS

is parse_document( \$script ), $expected, "sub";

#########################

unlink $in, $out;

__END__
