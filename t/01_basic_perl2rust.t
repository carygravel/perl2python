use warnings;
use strict;
use English qw( -no_match_vars );    # for $INPUT_RECORD_SEPARATOR
use Perl2Rust qw(map_document map_path);
use Test::More tests => 5;

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
println!("Hello world!") ;
EOS

is map_document( \$script ), $expected, "print()";

my $in  = 'test.pl';
my $out = 'test.rs';
open my $fh, '>', $in;
print $fh $script;
close $fh;
system("perl bin/perl2rust $in");
is slurp($out), $expected, "Hello world";
unlink $in, $out;

#########################

is map_path('lib/package/module.pm'), 'package/module.rs', "map_path lib";
is map_path('base/lib/package/module.pm'), 'base/package/module.rs',
  "map_path lib2";
is map_path('t/01_basics.t'), 'tests/01_basics.rs', "map_path t";
