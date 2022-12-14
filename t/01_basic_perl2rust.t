use warnings;
use strict;
use English qw( -no_match_vars );    # for $INPUT_RECORD_SEPARATOR
use Perl2Rust qw(map_document map_path);
use Test::More tests => 1;

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
