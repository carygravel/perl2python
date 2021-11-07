use warnings;
use strict;
use English qw( -no_match_vars );    # for $INPUT_RECORD_SEPARATOR
use perl2python qw(parse_document);
use Test::More tests => 1;

sub slurp {
    my ($file) = @_;
    open my $fh, '<', $file;
    local $INPUT_RECORD_SEPARATOR = undef;
    my $text = <$fh>;
    close $fh;
    return $text;
}

my $script = <<'EOS';
#!/usr/bin/perl
use warnings;
use strict;
print "Hello world!\n";
EOS

my $expected = <<'EOS';
#!/usr/bin/python3
print( "Hello world!" )
EOS

#my $in = 'test.pl';
#my $out = 'test.py';
#open my $fh, '>', $in;
#print $fh $script;
#close $fh;
#system("perl bin/perl2python $in");
is parse_document( \$script ), $expected, "Hello world";

#########################

#unlink $in, $out;

__END__
