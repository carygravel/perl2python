use warnings;
use strict;
use English   qw( -no_match_vars );        # for $INPUT_RECORD_SEPARATOR
use Perl2Rust qw(map_document map_path);
use Test::More tests => 8;

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
use gtk ;
#[test]
fn test_1(){

    assert_eq!(result,expected,"comment")   ;
    assert_eq!(result,expected,"comment")   ;
    assert_eq!([
    1, 2, 3 ],[
    4, 5, 6 ],"comment")                               ;
    method_with_is();
    assert_eq!(result, expected, "comment");
    assert_eq!( hashref.get(&String::from("array")), that, 'comment' );
    assert_eq!( MyClass.method(), 'return value', 'comment' );
    assert_eq!( MyClass.method(), 'return value', 'comment' );
    assert_eq!( iter.next(), 'return value', 'comment' );
    assert!( object is My::Class );
    assert!(true,'comment') ;
    assert!( dialog.get('property') == 'value', 'comment' );
}
EOS

$in  = 'test.t';
$out = 'test.rs';
open $fh, '>', $in;
print $fh $script;
close $fh;
system("perl bin/perl2rust $in");
is slurp($out), $expected, "Basic test";
unlink $in, $out;

$script = <<'EOS';
if ( not eval { require MyPackage; } ) {
    plan( skip_all => "MyPackage required to run tests" );
}
EOS

$expected = <<'EOS';
match MyPackage::new() { Ok(package) => {}, Err(_) => { println!("MyPackage required to run tests"); } }
EOS

is map_document( \$script ), $expected, "conditionally skip tests";

$script = <<'EOS';
eval "use MyPackage";
plan skip_all => "MyPackage required" if $@;
EOS

$expected = <<'EOS';
match MyPackage::new() { Ok(package) => {}, Err(_) => { println!("MyPackage required"); } }

EOS

is map_document( \$script ), $expected, "conditionally skip more tests";
