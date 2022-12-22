use warnings;
use strict;
use English   qw( -no_match_vars );        # for $INPUT_RECORD_SEPARATOR
use Perl2Rust qw(map_document map_path);
use Test::More tests => 12;

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
use gtk;
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
    assert_eq!( MyClass::method(), 'return value', 'comment' );
    assert_eq!( MyClass::method(), 'return value', 'comment' );
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
use MyModule::MySubModule::MySubSubModule;
MyModule::MySubModule::MySubSubModule::my_method();
let var = MyModule::MySubModule::MODULE_CONSTANT;
EOS

is map_document( \$script ), $expected, "import";

$script = <<'EOS';
use MyModule::MySubModule::MySubSubModule 2.40;
EOS

$expected = <<'EOS';
use MyModule::MySubModule::MySubSubModule ;
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
use MyModule::symbol;
use MyModule::*;
use MyModule::{symbol1,symbol2};
use glib;
    # To get TRUE and FALSE
EOS

is map_document( \$script ), $expected,
  "map use with symbol, special casing import Glib";

$script = <<'EOS';
use Gtk3 0.028 -init;
my $window = Gtk3::Window->new;
$event = Gtk3::Gdk::Event->new('key-press');
$event->keyval(Gtk3::Gdk::KEY_Delete);
$val=$event->keyval();
$dialog->signal_connect_after( key_press_event => sub {} );
return Gtk3::EVENT_PROPAGATE;
$flags = ${ Gtk3::TargetFlags->new(qw/same-widget/) };
EOS

$expected = <<'EOS';
use gtk;
let window = gtk::Window::new();
event = gdk::Event::new('key-press');
event.keyval=gdk::KEY_Delete;
val=event.keyval();
dialog.connect_after( "key_press_event" , sub {} );
return gdk::EVENT_PROPAGATE;
flags = gtk::TargetFlags::new(["same-widget"]);
EOS

is map_document( \$script ), $expected, "special case import Gtk3/Gdk";
