use strict;
use warnings;
use 5.008;
use ExtUtils::MakeMaker;
use ExtUtils::Depends;
use ExtUtils::PkgConfig;
use English;

# minimum required version of dependancies we need to build
our %build_reqs = ();

# minimum required version of dependancies we need to run
our %runtime_reqs = ();

# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    NAME         => 'perl2python',
    VERSION_FROM => 'lib/Perl2Python.pm',    # finds $VERSION
    PREREQ_PM    => {
        Readonly => 0,
    },
    CONFIGURE_REQUIRES => {
        'ExtUtils::Depends'   => 0,
        'ExtUtils::PkgConfig' => 0,
    },
    TEST_REQUIRES => {
        'File::Spec'     => 0,
        'File::Temp'     => 0.19,
        'Test::More'     => 0,
        'Test::Requires' => 0,
        'Test::Deep'     => 0,
    },
    META_MERGE => {
        'meta-spec' => { version => 2 },
        resources   => {
            repository => {
                type => 'git',
                url  => 'https://github.com/carygravel/perl2python.git',
                web  => 'https://github.com/carygravel/perl2python',
            },
        },
    },
    clean => { FILES => '$(SOURCE_TIDY)' },

    # CPAN does not recognise .xz encoded files
    #    dist  => { COMPRESS => 'xz -9', SUFFIX => '.xz', },
    (
        $] >= 5.005
        ?    ## Add these new keywords supported since 5.005
          (
            ABSTRACT_FROM =>
              'lib/Perl2Python.pm',    # retrieve abstract from module
            AUTHOR => 'Jeffrey Ratcliffe'
          )
        : ()
    ),
    LIBS   => [],    # e.g., '-lm'
    DEFINE => '',    # e.g., '-DHAVE_SOMETHING'
    INC    => '',    # e.g., '-I. -I/usr/include/other'
                     # Un-comment this if you add C files to link with later:
         # OBJECT            => '$(O_FILES)', # link all the C files too
);

sub MY::postamble {

    # GNU Make extensions that BSD make doesn't like.
    # Author-only stuff, so comment out for non-Linux.
    if ( $OSNAME ne 'linux' ) { return '' }
    return <<'END';
SHELL = bash
MANIFEST = $(shell cat MANIFEST)
SOURCE = $(filter bin/% examples/% %.pm %.PL %.pl %.t,$(MANIFEST))
SOURCE_TIDY = $(foreach file,$(SOURCE),$(file).tdy)

MANIFEST : $(SOURCE)
	git ls-files | egrep -v '^\.(git|be)' > $@

tardist : README.md

README.md : lib/Perl2Python.pm
	pod2markdown $< > $@

tidy : MANIFEST $(SOURCE_TIDY) README.md

%.tdy : %
	perltidy $* && if ! diff -q $@ $* > /dev/null; then cp $@ $*; fi
END
}
