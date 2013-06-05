use 5.01;
use strict;
use warnings;
use PDL::Core::Dev;
use ExtUtils::MakeMaker;
PDL::Core::Dev->import();

if (! gsl_ok()){
    die "can't find gsl-config or version too old? oldest supported version is 1.13";
}
if (! pdl_gsl_ok() ){
    die "gsl is installed, but can't load PDL::GSL::RNG, which means PDL apparently wasn't built with it. Try reinstalling PDL.";
}

WriteMakefile( 
    'NAME'      => 'PDL::GSL::Randist',
    'ABSTRACT'  => 'PDL binding to the GSL (GNU Scientific Libray) Random Distribution functions',
    'LIBS'      => [gsl_libs()],
    'INC'       => PDL_INCLUDE() . " " . gsl_cflags(),
    'TYPEMAPS'  => [PDL_TYPEMAP()],
    'OBJECT'    => 'Randist$(OBJ_EXT)',
    'NO_MYMETA' => 1,
    'PM' => { 'Randist.pm' => '$(INST_LIBDIR)/Randist.pm' },
    'MAN3PODS' => {
        'Randist.pm' => '$(INST_MAN3DIR)/PDL::GSL::Randist.$(MAN3EXT)'
    },
    'clean' => {
        'FILES' => 'Randist.xs Randist.pm Randist$(OBJ_EXT) Randist.c t/*.out t/*.o t/testvalues.txt t/mvtestvalues.txt t/testvalues-generator.*',
    },
    'VERSION_FROM' => 'gsl_randist.pp',
    'BUILD_REQUIRES' => { 
        'ExtUtils::MakeMaker' => 0,
        'PDL'                 => '2.006', # the version I dev'd with
        'YAML'                => '0.84',
        'autodie'             => 0,       # Not in core until 5.12
    },
    'PREREQ_PM' => {
        'PDL'        => '2.006',
    },
);

sub MY::postamble {
    pdlpp_postamble( ['gsl_randist.pp','Randist','PDL::GSL::Randist'] );
};

sub gsl_ok{
    use File::Spec;
    my $devnull = File::Spec->devnull; # Portable null device
    if (0 == system("gsl-config --version > $devnull 2> $devnull")){
        chomp( my $version = `gsl-config --version` );
        my ($major_version, $minor_version) = $version =~ /^(\d+)\.(\d+)$/;
        return $major_version == 1 && $minor_version >= 13;
    }
    return;
}
sub pdl_gsl_ok{
    eval { require PDL::GSL::RNG; };
    return !$@;
}
sub gsl_libs{
    my $libgsl = `gsl-config --libs`;
    chomp $libgsl;
    return $libgsl;
}
sub gsl_cflags{
    my $cflags = `gsl-config --cflags`;
    chomp $cflags;
    return $cflags;
}

#######################################################################
# testing is done by comparing the bound function outputs to their C output.
# in order to do this, test values are generated in tab-seperated values via C 
# according to the following pipeline, and compared in t/compare-to-c.t
#
# t/generate-testvalues-generator.pl -> t/testvalues-generator.c -> t/testvalues-generator.out -> t/testvalues.txt
# t/mvtestvalues-generator.c -> t/mvtestvalues-generator.out -> t/mvtestvalues.txt

package MY;
sub top_targets{
    use Cwd;
    my $cwd = Cwd::getcwd();
    $cwd =~ s/\//\\/g;
    $ENV{PATH} .= ";$cwd\\t" if $^O =~ /MSWin32/i;
    my $inherited = shift->SUPER::top_targets(@_);
    my $flags = `gsl-config --cflags`; chomp $flags;
    my $libs = `gsl-config --libs`; chomp $libs;
    my $_out = $^O =~ /MSWin32/i    ? 'testvalues-generator.out > t/testvalues.txt'
                                    : './t/testvalues-generator.out > t/testvalues.txt';
    my $_mvout = $^O =~ /MSWin32/i  ? 'mvtestvalues-generator.out > t/mvtestvalues.txt'
                                    : './t/mvtestvalues-generator.out > t/mvtestvalues.txt';

    # add gentest target to all
    $inherited =~ s/^(all.*)$/$1 gentest/xm;

    # add gentest target
    $inherited .= "
gentest : t/testvalues-generator.out t/mvtestvalues-generator.out t/testvalues.txt t/mvtestvalues.txt
t/testvalues-generator.out:
	perl t/generate-testvalues-generator.pl
	\$(CC) -c $flags t/testvalues-generator.c -o t/testvalues-generator.o
	\$(CC) t/testvalues-generator.o -o t/testvalues-generator.out $libs

t/mvtestvalues-generator.out:
	\$(CC) -c $flags t/mvtestvalues-generator.c -o t/mvtestvalues-generator.o
	\$(CC) t/mvtestvalues-generator.o -o t/mvtestvalues-generator.out $libs

# this won't work on windows... oh well
t/testvalues.txt:
	$_out
t/mvtestvalues.txt:
	$_mvout;
";
    return $inherited;
}
