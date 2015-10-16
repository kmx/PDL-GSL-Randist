use 5.010;
use strict;
use warnings;
use PDL::Core::Dev;
use ExtUtils::MakeMaker;

if (! gsl_ok()){
    warn "can't find gsl-config or version too old? oldest supported version is 1.13";
    exit 0;
}
if (! pdl_gsl_ok() ){
    warn "gsl is installed, but can't load PDL::GSL::RNG, which means PDL apparently wasn't built with it. Try reinstalling PDL.";
    exit 0;
}

my $package = [qw/gsl_randist.pd Randist PDL::GSL::Randist/];
my %eumm_args = pdlpp_stdargs($package);
$eumm_args{INC} .= " " . gsl_cflags();
$eumm_args{LIBS}->[0] .= " " . gsl_libs();


WriteMakefile( 
    %eumm_args,
    'ABSTRACT'  => 'PDL binding to the GSL (GNU Scientific Libray) Random Distribution functions',
    'AUTHOR'    => 'Tom Nishimura',
    'LICENSE'   => 'perl',
    'VERSION_FROM' => 'gsl_randist.pd',
    'BUILD_REQUIRES' => { 
        'ExtUtils::MakeMaker' => 0,
        'PDL'                 => '2.006', # the version I dev'd with
        'YAML'                => '0.84',
        'autodie'             => 0,       # Not in core until 5.12
    },
    'PREREQ_PM' => {
        'PDL'        => '2.006',
    },
    'META_MERGE' => {
        resources    => {
            repository   => 'https://github.com/tnishimura/PDL-GSL-Randist',
        },
    },
   'clean' => {
        'FILES' => 't/*.out t/*.exe t/*.o t/*.obj',
    },
);

sub MY::postamble {
    pdlpp_postamble($package);
};

sub gsl_ok{
    use File::Spec;
    my $devnull = File::Spec->devnull; # Portable null device
    if (0 == system("gsl-config --version > $devnull 2> $devnull")){
        my $version = `gsl-config --version`;
        $version =~ s/[\r\n]+$//; # MSWin32 friendly
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
    $libgsl =~ s/[\r\n]+$//; # MSWin32 friendly
    return $libgsl;
}
sub gsl_cflags{
    my $cflags = `gsl-config --cflags`;
    $cflags =~ s/[\r\n]+$//; # MSWin32 friendly
    return $cflags;
}

#######################################################################
# testing is done by comparing the bound function outputs to their C output.
# in order to do this, test values are generated in tab-seperated values via C 
# according to the following pipeline, and compared in t/compare-to-c.t
#
# t/generate-testvalues-generator.pl -> t/testvalues-generator.c -> t/testvalues-generator.out -> t/testvalues.txt
# t/mvtestvalues-generator.c -> t/mvtestvalues-generator.out -> t/mvtestvalues.txt
#
# to (re)generate the t/*testvalues.txt files use: make gentest

package MY;

sub top_targets{
    my $inherited = shift->SUPER::top_targets(@_);
    my $flags = `gsl-config --cflags`; $flags =~ s/[\r\n]+$//;
    my $libs  = `gsl-config --libs`;   $libs  =~ s/[\r\n]+$//;

    my $exe_ext = $^O eq 'MSWin32' ? 'exe' : 'out';
    my $sep     = $^O eq 'MSWin32' ? '\\' : '/';

    # add gentest target
    $inherited .= "
gentest: t/testvalues-generator.$exe_ext t/mvtestvalues-generator.$exe_ext
	.${sep}t${sep}testvalues-generator.$exe_ext > t${sep}testvalues.txt
	.${sep}t${sep}mvtestvalues-generator.$exe_ext > t${sep}mvtestvalues.txt

t/testvalues-generator.$exe_ext:
	perl t/generate-testvalues-generator.pl
	\$(CC) -c $flags t/testvalues-generator.c -o t/testvalues-generator.o
	\$(CC) t/testvalues-generator.o -o t/testvalues-generator.$exe_ext $libs

t/mvtestvalues-generator.$exe_ext:
	\$(CC) -c $flags t/mvtestvalues-generator.c -o t/mvtestvalues-generator.o
	\$(CC) t/mvtestvalues-generator.o -o t/mvtestvalues-generator.$exe_ext $libs
";
    return $inherited;
}
