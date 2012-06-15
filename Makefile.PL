use 5.006;
use strict;
use warnings;
use Data::Dumper;

use PDL::Core::Dev;
use ExtUtils::MakeMaker;
use File::ShareDir::Install;
PDL::Core::Dev->import();
require "pdlmaker.plm";

my $package = ['Randist.pp','Randist','PDL::GSL::Randist'];

# when not doing pdlmaker
# my %hash = pdlpp_stdargs($package);

my %hash = pdlmaker_init($package);
$hash{realclean}{FILES} .= " t/*.out t/testvalues.txt t/mvtestvalues.txt t/testvalues-generator.*";
$hash{VERSION_FROM} = 'Randist.pp';
$hash{PREREQ_PM}{PDL} = '2.4.2'; # not sure about this...
$hash{PREREQ_PM}{YAML} = '0'; 
$hash{PREREQ_PM}{'Test::Exception'} = '0'; 
$hash{PREREQ_PM}{'Test::More'} = '0'; 
$hash{PREREQ_PM}{'File::ShareDir'} = '0'; 
$hash{CONFIGURE_REQUIRES}{'File::ShareDir::Install'} = '0';
$hash{MIN_PERL_VERSION} = '5.010000'; # i'll remove this requirement if anyone cares

install_share 'share';

chomp( my $libgsl = `gsl-config --libs` );
chomp( my $cflags = `gsl-config --cflags` );
chomp( my $version = `gsl-config --version` );

my ($major_version, $minor_version) = $version =~ /^(\d+)\.(\d+)$/;

if ($libgsl && $major_version == 1 && $minor_version >= 13) { # 13 is the lowest I tested with...
    unshift @{ $hash{'LIBS'} }, $libgsl;
    $hash{'INC'} .= " $cflags";
    WriteMakefile( %hash );
    # print Dumper \%hash;
    sub MY::postamble {
        my $self = shift;
        my @ret = File::ShareDir::Install::postamble( $self );
        push @ret, pdlpp_postamble($package);
        return join "\n", @ret;
    };
}
else {
    warn "no GSL";
    exit;
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
    my $inherited = shift->SUPER::top_targets(@_);

    # add gentest target to all
    $inherited =~ s/^(all.*)$/$1 gentest/xm;

    # add gentest target
    $inherited .= q{
gentest :
	perl t/generate-testvalues-generator.pl
	$(CC) `gsl-config --libs --cflags` t/testvalues-generator.c -o t/testvalues-generator.out
	$(CC) `gsl-config --libs --cflags` t/mvtestvalues-generator.c -o t/mvtestvalues-generator.out
	# this won't work on windows... oh well
	./t/testvalues-generator.out > t/testvalues.txt
	./t/mvtestvalues-generator.out > t/mvtestvalues.txt
};
    return $inherited;
}
