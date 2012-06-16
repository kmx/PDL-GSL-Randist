use 5.006;
use strict;
use warnings;
use Data::Dumper;

use PDL::Core::Dev;
use ExtUtils::MakeMaker;
use File::ShareDir::Install;
PDL::Core::Dev->import();

my $package = ['gsl_randist.pp','GSL','PDL::Probability::GSL'];

my %hash = pdlpp_stdargs($package);

$hash{clean}{FILES} .= " t/*.out t/testvalues.txt t/mvtestvalues.txt t/testvalues-generator.*";
# $hash{PM}{'lib/PDL/GSL/Randist/RLike.pm'} = '$(INST_LIBDIR)/Randist/RLike.pm';

install_share module => 'PDL::Probability::GSL' => 'share';

chomp( my $libgsl = `gsl-config --libs` );
chomp( my $cflags = `gsl-config --cflags` );
chomp( my $version = `gsl-config --version` );

my ($major_version, $minor_version) = $version =~ /^(\d+)\.(\d+)$/;

if ($libgsl && $major_version == 1 && $minor_version >= 13) { # 13 is the lowest I tested with...
    unshift @{ $hash{'LIBS'} }, $libgsl;
    $hash{'INC'} .= " $cflags";
    WriteMakefile( %hash );
    print Dumper \%hash;
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
