use 5.006;
use strict;
use warnings;
use ExtUtils::MakeMaker;

WriteMakefile(
    NAME          => 'PDL::Probability',
    AUTHOR        => q{T. Nishimura <tnish@fastmail.jp>},
    VERSION_FROM  => 'lib/PDL/Probability.pm',
    ABSTRACT_FROM => 'lib/PDL/Probability.pm',
    dist          => { COMPRESS => 'gzip -9f', SUFFIX => 'gz', },
    DIR           => ['GSL'],
    ($ExtUtils::MakeMaker::VERSION >= 6.3002
      ? ('LICENSE'=> 'perl')
      : ()),
    PL_FILES            => {},
    PREREQ_PM => {
        'Test::More'     => 0,
        'File::ShareDir' => 0,
        'YAML'           => 0,
        'PDL'            => 0,
    },
    MIN_PERL_VERSION => '5.010000', 
    CONFIGURE_REQUIRES => {
        'File::ShareDir::Install' => '0',
    },
    #clean               => { FILES => 'ASDF-*' },
);
