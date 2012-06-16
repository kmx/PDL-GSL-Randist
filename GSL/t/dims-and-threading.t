#!/usr/bin/env perl
use strict;
use warnings;
use 5.010_000;
use Data::Dumper;
use autodie;
use Test::More qw(no_plan);
use Test::Exception;

use PDL;
use PDL::GSL::RNG;
use PDL::Probability::GSL;

my $rng = PDL::GSL::RNG->new('taus');
$rng->set_seed(1234);

{
    my $mmm = PDL::Probability::GSL::ran_multinomial($rng, 1000, [.25, .25, .25, .25], 10, 4);
    is_deeply([$mmm->dims()], [4, 10, 4], "multimultimultinomial (what?) dimensions",);
    is($mmm->sum(), 40000, "multimultimultinomial size");
}
