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
    #######################################################################
    # gaussian and binomial-- i'll only check these for the autogen'd

    is_deeply( [],     [PDL::Probability::GSL::ran_gaussian($rng, 3.1)->dims()], "ran_gaussian scalar");
    is_deeply( [3],    [PDL::Probability::GSL::ran_gaussian($rng, 3.1, 3)->dims()], "ran_gaussian 1d");
    is_deeply( [3, 5], [PDL::Probability::GSL::ran_gaussian($rng, 3.1, 3, 5)->dims()], "ran_gaussian 2d");

    is_deeply( [],     [PDL::Probability::GSL::ran_binomial($rng, .1, 10)->dims()], "ran_binomial scalar");
    is_deeply( [3],    [PDL::Probability::GSL::ran_binomial($rng, .1, 10, 3)->dims()], "ran_binomial 1d");
    is_deeply( [3, 5], [PDL::Probability::GSL::ran_binomial($rng, .1, 10, 3, 5)->dims()], "ran_binomial 2d");

    #######################################################################
    # multinomial
    
    my $mmm = PDL::Probability::GSL::ran_multinomial($rng, 1000, [.25, .25, .25, .25], 10, 4);
    is_deeply([$mmm->dims()], [4, 10, 4], "multimultimultinomial (what?) dimensions",);
    is($mmm->sum(), 40000, "multimultimultinomial size");

    #######################################################################
    # dirichlet
    
    is_deeply( [3],         [PDL::Probability::GSL::ran_dirichlet($rng, [.2, .4, .1])->dims()], "ran_dirichlet single");
    is_deeply( [3, 23, 14], [PDL::Probability::GSL::ran_dirichlet($rng, [.2, .4, .1], 23, 14)->dims()], "ran_dirichlet multi");

    #######################################################################
    # bivariate_gaussian
    
    is_deeply( [2], [PDL::Probability::GSL::ran_bivariate_gaussian($rng, [.2, .1], .9)->dims()], "ran_bivariate_gaussian single");
    is_deeply( [2, 34], [PDL::Probability::GSL::ran_bivariate_gaussian($rng, [.2, .1], .9, 34)->dims()], "ran_bivariate_gaussian multi");

    #######################################################################
    # sphericals
    
    for my $dir2d (qw/ran_dir_2d ran_dir_2d_trig_method/) {
        is_deeply( [2],        [$PDL::Probability::GSL::{$dir2d}($rng)->dims()], "$dir2d single");
        is_deeply( [2, 10],    [$PDL::Probability::GSL::{$dir2d}($rng, 10)->dims()], "$dir2d multi 1");
        is_deeply( [2, 10, 7], [$PDL::Probability::GSL::{$dir2d}($rng, 10, 7)->dims()], "$dir2d multi 2");
    }

    is_deeply( [3],        [PDL::Probability::GSL::ran_dir_3d($rng)->dims()], "ran_dir_3d single");
    is_deeply( [3, 10],    [PDL::Probability::GSL::ran_dir_3d($rng, 10)->dims()], "ran_dir_3d multi 1");
    is_deeply( [3, 10, 7], [PDL::Probability::GSL::ran_dir_3d($rng, 10, 7)->dims()], "ran_dir_3d multi 2");

    is_deeply( [7],        [PDL::Probability::GSL::ran_dir_nd($rng, 7)->dims()], "ran_dir_nd single");
    is_deeply( [7, 32, 3], [PDL::Probability::GSL::ran_dir_nd($rng, 7, 32, 3)->dims()], "ran_dir_nd multi");

}
