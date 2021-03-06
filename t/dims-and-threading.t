# make sure the bound functions are threading correctly and returned the
# appropriately sized pdls. not really complete yet-- should do a better job on
# autogen'd functions

use strict;
use warnings;
use 5.010_000;
use autodie;
use Test::More qw(no_plan);

use PDL;
use PDL::GSL::RNG;
use PDL::GSL::Randist qw/:gaussian :binomial :multinomial :spherical :dirichlet :bivariate_gaussian/;

my $rng = PDL::GSL::RNG->new('taus');
$rng->set_seed(time);

{
    #######################################################################
    # gaussian and binomial-- i'll only check these for the autogen'd

    is_deeply( [],     [ran_gaussian($rng, 3.1)->dims()], "ran_gaussian scalar");
    is_deeply( [3],    [ran_gaussian($rng, 3.1, 3)->dims()], "ran_gaussian 1d");
    is_deeply( [3, 5], [ran_gaussian($rng, 3.1, 3, 5)->dims()], "ran_gaussian 2d");

    is_deeply( [],     [ran_binomial($rng, .1, 10)->dims()], "ran_binomial scalar");
    is_deeply( [3],    [ran_binomial($rng, .1, 10, 3)->dims()], "ran_binomial 1d");
    is_deeply( [3, 5], [ran_binomial($rng, .1, 10, 3, 5)->dims()], "ran_binomial 2d");

    #######################################################################
    # multinomial
    
    my $mmm = ran_multinomial($rng, 1000, [.25, .25, .25, .25], 10, 4);
    is_deeply([$mmm->dims()], [4, 10, 4], "multimultimultinomial (what?) dimensions",);
    is($mmm->sum(), 40000, "multimultimultinomial size");

    #######################################################################
    # dirichlet
    
    is_deeply( [3],         [ran_dirichlet($rng, [.2, .4, .1])->dims()], "ran_dirichlet single");
    is_deeply( [3, 23, 14], [ran_dirichlet($rng, [.2, .4, .1], 23, 14)->dims()], "ran_dirichlet multi");

    #######################################################################
    # bivariate_gaussian
    
    is_deeply( [2],     [ran_bivariate_gaussian($rng, [.2, .1], .9)->dims()], "ran_bivariate_gaussian single");
    is_deeply( [2, 34], [ran_bivariate_gaussian($rng, [.2, .1], .9, 34)->dims()], "ran_bivariate_gaussian multi");

    #######################################################################
    # sphericals
    
    for my $dir2d (qw/ran_dir_2d ran_dir_2d_trig_method/) {
        is_deeply( [2],        [$PDL::GSL::Randist::{$dir2d}($rng)->dims()], "$dir2d single");
        is_deeply( [2, 10],    [$PDL::GSL::Randist::{$dir2d}($rng, 10)->dims()], "$dir2d multi 1");
        is_deeply( [2, 10, 7], [$PDL::GSL::Randist::{$dir2d}($rng, 10, 7)->dims()], "$dir2d multi 2");
    }

    is_deeply( [3],        [ran_dir_3d($rng)->dims()], "ran_dir_3d single");
    is_deeply( [3, 10],    [ran_dir_3d($rng, 10)->dims()], "ran_dir_3d multi 1");
    is_deeply( [3, 10, 7], [ran_dir_3d($rng, 10, 7)->dims()], "ran_dir_3d multi 2");

    is_deeply( [7],        [ran_dir_nd($rng, 7)->dims()], "ran_dir_nd single");
    is_deeply( [7, 32, 3], [ran_dir_nd($rng, 7, 32, 3)->dims()], "ran_dir_nd multi");

}
