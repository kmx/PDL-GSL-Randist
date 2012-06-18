#!/usr/bin/env perl
use strict;
use warnings;
use 5.010_000;
use Data::Dumper;
use autodie;

use PDL;
use PDL::GSL::RNG;
#use PDL::Probability::GSL qw/:binomial :gaussian :multinomial/;
use PDL::Probability::GSL qw/:all/;

my $rng = PDL::GSL::RNG->new('taus');
$rng->set_seed(time);

### PDF/CDF
# pdf's are named "ran_DISTNAME_pdf" and cdf are called "cdf_DISTNAME_P" and
# "cdf_DISTNAME_Q". (Note: P + Q = 1)

# evaluate pdf/cdf at various x's
my $x_continuous = zeroes(21)->xlinvals(-1, 1); # -1 to -1 by .1
my $x_discrete = long 1 .. 10;
print ran_gaussian_pdf($x_continuous, .5);
print cdf_gaussian_P($x_continuous, .5);
print ran_binomial_pdf($x_discrete, .5, 20);
print cdf_binomial_P($x_discrete, .5, 20);

### Inverse CDF
my $P = zeroes(9)->xlinvals(.1, .9);
print cdf_gaussian_Pinv($P, .5);

# should give us back $x_continuous
print cdf_gaussian_Pinv(cdf_gaussian_P($x_continuous, .5), .5); 

### Samplers
# sampling functions are called ran_DISTNAME.  The first argument is always a
# PDL::GSL::RNG object.  The following arguments are parameters specific to
# that dist. (n, p for binomial, sigma for gaussian, mu for exponential, etc.
# See the PDL::GSL::Randist pod).  
#
# There are three ways to specify the output.
# 1) Pass nothing in the last parameter, in which case it'll return a scalar PDL.

print ran_binomial($rng, pdl(.5), long(100)); # n = 100, p = .5
print ran_gaussian($rng, pdl(3)); # sigma = 3.0

# 2) Pass an output-PDL to be filled with results:

my $counts = zeroes long, 10;
my $values = zeroes double, 10;
# draw 10 samples,put them in the outpdl
ran_binomial($rng, pdl(.5), long(100), $counts);
ran_gaussian($rng, pdl(3), $values);

# 3) Pass dimensions 

# draw 10 samples, return as 1-D pdl. 
print ran_binomial($rng, pdl(.5), long(100), 10); 
print ran_gaussian($rng, pdl(3), 10); 

# draw 100 samples, return as 10x10 pdl
print ran_binomial($rng, pdl(.5), long(100), 10, 10);
print ran_gaussian($rng, pdl(3), 10, 10); 

# Multivariate dists like Multinomial are slighly different because the first
# dimensions are implicit:

# sample a single n=10 draw from a multinomial dist with p = [.1, .2, # .3, .4]
# return as a dim(4) PDL.
print ran_multinomial($rng, 10, pdl(.1, .2, .3, .4));

# draw 15 multinomial with p=[.1,.2,.3,.4] and return as 4 x 15
print ran_multinomial($rng, 10, pdl(.1, .2, .3, .4), 15);

