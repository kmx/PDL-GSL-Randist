#!/usr/bin/env perl
use strict;
use warnings;
use 5.010_000;
use Data::Dumper;
use autodie;

use PDL;
use PDL::GSL::RNG;
use Package::Alias 'Rd' =>  'PDL::GSL::Randist';

my $rng = PDL::GSL::RNG->new('taus');
$rng->set_seed(time);

# Examples are shown with a discrete dist (binomial) and a continuos dist
# (gaussian). It should be simple to translate them the other distributions.

# However, the bivariate-gaussian, dirichlet, multinomial, spherical, and
# sampling distribution functions have slightly different interfaces so pay
# attention to their section in the POD.

### Samplers
# sampling functions are called ran_DISTNAME.  The first argument is always a
# PDL::GSL::RNG object.  The following arguments are parameters specific to
# that dist. (n, p for binomial, sigma for gaussian, mu for exponential, etc.
# See the PDL::GSL::Randist pod).  Last arguments are for output specification.
# You can either pass nothing, in which case a single sample is returned.  If
# you pass a PDL, it will be filled with samples in-place.  If you pass a like
# of integers, it will return a new PDL of those dimensions filled with
# samples.

# draw a sample 
print Rd::ran_binomial($rng, pdl(.5), long(100)); # n = 100, p = .5
print Rd::ran_gaussian($rng, pdl(3)); # sigma = 3.0

# draw 10 samples,put them in an outpdl
my $counts = zeroes long, 10;
my $values = zeroes long, 10;
Rd::ran_binomial($rng, pdl(.5), long(100), $counts);
Rd::ran_gaussian($rng, pdl(3), $values);

# draw 10 samples, return as 1-D pdl. 
print Rd::ran_binomial($rng, pdl(.5), long(100), 10);
print Rd::ran_gaussian($rng, pdl(3), 10); 

# draw 100 samples, return as 10x10 pdl
print Rd::ran_binomial($rng, pdl(.5), long(100), 10, 10);
print Rd::ran_gaussian($rng, pdl(3), 10, 10); 

# draw a single n=10 draw from a multinomial dist with p = [.1, .2, .3, .4]
print Rd::ran_multinomial($rng, 10, pdl(.1, .2, .3, .4));

# a 10 n=10 draw from a multinomial dist with p = [.1, .2, .3, .4], return as a
# 4 x 10 pdl.
# *WARNING* interface for drawing multiple multivariate samples may change draw,
# since right now you have to specify the first dimension, which is redundant
print Rd::ran_multinomial($rng, 10, pdl(.1, .2, .3, .4), 4, 10);

### PDF/CDF
# pdf's are named "ran_DISTNAME_pdf" and cdf are called "cdf_DISTNAME_P" and
# "cdf_DISTNAME_Q". (Note: P + Q = 1)

# evaluate pdf/cdf at various x's
my $x_continuous = zeroes(21)->xlinvals(-1, 1); # -1 to -1 by .1
my $x_discrete = long 1 .. 10;
print Rd::ran_gaussian_pdf($x_continuous, .5);
print Rd::cdf_gaussian_P($x_continuous, .5);
print Rd::ran_binomial_pdf($x_discrete, .5, 20);
print Rd::cdf_binomial_P($x_discrete, .5, 20);

# inverse cdf. 
my $P = zeroes(9)->xlinvals(.1, .9);
print Rd::cdf_gaussian_Pinv($P, .5);

# should give us back $x_continuous
print Rd::cdf_gaussian_Pinv(Rd::cdf_gaussian_P($x_continuous, .5), .5); 

