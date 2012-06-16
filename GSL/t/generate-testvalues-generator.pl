#!/usr/bin/env perl
use strict;
use warnings FATAL => "all";
use 5.010_000;
use Data::Dumper;
use autodie;
END {close STDOUT}
$| = 1;

use YAML qw/LoadFile/;
my $config = LoadFile("share/gsl_randist.yml");
open my $output_fh, '>', 't/testvalues-generator.c';

sub genc{
    my ($funname, $x, @testvals) = @_;
    my $str = join q{\t}, $funname, $x, @testvals, '%.12f';
    my $c_args = join ", ", $x, @testvals;

    print $output_fh qq{    printf("$str\\n", $funname($c_args)); \n};
}

sub genc_sampler_continuous{
    my ($funname, @testvals) = @_;
    my $str = join q{\t}, $funname, @testvals, '%.12f';
    my $c_args = join ", ", 'rng', @testvals;

    print $output_fh qq{    printf("$str\\n", $funname($c_args)); \n};
}

sub genc_sampler_discrete{
    my ($funname, @testvals) = @_;
    my $str = join q{\t}, $funname, @testvals, '%d';
    my $c_args = join ", ", 'rng', @testvals;

    print $output_fh qq{    printf("$str\\n", $funname($c_args)); \n};
}

print $output_fh q[
#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>

const int seed = 9955; /* seed chosen randomly, I swear */

int main(void){
    gsl_rng * rng = gsl_rng_alloc (gsl_rng_taus);
    gsl_rng_set(rng, seed); 
    printf("%d\n", seed);

];

# generate tests for dists annotated in yml
while (my ($name,$spec) = each %$config) {
    my @args = defined $spec->{args} ? @{$spec->{args}} : ();
    my @testvals = map { $_->{testval} } @args;
    my @discrete_x_vals = (0 .. 10);

    if ($spec->{type} eq 'Continuous'){
        if ($spec->{pdf}){
            for (my $x = -5.0 ; $x <= 5.0 ; $x += .4){
                genc("gsl_ran_${name}_pdf", $x, @testvals);
            }
        }
        if ($spec->{cdf}){
            for (my $x = -5.0 ; $x <= 5.0 ; $x += .4){
                genc("gsl_cdf_${name}_P", $x, @testvals);
                genc("gsl_cdf_${name}_Q", $x, @testvals);
            }
        }
        if ($spec->{cdfinv}){
            for (my $P = 0.1 ; $P <= 0.9 ; $P += .1){
                my $c_args = join ", ", $P, @testvals;
                genc("gsl_cdf_${name}_Pinv", $P, @testvals);
                genc("gsl_cdf_${name}_Qinv", $P, @testvals);
            }
        }
        if ($spec->{sample}){
            for (1 .. 10){
                genc_sampler_continuous("gsl_ran_${name}", @testvals);
            }
        }
    }
    elsif ($spec->{type} eq 'Discrete'){
        if ($spec->{pdf}){
            for my $x (0 .. 10) {
                genc("gsl_ran_${name}_pdf", $x, @testvals);
            }
        }
        if ($spec->{cdf}){
            for my $x (0 .. 10) {
                genc("gsl_cdf_${name}_P", $x, @testvals);
                genc("gsl_cdf_${name}_Q", $x, @testvals);
            }
        }
        if ($spec->{sample}){
            for (1 .. 10){
                genc_sampler_discrete("gsl_ran_${name}", @testvals);
            }
        }
    }
}

# now generate tests for manually bound, currently only gaussian and gamma knuth

for my $sampler (qw/gsl_ran_gaussian_ziggurat gsl_ran_gaussian_ratio_method/) {
    for (1 .. 10){
        print $output_fh qq{\tprintf("$sampler\\t7.76\\t%.12f\\n", $sampler(rng, 7.76)); \n}
    }
}
for (1 .. 10){
    print $output_fh qq{\tprintf("gsl_ran_gamma_knuth\\t3.1\\t2.4\\t%.12f\\n", gsl_ran_gamma_knuth(rng,3.1, 2.4)); \n}
}

# for my $g (qw/gsl_ran_gaussian_pdf gsl_cdf_gaussian_P gsl_cdf_gaussian_Q/){
#     for (my $x = -5.0 ; $x <= 5.0 ; $x += .4){
#         print $output_fh qq{\tprintf("gsl_ran_gaussian_pdf\\t$x\\t0.0\\t7.76\\t%.12f\\n", gsl_ran_gaussian_pdf($x, 7.76));  \n};
#     }
# }
# for my $cdfinv (qw/gsl_cdf_gaussian_Pinv gsl_cdf_gaussian_Qinv/){
#     for (my $P = 0.1 ; $P <= 0.9 ; $P += .1){
#         print $output_fh qq{\tprintf("$cdfinv\\t$P\\t0.0\\t7.76\\t%.12f\\n", $cdfinv($P, 7.76));\n}; 
#     }
# }

print $output_fh q[
    return 0;
}

];

close $output_fh;
