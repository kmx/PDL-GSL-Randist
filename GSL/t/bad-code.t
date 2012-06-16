#!/usr/bin/env perl
use strict;
use warnings;
use 5.010_000;
use Data::Dumper;
use autodie;
use Test::More qw(no_plan);
use Test::Exception;
use PDL;
use PDL::Core qw/topdl/;
use PDL::Probability::GSL;

use YAML qw/LoadFile/;
my $config = LoadFile("share/gsl_randist.yml");

DIST:
while (my ($name,$specs) = each %$config) {
    my $ok = 1;
    my $testx = $specs->{type} eq 'Continuous' ? .1 : 1;
    my @args  = map { topdl $_ } ($testx), map { $_->{testval} } @{$specs->{args}};

    for my $i (0 .. $#args) {
        my @args_copy = map { $_->copy() } @args;
        $args_copy[$i]->setbadat(0);
        if ($specs->{pdf}){
            my $funname = "ran_${name}_pdf";
            my $bad = $PDL::Probability::GSL::{$funname}->(@args_copy);
            ok($bad->isbad(), "$funname bad-code test index $i");
        }
        if ($specs->{cdf}){
            my $funname_P = "cdf_${name}_P";
            my $funname_Q = "cdf_${name}_Q";
            my $bad_P = $PDL::Probability::GSL::{$funname_P}->(@args_copy);
            my $bad_Q = $PDL::Probability::GSL::{$funname_Q}->(@args_copy);
            ok($bad_P->isbad(), "$funname_P bad-code test index $i");
            ok($bad_Q->isbad(), "$funname_Q bad-code test index $i");
        }
        if ($specs->{cdfinv}){
            my $funname_Pinv = "cdf_${name}_Pinv";
            my $funname_Qinv = "cdf_${name}_Qinv";
            my $bad_Pinv = $PDL::Probability::GSL::{$funname_Pinv}->(@args_copy);
            my $bad_Qinv = $PDL::Probability::GSL::{$funname_Qinv}->(@args_copy);
            ok($bad_Pinv->isbad(), "$funname_Pinv bad-code test index $i");
            ok($bad_Qinv->isbad(), "$funname_Qinv bad-code test index $i");
        }
    }
}

my $prob;

for my $f (qw/ran_multinomial_pdf ran_multinomial_lnpdf ran_dirichlet_pdf ran_dirichlet_lnpdf/){
    ok( $PDL::Probability::GSL::{$f}->(pdl('.1, BAD, .3, .4'), pdl '2,3,4,5')->isbad(), "$f bad-code test index 0" );
    ok( $PDL::Probability::GSL::{$f}->(pdl('.1, .2, .3, .4'),  pdl '2,BAD,4,5')->isbad(), "$f bad-code test index 1" );
}

ok(PDL::Probability::GSL::ran_bivariate_gaussian_pdf(pdl('1, BAD'), pdl('.1, .1'), pdl('.8'))->isbad(), "ran_bivariate_gaussian_pdf bad-code test index 0");
ok(PDL::Probability::GSL::ran_bivariate_gaussian_pdf(pdl('1, 2'), pdl('.1, BAD'), pdl('.8'))->isbad(), "ran_bivariate_gaussian_pdf bad-code test index 1");
ok(PDL::Probability::GSL::ran_bivariate_gaussian_pdf(pdl('1, 2'), pdl('.1, .1'), pdl('BAD'))->isbad(), "ran_bivariate_gaussian_pdf bad-code test index 2");

