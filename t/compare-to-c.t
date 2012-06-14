#!/usr/bin/env perl
use strict;
use warnings FATAL => "all";
use 5.010_000;
use Data::Dumper;
use autodie;
END {close STDOUT}
$| = 1;
use Test::More qw(no_plan);
use Test::Exception;
use Scalar::Util qw/looks_like_number/;

use PDL;
use PDL::GSL::RNG;
use PDL::GSL::Randist;

for my $file (qw{t/testvalues.txt t/mvtestvalues.txt}) {
#for my $file (qw{t/testvalues.txt}) {
    open my $fh, '<', $file;
    my $seed = <$fh>;
    chomp($seed);

    my $rng = PDL::GSL::RNG->new('taus');
    $rng->set_seed($seed);

    while (defined(my $line = <$fh>)){
        chomp $line;
        my ($funname, @others) = split /\t/, $line;
        # say $line;

        my $c_val = pdl $others[-1];
        my @args = map { looks_like_number $_ ? $_ : pdl $_ } @others[0 .. $#others - 1];

        # say join "\n", $c_val, @args;

        $funname =~ s/gsl_//;
        if (! exists $PDL::GSL::Randist::{$funname}){
            die "can't find $funname";
        }
        if ($funname !~ /pdf|P|Q|Pinv|Qinv$/){ # is a sampler
            # say "$line\n$c_val\n" . join "\t", @args;
            my $p_val = $PDL::GSL::Randist::{$funname}->($rng, @args);
            ok(
                all(abs($p_val - $c_val) < .000_000_1),
                "$funname ($c_val) ($p_val)"
            );
        }
        else{
            my $p_val = $PDL::GSL::Randist::{$funname}->(@args);
            if ($c_val =~ /nan/){ # regexp instead of simple eq b/c some funcs return -nan
                ok(! $p_val->isfinite(), "$funname ($p_val) ($c_val) *");
            }
            else {
                ok(
                    all(abs($p_val - $c_val) < .000_000_1),
                    "$funname ($c_val) ($p_val)"
                );
            }
        }
    }
    close $fh;
}

