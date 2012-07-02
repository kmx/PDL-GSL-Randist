# make sure the annotation file gsl_randist.yml is not malformed or missing
# necessary fields.

use strict;
use warnings;
use 5.010_000;
use autodie;
use Test::More qw(no_plan);

use YAML qw/LoadFile/;
my $config = LoadFile("share/gsl_randist.yml");

DIST:
while (my ($name,$specs) = each %$config) {
    my $ok = 1;
    $ok = 0 if ($name !~ /^\w+$/);
    for my $key (qw/name rname type addlocation sample pdf cdf cdfinv args/) {
        if (! exists $specs->{$key}){
            ok(0, "$name yaml annotation");
            last DIST;
        }
    }
    for my $arg (@{$specs->{args}}) {
        for my $key (qw/name type testval/) {
            if (! exists $arg->{$key}){
                ok(0, "$name yaml annotation");
                last DIST;
            }
        }
    }
    ok(1, "$name yaml annotation");
}

