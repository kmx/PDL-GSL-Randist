#!/usr/bin/env perl
use strict;
use warnings;
use 5.010_000;
use Data::Dumper;
use autodie;
use Test::More qw(no_plan);
use Test::Exception;

use YAML qw/LoadFile/;
my $config = LoadFile("share/Randist.yml");

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

__END__
gumbel1: 
  name: Type-1 Gumbel Distribution
  rname: gumbel1
  type: Continuous
  sample: 1
  addlocation: 1
  pdf: 1
  cdf: 1
  cdfinv: 1
  args:
    - name: a
      type: double
      testval: 2.9
    - name: b
      type: double
      testval: 4.6
