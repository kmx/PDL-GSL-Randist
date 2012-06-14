#!perl -T

use Test::More tests => 1;

BEGIN {
    use_ok( 'PDL::GSL::Randist' ) || print "Bail out!\n";
}

diag( "Testing PDL::GSL::Randist $PDL::GSL::Randist::VERSION, Perl $], $^X" );
