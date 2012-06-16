#!perl -T

use Test::More tests => 2;

BEGIN {
    use_ok( 'PDL::Probability' ) || print "Bail out!\n";
    use_ok( 'PDL::Probability::GSL' ) || print "Bail out!\n";
}

diag( "Testing PDL::Probablity::GSL $PDL::Probability::GSL::VERSION, Perl $], $^X" );
