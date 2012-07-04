package PDL::Probability::RLike;
use strict;
use warnings;
use 5.010_000;
use Data::Dumper;
use Carp;
use autodie;
use YAML qw/LoadFile/;
use PDL;
use PDL::Probability::GSL;
use PDL::GSL::RNG;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw();
our @EXPORT = qw();
our %EXPORT_TAGS = ();

use File::ShareDir qw/:ALL/;
use Scalar::Util qw/looks_like_number/;

my $file = module_file('PDL::Probability::GSL', 'gsl_randist.yml');
my $config = LoadFile($file);

our $rng = PDL::GSL::RNG->new('taus');
$rng->set_seed(time);

sub set_rng{ 
    my $new_rng = shift;
    if (ref $new_rng ne 'PDL::GSL::RNG'){
        croak "first argument to set_rng must be PDL::GSL::RNG object";
    }
    $rng = $new_rng;
}

#######################################################################
# utility

sub _argument_checker{
    my $expected_arguments = shift;
    my $location_name = shift;
    my $val_or_dims = shift;

    croak "odd number of arguments passed to RLike function" if (@_ % 2 != 0);

    my %opt = @_;

    my @ordered_args;
    for my $arg (@$expected_arguments) {
        my $val = delete $opt{$arg->{name}};
        if (! defined $val){
            croak "need argument $arg";
        }
        if (! looks_like_number($val) and ref $val ne 'PDL'){
            if ($arg->{type} eq 'double'){
                $val = double $val;
            }
            elsif ($arg->{type} eq 'unsigned int'){
                $val = long $val;
            }
        }
        push @ordered_args, $val;
    }
    
    my $location = 0.0;
    $location = delete $opt{$location_name} if defined $location_name;
    if (! looks_like_number $location){
        $location = double $location;
    }

    if (keys %opt > 0){
        croak "unknown keys " . join ",", keys %opt;
    }
    return $val_or_dims, $location, @ordered_args;
}

sub _make_df{
    my ($pname, $type, $expected_arguments, $location_name) = @_;
    return sub {
        my ($x, $location, @args) = _argument_checker($expected_arguments, $location_name, @_);
        if (! looks_like_number($x) and ref $x ne 'PDL'){
            $x = $type eq 'Continuous' ? double($x) : long($x);
        }
        return $PDL::Probability::GSL::{$pname}->($x - $location, @args);
    };
}
sub _make_dfinv{
    my ($pname, $expected_arguments) = @_;
    return sub {
        my ($P, undef, @args) = _argument_checker($expected_arguments, undef, @_);
        return $PDL::Probability::GSL::{$pname}->($P, @args);
    };
}
sub _make_r_sampler{
    my ($pname, $rname, $expected_arguments, $location_name) = @_;
    return sub{
        my ($v, $location, @args) = _argument_checker($expected_arguments, $location_name, @_);
        if (looks_like_number $v || ref $v eq 'PDL'){
            return $location + $PDL::Probability::GSL::{$pname}->($rng, @args, $v);
        }
        elsif (ref $v eq 'ARRAY'){
            say "bar";
            return $location + $PDL::Probability::GSL::{$pname}->($rng, @args, @$v);
        }
        else{
            croak "first argument to $rname must be a count (integer), arrayref (for dimension of desired output pdl), or an output pdl";
        }
    }
}

sub _add_tag{
    my ($basename, @funcs) = @_;
    $EXPORT_TAGS{$basename} = [@funcs];
    push @EXPORT_OK, @funcs;
}

#######################################################################

while (my ($name,$specs) = each %$config) {
    my $rbasename = $specs->{rname};
    my $location_name = $specs->{addlocation} eq '1' ? 'location' : $specs->{addlocation};
    my @expected_arguments = defined $specs->{args} ? @{$specs->{args}} : ();
    my @exported;

    if ($specs->{pdf}){
        my $rname_pdf = "d$rbasename";
        my $pname_pdf = "ran_${name}_pdf";
        no strict 'refs';
        *{$rname_pdf} = _make_df($pname_pdf, $specs->{type}, \@expected_arguments, $location_name);
        use strict 'refs';
        push @exported, $rname_pdf;
    }
    if ($specs->{cdf}){
        my $rname_cdf = "p$rbasename";
        my $pname_cdf = "cdf_${name}_P";
        no strict 'refs';
        *{$rname_cdf} = _make_df($pname_cdf, $specs->{type}, \@expected_arguments, $location_name);
        use strict 'refs';
        push @exported, $rname_cdf;
    }
    if ($specs->{cdfinv}){
        my $rname_cdfinv = "q$rbasename";
        my $pname_cdfinv = "cdf_${name}_Pinv";
        no strict 'refs';
        *{$rname_cdfinv} = _make_dfinv($pname_cdfinv, \@expected_arguments);
        use strict 'refs';
        push @exported, $rname_cdfinv;
    }
    if ($specs->{sample}){
        my $rname_sampler = "r$rbasename";
        my $pname_sampler = "ran_${name}";

        no strict 'refs';
        *{$rname_sampler} = _make_r_sampler($pname_sampler, $rname_sampler, \@expected_arguments, $location_name);
        use strict 'refs';
        push @exported, $rname_sampler;
    }
    push @EXPORT_OK, @exported;
    $EXPORT_TAGS{$rbasename} = \@exported;
}


*rmultinom   = _make_r_sampler('ran_multinomial',  'rmultinom',   [qw/n p/]);
*dmultinom   = _make_df('ran_multinomial_pdf',   [qw/p n/]);
*dmultinomln = _make_df('ran_multinomial_lnpdf', [qw/p n/]);

_add_tag('multinom', qw/rmultinom dmultinom dmultinomln/);

*rdirichlet   = _make_r_sampler('ran_dirichlet', [qw/alpha/]);
*ddirichlet   = _make_df('ran_dirichlet_pdf', [qw/alpha/]);
*ddirichletln = _make_df('ran_dirichlet_lnpdf', [qw/alpha/]);

_add_tag('dirichlet', qw/rdirichlet ddirichlet ddirichlet/);

*rsample = _make_r_sampler('ran_sample', [qw/src count/]);
*rchoose = _make_r_sampler('ran_choose', [qw/src count/]);
*rshuffle = _make_r_sampler('ran_shuffle', [qw/src/]);

_add_tag('sample', qw/rsample rchoose/);
_add_tag('shuffle', qw/rshuffle/);

*rbinorm   = _make_r_sampler('ran_binorm', [qw/sigma rho/]);
*dbinorm   = _make_df('ran_binorm_pdf', [qw/sigma rho/]);

_add_tag('binorm', qw/rbinorm rbinorm/);

$EXPORT_TAGS{all} = \@EXPORT_OK;

1;

=head1 NAME

INCOMPLETE!!

PDL::Probability::RLike - R-Like interface to probability distribution functions.

=head1 SYNOPSIS

=head1 EXPORT 

Nothing is exported by default.  Export tags for each set of functions is given in 
parenthesis below.

=head1 FUNCTIONS

=head2 set_rng()

=cut
