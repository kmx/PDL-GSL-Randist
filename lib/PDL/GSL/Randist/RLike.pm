package PDL::GSL::Randist::RLike;
use version; our $VERSION = qv('0.0.1');
use strict;
use warnings;
use 5.010_000;
use Data::Dumper;
use Carp;
use autodie;
use YAML qw/LoadFile/;
use PDL::GSL::Randist;
use PDL::GSL::RNG;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT_OK = qw();
our @EXPORT = qw();
our %EXPORT_TAGS = ();

use File::ShareDir qw/:ALL/;
use Hash::Util qw/lock_hash/;
use Scalar::Util qw/looks_like_number/;

my $file = dist_file('PDL-GSL-Randist', 'Randist.yml');
my $config = LoadFile($file);

our $rng = PDL::GSL::RNG->new('taus');
$rng->set_seed(time);

sub set_seed{
    $rng->set_seed(shift);
}

sub _argument_checker{
    my $expected_arguments = shift;
    my $val_or_dims = shift;
    if (@_ % 2 != 0){
        croak "odd number of arguments passed to RLike function";
    }
    my %opt = @_;

    my @ordered_args;
    for my $key (keys %opt) {
        if (! exists $expected_arguments->{$key}){
            croak "unknown argument $key";
        }
        $ordered_args[$expected_arguments->{$key}] = $opt{$key};
    }
    return $val_or_dims, @ordered_args;
}

while (my ($name,$specs) = each %$config) {
    my $rbasename = $specs->{rname};
    my %expected_arguments;
    if (exists $specs->{args}){
        my $i = 0;
        for my $arg (@{$specs->{args}}) {
            $expected_arguments{$arg->{name}} = $i++;
        }
    }
    lock_hash(%expected_arguments);
    my @exported;

    if ($specs->{pdf}){
        my $rname_pdf = "d$rbasename";
        my $pname_pdf = "ran_${name}_pdf";
        no strict 'refs';
        *{$rname_pdf} = _make_df($pname_pdf, \%expected_arguments);
        use strict 'refs';
        push @exported, $rname_pdf;
    }
    if ($specs->{cdf}){
        my $rname_cdf = "p$rbasename";
        my $pname_cdf = "cdf_${name}_P";
        no strict 'refs';
        *{$rname_cdf} = _make_df($pname_cdf, \%expected_arguments);
        use strict 'refs';
        push @exported, $rname_cdf;
    }
    if ($specs->{cdfinv}){
        my $rname_cdfinv = "q$rbasename";
        my $pname_cdfinv = "cdf_${name}_Pinv";
        no strict 'refs';
        *{$rname_cdfinv} = _make_df($pname_cdfinv, \%expected_arguments);
        use strict 'refs';
        push @exported, $rname_cdfinv;
    }
    if ($specs->{sample}){
        my $rname_sampler = "r$rbasename";
        my $pname_sampler = "ran_${name}";

        no strict 'refs';
        *{$rname_sampler} = _make_r_sampler($pname_sampler, $rname_sampler, \%expected_arguments);
        use strict 'refs';
        push @exported, $rname_sampler;
    }
    push @EXPORT_OK, @exported;
    $EXPORT_TAGS{$rbasename} = \@exported;
}

sub _make_df{
    my ($pname, $expected_arguments) = @_;
    return sub {
        my ($P, @args) = _argument_checker($expected_arguments, @_);
        return $PDL::GSL::Randist::{$pname}->($P, @args);
    };
}
sub _make_r_sampler{
    my ($pname, $rname, $expected_arguments) = @_;
    return sub{
        my ($v, @args) = _argument_checker($expected_arguments, @_);
        if (looks_like_number $v || ref $v eq 'PDL'){
            return $PDL::GSL::Randist::{$pname}->($rng, @args, $v);
        }
        elsif (ref $v eq 'ARRAY'){
            return $PDL::GSL::Randist::{$pname}->($rng, @args, @$v);
        }
        else{
            croak "first argument to $rname must be a count (integer), arrayref (for dimension of desired output pdl), or an output pdl";
        }
    }
}

*rmultinom   = _make_r_sampler('ran_multinomial',  'rmultinom',   { numdraws => 0, p => 1});
*dmultinom   = _make_df('ran_multinomial_pdf',   { p => 0, counts => 1 });
*dmultinomln = _make_df('ran_multinomial_lnpdf', { p => 0, counts => 1 });
$EXPORT_TAGS{multinom} = [qw/rmultinom  dmultinom  dmultinomln/];
push @EXPORT_OK, qw/rmultinom  dmultinom  dmultinomln/;

$EXPORT_TAGS{all} = \@EXPORT_OK;

1;

