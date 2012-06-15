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
    my $name = shift;
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
        push @ordered_args, $opt{$key};
    }
    return $val_or_dims, @ordered_args;
}

while (my ($name,$specs) = each %$config) {
    say $name;
    my $rbasename = $specs->{rname};
    my %expected_arguments;
    if (exists $specs->{args}){
        for my $arg (@{$specs->{args}}) {
            $expected_arguments{$arg->{name}} = 1;
        }
    }
    lock_hash(%expected_arguments);
    my @exported;

    if ($specs->{pdf}){
        my $rname_pdf = "d$rbasename";
        my $pname_pdf = "ran_${name}_pdf";
        no strict 'refs';
        *{$rname_pdf} = sub {
            my ($x, @args) = _argument_checker($name, \%expected_arguments, @_);
            return $PDL::GSL::Randist::{$pname_pdf}->($x, @args);
        };
        use strict 'refs';
        push @exported, $rname_pdf;
    }
    if ($specs->{cdf}){
        my $rname_cdf = "p$rbasename";
        my $pname_cdf = "cdf_${name}_P";
        no strict 'refs';
        *{$rname_cdf} = sub {
            my ($x, @args) = _argument_checker($name, \%expected_arguments, @_);
            return $PDL::GSL::Randist::{$pname_cdf}->($x, @args);
        };
        use strict 'refs';
        push @exported, $rname_cdf;
    }
    if ($specs->{cdfinv}){
        my $rname_cdfinv = "q$rbasename";
        my $pname_cdfinv = "cdf_${name}_Pinv";
        no strict 'refs';
        *{$rname_cdfinv} = sub {
            my ($P, @args) = _argument_checker($name, \%expected_arguments, @_);
            return $PDL::GSL::Randist::{$pname_cdfinv}->($P, @args);
        };
        use strict 'refs';
        push @exported, $rname_cdfinv;
    }
    if ($specs->{sample}){
        my $rname_sampler = "r$rbasename";
        my $pname_sampler = "ran_${name}";

        no strict 'refs';
        *{$rname_sampler} = sub {
            my ($v, @args) = _argument_checker($name, \%expected_arguments, @_);
            if (looks_like_number $v || ref $v eq 'PDL'){
                return $PDL::GSL::Randist::{$pname_sampler}->($rng, @args, $v);
            }
            elsif (ref $v eq 'ARRAY'){
                return $PDL::GSL::Randist::{$pname_sampler}->($rng, @args, @$v);
            }
            else{
                croak "first argument to $rname_sampler must be a count (integer), arrayref (for dimension of desired output pdl), or an output pdl";
            }
        };
        use strict 'refs';
        push @exported, $rname_sampler;
    }
    push @EXPORT_OK, @exported;
    $EXPORT_TAGS{$rbasename} = \@exported;
}

$EXPORT_TAGS{all} = \@EXPORT_OK;

1;

