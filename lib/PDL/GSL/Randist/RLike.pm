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
    for my $arg (@$expected_arguments) {
        my $val = delete $opt{$arg};
        if (! defined $val){
            croak "need argument $arg";
        }
        push @ordered_args, $val;
    }
    return $val_or_dims, @ordered_args;
}

while (my ($name,$specs) = each %$config) {
    my $rbasename = $specs->{rname};
    my @expected_arguments = exists $specs->{args} ? map { $_->{name} } @{$specs->{args}} : ();
    my @exported;

    if ($specs->{pdf}){
        my $rname_pdf = "d$rbasename";
        my $pname_pdf = "ran_${name}_pdf";
        no strict 'refs';
        *{$rname_pdf} = _make_df($pname_pdf, \@expected_arguments);
        use strict 'refs';
        push @exported, $rname_pdf;
    }
    if ($specs->{cdf}){
        my $rname_cdf = "p$rbasename";
        my $pname_cdf = "cdf_${name}_P";
        no strict 'refs';
        *{$rname_cdf} = _make_df($pname_cdf, \@expected_arguments);
        use strict 'refs';
        push @exported, $rname_cdf;
    }
    if ($specs->{cdfinv}){
        my $rname_cdfinv = "q$rbasename";
        my $pname_cdfinv = "cdf_${name}_Pinv";
        no strict 'refs';
        *{$rname_cdfinv} = _make_df($pname_cdfinv, \@expected_arguments);
        use strict 'refs';
        push @exported, $rname_cdfinv;
    }
    if ($specs->{sample}){
        my $rname_sampler = "r$rbasename";
        my $pname_sampler = "ran_${name}";

        no strict 'refs';
        *{$rname_sampler} = _make_r_sampler($pname_sampler, $rname_sampler, \@expected_arguments);
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

*rmultinom   = _make_r_sampler('ran_multinomial',  'rmultinom',   [qw/numdraws p/]);
*dmultinom   = _make_df('ran_multinomial_pdf',   [qw/p counts/]);
*dmultinomln = _make_df('ran_multinomial_lnpdf', [qw/p counts/]);
$EXPORT_TAGS{multinom} = [qw/rmultinom  dmultinom  dmultinomln/];
push @EXPORT_OK, qw/rmultinom  dmultinom  dmultinomln/;

$EXPORT_TAGS{all} = \@EXPORT_OK;

1;

