use strict;
use warnings FATAL => "all";
use 5.010_000;
use Data::Dumper;
use autodie;
use Carp;

# The goal is to wrap as many of the samplers, pdf, cdf, and inverse-cdf 
# functions possible automatically.
# 
# Each distribution has a set of associate C functions for sampling
# (gsl_ran_NAME), probability density/mass evaluation (gsl_ran_NAME_pdf),
# cumulative distribution functions (gsl_cdf_NAME_P and gsl_cdf_NAME_Q) and
# inverse CDF- functions (gsl_cdf_NAME_Pinv and gsl_cdf_NAME_Qinv). A particular
# distribution may not have all of these.  (Note: the 'P' cdf functions + the 'Q'
# cdf functions = 1.)
# 
# For the Pareto distribution (which will be our example dist.), the function
# prototypes look like:
# 
#  double gsl_ran_pareto (const gsl_rng * r, double a, double b);
#  double gsl_ran_pareto_pdf (double x, double a, double b);
#  double gsl_cdf_pareto_P (double x, double a, double b);
#  double gsl_cdf_pareto_Q (double x, double a, double b);
#  double gsl_cdf_pareto_Pinv (double P, double a, double b);
#  double gsl_cdf_pareto_Qinv (double Q, double a, double b);
# 
# The strategy is to generate the appropriate pp_def's from the annotation in
# gsl_randist.yml (which has its own format documentation), which contains an
# entry for each distribution to be handled automatically. Each looks like:
# 
# Samplers are slightly more complicated b/c pp_def forces the gsl_rng * object to
# come last in the parameter list.  Therefore, we create a "meat" binding first:
# 
#  pp_def('ran_pareto_meat',
#      Pars => 'double a(); double b(); double [o] out()',
#      OtherPars => 'IV rng',
#      Code => q{
#          $out() = gsl_ran_pareto( INT2PTR(gsl_rng *, $COMP(rng)), $a(), $b());
#      }
#  );
# 
# and then wrap the meat to change the argument order to match the C version.
# This also allows various ways of specifying the output PDL dimensions (see user
# documentation for examples).  Meat-wrappers (which sounds a bit kinky) are
# generated via the gen_ran_meat_wrapper() by passing the sampler name and
# argument count:
# 
#  gen_ran_meat_wrapper('ran_pareto', 2);

pp_bless('PDL::Probability::GSL');
pp_addhdr('
#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>

');
pp_addpm(q{
use Carp;
use Scalar::Util qw/looks_like_number/;
});

#######################################################################
# gen_ran_meat_wrapper and gen_pp

my %outtype       = ( Continuous => 'double', Discrete => 'int',);
my %generic_types = ( Continuous => ['D'],    Discrete => ['L'],);

# don't create a pod entry at all. PMFunc => '' b/c we're blessing into this
# module, so redundant
sub pp_defnd{ pp_def(@_, PMFunc => '', Doc => undef); }

# create a pod entry with only the signature.
sub pp_defsig{ 
    pp_def(@_, PMFunc => '', Doc => '', BadDoc => ''); 
}

sub section_header{ pp_addpm(qq{\n=head1 $_[0]\n\n=cut\n\n}); }

# pod generator for samplers manually bound functions.
sub gen_sampler_pod{
    my ($funname, $type, $args) = @_;
    my $sig = join(" ; ", map({ "$_->{type} $_->{name}()" } @$args) , "$outtype{$type} [o] out()" );

    my $example_args = join(", ", map({ "\$$_->{name}" } @$args));

    pp_addpm(qq{

=head2 $funname

=for sig

  Signature: (PDL::GSL::RNG rng(); $sig)

Usage: 

  my \$single_draw = $funname(\$rng, $example_args);
  my \$multi_draws = $funname(\$rng, $example_args, [ ... output pdl dims ...] );
  my \$outpdl_draw = $funname(\$rng, $example_args, \$outpdl);

=cut

})
}



=head2 ran_gaussian

=for sig

  Signature: (PDL::GSL::RNG rng(); double sigma() ; double [o] out())

=cut


sub c_pdf_name      { 'gsl_' . perl_pdf_name(@_)}
sub c_sampler_name  { 'gsl_' . perl_sampler_name(@_) }
sub c_cdf_P_name    { 'gsl_' . perl_cdf_P_name(@_) }
sub c_cdf_Q_name    { 'gsl_' . perl_cdf_Q_name(@_) }
sub c_cdf_Pinv_name { 'gsl_' . perl_cdf_Pinv_name(@_) }
sub c_cdf_Qinv_name { 'gsl_' . perl_cdf_Qinv_name(@_) }
sub perl_pdf_name      { my ($name) = @_; "ran_${name}_pdf"; }
sub perl_sampler_name  { my ($name) = @_; "ran_${name}"; }
sub perl_cdf_P_name    { my ($name) = @_; "cdf_${name}_P"; }
sub perl_cdf_Q_name    { my ($name) = @_; "cdf_${name}_Q"; }
sub perl_cdf_Pinv_name { my ($name) = @_; "cdf_${name}_Pinv"; }
sub perl_cdf_Qinv_name { my ($name) = @_; "cdf_${name}_Qinv"; }

pp_addpm(q{

sub check_rng{
    croak "first argument must be a PDL::GSL::RNG" if ref($_[0]) ne 'PDL::GSL::RNG';
}

sub make_ran_meat_wrapper{
    my ($ran_meat_ref, $type, $numargs) = @_;
    my $outpdl_type = $type eq 'Continuous' ? 'double' : 'long';

    return sub {
        my ($rng, @opt) = @_;

        check_rng($rng);

        my @args = map { PDL::Core::topdl($_) } @opt[0 .. $numargs - 1];
        my @var_or_dims = @opt[$numargs .. $#opt];

        if (ref $var_or_dims[0] eq 'PDL'){
            $ran_meat_ref->(@args, $var_or_dims[0], $$rng);
            return $var_or_dims[0];
        }
        elsif (@var_or_dims > 0){
            my $tmp_output = $type eq 'Continuous' ? zeroes(@var_or_dims) : zeroes(long, @var_or_dims);
            $ran_meat_ref->(@args, $tmp_output, $$rng);
            return $tmp_output;
        }
        else {
            #my $tmp_output = pdl(0);
            my $tmp_output = null;
            $ran_meat_ref->(@args, $tmp_output, $$rng);
            return $tmp_output;
        }
    }
}

# for bivgauss and 2d/3d directional samplers. no type (assume double). base_dim == implicit 
# size of dim-0 (2 for bivgauss/2d, 3 for 3d)
sub make_mv_ran_meat_wrapper{
    my ($ran_meat_ref, $base_dims, $numargs) = @_;

    return sub {
        my ($rng, @opt) = @_;
        check_rng($rng);

        my @args = map { PDL::Core::topdl($_) } @opt[0 .. $numargs - 1];
        my @outpdl_or_dims = @opt[$numargs .. $#opt];

        if (ref $outpdl_or_dims[0] eq 'PDL'){
            $ran_meat_ref->(@args, $outpdl_or_dims[0], $$rng);
            return $outpdl_or_dims[0];
        }
        else {
            my $tmp_output = zeroes(double, $base_dims, @outpdl_or_dims);
            $ran_meat_ref->(@args, $tmp_output, $$rng);
            return $tmp_output;
        }
    }
}
});

sub code_and_badcode{
    my ($code, $outvar, @argnames) = @_;
    # if no argnames (like for landau), never badcode
    my $is_bad_condition = @argnames > 0 ? join("||", map "\$ISBAD($_())", @argnames) : 0;
    return (
        Code => $code,
        HandleBad => 1,
        BadCode => qq{ 
            if ($is_bad_condition){ 
                \$SETBAD($outvar()); 
            } else{ 
                $code 
            } 
        }
    )
}


sub gen_pp{
    my ($basename, $specs) = @_;
    my @args = defined $specs->{args} ? @{$specs->{args}} : ();
    @args = map { $_->{type} =~ s/unsigned\s//; $_ } @args; #converted unsigned int to int
    my @argnames = map { $_->{name} } @args;

    my $type = $specs->{type};

    if ($type ne 'Continuous' && $type ne 'Discrete'){
        croak "type must be Continuous or Discrete";
    }

    section_header($specs->{name});

    # pdf and cdf share the 
    { 
        # construct Pars for cdf/pdf.  
        # 'OUTTYPE val(); TYPE a(); TYPE b(); double [o] out()'
        my $df_pars = join(" ; ", 
            $outtype{$type} . " val()" , 
            map({ "$_->{type} $_->{name}()"} @args) ,
            "double [o] out()"
        );

        # '$val(), $a(), $b()'
        my $df_c_fun_args = join(", ", '$val()', map{'$' . $_->{name} . '()'} @args);

        if ($specs->{pdf}){
            # pp_def('ran_pareto_pdf',
            #     Pars => 'double val() ; double a() ; double b() ; double [o] out()',
            #     GenericTypes => [D],
            #     Code => ' $out() = gsl_ran_pareto_pdf($val(), $a(), $b()); ',
            #     HandleBad => '1',
            #     BadCode => ' if ($ISBAD(val())||$ISBAD(a())||$ISBAD(b())){ $SETBAD(out()); } else{  $out() = gsl_ran_pareto_pdf($val(), $a(), $b());  } ',
            # );
            my $perl_funname = perl_pdf_name($basename);
            my $c_funname = c_pdf_name($basename);

            my $code = qq{ \$out() = $c_funname($df_c_fun_args); };
            # my $badcode = qq{ if ($is_bad_condition){ \$SETBAD(out()); } else{ $code } };

            pp_defsig($perl_funname,
                Pars => $df_pars,
                GenericTypes => $generic_types{$type},
                code_and_badcode($code, 'out', 'val', @argnames)
            );
        }
        if ($specs->{cdf}){
            # pp_def('cdf_pareto_P',
            #     Pars => 'double val() ; double a() ; double b() ; double [o] out()',
            #     GenericTypes => [D]
            #     Code => '$out() = gsl_cdf_pareto_P($val(), $a(), $b());',
            #     HandleBad => '1',
            #     BadCode => ' if ($ISBAD(val())||$ISBAD(a())||$ISBAD(b())){ $SETBAD(out()); } else{ $out() = gsl_cdf_pareto_P($val(), $a(), $b()); } ',
            # );

            my $perl_funname_P = perl_cdf_P_name($basename);
            my $perl_funname_Q = perl_cdf_Q_name($basename);
            my $c_funname_P = c_cdf_P_name($basename);
            my $c_funname_Q = c_cdf_Q_name($basename);

            my $code_P = "\$out() = $c_funname_P($df_c_fun_args);";
            my $code_Q = "\$out() = $c_funname_Q($df_c_fun_args);";

            pp_defsig($perl_funname_P,
                Pars => $df_pars,
                GenericTypes => ['D'],
                code_and_badcode($code_P, 'out', 'val', @argnames)
            );
            pp_defsig($perl_funname_Q,
                Pars => $df_pars,
                GenericTypes => ['D'],
                code_and_badcode($code_Q, 'out', 'val', @argnames)
            );
        }
    }

    if ($specs->{cdfinv}){
        # pp_def('cdf_pareto_Pinv',
        #     Pars => 'double val() ; double a() ; double b() ; double [o] out()',
        #     Code => '$out() = gsl_cdf_pareto_Pinv($val(), $a(), $b());',
        #     HandleBad => '1',
        #     BadCode => ' if ($ISBAD(val())||$ISBAD(a())||$ISBAD(b())){ $SETBAD(out()); } else{ $out() = gsl_cdf_pareto_Pinv($val(), $a(), $b()); } ',
        # );

        my $perl_funname_Pinv = perl_cdf_Pinv_name($basename);
        my $perl_funname_Qinv = perl_cdf_Qinv_name($basename);
        my $c_funname_Pinv = c_cdf_Pinv_name($basename);
        my $c_funname_Qinv = c_cdf_Qinv_name($basename);

        my $pars_cdfinv = join(" ; ", "double val()" , map({ "$_->{type} $_->{name}()" } @args) , "double [o] out()");
        my $c_fun_args = join(", ", '$val()', map{'$' . $_->{name} . '()'} @args);

        # no type issues here since discrete dists don't have invcdf's
        my $code_Pinv = "\$out() = $c_funname_Pinv($c_fun_args);";
        my $code_Qinv = "\$out() = $c_funname_Qinv($c_fun_args);";
        # my $badcode_Pinv = qq{ if ($is_bad_condition){ \$SETBAD(out()); } else{ $code_Pinv } };
        # my $badcode_Qinv = qq{ if ($is_bad_condition){ \$SETBAD(out()); } else{ $code_Qinv } };

        pp_defsig($perl_funname_Pinv,
            Pars => $pars_cdfinv,
            code_and_badcode($code_Pinv, 'out', 'val', @argnames),
        );
        pp_defsig($perl_funname_Qinv,
            Pars => $pars_cdfinv,
            code_and_badcode($code_Qinv, 'out', 'val', @argnames), 
        );
    }

    if ($specs->{sample}){
        # pp_def('ran_pareto_meat',
        #     Pars => 'double a() ; double b() ; double [o] out()',
        #     Code => '$out() = gsl_ran_pareto(INT2PTR(gsl_rng *, $COMP(rng)), $a(), $b());',
        #     OtherPars => 'IV rng',
        # );

        my $perl_funname = perl_sampler_name($basename);
        my $c_funname = c_sampler_name($basename);

        my $perl_meat_funname = "${perl_funname}_meat";

        # sampling functions don't share the $pars and $c_fun_args
        # 'a(); b(); OUTTYPE [o] out()'
        my $ran_pars = join(" ; ", map({ "$_->{type} $_->{name}()" } @args) , "$outtype{$type} [o] out()" );

        # 'INT2PTR(gsl_rng *, $COMP(rng)), $a(), $b()'
        my $ran_c_fun_args = join(", ", 'INT2PTR(gsl_rng *, $COMP(rng))', map{'$' . $_->{name} . '()'}  @args);

        my $code = '$out() = ' . "$c_funname($ran_c_fun_args);";

        pp_defnd($perl_meat_funname,
            Pars => $ran_pars,
            OtherPars => 'IV rng',
            Code => $code,
        );
        my $numargs = scalar @args;
        pp_addpm(qq{

            *$perl_funname = make_ran_meat_wrapper(\\\&PDL::Probability::GSL::$perl_meat_funname, '$type', $numargs);

        });
        # pp_add_exported($perl_funname);
        gen_sampler_pod($perl_funname, $type, \@args);
    }
}

#######################################################################
# read in annotation and generate

use YAML qw/LoadFile/;
my $annotation = LoadFile("share/gsl_randist.yml");

for my $basename (sort keys %$annotation) {
    my $specs = $annotation->{$basename};
    # say STDERR "generating subroutines for $basename";
    gen_pp($basename, $specs);
}

#######################################################################
# alternate gaussian samplers

section_header('Alternate Gaussian Samplers');

{
    my $sig = 'double sigma(); double [o] out()';

    pp_defnd('ran_gaussian_ratio_method_meat',
        Pars => $sig,
        OtherPars => 'IV rng',
        Code => q{ $out() = gsl_ran_gaussian_ratio_method( INT2PTR(gsl_rng *, $COMP(rng)), $sigma()); },
    );

    pp_defnd('ran_gaussian_ziggurat_meat',
        Pars => $sig,
        OtherPars => 'IV rng',
        Code => q{ $out() = gsl_ran_gaussian_ziggurat( INT2PTR(gsl_rng *, $COMP(rng)), $sigma()); },
    );

    pp_addpm(qq{ *ran_gaussian_ziggurat = make_ran_meat_wrapper(\\\&PDL::Probability::GSL::ran_gaussian_ziggurat_meat, 'Continuous', 1); });
    pp_addpm(qq{ *ran_gaussian_ratio_method = make_ran_meat_wrapper(\\\&PDL::Probability::GSL::ran_gaussian_ratio_method_meat, 'Continuous', 1); });

    gen_sampler_pod('ran_gaussian_ratio_method', 'Continuous', [{name => 'sigma', type => 'double'}]);
    gen_sampler_pod('ran_gaussian_ziggurat', 'Continuous', [{name => 'sigma', type => 'double'}]);
}

#######################################################################
# alternate gamma sampler

section_header('Alternate Gamma Samplers');

{
    my $sig = 'double a() ; double b() ; double [o] out()';
    pp_defnd('ran_gamma_knuth_meat',
        Pars => $sig,
        OtherPars => 'IV rng',
        Code => '$out() = gsl_ran_gamma_knuth(INT2PTR(gsl_rng *, $COMP(rng)), $a(), $b());',
    );
    pp_addpm(q{ *ran_gamma_knuth = make_ran_meat_wrapper(\&PDL::Probability::GSL::ran_gamma_knuth_meat, 'Continuous', 2); });
    gen_sampler_pod('ran_gamma_knuth', 'Continuous', [{name => 'a', type => 'double'}, {name => 'b', type => 'double'}]); 
}

#######################################################################
# multinomial

section_header('Multinomial Distribution');

{
    my $sig = 'int numdraws(); double p(n); int [o] counts(n)';
    pp_defnd('ran_multinomial_meat',
        Pars => $sig,
        OtherPars => 'IV rng',
        Code => q{
            gsl_ran_multinomial(
                INT2PTR(gsl_rng *, $COMP(rng)),
                $SIZE(n),
                $numdraws(),
                $P(p),
                (unsigned int *) $P(counts)
            );
    });
    
    pp_addpm(q{
        sub ran_multinomial{
            my ($rng, $numdraws, $p, @outpdl_or_dims) = @_;
    
            check_rng($rng);
            $p = PDL::Core::topdl $p;
    
            if (ref $outpdl_or_dims[0] eq 'PDL'){
                ran_multinomial_meat($numdraws, $p, $outpdl_or_dims[0], $$rng);
                return $outpdl_or_dims[0];
            }
            else {
                # first dims is implicit
                my $tmp_output = zeroes(long, $p->dims(), @outpdl_or_dims);
                ran_multinomial_meat($numdraws, $p, $tmp_output, $$rng);
                return $tmp_output;
            }
        }
    });
    
    pp_addpm(qq{
    
=head2 ran_multinomial

Draws \$numdraws samples from a multinomial distribution with probabilities \$p,
returns counts in a pdl with the same (first) dimensions as \$p.

=for sig

  Signature: (PDL::GSL::RNG rng(); $sig)

Usage:

  ran_multinomial(\$rng, \$numdraws, \$p, \$outpdl); # \$outpdl's first dimensions must be (\$p->dims())

  my \$single_draw = ran_multinomial(\$rng, \$numdraws, \$p); 
  my \$multi_draws = ran_multinomial(\$rng, \$numdraws, \$p, \@dims); # returns pdl with dimensions (\$p->dims(), \@dims)

=cut

    });
}

for (qw/ran_multinomial_pdf ran_multinomial_lnpdf/) {
    pp_def($_,
        Pars => 'double p(n); int counts(n); double [o] probability()',
        Code => qq{
            \$probability() = gsl_$_(
            \$SIZE(n),
            \$P(p),
            (unsigned int *) \$P(counts)
            );
        },
        HandleBad => 1,
        BadCode => q{ $SETBAD(probability()); },
        PMFunc => '', 
        Doc => 'Note the slightly strange order of arguments.', 
        BadDoc => ''
    );
}

#######################################################################
# dirichlet

section_header('Dirichlet Distribution');

{
    my $sig = 'double alpha(n); double [o] theta(n)';
    pp_defnd('ran_dirichlet_meat',
        Pars => $sig,
        OtherPars => 'IV rng',
        Code => q{
            gsl_ran_dirichlet(
                INT2PTR(gsl_rng *, $COMP(rng)),
                $SIZE(n),
                $P(alpha),
                $P(theta)
            );
    });
        
    pp_addpm(q{
        sub ran_dirichlet{
            my ($rng, $alpha, @outpdl_or_dims) = @_;
    
            check_rng($rng);
    
            $alpha = PDL::Core::topdl $alpha;
    
            if (ref $outpdl_or_dims[0] eq 'PDL'){
                ran_dirichlet_meat($alpha, $outpdl_or_dims[0], $$rng);
                return $outpdl_or_dims[0];
            }
            else {
                # first dims is implicit
                my $tmp_output = zeroes(double, $alpha->dims(), @outpdl_or_dims);
                ran_dirichlet_meat($alpha, $tmp_output, $$rng);
                return $tmp_output;
            }
        }
    });

    pp_addpm(qq{
    
=head2 ran_dirichlet

=for sig

  Signature: (PDL::GSL::RNG rng(); $sig)

Usage:

  ran_dirichlet(\$rng, \$alpha, \$outpdl); # \$outpdl's first dimensions must be (\$alpha->dims())

  my \$single_draw = ran_dirichlet(\$rng, \$alpha); 
  my \$multi_draws = ran_dirichlet(\$rng, \$alpha, \@dims); # returns pdl with dimensions (\$alpha->dims(), \@dims)

=cut

    });
}


for (qw/ran_dirichlet_pdf ran_dirichlet_lnpdf/) {
    my $code = qq{ \$probability() = gsl_$_( \$SIZE(n), \$P(alpha), \$P(theta)); };
    pp_defnd($_,
        Pars => 'double alpha(n); double theta(n); double [o] probability()',
        Code => $code,
        HandleBad => 1,
        BadCode => qq{
            int i, bad;
            for (i = 0; i < \$SIZE(n); ++i) {
                if (\$ISBAD(alpha(n=>i)) || \$ISBAD(theta(n=>i))){
                    bad = 1;
                    break;
                }
            }
            if (bad){
                \$SETBAD(probability());
            }
            else{
                $code
            }
        },
    );
}

#######################################################################
# bivariate_gaussian

section_header('Bivariate Gaussian Distribution');

{
    my $sig = "double sigma(n=2); double rho(); double [o] out(n=2)";
    pp_defnd('ran_bivariate_gaussian_meat',
        Pars => $sig,
        OtherPars => 'IV rng',
        Code => q{
            double x, y;
            gsl_ran_bivariate_gaussian( INT2PTR(gsl_rng *, $COMP(rng)), $sigma(n=>0), $sigma(n=>1), $rho(), &x, &y);
            $out(n=>0) = x;
            $out(n=>1) = y;
        },
    );
        
    pp_addpm(qq{ *ran_bivariate_gaussian = make_mv_ran_meat_wrapper(\\\&PDL::Probability::GSL::ran_bivariate_gaussian_meat, 2, 2); });

    pp_addpm(qq{
=head2 ran_bivariate_gaussian

=for sig

  Signature: (PDL::GSL::RNG rng(); $sig)

Usage:

  ran_bivariate_gaussian(\$rng, pdl(\$sigma_x, \$sigma_y), \$rho, \$dim); # returns pdl dim(2, \$dim);
  
  my \$xy     = ran_bivariate_gaussian(\$rng, pdl(\$sigma_x, \$sigma_y), \$rho); # returns pdl dim(2)
  my \$xy_vec = ran_bivariate_gaussian(\$rng, pdl(\$sigma_x, \$sigma_y), \$rho, \$dim); # returns pdl dim(2, \$dim);

=cut 
    });
}

{
    my $code = q{ $prob() = gsl_ran_bivariate_gaussian_pdf( $xy(n => 0), $xy(n => 1), $sigma(n => 0), $sigma(n => 1), $rho()); };
    pp_defnd('ran_bivariate_gaussian_pdf',
        Pars => 'double xy(n=2); double sigma(n=2); double rho(); double [o] prob()',
        Code => $code,
        HandleBad => 1,
        BadCode => qq{
            if ( \$ISBAD(xy(n=>0)) || \$ISBAD(xy(n=>1)) || \$ISBAD(sigma(n=>0)) || \$ISBAD(sigma(n=>1)) || \$ISBAD(rho()) ){
                \$SETBAD(prob());
            }
            else{
                $code;
            }
        }
    );
}

#######################################################################
# Spherical Vector Distributions

section_header('Spherical Vector Distribution');
for my $twod (qw/ran_dir_2d ran_dir_2d_trig_method/) {
    my $sig = 'double [o] vector(n=2)';
    pp_defnd($twod . "_meat",
        Pars => $sig,
        OtherPars => 'IV rng',
        Code => qq{
            double x, y;
            gsl_$twod( INT2PTR(gsl_rng *, \$COMP(rng)), &x, &y);
            \$vector(n => 0) = x;
            \$vector(n => 1) = y;
        },
    );
    pp_addpm(qq{
=head2 $twod

=for sig

  Signature: (PDL::GSL::RNG rng(); $sig)

=cut 
    });
}
    
{
    my $sig = 'double [o] vector(n=3)';
    pp_defnd('ran_dir_3d_meat',
        Pars => $sig,
        OtherPars => 'IV rng',
        Code => q{
            double x, y, z;
            gsl_ran_dir_3d(INT2PTR(gsl_rng *, $COMP(rng)), &x, &y, &z);
            $vector(n => 0) = x;
            $vector(n => 1) = y;
            $vector(n => 2) = z;
        },
    );
    pp_addpm(qq{
=head2 ran_dir_3d_meat

=for sig

  Signature: (PDL::GSL::RNG rng(); $sig)

=cut 
    });
}

pp_addpm(qq{ *ran_dir_2d = make_mv_ran_meat_wrapper(\\\&PDL::Probability::GSL::ran_dir_2d_meat, 2, 0); });
pp_addpm(qq{ *ran_dir_2d_trig_method = make_mv_ran_meat_wrapper(\\\&PDL::Probability::GSL::ran_dir_2d_trig_method_meat, 2, 0); });
pp_addpm(qq{ *ran_dir_3d = make_mv_ran_meat_wrapper(\\\&PDL::Probability::GSL::ran_dir_3d_meat, 3, 0); });

pp_defnd('ran_dir_nd_meat',
    Pars => 'int size(); double [o] vector(n)', 
    OtherPars => 'IV rng',
    Code => q{
        gsl_ran_dir_nd(INT2PTR(gsl_rng *, $COMP(rng)), $size(), $P(vector));
    },
);

# note this is a little different-- always require at least one dimension
pp_addpm(q{
sub ran_dir_nd{
    my ($rng, $dim, @outpdl_or_additional_dims) = @_;
    
    check_rng($rng);

    if (ref $outpdl eq 'PDL'){
        ran_dir_nd_meat($dim, $outpdl, $$rng);
        return $outpdl;
    }
    else{
        my $tmpout = zeroes($dim, @outpdl_or_additional_dims);
        ran_dir_nd_meat($dim, $tmpout, $$rng);
        return $tmpout;
    }
}
=head2 ran_dir_nd

=for sig

  Signature: (PDL::GSL::RNG rng(); int size(); double [o] vector(n))

=cut 

});

#######################################################################
# Shuffling and Sampling

section_header('Shuffling and Sampling');
# choose/sample are ordered draws from src into dest.
#     void gsl_ran_shuffle (const gsl_rng * r, void * base, size_t n, size_t size)
#     int gsl_ran_choose (const gsl_rng * r, void * dest, size_t k, void * src, size_t n, size_t size)
#     void gsl_ran_sample (const gsl_rng * r, void * dest, size_t k, void * src, size_t n, size_t size)

for my $ss (qw/choose sample/) {
    
    pp_defnd("ran_${ss}_meat",
        Pars => 'dest(k); src(n)',
        OtherPars => 'IV rng',
        Code => qq{
            gsl_ran_$ss(
                INT2PTR(gsl_rng *, \$COMP(rng)), 
                \$P(dest), 
                \$SIZE(k), 
                \$P(src), 
                \$SIZE(n), 
                sizeof(\$GENERIC())
            );
        },
    );
}

# sample-with-replacement
pp_addpm(q{
    sub ran_sample{
        my ($rng, $src, $count, @outpdl_or_count) = @_;
        croak "ran_sample only supports 1-dimensional sources (for now)" if ($src->ndims() > 1);
        if (ref $outpdl_or_count[0] eq 'PDL'){
            croak "you passed an outpiddle of size " . $outpdl_or_count[0]->dim(0) . " which is less than requested count $count";
            ran_sample_meat($outpdl_or_count[0], $src, $$rng);
            return $outpdl_or_count[0];
        }
        else{
            my $out = zeroes($count, @outpdl_or_count);
            ran_sample_meat($out, $src, $$rng);
            return $out;
        }
    }
});

# sample-without-replacement
pp_addpm(q{
    sub ran_choose{
        my ($rng, $src, $count, @outpdl_or_count) = @_;
        croak "ran_choose only supports 1-dimensional sources (for now)" if ($src->ndims() > 1);
        croak "ran_choose is sample-without-replacement, so count needs to be greater than src size" if $src->dim(0) < $count;
        if (ref $outpdl_or_count[0] eq 'PDL'){
            croak "you passed an outpiddle of size " . $outpdl_or_count[0]->dim(0) . " which is less than requested count $count";
            ran_choose_meat($outpdl_or_count[0], $src, $$rng);
            return $outpdl_or_count[0];
        }
        else{
            my $out = zeroes($count, @outpdl_or_count);
            ran_choose_meat($out, $src, $$rng);
            return $out;
        }
    }
});

pp_defnd('ran_shuffle_meat',
    Pars => 'foo(n)', 
    OtherPars => 'IV rng',
    Code => q{
        gsl_ran_shuffle( INT2PTR(gsl_rng *, $COMP(rng)), $P(foo), $SIZE(n), sizeof($GENERIC()));
    },
);

pp_addpm(q{
    sub ran_shuffle{
        my ($rng, $src) = @_;
        croak "ran_shuffle only supports 1-dimensional sources (for now)" if ($src->ndims() > 1);
        $dst = $src->copy();
        ran_shuffle_meat($dst, $$rng);
        return $dst;
    }
});


pp_addpm(q{

=head2 ran_choose

Sample count items from src without replacement, put them (in order) into
out(). ran_choose is not threadable (src must be one dimensional). count must
be <= n.

=for sig

  Signature: (PDL::GSL::RNG rng(); src(n); count(); out())

=head2 ran_sample

Sample count items from src with replacement, put them (in order) into
out(). ran_sample is not threadable (src must be one dimensional). 

=for sig

  Signature: (PDL::GSL::RNG rng(); src(n); count(); out())

=head2 ran_shuffle

Return a new piddle with shuffled elements from src. src must be
one-dimensional.

=for sig

  Signature: (PDL::GSL::RNG rng(); src(n))

=cut

});

#######################################################################
# documentation


pp_addpm({At => 'Top'}, <<'PROLOGUE');
=head1 NAME

PDL::Probability::GSL - Comprehensive Perl Data Language (PDL) binding to the GNU
Scientific Library (GSL) Random Distribution (randist) functions.

=head1 VERSION

Version 0.01

=cut


=head1 SYNOPSIS

    use PDL;
    use PDL::GSL::RNG;
    use PDL::Probability::GSL;

    # I suggest you alias the module name:
    # use Package::Alias 'Rd' =>  'PDL::Probability::GSL';
    
    my $rng = PDL::GSL::RNG->new('taus');
    $rng->set_seed(time);
    
    # Examples are shown with a discrete dist (binomial) and a continuos dist
    # (gaussian). It should be simple to translate them the other distributions.
    
    # However, the bivariate-gaussian, dirichlet, multinomial, spherical, and
    # sampling distribution functions have slightly different interfaces so pay
    # attention to their section in the POD.
    
    ### Samplers
    # sampling functions are called ran_DISTNAME.  The first argument is always a
    # PDL::GSL::RNG object.  The following arguments are parameters specific to
    # that dist. (n, p for binomial, sigma for gaussian, mu for exponential, etc.
    # See the PDL::Probability::GSL pod).  Last arguments are for output specification.
    # You can either pass nothing, in which case a single sample is returned.  If
    # you pass a PDL, it will be filled with samples in-place.  If you pass a like
    # of integers, it will return a new PDL of those dimensions filled with
    # samples.
    
    # draw a sample 
    print PDL::Probability::GSL::ran_binomial($rng, pdl(.5), long(100)); # n = 100, p = .5
    print PDL::Probability::GSL::ran_gaussian($rng, pdl(3)); # sigma = 3.0
    
    # draw 10 samples,put them in an outpdl
    my $counts = zeroes long, 10;
    my $values = zeroes long, 10;
    PDL::Probability::GSL::ran_binomial($rng, pdl(.5), long(100), $counts);
    PDL::Probability::GSL::ran_gaussian($rng, pdl(3), $values);
    
    # draw 10 samples, return as 1-D pdl. 
    print PDL::Probability::GSL::ran_binomial($rng, pdl(.5), long(100), 10);
    print PDL::Probability::GSL::ran_gaussian($rng, pdl(3), 10); 
    
    # draw 100 samples, return as 10x10 pdl
    print PDL::Probability::GSL::ran_binomial($rng, pdl(.5), long(100), 10, 10);
    print PDL::Probability::GSL::ran_gaussian($rng, pdl(3), 10, 10); 
    
    # draw a single n=10 draw from a multinomial dist with p = [.1, .2, .3, .4]
    print PDL::Probability::GSL::ran_multinomial($rng, 10, pdl(.1, .2, .3, .4));
    
    # a 10 n=10 draw from a multinomial dist with p = [.1, .2, .3, .4], return as a
    # 4 x 10 pdl.
    # *WARNING* interface for drawing multiple multivariate samples may change draw,
    # since right now you have to specify the first dimension, which is redundant
    print PDL::Probability::GSL::ran_multinomial($rng, 10, pdl(.1, .2, .3, .4), 4, 10);
    
    ### PDF/CDF
    # pdf's are named "ran_DISTNAME_pdf" and cdf are called "cdf_DISTNAME_P" and
    # "cdf_DISTNAME_Q". (Note: P + Q = 1)
    
    # evaluate pdf/cdf at various x's
    my $x_continuous = zeroes(21)->xlinvals(-1, 1); # -1 to -1 by .1
    my $x_discrete = long 1 .. 10;
    print PDL::Probability::GSL::ran_gaussian_pdf($x_continuous, .5);
    print PDL::Probability::GSL::cdf_gaussian_P($x_continuous, .5);
    print PDL::Probability::GSL::ran_binomial_pdf($x_discrete, .5, 20);
    print PDL::Probability::GSL::cdf_binomial_P($x_discrete, .5, 20);
    
    # inverse cdf. 
    my $P = zeroes(9)->xlinvals(.1, .9);
    print PDL::Probability::GSL::cdf_gaussian_Pinv($P, .5);
    
    # should give us back $x_continuous
    print PDL::Probability::GSL::cdf_gaussian_Pinv(PDL::Probability::GSL::cdf_gaussian_P($x_continuous, .5), .5); 

=head1 EXPORT

Nothing is exported by default.  I suggest using Package::Alias or aliased
(both available on CPAN) so you can call functions like "Rd::ran_gaussian()"
instead of PDL::RNG::Randist::ran_gaussian()).

=head1 NOTES

=head2 Function Naming Convention

Samplers are named "ran_RANDIST()" (eg. "ran_gaussian()"), 
probability density/mass functions are named "ran_RANDIST_pdf()" (eg. "ran_gaussian_pdf()"), 
cumulative distribution functions are named "cdf_RANDIST_P()" and "cdf_RANDIST_Q()" (eg. "cdf_gaussian_P()" and "cdf_gaussian_Q()"), 
inverse cumulative distribution functions (quantiles) are named "cdf_RANDIST_Pinv()" and "cdf_RANDIST_Qinv()" (eg. "cdf_gaussian_Pinv()" and "cdf_gaussian_Qinv()"). 

Note that the relationship between the "P" cdf and thr "Q" cdf is cdf_RANDIST_P + cdf_RANDIST_Q = 1.

A particular distribution may not have all 4 types of functions.  (Discrete
distributions don't have inverse CDF's, Levy distributions don't have pdf's,
etc.). 

=head2 Arguments

All of the univariate distributions functions have arguments identical (even in
order) to their underlying C functions.  For the multivariate functions, see
their individual sections below.

=head2 Threading

Unless otherwise noted, PDL threading works on all variables expect the
PDL::GSL::RNG argument for samplers.

=head2 More Documentation

Most of the documentation is automatically generated.  For more details, see the 
L<The GSL Randist manual page|http://www.gnu.org/software/gsl/manual/html_node/Random-Number-Distributions.html>.

=head2 Location Parameters

None of the functions provided by this module contain a 'location' parameter.
For example, the gaussian functions only have a sigma argument, and not a mean.
I thought about adding the location parameters to the appropriate randists, but
this would be contrary to the goal of module to create a straight binding to
GSL's Randist.  Instead, I plan to make an alternative interface to the
functions (possibly modelled after R) which includes such conviniences.

=head2 Bad Values

All the non-sampler functions should handle PDL's BAD values appropriately.

=cut

PROLOGUE

pp_addpm({At => 'Bot'}, <<EPILOGUE);

=head1 AUTHOR

T. Nishimura, C<< <tnish at fastmail.jp> >>

=head1 BUGS

The discrete distribution has no binding yet.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc PDL::Probability::GSL

=head1 ACKNOWLEDGEMENTS

Thanks to the PDL team and the GSL team!  

=head1 LICENSE AND COPYRIGHT

Copyright 2012 T. Nishimura.

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut

EPILOGUE

pp_export_nothing();
pp_done();
