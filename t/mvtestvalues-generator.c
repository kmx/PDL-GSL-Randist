#include <stdio.h>
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#include <gsl/gsl_cdf.h>

const int seed = 9955; /* seed chosen randomly, I swear */
gsl_rng * rng;

#define SIZE 4

void print_int_array(int array[], size_t count){
    int i;
    printf("[");
    for (i = 0; i < count; i++) {
        printf("%d", array[i]);
        if (i < count - 1){
            printf(",");
        }
    }
    printf("]");
}

void print_double_array(double array[], size_t count){
    int i;
    printf("[");
    for (i = 0; i < count; i++) {
        printf("%.12f", array[i]);
        if (i < count - 1){
            printf(",");
        }
    }
    printf("]");
}

void test_multinomial(void){
    double p[SIZE] = { .1, .2, .3, .4 };
    int n[SIZE] = { 0, 0, 0, 0 };
    int numdraws = 100;
    double prob;

    gsl_ran_multinomial(rng, SIZE, numdraws, p, n); 

    printf("gsl_ran_multinomial\t%d\t", numdraws);
    print_double_array(p, SIZE);
    printf("\t");
    print_int_array(n, SIZE);
    printf("\n");

    prob = gsl_ran_multinomial_pdf(SIZE, p, n); 
    printf("gsl_ran_multinomial_pdf\t");
    print_double_array(p, SIZE);
    printf("\t");
    print_int_array(n, SIZE);
    printf("\t%.12f\n", prob);

    prob = gsl_ran_multinomial_lnpdf(SIZE, p, n); 
    printf("gsl_ran_multinomial_lnpdf\t");
    print_double_array(p, SIZE);
    printf("\t");
    print_int_array(n, SIZE);
    printf("\t%.12f\n", prob);
}

void test_dirichlet(void){
    double alpha[SIZE] = { .4, .9, .4, .2 };
    double theta[SIZE] = { 0.0, 0.0, 0.0, 0.0 };
    double prob;
    int i;
    
    gsl_ran_dirichlet (rng, SIZE, alpha, theta);
    printf("gsl_ran_dirichlet\t");
    print_double_array(alpha, SIZE);
    printf("\t");
    print_double_array(theta, SIZE);
    printf("\n");

    theta[0] = 0.107873072217;
    theta[1] = 0.518033738502;
    theta[2] = 0.220000000209;
    theta[3] = 0.154093189072;
    
    prob = gsl_ran_dirichlet_pdf (SIZE, alpha, theta);
    printf("gsl_ran_dirichlet_pdf\t");
    print_double_array(alpha, SIZE);
    printf("\t");
    print_double_array(theta, SIZE);
    printf("\t%.12f\n", prob);

    prob = gsl_ran_dirichlet_lnpdf (SIZE, alpha, theta);
    printf("gsl_ran_dirichlet_lnpdf\t");
    print_double_array(alpha, SIZE);
    printf("\t");
    print_double_array(theta, SIZE);
    printf("\t%.12f\n", prob);
}

void test_bivariate_gaussian(void){
    double sigma_x = 3.5, sigma_y = 4.5, rho = .7, x, y, prob;
    gsl_ran_bivariate_gaussian(rng, sigma_x, sigma_y, rho, &x, &y);
    printf("gsl_ran_bivariate_gaussian\t[%.12f,%.12f]\t%.12f\t[%.12f,%.12f]\n",
            sigma_x, sigma_y, rho, x, y); 

    prob = gsl_ran_bivariate_gaussian_pdf(x, y, sigma_x, sigma_y, rho);
    printf("gsl_ran_bivariate_gaussian_pdf\t[%.12f,%.12f]\t[%.12f,%.12f]\t%.12f\t%.12f\n",
            x, y, sigma_x, sigma_y, rho, prob); 
}
void test_dir(void){
    double x,y,z, nd[SIZE] = { 0,0,0,0 };

    gsl_ran_dir_2d(rng, &x, &y);
    printf("gsl_ran_dir_2d\t[%.12f,%.12f]\n", x, y);

    gsl_ran_dir_2d_trig_method(rng, &x, &y);
    printf("gsl_ran_dir_2d_trig_method\t[%.12f,%.12f]\n", x, y);

    gsl_ran_dir_3d(rng, &x, &y, &z);
    printf("gsl_ran_dir_3d\t[%.12f,%.12f,%.12f]\n", x, y, z);

    /* perl needs to pass either outpdl or dims */
    gsl_ran_dir_nd(rng, SIZE, nd);
    printf("gsl_ran_dir_nd\t%d\t[%.12f,%.12f,%.12f,%.12f]\n", SIZE, nd[0], nd[1], nd[2], nd[3]);
}

void test_shuffle(void){
    int original[SIZE] = {1,2,3,4};
    int to_be_shuffled[SIZE] = {1,2,3,4};
    int draws[5];

    gsl_ran_sample(rng, draws, 5, original, SIZE, sizeof(int));
    printf("gsl_ran_sample\t[%d,%d,%d,%d]\t%d\t[%d,%d,%d,%d,%d]\n", 
            original[0], original[1], original[2], original[3],
            5,
            draws[0], draws[1], draws[2], draws[3], draws[4]
            );

    gsl_ran_choose(rng, draws, 3, original, SIZE, sizeof(int));
    printf("gsl_ran_choose\t[%d,%d,%d,%d]\t%d\t[%d,%d,%d]\n", 
            original[0], original[1], original[2], original[3],
            3,
            draws[0], draws[1], draws[2]
            );

    gsl_ran_shuffle(rng, to_be_shuffled, SIZE, sizeof(int));
    printf("gsl_ran_shuffle\t[%d,%d,%d,%d]\t[%d,%d,%d,%d]\n", 
            original[0], original[1], original[2], original[3],
            to_be_shuffled[0], to_be_shuffled[1], to_be_shuffled[2], to_be_shuffled[3]
            );
}

int main(void){
    rng = gsl_rng_alloc (gsl_rng_taus);
    gsl_rng_set(rng, seed); 

    printf("%d\n", seed);
    test_multinomial();
    test_dirichlet();
    test_bivariate_gaussian();
    test_dir();
    test_shuffle();

    return 0;
}

