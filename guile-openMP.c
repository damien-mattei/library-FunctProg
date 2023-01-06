// openMP for Guile

#include <libguile.h>
#include <omp.h>
#include <stdio.h>


// Linux

// gcc `pkg-config --cflags guile-3.0` `pkg-config --libs guile-3.0` -L/usr/lib/llvm-14/lib/ -fopenmp -shared -o libguile-openMP.so -fPIC  -lguile-3.0 -lomp guile-openMP.c


// Mac OS

// gcc `pkg-config --cflags guile-3.0` -I/opt/homebrew/opt/libomp/include  -L/opt/homebrew/opt/libomp/lib -Xclang -fopenmp -shared -o libguile-openMP.so -fPIC  -lguile-3.0 -lomp guile-openMP.c

// or:

// clang `pkg-config --cflags guile-3.0` -I/opt/homebrew/opt/libomp/include  -L/opt/homebrew/opt/libomp/lib -Xclang -fopenmp -shared -o libguile-openMP.so -fPIC  -lguile-3.0 -lomp guile-openMP.c

// (define-module (openmp for) #:use-module (system foreign) #:use-module (system foreign-library) #:export (openmp))
// (define openmp (foreign-library-function "libguile-openMP" "openmp" #:return-type int #:arg-types (list int int '*)))




// OpenMP on macOS with Xcode tools:
// https://mac.r-project.org/openmp/

// export OMP_NUM_THREADS=8

// this main() in a library was only for testing openMP with Mac OS Xcode and Linux for use uncomment main() and comment openmp() functions

// clang `pkg-config --cflags guile-3.0` -I/opt/homebrew/opt/libomp/include  -L/opt/homebrew/opt/libomp/lib -Xclang -fopenmp -o libguile-openMP.so -fPIC  -lguile-3.0 -lomp guile-openMP.c

// gcc `pkg-config --cflags guile-3.0` `pkg-config --libs guile-3.0` -L/usr/lib/llvm-14/lib/ -fopenmp    -lguile-3.0 -lomp guile-openMP.c

/* int main() { */
  
/*   int start = 3; */
/*   int stop = 27; */
/*   int ncpus = omp_get_max_threads(); */
/*   printf("Found %i cores.\n",ncpus); */

/*   int i; */

/*   int iam,nthr; */
  
/* #pragma omp parallel for private(i,iam,nthr) */
  
  
/*   for (i=start; i<=stop; i++) { /\* i is private by default *\/ */
/*      iam = omp_get_thread_num(); */
/*      printf("iam=%i\n",iam); */
/*      nthr = omp_get_num_threads() ; */
/*      printf("total number of threads=%i\n",nthr); */
/*   } */

/* } */

int openmp(int start,int stop,char * func_name) { //SCM func_name) {

  //printf("guile-openMP.c : openmp\n");
  
  //char * c_string_func_name = scm_to_locale_string(func_name);

  //printf("guile-openMP.c : openmp : c_string_func_name = %s\n",c_string_func_name);

  //printf("guile-openMP.c : openmp : func_name = %s\n",func_name);
  
  SCM func;

  // here there is no need to init Guile as we are already in a C function called from the Scheme
  //scm_init_guile(); 
  //scm_c_primitive_load("script.scm");

  func = scm_variable_ref(scm_c_lookup(func_name));

  //double res2 = scm_to_double(scm_call_0(func));

  //int ncpus = omp_get_max_threads();
  //printf("guile-openMP.c : openmp : Found %i cores.\n",ncpus);

  int i;

  //omp_set_num_threads(ncpus);

  //int iam; // must be private in parallel if used
  //  private(i)
  
#pragma omp parallel for
  
  for (i=start; i<=stop; i++)  { /* i is private by default */

    scm_init_guile(); // needed here because we are in a new thread?
    scm_call_1( func , scm_from_int(i) );
    //scm_call_0( func );
    //iam = omp_get_thread_num();
    //printf("iam=%i\n",iam);

  }  
  
  return 1; //ncpus; 
  
}
