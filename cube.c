// openMP cube test

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>


// OpenMP on macOS with Xcode tools:
// https://mac.r-project.org/openmp/

// export OMP_NUM_THREADS=8

// this main() in a library was only for testing openMP with Mac OS Xcode and Linux for use uncomment main() and comment openmp() functions


// mac os :
// clang  -I/opt/homebrew/opt/libomp/include  -L/opt/homebrew/opt/libomp/lib -Xclang -fopenmp -o cube  -lomp cube.c

// gcc -L/usr/lib/llvm-14/lib/ -fopenmp  -o cube  -lomp cube.c


unsigned long long *vtst;



unsigned long long fct(unsigned long long x) {
  return x * x * x;
}


unsigned long long fctapply(unsigned long long i) {
  return vtst[i] = fct(vtst[i]);
  }


int main() {
  int vtstlen = 2642245;
  vtst = calloc(vtstlen, sizeof(unsigned long long));
  
  int ncpus = omp_get_max_threads();
  printf("Found a maximum of %i cores.\n",ncpus);
  printf("Program compute cube of numbers with and without parallelisation with OpenMP library.\n\n");
  printf("Initialising data.\n\n");
  //int iam,nthr;

  // init data sequential
  for (int i=0; i<vtstlen; i++) { /* i is private by default because it is the for indice*/
    //iam = omp_get_thread_num();
    //printf("iam=%i\n",iam);
    //nthr = omp_get_num_threads() ;
    //printf("total number of threads=%i\n",nthr);
    vtst[i]=i;
    
  }


  printf("STARTING computation without //.\n");
  

  for (int i=0; i<vtstlen; i++) {
    
    fctapply(i);
    
  }

  printf("ENDING computation without //.\n\n");

  // display a few results
  for (int i=0;i < 10; i++) {
    printf("%llu\n",vtst[i]);
  }
  printf( ".....\n");
  for (int i=vtstlen - 10; i < vtstlen; i++) {
    printf("%llu\n",vtst[i]);
  }
 
  
  printf("Initialising data in //.\n\n");
  //int iam,nthr;

#pragma omp parallel for private(vtstlen) shared(vtst)
  
  
  for (int i=0; i<vtstlen; i++) { /* i is private by default because it is the for indice*/
  
    vtst[i]=0;
    
  }

  printf("STARTING computation in //.\n");

  
  // setting private disable unecessary // overload work on some variables (mutex...)
#pragma omp parallel for private(vtstlen) shared(vtst)
  
  
  for (int i=0; i<vtstlen; i++) { /* i is private by default */
    
    fctapply(i);
    
  }
  
  printf("ENDING computation in //.\n\n");
  

  // display a few results
  for (int i=0;i < 10; i++) {
    printf("%llu\n",vtst[i]);
  }
  printf( ".....\n");
  for (int i=vtstlen - 10; i < vtstlen; i++) {
    printf("%llu\n",vtst[i]);
  }
  

}
