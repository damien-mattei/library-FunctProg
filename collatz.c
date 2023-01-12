// openMP cube - collatz test

#include <omp.h>
#include <stdio.h>
#include <stdlib.h>



// OpenMP on macOS with Xcode tools:
// https://mac.r-project.org/openmp/

// export OMP_NUM_THREADS=8

// this main() in a library was only for testing openMP with Mac OS Xcode and Linux for use uncomment main() and comment openmp() functions


// mac os :
// clang  -I/opt/homebrew/opt/libomp/include  -L/opt/homebrew/opt/libomp/lib -Xclang -fopenmp -o collatz  -lomp collatz.c

// gcc -L/usr/lib/llvm-14/lib/ -fopenmp  -o collatz  -lomp collatz.c


unsigned long long *vtst;



unsigned long long collatz(unsigned long long n) {

  if (n == 1) return 1;

  if ((n % 2) == 0)
    return n / 2;
  else
    return 3*n + 1;

}

unsigned long long fct(unsigned long long x) {

  unsigned long long c;
  if (x == 0)
    c = 0;
  else
    c = collatz(x);
  
  return (x * x * x) + c;
}


unsigned long long fctapply(unsigned long long i) {
  return vtst[i] = fct(vtst[i]);
}




int main() {
  int vtstlen = 2642245; // cubic root of 18,446,744,073,709,551,615 https://en.wikipedia.org/wiki/C_data_types
  vtst = calloc(vtstlen, sizeof(unsigned long long));
  
  int ncpus = omp_get_max_threads();
  printf("Found a maximum of %i cores.\n",ncpus);
  printf("Program compute cube of numbers and add collatz result (1) with and without parallelisation with OpenMP library.\n\n");
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
  
    vtst[i]=i;
    
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
