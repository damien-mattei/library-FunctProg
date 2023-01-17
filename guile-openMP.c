// openMP for Guile

#include <libguile.h>
#include <omp.h>
#include <stdio.h>
#include <sys/time.h> 


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

//#define CHRONO_MODE 1

int openmp(int start,int stop,char * func_name) { 

  //printf("guile-openMP.c : openmp\n");
  
  SCM func;

#ifdef CHRONO_MODE
  struct timeval t1, t2;
  double elapsedTime;
  static long double totalOpenMPcomputationTime = 0.0;
  static int chronoIndex = 0;

  printf("guile-openMP.c : START chrono number %i.Data size=%i ",chronoIndex,stop - start);

  fflush(stdout);
#endif

  
#ifdef CHRONO_MODE
  // start timer
  gettimeofday(&t1, NULL);
#endif

  scm_init_guile();
  func = scm_variable_ref(scm_c_lookup(func_name));

  static int ncpus = 0; //1;//0; //2; when 0 the program will determine itself the number of CPUs on the hardware
  static int one_display = 0;

  if (!ncpus) {
    ncpus = omp_get_max_threads(); // once by call
    printf("guile-openMP.c : openmp : Found %i cores.\n",ncpus);
  }
  else {
    if (!one_display) {
      one_display = 1;
      printf("guile-openMP.c : openmp : Using %i cores.\n",ncpus); }
  }

  char * arr_guile_init;

  arr_guile_init = calloc(ncpus, sizeof(char));
  
  
  int i,iam;

  for (i = 0 ; i < ncpus; i++) 
    arr_guile_init[i] = 0;


  omp_set_num_threads(ncpus);

  
#pragma omp parallel for private(i,iam) shared(arr_guile_init)
  
  for (i=start; i<=stop; i++)  { /* i is private by default */

    iam = omp_get_thread_num();
    if (!arr_guile_init[iam]) {
      arr_guile_init[iam] = 1;
      //printf("Thread number: iam=%i ,scm_init_guile()\n",iam);
      scm_init_guile(); // needed inside // region because we are in a new thread?
    }
    
    scm_call_1( func , scm_from_int(i) );

  }

#ifdef CHRONO_MODE
  // stop timer
  gettimeofday(&t2, NULL);

  // compute and print the elapsed time in millisec
  elapsedTime = (t2.tv_sec - t1.tv_sec) * 1000.0;      // sec to ms
  elapsedTime += (t2.tv_usec - t1.tv_usec) / 1000.0;   // us to ms
  totalOpenMPcomputationTime += elapsedTime;
  printf("guile-openMP.c : STOP chrono : elapsedTime = %f ms, totalOpenMPcomputationTime = %Lf ms.\n", elapsedTime,totalOpenMPcomputationTime);
  chronoIndex++;
#endif

  
  return 1;
  
}


int forfunct(int start,int stop,char * func_name) {


  SCM func;

#ifdef CHRONO_MODE
  struct timeval t1, t2;
  double elapsedTime;
  static long double totalcomputationTime = 0.0;
  static int chronoIndex = 0;

  printf("guile-openMP.c : START chrono number %i.Data size=%i ",chronoIndex,stop - start);

  fflush(stdout);
#endif

  
#ifdef CHRONO_MODE
  // start timer
  gettimeofday(&t1, NULL);
#endif

  scm_init_guile();
  
  func = scm_variable_ref(scm_c_lookup(func_name));

  for (int i=start; i<=stop; i++)
    scm_call_1( func , scm_from_int(i) );

#ifdef CHRONO_MODE
  // stop timer
  gettimeofday(&t2, NULL);

  // compute and print the elapsed time in millisec
  elapsedTime = (t2.tv_sec - t1.tv_sec) * 1000.0;      // sec to ms
  elapsedTime += (t2.tv_usec - t1.tv_usec) / 1000.0;   // us to ms
  totalcomputationTime += elapsedTime;
  printf("guile-openMP.c : STOP chrono : elapsedTime = %f ms, total For Funct Computation Time = %Lf ms.\n", elapsedTime,totalcomputationTime);
  chronoIndex++;
#endif

  
  return 1;
  
}
