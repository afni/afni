/*************************************************************************/
/* This program returns to stdout the number of OpenMP threads available */
/*************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#ifdef USE_OMP
# include <omp.h>
#endif

int main( int argc , char *argv[] )
{
  int nthr = 1 ;

#ifdef USE_OMP
#pragma omp parallel
#pragma omp master
  { nthr = omp_get_num_threads() ; }
#endif

  printf("%d\n",nthr) ;
  exit(0) ;
}
