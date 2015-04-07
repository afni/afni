/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

void Syntax(void)
{
   printf(
    "Usage: 3dnvals [-all] [-verbose] dataset [dataset dataset ...]\n"
    "* Prints (to stdout) the number of sub-bricks in a 3D dataset.\n"
    "* If -all is specified, prints out all 4 dimensions:\n"
    "    Nx, Ny, Nz, Nvals\n"
    "* If -verbose is used then the header name of the dataset is printed first.\n"
    "* The function of this simple program is to help in scripting.\n"
   ) ;
   PRINT_COMPILE_DATE ; exit(0) ;
}

/*---------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset ;
   int iarg , all = 0, verbose = 0, cnt = 0;
   int rval = -1;  /* exit(1) if all datasets fail   7 Apr 2015 [rickr] */

   if( argc < 2 || strncmp(argv[1],"-help",4) == 0 ) Syntax() ;

   iarg = 1 ;
   cnt = 1;
   while (cnt < argc) {
      if( strncmp(argv[iarg],"-all",4) == 0 ){ all = 1 ; iarg++ ; }
      else if( strncmp(argv[iarg],"-verbose",5) == 0 ){ verbose = 1 ; iarg++ ; }
      ++cnt;
   }
   for( ; iarg < argc ; iarg++ ){
      dset = THD_open_dataset( argv[iarg] ) ;
      if( dset == NULL ){
         if ( rval == -1 ) rval = 1; /* if unset, set bad exit code */
         printf("-1\n") ;
         continue ;
      }
      rval = 0; /* set good exit code */
      if (!all) {
         if (verbose) {
            printf("%s: %d\n",
                     DSET_HEADNAME(dset),
                     DSET_NVALS(dset)) ;
         } else {
            printf("%d\n",DSET_NVALS(dset)) ;
         }
      } else {
         if (verbose) {
            printf("%s: %d %d %d %d\n",
                     DSET_HEADNAME(dset),
                     DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset),
                     DSET_NVALS(dset)) ;
         } else {
            printf("%d %d %d %d\n",
                     DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset),
                     DSET_NVALS(dset)) ;
         }
      }
      THD_delete_3dim_dataset( dset , False ) ;
   }

   if( rval == -1 ) exit(0) ; /* unset rval is okay */
   exit(rval) ;
}
