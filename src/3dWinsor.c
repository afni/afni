/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   float irad=1.5 ;
   int nrep=1 , cbot=-1,ctop=-1 ;
   char * prefix = "winsor" ;
   int keepzero = 0 ;
   int iarg ;
   THD_3dim_dataset * inset , * outset ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dWinsor [options] dataset\n"
             "Apply a 3D 'Winsorizing' filter to a short-valued dataset.\n"
             "\n"
             "Options:\n"
             " -irad rr   = include all points within 'distance'\n"
             "                rr in the operation, where distance\n"
             "                is defined as sqrt(i*i+j*j+k*k), and\n"
             "                (i,j,k) are voxel index offsets\n"
             "                [default rr=1.5]\n"
             "\n"
             " -cbot bb   = set bottom clip index to bb\n"
             "                [default = 20%% of the number of points]\n"
             " -ctop tt   = set top clip index to tt\n"
             "                [default = 80%% of the number of points]\n"
             "\n"
             " -nrep nn   = repeat filter nn times [default nn=1]\n"
             "                if nn < 0, means to repeat filter until\n"
             "                less than abs(n) voxels change\n"
             "\n"
             " -keepzero  = don't filter voxels that are zero\n"
             "\n"
             " -prefix pp = use 'pp' as the prefix for the output\n"
             "                dataset [default pp='winsor']\n"
      ) ;
      exit(0) ;
   }

   /*-- scan command line --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-keepzero") == 0 ){
         keepzero = 1 ;
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-irad") == 0 ){
         irad = strtod( argv[++iarg] , NULL ) ;
         if( irad < 1.0 || irad > 4.0 ){
            fprintf(stderr,"*** Illegal value after -irad!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-cbot") == 0 ){
         cbot = strtod( argv[++iarg] , NULL ) ;
         if( cbot < 1 ){
            fprintf(stderr,"*** Illegal value after -cbot!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-ctop") == 0 ){
         ctop = strtod( argv[++iarg] , NULL ) ;
         if( ctop < 1 ){
            fprintf(stderr,"*** Illegal value after -ctop!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-nrep") == 0 ){
         nrep = strtod( argv[++iarg] , NULL ) ;
         if( nrep == 0 ){
            fprintf(stderr,"*** Illegal value after -nrep!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"*** Illegal value after -prefix!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[iarg]); exit(1) ;
   }

   if( iarg >= argc ){
      fprintf(stderr,"*** No dataset name on command line?\n"); exit(1);
   }

   /*-- read input --*/

   inset = THD_open_dataset( argv[iarg] ) ;
   if( inset == NULL ){
      fprintf(stderr,"*** Can't open dataset: %s\n",argv[iarg]); exit(1);
   }

   if( DSET_BRICK_TYPE(inset,0) != MRI_short ){
      fprintf(stderr,"*** Dataset not stored as shorts!\n"); exit(1);
   }

   DSET_load(inset) ;
   if( !DSET_LOADED(inset) ){
      fprintf(stderr,"*** Can't read dataset into memory!\n"); exit(1);
   }

   if( DSET_NVALS(inset) > 1 ){
      fprintf(stderr,"+++ WARNING: only processing sub-brick #0\n") ;
   }

   /*-- compute output --*/

   outset = WINsorize( inset, nrep, cbot, ctop, irad, prefix, keepzero ) ;

   if( outset == NULL ){
      fprintf(stderr,"*** Can't compute Winsor filter!\n"); exit(1);
   }

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dWinsor" , argc,argv , outset ) ;
   DSET_write(outset) ;
   exit(0) ;
}
