/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#define MAXIM 1024

int main( int argc , char * argv[] )
{
   int nim , ii , jj , kk , nx ;
   MRI_IMAGE ** inim ;
   float * far ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dcat a.1D b.1D ...\n"
            "where each file a.1D, b.1D, etc. is an ASCII file of numbers\n"
            "arranged in rows and columns.\n"
            "The row-by-row catenation of these files is written to stdout.\n"
            "\n"
            TS_HELP_STRING
           ) ;
      exit(0) ;
   }

   machdep() ;

   /* read input files */

   nim = argc-1 ;
   inim = (MRI_IMAGE **) malloc( sizeof(MRI_IMAGE *) * nim ) ;
   for( jj=0 ; jj < nim ; jj++ ){
#if 0                                   /** for testing only **/
      if( AFNI_yesenv("ragged") ){
        MRI_IMAGE *qim ;
        qim      = mri_read_ascii_ragged( argv[jj+1] , 3.e+33 ) ;
        inim[jj] = mri_transpose(qim) ; mri_free(qim) ;
      } else
#endif
      inim[jj] = mri_read_1D( argv[jj+1] ) ;
      if( inim[jj] == NULL ){
         fprintf(stderr,"** Can't read input file %s\n",argv[jj+1]) ;
         exit(1) ;
      }
      if( jj > 0 && inim[jj]->nx != inim[0]->nx ){
         fprintf(stderr,
                 "** Input file %s doesn't match first file %s in length!\n",
                 argv[jj+1],argv[1]) ;
         exit(1) ;
      }
   }

   nx = inim[0]->nx ;
   for( ii=0 ; ii < nx ; ii++ ){
      for( jj=0 ; jj < nim ; jj++ ){
         far = MRI_FLOAT_PTR(inim[jj]) ;
         for( kk=0 ; kk < inim[jj]->ny ; kk++ ){
            printf(" %g", far[ii+kk*nx] ) ;
         }
      }
      printf("\n") ;
   }

   exit(0) ;
}
