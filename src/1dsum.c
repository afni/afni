#include "mrilib.h"

#define MAXIM 1024

/*** Adapted from 1dcat.c -- RWCox -- 18 Apr 2001 ***/

int main( int argc , char * argv[] )
{
   int nim , ii , jj , kk , nx ;
   MRI_IMAGE ** inim ;
   float * far ;
   int ncol , ic ;
   float * csum ;
   int nn_ignore=0 , mm_use=0 , iarg=1 ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dsum [options] a.1D b.1D ...\n"
            "where each file a.1D, b.1D, etc. is an ASCII file of numbers arranged\n"
            "in rows and columns. The sum of each column is written to stdout.\n"
            "\n"
            "Options:\n"
            "  -ignore nn = skip the first nn rows of each file\n"
            "  -use    mm = use only mm rows from each file\n"
           ) ;
      exit(0) ;
   }

   machdep() ;

   /* parse options */

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strncmp(argv[iarg],"-ignore",4) == 0 ){
         nn_ignore = (int) strtod(argv[++iarg],NULL) ;
         if( nn_ignore < 0 ){fprintf(stderr,"** Illegal -ignore value!\n");exit(1);}
         iarg++ ; continue ;
      }

      if( strncmp(argv[iarg],"-use",4) == 0 ){
         mm_use = (int) strtod(argv[++iarg],NULL) ;
         if( mm_use < 0 ){fprintf(stderr,"** Illegal -use value!\n");exit(1);}
         iarg++ ; continue ;
      }

      fprintf(stderr,"** Unknown option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /* read input files */

   nim = argc-iarg ;
   inim = (MRI_IMAGE **) malloc( sizeof(MRI_IMAGE *) * nim ) ;
   ncol = 0 ;
   for( jj=0 ; jj < nim ; jj++ ){
      inim[jj] = mri_read_1D( argv[jj+iarg] ) ;
      if( inim[jj] == NULL ){
         fprintf(stderr,"** Can't read input file %s\n",argv[jj+iarg]) ;
         exit(1) ;
      }
      if( jj > 0 && inim[jj]->nx != inim[0]->nx ){
         fprintf(stderr,
                 "** Input file %s doesn't match first file %s in length!\n",
                 argv[jj+iarg],argv[iarg]) ;
         exit(1) ;
      }
      ncol += inim[jj]->ny ;
   }

   if( mm_use == 0 ){
      mm_use = inim[0]->nx - nn_ignore ;
      if( mm_use < 0 ){fprintf(stderr,"** -ignore is too big for these files!\n");exit(1);}
   }
   if( nn_ignore + mm_use > inim[0]->nx ){
      fprintf(stderr,"** -ignore + -use is too big for these files!\n");exit(1);
   }

   csum = (float *) malloc(sizeof(float)*ncol) ;
   for( ic=0 ; ic < ncol ; ic++ ) csum[ic] = 0.0 ;

   nx = inim[0]->nx ;
   for( ii=nn_ignore ; ii < nn_ignore+mm_use ; ii++ ){
      for( ic=jj=0 ; jj < nim ; jj++ ){
         far = MRI_FLOAT_PTR(inim[jj]) ;
         for( kk=0 ; kk < inim[jj]->ny ; kk++ ) csum[ic++] += far[ii+kk*nx] ;
      }
   }

   for( ic=0 ; ic < ncol ; ic++ ) printf("%g ",csum[ic]) ;
   printf("\n");
   exit(0) ;
}
