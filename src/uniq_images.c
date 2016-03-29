#include "mrilib.h"

static int images_equal( MRI_IMARR *aar , MRI_IMARR *bar )
{
   int nima = IMARR_COUNT(aar) ;
   int nimb = IMARR_COUNT(bar) ;
   int ii ;

   if( nima != nimb ) return 0 ;

   for( ii=0 ; ii < nima ; ii++ ){
     if( mri_equal( IMARR_SUBIM(aar,ii) , IMARR_SUBIM(bar,ii) ) == 0 ) return 0;
   }

   return 1 ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   MRI_IMARR **imar ;
   int num_imar , iopt=1 , jj,kk , ngood ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: uniq_images fileA fileB ...\n"
       "\n"
       "* Simple program to read in a list of image filenames,\n"
       "  determine which files have unique images inside, and\n"
       "  echo out only a list of the filenames with unique images.\n"
       "* This program is meant for use in scripts that deal with DICOM\n"
       "  servers that sometimes deal out multiple copies of the same\n"
       "  image in different filenames :-(\n"
       "* Author: Zhark the Comparator, October 2015.\n"
     ) ;
     exit(0) ;
   }

   mainENTRY("uniq_images") ; machdep() ;

   /* no options at this time, so leave iopt=1 */

   if( iopt+1 > argc )
     ERROR_exit("uniq_images: Need at least 1 filename, but don't have even that!") ;

   num_imar = argc - iopt ;
   imar     = (MRI_IMARR **)malloc(sizeof(MRI_IMARR *)*num_imar) ;
   for( ngood=jj=0 ; jj < num_imar ; jj++ ){
     imar[jj] = mri_read_file( argv[jj+iopt] ) ;
     if( imar[jj] == NULL ){
       ERROR_message("Can't read from file %s",argv[jj+iopt]) ;
     } else {
       ngood++ ;
     }
   }

   if( ngood < 1 )
     ERROR_exit("Didn't get at least 1 input file :-(") ;

   for( jj=0 ; jj < num_imar-1 ; jj++ ){
     if( imar[jj] == NULL ) continue ;
     for( kk=jj+1 ; kk < num_imar ; kk++ ){
       if( imar[kk] == NULL ) continue ;
       if( images_equal(imar[jj],imar[kk]) ) DESTROY_IMARR(imar[kk]) ;
     }
   }

   for( jj=0 ; jj < num_imar ; jj++ ){
     if( imar[jj] != NULL ){
       printf("%s ",argv[jj+iopt]) ;
       DESTROY_IMARR(imar[jj]) ;
     }
   }
   printf("\n") ;

   exit(0) ;
}
