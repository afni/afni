/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

int main( int argc , char * argv[] )
{
   char prefix[255]="obi-wan-kenobi" , fname[255] ;
   int datum = -1 ;
   int iarg = 1 ;
   int gnim , nx , ny , nz , kz ;
   char ** gname ;
   MRI_IMARR * arr ;
   MRI_IMAGE * im , * qim ;
   FILE * fp ;

   /***** help? *****/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
       "Usage: imstack [options] image_filenames ...\n"
       "Stacks up a set of 2D images into one big file (a la MGH).\n"
       "Options:\n"
       "  -datum type   Converts the output data file to be 'type',\n"
       "                  which is either 'short' or 'float'.\n"
       "                  The default type is the type of the first image.\n"
       "  -prefix name  Names the output files to be 'name'.b'type' and 'name'.hdr.\n"
       "                  The default name is 'obi-wan-kenobi'.\n"
      ) ;

      exit(0) ;
   }

   /***** scan for option *****/

   while( iarg < argc && argv[iarg][0] == '-' ){

      /*** -datum ***/

      if( strcmp(argv[iarg],"-datum") == 0 ){
         if( ++iarg >= argc ){
            fprintf(stderr,"-datum needs a type!\n") ; exit(1) ;
         }
         if( strcmp(argv[iarg],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcmp(argv[iarg],"float") == 0 ){
            datum = MRI_float ;
         } else {
            fprintf(stderr,"-datum %s is illegal!\n",argv[iarg]) ; exit(1) ;
         }
         iarg++ ; continue ;
      }

      /*** -prefix ***/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         if( ++iarg >= argc ){
            fprintf(stderr,"-prefix needs a name!\n") ; exit(1) ;
         }
         strcpy(prefix,argv[iarg]) ;
         iarg++ ; continue ;
      }

      /*** ERROR ***/

      fprintf(stderr,"Unrecognized option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /***** Check if any filenames left *****/

   if( iarg >= argc ){
      fprintf(stderr,"No input image filenames?!\n") ; exit(1) ;
   }

   /***** Perform filename expansion on the input list *****/

   MCW_warn_expand(1) ;
   MCW_file_expand( argc - iarg , argv + iarg , &gnim , &gname ) ;
   MCW_warn_expand(0) ;

   if( gnim < 1 ){
      fprintf(stderr,"Filename expansion fails on input filenames?!\n") ; exit(1) ;
   }

   /***** Read all files *****/

   arr = mri_read_many_files( gnim , gname ) ;
   if( arr == NULL || IMARR_COUNT(arr) <= 0 ){
      fprintf(stderr,"Can't read input files?!\n") ; exit(1) ;
   }
   MCW_free_expand( gnim , gname ) ;
   fprintf(stderr,"Read in %d 2D slices\n",IMARR_COUNT(arr)) ;

   /***** Set output datum, if not already fixed *****/

   if( datum < 0 ){
      datum = IMARR_SUBIMAGE(arr,0)->kind ;

      if( datum != MRI_short && datum != MRI_float ){
         fprintf(stderr,"Input image type is %s -- you must use -datum!\n",
                 MRI_TYPE_name[datum]) ;
         exit(1) ;
      }
   }

   /***** Check images for equal sizes *****/

   nx = IMARR_SUBIMAGE(arr,0)->nx ;
   ny = IMARR_SUBIMAGE(arr,0)->ny ;
   nz = IMARR_COUNT(arr) ;

   for( kz=1 ; kz < nz ; kz++ ){
      if( IMARR_SUBIMAGE(arr,kz)->nx != nx ||
          IMARR_SUBIMAGE(arr,kz)->ny != ny   ){

         fprintf(stderr,"All images must be the same size (%d x %d)\n",nx,ny) ;
         exit(1) ;
      }
   }

   /***** Write the output brick *****/

   sprintf(fname,"%s.b%s",prefix,MRI_TYPE_name[datum]) ;
   fp = fopen( fname , "w" ) ;
   if( fp == NULL ){
      fprintf(stderr,"Can't open output file %s\n",fname) ; exit(1) ;
   }

   for( kz=0 ; kz < nz ; kz++ ){

      im = IMARR_SUBIMAGE(arr,kz) ;
      if( im->kind != datum ) qim = mri_to_mri( datum , im ) ;
      else                    qim = im ;

      fwrite( mri_data_pointer(qim) , qim->pixel_size , qim->nx * qim->ny , fp ) ;

      if( qim != im ) mri_free(qim) ;
      mri_free(im);
   }
   fclose( fp ) ;
   fprintf(stderr,"Wrote output brick %s\n",fname) ;

   /***** Write the output header *****/

   sprintf(fname,"%s.hdr",prefix) ;
   fp = fopen( fname , "w" ) ;
   if( fp == NULL ){
      fprintf(stderr,"Can't open output file %s\n",fname) ; exit(1) ;
   }

   fprintf( fp , "%d %d %d 0\n" , nx,ny,nz ) ;
   fclose(fp) ;
   fprintf(stderr,"Wrote output header %s: %d %d %d\n",fname,nx,ny,nz) ;

   exit(0) ;
}
