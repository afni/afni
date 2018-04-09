#include "mrilib.h"

#include "thd_dset_to_grayplot.c"

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset ;
   MRI_IMAGE *imout ;
   char *prefix = "Grayplot.png" ;
   byte *mask=NULL ; int mask_nvox=0 ;
   int iarg = 1 ;
   int polort=2 ; float fwhm=6.0f ;

   if( argc < 2 ){
     printf("\n"
      "Usage: 3dGrayplot [options] dataset\n"
     ) ;
     exit(0) ;
   }

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("'-prefix' needs an argument") ;
       prefix = strdup(argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nvox = DSET_NVOX(mset) ;
       if( DSET_BRICK_TYPE(mset,0) != MRI_byte  &&
           DSET_BRICK_TYPE(mset,0) != MRI_short   )
         ERROR_exit("-mask dataset is NOT byte- or short-valued :(") ;

       if( DSET_BRICK_TYPE(mset,0) == MRI_byte ){
         mask = DSET_BRICK_ARRAY(mset,0) ;
       } else {
         short *smm = DSET_BRICK_ARRAY(mset,0) ;
         mask = (byte *)calloc(sizeof(byte),mask_nvox) ;
         EDIT_coerce_type( mask_nvox , MRI_short,smm , MRI_byte,mask ) ;
         DSET_unload(mset) ;
       }
 
       mmm = THD_countmask( mask_nvox , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 19 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   if( mask == NULL ) ERROR_exit("Need '-mask'") ;

   if( iarg >= argc ) ERROR_exit("No input dataset?") ;

   dset = THD_open_dataset( argv[iarg] ) ;
   CHECK_OPEN_ERROR(dset,argv[iarg]) ;
   DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;

   if( mask_nvox != DSET_NVOX(dset) )
     ERROR_exit("mask and dataset don't match :(") ;

   imout = THD_dset_to_grayplot( dset , mask , polort , fwhm ) ;

   mri_write_png( prefix , imout ) ;
   exit(0) ;
}
