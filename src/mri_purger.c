#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/*! Purge an image to disk in file fname.  If fname is NULL, then the internal
    im->fname string is used instead.  If that is also NULL, then a random
    name is made up.  If the write to disk fails, the image data in RAM will
    not be free()-ed.
------------------------------------------------------------------------------*/

void mri_purge( char *fname, MRI_IMAGE *im )
{
   void *iar ;
   FILE *fp ;
   size_t npix ;

ENTRY("mri_purge") ;

   if( im == NULL ) EXRETURN ;
   iar = mri_data_pointer(im) ; if( iar == NULL ) EXRETURN ;

   if( fname != NULL ){
     if( im->fname != NULL ) free((void *)im->fname) ;
     im->fname = strdup(fname) ;
   } else if( im->fname == NULL ){
     im->fname = UNIQ_idcode() ;
     im->fname[0] = 'T' ; im->fname[1] = 'M' ; im->fname[2] = 'P' ;
   }

   fp = fopen( im->fname , "wb" ) ;
   if( fp == NULL ){
     ERROR_message("mri_purge: Can't open file %s",im->fname) ;
     EXRETURN ;
   }

   npix = fwrite( iar , (size_t)im->pixel_size , (size_t)im->nvox , fp ) ;
   fclose(fp) ;

   if( npix < (size_t)im->nvox ){
     long long nb = ((long long)im->pixel_size) * ((long long)im->nvox) ;
     ERROR_message("mri_purge: Can't write %lld bytes to %s",nb,im->fname) ;
     remove(im->fname) ;
   } else {
     free(iar) ; mri_clear_data_pointer(im) ;
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*! Retrieve a mri_purge()-ed image from disk.  The temp file will be deleted.
------------------------------------------------------------------------------*/

void mri_unpurge( MRI_IMAGE *im )
{
   void *iar ;
   FILE *fp ;
   size_t npix ;

ENTRY("mri_unpurge") ;

   if( im == NULL || im->fname == NULL ) EXRETURN ;
   iar = mri_data_pointer(im) ; if( iar != NULL ) EXRETURN ;

   fp = fopen( im->fname , "rb" ) ;
   if( fp == NULL ){
     ERROR_message("mri_unpurge: Can't open file %s",im->fname) ;
     EXRETURN ;
   }

   iar = calloc( (size_t)im->pixel_size , (size_t)im->nvox ) ;
   if( iar == NULL ){
     long long nb = ((long long)im->pixel_size) * ((long long)im->nvox) ;
     ERROR_message("mri_unpurge: Can't malloc() %lld bytes",nb) ;
     fclose(fp) ; remove(im->fname) ; EXRETURN ;
   }

   npix = fread( iar , (size_t)im->pixel_size , (size_t)im->nvox , fp ) ;
   fclose(fp) ; remove(im->fname) ;

   if( npix < (size_t)im->nvox ){
     long long nb = ((long long)im->pixel_size) * ((long long)im->nvox) ;
     WARNING_message("mri_unpurge: Can't read %lld bytes from %s",nb,im->fname);
   }

   EXRETURN ;
}
