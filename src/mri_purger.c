#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/*! Purge an image file to disk, to a random filename.
     - If the write to disk works, the image array is free()-ed.
     - If the write to disk fails, the image array is not free()-ed.
     - Call mri_unpurge() to read the file back in and delete it.
     - Call mri_killpurge() to just delete the file from disk.
------------------------------------------------------------------------------*/

void mri_purge( MRI_IMAGE *im )
{
   void *iar ;
   FILE *fp ;
   size_t npix ;
   char *pg , *un ;

ENTRY("mri_purge") ;

   if( im == NULL ) EXRETURN ;
   iar = mri_data_pointer(im) ; if( iar == NULL ) EXRETURN ;

   if( im->fondisk == IS_PURGED ){   /* should not happen */
     if( im->fname != NULL ) remove(im->fname) ;
     im->fondisk = 0 ;
   }

   if( im->fname != NULL ) free(im->fname) ;

                                   pg = getenv( "TMPDIR" ) ;
   if( pg == NULL || *pg == '\0' ) pg = getenv( "TEMPDIR" ) ;
   if( pg == NULL || *pg == '\0' ) pg = "/tmp" ;
   if( !THD_is_directory(pg) )     pg = "." ;

   im->fname = malloc(strlen(pg)+64) ;
   un = UNIQ_idcode(); un[0] = 'T'; un[1] = 'I'; un[2] = 'M';
   strcpy(im->fname,pg); strcat(im->fname,"/"); strcat(im->fname,un);
   free(un) ;

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
     free(iar) ; mri_clear_data_pointer(im) ; im->fondisk = IS_PURGED ;
     if( PRINT_TRACING ){
       long long nb = ((long long)im->pixel_size) * ((long long)im->nvox) ;
       char str[256] ;
       sprintf(str,"wrote %lld bytes to file %s",nb,im->fname); STATUS(str);
     }
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

   if( im == NULL || !MRI_IS_PURGED(im) ) EXRETURN ;
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
     fclose(fp) ; remove(im->fname) ; im->fondisk = 0 ; EXRETURN ;
   }

   npix = fread( iar , (size_t)im->pixel_size , (size_t)im->nvox , fp ) ;
   fclose(fp) ; remove(im->fname) ; im->fondisk = 0 ;

   if( npix < (size_t)im->nvox ){
     long long nb = ((long long)im->pixel_size) * ((long long)im->nvox) ;
     WARNING_message("mri_unpurge: Can't read %lld bytes from %s",nb,im->fname);
   } else if( PRINT_TRACING ){
     long long nb = ((long long)im->pixel_size) * ((long long)im->nvox) ;
     char str[256] ;
     sprintf(str,"read %lld bytes from file %s",nb,im->fname); STATUS(str);
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*! If the image is purged to disk, kill the purge file. */

void mri_killpurge( MRI_IMAGE *im )
{
ENTRY("mri_killpurge") ;
   if( im == NULL || !MRI_IS_PURGED(im) ) EXRETURN ;
   remove(im->fname) ; im->fondisk = 0 ;
   EXRETURN ;
}
