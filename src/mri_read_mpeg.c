#include "mrilib.h"

#define TDIR "m2pAFNI/"

/*! Convert an MPEG file into a set of images.
    Must have program mpegtoppm in the path.   [03 Dec 2003] */

MRI_IMARR * mri_read_mpeg( char *fname )
{
   static int first=1 ;
   static char *tmpdir      = NULL ;
   static char *mpeg_filter = NULL ;  /* mpegtoppm  */

   char *pg ;
   int nf , ii , allgray=1 ;
   FILE *fp ;
   MRI_IMAGE *im ;
   MRI_IMARR *imar ;

   /*--- check input for OK-ness ---*/

   if( fname == NULL || *fname == '\0' ) return NULL ;

   /*--- first time in, setup up filters to PPM format ---*/

   if( first ){
     first = 0 ;

     /* get a temporary directory name */

                                     pg = getenv( "TMPDIR" ) ;
     if( pg == NULL || *pg == '\0' ) pg = getenv( "TEMPDIR" ) ;
     if( pg == NULL || *pg == '\0' ) pg = "/tmp" ;
     if( !THD_is_directory(pg) )     pg = "." ;
     tmpdir = malloc(strlen(pg)+16) ;
     sprintf( tmpdir , "%s/%s" , pg , TDIR ) ;

     /* find the mpegtoppm executable */

     pg = THD_find_executable( "mpegtoppm" ) ;
     if( pg != NULL ){
       mpeg_filter = malloc(strlen(pg)+strlen(tmpdir)+64 ) ;
       sprintf( mpeg_filter , "%s -prefix %s %%s" , pg , tmpdir ) ;
     }
   }

   if( mpeg_filter == NULL ) return NULL ;  /* can't filter? */

   /*--- create the filter for this file and run it to create .ppm files ---*/

   pg = malloc(nf+strlen(mpeg_filter)+32) ;  /* string to hold filter */
   sprintf( pg , mpeg_filter , fname ) ;
   THD_mkdir( tmpdir ) ;                    /* create the temp directory */
   if( !THD_is_directory(tmpdir) ){ free(pg); return NULL; }  /* can't?  */

   system( pg ) ;    /* run the command */

   /*--- read files from the temp directory ---*/

   INIT_IMARR(imar) ;
   for( ii=0 ; ; ii++ ){   /* loop until we fail to read */
     sprintf( pg , "%s/%06d.ppm" , tmpdir,ii ) ;
     im = mri_read_ppm( pg ) ;
     if( im == NULL ) break ;
     allgray = allgray && mri_isgray(im) ;
     remove( pg ) ;
   }
   remove( tmpdir ) ;
   free(pg) ;

   if( IMARR_COUNT(imar) == 0 ){ DESTROY_IMARR(imar); imar = NULL; }
   return imar ;
}
