#include "mrilib.h"

#undef  TDIR
#define TDIR "m2pAFNI/"

static int first=1 ;
static char *tmpdir      = NULL ;
static char *mpeg_filter = NULL ;

#undef  TSIZ
#define TSIZ 6666666

/*----------------------------*/
/*! Setup mpeg reading stuff. */

static void mpeg_setup(void)
{
   char *pg ;
   if( !first ) return ;

   first = 0 ;

   /* get a temporary directory name */

                                   pg = getenv( "TMPDIR" ) ;
   if( pg == NULL || *pg == '\0' ) pg = getenv( "TEMPDIR" ) ;
   if( pg == NULL || *pg == '\0' ) pg = "/tmp" ;
   if( !THD_is_directory(pg) )     pg = "." ;
   tmpdir = AFMALL( char, strlen(pg)+16) ;
   sprintf( tmpdir , "%s/%s" , pg , TDIR ) ;

   /* find the mpegtoppm executable */

   pg = THD_find_executable( "mpegtoppm" ) ;
   if( pg != NULL ){
     mpeg_filter = AFMALL( char, strlen(pg)+strlen(tmpdir)+64 ) ;
     sprintf( mpeg_filter , "%s -prefix %s %%s" , pg , tmpdir ) ;
   }
}

/*----------------------------------------------------------*/
/*! Convert an MPEG file into a set of images.
    Must have program mpegtoppm in the path.  [03 Dec 2003] */

MRI_IMARR * mri_read_mpeg( char *fname )
{
   char *pg ;
   int ii , allgray=1 ;
   FILE *fp ;
   MRI_IMAGE *im ;
   MRI_IMARR *imar ;

   /*--- check input for OK-ness ---*/

   if( fname == NULL || *fname == '\0' ) return NULL ;
   ii = mri_filesize(fname) ;
   if( ii <= 0 ) return NULL ;

   mpeg_setup() ;

   if( mpeg_filter == NULL ) return NULL ;  /* can't filter? */

   /*--- create the filter for this file and run it to create .ppm files ---*/

   pg = AFMALL(char, strlen(fname)+strlen(mpeg_filter)+32) ;  /* string to hold filter */
   sprintf( pg , mpeg_filter , fname ) ;
   THD_mkdir( tmpdir ) ;                    /* create the temp directory */
   if( !THD_is_directory(tmpdir) ){ free(pg); return NULL; }  /* can't?  */

   if( ii > TSIZ ) fprintf(stderr,"++ Decoding file %s",fname) ;
   system( pg ) ;    /* run the command */
   if( ii > TSIZ ) fprintf(stderr,".\n") ;

   /*--- read files from the temp directory ---*/

   INIT_IMARR(imar) ;
   for( ii=0 ; ; ii++ ){   /* loop until we fail to read */
     sprintf( pg , "%s%06d.ppm" , tmpdir,ii ) ;
     im = mri_read_ppm( pg ) ;
     if( im == NULL ) break ;
     allgray = allgray && mri_isgray(im) ;
     remove( pg ) ;
     ADDTO_IMARR(imar,im) ;
   }
   remove( tmpdir ) ; free(pg) ;

   /* if all images are grayscale, convert to byte-valued images */

   if( IMARR_COUNT(imar) == 0 ){
     DESTROY_IMARR(imar);
     imar = NULL;
   } else if( AFNI_yesenv("AFNI_MPEG_GRAYIZE") ){
     MRI_IMARR *qmar ;
     INIT_IMARR(qmar) ;
     for( ii=0 ; ii < IMARR_COUNT(imar) ; ii++ ){
       im = mri_to_byte( IMARR_SUBIM(imar,ii) ) ;
       ADDTO_IMARR(qmar,im) ;
       mri_free( IMARR_SUBIM(imar,ii) ) ;
     }
     FREE_IMARR(imar) ; imar = qmar ;
   }

   return imar ;
}

/*----------------------------------------------------------*/
/*! Count number of images in an MPEG file.
    Must have program mpegtoppm in the path.  [03 Dec 2003] */

int mri_imcount_mpeg( char *fname )
{
   char *pg , **ff , *fn ;
   int ii , nf=0 ;
   FILE *fp ;

   /*--- check input for OK-ness ---*/

   if( fname == NULL || *fname == '\0' ) return 0 ;
   ii = mri_filesize(fname) ;
   if( ii <= 0 ) return 0 ;

   mpeg_setup() ;

   if( mpeg_filter == NULL ) return 0 ;  /* can't filter? */

   /*--- create the filter for this file and run it to create .ppm files ---*/

   pg = AFMALL( char, strlen(fname)+strlen(mpeg_filter)+64) ;  /* string to hold filter */
   fn = AFMALL( char, strlen(fname)+32) ;
   sprintf(fn,"-count %s",fname) ;
   sprintf( pg , mpeg_filter , fn ) ;
   free(fn) ;
   THD_mkdir( tmpdir ) ;                 /* create the temp directory */
   if( !THD_is_directory(tmpdir) ){ free(pg); return 0; }  /* can't?  */

   system( pg ) ;    /* run the command */

   /*-- open the COUNT file in the temp directory --*/

   sprintf( pg , "%sCOUNT" , tmpdir ) ;
   fp = fopen(pg,"rb") ;
   if( fp != NULL ){ fscanf(fp,"%d",&nf); fclose(fp); remove(pg); }
   remove( tmpdir ) ; free(pg) ;
   return nf ;
}
