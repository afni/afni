#include "mrilib.h"

/*----------------------------------------------------------------------------*/
static char *tmpdir = NULL ;

static int test_tmpdir_write(void)
{
   char *un , *tf ; FILE *fp ; size_t bb=0 ;
   if( tmpdir==NULL || *tmpdir=='\0' || !THD_is_directory(tmpdir) ) return 0 ;
   un = UNIQ_idcode() ;
   tf = malloc(strlen(un)+strlen(tmpdir)+8) ;
   strcpy(tf,tmpdir) ; strcat(tf,"/") ; strcat(tf,un) ; free(un) ;
   fp = fopen(tf,"w+b") ;
   if( fp == NULL ){ free(tf) ; return 0 ; }
   bb = fwrite( &bb , sizeof(size_t) , 1 , fp ) ;
   fclose(fp) ; remove(tf) ; free(tf) ;
   return ( bb == 1 ) ;
}

/*----------------------------------------------------------------------------*/
/*! Function to get name of the directory to store TIM_* mri_purge() files.
    The return pointer should NOT be free()-ed!
*//*--------------------------------------------------------------------------*/

char * mri_purge_get_tmpdir(void)
{
   if( tmpdir == NULL ){
                                tmpdir = getenv( "TMPDIR" ) ;
     if( !test_tmpdir_write() ) tmpdir = getenv( "TEMPDIR" ) ;
     if( !test_tmpdir_write() ) tmpdir = "/tmp" ;
     if( !test_tmpdir_write() ) tmpdir = "." ;    /* the last resort */
   }
   return tmpdir ;
}

/*----------------------------------------------------------------------------*/
/*! Function to return a unique name for temporary file, with the
    given prefix attached (if it isn't NULL, that is).
    The return pointer should be free()-ed when you are done with it.
*//*--------------------------------------------------------------------------*/

char * mri_get_tempfilename( char *pref )
{
   char *tdir , *unam , *fnam ; int nn ;

   tdir = mri_purge_get_tmpdir() ;

   do{
     unam = UNIQ_idcode() ;                        /* unique part of filename */
     nn   = (pref != NULL) ? strlen(pref) : 0 ;           /* length of prefix */
     fnam = (char *)malloc(sizeof(char)*(strlen(tdir)+strlen(unam)+nn+4)) ;
     strcpy(fnam,tdir) ;                    /* start with temp directory name */
     nn = strlen(fnam) ; if( fnam[nn-1] != '/' ) strcat(fnam,"/") ;
     if( pref != NULL ){ strcat(fnam,pref); strcat(fnam,"_"); } /* add prefix */
     strcat(fnam,unam) ; free(unam) ;                 /* append unique string */
     nn = THD_is_ondisk(fnam) ;                        /* should never happen */
     if( nn ) free(fnam) ;
   } while(nn) ;                                         /* should never loop */

   return fnam ;
}


/*----------------------------------------------------------------------------*/
static char tsuf[8] = "\0" ;

/*! Function to set TIM suffix for this process */

static void purge_set_tsuf(int v)  /* 01 Feb 2007 */
{
   char *un ;
   if( tsuf[0] != '\0' ) return ;
   un = UNIQ_idcode();
   tsuf[0] = un[5] ; tsuf[1] = un[6] ; tsuf[2] = un[7] ; tsuf[3] = '\0' ;
   if( v )
     INFO_message("temp files: if program crashes, do /bin/rm -f %s/TIM_%s*",
                  mri_purge_get_tmpdir() , tsuf ) ;
   return ;
}

char * mri_purge_get_tsuf(void){ purge_set_tsuf(0); return tsuf; }

/*----------------------------------------------------------------------------*/
/* Functions to save a list of TIM_* mri_purge() files, so that they can
   be deleted at program rundown, if they weren't mri_free()-ed already.
   If mainENTRY() was used, then even a program crash might do cleanup, since
   the most common fatal signals are caught and will end up invoking exit(),
   which will in turn invoke purge_atexit().
------------------------------------------------------------------------------*/

static int atexit_called = 0 ;   /* was atexit() already called for this? */

static int    npurge = 0 ;       /* number of filenames in qpurge array */
static char **qpurge = NULL ;    /* filenames of TIM_* files still alive */

static void purge_atexit(void) /*--- called by exit(): delete TIM_* files ---*/
{
   int ii , nn ;
   for( nn=ii=0 ; ii < npurge ; ii++ ){
     if( qpurge[ii] != NULL ){
       INFO_message("removing temporary image file %s",qpurge[ii]) ;
       remove(qpurge[ii]) ; nn++ ;
     }
   }
   if( tmpdir != NULL && nn > 0 && tsuf[0] != '\0' )
     WARNING_message("-usetemp: Check %s/ for other TIM_%s* files",tmpdir,tsuf);
   return ;
}

/*----------------------------------------------------------------------------*/

static void add_purge( char *fn ) /*-------- add fn to the qpurge list ----*/
{
   int ii ;
   if( fn == NULL || *fn == '\0' ) return ;
   for( ii=0 ; ii < npurge ; ii++ )  /* see if already in list */
     if( qpurge[ii] != NULL && strcmp(qpurge[ii],fn) == 0 ) break ;
   if( ii < npurge ) return ;        /* already in list! */
   for( ii=0 ; ii < npurge ; ii++ )  /* find an empty slot */
     if( qpurge[ii] == NULL ) break ;
   if( ii == npurge )                /* make new empty slot */
     qpurge = (char **)realloc(qpurge,sizeof(char *)*(++npurge)) ;
   qpurge[ii] = strdup(fn) ;         /* fill empty slot */
   return ;
}

/*----------------------------------------------------------------------------*/

static void kill_purge( char *fn ) /*---- remove fn from the qpurge list ----*/
{
   int ii ;
   if( fn == NULL || *fn == '\0' || qpurge == NULL ) return ;
   for( ii=0 ; ii < npurge ; ii++ )  /* find in list */
     if( qpurge[ii] != NULL && strcmp(qpurge[ii],fn) == 0 ) break ;
   if( ii < npurge ){ free(qpurge[ii]) ; qpurge[ii] = NULL ; }
   return ;
}

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

   /* make up a unique name for the purge file */

   pg        = mri_purge_get_tmpdir() ; purge_set_tsuf(1) ;
   im->fname = malloc(strlen(pg)+64) ;
   un = UNIQ_idcode();
   un[0] = 'T'     ; un[1] = 'I'     ; un[2] = 'M'     ; un[3] = '_' ;
   un[4] = tsuf[0] ; un[5] = tsuf[1] ; un[6] = tsuf[2] ;
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
     add_purge(im->fname) ;
     if( !atexit_called ){ atexit(purge_atexit); atexit_called = 1; }
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
   iar = im->im ; if( iar != NULL ) EXRETURN ;

   fp = fopen( im->fname , "rb" ) ;
   if( fp == NULL ){
     ERROR_message("mri_unpurge: Can't open file %s",im->fname) ;
     EXRETURN ;
   }

   /* make space for data we are about to receive */

   iar = calloc( (size_t)im->pixel_size , (size_t)im->nvox ) ;
   if( iar == NULL ){
     long long nb = ((long long)im->pixel_size) * ((long long)im->nvox) ;
     ERROR_message("mri_unpurge: Can't malloc() %lld bytes",nb) ;
     fclose(fp); remove(im->fname); im->fondisk = 0; kill_purge(im->fname);
     EXRETURN;
   }
   mri_fix_data_pointer( iar , im ) ;

   /* get the data, babeee! */

   npix = fread( iar , (size_t)im->pixel_size , (size_t)im->nvox , fp ) ;
   fclose(fp); remove(im->fname); im->fondisk = 0; kill_purge(im->fname);

   if( npix < (size_t)im->nvox ){  /* didn't get enuf data? */
     long long nb = ((long long)im->pixel_size) * ((long long)im->nvox) ;
     WARNING_message("mri_unpurge: Can't read %lld bytes from %s",nb,im->fname);
   } else if( PRINT_TRACING ){     /* debug tracing output? */
     long long nb = ((long long)im->pixel_size) * ((long long)im->nvox) ;
     char str[256] ;
     sprintf(str,"read %lld bytes from file %s",nb,im->fname); STATUS(str);
   }

   EXRETURN ;
}

/*----------------------------------------------------------------------------*/
/*! If the image is purged to disk, kill the purge file.
    Called from mri_free() to make sure that purged images are cleaned up.
------------------------------------------------------------------------------*/

void mri_killpurge( MRI_IMAGE *im )
{
   if( MRI_IS_PURGED(im) ){
     ENTRY("mri_killpurge") ;             /* only do traceback if work to do */
     remove(im->fname); im->fondisk = 0; kill_purge(im->fname);
     if( PRINT_TRACING ){
       char str[256]; sprintf(str,"removed file %s",im->fname); STATUS(str);
     }
     EXRETURN ;
   }
   return ;
}
