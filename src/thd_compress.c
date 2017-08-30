/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*---- editable configuration variables [moved here 10 May 2013 -- RWcox] ----*/

static char *COMPRESS_program[]    = { "gzip -1c > '%s'"  ,
                                       "bzip2 -1c > '%s'" ,
                                       "compress > '%s'"  ,
                                       "pigz -1c > '%s'"  ,
                                       "cat > '%s'"} ;         /* shouldn't be called */

static int   COMPRESS_program_ok[] = { 1 , 1 , 1 , 1 , 0 } ;     /* RWCox 03 Aug 1998 */

static char *COMPRESS_unprogram[]  = { "gzip -dc '%s'"  ,
                                       "bzip2 -dc '%s'" ,
                                       "uncompress -c '%s'",
                                       "pigz -dc '%s'"  ,
                                       "brikcomp -c '%s'" } ;

/*----------------------------------------------------------------------------*/
/* Check if pigz can be used in place of gzip for compression,
   and if pbzip2 can be used in place of bzip2.
*//*--------------------------------------------------------------------------*/

static void COMPRESS_setup_programs(void)  /* 03 May 2013 */
{
   char *pgname=NULL ;
   static char *cprog_gzip=NULL , *cprog_bzip2=NULL ;
   static char *uprog_gzip=NULL , *uprog_bzip2=NULL ;
   static int first=1 ;
   int        cind=-1;
   int    skip_pigz=AFNI_yesenv("AFNI_DONT_USE_PIGZ") ;

   if( !first ) return ;
   first = 0 ;
   if( getenv("SKIP_COMPRESS_SETUP") ) return; /* Temp. fix. Strange R crash. 
                                                   ZSS  09 May 2013           */ 
   /* for some symmetry, init pgname based on AFNI_COMPRESSOR
    * (for either gzip or pigz)           19 Sep 2013 [rickr] */
   cind = THD_enviro_write_compression();
   if      ( cind == COMPRESS_GZIP ) pgname = THD_find_executable("gzip") ;
   else if ( cind == COMPRESS_PIGZ ) pgname = THD_find_executable("pigz") ;
   else                              pgname = NULL ;

   /* if that failed (ignore which), just start clean */
   if( pgname == NULL && !skip_pigz ) pgname = THD_find_executable("pigz") ;
   if( pgname == NULL               ) pgname = THD_find_executable("gzip") ;
   if( pgname == NULL ){
     COMPRESS_program_ok[0] = COMPRESS_program_ok[3] = 0 ;
   } else {
     cprog_gzip = (char *)malloc(sizeof(char)*(strlen(pgname)+32)) ;
     sprintf(cprog_gzip,"%s -1c > '%%s'",pgname) ;
     COMPRESS_program[0] = COMPRESS_program[3] = cprog_gzip ;
     uprog_gzip = (char *)malloc(sizeof(char)*(strlen(pgname)+32)) ;
     sprintf(uprog_gzip,"%s -dc '%%s'",pgname) ;
     COMPRESS_unprogram[0] = COMPRESS_unprogram[3] = uprog_gzip ;
   }

                        pgname = THD_find_executable("pbzip2") ;
   if( pgname == NULL ) pgname = THD_find_executable("bzip2") ;
   if( pgname == NULL ){
     COMPRESS_program_ok[1] = 0 ;
   } else {
     cprog_bzip2 = (char *)malloc(sizeof(char)*(strlen(pgname)+32)) ;
     sprintf(cprog_bzip2,"%s -1c > '%%s'",pgname) ;
     COMPRESS_program[1] = cprog_bzip2 ;
     uprog_bzip2 = (char *)malloc(sizeof(char)*(strlen(pgname)+32)) ;
     sprintf(uprog_bzip2,"%s -dc '%%s'",pgname) ;
     COMPRESS_unprogram[1] = uprog_bzip2 ;
   }

   return ;
}

/*--------------------------------------------------------------*/

/*** check if the file exists on disk
     -- returns 1 if it does, 0 if it does not ***/

int COMPRESS_is_file( char * pathname )
{
   static struct stat buf ;
   int ii ;

   if( pathname == NULL ) return 0 ;
   ii = stat( pathname , &buf ) ; if( ii != 0 ) return 0 ;
   ii = (buf.st_mode & S_IFREG) != 0 ; return ii ;
}

/*--------------------------------------------------------------*/

/*** check if the filename has the apposite
     suffix for the compression mode given  ***/

int COMPRESS_has_suffix( char * fname , int mode )
{
   int ll ;

   if( mode < 0                 ) return 1 ;
   if( mode > COMPRESS_LASTCODE ) return 0 ;

   ll = strlen(fname) ;
   return ( ll > COMPRESS_suffix_len[mode] &&
            strcmp(COMPRESS_suffix[mode] ,
                   fname+(ll-COMPRESS_suffix_len[mode])) == 0 ) ;
}

/*--------------------------------------------------------------*/

/*** return the compression code for the given filename;
     if the file doesn't exist, will return COMPRESS_NOFILE ***/

int COMPRESS_filecode( char * fname )
{
   int ii ;
   char * buf ;

   if( fname == NULL || fname[0] == '\0' ) return COMPRESS_NOFILE ;

   /** check the filename suffix **/

   for( ii=0 ; ii <= COMPRESS_LASTCODE ; ii++ ){
      if( COMPRESS_has_suffix(fname,ii) ){
         if( COMPRESS_is_file(fname) ) return ii ;
         else                          return COMPRESS_NOFILE ;
      }
   }
   if( COMPRESS_is_file(fname) ) return COMPRESS_NONE ;

   /** add the suffixes to the name, and check again **/

   buf = AFMALL(char, sizeof(char) * (strlen(fname)+16) ) ;
   for( ii=0 ; ii <= COMPRESS_LASTCODE ; ii++ ){
      strcpy(buf,fname) ; strcat(buf,COMPRESS_suffix[ii]) ;
      if( COMPRESS_is_file(buf) ){ free(buf) ; return ii ; }
   }
   free(buf) ; return COMPRESS_NOFILE ;
}

/*--------------------------------------------------------------*/

/*** keep a table of which open files used fopen and which
     used popen -- because they must be closed differently ***/

#define NFOPMAX 16
static int fop_init = 0 ;
static int fop_fileno[NFOPMAX] ;
static int fop_popend[NFOPMAX] ;

static void putin_fop_table( FILE * fp , int ppp )
{
   int ii ;

   if( fp == NULL ) return ;  /* can't do much with nothing */

   if( ! fop_init ){                       /* initialize the table */
      for( ii=0 ; ii < NFOPMAX ; ii++ ){
         fop_fileno[ii] = -1 ;
         fop_popend[ii] =  0 ;
      }
      fop_init = 1 ;
   }

   for( ii=0 ; ii < NFOPMAX ; ii++ )       /* find an unused entry */
      if( fop_fileno[ii] < 0 ) break ;

   if( ii == NFOPMAX ){
      fprintf(stderr,"\n*** AFNI compressor table overflow!\n") ;
      return ;
   }

   fop_fileno[ii] = fileno(fp) ;   /* save the file number */
   fop_popend[ii] = ppp ;          /* save the popen code */
   return ;
}

/*** Use this to close a file,
     so that pclose or fclose can be called correctly ***/

int COMPRESS_fclose( FILE * fp )
{
   int fn , ii ;

   if( fp == NULL || ! fop_init ) return fclose(fp) ;

   fn = fileno(fp) ;
   for( ii=0 ; ii < NFOPMAX ; ii++ ){   /* find the file number */
      if( fop_fileno[ii] == fn ){       /* found it! */
         fop_fileno[ii] = -1 ;          /* empty this table entry */
         if( fop_popend[ii] ) return pclose(fp) ;
         else                 return fclose(fp) ;
      }
   }

   return fclose(fp) ;  /* couldn't find it, so use fclose */
}

/*--------------------------------------------------------------*/

/*** return a malloc-ed filename string that has the
     correct compression suffix attached.
     If NULL is returned, the file doesn't exist.
     The return string should be free()-ed after it is used. ***/

char * COMPRESS_filename( char * fname )
{
   char * buf ;
   int ll , mm ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   mm  = COMPRESS_filecode( fname ) ;  /* find compression mode */
   if( mm == COMPRESS_NOFILE ) return NULL ;

   ll  = strlen(fname) ;
   buf = AFMALL(char, sizeof(char) * (ll+16) ) ;  /* worst case */

   if( mm == COMPRESS_NONE ){
      strcpy(buf,fname) ;
   } else {
      if( ! COMPRESS_has_suffix(fname,mm) ){
         strcpy(buf,fname) ; strcat(buf,COMPRESS_suffix[mm]) ;
      } else {
         strcpy(buf,fname) ;
      }
   }
   return buf ;
}

/*--- May 1998: a simple routine ---*/

char * COMPRESS_add_suffix( char * fname , int mm )
{
   char * buf ;
   int ll ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   ll  = strlen(fname) ;
   buf = AFMALL(char, sizeof(char) * (ll+16) ) ;

   strcpy(buf,fname) ;
   if( mm >= 0 && mm <= COMPRESS_LASTCODE &&
       ! COMPRESS_has_suffix(fname,mm)      ){

      strcat(buf,COMPRESS_suffix[mm]) ;
   }

   return buf ;
}

/*--------------------------------------------------------------*/

/*** open a file for readin, possibly using compresson ***/

FILE * COMPRESS_fopen_read( char * fname )
{
   FILE * fp ;
   int mm ;
   char * buf , * cmd ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   COMPRESS_setup_programs() ;  /* 03 May 2013 */

   mm = COMPRESS_filecode( fname ) ;  /* find compression mode */

   if( mm == COMPRESS_NOFILE ) return NULL ;  /* can't do nothin */

   if( mm == COMPRESS_NONE ){
      fp = fopen(fname,"r") ;   /* open it normally */
      putin_fop_table(fp,0) ;   /* save its open method */
      return fp ;
   }

#if 1
   if( ! COMPRESS_has_suffix(fname,mm) ){
      buf = AFMALL(char, sizeof(char) * (strlen(fname)+16) ) ;
      strcpy(buf,fname) ; strcat(buf,COMPRESS_suffix[mm]) ;
   } else {
      buf = fname ;
   }
#else
   buf = fname ;
#endif

   /* allow for longer program names            16 May, 2014 [rickr] */
   /* (problem noted by D. Thompson)                                 */
   cmd = AFMALL(char,  sizeof(char) *
                       (strlen(buf)+strlen(COMPRESS_unprogram[mm])+4) ) ;
   sprintf(cmd,COMPRESS_unprogram[mm],buf) ;

   fp = popen(cmd,"r") ;    /* open a pipe to read the file */
   putin_fop_table(fp,1) ;  /* save its open method */

   free(cmd) ; if( buf != fname ) free(buf) ;
   return fp ;
}

/*-------------------------------------------------------------
    open a file for writing, possibly using compresson;
     mm should be one of the COMPRESS_ codes at the top
     of file thd_compress.h
---------------------------------------------------------------*/

FILE * COMPRESS_fopen_write( char * fname , int mm )
{
   FILE * fp ;
   char * buf , * cmd ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   COMPRESS_setup_programs() ;  /* 03 May 2013 */

   /* Don't compress if the compression program isn't marked as OK   */
   /* [For modes that can only be compressed offline, like BRIKCOMP] */

   if( mm < 0 || ! COMPRESS_program_ok[mm] ){
      fp = fopen(fname,"w") ;   /* open it normally */
      putin_fop_table(fp,0) ;   /* save its open method */
      return fp ;
   }

#if 1
   if( ! COMPRESS_has_suffix(fname,mm) ){
      buf = AFMALL(char, sizeof(char) * (strlen(fname)+16) ) ;
      strcpy(buf,fname) ; strcat(buf,COMPRESS_suffix[mm]) ;
   } else {
      buf = fname ;
   }
#else
   buf = fname ;
#endif

   /* allow for longer program names            16 May, 2014 [rickr] */
   /* (problem noted by D. Thompson)                                 */
   cmd = AFMALL(char,  sizeof(char) *
                       (strlen(buf)+strlen(COMPRESS_program[mm])+4) ) ;
   sprintf(cmd,COMPRESS_program[mm],buf) ;

   fp = popen(cmd,"w") ;    /* open a pipe to write the file */
   putin_fop_table(fp,1) ;  /* save its open method */

   free(cmd) ; if( buf != fname ) free(buf) ;
   return fp ;
}

/*----------------------------------------------------------*/

int COMPRESS_unlink( char * fname )
{
   char * fff = COMPRESS_filename(fname) ;
   int     ii = -1 ;
   if( fff != NULL ){ ii=unlink(fff); free(fff); }
   return ii ;
}
