#include "niml_private.h"

#undef  SKIP_COMMENT_FLAG
#define SKIP_COMMENT_FLAG  1

static char * my_fgets( char *, int, FILE *, int ) ;

/*---------------------------------------------------------------*/
/*! Read an un-headered file into a data element, guessing at
    its structure from the 1st non-comment line.
-----------------------------------------------------------------*/

NI_element * NI_read_file_nohead( char *fname )
{
   FILE *fp ;
   char prefix[32] , *ptr ;
   NI_element *nel ;

   if( fname == NULL || *fname == '\0' ) return NULL ;

   fp = fopen( fname , "r" ) ; if( fp == NULL ) return NULL ;

   /** see if this looks like a NIML-formatted file **/

   memset(prefix,0,32) ; fread(prefix,1,31,fp) ; rewind(fp) ;
   ptr = strchr(prefix,'<') ;
   if( ptr != NULL && isalpha(*(ptr+1)) ){
     NI_stream ns ;
     fclose(fp) ;
     ptr = NI_malloc(strlen(fname)+16) ;
     sprintf(ptr,"file:%s",fname) ;
     ns = NI_stream_open(ptr,"r") ; NI_free(ptr) ;
     if( ns == NULL ) return NULL ;
     nel = NI_read_element( ns , 66 ) ;
     NI_stream_close(ns) ;
     return nel ;
   }

}


/*---------------------------------------------------------------*/
/*! Length of line buffer */
#define LBUF 65536

/*---------------------------------------------------------------*/
/*! Like fgets, but also
     - skips blank lines
     - skips comment lines (if flags&SKIP_COMMENT_FLAG)
     - skips leading and trailing whitespace
     - catenates lines that end in '\' (replacing '\' with ' ')
-----------------------------------------------------------------*/

static char * my_fgets( char *buf, int size, FILE *fts, int flags )
{
   char *ptr ;
   int nbuf , ll,ii , cflag ;
   static char *qbuf=NULL ;
   int skip_comm = (flags & SKIP_COMMENT_FLAG) != 0 ;

   if( buf == NULL || size < 1 || fts == NULL ){
     NI_free(qbuf); qbuf = NULL; return NULL;
   }

   if( qbuf == NULL ) qbuf = NI_malloc(LBUF) ;  /* 1st time in */

   nbuf  = 0 ;  /* num bytes stored in buf so far */
   cflag = 0 ;  /* flag if we're catenating lines */

   while(1){   /* loop and read lines, creating a logical line */

     ptr = fgets( qbuf , LBUF , fts ) ; /* read next whole line */

     if( ptr == NULL ) break ;          /* must be end-of-file */

     /* skip leading whitespace */

     for( ; *ptr != '\0' && isspace(*ptr) ; ptr++ ) ; /* nada */

     /* skip entirely blank lines, unless we are catenating */

     if( *ptr == '\0' ){ if(cflag) break; else continue; }

     /* skip comment lines (even if we are catenating) */

     if( skip_comm &&
         (*ptr == '#' || (*ptr == '/' && *(ptr+1) == '/')) ) continue ;

     /* strip trailing whitespace */

     ll = strlen(ptr) ;                                  /* will be > 0 */
     for( ii=ll-1 ; isspace(ptr[ii]) && ii > 0 ; ii-- )  /* blank => NUL */
       ptr[ii] = '\0' ;

     ll = strlen(ptr) ;                 /* number of chars left */
     if( ll == 0 ) continue ;           /* should not happen */

     cflag = (ptr[ll-1] == '\\') ;      /* catenate next line? */
     if( cflag && ll > 1 && ptr[ll-2] == '\\' ) cflag = 0 ;
     if( cflag ) ptr[ll-1] = ' ' ;      /* replace '\' with ' ' */

     /* now copy what's left (ll+1 bytes) at tail of output buffer */

     if( nbuf+ll+1 > size ){   /* too much for output buffer? */
       ll = size - (nbuf+1) ;
       if( ll <= 0 ) break ;   /* should not happen */
     }

     memcpy(buf+nbuf,ptr,ll+1) ; nbuf += ll ;
     if( !cflag ) break ;

   } /* loop to get next line if catenation is turned on */

   /* and we is done */

   if( nbuf > 0 ) return buf ;      /* return what we read already */
   return NULL ;                    /* signal of failure get data  */
}
