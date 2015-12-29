#define _DEBUGTRACE_MAIN_
#include <stdio.h>
#include <stdlib.h>
#include "mcw_malloc.h"
#include "AFNI_version.h"
#include "debugtrace.h"  /* contains 1 function */

/*--------------------------------------------------------------------------*/

static FILE *messfp = NULL ;
static char *messfn = NULL ;

void SET_message_file( char *fname )
{
   if( messfp != NULL ){ fclose(messfp); messfp = NULL; }
   if( messfn != NULL ){ free  (messfn); messfn = NULL; }

   { char *eee = getenv("AFNI_USE_ERROR_FILE") ;  /* 22 Sep 2015 */
     if( eee != NULL && (*eee == 'N' || *eee == 'n') ) return ;
   }

   if( fname != NULL && *fname != '\0' ) messfn = strdup(fname) ;
   return ;
}

static void fputs_messfp( char *msg )
{
   if( messfn != NULL && messfp == NULL ) messfp = fopen(messfn,"w") ;
   if( messfp != NULL ){ fputs(msg,messfp); fflush(messfp); }
   return ;
}


/*---------------------------------------------------------------------------*/
/* Originally written for THD_dataset_info(), now used in other places.
   * Used to write results into an ever-lengthening string.
   * On the first call, 'sss' should be NULL.  It will be malloc()-ed and
     contain the print-formatted results.
   * On a subsequent call, 'sss' is the return value from the previous call.
   * When finally done with it, you can free() the result.
   * Moved here from thd_info.c on 01 May 2015 by RWCox.
*//*-------------------------------------------------------------------------*/

#undef  ZMAX
#undef  SZMAX
#define ZMAX  32222        /* increased for Ziad (who else is so crazy?) */
#define SZMAX "%.32222s"   /* same as ZMAX */

char * THD_zzprintf( char *sss , char *fmt , ... )
{
   static char *sbuf = NULL ;  /* workspace */
   char *zz ;
   int   nzz , nsbuf ;
   va_list vararg_ptr ;

ENTRY("THD_zzprintf") ;

   va_start( vararg_ptr , fmt ) ;

   /* first time in ==> create workspace */

   if( sbuf == NULL ) sbuf = malloc(sizeof(char)*(ZMAX+90)) ;

   /* write current stuff into workspace */

   sbuf[0] = '\0' ;
   vsnprintf( sbuf , sizeof(char)*(ZMAX+89) , fmt , vararg_ptr ) ;
   nsbuf = strlen(sbuf) ;
   if( nsbuf == 0 ) RETURN(sss) ;  /* nothing happened */
   if( nsbuf >= ZMAX ){            /* too much happened */
     WARNING_message("THD_zzprintf() long string truncation = the ZSS syndrome") ;
     strcpy(sbuf+ZMAX-4,"...") ;
     nsbuf = strlen(sbuf) ;
   }

   /* make new space, copy input string sss, append new stuff, return result */

   if( sss == NULL || *sss == '\0' ){  /* no input string ==> copy new stuff */
     zz = (char *) malloc( sizeof(char)*(nsbuf+2) ) ;
     strcpy(zz,sbuf) ;
   } else {             /* the full Monty: copy old then new */
     nzz = strlen(sss) + nsbuf + 2 ;
     zz  = (char *) malloc( sizeof(char) * nzz ) ;
     strcpy(zz,sss) ; strcat(zz,sbuf) ;
     free(sss) ;       /* don't need input copy any more */
   }
   RETURN(zz) ;
}

/*--------------------------------------------------------------------------*/
/* Used to write messages into a buffer string rather than stderr */

static int   mess_use_outbuf = 0 ;     /* 01 May 2015 */
static char *mess_outbuf     = NULL ;

char * GET_message_outbuf(void){ return mess_outbuf ; }

void SET_message_outbuf( int use_outbuf )
{
   if( use_outbuf == 0 ){  /* turn message buffer string off */
     mess_use_outbuf = 0 ;
     if( mess_outbuf != NULL ) free(mess_outbuf) ;
     mess_outbuf = NULL ;
   } else {                /* turn it on */
     mess_use_outbuf = 1 ;
   }
}

/*---------------------------------------------------------------------------*/
/* Write the message to stderr or the message buffer string.
   If ump != 0, also write it to the message file pointer, if it is open.
*//*-------------------------------------------------------------------------*/

static void output_message( int ump, char *prefix, char *fmt, va_list vararg_ptr )
{
   char *ifmt , *msg , *epr ; int ll ;

   if( fmt == NULL || *fmt == '\0' ) return ;  /* makes no sense */

   if( prefix == NULL || *prefix == '\0' ){
     ifmt = fmt ;
   } else {
     ifmt = malloc( strlen(prefix)+strlen(fmt)+4 ) ;
     strcpy(ifmt,prefix) ;
     strcat(ifmt,fmt) ;
   }

   ll = strlen(ifmt) ; if( ll < 1024 ) ll = 1024 ;
   epr = getenv("AFNI_MESSAGE_PREFIX") ;
   if( epr != NULL ) ll += strlen(epr)+1 ;
   msg = malloc(sizeof(char)*16*ll+1) ; msg[0] = '\0' ;
   if( epr != NULL ){ strcpy(msg,epr); strcat(msg,"::"); }

   vsprintf(msg+strlen(msg),ifmt,vararg_ptr) ; ll = strlen(msg) ;
   if( msg[ll-1] != '\n' ){ msg[ll] = '\n' ; msg[ll+1] = '\0' ; }

   if( !mess_use_outbuf ){
     fputs(msg,stderr) ; fflush(stderr) ;
   } else {
     mess_outbuf = THD_zzprintf(mess_outbuf,"%s",msg) ;  /* 01 May 2015 */
   }
   if( ump ) fputs_messfp(msg) ;  /* 12 Mar 2007 */

   free(msg) ;  /* 03 Mar 2006: forgot the free! */
   if( ifmt != fmt ) free(ifmt) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void INFO_message( char *fmt , ... )
{
   va_list vararg_ptr ;
   va_start( vararg_ptr , fmt ) ;
   output_message( 0 , "++ " , fmt , vararg_ptr ) ;
   va_end( vararg_ptr ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void ININFO_message( char *fmt , ... )
{
   va_list vararg_ptr ;
   va_start( vararg_ptr , fmt ) ;
   output_message( 0 , " + " , fmt , vararg_ptr ) ;
   va_end( vararg_ptr ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void WARNING_message( char *fmt , ... )
{
   va_list vararg_ptr ;
   va_start( vararg_ptr , fmt ) ;
   output_message( 1 , "*+ WARNING: " , fmt , vararg_ptr ) ;
   va_end( vararg_ptr ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void ERROR_message( char *fmt , ... )
{
   va_list vararg_ptr ;
   va_start( vararg_ptr , fmt ) ;
   output_message( 1 , "** ERROR: " , fmt , vararg_ptr ) ;
   va_end( vararg_ptr ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void ERROR_exit( char *fmt , ... )
{
   va_list vararg_ptr ;
   va_start( vararg_ptr , fmt ) ;
   output_message( 1 , "** FATAL ERROR: " , fmt , vararg_ptr ) ;
   va_end( vararg_ptr ) ;
   fprintf(stderr,"** Program compile date = %s\n",__DATE__) ;
   exit(1) ;
}

/*--------------------------------------------------------------------------*/

#if 0
#ifdef USE_TRACING
void STATUS_message( char *fmt , ... )
{
   char *msg ; int ll ;
   va_list vararg_ptr ;
   va_start( vararg_ptr , fmt ) ;
   ll = strlen(fmt) ; if( ll < 128 ) ll = 128 ;
   msg = malloc(sizeof(char)*16*ll+1) ; msg[0] = '\0' ;
   sprintf(msg,"%*.*s%s -- ",DBG_num,DBG_num," ",DBROUT) ;
   ll = strlen(msg) ;
   vsprintf(msg+ll,fmt,vararg_ptr) ; ll = strlen(msg) ;
   if( msg[ll-1] != '\n' ){ msg[ll] = '\n'; msg[ll+1] = '\0'; }
   if( DBG_fp==NULL ) DBG_fp=stdout;
   fputs(msg,DBG_fp) ;
   strncpy(last_status,msg,1023); last_status[1023]='\0';
   free(msg) ; va_end( vararg_ptr ) ; return ;
}
#endif
#endif
