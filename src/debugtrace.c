#define _DEBUGTRACE_MAIN_
#include <stdio.h>
#include <stdlib.h>
#include "mcw_malloc.h"
#include "AFNI_label.h"
#include "debugtrace.h"  /* contains 1 function */

/*--------------------------------------------------------------------------*/

static FILE   *messfp = NULL ;
static char   *messfn = NULL ;

void SET_message_file( char *fname )
{
   if( messfp != NULL ){ fclose(messfp); messfp = NULL; }
   if( messfn != NULL ){ free  (messfn); messfn = NULL; }

   if( fname != NULL && *fname != '\0' ) messfn = strdup(fname) ;
   return ;
}

static void fputs_messfp( char *msg )
{
   if( messfn != NULL && messfp == NULL ) messfp = fopen(messfn,"w") ;
   if( messfp != NULL ){ fputs(msg,messfp); fflush(messfp); }
   return ;
}

/*--------------------------------------------------------------------------*/

static void output_message( int ump, char *prefix, char *fmt, va_list vararg_ptr )
{
   char *ifmt , *msg ; int ll ;

   if( fmt == NULL || *fmt == '\0' ) return ;
   if( prefix == NULL || *prefix == '\0' ){
     ifmt = fmt ;
   } else {
     ifmt = malloc( strlen(prefix)+strlen(fmt)+4 ) ;
     strcpy(ifmt,prefix) ;
     strcat(ifmt,fmt) ;
   }
   ll = strlen(ifmt) ; if( ll < 1024 ) ll = 1024 ;
   msg = malloc(sizeof(char)*16*ll+1) ; msg[0] = '\0' ;
   vsprintf(msg,ifmt,vararg_ptr) ; ll = strlen(msg) ;
   if( msg[ll-1] != '\n' ){ msg[ll] = '\n' ; msg[ll+1] = '\0' ; }
   fputs(msg,stderr) ;
   if( ump ) fputs_messfp(msg) ;  /* 12 Mar 2007 */
   free(msg) ;  /* 03 Mar 2006: forgot the free! */
   if( ifmt != fmt ) free(ifmt) ;
   fflush(stdout) ; fflush(stderr) ; return ;
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
