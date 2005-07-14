#define _DEBUGTRACE_MAIN_
#include <stdio.h>
#include <stdlib.h>
#include "mcw_malloc.h"
#include "debugtrace.h"  /* contains 1 function */

/*--------------------------------------------------------------------------*/

static void output_message( char *prefix , char *fmt , va_list vararg_ptr )
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
   msg = malloc(sizeof(char)*16*ll) ; msg[0] = '\0' ;
   vsprintf(msg,ifmt,vararg_ptr) ; ll = strlen(msg) ;
   if( msg[ll-1] != '\n' ){ msg[ll] = '\n' ; msg[ll+1] = '\0' ; }
   fputs(msg,stderr) ;
   if( ifmt != fmt ) free(ifmt) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void INFO_message( char *fmt , ... )
{
   va_list vararg_ptr ;
   va_start( vararg_ptr , fmt ) ;
   output_message( "++ " , fmt , vararg_ptr ) ;
   va_end( vararg_ptr ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void WARNING_message( char *fmt , ... )
{
   va_list vararg_ptr ;
   va_start( vararg_ptr , fmt ) ;
   output_message( "++ WARNING: " , fmt , vararg_ptr ) ;
   va_end( vararg_ptr ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void ERROR_message( char *fmt , ... )
{
   va_list vararg_ptr ;
   va_start( vararg_ptr , fmt ) ;
   output_message( "** ERROR: " , fmt , vararg_ptr ) ;
   va_end( vararg_ptr ) ;
   return ;
}

/*--------------------------------------------------------------------------*/

void ERROR_exit( char *fmt , ... )
{
   va_list vararg_ptr ;
   va_start( vararg_ptr , fmt ) ;
   output_message( "** FATAL ERROR: " , fmt , vararg_ptr ) ;
   va_end( vararg_ptr ) ;
   exit(1) ;
}
