/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <string.h>
#include <stdlib.h>

#include "afni_environ.h"

static int     host_num  = 0 ;
static char ** host_list = NULL ;

static char * init_hosts[] = { /* Initial list of OK computers */
    "141.106.106." ,           /* MCW computers (we're so trustworthy) */
    "127.0.0.1"    ,           /* localhost is always OK */
    "192.168."                 /* private class B networks */
} ;
#define INIT_NUM 3
#define HSIZE    32

#define ADDHOST(hh)                                                         \
  do{ host_list = (char **)realloc(host_list,sizeof(char *)*(host_num+1));  \
      host_list[host_num] = (char *) malloc(HSIZE);                         \
      strcpy( host_list[host_num] , hh ); host_num++; } while(0) ;          \

int TRUST_host( char * hostid )
{
   int ii ;
   char ename[HSIZE] , * str ;

   if( hostid == NULL ) return 0 ;

   if( host_num == 0 ){
      host_num = INIT_NUM ;
      host_list = (char **) malloc( sizeof(char *) * INIT_NUM ) ;
      for( ii=0 ; ii < INIT_NUM ; ii++ ){
         host_list[ii] = (char *) malloc(HSIZE) ;
         strcpy( host_list[ii] , init_hosts[ii] ) ;
      }

      str = my_getenv("AFNI_TRUSTHOST") ;
      if( str != NULL ) ADDHOST(str) ;

      for( ii=1 ; ii < 99 ; ii++ ){
         sprintf(ename,"AFNI_TRUSTHOST_%d",ii) ;
         str = my_getenv(ename) ;
         if( str != NULL ) ADDHOST(str) ;
      }
   }

   for( ii=0 ; ii < host_num ; ii++ )
      if( strstr(hostid,host_list[ii]) == hostid ) return 1 ;

   return 0 ;
}
