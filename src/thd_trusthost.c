#include <string.h>
#include <stdlib.h>

static int     host_num  = 0 ;
static char ** host_list = NULL ;

static char * init_hosts[] = { /* Initial list of OK computers */
    "141.106.106." ,           /* MCW computers (we're so trustworthy) */
    "127.0.0.1"    ,           /* localhost is always OK */
    "192.168.0."               /* private class B networks */
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

      str = getenv("AFNI_TRUSTHOST") ;
      if( str != NULL ) ADDHOST(str) ;

      for( ii=1 ; ii < 99 ; ii++ ){
         sprintf(ename,"AFNI_TRUSTHOST_%d",ii) ;
         str = getenv(ename) ;
         if( str != NULL ) ADDHOST(str) ;
      }
   }

   for( ii=0 ; ii < host_num ; ii++ )
      if( strstr(hostid,host_list[ii]) == hostid ) return 1 ;

   return 0 ;
}
