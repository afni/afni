/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

/*-- 21 Feb 2001: modified to be more flexible --*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include "afni_environ.h"
#include "Amalloc.h"

static int     host_num  = 0 ;
static char ** host_list = NULL ;

static char *init_hosts[] = { /* Initial list of OK computers */
    "141.106.106." ,           /* MCW computers (we're so trustworthy) */
    "128.231."     ,           /* NIH computers (also very trustworthy) */
    "127.0.0.1"    ,           /* localhost is always OK */
    "192.168."     ,           /* private class B networks */
    "0.0.0.0"                  /* sometimes this is localhost */
} ;
#define INIT_NUM (sizeof(init_hosts)/sizeof(char *))
#define HSIZE    32

#define USE_NIML
#ifdef  USE_NIML
# include "niml/niml.h"
#endif

/*----------------------------------------------------------------
   Return the Internet address (in 'dot' format, as a string)
   given the name of the host.  If NULL is returned, some
   error occurrrrred.  The string is malloc()-ed.
------------------------------------------------------------------*/

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>

static char * xxx_name_to_inet( char *host )
{
   struct hostent *hostp ;
   char *iname = NULL , *str ;
   int ll ;

   if( host == NULL || host[0] == '\0' ) return NULL ;

   hostp = gethostbyname(host) ; if( hostp == NULL ) return NULL ;

   str = inet_ntoa(*((struct in_addr *)(hostp->h_addr))) ;
   if( str == NULL || str[0] == '\0' ) return NULL ;

   ll = strlen(str) ; iname = AFMALL(char, ll+1) ; strcpy(iname,str) ;
   return iname ;
}

/*--------------------------------------------------------------------------
  Add a host to the trusted list
----------------------------------------------------------------------------*/

#include <ctype.h>

static void add_TRUST_host( char *hnam )
{
   char *hh=NULL ;
   int nh,ii ;

   if( hnam == NULL || hnam[0] == '\0' ) return ;

   /* see if host name is consistent with 012.345.678.901 format */

   nh = strlen(hnam) ;
   for( ii=0 ; ii < nh ; ii++ )
      if( !isdigit(hnam[ii]) && hnam[ii] != '.' ) break ;

   if( ii < nh ){                     /* not a dotted number */
      hh = xxx_name_to_inet( hnam ) ; /* so do a lookup on it */
      if( hh == NULL ) return ;       /* failed? */

   } else if( nh > HSIZE-1 ){         /* something bad? */
      return ;
   } else {
      hh = hnam ;                     /* store dotted number */
   }

   host_list = (char **) realloc(host_list,sizeof(char *)*(host_num+1)) ;
   host_list[host_num] = (char *) malloc(HSIZE) ;
   strcpy( host_list[host_num] , hh ) ; host_num++ ;

   if( hh != hnam ) free(hh) ;
   return ;
}

/*---------------------------------------------------------------------------
   Initialize the trusted list from the internal table and the environment
-----------------------------------------------------------------------------*/

static void init_TRUST_list(void)
{
   int ii ;
   char ename[HSIZE] , *str ;

   if( host_num == 0 ){
      host_num = INIT_NUM ;
      host_list = (char **) malloc( sizeof(char *) * INIT_NUM ) ;
      for( ii=0 ; ii < INIT_NUM ; ii++ ){
         host_list[ii] = (char *) malloc(HSIZE) ;
         strcpy( host_list[ii] , init_hosts[ii] ) ;
      }

      str = my_getenv("AFNI_TRUSTHOST") ;
      if( str != NULL ) add_TRUST_host(str) ;

      for( ii=1 ; ii <= 99 ; ii++ ){
         sprintf(ename,"AFNI_TRUSTHOST_%d",ii) ; str = my_getenv(ename) ;
         if( str == NULL && ii <= 9 ){
           sprintf(ename,"AFNI_TRUSTHOST_%02d",ii) ; str = my_getenv(ename) ;
         }
         if( str != NULL ) add_TRUST_host(str) ;
      }
   }

   return ;
}

/*---------------------------------------------------------------------------
  Externally callable routine to add a host to the trusted list
-----------------------------------------------------------------------------*/

void TRUST_addhost( char *hostname )
{
   if( hostname == NULL || hostname[0] == '\0' ) return ;
   if( host_num == 0 ) init_TRUST_list() ;
   add_TRUST_host(hostname) ;
#ifdef USE_NIML
   NI_add_trusted_host(hostname) ;
#endif
   return ;
}

/*---------------------------------------------------------------------------
   return 1 if we like this host (specified in 'dot' notation), 0 if we don't
-----------------------------------------------------------------------------*/

int TRUST_host( char *hostid )
{
   int ii ;

   if( host_num == 0 ) init_TRUST_list() ;

   if( hostid == NULL || hostid[0] == '\0' ) return 0 ;

   for( ii=0 ; ii < host_num ; ii++ )
      if( strstr(hostid,host_list[ii]) == hostid ) return 1 ;

   return 0 ;
}
