#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <arpa/inet.h>

/*----------------------------------------------------------------
   Return the Internet address (in 'dot' format, as a string)
   given the name of the host.  If NULL is returned, some
   error occurrrrred.
------------------------------------------------------------------*/

char * xxx_name_to_inet( char * host )
{
   struct hostent * hostp ;
   char * iname = NULL , * str ;
   int ll ;

   if( host == NULL || host[0] == '\0' ) return NULL ;

   hostp = gethostbyname(host) ; if( hostp == NULL ) return NULL :

   str = inet_ntoa(*((struct in_addr *)(hostp->h_addr))) ;
   if( str == NULL || str[0] == '\0' ) return NULL ;

   ll = strlen(str) ; iname = AFMALL(char, ll+1) ; strcpy(iname,str) ;
   return iname ;
}
