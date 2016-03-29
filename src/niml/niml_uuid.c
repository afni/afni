#include "niml_private.h"

/*************************************************************************/
/***************** Unique Identifier String functions ********************/
/*************************************************************************/

#include <sys/utsname.h>  /* Need by UNIQ_ functions for uname() */
#include <time.h>

static char *get_MAC_addr(void) ;  /* prototype */

/*-----------------------------------------------------------------------*/
/*! Return a globally unique string (I hope).  This can be hashed to
    produce a unique idcode (cf. UNIQ_idcode and UUID_idcode).

  Method: Generate a string from the system identfier information and
          the current time of day.  The output string is malloc()-ed,
          and should be free()-ed when no longer needed.

  20 Aug 2002 -- RWCox: break string and hashing into separate functions.
*//*---------------------------------------------------------------------*/

static char * get_UNIQ_string(void)
{
   static struct utsname ubuf ;
   static int ncall=0 ;                /* number of times I've been called */
   struct timeval tv ;
   int    nn , ii ;
   int  nbuf ;
   char *buf ;
#define NURR 32                        /* # bytes from /dev/urandom at a time */
#ifdef NURR
   static int nurr=0 ;
   static byte urr[NURR] ;     /* will use 1 byte from urr[nurr] */
#endif

   /* get info about this system */

   if( ncall == 0 ){                   /* 21 Aug 2002: only 1st time in */
     nn = uname( &ubuf ) ;             /* get info about this system */
     if( nn == -1 ){                   /* should never happen */
       strcpy( ubuf.nodename , "E" ) ;
       strcpy( ubuf.sysname  , "L" ) ;
       strcpy( ubuf.release  , "V" ) ;
       strcpy( ubuf.version  , "I" ) ;
       strcpy( ubuf.machine  , "S" ) ;
     }
   }

   /* store system info into a string buffer */

   nbuf = strlen(ubuf.nodename)+strlen(ubuf.sysname)
         +strlen(ubuf.release )+strlen(ubuf.version)+strlen(ubuf.machine) ;

   buf = (char *)malloc(nbuf+222) ;      /* include some extra space */
   strcpy(buf,ubuf.nodename) ;
   strcat(buf,ubuf.sysname ) ;
   strcat(buf,ubuf.release ) ;
   strcat(buf,ubuf.version ) ;
   strcat(buf,ubuf.machine ) ;

   /* get time and store into buf (along with process+user id and ncall) */

   nn = gettimeofday( &tv , NULL ) ;
   if( nn == -1 ){              /* should never happen */
     tv.tv_sec  = (long) time(NULL) ;  /* get seconds another way */
     tv.tv_usec = (long) buf ;         /* address as an integer */
   }

   /* even if called twice in very rapid succession,
      at least ncall will differ, so we'll get different ID codes  */

   sprintf( buf+nbuf,"%d%d%d%d%d%d",
            (int)tv.tv_sec,(int)tv.tv_usec,
            (int)getpid(),(int)getppid(),(int)getuid(),
            ncall ) ;
   ncall++ ;

   /* 06 Jan 2003: append MAC address, if possible */

   strcat(buf,get_MAC_addr()) ;

#ifdef NURR
   /* 24 Jul 2002: get random bytes from /dev/urandom  */
   /* 21 Aug 2002: read NURR bytes at a time, but only use 1 per call */

   if( nurr >= 0 ){
     if( nurr == 0 ){                              /* need to read more bytes */
       FILE *ufp=fopen("/dev/urandom","rb") ;
       if( ufp == NULL ){                          /* fails on open */
         nurr = -1; goto URR_DONE;                 /* so never try again */
       } else {                                    /* read some bytes */
         fread( &urr , 1,NURR, ufp ); fclose(ufp);
       }
     }
     nbuf = strlen(buf); sprintf(buf+nbuf,"%02x",(int)urr[nurr]);
     nurr = (nurr+1) % NURR ;
URR_DONE: ;
   }
#endif /* NURR */

   return buf ;
}

/*-----------------------------------------------------------------------*/
/*! Return a globally unique identifier (I hope).  This is a malloc()-ed
  string of length <= 31 (plus the NUL byte; the whole thing will fit
  into a char[32] array).  The output does not contain any '/'s, so
  it could be used as a temporary filename.  Repeated calls to this
  function should never return the same string.

  Method: Generate a string from the system identfier information and
          the current time of day. MD5 hash this to a 128 byte code.
          Base64 encode this to a 22 byte string. Replace '/' with '-'
          and '+' with '_'. Add 4 character prefix (1st 3 characters
          of environment variable IDCODE_PREFIX plus '_').

  Sample output: "XYZ_VdfGpfzy_NlY-2d7tA8Q1w"
*//*---------------------------------------------------------------------*/

char * UNIQ_idcode(void)
{
   char *buf , *idc ;

   /* get uniq string from system */

   buf = get_UNIQ_string() ;

   /* make the output by hashing the string in buf */

   idc = UNIQ_hashcode( buf ) ;

   /* free workspace and get outta here */

   free(buf) ; return idc ;
}

/*----------------------------------------------------------------------*/
/*! Fill the 3 character idcode prefix and cap with '\0' for safety
*//*--------------------------------------------------------------------*/

void UNIQ_hashprefix_fill( char *idc )
{
   if (idc) {
      char *eee ;
      int ii ;
      eee = getenv("IDCODE_PREFIX") ;
      if( eee != NULL && isalpha(eee[0]) ){
        for( ii=0 ; ii < 3 && isalnum(eee[ii]) ; ii++ )
          idc[ii] = eee[ii] ;
        idc[ii] = '\0';
      } else {
        strcpy(idc,"XYZ") ;  /* innocent default prefix */
      }
   }
   
   return;
}

/*----------------------------------------------------------------------*/
/*! Return the 3 character idcode prefix and cap with '\0',
    environment variable IDCODE_PREFIX is not expected to change
    within a session.
*//*--------------------------------------------------------------------*/
char * UNIQ_hashprefix( void )
{
   static char idr[4]={""};
   if (idr[0] == '\0') UNIQ_hashprefix_fill( idr );
   return(idr);
}

/*----------------------------------------------------------------------*/
/*! Make an idcode-formatted malloc-ed string from an input string.
    Unlike UNIQ_idcode(), this will always return the same value,
    given the same input.
*//*--------------------------------------------------------------------*/

char * UNIQ_hashcode( char *str )
{
   char *idc , *eee ;
   int ii , nn ;

   idc = (char *)calloc(1,32) ;
   
   UNIQ_hashprefix_fill( idc );  /* ZSS Apr 2013 */
   strcat(idc,"_") ;  /* recall idc was calloc()-ed */

   if( str == NULL || str[0] == '\0' ) str = "Onen i Estel Edain" ;
   eee = MD5_B64_string(str) ;
   nn = strlen(eee) ;
   for( ii=0 ; ii < nn ; ii++ ){
          if( eee[ii] == '/' ) eee[ii] = '-' ;  /* / -> - */
     else if( eee[ii] == '+' ) eee[ii] = '_' ;  /* + -> _ */
   }
   strcat(idc,eee) ; free(eee) ; return idc ;
}

/*----------------------------------------------------------------------*/
/*! Fill a user-supplied buffer (length at least 32) with an idcode.
    That is, idc should point to a char array of length 32 (or more).
*//*--------------------------------------------------------------------*/

void UNIQ_idcode_fill( char *idc )
{
   char *bbb ;
   if( idc == NULL ) return ;
   bbb = UNIQ_idcode() ;
   strcpy(idc,bbb) ; free(bbb) ; return ;
}

/*----------------------------------------------------------------------*/
/*! Produce a shorter idcode, purely alphanumeric.
*//*--------------------------------------------------------------------*/

char * UNIQ_idcode_11(void)
{
   static char *abc
          = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789." ;
   char *bbb , *uuu ; unsigned int ii , ss ;
   bbb = UNIQ_idcode() ;
   uuu = (char *)malloc(sizeof(char)*12) ;
   for( ii=0 ; ii < 11 ; ii++ ){
     ss = ((unsigned int)(bbb[2*ii+4]) + (unsigned int)(bbb[2*ii+5]))%62 ;
     uuu[ii] = abc[ss] ;
   }
   uuu[11] = '\0' ; free(bbb) ; return uuu ;
}

/*----------------------------------------------------------------------*/
/*! Hash a string and return a malloc()-ed string (36+1 bytes) in
    the "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx" format.
*//*--------------------------------------------------------------------*/

char *UUID_hashcode( char *str )
{
   MD5_CTX context;
   unsigned char digest[16];
   char *idc ;
   int ii , nn ;

   if( str == NULL || str[0] == '\0' ) str = "Onen i Estel Edain" ;

   MD5Init( &context ) ;
   MD5Update( &context, (unsigned char *)str, strlen(str) ) ;
   MD5Final( digest, &context ) ;

   idc = (char *)calloc(1,48) ;
   sprintf(idc,
     "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x" ,
     digest[0] , digest[1] , digest[2] , digest[3] , digest[4] ,
     digest[5] , digest[6] , digest[7] , digest[8] , digest[9] ,
     digest[10], digest[11], digest[12], digest[13], digest[14],
     digest[15]
    ) ;

   return idc ;
}

/*----------------------------------------------------------------------*/
/*! Hash a unique string and return a malloc()-ed string (36+1 bytes) in
    the "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx" format.
    The result should be unique worldwide, for all time.
*//*--------------------------------------------------------------------*/

char * UUID_idcode(void)
{
   char *buf , *idc ;

   /* get uniq string from system */

   buf = get_UNIQ_string() ;

   /* make the output by hashing the string in buf */

   idc = UUID_hashcode( buf ) ;

   /* free workspace and get outta here */

   free(buf) ; return idc ;
}

/************************************************************************/
#include <sys/ioctl.h>
#include <net/if.h>
static char *get_MAC_addr(void)  /* 06 Jan 2003 */
{
  static char str[64] = "?" ;
#if defined(LINUX) && defined(SIOCGIFHWADDR)
  static int ncall=0 ;

  if( ncall == 0 ){
    int sd ;
    sd = socket(AF_INET, SOCK_DGRAM, 0);
    if( sd >= 0 ){
      struct ifreq ifr ;
      strcpy(ifr.ifr_name, "eth0") ;
      if( ioctl(sd,SIOCGIFHWADDR,&ifr) >= 0 ){
        sprintf(str ,
                "%02x:%02x:%02x:%02x:%02x:%02x",
         (byte)ifr.ifr_hwaddr.sa_data[0], (byte)ifr.ifr_hwaddr.sa_data[1],
         (byte)ifr.ifr_hwaddr.sa_data[2], (byte)ifr.ifr_hwaddr.sa_data[3],
         (byte)ifr.ifr_hwaddr.sa_data[4], (byte)ifr.ifr_hwaddr.sa_data[5] ) ;
      }
      close(sd) ;
    }
    ncall = 1 ;
  }
#endif
  return str ;
}
