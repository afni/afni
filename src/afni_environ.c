/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*------------------------------------------------------------------------
   Read an entire file into a character string.  When you are
   done with the returned string, free() it.  If the string pointer
   is returned as NULL, something bad happened.
--------------------------------------------------------------------------*/

char * AFNI_suck_file( char * fname )
{
   int len , fd , ii ;
   char * buf ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   len = THD_filesize( fname ) ;
   if( len <= 0 ) return NULL ;

   fd = open( fname , O_RDONLY ) ;
   if( fd < 0 ) return NULL ;

   buf = (char *) malloc( sizeof(char) * (len+4) ) ;
   ii  = read( fd , buf , len ) ;
   close( fd ) ;
   if( ii <= 0 ){ free(buf) ; return NULL; }

   buf[len] = '\0' ;  /* 27 July 1998: 'len' used to be 'ii+1', which is bad */
   return buf ;
}

/*-----------------------------------------------------------------------
   Read environment section only from an AFNI setup file.
   [See also afni_setup.c]
-------------------------------------------------------------------------*/

#define ISTARRED(s) ( (s)[0]=='*' && (s)[1]=='*' && (s)[2]=='*' )

#define EOLSKIP                                                          \
  do{ for( ; fptr[0] != '\n' && fptr[0] != '\0' ; fptr++ ) ; /* nada */  \
      if( fptr[0] == '\0' ){ free(fbuf) ; return ; }                     \
      fptr++ ; } while(0)

#define GETSSS                                                            \
  do{ int nu=0,qq;                                                        \
      if( fptr-fbuf >= nbuf || fptr[0] == '\0' ){ free(fbuf); return; }   \
      str[0]='\0'; qq=sscanf(fptr,"%127s%n",str,&nu); nused+=nu;fptr+=nu; \
      if( str[0]=='\0' || qq==0 || nu==0 ){ free(fbuf); return; }         \
    } while(0)

#define GETSTR                                                             \
  do{ GETSSS ;                                                             \
      while(str[0]=='!' || (str[0]=='/' && str[1]=='/')){EOLSKIP; GETSSS;} \
    } while(0)

#define GETEQN                                      \
  do{ GETSTR ; if(ISTARRED(str)) goto SkipSection ; \
      strcpy(left,str) ;                            \
      GETSTR ; if(ISTARRED(str)) goto SkipSection ; \
      strcpy(middle,str) ;                          \
      GETSTR ; if(ISTARRED(str)) goto SkipSection ; \
      strcpy(right,str) ; } while(0)

#define NSBUF 256

static int afni_env_done = 0 ;

void AFNI_mark_environ_done(void){ afni_env_done = 1 ; return ; }

char * my_getenv( char * ename )
{
   if( !afni_env_done ){
      char * sysenv = getenv("AFNI_SYSTEM_AFNIRC") ;       /* 16 Apr 2000 */
      if( sysenv != NULL ) AFNI_process_environ(sysenv) ;  /* 16 Apr 2000 */
      AFNI_process_environ(NULL) ;
   }
   return getenv( ename ) ;
}

void AFNI_process_environ( char * fname )
{
   int    nbuf , nused , ii ;
   char * fbuf , * fptr ;
   char str[NSBUF] , left[NSBUF] , middle[NSBUF] , right[NSBUF] ;

   if( fname != NULL ){
      strcpy(str,fname) ;
   } else {
      char * home ;
      if( afni_env_done ) return ;
      home = getenv("HOME") ;
      if( home != NULL ){ strcpy(str,home) ; strcat(str,"/.afnirc") ; }
      else              { strcpy(str,".afnirc") ; }
      afni_env_done = 1 ;
   }

   fbuf = AFNI_suck_file( str ) ; if( fbuf == NULL ) return ;
   nbuf = strlen(fbuf) ;          if( nbuf == 0    ) return ;

   fptr = fbuf ; nused = 0 ;

   /** scan for section strings, which start with "***" **/

   str[0] = '\0' ;  /* initialize string */

   while( nused < nbuf ){

      /**----------------------------------------**/
      /**-- skip ahead to next section keyword --**/

      SkipSection: while( ! ISTARRED(str) ){ GETSTR; }

      /*- 04 Jun 1999 -*/

      if( strcmp(str,"***END") == 0 ) break ;  /* exit main loop */

      if( strcmp(str,"***ENVIRONMENT") != 0 ){ GETSTR ; goto SkipSection ; }

      /**---------------------------------------**/
      /**-- ENVIRONMENT section [04 Jun 1999] --**/

      if( strcmp(str,"***ENVIRONMENT") == 0 ){  /* loop, looking for environment settings */
         char * enveqn ; int nl , nr ;

         while(1){                          /* loop, looking for 'name = value' */
            GETEQN ;

            if( !THD_filename_ok(left) ) continue ;

            nl = strlen(left) ; nr = strlen(right) ;
            enveqn = (char *) malloc(nl+nr+4) ;
            strcpy(enveqn,left) ; strcat(enveqn,"=") ; strcat(enveqn,right) ;
            putenv(enveqn) ;
         }

         continue ;  /* to end of outer while */
      } /* end of ENVIRONMENT */

   }  /* end of while loop */

   free(fbuf) ; return ;
}

/*-----------------------------------------------------------------*/

int AFNI_yesenv( char * ename )     /* 21 Jun 2000 */
{
   char * ept ;
   if( ename == NULL ) return 0 ;
   ept = getenv(ename) ;
   return YESSISH(ept) ;
}

int AFNI_noenv( char * ename )     /* 21 Jun 2000 */
{
   char * ept ;
   if( ename == NULL ) return 0 ;
   ept = getenv(ename) ;
   return NOISH(ept) ;
}
