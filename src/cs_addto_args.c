#include "cs.h"
#include "string.h"

#define BLEN 4096

void addto_args( int argc , char * argv[] , int * new_argc , char *** new_argv )
{
   int     n_argc , ii , nsin , nall ;
   char ** n_argv ;
   char lbuf[4096] ;
   char * sin , * cpt , * sss ;

   /*-- sanity checks --*/

   if( new_argc == NULL || new_argv == NULL ) return ;

   if( argc == 0 || argv == NULL ) return ;

   if( strcmp( argv[argc-1] , "-@" ) != 0 ){  /* nothing to do */
      *new_argv = NULL ;
      return ;
   }

   /* make copy of input args (except the last one) */

   n_argc = argc-1 ;
   n_argv = (char **) malloc( sizeof(char *) * n_argc ) ;
   for( ii=0 ; ii < n_argc ; ii++ )
      n_argv[ii] = argv[ii] ;

   /* suck the standard input in */

   nall = BLEN ;
   sin  = (char *) malloc( sizeof(char) * nall ) ;  /* will hold stdin */
   nsin = 0 ;

   do{
      cpt = fgets( lbuf , BLEN , stdin ) ; /* read line */
      if( cpt == NULL ) break ;            /* end of file */
      ii = strlen(lbuf) ;
      if( ii+nsin >= nall-4 ){             /* make more sin space */
         nall += BLEN ;
         sin   = (char *) realloc( sin , sizeof(char) * nall ) ;
      }
      strcat(sin,lbuf) ; nsin = strlen(sin) ;  /* add to sin array */
   } while(1) ;

   if( nsin == 0 ){             /* nothing => return */
      *new_argc = n_argc ;
      *new_argv = n_argv ;
      free(sin) ; return ;
   }

   /* break input into tokens, put them in the new arg list */

   cpt = strtok( sin , " \t\n\r\f\v" ) ;

   while( cpt != NULL ){
      ii  = strlen(cpt) ;
      sss = (char *) malloc( sizeof(char) * (ii+1) ) ;
      strcpy(sss,cpt) ;
      n_argc++ ;
      n_argv = (char **) realloc( n_argv , sizeof(char *) * n_argc ) ;
      n_argv[n_argc-1] = sss ;

      cpt = strtok( NULL , " \t\n\r\f\v" ) ;
   }

   *new_argc = n_argc ;
   *new_argv = n_argv ;
   free(sin) ; return ;
}
