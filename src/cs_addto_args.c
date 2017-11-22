/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "cs.h"
#include "string.h"

#define BLEN 40960

/*------------------ 18 Nov 1999: utility routines --------------------*/

static void tokenize_string( char * sin , int * ntok , char *** stok )
{
   int     n_tok , ii ;
   char ** s_tok , *cpt , *sss ;

   if( stok == NULL ) return ;
   if( ntok == NULL || sin == NULL || sin[0] == '\0' ){ *stok = NULL; return; }

   n_tok = 0 ;
   s_tok = (char **) malloc( sizeof(char *) ) ;

   /* break input into tokens, copy them in the new arg list */

   cpt = strtok( sin , " \t\n\r\f\v" ) ;
   if( cpt == NULL ){ free(s_tok); *stok = NULL; return; }     /* do nothing */

   while( cpt != NULL ){
      ii  = strlen(cpt) ;
      sss = (char *) malloc( sizeof(char) * (ii+1) ) ;
      strcpy(sss,cpt) ;
      n_tok++ ;
      s_tok = (char **) realloc( s_tok , sizeof(char *) * n_tok ) ;
      s_tok[n_tok-1] = sss ;

      cpt = strtok( NULL , " \t\n\r\f\v" ) ;
   }

   *ntok = n_tok ; *stok = s_tok ; return ;
}

static void duplicate_string_list( int nin , char ** sin , char *** sout )
{
   int ii , ll ;
   char ** s_out = NULL ;

   if( sout == NULL ) return ;
   if( nin < 1 || sin == NULL ){ *sout = NULL ; return ; }

   s_out = (char **) malloc( sizeof(char *) * nin ) ;
   for( ii=0 ; ii < nin ; ii++ ){
      ll = strlen(sin[ii]) ;
      s_out[ii] = (char *) malloc( sizeof(char) * (ll+1) );
      strcpy( s_out[ii] , sin[ii] ) ;
   }

   *sout = s_out ; return ;
}

static void free_string_list( int nin , char ** sin )
{
   int ii ;
   if( sin == NULL ) return ;
   for( ii=0 ; ii < nin ; ii++ ) if( sin[ii] != NULL ) free(sin[ii]) ;
   free(sin) ; return ;
}

static void appendto_string_list( int *nfirst , char *** sfirst ,
                                  int nsecond , char ** ssecond  )
{
   int     nf=*nfirst , ii , ll ;
   char ** sf ;

   if( nsecond < 1 || ssecond == NULL ) return ;  /* nothing to do */

   if( *sfirst == NULL || nf == 0 )
      sf = (char **) malloc( sizeof(char *) * nsecond ) ;
   else
      sf = (char **) realloc( *sfirst , sizeof(char *)*(nf+nsecond) ) ;

   for( ii=0 ; ii < nsecond ; ii++ ){
      ll = strlen(ssecond[ii]) ;
      sf[nf+ii] = (char *) malloc( sizeof(char) * (ll+1) ) ;
      strcpy( sf[nf+ii] , ssecond[ii] ) ;
   }

   *nfirst = nf+nsecond ;
   *sfirst = sf         ; return ;
}

/*----------------------------------------------------------------------------
   18 Nov 1999: Take the input string (sin) and put its pieces at the
                front of the arg list, just after argv[0].
                If *new_argv is returned as NULL, then there are no new args.
------------------------------------------------------------------------------*/

void prepend_string_to_args( char * sin ,
                             int argc , char * argv[] ,
                             int * new_argc , char *** new_argv )
{
   int     n_argc , ii , ntok=0    ;
   char ** n_argv ,   ** stok=NULL ;
   char * cpt , * sss ;

   if( new_argc == NULL || new_argv == NULL ) return ;              /* error */

   if( sin == NULL || sin[0] == '\0' ){ *new_argv = NULL; return; } /* do nothing */

   /*-- if no inputs after argv[0], prepend and append are identical --*/

   if( argc < 2 ){
      append_string_to_args( sin , argc , argv , new_argc , new_argv ) ;
      return ;
   }

   /*-- OK, must do it my way --*/

   tokenize_string( sin , &ntok , &stok ) ;
   if( stok == NULL || ntok < 1 ){ *new_argv = NULL; return; }      /* do nothing */

   /* copy first input arg to output */

   duplicate_string_list( 1 , argv , &n_argv ) ;
   n_argc = 1 ;

   /* append token list to output */

   appendto_string_list( &n_argc , &n_argv , ntok , stok ) ;
   free_string_list( ntok , stok ) ;

   /* append rest of input args to output */

   appendto_string_list( &n_argc , &n_argv , argc-1 , argv+1 ) ;

   *new_argc = n_argc ;  /* the results! */
   *new_argv = n_argv ;
   return ;
}

/*----------------------------------------------------------------------------
   18 Nov 1999: Take the input string (sin) and append its pieces to
                the existing command line arguments.
                If *new_argv is returned as NULL, then there are no new args.
------------------------------------------------------------------------------*/

void append_string_to_args( char * sin ,
                            int argc , char * argv[] ,
                            int * new_argc , char *** new_argv )
{
   int     n_argc , ii , ntok=0    ;
   char ** n_argv ,   ** stok=NULL ;
   char * cpt , * sss ;

   if( new_argc == NULL || new_argv == NULL ) return ;              /* error */

   if( sin == NULL || sin[0] == '\0' ){ *new_argv = NULL; return; } /* do nothing */

   tokenize_string( sin , &ntok , &stok ) ;
   if( stok == NULL || ntok < 1 ){ *new_argv = NULL; return; }      /* do nothing */

   /* copy input args to output */

   if( argc > 0 ){
      duplicate_string_list( argc , argv , &n_argv ) ;
      n_argc = argc ;
   } else {                                                    /* shouldn't happen */
      n_argv = NULL ;
      n_argc = 0 ;
   }

   /* append token list to output */

   appendto_string_list( &n_argc , &n_argv , ntok , stok ) ;
   free_string_list( ntok , stok ) ;

   *new_argc = n_argc ;  /* the results! */
   *new_argv = n_argv ;
   return ;
}

/*-------------------------------------------------------------------------------
   Copy stdin to the command line arguments if the last argument on the
   command line is '-@'.
   18 Nov 1999: modified to put the actual args mangling in the routine above.
---------------------------------------------------------------------------------*/

void addto_args( int argc , char * argv[] , int * new_argc , char *** new_argv )
{
   int  ii , nsin , nall ;
   char lbuf[4096] ;
   char * sin , * cpt ;

   /*-- sanity checks --*/

   if( new_argc == NULL || new_argv == NULL ) return ;

   if( strcmp(argv[argc-1],"-@") != 0 ){ *new_argv = NULL; return; } /* do nothing */

   /* suck the standard input in */

   nall = BLEN ;
   sin  = (char *) malloc( sizeof(char) * nall ) ;  /* will hold stdin */
   nsin = 0 ;

   sin[0] = '\0';  /* 'terminate' this empty string    18 Apr 2006 [rickr] */

   do{
      cpt = afni_fgets( lbuf , BLEN , stdin ) ; /* read line */
      if( cpt == NULL ) break ;            /* end of file */
      ii = strlen(lbuf) ;
      if( ii+nsin >= nall-4 ){             /* make more sin space */
         nall += BLEN ;
         sin   = (char *) realloc( sin , sizeof(char) * nall ) ;
      }
      strcat(sin,lbuf) ; nsin = strlen(sin) ;  /* add to sin array */
   } while(1) ;

   if( nsin == 0 ){ *new_argv = NULL; free(sin); return; }  /* nothing was read */

   append_string_to_args( sin , argc-1 , argv , new_argc , new_argv ) ;  /* real work */

   free(sin) ; return ;
}
