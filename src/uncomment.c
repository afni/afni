/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

#ifdef putchar
#undef putchar
#endif
#define putchar(x) fputc((x),stdout)

char * suck_file( char * fname )
{
   int len , fd , ii ;
   char * buf ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   len = THD_filesize( fname ) ;
   if( len <= 0 ) return NULL ;

   fd = open( fname , O_RDONLY ) ;
   if( fd < 0 ) return NULL ;

#if 0
fprintf(stderr,"THD_filesize = %d\n",len) ;
#endif

#define NEX 4

   buf = (char *) malloc( sizeof(char) * (len+NEX) ) ;
   ii  = read( fd , buf , len ) ;
   close( fd ) ;
   if( ii <= 0 ){ free(buf) ; return NULL; }

   for( ii=0 ; ii < NEX ; ii++ ) buf[len+ii] = '\0' ;
   return buf ;
}

int main( int argc , char * argv[] )
{
   char * infile ;
   int nin , comm , ii , quote ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: uncomment infile.c > outfile.c\n") ;
      exit(0) ;
   }

   infile = suck_file( argv[1] ) ;
   if( infile == NULL || strlen(infile) == 0 ){
      fprintf(stderr,"*** Can't read from file %s\n",argv[1]) ;
      exit(1) ;
   }

   nin   = strlen(infile) ;
   comm  = 0 ;
   quote = 0 ;
   ii    = 0 ;

   while( ii < nin ){

      /* if inside a quote, and see an end quote: */

      if( quote && infile[ii] == '"' && infile[ii-1] != '\\' ){
         putchar(infile[ii++]) ; quote = 0 ; continue ;
      }

      /* if not inside a quote or comment, and see a start quote: */

      if( !quote && !comm && infile[ii] == '"' && infile[ii-1] != '\\' ){
         putchar(infile[ii++]) ; quote = 1 ; continue ;
      }

      /* if inside a comment and see an end comment: */

      if( comm && infile[ii] == '*' && infile[ii+1] == '/' ){
         ii += 2 ; comm = 0 ; continue ;
      }

      /* if not inside a quote or comment, and see a start comment: */

      if( !quote && !comm && infile[ii] == '/' && infile[ii+1] == '*' ){
         putchar(' ') ; ii += 2 ; comm = 1 ; continue ;
      }

      /* normal case: output character if not inside a comment: */

#if 0
fprintf(stderr,"at %d/%d: comm=%d char=%d\n",ii,nin,comm,infile[ii]) ;
#endif

      if( !comm ) putchar(infile[ii]) ;
      ii++ ;
   }
   putchar('\n') ;

        if( comm  ) fprintf(stderr,"*** Warning: ended inside a comment!\n") ;
   else if( quote ) fprintf(stderr,"*** Warning: ended inside a quote!\n") ;
   exit(0) ;
}
