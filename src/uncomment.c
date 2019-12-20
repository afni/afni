#include "mrilib.h"

#ifdef putchar
#undef putchar
#endif
#define putchar(x) fputc((x),stdout)

/*----------------------------------------------------------------------------*/

#define DBUF   8192
#define MAXBUF 16777216  /* 16 Mbytes */
#define NEX    4

char * suck_stdin(void)
{
   int  ii , nbuf , fd = fileno(stdin) ;
   char *buf ;

   nbuf  = 0 ;
   buf   = (char *)malloc( sizeof(char) * (DBUF+NEX) ) ;

   while( nbuf < MAXBUF ){
     ii = read( fd , buf+nbuf , DBUF ) ;      /* try to read up to DBUF bytes */
     if( ii <= 0 ) break ;                                     /* read failed */
     nbuf += ii ;                        /* add in how many bytes we now have */
     buf = (char *)realloc( buf, sizeof(char)*(nbuf+DBUF+NEX) ) ;   /* resize */
   }

   buf       = (char *)realloc( buf , sizeof(char)*(nbuf+NEX) ) ;
   buf[nbuf] = '\0' ; /* terminate string */
   return buf ;
}

/*----------------------------------------------------------------------------*/

char * suck_file( char *fname )
{
   int len , fd , ii ;
   char *buf ;

   if( fname == NULL || fname[0] == '\0' ) return NULL ;

   if( strcmp(fname,"-") == 0 || strncmp(fname,"stdin",5) == 0 )
     return suck_stdin() ;

   len = THD_filesize( fname ) ;
   if( len <= 0 ) return NULL ;

   fd = open( fname , O_RDONLY ) ;
   if( fd < 0 ) return NULL ;

#if 0
INFO_message("THD_filesize = %d",len) ;
#endif

   buf = (char *) malloc( sizeof(char) * (len+NEX) ) ;
   ii  = read( fd , buf , len ) ;
   close( fd ) ;
   if( ii <= 0 ){ free(buf) ; return NULL; }

   for( ii=0 ; ii < NEX ; ii++ ) buf[len+ii] = '\0' ;
   return buf ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   char * infile ;
   int nin , comm , ii , quote , lpos=0 , iarg=1 , nskip=0 ;

   /*- help? -*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("\n") ;
      printf("Usage: uncomment infile.c > outfile.c\n") ;
      printf("       If 'infile' is '-', reads from stdin.\n") ;
      printf("\n") ;
      printf("Removes comments from C source code.\n") ;
      printf("Always writes to stdout.\n") ;
      printf("Might want to pipe through\n"
             "  grep -v -e '^ *$' -e '^$'\n"
             "to remove any lines that are all blanks or purely empty.\n") ;
      printf("EXAMPLE:\n"
             "  wc -l afni.c                                          => 13836\n"
             "  uncomment afni.c | grep -v -e '^ *$' -e '^$' | wc -l  =>  9621\n" ) ;
      printf("\n") ;
#if 0
      printf("EXAMPLE:\n"
             "  git blame afni.c | uncomment -skip 58 - | \\\n"
             "    grep -v '[0-9]) *$' | grep -i cox | wc -l\n"
             "Counts the number of actual lines of code in\n"
             "afni.c that are credited to author 'cox'.\n"
             "The same thing without the un-commenting:\n"
             "  git blame afni.c | grep -v '[0-9]) *$' | grep -i cox | wc -l\n"
             "At this moment, the two answers are 8957 and 10058.\n"
            ) ;
#endif
      exit(0) ;
   }

   /*- the only option -*/

   if( strcasecmp(argv[iarg],"-skip") == 0 ){
     iarg++ ; if( iarg >= argc ) ERROR_exit("need arg after %s",argv[iarg-1]) ;
     nskip = (int)strtod(argv[iarg],NULL) ;
     if( nskip < 0 ) nskip = 0 ;
     iarg++ ;
   }
   if( iarg >= argc ) ERROR_exit("need a filename to read") ;

   /*- read entire input -*/

   infile = suck_file( argv[iarg] ) ;
   if( infile == NULL || *infile == '\0' )
     ERROR_exit("Can't get data from file %s",argv[iarg]) ;

   nin   = strlen(infile) ;
   comm  = 0 ;
   quote = 0 ;
   ii    = 0 ;
   lpos  = 0 ;

   while( ii < nin ){  /* scan each character */

      /* if end of line */

      if( infile[ii] == '\n' ){
        putchar(infile[ii++]) ; lpos = 0 ; continue ; /* reset line pos */
      }

      /* if in the skip zone for this line (early lpos) */

      if( lpos < nskip ){
        putchar(infile[ii++]) ; lpos++ ; continue ;
      }

      /* if inside a quote, and see an end quote */

      if( quote && infile[ii] == '"' && infile[ii-1] != '\\' ){
         putchar(infile[ii++]) ; quote = 0 ; lpos++ ; continue ;
      }

      /* if not inside a quote or comment, and see a start quote */

      if( !quote && !comm && infile[ii] == '"' && infile[ii-1] != '\\' ){
         putchar(infile[ii++]) ; quote = 1 ; lpos++ ; continue ;
      }

      /* if inside a comment and see an end comment */

      if( comm && infile[ii] == '*' && infile[ii+1] == '/' ){
         ii += 2 ; comm = 0 ; lpos += 2 ; continue ;
      }

      /* if not inside a quote or comment, and see a start comment */

      if( !quote && !comm && infile[ii] == '/' && infile[ii+1] == '*' ){
         putchar(' ') ; ii += 2 ; comm = 1 ; lpos += 2 ; continue ;
      }

      /* normal case: output character if not inside a comment: */

      if( !comm ) putchar(infile[ii]) ;
      if( infile[ii] == '\n' ) lpos = 0 ; else lpos++ ;
      ii++ ;
   }
   if( lpos > 0 ) putchar('\n') ;

        if( comm  ) WARNING_message("ended inside a comment!") ;
   else if( quote ) WARNING_message("ended inside a quote!") ;
   exit(0) ;
}
