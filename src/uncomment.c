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
   int nin , comm , ii , quote , iarg=1 , gitskip=0 , inskip=0 ;

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
      printf("EXAMPLE: using on C code annotated by git blame\n"
             "  git blame afni.c | uncomment -gitskip - | grep -v -e '[0-9]) *$' -e '^$' -e '^ *$' | head\n"
             "See the gitsum.csh script for how this '-gitskip' option can be used to add\n"
             "up how many lines of non-comment C code are tagged to various co-conspirators.\n" ) ;
      printf("\n") ;
      exit(0) ;
   }

   /*- the only option -*/

   if( strcasecmp(argv[iarg],"-gitskip") == 0 ){
     iarg++ ; gitskip = 1 ;
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

   inskip = gitskip ;

   while( ii < nin ){  /* scan each character */

      /* if end of line */

      if( infile[ii] == '\n' ){
        putchar(infile[ii++]) ; inskip = gitskip ; continue ; /* reset line pos */
      }

      /* if in the skip zone for this line = before first "<digit>)" sequence */

      if( inskip ){
        putchar(infile[ii]) ;
        if( infile[ii] == ')' && isdigit(infile[ii-1]) ) inskip = 0 ;
        ii++ ; continue ;
      }

      /* if inside a quote, and see an end quote */

      if( quote && infile[ii] == '"' && infile[ii-1] != '\\' ){
         putchar(infile[ii++]) ; quote = 0 ; continue ;
      }

      /* if not inside a quote or comment, and see a start quote */

      if( !quote && !comm && infile[ii] == '"' && infile[ii-1] != '\\' ){
         putchar(infile[ii++]) ; quote = 1 ; continue ;
      }

      /* if inside a comment and see an end comment */

      if( comm && infile[ii] == '*' && infile[ii+1] == '/' ){
         ii += 2 ; comm = 0 ; continue ;
      }

      /* if not inside a quote or comment, and see a start comment */

      if( !quote && !comm && infile[ii] == '/' && infile[ii+1] == '*' ){
         putchar(' ') ; ii += 2 ; comm = 1 ; continue ;
      }

      /* normal case: output character if not inside a comment: */

      if( !comm ) putchar(infile[ii]) ;
      ii++ ;
   }
   if( infile[ii-1] != '\n' ) putchar('\n') ;

        if( comm  ) WARNING_message("ended inside a comment!") ;
   else if( quote ) WARNING_message("ended inside a quote!") ;
   exit(0) ;
}
