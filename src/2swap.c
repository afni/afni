/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include <stdio.h>
#include <unistd.h>

typedef struct { unsigned char a,b ; } twobytes ;

#define TEMP_FILE "Elvis.Lives"
#define BUFSIZE   16000

static short buf[BUFSIZE] ;

int main( argc , argv )
   int argc ;
   char * argv[] ;
{
   FILE * infil , * outfil ;
   int narg , nbyte , nsh , quiet = 0 , ndone ;

   if( argc < 2 || strncmp(argv[1],"-help",2) == 0 ){
     printf("Usage: 2swap [-q] file ...\n") ;
     printf("-- Swaps byte pairs on the files listed.\n") ;
     printf("   The -q option means to work quietly.\n") ;
     exit(0) ;
   }

   narg = 1 ;
   if( strncmp(argv[1],"-q",2) == 0 ){ quiet = 1 ; narg++ ; }

   for( ; narg < argc ; narg++ ){
      infil = fopen( argv[narg] , "r" ) ;
      if( infil == NULL ){
         printf("** file %s not found!\n",argv[narg]) ; fflush(stdout) ;
         continue ;
      }

      outfil = fopen( TEMP_FILE , "w" ) ;
      if( outfil == NULL ){
         printf("** Cannot open temporary file!\n") ;
         exit(1) ;
      }

      if( !quiet ){ printf("-- opened %s",argv[narg]) ; fflush(stdout) ; }

      ndone = 0 ;
      do {
         nsh = fread( buf , sizeof(short) , BUFSIZE , infil ) ;
         if( nsh <= 0 ) break ;
         swap2( nsh , buf ) ;
         fwrite( buf , sizeof(short) , nsh , outfil ) ;
         ndone += nsh ;
         if( ndone > 1000000 && ! quiet ){
            ndone -= 1000000 ; printf(".") ; fflush(stdout) ;
         }
      } while( nsh == BUFSIZE ) ;

      fsync(fileno(outfil)) ; fclose( infil ) ; fclose( outfil ) ;

      unlink( argv[narg] ) ;
      rename( TEMP_FILE , argv[narg] ) ;

      if( !quiet ){ printf(".\n") ; fflush(stdout) ; }
   }
   exit(0) ;
}

int swap2( n , ar )
   int n ;
   short * ar ;
{
   register int ii ;
   register twobytes * tb = (twobytes *) ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
      tt       = tb[ii].a ;
      tb[ii].a = tb[ii].b ;
      tb[ii].b = tt ;
   }
   return 0 ;
}
