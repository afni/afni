#include <stdio.h>
#include <unistd.h>

typedef struct { unsigned char a,b,c,d ; } fourbytes ;

#define TEMP_FILE "Mozart.Lives"
#define BUFSIZE   32000

static int buf[BUFSIZE] ;

int main( argc , argv )
   int argc ;
   char * argv[] ;
{
   FILE * infil , * outfil ;
   int narg , nbyte , nint , quiet = 0 , ndone ;

   if( argc < 2 || strncmp(argv[1],"-help",2) == 0 ){
     printf("Usage: 4swap [-q] file ...\n") ;
     printf("-- Swaps byte quadruples on the files listed.\n") ;
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

      if( !quiet){ printf("-- opened %s",argv[narg]) ; fflush(stdout) ; }

      ndone = 0 ;
      do {
         nint = fread( buf , sizeof(int) , BUFSIZE , infil ) ;
         if( nint <= 0 ) break ;
         swap4( nint , buf ) ;
         fwrite( buf , sizeof(int) , nint , outfil ) ;
         ndone += nint ;
         if( ndone > 1000000 && ! quiet ){
            ndone -= 1000000 ; printf(".") ; fflush(stdout) ;
         }
      } while( nint == BUFSIZE ) ;

      fsync(fileno(outfil)) ; fclose( infil ) ; fclose( outfil ) ;

      unlink( argv[narg] ) ;
      rename( TEMP_FILE , argv[narg] ) ;

      if( !quiet ){ printf(".\n") ; fflush(stdout) ; }
   }
   exit(0) ;
}

int swap4( n , ar )
   int n ;
   int * ar ;
{
   register int ii ;
   register fourbytes * tb = (fourbytes *) ar ;
   register unsigned char tt ;

   for( ii=0 ; ii < n ; ii++ ){
      tt       = tb[ii].a ;
      tb[ii].a = tb[ii].d ;
      tb[ii].d = tt ;
      tt       = tb[ii].b ;
      tb[ii].b = tb[ii].c ;
      tb[ii].c = tt ;
   }
   return 0 ;
}
