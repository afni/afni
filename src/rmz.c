#include "mrilib.h"
#include <unistd.h>

#define NBUF 4096
static unsigned char buf1[NBUF] ;
static unsigned char buf2[NBUF] ;

int main( int argc , char * argv[] )
{
   int iarg , ii , ll , jj , nw , verb=1 ;
   FILE * fp ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: rmz [-q] filename ...\n"
             " -- Zeros out files before removing them\n") ;
      exit(0) ;
   }

   iarg = 1 ;
   if( strcmp(argv[iarg],"-q") == 0 ){ verb = 0 ; iarg++ ; }

   for( ii=0 ; ii < NBUF ; ii++ ) buf1[ii] = (7*ii)    % 255 ;
   for( ii=0 ; ii < NBUF ; ii++ ) buf2[ii] = (11*ii+3) %  61 ;

   for( ; iarg < argc ; iarg++ ){
      ii = THD_is_directory( argv[iarg] ) ;
      if( ii != 0 ){
         fprintf(stderr," ** Can't erase a directory file: %s\n",argv[iarg]) ;
         continue ;
      }

      ll = THD_filesize( argv[iarg] ) ;
      if( ll >= 0 && THD_is_file(argv[iarg]) ){
         fp = fopen( argv[iarg], "w" ) ;
         if( fp != NULL ){
            for( jj=0 ; jj < ll ; jj += NBUF ){
               nw = MIN(ll-jj,NBUF) ; fwrite( buf1, 1, nw, fp ) ;
            }
            fsync(fileno(fp)) ;
            for( jj=0 ; jj < ll ; jj += NBUF ){
               nw = MIN(ll-jj,NBUF) ; fwrite( buf2, 1, nw, fp ) ;
            }
            fsync(fileno(fp)) ;
            fclose(fp) ; unlink(argv[iarg]) ;
            if( verb ) fprintf(stderr," -- Removed file %s\n",argv[iarg]) ;
         } else {
            fprintf(stderr," ** Can't write to file %s\n",argv[iarg]) ;
         }
      } else {
         fprintf(stderr," ** Can't access file %s\n",argv[iarg]) ;
      }
   }

   sync() ; exit(0) ;
}
