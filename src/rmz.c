#include "mrilib.h"
#include <unistd.h>

#define NREP 7
#define NBUF 4096
static unsigned char buf[NBUF] ;

int main( int argc , char * argv[] )
{
   int iarg , ii , ll , jj , nw , verb=1 , ibot , irep ;
   FILE * fp ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: rmz [-q] filename ...\n"
             " -- Zeros out files before removing them\n") ;
      exit(0) ;
   }

   iarg = 1 ;
   if( strcmp(argv[iarg],"-q") == 0 ){ verb = 0 ; iarg++ ; }

   ibot = iarg ;
   for( irep=0 ; irep < NREP ; irep++ ){

      for( ii=0 ; ii < NBUF ; ii++ ) buf[ii] = ((3+2*irep)*ii+irep) % 255 ;

      for( iarg=ibot ; iarg < argc ; iarg++ ){
         ii = THD_is_directory( argv[iarg] ) ;
         if( ii != 0 && irep == 0 ){
            fprintf(stderr," ** Can't erase a directory file: %s\n",argv[iarg]) ;
            continue ;
         }

         ll = THD_filesize( argv[iarg] ) ;
         if( ll >= 0 && THD_is_file(argv[iarg]) ){
            fp = fopen( argv[iarg], "w" ) ;
            if( fp != NULL ){
               for( jj=0 ; jj < ll ; jj += NBUF ){
                  nw = MIN(ll-jj,NBUF) ; fwrite( buf, 1, nw, fp ) ;
               }
               fflush(fp) ; fsync(fileno(fp)) ; fclose(fp) ;
               if( irep == (NREP-1) ){
                  unlink(argv[iarg]) ;
                  if( verb ) fprintf(stderr," -- Removed file %s\n",argv[iarg]) ;
               }
            } else {
               if( irep == 0 )
                  fprintf(stderr," ** Can't write to file %s\n",argv[iarg]) ;
            }
         } else {
            if( irep == 0 )
               fprintf(stderr," ** Can't access file %s\n",argv[iarg]) ;
         }
      }
      sync() ;
   }

   exit(0) ;
}
