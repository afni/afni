#include "mrilib.h"
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#define NREP 2
#define NBUF 4096
static unsigned char buf[NBUF] ;

int main( int argc , char * argv[] )
{
   int iarg , ii,ll,jj , nw , verb=1 , ibot , irep,nrep=NREP , ng=0 ;
   FILE * fp ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: rmz [-q] [-#] filename ...\n"
             " -- Zeros out files before removing them\n") ;
      exit(0) ;
   }

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-q") == 0 ){
         verb = 0 ; iarg++ ; continue ;
      }

      irep = strtol( argv[iarg] , NULL , 10 ) ;
      if( irep < 0 ){
         nrep = -irep ; iarg++ ; continue ;
      } else {
         fprintf(stderr,"*** Unknown option: %s\n",argv[iarg]) ; exit(1) ;
      }

   }

   if( iarg >= argc ){
      fprintf(stderr,"*** No files to delete?\n") ; exit(1) ;
   }

   srand48((long)time(NULL)) ;

   ibot = iarg ;
   for( irep=0 ; irep < nrep ; irep++ ){

      jj = lrand48() % 7 ;

      for( ii=0 ; ii < NBUF ; ii++ ) buf[ii] = ((3+2*irep)*ii+jj) % 255 ;

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
               if( irep == (nrep-1) ){
                  unlink(argv[iarg]) ;
                  if( verb ) fprintf(stderr," -- Removed file %s\n",argv[iarg]) ;
               } else if( irep == 0 ) ng++ ;
            } else {
               if( irep == 0 )
                  fprintf(stderr," ** Can't write to file %s\n",argv[iarg]) ;
            }
         } else {
            if( irep == 0 )
               fprintf(stderr," ** Can't access file %s\n",argv[iarg]) ;
         }
      }
      sync() ; if( irep < nrep-1 ) sleep(1) ;
      if( ng == 0 ) break ;  /* none were 'good' */
   }

   exit(0) ;
}
