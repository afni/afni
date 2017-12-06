#include "mrilib.h"
#include <unistd.h>
#include <stdlib.h>
#include <time.h>

#define NREP 2
#define NBUF 12345
static unsigned char buf[NBUF] ;

int main( int argc , char * argv[] )
{
   int iarg , ii , verb=1 , ibot , irep,nrep=NREP , ng=0 , keep=0 ;
   long long ll , jj ;
   FILE * fp ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: rmz [-q] [-#] [-k] filename ...\n"
             " -- Zeros out files before removing them\n") ;
      exit(0) ;
   }

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-q") == 0 ){
        verb = 0 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-k") == 0 ){
        keep = 1 ; iarg++ ; continue ;
      }

      irep = strtol( argv[iarg] , NULL , 10 ) ;
      if( irep <= 0 ){
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
   for( irep=0 ; irep <= nrep ; irep++ ){

      jj = lrand48() % 217 ;

      if( irep < nrep || nrep == 0 ){
        for( ii=0 ; ii < NBUF ; ii++ ){
          buf[ii] = ((3+2*irep)*ii+jj) % 255 ;
          if( ii%257==256 ) jj += lrand48() % 7 ;
        }
      } else {
        for( ii=0 ; ii < NBUF ; ii++ ) buf[ii] = 0 ;  /* final loop ==> zero */
      }

      for( iarg=ibot ; iarg < argc ; iarg++ ){
         ii = THD_is_directory( argv[iarg] ) ;
         if( ii != 0 && irep == 0 ){
           fprintf(stderr," ** Can't erase a directory: %s\n",argv[iarg]) ;
           continue ;
         }

         ll = THD_filesize( argv[iarg] ) ;
         if( ll >= 0 && THD_is_file(argv[iarg]) ){
            fp = fopen( argv[iarg], "r+" ) ;
            if( fp != NULL ){
               for( jj=0 ; jj < ll ; jj += NBUF )
                  fwrite( buf, 1, NBUF, fp ) ;
               fflush(fp) ; fsync(fileno(fp)) ; fclose(fp) ;
               if( !keep && irep == nrep ){
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
      sync() ; fprintf(stderr,"++ End pass %d\n",irep+1); sleep(1) ;

      if( ng == 0 ) break ;  /* none were 'good' */
   }

   exit(0) ;
}
