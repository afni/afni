#include "niml.h"

/*--- Copy stdin to a NI_stream ---*/

#define NBUF 39000

int main( int argc , char *argv[] )
{
   NI_stream ns ;
   char lbuf[NBUF] , *bbb ;
   int nn , nl=0;

   if( argc < 2 ){
      fprintf(stderr,"Usage: nicat [-r] streamspec\n");exit(0);
   }

   /** write stdin to the stream **/

   if( strcmp(argv[1],"-r") != 0 ){
     ns = NI_stream_open( argv[1] , "w" ) ;
     if( ns == NULL ){
        fprintf(stderr,"NI_stream_open fails\n") ; exit(1) ;
     }
     while(1){
       nn = NI_stream_writecheck( ns , 400 ) ;
       if( nn == 1 ){ fprintf(stderr,"!\n") ; break ; }
       if( nn <  0 ){ fprintf(stderr,"BAD\n"); exit(1) ; }
       fprintf(stderr,".") ;
     }
     while(1){
       bbb = fgets( lbuf , NBUF , stdin ) ; if( bbb == NULL ) break ;
#if 0
       do{ nn = NI_stream_writecheck(ns,1) ; } while(nn==0) ;
       if( nn == -1 ){ fprintf(stderr,"NI_stream_writecheck fails\n");break;}
#endif
       nn = NI_stream_write( ns , lbuf , strlen(lbuf) ) ;
       if( nn < 0 ){
          fprintf(stderr,"NI_stream_write fails\n"); break ;
       } else if( nn < strlen(lbuf) ){
          fprintf(stderr,"nl=%d: wrote %d/%d bytes\n",nl,nn,strlen(lbuf)) ;
       }
       nl++ ;
     }
     fprintf(stderr,"Wrote %d lines of text to %s\n",nl,argv[1]) ;
     sleep(1) ; exit(0) ;
   }

   /** write the stream to stdout */

   if( argc < 3 ){ fprintf(stderr,"-r needs argv[2]\n"); exit(1); }

   ns = NI_stream_open( argv[2], "r" ) ;
   if( ns == NULL ){
      fprintf(stderr,"NI_stream_open fails\n") ; exit(1) ;
   }
   while(1){
     nn = NI_stream_readcheck( ns , 400 ) ;
     if( nn == 1 ){ fprintf(stderr,"!\n") ; break ; }
     if( nn <  0 ){ fprintf(stderr,"BAD\n"); exit(1) ; }
     fprintf(stderr,".") ;
   }
   while(1){
      nn = NI_stream_read( ns , lbuf , NBUF ) ;
      if( nn < 0 ){
         fprintf(stderr,"\nNI_stream_read fails\n"); break;
      }
      if( nn > 0 ){
         printf("%.*s",nn,lbuf) ; nl++ ;
      }
   }
   fprintf(stderr,"\nDid %d NI_stream_read()s\n",nl) ;
   sleep(1) ; exit(0) ;
}
