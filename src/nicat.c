#include "niml.h"

/*--- Copy stdin to a NI_stream ---*/

#define NBUF 65000

int main( int argc , char *argv[] )
{
   NI_stream ns ;
   char lbuf[NBUF] , *reop=NULL ;
   int nn , nl=0 , jj , ntot=0 , ct , ech , iarg=1 ;

   if( argc < 2 ){
      fprintf(stderr,"Usage: nicat [-reopen rr] [-rR] streamspec\n");exit(0);
   }

   if( strcmp(argv[iarg],"-reopen") == 0 ){
     reop = argv[++iarg] ;
     iarg++ ;
   }

   /** write stdin to the stream **/

   if( toupper(argv[iarg][1]) != 'R' ){
     ns = NI_stream_open( argv[iarg] , "w" ) ;
     if( ns == NULL ){
        fprintf(stderr,"NI_stream_open fails\n") ; exit(1) ;
     }
     while(1){
       nn = NI_stream_writecheck( ns , 400 ) ;
       if( nn == 1 ){ fprintf(stderr,"!\n") ; break ; }
       if( nn <  0 ){ fprintf(stderr,"BAD\n"); exit(1) ; }
       fprintf(stderr,"-") ;
     }
     if( reop ){                                 /* 23 Aug 2002 */
       fprintf(stderr,"reopening") ;
       nn = NI_stream_reopen( ns , reop ) ;
       if( nn == 0 ) fprintf(stderr,".BAD") ;
       else          fprintf(stderr,".GOOD") ;
       fprintf(stderr,"\n") ;
     }
     ct = NI_clock_time() ;
     while(1){
       jj = fread(lbuf,1,NBUF,stdin) ; if( jj <= 0 ) break ;
       nn = NI_stream_write( ns , lbuf , jj ) ;
       if( nn < 0 ){
          fprintf(stderr,"NI_stream_write fails\n"); break ;
       } else if( nn < jj ){
          fprintf(stderr,"nl=%d: wrote %d/%d bytes\n",nl,nn,jj) ;
       }
       nl++ ; ntot += jj ;
     }
     ct = NI_clock_time()-ct ;
     fprintf(stderr,"Wrote %d bytes in %d ms: %.3f Mbytes/s\n",
                    ntot,ct,(9.5367e-7*ntot)/(1.e-3*ct)       ) ;
     sleep(1) ; exit(0) ;
   }

   /** write the stream to stdout */

   if( argc < iarg+2 ){ fprintf(stderr,"%s needs argv[%d]\n",argv[1],iarg+1); exit(1); }

   ech = (argv[iarg][1] == 'r') ;

   ns = NI_stream_open( argv[iarg+1], (argv[iarg][2]=='\0') ? "r" : "w" ) ;
   if( ns == NULL ){
      fprintf(stderr,"NI_stream_open fails\n") ; exit(1) ;
   }
   while(1){
     nn = NI_stream_readcheck( ns , 400 ) ;
     if( nn == 1 ){ fprintf(stderr,"!\n") ; break ; }
     if( nn <  0 ){ fprintf(stderr,"BAD\n"); exit(1) ; }
     fprintf(stderr,"+") ;
   }
   if( reop ){                                 /* 23 Aug 2002 */
     fprintf(stderr,"reopening") ;
     nn = NI_stream_reopen( ns , reop ) ;
     if( nn == 0 ) fprintf(stderr,".BAD") ;
     else          fprintf(stderr,".GOOD") ;
     fprintf(stderr,"\n") ;
   }
   ct = NI_clock_time() ;
   while(1){
      nn = NI_stream_read( ns , lbuf , NBUF ) ;
      if( nn < 0 ){
         fprintf(stderr,"\nNI_stream_read fails\n"); break;
      } else {
         ntot += nn ;
      }
      if( ech && nn > 0 ){
         printf("%.*s",nn,lbuf) ; nl++ ;
      }
   }
   ct = NI_clock_time()-ct ;
   fprintf(stderr,"Read %d bytes in %d ms: %.3f Mbytes/s\n",
                   ntot,ct,(9.5367e-7*ntot)/(1.e-3*ct)       ) ;
   sleep(1) ; exit(0) ;
}
