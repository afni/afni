#include "mrilib.h"

#define NBUF 16384

int main( int argc , char * argv[] )
{
   char buf[NBUF] ;
   int nn ; double ee,pp , cutper=0.0 ;

   if( argc > 1 ){
      if( strcmp(argv[1],"-help") == 0 ){
         printf("Usage: ent16 [-%%nn]\n"
                "Computes an estimate of the entropy of stdin.\n"
                "If the flag '-%%75' is given (e.g.), then the\n"
                "  exit status is 1 only if the input could be\n"
                "  compressed at least 75%%, otherwise the exit\n"
                "  status is 0.  Legal values of 'nn' are 1..99.\n"
                "In any case, the entropy and compression estimates\n"
                "  are printed to stdout, even if no '-%%nn' flag is.\n"
                "  given.\n"
                "\n"
                "METHOD: entropy is estimated by building a histogram\n"
                "        of all 16 bit words in the input, then summing\n"
                "        over -p[i]*log2(p[i]), i=0..65535.  Compression\n"
                "        estimate seems to work pretty good for gzip -1\n"
                "        in most cases of binary image data.\n"
                "\n"
                "SAMPLE USAGE (csh syntax):\n"
                "  ent16 -%%75 < fred+orig.BRIK\n"
                "  if( $status == 1 ) gzip -1v fred+orig.BRIK\n"
               ) ;
          exit(0) ;

      } else if( strncmp(argv[1],"-%",2) == 0 ){
         cutper = strtod( argv[1]+2 , NULL ) ;
         if( cutper < 1.0 || cutper > 99.0 ) cutper = 0.0 ;

      } else {
         fprintf(stderr,"++ ent16: Unknown option %s ignored!\n",argv[1]) ;
      }
   }
   
   /*--------------------------*/
   
   ENTROPY_setup() ;
   do{
      nn = fread( buf , 1 , NBUF , stdin ) ;
      if( nn <= 0 ) break ;
      ENTROPY_accumulate( nn , buf ) ;
   } while(1) ;
   
   /*--------------------------*/

   ee = ENTROPY_compute() ; ENTROPY_setdown() ;
   pp = 100.0*(1.0-ee/16.0) ;

   printf("entropy=%6.3f bits/word  compression=%5.2f%%\n",
          ee , pp ) ;

   if( cutper > 0.0 && pp >= cutper ) exit(1) ;
   exit(0) ;
}
