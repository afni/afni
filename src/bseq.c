#include "mrilib.h"

#define NMAX 48
static char * srle[NMAX+1] = {
     NULL ,
     NULL ,
     NULL ,
     "21" ,
     "211" ,
     "311" ,
     "1113" ,
     "1123" ,
     "12113" ,
     "42111" ,
     "22114" ,
     "112133" ,
     "1221114" ,
     "5221111" ,
     "2221115" ,
     "52221111" ,
     "225111121" ,
     "252211121" ,
     "441112221" ,
     "4111142212" ,
     "5113112321" ,
     "27221111121" ,
     "51221111233" ,
     "212121111632" ,
     "2236111112121" ,
     "337111121221" ,
     "21212111116322" ,
     "34313131211211" ,
     "34313131211212" ,
     "212112131313431" ,
     "551212111113231" ,
     "7332212211112111" ,
     "71112111133221221" ,
     "742112111111122221" ,
     "842112111111122221" ,
     "7122122111121111332" ,
     "3632311131212111211" ,
     "844211211111122221" ,
     "8442112111111122221" ,
     "82121121234321111111" ,
     "44412112131121313131" ,
     "343111111222281211211" ,
     "313131341343112112112" ,
     "1132432111117212112213" ,
     "525313113111222111211121" ,
     "82121121231234321111111" ,
     "823431231211212211111111" ,
     "923431231211212211111111" ,
     "3111111832143212221121121"
} ;

typedef struct {
  int num ;
  int * ss ;
} SEQ ;

SEQ * expand_rle( char * str )
{
   SEQ * seq ;
   int * ss , ii , jj , vv ;
   char * cpt ;

   if( str == NULL ) return NULL ;
   for( ii=0,cpt=str ; *cpt != '\0' ; cpt++ )
      ii += (*cpt - '0') ;

   seq = (SEQ *) malloc( sizeof(SEQ) ) ;
   seq->ss = ss = (int *) malloc( sizeof(int) * ii ) ;
   seq->num = ii ;

   for( vv=1,ii=0,cpt=str ; *cpt != '\0' ; cpt++ ){
      jj = (*cpt - '0') ;
      for( jj=0 ; jj < (*cpt - '0') ; jj++ ) ss[ii++] = vv ;
      vv = -vv ;
   }

   return seq ;
}

int main( int argc , char * argv[] )
{
   int ir , ii , num , sum , jj ;
   SEQ * seq ;
   int * ss ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ) exit(0) ;

   ir = strtol( argv[1] , NULL , 10 ) ; if( ir < 3 ) exit(1) ;

   seq = expand_rle( srle[ir] ) ;

   printf("num = %d\n",seq->num) ;
   for( ii=0 ; ii < seq->num ; ii++ )
      printf(" %d",seq->ss[ii]) ;
   printf("\n") ;

   num = seq->num ; ss = seq->ss ;
   for( ii=0 ; ii < num-1 ; ii++ ){
      sum = 0 ;
      for( jj=0 ; jj < num-ii ; jj++ )
         sum += ss[jj] * ss[jj+ii] ;
      printf(" %d",sum) ;
   }
   printf("\n") ;

   exit(0) ;
}
