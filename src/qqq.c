#include "mrilib.h"

int main( int argc , char * argv[] )
{
   float * fp , * gp ;
   short * sp , * tp ;
   byte  * bp , * cp ;
   int nn , ii , ityp , dtyp , krep,kk ;
   double ct ;

   if( argc < 5 ){printf("Usage: qqq N {ntl} {float,short,byte} Krep\n");exit(0);}

   nn = strtol(argv[1],NULL,10) ;
   if( nn < 100 ) exit(1) ;

   switch( argv[2][0] ){
      default: exit(1) ;
      case 'n': ityp = 0 ; break ;
      case 't': ityp = 1 ; break ;
      case 'l': ityp = 2 ; break ;
   }

   switch( argv[3][0] ){
      default: exit(1) ;
      case 'f': dtyp = MRI_float ; fp = (float *)malloc(sizeof(float)*nn) ;
                                   gp = (float *)malloc(sizeof(float)*nn) ; break ;
      case 's': dtyp = MRI_short ; sp = (short *)malloc(sizeof(short)*nn) ;
                                   tp = (short *)malloc(sizeof(short)*nn) ; break ;
      case 'b': dtyp = MRI_byte  ; bp = (byte  *)malloc(sizeof(byte )*nn) ;
                                   cp = (byte  *)malloc(sizeof(byte )*nn) ; break ;
   }

   krep = strtol(argv[4],NULL,10) ;
   if( krep < 1 ) exit(1) ;

   switch( dtyp ){
      case MRI_float:
        for( ii=0 ; ii < nn ; ii++ ) fp[ii] = ii * cos((double)ii) ;
      break ;

      case MRI_short:
        for( ii=0 ; ii < nn ; ii++ ) sp[ii] = (short)(ii * cos((double)ii)) ;
      break ;

      case MRI_byte:
        for( ii=0 ; ii < nn ; ii++ ) bp[ii] = (byte)(7*ii+3) ;
      break ;
   }

   ct = COX_cpu_time() ;
   switch( ityp ){
      case 0:
         for( kk=0 ; kk < krep ; kk++ ){
            switch(dtyp){
               case MRI_float: memcpy( gp,fp+kk , sizeof(float)*(nn-kk)) ; break ;
               case MRI_short: memcpy( tp,sp+kk , sizeof(short)*(nn-kk)) ; break ;
               case MRI_byte:  memcpy( cp,bp+kk , sizeof(byte )*(nn-kk)) ; break ;
            }
         }
      break ;

      case 1:
         for( kk=0 ; kk < krep ; kk++ ){
            switch(dtyp){
               case MRI_float:
                  for( ii=1 ; ii < nn ; ii++ ) gp[ii] = 0.5*(fp[ii-1]+fp[ii]) ;
               break ;
               case MRI_short:
                  for( ii=1 ; ii < nn-2 ; ii+=2 ){
                     tp[ii  ] = (sp[ii-1]+sp[ii  ]) >> 1 ;
                     tp[ii+1] = (sp[ii  ]+sp[ii+1]) >> 1 ;
                  }
               break ;
               case MRI_byte:
                  for( ii=1 ; ii < nn ; ii++ ) cp[ii] = (bp[ii-1]+bp[ii]) >> 1 ;
               break ;
            }
        }
     break ;

     case 2:
         for( kk=0 ; kk < krep ; kk++ ){
            switch(dtyp){
               case MRI_float:
                  for( ii=1 ; ii < nn ; ii++ ) gp[ii] = 0.387*fp[ii-1]+0.613*fp[ii] ;
               break ;
               case MRI_short:
                  for( ii=1 ; ii < nn ; ii++ ) tp[ii] = (short)(0.387*sp[ii-1]+0.613*sp[ii]);
               break ;
               case MRI_byte:
                  for( ii=1 ; ii < nn ; ii++ ) cp[ii] = (byte)(0.387*bp[ii-1]+0.613*bp[ii]) ;
               break ;
            }
        }
     break ;
   }
   ct = COX_cpu_time()-ct ;
   printf("cpu time = %g\n",ct) ;
   exit(0);
}
