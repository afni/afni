#include "thd_shear3d.h"
#include "mrilib.h"

int main( int argc , char *argv[] )
{
   float th1=0.0, th2=0.0, th3=0.0 , thx,thy,thz ;
   int   iarg=0 ;
   MCW_3shear shr ;
   double ntop , nnow ;

   if( argc == 2 && strcmp(argv[1],"-help") == 0 ){
      printf("Usage: testshear [a b c]\n") ; exit(0) ;
   }

   if( argc > 2 ){
      th1 = (PI/180.0) * strtod( argv[++iarg] , NULL ) ;
      th2 = (PI/180.0) * strtod( argv[++iarg] , NULL ) ;
      th3 = (PI/180.0) * strtod( argv[++iarg] , NULL ) ;
      shr = rot_to_shear( 0,th1 , 1,th2 , 2,th3 , 0,0,0,0 , 1.0,1.0,1.0 ) ;
      DUMP_3SHEAR("dump",shr) ;
      nnow = norm_3shear(shr) ; 
      printf("Norm=%g thx=%6.1f thy=%6.1f thz=%6.1f\n",
              nnow, th1*(180.0/PI) , th2*(180.0/PI) , th3*(180.0/PI) );
      exit(0) ;
   }

   ntop = 0.0 ;
   for( thx=1.0 ; thx < 180.0 ; thx++ ){
      th1 = (PI/180.0) * thx ;
      for( thy=1.0 ; thy < 180.0 ; thy++ ){
         th2 = (PI/180.0) * thy ;
         for( thz=1.0 ; thz < 180.0 ; thz++ ){
            th3 = (PI/180.0) * thz ;
            shr = rot_to_shear( 0,th1, 1,th2, 2,th3, 0,0,0,0, 1.0,1.0,1.0 ) ;
            nnow = norm_3shear( shr ) ;
            if( nnow > ntop ){
               ntop = nnow ;
               printf("Norm=%7.4f at thx=%6.1f thy=%6.1f thz=%6.1f\n",
                       ntop , thx,thy,thz ) ;
            }
         }
      }
   }
   exit(0) ;
}
