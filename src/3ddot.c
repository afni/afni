#include "3ddata.h"
#include <string.h>

double DSET_cor( THD_3dim_dataset * xset , THD_3dim_dataset * yset ) ;

int main( int argc , char * argv[] )
{
   double dxy ;
   THD_3dim_dataset * xset , * yset ;

   /*-- read command line arguments --*/

   if( argc < 3 || strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: 3ddot dset1 dset2\n"
             "Output = correlation coefficient between 2 datasets\n") ;
      exit(0) ;
   }

   xset = THD_open_one_dataset( argv[1] ) ;
   yset = THD_open_one_dataset( argv[2] ) ;
   if( xset == NULL || yset == NULL ){
      fprintf(stderr,"cannot open both datasets!\n") ; exit(1) ;
   }
   if( DSET_NUM_TIMES(xset) > 1 || DSET_NUM_TIMES(yset) > 1 ){
      fprintf(stderr,"cannot use time-dependent datasets!\n") ; exit(1) ;
   }

   dxy = DSET_cor( xset , yset ) ;
   printf("%s *DOT* %s = %g\n" , argv[1] , argv[2] , dxy ) ;
   exit(0) ;
}

double DSET_cor( THD_3dim_dataset * xset , THD_3dim_dataset * yset )
{
   double sumxx , sumyy , sumxy , tx,ty , dxy ;
   void  *  xar , *  yar ;
   float * fxar , * fyar ;
   int ii , nxyz , ivx,ivy , itypx,itypy , fxar_new,fyar_new ;

   nxyz =  xset->daxes->nxx
         * xset->daxes->nyy * xset->daxes->nzz ;

   if(  yset->daxes->nxx
      * yset->daxes->nyy * yset->daxes->nzz  != nxyz ) return 0.0 ;

   THD_load_datablock( xset->dblk , NULL ) ;
   ivx   = DSET_PRINCIPAL_VALUE(xset) ;
   itypx = DSET_BRICK_TYPE(xset,ivx) ;
   xar   = DSET_ARRAY(xset,ivx) ;
   if( xar == NULL ) return 0.0 ;
   if( itypx == MRI_float ){
      fxar = (float *) xar ; fxar_new = 0 ;
   } else {
      fxar = (float *) malloc( sizeof(float) * nxyz ) ; fxar_new = 1 ;
      EDIT_coerce_type( nxyz , itypx,xar , MRI_float,fxar ) ;
      PURGE_DSET( xset ) ;
   }

   THD_load_datablock( yset->dblk , NULL ) ;
   ivy   = DSET_PRINCIPAL_VALUE(yset) ;
   itypy = DSET_BRICK_TYPE(yset,ivy) ;
   yar   = DSET_ARRAY(yset,ivy) ;
   if( yar == NULL ) return 0.0 ;
   if( itypy == MRI_float ){
      fyar = (float *) yar ; fyar_new = 0 ;
   } else {
      fyar = (float *) malloc( sizeof(float) * nxyz ) ; fyar_new = 1 ;
      EDIT_coerce_type( nxyz , itypy,yar , MRI_float,fyar ) ;
      PURGE_DSET( yset ) ;
   }

   sumxx = sumyy = sumxy = 0.0 ;

   for( ii=0 ; ii < nxyz ; ii++ ){
      tx = fxar[ii] ; ty = fyar[ii] ;
      sumxx += tx * tx ;
      sumyy += ty * ty ;
      sumxy += tx * ty ;
   }

   if( fxar_new ) free( fxar ) ;
   if( fyar_new ) free( fyar ) ;

   dxy = sumxx * sumyy ; if( dxy <= 0.0 ) return 0.0 ;
   dxy = sumxy / sqrt(dxy) ;
   return dxy ;
}
