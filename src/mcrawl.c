#include "mrilib.h"

int main( int argc , char * argv[] )
{
   float mfrac=0.50 ;
   double dsum ;
   int nvox , iarg=1 , hist[32768] , ii,npos=0 , ncut,nmed,kk,ib , qq,nold ;
   THD_3dim_dataset * dset ;
   short * sar ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: mcrawl [-mfrac x] dataset\n"); exit(0);
   }

   if( strcmp(argv[iarg],"-mfrac") == 0 ){
      mfrac = strtod( argv[++iarg] , NULL ) ;
      if( mfrac <= 0.0 ){ fprintf(stderr,"ILLEGAL -mfrac\n");exit(1);}
      if( mfrac >= 1.0 ) mfrac *= 0.01 ;
      iarg++ ;
   }

   dset = THD_open_dataset(argv[iarg]) ;
   if( !ISVALID_DSET(dset) ){ fprintf(stderr,"CAN'T open dataset\n");exit(1);}
   if( DSET_BRICK_TYPE(dset,0) != MRI_short ){ fprintf(stderr,"ILLEGAL dataset type\n");exit(1); }
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){ fprintf(stderr,"CAN'T load dataset\n");exit(1);}
   sar = DSET_ARRAY(dset,0) ;

   nvox = DSET_NVOX(dset) ;
   memset( hist , 0 , sizeof(int)*32768 ) ;

   dsum = 0.0 ;
   for( ii=0 ; ii < nvox ; ii++ ){
      if( sar[ii] > 0 ){
         hist[sar[ii]]++ ; dsum += (double)(sar[ii])*(double)(sar[ii]) ; npos++ ;
      }
   }

   DSET_unload(dset) ;

   printf("npos = %d\n",npos) ; if( npos <= 99 ) exit(1) ;

   qq = 0.65 * npos ; ib = rint(0.5*sqrt(dsum/npos)) ;
   for( kk=0,ii=32767 ; ii >= ib && kk < qq ; ii-- ) kk += hist[ii] ;

   ncut = ii ; qq = 0 ;
   do{
      for( npos=0,ii=ncut ; ii < 32768 ; ii++ ) npos += hist[ii] ;
      npos /= 2 ;  /* find median */
      for( kk=0,ii=ncut ; ii < 32768 && kk < npos ; ii++ ) kk += hist[ii] ;
      printf("ncut=%d ii=%d kk=%d\n",ncut,ii,kk) ;
      nold = ncut ;
      ncut = mfrac * ii ;
      qq++ ;
   } while( qq < 20 && ncut != nold ) ;

   exit(0) ;
}
