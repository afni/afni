#include "mrilib.h"

/*------------------- 08 Oct 2010: functions for despiking ----------------*/

#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)

#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b)

/*--- fast median of 9 values ---*/

static INLINE float median9f(float *p)
{
    register float temp ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[1]) ; SORT2(p[3],p[4]) ; SORT2(p[6],p[7]) ;
    SORT2(p[1],p[2]) ; SORT2(p[4],p[5]) ; SORT2(p[7],p[8]) ;
    SORT2(p[0],p[3]) ; SORT2(p[5],p[8]) ; SORT2(p[4],p[7]) ;
    SORT2(p[3],p[6]) ; SORT2(p[1],p[4]) ; SORT2(p[2],p[5]) ;
    SORT2(p[4],p[7]) ; SORT2(p[4],p[2]) ; SORT2(p[6],p[4]) ;
    SORT2(p[4],p[2]) ; return(p[4]) ;
}
#undef SORT2
#undef SWAP

/*--- get the local median and MAD of values vec[j-4 .. j+4] ---*/

#undef  mead9
#define mead9(j)                                               \
 { float qqq[9] ; int jj = (j)-4 ;                             \
   if( jj < 0 ) jj = 0; else if( jj+8 >= num ) jj = num-9;     \
   qqq[0] = vec[jj+0]; qqq[1] = vec[jj+1]; qqq[2] = vec[jj+2]; \
   qqq[3] = vec[jj+3]; qqq[4] = vec[jj+4]; qqq[5] = vec[jj+5]; \
   qqq[6] = vec[jj+6]; qqq[7] = vec[jj+7]; qqq[8] = vec[jj+8]; \
   med    = median9f(qqq);     qqq[0] = fabsf(qqq[0]-med);     \
   qqq[1] = fabsf(qqq[1]-med); qqq[2] = fabsf(qqq[2]-med);     \
   qqq[3] = fabsf(qqq[3]-med); qqq[4] = fabsf(qqq[4]-med);     \
   qqq[5] = fabsf(qqq[5]-med); qqq[6] = fabsf(qqq[6]-med);     \
   qqq[7] = fabsf(qqq[7]-med); qqq[8] = fabsf(qqq[8]-med);     \
   mad    = median9f(qqq); }


/*-------------------------------------------------------------------------*/
/*!  Return value is the number of spikes that were squashed
     (bigger spikes count more).
*//*-----------------------------------------------------------------------*/

float MYY_despike9( int num , float *vec )
{
   int ii ; float *zma,*zme , med,mad,val , spk=0.0f;

   if( num < 9 || vec == NULL ) return 0 ;
   zme = (float *)malloc(sizeof(float)*num) ;
   zma = (float *)malloc(sizeof(float)*num) ;

   for( ii=0 ; ii < num ; ii++ ){
     mead9(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ; free(zma) ;
   if( mad <= 0.0f ){ free(zme); return 0; }  /* should not happen */
   mad *= 7.777f ;  /* threshold value */

   for( ii=0 ; ii < num ; ii++ ){
     val = fabsf(vec[ii]-zme[ii]) / mad ;
     if( val > 1.0f ){ vec[ii] = zme[ii] ; spk += val ; }
   }

   free(zme) ; return spk ;
}
#undef mead9

/*----------------------------------------------------------------------*/
/* Count spikes.
   Then count steps as spikes in the first difference
   -- after isolated spikes were squashed.
*//*--------------------------------------------------------------------*/

float_pair THD_countspikes( int num , float *vec )
{
   float_pair spair = {0.0f,0.0f} ;
   int n1=num-1 , ii ;
   float *del , spk,stp ;

   if( num < 11 || vec == NULL ) return spair ;

   spk = MYY_despike9( num , vec ) ;

   del = (float *)malloc(sizeof(float)*n1) ;
   for( ii=0 ; ii < n1 ; ii++ ) del[ii] = vec[ii+1]-vec[ii] ;
   stp = MYY_despike9( n1 , del ) ;
   free(del) ;

   spair.a = spk ; spair.b = stp ; return spair ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   int do_automask=1 ;
   THD_3dim_dataset *inset=NULL ;
   byte *mask=NULL ;
   int mask_nx=0,mask_ny=0,mask_nz=0,nmask , verb=0 , nx,ny,nz,nvox , kk,ntime ;
   float_pair spair ; float spk=0.0f , stp=0.0f ;
   MRI_vectim *mrv ;
   int nopt=1 ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 3dCountSpikes dataset\n"
       "\n"
       "Does this program do something useful?\n"
       "If it does, that circumstance is completely accidental.\n"
     ) ;
     exit(0) ;
   }

   /*-- startup --*/

   mainENTRY("3dCountSpikes"); machdep();

   /*-- input --*/

   inset = THD_open_dataset(argv[nopt]) ;
   CHECK_OPEN_ERROR(inset,argv[nopt]) ; nopt++ ;
   ntime = DSET_NVALS(inset) ;
   if( ntime < 11 ) ERROR_exit("Input dataset is too short!") ;

   nx = DSET_NX(inset); ny = DSET_NY(inset); nz = DSET_NZ(inset); nvox = nx*ny*nz;

   if( mask != NULL ){
     if( mask_nx != nx || mask_ny != ny || mask_nz != nz )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( do_automask && nvox > nx ){
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     nmask = THD_countmask( DSET_NVOX(inset) , mask ) ;
     if( verb ) INFO_message("Number of voxels in automask = %d",nmask);
     if( nmask < 1 ) ERROR_exit("Automask is too small to process") ;

   } else {
     mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
     memset(mask,1,sizeof(byte)*nvox) ;
     if( verb ) INFO_message("No mask ==> processing all %d voxels",nvox);
   }

   /* convert input dataset to a vectim, which is more fun */

   mrv = THD_dset_to_vectim( inset , mask , 0 ) ;
   if( mrv == NULL ) ERROR_exit("Can't load time series data!?") ;
   DSET_unload(inset) ;

   for( kk=0 ; kk < mrv->nvec ; kk++ ){
     spair = THD_countspikes( mrv->nvals , VECTIM_PTR(mrv,kk) ) ;
     spk += spair.a ; stp += spair.b ;
   }

   printf("#spikiness = %.0f    #steppiness = %.0f\n",spk,stp) ;
   exit(0) ;
}
