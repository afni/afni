#include "mrilib.h"

void mri_metrics_pp( MRI_IMAGE *imp , MRI_IMAGE *imq , float *met ) ;

int main( int argc , char * argv[] )
{
   int narg , ndset , nvox , nvals,iv ;
   THD_3dim_dataset *xset , *yset , *mask_dset=NULL ;
   byte *mmm=NULL ;
   float met[9] ;

   /*-- read command line arguments --*/

   if( argc < 3 || strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: 3Hmetric [options] dset1 dset2\n"
             "Output = Histogram metrics between 2 dataset bricks\n"
             "Options:\n"
             "  -mask mset   Means to use the dataset 'mset' as a mask:\n"
             "                 Only voxels with nonzero values in 'mset'\n"
             "                 will be averaged from 'dataset'.\n"
            ) ;
      exit(0) ;
   }

   narg = 1 ;
   while( narg < argc && argv[narg][0] == '-' ){

     if( strncmp(argv[narg],"-mask",5) == 0 ){
       if( mask_dset != NULL ) ERROR_exit("Can't use -mask twice") ;
         if( narg+1 >= argc ) ERROR_exit("Need argument after -mask") ;
         mask_dset = THD_open_dataset( argv[++narg] ) ;
         if( mask_dset == NULL ) ERROR_exit("Can't open -mask %s",argv[narg]) ;
         narg++ ; continue ;
      }

      ERROR_exit("Unknown option: %s",argv[narg]) ;
   }

   /* should have at least 2 more arguments */

   ndset = argc - narg ;
   if( ndset <= 1 ) ERROR_exit("Need two input datasets") ;

   xset = THD_open_dataset( argv[narg++] ) ;
   yset = THD_open_dataset( argv[narg++] ) ;
   if( xset == NULL || yset == NULL )
     ERROR_exit("Cannot open both input datasets!\n") ;

   if( DSET_NVALS(xset) > 1 )
     WARNING_message("Will only use sub-brick #0 of 1st input dataset") ;
   nvals = DSET_NVALS(yset) ;

   nvox = DSET_NVOX(xset) ;
   if( nvox != DSET_NVOX(yset) )
     ERROR_exit("Input datasets grid dimensions don't match!\n") ;

   /* make a byte mask from mask dataset */

   if( mask_dset != NULL ){
     int mcount ;
     if( DSET_NVOX(mask_dset) != nvox )
       ERROR_exit("Input and mask datasets are not same dimensions!\n");
     DSET_load(mask_dset) ;
     if( !DSET_LOADED(mask_dset) ) ERROR_exit("Can't load mask dataset") ;
     mmm = THD_makemask( mask_dset , 0 , 1.0f,-1.0f ) ;
     mcount = THD_countmask( nvox , mmm ) ;
     INFO_message("Have %d voxels in the mask\n",mcount) ;
     if( mcount <= 666 ) ERROR_exit("Mask is too small") ;
     DSET_delete(mask_dset) ;
   }

   DSET_load(xset) ;
   if( !DSET_LOADED(xset) ) ERROR_exit("Can't load 1st dataset") ;
   DSET_load(yset) ;
   if( !DSET_LOADED(yset) ) ERROR_exit("Can't load 2nd dataset") ;

   printf("# KULL   HELL  TRIA   JDIV   JSDV   XISQ   XXSQ\n") ;
   for( iv=0 ; iv < nvals ; iv++ ){
#if 0
     mri_metrics( DSET_BRICK(xset,0) , DSET_BRICK(yset,iv) , met ) ;
     printf( "%f %f %f %f %f %f %f %f\n" ,
             met[METRIC_KULL], met[METRIC_HELL], met[METRIC_TRIA] ,
             met[METRIC_JDIV], met[METRIC_JSDV], met[METRIC_XISQ] ,
             met[METRIC_XXSQ], met[METRIC_AGDV]
           ) ;
#else
     mri_metrics_pp( DSET_BRICK(xset,0) , DSET_BRICK(yset,iv) , met ) ;
     printf( "%f %f %f %f\n",met[0],met[1],met[2],met[3]) ;
#endif
   }

   exit(0) ;
}

/*------------------------------------------------------------------------*/

void mri_metrics_pp( MRI_IMAGE *imp , MRI_IMAGE *imq , float *met )
{
   int nvox ;
   int *rst , *pst , *qst ;
   byte *par, *qar ;
   float qj,rij , rat,tmp,lrr,rm1,rp1 , fac ;
   float esum,tsum,hsum , jsum,dsum,xsum , qsum,asum ;
   register int ii,jj,kk ;
   MRI_IMAGE *imqq, *impp ;

   if( imp == NULL || imq == NULL            ) return ;
   if( met == NULL || imp->nvox != imq->nvox ) return ;

   nvox = imp->nvox ; fac = 1.0f / nvox ;

   impp = (imp->kind==MRI_byte) ? imp : mri_to_byte(imp) ;
   imqq = (imq->kind==MRI_byte) ? imq : mri_to_byte(imq) ;
   par  = MRI_BYTE_PTR(impp) ;
   qar  = MRI_BYTE_PTR(imqq) ;

   pst = (int *)calloc(256    ,sizeof(int)) ;
   qst = (int *)calloc(256    ,sizeof(int)) ;
   rst = (int *)calloc(256*256,sizeof(int)) ;

   for( kk=0 ; kk < nvox ; kk++ ){
     ii = par[kk] ; jj = qar[kk] ;
     pst[ii]++ ; qst[jj]++ ; rst[ii+256*jj]++ ;
   }

   esum = tsum = hsum = 0.0f ;
   jsum = dsum = xsum = 0.0f ;
   qsum = asum =        0.0f ;
   for( jj=0 ; jj < 256 ; jj++ ){
     qj = (float)qst[jj] ;
     if( qj > 0.0f ){
       kk = 256*jj ; qj *= fac ;
       for( ii=0 ; ii < 256 ; ii++ ){
         rij = (float)rst[ii+kk] ;
         if( rij > 0.0f ){
           rat = (qj *(float)pst[ii]) / rij ;
           lrr = logf(rat) ;

           esum += rij * lrr ;
           tmp   = 1.0f/sqrtf(rat)-1.0f ; hsum += rij * tmp ;
           tmp   = 1.0f/cbrtf(rat)-1.0f ; qsum += rij * tmp ;
           tmp   = lrr*lrr              ; asum += rij * tmp ;
         }
       }
     }
   }

   free((void*)rst); free((void*)qst); free((void*)pst);
   if( impp != imp ) mri_free(impp);
   if( imqq != imq ) mri_free(imqq);

   met[0] = -fac * esum ;
   met[1] =  fac * hsum ;
   met[2] =  fac * qsum * 2.0f ;
   met[3] =  fac * asum * 0.5f ;
   return ;
}
