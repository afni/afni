#include "mrilib.h"

/*----------------------------------------------------------------------------------
  Inputs: dset (3D+time) and qthr in (0..0.1).
  Outputs: *count = array of integer counts of outliers (1 count per sub-brick)
           *ctop  = median + 3.5 * MAD of count[]
  05 Nov 2001: modified to use THD_extract_array() instead of THD_extract_series()
------------------------------------------------------------------------------------*/

void THD_outlier_count( THD_3dim_dataset *dset, float qthr, int **count, int *ctop )
{
   int nvals , iv , nxyz , ii , oot , *ccc ;
   float alph,fmed,fmad , fbot,ftop ;
   MRI_IMAGE * flim ;
   float * far , * var , clip_val ;

ENTRY("THD_outlier_count") ;

   /*-- sanity checks --*/

   if( !ISVALID_DSET(dset) ) EXRETURN ;
   if( qthr <= 0.0 || qthr >= 0.1 ) qthr = 0.001 ;

   nvals = DSET_NUM_TIMES(dset) ;
   nxyz  = DSET_NVOX(dset) ;
   if( nvals < 5 ){ *count = NULL ; *ctop = 0 ; EXRETURN ; }
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){ *count = NULL ; *ctop = 0 ; EXRETURN ; }

   /*-- find clip level [will ignore voxels below this value] --*/

   flim = THD_median_brick( dset ) ;
   clip_val = THD_cliplevel( flim , 0.5 ) ;
   mri_free(flim) ;

   /*-- setup to count outliers --*/

   alph   = qginv(qthr/nvals) * sqrt(0.5*PI) ;
   *count = ccc = (int *) calloc( sizeof(int) , nvals ) ;
   var    = (float *) malloc( sizeof(float) * nvals ) ;

   /*--- loop over voxels and count ---*/

   far = (float *) calloc(sizeof(float),nvals+1) ;  /* 05 Nov 2001 */

   for( ii=0 ; ii < nxyz ; ii++ ){

      /*- get time series from voxel #ii -*/

      THD_extract_array( ii , dset , 0 , far ) ;          /* 05 Nov 2001 */
      memcpy(var,far,sizeof(float)*nvals ) ;              /* copy it */

      fmed = qmed_float( nvals , far ) ;                  /* median */
      if( clip_val > 0.0 && fmed < clip_val ) continue ;  /* below clip? */
      for( iv=0 ; iv < nvals ; iv++ )
         far[iv] = fabs(far[iv]-fmed) ;
      fmad = qmed_float( nvals , far ) ;                  /* MAD */
      fbot = fmed - alph*fmad ; ftop = fmed + alph*fmad ; /* inlier range */

      if( fmad > 0.0 ){                                   /* count outliers */
         for( iv=0 ; iv < nvals ; iv++ )
            if( var[iv] < fbot || var[iv] > ftop ) ccc[iv]++ ;
      }

   }

   free(far) ;  /* 05 Nov 2001 */

   for( iv=0 ; iv < nvals ; iv++ ) var[iv] = ccc[iv] ;    /* float-ize counts */
   qmedmad_float( nvals,var , &fmed,&fmad ) ; free(var) ; /* median and MAD */
   *ctop = (int)(fmed+3.5*fmad+0.499) ;                   /* too much? */

   EXRETURN ;
}
