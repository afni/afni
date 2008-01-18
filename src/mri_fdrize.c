#include "mrilib.h"

#undef  PMAX
#define PMAX 0.9999f  /* don't process p-values >= PMAX */

#undef QTOZ  /* convert q-value to z-score */
#if 0
#  define QTOZ(x) normal_p2t(x)
#else
#  define QTOZ(x) qginv(0.5*x)   /* a little faster */
#endif

/*--------------------------------------------------------------------------*/
/*! Take an image of statistics and convert to FDR-ized z-scores (in place):
      - im must be in float format
      - if statcode > 0, the data is a statistic to be converted to
        a p-value first; otherwise, the data is already p-value-ized
      - if flags&1==1, then the function tries to be compatible with the
        3dFDR program in '-old' mode:
          - in 3dFDR -old, processed input values that give p==1 are
            still counted in the number of thresholdings performed,
            which will make the output z-scores smaller
          - if flags&1==0, then this function will NOT count such
            p==1 voxels at all, which will make the z-scores larger
          - since this function actually sorts the p-values while 3dFDR
            just bins them, small differences will be present anyhoo
      - if flags&2==1, then the q-values are corrected for arbitrary
        correlation structure -- this is not necessary for FMRI data!
      - to mask, set input values to a statistic that will give p==1
        (e.g., 0.0 for t, F, or rho; 1.0 for p or z) -- and set flags=0!
*//*------------------------------------------------------------------------*/

void mri_fdrize( MRI_IMAGE *im, int statcode, float *stataux, int flags )
{
  float *far ;
  int ii,jj , nvox ;
  float *qq , nthr ; int *iq , nq ; double qval , qmin ;

ENTRY("mri_fdrize") ;

  if( im == NULL || im->kind != MRI_float )   EXRETURN ;
  far = MRI_FLOAT_PTR(im) ; if( far == NULL ) EXRETURN ;
  nvox = im->nvox ;

  /* convert to p-value? */

  if( FUNC_IS_STAT(statcode) ){
    for( ii=0 ; ii < nvox ; ii++ )
      far[ii] = THD_stat_to_pval( fabsf(far[ii]), statcode,stataux ) ;
  }

  qq = (float *)malloc(sizeof(float)*nvox) ;
  iq = (int   *)malloc(sizeof(int  )*nvox) ;
  for( nq=ii=0 ; ii < nvox ; ii++ ){
    if( far[ii] >= 0.0f && far[ii] < PMAX ){  /* reasonable p-value */
      qq[nq] = far[ii] ; iq[nq] = ii ; nq++ ;
    } else {
      far[ii] = 0.0f ;  /* clear out such criminal voxels */
    }
  }

  if( nq > 0 ){  /* something to process! */
    qsort_floatint( nq , qq , iq ) ;

    qmin = 1.0 ;
    nthr = (flags&1) ? nvox : nq ;
    if( flags&2 && nthr > 1 ) nthr *= (logf(nthr)+0.5772157f) ;
    for( jj=nq-1 ; jj >= 0 ; jj-- ){           /* convert to q, then z */
      qval = (nthr * qq[jj]) / (jj+1.0) ;
      if( qval > qmin ) qval = qmin; else qmin = qval;
           if( qval <  1.e-20 ) qval = 10.0 ;  /* honking big z-score */
      else if( qval >= 1.0    ) qval =  0.0 ;  /* very non-significant */
      else                      qval = QTOZ(qval) ;
      far[iq[jj]] = (float)qval ;
    }
  }

  free(iq); free(qq); EXRETURN;
}
