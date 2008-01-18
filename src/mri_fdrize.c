#include "mrilib.h"

#undef  PMAX
#define PMAX 0.999f

void mri_fdrize( MRI_IMAGE *im , int statcode , float *stataux )
{
  float *far ;
  int ii,jj , nvox ;
  float *qq ; int *iq , nq ; double qval ;

ENTRY("mri_fdrize") ;

  if( im == NULL || im->kind != MRI_float )   EXRETURN ;
  far = MRI_FLOAT_PTR(im) ; if( far == NULL ) EXRETURN ;
  nvox = im->nvox ;

  /* convert to p-value */

  if( statcode > 0 ){
    for( ii=0 ; ii < nvox ; ii++ )
      far[ii] = THD_stat_to_pval( fabsf(far[ii]), statcode,stataux ) ;
  }

  qq = (float *)malloc(sizeof(float)*nvox) ;
  iq = (int   *)malloc(sizeof(int  )*nvox) ;
  for( nq=ii=0 ; ii < nvox ; ii++ ){
    if( far[ii] >= 0.0f && far[ii] < PMAX ){
      qq[nq] = far[ii] ; iq[nq] = ii ; nq++ ;
    } else {
      far[ii] = 0.0f ;
    }
  }

  if( nq > 0 ){  /* something to process! */
    qsort_floatint( nq , qq , iq ) ;

    for( jj=0 ; jj < nq ; jj++ ){
      qval = (nq * qq[jj]) / (jj+1.0) ;
      if( qval < 1.e-20 ) qval = 10.0 ;
      else                qval = normal_p2t(qval) ;
      far[iq[jj]] = (float)qval ;
    }
  }

  free(iq); free(qq); EXRETURN;
}
