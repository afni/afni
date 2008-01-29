#include "mrilib.h"

#undef  PMAX
#define PMAX 0.9999f  /* don't process p-values >= PMAX */

#undef QTOZ  /* convert q-value to z-score */
#if 0
#  define QTOZ(x) normal_p2t(x)  /* the olde waye */
#else
#  define QTOZ(x) qginv(0.5*x)   /* a little faster */
#endif

#undef  ZTOP
#undef  QBOT
#define ZTOP 9.0           /* largest value of z(q) to output */
#define QBOT 2.25718e-19   /* smallest value of q to return */

/*--------------------------------------------------------------------------*/
/*! Take an image of statistics and convert to FDR-ized z-scores (in place):
      - im must be in float format
      - if statcode > 0, the data is a statistic to be converted to
        a p-value first
        (input data values == 0 are masked out as having p=1)
      - otherwise, the data is already p-value-ized
        (input data values < 0 or >= 1 will be masked out)
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
      - if flags&4==1, then the output is q-values, not z-values
      - to mask, set input values to a statistic that will give p==1
        (e.g., 0.0 for t, F, or rho; 1.0 for p) -- and set flags=0;
        masked out voxels will be set to 0 (or 1 if flags&4 is set).
*//*------------------------------------------------------------------------*/

int mri_fdrize( MRI_IMAGE *im, int statcode, float *stataux, int flags )
{
  float *far ;
  int ii,jj , nvox , doz ;
  float *qq , nthr , fbad ; int *iq , nq ; double qval , qmin ;

ENTRY("mri_fdrize") ;

  if( im == NULL || im->kind != MRI_float )   RETURN(0) ;
  far  = MRI_FLOAT_PTR(im); if( far == NULL ) RETURN(0) ;
  nvox = im->nvox ;
  doz  = (flags&4) == 0 ;

  /* convert to p-value? */

  if( FUNC_IS_STAT(statcode) ){     /* conversion to p-value */
    for( ii=0 ; ii < nvox ; ii++ ){
      if( far[ii] != 0.0f )
        far[ii] = THD_stat_to_pval( fabsf(far[ii]), statcode,stataux ) ;
      else
        far[ii] = 1.0f ;
    }
  } else {                          /* already supposed to be p-value */
    for( ii=0 ; ii < nvox ; ii++ )
      if( far[ii] < 0.0f || far[ii] > 1.0f ) far[ii] = 1.0f ;
  }

  qq   = (float *)malloc(sizeof(float)*nvox) ;
  iq   = (int   *)malloc(sizeof(int  )*nvox) ;
  fbad = (doz) ? 0.0f : 1.0f ;  /* output value for masked voxels */
  for( nq=ii=0 ; ii < nvox ; ii++ ){
    if( far[ii] >= 0.0f && far[ii] < PMAX ){  /* reasonable p-value */
      qq[nq] = far[ii] ; iq[nq] = ii ; nq++ ;
    } else {
      far[ii] = fbad ;  /* clear out such criminal voxels */
    }
  }

  if( nq > 0 ){  /* something to process! */
    qsort_floatint( nq , qq , iq ) ;  /* sort into increasing order */
                                      /* iq[] tracks where its from */
    qmin = 1.0 ;
    nthr = (flags&1) ? nvox : nq ;
    if( (flags&2) && nthr > 1 ) nthr *= (logf(nthr)+0.5772157f) ;
    for( jj=nq-1 ; jj >= 0 ; jj-- ){           /* convert to q, then z */
      qval = (nthr * qq[jj]) / (jj+1.0) ;
      if( qval > qmin ) qval = qmin; else qmin = qval;
      if( doz ){                               /**** convert q to z ****/
             if( qval <  QBOT ) qval = ZTOP ;  /* honking big z-score  */
        else if( qval >= 1.0  ) qval =  0.0 ;  /* very non-significant */
        else                    qval = QTOZ(qval) ; /* meaningful z(q) */
      }
      far[iq[jj]] = (float)qval ;
    }
  }

  free(iq); free(qq); RETURN(nq);
}

/*--------------------------------------------------------------------------*/

#undef  NCURV
#define NCURV 101

/*! Create a curve that gives the FDR z(q) value vs. the statistical
    threshold.  Stored as a mri_floatvec struct, with the statistical
    value give as x0+i*dx and the corresponding z(q) value in ar[i].  */

floatvec * mri_fdr_curve( MRI_IMAGE *im, int statcode, float *stataux )
{
  MRI_IMAGE *cim ;
  float *car , *far , *zar , *tar ;
  int nvox , ii , nq , *iq ;
  floatvec *fv=NULL ;
  float tbot,ttop , zbot,ztop , dt,tt,zz , t1,t2,z1,z2 , tf,zf ;
  int kk,klast,jj ;

ENTRY("mri_fdr_curve") ;

  /* check for legal inputs */

  if( !FUNC_IS_STAT(statcode) )              RETURN(NULL) ;
  if( im == NULL || im->kind != MRI_float )  RETURN(NULL) ;
  far = MRI_FLOAT_PTR(im); if( far == NULL ) RETURN(NULL) ;

  /* make a copy of the statistics and convert them to z(q) scores */

  cim = mri_to_float(im) ; car = MRI_FLOAT_PTR(cim) ;

  nq = mri_fdrize( cim , statcode , stataux , 0 ) ;
  if( nq < 9 ){ mri_free(cim); RETURN(NULL); }  /* bad FDR-izing */

  /* create iq[] = list of voxels that were validly FDR-ized */

  nvox = im->nvox ;
  iq   = (int *)malloc(sizeof(int)*nvox) ;
  for( nq=ii=0 ; ii < nvox ; ii++ ){      /* make list of voxels with */
    if( car[ii] > 0.0f ) iq[nq++] = ii ;  /* meaningful z(q) values   */
  }
  if( nq < 9 ){ free(iq); mri_free(cim); RETURN(NULL); }  /* bad */

  /* create list of z(q) scores and corresponding statistics */

  zar = (float *)malloc(sizeof(float)*nq) ;  /* z(q) values */
  tar = (float *)malloc(sizeof(float)*nq) ;  /* statistics */
  for( ii=0 ; ii < nq ; ii++ ){
    zar[ii] = car[iq[ii]] ; tar[ii] = fabsf(far[iq[ii]]) ;
  }
  free(iq) ; mri_free(cim) ;   /* toss the trash */

  qsort_floatfloat( nq , zar , tar ) ;  /* sort into increasing z */

  /* find the largest z(q) that isn't beyond the top value we like */

  for( klast=nq-1 ; klast > 0 && zar[klast] >= ZTOP ; klast-- ) ; /*nada */
  if( klast == 0 ){ free(tar); free(zar); RETURN(NULL); }
  if( klast < nq-1 ) klast++ ;

  /* make the floatvec, evenly spaced in the statistic (tar) values */

  tbot = tar[0] ; ttop = tar[klast] ; dt = (ttop-tbot)/(NCURV-1) ;
  zbot = zar[0] ; ztop = zar[klast] ;

  MAKE_floatvec(fv,NCURV) ; fv->dx = dt ; fv->x0 = tbot ;
  fv->ar[0] = zbot ;
  for( jj=ii=1 ; ii < NCURV-1 ; ii++ ){
    tt = tbot + ii*dt ;  /* the statistic for this point on the curve */
    for( ; jj < nq && tar[jj] < tt ; jj++ ) ; /*nada*/
    t1 = tar[jj-1] ; t2 = tar[jj] ; tf = (tt-t1)/(t2-t1) ;      /* linearly */
    z1 = zar[jj-1] ; z2 = zar[jj] ; zf = tf*z2 + (1.0f-tf)*z1 ; /* interp z */
    fv->ar[ii] = zf ;                                         /* to this tt */
  }
  fv->ar[NCURV-1] = ztop ;

  free(tar) ; free(zar) ; RETURN(fv) ;
}
