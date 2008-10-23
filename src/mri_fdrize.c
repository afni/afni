#include "mrilib.h"

#undef  PMAX
#define PMAX 0.9999f  /*** don't process p-values >= PMAX ***/

#undef  QTOZ                  /* convert q-value to z-score */
#undef  ZTOQ                  /* vice-versa */
#define QTOZ(x) qginv(0.5*x)
#define ZTOQ(x) (2.0*qg(x))

#undef  ZTOP
#undef  QBOT
#define ZTOP 9.0           /* largest value of z(q) to output */
#define QBOT 2.25718e-19   /* smallest value of q to return */
#define PBOT 1.e-15        /* smallest value of p to use */

/*--------------------------------------------------------------------------*/
#undef  QSTHRESH
#define QSTHRESH 0.15      /* q threshold for computing FDR_mdfv */

static floatvec *FDR_mdfv = NULL ;  /* missed detection fraction vs. log10(p) */

floatvec *mri_fdr_getmdf(void){ return FDR_mdfv; }  /* 22 Oct 2008 */

/*--------------------------------------------------------------------------*/
/* Estimate m1 = number of true positives.
   Actually, estimates m0 = number of true negatives, then m1 = nq-m0.
   Build a histogram of large-ish p-values [0.15..0.95], which should be
   approximately uniformly distribued, then find m0 by estimating the
   average-ish level of this histogram.  If something bad happens,
   return value is -1.
----------------------------------------------------------------------------*/

static int estimate_m1( int nq , float *qq )
{
   int jj , kk , nh=0 , hist[16] , mone ;

   if( nq < 299 || qq == NULL ) return -1 ;

   for( kk=0 ; kk < 16 ; kk++ ) hist[kk] = 0.0f ;
   for( jj=0 ; jj < nq ; jj++ ){  /* histogram bin width is 0.05 */
     kk = (int)( (qq[jj]-0.15f)*20.0f ) ; if( kk < 0 || kk > 15 ) continue ;
     hist[kk]++ ; nh++ ;
   }
   if( nh < 160 ) return -1 ; /* too few p-values in [0.15..0.95] range */
   qsort_int( 16 , hist ) ;   /* sort; use central values to get 'average' */
#if 0
   mone = nq - 20.0f * ( hist[6] + 2*hist[7] + 2*hist[8] + hist[9] ) / 6.0f ;
#else
   mone = nq - 20.0f * (    hist[5] + 2*hist[6] + 2*hist[7]
                        + 2*hist[8] + 2*hist[9] +   hist[10] ) / 10.0f ;
#endif
   return mone ;
}

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
  int ii,jj , nvox , doz , qsmal=0 ;
  float *qq , nthr , fbad ; int *iq , nq ; double qval , qmin ;

ENTRY("mri_fdrize") ;

  KILL_floatvec(FDR_mdfv) ; FDR_mdfv = NULL ; /* erase the past */

  if( im == NULL || im->kind != MRI_float )   RETURN(0) ;
  far  = MRI_FLOAT_PTR(im); if( far == NULL ) RETURN(0) ;
  nvox = im->nvox ;
  doz  = (flags&4) == 0 ;

  /*----- convert to p-value first? -----*/

  if( FUNC_IS_STAT(statcode) ){     /* conversion to p-value */
STATUS("convert to p-value") ;
    for( ii=0 ; ii < nvox ; ii++ ){
      if( far[ii] != 0.0f )
        far[ii] = THD_stat_to_pval( fabsf(far[ii]), statcode,stataux ) ;
      else
        far[ii] = 1.0f ;            /* will be ignored */
    }
  } else {                          /* already supposed to be p-value */
STATUS("input is p-value") ;
    for( ii=0 ; ii < nvox ; ii++ )  /* scan for bad values */
      if( far[ii] < 0.0f || far[ii] > 1.0f ) far[ii] = 1.0f ;
  }

  qq = (float *)malloc(sizeof(float)*nvox) ;  /* array of p-values */
  iq = (int   *)malloc(sizeof(int  )*nvox) ;  /* voxel indexes */

  if( qq == NULL || iq == NULL ){
    ERROR_message("mri_fdrize: out of memory!") ;
    if( qq != NULL ) free(qq) ;
    if( iq != NULL ) free(iq) ;
    RETURN(0) ;
  }

  /*---- build array of p-values into qq and source voxel indexes into iq ----*/

STATUS("find reasonable p-values") ;
  fbad = (doz) ? 0.0f : 1.0f ;  /* output value for masked voxels */
  for( nq=ii=0 ; ii < nvox ; ii++ ){
    if( far[ii] >= 0.0f && far[ii] < PMAX ){  /* reasonable p-value */
      qq[nq] = (far[ii] > PBOT) ? far[ii] : PBOT ;
      iq[nq] = ii ; nq++ ;
    } else {
      far[ii] = fbad ;  /* clear out such criminal voxels in the result array */
    }
  }

  /*----- process p-values to get q-values -----*/

  if( nq > 19 ){  /* something to process! */

    if( nvox-nq > 3333 ){  /* free up space, if significant */
      qq = (float *)realloc( qq , sizeof(float)*nq ) ;
      iq = (int   *)realloc( iq , sizeof(int  )*nq ) ;
    }

STATUS("sorting p-values") ;
    qsort_floatint( nq , qq , iq ) ;  /* sort into increasing order; */
                                      /* iq[] tracks where it's from */

    /* downward scan from large p's to calculate q's */

    qmin = 1.0 ;
    nthr = (flags&1) ? nvox : nq ;
    if( (flags&2) && nthr > 1 ) nthr *= (logf(nthr)+0.5772157f) ;
STATUS("convert to q/z") ;
    for( jj=nq-1 ; jj >= 0 ; jj-- ){           /* convert to q, then z */
      qval = (nthr * qq[jj]) / (jj+1.0) ;
      if( qval > qmin ) qval = qmin; else qmin = qval;
      if( qsmal == 0 && qval <= QSTHRESH ) qsmal = jj ;
      if( doz ){                               /**** convert q to z ****/
             if( qval <  QBOT ) qval = ZTOP ;  /* honking big z-score  */
        else if( qval >= 1.0  ) qval =  0.0 ;  /* very non-significant */
        else                    qval = QTOZ(qval) ; /* meaningful z(q) */
      }
      far[iq[jj]] = (float)qval ;   /* store q or z into result array */
    }
    free(iq) ; iq = NULL ;

    /* compute missed detection fraction vs. log10(p) */

    if( qsmal && nq > 199 && qq[0] > 0.0f ){
STATUS("computing mdf") ;
      int mm1 = estimate_m1( nq , qq ) ;  /* number of true positives? */

      if( mm1 > 9 ){
        float mone=(float)mm1 , ms , dpl ; int jtop,jbot , npp , kk ;
        float *mdf=(float *)malloc(sizeof(float)*nq) ;
        floatvec *fv ; float p1,p2,m1,m2,pf,mf , pl,pv ;
        if( mdf == NULL ){
          ERROR_message("mri_fdr_curve: out of memory!") ; goto finished ;
        }
        qmin = 1.0 ;
        for( jj=nq-1 ; jj >=0 ; jj-- ){      /* scan down again to get q */
          qval = (nthr * qq[jj]) / (jj+1.0) ;
          if( qval > qmin ) qval = qmin; else qmin = qval;
          if( qval < QBOT ) qval = QBOT;

          /* Number of values above this threshold is jj+1;
             Approximately qval*(jj+1) of these are false positive detections
               (that's what FDR means, dude);
             So about (1-qval)*(jj+1) are true positive detections;
             So about (1-qval)*(jj+1)/mone is the ratio
               of true detections to the number of true positives;
             So about 1-(1-qval)*(jj+1)/mone is about the
               fraction of missed true detections;
             If you believe this, I've got a bridge in Brooklyn for sale. */

          mdf[jj] = 1.0 - (1.0-qval)*(jj+1) / mone ;
               if( mdf[jj] < 0.0f ) mdf[jj] = 0.0f ;  /* make sure mdf */
          else if( mdf[jj] > 1.0f ) mdf[jj] = 1.0f ;  /* is reasonable */
        }
        for( jj=1 ; jj < nq ; jj++ ){  /* mdf decreases as p-value increases */
          if( mdf[jj] > mdf[jj-1] ) mdf[jj] = mdf[jj-1] ;
        }
        ms = mdf[nq-1] ;  /* cheapo trick: make sure mdf -> 0 as p -> 1 */
        if( ms > 0.0f ){
          float alp=1.0f/(1.0f-ms) , bet=alp*ms ;
          for( jj=0 ; jj < nq ; jj++ ) mdf[jj] = alp*mdf[jj]-bet ;
          mdf[nq-1] = 0.0f ;
        }
        /* now find last nonzero mdf */
        for( jj=nq-2 ; jj > 0 && mdf[jj] == 0.0f ; jj-- ) ; /*nada*/
        if( jj <= 1 || qq[jj+1] <= qq[0] ){
          if( jj       <= 1     ) STATUS("all mdf zero? ==> no mdf") ;
          if( qq[jj+1] <= qq[0] ) STATUS("qq=const? ==> no mdf") ;
          free(mdf) ; goto finished ;
        }
        jtop = jj+1 ;  /* mdf[jtop] = 0 */
        /* now find first mdf below 99.9% */
        for( jj=1 ; jj < jtop && mdf[jj] >= 0.999f ; jj++ ) ; /*nada*/
        jbot = (jj < jtop-9) ? jj-1 : 0 ;
        /* build a table of mdf vs log10(p) */
        ms   = log10( qq[jtop] / qq[jbot] ) ; /* will be positive */
        npp  = (int)( 0.99f + 5.0f * ms ) ;   /* number of grid points */
        if( npp < 3 ){
          if( PRINT_TRACING ){
            char str[256] ;
            sprintf(str,"nq=%d npp=%d jbot=%d jtop=%d qq[jtop]=%g qq[jbot]=%g ==> no mdf",
                    nq , npp , jbot,jtop , qq[jtop] , qq[jbot] ) ;
            STATUS(str) ;
          }
          free(mdf); goto finished;
        }
        dpl = ms / (npp-1) ;                    /* grid spacing in log10(p) */
        MAKE_floatvec(fv,npp) ; fv->x0 = log10(qq[jbot]) ; fv->dx = dpl ;
        fv->ar[0] = mdf[jbot] ;
        for( jj=jbot,kk=1 ; kk < npp-1 ; kk++ ){
          pl = fv->x0 + kk*dpl ;   /* kk-th grid point in log10(p) */
          pv = powf(10.0f,pl) ;    /* kk-th grid point in p */
          for( ; jj < jtop && qq[jj] < pv ; jj++ ) ; /*nada*/
          /* linearly interpolate mdf in log10(p) */
          p1 = log10(qq[jj-1]) ; p2 = log10(qq[jj]) ; pf = (pl-p1)/(p2-p1) ;
          m1 = mdf[jj-1]       ; m2 = mdf[jj]       ; mf = pf*m2 + (1.0f-pf)*m1;
          fv->ar[kk] = mf ;
        }
        fv->ar[npp-1] = 0.0f ;
        FDR_mdfv = fv ;        /* record the results for posterity */
        if( PRINT_TRACING ){
          char str[256]; sprintf(str,"MDF: npp=%d x0=%g dx=%g",npp,fv->x0,fv->dx);
          STATUS(str) ;
        }
        free(mdf) ;

#if 0
printf("# m1 = %d\n",mm1) ;
printf("# log10(p0) = %g  d(log10(p)) = %g\n",fv->x0,fv->dx) ;
for( kk=0 ; kk < npp ; kk++ ) printf("%g %g\n",fv->x0+kk*fv->dx,fv->ar[kk]) ;
#endif

      } /* end of producing mdf values */
      else {              STATUS("smal m1   ==> no mdf") ; }
    } else {
      if( !qsmal        ) STATUS("no qsmal  ==> no mdf") ;
      if( nq < 200      ) STATUS("small nq  ==> no mdf") ;
      if( qq[0] <= 0.0f ) STATUS("bad qq[0] ==> no mdf") ;
    }

  } /* end of producing q-values */

finished:
STATUS("finished") ;
  if( iq != NULL ) free(iq);
  if( qq != NULL ) free(qq);
  RETURN(nq);
}

/*--------------------------------------------------------------------------*/

#undef  NCURV
#define NCURV 101

/*! Create a curve that gives the FDR z(q) value vs. the statistical
    threshold.  Stored as a floatvec struct, with the statistical
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

  KILL_floatvec(FDR_mdfv) ;  /* erase the past */

  if( !FUNC_IS_STAT(statcode) )              RETURN(NULL) ;
  if( im == NULL || im->kind != MRI_float )  RETURN(NULL) ;
  far = MRI_FLOAT_PTR(im); if( far == NULL ) RETURN(NULL) ;

  /* make a copy of the statistics and convert them to z(q) scores */

STATUS("copy statistics image") ;
  cim = mri_to_float(im) ; car = MRI_FLOAT_PTR(cim) ;

  nq = mri_fdrize( cim , statcode , stataux , 0 ) ;
  if( nq < 9 ){ mri_free(cim); RETURN(NULL); }  /* bad FDR-izing */

  /* create iq[] = list of voxels that were validly FDR-ized */

STATUS("make list of valid q's") ;
  nvox = im->nvox ;
  iq   = (int *)malloc(sizeof(int)*nvox) ;
  if( iq == NULL ){
    ERROR_message("mri_fdr_curve: out of memory!") ;
    mri_free(cim); RETURN(NULL);
  }

  for( nq=ii=0 ; ii < nvox ; ii++ ){      /* make list of voxels with */
    if( car[ii] > 0.0f ) iq[nq++] = ii ;  /* meaningful z(q) values   */
  }
  if( nq < 9 ){ free(iq); mri_free(cim); RETURN(NULL); }  /* bad */

  /* create list of z(q) scores and corresponding statistics */

  zar = (float *)malloc(sizeof(float)*nq) ;  /* z(q) values */
  tar = (float *)malloc(sizeof(float)*nq) ;  /* statistics */
  if( zar == NULL || tar == NULL ){
    ERROR_message("mri_fdr_curve: out of memory!") ;
    if( zar != NULL ) free(zar) ;
    if( tar != NULL ) free(tar) ;
    free(iq); mri_free(cim); RETURN(NULL);
  }

STATUS("make list of z(q) vs. statistic") ;
  for( ii=0 ; ii < nq ; ii++ ){
    zar[ii] = car[iq[ii]] ; tar[ii] = fabsf(far[iq[ii]]) ;
  }
  free(iq) ; mri_free(cim) ;   /* toss the trash */

STATUS("sort the list") ;
  qsort_floatfloat( nq , zar , tar ) ;  /* sort into increasing z */

  /* find the largest z(q) that isn't beyond the top value we like */

  for( klast=nq-1 ; klast > 0 && zar[klast] >= ZTOP ; klast-- ) ; /*nada */
  if( klast == 0 ){ free(tar); free(zar); RETURN(NULL); }
  if( klast < nq-1 ) klast++ ;

  /* make the floatvec, evenly spaced in the statistic (tar) values */

  tbot = tar[0] ; ttop = tar[klast] ; dt = (ttop-tbot)/(NCURV-1) ;
  zbot = zar[0] ; ztop = zar[klast] ;

STATUS("make the floatvec") ;
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

STATUS("finished") ;
  free(tar) ; free(zar) ; RETURN(fv) ;
}
