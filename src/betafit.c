#include "mrilib.h"

/*----------------------------------------------------------------------
   Inputs: (a,b,xc) for incomplete beta
   Outputs:
   Let Ipq = Int( x**(a-1)*(1-x)**(b-1)*ln(x)**p*ln(1-x)**q, x=0..xc ).
   Then
     bi7[0] = I00     = normalization factor
     bi7[1] = I10/I00 = <ln(x)>
     bi7[2] = I01/I00 = <ln(1-x)>
     bi7[3] = d(bi7[1])/da = (I20*I00-I10**2)/I00**2
     bi7[4] = d(bi7[1])/db = (I11*I00-I10*I01)/I00**2
     bi7[5] = d(bi7[2])/da = (I11*I00-I10*I01)/I00**2
     bi7[6] = d(bi7[2])/db = (I02*I00-I01**2)/I00**2
   The integrals are calculated by transforming to y=a*ln(xc/x), and
   then using Gauss-Laguerre quadrature:

   Int( x**(a-1)*(1-x)**(b-1) * f(x) , x=0..xc )

   transforms to

   xc**a
   ----- * Int( exp(-y)*(1-xc*exp(-y/a))**(b-1)*f(xc*exp(-y/a)), y=0..infty )
     a

   The return value of this function is -1 if an error occurred, and
   is 0 if all is good.
-------------------------------------------------------------------------*/

static int bi7func( double a , double b , double xc , double * bi7 )
{
#define NL 20  /* must be between 2 and 20 - see cs_laguerre.c */

   static double *yy=NULL , *ww=NULL ;
   double xx , s00,s10,s01,s20,s11,s02 , ff , l0,l1 ;
   register int ii ;

   if( a  <= 0.0 || b  <= 0.0 ||
       xc <= 0.0 || xc >= 1.0 || bi7 == NULL ) return -1 ;  /* sanity check */

   /* initialize Laguerre integration table */

   if( yy == NULL ) get_laguerre_table( NL , &yy , &ww ) ;

   s00=s10=s01=s20=s11=s02 = 0.0 ;
   for( ii=NL-1 ; ii >= 0 ; ii-- ){
      xx = xc*exp(-yy[ii]/a) ;            /* x transformed from y */
      l0 = log(xx) ; l1 = log(1.0-xx) ;   /* logarithms for Ipq sums */
      ff = pow(1.0-xx,b-1.0) ;            /* (1-x)**(b-1) */
      s00 += ww[ii] * ff ;                /* spq = Ipq sum */
      s10 += ww[ii] * ff * l0 ;
      s20 += ww[ii] * ff * l0 * l0 ;
      s01 += ww[ii] * ff * l1 ;
      s02 += ww[ii] * ff * l1 * l1 ;
      s11 += ww[ii] * ff * l0 * l1 ;
   }

   if( s00 <= 0.0 ) return -1 ;

   bi7[0] = s00 * pow(xc,a) / a ;           /* normalizer */
   bi7[1] = s10/s00 ;                       /* R0 */
   bi7[2] = s01/s00 ;                       /* R1 */
   bi7[3] = (s20*s00-s10*s10)/(s00*s00) ;   /* dR0/da */
   bi7[4] = (s11*s00-s10*s01)/(s00*s00) ;   /* dR0/db */
   bi7[5] = bi7[4] ;                        /* dR1/da */
   bi7[6] = (s02*s00-s01*s01)/(s00*s00) ;   /* dR1/db */

   return 0 ;
}

/*----------------------------------------------------------------------
   Set the range of values to search for beta distribution fit
------------------------------------------------------------------------*/

#define LL   0.2
#define UL   10000.0

static double AL   = 0.21 ;
static double AU   = 9.9 ;
static double BL   = 5.9 ;
static double BU   = 999.9 ;
static int    NRAN = 6666 ;

static void betarange( double al,double au , double bl , double bu , int nran )
{
   if( al > 0.0 ) AL = al ;
   if( au > AL  ) AU = au ;
   if( bl > 0.0 ) BL = bl ;
   if( bu > BL  ) BU = bu ;
   if( nran > 1 ) NRAN = nran ;
}

static double ainit=0.0 ;
static double binit=0.0 ;

static void beta_init( double ai , double bi )
{
   ainit = ai ; binit = bi ; return ;
}

/*--------------------------------------------------------------------
   Solve the two equations
     I10(a,b)/I00(a,b) = e0
     I01(a,b)/I00(a,b) = e1
   for (a,b), using 2D Newton's method.
----------------------------------------------------------------------*/

static int betasolve( double e0, double e1, double xc, double * ap, double * bp )
{
   double bi7[7] , aa,bb , da,db , m11,m12,m21,m22 , r1,r2 , dd,ee ;
   int nite=0 , ii,jj ;

   if( ap == NULL || bp == NULL ||
       xc <= 0.0  || xc >= 1.0  || e0 >= 0.0 || e1 >= 0.0 ) return -1 ;

   /* randomly search for a good starting point */

   dd = 1.e+20 ; aa = bb = 0.0 ;
   if( ainit > 0.0 && binit > 0.0 ){
      ii = bi7func( ainit , binit , xc , bi7 ) ;
      if( ii == 0 ){
         r1 = bi7[1]-e0; r2 = bi7[2]-e1; dd = fabs(r1/e0)+fabs(r2/e1);
         aa = ainit ; bb = binit ;
      }
   }

   dd = 1.e+20 ; aa = bb = 0.0 ;
   for( jj=0 ; jj < NRAN ; jj++ ){
      da = AL +(AU-AL) * drand48() ;
      db = BL +(BU-BL) * drand48() ;
      ii = bi7func( da , db , xc , bi7 ) ; if( ii ) continue ;
      r1 = bi7[1]-e0; r2 = bi7[2]-e1; ee = fabs(r1/e0)+fabs(r2/e1);
      if( ee < dd ){ aa=da ; bb=db ; dd=ee ; /*if(ee<0.05)break;*/ }
   }
   if( aa == 0.0 || bb == 0.0 ) return -1 ;
#if 0
   fprintf(stderr,"%2d: aa=%15.10g  bb=%15.10g  ee=%g\n",nite,aa,bb,ee) ;
#endif

   do{
      ii = bi7func( aa , bb , xc , bi7 ) ;
      if( ii ) return -1 ;
      r1  = bi7[1] - e0 ;
      r2  = bi7[2] - e1 ; ee = fabs(r1/e0) + fabs(r2/e1) ;
      m11 = bi7[3] ; m12 = bi7[4] ; m21 = bi7[5] ; m22 = bi7[6] ;
      dd  = m11*m22 - m12*m21 ;
      if( dd == 0.0 ) return -1 ;
      da = ( m22*r1 - m12*r2 ) / dd ;
      db = (-m21*r1 + m11*r2 ) / dd ;
      nite++ ;
      aa -= da ; bb -=db ;
#if 0
      if( aa < LL ) aa = LL ; else if( aa > UL ) aa = UL ;
      if( bb < LL ) bb = LL ; else if( bb > UL ) bb = UL ;

      if( aa == LL || bb == LL || aa == UL || bb == UL ) return -1 ;
#else
      if( aa < AL ) aa = AL ; else if( aa > AU ) aa = AU ;
      if( bb < BL ) bb = BL ; else if( bb > BU ) bb = BU ;
#endif

   } while( nite < 99 && fabs(da)+fabs(db) > 0.02 ) ;

   *ap = aa ; *bp = bb ; return 0 ;
}

/*--------------------------------------------------------------------*/

typedef struct {
  int mcount , ibot ;
  float * bval , * cval ;
} BFIT_data ;

typedef struct {
  int mgood , itop ;
  float a,b,xcut,chisq,df_chisq,q_chisq,eps ;
} BFIT_result ;

void BFIT_free_data( BFIT_data * bfd )
{
   if( bfd != NULL ){
      if( bfd->bval != NULL ) free(bfd->bval) ;
      if( bfd->cval != NULL ) free(bfd->cval) ;
      free(bfd) ;
   }
}

void BFIT_free_result( BFIT_result * bfr ){ if( bfr != NULL ) free(bfr); }

/*--------------------------------------------------------------------*/

BFIT_data * BFIT_bootstrap_sample( BFIT_data * bfd )
{
   BFIT_data * nfd ;
   int ii , jj , mcount,ibot , nuse ;

   if( bfd == NULL ) return NULL ;
   mcount = bfd->mcount ;
   ibot   = bfd->ibot   ;
   nuse   = mcount - ibot ;

   nfd = (BFIT_data *) malloc(sizeof(BFIT_data)) ;

   nfd->mcount = mcount ;
   nfd->ibot   = ibot ;
   nfd->bval   = (float *) malloc( sizeof(float) * mcount ) ;
   if( bfd->cval != NULL )
      nfd->cval = (float *) malloc( sizeof(float) * mcount ) ;
   else
      nfd->cval = NULL ;

   for( ii=0 ; ii < ibot ; ii++ ){
      nfd->bval[ii] = 0.0 ;
      if( nfd->cval != NULL ) nfd->cval[ii] = 0.0 ;
   }

   for( ii=ibot ; ii < mcount ; ii++ ){
      jj = ((lrand48()>>8) % nuse) + ibot ;
      nfd->bval[ii] = bfd->bval[jj] ;
      if( nfd->cval != NULL )
         nfd->cval[ii] = bfd->cval[jj] ;
   }

   if( nfd->cval != NULL )
      qsort_floatfloat( mcount , nfd->bval , nfd->cval ) ;
   else
      qsort_float( mcount , nfd->bval ) ;

   return nfd ;
}

/*--------------------------------------------------------------------*/

BFIT_data * BFIT_prepare_dataset(
                THD_3dim_dataset * input_dset , int ival , int sqr ,
                THD_3dim_dataset * mask_dset  , int miv  ,
                float mask_bot  , float mask_top            )
{
   int mcount , ii,jj , nvox,ibot ;
   byte * mmm ;
   BFIT_data * bfd ;
   float * bval , * cval ;

   /* check inputs */

   if( !ISVALID_DSET(input_dset)     ||
       ival < 0                      ||
       ival >= DSET_NVALS(input_dset)  ) return NULL ;

   nvox = DSET_NVOX(input_dset) ;

   if( ISVALID_DSET(mask_dset)          &&
       (miv < 0                      ||
        miv >= DSET_NVALS(mask_dset) ||
        DSET_NVOX(mask_dset) != nvox   )   ) return NULL ;

   DSET_load(input_dset) ;
   if( DSET_ARRAY(input_dset,ival) == NULL ) return NULL ;

   /* inputs are OK */

   /*-- build a byte mask array --*/

   if( mask_dset == NULL ){
      mmm = (byte *) malloc( sizeof(byte) * nvox ) ;
      memset( mmm , 1, nvox ) ; mcount = nvox ;
   } else {

      mmm = THD_makemask( mask_dset , miv , mask_bot , mask_top ) ;
      mcount = THD_countmask( nvox , mmm ) ;

      if( !EQUIV_DSETS(mask_dset,input_dset) ) DSET_unload(mask_dset) ;
      if( mcount < 999 ){
         free(mmm) ;
         fprintf(stderr,"*** BFIT_prepare_dataset:\n"
                        "***   only %d voxels survive the masking!\n",
                 mcount ) ;
         return NULL ;
      }
   }

   /*-- load values into bval --*/

   bval = (float *) malloc( sizeof(float) * mcount ) ;

   switch( DSET_BRICK_TYPE(input_dset,ival) ){

         case MRI_short:{
            short * bar = (short *) DSET_ARRAY(input_dset,ival) ;
            float mfac = DSET_BRICK_FACTOR(input_dset,ival) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            for( ii=jj=0 ; ii < nvox ; ii++ )
               if( mmm[ii] ) bval[jj++] = mfac*bar[ii] ;
         }
         break ;

         case MRI_byte:{
            byte * bar = (byte *) DSET_ARRAY(input_dset,ival) ;
            float mfac = DSET_BRICK_FACTOR(input_dset,ival) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            for( ii=jj=0 ; ii < nvox ; ii++ )
               if( mmm[ii] ) bval[jj++] = mfac*bar[ii] ;
         }
         break ;

         case MRI_float:{
            float * bar = (float *) DSET_ARRAY(input_dset,ival) ;
            float mfac = DSET_BRICK_FACTOR(input_dset,ival) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
               for( ii=jj=0 ; ii < nvox ; ii++ )
                  if( mmm[ii] ) bval[jj++] = mfac*bar[ii] ;
         }
         break ;
   }

   free(mmm) ; DSET_unload(input_dset) ;  /* don't need no more */

   /* correlation coefficients must be squared prior to betafit, */
   /* then R**2 values must be sorted.                           */

   if( sqr ){
      cval = (float *) malloc( sizeof(float) * mcount ) ;
      for( ii=0 ; ii < mcount ; ii++ ){
         cval[ii] = bval[ii] ;                /* save cc values */
         bval[ii] = bval[ii]*bval[ii] ;
      }
      qsort_floatfloat( mcount , bval , cval ) ;
   } else {                                   /* already squared */
      cval = NULL ;
      qsort_float( mcount , bval ) ;
   }

   /* check sorted values for legality */

   if( bval[mcount-1] > 1.0 ){
      free(bval) ; if(cval!=NULL) free(cval) ;
      fprintf(stderr,"*** BFIT_prepare_dataset:\n"
                     "***   R**2 values > 1.0 exist in dataset!\n") ;
      return NULL ;
   }
   if( bval[0] < 0.0 ){
      free(bval) ; if(cval!=NULL) free(cval) ;
      fprintf(stderr,"*** BFIT_prepare_dataset:\n"
                     "***   R**2 values < 0.0 exist in dataset!\n") ;
      return NULL ;
   }

   /* find 1st bval > 0 [we don't use 0 values] */

   for( ibot=0; ibot<mcount && bval[ibot]<=0.0; ibot++ ) ; /* nada */

   /* make output structure */

   bfd = (BFIT_data *) malloc( sizeof(BFIT_data) ) ;

   bfd->mcount = mcount ;
   bfd->ibot   = ibot ;
   bfd->bval   = bval ;
   bfd->cval   = cval ;

   return bfd ;
}

/*--------------------------------------------------------------------*/

BFIT_result * BFIT_compute( BFIT_data * bfd ,
                            float pcut ,
                            float abot , float atop ,
                            float bbot , float btop ,
                            int   nran , int   nbin  )
{
   BFIT_result * bfr ;

   float eps,eps1 ;
   float *bval , *cval ;
   double e0,e1 , aa,bb,xc ;
   double chq,ccc,cdf ;
   int    ihqbot,ihqtop ;
   int mcount,mgood , ii,jj , ibot,itop , sqr ;
   float hbot,htop,dbin ;
   int * hbin, * jbin ;
   MRI_IMAGE * flim ;

   /* mangle inputs */

   if( bfd == NULL ) return NULL ;
   if( pcut < 20.0 || pcut >  99.0 ) return NULL ;
   if( abot < 0.1  || abot >= atop ) return NULL ;
   if( bbot < 9.9  || bbot >= btop ) return NULL ;

   if( nran < 10 ) nran = 10 ;

   mcount = bfd->mcount ;
   ibot   = bfd->ibot ;
   bval   = bfd->bval ;
   cval   = bfd->cval ; sqr = (cval != NULL) ;

   /* now set the cutoff value (xc) */

   itop  = (int)( ibot + 0.01*pcut*(mcount-ibot) + 0.5 ) ;
   mgood = itop - ibot ;
   if( mgood < 999 ){
      fprintf(stderr,"*** BFIT_compute: mgood=%d\n",mgood) ;
      return NULL ;
   }

   xc = bval[itop-1] ;

   /* compute the statistics of the values in (0,xc] */

   e0 = e1 = 0.0 ;
   for( ii=ibot ; ii < itop ; ii++ ){
     e0 += log(bval[ii]) ; e1 += log(1.0-bval[ii]) ;
   }
   e0 /= mgood ; e1 /= mgood ;

   /* and solve for the best fit parameters (aa,bb) */

   betarange( abot , atop , bbot , btop ,  nran ) ;

   ii = betasolve( e0,e1,xc , &aa,&bb );
   if( ii < 0 ) return NULL ; /* error */

   /*+++ At this point, could do some bootstrap to
         estimate how good the estimates aa and bb are +++*/

   /* estimate of outlier fraction */

   eps1 = mgood / ( (mcount-ibot)*(1.0-beta_t2p(xc,aa,bb)) ) ;
   eps  = 1.0-eps1 ;
   if( eps1 > 1.0 ) eps1 = 1.0 ;
   eps1 = (mcount-ibot) * eps1 ;

   /*-- compute histogram and chi-square --*/

   if( nbin >= 100 ){  /* don't do it if nbin is too small */

#define NEW_HCQ
#ifdef NEW_HCQ    /* use new method */

     { float * xbin = (float *) malloc(sizeof(float)*nbin) ;

       hbin = (int *) calloc((nbin+1),sizeof(int)) ;  /* actual histogram */
       jbin = (int *) calloc((nbin+1),sizeof(int)) ;  /* theoretical fit */

       htop = 1.0 - beta_t2p(xc,aa,bb) ;   /* CDF at top */
       dbin = htop / nbin ;                /* d(CDF) for each bin */
       ii   = rint( eps1 * dbin ) ;
       for( jj=0 ; jj < nbin ; jj++ ){
          xbin[jj] = beta_p2t( 1.0 - (jj+1)*dbin , aa , bb ) ;
          jbin[jj] = ii ;
       }
       xbin[nbin-1] = xc ;

       for( ii=ibot ; ii < mcount ; ii++ ){
          for( jj=0 ; jj < nbin ; jj++ ){
             if( bval[ii] <= xbin[jj] ){ hbin[jj]++ ; break ; }
          }
       }

       free(xbin) ;

       ihqbot = 0 ;
       ihqtop = nbin-1 ;
     }

#else             /* use old method */

     /* original data was already squared (e.g., R**2 values) */

     if( !sqr ){
        hbot = 0.0 ; htop = 1.001 * xc ;
        dbin = (htop-hbot)/nbin ;

        hbin = (int *) calloc((nbin+1),sizeof(int)) ;  /* actual histogram */
        jbin = (int *) calloc((nbin+1),sizeof(int)) ;  /* theoretical fit */

        for( ii=0 ; ii < nbin ; ii++ ){  /* beta fit */
           jbin[ii] = (int)( eps1 * ( beta_t2p(hbot+ii*dbin,aa,bb)
                                     -beta_t2p(hbot+ii*dbin+dbin,aa,bb) ) ) ;
        }

        flim = mri_new_vol_empty( mcount-ibot,1,1 , MRI_float ) ;
        mri_fix_data_pointer( bval+ibot , flim ) ;
        mri_histogram( flim , hbot,htop , TRUE , nbin,hbin ) ;

        ihqbot = 0 ;
        ihqtop = rint( xc / dbin ) ;

     } else {   /* original data was not squared (e.g., correlations) */

        double hb,ht ;
        htop = sqrt(1.001*xc) ;
        hbot = -htop ;
        dbin = (htop-hbot)/nbin ;

        hbin = (int *) calloc((nbin+1),sizeof(int)) ;  /* actual histogram */
        jbin = (int *) calloc((nbin+1),sizeof(int)) ;  /* theoretical fit */

        for( ii=0 ; ii < nbin ; ii++ ){  /* beta fit */
           hb = hbot+ii*dbin ; ht = hb+dbin ;
           hb = hb*hb ; ht = ht*ht ;
           if( hb > ht ){ double qq=hb ; hb=ht ; ht=qq ; }
           jbin[ii] = (int)( 0.5*eps1 * ( beta_t2p(hb,aa,bb)
                                         -beta_t2p(ht,aa,bb) ) ) ;
        }

        ihqbot = rint( (-sqrt(xc) - hbot) / dbin ) ;
        ihqtop = rint( ( sqrt(xc) - hbot) / dbin ) ;

        flim = mri_new_vol_empty( mcount-ibot,1,1 , MRI_float ) ;
        mri_fix_data_pointer( cval+ibot , flim ) ;
        mri_histogram( flim , hbot,htop , TRUE , nbin,hbin ) ;

     }
#endif

   /* compute upper-tail probability of chi-square */

     chq = cdf = 0.0 ;
     for( ii=ihqbot ; ii <= ihqtop ; ii++ ){
        ccc = jbin[ii] ;
        if( ccc > 1.0 ){
           chq += SQR(hbin[ii]-ccc) / ccc ;
           cdf++ ;
        }
     }
     cdf -= 3.0 ;
     ccc = chisq_t2p( chq , cdf ) ;

#ifndef NEW_HCQ
     mri_clear_data_pointer(flim) ; mri_free(flim) ;
#endif
     free(hbin) ; free(jbin) ;

   } else {
      chq = ccc = cdf = 0.0 ;
   }

   bfr = (BFIT_result *) malloc(sizeof(BFIT_result)) ;

   bfr->mgood    = mgood ;
   bfr->itop     = itop  ;

   bfr->a        = aa  ;
   bfr->b        = bb  ;
   bfr->xcut     = xc  ;
   bfr->chisq    = chq ;
   bfr->q_chisq  = ccc ;
   bfr->df_chisq = cdf ;
   bfr->eps      = eps ;

   return bfr ;
}
