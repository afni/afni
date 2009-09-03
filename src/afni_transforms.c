#include "afni.h"

/*--------------- Sample 0D function: log10 of each input point ------------*/

void log10_func( int num , float *vec )
{
   int ii ;
   float vmax , vmin ;

   if( num <= 0 || vec == NULL ) return ;

   /* find largest element */

   vmax = vec[0] ;
   for( ii=1 ; ii < num ; ii++ ) vmax = MAX( vmax , vec[ii] ) ;

   /* if all are nonpositive, return all zeros */

   if( vmax <= 0.0 ){
      for( ii=0 ; ii < num ; ii++ ) vec[ii] = 0.0 ;
      return ;
   }

   /* find smallest positive element */

   vmin = vmax ;
   for( ii=0 ; ii < num ; ii++ )
      if( vec[ii] > 0.0 ) vmin = MIN( vmin , vec[ii] ) ;

   /* take log10 of each positive element;
      nonpositive elements get the log10 of vmin instead */

   vmin = log10(vmin) ;
   for( ii=0 ; ii < num ; ii++ )
      vec[ii] = (vec[ii] > 0.0) ? log10(vec[ii]) : vmin ;

   return ;
}

/*------------ Sample 0D function: Signed sqrt of each input point ---------*/

void ssqrt_func( int num , float *vec )
{
   int ii ;
   double val ;

   if( num <= 0 || vec == NULL ) return ;

   for( ii=0 ; ii < num ; ii++ ){
      val = sqrt(fabs(vec[ii])) ;                /* will be positive */
      vec[ii] = (vec[ii] >= 0.0) ? val : -val ;  /* output sign = input sign */
   }

   return ;
}

/*--------------- Sample 1D function: Order Statistics Filter -------------*/

void osfilt3_func( int num , double to,double dt, float *vec )
{
   int ii ;
   float aa,bb,cc ;

   bb = vec[0] ; cc = vec[1] ;
   for( ii=1 ; ii < num-1 ; ii++ ){
      aa = bb ; bb = cc ; cc = vec[ii+1] ;
      vec[ii] = OSFILT(aa,bb,cc) ;         /* see mrilib.h */
   }

   return ;
}

/*--------------- Sample 1D function: Median of 3 Filter ----------------*/

void median3_func( int num , double to,double dt, float *vec )
{
   int ii ;
   float aa,bb,cc ;

   bb = vec[0] ; cc = vec[1] ;
   for( ii=1 ; ii < num-1 ; ii++ ){
      aa = bb ; bb = cc ; cc = vec[ii+1] ;
      vec[ii] = MEDIAN(aa,bb,cc) ;         /* see mrilib.h */
   }

   return ;
}

/*---------------- Sample 1D function: abs(FFT) [30 Jun 2000] --------------*/

void absfft_func( int num , double to,double dt, float *vec )
{
   static complex *cx=NULL ;
   static int      ncx=0 , numold=0 ;
   float f0,f1 ;
   int ii ;

   if( num < 2 ) return ;
   if( num > numold ){
      numold = num ;
      ncx    = csfft_nextup(numold) ;
      if( cx != NULL ) free(cx) ;
      cx = (complex *) malloc(sizeof(complex)*ncx) ;
   }

   get_linear_trend( num , vec , &f0,&f1 ) ;  /* thd_detrend.c */

   for( ii=0 ; ii < num ; ii++ ){ cx[ii].r = vec[ii]-(f0+f1*ii); cx[ii].i = 0.0; }
   for(      ; ii < ncx ; ii++ ){ cx[ii].r = cx[ii].i = 0.0 ; }

   csfft_cox( -1 , ncx , cx ) ;               /* csfft.c */

   vec[0] = 0.0 ;
   for( ii=1 ; ii < num ; ii++ ) vec[ii] = CABS(cx[ii]) ;

   return ;
}

/*---------------- Sample 1D function: 0..1 scaling [02 Sep 2009] ----------*/

void ztone_func( int num , double to,double dt, float *vec )
{
   int ii ; float vbot,vtop ;

   if( num < 2 || vec == NULL ) return ;
   vbot = vtop = vec[0] ;
   for( ii=1 ; ii < num ; ii++ ){
          if( vec[ii] < vbot ) vbot = vec[ii] ;
     else if( vec[ii] > vtop ) vtop = vec[ii] ;
   }
   if( vbot == vtop ){
     for( ii=0 ; ii < num ; ii++ ) vec[ii] = 0.5f ;
   } else {
     float fac = 1.0f / (vtop-vbot) ;
     for( ii=0 ; ii < num ; ii++ ) vec[ii] = (vec[ii]-vbot)*fac ;
   }

   return ;
}

/*---------------- Sample 1D function:  L1 normalize [03 Sep 2009] ----------*/

void L1normalize_func( int num , double to,double dt, float *vec )
{
   int ii ; float vsum ;

   if( num < 2 || vec == NULL ) return ;

   vsum = 0.0f ;
   for( ii=0 ; ii < num ; ii++ ) vsum += fabsf(vec[ii]) ;
   if( vsum == 0.0f ) return ;
   vsum = 1.0f / vsum ;
   for( ii=0 ; ii < num ; ii++ ) vec[ii] *= vsum ;
   return ;
}

/*---------------- Sample 1D function:  L2 normalize [03 Sep 2009] ----------*/

void L2normalize_func( int num , double to,double dt, float *vec )
{
   int ii ; float vsum ;

   if( num < 2 || vec == NULL ) return ;

   vsum = 0.0f ;
   for( ii=0 ; ii < num ; ii++ ) vsum += vec[ii]*vec[ii] ;
   if( vsum <= 0.0f ) return ;
   vsum = 1.0f / sqrtf(vsum) ;
   for( ii=0 ; ii < num ; ii++ ) vec[ii] *= vsum ;
   return ;
}

/*----------- Sample slice projection functions [31 Jan 2002] -----------*/

float min_proj( int n , float *ar )
{
   float v = ar[0] ;
   int ii ;
   for( ii=1 ; ii < n ; ii++ ) if( v > ar[ii] ) v = ar[ii] ;
   return v ;
}

float max_proj( int n , float *ar )
{
   float v = ar[0] ;
   int ii ;
   for( ii=1 ; ii < n ; ii++ ) if( v < ar[ii] ) v = ar[ii] ;
   return v ;
}

float mean_proj( int n , float *ar )
{
   float v=0.0 ;
   int ii ;
   for( ii=0 ; ii < n ; ii++ ) v += ar[ii] ;
   return (v/n) ;
}

float extreme_proj( int n , float *ar )  /* 02 Feb 2002 */
{
   float vv,ww , med=qmed_float(n,ar) ;
   int ii , jj ;

   jj = 0 ; vv = fabs(ar[0]-med) ;      /* Find the value */
   for( ii=1 ; ii < n ; ii++ ){         /* furthest from */
     ww = fabs(ar[ii]-med) ;            /* the median.  */
     if( ww > vv ){ vv=ww; jj=ii; }
   }
   return ar[jj] ;
}

float osfilt_proj( int n , float *ar )  /* 07 Dec 2007 */
{
   int ii , n2 ; float v=0.0f , d=0.0f ;
   qsort_float( n , ar ) ;
   n2 = n/2 ;
   for( ii=0 ; ii < n2 ; ii++ ){
     v += (ii+1)*(ar[ii]+ar[n-1-ii]) ;
     d += 2*(ii+1) ;
   }
   v += (n2+1)*ar[n2] ; d += (n2+1) ;
   return (v/d) ;
}

float mad_proj( int n , float *ar )  /* 07 Dec 2007 */
{
   float v ;
   qmedmad_float( n , ar , NULL , &v ) ; return v ;
}

/*======================================================================*/
/*----------------------- Sample 2D transformations --------------------*/
/*======================================================================*/

static float *atemp = NULL ;
static int   natemp = -666 ;

#define MAKE_ATEMP(nvox)                     \
  do{ if( natemp < (nvox) ){                 \
         if( atemp != NULL ) free(atemp) ;   \
         natemp = (nvox) ;                   \
         atemp  = (float *) malloc( sizeof(float) * natemp ) ; } } while(0)

#define AT(i,j) atemp[(i)+(j)*nx]

/*------------------------------------------------------------------------*/

void median9_box_func( int nx , int ny , double dx, double dy, float *ar )
{
   int ii , jj , nxy , joff ;
   float aa[9] ;
   float *ajj , *ajm , *ajp ;

   if( nx < 3 || ny < 3 ) return ;

   /** make space and copy input into it **/

   nxy = nx * ny ;
   MAKE_ATEMP(nxy) ; if( atemp == NULL ) return ;
   memcpy(atemp,ar,sizeof(float)*nxy) ;

   /** process copy of input back into the input array **/

   for( jj=0 ; jj < ny ; jj++ ){

      joff = jj * nx ;      /* offset into this row */
      ajj  = atemp + joff ; /* pointer to this row */

      ajm  = (jj==0   ) ? ajj : ajj-nx ;  /* pointer to last row */
      ajp  = (jj==ny-1) ? ajj : ajj+nx ;  /* pointer to next row */

      /* do interior points of this row */

      for( ii=1 ; ii < nx-1 ; ii++ ){
         aa[0] = ajm[ii-1] ; aa[1] = ajm[ii] ; aa[2] = ajm[ii+1] ;
         aa[3] = ajj[ii-1] ; aa[4] = ajj[ii] ; aa[5] = ajj[ii+1] ;
         aa[6] = ajp[ii-1] ; aa[7] = ajp[ii] ; aa[8] = ajp[ii+1] ;
#if 0
         isort_float( 9 , aa ) ; ar[ii+joff] = aa[4] ;
#else
         ar[ii+joff] = qmed_float(9,aa) ;  /* 25 Oct 2000 */
#endif
      }

      /* do leading edge point (ii=0) */

      aa[0] = ajm[0] ; aa[1] = ajm[0] ; aa[2] = ajm[1] ;
      aa[3] = ajj[0] ; aa[4] = ajj[0] ; aa[5] = ajj[1] ;
      aa[6] = ajp[0] ; aa[7] = ajp[0] ; aa[8] = ajp[1] ;
#if 0
      isort_float( 9 , aa ) ; ar[joff] = aa[4] ;
#else
      ar[joff] = qmed_float(9,aa) ;  /* 25 Oct 2000 */
#endif

      /* do trailing edge point (ii=nx-1) */

      aa[0] = ajm[nx-2] ; aa[1] = ajm[nx-1] ; aa[2] = ajm[nx-1] ;
      aa[3] = ajj[nx-2] ; aa[4] = ajj[nx-1] ; aa[5] = ajj[nx-1] ;
      aa[6] = ajp[nx-2] ; aa[7] = ajp[nx-1] ; aa[8] = ajp[nx-1] ;
#if 0
      isort_float( 9 , aa ) ; ar[nx-1+joff] = aa[4] ;
#else
      ar[nx-1+joff] = qmed_float(9,aa) ;  /* 25 Oct 2000 */
#endif
   }
   return ;
}

/*------------------------------------------------------------------------*/

void winsor9_box_func( int nx , int ny , double dx, double dy, float *ar )
{
   int ii , jj , nxy , joff ;
   float aa[9] ;
   float *ajj , *ajm , *ajp ;

   if( nx < 3 || ny < 3 ) return ;

   /** make space and copy input into it **/

   nxy = nx * ny ;
   MAKE_ATEMP(nxy) ; if( atemp == NULL ) return ;
   memcpy(atemp,ar,sizeof(float)*nxy) ;

   /** process copy of input back into the input array **/

   for( jj=0 ; jj < ny ; jj++ ){

      joff = jj * nx ;      /* offset into this row */
      ajj  = atemp + joff ; /* pointer to this row */

      ajm  = (jj==0   ) ? ajj : ajj-nx ;  /* pointer to last row */
      ajp  = (jj==ny-1) ? ajj : ajj+nx ;  /* pointer to next row */

      /* do interior points of this row */

      for( ii=1 ; ii < nx-1 ; ii++ ){
         aa[0] = ajm[ii-1] ; aa[1] = ajm[ii] ; aa[2] = ajm[ii+1] ;
         aa[3] = ajj[ii-1] ; aa[4] = ajj[ii] ; aa[5] = ajj[ii+1] ;
         aa[6] = ajp[ii-1] ; aa[7] = ajp[ii] ; aa[8] = ajp[ii+1] ;
         isort_float( 9 , aa ) ;
              if( ar[ii+joff] < aa[2] ) ar[ii+joff] = aa[2] ;
         else if( ar[ii+joff] > aa[6] ) ar[ii+joff] = aa[6] ;
      }

      /* do leading edge point (ii=0) */

      aa[0] = ajm[0] ; aa[1] = ajm[0] ; aa[2] = ajm[1] ;
      aa[3] = ajj[0] ; aa[4] = ajj[0] ; aa[5] = ajj[1] ;
      aa[6] = ajp[0] ; aa[7] = ajp[0] ; aa[8] = ajp[1] ;
      isort_float( 9 , aa ) ;
           if( ar[joff] < aa[2] ) ar[joff] = aa[2] ;
      else if( ar[joff] > aa[6] ) ar[joff] = aa[6] ;

      /* do trailing edge point (ii=nx-1) */

      aa[0] = ajm[nx-2] ; aa[1] = ajm[nx-1] ; aa[2] = ajm[nx-1] ;
      aa[3] = ajj[nx-2] ; aa[4] = ajj[nx-1] ; aa[5] = ajj[nx-1] ;
      aa[6] = ajp[nx-2] ; aa[7] = ajp[nx-1] ; aa[8] = ajp[nx-1] ;
      isort_float( 9 , aa ) ;
           if( ar[nx-1+joff] < aa[2] ) ar[nx-1+joff] = aa[2] ;
      else if( ar[nx-1+joff] > aa[6] ) ar[nx-1+joff] = aa[6] ;
   }
   return ;
}

/*------------------------------------------------------------------------*/

void osfilt9_box_func( int nx , int ny , double dx, double dy, float *ar )
{
   int ii , jj , nxy , joff ;
   float aa[9] ;
   float *ajj , *ajm , *ajp ;

   if( nx < 3 || ny < 3 ) return ;

   /** make space and copy input into it **/

   nxy = nx * ny ;
   MAKE_ATEMP(nxy) ; if( atemp == NULL ) return ;
   memcpy(atemp,ar,sizeof(float)*nxy) ;

   /** process copy of input back into the input array **/

   for( jj=0 ; jj < ny ; jj++ ){

      joff = jj * nx ;      /* offset into this row */
      ajj  = atemp + joff ; /* pointer to this row */

      ajm  = (jj==0   ) ? ajj : ajj-nx ;  /* pointer to last row */
      ajp  = (jj==ny-1) ? ajj : ajj+nx ;  /* pointer to next row */

      /* do interior points of this row */

#undef  OSUM
#define OSUM(a,b,c,d,e) ( 0.1*((a)+(e)) + 0.2*((b)+(d)) + 0.4*(c) )

      for( ii=1 ; ii < nx-1 ; ii++ ){
         aa[0] = ajm[ii-1] ; aa[1] = ajm[ii] ; aa[2] = ajm[ii+1] ;
         aa[3] = ajj[ii-1] ; aa[4] = ajj[ii] ; aa[5] = ajj[ii+1] ;
         aa[6] = ajp[ii-1] ; aa[7] = ajp[ii] ; aa[8] = ajp[ii+1] ;
         isort_float( 9 , aa ) ;
         ar[ii+joff] = OSUM( aa[2],aa[3],aa[4],aa[5],aa[6] ) ;
      }

      /* do leading edge point (ii=0) */

      aa[0] = ajm[0] ; aa[1] = ajm[0] ; aa[2] = ajm[1] ;
      aa[3] = ajj[0] ; aa[4] = ajj[0] ; aa[5] = ajj[1] ;
      aa[6] = ajp[0] ; aa[7] = ajp[0] ; aa[8] = ajp[1] ;
      isort_float( 9 , aa ) ;
      ar[joff] = OSUM( aa[2],aa[3],aa[4],aa[5],aa[6] ) ;

      /* do trailing edge point (ii=nx-1) */

      aa[0] = ajm[nx-2] ; aa[1] = ajm[nx-1] ; aa[2] = ajm[nx-1] ;
      aa[3] = ajj[nx-2] ; aa[4] = ajj[nx-1] ; aa[5] = ajj[nx-1] ;
      aa[6] = ajp[nx-2] ; aa[7] = ajp[nx-1] ; aa[8] = ajp[nx-1] ;
      isort_float( 9 , aa ) ;
      ar[nx-1+joff] = OSUM( aa[2],aa[3],aa[4],aa[5],aa[6] ) ;
   }
   return ;
}

/*------------------------------------------------------------------------*/

void median21_box_func( int nx , int ny , double dx, double dy, float *ar )
{
   int ii , jj , nxy , joff ;
   float aa[21] ;
   float *ajj , *ajm , *ajp , *ajmm , *ajpp ;

   if( nx < 5 || ny < 5 ) return ;

   /** make space and copy input into it **/

   nxy = nx * ny ;
   MAKE_ATEMP(nxy) ; if( atemp == NULL ) return ;
   memcpy(atemp,ar,sizeof(float)*nxy) ;

   /** process copy of input back into the input array **/

   for( jj=1 ; jj < ny-1 ; jj++ ){

      joff = jj * nx ;      /* offset into this row */
      ajj  = atemp + joff ; /* pointer to this row */

      ajm  = ajj-nx ;  /* pointer to last row */
      ajp  = ajj+nx ;  /* pointer to next row */

      ajmm = (jj == 1  ) ? ajm : ajm-nx ;  /* to last last row */
      ajpp = (jj ==ny-2) ? ajp : ajp+nx ;  /* to next next row */

      /* do interior points of this row */

      for( ii=2 ; ii < nx-2 ; ii++ ){
         aa[0]=ajmm[ii-1]; aa[1]=ajmm[ii]; aa[2]=ajmm[ii+1];

         aa[ 3]=ajm[ii-2]; aa[ 4]=ajm[ii-1]; aa[ 5]=ajm[ii]; aa[ 6]=ajm[ii+1]; aa[ 7]=ajm[ii+2];
         aa[ 8]=ajj[ii-2]; aa[ 9]=ajj[ii-1]; aa[10]=ajj[ii]; aa[11]=ajj[ii+1]; aa[12]=ajj[ii+2];
         aa[13]=ajp[ii-2]; aa[14]=ajp[ii-1]; aa[15]=ajp[ii]; aa[16]=ajp[ii+1]; aa[17]=ajp[ii+2];

         aa[18]=ajpp[ii-1]; aa[19]=ajpp[ii]; aa[20]=ajpp[ii+1];

#if 0
         isort_float( 21 , aa ) ; ar[ii+joff] = aa[10] ;
#else
         ar[ii+joff] = qmed_float(21,aa) ; /* 25 Oct 2000 */
#endif
      }

   }
   return ;
}

/*------------------------------------------------------------------------*/

void winsor21_box_func( int nx , int ny , double dx, double dy, float *ar )
{
   int ii , jj , nxy , joff ;
   float aa[21] ;
   float *ajj , *ajm , *ajp , *ajmm , *ajpp ;

   static int kbot=-1 , ktop ;

   if( nx < 5 || ny < 5 ) return ;

   /** initialize cutoffs [07 Dec 1999] **/

   if( kbot < 0 ){
      char *ee = my_getenv("AFNI_WINSOR21_CUTOFF") ;
      kbot = 6 ;   /* default */
      if( ee != NULL ){
         ii = strtol( ee , NULL , 10 ) ;
         if( ii > 0 && ii < 10 ) kbot = ii ;
      }
      ktop = 20 - kbot ;
   }

   /** make space and copy input into it **/

   nxy = nx * ny ;
   MAKE_ATEMP(nxy) ; if( atemp == NULL ) return ;
   memcpy(atemp,ar,sizeof(float)*nxy) ;

   /** process copy of input back into the input array **/

   for( jj=1 ; jj < ny-1 ; jj++ ){

      joff = jj * nx ;      /* offset into this row */
      ajj  = atemp + joff ; /* pointer to this row */

      ajm  = ajj-nx ;  /* pointer to last row */
      ajp  = ajj+nx ;  /* pointer to next row */

      ajmm = (jj == 1  ) ? ajm : ajm-nx ;  /* to last last row */
      ajpp = (jj ==ny-2) ? ajp : ajp+nx ;  /* to next next row */

      /* do interior points of this row */

      for( ii=2 ; ii < nx-2 ; ii++ ){
         aa[0]=ajmm[ii-1]; aa[1]=ajmm[ii]; aa[2]=ajmm[ii+1];

         aa[ 3]=ajm[ii-2]; aa[ 4]=ajm[ii-1]; aa[ 5]=ajm[ii]; aa[ 6]=ajm[ii+1]; aa[ 7]=ajm[ii+2];
         aa[ 8]=ajj[ii-2]; aa[ 9]=ajj[ii-1]; aa[10]=ajj[ii]; aa[11]=ajj[ii+1]; aa[12]=ajj[ii+2];
         aa[13]=ajp[ii-2]; aa[14]=ajp[ii-1]; aa[15]=ajp[ii]; aa[16]=ajp[ii+1]; aa[17]=ajp[ii+2];

         aa[18]=ajpp[ii-1]; aa[19]=ajpp[ii]; aa[20]=ajpp[ii+1];

         isort_float( 21 , aa ) ;

              if( ar[ii+joff] < aa[kbot] ) ar[ii+joff] = aa[kbot] ;
         else if( ar[ii+joff] > aa[ktop] ) ar[ii+joff] = aa[ktop] ;
      }

   }
   return ;
}

/*------- [30 Jun 2000: abs(2D FFT) function] ----------------------------*/

void fft2D_absfunc( int nx , int ny , double dx, double dy, float *ar )
{
   complex *cxar , *cpt ;
   int nxup,nyup , ii,jj ;
   float fi,fj , *fpt ;

   if( nx < 5 || ny < 5 ) return ;

   nxup = csfft_nextup_one35(nx) ;  /* get FFT size */
   nyup = csfft_nextup_one35(ny) ;

   cxar = (complex *) malloc(sizeof(complex)*nxup*nyup) ;

   /* copy input to output, sign-alternating and zero-padding along the way */

   cpt = cxar ;
   fpt = ar   ;
   fj  = 1.0  ;
   for( jj=0 ; jj < ny ; jj++ ){
      fi = fj ; fj = -fj ;
      for(ii=0; ii<nx  ; ii++){cpt->r=*fpt*fi; cpt->i=0.0; cpt++;fpt++;fi=-fi;}
      for(    ; ii<nxup; ii++){cpt->r=cpt->i=0.0; cpt++;}
   }
   for( ; jj < nyup ; jj++ ){cpt->r=cpt->i=0.0; cpt++;}

   /* row FFTs */

   for( jj=0 ; jj < ny ; jj++ )
      csfft_cox( -1 , nxup , cxar+jj*nxup ) ;

   /* column FFTs */

   cpt = (complex *) malloc(sizeof(complex)*nyup) ;

   for( ii=0 ; ii < nxup ; ii++ ){
      for( jj=0 ; jj < nyup ; jj++ ) cpt[jj] = cxar[ii+jj*nxup] ;
      csfft_cox( -1 , nyup , cpt ) ;
      for( jj=0 ; jj < nyup ; jj++ ) cxar[ii+jj*nxup] = cpt[jj] ;
   }

   /* copy to output */

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         ar[ii+jj*nx] = CABS(cxar[ii+jj*nxup]) ;

   free(cxar) ; free(cpt) ; return ;
}

/*------- [26 Mar 2008: arg(2D FFT) function] ----------------------------*/

void fft2D_phasefunc( int nx , int ny , double dx, double dy, float *ar )
{
   complex *cxar , *cpt ;
   int nxup,nyup , ii,jj ;
   float fi,fj , *fpt ;

   if( nx < 5 || ny < 5 ) return ;

   nxup = csfft_nextup_one35(nx) ;  /* get FFT size */
   nyup = csfft_nextup_one35(ny) ;

   cxar = (complex *) malloc(sizeof(complex)*nxup*nyup) ;

   /* copy input to output, sign-alternating and zero-padding along the way */

   cpt = cxar ;
   fpt = ar   ;
   fj  = 1.0  ;
   for( jj=0 ; jj < ny ; jj++ ){
      fi = fj ; fj = -fj ;
      for(ii=0; ii<nx  ; ii++){cpt->r=*fpt*fi; cpt->i=0.0; cpt++;fpt++;fi=-fi;}
      for(    ; ii<nxup; ii++){cpt->r=cpt->i=0.0; cpt++;}
   }
   for( ; jj < nyup ; jj++ ){cpt->r=cpt->i=0.0; cpt++;}

   /* row FFTs */

   for( jj=0 ; jj < ny ; jj++ )
      csfft_cox( -1 , nxup , cxar+jj*nxup ) ;

   /* column FFTs */

   cpt = (complex *) malloc(sizeof(complex)*nyup) ;

   for( ii=0 ; ii < nxup ; ii++ ){
      for( jj=0 ; jj < nyup ; jj++ ) cpt[jj] = cxar[ii+jj*nxup] ;
      csfft_cox( -1 , nyup , cpt ) ;
      for( jj=0 ; jj < nyup ; jj++ ) cxar[ii+jj*nxup] = cpt[jj] ;
   }

   /* copy to output */

   for( jj=0 ; jj < ny ; jj++ )
      for( ii=0 ; ii < nx ; ii++ )
         ar[ii+jj*nx] = CARG(cxar[ii+jj*nxup]) ;

   free(cxar) ; free(cpt) ; return ;
}
