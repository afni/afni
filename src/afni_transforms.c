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

/*--------------------------------------------------------------------------*/
/* adaptive = downweight things a long ways from median */

float adaptive_weighted_mean( int num , float *x )
{
   float med,mad, wt,wsum, xsum ; int ii ;

        if( num <= 0 || x == NULL ) return (0.0f) ;
   else if( num == 1              ) return (x[0]) ;
   else if( num == 2              ) return (0.5f*(x[0]+x[1])) ;

   qmedmad_float( num , x , &med , &mad ) ;
   if( mad <= 0.0f ) return (med) ;

   wsum = xsum = 0.0f ; mad = 0.56789f / mad ;
   for( ii=0 ; ii < num ; ii++ ){
     wt = mad*fabsf(x[ii]-med); wt = 1.0f / (1.0f+wt*wt*wt); wsum += wt;
     xsum += wt * x[ii] ;
   }
   return (xsum/wsum) ;
}

/*--------------------------------------------------------------------------*/
/* 1D adaptive filter 9 points wide */

void adpt_wt_mn9( int num , double to,double dt, float *vec )
{
   float x[9] , *nv ; int ii,jj,kk , n1=num-1 ;

   nv = (float *)malloc(sizeof(float)*num) ;

   for( ii=0 ; ii < num ; ii++ ){

     for( jj=-4 ; jj <= 4 ; jj++ ){
       kk = ii+jj ; if( kk < 0 ) kk = 0 ; else if( kk > n1 ) kk = n1 ;
       x[jj+4] = vec[kk] ;
     }

     nv[ii] = adaptive_weighted_mean( 9 , x ) ;
   }

   memcpy(vec,nv,sizeof(float)*num) ; free(nv) ; return ;
}

/*--------------------------------------------------------------------------*/
/* 1D adaptive filter 19 points wide */

void adpt_wt_mn19( int num , double to,double dt, float *vec )
{
   float x[19] , *nv ; int ii,jj,kk , n1=num-1 ;

   nv = (float *)malloc(sizeof(float)*num) ;

   for( ii=0 ; ii < num ; ii++ ){

     for( jj=-9 ; jj <= 9 ; jj++ ){
       kk = ii+jj ; if( kk < 0 ) kk = 0 ; else if( kk > n1 ) kk = n1 ;
       x[jj+9] = vec[kk] ;
     }

     nv[ii] = adaptive_weighted_mean( 19 , x ) ;
   }

   memcpy(vec,nv,sizeof(float)*num) ; free(nv) ; return ;
}

/*--------------------------------------------------------------------------*/
/* 1D adaptive filter user-initialized points wide (must be odd) */

void adpt_wt_mnXX( int num , double to,double dt, float *vec )
{
   static int nXX=0,nHH=0 ; static float *XX=NULL ;
   float *nv ; int ii,jj,kk , n1=num-1 ;

   if( vec == NULL ){
     nXX = num ; nHH = nXX/2 ;
     XX  = (float *)malloc(sizeof(float)*nXX) ;
     return ;
   }

   nv = (float *)malloc(sizeof(float)*num) ;

   for( ii=0 ; ii < num ; ii++ ){

     for( jj=-nHH ; jj <= nHH ; jj++ ){
       kk = ii+jj ; if( kk < 0 ) kk = 0 ; else if( kk > n1 ) kk = n1 ;
       XX[jj+nHH] = vec[kk] ;
     }

     nv[ii] = adaptive_weighted_mean( nXX , XX ) ;
   }

   memcpy(vec,nv,sizeof(float)*num) ; free(nv) ; return ;
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

/*-------- Sample 1D function: Despike7 Filter [07 Oct 2010] ----------*/

#define MTHR 6.789f  /* threshold parameter */

#if 0
#undef  mmm7
#define mmm7(j)                                            \
 { float qqq[7] ; int jj = (j)-3 ;                         \
   if( jj < 0 ) jj = 0; else if( jj+7 > num ) jj = num-7;  \
   memcpy(qqq,vec+jj,sizeof(float)*7) ;                    \
   med    = qmed_float(7,qqq); qqq[0] = fabsf(qqq[0]-med); \
   qqq[1] = fabsf(qqq[1]-med); qqq[2] = fabsf(qqq[2]-med); \
   qqq[3] = fabsf(qqq[3]-med); qqq[4] = fabsf(qqq[4]-med); \
   qqq[5] = fabsf(qqq[5]-med); qqq[6] = fabsf(qqq[6]-med); \
   mad    = qmed_float(7,qqq); }

void despike7_func( int num , double to,double dt , float *vec )
{
   int ii ; float *zma,*zme , med,mad,val ;

   if( num < 7 ) return ;
   zme = (float *)malloc(sizeof(float)*num) ;
   zma = (float *)malloc(sizeof(float)*num) ;

   for( ii=0 ; ii < num ; ii++ ){
     mmm7(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ; free(zma) ;
   if( mad <= 0.0f ){ free(zme) ; return ; }  /* should not happen */
   mad *= MTHR ;  /* threshold value */

   for( ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ) vec[ii] = zme[ii] ;

   free(zme) ; return ;
}
#undef mmm7
#endif

/** this next func doesn't work well -- don't include it in AFNI **/
#if 0
void despike7pp_func( int num , double to,double dt , float *vec )
{
   int ii ; float *dvv ;

   despike7_func(num,to,dt,vec) ;
   if( num < 9 ) return ;
   dvv = malloc(sizeof(float)*num) ;
   for( ii=1 ; ii < num ; ii++ ) dvv[ii-1] = vec[ii]  -vec[ii-1] ;
   despike7_func( num-1 , to,dt , dvv ) ;
   for( ii=1 ; ii < num ; ii++ ) vec[ii]   = vec[ii-1]+dvv[ii-1] ;
   free(dvv) ; return ;
}
#endif

/*-------- Sample 1D function: Despike9 Filter [08 Oct 2010] ----------*/

#undef  mmm9
#define mmm9(j)                                            \
 { float qqq[9] ; int jj = (j)-4 ;                         \
   if( jj < 0 ) jj = 0; else if( jj+9 > num ) jj = num-9;  \
   memcpy(qqq,vec+jj,sizeof(float)*9) ;                    \
   med    = qmed_float(9,qqq); qqq[0] = fabsf(qqq[0]-med); \
   qqq[1] = fabsf(qqq[1]-med); qqq[2] = fabsf(qqq[2]-med); \
   qqq[3] = fabsf(qqq[3]-med); qqq[4] = fabsf(qqq[4]-med); \
   qqq[5] = fabsf(qqq[5]-med); qqq[6] = fabsf(qqq[6]-med); \
   qqq[7] = fabsf(qqq[7]-med); qqq[8] = fabsf(qqq[8]-med); \
   mad    = qmed_float(9,qqq); }

void despike9_func( int num , double to,double dt , float *vec )
{
   int ii ; float *zma,*zme , med,mad,val ;

   if( num < 9 ) return ;
   zme = (float *)malloc(sizeof(float)*num) ;
   zma = (float *)malloc(sizeof(float)*num) ;

   for( ii=0 ; ii < num ; ii++ ){
     mmm9(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ; free(zma) ;
   if( mad <= 0.0f ){ free(zme) ; return ; }  /* should not happen */
   mad *= MTHR ;  /* threshold value */

   for( ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ) vec[ii] = zme[ii] ;

   free(zme) ; return ;
}
#undef mmm9

/*----------------------------------------------------------------------------*/
/* Despiking-25 filter */

#undef  SWAP
#define SWAP(x,y) (temp=x,x=y,y=temp)
#undef  SORT2
#define SORT2(a,b) if(a>b) SWAP(a,b)

static INLINE float median25f(float *p)
{
    register float temp ;
    SORT2(p[0], p[1]) ;   SORT2(p[3], p[4]) ;   SORT2(p[2], p[4]) ;
    SORT2(p[2], p[3]) ;   SORT2(p[6], p[7]) ;   SORT2(p[5], p[7]) ;
    SORT2(p[5], p[6]) ;   SORT2(p[9], p[10]) ;  SORT2(p[8], p[10]) ;
    SORT2(p[8], p[9]) ;   SORT2(p[12], p[13]) ; SORT2(p[11], p[13]) ;
    SORT2(p[11], p[12]) ; SORT2(p[15], p[16]) ; SORT2(p[14], p[16]) ;
    SORT2(p[14], p[15]) ; SORT2(p[18], p[19]) ; SORT2(p[17], p[19]) ;
    SORT2(p[17], p[18]) ; SORT2(p[21], p[22]) ; SORT2(p[20], p[22]) ;
    SORT2(p[20], p[21]) ; SORT2(p[23], p[24]) ; SORT2(p[2], p[5]) ;
    SORT2(p[3], p[6]) ;   SORT2(p[0], p[6]) ;   SORT2(p[0], p[3]) ;
    SORT2(p[4], p[7]) ;   SORT2(p[1], p[7]) ;   SORT2(p[1], p[4]) ;
    SORT2(p[11], p[14]) ; SORT2(p[8], p[14]) ;  SORT2(p[8], p[11]) ;
    SORT2(p[12], p[15]) ; SORT2(p[9], p[15]) ;  SORT2(p[9], p[12]) ;
    SORT2(p[13], p[16]) ; SORT2(p[10], p[16]) ; SORT2(p[10], p[13]) ;
    SORT2(p[20], p[23]) ; SORT2(p[17], p[23]) ; SORT2(p[17], p[20]) ;
    SORT2(p[21], p[24]) ; SORT2(p[18], p[24]) ; SORT2(p[18], p[21]) ;
    SORT2(p[19], p[22]) ; SORT2(p[8], p[17]) ;  SORT2(p[9], p[18]) ;
    SORT2(p[0], p[18]) ;  SORT2(p[0], p[9]) ;   SORT2(p[10], p[19]) ;
    SORT2(p[1], p[19]) ;  SORT2(p[1], p[10]) ;  SORT2(p[11], p[20]) ;
    SORT2(p[2], p[20]) ;  SORT2(p[2], p[11]) ;  SORT2(p[12], p[21]) ;
    SORT2(p[3], p[21]) ;  SORT2(p[3], p[12]) ;  SORT2(p[13], p[22]) ;
    SORT2(p[4], p[22]) ;  SORT2(p[4], p[13]) ;  SORT2(p[14], p[23]) ;
    SORT2(p[5], p[23]) ;  SORT2(p[5], p[14]) ;  SORT2(p[15], p[24]) ;
    SORT2(p[6], p[24]) ;  SORT2(p[6], p[15]) ;  SORT2(p[7], p[16]) ;
    SORT2(p[7], p[19]) ;  SORT2(p[13], p[21]) ; SORT2(p[15], p[23]) ;
    SORT2(p[7], p[13]) ;  SORT2(p[7], p[15]) ;  SORT2(p[1], p[9]) ;
    SORT2(p[3], p[11]) ;  SORT2(p[5], p[17]) ;  SORT2(p[11], p[17]) ;
    SORT2(p[9], p[17]) ;  SORT2(p[4], p[10]) ;  SORT2(p[6], p[12]) ;
    SORT2(p[7], p[14]) ;  SORT2(p[4], p[6]) ;   SORT2(p[4], p[7]) ;
    SORT2(p[12], p[14]) ; SORT2(p[10], p[14]) ; SORT2(p[6], p[7]) ;
    SORT2(p[10], p[12]) ; SORT2(p[6], p[10]) ;  SORT2(p[6], p[17]) ;
    SORT2(p[12], p[17]) ; SORT2(p[7], p[17]) ;  SORT2(p[7], p[10]) ;
    SORT2(p[12], p[18]) ; SORT2(p[7], p[12]) ;  SORT2(p[10], p[18]) ;
    SORT2(p[12], p[20]) ; SORT2(p[10], p[20]) ; SORT2(p[10], p[12]) ;
    return (p[12]);
}

/*--- get the local median and MAD of values vec[j-12 .. j+12] ---*/

#undef  mead25
#define mead25(j)                                              \
 { float qqq[25] ; int jj=(j)-12 ; register int pp;            \
   if( jj < 0 ) jj = 0; else if( jj+25 > num ) jj = num-25;    \
   for( pp=0 ; pp < 25 ; pp++ ) qqq[pp] = vec[jj+pp] ;         \
   med = median25f(qqq) ;                                      \
   for( pp=0 ; pp < 25 ; pp++ ) qqq[pp] = fabsf(qqq[pp]-med) ; \
   mad = median25f(qqq); }

void DES_despike25( int num , double to,double dt , float *vec )
{
   int ii , nsp ; float *zma,*zme , med,mad,val ;
   static float *deswks = NULL ;
   static int   ndeswks = 0 ;

   if( vec == NULL ) return ;
   if( num <  25 ) { despike9_func( num , to,dt , vec ) ; return ; }

   if( ndeswks < num ){
      deswks = (float *)realloc(deswks,sizeof(float)*(4*num)) ;
     ndeswks = num ;
   }

   zme = deswks ; zma = zme + num ;

   for( ii=0 ; ii < num ; ii++ ){
     mead25(ii) ; zme[ii] = med ; zma[ii] = mad ;
   }
   mad = qmed_float(num,zma) ;
   if( mad <= 0.0f ) return ;
   mad *= 6.1234f ;  /* threshold value */

   for( nsp=ii=0 ; ii < num ; ii++ )
     if( fabsf(vec[ii]-zme[ii]) > mad ){ vec[ii] = zme[ii]; nsp++; }

   return ;
}
#undef mead25
#undef SORT2
#undef SWAP

/*-------------- Sample 1D function: HRF Decon [29 Oct 2010] ---------------*/

static float lhs_legendre( float x, float bot, float top, float n )
{
   double xx ;
   xx = 2.0*(x-bot)/(top-bot) - 1.0 ;  /* now in range -1..1 */
   return (float)Plegendre(xx,n) ;
}

void hrfdecon_func( int num , double to,double dt , float *vec )
{
   int   nkern ,  nbase , ii,pp ;
   float *kern , **base , ksum=0.0f ;
   floatvec *bfit ;

   if( num < 49 || dt >= 4.0f ) return ;
   if( dt <= 0.0f ) dt = 1.0f ;

   nkern = 1+(int)(16.0f/dt) ; if( nkern >= num/2 ) return ;
   kern = (float *)calloc(sizeof(float),nkern) ;
   for( ii=1 ; ii < nkern ; ii++ ){
     kern[ii] = powf(ii*dt,8.6f)*exp(-ii*dt/0.547) ; ksum += kern[ii] ;
   }
   for( ii=1 ; ii < nkern ; ii++ ) kern[ii] /= ksum ;
   for( ii=nkern-1 ; ii > 2 ; ii-- ) if( kern[ii] >= 0.0444f ) break ;
   nkern = ii ;

   nbase = 1+(int)(num*dt/100.0f) ; if( nbase < 3 ) nbase = 3 ;
   base  = (float **)malloc(sizeof(float *)*nbase) ;
   for( pp=0 ; pp < nbase ; pp++ ){
     base[pp] = (float *)malloc(sizeof(float)*num) ;
     for( ii=0 ; ii < num ; ii++ )
       base[pp][ii] = lhs_legendre( (float)ii, 0.0f, num-1.0f, pp ) ;
   }

   despike9_func( num , to,dt , vec ) ;

   { float *vlam = (float *)malloc(sizeof(float)*(nbase+num)) ;
     for( ii=0 ; ii < num   ; ii++ ) vlam[ii]     = 9.999f ;
     for( ii=0 ; ii < nbase ; ii++ ) vlam[ii+num] = 0.0f ;
     THD_lasso_fixlam(9.999f) ;
     THD_lasso_setlamvec( nbase+num , vlam ) ; free(vlam) ;
   }

   bfit = THD_deconvolve( num , vec ,
                          0 , nkern-1 , kern ,
                          nbase , base , -1 , NULL , 0 , 7 , -3.333f ) ;

   if( bfit != NULL ){
     memcpy(vec,bfit->ar,sizeof(float)*num) ;
     KILL_floatvec(bfit) ;
   }

   for( pp=0 ; pp < nbase ; pp++ ) free(base[pp]) ;
   free(base) ; free(kern) ; return ;
}

/*---------------- Sample 1D function: abs(FFT) [30 Jun 2000] --------------*/

void absfft_func( int num , double to,double dt, float *vec )
{
   static complex *cx=NULL ;
   static int      ncx=0 , numold=0 ;
   float f0,f1,f2 ;
   int ii ;

   if( num < 2 ) return ;
   if( num != numold ){
     numold = num ;
     ncx    = csfft_nextup_even(numold) ;
     cx     = (complex *)realloc(cx,sizeof(complex)*ncx) ;
     INFO_message("1D FFT: ndata=%d nfft=%d",num,ncx) ;
   }

   get_quadratic_trend( num , vec , &f0,&f1,&f2 ) ;  /* thd_detrend.c */

   for( ii=0 ; ii < num ; ii++ ){ cx[ii].r = vec[ii]-(f0+f1*ii+f2*ii*ii); cx[ii].i = 0.0; }
   for(      ; ii < ncx ; ii++ ){ cx[ii].r = cx[ii].i = 0.0 ; }

   csfft_cox( -1 , ncx , cx ) ;                      /* csfft.c */

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
     v += (ii+1.0f)*(ar[ii]+ar[n-1-ii]) ;
     d += 2.0f*(ii+1.0f) ;
   }
   v += (n2+1.0f)*ar[n2] ; d += (n2+1.0f) ;
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

/*------------------------------------------------------------------------*/

void adapt_mean_21_box_func( int nx, int ny, double dx, double dy, float *ar )
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

         ar[ii+joff] = adaptive_weighted_mean( 21 , aa ) ;
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

   nxup = csfft_nextup_even(nx) ;  /* get FFT size */
   nyup = csfft_nextup_even(ny) ;

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

   nxup = csfft_nextup_even(nx) ;  /* get FFT size */
   nyup = csfft_nextup_even(ny) ;

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

/*----- 28 Oct 2014: sharpness estimate for an image -------------------------*/

void sharpness2D_func( int nx , int ny , double dx, double dy, float *ar )
{
   MRI_IMAGE *inim , *outim ;

   if( ar == NULL || nx < 5 || ny < 5 ) return ;

   inim = mri_new_vol_empty( nx,ny,1 , MRI_float ) ;
   mri_set_data_pointer(inim,ar) ;
   outim = mri_sharpness(inim) ;
   if( outim != NULL ){
     memcpy( ar , MRI_FLOAT_PTR(outim) , sizeof(float)*nx*ny ) ;
     mri_free(outim) ; mri_clear_and_free(inim) ;
   }
   return ;
}
