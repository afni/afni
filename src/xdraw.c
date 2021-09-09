#include "mrilib.h"
#include "xutil.h"
#include "xim.h"

/*--------------------------------------------------------------------------*/
/** AFNI_XDrawLines() replacement for XDrawLines() **/
/*--------------------------------------------------------------------------*/

/*======================================================================*/
/* Upsampling code, adapted from code in mri_dup.c [28 May 2020] */

#ifndef SHORTIZE
# define SHORTIZE(xx) (  ((xx) < -32767.0f) ? (short)-32767                    \
                       : ((xx) >  32767.0f) ? (short) 32767 : (short)rint(xx) )
#endif

#define RENUP_VEC(vv,kk)  { (vv) = (float *)realloc((vv),(kk)*sizeof(float)); }

/*-- seventh order interpolation polynomials --*/

#define S_M3(x) (x*(x*x-1.0f)*(x*x-4.0f)*(x-3.0f)*(4.0f-x)*0.0001984126984f)
#define S_M2(x) (x*(x*x-1.0f)*(x-2.0f)*(x*x-9.0f)*(x-4.0f)*0.001388888889f)
#define S_M1(x) (x*(x-1.0f)*(x*x-4.0f)*(x*x-9.0f)*(4.0f-x)*0.004166666667f)
#define S_00(x) ((x*x-1.0f)*(x*x-4.0f)*(x*x-9.0f)*(x-4.0f)*0.006944444444f)
#define S_P1(x) (x*(x+1.0f)*(x*x-4.0f)*(x*x-9.0f)*(4.0f-x)*0.006944444444f)
#define S_P2(x) (x*(x*x-1.0f)*(x+2.0f)*(x*x-9.0f)*(x-4.0f)*0.004166666667f)
#define S_P3(x) (x*(x*x-1.0f)*(x*x-4.0f)*(x+3.0f)*(4.0f-x)*0.001388888889f)
#define S_P4(x) (x*(x*x-1.0f)*(x*x-4.0f)*(x*x-9.0f)*0.0001984126984f)

/* 7 point interpolate as floats, cast to short at end */

#define INT7(k,i)                                                \
  ( vout = ((k)==0) ? ( far[i] )                                 \
                    : ( fm3[k] * far[i-3] + fm2[k] * far[i-2]    \
                      + fm1[k] * far[i-1] + f00[k] * far[i  ]    \
                      + fp1[k] * far[i+1] + fp2[k] * far[i+2]    \
                      + fp3[k] * far[i+3] + fp4[k] * far[i+4]  ), SHORTIZE(vout) )

/*----------------------------------------------------------------------------
  Up sample a short-valued array sar[0..nar-1] nup times to produce
  sout[0..(nar-1)*nup] -- so have (nar-1)*nup+1 output points.
  Uses 7th order polynomial interpolation (mostly).
  NOTE: the number of output points, and the use of shorts, is where
        this function differs from the original in mri_dup.c
  NOTE: for use in AFNI_XDrawLines(), which uses XPoint,
        which uses shorts as the pixel locations
------------------------------------------------------------------------------*/

static void upsample_7short( int nup , int nar , short *sar , short *sout )
{
   int kk,ii , ibot,itop ;
   float nupi , val,vout ;
   /* static arrays for interpolation */
   static int nupold = -1 ;
   static int nupmax = 0;
   static float *fm3=NULL, *fm2=NULL, *fm1=NULL, *f00=NULL,
                *fp1=NULL, *fp2=NULL, *fp3=NULL, *fp4=NULL;
   static float *far=NULL , *qar=NULL ;

   /*-- sanity checks --*/

   if( nup < 1 || nar < 2 || sar == NULL || sout == NULL ) return ;

   if( nup == 1 ){ memcpy( sout, sar, sizeof(short)*nar ); return; }

   /* temp float array for data;
      with trickery (pointer arithmetic) to use negative subscripts,
      because we need input [i-3]..[i+4] values to interpolate the
      output values between input indexes i and i+1, for i=0..nar-2 */

   qar = (float *)malloc(sizeof(float)*(nar+7)) ;
   far = qar + 3 ; /* start the indexing trickery! */
   for( kk=0 ; kk < nar ; kk++ ) far[kk] = (float)sar[kk] ;
   val = far[1] - far[0] ;
   far[-1] = far[0] - 3.0f*val ; /* linear extrapolate before start */
   far[-2] = far[0] - 2.0f*val ;
   far[-3] = far[0] -      val ;
   val = far[nar-1] - far[nar-2] ;
   far[nar]   = far[nar-1] +      val ; /* and after end */
   far[nar+1] = far[nar-1] + 2.0f*val ;
   far[nar+2] = far[nar-1] + 3.0f*val ;
   far[nar+3] = far[nar-1] + 4.0f*val ; /* this value not really needed */

   nupi = 1.0f / (float)nup ;

   /*-- initialize 7th order interpolation coefficients, if nup has changed --*/

   if (nupmax < nup) { /* resize coefficient arrays if bigger than of old */
      RENUP_VEC(fm3,nup); RENUP_VEC(fm2,nup);
      RENUP_VEC(fm1,nup); RENUP_VEC(f00,nup);
      RENUP_VEC(fp1,nup); RENUP_VEC(fp2,nup);
      RENUP_VEC(fp3,nup); RENUP_VEC(fp4,nup);
      nupmax = nup ; /* keep track of largest array size ever used */
   }

   if( nup != nupold ){ /* recalculate coefs if not the same as last time in */
     for( kk=0 ; kk < nup ; kk++ ){
       val = ((float)kk) * nupi ;
       fm3[kk] = S_M3(val); fm2[kk] = S_M2(val); fm1[kk] = S_M1(val);
       f00[kk] = S_00(val); fp1[kk] = S_P1(val); fp2[kk] = S_P2(val);
       fp3[kk] = S_P3(val); fp4[kk] = S_P4(val);
     }
     nupold = nup ; /* for the historical records */
   }

   /*-- FINALLY: interpolate --*/

   ibot = 0 ; itop = nar-2 ;  /* add points between [ii] and [ii+1] */

   switch( nup ){
      default:       /* outer and inner loops */
        for( ii=ibot ; ii <= itop ; ii++ )
          for( kk=0 ; kk < nup ; kk++ ) sout[kk+ii*nup] = INT7(kk,ii) ;
      break ;

      case 2:        /* inner loop unrolled for optimizer */
        for( ii=ibot ; ii <= itop ; ii++ ){
          sout[ii*nup]   = INT7(0,ii) ; sout[ii*nup+1]  = INT7(1,ii) ;
        }
      break ;

      case 3:        /* inner loop unrolled for optimizer */
        for( ii=ibot ; ii <= itop ; ii++ ){
          sout[ii*nup]   = INT7(0,ii) ; sout[ii*nup+1]  = INT7(1,ii) ;
          sout[ii*nup+2] = INT7(2,ii) ;
        }
      break ;

      case 4:        /* inner loop unrolled for optimizer */
        for( ii=ibot ; ii <= itop ; ii++ ){
          sout[ii*nup]   = INT7(0,ii) ; sout[ii*nup+1]  = INT7(1,ii) ;
          sout[ii*nup+2] = INT7(2,ii) ; sout[ii*nup+3]  = INT7(3,ii) ;
        }
      break ;
   }
   sout[(nar-1)*nup] = sar[nar-1] ; /* need final value */

   free(qar) ;
   return ;
}

/*--------------------------------------------------------------------------*/

/* Hermite spline basis funcs (damn, them Frenchies was SMART) */

#define H00(x) ( (x)*(x)*(2.0f*(x)-3.0f)+1.0f )
#define H01(x) ( (x)*(x)*(3.0f-2.0f*(x)) )
#define H10(x) ( (x)*((x)*(x)-2.0f*(x)+1.0f) )
#define H11(x) ( (x)*(x)*((x)-1.0f) )

/* interpolate as floats, cast to short at end */

#define INTM(k,i)                                             \
  ( vout = ((k)==0) ? ( far[i] )                              \
                    : ( h00[k] * far[i] + h01[k] * far[i+1]   \
                      + h10[k] * mk[i]  + h11[k] * mk[i+1] ) , SHORTIZE(vout) )

/*-------- Monotonic cubic spline interpolation:
           https://en.wikipedia.org/wiki/Monotone_cubic_interpolation --------*/

static void upsample_monoshort( int nup , int nar , short *sar , short *sout )
{
   int kk,ii , ibot,itop ;
   float nupi , val,vout , ak,bk,tk , dmax=0.0f,fmax=0.0f ;
   /* static arrays to avoid reallocation a milliard of times */
   static int nupold = -1 ;
   static int nupmax = 0;
   static float *far , *dk , *mk ;
   static float *h00=NULL , *h01=NULL , *h10=NULL , *h11=NULL ;

   /*-- sanity checks --*/

   if( nup < 1 || nar < 2 || sar == NULL || sout == NULL ) return ;

   if( nup == 1 ){ memcpy( sout, sar, sizeof(short)*nar ); return; }

   /* temp float arrays for data, etc */

   far = (float *)calloc(sizeof(float),nar) ;
   dk  = (float *)calloc(sizeof(float),nar) ; /* secants */
   mk  = (float *)calloc(sizeof(float),nar) ; /* slopes */

   for( kk=0 ; kk < nar   ; kk++ ){  /* copy input to floats */
     far[kk] = (float)sar[kk] ;
     val     = fabsf(far[kk]) ; if( val > fmax ) fmax = val ;
   }

   for( kk=0 ; kk < nar-1 ; kk++ ){  /* get secants */
     dk[kk] = far[kk+1]-far[kk] ;
     val    = fabsf(dk[kk]) ; if( val > dmax ) dmax = val ;
   }

   mk[0] = dk[0] ; mk[nar-1] = dk[nar-2] ; /* get slopes */
   for( kk=1 ; kk < nar-1 ; kk++ ) mk[kk]  = 0.5f*(dk[kk]*dk[kk-1]) ;

   /* adjust slopes for monotonicity */

   vout = 0.001f*dmax + 0.0000001f*fmax ;
   for( kk=0 ; kk < nar-1 ; kk++ ){
     if( fabsf(dk[kk]) <= vout ){
       mk[kk] = mk[kk+1] = 0.0f ;
     } else {
       ak = mk[kk] / dk[kk] ; bk = mk[kk+1] / dk[kk] ;
       if( ak < 0.0f ){ mk[kk]   = 0.0f ; ak = 0.0f ; }
       if( bk < 0.0f ){ mk[kk+1] = 0.0f ; bk = 0.0f ; }
       val = ak*ak+bk*bk ;
       if( val > 9.0f ){
         tk = 3.0f / sqrtf(val) ;
         mk[kk]   = tk * ak * dk[kk] ;
         mk[kk+1] = tk * bk * dk[kk] ;
       }
     }
   }

   nupi = 1.0f / (float)nup ;

   /*-- initialize Hermite interpolation coefficients, if nup has changed --*/

   if (nupmax < nup) { /* resize coefficient arrays if bigger than of old */
      RENUP_VEC(h00,nup); RENUP_VEC(h01,nup);
      RENUP_VEC(h10,nup); RENUP_VEC(h11,nup);
      nupmax = nup ; /* keep track of largest every used */
   }

   if( nup != nupold ){ /* recalculate if not the same as last time in */
     for( kk=0 ; kk < nup ; kk++ ){
       val = ((float)kk) * nupi ;
       h00[kk] = H00(val) ; h01[kk] = H01(val) ;
       h10[kk] = H10(val) ; h11[kk] = H11(val) ;
     }
     nupold = nup ; /* for the historical records */
   }

   /*-- FINALLY: interpolate --*/

   ibot = 0 ; itop = nar-2 ;  /* add points between [ii] and [ii+1] */

   switch( nup ){
      default:       /* outer and inner loops */
        for( ii=ibot ; ii <= itop ; ii++ )
          for( kk=0 ; kk < nup ; kk++ ) sout[kk+ii*nup] = INTM(kk,ii) ;
      break ;

      case 2:        /* inner loop unrolled for optimizer */
        for( ii=ibot ; ii <= itop ; ii++ ){
          sout[ii*nup]   = INTM(0,ii) ; sout[ii*nup+1]  = INTM(1,ii) ;
        }
      break ;

      case 3:        /* inner loop unrolled for optimizer */
        for( ii=ibot ; ii <= itop ; ii++ ){
          sout[ii*nup]   = INTM(0,ii) ; sout[ii*nup+1]  = INTM(1,ii) ;
          sout[ii*nup+2] = INTM(2,ii) ;
        }
      break ;

      case 4:        /* inner loop unrolled for optimizer */
        for( ii=ibot ; ii <= itop ; ii++ ){
          sout[ii*nup]   = INTM(0,ii) ; sout[ii*nup+1]  = INTM(1,ii) ;
          sout[ii*nup+2] = INTM(2,ii) ; sout[ii*nup+3]  = INTM(3,ii) ;
        }
      break ;
   }
   sout[(nar-1)*nup] = sar[nar-1] ; /* need final value */

   free(mk) ; free(dk) ; free(far) ;
   return ;
}

/*--------- Upsample/interpolate both ways, and combine them;
            Why? It looked better than either way alone, to me [RWC] ---------*/

static void upsample_comboshort( int nup , int nar , short *sar , short *sout )
{
   short *st1 , *st2 ; int ii,nout=(nar-1)*nup+1 ; float val ;

   if( nup < 1 || nar < 2 || sar == NULL || sout == NULL ) return ;
   if( nup == 1 ){ memcpy( sout, sar, sizeof(short)*nar ); return; }

   st1 = (short *)malloc(sizeof(short)*nout) ;
   st2 = (short *)malloc(sizeof(short)*nout) ;

   upsample_7short   ( nup,nar,sar,st1 ) ;
   upsample_monoshort( nup,nar,sar,st2 ) ;
   for( ii=0 ; ii < nout ; ii++ ){
     val = 0.3f*(float)st1[ii] + 0.7f*(float)st2[ii] ; /* mixing */
     sout[ii] = SHORTIZE(val) ;
   }
   free(st2) ; free(st1) ; return ;
}

/**---- this macro defines the function used for the actual upsampling ----**/

#define UPSAMPLE upsample_comboshort

/*--------------------------------------------------------------------------*/
/* Draw X11 lines, but upsampled to look smoother and more delicious  */

void AFNI_XDrawLines( Display *display, Drawable d,
                      GC gc, XPoint *points, int npoints, int mode , int nupsam )
{
   XPoint *new_points ;
   int     new_npoints , ii ;
   short  *old_xy , *new_xy ;

   /* this is for the jaggedy losers */

   if( nupsam <= 1 || npoints < 3 ){
     XDrawLines(display,d,gc,points,npoints,mode) ; return ;
   }

   /* this is for the REAL MEN out there (insert Tarzan yell) */

   new_npoints = (npoints-1)*nupsam+1 ;
   new_points  = (XPoint *)malloc(sizeof(XPoint)*new_npoints) ;

   old_xy = (short *)malloc(sizeof(short)*npoints) ;
   new_xy = (short *)malloc(sizeof(short)*new_npoints) ;

   /* upsample the x coordinates */
   for( ii=0 ; ii < npoints ; ii++ ) old_xy[ii] = points[ii].x ;
   UPSAMPLE( nupsam , npoints , old_xy , new_xy ) ;
   for( ii=0 ; ii < new_npoints ; ii++ ) new_points[ii].x = new_xy[ii] ;

   /* upsample the y coordinates */
   for( ii=0 ; ii < npoints ; ii++ ) old_xy[ii] = points[ii].y ;
   UPSAMPLE( nupsam , npoints , old_xy , new_xy ) ;
   for( ii=0 ; ii < new_npoints ; ii++ ) new_points[ii].y = new_xy[ii] ;

   /* and draw the straight lines between them */
   XDrawLines(display,d,gc,new_points,new_npoints,mode) ;

   free(new_xy) ; free(old_xy) ; free(new_points) ; return ;
}

/*----------------------------------------------------------------------------*/
/* Similar code for drawing a smooted (smoothed?) filled polygon (for pmplot) */
/* https://en.wikipedia.org/wiki/Reed_Smoot                                   */
/* https://en.wikipedia.org/wiki/Oliver_R._Smoot                              */
/*----------------------------------------------------------------------------*/

void AFNI_XFillPolygon( Display *display, Drawable d,
                        GC gc, XPoint *points, int npoints, int shape ,
                        int mode , int nupsam )
{
   XPoint *new_points ;
   int     new_npoints , ii ;
   short  *old_xy , *new_xy ;

   if( nupsam <= 1 ){
     XFillPolygon(display,d,gc,points,npoints,shape,mode) ; return ;
   }

   new_npoints = (npoints-1)*nupsam+1 ;
   new_points  = (XPoint *)malloc(sizeof(XPoint)*new_npoints) ;

   old_xy = (short *)malloc(sizeof(short)*npoints) ;
   new_xy = (short *)malloc(sizeof(short)*new_npoints) ;

   for( ii=0 ; ii < npoints ; ii++ ) old_xy[ii] = points[ii].x ;
   UPSAMPLE( nupsam , npoints , old_xy , new_xy ) ;
   for( ii=0 ; ii < new_npoints ; ii++ ) new_points[ii].x = new_xy[ii] ;

   for( ii=0 ; ii < npoints ; ii++ ) old_xy[ii] = points[ii].y ;
   UPSAMPLE( nupsam , npoints , old_xy , new_xy ) ;
   for( ii=0 ; ii < new_npoints ; ii++ ) new_points[ii].y = new_xy[ii] ;

   XFillPolygon(display,d,gc,new_points,new_npoints,shape,mode) ;

   free(new_xy) ; free(old_xy) ; free(new_points) ; return ;
}
