/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** Not 7D safe ***/

#define MAX_NUP 32

static void (*usammer)(int,int,float *,float *) = upsample_7 ;

static int usammer_mode = 7 ;

static void upsample_1by2( int, byte *, byte * ) ;  /* at end of file */
static void upsample_1by3( int, byte *, byte * ) ;
static void upsample_1by4( int, byte *, byte * ) ;

static MRI_IMAGE * mri_dup2D_rgb4( MRI_IMAGE *) ;   /* 14 Mar 2002 */
static MRI_IMAGE * mri_dup2D_rgb3( MRI_IMAGE *) ;   /* 14 Mar 2002 */
static MRI_IMAGE * mri_dup2D_rgb2( MRI_IMAGE *) ;   /* 22 Mar 2002 */

static MRI_IMAGE * mri_dup2D_rgb_NN( MRI_IMAGE *, int);/* 22 Feb 2004 [rickr] */

/*-----------------------------------------------------------------------------
  Set resample mode for mri_dup2D (1 or 7)
-------------------------------------------------------------------------------*/

void mri_dup2D_mode( int mm )
{
   switch( mm ){
      case 1:  usammer = upsample_1 ; break ;
     default:  usammer = upsample_7 ; break ;
   }
   usammer_mode = mm ;
}

/*------------------------------------------------------------------------------
   Blow up a 2D image nup times, using 7th order polynomial for interpolation.
--------------------------------------------------------------------------------*/

MRI_IMAGE * mri_dup2D( int nup , MRI_IMAGE *imin )
{
   MRI_IMAGE *flim , *newim ;
   float     *flar , *newar , *cold , *cnew ;
   int nx,ny , nxup,nyup , ii,jj,kk, NNmode = 0 ;

ENTRY("mri_dup2D") ;
   /*-- sanity checks --*/

   if( nup < 1 || nup > MAX_NUP || imin == NULL ) RETURN( NULL );

   if( nup == 1 ){ newim = mri_to_mri( imin->kind, imin ); RETURN(newim); }

   /* does the user want neighbor interpolation?     22 Feb 2004 [rickr] */
   if ( AFNI_yesenv("AFNI_IMAGE_ZOOM_NN") ) {
      mri_dup2D_mode(0);
      NNmode = 1;
   }

   /*-- complex-valued images: do each part separately --*/

   if( imin->kind == MRI_complex ){
      MRI_IMARR *impair ; MRI_IMAGE *rim, *iim, *tim ;

      impair = mri_complex_to_pair( imin ) ;
      if( impair == NULL ){
         fprintf(stderr,"*** mri_complex_to_pair fails in mri_dup2D!\n"); EXIT(1);
      }
      rim = IMAGE_IN_IMARR(impair,0) ;
      iim = IMAGE_IN_IMARR(impair,1) ;  FREE_IMARR(impair) ;
      tim = mri_dup2D( nup, rim ); mri_free( rim ); rim = tim ;
      tim = mri_dup2D( nup, iim ); mri_free( iim ); iim = tim ;
      newim = mri_pair_to_complex( rim , iim ) ;
      mri_free( rim ) ; mri_free( iim ) ;
      MRI_COPY_AUX(newim,imin) ;
      RETURN(newim) ;
   }

   /*-- 14 Mar 2002: RGB image up by 2..4, all colors at once --*/

   if( imin->kind == MRI_rgb ){
     MRI_IMAGE *qqim=NULL ;
     if ( NNmode )
         qqim = mri_dup2D_rgb_NN(imin, nup);  /* 22 Feb 2004 [rickr] */
     else {
       switch(nup){
         case 4: qqim = mri_dup2D_rgb4(imin); break; /* special purpose fast codes */
         case 3: qqim = mri_dup2D_rgb3(imin); break; /* using fixed pt arithmetic  */
         case 2: qqim = mri_dup2D_rgb2(imin); break;

        /*-- other factors: do each color separately as a byte image --*/
        default:{
           MRI_IMARR *imtriple ; MRI_IMAGE *rim, *gim, *bim, *tim ;

           imtriple = mri_rgb_to_3byte( imin ) ;
           if( imtriple == NULL ){
             fprintf(stderr,"*** mri_rgb_to_3float fails in mri_dup2D!\n"); RETURN(NULL);
           }
           rim = IMAGE_IN_IMARR(imtriple,0) ;
           gim = IMAGE_IN_IMARR(imtriple,1) ;
           bim = IMAGE_IN_IMARR(imtriple,2) ; FREE_IMARR(imtriple) ;
           tim = mri_dup2D( nup, rim ); mri_free(rim); rim = tim;
           tim = mri_dup2D( nup, gim ); mri_free(gim); gim = tim;
           tim = mri_dup2D( nup, bim ); mri_free(bim); bim = tim;
           newim = mri_3to_rgb( rim, gim, bim ) ;
           mri_free(rim) ; mri_free(gim) ; mri_free(bim) ;
           MRI_COPY_AUX(newim,imin) ;
           qqim = newim ;
         }
         break ;
       }
     }

     RETURN(qqim) ;
   }

   /*-- Special case: byte-valued image upsampled by 2/3/4 [13 Mar 2002] --*/

   if( imin->kind == MRI_byte && nup <= 4 ){
     void (*usbyte)(int,byte *,byte *) = NULL ;
     byte *bar=MRI_BYTE_PTR(imin) , *bnew , *cold, *cnew ;
     nx = imin->nx; ny = imin->ny; nxup = nx*nup; nyup = ny*nup ;
     newim = mri_new( nxup,nyup , MRI_byte ); bnew = MRI_BYTE_PTR(newim);
     switch( nup ){
       case 2: usbyte = upsample_1by2 ; break ;  /* special fast codes */
       case 3: usbyte = upsample_1by3 ; break ;
       case 4: usbyte = upsample_1by4 ; break ;
     }
     for( jj=0 ; jj < ny ; jj++ )                      /* upsample rows */
       usbyte( nx , bar+jj*nx , bnew+jj*nxup ) ;
     cold = (byte *) malloc( sizeof(byte) * ny   ) ;
     cnew = (byte *) malloc( sizeof(byte) * nyup ) ;
     for( ii=0 ; ii < nxup ; ii++ ){                   /* upsample cols */
       for( jj=0 ; jj < ny ; jj++ ) cold[jj] = bnew[ii+jj*nxup] ;
       usbyte( ny , cold , cnew ) ;
       for( jj=0 ; jj < nyup ; jj++ ) bnew[ii+jj*nxup] = cnew[jj] ;
     }
     free(cold); free(cnew); MRI_COPY_AUX(newim,imin); RETURN(newim);
   }

   /*-- otherwise, make sure we operate on a float image --*/

   if( imin->kind == MRI_float ) flim = imin ;
   else                          flim = mri_to_float( imin ) ;

   flar = MRI_FLOAT_PTR(flim) ;

   nx = flim->nx ; ny = flim->ny ; nxup = nx*nup ; nyup = ny*nup ;
   newim = mri_new( nxup , nyup , MRI_float ) ;
   newar = MRI_FLOAT_PTR(newim) ;

   /*-- upsample rows --*/

   for( jj=0 ; jj < ny ; jj++ )
      usammer( nup , nx , flar + jj*nx , newar + jj*nxup ) ;

   if( flim != imin ) mri_free(flim) ;

   /*-- upsample columns --*/

   cold = (float *) malloc( sizeof(float) * ny ) ;
   cnew = (float *) malloc( sizeof(float) * nyup ) ;
   if( cold == NULL || cnew == NULL ){
      fprintf(stderr,"*** mri_dup2D malloc failure!\n"); EXIT(1);
   }

   for( ii=0 ; ii < nxup ; ii++ ){
      for( jj=0 ; jj < ny ; jj++ ) cold[jj] = newar[ii + jj*nxup] ;
      usammer( nup , ny , cold , cnew ) ;
      for( jj=0 ; jj < nyup ; jj++ ) newar[ii+jj*nxup] = cnew[jj] ;
   }

   free(cold) ; free(cnew) ;

   /*-- type convert output, if necessary --*/

   switch( imin->kind ){

      case MRI_byte:{
         byte * bar ; MRI_IMAGE * bim ; float fmin , fmax ;

         bim = mri_new( nxup,nyup , MRI_byte ) ; bar = MRI_BYTE_PTR(bim) ;
         fmin = mri_min(imin) ; fmax = mri_max(imin) ;
         for( ii=0 ; ii < newim->nvox ; ii++ )
            bar[ii] =  (newar[ii] < fmin) ? fmin
                     : (newar[ii] > fmax) ? fmax : newar[ii] ;
         mri_free(newim) ; newim = bim ;
      }
      break ;

      case MRI_short:{
         short * sar ; MRI_IMAGE * sim ; float fmin , fmax ;

         sim = mri_new( nxup,nyup , MRI_short ) ; sar = MRI_SHORT_PTR(sim) ;
         fmin = mri_min(imin) ; fmax = mri_max(imin) ;
         for( ii=0 ; ii < newim->nvox ; ii++ )
            sar[ii] =  (newar[ii] < fmin) ? fmin
                     : (newar[ii] > fmax) ? fmax : newar[ii] ;
         mri_free(newim) ; newim = sim ;
      }
      break ;

      case MRI_float:{
         float fmin , fmax ;

         fmin = mri_min(imin) ; fmax = mri_max(imin) ;
         for( ii=0 ; ii < newim->nvox ; ii++ )
                 if( newar[ii] < fmin ) newar[ii] = fmin ;
            else if( newar[ii] > fmax ) newar[ii] = fmax ;
      }
   }

   /*-- finito --*/

   MRI_COPY_AUX(newim,imin) ;
   RETURN( newim );
}

/*======================================================================*/

/*-- seventh order Lagrange polynomials --*/

#define S_M3(x) (x*(x*x-1.0)*(x*x-4.0)*(x-3.0)*(4.0-x)*0.0001984126984)
#define S_M2(x) (x*(x*x-1.0)*(x-2.0)*(x*x-9.0)*(x-4.0)*0.001388888889)
#define S_M1(x) (x*(x-1.0)*(x*x-4.0)*(x*x-9.0)*(4.0-x)*0.004166666667)
#define S_00(x) ((x*x-1.0)*(x*x-4.0)*(x*x-9.0)*(x-4.0)*0.006944444444)
#define S_P1(x) (x*(x+1.0)*(x*x-4.0)*(x*x-9.0)*(4.0-x)*0.006944444444)
#define S_P2(x) (x*(x*x-1.0)*(x+2.0)*(x*x-9.0)*(x-4.0)*0.004166666667)
#define S_P3(x) (x*(x*x-1.0)*(x*x-4.0)*(x+3.0)*(4.0-x)*0.001388888889)
#define S_P4(x) (x*(x*x-1.0)*(x*x-4.0)*(x*x-9.0)*0.0001984126984)

#ifdef ZFILL
#  define FINS(i) ( ((i)<0 || (i)>=nar) ? 0.0 : far[(i)] )
#else
#  define FINS(i) ( ((i)<0) ? far[0] : ((i)>=nar) ? far[nar-1] : far[(i)] )
#endif

#define INT7(k,i) (  fm3[k] * far[i-3] + fm2[k] * far[i-2] \
                   + fm1[k] * far[i-1] + f00[k] * far[i  ] \
                   + fp1[k] * far[i+1] + fp2[k] * far[i+2] \
                   + fp3[k] * far[i+3] + fp4[k] * far[i+4]  )

#define FINT7(k,i) (  fm3[k] * FINS(i-3) + fm2[k] * FINS(i-2) \
                    + fm1[k] * FINS(i-1) + f00[k] * FINS(i  ) \
                    + fp1[k] * FINS(i+1) + fp2[k] * FINS(i+2) \
                    + fp3[k] * FINS(i+3) + fp4[k] * FINS(i+4)  )

/*----------------------------------------------------------------------------
  Up sample an array far[0..nar-1] nup times to produce fout[0..nar*nup-1].
  Uses 7th order polynomial interpolation.
------------------------------------------------------------------------------*/

void upsample_7( int nup , int nar , float * far , float * fout )
{
   int kk,ii , ibot,itop ;
   static int nupold = -1 ;
   static float fm3[MAX_NUP], fm2[MAX_NUP], fm1[MAX_NUP], f00[MAX_NUP],
                fp1[MAX_NUP], fp2[MAX_NUP], fp3[MAX_NUP], fp4[MAX_NUP] ;

   /*-- sanity checks --*/

   if( nup < 1 || nup > MAX_NUP || nar < 2 || far == NULL || fout == NULL ) return ;

   if( nup == 1 ){ memcpy( fout, far, sizeof(float)*nar ); return; }

   /*-- initialize interpolation coefficient, if nup has changed --*/

   if( nup != nupold ){
     float val ;
     for( kk=0 ; kk < nup ; kk++ ){
       val = ((float)kk) / ((float)nup) ;
       fm3[kk] = S_M3(val); fm2[kk] = S_M2(val); fm1[kk] = S_M1(val);
       f00[kk] = S_00(val); fp1[kk] = S_P1(val); fp2[kk] = S_P2(val);
       fp3[kk] = S_P3(val); fp4[kk] = S_P4(val);
     }
     nupold = nup ;
   }

   /*-- interpolate the intermediate places --*/

   ibot = 3 ; itop = nar-5 ;

   switch( nup ){
      default:
        for( ii=ibot ; ii <= itop ; ii++ )
          for( kk=0 ; kk < nup ; kk++ ) fout[kk+ii*nup] = INT7(kk,ii) ;
      break ;

      case 2:
        for( ii=ibot ; ii <= itop ; ii++ ){
          fout[ii*nup]   = INT7(0,ii) ; fout[ii*nup+1] = INT7(1,ii) ;
        }
      break ;

      case 3:
        for( ii=ibot ; ii <= itop ; ii++ ){
          fout[ii*nup]   = INT7(0,ii) ; fout[ii*nup+1] = INT7(1,ii) ;
          fout[ii*nup+2] = INT7(2,ii) ;
        }
      break ;

      case 4:
        for( ii=ibot ; ii <= itop ; ii++ ){
          fout[ii*nup]   = INT7(0,ii) ; fout[ii*nup+1] = INT7(1,ii) ;
          fout[ii*nup+2] = INT7(2,ii) ; fout[ii*nup+3] = INT7(3,ii) ;
        }
      break ;
   }

   /*-- interpolate the outside edges --*/

   for( ii=0 ; ii < ibot ; ii++ )
     for( kk=0 ; kk < nup ; kk++ ) fout[kk+ii*nup] = FINT7(kk,ii) ;

   for( ii=itop+1 ; ii < nar ; ii++ )
     for( kk=0 ; kk < nup ; kk++ ) fout[kk+ii*nup] =  FINT7(kk,ii) ;

   return ;
}

/*======================================================================*/

#define INT1(k,i)  (f00[k]*far[i]  + fp1[k]*far[i+1] )

#define FINT1(k,i) (f00[k]*FINS(i) + fp1[k]*FINS(i+1))

/*----------------------------------------------------------------------------
  Up sample an array far[0..nar-1] nup times to produce fout[0..nar*nup-1].
  Uses linear polynomial interpolation.
------------------------------------------------------------------------------*/

void upsample_1( int nup , int nar , float * far , float * fout )
{
   int kk,ii , ibot,itop ;
   static int nupold=-1 ;
   static float f00[MAX_NUP], fp1[MAX_NUP] ;

   /*-- sanity checks --*/

   if( nup < 1 || nup > MAX_NUP || nar < 2 || far == NULL || fout == NULL ) return ;

   if( nup == 1 ){ memcpy( fout, far, sizeof(float)*nar ); return; }

   /*-- initialize interpolation coefficient, if nup has changed --*/

   if( nup != nupold ){
     float val ;
     for( kk=0 ; kk < nup ; kk++ ){
       val = ((float)kk) / ((float)nup) ;
       f00[kk] = 1.0 - val ; fp1[kk] = val ;
     }
     nupold = nup ;
   }

   /*-- interpolate the intermediate places --*/

   ibot = 0 ; itop = nar-2 ;

   switch( nup ){
      default:
        for( ii=ibot ; ii <= itop ; ii++ )
          for( kk=0 ; kk < nup ; kk++ ) fout[kk+ii*nup] = INT1(kk,ii) ;
      break ;

      case 2:
        for( ii=ibot ; ii <= itop ; ii++ ){
          fout[ii*nup]   = INT1(0,ii) ; fout[ii*nup+1] = INT1(1,ii) ;
        }
      break ;

      case 3:
        for( ii=ibot ; ii <= itop ; ii++ ){
          fout[ii*nup]   = INT1(0,ii) ; fout[ii*nup+1] = INT1(1,ii) ;
          fout[ii*nup+2] = INT1(2,ii) ;
        }
      break ;

      case 4:
        for( ii=ibot ; ii <= itop ; ii++ ){
          fout[ii*nup]   = INT1(0,ii) ; fout[ii*nup+1] = INT1(1,ii) ;
          fout[ii*nup+2] = INT1(2,ii) ; fout[ii*nup+3] = INT1(3,ii) ;
        }
      break ;
   }

   /*-- interpolate the outside edges --*/

#if 0                             /* nugatory */
   for( ii=0 ; ii < ibot ; ii++ )
     for( kk=0 ; kk < nup ; kk++ ) fout[kk+ii*nup] = FINT1(kk,ii) ;
#endif

   for( ii=itop+1 ; ii < nar ; ii++ )
     for( kk=0 ; kk < nup ; kk++ ) fout[kk+ii*nup] =  FINT1(kk,ii) ;

   return ;
}

/*------------------------------------------------------------------*/
/*! Upsample an array of bytes by exactly 2.
--------------------------------------------------------------------*/

static void upsample_1by2( int nar, byte *bar , byte *bout )
{
   int ii ;
   if( nar < 1 || bar == NULL || bout == NULL ) return ;

   for( ii=0 ; ii < nar-1 ; ii++ ){
      bout[2*ii]   = bar[ii] ;
      bout[2*ii+1] = (bar[ii]+bar[ii+1]) >> 1 ;
   }
   bout[2*nar-1] = bout[2*nar-2] = bar[nar-1] ;
}

/*------------------------------------------------------------------*/
/*! Upsample an array of bytes by exactly 3.
--------------------------------------------------------------------*/

static void upsample_1by3( int nar, byte *bar , byte *bout )
{
   int ii ;
   if( nar < 1 || bar == NULL || bout == NULL ) return ;

   /* Note that 85/256 is about 1/3 and 171/256 is about 2/3;  */
   /* by using this trick, we avoid division and so are faster */

   for( ii=0 ; ii < nar-1 ; ii++ ){
      bout[3*ii]   = bar[ii] ;
      bout[3*ii+1] = (171*bar[ii]+ 85*bar[ii+1]) >> 8 ;
      bout[3*ii+2] = ( 85*bar[ii]+171*bar[ii+1]) >> 8 ;
   }
   bout[3*nar-1] = bout[3*nar-2] = bout[3*nar-3] = bar[nar-1] ;
}

/*------------------------------------------------------------------*/
/*! Upsample an array of bytes by exactly 4.
--------------------------------------------------------------------*/

static void upsample_1by4( int nar, byte *bar , byte *bout )
{
   int ii ;
   if( nar < 1 || bar == NULL || bout == NULL ) return ;

   for( ii=0 ; ii < nar-1 ; ii++ ){
      bout[4*ii]   = bar[ii] ;
      bout[4*ii+1] = (3*bar[ii]+  bar[ii+1]) >> 2 ;
      bout[4*ii+2] = (  bar[ii]+  bar[ii+1]) >> 1 ;
      bout[4*ii+3] = (  bar[ii]+3*bar[ii+1]) >> 2 ;
   }
   bout[4*nar-1] = bout[4*nar-2] = bout[4*nar-3] = bout[4*nar-4] = bar[nar-1];
}

/*************************************************************************/
/*************************************************************************/

/* added option for nearest neighbor (if AFNI_IMAGE_ZOOM_NN is Y/y)
                                                     22 Feb 2004 [rickr] */

static MRI_IMAGE * mri_dup2D_rgb_NN( MRI_IMAGE *inim, int nup )
{
   rgbyte *bin , *bout , *bin1 ;
   MRI_IMAGE *outim ;
   int ii,jj,kk,ll , nx,ny , nxup,nyup ;

ENTRY("mri_dup2D_rgb_NN") ;
   if( inim == NULL || inim->kind != MRI_rgb ) RETURN(NULL);

   bin = (rgbyte *) MRI_RGB_PTR(inim); if( bin == NULL ) RETURN(NULL);

   /* make output image **/

   nx = inim->nx ; ny = inim->ny ; nxup = nup*nx ; nyup = nup*ny ;
   outim = mri_new( nxup , nyup , MRI_rgb ) ;
   bout  = (rgbyte *) MRI_RGB_PTR(outim) ;

   for( jj=0 ; jj < ny ; jj++ ){   /* loop over input rows */
     
     for ( kk= 0; kk < nup; kk++ ) { /* do rows nup times */

       bin1 = bin;

       for( ii=0 ; ii < nx ; ii++ ){

         for ( ll= 0; ll < nup; ll++ ) {
	     *bout++ = *bin1;
         }

         bin1++;
       }

     }
     bin += nx ;
   }

   MRI_COPY_AUX(outim,inim) ;
   RETURN(outim) ;
}

/*************************************************************************/
/*************************************************************************/

static MRI_IMAGE * mri_dup2D_rgb4( MRI_IMAGE *inim )
{
   rgbyte *bin , *bout , *bin1,*bin2 , *bout1,*bout2,*bout3,*bout4 ;
   MRI_IMAGE *outim ;
   int ii,jj , nx,ny , nxup,nyup ;

ENTRY("mri_dup2D_rgb4") ;
   if( inim == NULL || inim->kind != MRI_rgb ) RETURN(NULL);

   bin = (rgbyte *) MRI_RGB_PTR(inim); if( bin == NULL ) RETURN(NULL);

   /* make output image **/

   nx = inim->nx ; ny = inim->ny ; nxup = 4*nx ; nyup = 4*ny ;
   outim = mri_new( nxup , nyup , MRI_rgb ) ;
   bout  = (rgbyte *) MRI_RGB_PTR(outim) ;

   /** macros for the 16 different interpolations
       between the four corners:   ul    ur >   00 10 20 30
                                            >=> 01 11 21 31
                                            >=> 02 12 22 32
                                   ll    lr >   03 13 23 33 **/

#define BOUT(ul,ur,ll,lr,a,b,c,d) ((a*ul+b*ur+c*ll+d*lr) >> 6)

#define BOUT_00(ul,ur,ll,lr) BOUT(ul,ur,ll,lr,49, 7, 7, 1)
#define BOUT_10(ul,ur,ll,lr) BOUT(ul,ur,ll,lr,35,21, 5, 3)
#define BOUT_20(ul,ur,ll,lr) BOUT(ul,ur,ll,lr,21,35, 3, 5)
#define BOUT_30(ul,ur,ll,lr) BOUT(ul,ur,ll,lr, 7,49, 1, 7)

#define BOUT_01(ul,ur,ll,lr) BOUT(ul,ur,ll,lr,35, 5,21, 3)
#define BOUT_11(ul,ur,ll,lr) BOUT(ul,ur,ll,lr,25,15,15, 9)
#define BOUT_21(ul,ur,ll,lr) BOUT(ul,ur,ll,lr,15,25, 9,15)
#define BOUT_31(ul,ur,ll,lr) BOUT(ul,ur,ll,lr, 5,35, 3,21)

#define BOUT_02(ul,ur,ll,lr) BOUT(ul,ur,ll,lr,21, 3,35, 5)
#define BOUT_12(ul,ur,ll,lr) BOUT(ul,ur,ll,lr,15, 9,25,15)
#define BOUT_22(ul,ur,ll,lr) BOUT(ul,ur,ll,lr, 9,15,15,25)
#define BOUT_32(ul,ur,ll,lr) BOUT(ul,ur,ll,lr, 3,21, 5,35)

#define BOUT_03(ul,ur,ll,lr) BOUT(ul,ur,ll,lr, 7, 1,49, 7)
#define BOUT_13(ul,ur,ll,lr) BOUT(ul,ur,ll,lr, 5, 3,35,21)
#define BOUT_23(ul,ur,ll,lr) BOUT(ul,ur,ll,lr, 3, 5,21,35)
#define BOUT_33(ul,ur,ll,lr) BOUT(ul,ur,ll,lr, 1, 7, 7,49)


  /** do 16 interpolations between  ul ur
                                    ll lr  for index #k, color #c **/

#define FOUR_ROWS(k,c,ul,ur,ll,lr)                    \
   { bout1[4*k+2].c = BOUT_00(ul.c,ur.c,ll.c,lr.c) ;  \
     bout1[4*k+3].c = BOUT_10(ul.c,ur.c,ll.c,lr.c) ;  \
     bout1[4*k+4].c = BOUT_20(ul.c,ur.c,ll.c,lr.c) ;  \
     bout1[4*k+5].c = BOUT_30(ul.c,ur.c,ll.c,lr.c) ;  \
     bout2[4*k+2].c = BOUT_01(ul.c,ur.c,ll.c,lr.c) ;  \
     bout2[4*k+3].c = BOUT_11(ul.c,ur.c,ll.c,lr.c) ;  \
     bout2[4*k+4].c = BOUT_21(ul.c,ur.c,ll.c,lr.c) ;  \
     bout2[4*k+5].c = BOUT_31(ul.c,ur.c,ll.c,lr.c) ;  \
     bout3[4*k+2].c = BOUT_02(ul.c,ur.c,ll.c,lr.c) ;  \
     bout3[4*k+3].c = BOUT_12(ul.c,ur.c,ll.c,lr.c) ;  \
     bout3[4*k+4].c = BOUT_22(ul.c,ur.c,ll.c,lr.c) ;  \
     bout3[4*k+5].c = BOUT_32(ul.c,ur.c,ll.c,lr.c) ;  \
     bout4[4*k+2].c = BOUT_03(ul.c,ur.c,ll.c,lr.c) ;  \
     bout4[4*k+3].c = BOUT_13(ul.c,ur.c,ll.c,lr.c) ;  \
     bout4[4*k+4].c = BOUT_23(ul.c,ur.c,ll.c,lr.c) ;  \
     bout4[4*k+5].c = BOUT_33(ul.c,ur.c,ll.c,lr.c) ; }

  /** do the above for all 3 colors */

#define FOUR_RGB(k,ul,ur,ll,lr)  { FOUR_ROWS(k,r,ul,ur,ll,lr) ; \
                                   FOUR_ROWS(k,g,ul,ur,ll,lr) ; \
                                   FOUR_ROWS(k,b,ul,ur,ll,lr) ;  }

   bin1  = bin    ;                  /* 2 input rows */
   bin2  = bin+nx ;

   bout1 = bout+2*nxup ;             /* 4 output rows */
   bout2 = bout1+nxup  ;
   bout3 = bout2+nxup  ;
   bout4 = bout3+nxup  ;

   for( jj=0 ; jj < ny-1 ; jj++ ){   /* loop over input rows */

     for( ii=0 ; ii < nx-1 ; ii++ ){
        FOUR_RGB(ii,bin1[ii],bin1[ii+1],bin2[ii],bin2[ii+1]) ;
     }

     /* at this point, output rows have elements [2..4*nx-3] filled;
        now copy [2] into [0..1] and [4*nx-3] into [4*nx-2..4*nx-1] */

     bout1[0] = bout1[1] = bout1[2] ;
     bout2[0] = bout2[1] = bout2[2] ;
     bout3[0] = bout3[1] = bout3[2] ;
     bout4[0] = bout4[1] = bout4[2] ;

     bout1[nxup-2] = bout1[nxup-1] = bout1[nxup-2] ;
     bout2[nxup-2] = bout2[nxup-1] = bout2[nxup-2] ;
     bout3[nxup-2] = bout3[nxup-1] = bout3[nxup-2] ;
     bout4[nxup-2] = bout4[nxup-1] = bout4[nxup-2] ;

     /* advance input and output rows */

     bin1 = bin2; bin2 += nx ;
     bout1 = bout4+nxup; bout2 = bout1+nxup;
     bout3 = bout2+nxup; bout4 = bout3+nxup;
   }

   /* copy row 2 into rows 0 and row 1 */

   bout1 = bout ; bout2 = bout1+nxup ; bout3 = bout2+nxup ;
   for( ii=0 ; ii < nxup ; ii++ )
      bout1[ii] = bout2[ii] = bout3[ii] ;

   /* copy rown nyup-3 into rows nyup-2 and nyup-1 */

   bout1 = bout + (nyup-3)*nxup ; bout2 = bout1+nxup ; bout3 = bout2+nxup ;
   for( ii=0 ; ii < nxup ; ii++ )
      bout2[ii] = bout3[ii] = bout1[ii] ;

   MRI_COPY_AUX(outim,inim) ;
   RETURN(outim) ;
}

/*************************************************************************/

static MRI_IMAGE * mri_dup2D_rgb3( MRI_IMAGE *inim )
{
   rgbyte *bin , *bout , *bin1,*bin2 , *bout1,*bout2,*bout3 ;
   MRI_IMAGE *outim ;
   int ii,jj , nx,ny , nxup,nyup ;

ENTRY("mri_dup2D_rgb3") ;
   if( inim == NULL || inim->kind != MRI_rgb ) RETURN(NULL) ;

   bin = (rgbyte *) MRI_RGB_PTR(inim); if( bin == NULL ) RETURN(NULL);

   /* make output image **/

   nx = inim->nx ; ny = inim->ny ; nxup = 3*nx ; nyup = 3*ny ;
   outim = mri_new( nxup , nyup , MRI_rgb ) ;
   bout  = (rgbyte *) MRI_RGB_PTR(outim) ;

   /** macros for the 9 different interpolations
       between the four corners:   ul  ur >   00 10 20
                                          >=> 01 11 21
                                   ll  lr >   02 12 22 **/

#define COUT_00(ul,ur,ll,lr) (     ul                              )
#define COUT_10(ul,ur,ll,lr) ((171*ul + 85*ur                ) >> 8)
#define COUT_20(ul,ur,ll,lr) (( 85*ul +171*ur                ) >> 8)

#define COUT_01(ul,ur,ll,lr) ((171*ul +         85*ll        ) >> 8)
#define COUT_11(ul,ur,ll,lr) ((114*ul + 57*ur + 57*ll + 28*lr) >> 8)
#define COUT_21(ul,ur,ll,lr) (( 57*ul +114*ur + 28*ll + 57*lr) >> 8)

#define COUT_02(ul,ur,ll,lr) (( 85*ul +        171*ll        ) >> 8)
#define COUT_12(ul,ur,ll,lr) (( 57*ul + 28*ur +114*ll + 57*lr) >> 8)
#define COUT_22(ul,ur,ll,lr) (( 28*ul + 57*ur + 57*ll +114*lr) >> 8)

  /** do 9 interpolations between  ul ur
                                   ll lr  for index #k, color #c **/

#define THREE_ROWS(k,c,ul,ur,ll,lr)                  \
   { bout1[3*k+1].c = COUT_00(ul.c,ur.c,ll.c,lr.c) ; \
     bout1[3*k+2].c = COUT_10(ul.c,ur.c,ll.c,lr.c) ; \
     bout1[3*k+3].c = COUT_20(ul.c,ur.c,ll.c,lr.c) ; \
     bout2[3*k+1].c = COUT_01(ul.c,ur.c,ll.c,lr.c) ; \
     bout2[3*k+2].c = COUT_11(ul.c,ur.c,ll.c,lr.c) ; \
     bout2[3*k+3].c = COUT_21(ul.c,ur.c,ll.c,lr.c) ; \
     bout3[3*k+1].c = COUT_02(ul.c,ur.c,ll.c,lr.c) ; \
     bout3[3*k+2].c = COUT_12(ul.c,ur.c,ll.c,lr.c) ; \
     bout3[3*k+3].c = COUT_22(ul.c,ur.c,ll.c,lr.c) ;  }

  /** do the above for all 3 colors **/

#define THREE_RGB(k,ul,ur,ll,lr)  { THREE_ROWS(k,r,ul,ur,ll,lr) ; \
                                    THREE_ROWS(k,g,ul,ur,ll,lr) ; \
                                    THREE_ROWS(k,b,ul,ur,ll,lr) ;  }

   bin1  = bin       ; bin2  = bin+nx    ;  /* 2 input rows */
   bout1 = bout +nxup; bout2 = bout1+nxup;  /* 3 output rows */
   bout3 = bout2+nxup;

   for( jj=0 ; jj < ny-1 ; jj++ ){   /* loop over input rows */

     for( ii=0 ; ii < nx-1 ; ii++ ){
        THREE_RGB(ii,bin1[ii],bin1[ii+1],bin2[ii],bin2[ii+1]) ;
     }

     /* here, ii=nx-1; can't use ii+1 */
     /* do the 3*ii+1 output only,    */
     /* and copy into 3*ii+2 column   */

     bout1[3*ii+1].r = COUT_00(bin1[ii].r,0,bin2[ii].r,0) ;
     bout2[3*ii+1].r = COUT_01(bin1[ii].r,0,bin2[ii].r,0) ;
     bout3[3*ii+1].r = COUT_02(bin1[ii].r,0,bin2[ii].r,0) ;

     bout1[3*ii+1].g = COUT_00(bin1[ii].g,0,bin2[ii].g,0) ;
     bout2[3*ii+1].g = COUT_01(bin1[ii].g,0,bin2[ii].g,0) ;
     bout3[3*ii+1].g = COUT_02(bin1[ii].g,0,bin2[ii].g,0) ;

     bout1[3*ii+1].b = COUT_00(bin1[ii].b,0,bin2[ii].b,0) ;
     bout2[3*ii+1].b = COUT_01(bin1[ii].b,0,bin2[ii].b,0) ;
     bout3[3*ii+1].b = COUT_02(bin1[ii].b,0,bin2[ii].b,0) ;

     bout1[3*ii+2] = bout1[3*ii+1] ;
     bout2[3*ii+2] = bout2[3*ii+1] ;
     bout3[3*ii+2] = bout3[3*ii+1] ;

     /* also copy [0] column output from column [1] */

     bout1[0] = bout1[1] ; bout2[0] = bout2[1] ; bout3[0] = bout3[1] ;

     /* advance input and output rows */

     bin1 = bin2; bin2 += nx ;
     bout1 = bout3+nxup; bout2 = bout1+nxup;
     bout3 = bout2+nxup;
   }

   /* here, jj=ny-1, so can't use jj+1 (bin2) */
   /* do the bout1 output row only, copy into bout2 */

   for( ii=0 ; ii < nx-1 ; ii++ ){
     bout1[3*ii+1].r = COUT_00(bin1[ii].r,bin1[ii+1].r,0,0) ;
     bout1[3*ii+2].r = COUT_10(bin1[ii].r,bin1[ii+1].r,0,0) ;
     bout1[3*ii+3].r = COUT_20(bin1[ii].r,bin1[ii+1].r,0,0) ;

     bout1[3*ii+1].g = COUT_00(bin1[ii].g,bin1[ii+1].g,0,0) ;
     bout1[3*ii+2].g = COUT_10(bin1[ii].g,bin1[ii+1].g,0,0) ;
     bout1[3*ii+3].g = COUT_20(bin1[ii].g,bin1[ii+1].g,0,0) ;

     bout1[3*ii+1].b = COUT_00(bin1[ii].b,bin1[ii+1].b,0,0) ;
     bout1[3*ii+2].b = COUT_10(bin1[ii].b,bin1[ii+1].b,0,0) ;
     bout1[3*ii+3].b = COUT_20(bin1[ii].b,bin1[ii+1].b,0,0) ;

     bout2[3*ii+1] = bout1[3*ii+1] ;
     bout2[3*ii+2] = bout1[3*ii+2] ;
     bout2[3*ii+3] = bout1[3*ii+3] ;
   }

   /* do bottom corners */

   bout1[0] = bout2[0] = bout2[1] = bout1[1] ;
   bout1[nxup-2] = bout1[nxup-1] = bout2[nxup-2] = bout2[nxup-1] = bout1[nxup-3] ;

   /* do first row of output as well */

   bout3 = bout+nxup ;
   for( ii=0 ; ii < nxup ; ii++ ) bout[ii] = bout3[ii] ;

   MRI_COPY_AUX(outim,inim) ;
   RETURN(outim) ;
}

/*************************************************************************/
/*************************************************************************/

static MRI_IMAGE * mri_dup2D_rgb2( MRI_IMAGE *inim )
{
   rgbyte *bin , *bout , *bin1,*bin2 , *bout1,*bout2 ;
   MRI_IMAGE *outim ;
   int ii,jj , nx,ny , nxup,nyup ;

ENTRY("mri_dup2D_rgb2") ;
   if( inim == NULL || inim->kind != MRI_rgb ) RETURN(NULL) ;

   bin = (rgbyte *) MRI_RGB_PTR(inim); if( bin == NULL ) RETURN(NULL);

   /* make output image **/

   nx = inim->nx ; ny = inim->ny ; nxup = 2*nx ; nyup = 2*ny ;
   outim = mri_new( nxup , nyup , MRI_rgb ) ;
   bout  = (rgbyte *) MRI_RGB_PTR(outim) ;

   /** macros for the 4 different interpolations
       between the four corners:   ul  ur >=> 00 10
                                   ll  lr >=> 01 11  **/

#define DOUT(ul,ur,ll,lr,a,b,c,d) ((a*ul+b*ur+c*ll+d*lr) >> 4)

#define DOUT_00(ul,ur,ll,lr) DOUT(ul,ur,ll,lr,9,3,3,1)
#define DOUT_10(ul,ur,ll,lr) DOUT(ul,ur,ll,lr,3,9,1,3)

#define DOUT_01(ul,ur,ll,lr) DOUT(ul,ur,ll,lr,3,1,9,3)
#define DOUT_11(ul,ur,ll,lr) DOUT(ul,ur,ll,lr,1,3,3,9)

  /** do r interpolations between  ul ur
                                   ll lr  for index #k, color #c **/

#define TWO_ROWS(k,c,ul,ur,ll,lr)                     \
   { bout1[2*k+1].c = DOUT_00(ul.c,ur.c,ll.c,lr.c) ;  \
     bout1[2*k+2].c = DOUT_10(ul.c,ur.c,ll.c,lr.c) ;  \
     bout2[2*k+1].c = DOUT_01(ul.c,ur.c,ll.c,lr.c) ;  \
     bout2[2*k+2].c = DOUT_11(ul.c,ur.c,ll.c,lr.c) ; }

  /** do the above for all 3 colors */

#define TWO_RGB(k,ul,ur,ll,lr)  { TWO_ROWS(k,r,ul,ur,ll,lr) ;  \
                                  TWO_ROWS(k,g,ul,ur,ll,lr) ;  \
                                  TWO_ROWS(k,b,ul,ur,ll,lr) ; }

   bin1  = bin    ;                 /* 2 input rows */
   bin2  = bin+nx ;

   bout1 = bout +nxup ;             /* 2 output rows */
   bout2 = bout1+nxup  ;

   for( jj=0 ; jj < ny-1 ; jj++ ){   /* loop over input rows */

     for( ii=0 ; ii < nx-1 ; ii++ ){
        TWO_RGB(ii,bin1[ii],bin1[ii+1],bin2[ii],bin2[ii+1]) ;
     }

     /* at this point, output rows have elements [1..2*nx-2] filled;
        now copy [1] into [0] and [2*nx-2] into [2*nx-1]            */

     bout1[0] = bout1[1] ;
     bout2[0] = bout2[1] ;

     bout1[nxup-1] = bout1[nxup-2] ;
     bout2[nxup-1] = bout2[nxup-2] ;

     /* advance input and output rows */

     bin1 = bin2; bin2 += nx ;
     bout1 = bout2+nxup; bout2 = bout1+nxup;
   }

   /* copy row 1 into row 0 */

   bout1 = bout ; bout2 = bout1+nxup ;
   for( ii=0 ; ii < nxup ; ii++ )
      bout1[ii] = bout2[ii] ;

   /* copy rown nyup-2 into row nyup-1 */

   bout1 = bout + (nyup-2)*nxup ; bout2 = bout1+nxup ;
   for( ii=0 ; ii < nxup ; ii++ )
      bout2[ii] = bout1[ii] ;

   MRI_COPY_AUX(outim,inim) ;
   RETURN(outim) ;
}
