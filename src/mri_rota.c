/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** NOT 7D SAFE ***/

#define FINS(i,j) (  ( (i)<0 || (j)<0 || (i)>=nx || (j)>=ny ) \
                     ? 0.0 : far[(i)+(j)*nx] )

/** option to precompute cubic interpolation weights **/

#undef USE_CGRID

#ifdef USE_CGRID
#  define CGRID 8192
   static int p_first = 1 ;
   static float p_m1[CGRID+1], p_00[CGRID+1],
                p_p1[CGRID+1], p_p2[CGRID+1] ;
#endif

   /* cubic interpolation polynomials */

#define P_M1(x)  ((x)*(1.0-(x))*((x)-2.0))
#define P_00(x)  (3.0*((x)+1.0)*((x)-1.0)*((x)-2.0))
#define P_P1(x)  (3.0*(x)*((x)+1.0)*(2.0-(x)))
#define P_P2(x)  ((x)*((x)+1.0)*((x)-1.0))

/**-------------------------------------------------------------------
    Rotate and shift an image, using bicubic interpolation:
       aa  = shift in x
       bb  = shift in y
       phi = angle in radians
    Sort of a replacement for mri_rotate in mri_warp.c; supposed
    to be faster.  If the input image is MRI_complex, the output
    will be also; otherwise, the output will be MRI_float.
----------------------------------------------------------------------**/

MRI_IMAGE *mri_rota( MRI_IMAGE *im, float aa, float bb, float phi )
{
   float rot_dx , rot_dy , rot_cph , rot_sph , top,bot,val ;
   MRI_IMAGE *imfl , *newImg ;
   MRI_IMARR *impair ;
   float *far , *nar ;
   float xx,yy , fx,fy ;
   int ii,jj, nx,ny , ix,jy , ifx,jfy ;
   float f_jm1,f_j00,f_jp1,f_jp2 , wt_m1,wt_00,wt_p1,wt_p2 ;

#ifdef USE_CGRID
   if( p_first ){
      p_first = 0 ;
      xx      = 1.0 / CGRID ;
      for( ii=0 ; ii <= CGRID ; ii++ ){
         yy       = ii * xx ;
         p_m1[ii] = P_M1(yy) ;
         p_00[ii] = P_00(yy) ;
         p_p1[ii] = P_P1(yy) ;
         p_p2[ii] = P_P2(yy) ;
      }
   }
#endif

   if( im == NULL || ! MRI_IS_2D(im) ){
      fprintf(stderr,"*** mri_rota only works on 2D images!\n") ; EXIT(1) ;
   }

   /** if complex image, break into pairs, do each separately, put back together **/

   if( im->kind == MRI_complex ){
      MRI_IMARR *impair ;
      MRI_IMAGE * rim , * iim , * tim ;
      impair = mri_complex_to_pair( im ) ;
      if( impair == NULL ){
         fprintf(stderr,"*** mri_complex_to_pair fails in mri_rota!\n") ; EXIT(1) ;
      }
      rim = IMAGE_IN_IMARR(impair,0) ;
      iim = IMAGE_IN_IMARR(impair,1) ;  FREE_IMARR(impair) ;
      tim = mri_rota( rim , aa,bb,phi ) ; mri_free( rim ) ; rim = tim ;
      tim = mri_rota( iim , aa,bb,phi ) ; mri_free( iim ) ; iim = tim ;
      newImg = mri_pair_to_complex( rim , iim ) ;
      mri_free( rim ) ; mri_free( iim ) ;
      MRI_COPY_AUX(newImg,im) ;
      return newImg ;
   }

   /** rotation params **/

   rot_cph = cos(phi) ; rot_sph = sin(phi) ;

   rot_dx  = (0.5 * im->nx) * (1.0-rot_cph) - aa*rot_cph - bb*rot_sph
            -(0.5 * im->ny) * rot_sph ;

   rot_dy  = (0.5 * im->nx) * rot_sph + aa*rot_sph - bb*rot_cph
            +(0.5 * im->ny) * (1.0-rot_cph) ;

   /** other initialization **/

   nx = im->nx ;  /* image dimensions */
   ny = im->ny ;

   if( im->kind == MRI_float ) imfl = im ;
   else                        imfl = mri_to_float( im ) ;

   far = MRI_FLOAT_PTR(imfl) ;              /* access to float data */
   newImg = mri_new( nx , nx , MRI_float ) ;   /* output image */
   nar = MRI_FLOAT_PTR(newImg) ;               /* output image data */

   bot = top = far[0] ;
   for( ii=0 ; ii < nx*ny ; ii++ )
           if( far[ii] < bot ) bot = far[ii] ;
      else if( far[ii] > top ) top = far[ii] ;

   /*** loop over output points and warp to them ***/

   for( jj=0 ; jj < nx ; jj++ ){
      xx = rot_sph * jj + rot_dx - rot_cph ;
      yy = rot_cph * jj + rot_dy + rot_sph ;
      for( ii=0 ; ii < nx ; ii++ ){

         xx += rot_cph ;  /* get x,y in original image */
         yy -= rot_sph ;

         ix = (xx >= 0.0) ? ((int) xx) : ((int) xx)-1 ;  /* floor */
         jy = (yy >= 0.0) ? ((int) yy) : ((int) yy)-1 ;

#ifdef USE_CGRID
         ifx   = (xx-ix)*CGRID + 0.499 ;
         wt_m1 = p_m1[ifx] ; wt_00 = p_00[ifx] ;
         wt_p1 = p_p1[ifx] ; wt_p2 = p_p2[ifx] ;
#else
         fx    = xx-ix ;
         wt_m1 = P_M1(fx) ; wt_00 = P_00(fx) ;
         wt_p1 = P_P1(fx) ; wt_p2 = P_P2(fx) ;
#endif

         if( ix > 0 && ix < nx-2 && jy > 0 && jy < ny-2 ){
            float * fym1, *fy00 , *fyp1 , *fyp2 ;

            fym1 = far + (ix-1 + (jy-1)*nx) ;
            fy00 = fym1 + nx ;
            fyp1 = fy00 + nx ;
            fyp2 = fyp1 + nx ;

            f_jm1 =  wt_m1 * fym1[0] + wt_00 * fym1[1]
                   + wt_p1 * fym1[2] + wt_p2 * fym1[3] ;

            f_j00 =  wt_m1 * fy00[0] + wt_00 * fy00[1]
                   + wt_p1 * fy00[2] + wt_p2 * fy00[3] ;

            f_jp1 =  wt_m1 * fyp1[0] + wt_00 * fyp1[1]
                   + wt_p1 * fyp1[2] + wt_p2 * fyp1[3] ;

            f_jp2 =  wt_m1 * fyp2[0] + wt_00 * fyp2[1]
                   + wt_p1 * fyp2[2] + wt_p2 * fyp2[3] ;

         } else {

            f_jm1 =  wt_m1 * FINS(ix-1,jy-1)
                   + wt_00 * FINS(ix  ,jy-1)
                   + wt_p1 * FINS(ix+1,jy-1)
                   + wt_p2 * FINS(ix+2,jy-1) ;

            f_j00 =   wt_m1 * FINS(ix-1,jy)
                    + wt_00 * FINS(ix  ,jy)
                    + wt_p1 * FINS(ix+1,jy)
                    + wt_p2 * FINS(ix+2,jy) ;

            f_jp1 =   wt_m1 * FINS(ix-1,jy+1)
                    + wt_00 * FINS(ix  ,jy+1)
                    + wt_p1 * FINS(ix+1,jy+1)
                    + wt_p2 * FINS(ix+2,jy+1) ;

            f_jp2 =   wt_m1 * FINS(ix-1,jy+2)
                    + wt_00 * FINS(ix  ,jy+2)
                    + wt_p1 * FINS(ix+1,jy+2)
                    + wt_p2 * FINS(ix+2,jy+2) ;
         }

#define THIRTYSIX 2.7777778e-2  /* 1./36.0, actually */

#ifdef USE_CGRID
         jfy = (yy-jy)*CGRID + 0.499 ;
         val = (  p_m1[jfy] * f_jm1 + p_00[jfy] * f_j00
                + p_p1[jfy] * f_jp1 + p_p2[jfy] * f_jp2 ) * THIRTYSIX ;
#else
         fy  = yy-jy ;
         val = (  P_M1(fy) * f_jm1 + P_00(fy) * f_j00
                + P_P1(fy) * f_jp1 + P_P2(fy) * f_jp2 ) * THIRTYSIX ;
#endif

              if( val < bot ) nar[ii+jj*nx] = bot ;  /* too small! */
         else if( val > top ) nar[ii+jj*nx] = top ;  /* too big!   */
         else                 nar[ii+jj*nx] = val ;  /* just right */

      }
   }

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   MRI_COPY_AUX(newImg,im) ;
   return newImg ;
}

/**-------------------------------------------------------------------
    Rotate and shift an image, using bilinear interpolation:
       aa  = shift in x
       bb  = shift in y
       phi = angle in radians
    Sort of a replacement for mri_rotate in mri_warp.c; supposed
    to be faster.  If the input image is MRI_complex, the output
    will be also; otherwise, the output will be MRI_float.
----------------------------------------------------------------------**/

MRI_IMAGE *mri_rota_bilinear( MRI_IMAGE *im, float aa, float bb, float phi )
{
   float rot_dx , rot_dy , rot_cph , rot_sph ;
   MRI_IMAGE *imfl , *newImg ;
   MRI_IMARR *impair ;
   float *far , *nar ;
   float xx,yy , fx,fy ;
   int ii,jj, nx,ny , ix,jy ;
   float f_j00,f_jp1 , wt_00,wt_p1 ;

   if( im == NULL || ! MRI_IS_2D(im) ){
      fprintf(stderr,"*** mri_rota_bilinear only works on 2D images!\n") ; EXIT(1) ;
   }

   /** if complex image, break into pairs, do each separately, put back together **/

   if( im->kind == MRI_complex ){
      MRI_IMARR *impair ;
      MRI_IMAGE * rim , * iim , * tim ;
      impair = mri_complex_to_pair( im ) ;
      if( impair == NULL ){
         fprintf(stderr,"*** mri_complex_to_pair fails in mri_rota!\n") ; EXIT(1) ;
      }
      rim = IMAGE_IN_IMARR(impair,0) ;
      iim = IMAGE_IN_IMARR(impair,1) ;  FREE_IMARR(impair) ;
      tim = mri_rota_bilinear( rim , aa,bb,phi ) ; mri_free( rim ) ; rim = tim ;
      tim = mri_rota_bilinear( iim , aa,bb,phi ) ; mri_free( iim ) ; iim = tim ;
      newImg = mri_pair_to_complex( rim , iim ) ;
      mri_free( rim ) ; mri_free( iim ) ;
      MRI_COPY_AUX(newImg,im) ;
      return newImg ;
   }

   /** rotation params **/

   rot_cph = cos(phi) ; rot_sph = sin(phi) ;

   rot_dx  = (0.5 * im->nx) * (1.0-rot_cph) - aa*rot_cph - bb*rot_sph
            -(0.5 * im->ny) * rot_sph ;

   rot_dy  = (0.5 * im->nx) * rot_sph + aa*rot_sph - bb*rot_cph
            +(0.5 * im->ny) * (1.0-rot_cph) ;

   /** other initialization **/

   nx = im->nx ;  /* image dimensions */
   ny = im->ny ;

   if( im->kind == MRI_float ) imfl = im ;
   else                        imfl = mri_to_float( im ) ;

   far = MRI_FLOAT_PTR(imfl) ;              /* access to float data */
   newImg = mri_new( nx , nx , MRI_float ) ;   /* output image */
   nar = MRI_FLOAT_PTR(newImg) ;               /* output image data */

   /*** loop over output points and warp to them ***/

   for( jj=0 ; jj < nx ; jj++ ){
      xx = rot_sph * jj + rot_dx - rot_cph ;
      yy = rot_cph * jj + rot_dy + rot_sph ;
      for( ii=0 ; ii < nx ; ii++ ){

         xx += rot_cph ;  /* get x,y in original image */
         yy -= rot_sph ;

         ix = (xx >= 0.0) ? ((int) xx) : ((int) xx)-1 ;  /* floor */
         jy = (yy >= 0.0) ? ((int) yy) : ((int) yy)-1 ;

         fx = xx-ix ; wt_00 = 1.0 - fx ; wt_p1 = fx ;

         if( ix >= 0 && ix < nx-1 && jy >= 0 && jy < ny-1 ){
            float *fy00 , *fyp1 ;

            fy00 = far + (ix + jy*nx) ; fyp1 = fy00 + nx ;

            f_j00 = wt_00 * fy00[0] + wt_p1 * fy00[1] ;
            f_jp1 = wt_00 * fyp1[0] + wt_p1 * fyp1[1] ;

         } else {
            f_j00 = wt_00 * FINS(ix,jy  ) + wt_p1 * FINS(ix+1,jy  ) ;
            f_jp1 = wt_00 * FINS(ix,jy+1) + wt_p1 * FINS(ix+1,jy+1) ;
         }

         fy  = yy-jy ; nar[ii+jj*nx] = (1.0-fy) * f_j00 + fy * f_jp1 ;

      }
   }

   /*** cleanup and return ***/

   if( im != imfl ) mri_free(imfl) ;  /* throw away unneeded workspace */
   MRI_COPY_AUX(newImg,im) ;
   return newImg ;
}

/*--------------------------------------------------------------------------
   Routines to rotate using FFTs and the shearing transformation
----------------------------------------------------------------------------*/

/*** Shift 2 rows at a time with the FFT ***/

void ft_shift2( int n, int nup, float af, float * f, float ag, float * g )
{
   static int nupold=0 ;
   static complex * row=NULL , * cf=NULL , * cg=NULL ;

   int ii , nby2=nup/2 , n21=nby2+1 ;
   complex fac , gac ;
   float sf , sg , dk ;

   /* make new memory for row storage? */

   if( nup > nupold ){
      if( row != NULL ){ free(row) ; free(cf) ; free(cg) ; }
      row = (complex *) malloc( sizeof(complex) * nup ) ;
      cf  = (complex *) malloc( sizeof(complex) * n21 ) ;
      cg  = (complex *) malloc( sizeof(complex) * n21 ) ;
      nupold = nup ;
   }

   /* FFT the pair of rows */

   for( ii=0 ; ii < n   ; ii++ ){ row[ii].r = f[ii] ; row[ii].i = g[ii] ; }
   for(      ; ii < nup ; ii++ ){ row[ii].r = row[ii].i = 0.0 ; }

   csfft_cox( -1 , nup , row ) ;

   /* untangle FFT coefficients from row into cf,cg */

   cf[0].r = 2.0 * row[0].r ; cf[0].i = 0.0 ;  /* twice too big */
   cg[0].r = 2.0 * row[0].i ; cg[0].i = 0.0 ;
   for( ii=1 ; ii < nby2 ; ii++ ){
      cf[ii].r =  row[ii].r + row[nup-ii].r ;
      cf[ii].i =  row[ii].i - row[nup-ii].i ;
      cg[ii].r =  row[ii].i + row[nup-ii].i ;
      cg[ii].i = -row[ii].r + row[nup-ii].r ;
   }
   cf[nby2].r = 2.0 * row[nby2].r ; cf[nby2].i = 0.0 ;
   cg[nby2].r = 2.0 * row[nby2].i ; cg[nby2].i = 0.0 ;

   /* phase shift both rows (cf,cg) */

   dk = (2.0*PI) / nup ;
   sf = -af * dk ; sg = -ag * dk ;
   for( ii=1 ; ii <= nby2 ; ii++ ){
      fac = CEXPIT(ii*sf) ; cf[ii] = CMULT( fac , cf[ii] ) ;
      gac = CEXPIT(ii*sg) ; cg[ii] = CMULT( gac , cg[ii] ) ;
   }
   cf[nby2].i = 0.0 ; cg[nby2].i = 0.0 ;

   /* retangle the coefficients from 2 rows */

   row[0].r = cf[0].r ; row[0].i = cg[0].r ;
   for( ii=1 ; ii < nby2 ; ii++ ){
      row[ii].r     =  cf[ii].r - cg[ii].i ;
      row[ii].i     =  cf[ii].i + cg[ii].r ;
      row[nup-ii].r =  cf[ii].r + cg[ii].i ;
      row[nup-ii].i = -cf[ii].i + cg[ii].r ;
   }
   row[nby2].r = cf[nby2].r ;
   row[nby2].i = cg[nby2].r ;

   /* inverse FFT and store back in output arrays */

   csfft_cox( 1 , nup , row ) ;

   sf = 0.5 / nup ;              /* 0.5 to allow for twice too big above */
   for( ii=0 ; ii < n ; ii++ ){
      f[ii] = sf * row[ii].r ; g[ii] = sf * row[ii].i ;
   }

   return ;
}

/*** Shear in the x-direction ***/

void ft_xshear( float a , float b , int nx , int ny , float * f )
{
   int jj , nxup ;
   float * fj0 , * fj1 , * zz=NULL ;
   float a0 , a1 ;

   if( a == 0.0 && b == 0.0 ) return ;          /* nothing to do */
   if( nx < 2 || ny < 1 || f == NULL ) return ; /* nothing to operate on */

   if( ny%2 == 1 ){                               /* we work in pairs, so */
      zz = (float *) malloc( sizeof(float)*nx ) ; /* if not an even number */
      for( jj=0 ; jj < nx ; jj++ ) zz[jj] = 0.0 ; /* of rows, make an extra */
   }

   nxup = nx ;                                 /* min FFT length */
   jj   = 2 ; while( jj < nxup ){ jj *= 2 ; }  /* next power of 2 larger */
   nxup = jj ;

   for( jj=0 ; jj < ny ; jj+=2 ){              /* shear rows in pairs */
      fj0 = f + (jj*nx) ;                      /* row 0 */
      fj1 = (jj < ny-1) ? (fj0 + nx) : zz ;    /* row 1 */
      a0  = a*(jj-0.5*ny) + b ;                /* phase ramp for row 0 */
      a1  = a0 + a ;                           /* phase ramp for row 1 */
      ft_shift2( nx , nxup , a0 , fj0 , a1 , fj1 ) ;
   }

   if( zz != NULL ) free(zz) ; /* toss the trash */
   return ;
}

/*** Shear in the y direction ***/

void ft_yshear( float a , float b , int nx , int ny , float * f )
{
   int jj , nyup , ii ;
   float * fj0 , * fj1 ;
   float a0 , a1 ;

   if( a == 0.0 && b == 0.0 ) return ;          /* nothing to do */
   if( ny < 2 || nx < 1 || f == NULL ) return ; /* nothing to operate on */

   /* make memory for a pair of columns */

   fj0 = (float *) malloc( sizeof(float) * 2*ny ) ; fj1 = fj0 + ny ;

   nyup = ny ;                                 /* min FFT length */
   jj   = 2 ; while( jj < nyup ){ jj *= 2 ; }  /* next power of 2 larger */
   nyup = jj ;

   for( jj=0 ; jj < nx ; jj+=2 ){              /* shear rows in pairs */

      if( jj < nx-1 ){
         for( ii=0; ii < ny; ii++ ){ fj0[ii] = f[jj+ii*nx]; fj1[ii] = f[jj+1+ii*nx]; }
      } else {
         for( ii=0; ii < ny; ii++ ){ fj0[ii] = f[jj+ii*nx]; fj1[ii] = 0.0; }
      }

      a0  = a*(jj-0.5*nx) + b ;                /* phase ramp for row 0 */
      a1  = a0 + a ;                           /* phase ramp for row 1 */
      ft_shift2( ny , nyup , a0 , fj0 , a1 , fj1 ) ;

      if( jj < nx-1 ){
         for( ii=0; ii < ny; ii++ ){ f[jj+ii*nx] = fj0[ii]; f[jj+1+ii*nx] = fj1[ii]; }
      } else {
         for( ii=0; ii < ny; ii++ ){ f[jj+ii*nx] = fj0[ii]; }
      }
   }

   free(fj0) ; return ;
}

/*** Image rotation using 3 shears ***/

MRI_IMAGE * mri_rota_shear( MRI_IMAGE *im, float aa, float bb, float phi )
{
   double cph , sph ;
   float a , b , bot,top ;
   MRI_IMAGE *flim ;
   float *flar ;
   int ii , nxy ;

   if( im == NULL || ! MRI_IS_2D(im) ){
      fprintf(stderr,"*** mri_rota_shear only works on 2D images!\n") ; EXIT(1) ;
   }

   /** if complex image, break into pairs, do each separately, put back together **/

   if( im->kind == MRI_complex ){
      MRI_IMARR *impair ;
      MRI_IMAGE * rim , * iim , * tim ;
      impair = mri_complex_to_pair( im ) ;
      if( impair == NULL ){
         fprintf(stderr,"*** mri_complex_to_pair fails in mri_rota!\n") ; EXIT(1) ;
      }
      rim  = IMAGE_IN_IMARR(impair,0) ;
      iim  = IMAGE_IN_IMARR(impair,1) ;  FREE_IMARR(impair) ;
      tim  = mri_rota_shear( rim , aa,bb,phi ) ; mri_free( rim ) ; rim = tim ;
      tim  = mri_rota_shear( iim , aa,bb,phi ) ; mri_free( iim ) ; iim = tim ;
      flim = mri_pair_to_complex( rim , iim ) ;
      mri_free( rim ) ; mri_free( iim ) ;
      MRI_COPY_AUX(flim,im) ;
      return flim ;
   }

   /** copy input to output **/

   flim = mri_to_float( im ) ;
   flar = MRI_FLOAT_PTR( flim ) ;

   /* find range of image data */

   bot = top = flar[0] ; nxy = im->nx * im->ny ;
   for( ii=1 ; ii < nxy ; ii++ )
           if( flar[ii] < bot ) bot = flar[ii] ;
      else if( flar[ii] > top ) top = flar[ii] ;

   /** rotation params **/

   cph = cos(phi) ; sph = sin(phi) ;

   /* More than 90 degrees?
      Must be reduced to less than 90 degrees by a 180 degree flip. */

   if( cph < 0.0 ){
      int ii , jj , top , nx=flim->nx , ny=flim->ny ;
      float val ;

      top = (nx+1)/2 ;
      for( jj=0 ; jj < ny ; jj++ ){
         for( ii=1 ; ii < top ; ii++ ){
            val               = flar[jj*nx+ii] ;
            flar[jj*nx+ii]    = flar[jj*nx+nx-ii] ;
            flar[jj*nx+nx-ii] = val ;
         }
      }

      top = (ny+1)/2 ;
      for( ii=0 ; ii < nx ; ii++ ){
         for( jj=1 ; jj < top ; jj++ ){
            val                 = flar[ii+jj*nx] ;
            flar[ii+jj*nx]      = flar[ii+(ny-jj)*nx] ;
            flar[ii+(ny-jj)*nx] = val ;
         }
      }

      cph = -cph ; sph = -sph ;
   }

   /* compute shear factors for each direction */

   b = sph ;
   a = (b != 0.0 ) ? ((cph - 1.0) / b) : (0.0) ;

   /* shear thrice */

   ft_xshear( a , 0.0       , im->nx , im->ny , flar ) ;
   ft_yshear( b , bb        , im->nx , im->ny , flar ) ;
   ft_xshear( a , aa - a*bb , im->nx , im->ny , flar ) ;

   /* make sure data does not go out of original range */

   for( ii=0 ; ii < nxy ; ii++ )
           if( flar[ii] < bot ) flar[ii] = bot ;
      else if( flar[ii] > top ) flar[ii] = top ;

   return flim ;
}

MRI_IMAGE * mri_rota_variable( int mode, MRI_IMAGE *im, float aa, float bb, float phi )
{
   switch(mode){

      default:
      case MRI_BICUBIC:  return mri_rota( im,aa,bb,phi ) ;

      case MRI_BILINEAR: return mri_rota_bilinear( im,aa,bb,phi ) ;

      case MRI_FOURIER:  return mri_rota_shear( im,aa,bb,phi ) ;
   }
}
