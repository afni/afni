/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** NOT 7D SAFE ***/

/*** prototype ***/

void nonmax_kill( int , int , float * ) ;

/*-------------------------------------------------------------------------
  Do Sobel edge enhancement on an image.  Output image will be floats.
  nloc = distance over which to suppress filter outputs which are not
         local maxima: _individual applies to each separate filter,
                       _collective applies to the merger of all 4 filters;
         (either or both may be zero).
---------------------------------------------------------------------------*/

MRI_IMAGE * mri_sobel( int nloc_individual , int nloc_collective , MRI_IMAGE * imin )
{
   MRI_IMAGE * imfl , * imout ;
   float * flin , * flout , * fjj,*fjp,*fjm ;
   int ii , jj , joff , nx,ny , ij , nij , nloc ;
   register float sxx,syy,sdd,see ;

   MRI_IMAGE * imxx , * imyy , * imdd , * imee ;
   float     * flxx , * flyy , * fldd , * flee ;

#ifdef DEBUG
printf("Entry: mri_sobel\n") ;
#endif

/*** setup input and output images ***/

   if( imin == NULL || ! MRI_IS_2D(imin) ){
      fprintf(stderr,"\n*** mri_sobel only works on 2D images!\n") ;
      EXIT(1) ;
   }

   nx = imin->nx ;
   ny = imin->ny ;
   if( imin->kind == MRI_float ) imfl = imin ;
   else                          imfl = mri_to_float( imin ) ;

   imout = mri_new( nx , ny , MRI_float ) ;
   flout = mri_data_pointer( imout ) ;
   flin  = mri_data_pointer( imfl ) ;

/*** setup arrays for each direction ***/

   imxx = mri_new( nx , ny , MRI_float ) ; flxx = mri_data_pointer( imxx ) ;
   imyy = mri_new( nx , ny , MRI_float ) ; flyy = mri_data_pointer( imyy ) ;
   imdd = mri_new( nx , ny , MRI_float ) ; fldd = mri_data_pointer( imdd ) ;
   imee = mri_new( nx , ny , MRI_float ) ; flee = mri_data_pointer( imee ) ;

   /*** initialize edges to be zeros ***/

   for( jj=0 ; jj < ny ; jj++ ){
      joff = nx * jj ;
      flxx[joff]      = flyy[joff]      = fldd[joff]      = flee[joff]     = 0;
      flxx[joff+nx-1] = flyy[joff+nx-1] = fldd[joff+nx-1] = flee[joff+nx-1]= 0;
   }
   for( ii=0 ; ii < nx ; ii++ ){
      flxx[ii]      = flyy[ii]      = fldd[ii]      = flee[ii]      = 0;
      flxx[joff+ii] = flyy[joff+ii] = fldd[joff+ii] = flee[joff+ii] = 0;
   }

/*** do scan over interior points to produce images for each direction ***/

   for( jj=1 ; jj < ny-1 ; jj++ ){

      joff = nx * jj ;
      fjj  = &flin[joff] ;
      fjm  = &flin[joff-nx] ;
      fjp  = &flin[joff+nx] ;

      for( ii=1 ; ii < nx-1 ; ii++ ){

         sxx = 2.0 * ( fjj[ii+1] - fjj[ii-1] )
                   + ( fjp[ii+1] + fjm[ii+1] - fjp[ii-1] - fjm[ii-1] ) ;

         syy = 2.0 * ( fjp[ii] - fjm[ii] )
                   + ( fjp[ii+1] + fjp[ii-1] - fjm[ii+1] - fjm[ii-1] ) ;

         sdd = 2.0 * ( fjp[ii+1] - fjm[ii-1] )
                   + ( fjj[ii+1] + fjp[ii] - fjj[ii-1] - fjm[ii] ) ;

         see = 2.0 * ( fjp[ii-1] - fjm[ii+1] )
                   + ( fjp[ii] + fjj[ii-1] - fjm[ii] - fjj[ii+1] ) ;

         flxx[ii+joff] = fabs(sxx) ; flyy[ii+joff] = fabs(syy) ;
         fldd[ii+joff] = fabs(sdd) ; flee[ii+joff] = fabs(see) ;
      }
   }

   if( imfl != imin ) mri_free( imfl ) ;  /* don't need anymore */

   /*.......................................................................*/
   /*** if nloc > 0, scan over +/- nloc points and kill non maxima points ***/

   nloc = nloc_individual ;
   if( nloc > 0 ){

      /*** do xx ***/

      for( jj=1 ; jj < ny-1 ; jj++ ){
         joff = jj * nx ;
         nonmax_kill( nloc , nx , &flxx[joff] ) ;
      }

       /*** do yy ***/

      for( ii=1 ; ii < nx-1 ; ii++ ){
         for( jj=0 ; jj < ny ; jj++ ) flout[jj] = flyy[jj*nx+ii] ;
         nonmax_kill( nloc , ny , flout ) ;
         for( jj=0 ; jj < ny ; jj++ ) flyy[jj*nx+ii] = flout[jj] ;
      }

      /*** do dd: here, ij = ii-jj ***/

      for( ij = -(ny-3) ; ij < nx-2 ; ij++ ){

         ii = MAX( 0 , ij ) ; jj = ii - ij ;

         for( nij=0 ; (ii<nx) && (jj<ny) ; nij++ , ii++ , jj++ ){
            flout[nij] = fldd[ii+nx*jj] ;
         }

         nonmax_kill( nloc , nij , flout ) ;

         ii = MAX( 0 , ij ) ; jj = ii - ij ;

         for( nij=0 ; (ii<nx) && (jj<ny) ; nij++ , ii++ , jj++ ){
            fldd[ii+nx*jj] = flout[nij] ;
         }
      }

      /*** do ee: here, ij = ii+jj ***/

      for( ij = 2 ; ij < (nx+ny-4) ; ij++ ){

         jj = MIN( ij , ny-1 ) ; ii = ij - jj ;

         for( nij=0 ; (ii<nx) && (jj>=0) ; nij++ , ii++ , jj-- ){
            flout[nij] = flee[ii+nx*jj] ;
         }

         nonmax_kill( nloc , nij , flout ) ;

         jj = MIN( ij , ny-1 ) ; ii = ij - jj ;

         for( nij=0 ; (ii<nx) && (jj>=0) ; nij++ , ii++ , jj-- ){
            flee[ii+nx*jj] = flout[nij] ;
         }
      }

   }  /* end if( nloc > 0 ) */

   /*.......................................................................*/
   /*** assign maximum of outputs at a given location to the final result ***/

   for( jj=0 ; jj < ny ; jj++ ){                 /* clear edges */
      joff = nx * jj ;
      flout[joff] = flout[joff+nx-1] = 0.0 ;
   }
   for( ii=0 ; ii < nx ; ii++ ){
      flout[ii] = flout[joff+ii] = 0.0 ;
   }

   for( jj=1 ; jj < ny-1 ; jj++ ){
      joff = nx * jj ;
      for( ii=1 ; ii < nx-1 ; ii++ ){
         sxx            = MAX( flxx[ii+joff] , flyy[ii+joff] ) ;
         sdd            = MAX( fldd[ii+joff] , flee[ii+joff] ) ;
         flout[ii+joff] = MAX( sxx           , sdd           ) ;
      }
   }

   /*.......................................................................*/

   nloc = nloc_collective ;
   if( nloc > 0 ){
      int xx,yy , xbot,xtop ,  ybot,ytop , dx,dy ;
      float val ;

      for( jj=1 ; jj < ny-1 ; jj++ ){
         joff = nx * jj ;
         ybot = MAX(0   ,jj-nloc) ;
         ytop = MIN(ny-1,jj+nloc) ;

         for( ii=1 ; ii < nx-1 ; ii++ ){
            xbot = MAX(0   ,ii-nloc) ;
            xtop = MIN(nx-1,ii+nloc) ;

            sxx = flxx[ii+joff] ; syy = flyy[ii+joff] ;
            sdd = fldd[ii+joff] ; see = flee[ii+joff] ; val = flout[ii+joff] ;

                 if( val == sxx ){ dx = 1 ; dy =  0 ; }
            else if( val == syy ){ dx = 0 ; dy =  1 ; }
            else if( val == sdd ){ dx = 1 ; dy =  1 ; }
            else                 { dx = 1 ; dy = -1 ; }

            for( xx=ii+dx , yy=jj+dy ;
                 (xx <= xtop) && (yy >= ybot) && (yy <= ytop) ;
                 xx += dx , yy+= dy ) val = MAX(val,flout[xx+nx*yy]) ;

            for( xx=ii-dx , yy=jj-dy ;
                 (xx >= xbot) && (yy >= ybot) && (yy <= ytop) ;
                 xx -= dx , yy-= dy ) val = MAX(val,flout[xx+nx*yy]) ;

            if( flout[ii+joff] < val ) flout[ii+joff] = 0.0 ;

         }  /* end for ii */
      }  /* end for jj */

      /*** now, destroy points in flout with no neighbors ***/

      for( ii=0 ; ii < nx*ny ; ii++ ) flee[ii] = 0.0 ;

      for( jj=1 ; jj < ny-1 ; jj++ ){
         joff = nx * jj ;
         for( ii=1 ; ii < nx-1 ; ii++ ){
            xx = ii+joff ;
            if( flout[xx] == 0.0 ) continue ;

            if( flout[xx-1]    != 0.0 || flout[xx+1]    != 0.0 ||
                flout[xx+nx]   != 0.0 || flout[xx-nx]   != 0.0 ||
                flout[xx+nx+1] != 0.0 || flout[xx+nx-1] != 0.0 ||
                flout[xx-nx+1] != 0.0 || flout[xx-nx-1] != 0.0   ) flee[xx] = flout[xx] ;
         }
      }

      for( ii=0 ; ii < nx*ny ; ii++ ) flout[ii] = flee[ii] ;

   }  /* end if nloc */

   /*........................................................................*/

   mri_free(imxx) ; mri_free(imyy) ; mri_free(imdd) ; mri_free(imee) ;

   MRI_COPY_AUX(imout,imin) ;
   return imout ;
}

/*--------------------------------------------------------------------------*/

void nonmax_kill( int nloc , int npt , float * ar )
{
            int ii ;
   register int jj , jbot,jtop ;
   register float val ;

   for( ii=0 ; ii < npt ; ii++ ){

      jbot = MAX( 0   , ii-nloc ) ;
      jtop = MIN( npt , ii+nloc ) ;
      val  = ar[jbot++] ;
      for( jj=jbot ; jj < jtop ; jj++ ) val = MAX(val,ar[jj]) ;

      if( ar[ii] != val ) ar[ii] = 0.0 ;
   }

   return ;
}

/*----------------------------------------------------------------
  Sharpen an image;  method same as "pgmenhance".
  phi is a parameter between 0.0 and 1.0: larger --> more sharp.
  logify is a flag: nonzero --> take log, sharpen, then exp
   (e.g., homomorphic filtering).
------------------------------------------------------------------*/

MRI_IMAGE * mri_sharpen( float phi , int logify , MRI_IMAGE * im )
{
   int ii,jj , nx , ny , joff,ijoff , npix ;
   MRI_IMAGE * flim , * outim ;
   float * flar , * outar ;
   float nphi , omphi , sum , bot,top ;

#ifdef DEBUG
printf("Entry: mri_sharpen\n") ;
#endif

   if( phi <= 0.0 || phi >= 1.0 ){
      fprintf(stderr,"*** mri_sharpen: illegal phi=%g\n",phi) ;
      return NULL ;
   }

   if( im->kind == MRI_float && !logify ){
      flim = im ;
   } else {
      flim = mri_to_float( im ) ;
   }
   flar = mri_data_pointer( flim ) ;

   nx = flim->nx ; ny = flim->ny ; npix = nx*ny ;
   outim = mri_new( nx , ny , MRI_float ) ;
   outar = mri_data_pointer( outim ) ;

   if( logify ){
      for( ii=0 ; ii < npix ; ii++ ) flar[ii] = log( fabs(flar[ii])+1.0 ) ;
   }

   for( ii=0 ; ii < nx ; ii++ ) outar[ii] = flar[ii] ;  /* copy 1st row */

   nphi  = phi / 9.0 ;
   omphi = 1.0/(1.0-phi) ;
   bot   = mri_min(flim) ;
   top   = mri_max(flim) ;

   for( jj=1 ; jj < ny-1 ; jj++ ){
      joff = jj * nx ;

      outar[joff]      = flar[joff] ;       /* copy 1st and last columns */
      outar[joff+nx-1] = flar[joff+nx-1] ;

      for( ii=1 ; ii < nx-1 ; ii++ ){       /* filter all intermediate points */
         ijoff = joff + ii ;

         sum = flar[ijoff-nx-1] + flar[ijoff-nx] + flar[ijoff-nx+1]
              +flar[ijoff-1]    + flar[ijoff]    + flar[ijoff+1]
              +flar[ijoff+nx-1] + flar[ijoff+nx] + flar[ijoff+nx+1] ;

         outar[ijoff] = (flar[ijoff] - nphi*sum) * omphi ;

              if( outar[ijoff] < bot ) outar[ijoff] = bot ;
         else if( outar[ijoff] > top ) outar[ijoff] = top ;
      }
   }

   joff = (ny-1) * nx ;
   for( ii=0 ; ii < nx ; ii++ ) outar[joff+ii] = flar[joff+ii] ;  /* copy last row */

   if( logify ){
      for( ii=0 ; ii < npix ; ii++ ) outar[ii] = exp(outar[ii]) ;
   }


   if( flim != im ) mri_free( flim ) ;
   return outim ;
}
