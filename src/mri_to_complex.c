/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "mrilib.h"

/*** PARTLY 7D SAFE ***/

/*****
       mri_to_complex      = creates a complex image from input    (7D SAFE)
       mri_to_complex_ext  = extended version allows a new size    (NOT 7D SAFE)
       mri_pair_to_complex = creates a complex image from 2 inputs (7D SAFE)
       mri_complex_to_pair = creates 2 float images from 1 complex (7D SAFE)
*****/

MRI_IMAGE *mri_to_complex( MRI_IMAGE *oldim )
{
   MRI_IMAGE *newim ;
   register int ii , npix ;

ENTRY("mri_to_complex") ;

   if( oldim == NULL ) RETURN( NULL );  /* 09 Feb 1999 */

   newim = mri_new_conforming( oldim , MRI_complex ) ;
   npix  = oldim->nvox ;

   switch( oldim->kind ){

      case MRI_byte:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.complex_data[ii].r = oldim->im.byte_data[ii] ;
         break ;

      case MRI_short:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.complex_data[ii].r = oldim->im.short_data[ii] ;
         break ;

      case MRI_int:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.complex_data[ii].r = oldim->im.int_data[ii] ;
         break ;

      case MRI_float:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.complex_data[ii].r = oldim->im.float_data[ii] ;
         break ;

      case MRI_double:
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.complex_data[ii].r = oldim->im.double_data[ii] ;
         break ;

      case MRI_complex:
#ifdef DONT_USE_MEMCPY
         for( ii=0 ; ii < npix ; ii++ )
            newim->im.complex_data[ii] = oldim->im.complex_data[ii] ;
#else
         (void) memcpy( newim->im.complex_data ,
                        oldim->im.complex_data , sizeof(complex)*npix ) ;
#endif
         break ;

      case MRI_rgb:{                          /* 16 Jun 2000 */
         byte * rgb = MRI_RGB_PTR(oldim) ;
         complex * car = MRI_COMPLEX_PTR(newim) ;
         for( ii=0 ; ii < npix ; ii++ ){      /* scale to brightness */
            car[ii].r =  0.299 * rgb[3*ii]    /* between 0 and 255   */
                       + 0.587 * rgb[3*ii+1]
                       + 0.114 * rgb[3*ii+2] ;
            car[ii].i = 0.0 ;
         }
      }
      break ;

      default:
         fprintf( stderr , "mri_to_complex:  unrecognized image kind\n" ) ;
         MRI_FATAL_ERROR ;
   }

   if( oldim->kind != MRI_complex ){
      for( ii=0 ; ii < npix ; ii++ )
         newim->im.complex_data[ii].i = 0.0 ;
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}

/************************************************************************
  xnew = new x dimension
  ynew = new y dimension
  if altern is non-zero, multiplies element (i,j) by (-1)^(i+j)
  [to shift center of FFT space to center of grid]
*************************************************************************/

MRI_IMAGE *mri_to_complex_ext( MRI_IMAGE *oldim, int xnew, int ynew, int altern )
{
   MRI_IMAGE *newim ;
   int oldx,oldy , itop,jtop , ii,jj , jbold,jbnew ;

ENTRY("mri_to_complex_ext") ;

   if( oldim == NULL ) RETURN( NULL );  /* 09 Feb 1999 */

   if( ! MRI_IS_2D(oldim) ){
     fprintf(stderr,"\n*** mri_to_complex_ext only works on 2D images\n") ;
     RETURN( NULL );
   }

   oldx = oldim->nx ;
   oldy = oldim->ny ;

   itop = (xnew<oldx) ? xnew : oldx ;  /* smallest x dimension */
   jtop = (ynew<oldy) ? ynew : oldy ;  /* smallest y dimension */

   newim = mri_new( xnew , ynew , MRI_complex ) ;

   /*** copy 0..itop by 0..jtop from old into new ***/

   for( jj=0 ; jj < jtop ; jj++ ){
      jbold = oldx * jj ;
      jbnew = xnew * jj ;
      for( ii=0 ; ii < itop ; ii++ ){
         newim->im.complex_data[ii+jbnew].i = 0.0 ;

         switch( oldim->kind ){
            case MRI_byte:
              newim->im.complex_data[ii+jbnew].r =
                 oldim->im.byte_data[ii+jbold] ;
              break ;
            case MRI_short:
              newim->im.complex_data[ii+jbnew].r =
                oldim->im.short_data[ii+jbold] ;
              break ;
            case MRI_int:
              newim->im.complex_data[ii+jbnew].r =
                  oldim->im.int_data[ii+jbold] ;
              break ;
            case MRI_float:
              newim->im.complex_data[ii+jbnew].r =
                oldim->im.float_data[ii+jbold] ;
              break ;
            case MRI_double:
              newim->im.complex_data[ii+jbnew].r =
               oldim->im.double_data[ii+jbold] ;
              break ;
            case MRI_complex:
              newim->im.complex_data[ii+jbnew] =
                oldim->im.complex_data[ii+jbold] ;
              break ;
         }
      }
   }

   /*** if old image is smaller in x, extend all x rows with zeros ***/

   if( oldx < xnew ){
      for( jj=0 ; jj < jtop ; jj++ ){
         jbnew = jj * xnew ;
         for( ii=oldx ; ii < xnew ; ii++ ){
            newim->im.complex_data[ii+jbnew].r = 0.0 ;
            newim->im.complex_data[ii+jbnew].i = 0.0 ;
         }
      }
   }

   /*** if old image is smaller in y, fill out last rows with zeros ***/

   for( jj=oldy ; jj < ynew ; jj++ ){
      jbnew = jj * xnew ;
      for( ii=0 ; ii < xnew ; ii++ ){
         newim->im.complex_data[ii+jbnew].r = 0.0 ;
         newim->im.complex_data[ii+jbnew].i = 0.0 ;
      }
   }

   if( altern ){
      for( jj=0 ; jj < ynew ; jj++ ){
         jbnew = jj * xnew ;
         for( ii=0 ; ii < xnew ; ii++ ){
            if( (ii+jj)%2 ){
               newim->im.complex_data[ii+jbnew].r = - newim->im.complex_data[ii+jbnew].r ;
               newim->im.complex_data[ii+jbnew].i = - newim->im.complex_data[ii+jbnew].i ;
            }
         }
      }
   }

   MRI_COPY_AUX(newim,oldim) ;
   RETURN( newim );
}

/*****************************************************************************/

MRI_IMAGE * mri_pair_to_complex( MRI_IMAGE * rim , MRI_IMAGE * iim )
{
   MRI_IMAGE * cim , * rfim , * ifim ;
   register complex * car ;
   register float   * rar , * iar ;
   register int ii , nvox ;

ENTRY("mri_pair_to_complex") ;

   if( rim == NULL || iim == NULL || rim->nvox != iim->nvox ) RETURN( NULL );

   cim = mri_new_conforming( rim , MRI_complex ) ;
   car = MRI_COMPLEX_PTR(cim) ;

   rfim = (rim->kind == MRI_float) ? rim : mri_to_float( rim ) ;
   ifim = (iim->kind == MRI_float) ? iim : mri_to_float( iim ) ;

   rar  = MRI_FLOAT_PTR(rfim) ; iar = MRI_FLOAT_PTR(ifim) ;
   nvox = rfim->nvox ;

   for( ii=0 ; ii < nvox ; ii++ ){ car[ii].r = rar[ii] ; car[ii].i = iar[ii] ; }

   if( rfim != rim ) mri_free( rfim ) ;
   if( ifim != iim ) mri_free( ifim ) ;

   RETURN( cim );
}

/*****************************************************************************/

MRI_IMARR * mri_complex_to_pair( MRI_IMAGE * cim )
{
   MRI_IMARR * imarr ;
   MRI_IMAGE * rim , * iim ;
   register int ii , nvox ;
   register float * rar , * iar ;
   register complex * car ;

ENTRY("mri_complex_to_pair") ;

   if( cim == NULL || cim->kind != MRI_complex ) RETURN( NULL );

   rim  = mri_new_conforming( cim , MRI_float ) ; rar = MRI_FLOAT_PTR(rim) ;
   iim  = mri_new_conforming( cim , MRI_float ) ; iar = MRI_FLOAT_PTR(iim) ;
   car  = MRI_COMPLEX_PTR(cim) ;
   nvox = cim->nvox ;

   for( ii=0 ; ii < nvox ; ii++ ){ rar[ii] = car[ii].r ; iar[ii] = car[ii].i ; }

   INIT_IMARR(imarr) ;
   ADDTO_IMARR(imarr,rim) ;
   ADDTO_IMARR(imarr,iim) ;

   RETURN( imarr );
}
