/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** NOT 7D SAFE ***/

/*-------------------------------------------------------------------
   MRI image flipper: 8 cases;
     rot    = one of the MRI_ROT_ codes (see mrilib.h)
     mirror = whether to left-right mirror after rotation
   Note that if no rotation is desired, the input and the
   output point to the same place!
---------------------------------------------------------------------*/

MRI_IMAGE * mri_flippo( int rot , int mirror , MRI_IMAGE * im )
{
   MRI_IMAGE * flim ;
   int nx,ny , fopt , nxout,nyout ;
   register int d1,d2,s1,s2,e1,e2,jb , i1,i2 ;
   float new_dx , new_dy ;

   /** sanity check **/

   if( im == NULL ) return NULL ;
   if( rot == MRI_ROT_0 && mirror == FALSE ) return im ;

   if( ! MRI_IS_2D(im) ){
      fprintf(stderr,"\n*** mri_flippo only works with 2D images!\n") ;
      return im ;
   }

   nx     = im->nx ; ny     = im->ny ;
   new_dx = im->dx ; new_dy = im->dy ;

   /* set the values to control the copy order:
        nxout = x dimen in output
        nyout = y dimen in output
        d1    = stepsize in original data in the new x direction
        d2    = stepsize in original data in the new y direction
        s1    = index in original data of new (0,0) point

        also, flip the dimensions for the 90 and 270 rotates
   */

   fopt = (mirror) ? (rot+MRI_FLMADD) : (rot) ;
   switch( fopt ){

      default:  return im ;                       /* should not happen */

      case (MRI_ROT_90):                          /* ROT_90, no mirror */
         nxout = ny ; nyout = nx   ;
         d1    = nx ; s1    = nx-1 ; d2 = -1 ; new_dx = im->dy ; new_dy = im->dx ;
       break ;

      case (MRI_ROT_180):                         /* ROT_180, no mirror */
         nxout = nx  ; nyout = ny      ;
         d1    = -1  ; s1    = nx*ny-1 ; d2 = -nx ;
       break ;

      case (MRI_ROT_270):                         /* ROT_270, no mirror */
         nxout = ny  ; nyout = nx        ;
         d1    = -nx ; s1    = nx*(ny-1) ; d2 = 1 ; new_dx = im->dy ; new_dy = im->dx ;
      break ;

      case (MRI_ROT_0+MRI_FLMADD):                /* ROT_0, mirror */
         nxout = nx  ; nyout = ny   ;
         d1    = -1  ; s1    = nx-1 ; d2 = nx  ;
      break ;

      case (MRI_ROT_90+MRI_FLMADD):               /* ROT_90, mirror */
         nxout = ny  ; nyout = nx ;
         d1    = -nx ; s1    = nx*ny-1 ; d2 = -1  ; new_dx = im->dy ; new_dy = im->dx ;
      break ;

      case (MRI_ROT_180+MRI_FLMADD):              /* ROT_180, mirror */
         nxout = nx  ; nyout = ny        ;
         d1    = 1   ; s1    = nx*(ny-1) ; d2 = -nx ;
      break ;

      case (MRI_ROT_270+MRI_FLMADD):              /* ROT_270, mirror */
         nxout = ny  ; nyout = nx      ;
         d1    = nx  ; s1    = 0  ; d2 = 1 ; new_dx = im->dy ; new_dy = im->dx ;
      break ;

   }

   flim = mri_new( nxout , nyout , im->kind ) ;

   jb = 0 ;
   s2 = 0 ;
   e1 = s1 + nxout * d1 ;
   e2 = s2 + nyout * d2 ;

   switch( im->kind ){

      case MRI_byte:{
         register byte * inar = MRI_BYTE_PTR(im) ;
         register byte * flar = MRI_BYTE_PTR(flim) ;
         for( i2=s2 ; i2 != e2 ; i2 += d2 )
            for( i1=s1 ; i1 != e1 ; i1 += d1 ) flar[jb++] = inar[i2+i1] ;
      }
      break ;

      case MRI_rgb:{                                     /* 11 Feb 1999 */
         register byte * inar = MRI_RGB_PTR(im) ;
         register byte * flar = MRI_RGB_PTR(flim) ;
         for( i2=s2 ; i2 != e2 ; i2 += d2 )
            for( i1=s1 ; i1 != e1 ; i1 += d1 ){
               flar[jb++] = inar[3*(i2+i1)  ] ;
               flar[jb++] = inar[3*(i2+i1)+1] ;
               flar[jb++] = inar[3*(i2+i1)+2] ;
            }
      }
      break ;

      case MRI_short:{
         register short * inar = MRI_SHORT_PTR(im) ;
         register short * flar = MRI_SHORT_PTR(flim) ;
         for( i2=s2 ; i2 != e2 ; i2 += d2 )
            for( i1=s1 ; i1 != e1 ; i1 += d1 ) flar[jb++] = inar[i2+i1] ;
      }
      break ;

      case MRI_int:{
         register int * inar = MRI_INT_PTR(im) ;
         register int * flar = MRI_INT_PTR(flim) ;
         for( i2=s2 ; i2 != e2 ; i2 += d2 )
            for( i1=s1 ; i1 != e1 ; i1 += d1 ) flar[jb++] = inar[i2+i1] ;
      }
      break ;

      case MRI_float:{
         register float * inar = MRI_FLOAT_PTR(im) ;
         register float * flar = MRI_FLOAT_PTR(flim) ;
         for( i2=s2 ; i2 != e2 ; i2 += d2 )
            for( i1=s1 ; i1 != e1 ; i1 += d1 ) flar[jb++] = inar[i2+i1] ;
      }
      break ;

      case MRI_double:{
         register double * inar = MRI_DOUBLE_PTR(im) ;
         register double * flar = MRI_DOUBLE_PTR(flim) ;
         for( i2=s2 ; i2 != e2 ; i2 += d2 )
            for( i1=s1 ; i1 != e1 ; i1 += d1 ) flar[jb++] = inar[i2+i1] ;
      }
      break ;

      case MRI_complex:{
         register complex * inar = MRI_COMPLEX_PTR(im) ;
         register complex * flar = MRI_COMPLEX_PTR(flim) ;
         for( i2=s2 ; i2 != e2 ; i2 += d2 )
            for( i1=s1 ; i1 != e1 ; i1 += d1 ) flar[jb++] = inar[i2+i1] ;
      }
      break ;

   }

   flim->dx = new_dx ; flim->dy = new_dy ; flim->dz = im->dz ;
   return flim ;
}
