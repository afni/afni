#include "mrilib.h"
#include "string.h"

/*** NOT 7D SAFE ***/

/** only works on short and byte images **/

MRI_IMAGE * mri_nsize( MRI_IMAGE * imin )
{
   MRI_IMAGE * imout = NULL ;
   int nx , ny , ntop , nxpad , nypad , ix,jy,ioff , ii;

   if( imin == NULL ){
      fprintf(stderr,"\n*** mri_nsize: NULL image passed as input!\n") ;
      return NULL ;
   }

   if( ! MRI_IS_2D(imin) ){
      fprintf(stderr,"\n*** mri_nsize only works on 2D images!\n") ;
      exit(1) ;
   }

   nx   = imin->nx ;  ny = imin->ny ;
   ntop = MAX(nx,ny) ;

        if( ntop <=  32 ) ntop =  32 ;  /* next power of 2 */
   else if( ntop <=  64 ) ntop =  64 ;
   else if( ntop <= 128 ) ntop = 128 ;
   else if( ntop <= 256 ) ntop = 256 ;
   else if( ntop <= 512 ) ntop = 512 ;
   else if( ntop <=1024 ) ntop =1024 ;
   else {
      fprintf(stderr,"\n*** mri_nsize: cannot scale up %d x %d images!\n",nx,ny) ;
      return NULL ;
   }

   switch( imin->kind ){

      case MRI_short:{
         short * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_short ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii] = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;

      case MRI_byte:{
         byte * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_byte ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii] = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;

      case MRI_int:{
         int * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_int ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii] = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;

      case MRI_float:{
         float * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_float ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii] = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;

      case MRI_double:{
         double * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_double ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii] = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;

      case MRI_complex:{
         complex * ptin , * ptout ;
         imout = mri_new( ntop,ntop , MRI_complex ) ;
         ptin  = mri_data_pointer( imin ) ;
         ptout = mri_data_pointer( imout ) ;

         for( ii=0 ; ii < ntop*ntop ; ii++ ) ptout[ii].r = ptout[ii].i = 0 ;

         nxpad = (ntop-nx) / 2 ;
         nypad = (ntop-ny) / 2 ;

         for( jy=0 ; jy < ny ; jy++ ){
            ioff = (jy+nypad) * ntop + nxpad ;
            for( ix=0 ; ix < nx ; ix++ )
               ptout[ix+ioff] = ptin[ix+jy*nx] ;
         }
      }
      break ;
   }

   MRI_COPY_AUX(imout,imin) ;
   return imout ;
}
