#include "mrilib.h"

/*-----------------------------------------------------------------
   Pad a 2D image by adding/subtracting of zeros:
        nxbot = # to add on -x side (can be negative, to remove planes)
        nxtop = # to add on +x side, etc.
       If all n?bot,n?top values are zero, just returns a copy
       of the input.  If too much is cut off, or something else
       bad happens, returns NULL.

       Adapted from edt_volpad.c -- 26 Feb 2001 -- RWCox
-------------------------------------------------------------------*/

MRI_IMAGE * mri_zeropad_2D( int nxbot , int nxtop ,
                            int nybot , int nytop , MRI_IMAGE * fim )
{
   int nxold,nyold , nxnew,nynew , nx,ny ;
   int ii,jj , iibot,iitop , jjbot,jjtop ;
   MRI_IMAGE * vim ;

ENTRY("mri_zeropad_2D") ;

   /*- check for user stupidity -*/

   if( fim == NULL ) RETURN(NULL) ;

   nx = fim->nx ; ny = fim->ny ;

   /*- special case: just copy input -*/

   if( nxbot == 0 && nybot == 0 &&
       nxtop == 0 && nytop == 0    ){

      vim = mri_copy( fim ) ;
      RETURN(vim) ;
   }

   nxold = nx ; nxnew = nxold + nxbot + nxtop ;  /* dimensions */
   nyold = ny ; nynew = nyold + nybot + nytop ;

   iibot = MAX(0,-nxbot) ; iitop = MIN(nxold,nxold+nxtop) ;  /* range of data  */
   jjbot = MAX(0,-nybot) ; jjtop = MIN(nyold,nyold+nytop) ;  /* in old dataset */

   if( nxnew < 1 || iibot >= iitop ||   /* check for reasonable sizes */
       nynew < 1 || jjbot >= jjtop   ){ /* and ranges of dataset     */

      fprintf(stderr,"*** mri_zeropad: can't cut image down to nothing!\n") ;
      RETURN(NULL) ;
   }

   vim = mri_new( nxnew , nynew , fim->kind ) ;
   MRI_COPY_AUX(vim,fim) ;
   memset( mri_data_pointer(vim) , 0 ,
           nxnew*nynew*mri_datum_size(vim->kind) ) ;

   /* macros for computing 1D subscripts from 2D indices */

#undef  SNEW  /* in case was defined in some stupid .h file */
#undef  SOLD
#define SNEW(i,j) ((i+nxbot)+(j+nybot)*nxnew)
#define SOLD(i,j) (i+j*nxold)

   switch( fim->kind ){  /* copy rows of old into new */

      default:
         fprintf(stderr,"*** mri_zeropad: unknown input datum=%d\n",fim->kind) ;
         mri_free(vim) ;
      RETURN(NULL) ;

      case MRI_byte:{
         byte * bnew = MRI_BYTE_PTR(vim), * bold = MRI_BYTE_PTR(fim) ;
         for( jj=jjbot ; jj < jjtop ; jj++ )
            for( ii=iibot ; ii < iitop ; ii++ )
               bnew[SNEW(ii,jj)] = bold[SOLD(ii,jj)] ;
      }
      break ;

      case MRI_rgb:{
         byte * bnew = MRI_RGB_PTR(vim), * bold = MRI_RGB_PTR(fim) ;
         for( jj=jjbot ; jj < jjtop ; jj++ )
            for( ii=iibot ; ii < iitop ; ii++ ){
               bnew[3*SNEW(ii,jj)  ] = bold[3*SOLD(ii,jj)  ] ;
               bnew[3*SNEW(ii,jj)+1] = bold[3*SOLD(ii,jj)+1] ;
               bnew[3*SNEW(ii,jj)+2] = bold[3*SOLD(ii,jj)+2] ;
            }
      }
      break ;

      case MRI_short:{
         short * bnew = MRI_SHORT_PTR(vim), * bold = MRI_SHORT_PTR(fim) ;
         for( jj=jjbot ; jj < jjtop ; jj++ )
            for( ii=iibot ; ii < iitop ; ii++ )
               bnew[SNEW(ii,jj)] = bold[SOLD(ii,jj)] ;
      }
      break ;

      case MRI_int:{
         int * bnew = MRI_INT_PTR(vim), * bold = MRI_INT_PTR(fim) ;
         for( jj=jjbot ; jj < jjtop ; jj++ )
            for( ii=iibot ; ii < iitop ; ii++ )
               bnew[SNEW(ii,jj)] = bold[SOLD(ii,jj)] ;
      }
      break ;

      case MRI_float:{
         float * bnew = MRI_FLOAT_PTR(vim), * bold = MRI_FLOAT_PTR(fim) ;
         for( jj=jjbot ; jj < jjtop ; jj++ )
            for( ii=iibot ; ii < iitop ; ii++ )
               bnew[SNEW(ii,jj)] = bold[SOLD(ii,jj)] ;
      }
      break ;

      case MRI_double:{
         double * bnew = MRI_DOUBLE_PTR(vim), * bold = MRI_DOUBLE_PTR(fim) ;
         for( jj=jjbot ; jj < jjtop ; jj++ )
            for( ii=iibot ; ii < iitop ; ii++ )
               bnew[SNEW(ii,jj)] = bold[SOLD(ii,jj)] ;
      }
      break ;

      case MRI_complex:{
         complex * bnew = MRI_COMPLEX_PTR(vim), * bold = MRI_COMPLEX_PTR(fim) ;
         for( jj=jjbot ; jj < jjtop ; jj++ )
            for( ii=iibot ; ii < iitop ; ii++ )
               bnew[SNEW(ii,jj)] = bold[SOLD(ii,jj)] ;
      }
      break ;

   } /* end of switch on datum type */

   RETURN(vim) ;
}
