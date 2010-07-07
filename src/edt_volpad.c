#include "mrilib.h"

/***** pad a volume by adding/subtracting planes of zeros:
        nxbot = # to add on -x side (can be negative, to remove planes)
        nxtop = # to add on +x side, etc.
       If all n?bot,n?top values are zero, just returns a copy
       of the input.  If too much is cut off, or something else
       bad happens, returns NULL.

       Adapted from thd_zeropad.c       -- 02 Feb 2001 -- RWCox
       Modified to allow uneven padding -- 09 Feb 2001 -- RWCox *****/

void * EDIT_volpad( int nxbot , int nxtop ,
                    int nybot , int nytop ,
                    int nzbot , int nztop ,
                    int nx, int ny, int nz, int ftype, void *fim )
{
   int nxold,nyold,nzold , nxnew,nynew,nznew , nxyold,nxynew ;
   int ii,jj,kk , iv , iibot,iitop , jjbot,jjtop , kkbot,kktop ;
   void *vnew ;

ENTRY("EDIT_volpad") ;

   /*- check for user stupidity -*/

   if( nx <= 0 || ny <= 0 || nz <= 0 || fim == NULL ) RETURN(NULL) ;

   /*- special case: just copy input -*/

   if( nxbot == 0 && nybot == 0 && nzbot == 0 &&
       nxtop == 0 && nytop == 0 && nztop == 0   ){

      vnew = malloc( mri_datum_size(ftype) * nx*ny*nz ) ;
      memcpy( vnew , fim , mri_datum_size(ftype) * nx*ny*nz ) ;
      RETURN(vnew) ;
   }

   nxold = nx ; nxnew = nxold + nxbot + nxtop ;  /* dimensions */
   nyold = ny ; nynew = nyold + nybot + nytop ;
   nzold = nz ; nznew = nzold + nzbot + nztop ;

   nxyold = nxold * nyold ; /* for computing 3D subscripts */
   nxynew = nxnew * nynew ;

   iibot = MAX(0,-nxbot) ; iitop = MIN(nxold,nxold+nxtop) ;  /* range of data  */
   jjbot = MAX(0,-nybot) ; jjtop = MIN(nyold,nyold+nytop) ;  /* in old dataset */
   kkbot = MAX(0,-nzbot) ; kktop = MIN(nzold,nzold+nztop) ;  /* to copy to new */

   if( nxnew < 2 || iibot >= iitop ||   /* check for reasonable sizes */
       nynew < 2 || jjbot >= jjtop ||   /* and ranges of dataset     */
       nznew < 2 || kkbot >= kktop   ){

      ERROR_message("EDIT_volpad: can't cut volume down to nothing!") ;
      RETURN(NULL) ;
   }

   vnew = calloc( nxnew*nynew*nznew , mri_datum_size(ftype) ) ; /* new brick */
   if( vnew == NULL ){
      ERROR_message("EDIT_volpad: Can't malloc space for new array") ;
      RETURN(NULL) ;
   }

   /* macros for computing 1D subscripts from 3D indices */

#undef  SNEW  /* in case was defined in some stupid .h file */
#undef  SOLD
#define SNEW(i,j,k) ((i+nxbot)+(j+nybot)*nxnew+(k+nzbot)*nxynew)
#define SOLD(i,j,k) (i+j*nxold+k*nxyold)

   switch( ftype ){  /* copy rows of old into new */

      default:
         ERROR_message("EDIT_volpad: unknown input datum=%d",ftype) ;
         free(vnew) ;
      RETURN(NULL) ;

      case MRI_byte:{
         byte *bnew = (byte *) vnew, *bold = (byte *) fim ;
         for( kk=kkbot ; kk < kktop ; kk++ )
            for( jj=jjbot ; jj < jjtop ; jj++ )
               for( ii=iibot ; ii < iitop ; ii++ )
                  bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
      }
      break ;

      case MRI_rgb:{
         byte *bnew = (byte *) vnew, *bold = (byte *) fim ;
         for( kk=kkbot ; kk < kktop ; kk++ )
            for( jj=jjbot ; jj < jjtop ; jj++ )
               for( ii=iibot ; ii < iitop ; ii++ ){
                  bnew[3*SNEW(ii,jj,kk)  ] = bold[3*SOLD(ii,jj,kk)  ] ;
                  bnew[3*SNEW(ii,jj,kk)+1] = bold[3*SOLD(ii,jj,kk)+1] ;
                  bnew[3*SNEW(ii,jj,kk)+2] = bold[3*SOLD(ii,jj,kk)+2] ;
               }
      }
      break ;

      case MRI_short:{
         short *bnew = (short *) vnew, *bold = (short *) fim ;
         for( kk=kkbot ; kk < kktop ; kk++ )
            for( jj=jjbot ; jj < jjtop ; jj++ )
               for( ii=iibot ; ii < iitop ; ii++ )
                  bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
      }
      break ;

      case MRI_int:{
         int *bnew = (int *) vnew, *bold = (int *) fim ;
         for( kk=kkbot ; kk < kktop ; kk++ )
            for( jj=jjbot ; jj < jjtop ; jj++ )
               for( ii=iibot ; ii < iitop ; ii++ )
                  bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
      }
      break ;

      case MRI_float:{
         float *bnew = (float *) vnew, *bold = (float *) fim ;
         for( kk=kkbot ; kk < kktop ; kk++ )
            for( jj=jjbot ; jj < jjtop ; jj++ )
               for( ii=iibot ; ii < iitop ; ii++ )
                  bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
      }
      break ;

      case MRI_double:{
         double *bnew = (double *) vnew, *bold = (double *) fim ;
         for( kk=kkbot ; kk < kktop ; kk++ )
            for( jj=jjbot ; jj < jjtop ; jj++ )
               for( ii=iibot ; ii < iitop ; ii++ )
                  bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
      }
      break ;

      case MRI_complex:{
         complex *bnew = (complex *) vnew, *bold = (complex *) fim ;
         for( kk=kkbot ; kk < kktop ; kk++ )
            for( jj=jjbot ; jj < jjtop ; jj++ )
               for( ii=iibot ; ii < iitop ; ii++ )
                  bnew[SNEW(ii,jj,kk)] = bold[SOLD(ii,jj,kk)] ;
      }
      break ;

   } /* end of switch on datum type */

   RETURN(vnew) ;
}

/*------------------------------------------------------------------
  14 Feb 2001: do the above to an image struct, just for fun
--------------------------------------------------------------------*/

MRI_IMAGE * mri_zeropad_3D( int nxbot , int nxtop ,
                            int nybot , int nytop ,
                            int nzbot , int nztop , MRI_IMAGE *im )
{
   MRI_IMAGE *jm ;
   void *var ;

ENTRY("mri_zeropad_3D") ;

   if( im == NULL || !MRI_IS_3D(im) ) RETURN(NULL) ; /* bad */

   /* do the padding work */

   var = EDIT_volpad( nxbot,nxtop,nybot,nytop,nzbot,nztop ,
                      im->nx , im->ny , im->nz ,
                      im->kind , mri_data_pointer(im) ) ;

   if( var == NULL ) RETURN(NULL) ; /* bad */

   /* put padded data into new image */

   jm = mri_new_vol_empty( im->nx + nxbot + nxtop ,
                           im->ny + nybot + nytop ,
                           im->nz + nzbot + nztop , im->kind ) ;
   MRI_COPY_AUX(jm,im) ;
   mri_fix_data_pointer( var , jm ) ;
   RETURN( jm );
}
