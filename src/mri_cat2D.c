#include "mrilib.h"

/*** NOT 7D SAFE ***/

/*--------------------------------------------------------------------
   Function to paste-up a bunch of 2D images into a larger 2D
   array:  0 1  2  3
           4 5  6  7
           8 9 10 11
   At this time, all the images must be the same dimensions and kind.
----------------------------------------------------------------------*/

MRI_IMAGE * mri_cat2D( int mx , int my , int gap , void * gapval , MRI_IMARR * imar )
{
   int nx , ny , ii , jj , kind , ij , nxout , nyout , ijoff , jout,iout ;
   MRI_IMAGE * imout , * imin ;
   void * vout ;

   /*--- sanity checks ---*/

   if( mx < 1 || my < 1 || imar == NULL || imar->num < mx*my ) return NULL ;
   if( gap < 0 || (gap > 0 && gapval == NULL) )                return NULL ;

   for( ij=0 ; ij < mx*my ; ij++ ){     /* find first non-empty image */
      imin = IMARR_SUBIMAGE(imar,ij) ;
      if( imin != NULL ) break ;
   }
   if( ij == mx*my ) return NULL ;      /* all are empty! */

   kind = imin->kind ;
   nx   = imin->nx ;
   ny   = imin->ny ;

   if( mx==1 && my==1 ){
      imout = mri_to_mri( kind , imin ) ;
      return imout ;
   }

   for( ij=0 ; ij < mx*my ; ij++ ){     /* check for consistency */
      imin = IMARR_SUBIMAGE(imar,ij) ;
      if( imin != NULL &&
          (imin->kind != kind || imin->nx != nx || imin->ny != ny) )
         return NULL ;
   }

   nxout = mx * nx + (mx-1) * gap ;
   nyout = my * ny + (my-1) * gap ;
   imout = mri_new( nxout , nyout , kind ) ;
   vout  = mri_data_pointer( imout ) ;

   ij = 0 ;
   for( jj=0 ; jj < my ; jj++ ){            /* loop over rows */
      for( ii=0 ; ii < mx ; ii++ , ij++ ){  /* loop over columns */

         imin  = IMARR_SUBIMAGE(imar,ij) ;
         ijoff = ii * (nx+gap) + jj * (ny+gap) * nxout ;

         /*--- NULL image ==> fill this spot with zeroes ---*/

         if( imin == NULL || mri_data_pointer(imin) == NULL ){
            switch( kind ){
               case MRI_byte:{
                  byte * pout = ((byte *) vout);
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
#ifdef DONT_USE_MEMCPY
                     for( iout=0 ; iout < nx ; iout++ ) pout[iout+ijoff] = 0 ;
#else
                     (void) memset( pout+ijoff , 0 , sizeof(byte)*nx ) ;
#endif
               } break ;
               case MRI_short:{
                  short * pout = ((short *) vout);
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
#ifdef DONT_USE_MEMCPY
                     for( iout=0 ; iout < nx ; iout++ ) pout[iout+ijoff] = 0 ;
#else
                     (void) memset( pout+ijoff , 0 , sizeof(short)*nx ) ;
#endif
               } break ;
               case MRI_int:{
                  int * pout = ((int *) vout);
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
#ifdef DONT_USE_MEMCPY
                     for( iout=0 ; iout < nx ; iout++ ) pout[iout+ijoff] = 0 ;
#else
                     (void) memset( pout+ijoff , 0 , sizeof(int)*nx ) ;
#endif
               } break ;
               case MRI_float:{
                  float * pout = ((float *) vout);
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     for( iout=0 ; iout < nx ; iout++ )
                        pout[iout+ijoff] = 0 ;
               } break ;
               case MRI_double:{
                  double * pout = ((double *) vout);
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     for( iout=0 ; iout < nx ; iout++ )
                        pout[iout+ijoff] = 0 ;
               } break ;
               case MRI_complex:{
                  complex * pout = ((complex *) vout);
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     for( iout=0 ; iout < nx ; iout++ )
                        pout[iout+ijoff].r = pout[iout+ijoff].i = 0 ;
               } break ;
            }

         /*--- Copy input image data into place ---*/

         } else {
            switch( kind ){
               case MRI_byte:{
                  byte * pout = ((byte *) vout) ,
                       * pin  =  (byte *) MRI_BYTE_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
#ifdef DONT_USE_MEMCPY
                     for( iout=0 ; iout < nx ; iout++ , pin++ )
                        pout[iout+ijoff] = *pin ;
#else
                     memcpy( pout+ijoff , pin , sizeof(byte)*nx ) , pin += nx ;
#endif
               } break ;
               case MRI_short:{
                  short * pout = ((short *) vout) ,
                        * pin  =  (short *) MRI_SHORT_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
#ifdef DONT_USE_MEMCPY
                     for( iout=0 ; iout < nx ; iout++ , pin++ )
                        pout[iout+ijoff] = *pin ;
#else
                     memcpy( pout+ijoff , pin , sizeof(short)*nx ) , pin += nx ;
#endif
               } break ;
               case MRI_int:{
                  int * pout = ((int *) vout) ,
                      * pin  =  (int *) MRI_INT_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
#ifdef DONT_USE_MEMCPY
                     for( iout=0 ; iout < nx ; iout++ , pin++ )
                        pout[iout+ijoff] = *pin ;
#else
                     memcpy( pout+ijoff , pin , sizeof(int)*nx ) , pin += nx ;
#endif
               } break ;
               case MRI_float:{
                  float * pout = ((float *) vout) ,
                        * pin  =  (float *) MRI_FLOAT_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
#ifdef DONT_USE_MEMCPY
                     for( iout=0 ; iout < nx ; iout++ , pin++ )
                        pout[iout+ijoff] = *pin ;
#else
                     memcpy( pout+ijoff , pin , sizeof(float)*nx ) , pin += nx ;
#endif
               } break ;
               case MRI_double:{
                  double * pout = ((double *) vout) ,
                         * pin  =  (double *) MRI_DOUBLE_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
#ifdef DONT_USE_MEMCPY
                     for( iout=0 ; iout < nx ; iout++ , pin++ )
                        pout[iout+ijoff] = *pin ;
#else
                     memcpy( pout+ijoff , pin , sizeof(double)*nx ) , pin += nx ;
#endif
               } break ;
               case MRI_complex:{
                  complex * pout = ((complex *) vout) ,
                          * pin  =  (complex *) MRI_COMPLEX_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
#ifdef DONT_USE_MEMCPY
                     for( iout=0 ; iout < nx ; iout++ , pin++ )
                        pout[iout+ijoff] = *pin ;
#else
                     memcpy( pout+ijoff , pin , sizeof(complex)*nx ) , pin += nx ;
#endif
               } break ;
            }
         }
      }
   }

   /*******************  Deal with the gaps  *******************/

   if( gap > 0 ){

      /**** put value into gap after each row ****/

      ii = nxout * gap ;
      for( jj=0 ; jj < my-1 ; jj++ ){
         ijoff = (ny + jj * (ny+gap)) * nxout ;
         switch( kind ){
            case MRI_byte:{
               byte gval = *((byte *)gapval) , * pout = ((byte *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;
            case MRI_short:{
               short gval = *((short *)gapval) , * pout = ((short *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;
            case MRI_float:{
               float gval = *((float *)gapval) , * pout = ((float *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;
            case MRI_int:{
               int gval = *((int *)gapval) , * pout = ((int *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;
            case MRI_double:{
               double gval = *((double *)gapval) , * pout = ((double *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;
            case MRI_complex:{
               complex gval = *((complex *)gapval) , * pout = ((complex *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;
         }
      }

      /**** put value into gap after each column ****/

      for( ii=0 ; ii < mx-1 ; ii++ ){
         ijoff = nx + ii*(nx+gap) ;
         switch( kind ){
            case MRI_byte:{
               byte gval = *((byte *)gapval) , * pout = ((byte *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;
            case MRI_short:{
               short gval = *((short *)gapval) , * pout = ((short *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;
            case MRI_float:{
               float gval = *((float *)gapval) , * pout = ((float *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;
            case MRI_int:{
               int gval = *((int *)gapval) , * pout = ((int *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;
            case MRI_double:{
               double gval = *((double *)gapval) , * pout = ((double *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;
            case MRI_complex:{
               complex gval = *((complex *)gapval) , * pout = ((complex *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;
         }
      }
   }

   return imout ;
}
