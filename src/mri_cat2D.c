/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

/*** NOT 7D SAFE ***/

/*--------------------------------------------------------------------
   Function to paste-up a bunch of 2D images into a larger 2D
   array:  0 1  2  3
           4 5  6  7
           8 9 10 11
   At this time, all the images must be the same dimensions and kind.

   Also see mri_catvol.c for linear catenation of 2-3D images, in
   various directions.  For that function, the images only have
   to conform in the necessary directions, not in all directions.
----------------------------------------------------------------------*/
static byte OK_wrap = 0;
static byte WrapZero = 0;
void mri_Set_KO_catwrap(void) { OK_wrap = 0; return; }
void mri_Set_OK_catwrap(void) { OK_wrap = 1; return; }
void mri_Set_OK_catrandwrap(void) { OK_wrap = 2; return; }
void mri_Set_OK_WrapZero(byte vv) { WrapZero = vv; return; }
void mri_Set_KO_WrapZero(void) { WrapZero = 0; return; }

MRI_IMAGE * mri_cat2D(  int mx , int my , int gap ,
                        void *gapval , MRI_IMARR *imar )
{
   int nx , ny , ii , jj , kind , ij , nxout , nyout , ijoff , jout,iout, iijj ;
   MRI_IMAGE *imout , *imin=NULL ;
   void *vout ;
   int warn = 0;
   
ENTRY("mri_cat2D") ;

   /*--- sanity checks ---*/

   if(   mx < 1 || my < 1 || imar == NULL ||
         (!OK_wrap && imar->num < mx*my) )
      RETURN( NULL );
   if( gap < 0 || (gap > 0 && gapval == NULL) )                RETURN( NULL );

   for( ij=0 ; ij < mx*my ; ij++ ){     /* find first non-empty image */
      imin = IMARR_SUBIMAGE(imar,ij%imar->num) ;
      if( imin != NULL ) break ;
   }
   if( ij == mx*my ) RETURN( NULL );      /* all are empty! */

   kind = imin->kind ;
   nx   = imin->nx ;
   ny   = imin->ny ;


   if( mx==1 && my==1 ){                    /* 1x1 case (shouldn't happen) */
      imout = mri_to_mri( kind , imin ) ;   /* Just copy input to output   */
      RETURN( imout );
   }


   for( ij=0 ; ij < mx*my ; ij++ ){     /* check for consistency */
      switch (OK_wrap) {
         default:
         case 0:
            iijj = ij;
            if (iijj >= imar->num) iijj = imar->num-1;
            break;
          case 1:
            iijj = ij%imar->num;
            break;
         case 2:
            if (ij < imar->num) iijj = ij;
            else iijj = (int)(lrand48() % (imar->num));
            break;
      }
      imin = IMARR_SUBIMAGE(imar,iijj) ;
      if( imin != NULL &&
          (imin->kind != kind || imin->nx != nx || imin->ny != ny) )
         RETURN( NULL );
   }

   nxout = mx * nx + (mx-1) * gap ;
   nyout = my * ny + (my-1) * gap ;
   imout = mri_new( nxout , nyout , kind ) ;
   vout  = mri_data_pointer( imout ) ;

   ij = 0 ;
   for( jj=0 ; jj < my ; jj++ ){            /* loop over rows */
      for( ii=0 ; ii < mx ; ii++ , ij++ ){  /* loop over columns */

         if (WrapZero && ij >= imar->num) imin = NULL;
         else {
            switch (OK_wrap) {
               default:
               case 0:
                  iijj = ij;
                  if (iijj >= imar->num) iijj = imar->num-1;
                  break;
               case 1:
                  iijj = ij%imar->num;
                  break;
               case 2:
                  if (ij < imar->num) iijj = ij;
                  else iijj = (int)(lrand48() % (imar->num));
                  break;
            }
            imin  = IMARR_SUBIMAGE(imar,iijj) ;
         }
         ijoff = ii * (nx+gap) + jj * (ny+gap) * nxout ;

         /*--- NULL image ==> fill this spot with zeroes ---*/

         if( imin == NULL || mri_data_pointer(imin) == NULL ){
            switch( kind ){
               case MRI_byte:{
                  byte *pout = ((byte *) vout);
                  byte bb;
                  if (WrapZero == 1) bb = 0;
                  else bb = WrapZero;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                        (void) memset( pout+ijoff , bb , sizeof(byte)*nx ) ;
                  
               } break ;

               case MRI_rgb:{                       /* 11 Feb 1999 */
                  byte *pout = ((byte *) vout);
                  byte bb;
                  if (WrapZero == 1) bb = 0;
                  else bb = WrapZero;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     (void)memset( pout+(3*ijoff) , bb , sizeof(byte)*(3*nx) );
               } break ;

               case MRI_rgba:{                      /* 09 Dec 2014 */
                  rgba *pout = (rgba *)vout ;
                  if (!warn && WrapZero > 1) {
                     fprintf(stderr,"No support for non-zero filling for rgba.\n"
                                    "Setting ignored.\n");
                     ++warn;
                  }
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     (void) memset( pout+ijoff , 0 , sizeof(rgba)*nx ) ;
               } break ;

               case MRI_short:{
                  short *pout = ((short *) vout);
                  if (!warn && WrapZero > 1) {
                     fprintf(stderr,"No support for non-zero filling for short\n"
                                    "Setting ignored.\n");
                     ++warn;
                  }
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     (void) memset( pout+ijoff , 0 , sizeof(short)*nx ) ;
               } break ;

               case MRI_int:{
                  int *pout = ((int *) vout);
                  int bb;
                  if (WrapZero == 1) bb = 0;
                  else bb = WrapZero;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     (void) memset( pout+ijoff , bb , sizeof(int)*nx ) ;
               } break ;

               case MRI_float:{
                  float *pout = ((float *) vout);
                  float ff;
                  if (WrapZero > 1) ff = WrapZero/255.0;
                  else ff = 0.0;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     for( iout=0 ; iout < nx ; iout++ )
                        pout[iout+ijoff] = ff ;
               } break ;

               case MRI_double:{
                  double *pout = ((double *) vout);
                  double ff;
                  if (WrapZero > 1) ff = WrapZero/255.0;
                  else ff = 0.0;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     for( iout=0 ; iout < nx ; iout++ )
                        pout[iout+ijoff] = ff ;
               } break ;

               case MRI_complex:{
                  complex *pout = ((complex *) vout);
                  float ff;
                  if (WrapZero > 1) ff = WrapZero/255.0;
                  else ff = 0.0;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     for( iout=0 ; iout < nx ; iout++ )
                        pout[iout+ijoff].r = pout[iout+ijoff].i = ff ;
               } break ;
            }

         /*--- Copy input image data into place ---*/

         } else {
            switch( kind ){
               case MRI_byte:{
                  byte *pout = ((byte *) vout) ,
                       *pin  =  (byte *) MRI_BYTE_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     memcpy( pout+ijoff , pin , sizeof(byte)*nx ) , pin += nx ;
               } break ;

               case MRI_rgb:{                               /* 11 Feb 1999 */
                  byte *pout = ((byte *) vout) ,
                       *pin  =  (byte *) MRI_RGB_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     memcpy( pout+(3*ijoff) , pin , sizeof(byte)*(3*nx) ) , pin += 3*nx ;
               } break ;

               case MRI_rgba:{                              /* 09 Dec 2014 */
                  rgba *pout = (rgba *)vout ,
                       *pin  = MRI_RGBA_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     memcpy( pout+ijoff , pin , sizeof(rgba)*nx ) , pin += nx ;
               } break ;

               case MRI_short:{
                  short *pout = ((short *) vout) ,
                        *pin  =  (short *) MRI_SHORT_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     memcpy( pout+ijoff , pin , sizeof(short)*nx ) , pin += nx ;
               } break ;

               case MRI_int:{
                  int *pout = ((int *) vout) ,
                      *pin  =  (int *) MRI_INT_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     memcpy( pout+ijoff , pin , sizeof(int)*nx ) , pin += nx ;
               } break ;

               case MRI_float:{
                  float *pout = ((float *) vout) ,
                        *pin  =  (float *) MRI_FLOAT_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     memcpy( pout+ijoff , pin , sizeof(float)*nx ) , pin += nx ;
               } break ;

               case MRI_double:{
                  double *pout = ((double *) vout) ,
                         *pin  =  (double *) MRI_DOUBLE_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     memcpy( pout+ijoff , pin , sizeof(double)*nx ) , pin += nx ;
               } break ;

               case MRI_complex:{
                  complex *pout = ((complex *) vout) ,
                          *pin  =  (complex *) MRI_COMPLEX_PTR(imin) ;
                  for( jout=0 ; jout < ny ; jout++ , ijoff+=nxout )
                     memcpy( pout+ijoff , pin , sizeof(complex)*nx ) , pin += nx ;
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
               byte gval = *((byte *)gapval) , *pout = ((byte *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;

            case MRI_rgba:{      /* 09 Dec 2014 */
              rgba gval = *((rgba *)gapval) , *pout = (rgba *)vout ;
              for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;

            case MRI_rgb:{       /* 11 Feb 1999 */
               byte rval = *(((byte *)gapval)  ) ,
                    gval = *(((byte *)gapval)+1) ,
                    bval = *(((byte *)gapval)+2) , *pout = ((byte *) vout) ;

               for( ij=0 ; ij < ii ; ij++ ){
                  pout[3*(ij+ijoff)  ] = rval ;
                  pout[3*(ij+ijoff)+1] = gval ;
                  pout[3*(ij+ijoff)+2] = bval ;
               }
            } break ;

            case MRI_short:{
               short gval = *((short *)gapval) , *pout = ((short *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;

            case MRI_float:{
               float gval = *((float *)gapval) , *pout = ((float *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;

            case MRI_int:{
               int gval = *((int *)gapval) , *pout = ((int *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;

            case MRI_double:{
               double gval = *((double *)gapval) , *pout = ((double *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;

            case MRI_complex:{
               complex gval = *((complex *)gapval) , *pout = ((complex *) vout) ;
               for( ij=0 ; ij < ii ; ij++ ) pout[ij+ijoff] = gval ;
            } break ;
         }
      }

      /**** put value into gap after each column ****/

      for( ii=0 ; ii < mx-1 ; ii++ ){
         ijoff = nx + ii*(nx+gap) ;
         switch( kind ){
            case MRI_byte:{
               byte gval = *((byte *)gapval) , *pout = ((byte *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;

            case MRI_rgba:{             /* 09 Dec 2014 */
               rgba gval = *((rgba *)gapval) , *pout = ((rgba *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;

            case MRI_rgb:{              /* 11 Feb 1999 */
               byte rval = *(((byte *)gapval)  ) ,
                    gval = *(((byte *)gapval)+1) ,
                    bval = *(((byte *)gapval)+2) , *pout = ((byte *) vout) ;

               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ){
                     pout[3*(jj*nxout+ijoff)  ] = rval ;
                     pout[3*(jj*nxout+ijoff)+1] = gval ;
                     pout[3*(jj*nxout+ijoff)+2] = bval ;
                  }
            } break ;

            case MRI_short:{
               short gval = *((short *)gapval) , *pout = ((short *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;

            case MRI_float:{
               float gval = *((float *)gapval) , *pout = ((float *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;

            case MRI_int:{
               int gval = *((int *)gapval) , *pout = ((int *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;

            case MRI_double:{
               double gval = *((double *)gapval) , *pout = ((double *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;

            case MRI_complex:{
               complex gval = *((complex *)gapval) , *pout = ((complex *) vout) ;
               for( ij=0 ; ij < gap ; ij++ , ijoff++ )
                  for( jj=0 ; jj < nyout ; jj++ ) pout[jj*nxout+ijoff] = gval ;
            } break ;
         }
      }
   }

   RETURN( imout );
}
