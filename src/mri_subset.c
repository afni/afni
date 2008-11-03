#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/*! Create a subset of the input image, where certain x-rows are extracted:
    -  ima   = 2D image
    -  ng    = number of x-rows to extract
    -  glist = list of x-rows to extract
*//*-------------------------------------------------------------------------*/

MRI_IMAGE * mri_subset_x2D( int ng , int *glist , MRI_IMAGE *ima )
{
   MRI_IMAGE *imb ;
   char *aar , *bar , *apt , *bpt ;
   int nxa , nxb , ny , ii , jj , ps , ia ;

ENTRY("mri_subset_x2D") ;

   if( ima == NULL || ng < 1 || glist == NULL ) RETURN(NULL) ;
   nxa = ima->nx ; ny = ima->ny ; nxb = ng ;
   imb = mri_new( nxb , ny , ima->kind ) ;
   aar = (char *)mri_data_pointer(ima) ;
   bar = (char *)mri_data_pointer(imb) ;
   ps  = ima->pixel_size ;

   for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nxb ; ii++ ){
       ia = glist[ii] ; if( ia < 0 || ia >= nxa ) continue ;

       apt = aar + ps*(ia+jj*nxa) ;
       bpt = bar + ps*(ii+jj*nxb) ;

       switch( ps ){  /* copy ps bytes */
         default:  memcpy(bpt,apt,ps) ; break ;

         case 4: *bpt++ = *apt++ ; /* fall thru */
         case 3: *bpt++ = *apt++ ; /* fall thru */
         case 2: *bpt++ = *apt++ ; /* fall thru */
         case 1: *bpt++ = *apt++ ; break ;
       }
     }
   }

   MRI_COPY_AUX(imb,ima) ; RETURN(imb) ;
}
