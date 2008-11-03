#include "mrilib.h"

/* Interleave two images in a checkerboard */

MRI_IMAGE * mri_check_2D( int bsiz , MRI_IMAGE *ima , MRI_IMAGE *imb )
{
   MRI_IMAGE *imc ;
   int nx , ny , ii , jj , ib,jb , ps , kk ;
   char *aar , *bar , *car , *sar ;

ENTRY("mri_checkboard") ;

   if( ima == NULL || imb == NULL ) RETURN(NULL) ;
   nx = ima->nx ; ny = ima->ny ;
   if( imb->nx != nx || imb->ny != ny || ima->kind != imb->kind ) RETURN(NULL);

   if( bsiz < 1 ) bsiz = (int)sqrt(0.5*(nx+ny)) ;

   aar = (char *)mri_data_pointer(ima) ; if( aar == NULL ) RETURN(NULL) ;
   bar = (char *)mri_data_pointer(imb) ; if( bar == NULL ) RETURN(NULL) ;
   ps  = ima->pixel_size ;
   imc = mri_new( nx , ny , ima->kind ) ;
   car = (char *)mri_data_pointer(imc) ;

   for( kk=jj=0 ; jj < ny ; jj++ ){
     jb = (jj/bsiz) % 2 ;
     for( ii=0 ; ii < nx ; ii++ ){
       ib  = (ii/bsiz) % 2 ;
       sar = (ib==jb) ? aar : bar ;  /* choose source image */

       switch( ps ){  /* copy ps bytes from source to result */
         default:
           memcpy( car+kk , sar+kk , ps ) ; kk += ps ;
         break ;

         case 4: car[kk] = sar[kk] ; kk++ ; /* fall thru */
         case 3: car[kk] = sar[kk] ; kk++ ; /* fall thru */
         case 2: car[kk] = sar[kk] ; kk++ ; /* fall thru */
         case 1: car[kk] = sar[kk] ; kk++ ; break ;
       }
     }
   }

   MRI_COPY_AUX(imc,ima) ; RETURN(imc) ;
}
