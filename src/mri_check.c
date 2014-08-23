#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/* Interleave two images in a checkerboard */

MRI_IMAGE * mri_check_2D( int bsiz , MRI_IMAGE *ima , MRI_IMAGE *imb )
{
   MRI_IMAGE *imc ;
   int nx , ny , ii , jj , ib,jb , ps , kk ;
   char *aar , *bar , *car , *sar ;

ENTRY("mri_check_2D") ;

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

/*----------------------------------------------------------------------------*/
/* 'Wiper' combine 2 images, according to wcode method */

MRI_IMAGE * mri_wiper_2D( int wcode,float wfac, MRI_IMAGE *ima, MRI_IMAGE *imb )
{
   MRI_IMAGE *imc ;
   int nx , ny , ii , jj , ib,jb , ps , kk ;
   char *aar , *bar , *car , *sar ;
   byte *mar ;

ENTRY("mri_wiper_2D") ;

   if( ima == NULL || imb == NULL ) RETURN(NULL) ;
   nx = ima->nx ; ny = ima->ny ;
   if( imb->nx != nx || imb->ny != ny || ima->kind != imb->kind ) RETURN(NULL);

        if( wfac <= 0.0f ){ imc = mri_copy(imb) ; RETURN(imc) ; }
   else if( wfac >= 1.0f ){ imc = mri_copy(ima) ; RETURN(imc) ; }

#undef  IJ
#define IJ(i,j) ((i)+(j)*nx)

   mar = (byte *)calloc(sizeof(byte),nx*ny) ;
   imc = mri_new( nx , ny , ima->kind ) ;
   car = (char *)mri_data_pointer(imc) ;

   ps  = ima->pixel_size ;
   aar = (char *)mri_data_pointer(ima) ; if( aar == NULL ) RETURN(NULL) ;
   bar = (char *)mri_data_pointer(imb) ; if( bar == NULL ) RETURN(NULL) ;

   /* make mask */

   switch( wcode ){
     default:
     case WIPER_FROM_LEFT:
       ib = (int)rintf(wfac*nx) ;
       for( jj=0 ; jj < ny ; jj++ )
         for( ii=ib ; ii < nx ; ii++ ) mar[IJ(ii,jj)] = 1 ;
     break ;

     case WIPER_FROM_BOTTOM:
       jb = (int)rintf(wfac*ny) ;
       for( jj=jb ; jj < ny ; jj++ )
         for( ii=0 ; ii < nx ; ii++ ) mar[IJ(ii,jj)] = 1 ;
     break ;

     case WIPER_FROM_CENTER:{
       float nxh=0.5f*(nx-1)    , nyh=0.5f*(ny-1)    ;
       float nxq=1.0f/(nxh*nxh) , nyq=1.0f/(nyh*nyh) ;
       float jr,rr ;
       for( jj=0 ; jj < ny ; jj++ ){
         jr = (jj-nyh)*(jj-nyh)*nyq ;
         for( ii=0 ; ii < nx ; ii++ ){
           rr = (ii-nxh)*(ii-nxh)*nxq+jr; if( rr >= wfac ) mar[IJ(ii,jj)] = 1;
         }
       }
     }
     break ;
   }

   /* copy data, depending on mask */

   for( kk=jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++ ){
       sar = mar[IJ(ii,jj)] ? bar : aar ;  /* choose source image */

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

   free(mar) ; MRI_COPY_AUX(imc,ima) ; RETURN(imc) ;
}

/*----------------------------------------------------------------------------*/
/* Mix 2 images in a fraction from 0 to 1 (0=ima 1=imb). */

MRI_IMAGE * mri_mix_2D( float wfac, MRI_IMAGE *ima, MRI_IMAGE *imb )
{
   MRI_IMAGE *imc=NULL ;
   int nx , ny , ii , nxy ;
   float fa , fb ;

ENTRY("mri_mix_2D") ;

   if( ima == NULL || imb == NULL ) RETURN(NULL) ;
   nx = ima->nx ; ny = ima->ny ; nxy = nx*ny ;
   if( imb->nx != nx || imb->ny != ny || ima->kind != imb->kind ) RETURN(NULL);

        if( wfac <= 0.0f ){ imc = mri_copy(ima) ; RETURN(imc) ; }
   else if( wfac >= 1.0f ){ imc = mri_copy(imb) ; RETURN(imc) ; }

   imc = mri_new( nx , ny , ima->kind ) ;
   fa  = 1.0f-wfac ; fb = wfac ;

   switch( ima->kind ){

     default: mri_free(imc) ; imc = NULL ; break ; /* bad */

     case MRI_byte:{
       byte *ar=MRI_BYTE_PTR(ima), *br=MRI_BYTE_PTR(imb), *cr=MRI_BYTE_PTR(imc) ;
       for( ii=0 ; ii < nxy ; ii++ )
         cr[ii] = (byte)(fa*ar[ii]+fb*br[ii]+0.49f) ;
     }
     break ;

     case MRI_short:{
       short *ar=MRI_SHORT_PTR(ima), *br=MRI_SHORT_PTR(imb), *cr=MRI_SHORT_PTR(imc) ;
       for( ii=0 ; ii < nxy ; ii++ )
         cr[ii] = (short)(fa*ar[ii]+fb*br[ii]+0.49f) ;
     }
     break ;

     case MRI_float:{
       float *ar=MRI_FLOAT_PTR(ima), *br=MRI_FLOAT_PTR(imb), *cr=MRI_FLOAT_PTR(imc) ;
       for( ii=0 ; ii < nxy ; ii++ )
         cr[ii] = fa*ar[ii]+fb*br[ii] ;
     }
     break ;

     case MRI_rgb:{
       byte *ar=MRI_BYTE_PTR(ima), *br=MRI_BYTE_PTR(imb), *cr=MRI_BYTE_PTR(imc) ;
       for( ii=0 ; ii < 3*nxy ; ii++ )
         cr[ii] = (byte)(fa*ar[ii]+fb*br[ii]+0.49f) ;
     }
     break ;

     case MRI_complex:{
       complex *ar=MRI_COMPLEX_PTR(ima), *br=MRI_COMPLEX_PTR(imb), *cr=MRI_COMPLEX_PTR(imc) ;
       for( ii=0 ; ii < nxy ; ii++ ){
         cr[ii].r = fa*ar[ii].r+fb*br[ii].r ; cr[ii].i = fa*ar[ii].i+fb*br[ii].i ;
       }
     }
     break ;

   }

   RETURN(imc) ;
}
