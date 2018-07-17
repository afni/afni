#include "mrilib.h"

/*----------------------------------------------------------------------------*/
/* For putting 3D images together in a line (1D catenation).
   Also see mri_cat2D.c for 1D or 2D catenation of 2D images. */

#define OUTPTR(i,j,k) ( cout + dsize*( ((k)*nyout+(j))*nxout+(i) ) )
#define INPTR(i,j,k)  ( cin  + dsize*( ((k)*nyin +(j))*nxin +(i) ) )

/*----------------------------------------------------------------------------*/
/* This xx catvol is modified to allow for NULL input images.
   At this time [17 Jul 2018], the yy and zz catvol do NOT work with NULLs!
*//*--------------------------------------------------------------------------*/

static MRI_IMAGE * mri_catvol_1D_xx( MRI_IMARR *imar , int na, int nb )
{
   int nxin,nyin,nzin , nxout,nyout,nzout , nim , ii,jj,kk,nn , datum,dsize ;
   MRI_IMAGE *imout , *imin ;
   char *cout , *cin ;

ENTRY("mri_catvol_1D_xx") ;

   if( imar == NULL ) RETURN(NULL) ;

   nim = IMARR_COUNT(imar) ;
   if( nim == 0 ) RETURN(NULL) ;

   if( na < 0     ) na = 0 ;
   if( nb > nim-1 ) nb = nim-1 ;
   if( na > nb    ){ na = 0 ; nb = nim-1 ; }

   imin = IMARR_SUBIM(imar,na) ;
   if( na == nb ){ imout = mri_copy(imin) ; RETURN(imout) ; }

   for( nn=na ; nn <= nb ; nn++ ){ /* find first non-NULL image */
     imin = IMARR_SUBIM(imar,nn) ;
     if( imin != NULL ) break ;
   }
   if( imin == NULL ) RETURN(NULL) ;

   nyout = nyin = imin->ny ; nzout = nzin = imin->nz ; nxout = imin->nx ;
   datum = imin->kind ;
   for( nn=na ; nn <= nb ; nn++ ){
     imin = IMARR_SUBIM(imar,nn) ; if( imin == NULL ) continue ;
     if( nyin != imin->ny || nzin != imin->nz || datum != imin->kind ) RETURN(NULL);
     nxout += imin->nx ;
   }

   imout = mri_new_vol(nxout,nyout,nzout,datum) ;
   cout  = mri_data_pointer(imout) ;
   dsize = mri_datum_size(imout->kind) ;

   /* copy line by line */

   for( kk=0 ; kk < nzin ; kk++ ){
     for( jj=0 ; jj < nyin ; jj++ ){
       for( ii=0,nn=na ; nn <= nb ; nn++ ){
         imin = IMARR_SUBIM(imar,nn); if( imin == NULL ) continue ;
         nxin = imin->nx; cin = mri_data_pointer(imin);
         memcpy( OUTPTR(ii,jj,kk) , INPTR(0,jj,kk) , dsize*nxin ) ;
         ii += nxin ;
       }
     }
   }

   RETURN(imout) ;
}

/*----------------------------------------------------------------------------*/

static MRI_IMAGE * mri_catvol_1D_yy( MRI_IMARR *imar , int na, int nb )
{
   int nxin,nyin,nzin , nxout,nyout,nzout , nim , ii,jj,kk,nn , datum,dsize ;
   MRI_IMAGE *imout , *imin ;
   char *cout , *cin ;

ENTRY("mri_catvol_1D_yy") ;

   if( imar == NULL ) RETURN(NULL) ;

   nim = IMARR_COUNT(imar) ;
   if( nim == 0 ) RETURN(NULL) ;

   if( na < 0     ) na = 0 ;
   if( nb > nim-1 ) nb = nim-1 ;
   if( na > nb    ){ na = 0 ; nb = nim-1 ; }

   imin = IMARR_SUBIM(imar,na) ;
   if( na == nb ){ imout = mri_copy(imin) ; RETURN(imout) ; }

   nxout = nxin = imin->nx ; nzout = nzin = imin->nz ; nyout = imin->ny ;
   datum = imin->kind ;
   for( nn=na+1 ; nn <= nb ; nn++ ){
     imin = IMARR_SUBIM(imar,nn) ;
     if( nxin != imin->nx || nzin != imin->nz || datum != imin->kind ) RETURN(NULL);
     nyout += imin->ny ;
   }

   imout = mri_new_vol(nxout,nyout,nzout,datum) ;
   cout  = mri_data_pointer(imout) ;
   dsize = mri_datum_size(imout->kind) ;

   /* copy plane by plane */

   for( kk=0 ; kk < nzin ; kk++ ){
     for( jj=0,nn=na ; nn <= nb ; nn++ ){
       imin = IMARR_SUBIM(imar,nn); nyin = imin->ny; cin = mri_data_pointer(imin);
       memcpy( OUTPTR(0,jj,kk) , INPTR(0,0,kk) , dsize*nxin*nyin ) ;
       jj += nyin ;
     }
   }

   RETURN(imout) ;
}

/*----------------------------------------------------------------------------*/

static MRI_IMAGE * mri_catvol_1D_zz( MRI_IMARR *imar , int na , int nb )
{
   int nxin,nyin,nzin , nxout,nyout,nzout , nim , ii,jj,kk,nn , datum,dsize ;
   MRI_IMAGE *imout , *imin ;
   char *cout , *cin ;

ENTRY("mri_catvol_1D_zz") ;

   if( imar == NULL ) RETURN(NULL) ;

   nim = IMARR_COUNT(imar) ;
   if( nim == 0 ) RETURN(NULL) ;

   if( na < 0     ) na = 0 ;
   if( nb > nim-1 ) nb = nim-1 ;
   if( na > nb    ){ na = 0 ; nb = nim-1 ; }

   imin = IMARR_SUBIM(imar,na) ;
   if( na == nb ){ imout = mri_copy(imin) ; RETURN(imout) ; }

   nxout = nxin = imin->nx ; nyout = nyin = imin->ny ; nzout = imin->nz ;
   datum = imin->kind ;
   for( nn=na+1 ; nn <= nb ; nn++ ){
     imin = IMARR_SUBIM(imar,nn) ;
     if( nxin != imin->nx || nyin != imin->ny || datum != imin->kind ) RETURN(NULL);
     nzout += imin->nz ;
   }

   imout = mri_new_vol(nxout,nyout,nzout,datum) ;
   cout  = mri_data_pointer(imout) ;
   dsize = mri_datum_size(imout->kind) ;

   /* copy volume by volume */

   for( kk=0,nn=na ; nn <= nb ; nn++ ){
     imin = IMARR_SUBIM(imar,nn); nzin = imin->nz; cin = mri_data_pointer(imin);
     memcpy( OUTPTR(0,0,kk) , INPTR(0,0,0) , dsize*nxin*nyin*nzin ) ;
     kk += nzin ;
   }

   RETURN(imout) ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_catvol_1D_ab( MRI_IMARR *imar , int dir , int na , int nb )
{
   MRI_IMAGE *imout ;

ENTRY("mri_catvol_1D_ab") ;

        if( dir <= 1 ) imout = mri_catvol_1D_xx(imar,na,nb) ;
   else if( dir == 2 ) imout = mri_catvol_1D_yy(imar,na,nb) ;
   else                imout = mri_catvol_1D_zz(imar,na,nb) ;

   RETURN(imout) ;
}

/*----------------------------------------------------------------------------*/

MRI_IMAGE * mri_catvol_1D( MRI_IMARR *imar , int dir )
{
   MRI_IMAGE *imout ;

ENTRY("mri_catvol_1D") ;

   if( imar == NULL || IMARR_COUNT(imar) == 0 ) RETURN(NULL) ;

   imout = mri_catvol_1D_ab( imar , dir , 0 , IMARR_COUNT(imar)-1 ) ;

   RETURN(imout) ;
}
