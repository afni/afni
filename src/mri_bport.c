#include "mrilib.h"

/*---------------------------------------------------------------------------*/
/* Return a set of columns (cosines and sines) to filter out frequencies
   from fbot to ftop, with time step dt and number of time points ntime.
   Put nbefore all zero entries before these, and nafter all zero entries
   after them -- output column length is ntime+nbefore+nafter.
*//*-------------------------------------------------------------------------*/

MRI_IMAGE * mri_bport_contig( float dt , float fbot , float ftop ,
                              int ntime , int nbefore , int nafter )
{
   MRI_IMAGE *fim ; float *far , *fii , *fip ;
   int nfbot , nftop , ff , ii,jj , nev=(ntime%2==0) , ncol,nrow ;
   float df , freq ;

ENTRY("mri_bport_contig") ;

   if( dt   <= 0.0f ) dt   = 1.0f ;
   if( fbot <  0.0f ) fbot = 0.0f ;
   if( ntime < 9 || ftop < fbot ) RETURN(NULL) ;
   if( nbefore < 0 ) nbefore = 0 ;
   if( nafter  < 0 ) nafter  = 0 ;

   df  = 1.0f / (ntime * dt) ;  /* frequency spacing */

   nfbot = (int)rintf(fbot/df+0.1666666f) ; if( nfbot > ntime/2 ) nfbot = ntime/2 ;
   nftop = (int)rintf(ftop/df-0.1666666f) ;
   if( nftop < nfbot   ) nftop = nfbot   ;
   if( nftop > ntime/2 ) nftop = ntime/2 ;
   ININFO_message("Frequency indexes: blocklen=%d nfbot=%d nftop=%d",ntime,nfbot,nftop) ;

   ncol = 2*(nftop-nfbot-1) ; if( ncol < 0 ) ncol = 0 ;
   ncol += (nfbot == 0 || (nfbot == ntime/2 && nev==1) ) ? 1 : 2 ;
   if( nftop > nfbot )
     ncol += (nftop == ntime/2 && nev) ? 1 : 2 ;

   if( ncol <= 0 ){
     ININFO_message("Failure :-(") ; RETURN(NULL) ;  /* should never happen */
   }

   nrow = ntime + nbefore + nafter ;
   fim  = mri_new( nrow , ncol , MRI_float ) ;
   far  = MRI_FLOAT_PTR(fim) ;

   for( ii=0,ff=nfbot ; ff <= nftop ; ff++ ){
     fii = far + (ii*nrow + nbefore) ;
     if( ff == 0 ){
       for( jj=0 ; jj < ntime ; jj++ ) fii[jj] = 1.0f ;
       ii++ ;
     } else if( ff == ntime/2 && nev ){
       for( jj=0 ; jj < ntime ; jj++ ) fii[jj] = 2*(jj%2)-1 ;
       ii++ ;
     } else {
       fip = fii + nrow ; freq = ff*df * (2.0f*3.141593f) * dt ;
       for( jj=0 ; jj < ntime ; jj++ ){
         fii[jj] = cos(freq*jj) ; fip[jj] = sin(freq*jj) ;
       }
       ii += 2 ;
     }
   }

   RETURN(fim) ;
}

/*---------------------------------------------------------------------------*/

MRI_IMAGE * mri_bport( float dt, float fbot, float ftop, int nblock, int *ntim )
{
   MRI_IMAGE *tim ; MRI_IMARR *bimar ;
   int nrow , nbef , naft , tt ;

ENTRY("mri_bport") ;

   if( nblock <= 0 || ntim == NULL ) RETURN(NULL) ;
   if( nblock == 1 ){
     tim = mri_bport_contig( dt,fbot,ftop , ntim[0],0,0 ) ;
     RETURN(tim) ;
   }

   for( nrow=tt=0 ; tt < nblock ; tt++ ){
     if( ntim[tt] < 9 ) RETURN(NULL) ;
     nrow += ntim[tt] ;
   }
   INIT_IMARR(bimar) ;

   nbef = 0 ;
   naft = nrow - ntim[0] ;
   for( tt=0 ; tt < nblock ; tt++ ){
     tim = mri_bport_contig( dt,fbot,ftop , ntim[tt],nbef,naft ) ;
     if( tim == NULL ){
       DESTROY_IMARR(bimar) ; RETURN(NULL) ;
     }
     ADDTO_IMARR(bimar,tim) ;
     if( tt < nblock-1 ){ nbef += ntim[tt]; naft -= ntim[tt+1]; }
   }

   tim = mri_catvol_1D( bimar , 2 ) ;
   DESTROY_IMARR(bimar) ;
   RETURN(tim) ;
}
