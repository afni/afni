#include "mrilib.h"

static int BP_ffbot = 0 ;
void mri_bport_set_ffbot( int q ){ BP_ffbot = q ; }

static int BP_invert = 0 ;
void mri_bport_set_invert( int q ){ BP_invert = q ; }

static int BP_quad = 0 ;
void mri_bport_set_quad( int q ){ BP_quad = q ; }

/*---------------------------------------------------------------------------*/

intvec * invert_integer_list( int nind , int *ind , int ibot , int itop )
{
   intvec *ivv ; int ii,jj,kk ;

   if( nind <= 0 || ind == NULL || ibot >= itop ) return NULL ;

   MAKE_intvec(ivv,itop-ibot+1) ;

   for( kk=0,ii=ibot ; ii <= itop ; ii++ ){
     for( jj=0 ; jj < nind ; jj++ ) if( ind[jj] == ii ) break ;
     if( jj == nind ) ivv->ar[kk++] = ii ;
   }

   if( kk == 0 ){ KILL_intvec(ivv) ; ivv = NULL ; }
   else         { RESIZE_intvec(ivv,kk) ; }

   return ivv ;
}

/*---------------------------------------------------------------------------*/

MRI_IMAGE * mri_bport_indexed( int nind  , int *ind ,
                               int ntime , int nbefore , int nafter )
{
   MRI_IMAGE *fim ; float *far , *fii , *fip , freq ;
   int ff , ii,jj,kk , nev=(ntime%2==0) , ncol,nrow , nth=ntime/2 ;
   int nqind , *qind ;

ENTRY("mri_bport_indexed") ;

   if( nind <= 0 || ind == NULL || ntime < 9 ) RETURN(NULL) ;

   if( nbefore < 0 ) nbefore = 0 ;
   if( nafter  < 0 ) nafter  = 0 ;

   /*--- edit a local copy of the list of indexes ---*/

   qind = (int *)malloc(sizeof(int)*nind) ;

   /* cast out illegal values */

   for( nqind=ii=0 ; ii < nind ; ii++ ){
     if( ind[ii] >= BP_ffbot && ind[ii] <= nth ) qind[nqind++] = ind[ii] ;
   }
   if( nqind == 0 ){ free(qind) ; RETURN(NULL) ; }

   /* cast out duplicate values */

   qsort_int( nqind , qind ) ;
   for( jj=ii=1 ; ii < nqind ; ii++ ){
     if( qind[ii] == qind[ii-1] ) continue ;
     if( jj < ii ) qind[jj] = qind[ii] ;
     jj++ ;
   }
   nqind = jj ;

   if( BP_invert ){
     intvec *ivv = invert_integer_list( nqind,qind , BP_ffbot,nth ) ;
     if( ivv == NULL ){ free(qind) ; RETURN(NULL) ; }
     nqind = ivv->nar ; qind = realloc(qind,sizeof(int)*nqind) ;
     for( ii=0 ; ii < nqind ; ii++ ) qind[ii] = ivv->ar[ii] ;
     KILL_intvec(ivv) ;
   }

#if 1
   fprintf(stderr," + Frequency indexes: blocklen=%d ::",ntime) ;
   for( ii=0 ; ii < nqind ; ii++ ) fprintf(stderr," %d",qind[ii]) ;
   fprintf(stderr,"\n") ;
#endif

   /* count columns to generate */

   for( ncol=ii=0 ; ii < nqind ; ii++ ){
     if( qind[ii] == 0 || (qind[ii] == nth && nev) ) ncol +=  1 ;
     else                                            ncol +=  2 ;
   }
   if( ncol == 0 ){ free(qind) ; RETURN(NULL) ; }

   if( BP_quad ) ncol += 2 ;

   nrow = ntime + nbefore + nafter ;
   fim  = mri_new( nrow , ncol , MRI_float ) ;
   far  = MRI_FLOAT_PTR(fim) ;

   for( jj=ii=0 ; jj < nqind ; jj++ ){
     ff  = qind[jj] ;
     fii = far + (ii*nrow + nbefore) ;
     if( ff == 0 ){
       for( kk=0 ; kk < ntime ; kk++ ) fii[kk] = 1.0f ;
       ii++ ;  /* added 1 col */
     } else if( ff == nth && nev ){
       for( kk=0 ; kk < ntime ; kk++ ) fii[kk] = 2*(kk%2)-1 ;
       ii++ ;  /* added 1 col */
     } else {
       fip = fii + nrow ; freq = ff * (2.0f*3.141593f) / (float)ntime ;
       for( kk=0 ; kk < ntime ; kk++ ){
         fii[kk] = cosf(freq*kk) ; fip[kk] = sinf(freq*kk) ;
       }
       ii += 2 ;  /* added 2 cols */
     }
   }

   if( BP_quad ){
     float xmid=0.5*(ntime-1) , xfac=1.0f/xmid ;
     fii = far + (ii*nrow + nbefore) ;
     for( kk=0 ; kk < ntime ; kk++ ) fii[kk] = (float)Plegendre(xfac*(kk-xmid),1) ;
     ii++ ; fii = far + (ii*nrow + nbefore) ;
     for( kk=0 ; kk < ntime ; kk++ ) fii[kk] = (float)Plegendre(xfac*(kk-xmid),2) ;
   }

   free(qind) ; RETURN(fim) ;
}

/*---------------------------------------------------------------------------*/

intvec * mri_bport_make_indexes( float dt, float fbot, float ftop, int ntime )
{
   int nfbot , nftop , nev=(ntime%2==0) , nth=ntime/2 , ff ;
   float df ;
   intvec *ivv ;

ENTRY("mri_bport_make_indexes") ;

   if( dt   <= 0.0f ) dt   = 1.0f ;
   if( fbot <  0.0f ) fbot = 0.0f ;
   if( ntime < 9 || ftop < fbot ) RETURN(NULL) ;

   df = 1.0f / (ntime *dt) ;  /* frequency spacing */

   nfbot = (int)rintf(fbot/df+0.1666666f) ;
   /* guard against int overflow with floats   17 Dec 2020 [rickr] */
   if   ( fbot > df*(nth+1)  ) nfbot = nth ;
   else if( nfbot > nth      ) nfbot = nth ;
   else if( nfbot < BP_ffbot ) nfbot = BP_ffbot ;

   nftop = (int)rintf(ftop/df-0.1666666f) ;
   /* guard against int overflow with floats */
   if   ( ftop > df*(nth+1) ) nftop = nth ;
   else if( nftop < nfbot   ) nftop = nfbot ;
   else if( nftop > ntime/2 ) nftop = nth ;


   MAKE_intvec(ivv,nftop-nfbot+1) ;

   for( ff=nfbot ; ff <= nftop ; ff++ ) ivv->ar[ff-nfbot] = ff ;

#if 0
fprintf(stderr,"indexes: fbot=%g ftop=%g df=%g ntime=%d\n",fbot,ftop,df,ntime) ;
for( ff=nfbot ; ff <= nftop ; ff++ ) fprintf(stderr," %d",ivv->ar[ff-nfbot]) ;
fprintf(stderr,"\n") ;
#endif

   RETURN(ivv) ;
}

/*---------------------------------------------------------------------------*/

MRI_IMAGE * mri_bport_multi_contig( float dt ,
                                    int nband, float *fbot, float *ftop,
                                    int ntime, int nbefore, int nafter  )
{
   intvec *ivv , *jvv ; int ib ;
   MRI_IMAGE *fim ;

ENTRY("mri_bport_multi_contig") ;

   if( nband <= 0 || fbot == NULL || ftop == NULL ) RETURN(NULL) ;

   ivv = NULL ;
   for( ib=0 ; ib < nband ; ib++ ){
     jvv = mri_bport_make_indexes( dt , fbot[ib] , ftop[ib] , ntime ) ;
     if( jvv != NULL ){
       if( ivv == NULL ){
         ivv = jvv ;
       } else {
         APPEND_intvec(ivv,jvv) ; KILL_intvec(jvv) ;
       }
     }
     jvv = NULL ;
   }
   if( ivv == NULL ) RETURN(NULL) ;

   fim = mri_bport_indexed( ivv->nar, ivv->ar, ntime, nbefore, nafter ) ;

   KILL_intvec(ivv) ; RETURN(fim) ;
}

/*---------------------------------------------------------------------------*/

MRI_IMAGE * mri_bport_multi( float dt,
                             int nband , float *fbot , float *ftop ,
                             int nblock, int *ntim                  )
{
   MRI_IMAGE *tim ; MRI_IMARR *bimar ;
   int nrow , nbef , naft , tt ;

ENTRY("mri_bport_multi") ;

   if( nblock <= 0 || ntim == NULL ) RETURN(NULL) ;
   if( nblock == 1 ){
     tim = mri_bport_multi_contig( dt,nband,fbot,ftop , ntim[0],0,0 ) ;
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
     tim = mri_bport_multi_contig( dt,nband,fbot,ftop , ntim[tt],nbef,naft ) ;
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

   nfbot = (int)rintf(fbot/df+0.1666666f) ;
   if( nfbot > ntime/2    ) nfbot = ntime/2 ;
   if( nfbot < BP_ffbot   ) nfbot = BP_ffbot ;

   nftop = (int)rintf(ftop/df-0.1666666f) ;
   if( nftop < nfbot   ) nftop = nfbot   ;
   if( nftop > ntime/2 ) nftop = ntime/2 ;
#if 1
   ININFO_message("Frequency indexes: blocklen=%d nfbot=%d nftop=%d",ntime,nfbot,nftop) ;
#endif

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
