#include "mrilib.h"

/*--------------------------------------------------------------------------*/

static int lev_num ;
static int lev_siz[256] ;

static MRI_vectim * THD_dset_grayplot_prep( THD_3dim_dataset *dset ,
                                            byte *mmask ,
                                            int polort , float fwhm )
{
   int cmval , nmask , nxyz , nts ;
   byte mm,*tmask ; int ii,jj ;
   int nvim ; MRI_vectim **vim , *vout ;
   float *tsar ;

   lev_num = 0 ;
   if( !ISVALID_DSET(dset) || mmask == NULL ) return NULL ;
   nts = DSET_NVALS(dset) ;
   if( nts < 19 ) return NULL ;

   nxyz = DSET_NVOX(dset) ;
   nmask = THD_countmask( nxyz , mmask ) ;
   if( nmask < 19 ) return NULL ;

   nvim  = 0 ; vim = NULL ;
   tmask = (byte *)malloc(sizeof(byte)*nxyz) ;
   for( ii=1 ; ii <= 255 ; ii++ ){
     mm = (byte)ii ;
     memset( tmask , 0 , sizeof(byte)*nxyz) ;
     for( cmval=jj=0 ; jj < nxyz ; jj++ ){
       if( mmask[jj] == mm ){ cmval++ ; tmask[jj] = 1 ; }
     }
     if( cmval > 9 ){
       vim = (MRI_vectim **)realloc( vim , sizeof(MRI_vectim *)*(nvim+1) ) ;
       vim[nvim] = THD_dset_to_vectim( dset , tmask , 0 ) ;
       if( polort >= 0 ){
         for( jj=0 ; jj < vim[nvim]->nvec ; jj++ ){
           tsar = VECTIM_PTR( vim[nvim] , jj ) ;
           THD_generic_detrend_LSQ( nts,tsar , polort , 0,NULL,NULL ) ;
         }
       }
       for( jj=0 ; jj < vim[nvim]->nvec ; jj++ ){
         tsar = VECTIM_PTR( vim[nvim] , jj ) ;
         THD_normRMS( nts , tsar ) ;
       }
       if( fwhm > 0 )
         mri_blur3D_vectim( vim[nvim] , fwhm ) ;

       lev_siz[nvim] = cmval ;
       nvim++ ;
     }
   }
   free(tmask) ;

   lev_num = nvim ;

   if( nvim == 0 ) return NULL ;

   if( nvim == 1 ){
     vout = vim[0] ;
   } else {
     vout = THD_xyzcat_vectims( nvim , vim ) ;
     for( ii=0 ; ii < nvim ; ii++ ) VECTIM_destroy(vim[ii]) ;
   }

   free(vim) ; 
   return vout ;
}

/*--------------------------------------------------------------------------*/

static void resample_1D_float( int nin, float *xin, int nout, float *xout )
{
   int ii , jj , nout1 , jbot,jtop ;
   float ffac , fjmid , fj ;

   if( nin < 2 || xin == NULL || nout < 2 || xout == NULL ) return ;

   if( nin == nout ){
     memcpy( xout , xin , sizeof(float)*nin) ;
     return ;
   }

   ffac = (nin-1.0f)/(nout-1.0f) ;
   nout1 = nout-1 ;

   if( ffac < 1.0f ){              /* interpolate to finer grid */
     xout[0]      = xin[0] ;
     xout[nout-1] = xin[nin-1] ;
     for( ii=1 ; ii < nout1 ; ii++ ){
       fjmid = ii*ffac ;
       jbot  = (int)fjmid ; fj = (fjmid-jbot) ;
       xout[ii] = (1.0f-fj)*xin[jbot] + fj*xin[jbot+1] ;
     }
   } else {                        /* coarser grid */
     int npm = (int)rintf(ffac) ;
     float *xq = (float *)malloc(sizeof(float)*nin) ;
     float *wt = (float *)malloc(sizeof(float)*(npm+1)) ;
     float sum, wsum , ww ;
     memcpy( xq , xin , sizeof(float)*nin) ;
     for( jj=0 ; jj <= npm ; jj++ ) wt[jj] = (npm-jj+0.5f) ;
     for( ii=0 ; ii < nout ; ii++ ){
       jbot = ii-npm ; if( jbot < 0     ) jbot = 0 ;
       jtop = ii+npm ; if( jbot > nout1 ) jbot = nout1 ;
       sum = wsum = 0.0f ;
       for( jj=jbot ; jj <= jtop ; jj++ ){
         ww = wt[abs(jj-ii)] ; wsum += ww ; sum += ww*xin[jj] ;
       }
       xq[ii] = sum / wsum ;
     }
     free(wt) ;
     xout[0]      = xq[0] ;
     xout[nout-1] = xq[nin-1] ;
     for( ii=1 ; ii < nout1 ; ii++ ){
       fjmid = ii*ffac ;
       jbot  = (int)fjmid ; fj = (fjmid-jbot) ;
       xout[ii] = (1.0f-fj)*xq[jbot] + fj*xq[jbot+1] ;
     }
     free(xq) ;
   }

   return ;
}

/*--------------------------------------------------------------------------*/

static MRI_IMAGE * mri_vectim_to_grayplot( MRI_vectim *imts, int nx, int ny )
{
   int nxx=nx , nyy=ny , ntt,nss , ii,jj ;
   MRI_IMAGE *imout ; byte *outar ;
   MRI_IMAGE *imttt ; byte *tttar , *tar ;
   float zbot , ztop , val , zfac , *qar , *zar=NULL , *yar ;

   if( imts == NULL ) return NULL ;

   if( nxx < 512 ) nxx = 512 ; else if( nxx > 32768 ) nxx = 32768 ;
   if( nyy < 256 ) nyy = 256 ; else if( nyy > 32768 ) nyy = 32768 ;

   ntt = imts->nvals ;
   nss = imts->nvec ; if( ntt < 19 || nss < 19 ) return NULL ;

   zbot = 6.66e+33 ; ztop = -zbot ;
   for( jj=0 ; jj < nss ; jj++ ){ qar = VECTIM_PTR(imts,jj) ;
     for( ii=0 ; ii < ntt ; ii++ ){
            if( qar[ii] < zbot ) zbot = qar[ii] ;
       else if( qar[ii] > ztop ) ztop = qar[ii] ;
     }
   }
   if( zbot >= ztop ) return NULL ;
   zfac = 255.4f / (ztop-zbot) ;

   imttt = mri_new( nxx,nss , MRI_byte ) ;
   tttar = MRI_BYTE_PTR(imttt) ;

   if( nxx != ntt ){
     zar = malloc(sizeof(float)*nxx) ;
   }
   for( jj=0 ; jj < nss ; jj++ ){
     qar = VECTIM_PTR(imts,jj) ;
     if( zar != NULL ){
       resample_1D_float( ntt,qar , nxx,zar ) ; yar = zar ;
     } else {
       yar = qar ;
     }
     tar = tttar + jj*nxx ;
     for( ii=0 ; ii < nxx ; ii++ ){
       val = (yar[ii]-zbot)*zfac ;
       tar[ii] = BYTEIZE(val) ;
     }
   }
   if( zar != NULL ){ free(zar); zar = NULL; }

   if( nss == nyy ){
     return imttt ;
   }

   imout = mri_new( nxx,nyy , MRI_byte ) ;
   outar = MRI_BYTE_PTR(imout) ;

   yar = (float *)malloc(sizeof(float)*nss) ;
   zar = (float *)malloc(sizeof(float)*nyy) ;

   for( ii=0 ; ii < nxx ; ii++ ){
     for( jj=0 ; jj < nss ; jj++ )
       yar[jj] = tttar[ii+jj*nxx] ;
     resample_1D_float( nss,yar , nyy,zar ) ;
     for( jj=0 ; jj < nyy ; jj++ )
       outar[ii+jj*nxx] = BYTEIZE(zar[jj]) ;
   }

   free(zar) ; free(yar) ; mri_free(imttt) ;

   if( lev_num > 1 ){
     float yfac ; int kk ;
     for( kk=1 ; kk < lev_num ; kk++ ) lev_siz[kk] += lev_siz[kk-1] ;
     yfac = nyy / (float)nss ;
     for( kk=0 ; kk < lev_num-1 ; kk++ ){
       jj = (int)rintf( yfac * lev_siz[kk] ) ;
       for( ii=0 ; ii < nxx ; ii++ ){
         if( ii%19 < 9 ) outar[ii+jj*nxx] = 0 ;
       }
     }
   }

   return imout ;
}

/*--------------------------------------------------------------------------*/

MRI_IMAGE * THD_dset_to_grayplot( THD_3dim_dataset *dset ,
                                  byte *mmask ,
                                  int nxout , int nyout ,
                                  int polort , float fwhm )
{
   MRI_vectim *vim ; MRI_IMAGE *imout ;

   vim = THD_dset_grayplot_prep( dset , mmask , polort , fwhm ) ;

   if( nxout < 128 ) nxout = 1024 ;
   if( nyout <  64 ) nyout =  512 ;

   imout = mri_vectim_to_grayplot( vim, nxout , nyout ) ;

   VECTIM_destroy(vim) ;

   return imout ;
}
