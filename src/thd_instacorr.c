#include "mrilib.h"

#undef  EM
#define EM(s) ERROR_message("InstaCorr setup bad: %s",(s))

/* Return 1 if methods uses timeseries normalization, 
          0 otherwise */
int THD_instacorr_cmeth_needs_norm(int cmeth) {
   switch (cmeth) {
      case NBISTAT_EUCLIDIAN_DIST:
      case NBISTAT_CITYBLOCK_DIST:
         return(0);
      default:
         return(1);
   }
   return(1);
}


/*---------------------------------------------------------------------------*/
/* Read and pre-process time series for InstaCorr [moved here 10 Nov 2011].  */

static MRI_vectim * THD_instacorr_tsprep( ICOR_setup *iset , THD_3dim_dataset *dset )
{
   MRI_vectim *mv ;
   int iv , nmmm , ntime ;
   float **dvec , **gvec=NULL ; int ngvec=0 ;

ENTRY("THD_instacorr_tsprep") ;

   if( iset == NULL || dset == NULL ) RETURN(NULL) ;

   /*--- Extract time series for analysis ---*/

   ININFO_message("Extracting dataset time series: %s",DSET_BRIKNAME(dset)) ;

   mv = THD_dset_to_vectim_stend( dset , iset->mmm , iset->start , iset->end ) ;
   if( mv == NULL ){
     EM("Can't extract dataset time series!") ; RETURN(NULL) ;
   }
   nmmm  = mv->nvec ;
   ntime = mv->nvals ;  /* #dataset time points to process */

   if( iset->despike ){
     int_pair ip ;
     ININFO_message("- Testing time series for spikes") ;
     ip = THD_vectim_despike9( mv ) ;
     if( ip.i > 0 )
       ININFO_message(" -- Removed %d spikes from %d time series",ip.j,ip.i) ;
     else
       ININFO_message(" -- No spikes were found") ;
   }

   /*--- Filter time series ---*/

   ININFO_message("- Filtering %d dataset time series",nmmm) ;

   if( iset->fbot <  0.0f       ) iset->fbot = 0.0f ;
   if( iset->ftop <= iset->fbot ) iset->ftop = 999999.9f ;  /* infinity */

   dvec = (float **)malloc(sizeof(float *)*nmmm) ;
   for( iv=0 ; iv < nmmm ; iv++ ) dvec[iv] = VECTIM_PTR(mv,iv) ;

   if( iset->gortim != NULL ){         /** set up the extra orts **/
     if( iset->gortim->nx < ntime ){

       /* too short to be useful */

       ERROR_message("Global Ort time series length=%d is too short!",iset->gortim->nx);
       ERROR_message(" ==> ignoring Global Ort file") ;

     } else if( iset->gortim->nx >= ntime && iset->gortim->nx < ntime+iset->start ){

       /* too short to ignore initial points */

       if( iset->start > 0 )
         ININFO_message("Global Ort time series length=%d: not ignoring initial points",
                        iset->gortim->nx ) ;
       ngvec = iset->gortim->ny ;
       gvec  = (float **)malloc(sizeof(float *)*ngvec) ;
       for( iv=0 ; iv < ngvec ; iv++ )
         gvec[iv] = MRI_FLOAT_PTR(iset->gortim) + iv*iset->gortim->nx ;

     } else {

       /* long enough to ignore initial points */

       if( iset->start > 0 )
         ININFO_message("Global Ort time series length=%d: ignoring first %d points",
                        iset->gortim->nx , iset->start ) ;
       ngvec = iset->gortim->ny ;
       gvec  = (float **)malloc(sizeof(float *)*ngvec) ;
       for( iv=0 ; iv < ngvec ; iv++ )
         gvec[iv] = MRI_FLOAT_PTR(iset->gortim) + iv*iset->gortim->nx + iset->start ;

     }
   }

   (void)THD_bandpass_OK( ntime , mv->dt , iset->fbot,iset->ftop , 1 ) ;

   iset->ndet = THD_bandpass_vectors( ntime, nmmm, dvec, mv->dt,
                                      iset->fbot, iset->ftop, iset->polort,
                                      ngvec, gvec ) ;

/** ININFO_message("Filtering removed %d DOF",iset->ndet) ; **/

   free(dvec) ; if( gvec != NULL ) free(gvec) ;

   /*--- Blur time series ---*/

   if( iset->blur > 0.0f ){
     int nrep ; float fx,fy,fz ;
     ININFO_message("- Starting %.1f mm blur %s" ,
                    iset->blur , (iset->mmm==NULL) ? "\0" : "(in mask)" ) ;
     mri_blur3D_getfac( iset->blur ,
                        mv->dx , mv->dy , mv->dz ,
                        &nrep , &fx , &fy , &fz ) ;
     ININFO_message("- Spatially blurring %d dataset volumes: %d iterations",mv->nvals,nrep) ;
     mri_blur3D_vectim( mv , iset->blur ) ;
   }

   /*-- normalize --*/
   
   if (THD_instacorr_cmeth_needs_norm(iset->cmeth)) {
      ININFO_message("- Normalizing dataset time series") ;
      THD_vectim_normalize( mv ) ;
   }

   RETURN(mv) ;
}

/*---------------------------------------------------------------------------*/
/*! Return value is 0 if an error occurs, or number of voxels prepared. */

int THD_instacorr_prepare( ICOR_setup *iset )
{
   int nmmm=0 ; byte *mmm=NULL ;

ENTRY("THD_instacorr_prepare") ;

   /*-- check inputs for a reasonable amount of usefulness --*/

   if( iset == NULL || !ISVALID_DSET(iset->dset) ){ EM("no dataset"); RETURN(0); }
   if( DSET_NVALS(iset->dset) < 4 )               { EM("nvals < 4") ; RETURN(0); }
   if( ISVALID_DSET(iset->eset) &&
       ( DSET_NVOX (iset->eset) != DSET_NVOX (iset->dset) ||
         DSET_NVALS(iset->eset) != DSET_NVALS(iset->dset)   ) ){ EM("eset <> dset"); RETURN(0); }

   /*-- create mask --*/

   if( iset->mmm != NULL ){ free(iset->mmm) ; iset->mmm = NULL ; }

   INFO_message("InstaCorr preparations:") ;

   /*-- automask? --*/

   if( iset->automask ){
     ININFO_message("Computing automask") ;
     mmm = THD_automask(iset->dset) ;
     if( mmm == NULL ){
       ERROR_message("Can't create automask from '%s'?!",DSET_BRIKNAME(iset->dset)) ;
       RETURN(0) ;
     }
     nmmm = THD_countmask( DSET_NVOX(iset->dset) , mmm ) ;
     if( nmmm < 9 ){
       ERROR_message("Automask from '%s' has %d voxels!" ,
                     DSET_BRIKNAME(iset->dset) , nmmm ) ;
       free(mmm) ; RETURN(0) ;
     }
     ININFO_message("Automask from '%s' has %d voxels",DSET_BRIKNAME(iset->dset),nmmm) ;
     iset->mmm = mmm ;
   }

   /*-- or read a mask dataset? ---*/

   if( iset->mmm == NULL && iset->mset != NULL ){
     if( DSET_NVOX(iset->mset) != DSET_NVOX(iset->dset) ){
       ERROR_message("Mask dataset '%s' doesn't match input dataset '%s'",
                     DSET_BRIKNAME(iset->mset) , DSET_BRIKNAME(iset->dset) ) ;
       RETURN(0) ;
     }
     ININFO_message("Extracting mask from mask dataset sub-brick #%d",iset->mindex) ;
     mmm = THD_makemask( iset->mset , iset->mindex , 1.0f,-1.0f ) ;
     if( mmm == NULL ){
       ERROR_message("Can't make mask from '%s[%d]'" ,
                     DSET_BRIKNAME(iset->mset) , iset->mindex ) ;
       RETURN(0) ;
     }
     nmmm = THD_countmask( DSET_NVOX(iset->mset) , mmm ) ;
     if( nmmm < 9 ){
       ERROR_message("Mask from '%s[%d]' has %d voxels!" ,
                     DSET_BRIKNAME(iset->mset) , iset->mindex , nmmm ) ;
       free(mmm) ; RETURN(0) ;
     }
     ININFO_message("Mask from '%s[%d]' has %d voxels!" ,
                   DSET_BRIKNAME(iset->mset) , iset->mindex , nmmm ) ;
     iset->mmm = mmm ;
   }

   if( iset->mmm == NULL ){
     ININFO_message("No mask for InstaCorr") ;
     nmmm = DSET_NVOX(iset->dset) ;
   }

   iset->mv = THD_instacorr_tsprep( iset , iset->dset ) ;
   if( iset->mv == NULL ) RETURN(0) ;
   iset->ev = THD_instacorr_tsprep( iset , iset->eset ) ;

   RETURN(nmmm) ;
}

/*---------------------------------------------------------------------------*/
/*! Compute the instant correlation from spatial voxel index ijk --
    mri_free() this image when you are done with it!
*//*-------------------------------------------------------------------------*/

MRI_IMAGE * THD_instacorr( ICOR_setup *iset , int ijk , int ata )
{
   int kk ; MRI_IMAGE *qim ; float *qar , *dar , *tsar ; int *ivar ;
   float sblur ;
   MRI_vectim *mv ;

ENTRY("THD_instacorr") ;

   if( iset == NULL || iset->mv == NULL || ijk < 0 ) RETURN(NULL) ;

   /** extract reference time series **/

   tsar = (float *)malloc(sizeof(float)*(iset->mv->nvals+iset->start)) ;
   kk   = THD_vectim_ifind( ijk , iset->mv ) ;

   if( kk >= 0 ){ /* direct from vectim, if available */

     memcpy( tsar , VECTIM_PTR(iset->mv,kk) , sizeof(float)*iset->mv->nvals ) ;

   } else {       /* non-vectim voxel in dataset ==> must be processed */

     free(tsar) ; RETURN(NULL) ;  /* don't allow this */

   }

   /** blur the ref time series, if ordered [15 May 2009] **/

   sblur = iset->sblur ;

   if( sblur != 0.0f ){
     int gblur = AFNI_yesenv("AFNI_INSTACORR_SEEDBLUR") ;
     int grad  = (gblur) ? 1.2345f*sblur : 1.0001f*sblur ;
     MCW_cluster *smask ;
     float wtsum=1.0f , fac , *qar ;
     float *sar=(float *)malloc(sizeof(float)*iset->mv->nvals)  ;
     int qi,qj,qk , ii,ij,ik , qjk,qq , nx,ny,nz,nxy ; register int tt ;

     if( grad > 0.0f )
       smask = MCW_spheremask( iset->mv->dx, iset->mv->dy, iset->mv->dz, grad ) ;
     else
       smask = MCW_spheremask( 1.0f, 1.0f, 1.0f, -grad ) ;

     nx = iset->mv->nx ; ny = iset->mv->ny ; nz = iset->mv->nz ; nxy = nx*ny ;
     ii = ijk % nx ; ik = ijk / nxy ; ij = (ijk-ik*nxy) / nx ;

     fac = FWHM_TO_SIGMA(sblur); fac = 1.0 / (2.0f * fac*fac); /* for gblur */

     memcpy(sar,tsar,sizeof(float)*iset->mv->nvals) ;  /* copy of seed */

     for( kk=1 ; kk < smask->num_pt ; kk++ ){  /* add stuff to this copy */
       qi  = ii + smask->i[kk] ; if( qi < 0 || qi >= nx ) continue ;
       qj  = ij + smask->j[kk] ; if( qj < 0 || qj >= ny ) continue ;
       qk  = ik + smask->k[kk] ; if( qk < 0 || qk >= nz ) continue ;
       qjk = qi + qj*nx + qk*nxy ;
       qq  = THD_vectim_ifind( qjk , iset->mv ) ;
       if( qq >= 0 ){
         register float wt ;
         if( gblur ){ float rad=smask->mag[kk]; wt = exp(-fac*rad*rad); } else wt = 1.0f;
          wtsum += wt ; qar = VECTIM_PTR(iset->mv,qq) ;
          for( tt=0 ; tt < iset->mv->nvals  ; tt++ ) sar[tt] += wt * qar[tt] ;
         }
       }
       if( wtsum > 1.0f ){
         fac = 1.0f / wtsum ;
         for( tt=0 ; tt < iset->mv->nvals ; tt++ ) tsar[tt] = fac * sar[tt] ;
       }
       free(sar) ; KILL_CLUSTER(smask) ;
       THD_normalize( iset->mv->nvals , tsar ) ;
   } /* end of seed blur */

   /** save seed in iset struct [15 May 2009] **/

   iset->tseed = (float *)realloc( iset->tseed , sizeof(float)*iset->mv->nvals ) ;
   memcpy( iset->tseed , tsar , sizeof(float)*iset->mv->nvals ) ;

   /** do the correlations **/

   dar = (float *)malloc(sizeof(float)*iset->mv->nvec) ;

   mv = (iset->ev == NULL) ? iset->mv : iset->ev ;

   switch( iset->cmeth ){
     default:
     case NBISTAT_PEARSON_CORR:
       THD_vectim_dotprod ( mv , tsar , dar , ata ) ; break ;

     case NBISTAT_SPEARMAN_CORR:
       THD_vectim_spearman( mv , tsar , dar ) ; break ;

     case NBISTAT_QUADRANT_CORR:
       THD_vectim_quadrant( mv , tsar , dar ) ; break ;

     case NBISTAT_TICTACTOE_CORR:
       THD_vectim_tictactoe( mv , tsar , dar ) ; break ;

     case NBISTAT_KENDALL_TAUB:
       THD_vectim_ktaub( mv , tsar , dar ) ; break ;  /* 29 Apr 2010 */

     case NBISTAT_BC_PEARSON_M:
       THD_vectim_pearsonBC( mv,sblur,ijk,0,dar ); break; /* 07 Mar 2011 */

     case NBISTAT_BC_PEARSON_V:
       THD_vectim_pearsonBC( mv,sblur,ijk,1,dar ); break; /* 07 Mar 2011 */
       
     case NBISTAT_EUCLIDIAN_DIST:/* 04 Apr 2012, ZSS*/
       THD_vectim_distance( mv , tsar , dar , 0, "inv;n_scale") ; break ;  

     case NBISTAT_CITYBLOCK_DIST:/* 04 Apr 2012, ZSS*/
       THD_vectim_distance( mv , tsar , dar , 1, "inv;n_scale") ; break ;  

     case NBISTAT_QUANTILE_CORR: /* 11 May 2012 */
       THD_vectim_quantile( mv , tsar , dar ) ; break ;
   }

   /** put them into the output image **/

   qim  = mri_new_vol( mv->nx , mv->ny , mv->nz , MRI_float ) ;
   qar  = MRI_FLOAT_PTR(qim) ;
   ivar = mv->ivec ;
   for( kk=0 ; kk < mv->nvec ; kk++ ) qar[ivar[kk]] = dar[kk] ;

   /** e finito **/

   free(dar) ; free(tsar) ; RETURN(qim) ;
}
