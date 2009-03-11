#include "mrilib.h"

/*--------------------------------------------------------------------------*/
static float hbot1 =  1.0f ;
static float htop1 = -1.0f ;
static float hbot2 =  1.0f ;
static float htop2 = -1.0f ;
void mri_nbistat_setclip( float hb1, float ht1 , float hb2, float ht2 )
{
  hbot1 = hb1 ; htop1 = ht1 ; hbot2 = hb2 ; htop2 = ht2 ;
}

/*--------------------------------------------------------------------------*/
static MRI_IMAGE *wim = NULL ;
static MRI_IMAGE *wnim = NULL ;

void mri_bistat_setweight( MRI_IMAGE *wm )  /* 14 Aug 2007 */
{
   if( wim != NULL ){ mri_free(wim); wim = NULL; }
   if( wm != NULL ){ wim = mri_to_float(wm); }
   return ;
}

/*--------------------------------------------------------------------------*/
/*! Input = 2 1D images, and an NBISTAT_ code to compute some statistic.
   Output = statistic's value.
*//*------------------------------------------------------------------------*/

float mri_nbistat( int code , MRI_IMAGE *im , MRI_IMAGE *jm )
{
   MRI_IMAGE *fim , *gim ;
   float     *far , *gar ; float outval=0.0f ;
   int npt , ii ;

   if( im == NULL || jm == NULL || im->nvox == 0 || im->nvox != jm->nvox )
     return outval ;

   /* convert input to float format, if not already there */

   fim = (im->kind == MRI_float) ? im : mri_to_float(im) ;
   gim = (jm->kind == MRI_float) ? jm : mri_to_float(jm) ;
   far = MRI_FLOAT_PTR(fim) ;  /* array of values to statisticate */
   gar = MRI_FLOAT_PTR(gim) ;
   npt = fim->nvox ;           /* number of values */

   if( hbot1 < htop1 ){
     for( ii=0 ; ii < npt ; ii++ )
            if( far[ii] < hbot1 ) far[ii] = hbot1 ;
       else if( far[ii] > htop1 ) far[ii] = htop1 ;
   }
   if( hbot2 < htop2 ){
     for( ii=0 ; ii < npt ; ii++ )
            if( gar[ii] < hbot2 ) gar[ii] = hbot2 ;
       else if( gar[ii] > htop2 ) gar[ii] = htop2 ;
   }

   switch( code ){

     case NBISTAT_NUM: outval = (float)npt ; break ;  /* quite easy */

     case NBISTAT_SPEARMAN_CORR:
       outval = THD_spearman_corr( npt , far , gar ) ; break ;

     case NBISTAT_QUADRANT_CORR:
       outval = THD_quadrant_corr( npt , far , gar ) ; break ;

     case NBISTAT_PEARSON_CORR:
       if( wnim == NULL )
         outval = THD_pearson_corr( npt , far , gar ) ;
       else
         outval = THD_pearson_corr_wt( npt , far , gar , MRI_FLOAT_PTR(wnim) ) ;
       break ;

     case NBISTAT_MUTUAL_INFO:
       outval = THD_mutual_info( npt , far , gar ) ; break ;

     case NBISTAT_NORMUT_INFO:
       outval = THD_norm_mutinf( npt , far , gar ) ;
       if( outval != 0.0f ) outval = 1.0f / outval ;
       break ;

     case NBISTAT_JOINT_ENTROPY:
       outval = THD_jointentrop( npt , far , gar ) ; break ;

     case NBISTAT_HELLINGER:
       outval = THD_hellinger( npt , far , gar ) ; break ;

     case NBISTAT_CORR_RATIO_M:
       THD_corr_ratio_mode(1) ;
       outval = THD_corr_ratio( npt , far , gar ) ; break ;

     case NBISTAT_CORR_RATIO_A:
       THD_corr_ratio_mode(2) ;
       outval = THD_corr_ratio( npt , far , gar ) ; break ;

     case NBISTAT_CORR_RATIO_U:
       THD_corr_ratio_mode(0) ;
       outval = THD_corr_ratio( npt , far , gar ) ; break ;

     case NBISTAT_NCD:
       outval = THD_ncdfloat( npt , far , gar ) ; break ;
   }

   /* cleanup and exit */

   if( fim != im  ) mri_free(fim) ;
   if( gim != jm  ) mri_free(gim) ;
   return outval ;
}

/*--------------------------------------------------------------------------*/
/*! Compute a local statistic at each voxel of an image pair, possibly with
    a mask; 'local' is defined with a neighborhood; 'statistic' is defined
    by an NBISTAT_ code.
*//*------------------------------------------------------------------------*/

MRI_IMAGE * mri_localbistat( MRI_IMAGE *im, MRI_IMAGE *jm ,
                             byte *mask, MCW_cluster *nbhd, int code )
{
   MRI_IMAGE *outim , *nbim , *nbjm ;
   float     *outar ;
   int ii,jj,kk , nx,ny,nz , ijk ;

ENTRY("mri_localbistat") ;

   if( im == NULL || nbhd == NULL ) RETURN(NULL) ;

   outim = mri_new_conforming( im , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;
   nx = outim->nx ; ny = outim->ny ; nz = outim->nz ;

   ijk = (int)cbrt((double)nbhd->num_pt) ;  /* for entropy, etc. */
   set_2Dhist_hbin( ijk ) ;

   for( ijk=kk=0 ; kk < nz ; kk++ ){
    for( jj=0 ; jj < ny ; jj++ ){
     for( ii=0 ; ii < nx ; ii++ ){
       nbim = mri_get_nbhd( im , mask , ii,jj,kk , nbhd ) ;
       nbjm = mri_get_nbhd( jm , mask , ii,jj,kk , nbhd ) ;
       outar[ijk++] = mri_nbistat( code , nbim , nbjm ) ;
       mri_free(nbim) ; mri_free(nbjm) ;
   }}}

   RETURN(outim) ;
}

/*--------------------------------------------------------------------------*/

static int verb=0 , vn=0 ;
void THD_localbistat_verb(int i){ verb=i; vn=0; }

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/*--------------------------------------------------------------------------*/

THD_3dim_dataset * THD_localbistat( THD_3dim_dataset *dset ,
                                    THD_3dim_dataset *eset , byte *mask ,
                                    MCW_cluster *nbhd , int ncode, int *code )
{
   THD_3dim_dataset *oset ;
   MRI_IMAGE *nbim , *nbjm ;
   int iv,cc , nvin,nvout , nx,ny,nz,nxyz , ii,jj,kk,ijk ;
   float **aar ;
   int vstep ;

ENTRY("THD_localbistat") ;

   if( dset == NULL || eset == NULL ||
       nbhd == NULL || ncode < 1    || code == NULL ) RETURN(NULL);

   if( DSET_NVOX(dset) != DSET_NVOX(eset) )   RETURN(NULL) ;
   DSET_load(dset) ; if( !DSET_LOADED(dset) ) RETURN(NULL) ;
   DSET_load(eset) ; if( !DSET_LOADED(eset) ) RETURN(NULL) ;

   oset  = EDIT_empty_copy( dset ) ;
   nvin  = DSET_NVALS( dset ) ;
   nvout = nvin * ncode ;
   EDIT_dset_items( oset ,
                      ADN_nvals     , nvout         ,
                      ADN_datum_all , MRI_float     ,
                      ADN_ntt       , nvout         ,
                      ADN_nsl       , 0             ,
                      ADN_brick_fac , NULL          ,
                      ADN_prefix    , "localbistat" ,
                    ADN_none ) ;

   nx = DSET_NX(dset) ;
   ny = DSET_NY(dset) ;
   nz = DSET_NZ(dset) ; nxyz = nx*ny*nz ;

   vstep = (verb && nxyz > 66666) ? nxyz/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ voxel loop:") ;

   aar = (float **)malloc(sizeof(float *)*ncode) ;

   for( iv=0 ; iv < nvin ; iv++ ){
     for( cc=0 ; cc < ncode ; cc++ ){
       aar[cc] = (float *)malloc(sizeof(float)*nxyz) ;
       if( aar[cc] == NULL )
         ERROR_exit("THD_localbistat: out of memory at iv=%d cc=%d",iv,cc);
     }

     for( ijk=kk=0 ; kk < nz ; kk++ ){
      for( jj=0 ; jj < ny ; jj++ ){
       for( ii=0 ; ii < nx ; ii++,ijk++ ){
         if( vstep && ijk%vstep==vstep-1 ) vstep_print() ;
         nbim = THD_get_dset_nbhd( dset,iv , mask,ii,jj,kk , nbhd ) ;
         nbjm = THD_get_dset_nbhd( eset,iv , mask,ii,jj,kk , nbhd ) ;
         if( wim != NULL ) wnim = mri_get_nbhd( wim , mask,ii,jj,kk , nbhd ) ;
         for( cc=0 ; cc < ncode ; cc++ )
           aar[cc][ijk] = mri_nbistat( code[cc] , nbim,nbjm ) ;
         mri_free(nbim) ; mri_free(nbjm) ;
         if( wnim != NULL ){ mri_free(wnim); wnim = NULL; }
     }}}

     DSET_unload_one(dset,iv) ; DSET_unload_one(eset,iv)  ;

     for( cc=0 ; cc < ncode ; cc++ )
       EDIT_substitute_brick( oset , iv*ncode+cc , MRI_float , aar[cc] ) ;
   }

   if( vstep ) fprintf(stderr,"\n") ;
   free((void *)aar) ;
   RETURN(oset) ;
}
