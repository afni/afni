#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

#define SPEARMAN 1
#define QUADRANT 2
#define PEARSON  3
#define KTAUB    4
#define DOT      5

#undef  MYatanh
#define MYatanh(x) ( ((x)<-0.999329f) ? -4.0f                \
                    :((x)>+0.999329f) ? +4.0f : atanhf(x) )

/*-------------------------------------------------------------*/
float THD_dotprod( int nx , float *xx , float *yy ){
  int jj ; float sum=0.0f ;
  for( jj=0 ; jj < nx ; jj++ ) sum += xx[jj]*yy[jj] ;
  return sum ;
}
/*-------------------------------------------------------------*/

THD_3dim_dataset *THD_Tcorr1D(THD_3dim_dataset *xset, byte *mask, int nmask,
                              MRI_IMAGE *ysim,
                              char *smethod, char *prefix, int do_short , int do_atanh )
{
   THD_3dim_dataset *cset = NULL;
   int method=PEARSON ;
   int ny, kk, datum=MRI_float ; char str[32], fmt[32] ; float cfac=0.0f,sfac=0.0f ;
   float (*corfun)(int,float *,float *) = NULL ;  /* ptr to corr function */
   int nvox , nvals , ii;
   int nconst=0 ;

ENTRY("THD_Tcorr1D");

   if( do_short ) datum = MRI_short ;  /* 30 Jan 2017 */

   if (!smethod || smethod[0] == '\0') {
     method = PEARSON;
   } else if (!strcmp(smethod,"pearson")) {
     method = PEARSON;
   } else if (!strcmp(smethod,"spearman")) {
     method = SPEARMAN;
   } else if (!strcmp(smethod,"quadrant")) {
     method = QUADRANT;
   } else if (!strcmp(smethod,"ktaub")) {
     method = KTAUB;
   } else if (!strcmp(smethod,"dot")) {
     method = DOT;
   } else {
     ERROR_message("Bad value %s for correlation method", smethod);
     RETURN(NULL);
   }

   if (!prefix) prefix = "Tcorr1D";

   nvals = DSET_NVALS(xset) ;  /* number of time points */

   ii = (method==DOT) ? 2 : 3 ;
   if( nvals < ii )
     ERROR_exit("Input dataset length (%d) is less than %d?!", nvals,ii) ;

   if( ysim->nx < nvals )
     ERROR_exit("ysim has %d time points, but dataset has %d values",
                ysim->nx,nvals) ;
   else if( ysim->nx > nvals )
     WARNING_message("ysim has %d time points, dataset has %d",
                     ysim->nx,nvals) ;

   if( mri_allzero(ysim) )
     ERROR_exit("ysim is all zero!") ;

   ny = ysim->ny ;
   if( ny > 1 )
     INFO_message("ysim has %d columns: correlating with ALL of them!",
                   ny) ;

   ININFO_message("loading dataset %s into memory",DSET_BRIKNAME(xset)) ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   nvox = DSET_NVOX(xset) ;
   if( mask == NULL ) nmask = nvox ;

   /*-- create output dataset --*/

   cset = EDIT_empty_copy( xset ) ;
   EDIT_dset_items( cset ,
                      ADN_prefix    , prefix         ,
                      ADN_nvals     , ny             ,
                      ADN_ntt       , 0              , /* no time axis */
                      ADN_type      , HEAD_FUNC_TYPE ,
                      ADN_func_type , FUNC_BUCK_TYPE ,
                    ADN_none ) ;

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(cset)) )
     ERROR_exit("Output dataset %s already exists!",DSET_HEADNAME(cset)) ;

        if( ny <=    10 ) kk = 1 ;  /* number of digits for */
   else if( ny <=   100 ) kk = 2 ;  /* brick label string */
   else if( ny <=  1000 ) kk = 3 ;
   else if( ny <= 10000 ) kk = 4 ;
   else                   kk = 5 ;
   switch( method ){              /* brick label string format */
     default:
     case PEARSON:  sprintf(fmt,"PearCorr#%%0%dd",kk) ; break ;
     case SPEARMAN: sprintf(fmt,"SpmnCorr#%%0%dd",kk) ; break ;
     case QUADRANT: sprintf(fmt,"QuadCorr#%%0%dd",kk) ; break ;
     case KTAUB:    sprintf(fmt,"TaubCorr#%%0%dd",kk) ; break ;
     case DOT:      sprintf(fmt, "DotProd#%%0%dd",kk) ; break ;
   }
   if( datum == MRI_short ){
      cfac = (do_atanh) ? 0.000125f : 0.0001f ;  /* scale factor for -short */
      sfac = 1.0f/cfac + 0.111f ;
   }

   /* for each sub-brick in output file */

   for( kk=0 ; kk < ny ; kk++ ){
     EDIT_substitute_brick(cset,kk,datum,NULL) ; /* make brick */
     EDIT_BRICK_TO_FICO(cset,kk,nvals,1,1) ;    /* stat params */
     EDIT_BRICK_FACTOR(cset,kk,cfac) ;     /* set brick factor */
     sprintf(str,fmt,kk) ;
     EDIT_BRICK_LABEL(cset,kk,str) ;         /* labelize brick */
   }

   switch( method ){               /* set correlation function */
     default:
     case PEARSON:  corfun = THD_pearson_corr  ; break ;
     case SPEARMAN: corfun = THD_spearman_corr ; break ;
     case QUADRANT: corfun = THD_quadrant_corr ; break ;
     case KTAUB:    corfun = THD_ktaub_corr    ; break ;
     case DOT:      corfun = THD_dotprod       ; break ;
   }

   /* 27 Jun 2010: OpenMP-ize over columns in ysim */

 AFNI_OMP_START ;
#pragma omp parallel if( ny > 1 )
 { float *ysar, *xsar, *fcar = NULL, *ydar, val ;
   int ii, kk, jj ; short *scar = NULL;

#ifdef USE_OMP
   if( omp_get_thread_num() == 0 )
     INFO_message("Start correlations: %d voxels X %d time series(%d); %d threads",
                  nmask , ny , nvals , omp_get_num_threads() ) ;
#else
   INFO_message("Start correlations: %d voxels X %d time series(%d)",nmask,ny,nvals) ;
#endif

   ydar = (float *)malloc(sizeof(float)*nvals) ;  /* 1D data duplicate */
   xsar = (float *)malloc(sizeof(float)*nvals) ;  /* 3D data duplicate */

   /* 27 Jun 2010: loop over columns in ysim */

#pragma omp for
   for( kk=0 ; kk < ny ; kk++ ){  /* loop over ysim columns */
     if( datum == MRI_short ) scar = DSET_ARRAY(cset,kk) ; /* output array */
     else                     fcar = DSET_ARRAY(cset,kk) ;
     ysar = MRI_FLOAT_PTR(ysim) + (kk * ysim->nx) ;     /* 1D data pointer */

     /* loop over voxels, correlate */

     for( ii=0 ; ii < nvox ; ii++ ){

       if( mask != NULL && mask[ii] == 0 ) continue ;    /* skip this'n */

       /* get time series to correlate */

       (void)THD_extract_array(ii,xset,0,xsar) ;             /* 3D data */
       for( jj=1 ; jj < nvals && xsar[jj]==xsar[0] ; jj++ ) ;   /* nada */
       if( jj == nvals ){                           /* data was constant */
#pragma omp atomic
         nconst++ ;
         continue ;
       }
       for( jj=0 ; jj < nvals ; jj++ ) ydar[jj] = ysar[jj] ; /* 1D data */

       val = corfun( nvals , xsar , ydar ) ;         /* !! correlate !! */
       if( do_atanh ) val = MYatanh(val) ;

       if( datum == MRI_short ) scar[ii] = (short)(sfac*val) ;
       else                     fcar[ii] = val ;

     } /* end of loop over voxels */

#pragma omp critical
     { if( ny > 1 ) fprintf(stderr,"[%d]",kk) ; }
   } /* end of loop over ysim columns */

   free(ydar) ; free(xsar) ;
 } /* end OpenMP */
 AFNI_OMP_END ;

   if( ny > 1 ){ fprintf(stderr,"\n") ; nconst /= ny ; }
   if( nconst > 0 )
     WARNING_message("THD_Tcorr1D: %d voxel%s skipped because were constant in time",
                     nconst , (nconst==1) ? "\0" : "s" ) ;

   RETURN(cset);
}
