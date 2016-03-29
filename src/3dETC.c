#include "mrilib.h"

/* CATIE = Clustering Across Threshold Intervals Equitably */
/* ETC   = Equitable Threshold Clustering */
/* ETIC  = Equitable Threshold Interval Clustering */
/* CWET  = Clustering With Equitable Thresholding */
/* TICS  = Threshold Interval Cluster Significance */
/* SETIC = Significance of Equitable Threshold Interval Clustering */
/* CITES = Cluster Interval Thresholding with Equitable Significance */

/*----------------------------------------------------------------------------*/
/* bim      = image to clusterize
   statcode = statistical code for tim
   statpar  = statistical parameters for tim
   tim      = threshold image
   mask     = mask (or NULL)
   nthresh  = number of thresholds
   pthr     = p-value of thresholds [nthresh of them]
   cthr     = cluster size of thresholds [nthresh]
   nnlev    = NN level (1 or 2 or 3)
   signmeth = sign method:
                0 = normal method
                1 = keep only positive values from tim
               -1 = keep only negative values from tim
               66 = bi-sided clustering
*//*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_multi_threshold_clusterize(
              MRI_IMAGE *bim ,
              int statcode , float *statpar , MRI_IMAGE *tim ,
              byte *mask ,
              int nthresh , float *pthr , float *cthr ,
              int nnlev , int signmeth                 )
{
   int nx,ny,nz , ith,iclu,ptmin,ngood,nadd ; size_t ndar ;
   float rmm,pval,thr ;
   MRI_IMAGE *cim ; float *car ;
   MRI_IMAGE *uim ; float *uar ;
   MRI_IMAGE *dim ; float *dar ;
   MRI_IMAGE *eim ; float *ear ;
   MCW_cluster_array *clar ; MCW_cluster *cl ;

ENTRY("mri_multi_threshold_clusterize") ;

   if( bim == NULL || tim  == NULL                 ) RETURN(NULL) ;
   if( nthresh < 1 || pthr == NULL || cthr == NULL ) RETURN(NULL) ;

   nx = bim->nx ; ny = bim->ny ; nz = bim->nz ;
   if( tim->nx != nx || tim->ny != ny || tim->nz != nz ) RETURN(NULL) ;

   /* float copy of input volumes */

   cim = mri_to_float(bim) ; car = MRI_FLOAT_PTR(cim) ;
   uim = mri_to_float(tim) ; uar = MRI_FLOAT_PTR(uim) ;

   /* edit the input volumes as ordered */

        if( signmeth ==  1 ) mri_threshold( -1.e33 , 0.0 , uim , uim ) ;
   else if( signmeth == -1 ) mri_threshold(  0.0 , 1.e33 , uim , uim ) ;
   mri_maskify( uim , mask ) ; mri_maskify( cim , mask ) ;

   switch( nnlev ){
     default: rmm = 1.01f ; break ;   /* NN1 */
     case 2:  rmm = 1.44f ; break ;   /* NN2 */
     case 3:  rmm = 1.75f ; break ;   /* NN3 */
   }


   dim = mri_new_vol( nx,ny,nz , MRI_float) ;
   dar = MRI_FLOAT_PTR(dim) ; ndar = sizeof(float)*nx*ny*nz ;

   eim = mri_new_vol( nx,ny,nz , MRI_float) ;  /* zero filled == output */
   ear = MRI_FLOAT_PTR(eim) ;

   for( ith=0 ; ith < nthresh ; ith++ ){
     memcpy( dar , car , ndar ) ;
     pval = pthr[ith] ;
#if 0
     if( statcode != FUNC_FT_TYPE &&
         THD_stat_is_2sided(statcode,signmeth) ) pval *= 2.0f ;
#endif
     thr = THD_pval_to_stat(pval,statcode,statpar) ;
     mri_threshold( -thr , thr , uim , dim ) ;

     if( signmeth != 66 )
       clar = MCW_find_clusters( nx,ny,nz, 1.0f,1.0f,1.0f, MRI_float,dar,rmm ) ;
     else
       ERROR_exit("bi_clusterize not implemented yet :-(") ;

     if( clar == NULL ){
#if 1
ININFO_message("ith=%d pthr=%g thr=%g cthr=%g nclu=0 nclu_good=0 nvox_add=0" ,
  ith,pval,thr,cthr[ith] ) ;
#endif
       continue ; /* pval threshold so high that we got nuthin */
     }

     ptmin = (int)(cthr[ith]+0.951f) ;
     for( nadd=ngood=iclu=0 ; iclu < clar->num_clu ; iclu++ ){
       cl = clar->clar[iclu] ;
       if( cl->num_pt >= ptmin ){
         MCW_cluster_to_vol( nx,ny,nz , MRI_float,ear , cl ) ;
         ngood++ ; nadd += cl->num_pt ;
       }
     }

#if 1
ININFO_message("ith=%d pthr=%g thr=%g cthr=%g nclu=%d nclu_good=%d nvox_add=%d" ,
  ith,pval,thr,cthr[ith],(clar==NULL)?0:clar->num_clu,ngood,nadd ) ;
#endif
   }

   mri_free(dim) ; mri_free(uim) ; mri_free(cim) ;
   RETURN(eim) ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset=NULL , *oset=NULL ;
   MRI_IMAGE *thim=NULL ; float *thar ; int nthar ;
   byte *mask=NULL ; int nmask=0 ;
   MRI_IMAGE *datim=NULL, *thrim=NULL , *outim=NULL ;
   int iarg , dind=0 , tind=1 , ith , nnlev=1 ;
   int scode ; float *spar=NULL ;
   char *prefix = "ETC.nii" ;

   /*-- some pitiful help --*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
      "Usage: 3dETC [options] inputdataset\n"
      "\n"
      "Options:\n"
      "========\n"
      " -input dset  = alternative way to input the dataset\n"
      " -prefix ppp  = output prefix\n"
      " -thresh ttt  = 1D file with\n"
      "                 column #1 = p-value\n"
      "                 column #2 = cluster threshold\n"
      " -mask   mmm  = dataset with mask\n"
      " -1dindex ii  = output comes from sub-brick #ii\n"
      " -1tindex jj  = threshold on sub-brick #jj\n"
      " -NN      nn  = nn is 1 or 2 or 3 [default 1]\n"
      "\n"
      "-- Experimental - RWCox - 24 Dec 2015\n"
     ) ;
     exit(0) ;
   }

   /*-- scan options --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       if( dset != NULL )
         ERROR_exit("You can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg >= argc )
         ERROR_exit("Option '%s' needs an argument to follow!",argv[iarg-1]) ;
       dset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(dset,argv[iarg]) ;
       DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-mask") == 0 ){
       bytevec *bvec ; int nmask_hits ;
       if( mask != NULL )
         ERROR_exit("Can't use '-mask' twice!") ;
       if( ++iarg >= argc )
         ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       bvec = THD_create_mask_from_string(argv[iarg]) ;
       if( bvec == NULL )
         ERROR_exit("Can't create mask from '-mask' option") ;
       mask = bvec->ar ; nmask = bvec->nar ;
       nmask_hits = THD_countmask( nmask , mask ) ;
       if( nmask_hits > 0 )
         INFO_message("%d voxels in -mask definition (out of %d total)",
                      nmask_hits,nmask) ;
       else
         ERROR_exit("no nonzero voxels in -mask dataset") ;
       iarg++ ; continue ;
     }


     if( strcasecmp(argv[iarg],"-prefix") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Option '%s' needs an argument to follow!",argv[iarg-1]) ;
       prefix = strdup(argv[iarg]) ;
       if( ! THD_filename_ok(prefix) )
         ERROR_exit("-prefix '%s' is not a good filename prefix",prefix) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-1tindex") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Option '%s' needs an argument to follow!",argv[iarg-1]) ;
       tind = (int)strtod(argv[iarg],NULL) ;
       if( tind < 0 )
         ERROR_exit("-tind '%s' is illegal!",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-1dindex") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Option '%s' needs an argument to follow!",argv[iarg-1]) ;
       dind = (int)strtod(argv[iarg],NULL) ;
       if( dind < 0 )
         ERROR_exit("-dind '%s' is illegal!",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-NN") == 0 ){
       if( ++iarg >= argc )
         ERROR_exit("Option '%s' needs an argument to follow!",argv[iarg-1]) ;
       nnlev = (int)strtod(argv[iarg],NULL) ;
       if( nnlev < 1 || nnlev > 3 )
         ERROR_exit("-nnlev '%s' is illegal!",argv[iarg]) ;
       iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-NN1") == 0 ){
       nnlev = 1 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-NN2") == 0 ){
       nnlev = 2 ; iarg++ ; continue ;
     }
     if( strcasecmp(argv[iarg],"-NN3") == 0 ){
       nnlev = 3 ; iarg++ ; continue ;
     }

     if( strcasecmp(argv[iarg],"-thresh") == 0 ){
       int nbad=0 ;
       if( thim != NULL )
         ERROR_exit("You can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg >= argc ) ERROR_exit("Option '%s' needs an argument to follow!",argv[iarg-1]) ;
       thim = mri_read_1D( argv[iarg] ) ;
       if( thim == NULL )
         ERROR_exit("Cannot read file from option -thresh '%s'",argv[iarg]) ;
       if( thim->ny < 2 )
         ERROR_exit("-thresh '%s' doesn't have at least 2 columns!",argv[iarg]) ;
       nthar = thim->nx ; thar  = MRI_FLOAT_PTR(thim) ;
       for( ith=0 ; ith < nthar ; ith++ ){
         if( thar[ith] <= 0.0f || thar[ith] > 0.10f ) nbad++ ;
       }
       if( nbad > 0 )
         ERROR_exit("Some value%s in -thresh '%s' Column #1 %s outside 0 < p <= 0.1 :-(",
                    (nbad==1)?"\0":"s" , argv[iarg] ,
                    (nbad==1)?"is":"are" ) ;
       for( ith=0 ; ith < nthar ; ith++ ){
         if( thar[ith+nthar] < 1.0f ) nbad++ ;
       }
       if( nbad > 0 )
         ERROR_exit("Some value%s in -thresh '%s' Column #2 %s less than 1 :-(",
                    (nbad==1)?"\0":"s" , argv[iarg] ,
                    (nbad==1)?"is":"are" ) ;
       INFO_message("-thresh table has %d levels of thresholding",nthar) ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s' :-(",argv[iarg] ) ; exit(1) ;
   }

   /*-- did we get the input dataset yet? --*/

   if( dset == NULL ){
     if( iarg >= argc ) ERROR_exit("no input dataset?!") ;
     dset = THD_open_dataset( argv[iarg] ) ;
     CHECK_OPEN_ERROR(dset,argv[iarg]) ;
     DSET_load(dset) ; CHECK_LOAD_ERROR(dset) ;
     iarg++ ;
   }

   /*-- check things --*/

   if( nmask > 0 && DSET_NVOX(dset) != nmask )
     ERROR_exit("mask and input datasets don't match in number of voxels") ;

   if( thim == NULL ) ERROR_exit("no -thresh option was given!?") ;

   if( dind >= DSET_NVALS(dset) )
     ERROR_exit("data index %d is beyond end of input dataset!",dind) ;
   if( tind >= DSET_NVALS(dset) )
     ERROR_exit("threshold index %d is beyond end of input dataset!",tind) ;

   /*-- record things for posterity, et cetera --*/

   mainENTRY("3dETC main"); machdep(); AFNI_logger("3dETC",argc,argv);
   PRINT_VERSION("3dETC") ; AUTHOR("Bob the Equable") ;

   /*-- get the data and threshold volumes --*/

   datim = THD_extract_float_brick( dind , dset ) ;
   thrim = THD_extract_float_brick( tind , dset ) ;
   if( datim == NULL || thrim == NULL )  /* should be impossible */
     ERROR_exit("Can't get data and/or thresh data from input dataset???") ;
   DSET_unload(dset) ;

   scode = DSET_BRICK_STATCODE(dset,tind) ;
   if( scode < 0 )
     ERROR_exit("thresh sub-brick index %d is NOT a statistical volume!?",tind) ;
   spar = DSET_BRICK_STATAUX(dset,tind) ;

#if 1
INFO_message("value range in thrim = %g .. %g",mri_min(thrim),mri_max(thrim)) ;
#endif

   outim = mri_multi_threshold_clusterize(
             datim , scode,spar,thrim , mask ,
             nthar , thar , thar+nthar , nnlev , 0 ) ;

   oset = EDIT_empty_copy(dset) ;
   EDIT_dset_items( oset ,
                      ADN_prefix    , prefix ,
                      ADN_nvals     , 1 ,
                      ADN_ntt       , 0 ,
                      ADN_brick_fac , NULL ,
                      ADN_type      , HEAD_FUNC_TYPE ,
                      ADN_func_type , FUNC_BUCK_TYPE ,
                    ADN_none ) ;
   EDIT_substitute_brick( oset , 0 , MRI_float , MRI_FLOAT_PTR(outim) ) ;
   DSET_write(oset) ; WROTE_DSET(oset) ;

   INFO_message("# nonzero voxels = %d",mri_nonzero_count(outim)) ;

   exit(0) ;
}
