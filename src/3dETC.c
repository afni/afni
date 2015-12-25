#include "mrilib.h"

/* CATIE = Clustering Across Threshold Intervals Equitably */
/* ETC   = Equitable Threshold Clustering */
/* CWET  = Clustering With Equitable Thresholding */
/* TICS  = Threshold Interal Cluster Significance */

MRI_IMAGE * mri_multi_threshold_clusterize(
              MRI_IMAGE *bim , MRI_IMAGE *tim ,
              int nnlev , int thr_signed ,
              int nthresh , float *thr , float *cthr , byte *mask )
{
   int nx,ny,nz , ith,iclu,ptmin ; float rmm ; size_t ndar ;
   MRI_IMAGE *cim ; float *car ;
   MRI_IMAGE *uim ; float *uar ;
   MRI_IMAGE *dim ; float *dar ;
   MRI_IMAGE *eim ; float *ear ;
   MCW_cluster_array *clar ; MCW_cluster *cl ;

ENTRY("mri_multi_threshold_clusterize") ;

   if( bim == NULL || tim == NULL                 ) RETURN(NULL) ;
   if( nthresh < 1 || thr == NULL || cthr == NULL ) RETURN(NULL) ;

   nx = bim->nx ; ny = bim->ny ; nz = bim->nz ;
   if( tim->nx != nx || tim->ny != ny || tim->nz != nz ) RETURN(NULL) ;

   /* float copy of input volumes */

   cim = mri_to_float(bim) ; car = MRI_FLOAT_PTR(cim) ;
   uim = mri_to_float(bim) ; uar = MRI_FLOAT_PTR(cim) ;

   /* edit the input volumes as ordered */

        if( thr_signed > 0 ) mri_threshold( -1.e22 , 0.0 , uim , uim ) ;
   else if( thr_signed < 0 ) mri_threshold(  0.0 , 1.e22 , uim , uim ) ;
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
     mri_threshold( -thr[ith] , thr[ith] , uim , dim ) ;

     clar = MCW_find_clusters( nx,ny,nz , 1.0f,1.0f,1.0f , MRI_float,dar , rmm ) ;

     if( clar == NULL ) break ;

     ptmin = (int)(cthr[ith]+0.951f) ;
     for( iclu=0 ; iclu < clar->num_clu ; iclu++ ){
       cl = clar->clar[iclu] ;
       if( cl->num_pt >= ptmin )
         MCW_cluster_to_vol( nx,ny,nz , MRI_float,ear , cl ) ;
     }
   }

   mri_free(dim) ; mri_free(uim) ; mri_free(cim) ;
   RETURN(eim) ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *dset=NULL , *oset=NULL ;
   MRI_IMAGE *thim=NULL ; float *thar ; int nthar ;
   int iarg , dind=0 , tind=1 , ith ;
   char *prefix = "ETC.nii" ;

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
      " -1dindex ii  = output comes from sub-brick #ii\n"
      " -1tindex jj  = threshold on sub-brick #jj\n"
      "\n"
      "-- Experimental - RWCox - 24 Dec 2015\n"
     ) ;
     exit(0) ;
   }

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcasecmp(argv[iarg],"-input") == 0 ){
       if( dset != NULL )
         ERROR_exit("You can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg >= argc )
         ERROR_exit("Option '%s' needs an argument to follow!",argv[iarg-1]) ;
       dset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(dset,argv[iarg]) ;
       DSET_load(dset) ;
       CHECK_LOAD_ERROR(dset) ;
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

     if( strcasecmp(argv[iarg],"-thresh") == 0 ){
       int nbad=0 ;
       if( thim != NULL )
         ERROR_exit("You can't use option '%s' twice!",argv[iarg]) ;
       if( ++iarg >= argc )
         ERROR_exit("Option '%s' needs an argument to follow!",argv[iarg-1]) ;
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
       iarg++ ;
     }

     ERROR_exit("Unknown option '%s' :-(",argv[iarg] ) ; exit(1) ;
   }

   exit(0) ;
}
