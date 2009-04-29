#include "mrilib.h"

#define MEAN     1
#define ZMEAN    2
#define QMEAN    3

/*----------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *xset=NULL ;
   int nopt=1 , do_automask=0 ;
   int nvox , nvals , ii,jj,kk , polort=1 , ntime ;
   float *xsar , *ysar ; float cc,csum,Mcsum,Zcsum,Qcsum , Tcount ;
   byte *mask=NULL ; int nxmask,nymask,nzmask , nmask , vstep=0 ;
   int nref=0 ; float **ref=NULL ;
   MRI_IMAGE *ortim=NULL ; float *ortar=NULL ;
   int *indx=NULL ; MRI_IMARR *timar=NULL ;
   char *Mprefix=NULL ; THD_3dim_dataset *Mset=NULL ; float *Mar=NULL ;
   char *Zprefix=NULL ; THD_3dim_dataset *Zset=NULL ; float *Zar=NULL ;
   char *Qprefix=NULL ; THD_3dim_dataset *Qset=NULL ; float *Qar=NULL ;
   char *Tprefix=NULL ; THD_3dim_dataset *Tset=NULL ; float *Tar=NULL ; float Thresh=0.0f ;
   int nout=0 ;
   int isodd ;  /* 29 Apr 2009: for unrolling innermost dot product */

   /*----*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTcorrMap [options]\n"
             "For each voxel, computes the correlation between it and all\n"
             "other voxels, and averages these into the output.  Supposed\n"
             "to give a measure of how 'connected' each voxel is to the\n"
             "rest of the brain.  (As if life were that simple.)\n"
             "\n"
             "Options:\n"
             "  -input dd = Read 3D+time dataset 'dd' (a mandatory option).\n"
             "\n"
             "  -Mean pp  = Save average correlations into dataset prefix 'pp'\n"
             "  -Zmean pp = Save tanh of mean arctanh(correlation) into 'pp'\n"
             "  -Qmean pp = Save RMS(correlation) into 'pp'\n"
             "  -Thresh tt pp\n"
             "            = Save the COUNT of how many voxels survived thresholding\n"
             "              at level abs(rho) >= tt.\n"
             "  [At least one of these output options must be given]\n"
             "\n"
             "  -polort m = Remove polynomical trend of order 'm', for m=-1..19.\n"
             "               [default is m=1; removal is by least squares].\n"
             "               Using m=-1 means no detrending; this is only useful\n"
             "               for data/information that has been pre-processed.\n"
             "  -ort rr   = 1D file with other time series to be removed\n"
             "               (via least squares regression) before correlation.\n"
             "\n"
             "  -mask mm  = Read dataset 'mm' as a voxel mask.\n"
             "  -automask = Create a mask from the input dataset.\n"
             "\n"
             "-- This purely experimental program is somewhat slow.\n"
             "-- For Kyle, AKA the new Pat.\n"
             "-- RWCox - August 2008.\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

#ifdef USING_MCW_MALLOC
   enable_mcw_malloc() ;
#endif
   mainENTRY("3dTcorrMap main"); machdep(); PRINT_VERSION("3dTcorrMap");
   AFNI_logger("3dTcorrMap",argc,argv);

   /*-- option processing --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcasecmp(argv[nopt],"-ort") == 0 ){
        ortim = mri_read_1D( argv[++nopt] ) ;
        if( ortim == NULL ) ERROR_exit("Can't read file after -ort") ;
        nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-automask") == 0 ){
         if( mask != NULL ) ERROR_exit("Can't use -automask and -mask!") ;
         do_automask = 1 ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-prefix") == 0 ){
         WARNING_message("-prefix option is converted to -Mean in 3dTcorrMap") ;
         Mprefix = argv[++nopt] ; nout++ ;
         if( !THD_filename_ok(Mprefix) ) ERROR_exit("Illegal string after -prefix!\n") ;
         nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-Mean") == 0 ){
         Mprefix = argv[++nopt] ; nout++ ;
         if( !THD_filename_ok(Mprefix) ) ERROR_exit("Illegal prefix after -Mean!\n") ;
         nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-Zmean") == 0 ){
         Zprefix = argv[++nopt] ; nout++ ;
         if( !THD_filename_ok(Zprefix) ) ERROR_exit("Illegal prefix after -Zmean!\n") ;
         nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-Qmean") == 0 ){
         Qprefix = argv[++nopt] ; nout++ ;
         if( !THD_filename_ok(Qprefix) ) ERROR_exit("Illegal prefix after -Qmean!\n") ;
         nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-Thresh") == 0 ){
         Thresh = (float)strtod(argv[++nopt],NULL) ;
         if( Thresh <= 0.0f || Thresh >= 0.99f ) ERROR_exit("Illegal -Thresh value %g",Thresh) ;
         Tprefix = argv[++nopt] ; nout++ ;
         if( !THD_filename_ok(Tprefix) ) ERROR_exit("Illegal prefix after -Thresh!\n") ;
         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-polort") == 0 ){
         char *cpt ;
         int val = (int)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < -1 || val > 19 )
            ERROR_exit("Illegal value after -polort!\n") ;
         polort = val ; nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-input") == 0 ){
        if( xset != NULL ) ERROR_exit("Can't use -input twice!") ;
        xset = THD_open_dataset(argv[++nopt]); CHECK_OPEN_ERROR(xset,argv[nopt]);
        nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-mask") == 0 ){
        THD_3dim_dataset *mset ;
        if( do_automask ) ERROR_exit("Can't use -automask and -mask") ;
        if( mask != NULL ) ERROR_exit("Can't use -mask twice") ;
        if( ++nopt >= argc ) ERROR_exit("Need argument after '-mask'") ;
        mset = THD_open_dataset( argv[nopt] ); CHECK_OPEN_ERROR(mset,argv[nopt]);
        DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
        nxmask = DSET_NX(mset); nymask = DSET_NY(mset); nzmask = DSET_NZ(mset);
        mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
        if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[nopt]) ;
        nmask = THD_countmask( nxmask*nymask*nzmask , mask ) ;
        INFO_message("Number of voxels in mask = %d",nmask) ;
        if( nmask < 9 ) ERROR_exit("Mask is too small to process") ;
        nopt++ ; continue ;
      }

      ERROR_exit("Unknown option: %s",argv[nopt]) ;
   }

   /*-- open dataset, check for legality --*/

   if( nout == 0 ) ERROR_exit("Don't you want any output datasets?") ;

   if( nopt >= argc && xset == NULL)
     ERROR_exit("No -input option and no dataset in final position?!") ;

   if( xset == NULL ){
     xset = THD_open_dataset(argv[nopt]); CHECK_OPEN_ERROR(xset,argv[nopt]);
   }
   ntime = DSET_NVALS(xset) ;
   if( ntime < 9 ) ERROR_exit("Input dataset is too short!") ;
   if( ortim != NULL && ortim->nx < ntime )
     ERROR_exit("-ort file is shorter than input dataset!") ;

   /*-- compute references, if any --*/

   if( polort >= 0 ){
     nref = polort + 1 ; ref = THD_build_polyref( nref , ntime ) ;
   }
   if( ortim != NULL ){
     jj = ortim->ny ; ortar = MRI_FLOAT_PTR(ortim) ;
     ref = (float **)realloc( ref , sizeof(float *)*(nref+jj) ) ;
     for( ii=0 ; ii < jj ; ii++ )
       ref[ii+nref] = ortar + (ii*ortim->nx) ;
     nref += jj ;
   }

   /*-- compute mask array, if desired --*/

   nvox = DSET_NVOX(xset) ;

   if( do_automask ){
      mask  = THD_automask( xset ) ;
      nmask = THD_countmask( nvox , mask ) ;
      if( nmask > 9 )
        INFO_message("%d voxels survive -automask",nmask) ;
      else
        ERROR_exit("only %d voxels survive -automask",nmask) ;
   } else if( mask != NULL ){
      if( nxmask != DSET_NX(xset) ||
          nymask != DSET_NY(xset) || nzmask != DSET_NZ(xset) )
        ERROR_exit("-mask and -input datasets differ in voxel grids!") ;
   } else {
      nmask = nvox ;
      mask  = (byte *)malloc(sizeof(byte)*nmask) ;
      memset( mask  , 1 , sizeof(byte)*nmask ) ;
      INFO_message("computing for all %d voxels!",nmask) ;
   }

   /*-- create output datasets --*/

   if( Mprefix != NULL ){
     Mset = EDIT_empty_copy( xset ) ;
     EDIT_dset_items( Mset ,
                        ADN_prefix    , Mprefix        ,
                        ADN_nvals     , 1              ,
                        ADN_ntt       , 0              ,
                        ADN_brick_fac , NULL           ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                      ADN_none ) ;
     EDIT_substitute_brick( Mset , 0 , MRI_float , NULL ) ;
     Mar = DSET_ARRAY(Mset,0) ;  /* get array  */
     EDIT_BRICK_TO_NOSTAT(Mset,0) ;
     if( THD_deathcon() && THD_is_file(DSET_HEADNAME(Mset)) )
       ERROR_exit("Output dataset %s already exists!",
                  DSET_HEADNAME(Mset)) ;
     tross_Make_History( "3dTcorrMap" , argc,argv , Mset ) ;
   }

   if( Zprefix != NULL ){
     Zset = EDIT_empty_copy( xset ) ;
     EDIT_dset_items( Zset ,
                        ADN_prefix    , Zprefix        ,
                        ADN_nvals     , 1              ,
                        ADN_ntt       , 0              ,
                        ADN_brick_fac , NULL           ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                      ADN_none ) ;
     EDIT_substitute_brick( Zset , 0 , MRI_float , NULL ) ;
     Zar = DSET_ARRAY(Zset,0) ;  /* get array  */
     EDIT_BRICK_TO_NOSTAT(Zset,0) ;
     if( THD_deathcon() && THD_is_file(DSET_HEADNAME(Zset)) )
       ERROR_exit("Output dataset %s already exists!",
                  DSET_HEADNAME(Zset)) ;
     tross_Make_History( "3dTcorrMap" , argc,argv , Zset ) ;
   }

   if( Qprefix != NULL ){
     Qset = EDIT_empty_copy( xset ) ;
     EDIT_dset_items( Qset ,
                        ADN_prefix    , Qprefix        ,
                        ADN_nvals     , 1              ,
                        ADN_ntt       , 0              ,
                        ADN_brick_fac , NULL           ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                      ADN_none ) ;
     EDIT_substitute_brick( Qset , 0 , MRI_float , NULL ) ;
     Qar = DSET_ARRAY(Qset,0) ;  /* get array  */
     EDIT_BRICK_TO_NOSTAT(Qset,0) ;
     if( THD_deathcon() && THD_is_file(DSET_HEADNAME(Qset)) )
       ERROR_exit("Output dataset %s already exists!",
                  DSET_HEADNAME(Qset)) ;
     tross_Make_History( "3dTcorrMap" , argc,argv , Qset ) ;
   }

   if( Tprefix != NULL ){
     Tset = EDIT_empty_copy( xset ) ;
     EDIT_dset_items( Tset ,
                        ADN_prefix    , Tprefix        ,
                        ADN_nvals     , 1              ,
                        ADN_ntt       , 0              ,
                        ADN_brick_fac , NULL           ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                      ADN_none ) ;
     EDIT_substitute_brick( Tset , 0 , MRI_float , NULL ) ;
     Tar = DSET_ARRAY(Tset,0) ;  /* get array  */
     EDIT_BRICK_TO_NOSTAT(Tset,0) ;
     if( THD_deathcon() && THD_is_file(DSET_HEADNAME(Tset)) )
       ERROR_exit("Output dataset %s already exists!",
                  DSET_HEADNAME(Tset)) ;
     tross_Make_History( "3dTcorrMap" , argc,argv , Tset ) ;
   }

   /*--- load input data and pre-process it ---*/

   INFO_message("Loading input dataset") ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   /* remove trends now */

   if( nref > 0 ){
     THD_3dim_dataset *yset ;
     ININFO_message("Detrending input dataset with %d references",nref) ;
     yset = THD_detrend_dataset( xset , nref,ref , 2,0 , mask , NULL ) ;
     if( yset == NULL ) ERROR_exit("Detrending fails!?") ;
     DSET_delete(xset) ; xset = yset ;
   }

   /* check for all zero voxels, and remove them from the mask */

   for( ii=0 ; ii < nvox ; ii++ ){
     xsar = (float *)malloc( sizeof(float)*ntime ) ;
     if( mask[ii] == 0 ) continue ;
     (void)THD_extract_array( ii , xset , 0 , xsar ) ;
     for( kk=0 ; kk < ntime && xsar[kk] == 0.0f ; kk++ ) ; /*nada */
     if( kk == ntime ) mask[ii] = 1 ; /* xsar is all 0 */
     free(xsar) ;
   }
   ii = THD_countmask( nvox , mask ) ;
   if( ii < nmask ){
     if( ii > 9 ) ININFO_message("only %d voxels in dataset are actually non-zero"  ,ii);
     else         ERROR_exit    ("only %d voxels in dataset are actually non-zero!?",ii);
     nmask = ii ;
   }

   /* extract dataset time series into an array of
      time series vectors, for ease and speed of access */

   indx = (int *)malloc(sizeof(int)*nmask) ;
   for( ii=jj=0 ; ii < nvox ; ii++ ) if( mask[ii] ) indx[jj++] = ii ;
   ININFO_message("extracting mask-ed time series") ;
   timar = THD_extract_many_series( nmask , indx , xset ) ;
   if( timar == NULL ) ERROR_exit("extraction failed!?") ;
   DSET_delete(xset) ; free(mask) ;

   /* normalize so sum of squares is 1, for speed in computing correlations */

   ININFO_message("normalizing extracted time series") ;
   for( ii=0 ; ii < nmask ; ii++ ){
     xsar = MRI_FLOAT_PTR( IMARR_SUBIM(timar,ii) ) ;
     csum = 0.0f ;
     for( jj=0 ; jj < ntime ; jj++ ) csum += xsar[jj]*xsar[jj] ;
     if( csum > 0.0f ){  /* should always be true */
       csum = 1.0f / sqrtf(csum) ;
       for( jj=0 ; jj < ntime ; jj++ ) xsar[jj] *= csum ;
     }
   }

   /*--- loop over voxels, correlate (lots of CPU time now) ---*/

   vstep = (nmask > 999) ? nmask/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ Voxel loop: ") ;

   isodd = (ntime%2 == 1) ;

   for( ii=0 ; ii < nmask ; ii++ ){  /* time series to correlate with */

     if( vstep && ii%vstep==vstep-1 ) vstep_print() ;
     xsar = MRI_FLOAT_PTR( IMARR_SUBIM(timar,ii) ) ;

     Tcount = Mcsum = Zcsum = Qcsum = 0.0f ;
     for( jj=0 ; jj < nmask ; jj++ ){  /* loop over other voxels, correlate w/ii */

       if( jj==ii ) continue ;
       ysar = MRI_FLOAT_PTR( IMARR_SUBIM(timar,jj) ) ;

       /** dot products (unrolled by 2 on 29 Apr 2009) **/

       if( isodd ){
         for( cc=xsar[0]*ysar[0],kk=1 ; kk < ntime ; kk+=2 )
           cc += xsar[kk]*ysar[kk] + xsar[kk+1]*ysar[kk+1] ;
       } else {
         for( cc=0.0f,kk=0 ; kk < ntime ; kk+=2 )
           cc += xsar[kk]*ysar[kk] + xsar[kk+1]*ysar[kk+1] ;
       }

       Mcsum += cc ;
       Zcsum += 0.5f * logf((1.0001f+cc)/(1.0001f-cc));
       Qcsum += cc*cc ;
       if( fabsf(cc) >= Thresh ) Tcount++ ;
     }
     if( Mar != NULL ) Mar[indx[ii]] = Mcsum / (nmask-1.0f) ;
     if( Zar != NULL ) Zar[indx[ii]] = tanh( Zcsum / (nmask-1.0f) ) ;
     if( Qar != NULL ) Qar[indx[ii]] = sqrt( Qcsum / (nmask-1.0f) ) ;
     if( Tar != NULL ) Tar[indx[ii]] = Tcount ;

   }
   if( vstep ) fprintf(stderr,"!\n") ;

   /*--- finito ---*/

   free(indx) ; DESTROY_IMARR(timar) ;

   if( Mset != NULL ){
     DSET_write(Mset) ; WROTE_DSET(Mset) ; DSET_delete(Mset) ;
   }
   if( Zset != NULL ){
     DSET_write(Zset) ; WROTE_DSET(Zset) ; DSET_delete(Zset) ;
   }
   if( Qset != NULL ){
     DSET_write(Qset) ; WROTE_DSET(Qset) ; DSET_delete(Qset) ;
   }
   if( Tset != NULL ){
     DSET_write(Tset) ; WROTE_DSET(Tset) ; DSET_delete(Tset) ;
   }

   INFO_message("total CPU time = %.2f s",COX_cpu_time()) ; exit(0) ;
}
