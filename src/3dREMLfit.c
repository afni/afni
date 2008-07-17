#include "mrilib.h"

#undef FLOATIZE      /* we will use double precision for matrices */
#include "remla.c"   /* do NOT change this to FLOATIZE !!! */

#undef  INMASK
#define INMASK(i) ( mask == NULL || mask[i] != 0 )

static void vstep_print(void)
{
   static char xx[10] = "0123456789" ; static int vn=0 ;
   fprintf(stderr , "%c" , xx[vn%10] ) ;
   if( vn%10 == 9) fprintf(stderr,".") ;
   vn++ ;
}

/*--------------------------------------------------------------------------*/

static int    Argc ;
static char **Argv ;

THD_3dim_dataset * create_float_dataset( THD_3dim_dataset *tset ,
                                         int nvol , char *prefix )
{
   THD_3dim_dataset *nset ; int kk ;

   if( tset == NULL || nvol < 1 || prefix == NULL ) return NULL ;

   nset = EDIT_empty_copy( tset ) ;
   EDIT_dset_items( nset ,
                      ADN_prefix    , prefix ,
                      ADN_brick_fac , NULL   ,
                      ADN_nvals     , nvol   ,
                      ADN_ntt       , nvol   ,
                    ADN_none ) ;
  tross_Copy_History( tset , nset ) ;
  tross_Make_History( "3dREMLfit" , Argc,Argv , nset ) ;
  for( kk=0 ; kk < nvol ; kk++ )
     EDIT_substitute_brick( nset , kk , MRI_float , NULL ) ;
  return nset ;
}

/*--------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset=NULL ;
   byte *mask=NULL ; int mask_nx,mask_ny,mask_nz , automask=0 ;
   float *iv ;
   int iarg, ii,jj, nreg, ntime, *tau=NULL, rnum, nfull, nvals,nvox,vv ;
   NI_element *nelmat=NULL ; char *matname=NULL ;
   MTYPE rhomax=0.7 , bmax=0.7 ; int rhonum=7 , bnum=14 ;
   char *cgl , *rst ;
   matrix X ; vector y ;
   float cput ;
   reml_collection *rrcol ;
   int nprefix=0 , vstep ;
   char *Rbeta_prefix  = NULL ; THD_3dim_dataset *Rbeta_dset  = NULL ;
   char *Rvar_prefix   = NULL ; THD_3dim_dataset *Rvar_dset   = NULL ;
   char *Rfitts_prefix = NULL ; THD_3dim_dataset *Rfitts_dset = NULL ;
   char *Obeta_prefix  = NULL ; THD_3dim_dataset *Obeta_dset  = NULL ;
   char *Ovar_prefix   = NULL ; THD_3dim_dataset *Ovar_dset   = NULL ;
   char *Ofitts_prefix = NULL ; THD_3dim_dataset *Ofitts_dset = NULL ;
   int Ngoodlist,*goodlist=NULL , Nruns,*runs=NULL ;
   NI_int_array *giar ;

   /**------- help? -------**/

   Argc = argc ; Argv = argv ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 3dREMLfit [option]\n"
      "Least squares fit with REML estimation of the ARMA(1,1) noise.\n"
      "\n"
      "Input Options (the first two are mandatory)\n"
      "-------------------------------------------\n"
      " -input ddd  = Read time series dataset 'ddd'.\n"
      " -matrix mmm = Read the matrix 'mmm', which should have been\n"
      "                 output from 3dDeconvolve via the '-x1D' option.\n"
      " -mask kkk   = Read dataset 'kkk' as a mask for the input.\n"
      " -automask   = What do you think?\n"
      "\n"
      "Output Options (at least one must be given)\n"
      "-------------------------------------------\n"
      " -Rbeta_prefix  = dataset for beta weights from the REML estimation\n"
      " -Rvar_prefix   = dataset for REML variance parameters\n"
#if 0
      " -Rfitts_prefix = dataset for REML fitted model\n"
#endif
      "\n"
      " -Obeta_prefix  = dataset for beta weights from the OLSQ estimation\n"
      " -Ovar_prefix   = dataset for OLSQ variance parameters\n"
#if 0
      " -Ofitts_prefix = dataset for OLSQ fitted model\n"
#endif
      "\n"
      "The following options control the ARMA(1,1)\n"
      "parameter estimation for each voxel time series\n"
      "-----------------------------------------------\n"
      " -MAXrho rm = Set the max allowed rho parameter to 'rm' (default=0.7).\n"
      " -Nrho nr   = Use 'nr' values for the rho parameter (default=7).\n"
      " -MAXb bm   = Set max allow MA b parameter to 'bm' (default=0.7).\n"
      " -Nb nb     = Use 'nb' values for the b parameter (default=14).\n"
      "\n"
      "Notes\n"
      "-----\n"
      "* ARMA(1,1) parameters 'a' (AR) and 'b' (MA) are estimated\n"
      "    only on a discrete grid, for the sake of CPU time.\n"
      "* Each voxel gets a separate pair of 'a' and 'b' parameters.\n"
      "* OLSQ = Ordinary Least Squares; these outputs can be used to\n"
      "         compare the REML estimations with the simpler OLSQ results.\n"
      "* All output datasets are in float format.  Calculations internally\n"
      "    are done in double precision.\n"
      "* Despite my best efforts, this program is somewhat slow.\n"
      "\n"
      "-- RWCox - July 2008\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*------- official startup ------*/

   PRINT_VERSION("3dREMLfit"); mainENTRY("3dREMLfit main"); machdep();
   AFNI_logger("3dREMLfit",argc,argv); AUTHOR("RWCox");

   /**------- scan command line --------**/

   iarg = 1 ;
   while( iarg < argc ){

      /** ARMA params **/

     if( strcmp(argv[iarg],"-MAXrho") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       rhomax = (MTYPE)strtod(argv[iarg],NULL) ;
            if( rhomax < 0.3 ) rhomax = 0.3 ;
       else if( rhomax > 0.9 ) rhomax = 0.9 ;
       iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-Nrho") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       rhonum = (int)strtod(argv[iarg],NULL) ;
            if( rhonum <  2 ) rhonum =  2 ;
       else if( rhonum > 20 ) rhonum = 20 ;
       iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-MAXb") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       bmax = (MTYPE)strtod(argv[iarg],NULL) ;
            if( bmax < 0.3 ) bmax = 0.3 ;
       else if( bmax > 0.9 ) bmax = 0.9 ;
       iarg++ ; continue ;
     }
     if( strcmp(argv[iarg],"-Nb") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       bnum = (int)strtod(argv[iarg],NULL) ;
            if( bnum <  2 ) bnum =  2 ;
       else if( bnum > 20 ) bnum = 20 ;
       iarg++ ; continue ;
     }

      /** -matrix **/

     if( strcmp(argv[iarg],"-matrix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       if( nelmat != NULL ) ERROR_exit("More than 1 -matrix option!?");
       nelmat = NI_read_element_fromfile( argv[iarg] ) ; /* read NIML file */
       matname = argv[iarg];
       if( nelmat == NULL || nelmat->type != NI_ELEMENT_TYPE )
         ERROR_exit("Can't process -matrix file!?");
       iarg++ ; continue ;
     }

      /** -input **/

     if( strcmp(argv[iarg],"-input") == 0 ){
       if( inset != NULL  ) ERROR_exit("Can't have two -input options!?") ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       inset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(inset,argv[iarg]) ;
       iarg++ ; continue ;
     }

     /** -mask **/

     if( strcmp(argv[iarg],"-mask") == 0 ){
       THD_3dim_dataset *mset ; int mmm ;
       if( ++iarg >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[iarg] ) ;
       CHECK_OPEN_ERROR(mset,argv[iarg]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[iarg]) ;
       mmm = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       INFO_message("Number of voxels in mask = %d",mmm) ;
       if( mmm < 2 ) ERROR_exit("Mask is too small to process") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't have -automask and -mask") ;
       automask = 1 ; iarg++ ; continue ;
     }

     /** prefix options */

     if( strcmp(argv[iarg],"-Rbeta_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Rbeta_prefix = strdup(argv[iarg]) ; nprefix++ ;
       if( !THD_filename_ok(Rbeta_prefix) )
         ERROR_exit("Illegal string after -Rbeta_prefix") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Rvar_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Rvar_prefix = strdup(argv[iarg]) ; nprefix++ ;
       if( !THD_filename_ok(Rvar_prefix) )
         ERROR_exit("Illegal string after -Rvar_prefix") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Rfitts_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Rfitts_prefix = strdup(argv[iarg]) ; nprefix++ ;
       if( !THD_filename_ok(Rfitts_prefix) )
         ERROR_exit("Illegal string after -Rfitts_prefix") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Obeta_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Obeta_prefix = strdup(argv[iarg]) ; nprefix++ ;
       if( !THD_filename_ok(Obeta_prefix) )
         ERROR_exit("Illegal string after -Obeta_prefix") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Ovar_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Ovar_prefix = strdup(argv[iarg]) ; nprefix++ ;
       if( !THD_filename_ok(Ovar_prefix) )
         ERROR_exit("Illegal string after -Ovar_prefix") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-Ofitts_prefix") == 0 ){
       if( ++iarg >= argc ) ERROR_exit("Need argument after '%s'",argv[iarg-1]) ;
       Ofitts_prefix = strdup(argv[iarg]) ; nprefix++ ;
       if( !THD_filename_ok(Ofitts_prefix) )
         ERROR_exit("Illegal string after -Ofitts_prefix") ;
       iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   /**-------- sanity checks, dataset input, maskifying --------**/

   if( inset   == NULL ) ERROR_exit("No -input dataset?!") ;
   if( nprefix == 0    ) ERROR_exit("No output prefixes given?!") ;

   INFO_message("Loading input dataset into memory") ;
   DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
   nvals = DSET_NVALS(inset) ; nvox = DSET_NVOX(inset) ;

   if( mask != NULL ){
     if( mask_nx != DSET_NX(inset) ||
         mask_ny != DSET_NY(inset) ||
         mask_nz != DSET_NZ(inset)   )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     int mmm ;
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     mmm = THD_countmask( DSET_NVOX(inset) , mask ) ;
     INFO_message("Number of voxels in automask = %d",mmm) ;
     if( mmm < 2 ) ERROR_exit("Automask is too small to process") ;
   }

   /**-------- process the matrix --------**/

   nreg  = nelmat->vec_num ;  /* number of matrix columns */
   ntime = nelmat->vec_len ;  /* number of matrix rows */

   /* number of rows in the full matrix */

   cgl = NI_get_attribute( nelmat , "NRowFull" ) ;
   if( cgl == NULL ) ERROR_exit("Matrix is missing 'NRowFull' attribute!") ;
   nfull = (int)strtod(cgl,NULL) ;
   if( nvals != nfull )
     ERROR_exit("Dataset has %d time points, but matrix indicates %d",
                nvals , nfull ) ;

   /* the goodlist = mapping from  matrix row index to time index */

   cgl = NI_get_attribute( nelmat , "GoodList" ) ;
   if( cgl == NULL ) ERROR_exit("Matrix is missing 'GoodList' attribute!") ;
   giar = NI_decode_int_list( cgl , ";," ) ;
   if( giar == NULL || giar->num < ntime )
     ERROR_exit("Matrix 'GoodList' badly formatted?!") ;
   Ngoodlist = giar->num ; goodlist = giar->ar ;
   if( Ngoodlist != ntime )
     ERROR_exit("Matrix 'GoodList' incorrect length?!") ;

   /* run starting points in time index */

   rst = NI_get_attribute( nelmat , "RunStart" ) ;
   if( rst != NULL ){
     NI_int_array *riar = NI_decode_int_list( rst , ";,") ;
     if( riar == NULL ) ERROR_exit("-matrix 'RunStart' badly formatted?") ;
     Nruns = riar->num ; runs = riar->ar ;
   } else {
     INFO_message("Matrix missing 'RunStart' attribute ==> assuming 1 run");
     Nruns = 1 ; runs = calloc(sizeof(int),1) ;
   }

   /* set up pseudo-time tau[] vector for R matrix formation */

   rnum = 0 ; tau = (int *)malloc(sizeof(int)*ntime) ;
   for( ii=0 ; ii < ntime ; ii++ ){
     jj = goodlist[ii] ;        /* time index of the ii-th matrix row */
                              /* then find which run this point is in */
     for( ; rnum+1 < Nruns && jj >= runs[rnum+1] ; rnum++ ) ;   /*nada*/
     tau[ii] = jj + 10000*rnum ;  /* the 10000 means 'very far apart' */
   }

   /* re-create the regression matrix X */

   matrix_initialize( &X ) ;
   matrix_create( ntime , nreg , &X ) ;
   if( nelmat->vec_typ[0] == NI_FLOAT ){
     float *cd ;
     for( jj=0 ; jj < nreg ; jj++ ){
       cd = (float *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) X.elts[ii][jj] = (MTYPE)cd[ii] ;
     }
   } else if( nelmat->vec_typ[0] == NI_DOUBLE ){
     double *cd ;
     for( jj=0 ; jj < nreg ; jj++ ){
       cd = (double *)nelmat->vec[jj] ;
       for( ii=0 ; ii < ntime ; ii++ ) X.elts[ii][jj] = (MTYPE)cd[ii] ;
     }
   } else {
     ERROR_exit("-matrix file stored will illegal data type!?") ;
   }

   /**------- set up for REML estimation -------**/

   cput = COX_cpu_time() ;
   INFO_message("starting REML setup calculations: CPU=%.2f",cput) ;
   rrcol = REML_setup( &X , tau , rhonum,rhomax,bnum,bmax ) ;
   if( rrcol == NULL ) ERROR_exit("REML setup fails?!" ) ;
   cput = COX_cpu_time() - cput ;
   INFO_message("REML setup: rows=%d cols=%d %d cases; net CPU=%.2f",
                ntime,nreg,rrcol->nset,cput) ;

   /*----- create output datasets -----*/

   Rbeta_dset = create_float_dataset( inset , nreg , Rbeta_prefix ) ;
   Rvar_dset  = create_float_dataset( inset , 4    , Rvar_prefix  ) ;
#if 0
   Rfitts_dset= create_float_dataset( inset , nfull, Rfitts_prefix) ;
#endif

   Obeta_dset = create_float_dataset( inset , nreg , Obeta_prefix ) ;
   Ovar_dset  = create_float_dataset( inset , 1    , Ovar_prefix  ) ;
#if 0
   Ofitts_dset= create_float_dataset( inset , nfull, Ofitts_prefix) ;
#endif

   /***------- loop over voxels and process them ------***/

   cput = COX_cpu_time() ;
   vector_initialize( &y ) ; vector_create_noinit( ntime , &y ) ;
   iv = (float *)malloc(sizeof(float)*nvals) ;
   vstep = (nvox > 999) ? nvox/50 : 0 ;
   if( vstep ) fprintf(stderr,"++ voxel loop: ") ;

   for( vv=0 ; vv < nvox ; vv++ ){
     if( vstep && vv%vstep==vstep-1 ) vstep_print() ;
     if( !INMASK(vv) ) continue ;
     (void)THD_extract_array( vv , inset , 0 , iv ) ;
     for( ii=0 ; ii < ntime ; ii++ ) y.elts[ii] = (MTYPE)iv[goodlist[ii]] ;
     (void)REML_find_best_case( &y , rrcol ) ;

     if( Rbeta_dset != NULL ){
       for( ii=0 ; ii < nreg ; ii++ ) iv[ii] = REML_best_beta_vector.elts[ii] ;
       THD_insert_series( vv , Rbeta_dset , nreg , MRI_float , iv , 0 ) ;
     }
     if( Rvar_dset != NULL ){
       iv[0] = sqrt( REML_best_ssq / (ntime-nreg) ) ;
       iv[1] = REML_best_rho ; iv[2] = REML_best_bb ; iv[3] = REML_best_lam ;
       THD_insert_series( vv , Rvar_dset , 4 , MRI_float , iv , 0 ) ;
     }
     if( Obeta_dset != NULL ){
       for( ii=0 ; ii < nreg ; ii++ ) iv[ii] = OLSQ_best_beta_vector.elts[ii] ;
       THD_insert_series( vv , Obeta_dset , nreg , MRI_float , iv , 0 ) ;
     }
     if( Ovar_dset != NULL ){
       iv[0] = sqrt( OLSQ_best_ssq / (ntime-nreg) ) ;
       THD_insert_series( vv , Ovar_dset , 1 , MRI_float , iv , 0 ) ;
     }

   }
   if( vstep ) fprintf(stderr,"\n") ;
   cput = COX_cpu_time() - cput ;
   INFO_message("REML fitting: net CPU=%.2f",cput) ;

   if( Rbeta_dset != NULL ){ DSET_write(Rbeta_dset); WROTE_DSET(Rbeta_dset); }
   if( Rvar_dset  != NULL ){ DSET_write(Rvar_dset) ; WROTE_DSET(Rvar_dset ); }
   if( Obeta_dset != NULL ){ DSET_write(Obeta_dset); WROTE_DSET(Obeta_dset); }
   if( Ovar_dset  != NULL ){ DSET_write(Ovar_dset) ; WROTE_DSET(Ovar_dset ); }

   exit(0) ;
}
