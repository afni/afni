#include "mrilib.h"

#define MEAN     1
#define ZMEAN    2
#define QMEAN    3

#ifdef USE_OMP
#include <omp.h>
#endif

#include "parser.h"

/*----------------------------------------------------------------------------*/
static float etime (struct  timeval  *t, int Report  )
{/*etime*/
   struct  timeval  tn;
   float Time_Fact = 1000000.0;
   float delta_t;

   /* get time */
   gettimeofday(&tn, NULL);

   if (Report)
      {
         delta_t = ( ( (float)(tn.tv_sec - t->tv_sec)*Time_Fact) +
                       (float)(tn.tv_usec - t->tv_usec) ) /Time_Fact;
      }
   else
      {
         t->tv_sec = tn.tv_sec;
         t->tv_usec = tn.tv_usec;
         delta_t = 0.0;
      }

   return (delta_t);
}

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
   float *xsar ; float acc,cc,csum,Mcsum,Zcsum,Qcsum , Tcount ;
   byte *mask=NULL ; int nxmask=0,nymask=0,nzmask=0 , nmask=0 , vstep=0 ;
   int nref=0 , iv=0, N_iv=0, Tbone=0;
   float **ref=NULL, t0=0.0, t1=0.0, ti=0.0;
   MRI_IMAGE *ortim=NULL ; float *ortar=NULL ;
   int *indx=NULL ; MRI_IMARR *timar=NULL ;
   char *Mprefix=NULL ; THD_3dim_dataset *Mset=NULL ; float *Mar=NULL ;
   char *Zprefix=NULL ; THD_3dim_dataset *Zset=NULL ; float *Zar=NULL ;
   char *Qprefix=NULL ; THD_3dim_dataset *Qset=NULL ; float *Qar=NULL ;
   char *Tprefix=NULL ; THD_3dim_dataset *Tset=NULL ; float *Tar=NULL ;
   char *Pprefix=NULL ; THD_3dim_dataset *Pset=NULL ; float *Par=NULL ;
      float Thresh=0.0f ;
   char *Tvprefix=NULL ; THD_3dim_dataset *Tvset=NULL ;
      float **Tvar=NULL ; float  *Threshv=NULL, *Tvcount=NULL ;
   char stmp[256];
   int nout=0 ;
   int isodd ;  /* 29 Apr 2009: for unrolling innermost dot product */
   struct  timeval  tt;
   float dtt=0.0;
   float *ccar=NULL ; /* 29 Apr 2009: for OpenMP usage */

   float Pcsum ; int nPcsum ;  /* 23 Jun 2009 */

   char        *expr_string=NULL , expr_type='\0' ;  /* 23 Jun 2009 */
   PARSER_code *expr_code=NULL ;
   char *Eprefix=NULL ; THD_3dim_dataset *Eset=NULL ; float *Ear=NULL ;
   double *atoz[26] ;
   double *eear=NULL ; float Esum ; int nEsum ;

   /*----*/

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ){
      printf(
       "Usage: 3dTcorrMap [options]\n"
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
       "  -Pmean pp = Save average of squared positive correlations into 'pp'\n"
       "              (negative correlations don't count in this calculation)\n"
       "  -Thresh tt pp\n"
       "            = Save the COUNT of how many voxels survived thresholding\n"
       "              at level abs(correlation) >= tt.\n"
       "\n"
       "  -VarThresh t0 t1 dt pp\n"
       "            = Save the COUNT of how many voxels survive thresholding\n"
       "              at several levels abs(correlation) >= tt, for\n"
       "              tt = t0, t0+dt, ..., t1.  This option produces\n"
       "              a multi-volume dataset, with prefix 'pp'.\n"
       "  -VarThreshN t0 t1 dt pp\n"
       "            = Like '-VarThresh', but the output counts are\n"
       "              'Normalized' (divided) by the expected number\n"
       "              of such supra-threshold voxels that would occur\n"
       "              from white noise timeseries.\n"
       "           ** N.B.: You can't use '-VarThresh' and '-VarThreshN'\n"
       "                    in the same run of the program!\n"
       "\n"
       "  -Aexpr expr ppp\n"
       "            = For each correlation 'r', compute the calc-style\n"
       "              expression 'expr', and average these values to get\n"
       "              the output that goes into dataset 'ppp'.\n"
       "  -Cexpr expr ppp\n"
       "            = As in '-Aexpr', but only average together nonzero\n"
       "              values computed by 'expr'.  Example:\n"
       "                -Cexpr 'step(r-0.3)*r' TCa03\n"
       "              would compute (for each voxel) the average of all\n"
       "              correlation coefficients larger than 0.3.\n"
       "  -Sexpr expr ppp\n"
       "            = As above, but the sum of the expressions is computed\n"
       "              rather than the average.  Example:\n"
       "                -Sexpr 'step(r-0.3)' TCn03\n"
       "              would compute the number of voxels with correlation\n"
       "              coefficients larger than 0.3.\n"
       "           ** N.B.: At most one '-?expr' option can be used in\n"
       "                    the same run of the program!\n"
       "           ** N.B.: Only the symbol 'r' has any meaning in the\n"
       "                    expression; all other symbols will be treated\n"
       "                    as zeroes.\n"
       "\n"
       "  *** At least one of the above output options must be given!!! ***\n"
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
       "-- For Kyle, AKA the new Pat (if such a thing were possible).\n"
       "-- RWCox - August 2008.\n"
            ) ;

      PRINT_AFNI_OMP_USAGE("3dTcorrMap",NULL) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

#if defined(USING_MCW_MALLOC) && !defined(USE_OMP)
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
      if( strcasecmp(argv[nopt],"-Pmean") == 0 ){
         Pprefix = argv[++nopt] ; nout++ ;
         if( !THD_filename_ok(Pprefix) ) ERROR_exit("Illegal prefix after -Pmean!\n") ;
         nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-Thresh") == 0 ){
         Thresh = (float)strtod(argv[++nopt],NULL) ;
         if( Thresh <= 0.0f || Thresh >= 0.99f )
           ERROR_exit("Illegal -Thresh value %g",Thresh) ;
         Tprefix = argv[++nopt] ; nout++ ;
         if( !THD_filename_ok(Tprefix) )
           ERROR_exit("Illegal prefix after -Thresh!\n") ;
         nopt++ ; continue ;
      }
      if( strcasecmp(argv[nopt],"-VarThresh") == 0 ||
          strcasecmp(argv[nopt],"-VarThreshN") == 0 ){

         if (nopt+4 >= argc) {
            ERROR_exit("Need three values and a prefix after -VarThresh*");
         }

         Tbone = !strcasecmp(argv[nopt],"-VarThreshN");

         t0 = (float)strtod(argv[++nopt],NULL) ;
         t1 = (float)strtod(argv[++nopt],NULL) ;
         ti = (float)strtod(argv[++nopt],NULL) ;
         N_iv = (int)((t1-t0)/ti)+1;
         if( t0 <= 0.0f || t0 >= 0.99f )
            ERROR_exit("Illegal 1st value of %g after -VarThresh* ",t0) ;
         if( t1 <= 0.0f || t1 >= 0.99f )
            ERROR_exit("Illegal 2nd value of %g after -VarThresh* ",t1) ;
         if( ti <= 0.0f || ti >= 0.99f )
            ERROR_exit("Illegal 3rd value of %g after -VarThresh* ",ti) ;
         if (N_iv <= 0)
            ERROR_exit("Bad combination of values after -VarThresh* ") ;

         Threshv = (float *)calloc(N_iv+1, sizeof(float));
         for (iv=0; iv < N_iv; ++iv)
           Threshv[iv] = t0 + (float)iv*ti;

         Tvprefix = argv[++nopt] ; nout++ ;
         if( !THD_filename_ok(Tvprefix) )
            ERROR_exit("Illegal prefix after %s",argv[nopt-4]) ;
         if( Tvprefix[0] == '-' )
            WARNING_message("%s prefix '%s' starts with '-' :: is this a mistake?",
                           argv[nopt-4] , Tvprefix ) ;

         INFO_message("VarThresh mode with %d levels: %.3f .. %.3f\n",
                      N_iv , Threshv[0] , Threshv[N_iv-1] ) ;

         nopt++ ; continue ;
      }

      if( strcasecmp(argv[nopt],"-Aexpr") == 0 ||
          strcasecmp(argv[nopt],"-Cexpr") == 0 ||
          strcasecmp(argv[nopt],"-Sexpr") == 0   ){

        if( expr_type != '\0' ) ERROR_exit("Can't have 2 'expr' options!") ;
        expr_type   = argv[nopt][1] ; nopt++ ;
        expr_string = strdup(argv[nopt]) ;
        expr_code   = PARSER_generate_code( expr_string ) ;
        if( expr_code == NULL )
          ERROR_exit("Illegal expression in option '%s'",argv[nopt-1]) ;
        if( !PARSER_has_symbol("r",expr_code) )
          ERROR_exit("Expression doesn't contain 'r' symbol!") ;
        Eprefix = argv[++nopt] ; nout++ ;
        if( !THD_filename_ok(Eprefix) )
          ERROR_exit("Illegal prefix after '%s'",argv[nopt-2]) ;
        for( ii=0 ; ii < 26 ; ii++ ) atoz[ii] = NULL ;
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
   if( ntime < 9 )
     ERROR_exit("Input dataset '%s' is too short: %d time points" ,
                DSET_HEADNAME(xset) , ntime ) ;
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

   if( Pprefix != NULL ){
     Pset = EDIT_empty_copy( xset ) ;
     EDIT_dset_items( Pset ,
                        ADN_prefix    , Pprefix        ,
                        ADN_nvals     , 1              ,
                        ADN_ntt       , 0              ,
                        ADN_brick_fac , NULL           ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                      ADN_none ) ;
     EDIT_substitute_brick( Pset , 0 , MRI_float , NULL ) ;
     Par = DSET_ARRAY(Pset,0) ;  /* get array  */
     EDIT_BRICK_TO_NOSTAT(Pset,0) ;
     if( THD_deathcon() && THD_is_file(DSET_HEADNAME(Pset)) )
       ERROR_exit("Output dataset %s already exists!",
                  DSET_HEADNAME(Pset)) ;
     tross_Make_History( "3dTcorrMap" , argc,argv , Pset ) ;
   }

   if( Eprefix != NULL ){
     Eset = EDIT_empty_copy( xset ) ;
     EDIT_dset_items( Eset ,
                        ADN_prefix    , Eprefix        ,
                        ADN_nvals     , 1              ,
                        ADN_ntt       , 0              ,
                        ADN_brick_fac , NULL           ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                      ADN_none ) ;
     EDIT_substitute_brick( Eset , 0 , MRI_float , NULL ) ;
     Ear = DSET_ARRAY(Eset,0) ;  /* get array  */
     EDIT_BRICK_TO_NOSTAT(Eset,0) ;
     if( THD_deathcon() && THD_is_file(DSET_HEADNAME(Eset)) )
       ERROR_exit("Output dataset %s already exists!",
                  DSET_HEADNAME(Eset)) ;
     tross_Make_History( "3dTcorrMap" , argc,argv , Eset ) ;
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

   if( Tvprefix != NULL ){
     Tvset = EDIT_empty_copy( xset ) ;
     EDIT_dset_items( Tvset ,
                        ADN_prefix    , Tvprefix       ,
                        ADN_nvals     , N_iv           ,
                        ADN_ntt       , 0              ,
                        ADN_brick_fac , NULL           ,
                        ADN_type      , HEAD_FUNC_TYPE ,
                        ADN_func_type , FUNC_BUCK_TYPE ,
                      ADN_none ) ;
     Tvar    = (float **)calloc(N_iv,sizeof(float*));
     Tvcount = (float * )calloc(N_iv,sizeof(float ));
     for( iv=0 ; iv < N_iv ; ++iv ){
      EDIT_substitute_brick( Tvset , iv , MRI_float , NULL ) ;
      Tvar[iv] = DSET_ARRAY(Tvset,iv) ;  /* get array  */
      EDIT_BRICK_TO_NOSTAT(Tvset,iv) ;
      if (Tbone) sprintf(stmp,"nTc%.3f", Threshv[iv]);
      else       sprintf(stmp,"Tc%.3f" , Threshv[iv]);
      EDIT_BRICK_LABEL(Tvset, iv, stmp);
     }
     if( THD_deathcon() && THD_is_file(DSET_HEADNAME(Tvset)) )
       ERROR_exit("Output dataset %s already exists!",
                  DSET_HEADNAME(Tvset)) ;
     tross_Make_History( "3dTcorrMap" , argc,argv , Tvset ) ;
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

   isodd = (ntime%2 == 1) ;
   ccar = (float *)malloc(sizeof(float)*nmask) ;  /* 29 Apr 2009 */

   if( expr_type != '\0' ){  /* 23 Jun 2009 */
     atoz[17] = (double *)malloc(sizeof(double)*nmask) ;
     eear     = (double *)malloc(sizeof(double)*nmask) ;
   }

   /* initialize timer */
   etime(&tt,0);

   /* 29 Apr 2009: print # of threads message to amuse the user */

#ifdef USE_OMP
#pragma omp parallel
 {
  if( omp_get_thread_num() == 0 )
    INFO_message("Starting long loop: OpenMP thread count = %d",omp_get_num_threads()) ;
 }
#else
 INFO_message("Starting the long long loop through all voxels") ;
#endif

   for( ii=0 ; ii < nmask ; ii++ ){  /* outer loop over voxels: */
                                     /* time series to correlate with */

     if( vstep && ii%vstep == vstep-1 ){
       if( ii < vstep ){
         dtt = etime(&tt,1) ;
         ININFO_message("Single loop duration: %.3f mins\n"
                     "   Remaining time estim: %.3f mins = %.3f hrs\n",
                        dtt/60.0, dtt/60.0*49.0 , dtt/3600.0*49.0 ) ;
         fprintf(stderr,"++ Voxel loop: ") ;
       }
       vstep_print() ;
     }

     xsar = MRI_FLOAT_PTR( IMARR_SUBIM(timar,ii) ) ;  /* ii-th time series */

#pragma omp parallel
 { int vv,uu ; float *ysar ; float qcc ;
 AFNI_OMP_START ;
#pragma omp for
     for( vv=0 ; vv < nmask ; vv++ ){ /* inner loop over voxels */

       if( vv==ii ){ ccar[vv] = 0.0f ; continue ; }
       ysar = MRI_FLOAT_PTR( IMARR_SUBIM(timar,vv) ) ;

       /** dot products (unrolled by 2 on 29 Apr 2009) **/

       if( isodd ){
         for( qcc=xsar[0]*ysar[0],uu=1 ; uu < ntime ; uu+=2 )
           qcc += xsar[uu]*ysar[uu] + xsar[uu+1]*ysar[uu+1] ;
       } else {
         for( qcc=0.0f,uu=0 ; uu < ntime ; uu+=2 )
           qcc += xsar[uu]*ysar[uu] + xsar[uu+1]*ysar[uu+1] ;
       }
       ccar[vv] = qcc ; /* save correlation in ccar for later (OpenMP mod) */
     } /* end of inner loop over voxels (vv) */
 AFNI_OMP_END ;
 } /* end OpenMP */

     /** combine results in ccar to give output values **/

     Tcount = Mcsum = Zcsum = Qcsum = 0.0f ;
     for( iv=0 ; iv < N_iv ; ++iv ) Tvcount[iv] = 0.0f ;
     Pcsum = 0.0f ; nPcsum = 0 ;

     for( jj=0 ; jj < nmask ; jj++ ){
       if( jj == ii ) continue ;
       cc = ccar[jj] ;
       Mcsum += cc ;
       Zcsum += 0.5f * logf((1.0001f+cc)/(1.0001f-cc));
       Qcsum += cc*cc ;
       if( cc > 0.0f ){ Pcsum += cc*cc ; nPcsum++ ; }
       acc = (cc < 0) ? -cc : cc ;
       if( acc >= Thresh ) Tcount++ ;
       if( Threshv ){
          iv = N_iv - 1 ;
          while( iv > -1 ){
            if( acc >= Threshv[iv] ){
              do { Tvcount[iv--]++; } while (iv > -1);
            }
            --iv;
          }
        }
     } /* end of combining */

     if( Mar != NULL ) Mar[indx[ii]] = Mcsum / (nmask-1.0f) ;
     if( Zar != NULL ) Zar[indx[ii]] = tanh( Zcsum / (nmask-1.0f) ) ;
     if( Qar != NULL ) Qar[indx[ii]] = sqrt( Qcsum / (nmask-1.0f) ) ;
     if( Tar != NULL ) Tar[indx[ii]] = Tcount ;
     if( Tvar != NULL ){
       for(iv=0 ; iv < N_iv ; ++iv ) Tvar[iv][indx[ii]] = Tvcount[iv] ;
     }

     if( Par != NULL )  /* 23 Jun 2009 */
       Par[indx[ii]] = Pcsum / MAX(1,nPcsum) ;

     if( expr_type != '\0' ){  /* 23 Jun 2009 */
       for( jj=0 ; jj < nmask ; jj++ ) atoz[17][jj] = (double)ccar[jj] ;
       PARSER_evaluate_vector( expr_code , atoz , nmask, eear ) ;
       Esum = 0.0f ; nEsum = 0.0f ;
       for( jj=0 ; jj < nmask ; jj++ ){
         if( jj == ii ) continue ;
         Esum += eear[jj] ; if( eear[jj] != 0.0 ) nEsum++ ;
       }
       switch( expr_type ){
         case 'A': Ear[indx[ii]] = Esum / (nmask-1.0f) ; break ;
         case 'C': Ear[indx[ii]] = Esum / MAX(1,nEsum) ; break ;
         case 'S': Ear[indx[ii]] = Esum ;                break ;
       }
     }

   } /* end of outer loop over voxels (ii) */

   if (Tbone) { /* scale by expected number of voxels by chance */
      float p[3], sc, pval;
      p[0] = (float)ntime;
      p[1] = 1.0;
      p[2] = (float)nref;
      for (iv=0; iv<N_iv; ++iv)  {
         pval = THD_stat_to_pval (Threshv[iv], NI_STAT_CORREL, p);
         sc =  (float) DSET_NVOX(Tvset) * pval;
         if (sc < 0.05) sc = 0.05;
         /*
         fprintf(stderr,"Scaling sb %d with threshold %.2f\n"
                        "and p %.5f by %f\n",
                        iv, Threshv[iv],
                        pval, sc);
         */
         for( ii=0 ; ii < nmask ; ii++ ) Tvar[iv][indx[ii]] /= sc;
      }
   }

   if( vstep ) fprintf(stderr,"!\n") ;

   /*--- finito ---*/

   free(indx) ; DESTROY_IMARR(timar) ; free(ccar) ;

   if (Tvar) free(Tvar); Tvar = NULL;
   if (Threshv) free(Threshv); Threshv=NULL;
   if (Tvcount) free(Tvcount); Tvcount=NULL;

   if( Mset != NULL ){
     DSET_write(Mset) ; WROTE_DSET(Mset) ; DSET_delete(Mset) ;
   }
   if( Pset != NULL ){
     DSET_write(Pset) ; WROTE_DSET(Pset) ; DSET_delete(Pset) ;
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
   if( Tvset != NULL ){
     DSET_write(Tvset) ; WROTE_DSET(Tvset) ; DSET_delete(Tvset) ;
   }
   if( Eset != NULL ){
     DSET_write(Eset) ; WROTE_DSET(Eset) ; DSET_delete(Eset) ;
   }

   INFO_message("total CPU time = %.2f s",COX_cpu_time()) ; exit(0) ;
}
