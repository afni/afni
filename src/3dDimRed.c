#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

/*----------------------------- macros and prototypes ------------------------*/

#undef  DR_workspace
#define DR_workspace(na,nb) (double *)malloc(sizeof(double)*(2*(na)+(nb)+3)*(nb))

static void vec_dimred( int nt , int nv , int nfixed , float *inar , int rdim ,
                        int polort , float dt , float fbot , float ftop ,
                        int despike , int vnorm ,
                        double *ws , float *umm , float *svv  ) ;

static void preproc_dimred( int nt , int nvv , float *inar ,
                            int polort ,
                            float dt , float fbot , float ftop ,
                            int despike , int vnorm  ) ;

static void DR_set_svd_sort( int ss ) ;
static void DR_svd_double( int m, int n, double *a, double *s, double *u, double *v ) ;

/*----------------------------------------------------------------------------*/

static char *chunk_fnam = NULL ;  /* for -chunk temp storage */
static FILE *chunk_file = NULL ;

static void DR_atexit(void) /*-- called by exit(): delete chunk_file --*/
{
   if( chunk_file != NULL ){ fclose(chunk_file) ; chunk_file = NULL ; }
   if( chunk_fnam != NULL ){
     INFO_message("Deleting -chunk file %s",chunk_fnam) ;
     remove(chunk_fnam) ; chunk_fnam = NULL ;
   }
   return ;
}

/*----------------------------------------------------------------------------*/

static char * append_to_prefix( char *pre , char *app )
{
   char *prA , *prB , *oot ; int lll ;

   prA = strdup(pre) ; lll = strlen(prA) ;

   if( STRING_HAS_SUFFIX(prA,".nii") && lll > 4 ){
     prA[lll-4] = '\0' ;
     prB        = strdup(".nii") ;
   } else if( STRING_HAS_SUFFIX(prA,".nii.gz") && lll > 7 ){
     prA[lll-7] = '\0' ;
     prB        = strdup(".nii.gz") ;
   } else {
     prB        = strdup("\0") ;
   }

   oot = (char *)malloc(sizeof(char)*(lll+strlen(app)+9)) ;
   strcpy(oot,prA) ;
   strcat(oot,"_") ;
   strcat(oot,app) ;
   strcat(oot,prB) ;
   free(prB) ; free(prA) ; return oot ;
}

/*----------------------------------------------------------------------------*/

void DR_syntax(void)
{
   printf(
     "Program 3dDimRed does 'dimensional reduction' on a collection of input 3D+time\n"
     "datasets, and produces as output a smaller collection of 3D+time datasets.\n"
     "The process of dimensional reduction is via the Singular Value Decomposition\n"
     "(SVD), and the goal is to produce (at each voxel) a collection of time series\n"
     "vectors whose linear span captures most of the variability present in the\n"
     "input.  The number of output datasets is specified by the user, via the\n"
     "'-rdim' option.\n"
     "\n"
     "Usage: 3dDimRed [options] inset1 inset2 ...\n"
     "\n"
     "where 'inset1' is the first input 3D+time dataset, 'inset2' is the second\n"
     "input 3D+time dataset, and so forth.  All input datasets must be on the\n"
     "same 3D+time grid, and there must be at least 2 input datasets.\n"
     "\n"
     "--------\n"
     "OPTIONS:\n"
     "--------\n"
     " -rdim rr        = The number 'rr' is the number of dimensions to use for the\n"
     "                   output = the number of output 3D+time datasets.\n"
     "                   * 'rr' must be more than 0 and less than the number of\n"
     "                     input datasets.\n"
     "                   * If 'rr' is set to 0, or is not given, then no output\n"
     "                     3D+time datasets will be produced!\n"
     "\n"
     " -prefix pp      = The string 'pp' is the root prefix for the output datasets.\n"
     "                   The first one will get the prefix 'pp_001', etc.\n"
     "                   * If '-prefix' is not given, then the default root prefix\n"
     "                     is 'dimred'.\n"
     "\n"
     " -sing           = Save the singular values at each voxel into a dataset with\n"
     "                   prefix 'pp_sing', where 'pp' is the root prefix.\n"
     "                   * If this option is not given, the singular values are not\n"
     "                     saved.\n"
     "\n"
     " -1Dcols a b ... = This option lets you specify some extra time series to be\n"
     "                   included in the dimension reduction, which are fixed for each\n"
     "                   input voxel.  Each column of each 1D file following '-1Dcols'\n"
     "                   is added to the collection of vectors to be processed at each\n"
     "                   voxel.\n"
     "\n"
     " -input x y ...  = Alternate way to input 3D+time datasets.\n"
     "\n"
     " -polort qq      = Detrend the time series with polynomial basis of order 'qq'\n"
     "                   prior to further processing.\n"
     "                   * You cannot use this option with '-band'!\n"
     "                   * If neither '-polort' nor '-band' is given, then the mean\n"
     "                     of each time series is removed (e.g., '-polort 0').\n"
     "\n"
     " -band fbot ftop = This option specifies to bandpass the time series prior\n"
     "                   to further processing (as in program 3dBandpass).\n"
     "                   * You cannot use this option with '-polort'!\n"
     "\n"
     " -dt dd          = Set time step to 'dd' seconds (for use with '-band').\n"
     "   *OR*            * Usually the time step is taken from the 3D+time dataset\n"
     " -TR dd              header, but '-dt' (or '-TR') lets you over-ride that.\n"
     "                   * If all inputs are 1D files, then this option is needed\n"
     "                     to set the right time step; otherwise, TR is taken as 1.\n"
     "\n"
     " -despike        = Despike each input time series before other processing.\n"
     "                   * Hopefully, you don't need to do this, which is why it\n"
     "                     is optional.\n"
     "\n"
     " -novnorm        = By default, just before the SVD, each time series vector is\n"
     "                   normalized to L2 magnitude 1.  Use '-novnorm' to turn this\n"
     "                   step off.\n"
     "\n"
     " -mask mset      = Mask dataset\n"
     " -automask       = Create mask from first input dataset\n"
     "\n"
     " -chunk          = 3dDimRed can use up a LOT of memory.  This option lets\n"
     "                   you cut down on that by having it operate on the voxels\n"
     "                   in chunks.  The downside to doing this is more disk I/O.\n"
     "                   * '-chunk' alone means do 10,000 voxels at one time.\n"
     "                   * '-chunk 999' means to do 999 voxels at one time (etc).\n"
     "\n"
     " -quiet          = Suppress some of the fun informative progress messages.\n"
     "\n"
     "------\n"
     "NOTES:\n"
     "------\n"
     " * You can input ONLY 1D files, via the -1Dcols option.  In that case,\n"
     "   there is only 1 'voxel', and instead of 'rdim' datasets being output\n"
     "   output to hold the results, a single 1D dataset with 'rdim' columns\n"
     "   will be produced.\n"
     " * The output vectors are the first 'rdim' left singular vectors, as\n"
     "   would be produced by '1dsvd -1Dleft' (for example).  They are intended\n"
     "   to be used in further processing scripts, and are probably not useful\n"
     "   by themselves.\n"
     " * Written by Zhark the Singular -- April 2012\n"
   ) ;
   PRINT_AFNI_OMP_USAGE("3dDimRed",NULL) ;
   PRINT_COMPILE_DATE ; exit(0) ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int nopt=1 , verb=1 ;
   int do_despike=0 , do_vnorm=1 , do_automask=0 , do_sing=0 , polort=-1 ;
   float fbot=-666.0f, ftop=-999.9f , dt=0.0f ; int have_freq=0 ;
   byte *mask=NULL ; int mask_nx=0,mask_ny=0,mask_nz=0,nmask=0 , *imask=NULL ;
   int nx,ny,nz,nt,nvox , nfft=0 , rdim=0 , kk,ii,qq,pp , nbad=0 ;
   int ndset=0,nim=0,nvim=0 , ndim=0 , chunk=0,im,imbot,imtop ;
   char *prefix="dimred" ;
   MRI_IMARR *imarc ;
   RwcPointer_array *dsar ;
   THD_3dim_dataset *iset=NULL,*kset ; MRI_IMAGE *tim ;
   float *cfixv=NULL , *cvect=NULL , *csave=NULL ;
   size_t msize , ntsiz , ndsiz ;
   char *prefout , prefapp[32] ;

   /*-- startup --*/

   mainENTRY("3dDimRed"); machdep();
   AFNI_logger("3dDimRed",argc,argv);
   PRINT_VERSION("3dDimRed"); AUTHOR("Uncle John's Band");
   enable_mcw_malloc() ;
   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   INIT_XTARR(dsar) ; INIT_IMARR(imarc) ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcmp(argv[nopt],"-") == 0 ){ nopt++ ; continue ; }

     if( strcasecmp(argv[1],"-help") == 0 ) DR_syntax() ;

     if( strcasecmp(argv[nopt],"-1Dcols") == 0 ){
       MRI_IMAGE *qim ;
       if( ++nopt >= argc ) ERROR_exit("need an argument after %s",argv[nopt-1]) ;
       if( argv[nopt][0] == '-' ) ERROR_exit("Illegal argument after %s",argv[nopt-1]) ;
       for( ; nopt < argc && argv[nopt][0] != '-' ; nopt++ ){
         qim = mri_read_1D(argv[nopt]) ;
         if( qim == NULL ) ERROR_exit("Can't read from -1Dcols file '%s'",argv[nopt]) ;
         mri_add_name(argv[nopt],qim) ;
         ADDTO_IMARR(imarc,qim) ;
       }
       continue ;
     }

     if( strcasecmp(argv[nopt],"-rdim") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after -rdim!") ;
       rdim = (int)strtod(argv[nopt],NULL) ;
       if( rdim <= 0 )
         WARNING_message("-rdim value means no output time series datasets") ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-chunk") == 0 ){
       if( ++nopt < argc && isdigit(argv[nopt][0]) ){
         chunk = (int)strtod(argv[nopt++],NULL) ;
         if( chunk < 666 ) chunk = 666 ;  /* Lame user */
       } else {
         chunk = 10000 ;
       }
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-despike") == 0 ){
       do_despike++ ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-sing") == 0 ){
       do_sing++ ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after -prefix!") ;
       prefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix option!") ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't use -mask AND -automask!") ;
       do_automask = 1 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-mask") == 0 ){
       THD_3dim_dataset *mset ;
       if( ++nopt >= argc ) ERROR_exit("Need argument after '-mask'") ;
       if( mask != NULL || do_automask ) ERROR_exit("Can't have two mask inputs") ;
       mset = THD_open_dataset( argv[nopt] ) ;
       CHECK_OPEN_ERROR(mset,argv[nopt]) ;
       DSET_load(mset) ; CHECK_LOAD_ERROR(mset) ;
       mask_nx = DSET_NX(mset); mask_ny = DSET_NY(mset); mask_nz = DSET_NZ(mset);
       mask = THD_makemask( mset , 0 , 0.5f, 0.0f ) ; DSET_delete(mset) ;
       if( mask == NULL ) ERROR_exit("Can't make mask from dataset '%s'",argv[nopt]) ;
       nmask = THD_countmask( mask_nx*mask_ny*mask_nz , mask ) ;
       if( verb ) INFO_message("Number of voxels in mask = %d",nmask) ;
       if( nmask < 1 ) ERROR_exit("Mask is empty: nothing to process :-(") ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-novnorm") == 0 ){
       do_vnorm = 0 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-quiet") == 0 ){
       verb = 0 ; nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-dt") == 0 || strcasecmp(argv[nopt],"-TR") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after %s",argv[nopt-1]) ;
       dt = (float)strtod(argv[nopt],NULL) ;
       if( dt <= 0.0f ) WARNING_message("value after %s illegal!",argv[nopt-1]) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-polort") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after -polort!") ;
       if( have_freq ) ERROR_exit("Can't use -polort and -band together!") ;
       polort = (int)strtod(argv[nopt],NULL) ;
       nopt++ ; continue ;
     }

     if( strcasecmp(argv[nopt],"-band") == 0 ){
       if( ++nopt >= argc-1 ) ERROR_exit("need 2 arguments after -band!") ;
       if( polort >= 0 ) ERROR_exit("Can't use -band and -polort together!") ;
       fbot = strtod(argv[nopt++],NULL) ;
       ftop = strtod(argv[nopt++],NULL) ;
       if( fbot < 0.0f || ftop <= fbot ) WARNING_message("values after -band are illegal!") ;
       else                              have_freq = 1 ;
       continue ;
     }

     if( strcasecmp(argv[nopt],"-input") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("Need argument after '%s'",argv[nopt-1]);
       if( argv[nopt][0] == '-' ) ERROR_exit("Illegal argument after '%s'",argv[nopt-1]) ;
       for( ; nopt < argc && argv[nopt][0] != '-' ; nopt++ ){
         kset = THD_open_dataset(argv[nopt]) ;
         if( kset == NULL ) ERROR_exit("Can't read dataset '%s'",argv[nopt]) ;
         ADDTO_XTARR(dsar,kset) ;
         ii = XTARR_NUM(dsar)-1 ; XTARR_IC(dsar,ii) = IC_DSET ;
       }
       continue ;
     }

     ERROR_message("Unknown option: %s\n",argv[nopt]) ;
     suggest_best_prog_option(argv[0], argv[nopt]);
     exit(1);

   } /*-- end of loop over options --*/

   if( argc < 2 ) DR_syntax() ;

   /*--- read rest of args as input datasets ---*/

   for( ; nopt < argc ; nopt++ ){
     kset = THD_open_dataset(argv[nopt]) ;
     if( kset == NULL ) ERROR_exit("Can't read dataset '%s'",argv[nopt]) ;
     ADDTO_XTARR(dsar,kset) ;
     ii = XTARR_NUM(dsar)-1 ; XTARR_IC(dsar,ii) = IC_DSET ;
   }

   /*---- check stuff ----*/

   ndset = XTARR_NUM(dsar) ; nim = IMARR_COUNT(imarc) ;
   if( ndset < 2 && nim <= 0 )
     ERROR_exit("Inadequate number of input datasets!  What am I supposed to do?!") ;
   if( ndset == 0 && nim == 1 && IMARR_SUBIM(imarc,0)->ny == 1 )
     ERROR_exit("Inadequate number of input 1D columns!  What am I supposed to do?!") ;

   if( rdim <= 0 && !do_sing )
     ERROR_exit("No output specified by either -rdim or -sing :-(") ;

   /* check input datasets and/or 1D files for matches of various kinds */

   if( ndset > 0 ){
     iset = (THD_3dim_dataset *)XTARR_XT(dsar,0) ;
     nx = DSET_NX(iset); ny = DSET_NY(iset); nz = DSET_NZ(iset); nvox = nx*ny*nz;
     nt = DSET_NVALS(iset) ;
     if( dt <= 0.0f ) dt = DSET_TR(iset) ;
     if( nt < 2 ){
       ERROR_message("Dataset %s has only 1 point in the time direction!",DSET_HEADNAME(iset)) ;
       nbad++ ;
     }
     if( dt <= 0.0f && have_freq ){
       WARNING_message("TR not present in first input dataset: using 1 s") ;
       dt = 1.0f ;
     }

     for( kk=1 ; kk < ndset ; kk++ ){
       kset = (THD_3dim_dataset *)XTARR_XT(dsar,kk) ;
       if( DSET_NVOX(kset) != nvox ){
         ERROR_message("Dataset %s has %d voxels, but first dataset has %d",
                       DSET_HEADNAME(kset) , DSET_NVOX(kset) , nvox ) ;
         nbad++ ;
       }
       if( DSET_NVALS(kset) != nt ){
         ERROR_message("Dataset %s has %d time points, but first dataset has %d",
                       DSET_HEADNAME(kset) , DSET_NVALS(kset) , nt ) ;
         nbad++ ;
       }
     }
   } else {                       /* no actual input datasets! */
     tim = IMARR_SUBIM(imarc,0) ;
     nvox = 1 ; nt = tim->nx ;
     if( nt < 2 ){
       ERROR_message("1D file %s has only 1 time point!",tim->name) ;
       nbad++ ;
     }
     if( mask != NULL ){
       WARNING_message("skipping -mask since no 3D+time datasets were input") ;
       free(mask) ; mask = NULL ;
     }
   }

   for( kk=0 ; kk < nim ; kk++ ){
     tim = IMARR_SUBIM(imarc,kk) ;
     if( tim->nx != nt ){
       ERROR_message("1D file %s has %d time points, but should have %d",
                     tim->name , tim->nx , nt ) ;
       nbad++ ;
     }
     nvim += tim->ny ; /* count of number of 1D time series vectors */
   }

   if( mask != NULL ){
     if( mask_nx != nx || mask_ny != ny || mask_nz != nz ){
       ERROR_message("-mask dataset grid doesn't match input dataset") ;
       nbad++ ;
     }
   } else if( do_automask ){
     if( ndset == 0 ){
       WARNING_message("ignoring -automask since no 3D+time datasets were input") ;
     } else {
       mask = THD_automask(iset) ;
       if( mask == NULL ){
         ERROR_message("Can't create -automask from first input dataset?") ; nbad++ ;
       }
       nmask = THD_countmask( nvox , mask ) ;
       if( verb ) INFO_message("Number of voxels in automask = %d",nmask);
       if( nmask < 1 ){ ERROR_message("Automask is too small to process"); nbad++; }
     }
   }
   if( mask == NULL ){
     mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
     memset(mask,1,sizeof(byte)*nvox) ;
     if( verb && nvox > 1 ) INFO_message("No mask ==> processing all %d voxels",nvox);
   }

   /* make list of voxels to process, from mask */

   imask = (int *)malloc(sizeof(int)*nmask) ;
   for( qq=im=0 ; im < nvox ; im++ ){
     if( mask[im] ) imask[qq++] = im ;
   }
   free(mask) ; mask = NULL ;

   ndim = ndset + nvim ;  /* total number of input dimensions */
   if( ndim <= rdim ){
     ERROR_message("Input dimension = %d is not bigger than output = %d",ndim,rdim) ;
     nbad++ ;
   }

   ndsiz = sizeof(float)*ndim ;  /* size of singular values vector */
   ntsiz = sizeof(float)*nt ;    /* size of time series vectors */

   if( nbad > 0 )
     ERROR_exit("Can't continue after above ERRORs") ;  /*** BAD USER ***/

   /*---- create temp file for usufruct of chunk -----*/

   if( chunk > 0 && ndset == 0 ) chunk = 0 ;

   if( chunk > 0 && chunk < nmask ){
     chunk_fnam    = UNIQ_idcode() ;
     chunk_fnam[0] = 'D' ; chunk_fnam[1] = 'R' ; chunk_fnam[2] = 'X' ;
     chunk_file    = fopen( chunk_fnam , "w+b" ) ;
     if( chunk_file == NULL )
       ERROR_exit("Unable to create -chunk temporary file %s",chunk_fnam) ;
     INFO_message("Creating -chunk temporary file %s",chunk_fnam) ;
     atexit(DR_atexit) ;
   }

   if( chunk <= 0 || chunk > nmask ) chunk = nmask ;

   if( verb )
     INFO_message("Chunk size = %s voxels",commaized_integer_string(nmask)) ;

   DR_set_svd_sort(-1) ;

   /*--- allocate space to hold results from one chunk ---*/
   /*--- for each voxel, rdim vectors of length nt,    ---*/
   /*--- plus the singular values, of length ndim.     ---*/

/* macro == vi-th output nt-vector for the ic-th voxel */

#undef  CSUU
#define CSUU(ic,vi) (csave+((ic)*(rdim*nt+ndim)+(vi)*nt))

/* macro == output ndim-vector of singular values for the ic-th voxel */

#undef  CSSV
#define CSSV(ic)    (csave+((ic)*(rdim*nt+ndim)+rdim*nt))

/* macro == time series nt-vector from the kd-th dataset, ic-th voxel */

#undef  CVD
#define CVD(kd,ic)  (cvect+(kd+(ic)*ndset)*nt)

/* macro == ic-th fixed time series nt-vector */

#undef  CFV
#define CFV(ic)     (cfixv+(ic)*nt)

   msize = (rdim*ntsiz+ndsiz)*chunk ;
   csave = (float *)malloc(msize) ;
   if( verb || csave == NULL )
     ININFO_message("Output memory chunk = %s bytes (%s)",
                    commaized_integer_string(msize) ,
                    approximate_number_string((double)msize) ) ;
   if( csave == NULL )
     ERROR_exit("Can't allocate memory for output chunk") ;

   /* memory to hold dataset inputs from one chunk: ndset vectors of length nt */

   if( ndset > 0 ){
     msize = ntsiz*chunk*ndset ;
     cvect = (float *)malloc(msize) ;
     if( verb || cvect == NULL )
       ININFO_message("Dataset input memory chunk = %s bytes (%s)",
                      commaized_integer_string(msize) ,
                      approximate_number_string((double)msize) ) ;
     if( cvect == NULL )
       ERROR_exit("Can't allocate memory for input chunk") ;
   }

   /* memory for fixed vectors, and pre-process them now */

   if( nvim > 0 ){
     MRI_IMAGE *tim ; float *tar ;
     cfixv = (float *)malloc(ntsiz*nvim) ;
     if( cfixv == NULL )
       ERROR_exit("Can't allocate memory for fixed vectors") ;
     for( qq=kk=0 ; kk < nim ; kk++ ){
       tim = IMARR_SUBIM(imarc,kk) ; tar = MRI_FLOAT_PTR(tim) ;
       for( pp=0 ; pp < tim->ny ; pp++,qq++ )
         AAmemcpy( CFV(qq) , tar+pp*nt , ntsiz ) ;
     }
     DESTROY_IMARR(imarc) ;
     preproc_dimred( nt , nvim , cfixv ,
                     polort , dt , fbot , ftop , do_despike , do_vnorm ) ;
   }

#ifdef USE_OMP
#pragma omp parallel
 {
  if( verb && omp_get_thread_num() == 0 )
    INFO_message("OpenMP thread count = %d",omp_get_num_threads()) ;
 }
#else
  if( verb && nmask > 1 ) INFO_message("Start main voxel loop") ;
#endif

   /********** loop over chunks **********/

   for( imbot=0 ; imbot < nmask ; imbot+=chunk ){

     imtop = imbot + chunk ; if( imtop > nmask ) imtop = nmask ;

     if( verb && chunk < nmask )
       ININFO_message("start chunk: voxels %d..%d",imbot,imtop-1) ;

     /* extract dataset data into cvect */

ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
     for( kk=0 ; kk < ndset ; kk++ ){
       kset = (THD_3dim_dataset *)XTARR_XT(dsar,kk) ;
ININFO_message("Load dataset %s",DSET_HEADNAME(kset)) ;
       DSET_load(kset) ;
       if( !DSET_LOADED(kset) )
         ERROR_exit("Can't load data from dataset %s",DSET_BRIKNAME(kset)) ;
       for( ii=imbot ; ii < imtop ; ii++ )
         THD_extract_array( imask[ii] , kset , 0 , CVD(kk,ii-imbot) ) ;
       DSET_unload(kset) ;
     }
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;

 AFNI_OMP_START ;
#pragma omp parallel if( imtop-imbot > 66 )
     { float *tsar , *umat , *sval ; double *ws ; int iv,kd ;
       tsar = (float *)malloc(ntsiz*ndim) ;                /* input vectors */
       umat = (rdim <= 0) ? NULL
              : (float *)malloc(sizeof(float)*ntsiz*rdim); /* output vectors */
       sval = (float *)malloc(ndsiz) ;                     /* output sing vals */
       ws   = DR_workspace(nt,ndim) ;
#pragma omp for
       for( iv=imbot ; iv < imtop ; iv++ ){
         /* copy fixed data into tsar */
         if( cfixv != NULL ){
ININFO_message("iv = %d: cfixv -> tsar",iv) ;
           AAmemcpy( tsar , cfixv , ntsiz*nvim ) ;
         }
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
         /* copy dataset data into tsar */
         if( cvect != NULL ){
ININFO_message("iv = %d: cvect -> tsar",iv) ;
           for( kd=0 ; kd < ndset ; kd++ )
             AAmemcpy( tsar+(nvim+kd)*nt , CVD(kd,iv-imbot) , ntsiz ) ;
         }
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
         /* process it into umat and sval */
ININFO_message("iv = %d: call vec_dimred",iv) ;
         vec_dimred( nt , ndim , nvim , tsar , rdim ,
                     polort , dt , fbot , ftop , do_despike , do_vnorm ,
                     ws , umat , sval ) ;
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
         /* copy umat and sval into csave */
         if( umat != NULL ){
ININFO_message("iv = %d: umat -> csave",iv) ;
           AAmemcpy( CSUU(iv-imbot,0) , umat , ntsiz*rdim ) ;
         }
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
ININFO_message("iv = %d: sval -> csave",iv) ;
         AAmemcpy( CSSV(iv-imbot) , sval , ndsiz ) ;
       } /* end of parallel-ized loop over voxels */
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
       free(ws) ; free(sval) ; free(tsar) ; if( umat != NULL ) free(umat) ;
     } /* end of parallel-ized code */
     AFNI_OMP_END ;

     /* if using temporary file, write csave to disk */

     if( chunk_file != NULL ){
       msize = fwrite( csave, (rdim*ntsiz+ndsiz), imtop-imbot, chunk_file ) ;
       if( msize < imtop-imbot )
         ERROR_exit("Failure to write to -chunk file %s -- is disk full?",
                    chunk_fnam ) ;
     }
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;

   } /* end of loop over chunks of voxels */

   if( cvect != NULL ) free(cvect) ;
   if( cfixv != NULL ) free(cfixv) ;

   if( ndset > 0 && nvox > 1 ){ /* create output datasets for vectors */

     for( kk=0 ; kk < rdim ; kk++ ){

       /* create the empty dataset */

       sprintf(prefapp,"%03d",kk+1) ;
       prefout = append_to_prefix( prefix , prefapp ) ;
       iset = (THD_3dim_dataset *)XTARR_XT(dsar,0) ;
       kset = EDIT_empty_copy(iset) ;
       EDIT_dset_items( kset , ADN_prefix , prefout , NULL ) ;
       tross_Copy_History( iset , kset ) ;
       tross_Make_History( "3dDimRed" , argc,argv , kset ) ;
       free(prefout) ;

       /* put in the sub-bricks */
       for( pp=0 ; pp < nt ; pp++ )
         EDIT_substitute_brick( kset , pp , MRI_float , NULL ) ;

       if( chunk_file != NULL ) rewind(chunk_file) ;

       /* loop over chunks, fill dataset time series */

       for( imbot=0 ; imbot < nmask ; imbot += chunk ){

         imtop = imbot + chunk ; if( imtop > nmask ) imtop = nmask ;

         if( chunk_file != NULL ){  /* get data back from disk */
           msize = fread( csave, (rdim*ntsiz+ndsiz), imtop-imbot, chunk_file ) ;
           if( msize < imtop-imbot )  /* should never happen */
             ERROR_exit("Failure to read from -chunk file %s :-(",chunk_fnam) ;
         }

         for( im=imbot ; im < imtop ; im++ )  /* shove it into the time series */
           THD_insert_series( imask[im], kset, nt, MRI_float, CSUU(im-imbot,kk),0 ) ;
       }

       DSET_write(kset) ; WROTE_DSET(kset) ; DSET_delete(kset) ;
     }

   } else if( rdim > 0 ){  /* create a 1D output file */

     MRI_IMAGE *qim ; float *qar ; char *prefout ;

     qim = mri_new(nt,rdim,MRI_float) ; qar = MRI_FLOAT_PTR(qim) ;
     AAmemcpy( qar , CSUU(0,0) , ntsiz*rdim ) ;
     prefout = append_to_prefix( prefix , "vect.1D" ) ;
     mri_write_1D(prefout,qim) ; mri_free(qim) ;
     INFO_message("Wrote vector file %s",prefout) ; free(prefout) ;

   } /* end of writing out vectors */

   /* write the singular values dataset? */

   if( do_sing ){
     if( ndset > 0 && nvox > 1 ){
       prefout = append_to_prefix( prefix , "sing" ) ;
       iset = (THD_3dim_dataset *)XTARR_XT(dsar,0) ;
       kset = EDIT_empty_copy(iset) ;
       EDIT_dset_items( kset ,
                          ADN_prefix , prefout ,
                          ADN_nvals  , ndim    ,
                        NULL ) ;
       tross_Copy_History( iset , kset ) ;
       tross_Make_History( "3dDimRed" , argc,argv , kset ) ;
       free(prefout) ;

       for( pp=0 ; pp < ndim ; pp++ )
         EDIT_substitute_brick( kset , pp , MRI_float , NULL ) ;

       if( chunk_file != NULL ) rewind(chunk_file) ;

       for( imbot=0 ; imbot < nmask ; imbot += chunk ){

         imtop = imbot + chunk ; if( imtop > nmask ) imtop = nmask ;

         if( chunk_file != NULL ){
           msize = fread( csave, (rdim*ntsiz+ndsiz), imtop-imbot, chunk_file ) ;
           if( msize < imtop-imbot )  /* should never happen */
             ERROR_exit("Failure to read from -chunk file %s :-(",chunk_fnam) ;
         }

         for( im=imbot ; im < imtop ; im++ )
           THD_insert_series( imask[im], kset, ndim, MRI_float, CSSV(im-imbot),0 ) ;

       }

       DSET_write(kset) ; WROTE_DSET(kset) ; DSET_delete(kset) ;

     } else {  /* write a 1D file with the singular values */

       MRI_IMAGE *qim ; float *qar ; char *prefout ;

       qim = mri_new(ndim,1,MRI_float) ; qar = MRI_FLOAT_PTR(qim) ;
       AAmemcpy( qar , CSSV(0) , ndsiz ) ;
       prefout = append_to_prefix( prefix , "sing.1D" ) ;
       mri_write_1D(prefout,qim) ; mri_free(qim) ;
       INFO_message("Wrote singular values file %s",prefout) ; free(prefout) ;
     }

   } /* end of dosing */

   free(csave) ;
   exit(0) ;
}

/*----------------------------------------------------------------------------*/

#undef  INVEC
#define INVEC(k) (inar+(k)*nt)

static void preproc_dimred( int nt , int nvv , float *inar ,
                            int polort ,
                            float dt , float fbot , float ftop ,
                            int despike , int vnorm  )
{
   static double *pcc=NULL ;   /* for polort detrending */
   static float **pref=NULL ;
   int ii,jj,kk ;

   /*-- cleanup the trash call? --*/

#pragma omp critical
   { if( nt <= 0 ){
       if( pcc != NULL ){ free(pcc); pcc = NULL; }
       if( pref != NULL ){
         for( ii=0 ; ii <= polort ; ii++ ) free(pref[ii]) ;
         free(pref) ; pref = NULL ;
       }
     }
   }
   if( nt <= 0 || nvv <= 0 ) return ;

   /*-- first time in: create polort detrending stuff? --*/

#pragma omp critical
   { if( pcc == NULL && polort > 0 ){
       pref = THD_build_polyref(polort+1,nt) ;
       pcc  = startup_lsqfit( nt , NULL , polort+1 , pref ) ;
     }
   }

   /*-- despike? --*/

   if( despike ){
     for( kk=0 ; kk < nvv ; kk++ ) THD_despike9(nt,INVEC(kk)) ;
   }

   /*-- polort detrend? --*/

   if( polort == 0 ){
     for( kk=0 ; kk < nvv ; kk++ ) THD_const_detrend(nt,INVEC(kk),NULL) ;
   } else if( polort > 0 && pcc != NULL ){
     float *coef , *vec , *pr , cf ;
     for( kk=0 ; kk < nvv ; kk++ ){
       vec  = INVEC(kk) ;
       coef = delayed_lsqfit( nt , vec , polort+1 , pref , pcc ) ;
       if( coef != NULL ){
         for( jj=0 ; jj <= polort ; jj++ ){
           cf = coef[jj] ; pr = pref[jj] ;
           for( ii=0 ; ii < nt ; ii++ ) vec[ii] -= cf * pr[ii] ;
         }
         free(coef) ;
       }
     }
   }

   /*-- bandpass? --*/

   if( dt > 0.0f && fbot >= 0.0f && fbot < ftop ){
     float **vvar = malloc(sizeof(float *)*nvv) ;
     for( kk=0 ; kk < nvv ; kk++ ) vvar[kk] = INVEC(kk) ;
     THD_bandpass_vectors( nt , nvv , vvar , dt , fbot , ftop , 2 , 0 , NULL ) ;
     free(vvar) ;
   }

   /*-- L2 normalize? --*/

   if( vnorm ){
     for( kk=0 ; kk < nvv ; kk++ ) THD_normalize(nt,INVEC(kk)) ;
   }

   return ;
}

/*----------------------------------------------------------------------------*/
/* assume the first nfixed vectors are already pre-processed */

static void vec_dimred( int nt , int nv , int nfixed , float *inar , int rdim ,
                        int polort , float dt , float fbot , float ftop ,
                        int despike , int vnorm ,
                        double *ws , float *umm , float *svv  )
{
   int ntv=nt*nv , ntr=nt*rdim , ii,jj,pp,nnz ;
   float *vv ;
   double *wss , *amat , *sval , *umat , *vmat ;
   MRI_IMAGE *outim ; float *outar ;

   if( umm == NULL && svv == NULL ) return ;  /* WTF? */

ININFO_message("  preproc") ;
   preproc_dimred( nt , nv-nfixed , inar+nfixed*nt ,
                   polort , dt,fbot,ftop , despike , vnorm ) ;
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;

   wss = ws ; if( wss == NULL ) wss = DR_workspace(nt,nv) ;

   amat = wss ;          /* nt*nv long */
   umat = amat + ntv ;   /* nt*nv long */
   vmat = umat + nv*nv ; /* nv*nv long */
   sval = vmat + ntv ;   /* nv    long */

   /** copy vectors into amat, removing all zero columns **/

ININFO_message("  copy data in") ;
#undef  AM
#undef  IM
#define AM(i,j) amat[(i)+(j)*nt]
#define IM(i,j) inar[(i)+(j)*nt]
   for( jj=pp=0 ; jj < nv ; jj++ ){
     for( nnz=ii=0 ; ii < nt ; ii++ ){
       AM(ii,pp) = (double)IM(ii,jj) ; nnz += ( AM(ii,pp) != 0.0 ) ;
     }
     if( nnz > 0 ) pp++ ;
   }
#undef  AM
#undef  IM
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;

if( pp < nv )
ININFO_message("removed %d all zero vectors",nv-pp) ;

   /* initialize outputs to 0? */

   if( pp < nv ){
     if( umm != NULL ) AAmemset(umm,0,sizeof(float)*ntr) ;
     if( svv != NULL ) AAmemset(svv,0,sizeof(float)*nv ) ;
     if( pp == 0 ){  /* if all inputs 0, output 0 */
       if( wss != ws ) free(wss) ;
       return ;
     }
   }

   /* second dimension is now pp, possibly less than nv */

ININFO_message("**svd") ;
   DR_svd_double( nt , pp , amat , sval , umat , vmat ) ;
ININFO_message("**svd done") ;
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;

   if( umm != NULL ){
ININFO_message("  umat -> umm") ;
     nnz = nt*MIN(pp,rdim) ;
     for( ii=0 ; ii < nnz ; ii++ ) umm[ii] = (float)umat[ii] ;
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
   }
   if( svv != NULL ){
ININFO_message("  sval -> svv") ;
     for( ii=0 ; ii < pp ; ii++ ) svv[ii] = (float)sval[ii] ;
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
   }

   if( wss != ws ) free(wss) ;
   return ;
}

/*--------------------------------------------------------------------*/

#include "eispack.h"

#ifdef isfinite
# define IS_GOOD_FLOAT(x) isfinite(x)
#else
# define IS_GOOD_FLOAT(x) finite(x)
# define isfinite finite
#endif

/*--------------------------------------------------------------------*/

#define CHECK_SVD

#undef CHK
#ifdef CHECK_SVD
# define CHK 1
# define A(i,j) aa[(i)+(j)*m]
# define U(i,j) uu[(i)+(j)*m]
# define V(i,j) vv[(i)+(j)*n]
#else
# define CHK 0
#endif

/** setup for sorting SVD values:
      0 = no sort (whatever the function returns)
     +1 = sort in increasing order of singular values
     -1 = sort in descending order of singular values **/

static int svd_sort = 0 ;
static void DR_set_svd_sort( int ss ){ svd_sort = ss; }

/*----------------------------------------------------------------------------*/
/*! Compute SVD of double precision matrix:                      T
                                            [a] = [u] diag[s] [v]
    - m = # of rows in a = length of each column
    - n = # of columns in a = length of each row
    - a = pointer to input matrix; a[i+j*m] has the (i,j) element
          (m X n matrix, stored in column-first order)
    - s = pointer to output singular values; length = n (cannot be NULL)
    - u = pointer to output matrix, if desired; length = m*n (m X n matrix)
    - v = pointer to output matrix, if desired; length = n*n (n x n matrix)

  Modified 10 Jan 2007 to add sorting of s and corresponding columns of u & v.
------------------------------------------------------------------------------*/

static void DR_svd_double( int m, int n, double *a, double *s, double *u, double *v )
{
   integer mm,nn , lda,ldu,ldv , ierr ;
   doublereal *aa, *ww , *uu , *vv , *rv1 ;
   logical    matu , matv ;

   if( a == NULL || s == NULL || m < 1 || n < 1 ) return ;

   mm  = m ;
   nn  = n ;
   aa  = a ;
   lda = m ;
   ww  = s ;

   /* make space for u matrix, if not supplied */

   if( u == NULL ){
     matu = (logical) CHK ;
     uu   = (doublereal *)calloc(sizeof(double),m*n) ;
   } else {
     matu = (logical) 1 ;
     uu = u ;
   }
   ldu = m ;

   /* make space for v matrix if not supplied */

   if( v == NULL ){
     matv = (logical) CHK ;
     vv   = (CHK) ? (doublereal *)calloc(sizeof(double),n*n) : NULL ;
   } else {
     matv = (logical) 1 ;
     vv   = v ;
   }
   ldv = n ;

   rv1 = (double *)calloc(sizeof(double),n) ;  /* workspace */

   /** the actual SVD **/

   (void) svd_( &mm , &nn , &lda , aa , ww ,
                &matu , &ldu , uu , &matv , &ldv , vv , &ierr , rv1 ) ;

#ifdef CHECK_SVD
   /** back-compute [A] from [U] diag[ww] [V]'
       and see if it is close to the input matrix;
       if not, compute the results in another function;
       this is needed because the svd() function compiles with
       rare computational errors on some compilers' optimizers **/
   { register int i,j,k ; register doublereal aij ; double err=0.0,amag=1.e-11 ;
     for( j=0 ; j < n ; j++ ){
      for( i=0 ; i < m ; i++ ){
        aij = A(i,j) ; amag += fabs(aij) ;
        for( k=0 ; k < n ; k++ ) aij -= U(i,k)*V(j,k)*ww[k] ;
        err += fabs(aij) ;
     }}
     amag /= (m*n) ; /* average absolute value of matrix elements */
     err  /= (m*n) ; /* average absolute error per matrix element */
     if( err >= 1.e-5*amag || !IS_GOOD_FLOAT(err) ){
       fprintf(stderr,"SVD avg err=%g; recomputing ...",err) ;

#if 1     /* mangle all zero columns */
       { double arep=1.e-11*amag , *aj ;
         for( j=0 ; j < nn ; j++ ){
           aj = aa + j*mm ;
           for( i=0 ; i < mm ; i++ ) if( aj[i] != 0.0 ) break ;
           if( i == mm ){
ININFO_message("mangling all zero column %d",j) ;
             for( i=0 ; i < mm ; i++ ) aj[i] = (drand48()-0.5)*arep ;
           }
         }
       }
#endif

       /* svd_slow is compiled without optimization */

       (void) svd_slow_( &mm , &nn , &lda , aa , ww ,
                         &matu , &ldu , uu , &matv , &ldv , vv , &ierr , rv1 ) ;
       err = 0.0 ;
       for( j=0 ; j < n ; j++ ){
        for( i=0 ; i < m ; i++ ){
          aij = A(i,j) ;
          for( k=0 ; k < n ; k++ ) aij -= U(i,k)*V(j,k)*ww[k] ;
          err += fabs(aij) ;
       }}
       err /= (m*n) ;
       fprintf(stderr," new avg err=%g %s\n",
               err , (err >= 1.e-5*amag || !IS_GOOD_FLOAT(err)) ? "**BAD**" : "**OK**" ) ;
     }
   }
#endif

   free((void *)rv1) ;

   /* discard [u] and [v] spaces if not needed for output */

   if( u == NULL && uu != NULL ) free((void *)uu) ;
   if( v == NULL && vv != NULL ) free((void *)vv) ;

   /*--- 10 Jan 2007: sort the singular values and columns of U and V ---*/

   if( n > 1 && svd_sort != 0 ){
     double *sv ; int *iv , jj,kk ;
     sv = (double *)malloc(sizeof(double)*n) ;
     iv = (int *)   malloc(sizeof(int)   *n) ;
     for( kk=0 ; kk < n ; kk++ ){
       iv[kk] = kk ; sv[kk] = (svd_sort > 0) ? s[kk] : -s[kk] ;
     }
ININFO_message("sorting sv") ;
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
     qsort_doubleint( n , sv , iv ) ;
ININFO_message(" -- sorted") ;
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
     if( u != NULL ){
       double *cc ;
ININFO_message("malloc-ing cc: n=%d",n) ;
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
       cc = (double *)calloc(sizeof(double),m*n) ;
ININFO_message("copying u") ;
       AAmemcpy( cc , u , sizeof(double)*m*n ) ;
       for( jj=0 ; jj < n ; jj++ ){
ININFO_message(" u[%d] <- cc[%d]",jj,kk) ;
         kk = iv[jj] ;  /* where the new jj-th col came from */
         AAmemcpy( u+jj*m , cc+kk*m , sizeof(double)*m ) ;
       }
ININFO_message("freeing cc") ;
       free((void *)cc) ;
ININFO_message("cc is freed") ;
     }
     if( v != NULL ){
       double *cc ;
ININFO_message("malloc-ing cc: n=%d",n) ;
ININFO_message("MCW_MALLOC_status = %s",MCW_MALLOC_status) ;
       cc = (double *)calloc(sizeof(double),n*n) ;
ININFO_message("copying cc <- v") ; 
       AAmemcpy( cc , v , sizeof(double)*n*n ) ;
       for( jj=0 ; jj < n ; jj++ ){
         kk = iv[jj] ;
         AAmemcpy( v+jj*n , cc+kk*n , sizeof(double)*n ) ;
       }
       free((void *)cc) ;
     }
ININFO_message("getting s back") ;
     for( kk=0 ; kk < n ; kk++ )
       s[kk] = (svd_sort > 0) ? sv[kk] : -sv[kk] ;
     free((void *)iv) ; free((void *)sv) ;
   }

   return ;
}
