#include "mrilib.h"

#ifdef USE_OMP
#include <omp.h>
#endif

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
     " -prefix pp     =  The string 'pp' is the root prefix for the output datasets.\n"
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
   byte *mask=NULL ; int mask_nx=0,mask_ny=0,mask_nz=0,nmask=0 ;
   int nx,ny,nz,nt,nvox , nfft=0 , rdim=0 , kk,ii , nbad=0 ;
   int ndset=0,nim=0,nvim=0 , ndim=0 ;
   char *prefix="dimred" ;
   MRI_IMARR *imarc ;
   XtPointer_array *dsar ;
   THD_3dim_dataset *iset,*kset ; MRI_IMAGE *tim ;

   if( argc < 2 || strcasecmp(argv[1],"-help") == 0 ) DR_syntax() ;

   /*-- startup --*/

   mainENTRY("3dDimRed"); machdep();
   AFNI_logger("3dDimRed",argc,argv);
   PRINT_VERSION("3dDimRed"); AUTHOR("Uncle John's Band");

   INIT_XTARR(dsar) ; INIT_IMARR(imarc) ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcmp(argv[nopt],"-") == 0 ){ nopt++ ; continue ; }

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
       if( nmask < 1 ) ERROR_exit("Mask is too small to process") ;
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

   /* check datasets for match */

   if( ndset > 0 ){
     iset = (THD_3dim_dataset *)XTARR_XT(dsar,0) ;
     nx = DSET_NX(iset); ny = DSET_NY(iset); nz = DSET_NZ(iset); nvox = nx*ny*nz;
     nt = DSET_NVALS(iset) ;
     if( nt < 2 ){
       ERROR_message("Dataset %s has only 1 point in the time direction!",DSET_HEADNAME(iset)) ;
       nbad++ ;
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
   } else {
     tim = IMARR_SUBIM(imarc,0) ;
     nvox = 1 ; nt = tim->nx ;
     if( nt < 2 ){
       ERROR_message("1D file %s has only 1 time point!",tim->name) ;
       nbad++ ;
     }
   }

   if( nim > 0 ){
     for( kk=0 ; kk < nim ; kk++ ){
       tim = IMARR_SUBIM(imarc,kk) ;
       if( tim->nx != nt ){
         ERROR_message("1D file %s has %d time points, but should have %d",
                       tim->name , tim->nx , nt ) ;
         nbad++ ;
       }
       nvim += tim->ny ;
     }
   }

   if( nbad > 0 )
     ERROR_exit("Can't continue after above ERRORs") ;

   ndim = ndset + nvim ;
   if( ndim <= rdim )
     ERROR_exit("Input dimension = %d is not bigger than output = %d",ndim,rdim) ;

   exit(0) ;
}
