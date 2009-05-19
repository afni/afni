#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int do_norm=0 , qdet=1 , have_freq=0 , do_automask=0 ;
   float dt=0.0f , fbot=0.0f,ftop=999999.9f , blur=0.0f ;
   MRI_IMARR *ortar=NULL ; MRI_IMAGE *ortim=NULL ;
   THD_3dim_dataset *inset=NULL ;
   char *prefix="bandpass" ;
   byte *mask=NULL ;
   int mask_nx,mask_ny,mask_nz,nmask , verb=1 , nx,ny,nz,nvox ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 3dBandpass [options] fbot ftop dataset\n"
            "\n"
            " * dataset is a 3D+time sequence of volumes\n"
            " * fbot = lowest frequency in the passband, in Hz\n"
            "          [can be 0 if you want to do a lowpass filter only,]\n"
            "          [but the mean and Nyquist freq are always removed!]\n"
            " * ftop = highest frequency in the passband (must be > fbot)\n"
            "          [if ftop > Nyquist freq, then we have a highpass filter only]\n"
            " * You cannot construct a 'notch' filter with this program!\n"
            " * Program will fail if fbot and ftop are too close for comfort.\n"
            " * The actual FFT length used will be printed, and may be larger\n"
            "    than the input time series length for the sake of efficiency.\n"
            "    [The program will use a power-of-2, possibly multiplied by]\n"
            "    [a single factor of 3 and/or a single factor of 5; e.g.,  ]
            "    [240=16*3*5 would be chosen if there are 239 time points. ]
            "\n"
            "Options:\n"
            "  -dt dd          = set time step to 'dd' sec [default=from dataset header]\n"
            "  -ort f.1D       = Also orthogonalize input to columns in f.1D\n"
            "                     [multiple '-ort' options are allowed].\n"
            "  -nodetrend      = Skip the quadratic detrending of the input that\n"
            "                     occurs before the FFT-based bandpassing.\n"
            "  -norm           = Make all output time series have L2 norm = 1.\n"
            "  -mask mset      = Mask dataset\n"
            "  -automask       = Create a mask from the input dataset\n"
            "  -blur fff       = Blur (inside the mask only) with a filter\n"
            "                     width (FWHM) of 'fff' millimeters.\n"
            "  -input dataset  = Alternative way to specify input dataset.\n"
            "  -band fbot ftop = Alternative way to specify passband frequencies.\n"
            "  -prefix ppp     = Set prefix name of output dataset.\n"
            "  -quiet          = Turn off informative messages.\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcmp(argv[nopt],"-blur") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after -blur!") ;
       blur = PARSER_strtod(argv[nopt]) ;
       if( blur <= 0.0f ) WARNING_message("non-positive blur?!") ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after -prefix!") ;
       prefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix) ) ERROR_exit("bad -prefix option!") ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-automask") == 0 ){
       if( mask != NULL ) ERROR_exit("Can't use -mask AND -automask!") ;
       do_automask = 1 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-mask") == 0 ){
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

     if( strcmp(argv[nopt],"-norm") == 0 ){
       do_norm = 1 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-quiet") == 0 ){
       verb = 0 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-ort") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after -ort!") ;
       if( ortar == NULL ) INIT_IMARR(ortar) ;
       ortim = mri_read_1D( argv[nopt] ) ;
       if( ortim == NULL ) ERROR_exit("can't read from -ort '%s'",argv[nopt]) ;
       ADDTO_IMARR(ortar,ortim) ;
       nopt++ ; continue ;
     }

     if( strncmp(argv[nopt],"-nodetrend",6) == 0 ){
       qdet = 0 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-dt") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after -dt!") ;
       dt = (float)PARSER_strtod(argv[nopt]) ;
       if( dt <= 0.0f ) WARNING_message("value after -dt illegal!") ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-input") == 0 ){
       if( inset != NULL ) ERROR_exit("Can't have 2 -input options!") ;
       if( ++nopt >= argc ) ERROR_exit("need an argument after -input!") ;
       inset = THD_open_dataset(argv[nopt]) ;
       CHECK_OPEN_ERROR(inset,argv[nopt]) ;
       nopt++ ; continue ;
     }

     if( strncmp(argv[nopt],"-band",5) == 0 ){
       if( ++nopt >= argc-1 ) ERROR_exit("need 2 arguments after -band!") ;
       fbot = (float)PARSER_strtod(argv[nopt++]) ;
       ftop = (float)PARSER_strtod(argv[nopt++]) ;
       have_freq = 1 ; continue ;
     }

     fprintf(stderr,"** Unknown option: %s\n",argv[nopt]) ; exit(1) ;
   }

   /** check inputs for reasonablositiness **/

   if( !have_freq ){
     if( nopt+1 >= argc ) ERROR_exit("Need frequencies on command line after options!") ;
     fbot = (float)PARSER_strtod(argv[nopt++]) ;
     ftop = (float)PARSER_strtod(argv[nopt++]) ;
   }

   if( inset == NULL ){
     if( nopt >= argc ) ERROR_exit("Need input dataset name on command line after options!") ;
     inset = THD_open_dataset(argv[nopt]) ;
     CHECK_OPEN_ERROR(inset,argv[nopt]) ; nopt++ ;
   }

   if( fbot < 0.0f  ) ERROR_exit("fbot value can't be negative!") ;
   if( ftop <= fbot ) ERROR_exit("ftop value must be greater than fbot value!") ;

   ntime = DSET_NVALS(inset) ;
   if( ntime < 9 ) ERROR_exit("Input dataset needs is too short!") ;

   if( dt <= 0.0f ){
     dt = DSET_TR(inset) ;
     if( dt <= 0.0f ){
       INFO_message("Setting dt=1.0 since input dataset lacks a time axis!") ;
       dt = 1.0f ;
     }
   }

   if( !THD_bandpass_OK(ntime,dt,fbot,ftop,1) ) ERROR_exit("Can't continue!") ;

   nx = DSET_NX(inset); ny = DSET_NY(inset); nz = DSET_NZ(inset); nvox = nx*ny*nz;

   if( mask != NULL ){
     if( mask_nx != nx || mask_ny != ny || mask_nz != nz )
       ERROR_exit("-mask dataset grid doesn't match input dataset") ;

   } else if( automask ){
     mask = THD_automask( inset ) ;
     if( mask == NULL )
       ERROR_message("Can't create -automask from input dataset?") ;
     nmask = THD_countmask( DSET_NVOX(inset) , mask ) ;
     if( verb ) INFO_message("Number of voxels in automask = %d",nmask);
     if( nmask < 1 ) ERROR_exit("Automask is too small to process") ;

   } else {
     mask = (byte *)malloc(sizeof(byte)*nvox) ; nmask = nvox ;
     memset(mask,1,sizeof(byte)*nvox) ;
     if( verb ) INFO_message("No mask ==> processing all %d voxels",nvox);
   }

   ?????

   /* make list of pointers to start of each input vector */

   vec = (float **)malloc(sizeof(float *)*ny) ;
   for( iv=0 ; iv < ny ; iv++ ) vec[iv] = MRI_FLOAT_PTR(inim) + iv*nx ;

   /* similarly for the ort vectors */

   if( ortim != NULL ){
     if( ortim->nx != nx )
       ERROR_exit("-ort file and input 1D file differ in column lengths!") ;
     nort = ortim->ny ;
     ort  = (float **)malloc(sizeof(float)*nort) ;
     for( iv=0 ; iv < nort ; iv++ ) ort[iv] = MRI_FLOAT_PTR(ortim) + iv*nx ;
   }

   /* all the real work now */

   (void)THD_bandpass_vectors( nx, ny, vec, dt, fbot,ftop, qdet, nort, ort ) ;

   if( do_norm ){
     for( iv=0 ; iv < ny ; iv++ ) THD_normalize( nx , vec[iv] ) ;
   }

   /* write to stdout */

   mri_write_1D( "-" , inim ) ;
   exit(0) ;
}
