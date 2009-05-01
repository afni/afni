#include "mrilib.h"

int main( int argc , char * argv[] )
{
   MRI_IMAGE *inim , *ortim=NULL ;
   float **vec , **ort=NULL ; int iv , nx,ny , nopt, nort=0 , do_norm=0 ;
   int qdet=1 ; float dt=1.0f , fbot,ftop ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dBandpass [options] fbot ftop infile\n"
            "\n"
            " * infile is an AFNI *.1D file; each column is processed\n"
            " * fbot = lowest frequency in the passband, in Hz\n"
            "          [can be 0 if you want to do a lowpass filter only,]\n"
            "           but the mean and Nyquist freq are always removed ]\n"
            " * ftop = highest frequency in the passband (must be > fbot)\n"
            "          [if ftop > Nyquist freq, then we have a highpass filter only]\n"
            " * Output vectors appear on stdout; redirect as desired\n"
            " * Program will fail if fbot and ftop are too close for comfort\n"
            " * The actual FFT length used will be printed, and may be larger\n"
            "   than the input time series length for the sake of efficiency.\n"
            "\n"
            "Options:\n"
            "  -dt dd     = set time step to 'dd' sec [default = 1.0]\n"
            "  -ort f.1D  = Also orthogonalize input to columns in f.1D\n"
            "               [only one '-ort' option is allowed]\n"
            "  -nodetrend = Skip the quadratic detrending of the input\n"
            "  -norm      = Make output time series have L2 norm = 1\n"
            "\n"
            "Example:\n"
            "  1deval -num 1000 -expr 'gran(0,1)' > r1000.1D\n"
            "  1dBandpass 0.025 0.20 r1000.1D  > f1000.1D\n"
            "  1dfft f1000.1D - | 1dplot -del 0.000977 -stdin -plabel 'Filtered |FFT|'\n"
            "\n"
            "Goal:\n"
            " * Mostly to test the functions in thd_bandpass.c -- RWCox -- May 2009\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcmp(argv[nopt],"-norm") == 0 ){
       do_norm = 1 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-ort") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after -ort!") ;
       if( ortim != NULL ) ERROR_exit("can't have 2 -ort options!") ;
       ortim = mri_read_1D( argv[nopt] ) ;
       if( ortim == NULL ) ERROR_exit("can't read from -ort '%s'",argv[nopt]) ;
       nopt++ ; continue ;
     }

     if( strncmp(argv[nopt],"-nodetrend",6) == 0 ){
       qdet = 0 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-dt") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("need an argument after -dt!") ;
       dt = (float)strtod(argv[nopt],NULL) ;
       if( dt <= 0.0f ){
         WARNING_message("value after -dt illegal: setting time step to 1.0") ;
         dt = 1.0f ;
       }
       nopt++ ; continue ;
     }

      fprintf(stderr,"** Unknown option: %s\n",argv[nopt]) ; exit(1) ;
   }

   /** check inputs for reasonablositiness **/

   if( nopt+2 >= argc ) ERROR_exit("Need more arguments on command line!") ;

   fbot = (float)strtod(argv[nopt++],NULL) ;
   if( fbot < 0.0f ) ERROR_exit("fbot value can't be negative!") ;

   ftop = (float)strtod(argv[nopt++],NULL) ;
   if( ftop <= fbot ) ERROR_exit("ftop value must be greater than fbot value!") ;

   inim = mri_read_1D(argv[nopt]) ;
   if( inim == NULL ) ERROR_exit("Can't read 1D file '%s'",argv[nopt]) ;
   nx = inim->nx ; ny = inim->ny ;
   if( nx < 9 ) ERROR_exit("1D file '%s' has only %d lines",argv[nopt],nx) ;

   if( !THD_bandpass_OK(nx,dt,fbot,ftop,1) ) ERROR_exit("Can't continue!") ;

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

   THD_bandpass_vectors( nx , ny , vec , dt , fbot,ftop , qdet , nort , ort ) ;

   if( do_norm ){
     for( iv=0 ; iv < ny ; iv++ ) THD_normalize( nx , vec[iv] ) ;
   }

   /* write to stdout */

   mri_write_1D( "-" , inim ) ;
   exit(0) ;
}
