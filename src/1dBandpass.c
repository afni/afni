#include "mrilib.h"

int main( int argc , char * argv[] )
{
   MRI_IMAGE *inim ;
   float **vec ; int iv , nx,ny , jbot,jtop , nopt,nfft ;
   int qdet=1 ; float dt=1.0f , fbot,ftop , df ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dBandpass [options] fbot ftop infile\n"
            "* infile is an AFNI *.1D file,\n"
            "* fbot = lowest frequency in the passband (can be 0)\n"
            "* ftop = highest frequency in the passband (must be > fbot)\n"
            "* Output on stdout\n"
            "Options:\n"
            "  -dt dd      = set time step to 'dd' sec [default = 1.0]\n"
            "  -nodetrend  = Skip the detrending of the input.\n"
            "\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strncmp(argv[nopt],"-nodetrend",6) == 0 ){ /* 29 Nov 1999 */
       qdet = 0 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-dt") == 0 ){
       if( nopt >= argc ) ERROR_exit("need an argument after -dt!") ;
       dt = (float)strtod(argv[nopt],NULL) ;
       if( dt <= 0.0f ){
         WARNING_message("value after -dt illegal: setting time step to 1.0") ;
         dt = 1.0f ;
       }
       nopt++ ; continue ;
     }

      fprintf(stderr,"** Unknown option: %s\n",argv[nopt]) ; exit(1) ;
   }

   if( nopt+2 >= argc ) ERROR_exit("Need more arguments on command line!") ;

   fbot = (float)strtod(argv[nopt++],NULL) ;
   if( fbot < 0.0f ) ERROR_exit("fbot value can't be negative!") ;
   ftop = (float)strtod(argv[nopt++],NULL) ;
   if( ftop <= fbot ) ERROR_exit("ftop value must be greater than fbot value!") ;
   inim = mri_read_1D(argv[nopt]) ;
   if( inim == NULL ) ERROR_exit("Can't read 1D file '%s'",argv[nopt]) ;
   nx = inim->nx ; ny = inim->ny ;
   if( nx < 9 ) ERROR_exit("1D file '%s' has only %d lines",argv[nopt],nx) ;

   nfft = csfft_nextup_one35(nx) ;
   df   = 1.0f / (nfft * dt) ;
   jbot = (int)rint(fbot/df) ;
   jtop = (int)rint(ftop/df) ;
   if( jbot+1 >= jtop )
     ERROR_exit("fbot and ftop too close: jbot=%d jtop=%d",jbot,jtop) ;
   else
     INFO_message("nfft=%d df=%.6f passband index range=%d .. %d",nfft,df,jbot,jtop) ;

   vec = (float **)malloc(sizeof(float *)*ny) ;
   for( iv=0 ; iv < ny ; iv++ ) vec[iv] = MRI_FLOAT_PTR(inim) + iv*nx ;

   THD_bandpass_vectors( nx , ny , vec , dt , fbot,ftop , qdet ) ;

   mri_write_1D( "-" , inim ) ;
   exit(0) ;
}
