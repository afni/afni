#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *inim ,  *outim ;
   float     *inar ,  *outar ;
   int      nxin   , nxout   , ny ;
   int iarg=1 , jj , nup ;

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
     printf("Program 1dUpsample:\n"
            "Upsamples a 1D time series (along the column direction)\n"
            "to a finer time grid.\n"
            "Usage:  1dUpsample n fred.1D > ethel.1D\n"
            "\n"
            "Where 'n' is the upsample factor (integer from 2..32)\n"
            "\n"
            "Notes:\n"
            "* Interpolation is done with 7th order polynomials.\n"
            "* This program is a quick hack for Gang Chen.\n"
     "\n") ;
     exit(0) ;
   }

   nup = (int)strtod(argv[iarg++],NULL) ;
   if( nup < 2 || nup > 32 )
     ERROR_exit("1dUpsample 'n' parameter = %d is out of range 2..32",nup) ;

   inim = mri_read_1D(argv[iarg]) ;
   if( inim == NULL )
     ERROR_exit("1dUpsample can't read file '%d'",argv[iarg]) ;
   if( inim->nx < 2 )
     ERROR_exit("1dUpsample doesn't like file '%d'",argv[iarg]) ;

   nxin = inim->nx ; ny = inim->ny ; inar = MRI_FLOAT_PTR(inim) ;
   nxout = nxin * nup ;
   outim = mri_new( nxout , ny , MRI_float ) ;
   outar = MRI_FLOAT_PTR(outim) ;

   for( jj=0 ; jj < ny ; jj++ ){
     upsample_7( nup , nxin , inar + nxin*jj , outar + nxout*jj ) ;
   }

   mri_write_1D( "-" , outim ) ;
   exit(0) ;
}
