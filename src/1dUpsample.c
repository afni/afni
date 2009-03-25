#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *inim ,  *outim ;
   float     *inar ,  *outar ;
   int      nxin   , nxout   , ny ;
   int iarg=1 , jj , nup , do_one=0 ;

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
     printf("Program 1dUpsample:\n"
            "Upsamples a 1D time series (along the column direction)\n"
            "to a finer time grid.\n"
            "Usage:  1dUpsample [options] n fred.1D > ethel.1D\n"
            "\n"
            "Where 'n' is the upsample factor (integer from 2..32)\n"
            "\n"
            "NOTES:\n"
            "------\n"
            "* Interpolation is done with 7th order polynomials.\n"
            "   (Why 7? It's a nice number, and the code already existed.)\n"
            "* The only option is '-1' or '-one', to use 1st order\n"
            "   polynomials instead (i.e., linear interpolation).\n"
            "* Output is written to stdout.\n"
            "* If you want to interpolate along the row direction,\n"
            "   transpose before input, then transpose the output.\n"
            "* Example:\n"
            "   1dUpsample 5 '1D: 4 5 4 3 4' | 1dplot -stdin -dx 0.2 \n"
            "* If the input has M time points, the output will\n"
            "   have n*M time points.  The last n-1 of them\n"
            "   will be past the end of the original time series.\n"
            "* This program is a quick hack for Gang Chen.\n"
            "   Where are my Twizzlers?\n"
     "\n") ;
     exit(0) ;
   }

   if( argv[iarg][0] == '-' ){ do_one = 1 ; iarg++ ; }  /* very cheap */

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
     if( do_one )
       upsample_1( nup , nxin , inar + nxin*jj , outar + nxout*jj ) ;
     else
       upsample_7( nup , nxin , inar + nxin*jj , outar + nxout*jj ) ;
   }

   mri_write_1D( "-" , outim ) ;
   exit(0) ;
}
