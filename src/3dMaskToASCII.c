#include "mrilib.h"

int main( int argc , char *argv[] )
{
   bytevec *bvec ;
   int nmask ; char *mask , *str ; byte *mbin ;
   int iarg=1 ; int tob64=1 ;

   if( argc < 2 ){
     printf(
      "Usage: 3dMaskToASCII [-tobin] dataset > outputfile\n"
      "\n"
      "This program reads as input a byte-valued 0/1 dataset, such as\n"
      "produced by 3dAutomask, and turns it into an ASCII string.\n"
      "This string can be used to specify a mask in a few places\n"
      "in AFNI, and will be allowed in more as time goes on.\n"
      "\n"
      "OPTION:\n"
      " -tobin = read 'dataset' as an ASCII string mask, expand it,\n"
      "          and write the byte-valued mask to stdout.  This file\n"
      "          corresponds to the .BRIK file of an AFNI dataset.\n"
      "          The information needed to create a .HEAD file isn't\n"
      "          stored in the ASCII string.\n"
      "\n"
      "* Jul 2010: -STATmask options in 3dREMLfit and 3dDeconvolve\n"
      "            accept a dataset mask or an ASCII string mask.\n"
      "\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   /*-----*/

   if( argv[iarg][0] == '-' ){
     if( strcmp(argv[iarg],"-tobin") == 0 ) tob64 = 0 ;
     iarg++ ;
   }

   /*-----*/

   bvec = THD_create_mask_from_string(argv[iarg]) ;
   if( bvec == NULL || bvec->nar < 1 ) ERROR_exit("Can't read mask :-(") ;

   if( tob64 ){   /* convert to string (default operation) */

     str = mask_to_b64string( bvec->nar , bvec->ar ) ;
     if( str == NULL ) ERROR_exit("Can't create ASCII string :-(") ;
     printf("%s\n",str) ;

   } else {       /* write byte mask directly out */

     if( isatty(fileno(stdout)) )
       ERROR_exit("Can't write byte mask to terminal directly") ;
     fwrite(bvec->ar,sizeof(byte),bvec->nar,stdout) ;
     INFO_message("Wrote %d bytes to re-directed stdout",bvec->nar) ;

   }

   exit(0) ;
}
