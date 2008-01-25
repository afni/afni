#include "mrilib.h"

int main( int argc , char *argv[] )
{
   MRI_IMAGE *im ;
   int dec=0 , flip=0 , iarg ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: 1dTsort [options] file.1D\n"
      "Sorts each column of the input 1D file and writes result to stdout.\n"
      "\n"
      "Options\n"
      "-------\n"
      " -inc     = sort into increasing order [default]\n"
      " -dec     = sort into decreasing order\n"
      " -flip    = transpose the file before OUTPUT\n"
      "            * the INPUT can be transposed using file.1D\\'\n"
      "            * thus, to sort each ROW, do something like\n"
      "               1dTsort -flip file.1D\\' > sfile.1D\n"
      "\n"
      "N.B.: Data will be read from standard input if the filename IS stdin,\n"
      "      and will also be row/column transposed if the filename is stdin\\'\n"
      "      For example:\n"
      "        1deval -num 100 -expr 'uran(1)' | 1dTsort stdin | 1dplot stdin\n"
      "\n" ) ;
      exit(0) ;
   }

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-inc") == 0 ){
       dec = 0 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-dec") == 0 ){
       dec = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-flip") == 0 ){
       flip = 1 ; iarg++ ; continue ;
     }

     ERROR_exit("Unknown option '%s'",argv[iarg]) ;
   }

   if( iarg >= argc ) ERROR_exit("No 1D file on command line?!") ;

   im = mri_read_1D( argv[iarg] ) ;
   if( im == NULL ) ERROR_exit("Can't read 1D file %s",argv[iarg]) ;

   mri_xsort_inplace( im , dec ) ;

   if( flip ){
     MRI_IMAGE *qim=mri_transpose(im) ;
     mri_free(im) ; im = qim ;
   }

   mri_write_1D( "-" , im ) ; mri_free(im) ; exit(0) ;
}
