/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

int main( int argc , char * argv[] )
{
   MRI_IMAGE * inim ;
   int ii , jj , nx,ny , nopt;
   float *iar ;
   double sq ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dnorm infile outfile\n"
            "where infile is an AFNI *.1D file (ASCII list of numbers arranged\n"
            "in columns); outfile will be a similar file, with each column being\n"
            "L2 normalized.\n"
            "* If 'infile'  is '-', it will be read from stdin.\n"
            "* If 'outfile' is '-', it will be written to stdout.\n"
            "* This program has no options!\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   nopt = 1 ;
   if( nopt+1 >= argc )
     ERROR_exit(" Need input and output filenames!") ;

   if( argc > nopt+1 && !THD_filename_ok(argv[nopt+1]) )
     ERROR_exit(" Illegal output filename!") ;

   if( argc > nopt+1 && strcmp(argv[nopt+1],"-") != 0 && THD_is_file(argv[nopt+1]) )
     ERROR_exit("Output file already exists!") ;

   /* read input file */

   inim = mri_read_1D( argv[nopt] ) ;
   if( inim == NULL )
     ERROR_exit("Can't read input file!") ;

   nx = inim->nx ; ny = inim->ny ; iar = MRI_FLOAT_PTR(inim) ;

   for( jj=0 ; jj < ny ; jj++ ){
     THD_normalize( nx , iar + jj*nx ) ;
   }

   mri_write_1D( (argc > nopt+1) ? argv[nopt+1] : "-" , inim ) ;
   exit(0) ;
}
