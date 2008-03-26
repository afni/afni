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
   int mode=2 ;  /* 26 Mar 2008 */

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dnorm infile outfile\n"
            "where infile is an AFNI *.1D file (ASCII list of numbers arranged\n"
            "in columns); outfile will be a similar file, with each column being\n"
            "L_2 normalized (sum of squares = 1).\n"
            "* If 'infile'  is '-', it will be read from stdin.\n"
            "* If 'outfile' is '-', it will be written to stdout.\n"
            "\n"
            "Options:\n"
            "--------\n"
            " -norm1  = Normalize so sum of absolute values is 1 (L_1 norm)\n"
            " -normx  = So that max absolute value is 1 (L_infinity norm)\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   nopt = 1 ;

   if( strncmp(argv[nopt],"-norm",5) == 0 ){       /* 26 Mar 2008 */
          if( argv[nopt][5] == '1' ) mode = 1 ;
     else if( argv[nopt][5] == 'x' ) mode = 666 ;
     else if( argv[nopt][5] == '2' ) mode = 2 ;
     else ERROR_message("Don't understand option '%s'",argv[nopt]) ;
     nopt++ ;
   }

   if( nopt+1 >= argc )
     ERROR_exit("Need input and output filenames!") ;

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
     switch( mode ){
       default:  THD_normalize( nx , iar + jj*nx ); break;
       case 1:   THD_normL1   ( nx , iar + jj*nx ); break;
       case 666: THD_normmax  ( nx , iar + jj*nx ); break;
     }
   }

   mri_write_1D( (argc > nopt+1) ? argv[nopt+1] : "-" , inim ) ;
   exit(0) ;
}
