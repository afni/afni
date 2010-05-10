/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

int main( int argc , char * argv[] )
{
   MRI_IMAGE * inim ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
       "Usage: 1dtranspose infile outfile\n"
       "where infile is an AFNI *.1D file (ASCII list of numbers arranged\n"
       "in columns); outfile will be a similar file, but transposed.\n"
       "You can use a column subvector selector list on infile, as in\n"
       "  1dtranspose 'fred.1D[0,3,7]' ethel.1D\n"
       "\n"
       "* This program may produce files with lines longer than a\n"
       "   text editor can handle.\n"
       "* If 'outfile' is '-' (or missing entirely), output goes to stdout.\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("1dtranspose"); machdep() ;

   if( argc > 2 && !THD_filename_ok(argv[2]) ){
      fprintf(stderr,"** Illegal output filename!\n"); exit(1);
   }
   if( !THD_ok_overwrite() && argc > 2 && strcmp(argv[2],"-") != 0 && 
         THD_is_file(argv[2])  ){
      fprintf(stderr,"** Output file already exists!\n"); exit(1);
   }

   /* read input file */

   inim = mri_read_1D( argv[1] ) ;
   if( inim == NULL ) ERROR_exit("Can't read input file '%s'",argv[1]) ;

#if 0
INFO_message("nx=%d ny=%d",inim->nx,inim->ny) ;
#endif

   mri_write_ascii( (argc>2) ? argv[2] : "-" , inim ) ;
   exit(0) ;
}
