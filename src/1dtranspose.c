/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

int main( int argc , char * argv[] )
{
   MRI_IMAGE * inim ;

   /*-- help? --*/

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dtranspose infile outfile\n"
            "where infile is an AFNI *.1D file (ASCII list of numbers arranged\n"
            "in columns); outfile will be a similar file, but transposed.\n"
            "You can use a column subvector selector list on infile, as in\n"
            "  1dtranspose 'fred.1D[0,3,7]' ethel.1D\n"
            "\n"
            "N.B.: This program may produce files with lines longer than a\n"
            "      text editor can handle.\n"
           ) ;
      exit(0) ;
   }

   if( !THD_filename_ok(argv[2]) ){
      fprintf(stderr,"** Illegal output filename!\n"); exit(1);
   }
   if( THD_is_file(argv[2]) ){
      fprintf(stderr,"** Output file already exists!\n"); exit(1);
   }

   /* read input file */

   inim = mri_read_1D( argv[1] ) ;
   if( inim == NULL ){
      fprintf(stderr,"** Can't read input file!\n"); exit(1);
   }

   mri_write_ascii( argv[2] , inim ) ;
   exit(0) ;
}
