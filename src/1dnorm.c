/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

int main( int argc , char * argv[] )
{
   MRI_IMAGE * inim ;
   int ii , jj , nx,ny , nopt;
   float * iar ;
   double sq ;

   /*-- help? --*/

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dnorm infile outfile\n"
            "where infile is an AFNI *.1D file (ASCII list of numbers arranged\n"
            "in columns); outfile will be a similar file, with each column being\n"
            "L2 normalized.\n"
           ) ;
      exit(0) ;
   }

   nopt = 1 ;
   if( nopt+1 >= argc ){
      fprintf(stderr,"** Need input and output filenames!\n");exit(1);
   }

   if( !THD_filename_ok(argv[nopt+1]) ){
      fprintf(stderr,"** Illegal output filename!\n"); exit(1);
   }
   if( THD_is_file(argv[nopt+1]) ){
      fprintf(stderr,"** Output file already exists!\n"); exit(1);
   }

   /* read input file */

   inim = mri_read_1D( argv[nopt] ) ;
   if( inim == NULL ){
      fprintf(stderr,"** Can't read input file!\n"); exit(1);
   }

   nx = inim->nx ; ny = inim->ny ; iar = MRI_FLOAT_PTR(inim) ;

   for( jj=0 ; jj < ny ; jj++ ){
      sq = 0.0 ;
      for( ii=0 ; ii < nx ; ii++ ) sq += SQR(iar[ii+jj*nx]) ;
      if( sq > 0.0 ){
         sq = 1.0 / sq ;
         for( ii=0 ; ii < nx ; ii++ ) iar[ii+jj*nx] *= sq ;
      }
   }

   mri_write_1D( argv[nopt+1] , inim ) ;
   exit(0) ;
}
