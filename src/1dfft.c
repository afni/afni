/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#define UMIN 4

int main( int argc , char * argv[] )
{
   MRI_IMAGE * inim , * outim ;
   int ii , jj , nx,nfft=0,ny , nopt,nby2 , ignore=0,nxi , use=0 ;
   complex * cxar ;
   float * iar , * oar , * far ;

   /*-- help? --*/

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dfft [options] infile outfile\n"
            "where infile is an AFNI *.1D file (ASCII list of numbers arranged\n"
            "in columns); outfile will be a similar file, with the absolute\n"
            "value of the FFT of the input columns.  The length of the file\n"
            "will be the FFT length/2.\n"
            "\n"
            "Options:\n"
            "  -ignore sss = Skip the first 'sss' lines in the input file.\n"
            "                [default = no skipping]\n"
            "  -use uuu    = Use only 'uuu' lines of the input file.\n"
            "                [default = use them all, Frank]\n"
            "  -nfft nnn   = Set FFT length to 'nnn'.\n"
            "                [default = length of data (# of lines used)]\n"
            "\n"
            "Nota Bene:\n"
            " * Each input time series has any quadratic trend of the\n"
            "     form 'a+b*t+c*t*t' removed before the FFT, where 't'\n"
            "     is the line number.\n"
            " * The FFT length will be a power-of-2 times at most one\n"
            "     factor of 3 and one factor of 5.  The smallest such\n"
            "     length >= to the specified FFT length will be used.\n"
            " * If the FFT length is longer than the file length, the\n"
            "     data is zero-padded to make up the difference.\n"
            " * Do NOT call the output of this program the Power Spectrum!\n"
            "     That is something else entirely.\n"
           ) ;
      exit(0) ;
   }

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-nfft") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"** Need argument after -nfft!\n");exit(1);
         }
         nfft = strtol( argv[nopt] , NULL , 10 ) ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-ignore") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"** Need argument after -ignore!\n");exit(1);
         }
         ignore = strtol( argv[nopt] , NULL , 10 ) ;
         if( ignore < 0 ){
            fprintf(stderr,"** Illegal value after -ignore!\n");exit(1);
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-use") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"** Need argument after -use!\n");exit(1);
         }
         use = strtol( argv[nopt] , NULL , 10 ) ;
         if( use < UMIN ){
            fprintf(stderr,"** Illegal value after -use!\n");exit(1);
         }
         nopt++ ; continue ;
      }

      fprintf(stderr,"** Unknown option: %s\n",argv[nopt]) ; exit(1) ;
   }

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

   nx = inim->nx ; ny = inim->ny ;
   nxi = nx - ignore ;
   if( use > 0 ){
      if( use > nxi ){
         fprintf(stderr,
                 "** Not enough data in file for ignore=%d and use=%d\n",
                 ignore , use ) ;
         exit(1) ;
      }
      nxi = use ;
   } else if( nxi < UMIN ){
      fprintf(stderr,"** Input file has too few rows!\n"); exit(1);
   }

   if( nfft < nxi ) nfft = nxi ;
   nfft = csfft_nextup_one35(nfft) ;
   fprintf(stderr,"++ FFT length = %d\n",nfft) ;
   nby2 = nfft/2 ;

   cxar  = (complex *) malloc( sizeof(complex) * nfft ) ;
   outim = mri_new( nby2 , ny , MRI_float ) ;
   iar   = MRI_FLOAT_PTR(inim) ;
   oar   = MRI_FLOAT_PTR(outim) ;

   for( jj=0 ; jj < ny ; jj++ ){
      far = iar + jj*nx ;
      THD_quadratic_detrend( nx , far , NULL,NULL,NULL ) ;
      for( ii=0   ; ii < nxi  ; ii++ ){
         cxar[ii].r = far[ii+ignore]; cxar[ii].i = 0.0;
      }
      for( ii=nxi ; ii < nfft ; ii++ ){ cxar[ii].r = cxar[ii].i = 0.0; }
      csfft_cox( -1 , nfft , cxar ) ;
      far = oar + jj*nby2 ;
      for( ii=0   ; ii < nby2 ; ii++ ){ far[ii] = CABS(cxar[ii+1]) ; }
   }

   mri_write_1D( argv[nopt+1] , outim ) ;
   exit(0) ;
}
