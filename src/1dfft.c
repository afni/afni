/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#define UMIN 4

#define TOCX   1
#define FROMCX 2

int main( int argc , char * argv[] )
{
   MRI_IMAGE * inim , * outim ;
   int ii , jj , nx,nfft=0,ny , nopt,nby2 , ignore=0,nxi , use=0 ;
   complex * cxar ;
   float * iar , * oar , * far ;
   int nodetrend=0 , cxop=0 ;     /* 29 Nov 1999 */
   int hilbert=0 ;                /* 09 Dec 1999 */

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dfft [options] infile outfile\n"
            "where infile is an AFNI *.1D file (ASCII list of numbers arranged\n"
            "in columns); outfile will be a similar file, with the absolute\n"
            "value of the FFT of the input columns.  The length of the file\n"
            "will be 1+(FFT length)/2.\n"
            "\n"
            "Options:\n"
            "  -ignore sss = Skip the first 'sss' lines in the input file.\n"
            "                [default = no skipping]\n"
            "  -use uuu    = Use only 'uuu' lines of the input file.\n"
            "                [default = use them all, Frank]\n"
            "  -nfft nnn   = Set FFT length to 'nnn'.\n"
            "                [default = length of data (# of lines used)]\n"
            "  -tocx       = Save Re and Im parts of transform in 2 columns.\n"
            "  -fromcx     = Convert 2 column complex input into 1 column\n"
            "                  real output.\n"
            "  -hilbert    = When -fromcx is used, the inverse FFT will\n"
            "                  do the Hilbert transform instead.\n"
            "  -nodetrend  = Skip the detrending of the input.\n"
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
            " * If 'outfile' is '-', the output appears on stdout.\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strncmp(argv[nopt],"-nodetrend",6) == 0 ){ /* 29 Nov 1999 */
         nodetrend++ ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-hilbert") == 0 ){      /* 09 Dec 1999 */
         hilbert = 1 ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-tocx") == 0 ){         /* 29 Nov 1999 */
         cxop = TOCX ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-fromcx") == 0 ){       /* 29 Nov 1999 */
         cxop = FROMCX ;
         nopt++ ; continue ;
      }

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

   if( nopt >= argc ){
      fprintf(stderr,"** Need input filenames!\n");exit(1);
   }

   if( argc > nopt+1 && !THD_filename_ok(argv[nopt+1]) ){
     fprintf(stderr,"** Illegal output filename!\n"); exit(1);
   }
   if( argc > nopt+1 && strcmp(argv[nopt+1],"-") != 0 && THD_is_file(argv[nopt+1]) ){
     fprintf(stderr,"** Output file already exists!\n"); exit(1);
   }

   if( hilbert && cxop != FROMCX ){
      fprintf(stderr,"** -hilbert is illegal without -fromcx!\n"); exit(1);
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

   /*- 29 Nov 1999: complex operations are finicky -*/

   switch( cxop ){
      case TOCX:
         if( ny != 1 ){
            fprintf(stderr,
                    "** -tocx option only works on 1 column files!\n") ;
            exit(1) ;
         }
      break ;

      case FROMCX:
         if( ny != 2 ){
            fprintf(stderr,
                    "** -fromcx option only works on 2 column files!\n") ;
            exit(1) ;
         }
         if( nx != nxi ){
            fprintf(stderr,
                   "** -fromcx option doesn't allow -ignore or -use!\n") ;
            exit(1) ;
         }
         jj = csfft_nextup_one35( 2*(nx-1) ) ;
         if( jj != 2*(nx-1) ){
            fprintf(stderr,
                    "** -fromcx only works with certain file lengths!\n") ;
            exit(1) ;
         }
      break ;
   }

   /* 29 Nov 1999: two possible paths */

   if( cxop != FROMCX ){                     /* real input */
      if( nfft < nxi ) nfft = nxi ;
      nfft = csfft_nextup_one35(nfft) ;
      fprintf(stderr,"++ 1dfft length = %d\n",nfft) ;
      nby2 = nfft/2 ;

      cxar = (complex *) malloc( sizeof(complex) * nfft ) ;
      if( cxop == TOCX )
         outim = mri_new( nby2+1 , 2 , MRI_float ) ;   /* complex output */
      else
         outim = mri_new( nby2+1 , ny , MRI_float ) ;  /* abs() output */

      iar = MRI_FLOAT_PTR(inim) ;
      oar = MRI_FLOAT_PTR(outim) ;

      for( jj=0 ; jj < ny ; jj++ ){
         far = iar + jj*nx ;
         if( !nodetrend ){
            float f0,f1,f2 ;
            THD_quadratic_detrend( nxi , far+ignore , &f0,&f1,&f2 ) ;
#if 0
            fprintf(stderr,"++ quadratic trend: %g + %g * i + %g * i*i\n"
                           "                    mid = %g  end = %g\n" ,
                    f0,f1,f2 , f0+0.5*nxi*f1+0.25*nxi*nxi*f2 ,
                               f0+(nxi-1)*f1+(nxi-1)*(nxi-1)*f2 ) ;
#endif
         }
         for( ii=0 ; ii < nxi ; ii++ ){
            cxar[ii].r = far[ii+ignore]; cxar[ii].i = 0.0;
         }
         for( ii=nxi ; ii < nfft ; ii++ ){ cxar[ii].r = cxar[ii].i = 0.0; }
         csfft_cox( -1 , nfft , cxar ) ;
         far = oar + jj*(nby2+1) ;
         if( cxop == TOCX )
            for( ii=0 ; ii <= nby2 ; ii++ ){ far[ii]          = cxar[ii].r ;
                                             far[ii+(nby2+1)] = cxar[ii].i ; }
         else
            for( ii=0 ; ii <= nby2 ; ii++ ){ far[ii] = CABS(cxar[ii]) ; }
      }

   } else {   /* complex input */
      nfft = 2*(nx-1) ;
      nby2 = nfft/2 ;
      fprintf(stderr,"++ FFT length = %d\n",nfft) ;
      cxar = (complex *) malloc( sizeof(complex) * nfft ) ;

      outim = mri_new( nfft , 1 , MRI_float ) ;

      iar = MRI_FLOAT_PTR(inim) ;
      oar = MRI_FLOAT_PTR(outim) ;

      cxar[0].r    = iar[0]    ; cxar[0].i    = 0.0 ;
      cxar[nby2].r = iar[nx-1] ; cxar[nby2].i = 0.0 ;
      for( ii=1 ; ii < nby2 ; ii++ ){
         cxar[ii].r      = iar[ii] ; cxar[ii].i      =  iar[ii+nx] ;
         cxar[nfft-ii].r = iar[ii] ; cxar[nfft-ii].i = -iar[ii+nx] ;
      }

      if( hilbert ){
         float val ;
         cxar[0].r = cxar[0].i = cxar[nby2].r = cxar[nby2].i = 0.0 ;
         for( ii=1 ; ii < nby2 ; ii++ ){
            val = cxar[ii].r      ; cxar[ii].r      = -cxar[ii].i ;
                                    cxar[ii].i      =  val ;
            val = cxar[nfft-ii].r ; cxar[nfft-ii].r =  cxar[nfft-ii].i ;
                                    cxar[nfft-ii].i = -val ;
         }
      }
      csfft_cox( 1 , nfft , cxar ) ;
      for( ii=0 ; ii < nfft ; ii++ ) oar[ii] = cxar[ii].r / nfft ;
   }

   mri_write_1D( (argc > nopt+1) ? argv[nopt+1] : "-" , outim ) ;
   exit(0) ;
}
