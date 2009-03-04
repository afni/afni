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
   float *iar , *qar , ss=0.0f ;
   int mode=2 , dem=0 ;  /* 26 Mar 2008 */

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: 1dnorm [options] infile outfile\n"
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
            "\n"
            " -demean = Subtract each column's mean before normalizing\n"
            " -demed  = Subtract each column's median before normalizing\n"
            "            [-demean and -demed are mutually exclusive!]\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   nopt = 1 ;

   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strncmp(argv[nopt],"-norm",5) == 0 ){       /* 26 Mar 2008 */
            if( argv[nopt][5] == '1' ) mode = 1 ;
       else if( argv[nopt][5] == 'x' ) mode = 666 ;
       else if( argv[nopt][5] == '2' ) mode = 2 ;
       else ERROR_message("Don't understand option '%s'",argv[nopt]) ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-demean") == 0 ){        /* 04 Mar 2009 */
       if( dem == 1 ) WARNING_message("-demean over-riding -demed") ;
       dem = 2 ; nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-demed") == 0 ){         /* 04 Mar 2009 */
       if( dem == 2 ) WARNING_message("-demed over-riding -demean") ;
       dem = 1 ; nopt++ ; continue ;
     }

     ERROR_exit("Don't understand option '%s'",argv[nopt]) ;
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
     ERROR_exit("Can't read input file '%s'",argv[nopt]) ;

   /* nx = length of each column; ny = number of columns */

   nx = inim->nx ; ny = inim->ny ; iar = MRI_FLOAT_PTR(inim) ;

   for( jj=0 ; jj < ny ; jj++ ){
     qar = iar + jj*nx ;          /* the jj-th column */
     switch( dem ){               /* compute ss = value to subtract */
       case 1: qmedmad_float  ( nx , qar , &ss , NULL ) ; break ;
       case 2: meansigma_float( nx , qar , &ss , NULL ) ; break ;
     }
     if( ss != 0.0f ){ for( ii=0 ; ii < nx ; ii++ ) qar[ii] -= ss ; }
     switch( mode ){              /* normalization */
       default:  THD_normalize( nx , qar ) ; break ;  /* L_2 */
       case 1:   THD_normL1   ( nx , qar ) ; break ;  /* L_1 */
       case 666: THD_normmax  ( nx , qar ) ; break ;  /* L_infinity */
     }
   }

   /* save the results */

   mri_write_1D( (argc > nopt+1) ? argv[nopt+1] : "-" , inim ) ;
   exit(0) ;
}
