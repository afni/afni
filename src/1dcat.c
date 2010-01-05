/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

#define MAXIM 1024
int main( int argc , char * argv[] )
{
   int nim , ii , jj , kk , nx, narg, oform ;
   MRI_IMAGE ** inim ;
   float * far ;
   char *formatstr=NULL;

   mainENTRY("1dcat:main");
   
   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
"Usage: 1dcat [-form option] a.1D b.1D ...\n"
"  where each file a.1D, b.1D, etc. is a 1D file.\n"
"  In the simplest form, a 1D file is an ASCII file of numbers\n"
"  arranged in rows and columns.\n"
"The row-by-row concatenation of the columns included in these  \n"
"files is written to stdout.\n"
"\n"
"1dcat takes as input one or more 1D files, and writes out a 1D file\n"
"containing the side-by-side concatenation of all or a subset of the\n"
"columns from the input files.\n"
"All files must have the same number of rows.\n"
"For help on -form's usage, see ccalc's help for the option of the same name.\n"
"Example:\n"
"  Input file 1:\n   1\n   2\n   3\n   4\n"
"  Input file 2:\n   5\n   6\n   7\n   8\n"
"\n"
"  1dcat data1.1D data2.1D > catout.1D\n" 
"  Output file: \n   1 5\n   2 6\n   3 7\n   4 8\n"
"\n"
"For generic 1D file usage help, see '1dplot -help'\n"
           ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   machdep() ;

   /* do we have any options? */
   oform = CCALC_NOT_SET; 
   narg = 1;
   while (narg < argc && argv[narg][0] == '-') {
      if (strcmp(argv[narg],"-form") == 0) {
         ++narg;
         if (narg >= argc)  {
	         fprintf (stderr, "need argument after -form ");
	         exit (1);
         }
         if (strcmp(argv[narg],"double") == 0 ) oform = CCALC_DOUBLE;
         else if (strcmp(argv[narg],"nice") == 0 ) oform = CCALC_NICE;
         else if (strcmp(argv[narg],"int") == 0 ) oform = CCALC_INT;
         else if (strcmp(argv[narg],"rint") == 0 ) oform = CCALC_INT;
         else if (strcmp(argv[narg],"fint") == 0 ) oform = CCALC_FINT;
         else if (strcmp(argv[narg],"cint") == 0 ) oform = CCALC_CINT;
         else if (strlen(argv[narg])<=256) {
            oform = CCALC_CUSTOM;
            formatstr = argv[narg];
         }
         else {
            fprintf (stderr,  "Format type '%s' not supported.\n"
                              "See -help for details.\n", argv[narg]);
            exit (1);
         }
         ++narg;
      } else if (strncmp(argv[narg],"-d",2) == 0) {
         oform = CCALC_DOUBLE; ++narg;
      } else if (strncmp(argv[narg],"-n",2) == 0) {
         oform = CCALC_NICE; ++narg;
      } else if (strncmp(argv[narg],"-i",2) == 0) {
         oform = CCALC_INT; ++narg;
      } else if (strncmp(argv[narg],"-r",2) == 0) {
         oform = CCALC_INT; ++narg;
      } else if (strncmp(argv[narg],"-f",2) == 0) {
         oform = CCALC_FINT; ++narg;
      } else if (strncmp(argv[narg],"-c",2) == 0) {
         oform = CCALC_CINT; ++narg;
      } else { /* break if option is not recognized */     
         ++narg;
         break; 
      }
   }
   
   /* read input files */

   nim = argc-narg ;
   inim = (MRI_IMAGE **) malloc( sizeof(MRI_IMAGE *) * nim ) ;
   for( jj=0 ; jj < nim ; jj++ ){
#if 1                                   /** for testing only **/
      if( AFNI_yesenv("ragged") ){
        MRI_IMAGE *qim ;
        qim      = mri_read_ascii_ragged( argv[jj+narg] , 3.e+33 ) ;
        fprintf(stderr,"qim: nx=%d ny=%d\n",qim->nx,qim->ny) ;
        inim[jj] = mri_transpose(qim) ; mri_free(qim) ;
      } else
#endif
      inim[jj] = mri_read_1D( argv[jj+narg] ) ;
      if( inim[jj] == NULL )
        ERROR_exit("Can't read input file '%s'",argv[jj+narg]) ;
      if( jj > 0 && inim[jj]->nx != inim[0]->nx )
        ERROR_exit("Input file %s doesn't match first file %s in length!",
                   argv[jj+narg],argv[1]) ;
   }

   nx = inim[0]->nx ;
   if (oform == CCALC_NOT_SET) {
      for( ii=0 ; ii < nx ; ii++ ){
         for( jj=0 ; jj < nim ; jj++ ){
            far = MRI_FLOAT_PTR(inim[jj]) ;
            for( kk=0 ; kk < inim[jj]->ny ; kk++ ){
               printf(" %g", far[ii+kk*nx] ) ; 
               /* printf(" %+.2f", far[ii+kk*nx] ) ;*/
            }
         }
         printf("\n") ;
      }
   } else {
      for( ii=0 ; ii < nx ; ii++ ){
         for( jj=0 ; jj < nim ; jj++ ){
            far = MRI_FLOAT_PTR(inim[jj]) ;
            for( kk=0 ; kk < inim[jj]->ny ; kk++ ){
               printf(  " %s", 
                        format_value_4print(far[ii+kk*nx], oform, formatstr )); 
            }
         }
         printf("\n") ;
      }
   }
   exit(0) ;
}
