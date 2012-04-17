#include "mrilib.h"

void usage_1dTsort( int detail ){
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
      " -col j   = sort only on column #j (counting starts at 0),\n"
      "            and carry the rest of the columns with it.\n"
      " -imode   = typecast all values to integers, return the mode in\n"
      "            the input then exit. No sorting results are returned.\n" 
      "\n"
      "N.B.: Data will be read from standard input if the filename IS stdin,\n"
      "      and will also be row/column transposed if the filename is stdin\\'\n"
      "      For example:\n"
      "        1deval -num 100 -expr 'uran(1)' | 1dTsort stdin | 1dplot stdin\n"
      "\n" ) ;
      PRINT_COMPILE_DATE ;
      return;
}

int main( int argc , char *argv[] )
{
   MRI_IMAGE *im ;
   int dec=0 , flip=0 , iarg , csort=-1 , mode = 0;

   mainENTRY("1dTsort main") ; machdep() ;
   
   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){
     if( strcmp(argv[iarg],"-help") == 0 || strcmp(argv[iarg],"-h") == 0){
         usage_1dTsort(strlen(argv[iarg])>3 ? 2:1);
         exit(0) ;
     }
     if( strcmp(argv[iarg],"-col") == 0 ){  /* 07 Oct 2011 */
       if( ++iarg >= argc ) ERROR_exit("Need a value after -col") ;
       csort = (int)strtod(argv[iarg],NULL) ;
       if( csort < 0 ) ERROR_exit("Illegal value after -col") ;
       iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-inc") == 0 ){
       dec = 0 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-dec") == 0 ){
       dec = 1 ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-flip") == 0 ){
       flip = 1 ; iarg++ ; continue ;
     }
     
     if( strcmp(argv[iarg],"-imode") == 0 ){
       mode = 1 ; iarg++ ; continue ;
     }

     ERROR_message("Unknown option '%s'",argv[iarg]) ;
     suggest_best_prog_option(argv[0], argv[iarg]);
     exit(1);
   }
   
   if( argc < 2 ){
     usage_1dTsort(1) ; exit(0) ; 
   }

   if( iarg >= argc ) ERROR_exit("No 1D file on command line?!") ;

   im = mri_read_1D( argv[iarg] ) ;
   if( im == NULL ) ERROR_exit("Can't read 1D file %s",argv[iarg]) ;

   if( csort < 0 ) mri_xsort_inplace( im , dec ) ;
   else            mri_csort_inplace( im , dec , csort ) ;

   if( flip ){
     MRI_IMAGE *qim=mri_transpose(im) ;
     mri_free(im) ; im = qim ;
   }

   if (mode) { /* here mostly to debug qmode_int */
      int *iv = NULL, i=0;
      float *far=NULL;
      iv = (int *)calloc(im->nx*im->ny, sizeof(int));
      far = MRI_FLOAT_PTR(im);
      for (i=0; i<im->nx*im->ny; ++i) iv[i]=(int)far[i];
      mode = qmode_int(iv, im->nx*im->ny);
      fprintf(stdout,"mode: %d\n", mode);
      free(iv); iv=NULL;
      exit(0);
   }
   mri_write_1D( "-" , im ) ; mri_free(im) ; exit(0) ;
}
