#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int iarg ;
   THD_3dim_dataset *inset , *outset ;
   int add_I=0 , add_S=0 , add_A=0 , add_P=0 , add_L=0 , add_R=0 ;
   char * prefix="zeropad" ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dZeropad [options] dataset\n"
             "Adds planes of zeros to a dataset (i.e., pads it out).\n"
             "\n"
             "Options:\n"
             "  -I n = adds 'n' planes of zero at the Inferior edge\n"
             "  -S n = adds 'n' planes of zero at the Superior edge\n"
             "  -A n = adds 'n' planes of zero at the Anterior edge\n"
             "  -P n = adds 'n' planes of zero at the Posterior edge\n"
             "  -L n = adds 'n' planes of zero at the Left edge\n"
             "  -R n = adds 'n' planes of zero at the Right edge\n"
             "\n"
             " -prefix ppp = write result into dataset with prefix 'ppp'\n"
             "                 [default = 'zeropad']\n"
             "\n"
             "Nota Bene:\n"
             " * You can use negative values of n to cut planes off the edges\n"
             "     of a dataset.  At least one plane must be added/removed\n"
             "     or the program won't do anything.\n"
             " * Anat parent and Talairach markers are NOT preserved in the\n"
             "     new dataset.\n"
             " * If the old dataset has z-slice-dependent time offsets, and\n"
             "     if new z-planes are added, all the slice-dependent time\n"
             "     offsets will be removed.\n"
             " * You can use program '3dinfo' to find out how many planes\n"
             "     a dataset has in each direction.\n"
             " * Program works for byte-, short-, float-, and complex-valued\n"
             "     datasets.\n"
             " * You can use a sub-brick selector on the input dataset.\n"
             " * 3dZeropad won't overwrite an existing dataset (I hope).\n"
             "\n"
             " Author: RWCox - July 2000\n"
           ) ;
      exit(0) ;
   }

   /*-- read command line options --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      /*- -I, -S, etc. -*/

      if( strlen(argv[iarg]) == 2 ){
         switch( argv[iarg][1] ){
            case 'I': add_I = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'S': add_S = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'A': add_A = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'P': add_P = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'L': add_L = (int) strtod(argv[++iarg],NULL) ; break ;
            case 'R': add_R = (int) strtod(argv[++iarg],NULL) ; break ;

            default:
               fprintf(stderr,"*** Illegal option: %s\n",argv[iarg]) ; exit(1) ;
         }

         iarg++ ; continue ;  /* skip to next argument */
      }

      /*- -prefix -*/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"*** Illegal string after -prefix!\n"); exit(1) ;
         }
         iarg++ ; continue ;
      }

      /*- what the hell? -*/

      fprintf(stderr,"*** Illegal option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*- check to see if the user asked for something, anything -*/

   if( add_I==0 && add_S==0 && add_P==0 && add_A==0 && add_L==0 && add_R==0 ){
      fprintf(stderr,"*** Don't you want to DO something!?\n"); exit(1);
   }

   /*-- read the input dataset --*/

   if( iarg >= argc ){
      fprintf(stderr,"*** No input dataset on command line!\n"); exit(1);
   }

#if 0
   if( strncmp(argv[iarg],"3dcalc(",7) == 0 ){
      fprintf(stderr,"*** Can't use '3dcalc()' input datasets here!\n"); exit(1);
   }
#endif

   inset = THD_open_dataset( argv[iarg] ) ;
   if( inset == NULL ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[iarg]); exit(1);
   }

#if 0
   if( DSET_IS_MASTERED(inset) ){
      fprintf(stderr,"*** Can't use partial datasets!\n"); exit(1);
   }
#endif

   /*-- 04 Oct 2000: all the real work is now in thd_zeropad.c --*/

   outset = THD_zeropad( inset ,
                         add_I, add_S, add_A, add_P, add_L, add_R,
                         prefix ) ;

   if( outset == NULL ){
      fprintf(stderr,"*** 3dZeropad: Some error occurred in processing!\n") ;
      exit(1) ;
   }

   DSET_write(outset) ;
   exit(0) ;
}
