/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int iarg ;
   THD_3dim_dataset *inset , *outset ;
   int add_I=0 , add_S=0 , add_A=0 , add_P=0 , add_L=0 , add_R=0 ;
   char * prefix="zeropad" ;

   int add_z=0 ;   /* 07 Feb 2001 */
   int mm_flag=0 ; /* 13 Feb 2001 */
   int flag ;

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
             "  -z n = adds 'n' planes of zeros on EACH of the\n"
             "          dataset z-axis (slice-direction) faces\n"
             "\n"
             " -mm   = pad counts 'n' are in mm instead of slices:\n"
             "         * each 'n' is an integer\n"
             "         * at least 'n' mm of slices will be added/removed:\n"
             "            n =  3 and slice thickness = 2.5 mm ==> 2 slices added\n"
             "            n = -6 and slice thickness = 2.5 mm ==> 3 slices removed\n"
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
#if 0
             " * If the old dataset has z-slice-dependent time offsets, and\n"
             "     if new z-planes are added, all the slice-dependent time\n"
             "     offsets will be removed.\n"
#else
             " * If the old dataset has z-slice-dependent time offsets, and\n"
             "     if new (zero filled) z-planes are added, the time offsets\n"
             "     of the new slices will be set to zero.\n"
#endif
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

            /* 07 Feb 2001: slice-direction is special */

            case 'z':
            case 'Z': add_z = (int) strtod(argv[++iarg],NULL) ; break ;

            default:
               fprintf(stderr,"*** Illegal option: %s\n",argv[iarg]) ; exit(1) ;
         }

         iarg++ ; continue ;  /* skip to next argument */
      }

      /*- -mm -*/

      if( strcmp(argv[iarg],"-mm") == 0 ){
         mm_flag = 1 ;
         iarg++ ; continue ;
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

   if( add_I==0 && add_S==0 && add_P==0 &&
       add_A==0 && add_L==0 && add_R==0 && add_z==0 ){

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

   /*-- 07 Feb 2001: if -z was used, fix that now --*/

   if( add_z != 0 ){
      switch( inset->daxes->zzorient ){
         case ORI_R2L_TYPE:
         case ORI_L2R_TYPE:
            if( add_R != 0 && add_R != add_z ) fprintf(stderr,"+++ -z overrides -R\n");
            if( add_L != 0 && add_L != add_z ) fprintf(stderr,"+++ -z overrides -L\n");
            add_R = add_L = add_z ;
         break ;

         case ORI_P2A_TYPE:
         case ORI_A2P_TYPE:
            if( add_P != 0 && add_P != add_z ) fprintf(stderr,"+++ -z overrides -P\n");
            if( add_A != 0 && add_A != add_z ) fprintf(stderr,"+++ -z overrides -A\n");
            add_P = add_A = add_z ;
         break ;

         case ORI_I2S_TYPE:
         case ORI_S2I_TYPE:
            if( add_I != 0 && add_I != add_z ) fprintf(stderr,"+++ -z overrides -I\n");
            if( add_I != 0 && add_S != add_z ) fprintf(stderr,"+++ -z overrides -S\n");
            add_I = add_S = add_z ;
         break ;
      }
   }

   /*-- 04 Oct 2000: all the real work is now in thd_zeropad.c --*/

   flag = ZPAD_PURGE ;
   if( mm_flag ) flag |= ZPAD_MM ;

   outset = THD_zeropad( inset ,
                         add_I, add_S, add_A, add_P, add_L, add_R,
                         prefix , flag ) ;

   if( THD_is_file(DSET_HEADNAME(outset)) ){
      fprintf(stderr,
              "*** 3dZeropad: output file %s already exists - FATAL ERROR!\n",
              DSET_HEADNAME(outset) ) ;
      exit(1) ;
   }

   if( outset == NULL ){
      fprintf(stderr,"*** 3dZeropad: Some error occurred in processing!\n") ;
      exit(1) ;
   }

   tross_Copy_History( inset , outset ) ;             /* 31 Jan 2001 - RWCox */
   tross_Make_History( "3dZeropad" , argc,argv , outset ) ;

   DSET_write(outset) ;
   exit(0) ;
}
