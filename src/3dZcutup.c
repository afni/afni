/**---------- Adapted from 3dZeropad.c by RWCox - 08 Aug 2001 ----------**/

#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int iarg ;
   THD_3dim_dataset *inset , *outset ;
   int keep_bot=-1 , keep_top , nz ;
   int add_I=0 , add_S=0 , add_A=0 , add_P=0 , add_L=0 , add_R=0 ;
   char * prefix="zcutup" ;

   mainENTRY("3dZcutup main") ; machdep() ;

   /*-- help? --*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dZcutup [options] dataset\n"
             "Cuts slices off a dataset in its z-direction, and writes a new\n"
             "dataset.  The z-direction and number of slices in a dataset\n"
             "can be determined using the 3dinfo program.\n"
             "Options:\n"
             " -keep b t   = Keep slices numbered 'b' through 't', inclusive.\n"
             "                 This is a mandatory option.  If you want to\n"
             "                 create a single-slice dataset, this is allowed,\n"
             "                 but AFNI may not display such datasets properly.\n"
             "                 A single slice dataset would have b=t.  Slice\n"
             "                 numbers start at 0.\n"
             " -prefix ppp = Write result into dataset with prefix 'ppp'\n"
             "                 [default = 'zcutup']\n"
             "Notes:\n"
             " * You can use a sub-brick selector on the input dataset.\n"
             " * 3dZcutup won't overwrite an existing dataset (I hope).\n"
             " * This program is adapted from 3dZeropad, which does the\n"
             "     same thing, but along all 3 axes.\n"
             " * You can glue datasets back together in the z-direction\n"
             "     using program 3dZcat.  A sample C shell script that\n"
             "     uses these progams to carry out an analysis of a large\n"
             "     dataset is:\n"
             "\n"
             "  #!/bin/csh\n"
             "  # Cut 3D+time dataset epi07+orig into individual slices\n"
             "\n"
             "  foreach sl ( `count -dig 2 0 20` )\n"
             "    3dZcutup -prefix zcut${sl} -keep $sl $sl epi07+orig\n"
             "\n"
             "    # Analyze this slice with 3dDeconvolve separately\n"
             "\n"
             "    3dDeconvolve -input zcut${sl}+orig.HEAD            \\\n"
             "                 -num_stimts 3                         \\\n"
             "                 -stim_file 1 ann_response_07.1D       \\\n"
             "                 -stim_file 2 antiann_response_07.1D   \\\n"
             "                 -stim_file 3 righthand_response_07.1D \\\n"
             "                 -stim_label 1 annulus                 \\\n"
             "                 -stim_label 2 antiann                 \\\n"
             "                 -stim_label 3 motor                   \\\n"
             "                 -stim_minlag 1 0  -stim_maxlag 1 0    \\\n"
             "                 -stim_minlag 2 0  -stim_maxlag 2 0    \\\n"
             "                 -stim_minlag 3 0  -stim_maxlag 3 0    \\\n"
             "                 -fitts zcut${sl}_fitts                \\\n"
             "                 -fout -bucket zcut${sl}_stats\n"
             "  end\n"
             "\n"
             "  # Assemble slicewise outputs into final datasets\n"
             "\n"
             "  time 3dZcat -verb -prefix zc07a_fitts zcut??_fitts+orig.HEAD\n"
             "  time 3dZcat -verb -prefix zc07a_stats zcut??_stats+orig.HEAD\n"
             "\n"
             "  # Remove individual slice datasets\n"
             "\n"
             "  /bin/rm -f zcut*\n"
           ) ;
      exit(0) ;
   }

   /*-- read command line options --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      /*- -prefix -*/

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"*** Illegal string after -prefix!\n"); exit(1) ;
         }
         iarg++ ; continue ;
      }

      /*- -keep -*/

      if( strcmp(argv[iarg],"-keep") == 0 ){
         if( iarg+2 >= argc ){
           fprintf(stderr,"*** Need 2 arguments after -keep!\n"); exit(1);
         }
         keep_bot = strtol( argv[++iarg] , NULL , 10 ) ;
         keep_top = strtol( argv[++iarg] , NULL , 10 ) ;
         if( keep_bot < 0 || keep_top > keep_top ){
            fprintf(stderr,"*** Nonsense values after -keep!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      /*- what the hell? -*/

      fprintf(stderr,"*** Illegal option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*- check to see if the user asked for something, anything -*/

   if( keep_bot < 0 ){
      fprintf(stderr,"*** Don't you want to -keep SOMETHING!?\n"); exit(1);
   }

   /*-- read the input dataset --*/

   if( iarg >= argc ){
      fprintf(stderr,"*** No input dataset on command line!\n"); exit(1);
   }

   inset = THD_open_dataset( argv[iarg] ) ;
   if( inset == NULL ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[iarg]); exit(1);
   }

   nz = DSET_NZ(inset) ;
   if( keep_top >= nz ){
      fprintf(stderr,"*** -keep %d %d goes past last slice %d\n",
              keep_bot,keep_top,nz-1 ) ;
      exit(1) ;
   }

   /*-- set orientation codes correctly --*/

   switch( inset->daxes->zzorient ){
      case ORI_R2L_TYPE:
         add_R = -keep_bot ; add_L = keep_top - (nz-1) ; break ;
      case ORI_L2R_TYPE:
         add_L = -keep_bot ; add_R = keep_top - (nz-1) ; break ;
      case ORI_P2A_TYPE:
         add_P = -keep_bot ; add_A = keep_top - (nz-1) ; break ;
      case ORI_A2P_TYPE:
         add_A = -keep_bot ; add_P = keep_top - (nz-1) ; break ;
      case ORI_I2S_TYPE:
         add_I = -keep_bot ; add_S = keep_top - (nz-1) ; break ;
      case ORI_S2I_TYPE:
         add_S = -keep_bot ; add_I = keep_top - (nz-1) ; break ;
      default:
         fprintf(stderr,"*** Unknown orientation code in dataset!\n");
         exit(1) ;
   }

   outset = THD_zeropad( inset ,
                         add_I, add_S, add_A, add_P, add_L, add_R,
                         prefix , ZPAD_PURGE ) ;

   if( outset == NULL ){
      fprintf(stderr,"*** 3dZcutup: Some error occurred in processing!\n") ;
      exit(1) ;
   }

STATUS("checking output filename") ;

   if( THD_is_file(DSET_HEADNAME(outset)) ){
      fprintf(stderr,
              "*** 3dZcutup: output file %s already exists - FATAL ERROR!\n",
              DSET_HEADNAME(outset) ) ;
      exit(1) ;
   }

STATUS("making history") ;

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dZcutup" , argc,argv , outset ) ;

STATUS("writing output") ;

   DSET_write(outset) ;
   exit(0) ;
}
