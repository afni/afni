#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int iarg , nvox=0 , iv,ii,cnum , verb=1 ;
   THD_3dim_dataset *aset , *bset ;
   byte *amm , *bmm ; int naa , nbb , nabu,nabi , naout , nbout ;
   float paout , pbout , xrat,yrat,zrat ;
   float_triple axyz , bxyz ;

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ){
      printf(
       "Usage: 3dABoverlap [options] A B\n"
       "Output (to screen) is a count of various things about how\n"
       "the automasks of datasets A and B overlap or don't overlap.\n"
       "\n"
       "* Dataset B will be resampled to match dataset A, if necessary,\n"
       "   which will be slow if A is high resolution.  In such a case,\n"
       "   you should only use one sub-brick from dataset B.\n"
       "  ++ The resampling of B is done before the automask is generated.\n"
       "* The values output are labeled thusly:\n"
       "    #A         = number of voxels in the A mask\n"
       "    #B         = number of voxels in the B mask\n"
       "    #(A uni B) = number of voxels in the either or both masks (set union)\n"
       "    #(A int B) = number of voxels present in BOTH masks (set intesection)\n"
       "    #(A \\ B)   = number of voxels in A mask that aren't in B mask\n"
       "    #(B \\ A)   = number of voxels in B mask that arent' in A mask\n"
       "    %%(A \\ B)   = percentage of voxels from A mask that aren't in B mask\n"
       "    %%(B \\ A)   = percentage of voxels from B mask that aren't in A mask\n"
       "    Rx(B/A)    = radius of gyration of B mask / A mask, in x direction\n"
       "    Ry(B/A)    = radius of gyration of B mask / A mask, in y direction\n"
       "    Rz(B/A)    = radius of gyration of B mask / A mask, in z direction\n"
       "* If B is an EPI dataset sub-brick, and A is a skull stripped anatomical\n"
       "   dataset, then %%(B \\ A) might be useful for assessing if the EPI\n"
       "   brick B is grossly misaligned with respect to the anatomical brick A.\n"
       "* The radius of gyration ratios might be useful for determining if one\n"
       "   dataset is grossly larger or smaller than the other.\n"
       "\n"
       "OPTIONS\n"
       "-------\n"
       " -verb  = print out some progress reports (to stderr)\n"
       " -quiet = be as quiet as possible (without being entirely mute)\n"
      ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   iarg = 1 ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   /* check options */

   while( iarg < argc && argv[iarg][0] == '-' ){

     if( strcmp(argv[iarg],"-verb") == 0 ){
       verb++ ; iarg++ ; continue ;
     }

     if( strcmp(argv[iarg],"-quiet") == 0 ){
       verb = 0 ; iarg++ ; continue ;
     }

     ERROR_exit("Illegal option: %s",argv[iarg]) ;
   }

   mainENTRY("3dOverlap main") ; machdep() ;
   if( verb ) PRINT_VERSION("3dOverlap") ;
   AFNI_logger("3dOverlap",argc,argv) ;

   /* input datasets */

   if( iarg+1 >= argc ) ERROR_exit("Need 2 input datasets on command line") ;

   aset = THD_open_dataset(argv[iarg]) ; CHECK_OPEN_ERROR(aset,argv[iarg]) ; iarg++ ;
   bset = THD_open_dataset(argv[iarg]) ; CHECK_OPEN_ERROR(bset,argv[iarg]) ; iarg++ ;

   nvox = DSET_NVOX(aset) ;

   if( !EQUIV_GRIDS(aset,bset) ){  /** must resample **/
     THD_3dim_dataset *cset ;
     if( verb ) INFO_message("resampling dataset B to match dataset A") ;

     cset = r_new_resam_dset( bset, aset, 0.0,0.0,0.0,NULL, MRI_BILINEAR, NULL, 1 ) ;
     DSET_delete(bset) ; bset = cset ;
   }

   if( iarg < argc ) WARNING_message("Extra arguments?") ;

   DSET_load(aset); CHECK_LOAD_ERROR(aset); amm = THD_automask(aset); DSET_unload(aset);
   DSET_load(bset); CHECK_LOAD_ERROR(bset); bmm = THD_automask(bset); DSET_unload(bset);

   naa   = mask_count          ( nvox , amm ) ;
   nbb   = mask_count          ( nvox , bmm ) ;
   nabi  = mask_intersect_count( nvox , amm , bmm ) ;
   nabu  = mask_union_count    ( nvox , amm , bmm ) ;
   naout = naa - nabi ;
   nbout = nbb - nabi ;
   paout = (naa > 0) ? naout/(float)naa : 0.0f ;
   pbout = (nbb > 0) ? nbout/(float)nbb : 0.0f ;

   axyz  = mask_rgyrate( DSET_NX(aset),DSET_NY(aset),DSET_NZ(aset) , amm ) ;
   bxyz  = mask_rgyrate( DSET_NX(bset),DSET_NY(bset),DSET_NZ(bset) , bmm ) ;

   xrat = (axyz.a > 0.0f && bxyz.a > 0.0f) ? bxyz.a / axyz.a : 0.0f ;
   yrat = (axyz.b > 0.0f && bxyz.b > 0.0f) ? bxyz.b / axyz.b : 0.0f ;
   zrat = (axyz.c > 0.0f && bxyz.c > 0.0f) ? bxyz.c / axyz.c : 0.0f ;

   if( verb )
     printf("#A=%s  B=%s\n",DSET_BRIKNAME(aset),DSET_BRIKNAME(bset)) ;
   if( verb )
     printf("#A           #B           #(A uni B)   #(A int B)   "
            "#(A \\ B)     #(B \\ A)     %%(A \\ B)    %%(B \\ A)    "
            "Rx(B/A)    Ry(B/A)    Rz(B/A)\n") ;
   printf("%-12d %-12d %-12d %-12d %-12d %-12d %7.4f     %7.4f    %7.4f    %7.4f    %7.4f\n",
          naa  , nbb , nabu, nabi, naout,nbout,100.0f*paout,100.0f*pbout,
          xrat,yrat,zrat ) ;

   exit(0) ;
}
