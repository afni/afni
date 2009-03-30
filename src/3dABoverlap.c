#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int narg , nvox=0 , iv,ii,cnum ;
   THD_3dim_dataset *aset , *bset ;
   byte *amm , *bmm ; int naa , nbb , nabu,nabi , naout , nbout ;
   float paout , pbout ;

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: 3dABoverlap AA BB\n"
             "Output (to screen) is a count of various things about how\n"
             "the automasks of datasets AA and BB overlap or don't overlap.\n"
             "\n"
             "* Dataset B will be resampled to match A, if necessary,\n"
             "   which will be slow.\n"
             "* The last value is the percentage of B that is outside A,\n"
             "   which might be useful for assessing if an EPI dataset\n"
             "   brick is grossly misregistered with an anatomical dataset.\n"
      ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   narg = 1 ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dOverlap main") ; machdep() ; PRINT_VERSION("3dOverlap") ;

   AFNI_logger("3dOverlap",argc,argv) ;

   /* check options */

   while( narg < argc && argv[narg][0] == '-' ){
      ERROR_exit("Illegal option: %s",argv[narg]) ;
   }

   /* input datasets */

   if( narg+1 >= argc ) ERROR_exit("Need 2 input datasets on command line") ;

   aset = THD_open_dataset(argv[narg]) ; CHECK_OPEN_ERROR(aset,argv[narg]) ; narg++ ;
   bset = THD_open_dataset(argv[narg]) ; CHECK_OPEN_ERROR(bset,argv[narg]) ; narg++ ;

   nvox = DSET_NVOX(aset) ;

   if( DSET_NX(aset) != DSET_NX(bset) ||
       DSET_NY(aset) != DSET_NY(bset) ||
       DSET_NZ(aset) != DSET_NZ(bset)   ){  /* resample */

     THD_3dim_dataset *cset ;
     INFO_message("resampling dataset B to match dataset A") ;

     cset = r_new_resam_dset( bset , aset , 0.0,0.0,0.0,NULL , MRI_BILINEAR , NULL , 1 ) ;
     DSET_delete(bset) ; bset = cset ;
   }

   if( narg < argc ) WARNING_message("Extra arguments?") ;

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

   printf("#A=%s  B=%s\n",DSET_BRIKNAME(aset),DSET_BRIKNAME(bset)) ;
   printf("#A           #B           #(A union B) #(A int B)   #(A \\ B)     #(B \\ A)     %%(A \\ B)    %%(B \\ A)\n") ;
   printf("%-12d %-12d %-12d %-12d %-12d %-12d %7.4f     %7.4f\n",
          naa  , nbb , nabu, nabi, naout,nbout,100.0f*paout,100.0f*pbout ) ;

   exit(0) ;
}
