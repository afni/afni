#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset *dset , *mset ;
   char *prefix = "automask" ;
   byte *mask ;
   int iarg=1 , fillin=0 , nmask,nfill ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAutomask [options] dataset\n"
             "Input dataset is EPI 3D+time.\n"
             "Output dataset is a brain-only mask dataset.\n"
             "Method:\n"
             " + Uses 3dClipLevel algorithm to find clipping level.\n"
             " + Keeps only the largest connected component of the\n"
             "   supra-threshold voxels, after an erosion/dilation step.\n"
             " + Writes result as a 'fim' type of functional dataset.\n"
             "Options:\n"
             "  -prefix ppp = Write mask into dataset with prefix 'ppp'.\n"
             "                 [default='automask']\n"
             "  -fillin nnn = Fill in holes inside the mask of width up\n"
             "                 to 'nnn' voxels. [default=0=no fillin]\n"
            ) ;
      exit(0) ;
   }

   mainENTRY("3dAutomask main"); machdep(); AFNI_logger("3dAutomask",argc,argv);

   /*-- options --*/

   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-prefix") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
           fprintf(stderr,"** -prefix %s is illegal!\n",prefix) ;
           exit(1) ;
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-fillin") == 0 ){
         fillin = strtol( argv[++iarg] , NULL , 10 ) ;
         if( fillin <  0 ){
           fprintf(stderr,"** -fillin %s is illegal!\n",argv[iarg]) ;
           exit(1) ;
         } else if( fillin > 0 ){
           fillin = (fillin+2) / 2 ;
         }
         iarg++ ; continue ;
      }

      fprintf(stderr,"** ILLEGAL option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*-- read data --*/

   dset = THD_open_dataset(argv[iarg]) ;
   if( !ISVALID_DSET(dset) ){ fprintf(stderr,"** CAN'T open dataset\n");exit(1); }
   if( DSET_BRICK_TYPE(dset,0) != MRI_short &&
       DSET_BRICK_TYPE(dset,0) != MRI_byte  &&
       DSET_BRICK_TYPE(dset,0) != MRI_float   ){
      fprintf(stderr,"** ILLEGAL dataset type\n"); exit(1);
   }
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){ fprintf(stderr,"** CAN'T load dataset\n");exit(1); }

   mask = THD_automask( dset ) ;  /* all the work */

   if( mask == NULL ){ fprintf(stderr,"** Can't make mask!\n"); exit(1); }

   DSET_unload( dset ) ;  /* don't need data any more */

   /* 18 Apr 2002: print voxel count */

   nmask = THD_countmask( DSET_NVOX(dset) , mask ) ;
   fprintf(stderr,"++ %d voxels in the mask\n",nmask) ;
   if( nmask == 0 ){
      fprintf(stderr,"** Quitting without saving mask\n"); exit(1);
   }

   /* 18 Apr 2002: maybe fill in voxels */

   if( fillin > 0 ){
     nfill = THD_mask_fillin_completely(
                 DSET_NX(dset),DSET_NY(dset),DSET_NZ(dset), mask, fillin ) ;
     fprintf(stderr,"++ %d voxels filled in; %d voxels total\n",
             nfill,nfill+nmask ) ;
   }

   /* create output dataset */

   mset = EDIT_empty_copy( dset ) ;
   EDIT_dset_items( mset ,
                      ADN_prefix     , prefix   ,
                      ADN_datum_all  , MRI_byte ,
                      ADN_nvals      , 1        ,
                      ADN_ntt        , 0        ,
                      ADN_type       , HEAD_FUNC_TYPE ,
                      ADN_func_type  , FUNC_FIM_TYPE ,
                    ADN_none ) ;
   EDIT_substitute_brick( mset , 0 , MRI_byte , mask ) ;

   /* 16 Apr 2002: make history */

   tross_Copy_History( dset , mset ) ;
   tross_Make_History( "3dAutomask", argc,argv, mset ) ;

   DSET_write( mset ) ;
   exit(0) ;
}
