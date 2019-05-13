#include "mrilib.h"
#include "mri_pvmap.c"
#include "mri_spatconcen.c" /* 24 Apr 2019 */

/*----------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   char *prefix = "PVmap" , *uvpref=NULL , *cpt ;
   THD_3dim_dataset *inset=NULL , *outset=NULL , *mset=NULL ;
   char *maskname=NULL ; byte *mask=NULL ; int nmask ;
   int nopt ;
   MRI_IMAGE *pvim ;
   MRI_IMAGE *uvim ; float_pair uvlam ; float ulam , vlam , scon ;

   if( argc < 2 || ! strncmp(argv[1],"-h",2) ){
     printf("\n"
            "3dPVmap [-prefix XXX] [-mask MMM] [-automask] inputdataset\n"
            "\n"
            "Computes the first 2 principal component vectors of a\n"
            "time series datasets, then outputs the R-squared coefficient\n"
            "of each voxel time series with these first 2 components.\n"
            "\n"
            "Each voxel times series from the input dataset is minimally pre-processed\n"
            "before the PCA is computed:\n"
            "  Despiking\n"
            "  Legendre polynomial detrending\n"
            "  L2 normalizing (sum-of-squares = 1)\n"
            "If you want more impressive pre-processing, you'll have to do that\n"
            "before running 3dPVmap (e.g., use the errts dataset from afni_proc.py).\n"
            "\n"
            "Program also outputs the first 2 principal component time series\n"
            "vectors into a 1D file, for fun and profit.\n"
            "\n"
            "The fractions of total-sum-of-squares allocable to the first 2\n"
            "principal components are written to stdout at the end of the program.\n"
            "along with a 3rd number that is a measure of the spatial concentration\n"
            "or dispersion of the PVmap.\n"
            "\n"
            "These values can be captured into a file by Unix shell redirection\n"
            "or into a shell variable by assigment:\n"
            "  3dPVmap -mask AUTO Fred.nii > Fred.sval.1D\n"
            "  set sval = ( `3dPVmap -mask AUTO Fred.nii` )  # csh syntax\n"
            "If the first value is very large, for example, this might indicate\n"
            "the widespread presence of some artifact in the dataset.\n"
            "\n"
            "If the 3rd number is bigger than 1, it indicates that the PVmap\n"
            "is more concentrated in space; if it is less than one, it indicates\n"
            "that it is more dispersed in space (relative to a uniform density).\n"
            "  3dPVmap -mask AUTO Zork.nii\n"
            "  ++ mask has 21300 voxels\n"
            "  ++ Output dataset ./PVmap+orig.BRIK\n"
            "  0.095960 0.074847 1.356635\n"
            "The first principal component accounted for 9.6%% of the total sum-of-squares,\n"
            "the second component for 7.5%%, and the PVmap is fairly concentrated in space.\n"
            "These %% values are not very unusual, but the concentration is fairly high\n"
            "and the dataset should be further investigated.\n"
            "\n"
            "A concentration value below 1 indicates the PVmap is fairly dispersed; this\n"
            "often means the larger PVmap values are found near the edges of the brain\n"
            "and can be caused by motion or respiration artifacts.\n"
            "\n"
            "The goal is to visualize any widespread time series artifacts.\n"
            "For example, if a 'significant' part of the brain shows R-squared > 0.25,\n"
            "that could be a subject for concern -- look at your data!\n"
            "\n"
            "Author: Zhark the Unprincipaled\n\n"
           ) ;
     exit(0) ;
   }

   nopt = 1 ;
   while( nopt < argc && argv[nopt][0] == '-' ){

     if( strcmp(argv[nopt],"-prefix") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("-prefix needs an argument!");
       prefix = strdup(argv[nopt]) ;
       if( !THD_filename_ok(prefix) )
         ERROR_exit("%s is not a valid prefix!",prefix);
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-mask") == 0 ){
       if( ++nopt >= argc ) ERROR_exit("-mask needs an argument!");
       maskname = strdup(argv[nopt]) ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-automask") == 0 ){  /* 18 Apr 2019 */
       maskname = strdup("AUTO") ;
       nopt++ ; continue ;
     }

     if( strcmp(argv[nopt],"-") == 0 ){ nopt++ ; continue ; }

     ERROR_exit("Unknown option %s",argv[nopt]) ;
   }

   if( nopt >= argc ) ERROR_exit("No input dataset name?") ;

   inset = THD_open_dataset(argv[nopt]) ;
   CHECK_OPEN_ERROR(inset,argv[nopt]) ;
   if( DSET_NVALS(inset) < 9 ) ERROR_exit("input dataset too short") ;

   if( maskname != NULL ){
     if( strncasecmp(maskname,"AUTO",4) != 0 ){
       mset = THD_open_dataset(maskname) ;
       CHECK_OPEN_ERROR(mset,maskname) ;
       if( !EQUIV_GRIDXYZ(inset,mset) )
         ERROR_exit("-mask and input dataset don't match") ;
       mask = THD_makemask( mset , 0 , 1.0f,0.0f ) ;
       nmask = THD_countmask( DSET_NVOX(mset) , mask ) ;
     } else {
       mask = THD_automask(inset) ;
       if( mask == NULL ) ERROR_exit("Can't make automask :(") ;
       nmask = THD_countmask( DSET_NVOX(inset) , mask ) ;
     }
     INFO_message("mask has %d voxels",nmask) ;
     if( nmask < 9 ) ERROR_exit("mask is too small") ;
   } else {
     nmask = DSET_NVOX(inset) ;
     INFO_message("No mask == using all %d voxels",nmask) ;
   }

   pvim = THD_dataset_to_pvmap( inset , mask ) ;

   if( pvim == NULL ) ERROR_exit("Can't compute PVmap :(") ;
   DSET_unload(inset) ;

   outset = EDIT_empty_copy(inset) ;
   EDIT_dset_items( outset ,
                         ADN_prefix     , prefix   ,
                         ADN_datum_all  , MRI_float ,
                         ADN_nvals      , 1        ,
                         ADN_ntt        , 0        ,
                         ADN_type       , HEAD_FUNC_TYPE ,
                         ADN_func_type  , FUNC_FIM_TYPE ,
                    ADN_none ) ;
   EDIT_substitute_brick( outset , 0 , MRI_float , MRI_FLOAT_PTR(pvim) ) ;

   tross_Copy_History( inset , outset ) ;
   tross_Make_History( "3dPVmap", argc,argv, outset ) ;
   DSET_write(outset) ;
   WROTE_DSET(outset) ;

   uvim = mri_pvmap_get_vecpair() ;
   uvlam = mri_pvmap_get_lampair() ;

   uvpref = (char *)malloc(sizeof(char)*(strlen(prefix)+32)) ;
   strcpy(uvpref,prefix) ;
   cpt = strstr(uvpref,".nii" ) ; if( cpt != NULL ) *cpt = '\0' ;
   cpt = strstr(uvpref,".HEAD") ; if( cpt != NULL ) *cpt = '\0' ;
   cpt = strrchr(uvpref,'+'   ) ; if( cpt != NULL ) *cpt = '\0' ;
   strcat(uvpref,".1D") ;
   mri_write_1D( uvpref , uvim ) ;

   ulam = uvlam.a*uvlam.a / nmask ;
   vlam = uvlam.b*uvlam.b / nmask ;
   scon = mri_spatial_concentration(pvim) ;
   printf("%.6f %.6f %.6f\n",ulam,vlam,scon) ;

   exit(0) ;
}
