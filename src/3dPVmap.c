#include "mrilib.h"

#include "mri_pvmap.c"

/*----------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   char *prefix = "PVmap" ;
   THD_3dim_dataset *inset=NULL , *outset=NULL , *mset=NULL ;
   char *maskname=NULL ; byte *mask=NULL ; int nmask ;
   int nopt ;
   MRI_IMAGE *pvim ;

   if( argc < 2 ){
     printf("\n"
            "3dPVmap -prefix XXX -mask MMM inputdataset\n"
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

   exit(0) ;
}
