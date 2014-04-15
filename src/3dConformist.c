#include "mrilib.h"
#include "thd_conformist.c"

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int ndset=0 , ii ;
   THD_3dim_dataset **dset ;

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf(
       "** Program 3dConformist reads in a collection of datasets and\n"
       "   zero pads them to the same size.\n"
       "** The output volume size is the smallest region that includes\n"
       "   all datasets (i.e., the minimal covering box).\n"
       "** If the datasets cannot be processed (e.g., different grid\n"
       "   spacings), then nothing will happen except for error messages.\n"
       "** The purpose of this program is to be used in scripts that\n"
       "   process lots of datasets and needs to make them all conform\n"
       "   to the same size for collective voxel-wise analyses.\n"
       "** The input datasets ARE ALTERED (embiggened)! <<<<<<------******\n"
       "   Therefore, don't use this program casually.\n"
     ) ;
     exit(0) ;
   }

   mainENTRY("3dConformist") ; machdep() ; PRINT_VERSION("3dConformist") ;

   ndset = argc-1 ;
   dset  = (THD_3dim_dataset **)malloc(sizeof(THD_3dim_dataset *)*ndset) ;
   for( ii=0 ; ii < ndset ; ii++ ){
     dset[ii] = THD_open_dataset(argv[ii+1]) ;
     CHECK_OPEN_ERROR(dset[ii],argv[ii+1]) ;
   }

   ii = THD_conformist(ndset,dset,CONFORM_REWRITE,NULL) ;

   switch(ii){
     default: INFO_message ("3dConformist: Re-wrote %d datasets",ii) ; break ;
     case  0: INFO_message ("3dConformist: all datasets matched OK") ; break ;
     case -1: ERROR_message("3dConformist: bad input")               ; break ;
     case -2: ERROR_message("3dConformist: bad inputs")              ; break ;
     case -3: ERROR_message("3dConformist: can't match grids")       ; break ;
   }

   exit(0) ;
}
