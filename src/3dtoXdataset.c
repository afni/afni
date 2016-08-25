#include "mrilib.h"

#define SFAC 0.0002f

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *inset , *mask_dset ;
   FILE *fpout=NULL ;
   char *prefix="Xdataset" ;
   char fpname[THD_MAX_NAME] ;
   int nopt=1 , nvox,ngood , ii,jj , *ijkmask ;
   byte *mask_vol ; float *inar , val ; short *outar ;

   if( argc < 3 || strcasecmp(argv[1],"-help") == 0 ){
     printf("\n"
       "Convert input datasets to the format needed for 3dClustSimX.\n"
       "\n"
       "Usage:\n"
       "\n"
       " 3dtoXdataset -prefix PPP maskdataset inputdataset ...\n"
       "\n"
     ) ;
     exit(0) ;
   }

   if( strcasecmp(argv[nopt],"-prefix") == 0 ){
     prefix = strdup(argv[++nopt]) ;
   }

   if( strstr(prefix,".sdat") == NULL ){
     sprintf(fpname,"%s.sdat",prefix) ;
   } else {
     strcpy(fpname,prefix) ;
   }

   mask_dset = THD_open_dataset(argv[++nopt]) ;
   if( mask_dset == NULL )
     ERROR_exit("can't open mask dataset '%s'",argv[nopt]) ;

   mask_vol = THD_makemask( mask_dset , 0 , 1.0,0.0 ) ;
   if( mask_vol == NULL )
     ERROR_exit("can't use -mask dataset '%s'",argv[nopt]) ;
   DSET_unload(mask_dset) ;

   nvox = DSET_NVOX(mask_dset) ;
   ngood = THD_countmask( nvox , mask_vol ) ;
   INFO_message("mask has %d voxels",ngood) ;

   ijkmask = (int *  )malloc(sizeof(int)  *ngood) ;
   outar   = (short *)malloc(sizeof(short)*ngood) ;
   for( jj=ii=0 ; ii < nvox ; ii++ ){
     if( mask_vol[ii] ) ijkmask[jj++] = ii ;
   }

   for( ++nopt ; nopt < argc ; nopt++ ){
     ININFO_message("process dataset %s",argv[nopt]) ;
     inset = THD_open_dataset(argv[nopt]) ;
     if( inset == NULL )
       ERROR_exit("can't open dataset '%s'",argv[nopt]) ;
     DSET_load(inset) ; CHECK_LOAD_ERROR(inset) ;
     if( DSET_NVOX(inset) != nvox )
       ERROR_exit("data grid mismatch at input %s",argv[nopt]) ;
     if( !DSET_datum_constant(inset) || DSET_BRICK_TYPE(inset,0) != MRI_float )
       ERROR_exit("data is not pure float type at input %s",argv[nopt]) ;

     if( fpout == NULL ){
       fpout = fopen( fpname , "w" ) ;
       if( fpout == NULL )
         ERROR_exit("can't open file '%s' for writing",fpname) ;
     }
     for( jj=0 ; jj < DSET_NVALS(inset) ; jj++ ){
       inar = DSET_ARRAY(inset,jj) ;
       for( ii=0 ; ii < ngood ; ii++ ){
         val = inar[ijkmask[ii]] / SFAC ;
         outar[ii] = SHORTIZE(val) ;
       }
       fwrite(outar,sizeof(short),ngood,fpout) ;
     }
     DSET_delete(inset) ;
   }

   fclose(fpout) ;
   INFO_message("output file is %s",fpname) ;
   exit(0) ;
}
