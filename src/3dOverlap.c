#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int narg , nvox=0 , iv,ii,cnum ;
   THD_3dim_dataset * xset ;
   byte *mmm=NULL ;
   short *ccc=NULL , ctop ;

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: 3dOverlap [options] dset1 dset2 ...\n"
             "Output = count of number of voxels that are nonzero in\n"
             "         ALL of the input input dataset sub-bricks\n"
             "         - the result is a number printed to stdout"
             "Options:\n"
             "  None (yet).\n"
            ) ;
      exit(0) ;
   }

   narg = 1 ;

   /* if there were any options, I'd check them here */

   ctop = 0 ;
   for( ; narg < argc ; narg++ ){

      xset = THD_open_dataset( argv[narg] ) ;
      if( xset == NULL ){
         fprintf(stderr,"*** Can't open dataset %s\n",argv[narg]); exit(1);
      }
      DSET_load(xset) ;
      if( !DSET_LOADED(xset) ){
         fprintf(stderr,"*** Can't load dataset %s\n",argv[narg]); exit(1);
      }

      if( nvox == 0 ){
         nvox = DSET_NVOX(xset) ; ccc = calloc(sizeof(short),nvox) ;
      } else if( DSET_NVOX(xset) != nvox ){
         fprintf(stderr,"*** Dataset %s doesn't match in size!\n",argv[narg]); exit(1);
      }

      for( iv=0 ; iv < DSET_NVALS(xset) ; iv++ ){
         mmm = THD_makemask( xset , iv , 1.0,-1.0 ) ;
         if( mmm == NULL ){
            fprintf(stderr,"*** %s[%d] counting fails!\n",argv[narg],iv); exit(1);
         }
         for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) ccc[ii]++ ;
         free(mmm) ; ctop++ ;
      }

      DSET_delete(xset) ;
   }

   cnum = 0 ;
   for( ii=0 ; ii < nvox ; ii++ ) if( ccc[ii] == ctop ) cnum++ ;
   printf("%d\n",cnum) ; exit(0) ;
}
