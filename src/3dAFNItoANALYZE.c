#include "mrilib.h"

int main( int argc , char *argv[] )
{
   char *aname ;
   THD_3dim_dataset *dset ;
   int ii , scl ;
   MRI_IMAGE *im , *qim ;
   char *fname ;
   float fac ;

   /*-- help me if you can --*/

   if( argc < 3 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAFNItoANALYZE aname dset\n"
             "Writes AFNI dataset 'dset' to 1 or more ANALYZE 7.5 format\n"
             ".hdr/.img file pairs (one pair for each sub-brick in the\n"
             "AFNI dataset).  The ANALYZE files will be named\n"
             "  aname_0000.hdr aname_0000.img   for sub-brick #0\n"
             "  aname_0001.hdr aname_0001.img   for sub-brick #1\n"
             "and so forth.  Each file pair will contain a single 3D array.\n"
             "\n"
             "* If the AFNI dataset does not include sub-brick scale\n"
             "  factors, then the ANALYZE files will be written in the\n"
             "  datum type of the AFNI dataset.\n"
             "* If the AFNI dataset does have sub-brick scale factors,\n"
             "  then each sub-brick will be scaled to floating format\n"
             "  and the ANALYZE files will be written as floats.\n"
             "* The .hdr and .img files are written in the native byte\n"
             "  order of the computer on which this program is executed.\n"
            ) ;
      exit(0) ;
   }

   /*-- read inputs --*/

   aname = argv[1] ;
   if( !THD_filename_ok(aname) ){
      fprintf(stderr,"** Illegal aname string %s\n",aname) ;
      exit(1) ;
   }
   fname = malloc( strlen(aname)+16 ) ;

   dset = THD_open_dataset( argv[2] ) ;
   if( dset == NULL ){
      fprintf(stderr,"** Can't open dataset %s\n",argv[2]) ;
      exit(1) ;
   }

   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
      fprintf(stderr,"** Can't load dataset %s\n",argv[2]) ;
      exit(1) ;
   }

   /* determine if we scale to floats */

   scl = THD_need_brick_factor( dset ) ;

   /* loop over sub-bricks */

   for( ii=0 ; ii < DSET_NVALS(dset) ; ii++ ){

      im = DSET_BRICK(dset,ii) ;             /* get the sub-brick */

      if( scl ){                             /* scale it to floats */
         fac = DSET_BRICK_FACTOR(dset,ii) ;
         if( fac == 0.0 ) fac = 1.0 ;
         qim = mri_scale_to_float( fac , im ) ;
      } else {
         qim = im ;
      }

      qim->dx = fabs( DSET_DX(dset) ) ;      /* load voxel sizes */
      qim->dy = fabs( DSET_DY(dset) ) ;
      qim->dz = fabs( DSET_DZ(dset) ) ;
      qim->dw = 1.0 ;

      sprintf(fname,"%s_%04d",aname,ii) ;    /* make up a filename */
      mri_write_analyze( fname , qim ) ;     /* do the real work */

      DSET_unload_one(dset,ii) ;             /* clean up the trash */
      if( scl ) mri_free(qim) ;
   }

   free(fname) ; exit(0) ;
}
