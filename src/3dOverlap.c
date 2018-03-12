#include "mrilib.h"

int main( int argc , char * argv[] )
{
   int narg , nvox=0 , iv,ii,cnum ;
   THD_3dim_dataset *xset , *oset=NULL , *fset=NULL ;
   byte *mmm=NULL ;
   short *ccc=NULL , ctop ;
   char *psave=NULL ;       /* 22 Feb 2001 */

   /*-- read command line arguments --*/

   if( argc < 2 || strncmp(argv[1],"-help",5) == 0 ){
      printf("Usage: 3dOverlap [options] dset1 dset2 ...\n"
             "Output = count of number of voxels that are nonzero in ALL\n"
             "         of the input dataset sub-bricks\n"
             "The result is simply a number printed to stdout.  (If a single\n"
             "brick was input, this is just the count of number of nonzero\n"
             "voxels in that brick.)\n"
             "Options:\n"
             "  -save ppp = Save the count of overlaps at each voxel into a\n"
             "              dataset with prefix 'ppp' (properly thresholded,\n"
             "              this could be used as a mask dataset).\n"
             "Example:\n"
             "  3dOverlap -save abcnum a+orig b+orig c+orig\n"
             "  3dmaskave -mask 'abcnum+orig<3..3>' a+orig\n"
             "\n"
             "Also see program 3dABoverlap :)\n"
            ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   narg = 1 ;

   /*-- 20 Apr 2001: addto the arglist, if user wants to [RWCox] --*/

   mainENTRY("3dOverlap main") ; machdep() ; PRINT_VERSION("3dOverlap") ;

   { int new_argc ; char ** new_argv ;
     addto_args( argc , argv , &new_argc , &new_argv ) ;
     if( new_argv != NULL ){ argc = new_argc ; argv = new_argv ; }
   }

   AFNI_logger("3dOverlap",argc,argv) ;

   /* check options */

   while( narg < argc && argv[narg][0] == '-' ){

      if( strcmp(argv[narg],"-save") == 0 ){  /* 22 Feb 2001 */
         psave = argv[++narg] ;
         if( !THD_filename_ok(psave) ){
            fprintf(stderr,"** Illegal -save prefix!\n") ; exit(1) ;
         }
         narg++ ; continue ;
      }

      fprintf(stderr,"** Illegal option: %s\n",argv[narg]) ; exit(1) ;
   }

   /* loop over input datasets */

   ctop = 0 ;                      /* count of bricks */
   for( ; narg < argc ; narg++ ){

      xset = THD_open_dataset( argv[narg] ) ;
      CHECK_OPEN_ERROR(xset,argv[narg]) ;
      DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

      if( fset == NULL ) fset = xset ;

      if( nvox == 0 ){
         nvox = DSET_NVOX(xset) ; ccc = calloc(sizeof(short),nvox) ;
      } else if( DSET_NVOX(xset) != nvox ){
         ERROR_exit("Dataset %s doesn't match in first one in voxels!\n",argv[narg]);
      } else if( !EQUIV_GRIDS(xset,fset) ){
         WARNING_message("Dataset %s doesn't match first one's grid!",argv[narg]) ;
      }

      for( iv=0 ; iv < DSET_NVALS(xset) ; iv++ ){
         mmm = THD_makemask( xset , iv , 1.0,-1.0 ) ;
         if( mmm == NULL ){
            fprintf(stderr,"*** %s[%d] counting fails!\n",argv[narg],iv); exit(1);
         }
         for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) ccc[ii]++ ;
         free(mmm) ; ctop++ ;
      }

      if( psave != NULL && oset == NULL ){  /* 22 Feb 2001: make output dataset */
         oset = EDIT_empty_copy(xset) ;
         EDIT_dset_items( oset ,
                             ADN_prefix    , psave ,
                             ADN_nvals     , 1 ,
                             ADN_ntt       , 0 ,
                             ADN_brick_fac , NULL ,
                             ADN_datum_all , MRI_short ,
                          ADN_none ) ;

         if( ISFUNC(oset) )
            EDIT_dset_items( oset , ADN_func_type,FUNC_FIM_TYPE , ADN_none ) ;

         tross_Copy_History( xset , oset ) ;
         tross_Make_History( "3dOverlap" , argc,argv , oset ) ;

         if( THD_deathcon() && THD_is_file(DSET_HEADNAME(oset)) ){
            fprintf(stderr,
                    "** Output file %s already exists -- will not overwrite!\n",
                    DSET_HEADNAME(oset) ) ;
            DSET_delete(oset) ; oset = NULL ; psave = NULL ;
          }
      }

      if( xset != fset ) DSET_delete(xset) ;
      else               DSET_unload(xset) ;
   }

   /* tot up the results */

   cnum = 0 ;
   for( ii=0 ; ii < nvox ; ii++ ) if( ccc[ii] == ctop ) cnum++ ;
   printf("%d\n",cnum) ;

   if( oset != NULL ){
      EDIT_substitute_brick( oset , 0 , MRI_short , ccc ) ;
      DSET_write(oset) ;
   }

   exit(0) ;
}
