#include "mrilib.h"

int main( int argc , char * argv[] )
{
   THD_3dim_dataset * dset , * oset=NULL ;
   int nvals , iv , nxyz , ii , iarg , saveit=0 , oot , ic,cc ;
   int * count ;
   float qthr=0.001 , alph,fmed,fmad , fbot,ftop,fsig , sq2p ;
   MRI_IMAGE * flim ;
   float * far , * var ;
   byte * mmm=NULL ;
   int    mmvox=0 ;
   char * prefix=NULL ;
   int do_autoclip=0 , npass=0 ;   /* 12 Aug 2001 */
   float clip_val=0.0 ;

   /*----- Read command line -----*/

   if( argc < 2 ){
      printf("Usage: 3dToutcount [options] dataset\n"
             "Calculates number of 'outliers' a 3D+time dataset, at each\n"
             "time point, and writes the results to stdout.\n"
             "\n"
             "Options:\n"
             " -mask mset = Only count voxels in the mask dataset.\n"
             " -qthr q    = Use 'q' instead of 0.001 in the calculation\n"
             "                of alpha (below): 0 < q < 1.\n"
             " -autoclip  = Clip off 'small' voxels (as in 3dClipLevel).\n"
             " -save ppp  = Make a new dataset, and save the outlier Q in each\n"
             "                voxel, where Q is calculated from voxel value v by\n"
             "                Q = -log10(qg(abs((v-median)/(sqrt(PI/2)*MAD))))\n"
             "             or Q = 0 if v is 'close' to the median.\n"
             "              The prefix of the new dataset (float format) is 'ppp'.\n"
             "\n"
             "OUTLIERS are defined as follows:\n"
             " * The median and MAD of each time series are calculated.\n"
             " * In each time series, points that are 'far away' from the\n"
             "    median are called outliers, where 'far' is defined by\n"
             "      alpha * sqrt(PI/2) * MAD\n"
             "      alpha = qginv(0.001/N) (inverse of reversed Gaussian CDF)\n"
             "      N     = length of time series\n"
             " * Some outliers are to be expected, but if a large fraction of the\n"
             "    voxels in a volume are called outliers, you should investigate\n"
             "    the dataset more fully.\n"
             "\n"
             "Since the results are written to stdout, you probably want to redirect\n"
             "them to a file or another program, as in this example:\n"
             "  3dToutcount -autoclip v1+orig | 1dplot -stdin\n"
             "\n"
             "NOTE: also see program 3dTqual for a similar quality check.\n"
           ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      exit(0) ;
   }

   mainENTRY("3dToutcount main"); machdep(); AFNI_logger("3dToutcount",argc,argv);

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-autoclip") == 0 ){  /* 12 Aug 2001 */
         do_autoclip = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-save") == 0 ){
         prefix = argv[++iarg] ;
         if( !THD_filename_ok(prefix) ){
            fprintf(stderr,"** ERROR: -save prefix is not good!\n"); exit(1);
         }
         saveit = 1 ; iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-qthr") == 0 ){
         qthr = strtod( argv[++iarg] , NULL ) ;
         if( qthr <= 0.0 || qthr >= 0.999 ){
            fprintf(stderr,"** ERROR: -qthr value is illegal!\n"); exit(1);
         }
         iarg++ ; continue ;
      }

      if( strcmp(argv[iarg],"-mask") == 0 ){
         int mcount ;
         THD_3dim_dataset * mask_dset = THD_open_dataset(argv[++iarg]) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"** ERROR: can't open -mask dataset!\n"); exit(1);
         }
         if( mmm != NULL ){
            fprintf(stderr,"** ERROR: can't have 2 -mask options!\n"); exit(1);
         }
         mmm = THD_makemask( mask_dset , 0 , 1.0,-1.0 ) ;
         mmvox = DSET_NVOX( mask_dset ) ;
         mcount = THD_countmask( mmvox , mmm ) ;
         fprintf(stderr,"++ %d voxels in the mask\n",mcount) ;
         if( mcount <= 5 ){
            fprintf(stderr,"** Mask is too small!\n");exit(1);
         }
         DSET_delete(mask_dset) ; iarg++ ; continue ;
      }

      fprintf(stderr,"** Unknown option: %s\n",argv[iarg]) ; exit(1) ;
   }

   /*----- read input dataset -----*/

   if( iarg >= argc ){
      fprintf(stderr,"** No input dataset!?\n"); exit(1);
   }

   dset = THD_open_dataset( argv[iarg] ) ;
   if( !ISVALID_DSET(dset) ){
      fprintf(stderr,"** Can't open dataset %s\n",argv[iarg]); exit(1);
   }
   nvals = DSET_NUM_TIMES(dset) ;
   if( nvals < 5 ){
      fprintf(stderr,"** Can't use dataset with < 5 time points per voxel!\n") ;
      exit(1) ;
   }
   nxyz = DSET_NVOX(dset) ;
   if( mmm != NULL && mmvox != nxyz ){
      fprintf(stderr,"** Mask and input datasets not the same size!\n") ;
      exit(1) ;
   }
   DSET_load(dset) ;
   if( !DSET_LOADED(dset) ){
      fprintf(stderr,"** Can't read dataset from disk!\n") ;
      exit(1) ;
   }

   /*-- 12 Aug 2001: compute clip, if desired --*/

   if( do_autoclip ){
      MRI_IMAGE * medim = THD_median_brick( dset ) ;
      clip_val = THD_cliplevel( medim , 0.5 ) ;
      fprintf(stderr,"++ Autoclip value = %g\n",clip_val) ;
      mri_free(medim) ;
   }

   /*-- setup to save a new dataset, if desired --*/

   if( saveit ){
      float * bar ;
      oset = EDIT_empty_copy( dset ) ;
      EDIT_dset_items( oset ,
                         ADN_prefix    , prefix ,
                         ADN_brick_fac , NULL ,
                         ADN_datum_all , MRI_float ,
                       ADN_none ) ;

      if( THD_is_file(DSET_HEADNAME(oset)) ){
         fprintf(stderr,"** ERROR: -save dataset already exists!\n"); exit(1);
      }

      if( ISFUNC(oset) )
         EDIT_dset_items( oset , ADN_func_type,FUNC_FIM_TYPE , ADN_none ) ;

      for( iv=0 ; iv < nvals ; iv++ ){
         EDIT_substitute_brick( oset , iv , MRI_float , NULL ) ;
         bar = DSET_ARRAY(oset,iv) ;
         for( ii=0 ; ii < nxyz ; ii++ ) bar[ii] = 0.0 ;
      }
   }

   /*-- setup to count --*/

   sq2p  = sqrt(0.5*PI) ;
   alph  = qginv(qthr/nvals) * sq2p ;
   count = (int *) calloc( sizeof(int) , nvals ) ;
   var   = (float *) malloc( sizeof(float) * nvals ) ;

   /*--- loop over voxels and count ---*/

   for( cc=ii=0 ; ii < nxyz ; ii++ ){
      if( mmm != NULL && mmm[ii] == 0 ) continue ;  /* masked out */
      flim = THD_extract_series( ii , dset , 0 ) ;
      far  = MRI_FLOAT_PTR(flim) ;
      memcpy(var,far,sizeof(float)*nvals ) ;

      fmed = qmed_float( nvals , far ) ;
      if( clip_val > 0.0 && fmed < clip_val ) continue ; /* 12 Aug 2001 */
      npass++ ;
      for( iv=0 ; iv < nvals ; iv++ ) far[iv] = fabs(far[iv]-fmed) ;
      fmad = qmed_float( nvals , far ) ;
      fbot = fmed - alph*fmad ; ftop = fmed + alph*fmad ;

      if( fmad > 0.0 ){
         if( saveit ) fsig = 1.0/(sq2p*fmad) ;
         for( ic=iv=0 ; iv < nvals ; iv++ ){
            oot = (var[iv] < fbot || var[iv] > ftop ) ;
            if( oot ){ count[iv]++ ; cc++ ; }
            if( saveit ){
               if( oot ){ far[iv] = -log10qg(fabs((var[iv]-fmed)*fsig)); ic++; }
               else     { far[iv] = 0.0 ; }
            }
         }

         if( ic > 0 )
            THD_insert_series( ii,oset, nvals,MRI_float,far , 0 ) ;
      }

      mri_free(flim) ;
   }

   if( saveit && cc > 0 ) DSET_write( oset ) ;

   for( iv=0 ; iv < nvals ; iv++ ) printf("%6d\n",count[iv]) ;

#if 0
   DSET_unload(dset) ; free(count) ; free(var) ; if(mmm!=NULL)free(mmm) ;
#endif

   if( npass < nxyz )
      fprintf(stderr,"++ %d voxels pass mask/clip\n",npass) ;

   exit(0) ;
}
