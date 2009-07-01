#ifdef USE_OMP
#include <omp.h>
#endif

#include "mrilib.h"

#define SPEARMAN 1
#define QUADRANT 2
#define PEARSON  3

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *xset , *cset ;
   int nopt=1 , method=PEARSON , do_autoclip=0 ;
   int nvox , nvals , ii,jj,kout , polort=1 , ix,jy,kz ;
   MRI_IMAGE *xsim , *ysim ;
   float     *xsar , *ysar ;
   short     *car ;
   char * prefix = "ATcorr" ;
   byte * mmm=NULL ;
   int   nmask , abuc=1 ;
   char str[32] ;

   /*----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAutoTcorrelate [options] dset\n"
             "Computes the correlation coefficient between each pair of\n"
             "voxels in the input dataset, and stores the output into\n"
             "a new anatomical bucket dataset.\n"
             "\n"
             "Options:\n"
             "  -pearson  = Correlation is the normal Pearson (product moment)\n"
             "                correlation coefficient [default].\n"
             "  -spearman = Correlation is the Spearman (rank) correlation\n"
             "                coefficient.\n"
             "  -quadrant = Correlation is the quadrant correlation coefficient.\n"
             "\n"
             "  -polort m = Remove polynomical trend of order 'm', for m=-1..3.\n"
             "                [default is m=1; removal is by least squares].\n"
             "                Using m=-1 means no detrending; this is only useful\n"
             "                for data/information that has been pre-processed.\n"
             "\n"
             "  -autoclip = Clip off low-intensity regions in the dataset,\n"
             "  -automask =  so that the correlation is only computed between\n"
             "               high-intensity (presumably brain) voxels.  The\n"
             "               intensity level is determined the same way that\n"
             "               3dClipLevel works.\n"
             "\n"
             "  -prefix p = Save output into dataset with prefix 'p'\n"
             "               [default prefix is 'ATcorr'].\n"
             "\n"
             "  -time     = Save output as a 3D+time dataset instead\n"
             "               of a anat bucket.\n"
             "\n"
             "Notes:\n"
             " * The output dataset is anatomical bucket type of shorts.\n"
             " * The output file might be gigantic and you might run out\n"
             "    of memory running this program.  Use at your own risk!\n"
             " * The program prints out an estimate of its memory usage\n"
             "    when it starts.  It also prints out a progress 'meter'\n"
             "    of 1 dot per 10 output sub-bricks.\n"
             " * This is a quick hack for Peter Bandettini. Now pay up.\n"
             "\n"
             "-- RWCox - Jan 31 2002\n"
            ) ;
      /** PRINT_AFNI_OMP_USAGE("3dAutoTcorrelate",NULL) ; **/
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dAutoTcorrelate main"); machdep(); PRINT_VERSION("3dAutoTcorrelate");
   AFNI_logger("3dAutoTcorrelate",argc,argv);

   /*-- option processing --*/

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-time") == 0 ){
         abuc = 0 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-autoclip") == 0 ||
          strcmp(argv[nopt],"-automask") == 0   ){

         do_autoclip = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-pearson") == 0 ){
         method = PEARSON ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-spearman") == 0 ){
         method = SPEARMAN ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-quadrant") == 0 ){
         method = QUADRANT ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         prefix = argv[++nopt] ;
         if( !THD_filename_ok(prefix) ){
            ERROR_exit("Illegal value after -prefix!") ;
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-polort") == 0 ){
         char *cpt ;
         int val = strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < -1 || val > 3 ){
            ERROR_exit("Illegal value after -polort!") ;
         }
         polort = val ; nopt++ ; continue ;
      }

      ERROR_exit("Illegal option: %s",argv[nopt]) ;
   }

   /*-- open dataset, check for legality --*/

   if( nopt >= argc ) ERROR_exit("Need a dataset on command line!?") ;

   xset = THD_open_dataset(argv[nopt]); CHECK_OPEN_ERROR(xset,argv[nopt]);
   if( DSET_NVALS(xset) < 3 )
      ERROR_exit("Input dataset %s does not have 3 or more sub-bricks!",argv[nopt]) ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   /*-- compute mask array, if desired --*/

   nvox = DSET_NVOX(xset) ; nvals = DSET_NVALS(xset) ;

   if( do_autoclip ){
      mmm   = THD_automask( xset ) ;
      nmask = THD_countmask( nvox , mmm ) ;
      INFO_message("%d voxels survive -autoclip",nmask) ;
      if( nmask < 2 ) exit(1) ;
   } else {
      nmask = nvox ;
      INFO_message("computing for all %d voxels",nmask) ;
   }

   /*-- create output dataset --*/

   cset = EDIT_empty_copy( xset ) ;

   if( abuc ){
     EDIT_dset_items( cset ,
                        ADN_prefix    , prefix         ,
                        ADN_nvals     , nmask          ,
                        ADN_ntt       , 0              , /* no time axis */
                        ADN_type      , HEAD_ANAT_TYPE ,
                        ADN_func_type , ANAT_BUCK_TYPE ,
                      ADN_none ) ;
   } else {
     EDIT_dset_items( cset ,
                        ADN_prefix    , prefix         ,
                        ADN_nvals     , nmask          ,
                        ADN_ntt       , nmask          ,  /* num times */
                        ADN_ttdel     , 1.0            ,  /* fake TR */
                        ADN_nsl       , 0              ,  /* no slice offsets */
                        ADN_type      , HEAD_ANAT_TYPE ,
                        ADN_func_type , ANAT_EPI_TYPE  ,
                      ADN_none ) ;
   }

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(cset)) )
      ERROR_exit("Output dataset %s already exists!",DSET_HEADNAME(cset)) ;

   { double nb = (double)(xset->dblk->total_bytes) ;
     nb += (double)(nmask) * (double)(nvox) * sizeof(short) ;
     nb /= (1024.0*1024.0) ;
     INFO_message(
       "++ Memory required = %.1f Mbytes for %d output sub-bricks",nb,nmask);
     if( nb > 2000.0 )
       ERROR_exit("Can't use more than 2000 Mbytes of memory!");
   }

   tross_Make_History( "3dAutoTcorrelate" , argc,argv , cset ) ;

   /* loop over voxels, correlate */

   kout = 0 ;
   for( ii=0 ; ii < nvox ; ii++ ){  /* voxel to correlate with */

      if( mmm != NULL && mmm[ii] == 0 ) continue ; /* skip it */

      if( kout > 0 && kout%10 == 0 )
         fprintf(stderr, (kout%1000==0) ? "!"
                        :(kout%100 ==0) ? ":" : "." ) ;  /* progress meter */

      /* create and modify output brick */

      EDIT_substitute_brick( cset,kout, MRI_short, NULL ) ; /* make array */
      car = DSET_ARRAY(cset,kout) ;                         /* get array  */

      EDIT_BRICK_TO_FICO(cset,kout,nvals,1,polort+1) ;  /* stat params  */
      EDIT_BRICK_FACTOR(cset,kout,0.0001) ;             /* scale factor */

      ix = DSET_index_to_ix(cset,ii) ;                  /* brick label  */
      jy = DSET_index_to_jy(cset,ii) ;
      kz = DSET_index_to_kz(cset,ii) ;
      sprintf(str,"%03d_%03d_%03d",ix,jy,kz) ;
      EDIT_BRICK_LABEL(cset,kout,str) ;

      /* get ref time series from this voxel */

      xsim = THD_extract_series(ii,xset,0) ; xsar = MRI_FLOAT_PTR(xsim) ;

      DETREND_polort(polort,nvals,xsar) ;  /* remove polynomial trend */

      for( jj=0 ; jj < nvox ; jj++ ){  /* loop over voxels, correlate w/ref */

         if( mmm != NULL && mmm[jj] == 0 ){  /* the easy case */
            car[jj] = 0 ; continue ;
         }

         ysim = THD_extract_series(jj,xset,0) ; ysar = MRI_FLOAT_PTR(ysim) ;
         DETREND_polort(polort,nvals,ysar) ;

         switch( method ){                    /* correlate */
            default:
            case PEARSON:
              car[jj] = rint(10000.0*THD_pearson_corr (nvals,xsar,ysar));
            break;
            case SPEARMAN:
              car[jj] = rint(10000.0*THD_spearman_corr(nvals,xsar,ysar));
            break;
            case QUADRANT:
              car[jj] = rint(10000.0*THD_quadrant_corr(nvals,xsar,ysar));
            break;
         }

         mri_free(ysim) ;

      } /* end of loop over voxels in this sub-brick */

      mri_free(xsim) ;
      kout++ ;         /* move to next output brick */

   } /* end of loop over ref voxels */

   /* toss the other trash */

   DSET_unload(xset); if( mmm != NULL ) free(mmm);

   /* finito */

   DSET_write(cset) ;
   WROTE_DSET(cset) ;
   exit(0) ;
}
