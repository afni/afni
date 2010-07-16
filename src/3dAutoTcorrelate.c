#ifdef USE_OMP
#include <omp.h>
#endif

#include "mrilib.h"

#define SPEARMAN 1
#define QUADRANT 2
#define PEARSON  3
#define ETA2     4

#ifdef USE_OMP
#define pearson_func my_THD_pearson_corr
#define etasqrd_func my_THD_eta_squared
/*----------------------------------------------------------------*/
/**** Include these here for potential optimization for OpenMP ****/
/*----------------------------------------------------------------*/
/*! Pearson correlation of x[] and y[] (x and y are NOT modified. */

float my_THD_pearson_corr( int n, float *x , float *y )
{
   float xv,yv,xy , vv,ww , xm,ym ;
   register int ii ;

   xm = ym = 0.0f ;
   for( ii=0 ; ii < n ; ii++ ){ xm += x[ii] ; ym += y[ii] ; }
   xm /= n ; ym /= n ;
   xv = yv = xy = 0.0f ;
   for( ii=0 ; ii < n ; ii++ ){
     vv = x[ii]-xm ; ww = y[ii]-ym ; xv += vv*vv ; yv += ww*ww ; xy += vv*ww ;
   }

   if( xv <= 0.0f || yv <= 0.0f ) return 0.0f ;
   return xy/sqrtf(xv*yv) ;
}

/*----------------------------------------------------------------*/
/*! eta^2 (Cohen, NeuroImage 2008)              25 Jun 2010 [rickr]
 *
 *  eta^2 = 1 -  SUM[ (a_i - m_i)^2 + (b_i - m_i)^2 ]
 *               ------------------------------------
 *               SUM[ (a_i - M  )^2 + (b_i - M  )^2 ]
 *
 *  where  o  a_i and b_i are the vector elements
 *         o  m_i = (a_i + b_i)/2
 *         o  M = mean across both vectors
 -----------------------------------------------------------------*/

float my_THD_eta_squared( int n, float *x , float *y )
{
   float num , denom , gm , lm, vv, ww;
   register int ii ;

   gm = 0.0f ;
   for( ii=0 ; ii < n ; ii++ ){ gm += x[ii] + y[ii] ; }
   gm /= (2.0f*n) ;

   num = denom = 0.0f ;
   for( ii=0 ; ii < n ; ii++ ){
     lm = 0.5f * ( x[ii] + y[ii] ) ;
     vv = (x[ii]-lm); ww = (y[ii]-lm); num   += ( vv*vv + ww*ww );
     vv = (x[ii]-gm); ww = (y[ii]-gm); denom += ( vv*vv + ww*ww );
   }

   if( num < 0.0f || denom <= 0.0f || num >= denom ) return 0.0f ;
   return (1.0f - num/denom) ;
}
#else
#define pearson_func THD_pearson_corr  /* in thd_correlate.c */
#define etasqrd_func THD_eta_squared
#endif /* USE_OMP */

/*----------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static int nn=0 ;
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[nn%10] ) ;
   if( nn%10 == 9) fprintf(stderr,".") ;
   nn++ ;
}

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *xset , *cset, *mset=NULL ;
   int nopt=1 , method=PEARSON , do_autoclip=0 ;
   int nvox , nvals , ii,jj,kout , polort=1 , ix,jy,kz ;
   char * prefix = "ATcorr" ;
   byte * mmm=NULL ;
   int   nmask , abuc=1 ;
   int   all_source=0;          /* output all source voxels  25 Jun 2010 [rickr] */
   char str[32] ;
   int *imap ; MRI_vectim *xvectim ;

   /*----*/

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dAutoTcorrelate [options] dset\n"
             "Computes the correlation coefficient between each pair of\n"
             "voxels in the input dataset, and stores the output into\n"
             "a new anatomical bucket dataset.\n"
             "\n"
             "*** Also see program 3dTcorrMap ***\n"
             "\n"
             "Options:\n"
             "  -pearson  = Correlation is the normal Pearson (product moment)\n"
             "                correlation coefficient [default].\n"
             "  -eta2     = Output is eta^2 measure from Cohen, NeuroImage, 2008.\n"
             "                Note: -polort -1 is recommended with this option.\n"
#if 0
             "  -spearman = Correlation is the Spearman (rank) correlation\n"
             "                coefficient.\n"
             "  -quadrant = Correlation is the quadrant correlation coefficient.\n"
#else
             "  -spearman AND -quadrant are disabled at this time :-(\n"
#endif
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
             "  -mask MSET = Mask of both 'source' and 'target' voxels.\n"
             "\n"
             "              Restrict computations to those in the mask.  Output\n"
             "               volumes are restricted to masked voxels.  Also, only\n"
             "               masked voxels will have non-zero output.\n"
             "\n"
             "              A dataset with 1000 voxels would lead to output of\n"
             "               1000 voxels by 1000 sub-bricks.  With a -mask of 50\n"
             "               voxels, output would be 1000 voxels by 50 sub-bricks,\n"
             "               where the 950 unmasked voxels would be all zero over\n"
             "               the 50 sub-bricks.\n"
             "\n"
             "  -mask_only_targets = Provide output for all voxels.\n"
             "\n"
             "              Used with -mask, every voxel is correlated with each\n"
             "              of the mask voxels.  In the example above, there\n"
             "              would be 50 useful sub-bricks for all 1000 voxels.\n"
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
             "    * If you get an error message like\n"
             "        *** malloc error for dataset sub-brick\n"
             "      this means that the program ran out of memory when making\n"
             "      the output dataset.\n"
             " * The program prints out an estimate of its memory usage\n"
             "    when it starts.  It also prints out a progress 'meter'\n"
             "    to keep you pacified.\n"
             " * This is a quick hack for Peter Bandettini. Now pay up.\n"
             " * OpenMP-ized for Hang Joon Jo.  Where's my soju?\n"
             "\n"
             "-- RWCox - 31 Jan 2002 and 16 Jul 2010\n"
            ) ;
      PRINT_AFNI_OMP_USAGE("3dAutoTcorrelate",NULL) ;
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

      if( strcmp(argv[nopt],"-mask") == 0 ){
         mset = THD_open_dataset(argv[++nopt]);
         CHECK_OPEN_ERROR(mset,argv[nopt]);
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-mask_only_targets") == 0 ){
         all_source = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-pearson") == 0 ){
         method = PEARSON ; nopt++ ; continue ;
      }

#if 0
      if( strcmp(argv[nopt],"-spearman") == 0 ){
         method = SPEARMAN ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-quadrant") == 0 ){
         method = QUADRANT ; nopt++ ; continue ;
      }
#endif

      if( strcmp(argv[nopt],"-eta2") == 0 ){
         method = ETA2 ; nopt++ ; continue ;
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

   if( mset ){
      if( DSET_NVOX(mset) != nvox )
         ERROR_exit("Input and mask dataset differ in number of voxels!") ;
      mmm = THD_makemask(mset, 0, 1.0, 0.0) ;
      nmask = THD_countmask( nvox , mmm ) ;
      INFO_message("%d voxels in -mask dataset",nmask) ;
      if( nmask < 2 ) ERROR_exit("Only %d voxels in -mask, exiting...",nmask);
      DSET_unload(mset) ;
   } else if( do_autoclip ){
      mmm   = THD_automask( xset ) ;
      nmask = THD_countmask( nvox , mmm ) ;
      INFO_message("%d voxels survive -autoclip",nmask) ;
      if( nmask < 2 ) ERROR_exit("Only %d voxels in -automask!",nmask);
   } else {
      nmask = nvox ;
      INFO_message("computing for all %d voxels",nmask) ;
   }

   if( method == ETA2 && polort >= 0 )
      WARNING_message("Polort for -eta2 should probably be -1...");

   /*-- create vectim from input dataset --*/

   INFO_message("vectim-izing input dataset") ;
   xvectim = THD_dset_to_vectim( xset , NULL , 0 ) ;
   if( xvectim == NULL ) ERROR_exit("Can't create vectim?!") ;
   DSET_unload(xset) ;
   if( polort > 0 ){
     for( ii=0 ; ii < nvox ; ii++ ){  /* remove polynomial trend */
       DETREND_polort(polort,nvals,VECTIM_PTR(xvectim,ii)) ;
     }
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
       "Memory required = %.1f Mbytes for %d output sub-bricks",nb,nmask);
   }

   tross_Make_History( "3dAutoTcorrelate" , argc,argv , cset ) ;

   /*--- make output dataset first ---*/

   imap = (int *)calloc(sizeof(int),nmask) ;

   for( kout=ii=0 ; ii < nvox ; ii++ ){
      if( mmm != NULL && mmm[ii] == 0 ) continue ; /* skip it */

      EDIT_substitute_brick( cset,kout, MRI_short, NULL ) ; /* make array   */
      EDIT_BRICK_TO_NOSTAT(cset,kout) ;                     /* stat params  */
      EDIT_BRICK_FACTOR(cset,kout,0.0001) ;                 /* scale factor */

      ix = DSET_index_to_ix(cset,ii) ;                      /* brick label  */
      jy = DSET_index_to_jy(cset,ii) ;
      kz = DSET_index_to_kz(cset,ii) ;
      sprintf(str,"%03d_%03d_%03d",ix,jy,kz) ;
      EDIT_BRICK_LABEL(cset,kout,str) ;
      imap[kout] = ii ; kout++ ;
   }

   /* loop over mask voxels, correlate */

#pragma omp parallel if( nmask > 999 )
{
   int ii,jj,kout , ithr,nthr , vstep,vii ; float *xsar , *ysar ; short *car ;

AFNI_OMP_START ;

#ifdef USE_OMP
   ithr = omp_get_thread_num() ;
   nthr = omp_get_num_threads() ;
   if( ithr == 0 ) INFO_message("%d OpenMP threads started",nthr) ;
#else
   ithr = 0 ; nthr = 1 ;
#endif

   vstep = (int)( nmask / (nthr*50.0f) + 0.901f ) ; vii = 0 ;
   if( ithr == 0 ) fprintf(stderr,"Looping:") ;

#pragma omp for
   for( kout=0 ; kout < nmask ; kout++ ){

      ii = imap[kout] ;  /* we know that ii is in the mask */

      if( ithr == 0 ){ vii++ ; if( vii%vstep == vstep/2 ) vstep_print(); }

      /* get ref time series from this voxel */

      xsar = VECTIM_PTR(xvectim,ii) ;
      car  = DSET_ARRAY(cset,kout) ;

      for( jj=0 ; jj < nvox ; jj++ ){  /* loop over voxels, correlate w/ref */

         /* skip unmasked voxels, unless want correlations from all source voxels */

         if( !all_source && mmm != NULL && mmm[jj] == 0 ){  /* the easy case */
           car[jj] = 0 ; continue ;
         }

         ysar = VECTIM_PTR(xvectim,jj) ;

         switch( method ){                    /* correlate */
            default:
            case PEARSON:
              car[jj] = (short)(10000.49f*pearson_func(nvals,xsar,ysar));
            break;
            case ETA2:
              car[jj] = (short)(10000.49f*etasqrd_func(nvals,xsar,ysar));
            break;
#if 0
            case SPEARMAN:
              car[jj] = (short)(10000.49*THD_spearman_corr(nvals,xsar,ysar));
            break;
            case QUADRANT:
              car[jj] = (short)(10000.49*THD_quadrant_corr(nvals,xsar,ysar));
            break ;
#endif
         }
      } /* end of loop over voxels in this sub-brick */

   } /* end of loop over ref voxels */

AFNI_OMP_END ;
}
   fprintf(stderr," Done!\n") ;

   /* toss the other trash */

   free(imap); VECTIM_destroy(xvectim); if( mmm != NULL ) free(mmm);

   /* finito */

   INFO_message("Writing output dataset to disk") ;
   DSET_write(cset) ;
   WROTE_DSET(cset) ;
   exit(0) ;
}
