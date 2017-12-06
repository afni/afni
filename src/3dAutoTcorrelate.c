#ifdef USE_OMP
#include <omp.h>
#endif

#include "mrilib.h"
#include <sys/mman.h>
#include <sys/types.h>

#define SPEARMAN 1
#define QUADRANT 2
#define PEARSON  3
#define ETA2     4

#define ALLOW_MMAP

#ifndef ALLOW_MMAP
# define do_mmap 0
#else
  static int do_mmap = 0 ;
#endif

/*----------------------------------------------------------------*/
/**** Include these here for potential optimization for OpenMP ****/
/*----------------------------------------------------------------*/
/*! Pearson correlation of x[] and y[] (x and y are NOT modified.
    And we know ahead of time that the time series have 0 mean
    and L2 norm 1.
*//*--------------------------------------------------------------*/

float zm_THD_pearson_corr( int n, float *x , float *y ) /* inputs are */
{                                                       /* zero mean  */
   register float xy ; register int ii ;                /* and norm=1 */
   if( n%2 == 0 ){
     xy = 0.0f ;
     for( ii=0 ; ii < n ; ii+=2 ) xy += x[ii]*y[ii] + x[ii+1]*y[ii+1] ;
   } else {
     xy = x[0]*y[0] ;
     for( ii=1 ; ii < n ; ii+=2 ) xy += x[ii]*y[ii] + x[ii+1]*y[ii+1] ;
   }
   return xy ;
}

/*----------------------------------------------------------------*/
/* General correlation calculation. */

#if 0
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
#endif

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

/*----------------------------------------------------------------------------*/

static void vstep_print(void)
{
   static int nn=0 ;
   static char xx[10] = "0123456789" ;
   fprintf(stderr , "%c" , xx[nn%10] ) ;
   if( nn%10 == 9) fprintf(stderr,",") ;
   nn++ ;
}

/*-----------------------------------------------------------------------------*/

#ifdef ALLOW_MMAP

static char    *cbrik = NULL ;
static int64_t ncbrik = 0 ;
static char    *cbrik_name = NULL ;

#undef MFLAG
#if defined(MAP_NOCACHE)
# define MFLAG (MAP_SHARED | MAP_NOCACHE)
#elif defined(MAP_NORESERVE)
# define MFLAG (MAP_SHARED | MAP_NORESERVE)
#else
# define MFLAG MAP_SHARED
#endif

#include <signal.h>

void AC_sigfunc(int sig)   /** signal handler for fatal errors **/
{
   char *sname ;
   static volatile int fff=0 ;
   if( fff ) _exit(1) ; else fff = 1 ;
   switch(sig){
      default:      sname = "unknown" ; break ;
      case SIGINT:  sname = "SIGINT"  ; break ;
      case SIGPIPE: sname = "SIGPIPE" ; break ;
      case SIGSEGV: sname = "SIGSEGV" ; break ;
      case SIGBUS:  sname = "SIGBUS"  ; break ;
      case SIGTERM: sname = "SIGTERM" ; break ;
   }
   fprintf(stderr,"\n** 3dAutoTcorrelate: Fatal Signal %d (%s) received\n",sig,sname) ;
   if( cbrik != NULL ){
     fprintf(stderr,"** Un-mmap-ing %s file (but not deleting it)\n",cbrik_name) ;
     munmap(cbrik,ncbrik) ; cbrik = NULL ;
   }
   exit(1) ;
}

#endif  /* ALLOW_MMAP */

/*----------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   THD_3dim_dataset *xset , *cset, *mset=NULL, *msetinner=NULL ;
   int nopt=1 , method=PEARSON , do_autoclip=0 ;
   int nvox , nvals , ii,kout , polort=1 , ix,jy,kz ;
   char *prefix = "ATcorr" ;
   byte *mask=NULL, *maskinner=NULL;
   int   nmask , nmaskinner , abuc=1 ;
   int   all_source=0;        /* output all source voxels  25 Jun 2010 [rickr] */
   char str[32] , *cpt ;
   int *imap ; MRI_vectim *xvectim ;
   float (*corfun)(int,float *,float*) = NULL ;

   FILE *fout1D=NULL;
   
   /*----*/

   AFNI_SETUP_OMP(0) ;  /* 24 Jun 2013 */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf(
"Usage: 3dAutoTcorrelate [options] dset\n"
"Computes the correlation coefficient between the time series of each\n"
"pair of voxels in the input dataset, and stores the output into a\n"
"new anatomical bucket dataset [scaled to shorts to save memory space].\n"
"\n"
"*** Also see program 3dTcorrMap ***\n"
"\n"
"Options:\n"
"  -pearson  = Correlation is the normal Pearson (product moment)\n"
"               correlation coefficient [default].\n"
"  -eta2     = Output is eta^2 measure from Cohen et al., NeuroImage, 2008:\n"
"               http://www.ncbi.nlm.nih.gov/pmc/articles/PMC2705206/\n"
"               http://dx.doi.org/10.1016/j.neuroimage.2008.01.066\n"
"             ** '-eta2' is intended to be used to measure the similarity\n"
"                 between 2 correlation maps; therefore, this option is\n"
"                 to be used in a second stage analysis, where the input\n"
"                 dataset is the output of running 3dAutoTcorrelate with\n"
"                 the '-pearson' option -- the voxel 'time series' from\n"
"                 that first stage run is the correlation map of that\n"
"                 voxel with all other voxels.\n"
"             ** '-polort -1' is recommended with this option!\n"
"             ** Odds are you do not want use this option if the dataset\n"
"                on which eta^2 is to be computed was generated with\n"
"                options -mask_only_targets or -mask_source.\n"
"                In this program, the eta^2 is computed between pseudo-\n"
"                timeseries (the 4th dimension of the dataset).\n"
"                If you want to compute eta^2 between sub-bricks then use\n"
"                3ddot -eta2 instead.\n"
   #if 0
"  -spearman = Correlation is the Spearman (rank) correlation\n"
"               coefficient.\n"
"  -quadrant = Correlation is the quadrant correlation coefficient.\n"
   #else
"  -spearman AND -quadrant are disabled at this time :-(\n"
   #endif
"\n"
"  -polort m = Remove polynomial trend of order 'm', for m=-1..3.\n"
"               [default is m=1; removal is by least squares].\n"
"               Using m=-1 means no detrending; this is only useful\n"
"               for data/information that has been pre-processed.\n"
"\n"
"  -autoclip = Clip off low-intensity regions in the dataset,\n"
"  -automask =  so that the correlation is only computed between\n"
"               high-intensity (presumably brain) voxels.  The\n"
"               mask is determined the same way that 3dAutomask works.\n"
"\n"
"  -mask mmm = Mask of both 'source' and 'target' voxels.\n"
"              ** Restricts computations to those in the mask.  Output\n"
"                  volumes are restricted to masked voxels.  Also, only\n"
"                  masked voxels will have non-zero output.\n"
"              ** A dataset with 1000 voxels would lead to output of\n"
"                  1000 sub-bricks.  With a '-mask' of 50 voxels, the\n"
"                  output dataset have 50 sub-bricks, where the 950\n"
"                  unmasked voxels would be all zero in all 50 sub-bricks\n"
"                  (unless option '-mask_only_targets' is also used).\n"
"              ** The mask is encoded in the output dataset header in the\n"
"                  attribute named 'AFNI_AUTOTCORR_MASK' (cf. 3dMaskToASCII).\n"
"\n"
"  -mask_only_targets = Provide output for all voxels.\n"
"              ** Used with '-mask': every voxel is correlated with each\n"
"                  of the mask voxels.  In the example above, there would\n"
"                  be 50 output sub-bricks; the n-th output sub-brick\n"
"                  would contain the correlations of the n-th voxel in\n"
"                  the mask with ALL 1000 voxels in the dataset (rather\n"
"                  than with just the 50 voxels in the mask).\n"
"\n"
"  -mask_source sss = Provide ouput for voxels only in mask sss\n"
"               ** For each seed in mask mm, compute correlations only with \n"
"                   non-zero voxels in sss. If you have 250 non-zero voxels \n"
"                   in sss, then the output will still have 50 sub-bricks, but\n"
"                   each n-th sub-brick will have non-zero values at the 250\n"
"                   non-zero voxels in sss\n"
"                   Do not use this option along with -mask_only_targets\n"
"\n"
"  -prefix p = Save output into dataset with prefix 'p'\n"
"               [default prefix is 'ATcorr'].\n"
"  -out1D FILE.1D = Save output in a text file formatted thusly:\n"
"                   Row 1 contains the 1D indices of non zero voxels in the \n"
"                         mask from option -mask.\n"
"                   Column 1 contains the 1D indices of non zero voxels in the\n"
"                         mask from option -mask_source\n"
"                   The rest of the matrix contains the correlation/eta2 \n"
"                   values. Each column k corresponds to sub-brick k in \n"
"                   the output volume p.\n"
"                   To see 1D indices in AFNI, right click on the top left\n"
"                   corner of the AFNI controller - where coordinates are\n"
"                   shown - and chose voxel indices.\n"
"                   A 1D index (ijk) is computed from the 3D (i,j,k) indices:\n"
"                       ijk = i + j*Ni + k*Ni*Nj , with Ni and Nj being the\n"
"                   number of voxels in the slice orientation and given by:\n"
"                       3dinfo -ni -nj YOUR_VOLUME_HERE\n"
"                   This option can only be used in conjunction with \n"
"                   options -mask and -mask_source. Otherwise it makes little\n"
"                   sense to write a potentially enormous text file.\n"
"\n"
"  -time     = Mark output as a 3D+time dataset instead of an anat bucket.\n"
   #ifdef ALLOW_MMAP
"\n"
"  -mmap     = Write .BRIK results to disk directly using Unix mmap().\n"
"               This trick can speed the program up  when the amount\n"
"               of memory required to hold the output is very large.\n"
"              ** In many case, the amount of time needed to write\n"
"                 the results to disk is longer than the CPU time.\n"
"                 This option can shorten the disk write time.\n"
"              ** If the program crashes, you'll have to manually\n"
"                 remove the .BRIK file, which will have been created\n"
"                 before the loop over voxels and written into during\n"
"                 that loop, rather than being written all at once\n"
"                 at the end of the analysis, as is usually the case.\n"
"              ** If the amount of memory needed is bigger than the\n"
"                 RAM on your system, this program will be very slow\n"
"                 with or without '-mmap'.\n"
"              ** This option won't work with NIfTI-1 (.nii) output!\n"
   #else
"\n"
"  -mmap is disabled at this time :-(\n"
   #endif
"\n"
"Example: correlate every voxel in mask_in+tlrc with only those voxels in\n"
"         mask_out+tlrc (the rest of each volume is zero, for speed).\n"
"         Assume detrending was already done along with other pre-processing.\n"
"         The output will have one volume per masked voxel in mask_in+tlrc.\n"
"         Volumes will be labeled by the ijk index triples of mask_in+tlrc.\n"
"\n"
"   3dAutoTcorrelate -mask_source mask_out+tlrc -mask mask_in+tlrc \\\n"
"                    -polort -1 -prefix test_corr clean_epi+tlrc\n"
"\n"
"Notes:\n"
" * The output dataset is anatomical bucket type of shorts\n"
"    (unless '-time' is used).\n"
" * Values are scaled so that a correlation (or eta-squared)\n"
"    of 1 corresponds to a value of 10000.\n"
" * The output file might be gigantic and you might run out\n"
"    of memory running this program.  Use at your own risk!\n"
"   ++ If you get an error message like\n"
"        *** malloc error for dataset sub-brick\n"
"      this means that the program ran out of memory when making\n"
"      the output dataset.\n"
   #ifdef ALLOW_MMAP
"   ++ If this happens, you can try to use the '-mmap' option,\n"
"      and if you are lucky, the program may actually run.\n"
   #endif
" * The program prints out an estimate of its memory usage\n"
"    when it starts.  It also prints out a progress 'meter'\n"
"    to keep you pacified.\n"
" * This is a quick hack for Peter Bandettini. Now pay up.\n"
" * OpenMP-ized for Hang Joon Jo.  Where's my baem-sul?\n"
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

#ifdef ALLOW_MMAP
      if( strcmp(argv[nopt],"-mmap") == 0 ){  /* Jul 2010 */
        do_mmap = 1 ; nopt++ ; continue ;
      }
#endif

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

      if( strcmp(argv[nopt],"-mask_source") == 0 ){
         msetinner = THD_open_dataset(argv[++nopt]);
         CHECK_OPEN_ERROR(msetinner,argv[nopt]);
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-out1D") == 0 ){
         if (!(fout1D = fopen(argv[++nopt], "w"))) {
            ERROR_message("Failed to open %s for writing", argv[nopt]);
            exit(1);
         }
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
         prefix = strdup(argv[++nopt]) ;
         if( !THD_filename_ok(prefix) ){
            ERROR_exit("Illegal value after -prefix!") ;
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-polort") == 0 ){
         int val = (int)strtod(argv[++nopt],&cpt) ;
         if( *cpt != '\0' || val < -1 || val > 3 ){
            ERROR_exit("Illegal value after -polort!") ;
         }
         polort = val ; nopt++ ; continue ;
      }

      ERROR_exit("Illegal option: %s",argv[nopt]) ;
   }

#ifdef ALLOW_MMAP
   cpt = strstr(prefix,".nii") ;  /* Jul 2010 */
   if( do_mmap && cpt != NULL ){
     *cpt = '\0' ; if( *prefix == '\0' ) prefix = "ATcorr" ;
     WARNING_message("-mmap ==> can't use NIfTI-1 ==> output prefix = %s",prefix) ;
   }
#endif

   /*-- open dataset, check for legality --*/

   if( nopt >= argc ) ERROR_exit("Need a dataset on command line!?") ;

   if (fout1D && (!mset || !msetinner)) {
      fclose(fout1D);
      ERROR_message("Must use -mask and -mask_source with -out1D");
      exit(1);
   }

   xset = THD_open_dataset(argv[nopt]); CHECK_OPEN_ERROR(xset,argv[nopt]);
   if( DSET_NVALS(xset) < 3 )
     ERROR_exit("Input dataset %s does not have 3 or more sub-bricks!",argv[nopt]) ;
   DSET_load(xset) ; CHECK_LOAD_ERROR(xset) ;

   /*-- compute mask array, if desired --*/

   nvox = DSET_NVOX(xset) ; nvals = DSET_NVALS(xset) ;

   if( mset ){
      if( DSET_NVOX(mset) != nvox )
         ERROR_exit("Input and mask dataset differ in number of voxels!") ;
      mask  = THD_makemask(mset, 0, 1.0, 0.0) ;
      nmask = THD_countmask( nvox , mask ) ;
      INFO_message("%d voxels in -mask dataset",nmask) ;
      if( nmask < 2 ) ERROR_exit("Only %d voxels in -mask, exiting...",nmask);
      DSET_unload(mset) ;
   } else if( do_autoclip ){
      mask  = THD_automask( xset ) ;
      nmask = THD_countmask( nvox , mask ) ;
      INFO_message("%d voxels survive -autoclip",nmask) ;
      if( nmask < 2 ) ERROR_exit("Only %d voxels in -automask!",nmask);
   } else {
      nmask = nvox ;
      INFO_message("computing for all %d voxels",nmask) ;
   }
   if (msetinner) {
      if( DSET_NVOX(msetinner) != nvox )
         ERROR_exit("Input and mask_source dataset differ in number of voxels!");
      maskinner  = THD_makemask(msetinner, 0, 1.0, 0.0) ;
      nmaskinner = THD_countmask( nvox , maskinner ) ;
      INFO_message("%d voxels in -mask dataset",nmaskinner) ;
      if( nmaskinner < 2 ) 
         ERROR_exit("Only %d voxels in -mask, exiting...",nmaskinner);
      DSET_unload(msetinner) ;
      DSET_delete(msetinner); msetinner = NULL;
   } else {
      if (mask) {
         maskinner = mask;
         nmaskinner = -nvox;  /* a flag to be sure we remember this deed */
      }
   }
   
   if( method == ETA2 && polort >= 0 )
      WARNING_message("Polort for -eta2 should probably be -1...");

   /*-- create vectim from input dataset --*/

   /**  For the case of Pearson correlation, we make sure the  **/
   /**  data time series have their mean removed (polort >= 0) **/
   /**  and are normalized, so that correlation = dot product, **/
   /**  and we can use function zm_THD_pearson_corr for speed. **/

   switch( method ){
     default:
     case PEARSON: corfun = zm_THD_pearson_corr ; break ;
     case ETA2:    corfun = my_THD_eta_squared  ; break ;
   }

   INFO_message("vectim-izing input dataset") ;
   xvectim = THD_dset_to_vectim( xset , NULL , 0 ) ;
   if( xvectim == NULL ) ERROR_exit("Can't create vectim?!") ;
   DSET_unload(xset) ;
   if( polort < 0 && method == PEARSON ){
     polort = 0; WARNING_message("Pearson correlation always uses polort >= 0");
   }
   if( polort >= 0 ){
     for( ii=0 ; ii < nvox ; ii++ ){  /* remove polynomial trend */
       DETREND_polort(polort,nvals,VECTIM_PTR(xvectim,ii)) ;
     }
   }
   if( method == PEARSON ) THD_vectim_normalize(xvectim) ;  /* L2 norm = 1 */

   /*-- create output dataset --*/

   cset = EDIT_empty_copy( xset ) ;

   if( abuc ){
     EDIT_dset_items( cset ,
                        ADN_prefix    , prefix         ,
                        ADN_nvals     , nmask          ,
                        ADN_ntt       , 0              , /* no time axis */
                        ADN_type      , HEAD_ANAT_TYPE ,
                        ADN_func_type , ANAT_BUCK_TYPE ,
                        ADN_datum_all , MRI_short      ,
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
                        ADN_datum_all , MRI_short      ,
                      ADN_none ) ;
   }

   if( THD_deathcon() && THD_is_file(DSET_HEADNAME(cset)) )
     ERROR_exit("Output dataset %s already exists!",DSET_HEADNAME(cset)) ;

   { double nb = (double)(xset->dblk->total_bytes)
                +(double)(cset->dblk->total_bytes) ;
     nb /= 1000000 ;
     INFO_message(
       "Memory required = %.1f Mbytes for %d output sub-bricks",nb,nmask);
     if( nb > 2000.0 && (sizeof(void *) < 8 || sizeof(size_t) < 8) )
       ERROR_exit("Can't run on a 32-bit system!") ;
#ifdef ALLOW_MMAP
     if( nb > 1000.0 && !do_mmap )
       INFO_message("If you run out of memory, try using the -mmap option.") ;
#endif
   }

   tross_Make_History( "3dAutoTcorrelate" , argc,argv , cset ) ;

   /*--- make output dataset first ---*/

   imap = (int *)calloc(sizeof(int),nmask) ;

   if( !do_mmap )
     ININFO_message("creating output dataset in memory") ;

   for( kout=ii=0 ; ii < nvox ; ii++ ){
      if( mask != NULL && mask[ii] == 0 ) continue ; /* skip it */

      EDIT_BRICK_TO_NOSTAT(cset,kout) ;                     /* stat params  */
      EDIT_BRICK_FACTOR(cset,kout,0.0001) ;                 /* scale factor */

      ix = DSET_index_to_ix(cset,ii) ;                      /* brick label  */
      jy = DSET_index_to_jy(cset,ii) ;
      kz = DSET_index_to_kz(cset,ii) ;
      sprintf(str,"%03d_%03d_%03d",ix,jy,kz) ;
      EDIT_BRICK_LABEL(cset,kout,str) ;
      if( !do_mmap )
        EDIT_substitute_brick(cset,kout,MRI_short,NULL) ;   /* make array   */

      imap[kout] = ii ; kout++ ;
   }

#ifdef ALLOW_MMAP
   if( do_mmap ){
     FILE *fp ; int64_t qq,dq ; char buf[2]={0,0} ;

     signal(SIGINT ,AC_sigfunc) ;  /* setup signal handler */
     signal(SIGBUS ,AC_sigfunc) ;  /* for fatal errors */
     signal(SIGSEGV,AC_sigfunc) ;
     signal(SIGTERM,AC_sigfunc) ;

     cbrik_name = strdup(DSET_BRIKNAME(cset)) ;   /* save name for later use */
     ncbrik = cset->dblk->total_bytes ;
     ININFO_message("creating output file %s via mmap() [%s bytes -- about %s]",
                    cbrik_name , commaized_integer_string(ncbrik) ,
                                 approximate_number_string((double)ncbrik) ) ;
     fp = fopen(cbrik_name,"w+") ;
     if( fp == NULL ) ERROR_exit("Can't create output BRIK file :-(") ;
     fseek( fp , ncbrik-1 , SEEK_SET ) ;
     fwrite( &buf , 1 , 2 , fp ) ; fflush(fp) ;
     cbrik = mmap( 0 , (size_t)ncbrik ,
                   PROT_READ | PROT_WRITE , MFLAG , fileno(fp) , 0 ) ;
     fclose(fp) ;
     if( cbrik == (char *)(-1) ){
       remove(cbrik_name) ; ERROR_exit("Can't mmap output BRIK file :-(") ;
     }
     ININFO_message("Testing output BRIK") ;
     dq = ncbrik / 65536 ; if( dq < 1024 ) dq = 1024 ;
     for( qq=0 ; qq < ncbrik ; qq+=dq ) cbrik[qq] = 0 ;
     cbrik[ncbrik-1] = 0 ;
     ININFO_message("... done with test") ;
   }
#endif

   /*---------- loop over mask voxels, correlate ----------*/

AFNI_OMP_START ;
#pragma omp parallel if( nmask > 999 )
{
   int ii,jj,kout , ithr,nthr , vstep,vii ;
   float *xsar , *ysar ; short *car=NULL ;

#ifdef USE_OMP
   ithr = omp_get_thread_num() ;
   nthr = omp_get_num_threads() ;
   if( ithr == 0 ) INFO_message("%d OpenMP threads started",nthr) ;
#else
   ithr = 0 ; nthr = 1 ;
#endif

#ifdef ALLOW_MMAP
   if( do_mmap ) car = (short *)malloc(sizeof(short)*nvox) ;
#endif

   vstep = (int)( nmask / (nthr*50.0f) + 0.901f ) ; vii = 0 ;
   if( ithr == 0 ) fprintf(stderr,"Looping:") ;

#pragma omp for
   for( kout=0 ; kout < nmask ; kout++ ){  /*----- outer voxel loop -----*/

      ii = imap[kout] ;  /* ii= source voxel (we know that ii is in the mask) */

      if( ithr == 0 && vstep > 2 ) /* allow small dsets 16 Jun 2011 [rickr] */
      { vii++ ; if( vii%vstep == vstep/2 ) vstep_print(); }

      /* get ref time series from this voxel */

      xsar = VECTIM_PTR(xvectim,ii) ;
      if( !do_mmap ) car = DSET_ARRAY(cset,kout) ;

      for( jj=0 ; jj < nvox ; jj++ ){  /*----- inner loop over voxels -----*/

         car[jj] = 0 ;  /* default value */

         /* skip unmasked voxels, unless want correlations from all source voxels */

         if( !all_source && maskinner != NULL && maskinner[jj] == 0 ) continue ;

#if 0
         if( jj == kout ){ car[jj] = 10000 ; continue ; } /* correlation = 1.0 */
#endif

         ysar = VECTIM_PTR(xvectim,jj) ;

         car[jj] = (short)(10000.49f*corfun(nvals,xsar,ysar)) ;

      } /* end of inner loop over voxels */

#ifdef ALLOW_MMAP
      if( do_mmap ){  /* copy results to disk mmap now */
        short *cout = ((short *)(cbrik)) + (int64_t)(nvox)*(int64_t)(kout) ;
        AAmemcpy( cout , car , sizeof(short)*nvox ) ;
      }
#endif

   } /* end of outer loop over ref voxels */

   if( do_mmap ) free(car) ;             /* workspace for each thread */
   if( ithr == 0 ) fprintf(stderr,".") ;

} /* end OpenMP */
AFNI_OMP_END ;

   /*----------  Finish up ---------*/

   /* write mask info (if any) to output dataset header */

   if( mask != NULL ){
     char *maskstring = mask_to_b64string(nvox,mask) ;
     THD_set_string_atr( cset->dblk , "AFNI_AUTOTCORR_MASK" , maskstring ) ;
     free(maskstring) ; free(mask) ;
     if( ISVALID_DSET(mset) ){
       THD_set_string_atr( cset->dblk , "AFNI_AUTOCORR_MASK_IDCODE" ,
                                        DSET_IDCODE_STR(mset)        ) ;
       THD_set_string_atr( cset->dblk , "AFNI_AUTOCORR_MASK_NAME"   ,
                                        DSET_HEADNAME(mset)          ) ;
       DSET_delete(mset) ;
     }
   }
   
   
   /* toss some trash */

   VECTIM_destroy(xvectim) ;

   /* if did mmap(), finish that off as well */

   if( !do_mmap ){
     fprintf(stderr,"Done..\n") ;
   } else {
#ifdef ALLOW_MMAP
     char *ptr = cbrik ;
     fprintf(stderr,"Done.[un-mmap-ing") ;
     for( ii=0 ; ii < nmask ; ii++ ){  /* map sub-bricks */
       mri_fix_data_pointer( ptr , DSET_BRICK(cset,ii) ) ;
       ptr += DSET_BRICK_BYTES(cset,ii) ;
     }
     THD_load_statistics(cset) ;       /* so can do stats */
     fprintf(stderr,".") ;
     munmap(cbrik,ncbrik) ; cbrik = NULL ;
     fprintf(stderr,"].\n") ;
#endif
   }

   if (fout1D) { /* write results to ASCII file, hopefully when masked */
      if (fout1D && (!mask || !maskinner)) {
         ERROR_message("Option -1Dout restricted to commands using"
                       "-mask and -mask_source options."); 
      } else {
         float *far = (float *)calloc(nmask, sizeof(float));
         int jj;
         fprintf(fout1D,"#Text output of:\n#");
         for (ii=0; ii<argc; ++ii) fprintf(fout1D,"%s ", argv[ii]);
         fprintf(fout1D,"\n"
              "#First row contains 1D indices of voxels from -mask\n"
              "#First column contains 1D indices of voxels from -mask_source\n");
         /* write the matrix */
         fprintf(fout1D,"           ");
         for( ii=0 ; ii < nmask ; ii++ ) {
            fprintf(fout1D,"%08d ", imap[ii]);
         }
         fprintf(fout1D," #Mask Voxel Indices (from -mask)");
         for( jj=0 ; jj < nvox ; jj++ ){
            if (maskinner[jj]) {
               fprintf(fout1D,"\n%08d   ", jj);
               THD_extract_float_array( jj, cset, far );
               for (ii=0; ii < nmask; ++ii)
                  fprintf(fout1D,"%f ", far[ii]);
            } 
         }
         free(far); far=NULL;
         fclose(fout1D); fout1D = NULL;  
      }
   }
   
   /* free inner mask if not a copy of mask */
   if ( maskinner && maskinner != mask) {
      free(maskinner); 
   }
   nmaskinner = 0;
   maskinner=NULL;
   if (imap) free(imap) ; imap = NULL;
   
   /* finito */

   if( !do_mmap ){
     INFO_message("Writing output dataset to disk [%s bytes]",
                  commaized_integer_string(cset->dblk->total_bytes)) ;
     THD_set_write_compression(COMPRESS_NONE) ; AFNI_setenv("AFNI_AUTOGZIP=NO") ;
     DSET_write(cset) ;
   } else {
     INFO_message("Writing output dataset header") ;
     DSET_write_header(cset) ;
   }
   WROTE_DSET(cset) ;

   exit(0) ;
}
