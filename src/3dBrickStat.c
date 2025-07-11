/*********************** 3dBrickStat.c *************************************/
/* Author: Daniel Glen, 26 Apr 2005 */
#include "mrilib.h"
#include "thd_shear3d.h"

static int datum                   = MRI_float ;
static void Print_Header_MinMax(int Minflag, int Maxflag, 
                                THD_3dim_dataset * dset);
static void Max_func(int Minflag, int Maxflag, int Meanflag, int Countflag,
                     int Posflag, int Negflag, int Zeroflag, int Absflag, 
                     int nan_flag, int Sumflag,
                     int Varflag, int Volflag, THD_3dim_dataset * dset, 
                     byte *mmm, int mmvox);

void usage_3dBrickStat(int detail) {
   printf(
"Usage: 3dBrickStat [options] dataset\n"
"Compute maximum and/or minimum voxel values of an input dataset\n"
"\n"
"The output is a number to the console.  The input dataset\n"
"may use a sub-brick selection list, as in program 3dcalc.\n"
"\n"
"Note that this program computes ONE number as the output; e.g.,\n"
"the mean over all voxels and time points.  If you want (say) the\n"
"mean over all voxels but for each time point individually, see\n"
"program 3dmaskave.\n"
"\n"
"Note: If you don't specify one sub-brick, the parameter you get\n"
"----- back is computed from all the sub-bricks in dataset.\n"
"Options :\n"
"  -quick = get the information from the header only (default)\n"
"  -slow = read the whole dataset to find the min and max values\n"
"         all other options except min and max imply slow\n"
"  -min = print the minimum value in dataset\n"
"  -max = print the maximum value in dataset (default)\n"
"  -mean = print the mean value in dataset \n"
"  -sum = print the sum of values in the dataset\n"
"  -var = print the variance in the dataset \n"
"  -stdev = print the standard deviation in the dataset \n"
"           -stdev and -var are mutually exclusive\n"
"  -count = print the number of voxels included\n"
"  -volume = print the volume of voxels included in microliters\n"
"  -positive = include only positive voxel values \n"
"  -negative = include only negative voxel values \n"
"  -zero = include only zero voxel values \n"
"  -non-positive = include only voxel values 0 or negative \n"
"  -non-negative = include only voxel values 0 or greater \n"
"  -non-zero = include only voxel values not equal to 0 \n"
"  -absolute = use absolute value of voxel values for all calculations\n"
"              can be combined with restrictive non-positive, non-negative,\n"
"              etc. even if not practical. Ignored for percentile and\n"
"              median computations.\n"
"  -nan = include only voxel values that are not numbers (e.g., NaN or inf).\n"
"         This is basically meant for counting bad numbers in a dataset.\n"
"         -nan forces -slow mode.\n"
"  -nonan = exclude voxel values that are not numbers\n"
"           (exclude any NaN or inf values from computations).\n"
"  -mask dset = use dset as mask to include/exclude voxels\n"
"  -mrange MIN MAX = Only accept values between MIN and MAX (inclusive)\n"
"                    from the mask. Default it to accept all non-zero\n"
"                    voxels.\n"
"  -mvalue VAL = Only accept values equal to VAL from the mask.\n"
"  -automask = automatically compute mask for dataset\n"
"    Can not be combined with -mask\n"
"  -percentile p0 ps p1 write the percentile values starting\n"
"              at p0%% and ending at p1%% at a step of ps%%\n"
"              Output is of the form p%% value   p%% value ...\n"
"              Percentile values are output first. \n"
"              Only one sub-brick is accepted as input with this option.\n"
"              Write the author if you REALLY need this option\n"
"              to work with multiple sub-bricks.\n"
"  -perclist NUM_PERC PERC1 PERC2 ...\n"
"              Like -percentile, but output the given percentiles, rather\n"
"              than a list on an evenly spaced grid using 'ps'.\n"
"  -median a shortcut for -percentile 50 1 50 (or -perclist 1 50)\n"
"  -perc_quiet = only print percentile results, not input percentile cutoffs\n"
"  -ver = print author and version info\n"
"  -help = print this help screen\n"
) ;
   printf("\n" MASTER_SHORTHELP_STRING ) ;
   PRINT_COMPILE_DATE ;
   return;
}
/*
  static void Max_tsfunc( double tzero , double tdelta ,
  int npts , float ts[] , double ts_mean ,
  double ts_slope , void *ud , int nbriks, float *val ) ;
  static float minvalue=1E10, maxvalue=-1E10;
*/
 
/*! compute the overall minimum and maximum voxel values for a dataset */
int main( int argc , char * argv[] )
{
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   int nopt, nbriks;
   int slow_flag, quick_flag, min_flag, max_flag, mean_flag, 
      automask,count_flag, sum_flag, var_flag, absolute_flag;
   int positive_flag, negative_flag, zero_flag, nan_flag, perc_flag, vol_flag;

   byte * mmm=NULL ;
   int    mmvox=0 ;
   int nxyz, i;
   float *dvec = NULL, mmin=0.0, mmax=0.0;
   int N_mp=0, perc_quiet=0;
   double *mpv=NULL, *perc = NULL;
   double mp =0.0f, mp0 = 0.0f, mps = 0.0f, mp1 = 0.0f, di =0.0f ;
   byte *mmf = NULL;
   MRI_IMAGE *anat_im = NULL;
   char *mask_dset_name=NULL;
   void *tmp_vec = NULL;

   /*----- Read command line -----*/

   mainENTRY("3dBrickStat main"); machdep(); 
   AFNI_logger("3dBrickStat",argc,argv);
   nopt = 1 ;

   min_flag  = 0;
   max_flag = -1;
   mean_flag = 0;
   sum_flag = 0;
   var_flag = 0;
   slow_flag = 0;
   quick_flag = -1;
   automask = 0;
   count_flag = 0;
   vol_flag = 0;
   positive_flag = -1;
   negative_flag = -1;
   absolute_flag = 0;
   zero_flag = -1;
   nan_flag = -1;
   perc_flag = 0;
   mmin = 1.0;
   mmax = -1.0;
   mask_dset_name = NULL;      
   
   datum = MRI_float;
   while( nopt < argc && argv[nopt][0] == '-' ){
      if( strcmp(argv[nopt],"-help") == 0 ||
          strcmp(argv[nopt],"-h") == 0){
         usage_3dBrickStat(strlen(argv[nopt])> 3 ? 2:1);
         exit(0);
      }
      
      if( strcmp(argv[nopt],"-ver") == 0 ){
         PRINT_VERSION("3dBrickStat"); AUTHOR("Daniel Glen");
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-quick") == 0 ){
         quick_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-percentile") == 0 ){
         perc_flag = 1;
         ++nopt;
         if (nopt + 2 >= argc) {
            ERROR_exit( "** Error: Need 3 parameter after -percentile\n"); 
         }
         mp0 = atof(argv[nopt])/100.0f; ++nopt;
         mps = atof(argv[nopt])/100.0f; ++nopt;
         mp1 = atof(argv[nopt])/100.0f; 
         if (mps == 0.0f) {
            ERROR_exit( "** Error: step cannot be 0" ); 
         }
         if (mp0 < 0 || mp0 > 100 || mp1 < 0 || mp1 > 100) {
            ERROR_exit( "** Error: p0 and p1 must be >=0 and <= 100" ); 
         }
        
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-perclist") == 0 ) {
         /* initialize N_mp and mpv from user list */
         perc_flag = 1;
         ++nopt;

         /* init N_mp */
         if (nopt + 1 >= argc)
            ERROR_exit( "Need at least 2 params after -perclist\n");

         N_mp = atoi(argv[nopt]);
         if( N_mp <= 0 )
            ERROR_exit("need NUM_PERC (and percs) after -perclist\n"
                       "  (have NUM_PERC = '%s')\n", argv[nopt]);
         ++nopt;

         /* allocate mpv */
         mpv = (double *)malloc(sizeof(double)*N_mp);
         if (!mpv)
            ERROR_exit("Failed to allocate %d doubles for mpv", N_mp);
         if (nopt + N_mp >= argc)
            ERROR_exit("Need %d percentiles for -perclist\n", N_mp);

         /* and populate mpv */
         for(i=0; i<N_mp; i++) {
            /* use mp0 just for a local var */
            mp0 = atof(argv[nopt])/100.0f;
            if (mp0 < 0 || mp0 > 100 
                || (mp0 == 0 && ! isdigit(argv[nopt][0])) ) {
               ERROR_message("** Error: bad -perclist perc #%d of %d: %s\n"
                             "   percentiles should be in the range [0,100]\n",
                             i+1, N_mp, argv[nopt]);
               exit(1);
            }
        
            mpv[i] = mp0;
            ++nopt;
         }

         continue;
      }

      if( strcmp(argv[nopt],"-perc_quiet") == 0 ){
         perc_quiet = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-median") == 0 ){
         perc_flag = 1;
         mp0 = 0.50f; 
         mps = 0.01f; 
         mp1 = 0.50f;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-slow") == 0 ){
         slow_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-min") == 0 ){
         min_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-max") == 0 ){
         max_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-sum") == 0 ){
         sum_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-mean") == 0 ){
         mean_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-var") == 0 ){
         if (var_flag) {
            ERROR_message("Looks like -stdev is already used.\n"
                          "-var and -stdev are mutually exclusive");
            exit (1);
         }
         var_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-stdev") == 0 ){
         if (var_flag) {
            ERROR_message("Looks like -var is already used.\n"
                          "-var and -stdev are mutually exclusive");
            exit (1);
         }
         var_flag = 2;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-count") == 0 ){
         count_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-volume") == 0 ){
         vol_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-positive") == 0 ){
         if(positive_flag!=-1) {
            ERROR_exit( "Can not use multiple +/-/0 options");
          
         }
         positive_flag = 1;
         negative_flag = 0;
         zero_flag = 0;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-negative") == 0 ){
         if(positive_flag!=-1) {
            ERROR_exit( "Can not use multiple +/-/0 options");
         }
         positive_flag = 0;
         negative_flag = 1;
         zero_flag = 0;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-zero") == 0 ){
         if(positive_flag!=-1) {
            ERROR_exit( "Can not use multiple +/-/0 options");
         }
         positive_flag = 0;
         negative_flag = 0;
         zero_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-non-positive") == 0 ){
         if(positive_flag!=-1) {
            ERROR_exit( "Can not use multiple +/-/0 options");
         }
         positive_flag = 0;
         negative_flag = 1;
         zero_flag = 1;
         nopt++; continue;
      }
      if( strcmp(argv[nopt],"-non-negative") == 0 ){
         if(positive_flag!=-1) {
            ERROR_exit( "Can not use multiple +/-/0 options");
         }
         positive_flag = 1;
         negative_flag = 0;
         zero_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-non-zero") == 0 ){
         if(positive_flag!=-1) {
            ERROR_exit( "Can not use multiple +/-/0 options");
         }
         positive_flag = 1;
         negative_flag = 1;
         zero_flag = 0;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-absolute") == 0 ){
         absolute_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-nan") == 0 ){
         if(nan_flag!=-1) {
            ERROR_exit( "Can not use both -nan -nonan options");
         }
         nan_flag = 1;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-nonan") == 0 ){
         if(nan_flag!=-1) {
            ERROR_exit( "Can not use both -nan -nonan options");
          
         }
         nan_flag = 0;
         nopt++; continue;
      }

      if( strcmp(argv[nopt],"-autoclip") == 0 ||
          strcmp(argv[nopt],"-automask") == 0   ){

         if( mmm != NULL ){
            ERROR_exit(" ERROR: can't use -autoclip/mask with -mask!");
           
         }
         automask = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-mrange") == 0 ){
         if (nopt+2 >= argc) {
            ERROR_exit(" ERROR: Need two values after -mrange");
         }
         mmin = atof(argv[++nopt]);
         mmax = atof(argv[++nopt]);
         if (mmax < mmin) {
            ERROR_exit(
                       "1st value in -mrange %s %s should be the smallest one",
                       argv[nopt-1], argv[nopt]);
         } 
         nopt++ ; continue ;
      }
      
      if( strcmp(argv[nopt],"-mvalue") == 0 ){
         if (nopt+1 >= argc) {
            ERROR_exit(" ERROR: Need 1 value after -mvalue");
         }
         mmin = atof(argv[++nopt]);
         mmax = mmin ;
         nopt++ ; continue ;
      }
      
      if( strcmp(argv[nopt],"-mask") == 0 ){
         if( mask_dset_name != NULL )
            ERROR_exit(" ERROR: can't have 2 -mask options!");         
         mask_dset_name = argv[++nopt];
         nopt++ ; continue ;
      }

      ERROR_message( " Error - unknown option %s", argv[nopt]);
      suggest_best_prog_option(argv[0], argv[nopt]);
      exit(1);
   }

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      ERROR_message("Too few options");
      usage_3dBrickStat(0);
      exit(1) ;
   }

   if (mask_dset_name) {
      int ninmask = 0;
      THD_3dim_dataset * mask_dset ;
      if( automask ){
         ERROR_exit(" ERROR: can't use -mask with -automask!");
      }
      mask_dset = THD_open_dataset(mask_dset_name) ;
      CHECK_OPEN_ERROR(mask_dset,mask_dset_name) ;
       
      mmm = THD_makemask( mask_dset , 0 , mmin, mmax ) ;
      mmvox = DSET_NVOX( mask_dset ) ;
      ninmask = THD_countmask (mmvox, mmm);
      if (!ninmask) {
         ERROR_exit(" No voxels in mask !");
      }  
      /* text output program, so avoid extras   26 Dec 2013 [rickr] */
      /* INFO_message("%d voxels in mask\n", ninmask); */
      DSET_delete(mask_dset) ; 
   }
         
   if(((mmm!=NULL) && (quick_flag))||(automask &&quick_flag)) {
      if(quick_flag==1)
         WARNING_message( "+++ Warning - can't have quick option with mask");
      quick_flag = 0;
      slow_flag = 1;
   }

   /* if max_flag is not set by user, check if other user options set */
   if(max_flag==-1) {                
      if(min_flag || mean_flag || count_flag || vol_flag || sum_flag
         || perc_flag || var_flag) 
         max_flag = 0;
      else
         max_flag = 1;                  /* otherwise check only for max */
   }

   if((var_flag==1)||(mean_flag==1)||(count_flag==1)||
      (vol_flag==1)||(absolute_flag==1) ||
      (positive_flag!=-1)||(nan_flag!=-1)||
      (sum_flag == 1)||(perc_flag == 1) || (var_flag==2))
      {
         /* mean flag or count_flag implies slow */
         slow_flag = 1;
      }
   
   /* check slow and quick options */
   if((slow_flag) && (quick_flag!=1))  /* if user asked for slow, do so */
      quick_flag = 0;
   else
      quick_flag = 1;

   if((max_flag==0) && (min_flag==0))   /* if the user only asked for mean */
      quick_flag = 0;                  /*  no need to do quick way */

   if((quick_flag) && 
      ((absolute_flag==1)||(positive_flag==1)||(negative_flag==1)||(zero_flag==1)))
      WARNING_message( " Warning - ignoring +/-/0/abs flags for quick computations");

   if(positive_flag==-1) {   /* if no +/-/0 options set, allow all voxels */
      positive_flag = 1;
      negative_flag = 1;
      zero_flag = 1;
   }

   /*----- read input dataset -----*/

   if( nopt >= argc ){
      ERROR_exit(" No input dataset!?"); 
   }

   old_dset = THD_open_dataset( argv[nopt] ) ;
   CHECK_OPEN_ERROR(old_dset,argv[nopt]) ;

   nxyz = DSET_NVOX(old_dset) ;
   if( mmm != NULL && mmvox != nxyz ){
      ERROR_exit(" Mask and input datasets not the same size!") ;
      
   }

   if(automask && mmm == NULL ){
      mmm = THD_automask( old_dset ) ;
      for(i=0;i<nxyz;i++) {
         if(mmm[i]!=0) ++mmvox;
      }
   }

   if(quick_flag)
      Print_Header_MinMax(min_flag, max_flag, old_dset);
 
   if(slow_flag!=1)
      exit(0);

   /* ZSS do some diddlyiddly sorting - DO not affect Daniel's
      function later on */
   if (perc_flag == 1) {
      DSET_mallocize (old_dset);
      DSET_load (old_dset);
      if (DSET_NVALS(old_dset) != 1) {
         ERROR_exit( "-percentile can only be used on one sub-brick only.\n"
                     "Use sub-brick selectors '[.]' to specify "
                     "sub-brick of interest.\n");
      }
      
      /* prep for input and output of percentiles */
      /* if N_mp > 0, it and mpv have already been initialized  15 Mar 2021 */
      /* (just allocate perc) */
      if( N_mp > 0 ) {
         perc = (double *)malloc(sizeof(double)*N_mp);
         if (!mpv || !perc) {
            ERROR_message("Failed to allocate mpv or perc");
            exit(1);
         }  
      } else {
         if (mp0 > mp1) {
            N_mp = 1; 
         } else {
            /* allocate one above ceiling to prevent truncation error
               (and crash), N_mp is recomputed anyway 16 Mar 2009 [rickr] */
            N_mp = (int)((double)(mp1-mp0)/(double)mps) + 2;
         } 
         mpv = (double *)malloc(sizeof(double)*N_mp);
         perc = (double *)malloc(sizeof(double)*N_mp);
         if (!mpv || !perc) {
            ERROR_message("Failed to allocate for mpv or perc");
            exit(1);
         }  
         N_mp = 0;
         mp = mp0;
         do {
            mpv[N_mp] = mp; ++N_mp; mp += mps;
         } while (mp <= mp1+.00000001);
      }

      // [PT: March 24, 2021] Squash a bug that affects mean/stdev
      // calcs when a non-full-FOV mask is used, by setting the arg
      // 'option' to be 1, not 0 in Percentate().  This way, the input
      // dset is not sorted/changed (while the mask wasn't).  Below,
      // non-percentile calcs should now be OK.
      tmp_vec = Percentate (DSET_ARRAY(old_dset, 0), mmm, nxyz,
                            DSET_BRICK_TYPE(old_dset, 0), mpv, N_mp,
                            1, perc,
                            zero_flag, positive_flag, negative_flag );

      if ( !tmp_vec ) {
         ERROR_message("Failed to compute percentiles.");
         exit(1);         
      }
      
      /* take care of brick factor */
      if (DSET_BRICK_FACTOR(old_dset,0)) {
         for (i=0; i<N_mp; ++i) {
            perc[i] = perc[i]*DSET_BRICK_FACTOR(old_dset,0);
         }
      }
      
      for (i=0; i<N_mp; ++i) {
         if( perc_quiet )
            fprintf(stdout,"%f   ", perc[i]); 
         else
            fprintf(stdout,"%.1f %f   ", mpv[i]*100.0f, perc[i]); 
      }

      free(mpv);     mpv     = NULL;
      free(perc);    perc    = NULL;
      free(tmp_vec); tmp_vec = NULL;
   }

   Max_func(min_flag, max_flag, mean_flag,count_flag,
            positive_flag, negative_flag, zero_flag, absolute_flag,
            nan_flag, sum_flag, var_flag, vol_flag,old_dset, mmm, mmvox);

   
   if(mmm!=NULL)
      free(mmm);
   
   exit(0);

   /* unused code time series method for extracting data */
#if 0
   EDIT_dset_items( old_dset ,
                    ADN_ntt    , DSET_NVALS(old_dset) ,
                    ADN_ttorg  , 0.0 ,
                    ADN_ttdel  , 1.0 ,
                    ADN_tunits , UNITS_SEC_TYPE ,
                    NULL ) ;
   nbriks = 1;

   /*------------- ready to compute new min, max -----------*/
   new_dset = MAKER_4D_to_typed_fbuc(
              old_dset ,             /* input dataset */
              "temp" ,               /* output prefix */
              datum ,                /* output datum  */
              0 ,                    /* ignore count  */
              0 ,                /* can't detrend in maker function  KRH 12/02*/
              nbriks ,               /* number of briks */
(generic_func *)Max_tsfunc ,         /* timeseries processor */
              NULL,                  /* data for tsfunc */
              NULL,                  /* mask */
              0                      /* Allow auto scaling of output */
              ) ;
   if(min_flag)
      printf("%-13.6g ", minvalue); 
   if(max_flag)
      printf("%-13.6g", maxvalue); 
   printf("\n");
   exit(0) ;
#endif
}

/*! Print the minimum and maximum values from the header */
static void
Print_Header_MinMax(Minflag, Maxflag, dset)
   int Minflag, Maxflag;
THD_3dim_dataset * dset;
{
   int ival, nval_per;
   float tf=0.0; 
   double scaledmin, scaledmax, internalmin;
   double internalmax, overallmin, overallmax;

   overallmin = 1E10;
   overallmax = -1E10;

   ENTRY("Print_Header_MinMax");
   /* print out stuff for each sub-brick */
   nval_per = dset->dblk->nvals ;
   for( ival=0 ; ival < nval_per ; ival++ ){
      tf = DSET_BRICK_FACTOR(dset,ival) ;
      if( ISVALID_STATISTIC(dset->stats) ){
         if( tf != 0.0 ){
            internalmin = dset->stats->bstat[ival].min/tf;
            internalmax = dset->stats->bstat[ival].max/tf;
         }
         scaledmin = dset->stats->bstat[ival].min;
         scaledmax = dset->stats->bstat[ival].max;
         if( tf != 0.0 ){
            if(internalmin < overallmin)
               overallmin = scaledmin;
            if(internalmax > overallmax)
               overallmax = scaledmax;
         }
         else {
            if(scaledmin < overallmin)
               overallmin = scaledmin;
            if(scaledmax > overallmax)
               overallmax = scaledmax;
         }
      } 
      else {
         WARNING_message("No valid statistics in header. \n"
                         "Use -slow option to generate a new one.") ;
         EXRETURN;
      }
   }

   if(Minflag)
      printf("%-13.6g ", overallmin);
   if(Maxflag)
      printf("%-13.6g", overallmax);
   if( tf != 0.0)
      printf(" [*%g]\n",tf) ;
   else
      printf("\n");

   EXRETURN;
}


/*! search whole dataset for minimum and maximum */
/* load all at one time */
static void Max_func(int Minflag, int Maxflag, int Meanflag, int Countflag,
                     int Posflag, int Negflag, int Zeroflag, int Absflag, 
                     int nan_flag, 
                     int Sumflag,
                     int Varflag, int Volflag, THD_3dim_dataset * dset, 
                     byte *mmm, int mmvox)
{
   double overallmin, overallmax, overallmean;
   double voxval, fac, sum, sum2, vr;
   int nvox, npts;
   int i,k;
   int test_flag;

   MRI_IMAGE *data_im = NULL;

   ENTRY("Max_func");

   /* maybe the mask came up empty    [11 Jun 2019 rickr] 
      and check whether a mask was used at all [PT: Sep 1, 2022]
    */
   if( (mmm && mmvox > 0) || !mmm ) { 
      overallmin = 1E10;
      overallmax = -1E10;
   } else {
      overallmin = 0;
      overallmax = 0;
   }
   sum = 0.0;
   vr = 0.0;
   sum2 = 0.0;
   npts = 0;
   DSET_mallocize (dset);
   DSET_load (dset);                    /* load dataset */
   npts = 0;                            /* keep track of number of points */
   for(i=0;i<dset->dblk->nvals; i++) {  /* for each sub-brik in dataset */
      data_im = DSET_BRICK (dset, i);   // set pointer to 0th sub-brik of dset 
      fac = DSET_BRICK_FACTOR(dset, i); /* get scale factor for each sub-brik*/
      if(fac==0.0) fac=1.0;
      if( mmm != NULL)                  /* masked out */
         nvox = mmvox;
      else
         nvox = data_im->nvox;           /* number of voxels in the sub-brik */

      for(k=0;k<nvox;k++) {
         if( mmm != NULL && mmm[k] == 0 ) continue ;  /* masked out */

         switch( data_im->kind ){
         case MRI_short:{
            short *ar = mri_data_pointer(data_im) ;
            voxval = ar[k];
         }
            break ;
   
         case MRI_byte:{
            byte *ar = mri_data_pointer(data_im) ;
            voxval = ar[k];
         }
            break ;
   
         case MRI_float:{
            float *ar = mri_data_pointer(data_im) ;
            voxval = ar[k];
         }
            break ;
   
         case MRI_double:{
            double *ar = mri_data_pointer(data_im) ;
            voxval = ar[k];
         }
            break ;
   
         case MRI_int:{
            int *ar = mri_data_pointer(data_im) ;
            voxval = ar[k];
         }
            break ;
   
         case MRI_complex:{
            complex *ar = mri_data_pointer(data_im) ;
            voxval = CABS(ar[k]);
         }
            break ;
   
         case MRI_rgb:{
            byte *ar = mri_data_pointer(data_im) ;
            voxval = 0.299*ar[3*k]+0.587*ar[3*k+1]+0.114*ar[3*k+2];
         }
            break ;
   
         default:                          /* unknown type */
            voxval = 0.0;                   /* ignore this voxel */
            k = nvox;                       /* skip to next sub-brik */
            WARNING_message("Unknown type, %s, in sub-brik %d", 
                            MRI_TYPE_name[data_im->kind], i);
            break;
         }

         if( mmm == NULL || ((mmm!=NULL) && mmm[k] != 0 )){ // masked in voxel?
            voxval = voxval * fac;             /* apply scale factor */
            if(nan_flag!=-1) {               // check for various not a numbers
               test_flag = isfinite(voxval);
               if((nan_flag==1) && (test_flag==1)) /* only looking for NaNs*/
                  continue;
               if((nan_flag==0) && (test_flag==0)) // only looking for finites
                  continue;
               if(test_flag==0) { /* not a number (and nan_flag==1, so count)*/
                  ++npts;
                  continue;
               }
            }
            /* use only absolute values */
            if(Absflag) voxval = abs(voxval);
            /* limit data by sign */
            if(((voxval<0)&&Negflag)||((voxval==0)&&Zeroflag)
               ||((voxval>0)&&Posflag)) {
               sum += voxval;
               if (Varflag) sum2 += voxval*voxval;
               ++npts;            
               if(voxval<overallmin)
                  overallmin = voxval;
               if(voxval>overallmax)
                  overallmax = voxval;
            }
         }
      }
   }
   if(Minflag)
      printf("%-13.6g ", overallmin);
   if(Maxflag)
      printf("%-13.6g ", overallmax);
   if(Meanflag)
      {
         /* 11 Jun 2019 */
         if( npts > 0 ) overallmean = sum/npts;
         else           overallmean = 0.0;
         printf("%-13.6g ", overallmean);
      }
   if(Countflag)
      printf("%-13d", npts);

   if(Volflag)
      printf("%-13.6f", DSET_VOXVOL(dset) * npts);


   if (Sumflag) 
      printf("%-13.6g ", sum);
   if (Varflag) {
      /* 11 Jun 2019 */
      if( npts > 1 ) vr = (sum2-sum*sum/(double)npts)/(double)(npts-1);
      else           vr = 0.0;

      if (Varflag == 2) printf("%-13.6g ", sqrt(vr)); 
      else  printf("%-13.6g ", vr);   
   }
   printf("\n");

   mri_free (data_im);
   /*    DSET_unload_one (dset, 0);*/
   EXRETURN;
}

/* unused code time series method for extracting data */
#if 0

/*! search whole dataset for minimum and maximum */
static void Max_tsfunc( double tzero, double tdelta ,
                        int npts, float ts[],
                        double ts_mean, double ts_slope,
                        void * ud, int nbriks, float * val          )
{
   static int nvox, ncall;
   int i;

   ENTRY("Max_tsfunc"); 
   /* ts is input vector data */

   /** is this a "notification"? **/
   if( val == NULL ){

      if( npts > 0 ){  /* the "start notification" */

         nvox  = npts ;                       /* keep track of   */
         ncall = 0 ;                          /* number of calls */

      } else {  /* the "end notification" */

         /* nothing to do here */
      }
      return ;
   }
   ncall++;
   for(i=0;i<npts;i++) {
      if(ts[i]>maxvalue)
         maxvalue = ts[i];
      if(ts[i]<minvalue)
         minvalue = ts[i];
   }

   EXRETURN;
}
#endif
