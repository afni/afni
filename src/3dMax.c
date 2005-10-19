/*********************** 3dMax.c **********************************************/
/* Author: Daniel Glen, 26 Apr 2005 */
#include "mrilib.h"
#include "thd_shear3d.h"

static int datum                   = MRI_float ;
static void Print_Header_MinMax(int Minflag, int Maxflag, THD_3dim_dataset * dset);
static void Max_func(int Minflag, int Maxflag, int Meanflag, int Countflag, int Posflag,\
    int Negflag, int Zeroflag, int nan_flag, THD_3dim_dataset * dset, byte *mmm, int mmvox);
static void Max_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void * ud , int nbriks, float * val ) ;
static float minvalue=1E10, maxvalue=-1E10;
/*! compute the overall minimum and maximum voxel values for a dataset */
int main( int argc , char * argv[] )
{
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   int nopt, nbriks;
   int slow_flag, quick_flag, min_flag, max_flag, mean_flag, automask,count_flag;
   int positive_flag, negative_flag, zero_flag, nan_flag;
   byte * mmm=NULL ;
   int    mmvox=0 ;
   int nxyz, i;
   MRI_IMAGE *anat_im = NULL;


   /*----- Read command line -----*/
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dMax [options] dataset\n"
             "Compute maximum and/or minimum voxel values of an input dataset\n"
             "\n"
             "The output is a number to the console.  The input dataset\n"
             "may use a sub-brick selection list, as in program 3dcalc.\n"
             "Options :\n"
             "  -quick = get the information from the header only (default)\n"
             "  -slow = read the whole dataset to find the min and max values\n"
             "  -min = print the minimum value in dataset\n"
             "  -max = print the minimum value in dataset (default)\n"
             "  -mean = print the mean value in dataset (implies slow)\n"
             "  -count = print the number of voxels included (implies slow)\n"
             "  -positive = include only positive voxel values (implies slow)\n"
             "  -negative = include only negative voxel values (implies slow)\n"
             "  -zero = include only zero voxel values (implies slow)\n"
             "  -non-positive = include only voxel values 0 or negative (implies slow)\n"
             "  -non-negative = include only voxel values 0 or greater (implies slow)\n"
             "  -non-zero = include only voxel values not equal to 0 (implies slow)\n"
             "  -nan = include only voxel values that are not numbers (NaN, inf, -if,\n       implies slow)\n"
             "  -nonan =exclude voxel values that are not numbers\n"
             "  -mask dset = use dset as mask to include/exclude voxels\n"
             "  -automask = automatically compute mask for dataset\n"
             "    Can not be combined with -mask\n"
             "  -help = print this help screen\n"
           ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      exit(0) ;
   }

   mainENTRY("3dMax main"); machdep(); AFNI_logger("3dMax",argc,argv);

   nopt = 1 ;

   min_flag  = 0;
   max_flag = -1;
   mean_flag = 0;
   slow_flag = 0;
   quick_flag = -1;
   automask = 0;
   count_flag = 0;
   positive_flag = -1;
   negative_flag = -1;
   zero_flag = -1;
   nan_flag = -1;

   datum = MRI_float;
   while( nopt < argc && argv[nopt][0] == '-' ){
      if( strcmp(argv[nopt],"-quick") == 0 ){
	quick_flag = 1;
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

      if( strcmp(argv[nopt],"-mean") == 0 ){
	mean_flag = 1;
        nopt++; continue;
      }

      if( strcmp(argv[nopt],"-count") == 0 ){
	count_flag = 1;
        nopt++; continue;
      }

      if( strcmp(argv[nopt],"-positive") == 0 ){
        if(positive_flag!=-1) {
          fprintf(stderr, "***Can not use multiple +/-/0 options");
          exit(1) ;
        }
        positive_flag = 1;
	negative_flag = 0;
        zero_flag = 0;
        nopt++; continue;
      }

      if( strcmp(argv[nopt],"-negative") == 0 ){
        if(positive_flag!=-1) {
          fprintf(stderr, "***Can not use multiple +/-/0 options");
          exit(1) ;
        }
        positive_flag = 0;
	negative_flag = 1;
        zero_flag = 0;
        nopt++; continue;
      }

      if( strcmp(argv[nopt],"-zero") == 0 ){
        if(positive_flag!=-1) {
          fprintf(stderr, "***Can not use multiple +/-/0 options");
          exit(1) ;
        }
        positive_flag = 0;
        negative_flag = 0;
	zero_flag = 1;
        nopt++; continue;
      }

      if( strcmp(argv[nopt],"-non-positive") == 0 ){
        if(positive_flag!=-1) {
          fprintf(stderr, "***Can not use multiple +/-/0 options");
          exit(1) ;
        }
        positive_flag = 0;
	negative_flag = 1;
        zero_flag = 1;
        nopt++; continue;
      }
      if( strcmp(argv[nopt],"-non-negative") == 0 ){
        if(positive_flag!=-1) {
          fprintf(stderr, "***Can not use multiple +/-/0 options");
          exit(1) ;
        }
        positive_flag = 1;
	negative_flag = 0;
        zero_flag = 1;
        nopt++; continue;
      }

      if( strcmp(argv[nopt],"-non-zero") == 0 ){
        if(positive_flag!=-1) {
          fprintf(stderr, "***Can not use multiple +/-/0 options");
          exit(1) ;
        }
        positive_flag = 1;
	negative_flag = 1;
        zero_flag = 0;
        nopt++; continue;
      }

      if( strcmp(argv[nopt],"-nan") == 0 ){
        if(nan_flag!=-1) {
          fprintf(stderr, "***Can not use both -nan -nonan options");
          exit(1) ;
        }
        nan_flag = 1;
        nopt++; continue;
      }

      if( strcmp(argv[nopt],"-nonan") == 0 ){
        if(nan_flag!=-1) {
          fprintf(stderr, "***Can not use both -nan -nonan options");
          exit(1) ;
        }
        nan_flag = 0;
        nopt++; continue;
      }

      if( strcmp(argv[nopt],"-autoclip") == 0 ||
          strcmp(argv[nopt],"-automask") == 0   ){

         if( mmm != NULL ){
           fprintf(stderr,"** ERROR: can't use -autoclip/mask with -mask!\n");
           exit(1) ;
         }
         automask = 1 ; nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-mask") == 0 ){
         THD_3dim_dataset * mask_dset ;
         if( automask ){
           fprintf(stderr,"** ERROR: can't use -mask with -automask!\n");
           exit(1) ;
         }
         mask_dset = THD_open_dataset(argv[++nopt]) ;
         if( mask_dset == NULL ){
            fprintf(stderr,"** ERROR: can't open -mask dataset!\n"); exit(1);
         }
         if( mmm != NULL ){
            fprintf(stderr,"** ERROR: can't have 2 -mask options!\n"); exit(1);
         }
         mmm = THD_makemask( mask_dset , 0 , 1.0,-1.0 ) ;
         mmvox = DSET_NVOX( mask_dset ) ;

         DSET_delete(mask_dset) ; nopt++ ; continue ;
      }

      fprintf(stderr, "*** Error - unknown option %s\n", argv[nopt]);
      exit(1);
   }

   if(((mmm!=NULL) && (quick_flag))||(automask &&quick_flag)) {
      if(quick_flag==1)
         fprintf(stderr, "+++ Warning - can't have quick option with mask\n");
      quick_flag = 0;
      slow_flag = 1;
   }

   if(max_flag==-1) {                   /* if max_flag is not set by user,*/
     if(min_flag || mean_flag ||count_flag)   /* check if other user options set */
         max_flag = 0;
      else
	max_flag = 1;                  /* otherwise check only for max */
     }

   if((mean_flag==1)||(count_flag==1)||(positive_flag!=-1)||(nan_flag!=-1))  /* mean flag or count_flag implies slow */
     slow_flag = 1;

   /* check slow and quick options */
   if((slow_flag)&&(quick_flag!=1))  /* if user asked for slow give it to him */
      quick_flag = 0;
   else
      quick_flag = 1;

   if((max_flag==0)&&(min_flag==0))   /* if the user only asked for mean */
     quick_flag = 0;                  /*  no need to do quick way */

   if((quick_flag) && ((positive_flag==1)||(negative_flag==1)||(zero_flag==1)))
     fprintf(stderr, "+++ Warning - ignoring +/-/0 flags for quick computations\n");

   if(positive_flag==-1) {   /* if no +/-/0 options set, allow all voxels */
     positive_flag = 1;
     negative_flag = 1;
     zero_flag = 1;
   }

   /*----- read input dataset -----*/

   if( nopt >= argc ){
      fprintf(stderr,"*** No input dataset!?\n"); exit(1);
   }

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(old_dset) ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]); exit(1);
   }

   nxyz = DSET_NVOX(old_dset) ;
   if( mmm != NULL && mmvox != nxyz ){
      fprintf(stderr,"** Mask and input datasets not the same size!\n") ;
      exit(1) ;
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

   Max_func(min_flag, max_flag, mean_flag,count_flag,positive_flag, negative_flag, zero_flag,\
     nan_flag, old_dset, mmm, mmvox);

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
                 0 ,              /* can't detrend in maker function  KRH 12/02*/
                 nbriks ,               /* number of briks */
		 Max_tsfunc ,         /* timeseries processor */
                 NULL                   /* data for tsfunc */
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
  float tf; 
  double scaledmin, scaledmax, internalmin, internalmax, overallmin, overallmax;

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
         printf( "No valid statistics in header\n") ;
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
static void Max_func(Minflag, Maxflag, Meanflag, Countflag, Posflag, Negflag, Zeroflag, nan_flag, dset, mmm, mmvox)
int Minflag, Maxflag;
THD_3dim_dataset * dset;
byte *mmm;  /* mask pointer */
int mmvox;
{
   double overallmin, overallmax, overallmean;
   double voxval, fac, sum;
   int nvox, npts;
   int i,k;
   int test_flag;

   MRI_IMAGE *data_im = NULL;

   ENTRY("Max_func");

   overallmin = 1E10;
   overallmax = -1E10;
   sum = 0.0;
   npts = 0;
   DSET_mallocize (dset);
   DSET_load (dset);	                /* load dataset */
   npts = 0;                            /* keep track of number of points */
   for(i=0;i<dset->dblk->nvals; i++) {  /* for each sub-brik in dataset */
      data_im = DSET_BRICK (dset, i);	/* set pointer to the 0th sub-brik of the dataset */
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
                 fprintf(stderr,"Unknown type, %s, in sub-brik %d\n", MRI_TYPE_name[data_im->kind], i);
	       break;
            }

             if( mmm == NULL || ((mmm!=NULL) && mmm[k] != 0 )){   /* masked in voxel? */
	      voxval = voxval * fac;             /* apply scale factor */
              if(nan_flag!=-1) {       /* check for various not a numbers */
                test_flag = finite(voxval);
                if((nan_flag==1) && (test_flag==1)) /* only looking for NaNs*/
		  continue;
                if((nan_flag==0) && (test_flag==0)) /* only looking for finites */
		  continue;
                if(test_flag==0) {  /* not a number */
		  ++npts;
                  continue;
                }
              }
              if(((voxval<0)&&Negflag)||((voxval==0)&&Zeroflag)||((voxval>0)&&Posflag)) {
	         sum += voxval;
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
       overallmean = sum/npts;
       printf("%-13.6g ", overallmean);
     }
   if(Countflag)
     printf("%-13d", npts);

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
