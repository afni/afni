/*********************** 3dMax.c **********************************************/
/* Author: Daniel Glen, 26 Apr 2005 */
#include "mrilib.h"
#include "thd_shear3d.h"

static int datum                   = MRI_float ;
static void Print_Header_MinMax(int Minflag, int Maxflag, THD_3dim_dataset * dset);
static void Max_func(int Minflag, int Maxflag, THD_3dim_dataset * dset);
static void Max_tsfunc( double tzero , double tdelta ,
                         int npts , float ts[] , double ts_mean ,
                         double ts_slope , void * ud , int nbriks, float * val ) ;
static float minvalue=1E10, maxvalue=-1E10;
/*! compute the overall minimum and maximum voxel values for a dataset */
int main( int argc , char * argv[] )
{
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   int nopt, nbriks;
   int slow_flag, quick_flag, min_flag, max_flag;

   /*----- Read command line -----*/
   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dMax [options] dataset\n"
             "Compute maximum and/or the minimum voxeel values of an input dataset\n"
             "\n"
             "The output is a number to the console.  The input dataset\n"
             "may use a sub-brick selection list, as in program 3dcalc.\n"
             "Options :\n"
             "  -quick = get the information from the header only (default)\n"
             "  -slow = read the whole dataset to find the min and max values\n"
             "  -min = print the minimum value in dataset\n"
             "  -max = print the minimum value in dataset (default)\n"
             "  -help = print this help screen\n"
           ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      exit(0) ;
   }

   mainENTRY("3dMax main"); machdep(); AFNI_logger("3dMax",argc,argv);

   nopt = 1 ;

   min_flag  = 0;
   max_flag = -1;
   slow_flag = 0;
   quick_flag = -1;

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

      /*-- datum --*/

      if( strcmp(argv[nopt],"-datum") == 0 ){
         if( ++nopt >= argc ){
            fprintf(stderr,"*** -datum needs an argument!\n"); exit(1);
         }
         if( strcmp(argv[nopt],"short") == 0 ){
            datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            datum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            datum = MRI_byte ;
         } else {
            fprintf(stderr,"-datum of type '%s' is not supported!\n",
                    argv[nopt] ) ;
            exit(1) ;
         }
         nopt++ ; continue ;
      }

      fprintf(stderr, "*** Error - unknown option %s\n", argv[nopt]);
      exit(1);
   }



   if((slow_flag)&&(quick_flag!=1))
      quick_flag = 0;
   if((min_flag)&&(max_flag!=1))
      max_flag = 0;

   /*----- read input dataset -----*/

   if( nopt >= argc ){
      fprintf(stderr,"*** No input dataset!?\n"); exit(1);
   }

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( !ISVALID_DSET(old_dset) ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]); exit(1);
   }

   if(quick_flag)
      Print_Header_MinMax(min_flag, max_flag, old_dset);
 
   if(slow_flag!=1)
      exit(0);

   Max_func(min_flag, max_flag, old_dset);
   exit(0);

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
static void Max_func(Minflag, Maxflag, dset)
int Minflag, Maxflag;
THD_3dim_dataset * dset;
{
   double overallmin, overallmax;
   double voxval;
   int nvox;
   int i,k;

   MRI_IMAGE *data_im = NULL;

   ENTRY("Max_func");

   overallmin = 1E10;
   overallmax = -1E10;
   DSET_mallocize (dset);
   DSET_load (dset);	                /* load dataset */
   for(i=0;i<dset->dblk->nvals; i++) {  /* for each sub-brik in dataset */
      data_im = DSET_BRICK (dset, i);	/* set pointer to the 0th sub-brik of the dataset */
      nvox = data_im->nvox;
      for(k=0;k<nvox;k++) {
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
               default:
               fprintf(stderr,"+++ Warning - Don't know how to calculate max for this sub-brik image - %d!\n", i) ;
	       break;
            }

            if(voxval<overallmin)
	       overallmin = voxval;
            if(voxval>overallmax)
                overallmax = voxval;
      }
   }
   if(Minflag)
      printf("%-13.6g ", overallmin);
   if(Maxflag)
      printf("%-13.6g\n", overallmax);

    mri_free (data_im);
    /*    DSET_unload_one (dset, 0);*/
    EXRETURN;
}



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

