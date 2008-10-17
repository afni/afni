/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/

#include "mrilib.h"

void osfilt3_func( int num , float * vec ) ;
void median3_func( int num , float * vec ) ;
void linear3_func( int num , float * vec ) ;

/*-- 01 & 03 Mar 2001: linear filtering functions --*/

void linear_filter_extend( int , float * , int , float * ) ;
void linear_filter_zero  ( int , float * , int , float * ) ;
void linear_filter_trend ( int , float * , int , float * ) ;
float * hamming_window ( int ) ;
float * blackman_window( int ) ;
float * custom_filter( int ) ;
char custom_file[256] = "\0"; /*KRH 01/03, needed for custom filter */
int custom_ntaps = 0;

static float af=0.15 , bf=0.70 , cf=0.15 ;

int main( int argc , char * argv[] )
{
   void (*smth)(int,float *) = linear3_func ;     /* default filter */

   char prefix[256] = "smooth" ;
   int new_datum = ILLEGAL_TYPE , old_datum ;
   int nopt ;
   THD_3dim_dataset * old_dset , * new_dset ;
   int ii,kk , nxyz , ntime , use_fac , ityp , nbytes ;
   void * new_brick ;

   byte  ** bptr = NULL ;  /* one of these will be the array of */
   short ** sptr = NULL ;  /* pointers to input dataset sub-bricks */
   float ** fptr = NULL ;  /* (depending on input datum type) */

   byte  ** new_bptr = NULL ;  /* one of these will be the array of */
   short ** new_sptr = NULL ;  /* pointers to output dataset sub-bricks */
   float ** new_fptr = NULL ;  /* (depending on output datum type) */

   float * fxar = NULL ;  /* array loaded from input dataset */
   float * fac  = NULL ;  /* array of brick scaling factors */
   float * faci = NULL ;

#define BLACKMAN 1
#define HAMMING  2
#define CUSTOM   3

#define EXTEND   77
#define ZERO     78
#define TREND    79

   int ntap=0 ;      /* 01 Mar 2001 */
   float *ftap=NULL ;
   int nwin=0,nfil=EXTEND ;      /* 03 Mar 2001 */

   void (*lfil)(int,float *,int,float *) = linear_filter_extend ;
   float * (*lwin)(int) = NULL ;

   /* start of code */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTsmooth [options] dataset\n"
             "Smooths each voxel time series in a 3D+time dataset and produces\n"
             "as output a new 3D+time dataset (e.g., lowpass filter in time).\n"
             "\n"
             "General Options:\n"
             "  -prefix ppp  = Sets the prefix of the output dataset to be 'ppp'.\n"
             "                   [default = 'smooth']\n"
             "  -datum type  = Coerce output dataset to be stored as the given type.\n"
             "                   [default = input data type]\n"
             "\n"
             "Three Point Filtering Options [07 July 1999]\n"
             "--------------------------------------------\n"
             "The following options define the smoothing filter to be used.\n"
             "All these filters  use 3 input points to compute one output point:\n"
             "  Let a = input value before the current point\n"
             "      b = input value at the current point\n"
             "      c = input value after the current point\n"
             "           [at the left end, a=b; at the right end, c=b]\n"
             "\n"
             "  -lin = 3 point linear filter: 0.15*a + 0.70*b + 0.15*c\n"
             "           [This is the default smoother]\n"
             "  -med = 3 point median filter: median(a,b,c)\n"
             "  -osf = 3 point order statistics filter:\n"
             "           0.15*min(a,b,c) + 0.70*median(a,b,c) + 0.15*max(a,b,c)\n"
             "\n"
             "  -3lin m = 3 point linear filter: 0.5*(1-m)*a + m*b + 0.5*(1-m)*c\n"
             "              Here, 'm' is a number strictly between 0 and 1.\n"
             "\n"
             "General Linear Filtering Options [03 Mar 2001]\n"
             "----------------------------------------------\n"
             "  -hamming N  = Use N point Hamming or Blackman windows.\n"
             "  -blackman N     (N must be odd and bigger than 1.)\n"
             "  -custom coeff_filename.1D (odd # of coefficients must be in a \n"
	     "                             single column in ASCII file)\n"
	     "   (-custom added Jan 2003)\n"
             "    WARNING: If you use long filters, you do NOT want to include the\n"
             "             large early images in the program.  Do something like\n"
             "                3dTsmooth -hamming 13 'fred+orig[4..$]'\n"
             "             to eliminate the first 4 images (say).\n"
             " The following options determing how the general filters treat\n"
             " time points before the beginning and after the end:\n"
             "  -EXTEND = BEFORE: use the first value; AFTER: use the last value\n"
             "  -ZERO   = BEFORE and AFTER: use zero\n"
             "  -TREND  = compute a linear trend, and extrapolate BEFORE and AFTER\n"
             " The default is -EXTEND.  These options do NOT affect the operation\n"
             " of the 3 point filters described above, which always use -EXTEND.\n"
           ) ;
      printf("\n" MASTER_SHORTHELP_STRING ) ;
      PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("3dTsmooth main"); machdep(); AFNI_logger("3dTsmooth",argc,argv);
   PRINT_VERSION("3dTsmooth") ;

   /* parse options */

   nopt = 1 ;

   while( nopt < argc && argv[nopt][0] == '-' ){

      if( strcmp(argv[nopt],"-EXTEND") == 0 ){         /* 03 Mar 2001 */
         nfil = EXTEND ; lfil = linear_filter_extend ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-ZERO") == 0 ){           /* 03 Mar 2001 */
         nfil = ZERO ; lfil = linear_filter_zero ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-TREND") == 0 ){          /* 03 Mar 2001 */
         nfil = TREND ; lfil = linear_filter_trend ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-hamming") == 0 ){
         if( ++nopt >= argc ){fprintf(stderr,"*** Illegal -hamming!\n");exit(1);}
         ntap = (int) strtod(argv[nopt],NULL) ;
         if( ntap < 3 || ntap%2 != 1 ){fprintf(stderr,"*** Illegal -hamming!\n");exit(1);}
         nwin = HAMMING ; lwin = hamming_window ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-blackman") == 0 ){
         if( ++nopt >= argc ){fprintf(stderr,"*** Illegal -blackman!\n");exit(1);}
         ntap = (int) strtod(argv[nopt],NULL) ;
         if( ntap < 3 || ntap%2 != 1 ){fprintf(stderr,"*** Illegal -blackman!\n");exit(1);}
         nwin = BLACKMAN ; lwin = blackman_window ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-custom") == 0 ){
         if( ++nopt >= argc ){fprintf(stderr,"*** Illegal -custom!\n");exit(1);}
         strcpy(custom_file, argv[nopt]) ;
         nwin = CUSTOM ; lwin = custom_filter ;
	 ntap = 1;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-prefix") == 0 ){
         if( ++nopt >= argc ){fprintf(stderr,"*** Illegal -prefix!\n");exit(1);}
         strcpy(prefix,argv[nopt]) ;
         if( !THD_filename_ok(prefix) ){fprintf(stderr,"*** Illegal -prefix!\n");exit(1);}
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-datum") == 0 ){
         if( ++nopt >= argc ){fprintf(stderr,"*** Illegal -datum!\n");exit(1);}
         if( strcmp(argv[nopt],"short") == 0 ){
            new_datum = MRI_short ;
         } else if( strcmp(argv[nopt],"float") == 0 ){
            new_datum = MRI_float ;
         } else if( strcmp(argv[nopt],"byte") == 0 ){
            new_datum = MRI_byte ;
         } else {
            fprintf(stderr,"*** Illegal -datum!\n");exit(1);
         }
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-lin") == 0 ){
         bf = 0.70 ; af = cf = 0.15 ;
         smth = linear3_func ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-med") == 0 ){
         smth = median3_func ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-osf") == 0 ){
         smth = osfilt3_func ;
         nopt++ ; continue ;
      }

      if( strcmp(argv[nopt],"-3lin") == 0 ){
         if( ++nopt >= argc ){fprintf(stderr,"*** Illegal -3lin!\n");exit(1);}
         bf = strtod( argv[nopt] , NULL ) ;
         if( bf <= 0.0 || bf >= 1.0 ){fprintf(stderr,"*** Illegal -3lin!\n");exit(1);}
         af = cf = 0.5*(1.0-bf) ;
         smth = linear3_func ;
         nopt++ ; continue ;
      }

      fprintf(stderr,"*** Unknown option: %s\n",argv[nopt]) ; exit(1) ;

   }  /* end of loop over options */

   if( nopt >= argc ){
      fprintf(stderr,"*** No input dataset?!\n") ; exit(1) ;
   }

   /* open dataset */

   old_dset = THD_open_dataset( argv[nopt] ) ;
   if( old_dset == NULL ){
      fprintf(stderr,"*** Can't open dataset %s\n",argv[nopt]) ; exit(1) ;
   }

   ntime = DSET_NVALS(old_dset) ;
   nxyz  = DSET_NVOX(old_dset) ;

   if( ntime < 4 ){
      fprintf(stderr,"*** Can't smooth dataset with less than 4 time points!\n") ;
      exit(1) ;
   }

   DSET_load(old_dset) ; CHECK_LOAD_ERROR(old_dset) ;

   old_datum = DSET_BRICK_TYPE(old_dset,0) ;
   if( new_datum < 0 ) new_datum = old_datum ;

   switch( old_datum ){  /* pointer type depends on input datum type */

      /** create array of pointers into old dataset sub-bricks **/

      /*--------- input is bytes ----------*/
      /* voxel #i at time #k is bptr[k][i] */
      /* for i=0..nxyz-1 and k=0..ntime-1. */

      case MRI_byte:
         bptr = (byte **) malloc( sizeof(byte *) * ntime ) ;
         for( kk=0 ; kk < ntime ; kk++ )
            bptr[kk] = (byte *) DSET_ARRAY(old_dset,kk) ;
      break ;

      /*--------- input is shorts ---------*/
      /* voxel #i at time #k is sptr[k][i] */
      /* for i=0..nxyz-1 and k=0..ntime-1. */

      case MRI_short:
         sptr = (short **) malloc( sizeof(short *) * ntime ) ;
         for( kk=0 ; kk < ntime ; kk++ )
            sptr[kk] = (short *) DSET_ARRAY(old_dset,kk) ;
      break ;

      /*--------- input is floats ---------*/
      /* voxel #i at time #k is fptr[k][i] */
      /* for i=0..nxyz-1 and k=0..ntime-1. */

      case MRI_float:
         fptr = (float **) malloc( sizeof(float *) * ntime ) ;
         for( kk=0 ; kk < ntime ; kk++ )
            fptr[kk] = (float *) DSET_ARRAY(old_dset,kk) ;
      break ;

   } /* end of switch on input type */

   /*---- allocate space for 1 voxel timeseries ----*/

   fxar = (float *) malloc( sizeof(float) * ntime ) ;   /* voxel timeseries */

   /*--- get scaling factors for sub-bricks ---*/

   fac = (float *) malloc( sizeof(float) * ntime ) ;   /* factors */

   use_fac = 0 ;
   for( kk=0 ; kk < ntime ; kk++ ){
      fac[kk] = DSET_BRICK_FACTOR(old_dset,kk) ;
      if( fac[kk] != 0.0 ) use_fac++ ;
      else                 fac[kk] = 1.0 ;
   }
   if( !use_fac ){
      free(fac) ; fac == NULL ;
   } else {
      faci = (float *) malloc( sizeof(float) * ntime ) ;
      for( kk=0 ; kk < ntime ; kk++ ) faci[kk] = 1.0 / fac[kk] ;
   }

   /*---------------------- make a new dataset ----------------------*/

   new_dset = EDIT_empty_copy( old_dset ) ;

   tross_Copy_History( old_dset , new_dset ) ;
   tross_Make_History( "3dTsmooth" , argc,argv , new_dset ) ;

   /*-- edit some of its internal parameters --*/

   EDIT_dset_items( new_dset ,
                      ADN_prefix      , prefix ,
                      ADN_malloc_type , DATABLOCK_MEM_MALLOC ,
                      ADN_datum_all   , new_datum ,
                    ADN_none ) ;

   /*-- make brick(s) for this dataset --*/

   switch( new_datum ){
      case MRI_byte:
         new_bptr = (byte **) malloc( sizeof(byte *) * ntime ) ;
      break ;

      case MRI_short:
         new_sptr = (short **) malloc( sizeof(short *) * ntime ) ;
      break ;

      case MRI_float:
         new_fptr = (float **) malloc( sizeof(float *) * ntime ) ;
      break ;
   }

   for( kk=0 ; kk < ntime ; kk++ ){
      ityp      = DSET_BRICK_TYPE(new_dset,kk) ;   /* type of data */
      nbytes    = DSET_BRICK_BYTES(new_dset,kk) ;  /* how much data */
      new_brick = malloc( nbytes ) ;               /* make room */

      if( new_brick == NULL ){
        fprintf(stderr,"*** Can't get memory for output dataset!\n") ; exit(1) ;
      }

      EDIT_substitute_brick( new_dset , kk , ityp , new_brick ) ;

      switch( new_datum ){
         case MRI_byte:  new_bptr[kk] = (byte * ) new_brick ; break ;
         case MRI_short: new_sptr[kk] = (short *) new_brick ; break ;
         case MRI_float: new_fptr[kk] = (float *) new_brick ; break ;
      }
   }

   if( lwin != NULL && ntap > 0 ){        /* 03 Mar 2001 */
      ftap = lwin(ntap) ;
      if( lfil == NULL ) lfil = linear_filter_extend ;
      if( nwin == CUSTOM ) ntap = custom_ntaps ;
   }

   /*----------------------------------------------------*/
   /*----- Setup has ended.  Now do some real work. -----*/

   /***** loop over voxels *****/

   for( ii=0 ; ii < nxyz ; ii++  ){  /* 1 time series at a time */

      /*** load data from input dataset, depending on type ***/

      switch( old_datum ){
         case MRI_byte:
            for( kk=0 ; kk < ntime ; kk++ ) fxar[kk] = bptr[kk][ii] ;
         break ;

         case MRI_short:
            for( kk=0 ; kk < ntime ; kk++ ) fxar[kk] = sptr[kk][ii] ;
         break ;

         case MRI_float:
            for( kk=0 ; kk < ntime ; kk++ ) fxar[kk] = fptr[kk][ii] ;
         break ;
      } /* end of switch over input type */

      if( use_fac )
         for( kk=0 ; kk < ntime ; kk++ ) fxar[kk] *= fac[kk] ;

      /* do smoothing */

      if( ftap != NULL )
         lfil( ntap,ftap , ntime,fxar ) ;  /* 01 Mar 2001 */
      else
         smth( ntime , fxar ) ;            /* 3 point smoother */

      /*** put data into output dataset ***/

      switch( new_datum ){

         case MRI_byte:
            if( use_fac )
               for( kk=0 ; kk < ntime ; kk++ ) new_bptr[kk][ii] = (byte)(fxar[kk] * faci[kk]) ;
            else
               for( kk=0 ; kk < ntime ; kk++ ) new_bptr[kk][ii] = (byte) fxar[kk] ;
         break ;

         case MRI_short:
            if( use_fac )
               for( kk=0 ; kk < ntime ; kk++ ) new_sptr[kk][ii] = (short)(fxar[kk] * faci[kk]) ;
            else
               for( kk=0 ; kk < ntime ; kk++ ) new_sptr[kk][ii] = (short) fxar[kk] ;
         break ;

         case MRI_float:
            if( use_fac )
               for( kk=0 ; kk < ntime ; kk++ ) new_fptr[kk][ii] = (float)(fxar[kk] * faci[kk]) ;
            else
               for( kk=0 ; kk < ntime ; kk++ ) new_fptr[kk][ii] = (float) fxar[kk] ;
         break ;
      }
   }  /* end of loop over voxels */

   DSET_unload(old_dset) ; free(ftap) ;
   DSET_write(new_dset) ;
   fprintf(stderr,"++ output dataset: %s\n",DSET_BRIKNAME(new_dset)) ;
   exit(0) ;
}

/*--------------- Order Statistics Filter ----------------*/

void osfilt3_func( int num , float * vec )
{
   int ii ;
   float aa,bb,cc ;

   bb = vec[0] ; cc = vec[1] ; vec[0] = OSFILT(bb,bb,cc) ;
   for( ii=1 ; ii < num-1 ; ii++ ){
      aa = bb ; bb = cc ; cc = vec[ii+1] ;
      vec[ii] = OSFILT(aa,bb,cc) ;         /* see mrilib.h */
   }
   vec[num-1] = OSFILT(bb,cc,cc) ;

   return ;
}

/*--------------- Median of 3 Filter ----------------*/

void median3_func( int num , float * vec )
{
   int ii ;
   float aa,bb,cc ;

   bb = vec[0] ; cc = vec[1] ;
   for( ii=1 ; ii < num-1 ; ii++ ){
      aa = bb ; bb = cc ; cc = vec[ii+1] ;
      vec[ii] = MEDIAN(aa,bb,cc) ;         /* see mrilib.h */
   }

   return ;
}

/*--------------- Linear Filter ----------------*/

#define LSUM(a,b,c) af*(a)+bf*(b)+cf*(c)

void linear3_func( int num , float * vec )
{
   int ii ;
   float aa,bb,cc ;

   bb = vec[0] ; cc = vec[1] ; vec[0] = LSUM(bb,bb,cc) ;
   for( ii=1 ; ii < num-1 ; ii++ ){
      aa = bb ; bb = cc ; cc = vec[ii+1] ;
      vec[ii] = LSUM(aa,bb,cc) ;
   }
   vec[num-1] = LSUM(bb,cc,cc) ;

   return ;
}

/*-------------------------------------------------------------------------*/

void linear_filter_extend( int ntap , float *wt , int npt , float *x )
{
   int ii , nt2=(ntap-1)/2 , jj ;
   float sum ;
   static int nfar=0 ;
   static float *far=NULL ;

   if( npt > nfar ){
      if(far != NULL) free(far) ;
      far = (float *)malloc(sizeof(float)*npt) ; nfar = npt ;
   }

#undef XX
#define XX(i) ( ((i)<0) ? far[0] : ((i)>npt-1) ? far[npt-1] : far[i] )

   memcpy( far , x , sizeof(float)*npt ) ;

   for( ii=0 ; ii < npt ; ii++ ){
      for( sum=0.0,jj=0 ; jj < ntap ; jj++ ) sum += wt[jj] * XX(ii-nt2+jj) ;
      x[ii] = sum ;
   }

   return ;
}

/*-------------------------------------------------------------------------*/

void linear_filter_zero( int ntap , float *wt , int npt , float *x )
{
   int ii , nt2=(ntap-1)/2 , jj ;
   float sum ;
   static int nfar=0 ;
   static float *far=NULL ;

   if( npt > nfar ){
      if(far != NULL) free(far) ;
      far = (float *)malloc(sizeof(float)*npt) ; nfar = npt ;
   }

#undef XX
#define XX(i) ( ((i)<0 || (i)>npt-1) ? 0 : far[i] )

   memcpy( far , x , sizeof(float)*npt ) ;

   for( ii=0 ; ii < npt ; ii++ ){
      for( sum=0.0,jj=0 ; jj < ntap ; jj++ ) sum += wt[jj] * XX(ii-nt2+jj) ;
      x[ii] = sum ;
   }

   return ;
}

/*-------------------------------------------------------------------------*/

void linear_filter_trend( int ntap , float *wt , int npt , float *x )
{
   int ii , nt2=(ntap-1)/2 , jj ;
   float sum ;
   static int nfar=0 ;
   static float *far=NULL ;
   float a=0.0,b=0.0 ;

   if( npt > nfar ){
      if(far != NULL) free(far) ;
      far = (float *)malloc(sizeof(float)*npt) ; nfar = npt ;
   }

#undef XX
#define XX(i) ( ((i)<0 || (i)>npt-1) ? (a+b*(i)) : far[i] )

   memcpy( far , x , sizeof(float)*npt ) ;

   get_linear_trend( npt,far , &a,&b ) ; /* cf. thd_detrend.c */

   for( ii=0 ; ii < npt ; ii++ ){
      for( sum=0.0,jj=0 ; jj < ntap ; jj++ ) sum += wt[jj] * XX(ii-nt2+jj) ;
      x[ii] = sum ;
   }

   return ;
}

/*-------------------------------------------------------------------------*/

float * custom_filter( int dummy )
{
   MRI_IMAGE * filter_data=NULL;
   float * filter_coefficients = NULL;

   filter_data = mri_read_1D(custom_file);
   if MRI_IS_1D(filter_data) {
     custom_ntaps = filter_data->nx;
     if (custom_ntaps % 2) {
       filter_coefficients = MRI_FLOAT_PTR(filter_data);
     } else {
       /*ERROR, EVEN NUMBER OF FILTER TAPS */
     }
   }

   return filter_coefficients ;
}

/*-------------------------------------------------------------------------*/

float * hamming_window( int ntap )
{
   float * wt , tau , t , sum ;
   int ii , nt2=(ntap-1)/2 ;

   if( ntap < 3 ) return NULL ;

   wt = (float *) calloc(sizeof(float),ntap) ;
   tau = nt2 + 1.0 ;

   for( sum=0.0,ii=0 ; ii <= 2*nt2 ; ii++ ){
      t = PI*(ii-nt2)/tau ;
      wt[ii] = 0.54 + 0.46*cos(t) ;
      sum += wt[ii] ;
   }
   sum = 1.0 / sum ;
   for( ii=0 ; ii < ntap ; ii++ ) wt[ii] *= sum ;

   return wt ;
}

/*-------------------------------------------------------------------------*/

float * blackman_window( int ntap )
{
   float * wt , tau , t , sum ;
   int ii , nt2=(ntap-1)/2 ;

   if( ntap < 3 ) return NULL ;

   wt = (float *) calloc(sizeof(float),ntap) ;
   tau = nt2 + 1.0 ;

   for( sum=0.0,ii=0 ; ii <= 2*nt2 ; ii++ ){
      t = PI*(ii-nt2)/tau ;
      wt[ii] = 0.42 + 0.5*cos(t) + 0.08*cos(2*t) ;
      sum += wt[ii] ;
   }
   sum = 1.0 / sum ;
   for( ii=0 ; ii < ntap ; ii++ ) wt[ii] *= sum ;

   return wt ;
}
