#include "mrilib.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

void osfilt3_func( int num , float * vec ) ;
void median3_func( int num , float * vec ) ;
void linear3_func( int num , float * vec ) ;

int main( int argc , char * argv[] )
{
   void (*smth)(int,float *) = linear3_func ;
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

   /* start of code */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: 3dTsmooth [options] dataset\n"
             "Smooths each voxel time series in a 3D+time dataset and produces as\n"
             "output a new 3D+time dataset.\n"
             "\n"
             "General Options:\n"
             "  -prefix ppp  = Sets the prefix of the output dataset to be 'ppp'.\n"
             "                   [default = 'smooth']\n"
             "  -datum type  = Coerce the output data to be stored as the given type.\n"
             "                   [default = input data type]\n"
             "\n"
             "The following options define the smoothing filter to be used.\n"
             "All filters in this program use 3 input points to compute one\n"
             "output point: a = input value before the current point\n"
             "              b = input value at the current point\n"
             "              c = input value after the current point\n"
             "\n"
             "  -lin = 3 point linear filter: 0.15*a + 0.70*b + 0.15*c\n"
             "           [This is the default smoother]\n"
             "  -med = 3 point median filter: median(a,b,c)\n"
             "  -osf = 3 point order statistics filter:\n"
             "           0.15*min(a,b,c) + 0.70*median(a,b,c) + 0.15*max(a,b,c)\n"
           ) ;
      exit(0) ;
   }

   /* parse options */

   nopt = 1 ;

   while( nopt < argc && argv[nopt][0] == '-' ){

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

   ntime = DSET_NUM_TIMES(old_dset) ;
   nxyz  = DSET_NVOX(old_dset) ;

   if( ntime < 4 ){
      fprintf(stderr,"*** Can't smooth dataset with less than 4 time points!\n") ;
      exit(1) ;
   }

   DSET_load(old_dset) ;
   if( !DSET_LOADED(old_dset) ){
      fprintf(stderr,"*** Can't load datsaet from disk!\n") ; exit(1) ;
   }

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

      smth( ntime , fxar ) ;  /* call actual smoother */

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

   DSET_unload(old_dset) ;
   DSET_write(new_dset) ;
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

void linear3_func( int num , float * vec )
{
   int ii ;
   float aa,bb,cc ;

   bb = vec[0] ; cc = vec[1] ; vec[0] = OSFSUM(bb,bb,cc) ;
   for( ii=1 ; ii < num-1 ; ii++ ){
      aa = bb ; bb = cc ; cc = vec[ii+1] ;
      vec[ii] = OSFSUM(aa,bb,cc) ;
   }
   vec[num-1] = OSFSUM(bb,cc,cc) ;

   return ;
}
