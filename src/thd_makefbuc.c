/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "thd_maker.h"

/*-------------------------------------------------------------------------
   Routine to create a 3D 'fbuc' dataset from a 3D+time dataset,
   using a user supplied function to process each time series.
   "user_func" should like

   void user_func( double tzero , double tdelta ,
                   int npts , float ts[] , double ts_mean , double ts_slope ,
                   void * ud , int nbrik , float * val )

   where tzero  =  time at ts[0]
         tdelta =  time at ts[1] (i.e., ts[k] is at tzero + k*tdelta);
                     tzero and tdelta will be in sec if this is truly "time"
         npts   =  number of points in ts array
         ts     =  one voxel time series array, ts[0] .. ts[npts-1];
                     note that this will always be a float array, and
                     that ts will start with the "ignore"-th point of
                     the actual voxel time series.
        ts_mean =  mean value of ts array
       ts_slope =  slope of ts array;
                     this will be inversely proportional to tdelta
                     (units of 1/sec);
                     if "detrend" is nonzero, then the mean and slope
                     will been removed from the ts array
         ud     =  the user_data pointer passed in here -- this can
                     contain whatever control information the user wants
         nbrik  =  number of output values that this function should return
                     for the voxel corresponding to input data 'ts'
         val    =  pointer to return values for this voxel;
                     note that this is a float array of length nbrik,
                     and that values that you don't fill in will be
                     set to zero (don't overrun this array!).

  Before the first timeseries is passed, user_func will be called with
  arguments
     ( 0.0 , 0.0 , nvox , NULL , 0.0 , 0.0 , user_data , nbrik , NULL )
  where nvox = number of voxels that will be processed.
  This is to allow for some setup (e.g., malloc, PLUTO_popup_meter, ...).

  After the last timeseries is passed, user_func will be called again with
  arguments
     ( 0.0 , 0.0 , 0 , NULL , 0.0 , 0.0 , user_data , nbrik , NULL )
  This is to allow for cleanup (e.g., free of malloc, ...).  Note that the
  only difference between these "notification" calls is the third argument.

  The inputs to the present routine are
    old_dset   = pointer to old dataset;
                   note that this dataset must not be warp-on-demand
    new_prefix = string to use as filename prefix
    new_datum  = type of data to store in output brick;
                   if negative, will use datum from old_dset
    ignore     = number of data points to ignore at the start
    detrend    = if nonzero, this routine will detrend (a+b*t)
                   each time series before passing it to user_func
    nbrik      = number of values (and sub-bricks) to create at each
                   voxel location
    user_func  = discussed above
    user_data  = discussed above

  The output is a pointer to a new dataset.  If NULL is returned,
  some error occurred.

  Note that sub-brick labels, statistical data, etc., must be added to
  the new dataset (using EDIT_dset_items) after this routine returns.
---------------------------------------------------------------------------*/

/*------------------ macros to return workspace at exit -------------------*/

#undef  FREE_FOUT
#define FREE_FOUT                                            \
  do{ int jv ;                                               \
      if( fout != NULL ){                                    \
         for( jv=0 ; jv < nbrik ; jv++ ) FREEUP(fout[jv]) ;  \
         free(fout) ;                                        \
      } } while(0)

#undef  FREE_WORKSPACE
#define FREE_WORKSPACE                              \
  do{ FREEUP(bptr) ; FREEUP(sptr) ; FREEUP(fptr) ;  \
      FREEUP(cptr) ; FREEUP(fxar) ; FREEUP(fac)  ;  \
      FREEUP(dtr)  ; FREEUP(val)  ; FREE_FOUT    ;  \
    } while(0)

/*-------------------------------------------------------------------------*/

THD_3dim_dataset * MAKER_4D_to_typed_fbuc( THD_3dim_dataset * old_dset ,
                                           char * new_prefix , int new_datum ,
                                           int ignore , int detrend ,
                                           int nbrik , generic_func * user_func ,
                                           void * user_data , byte *mmm)
{
   THD_3dim_dataset * new_dset ;  /* output dataset */

   byte    ** bptr = NULL ;  /* one of these will be the array of */
   short   ** sptr = NULL ;  /* pointers to input dataset sub-bricks */
   float   ** fptr = NULL ;  /* (depending on input datum type) */
   complex ** cptr = NULL ;

   float *  fxar = NULL ;  /* array loaded from input dataset */
   float *  fac  = NULL ;  /* array of input brick scaling factors */
   float ** fout = NULL ;  /* will be arrays of output floats */
   float *  dtr  = NULL ;  /* will be array of detrending coeff */
   float *  val  = NULL ;  /* will be array of output values */

   float d0fac , d1fac , x0,x1;
   double tzero=0 , tdelta , ts_mean , ts_slope ;
   int   ii , old_datum , nuse , use_fac , iz,izold, nxy,nvox , iv ;
   register int kk ;
   int nbad=0 ;        /* 08 Aug 2000 */

   void (*ufunc)(double,double,int,float *,double,double,void *,int,float *)
     = (void (*)(double,double,int,float *,double,double,void *,int,float *)) user_func;

   /*----------------------------------------------------------*/
   /*----- Check inputs to see if they are reasonable-ish -----*/

   if( ! ISVALID_3DIM_DATASET(old_dset) ) return NULL ;

   if( new_datum >= 0         &&
       new_datum != MRI_byte  &&
       new_datum != MRI_short &&
       new_datum != MRI_float   ) return NULL ;

   if( user_func == NULL ) return NULL ;

   if( nbrik <= 0 ) return NULL ;

   if( ignore < 0 ) ignore = 0 ;

   /*--------- set up pointers to each sub-brick in the input dataset ---------*/

   old_datum = DSET_BRICK_TYPE( old_dset , 0 ) ;   /* get old dataset datum */
   nuse      = DSET_NUM_TIMES(old_dset) - ignore ; /* # of points on time axis */
   if( nuse < 2 ) return NULL ;

   if( new_datum < 0 ) new_datum = old_datum ;   /* output datum = input */
   if( new_datum == MRI_complex ) return NULL ;  /* but complex = bad news */

   DSET_load( old_dset ) ;  /* must be in memory before we get pointers to it */

   kk = THD_count_databricks( old_dset->dblk ) ;  /* check if it was */
   if( kk < DSET_NVALS(old_dset) ){               /* loaded correctly */
      DSET_unload( old_dset ) ;
      return NULL ;
   }

   switch( old_datum ){  /* pointer type depends on input datum type */

      default:                      /** don't know what to do **/
         DSET_unload( old_dset ) ;
         return NULL ;

      /** create array of pointers into old dataset sub-bricks **/

      /*--------- input is bytes ----------*/
      /* voxel #i at time #k is bptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_byte:
         bptr = (byte **) malloc( sizeof(byte *) * nuse ) ;
         if( bptr == NULL ) return NULL ;
         for( kk=0 ; kk < nuse ; kk++ )
            bptr[kk] = (byte *) DSET_ARRAY(old_dset,kk+ignore) ;
      break ;

      /*--------- input is shorts ---------*/
      /* voxel #i at time #k is sptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_short:
         sptr = (short **) malloc( sizeof(short *) * nuse ) ;
         if( sptr == NULL ) return NULL ;
         for( kk=0 ; kk < nuse ; kk++ )
            sptr[kk] = (short *) DSET_ARRAY(old_dset,kk+ignore) ;
      break ;

      /*--------- input is floats ---------*/
      /* voxel #i at time #k is fptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_float:
         fptr = (float **) malloc( sizeof(float *) * nuse ) ;
         if( fptr == NULL ) return NULL ;
         for( kk=0 ; kk < nuse ; kk++ )
            fptr[kk] = (float *) DSET_ARRAY(old_dset,kk+ignore) ;
      break ;

      /*--------- input is complex ---------*/
      /* voxel #i at time #k is cptr[k][i]  */
      /* for i=0..nvox-1 and k=0..nuse-1.   */

      case MRI_complex:
         cptr = (complex **) malloc( sizeof(complex *) * nuse ) ;
         if( cptr == NULL ) return NULL ;
         for( kk=0 ; kk < nuse ; kk++ )
            cptr[kk] = (complex *) DSET_ARRAY(old_dset,kk+ignore) ;
      break ;

   } /* end of switch on input type */

   /*---- allocate space for 1 voxel timeseries ----*/

   fxar = (float *) malloc( sizeof(float) * nuse ) ;   /* voxel timeseries */
   if( fxar == NULL ){ FREE_WORKSPACE ; return NULL ; }

   /*--- get scaling factors for input sub-bricks ---*/

   fac = (float *) malloc( sizeof(float) * nuse ) ;   /* factors */
   if( fac == NULL ){ FREE_WORKSPACE ; return NULL ; }

   use_fac = 0 ;
   for( kk=0 ; kk < nuse ; kk++ ){
      fac[kk] = DSET_BRICK_FACTOR(old_dset,kk+ignore) ;
      if( fac[kk] != 0.0 ) use_fac++ ;
      else                 fac[kk] = 1.0 ;
   }
   if( !use_fac ) FREEUP(fac) ;

   /*--- setup for detrending ---*/

   dtr = (float *) malloc( sizeof(float) * nuse ) ;
   if( dtr == NULL ){ FREE_WORKSPACE ; return NULL ; }

   d0fac = 1.0 / nuse ;
   d1fac = 12.0 / nuse / (nuse*nuse - 1.0) ;
   for( kk=0 ; kk < nuse ; kk++ )
      dtr[kk] = kk - 0.5 * (nuse-1) ;  /* linear trend, orthogonal to 1 */

   /*---------------------- make a new dataset ----------------------*/

   new_dset = EDIT_empty_copy( old_dset ) ; /* start with copy of old one */

   /*-- edit some of its internal parameters --*/

   ii = EDIT_dset_items(
           new_dset ,
              ADN_prefix      , new_prefix ,           /* filename prefix */
              ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
              ADN_datum_all   , new_datum ,            /* atomic datum */
              ADN_nvals       , nbrik ,                /* # sub-bricks */
              ADN_ntt         , 0 ,                    /* # time points */
              ADN_type        , ISHEAD(old_dset)       /* dataset type */
                                 ? HEAD_FUNC_TYPE
                                 : GEN_FUNC_TYPE ,
              ADN_func_type   , FUNC_BUCK_TYPE ,        /* function type */
           ADN_none ) ;

   if( ii != 0 ){
      ERROR_message("Error creating dataset '%s'",new_prefix) ;
      THD_delete_3dim_dataset( new_dset , False ) ;  /* some error above */
      FREE_WORKSPACE ; return NULL ;
   }

   THD_init_datablock_labels( new_dset->dblk ) ;
   THD_init_datablock_keywords( new_dset->dblk ) ;
   THD_init_datablock_stataux( new_dset->dblk ) ;

   /*------ make floating point output bricks
            (only at the end will scale to byte or shorts) ------*/

   nvox = old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz ;

   fout = (float **) malloc( sizeof(float *) * nbrik ) ;

   if( fout == NULL ){
      THD_delete_3dim_dataset( new_dset , False ) ;
      FREE_WORKSPACE ; return NULL ;
   }

   for( iv=0 ; iv < nbrik ; iv++ ) fout[iv] = NULL ;

   for( iv=0 ; iv < nbrik ; iv++ ){
      fout[iv] = (float *) malloc( sizeof(float) * nvox ) ;
      if( fout[iv] == NULL ){
         THD_delete_3dim_dataset( new_dset , False ) ;
         FREE_WORKSPACE ; return NULL ;
      }
   }

   /*-- floating point storage for output from 1 voxel --*/

   val = (float *) malloc( sizeof(float) * nbrik ) ;
   if( val == NULL ){
      THD_delete_3dim_dataset( new_dset , False ) ;
      FREE_WORKSPACE ; return NULL ;
   }

   /*----- set up to find time at each voxel -----*/

   tdelta = old_dset->taxis->ttdel ;
   if( DSET_TIMEUNITS(old_dset) == UNITS_MSEC_TYPE ) tdelta *= 0.001 ;
   if( tdelta == 0.0 ) tdelta = 1.0 ;

   izold  = -666 ;
   nxy    = old_dset->daxes->nxx * old_dset->daxes->nyy ;

   /*----------------------------------------------------*/
   /*----- Setup has ended.  Now do some real work. -----*/

   /* start notification */

#if 0
   user_func(  0.0 , 0.0 , nvox , NULL,0.0,0.0 , user_data , nbrik , NULL ) ;
#else
   ufunc(  0.0 , 0.0 , nvox , NULL,0.0,0.0 , user_data , nbrik , NULL ) ;
#endif

   /***** loop over voxels *****/

   for( ii=0 ; ii < nvox ; ii++  ){  /* 1 time series at a time */

      /*** load data from input dataset, depending on type ***/

      switch( old_datum ){

         /*** input = bytes ***/

         case MRI_byte:
            for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] = bptr[kk][ii] ;
         break ;

         /*** input = shorts ***/

         case MRI_short:
            for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] = sptr[kk][ii] ;
         break ;

         /*** input = floats ***/

         case MRI_float:
            for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] = fptr[kk][ii] ;
         break ;

         /*** input = complex (note we use absolute value) ***/

         case MRI_complex:
            for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] = CABS(cptr[kk][ii]) ;
         break ;

      } /* end of switch over input type */

      /*** scale? ***/

      if( use_fac )
         for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] *= fac[kk] ;

      /** compute mean and slope **/

      x0 = x1 = 0.0 ;
      for( kk=0 ; kk < nuse ; kk++ ){
         x0 += fxar[kk] ; x1 += fxar[kk] * dtr[kk] ;
      }

      x0 *= d0fac ; x1 *= d1fac ;  /* factors to remove mean and trend */

      ts_mean  = x0 ;
      ts_slope = x1 / tdelta ;

      /** detrend? **/

      if( detrend )
         for( kk=0 ; kk < nuse ; kk++ ) fxar[kk] -= (x0 + x1 * dtr[kk]) ;

      /** compute start time of this timeseries **/

      iz = ii / nxy ;    /* which slice am I in? */

      if( iz != izold ){          /* in a new slice? */
         tzero = THD_timeof( ignore ,
                             old_dset->daxes->zzorg
                           + iz*old_dset->daxes->zzdel , old_dset->taxis ) ;
         izold = iz ;

         if( DSET_TIMEUNITS(old_dset) == UNITS_MSEC_TYPE ) tzero *= 0.001 ;
      }

      /*** compute output ***/

      for( iv=0 ; iv < nbrik ; iv++ ) val[iv] = 0.0 ;

      if (!mmm || mmm[ii]) { /* not the most efficient place to
                          mask, but it is the least obtrusive */
#if 0
      user_func( tzero,tdelta, nuse,fxar,ts_mean,ts_slope, user_data, nbrik,val );
#else
      ufunc( tzero,tdelta, nuse,fxar,ts_mean,ts_slope, user_data, nbrik,val );
#endif
      }
      
      for( iv=0 ; iv < nbrik ; iv++ ) fout[iv][ii] = val[iv] ;

   } /* end of outer loop over 1 voxels at a time */

   DSET_unload( old_dset ) ;  /* don't need this no more */

   /* end notification */

#if 0
   user_func( 0.0 , 0.0 , 0 , NULL,0.0,0.0 , user_data , nbrik , NULL ) ;
#else
   ufunc( 0.0 , 0.0 , 0 , NULL,0.0,0.0 , user_data , nbrik , NULL ) ;
#endif

   /*---- Count and correct float errors ----*/

   for( iv=0 ; iv < nbrik ; iv++ )
      nbad += thd_floatscan( nvox, fout[iv] ) ;

   if( nbad > 0 )
      fprintf(stderr,
              "++ Warning: %d bad floats computed in MAKER_4D_to_typed_fbuc\n\a",
              nbad ) ;

   /*------------------------------------------------------------------------*/
   /*------- The output is now in fout[iv][ii], iv=0..nbrik-1, ii=0..nvox-1.
             We must now put this into the output dataset. ------------------*/

   switch( new_datum ){

      /*** output is floats is the simplest:
           we just have to attach the fout brick to the dataset ***/

      case MRI_float:
         for( iv=0 ; iv < nbrik ; iv++ ){
            EDIT_substitute_brick( new_dset , iv , MRI_float , fout[iv] ) ;
            fout[iv] = NULL ;  /* so it won't be freed later */
         }
      break ;

      /*** output is shorts:
           we have to create scaled sub-bricks from fout ***/

      case MRI_short:{
         short * bout ;
         float sfac ;

         for( iv=0 ; iv < nbrik ; iv++ ){
            bout = (short *) malloc( sizeof(short) * nvox ) ;
            if( bout == NULL ){
               fprintf(stderr,
                "\nFinal malloc error in MAKER_4D_to_fbuc - is memory exhausted?\n\a");
               EXIT(1) ;
            }
            sfac = MCW_vol_amax( nvox,1,1 , MRI_float , fout[iv] ) ;
            if( sfac > 0.0 ){
               sfac = 32767.0 / sfac ;
               EDIT_coerce_scale_type( nvox,sfac ,
                                       MRI_float,fout[iv] , MRI_short,bout ) ;
               sfac = 1.0 / sfac ;
            }
            val[iv] = sfac ;
            EDIT_substitute_brick( new_dset , iv , MRI_short , bout ) ;
         }
         EDIT_dset_items( new_dset , ADN_brick_fac , val , ADN_none ) ;
      }
      break ;

      /*** output is bytes (byte = unsigned char)
           we have to create a scaled sub-brick from fout ***/

      case MRI_byte:{
         byte * bout ;
         float sfac ;

         for( iv=0 ; iv < nbrik ; iv++ ){
            bout = (byte *) malloc( sizeof(byte) * nvox ) ;
            if( bout == NULL ){
               fprintf(stderr,
                "\nFinal malloc error in MAKER_4D_to_fbuc - is memory exhausted?\n\a");
               EXIT(1) ;
            }
            sfac = MCW_vol_amax( nvox,1,1 , MRI_float , fout[iv] ) ;
            if( sfac > 0.0 ){
               sfac = 255.0 / sfac ;
               EDIT_coerce_scale_type( nvox,sfac ,
                                       MRI_float,fout[iv] , MRI_byte,bout ) ;
               sfac = 1.0 / sfac ;
            }
            val[iv] = sfac ;
            EDIT_substitute_brick( new_dset , iv , MRI_byte , bout ) ;
         }
         EDIT_dset_items( new_dset , ADN_brick_fac , val , ADN_none ) ;
      }
      break ;

   } /* end of switch on output data type */

   /*-------------- Cleanup and go home ----------------*/

   FREE_WORKSPACE ;
   return new_dset ;
}
