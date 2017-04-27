/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to compute power spectrum of a 3D+time dataset.
  This is a moderately complex example, showing how to deal
  with different data types on input and output.
************************************************************************/

/*------------- string to 'help' the user -------------*/

static char helpstring[] =
  " Purpose: Compute 'Power Spectrum' of a 3D+time dataset.\n"
  " Input items are:\n"
  "   Input = 3D+time dataset to analyze\n"
  "\n"
  "   Output: Prefix = Filename prefix for new dataset\n"
  "           Datum  = How to store results\n"
  "\n"
  "   Ignore Count   = How many points to ignore at start\n"
  "   Taper Percent  = Amount of data to taper (Hamming)\n"
  "   FFT Length     = Fourier transform size to use [N]\n"
  "                    (If N > size of data, data will be zero)\n"
  "                    (padded. 'shortest' means to use N just)\n"
  "                    (above the length of the time series.  )\n"
  "\n"
  " The output dataset will be stored in the 3D+time format, with\n"
  " the 'time' index actually being frequency.  The frequency grid\n"
  " spacing will be 1/(N*dt), where N=FFT length and dt = input\n"
  " dataset time spacing.\n"
  "\n"
  " The method used is the simplest known: squared periodogram.\n"
  " A single FFT is done (i.e., each point has DOF=2.)\n"
;

/*------------- strings for output format -------------*/

static char * type_strings[]
  = { "as Input" , "Byte" , "Short" , "Float" } ;

#define NUM_TYPE_STRINGS (sizeof(type_strings)/sizeof(char *))

/*------------- strings for FFT length -------------*/

static char * fft_strings[] =
#if 0
   { "shortest", "32", "64", "128", "256", "512", "1024", "2048", "4096" } ;
#else
   /*                    3*       15*      2**      5*    */
   { "shortest", "32" ,  "48" ,   "60" ,   "64" ,   "80" ,
                         "96" ,  "120" ,  "128" ,  "160" ,
                        "192" ,  "240" ,  "256" ,  "320" ,
                        "384" ,  "480" ,  "512" ,  "640" ,
                        "768" ,  "960" , "1024" , "1280" ,
                       "1536" , "1920" , "2048"            } ;
#endif

#define NUM_FFT_STRINGS (sizeof(fft_strings)/sizeof(char *))

/*--------------- prototypes for internal routines ---------------*/

char * POWER_main( PLUGIN_interface * ) ;  /* the entry point */

#undef ALLOW_TESTING
#ifdef ALLOW_TESTING
PLUGIN_interface * TEST_init(void) ;
char * TEST_main( PLUGIN_interface * ) ;  /* the entry point */
#endif

/***********************************************************************
   Set up the interface to the user:
    1) Create a new interface using "PLUTO_new_interface";

    2) For each line of inputs, create the line with "PLUTO_add_option"
         (this line of inputs can be optional or mandatory);

    3) For each item on the line, create the item with
        "PLUTO_add_dataset" for a dataset chooser,
        "PLUTO_add_string"  for a string chooser,
        "PLUTO_add_number"  for a number chooser.
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   if( ncall > 1 ) return NULL ;  /* two interfaces */

#ifdef ALLOW_TESTING
   if( ncall == 1 ) return TEST_init() ;
#else
   if( ncall == 1 ) return NULL ;
#endif

   /*---------------- set titles and call point ----------------*/

   CHECK_IF_ALLOWED("POWERSPECTRUM","Power Spectrum") ;  /* 30 Sep 2016 */

   plint = PLUTO_new_interface( "Power Spectrum" ,
                                "Power Spectrum of a 3D+time Dataset" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , POWER_main  ) ;

   PLUTO_add_hint( plint , "Power Spectrum of a 3D+time Dataset" ) ;

   PLUTO_set_sequence( plint , "A:newdset:statistics" ) ;

   /*--------- 1st line: Input dataset ---------*/

   PLUTO_add_option( plint ,
                     "Input" ,  /* label at left of input line */
                     "Input" ,  /* tag to return to plugin */
                     TRUE       /* is this mandatory? */
                   ) ;

   PLUTO_add_dataset(  plint ,
                       "---->>" ,         /* label next to button   */
                       ANAT_ALL_MASK ,    /* take any anat datasets */
                       FUNC_FIM_MASK ,    /* only allow fim funcs   */
                       DIMEN_4D_MASK |    /* need 3D+time datasets  */
                       BRICK_ALLREAL_MASK /* need real-valued datasets */
                    ) ;

   /*---------- 2nd line: Output dataset ----------*/

   PLUTO_add_option( plint ,
                     "Output" ,  /* label at left of input line */
                     "Output" ,  /* tag to return to plugin */
                     TRUE        /* is this mandatory? */
                   ) ;

   PLUTO_add_string(   plint ,
                       "Prefix" ,  /* label next to textfield */
                       0,NULL ,    /* no fixed strings to choose among */
                       19          /* 19 spaces for typing in value */
                   ) ;

   PLUTO_add_string(   plint ,
                       "Datum" ,          /* label next to chooser button */
                       NUM_TYPE_STRINGS , /* number of strings to choose among */
                       type_strings ,     /* list of strings to choose among */
                       0                  /* index of default string */
                   ) ;

   /*--------- Other lines: Parameters ---------*/

   PLUTO_add_option( plint , "Ignore" , "Ignore" , TRUE ) ;

   PLUTO_add_number( plint ,
                     "Count" ,   /* label next to chooser */
                     0 ,         /* smallest possible value */
                     999 ,       /* largest possible value */
                     0 ,         /* decimal shift (none in this case) */
                     4 ,         /* default value */
                     TRUE        /* allow user to edit value? */
                   ) ;

   PLUTO_add_option( plint , "Taper" , "Taper" , TRUE ) ;

   PLUTO_add_number( plint ,
                     "Percent" ,    /* label next to chooser */
                     0 ,            /* smallest possible value */
                     10 ,           /* largest possible value */
                     -1 ,           /* decimal shift (1 right == 0 to 100) */
                     0 ,            /* default value (with shift == 0) */
                     FALSE          /* allow user to edit value? */
                   ) ;

   PLUTO_add_option( plint , "FFT" , "FFT" , TRUE ) ;

   PLUTO_add_string( plint ,
                     "Length" ,         /* label next to chooser */
                     NUM_FFT_STRINGS ,  /* number of strings to choose among */
                     fft_strings ,      /* list of strings to choose among */
                     0                  /* index of default string */
                   ) ;

   /*--------- done with interface setup ---------*/

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

/*------------------ macros to return workspace at exit -------------------*/

#undef  FREEUP
#define FREEUP(x) do{ if((x) != NULL){free((x)); (x)=NULL;} } while(0)

#define FREE_WORKSPACE                              \
  do{ FREEUP(bptr) ; FREEUP(sptr) ; FREEUP(fptr) ;  \
      FREEUP(fout) ; FREEUP(cxar) ; FREEUP(tar)  ;  \
      FREEUP(fxar) ; FREEUP(fyar) ; FREEUP(dtr)  ;  \
    } while(0)

/*-------------------------------------------------------------------------*/

char * POWER_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;                          /* input dataset idcode */
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   char * new_prefix , * str ;                 /* strings from user */
   int   new_datum , ignore , nfft , ninp ,    /* control parameters */
         old_datum , nuse , ntaper , ktbot ;
   float taper ;

   byte   ** bptr  = NULL ;  /* one of these will be the array of */
   short  ** sptr  = NULL ;  /* pointers to input dataset sub-bricks */
   float  ** fptr  = NULL ;  /* (depending on input datum type) */

   complex * cxar  = NULL ;  /* will be array of data to FFT */
   float   * fxar  = NULL ;  /* array loaded from input dataset */
   float   * fyar  = NULL ;  /* array loaded from input dataset */
   float  ** fout  = NULL ;  /* will be array of output floats */

   float   * tar   = NULL ;  /* will be array of taper coefficients */
   float   * dtr   = NULL ;  /* will be array of detrending coeff */

   float dfreq , pfact , phi , xr,xi , yr,yi ;
   float x0,x1 , y0,y1 , d0fac,d1fac ;
   int   nfreq , nvox , perc , new_units ;
   int   istr , ii,iip , ibot,itop , kk , icx ;       /* temp variables */

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   /*--------- go to first input line ---------*/

   PLUTO_next_option(plint) ;

   idc      = PLUTO_get_idcode(plint) ;   /* get dataset item */
   old_dset = PLUTO_find_dset(idc) ;      /* get ptr to dataset */
   if( old_dset == NULL )
      return "*************************\n"
             "Cannot find Input Dataset\n"
             "*************************"  ;

   /*--------- go to second input line ---------*/

   PLUTO_next_option(plint) ;

   new_prefix = PLUTO_get_string(plint) ;   /* get string item (the output prefix) */
   if( ! PLUTO_prefix_ok(new_prefix) )      /* check if it is OK */
      return "************************\n"
             "Output Prefix is illegal\n"
             "************************"  ;

   str  = PLUTO_get_string(plint) ;              /* get string item (the datum type) */
   istr = PLUTO_string_index( str ,              /* find it in the list it came from */
                              NUM_TYPE_STRINGS ,
                              type_strings ) ;
   switch( istr ){
      default:
      case 0:
         new_datum = DSET_BRICK_TYPE( old_dset , 0 ) ;  /* use old dataset type */
      break ;

      case 1: new_datum = MRI_byte  ; break ;  /* assign type of user's choice */
      case 2: new_datum = MRI_short ; break ;
      case 3: new_datum = MRI_float ; break ;
   }

   /*--------- go to next input lines ---------*/

   PLUTO_next_option(plint) ;                 /* skip to next line */
   ignore = PLUTO_get_number(plint) ;         /* get number item (ignore) */

   PLUTO_next_option(plint) ;                 /* skip to next line */
   taper  = PLUTO_get_number(plint) * 0.01 ;  /* get number item (taper %) */

   /* compute FFT length to use */

   PLUTO_next_option(plint) ;          /* skip to next line */

   str  = PLUTO_get_string(plint) ;    /* get string item for FFT count */
   ninp = DSET_NUM_TIMES(old_dset) ;   /* number of values in input */
   nuse = ninp - ignore ;              /* number of values to actually use */

   if( nuse < 4 )
      return "*****************************\n"
             "Not enough time points to FFT\n"
             "*****************************"  ;

   if( strcmp(str,fft_strings[0]) == 0 ){

      /*-- get next larger power-of-2 --*/
#if 0
      for( nfft=32 ; nfft < nuse ; nfft *= 2 ) ; /* loop until nfft >= nuse */
#else
      nfft = csfft_nextup_even(nuse) ;
#endif

   } else {
      nfft = strtol( str , NULL , 10 ) ;  /* just convert string to integer */
   }

   /* if the input FFT length is less than the data length,
      tell the user and truncate the amount of data to use */

   if( nfft < nuse ){
      char str[256] ;

      sprintf( str , "******************************\n"
                     "Warning:\n"
                     " Number of points in FFT =%4d\n"
                     " is less than available data\n"
                     " in time series = %d\n"
                     "******************************" ,
               nfft , nuse ) ;

      PLUTO_popup_transient( plint , str ) ;

      nuse = nfft ;  /* can't use more data than the FFT length */
   }

   /* compute the number of output points and the output grid spacing */

   nfreq = nfft / 2 ;                                 /* # frequencies */
   dfreq = 1.0 / (nfft * DSET_TIMESTEP(old_dset) ) ;  /* frequency grid */

   switch( DSET_TIMEUNITS(old_dset) ){
      case UNITS_MSEC_TYPE: dfreq *= 1000.0 ; new_units = UNITS_HZ_TYPE ; break;
      case UNITS_SEC_TYPE:                    new_units = UNITS_HZ_TYPE ; break;
      case UNITS_HZ_TYPE:                     new_units = UNITS_SEC_TYPE; break;

      default: new_units = DSET_TIMEUNITS(old_dset) ; break ; /* shouldn't happen */
   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   PLUTO_popup_meter( plint ) ;  /* popup a progress meter */

   /*--------- set up pointers to each sub-brick in the input dataset ---------*/

   DSET_load( old_dset ) ;  /* must be in memory before we get pointers to it */

   old_datum = DSET_BRICK_TYPE( old_dset , 0 ) ; /* get old dataset datum type */

   switch( old_datum ){  /* pointer type depends on input datum type */

      default:
         return "******************************\n"
                "Illegal datum in Input Dataset\n"
                "******************************"  ;

      /** create array of pointers into old dataset sub-bricks **/
      /** Note that we skip the first 'ignore' sub-bricks here **/

      /*--------- input is bytes ----------*/
      /* voxel #i at time #k is bptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_byte:
         bptr = (byte **) malloc( sizeof(byte *) * nuse ) ;
         if( bptr == NULL ) return "Malloc\nFailure!\n [bptr]" ;
         for( kk=0 ; kk < nuse ; kk++ )
            bptr[kk] = (byte *) DSET_ARRAY(old_dset,kk+ignore) ;
      break ;

      /*--------- input is shorts ---------*/
      /* voxel #i at time #k is sptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_short:
         sptr = (short **) malloc( sizeof(short *) * nuse ) ;
         if( sptr == NULL ) return "Malloc\nFailure!\n [sptr]" ;
         for( kk=0 ; kk < nuse ; kk++ )
            sptr[kk] = (short *) DSET_ARRAY(old_dset,kk+ignore) ;
      break ;

      /*--------- input is floats ---------*/
      /* voxel #i at time #k is fptr[k][i] */
      /* for i=0..nvox-1 and k=0..nuse-1.  */

      case MRI_float:
         fptr = (float **) malloc( sizeof(float *) * nuse ) ;
         if( fptr == NULL ) return "Malloc\nFailure!\n [fptr]" ;
         for( kk=0 ; kk < nuse ; kk++ )
            fptr[kk] = (float *) DSET_ARRAY(old_dset,kk+ignore) ;
      break ;

   } /* end of switch on input type */

   /*---- allocate space for 2 voxel timeseries and 1 FFT ----*/

   cxar = (complex *) malloc( sizeof(complex) * nfft ) ; /* FFT */
   fxar = (float *)   malloc( sizeof(float) * nuse ) ;   /* input */
   fyar = (float *)   malloc( sizeof(float) * nuse ) ;   /* input */
   if( cxar == NULL || fxar == NULL || fyar == NULL ){
      FREE_WORKSPACE ;
      return "Malloc\nFailure!\n [cxar]" ;
   }

   /*--------- make space for taper coefficient array ---------*/

   tar = (float *) malloc( sizeof(float) * MAX(nuse,nfreq) ) ;
   dtr = (float *) malloc( sizeof(float) * nuse ) ;

   if( tar == NULL || dtr == NULL ){
      FREE_WORKSPACE ;
      return "Malloc\nFailure!\n [tar]" ;
   }

   ntaper = (int)(0.5 * taper * nuse + 0.49) ; /* will taper data over */
   phi    = PI / MAX(ntaper,1) ;               /* kk=0..ntaper-1 on left */
   ktbot  = nuse - ntaper ;                    /* kk=ktbot..nuse-1 on right */
   pfact  = 0.0 ;                              /* sum of taper**2 */

   for( kk=0 ; kk < nuse ; kk++ ){                       /* Hamming-ize */
      if( kk < ntaper )
         tar[kk] = 0.54 - 0.46 * cos(kk*phi) ;           /* ramp up */
      else if( kk >= ktbot )
         tar[kk] = 0.54 + 0.46 * cos((kk-ktbot+1)*phi) ; /* ramp down */
      else
         tar[kk] = 1.0 ;                                 /* in the middle */

      pfact  += tar[kk] * tar[kk] ;

      dtr[kk] = kk - 0.5 * (nuse-1) ;  /* factors for linear detrending */
   }

   d0fac = 1.0 / nuse ;
   d1fac = 12.0 / nuse / (nuse*nuse - 1.0) ;

   /*--- compute factor to go from |FFT|**2 to PSD;
         includes the scaling needed for loss of energy with tapering ---*/

   pfact = DSET_TIMESTEP(old_dset) / pfact ;

   /*--- include scaling factors for sub-bricks, if any ---*/

   for( kk=0 ; kk < nuse ; kk++ )
      if( DSET_BRICK_FACTOR(old_dset,kk+ignore) > 0.0 )
         tar[kk] *= DSET_BRICK_FACTOR(old_dset,kk+ignore) ;

   /*---------------------- make a new dataset ----------------------*/

   new_dset = EDIT_empty_copy( old_dset ) ; /* start with copy of old one */

   { char * his = PLUTO_commandstring(plint) ;
     tross_Copy_History( old_dset , new_dset ) ;
     tross_Append_History( new_dset , his ) ; free(his) ;
   }

   /*-- edit some of its internal parameters --*/

   ii = EDIT_dset_items(
           new_dset ,
              ADN_prefix      , new_prefix ,           /* filename prefix */
              ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
              ADN_datum_all   , new_datum ,            /* atomic datum */
              ADN_nvals       , nfreq ,                /* # sub-bricks */
              ADN_ntt         , nfreq ,                /* # time points */
              ADN_ttorg       , dfreq ,                /* time origin */
              ADN_ttdel       , dfreq ,                /* time step */
              ADN_ttdur       , dfreq ,                /* time duration */
              ADN_nsl         , 0 ,                    /* z-axis time slicing */
              ADN_tunits      , new_units ,            /* time units */
           ADN_none ) ;

   if( ii != 0 ){
      THD_delete_3dim_dataset( new_dset , False ) ;
      FREE_WORKSPACE ;
      return "***********************************\n"
             "Error while creating output dataset\n"
             "***********************************"  ;
   }

   /*------ make floating point output sub-bricks
            (only at the end will scale to byte or shorts)

            Output #ii at freq #kk will go into fout[kk][ii],
            for kk=0..nfreq-1, and for ii=0..nvox-1.          ------*/

   nvox = old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz ;

   fout = (float **) malloc( sizeof(float *) * nfreq ) ;  /* ptrs to sub-bricks */

   if( fout == NULL ){
      THD_delete_3dim_dataset( new_dset , False ) ;
      FREE_WORKSPACE ;
      return "Malloc\nFailure!\n [fout]" ;
   }

   for( kk=0 ; kk < nfreq ; kk++ ){
      fout[kk] = (float *) malloc( sizeof(float) * nvox ) ; /* sub-brick # kk */
      if( fout[kk] == NULL ) break ;
   }

   if( kk < nfreq ){
      for( ; kk >= 0 ; kk-- ) FREEUP(fout[kk]) ;   /* free all we did get */
      THD_delete_3dim_dataset( new_dset , False ) ;
      FREE_WORKSPACE ;
      return "Malloc\nFailure!\n [arrays]" ;
   }

   { char buf[128] ;
     ii = (nfreq * nvox * sizeof(float)) / (1024*1024) ;
     sprintf( buf , "  \n"
                    "*** 3D+time Power Spectral Density:\n"
                    "*** Using %d MBytes of workspace,\n "
                    "*** with FFT length = %d\n" , ii,nfft ) ;
     PLUTO_popup_transient( plint , buf ) ;
   }

   /*----------------------------------------------------*/
   /*----- Setup has ended.  Now do some real work. -----*/

   /***** loop over voxels *****/

   for( ii=0 ; ii < nvox ; ii += 2 ){  /* 2 time series at a time */

      iip = (ii+1) % nvox ;           /* voxel after ii */

      /*** load data from input dataset, depending on type ***/

      switch( old_datum ){

         /*** input = bytes ***/

         case MRI_byte:
            for( kk=0 ; kk < nuse ; kk++ ){
               fxar[kk] = bptr[kk][ii] ;
               fyar[kk] = bptr[kk][iip] ;
            }
         break ;

         /*** input = shorts ***/

         case MRI_short:
            for( kk=0 ; kk < nuse ; kk++ ){
               fxar[kk] = sptr[kk][ii] ;
               fyar[kk] = sptr[kk][iip] ;
            }
         break ;

         /*** input = floats ***/

         case MRI_float:
            for( kk=0 ; kk < nuse ; kk++ ){
               fxar[kk] = fptr[kk][ii] ;
               fyar[kk] = fptr[kk][iip] ;
            }
         break ;

      } /* end of switch over input type */

      /*** detrend:
             x0 = sum( fxar[kk] )
             x1 = sum( fxar[kk] * (kk-0.5*(N-1)) )
           x0 is used to remove the mean of fxar
           x1 is used to remove the linear trend of fxar ***/

      x0 = x1 = y0 = y1 = 0.0 ;
      for( kk=0 ; kk < nuse ; kk++ ){
         x0 += fxar[kk] ; x1 += fxar[kk] * dtr[kk] ;
         y0 += fyar[kk] ; y1 += fyar[kk] * dtr[kk] ;
      }

      x0 *= d0fac ; x1 *= d1fac ;  /* factors to remove mean and trend */
      y0 *= d0fac ; y1 *= d1fac ;

      for( kk=0 ; kk < nuse ; kk++ ){
         fxar[kk] -= (x0 + x1 * dtr[kk]) ;  /* remove mean and trend here! */
         fyar[kk] -= (y0 + y1 * dtr[kk]) ;
      }

      /*** taper, scale, and put into cxar array ***/

      for( kk=0 ; kk < nuse ; kk++ ){
         cxar[kk].r = fxar[kk] * tar[kk] ;
         cxar[kk].i = fyar[kk] * tar[kk] ;
      }

      /*** load zeros after where data was put ***/

      for( kk=nuse ; kk < nfft ; kk++ ) cxar[kk].r = cxar[kk].i = 0.0 ;

      /***** do the FFT (at long last) *****/

      csfft_cox( -1 , nfft , cxar ) ;

      /***** now compute output into corresponding voxels in fout *****/

      /*--- Let x = fxar (1st real time series)
                y = fyar (2nd real time series)
                z = cxar (complex time series) = x + i y
                N = nfft (length of FFT)

            Then after FFT, since x and y are real, we have
              zhat[k]  = xhat[k] + i yhat[k]  > for k=1..N/2
            zhat[N-k]* = xhat[k] - i yhat[k]

            so we can untangle the FFTs of x and y by
              xhat[k] = 0.5 ( zhat[k] + zhat[N-k]* )
              yhat[k] = 0.5 ( zhat[k] - zhat[N-k]* ) / i

            This is the basis for doing 2 time series at once. ---*/

      for( kk=1 ; kk <= nfreq ; kk++ ){
         xr = 0.5 * ( cxar[kk].r + cxar[nfft-kk].r ) ; /* Re xhat[kk] */
         xi = 0.5 * ( cxar[kk].i - cxar[nfft-kk].i ) ; /* Im xhat[kk] */
         yr = 0.5 * ( cxar[kk].i + cxar[nfft-kk].i ) ; /* Re yhat[kk] */
         yi = 0.5 * ( cxar[kk].r - cxar[nfft-kk].r ) ; /*-Im yhat[kk] */

         fout[kk-1][ii]  = pfact * (xr*xr + xi*xi) ;
         fout[kk-1][iip] = pfact * (yr*yr + yi*yi) ;
      }

      perc = (100 * ii) / nvox ;        /* display percentage done */
      PLUTO_set_meter( plint , perc ) ; /* on the progress meter */

   } /* end of outer loop over 2 voxels at a time */

   DSET_unload( old_dset ) ;  /* don't need this no more */

   /*------------------------------------------------------------*/
   /*------- The output is now in fout[kk][ii],
             for kk=0..nfreq-1 , ii=0..nvox-1.
             We must now put this into the output dataset -------*/

   switch( new_datum ){

      /*** output is floats is the simplest:
           we just have to attach the fout bricks to the dataset ***/

      case MRI_float:
         for( kk=0 ; kk < nfreq ; kk++ )
            EDIT_substitute_brick( new_dset , kk , MRI_float , fout[kk] ) ;
      break ;

      /*** output is shorts:
           we have to create a scaled sub-brick from fout ***/

      case MRI_short:{
         short * bout ;
         float fac ;

         for( kk=0 ; kk < nfreq ; kk++ ){  /* loop over sub-bricks */

            /*-- get output sub-brick --*/

            bout = (short *) malloc( sizeof(short) * nvox ) ;
            if( bout == NULL ){
               fprintf(stderr,"\nFinal malloc error in plug_power!\n\a") ;
               return("\nFinal malloc error in plug_power!\n") ;
               /* EXIT(1) ;*/
            }

            /*-- find scaling and then scale --*/

            fac = MCW_vol_amax( nvox,1,1 , MRI_float , fout[kk] ) ;
            if( fac > 0.0 ){
               fac = 32767.0 / fac ;
               EDIT_coerce_scale_type( nvox,fac ,
                                       MRI_float,fout[kk] , MRI_short,bout ) ;
               fac = 1.0 / fac ;
            }

            free( fout[kk] ) ;  /* don't need this anymore */

            /*-- put output brick into dataset, and store scale factor --*/

            EDIT_substitute_brick( new_dset , kk , MRI_short , bout ) ;
            tar[kk] = fac ;

            perc = (100 * kk) / nfreq ;
            PLUTO_set_meter( plint , perc ) ; /* on the progress meter */
         }

         /*-- save scale factor array into dataset --*/

         EDIT_dset_items( new_dset , ADN_brick_fac , tar , ADN_none ) ;

      }
      break ;

      /*** output is bytes (byte = unsigned char)
           we have to create a scaled sub-brick from fout ***/

      case MRI_byte:{
         byte * bout ;
         float fac ;

         for( kk=0 ; kk < nfreq ; kk++ ){  /* loop over sub-bricks */

            /*-- get output sub-brick --*/

            bout = (byte *) malloc( sizeof(byte) * nvox ) ;
            if( bout == NULL ){
               fprintf(stderr,"\nFinal malloc error in plug_power!\n\a") ;
               return("\nFinal malloc error in plug_power!\n\a") ;
               /* EXIT(1) ;*/
            }

            /*-- find scaling and then scale --*/

            fac = MCW_vol_amax( nvox,1,1 , MRI_float , fout[kk] ) ;
            if( fac > 0.0 ){
               fac = 255.0 / fac ;
               EDIT_coerce_scale_type( nvox,fac ,
                                       MRI_float,fout[kk] , MRI_byte,bout ) ;
               fac = 1.0 / fac ;
            }

            free( fout[kk] ) ;  /* don't need this anymore */

            /*-- put output brick into dataset, and store scale factor --*/

            EDIT_substitute_brick( new_dset , kk , MRI_byte , bout ) ;
            tar[kk] = fac ;

            perc = (100 * kk) / nfreq ;
            PLUTO_set_meter( plint , perc ) ; /* on the progress meter */
         }

         /*-- save scale factor array into dataset --*/

         EDIT_dset_items( new_dset , ADN_brick_fac , tar , ADN_none ) ;
      }
      break ;

   } /* end of switch on output data type */

   /*-------------- Cleanup and go home ----------------*/

   PLUTO_set_meter( plint , 100 ) ;  /* set progress meter to 100% */

   PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ;

   FREE_WORKSPACE ;
   return NULL ;  /* null string returned means all was OK */
}

#ifdef ALLOW_TESTING
/*****************************************************************************
 -----------------------------------------------------------------------------
           Create the second interface within this plugin.
 -----------------------------------------------------------------------------*/

PLUGIN_interface * TEST_init( void )
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "Testing" ,
                                "Testing, Testing, 1-2-3 ..." ,
                                NULL ,
                                PLUGIN_CALL_VIA_MENU , TEST_main  ) ;

   PLUTO_add_hint( plint , "1-2-3, 1-2-3, ..." ) ;

   PLUTO_add_option( plint ,
                     "Input" ,  /* label at left of input line */
                     "Input" ,  /* tag to return to plugin */
                     TRUE       /* is this mandatory? */
                   ) ;

   PLUTO_add_dataset_list(  plint ,
                            "Datasets" ,       /* label next to button   */
                            ANAT_ALL_MASK ,    /* take any anat datasets */
                            FUNC_FIM_MASK ,    /* only allow fim funcs   */
                            DIMEN_4D_MASK |    /* need 3D+time datasets  */
                            BRICK_ALLREAL_MASK /* need real-valued datasets */
                         ) ;
   return plint ;
}

char * TEST_main( PLUGIN_interface * plint )
{
   MRI_IMAGE * tsim ;
   MCW_idclist * idclist ;
   MCW_idcode * idc ;
   THD_3dim_dataset * dset ;
   char str[256] ;
   int id ;

   /*--------- go to first input line ---------*/

   PLUTO_next_option(plint) ;

   idclist = PLUTO_get_idclist(plint) ;
   if( PLUTO_idclist_count(idclist) == 0 )
      return " \nNo input dataset list!\n " ;

   id = 0 ;
   do {
      idc  = PLUTO_idclist_next(idclist) ;
      dset = PLUTO_find_dset(idc) ;
      if( dset == NULL ) return NULL ;
      id++ ;
      sprintf(str, " \nDataset %d = %s\n nx = %d\n ny = %d\n nz = %d\n " ,
              id , DSET_FILECODE(dset) , dset->daxes->nxx,dset->daxes->nyy,dset->daxes->nzz ) ;

      PLUTO_popup_transient( plint , str ) ;
   } while(1) ;
   return NULL ;
}
#endif  /* ALLOW_TESTING */
