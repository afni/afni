/*****************************************************************************
   Major portions of this software are copyrighted by the Medical College
   of Wisconsin, 1994-2000, and are released under the Gnu General Public
   License, Version 2.  See the file README.Copyright for details.
******************************************************************************/
   
#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

#define TEST_VOXEL 6177
#define TEST_TIME 0
#define RMB_DEBUG 0

/***********************************************************************
  Plugin that averages epochs from single trial data
************************************************************************/

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "Purpose: Averagin epochs of single trial data\n"
  "\n"
  "Input items to this plugin are:\n"
  "   Datasets:   Input  = 3D+time dataset to process\n"
  "                      = reference time-series\n"
  "               Output = Prefix for new dataset\n"
  "   Additional Parameters\n"
  "               delta     = shift timeseries by delta before splitting and averaging\n"
  "               maxlength = maximum avg ts length\n"
  "               no1?      = images w/ only one img in avg ignored\n"
  "Author -- RM Birn"
;

/*------------- strings for output format -------------*/

static char * yes_no_strings[] = { "No" , "Yes" } ;

/*--------------- prototypes for internal routines ---------------*/

char * STAVG_main( PLUGIN_interface * ) ;  /* the entry point */

float ** avg_epochs( THD_3dim_dataset * dset, float * ref, 
                    int user_maxlength, int no1, PLUGIN_interface *plint );

MRI_IMARR * dset_to_mri(THD_3dim_dataset * dset);


/*---------------------------- global data ---------------------------*/

static PLUGIN_interface * global_plint = NULL ;
int M_maxlength;

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

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;     /* will be the output of this routine */

   if( ncall > 0 ) return NULL ;  /* one interfaces */

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "SingleTrial Avg" ,
                                "Averaging of epochs in Single Trial data" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , STAVG_main  ) ;

   PLUTO_add_hint( plint , "Averaging of epochs in Single Trial data" ) ;

   global_plint = plint ;  /* make global copy */

   PLUTO_set_sequence( plint , "z:Birn" ) ;

   /*--------- 1st line ---------*/

   PLUTO_add_option( plint ,
                     "Datasets" ,  /* label at left of input line */
                     "Datasets" ,  /* tag to return to plugin */
                     TRUE          /* is this mandatory? */
                   ) ;

   PLUTO_add_dataset(  plint ,
                       "Input" ,          /* label next to button   */
                       ANAT_ALL_MASK ,    /* take any anat datasets */
                       FUNC_FIM_MASK ,    /* only allow fim funcs   */
                       DIMEN_4D_MASK |    /* need 3D+time datasets  */
                       BRICK_ALLREAL_MASK /* need real-valued datasets */
                    ) ;
   PLUTO_add_hint( plint , "Input 3d+t dataset" ) ;

   PLUTO_add_string( plint ,
                     "Output" ,  /* label next to textfield */
                     0,NULL ,    /* no fixed strings to choose among */
                     19          /* 19 spaces for typing in value */
                   ) ;
   PLUTO_add_hint( plint , "Name of output dataset" ) ;

   /*---------- 2nd line --------*/

   PLUTO_add_option( plint ,
                     "Timing" ,
                     "Timing" ,
                     TRUE
                   ) ;


   PLUTO_add_timeseries(plint, "Stim. Timing");
   PLUTO_add_hint( plint , "Stimulus Timing (0 = no task, 1 = task)" ) ;

   PLUTO_add_number( plint ,
                     "delta" ,   
                     -1000 ,    
                     1000 ,  
                     0 ,    
                     0 ,   
                     TRUE
                   ) ;
   PLUTO_add_hint( plint , "Shift data timecourse by delta before splitting and averaging" ) ;

   

   /*---------- 3rd line --------*/

   PLUTO_add_option( plint ,
                     "Parameters" ,  /* label at left of input line */
                     "Parameters" ,  /* tag to return to plugin */
                     FALSE            /* is this mandatory? */
                   ) ;

   PLUTO_add_number( plint ,
                     "maxlength" ,    /* label next to chooser */
                     0 ,         /* smallest possible value */
                     1000 ,        /* largest possible value */
                     0 ,         /* decimal shift (none in this case) */
                     15 ,         /* default value */
                     TRUE       /* allow user to edit value? */
                   ) ;
   PLUTO_add_hint( plint , "maximum # of timepoints of output dataset" ) ;

   PLUTO_add_string( plint ,
                     "no1?" ,               /* label next to chooser button */
                     2  ,               /* number of strings to choose among */
                     yes_no_strings ,  /* list of strings to choose among */
                     1                  /* index of default string */
                   ) ;

   PLUTO_add_hint( plint , "ignore timepoints where only one image is in average" ) ;


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
#define FREEUP(x) if((x) != NULL){free((x)); (x)=NULL;}

#define FREE_WORKSPACE                              \
  do{ FREEUP(bptr) ; FREEUP(sptr) ; FREEUP(fptr) ;  \
      FREEUP(fout) ; \
      FREEUP(fxar) ; \
    } while(0)

/*-------------------------------------------------------------------------*/

char * STAVG_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;                          /* input dataset idcode */
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   char * new_prefix , * str , * str2;         /* strings from user */
   int   new_datum ,                           /* control parameters */
         old_datum , ntime ;

   int   te, ne, tinc, kim, nia;
   int   numepochs, minlength, maxlength, lastindex, navgpts;
   int   nvox , perc , new_units, old_units ;
   int   ii, ibot,itop , kk, jj; 
   int   no1, user_maxlength, delta;
   int   *pEpochLength, *pTimeIndex;
   int   nx, ny, nz, npix;
   float *pNumAvg;
   float old_dtime;

   MRI_IMAGE * stimim;
   
   MRI_IMARR *avgimar;

   byte   ** bptr  = NULL ;  /* one of these will be the array of */
   short  ** sptr  = NULL ;  /* pointers to input dataset sub-bricks */
   float  ** fptr  = NULL ;  /* (depending on input datum type) */

   float   * fxar  = NULL ;  /* array loaded from input dataset */
   float   * stimar = NULL ;
   float  ** fout  = NULL ;  /* will be array of output floats */

   float   * tar   = NULL ;  /* will be array of taper coefficients */

   float   * nstimar;

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

   ntime = DSET_NUM_TIMES(old_dset) ;
   if( ntime < 2 )
      return "*****************************\n"
             "Dataset has only 1 time point\n"
             "*****************************"  ;

   ii = DSET_NVALS_PER_TIME(old_dset) ;
   if( ii > 1 )
      return "************************************\n"
             "Dataset has > 1 value per time point\n"
             "************************************"  ;
   
   old_datum = DSET_BRICK_TYPE( old_dset , 0 ) ; /* get old dataset datum type */
   new_datum = old_datum;
   old_dtime = DSET_TIMESTEP(old_dset);
   old_units = DSET_TIMEUNITS(old_dset);
   
   nvox = old_dset->daxes->nxx * old_dset->daxes->nyy * old_dset->daxes->nzz;
   npix = old_dset->daxes->nxx * old_dset->daxes->nyy;
   nx = old_dset->daxes->nxx;


   new_prefix = PLUTO_get_string(plint) ;   /* get string item (the output prefix) */
   if( ! PLUTO_prefix_ok(new_prefix) )      /* check if it is OK */
      return "************************\n"
             "Output Prefix is illegal\n"
             "************************"  ;

   /*--------- go to next input line ---------*/

   PLUTO_next_option(plint);

   stimim = PLUTO_get_timeseries(plint);
   if( stimim == NULL ) return "Please specify stimulus timing";

   if( stimim->nx < ntime ){
      return "**************************************\n"
             "Not enough pts in stimulus time-series\n"
             "**************************************";
   }

   stimar = MRI_FLOAT_PTR(stimim);


   delta = PLUTO_get_number(plint);

   if( abs(delta) > ntime ){
      return "************************\n"
             "Delta shift is too large\n"
             "************************";
   }
  
   /*initialize variables if not user specified */
   user_maxlength = ntime;
   no1 = 0;

   /*--------- see if the 3rd option line is present --------*/

   str = PLUTO_get_optiontag( plint ) ;
   if( str != NULL ){
      user_maxlength = (int) PLUTO_get_number(plint) ;
      str2  = PLUTO_get_string(plint) ;      /* get string item (the method) */
      no1   = PLUTO_string_index( str2 ,      /* find it in list it is from */
                                 2 ,
                                 yes_no_strings) ;
   }
   

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   PLUTO_popup_meter( plint ) ;  /* popup a progress meter */

   /*________________[ Main Code ]_________________________*/
  
   fout = avg_epochs( old_dset, stimar, user_maxlength, 1, plint );
   
   if( RMB_DEBUG ) fprintf(stderr, "Done with avg_epochs\n");
   maxlength = M_maxlength;
   
   
   /*______________________________________________________*/

   
   new_dset = EDIT_empty_copy( old_dset ) ; /* start with copy of old one */

   { char * his = PLUTO_commandstring(plint) ;
     tross_Copy_History( old_dset , new_dset ) ;
     tross_Append_History( new_dset , his ) ; free( his ) ;
   }
   
   /*-- edit some of its internal parameters --*/
   ii = EDIT_dset_items(
           new_dset ,
              ADN_prefix      , new_prefix ,           /* filename prefix */
              ADN_malloc_type , DATABLOCK_MEM_MALLOC , /* store in memory */
              ADN_datum_all   , new_datum ,            /* atomic datum */
              ADN_nvals       , maxlength ,            /* # sub-bricks */
              ADN_ntt         , maxlength ,            /* # time points */
           /*   ADN_ttorg       , old_dtime ,  */              /* time origin */
           /*   ADN_ttdel       , old_dtime ,  */            /* time step */
           /*   ADN_ttdur       , old_dtime ,  */            /* time duration */
           /*   ADN_nsl         , 0 ,          */        /* z-axis time slicing */
           /*   ADN_tunits      , old_units ,  */        /* time units */
           ADN_none ) ;

   if( ii != 0 ){
      THD_delete_3dim_dataset( new_dset , False ) ;
      FREE_WORKSPACE ;
      return "***********************************\n"
             "Error while creating output dataset\n"
             "***********************************"  ;
   }


   /*------------------------------------------------------------*/
   /*------- The output is now in fout[kk][ii],
             for kk=0..maxlength-1 , ii=0..nvox-1.
             We must now put this into the output dataset -------*/

   switch( new_datum ){

      /*** output is floats is the simplest:
           we just have to attach the fout bricks to the dataset ***/

      case MRI_float:
         for( kk=0 ; kk < maxlength ; kk++ )
            EDIT_substitute_brick( new_dset , kk , MRI_float , fout[kk] ) ;
      break ;

      /*** output is shorts:
           we have to create a scaled sub-brick from fout ***/

      case MRI_short:{
         short * bout ;
         float fac ; 

         for( kk=0 ; kk < maxlength ; kk++ ){  /* loop over sub-bricks */

            /*-- get output sub-brick --*/
            bout = (short *) malloc( sizeof(short) * nvox ) ;
            if( bout == NULL ){
               fprintf(stderr,"\nFinal malloc error in plug_stavg!\n\a") ;
               exit(1) ;
            }

            /*-- find scaling and then scale --*/
            /*fac = MCW_vol_amax( nvox,1,1 , MRI_float , fout[kk] ) ;*/
            fac = 1.0;
            EDIT_coerce_scale_type( nvox,fac ,
                                    MRI_float,fout[kk] , MRI_short,bout ) ;
            free( fout[kk] ) ;  /* don't need this anymore */

            /*-- put output brick into dataset, and store scale factor --*/
            EDIT_substitute_brick( new_dset , kk , MRI_short , bout ) ;
         }
      }
      break ;

      /*** output is bytes (byte = unsigned char)
           we have to create a scaled sub-brick from fout ***/

      case MRI_byte:{
         byte * bout ;
         float fac ;

         for( kk=0 ; kk < maxlength ; kk++ ){  /* loop over sub-bricks */

            /*-- get output sub-brick --*/

            bout = (byte *) malloc( sizeof(byte) * nvox ) ;
            if( bout == NULL ){
               fprintf(stderr,"\nFinal malloc error in plug_stavg!\n\a") ;
               exit(1) ;
            }

            /*-- find scaling and then scale --*/

            fac = 1.0;
            EDIT_coerce_scale_type( nvox,fac ,
                                    MRI_float,fout[kk] , MRI_byte,bout ) ;

            free( fout[kk] ) ;  /* don't need this anymore */

            /*-- put output brick into dataset, and store scale factor --*/

            EDIT_substitute_brick( new_dset , kk , MRI_byte , bout ) ;
         }

      }
      break ;

   } /* end of switch on output data type */

   /*-------------- Cleanup and go home ----------------*/

   PLUTO_set_meter( plint , 100 ) ;  /* set progress meter to 100% */

   PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ;

   FREE_WORKSPACE ;
   return NULL ;  /* null string returned means all was OK */
}

/*---------------------------------------------------------------*/
float ** avg_epochs( THD_3dim_dataset * dset, float * ref, 
                    int user_maxlength, int no1, PLUGIN_interface * plint )
/*---------------------------------------------------------------*/
{

   int     numepochs, lastindex;
   int     nvox, numims, nx, ny, nz;
   int     kim, ne, te, tinc, nia;
   int     ii, kk;
   int     maxlength, minlength;
   int     datum;
   float ** fxar;
   float ** outar;    /* output averaged time-series */
   float * pNumAvg;  /* array for number of pts to avg at each time*/
   int   * pTimeIndex; /* array of time markers (1st img of each epoch) */
   int   * pEpochLength; /* array of epoch lengths */
   float ** tempar;
   MRI_IMARR *inimar;
   
   
   nx = dset->daxes->nxx;
   ny = dset->daxes->nyy;
   nz = dset->daxes->nzz;
   nvox = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz;
   numims = DSET_NUM_TIMES(dset);
   datum = DSET_BRICK_TYPE( dset , 0 ) ; /* get old dataset datum type */
   
   PLUTO_popup_meter(plint) ;
   
   DSET_load(dset);
   
   inimar =  dset_to_mri(dset);
   if( inimar == NULL ) return "Error in reading data";

   fxar = (float **) malloc( sizeof( float *) * numims);
   if( datum == MRI_float){
      for( kk=0; kk<numims; kk++){
         fxar[kk] = MRI_FLOAT_PTR(IMAGE_IN_IMARR(inimar,kk));
      }
   }
   else{
      for( kk=0; kk<numims; kk++){
         fxar[kk] = MRI_FLOAT_PTR(mri_to_float(IMAGE_IN_IMARR(inimar,kk)));
      }
   }

   nia = 0;    /* number of images (timepoints) averaged  where num epochs > 1*/
   
   if( RMB_DEBUG ) fprintf(stderr, "Start stavg\n");
   
   /* determine number of epochs to average */
   if( RMB_DEBUG ) fprintf(stderr, "Determining number of epochs...");
   numepochs = 1;
   for( kim=0; kim < numims; kim++ ){
      if( ref[kim] > 0) numepochs++;
   }
   if( RMB_DEBUG ) fprintf(stderr, "done\n");

   /* set array of epoch lengths */
   if( RMB_DEBUG ) fprintf(stderr, "Set array of epoch lengths...");
   pEpochLength = (int *)malloc(sizeof(int) * numepochs);
   for( ne=0; ne < numepochs; ne++) pEpochLength[ne] = 0;
   ne = 0;
   for( kim=0; kim < numims; kim++ ){
      if( ref[kim] > 0) ne++;
      pEpochLength[ne]++;
   }
   if( RMB_DEBUG ) fprintf(stderr, "done\n");

   /* set array of time markers (1st img of each epoch) */
   if( RMB_DEBUG ) fprintf(stderr, "Set array of time markers...");
   pTimeIndex = (int *)malloc(sizeof(int) * (numepochs - 1));
   lastindex = 0;
   minlength = numims;
   maxlength = 0;
   for( ne=0; ne < (numepochs-1); ne++){
      pTimeIndex[ne] = lastindex + pEpochLength[ne];
      lastindex = pTimeIndex[ne];
      if(pEpochLength[ne+1] > maxlength) maxlength = pEpochLength[ne+1];
      if(pEpochLength[ne+1] < minlength) minlength = pEpochLength[ne+1];
   }
   if( RMB_DEBUG ) fprintf(stderr, "done\n");

   if(maxlength > user_maxlength) maxlength = user_maxlength;

   if( RMB_DEBUG ) fprintf(stderr, "init...");
   pNumAvg = (float *) malloc( sizeof(float) * maxlength);
   outar = (float **) malloc( sizeof(float *) * maxlength);
   for( te=0; te < maxlength; te++){
      outar[te] = (float *) malloc( sizeof(float) * nvox);
   }

   for( te=0; te < maxlength; te++){
      for( ii=0; ii<nvox; ii++){
         outar[te][ii] = 0.0;
      }
   }
   if( RMB_DEBUG ) fprintf(stderr, "done\n");
   if( RMB_DEBUG ) fprintf(stderr, "Start averaging...");
   for( te=0; te < maxlength; te++){
      pNumAvg[te] = 0.0;   
      for( ne=0; ne < (numepochs-1); ne++){
         tinc = pTimeIndex[ne] + te;
         if( te < pEpochLength[ne+1] ){
            for( ii=0; ii<nvox; ii++){
               outar[te][ii] += fxar[tinc][ii];
            }
            pNumAvg[te]++;
         }
      }
      for( ii=0; ii<nvox; ii++){
         outar[te][ii] = outar[te][ii]/pNumAvg[te];
      }
      if( pNumAvg[te] > 1) nia ++;
      PLUTO_set_meter(plint, (100*(te+1))/maxlength ) ;
   }
   if( RMB_DEBUG ) fprintf(stderr, "done\n");
   if ( no1 ){     /* ignore images with only one average */
      if( nia < maxlength) maxlength = nia;
   }
   
   M_maxlength = maxlength;
   
   
   if( RMB_DEBUG ) fprintf(stderr, "malloc output...");
   tempar = (float **) malloc(sizeof(float *) * maxlength);
   for( te=0 ; te < maxlength ; te++ ){
      tempar[te] = (float *) malloc( sizeof(float) * nvox);
   }
   if( RMB_DEBUG ) fprintf(stderr, "done\n");
   
   if( RMB_DEBUG ) fprintf(stderr, "convert to output...");
   for( te=0; te < maxlength; te++){
      for( ii=0; ii<nvox; ii++){
         tempar[te][ii] = outar[te][ii];
      }
   }
   if( RMB_DEBUG ) fprintf(stderr, "done\n");

   /* toss arrays */
   if( RMB_DEBUG ) fprintf(stderr, "free mem...");
   FREE_IMARR( inimar );
   free( outar );
   free( pNumAvg );
   free( pTimeIndex );
   free( pEpochLength );
   if( RMB_DEBUG ) fprintf(stderr, "done\n");
   DSET_unload(dset);
   
   return(tempar);
}


/*-------------------------------------------------------*/
MRI_IMARR * dset_to_mri(THD_3dim_dataset * dset)
/*--------------------------------------------------------*/
{

   int ii, kk, ntime, datum;
   int nvox, nx, ny, nz;
   int use_fac;
   
   MRI_IMARR * ims_in;
   MRI_IMAGE * im, *temp_im;
   

   byte   ** bptr  = NULL ;  /* one of these will be the array of */
   short  ** sptr  = NULL ;  /* pointers to input dataset sub-bricks */
   float  ** fptr  = NULL ;  /* (depending on input datum type) */
   
   float * fac  = NULL ;  /* array of brick scaling factors */
   
   float * fout;
   

   ntime = DSET_NUM_TIMES(dset) ;
   nx = dset->daxes->nxx;
   ny = dset->daxes->nyy;
   nz = dset->daxes->nzz;
   nvox = dset->daxes->nxx * dset->daxes->nyy * dset->daxes->nzz ;
   datum = DSET_BRICK_TYPE( dset , 0 ) ; /* get dataset datum type */

   switch( datum ){  /* pointer type depends on input datum type */

      default:
         return NULL  ;

      /** create array of pointers into old dataset sub-bricks **/

      /*--------- input is bytes ----------*/
      /* voxel #i at time #k is bptr[k][i] */
      /* for i=0..nvox-1 and k=0..ntime-1.  */

      case MRI_byte:
         bptr = (byte **) malloc( sizeof(byte *) * ntime ) ;
         if( bptr == NULL ) return NULL ;
         for( kk=0 ; kk < ntime ; kk++ )
            bptr[kk] = (byte *) DSET_ARRAY(dset,kk) ;
      break ;

      /*--------- input is shorts ---------*/
      /* voxel #i at time #k is sptr[k][i] */
      /* for i=0..nvox-1 and k=0..ntime-1.  */

      case MRI_short:
         sptr = (short **) malloc( sizeof(short *) * ntime ) ;
         if( sptr == NULL ) return NULL ;
         for( kk=0 ; kk < ntime; kk++ )
            sptr[kk] = (short *) DSET_ARRAY(dset,kk) ;
      break ;

      /*--------- input is floats ---------*/
      /* voxel #i at time #k is fptr[k][i] */
      /* for i=0..nvox-1 and k=0..ntime-1.  */

      case MRI_float:
         fptr = (float **) malloc( sizeof(float *) * ntime) ;
         if( fptr == NULL ) return NULL ;
         for( kk=0 ; kk < ntime; kk++ )
            fptr[kk] = (float *) DSET_ARRAY(dset,kk) ;
      break ;

   } /* end of switch on input type */
   
   INIT_IMARR(ims_in) ;
   for( kk=0 ; kk < ntime ; kk++ ){
      im = mri_new_vol_empty( nx , ny , nz , datum ) ;
      ADDTO_IMARR(ims_in,im) ;
   }
   
   for( kk=0 ; kk < ntime ; kk++ ){
      im = IMARR_SUBIMAGE(ims_in,kk) ;
      
      switch( datum ){
         case MRI_byte:  mri_fix_data_pointer( bptr[kk], im ) ; break ;
         case MRI_short: mri_fix_data_pointer( sptr[kk], im ) ; break ;
         case MRI_float: mri_fix_data_pointer( fptr[kk], im ) ; break ;
      }
   }


   
   return(ims_in);
}


