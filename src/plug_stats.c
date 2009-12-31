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
  Plugin to compute voxelwise mean, slope, or sigma of a 3D+time dataset.
************************************************************************/

/*--------------------- string to 'help' the user --------------------*/

static char helpstring[] =
  "Purpose: Compute mean, slope, or sigma of a 3D+time dataset.\n"
  "Input items are:\n"
  "   3d+time = 3D+time dataset to analyze\n"
  "   Method  = Mean, Slope, or Sigma = type of analysis to do\n"
  "   Ignore  = How many points to ignore at start\n"
  "\n"
  "Output: Prefix = Filename prefix for new dataset"
;

/*--------------------- strings for output format --------------------*/

static char * method_strings[] = { "Mean" , "Slope" , "Sigma" , "CVar" } ;

#define NUM_METHOD_STRINGS (sizeof(method_strings)/sizeof(char *))

#define METH_MEAN  0
#define METH_SLOPE 1
#define METH_SIGMA 2
#define METH_CVAR  3

/*----------------- prototypes for internal routines -----------------*/

char * STATS_main( PLUGIN_interface * ) ;  /* the entry point */

void STATS_tsfunc( double tzero , double tdelta ,
                   int npts , float ts[] , double ts_mean , double ts_slope ,
                   void * ud , float * val ) ;

/*---------------------------- global data ---------------------------*/

static PLUGIN_interface * global_plint = NULL ;

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

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*---------------- set titles and call point ----------------*/

   plint = PLUTO_new_interface( "3D+t Statistic" ,
                                "Voxel Statistics of 3D+time Dataset" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , STATS_main  ) ;

   PLUTO_add_hint( plint , "Voxel Statistics of 3D+time Dataset" ) ;

   PLUTO_set_sequence( plint , "A:newdset:statistics" ) ;

   global_plint = plint ;  /* make global copy */

   /*--------- 1st line: Input dataset ---------*/

   PLUTO_add_option( plint ,
                     "Input" ,  /* label at left of input line */
                     "Input" ,  /* tag to return to plugin */
                     TRUE       /* is this mandatory? */
                   ) ;

   PLUTO_add_dataset(  plint ,
                       "3D+time" ,        /* label next to button   */
                       ANAT_ALL_MASK ,    /* take any anat datasets */
                       FUNC_FIM_MASK ,    /* only allow fim funcs   */
                       DIMEN_4D_MASK |    /* need 3D+time datasets  */
                       BRICK_ALLREAL_MASK /* need real-valued datasets */
                    ) ;

   PLUTO_add_hint( plint , "Choose input dataset" ) ;

   /*---------- 2nd line: other inputs ----------*/

   PLUTO_add_option( plint ,
                     "Input" ,  /* label at left of input line */
                     "Input" ,  /* tag to return to plugin */
                     TRUE       /* is this mandatory? */
                   ) ;

   PLUTO_add_hint( plint , "Control parameters" ) ;

   PLUTO_add_string( plint ,
                     "Method" ,           /* label next to chooser button */
                     NUM_METHOD_STRINGS , /* number of strings to choose among */
                     method_strings ,     /* list of strings to choose among */
                     0                    /* index of default string */
                   ) ;

   PLUTO_add_hint( plint , "Choose statistic to compute" ) ;

   PLUTO_add_number( plint ,
                     "Ignore" ,  /* label next to chooser */
                     0 ,         /* smallest possible value */
                     20 ,        /* largest possible value */
                     0 ,         /* decimal shift (none in this case) */
                     3 ,         /* default value */
                     FALSE       /* allow user to edit value? */
                   ) ;

   PLUTO_add_hint( plint , "Number of points to ignore at start of time series" ) ;

   /*---------- 3rd line: Output dataset ----------*/

   PLUTO_add_option( plint ,
                     "Output" ,  /* label at left of input line */
                     "Output" ,  /* tag to return to plugin */
                     TRUE        /* is this mandatory? */
                   ) ;

   PLUTO_add_string( plint ,
                     "Prefix" ,  /* label next to textfield */
                     0,NULL ,    /* no fixed strings to choose among */
                     19          /* 19 spaces for typing in value */
                   ) ;

   PLUTO_add_hint( plint , "Name of output dataset" ) ;

   /*--------- done with interface setup ---------*/

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
  If the return string is not NULL, some error transpired, and
  AFNI will popup the return string in a message box.
****************************************************************************/

char * STATS_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;                          /* input dataset idcode */
   THD_3dim_dataset * old_dset , * new_dset ;  /* input and output datasets */
   char * new_prefix , * str ;                 /* strings from user */
   int meth , ignore ;

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

   /*--------- go to next input line ---------*/

   PLUTO_next_option(plint) ;

   str  = PLUTO_get_string(plint) ;      /* get string item (the method) */
   meth = PLUTO_string_index( str ,      /* find it in list it is from */
                              NUM_METHOD_STRINGS ,
                              method_strings ) ;

   ignore = PLUTO_get_number(plint) ;    /* get number item */

   /*--------- go to next input line ---------*/

   PLUTO_next_option(plint) ;

   new_prefix = PLUTO_get_string(plint) ;   /* get string item (the output prefix) */
   if( ! PLUTO_prefix_ok(new_prefix) )      /* check if it is OK */
      return "************************\n"
             "Output Prefix is illegal\n"
             "************************"  ;

   /*------------- ready to compute new dataset -----------*/

   new_dset = PLUTO_4D_to_fim( old_dset ,             /* input dataset */
                               new_prefix ,           /* output prefix */
                               ignore ,               /* ignore count */
                               1 ,                    /* detrend = ON */
                               STATS_tsfunc ,         /* timeseries processor */
                               ITOP(meth)             /* data for tsfunc */
                             ) ;

   PLUTO_add_dset( plint , new_dset , DSET_ACTION_MAKE_CURRENT ) ;

   return NULL ;  /* null string returned means all was OK */
}

/**********************************************************************
   Function that does the real work
***********************************************************************/

void STATS_tsfunc( double tzero , double tdelta ,
                   int npts , float ts[] , double ts_mean , double ts_slope ,
                   void * ud , float * val )
{
   int meth = PTOI(ud) ;
   static int nvox , ncall ;

   /** is this a "notification"? **/

   if( val == NULL ){

      if( npts > 0 ){  /* the "start notification" */

         PLUTO_popup_meter( global_plint ) ;  /* progress meter  */
         nvox  = npts ;                       /* keep track of   */
         ncall = 0 ;                          /* number of calls */

      } else {  /* the "end notification" */

         PLUTO_set_meter( global_plint , 100 ) ; /* set meter to 100% */

      }
      return ;
   }

   /** OK, actually do some work **/

   switch( meth ){

      default:
      case METH_MEAN:  *val = ts_mean  ; break ;

      case METH_SLOPE: *val = ts_slope ; break ;

      case METH_CVAR:
      case METH_SIGMA:{
         register int ii ;
         register double sum ;

         sum = 0.0 ;
         for( ii=0 ; ii < npts ; ii++ ) sum += ts[ii] * ts[ii] ;

         sum = sqrt( sum/(npts-1) ) ;

         if( meth == METH_SIGMA )  *val = sum ;
         else if( ts_mean != 0.0 ) *val = sum / fabs(ts_mean) ;
         else                      *val = 0.0 ;
      }
   }

   /** set the progress meter to the % of completion **/

   ncall++ ;
   PLUTO_set_meter( global_plint , (100*ncall)/nvox ) ;
   return ;
}
