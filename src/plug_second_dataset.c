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
  Plugin to provide a 2nd dataset's timeseries in place of first
************************************************************************/

char * DSET2_main( PLUGIN_interface *) ;
void   DSET2_func( int num , double to,double dt, float * vec ) ;

static char helpstring[] =
   " Purpose: Control the 'Dataset#2' 1D timeseries function\n"
   "\n"
   " Dataset = a 3D+time dataset from which to extract data\n"
   "\n"
   " Justify = Left  means to put timeseries data at start of output array\n"
   "           Right means to put timeseries data at end of output array\n"
   "\n"
   " Fill    = Extend means to copy the last value\n"
   "           Zero   means to fill with zeros\n"
   "\n"
   " Justify and Fill are only used when the number of points in the input\n"
   " dataset doesn't match the number of points in the timeseries being replaced.\n"
   " Let M = length of dataset, N = length of timeseries,\n"
   " ts[] = timeseries array, ds[] = dataset array.\n"
   "\n"
   " Case: M < N (too little data to fill timeseries)\n"
   "    Left: ts[i] = ds[i]    i=0..M-1                        Data goes to left\n"
   "                = ds[M-1]  i=M..N-1 if Extend              edge of timeseries\n"
   "                = 0.0      i=M..N-1 if Zero\n"
   "   Right: ts[i] = ds[0]    i=0..J-1 if Extend (with J=N-M) Data goes to right\n"
   "                = 0.0      i=0..J-1 if Zero                edge of timeseries\n"
   "                = ds[i-J]  i=J..N-1\n"
   " Case: M > N (too much data for timeseries)\n"
   "    Left: ts[i] = ds[i]    i=0..N-1                        Left data to ts[]\n"
   "   Right: ts[i] = ds[i+J]  i=0..N-1 (with J=M-N)           Right data to ts[]\n"
   "\n"
   " -- RWCox - 18 May 2000\n"
;

static THD_3dim_dataset * dset = NULL ;
static int justify = 0 ;
static int fill    = 0 ;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

static char *lr[2] = { "Left" , "Right" } ;
static char *ez[2] = { "Extend" , "Zero" } ;

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   AFNI_register_nD_function( 1 , "Dataset#2" , DSET2_func , NEEDS_DSET_INDEX ) ;

   plint = PLUTO_new_interface( "Dataset#2" , "Controls 1D function Dataset#2" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , DSET2_main  ) ;

   PLUTO_add_hint( plint , "Controls 1D function Dataset#2" ) ;

   PLUTO_set_sequence( plint , "A:funcs:dataset#2" ) ;

   PLUTO_add_option( plint , "Input" , "Input" , TRUE ) ;
   PLUTO_add_dataset(plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_4D_MASK | BRICK_ALLREAL_MASK ) ;

   PLUTO_add_option( plint , "Where" , "Where" , TRUE ) ;
   PLUTO_add_string( plint, "Justify", 2, lr, justify ) ;

   PLUTO_add_option( plint , "How" , "How" , TRUE ) ;
   PLUTO_add_string( plint, "Fill", 2, ez, fill ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * DSET2_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;
   char * str ;

   if( plint == NULL )
      return "***********************\n"
             "DSET2_main:  NULL input\n"
             "***********************"  ;

   PLUTO_next_option(plint) ;
   idc  = PLUTO_get_idcode(plint) ;
   dset = PLUTO_find_dset(idc) ;
   if( dset == NULL )
      return "********************************\n"
             "DSET2_main:  bad input dataset\n"
             "********************************"  ;

   PLUTO_next_option(plint) ;
   str = PLUTO_get_string(plint) ;
   justify = (strcmp(str,lr[0]) != 0) ;

   PLUTO_next_option(plint) ;
   str = PLUTO_get_string(plint) ;
   fill = (strcmp(str,ez[0]) != 0) ;

   return NULL ;
}

/*----------------------------------------------------------------------------*/

void DSET2_func( int num , double to,double dt, float * vec )
{
   int ii , ijk , jj ;
   MRI_IMAGE * tsim ;
   float val ;

   if( !ISVALID_DSET(dset) ) return ;                /* nothing to do */

   DSET_load(dset) ;                                 /* if needed */

   ijk = AFNI_needs_dset_ijk() ;                     /* voxel index from AFNI */

   tsim = THD_extract_series( ijk , dset , 0 ) ;     /* get data */

   if( tsim == NULL ) return ;                       /* bad news */

   if( tsim->nx == num ){                            /* exact fit */

      memcpy(vec,MRI_FLOAT_PTR(tsim),sizeof(float)*num) ;  /* copy data */

   } else if( tsim->nx < num ){                      /* too little data */

      if( justify == 0 ){                            /* left justify */
         memcpy(vec,MRI_FLOAT_PTR(tsim),sizeof(float)*tsim->nx) ;

         val = (fill == 0) ? vec[tsim->nx-1]         /* extend fill */
                           : 0.0             ;       /* zero fill   */

         for( ii=tsim->nx ; ii < num ; ii++ ) vec[ii] = val ;

      } else {                                       /* right justify */
         jj = num - tsim->nx ;
         memcpy(vec+jj,MRI_FLOAT_PTR(tsim),sizeof(float)*tsim->nx) ;

         val = (fill == 0) ? vec[jj]                 /* extend fill */
                           : 0.0     ;               /* zero fill   */

         for( ii=0 ; ii < jj ; ii++ ) vec[ii] = val ;
      }

   } else {                                          /* too much data */

      if( justify == 0 ){                            /* left justify */
         memcpy(vec,MRI_FLOAT_PTR(tsim),sizeof(float)*num) ;
      } else {
         jj = tsim->nx - num ;
         memcpy(vec,MRI_FLOAT_PTR(tsim)+jj,sizeof(float)*num) ;
      }
   }

   mri_free(tsim) ;                                  /* toss the trash */
   return ;
}
