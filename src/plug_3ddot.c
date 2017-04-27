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
  Simple plugin to compute dot product of two datasets.
************************************************************************/

static char helpstring[] =
  " Purpose: Compute correlation of two 3D datasets\n"
  " Inputs:\n"
  " Dataset 1   = 3D dataset that must already be in memory\n"
  " Dataset 2   = 3D dataset that must already be in memory\n"
  " Remove Mean = If this option is toggled, then the means\n"
  "                of each dataset will be removed before\n"
  "                the correlations are computed.\n"
  " The output is popped up into a window on the screen.\n"
  "Author -- RW Cox"
;

/*---------- prototypes for internal routines ----------*/

static char * DOT_main( PLUGIN_interface * ) ;

static double DSET_cor( int , THD_3dim_dataset * , THD_3dim_dataset * , double *sxy) ;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   CHECK_IF_ALLOWED("3DCORRELATION","3D Correlation") ;  /* 30 Sep 2016 */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "3D Correlation" , "3D Dataset Correlation" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , DOT_main  ) ;

   PLUTO_set_sequence( plint , "A:afniinfo:dset" ) ;

   PLUTO_add_hint( plint , "3D Dataset Correlation" ) ;

   /*-- first line of input: Dataset --*/

   PLUTO_add_option( plint , "Dataset" , "Dataset" , TRUE ) ;
   PLUTO_add_dataset(plint , "# 1" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    SESSION_ALL_MASK |
                                    DIMEN_3D_MASK    | BRICK_ALLREAL_MASK ) ;

   /*-- second line of input: Dataset --*/

   PLUTO_add_option( plint , "Dataset" , "Dataset" , TRUE ) ;
   PLUTO_add_dataset(plint , "# 2" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_3D_MASK | BRICK_ALLREAL_MASK ) ;

   /*-- third line of input: Remove Mean option --*/

   PLUTO_add_option( plint , "Remove Mean" , "Remove Mean" , False ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

static char * DOT_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;
   THD_3dim_dataset * xset , * yset ;
   char * tag ;
   int demean ;
   double dcor, sxy ;
   char str[256] ;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   PLUTO_next_option(plint) ;                             /* go to next input line */ 
   idc  = PLUTO_get_idcode(plint) ; /* get 1st dataset item */
   xset = PLUTO_find_dset(idc) ;                   /* get ptr to dataset */
   if( xset == NULL )
      return "**********************\n"
             "Cannot find Dataset #1\n"
             "**********************"  ;

   PLUTO_next_option(plint) ;
   idc  = PLUTO_get_idcode(plint) ;
   yset = PLUTO_find_dset(idc) ;
   if( yset == NULL )
      return "**********************\n"
             "Cannot find Dataset #2\n"
             "**********************"  ;

   /*-- The first two input lines (processed above) were mandatory.
        The next one is optional.  Check to see if it is present,
        and if it has the right name.
        If so, then mark its presence in the variable "demean".   --*/

   tag    = PLUTO_get_optiontag(plint) ;
   demean = ( tag != NULL && strcmp(tag,"Remove Mean") == 0 ) ;

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- do the actual work --*/

   dcor = DSET_cor( demean , xset , yset, &sxy ) ;

   if( dcor < -1.0 )
      return "*********************************\n"
             "Error while computing correlation\n"
             "*********************************"  ;

   /*-- put the output to the screen --*/

   sprintf(str , "    Dataset %s\n"
                 "and Dataset %s\n\n"
                 "Correlation = %g\n"
                 "        Dot = %g\n"
                 "%s" ,
           DSET_FILECODE(xset) , DSET_FILECODE(yset) ,
           dcor , dcor * sxy,
           (demean) ? "[mean removed]" : " " ) ;

   PLUTO_popup_message( plint , str ) ;

   return NULL ;  /* null string returned means all was OK */
}

/*----------------------------------------------------------------------------
  Compute the correlation between two datasets.  Returns a number
  less than -1.0 if an error occurs.
------------------------------------------------------------------------------*/

static double DSET_cor( int demean, THD_3dim_dataset * xset, THD_3dim_dataset * yset, double *sxy )
{
   double sumxx , sumyy , sumxy , tx,ty , dxy , xbar,ybar ;
   void  *  xar , *  yar ;
   float * fxar , * fyar ;
   int ii , nxyz , ivx,ivy , itypx,itypy , fxar_new,fyar_new ;

   
   if (sxy) *sxy = 0.0;
   
   /*-- check datasets for conformity in dimensions --*/
   
   nxyz = xset->daxes->nxx * xset->daxes->nyy * xset->daxes->nzz ;

   if(  yset->daxes->nxx * yset->daxes->nyy * yset->daxes->nzz != nxyz )
      return -666.0 ;

   /*-- load the first dataset into memory --*/

   DSET_load( xset ) ;

   ivx   = DSET_PRINCIPAL_VALUE(xset) ;  /* most important place */
   itypx = DSET_BRICK_TYPE(xset,ivx) ;   /* type of data stored here */
   xar   = DSET_ARRAY(xset,ivx) ;        /* get the array */
   if( xar == NULL ){
      DSET_unload(xset) ;  /* free memory if an error happened */
      return -666.0 ;
   }

   /*-- if the array is not floating point, make a floating point copy --*/

   if( itypx == MRI_float ){
      fxar = (float *) xar ; fxar_new = 0 ;
   } else {
      fxar = (float *) malloc( sizeof(float) * nxyz ) ; fxar_new = 1 ;
      EDIT_coerce_type( nxyz , itypx,xar , MRI_float,fxar ) ;
      DSET_unload( xset ) ;  /* don't need this in memory anymore */
   }

   /*-- do the same for the second dataset --*/

   DSET_load( yset ) ;

   ivy   = DSET_PRINCIPAL_VALUE(yset) ;  /* most important place */
   itypy = DSET_BRICK_TYPE(yset,ivy) ;   /* type of data stored here */
   yar   = DSET_ARRAY(yset,ivy) ;        /* get the array */
   if( yar == NULL ){
      if( fxar_new ) free(fxar) ; else DSET_unload(xset) ;  /* free memory */
      DSET_unload(yset) ;                                   /* if an error */
      return -666.0 ;
   }

   /*-- if the array is not floating point, make a floating point copy --*/

   if( itypy == MRI_float ){
      fyar = (float *) yar ; fyar_new = 0 ;
   } else {
      fyar = (float *) malloc( sizeof(float) * nxyz ) ; fyar_new = 1 ;
      EDIT_coerce_type( nxyz , itypy,yar , MRI_float,fyar ) ;
      DSET_unload( yset ) ;  /* don't need this in memory anymore */
   }

   /*-- if needed, compute the mean of each dataset --*/

   xbar = ybar = 0.0 ;
   if( demean ){
      for( ii=0 ; ii < nxyz ; ii++ ){
         xbar += fxar[ii] ;
         ybar += fyar[ii] ;
      }
      xbar /= nxyz ;
      ybar /= nxyz ;
   }

   /*-- now compute the quadratic sums --*/

   sumxx = sumyy = sumxy = 0.0 ;
   for( ii=0 ; ii < nxyz ; ii++ ){
      tx = (fxar[ii]-xbar) ; ty = (fyar[ii]-ybar) ;
      sumxx += tx * tx ;
      sumyy += ty * ty ;
      sumxy += tx * ty ;
   }

   /*-- free up arrays --*/

   if( fxar_new ) free(fxar) ; else DSET_unload(xset) ;
   if( fyar_new ) free(fyar) ; else DSET_unload(yset) ;

   /*-- compute correlation --*/

   dxy = sumxx * sumyy ; if( dxy <= 0.0 ) return -666.0 ;
   if (sxy) *sxy =  sqrt(dxy) ;
   dxy = sumxy / sqrt(dxy) ;
   return dxy ;
}
