#include "afni.h"

/*****************************************************************************
  This software is copyrighted and owned by the Medical College of Wisconsin.
  See the file README.Copyright for details.
******************************************************************************/

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to plot a histogram of a sub-brick.
  [Interface is adapted from plug_maskave.c - RWCox - 30 Sep 1999]
************************************************************************/

char * HISTO_main( PLUGIN_interface * ) ;

static char helpstring[] =
   " Purpose: Plot a histogram of data from a dataset brick.\n"
   "\n"
   " Source:  Dataset   = data to be processed\n"
   "          Sub-brick = which one to use\n"
   " Mask:    Dataset   = masking dataset\n"
   "          Sub-brick = which one to use\n\n"
   " Range:   Bottom    = min value from mask dataset to use\n"
   "          Top       = max value from mask dataset to use\n"
   "          [if Bottom >  Top, then all nonzero mask voxels will be used; ]\n"
   "          [if Bottom <= Top, then only nonzero mask voxels in this range]\n"
   "          [                  will be used in computing the statistics.  ]\n\n"
   " Author -- RW Cox - 30 September 1999\n"
;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "Histogram" , "Histogram of Dataset Brick" ,
                                 helpstring ,
                                 PLUGIN_CALL_VIA_MENU , HISTO_main  ) ;

   PLUTO_add_hint( plint , "Histogram of Dataset Brick" ) ;

   PLUTO_set_sequence( plint , "A:afniinfo:dset" ) ;

   /*-- first line of input --*/

   PLUTO_add_option( plint , "Source" , "Source" , TRUE ) ;
   PLUTO_add_dataset(plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;

   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ;

   /*-- second line of input --*/

   PLUTO_add_option( plint , "Mask" , "Mask" , FALSE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_3D_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ;

   /*-- third line of input --*/

   PLUTO_add_option( plint , "Range"  , "Range" , FALSE ) ;
   PLUTO_add_number( plint , "Bottom" , -99999,99999, 1, 0,1 ) ;
   PLUTO_add_number( plint , "Top"    , -99999,99999,-1, 0,1 ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * HISTO_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;
   THD_3dim_dataset * input_dset , * mask_dset=NULL ;
   int iv , mcount , nvox , ii,jj , nbin ;
   float mask_bot=666.0 , mask_top=-666.0 , hbot,htop ;
   char * tag , * str , buf[THD_MAX_NAME+16] ;
   byte * mmm ;
   MRI_IMAGE * flim ;
   float     * flar ;
   int       * hbin ;

   char * cname=NULL ;  /* 06 Aug 1998 */
   int miv=0 ;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "***********************\n"
             "HISTO_main:  NULL input\n"
             "***********************"  ;

   /*-- read 1st line --*/

   PLUTO_next_option(plint) ;
   idc        = PLUTO_get_idcode(plint) ;
   input_dset = PLUTO_find_dset(idc) ;
   if( input_dset == NULL )
      return "******************************\n"
             "HISTO_main:  bad input dataset\n"
             "******************************"  ;

   iv = (int) PLUTO_get_number(plint) ;
   if( iv >= DSET_NVALS(input_dset) || iv < 0 )
      return "********************************\n"
             "HISTO_main:  bad input sub-brick\n"
             "********************************" ;

   DSET_load(input_dset) ;
   if( DSET_ARRAY(input_dset,iv) == NULL )
      return "*******************************\n"
             "HISTO_main:  can't load dataset\n"
             "*******************************"  ;
   nvox = DSET_NVOX(input_dset) ;

   /*-- read optional lines --*/

   while( (tag=PLUTO_get_optiontag(plint)) != NULL ){

      /*-- Mask range of values --*/

      if( strcmp(tag,"Range") == 0 ){
         if( mask_dset == NULL )
            return "*****************************************\n"
                   "HISTO_main:  Can't use Range without Mask\n"
                   "*****************************************"  ;

         mask_bot = PLUTO_get_number(plint) ;
         mask_top = PLUTO_get_number(plint) ;
         continue ;
      }

      /*-- Mask itself --*/

      if( strcmp(tag,"Mask") == 0 ){

         idc       = PLUTO_get_idcode(plint) ;
         mask_dset = PLUTO_find_dset(idc) ;

         if( mask_dset == NULL )
            return "*****************************\n"
                   "HISTO_main:  bad mask dataset\n"
                   "*****************************"  ;

         if( DSET_NVOX(mask_dset) != nvox )
           return "***********************************************************\n"
                  "HISTO_main: mask input dataset doesn't match source dataset\n"
                  "***********************************************************" ;

         miv = (int) PLUTO_get_number(plint) ;  /* 06 Aug 1998 */
         if( miv >= DSET_NVALS(mask_dset) || miv < 0 )
            return "***************************************************\n"
                   "HISTO_main: mask dataset sub-brick index is illegal\n"
                   "***************************************************"  ;

         DSET_load(mask_dset) ;
         if( DSET_ARRAY(mask_dset,miv) == NULL )
            return "************************************\n"
                   "HISTO_main:  can't load mask dataset\n"
                   "************************************"  ;
         continue ;
      }
   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- build a byte mask array --*/

   mmm = (byte *) calloc( sizeof(byte) * nvox , 1 ) ;
   if( mmm == NULL )
      return " \n*** Can't malloc mask workspace! ***\n" ;

   if( mask_dset == NULL ){
      memset( mmm , 1, nvox ) ;
      mcount = nvox ;
   } else {

      /* separate code for each input data type */

      switch( DSET_BRICK_TYPE(mask_dset,miv) ){
         default:
            free(mmm) ;
            return "*** Can't use mask dataset -- illegal data type! ***" ;

         case MRI_short:{
            short mbot , mtop ;
            short * mar = (short *) DSET_ARRAY(mask_dset,miv) ;
            float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            if( mask_bot <= mask_top ){
               mbot = SHORTIZE(mask_bot/mfac) ;
               mtop = SHORTIZE(mask_top/mfac) ;
            } else {
               mbot = (short) -MRI_TYPE_maxval[MRI_short] ;
               mtop = (short)  MRI_TYPE_maxval[MRI_short] ;
            }
            for( mcount=0,ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii]=1; mcount++; }
         }
         break ;

         case MRI_byte:{
            byte mbot , mtop ;
            byte * mar = (byte *) DSET_ARRAY(mask_dset,miv) ;
            float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            if( mask_bot <= mask_top ){
               mbot = BYTEIZE(mask_bot/mfac) ;
               mtop = BYTEIZE(mask_top/mfac) ;
               if( mtop == 0 ){
                  free(mmm) ;
                  return "*** Illegal mask range for mask dataset of bytes. ***" ;
               }
            } else {
               mbot = 0 ;
               mtop = (byte) MRI_TYPE_maxval[MRI_short] ;
            }
            for( mcount=0,ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii]=1; mcount++; }
         }
         break ;

         case MRI_float:{
            float mbot , mtop ;
            float * mar = (float *) DSET_ARRAY(mask_dset,miv) ;
            float mfac = DSET_BRICK_FACTOR(mask_dset,miv) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            if( mask_bot <= mask_top ){
               mbot = (float) (mask_bot/mfac) ;
               mtop = (float) (mask_top/mfac) ;
            } else {
               mbot = -WAY_BIG ;
               mtop =  WAY_BIG ;
            }
            for( mcount=0,ii=0 ; ii < nvox ; ii++ )
               if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii]=1; mcount++; }
         }
         break ;
      }

      if( !EQUIV_DSETS(mask_dset,input_dset) ) DSET_unload(mask_dset) ;
      if( mcount < 3 ){
         free(mmm) ; return "*** Less than 3 voxels survive the mask! ***" ;
      }
      sprintf(buf," \n%d voxels in the mask\n ",mcount) ;
      PLUTO_popup_transient(plint,buf) ;
   }

   /*-- allocate an array to histogrammatize --*/

   flim = mri_new( mcount , 1 , MRI_float ) ;
   flar = MRI_FLOAT_PTR(flim) ;

   /*-- load values into this array --*/

   switch( DSET_BRICK_TYPE(input_dset,iv) ){
      default:
         free(mmm) ; mri_free(flim) ;
         return "*** Can't use source dataset -- illegal data type! ***" ;

      case MRI_short:{
         short * bar = (short *) DSET_ARRAY(input_dset,iv) ;
         float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         for( ii=jj=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) flar[jj++] = mfac*bar[ii] ;
      }
      break ;

      case MRI_byte:{
         byte * bar = (byte *) DSET_ARRAY(input_dset,iv) ;
         float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         for( ii=jj=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) flar[jj++] = mfac*bar[ii] ;
      }
      break ;

      case MRI_float:{
         float * bar = (float *) DSET_ARRAY(input_dset,iv) ;
         float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         for( ii=jj=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) flar[jj++] = mfac*bar[ii] ;
      }
      break ;
   }

   /*-- set range and size of histogram --*/

   hbot = mri_min(flim) ; htop = mri_max(flim) ;
   if( hbot >= htop ){
      free(mmm) ; mri_free(flim) ;
      return "***********************************\n"
             "Selected voxels have no data range!\n"
             "***********************************"  ;
   }

   switch( DSET_BRICK_TYPE(input_dset,iv) ){
      case MRI_float:
         nbin = (int) sqrt((double)mcount) ;
      break ;

      case MRI_short:
      case MRI_byte:{
         float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
         if( mfac == 0.0 || mfac == 1.0 )
            nbin = (int)( htop - hbot ) ;
         else
            nbin = (int) sqrt((double)mcount) ;
      }
      break ;
   }
   if( nbin < 10 ) nbin = 10 ; else if( nbin > 512 ) nbin = 512 ;

   /*-- actually compute and plot histogram --*/

   hbin = (int *) calloc((nbin+1),sizeof(int)) ;

   mri_histogram( flim , hbot,htop , TRUE , nbin,hbin ) ;
   sprintf(buf,"%s[%d] %d voxels",DSET_FILECODE(input_dset),iv,mcount) ;
   PLUTO_histoplot( nbin,hbot,htop,hbin , NULL , NULL ,  buf ) ;

   /*-- go home to mama --*/

   free(hbin) ; free(mmm) ; mri_free(flim) ; return NULL ;
}
