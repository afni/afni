#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***********************************************************************
  Plugin to compute statistics over ROI.
************************************************************************/

char * MASKAVE_main( PLUGIN_interface * ) ;

static char helpstring[] =
   " Purpose: Compute statistics over a mask-selected ROI.\n"
   "\n"
   " Source: Dataset   = data to be averaged\n"
   "         Sub-brick = which one to use\n"
   "                     [if -1 is input, will do all sub-bricks]\n"
   " Mask:   Dataset   = masking dataset\n"
   " Range:  Bottom    = min value from mask dataset to use\n"
   "         Top       = max value from mask dataset to use\n"
   "         [if Bottom >  Top, then all nonzero mask voxels will be used;]\n"
   "         [if Bottom <= Top, then only nonzero mask voxel in this range]\n"
   "         [                  will be used in computing the statistics. ]\n"
   "\n"
   " Author -- RW Cox"
;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "ROI Average" , "Average Dataset over ROI" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , MASKAVE_main  ) ;

   PLUTO_add_hint( plint , "Average Dataset over ROI" ) ;

   /*-- first line of input --*/

   PLUTO_add_option( plint , "Source" , "Source" , TRUE ) ;
   PLUTO_add_dataset(plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;

   PLUTO_add_number( plint , "Sub-brick" , -1,9999,0 , 0,1 ) ;

   /*-- second line of input --*/

   PLUTO_add_option( plint , "Mask" , "Mask" , TRUE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_3D_MASK | BRICK_ALLREAL_MASK ) ;

   /*-- third line of input --*/

   PLUTO_add_option( plint , "Range"  , "Range" , FALSE ) ;
   PLUTO_add_number( plint , "Bottom" , -99999,99999, 1, 0,1 ) ;
   PLUTO_add_number( plint , "Top"    , -99999,99999,-1, 0,1 ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * MASKAVE_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;
   THD_3dim_dataset * input_dset , * mask_dset ;
   int iv , mcount , nvox , ii , sigmait , nvals , doall , ivbot,ivtop ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   double sum=0.0 , sigma=0.0 ;
   float * sumar=NULL , * sigmar=NULL ;
   char * tag , * str , buf[64] , abuf[32],sbuf[32] ;
   byte * mmm ;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "*************************\n"
             "MASKAVE_main:  NULL input\n"
             "*************************"  ;

   /*-- read 1st line --*/

   PLUTO_next_option(plint) ;
   idc        = PLUTO_get_idcode(plint) ;
   input_dset = PLUTO_find_dset(idc) ;
   if( input_dset == NULL )
      return "********************************\n"
             "MASKAVE_main:  bad input dataset\n"
             "********************************"  ;

   iv = (int) PLUTO_get_number(plint) ;
   if( iv >= DSET_NVALS(input_dset) )
      return "**********************************\n"
             "MASKAVE_main:  bad input sub-brick\n"
             "**********************************" ;
   doall = (iv < 0) ;
   if( doall ){
      nvals  = DSET_NVALS(input_dset) ;
      ivbot  = 0 ; ivtop = nvals-1 ;
   } else {
      ivbot = ivtop = iv ;
   }
   DSET_load(input_dset) ;
   if( DSET_ARRAY(input_dset,ivbot) == NULL )
      return "*********************************\n"
             "MASKAVE_main:  can't load dataset\n"
             "*********************************"  ;
   nvox = DSET_NVOX(input_dset) ;

   /*-- read 2nd line --*/

   PLUTO_next_option(plint) ;
   idc       = PLUTO_get_idcode(plint) ;
   mask_dset = PLUTO_find_dset(idc) ;
   if( mask_dset == NULL )
      return "*******************************\n"
             "MASKAVE_main:  bad mask dataset\n"
             "*******************************"  ;
   if( DSET_NVOX(mask_dset) != nvox )
      return "*************************************************************\n"
             "MASKAVE_main: mask input dataset doesn't match source dataset\n"
             "*************************************************************"  ;
   DSET_load(mask_dset) ;
   if( DSET_ARRAY(mask_dset,0) == NULL )
      return "**************************************\n"
             "MASKAVE_main:  can't load mask dataset\n"
             "**************************************"  ;

   /*-- read optional lines --*/

   tag = PLUTO_get_optiontag(plint) ;
   while( tag != NULL ){

      if( strcmp(tag,"Range") == 0 ){
         mask_bot = PLUTO_get_number(plint) ;
         mask_top = PLUTO_get_number(plint) ;
      }

      tag = PLUTO_get_optiontag(plint) ;
   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- build a byte mask array --*/

   mmm = (byte *) malloc( sizeof(byte) * nvox ) ;
   if( mmm == NULL )
      return "*** Can't malloc workspace! ***" ;

   /* separate code for each input data type */

   switch( DSET_BRICK_TYPE(mask_dset,0) ){
      default:
         free(mmm) ;
         return "*** Can't use mask dataset -- illegal data type! ***" ;

      case MRI_short:{
         short mbot , mtop ;
         short * mar = (short *) DSET_ARRAY(mask_dset,0) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,0) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top ){
            mbot = (short) (mask_bot/mfac) ;
            mtop = (short) (mask_top/mfac) ;
         } else {
            mbot = (short) -MRI_TYPE_maxval[MRI_short] ;
            mtop = (short)  MRI_TYPE_maxval[MRI_short] ;
         }
         for( mcount=0,ii=0 ; ii < nvox ; ii++ )
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii] = 1 ; mcount++ ; }
            else                                                    { mmm[ii] = 0 ; }
      }
      break ;

      case MRI_byte:{
         byte mbot , mtop ;
         byte * mar = (byte *) DSET_ARRAY(mask_dset,0) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,0) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top ){
            mbot = (byte) ((mask_bot > 0.0) ? (mask_bot/mfac) : 0.0) ;
            mtop = (byte) ((mask_top > 0.0) ? (mask_top/mfac) : 0.0) ;
            if( mtop == 0 ){
               fprintf(stderr,"*** Illegal mask range for mask dataset of bytes.\n") ; exit(1) ;
            }
         } else {
            mbot = 0 ;
            mtop = (byte) MRI_TYPE_maxval[MRI_short] ;
         }
         for( mcount=0,ii=0 ; ii < nvox ; ii++ )
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii] = 1 ; mcount++ ; }
            else                                                    { mmm[ii] = 0 ; }
      }
      break ;

      case MRI_float:{
         float mbot , mtop ;
         float * mar = (float *) DSET_ARRAY(mask_dset,0) ;
         float mfac = DSET_BRICK_FACTOR(mask_dset,0) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( mask_bot <= mask_top ){
            mbot = (float) (mask_bot/mfac) ;
            mtop = (float) (mask_top/mfac) ;
         } else {
            mbot = -WAY_BIG ;
            mtop =  WAY_BIG ;
         }
         for( mcount=0,ii=0 ; ii < nvox ; ii++ )
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii] = 1 ; mcount++ ; }
            else                                                    { mmm[ii] = 0 ; }
      }
      break ;
   }

   if( mcount == 0 ){
      free(mmm) ;
      return "*** No voxels survive the masking operations! ***" ;
   }
   sigmait = (mcount > 1) ;

   /*-- compute statistics --*/

   if( doall ){
      sumar  = (float *) malloc( sizeof(float) * nvals ) ;
      sigmar = (float *) malloc( sizeof(float) * nvals ) ;
   }

   for( iv=ivbot ; iv <= ivtop ; iv++ ){
      switch( DSET_BRICK_TYPE(input_dset,iv) ){

         default:
            free(mmm) ; if( doall ){ free(sumar) ; free(sigmar) ; }
            return "*** Can't use source dataset -- illegal data type! ***" ;

         case MRI_short:{
            short * bar = (short *) DSET_ARRAY(input_dset,iv) ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 ) mfac = 1.0 ;

            for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sum += bar[ii] ;
            sum = sum / mcount ;

            if( sigmait ){
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mcount-1) ) ;
            } else {
               sigma = 0.0 ;
            }
            sum = mfac * sum ;
         }
         break ;

         case MRI_byte:{
            byte * bar = (byte *) DSET_ARRAY(input_dset,iv) ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 ) mfac = 1.0 ;

            for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sum += bar[ii] ;
            sum = sum / mcount ;

            if( sigmait ){
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mcount-1) ) ;
            } else {
               sigma = 0.0 ;
            }
            sum = mfac * sum ;
         }
         break ;

         case MRI_float:{
            float * bar = (float *) DSET_ARRAY(input_dset,iv) ;
            float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
            if( mfac == 0.0 ) mfac = 1.0 ;

            for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sum += bar[ii] ;
            sum = sum / mcount ;

            if( sigmait ){
               for( ii=0 ; ii < nvox ; ii++ ) if( mmm[ii] ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mcount-1) ) ;
            } else {
               sigma = 0.0 ;
            }
            sum = mfac * sum ;
         }
         break ;
      }

      if( doall ){ sumar[iv] = sum ; sigmar[iv] = sigma ; }
   }

   free(mmm) ;

   /*-- send report --*/

   if( doall ){
      str = (char *) malloc( 1024 + 64*nvals ) ;
      sprintf(str," ****** ROI statistics ****** \n"
                  " Source  = %s [all sub-bricks] \n"
                  " Mask    = %s" ,
              DSET_FILECODE(input_dset) , DSET_FILECODE(mask_dset) ) ;
      if( mask_bot <= mask_top ){
         sprintf(buf," [range %g .. %g]" , mask_bot , mask_top ) ;
         strcat(str,buf) ;
      }
      strcat(str," \n") ;
      sprintf(buf," Count   = %d voxels\n",mcount) ; strcat(str,buf) ;
      for( iv=0 ; iv < nvals ; iv++ ){
         AV_fval_to_char( sumar[iv]  , abuf ) ;
         AV_fval_to_char( sigmar[iv] , sbuf ) ;
         sprintf(buf," Average = %9.9s  Sigma = %9.9s [%s]  \n",
                 abuf,sbuf , DSET_BRICK_LABEL(input_dset,iv) ) ;
         strcat(str,buf) ;
      }
      PLUTO_popup_textwin( plint , str ) ;
      free(str) ; free(sumar) ; free(sigmar) ;

   } else if( mask_bot <= mask_top ){
      str = (char *) malloc( 1024 ) ;
      sprintf( str , " *** ROI Statistics *** \n"
                     " Source  = %s [%s] \n"
                     " Mask    = %s [range %g .. %g] \n"
                     " Count   = %d voxels \n"
                     " Average = %g \n"
                     " Sigma   = %g " ,
               DSET_FILECODE(input_dset) , DSET_BRICK_LABEL(input_dset,ivbot) ,
               DSET_FILECODE(mask_dset)  , mask_bot , mask_top ,
               mcount , sum , sigma ) ;
      PLUTO_popup_message(plint,str) ;
      free(str) ;

   } else {
      str = (char *) malloc( 1024 ) ;
      sprintf( str , " *** ROI Statistics *** \n"
                     " Source  = %s [%s] \n"
                     " Mask    = %s \n"
                     " Count   = %d voxels \n"
                     " Average = %g \n"
                     " Sigma   = %g " ,
               DSET_FILECODE(input_dset) , DSET_BRICK_LABEL(input_dset,ivbot) ,
               DSET_FILECODE(mask_dset)  , mcount , sum , sigma ) ;
      PLUTO_popup_message(plint,str) ;
      free(str) ;
   }

   return NULL ;
}
