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
  Plugin to compute statistics over ROI.
************************************************************************/

char * MASKAVE_main( PLUGIN_interface * ) ;

static char helpstring[] =
   " Purpose: Compute statistics over a mask-selected ROI.\n"
   "\n"
   " Source:  Dataset   = data to be averaged\n"
   "          Sub-brick = which one to use\n"
   "                      [if -1 is input, will do all sub-bricks]\n\n"
   " Mask:    Dataset   = masking dataset\n"
   "          Sub-brick = which one to use\n\n"
   " Range:   Bottom    = min value from mask dataset to use\n"
   "          Top       = max value from mask dataset to use\n"
   "          [if Bottom >  Top, then all nonzero mask voxels will be used; ]\n"
   "          [if Bottom <= Top, then only nonzero mask voxels in this range]\n"
   "          [                  will be used in computing the statistics.  ]\n\n"
   " 1D Save: Name      = If all input sub-bricks are used (i.e., setting\n"
   "                      'Source Sub-brick' = -1 above), then this option\n"
   "                      lets you save the resulting average time series\n"
   "                      into the interal list of timeseries available via\n"
   "                      the 'Choose Timeseries', 'Pick Ideal', ... buttons.\n"
   "          To Disk?  = If 'YES' is chosen, then will also write the\n"
   "                      timeseries to disk in the *.1D format.\n\n"
   " Author -- RW Cox"
;

#define NUM_yesno_list 2
static char *yesno_list[] = { "YES" , "NO" } ;


/***********************************************************************
   Set up the interface to the user
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "ROI Average" , "Average Dataset over ROI" , helpstring ,
                                 PLUGIN_CALL_VIA_MENU , MASKAVE_main  ) ;

   PLUTO_add_hint( plint , "Average Dataset over ROI" ) ;

   PLUTO_set_sequence( plint , "A:afniinfo:dset" ) ;

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
                                 DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ;  /* 06 Aug 1998 */

   /*-- third line of input --*/

   PLUTO_add_option( plint , "Range"  , "Range" , FALSE ) ;
   PLUTO_add_number( plint , "Bottom" , -99999,99999, 1, 0,1 ) ;
   PLUTO_add_number( plint , "Top"    , -99999,99999,-1, 0,1 ) ;

   /*-- 4th line of input (06 Aug 1998) --*/

   PLUTO_add_option( plint , "1D Save" , "1D Save" , FALSE ) ;
   PLUTO_add_string( plint , "Name" , 0,NULL , 12 ) ;
   PLUTO_add_string( plint , "To Disk?" , NUM_yesno_list , yesno_list , 1 ) ;

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
   double sum , sigma ;
   float * sumar=NULL , * sigmar=NULL ;
   char * tag , * str , buf[64] , abuf[32],sbuf[32] ;
   byte * mmm ;

   char * cname=NULL ;  /* 06 Aug 1998 */
   int    cdisk=0 ;     /* 22 Aug 2000 */
   int miv=0 ;

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

   miv = (int) PLUTO_get_number(plint) ;  /* 06 Aug 1998 */
   if( miv >= DSET_NVALS(mask_dset) )
      return "*****************************************************\n"
             "MASKAVE_main: mask dataset sub-brick index is too big\n"
             "*****************************************************"  ;

   DSET_load(mask_dset) ;
   if( DSET_ARRAY(mask_dset,0) == NULL )
      return "**************************************\n"
             "MASKAVE_main:  can't load mask dataset\n"
             "**************************************"  ;

   /*-- read optional lines --*/

   while( (tag=PLUTO_get_optiontag(plint)) != NULL ){

      if( strcmp(tag,"Range") == 0 ){
         mask_bot = PLUTO_get_number(plint) ;
         mask_top = PLUTO_get_number(plint) ;
         continue ;
      }

      if( strcmp(tag,"1D Save") == 0 ){
         char * yn ;
         cname = PLUTO_get_string(plint) ;
         yn    = PLUTO_get_string(plint) ;
         cdisk = (strcmp(yn,yesno_list[0]) == 0) ;
         continue ;
      }

   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- build a byte mask array --*/

   mmm = (byte *) malloc( sizeof(byte) * nvox ) ;
   if( mmm == NULL )
      return "*** Can't malloc workspace! ***" ;

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
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii] = 1 ; mcount++ ; }
            else                                                    { mmm[ii] = 0 ; }
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
            if( mar[ii] >= mbot && mar[ii] <= mtop && mar[ii] != 0 ){ mmm[ii] = 1 ; mcount++ ; }
            else                                                    { mmm[ii] = 0 ; }
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
      sum = sigma = 0.0 ;                         /* 13 Dec 1999 */
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
               for( ii=0 ; ii < nvox ; ii++ )
                  if( mmm[ii] ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mcount-1) ) ;
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
               for( ii=0 ; ii < nvox ; ii++ )
                  if( mmm[ii] ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mcount-1) ) ;
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
               for( ii=0 ; ii < nvox ; ii++ )
                  if( mmm[ii] ) sigma += SQR(bar[ii]-sum) ;
               sigma = mfac * sqrt( sigma/(mcount-1) ) ;
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
                  " Mask    = %s [%s]" ,
              DSET_FILECODE(input_dset) ,
              DSET_FILECODE(mask_dset)  , DSET_BRICK_LABEL(mask_dset,miv) ) ;
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

      /* 06 Aug 1998 */

      if( cname != NULL && cname[0] != '\0' ){
         MRI_IMAGE * qim = mri_new_vol_empty( nvals,1,1 , MRI_float ) ;
         mri_fix_data_pointer( sumar , qim ) ;
         PLUTO_register_timeseries( cname , qim ) ;

         if( cdisk ){                         /* 22 Aug 2000 */
            if( PLUTO_prefix_ok(cname) ){
               char * cn ;
               if( strstr(cname,".1D") == NULL ){
                  cn = malloc(strlen(cname)+8) ;
                  strcpy(cn,cname) ; strcat(cn,".1D") ;
               } else {
                  cn = cname ;
               }
               mri_write_1D( cn , qim ) ;
               if( cn != cname ) free(cn) ;
            } else {
               PLUTO_popup_transient(plint," \n"
                                           "** Illegal filename **\n"
                                           "** in 'To Disk?' !! **\n" ) ;
            }
         }

         mri_fix_data_pointer( NULL , qim ) ; mri_free(qim) ;
      }

      free(str) ; free(sumar) ; free(sigmar) ;

   } else if( mask_bot <= mask_top ){
      str = (char *) malloc( 1024 ) ;
      sprintf( str , " *** ROI Statistics *** \n"
                     " Source  = %s [%s] \n"
                     " Mask    = %s [%s] [range %g .. %g] \n"
                     " Count   = %d voxels \n"
                     " Average = %g \n"
                     " Sigma   = %g " ,
               DSET_FILECODE(input_dset) , DSET_BRICK_LABEL(input_dset,ivbot) ,
               DSET_FILECODE(mask_dset)  , DSET_BRICK_LABEL(mask_dset,miv)    ,
               mask_bot , mask_top , mcount , sum , sigma ) ;
      PLUTO_popup_message(plint,str) ;
      free(str) ;

   } else {
      str = (char *) malloc( 1024 ) ;
      sprintf( str , " *** ROI Statistics *** \n"
                     " Source  = %s [%s] \n"
                     " Mask    = %s [%s] \n"
                     " Count   = %d voxels \n"
                     " Average = %g \n"
                     " Sigma   = %g " ,
               DSET_FILECODE(input_dset) , DSET_BRICK_LABEL(input_dset,ivbot) ,
               DSET_FILECODE(mask_dset)  , DSET_BRICK_LABEL(mask_dset,miv)    ,
               mcount , sum , sigma ) ;
      PLUTO_popup_message(plint,str) ;
      free(str) ;
   }

   return NULL ;
}
