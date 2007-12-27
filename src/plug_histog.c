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
  Plugin to plot a histogram of a sub-brick.
  [Interface is adapted from plug_maskave.c - RWCox - 30 Sep 1999]
  01 Mar 2001 -- Modified to include Max Count -- RWCox
************************************************************************/

static char * HISTO_main( PLUGIN_interface * ) ;

static PLUGIN_interface * CORREL_init(void) ;

static char helpstring[] =
   " Purpose: Plot a histogram of data from a dataset brick.\n"
   "\n"
   " Source:  Dataset   = data to be processed\n"
   "          Sub-brick = which one to use\n\n"
   " Values:  Bottom    = minimum value from dataset to include\n"
   "          Top       = maximum value from dataset to include\n\n"
   " Bins:    Number    = number of bins to use\n"
   "          Max Count = maximum count per bin\n\n"
   "          Smooth    = +/- bin range to smooth histogram\n"
   " Mask:    Dataset   = masking dataset\n"
   "          Sub-brick = which one to use\n\n"
   " Range:   Bottom    = min value from mask dataset to use\n"
   "          Top       = max value from mask dataset to use\n"
   "          [if Bottom >  Top, then all nonzero mask voxels will be used; ]\n"
   "          [if Bottom <= Top, then only nonzero mask voxels in this range]\n"
   "          [                  will be used in computing the statistics.  ]\n"
   " Aboot:   If activated, then only voxels within a distance of Radius mm\n"
   "          of the current crosshairs will be used in the histogram.\n"
   " Output:  Name of the ascii file to which histogram values are written.\n"
   "\n"
   " Author -- RW Cox - 30 September 1999\n"
   " Output feature added by V Roopchansingh, Feb 2002\n"
;

/***********************************************************************
   Set up the interface to the user
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

#if 0
   if( ncall > 0 ) return NULL ;  /* only one interface */
#else
   if( ncall == 1 ) return CORREL_init() ;
   if( ncall >  1 ) return NULL ;
#endif

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "Histogram" ,
                                "Histogram of Dataset Brick" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , HISTO_main  ) ;

   PLUTO_add_hint( plint , "Histogram of Dataset Brick" ) ;

   PLUTO_set_sequence( plint , "A:afniinfo:dsethistog" ) ;

   PLUTO_set_runlabels( plint , "Plot+Keep" , "Plot+Close" ) ;  /* 04 Nov 2003 */

   /*-- first line of input --*/

   PLUTO_add_option( plint , "Source" , "Source" , TRUE ) ;
   PLUTO_add_dataset(plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;

   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ;

   /*-- second line of input --*/

   PLUTO_add_option( plint , "Values" , "Values" , FALSE ) ;
   PLUTO_add_number( plint , "Bottom" , -99999,99999, 0,  1,1 ) ;
   PLUTO_add_number( plint , "Top"    , -99999,99999, 0, -1,1 ) ;

   /*-- third line of input --*/

   PLUTO_add_option( plint , "Bins" , "Bins" , FALSE ) ;
   PLUTO_add_number( plint , "Number" , 10,1000,0, 100,1 ) ;
   PLUTO_add_number( plint , "Max Count" , 0,999999999,0 , 0,1 ) ;
   PLUTO_add_number( plint , "Smooth"    , 0,99,0 , 0,1 ) ;  /* 03 Dec 2004 */

   /*-- fourth line of input --*/

   PLUTO_add_option( plint , "Mask" , "Mask" , FALSE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ;

   /*-- fifth line of input --*/

   PLUTO_add_option( plint , "Range"  , "Range" , FALSE ) ;
   PLUTO_add_number( plint , "Bottom" , -99999,99999, 0,  1,1 ) ;
   PLUTO_add_number( plint , "Top"    , -99999,99999, 0, -1,1 ) ;

   /*-- sixth line of input [20 Mar 2001] --*/

   PLUTO_add_option( plint , "Aboot" , "Aboot" , FALSE ) ;
   PLUTO_add_number( plint , "Radius" , 2,100,0,10,1 ) ;

   /*-- seventh line of input [05 Feb 2002 - VR] --*/

   PLUTO_add_option( plint , "Output", "Output", FALSE ) ;
   PLUTO_add_string( plint , "Filename", 0 , NULL , 20 ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

static char * HISTO_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;
   THD_3dim_dataset * input_dset , * mask_dset=NULL ;
   int iv , mcount , nvox , ii,jj , nbin=-1 , do_mval=0,mval ;
   float mask_bot=666.0 , mask_top=-666.0 , hbot,htop ;
   float val_bot=666.0  , val_top=-666.0 ;
   char * tag , * str , buf[THD_MAX_NAME+16] ;
   byte * mmm ;
   MRI_IMAGE * flim ;
   float     * flar ;
   int       * hbin ;
   int smooth=0 ;      /* 03 Dec 2004 */

   int miv=0 ;

   int maxcount=0 ; /* 01 Mar 2001 */
   float hrad=0.0 ; /* 20 Mar 2001 */

   char * histout=NULL ; /* 05 Feb 2002 - VR */
   FILE * HISTOUT=NULL ; /* 05 Feb 2002 - VR */
   int writehist=0     ; /* 05 Feb 2002 - VR */
   float dx            ; /* 05 Feb 2002 - VR */

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

      /*-- Dataset range of values --*/

      if( strcmp(tag,"Values") == 0 ){
         val_bot = PLUTO_get_number(plint) ;
         val_top = PLUTO_get_number(plint) ;
         do_mval = (val_bot < val_top) ;
         continue ;
      }

      /*-- Number of bins --*/

      if( strcmp(tag,"Bins") == 0 ){
         nbin     = PLUTO_get_number(plint) ;
         maxcount = PLUTO_get_number(plint) ;
         smooth   = PLUTO_get_number(plint) ;  /* 03 Dec 2004 */
         continue ;
      }

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

      /*-- 20 Mar 2001: Aboot --*/

      if( strcmp(tag,"Aboot") == 0 ){
         hrad = PLUTO_get_number(plint) ;
         continue ;
      }

      /*-- 05 Feb 2002: Output - VR --*/

      if( strcmp(tag,"Output") == 0 ){
         histout = PLUTO_get_string(plint) ;
	 writehist = 1 ;
         continue ;
      }
   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- build a byte mask array --*/

   if( mask_dset == NULL ){
      mmm = (byte *) malloc( sizeof(byte) * nvox ) ;
      if( mmm == NULL )
         return " \n*** Can't malloc workspace! ***\n" ;
      memset( mmm , 1, nvox ) ; mcount = nvox ;
   } else {

      mmm = THD_makemask( mask_dset , miv , mask_bot , mask_top ) ;
      if( mmm == NULL )
         return " \n*** Can't make mask for some reason! ***\n" ;
      mcount = THD_countmask( nvox , mmm ) ;

      if( !EQUIV_DSETS(mask_dset,input_dset) ) DSET_unload(mask_dset) ;
      if( mcount < 3 ){
         free(mmm) ;
         return " \n*** Less than 3 voxels survive the mask! ***\n" ;
      }
      sprintf(buf," \n"
                  " %d voxels in the mask\n"
                  " out of %d dataset voxels\n ",mcount,nvox) ;
      PLUTO_popup_transient(plint,buf) ;
   }

   /*-- 20 Mar 2001: modify mask via Aboot Radius, if present --*/

   if( hrad > 0.0 ){
      MCW_cluster * cl ;
      short *di,*dj,*dk ;
      int nd , xx,yy,zz , dd , nx,ny,nz,nxy, nx1,ny1,nz1 , ip,jp,kp ;

      cl = MCW_build_mask( fabs(DSET_DX(input_dset)) ,
                           fabs(DSET_DY(input_dset)) ,
                           fabs(DSET_DZ(input_dset)) , hrad ) ;

      if( cl == NULL || cl->num_pt < 6 ){
         KILL_CLUSTER(cl);
         PLUTO_popup_transient(plint, " \n"
                                      " Aboot Radius too small\n"
                                      " for this dataset!\n"     ) ;
      } else {
         ADDTO_CLUSTER(cl,0,0,0,0) ;
         di = cl->i ; dj = cl->j ; dk = cl->k ; nd = cl->num_pt ;
         nx = DSET_NX(input_dset) ; nx1 = nx-1 ;
         ny = DSET_NY(input_dset) ; ny1 = ny-1 ; nxy  = nx*ny ;
         nz = DSET_NZ(input_dset) ; nz1 = nz-1 ;
         xx = plint->im3d->vinfo->i1 ;
         yy = plint->im3d->vinfo->j2 ;
         zz = plint->im3d->vinfo->k3 ;
         for( dd=0 ; dd < nd ; dd++ ){
            ip = xx+di[dd] ; if( ip < 0 || ip > nx1 ) continue ;
            jp = yy+dj[dd] ; if( jp < 0 || jp > ny1 ) continue ;
            kp = zz+dk[dd] ; if( kp < 0 || kp > nz1 ) continue ;
            mmm[ip+jp*nx+kp*nxy]++ ;
         }
         KILL_CLUSTER(cl) ;
         for( dd=0 ; dd < nvox ; dd++ ) if( mmm[dd] == 1 ) mmm[dd] = 0 ;
         mcount = THD_countmask( nvox , mmm ) ;

         if( mcount < 3 ){
            free(mmm) ;
            return " \n*** Less than 3 voxels survive the mask+radius! ***\n" ;
         }
         sprintf(buf," \n"
                     " %d voxels in the mask+radius\n"
                     " out of %d dataset voxels\n ",mcount,nvox) ;
         PLUTO_popup_transient(plint,buf) ;
      }
   }

   /*-- check for text output of histogram - 05 Feb 2002 - VR --*/

   if ( writehist )
   {
      static char hbuf[1024] ;
      if ( ( histout == NULL ) || ( strlen (histout) == 0 ) ){
         sprintf( hbuf , "%s.histog" , DSET_PREFIX(input_dset) ) ;
      } else {
         strcpy( hbuf , histout ) ;
         if( strstr(hbuf,".hist") == NULL ) strcat( hbuf , ".histog" ) ;
      }
      histout = hbuf ;

      if (THD_is_file(histout))
      {
         free(mmm) ;

         return "*******************************\n"
                "Outfile exists, won't overwrite\n"
                "*******************************\n" ;
      }
      else {
         HISTOUT = fopen (histout, "w") ;
         if( HISTOUT == NULL ){
            free(mmm) ;
            return "**********************************\n"
                   "Can't open Outfile for some reason\n"
                   "**********************************\n" ;
         }
      }
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
         if( do_mval ){
            float val ;
            for( ii=jj=0 ; ii < nvox ; ii++ ){
               if( mmm[ii] ){
                  val = mfac*bar[ii] ;
                  if( val >= val_bot && val <= val_top ) flar[jj++] = val ;
               }
            }
            mval = jj ;
         } else {
            for( ii=jj=0 ; ii < nvox ; ii++ )
               if( mmm[ii] ) flar[jj++] = mfac*bar[ii] ;
         }
      }
      break ;

      case MRI_byte:{
         byte * bar = (byte *) DSET_ARRAY(input_dset,iv) ;
         float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( do_mval ){
            float val ;
            for( ii=jj=0 ; ii < nvox ; ii++ ){
               if( mmm[ii] ){
                  val = mfac*bar[ii] ;
                  if( val >= val_bot && val <= val_top ) flar[jj++] = val ;
               }
            }
            mval = jj ;
         } else {
            for( ii=jj=0 ; ii < nvox ; ii++ )
               if( mmm[ii] ) flar[jj++] = mfac*bar[ii] ;
         }
      }
      break ;

      case MRI_float:{
         float * bar = (float *) DSET_ARRAY(input_dset,iv) ;
         float mfac = DSET_BRICK_FACTOR(input_dset,iv) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         if( do_mval ){
            float val ;
            for( ii=jj=0 ; ii < nvox ; ii++ ){
               if( mmm[ii] ){
                  val = mfac*bar[ii] ;
                  if( val >= val_bot && val <= val_top ) flar[jj++] = val ;
               }
            }
            mval = jj ;
         } else {
            for( ii=jj=0 ; ii < nvox ; ii++ )
               if( mmm[ii] ) flar[jj++] = mfac*bar[ii] ;
         }
      }
      break ;
   }

   if( do_mval ){
      if( mval == 0 ){
         free(mmm) ; mri_free(flim) ;
         return "*** Can't use source dataset -- no data in Values range ***" ;
      }
      flim->nx = flim->nvox = mcount = mval ;
   }

   /*-- set range and size of histogram --*/

   if( val_bot > val_top ){
      hbot = mri_min(flim) ; htop = mri_max(flim) ;
      if( hbot >= htop ){
         free(mmm) ; mri_free(flim) ;
         return "***********************************\n"
                "Selected voxels have no data range!\n"
                "***********************************"  ;
      }
   } else {
      hbot = val_bot ; htop = val_top ;
   }

   if( nbin < 10 || nbin > 1000 ){
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
      if( nbin < 10 ) nbin = 10 ; else if( nbin > 1000 ) nbin = 1000 ;
   }

   /*-- actually compute and plot histogram --*/

   hbin = (int *) calloc((nbin+1),sizeof(int)) ;

   mri_histogram( flim , hbot,htop , TRUE , nbin,hbin ) ;

   if( smooth > 0 ){  /* 03 Dec 2004 */
     int nwid=smooth , *gbin=(int *)calloc((nbin+1),sizeof(int)) , ibot,itop ;
     float ws,wss , *wt ;

     ws = 0.0 ;
     wt = (float *)malloc(sizeof(float)*(2*nwid+1)) ;
     for( ii=0 ; ii <= 2*nwid ; ii++ ){
       wt[ii] = nwid-abs(nwid-ii) + 0.5f ;
       ws += wt[ii] ;
     }
     for( ii=0 ; ii <= 2*nwid ; ii++ ) wt[ii] /= ws ;

     for( jj=0 ; jj <= nbin ; jj++ ){
       ibot = jj-nwid ; if( ibot < 0    ) ibot = 0 ;
       itop = jj+nwid ; if( itop > nbin ) itop = nbin ;
       ws = wss = 0.0 ;
       for( ii=ibot ; ii <= itop ; ii++ ){
         ws += wt[nwid-jj+ii] * hbin[ii] ; wss += wt[nwid-jj+ii] ;
       }
       gbin[jj] = rint(ws/wss) ;
     }
     memcpy(hbin,gbin,sizeof(int)*(nbin+1)) ;
     free((void *)wt) ; free((void *)gbin) ;
   }

   if( maxcount > 0 ){
     for( ii=0 ; ii <= nbin ; ii++ ) hbin[ii] = MIN( hbin[ii] , maxcount ) ;
   }
   hrad = AFNI_numenv("AFNI_1DPLOT_THIK") ;
   if( hrad <= 0.0f || hrad >= 0.02f ) hrad = 0.005f ;
   plot_ts_setthik(hrad) ;
   plot_ts_xypush(0,-1) ;
   sprintf(buf,"\\noesc %s[%d] %d voxels",DSET_FILECODE(input_dset),iv,mcount);
   PLUTO_histoplot( nbin,hbot,htop,hbin , NULL , NULL ,  buf , 0,NULL ) ;

   /*-- 05 Feb 2002: Output - VR --*/

   if ( HISTOUT != NULL )
   {
      if( hbot >= htop ){ hbot = 0.0 ; htop = nbin ;}

      dx = (htop-hbot)/nbin ;

      for( ii=0 ; ii <= nbin ; ii++ )
         fprintf (HISTOUT, "%12.6f %13d \n", hbot+ii*dx, hbin[ii]) ;

      fclose (HISTOUT) ;

      fprintf (stderr, "%s written to disk \n", histout) ;
   }

   /*-- go home to mama --*/

   free(hbin) ; free(mmm) ; mri_free(flim) ; return NULL ;
}

/******************************************************************************/

#define FIT_FISHER
#undef  DO_GREEN

static char c_helpstring[] =
 "Purpose: Plot a histogram of correlation coefficient of a 3D+time\n"
 "         dataset with an input time series file.\n"
 "\n"
 "Source: Dataset   = 3D+time dataset to use\n"
 "        Ignore    = number of points at beginning to ignore\n"
 "\n"
 "Vector: 1D File   = time series to correlate the dataset with\n"
 "        Polort    = degree of polynomial detrending to use\n"
 "\n"
 "Mask:   Dataset   = optional mask dataset to use, to restrict the voxels\n"
 "                    which will be processed\n"
 "        Sub-brick = which sub-brick of the mask dataset to use\n"
 "\n"
 "Range:  Bottom    = minimum value of mask dataset to use\n"
 "        Top       = maximum value of mask dataset to use\n"
 "                    [default is to use all nonzero values in the mask]\n"
 "\n"
 "All selected voxel time series from the source are correlated with the\n"
 "input 1D vector, using the same algorithms as the AFNI internal FIM.\n"
 "The array of correlation coefficients is then put into 100 bins, ranging\n"
 "from -1.0 to 1.0, and the histogram graph is popped up to the display.\n"
 "\n"
 "Overlaid on the histogram are other graphs:\n"
 " * The first [red] is a normal fit to the Fisher z-transform of the\n"
 "     correlation coefficient.\n"
#ifdef DO_GREEN
 " * The second [green] is the nominal fit to the number of degrees of\n"
 "     freedom, assuming the data time series are normal white noise.\n"
#endif
 "\n"
 "-- Bob Cox - October 1999\n"
;

static char * CORREL_main( PLUGIN_interface * ) ;

static PLUGIN_interface * CORREL_init(void)
{
   PLUGIN_interface * plint ;

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "Histogram: CC" ,
                                "Histogram of Correlation Coefficient" ,
                                c_helpstring ,
                                PLUGIN_CALL_VIA_MENU , CORREL_main  ) ;

   PLUTO_add_hint( plint , "Histogram: Correlation Coefficient" ) ;

   PLUTO_set_sequence( plint , "A:afniinfo:dset" ) ;

   /*-- first line of input --*/

   PLUTO_add_option( plint , "Source" , "Source" , TRUE ) ;

   PLUTO_add_dataset(  plint ,
                       "Dataset" ,        /* label next to button   */
                       ANAT_ALL_MASK ,    /* take any anat datasets */
                       FUNC_FIM_MASK ,    /* only allow fim funcs   */
                       DIMEN_4D_MASK |    /* need 3D+time datasets  */
                       BRICK_ALLREAL_MASK /* need real-valued datasets */
                    ) ;

   PLUTO_add_number( plint ,
                     "Ignore" ,   /* label next to chooser */
                     0 ,          /* smallest possible value */
                     999 ,        /* largest possible value */
                     0 ,          /* decimal shift (none in this case) */
                     4 ,          /* default value */
                     TRUE         /* allow user to edit value? */
                   ) ;

   /*-- second line of input --*/

   PLUTO_add_option( plint , "Vector" , "Vector" , TRUE ) ;

   PLUTO_add_timeseries( plint , "1D File" ) ;

   PLUTO_add_number( plint , "Polort" , 0,MAX_POLORT,0,1,FALSE ) ;

   /*-- (optional) third line of input --*/

   PLUTO_add_option( plint , "Mask" , "Mask" , FALSE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ;

   /*-- (optional) fourth line of input --*/

   PLUTO_add_option( plint , "Range"  , "Range" , FALSE ) ;
   PLUTO_add_number( plint , "Bottom" , -99999,99999, 1, 0,1 ) ;
   PLUTO_add_number( plint , "Top"    , -99999,99999,-1, 0,1 ) ;


   return plint ;
}

/*-----------------------------------------------------------------------------*/

#include "uuu.c"

static char *  CORREL_main( PLUGIN_interface * plint )
{
   MCW_idcode * idc ;
   THD_3dim_dataset * input_dset , * mask_dset = NULL ;
   MRI_IMAGE * tsim , * flim ;
   int ignore , nvox , ntim , polort , miv , it , ip , nupdt , nbin ;
   int mcount , ii , jj ;
   float * tsar ;
   float mask_bot=666.0 , mask_top=-666.0 , hbot,htop ;
   byte * mmm ;
   char buf[THD_MAX_NAME+16] , * tag ;

   PCOR_references * pc_ref ;
   PCOR_voxel_corr * pc_vc ;
   int fim_nref ;
   float * ref_vec , * vval , * zval , * aval ;
   int   * hbin , * jbin , * kbin , *jist[2] ;
   float sum , sumq , dbin , gval,rval,gg , sqp , zmid,zmed,zsig ;
   float pstar,zstar,zplus,zminus,psum,msum ;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "************************\n"
             "CORREL_main:  NULL input\n"
             "************************"  ;

   /*-- read 1st line --*/

   PLUTO_next_option(plint) ;
   idc        = PLUTO_get_idcode(plint) ;
   input_dset = PLUTO_find_dset(idc) ;
   if( input_dset == NULL )
      return "*******************************\n"
             "CORREL_main:  bad input dataset\n"
             "*******************************"  ;

   nvox = DSET_NVOX(input_dset) ;
   ntim = DSET_NUM_TIMES(input_dset) ;

   ignore = (int) PLUTO_get_number(plint) ;
   if( ignore >= ntim-5 || ignore < 0 )
      return "******************************\n"
             "CORREL_main:  bad ignore count\n"
             "******************************" ;

   DSET_load(input_dset) ;
   if( DSET_ARRAY(input_dset,0) == NULL )
      return "********************************\n"
             "CORREL_main:  can't load dataset\n"
             "********************************"  ;

   /*-- read 2nd line --*/

   PLUTO_next_option(plint) ;
   tsim = PLUTO_get_timeseries(plint) ;
   if( tsim == NULL || tsim->nx < ntim )
      return "*****************************\n"
             "CORREL_main: bad input vector\n"
             "*****************************"  ;

   flim = mri_to_float(tsim) ; tsar = MRI_FLOAT_PTR(flim) ;

   polort = (int) PLUTO_get_number(plint) ;

   /*-- read optional lines --*/

   while( (tag=PLUTO_get_optiontag(plint)) != NULL ){

      /*-- Mask range of values --*/

      if( strcmp(tag,"Range") == 0 ){
         if( mask_dset == NULL ){
            mri_free(flim) ;
            return "******************************************\n"
                   "CORREL_main:  Can't use Range without Mask\n"
                   "******************************************"  ;
         }

         mask_bot = PLUTO_get_number(plint) ;
         mask_top = PLUTO_get_number(plint) ;
         continue ;
      }

      /*-- Mask itself --*/

      if( strcmp(tag,"Mask") == 0 ){

         idc       = PLUTO_get_idcode(plint) ;
         mask_dset = PLUTO_find_dset(idc) ;

         if( mask_dset == NULL ){
            mri_free(flim) ;
            return "******************************\n"
                   "CORREL_main:  bad mask dataset\n"
                   "******************************"  ;
         }

         if( DSET_NVOX(mask_dset) != nvox ){
           mri_free(flim) ;
           return "************************************************************\n"
                  "CORREL_main: mask input dataset doesn't match source dataset\n"
                  "************************************************************" ;
         }

         miv = (int) PLUTO_get_number(plint) ;
         if( miv >= DSET_NVALS(mask_dset) || miv < 0 ){
            mri_free(flim) ;
            return "****************************************************\n"
                   "CORREL_main: mask dataset sub-brick index is illegal\n"
                   "****************************************************"  ;
         }

         DSET_load(mask_dset) ;
         if( DSET_ARRAY(mask_dset,miv) == NULL ){
            mri_free(flim) ;
            return "*************************************\n"
                   "CORREL_main:  can't load mask dataset\n"
                   "*************************************"  ;
         }
         continue ;
      }
   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- build a byte mask array --*/

   if( mask_dset == NULL ){
      mmm = (byte *) malloc( sizeof(byte) * nvox ) ;
      if( mmm == NULL )
         return " \n*** Can't malloc workspace! ***\n" ;
      memset( mmm , 1, nvox ) ; mcount = nvox ;
   } else {

      mmm = THD_makemask( mask_dset , miv , mask_bot , mask_top ) ;
      if( mmm == NULL )
         return " \n*** Can't make mask for some reason! ***\n" ;
      mcount = THD_countmask( nvox , mmm ) ;

      if( !EQUIV_DSETS(mask_dset,input_dset) ) DSET_unload(mask_dset) ;
      if( mcount < 3 ){
         free(mmm) ;
         return " \n*** Less than 3 voxels survive the mask! ***\n" ;
      }
      sprintf(buf," \n"
                  " %d voxels in the mask\n"
                  " out of %d dataset voxels\n ",mcount,nvox) ;
      PLUTO_popup_transient(plint,buf) ;
   }

   /*-- setup to do the FIM calculation --*/

   fim_nref = polort+2 ;
   ref_vec = (float *) malloc( sizeof(float) * fim_nref ) ;
   vval    = (float *) malloc( sizeof(float) * mcount ) ;

   pc_ref = new_PCOR_references( fim_nref ) ;
   pc_vc  = new_PCOR_voxel_corr( mcount , fim_nref ) ;

   /*-- do FIM --*/

   for( nupdt=0,it=ignore ; it < ntim ; it++ ){
      if( tsar[it] >= WAY_BIG ) continue ;           /* skip this */

      ref_vec[0] = 1.0 ;
      for( ip=1 ; ip <= polort ; ip++ )
         ref_vec[ip] = ref_vec[ip-1] * ((float)it) ;

      ref_vec[ip] = tsar[it] ; /* vector value */

      update_PCOR_references( ref_vec , pc_ref ) ;

      /*-- load values into vval --*/

      switch( DSET_BRICK_TYPE(input_dset,it) ){

         case MRI_short:{
            short * bar = (short *) DSET_ARRAY(input_dset,it) ;
            float mfac = DSET_BRICK_FACTOR(input_dset,it) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            for( ii=jj=0 ; ii < nvox ; ii++ )
               if( mmm[ii] ) vval[jj++] = mfac*bar[ii] ;
         }
         break ;

         case MRI_byte:{
            byte * bar = (byte *) DSET_ARRAY(input_dset,it) ;
            float mfac = DSET_BRICK_FACTOR(input_dset,it) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
            for( ii=jj=0 ; ii < nvox ; ii++ )
               if( mmm[ii] ) vval[jj++] = mfac*bar[ii] ;
         }
         break ;

         case MRI_float:{
            float * bar = (float *) DSET_ARRAY(input_dset,it) ;
            float mfac = DSET_BRICK_FACTOR(input_dset,it) ;
            if( mfac == 0.0 ) mfac = 1.0 ;
               for( ii=jj=0 ; ii < nvox ; ii++ )
                  if( mmm[ii] ) vval[jj++] = mfac*bar[ii] ;
         }
         break ;
      }

      PCOR_update_float( vval , pc_ref , pc_vc ) ;
      nupdt++ ;
   }

   free(ref_vec) ; mri_free(flim) ; free(mmm) ;

   /*-- get correlation coefficient --*/

   PCOR_get_pcor( pc_ref , pc_vc , vval ) ;

   free_PCOR_references(pc_ref) ; free_PCOR_voxel_corr(pc_vc) ;

   /*-- compute statistics --*/

   sum = 0.0 ;
   for( ii=0 ; ii < mcount ; ii++ ) sum += vval[ii] ;
   sum /= mcount ; sumq = 0.0 ;
   for( ii=0 ; ii < mcount ; ii++ )
      sumq += (vval[ii]-sum)*(vval[ii]-sum) ;
   sumq = sqrt(sumq/mcount) ;

   /*-- get robust statistics of Fisher z-transform --*/

   zval = (float *) malloc( sizeof(float) * mcount ) ;
   aval = (float *) malloc( sizeof(float) * mcount ) ;
   for( ii=0 ; ii < mcount ; ii++ ) zval[ii] = atanh(vval[ii]) ;
   qsort_float( mcount , zval ) ;
   if( mcount%2 == 1 )              /* median */
      zmid = zval[mcount/2] ;
   else
      zmid = 0.5 * ( zval[mcount/2] + zval[mcount/2-1] ) ;

   for( ii=0 ; ii < mcount ; ii++ ) aval[ii] = fabs(zval[ii]-zmid) ;
   qsort_float( mcount , aval ) ;
   if( mcount%2 == 1 )              /* MAD = median absolute deviation */
      zmed = aval[mcount/2] ;
   else
      zmed = 0.5 * ( aval[mcount/2] + aval[mcount/2-1] ) ;
   zsig = 1.4826 * zmed ;           /* estimate st.dev. */
                                    /* 1/1.4826 = sqrt(2)*erfinv(0.5) */
   free(aval) ;
   free(zval) ;

   /*-- do histogram --*/

   hbot = -1.0 ; htop = 1.0 ; nbin = 100 ; dbin = (htop-hbot)/nbin ;

   hbin = (int *) calloc((nbin+1),sizeof(int)) ;
   jbin = (int *) calloc((nbin+1),sizeof(int)) ;  /* 04 Oct 1999 */

#ifndef FIT_FISHER
   sqp = 1.0/(sqrt(2.0*PI)*sumq) ;                /* Gaussian fit */
   for( ii=0 ; ii < nbin ; ii++ ){                /* to rho data  */
      gval = hbot + (ii+0.5)*dbin - sum ;
      gval = sqp * exp( -0.5*gval*gval/(sumq*sumq) ) ;
      jbin[ii] = (int)( mcount * dbin * gval + 0.5 ) ;
   }
#else
   sqp = 1.0/(sqrt(2.0*PI)*zsig) ;                /* Gaussian fit */
   for( ii=0 ; ii < nbin ; ii++ ){                /* to z(rho)    */
      rval = hbot + (ii+0.5)*dbin ;
      gval = atanh(rval) - zmid ;
      gval = sqp * exp( -0.5*gval*gval/(zsig*zsig) )/sqrt(1.0-rval*rval) ;
      jbin[ii] = (int)( mcount * dbin * gval + 0.5 ) ;
   }
   sum  = tanh(zmid) ;
   sumq = 0.5*( tanh(zmid+zmed) - tanh(zmid-zmed) ) ;
#endif

   kbin = (int *) calloc((nbin+1),sizeof(int)) ;
   for( ii=0 ; ii < nbin ; ii++ ){
      gval = correl_t2p( fabs(hbot+ii*dbin) ,
                         (double)nupdt , (double)1 , (double)(polort+1) ) ;
      sqp = correl_t2p( fabs(hbot+(ii+1)*dbin) ,
                        (double)nupdt , (double)1 , (double)(polort+1) ) ;
      kbin[ii] = (int)( 0.5*mcount*fabs(gval-sqp) ) ;
   }
   jist[0] = jbin ; jist[1] = kbin ;

   flim = mri_new_vol_empty( mcount,1,1 , MRI_float ) ;
   mri_fix_data_pointer( vval , flim ) ;
   mri_histogram( flim , hbot,htop , TRUE , nbin,hbin ) ;

   { char * ps = my_getenv("PTAIL") ;
     float pp=0.0 ;
     if( ps != NULL ) pp = strtod(ps,NULL) ;
     set_unusuality_tail(pp) ;
   }

   for( ii=0 ; ii < mcount ; ii++ ) vval[ii] = -vval[ii] ;
   msum = unusuality( mcount , vval ) ;
   for( ii=0 ; ii < mcount ; ii++ ) vval[ii] = -vval[ii] ;
   psum = unusuality( mcount , vval ) ;

   sprintf(buf,"%s^.%s:\\rho_{mid}=%.2f\\pm%.2f,u=%.1f,%.1f",
               DSET_FILECODE(input_dset),
               (tsim->name != NULL) ? THD_trailname(tsim->name,0) : " " ,
               sum,sumq , psum,msum ) ;
   plot_ts_xypush(0,-1) ;
#ifdef DO_GREEN
   PLUTO_histoplot( nbin,hbot,htop,hbin ,
                    "Correlation Coefficient",NULL,buf , 2,jist ) ;
#else
   PLUTO_histoplot( nbin,hbot,htop,hbin ,
                    "Correlation Coefficient",NULL,buf , 1,jist ) ;
#endif

   mri_clear_data_pointer(flim) ; mri_free(flim) ;
   free(vval) ; free(hbin) ; free(jbin) ; free(kbin) ;
   return NULL ;
}
