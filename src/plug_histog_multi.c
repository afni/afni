#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***************************************************************************/

#define MAX_DSET 3

static char *MHIST_main( PLUGIN_interface * ) ;

static char helpstring[] =
   " Purpose: Plot histogram of data from dataset brick or bricks.\n"
   "\n"
   " Source#1:  Dataset   = data to be processed\n"
   "            Index Bot = first sub-brick to use\n"
   "            Index Top = last sub-brick to use\n"
   "            Color     = line color to use\n"
   " Source#2:  Optional second (etc.) datasets to histogramize\n\n"
   " Values:  Bottom    = minimum value from dataset to include\n"
   "          Top       = maximum value from dataset to include\n\n"
   " Bins:    Number    = number of bins to use\n"
   " Mask:    Dataset   = masking dataset\n"
   "          Sub-brick = which one to use\n\n"
   " Aboot:   If activated, then only voxels within a distance of Radius mm\n"
   "          of the current crosshairs will be used in the histogram.\n"
   "\n"
   " Author -- RW Cox - November 2007\n"
;

/***********************************************************************
   Set up the interface to the user
************************************************************************/

DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface *plint ;
   int ii ; char label[32] ;
#define NCTAB 4
   static int ctab[NCTAB] = { 6 , 7 , 14 , 16 } ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "HistogMulti" ,
                                "Histogram of Dataset Bricks" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , MHIST_main  ) ;

   PLUTO_add_hint( plint , "Histogram of Dataset Bricks" ) ;

   PLUTO_set_sequence( plint , "A:afniinfo:dsethistog" ) ;

   PLUTO_set_runlabels( plint , "Plot+Keep" , "Plot+Close" ) ;  /* 04 Nov 2003 */

   /*-- Source dataset inputs --*/

   for( ii=0 ; ii < MAX_DSET ; ii++ ){
     sprintf(label,"Source#%d",ii+1) ;
     PLUTO_add_option( plint , label , "Source" , (ii==0) ? TRUE : FALSE ) ;
     PLUTO_add_dataset(plint , "Dataset" ,
                                ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
     PLUTO_add_number( plint , "Index Bot" , 0,9999,0 , 0,1 ) ;
     PLUTO_add_number( plint , "Index Top" , 0,9999,0 , 0,1 ) ;
     PLUTO_set_initcolorindex( ctab[ii%NCTAB] ) ;
     PLUTO_add_overlaycolor( plint , "Color" ) ;
   }

   /*-- Values to use --*/

   PLUTO_add_option( plint , "Values" , "Values" , FALSE ) ;
   PLUTO_add_number( plint , "Bottom" , -99999,99999, 0,  1,1 ) ;
   PLUTO_add_number( plint , "Top"    , -99999,99999, 0, -1,1 ) ;

   /*-- Bins to use --*/

   PLUTO_add_option( plint , "Bins" , "Bins" , FALSE ) ;
   PLUTO_add_number( plint , "Number" , 10,1000,0, 100,1 ) ;

   /*-- Mask to use --*/

   PLUTO_add_option( plint , "Mask" , "Mask" , FALSE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ;

   /*-- Aboot --*/

   PLUTO_add_option( plint , "Aboot" , "Aboot" , FALSE ) ;
   PLUTO_add_number( plint , "Radius" , 2,100,0,10,1 ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

static char * MHIST_main( PLUGIN_interface *plint )
{
   MCW_idcode *idc ;
   THD_3dim_dataset *input_dset , *mask_dset=NULL ;
   int iv , mcount , nvox , ii,jj , nbin=-1 , do_mval=0,mval ;
   float mask_bot=666.0 , mask_top=-666.0 , hbot,htop ;
   float val_bot=666.0  , val_top=-666.0 ;
   char *tag , *str , buf[THD_MAX_NAME+16] ;
   byte *mmm ;
   MRI_IMAGE *flim ;
   float     *flar ;
   int       *hbin ;
   int smooth=0 ;      /* 03 Dec 2004 */

   int miv=0 ;

   int maxcount=0 ; /* 01 Mar 2001 */
   float hrad=0.0 ; /* 20 Mar 2001 */

   char *histout=NULL ; /* 05 Feb 2002 - VR */
   FILE *MHISTUT=NULL ; /* 05 Feb 2002 - VR */
   int writehist=0    ; /* 05 Feb 2002 - VR */
   float dx           ; /* 05 Feb 2002 - VR */

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "***********************\n"
             "MHIST_main:  NULL input\n"
             "***********************"  ;

   /*-- read 1st line --*/

   PLUTO_next_option(plint) ;
   idc        = PLUTO_get_idcode(plint) ;
   input_dset = PLUTO_find_dset(idc) ;
   if( input_dset == NULL )
      return "******************************\n"
             "MHIST_main:  bad input dataset\n"
             "******************************"  ;

   iv = (int) PLUTO_get_number(plint) ;
   if( iv >= DSET_NVALS(input_dset) || iv < 0 )
      return "********************************\n"
             "MHIST_main:  bad input sub-brick\n"
             "********************************" ;

   DSET_load(input_dset) ;
   if( DSET_ARRAY(input_dset,iv) == NULL )
      return "*******************************\n"
             "MHIST_main:  can't load dataset\n"
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
                   "MHIST_main:  Can't use Range without Mask\n"
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
                   "MHIST_main:  bad mask dataset\n"
                   "*****************************"  ;

         if( DSET_NVOX(mask_dset) != nvox )
           return "***********************************************************\n"
                  "MHIST_main: mask input dataset doesn't match source dataset\n"
                  "***********************************************************" ;

         miv = (int) PLUTO_get_number(plint) ;  /* 06 Aug 1998 */
         if( miv >= DSET_NVALS(mask_dset) || miv < 0 )
            return "***************************************************\n"
                   "MHIST_main: mask dataset sub-brick index is illegal\n"
                   "***************************************************"  ;

         DSET_load(mask_dset) ;
         if( DSET_ARRAY(mask_dset,miv) == NULL )
            return "************************************\n"
                   "MHIST_main:  can't load mask dataset\n"
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
      MCW_cluster *cl ;
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
         MHISTUT = fopen (histout, "w") ;
         if( MHISTUT == NULL ){
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
         short *bar = (short *) DSET_ARRAY(input_dset,iv) ;
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
         byte *bar = (byte *) DSET_ARRAY(input_dset,iv) ;
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
         float *bar = (float *) DSET_ARRAY(input_dset,iv) ;
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
   sprintf(buf,"\\noesc %s[%d] %d voxels",DSET_FILECODE(input_dset),iv,mcount);
   PLUTO_histoplot( nbin,hbot,htop,hbin , NULL , NULL ,  buf , 0,NULL ) ;

   /*-- 05 Feb 2002: Output - VR --*/

   if ( MHISTUT != NULL )
   {
      if( hbot >= htop ){ hbot = 0.0 ; htop = nbin ;}

      dx = (htop-hbot)/nbin ;

      for( ii=0 ; ii <= nbin ; ii++ )
         fprintf (MHISTUT, "%12.6f %13d \n", hbot+ii*dx, hbin[ii]) ;

      fclose (MHISTUT) ;

      fprintf (stderr, "%s written to disk \n", histout) ;
   }

   /*-- go home to mama --*/

   free(hbin) ; free(mmm) ; mri_free(flim) ; return NULL ;
}
