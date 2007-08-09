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
  Plugin to plot a scatterplot of 2 bricks.
  [Adapted from plug_histog.c - RWCox - 13 Jan 2000]
  11 Aug 2001: modified to print correlation coefficient
************************************************************************/

char * SCAT_main( PLUGIN_interface * ) ;

static char helpstring[] =
   " \n"
   " Purpose:  Scatterplot data from 2 bricks.\n"
   "\n"
   " Source x: Dataset   = dataset for x-axis values\n"
   "           Sub-brick = which one to use\n"
   "           Bottom    = minimum value to include\n"
   "           Top       = maximum value to include\n"
   "\n"
   " Source y: Dataset   = dataset for y-axis values\n"
   "           Sub-brick = which one to use\n"
   "           Bottom    = minimum value to include\n"
   "           Top       = maximum value to include\n"
   "\n"
   " Mask:     Dataset   = masking dataset\n"
   "           Sub-brick = which one to use\n"
   "           Bottom    = min value from mask dataset to use\n"
   "           Top       = max value from mask dataset to use\n"
   "\n"
   "In the above definitions:\n"
   "  if Bottom >  Top, then all voxels will be used\n"
   "  if Bottom <= Top, then only voxels in this range will be used\n"
   "\n"
   "Note: A maximum of 4,000,000 (four million) points can be plotted.\n\n"
   " Author -- RW Cox - 13 January 2000\n"
;

/***********************************************************************
   Set up the interface to the user
************************************************************************/


DEFINE_PLUGIN_PROTOTYPE

PLUGIN_interface * PLUGIN_init( int ncall )
{
   PLUGIN_interface * plint ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "ScatterPlot" ,
                                "ScatterPlot of 2 Bricks" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , SCAT_main  ) ;

   PLUTO_add_hint( plint , "Of 2 Bricks" ) ;

   PLUTO_set_sequence( plint , "A:afniinfo:dset" ) ;

   PLUTO_set_runlabels( plint , "Plot+Keep" , "Plot+Close" ) ;  /* 04 Nov 2003 */

   /*-- first line of input --*/

   PLUTO_add_option( plint , "Source x" , "Source x" , TRUE ) ;
   PLUTO_add_dataset(plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ;
   PLUTO_add_number( plint , "Bottom" , -99999,99999, 0,  1,1 ) ;
   PLUTO_add_number( plint , "Top"    , -99999,99999, 0, -1,1 ) ;

   /*-- second line of input --*/

   PLUTO_add_option( plint , "Source y" , "Source y" , TRUE ) ;
   PLUTO_add_dataset(plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ;
   PLUTO_add_number( plint , "Bottom" , -99999,99999, 0,  1,1 ) ;
   PLUTO_add_number( plint , "Top"    , -99999,99999, 0, -1,1 ) ;

   /*-- third line of input --*/

   PLUTO_add_option( plint , "Mask" , "Mask" , FALSE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ;
   PLUTO_add_number( plint , "Bottom" , -99999,99999, 0,  1,1 ) ;
   PLUTO_add_number( plint , "Top"    , -99999,99999, 0, -1,1 ) ;

   /*-- fourth line of input [30 Oct 2006] --*/

   PLUTO_add_option( plint , "Aboot" , "Aboot" , FALSE ) ;
   PLUTO_add_number( plint , "Radius" , 2,100,0,10,1 ) ;

   /*-- fifth and sixth lines of input [08 Aug 2007] --*/

   PLUTO_add_option( plint , "Percent" , "Percent" , FALSE ) ;
   PLUTO_add_number( plint , "Percent" , 1,100,0,10,1 ) ;

   PLUTO_add_option( plint , "Label" , "Label" , FALSE ) ;
   PLUTO_add_string( plint , "Label" , 0,NULL , 16 ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

char * SCAT_main( PLUGIN_interface *plint )
{
   MCW_idcode *idc ;
   THD_3dim_dataset *xdset, *ydset , *mask_dset=NULL ;
   int ivx,ivy , mcount , nvox , ii,jj , nbin=-1 ;
   float mask_bot=666.0 , mask_top=-666.0 ;
   float xbot,xtop , ybot,ytop , pcor=0 , a=0,b=0 ;
   char *tag , *str ;
   char xlab[THD_MAX_NAME],ylab[THD_MAX_NAME],tlab[THD_MAX_NAME] ;
   char ab[16] , bb[16] , *pab,*pbb ;
   byte *mmm ;
   float *xar , *yar ;

   char *cname=NULL ;  /* 06 Aug 1998 */
   int miv=0 ;

   float hrad=0.0f ;   /* 30 Oct 2006 */
   float perc=0.0f ;   /* 08 Aug 2007 */
   char *label=NULL ;  /* 09 Aug 2007 */

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "**********************\n"
             "SCAT_main:  NULL input\n"
             "**********************"  ;

   /*-- read x dataset line --*/

   PLUTO_next_option(plint) ;
   idc   = PLUTO_get_idcode(plint) ;
   xdset = PLUTO_find_dset(idc) ;
   if( xdset == NULL )
      return "*************************\n"
             "SCAT_main:  bad x dataset\n"
             "*************************"  ;

   ivx = (int) PLUTO_get_number(plint) ;
   if( ivx >= DSET_NVALS(xdset) || ivx < 0 )
      return "***************************\n"
             "SCAT_main:  bad x sub-brick\n"
             "***************************" ;

   DSET_load(xdset) ;
   if( DSET_ARRAY(xdset,ivx) == NULL )
      return "********************************\n"
             "SCAT_main:  can't load x dataset\n"
             "********************************"  ;
   nvox = DSET_NVOX(xdset) ;

   xbot = PLUTO_get_number(plint) ;
   xtop = PLUTO_get_number(plint) ;

   /*-- read y dataset line --*/

   PLUTO_next_option(plint) ;
   idc   = PLUTO_get_idcode(plint) ;
   ydset = PLUTO_find_dset(idc) ;
   if( ydset == NULL )
      return "*************************\n"
             "SCAT_main:  bad y dataset\n"
             "*************************"  ;

   ivy = (int) PLUTO_get_number(plint) ;
   if( ivy >= DSET_NVALS(ydset) || ivy < 0 )
      return "***************************\n"
             "SCAT_main:  bad y sub-brick\n"
             "***************************" ;

   if( DSET_NVOX(ydset) != nvox )
      return "************************************************\n"
             "SCAT_main:  x and y datasets don't match in size\n"
             "************************************************" ;

   DSET_load(ydset) ;
   if( DSET_ARRAY(ydset,ivy) == NULL )
      return "********************************\n"
             "SCAT_main:  can't load y dataset\n"
             "********************************"  ;

   ybot = PLUTO_get_number(plint) ;
   ytop = PLUTO_get_number(plint) ;

   /*-- read optional line(s) --*/

   while( (tag=PLUTO_get_optiontag(plint)) != NULL ){

      /*-- Mask dataset --*/

      if( strcmp(tag,"Mask") == 0 ){

         idc       = PLUTO_get_idcode(plint) ;
         mask_dset = PLUTO_find_dset(idc) ;

         if( mask_dset == NULL )
            return "****************************\n"
                   "SCAT_main:  bad mask dataset\n"
                   "****************************"  ;

         if( DSET_NVOX(mask_dset) != nvox )
           return "**********************************************************\n"
                  "SCAT_main: mask input dataset doesn't match source dataset\n"
                  "**********************************************************" ;

         miv = (int) PLUTO_get_number(plint) ;  /* 06 Aug 1998 */
         if( miv >= DSET_NVALS(mask_dset) || miv < 0 )
            return "**************************************************\n"
                   "SCAT_main: mask dataset sub-brick index is illegal\n"
                   "**************************************************"  ;

         DSET_load(mask_dset) ;
         if( DSET_ARRAY(mask_dset,miv) == NULL )
            return "***********************************\n"
                   "SCAT_main:  can't load mask dataset\n"
                   "***********************************"  ;

         mask_bot = PLUTO_get_number(plint) ;
         mask_top = PLUTO_get_number(plint) ;
         continue ;
      }

      /*-- 30 Oct 2006: Aboot --*/

      if( strcmp(tag,"Aboot") == 0 ){
         hrad = PLUTO_get_number(plint) ;
         continue ;
      }

      /*-- 08 Aug 2007: Percent --*/

      if( strcmp(tag,"Percent") == 0 ){
         perc = PLUTO_get_number(plint) ;
         continue ;
      }

      /*-- 09 Aug 2007: Label --*/

      if( strcmp(tag,"Label") == 0 ){
        char *ll = PLUTO_get_string(plint) ;
        if( ll != NULL ) label = strdup(ll) ;
        continue ;
      }
   }

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- build the byte mask array --*/

   if( mask_dset == NULL ){
      mmm = (byte *) malloc( sizeof(byte) * nvox ) ;
      if( mmm == NULL )
         return " \n*** Can't malloc workspace for mask! ***\n" ;
      memset( mmm , 1, nvox ) ; mcount = nvox ;
   } else {

      mmm = THD_makemask( mask_dset , miv , mask_bot , mask_top ) ;
      if( mmm == NULL )
         return " \n*** Can't make mask for some reason! ***\n" ;
      mcount = THD_countmask( nvox , mmm ) ;

#if 0
      if( !EQUIV_DSETS(mask_dset,xdset) &&
          !EQUIV_DSETS(mask_dset,ydset)   ) DSET_unload(mask_dset) ;
#endif

      if( mcount < 3 ){
        free(mmm) ;
        return " \n*** Less than 3 voxels survive the mask! ***\n" ;
      }
   }

   /*-- 30 Oct 2006: modify mask via Aboot Radius, if present --*/

   if( hrad > 0.0f ){
      MCW_cluster *cl ;
      short *di,*dj,*dk ;
      int nd , xx,yy,zz , dd , nx,ny,nz,nxy, nx1,ny1,nz1 , ip,jp,kp ;

      cl = MCW_build_mask( fabs(DSET_DX(xdset)) ,
                           fabs(DSET_DY(xdset)) ,
                           fabs(DSET_DZ(xdset)) , hrad ) ;

      if( cl == NULL || cl->num_pt < 6 ){
         KILL_CLUSTER(cl);
         PLUTO_popup_transient(plint, " \n"
                                      " Aboot Radius too small\n"
                                      " for this dataset!\n"     ) ;
      } else {
         ADDTO_CLUSTER(cl,0,0,0,0) ;
         di = cl->i ; dj = cl->j ; dk = cl->k ; nd = cl->num_pt ;
         nx = DSET_NX(xdset) ; nx1 = nx-1 ;
         ny = DSET_NY(xdset) ; ny1 = ny-1 ; nxy  = nx*ny ;
         nz = DSET_NZ(xdset) ; nz1 = nz-1 ;
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
         if( perc == 0.0f || perc == 100.0f ){
           char buf[256] ;
           sprintf(buf," \n"
                       " %d voxels in the mask+radius\n"
                       " out of %d dataset voxels\n ",mcount,nvox) ;
           PLUTO_popup_transient(plint,buf) ;
         }
      }
   }

   /* 08 Aug 2007: modify mask by randomly selecting a percentage of it */

   if( perc > 0.0f && perc < 100.0f ){
     double pp = 0.01 * perc ; char buf[256] ;
     for( ii=0 ; ii < nvox ; ii++ ){
       if( mmm[ii] && drand48() > pp ) mmm[ii] = 0 ;
     }
     mcount = THD_countmask( nvox , mmm ) ;
     if( mcount < 3 ){
       free(mmm) ;
       return " \n*** Less than 3 voxels survive the percenting! ***\n" ;
     }
     sprintf(buf," \n"
                 " %d voxels in mask after percenting\n"
                 " out of %d dataset voxels\n ",mcount,nvox) ;
     PLUTO_popup_transient(plint,buf) ;
   }

   /*-- allocate the space for the data to be plotted --*/

   xar = (float *) malloc(sizeof(float)*mcount) ;
   yar = (float *) malloc(sizeof(float)*mcount) ;

   /*-- load values into x array --*/

   switch( DSET_BRICK_TYPE(xdset,ivx) ){
      case MRI_short:{
         short *bar = (short *) DSET_ARRAY(xdset,ivx) ;
         float mfac = DSET_BRICK_FACTOR(xdset,ivx) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         for( ii=jj=0 ; ii < nvox ; ii++ )
            if( mmm[ii] ) xar[jj++] = mfac*bar[ii] ;
      }
      break ;

      case MRI_byte:{
         byte *bar = (byte *) DSET_ARRAY(xdset,ivx) ;
         float mfac = DSET_BRICK_FACTOR(xdset,ivx) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         for( ii=jj=0 ; ii < nvox ; ii++ )
            if( mmm[ii] ) xar[jj++] = mfac*bar[ii] ;
      }
      break ;

      case MRI_float:{
         float *bar = (float *) DSET_ARRAY(xdset,ivx) ;
         float mfac = DSET_BRICK_FACTOR(xdset,ivx) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         for( ii=jj=0 ; ii < nvox ; ii++ )
            if( mmm[ii] ) xar[jj++] = mfac*bar[ii] ;
      }
      break ;
   }

   /*-- load values into y array --*/

   switch( DSET_BRICK_TYPE(ydset,ivy) ){
      case MRI_short:{
         short *bar = (short *) DSET_ARRAY(ydset,ivy) ;
         float mfac = DSET_BRICK_FACTOR(ydset,ivy) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         for( ii=jj=0 ; ii < nvox ; ii++ )
            if( mmm[ii] ) yar[jj++] = mfac*bar[ii] ;
      }
      break ;

      case MRI_byte:{
         byte *bar = (byte *) DSET_ARRAY(ydset,ivy) ;
         float mfac = DSET_BRICK_FACTOR(ydset,ivy) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         for( ii=jj=0 ; ii < nvox ; ii++ )
            if( mmm[ii] ) yar[jj++] = mfac*bar[ii] ;
      }
      break ;

      case MRI_float:{
         float *bar = (float *) DSET_ARRAY(ydset,ivy) ;
         float mfac = DSET_BRICK_FACTOR(ydset,ivy) ;
         if( mfac == 0.0 ) mfac = 1.0 ;
         for( ii=jj=0 ; ii < nvox ; ii++ )
            if( mmm[ii] ) yar[jj++] = mfac*bar[ii] ;
      }
      break ;
   }

   /* remove those voxels that aren't in the data ranges for both datasets */

   if( xbot < xtop || ybot < ytop ){
      int nm ; float *tar ;

      /* make the mask of those that survive the x range */

      if( xbot < xtop ){
         for( jj=0 ; jj < mcount ; jj++ )
            mmm[jj] = ( xar[jj] >= xbot && xar[jj] <= xtop ) ;
      } else {
         memset( mmm , 1 , mcount ) ;
      }

      /* and hit that mask with the y range */

      if( ybot < ytop ){
         for( jj=0 ; jj < mcount ; jj++ )
            if( mmm[jj] )
               mmm[jj] = ( yar[jj] >= ybot && yar[jj] <= ytop ) ;
      }

      nm = THD_countmask( mcount , mmm ) ;  /* how many are left */

      if( nm < 1 ){
         free(mmm) ; free(xar) ; free(yar) ;
         return " \n*** No values survive to be plotted! ***\n" ;
      }

      /* copy survivors into new lists and free old lists */

      if( nm < mcount ){  /* we actually lost some points */
         tar = (float *) malloc(sizeof(float)*nm) ;
         for( ii=jj=0 ; ii < mcount ; ii++ )
            if( mmm[ii] ) tar[jj++] = xar[ii] ;
         free(xar) ; xar = tar ;

         tar = (float *) malloc(sizeof(float)*nm) ;
         for( ii=jj=0 ; ii < mcount ; ii++ )
            if( mmm[ii] ) tar[jj++] = yar[ii] ;
         free(yar) ; yar = tar ;

         mcount = nm ;
      }
   }

   free(mmm) ;  /* don't need this no more */

   if( mcount > 4000000 ){
      static char msg[128] ;
      free(xar) ; free(yar) ;
      sprintf(msg," \n*** Attempt to scatterplot %d points!\n"
                     "*** Maximum allowed is     4000000.\n" , mcount ) ;
      return msg ;
   }

   /* compute the labels for the plot */

   if( xbot >= xtop ){
      sprintf( xlab , "\\noesc %s[%d]" , DSET_FILECODE(xdset),ivx ) ;
   } else {
      AV_fval_to_char(xbot,ab) ; AV_fval_to_char(xtop,bb) ;
      pab = ab ; if( *pab == ' ' ) pab++ ;
      pbb = bb ; if( *pbb == ' ' ) pbb++ ;
      sprintf( xlab , "\\noesc %s[%d]<%s..%s>" , DSET_FILECODE(xdset),ivx,pab,pbb ) ;
   }

   if( ybot >= ytop ){
      sprintf( ylab , "\\noesc %s[%d]" , DSET_FILECODE(ydset),ivy ) ;
   } else {
      AV_fval_to_char(ybot,ab) ; AV_fval_to_char(ytop,bb) ;
      pab = ab ; if( *pab == ' ' ) pab++ ;
      pbb = bb ; if( *pbb == ' ' ) pbb++ ;
      sprintf( ylab , "\\noesc %s[%d]<%s..%s>" , DSET_FILECODE(ydset),ivy,pab,pbb ) ;
   }

   /*- 11 Aug 2001: compute correlation coefficient -*/

   if( mcount > 1 ){
     float xbar=0,ybar=0 , xq=0,yq=0,xyq=0 ;
     for( ii=0 ; ii < mcount ; ii++ ){ xbar += xar[ii]; ybar += yar[ii]; }
     xbar /= mcount ; ybar /= mcount ;
     for( ii=0 ; ii < mcount ; ii++ ){
        xq  += (xar[ii]-xbar)*(xar[ii]-xbar) ;
        yq  += (yar[ii]-ybar)*(yar[ii]-ybar) ;
        xyq += (xar[ii]-xbar)*(yar[ii]-ybar) ;
     }
     if( xq > 0.0 && yq > 0.0 ){
       pcor = xyq/sqrt(xq*yq); a = xyq/xq; b = (xq*ybar-xbar*xyq)/xq; }
   }

   if( label != NULL ){
     strcpy(tlab,label) ;   /* 09 Aug 2007 */
   } else if( mask_dset == NULL ){
      sprintf(tlab,"Scatter Plot: %d Voxels",mcount) ;
   } else {
      sprintf(tlab,"\\noesc Scatter Plot: %d Voxels (%s)",
              mcount , DSET_FILECODE(mask_dset)   ) ;
   }
   if( pcor != 0.0 ){
     char abuf[16] ; AV_fval_to_char(a,abuf) ;
     sprintf(tlab+strlen(tlab)," R=%.3f a=%s",pcor,abuf) ;
   }

   /*-- actually plot data --*/

   PLUTO_scatterplot( mcount , xar,yar , xlab,ylab,tlab , a,b ) ;

   /*-- go home to papa --*/

   if( label != NULL ) free(label) ;
   free(xar) ; free(yar) ; return NULL ;
}
