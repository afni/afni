#include "afni.h"

#ifndef ALLOW_PLUGINS
#  error "Plugins not properly set up -- see machdep.h"
#endif

/***************************************************************************/

static int max_dset = 3 ;   /* number of datasets to allow */
#undef  MAXMAX
#define MAXMAX 9

#undef  DEBUG
#define DEBUG 0

static char *MHIST_main( PLUGIN_interface * ) ;

static char helpstring[] =
   " Purpose: Plot histogram of data from dataset brick or bricks.\n"
   "\n"
   " Source#1:  Dataset   = data to be processed\n"
   "            Index Bot = first sub-brick to use\n"
   "            Index Top = last sub-brick to use\n"
   "            * if(Bot > Top) then all sub-bricks will be used\n"
   "            Color     = line color to use\n"
   " Source#2:  Optional second (etc.) datasets to histogramize (etc.)\n\n"
   " Values:  Bottom    = minimum value from dataset to include\n"
   "          Top       = maximum value from dataset to include\n\n"
   " Bins:    Number    = number of bins to use\n"
   "          Smooth    = number of bins to smooth over (+/-)\n"
   " Mask:    Dataset   = masking dataset\n"
   "          Sub-brick = which one to use for the mask \n\n"
   " Aboot:   If activated, then only voxels within a distance of Radius mm\n"
   "          of the current crosshairs will be used in the histogram.\n"
   "          *  Radius=0 means only that voxel will be used -- in which case\n"
   "             you must have more than one sub-brick included in the Source!\n"
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
#define NCTAB 5
   static int ctab[NCTAB] = { 6 , 7 , 14 , 16 , 20 } ;

   if( ncall > 0 ) return NULL ;  /* only one interface */

   /*-- set titles and call point --*/

   plint = PLUTO_new_interface( "Histogram: Multi" ,
                                "Histogram of Dataset Bricks" ,
                                helpstring ,
                                PLUGIN_CALL_VIA_MENU , MHIST_main  ) ;

   PLUTO_add_hint( plint , "Histogram of Dataset Bricks" ) ;

   PLUTO_set_sequence( plint , "A:afniinfo:dsethistog" ) ;

   PLUTO_set_runlabels( plint , "Plot+Keep" , "Plot+Close" ) ;  /* 04 Nov 2003 */

   /*-- Source dataset inputs --*/

   ii = (int)AFNI_numenv("AFNI_HISTOG_MAXDSET") ;
   if( ii > max_dset && ii <= MAXMAX ) max_dset = ii ;

   for( ii=0 ; ii < max_dset ; ii++ ){
     sprintf(label,"Source#%d",ii+1) ;
     PLUTO_add_option( plint , label , "Source" , FALSE ) ;
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
   PLUTO_add_number( plint , "Number" , 10,1000,0, 10,1 ) ;
   PLUTO_add_number( plint , "Smooth" ,  0,100 ,0,  0,1 ) ;

   /*-- Mask to use --*/

   PLUTO_add_option( plint , "Mask" , "Mask" , FALSE ) ;
   PLUTO_add_dataset( plint , "Dataset" ,
                                    ANAT_ALL_MASK , FUNC_ALL_MASK ,
                                    DIMEN_ALL_MASK | BRICK_ALLREAL_MASK ) ;
   PLUTO_add_number( plint , "Sub-brick" , 0,9999,0 , 0,1 ) ;

   /*-- Aboot --*/

   PLUTO_add_option( plint , "Aboot" , "Aboot" , FALSE ) ;
   PLUTO_add_number( plint , "Radius" , 0,100,0,1,1 ) ;

   return plint ;
}

/***************************************************************************
  Main routine for this plugin (will be called from AFNI).
****************************************************************************/

static char * MHIST_main( PLUGIN_interface *plint )
{
   MCW_idcode *idc ;
   THD_3dim_dataset *input_dset[MAXMAX] , *mask_dset=NULL ;
   int                   ovcolr[MAXMAX] , indbot[MAXMAX] , indtop[MAXMAX] ;
   int num_dset=0 ;
   int iv , mcount , nvox=0 , ii,jj , nbin=-1 ,
       do_mval=0,mval , id,ivbot,ivtop , nvals , mval_max , tval ;
   float hbot,htop ;
   float val_bot=666.0  , val_top=-666.0 ;
   char *tag , *str , buf[THD_MAX_NAME+16] ;
   byte *mmm ;
   MRI_IMAGE *flim[MAXMAX] ;
   float     *flar ;
   int       *hbin[MAXMAX] ;
   int smooth=0 ;
   int miv=0 ;
   int maxcount=0 ;
   float hrad=-1.0f ;
   float ovc_rrr[2*MAXMAX] , ovc_ggg[2*MAXMAX] , ovc_bbb[2*MAXMAX] ;

   /*--------------------------------------------------------------------*/
   /*----- Check inputs from AFNI to see if they are reasonable-ish -----*/

   if( plint == NULL )
      return "***********************\n"
             "MHIST_main:  NULL input\n"
             "***********************"  ;

if(DEBUG)fprintf(stderr,"+++++++++++++++++++++++++++++++++++\n") ;

   while( (tag=PLUTO_get_optiontag(plint)) != NULL ){

     /* Source (dataset) */

     if( strcmp(tag,"Source") == 0 ){

       idc                  = PLUTO_get_idcode(plint) ;
       input_dset[num_dset] = PLUTO_find_dset(idc) ;

       if( ! ISVALID_DSET( input_dset[num_dset] ) )
         return("******************************\n"
                "MHIST_main:  bad input dataset\n"
                "******************************") ;

       DSET_load(input_dset[num_dset]) ;

       if( !DSET_LOADED(input_dset[num_dset]) )
         return "*******************************\n"
                "MHIST_main:  can't load dataset\n"
                "*******************************"  ;

       indbot[num_dset] = (int)PLUTO_get_number(plint) ;
       indtop[num_dset] = (int)PLUTO_get_number(plint) ;
       ovcolr[num_dset] = PLUTO_get_overlaycolor(plint) ;

       ovc_rrr[num_dset] = DCOV_REDBYTE  (plint->im3d->dc,ovcolr[num_dset])/255.0f ;
       ovc_ggg[num_dset] = DCOV_GREENBYTE(plint->im3d->dc,ovcolr[num_dset])/255.0f ;
       ovc_bbb[num_dset] = DCOV_BLUEBYTE (plint->im3d->dc,ovcolr[num_dset])/255.0f ;

if(DEBUG)fprintf(stderr,"++ Dataset #%d '%s' - %d..%d  ovc=%d\n",
                 num_dset+1,DSET_BRIKNAME(input_dset[num_dset]),
                 indbot[num_dset],indtop[num_dset],ovcolr[num_dset]) ;

       if( num_dset == 0 )
         nvox = DSET_NVOX(input_dset[0]) ;
       else if( DSET_NVOX(input_dset[num_dset]) != nvox )
         return "*************************************\n"
                "MHIST_main:  incompatible datasets?!?\n"
                "*************************************"  ;

       num_dset++ ; continue ;
     }

     if( strcmp(tag,"Values") == 0 ){
       val_bot = PLUTO_get_number(plint) ;
       val_top = PLUTO_get_number(plint) ;
       do_mval = (val_bot < val_top) ;
       continue ;
     }

     if( strcmp(tag,"Mask") == 0 ){
       if( num_dset == 0 ) break ;   /* no data == bad! */
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

     if( strcmp(tag,"Bins") == 0 ){
       nbin     = PLUTO_get_number(plint) ;
       smooth   = PLUTO_get_number(plint) ;
#if 0
       maxcount = PLUTO_get_number(plint) ;
#endif
       continue ;
     }

     if( strcmp(tag,"Aboot") == 0 ){
       hrad = PLUTO_get_number(plint) ;
       continue ;
     }

   }

   if( num_dset == 0 )
     return("********************************\n"
            "MHIST_main: no input datasets?!?\n"
            "********************************") ;

   /*------------------------------------------------------*/
   /*---------- At this point, the inputs are OK ----------*/

   /*-- build a byte mask array --*/

   if( mask_dset == NULL ){
      mmm = (byte *) malloc( sizeof(byte) * nvox ) ;
      if( mmm == NULL )
         return " \n*** Can't malloc workspace! ***\n" ;
      memset( mmm , 1, nvox ) ; mcount = nvox ;
   } else {

      mmm = THD_makemask( mask_dset , miv , 666.0f , -666.0f ) ;
      if( mmm == NULL )
        return " \n*** Can't make mask for some reason! ***\n" ;
      mcount = THD_countmask( nvox , mmm ) ;

      if( mcount < 3 ){
        free(mmm) ;
        return " \n*** Less than 3 voxels survive the mask! ***\n" ;
      }
#if 0
      sprintf(buf," \n"
                  " %d voxels in the mask\n"
                  " out of %d dataset voxels\n ",mcount,nvox) ;
      PLUTO_popup_transient(plint,buf) ;
#endif
   }

   /*-- 20 Mar 2001: modify mask via Aboot Radius, if present --*/

   if( hrad >= 0.0 ){
     MCW_cluster *cl ;
     short *di,*dj,*dk ;
     int nd , xx,yy,zz , dd , nx,ny,nz,nxy, nx1,ny1,nz1 , ip,jp,kp ;
     float dx,dy,dz , dm ;

     dx = fabs(DSET_DX(input_dset[0])) ;
     dy = fabs(DSET_DY(input_dset[0])) ; dm = MIN(dx,dy) ;
     dz = fabs(DSET_DZ(input_dset[0])) ; dm = MIN(dm,dz) ;

     if( hrad < dm ){
       INIT_CLUSTER(cl) ;
     } else {
       cl = MCW_build_mask( dx,dy,dz , hrad ) ;
       if( cl == NULL ) INIT_CLUSTER(cl) ;
     }
     ADDTO_CLUSTER(cl,0,0,0,0) ;
     di = cl->i ; dj = cl->j ; dk = cl->k ; nd = cl->num_pt ;
     nx = DSET_NX(input_dset[0]) ; nx1 = nx-1 ;
     ny = DSET_NY(input_dset[0]) ; ny1 = ny-1 ; nxy  = nx*ny ;
     nz = DSET_NZ(input_dset[0]) ; nz1 = nz-1 ;
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

     if( mcount < 1 ){
       free(mmm) ;
       return " \n*** Less than 1 voxels survive the mask+radius! ***\n" ;
     }
#if 0
     sprintf(buf," \n"
                 " %d voxels in the mask+radius\n"
                 " out of %d dataset voxels\n ",mcount,nvox) ;
     PLUTO_popup_transient(plint,buf) ;
#endif
   }

   /*------ loop over input datasets ------*/

   for( id=0 ; id < num_dset ; id++ ) flim[id] = NULL ;

   tval = mval_max = 0 ;

   for( id=0 ; id < num_dset ; id++ ){  /* load data into flim[id] */

     ivbot = indbot[id] ; ivtop = indtop[id] ; nvals = DSET_NVALS(input_dset[id]) ;
     if( ivtop > nvals-1 ) ivtop = nvals-1 ;
     if( ivbot > ivtop ){ ivbot = 0 ; ivtop = nvals-1 ; }
     indbot[id] = ivbot ; indtop[id] = ivtop ;
     nvals = ivtop-ivbot+1 ;
if(DEBUG)fprintf(stderr,"++ Dataset #%d -- sub-bricks [%d..%d]\n",id,ivbot,ivtop) ;

     /*-- allocate an array to histogrammatize --*/

     flim[id] = mri_new( mcount*nvals , 1 , MRI_float ) ;
     flar = MRI_FLOAT_PTR(flim[id]) ;

      /*-- load values into this array --*/

     for( jj=0,iv=ivbot ; iv <= ivtop ; iv++ ){
       switch( DSET_BRICK_TYPE(input_dset[id],iv) ){
         default:
           free(mmm) ;
           for( jj=0 ; jj < num_dset ; jj++ ) mri_free(flim[jj]) ;
           return "*** Can't use source dataset -- illegal data type! ***" ;

         case MRI_short:{
           short *bar = (short *) DSET_ARRAY(input_dset[id],iv) ;
           float mfac = DSET_BRICK_FACTOR(input_dset[id],iv) ;
           if( mfac == 0.0 ) mfac = 1.0 ;
           if( do_mval ){
             float val ;
             for( ii=0 ; ii < nvox ; ii++ ){
               if( mmm[ii] ){
                 val = mfac*bar[ii] ;
                 if( val >= val_bot && val <= val_top ) flar[jj++] = val ;
               }
             }
           } else {
             for( ii=0 ; ii < nvox ; ii++ )
               if( mmm[ii] ) flar[jj++] = mfac*bar[ii] ;
           }
         }
         break ;

         case MRI_byte:{
           byte *bar = (byte *) DSET_ARRAY(input_dset[id],iv) ;
           float mfac = DSET_BRICK_FACTOR(input_dset[id],iv) ;
           if( mfac == 0.0 ) mfac = 1.0 ;
           if( do_mval ){
             float val ;
             for( ii=0 ; ii < nvox ; ii++ ){
               if( mmm[ii] ){
                 val = mfac*bar[ii] ;
                 if( val >= val_bot && val <= val_top ) flar[jj++] = val ;
               }
             }
           } else {
             for( ii=0 ; ii < nvox ; ii++ )
               if( mmm[ii] ) flar[jj++] = mfac*bar[ii] ;
           }
         }
         break ;

         case MRI_float:{
           float *bar = (float *) DSET_ARRAY(input_dset[id],iv) ;
           float mfac = DSET_BRICK_FACTOR(input_dset[id],iv) ;
           if( mfac == 0.0 ) mfac = 1.0 ;
           if( do_mval ){
             float val ;
             for( ii=0 ; ii < nvox ; ii++ ){
               if( mmm[ii] ){
                 val = mfac*bar[ii] ;
                 if( val >= val_bot && val <= val_top ) flar[jj++] = val ;
               }
             }
           } else {
             for( ii=0 ; ii < nvox ; ii++ )
               if( mmm[ii] ) flar[jj++] = mfac*bar[ii] ;
           }
         }
         break ;
       } /* end of switch on sub-brick type */
     } /* end of loop over sub-bricks */
     mval = jj ; /* number of values loaded into flar */
     mval_max = MAX(mval,mval_max) ; tval += mval ;

     if( mval == 0 ){
       free(mmm) ;
       for( jj=0 ; jj < num_dset ; jj++ ) mri_free(flim[jj]) ;
       return " \n*** Can't use source dataset -- no data in Values range ***\n " ;
     }
     flim[id]->nx = flim[id]->nvox = mval ;

   }
   free(mmm) ;  /* done with mask */

   if( mval_max < 9 ){
     for( jj=0 ; jj < num_dset ; jj++ ) mri_free(flim[jj]) ;
     return " \n*** Less than 9 values?! -- can't build histogram!\n " ;
   }

   if( val_bot < val_top ){
     hbot = val_bot ; htop = val_top ;
   } else {
     val_bot = 1.e+33 ; val_top = -val_bot ;
     for( id=0 ; id < num_dset ; id++ ){  /* find data range to use */
       hbot    = mri_min(flim[id]) ; htop    = mri_max(flim[id]) ;
       val_bot = MIN(val_bot,hbot) ; val_top = MAX(val_top,htop) ;
     }
     hbot = val_bot ; htop = val_top ;
     if( hbot >= htop ){
       for( jj=0 ; jj < num_dset ; jj++ ) mri_free(flim[jj]) ;
       return " \n*** Can't use source dataset -- no data range ***\n " ;
     }
   }
if(DEBUG)fprintf(stderr,"++ hbot=%g  htop=%g\n",hbot,htop) ;

   if( nbin < 10 || nbin > 1000 ){
     if( (int)hbot == hbot && (int)htop == htop ){
       nbin = htop - hbot ;
       if( nbin < 10 ){ nbin = 10 ; }
       else           { while( nbin > 1000 ) nbin /= 2 ; }
     } else {
       nbin = (int) sqrt((double)mval_max) ;
       if( nbin < 10 ) nbin = 10 ; else if( nbin > 1000 ) nbin = 1000 ;
     }
   }
if(DEBUG)fprintf(stderr,"++ nbin=%d\n",nbin) ;

   /*-- actually compute and plot histograms --*/

   for( id=0 ; id < num_dset ; id++ ){
     hbin[id] = (int *) calloc((nbin+1),sizeof(int)) ;

if(DEBUG)fprintf(stderr,"++ mri_histogram(#%d)\n",id) ;
     mri_histogram( flim[id] , hbot,htop , TRUE , nbin,hbin[id] ) ;
     mri_free(flim[id]) ;

     if( smooth > 0 ){
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
           ws += wt[nwid-jj+ii] * hbin[id][ii] ; wss += wt[nwid-jj+ii] ;
         }
         gbin[jj] = rint(ws/wss) ;
       }
       memcpy(hbin[id],gbin,sizeof(int)*(nbin+1)) ;
       free((void *)wt) ; free((void *)gbin) ;
     }

#if 0
     if( maxcount > 0 ){
        for( ii=0 ; ii <= nbin ; ii++ ) hbin[id][ii] = MIN(hbin[id][ii],maxcount);
     }
#endif
   }
if(DEBUG)fprintf(stderr,"++ about to plot\n") ;
   hrad = AFNI_numenv("AFNI_1DPLOT_THIK") ;
   if( hrad <= 0.0f || hrad >= 0.02f ) hrad = 0.004f ;
   plot_ts_setTHIK(hrad) ; plot_ts_setthik(0.0015f) ;
   for( jj=0 ; jj < num_dset ; jj++ ){  /* for cumulative histogram [19 Feb 2013] */
     ovc_rrr[jj+num_dset] = ovc_rrr[jj] ;
     ovc_ggg[jj+num_dset] = ovc_ggg[jj] ;
     ovc_bbb[jj+num_dset] = ovc_bbb[jj] ;
   }
   plot_ts_setcolors( 2*num_dset , ovc_rrr , ovc_ggg , ovc_bbb ) ;
   plot_ts_xypush(0,-1) ;
   sprintf(buf,"#mask=%s #values=%s",commaized_integer_string(mcount),commaized_integer_string(tval));
   PLUTO_histoplot( nbin,hbot,htop,hbin[0] , NULL , NULL ,  buf , num_dset-1,hbin+1 ) ;

   /*-- go home to mama --*/

   for( jj=0 ; jj < num_dset ; jj++ ) free(hbin[jj]) ;
   return NULL ;
}
