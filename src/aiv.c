
#include "mrilib.h"
#include "imseq.h"
   
/*------------------------------------------------------------------------*/

typedef struct {
   MCW_imseq * seq ;
   MRI_IMARR * imar ;
   int rgb_count ;
   generic_func * kill_func ;
   void * kill_data ;
} PLUGIN_imseq ;

void * PLUTO_imseq_popup( MRI_IMARR *, generic_func *, void * ) ;
void * PLUTO_imseq_popim( MRI_IMAGE *, generic_func *, void * ) ;

void   PLUTO_imseq_addto( void * , MRI_IMAGE * ) ;
void   PLUTO_imseq_destroy( void * ) ;
void   PLUTO_imseq_retitle( void * , char * ) ;
void   PLUTO_imseq_rekill( void *, generic_func *, void * ) ;

XtPointer PLUTO_imseq_getim( int , int , XtPointer ) ;
void PLUTO_imseq_send_CB( MCW_imseq * , XtPointer , ISQ_cbs * ) ;

/*------------------------------------------------------------------------*/

static char * FALLback[] =
  {   "AFNI*fontList:              9x15bold=charset1"    ,
      "AFNI*pbar*fontList:         6x10=charset1"        ,
      "AFNI*imseq*fontList:        7x13=charset1"        ,
      "AFNI*background:            gray30"               ,
      "AFNI*menu*background:       gray30"               ,
      "AFNI*borderColor:           gray30"               ,
      "AFNI*foreground:            yellow"               ,
      "AFNI*borderWidth:           0"                    ,
      "AFNI*troughColor:           green"                ,
      "AFNI*XmLabel.translations:  #override<Btn2Down>:" , /* Motif 2.0 bug */
      "AFNI*help*background:       black"                ,
      "AFNI*help*foreground:       yellow"               ,
      "AFNI*help*helpborder:       False"                ,
      "AFNI*help*waitPeriod:       1066"                 ,
      "AFNI*help*fontList:         9x15bold=charset1"    ,
      "AFNI*cluefont:              9x15bold"             ,
      "AFNI*help*cancelWaitPeriod: 50"                   ,
   NULL } ;

static MCW_DC       *MAIN_dc ;
static XtAppContext  MAIN_app ;
static MRI_IMARR    *MAIN_imar ;

#define DEFAULT_NCOLOVR 20

static char * INIT_colovr[DEFAULT_NCOLOVR] = {
   "#ffff00" , "#ffcc00"   , "#ff9900"  , "#ff6900" , "#ff4400" , "#ff0000" ,
   "#0000ff" , "#0044ff"   , "#0069ff"  , "#0099ff" , "#00ccff" , "#00ffff" ,
   "green"   , "limegreen" , "violet"   , "hotpink" ,
   "white"   , "#dddddd"   , "#bbbbbb"  , "black"
} ;

static char * INIT_labovr[DEFAULT_NCOLOVR] = {
   "yellow" , "yell-oran" , "oran-yell" , "orange"   , "oran-red" , "red"   ,
   "dk-blue", "blue"      , "lt-blue1"  , "lt-blue2" , "blue-cyan", "cyan"  ,
   "green"  , "limegreen" , "violet"    , "hotpink"  ,
   "white"  , "gry-dd"    , "gry-bb"    , "black"
} ;


/*------------------------------------------------------------------------*/

void killer( void *pt ){ exit(0); }

void AFNI_handler(char * msg){ return ; }

/*------------------------------------------------------------------------*/

void timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   PLUTO_imseq_popup( MAIN_imar , killer , NULL ) ;
}

/*------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int ii ;
   MRI_IMAGE *im ;
   Widget shell ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf("Usage: aiv image ...\n"
            "AFNI Image Viewer:\n"
            "Shows the 2D images on the command line in an AFNI-like image viewer.\n"
            "Image formats are those supported by to3d:\n"
            " * raw PPM or PGM\n"
            " * JPEG (if djpeg is in the path)\n"
            " * GIF, TIFF, BMP, PNG (if netpbm is in the path)\n"
            " * various MRI formats\n"
           ) ;
     exit(0) ;
   }

   INIT_IMARR(MAIN_imar) ;

   for( ii=1 ; ii < argc ; ii++ ){
     fprintf(stderr,"+") ;
     im = mri_read( argv[ii] ) ;
     if( im == NULL ){
       fprintf(stderr,"\n** Can't read image %s - skipping **\n",argv[ii]) ;
       continue ;
     } else
       fprintf(stderr,"%s",argv[ii]) ;
     ADDTO_IMARR( MAIN_imar , im ) ;
   }
   if( IMARR_COUNT(MAIN_imar) == 0 ){
     fprintf(stderr,"\n** NO IMAGES?\n") ; exit(1) ;
   }
   fprintf(stderr," = %d images\n",IMARR_COUNT(MAIN_imar)) ;

   shell = XtVaAppInitialize( &MAIN_app , "AFNI" , NULL , 0 ,
                              &argc , argv , FALLback , NULL ) ;

   if( shell == NULL ){
     fprintf(stderr,"** Can't initialize X11 ***\n") ; exit(1) ;
   }

   (void) XtAppSetWarningHandler(MAIN_app,AFNI_handler) ;

   MAIN_dc = MCW_new_DC( shell, 128,
                         DEFAULT_NCOLOVR, INIT_colovr, INIT_labovr, 1.0, 0 ) ;


   (void) XtAppAddTimeOut( MAIN_app, 234, timeout_CB, NULL ) ;
   XtAppMainLoop(MAIN_app) ;
}

/*-----------------------------------------------------------------------
   28 April 2000: Open an image sequence display window.
   The input image(s) are copied, so they can be destroyed after
   these calls.
      PLUTO_imseq_popim(im,kfunc,kdata)
         starts the image window off with a single image;
         return value is a "handle" that is used in other calls;
         if kfunc is not NULL, it is a function that will be called
         with argument kdata when the imseq window is closed
         (e.g., this can be used to set the saved handle to NULL)
      PLUTO_imseq_popup(imar,kfunc,kdata)
         starts the window off with an array of images
      PLUTO_imseq_retitle(handle,string)
         changes the window manager title
      PLUTO_imseq_rekill(handle,kfunc,kdata)
         changes the kill function/data to kfunc/kdata;
      PLUTO_imseq_addto(handle,im)
         adds the single image "im" to the display
      PLUTO_imseq_destroy(handle)
         pops down the image viewer;
         destroys the internal copies of the images;
         calls kfunc(kdata) if kfunc is not NULL
         (so do PLUTO_imseq_rekill(handle,NULL,NULL); before
                PLUTO_imseq_destroy(handle);
          if you don't want the kfunc to be called)
-------------------------------------------------------------------------*/

void * PLUTO_imseq_popim( MRI_IMAGE * im, generic_func * kfunc, void * kdata )
{
   MRI_IMARR * imar ;
   void * handle ;

   if( im == NULL ) return NULL ;
   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,im) ;
   handle = PLUTO_imseq_popup( imar,kfunc,kdata ) ;
   FREE_IMARR(imar) ;
   return handle ;
}

void * PLUTO_imseq_popup( MRI_IMARR * imar, generic_func * kfunc, void * kdata )
{
   int ntot , ii ;
   MRI_IMAGE * im , * cim ;
   PLUGIN_imseq * psq ;

   if( imar == NULL || IMARR_COUNT(imar) == 0 ) return NULL ;

   ntot = IMARR_COUNT(imar) ;

   psq = (PLUGIN_imseq *) calloc(1,sizeof(PLUGIN_imseq)) ;
   if( psq == NULL ) return NULL ;

   INIT_IMARR(psq->imar) ;
   psq->kill_func = kfunc ;
   psq->kill_data = kdata ;
   psq->rgb_count = 0 ;

   for( ii=0 ; ii < ntot ; ii++ ){
      im = IMARR_SUBIMAGE(imar,ii) ;
      if( im != NULL ){
         cim = mri_copy( im ) ;
         ADDTO_IMARR(psq->imar,cim) ;
         if( cim->kind == MRI_rgb ) psq->rgb_count++ ;
      }
   }
   ntot = IMARR_COUNT(psq->imar) ;
   if( ntot == 0 ){
      DESTROY_IMARR(psq->imar) ; free(psq) ; return NULL ;
   }

   psq->seq = open_MCW_imseq( MAIN_dc , PLUTO_imseq_getim , psq ) ;

   drive_MCW_imseq( psq->seq , isqDR_clearstat , NULL ) ;

   { ISQ_options opt ;       /* change some options from the defaults */

     ISQ_DEFAULT_OPT(opt) ;
     opt.save_one = False ;  /* change to Save:bkg */
     opt.save_pnm = False ;
     drive_MCW_imseq( psq->seq , isqDR_options      , (XtPointer) &opt ) ;
     drive_MCW_imseq( psq->seq , isqDR_periodicmont , (XtPointer) 0    ) ;
   }

   /* make it popup */

   drive_MCW_imseq( psq->seq , isqDR_realize, NULL ) ;
   drive_MCW_imseq( psq->seq , isqDR_title, "Images" ) ;

   if( ntot == 1 )
      drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_offwid ) ;
   else
      drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_onwid ) ;

   drive_MCW_imseq( psq->seq , isqDR_display, (XtPointer)0 ) ;

   return (void *) psq ;
}

/*-----------------------------------------------------------------------*/

void PLUTO_imseq_retitle( void * handle , char * title )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;

   if( psq == NULL || psq->seq == NULL || title == NULL ) return ;
   drive_MCW_imseq( psq->seq , isqDR_title, title ) ;
   return ;
}

/*-----------------------------------------------------------------------*/

void PLUTO_imseq_rekill( void * handle, generic_func * kfunc, void * kdata )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;

   if( psq == NULL ) return ;
   psq->kill_func = kfunc ;
   psq->kill_data = kdata ;
   return ;
}

/*-----------------------------------------------------------------------*/

void PLUTO_imseq_addto( void * handle , MRI_IMAGE * im )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;
   int ntot , ii ;
   MRI_IMAGE * cim ;

   if( psq == NULL || psq->seq == NULL || im == NULL ) return ;

   ntot = IMARR_COUNT(psq->imar) ;
   cim  = mri_copy(im) ;
   if( cim->kind == MRI_rgb ) psq->rgb_count++ ;
   ADDTO_IMARR(psq->imar,cim) ;

   drive_MCW_imseq( psq->seq , isqDR_newseq , psq ) ;

   if( ntot == 1 )
      drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_offwid ) ;
   else {
      drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_onwid ) ;
      drive_MCW_imseq( psq->seq , isqDR_opacitybut , (XtPointer) 0 ) ; /* 07 Mar 2001 */
      drive_MCW_imseq( psq->seq , isqDR_zoombut    , (XtPointer) 0 ) ; /* 12 Mar 2002 */
   }

   drive_MCW_imseq( psq->seq , isqDR_reimage , (XtPointer)(ntot) ) ;

   return ;
}

/*-----------------------------------------------------------------------*/

void PLUTO_imseq_destroy( void * handle )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;

   if( psq == NULL ) return ;
   drive_MCW_imseq( psq->seq , isqDR_destroy , NULL ) ;
   return ;
}

/*------------------------------------------------------------------
   Routine to provide data to the imseq.
   Just returns the control information, or the selected image.
--------------------------------------------------------------------*/

XtPointer PLUTO_imseq_getim( int n , int type , XtPointer handle )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;

   int ntot = 0 ;

   if( psq->imar != NULL ) ntot = IMARR_COUNT(psq->imar) ;
   if( ntot < 1 ) ntot = 1 ;

   /*--- send control info ---*/

   if( type == isqCR_getstatus ){
      MCW_imseq_status * stat = myXtNew( MCW_imseq_status ) ;  /* will be free-d */
                                                               /* when imseq is */
                                                               /* destroyed    */
      stat->num_total  = ntot ;
      stat->num_series = ntot ;
      stat->send_CB    = PLUTO_imseq_send_CB ;
      stat->parent     = NULL ;
      stat->aux        = NULL ;

      stat->transforms0D = NULL ;
      stat->transforms2D = NULL ;
      stat->slice_proj   = NULL ;

      return (XtPointer) stat ;
   }

   /*--- no overlay, never ---*/

   if( type == isqCR_getoverlay ) return NULL ;

   /*--- return a copy of an image
         (since the imseq will delete it when it is done) ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
      MRI_IMAGE * im = NULL , * rim ;

      if( psq->imar != NULL ){
         if( n < 0 ) n = 0 ; else if( n >= ntot ) n = ntot-1 ;
         rim = IMARR_SUBIMAGE(psq->imar,n) ;
         if( psq->rgb_count > 0 )
            im = mri_to_rgb( rim ) ;
         else
            im = mri_copy( rim ) ;
      }
      return (XtPointer) im ;
   }

   return NULL ; /* should not occur, but who knows? */
}

/*---------------------------------------------------------------------------
   Routine called when the imseq wants to send a message.
   In this case, all we need to handle is the destroy message,
   so that we can free some memory.
-----------------------------------------------------------------------------*/

void PLUTO_imseq_send_CB( MCW_imseq * seq , XtPointer handle , ISQ_cbs * cbs )
{
   PLUGIN_imseq * psq = (PLUGIN_imseq *) handle ;

   switch( cbs->reason ){
      case isqCR_destroy:{
         myXtFree(psq->seq) ;
         DESTROY_IMARR( psq->imar ) ;

         if( psq->kill_func != NULL )
            psq->kill_func( psq->kill_data ) ;

         free(psq) ;
      }
      break ;
   }
   return ;
}
