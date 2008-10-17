/*------------------------------------------------------------------------*/
/*--- AFNI Image Viewer - quicky adaptation from afni_plugin.c stuff. ----*/
/*------------------------------------------------------------------------*/

#include "mrilib.h"
#include "imseq.h"

/*------------------------------------------------------------------------*/

typedef struct {
   MCW_imseq *seq ;
   MRI_IMARR *imar ;
   int rgb_count ;
   generic_func *kill_func ;
   void *kill_data ;
} AIVVVV_imseq ;

static AIVVVV_imseq *psq_global ;
static XtAppContext AIVVV_appcontext ;

static NI_stream AIVVV_stream = (NI_stream)NULL ;
static char      AIVVV_strnam[64] ;
static int       AIVVV_have_dummy = 0 ;

static void * AIVVV_imseq_popup( MRI_IMARR *, generic_func *, void * ) ;
static void   AIVVV_imseq_retitle( void * , char * ) ;
static XtPointer AIVVV_imseq_getim( int , int , XtPointer ) ;
static void AIVVV_imseq_send_CB( MCW_imseq * , XtPointer , ISQ_cbs * ) ;
static void AIVVV_imseq_addto( MRI_IMAGE *im ) ;  /* 25 Jul 2005 */
static Boolean AIVVV_workproc( XtPointer ) ;      /* 25 Jul 2005 */
static void AIVVV_niml_quitter( char *, NI_stream , NI_element * ) ;

/*------------------------------------------------------------------------*/

static char *FALLback[] =
  {   "AFNI*fontList:              9x15bold=charset1"    ,
      "AFNI*pbar*fontList:         6x10=charset1"        ,
      "AFNI*imseq*fontList:        7x13=charset1"        ,
      "AFNI*background:            gray20"               ,
      "AFNI*menu*background:       gray10"               ,
      "AFNI*borderColor:           gray20"               ,
      "AFNI*foreground:            yellow"               ,
      "AFNI*borderWidth:           0"                    ,
      "AFNI*troughColor:           blue"                 ,
      "AFNI*XmLabel.translations:  #override<Btn2Down>:" , /* Motif 2.0 bug */
      "AFNI*help*background:       black"                ,
      "AFNI*help*foreground:       yellow"               ,
      "AFNI*help*helpborder:       False"                ,
      "AFNI*help*waitPeriod:       1066"                 ,
      "AFNI*help*fontList:         9x15bold=charset1"    ,
      "AFNI*cluefont:              9x15bold"             ,
      "AFNI*help*cancelWaitPeriod: 50"                   ,
      "AFNI*XmList.translations: #override"                /* 24 Feb 2007 */
           "<Btn4Down>: ListPrevItem()\\n"
           "<Btn5Down>: ListNextItem()"                  ,
      "AFNI*XmText.translations: #override"
           "<Btn4Down>: previous-line()\\n"
           "<Btn5Down>: next-line()"                     ,
      "AFNI*XmScrollBar.translations: #override"
           "<Btn4Down>: IncrementUpOrLeft(0) IncrementUpOrLeft(1)\\n"
           "<Btn5Down>: IncrementDownOrRight(1) IncrementDownOrRight(0)" ,
   NULL } ;

static MCW_DC       *MAIN_dc ;
static XtAppContext  MAIN_app ;
static MRI_IMARR    *MAIN_imar ;

#define DEFAULT_NCOLOVR 20

static char *INIT_colovr[DEFAULT_NCOLOVR] = {
   "#ffff00" , "#ffcc00"   , "#ff9900"  , "#ff6900" , "#ff4400" , "#ff0000" ,
   "#0000ff" , "#0044ff"   , "#0069ff"  , "#0099ff" , "#00ccff" , "#00ffff" ,
   "green"   , "limegreen" , "violet"   , "hotpink" ,
   "white"   , "#dddddd"   , "#bbbbbb"  , "black"
} ;

static char *INIT_labovr[DEFAULT_NCOLOVR] = {
   "yellow" , "yell-oran" , "oran-yell" , "orange"   , "oran-red" , "red"   ,
   "dk-blue", "blue"      , "lt-blue1"  , "lt-blue2" , "blue-cyan", "cyan"  ,
   "green"  , "limegreen" , "violet"    , "hotpink"  ,
   "white"  , "gry-dd"    , "gry-bb"    , "black"
} ;

static char wintit[256] = { "Images" } ;
/*------------------------------------------------------------------------*/

static void killer( void *pt ){ exit(0); }
static void AFNI_handler(char *msg){ return ; } /* hide X11 warnings */

/*------------------------------------------------------------------------*/
/*! Called to start up display, after X11 has had time to get going. */

static void timeout_CB( XtPointer client_data , XtIntervalId *id )
{
ENTRY("timeout_CB") ;
   (void) AIVVV_imseq_popup( MAIN_imar , killer , NULL ) ;
   if( AIVVV_stream != (NI_stream)NULL ){
     XtAppAddWorkProc( AIVVV_appcontext, AIVVV_workproc, NULL ) ;
     NI_register_doer( "QUIT" , AIVVV_niml_quitter ) ;
     NI_register_doer( "EXIT" , AIVVV_niml_quitter ) ;
   }
   EXRETURN ;
}

/*------------------------------------------------------------------------*/

int main( int argc , char *argv[] )
{
   int ii , quiet = 0, verb=0 , iarg=1 , jj ;
   MRI_IMAGE *im ;     /* 1 input image */
   MRI_IMARR *qar ;    /* all input images */
   Widget shell ;
   int gnim ; char **gname=NULL ;   /* 23 Dec 2002: glob filenames */

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
     printf(
      "Usage: aiv [-v] [-q] [-title] [-p xxxx ] image ...\n"
      "AFNI Image Viewer program.\n"
      "Shows the 2D images on the command line in an AFNI-like image viewer.\n"
      "Can also read images in NIML '<MRI_IMAGE...>' format from a TCP/IP socket.\n"
      "Image file formats are those supported by to3d:\n"
      " * various MRI formats (e.g., DICOM, GEMS I.xxx)\n"
      " * raw PPM or PGM\n"
      " * JPEG (if djpeg is in the path)\n"
      " * GIF, TIFF, BMP, and PNG (if netpbm is in the path)\n"
      "\n"
      "The '-v' option will make aiv print out the image filenames\n"
      "as it reads them - this can be a useful progress meter if\n"
      "the program starts up slowly.\n"
      "\n"
      "The '-q' option tells the program to be very quiet.\n"
      "\n"
      "The '-title WORD' option titles the window WORD. \n"
      "The default is the name of the image file if only one is \n"
      "specified on the command line. If many images are read in\n"
      "the default window title is 'Images'.\n"
      "The '-p xxxx' option will make aiv listen to TCP/IP port 'xxxx'\n"
      "for incoming images in the NIML '<MRI_IMAGE...>' format.  The\n"
      "port number must be between 1024 and 65535, inclusive.  For\n"
      "conversion to NIML '<MRI_IMAGE...>' format, see program im2niml.\n"
      "\n"
      "Normally, at least one image must be given on the command line.\n"
      "If the '-p xxxx' option is used, then you don't have to input\n"
      "any images this way; however, since the program requires at least\n"
      "one image to start up, a crude 'X' will be displayed.  When the\n"
      "first image arrives via the socket, the 'X' image will be replaced.\n"
      "Subsequent images arriving by socket will be added to the sequence.\n"
      "\n-----------------------------------------------------------------\n"
      "Sample program fragment, for sending images from one program\n"
      "into a copy of aiv (which that program also starts up):\n"
      "\n"
      "#include \"mrilib.h\"\n"
      "NI_stream ns; MRI_IMAGE *im; float *far; int nx,ny;\n"
      "system(\"aiv -p 4444 &\");                               /* start aiv */\n"
      "ns = NI_stream_open( \"tcp:localhost:4444\" , \"w\" ); /* connect to it */\n"
      "while(1){\n"
      "  /** ......... create 2D nx X ny data into the far array .........**/\n"
      "  im = mri_new_vol_empty( nx , ny , 1 , MRI_float );  /* fake image */\n"
      "  mri_fix_data_pointer( far , im );                  /* attach data */\n"
      "  NI_element nel = mri_to_niml(im);      /* convert to NIML element */\n"
      "  NI_write_element( ns , nel , NI_BINARY_MODE );     /* send to aiv */\n"
      "  NI_free_element(nel); mri_clear_data_pointer(im); mri_free(im);\n"
      "}\n"
      "NI_stream_writestring( ns , \"<ni_do ni_verb='QUIT'>\" ) ;\n"
      "NI_stream_close( ns ) ;  /* do this, or the above, if done with aiv */\n"
      "\n"
      "-- Author: RW Cox\n"
     ) ;
     PRINT_COMPILE_DATE ; exit(0) ;
   }

   mainENTRY("aiv main") ; machdep() ;

   /* options? */

   while( iarg < argc && argv[iarg][0] == '-' ){
     /*-- title --*/
     if( strncmp(argv[iarg],"-title",6) == 0 ){
         if (iarg+1 > argc) {
            ERROR_message("Need a string after -title");
            /* ignore option */
         } else {
            snprintf(wintit,128*sizeof(char),"%s", argv[++iarg]); 
         }
         iarg++; continue; 
     } 
     /*-- verbosity --*/

     if( strncmp(argv[iarg],"-v",2) == 0 ){ verb=1; iarg++; continue; }
     if( strncmp(argv[iarg],"-q",2) == 0 ){ quiet=1; iarg++; continue; }
      
     /*-- port or sherry? --*/

     if( strncmp(argv[iarg],"-p",2) == 0 ){
       int port = (int)strtol(argv[++iarg],NULL,10) ;
       if( AIVVV_stream != NULL ){
         ERROR_message("Can't use multiple '-p' options!") ;
         iarg++ ; continue ;   /* skip to next option */
       }
       if( port <= 1023 ){
         ERROR_message("Illegal value after -p; not listening.") ;
       } else {
         sprintf(AIVVV_strnam,"tcp:x:%d",port) ;
         AIVVV_stream = NI_stream_open( AIVVV_strnam , "r" ) ;
         if( AIVVV_stream == (NI_stream)NULL ){
           ERROR_message("Can't listen to port %d!",port) ;
         } else {
           int nn ;
           nn = NI_stream_goodcheck(AIVVV_stream,66) ;
           if( verb ){
             if( nn > 0 ) INFO_message("Connected to port %d",port) ;
             else         INFO_message("Listening to port %d",port) ;
           }
         }
       }
       iarg++ ; continue ;
     }

     /*-- WTF? --*/

     ERROR_message("Unknown option: %s",argv[iarg]) ;
   }
   if (!quiet) { PRINT_VERSION("aiv") ; } 
   

   /* glob filenames, read images */

   MCW_file_expand( argc-iarg , argv+iarg , &gnim , &gname ) ;
   if( gnim == 0 && AIVVV_stream==(NI_stream)NULL )
     ERROR_exit("No filenames on command line (after wildcard expansion)?!") ;

   INIT_IMARR(MAIN_imar) ;

   for( ii=0 ; ii < gnim ; ii++ ){
     if( !THD_filename_ok(gname[ii]) ) continue ;  /* 23 Apr 2003 */
     if (!strcmp(wintit,"Images") && gnim == 1) { /* give window a useful name */
         snprintf(wintit,128*sizeof(char),"%s", gname[ii]);
     }
     if( verb ) fprintf(stderr,"+") ;
     qar = mri_read_file( gname[ii] ) ;  /* may have more than 1 2D image */
     if( qar == NULL || IMARR_COUNT(qar) < 1 ){
       if( verb )
         fprintf(stderr,"\n** Can't read file %s - skipping!",gname[ii]) ;
       else
         fprintf(stderr,"** AIV ERROR: Can't read file %s - skipping!\n",gname[ii]) ;
       continue ;
     } else if( verb ){
       fprintf(stderr,"%s",gname[ii]) ;
     }

     if( IMARR_COUNT(qar) == 1 ){  /* possibly a 3D dataset from AFNI */
       im = IMARR_SUBIM(qar,0) ;
       if( im != NULL && im->nz > 1 ){  /* break 3D array into 2D images */
         MRI_IMARR *zar = mri_to_imarr(im) ;
         if( zar != NULL ){ DESTROY_IMARR(qar) ; qar = zar ; }
       }
     }

     for( jj=0 ; jj < IMARR_COUNT(qar) ; jj++ ){
       im = IMARR_SUBIM(qar,jj) ;
       if( im != NULL && im->nx > 1 && im->ny > 1 ) ADDTO_IMARR(MAIN_imar,im);
     }
     FREE_IMARR(qar) ;  /* just FREE, not DESTROY */
   }

   /* print a message about the images? */

   if( IMARR_COUNT(MAIN_imar) == 0 && AIVVV_stream==(NI_stream)NULL )
     ERROR_exit("No images found on command line!?") ;
   if( IMARR_COUNT(MAIN_imar) > 0 ){
     if (!quiet) fprintf(stderr, (verb) ? " = " : "++ " ) ;
     if( IMARR_COUNT(MAIN_imar) == 1 )
       if (!quiet) fprintf(stderr,"1 image\n") ;
     else
       if (!quiet) fprintf(stderr,"%d images\n",IMARR_COUNT(MAIN_imar)) ;
   }

   if( gnim > 0 ) MCW_free_expand( gnim , gname ) ;

   /* connect to X11 */

   shell = XtVaAppInitialize( &MAIN_app , "AFNI" , NULL , 0 ,
                              &argc , argv , FALLback , NULL ) ;

   if( shell == NULL )
     ERROR_exit("Can't initialize X11") ;

   AIVVV_appcontext = XtWidgetToApplicationContext(shell) ;

   (void) XtAppSetWarningHandler(MAIN_app,AFNI_handler) ;

   MAIN_dc = MCW_new_DC( shell, 128,
                         DEFAULT_NCOLOVR, INIT_colovr, INIT_labovr, 1.0, 0 ) ;

   srand48((long)time(NULL)) ;

   /* wait a little bit, then popup the image viewer window */

   (void) XtAppAddTimeOut( MAIN_app, 234, timeout_CB, NULL ) ;
   XtAppMainLoop(MAIN_app) ;  /* will never return */
   exit(0) ;
}

/*-------------------------------------------------------------------------*/
/*! Open the image viewer. */

static void * AIVVV_imseq_popup( MRI_IMARR *imar, generic_func *kfunc, void *kdata )
{
   int ntot , ii ;
   MRI_IMAGE *im , *cim ;
   AIVVVV_imseq *psq ;

ENTRY("AIVVV_imseq_popup") ;

   if( imar == NULL ) RETURN(NULL) ;

   ntot = IMARR_COUNT(imar) ;
   if( ntot == 0 ){               /** dummy 'X' image **/
#define QQ_NXYZ 16
     static byte xxx[QQ_NXYZ*QQ_NXYZ] = {
       0,0,21,131,135,135,135,8,0,0,3,100,135,135,135,128,
       0,0,0,108,255,255,255,86,0,0,115,255,255,255,255,121,
       0,0,0,21,216,255,255,213,0,19,223,255,255,255,187,5,
       0,0,0,0,92,244,255,255,114,114,255,255,255,234,58,0,
       0,0,0,0,0,174,255,255,252,230,255,255,255,130,0,0,
       0,0,0,0,0,58,244,255,255,255,255,255,228,29,0,0,
       0,0,0,0,0,0,118,255,255,255,255,255,74,0,0,0,
       0,0,0,0,0,0,55,248,255,255,255,199,3,0,0,0,
       0,0,0,0,0,5,170,255,255,255,255,227,32,0,0,0,
       0,0,0,0,0,104,255,255,255,255,255,255,140,5,0,0,
       0,0,0,0,13,217,255,255,252,215,255,255,255,67,0,0,
       0,0,0,0,159,255,255,255,212,23,233,255,255,187,7,0,
       0,0,0,81,241,255,255,255,85,0,72,255,255,255,66,0,
       0,0,16,206,255,255,255,212,0,0,8,193,255,255,237,12,
       0,0,94,255,255,255,255,86,0,0,0,73,255,255,255,121,
       0,14,129,134,134,134,85,1,0,0,0,3,106,134,134,127 } ;
     byte *ar ; MRI_IMAGE *xim ;

     ar = (byte *)malloc(sizeof(byte)*QQ_NXYZ*QQ_NXYZ) ;
     memcpy(ar,xxx,sizeof(byte)*QQ_NXYZ*QQ_NXYZ) ;
     xim = mri_new_vol_empty( QQ_NXYZ,QQ_NXYZ,1 , MRI_byte ) ;
     mri_fix_data_pointer( ar , xim ) ;
     xim->dx = xim->dy = 16.0 ;
     ADDTO_IMARR(imar,xim) ; ntot = 1 ; AIVVV_have_dummy = 1 ;
   }

   /* psq holds all the data needed for viewing */

   psq = psq_global = (AIVVVV_imseq *)calloc(1,sizeof(AIVVVV_imseq)) ;
   if( psq == NULL ) RETURN(NULL) ;  /* should never happen */

   psq->imar = imar ;

   psq->kill_func = kfunc ;
   psq->kill_data = kdata ;
   psq->rgb_count = 0 ;

   /* actually create the viewer */

   psq->seq = open_MCW_imseq( MAIN_dc , AIVVV_imseq_getim , psq ) ;

   drive_MCW_imseq( psq->seq , isqDR_clearstat , NULL ) ;

   { ISQ_options opt ;       /* change some options from the defaults */

     ISQ_DEFAULT_OPT(opt) ;
     opt.save_one = False ;  /* change to Save:bkg */
     opt.save_pnm = False ;
     drive_MCW_imseq( psq->seq , isqDR_options      , (XtPointer) &opt ) ;
     drive_MCW_imseq( psq->seq , isqDR_periodicmont , (XtPointer) 0    ) ;
     drive_MCW_imseq( psq->seq , isqDR_penbbox      , (XtPointer) 0    ) ;
   }

   /* make it popup */

   drive_MCW_imseq( psq->seq , isqDR_realize, NULL ) ;
   drive_MCW_imseq( psq->seq , isqDR_title, wintit ) ;

   if( ntot == 1 )
     drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_offwid );
   else
     drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_onwid  );

   /* show the first image */

   drive_MCW_imseq( psq->seq , isqDR_display, (XtPointer)0 ) ;

   drive_MCW_imseq( psq->seq , isqDR_imhelptext ,
                    (XtPointer)
                     "Keyboard Shortcuts:\n\n"
                     "q = close window         a = fix aspect ratio\n"
                     "p = toggle panning mode  c = crop image mode\n"
                     "s = sharpen image        m = toggle Min-to-Max\n"
                     "D = open Disp panel      M = open Montage panel\n"
                     "S = Save image           l = left-right mirror\n"
                     "> = Page Up   = forward  1 image\n"
                     "< = Page Down = backward 1 image\n"
                     "v/V = Video image sequence up/down\n"
                     "r/R = Ricochet image sequence up/down\n"
                     "i/I = image fraction down/up\n"
                   ) ;

   RETURN( (void *)psq ) ;
}

/*-----------------------------------------------------------------------*/

static void AIVVV_imseq_retitle( void *handle , char *title )
{
   AIVVVV_imseq *psq = (AIVVVV_imseq *) handle ;

   if( psq == NULL || psq->seq == NULL || title == NULL ) return ;
   drive_MCW_imseq( psq->seq , isqDR_title, title ) ;
   return ;
}

/*------------------------------------------------------------------
   Routine to provide data to the imseq.
   Just returns the control information, or the selected image.
--------------------------------------------------------------------*/

static XtPointer AIVVV_imseq_getim( int n, int type, XtPointer handle )
{
   AIVVVV_imseq *psq = (AIVVVV_imseq *) handle ;
   int ntot = 0 ;

ENTRY("AIVVV_imseq_getim") ;

   if( psq->imar != NULL ) ntot = IMARR_COUNT(psq->imar) ;
   if( ntot < 1 ) ntot = 1 ;

   /*--- send control info ---*/

   if( type == isqCR_getstatus ){
     MCW_imseq_status *stat = myXtNew( MCW_imseq_status ) ; /* will be freed */
                                                            /* when imseq is */
                                                            /* destroyed    */
     stat->num_total  = ntot ;
     stat->num_series = ntot ;
     stat->send_CB    = AIVVV_imseq_send_CB ;
     stat->parent     = NULL ;
     stat->aux        = NULL ;

     stat->transforms0D = NULL ;
     stat->transforms2D = NULL ;
     stat->slice_proj   = NULL ;

     RETURN( (XtPointer)stat ) ;
   }

   /*--- return a copy of an image
         (since the imseq will delete it when it is done) ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
     MRI_IMAGE *im = NULL , *rim ;

     if( psq->imar != NULL ){
       if( n < 0 ) n = 0 ; else if( n >= ntot ) n = ntot-1 ;
       rim = IMARR_SUBIMAGE(psq->imar,n) ;
       if( psq->rgb_count > 0 )
         im = mri_to_rgb( rim ) ;
       else
         im = mri_copy( rim ) ;
     }
     RETURN( (XtPointer)im ) ;
   }

   RETURN(NULL) ; /* any other request gets nothing */
}

/*---------------------------------------------------------------------------
   Routine called when the imseq wants to send a message.
   In this case, all we need to handle is the destroy message,
   so that we can free some memory.
-----------------------------------------------------------------------------*/

static void AIVVV_imseq_send_CB( MCW_imseq *seq, XtPointer handle, ISQ_cbs *cbs )
{
   AIVVVV_imseq *psq = (AIVVVV_imseq *) handle ;

ENTRY("AIVVV_imseq_send_CB") ;

   switch( cbs->reason ){
     case isqCR_destroy:{
       myXtFree(psq->seq) ;
       DESTROY_IMARR( psq->imar ) ;

       if( psq->kill_func != NULL )
         psq->kill_func( psq->kill_data ) ;
         free(psq) ;
     }
     break ;

     case isqCR_newimage:{
       drive_MCW_imseq( psq->seq, isqDR_display, (XtPointer)cbs->nim );
     }
     break ;
   }
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Add an image to the display sequence. */

static void AIVVV_imseq_addto( MRI_IMAGE *im )
{
   int ntot , num ;
   AIVVVV_imseq *psq = psq_global ;

ENTRY("AIVVV_imseq_addto") ;

   if( im == NULL ) EXRETURN ;

   if( im->nx < 4 || im->ny < 4 ) EXRETURN ;

   /** if more than 1 slice, carve up the volume
       into multiple 2D slices and recursively add each one **/

   num = im->nz * im->nt * im->nu * im->nv * im->nw ;
   if( num > 1 ){
     MRI_IMAGE *qim ; int kk,nb ; char *iar=(char *)mri_data_pointer(im) ;
     nb = im->nx * im->ny * im->pixel_size ;
     for( kk=0 ; kk < num ; kk++ ){
       qim = mri_new_vol_empty( im->nx , im->ny , 1, im->kind ) ;
       qim->dx = im->dx ; qim->dy = im->dy ;
       mri_fix_data_pointer( iar + kk*nb , qim ) ;
       AIVVV_imseq_addto( qim ) ;
     }
     EXRETURN ;
   }

   if( AIVVV_have_dummy ){             /* replace the dummy 'X' */
     IMARR_SUBIM(psq->imar,0) = im ;
     AIVVV_have_dummy = 0 ;
   } else {
     ADDTO_IMARR(psq->imar,im) ;       /* add to sequence */
   }
   if( im->kind == MRI_rgb ) psq->rgb_count++ ;

   drive_MCW_imseq( psq->seq , isqDR_newseq , psq ) ;

   ntot = IMARR_COUNT(psq->imar) ;
   if( ntot == 2 )
     drive_MCW_imseq( psq->seq , isqDR_onoffwid , (XtPointer) isqDR_onwid ) ;

   drive_MCW_imseq( psq->seq , isqDR_reimage , (XtPointer)(ntot-1) ) ;
   EXRETURN ;
}

/*---------------------------------------------------------------------------*/
/* Listen to the socket and see if any new <MRI_IMAGE ...> NIML elements
   appear.
-----------------------------------------------------------------------------*/

static Boolean AIVVV_workproc( XtPointer fred )
{
   int nn ;
   NI_element *nel ;
   MRI_IMAGE *im ;

   nn = NI_stream_goodcheck(AIVVV_stream,3) ;
   if( nn < 0 ){                              /* dead? reopen it */
     NI_stream_closenow(AIVVV_stream) ;
     NI_sleep(9) ;
     AIVVV_stream = NI_stream_open( AIVVV_strnam , "r" ) ;
     return False ;
   }

   nn = NI_stream_hasinput(AIVVV_stream,9) ;   /* anything? */
   if( nn <= 0 ) return False ;                /* no data */

   /* read data, add image */

   nel = (NI_element *)NI_read_element(AIVVV_stream,99) ;
   if( NI_element_type(nel) != NI_ELEMENT_TYPE ){  /* bad read */
     NI_free_element(nel) ; return False ;
   }

   /* the only type of element we deal with is MRI_IMAGE */

   im = niml_to_mri( nel ) ;   /* convert element to image */
   NI_free_element( nel ) ;
   AIVVV_imseq_addto( im ) ;   /* add image to the display sequence */
   return False ;
}

/*---------------------------------------------------------------------------*/

static void AIVVV_niml_quitter( char *obj, NI_stream ns, NI_element *nel )
{
   INFO_message("Received remote command to exit") ;
   NI_stream_closenow(AIVVV_stream) ;
   NI_sleep(333) ;
   exit(0) ;
}
