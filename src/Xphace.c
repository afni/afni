#include <Xm/XmAll.h>

#include "mrilib.h"
#include "imseq.h"

static XtAppContext   MAIN_app ;
static MCW_DC *       MAIN_dc ;
static Widget         MAIN_shell=NULL ;
static Widget         MAIN_rc ;
static Widget         MAGN_scale , PHASE_scale ;

#define FIX_SCALE_SIZE_PROBLEM

static int P_swide = 512 ;
#ifdef FIX_SCALE_SIZE_PROBLEM
#  define FIX_SCALE_SIZE                                    \
     do{ XtVaSetValues(MAGN_scale ,XmNwidth,P_swide,NULL) ; \
         XtVaSetValues(PHASE_scale,XmNwidth,P_swide,NULL) ; } while(0)
#else
#  define FIX_SCALE_SIZE /* nada */
#endif

/*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_fft2D( MRI_IMAGE * im , int mode )
{
   MRI_IMAGE * cxim , * outim ;
   int nx,ny , nxup,nyup , ii,jj ;
   complex * cxar , * outar , * cpt , * opt ;
   float fac ;

   if( im == NULL ) return NULL ;

   /* convert input to complex */

   cxim = mri_to_complex(im) ;
   cxar = MRI_COMPLEX_PTR(cxim) ;

   /* compute size of output */

   nx = cxim->nx ; nxup = csfft_nextup_one35(nx) ;
   ny = cxim->ny ; nyup = csfft_nextup_one35(ny) ;

   /* create output array */

   outim = mri_new( nxup , nyup , MRI_complex ) ;
   outar = MRI_COMPLEX_PTR(outim) ;

   /* copy input to output, zero padding along the way */

   opt = outar ;
   cpt = cxar  ;
   for( jj=0 ; jj < ny ; jj++ ){
      for( ii=0 ; ii < nx   ; ii++ ){opt->r=cpt->r; opt->i=cpt->i; opt++; cpt++;}
      for(      ; ii < nxup ; ii++ ){opt->r=opt->i=0.0; opt++;}
   }
   for( ; jj < nyup ; jj++ ){opt->r=opt->i=0.0; opt++;}

   mri_free(cxim) ;

   /* row FFTs */

   for( jj=0 ; jj < ny ; jj++ )
      csfft_cox( mode , nxup , outar+jj*nxup ) ;

   /* column FFTs */

   cxar = (complex *) malloc(sizeof(complex)*nyup) ;

   for( ii=0 ; ii < nxup ; ii++ ){
      for( jj=0 ; jj < nyup ; jj++ ) cxar[jj] = outar[ii+jj*nxup] ;
      csfft_cox( mode , nyup , cxar ) ;
      for( jj=0 ; jj < nyup ; jj++ ) outar[ii+jj*nxup] = cxar[jj] ;
   }

   fac = sqrt(1.0/(nxup*nyup)) ;
   for( ii=0 ; ii < nxup*nyup ; ii++ ){
      outar[ii].r *= fac ; outar[ii].i *= fac ;
   }

   free(cxar) ; return outim ;
}

/*--------------------------------------------------------------------------*/

MRI_IMAGE * cx_scramble( MRI_IMAGE * ima , MRI_IMAGE * imb ,
                         float alpha     , float beta       )
{
   int ii , npix ;
   double r1,r2 , t1,t2 , rr,tt ;
   complex * ar , * br , * cr ;
   double aa,aa1 , bb,bb1 ;
   MRI_IMAGE * imc ;

   if( ima == NULL        || ima->kind != MRI_complex ||
       imb == NULL        || imb->kind != MRI_complex ||
       ima->nx != imb->nx || ima->ny != imb->ny       ||
       alpha < 0.0        || alpha > 1.0              ||
       beta  < 0.0        || beta  > 1.0                ) return NULL ;

   npix = ima->nvox ;
   ar   = MRI_COMPLEX_PTR(ima) ;
   br   = MRI_COMPLEX_PTR(imb) ;
   imc  = mri_new_conforming( ima , MRI_complex ) ;
   cr   = MRI_COMPLEX_PTR(imc) ;

   aa   = alpha ; aa1 = 1.0 - aa ;
   bb   = beta  ; bb1 = 1.0 - bb ;

   for( ii=0 ; ii < npix ; ii++ ){
      r1 = CABS(ar[ii]) ; r2 = CABS(br[ii]) ; rr = pow(r1,aa)*pow(r2,aa1) ;
      t1 = CARG(ar[ii]) ; t2 = CARG(br[ii]) ; tt = t1-t2 ;
           if( tt < -PI ) t2 -= 2.0*PI ;
      else if( tt >  PI ) t2 += 2.0*PI ;
      tt = bb*t1 + bb1*t2 ;
      cr[ii].r = rr * cos(tt) ; cr[ii].i = rr * sin(tt) ;
   }

   return imc ;
}

/*--------------------------------------------------------------------------*/

MRI_IMAGE * mri_scramble( MRI_IMAGE * ima , MRI_IMAGE * imb ,
                          float alpha     , float beta       )
{
   MRI_IMAGE * cxa, * cxb, * cxc ;
   int nx,ny , nxup,nyup ;

   if( ima == NULL        || imb == NULL        ||
       ima->nx != imb->nx || ima->ny != imb->ny ||
       alpha < 0.0        || alpha > 1.0        ||
       beta  < 0.0        || beta  > 1.0          ) return NULL ;

   cxa = mri_fft2D( ima , -1 ) ;
   cxb = mri_fft2D( imb , -1 ) ;
   cxc = cx_scramble( cxa,cxb,alpha,beta ) ;
   mri_free(cxa) ; mri_free(cxb) ;
   cxa = mri_fft2D( cxc , 1 ) ;
   mri_free(cxc) ;
   cxb = mri_to_mri( ima->kind , cxa ) ;
   mri_free(cxa) ;

   if( cxb->nx > ima->nx || cxb->ny > ima->ny ){
      cxc = mri_cut_2D( cxb , 0,ima->nx-1,0,ima->ny-1 ) ;
      mri_free(cxb) ; cxb = cxc ;
   }

   return cxb ;
}
/*--------------------------------------------------------------------------*/

typedef struct {
   MCW_imseq * seq ;
   MRI_IMAGE * im ;
} PLUGIN_impopper ;

static void * P_handle = NULL ;

#define PLUTO_popup_open(hh) \
   ( (hh) != NULL && ISQ_REALZ(((PLUGIN_impopper *)(hh))->seq) )

/*---------------------------------------------------------------------------
   Routine called when the imseq wants to send a message.
   In this case, all we need to handle is the destroy message,
   so that we can free some memory.
-----------------------------------------------------------------------------*/

void PLUGIN_seq_send_CB( MCW_imseq * seq , XtPointer handle , ISQ_cbs * cbs )
{
   PLUGIN_impopper * imp = (PLUGIN_impopper *) handle ;

   if( imp == NULL ) return ;

   switch( cbs->reason ){

      case isqCR_destroy:{
         XtFree((char*)imp->seq->status) ;
         XtFree((char*)imp->seq)         ; imp->seq = NULL ;
         mri_free( imp->im )             ; imp->im  = NULL ;
      }
      break ;
   }
   return ;
}

/*------------------------------------------------------------------
   Routine to provide data to the imseq for PLUGIN_popup_image.
   Just returns the control information, or the given image.
--------------------------------------------------------------------*/

XtPointer PLUGIN_imseq_getim( int n , int type , XtPointer handle )
{
   PLUGIN_impopper * imp = (PLUGIN_impopper *) handle ;

   if( imp == NULL ) return(NULL) ;

   /*--- control info ---*/

   if( type == isqCR_getstatus ){
      MCW_imseq_status * stat = XtNew( MCW_imseq_status ) ;
      stat->num_total  = 1 ;
      stat->num_series = 1 ;
      stat->send_CB    = PLUGIN_seq_send_CB ;
      stat->parent     = (XtPointer) imp  ;
      stat->aux        = NULL ;

      stat->transforms0D = NULL ;
      stat->transforms2D = NULL ;
      stat->slice_proj   = NULL ;

      return((XtPointer) stat) ;
   }

   /*--- no overlay ---*/

   if( type == isqCR_getoverlay ) return(NULL) ;

   /*--- return a copy of the image
         (since the imseq will delete it when it is done) ---*/

   if( type == isqCR_getimage || type == isqCR_getqimage ){
      MRI_IMAGE * im = NULL ;
      if( imp->im != NULL ) im = mri_to_mri( imp->im->kind , imp->im ) ;
      return((XtPointer) im) ;
   }

   return(NULL) ;  /* should not occur, but who knows? */
}

/*-----------------------------------------------------------------------
  The following is adapted from PLUTO_popup_image() in afni_plugin.c
-------------------------------------------------------------------------*/

static void * PH_popup_image( void * handle , MRI_IMAGE * im )
{
   PLUGIN_impopper * imp = (PLUGIN_impopper *) handle ;

   /*-- input image is NULL ==> popdown, if applicable --*/

   if( im == NULL ){
      if( imp != NULL )
         drive_MCW_imseq( imp->seq , isqDR_destroy , NULL ) ;

      return ((void *) imp) ;
   }

   /*-- input = no popper handle ==> create one --*/

   if( imp == NULL ){
      imp      = XtNew(PLUGIN_impopper) ;
      imp->seq = NULL ; imp->im  = NULL ;
   }

   /*-- input = non-null image ==> replace image --*/

   mri_free( imp->im ) ;                   /* toss old copy */
   imp->im = mri_to_mri( im->kind , im ) ; /* make new copy */

   /*-- input = inactive popper handle ==> activate it --*/

   if( imp->seq == NULL ){
      imp->seq = open_MCW_imseq( MAIN_dc ,
                                 PLUGIN_imseq_getim , (XtPointer) imp ) ;

      drive_MCW_imseq( imp->seq , isqDR_realize, NULL ) ;
      drive_MCW_imseq( imp->seq , isqDR_onoffwid , (XtPointer) isqDR_offwid ) ;

      XtVaSetValues( imp->seq->wtop ,
                       XmNmwmDecorations , MWM_DECOR_BORDER | MWM_DECOR_TITLE | MWM_DECOR_MENU ,
                       XmNmwmFunctions   , MWM_FUNC_MOVE | MWM_FUNC_CLOSE ,
                       XmNtitle          , "Xphace Images" ,
                     NULL ) ;

   }

   drive_MCW_imseq( imp->seq , isqDR_clearstat , NULL ) ;
   drive_MCW_imseq( imp->seq , isqDR_reimage , (XtPointer) 0 ) ;

   return ((void *) imp) ;
}

/*--------------------------------------------------------------------------*/

static char * FALLback[] =
  {   "AFNI*fontList:             9x15bold=charset1"    ,
      "AFNI*background:           gray40"               ,
      "AFNI*menu*background:      gray40"               ,
      "AFNI*borderColor:          gray40"               ,
      "AFNI*foreground:           yellow"               ,
      "AFNI*borderWidth:          0"                    ,
      "AFNI*troughColor:          green"                ,
      "AFNI*XmLabel.translations: #override<Btn2Down>:" , /* Motif 2.0 bug */
   NULL } ;

#ifndef LABEL_ARG
#define LABEL_ARG(str) \
  XtVaTypedArg , XmNlabelString , XmRString , (str) , strlen(str)+1
#endif

void PH_scale_CB(Widget,XtPointer,XtPointer) ;
void PH_redraw(void) ;
int  PH_loadim(char *) ;
void PH_startup_timeout_CB( XtPointer client_data , XtIntervalId * id ) ;

/*--------------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   Widget rc , lab ;
   int ww , uu ;

   if( argc < 2 || strcmp(argv[1],"-help") == 0 ){
      printf("Usage: Xphace im1 [im2]\n"
             "Image mergerizing.\n"
             "Image files are in PGM format.\n") ; exit(0) ;
   }

   ww = PH_loadim( argv[1] ) ;
   if( ww < 0 ) exit(1) ;
   if( argc > 2 ){
      ww = PH_loadim( argv[2] ) ;
      if( ww < 0 ) exit(1) ;
   } else {
      PH_loadim( "noise=1" ) ;
   }

   MAIN_shell = XtVaAppInitialize( &MAIN_app , "AFNI" , NULL , 0 ,
                                   &argc , argv , FALLback , NULL ) ;

   if( MAIN_shell == NULL ){
      fprintf(stderr,"\n*** Cannot initialize X11 ***\n") ; exit(1) ;
   }

   MAIN_dc = MCW_new_DC( MAIN_shell , 32 , 0 , NULL,NULL , 1.0 , 0 ) ;

   XtVaSetValues( XmGetXmDisplay(XtDisplay(MAIN_shell)) ,
                    XmNdragInitiatorProtocolStyle , XmDRAG_NONE ,
                    XmNdragReceiverProtocolStyle  , XmDRAG_NONE ,
                  NULL ) ;

   MAIN_rc = XtVaCreateWidget( "AFNI" , xmRowColumnWidgetClass , MAIN_shell ,
                                 XmNpacking     , XmPACK_TIGHT ,
                                 XmNorientation , XmVERTICAL   ,
                                 XmNtraversalOn , False ,
                               NULL ) ;

   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

   rc = XtVaCreateWidget( "AFNI" , xmRowColumnWidgetClass , MAIN_rc ,
                            XmNpacking     , XmPACK_TIGHT ,
                            XmNorientation , XmHORIZONTAL ,
                            XmNtraversalOn , False ,
                          NULL ) ;

   lab = XtVaCreateManagedWidget( "AFNI" , xmLabelWidgetClass , rc ,
                                    LABEL_ARG( "Magn. " ) ,
                                    XmNmarginHeight, 0 ,
                                    XmNmarginWidth , 0 ,
                                  NULL ) ;

   MAGN_scale = XtVaCreateManagedWidget( "AFNI" , xmScaleWidgetClass , rc ,
                                            XmNminimum       , 0 ,
                                            XmNmaximum       , 100 ,
                                            XmNvalue         , 0 ,
                                            XmNwidth         , P_swide ,
                                            XmNshowValue     , True ,
                                            XmNscaleMultiple , 5 ,
                                            XmNorientation   , XmHORIZONTAL ,
                                            XmNtraversalOn , False ,
                                         NULL ) ;

   XtAddCallback( MAGN_scale , XmNvalueChangedCallback , PH_scale_CB , NULL ) ;
   XtManageChild( rc ) ;

   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

   rc = XtVaCreateWidget( "AFNI" , xmRowColumnWidgetClass , MAIN_rc ,
                            XmNpacking     , XmPACK_TIGHT ,
                            XmNorientation , XmHORIZONTAL ,
                            XmNtraversalOn , False ,
                          NULL ) ;

   lab = XtVaCreateManagedWidget( "AFNI" , xmLabelWidgetClass , rc ,
                                    LABEL_ARG( "Phase " ) ,
                                    XmNmarginHeight, 0 ,
                                    XmNmarginWidth , 0 ,
                                  NULL ) ;

   PHASE_scale = XtVaCreateManagedWidget( "AFNI" , xmScaleWidgetClass , rc ,
                                            XmNminimum       , 0 ,
                                            XmNmaximum       , 100 ,
                                            XmNvalue         , 0 ,
                                            XmNwidth         , P_swide ,
                                            XmNshowValue     , True ,
                                            XmNscaleMultiple , 5 ,
                                            XmNorientation   , XmHORIZONTAL ,
                                            XmNtraversalOn , False ,
                                         NULL ) ;

   XtAddCallback( PHASE_scale , XmNvalueChangedCallback , PH_scale_CB , NULL ) ;
   XtManageChild( rc ) ;

   /* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

   XtManageChild( MAIN_rc ) ;
   XtRealizeWidget( MAIN_shell ) ;

#if 0
   XtVaSetValues( MAIN_rc     , XmNwidth , P_swide , NULL ) ;
   XtVaSetValues( MAGN_scale  , XmNwidth , P_swide , NULL ) ;
   XtVaSetValues( PHASE_scale , XmNwidth , P_swide , NULL ) ;
#endif

   XtVaSetValues( MAIN_shell ,
                    XmNmwmDecorations , MWM_DECOR_BORDER | MWM_DECOR_TITLE | MWM_DECOR_MENU ,
                    XmNmwmFunctions   , MWM_FUNC_MOVE | MWM_FUNC_CLOSE ,
                    XmNtitle          , "Xphace Controls" ,
                  NULL ) ;

   (void) XtAppAddTimeOut( MAIN_app , 1234 , PH_startup_timeout_CB , NULL ) ;
   XtAppMainLoop( MAIN_app ) ;
   exit(0) ;
}

/*------------------------------------------------------------------------*/

void PH_startup_timeout_CB( XtPointer client_data , XtIntervalId * id )
{
   PH_redraw() ; return ;
}

/*------------------------------------------------------------------------*/

static MRI_IMAGE * P_ima=NULL , * P_imb=NULL ;
static float P_alpha=0.0 , P_beta=0.0 ;

#define CONST 1
#define NOISE 2

static int   P_ima_null=CONST , P_imb_null=CONST ;
static float P_ima_val =0.0   , P_imb_val =0.0   ;

static int P_nx=256 , P_ny=256 ;

#define Mfree(im) do{mri_free(im);im=NULL;}while(0)

MRI_IMAGE * PH_fakeim( int , int , int , float ) ;

static int doA = 1 ;

int PH_loadim( char * str )
{
   MRI_IMAGE * im=NULL ;
   int nxa,nxb , nya,nyb ;

   if( str[0] == '\0' || strncmp(str,"black",5) == 0 ){
      if( doA ){ P_ima_null = CONST ; P_ima_val = 0.0 ; Mfree(P_ima) ; }
      else     { P_imb_null = CONST ; P_imb_val = 0.0 ; Mfree(P_imb) ; }
   } else if( strncmp(str,"const=",6) == 0 ){
      float val=0.0 ;
      sscanf(str+6,"%f",&val) ;
      if( doA ){ P_ima_null = CONST ; P_ima_val = val ; Mfree(P_ima) ; }
      else     { P_imb_null = CONST ; P_imb_val = val ; Mfree(P_imb) ; }
   } else if( strncmp(str,"noise=",6) == 0 ){
      float val=0.0 ;
      sscanf(str+6,"%f",&val) ;
      if( doA ){ P_ima_null = NOISE ; P_ima_val = val ; Mfree(P_ima) ; }
      else     { P_imb_null = NOISE ; P_imb_val = val ; Mfree(P_imb) ; }
   } else {
      float top ; MRI_IMAGE * qim ;
      im = mri_read( str ) ;
      if( im == NULL ) return -1;
      top = mri_maxabs(im) ;
      if( top > 0.0 ) top = 1.0 / top ;
      qim = mri_scale_to_float( top , im ) ;
      Mfree(im) ; im = qim ;
      if( doA ){ Mfree(P_ima) ; P_ima = im ; }
      else     { Mfree(P_imb) ; P_imb = im ; }
   }

   nxa = (P_ima != NULL) ? P_ima->nx : 0 ;
   nxb = (P_imb != NULL) ? P_imb->nx : 0 ;
   nya = (P_ima != NULL) ? P_ima->ny : 0 ;
   nyb = (P_imb != NULL) ? P_imb->ny : 0 ;

   P_nx = MAX(nxa,nxb) ; if( P_nx == 0 ) P_nx = 256 ;
   P_ny = MAX(nya,nyb) ; if( P_ny == 0 ) P_ny = 256 ;

   if( nxa > 0 && (nxa != P_nx || nya != P_ny) ){
      im = mri_resize(P_ima,P_nx,P_ny) ; Mfree(P_ima) ; P_ima = im ;
   }

   if( nxb > 0 && (nxb != P_nx || nyb != P_ny) ){
      im = mri_resize(P_imb,P_nx,P_ny) ; Mfree(P_imb) ; P_imb = im ;
   }

   if( P_ima == NULL ) P_ima = PH_fakeim( P_nx,P_ny , P_ima_null,P_ima_val ) ;
   if( P_imb == NULL ) P_imb = PH_fakeim( P_nx,P_ny , P_imb_null,P_imb_val ) ;

   doA = 0 ; return 0 ;
}

MRI_IMAGE * PH_fakeim( int nx , int ny , int code , float val )
{
   MRI_IMAGE * im ;
   float * far ;
   int ii , nvox ;

   im = mri_new( nx , ny , MRI_float ) ;
   far = MRI_FLOAT_PTR(im) ;
   nvox = im->nvox ;

#if 0
fprintf(stderr,"PH_fakeim: code=%d val=%f\n",code,val) ;
#endif

   switch( code ){

      default:
      case CONST:
         for( ii=0 ; ii < nvox ; ii++ ) far[ii] = val ;
      break ;

      case NOISE:
         for( ii=0 ; ii < nvox ; ii++ ) far[ii] = val * drand48() ;
      break ;
   }

   return im ;
}

void PH_scale_CB( Widget w , XtPointer cd , XtPointer cb )
{
   XmScaleCallbackStruct * cbs = (XmScaleCallbackStruct *) cb ;
   float val = 0.01 * cbs->value ;

   FIX_SCALE_SIZE ;

        if( w == MAGN_scale  ) P_alpha = val ;
   else if( w == PHASE_scale ) P_beta  = val ;

   PH_redraw() ;
}

void PH_redraw(void)
{
   MRI_IMAGE * imc , * im3 ;
   MRI_IMARR * imar ;
   float fgap = 0.0 ;

   if( P_ima == NULL || P_imb == NULL ) return ;

#if 0
fprintf(stderr,"PH_redraw:  A:nx=%d ny=%d   B:nx=%d ny=%d\n",
               P_ima->nx,P_ima->ny , P_imb->nx,P_imb->ny ) ;
fprintf(stderr,"PH_redraw: calling mri_scramble\n") ;
#endif

   imc = mri_scramble( P_ima,P_imb , 1.0-P_alpha,1.0-P_beta )  ;

#if 0
if(imc==NULL)fprintf(stderr,"  - returned NULL!\n") ;
#endif

   INIT_IMARR(imar) ;
   ADDTO_IMARR(imar,P_ima); ADDTO_IMARR(imar,imc); ADDTO_IMARR(imar,P_imb);

#if 0
fprintf(stderr,"PH_redraw: calling mri_cat2D\n") ;
if(imc!=NULL)fprintf(stderr,"  C:nx=%d ny=%d\n", imc->nx,imc->ny ) ;
#endif

   im3 = mri_cat2D( 3,1 , 3 , &fgap , imar ) ;
   FREE_IMARR(imar) ; mri_free(imc) ;

#if 0
if(im3==NULL)fprintf(stderr,"  - returned NULL!\n") ;
fprintf(stderr,"PH_redraw: calling PH_popup_image\n") ;
#endif

   P_handle = PH_popup_image( P_handle , im3 ) ;

   mri_free(im3) ; return ;
}
