#include "coxplot.h"

#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <Xm/XmAll.h>

/*************************************************************************
 *************************************************************************
 *************************************************************************/

static char * vcl[] =  { "StaticGray"  , "GrayScale" , "StaticColor" ,
                         "PseudoColor" , "TrueColor" , "DirectColor"  } ;

static XImage * xim    = NULL ;
static int      xim_ww = 0 ;
static int      xim_hh = 0 ;

#include "math.h"

static int need_plot = 1 ;

#define STATUS(str) fprintf(stderr,"-- " str "\n")

void make_plot(void)
{
   integer ii,jj,kk,ll ;
   float a,b,c,d,e,f,g,h ;

   /*----- make the graph plot -----*/

STATUS("enter make_plot") ;

   ii = create_memplot( "Elvis" , 0.0 ) ;
   if( ii != 0 ){ fprintf(stderr,"Can't create memplot Elvis!\n") ; exit(1) ; }

STATUS("created Elvis") ;

   set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
   set_thick_memplot( 0.0 ) ;

STATUS("set color and thickness") ;

   plotpak_set( 0.1,1.25 , 0.1,0.95 , 0.0,10.0 , 0.0,5.0 , 1 ) ;

STATUS("plotpak_set") ;

   plotpak_periml( 5,10 , 5,10 ) ;

STATUS("drew perimeter") ;

   set_color_memplot( 0.0 , 0.0 , 0.9 ) ;
   set_thick_memplot( 0.002 ) ;
   plotpak_frstpt( 0.0 , 0.0 ) ;
   for( ii=0 ; ii < 1000 ; ii++ ){
      a = 0.01 * ii ; b = sin(a) ; b = 5.0 * b * b / (1.0+0.1*a) ;
      plotpak_vector( a , b ) ;
   }

STATUS("drew sin") ;

   set_color_memplot( 0.9 , 0.0 , 0.0 ) ;
   set_thick_memplot( 0.005 ) ;
   plotpak_frstpt( 0.0 , 5.0 ) ;
   for( ii=0 ; ii < 1000 ; ii++ ){
      a = 0.01 * ii ; b = cos(a) ; b = 5.0 * b * b / (1.0+0.1*a) ;
      plotpak_vector( a , b ) ;
   }

STATUS("drew cos") ;

   fprintf(stderr,"-- Created memplot Elvis: %d\n",nline_active_memplot()) ;

#if 1
   /*----- make the surface plot -----*/

   ii = create_memplot( "Pelvis" , 1.0 ) ;
   if( ii != 0 ){ fprintf(stderr,"Can't create memplot Pelvis!\n") ; exit(1) ; }

STATUS("created Pelvis") ;

#define NX 90
#define NY 90
   { float * x , * y , * z , dx , dy ;
     x = (float *) malloc( sizeof(float) * NX ) ;
     y = (float *) malloc( sizeof(float) * NY ) ;
     z = (float *) malloc( sizeof(float) * NX*NY ) ;
     dx = 2.0/(NX-1.0) ; dy = 2.0/(NY-1.0) ;
     for( ii=0 ; ii < NX ; ii++ ) x[ii] = -1.0 + ii * dx ;
     for( ii=0 ; ii < NY ; ii++ ) y[ii] = -1.0 + ii * dy ;
     for( jj=0 ; jj < NY ; jj++ ){
        for( ii=0 ; ii < NX ; ii++ )
           z[ii+jj*NX] = sin( 3.0*(x[ii]-y[jj]) + 10.0*x[ii]*y[jj] )
                        / ( 1.0 + x[ii]*x[ii] + y[jj]*y[jj] ) ;
     }

STATUS("created srface arrays") ;

     set_thick_memplot( 0.0 ) ;
     set_color_memplot( 0.0 , 0.0 , 0.0 ) ;
     plotpak_srface( x , y , z , NX , NY , 45.0 , 33.0 ) ;

STATUS("plotpak_srface") ;
     free((char *)x) ; free((char *)y) ; free((char *)z) ;
STATUS("free srface arrays") ;

     set_color_memplot( 1.0 , 0.0 , 0.0 ) ;
     plotpak_set( 0.0,1.0 , 0.0,1.0 , 0.0,1.0 , 0.0,1.0 , 1 ) ;
     plotpak_pwritf( 0.5 , 0.94 , "Sample Surface Plot" , 20 , 0 , 0 ) ;
STATUS("pwritf") ;

     fprintf(stderr,"-- Created memplot Pelvis: %d\n",nline_active_memplot()) ;
   }
#endif

   need_plot = 0 ;
   return ;
}

void elvis_CB( Widget w , XtPointer cd , XtPointer cb )
{
   XmDrawingAreaCallbackStruct * cbs = (XmDrawingAreaCallbackStruct *) cb ;
   XExposeEvent * ev = (XExposeEvent *) cbs->event ;
   int start , end ;
   static int ncall = 0 ;

   if( cbs->reason != XmCR_EXPOSE || ev->count > 0 ) return ;

STATUS("enter elvis_CB") ;

   if( need_plot ){
      make_plot() ;
      set_X11_background( XtDisplay(w) , XtWindow(w) , 255,255,255 ) ;
   }
   XClearWindow( XtDisplay(w) , XtWindow(w) ) ;

#if 1
   if( ncall%2 == 0 ) set_active_memplot( "Elvis" ) ;
   else               set_active_memplot( "Pelvis" ) ;
   ncall++ ;
#endif

STATUS("XClearWindow") ;

   start = end = 0 ;
#if 0
   memplot_to_X11( XtDisplay(w) , XtWindow(w) ) ;
#else
   memplot_to_X11_free( XtDisplay(w) , XtWindow(w) ) ;
#endif
   return ;
}

#define DEATH
void fred_CB( Widget w , XtPointer qd , XtPointer qb )
{
#ifdef DEATH
   exit(0) ;
#else
   X11_colordef * cd ;

   cd = get_X11_colordef( XtDisplay(w) , XtWindow(w) ) ;

   if( cd == NULL ){
      fprintf(stderr,"get_X11_colordef fails!\n") ; return ;
   }

   if( cd->classKRH == TrueColor ){
      fprintf(stderr,"get_X11_colordef: visual class=TrueColor depth=%d\n"
                     "  masks:  red = %x  green = %x  blue = %x\n"
                     "  shifts: red = %d  green = %d  blue = %d\n",
                     cd->depth ,
                     cd->rrmask , cd->ggmask , cd->bbmask ,
                     cd->rrshift , cd->ggshift , cd->bbshift ) ;

   } else if ( cd->classKRH == PseudoColor ){
      int ii ;
      fprintf(stderr,"get_X11_colordef: visual class=PseudoColor depth=%d colors=%d\n",
                     cd->depth , cd->ncolors ) ;
      for( ii=0 ; ii < cd->ncolors ; ii++ ){
         fprintf(stderr,"  %3d: r=%4x g=%4x b=%4x\n",
                        ii,cd->rr[ii],cd->gg[ii],cd->bb[ii]) ;
      }
   }

   FREE_X11_colordef(cd) ;
#endif /* DEATH */
}

int main( int argc , char * argv[] )
{
        XtAppContext    app;            /* the application context */
        Display         *dpy;           /* display */
        Colormap        colormap;       /* created colormap */
        XVisualInfo     vinfo;          /* template for find visual */
        Visual          *vis ;          /* the Visual itself */
        XVisualInfo     *vinfo_list;    /* returned list of visuals */
        int             count;          /* number of matchs (only 1?) */
        int             vid , stat ;
        Widget          fred , fff , top ;

        if( argc < 2 ){ fprintf(stderr,"Usage: xxx visid\n") ; exit(0) ; }

        top = XtVaAppInitialize( &app , "xxx" , NULL , 0 , &argc,argv , NULL,NULL ) ;
        dpy = XtDisplay (top);

STATUS("XtVaAppInitialize") ;

        vid = strtol( argv[1] , NULL , 0 ) ;
        vinfo.visualid = (VisualID) vid ;
        vinfo_list = XGetVisualInfo (dpy, VisualIDMask, &vinfo, &count);
        if( count == 0 || vinfo_list == NULL ){
           fprintf(stderr,"no such visual\n");exit(1);
        }
        vinfo = vinfo_list[0] ;

STATUS("XGetVisualInfo") ;

        vid = vinfo.visualid ;
        vis = vinfo.visual ;

        if( vis != DefaultVisual(dpy,vinfo.screen) ){
           colormap = XCreateColormap( dpy, RootWindowOfScreen(XtScreen(top)) ,
                                       vis , AllocNone ) ;
STATUS("XCreateColormap") ;
        } else {
           colormap = DefaultColormap(dpy,vinfo.screen) ;
STATUS("DefaultColormap") ;
        }

        XtVaSetValues( top ,
                          XtNborderColor , 0 ,
                          XtNbackground  , 0 ,
                          XtNdepth       , vinfo.depth ,
                          XtNcolormap    , colormap ,
                          XtNvisual      , vis ,
                       NULL ) ;

STATUS("XtVaSetValues") ;

        fff = XtVaCreateWidget( "dialog" , xmFormWidgetClass , top ,
                                   XmNborderWidth , 0 ,
                                NULL ) ;

STATUS("XtVaCreateWidget") ;

#ifndef LABEL_ARG
#define LABEL_ARG(str) \
  XtVaTypedArg , XmNlabelString , XmRString , (str) , strlen(str)+1
#endif

        fred = XtVaCreateManagedWidget( "dialog" , xmPushButtonWidgetClass , fff ,
                                          LABEL_ARG("Death") ,
                                          XmNtopAttachment    , XmATTACH_FORM ,
                                          XmNleftAttachment   , XmATTACH_FORM ,
                                          XmNrightAttachment  , XmATTACH_FORM ,
                                         NULL ) ;
        XtAddCallback( fred , XmNactivateCallback , fred_CB , NULL ) ;

STATUS("XtVaCreateManagedWidget: button") ;

        fred = XtVaCreateManagedWidget( "dialog" , xmDrawingAreaWidgetClass , fff ,
                                          XmNtopAttachment    , XmATTACH_WIDGET ,
                                          XmNtopWidget        , fred ,
                                          XmNleftAttachment   , XmATTACH_FORM ,
                                          XmNrightAttachment  , XmATTACH_FORM ,
                                          XmNbottomAttachment , XmATTACH_FORM ,
                                        NULL ) ;

        XtAddCallback( fred , XmNexposeCallback , elvis_CB , NULL ) ;

STATUS("XtVaCreateManagedWidget: drawing area") ;

        xim_ww = xim_hh = 77 ;

        XtVaSetValues( top ,
                         XmNwidth , xim_ww ,
                         XmNheight , xim_hh+40 ,
                       NULL ) ;

        XtManageChild(fff) ;
        XtRealizeWidget(top);

STATUS("XtVaCreateManagedWidget: realize") ;

        XtAppMainLoop(app);

        exit(0); /* never reached */
}
