#include <X11/Intrinsic.h>
#include <X11/StringDefs.h>
#include <X11/Shell.h>
#include <stdio.h>

#include <Xm/XmAll.h>

static char * vcl[] =  { "StaticGray"  , "GrayScale" , "StaticColor" ,
                         "PseudoColor" , "TrueColor" , "DirectColor"  } ;

void fred_CB( Widget w , XtPointer cd , XtPointer cb ){ exit(0); }

int main( int argc , char * argv[] )
{
        XtAppContext    app;            /* the application context */
        Widget          top;            /* toplevel widget */
        Display         *dpy;           /* display */
        Colormap        colormap;       /* created colormap */
        XVisualInfo     vinfo;          /* template for find visual */
        XVisualInfo     *vinfo_list;    /* returned list of visuals */
        int             count;          /* number of matchs (only 1?) */
        int             vid ;
        Widget          fred ;

        /*
         * The following creates a _dummy_ toplevel widget so we can
         * retrieve the appropriate visual resource.
         */

        top = XtVaAppInitialize( &app , "test" , NULL , 0 , &argc , argv , NULL , NULL ) ;
        dpy = XtDisplay (top);

        vid = strtol( argv[1] , NULL , 10 ) ;
        vinfo.visualid = (VisualID) vid ;
        vinfo_list = XGetVisualInfo (dpy, VisualIDMask, &vinfo, &count);
        if( count == 0 || vinfo_list == NULL ){fprintf(stderr,"no match\n");exit(1);}
        vid = vinfo_list[0].visualid ;

        colormap = XCreateColormap( dpy, RootWindowOfScreen(XtScreen(top)) ,
                                    vinfo_list[0].visual , AllocNone ) ;

        XtVaSetValues( top ,
                          XtNborderColor , 0 ,
                          XtNbackground  , 0 ,
                          XtNdepth       , vinfo_list[0].depth ,
                          XtNcolormap    , colormap ,
                          XtNvisual      , vinfo_list[0].visual ,
                       NULL ) ;

#ifndef LABEL_ARG
#define LABEL_ARG(str) \
  XtVaTypedArg , XmNlabelString , XmRString , (str) , strlen(str)+1
#endif

        fred = XtVaCreateManagedWidget( "dialog" , xmPushButtonWidgetClass , top ,
                                          LABEL_ARG("Jumpback") ,
                                         NULL ) ;

        XtAddCallback( fred , XmNactivateCallback , fred_CB , NULL ) ;

        XtRealizeWidget(top);
        XtAppMainLoop(app);
        return (0);
}
