#include <X11/X.h>
#include <X11/Intrinsic.h>
#include <stdio.h>

int main( int argc , char * argv[] )
{
   int ii ;
   XColor ex_col , sc_col ;
   Widget shell ;
   XtAppContext app ;
   Display * display ;
   Screen *  screen ;
   Colormap  colormap ;

   shell = XtVaAppInitialize(
              &app , "AFNI" , NULL , 0 , &argc , argv , NULL , NULL ) ;

   display  = XtDisplay(shell) ;
   screen   = XtScreen(shell) ;
   colormap = DefaultColormapOfScreen( screen ) ;

   ii = XParseColor( display , colormap , argv[1] , &ex_col ) ;
   if( ii == 0 ){ fprintf(stderr,"XParseColor failed\n") ; exit(1) ; }

   printf("R = %d  G = %d  B = %d\n",ex_col.red,ex_col.green,ex_col.blue) ;
   exit(0) ;
}
