#include <X11/Intrinsic.h>
#include <stdio.h>

Boolean workproc_A( XtPointer ) ;  /* protos */
Boolean workproc_B( XtPointer ) ;
void killit_CB( XtPointer , XtIntervalId * ) ;

static XtAppContext app ;          /* global */

/*------------------------------------------------------------------*/

int main( int argc , char * argv[] )
{
   Widget wid ;

   wid = XtVaAppInitialize(
            &app, "Elvis", NULL, 0, &argc, argv, NULL, NULL ) ;
   if( wid == NULL ){
      fprintf(stderr,"*** Cannot initialize X11!\n") ; exit(1) ;
   }

   XtAppAddTimeOut( app , 1234 , killit_CB , NULL ) ;

   XtAppAddWorkProc( app, workproc_A, NULL ) ;
   XtAppMainLoop(app) ;
   exit(0) ;
}

/*------------------------------------------------------------------*/

void killit_CB( XtPointer xyzzy , XtIntervalId * zork )
{ exit(0) ; }

/*------------------------------------------------------------------*/

#define WMAX 4

Boolean workproc_A( XtPointer elvis )
{
   static int ncall=0 ;

   if( ncall == 0 ) XtAppAddWorkProc( app, workproc_B, NULL ) ;
   ncall++ ; printf("workproc_A: %d\n",ncall) ;
   return (ncall < WMAX) ? False : True ;
}

/*------------------------------------------------------------------*/

Boolean workproc_B( XtPointer presley )
{
   static int ncall=0 ;
   ncall++ ; printf("workproc_B: %d\n",ncall) ;
   return (ncall < WMAX) ? False : True ;
}

/*------------------------------------------------------------------*/

/*----------  Fix a Linux stupidity  -------------------------------*/

#ifdef NEED_XSETLOCALE
#include <locale.h>
char * _Xsetlocale( int category, const char * locale)
{ return setlocale(category,locale) ; }
#endif
