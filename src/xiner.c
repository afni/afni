#include <stdio.h>
#include <stdlib.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/extensions/Xinerama.h>  /* link to -lXinerama */

int main( int argc , char * argv[] )
{
   Display * dpy ;
   char * dname=NULL ;
   int iarg , quiet=0 ;
   XineramaScreenInfo * xsi ; int nxsi ;

   /*-- help the pitiful user --*/

   if( argc > 1 && strcmp(argv[1],"-help") == 0 ){
      printf("Usage: xiner [-display host:num] [-q]\n"
             "Gets the Xinerama screen information from the X server,\n"
             "Writes a human-readable summary to stderr, and a simpler\n"
             "summary to stdout (for use with xrdb).\n"
             "If -q option is present, the stderr output is skipped.\n"
             "If Xinerama is not active, the exit status is 1, else 0.\n"
            ) ;
      exit(0);
   }

   /*-- scan the command line options --*/

   iarg = 1 ;
   while( iarg < argc && argv[iarg][0] == '-' ){

      if( strcmp(argv[iarg],"-display") == 0 ){
         dname = argv[++iarg] ; ++iarg ; continue ;
      }

      if( strcmp(argv[iarg],"-q") == 0 ){
         quiet++ ; iarg++ ; continue ;
      }

      fprintf(stderr,"**xiner: Unknown option: %s\n",argv[iarg]) ;
      exit(1) ;
   }

   /*-- see if we can connect --*/

   dpy = XOpenDisplay( dname ) ;
   if( dpy == NULL ){
      fprintf(stderr,"**xiner: Can't open display\n") ; exit(1) ;
   }

   /*-- see if AFNI.xinerama is already set --*/

   if( !quiet ){
      char * xdef = XGetDefault(dpy,"AFNI","xinerama") ;
      if( xdef != NULL ){
         fprintf(stderr,"AFNI.xinerama is now set=%s\n",xdef) ;
      }
   }

   /*-- Try to get the Xinerama info from the display --*/

   xsi = XineramaQueryScreens( dpy , &nxsi ) ;

   if( xsi == NULL || nxsi == 0 ){
      fprintf(stderr,"Xinerama not active on server\n") ;
      exit(1) ;  /* The premature end of a promising career */
   } else {
      int ii ; char *xp , *xstr ;

      xstr = (char *) malloc( sizeof(char) * (nxsi+1)*64 ) ;
      sprintf(xstr,"AFNI.xinerama: %d",nxsi) ;

      if(!quiet) fprintf(stderr,"Xinerama information from server:\n") ;
      for( ii=0 ; ii < nxsi ; ii++ ){
         if( !quiet )
          fprintf(stderr,
                  " Screen %2d: x origin=%4d y origin=%4d width=%4d height=%4d\n",
                  xsi[ii].screen_number ,
                  xsi[ii].x_org , xsi[ii].y_org ,
                  xsi[ii].width , xsi[ii].height ) ;

         sprintf( xstr + strlen(xstr) , " %d %d %d %d %d" ,
                    xsi[ii].screen_number ,
                    xsi[ii].x_org , xsi[ii].y_org ,
                    xsi[ii].width , xsi[ii].height ) ;
      }

      printf("%s\n",xstr) ;
   }

   /*-- Ciao, baby --*/

   exit(0) ;
}
