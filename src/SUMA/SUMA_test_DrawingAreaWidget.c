
#include <stdio.h>

#ifdef BUILD_WITHOUT_SUMA

   /* -----------------------------------------------------------------
      If BUILD_WITHOUT_SUMA, compile as a standalone that does not
      depend on SUMA.
      In this case, one should be able to compile using something like:
     
         gcc -DBUILD_WITHOUT_SUMA -o IlikeJello  \
             SUMA_test_DrawingAreaWidget.c -I/usr/include/GL -lGLw
    * ----------------------------------------------------------------- */

   #include <X11/IntrinsicP.h>
   #include <X11/StringDefs.h>

#else

   /* -----------------------------------------------------------------
      By default, compile as a normal SUMA program.
     
         make SUMA_test_DrawingAreaWidget
    * ----------------------------------------------------------------- */

   #include "SUMA_X_objs.h"

#endif

#ifdef SUMA_MOTIF_GLXAREA
   #include <GL/GLwMDrawA.h>
#else
   #include <GL/GLwDrawA.h>  /* OpenGL drawing area. */
#endif


/* Just test if there is a "valid" DrawingAreaWidgetClass pointer */
int main(int argc, char **argv)
{

#ifdef SUMA_MOTIF_GLXAREA

   if( glwMDrawingAreaWidgetClass == NULL ) {
       fprintf(stderr,"** ERROR: glwMDrawingAreaWidgetClass is NULL\n"
       "   This might be an error in GLwDrawA.h where the class is\n"
       "   not referenced using 'extern'.  An alternative is to use\n"
       "   the local build of libGLws.a.\n");
       return(1);
   }
   printf("-- good: have non-NULL glwMDrawingAreaWidgetClass\n");

#else

   if( glwDrawingAreaWidgetClass == NULL ) {
       fprintf(stderr,"** ERROR: glwDrawingAreaWidgetClass is NULL\n"
       "   This might be an error in GLwDrawA.h where the class is\n"
       "   not referenced using 'extern'.  An alternative is to use\n"
       "   the local build of libGLws.a.\n");
       return(1);
   }
   printf("-- good: have non-NULL glwDrawingAreaWidgetClass\n");

#endif

   return 0;
}
