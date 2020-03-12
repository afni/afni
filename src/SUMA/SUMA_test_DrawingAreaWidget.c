
#include <stdio.h>

#include "../suma_suma.h"

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
