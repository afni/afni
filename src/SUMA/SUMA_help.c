#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 

/*!
   \brief function called when help window is open
*/
void SUMA_Help_open (void *p)
{
   static char FuncName[]={"SUMA_Help_open"};

   SUMA_ENTRY;
   /* nothing to do here */
   
   SUMA_RETURNe;
}

/*!
   \brief function called when help window is destroyed
*/
void SUMA_Help_destroyed (void *p)
{
   static char FuncName[]={"SUMA_Help_destroyed"};
   
   SUMA_ENTRY;

   SUMAg_CF->X->Help_TextShell = NULL;
   
   SUMA_RETURNe;
}

/*!
   \brief function called when Message window is open
*/
void SUMA_Message_open (void *p)
{
   static char FuncName[]={"SUMA_Message_open"};

   SUMA_ENTRY;
   /* nothing to do here */
   
   SUMA_RETURNe;
}

/*!
   \brief function called when Message window is destroyed
*/
void SUMA_Message_destroyed (void *p)
{
   static char FuncName[]={"SUMA_Message_destroyed"};
   
   SUMA_ENTRY;

   SUMAg_CF->X->Log_TextShell = NULL;
   
   SUMA_RETURNe;
}



char * SUMA_help_message_Info(void)
{
   static char FuncName[]={"SUMA_help_message_Info"};
   char stmp[1000], *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);

   s = SUMA_New_Additions (0, 1);
   SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;
   
   SS = SUMA_StringAppend (SS, 
      "\nKeyboard Controls\n");
   SS = SUMA_StringAppend (SS, 
      "     a: attenuation by background, toggle.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     B: Backface culling, toggle.\n");
   SS = SUMA_StringAppend (SS, 
      "     b: background color, toggle.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     c: load a node color file.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     Ctrl+d: draw ROI controller.\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "     d: Show all DO objects in DOv.\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "     Alt+e: Look for OpenGL errors.\n\n"); 
   SS = SUMA_StringAppend (SS, 
      "     F: Flip light position between +z and -z.\n");
   SS = SUMA_StringAppend (SS, 
      "     f: functional overlay, toggle.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     H: Highlight nodes inside a specified box.\n"
      "        Does not update other viewers\n"
      "        Paints into existing colors\n"
      "        Highlight is wiped out with new\n"
      "        colors.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     h: NO LONGER USED.\n"
      "        Please use Ctrl+h instead.\n");
   SS = SUMA_StringAppend (SS, 
      "     Ctrl+h: help message\n\n");
   SS = SUMA_StringAppend (SS, 
      "     J: Set the selected FaceSet on Surface Object\n"
      "        in Focus. Does not update in other viewers\n"
      "        or in AFNI.\n");
   SS = SUMA_StringAppend (SS, 
      "     j: Set the cross hair to a certain node on \n"
      "        SO in Focus.\n"
      "        Does update in other viewers\n"
      "        if linked by index"
      "        and AFNI if connected\n");
   SS = SUMA_StringAppend (SS, 
      "     Ctrl+j: Set the cross hair's XYZ location. \n"
      "        Does update in other viewers\n"
      "        if linked by XYZ"
      "        and AFNI if connected\n");
   SS = SUMA_StringAppend (SS, 
      "     Alt+j: Set the Focus node. \n"
      "        Cross hair's XYZ remain unchanged.\n"
      "        Does not update in other viewers\n"
      "        or in AFNI\n\n");
   SS = SUMA_StringAppend (SS, 
      "     L: Light's XYZ coordinates\n");
   SS = SUMA_StringAppend (SS, 
      "     l: look at point\n");
   SS = SUMA_StringAppend (SS, 
      "     Alt+l: look at cross hair\n");
   SS = SUMA_StringAppend (SS, 
      "     Ctrl+l: Switch locking mode for all viewers \n"
      "             between: No Lock, Index Lock and \n"
      "             XYZ Lock. The switching is order is \n"
      "             based on the lock of the first viewer.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     Alt+Ctrl+M: Dumps memory trace to file \n"
      "                 called malldump.NNN where NNN\n"
      "                 is the smallest number between\n"
      "                 001 and 999 that has not been used.\n");
   SS = SUMA_StringAppend (SS, 
      "     m: momentum, toggle\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "     n: bring a node to direct view (does not work AT ALL)\n");
   SS = SUMA_StringAppend (SS, 
      "     Ctrl+n: Open a new surface viewer window.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     p: Viewer rendering mode  \n"
      "        (Fill, Line, Points), switch.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     r: record current image\n"
      "        in an a la AFNI image viewer.\n"
      "        Identical images are rejected.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     R: Toggle continuous recording \n"
      "        to an a la AFNI image viewer.\n"
      "        Identical images are rejected.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     s: NO LONGER IN USE. \n"
      "        View the surface's structure contents.\n"
      "        Use:View->Surface Controller->More.\n");
   SS = SUMA_StringAppend (SS, 
      "     Ctrl+s: Open controller for \n"
      "             surface in Focus.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "     Ctrl+Alt+s: Input filename with coordinates\n"
      "                 forming a segment (6 values) on \n"
      "                 each line.\n");
   SS = SUMA_StringAppend (SS, 
      "     Alt+s: Switch mouse buttons 1 and 3.\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "     S: Show all surface objects registered in DOv.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     t: talk to AFNI, toggle.\n");
   SS = SUMA_StringAppend (SS, 
      "     Ctrl+t: Force a resend of \n"
      "            surfaces to AFNI.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     T: Start listening for niml connections\n\n");
   SS = SUMA_StringAppend (SS, 
      "     Ctrl+u: Open SUMA controller.\n\n");   
   SS = SUMA_StringAppend (SS, 
      "     v: NO LONGER IN USE. \n"
      "        View the viewer's structure contents.\n"
      "        Use: View->Viewer Controller->More.\n"
      "\n");
   SS = SUMA_StringAppend (SS, 
      "     w: This option has been dropped.\n"
      "        Instead, use 'r' or 'R' recording options\n"
      "        or use a screen grab instead. \n"
      "        (like xv on unix systems, and grab on Macs.)\n");
   SS = SUMA_StringAppend (SS, 
      "     W: Write ascii files containing the NodeList,\n"
      "        the FaceSetList and the nodecolors of the \n"
      "        surface in focus.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     Z/z: Zoom in/out\n\n");
   SS = SUMA_StringAppend (SS, 
      "     [: Show/Hide left hemisphere.\n"
      "     ]: Show/Hide right hemisphere.\n"
      "        Window title shows which \n"
      "        hemispheres are shown :LR:\n"
      "        :-R: :L-: or :--:\n\n");
   SS = SUMA_StringAppend (SS, 
      "  8: Set the number of smoothing iterations\n"
      "     to be applied to the foreground colors.\n"
      "     This setting will be applied to all subsequent\n"
      "     color sets.\n");
   SS = SUMA_StringAppend (SS, 
      "  *: Smooth node colors by averaging with neighbors.\n"
      "     The smoothing is only applied to the current colors,\n"
      "     and will be not be applied to new color sets.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "     @: Compute curvatures along principal directions \n"
      "        on the surface, results written to disk.\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "     (: Compute convexity of surface, \n"
      "        results written to disk.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     ,/. (think </>): Switch to next/previous view state.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     SPACE: Toggle between Mapping Reference and\n"
      "            Current view state.\n\n");

   SS = SUMA_StringAppend (SS, 
      "     L-R arrows: rotate about screen's Y axis\n");
   SS = SUMA_StringAppend (SS, 
      "     U-D arrows: rotate about screen's X axis\n");
   SS = SUMA_StringAppend (SS, 
      "     Shift+L-R arrows: translate about screen's \n"
      "                       Y axis\n");
   SS = SUMA_StringAppend (SS, 
      "     Shift+U-D arrows: translate about screen's \n"
      "                       X axis\n");
   SS = SUMA_StringAppend (SS, 
      "     Ctrl+L-R arrows: LR cardinal views\n");
   SS = SUMA_StringAppend (SS, 
      "     Ctrl+U-D arrows: IS cardinal views\n");
   SS = SUMA_StringAppend (SS, 
      "     Ctrl+Shift+U-D arrows: AP cardinal views\n\n");

   SS = SUMA_StringAppend (SS, 
      "     F1: object axis (X-Red, Y-Green, Z-Blue), \n"
      "         toggle. \n");
   SS = SUMA_StringAppend (SS, 
      "     F2: screen axis (X-Red, Y-Green), toggle. \n");
   SS = SUMA_StringAppend (SS, 
      "     F3: cross hair, toggle. \n");
   SS = SUMA_StringAppend (SS, 
      "     F4: node selection highlight, toggle. \n");
   SS = SUMA_StringAppend (SS, 
      "     F5: FaceSet selection highlight, toggle.\n");
   SS = SUMA_StringAppend (SS, 
      "     F6: Viewer background color, toggle.\n");
   SS = SUMA_StringAppend (SS, 
      "     F7: Switch between color mixing modes.\n");
   SS = SUMA_StringAppend (SS, 
      "     F12: Time 20 scene renderings.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     HOME: reset view to startup\n\n");
   SS = SUMA_StringAppend (SS, 
      "     ESCAPE: close the surface viewer window.\n");
   SS = SUMA_StringAppend (SS, 
      "     Shft+ESCAPE: close all surface viewer windows.\n\n");
   SS = SUMA_StringAppend (SS, 
      "     Mouse Controls:\n");
   SS = SUMA_StringAppend (SS, 
      "     Button 1-Motion: rotation as if you were using\n"
      "                      a trackball.\n");
   SS = SUMA_StringAppend (SS, 
      "       Pure vertical motion is equivalent to using \n"
      "       the up/down arrow keys.\n");
   SS = SUMA_StringAppend (SS, 
      "       Pure horizontal motion is equivalent to using \n"
      "       the left/right arrow keys.\n");
   SS = SUMA_StringAppend (SS, 
      "       Of course, the advantage to using the mouse is \n"
      "       a continuous range of rotation angles and \n");
   SS = SUMA_StringAppend (SS, 
      "       simultaneous rotations about the screen's \n"
      "       X & Y axis.\n");
   SS = SUMA_StringAppend (SS, 
      "       This mode of rotation is similar to SGI's \n"
      "       ivview interface.\n");
   SS = SUMA_StringAppend (SS, 
      "     Button 2-Motion: translation\n"); 
   SS = SUMA_StringAppend (SS, 
      "     Button 1+2-Motion OR \n"
      "      Shift+Button2-Motion: \n"
      "          Zoom in/out\n");
   SS = SUMA_StringAppend (SS, 
      "     Button 3-Press: picking \n");
   SS = SUMA_StringAppend (SS, 
      "     shft+Button 3-Press: ROI drawing \n"
      "                          (when in DrawROI mode)\n");
   SS = SUMA_StringAppend (SS, 
      "    \n");
   SS = SUMA_StringAppend (SS, 
      "    File Menu:\n"
      "    ->Save View: Save viewer's display settings.\n"
      "    ->Load View: Load and apply display settings.\n"
      "    ->Close: Close this viewer.\n"
      "             Exit SUMA if this is the only viewer.\n");
   SS = SUMA_StringAppend (SS, 
      "    View Menu:\n"
      "    ->SUMA Controller: Open SUMA controller interface.\n"
      "    ->Surface Controller: Open selected surface's \n"
      "                          controller interface.\n"
      "    ->Viewer Controller: Open viewer's controller interface.\n"
      "    --------\n"
      "    ->Cross Hair: Toggle cross hair display.\n"
      "    ->Node in Focus: Toggle highlight of selected node.\n"
      "    ->Selected Faceset: Toggle highlight of selected faceset.\n");
   SS = SUMA_StringAppend (SS, 
      "    Tools Menu:\n"
      "    ->Draw ROI: Open Draw ROI controller.\n");
   SS = SUMA_StringAppend (SS, 
      "    Help Menu:\n"
      "    ->Usage: Opens window with this message.\n"
      "    ->Message Log: Opens window that will \n"
      "                   contain errors and warnings\n"
      "                   typically output to screen.\n"
      "    -------\n"
      "    ->SUMA Global: Output debugging information\n"
      "                   about some of SUMA's global \n"
      "                   structure's variables.\n"
      "    ->Viewer Struct: Output debugging info on \n"
      "                     a viewer's structure.\n"
      "    ->Surface Struct: Output debugging info on\n"
      "                      the selected surface's struct.\n"
      "    -------\n"
      "    ->InOut Notify: Turn on/off function in/out tracing.\n"
      "    ->MemTrace: Turn on memory tracing.\n"
      "                Once turned on, this can't be turned off.\n"
      "\n");
   SS = SUMA_StringAppend (SS, 
      "    More help at \n"
      "    http://afni.nimh.nih.gov/ssc/ziad/SUMA/SUMA_doc.htm\n");
   SS = SUMA_StringAppend (SS, 
      "\n");
   
   /* add latest additions */
   SS = SUMA_StringAppend (SS, "Current Version Info:\n");
   s = SUMA_New_Additions (0, 0);
   SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;

   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN (s);

}
/*!
Controls help message
*/
void SUMA_help_message(FILE *Out)
{
	char *s=NULL;
   static char FuncName[]={"SUMA_help_message"};
   
   SUMA_ENTRY;

   if (Out == NULL) {
		Out = stdout;
	}
   
   s = SUMA_help_message_Info();
   if (!s) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_help_message_Info.\n", FuncName);
   }else {
      fprintf (Out, "%s\n", s);
      SUMA_free(s);
   }
	
   SUMA_RETURNe;
}

char *SUMA_All_Programs(void )
{
   char *s=NULL;
   static char FuncName[]={"SUMA_All_Programs"};
   SUMA_STRING  *SS = NULL; 
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   SS = SUMA_StringAppend ( SS,
         "+ List of programs in the SUMA package:\n"
         "  3dSurf2Vol\n"
         "  3dVol2Surf\n"
         "  CompareSurfaces\n"
         "  ConvertSurface\n"
         "  CreateIcosahedron\n"
         "  FSread_annot\n"
         "  inspec\n"
         "  MakeColorMap\n"
         "  MapIcosahedron\n"
         "  quickspec\n"
         "  ROI2dataset\n"
         "  ScaleToMap\n"
         "  SUMA_glxdino\n"
         "  SurfMeasures\n"
         "  SurfaceMetrics\n"
         "  SurfSmooth\n"
         "  SurfPatch\n"
         "  SurfQual\n"
     );
   
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN(s);      
   
}
/*!
   \brief Returns a string with the new additions and version information
   
   \param ver (float) v (v > 0) for info on version v alone 
                      0.0 just the latest version info
                      -1.0 for all versions
   \param StampOnly (SUMA_Boolean) Want version number and time stamp only ?
                      
   \return s (char *) the string, yall have to free it with SUMA_free
   \sa SUMA_New_Additions_perver
   
   - To add a new version, you must add a case statement in SUMA_New_Additions_perver
     AND add the version number in the beginning of verv in SUMA_New_Additions
*/
char * SUMA_New_Additions (int ver, SUMA_Boolean StampOnly)
{
   static char FuncName[]={"SUMA_New_Additions"};
   char *s = NULL;
   int i;
   SUMA_STRING *SS = NULL;
   int verv[] = { 24800, 24500, -10000}; /* modify this dude and you must update SUMA_New_Additions_perver  
                                       Add to the left of the vector, leave the last value of -10000 untouched
                                       If you like to think of floating point version numbers,divide by 10000*/
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   if (ver == 0) { /* just the latest */
      s = SUMA_New_Additions_perver( verv[0], StampOnly);
      if (s) {
         SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;
      }
   } else if (ver < 0) {
      /* all history */
      SS = SUMA_StringAppend (SS, "All Version Info:\n"); 
      i = 0;
      while (verv[i] > 0) {
         s = SUMA_New_Additions_perver( verv[i], StampOnly);
         if (s) {
         SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;
         SS = SUMA_StringAppend (SS, "\n");
         }
         ++i;
      }
      
   } else {
      /* just for ver */
      s = SUMA_New_Additions_perver( ver, StampOnly);
      if (s) {
         SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;
      }
   }
   
   /* add the compile date */
   SS = SUMA_StringAppend_va (SS, "\nCompile Date:\n   %s\n",__DATE__);
   
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN(s);      
   
}
/*!
   \brief Returns a string with version information
   \param ver (float) Version number
   \param StampOnly (SUMA_Boolean) if YUP 
                     then return the time stamp of the version only)
   \return s (char *) the string, yall have to free it with SUMA_free
   \sa SUMA_New_Additions
   
   - To add a new version, you must add a case statement in SUMA_New_Additions_perver
     AND add the version number in the beginning of verv in SUMA_New_Additions
*/
char * SUMA_New_Additions_perver (int ver, SUMA_Boolean StampOnly)
{
   static char FuncName[]={"SUMA_New_Additions_perver"};
   char *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   
   switch (ver) {
      /* Must modify verv in SUMA_New_Additions when you touch this block */
      /*
      case XX:
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %.2f, DATEHERE\n", (float)ver/10000.0); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
            "New Programs:\n"
            "  + \n"
            "Modifications:\n"
            "  + \n");
         break; 
      */
      /*
      case 24900:
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %.2f, DATEHERE\n", (float)ver/10000.0); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
            "New Programs:\n"
            "  + \n"
            "Modifications:\n"
            "  + \n");
         break; 
         */
      case 24800:
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %.4f, Jan. 16 2004\n", (float)ver/10000.0); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
            "New Programs:\n"
            "  + FS_readannot: Program to read FreeSurfer's\n"
            "                  annotation files.\n"
            "  + SurfPatch: Program to create surface patches\n"
            "               from a set of nodes.\n"
            "  + SurfQual: Program to report defects in surfaces.\n"
            "              For the moment, works on spherical \n"
            "              surfaces only.\n"
            "Modifications:\n"
            "  + Added affine transforms to ConvertSurface.\n"
            "  + Added datasets into SUMA's code (no interface).\n"
            "  + Added saving/loading of viewer settings.\n"
            "  + Beginning of multiple group support in SUMA.\n"
            "  + Redisplays of Surface Viewers due to X events\n"
            "    are no longer passed to the image recorder.\n" );
         break; 
         
      case 24500:
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %.4f, Jan. 6 2004\n", (float)ver/10000.0); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
            "New Programs:\n"
            "  + inspec: Shows the contents of a spec file\n"
            "  + quickspec: Creates a minimal spec file for one\n"
            "               or a bunch of surfaces.\n"
            "  + SurfSmooth: Smoothes surface data or geometry\n"
            "  + SurfMeasures: Outputs various surface attributes  \n"
            "                  and measurements such as:\n"
            "                  Thickness, Area, Volume, etc.\n"
            "Modifications:\n"
            "  + Foreground color smoothing option (SUMA keyb. 8)\n"
            "  + No more MappingRef field in Spec files.\n"
            "    The field is broken up into a set of other\n"
            "    fields for more flexibility.\n"
            "  + Surface input to command-line programs is \n"
            "    now done via -spec files too.\n"
            "  + One-way communication with SUMA via niml.\n"
            "    Only available with SurfSmooth for the moment.\n"
            "  + Began, in good faith, to update the new version \n"
            "    information.\n"); 
         break;
      
      default:
         SS = SUMA_StringAppend_va(SS, "++ %f? No such version, fool!\n", (float)ver/10000.0);
         break;
   }
   
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN(s);
}

/*!
SUMA version 
*/

void SUMA_Version (FILE *Out)
{
   static char FuncName[]={"SUMA_Version"};
   char *s = NULL;
   
   if (Out == NULL) {
		Out = stdout;
	}
   s = SUMA_New_Additions (0, 0);
	if (s) {
      fprintf (Out, "\n   %s\n", s);
      SUMA_free(s);
   } else {
      fprintf (Out, "\n");
   }
   
	return;
}

/*!
Surface .. Volume relationships
*/
void SUMA_VolSurf_help (FILE *Out)
{
	if (Out == NULL) {
		Out = stdout;
	}
    fprintf (Out, "SUMA_VolSurf_help: This function is obsolete.\n");
	 return;
	 fprintf (Out, "\nVolume <--> Surface jeremiad:\n");
	 fprintf (Out, "-----------------------------\n");
	 fprintf (Out, "\tTo transform surface node coordinates to voxel coordinates a few parameters are required.\n");
	 fprintf (Out, "\tThose paramters vary depending on the type of surfaces used. Currently, SUMA supports \n");
	 fprintf (Out, "\tFreeSurfer and SureFit surfaces.\n");
	 fprintf (Out, "\nParent Volume (VolPar):\n");
	 fprintf (Out, "\tThe surface model is created from a high-resolution anatomical scan\n"); 
	 fprintf (Out, "\treferred to as Parent Volume (VolPar).\n"); 
	 fprintf (Out, "\tTo align the surface with data from a particular experiment, VolPar must\n"); 
	 fprintf (Out, "\tbe brought to alignment with the experiemt's data.\n"); 
	 fprintf (Out, "\tFor example, VolPar is aligned with data from experiment Day1 using:\n"); 
	 fprintf (Out, "\t3dvolreg -clipit -twopass -twodup -zpad 8 -rotcom -verbose \n"); 
	 fprintf (Out, "\t-base SPGR_Day1 -prefix VolPar_Day1 VolMast >>& VolParAlignLog\n");
	 fprintf (Out, "\twhere SPGR_Day1 is the high-resolution anatomical scan obtained in\n"); 
	 fprintf (Out, "\texperiment Day1 and VolPar_Day1 is VolPar aligned to SPGR_Day1.\n"); 
	 fprintf (Out, "\nSurface segmentation programs typically require the resolution of VolPar to\n"); 
	 fprintf (Out, "\tbe 1x1x1mm. Such volumes, especially for FreeSurfer are quite large and\n"); 
	 fprintf (Out, "\t3dvolreg might run out of memory. If that happens, you could resample \n"); 
	 fprintf (Out, "\tVolPar to a lower resolution such as 1.2x1.2x1.2mm, prior to registration. \n"); 
	 fprintf (Out, "\tNote that SPGR_Day1 must have the same resolution and number of slices as VolPar.\n"); 
	 fprintf (Out, "\n\t+FreeSurfer Parent Volume:\n"); 
	 fprintf (Out, "\tConstruct VolPar from the .COR images used to create the surface using:\n"); 
	 fprintf (Out, "\tto3d -prefix CW-cSurfParent-SPGR -xSLAB 127.5L-R -ySLAB 127.5S-I -zSLAB 127.5P-A COR-???\n");
	 fprintf (Out, "\tExample command line for a FreeSurfer suface with VolPar aligned to experiment ARzs:\n"); 
	 fprintf (Out, "\t./suma -vp CW-cSurfParent-SPGR_Reg2_ARzsspgrax_1mm_256pad_cor_RSP_down12+orig\\\n");
	 fprintf (Out, "\t -spec CW-FreeSurfer.SumaSpec\n");
	 fprintf (Out, "\n\t+SureFit Parent Volume:\n"); 
	 fprintf (Out, "\tVolPar is the anatomical 1x1x1 mm volume in the correct orientation (LPI) \n"); 
	 fprintf (Out, "\tthat is used by SureFit to create the surface. Typically, this volume has \n"); 
	 fprintf (Out, "\tthe .Orient string in its name unless it was in LPI orientation from the \n"); 
	 fprintf (Out, "\tstart. Because SureFit crops the volume before segmentation, it is also \n"); 
	 fprintf (Out, "\tnecessary to supply the .params file along with VolPar. The .params file is \n"); 
	 fprintf (Out, "\ttypically named something like: (anything here).L.full.sMRI.params for the \n"); 
	 fprintf (Out, "\tleft full hemisphere.  Example command line for a SureFit surface with VolPar:\n"); 
	 fprintf (Out, "\t./suma -vp colin_short_Orient+orig. colin_short+orig.L.full.sMRI.params\\\n");
	 fprintf (Out, "\t -s_s colin.fiducial.coord colin.topo");
	 fprintf (Out, "\nor:\n");
	 fprintf (Out, "\t./suma -vp CW-cSurfParent-SPGR-AX_LPI+orig. -spec CW-SureFit.SumaSpec\n");
	 fprintf (Out, "\t\n"); 
	 return;
}
