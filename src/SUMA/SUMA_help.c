#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 

/*!
   \brief function called when help window is open
*/
void SUMA_Help_open (void *p)
{
   static char FuncName[]={"SUMA_Help_open"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   /* nothing to do here */
   
   SUMA_RETURNe;
}

/*!
   \brief function called when help window is destroyed
*/
void SUMA_Help_destroyed (void *p)
{
   static char FuncName[]={"SUMA_Help_destroyed"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SUMAg_CF->X->Help_TextShell = NULL;
   
   SUMA_RETURNe;
}

/*!
   \brief function called when Message window is open
*/
void SUMA_Message_open (void *p)
{
   static char FuncName[]={"SUMA_Message_open"};

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   /* nothing to do here */
   
   SUMA_RETURNe;
}

/*!
   \brief function called when Message window is destroyed
*/
void SUMA_Message_destroyed (void *p)
{
   static char FuncName[]={"SUMA_Message_destroyed"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SUMAg_CF->X->Log_TextShell = NULL;
   
   SUMA_RETURNe;
}



char * SUMA_help_message_Info(void)
{
   static char FuncName[]={"SUMA_help_message_Info"};
   char stmp[1000], *s = NULL;
   SUMA_STRING *SS = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SS = SUMA_StringAppend (NULL, NULL);

   s = SUMA_New_Additions (0.0, 1);
   SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;
   
   SS = SUMA_StringAppend (SS, 
      "\nKeyboard Controls\n");
   SS = SUMA_StringAppend (SS, 
      "\t  a: attenuation by background, toggle.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  B: Backface culling, toggle.\n");
   SS = SUMA_StringAppend (SS, 
      "\t  b: background color, toggle.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  c: load a node color file.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+d: draw ROI controller.\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "\t  d: Show all DO objects in DOv.\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "\t  Alt+e: Look for OpenGL errors.\n\n"); 
   SS = SUMA_StringAppend (SS, 
      "\t  F: Flip light position between +z and -z.\n");
   SS = SUMA_StringAppend (SS, 
      "\t  f: functional overlay, toggle.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  H: Highlight nodes inside a specified box.\n"
      "\t     Does not update other viewers\n"
      "\t     Paints into existing colors\n"
      "\t     Highlight is wiped out with new\n"
      "\t     colors.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  h: NO LONGER USED.\n"
      "\t     Please use Ctrl+h instead.\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+h: help message\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  J: Set the selected FaceSet on Surface Object\n"
      "\t     in Focus. Does not update in other viewers\n"
      "\t     or in AFNI.\n");
   SS = SUMA_StringAppend (SS, 
      "\t  j: Set the cross hair to a certain node on \n"
      "\t     SO in Focus.\n"
      "\t     Does update in other viewers\n"
      "\t     if linked by index"
      "\t     and AFNI if connected\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+j: Set the cross hair's XYZ location. \n"
      "\t     Does update in other viewers\n"
      "\t     if linked by XYZ"
      "\t     and AFNI if connected\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Alt+j: Set the Focus node. \n"
      "\t     Cross hair's XYZ remain unchanged.\n"
      "\t     Does not update in other viewers\n"
      "\t     or in AFNI\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  L: Light's XYZ coordinates\n");
   SS = SUMA_StringAppend (SS, 
      "\t  l: look at point\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Alt+l: look at cross hair\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+l: Switch locking mode for all viewers \n"
      "\t          between: No Lock, Index Lock and \n"
      "\t          XYZ Lock. The switching is order is \n"
      "\t          based on the lock of the first viewer.\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+M: Show memory trace if Debug flag \n"
      "\t          MemTrace is on. (requires compilation \n"
      "\t          with SUMA_MEMTRACE_FLAG 1).\n");
   SS = SUMA_StringAppend (SS, 
      "\t  m: momentum, toggle\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "\t  n: bring a node to direct view (does not work AT ALL)\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+n: Open a new surface viewer window.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  p: Viewer rendering mode  \n"
      "\t     (Fill, Line, Points), switch.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  r: record current image\n"
      "\t     in an a la AFNI image viewer.\n"
      "\t     Identical images are rejected.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  R: Toggle continuous recording \n"
      "\t     to an a la AFNI image viewer.\n"
      "\t     Identical images are rejected.\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "\t  s: NO LONGER IN USE. Use:\n"
      "\t     View->Surface Controller->More.\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+s: Open controller for \n"
      "\t          surface in Focus.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+Alt+s: Input filename with coordinates\n"
      "\t              forming a segment (6 values) on \n"
      "\t              each line.\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Alt+s: Switch mouse buttons 1 and 3.\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "\t  S: Show all surface objects registered in DOv.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  t: talk to AFNI, toggle.\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+t: Force a resend of \n"
      "            surfaces to AFNI.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  T: Start listening for niml connections\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+u: Open SUMA controller.\n\n");   
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "\t  v: Show current surface viewer structure (cSV).\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  w: This option has been dropped.\n"
      "\t     Instead, use 'r' or 'R' recording options\n"
      "\t     or use a screen grab instead. \n"
      "\t     (like xv on unix systems, and grab on Macs.)\n");
   SS = SUMA_StringAppend (SS, 
      "\t  W: Write ascii files containing the NodeList,\n"
      "\t     the FaceSetList and the nodecolors of the \n"
      "\t     surface in focus.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Z/z: Zoom in/out\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  [: Show/Hide left hemisphere.\n"
      "\t  ]: Show/Hide right hemisphere.\n"
      "\t     Window title shows which \n"
      "\t     hemispheres are shown :LR:\n"
      "\t     :-R: :L-: or :--:\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  *: Smooth node colors by averaging with neighbors.\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "\t  @: Compute curvatures along principal directions \n"
      "\t     on the surface, results written to disk.\n\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "\t  (: Compute convexity of surface, \n"
      "\t     results written to disk.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  ,/. (think </>): Switch to next/previous view state.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  SPACE: Toggle between Mapping Reference and\n"
      "\t         Current view state.\n\n");

   SS = SUMA_StringAppend (SS, 
      "\t  L-R arrows: rotate about screen's Y axis\n");
   SS = SUMA_StringAppend (SS, 
      "\t  U-D arrows: rotate about screen's X axis\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Shift+L-R arrows: translate about screen's \n"
      "\t                    Y axis\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Shift+U-D arrows: translate about screen's \n"
      "\t                    X axis\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+L-R arrows: LR cardinal views\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+U-D arrows: IS cardinal views\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Ctrl+Shift+U-D arrows: AP cardinal views\n\n");

   SS = SUMA_StringAppend (SS, 
      "\t  F1: object axis (X-Red, Y-Green, Z-Blue), \n"
      "\t      toggle. \n");
   SS = SUMA_StringAppend (SS, 
      "\t  F2: screen axis (X-Red, Y-Green), toggle. \n");
   SS = SUMA_StringAppend (SS, 
      "\t  F3: cross hair, toggle. \n");
   SS = SUMA_StringAppend (SS, 
      "\t  F4: node selection highlight, toggle. \n");
   SS = SUMA_StringAppend (SS, 
      "\t  F5: FaceSet selection highlight, toggle.\n");
   SS = SUMA_StringAppend (SS, 
      "\t  F6: Viewer background color, toggle.\n");
   SS = SUMA_StringAppend (SS, 
      "\t  F7: Switch between color mixing modes.\n");
   SS = SUMA_StringAppend (SS, 
      "\t  F12: Time 20 scene renderings.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  HOME: reset view to startup\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  ESCAPE: close the surface viewer window.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, 
      "\t  Shft+ESCAPE: close all surface viewer windows.\n\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Mouse Controls:\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Button 1-Motion: rotation as if you were using\n"
      "\t                   a trackball.\n");
   SS = SUMA_StringAppend (SS, 
      "\t    Pure vertical motion is equivalent to using \n"
      "\t    the up/down arrow keys.\n");
   SS = SUMA_StringAppend (SS, 
      "\t    Pure horizontal motion is equivalent to using \n"
      "\t    the left/right arrow keys.\n");
   SS = SUMA_StringAppend (SS, 
      "\t    Of course, the advantage to using the mouse is \n"
      "\t    a continuous range of rotation angles and \n");
   SS = SUMA_StringAppend (SS, 
      "\t    simultaneous rotations about the screen's \n"
      "\t    X & Y axis.\n");
   SS = SUMA_StringAppend (SS, 
      "\t    This mode of rotation is similar to SGI's \n"
      "\t    ivview interface.\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Button 2-Motion: translation\n"); 
   SS = SUMA_StringAppend (SS, 
      "\t  Button 1+2-Motion OR \n"
      "\t   Shift+Button2-Motion: \n"
      "\t       Zoom in/out\n");
   SS = SUMA_StringAppend (SS, 
      "\t  Button 3-Press: picking \n");
   SS = SUMA_StringAppend (SS, 
      "\t  shft+Button 3-Press: ROI drawing \n"
      "\t                       (when in DrawROI mode)\n");
   SS = SUMA_StringAppend (SS, 
      "\t \n");
   SS = SUMA_StringAppend (SS, 
      "\t More help at \n"
      "\t http://afni.nimh.nih.gov/ssc/ziad/SUMA/SUMA_doc.htm\n");
   SS = SUMA_StringAppend (SS, 
      "\n");
   
   /* add latest additions */
   SS = SUMA_StringAppend (SS, "Current Version Info:\n");
   s = SUMA_New_Additions (0.0, 0);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
char * SUMA_New_Additions (float ver, SUMA_Boolean StampOnly)
{
   static char FuncName[]={"SUMA_New_Additions"};
   char *s = NULL;
   int i;
   SUMA_STRING *SS = NULL;
   float verv[] = {2.45, -1.0}; /* modify this dude and you must update SUMA_New_Additions_perver  
                                       Add to the left of the vector, leave the last value of -1 untouched*/
   
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
char * SUMA_New_Additions_perver (float ver, SUMA_Boolean StampOnly)
{
   static char FuncName[]={"SUMA_New_Additions_perver"};
   char *s = NULL;
   const int ti = 10000;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   switch ((int)(ver*ti)) {
      /* Must modify verv in SUMA_New_Additions when you touch this block */
      /*
      case (int)(XXX*ti):
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %.2f, DATEHERE\n", ver); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
            "New Programs:\n"
            "  + \n"
            "Modifications:\n"
            "  + \n");
         break; 
      */
      case (int)(2.45*ti):
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %.2f, Jan. 6 2004\n", ver); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
            "New Programs:\n"
            "  + SurfSmooth: Smoothes surface data or geometry\n"
            "  + inspec: Shows the contents of a spec file\n"
            "  + quickspec: Creates a minimal spec file for one\n"
            "             or a bunch of surfaces.\n"
            "Modifications:\n"
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
         SS = SUMA_StringAppend_va(SS, "++ %f? No such version, fool!\n", ver);
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
   s = SUMA_New_Additions (0.0, 0);
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
