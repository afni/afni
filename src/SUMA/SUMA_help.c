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

   SS = SUMA_StringAppend (SS, "\nKeyboard Controls\n");
   SS = SUMA_StringAppend (SS, "\t  a: attenuation by background, toggle.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  B: Backface culling, toggle.\n");
   SS = SUMA_StringAppend (SS, "\t  b: background color, toggle.\n");
   SS = SUMA_StringAppend (SS, "\t  c: node color file.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  d: Show all DO objects in DOv.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t   Alt+e: Look for OpenGL errors.\n"); 
   SS = SUMA_StringAppend (SS, "\t  f: functional overlay, toggle.\n");
   SS = SUMA_StringAppend (SS, "\t  F: Flip light position between +z and -z.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  H: Highlight nodes inside a specified box.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  j: Set the cross hair to a certain node on SO in Focus.\n\t    Does not update in other viewers\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t   ctrl+j: Set the cross hair's XYZ location. \n\t    Does not update in other viewers\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t   alt+j: Set the Focus node. Cross hair's XYZ remain unchanged.\n\t    Does not update in other viewers\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  J: Set the selected FaceSet on SO in Focus.\n\t    Does not update in other viewers.\n");
   SS = SUMA_StringAppend (SS, "\t  h: NO LONGER USED\n");
   SS = SUMA_StringAppend (SS, "\t   Ctrl+h: help message\n");
   SS = SUMA_StringAppend (SS, "\t  l: look at point\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t   ctrl+l: Switch locking mode for all viewers between:\n No Lock, Index Lock and XYZ Lock.\nThe switching is order is based on the lock of the first viewer.");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  L: look from point\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t   Ctrl+M: Show memory trace if Debug flag MemTrace is on. \n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t           (requires compilation with SUMA_MEMTRACE_FLAG 1).\n");
   SS = SUMA_StringAppend (SS, "\t  m: momentum, toggle\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  n: bring a node to direct view (does not work yet)\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t   Ctrl+n: Open a new surface viewer window.\n");
   SS = SUMA_StringAppend (SS, "\t  p: rendering mode (Fill, Line, Points), switch.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  s: NO LONGER IN USE. Use View->Surface Controller->More.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t   Ctrl+Alt+s: Input filename with coordinates forming a segment (6 values) on each line.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  S: Show all surface objects registered in DOv.\n");
   SS = SUMA_StringAppend (SS, "\t  t: talk to AFNI, toggle.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t   Ctrl+t: Force a resend of surfaces to AFNI.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  v: Show current surface viewer structure (cSV).\n");
   SS = SUMA_StringAppend (SS, "\t  w: Write the rendered scene to an image file on disk (Surface_Label*.eps or suma_img*.eps).\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  W: Write ascii files containing the NodeList, the FaceSetList and the nodecolors of the surface in focus.\n");
   SS = SUMA_StringAppend (SS, "\t  Z/z: Zoom in/out\n");

   SS = SUMA_StringAppend (SS, "\t  *: Smooth node colors by averaging with neighbors.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  @: Compute curvatures along principal directions on the surface, results written to disk.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t  (: Compute convexity of surface, results written to disk.\n");
   SS = SUMA_StringAppend (SS, "\t  ,/. (think </>): Switch to next/previous view state.\n");
   SS = SUMA_StringAppend (SS, "\t  SPACE: Toggle between Mapping Reference and Current view state.\n");

   SS = SUMA_StringAppend (SS, "\t  L-R arrows: rotate about screen's Y axis\n");
   SS = SUMA_StringAppend (SS, "\t  U-D arrows: rotate about screen's X axis\n");
   SS = SUMA_StringAppend (SS, "\t  Shift+L-R arrows: translate about screen's Y axis\n");
   SS = SUMA_StringAppend (SS, "\t  Shift+U-D arrows: translate about screen's X axis\n");
   SS = SUMA_StringAppend (SS, "\t  Ctrl+L-R arrows: LR cardinal views\n");
   SS = SUMA_StringAppend (SS, "\t  Ctrl+U-D arrows: IS cardinal views\n");
   SS = SUMA_StringAppend (SS, "\t  Ctrl+Shift+U-D arrows: AP cardinal views\n");

   SS = SUMA_StringAppend (SS, "\t  F1: object axis (X-Red, Y-Green, Z-Blue), toggle. \n");
   SS = SUMA_StringAppend (SS, "\t  F2: screen axis (X-Red, Y-Green), toggle. \n");
   SS = SUMA_StringAppend (SS, "\t  F3: cross hair, toggle. \n");
   SS = SUMA_StringAppend (SS, "\t  F4: node selection highlight, toggle. \n");
   SS = SUMA_StringAppend (SS, "\t  F5: FaceSet selection highlight, toggle.\n");
   SS = SUMA_StringAppend (SS, "\t  F6: Viewer background color, toggle.\n");
   SS = SUMA_StringAppend (SS, "\t  F12: Time 20 scene renderings.\n");
   SS = SUMA_StringAppend (SS, "\t  HOME: reset view to startup\n");
   SS = SUMA_StringAppend (SS, "\t  ESCAPE: close the surface viewer window.\n");
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend (SS, "\t   Shft+ESCAPE: close all surface viewer windows.\n");
   SS = SUMA_StringAppend (SS, "\t  Mouse Controls:\n");
   SS = SUMA_StringAppend (SS, "\t  Button 1-Motion: rotation as if you were using a trackball.\n");
   SS = SUMA_StringAppend (SS, "\t    Pure vertical motion is equivalent to using the up/down arrow keys.\n");
   SS = SUMA_StringAppend (SS, "\t    Pure horizontal motion is equivalent to using the left/right arrow keys.\n");
   SS = SUMA_StringAppend (SS, "\t    Of course, the advantage to using the mouse is a continuous range of rotation \n");
   SS = SUMA_StringAppend (SS, "\t    angles and simultaneous rotations about the screen's X & Y axis.\n");
   SS = SUMA_StringAppend (SS, "\t    This mode of rotation is similar to SGI's ivview interface.\n");
   SS = SUMA_StringAppend (SS, "\t  Button 2-Motion: translation\n"); 
   SS = SUMA_StringAppend (SS, "\t  Button 1+2-Motion OR Shift+Button2-Motion: Zoom in/out\n");
   SS = SUMA_StringAppend (SS, "\t  Button 3-Press: picking \n");
   SS = SUMA_StringAppend (SS, "\n");
   SS = SUMA_StringAppend (SS, "More help at http://afni.nimh.nih.gov/ssc/ziad/SUMA/SUMA_doc.htm\n");
   SS = SUMA_StringAppend (SS, "\n");
      

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
SUMA version 
*/
void SUMA_Version (FILE *Out)
{
	if (Out == NULL) {
		Out = stdout;
	}
	fprintf (Out, "\n\tSUMA version 1.2, Tue Aug 20 2002\n");
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
