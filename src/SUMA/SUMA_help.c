#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 

/*!
Controls help message
*/
void SUMA_help_message(FILE *Out)
{
	if (Out == NULL) {
		Out = stdout;
	}
    fprintf (Out, "\nKeyboard Controls\n");
	 fprintf (Out, "\t  a: attenuation by background, toggle.\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t  B: Backface culling, toggle.\n");
	 fprintf (Out, "\t  b: background color, toggle.\n");
	 fprintf (Out, "\t  c: node color file.\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t  d: Show all DO objects in DOv.\n");
	 fprintf (Out, "\t  f: functional overlay, toggle.\n");
	 fprintf (Out, "\t  F: Flip light position between +z and -z.\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t  H: Highlight nodes inside a specified box.\n");
	 fprintf (Out, "\t  h: help message\n");
	 fprintf (Out, "\t   Ctrl+h: Debug flags\n");
	 fprintf (Out, "\t  l: look at point\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t  L: look from point\n");
	 fprintf (Out, "\t  m: momentum, toggle\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t  n: bring a node to direct view (does not work yet)\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t   Ctrl+n: Open a new surface viewer windows.\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t  s: Show surface object structures in ShowDO vector.\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t  S: Show all surface objects registered in DOv.\n");
	 fprintf (Out, "\t  t: talk to AFNI, toggle.\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t  v: Show current surface viewer structure (cSV).\n");
	 fprintf (Out, "\t  w: Write the rendered scene to an image file on disk (suma_img*.eps).\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t  W: Write ascii files containing the NodeList and the FaceSetList of the surface in focus.\n");
	 fprintf (Out, "\t  Z/z: Zoom in/out\n");
	 
	 fprintf (Out, "\t  *: Smooth node colors by averaging with neighbors.\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t  @: Compute curvatures along principal directions on the surface, results written to disk.\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t  (: Compute convexity of surface, results written to disk.\n");
	 fprintf (Out, "\t  ,/. (think </>): Switch to next/previous view state.\n");
	 fprintf (Out, "\t  SPACE: Toggle between Mapping Reference and Current view state.\n");
	 
	 fprintf (Out, "\t  L-R arrows: rotate about screen's Y axis\n");
	 fprintf (Out, "\t  U-D arrows: rotate about screen's X axis\n");
	 fprintf (Out, "\t  Shift+L-R arrows: translate about screen's Y axis\n");
	 fprintf (Out, "\t  Shift+U-D arrows: translate about screen's X axis\n");
	 fprintf (Out, "\t  Ctrl+L-R arrows: LR cardinal views\n");
	 fprintf (Out, "\t  Ctrl+U-D arrows: IS cardinal views\n");
	 fprintf (Out, "\t  Ctrl+Shift+U-D arrows: AP cardinal views\n");

	 fprintf (Out, "\t  F1: object axis (X-Red, Y-Green, Z-Blue), toggle. \n");
	 fprintf (Out, "\t  F2: screen axis (X-Red, Y-Green), toggle. \n");
	 fprintf (Out, "\t  F3: cross hair, toggle. \n");
	 fprintf (Out, "\t  F4: node selection highlight, toggle. \n");
	 fprintf (Out, "\t  F5: FaceSet selection highlight, toggle.\n");
	 fprintf (Out, "\t  F12: Time 20 scene renderings.\n");
	 fprintf (Out, "\t  HOME: reset view to startup\n");
	 fprintf (Out, "\t  ESCAPE: close the surface viewer window.\n");
	 if (SUMAg_CF->Dev) fprintf (Out, "\t   Shft+ESCAPE: close all surface viewer windows.\n");
	 fprintf (Out, "\t  Mouse Controls:\n");
	 fprintf (Out, "\t  Button 1-Motion: rotation as if you were using a trackball.\n");
	 fprintf (Out, "\t    Pure vertical motion is equivalent to using the up/down arrow keys.\n");
	 fprintf (Out, "\t    Pure horizontal motion is equivalent to using the left/right arrow keys.\n");
	 fprintf (Out, "\t    Of course, the advantage to using the mouse is a continuous range of rotation \n");
	 fprintf (Out, "\t    angles and simultaneous rotations about the screen's X & Y axis.\n");
	 fprintf (Out, "\t    This mode of rotation is similar to SGI's ivview interface.\n");
	 fprintf (Out, "\t  Button 2-Motion: translation\n"); 
	 fprintf (Out, "\t  Button 1+2-Motion OR Shift+Button2-Motion: Zoom in/out\n");
	 fprintf (Out, "\t  Button 3-Press: picking \n");
	 fprintf (Out, "\n");
	 fprintf (Out, "More help at http://afni.nimh.nih.gov/ssc/ziad/SUMA/SUMA_doc.htm\n");
	 fprintf (Out, "\n");
	 return;
}

/*!
SUMA version 
*/
void SUMA_Version (FILE *Out)
{
	if (Out == NULL) {
		Out = stdout;
	}
	fprintf (Out, "\n\tSUMA version 1.0, Fri Jun 21 2002\n");
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
