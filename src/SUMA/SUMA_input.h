#ifndef SUMA_INPUT_INCLUDED
#define SUMA_INPUT_INCLUDED

/* These are for button or modifier key masks for state variables, some of these are not used, I am using Button1Mask and other prdefined ones instead */
	#define NO_BUTTON_MOTION 0
	#define BUTTON_1_MOTION 256 /* Button1Mask */
	#define BUTTON_2_MOTION 512 /* Button2Mask */
	#define BUTTON_3_MOTION 1024 /* Button3Mask */
	#define BUTTON_12_MOTION Button1Mask+Button2Mask /* Button1Mask+Button2Mask */

SUMA_Boolean SUMA_MarkLineSurfaceIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov);
void SUMA_momentum(XtPointer clientData, XtIntervalId *id);


#endif
