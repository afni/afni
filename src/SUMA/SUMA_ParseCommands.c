/*#define STAND_ALONE*/
#define DEBUG_3
#ifdef DEBUG_1
	#define DEBUG_2
	#define DEBUG_3
#endif
   
/* Header FILES */
   
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <assert.h>
   
#include <Xm/Form.h>    /* Motif Form widget. */
#include <Xm/Frame.h>   /* Motif Frame widget. */
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* For XA_RGB_DEFAULT_MAP. */
#include <X11/Xmu/StdCmap.h>  /* For XmuLookupStandardColormap. */
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>
#include <GL/GLwMDrawA.h>  /* Motif OpenGL drawing area. */

#include "SUMA_suma.h"

 
/* CODE */
   
   
   
/*!**
File : SUMA_ParseCommands.c
\author Ziad Saad
Date : Tue Feb 5 10:39:02 EST 2002
   
Purpose : 
   obtain the next command Scom from the string of commands S
   
   
Usage : 
		Ret = SUMA_GetNextCommand (char *S, char d, char term, char *Scom);
   
   
Input paramters : 
\param  S (char *) : String containing commands like "Initialize|ShowSurf|LightsON~" 
\param  d (char)  : character delimiting multiple commands ('|' in this example)
\param  term (char) : character terminating entire command ('~' in this example)
\param  Scom (char *): Null terminated string that will contain latest command (LightsON) in this example
                     S will be returned "Initialize|ShowSurf~"

Returns : 
\return   code of the command as defined for SUMA_ENGINE_CODE
   
Support : 
\sa   
\sa   
      
   
***/
int SUMA_GetNextCommand (char *S, char d, char term, char *Scom)
{/*SUMA_GetNextCommand*/
   char FuncName[100]; 
   int i=0, iBegin, iStop;
	/* initialize function name for verbose output */
   sprintf (FuncName,"SUMA_GetNextCommand");
	
	iStop = strlen(S)-1;
	
	/*fprintf(stdout,"%s %c %c\n", S, S[iStop], term);  */
	if (S[iStop] != term) {
		fprintf (stderr, "%s Error: Command poorly terminated!\n\a", FuncName);
		return (0);
	}
	/* Make sure character just before term is not d */
	if (S[iStop-1] == d) {
		S[iStop] = '\0';
		iStop -= 1;
		S[iStop] = term;
	}
	
	/* search for command delimiter */
	iBegin = iStop -1;
	while (iBegin > -1 && S[iBegin]!= d) --iBegin;
	++iBegin;
	
	/* copy command to Scom*/
	for (i=0; i< iStop - iBegin; ++i)  {
		/*fprintf(stdout,"%d %c\n", iBegin+i, S[iBegin+i]);*/
		Scom[i] = S[iBegin+i]; 
		S[iBegin+i] = '\0'; /*for esthetics */}
	
	/* seal strings with terminators */
	Scom[iStop-iBegin] = '\0';
	if (iBegin > 0) {
		S[iBegin-1] = term;
		iStop = iBegin-1;
	}
	else {
		S[iBegin] = term;
		iStop = iBegin;

	}
	
	/*get the code of the command*/
	return (SUMA_CommandCode(Scom));

}/*SUMA_GetNextCommand*/

int SUMA_CommandCode(char *Scom)
{	
	if (!strlen(Scom)) return (SE_Empty);
	if (strcmp(Scom,"~") == 0) return (SE_Empty);
	
	/*fprintf(stdout,"Looking for %s\n", Scom);*/
	if (!strcmp(Scom,"SetLookAt")) return(SE_SetLookAt);	
	if (!strcmp(Scom,"SetLookFrom")) return(SE_SetLookFrom);
	if (!strcmp(Scom,"Redisplay")) return(SE_Redisplay);
	if (!strcmp(Scom,"SetNodeColor")) return (SE_SetNodeColor);
	if (!strcmp(Scom,"FlipLight0Pos"))	return(SE_FlipLight0Pos);
	if (!strcmp(Scom,"GetNearestNode"))	return (SE_GetNearestNode);
	if (!strcmp(Scom,"SetLookAtNode"))	return (SE_SetLookAtNode);
	if (!strcmp(Scom,"SetRotMatrix"))	return (SE_SetRotMatrix);
	if (!strcmp(Scom,"SetCrossHair"))	return (SE_SetCrossHair);
	if (!strcmp(Scom,"ToggleCrossHair"))	return (SE_ToggleCrossHair);
	if (!strcmp(Scom,"HighlightNodes"))	return (SE_HighlightNodes);
	if (!strcmp(Scom,"ToggleShowSelectedNode"))	return (SE_ToggleShowSelectedNode);
	if (!strcmp(Scom,"SetSelectedNode"))	return (SE_SetSelectedNode);
	if (!strcmp(Scom,"SetSelectedFaceSet"))	return (SE_SetSelectedFaceSet);
	if (!strcmp(Scom,"ToggleShowSelectedFaceSet"))	return (SE_ToggleShowSelectedFaceSet);
	if (!strcmp(Scom,"ToggleTalkToAfni")) return (SE_ToggleTalkToAfni);
	if (!strcmp(Scom,"SetAfniCrossHair")) return (SE_SetAfniCrossHair);
	if (!strcmp(Scom,"SetAfniSurf")) return (SE_SetAfniSurf);
	if (!strcmp(Scom,"BindCrossHair")) return(SE_BindCrossHair);
	if (!strcmp(Scom,"Home")) return (SE_Home);
	/*if (!strcmp(Scom,"")) return(SE_);*/
	
	/* Last one is Bad Code */
	return (SE_BadCode);
	  
}

/*!**
File : SUMA_ParseCommands.c
\author Ziad Saad
Date : Tue Feb 5 10:39:02 EST 2002
   
Purpose : 
   Append or prepend command Scom to S
   
   
Usage : 
		Ret =  SUMA_RegisterCommand(char *S, char d, char term, char *Scom, SUMA_Boolean Prepend);
   
   
Input paramters : 
\param  S (char *) : String containing commands like "Initialize|ShowSurf~" MUST BE NULL TERMINATED
\param  d (char)  : character delimiting multiple commands ('|' in this example)
\param  term (char) : character terminating entire command ('~' in this example)
\param  Scom (char *): Null terminated string that will contain latest command (LightsON) in this example
                     S will be returned "Initialize|ShowSurf/LightsON~" if Prepend is YUP
							S will be returned "LightsON|Initialize|ShowSurf" is Prepend if NOPE
\param Prepend (SUMA_Boolean): append or prepend command

Returns : 
\return   NOPE for failure, YUP success
   
Support : 
\sa   
\sa   
      
   
***/
SUMA_Boolean SUMA_RegisterCommand (char *S, char d, char term, char *Scom, SUMA_Boolean Prepend)
{	int i, iStop, iorig, iStopNew, nCom;
	char FuncName[100];
	/* initialize function name for verbose output */
   sprintf (FuncName,"SUMA_RegisterCommand");
	
	iStop = strlen(S)-1;
		
	nCom = strlen(Scom);
	/*fprintf (stdout,"Scom->%s<-, length %d\n", Scom, nCom);*/
	if (strlen(Scom) + iStop + 2 > SUMA_MAX_COMMAND_LENGTH ) {
		fprintf (stderr, "%s Error: Resultant command longer than SUMA_MAX_COMMAND_LENGTH!\n\a", FuncName);
		return (NOPE);
	}
	if (S[iStop] != term) {
		fprintf (stderr, "%s Error: S improperly terminated!\n\a", FuncName);
		return (NOPE);
	}
	if (!Prepend) {
		/* add a delimiter */
		if (S[iStop-1] != d) { 
			S[iStop] = d;
			iStop += 1;
		}
		/*append the command */
		for (i=0; i <nCom; ++i) {
			S[iStop+i] = Scom[i];
		} 
		iStop += nCom;
		S[iStop] = term;
		S[iStop+1] = '\0';
		return (YUP);
	} else {
		/* move old string forward*/
		iStopNew = iStop+nCom+1;
		S[iStopNew+1] = '\0';
		iorig = 0;
		while (iorig <= iStop) {
			S[iStopNew-iorig] = S[iStop-iorig];
			++iorig;
		}
		S[iStopNew-iorig] = d;
		
		/*add new one */
		for (i=0; i < nCom; ++i) {
			S[i] = Scom[i];
		}
		iStop = iStopNew;
		return (YUP);
	}
}

int SUMA_EngineFieldCode(char *Scom)
{	
	
	if (!strlen(Scom)) return (SEF_Empty);
	
	/*fprintf(stdout,"Looking for %s\n", Scom);*/
	if (!strcmp(Scom,"fm")) return(SEF_fm);	
	if (!strcmp(Scom,"im")) return(SEF_im);	
	if (!strcmp(Scom,"fv3")) return(SEF_fv3);
	if (!strcmp(Scom,"iv3")) return(SEF_iv3);
	if (!strcmp(Scom,"fv15")) return(SEF_fv15);
	if (!strcmp(Scom,"iv15")) return(SEF_iv15);
	if (!strcmp(Scom,"i")) return(SEF_i);
	if (!strcmp(Scom,"f")) return (SEF_f);
	if (!strcmp(Scom,"s")) return (SEF_s);
	/*if (!strcmp(Scom,"")) return(SEF_);*/
	
	/* Last one is Bad Code */
	return (SEF_BadCode);
	  
}

int SUMA_EngineSourceCode (char *Scom)
{
	if (!strlen(Scom)) return (SES_Empty);
	if (!strcmp(Scom,"suma")) return(SES_Suma);
	if (!strcmp(Scom,"afni")) return(SES_Afni);
	
	/* got here? Unknown */
	return (SES_Unknown);
}  

void SUMA_EngineSourceString (char *Scom, int i)
{
	switch (i) {
		case SES_Empty:
			Scom[0]='\0';
			break;
		case SES_Suma:
			sprintf(Scom,"suma");
			break;
		case SES_Afni:
			sprintf(Scom, "afni");
			break;
		default:
			sprintf(Scom, "unknown");
			break;
	}
	return;
}
/*!
SUMA_Boolean SUMA_RegisterEngineData (SUMA_EngineData *MTI, char *Fldname, void *FldValp, char *DestName, char *SourceName, SUMA_Boolean PassByPointer)
\param MTI (SUMA_EngineData *) pointer to EngineData structure
\param Fldname (char *) Field name
\param FldValp (void *) Pointer to the value that is to be placed in Fldname
\param DestName (char *) Name of EngineCommand that the data in Fldname is destined to
\param PassByPointer (SUMA_Boolean) flag (YUP/NOPE), if YUP then assignment is done at the pointer level (MTI->Fld = FldValp)
                        if NOPE then space is allocated for Fldname and values are copied from FldValp[i][j] to MTI->Fld[i][j]

\ret YUP/NOPE

+PassByPointer option is only useful when dealing with fields that are/can be dynamically allocated like fm and fi. For fields like fv3 or iv15 then assignments are done by value and not pointers.
+You cannot set the value of a field unless the destination for the pre-existing data in that field has been reached								
+When passing by value for fields requiring allocation, like fm or fi, you must be sure that MTI->N_cols and MTI->N_rows are
set correctly before you call the function.

\sa SUMA_EngineDataFieldCode
\sa SUMA_ReleaseEngineData
\sa SUMA_InitializeEngineData
\sa SUMA_FreeEngineData
\sa EngineData
\sa SUMA_define.h

*/

SUMA_Boolean SUMA_RegisterEngineData (SUMA_EngineData *MTI, char *Fldname, void *FldValp, char *DestName, char *SourceName, SUMA_Boolean PassByPointer)
{ /* SUMA_RegisterEngineData*/
	int Dest, Fld, Src;
	char FuncName[100];
	
	sprintf(FuncName, "SUMA_RegisterEngineData");

	Dest = SUMA_CommandCode((char *)DestName);
	Fld = SUMA_EngineFieldCode((char *)Fldname);
	Src = SUMA_EngineSourceCode ((char *)SourceName);
	
	/* make sure Destination is good and wholesome*/
	switch (Dest) {
		case SE_BadCode:
			fprintf (SUMA_STDERR, "Error in %s: Bad code string.\n", FuncName);
			return (NOPE);
			break;
		case SE_Empty:
			fprintf (SUMA_STDERR, "Error in %s: Empty code string.\n", FuncName);
			return (NOPE);
			break;
		default:
			break;
	}
	
	/*fprintf(SUMA_STDOUT, "%s: Registering %s for %s\n", FuncName, Fldname, DestName);*/
	
	switch (Fld) { /* switch Fld */
		case SEF_fm:
			if (MTI->fm_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
				fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, MTI->fm_Dest);
				return (NOPE);
			}
			/* space available*/
			if (PassByPointer) {
				/* pass the pointer */
				MTI->fm = (float **)FldValp;
				MTI->fm_LocalAlloc = NOPE; /* allocation not done by Engine functions */
			}
			else { /* pass by value */
				if (MTI->fm != NULL) {
					fprintf(SUMA_STDERR, "Error %s: Passing by value and MTI->fm is not NULL. Clean up your act.\n", FuncName);
					return(NOPE);
				} 
				if (!MTI->N_rows || !MTI->N_cols) {
					fprintf(SUMA_STDERR, "Error %s: MTI->N_rows or MTI->N_cols is 0.\n", FuncName);
					return(NOPE);
				}
				MTI->fm = (float **)SUMA_allocate2D(MTI->N_rows, MTI->N_cols, sizeof(float));
				if (MTI->fm == NULL) {
					fprintf(SUMA_STDERR, "Error %s: Failed to allocate fm.\n", FuncName);
					return(NOPE);
				}
				MTI->fm_LocalAlloc = YUP; /* allocation done by Engine functions, this can be freed*/
				{/* copy the data */
					float **fm; 
					int i, j;
					fm = (float **)FldValp;
					for (i=0; i < MTI->N_rows; ++i) {
						for (j=0; j < MTI->N_cols; ++j) 
							MTI->fm[i][j] = fm[i][j];
					}
				}/* copy the data */
			}/* pass by value */
			/* set the new destination*/
			MTI->fm_Dest = Dest;
			MTI->fm_Source = Src;
			return (YUP);	
			break;
		case SEF_im:
			if (MTI->im_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
				fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, MTI->im_Dest);
				return (NOPE);
			}
			/* space available*/
			if (PassByPointer) {
				/* pass the pointer */
				MTI->im = (int **)FldValp;
				MTI->im_LocalAlloc = NOPE; /* allocation not done by Engine functions */
			}	else { /* pass by value */
				if (MTI->im != NULL) {
					fprintf(SUMA_STDERR, "Error %s: Passing by value and MTI->im is not NULL. Clean up your act.\n", FuncName);
					return(NOPE);
				} 
				if (!MTI->N_rows || !MTI->N_cols) {
					fprintf(SUMA_STDERR, "Error %s: MTI->N_rows or MTI->N_cols is 0.\n", FuncName);
					return(NOPE);
				}
				MTI->im = (int **)SUMA_allocate2D(MTI->N_rows, MTI->N_cols, sizeof(int));
				if (MTI->im == NULL) {
					fprintf(SUMA_STDERR, "Error %s: Failed to allocate im.\n", FuncName);
					return(NOPE);
				}
				MTI->im_LocalAlloc = YUP; /* allocation done by Engine functions, this can be freed*/
				{/* copy the data */
					int **im; 
					int i, j;
					im = (int **)FldValp;
					for (i=0; i < MTI->N_rows; ++i) {
						for (j=0; j < MTI->N_cols; ++j) 
							MTI->im[i][j] = im[i][j];
					}
				}/* copy the data */
			}/* pass by value */
			/* set the new destination*/
			MTI->im_Dest = Dest;
			MTI->im_Source = Src;
			return (YUP);	
			break;

		case SEF_i:
			if (MTI->i_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
				fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, MTI->i_Dest);
				return (NOPE);
			}
			{ /* assign by value */
				int *it;
				it = (int*)FldValp;
				MTI->i = *it;
			}
			MTI->i_Dest = Dest;
			MTI->i_Source = Src;
			return (YUP);	
			break;
			
		case SEF_f:
			if (MTI->f_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
				fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, MTI->f_Dest);
				return (NOPE);
			}
			{ /* assign by value */
				float *ft;
				ft = (float*)FldValp;
				MTI->f = *ft;
			}
			MTI->f_Dest = Dest;
			MTI->f_Source = Src;
			return (YUP);	
			break;

		case SEF_fv3:
			if (MTI->fv3_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
				fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, MTI->fv3_Dest);
				return (NOPE);
			}
			{ /* assign by value */
				float *fvt;
				int kt;
				fvt = (float*)FldValp;
				for (kt=0; kt < 3; ++kt) MTI->fv3[kt] = fvt[kt];
			}
			MTI->fv3_Dest = Dest;
			MTI->fv3_Source = Src;
			return (YUP);	
			break;

		case SEF_fv15:
			if (MTI->fv15_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
				fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, MTI->fv15_Dest);
				return (NOPE);
			}
			{ /* assign by value */
				float *fvt;
				int kt;
				fvt = (float*)FldValp;
				for (kt=0; kt < 15; ++kt) MTI->fv15[kt] = fvt[kt];
			}
			MTI->fv15_Dest = Dest;
			MTI->fv15_Source = Src;
			return (YUP);	
			break;
			
		case SEF_iv3:
			if (MTI->iv3_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
				fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, MTI->iv3_Dest);
				return (NOPE);
			}
			{ /* assign by value */
				int *ivt;
				int kt;
				ivt = (int*)FldValp;
				for (kt=0; kt < 3; ++kt) MTI->iv3[kt] = ivt[kt];
			}
			MTI->iv3_Dest = Dest;
			MTI->iv3_Source = Src;
			return (YUP);	
			break;
			
		case SEF_iv15:
			if (MTI->iv15_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
				fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, MTI->iv15_Dest);
				return (NOPE);
			}
			{ /* assign by value */
				int *ivt;
				int kt;
				ivt = (int*)FldValp;
				for (kt=0; kt < 15; ++kt) MTI->iv15[kt] = ivt[kt];
			}
			MTI->iv15_Dest = Dest;
			MTI->iv15_Source = Src;
			return (YUP);	
			break;

		case SEF_s:
			if (MTI->s_Dest != SEF_Empty) { /* Make sure the data in this field in not predestined */
				fprintf(SUMA_STDERR, "Error %s: field %s has a preset destination (%d).\n", FuncName, Fldname, MTI->s_Dest);
				return (NOPE);
			}
			{ /* assign by value */
				char *st;
				st = (char*)FldValp;
				if (strlen(st) < SUMA_MAX_STRING_LENGTH) {
					sprintf(MTI->s,"%s", st);
				} else {
					fprintf(SUMA_STDERR, "Error %s: string in FldValp is longer than SUMA_MAX_STRING_LENGTH.\n", FuncName);
					return (NOPE);
				}
			}
			MTI->s_Dest = Dest;
			MTI->s_Source = Src;
			return (YUP);	
			break;
			
		default:
			fprintf(SUMA_STDERR, "Error %s: Not setup for field %s yet.\n", FuncName, Fldname);
			return (NOPE);
			break;
	}/* switch Fld */
	 

}/* SUMA_RegisterEngineData*/

/*!
SUMA_Boolean SUMA_InitializeEngineData (SUMA_EngineData *MTI)
\param MTI (SUMA_EngineData *) pointer to allocated (or NULL) structure
	allocation for MTI is done if MTI is NULL
	structure fields fm and im are set to NULL, s to "" 
	i, v, fv3, iv3, fv15 and iv15 are initialized to 0 or 0.0
	_Dest are all set to SE_Empty
	N_rows and N_cols = 0
*/
SUMA_Boolean SUMA_InitializeEngineData (SUMA_EngineData *MTI)
{	int i;
	char FuncName[100];
	
	sprintf(FuncName, "SUMA_InitializeEngineData");
	
	if (MTI == NULL) MTI = malloc(sizeof(SUMA_EngineData));
		
	if (MTI == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate for MTI.\n", FuncName);
		return (NOPE);	
	}
	
	MTI->fm = NULL;
	MTI->im = NULL;
	MTI->N_rows = 0;
	MTI->N_cols = 0;
	MTI->i = 0;
	MTI->f = 0.0;
	MTI->iv3[0] = MTI->iv3[1] = MTI->iv3[2] = 0;
	MTI->fv3[0] = MTI->fv3[1] = MTI->fv3[2] = 0.0;
	for (i=0; i < 15; ++i) {
		MTI->fv15[i] = 0.0;
		MTI->iv15[i] = 0.0;
	}
	sprintf(MTI->s,"NOTHING");
	
	MTI->fm_Dest = MTI->im_Dest = MTI->i_Dest = MTI->f_Dest = MTI->iv3_Dest = MTI->fv3_Dest = \
	MTI->fv15_Dest = MTI->iv15_Dest = MTI->s_Dest = SE_Empty;
	
	MTI->fm_Source = MTI->im_Source = MTI->i_Source = MTI->f_Source = MTI->iv3_Source = MTI->fv3_Source = \
	MTI->fv15_Source = MTI->iv15_Source = MTI->s_Source = SES_Empty;

	return (YUP);
}

/*!
SUMA_Boolean SUMA_FreeEngineData (SUMA_EngineData *MTI)

free memory allocated for MTI in SUMA_InitializeEngineData

\param MTI (SUMA_EngineData *) pointer to SUMA_EngineData

\ret YUP/NOPE

if space for im or fm has been dynamically allocated in SUMA_RegisterEngineData
it is released. MTI itself is not freed
	
*/
SUMA_Boolean SUMA_FreeEngineData (SUMA_EngineData *MTI)
{
	char FuncName[100];
	
	sprintf(FuncName, "SUMA_FreeEngineData");
	if (MTI == NULL) {
		fprintf(SUMA_STDERR,"Error %s: MTI is null, nothing to do!\n", FuncName);
		return (NOPE);
	}
	
	/* check on Dynamic Memory Allocations needs */
	if (MTI->fm_LocalAlloc) {
		if (!MTI->N_rows || !MTI->N_cols) {
			fprintf(SUMA_STDERR,"Error %s: N_rows or N_cols are 0.\n", FuncName);
			return (NOPE);
		}
		if (MTI->fm == NULL) {
			fprintf(SUMA_STDERR,"Error %s: MTI->fm is NULL, not good here.\n", FuncName);
			return (NOPE);
		}
		/* OK, free MTI->fm */
		SUMA_free2D((char **)MTI->fm, MTI->N_rows);
	} 
	
	if (MTI->im_LocalAlloc) {
		if (!MTI->N_rows || !MTI->N_cols) {
			fprintf(SUMA_STDERR,"Error %s: N_rows or N_cols are 0.\n", FuncName);
			return (NOPE);
		}
		if (MTI->im == NULL) {
			fprintf(SUMA_STDERR,"Error %s: MTI->im is NULL, not good here.\n", FuncName);
			return (NOPE);
		}
		/* OK, free MTI->im */
		SUMA_free2D((char **)MTI->im, MTI->N_rows);
	} 
	
	/* good deal, DO NOT flush MTI in case it was not dynamically allocated*/
	return (YUP);
}

/*!
SUMA_Boolean SUMA_ReleaseEngineData (SUMA_EngineData *MTI, char *Location)

This function releases data fields that were detined to Location

\param MTI (SUMA_EngineData *) pointer to Engine data structure
\param Location (char *) location in SUMA_Engine, from which the function was called (one of the commands in SUMA_Engine)

\ret YUP/NOPE

Memory is freed for fm and im only if their assignment in SUMA_RegisterEngineData was done by value 

\sa SUMA_EngineDataFieldCode
\sa SUMA_ReleaseEngineData
\sa SUMA_InitializeEngineData
\sa SUMA_FreeEngineData
\sa EngineData
\sa SUMA_define.h
\sa SUMA_RegisterEngineData

*/
SUMA_Boolean SUMA_ReleaseEngineData (SUMA_EngineData *MTI, char *Location)
{/* SUMA_ReleaseEngineData*/
	int Loc;
	char FuncName[100];
	sprintf(FuncName, "SUMA_ReleaseEngineData");

	/* search through all fields and clear (or release) all those who should be */
	Loc = SUMA_CommandCode((char *)Location);
	
	/* make sure Destination is good and wholesome*/
	switch (Loc) {
		case SE_BadCode:
			fprintf (SUMA_STDERR, "Error in %s: Bad code string.\n", FuncName);
			return (NOPE);
			break;
		case SE_Empty:
			fprintf (SUMA_STDERR, "Error in %s: Empty code string.\n", FuncName);
			return (NOPE);
			break;
		default:
			break;
	}

	/*fprintf(SUMA_STDOUT,"%s : Releasing location %s\n", FuncName, Location);*/
	/* go through all the fields*/
	/* fm */
	if (MTI->fm_Dest == Loc) {
		/* needs to be released */
		if (MTI->fm_LocalAlloc) { /* locally allocated */
			/* must be freed */
			if (!MTI->N_rows || !MTI->N_cols) {
				fprintf (SUMA_STDERR, "Error in %s: MTI->N_rows or MTI->N_cols is 0 .\n", FuncName);
				return (NOPE);
			}
			if (MTI->fm == NULL) {
				fprintf (SUMA_STDERR, "Error in %s: fm is null already. This should not be .\n", FuncName);
				return (NOPE);
			}
			SUMA_free2D((char **)MTI->fm, MTI->N_rows);
			MTI->N_rows = MTI->N_cols = 0; 
			MTI->fm = NULL;
			MTI->fm_Dest = SE_Empty;
			MTI->fm_Source = SES_Empty;
		} /* locally allocated */ else { /* passed by pointer */
			MTI->fm = NULL; 
			MTI->N_rows = MTI->N_cols = 0; 
			MTI->fm_Dest = SE_Empty;
			MTI->fm_Source = SES_Empty;
		}/* passed by pointer */
	}
	
	/*im*/
	if (MTI->im_Dest == Loc) {
		/* needs to be released */
		if (MTI->im_LocalAlloc) { /* locally allocated */
			/* must be freed */
			if (!MTI->N_rows || !MTI->N_cols) {
				fprintf (SUMA_STDERR, "Error in %s: MTI->N_rows or MTI->N_cols is 0 .\n", FuncName);
				return (NOPE);
			}
			if (MTI->im == NULL) {
				fprintf (SUMA_STDERR, "Error in %s: im is null already. This should not be .\n", FuncName);
				return (NOPE);
			}
			SUMA_free2D((char **)MTI->im, MTI->N_rows);
			MTI->N_rows = MTI->N_cols = 0; 
			MTI->im = NULL;
			MTI->im_Dest = SE_Empty;
			MTI->im_Source = SES_Empty;
		} /* locally allocated */ else { /* passed by pointer */
			MTI->im = NULL; 
			MTI->N_rows = MTI->N_cols = 0; 
			MTI->im_Dest = SE_Empty;
			MTI->im_Source = SES_Empty;
		}/* passed by pointer */
	}

	/* i */
	if (MTI->i_Dest == Loc) {
		MTI->i_Dest = SE_Empty;
		MTI->i_Source = SES_Empty;
	}

	/* iv3 */
	if (MTI->iv3_Dest == Loc) {
		MTI->iv3_Dest = SE_Empty;
		MTI->iv3_Source = SES_Empty;
	}
	
	/* iv15 */
	if (MTI->iv15_Dest == Loc) {
		MTI->iv15_Dest = SE_Empty;
		MTI->iv15_Source = SES_Empty;
	}
	
	/* f */
	if (MTI->f_Dest == Loc) {
		MTI->f_Dest = SE_Empty;
		MTI->f_Source = SES_Empty;
	}

	/* fv3 */
	if (MTI->fv3_Dest == Loc) {
		MTI->fv3_Dest = SE_Empty;
		MTI->fv3_Source = SES_Empty;
	}
	
	/* fv15 */
	if (MTI->fv15_Dest == Loc) {
		MTI->fv15_Dest = SE_Empty;
		MTI->fv15_Source = SES_Empty;
	}
	
	/*s*/
	if (MTI->s_Dest == Loc) {
		MTI->s_Dest = SE_Empty;
		MTI->s_Source = SES_Empty;
	}

	/* return, tout va bien */
	return (YUP);
}
#ifdef STAND_ALONE
void usage ()
   
  {/*Usage*/
          printf ("\n\33[1mUsage: \33[0m SUMA_GetNextCommand ..... \n");
          printf ("\t ..... \n\n");
          printf ("\t To Compile:\ngcc -DSTAND_ALONE -Wall -o $1 $1.c -I/usr/X11R6/include -I./\n");
          printf ("\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \tTue Feb 5 10:39:02 EST 2002 \n");
          exit (0);
  }/*Usage*/
   
int main (int argc,char *argv[])
{/* Main */
   char FuncName[100]; 
   char Scom[500], S[500], term, d;

   /* initialize Main function name for verbose output */
   sprintf (FuncName,"SUMA_GetNextCommand-Main-");
	
	term = '~';
	d = '|';
   
   if (argc < 2)
       {
          usage ();
          exit (1);
       }
   
	/* copy argc into string */
	sprintf (S,"%s", argv[1]);
	fprintf(stderr,"INITIAL: %s, \n", S);

	if(!SUMA_RegisterCommand (S,  d, term, "Newly Registered Append", NOPE)) {
		fprintf (stderr, "%s Error: Failed to register new command\n", FuncName);
	}
	if(!SUMA_RegisterCommand (S,  d, term, "Newly Registered Prepend", YUP)) {
		fprintf (stderr, "%s Error: Failed to register new command\n", FuncName);
	}

	/*repeat until nothing left */
	fprintf(stderr,"%s\n", S);
	while (SUMA_GetNextCommand (S, d, term, Scom) != SE_Empty)  {
		 fprintf (stdout, "Command->%s<-\n", Scom);	
		 fprintf(stderr,"%s\n", S);
	}
return(0);
}/* Main */
#endif
