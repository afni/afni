#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <Xm/Form.h>    /* Motif Form widget. */
#include <Xm/Frame.h>   /* Motif Frame widget. */
#include <X11/keysym.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>  /* For XA_RGB_DEFAULT_MAP. */
#include <X11/Xmu/StdCmap.h>  /* For XmuLookupStandardColormap. */
#include <math.h>
#include <GL/gl.h>
#include <GL/glu.h>
#include <GL/glx.h>
#include <GL/GLwMDrawA.h>  /* Motif OpenGL drawing area. */

#include "SUMA_suma.h"

/* This is used to hold the functions that manipulate SV, Surface Viewer Structures */
/*!
Create a SurfaceViewer data structure
*/
SUMA_SurfaceViewer *SUMA_Alloc_SurfaceViewer_Struct (int N)
{
	SUMA_SurfaceViewer *SV, *SVv;
	static char FuncName[]={"SUMA_Alloc_SurfaceViewer_Struct"};
	int i, j;
	
	SVv =  (SUMA_SurfaceViewer *)malloc(sizeof(SUMA_SurfaceViewer)*N);
	if (SVv == NULL) {
		fprintf(SUMA_STDERR,"Error %s:: Failed to malloc SV\n", FuncName);
		return (NULL);
	}
	for (i=0; i < N; ++i) {
		SV = &SVv[i];
		
		SV->N_GVS = SUMA_N_STANDARD_VIEWS;
		SV->GVS = (SUMA_GEOMVIEW_STRUCT *)malloc(sizeof(SUMA_GEOMVIEW_STRUCT)*SV->N_GVS);
		if (!SV->GVS) {
			fprintf(SUMA_STDERR,"Error %s: Could not allocate for N_GVS.\n", FuncName);
			return (NULL);
		}
		SV->StdView = SUMA_3D; /* default */
		
		/* set the standards for all viewing modes here */
		SV->verbose = 1;
		SV->Aspect = 1.0;
		SV->FOV = NULL;
		for (j=0; j < SV->N_GVS; ++j) {
			switch (j) {
				case SUMA_2D_Z0:
					SV->GVS[j].currentQuat[0] = 0.252199;
					SV->GVS[j].currentQuat[1] = -0.129341;
					SV->GVS[j].currentQuat[2] = -0.016295;
					SV->GVS[j].currentQuat[3] = 0.958854;

					SV->GVS[j].ApplyMomentum = False;

					SV->GVS[j].MinIdleDelta = 1;
					SV->GVS[j].TranslateGain = TRANSLATE_GAIN;
					SV->GVS[j].ArrowtranslateDeltaX = ARROW_TRANSLATE_DELTAX;
					SV->GVS[j].ArrowtranslateDeltaY = ARROW_TRANSLATE_DELTAY;

					SV->GVS[j].ViewCamUp[0] = 0.0;
					SV->GVS[j].ViewCamUp[1] = 1.0;
					SV->GVS[j].ViewCamUp[2] = 0.0;

					SV->GVS[j].ViewFrom[0] = 0.0;
					SV->GVS[j].ViewFrom[1] = 0.0;
					SV->GVS[j].ViewFrom[2] = SUMA_DEFAULT_VIEW_FROM;

					SV->GVS[j].ViewCenter[0] = 0.0;
					SV->GVS[j].ViewCenter[1] = 0.0;
					SV->GVS[j].ViewCenter[2] = 0.0;

					SV->GVS[j].RotaCenter[0] = 0.0;
					SV->GVS[j].RotaCenter[1] = 0.0;
					SV->GVS[j].RotaCenter[2] = 0.0;
					break;
				case SUMA_3D:
					SV->GVS[j].currentQuat[0] = 0.252199;
					SV->GVS[j].currentQuat[1] = -0.129341;
					SV->GVS[j].currentQuat[2] = -0.016295;
					SV->GVS[j].currentQuat[3] = 0.958854;

					SV->GVS[j].ApplyMomentum = False;

					SV->GVS[j].MinIdleDelta = 1;
					SV->GVS[j].TranslateGain = TRANSLATE_GAIN;
					SV->GVS[j].ArrowtranslateDeltaX = ARROW_TRANSLATE_DELTAX;
					SV->GVS[j].ArrowtranslateDeltaY = ARROW_TRANSLATE_DELTAY;

					SV->GVS[j].ViewCamUp[0] = 0.0;
					SV->GVS[j].ViewCamUp[1] = 1.0;
					SV->GVS[j].ViewCamUp[2] = 0.0;

					SV->GVS[j].ViewFrom[0] = 0.0;
					SV->GVS[j].ViewFrom[1] = 0.0;
					SV->GVS[j].ViewFrom[2] = 0.0;

					SV->GVS[j].ViewCenter[0] = 0.0;
					SV->GVS[j].ViewCenter[1] = 0.0;
					SV->GVS[j].ViewCenter[2] = 0.0;

					SV->GVS[j].RotaCenter[0] = 0.0;
					SV->GVS[j].RotaCenter[1] = 0.0;
					SV->GVS[j].RotaCenter[2] = 0.0;
					break;
				default:
					fprintf(SUMA_STDERR,"Error %s: Undefined viewing mode.\n", FuncName);
					return (NULL);
					
			}
		}
		

		SV->light0_position[0] = 0.0;
		SV->light0_position[1] = 0.0;
		
		SV->light0_position[2] = 1.0 * SUMA_INTITIAL_LIGHT0_SWITCH; 
		SV->light0_position[3] = 0.0;

		SV->light1_position[0] = 1.0;
		SV->light1_position[1] = 1.0;
		SV->light1_position[2] = 1.0;
		SV->light1_position[3] = 0.0;

		SV->WindWidth = 350;
		SV->WindHeight = 350;

		
		SV->ShowDO = (int *)calloc(sizeof(int), SUMA_MAX_DISPLAYABLE_OBJECTS);
		if (SV->ShowDO == NULL) {
			fprintf(stderr,"Error SUMA_Alloc_SurfaceViewer_Struct: Failed to malloc SV->ShowDO\n");
			return (NULL);
		}
		SV->N_DO = 0; /* Nothing is registered with the viewer yet */

		SV->ShowEyeAxis = 1;
		SV->ShowMeshAxis = 1;
		
		SV->Ch = SUMA_Alloc_CrossHair ();
		if (SV->Ch == NULL) {
			fprintf(stderr,"Error SUMA_Alloc_SurfaceViewer_Struct: Failed in SUMA_Alloc_CrossHair\n");
			return (NULL); 
		} else SV->ShowCrossHair = 1;
		
		SV->X = (SUMA_X *)malloc(sizeof(SUMA_X));
		if (SV->X == NULL) {
			fprintf(stderr,"Error SUMA_Alloc_SurfaceViewer_Struct: Failed to malloc SV->X\n");
			return (NULL);
		}

		SV->X->MOMENTUMID = 0;
		SV->X->REDISPLAYPENDING = 0;
		SV->X->DOUBLEBUFFER = True;
		SV->X->WIDTH = SV->X->HEIGHT = 300; /* if you change this, make sure you do so for fallbackResources in SUMA_display */

		SV->Focus_SO_ID = -1;
		SV->Focus_DO_ID = -1;
		
		SV->TalkToAfni = NOPE;
		
		SV->VSv = NULL;
		SV->N_VSv = 0;
		SV->LastNonMapStateID = -1;
		
		SV->PolyMode = 0;
		
		#if SUMA_BACKFACE_CULL
			SV->BF_Cull = YUP;
		#else
			SV->BF_Cull = NOPE;
		#endif

		SV->ShowForeground = YUP;
		SV->ShowBackground = YUP;
		SV->Back_Modfact = SUMA_BACKGROUND_MODULATION_FACTOR;

	}
	return (SVv);
}

SUMA_Boolean SUMA_Free_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV)
{
	static char FuncName[]={"SUMA_Free_SurfaceViewer_Struct"};
	int i;
	
	if (SV->Ch) SUMA_Free_CrossHair (SV->Ch);
	if (SV->X) free (SV->X);
	if (SV->ShowDO) free (SV->ShowDO);
	if (SV->VSv) {
		for (i=0; i < SV->N_VSv; ++i) {
			if (!SUMA_Free_ViewState (&(SV->VSv[i]))) {
				fprintf (SUMA_STDERR,"Error %s: failed in SUMA_Free_ViewState.\n", FuncName);
			}
		}
	}
	if (SV->GVS) free (SV->GVS);
	if (SV->State) SV->State = NULL; /* never free that one */ 
	return(YUP);
}

SUMA_Boolean SUMA_Free_SurfaceViewer_Struct_Vect (SUMA_SurfaceViewer *SVv, int N)
{
	int i;
	SUMA_Boolean Ret= YUP;
	
	for (i=0; i < N; ++i)  {
		if (&SVv[i] != NULL) {
			Ret = Ret * SUMA_Free_SurfaceViewer_Struct (&SVv[i]);
		}
	}
	return(Ret);
}

/*!
Updates the View Center and view from of SV based on the contents of ShowDO
*/

SUMA_Boolean SUMA_UpdateViewPoint (SUMA_SurfaceViewer *SV, SUMA_DO *dov, int N_dov)
{
	int i, do_id, TotWeight;
	float NewCenter[3];
	SUMA_SurfaceObject *so_op;
	
	NewCenter[0] = 0.0;
	NewCenter[1] = 0.0;
	NewCenter[2] = 0.0;
	TotWeight = 0;
	
	i = 0;
	while (i < SV->N_DO) {
		do_id = SV->ShowDO[i];
		switch (dov[do_id].ObjectType) {
			case SO_type:
				so_op = (SUMA_SurfaceObject *)dov[do_id].OP;
				if (so_op->ViewCenterWeight) {
					NewCenter[0] += so_op->ViewCenterWeight*so_op->Center[0];
					NewCenter[1] += so_op->ViewCenterWeight*so_op->Center[1];
					NewCenter[2] += so_op->ViewCenterWeight*so_op->Center[2];
					TotWeight += so_op->ViewCenterWeight;
				}
				break;
			default:
				break;
		} 
		++i;
	}
	if (TotWeight) {
		SV->GVS[SV->StdView].ViewCenter[0] = NewCenter[0]/(float)TotWeight;
		SV->GVS[SV->StdView].ViewCenter[1] = NewCenter[1]/(float)TotWeight;
		SV->GVS[SV->StdView].ViewCenter[2] = NewCenter[2]/(float)TotWeight;
		SV->GVS[SV->StdView].ViewFrom[0] = SV->GVS[SV->StdView].ViewCenter[0];
		SV->GVS[SV->StdView].ViewFrom[1] = SV->GVS[SV->StdView].ViewCenter[1];
		SV->GVS[SV->StdView].ViewFrom[2] = SV->GVS[SV->StdView].ViewCenter[2]+SUMA_DEFAULT_VIEW_FROM;	
		SV->GVS[SV->StdView].ViewDistance = SUMA_DEFAULT_VIEW_FROM;	
		
	} else
	{/* default back to o.o, o.o, o.o */
		SV->GVS[SV->StdView].ViewCenter[0] = SV->GVS[SV->StdView].ViewCenter[1] = SV->GVS[SV->StdView].ViewCenter[2] = 0.0;
		SV->GVS[SV->StdView].ViewFrom[0] = SV->GVS[SV->StdView].ViewFrom[1] = 0.0; SV->GVS[SV->StdView].ViewFrom[2] = SUMA_DEFAULT_VIEW_FROM;
		SV->GVS[SV->StdView].ViewDistance = SUMA_DEFAULT_VIEW_FROM;	
	}
	
		/* Store that info in case subjects change things */
		SV->GVS[SV->StdView].ViewCenterOrig[0] = SV->GVS[SV->StdView].ViewCenter[0];
		SV->GVS[SV->StdView].ViewCenterOrig[1] = SV->GVS[SV->StdView].ViewCenter[1];
		SV->GVS[SV->StdView].ViewCenterOrig[2] = SV->GVS[SV->StdView].ViewCenter[2];
		SV->GVS[SV->StdView].ViewFromOrig[0] = SV->GVS[SV->StdView].ViewFrom[0];
		SV->GVS[SV->StdView].ViewFromOrig[1] = SV->GVS[SV->StdView].ViewFrom[1];
		SV->GVS[SV->StdView].ViewFromOrig[2] = SV->GVS[SV->StdView].ViewFrom[2];

	return (YUP);
	
	
}
/*!
Updates the Rotation Center of SV based on the contents of ShowDO
*/
SUMA_Boolean SUMA_UpdateRotaCenter (SUMA_SurfaceViewer *SV, SUMA_DO *dov, int N_dov)
{
	int i, do_id, TotWeight;
	float NewCenter[3];
	SUMA_SurfaceObject *so_op;
	
	NewCenter[0] = 0.0;
	NewCenter[1] = 0.0;
	NewCenter[2] = 0.0;
	TotWeight = 0;
	
	i = 0;
	while (i < SV->N_DO) {
		do_id = SV->ShowDO[i];
		switch (dov[do_id].ObjectType) {
			case SO_type:
				so_op = (SUMA_SurfaceObject *)dov[do_id].OP;
				if (so_op->RotationWeight) {
					NewCenter[0] += so_op->RotationWeight*so_op->Center[0];
					NewCenter[1] += so_op->RotationWeight*so_op->Center[1];
					NewCenter[2] += so_op->RotationWeight*so_op->Center[2];
					TotWeight += so_op->RotationWeight;
				}
				break;
			default:
				break;
		} 
		++i;
	}
	if (TotWeight) {
		SV->GVS[SV->StdView].RotaCenter[0] = NewCenter[0]/(float)TotWeight;
		SV->GVS[SV->StdView].RotaCenter[1] = NewCenter[1]/(float)TotWeight;
		SV->GVS[SV->StdView].RotaCenter[2] = NewCenter[2]/(float)TotWeight;
	} else
	{/* default back to o.o, o.o, o.o */
		SV->GVS[SV->StdView].RotaCenter[0] = SV->GVS[SV->StdView].RotaCenter[1] = SV->GVS[SV->StdView].RotaCenter[2] = 0.0;
	}
	return (YUP);
	
}

/*!
output the state variable contents of the Surface Viewer 
*/
void Show_SUMA_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV, FILE *Out)
{
	int i;

	if (Out == NULL) Out = stdout;
	
	fprintf(Out,"\nSV contents:\n");
	fprintf(Out,"\tverbose = %d\n", SV->verbose);
	fprintf(Out,"\tAspect = %f\n", SV->Aspect);
	fprintf(Out,"\tViewFrom = [%f %f %f]\n", SV->GVS[SV->StdView].ViewFrom[0], SV->GVS[SV->StdView].ViewFrom[1], SV->GVS[SV->StdView].ViewFrom[2]);
	fprintf(Out,"\tViewFromOrig = [%f %f %f]\n", SV->GVS[SV->StdView].ViewFromOrig[0], SV->GVS[SV->StdView].ViewFromOrig[1], SV->GVS[SV->StdView].ViewFromOrig[2]);
	fprintf(Out,"\tViewCenter = [%f %f %f]\n", SV->GVS[SV->StdView].ViewCenter[0], SV->GVS[SV->StdView].ViewCenter[1], SV->GVS[SV->StdView].ViewCenter[2]);
	fprintf(Out,"\tViewCenterOrig = [%f %f %f]\n", SV->GVS[SV->StdView].ViewCenterOrig[0], SV->GVS[SV->StdView].ViewCenterOrig[1], SV->GVS[SV->StdView].ViewCenterOrig[2]);
	fprintf(Out,"\tViewCamUp = [%f %f %f]\n", SV->GVS[SV->StdView].ViewCamUp[0], SV->GVS[SV->StdView].ViewCamUp[1], SV->GVS[SV->StdView].ViewCamUp[2]);
	fprintf(Out,"\tRotaCenter = [%f %f %f]\n", SV->GVS[SV->StdView].RotaCenter[0], SV->GVS[SV->StdView].RotaCenter[1], SV->GVS[SV->StdView].RotaCenter[2]);
	fprintf(Out,"\tlight0_position = [%f %f %f %f]\n", SV->light0_position[0], SV->light0_position[1], SV->light0_position[2], SV->light0_position[3]);
	fprintf(Out,"\tlight1_position = [%f %f %f %f]\n", SV->light1_position[0], SV->light1_position[1], SV->light1_position[2], SV->light1_position[3]);
	fprintf(Out,"\tWindWidth = %d\n", SV->WindWidth);
	fprintf(Out,"\tWindHeight = %d\n", SV->WindHeight);
	fprintf(Out,"\tcurrentQuat = [%f %f %f %f]\n", SV->GVS[SV->StdView].currentQuat[0], SV->GVS[SV->StdView].currentQuat[1], SV->GVS[SV->StdView].currentQuat[2], SV->GVS[SV->StdView].currentQuat[3]);
	fprintf(Out,"\tdeltaQuat = [%f %f %f %f]\n", SV->GVS[SV->StdView].deltaQuat[0], SV->GVS[SV->StdView].deltaQuat[1], SV->GVS[SV->StdView].deltaQuat[2], SV->GVS[SV->StdView].deltaQuat[3]);
	fprintf(Out,"\tApplyMomentum = %d\n", SV->GVS[SV->StdView].ApplyMomentum);
	fprintf(Out,"\tMinIdleDelta = %d\n", SV->GVS[SV->StdView].MinIdleDelta);
	fprintf(Out,"\tzoomDelta = %f, zoomBegin = %f\n", SV->GVS[SV->StdView].zoomDelta, SV->GVS[SV->StdView].zoomBegin);
	fprintf(Out,"\tspinDeltaX/Y = %d/%d\n", SV->GVS[SV->StdView].spinDeltaX, SV->GVS[SV->StdView].spinDeltaY);
	fprintf(Out,"\tspinBeginX/Y = %d/%d\n", SV->GVS[SV->StdView].spinBeginX, SV->GVS[SV->StdView].spinBeginY);	
	fprintf(Out,"\tTranslateGain = %f\n", SV->GVS[SV->StdView].TranslateGain);
	fprintf(Out,"\tArrowtranslateDeltaX/Y = %f/%f\n", SV->GVS[SV->StdView].ArrowtranslateDeltaX, SV->GVS[SV->StdView].ArrowtranslateDeltaY);
	fprintf(Out,"\ttranslateBeginX/Y = %d/%d\n", SV->GVS[SV->StdView].translateBeginX, SV->GVS[SV->StdView].translateBeginY);
	fprintf(Out,"\ttranslateDeltaX/Y = %f/%f\n", SV->GVS[SV->StdView].translateDeltaX, SV->GVS[SV->StdView].translateDeltaY);
	fprintf(Out,"\ttranslateVec = [%f %f 0.0]\n", SV->GVS[SV->StdView].translateVec[0], SV->GVS[SV->StdView].translateVec[1]);
	fprintf(Out,"\tShow Mesh Axis %d\n", SV->ShowMeshAxis);
	fprintf(Out,"\tShow Eye Axis %d\n", SV->ShowEyeAxis);
	fprintf(Out,"\tShow Cross Hair %d\n", SV->ShowCrossHair);
	fprintf(Out,"\tPolyMode %d\n", SV->PolyMode);
	
	fprintf(Out,"\tN_DO = %d\n", SV->N_DO);
	fprintf(Out,"\tShowDO = [");
	for (i=0; i< SV->N_DO; ++i)
		fprintf(Out,"%d, ", SV->ShowDO[i]);
	fprintf(Out,"]\n");
	if (SV->X == NULL) fprintf(Out,"\tX struct is NULL!\n");
	else {
	fprintf(Out,"\tX struct defined.\n");
	}
	fprintf(Out,"\tSO in focus %d\n", SV->Focus_SO_ID);
	fprintf(Out,"\tDO in focus %d\n", SV->Focus_DO_ID);
	/* show some state stuff */
	fprintf(Out,"\nView States:\n");
	for (i=0; i < SV->N_VSv; ++i) {
		fprintf(Out,"\nView State %d/%d (FOV = %f):\n", i, SV->N_VSv, SV->FOV[i]);
		if (!SUMA_Show_ViewState (&(SV->VSv[i]), Out)) {
			fprintf(Out,"Error in SUMA_Show_ViewState\n");
		}
	}
	fprintf(Out, "\nStandard viewing mode: %d\n", SV->StdView );
	fprintf(Out, "\nBackground Modulation Factor= %f\n", SV->Back_Modfact);
	fprintf(Out, "\nLast non mappable visited %d\n", SV->LastNonMapStateID);
	
	/*fprintf(Out,"\t\n", SV->);
	fprintf(Out,"\t\n", SV->);
	fprintf(Out,"\t\n", SV->);
	fprintf(Out,"\t\n", SV->);*/
	fprintf(Out,"\n");
	return;
}

/*! Show the ViewState structure */
SUMA_Boolean SUMA_Show_ViewState(SUMA_ViewState *VS, FILE *Out) 
{
	int i;
	if (Out == NULL) Out = stdout;

	if (VS == NULL) {
		fprintf(Out,"VS is NULL\n");		
		return(NOPE);
	}

	if (VS->Name) fprintf(Out,"\tName: %s\n", VS->Name);
	else fprintf(Out,"\tName: NULL\n");
	
	if (VS->N_MembSOs) {
		fprintf(Out,"\t%d MembSOs: ", VS->N_MembSOs);
		for (i=0; i < VS->N_MembSOs; ++i) fprintf(Out,"%d, ", VS->MembSOs[i]);
		fprintf(Out,"\n");
	} else {
		fprintf(Out,"\tNo MembSOs\n");
	}
	
	if (VS->Hist) {
		if (VS->Hist->N_DO) {
			fprintf(Out,"\tHist->N_DO = %d\nHist->ShowDO: ", VS->Hist->N_DO);
			for (i=0; i < VS->Hist->N_DO; ++i) {
				fprintf(Out,"\t%d, ", VS->Hist->ShowDO[i]);
			}
		}
	} else {
		fprintf(Out,"\tHist is NULL\n");
	}
	
	return (YUP);
}

/*!
	Create & free ViewState_Hist structure 
*/
SUMA_ViewState_Hist *SUMA_Alloc_ViewState_Hist (void)
{
	static char FuncName[]={"SUMA_Alloc_ViewState_Hist"};
	SUMA_ViewState_Hist *vsh;
	
	vsh = (SUMA_ViewState_Hist *)malloc(sizeof(SUMA_ViewState_Hist));
	if (vsh == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Could not allocate for vsh.\n", FuncName);
		return (NULL);
	}
	vsh->ShowDO = NULL;
	vsh->N_DO = 0;
	return (vsh);
}	
SUMA_Boolean SUMA_Free_ViewState_Hist (SUMA_ViewState_Hist *vsh)
{
	if (vsh == NULL) return (YUP);
	if (vsh->ShowDO) free(vsh->ShowDO);
	if (vsh) free (vsh);
	return (YUP);
}

/*!
	Create & free SUMA_ViewState structure 
*/
SUMA_ViewState *SUMA_Alloc_ViewState (int N)
{
	SUMA_ViewState *vs;
	int i;
	static char FuncName[]={"SUMA_Alloc_ViewState"};
	
	vs = (SUMA_ViewState *)malloc(sizeof(SUMA_ViewState)*N);
	if (vs == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Could not allocate for vs.\n", FuncName);
		return (NULL);
	}
	for (i=0; i< N; ++i) {
		vs[i].Name = NULL;
		vs[i].MembSOs = NULL;
		vs[i].N_MembSOs = 0;
		vs[i].Hist = SUMA_Alloc_ViewState_Hist ();
		if (vs[i].Hist == NULL) {
			fprintf(SUMA_STDERR,"Error %s: Could not allocate for vs->Hist.\n", FuncName);
			free(vs);
			return (NULL);
		}
	}
	return (vs);
}	

SUMA_Boolean SUMA_Free_ViewState (SUMA_ViewState *vs)
{
	if (vs == NULL) return (YUP);
	if (vs->Name) free(vs->Name);
	if (vs->MembSOs) free(vs->MembSOs);
	if (vs->Hist) SUMA_Free_ViewState_Hist (vs->Hist);
	if (vs) free (vs);
	return (YUP);
}

/*! 
	locate the index i (into csv->VSv[i]) of state 
	-1 if not found
*/
int SUMA_WhichState (char *state, SUMA_SurfaceViewer *csv)
{
	static char FuncName[]={"SUMA_WhichState"};
	int i = 0;
	
	while (i < csv->N_VSv) {
		/*fprintf(SUMA_STDERR,"%s: comparing csv->VSv[%d].Name = %s to %s ...\n", FuncName, i, csv->VSv[i].Name, state);*/
		if (strcmp(csv->VSv[i].Name, state) == 0) {
			/*fprintf(SUMA_STDERR,"%s: FOUND, i=%d!\n", FuncName, i);*/
			return (i);
		}
		++i;
	}
	return (-1);
}

/*! 
	register the different view states and surfaces belonging to different 
	view states in the surface viewer's structure
	Essentially, it creates the vector VSv that is a part of the surface viewer structure
*/
SUMA_Boolean SUMA_RegisterSpecSO (SUMA_SurfSpecFile *Spec, SUMA_SurfaceViewer *csv, SUMA_DO* dov, int N_dov)
{
	static char FuncName[]={"SUMA_RegisterSpecSO"};
	int is, i;
	SUMA_SurfaceObject * SO;
	
	/* allocate for space depending on the number of states present */
	
	/*fprintf(SUMA_STDERR,"%s: Entering ...\n", FuncName);*/
	
	csv->VSv = SUMA_Alloc_ViewState (Spec->N_States);
	if (csv->VSv == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate for VSv.\n", FuncName);
		return (NOPE);
	}
	csv->N_VSv = 0;
	
	/* register the various states from each SO in DOv */
	for (i=0; i < N_dov; ++i) {
		if (SUMA_isSO(dov[i])) {
			SO = (SUMA_SurfaceObject *)(dov[i].OP);
			if (csv->N_VSv == 0) {
				/* delaware encountered, snag it*/
				csv->VSv[csv->N_VSv].Name = (char *)malloc(sizeof(char)*strlen(SO->State));
				if (csv->VSv[csv->N_VSv].Name == NULL) {
					fprintf(SUMA_STDERR,"Error %s: Failed to allocate for csv->VSv[csv->N_VSv].Name.\n", FuncName);
					return (NOPE);
				}
				csv->VSv[csv->N_VSv].Name = strcpy (csv->VSv[csv->N_VSv].Name, SO->State);  
				csv->VSv[csv->N_VSv].N_MembSOs = 1;
				csv->N_VSv += 1;
			}else {
				is = SUMA_WhichState (SO->State, csv);
				if (is < 0) {
					/* add state if it is a new one */
					csv->VSv[csv->N_VSv].Name = (char *)malloc(sizeof(char)*strlen(SO->State));
					if (csv->VSv[csv->N_VSv].Name == NULL) {
						fprintf(SUMA_STDERR,"Error %s: Failed to allocate for csv->VSv[csv->N_VSv].Name.\n", FuncName);
						return (NOPE);
					}	
					csv->VSv[csv->N_VSv].Name = strcpy (csv->VSv[csv->N_VSv].Name, SO->State); 
					csv->VSv[csv->N_VSv].N_MembSOs = 1;
					csv->N_VSv += 1;
				} else { /* old one, count it */
					csv->VSv[is].N_MembSOs += 1;
				}
			}
		
		}
	}
	
	
	/*fprintf(SUMA_STDERR,"%s: allocating ...\n", FuncName);*/
	
	/* allocate for FOV */
	csv->FOV = (float *)calloc(csv->N_VSv, sizeof(float));
	
	/* allocate space for MembSOs counters will be reset for later use counting proceeds
	also initialize FOV*/
	for (i=0; i < csv->N_VSv; ++i) {
		csv->FOV[i] = FOV_INITIAL;
		csv->VSv[i].MembSOs = (int *) calloc(csv->VSv[i].N_MembSOs, sizeof(int));
		if (csv->VSv[i].MembSOs == NULL) {
			fprintf(SUMA_STDERR,"Error %s: Failed to allocate for csv->VSv[i].MembSOs.\n", FuncName);
			return (NOPE);
		}	
		csv->VSv[i].N_MembSOs = 0;
	}

	
	/*fprintf(SUMA_STDERR,"%s: placement ...\n", FuncName);*/
	
	/* now place each SO where it belongs */
	for (i=0; i < N_dov; ++i) {
		if (SUMA_isSO(dov[i])) {
			SO = (SUMA_SurfaceObject *)(dov[i].OP);
			/* find out which state it goes in */
			is = SUMA_WhichState (SO->State, csv);
			if (is < 0) {
				fprintf(SUMA_STDERR,"Error %s: This should not be.\n", FuncName);
				return (NOPE);
			}
			/*
			fprintf (SUMA_STDERR,"%s: Performing csv->VSv[%d].MembSOs[%d] = %d ...\n", \
				FuncName, is, csv->VSv[is].N_MembSOs, i);
			*/
			/* store it where it should be */
			csv->VSv[is].MembSOs[csv->VSv[is].N_MembSOs] = i; /* store it's id as valid member of the state*/
			csv->VSv[is].N_MembSOs += 1; /* count it, again */ 
		}
	}
	
	/*fprintf(SUMA_STDERR,"%s: Leaving ...\n", FuncName);*/

	return (YUP);
}

/*! allocate and intialize SUMA_CommonFields */
SUMA_CommonFields * SUMA_Create_CommonFields ()
{
	static char FuncName[]={"SUMA_Create_CommonFields"};
	SUMA_CommonFields *cf;
	
	cf = NULL;
	
	/* allocate */
	cf = (SUMA_CommonFields *)malloc(sizeof(SUMA_CommonFields));
	
	if (cf == NULL) {
		fprintf(SUMA_STDERR,"Error %s: Failed to allocate.\n", FuncName);
		return (cf);
	}
	
	cf->Dev = NOPE;
	
	return (cf);

}

/*! free SUMA_CommonFields */
SUMA_Boolean SUMA_Free_CommonFields (SUMA_CommonFields *cf)
{
	static char FuncName[]={"SUMA_Free_CommonFields"};
	if (cf) free(cf);
	
	return (YUP);
}

void SUMA_Show_CommonFields (SUMA_CommonFields *cf)
{
	static char FuncName[]={"SUMA_Show_CommonFields"};
	if (cf == NULL) {
		fprintf (SUMA_STDOUT,"%s: NULL structure.\n", FuncName);
		return;
	}
	fprintf (SUMA_STDOUT,"%s: AfniHostName: %s\n", FuncName, cf->AfniHostName);
	fprintf (SUMA_STDOUT,"%s: NimlAfniStream: %s\n", FuncName, cf->NimlAfniStream);
	return;
}
/*! assign new afni host name 
	 SUMA_Assign_AfniHostName (cf, AfniHostName)
	
	Assigns a new AfniHostName for niml communication
	
	\param cf (SUMA_CommonFields *) pointer to Common Fields structure, field AfniHostName will be modified here
	\param AfniHostName (char *) hostname in IP number form, or name form afni.nimh.nih.gov or afni (if in /etc/hosts file)
	                              NULL to set cf->AfniHostName to localhost
	\ret ans (SUMA_Boolean) YUP/NOPE
	
	
*/
SUMA_Boolean SUMA_Assign_AfniHostName (SUMA_CommonFields *cf, char *AfniHostName)
{
	static char FuncName[]={"SUMA_Assign_AfniHostName"};

	if (AfniHostName == NULL)
		sprintf(cf->AfniHostName, "localhost");
	else {	
		if (strlen(AfniHostName) > SUMA_MAX_NAME_LENGTH - 20) {
			fprintf(SUMA_STDERR,"Error %s: too long a host name (> %d chars).\n", FuncName, SUMA_MAX_NAME_LENGTH - 20);
			return (NOPE);
		}
		sprintf(cf->AfniHostName,"%s", AfniHostName);
	}

	sprintf(cf->NimlAfniStream,"tcp:%s:53211", cf->AfniHostName);

	fprintf(SUMA_STDOUT, "%s: Set AfniHostName to %s (stream name: %s)\n", FuncName, cf->AfniHostName, cf->NimlAfniStream);
	return (YUP);
}

/*!
	This function determines the most suitable standard view of a surface viewer
	This is based on the surface objects being displayed and their embedding dimension.
	The highest Embedding dimension of the lot determines what view to use 
	ans = SUMA_BestStandardView (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int N_dov)
	
	\param sv (SUMA_SurfaceViewer *) Surface viewer structure
	\param dov (SUMA_DO *) vector of displayable objects
	\param N_dov (int) number of displayable objects
	\ret ans (SUMA_SUMA_STANDARD_VIEWS) recommended view
	
*/	
SUMA_STANDARD_VIEWS SUMA_BestStandardView (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int N_dov)
{
	static char FuncName[] = {"SUMA_BestStandardView"};
	SUMA_STANDARD_VIEWS ans;
	int i, maxdim = -1, is;
	SUMA_SurfaceObject *SO = NULL;
	
	is = sv->iState;
	if (is < 0) {
		fprintf(SUMA_STDERR, "Error %s: sv->iState undefined.\n", FuncName);
		return (SUMA_Dunno); 
	}
	
	for (i=0; i<sv->VSv[is].N_MembSOs; ++i) {	
		SO = (SUMA_SurfaceObject *)(dov[sv->VSv[is].MembSOs[i]].OP);
		if (SO == NULL) {
			fprintf(SUMA_STDERR,"Error %s: SO is null ???\n.", FuncName);
			return (SUMA_Dunno);
		}
		if (SO->EmbedDim > maxdim) maxdim = SO->EmbedDim;
	}
	
	switch (maxdim) {
		case 2:
			return (SUMA_2D_Z0);
		case 3:
			return(SUMA_3D);
		default:
			fprintf(SUMA_STDERR,"Error %s: No provision for such a maximum embedding dimension.\n", FuncName);
			return(SUMA_Dunno);
	}

}
