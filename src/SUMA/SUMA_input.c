#include "SUMA_suma.h"

extern SUMA_SurfaceViewer *SUMAg_cSV;
extern int SUMAg_N_DOv; 
extern SUMA_DO *SUMAg_DOv;
extern SUMA_CommonFields *SUMAg_CF; 


/*! Mouse and Keyboard input handler function */

void
input(Widget w, XtPointer clientData, XtPointer callData)
{
	GLwDrawingAreaCallbackStruct *cd = (GLwDrawingAreaCallbackStruct *) callData;
	char buffer[10];
	KeySym keysym;
	int xls, ntot;
	float ArrowDeltaRot = 0.05; /* The larger the value, the bigger the rotation increment */
	SUMA_EngineData EngineData; /* Do not free EngineData, only its contents*/
	char CommString[SUMA_MAX_COMMAND_LENGTH];
	static char FuncName[]= {"SUMA_input"};
	char s[SUMA_MAX_STRING_LENGTH], sfield[100], sdestination[100];
	static char ssource[]={"suma"};
	int it, ii, iv3[3];
	float **fm, fv3[3], fv15[15];
	XKeyEvent Kev;
	XButtonEvent Bev;
	XMotionEvent Mev;
	
	/*float ft;
	int **im, iv15[15];*/ /* keep unused variables undeclared to quite compiler */
	
	/* initialize EngineData */
	if (!SUMA_InitializeEngineData (&EngineData)) {
		fprintf(SUMA_STDERR,"Error %s: Failed to initialize EngineData\n", FuncName);
		return;
	}

	Kev = (XKeyEvent) cd->event->xkey;
	Bev = (XButtonEvent) cd->event->xbutton;
	Mev = (XMotionEvent) cd->event->xmotion;
	
	/* a sample keypresses */
	#if 0
		if (Kev.state & ShiftMask) {
			fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);
		}else if (Kev.state & ControlMask){
			fprintf (SUMA_STDERR,"%s: Control down\n", FuncName);
		}else if (Kev.state & Mod1Mask){
			fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);
		}else if (Kev.state & Mod2Mask){
			fprintf (SUMA_STDERR,"%s: Mod2 down\n", FuncName);
		}else if (Kev.state & Mod3Mask){
			fprintf (SUMA_STDERR,"%s: Mod3 down\n", FuncName);
		}else if (Kev.state & Mod4Mask){
			fprintf (SUMA_STDERR,"%s: Mod4 down\n", FuncName);
		}else if (Kev.state & Mod5Mask){
			fprintf (SUMA_STDERR,"%s: Mod5 down\n", FuncName);
		}else {
			/*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
		}
	#endif
	
  switch (Kev.type) { /* switch event type */
  case KeyPress:
		xls = XLookupString((XKeyEvent *) cd->event, buffer, 8, &keysym, NULL);
		
		/* XK_* are found in keysymdef.h */ 
		switch (keysym) { /* keysym */
      	case XK_space:   /* The spacebar. */
				/* toggle between state containing mapping reference of SO in focus and other view */
				{
					SUMA_SurfaceObject *SO = NULL;
					int curstateID = -1, nxtstateID = -1;

					curstateID = SUMA_WhichState(SUMAg_cSV->State, SUMAg_cSV);
					SO = (SUMA_SurfaceObject *)SUMAg_DOv[SUMAg_cSV->Focus_SO_ID].OP;
					if (SUMA_isINHmappable (SO)) {
						/* get the last non mappable state in SV */
						if (SUMAg_cSV->LastNonMapStateID < 0) { /* not recorded, complain and quit */
							fprintf(SUMA_STDERR,"Warning %s: Nothing defined to toggle with yet.\n", FuncName); 
							break;
						}

						if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, SUMAg_cSV, SUMAg_cSV->LastNonMapStateID)) {
							fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SwitchState.\n", FuncName);
							break;
						}

					} else {/* that's a non mappable, go to state containing reference */
						/* find SO that is mappable reference & get corresponding state ID*/
						nxtstateID = SUMA_findDO(SO->MapRef_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
						if (nxtstateID < 0) {
							fprintf (SUMA_STDERR,"%s: Failed in SUMA_findDO This should not happen.\n", FuncName);
							break;
						}
						/* store this location */
						SUMAg_cSV->LastNonMapStateID = curstateID;

						/* go there */
						if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, SUMAg_cSV, nxtstateID)) {
							fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SwitchState.\n", FuncName);
							break;
						}
					}

				}
				postRedisplay();
				break;

			case XK_Escape: /* there's more:  XK_BackSpace XK_Tab XK_Linefeed XK_Return XK_Delete */
         	exit(0);
         	break;

			case XK_a:
				/* toggle background attenuation */

				if (SUMAg_cSV->Back_Modfact) {
					fprintf (SUMA_STDOUT,"%s: Modulation by background intensity OFF.\n", FuncName);
					SUMAg_cSV->Back_Modfact = 0;
				} else {
					fprintf (SUMA_STDOUT,"%s: Modulation by background intensity ON.\n", FuncName);
					SUMAg_cSV->Back_Modfact = SUMA_BACKGROUND_MODULATION_FACTOR;
				}

				{
					SUMA_SurfaceObject *SO = NULL;

					for (ii=0; ii< SUMAg_cSV->N_DO; ++ii) {
						if (SUMA_isSO(SUMAg_DOv[SUMAg_cSV->ShowDO[ii]])) {
							SO = (SUMA_SurfaceObject*)SUMAg_DOv[SUMAg_cSV->ShowDO[ii]].OP;
							/* remix colors */
							if (!SUMA_Overlays_2_GLCOLAR4(SO->Overlays, SO->N_Overlays, SO->glar_ColorList, SO->N_Node, \
								SUMAg_cSV->Back_Modfact, SUMAg_cSV->ShowBackground, SUMAg_cSV->ShowForeground)) {
								fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Overlays_2_GLCOLAR4.\n", FuncName);
								return;
							}
						}
					}
				}

				postRedisplay();
				break;

			case XK_B:
				if (SUMAg_CF->Dev)  {
					SUMAg_cSV->BF_Cull = !SUMAg_cSV->BF_Cull;
					if (SUMAg_cSV->BF_Cull) {
						glCullFace (GL_BACK);
   					glEnable (GL_CULL_FACE);
					} else {
						glDisable(GL_CULL_FACE);
					}
					postRedisplay();
				}
				break;

			case XK_b:
				/* Show/hide the background */
				sprintf(CommString,"Redisplay|Remix|ToggleBackground~");
				if (!SUMA_Engine (CommString, &EngineData)) {
					fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
				}
				break;				


			case XK_c:
				fprintf(stdout,"Enter name of color file: ");
				/*Load colors from file */
				fscanf(stdin,"%s", s);
				EngineData.N_cols = 4;
				/* find out if file exists and how many values it contains */
				ntot = SUMA_float_file_size (s);
				if (ntot < 0) {
					fprintf(stderr,"Error SUMA_input: filename %s could not be open.\n", s);
					return;
				}

				/* make sure it's a full matrix */
				if ((ntot % EngineData.N_cols)) {
					fprintf(stderr,"Error SUMA_Read_2Dfile: file %s contains %d values, not divisible by ncols %d.\n", s, ntot, EngineData.N_cols);
					return;
				}
				EngineData.N_rows = ntot/EngineData.N_cols;

				/* allocate space */
				fm = (float **)SUMA_allocate2D (EngineData.N_rows, EngineData.N_cols, sizeof(float));
				if (fm == NULL) {
					fprintf(stderr,"Error SUMA_input: Failed to allocate space for fm\n");
					return;
				}

				EngineData.N_rows = SUMA_Read_2Dfile (s, fm, EngineData.N_cols, EngineData.N_rows);
				if (EngineData.N_rows < 0) {
					fprintf(stderr,"SUMA_input Error: Failed to read full matrix from %s\n", s);
					return;
				}

				/*register fm with EngineData */
				sprintf(sfield,"fm");
				sprintf(sdestination,"SetNodeColor");
				if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)fm, sdestination, ssource, YUP)) {
					fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
					break;
				}

				sprintf(CommString,"Redisplay|SetNodeColor~");
				if (!SUMA_Engine (CommString, &EngineData)) {
					fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
				}

				/* free fm since it was registered by pointer and is not automatically freed after the call to SUMA_Engine */
				if (fm) SUMA_free2D ((char **)fm, EngineData.N_rows);

				break;

			case XK_d:
				if (SUMAg_CF->Dev) {
					SUMA_Show_DOv(SUMAg_DOv, SUMAg_N_DOv, stdout);
				}
				break;


			case XK_F:
				sprintf(CommString,"Redisplay|FlipLight0Pos~");
				if (!SUMA_Engine (CommString, &EngineData)) {
					fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
				}
				break;

			case XK_f:
				/* Show/hide the foreground */
				sprintf(CommString,"Redisplay|Remix|ToggleForeground~");
				if (!SUMA_Engine (CommString, &EngineData)) {
					fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
				}
				break;				

			case XK_H:
				if (SUMAg_CF->Dev) {
					do {
						fprintf(stdout,"Enter XYZ of center followed by size of Box (comma separated):\n");
					} while (fscanf(stdin,"%f, %f, %f, %f, %f, %f", &(fv15[0]), &(fv15[1]),&(fv15[2]),\
																&(fv15[3]), &(fv15[4]),&(fv15[5])) != 6);
					fprintf(stdout,"You Entered: Center: %f %f %f Size %f %f %f\n", \
						fv15[0], fv15[1],fv15[2],\
						fv15[3], fv15[4],fv15[5]);

					/* register fv15 with EngineData */
					sprintf(sfield,"fv15");
					sprintf(sdestination,"HighlightNodes");
					if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)fv15, sdestination, ssource, NOPE)) {
						fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
						break;
					}

					sprintf(CommString,"Redisplay|HighlightNodes~");			
					if (!SUMA_Engine (CommString, &EngineData)) {
						fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
					}
				}
				break;

			case XK_h:
				SUMA_help_message(NULL);
				break;

			case XK_l:
				do {
					fflush (stdin);
					fprintf(stdout,"Enter XYZ coordinates to look at (comma separated):\n");
				} while (fscanf(stdin,"%f, %f, %f", &(fv3[0]), &(fv3[1]),&(fv3[2])) != 3);
				fprintf(stdout,"You Entered: %f %f %f\n", fv3[0], fv3[1],fv3[2]);

				/* register fv3 with EngineData */
				sprintf(sfield,"fv3");
				sprintf(sdestination,"SetLookAt");
				if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)fv3, sdestination, ssource, NOPE)) {
					fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
					break;
				}

				sprintf(CommString,"Redisplay|SetLookAt~");			
				if (!SUMA_Engine (CommString, &EngineData)) {
					fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
				}
				break;

			case XK_L:
				if (SUMAg_CF->Dev) {
					do {
						fprintf(stdout,"Enter XYZ coordinates to look from (comma separated):\n");
					} while (fscanf(stdin,"%f, %f, %f", &(fv3[0]), &(fv3[1]),&(fv3[2])) != 3);
					fprintf(stdout,"You Entered: %f %f %f\n", fv3[0], fv3[1],fv3[2]);
				}
				break;

			case XK_m:
         		SUMAg_cSV->GVS[SUMAg_cSV->StdView].ApplyMomentum = !SUMAg_cSV->GVS[SUMAg_cSV->StdView].ApplyMomentum;
					if (SUMAg_cSV->GVS[SUMAg_cSV->StdView].ApplyMomentum) {
	         		 SUMAg_cSV->X->MOMENTUMID = XtAppAddTimeOut(SUMAg_cSV->X->APP, 1, momentum, 0);
						 /* wait till user initiates turning */
						SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX = 0; SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY = 0;
						SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaX = 0; SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaY = 0;
					}
       			else {
						if (SUMAg_cSV->X->MOMENTUMID)  XtRemoveTimeOut(SUMAg_cSV->X->MOMENTUMID);
     			 	}
				 break;

			case XK_n:
				if (SUMAg_CF->Dev) {
					do {
						fprintf(stdout,"Enter XYZ of center followed by size of Box (comma separated):\n");
					} while (fscanf(stdin,"%f, %f, %f, %f, %f, %f", &(fv15[0]), &(fv15[1]),&(fv15[2]),\
																&(fv15[3]), &(fv15[4]),&(fv15[5])) != 6);
					fprintf(stdout,"You Entered: Center: %f %f %f Size %f %f %f\n", \
						fv15[0], fv15[1],fv15[2],\
						fv15[3], fv15[4],fv15[5]);

					/* register fv15 with EngineData */
					sprintf(sfield,"fv15");
					sprintf(sdestination,"GetNearestNode");
					if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)fv15, sdestination, ssource, NOPE)) {
						fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
						break;
					}					

					sprintf(CommString,"Redisplay|GetNearestNode~");			
					if (!SUMA_Engine (CommString, &EngineData)) {
						fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
					}
				}
				break;

			case XK_p:
				 SUMAg_cSV->PolyMode = ((SUMAg_cSV->PolyMode+1) % 3);
				 switch (SUMAg_cSV->PolyMode) {
					case 0:
						glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
						break;
					case 1:
						glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
						break;
					case 2:
						glPolygonMode(GL_FRONT_AND_BACK, GL_POINT);
						break;
				}
				postRedisplay();
				break;

			case XK_S:
				if (SUMAg_CF->Dev) {
					int *do_id, n_do_id;
					do_id = SUMA_GetDO_Type(SUMAg_DOv, SUMAg_N_DOv, SO_type, &n_do_id);
					if (n_do_id) {
						while (n_do_id) {
							SUMA_Print_Surface_Object((SUMA_SurfaceObject *)SUMAg_DOv[do_id[n_do_id-1]].OP, stdout);
							--n_do_id;
						}
						free(do_id);
					}
					break;
				}
			case XK_s:
				if (SUMAg_CF->Dev) {
					for (ii=0; ii< SUMAg_cSV->N_DO; ++ii) {
						if (SUMA_isSO(SUMAg_DOv[SUMAg_cSV->ShowDO[ii]])) 
							SUMA_Print_Surface_Object((SUMA_SurfaceObject*)SUMAg_DOv[SUMAg_cSV->ShowDO[ii]].OP, stdout);
					}
				}
				break;

			case XK_t:
				sprintf(CommString,"ToggleTalkToAfni~");			
				if (!SUMA_Engine (CommString, &EngineData)) {
					fprintf(SUMA_STDERR, "Error SUMA_input: SUMA_Engine call failed.\n");
				}
				break;

			case XK_v:
				if (SUMAg_CF->Dev) {
					Show_SUMA_SurfaceViewer_Struct (SUMAg_cSV, stdout);
				}
				break;

			case XK_W:
				if (SUMAg_CF->Dev) {
					FILE *Fout;
					SUMA_SurfaceObject *SO;

					Fout = fopen("NodeList.txt", "w");
					if (Fout == NULL) {
						fprintf(SUMA_STDERR, "Error SUMA_input: Could not open file for writing.\n");
						break;
					}
					SO = (SUMA_SurfaceObject *)SUMAg_DOv[SUMAg_cSV->Focus_SO_ID].OP;
					for (ii=0; ii < SO->N_Node; ++ii) {
						fprintf(Fout, "%f\t%f\t%f\n", \
							SO->NodeList[ii][0], SO->NodeList[ii][1],SO->NodeList[ii][2]);
					}
					fclose (Fout);

					Fout = fopen("FaceSetList.txt", "w");
					if (Fout == NULL) {
						fprintf(SUMA_STDERR, "Error SUMA_input: Could not open file for writing.\n");
						break;
					}
					for (ii=0; ii < SO->N_FaceSet; ++ii) {
						fprintf(Fout, "%d\t%d\t%d\n", \
							SO->FaceSetList[ii][0], SO->FaceSetList[ii][1],SO->FaceSetList[ii][2]);
					}
					fclose (Fout);

					fprintf(SUMA_STDERR, "SUMA_input: Wrote NodeList.txt & FaceSetList.txt to disk.\n");
				}
				break;

			case XK_w:
				fprintf(SUMA_STDOUT,"%s: Began rendering to file. Please wait ...\n", FuncName);
				if (!SUMA_RenderToPixMap (SUMAg_cSV, SUMAg_DOv)) {
					fprintf(SUMA_STDERR, "Error %s: Failed to write image.\n", FuncName);
				} 
				break;

			case XK_Z:
				/*fprintf(stdout,"Zoom in");*/
				SUMAg_cSV->FOV[SUMAg_cSV->iState] /= FOV_IN_FACT; if (SUMAg_cSV->FOV[SUMAg_cSV->iState] < FOV_MIN) SUMAg_cSV->FOV[SUMAg_cSV->iState] = FOV_MIN; 
				postRedisplay();
				break;

			case XK_z:
				/*fprintf(stdout,"Zoom out");*/
				SUMAg_cSV->FOV[SUMAg_cSV->iState] /= FOV_OUT_FACT; if (SUMAg_cSV->FOV[SUMAg_cSV->iState] > FOV_MAX) SUMAg_cSV->FOV[SUMAg_cSV->iState] = FOV_MAX;
				postRedisplay();
				break;

			case XK_asterisk:
				fprintf(SUMA_STDOUT, "%s: smoothing node attributes ...\n", FuncName);
				{
					SUMA_SurfaceObject *SO;
					float * attr_sm;
					float *attrbuf;
					int ic, cnt;
					int allcols;

					SO = (SUMA_SurfaceObject *)SUMAg_DOv[SUMAg_cSV->Focus_SO_ID].OP;
					attrbuf = (float *)calloc(SO->N_Node, sizeof(int));
					if (attrbuf == NULL) {
						fprintf(stderr,"Error SUMA_input: Failed to allocate for attrbuf.\n");
						break;
					}

					allcols = 4 * SO->N_Node;
					/* the colors are stored in glar_ColorList, RGBA */
					for (ic=0; ic < 3; ++ic) { /* ic */
						ii = ic;
						cnt = 0;
						while (ii < allcols) {
							attrbuf[cnt] = SO->glar_ColorList[ii];
							ii += 4;
							cnt += 1;
						} 

						attr_sm = SUMA_SmoothAttr_Neighb (attrbuf, SO->N_Node, NULL, SO->FN); 
						if (attr_sm == NULL) {
							fprintf(stderr,"Error SUMA_input: Failed in SUMA_SmoothAttr_Neighb\n");
							break;
						}

						/* copy results back into colorvector */
						ii = ic; 
						cnt = 0;
						while (ii < allcols) {
							SO->glar_ColorList[ii] = attr_sm[cnt];
							ii += 4;
							cnt += 1;
						} 
					} /* ic */	

					free (attr_sm);
					free (attrbuf);
					/*fprintf(SUMA_STDOUT, "%s: Smoothing Done ...\n", FuncName);*/
					postRedisplay();
				}

				break;

 			case XK_at:
				if (SUMAg_CF->Dev) {
					/* calculate the curvature */
					fprintf(SUMA_STDOUT, "%s: Calculating surface curvature ...\n", FuncName);
					{
						SUMA_SurfaceObject *SO;
						SO = (SUMA_SurfaceObject *)SUMAg_DOv[SUMAg_cSV->Focus_SO_ID].OP;
						SO->SC = SUMA_Surface_Curvature (SO->NodeList, SO->N_Node, SO->NodeNormList, SO->PolyArea, SO->N_FaceSet, SO->FN, SO->EL);
						if (SO->SC == NULL) {
								fprintf(stderr,"Error %s: Failed in SUMA_Surface_Curvature\n", FuncName);
								break;
							}					
					}	
				}
				break;

			case XK_parenleft:
				if (SUMAg_CF->Dev) {
					SUMA_SurfaceObject *SO;
					SUMA_COLOR_MAP *CM;
					SUMA_SCALE_TO_MAP_OPT * OptScl;
					SUMA_STANDARD_CMAP MapType;
					SUMA_COLOR_SCALED_VECT * SV;
					float ClipRange[2], *Vsort;
					float * attr_sm;

					fprintf(SUMA_STDOUT, "%s: Calculating convexity ...\n", FuncName);
					SO = (SUMA_SurfaceObject *)SUMAg_DOv[SUMAg_cSV->Focus_SO_ID].OP;	
					if (SO->Cx) {
						fprintf(stderr,"Error %s: SO->Cx must be null prior to new assignment\n", FuncName);
						break;
					}
					SO->Cx = SUMA_Convexity	(SO->NodeList, SO->N_Node, SO->NodeNormList, SO->FN);	
					if (SO->Cx == NULL) {
							fprintf(stderr,"Error %s: Failed in SUMA_Convexity\n", FuncName);
							break;
					}	
					/* smooth estimate twice */
					attr_sm = SUMA_SmoothAttr_Neighb (SO->Cx, SO->N_Node, NULL, SO->FN);
					if (attr_sm == NULL) {
							fprintf(stderr,"Error %s: Failed in SUMA_SmoothAttr_Neighb\n", FuncName);
							break;
					}	
					SO->Cx = SUMA_SmoothAttr_Neighb (attr_sm, SO->N_Node, SO->Cx, SO->FN);
					if (attr_sm) free(attr_sm);

					fprintf(SUMA_STDOUT, "%s: Use SUMA_ScaleToMap to colorize Conv.txt and display it on surface.\n", FuncName);
					CM = SUMA_GetStandardMap (SUMA_CMAP_nGRAY20);
					if (CM == NULL) {
						fprintf (SUMA_STDERR,"Error %s: Could not get standard colormap.\n", FuncName);
						exit (1); 
					}

					/* get the options for creating the scaled color mapping */
					OptScl = SUMA_ScaleToMapOptInit();
					if (!OptScl) {
						fprintf (SUMA_STDERR,"Error %s: Could not get scaling option structure.\n", FuncName);
						exit (1); 
					}

					/* work the options a bit */
					OptScl->ApplyClip = YUP;
					ClipRange[0] = 5; ClipRange[1] = 95; /* percentile clipping range*/ 
					Vsort = SUMA_PercRange (SO->Cx, NULL, SO->N_Node, ClipRange, ClipRange); 
					OptScl->ClipRange[0] = ClipRange[0]; OptScl->ClipRange[1] = ClipRange[1];

					OptScl->BrightFact = 0.4;

					/* map the values in SO->Cx to the colormap */
						/* allocate space for the result */
						SV = SUMA_Create_ColorScaledVect(SO->N_Node);
						if (!SV) {
							fprintf (SUMA_STDERR,"Error %s: Could not allocate for SV.\n", FuncName);
							exit(1);
						}

						/* finally ! */
						/*fprintf (SUMA_STDERR,"%s: 1st color in map %f %f %f\n", FuncName, CM->M[0][0], CM->M[0][1],CM->M[0][2]);*/
						if (!SUMA_ScaleToMap (SO->Cx, SO->N_Node, Vsort[0], Vsort[SO->N_Node-1], CM, OptScl, SV)) {
							fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_ScaleToMap.\n", FuncName);
							exit(1);
						}

						/* Now place SV in the color array */
						SUMA_RGBmat_2_GLCOLAR4(SV->cM, SO->glar_ColorList, SO->N_Node);

						/* free */
						if (Vsort) free(Vsort);
						if (CM) SUMA_Free_ColorMap (CM);
 						if (OptScl) free(OptScl);
						if (SV) SUMA_Free_ColorScaledVect (SV);
						if (SO->Cx) {
							free(SO->Cx);
							SO->Cx = NULL;
						}

					fprintf(SUMA_STDOUT, "%s: Convexity mapping done ...\n", FuncName);
					postRedisplay();	
				}
				break;
			case XK_comma:
				{
					/* switch state, back one */
					int nxtstateID = -1;

					if (SUMAg_cSV->N_VSv < 2) break;

					/*fprintf(SUMA_STDERR,"%s: Current viewing state is %s ...\n", FuncName, SUMAg_cSV->State);*/
					/* toggle to the next view state */
					nxtstateID = SUMA_PrevState(SUMAg_cSV);
					if (nxtstateID < 0) {
						fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_PrevState.\n", FuncName);
						break;
					}
					fprintf(SUMA_STDERR,"%s: Switching from %s to %s viewing state.\n", \
						FuncName, SUMAg_cSV->State, SUMAg_cSV->VSv[nxtstateID].Name);

					if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, SUMAg_cSV, nxtstateID)) {
						fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SwitchState.\n", FuncName);
						break;
					}

					/* register a call to redisplay (you also need to copy the color data, in case the next surface is of the same family*/
					sprintf(CommString,"Redisplay~");
					if (!SUMA_Engine (CommString, &EngineData)) {
						fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
					}
				}
				break;

			case XK_period:
				{
					/* switch state, forward one */
					int nxtstateID=-1;

					if (SUMAg_cSV->N_VSv < 2) break;

					/*fprintf(SUMA_STDERR,"%s: Current viewing state is %s ...\n", FuncName, SUMAg_cSV->State);*/
					/* toggle to the next view state */
					nxtstateID = SUMA_NextState(SUMAg_cSV);
					if (nxtstateID < 0) {
						fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_NextState.\n", FuncName);
						break;
					}
					fprintf(SUMA_STDERR,"%s: Switching from %s to %s viewing state.\n", FuncName, SUMAg_cSV->State, SUMAg_cSV->VSv[nxtstateID].Name);

					if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, SUMAg_cSV, nxtstateID)) {
						fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SwitchState.\n", FuncName);
						break;
					}

					/* register a call to redisplay (you also need to copy the color data, in case the next surface is of the same family*/
					sprintf(CommString,"Redisplay~");
					if (!SUMA_Engine (CommString, &EngineData)) {
						fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
					}

					break;
				}
				break;

			case XK_F1: /* F1 */
				/*printf("F1\n");*/
				SUMAg_cSV->ShowEyeAxis = !SUMAg_cSV->ShowEyeAxis;
				postRedisplay();
				break;

			case XK_F2:
				/*printf("F2\n");*/
				{
					int *do_id, n_do_id;
					/*SUMA_SurfaceObject *ptr_tmp;*/
					SUMAg_cSV->ShowMeshAxis = !SUMAg_cSV->ShowMeshAxis;
					do_id = SUMA_GetDO_Type(SUMAg_DOv, SUMAg_N_DOv, SO_type, &n_do_id);
					if (n_do_id) {
						while (n_do_id) {
							/*ptr_tmp = (SUMA_SurfaceObject *)SUMAg_DOv[do_id[n_do_id-1]].OP;
							ptr_tmp->ShowMeshAxis = SUMAg_cSV->ShowMeshAxis;*/
							((SUMA_SurfaceObject *)SUMAg_DOv[do_id[n_do_id-1]].OP)->ShowMeshAxis = SUMAg_cSV->ShowMeshAxis;
							--n_do_id;
						}
						free(do_id);
					}
				}
				postRedisplay();
				break;

			case XK_F3: /* F3 */
				sprintf(CommString, "Redisplay|ToggleCrossHair~");
				if (!SUMA_Engine (CommString, &EngineData)) {
					fprintf(stderr,"Error SUMA_input: Failed SUMA_Engine\n");
				} 
				break;

			case XK_F4: /* F4 */
				sprintf(CommString, "Redisplay|ToggleShowSelectedNode~");
				if (!SUMA_Engine (CommString, &EngineData)) {
					fprintf(stderr,"Error SUMA_input: Failed SUMA_Engine\n");
				} 
				break;

			case XK_F5: /* F5 */
				sprintf(CommString, "Redisplay|ToggleShowSelectedFaceSet~");
				if (!SUMA_Engine (CommString, &EngineData)) {
					fprintf(stderr,"Error SUMA_input: Failed SUMA_Engine\n");
				} 
				break;

			case XK_Home:	
				/*printf("HOME\n");*/
				sprintf(CommString, "Redisplay|FOVreset|Home~");
				if (!SUMA_Engine (CommString, &EngineData)) {
					fprintf(stderr,"Error SUMA_input: Failed SUMA_Engine\n");
				} 
				break;

			case XK_Left:	/*KEY_LEFT:*/
				/*fprintf(stdout,"Left Key\n");*/
				if ((Kev.state & ControlMask) && (Kev.state & ShiftMask)) {
					/* do nothing about ctrl+shift+this key */
				}else if (Kev.state & ShiftMask) {
					/*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[0] -= (GLfloat)SUMAg_cSV->GVS[SUMAg_cSV->StdView].ArrowtranslateDeltaX/(float)SUMAg_cSV->WindWidth*SUMAg_cSV->GVS[SUMAg_cSV->StdView].TranslateGain;
					/*SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[1] -= 0;*/
					postRedisplay();
				}else if (Kev.state & ControlMask){
					float a[3];
					/* From top view, rotate about x 90 degrees */ 
					a[0] = 1.0; a[1] = 0.0;
					axis_to_quat(a, 3.14159/2, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					/* then rotate about y 90 degrees */
					a[0] = 0.0; a[1] = 1.0;
					axis_to_quat(a, 3.14159/2, SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat);
					/*add and apply rotation*/
					add_quats (SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					postRedisplay();
				}else if (Kev.state & Mod1Mask) {
					/*ffprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
				}else {
					/*ffprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
					trackball(SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, 
						ArrowDeltaRot, 0.0, /* first point */
						-ArrowDeltaRot, 0.0); /* ending x,y */
					add_quats (SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX = -2*ArrowDeltaRot*SUMAg_cSV->WindWidth;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY = 0;
					postRedisplay();
				}
					
				break;

			case XK_Right:	/*KEY_RIGHT: */
				/*printf("Right Key\n");*/
				if ((Kev.state & ControlMask) && (Kev.state & ShiftMask)) {
					/* do nothing about ctrl+shift+this key */
				}else if (Kev.state & ShiftMask) {
					/*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[0] += (GLfloat)SUMAg_cSV->GVS[SUMAg_cSV->StdView].ArrowtranslateDeltaX/(float)SUMAg_cSV->WindWidth*SUMAg_cSV->GVS[SUMAg_cSV->StdView].TranslateGain;
					/*SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[1] -= 0;*/
					postRedisplay();
				}else if (Kev.state & ControlMask){
					/*fprintf (SUMA_STDERR,"%s: Control down\n", FuncName);*/
					float a[3];
					/* From top view, rotate about x 90 degrees */ 
					a[0] = 1.0; a[1] = 0.0;
					axis_to_quat(a, 3.14159/2, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					/* then rotate about y -90 degrees */
					a[0] = 0.0; a[1] = 1.0;
					axis_to_quat(a, -3.14159/2, SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat);
					/*add and apply rotation*/
					add_quats (SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					postRedisplay();
					
				}else if (Kev.state & Mod1Mask) {
					/*fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
				}else {
					/*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
					trackball(SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, 
						-ArrowDeltaRot, 0.0, /* first point */
						ArrowDeltaRot, 0.0); /* ending x,y */
					add_quats (SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX = 2*ArrowDeltaRot*SUMAg_cSV->WindWidth;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY = 0;
					postRedisplay();
				}
				break;

			case XK_Down:	/*KEY_DOWN*/
				/*printf("Down Key\n");*/
				if ((Kev.state & ControlMask) && (Kev.state & ShiftMask)) {
					float a[3];
					/* Posterior view ctrl+shift+down*/
					/* From top view, first rotate by 90 degrees about x axis */
					a[0] = 1.0; a[1] = 0.0;
					axis_to_quat(a, 3.14159/2, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					/* then rotate by 180 degrees about y axis */
					a[0] = 0.0; a[1] = 1.0;
					axis_to_quat(a, 3.14159, SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat);
					/*add and apply rotation*/
					add_quats (SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					postRedisplay();
				}else if (Kev.state & ShiftMask) {
					/*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
					/*SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[0] += 0;*/
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[1] -=  (GLfloat)SUMAg_cSV->GVS[SUMAg_cSV->StdView].ArrowtranslateDeltaY/(float)SUMAg_cSV->WindHeight*SUMAg_cSV->GVS[SUMAg_cSV->StdView].TranslateGain;
					postRedisplay();
				}else if (Kev.state & ControlMask){
					/*fprintf (SUMA_STDERR,"%s: Control down\n", FuncName);*/
					/* Inferior view ctrl+down*/
					float a[3];
					/* From top view, rotate by 180 degrees about y axis */
					a[0] = 0.0; a[1] = 1.0;
					axis_to_quat(a, 3.14159, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					postRedisplay();
				}else if (Kev.state & Mod1Mask) {
					/*fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
				}else {
					/*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
					trackball(SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, 
						0.0, ArrowDeltaRot, /* first point */
						0.0, -ArrowDeltaRot); /* ending x,y */
					/*fprintf(stdout,"\ncurrentQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat[i]);} fprintf(stdout,"\n");
					fprintf(stdout,"\ndeltaQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat[i]);} fprintf(stdout,"\n");*/
					add_quats (SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					/*fprintf(stdout,"\nnewQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat[i]);} fprintf(stdout,"\n");*/
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX = 0;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY = -2*ArrowDeltaRot*SUMAg_cSV->WindHeight;
					postRedisplay();
				}
				
				break;

			case XK_Up: /*KEY_UP*/
				/*printf("Up Key\n");*/
				if ((Kev.state & ControlMask) && (Kev.state & ShiftMask)) {
					float a[3];
					/* Posterior view ctrl+shift+up*/
					/* From top view, rotate by 90 degrees about x axis */
					a[0] = 1.0; a[1] = 0.0;
					axis_to_quat(a, 3.14159/2, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					postRedisplay();
				}else if (Kev.state & ShiftMask) {
					/*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
					#ifdef USELESS_BLOCK
						/* This shows how to have momentum work for arrow translation. But that is largely useless
						because the object  quickly disappears from view */
						SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaX = 0/(float)SUMAg_cSV->WindWidth*SUMAg_cSV->GVS[SUMAg_cSV->StdView].TranslateGain;
						SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaY = SUMAg_cSV->GVS[SUMAg_cSV->StdView].ArrowtranslateDeltaY/(float)SUMAg_cSV->WindHeight*SUMAg_cSV->GVS[SUMAg_cSV->StdView].TranslateGain;
						SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[0] += (GLfloat)SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaX;
						SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[1] += (GLfloat)SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaY;
						SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaX = 0; /* if you do not turn these back to 0 then the surface will quickly go out of sight if momentum is turned on */
						SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaY = 0;
					#endif
					/*SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[0] += 0;*/
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[1] +=  (GLfloat)SUMAg_cSV->GVS[SUMAg_cSV->StdView].ArrowtranslateDeltaY/(float)SUMAg_cSV->WindHeight*SUMAg_cSV->GVS[SUMAg_cSV->StdView].TranslateGain;
					postRedisplay();
				}else if (Kev.state & ControlMask){
					/*fprintf (SUMA_STDERR,"%s: Control down\n", FuncName);*/
					/* Top view ctrl+up*/
					float a[3];
					/* Default top view, rotate by nothing */
					a[0] = 1.0; a[1] = 0.0;
					axis_to_quat(a, 0, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					postRedisplay();
				}else if (Kev.state & Mod1Mask) {
					/*fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
				}else {
					/*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
					trackball(SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, 
						0.0, -ArrowDeltaRot, /* first point */
						0.0, ArrowDeltaRot); /* ending x,y */
					/*fprintf(stdout,"\ncurrentQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat[i]);} fprintf(stdout,"\n");
					fprintf(stdout,"\ndeltaQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat[i]);} fprintf(stdout,"\n");*/
					add_quats (SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
					/*fprintf(stdout,"\nnewQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat[i]);} fprintf(stdout,"\n");*/
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX = 0;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY = 2*ArrowDeltaRot*SUMAg_cSV->WindHeight;
					postRedisplay();
						
				}
				
				break;

			default:
				break;

   	} /* keysym */
	break;
	
	case ButtonPress:
	 	/*fprintf(stdout,"In ButtonPress\n");		*/
		switch (Bev.button) { /* switch type of button Press */
			case Button1:
				if (Bev.state & Button2Mask) {
					/* setup initial zooming conditions */
					/*fprintf(SUMA_STDERR,"%s: Button 1 &2 down. New\n", FuncName); */
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomBegin = (float)Bev.y;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomDelta = 0;	
				}else {
					/*fprintf(SUMA_STDERR,"%s: Button 1 down. New\n", FuncName);*/
					/* setup initial spinning conditions */
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinBeginX = (int)Bev.x;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinBeginY = (int)Bev.y;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX = 0;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY = 0;	
				}
				break;
				
			case Button2:
				if (Bev.state & ShiftMask) {
					/* setup initial zooming conditions */
					/*fprintf(SUMA_STDERR,"%s: Button 2 & Shift\n", FuncName); */
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomBegin = (float)Bev.y;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomDelta = 0;	
				} else {	
					/*fprintf(stdout,"Button 2 down, plain jane\n");*/
					/* setup initial translation conditions */
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateBeginX = (int)Bev.x;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateBeginY = (int)Bev.y;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaX = 0;
					SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaY = 0;
				}
				break;
				
			case Button3:
					/*fprintf(stdout,"Button 3 down, plain jane\n");*/
					/* Report on coordinates */
					if (SUMA_ShownSOs(SUMAg_cSV, SUMAg_DOv, NULL) == 0) { /* no surfaces, break */
						break;
					}
					if (SUMA_ShownSOs(SUMAg_cSV, SUMAg_DOv, NULL) == 1) /* only one surface object can be displayed when picking */
					{/* report on coordinates, within case 0 */
						GLfloat rotationMatrix[4][4];
						GLint viewport[4];
						GLdouble mvmatrix[16], projmatrix[16];
						GLint realy; /* OpenGL y coordinate position */
						int x, y;

						x = (int)Bev.x;
						y = (int)Bev.y;

						/* go through the ModelView transforms as you would in display since the modelview matrix is popped
						after each display call */
						build_rotmatrix(rotationMatrix, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
						glMatrixMode(GL_MODELVIEW);
						glPushMatrix();
						glTranslatef (SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[0], SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[1], 0.0);
						glTranslatef (SUMAg_cSV->GVS[SUMAg_cSV->StdView].RotaCenter[0], SUMAg_cSV->GVS[SUMAg_cSV->StdView].RotaCenter[1], SUMAg_cSV->GVS[SUMAg_cSV->StdView].RotaCenter[2]);
						glMultMatrixf(&rotationMatrix[0][0]);
						glTranslatef (-SUMAg_cSV->GVS[SUMAg_cSV->StdView].RotaCenter[0], -SUMAg_cSV->GVS[SUMAg_cSV->StdView].RotaCenter[1], -SUMAg_cSV->GVS[SUMAg_cSV->StdView].RotaCenter[2]);

						glGetIntegerv(GL_VIEWPORT, viewport);
						glGetDoublev(GL_MODELVIEW_MATRIX, mvmatrix);
						glGetDoublev(GL_PROJECTION_MATRIX, projmatrix);
						/* viewport[3] is height of window in pixels */
						realy = viewport[3] - (GLint)y -1;

						/*fprintf (SUMA_STDOUT, "Coordinates at cursor are (%4d, %4d)\n", x, realy);*/

						/* set the pick points at both ends of the clip planes */
						gluUnProject((GLdouble)x, (GLdouble)realy, 0.0,\
							mvmatrix, projmatrix, viewport, \
							&(SUMAg_cSV->Pick0[0]), &(SUMAg_cSV->Pick0[1]), &(SUMAg_cSV->Pick0[2]));
						/*fprintf (SUMA_STDOUT, "World Coords at z=0.0 (near clip plane) are (%f, %f, %f)\n",\
							(SUMAg_cSV->Pick0[0]), (SUMAg_cSV->Pick0[1]), (SUMAg_cSV->Pick0[2]));*/

						gluUnProject((GLdouble)x, (GLdouble)realy, 1.0,\
							mvmatrix, projmatrix, viewport, \
							&(SUMAg_cSV->Pick1[0]), &(SUMAg_cSV->Pick1[1]), &(SUMAg_cSV->Pick1[2]));
						/*fprintf (SUMA_STDOUT, "World Coords at z=1.0 (far clip plane) are (%f, %f, %f)\n",\
							(SUMAg_cSV->Pick1[0]), (SUMAg_cSV->Pick1[1]), (SUMAg_cSV->Pick1[2]));*/

						glPopMatrix();

						/* do the intersection on the surface object in focus */
						if (SUMAg_cSV->Focus_SO_ID < 0)
						{
							fprintf(SUMA_STDERR,"Error %s: SUMAg_cSV->Focus_SO_ID is not set.\nNo intersection will be computed.\n", FuncName);
							break;
						}


						{/* determine intersection */
							SUMA_SurfaceObject *SO;
							float P0f[3], P1f[3];
							SO = (SUMA_SurfaceObject *)SUMAg_DOv[SUMAg_cSV->Focus_SO_ID].OP;
							P0f[0] = SUMAg_cSV->Pick0[0];
							P0f[1] = SUMAg_cSV->Pick0[1];
							P0f[2] = SUMAg_cSV->Pick0[2];
							P1f[0] = SUMAg_cSV->Pick1[0];
							P1f[1] = SUMAg_cSV->Pick1[1];
							P1f[2] = SUMAg_cSV->Pick1[2];

							#ifdef SUMA_LOCAL_FIND_CLOSE_NODES /* snippet of code that may be useful in the future */
							{/* locate the nodes closest to the line */
								/* calculate the distance of the nodes to the line */
								int  i2min, *Indx;
								float *d2, d2min;

								d2 = (float *)calloc(SO->N_Node, sizeof(float));
								if (d2 == NULL) {
									fprintf(SUMA_STDERR, "Error %s: Could not allocate for d2\n", FuncName);
									break;
								}
								if (!SUMA_Point_To_Line_Distance (SO->NodeList, SO->N_Node, P0f, P1f, d2, &d2min, &i2min)) {
									fprintf(SUMA_STDERR, "Error %s: SUMA_Point_To_Line_Distance Failed\n", FuncName);
									break;
								}
								free(do_id);
								/* report some results */
								fprintf (SUMA_STDOUT, "Node [%d] %f, %f, %f was closest (%f mm) to line\n", \
									i2min, SO->NodeList[i2min][0], SO->NodeList[i2min][1], SO->NodeList[i2min][2], sqrt(d2min));
								/* sort the distances to the line and find the closest 50 nodes */
								Indx = SUMA_z_qsort ( d2 , SO->N_Node );	
								/* from the closest nodes, find the one that is closest to P0f (that would be closest to the user) that should be the node of choice */
								/* a better way to do this is to find the polygon that is pierced by the line and highlight the node closest to the line in that
								polygon. And in case there are multiple polygons pierced, choose the one that is closest to P0f. This way, you are guaranteed to 
								get the correct node */

								/* prepare EngineData to points cetered on closest node (poor man's method for now)*/
								fv15[0] = SO->NodeList[i2min][0];
								fv15[1] = SO->NodeList[i2min][1];
								fv15[2] = SO->NodeList[i2min][2];

								fv15[3] = 5.0;
								fv15[4] = 5.0;
								fv15[5] = 5.0;

								/* register fv15 with EngineData */
								sprintf(sfield,"fv15");
								sprintf(sdestination,"HighlightNodes");
								if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)fv15, sdestination, ssource, NOPE)) {
									fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
									break;
								}					

								/*make call to SUMA_Engine */
								sprintf(CommString,"Redisplay|HighlightNodes~");
								if (!SUMA_Engine (CommString, &EngineData)) {
									fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
								}
								/* done with d2, and others, free them */
								if (d2 != NULL) free(d2);
								free (Indx);
							}/* locate the nodes closest to the line */
							#endif

							/* Now the FaceSetIntersection game */
							{/* FaceSet Intersection */
								SUMA_MT_INTERSECT_TRIANGLE *MTI;

								if (SO->FaceSetDim != 3) {
									fprintf(SUMA_STDERR,"Error %s: SUMA_MT_intersect_triangle only works for triangular meshes.\n", FuncName);
								} else {
									MTI = SUMA_MT_intersect_triangle(P0f, P1f, SO->NodeList, SO->N_Node, SO->FaceSetList, SO->N_FaceSet);
									if (MTI == NULL) {
										fprintf(SUMA_STDERR,"Error %s: SUMA_MT_intersect_triangle failed.\n", FuncName);
									}else {
										/*
										if (!SUMA_Show_MT_intersect_triangle(MTI, NULL)) {
											fprintf(SUMA_STDERR,"Error %s: SUMA_Show_MT_intersect_triangle failed.\n", FuncName);
										} 
										*/
										/* Mark intersection Facsets */
											if (MTI->N_hits) {
												/* print nodes about the closets faceset*/
												fprintf(SUMA_STDOUT, "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
												fprintf(SUMA_STDOUT, "Nodes forming closest FaceSet:\n");
												fprintf(SUMA_STDOUT, "%d, %d, %d\n", \
												SO->FaceSetList[MTI->ifacemin][0], SO->FaceSetList[MTI->ifacemin][1],SO->FaceSetList[MTI->ifacemin][2]);

												fprintf (SUMA_STDOUT,"Coordinates of Nodes forming closest FaceSet:\n");
												for (it=0; it < 3; ++it) { 
													fprintf(SUMA_STDOUT, "%f, %f, %f\n", SO->NodeList[SO->FaceSetList[MTI->ifacemin][it]][0],\
																										SO->NodeList[SO->FaceSetList[MTI->ifacemin][it]][1],\
																										SO->NodeList[SO->FaceSetList[MTI->ifacemin][it]][2]);
												}
												fprintf(SUMA_STDOUT, "\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");
												#ifdef SUMA_LOCAL_COLORINTERSECTION
												EngineData.N_rows = 3;
												EngineData.N_cols = 4;
												fm = (float **)SUMA_allocate2D(EngineData.N_rows, EngineData.N_cols, (sizeof(float)));
												fm[0][0] = SO->FaceSetList[MTI->ifacemin][0];
												fm[0][1] = 0.0; fm[0][2] = 1.0; fm[0][3] = 1.0; 
												fm[1][0] = SO->FaceSetList[MTI->ifacemin][1];
												fm[1][1] = 0.0; fm[1][2] = 1.0; fm[1][3] = 1.0; 
												fm[2][0] = SO->FaceSetList[MTI->ifacemin][2];
												fm[2][1] = 0.0; fm[2][2] = 1.0; fm[2][3] = 1.0; 
												/* register fm with EngineData */
													sprintf(sfield,"fm");
													sprintf(sdestination,"SetNodeColor");
													if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)fm, sdestination, ssource, YUP)) {
														fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
														break;
													}

												sprintf(CommString,"SetNodeColor~");
												if (!SUMA_Engine (CommString, &EngineData)) {
													fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
												}

												/* free fm since it was registered by pointer and is not automatically freed after the call to SUMA_Engine */
												if (fm) SUMA_free2D ((char **)fm, EngineData.N_rows);

												#endif

												/* Set the Nodeselection at the closest node */
												it = MTI->inodemin;
												sprintf(sfield,"i");
												sprintf(sdestination,"SetSelectedNode");
												if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)(&it), sdestination, ssource, NOPE)) {
													fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
													break;
												}
												sprintf(CommString,"Redisplay|SetSelectedNode~");
												if (!SUMA_Engine (CommString, &EngineData)) {
													fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
												}

												/* Set the FaceSetselection */
												it = MTI->ifacemin;
												sprintf(sfield,"i");
												sprintf(sdestination,"SetSelectedFaceSet");
												if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)(&it), sdestination, ssource, NOPE)) {
													fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
													break;
												}
												sprintf(CommString,"SetSelectedFaceSet~");
												if (!SUMA_Engine (CommString, &EngineData)) {
													fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
												}
												/* Now set the cross hair position at the intersection*/
												sprintf(sfield,"fv3");
												sprintf(sdestination,"SetCrossHair");
												if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)MTI->P, sdestination, ssource,NOPE)) {
													fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
													break;
												}
												sprintf(CommString,"SetCrossHair~");
												if (!SUMA_Engine (CommString, &EngineData)) {
													fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
												}

												/* attach the cross hair to the selected surface */
												iv3[0] = SUMA_findDO(SO->idcode_str, SUMAg_DOv, SUMAg_N_DOv);
												iv3[1] = MTI->inodemin;
												sprintf(sfield,"iv3");
												sprintf(sdestination,"BindCrossHair");
												if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)(iv3), sdestination, ssource, NOPE)) {
													fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
													break;
												}
												sprintf(CommString,"Redisplay|BindCrossHair~");
												if (!SUMA_Engine (CommString, &EngineData)) {
													fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
												}

											}
											/* clear MTI */
											if (!SUMA_Free_MT_intersect_triangle(MTI)) 
												fprintf(SUMA_STDERR,"Error %s: SUMA_Free_MT_intersect_triangle failed.\n", FuncName);
									}
								}
							}/* FaceSet Intersection */	
						}/* determine intersection */
					}/* report on coordinates, within case 0 */
					else {
						fprintf(SUMA_STDERR,"Error %s: Cannot pick when more than one Surface Object is displayed.\nThis can be implemented if needed, please complain to the author.(ziad@nih.gov)\n", FuncName); 
						break;
					}												
				break;
		} /* switch type of button Press */
		break;
		
	case ButtonRelease:
		/*fprintf(SUMA_STDERR,"%s: In ButtonRelease\n", FuncName); */
		break;
		
	case MotionNotify:
	 	/*fprintf(stdout,"In MotionNotify\n"); */
		if (((Mev.state & Button1MotionMask) && (Mev.state & Button2MotionMask)) || ((Mev.state & Button2MotionMask) && (Mev.state & ShiftMask))) {
			/*fprintf(SUMA_STDERR,"%s: In motion, Butt1 & Butt2\n", FuncName);*/
			SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomDelta = 1.0 + (float)((int)Mev.y - SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomBegin)/MOUSE_ZOOM_FACT;
			if (SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomDelta > 2.0) SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomDelta = 2.0;
			else if (SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomDelta < 0.5) SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomDelta = 0.5;
			SUMAg_cSV->FOV[SUMAg_cSV->iState] /= SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomDelta;
			if (SUMAg_cSV->FOV[SUMAg_cSV->iState] < FOV_MIN) SUMAg_cSV->FOV[SUMAg_cSV->iState] = FOV_MIN;
			else if (SUMAg_cSV->FOV[SUMAg_cSV->iState] > FOV_MAX) SUMAg_cSV->FOV[SUMAg_cSV->iState] = FOV_MAX;
				SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomBegin = (float)(int)Mev.y;
				/*fprintf(stdout, "FOV zoom Delta = %f=n", SUMAg_cSV->GVS[SUMAg_cSV->StdView].zoomDelta);*/
			postRedisplay();			
		} else if(Mev.state & Button1MotionMask) {
			/*fprintf(SUMA_STDERR,"%s: In motion, Butt1 \n", FuncName); */
			/* spinning mode */
			SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX = ((int)Mev.x - SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinBeginX);
			SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY = ((int)Mev.y - SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinBeginY);
			/*fprintf(stdout,"\nspinBeginX %d spinBeginY %d\nspinDeltaX %d spinDeltaY %d\nWindWidth %d WindHeight %d\n", \
							SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinBeginX, SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinBeginY, SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX, SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY, SUMAg_cSV->WindWidth, SUMAg_cSV->WindHeight);*/
			if (SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX || SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY){
				trackball(SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, 
					(float)(2*SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinBeginX - SUMAg_cSV->WindWidth)/(float)SUMAg_cSV->WindWidth, (float)(SUMAg_cSV->WindHeight - 2*SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinBeginY)/(float)SUMAg_cSV->WindHeight,
				 	(float)(2*(int)Mev.x - SUMAg_cSV->WindWidth)/(float)SUMAg_cSV->WindWidth, (float)(SUMAg_cSV->WindHeight - 2*(int)Mev.y)/(float)SUMAg_cSV->WindHeight); /* comput the increment Quat */
				SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinBeginX = (int)Mev.x;
				SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinBeginY = (int)Mev.y;
				add_quats (SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
				postRedisplay();
			}
		
		}else if(Mev.state & Button2MotionMask) { 
			/* fprintf(SUMA_STDERR,"%s: In motion, Butt2 \n", FuncName);*/
			SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaX = (float)((int)Mev.x - SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateBeginX)/(float)SUMAg_cSV->WindWidth*SUMAg_cSV->GVS[SUMAg_cSV->StdView].TranslateGain;
			SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaY = -(float)((int)Mev.y - SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateBeginY)/(float)SUMAg_cSV->WindHeight*SUMAg_cSV->GVS[SUMAg_cSV->StdView].TranslateGain;
			if (SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaX || SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaY){
				SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[0] += (GLfloat)SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaX;
				SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[1] += (GLfloat)SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaY;
				SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateBeginX = (int)Mev.x;
				SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateBeginY = (int)Mev.y;
				postRedisplay();
			}
				
		}
		
		break;
  }/* switch event type */
}

void momentum(XtPointer clientData, XtIntervalId *id)
{
	static int ReDisp;
	ReDisp = 0;
	/*fprintf(stdout,"In momentum ...\n");*/
		if ( ((SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX*SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX) > SUMAg_cSV->GVS[SUMAg_cSV->StdView].MinIdleDelta ) || ((SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY*SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY) > SUMAg_cSV->GVS[SUMAg_cSV->StdView].MinIdleDelta ) ) 
		{ /* rotate if momentum is enabled and spinDeltaX or spinDeltaY are larger than the minimum set */ 
			/*fprintf(stdout,"momentum:  spinDeltaX %d spinDeltaY %d\n",  SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaX, SUMAg_cSV->GVS[SUMAg_cSV->StdView].spinDeltaY);*/
			add_quats (SUMAg_cSV->GVS[SUMAg_cSV->StdView].deltaQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat, SUMAg_cSV->GVS[SUMAg_cSV->StdView].currentQuat);
			ReDisp = 1;
		}
		if ( ((SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaX*SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaX) > SUMAg_cSV->GVS[SUMAg_cSV->StdView].MinIdleDelta ) || ((SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaY*SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaY) > SUMAg_cSV->GVS[SUMAg_cSV->StdView].MinIdleDelta ) )
		{ /* translate */
			SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[0] += (GLfloat)SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaX;
			SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateVec[1] += (GLfloat)SUMAg_cSV->GVS[SUMAg_cSV->StdView].translateDeltaY;
			ReDisp = 1;
		}
	if (ReDisp) {
		/*fprintf(stdout,"Momentum Redisplay\n");*/
		postRedisplay();
	}
	SUMAg_cSV->X->MOMENTUMID = XtAppAddTimeOut(SUMAg_cSV->X->APP, 1, momentum, 0);

  return;         
}

 
