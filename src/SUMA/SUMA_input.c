#include "SUMA_suma.h"

/* extern SUMA_SurfaceViewer *SUMAg_cSV; */ /* no longer in use Tue Aug 13 15:55:29 EDT 2002 */
extern int SUMAg_N_DOv; 
extern SUMA_DO *SUMAg_DOv;
extern SUMA_CommonFields *SUMAg_CF; 
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv;

/*! Mouse and Keyboard input handler function */

void
SUMA_input(Widget w, XtPointer clientData, XtPointer callData)
{
   GLwDrawingAreaCallbackStruct *cd;
   char buffer[10], cbuf = '\0', cbuf2='\0';
   KeySym keysym;
   int xls, ntot, id = 0, ND, ip, NP;
   float ArrowDeltaRot = 0.05; /* The larger the value, the bigger the rotation increment */
   SUMA_EngineData EngineData; /* Do not free EngineData, only its contents*/
   char CommString[SUMA_MAX_COMMAND_LENGTH];
   static char FuncName[]= {"SUMA_input"};
   char s[SUMA_MAX_STRING_LENGTH], sfield[100], sdestination[100];
   static char ssource[]={"suma"};
   int it, ii, iv3[3], hit = 0;
   float **fm, fv3[3], fv15[15];
   XKeyEvent Kev;
   XButtonEvent Bev;
   XMotionEvent Mev;
   int isv;
   SUMA_SurfaceViewer *sv;
   GLfloat *glar_ColorList = NULL;
   static Time B1time = 0;
   static int pButton, mButton, rButton;
   SUMA_Boolean ROI_mode, DoubleClick;
   SUMA_Boolean LocalHead = NOPE; /* local debugging messages */

   /*float ft;
   int **im, iv15[15];*/ /* keep unused variables undeclared to quite compiler */

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* get the callData pointer */
   cd = (GLwDrawingAreaCallbackStruct *) callData;
   
   /* find out who's calling, only GLXAREA calls this function */
   SUMA_GLXAREA_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_GLXAREA_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }
   if (LocalHead) fprintf (SUMA_STDERR,"%s: A call from SUMA_SurfaceViewer[%d], Pointer %p\n", FuncName, isv, sv);
   
   /* initialize EngineData */
   if (!SUMA_InitializeEngineData (&EngineData)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to initialize EngineData\n", FuncName);
      SUMA_RETURNe;
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
               SUMA_SurfaceObject *SO = NULL, *SOmap = NULL;
               int curstateID = -1, nxtstateID = -1, dov_ID = -1;

               curstateID = SUMA_WhichState(sv->State, sv);
               SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
               if (SUMA_isINHmappable (SO)) {
                  /* get the last non mappable state in SV */
                  if (sv->LastNonMapStateID < 0) { /* not recorded, complain and quit */
                     fprintf(SUMA_STDERR,"Warning %s: Nothing defined to toggle with yet.\n", FuncName); 
                     break;
                  }
                  
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,"%s: surface is inherrently mappable, switching to last non mappable state %d.\n", \
                        FuncName, sv->LastNonMapStateID);
                        
                  if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, sv, sv->LastNonMapStateID)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SwitchState.\n", FuncName);
                     break;
                  }

               } else {/* that's a non mappable, go to state containing reference */
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,"%s: surface is not inherrently mappable, searching for mapping reference and its state.\n", \
                        FuncName);
                        
                  /* find SO that is mappable reference & get corresponding state ID*/
                  dov_ID = SUMA_findSO_inDOv(SO->MapRef_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
                  SOmap = (SUMA_SurfaceObject *)SUMAg_DOv[dov_ID].OP;
                  nxtstateID = SUMA_WhichState(SOmap->State, sv);
                  
                  if (nxtstateID < 0) {
                     fprintf (SUMA_STDERR,"%s: Failed in SUMA_findSO_inDOv This should not happen.\n", FuncName);
                     break;
                  }
                  
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,"%s: Found mapping reference in viewer state %d.\n", FuncName, nxtstateID);
                     
                  /* store this location */
                  sv->LastNonMapStateID = curstateID;

                  /* go there */
                  if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, sv, nxtstateID)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SwitchState.\n", FuncName);
                     break;
                  }
               }

            }
            SUMA_postRedisplay(w, clientData, callData);
            break;

         case XK_Escape: /* there's more:  XK_BackSpace XK_Tab XK_Linefeed XK_Return XK_Delete */
            /* control mask and escape is grabbed by gnome window manager .... */
            if (Kev.state & ShiftMask){/* kill all */
               exit(0);
            }else { 
               SUMA_ButtClose_pushed (w, clientData, callData);
            }
            break;

         case XK_a:
            /* toggle background attenuation */

            if (sv->Back_Modfact) {
               fprintf (SUMA_STDOUT,"%s: Modulation by background intensity OFF.\n", FuncName);
               sv->Back_Modfact = 0;
            } else {
               fprintf (SUMA_STDOUT,"%s: Modulation by background intensity ON.\n", FuncName);
               sv->Back_Modfact = SUMA_BACKGROUND_MODULATION_FACTOR;
            }
            
            /* set the remix flag */
            if (!SUMA_SetShownLocalRemixFlag (sv)) {
               fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SetShownLocalRemixFlag.\n", FuncName);
               break;
            }
            
            #if 0
            {
               SUMA_SurfaceObject *SO = NULL;

               for (ii=0; ii< sv->N_DO; ++ii) {
                  if (SUMA_isSO(SUMAg_DOv[sv->ShowDO[ii]])) {
                     SO = (SUMA_SurfaceObject*)SUMAg_DOv[sv->ShowDO[ii]].OP;
                     /* remix colors */
                     glar_ColorList = SUMA_GetColorList (sv, SO->idcode_str);
                     if (!glar_ColorList) {
                        fprintf (SUMA_STDERR,"Error %s: NULL glar_ColorList.\n", FuncName);
                        SUMA_RETURNe;
                     }
                     if (!SUMA_Overlays_2_GLCOLAR4(SO->Overlays, SO->N_Overlays, glar_ColorList, SO->N_Node, \
                        sv->Back_Modfact, sv->ShowBackground, sv->ShowForeground)) {
                        fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Overlays_2_GLCOLAR4.\n", FuncName);
                        SUMA_RETURNe;
                     }
                  }
               }
            }
            #endif

            SUMA_postRedisplay(w, clientData, callData);
            break;

         case XK_B:
            if (SUMAg_CF->Dev)  {
               sv->BF_Cull = !sv->BF_Cull;
               if (sv->BF_Cull) {
                  glCullFace (GL_BACK);
                  glEnable (GL_CULL_FACE);
                  fprintf (SUMA_STDOUT,"%s: BackFace Culling enabled.\n", FuncName);
               } else {
                  glDisable(GL_CULL_FACE);
                  fprintf (SUMA_STDOUT,"%s: BackFace Culling disabled.\n", FuncName);
            }
               SUMA_postRedisplay(w, clientData, callData);
            }
            break;

         case XK_b:
            /* Show/hide the background */
            sprintf(CommString,"Redisplay|ToggleBackground~");
            if (!SUMA_Engine (CommString, &EngineData, sv)) {
               fprintf(SUMA_STDERR, "Error SUMA_input: SUMA_Engine call failed.\n");
            }
            break;            

         case XK_c:
            fprintf(stdout,"Enter name of color file (enter nothing to cancel): ");
            /*Load colors from file */
            {int i=0;
               while ((cbuf = getc(stdin)) != '\n' && i < SUMA_MAX_STRING_LENGTH-1) {
                  s[i] = cbuf;
                  ++ i;
               }
               if (i == SUMA_MAX_STRING_LENGTH-1) {
                  fprintf(SUMA_STDERR,"Error %s: Filename should not be longer than %d.\n", FuncName, SUMA_MAX_STRING_LENGTH-1);
                  fflush(stdin);
                  SUMA_RETURNe;
               }
               s[i] = '\0';
               if (!i) SUMA_RETURNe;
            }
            EngineData.N_cols = 4;
            /* find out if file exists and how many values it contains */
            ntot = SUMA_float_file_size (s);
            if (ntot < 0) {
               fprintf(stderr,"Error SUMA_input: filename %s could not be open.\n", s);
               SUMA_RETURNe;
            }

            /* make sure it's a full matrix */
            if ((ntot % EngineData.N_cols)) {
               fprintf(stderr,"Error SUMA_Read_2Dfile: file %s contains %d values, not divisible by ncols %d.\n", s, ntot, EngineData.N_cols);
               SUMA_RETURNe;
            }
            EngineData.N_rows = ntot/EngineData.N_cols;

            /* allocate space */
            fm = (float **)SUMA_allocate2D (EngineData.N_rows, EngineData.N_cols, sizeof(float));
            if (fm == NULL) {
               fprintf(stderr,"Error SUMA_input: Failed to allocate space for fm\n");
               SUMA_RETURNe;
            }

            EngineData.N_rows = SUMA_Read_2Dfile (s, fm, EngineData.N_cols, EngineData.N_rows);
            if (EngineData.N_rows < 0) {
               fprintf(stderr,"SUMA_input Error: Failed to read full matrix from %s\n", s);
               SUMA_RETURNe;
            }

            /*register fm with EngineData */
            sprintf(sfield,"fm");
            sprintf(sdestination,"SetNodeColor");
            if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)fm, sdestination, ssource, YUP)) {
               fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
               break;
            }

            sprintf(CommString,"Redisplay|SetNodeColor~");
            if (!SUMA_Engine (CommString, &EngineData, sv)) {
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

         case XK_e:
            if (SUMAg_CF->Dev) {
               if (Kev.state & Mod1Mask){ /* alt + e */
                  int error, cnt = 0;
                  fprintf (SUMA_STDERR, "%s: Looking for OpenGL errors ...\n", FuncName);
                  while ((error = glGetError()) != GL_NO_ERROR) {
                     ++cnt;
                    fprintf (SUMA_STDERR, "GL error %d: %s\n", cnt, gluErrorString(error)); 
                  }
                  if (!cnt) {
                     fprintf (SUMA_STDERR, "%s: No errors found.\n", FuncName);
                  }
               }
            }
            break;
            
         case XK_F:
            sprintf(CommString,"Redisplay|FlipLight0Pos~");
            if (!SUMA_Engine (CommString, &EngineData, sv)) {
               fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
            }
            break;

         case XK_f:
            /* Show/hide the foreground */
            sprintf(CommString,"Redisplay|ToggleForeground~");
            if (!SUMA_Engine (CommString, &EngineData, sv)) {
               fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
            }
            break;            

         case XK_H:
            if (SUMAg_CF->Dev) {
               fprintf(stdout,"Enter XYZ of center followed by size of Box (enter nothing to cancel):\n");

               it = SUMA_ReadNumStdin (fv15, 6);
               if (it > 0 && it < 6) {
                  fprintf(SUMA_STDERR,"Error %s: read %d values, expected 6.\n", FuncName, it);
                  SUMA_RETURNe;
               }else if (it < 0) {
                  fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ReadNumStdin.\n", FuncName);
                  SUMA_RETURNe;
               }else if (it == 0) {
                  SUMA_RETURNe;
               }
            
               fprintf (SUMA_STDOUT, "Parsed Input:\n\tCenter %f, %f, %f.\n\tBox Size %f, %f, %f\n",\
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
               if (!SUMA_Engine (CommString, &EngineData, sv)) {
                  fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
               }
            }
            break;

         case XK_h:
            if (Kev.state & ControlMask){
               fprintf(SUMA_STDOUT,"Enter Debug Flags: (0/1)\n");
               fflush (stdin);
               do {
                  fprintf(SUMA_STDOUT,"-InOut_Notify (current %d): ", SUMAg_CF->InOut_Notify);
                   cbuf = getc(stdin);
                   if (cbuf != 'n') {
                      cbuf2 = getc(stdin);
                   }else { cbuf2 = cbuf; }
               } while (cbuf2 != 'n' && cbuf != '1' && cbuf != '0'); 
               if (cbuf == '1') { SUMAg_CF->InOut_Notify = YUP; SUMAg_CF->InOut_Level = 1;
               }else if (cbuf == '0') { SUMAg_CF->InOut_Notify = NOPE; SUMAg_CF->InOut_Level = 0;}   
               #if SUMA_MEMTRACE_FLAG
                  fflush (stdin);
                  do {
                     fprintf(SUMA_STDOUT,"-MemTrace (current %d): ", SUMAg_CF->MemTrace);
                      cbuf = getc(stdin);
                      if (cbuf != 'n') {
                         cbuf2 = getc(stdin);
                      }else { cbuf2 = cbuf; }
                  } while (cbuf2 != 'n' && cbuf != '1' && cbuf != '0'); 
                  if (cbuf == '1') { SUMAg_CF->MemTrace = YUP; 
                  }else if (cbuf == '0') { SUMAg_CF->MemTrace = NOPE;}   
               #endif   
            }else {
               SUMA_help_message(NULL);
            }
            break;

         case XK_j:
            if (SUMAg_CF->Dev) {
               if (Kev.state & ControlMask){     
                  fprintf(stdout,"Enter XYZ location to center cross hair at (nothing to cancel):\n");
                  it = SUMA_ReadNumStdin (fv3, 3);
                  if (it < 0) {
                     fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ReadNumStdin.\n", FuncName);
                     SUMA_RETURNe;
                  }else if (it == 0) {
                     SUMA_RETURNe;
                  }
                  /* Now set the cross hair position */
                     sprintf(sfield,"fv3");
                     sprintf(sdestination,"SetCrossHair");
                     if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)fv3, sdestination, ssource,NOPE)) {
                        fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
                        SUMA_RETURNe;
                     }
                     sprintf(CommString,"SetCrossHair~");
                     if (!SUMA_Engine (CommString, &EngineData, sv)) {
                        fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
                        SUMA_RETURNe;
                     }
               } else if (Kev.state & Mod1Mask){     
                  fprintf(stdout,"Enter index of focus node, cross hair's XYZ will not be affected (nothing to cancel):\n");
                  it = SUMA_ReadNumStdin (fv3, 1);
                  if (it < 0) {
                     fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ReadNumStdin.\n", FuncName);
                     SUMA_RETURNe;
                  }else if (it == 0) {
                     SUMA_RETURNe;
                  }
                  /* Set the Nodeselection  */
                  it = (int) fv3[0];
                  sprintf(sfield,"i");
                  sprintf(sdestination,"SetSelectedNode");
                  if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)(&it), sdestination, ssource, NOPE)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
                     SUMA_RETURNe;
                  }
                  sprintf(CommString,"SetSelectedNode~");
                  if (!SUMA_Engine (CommString, &EngineData, sv)) {
                     fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
                     SUMA_RETURNe;
                  }
               } else {
                  fprintf(stdout,"Enter index of node to send the cross hair to (nothing to cancel):\n");
                  it = SUMA_ReadNumStdin (fv3, 1);
                  if (it < 0) {
                     fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ReadNumStdin.\n", FuncName);
                     SUMA_RETURNe;
                  }else if (it == 0) {
                     SUMA_RETURNe;
                  }
                  /* Set the Nodeselection  */
                  it = (int) fv3[0];
                  sprintf(sfield,"i");
                  sprintf(sdestination,"SetSelectedNode");
                  if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)(&it), sdestination, ssource, NOPE)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
                     SUMA_RETURNe;
                  }
                  sprintf(CommString,"SetSelectedNode~");
                  if (!SUMA_Engine (CommString, &EngineData, sv)) {
                     fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
                     SUMA_RETURNe;
                  }

                  /* Now set the cross hair position at the selected node*/
                  {
                     SUMA_SurfaceObject *SO= NULL;
                     SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
                     sprintf(sfield,"fv3");
                     sprintf(sdestination,"SetCrossHair");
                     if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)&(SO->NodeList[3*it]), sdestination, ssource,NOPE)) {
                        fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
                        SUMA_RETURNe;
                     }
                     sprintf(CommString,"SetCrossHair~");
                     if (!SUMA_Engine (CommString, &EngineData, sv)) {
                        fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
                        SUMA_RETURNe;
                     }
                  }

               }

               /* redisplay curent only*/
               sv->ResetGLStateVariables = YUP;
               SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
            }
            break;
         
         case XK_J:
            if (SUMAg_CF->Dev) {
               fprintf(stdout,"Enter index of FaceSet to highlight (nothing to cancel):\n");
               it = SUMA_ReadNumStdin (fv3, 1);
               if (it < 0) {
                  fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ReadNumStdin.\n", FuncName);
                  SUMA_RETURNe;
               }else if (it == 0) {
                  SUMA_RETURNe;
               }
               /* Set the Nodeselection  */
               it = (int) fv3[0];
               sprintf(sfield,"i");
               sprintf(sdestination,"SetSelectedFaceSet");
               if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)(&it), sdestination, ssource, NOPE)) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
                  SUMA_RETURNe;
               }
               sprintf(CommString,"SetSelectedFaceSet~");
               if (!SUMA_Engine (CommString, &EngineData, sv)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
                  SUMA_RETURNe;
               }

               /* redisplay curent only*/
               sv->ResetGLStateVariables = YUP;
               SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
            }
            break; 
              
         case XK_l:
            if (Kev.state & ControlMask){
               if (SUMAg_CF->Dev) {
                  char LockName[100];
                  SUMA_LockEnum_LockType (SUMAg_CF->Locked[0], LockName);
                  fprintf (SUMA_STDERR,"%s: Switching Locktype from %s", FuncName, LockName);
                  /* change the locking type of viewer 0 */
                  SUMAg_CF->Locked[0] = (int)fmod(SUMAg_CF->Locked[0]+1, SUMA_N_Lock_Types);
                  SUMA_LockEnum_LockType (SUMAg_CF->Locked[0], LockName);
                  fprintf (SUMA_STDERR," %s\n", LockName);
                  /* Change the locking type of all remaining viewers */
                  for (ii=1; ii<SUMAg_N_SVv; ++ii) {
                     SUMAg_CF->Locked[ii] = SUMAg_CF->Locked[0];                  
                  }
               }
            } else {
               fprintf(stdout,"Enter XYZ coordinates to look at (enter nothing to cancel):\n");

               it = SUMA_ReadNumStdin (fv3, 3);
               if (it > 0 && it < 3) {
                  fprintf(SUMA_STDERR,"Error %s: read %d values, expected 3.\n", FuncName, it);
                  SUMA_RETURNe;
               }else if (it < 0) {
                  fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ReadNumStdin.\n", FuncName);
                  SUMA_RETURNe;
               }else if (it == 0) {
                  SUMA_RETURNe;
               }

               fprintf(stdout,"Parsed input: %f %f %f\n", fv3[0], fv3[1],fv3[2]);

               /* register fv3 with EngineData */
               sprintf(sfield,"fv3");
               sprintf(sdestination,"SetLookAt");
               if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)fv3, sdestination, ssource, NOPE)) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
                  break;
               }

               sprintf(CommString,"SetLookAt~");         
               if (!SUMA_Engine (CommString, &EngineData, sv)) {
                  fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
               }
            }
            break;

         case XK_L:
            if (SUMAg_CF->Dev) {
               fprintf(stdout,"NOT USED, Sorry.\n");
               SUMA_RETURNe;
               fprintf(stdout,"Enter XYZ coordinates to look from (enter nothing to cancel):\n");
               it = SUMA_ReadNumStdin (fv3, 3);
               if (it > 0 && it < 3) {
                  fprintf(SUMA_STDERR,"Error %s: read %d values, expected 3.\n", FuncName, it);
                  SUMA_RETURNe;
               }else if (it < 0) {
                  fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ReadNumStdin.\n", FuncName);
                  SUMA_RETURNe;
               }else if (it == 0) {
                  SUMA_RETURNe;
               }
            
               fprintf(stdout,"Parsed input: %f %f %f\n", fv3[0], fv3[1],fv3[2]);

            }
            break;

         case XK_M:
            if (Kev.state & ControlMask){
               #if SUMA_MEMTRACE_FLAG
                  if (SUMAg_CF->MemTrace) {
                    SUMA_ShowMemTrace (SUMAg_CF->Mem, NULL);
                  } else {
                     fprintf (SUMA_STDERR,"%s: Memtrace is disabled. Try ctrl+h.\n", FuncName);
                     SUMA_RETURNe;
                  }
               #endif
            }
            break;
            
         case XK_m:
               if (Kev.state & ControlMask){
                  if (SUMAg_CF->Dev) {
                     SUMA_SurfaceObject *SO;
                     
                     fprintf(SUMA_STDOUT, "%s: Enter mm distance [RAI] to move center of all mappable surfaces in DOv by.\n", FuncName);
                     it = SUMA_ReadNumStdin (fv3, 3);
                     if (it > 0 && it < 3) {
                        fprintf(SUMA_STDERR,"Error %s: read %d values, expected 3.\n", FuncName, it);
                        SUMA_RETURNe;
                     }else if (it < 0) {
                        fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ReadNumStdin.\n", FuncName);
                        SUMA_RETURNe;
                     }else if (it == 0) {
                        fprintf(SUMA_STDERR,"%s: Nothing read.\n", FuncName);
                        SUMA_RETURNe;
                     }
                     
                     for (it = 0; it < SUMAg_N_DOv; ++it) {
                        if (SUMA_isSO (SUMAg_DOv[it])) {
                           SO = (SUMA_SurfaceObject *)SUMAg_DOv[it].OP;
                           if (SUMA_isINHmappable(SO)) {
                              int imax;
                              /* add the shift */
                              fprintf (SUMA_STDERR,"%s: Shifting %s by %f %f %f mm RAI.\n", FuncName, SO->Label, fv3[0], fv3[1], fv3[2]);
                              ii = 0;
                              imax = 3 * SO->N_Node;
                              while (ii < imax) {
                                 SO->NodeList[ii] += fv3[0]; ++ii;
                                 SO->NodeList[ii] += fv3[1]; ++ii;
                                 SO->NodeList[ii] += fv3[2]; ++ii;
                              }
                           }
                        }
                     }
                     
                     SUMA_postRedisplay(w, clientData, callData);
                  }
               } else {
                  sv->GVS[sv->StdView].ApplyMomentum = !sv->GVS[sv->StdView].ApplyMomentum;
                  if (sv->GVS[sv->StdView].ApplyMomentum) {
                      sv->X->MOMENTUMID = XtAppAddTimeOut(SUMAg_CF->App, 1, SUMA_momentum, (XtPointer) w);
                      /* wait till user initiates turning */
                     sv->GVS[sv->StdView].spinDeltaX = 0; sv->GVS[sv->StdView].spinDeltaY = 0;
                     sv->GVS[sv->StdView].translateDeltaX = 0; sv->GVS[sv->StdView].translateDeltaY = 0;
                  }
                   else {
                     if (sv->X->MOMENTUMID)  XtRemoveTimeOut(sv->X->MOMENTUMID);
                     }
               }
             break;

         case XK_n:
            if (SUMAg_CF->Dev) {
               
               if (Kev.state & ControlMask){
                  fprintf(SUMA_STDOUT, "%s: Opening a new controller...\n", FuncName);
                  /* open a new controller */
                  if (!SUMA_X_SurfaceViewer_Create ()) {
                     fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_X_SurfaceViewer_Create.\n", FuncName);
                     SUMA_RETURNe;
                  } 
               }else {
                  fprintf(stdout,"Enter XYZ of center followed by size of Box (enter nothing to cancel):\n");

                  it = SUMA_ReadNumStdin (fv15, 6);
                  if (it > 0 && it < 6) {
                     fprintf(SUMA_STDERR,"Error %s: read %d values, expected 6.\n", FuncName, it);
                     SUMA_RETURNe;
                  }else if (it < 0) {
                     fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ReadNumStdin.\n", FuncName);
                     SUMA_RETURNe;
                  }else if (it == 0) {
                     SUMA_RETURNe;
                  }

                  fprintf (SUMA_STDOUT, "Parsed Input:\n\tCenter %f, %f, %f.\n\tBox Size %f, %f, %f\n", \
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
                  if (!SUMA_Engine (CommString, &EngineData, sv)) {
                     fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
                  }
               }
            }
            break;

         case XK_p:
             sv->PolyMode = ((sv->PolyMode+1) % 3);
             switch (sv->PolyMode) {
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
            SUMA_postRedisplay(w, clientData, callData);
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
                  SUMA_free(do_id);
               }
               break;
            }
         case XK_s:
            if ((Kev.state & Mod1Mask) && (Kev.state & ControlMask) && SUMAg_CF->Dev){
               int i=0;
                  FILE *tmp;
                  int itmp, itmp2;
                  SUMA_SegmentDO *SDO = NULL;
                  
                  fprintf(stdout,"Enter name of segments file (enter nothing to cancel): ");
                  /* load segments from file */
                     while ((cbuf = getc(stdin)) != '\n' && i < SUMA_MAX_STRING_LENGTH-1) {
                        s[i] = cbuf;
                        ++ i;
                     }
                     if (i == SUMA_MAX_STRING_LENGTH-1) {
                        fprintf(SUMA_STDERR,"Error %s: Filename should not be longer than %d.\n", FuncName, SUMA_MAX_STRING_LENGTH-1);
                        fflush(stdin);
                        SUMA_RETURNe;
                     }
                     s[i] = '\0';
                     if (!i) SUMA_RETURNe;

                  /* find out if file exists and how many values it contains */
                  ntot = SUMA_float_file_size (s);
                  if (ntot < 0) {
                     fprintf(SUMA_STDERR,"Error %s: filename %s could not be open.\n", FuncName,  s);
                     SUMA_RETURNe;
                  }

                  /* make sure it's a full matrix */
                  if ((ntot % 6)) {
                     fprintf(SUMA_STDERR,"Error %s: file %s contains %d values, not divisible by 6 (3 values per node).\n" , 
                        FuncName, s, ntot);
                     SUMA_RETURNe;
                  }

                  ntot = ntot / 6;
                  /* allocate for segments DO */
                  SDO = SUMA_Alloc_SegmentDO (ntot, s);
                  if (!SDO) {
                     fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Allocate_SegmentDO.\n", FuncName);
                     SUMA_RETURNe;
                  }
                  
                  fprintf(SUMA_STDERR,"%s: Reading %d nodes from file %s...\n", FuncName, 2*SDO->N_n, s);
                  /* fill up SDO */
                  tmp = fopen(s, "r");
                  itmp = 0;
                  while (itmp < 3 * SDO->N_n) {   
                     itmp2 = itmp;
                     fscanf (tmp,"%f",&(SDO->n0[itmp])); ++itmp;
                     fscanf (tmp,"%f",&(SDO->n0[itmp])); ++itmp;
                     fscanf (tmp,"%f",&(SDO->n0[itmp])); ++itmp;
                     fscanf (tmp,"%f",&(SDO->n1[itmp2])); ++itmp2;
                     fscanf (tmp,"%f",&(SDO->n1[itmp2])); ++itmp2;
                     fscanf (tmp,"%f",&(SDO->n1[itmp2])); ++itmp2;
                  }
                  for (itmp=0; itmp < 6* SDO->N_n; ++itmp) {
                     
                  }
                  fclose (tmp);   

                  /* addDO */
                  if (!SUMA_AddDO(SUMAg_DOv, &SUMAg_N_DOv, (void *)SDO, LS_type, SUMA_LOCAL)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
                     SUMA_RETURNe;
                  }

                  /* register DO with viewer */
                  if (!SUMA_RegisterDO(SUMAg_N_DOv-1, sv)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterDO.\n", FuncName);
                     SUMA_RETURNe;
                  }
                  
                  /* redisplay curent only*/
                  sv->ResetGLStateVariables = YUP;
                  SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
               
   
            } else if (Kev.state & Mod1Mask){
               /* swap buttons 1 and 3 */
               SUMAg_CF->SwapButtons_1_3 = !SUMAg_CF->SwapButtons_1_3;
               if (SUMAg_CF->SwapButtons_1_3) {
                  fprintf (SUMA_STDOUT,"%s: Buttons 1 and 3 are swapped.\n", FuncName);
               } else {
                  fprintf (SUMA_STDOUT,"%s: Default functions for buttons 1 and 3.\n", FuncName);
               }               
            } else if (SUMAg_CF->Dev) {
               for (ii=0; ii< sv->N_DO; ++ii) {
                  if (SUMA_isSO(SUMAg_DOv[sv->ShowDO[ii]])) 
                     SUMA_Print_Surface_Object((SUMA_SurfaceObject*)SUMAg_DOv[sv->ShowDO[ii]].OP, stdout);
               }
            }
            break;

         case XK_t:
            if ((Kev.state & ControlMask) && SUMAg_CF->Dev){
                  fprintf(SUMA_STDOUT, "%s: Forcing a resend of Surfaces to Afni...\n", FuncName);
                  sprintf(CommString,"SetForceAfniSurf~");
                  if (!SUMA_Engine (CommString, &EngineData, sv)) {
                     fprintf(SUMA_STDERR, "Error SUMA_input: SUMA_Engine call failed.\n");
                  }
            } else {
               sprintf(CommString,"ToggleConnected~");         
               if (!SUMA_Engine (CommString, &EngineData, sv)) {
                  fprintf(SUMA_STDERR, "Error SUMA_input: SUMA_Engine call failed.\n");
               }
            }
            break;

         case XK_v:
            if (SUMAg_CF->Dev) {
               SUMA_Show_SurfaceViewer_Struct (sv, stdout);
            }
            break;

         case XK_W:
            if (SUMAg_CF->Dev) {
               FILE *Fout;
               SUMA_SurfaceObject *SO;
               char stmploc[SUMA_MAX_LABEL_LENGTH+50];
               
               SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
               ND = SO->NodeDim;
               NP = SO->FaceSetDim;

               sprintf (stmploc, "%s_NodeList.txt", SO->Label); 
               if (LocalHead) fprintf (SUMA_STDERR,"%s: Preparing to write %s.\n", FuncName, stmploc); 
               Fout = fopen(stmploc, "w");
               if (Fout == NULL) {
                  fprintf(SUMA_STDERR, "Error %s: Could not open file %s for writing.\n", FuncName, stmploc);
                  break;
               }
               
               for (ii=0; ii < SO->N_Node; ++ii) {
                  id = ND * ii;
                  fprintf(Fout, "%f\t%f\t%f\n", \
                     SO->NodeList[id], SO->NodeList[id+1],SO->NodeList[id+2]);
               }
               fclose (Fout);

               sprintf (stmploc, "%s_FaceSetList.txt", SO->Label);
               if (LocalHead) fprintf (SUMA_STDERR,"%s: Preparing to write %s.\n", FuncName, stmploc); 
               Fout = fopen(stmploc, "w");
               if (Fout == NULL) {
                  fprintf(SUMA_STDERR, "Error %s: Could not open file %s for writing.\n", FuncName, stmploc);
                  break;
               }
               for (ii=0; ii < SO->N_FaceSet; ++ii) {
                  ip = NP * ii;
                  fprintf(Fout, "%d\t%d\t%d\n", \
                     SO->FaceSetList[ip], SO->FaceSetList[ip+1],SO->FaceSetList[ip+2]);
               }
               fclose (Fout);

               sprintf (stmploc, "%s_NodeColList.txt", SO->Label);
               if (LocalHead) fprintf (SUMA_STDERR,"%s: Preparing to write %s.\n", FuncName, stmploc); 
               Fout = fopen(stmploc, "w");
               if (Fout == NULL) {
                  fprintf(SUMA_STDERR, "Error %s: Could not open file %s for writing.\n", FuncName, stmploc);
                  break;
               }
                glar_ColorList = SUMA_GetColorList (sv, SO->idcode_str);
                if (!glar_ColorList) {
                  fprintf(SUMA_STDERR, "Error %s: NULL glar_ColorList. BAD.\n", FuncName);
                  break;
                }
               for (ii=0; ii < SO->N_Node; ++ii) {
                  ip = 4 * ii;
                  fprintf(Fout, "%d\t%f\t%f\t%f\n", \
                     ii, glar_ColorList[ip], glar_ColorList[ip+1], glar_ColorList[ip+2]);
               }
               fclose (Fout);


               fprintf(SUMA_STDERR, "%s: Wrote %s_NodeList.txt, %s_FaceSetList.txt & %s_NodeColList.txt to disk.\n", FuncName, SO->Label, SO->Label, SO->Label);
            }
            break;

         case XK_w:
            fprintf(SUMA_STDOUT,"%s: Began rendering to file. Please wait ...\n", FuncName);
            if (!SUMA_RenderToPixMap (sv, SUMAg_DOv)) {
               fprintf(SUMA_STDERR, "Error %s: Failed to write image.\n", FuncName);
            } 
            break;

         case XK_Z:
            /*fprintf(stdout,"Zoom in");*/
            sv->FOV[sv->iState] /= FOV_IN_FACT; if (sv->FOV[sv->iState] < FOV_MIN) sv->FOV[sv->iState] = FOV_MIN; 
            SUMA_postRedisplay(w, clientData, callData);
            break;

         case XK_z:
            /*fprintf(stdout,"Zoom out");*/
            sv->FOV[sv->iState] /= FOV_OUT_FACT; if (sv->FOV[sv->iState] > FOV_MAX) sv->FOV[sv->iState] = FOV_MAX;
            SUMA_postRedisplay(w, clientData, callData);
            break;

         case XK_asterisk:
            fprintf(SUMA_STDOUT, "%s: smoothing node attributes ...\n", FuncName);
            {
               SUMA_SurfaceObject *SO;
               float * attr_sm;
               float *attrbuf;
               int ic, cnt;
               int allcols;
               
               SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
               attrbuf = (float *)SUMA_calloc(SO->N_Node, sizeof(int));
               if (attrbuf == NULL) {
                  fprintf(stderr,"Error SUMA_input: Failed to allocate for attrbuf.\n");
                  break;
               }

               allcols = 4 * SO->N_Node;
               /* the colors are stored in glar_ColorList, RGBA */
               glar_ColorList = SUMA_GetColorList (sv, SO->idcode_str);
               if (!glar_ColorList) {
                  fprintf(SUMA_STDERR, "Error %s: Null glar_ColorList.\n", FuncName);
                  break;
               }
               for (ic=0; ic < 3; ++ic) { /* ic */
                  ii = ic;
                  cnt = 0;
                  while (ii < allcols) {
                     attrbuf[cnt] = glar_ColorList[ii];
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
                     glar_ColorList[ii] = attr_sm[cnt];
                     ii += 4;
                     cnt += 1;
                  } 
               } /* ic */   

               SUMA_free(attr_sm);
               SUMA_free(attrbuf);
               /*fprintf(SUMA_STDOUT, "%s: Smoothing Done ...\n", FuncName);*/
               SUMA_postRedisplay(w, clientData, callData);
            }

            break;

          case XK_at:
            if (SUMAg_CF->Dev) {
               /* calculate the curvature */
               fprintf(SUMA_STDOUT, "%s: Calculating surface curvature ...\n", FuncName);
               {
                  SUMA_SurfaceObject *SO;
                  SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
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
               SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;   
               if (SO->Cx) {
                  fprintf(stderr,"Error %s: SO->Cx must be null prior to new assignment\n", FuncName);
                  break;
               }
               SO->Cx = SUMA_Convexity   (SO->NodeList, SO->N_Node, SO->NodeNormList, SO->FN);   
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
               if (attr_sm) SUMA_free(attr_sm);

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
                  glar_ColorList = SUMA_GetColorList (sv, SO->idcode_str);
                  if (!glar_ColorList) {
                     fprintf (SUMA_STDERR,"Error %s: NULL glar_ColorList. BAD.\n", FuncName);
                     break;
                  }  
                  SUMA_RGBmat_2_GLCOLAR4(SV->cM, glar_ColorList, SO->N_Node);

                  /* free */
                  if (Vsort) SUMA_free(Vsort);
                  if (CM) SUMA_Free_ColorMap (CM);
                   if (OptScl) SUMA_free(OptScl);
                  if (SV) SUMA_Free_ColorScaledVect (SV);
                  if (SO->Cx) {
                     SUMA_free(SO->Cx);
                     SO->Cx = NULL;
                  }

               fprintf(SUMA_STDOUT, "%s: Convexity mapping done ...\n", FuncName);
               SUMA_postRedisplay(w, clientData, callData);   
            }
            break;
         case XK_comma:
            {
               /* switch state, back one */
               int nxtstateID = -1;

               if (sv->N_VSv < 2) break;

               /*fprintf(SUMA_STDERR,"%s: Current viewing state is %s ...\n", FuncName, sv->State);*/
               /* toggle to the next view state */
               nxtstateID = SUMA_PrevState(sv);
               if (nxtstateID < 0) {
                  fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_PrevState.\n", FuncName);
                  break;
               }
               fprintf(SUMA_STDERR,"%s: Switching from %s to %s viewing state.\n", \
                  FuncName, sv->State, sv->VSv[nxtstateID].Name);

               if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, sv, nxtstateID)) {
                  fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SwitchState.\n", FuncName);
                  break;
               }

               /* register a call to redisplay (you also need to copy the color data, in case the next surface is of the same family*/
               sprintf(CommString,"Redisplay~");
               if (!SUMA_Engine (CommString, &EngineData, sv)) {
                  fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
               }
            }
            break;

         case XK_period:
            {
               /* switch state, forward one */
               int nxtstateID=-1;

               if (sv->N_VSv < 2) break;

               /*fprintf(SUMA_STDERR,"%s: Current viewing state is %s ...\n", FuncName, sv->State);*/
               /* toggle to the next view state */
               nxtstateID = SUMA_NextState(sv);
               if (nxtstateID < 0) {
                  fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_NextState.\n", FuncName);
                  break;
               }
               fprintf(SUMA_STDERR,"%s: Switching from %s to %s viewing state.\n", FuncName, sv->State, sv->VSv[nxtstateID].Name);

               if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, sv, nxtstateID)) {
                  fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SwitchState.\n", FuncName);
                  break;
               }

               /* register a call to redisplay (you also need to copy the color data, in case the next surface is of the same family*/
               sprintf(CommString,"Redisplay~");
               if (!SUMA_Engine (CommString, &EngineData, sv)) {
                  fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
               }

               break;
            }
            break;

         case XK_F1: /* F1 */
            /*printf("F1\n");*/
            sv->ShowEyeAxis = !sv->ShowEyeAxis;
            SUMA_postRedisplay(w, clientData, callData);
            break;

         case XK_F2:
            /*printf("F2\n");*/
            {
               int *do_id, n_do_id;
               /*SUMA_SurfaceObject *ptr_tmp;*/
               sv->ShowMeshAxis = !sv->ShowMeshAxis;
               do_id = SUMA_GetDO_Type(SUMAg_DOv, SUMAg_N_DOv, SO_type, &n_do_id);
               if (n_do_id) {
                  while (n_do_id) {
                     /*ptr_tmp = (SUMA_SurfaceObject *)SUMAg_DOv[do_id[n_do_id-1]].OP;
                     ptr_tmp->ShowMeshAxis = sv->ShowMeshAxis;*/
                     ((SUMA_SurfaceObject *)SUMAg_DOv[do_id[n_do_id-1]].OP)->ShowMeshAxis = sv->ShowMeshAxis;
                     --n_do_id;
                  }
                  SUMA_free(do_id);
               }
            }
            SUMA_postRedisplay(w, clientData, callData);
            break;

         case XK_F3: /* F3 */
            sprintf(CommString, "Redisplay|ToggleCrossHair~");
            if (!SUMA_Engine (CommString, &EngineData, sv)) {
               fprintf(stderr,"Error SUMA_input: Failed SUMA_Engine\n");
            } 
            break;

         case XK_F4: /* F4 */
            sprintf(CommString, "Redisplay|ToggleShowSelectedNode~");
            if (!SUMA_Engine (CommString, &EngineData, sv)) {
               fprintf(stderr,"Error SUMA_input: Failed SUMA_Engine\n");
            } 
            break;

         case XK_F5: /* F5 */
            sprintf(CommString, "Redisplay|ToggleShowSelectedFaceSet~");
            if (!SUMA_Engine (CommString, &EngineData, sv)) {
               fprintf(stderr,"Error SUMA_input: Failed SUMA_Engine\n");
            } 
            break;

         case XK_F6: /*F6 */
            if (sv->clear_color[0] == 0.0) { /* flip background from black to white */
               sv->clear_color[0] = sv->clear_color[1] = sv->clear_color[2] = sv->clear_color[3] = 1.0;
            } else { /* goto black */
               sv->clear_color[0] = sv->clear_color[1] = sv->clear_color[2] = sv->clear_color[3] = 0.0;
            }
            sprintf(CommString, "Redisplay~");
            if (!SUMA_Engine (CommString, &EngineData, sv)) {
               fprintf(stderr,"Error SUMA_input: Failed SUMA_Engine\n");
            }
            break; 
            
         case XK_F12: /* F12 */
            /* time display speed */
            {
               int i, nd = 20;
               GLfloat buf; 
               float delta_t;
               struct  timeval tti;
               
               buf = sv->light0_position[2];
               fprintf (SUMA_STDOUT,"%s: Timing Display speed (20 displays): ", FuncName); fflush (SUMA_STDOUT);
               SUMA_etime (&tti, 0);
               for (i=0; i< nd-1; ++i) {
                  fprintf (SUMA_STDOUT,"%d\t", i); fflush (SUMA_STDOUT);
                  sv->light0_position[2] *= -1;
                  glLightfv(GL_LIGHT0, GL_POSITION, sv->light0_position);
                  /* direct call to display */
                  SUMA_display(sv, SUMAg_DOv);
                  /* wait for display */
                  glFinish();
               }
               delta_t = SUMA_etime (&tti, 1);
               sv->light0_position[2] = buf;
               glLightfv(GL_LIGHT0, GL_POSITION, sv->light0_position);
               SUMA_postRedisplay(w, clientData, callData);
               fprintf (SUMA_STDOUT,"Done.\nElapsed time: %f seconds. %.2f displays/second.\n", delta_t, nd/delta_t);
            } 
            break;
         
         case XK_Home:   
            /*printf("HOME\n");*/
            sprintf(CommString, "Redisplay|FOVreset|Home~");
            if (!SUMA_Engine (CommString, &EngineData, sv)) {
               fprintf(stderr,"Error SUMA_input: Failed SUMA_Engine\n");
            } 
            break;

         case XK_Left:   /*KEY_LEFT:*/
            /*fprintf(stdout,"Left Key\n");*/
            if ((Kev.state & ControlMask) && (Kev.state & ShiftMask)) {
               /* do nothing about ctrl+shift+this key */
            }else if (Kev.state & ShiftMask) {
               /*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
               sv->GVS[sv->StdView].translateVec[0] -= (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaX/(float)sv->WindWidth*sv->GVS[sv->StdView].TranslateGain;
               /*sv->GVS[sv->StdView].translateVec[1] -= 0;*/
               SUMA_postRedisplay(w, clientData, callData);
            }else if (Kev.state & ControlMask){
               float a[3], cQ[4], dQ[4];
               /* From top view, rotate about x 90 degrees.*/ 
               a[0] = 1.0; a[1] = 0.0;
               axis_to_quat(a, SUMA_PI/2.0, cQ);
               /* then rotate about y 90 degrees */
               a[0] = 0.0; a[1] = 1.0; a[2] = 0.0;
               axis_to_quat(a, SUMA_PI/2.0, dQ);
               /*add and apply rotation*/
               add_quats (dQ, cQ, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, clientData, callData);
            }else if (Kev.state & Mod1Mask) {
               /*ffprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
            }else {
               /*ffprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
               trackball(sv->GVS[sv->StdView].deltaQuat, 
                  ArrowDeltaRot, 0.0, /* first point */
                  -ArrowDeltaRot, 0.0); /* ending x,y */
               add_quats (sv->GVS[sv->StdView].deltaQuat, sv->GVS[sv->StdView].currentQuat, sv->GVS[sv->StdView].currentQuat);
               sv->GVS[sv->StdView].spinDeltaX = -2*ArrowDeltaRot*sv->WindWidth;
               sv->GVS[sv->StdView].spinDeltaY = 0;
               SUMA_postRedisplay(w, clientData, callData);
            }
               
            break;

         case XK_Right:   /*KEY_RIGHT: */
            /*printf("Right Key\n");*/
            if ((Kev.state & ControlMask) && (Kev.state & ShiftMask)) {
               /* do nothing about ctrl+shift+this key */
            }else if (Kev.state & ShiftMask) {
               /*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
               sv->GVS[sv->StdView].translateVec[0] += (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaX/(float)sv->WindWidth*sv->GVS[sv->StdView].TranslateGain;
               /*sv->GVS[sv->StdView].translateVec[1] -= 0;*/
               SUMA_postRedisplay(w, clientData, callData);
            }else if (Kev.state & ControlMask){
               float a[3], cQ[4], dQ[4];
               /* From top view, rotate about x 90 degrees */ 
               a[0] = 1.0; a[1] = 0.0; a[2] = 0.0;
               axis_to_quat(a, SUMA_PI/2.0, cQ);
               /* then rotate about y -90 degrees */
               a[0] = 0.0; a[1] = 1.0;
               axis_to_quat(a, -SUMA_PI/2.0, dQ);
               /*add and apply rotation*/
               add_quats (dQ, cQ, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, clientData, callData);
               
            }else if (Kev.state & Mod1Mask) {
               /*fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
            }else {
               /*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
               trackball(sv->GVS[sv->StdView].deltaQuat, 
                  -ArrowDeltaRot, 0.0, /* first point */
                  ArrowDeltaRot, 0.0); /* ending x,y */
               add_quats (sv->GVS[sv->StdView].deltaQuat, sv->GVS[sv->StdView].currentQuat, sv->GVS[sv->StdView].currentQuat);
               sv->GVS[sv->StdView].spinDeltaX = 2*ArrowDeltaRot*sv->WindWidth;
               sv->GVS[sv->StdView].spinDeltaY = 0;
               SUMA_postRedisplay(w, clientData, callData);
            }
            break;

         case XK_Down:   /*KEY_DOWN*/
            /*printf("Down Key\n");*/
            if ((Kev.state & ControlMask) && (Kev.state & ShiftMask)) {
               float a[3], cQ[4], dQ[4];
               /* Posterior view ctrl+shift+down*/
               /* From top view, first rotate by 90 degrees about x axis */
               a[0] = 1.0; a[1] = 0.0; a[2] = 0.0;
               axis_to_quat(a, SUMA_PI/2, cQ);
               /* then rotate by 180 degrees about y axis */
               a[0] = 0.0; a[1] = 1.0; a[2] = 0.0;
               axis_to_quat(a, SUMA_PI, dQ);
               /*add rotation */
               add_quats (dQ, cQ, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, clientData, callData);
            }else if (Kev.state & ShiftMask) {
               /*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
               /*sv->GVS[sv->StdView].translateVec[0] += 0;*/
               sv->GVS[sv->StdView].translateVec[1] -=  (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaY/(float)sv->WindHeight*sv->GVS[sv->StdView].TranslateGain;
               SUMA_postRedisplay(w, clientData, callData);
            }else if (Kev.state & ControlMask){
               /*fprintf (SUMA_STDERR,"%s: Control down\n", FuncName);*/
               /* Inferior view ctrl+down*/
               float a[3];
               /* From top view, rotate by 180 degrees about y axis */
               a[0] = 0.0; a[1] = 1.0; a[2] = 0.0;
               axis_to_quat(a, SUMA_PI, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, clientData, callData);
            }else if (Kev.state & Mod1Mask) {
               /*fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
            }else {
               /*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
               trackball(sv->GVS[sv->StdView].deltaQuat, 
                  0.0, ArrowDeltaRot, /* first point */
                  0.0, -ArrowDeltaRot); /* ending x,y */
               /*fprintf(stdout,"\ncurrentQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", sv->GVS[sv->StdView].currentQuat[i]);} fprintf(stdout,"\n");
               fprintf(stdout,"\ndeltaQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", sv->GVS[sv->StdView].deltaQuat[i]);} fprintf(stdout,"\n");*/
               add_quats (sv->GVS[sv->StdView].deltaQuat, sv->GVS[sv->StdView].currentQuat, sv->GVS[sv->StdView].currentQuat);
               /*fprintf(stdout,"\nnewQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", sv->GVS[sv->StdView].currentQuat[i]);} fprintf(stdout,"\n");*/
               sv->GVS[sv->StdView].spinDeltaX = 0;
               sv->GVS[sv->StdView].spinDeltaY = -2*ArrowDeltaRot*sv->WindHeight;
               SUMA_postRedisplay(w, clientData, callData);
            }
            
            break;

         case XK_Up: /*KEY_UP*/
            /*printf("Up Key\n");*/
            if ((Kev.state & ControlMask) && (Kev.state & ShiftMask)) {
               float a[3];
               /* Posterior view ctrl+shift+up*/
               /* From top view, rotate by 90 degrees about x axis */
               a[0] = 1.0; a[1] = 0.0; a[2] = 0.0;
               axis_to_quat(a, SUMA_PI/2, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, clientData, callData);
            }else if (Kev.state & ShiftMask) {
               /*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
               #ifdef USELESS_BLOCK
                  /* This shows how to have SUMA_momentum work for arrow translation. But that is largely useless
                  because the object  quickly disappears from view */
                  sv->GVS[sv->StdView].translateDeltaX = 0/(float)sv->WindWidth*sv->GVS[sv->StdView].TranslateGain;
                  sv->GVS[sv->StdView].translateDeltaY = sv->GVS[sv->StdView].ArrowtranslateDeltaY/(float)sv->WindHeight*sv->GVS[sv->StdView].TranslateGain;
                  sv->GVS[sv->StdView].translateVec[0] += (GLfloat)sv->GVS[sv->StdView].translateDeltaX;
                  sv->GVS[sv->StdView].translateVec[1] += (GLfloat)sv->GVS[sv->StdView].translateDeltaY;
                  sv->GVS[sv->StdView].translateDeltaX = 0; /* if you do not turn these back to 0 then the surface will quickly go out of sight if SUMA_momentum is turned on */
                  sv->GVS[sv->StdView].translateDeltaY = 0;
               #endif
               /*sv->GVS[sv->StdView].translateVec[0] += 0;*/
               sv->GVS[sv->StdView].translateVec[1] +=  (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaY/(float)sv->WindHeight*sv->GVS[sv->StdView].TranslateGain;
               SUMA_postRedisplay(w, clientData, callData);
            }else if (Kev.state & ControlMask){
               /*fprintf (SUMA_STDERR,"%s: Control down\n", FuncName);*/
               /* Top view ctrl+up*/
               float a[3];
               /* Default top view, rotate by nothing */
               a[0] = 1.0; a[1] = 0.0; a[2] = 0.0;
               axis_to_quat(a, 0, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, clientData, callData);
            }else if (Kev.state & Mod1Mask) {
               /*fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
            }else {
               if (LocalHead) fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);
               trackball(sv->GVS[sv->StdView].deltaQuat, 
                  0.0, -ArrowDeltaRot, /* first point */
                  0.0, ArrowDeltaRot); /* ending x,y */
               if (LocalHead) {
                  fprintf(stdout,"\ncurrentQuat\n");
                  for (ii=0; ii<4; ++ii) { 
                     fprintf(stdout,"%f\t", sv->GVS[sv->StdView].currentQuat[ii]);
                  } 
                  fprintf(stdout,"\n");
                  fprintf(stdout,"\ndeltaQuat\n");
                  for (ii=0; ii<4; ++ii) { 
                     fprintf(stdout,"%f\t", sv->GVS[sv->StdView].deltaQuat[ii]);
                  } 
                  fprintf(stdout,"\n");
               }
               add_quats (sv->GVS[sv->StdView].deltaQuat, sv->GVS[sv->StdView].currentQuat, sv->GVS[sv->StdView].currentQuat);
               if (LocalHead) {
                  fprintf(stdout,"\nnewQuat\n");
                  for (ii=0; ii<4; ++ii) { 
                     fprintf(stdout,"%f\t", sv->GVS[sv->StdView].currentQuat[ii]);
                  } 
                  fprintf(stdout,"\n");
               }
               sv->GVS[sv->StdView].spinDeltaX = 0;
               sv->GVS[sv->StdView].spinDeltaY = 2*ArrowDeltaRot*sv->WindHeight;
               SUMA_postRedisplay(w, clientData, callData);
                  
            }
            
            break;

         default:
            break;

      } /* keysym */
   break;
   
   case ButtonPress:
      if (LocalHead) fprintf(stdout,"In ButtonPress\n");      
      pButton = Bev.button;
      if (SUMAg_CF->SwapButtons_1_3) {
         if (pButton == Button1) pButton = Button3;
         else if (pButton == Button3) pButton = Button1;
      }
     
     /* trap for double click */
      if (Bev.time - B1time < SUMA_DOUBLE_CLICK_MAX_DELAY) {
         if (LocalHead) fprintf(SUMA_STDERR, "%s: Double click.\n", FuncName);
         DoubleClick = YUP;
      } else {
         DoubleClick = NOPE;
      }
      B1time = Bev.time; 
            
      switch (pButton) { /* switch type of button Press */
         case Button1:
            if (Bev.state & Button2Mask) {
               /* setup initial zooming conditions */
               /*fprintf(SUMA_STDERR,"%s: Button 1 &2 down. New\n", FuncName); */
               sv->GVS[sv->StdView].zoomBegin = (float)Bev.y;
               sv->GVS[sv->StdView].zoomDelta = 0;   
            }else {
               /*fprintf(SUMA_STDERR,"%s: Button 1 down. New\n", FuncName);*/
               /* setup initial spinning conditions */
               sv->GVS[sv->StdView].spinBeginX = (int)Bev.x;
               sv->GVS[sv->StdView].spinBeginY = (int)Bev.y;
               sv->GVS[sv->StdView].spinDeltaX = 0;
               sv->GVS[sv->StdView].spinDeltaY = 0;   
            }
            break;
            
         case Button2:
            if (Bev.state & ShiftMask) {
               /* setup initial zooming conditions */
               /*fprintf(SUMA_STDERR,"%s: Button 2 & Shift\n", FuncName); */
               sv->GVS[sv->StdView].zoomBegin = (float)Bev.y;
               sv->GVS[sv->StdView].zoomDelta = 0;   
            } else {   
               /*fprintf(stdout,"Button 2 down, plain jane\n");*/
               /* setup initial translation conditions */
               sv->GVS[sv->StdView].translateBeginX = (int)Bev.x;
               sv->GVS[sv->StdView].translateBeginY = (int)Bev.y;
               sv->GVS[sv->StdView].translateDeltaX = 0;
               sv->GVS[sv->StdView].translateDeltaY = 0;
            }
            break;
            
         case Button3:
               if (LocalHead) fprintf(SUMA_STDERR,"%s: Button 3 downplain jane, viewer #%d : X=%f, Y = %f\n", \
                  FuncName, SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv), (float)Bev.x, (float)Bev.y);
               
               if (Bev.state & ShiftMask) {
                  /* ROI drawing mode */
                  ROI_mode = YUP;     
               }else {
                  ROI_mode = NOPE;
               }
               
               if (!DoubleClick) {
               /* you do not want to waist time doing double calculations if the user clicks twice by mistake */
                  /* make sure no viewer, other than the one clicked in is in momentum mode */
                  if (SUMAg_N_SVv > 1) {
                     for (ii=0; ii < SUMAg_N_SVv; ++ii) {
                        if (&(SUMAg_SVv[ii]) != sv) {
                           if (SUMAg_SVv[ii].GVS[SUMAg_SVv[ii].StdView].ApplyMomentum) {
                              fprintf (SUMA_STDERR,"Error %s: You cannot select while other viewers (like #%d) are in momentum mode.\n", FuncName, ii);
                              SUMA_RETURNe;
                           }
                        }
                     }
                  }  

                  /* make sure all OpenGL commands are completed before proceeding 
                  This is an attempt at reducing the picking problem when multiple
                  viewers are open simultaneously. This may be redundant since glFinish 
                  is also called in SUMA_handleRedisplay....*/
                  if (0 && SUMAg_N_SVv > 1) {
                     glFinish();
                  }

                  ii = SUMA_ShownSOs(sv, SUMAg_DOv, NULL);
                  if (ii == 0) { /* no surfaces, break */
                     break;
                  }


                  if (!SUMA_GetSelectionLine (sv, (int)Bev.x, (int)Bev.y)) {
                     fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_GetSelectionLine.\n", FuncName);
                     break;
                  } 


                  /* perform the intersection calcluation and mark the surface */
                  hit = SUMA_MarkLineSurfaceIntersect (sv, SUMAg_DOv);
                  if (hit < 0) {
                     fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_MarkLineSurfaceIntersect.\n", FuncName);
                     break;
                  }else if (hit == 0) { /* nothing hit, get out */
                     break;
                  }
               }
               
               if (ROI_mode && sv->Focus_SO_ID >= 0) {
                  SUMA_DRAWN_ROI *DrawnROI = NULL;
                  SUMA_SurfaceObject *SO = NULL;
                  SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
                  /* Check to see if surface that was clicked on has any ROIs being drawn on it */
                  /* search in DOv for an open ROI that has a parent surface related to SO */
                  DrawnROI = SUMA_FetchROI_InCreation (SO, SUMAg_DOv, SUMAg_N_DOv); 
                  if (!DoubleClick) {
                     if (!DrawnROI) {
                        /* No such ROI found, create one */
                        fprintf (SUMA_STDERR, "%s: No ROI found, creating a new one.\n", FuncName);
                        
                        DrawnROI = SUMA_AllocateDrawnROI (SO->idcode_str, SUMA_ROI_InCreation, SUMA_ROI_ClosedPath, NULL);
                        if (!DrawnROI) {
                           fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AllocateDrawnROI.\n", FuncName);
                        }
                        /* Although ROIs are stored as DOs, they are dependent on the surfaces they are related to 
                        ROIs at this stage are node indices only (and perhaps the mesh) but the coordinates of the indices
                        come from the surface onto which they are displayed. So when you are drawing a surface, using CreateMesh,
                        you will search DOv for ROIs related to the surface displayed and overlay them accordingly */
                        /* Add the ROI to DO */
                        if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)DrawnROI, ROIdO_type, SUMA_LOCAL)) {
                           fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
                        }
                     } else {
                        fprintf(SUMA_STDOUT,"%s: ROI %p fetched. Status %d.\n", FuncName, DrawnROI, DrawnROI->DrawStatus); 
                     }
                     
                     /* Click is part of ROI loop, add it*/
                     if (DrawnROI->N_CtrlNodev) {
                        if (sv->Ch->NodeID == DrawnROI->CtrlNodev[DrawnROI->N_CtrlNodev - 1]) {
                           fprintf (SUMA_STDERR, "%s: Same node selected twice in a row, ignoring second selection.\n", FuncName);
                           break;
                        }
                     }
                     
                     if (LocalHead) fprintf (SUMA_STDERR, "%s: Node %d is collected as part of ROI %s.\nControl Node #%d\n", \
                        FuncName, sv->Ch->NodeID, DrawnROI->idcode_str, DrawnROI->N_CtrlNodev);
                     ii = 3 * DrawnROI->N_CtrlNodev;
                     DrawnROI->Pick0v[ii] = sv->Pick0[0];
                     DrawnROI->Pick0v[ii+1] = sv->Pick0[1];
                     DrawnROI->Pick0v[ii+2] = sv->Pick0[2];

                     DrawnROI->Pick1v[ii] = sv->Pick1[0];
                     DrawnROI->Pick1v[ii+1] = sv->Pick1[1];
                     DrawnROI->Pick1v[ii+2] = sv->Pick1[2];


                     DrawnROI->CtrlNodev[DrawnROI->N_CtrlNodev] = sv->Ch->NodeID;
                     DrawnROI->N_CtrlNodev = DrawnROI->N_CtrlNodev + 1;
                     if (DrawnROI->N_CtrlNodev >= SUMA_MAX_ROI_CTRL_NODES) {
                        fprintf (SUMA_STDERR, "Error %s: Exceeded maximum number of ROI control nodes allowed (%d). This can be fixed, complain to authors of the program.\n", FuncName, SUMA_MAX_ROI_CTRL_NODES);
                        DrawnROI->N_CtrlNodev = DrawnROI->N_CtrlNodev - 1;
                     }
                  } else {
                     /* Double click here */
                     if (DrawnROI) { /*   close ROI */
                        fprintf(SUMA_STDOUT,"%s: Now closing ROI %p. Status was %d \n", FuncName, DrawnROI, DrawnROI->DrawStatus);                        
                        DrawnROI->DrawStatus = SUMA_ROI_Finished;
                        fprintf(SUMA_STDOUT,"%s: Now closing ROI %p. Status is %d \n", FuncName, DrawnROI, DrawnROI->DrawStatus); 
                     } else {
                        /* tremors, nothing to do */
                        break;
                     }
                  }
                  if (DrawnROI->N_CtrlNodev > 1) {/* have a segment, need a plane */
                     float *Eq = NULL, NodeDist, NodeDist2, dx = 0.0, dy = 0.0, dz = 0.0, d_surf = 0.0;
                     SUMA_SURF_PLANE_INTERSECT *SPI = NULL;
                     SUMA_ROI *ROIe = NULL, *ROIt = NULL, *ROIn = NULL, *ROIts = NULL;
                     int N_left, *Path = NULL, N_Path, Nx, Ny, N_Bv = 0, iloc, *tPath = NULL, N_Tri, iloc3_0 = 0, iloc3_1= 0;
                     SUMA_TRI_BRANCH *Bv = NULL; 
                     
                     /* For node i (> 0) find the equation of the plane formed by
                     Pick0v[i], Pick1v[i], Pick1v[i-1] */

                     if (LocalHead) fprintf (SUMA_STDERR, "%s: Computing equation of cuting plane.\n", FuncName);
                     /* Before: use picking points instead of closest nodes, to avoid having a triangle intersected at one node only */
                     /* Now: using picking points is problematic once you rotate the surface between one click and the next. 
                     So it is best to use the two nearest nodes to the clicking points and for the third point, the near
                     clipping plane intersection */ 
                     if (DrawnROI->DrawStatus == SUMA_ROI_InCreation) {
                        /* set the from->to nodes for calcluating the shortest path */                           
                        Nx = DrawnROI->CtrlNodev[DrawnROI->N_CtrlNodev - 2];
                        Ny = DrawnROI->CtrlNodev[DrawnROI->N_CtrlNodev - 1];
                        ii = 3 * (DrawnROI->N_CtrlNodev - 1);
                        Eq = SUMA_Plane_Equation ( &(SO->NodeList[3*Nx]), 
                                                   &(DrawnROI->Pick0v[ii]),
                                                   &(SO->NodeList[3*Ny]) );
                     } else if (DrawnROI->DrawStatus == SUMA_ROI_Finished) {
                        /* set the from->to nodes for calcluating the shortest path */                           
                        Nx = DrawnROI->CtrlNodev[DrawnROI->N_CtrlNodev - 1];
                        Ny = DrawnROI->CtrlNodev[0];
                        ii = 3 * (DrawnROI->N_CtrlNodev - 1);;
                        Eq = SUMA_Plane_Equation ( &(SO->NodeList[3*Nx]), 
                                                   &(DrawnROI->Pick0v[ii]),
                                                   &(SO->NodeList[3*Ny]) );
                     } else {
                        fprintf(SUMA_STDOUT,"Error %s: Don't know what to do with this (%d) status.\n", FuncName, DrawnROI->DrawStatus);
                        break;
                     }
                     if (!Eq) {
                        fprintf(SUMA_STDOUT,"Error %s: Failed in SUMA_Plane_Equation.\n", FuncName);
                        break;
                     }

                     /* compute the intersection of the plane with the surface */
                     if (LocalHead) fprintf (SUMA_STDERR, "%s: Computing Intersection with Surface.\n", FuncName);
                     SPI = SUMA_Surf_Plane_Intersect (SO, Eq);
                     if (!SPI) {
                        fprintf(SUMA_STDOUT,"Error %s: Failed in SUMA_Surf_Plane_Intersect.\n", FuncName);
                        break;
                     }
                     #if 0
                     {
                        FILE * Outtemp;
                        fprintf (SUMA_STDERR, "%s: Writing EL structure to disk .\n", FuncName);
                        Outtemp = fopen ("SEL.txt", "w");
                        SUMA_Show_Edge_List (SO->EL, Outtemp);
                        fprintf (SUMA_STDERR, "%s: Writing SPI structure to disk .\n", FuncName);
                        SUMA_Show_SPI (SPI, Outtemp, SO);
                        fclose (Outtemp);
                     }
                     #endif
                     
                     /* calculate shortest path */
                     N_left = SPI->N_NodesInMesh;
                     Path = SUMA_Dijkstra (SO, Nx, Ny, SPI->isNodeInMesh, &N_left, 1, &NodeDist, &N_Path);
                     if (NodeDist < 0 || !Path) {
                        fprintf(SUMA_STDERR,"\aError %s: Failed in fast SUMA_Dijkstra.\n*** Two points are not connected by intersection. Repeat last selection.\n", FuncName);
                        /* remove last point from path */
                        DrawnROI->N_CtrlNodev = DrawnROI->N_CtrlNodev - 1;
                        
                        /* clean up */
                        if (SPI) SUMA_free_SPI (SPI); 
                        SPI = NULL;
                        if (Path) SUMA_free (Path);
                        break;   
                     }
                     fprintf (SUMA_STDERR, "%s: Shortest inter nodal distance along edges between nodes %d <--> %d (%d nodes) is %f.\n", 
                        FuncName, Nx, Ny, N_Path, NodeDist);
                     
                        #if 0 /* this uses the first method implemented for shortest path. Slow but works. keep for debugging. */
                           /* the old intersection structure had isNodeInMesh irreversibly modified, recompute intersection */
                           if (SPI) SUMA_free_SPI (SPI);
                           if (Path) SUMA_free (Path);
                           /* now repeat with method 1 */
                           SPI = SUMA_Surf_Plane_Intersect (SO, Eq);
                           N_left = SPI->N_NodesInMesh;
                           Nx = DrawnROI->CtrlNodev[DrawnROI->N_CtrlNodev - 2];
                           Ny = DrawnROI->CtrlNodev[DrawnROI->N_CtrlNodev - 1];
                           Path = SUMA_Dijkstra (SO, Nx, Ny, SPI->isNodeInMesh, &N_left, 0, &NodeDist, &N_Path);
                           if (NodeDist < 0) {
                              fprintf(SUMA_STDERR,"Error %s: SUMA_Dijkstra.\n", FuncName);
                              SUMA_RETURNe;
                           }
                           
                           fprintf (SUMA_STDERR, "%s: Shortest inter nodal distance along edges between nodes %d <--> %dis %f.\n", FuncName, Nx, Ny , NodeDist);
                        #endif
                        
                     #if 0
                        /* Show all intersected edges */
                        ROIe =  SUMA_AllocateROI (SO->idcode_str, SUMA_ROI_EdgeGroup, "SurfPlane Intersection - Edges", SPI->N_IntersEdges, SPI->IntersEdges);
                        if (!ROIe) {
                           fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AllocateROI.\n", FuncName);
                        }
                        if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIe, ROIO_type, SUMA_LOCAL)) {
                           fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
                        }

                     #endif
                     #if 0
                        /* Show all intersected triangles */
                        ROIt =  SUMA_AllocateROI (SO->idcode_str, SUMA_ROI_FaceGroup, "SurfPlane Intersection - Triangles", SPI->N_IntersTri, SPI->IntersTri);
                        if (!ROIt) {
                           fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AllocateROI.\n", FuncName);
                           break;
                        }
                        if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIt, ROIO_type, SUMA_LOCAL)) {
                           fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
                           break;
                        }
                     #endif
                     
                     /* calculate shortest path along the intersection of the plane with the surface */
                     
                     
                     #if 0
                     /* another way to determine intersection, using triangle strips instead of connected nodes */
                     
                     /* the intersection strips are arranged in branches and although you are likely to have Nx 
                     be a part of the first edge of the first Branch, that may not be the case all the time.
                     See SUMA_AssignTriBranch for more info. Also, while the list of triangles forming a branch is 
                     in the correct order, you cannot readily tell which traveling direction is shortest between two
                     nodes. Instead of these functions, you can use SUMA_IntersectionStrip to find the shortest triangle
                     strip between two nodes but these functions may prove useful some other time. */
                     if (LocalHead) fprintf(SUMA_STDERR,"%s: Calling SUMA_AssignTriBranch ...\n", FuncName); 

                     Bv = SUMA_AssignTriBranch (SO, SPI, Nx, &N_Bv, NOPE);
                     if (!Bv) {
                        fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AssignTriBranch.\n", FuncName);
                        break;
                     }
                     fprintf(SUMA_STDERR,"%s: %d branches.\n", FuncName, N_Bv);

                     
                     /* show me the first branch, it should contain Nx and Ny if the surface has no cuts in it*/
                     if (!SUMA_show_STB (&(Bv[0]), NULL)) {
                        fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_show_STB.\n", FuncName);
                        break;
                     }
                     #endif
                     
                     
                     #if 1
                     /* get the triangle path corresponding to shortest distance between Nx and Ny */
                     
                     /* Old Method: Does not result is a strip of triangle that is continuous or connected
                     by an intersected edge. Function is left here for historical reasons. 
                        tPath = SUMA_NodePath_to_TriPath_Inters (SO, SPI, Path, N_Path, &N_Tri); */
                        
                     /* you should not need to go much larger than NodeDist except when you are going for 
                     1 or 2 triangles away where discrete jumps in d might exceed the limit. 
                     Ideally, you want this measure to be 1.5 NodeDist or say, 15 mm, whichever is less.... */
                     tPath = SUMA_IntersectionStrip (SO, SPI, Path, N_Path, &d_surf, 2.5 *NodeDist, &N_Tri);                      if (!tPath) {
                        fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_IntersectionStrip.\n", FuncName);
                        break;
                     }
                     fprintf (SUMA_STDERR, "%s: Shortest inter nodal distance along surface between nodes %d <--> %d is %f.\n", 
                        FuncName, Nx, Ny, d_surf);

                     
                     /* Show intersected triangles, along shortest path */
                     ROIts =  SUMA_AllocateROI (SO->idcode_str, SUMA_ROI_FaceGroup, "SurfPlane Intersection - Triangles- Shortest", N_Tri, tPath);
                     if (!ROIts) {
                        fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AllocateROI.\n", FuncName);
                        break;
                     }
                     if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIts, ROIO_type, SUMA_LOCAL)) {
                        fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
                        break;
                     }
                     
                     #endif
                        
                     if (Path) {
                        #if 0
                           /* Show me the Path */
                           for (ii=0; ii < N_Path; ++ii) fprintf(SUMA_STDERR," %d\t", Path[ii]);
                        #endif

                        /* Show Path */
                        ROIn =  SUMA_AllocateROI (SO->idcode_str, SUMA_ROI_NodeGroup, "SurfPlane Intersection - Nodes", N_Path, Path);
                        if (!ROIn) {
                           fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AllocateROI.\n", FuncName);
                           break;
                       }
                        if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)ROIn, ROIO_type, SUMA_LOCAL)) {
                           fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
                           break;
                        }
                        SUMA_free (Path); 
                     }                        

                     if (tPath) SUMA_free(tPath);
                     
                     if (LocalHead) fprintf(SUMA_STDERR,"%s: Freeing Bv...\n", FuncName);
                     if (Bv) SUMA_free_STB (Bv, N_Bv);
                     
                     if (LocalHead) fprintf(SUMA_STDERR,"%s: Freeing Eq...\n", FuncName);
                     if (Eq) SUMA_free(Eq);
                     
                     if (LocalHead) fprintf(SUMA_STDERR,"%s: Freeing SPI...\n", FuncName);
                     if (SPI) SUMA_free_SPI (SPI);
                     if (LocalHead) fprintf(SUMA_STDERR,"%s:Done Freeing...\n", FuncName);
                  }
               }
               
               
               /* redisplay */
               sv->ResetGLStateVariables = YUP;
               SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
            break;
      } /* switch type of button Press */
      break;
      
   case ButtonRelease:
      if (LocalHead) fprintf(SUMA_STDERR,"%s: In ButtonRelease\n", FuncName); 
      rButton = Bev.button;
      if (SUMAg_CF->SwapButtons_1_3) {
         if (rButton == Button1) rButton = Button3;
         else if (rButton == Button3) rButton = Button1;
      }
      switch (rButton) { /* switch type of button Press */
         case Button3:
               if (LocalHead) fprintf(SUMA_STDERR,"%s: In ButtonRelease3\n", FuncName); 
         break;
      } /* switch type of button Press */
      break;
      
   case MotionNotify:
      if (LocalHead) fprintf(stdout,"In MotionNotify\n"); 
      if (SUMAg_CF->SwapButtons_1_3) {
        if (((Mev.state & Button3MotionMask) && (Mev.state & Button2MotionMask)) || ((Mev.state & Button2MotionMask) && (Mev.state & ShiftMask))) {
            mButton = SUMA_Button_12_Motion;
         } else if(Mev.state & Button3MotionMask) {
            mButton = SUMA_Button_1_Motion;
         }else if(Mev.state & Button2MotionMask) { 
            mButton = SUMA_Button_2_Motion;
         }else {
            break;
         } 
      } else {
         if (((Mev.state & Button1MotionMask) && (Mev.state & Button2MotionMask)) || ((Mev.state & Button2MotionMask) && (Mev.state & ShiftMask))) {
            mButton = SUMA_Button_12_Motion;
         } else if(Mev.state & Button1MotionMask) {
            mButton = SUMA_Button_1_Motion;
         }else if(Mev.state & Button2MotionMask) { 
            mButton = SUMA_Button_2_Motion;
         } else {
            break;
         }
      }
      
      switch (mButton) {
         case SUMA_Button_12_Motion:
         case SUMA_Button_2_Shift_Motion:
            /*fprintf(SUMA_STDERR,"%s: In motion, Butt1 & Butt2\n", FuncName);*/
            sv->GVS[sv->StdView].zoomDelta = 1.0 + (float)((int)Mev.y - sv->GVS[sv->StdView].zoomBegin)/MOUSE_ZOOM_FACT;
            if (sv->GVS[sv->StdView].zoomDelta > 2.0) sv->GVS[sv->StdView].zoomDelta = 2.0;
            else if (sv->GVS[sv->StdView].zoomDelta < 0.5) sv->GVS[sv->StdView].zoomDelta = 0.5;
            sv->FOV[sv->iState] /= sv->GVS[sv->StdView].zoomDelta;
            if (sv->FOV[sv->iState] < FOV_MIN) sv->FOV[sv->iState] = FOV_MIN;
            else if (sv->FOV[sv->iState] > FOV_MAX) sv->FOV[sv->iState] = FOV_MAX;
               sv->GVS[sv->StdView].zoomBegin = (float)(int)Mev.y;
               /*fprintf(stdout, "FOV zoom Delta = %f=n", sv->GVS[sv->StdView].zoomDelta);*/
            SUMA_postRedisplay(w, clientData, callData);    
            break;
            
         case SUMA_Button_1_Motion:     
            /*fprintf(SUMA_STDERR,"%s: In motion, Butt1 \n", FuncName); */
            /* spinning mode */
            sv->GVS[sv->StdView].spinDeltaX = ((int)Mev.x - sv->GVS[sv->StdView].spinBeginX);
            sv->GVS[sv->StdView].spinDeltaY = ((int)Mev.y - sv->GVS[sv->StdView].spinBeginY);
            /*fprintf(stdout,"\nspinBeginX %d spinBeginY %d\nspinDeltaX %d spinDeltaY %d\nWindWidth %d WindHeight %d\n", \
                        sv->GVS[sv->StdView].spinBeginX, sv->GVS[sv->StdView].spinBeginY, sv->GVS[sv->StdView].spinDeltaX, sv->GVS[sv->StdView].spinDeltaY, sv->WindWidth, sv->WindHeight);*/
            if (sv->GVS[sv->StdView].spinDeltaX || sv->GVS[sv->StdView].spinDeltaY){
               trackball(sv->GVS[sv->StdView].deltaQuat, 
                  (float)(2*sv->GVS[sv->StdView].spinBeginX - sv->WindWidth)/(float)sv->WindWidth, (float)(sv->WindHeight - 2*sv->GVS[sv->StdView].spinBeginY)/(float)sv->WindHeight,
                   (float)(2*(int)Mev.x - sv->WindWidth)/(float)sv->WindWidth, (float)(sv->WindHeight - 2*(int)Mev.y)/(float)sv->WindHeight); /* comput the increment Quat */
               sv->GVS[sv->StdView].spinBeginX = (int)Mev.x;
               sv->GVS[sv->StdView].spinBeginY = (int)Mev.y;
               add_quats (sv->GVS[sv->StdView].deltaQuat, sv->GVS[sv->StdView].currentQuat, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, clientData, callData);
            }
            break;
            
         case SUMA_Button_2_Motion:
            /* fprintf(SUMA_STDERR,"%s: In motion, Butt2 \n", FuncName);*/
            sv->GVS[sv->StdView].translateDeltaX = (float)((int)Mev.x - sv->GVS[sv->StdView].translateBeginX)/(float)sv->WindWidth*sv->GVS[sv->StdView].TranslateGain;
            sv->GVS[sv->StdView].translateDeltaY = -(float)((int)Mev.y - sv->GVS[sv->StdView].translateBeginY)/(float)sv->WindHeight*sv->GVS[sv->StdView].TranslateGain;
            if (sv->GVS[sv->StdView].translateDeltaX || sv->GVS[sv->StdView].translateDeltaY){
               sv->GVS[sv->StdView].translateVec[0] += (GLfloat)sv->GVS[sv->StdView].translateDeltaX;
               sv->GVS[sv->StdView].translateVec[1] += (GLfloat)sv->GVS[sv->StdView].translateDeltaY;
               sv->GVS[sv->StdView].translateBeginX = (int)Mev.x;
               sv->GVS[sv->StdView].translateBeginY = (int)Mev.y;
               SUMA_postRedisplay(w, clientData, callData);
            }  
            break; 
      }
      
      
      break;
  }/* switch event type */

   SUMA_RETURNe;
}

/*!
   SUMA_momentum(XtPointer clientData, XtIntervalId *id);
   
   client data contains the widget responsible for the call to SUMA_momentum
*/
void SUMA_momentum(XtPointer clientData, XtIntervalId *id)
{
   static char FuncName[]={"SUMA_momentum"};
   static int ReDisp;
   Widget w;
   int isv;
   SUMA_SurfaceViewer *sv;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* the widget is passed as client data */
   w = (Widget)clientData;
   
   /* find out which Surface viewer the widget belongs to */
   SUMA_ANY_WIDGET2SV((Widget)clientData, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_ANY_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }

   
   ReDisp = 0;
   if ( ((sv->GVS[sv->StdView].spinDeltaX*sv->GVS[sv->StdView].spinDeltaX) > sv->GVS[sv->StdView].MinIdleDelta ) || ((sv->GVS[sv->StdView].spinDeltaY*sv->GVS[sv->StdView].spinDeltaY) > sv->GVS[sv->StdView].MinIdleDelta ) ) 
      { /* rotate if SUMA_momentum is enabled and spinDeltaX or spinDeltaY are larger than the minimum set */ 
         /*fprintf(stdout,"SUMA_momentum:  spinDeltaX %d spinDeltaY %d\n",  sv->GVS[sv->StdView].spinDeltaX, sv->GVS[sv->StdView].spinDeltaY);*/
         add_quats (sv->GVS[sv->StdView].deltaQuat, sv->GVS[sv->StdView].currentQuat, sv->GVS[sv->StdView].currentQuat);
         ReDisp = 1;
      }
   if ( ((sv->GVS[sv->StdView].translateDeltaX*sv->GVS[sv->StdView].translateDeltaX) > sv->GVS[sv->StdView].MinIdleDelta ) || ((sv->GVS[sv->StdView].translateDeltaY*sv->GVS[sv->StdView].translateDeltaY) > sv->GVS[sv->StdView].MinIdleDelta ) )
      { /* translate */
         sv->GVS[sv->StdView].translateVec[0] += (GLfloat)sv->GVS[sv->StdView].translateDeltaX;
         sv->GVS[sv->StdView].translateVec[1] += (GLfloat)sv->GVS[sv->StdView].translateDeltaY;
         ReDisp = 1;
      }
   if (ReDisp) {
      /*fprintf(stdout,"Momentum Redisplay\n");*/
      SUMA_postRedisplay(w, NULL, NULL);
   }
   sv->X->MOMENTUMID = XtAppAddTimeOut(SUMAg_CF->App, 1, SUMA_momentum, (XtPointer) w);

  SUMA_RETURNe;         
}

 
/*!
   Determines the intersection between ]sv->Pick0 sv->Pick1[ and SO
   Highlights the intersected faceset, node and updates cross hair location 
   This used to be part of Button3's code in SUMA_input
   ans = SUMA_MarkLineSurfaceIntersect (sv, dov);
   \param sv (SUMA_SurfaceViewer *) surface viewer pointer
   \param dov (SUMA_DO *) displayable object vector pointer
   \ret ans (int)  -1 error, 0 no hit, hit 
   
   also requires SUMAg_DOv and SUMAg_N_DOv
*/
int SUMA_MarkLineSurfaceIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov)
{/* determine intersection */
   float P0f[3], P1f[3];
   static char FuncName[]={"SUMA_MarkLineSurfaceIntersect"};
   int NP; 
   SUMA_MT_INTERSECT_TRIANGLE *MTI = NULL, *MTIi = NULL;
   float delta_t_tmp, dmin; 
   struct timeval tt_tmp; 
   int ip, it, id, iv3[3], ii, N_SOlist, SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS], imin;
   char sfield[100], sdestination[100], CommString[SUMA_MAX_COMMAND_LENGTH];
   static char ssource[]={"suma"};
   SUMA_EngineData EngineData; /* Do not free EngineData, only its contents*/
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* initialize EngineData */
   if (!SUMA_InitializeEngineData (&EngineData)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to initialize EngineData\n", FuncName);
      SUMA_RETURN (-1);
   }

   P0f[0] = sv->Pick0[0];
   P0f[1] = sv->Pick0[1];
   P0f[2] = sv->Pick0[2];
   P1f[0] = sv->Pick1[0];
   P1f[1] = sv->Pick1[1];
   P1f[2] = sv->Pick1[2];
   
   N_SOlist = SUMA_ShownSOs(sv, dov, SOlist);
   imin = -1;
   dmin = 10000000.0;
   for (ii=0; ii < N_SOlist; ++ii) { /* find the closest intersection */
      if (LocalHead) fprintf (SUMA_STDERR, "%s: working %d/%d shown surfaces ...\n", FuncName, ii, N_SOlist);
      SO = (SUMA_SurfaceObject *)dov[SOlist[ii]].OP;
      if (SO->FaceSetDim != 3) {
         fprintf(SUMA_STDERR,"Error %s: SUMA_MT_intersect_triangle only works for triangular meshes.\n", FuncName);
      } else {

           SUMA_etime (&tt_tmp, 0);

         MTIi = SUMA_MT_intersect_triangle(P0f, P1f, SO->NodeList, SO->N_Node, SO->FaceSetList, SO->N_FaceSet);

         delta_t_tmp = SUMA_etime (&tt_tmp, 1);
         if (LocalHead) fprintf (SUMA_STDERR, "Local Debug %s: Intersection took %f seconds.\n", FuncName, delta_t_tmp);

         if (MTIi == NULL) {
            fprintf(SUMA_STDERR,"Error %s: SUMA_MT_intersect_triangle failed.\n", FuncName);
            SUMA_RETURN (-1);
         }
         
         if (MTIi->N_hits) { /* decide on the closest surface to the clicking point */
            if (MTIi->t[MTIi->ifacemin] < dmin) {
               if (LocalHead) fprintf (SUMA_STDERR, "%s: A minimum for surface %d.\n", FuncName, ii);
               dmin = MTIi->t[MTIi->ifacemin];
               imin = SOlist[ii];
               MTI = MTIi;
            }else {     
               /* not good, toss it away */
               if (LocalHead) fprintf (SUMA_STDERR, "%s: ii=%d freeing MTIi...\n", FuncName, ii);
               if (!SUMA_Free_MT_intersect_triangle(MTIi)) 
                 fprintf(SUMA_STDERR,"Error %s: SUMA_Free_MT_intersect_triangle failed.\n", FuncName);
               MTIi = NULL;
            }
         }else {
            /* not good, toss it away */
           if (LocalHead) fprintf (SUMA_STDERR, "%s: ii=%d freeing MTIi no hits...\n", FuncName, ii);
           if (!SUMA_Free_MT_intersect_triangle(MTIi)) 
               fprintf(SUMA_STDERR,"Error %s: SUMA_Free_MT_intersect_triangle failed.\n", FuncName);
           MTIi = NULL;
        }
      }
    } 

   if (LocalHead) fprintf (SUMA_STDERR, "%s: Closest surface is indexed %d in DOv.\n", FuncName, imin);
      
   /* Mark intersection Facsets */
   if (imin >= 0) {
      sv->Focus_SO_ID = imin;
      SO = (SUMA_SurfaceObject *)dov[imin].OP;
      NP = SO->FaceSetDim;
      ip = NP * MTI->ifacemin;
      /* print nodes about the closets faceset*/
      fprintf(SUMA_STDOUT, "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
      fprintf(SUMA_STDOUT, "Selected surface %s (Focus_SO_ID # %d). FaceSet %d, Closest Node %d\n", 
         SO->Label, sv->Focus_SO_ID, MTI->ifacemin, MTI->inodemin);
      fprintf(SUMA_STDOUT, "Nodes forming closest FaceSet:\n");
      fprintf(SUMA_STDOUT, "%d, %d, %d\n", \
      SO->FaceSetList[ip], SO->FaceSetList[ip+1],SO->FaceSetList[ip+2]);

      fprintf (SUMA_STDOUT,"Coordinates of Nodes forming closest FaceSet:\n");
      for (it=0; it < 3; ++it) { 

         id = SO->NodeDim * SO->FaceSetList[ip+it];
         fprintf(SUMA_STDOUT, "%f, %f, %f\n", SO->NodeList[id],\
                                                SO->NodeList[id+1],\
                                                SO->NodeList[id+2]);
      }
      fprintf(SUMA_STDOUT, "\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");

      /* check to see if AFNI needs to be notified */
      if (SUMAg_CF->Connected && sv->LinkAfniCrossHair) {
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Notifying Afni of CrossHair XYZ\n", FuncName);
         /* register a call to SetAfniCrossHair */
         sprintf(CommString,"SetAfniCrossHair~");
         if (!SUMA_Engine (CommString, &EngineData, sv)) {
            fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
         }
      }else {
         if (LocalHead) fprintf(SUMA_STDERR,"%s: No Notification to AFNI.\n", FuncName);
      }

      /* Set the Nodeselection at the closest node */
      it = MTI->inodemin;
      sprintf(sfield,"i");
      sprintf(sdestination,"SetSelectedNode");
      if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)(&it), sdestination, ssource, NOPE)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
         SUMA_RETURN (-1);
      }
      sprintf(CommString,"SetSelectedNode~");
      if (!SUMA_Engine (CommString, &EngineData, sv)) {
         fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
         SUMA_RETURN (-1);
      }


      /* Set the FaceSetselection */
      it = MTI->ifacemin;
      sprintf(sfield,"i");
      sprintf(sdestination,"SetSelectedFaceSet");
      if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)(&it), sdestination, ssource, NOPE)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
         SUMA_RETURN (-1);
      }
      sprintf(CommString,"SetSelectedFaceSet~");
      if (!SUMA_Engine (CommString, &EngineData, sv)) {
         fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
         SUMA_RETURN (-1);
      }
      /* Now set the cross hair position at the intersection*/
      sprintf(sfield,"fv3");
      sprintf(sdestination,"SetCrossHair");
      if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)MTI->P, sdestination, ssource,NOPE)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
         SUMA_RETURN (-1);
      }
      sprintf(CommString,"SetCrossHair~");
      if (!SUMA_Engine (CommString, &EngineData, sv)) {
         fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
         SUMA_RETURN (-1);
      }

      /* attach the cross hair to the selected surface */
      iv3[0] = SUMA_findSO_inDOv(SO->idcode_str, SUMAg_DOv, SUMAg_N_DOv);
      iv3[1] = MTI->inodemin;
      sprintf(sfield,"iv3");
      sprintf(sdestination,"BindCrossHair");
      if (!SUMA_RegisterEngineData (&EngineData, sfield, (void *)(iv3), sdestination, ssource, NOPE)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register %s to %s\n", FuncName, sfield, sdestination);
         SUMA_RETURN (-1);
      }
      sprintf(CommString,"LockCrossHair|BindCrossHair~"); 
      if (!SUMA_Engine (CommString, &EngineData, sv)) {
         fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
         SUMA_RETURN (-1);
      }
      
   } 
   /* clear MTI */
   if (MTI) {
      if (!SUMA_Free_MT_intersect_triangle(MTI)) 
         fprintf(SUMA_STDERR,"Error %s: SUMA_Free_MT_intersect_triangle failed.\n", FuncName);
   }
   
   if (imin >= 0) {
      SUMA_RETURN (1); /* hit */
   } else {
      SUMA_RETURN (0); /* no hit */
   }
}/* determine intersection */
