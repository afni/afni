#include "SUMA_suma.h"

/* extern SUMA_SurfaceViewer *SUMAg_cSV; */ /* no longer in use Tue Aug 13 15:55:29 EDT 2002 */
extern int SUMAg_N_DOv; 
extern SUMA_DO *SUMAg_DOv;
extern SUMA_CommonFields *SUMAg_CF; 
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv;



/*! Mouse and Keyboard input handler function for SUMA's viewer*/

void SUMA_input(Widget w, XtPointer clientData, XtPointer callData)
{
   GLwDrawingAreaCallbackStruct *cd;
   char buffer[10], cbuf = '\0', cbuf2='\0';
   KeySym keysym;
   int xls, ntot, id = 0, ND, ip, NP;
   float ArrowDeltaRot = 0.05; /* The larger the value, the bigger the rotation increment */
   SUMA_EngineData *ED = NULL; 
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
   SUMA_SurfaceViewer *sv, *svi = NULL;
   GLfloat *glar_ColorList = NULL;
   static Time B1time = 0;
   static int pButton, mButton, rButton;
   SUMA_Boolean ROI_mode; 
   static SUMA_Boolean DoubleClick = NOPE;
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL; /* Use this only to create prompt that are not to be preserved */
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
               XtCloseDisplay( SUMAg_CF->X->DPY_controller1 ) ;
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
            if (!list) list = SUMA_CreateList();
            ED = SUMA_InitializeEngineListData (SE_ToggleBackground);
            if (!SUMA_RegisterEngineListCommand (  list, ED,
                                                   SEF_Empty, NULL,
                                                   SES_Suma, (void *)sv, NOPE,
                                                   SEI_Head, NULL)) {
               fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
            }
            
            ED = SUMA_InitializeEngineListData (SE_Redisplay);
            if (!SUMA_RegisterEngineListCommand (  list, ED,
                                                   SEF_Empty, NULL,
                                                   SES_Suma, (void *)sv, NOPE,
                                                   SEI_Head, NULL)) {
               fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
            }
                                                    
            if (!SUMA_Engine (&list)) {
               fprintf(SUMA_STDERR, "Error SUMA_input: SUMA_Engine call failed.\n");
            }
            break;            

         case XK_c:
            if (!list) list = SUMA_CreateList();
            ED = SUMA_InitializeEngineListData (SE_OpenColFileSelection);
            if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                                   SEF_vp, (void *)(SUMAg_DOv[sv->Focus_SO_ID].OP),
                                                   SES_Suma, (void *)sv, NOPE,
                                                   SEI_Head, NULL))) {
               fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
            }
            
            if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, sv->X->TOPLEVEL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, NextElm)) {
               fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
            }  
            
            if (!SUMA_Engine (&list)) {
               fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
            }
            
            break;
             
            #if 0
            /* THE OLD WAY (part of it) FOR SETTING NODE COLORS DIRECTLY, Left here for documentation */
            /* allocate space */
            fm = (float **)SUMA_allocate2D (ntot/4, 4, sizeof(float));
            if (fm == NULL) {
               fprintf(stderr,"Error SUMA_input: Failed to allocate space for fm\n");
               SUMA_RETURNe;
            }

            if (SUMA_Read_2Dfile (s, fm, 4, ntot/4) != ntot/4 ) {
               fprintf(stderr,"SUMA_input Error: Failed to read full matrix from %s\n", s);
               SUMA_RETURNe;
            }
               
            if (!list) list = SUMA_CreateList();
            ED = SUMA_InitializeEngineListData (SE_SetNodeColor);
            ED->N_cols = 4;
            ED->N_rows = ntot/4;
            if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                   SEF_fm, (void*)fm,
                                                   SES_Suma, (void *)sv, YUP,
                                                   SEI_Head, NULL)) {
               fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
               break;                                      
            } 

            
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);

            if (!SUMA_Engine (&list)) {
               fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
            }

            /* free fm since it was registered by pointer and is not automatically freed after the call to SUMA_Engine */
            if (fm) SUMA_free2D ((char **)fm, ntot/4);
            break;
            #endif 
            

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
            /* flip light position */
            if (!list) list = SUMA_CreateList(); 
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_FlipLight0Pos, SES_Suma, sv);
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);

            if (!SUMA_Engine (&list)) {
               fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
            }
            break;

         case XK_f:
            /* Show/hide the foreground */
            if (!list) list = SUMA_CreateList(); 
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleForeground, SES_Suma, sv);
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);

            if (!SUMA_Engine (&list)) {
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

               /* register fv15 with ED */
               if (!list) list = SUMA_CreateList(); 
               ED = SUMA_InitializeEngineListData (SE_HighlightNodes);
               if (!SUMA_RegisterEngineListCommand (     list, ED, 
                                                         SEF_fv15, (void*)fv15,
                                                         SES_Suma, (void *)sv, NOPE,
                                                         SEI_Head, NULL)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
                     break;                                      
               }
               
               SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);

               if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
               }
            }
            break;

         case XK_h:
            if (Kev.state & ControlMask){
              if (!list) list = SUMA_CreateList();
              SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Help, SES_Suma, NULL); 
              if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
              }    
            }else{
               if (SUMAg_CF->Dev) {
                  SUMA_SLP_Note("Please use ctrl+h for help.\nh alone will be reassigned in future versions.");
                  #if 0
                  /* fake some error logs */
                  SUMA_RegisterMessage (SUMAg_CF->MessageList, "Test Notice", FuncName, SMT_Notice, SMA_Log);
                  SUMA_RegisterMessage (SUMAg_CF->MessageList, "Test Notice2", FuncName, SMT_Notice, SMA_LogAndPopup);
                  SUMA_RegisterMessage (SUMAg_CF->MessageList, "Test Warning", FuncName, SMT_Warning, SMA_LogAndPopup);
                  SUMA_RegisterMessage (SUMAg_CF->MessageList, "Test Error", FuncName, SMT_Error, SMA_LogAndPopup);
                  SUMA_RegisterMessage (SUMAg_CF->MessageList, "Test Critical", FuncName, SMT_Critical, SMA_LogAndPopup);
                  #endif
               }
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
                  if (!list) list = SUMA_CreateList ();
                  ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
                  if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                         SEF_fv3, (void*)fv3,
                                                         SES_Suma, (void *)sv, NOPE,
                                                         SEI_Head, NULL)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
                     break;                                      
                  } 

                  if (!SUMA_Engine (&list)) {
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
                  if (!list) list = SUMA_CreateList ();
                  ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
                  if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                         SEF_i, (void*)(&it),
                                                         SES_Suma, (void *)sv, NOPE,
                                                         SEI_Head, NULL)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
                     break;                                      
                  } 
                  
                  if (!SUMA_Engine (&list)) {
                     fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
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
                  if (!list) list = SUMA_CreateList ();
                  ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
                  if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                         SEF_i, (void*)(&it),
                                                         SES_Suma, (void *)sv, NOPE,
                                                         SEI_Head, NULL)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
                     break;                                      
                  } 
                  

                  /* Now set the cross hair position at the selected node*/
                  {
                     SUMA_SurfaceObject *SO= NULL;
                     SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
                     ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
                     if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                            SEF_fv3, (void*)&(SO->NodeList[3*it]),
                                                            SES_Suma, (void *)sv, NOPE,
                                                            SEI_Head, NULL)) {
                        fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
                        break;                                      
                     } 
                     
                     /* call with the list */
                     if (!SUMA_Engine (&list)) {
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
               if (!list) list = SUMA_CreateList ();
               ED = SUMA_InitializeEngineListData (SE_SetSelectedFaceSet);
               if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                      SEF_i, (void*)&it,
                                                      SES_Suma, (void *)sv, NOPE,
                                                      SEI_Head, NULL)) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
                  break;                                      
               }
               
               if (!SUMA_Engine (&list)) {
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
                  if (!list) list = SUMA_CreateList();
                  ED = SUMA_InitializeEngineListData (SE_ToggleLockAllCrossHair);
                  if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                         SEF_Empty, NULL, 
                                                        SES_Suma, (void *)sv, NOPE, 
                                                        SEI_Head, NULL )) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
                     break;
                  }
                  if (!SUMA_Engine (&list)) {
                     fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
                  }
               }
            } else {
               sv->X->LookAt_prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, "Enter X,Y,Z coordinates to look at:", 
                                                      "0,0,0",
                                                      sv->X->TOPLEVEL, YUP,
                                                      SUMA_APPLY_BUTTON,
                                                      SUMA_LookAtCoordinates, (void *)sv,
                                                      NULL, NULL,
                                                      NULL, NULL,
                                                      SUMA_isNumString, (void*)3,  
                                                      sv->X->LookAt_prmpt);
               
               sv->X->LookAt_prmpt = SUMA_CreatePromptDialog(sv->X->Title, sv->X->LookAt_prmpt);
               
            }
            break;

         case XK_L:
            if (SUMAg_CF->Dev) {
               prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, "Enter X,Y,Z coordinates of light0:", 
                                                      "",
                                                      sv->X->TOPLEVEL, NOPE,
                                                      SUMA_APPLY_BUTTON,
                                                      SUMA_SetLight0, (void *)sv,
                                                      NULL, NULL,
                                                      NULL, NULL,
                                                      SUMA_isNumString, (void*)3,  
                                                      prmpt);
               
               prmpt = SUMA_CreatePromptDialog(sv->X->Title, prmpt);
               

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
                      sv->X->MOMENTUMID = XtAppAddTimeOut(SUMAg_CF->X->App, 1, SUMA_momentum, (XtPointer) w);
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
               if (Kev.state & ControlMask){
                  fprintf(SUMA_STDOUT, "%s: Opening a new controller...\n", FuncName);
                  /* open a new controller */
                  if (!SUMA_X_SurfaceViewer_Create ()) {
                     fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_X_SurfaceViewer_Create.\n", FuncName);
                     SUMA_RETURNe;
                  } 
               }else {
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

                     fprintf (SUMA_STDOUT, "Parsed Input:\n\tCenter %f, %f, %f.\n\tBox Size %f, %f, %f\n", \
                        fv15[0], fv15[1],fv15[2],\
                        fv15[3], fv15[4],fv15[5]);

                     /* register fv15 with ED */
                     if (!list) list = SUMA_CreateList ();
                     ED = SUMA_InitializeEngineListData (SE_GetNearestNode);
                     if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                            SEF_fv15, (void *)fv15, 
                                                            SES_Suma, (void *)sv, NOPE, 
                                                            SEI_Head, NULL )) {
                        fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
                        break;
                     }

                     SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
                     if (!SUMA_Engine (&list)) {
                        fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
                     }
                  }
               }
            break;

         case XK_p:
            sv->PolyMode = ((sv->PolyMode+1) % SRM_N_RenderModes);
            if (sv->PolyMode <= SRM_ViewerDefault) sv->PolyMode = SRM_Fill;
            
            SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
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
               #if 0
               /** Feb 03/03 No longer in use.*/
               for (ii=0; ii< sv->N_DO; ++ii) {
                  if (SUMA_isSO(SUMAg_DOv[sv->ShowDO[ii]])) 
                     SUMA_Print_Surface_Object((SUMA_SurfaceObject*)SUMAg_DOv[sv->ShowDO[ii]].OP, stdout);
               }
               #endif
            }
            break;

         case XK_t:
            if ((Kev.state & ControlMask) && SUMAg_CF->Dev){
                  fprintf(SUMA_STDOUT, "%s: Forcing a resend of Surfaces to Afni...\n", FuncName);
                  if (!list) list = SUMA_CreateList();
                  SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_SetForceAfniSurf, SES_Suma, sv);
                  
                  if (!SUMA_Engine (&list)) {
                     fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
                  }
            } else {
               if (!list) list = SUMA_CreateList();
               SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleConnected, SES_Suma, sv);
               
               if (!SUMA_Engine (&list)) {
                     fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
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
                  if (!SO->PolyArea) {
                     fprintf(SUMA_STDOUT, "%s: Computing required mesh area.\n", FuncName);
                     if (!SUMA_SurfaceMetrics (SO, "PolyArea", NULL)) {
                        fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SurfaceMetrics.\n", FuncName);
                        break;
                     }
                  }
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
               if (!list) list = SUMA_CreateList();
               SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
               if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
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
               if (!list) list = SUMA_CreateList();
               SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
               if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
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
            if (!list) list = SUMA_CreateList();
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleCrossHair, SES_Suma, sv);
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
            if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
            }
            break;

         case XK_F4: /* F4 */
            if (!list) list = SUMA_CreateList();
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleShowSelectedNode, SES_Suma, sv);
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
            if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
            }
            break;

         case XK_F5: /* F5 */
            if (!list) list = SUMA_CreateList();
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleShowSelectedFaceSet, SES_Suma, sv);
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
            if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
            }
            break;

         case XK_F6: /*F6 */
            if (sv->clear_color[0] == 0.0) { /* flip background from black to white */
               sv->clear_color[0] = sv->clear_color[1] = sv->clear_color[2] = sv->clear_color[3] = 1.0;
            } else { /* goto black */
               sv->clear_color[0] = sv->clear_color[1] = sv->clear_color[2] = sv->clear_color[3] = 0.0;
            }
            if (!list) list = SUMA_CreateList();
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
            if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
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
            if (!list) list = SUMA_CreateList();
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Home, SES_Suma, sv);
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_FOVreset, SES_Suma, sv);
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
            if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
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
               /* check to see if other viewers need to be notified */
               ii = SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv);
               if (SUMAg_CF->ViewLocked[ii]) {
                  for (it=0; it < SUMAg_N_SVv; ++it) {
                     svi = &SUMAg_SVv[it];
                     if (it != ii && SUMAg_CF->ViewLocked[it]) {
                        svi->GVS[svi->StdView].spinBeginX = (int)Bev.x;
                        svi->GVS[svi->StdView].spinBeginY = (int)Bev.y;
                        svi->GVS[svi->StdView].spinDeltaX = 0;
                        svi->GVS[svi->StdView].spinDeltaY = 0; 
                     }  
                  }
               }
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
               
               /* are we in ROI drawing mode ? */
               if (Bev.state & ShiftMask && SUMAg_CF->ROI_mode && sv->Focus_SO_ID >= 0) {
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
                              sprintf (s,"You cannot select while other viewers\n(like #%d) are in momentum mode.\n", ii);
                              SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                                    s, FuncName, SMT_Error, SMA_LogAndPopup);
                              SUMA_RETURNe;
                           }
                        }
                     }
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
               
               if (ROI_mode) {
                  /* keep track of mouse motion in window */
                  if (!SUMA_CreateBrushStroke (sv)) {
                     SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                           "Failed to create BrushStroke.", FuncName, 
                                           SMT_Error, SMA_LogAndPopup);
                     SUMA_RETURNe;

                  }

                  SUMA_AddToBrushStroke (sv, (int)Bev.x, (int)Bev.y, sv->Pick0,
                     sv->Pick1, YUP); 
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
            if (SUMAg_CF->ROI_mode) {
               SUMA_DRAWN_ROI *DrawnROI = NULL;
               SUMA_SurfaceObject *SO = NULL;
               SUMA_BRUSH_STROKE_ACTION BsA=SUMA_BSA_Undefined;
               
               if (sv->BS) { 
                  /* Process the brush stroke*/
                  if (DoubleClick) BsA = SUMA_BSA_JoinEnds;
                  else BsA = SUMA_BSA_AppendStrokeOrFill;
                  if (!(DrawnROI = SUMA_ProcessBrushStroke (sv, BsA))) {
                     if (LocalHead) fprintf (SUMA_STDERR, "%s: NULL DrawnROI returned.\n", FuncName);
                     SUMA_ClearBrushStroke (sv);
                     break;
                  }

                  /* Showme the DrawnROI */
                  if (LocalHead) SUMA_ShowDrawnROI (DrawnROI, NULL, NOPE);

                  /* do smething with the BrushStroke, then wipe it clean, OK to show even if empty*/
                  if (LocalHead) SUMA_ShowBrushStroke (sv, NULL);

                  /* SUMA_DrawBrushStroke (sv, YUP); */
                  SUMA_ClearBrushStroke (sv);

                  /* redisplay all others */
                  if (!list) list = SUMA_CreateList ();
                  SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_RedisplayNow_AllOtherVisible, SES_SumaWidget, sv);
                  SUMA_Engine (&list);
               
                  /* redisplay . DO NOT REDISPLAY WITH SE_Redisplay_AllVisible or you will have GL state synchronization problems */
                  sv->ResetGLStateVariables = YUP;
                  SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);

               }/* if sv->BS */
            } /* if SUMAg_CF->ROImode */
            
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
         }else if(Mev.state & Button1MotionMask) { 
            mButton = SUMA_Button_3_Motion;
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
         } else if(Mev.state & Button3MotionMask) { 
            mButton = SUMA_Button_3_Motion;
         }else {
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
            ii = SUMA_WhichSV (sv, SUMAg_SVv, SUMAg_N_SVv);
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
               
               ii = SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv);
               if (ii < 0) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to find index of sv.\n", FuncName);
                  break;
               }
               if (!SUMAg_CF->ViewLocked[ii]) { /* No locking, just redisplay current viewer */
                  SUMA_postRedisplay(w, clientData, callData);    
               } else { /* locking, update and redisplay those locked */
                  DList *list = NULL;
                  SUMA_EngineData *ED = NULL;
                  /* redisplay current viewer immediately */
                  list = SUMA_CreateList ();
                  ED = SUMA_InitializeEngineListData (SE_RedisplayNow);
                  SUMA_RegisterEngineListCommand (list, ED,
                                                   SEF_Empty, NULL,
                                                   SES_Suma, (void *)sv, NOPE,
                                                   SEI_Head, NULL);
                  for (it=0; it < SUMAg_N_SVv; ++it) {
                     svi = &SUMAg_SVv[it];
                     if (it != ii && SUMAg_CF->ViewLocked[it]) {
                        /* copy quaternions */
                        svi->GVS[svi->StdView].spinBeginX = sv->GVS[sv->StdView].spinBeginX;
                        svi->GVS[svi->StdView].spinBeginY = sv->GVS[sv->StdView].spinBeginY;
                        SUMA_COPY_VEC(sv->GVS[sv->StdView].deltaQuat, svi->GVS[svi->StdView].deltaQuat, 4, float, float);
                        SUMA_COPY_VEC(sv->GVS[sv->StdView].currentQuat, svi->GVS[svi->StdView].currentQuat, 4, float, float);
                       
                        /* add a redisplay now */
                        ED = SUMA_InitializeEngineListData (SE_RedisplayNow);
                        SUMA_RegisterEngineListCommand ( list, ED,
                                                         SEF_Empty, NULL,
                                                         SES_Suma, (void *)svi, NOPE,
                                                         SEI_Head, NULL); 
                     }
                  }
                  if (!SUMA_Engine (&list)) {
                     fprintf (SUMA_STDERR, "Error %s: Failed calling SUMA_Engine.\n", FuncName);
                     break;
                  }
               }
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
         
         case SUMA_Button_3_Motion:
            if (LocalHead) fprintf(SUMA_STDERR,"%s: In motion, Butt3 \n", FuncName); 
            
            if (SUMAg_CF->ROI_mode && sv->Focus_SO_ID >= 0 && sv->BS) {
               /* ROI drawing mode */
               ii = SUMA_ShownSOs(sv, SUMAg_DOv, NULL);
               if (ii == 0) { /* no surfaces, break */
                  break;
               }


               if (!SUMA_GetSelectionLine (sv, (int)Mev.x, (int)Mev.y)) {
                  fprintf (SUMA_STDERR, "Error %s: Failed in "
                                       "SUMA_GetSelectionLine.\n", FuncName);
                  break;
               } 

               if (!SUMA_AddToBrushStroke (sv, (int)Mev.x, (int)Mev.y,
                     sv->Pick0, sv->Pick1, YUP)) {
                  SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                        "Failed to add to BrushStroke.", 
                                        FuncName, 
                                        SMT_Error, SMA_LogAndPopup);
                  break;
               }
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
   sv->X->MOMENTUMID = XtAppAddTimeOut(SUMAg_CF->X->App, 1, SUMA_momentum, (XtPointer) w);

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
   SUMA_EngineData *ED = NULL;
   DList *list = NULL;
   DListElmt *SetNodeElem = NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = YUP;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);


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

         MTIi = SUMA_MT_intersect_triangle(P0f, P1f, SO->NodeList, SO->N_Node, SO->FaceSetList, SO->N_FaceSet, NULL);

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
               MTIi = SUMA_Free_MT_intersect_triangle(MTIi); 
            }
         }else {
            /* not good, toss it away */
           if (LocalHead) fprintf (SUMA_STDERR, "%s: ii=%d freeing MTIi no hits...\n", FuncName, ii);
           MTIi = SUMA_Free_MT_intersect_triangle(MTIi); 
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

      /* Set the Nodeselection at the closest node */
      it = MTI->inodemin;
      if (!list) list = SUMA_CreateList();
      ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
      SetNodeElem = SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_i, (void*)&it,
                                             SES_Suma, (void *)sv, NOPE,
                                             SEI_Head, NULL);
      if (!SetNodeElem) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register SetNodeElem\n", FuncName);
         SUMA_RETURN (-1);
      }

      /* Set the FaceSetselection */
      it = MTI->ifacemin;
      ED = SUMA_InitializeEngineListData (SE_SetSelectedFaceSet);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_i, (void*)&it,
                                             SES_Suma, (void *)sv, NOPE,
                                             SEI_Head, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURN (-1);
      }
      
      /* Now set the cross hair position at the intersection*/
      ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_fv3, (void*)MTI->P,
                                             SES_Suma, (void *)sv, NOPE,
                                             SEI_Head, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURN (-1);
      }

      /* attach the cross hair to the selected surface */
      iv3[0] = SUMA_findSO_inDOv(SO->idcode_str, SUMAg_DOv, SUMAg_N_DOv);
      iv3[1] = MTI->inodemin;
      ED = SUMA_InitializeEngineListData (SE_BindCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_iv3, (void*)iv3,
                                             SES_Suma, (void *)sv, NOPE,
                                             SEI_Head, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURN (-1);
      }
      
      /* check to see if AFNI needs to be notified */
      if (SUMAg_CF->Connected && sv->LinkAfniCrossHair) {
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Notifying Afni of CrossHair XYZ\n", FuncName);
         /* register a call to SetAfniCrossHair */
         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_SetAfniCrossHair, SES_Suma, sv);
         if (!SUMA_Engine (&list)) {
            fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
            SUMA_RETURN (-1);
         }
      }else {
         if (LocalHead) fprintf(SUMA_STDERR,"%s: No Notification to AFNI.\n", FuncName);
      }

      /* now put in a request for locking cross hair but you must do this after the node selection has been executed 
      NOTE: You do not always have SetNodeElem because the list might get emptied in the call to AFNI notification.
      You should just put the next call at the end of the list.*/
      if (!list) list = SUMA_CreateList();
      ED = SUMA_InitializeEngineListData (SE_LockCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_iv3, (void*)iv3,
                                             SES_Suma, (void *)sv, NOPE,
                                             SEI_Tail, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURN (-1);
      }
      
      if (!SUMA_Engine (&list)) {
         fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
         SUMA_RETURN (-1);
      }
      

   } 
   /* clear MTI */
   if (MTI) {
      MTI = SUMA_Free_MT_intersect_triangle(MTI);
   }
   
   if (imin >= 0) {
      SUMA_RETURN (1); /* hit */
   } else {
      SUMA_RETURN (0); /* no hit */
   }
}/* determine intersection */

/*!
   \brief Show the contents of a brush stroke
   SUMA_ShowBrushStroke (sv, Out);
   
*/
void SUMA_ShowBrushStroke (SUMA_SurfaceViewer *sv, FILE *out)
{
   static char FuncName[]={"SUMA_ShowBrushStroke"};
   int i, k, N=0;
   SUMA_BRUSH_STROKE_DATUM *bsd=NULL;
   DListElmt *Next_Elm = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!out) out = SUMA_STDERR;
   
   if (!sv->BS) {
      fprintf(out, "%s: NULL sv->BS\n", FuncName);
      SUMA_RETURNe;
   }
   
   N = dlist_size(sv->BS);
   if (!N) {
      fprintf(out, "%s: Empty sv->BS. (N = 0)\n", FuncName);
      SUMA_RETURNe;
   }
   
   fprintf(out, "%s: Brush stroke has %d elements:\n", FuncName, N);
   
   i = 0;
   do {
      if (Next_Elm==NULL) Next_Elm = dlist_head(sv->BS);
      else Next_Elm = Next_Elm->next;
      if (!Next_Elm->data) {
         fprintf(out, "%s: Element->data %d is NULL!\n", FuncName, i);
      }else {
         bsd = (SUMA_BRUSH_STROKE_DATUM *)Next_Elm->data; 
         fprintf(out, "%d: (%f %f) [%.2f, %.2f, %.2f <--> %.2f, %.2f, %.2f]\t Node  %d, Tri %d\n", 
                     i, bsd->x, bsd->y,
                     bsd->NP[0], bsd->NP[1], bsd->NP[2],
                     bsd->FP[0], bsd->FP[1], bsd->FP[2],
                     bsd->SurfNode, bsd->SurfTri);
      }
      ++i;
   }while (dlist_tail(sv->BS) != Next_Elm);
   
   fprintf(out, "\n");
   
   SUMA_RETURNe;
}

/*!
   \brief Clear the contents of sv->BS and sets it to NULL
   
   SUMA_ClearBrushStroke (sv);
   
   
   \sa SUMA_CreateBrushStroke
*/
void  SUMA_ClearBrushStroke (SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_ClearBrushStroke"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* THE NEW VERSION */
   if (sv->BS) {
      SUMA_EmptyDestroyList(sv->BS);
      sv->BS = NULL;
   }
   
   SUMA_RETURNe;
}
/*!
   \brief Creates the BrushStroke structure inside sv structure
   success = SUMA_CreateBrushStroke (sv);
   
   \param sv (SUMA_SurfaceViewer *) Surface viewer structure
   \return YUP/NOPE
   
   sv->BS must be null before this function is called. 
   The liat and its components are then allocated for.
   
   \sa SUMA_ClearBrushStroke
*/
SUMA_Boolean  SUMA_CreateBrushStroke (SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_CreateBrushStroke"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
      
   /* New Version */
   if (sv->BS) {  /* bad news, this should be NULL to begin with */
      SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                            "Brush Stroke not NULL.", FuncName, 
                            SMT_Critical, SMA_LogAndPopup);
      SUMA_RETURN(NOPE); 
      
   }
   sv->BS = (DList *)SUMA_malloc(sizeof(DList));
   dlist_init(sv->BS, SUMA_FreeBSDatum);  
   
   SUMA_RETURN (YUP);
}

SUMA_BRUSH_STROKE_DATUM * SUMA_CreateBSDatum(void)
{
   static char FuncName[]={"SUMA_CreateBSDatum"};
   SUMA_BRUSH_STROKE_DATUM *bsd = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   bsd = (SUMA_BRUSH_STROKE_DATUM *)SUMA_malloc(sizeof(SUMA_BRUSH_STROKE_DATUM));
   if (!bsd) {
      SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                            "Failed to allocate.", FuncName, 
                            SMT_Critical, SMA_LogAndPopup);
      SUMA_RETURN(NULL); 
   }
   /* setup defaults */
   bsd->x = bsd->y = 0.0;
   bsd->NP[0] = bsd->NP[1] = bsd->NP[2] = 0.0;
   bsd->FP[0] = bsd->FP[1] = bsd->FP[2] = 0.0;
   bsd->SurfNode = -1;
   bsd->SurfTri = -1;
   bsd->Decimated = NOPE;
   
   SUMA_RETURN(bsd);
}

/*!
   \brief free a brush stroke datum that is contained inside the doubly linked BS
*/
void SUMA_FreeBSDatum (void *bsd)
{
   static char FuncName[]={"SUMA_FreeBSDatum"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* nothing is allocated for inside bsd */
   if (bsd) SUMA_free(bsd);
   
   SUMA_RETURNe;
}



/*!
   \brief Adds, new point to the brush stroke
   success = SUMA_AddToBrushStroke ( sv,  x,  y, Show);
   
   \param sv (SUMA_SurfaceViewer *) pointer to surface viewer where stroke is occuring
   \param x (int) X coordinate of mouse
   \param y (int) Y coordinate of mouse
   \param NP (GLdouble *) vector of XYZ coordinates of Near Plane intersection point
   \param FP (GLdouble *) vector of XYZ coordinates of Far Plane intersection point.
   \param Show (SUMA_Boolean) if YUP: Then trace is drawn as you move the mouse
   \return YUP/NOPE, success indicator
   
*/
SUMA_Boolean  SUMA_AddToBrushStroke (SUMA_SurfaceViewer *sv, int x, int y, GLdouble *NP, GLdouble *FP, SUMA_Boolean Show)
{
   static char FuncName[]={"SUMA_AddToBrushStroke"};
   int ip;
   SUMA_BRUSH_STROKE_DATUM *bsd=NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* New version */
   bsd = SUMA_CreateBSDatum();
   bsd->x = (float)x;
   bsd->y = (float)y;
   bsd->NP[0] = NP[0]; bsd->NP[1] = NP[1]; bsd->NP[2] = NP[2];
   bsd->FP[0] = FP[0]; bsd->FP[1] = FP[1]; bsd->FP[2] = FP[2]; 
   dlist_ins_next (sv->BS, dlist_tail(sv->BS), (void*)bsd);
   
   /* incremental draw */
   if (Show) SUMA_DrawBrushStroke (sv, YUP);
   
   SUMA_RETURN (YUP);
}

/*!
   Sets the foreground color of the drawing area
*/
void SUMA_SetSVForegroundColor (SUMA_SurfaceViewer *sv, const char *Color)
{
   static char FuncName[]={"SUMA_SetSVForegroundColor"};
   XColor col, unused;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
	/* using sv->X->CMAP instead of 
	         DefaultColormapOfScreen(XtScreen(sv->X->GLXAREA))
		is useless */
   if (!XAllocNamedColor (sv->X->DPY, DefaultColormapOfScreen(XtScreen(sv->X->GLXAREA)),
      Color, &col, &unused)) {
      fprintf (SUMA_STDERR, "Error %s: Can't allocate for %s color.\n", FuncName, Color);
      SUMA_RETURNe;  
   }
   XSetForeground (sv->X->DPY, sv->X->gc, col.pixel);
   
   SUMA_RETURNe;  
}
/*!
   \brief Draws the brushstroke
   
   \param sv (SUMA_SurfaceViewer *) pointer to surface viewer structure
   \param incremental (SUMA_Boolean) YUP: draw a line between the last two points
                                     NOPE: draw the whole thing
												 
	- NB: This function used to crash when run on SGI if display is not in TrueColor mode.
	This happens even though the visual chosen by SUMA does not change.
	To put the SGI in true color mode, you need to add to /var/X11/xdm/Xservers
	the following:  -class TrueColor -depth 24
	and then restart X or the system.
   The bug was that the graphics context (sv->X->gc) was created using the Screen's 
   root window and not the GLX visual's window. 
*/
void SUMA_DrawBrushStroke (SUMA_SurfaceViewer *sv, SUMA_Boolean incr)
{
   static char FuncName[]={"SUMA_DrawBrushStroke"};
   int i, N;
   DListElmt *NE=NULL, *NEn=NULL;
   SUMA_BRUSH_STROKE_DATUM *bsd=NULL, *bsdn = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
	if (!sv->BS) SUMA_RETURNe;
   
   N = dlist_size(sv->BS);
   if (N < 2) SUMA_RETURNe;
   
   if (!incr) {   
      do {
         if (!NE) NE = dlist_head(sv->BS);
         else NE = NE->next;
         
         NEn = NE->next;
         
         bsd = (SUMA_BRUSH_STROKE_DATUM *)NE->data;
         bsdn = (SUMA_BRUSH_STROKE_DATUM *)NEn->data;
         
         XDrawLine (sv->X->DPY, XtWindow(sv->X->GLXAREA), sv->X->gc, 
                    (int)bsd->x, (int)bsd->y,
                    (int)bsdn->x, (int)bsdn->y);
          
      } while (NEn != dlist_tail(sv->BS));
      
   } else {
      NEn = dlist_tail(sv->BS);
      NE = NEn->prev;
      
      bsd = (SUMA_BRUSH_STROKE_DATUM *)NE->data;
      bsdn = (SUMA_BRUSH_STROKE_DATUM *)NEn->data;
      
      XDrawLine (sv->X->DPY, XtWindow(sv->X->GLXAREA), sv->X->gc, 
                    (int)bsd->x, (int)bsd->y,
                    (int)bsdn->x, (int)bsdn->y);
   }
   SUMA_RETURNe;

}

/*!
   \brief Processes the brushstroke sent from a viewer
   
*/
SUMA_DRAWN_ROI * SUMA_ProcessBrushStroke (SUMA_SurfaceViewer *sv, SUMA_BRUSH_STROKE_ACTION BsA)
{
   static char FuncName[]={"SUMA_ProcessBrushStroke"};
   SUMA_DRAWN_ROI *DrawnROI = NULL;
   SUMA_ROI_DATUM *ROIstroke = NULL, *ROIlink=NULL, *ROIfill=NULL;
   SUMA_SurfaceObject *SO = NULL;
   int ii=0, TailNode = -1, FirstSurfNode = -1, ft = -1, N_SurfNode = 0;
   int HeadNode = -1, *ROI_Mask=NULL, N_ROI_Mask = 0;
   DListElmt *El = NULL;
   SUMA_BRUSH_STROKE_DATUM *bsd=NULL;
   char *sbuf;
   SUMA_ROI_ACTION_STRUCT *ROIA;
   DListElmt *tmpStackPos=NULL;
   SUMA_Boolean Shaded = NOPE, LocalHead = NOPE;

      
   SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!SO) {
      fprintf (SUMA_STDERR, "%s: No surface object in focus, nothing to do.\n", FuncName); 
      SUMA_RETURN (DrawnROI);
   }
   
   if (!sv->BS) {
      fprintf (SUMA_STDERR, "%s: No Brushstroke (BS), nothing to do.\n", FuncName); 
      SUMA_RETURN (DrawnROI);
   }
   
   if (!SUMAg_CF->ROI_mode) {
      fprintf (SUMA_STDERR, "%s: Not in ROI mode, nothing to do.\n", FuncName); 
      SUMA_RETURN (DrawnROI);
   }
   
   /* We are in ROI mode, is there an ROI in curDrawnROI that works with the current surface ? */
   if (SUMAg_CF->X->DrawROI->curDrawnROI) {
      if (SUMA_isdROIrelated(SUMAg_CF->X->DrawROI->curDrawnROI, SO) && SUMAg_CF->X->DrawROI->curDrawnROI->DrawStatus != SUMA_ROI_Finished) {
         if (LocalHead) fprintf (SUMA_STDERR,"%s: using currDrawnROI.\n", FuncName);
         DrawnROI = SUMAg_CF->X->DrawROI->curDrawnROI;
      }else {
         if (LocalHead) fprintf (SUMA_STDERR,"%s: No match between currDrawnROI and SO.\n", FuncName);
         DrawnROI = NULL;
      }
   }
   if (!DrawnROI) { /* try some more */
      if ((DrawnROI = SUMA_FetchROI_InCreation (SO, SUMAg_DOv, SUMAg_N_DOv))){
         if (LocalHead) fprintf (SUMA_STDERR,"%s: using ROI in creation.\n", FuncName);
         /* There is an ROI being created on this surface, initialize DrawROI window*/
         SUMA_InitializeDrawROIWindow(DrawnROI);
      } else {
         /* wait till later */
         if (LocalHead) fprintf (SUMA_STDERR,"%s: will create a new ROI.\n", FuncName);
      }
   }
   
   if (!DrawnROI && BsA == SUMA_BSA_JoinEnds) {
      SUMA_SLP_Err ("NO ROI to close.");
      SUMA_RETURN (DrawnROI);
   }
   
   if (!DrawnROI) { /* No ROI found, create one */
      if (LocalHead) fprintf (SUMA_STDERR, "%s: No ROI found, creating a new one.\n", FuncName);
      SUMA_GET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIlbl->textfield, sbuf);
      DrawnROI = SUMA_AllocateDrawnROI (SO->idcode_str, SUMA_ROI_InCreation, SUMA_ROI_OpenPath, 
                                        sbuf, 
                                        SUMAg_CF->X->DrawROI->ROIval->value);
      if (!DrawnROI) {
         SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                               "Failed to allocate for DrawnROI.", FuncName, 
                               SMT_Critical, SMA_LogAndPopup);
         SUMA_RETURN (NULL);
      }

      /* Although ROIs are stored as DOs, they are dependent on the surfaces they are related to 
      ROIs at this stage are node indices only (and perhaps the mesh) but the coordinates of the indices
      come from the surface onto which they are displayed. So when you are drawing a surface, using CreateMesh,
      you will search DOv for ROIs related to the surface displayed and overlay them accordingly */
      /* Add the ROI to DO */
      if (!SUMA_AddDO (SUMAg_DOv, &SUMAg_N_DOv, (void *)DrawnROI, ROIdO_type, SUMA_LOCAL)) {
                           fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
      }

      /* is the Switch ROI window open ? */
      SUMA_IS_DRAW_ROI_SWITCH_ROI_SHADED(Shaded);
      if (!Shaded) {
         SUMA_cb_DrawROI_SwitchROI (NULL, (XtPointer) SUMAg_CF->X->DrawROI->SwitchROIlst, NULL);
      }
   

   } else {
      if (LocalHead) fprintf(SUMA_STDOUT,"%s: ROI %p fetched. Status %d.\n", FuncName, DrawnROI, DrawnROI->DrawStatus); 
   } 

   if (BsA == SUMA_BSA_AppendStrokeOrFill) {
      if (DrawnROI->Type == SUMA_ROI_ClosedPath || DrawnROI->Type == SUMA_ROI_FilledArea) BsA = SUMA_BSA_FillArea;
      else if (DrawnROI->Type == SUMA_ROI_OpenPath) BsA = SUMA_BSA_AppendStroke;
   }
   if (DrawnROI->Type == SUMA_ROI_ClosedPath && BsA != SUMA_BSA_FillArea) {
      SUMA_SLP_Err ("You can only fill a closed path.\nYou cannot append more paths to it.");
      SUMA_RETURN (DrawnROI);
   }
   if (DrawnROI->Type == SUMA_ROI_FilledArea && BsA != SUMA_BSA_FillArea) {
      SUMA_SLP_Err ("You cannot add paths to a filled ROI.");
      SUMA_RETURN (DrawnROI);
   }
   
   /* Good, now initialize the DrawROI widget, if needed */
   if (SUMAg_CF->X->DrawROI->curDrawnROI != DrawnROI) {
      if (!SUMA_InitializeDrawROIWindow (DrawnROI)) {
         SUMA_SL_Err("Failed to initialize DrawWindow.");
      }
   }

   /* Now you must transform the brushstroke to a series of nodes (not necessarily connected)*/
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Turning BrushStroke to NodeStroke ...\n", FuncName);
   if (!SUMA_BrushStrokeToNodeStroke (sv)) {
      SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                         "Failed in SUMA_BrushStrokeToNodeStroke.", FuncName, 
                         SMT_Error, SMA_LogAndPopup);
      SUMA_RETURN(NULL);
   }
      
   switch (BsA) {
      case SUMA_BSA_AppendStroke:
         /* Turn the brush stroke into a series of connected nodes */
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Turning NodeStroke to ROIStroke ...\n", FuncName);
         if (!(ROIstroke = SUMA_NodeStrokeToConnectedNodes (sv))) {
            SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                               "Failed in SUMA_NodeStrokeToConnectedNodes.", FuncName, 
                               SMT_Critical, SMA_LogAndPopup);
            if (ROIlink) SUMA_FreeROIDatum((void *)ROIlink); ROIlink = NULL;
            if (ROIstroke) SUMA_FreeROIDatum((void *)ROIstroke); ROIstroke = NULL;
            SUMA_RETURN(NULL);
         }

         if (LocalHead) fprintf (SUMA_STDERR, "%s: Turning NodeStroke to ROIStroke . DONE.\n", FuncName);
         /* if this is the first element of ROI, create the first ROIdatum and get out */
         if (dlist_size(DrawnROI->ROIstrokelist)) {
            /* Not the beginning of an ROI */
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Adding ROIstroke to previous ones ...\n", FuncName);
            /* make sure new brushstroke is not just one node that is the tail of the ROI*/
            SUMA_DRAWN_ROI_TAIL_NODE(DrawnROI,TailNode);

            SUMA_BS_FIRST_SURF_NODE(sv->BS, FirstSurfNode, ft, El);
            SUMA_BS_COUNT_SURF_NODES(sv->BS, N_SurfNode);
            if (FirstSurfNode == TailNode && N_SurfNode == 1) {
               /* nothing to do here */
               fprintf (SUMA_STDERR, "%s: New stroke has one node that is identical to tail node. Dumping element.\n", FuncName);
               SUMA_RETURN(DrawnROI);
            }

            /* Connect this chunk to the last open Node in ROI */
            if (FirstSurfNode != TailNode) {
               if (LocalHead) fprintf (SUMA_STDERR, "%s: linking Tail Node to New stroke.\n", FuncName);

               ROIlink = SUMA_LinkTailNodeToNodeStroke (sv, DrawnROI);
               if (!ROIlink) {
                  SUMA_SL_Err("Failed to connect Tail node to Node stroke, try again.");
                  SUMA_RETURN(NULL);
               }
               if (LocalHead) {
                  fprintf (SUMA_STDERR, "%s: RIOlink, before prepending:\n", FuncName);
                  SUMA_ShowDrawnROIDatum (ROIlink, NULL, NOPE);
               }

               /* connect the ROIlink with the ROIstroke */
               if (LocalHead) {
                  fprintf (SUMA_STDERR, "%s: RIOstroke, before prepending:\n", FuncName);
                  SUMA_ShowDrawnROIDatum (ROIstroke, NULL, NOPE);
               }
               if (!SUMA_PrependToROIdatum (ROIlink, ROIstroke)) {
                  SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                     "Failed to merge ROIs.", FuncName,
                                     SMT_Critical, SMA_LogAndPopup);
                  if (ROIlink) SUMA_FreeROIDatum((void *)ROIlink); ROIlink = NULL;
                  if (ROIstroke) SUMA_FreeROIDatum((void *)ROIstroke); ROIstroke = NULL;
                  SUMA_RETURN(NULL);   
               }

               if (LocalHead) {
                  fprintf (SUMA_STDERR, "%s: RIOstroke, after prepending:\n", FuncName);
                  SUMA_ShowDrawnROIDatum (ROIstroke, NULL, NOPE);
               }
              /* now free ROIlink, not needed anymore */
               if (ROIlink) SUMA_FreeROIDatum ((void *)ROIlink); ROIlink = NULL;
            }
         }else{
            if (LocalHead) fprintf (SUMA_STDERR, "%s: First ROIStroke of ROI.\n", FuncName);
         }
         break;
      case SUMA_BSA_JoinEnds:
         /* Join ends here */
         if (DrawnROI) { /*   close ROI */
            SUMA_DRAWN_ROI_HEAD_NODE(DrawnROI,HeadNode);         
            SUMA_BS_FIRST_SURF_NODE(sv->BS, FirstSurfNode, ft, El);
            bsd = (SUMA_BRUSH_STROKE_DATUM *)El->data;
            if (LocalHead) fprintf(SUMA_STDERR, "%s: Trying to join node %d to node %d.\n", FuncName, FirstSurfNode, HeadNode);
            /* Now compute the intersection of the surface with the plane */
            ROIstroke = SUMA_Surf_Plane_Intersect_ROI (SO, FirstSurfNode, HeadNode, bsd->NP);

            if (!ROIstroke) {
               SUMA_SL_Err ("Failed to close path. Repeat new stroke.");
               SUMA_RETURN(DrawnROI);
            }
            /* what is the last node of ROIstroke ? 
            It is possible that the returned ROIstroke 
            was not a successful closure (a partial success), investigate*/
            if (LocalHead) fprintf(SUMA_STDERR, "%s: Last node of ROIstroke is %d\n", FuncName, ROIstroke->nPath[ROIstroke->N_n-1]); 
            if (ROIstroke->nPath[ROIstroke->N_n-1] != HeadNode) {
               /* pretend this is not a JoinEnds exercice */
               BsA = SUMA_BSA_AppendStroke;
               SUMA_SL_Err ("Failed to close path. Continue with stroke.");
               SUMA_RETURN(DrawnROI);
            }else {
               /* Do not remove the last point from ROIstroke, otherwise it will make drawing a closed ROI painful */
            } 
         } else {
            /* tremors, nothing to do */
         }
         break;
      case SUMA_BSA_FillArea:
         SUMA_BS_FIRST_SURF_NODE(sv->BS, FirstSurfNode, ft, El);
         fprintf (SUMA_STDERR, "%s: Should be filling from node %d\n", FuncName, FirstSurfNode);
         /* create the mask from ROIs on this surface */
         ROI_Mask = SUMA_Build_Mask_AllROI (SUMAg_DOv, SUMAg_N_DOv, SO, NULL, &N_ROI_Mask);
         /* Now fill it up */
         ROIfill = SUMA_FillToMask (SO, ROI_Mask, FirstSurfNode);
         if (ROI_Mask) SUMA_free(ROI_Mask);
         if (!ROIfill) {
            SUMA_SLP_Err("Failed to fill area:\nPerhaps seed on edge\nor nothing to fill.");
            SUMA_RETURN(DrawnROI);
         }
         
         break;
      default:
         fprintf (SUMA_STDERR, "Error %s: Why are you doing this to me ?.\n", FuncName);
         break;
   }
        
   
   /* Another switch on BsA, it is possible that its value changed within this function */
   
   switch (BsA) {
      case SUMA_BSA_AppendStroke:
         /* store the action */
         ROIstroke->action = SUMA_BSA_AppendStroke;
         /*now add the ROIdatum to the list of ROIs */
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Adding ROIStroke to DrawnROI->ROIstrokelist\n", FuncName);
         ROIA = (SUMA_ROI_ACTION_STRUCT *) SUMA_malloc (sizeof(SUMA_ROI_ACTION_STRUCT *)); /* this structure is freed in SUMA_DestroyROIActionData */
         ROIA->DrawnROI = DrawnROI;
         ROIA->ROId = ROIstroke;
         tmpStackPos = SUMA_PushActionStack (DrawnROI->ActionStack, DrawnROI->StackPos, SUMA_AddToTailROIDatum, (void *)ROIA, SUMA_DestroyROIActionData);
         if (tmpStackPos) DrawnROI->StackPos = tmpStackPos;
         else {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_PushActionStack.\n", FuncName);
            SUMA_RETURN (DrawnROI);
         } 
         break;
      case SUMA_BSA_JoinEnds:
         /* store the action */
         ROIstroke->action = SUMA_BSA_JoinEnds;
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Closing path.\n", FuncName);
         ROIA = (SUMA_ROI_ACTION_STRUCT *) SUMA_malloc (sizeof(SUMA_ROI_ACTION_STRUCT *)); /* this structure is freed in SUMA_DestroyROIActionData */
         ROIA->DrawnROI = DrawnROI;
         ROIA->ROId = ROIstroke;
         tmpStackPos = SUMA_PushActionStack (DrawnROI->ActionStack, DrawnROI->StackPos, SUMA_AddToTailJunctionROIDatum, (void *)ROIA, SUMA_DestroyROIActionData);
         if (tmpStackPos) DrawnROI->StackPos = tmpStackPos;
         else {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_PushActionStack.\n", FuncName);
            SUMA_RETURN (DrawnROI);
         }
         break;
      case SUMA_BSA_FillArea:
         /* store the action */
         ROIfill->action = SUMA_BSA_FillArea;
         /* Now add ROIdatum to stack */
         ROIA = (SUMA_ROI_ACTION_STRUCT *) SUMA_malloc (sizeof(SUMA_ROI_ACTION_STRUCT *)); /* this structure is freed in SUMA_DestroyROIActionData */
         ROIA->DrawnROI = DrawnROI;
         ROIA->ROId = ROIfill;
         tmpStackPos = SUMA_PushActionStack (DrawnROI->ActionStack, DrawnROI->StackPos, SUMA_AddFillROIDatum, (void *)ROIA, SUMA_DestroyROIActionData);
         if (tmpStackPos) DrawnROI->StackPos = tmpStackPos;
         else {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_PushActionStack.\n", FuncName);
            SUMA_RETURN (DrawnROI);
         } 
         break;
      default:
         fprintf (SUMA_STDERR, "Error %s: Why are you doing this to me ?.\n", FuncName);
         break; 
   }      
      
   SUMA_RETURN(DrawnROI);
}

/*!
   Function that turns a brushstroke to a series of nodes on the surface.
   
   No surface paths are created from one node to the next yet.
   
   It is not always the case that BrushStroke->N_SurfNodes is equal to BrushStroke->N
*/
SUMA_Boolean SUMA_BrushStrokeToNodeStroke (SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_BrushStrokeToNodeStroke"};
   DList * NS=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_MT_INTERSECT_TRIANGLE *MTI = NULL;
   float delta_t_tmp; 
   struct timeval tt_tmp;
   int N = -1;
   SUMA_Boolean LocalHead=NOPE;
   SUMA_BRUSH_STROKE_DATUM *bsd=NULL, *obsd=NULL;
   DListElmt *Elmt = NULL, *oElmt=NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   
   SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
   /* ONLY WORK ON FocusSO */
   if (SO->FaceSetDim != 3) {
      fprintf(SUMA_STDERR,"Error %s: SUMA_MT_intersect_triangle only works for triangular meshes.\n", FuncName);
      SUMA_RETURN(NOPE);
   }
   
   N = dlist_size(sv->BS);
   if (!N) {
      fprintf (SUMA_STDERR, "%s: Empty brushstroke, nothing to do.\n", FuncName);
      SUMA_RETURN(NOPE);
   }else if (LocalHead) fprintf (SUMA_STDERR, "%s: %d element(s) in sv->BS.\n", FuncName, N);
   
   /* the first node of the brushstroke is stored as the cross hair's node id, just copy it */ 
   Elmt = dlist_head(sv->BS);
   bsd = (SUMA_BRUSH_STROKE_DATUM *)Elmt->data;
   
   bsd->SurfNode = SO->SelectedNode;
   bsd->SurfTri = SO->SelectedFaceSet;
   
   #ifdef DISASTER_LOOP 
      /* Now as a brute force method, do all the remaing nodes in the path. 
      In the future, you want to downsample the path is some clever fashion */
      if (N > 1) {
         if (LocalHead) {
            fprintf (SUMA_STDERR, "%s: Disaster loop, hold on .\n", FuncName);  
            SUMA_etime (&tt_tmp, 0);
         }

         MTI = NULL;
         do {
            Elmt = Elmt->next;
            bsd = (SUMA_BRUSH_STROKE_DATUM *)Elmt->data;

            MTI = SUMA_MT_intersect_triangle(   bsd->NP, bsd->FP, 
                                                SO->NodeList, SO->N_Node, 
                                                SO->FaceSetList, SO->N_FaceSet, 
                                                MTI);

            if (!MTI) {
               fprintf(SUMA_STDERR,"Error %s: SUMA_MT_intersect_triangle failed.\n", FuncName);
               SUMA_RETURN (NOPE);
            }

            if (MTI->N_hits) { /* There is a hit, store it if useful */
               oElmt = Elmt->prev;
               obsd = (SUMA_BRUSH_STROKE_DATUM *)oElmt->data;
               if (obsd->SurfNode != MTI->inodemin) { /* a new one, bring it on */
                  bsd->SurfNode = MTI->inodemin;
                  bsd->SurfTri = MTI->ifacemin;
               }else {
                  /* destroy Elmt, it is redundant */
                  if (LocalHead) fprintf (SUMA_STDERR, "%s: Removing redundant BS element.\n", FuncName);
                  dlist_remove (sv->BS, Elmt, (void*)(&bsd));
                  SUMA_FreeBSDatum (bsd);
                  Elmt = oElmt;
               }
            }

         }while (Elmt != dlist_tail(sv->BS));

         /* free MTI */
         MTI = SUMA_Free_MT_intersect_triangle(MTI); 

         if (LocalHead) {   
            delta_t_tmp = SUMA_etime (&tt_tmp, 1);
            if (LocalHead) fprintf (SUMA_STDERR, "Local Debug %s: Intersections took %f seconds.\n", FuncName, delta_t_tmp);
         }

      }
   #else
   if (N > 1) { /* new, faster method */   
      DListElmt *Eli=NULL, *Eln=NULL;
      float ip[3], d[3];
      int IncTri[100], N_IncTri=0, n1=-1, n2=-1, n3=-1, ni = -1, ti = -1, N_Neighb=0,DeciLevel = 0, i, j, Removed=0;
      int DeciReentry=0, UsedNode[3];
      SUMA_BRUSH_STROKE_DATUM *bsdi=NULL, *bsdn=NULL, *bsd_deci=NULL;
      SUMA_Boolean  DoesInters=NOPE; /* flag for Decimation mode */
      SUMA_Boolean  TrackOscillation = YUP; /* flag to tracking algorithm oscillation */
      SUMA_Boolean  TryBruteForce = NOPE;
      int *Visited = NULL;
      
      if (TrackOscillation) {
         Visited = (int *)SUMA_calloc(SO->N_Node, sizeof(int)); 
         if (!Visited) {
            SUMA_SLP_Err("Failed to allocate for Visited.\n");
            SUMA_RETURN(NOPE);
         }
      }
      
      Eli = Elmt; /* initialize current element to the very fist in the brushstroke */
      MTI = NULL;
      TryBruteForce = NOPE;
      do {   
         bsdi = (SUMA_BRUSH_STROKE_DATUM *)Eli->data;
         n1 = bsdi->SurfNode;
         
         Eln = Eli->next; /* get the next element in line */
         bsdn = (SUMA_BRUSH_STROKE_DATUM *)Eln->data;
         
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Working from node %d.\n", FuncName, n1);

         if (!TryBruteForce) { /* try the fast method */
            N_Neighb = SO->FN->N_Neighb[n1];
            if (N_Neighb < 3) {
               /* nothing found */
               SUMA_SLP_Err ("Node has less than 3 neighbors.\n.This method will not apply.");
               SUMA_RETURN(NOPE);
            }

            /* does the ray formed by Eln's NP and FP hit any of the triangles incident to bsdi->SurfNode (or n1) ? */
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Searching incident triangles:\n", FuncName);
            i=0;
            DoesInters = NOPE;
            while ((i < N_Neighb ) && (!DoesInters)) { 
               n2 = SO->FN->FirstNeighb[n1][i];
               if ( i+1 == N_Neighb) n3 = SO->FN->FirstNeighb[n1][0];
               else n3 = SO->FN->FirstNeighb[n1][i+1];
               #if 0
                  if (LocalHead) {
                     fprintf (SUMA_STDERR, " %d: [%d %d %d] Tri %d\n", i, n1, n2, n3, SUMA_whichTri(SO->EL, n1, n2, n3));
                     fprintf (SUMA_STDERR, " %d: [%.2f, %.2f, %.2f]\n", 
                                             n1, SO->NodeList[3*n1], SO->NodeList[3*n1+1], SO->NodeList[3*n1+2]);
                     fprintf (SUMA_STDERR, " %d: [%.2f, %.2f, %.2f]\n", 
                                             n2, SO->NodeList[3*n2], SO->NodeList[3*n2+1], SO->NodeList[3*n2+2]);
                     fprintf (SUMA_STDERR, " %d: [%.2f, %.2f, %.2f]\n", 
                                             n3, SO->NodeList[3*n3], SO->NodeList[3*n3+1], SO->NodeList[3*n3+2]);                        
                     fprintf (SUMA_STDERR, " NP: [%.2f, %.2f, %.2f] FP: [%.3f, %.2f, %.2f]\n", 
                                             bsdn->NP[0], bsdn->NP[1], bsdn->NP[2], 
                                             bsdn->FP[0], bsdn->FP[1], bsdn->FP[2]);
                  }
               #endif
               DoesInters = SUMA_MT_isIntersect_Triangle (bsdn->NP, bsdn->FP, 
                                                          &(SO->NodeList[3*n1]), &(SO->NodeList[3*n2]), &(SO->NodeList[3*n3]),
                                                          ip, d, &ni);
               if (DoesInters) {
                  if (ni == 0) ni = n1;
                  else if (ni == 1) ni = n2;
                  else ni = n3;

                  ti = SUMA_whichTri(SO->EL, n1, n2, n3);
               }

               #if 0
                  if (LocalHead) fprintf (SUMA_STDERR, "%s: DoesInters = %d, ni = %d\n", FuncName, DoesInters, ni);
                  {
                     /* for debuging */                                           
                     MTI = NULL;MTI = SUMA_MT_intersect_triangle(   bsdn->NP, bsdn->FP, 
                                                      SO->NodeList, SO->N_Node, 
                                                      SO->FaceSetList, SO->N_FaceSet, 
                                                      MTI);
                     fprintf (SUMA_STDERR, "%s: Intersection would be with triangle %d, node %d\n", FuncName, MTI->ifacemin, MTI->inodemin);                                 
                  }
               #endif
               ++i;
            } 
            if (LocalHead) fprintf (SUMA_STDERR, "\n");
            
         } else  { /* try brute force flag has been set*/
         
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Trying brute force here \n", FuncName);
            /* Now skip and remove decimated elements */
            SUMA_REMOVE_NEXT_NON_DECIMATED (sv->BS, Eli, Eln);
            DeciLevel = 0;
            DeciReentry = 0;
            if (!Eln) {
             SUMA_SL_Err ("I tried hard to figure out your trace.\nI failed, now you try again.");
             SUMA_RETURN(YUP);
            }

            bsdn = (SUMA_BRUSH_STROKE_DATUM *)Eln->data;
            MTI = SUMA_MT_intersect_triangle(   bsdn->NP, bsdn->FP, 
                                    SO->NodeList, SO->N_Node, 
                                    SO->FaceSetList, SO->N_FaceSet, 
                                    MTI);

            if (!MTI) {
               SUMA_SL_Err ("I tried harder to figure out your trace.\nI failed, do try again.");
               SUMA_RETURN (YUP);
            }

            if (MTI->N_hits) { /* There is a hit, store it if useful */
               DoesInters = YUP;
               if (bsdi->SurfNode != MTI->inodemin) { /* a new one, bring it on */
                  if (LocalHead) fprintf (SUMA_STDERR, "%s: Brute Force: Found intersection at new node %d.\n", FuncName, MTI->inodemin);
                  ni = MTI->inodemin;
                  ti = MTI->ifacemin;
               }else {
                  /* set ni to n1 and let the element be destroyed */
                  if (LocalHead) fprintf (SUMA_STDERR, "%s: Brute Force: Found intersection at n1 = %d!.\n", FuncName, MTI->inodemin);
                  ni = n1;
               }
            } else {
               /* No hits at all, get out of this business */
               SUMA_SL_Err ("Why are you drawing out of bounds ?");
               SUMA_RETURN (YUP);
            }
            /* reset the TryBruteForce flag */
            TryBruteForce = NOPE;
         }
         
         if (!DoesInters) { /* no intersection found, insert an element between Eli and Eln and try again */
            ++DeciLevel;
            if (LocalHead) fprintf (SUMA_STDERR, "%s: No intersection found. Decimating, level %d.\n", FuncName, DeciLevel);
            
            if (DeciLevel > 3000) { /* this condition is only here to keep things from going awry. */
               if (LocalHead) fprintf (SUMA_STDERR,"%s: Decimation method failed. Trying from brute force", FuncName);
               TryBruteForce = YUP;
            } else {
               bsd_deci = SUMA_CreateBSDatum();
               bsd_deci->Decimated = YUP;
               bsd_deci->x = (bsdi->x + bsdn->x)/2.0;
               bsd_deci->y = (bsdi->y + bsdn->y)/2.0;
               for (j=0; j < 3; ++j) bsd_deci->NP[j] = (bsdi->NP[j] + bsdn->NP[j])/2.0;
               for (j=0; j < 3; ++j) bsd_deci->FP[j] = (bsdi->FP[j] + bsdn->FP[j])/2.0;
               bsd_deci->SurfNode = -1;
               bsd_deci->SurfTri = -1;

               dlist_ins_next (sv->BS, Eli, bsd_deci);
            }
         } else {
            /* intersection found */
            if (ni == n1 && !DeciLevel) { 
               /* same node reached, not during decimation, perhaps path was too densely sampled, delete Eln */
               ++Removed;
               if (LocalHead) fprintf (SUMA_STDERR, "%s: Same node reached without decimation, deleting for the %dth time.\n",
                   FuncName, Removed);
               dlist_remove(sv->BS, Eln, (void*)&bsdn);
               SUMA_FreeBSDatum(bsdn);
            } else {
               if (ni == n1 && DeciLevel) {
                  /* back to the starting point during decimation  */
                  if (DeciLevel) { /* user went out of bounds or drawing over cuts in surface */
                     #if 0
                        /* same node reached during decimation, to hell with it, use brute force intersection: YOU PAY PRICE IN TIME MISTER*/
                        TryBruteForce = YUP;
                        /* cancel the find */
                        DoesInters = NOPE;
                     #else
                        /* same node reached during decimation, try othernode in triangle */
                        if (!DeciReentry) {
                           UsedNode[0] = n1;
                           if (LocalHead) fprintf (SUMA_STDERR, "%s: Same node reached during decimation.\n Switching to second closest node.\n", FuncName);
                           if (d[1] < d[2]) {
                              ni = n2;
                              UsedNode[1] = n2;
                           } else {
                              ni = n3;
                              UsedNode[1] = n3;
                           }
                        } else if (DeciReentry == 1){
                           /* been there before, one last node is left */
                           if (LocalHead) fprintf (SUMA_STDERR, "%s: Last chance!\n", FuncName);
                           if (n2 != UsedNode[0] && n2 != UsedNode[1]) ni = n2; 
                           else if (n3 != UsedNode[0] && n3 != UsedNode[1]) ni = n3; 
                        } else {
                           /* Decimation failed, Do intersection with entire surface */
                           TryBruteForce = YUP;
                           /* cancel the find */
                           DoesInters = NOPE;
                        }
                     #endif
                     ++DeciReentry;
                  }
               }else {
                  /* ni != n1  Reset DeciLevel */
                  DeciLevel = 0;
                  DeciReentry = 0;
               }

               /* algorithm might fall into oscillatory patterns, keep track of nodes visited. 
                  It is possible that a node is visited multiple times when users go over the 
                  same region over and over and over.*/
               if (TrackOscillation) {
                  ++Visited[ni];
                  if (Visited[ni] == 9) {
                     DoesInters = NOPE;
                     TryBruteForce = YUP;
                     SUMA_SL_Err ("Path tracing oscillation. Trying with brute force.");
                  }
                  if (Visited[ni] > 9) {
                     SUMA_SL_Err ("Path tracing oscillation remaining. Quitting tracing.");
                     SUMA_RETURN(YUP);
                  }
               }

               if (DoesInters) { /* it is possible that the find is cancelled */
                  if (LocalHead) fprintf (SUMA_STDERR, "%s: Found new node.\n", FuncName);
                  /* good, new node is useful*/
                  bsdn->SurfNode = ni;
                  bsdn->SurfTri = ti;
                  /* set Eli to Eln */
                  Eli = Eln; 
               }
            }
         }
         /* repeat until you have no more element */
      } while (Eli != dlist_tail(sv->BS));
       
      if (MTI) MTI = SUMA_Free_MT_intersect_triangle(MTI); 
      if (TrackOscillation) {
         if (Visited) SUMA_free(Visited);
      }
   }/* new, faster method */   
   #endif
   SUMA_RETURN(YUP);
}


/*!
   \brief Function to link a node on the surface to a certain node in NodeStroke
   
   \param sv (SUMA_SurfaceViewer *) with a valid BrushStroke in it
   \param NonSurf (int) index of node on surface to connect to NinStroke
   \param ELinStroke (DListElmt *) sv->BS element containing in SurfNode the index of the node  to connect NonSurf to   
   \sa SUMA_LinkTailNodeToNodeStroke
*/
SUMA_ROI_DATUM *SUMA_LinkThisNodeToNodeInStroke (SUMA_SurfaceViewer *sv, int NonSurf, DListElmt *ELinStroke)
{
   static char FuncName[]={"SUMA_LinkThisNodeToNodeInStroke"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ROI_DATUM *ROId=NULL;
   SUMA_SurfaceObject *SO=NULL;
   int Nfrom, Nto;
   SUMA_BRUSH_STROKE_DATUM *bsd=NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
   
   Nfrom = NonSurf;
   bsd = (SUMA_BRUSH_STROKE_DATUM *)ELinStroke->data;
   Nto = bsd->SurfNode;
   
   /* Now compute the intersection of the surface with the plane */
   ROId = SUMA_Surf_Plane_Intersect_ROI (SO, Nfrom, Nto, bsd->NP);
   
   if (!ROId) {
      fprintf (SUMA_STDERR, "Error %s: Failed to link tail node to first node in new stroke. Repeat new stroke.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   
   SUMA_RETURN(ROId);
}

/*!
   \brief Function to link a node on the surface to the first node of a NodeStroke
   
   -This function returns an ROI_datum that represents the link between the last node visited and 
   the first node of the Nodestroke
   
   \sa SUMA_LinkThisNodeToNodeInStroke 
*/
SUMA_ROI_DATUM *SUMA_LinkTailNodeToNodeStroke (SUMA_SurfaceViewer *sv, SUMA_DRAWN_ROI *DrawnROI)
{
 
   static char FuncName[]={"SUMA_LinkTailNodeToNodeStroke"};
   SUMA_ROI_DATUM *ROId=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   int Nfrom=-1, Nto=-1, Trito=-1;
   DListElmt *Elm=NULL;
   SUMA_BRUSH_STROKE_DATUM *bsd=NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
   
   /* get the equation of the plane fromed by TailNode, FirstNodeinBrushStroke and its NearPlanePoint */
   SUMA_DRAWN_ROI_TAIL_NODE(DrawnROI, Nfrom);
   if (Nfrom < 0) {
      fprintf (SUMA_STDERR, "Error %s: No tail node found.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   
   /* get the first node in the stroke */
   SUMA_BS_FIRST_SURF_NODE(sv->BS, Nto, Trito, Elm);
   if (Nto < 0 || !Elm) {
      SUMA_SLP_Err ("Failed in SUMA_BS_FIRST_SURF_NODE macro.");
      SUMA_RETURN(NULL); 
   }
   bsd = (SUMA_BRUSH_STROKE_DATUM *)Elm->data;
   
   /* Now compute the intersection of the surface with the plane */
   ROId = SUMA_Surf_Plane_Intersect_ROI (SO, Nfrom, Nto, bsd->NP);
   
   if (!ROId) {
      fprintf (SUMA_STDERR, "Error %s: Failed to link tail node to first node in new stroke. Repeat new stroke.\n", FuncName);
      SUMA_RETURN(NULL);
   }
   
   SUMA_RETURN(ROId);
}


/*!
   This function turns the NodeStroke into a datum of connected nodes
*/
SUMA_ROI_DATUM *SUMA_NodeStrokeToConnectedNodes (SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_NodeStrokeToConnectedNodes"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ROI_DATUM *ROId=NULL, *ROIlink = NULL;
   int i=0;
   SUMA_SurfaceObject *SO=NULL;
   DListElmt *Elmt = NULL, *oElmt = NULL;
   SUMA_BRUSH_STROKE_DATUM *bsd=NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   
   ROId = SUMA_AllocROIDatum();

   /* fill up node series here */
   ROId->N_n = 1;
   ROId->N_t = 1;
   ROId->nPath = (int *) SUMA_calloc (ROId->N_n, sizeof(int));
   ROId->tPath = (int *) SUMA_calloc (ROId->N_t, sizeof(int));
   
   SUMA_BS_FIRST_SURF_NODE(sv->BS, ROId->nPath[0], ROId->tPath[0], Elmt);
   ROId->Type = SUMA_ROI_NodeSegment;
   
   /* try filling up the rest */
   SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
   oElmt = Elmt;
   do {
      /* get the next element with a surfnode */
     SUMA_BS_NEXT_SURF_NODE(sv->BS, oElmt, Elmt);
    
     if (!Elmt) {
      /* perhaps reached end of list without success */
      SUMA_S_Note("Reached EOL without finding Elmt.\nNot necessarily a bad thing.");
      SUMA_RETURN(ROId);
     } else {
      if (LocalHead) {
         fprintf (SUMA_STDERR, "%s: Working with element %p.\n", FuncName, Elmt);
      }
     }
     bsd = (SUMA_BRUSH_STROKE_DATUM *)Elmt->data;
     if (LocalHead) fprintf (SUMA_STDERR, "%s: %d %d\nWill look for edge %d %d\n", 
                     FuncName, ROId->N_n, bsd->SurfNode,
                     ROId->nPath[ROId->N_n-1], bsd->SurfNode);
     if (SUMA_FindEdge(SO->EL, ROId->nPath[ROId->N_n-1], bsd->SurfNode) < 0) {
         /* Not found, link nodes together*/
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Edge not found, linking together.\n", FuncName);
         if (!(ROIlink = SUMA_LinkThisNodeToNodeInStroke (sv, ROId->nPath[ROId->N_n-1],  Elmt))) {
            SUMA_SLP_Err ("Failed to connect nodes in stroke.");
            SUMA_RETURN (ROId);
         }
         /* merge ROIlink with ROId */
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Merging ROIs together.\n", FuncName);
         if (!SUMA_AppendToROIdatum (ROIlink, ROId)) {
               SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                  "Failed to merge ROIs.", FuncName,
                                  SMT_Critical, SMA_LogAndPopup);
               if (ROIlink) SUMA_FreeROIDatum((void *)ROIlink); ROIlink = NULL;
               SUMA_RETURN(ROId);   
         }
         if (ROIlink) SUMA_FreeROIDatum((void *)ROIlink); ROIlink = NULL;
      }else {
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Nodes connected.\n", FuncName);
         /* nodes are connected, just add to path */
         ++ROId->N_n;
         ROId->nPath = (int *) SUMA_realloc (ROId->nPath, sizeof(int)*ROId->N_n);
         ROId->nPath[ROId->N_n-1] = bsd->SurfNode;
      }     
   
      oElmt = Elmt;
   } while (Elmt != dlist_tail(sv->BS));
      
   SUMA_RETURN (ROId);
}


/*!
Executes an action
Adds it to the action stack
Update the action stack pointer 
DO not do StckPos = SUMA_PushActionStack (..., StackPos, ....);
that is because the function might return NULL if something failed and you'd lose the current stack position for good.
*/
DListElmt * SUMA_PushActionStack (DList *ActionStack, DListElmt *StackPos, 
                                  SUMA_ACTION_RESULT (*ActionFunction)(void *ActionData, SUMA_ACTION_POLARITY Pol), 
                                  void *ActionData, 
                                  void (*ActionDataDestructor)(void *Actiondata))
{
   static char FuncName[]={"SUMA_PushActionStack"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ACTION_STACK_DATA *AS_data=NULL;
   SUMA_ACTION_RESULT ActionResult = SAR_Undefined;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* execute action */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Executing Action.\n", FuncName);
   ActionResult = ActionFunction (ActionData, SAP_Do);
   switch (ActionResult) {
      case SAR_Fail:
         SUMA_SLP_Err("Action failed.");
         SUMA_RETURN(NULL);
         break;
      case SAR_Succeed:
         break;
      default:
         SUMA_SLP_Err("Action result not understood.");
         SUMA_RETURN(NULL);
         break;
   }
   
   /* remove all elements above the position in the stack */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Removing Action Stack Elements above current position.\n", FuncName);
   while (StackPos != dlist_tail(ActionStack)) {
      void *asdata=NULL;
      
      dlist_remove(ActionStack, dlist_tail(ActionStack), &asdata);
      
      /* now free the asdata */
      SUMA_FreeActionStackData(asdata);
   }
   
   /* Pushing the action and its data onto the stack */
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Pushing the action and its data onto the stack.\n", FuncName);
   AS_data = (SUMA_ACTION_STACK_DATA *)SUMA_malloc (sizeof(SUMA_ACTION_STACK_DATA));
   AS_data->ActionDataDestructor = ActionDataDestructor;
   AS_data->ActionFunction = ActionFunction;
   AS_data->ActionData = ActionData;
   dlist_ins_next (ActionStack, dlist_tail(ActionStack), (void*)AS_data);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Updating StackPos ...\n", FuncName);
   StackPos = dlist_tail(ActionStack);
   
   SUMA_RETURN(StackPos);
}

/*!
   Redo an action of the ActionStack
   If StackPos is NULL, it is assumed that you are at the bottom of the stack 
*/
DListElmt * SUMA_RedoAction (DList *ActionStack, DListElmt *StackPos)
{
   static char FuncName[]={"SUMA_RedoAction"};
   SUMA_ACTION_STACK_DATA *AS_data=NULL;
   SUMA_ACTION_RESULT ActionResult = SAR_Undefined;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!StackPos) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: At bottom of stack. Working up.\n", FuncName);
      StackPos = dlist_head(ActionStack);
   } else if (StackPos == dlist_tail(ActionStack)) {
      SUMA_SLP_Err("At top of stack, nothing to do.");
      SUMA_RETURN(StackPos);
   } else {
      StackPos = dlist_next(StackPos);
   }
   
   /* execute action above StackPos again */
   AS_data = (SUMA_ACTION_STACK_DATA *)StackPos->data;
   ActionResult = AS_data->ActionFunction (AS_data->ActionData, SAP_Redo);
   switch (ActionResult) {
      case SAR_Fail:
         SUMA_SLP_Err("Action failed.");
         SUMA_RETURN(NULL);
         break;
      case SAR_Succeed:
         break;
      default:
         SUMA_SLP_Err("Action result not understood.");
         SUMA_RETURN(NULL);
         break;
   }
   
   SUMA_RETURN(StackPos);   
}   
/*!
   Undo an action on the ActionStack
   
   \returns StackNewPos = StackPos if reached the bottom of the stack. 
                     It is your job to make sure this function is not called again.
                        = StackPos->prev if all went well
                        =NULL if trouble
   
*/
DListElmt * SUMA_UndoAction (DList *ActionStack, DListElmt *StackPos)
{
   static char FuncName[]={"SUMA_UndoAction"};
   SUMA_ACTION_STACK_DATA *AS_data=NULL;
   SUMA_ACTION_RESULT ActionResult = SAR_Undefined;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   if (!StackPos) {
      SUMA_SLP_Err("At bottom of stack.");
      SUMA_RETURN(StackPos);
   }
   
   AS_data = (SUMA_ACTION_STACK_DATA *)StackPos->data;
   
   /* execute reverse of action */
   ActionResult = AS_data->ActionFunction (AS_data->ActionData, SAP_Undo);
   switch (ActionResult) {
      case SAR_Fail:
         SUMA_SLP_Err("Action failed.");
         SUMA_RETURN(NULL);
         break;
      case SAR_Succeed:
         break;
      default:
         SUMA_SLP_Err("Action result not understood.");
         SUMA_RETURN(NULL);
         break;
   }
   
   /* now move StackPos down */
   if (StackPos == dlist_head(ActionStack)) {
      /* do nothing to StackPos */
   } else {
      StackPos = dlist_prev(StackPos);
   }
   
   SUMA_RETURN(StackPos);
}

/*!
   \brief Mark an ROI as finished
*/
SUMA_ACTION_RESULT SUMA_FinishedROI (void *data, SUMA_ACTION_POLARITY Pol)
{
   static char FuncName[]={"SUMA_FinishedROI"};
   SUMA_ROI_ACTION_STRUCT *ROIA=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   ROIA = (SUMA_ROI_ACTION_STRUCT *)data;
   
   switch (Pol) {
      case SAP_Do:
      case SAP_Redo:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Marking as finished...\n", FuncName);
         /* set the drawing status */
         ROIA->DrawnROI->DrawStatus = SUMA_ROI_Finished;
         break;
      case SAP_Undo:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Marking as InCreation...\n", FuncName);
         ROIA->DrawnROI->DrawStatus = SUMA_ROI_InCreation;
         break;
      default:
         fprintf (SUMA_STDERR, "Error %s: Should not be here.\n", FuncName);
         break;
   }
   
   SUMA_RETURN(SAR_Succeed);
}

/*!
   \brief This function is like SUMA_AddToTailROIDatum, except that it also updates the type of the ROI
   to be a filled one. You call this function when you are adding an ROIDatum that fills a closed  path
   \param data (void *) of SUMA_ROI_ACTION_STRUCT * containing the ROIlist and the ROIdatum
   \param Pol (SUMA_ACTION_POLARITY) SAP_Do, SAP_Redo, SAP_Undo
*/

SUMA_ACTION_RESULT SUMA_AddFillROIDatum (void *data, SUMA_ACTION_POLARITY Pol)
{
   static char FuncName[]={"SUMA_AddFillROIDatum"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ROI_ACTION_STRUCT *ROIA=NULL;
   void *eldata=NULL;
   DListElmt *tail_elm=NULL;
   SUMA_ROI_DATUM *ROId=NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   ROIA = (SUMA_ROI_ACTION_STRUCT *)data;
   
   switch (Pol) {
      case SAP_Do:
      case SAP_Redo:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Adding to ROIstrokelist...\n", FuncName);
         dlist_ins_next(ROIA->DrawnROI->ROIstrokelist, dlist_tail(ROIA->DrawnROI->ROIstrokelist), (void *)ROIA->ROId);
         ROIA->DrawnROI->Type = SUMA_ROI_FilledArea;
         break;
      case SAP_Undo:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Removing from ROIstrokelist...\n", FuncName);
         dlist_remove(ROIA->DrawnROI->ROIstrokelist, dlist_tail(ROIA->DrawnROI->ROIstrokelist), &eldata);
         /* eldata contains the ROIdatum that has been removed from the list. It should not be freed here 
         This structure is to remain in the stack for later usage.*/
         /* if the tail is a segment, then turn the ROI type to a closedpath */
         tail_elm = dlist_tail(ROIA->DrawnROI->ROIstrokelist);
         ROId = (SUMA_ROI_DATUM *)tail_elm->data;
         if (ROId->Type == SUMA_ROI_NodeSegment) { /* we are no longer dealing with filled ROI */
            ROIA->DrawnROI->Type = SUMA_ROI_ClosedPath;
         }
         break;
      default:
         fprintf (SUMA_STDERR, "Error %s: Should not be here.\n", FuncName);
         break;
   }
   
   SUMA_RETURN(SAR_Succeed);
}

/*!
   \brief This function is like SUMA_AddToTailROIDatum, except that it also updates the type of the ROI
   to be a closed one. You call this function when you are addind the last ROIDatum that closes the path
   \param data (void *) of SUMA_ROI_ACTION_STRUCT * containing the ROIlist and the ROIdatum
   \param Pol (SUMA_ACTION_POLARITY) SAP_Do, SAP_Redo, SAP_Undo
*/
SUMA_ACTION_RESULT SUMA_AddToTailJunctionROIDatum (void *data, SUMA_ACTION_POLARITY Pol)
{
   static char FuncName[]={"SUMA_AddToTailJunctionROIDatum"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ROI_ACTION_STRUCT *ROIA=NULL;
   void *eldata=NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   ROIA = (SUMA_ROI_ACTION_STRUCT *)data;
   
   switch (Pol) {
      case SAP_Do:
      case SAP_Redo:
         if (ROIA->DrawnROI->Type == SUMA_ROI_ClosedPath) {
            SUMA_SLP_Err ("Trying to close a closed path!");
            SUMA_RETURN(SAR_Fail);
         }
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Adding to ROIstrokelist...\n", FuncName);
         dlist_ins_next(ROIA->DrawnROI->ROIstrokelist, dlist_tail(ROIA->DrawnROI->ROIstrokelist), (void *)ROIA->ROId);
         ROIA->DrawnROI->Type = SUMA_ROI_ClosedPath;
         break;
      case SAP_Undo:
         if (ROIA->DrawnROI->Type == SUMA_ROI_OpenPath) {
            SUMA_SLP_Err ("Trying to open an open path!");
            SUMA_RETURN(SAR_Fail);
         }
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Removing from ROIstrokelist...\n", FuncName);
         dlist_remove(ROIA->DrawnROI->ROIstrokelist, dlist_tail(ROIA->DrawnROI->ROIstrokelist), &eldata);
         /* eldata contains the ROIdatum that has been removed from the list. It should not be freed here 
         This structure is to remain in the stack for later usage.*/
         ROIA->DrawnROI->Type = SUMA_ROI_OpenPath;
         break;
      default:
         fprintf (SUMA_STDERR, "Error %s: Should not be here.\n", FuncName);
         break;
   }
   
   SUMA_RETURN(SAR_Succeed);
}


/*!
   \brief Adds (or removes) an ROIdatum to the tail of the ROI list 
   
   \param data (void *) of SUMA_ROI_ACTION_STRUCT * containing the ROIlist and the ROIdatum
   \param Pol (SUMA_ACTION_POLARITY) SAP_Do, SAP_Redo, SAP_Undo
*/
SUMA_ACTION_RESULT SUMA_AddToTailROIDatum (void *data, SUMA_ACTION_POLARITY Pol)
{
   static char FuncName[]={"SUMA_AddToTailROIDatum"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ROI_ACTION_STRUCT *ROIA=NULL;
   void *eldata=NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   ROIA = (SUMA_ROI_ACTION_STRUCT *)data;
   
   switch (Pol) {
      case SAP_Do:
      case SAP_Redo:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Adding to ROIstrokelist...\n", FuncName);
         dlist_ins_next(ROIA->DrawnROI->ROIstrokelist, dlist_tail(ROIA->DrawnROI->ROIstrokelist), (void *)ROIA->ROId);
         break;
      case SAP_Undo:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Removing from ROIstrokelist...\n", FuncName);
         dlist_remove(ROIA->DrawnROI->ROIstrokelist, dlist_tail(ROIA->DrawnROI->ROIstrokelist), &eldata);
         /* eldata contains the ROIdatum that has been removed from the list. It should not be freed here 
         This structure is to remain in the stack for later usage.*/
         break;
      default:
         fprintf (SUMA_STDERR, "Error %s: Should not be here.\n", FuncName);
         break;
   }
   
   SUMA_RETURN(SAR_Succeed);
}

/*!
   A function to destroy the ROI action data structure.
   
   \param data (void *) of SUMA_ROI_ACTION_STRUCT * containing the ROIlist and the ROIdatum

   -  Only  ROIA->ROId and ROIA are freed. ROIA->DrawnROI should be freed when the DrawnROI list is destroyed. 
*/

void SUMA_DestroyROIActionData (void *data)
{  
   static char FuncName[]={"SUMA_DestroyROIActionData"};
   SUMA_Boolean LocalHead = YUP;
   SUMA_ROI_ACTION_STRUCT *ROIA=NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   ROIA = (SUMA_ROI_ACTION_STRUCT *)data;
   
   if (!ROIA) SUMA_RETURNe;
   
   if (ROIA->ROId) { /* free the ROI datum */
      SUMA_FreeROIDatum ((void *)ROIA->ROId);
      ROIA->ROId = NULL;
   }
   
   ROIA->DrawnROI = NULL; /* this should not be freed here, it is freed when the function for destroying DrawnROI is called */
   SUMA_free(ROIA);
   
   SUMA_RETURNe;
}

/*! 
   \brief set the position of light0
   \param s (char *) a strng containing X, Y, Z coordinates
   \param data (void *) a typecast of the pointer to the surface viewer to be affected

*/
void SUMA_SetLight0 (char *s, void *data)
{
   static char FuncName[]={"SUMA_SetLight0"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   float fv3[3];
   SUMA_Boolean LocalHead = YUP; 

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, fv3, 3) != 3) { /* problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }

   /* register fv3 with ED */
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_SetLight0Pos);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3, (void *)fv3, 
                                          SES_Suma, (void *)sv, NOPE, 
                                          SEI_Tail, NULL )) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      SUMA_RETURNe;
   }
   
   SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);

   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }

   SUMA_RETURNe;
}
/*!
   \brief rotates surface to face a certain coordinate 

   \param s (char *) a strng containing X, Y, Z coordinates
   \param data (void *) a typecast of the pointer to the surface viewer to be affected

*/
void SUMA_LookAtCoordinates (char *s, void *data)
{
   static char FuncName[]={"SUMA_LookAtCoordinates"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   float fv3[3];
   SUMA_Boolean LocalHead = YUP; 

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, fv3, 3) != 3) { /* problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }

   /* register fv3 with ED */
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_SetLookAt);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3, (void *)fv3, 
                                          SES_Suma, (void *)sv, NOPE, 
                                          SEI_Head, NULL )) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      SUMA_RETURNe;
   }
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }

   SUMA_RETURNe;
}
