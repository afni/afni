#define DEBUG_1
#ifdef DEBUG_1
   #define DEBUG_2
   #define DEBUG_3
#endif
   
/* Header FILES */
   
#include "SUMA_suma.h"

/* extern SUMA_SurfaceViewer *SUMAg_cSV; */   /* no longer used Tue Aug 13 16:07:41 EDT 2002 */
extern SUMA_DO *SUMAg_DOv;   
extern int SUMAg_N_DOv; 
extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv;

/*!
   \brief This is the function that runs the viewers. 
   success = SUMA_Engine (listp);
   
   \param listp (DList **) pointer to doubly linked list pointer containing Engine commands
   
   \return success (YUP/NOPE)
   
   - *listp is destroyed just before SUMA_Engine returns and *listp is set to null
   - To add a new command:
   include it SUMA_define.h in SUMA_ENGINE_CODE's typedef
   include it in SUMA_ParseCommands.c, SUMA_CommandCode, SUMA_CommandString functions 
   - OLD Format: SUMA_Boolean SUMA_Engine (char *Command, SUMA_EngineData *EngineData, SUMA_SurfaceViewer *sv)
      
*/

SUMA_Boolean SUMA_Engine (DList **listp)
{
   char tmpcom[SUMA_MAX_COMMAND_LENGTH], sfield[100], sdestination[100];
   const char *NextCom;
   static char FuncName[]={"SUMA_Engine"};
   int NextComCode, ii, i, id, ND, ip, NP;
   SUMA_SurfaceObject *SO = NULL;
   float delta_t;
   struct  timeval tt;
   int it, Wait_tot, nn=0, N_SOlist, SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS];
   float ft, **fm, fv15[15];
   XtPointer elvis=NULL;
   NI_element *nel;
   SUMA_Boolean Found, LocalHead = NOPE;
   SUMA_SurfaceViewer *svi;
   SUMA_SurfaceViewer *sv = NULL;
   static char Command[]={"OBSOLETE-since:Thu Jan 23 16:55:03 EST 2003"};
   SUMA_EngineData *EngineData=NULL, *ED = NULL; /* EngineData is what get passed from a list element, 
                                                   ED is what gets added to the list inside SUMA_Engine */ 
   DListElmt *NextElem_CANT_TOUCH_THIS, *LocElm=NULL;
   DList *list= NULL;
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell = NULL, *LogShell=NULL;

   /*int iv3[3], iv15[15], **im;
   float fv3[3];
   char s[SUMA_MAX_STRING_LENGTH];*/ /* keep standard unused variables undeclared, else compiler complains*/
   
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   list = *listp; /* listp is now passed instead of list so that I can set list to NULL from within this function */
   
   if (!list) {
      fprintf (SUMA_STDERR, "Error %s: Nothing to do.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: ", FuncName);
   while (list->size) {/* cycle through NextComs */
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Fetching next element\n", FuncName);
     /* get the next command from the head of the list */
      NextElem_CANT_TOUCH_THIS = dlist_head(list);
      EngineData = (SUMA_EngineData *)NextElem_CANT_TOUCH_THIS->data;
      
      /* decide on what Srcp might be. Currently only sv is passed when the source is Suma*/
      sv = NULL;
      switch (EngineData->Src) {
         case SES_Suma: 
         case SES_SumaFromAfni:
         case SES_SumaWidget:
         case SES_SumaFromAny:
            sv = (SUMA_SurfaceViewer *)EngineData->Srcp;
         case SES_Afni:
            break;
         default:  
      } 
      
      NextComCode = EngineData->CommandCode;
      if (!NextComCode) {
         fprintf (stderr, "%s Error: Bad next element code\n", FuncName);
         SUMA_RETURN (NOPE);
      } 
      NextCom = SUMA_CommandString (NextComCode);
      if (LocalHead) fprintf (SUMA_STDERR,"->%s<-\t", NextCom);
      switch (NextComCode) {/* switch NextComCode */
         case SE_SendColorMapToAfni:
            /* expects in i the code of one of SUMA's standard colormaps */
            {
               SUMA_COLOR_MAP *cmap;
               NI_element *nel=NULL;
               NI_stream ns;
               int i;
               char sbuf[50], *stmp=NULL;
               
               if (EngineData->i_Dest != NextComCode ) {
                  fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n", \
                   FuncName, NextCom, NextComCode);
                  break;
               }
               
               /* get the CMAP */
               if (!(cmap = SUMA_GetStandardMap (EngineData->i))) {
                  SUMA_SLP_Err("Failed to create colormap");
                  break;
               }
               
               if (cmap->N_Col > 128) {
                  SUMA_SLP_Err(  "Cannot send more\n"
                                 "than 128 colors to\n"
                                 "AFNI.");
                  SUMA_Free_ColorMap(cmap); cmap = NULL;
                  break;
               }
               
               /* send AFNI the color map */
               nel = NI_new_data_element("ni_do", 0);
               NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
               stmp = SUMA_append_string("DEFINE_COLORSCALE ", cmap->Name);
               for (i=0; i < cmap->N_Col; ++i) {
                  sprintf(sbuf,"rgbi:%f/%f/%f", 
                           cmap->M[i][0], cmap->M[i][1], cmap->M[i][2]);
                  stmp = SUMA_append_replace_string(stmp, sbuf, " ", 1);
               }
               SUMA_LH(stmp);
               NI_set_attribute ( nel, "ni_object", stmp);
               
               /* SUMA_ShowNel(nel); */
               
               if (NI_write_element( SUMAg_CF->ns , nel, NI_BINARY_MODE ) < 0) {
                  SUMA_SLP_Err("Failed to send CMAP to afni");
                  NI_free_element(nel) ; nel = NULL;
                  if (stmp) free(stmp);
                  SUMA_Free_ColorMap(cmap); cmap = NULL;
                  break;
               }
               
               NI_free_element(nel) ; nel = NULL;
               if (stmp) free(stmp);
               
               /* Now set the colormap in AFNI */
               nel = NI_new_data_element("ni_do", 0);
               NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
               stmp = SUMA_append_string("SET_PBAR_ALL ", "A.+99");
               sprintf(sbuf, " 1 %s", cmap->Name);
               stmp = SUMA_append_replace_string(stmp, sbuf, "", 1);
               NI_set_attribute ( nel, "ni_object", stmp);
               
               /* SUMA_ShowNel(nel); */
               
               if (NI_write_element( SUMAg_CF->ns , nel, NI_BINARY_MODE ) < 0) {
                  SUMA_SLP_Err("Failed to send CMAP to afni");
                  NI_free_element(nel) ; nel = NULL;
                  if (stmp) free(stmp);
                  SUMA_Free_ColorMap(cmap); cmap = NULL;
                  break;
               }
               
               NI_free_element(nel) ; nel = NULL;
               if (stmp) free(stmp);
              
               /* set the autorange off */
               nel = NI_new_data_element("ni_do", 0);
               NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
               NI_set_attribute ( nel, "ni_object", "SET_FUNC_AUTORANGE A.-");
               if (NI_write_element( SUMAg_CF->ns , nel, NI_BINARY_MODE ) < 0) {
                  SUMA_SLP_Err("Failed to send CMAP to afni");
                  NI_free_element(nel) ; nel = NULL;
                  if (stmp) free(stmp);
                  SUMA_Free_ColorMap(cmap); cmap = NULL;
                  break;
               }
               
               NI_free_element(nel) ; nel = NULL;
               
               /* set the range of the colorbar */
               nel = NI_new_data_element("ni_do", 0);
               NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
               sprintf(sbuf," %d", cmap->N_Col);
               stmp = SUMA_append_string("SET_FUNC_RANGE A.", sbuf);
               NI_set_attribute ( nel, "ni_object", stmp);
               if (NI_write_element( SUMAg_CF->ns , nel, NI_BINARY_MODE ) < 0) {
                  SUMA_SLP_Err("Failed to send CMAP to afni");
                  NI_free_element(nel) ; nel = NULL;
                  if (stmp) free(stmp);
                  SUMA_Free_ColorMap(cmap); cmap = NULL;
                  break;
               }
               NI_free_element(nel) ; nel = NULL;
               if (stmp) free(stmp);
               
               SUMA_Free_ColorMap(cmap); cmap = NULL;
            }
            break;
         case SE_OpenDrawnROIFileSelection:
            /* opens the open ROI file selection window. 
            Expects NULL in vp (to be used later and a position reference widget typecast to ip, the latter can be null.*/
            if (EngineData->vp_Dest != NextComCode || EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n", \
                  FuncName, NextCom, NextComCode);
               break;
            }
            /* open the ROI file */
            if (!sv) sv = &(SUMAg_SVv[0]);
            if (!EngineData->ip) {
               SUMAg_CF->X->FileSelectDlg = SUMA_CreateFileSelectionDialogStruct (sv->X->TOPLEVEL, SUMA_FILE_OPEN, YUP,
                                                        SUMA_OpenDrawnROI, (void *)EngineData->vp,
                                                        NULL, NULL,
                                                        "*.roi",
                                                        SUMAg_CF->X->FileSelectDlg);
            } else {
               SUMAg_CF->X->FileSelectDlg = SUMA_CreateFileSelectionDialogStruct ((Widget) EngineData->ip, SUMA_FILE_OPEN, YUP,
                                                        SUMA_OpenDrawnROI, (void *)EngineData->vp,
                                                        NULL, NULL,
                                                        "*.roi",
                                                        SUMAg_CF->X->FileSelectDlg);
            }
            SUMAg_CF->X->FileSelectDlg = SUMA_CreateFileSelectionDialog ("Select ROI File to Open", &SUMAg_CF->X->FileSelectDlg);
            break;
            
         case SE_SaveDrawnROIFileSelection:
            /* opens the save roi  file selection window. 
            Expects NULL in vp (to be used later and a position reference widget typecast to ip, the latter can be null.*/
            if (EngineData->vp_Dest != NextComCode || EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n", \
                  FuncName, NextCom, NextComCode);
               break;
            }
            
            /* save ROI to file */
            if (!sv) sv = &(SUMAg_SVv[0]);
            if (!EngineData->ip) {
               SUMAg_CF->X->FileSelectDlg = SUMA_CreateFileSelectionDialogStruct (sv->X->TOPLEVEL, SUMA_FILE_SAVE, YUP,
                                                        SUMA_SaveDrawnROI, (void *)EngineData->vp,
                                                        NULL, NULL,
                                                        NULL,
                                                        SUMAg_CF->X->FileSelectDlg);
            } else {
               SUMAg_CF->X->FileSelectDlg = SUMA_CreateFileSelectionDialogStruct ((Widget) EngineData->ip, SUMA_FILE_SAVE, YUP,
                                                        SUMA_SaveDrawnROI, (void *)EngineData->vp,
                                                        NULL, NULL,
                                                        NULL,
                                                        SUMAg_CF->X->FileSelectDlg);
            }
            
            SUMAg_CF->X->FileSelectDlg = SUMA_CreateFileSelectionDialog ("Select ROI File to Save", &SUMAg_CF->X->FileSelectDlg);
            
            break;

         case SE_OpenColFileSelection:
            /* opens the color file selection window. 
            Expect SO in vp and a position reference widget typecast to ip, the latter can be null.*/
            
            if (EngineData->vp_Dest != NextComCode || EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n", \
                  FuncName, NextCom, NextComCode);
               break;
            }
            
            /*Load colors from file */
            if (!sv) sv = &(SUMAg_SVv[0]);
            if (!EngineData->ip) {
               SUMAg_CF->X->FileSelectDlg = SUMA_CreateFileSelectionDialogStruct (sv->X->TOPLEVEL, SUMA_FILE_OPEN, YUP,
                                                        SUMA_LoadColorPlaneFile, (void *)EngineData->vp,
                                                        NULL, NULL,
                                                        "*.col",
                                                        SUMAg_CF->X->FileSelectDlg);
            } else {
               SUMAg_CF->X->FileSelectDlg = SUMA_CreateFileSelectionDialogStruct ((Widget) EngineData->ip, SUMA_FILE_OPEN, YUP,
                                                        SUMA_LoadColorPlaneFile, (void *)EngineData->vp,
                                                        NULL, NULL,
                                                        "*.col",
                                                        SUMAg_CF->X->FileSelectDlg);
            }
            
            SUMAg_CF->X->FileSelectDlg = SUMA_CreateFileSelectionDialog ("Select Node Color File", &SUMAg_CF->X->FileSelectDlg);
            
            break;
            
         case SE_OpenDrawROI:
            /* opens the DrawROI window, expects a surface viewer pointer in EngineData->Srcp*/
            {
               SUMA_DRAWN_ROI *DrawnROI=NULL;
               
               if (!sv) {
                  fprintf (SUMA_STDERR, "Error %s: Null sv.\n", FuncName);
                  SUMA_RETURN(NOPE);
               }   
               
               /* determine if there are ROIs being drawn on surfaces displayed here */
               DrawnROI = NULL;
               /* start with the Focus_SO */
               if (sv->Focus_SO_ID >= 0) {
                  SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
                  DrawnROI = SUMA_FetchROI_InCreation (SO, SUMAg_DOv,  SUMAg_N_DOv); 
               }
               if (!DrawnROI) { /* none found on focus surface, check other surfaces in this viewer */
                  N_SOlist = SUMA_RegisteredSOs(sv, SUMAg_DOv, SOlist);
                  if (N_SOlist) {
                     it = 0;
                     do {
                        DrawnROI = SUMA_FetchROI_InCreation (SO, SUMAg_DOv,  SUMAg_N_DOv);
                        ++it;
                     } while (!DrawnROI && it < N_SOlist);
                  }
               }
               
               /* call function to create ROI window */
               if (!SUMA_OpenDrawROIWindow (DrawnROI)) {
                  SUMA_RegisterMessage (SUMAg_CF->MessageList, "Failed to open Draw ROI window", FuncName, 
                                       SMT_Error, SMA_LogAndPopup);

               }
               break;
            
            }
         case SE_SetRenderMode:
            { /* sets the rendering mode of a surface, expects SO in vp and rendering mode in i*/
               SO = (SUMA_SurfaceObject *)EngineData->vp;
               SO->PolyMode = EngineData->i;                  
            }  
            break;
            
         case SE_UpdateLog:
            /* Updates the Log window if it is open */
            {
               if (SUMAg_CF->X->Log_TextShell) {
                  char *s = NULL;
                  s = SUMA_BuildMessageLog (SUMAg_CF->MessageList);
                  SUMAg_CF->X->Log_TextShell->CursorAtBottom = YUP;
                  (void) SUMA_CreateTextShell (s, "Message Log", SUMAg_CF->X->Log_TextShell);
                  XRaiseWindow(SUMAg_CF->X->DPY_controller1, XtWindow(SUMAg_CF->X->Log_TextShell->toplevel));
                  if (s) SUMA_free(s);
               }
            }
            break;
            
         case SE_Log:
            /* opens log window, needs nothing for the moment*/
            {
               char *s = NULL;
               if (SUMAg_CF->X->Log_TextShell) { /* just raise it */
                  XRaiseWindow(SUMAg_CF->X->DPY_controller1, XtWindow(SUMAg_CF->X->Log_TextShell->toplevel));
                  break;
               }else { /* create it */
                  s = SUMA_BuildMessageLog (SUMAg_CF->MessageList);
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: Message string:\n%s\n", FuncName, s);
                  LogShell =  SUMA_CreateTestShellStruct (SUMA_Message_open, NULL, 
                                                          SUMA_Message_destroyed, NULL);
                  if (!LogShell) {
                     fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateTestShellStruct.\n", FuncName);
                     break;
                  }
                  SUMAg_CF->X->Log_TextShell = SUMA_CreateTextShell(s, "SUMA log", LogShell);
                  SUMA_free(s);
               }
            }
            break;
            
         case SE_Help:
            /* opens help window, needs nothing for the moment*/
            {
               char *s = NULL;
               if (SUMAg_CF->X->Help_TextShell) { /* just raise it */
                     XRaiseWindow(SUMAg_CF->X->DPY_controller1, XtWindow(SUMAg_CF->X->Help_TextShell->toplevel));
                     break;
               }
                  
               s = SUMA_help_message_Info();
               if (!s) {
                  fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_help_message_Info.\n", FuncName);
                  break;
               }else {
                  TextShell =  SUMA_CreateTestShellStruct (SUMA_Help_open, NULL, 
                                                           SUMA_Help_destroyed, NULL);
                  if (!TextShell) {
                     fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateTestShellStruct.\n", FuncName);
                     break;
                  }
                  SUMAg_CF->X->Help_TextShell = SUMA_CreateTextShell(s, "SUMA help", TextShell);
                  SUMA_free(s);   
               }
            }
            break;
         case SE_Load_Group:
            /* Does not need an sv 
               expects  a pointer to .spec filename in cp, 
                        a VolumeParent name in vp,
                        the indices of the viewers to register the surfaces with in iv15. 
                        and the number of viewers specified in iv15 in i.
                        Surfaces are registered with all i viewers in iv15*/
            
            if (EngineData->cp_Dest != NextComCode || EngineData->vp_Dest != NextComCode 
               || EngineData->iv15_Dest != NextComCode || EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n%d %d %d %d\n", \
                  FuncName, NextCom, NextComCode, EngineData->cp_Dest, EngineData->vp_Dest, 
                  EngineData->iv15_Dest, EngineData->i_Dest);
               break;
            } 
            {
     		      SUMA_SurfSpecFile Spec;   
               char *VolParName = NULL, *specfilename = NULL;
               
               VolParName = (char *)EngineData->vp;
               specfilename = EngineData->cp;
               
               
               /* Load The spec file */
		         if (LocalHead) fprintf (SUMA_STDERR, "%s: Reading Spec File ...\n", FuncName);
               if (!SUMA_Read_SpecFile (specfilename, &Spec)) {
			         fprintf(SUMA_STDERR,"Error %s: Error in SUMA_Read_SpecFile.\n", FuncName);
			         exit(1);
		         }	

		         /* make sure only one group was read in */
		         if (Spec.N_Groups != 1) {
			         fprintf(SUMA_STDERR,"Error %s: One and only one group of surfaces is allowed at the moment (%d found).\n", FuncName, Spec.N_Groups);
			         exit(1);
		         }

		         /* load the surfaces specified in the specs file, one by one*/			
		         if (LocalHead) fprintf (SUMA_STDERR, "%s: Loading Surfaces in Spec File ...\n", FuncName);
		         if (!SUMA_LoadSpec (&Spec, SUMAg_DOv, &SUMAg_N_DOv, VolParName)) {
			         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_LoadSpec.\n", FuncName);
			         exit(1);
		         }

	            /* Register the surfaces in Spec file with the surface viewer and perform setups */
	            if (LocalHead) fprintf (SUMA_STDERR, "%s: Registering surfaces with surface viewers ...\n", FuncName);
               for (ii = 0; ii < EngineData->i; ++ii) {
		            if (!SUMA_SetupSVforDOs (Spec, SUMAg_DOv, SUMAg_N_DOv, &SUMAg_SVv[EngineData->iv15[ii]])) {
			            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_SetupSVforDOs function.\n", FuncName);
			            exit(1);
		            }
	            }

               if (LocalHead) fprintf (SUMA_STDERR, "%s: Adding call to Home and Redisplay \n", FuncName);
               /* add a call to Home and a redisplay */
               if (!list) {
                  if (LocalHead) fprintf (SUMA_STDERR, "%s: Creating new list.\n", FuncName);
                  list = SUMA_CreateList();
               }
               ED = SUMA_InitializeEngineListData (SE_Home_AllVisible);
               if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                      SEF_Empty, NULL, 
                                                      SES_Afni, NULL, NOPE, 
                                                      SEI_Tail, NULL )) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
                  break;
               }
               ED = SUMA_InitializeEngineListData (SE_Redisplay_AllVisible);
               if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                      SEF_Empty, NULL, 
                                                      SES_Afni, NULL, NOPE, 
                                                      SEI_Tail, NULL )) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
                  break;
               }
   
            }
            if (LocalHead) fprintf (SUMA_STDERR, "%s: Done in SE_Load_Spec.\n", FuncName);
            break;
            
         case SE_SetLookAt:
            /* expects a center XYZ in EngineData->fv3[0 .. 2] */
            if (EngineData->fv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            }
            /* calculate the transform required to bring the new look at location to the current one */
            {
               float ulook_old[3], ulook_new[3];
               int Step = 10, iStep;
               float fracUp, fracDown;
              
               ulook_old[0] = sv->GVS[sv->StdView].ViewFrom[0] - sv->GVS[sv->StdView].ViewCenter[0];
               ulook_old[1] = sv->GVS[sv->StdView].ViewFrom[1] - sv->GVS[sv->StdView].ViewCenter[1];
               ulook_old[2] = sv->GVS[sv->StdView].ViewFrom[2] - sv->GVS[sv->StdView].ViewCenter[2];
               ulook_new[0] = ulook_new[1] = ulook_new[2] = 0.0;
               fm = (float **)SUMA_allocate2D(4,4,sizeof(float));
               
               for (iStep = Step; iStep >= 1; --iStep) {
                  fracUp = (float)(iStep)/(float)Step;
                  fracDown = (float)(Step - iStep)/(float)Step;
                  if (LocalHead) fprintf (SUMA_STDERR,"%s:%d, fracUp %f, fracDown %f, fv3[%f %f %f]\n", 
                                 FuncName, iStep, fracUp, fracDown, EngineData->fv3[0], 
                                 EngineData->fv3[1], EngineData->fv3[2]);
                  ulook_new[0] = (EngineData->fv3[0] * fracUp + sv->GVS[sv->StdView].ViewFrom[0] * fracDown) \
                                 - sv->GVS[sv->StdView].ViewCenter[0];
                  ulook_new[1] = (EngineData->fv3[1] * fracUp + sv->GVS[sv->StdView].ViewFrom[1] * fracDown) \
                                 - sv->GVS[sv->StdView].ViewCenter[1];
                  ulook_new[2] = (EngineData->fv3[2] * fracUp + sv->GVS[sv->StdView].ViewFrom[2] * fracDown) \
                                 - sv->GVS[sv->StdView].ViewCenter[2];
                  if (fm == NULL) {
                     fprintf (SUMA_STDERR,"Error %s: Failed to allocate fm.\n",FuncName);
                     break;
                  }
                  if (!SUMA_FromToRotation (ulook_new, ulook_old, fm)) {
                     fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_FromToRotation.\n",FuncName);
                     break;
                  }
                  
                  /* add a SetRotMatrix to list*/
                  ED = SUMA_InitializeEngineListData (SE_SetRotMatrix);
                  ED->N_cols = 4;
                  ED->N_rows = 4;
                  if (!(LocElm = SUMA_RegisterEngineListCommand (  list, ED, 
                                                         SEF_fm, (void *)fm, 
                                                         EngineData->Src, EngineData->Srcp, NOPE, 
                                                         SEI_Head, NULL ))) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
                     break;
                  }
                                    
                  /* add a redisplay call */
                  ED = SUMA_InitializeEngineListData (SE_RedisplayNow);
                  if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                         SEF_Empty, NULL, 
                                                         EngineData->Src, EngineData->Srcp, NOPE, 
                                                         SEI_After, LocElm )) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
                     break;
                  }
                  
               }
               /* fm was copied into engine data, free it */
               SUMA_free2D((char **)fm, 4);
            }
            break;
         
         case SE_ToggleConnected:
               /* expects nothing in EngineData */
               if (!SUMA_CanTalkToAfni (SUMAg_DOv, SUMAg_N_DOv)) {
                  fprintf(SUMA_STDOUT,"%s: Cannot connect to AFNI.\n\tNot one of the surfaces is mappable and has a Surface Volume.\n\tDid you use the -sv option when launching SUMA ?\n", FuncName);
                  break;
               }
               SUMAg_CF->Connected = !SUMAg_CF->Connected;
               if (SUMAg_CF->Connected) {
                 /* find out if the stream has been established already */
                  if (SUMAg_CF->ns) { /* stream is open, nothing to do */
                     if (LocalHead) fprintf(SUMA_STDOUT,"%s: Stream existed, reusing.\n", FuncName);
                     fprintf(SUMA_STDOUT,"%s: Connected to AFNI.\n", FuncName);
                  }else {   /* must open stream */              
                     /* contact afni */
                        fprintf(SUMA_STDOUT,"%s: Contacting afni ...\n", FuncName);
                        SUMAg_CF->ns = NI_stream_open( SUMAg_CF->NimlAfniStream , "w" ) ;
                        if (!SUMAg_CF->ns) {
                           SUMA_SLP_Err("NI_stream_open failed.");
                           SUMA_BEEP;
                           SUMAg_CF->Connected = !SUMAg_CF->Connected;
                           break ;
                        }
                        if (!strcmp(SUMAg_CF->AfniHostName,"localhost")) { /* only try shared memory when 
                                                                              AfniHostName is localhost */
                           fprintf (SUMA_STDERR, "%s: Trying shared memory...\n", FuncName);
                           if( strstr( SUMAg_CF->NimlAfniStream , "tcp:localhost:" ) != NULL ) {
                              if (!NI_stream_reopen( SUMAg_CF->ns , "shm:WeLikeElvis:1M" )) {
                                 fprintf (SUMA_STDERR, "Warning %s: Shared memory communcation failed.\n", FuncName);
                              }
                           }
                        }
                        /*   SUMAg_CF->ns = NI_stream_open( "tcp:128.231.212.194:53211" , "w" ) ;*/

                     if( SUMAg_CF->ns == NULL ){
                        SUMA_SLP_Err("NI_stream_open failed");
                        SUMA_BEEP; 
                        SUMAg_CF->Connected = !SUMAg_CF->Connected;
                        break ;
                     }

                     Wait_tot = 0;
                     while(Wait_tot < SUMA_WriteCheckWaitMax){
                       nn = NI_stream_writecheck( SUMAg_CF->ns , SUMA_WriteCheckWait) ;
                       if( nn == 1 ){ fprintf(stderr,"\n") ; break ; }
                       if( nn <  0 ){ fprintf(stderr,"BAD\n"); SUMAg_CF->Connected = !SUMAg_CF->Connected; SUMAg_CF->ns = NULL; break;}
                       Wait_tot += SUMA_WriteCheckWait;
                       fprintf(SUMA_STDERR,".") ;
                     }

                     /* make sure you did not exit because of time out */
                     if (nn!=1) {
                        SUMAg_CF->Connected = !SUMAg_CF->Connected;
                        SUMAg_CF->ns = NULL;
                        fprintf(SUMA_STDERR,"Error %s: WriteCheck timed out (> %d ms).\n", FuncName, SUMA_WriteCheckWaitMax);
                        break ;
                     }
                  } 
                  
                  /* Stream is open */
                  
                  /* start the listening WorkProcess */
                  SUMA_register_workproc(SUMA_niml_workproc, (XtPointer)sv);

                  /* register a call for sending the surface to afni (SetAfniSurf)*/
                  if (LocalHead) fprintf(SUMA_STDERR,"Notifying Afni of New surface...\n");
                  ED = SUMA_InitializeEngineListData (SE_SetAfniSurf);
                  SUMA_RegisterEngineListCommand (list, ED, 
                                                   SEF_Empty, NULL,
                                                   SES_Suma, (void *)sv, NOPE,
                                                   SEI_Head, NULL); 
                  break;
               } else {
                  fprintf(SUMA_STDOUT,"%s: Disconnecting from afni.\n", FuncName);
                  /* remove the listening workprocess) */
                  SUMA_remove_workproc( SUMA_niml_workproc );
                   
                  if (!SUMAg_CF->ns) {
                     /* It looks like the stream was closed, do the clean up */
                     fprintf(SUMA_STDERR,"Warning %s: sv->ns is null, stream must have gotten closed. Cleaning up ...\n", FuncName);
                     ED = SUMA_InitializeEngineListData (SE_CloseStream4All);
                     SUMA_RegisterEngineListCommand (list, ED, 
                                                   SEF_Empty, NULL,
                                                   SES_Suma, (void *)sv, NOPE,
                                                   SEI_Head, NULL); 
                     
                     break;
                  }
                  
                 
                  /* Close the stream if nobody else wants it. 
                  This is not a great condition, one should be able to leave the stream open 
                  even if no viewer, for the moment, does not want to talk to AFNI.
                  Perhaps in the future. */
                  if (SUMAg_N_SVv == 1) {
                     fprintf(SUMA_STDERR,"%s: Nobody wants to talk to AFNI anymore, closing stream ...\n", FuncName);
                     NI_stream_close(SUMAg_CF->ns);
                     SUMAg_CF->ns = NULL;
                  }
                  break;
               }
   
         case SE_CloseStream4All:
            /* expects nothing in EngineData */
               
            /* odds are AFNI died or closed stream, mark all surfaces as unsent */
            for (ii=0; ii<SUMAg_N_DOv; ++ii) {
               if (SUMA_isSO(SUMAg_DOv[ii])) {
                  SO = (SUMA_SurfaceObject *)(SUMAg_DOv[ii].OP);
                  if (SO->SentToAfni) SO->SentToAfni = NOPE;
               }
            }
            
            /* same for parent fields */
            /* check first if stream in SUMAg_CF still good by any chance */
            nn = NI_stream_goodcheck(SUMAg_CF->ns , 1 ) ;
            
            if( nn >= 0 ){ 
               fprintf(stderr,"Error %s: Stream still alive, this should not be. Closing anyway.\n", FuncName); 
               NI_stream_close(SUMAg_CF->ns); 
            }
            
            /* clean up and get out of here*/         
            SUMAg_CF->ns = NULL;
            break;
            
         case SE_SetForceAfniSurf:
            /* expects nothing in EngineData */
            /* send to afni surfaces that can be sent even if they have been sent already */
            for (ii=0; ii<sv->N_DO; ++ii) {
               if (SUMA_isSO(SUMAg_DOv[sv->RegisteredDO[ii]])) {
                  SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->RegisteredDO[ii]].OP);
                  if (SO->SentToAfni) SO->SentToAfni = NOPE;
               }
            }
            
            /* proceed to SE_SetAfniSurf: */
            ED = SUMA_InitializeEngineListData (SE_SetAfniSurf);
            SUMA_RegisterEngineListCommand (list, ED, 
                                            SEF_Empty, NULL,
                                            SES_Suma, (void *)sv, NOPE,
                                            SEI_Head, NULL); 
            break;
            
         case SE_SetAfniSurf:
            /* expects nothing in EngineData */
            { int loc_ID;
            
            /* send to afni the list of inherently mappable surfaces and with a surface volume*/
            /* No surfaces are sent twice because there should not be duplicate 
            inherently mappable surfaces in SUMAg_DOv */
            /* prior to Wed Nov  6 17:47:20 EST 2002, only mappable surfaces that are related to the ones shown in the viewer
            were being sent to AFNI. Now all mappable surfaces loaded are sent regardless of what is shown */
            
            for (ii=0; ii<SUMAg_N_DOv; ++ii) {
               if (SUMA_isSO(SUMAg_DOv[ii])) {
                  SO = (SUMA_SurfaceObject *)(SUMAg_DOv[ii].OP);
                  if (!SUMA_isINHmappable(SO)) {
                     continue;
                  }
                  /* if this surface has been sent to AFNI before, bypass it */
                  if (SO->SentToAfni) {
                     if (LocalHead) fprintf(SUMA_STDERR, "Warning %s: Surface %s has been sent to AFNI before.\n", \
                        FuncName, SO->idcode_str);
                     continue;
                  }else {
                     if (LocalHead) fprintf(SUMA_STDERR, "Warning %s: Surface %s Will be sent to AFNI.\n", \
                        FuncName, SO->idcode_str);
                  }
                  nel = SUMA_makeNI_SurfIXYZ (SO);
                  if (!nel) {
                     fprintf(SUMA_STDERR,"Error %s: SUMA_makeNI_SurfIXYZ failed\n", FuncName);
                     break;
                  }
                  /* send surface nel */
                  if (LocalHead) fprintf(SUMA_STDERR,"%s: Sending SURF_iXYZ nel...\n ", FuncName) ;
                  nn = NI_write_element( SUMAg_CF->ns , nel , NI_BINARY_MODE ) ;

                  if( nn < 0 ){
                       fprintf(SUMA_STDERR,"Error %s: NI_write_element failed\n", FuncName);
                  }

                  #if 0
                     {
                        NI_stream nstdout;
                         nstdout = NI_stream_open( "fd:1","w");
                          if( nstdout == NULL ){ fprintf(SUMA_STDERR,"Can't open fd:1\n"); break; }
                           NI_write_element( nstdout , nel , NI_TEXT_MODE ) ;
                         NI_stream_close(nstdout);
                     }
                  #endif

                  NI_free_element(nel);

                  /* send triangles */
                  nel = SUMA_makeNI_SurfIJK (SO);
                  if (!nel) {
                     fprintf(SUMA_STDERR,"Error %s: SUMA_makeNI_SurfIJK failed\n", FuncName);
                     break;
                  }
                  /* send surface nel */
                  fprintf(SUMA_STDERR,"%s: Sending SURF_IJK nel ...\n", FuncName) ;
                  nn = NI_write_element( SUMAg_CF->ns , nel , NI_BINARY_MODE ) ;

                  if( nn < 0 ){
                       fprintf(SUMA_STDERR,"Error %s: NI_write_element failed\n", FuncName);
                  }
                  NI_free_element(nel);
                  nel = NULL;

                  /* mark surface as sent to afni */
                  SO->SentToAfni = YUP;
               }
            }
            break;
            }
         case SE_ToggleShowSelectedNode:
            /* expects nothing in EngineData */
            {
               int CommonState = -1;
               
               for (ii=0; ii<sv->N_DO; ++ii) {
                  if (SUMA_isSO(SUMAg_DOv[sv->RegisteredDO[ii]])) {
                     if (CommonState < 0) {
                        SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->RegisteredDO[ii]].OP);
                        SO->ShowSelectedNode = !SO->ShowSelectedNode;
                        CommonState = SO->ShowSelectedNode;
                        fprintf(SUMA_STDOUT,"SO->ShowSelectedNode = %d\n", SO->ShowSelectedNode);
                     } else {
                        SO->ShowSelectedNode = CommonState;
                     }
                  }
                
               }
            
            XmToggleButtonSetState (sv->X->ViewMenu[SW_ViewNodeInFocus], 
                  CommonState, NOPE); 
                
            }
            break;
         
         case SE_SetSelectedNode:
            /* expects a node index in i */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
            if (EngineData->i >= 0 && EngineData->i < SO->N_Node) {
               SO->SelectedNode = EngineData->i;
            } else {
               SUMA_SLP_Err("Node index < 0 || > Number of nodes in surface");
               break;
            }
            break;
            
         case SE_ToggleShowSelectedFaceSet:
            /* expects nothing ! */
            { int CommonState = -1;
               for (ii=0; ii<sv->N_DO; ++ii) {
                  if (SUMA_isSO(SUMAg_DOv[sv->RegisteredDO[ii]])) {
                     SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->RegisteredDO[ii]].OP);
                     if (CommonState < 0) { /* first surface, set the common state */
                        SO->ShowSelectedFaceSet = !SO->ShowSelectedFaceSet;
                        CommonState = SO->ShowSelectedFaceSet;
                        fprintf(SUMA_STDOUT,"SO->ShowSelectedFaceSet = %d\n", \
                           SO->ShowSelectedFaceSet);
                      }else {
                        SO->ShowSelectedFaceSet = CommonState;
                     }
                  }
               }
            XmToggleButtonSetState (sv->X->ViewMenu[SW_ViewSelectedFaceset], 
                  CommonState, NOPE);  
            }          
            break;
         
         case SE_SetSelectedFaceSet:
            /* expects the index for the selected FaceSet */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
            if (EngineData->i < 0 || EngineData->i >= SO->N_FaceSet) {
               SUMA_SLP_Err("Node index < 0 || > Number of FaceSets in surface");
               break;
            }
            ND = SO->NodeDim;
            NP = SO->FaceSetDim;
            ip = NP * EngineData->i;
            id = ND * SO->FaceSetList[ip];
            SO->FaceSetMarker->n0[0] = SO->NodeList[id];
            SO->FaceSetMarker->n0[1] = SO->NodeList[id+1];
            SO->FaceSetMarker->n0[2] = SO->NodeList[id+2];
            id = ND * SO->FaceSetList[ip+1];
            SO->FaceSetMarker->n1[0] = SO->NodeList[id];
            SO->FaceSetMarker->n1[1] = SO->NodeList[id+1];
            SO->FaceSetMarker->n1[2] = SO->NodeList[id+2];
            id = ND * SO->FaceSetList[ip+2];
            SO->FaceSetMarker->n2[0] = SO->NodeList[id];
            SO->FaceSetMarker->n2[1] = SO->NodeList[id+1];
            SO->FaceSetMarker->n2[2] = SO->NodeList[id+2];
            SO->FaceSetMarker->NormVect[0] = SO->FaceNormList[ip];
            SO->FaceSetMarker->NormVect[1] = SO->FaceNormList[ip+1];
            SO->FaceSetMarker->NormVect[2] = SO->FaceNormList[ip+2];
            
            SO->SelectedFaceSet = EngineData->i;
            break;
            
         case SE_ToggleCrossHair:
            /* expects nothing in EngineData */
            sv->ShowCrossHair = !sv->ShowCrossHair;
            XmToggleButtonSetState (sv->X->ViewMenu[SW_ViewCrossHair], 
               sv->ShowCrossHair, NOPE);            
            break;
            
         case SE_SetCrossHair:
            /* Expects Cross Hair coordinates in fv3 */
            if (EngineData->fv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            }
            sv->Ch->c[0] = EngineData->fv3[0]; sv->Ch->c[1]= EngineData->fv3[1]; sv->Ch->c[2]= EngineData->fv3[2];
            break;
         
         case SE_BindCrossHair:
            /* expects SurfaceID to bind cross hair to*/
            if (EngineData->iv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            }
            sv->Ch->SurfaceID = EngineData->iv3[0];
            sv->Ch->NodeID = EngineData->iv3[1];
            
            break;
         
         case SE_ToggleLockView:
            /* expects index of viewer in i to toggle its lock view */
            /* toggles the lock view button */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            }
            SUMAg_CF->ViewLocked[EngineData->i] = !SUMAg_CF->ViewLocked[EngineData->i];
            /* update button if needed*/
            if (EngineData->Src != SES_SumaWidget) {
               XmToggleButtonSetState (SUMAg_CF->X->SumaCont->LockView_tbg[EngineData->i], SUMAg_CF->ViewLocked[EngineData->i], NOPE);
            }
            
            /* call function to update the AllLock button */
            SUMA_set_LockView_atb ();
            
            break;
         
         case SE_ToggleLockAllViews:
            /* expects nothing, toggles all locked view buttons */
            
            /* get the current value of the button */
            {
               SUMA_Boolean CurState;
               CurState = XmToggleButtonGetState (SUMAg_CF->X->SumaCont->LockAllView_tb);
               for (ii=0; ii< SUMA_MAX_SURF_VIEWERS; ++ii) { /* set all buttons accrodingly */
                  XmToggleButtonSetState (SUMAg_CF->X->SumaCont->LockView_tbg[ii], CurState, NOPE);
                  SUMAg_CF->ViewLocked[ii] = CurState;
               }
            }
            break;
         
         case SE_ToggleLockAllCrossHair:
            /* expects nothing, toggles cross hair lock for all viewers */
            {
               char LockName[100];
               SUMA_LockEnum_LockType (SUMAg_CF->Locked[0], LockName);
               fprintf (SUMA_STDERR,"%s: Switching Locktype from %s", FuncName, LockName);
               /* change the locking type of viewer 0 */
               SUMAg_CF->Locked[0] = (int)fmod(SUMAg_CF->Locked[0]+1, SUMA_N_Lock_Types);
               SUMA_LockEnum_LockType (SUMAg_CF->Locked[0], LockName);
               fprintf (SUMA_STDERR," %s\n", LockName);
               /* update the widget*/
               SUMA_set_Lock_rb (SUMAg_CF->X->SumaCont->Lock_rbg, 0, SUMAg_CF->Locked[0]);
               /* Change the locking type of all remaining viewers, including unopen ones */
               for (ii=1; ii< SUMA_MAX_SURF_VIEWERS; ++ii) {
                  SUMAg_CF->Locked[ii] = SUMAg_CF->Locked[0];                  
                  SUMA_set_Lock_rb (SUMAg_CF->X->SumaCont->Lock_rbg, ii, SUMAg_CF->Locked[ii]);
               }

               /* now update the all lock keys */
               SUMA_set_Lock_arb (SUMAg_CF->X->SumaCont->Lock_rbg);

            }
            break;
         
         case SE_SetLockAllCrossHair:
            /* expects a Lock value in i , sets the lock of all viewers */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            }
            {
               
               /* Change the locking type of all remaining viewers, including unopen ones */
               for (ii=0; ii< SUMA_MAX_SURF_VIEWERS; ++ii) {
                  SUMAg_CF->Locked[ii] = EngineData->i;                  
                  SUMA_set_Lock_rb (SUMAg_CF->X->SumaCont->Lock_rbg, ii, SUMAg_CF->Locked[ii]);
               }

               /* now update the all lock keys */
               SUMA_set_Lock_arb (SUMAg_CF->X->SumaCont->Lock_rbg);
            }
            break;
            
         case SE_LockCrossHair:
            /* expects nothing in EngineData */

            /* calls other viewers and determine if the cross hair needs to be locked to the calling sv */

            /* check to see if other viewers need to share the fate */
            ii = SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv);
            if (ii < 0) {
               fprintf (SUMA_STDERR,"Error %s: Failed to find index of sv.\n", FuncName);
               break;
            }
            if (SUMAg_CF->Locked[ii]) { /* This one's locked, find out which other viewers are locked to this one */
               for (i=0; i < SUMAg_N_SVv; ++i) {
                  svi = &SUMAg_SVv[i];
                  if (i != ii) {
                     switch (SUMAg_CF->Locked[i]) { 
                        case SUMA_No_Lock:
                           if (LocalHead) fprintf (SUMA_STDERR, "%s: No lock for viewer %d.\n", FuncName, i);
                           break;
                        case SUMA_XYZ_Lock:
                           if (LocalHead) fprintf (SUMA_STDERR, "%s: Try to XYZ lock viewer %d.\n", FuncName, i);
                           /* just set the XYZ, and free the binding to the surfaces */
                           svi->Ch->c[0] = sv->Ch->c[0];
                           svi->Ch->c[1] = sv->Ch->c[1];
                           svi->Ch->c[2] = sv->Ch->c[2];
                           svi->Ch->NodeID = -1;
                           svi->Ch->SurfaceID = -1;
                           /* FORCE a redisplay */
                           svi->ResetGLStateVariables = YUP;
                           SUMA_handleRedisplay((XtPointer)svi->X->GLXAREA);                           
                           break;
                        case SUMA_I_Lock: 
                           {
                              SUMA_SurfaceObject *SO1 = NULL, *SO2 = NULL;
                              
                              if (LocalHead) fprintf (SUMA_STDERR, "%s: Try to I lock viewer %d to node %d.\n", FuncName, i, sv->Ch->NodeID);
                              
                              /* determine the list of shown surfaces */
                              N_SOlist = SUMA_RegisteredSOs(svi, SUMAg_DOv, SOlist);

                              /* first find the surface that the cross hair is bound to */
                              if (sv->Ch->SurfaceID < 0) {
                                 fprintf (SUMA_STDERR, "%s: Cannot link from this viewer's cross hair. No bound surface.\n", FuncName);
                                 break;
                              }
                              if (sv->Ch->NodeID < 0) {
                                 fprintf (SUMA_STDERR, "%s: Cannot link from this viewer's cross hair. No NodeID.\n", FuncName);
                                 break;
                              }
                              SO1 = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Ch->SurfaceID].OP;
                              Found = NOPE;
                              it = 0;
                              while (it < N_SOlist && !Found) {
                                 SO2 = (SUMA_SurfaceObject *)SUMAg_DOv[SOlist[it]].OP;
                                 if (SUMA_isRelated (SO1, SO2)) {
                                    svi->Ch->SurfaceID = SOlist[it];
                                    if (sv->Ch->NodeID > SO2->N_Node) {
                                       fprintf (SUMA_STDERR,"Error %s: NodeID is larger than N_Node. Setting NodeID to 0.\n", FuncName);
                                       svi->Ch->NodeID = 0;
                                    }else{
                                       svi->Ch->NodeID = sv->Ch->NodeID;
                                    }
                                    
                                    /* set the XYZ */
                                    svi->Ch->c[0] = SO2->NodeList[SO2->NodeDim*svi->Ch->NodeID];
                                    svi->Ch->c[1] = SO2->NodeList[SO2->NodeDim*svi->Ch->NodeID+1];
                                    svi->Ch->c[2] = SO2->NodeList[SO2->NodeDim*svi->Ch->NodeID+2];
                                    fprintf (SUMA_STDERR,"%s: new XYZ %f %f %f\n", FuncName, 
                                       svi->Ch->c[0], svi->Ch->c[1], svi->Ch->c[2]); 
                                    Found = YUP;
                                 }
                                 ++it;
                              }
                              if (!Found) {
                                 fprintf (SUMA_STDERR,"%s: No related surfaces found in viewer, cross hair will not be touched .\n", FuncName);
                                 break;
                              } else {
                                 /* FORCE a redisplay */
                                 svi->ResetGLStateVariables = YUP;
                                 SUMA_handleRedisplay((XtPointer)svi->X->GLXAREA);
                              }
                              
                           }
                           break;
                        default:
                           fprintf(SUMA_STDERR,"Error %s: Lock type (%d) undefined.\n", FuncName, SUMAg_CF->Locked[ii]);
                           break;
                     }
                  }
               }
            }else{
               /* not locked to anything */
            }
            break;
                        
         case SE_SetAfniCrossHair:
            /* expects nothing in EngineData */
            /* sends the current cross hair to afni */
            /* form nel */
            nel = SUMA_makeNI_CrossHair (sv);
            if (!nel) {
               fprintf(SUMA_STDERR,"Error %s: SUMA_makeNI_SurfIXYZ failed\n", FuncName);
               break;
               }
            /*send it to afni */
            /*fprintf(SUMA_STDERR,"Sending cross hair nel ") ;*/
            nn = NI_write_element( SUMAg_CF->ns , nel , NI_TEXT_MODE ) ;
            /*SUMA_nel_stdout (nel);*/
      
            if( nn < 0 ){
                   fprintf(SUMA_STDERR,"Error %s: NI_write_element failed\n", FuncName);
            }
            
            NI_free_element(nel);

            break;
                  
         case SE_SetLookAtNode:
            /* expects a center XYZ in EngineData->fv15[0 .. 2]
            expects a normal vector in EngineData->fv15[3 .. 5] */
            if (EngineData->fv15_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            
            { float CurrentDistance;
              float **fm2_3;
              
            /* modify the ViewFrom Value such that the viewing distance remains the same */
            CurrentDistance = sqrt((sv->GVS[sv->StdView].ViewFrom[0]-sv->GVS[sv->StdView].ViewCenter[0])*(sv->GVS[sv->StdView].ViewFrom[0]-sv->GVS[sv->StdView].ViewCenter[0]) +\
                                    (sv->GVS[sv->StdView].ViewFrom[1]-sv->GVS[sv->StdView].ViewCenter[1])*(sv->GVS[sv->StdView].ViewFrom[1]-sv->GVS[sv->StdView].ViewCenter[1]) +\
                                    (sv->GVS[sv->StdView].ViewFrom[2]-sv->GVS[sv->StdView].ViewCenter[2])*(sv->GVS[sv->StdView].ViewFrom[2]-sv->GVS[sv->StdView].ViewCenter[2]));
            
            /* set the ViewCenter Value to that of the node's XYZ*/
            sv->GVS[sv->StdView].ViewCenter[0] = EngineData->fv15[0];
            sv->GVS[sv->StdView].ViewCenter[1] = EngineData->fv15[1]; 
            sv->GVS[sv->StdView].ViewCenter[2] = EngineData->fv15[2];
            
            /* obtain the LookFrom point based on CurrentDistance and the normal vector */
            fm2_3 = SUMA_Point_At_Distance(&(EngineData->fv15[3]), sv->GVS[sv->StdView].ViewCenter, CurrentDistance);
            if (fm2_3 == NULL) {
               fprintf(SUMA_STDOUT,"Error %s: SUMA_Point_At_Distance failed.\n", FuncName);
               break;
            }
            fprintf(SUMA_STDOUT,"\nPoints: %f %f %f\n%f %f %f\n", \
               fm2_3[0][0], fm2_3[0][1], fm2_3[0][2], \
               fm2_3[1][0], fm2_3[1][1], fm2_3[1][2]);
            
            sv->GVS[sv->StdView].ViewFrom[0] = fm2_3[0][0]; 
            sv->GVS[sv->StdView].ViewFrom[1] = fm2_3[0][1]; 
            sv->GVS[sv->StdView].ViewFrom[2] = fm2_3[0][2]; 
            
            /* fm2_3 not needed anymore */
            SUMA_free2D((char **)fm2_3, 2);
            
            gluLookAt (sv->GVS[sv->StdView].ViewFrom[0], sv->GVS[sv->StdView].ViewFrom[1], sv->GVS[sv->StdView].ViewFrom[2], sv->GVS[sv->StdView].ViewCenter[0], sv->GVS[sv->StdView].ViewCenter[1], sv->GVS[sv->StdView].ViewCenter[2], sv->GVS[sv->StdView].ViewCamUp[0], sv->GVS[sv->StdView].ViewCamUp[1], sv->GVS[sv->StdView].ViewCamUp[2]);
            }
            
            break;
         case SE_SetLookFrom:
            /* expects a center XYZ in EngineData->fv3[0 .. 2] */
            if (EngineData->fv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            /* set the LookFrom option */
            sv->GVS[sv->StdView].ViewFrom[0] = EngineData->fv3[0];
            sv->GVS[sv->StdView].ViewFrom[1] = EngineData->fv3[1]; 
            sv->GVS[sv->StdView].ViewFrom[2] = EngineData->fv3[2];
            gluLookAt (sv->GVS[sv->StdView].ViewFrom[0], sv->GVS[sv->StdView].ViewFrom[1], sv->GVS[sv->StdView].ViewFrom[2], sv->GVS[sv->StdView].ViewCenter[0], sv->GVS[sv->StdView].ViewCenter[1], sv->GVS[sv->StdView].ViewCenter[2], sv->GVS[sv->StdView].ViewCamUp[0], sv->GVS[sv->StdView].ViewCamUp[1], sv->GVS[sv->StdView].ViewCamUp[2]);
            break;

         case SE_Redisplay_AllVisible:
            /* expects nothing in EngineData */
            /* post a redisplay to all visible viewers */
            for (ii=0; ii<SUMAg_N_SVv; ++ii) {
               if (LocalHead) fprintf (SUMA_STDERR,"%s: Checking viewer %d.\n", FuncName, ii);
               if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
                  /* you must check for both conditions because by default 
                  all viewers are initialized to isShaded = NOPE, even before they are ever opened */
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: Redisplaying viewer %d.\n", FuncName, ii);
                  SUMAg_SVv[ii].ResetGLStateVariables = YUP;
                  SUMA_postRedisplay(SUMAg_SVv[ii].X->GLXAREA, NULL, NULL);
               }
            }
            break;
            
         case SE_Redisplay:
            /* expects nothing in EngineData */
            /*post a redisplay to one specific viewer*/
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Redisplay ...", FuncName);
            SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
            if (LocalHead) fprintf (SUMA_STDOUT," Done\n");
            break;
         
         case SE_RedisplayNow:
            /* expects nothing in EngineData */
            /*call handle redisplay immediately to one specific viewer*/
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Redisplaying NOW ...", FuncName);
            sv->ResetGLStateVariables = YUP;
            SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
            if (LocalHead) fprintf (SUMA_STDOUT," Done\n");
            break;
            
         case SE_RedisplayNow_AllVisible:
            /* expects nothing in EngineData */
            /* causes  an immediate redisplay to all visible viewers */
            for (ii=0; ii<SUMAg_N_SVv; ++ii) {
               if (LocalHead) fprintf (SUMA_STDERR,"%s: Checking viewer %d.\n", FuncName, ii);
               if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
                  /* you must check for both conditions because by default 
                  all viewers are initialized to isShaded = NOPE, even before they are ever opened */
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: Redisplaying viewer %d.\n", FuncName, ii);
                  SUMAg_SVv[ii].ResetGLStateVariables = YUP;
                  SUMA_handleRedisplay((XtPointer)SUMAg_SVv[ii].X->GLXAREA);
               }
            }
            break;
         
         case SE_RedisplayNow_AllOtherVisible:
            /* expects nothing in EngineData, expects sv in srcp*/
            /* causes an immediate redisplay to all visible viewers other than sv*/
            for (ii=0; ii<SUMAg_N_SVv; ++ii) {
               if (LocalHead) fprintf (SUMA_STDERR,"%s: Checking viewer %d.\n", FuncName, ii);
               if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL && &(SUMAg_SVv[ii]) != sv) {
                  /* you must check for both conditions because by default 
                  all viewers are initialized to isShaded = NOPE, even before they are ever opened */
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: Redisplaying viewer %d.\n", FuncName, ii);
                  SUMAg_SVv[ii].ResetGLStateVariables = YUP;
                  SUMA_handleRedisplay((XtPointer)SUMAg_SVv[ii].X->GLXAREA);
               }
            }
            break;

         case SE_ResetOpenGLState:
            /* reset OPEN GL's state variables */
            /* expects the surface viewer pointer in vp */
            if (EngineData->vp_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            } 
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Resetting OpenGL state variables.\n", FuncName);
            
            /* No need to call SUMA_OpenGLStateReset, that is now done in SUMA_display */
            svi = (SUMA_SurfaceViewer *)EngineData->vp;
            svi->ResetGLStateVariables = YUP;
            break;
            
         case SE_ToggleForeground:
            /* expects nothing in EngineData */
            /* Show/hide the foreground */
            sv->ShowForeground = !sv->ShowForeground;
            if (!sv->ShowForeground) {
               fprintf(SUMA_STDOUT,"%s: Foreground Colors Off.\n", FuncName);
            } else {
               fprintf(SUMA_STDOUT,"%s: Foreground Colors ON.\n", FuncName);
            }
            /* set the color remix flag */
            if (!SUMA_SetShownLocalRemixFlag (sv)) {
               fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SetShownLocalRemixFlag.\n", FuncName);
               break;
            }
            break;
         
         case SE_ToggleBackground:
            /* expects nothing in EngineData */
            /* Show/hide the background */
            sv->ShowBackground = !sv->ShowBackground;
            if (!sv->ShowBackground) {
               fprintf(SUMA_STDOUT,"%s: Background Colors OFF.\n", FuncName);
            } else {
               fprintf(SUMA_STDOUT,"%s: Background Colors ON.\n", FuncName);
            }
            /* set the color remix flag */
            if (!SUMA_SetShownLocalRemixFlag (sv)) {
               fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SetShownLocalRemixFlag.\n", FuncName);
               break;
            }
            break;
                     
         case SE_Home:
            /* expects nothing in EngineData, needs sv */
            sv->GVS[sv->StdView].translateVec[0]=0; sv->GVS[sv->StdView].translateVec[1]=0;
            glMatrixMode(GL_PROJECTION);
            /* sv->FOV[sv->iState] = FOV_INITIAL;   *//* Now done in SE_FOVreset *//* reset the zooming */
            sv->GVS[sv->StdView].ViewFrom[0] = sv->GVS[sv->StdView].ViewFromOrig[0];
            sv->GVS[sv->StdView].ViewFrom[1] = sv->GVS[sv->StdView].ViewFromOrig[1];
            sv->GVS[sv->StdView].ViewFrom[2] = sv->GVS[sv->StdView].ViewFromOrig[2];
            sv->GVS[sv->StdView].ViewCenter[0] = sv->GVS[sv->StdView].ViewCenterOrig[0];
            sv->GVS[sv->StdView].ViewCenter[1] = sv->GVS[sv->StdView].ViewCenterOrig[1];
            sv->GVS[sv->StdView].ViewCenter[2] = sv->GVS[sv->StdView].ViewCenterOrig[2];
            
            glMatrixMode(GL_MODELVIEW);
            glLoadIdentity();
            gluLookAt (sv->GVS[sv->StdView].ViewFrom[0], sv->GVS[sv->StdView].ViewFrom[1], sv->GVS[sv->StdView].ViewFrom[2], sv->GVS[sv->StdView].ViewCenter[0], sv->GVS[sv->StdView].ViewCenter[1], sv->GVS[sv->StdView].ViewCenter[2], sv->GVS[sv->StdView].ViewCamUp[0], sv->GVS[sv->StdView].ViewCamUp[1], sv->GVS[sv->StdView].ViewCamUp[2]);
            break;
         
         case SE_Home_AllVisible:
            /* expects nothing in EngineData, needs no sv */
            {
               for (ii=0; ii<SUMAg_N_SVv; ++ii) {
                  if (LocalHead) fprintf (SUMA_STDERR,"%s: Checking viewer %d.\n", FuncName, ii);
                  if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
                     /* you must check for both conditions because by default 
                     all viewers are initialized to isShaded = NOPE, even before they are ever opened */
                     if (LocalHead) fprintf (SUMA_STDERR,"%s: Home call viewer %d.\n", FuncName, ii);
                     if (!list) list= SUMA_CreateList();
                     SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Home, SES_Suma, &SUMAg_SVv[ii]);
                  }
               }
            }
            break;
         
         case SE_FOVreset:
            /* expects nothing in EngineData */
            sv->FOV[sv->iState] = FOV_INITIAL;   /* reset the zooming */
            break;
            
         case SE_SetNodeColor:
            /* expects a four-columned fm in EngineData->fm[0 .. N][0..3] 
            [Node Index] [R] [G] [B] RGB between 0 and 1*/
            if (EngineData->fm_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            }
            SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
            {
               GLfloat *glar_ColorList;
               glar_ColorList = SUMA_GetColorList(sv, SO->idcode_str);
               if (!glar_ColorList) {
                  fprintf (SUMA_STDERR,"Error %s: NULL color list array. Trouble.\n", FuncName);
                  break;
               }
               for (i=0; i < EngineData->N_rows; ++i){
                  ii = (int)(EngineData->fm[i][0]);
                  glar_ColorList[4*ii] = EngineData->fm[i][1];
                  glar_ColorList[4*ii+1] = EngineData->fm[i][2];
                  glar_ColorList[4*ii+2] = EngineData->fm[i][3];
                  glar_ColorList[4*ii+3] = 0.5;
               }
            }
            break;
            
         case SE_FlipLight0Pos:
            /* expects nothing in EngineData */
            sv->light0_position[2] *= -1;
            glLightfv(GL_LIGHT0, GL_POSITION, sv->light0_position);
            break;
         
         case SE_SetLight0Pos:
            /* expects light XYZ position in fv[3] */
            if (EngineData->fv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            }
            sv->light0_position[0] = EngineData->fv3[0];
            sv->light0_position[1] = EngineData->fv3[1];
            sv->light0_position[2] = EngineData->fv3[2];
            glLightfv(GL_LIGHT0, GL_POSITION, sv->light0_position);
            break;
            
         case SE_HighlightNodes:
            /* highlight nodes inside the search box */
            /* expects Node XYZ in EngineData->fv15[0..2]
            Box dimensions in EngineData->fv15[3..5] */
            if (EngineData->fv15_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            }
            {
               SUMA_ISINBOX IB;
               
               SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
               ND = SO->NodeDim;
            
               SUMA_etime (&tt, 0);
               IB = SUMA_isinbox (SO->NodeList, SO->N_Node, &(EngineData->fv15[0]), &(EngineData->fv15[3]),  YUP);
               delta_t = SUMA_etime (&tt, 1);
               fprintf (SUMA_STDOUT,"Elapsed time for isinbox operation: %f\n", delta_t);
               fprintf (SUMA_STDOUT,"\t%d nodes (out of %d) found in box\n",IB.nIsIn, SO->N_Node);
               
               if (IB.nIsIn) { /* found some, find the closest node */
                  /* locate the closest node and store it's id in EngineData*/
                  SUMA_MIN_LOC_VEC (IB.d, IB.nIsIn, ft, it);
                  
                  /* XYZ and normal of the closets to the center */
                  #ifdef STUFF
                     /* This is not being used and if it is to be used, EngineData should 
                     not be set manually */
                     id = ND * IB.IsIn[it];
                     EngineData->fv15[0] = SO->NodeList[id];
                     EngineData->fv15[1] = SO->NodeList[id+1];
                     EngineData->fv15[2] = SO->NodeList[id+2];
                     EngineData->fv15[3] = SO->NodeNormList[id];
                     EngineData->fv15[4] = SO->NodeNormList[id+1];
                     EngineData->fv15[5] = SO->NodeNormList[id+2];
                  #endif
                  /* Color the nodes*/
                  fm = (float **)SUMA_allocate2D(IB.nIsIn, 4, sizeof(float));
                  if (fm == NULL) {
                     fprintf(SUMA_STDERR,"Error %s: Could not allocate for fm.\n", FuncName);
                     break;
                  }
                  for (i=0; i < IB.nIsIn; ++i) {
                      /* id = ND * IB.IsIn[i]; */
                      /*fprintf (SUMA_STDOUT,"\t[%d] %f %f %f\n", IB.IsIn[i] ,\
                                   SO->NodeList[id], SO->NodeList[id+1], SO->NodeList[id+2]);*/
                     /* color those nodes in yellow, just for kicks */
                     fm[i][0] = (float)IB.IsIn[i];
                     fm[i][1] = 0; 
                     fm[i][2] = 0.4;
                     fm[i][3] = 0.4; 
                  }

                  /* Place a call to Redisplay and SetNodeColor */
                  ED = SUMA_InitializeEngineListData (SE_SetNodeColor);
                  ED->N_cols = 4;
                  ED->N_rows = IB.nIsIn;
                  if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                         SEF_fm, (void*)fm,
                                                         SES_Suma, (void *)sv, NOPE,
                                                         SEI_Head, NULL)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
                     break;                                      
                  } 
                  ED = SUMA_InitializeEngineListData (SE_Redisplay);
                  SUMA_RegisterEngineListCommand (  list, ED, 
                                                    SEF_Empty, NULL,
                                                    SES_Suma, (void *)sv, NOPE, 
                                                    SEI_Head, NULL);

                  /* free fm since it was copied to EngineData*/
                  if (fm) SUMA_free2D ((char **)fm, IB.nIsIn);
                  
                  /* get ridd of IB's vectors */
                  if (!SUMA_Free_IsInBox (&IB)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to free IB\n", FuncName);
                  }
               } else { /* no node is close enough */
                  /* Do nothing yet */
                  fprintf (SUMA_STDOUT,"\nNo nodes found inside the specified box.\n");
               }
            }
            break;

         case SE_GetNearestNode:
            /* lookfor nodes inside the search box */
            /* expects Node XYZ in EngineData->fv15[0..2]
            Box dimensions in EngineData->fv15[3..5] */
            if (EngineData->fv15_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            }
            {
               SUMA_ISINBOX IB;
               
               SO = (SUMA_SurfaceObject *)(SUMAg_DOv[sv->Focus_SO_ID].OP);
               ND = SO->NodeDim;
               SUMA_etime (&tt, 0);
               IB = SUMA_isinbox (SO->NodeList, SO->N_Node, &(EngineData->fv15[0]), &(EngineData->fv15[3]),  YUP);
               delta_t = SUMA_etime (&tt, 1);
               fprintf (SUMA_STDOUT,"Elapsed time for isinbox operation: %f\n", delta_t);
               fprintf (SUMA_STDOUT,"\t%d nodes (out of %d) found in box\n",IB.nIsIn, SO->N_Node);
               
               if (IB.nIsIn) { /* found some, find the closest node */
                  /* locate the closest node and store it's id in EngineData*/
                  SUMA_MIN_LOC_VEC (IB.d, IB.nIsIn, ft, it);
                  
                  /* get the XYZ and normal of that node */
                  id = ND * IB.IsIn[it];
                  fv15[0] = SO->NodeList[id];
                  fv15[1] = SO->NodeList[id+1];
                  fv15[2] = SO->NodeList[id+2];
                  fv15[3] = SO->NodeNormList[id];
                  fv15[4] = SO->NodeNormList[id+1];
                  fv15[5] = SO->NodeNormList[id+2];
                  
                  ED = SUMA_InitializeEngineListData (SE_SetLookAtNode);
                  if (!SUMA_RegisterEngineListCommand (  list, ED,
                                                         SEF_fv15, (void *)fv15, 
                                                         SES_Suma, (void *)sv, NOPE, 
                                                         SEI_Head, NULL)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
                     break;
                  }
                  
                  /* get ridd of IB's vectors */
                  if (!SUMA_Free_IsInBox (&IB)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed to free IB\n", FuncName);
                  }
               } else { /* no node is close enough */
                  /* Do nothing yet */
               }
            }
            break;
            
         case SE_SetRotMatrix:
            /* expects a rotation matrix in fm, 4x4 */
            /* takes the rotation matrix 3x3 with 0 in 4th row and column and 1.0 at 4,4 
            makes a quaternion from it and sets csv->currentQuat and posts redisplay */
            if (EngineData->fm_Dest != NextComCode) {
               fprintf (SUMA_STDERR,"Error %s: Data not destined correctly for %s (%d).\n",FuncName, NextCom, NextComCode);
               break;
            }
            if (EngineData->N_rows != 4 || EngineData->N_cols != 4) {
               fprintf(SUMA_STDERR,"Error %s: fm must have 4 cols and 4 rows in SetRotMatrix\n", FuncName);
               break;
            }
            if (!SUMA_mattoquat (EngineData->fm, sv->GVS[sv->StdView].currentQuat))
               {
                  fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_mattoquat\n", FuncName);
                  break;
               }
            break;
            
         /*case SE_Something:
            break;*/

         case SE_BadCode:
            fprintf(SUMA_STDERR,"Error SUMA_Engine: Command ->%s<- Not understood. Perhaps Code is not defined in SUMA_CommandCode\n", NextCom);
            break;
         
      } /* switch NextComCode */
      
      /* release used Element */
      if (LocalHead) fprintf (SUMA_STDERR, "\n%s: Releasing Engine Element.\n", FuncName);
      if (!SUMA_ReleaseEngineListElement (list, NextElem_CANT_TOUCH_THIS)) {
            fprintf(SUMA_STDERR,"Error SUMA_Engine: Failed to Release element \n");
         }
         
   } /* cycle through NextCom */
   
   if (LocalHead) fprintf (SUMA_STDERR, "\n%s: Destroying List.\n", FuncName);
   /* If you get here, all is well, destroy the list since it is empty*/
   list = SUMA_DestroyList (list);
   *listp = NULL; 
   
   SUMA_RETURN (YUP);
}

/*!
   ans = SUMA_RegisteredSOs (sv, dov, SO_IDs);
   gets the IDs (indices into dov) and number of the Surface Objects shown in sv
   \param sv (SUMA_SurfaceViewer *) the surface viewer structure
   \param dov (SUMA_DO *) the Displayable Objects vector (accessible to sv)
   \param SO_IDs (int *) pre-allocated integer vector that will contain the IDs of the SO shown in sv
         send NULL if you do not care for it and all you'll get is ans
   \ret ans (int) the number of SOs shown in SV
   Still confused ? read the code for the function, it is shorter than the documentation.
*/
int SUMA_RegisteredSOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *SO_IDs)
{
   static char FuncName[]={"SUMA_RegisteredSOs"};
   int i, k = 0;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   for (i=0; i< sv->N_DO; ++i) {
      if (SUMA_isSO(dov[sv->RegisteredDO[i]])) {
         if (SO_IDs != NULL) SO_IDs[k] = sv->RegisteredDO[i];
         ++k;
      }
   }

   SUMA_RETURN (k);
}
/*!
   ans = SUMA_VisibleSOs (sv, dov, SO_IDs);
   gets the IDs (indices into dov) and number of the Surface Objects 
         registered with sv and with the SO->Side matching sv->ShowRight/
         sv->ShowLeft
   \param sv (SUMA_SurfaceViewer *) the surface viewer structure
   \param dov (SUMA_DO *) the Displayable Objects vector (accessible to sv)
   \param SO_IDs (int *) pre-allocated integer vector that will contain the IDs of the SO shown in sv
         send NULL if you do not care for it and all you'll get is ans
   \ret ans (int) the number of SOs shown in SV
   Still confused ? read the code for the function, it is shorter than the documentation.
*/
int SUMA_VisibleSOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *SO_IDs)
{
   static char FuncName[]={"SUMA_VisibleSOs"};
   SUMA_SurfaceObject *SO=NULL;
   int i, k = 0;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   for (i=0; i< sv->N_DO; ++i) {
      if (SUMA_isSO(dov[sv->RegisteredDO[i]])) {
         SO = (SUMA_SurfaceObject *)dov[sv->RegisteredDO[i]].OP;
         if ( SO->Side == SUMA_NO_SIDE || SO->Side == SUMA_SIDE_ERROR ) {
            if (SO_IDs) {
               SO_IDs[k] = sv->RegisteredDO[i];
            }
            ++k;
         } else if (  (SO->Side == SUMA_RIGHT && sv->ShowRight) || 
                      (SO->Side == SUMA_LEFT && sv->ShowLeft) ) {
            if (SO_IDs) {
               SO_IDs[k] = sv->RegisteredDO[i];
            }
            ++k;
         }
      }
   }

   SUMA_RETURN (k);
}

/*! 
   nxtState = SUMA_NextState(sv);

   get the next Viewing State available in sv
   \param sv (SUMA_SurfaceViewer *) pointer to surface viewer structure 
   \ret nxtState (int) the index into sv->VSv of the next state
      -1 if there is trouble
   \sa SUMA_PrevState 
*/
int SUMA_NextState(SUMA_SurfaceViewer *sv)
{
   static char FuncName[] = {"SUMA_NextState"};
   int inxt, icur;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   icur = SUMA_WhichState (sv->State, sv);
   if (icur < 0) {
      fprintf(SUMA_STDERR,"Error %s: SUMA_WhichState failed.\n", FuncName);
      SUMA_RETURN (-1);
   } else {
      SUMA_RETURN((icur + 1) % sv->N_VSv);
   }
   
   SUMA_RETURN (-1);
}

/*!
   precState = SUMA_PreviState (sv);
   get the previous Viewing State available in sv
   \sa SUMA_NextState
*/
int SUMA_PrevState(SUMA_SurfaceViewer *sv)
{
   static char FuncName[] = {"SUMA_PrevState"};
   int inxt, icur;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   icur = SUMA_WhichState (sv->State, sv);   if (icur < 0) {
      fprintf(SUMA_STDERR,"Error %s: SUMA_WhichState failed.\n", FuncName);
      SUMA_RETURN (-1);
   } else {
      icur = icur -1;
      if (icur < 0) icur = sv->N_VSv + icur;
      SUMA_RETURN(icur);
   }
   
   SUMA_RETURN (-1);
}



/*!
   SOnxtID = SUMA_NextSO (dov, n_dov, CurrentIDcode, SOnxt);
   Get the next Surface Object in DOv
   \param dov (SUMA_DO *) vector containing all displayable objects
   \param n_dov (int) number of elements in dov
   \param CurrentIDcode (char *) idcode of current surface
   \param SOnxt (SUMA_SurfaceObject *) pointer to next surface object 
   \ret SOnxtID (int) index into dov of SOnxt (-1) if there's an error
*/

int SUMA_NextSO (SUMA_DO *dov, int n_dov, char *idcode, SUMA_SurfaceObject *SOnxt)
{
   static char FuncName[] = {"SUMA_NextSO"};
   int icur, icheck, ncheck;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (SOnxt != NULL) {
      fprintf(SUMA_STDERR,"Error %s: SOnxt should be null when you call this function.\n", FuncName);
      SUMA_RETURN (-1);
   }
   if (n_dov < 1) {
      fprintf(SUMA_STDERR,"Error %s: dov contains no elements.\n", FuncName);
      SUMA_RETURN (-1);
   }
   icur = SUMA_findSO_inDOv (idcode, dov, n_dov);
   if (icur < 0) {
      fprintf (SUMA_STDERR,"Error %s: idcode not found in dov.\n", FuncName);
      SUMA_RETURN (-1);
   }
   
   ncheck = 0;
   icheck = icur;
   while (ncheck < n_dov) {
      icheck = (icheck + 1) % n_dov;
      /*fprintf(SUMA_STDERR,"%s: Checking %d\n", FuncName, icheck);*/
      if (SUMA_isSO(dov[icheck])) {
         /*fprintf(SUMA_STDERR,"%s: Settling on %d\n", FuncName, icheck);*/
         SOnxt = (SUMA_SurfaceObject *)dov[icheck].OP;
         SUMA_RETURN (icheck);
      }
      ++ncheck;
   }
   /* should not get here */
   SUMA_RETURN (-1);
}

/*! 
   Replaces one surface in RegisteredDO with another 

*/
SUMA_Boolean SUMA_SwitchSO (SUMA_DO *dov, int N_dov, int SOcurID, int SOnxtID, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_SwitchSO"};
   SUMA_Axis *EyeAxis;
   int EyeAxis_ID;
   char CommString[100];
   SUMA_EngineData ED;
   DList *list = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* unregister the current surface from RegisteredDO */
   /*fprintf(SUMA_STDERR,"%s: Unregistering DOv[%d]...\n", FuncName, SOcurID);*/
   if (!SUMA_UnRegisterDO(SOcurID, sv)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to UnRegisterDO.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   /* set the focus ID to the current surface */
   sv->Focus_SO_ID = SOnxtID;

   /* register the new surface in RegisteredDO */
   /*fprintf(SUMA_STDERR,"%s: Registering DOv[%d]...\n", FuncName, sv->Focus_SO_ID); */
   if (!SUMA_RegisterDO(sv->Focus_SO_ID, sv)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to RegisterDO.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   /* modify the rotation center */
   if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update center of rotation", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* set the viewing points */
   if (!SUMA_UpdateViewPoint(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update view point", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* Change the defaults of the eye axis to fit standard EyeAxis */
   EyeAxis_ID = SUMA_GetEyeAxis (sv, dov);

   if (EyeAxis_ID < 0) {
      fprintf(SUMA_STDERR,"Error %s: No Eye Axis. %d\n", FuncName, EyeAxis_ID);
   } else {
      EyeAxis = (SUMA_Axis *)(dov[EyeAxis_ID].OP);
      SUMA_EyeAxisStandard (EyeAxis, sv);
   }
   
   /* Home call baby */
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Home, SES_Suma, sv);

   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
   }

   
   /* take care of the cross hair's XYZ */

   /* to do elsewhere */
   /* when a cross hair needs to be communicated, you must use the MapRef_idcode_str surface and not the Focus_Surface */
   SUMA_RETURN (YUP);
}

/*! 
   ans = SUMA_SwitchState (dov, N_dov, sv, nxtstateID);
   
   Replaces one viewing state with another

*/
SUMA_Boolean SUMA_SwitchState (SUMA_DO *dov, int N_dov, SUMA_SurfaceViewer *sv, int nxtstateID)
{
   static char FuncName[]={"SUMA_SwitchState"};
   SUMA_Axis *EyeAxis;
   int EyeAxis_ID, I_C, OverInd, ND, id;
   char CommString[100];
   SUMA_EngineData ED;
   int curstateID, i, j, jmax, prec_ID;
   SUMA_SurfaceObject *SO_nxt, *SO_prec;
   float *XYZ, *XYZmap;
   DList *list = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   XYZ = NULL;
   XYZmap = NULL;
   
   curstateID = SUMA_WhichState(sv->State, sv);
   
   /* unregister all the surfaces for the current view */
   if (LocalHead) fprintf(SUMA_STDERR,"Local Debug %s: Unregistering \n", FuncName);
   for (i=0; i<sv->VSv[curstateID].N_MembSOs; ++i) {
      if (!SUMA_UnRegisterDO(sv->VSv[curstateID].MembSOs[i], sv)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to UnRegisterDO.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
   }
   
   /* register all the surfaces from the next view */
   if (LocalHead) fprintf(SUMA_STDERR,"Local Debug %s: Registering DOv...\n", FuncName);
   for (i=0; i<sv->VSv[nxtstateID].N_MembSOs; ++i) {
      if (!SUMA_RegisterDO(sv->VSv[nxtstateID].MembSOs[i], sv)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to RegisterDO.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
   }
   
   /*set the Color Remix flag */
   if (!SUMA_SetShownLocalRemixFlag (sv)) {
      fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_SetShownLocalRemixFlag.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   
   /* if no coloroverlay exists, link to MapReference surface, if possible */
   for (i=0; i<sv->VSv[nxtstateID].N_MembSOs; ++i) {
      /* next surface being checked */
      SO_nxt = (SUMA_SurfaceObject *)(dov[sv->VSv[nxtstateID].MembSOs[i]].OP);

      /* Get the Mapping Reference surface, that's the precursor*/
      if (!SO_nxt->MapRef_idcode_str) {
         prec_ID = -1;
      }else {
         prec_ID = SUMA_findSO_inDOv(SO_nxt->MapRef_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
      }
      if (prec_ID < 0) {
         /* no precursors found, notify user */
         fprintf(SUMA_STDERR, "\n\aWarning %s: No precursors found for surface %d.\nColors, selected nodes and facesets will not be reflect those in previous state.\n.",\
          FuncName, sv->VSv[nxtstateID].MembSOs[i]);
         continue;
      }

      SO_prec = (SUMA_SurfaceObject *)(dov[prec_ID].OP);

      /* check for risk of node inconsistencies */

      if (SO_prec->N_Node >= SO_nxt->N_Node ) {/* > or equal number of nodes*/
         /* matching number of nodes */
         /* Create a link to each overlay plane in the precursor unless such a plane exists already  */
         for (j=0; j < SO_prec->N_Overlays; ++j) {
            if (!SUMA_Fetch_OverlayPointer (SO_nxt->Overlays, SO_nxt->N_Overlays, SO_prec->Overlays[j]->Name, &OverInd)) {
               /* plane not found, create a link to it */
               if (LocalHead) fprintf (SUMA_STDERR,"Local Debug %s: Overlay plane %s not found, creating the link.\n", FuncName, SO_prec->Overlays[j]->Name);
               SO_nxt->Overlays_Inode[SO_nxt->N_Overlays] = SUMA_CreateInodeLink (SO_nxt->Overlays_Inode[SO_nxt->N_Overlays], SO_prec->Overlays_Inode[j]);
               if (!SO_nxt->Overlays_Inode[SO_nxt->N_Overlays]) {
                  fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInodeLink\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
               /* now copy the actual overlay plane pointer */
               SO_nxt->Overlays[SO_nxt->N_Overlays] = SO_prec->Overlays[j];
               /*increment the number of overlay planes */
               ++SO_nxt->N_Overlays;
            } else {
               /* plane found, do nothing */
               if (LocalHead) fprintf (SUMA_STDERR,"Local Debug %s: Overlay plane %s found. Index#%d\n.", FuncName, SO_prec->Overlays[j]->Name, OverInd);
            }
         }


         if (SO_prec->N_Node > SO_nxt->N_Node) {/* More in prec */
            /* just warn */
            fprintf(SUMA_STDERR, "Warning %s: More nodes (%d) in precursor surface. \n Assuming upcoming surface is a subset of precursor.\n", FuncName, SO_prec->N_Node - SO_nxt->N_Node);
         }/* More in prec */ 

         /* link the selected nodes and facesets, if possible */
         /*fprintf(SUMA_STDERR, "%s: Linking selected nodes  ...\n", FuncName);*/
         /* check for risk of node inconsistencies */
         if (SO_prec->N_Node == SO_nxt->N_Node) {
            SO_nxt->SelectedNode = SO_prec->SelectedNode;
            } else { /* more nodes in precursor, make sure selected node is OK */
            if (SO_prec->SelectedNode < SO_nxt->N_Node) {
               SO_nxt->SelectedNode = SO_prec->SelectedNode;
               } else { /* this node does not exist in the upcoming thing */
               fprintf(SUMA_STDERR, "\n\aWarning %s: Slected node in precursor state does not exist in current state.\n Selected Node is left at previous setting in this view state.\n", FuncName);
               }
            }

         } /* > or equal number of nodes */ else { /* less in prec */
            fprintf(SUMA_STDERR, "\n\aWarning %s: More nodes (%d) in upcoming surface. Colors, selected nodes and facesets are not carried through from precursor.\n", FuncName, SO_nxt->N_Node - SO_prec->N_Node);
         }

         #if 0
         /* You do not want to mix colors yet, the flag for doing that has already been set*/
         /* Here you need to remix the colors */
         if (!SUMA_Overlays_2_GLCOLAR4(SO_nxt->Overlays, SO_nxt->N_Overlays, SUMA_GetColorList (sv, SO_nxt->idcode_str), SO_nxt->N_Node,\
             sv->Back_Modfact, sv->ShowBackground, sv->ShowForeground)) {
            fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Overlays_2_GLCOLAR4.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         #endif
         
      }
   
   /* Bind the cross hair to a reasonable surface, if possible */
   if (sv->Ch->SurfaceID >= 0) {      
      if (LocalHead) fprintf(SUMA_STDERR, "Local Debug %s: Linking Cross Hair via SurfaceID...\n", FuncName);
      j = SUMA_MapRefRelative (sv->Ch->SurfaceID, sv->VSv[nxtstateID].MembSOs, sv->VSv[nxtstateID].N_MembSOs, dov);
      if (LocalHead) fprintf(SUMA_STDERR, "Local Debug %s: Cross Hair's  New SurfaceID = %d\n", FuncName, j );
      
      /* set the XYZ of the cross hair based on the coordinates of the upcoming surface, if possible */
      if (j >= 0) {
         SO_nxt = (SUMA_SurfaceObject *)(dov[j].OP);
         ND = SO_nxt->NodeDim;
         id = ND * sv->Ch->NodeID;
         if (sv->Ch->NodeID >= 0) {
            if (LocalHead) fprintf(SUMA_STDERR, "Local Debug %s: Using NodeID for link.\n", FuncName);
            sv->Ch->c[0] = SO_nxt->NodeList[id];
            sv->Ch->c[1] = SO_nxt->NodeList[id+1];
            sv->Ch->c[2] = SO_nxt->NodeList[id+2];
         } else {
            /* no node associated with cross hair, use XYZ */
            if (LocalHead) fprintf(SUMA_STDERR, "Local Debug %s: Using XYZ for link.\n", FuncName);
            SO_prec = (SUMA_SurfaceObject *)(dov[sv->Ch->SurfaceID].OP);
            /* go from XYZ to XYZmap on current surface then from XYZmap to XYZ on new surface */
            I_C = -1;
            XYZmap = SUMA_XYZ_XYZmap (sv->Ch->c, SO_prec, dov, N_dov, &I_C);
            if (XYZmap == NULL) {
               fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_XYZ_XYZmap\n", FuncName); 
            }else {
               XYZ = SUMA_XYZmap_XYZ (XYZmap, SO_nxt, dov, N_dov, &I_C);
               if (XYZ == NULL) {
                  fprintf(SUMA_STDERR, "Error %s: Failed in SUMA_XYZmap_XYZ\n", FuncName); 
               } else {
                  sv->Ch->c[0] = XYZ[0];
                  sv->Ch->c[1] = XYZ[1];
                  sv->Ch->c[2] = XYZ[2];
               }
               
            }
            if (XYZ) SUMA_free(XYZ);
            if (XYZmap) SUMA_free(XYZmap);
         }
      } else {
         fprintf(SUMA_STDERR, "%s: No relatives between states. CrossHair location will not correspond between states\n", FuncName); 
      }
       sv->Ch->SurfaceID = j;
      if (LocalHead) fprintf(SUMA_STDERR, "Local Debug %s: Linking Cross Hair Via NodeID Done.\n", FuncName);
   }
   


   /* switch the state accordingly */
   sv->State =  sv->VSv[nxtstateID].Name;
   sv->iState = nxtstateID;
   
   /* set the focus ID to the first surface in the next view   */
   sv->Focus_SO_ID = sv->VSv[nxtstateID].MembSOs[0];

   /* decide what the best state is */
   sv->StdView = SUMA_BestStandardView (sv,dov, N_dov);
   if (LocalHead) fprintf(SUMA_STDOUT,"%s: Standard View Now %d\n", FuncName, sv->StdView);
   if (sv->StdView == SUMA_Dunno) {
      fprintf(SUMA_STDERR,"Error %s: Could not determine the best standard view. Choosing default SUMA_3D\n", FuncName);
      sv->StdView = SUMA_3D;
   }

   /* modify the rotation center */
   if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update center of rotation", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* set the viewing points */
   if (!SUMA_UpdateViewPoint(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update view point", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* Change the defaults of the eye axis to fit standard EyeAxis */
   EyeAxis_ID = SUMA_GetEyeAxis (sv, dov);

   if (EyeAxis_ID < 0) {
      fprintf(SUMA_STDERR,"Error %s: No Eye Axis. %d\n", FuncName, EyeAxis_ID);
   } else {
      EyeAxis = (SUMA_Axis *)(dov[EyeAxis_ID].OP);
      SUMA_EyeAxisStandard (EyeAxis, sv);
   }
   
    
   /* Home call baby */
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Home, SES_Suma, sv);
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
   }

   SUMA_RETURN (YUP);
}

/*!
\brief ans = SUMA_OpenGLStateReset (dov, N_dov, sv);
Used when going from one surface viewer to another. The OpenGL state variables 
need to be reset when moving from one viewer to the next. Otherwise you risk having 
unpredictable results the first time you do something in one viewer after you'd been
in another. 
This function is a stripped down version of SUMA_SwitchState and should 
be followed by a call to SUMA_postRedisplay for all the changes to take effect.
Do not try executing all the commands in SUMA_display that affect the modelview 
matrix and the projection matrix without calling for a display the changes will not take effect.

\param dov (SUMA_DO *) Pointer to vector of displayable objects, typically SUMAg_DOv
\param N_dov (int) number of elements in dov, typically SUMAg_N_DOv
\param sv (SUMA_SurfaceViewer *) viewer making the request.
\return YUP/NOPE Good/Bad

*/
SUMA_Boolean SUMA_OpenGLStateReset (SUMA_DO *dov, int N_dov, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_OpenGLStateReset"};
   SUMA_Axis *EyeAxis;
   int EyeAxis_ID, I_C, OverInd, ND, id;
   char CommString[100];
   SUMA_EngineData ED;
   int  i, j, jmax, prec_ID;
   SUMA_SurfaceObject *SO_nxt, *SO_prec;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);   
   
   #if 0
   /* modify the rotation center */
   if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update center of rotation", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* set the viewing points */
   if (!SUMA_UpdateViewPoint(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update view point", FuncName);
      SUMA_RETURN (NOPE);
   }
   #endif
   
   /* This is all that is needed, the others above do not need to be updated at this stage*/
   
   /* Change the defaults of the eye axis to fit standard EyeAxis */
   EyeAxis_ID = SUMA_GetEyeAxis (sv, dov);

   if (EyeAxis_ID < 0) {
      fprintf(SUMA_STDERR,"Error %s: No Eye Axis. %d\n", FuncName, EyeAxis_ID);
   } else {
      EyeAxis = (SUMA_Axis *)(dov[EyeAxis_ID].OP);
      SUMA_EyeAxisStandard (EyeAxis, sv);
   }
   

   #if 0
   /* force an axis drawing to set the projection matrix correctly */
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Setting up matrix mode and perspective ...\n", FuncName);
   glMatrixMode (GL_PROJECTION);
   glLoadIdentity ();
   gluPerspective((GLdouble)sv->FOV[sv->iState], sv->Aspect, SUMA_PERSPECTIVE_NEAR, SUMA_PERSPECTIVE_FAR); /*lower angle is larger zoom,*/
   #endif
   
   /* You still need to call SUMA_display via SUMA_postRedisplay but that is done after this function returns */ 

   SUMA_RETURN (YUP);
}

/*!
   EyeAxisID = SUMA_GetEyeAxis (sv, dov);
   gets the ID (indices into dov) of the Eye Axis in sv
   \param sv (SUMA_SurfaceViewer *) the surface viewer structure
   \param dov (SUMA_DO *) the Displayable Objects vector (accessible to sv)
   \param Ax (SUMA_Axis *) a pointer to the Eye Axis structure (NULL if error )
   \ret EyeAxisID (int) the index into dov of the Eye Axis 
      if an error is encountered, including more than one Eye Axis, a -1 is returned
*/
int SUMA_GetEyeAxis (SUMA_SurfaceViewer *sv, SUMA_DO *dov)
{
   static char FuncName[]={"SUMA_GetEyeAxis"};
   int i, k = -1, cnt = 0;
   SUMA_Axis *AO;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   for (i=0; i< sv->N_DO; ++i) {
      if (dov[sv->RegisteredDO[i]].ObjectType == AO_type) {
         AO = (SUMA_Axis *)(dov[sv->RegisteredDO[i]].OP);
         if (strcmp(AO->Name, "Eye Axis") == 0) {
            k = sv->RegisteredDO[i];
            ++cnt;
         }
      }
   }
   if (cnt > 1) {
      fprintf (SUMA_STDERR,"Error %s: Found more than one Eye Axis. \n", FuncName);
      SUMA_RETURN (-1);
   }
   
   SUMA_RETURN (k);
}

/*! 
   transform current XYZ to XYZmap 
   The XYZ on an auxilliary surface are of no relevance to the volume. They must be transformed
   to mappable XYZ (in mm, RAI, in alignment with the Parent Volume)   
   XYZmap = SUMA_XYZ_XYZmap (XYZ, SO, dov, N_dov, I_C);

   \param XYZ (float *) XYZ triplet in SO's native coordinate space
   \param SO (SUMA_SurfaceObject *SO) obvious, ain't it
   \param dov (SUMA_DO*) vector containing all displayable objects
   \param N_dov (int) number of elements in dov
   \param I_C (int *) (pre allocated) pointer to the index of the closest (or representative) node 
                       in SO to the XYZ location. If you do not have it, make sure *I_C = -1. If you
                       do so, the function will search for nodes contained in a box mm wide
                       and centered on XYZ. If nodes are found in the box the I_C is set to the
                       index of the closest node and XYZmap contains the coordinates of I_C in the 
                       SO->MapRef_idcode_str surface.
   \ret XYZmap (float *) Mappable XYZ coordinates. NULL in case of trouble.

*/

float * SUMA_XYZ_XYZmap (float *XYZ, SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov, int *I_C)
{/* SUMA_XYZ_XYZmap */
   static char FuncName[]={"SUMA_XYZ_XYZmap"};
   float *XYZmap;
   int iclosest, id, ND;
   SUMA_SurfaceObject *SOmap;
   int SOmapID;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* allocate for return */
   XYZmap = (float *)SUMA_calloc (3, sizeof(float));
   if (XYZmap == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Could not allocate for XYZmap.\n", FuncName);
      SUMA_RETURN (NULL);
   } 
   
   /* if surface is Inherently mappable, do the obivious */
   if (SUMA_isINHmappable(SO)){
      /*fprintf(SUMA_STDERR,"%s: Surface is inherently mappable. XYZmap = XYZ.\n", FuncName); */
      SUMA_COPY_VEC (XYZ, XYZmap, 3, float, float);
      SUMA_RETURN (XYZmap);   
   }
   /* if surface is not Inherrently mappable, do the deed */
   if (!SUMA_ismappable(SO)){
      fprintf(SUMA_STDERR,"%s: Surface is NOT mappable, returning NULL.\n", FuncName);
      SUMA_free(XYZmap);
      SUMA_RETURN (NULL);
   }

   /* surface is mappable, things will get more complicated */

   /* find the closest node in SO */
   if (*I_C < 0) { /* user has not specified closest node ID*/
      /* must find closest node on my own */
         {
            SUMA_ISINBOX IB;
            float Bd[3], distance;
            int ii;
            
            /* set the search box dimensions */
            Bd[0] = Bd[1] = Bd[2] = SUMA_XYZ_XFORM_BOXDIM_MM;
            IB = SUMA_isinbox (SO->NodeList, SO->N_Node, XYZ, Bd,  YUP);
            fprintf (SUMA_STDOUT,"%s: %d nodes (out of %d) found in box\n",FuncName, IB.nIsIn, SO->N_Node);

            if (IB.nIsIn) { /* found some, find the closest node */
               /* locate the closest node and store it's id in EngineData*/
               /*for (ii=0; ii<IB.nIsIn; ++ii) {
                  fprintf (SUMA_STDERR,"%d\t%.3f\t\t", IB.IsIn[ii], IB.d[ii]);
               }*/
               SUMA_MIN_LOC_VEC (IB.d, IB.nIsIn, distance, iclosest);
               iclosest = IB.IsIn[iclosest];
               /* get ridd of IB's vectors */
               if (!SUMA_Free_IsInBox (&IB)) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to free IB\n", FuncName);
               }

            } else { /* no node is close enough */
               fprintf (SUMA_STDERR,"%s: No node was close enough to XYZ, no linkage possible\n", FuncName);
               SUMA_free(XYZmap);
               SUMA_RETURN (NULL);
            }
            /* store iclosest for lazy user */
            *I_C = iclosest;
         }
   } else { 
      iclosest = *I_C;
   }
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Node identified for linking purposes is %d\n", FuncName, *I_C);
   /* find the SO that is the Mappable cahuna */
   SOmapID = SUMA_findSO_inDOv(SO->MapRef_idcode_str, dov, N_dov);
   if (SOmapID < 0) {
      fprintf (SUMA_STDERR,"%s: Failed in SUMA_findSO_inDOv This should not happen.\n", FuncName);
      SUMA_free(XYZmap);
      SUMA_RETURN (NULL);
   }

   SOmap = (SUMA_SurfaceObject *)(dov[SOmapID].OP);
   ND = SOmap->NodeDim;
   id = ND * iclosest;
   XYZmap[0]=SOmap->NodeList[id];
   XYZmap[1]=SOmap->NodeList[id+1];
   XYZmap[2]=SOmap->NodeList[id+2];

   /* all is done */

   SUMA_RETURN (XYZmap);
}/* SUMA_XYZ_XYZmap */

/*! 
   transform  XYZmap to XYZ on current surface
   
   XYZ = SUMA_XYZmap_XYZ (XYZmap, SO, dov, N_dov, I_C);

   \param XYZmap (float *) XYZmap triplet in SO's MapRef coordinate space
   \param SO (SUMA_SurfaceObject *SO) obvious, ain't it
   \param dov (SUMA_DO*) vector containing all displayable objects
   \param N_dov (int) number of elements in dov
   \param I_C (int *) (pre allocated) pointer to the index of the closest (or representative) node 
                       in SO's MapRef to the XYZmap location. If you do not have it, make sure *I_C = -1. If you
                       do so, the function will search for nodes contained in a box mm wide
                       and centered on XYZmap. If nodes are found in the box the I_C is set to the
                       index of the closest node and XYZ contains the coordinates of I_C in the 
                       SO surface.
   \ret XYZ (float *) Equivalent of XYZmap on the auxilliary surface SO. NULL in case of trouble.

   \sa SUMA_XYZ_XYZmap
*/

float * SUMA_XYZmap_XYZ (float *XYZmap, SUMA_SurfaceObject *SO, SUMA_DO* dov, int N_dov, int *I_C)
{/* SUMA_XYZmap_XYZ */
   static char FuncName[]={"SUMA_XYZmap_XYZ"};
   float *XYZ;
   int iclosest, id, ND;
   SUMA_SurfaceObject *SOmap;
   int SOmapID;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* allocate for return */
   XYZ = (float *)SUMA_calloc (3, sizeof(float));
   if (XYZ == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Could not allocate for XYZ.\n", FuncName);
      SUMA_RETURN (NULL);
   } 
   
   /* if surface is not mappable, do the deed */
   if (!SUMA_ismappable(SO)){
      fprintf(SUMA_STDERR,"%s: Surface is NOT mappable, returning NULL.\n", FuncName);
      SUMA_free(XYZ);
      SUMA_RETURN (NULL);
   }

   /* if surface is Inherently mappable, do the obivious */
   if (SUMA_isINHmappable(SO)){
      fprintf(SUMA_STDERR,"%s: Surface is inherently mappable. XYZ = XYZmap.\n", FuncName);
      SUMA_COPY_VEC (XYZmap, XYZ, 3, float, float);
      SOmap = SO;
      /* do not return yet, must fix the node id too */
   } else {
      /* surface is mappable, things will get more complicated */
      /* find the SO that is the Mappable cahuna */
      SOmapID = SUMA_findSO_inDOv(SO->MapRef_idcode_str, dov, N_dov);
      if (SOmapID < 0) {
         fprintf (SUMA_STDERR,"%s: Failed in SUMA_findSO_inDOv This should not happen.\n", FuncName);
         SUMA_free(XYZ);
         SUMA_RETURN (NULL);
      }
      SOmap = (SUMA_SurfaceObject *)(dov[SOmapID].OP);
   }
   /* find the closest node in SO */
   if (*I_C < 0) { /* user has not specified closest node ID*/
      /* must find closest node on my own */
         {
            SUMA_ISINBOX IB;
            float Bd[3], distance;
            int ii;
            
            /* set the search box dimensions */
            Bd[0] = Bd[1] = Bd[2] = SUMA_XYZ_XFORM_BOXDIM_MM;
            IB = SUMA_isinbox (SOmap->NodeList, SOmap->N_Node, XYZmap, Bd,  YUP);
            fprintf (SUMA_STDERR,"%s: %d nodes (out of %d) found in box\n",FuncName, IB.nIsIn, SOmap->N_Node);

            if (IB.nIsIn) { /* found some, find the closest node */
               /* locate the closest node and store it's id in EngineData*/
               /*for (ii=0; ii<IB.nIsIn; ++ii) {
                  fprintf (SUMA_STDERR,"%d\t%.3f\t\t", IB.IsIn[ii], IB.d[ii]);
               }*/
               SUMA_MIN_LOC_VEC (IB.d, IB.nIsIn, distance, iclosest);
               iclosest = IB.IsIn[iclosest];
               /* get ridd of IB's vectors */
               if (!SUMA_Free_IsInBox (&IB)) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to free IB\n", FuncName);
               }

            } else { /* no node is close enough */
               if (SO != SOmap) {
                  SUMA_SL_Warn(  "No node was close enough\n"
                                 "to XYZmap, no linkage possible."   );
                  SUMA_free(XYZ);
                  SUMA_RETURN (NULL);
               } else {
                  /* comes from inherrently mappable stuff, makes sense to leave XYZ */
                  SUMA_SL_Warn(  "No node was close enough\n"
                                 "to XYZmap, linking by coordinate."   );
                  SUMA_RETURN (XYZ);
               }
            }
            /* store iclosest for lazy user */
            *I_C = iclosest;
         }
   } else { 
      iclosest = *I_C;
   }
   fprintf (SUMA_STDERR,"%s: Node identified for linking purposes is %d\n", FuncName, *I_C);
   ND = SO->NodeDim;
   id = ND * iclosest;
   XYZ[0]=SO->NodeList[id];
   XYZ[1]=SO->NodeList[id+1];
   XYZ[2]=SO->NodeList[id+2];

   /* all is done */
   SUMA_RETURN (XYZ);
}/* SUMA_XYZmap_XYZ */

/*! 
   Prec_ID = SUMA_MapRefRelative (Cur_ID, Prec_List, N_Prec_List, dov);
   Returns the ID (index into dov) of the surface object in Prec_List that is related 
   (via MapRef) to the surface object Cur_ID.
   This means that SOcur.MapRef_idcode_str = SOprec.MapRef_icode_str or SOprec.idcode_str

   \param Cur_ID (int) index into dov of the current surface object
   \param Prec_List (int *) indices into dov of the precursor surface objects 
   \param N_Prec_List (int) number of indices in Prec_List
   \param dov (SUMA_DO *) the vector of Displayable Object Structures
   \ret Prec_ID (int) index into dov of the surface object that is related to Cur_ID

*/
int SUMA_MapRefRelative (int cur_id, int *prec_list, int N_prec_list, SUMA_DO *dov) 
{
   int i, rel_id = -1;
   static char FuncName[]={"SUMA_MapRefRelative"};
   SUMA_SurfaceObject *SOcur, *SO_prec;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SOcur = (SUMA_SurfaceObject *)(dov[cur_id].OP);
   /* if surface has no MapRef then it cannot receive colors from precursors */
   if (!SUMA_ismappable(SOcur)) {
      SUMA_RETURN (-1);
   }

   for (i=0; i<N_prec_list; ++i) {
      SO_prec = (SUMA_SurfaceObject *)(dov[prec_list[i]].OP);
      if (strcmp(SOcur->MapRef_idcode_str, SO_prec->MapRef_idcode_str) == 0 || strcmp(SOcur->MapRef_idcode_str, SO_prec->idcode_str) == 0) {
         /* there's some relationship here, save it for return */
         if (rel_id < 0) {
            rel_id = prec_list[i];
         } else {
            fprintf (SUMA_STDERR,"Error %s: I did not think that would occur! Ignoring other relatives for now.\n", FuncName); 
         }

      }
   }

   SUMA_RETURN (rel_id);

}

