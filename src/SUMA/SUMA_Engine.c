#define DEBUG_1
#ifdef DEBUG_1
   #define DEBUG_2
   #define DEBUG_3
#endif
   
/* Need to figure out what to do with talking to multiple sockets (MATLAB and AFNI, for example). 
   So far, it looks like: 
   *- toggling should be separate. Maybe use Y key for matlab
      Not sure if SUMA_Engine talk toggling targets need to be separate 
      for the various stream. Me thinks that should not be the case.
   *- Talk_mode should be a part of the SUMAg_CF structure and should be set 
      for each stream. You'll also need to make sure relevant NI_write calls
      abide by the settings. You should remove reliance on NI_TALK_MODE
      and the current env used to control it.
   *- Everytime one deals with SUMA_AFNI_STREAM_INDEX, one should have
      a case for    SUMA_TO_MATLAB_STREAM_INDEX . However, one does not
      want to call SE_SendColorMapToAfni twice, once for AFNI and another
      time for matlab. You want to send, not generate, the nel in question twice. */
/* Header FILES */
   
#include "SUMA_suma.h"
static FILE *sumaout = NULL;             /* no default output stream */
static int SUMA_drive_set_outstream(char *outfile);
static FILE *SUMA_drive_get_outstream(void);

/* Make suma call itself with DriveSuma command in scom */
SUMA_Boolean SUMA_MakeMeDo (char *scom, int method) 
{
   static char FuncName[]={"SUMA_MakeMeDo"};
   SUMA_Boolean res = NOPE;
   int i, exflag, margc;
   char **margv=NULL;
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!scom) SUMA_RETURN(YUP);
   
   SUMA_LH("Talking to self");
   /* The shortcut, proper way (below, method = 0) needs fixing. 
      SUMA does not seem to get command it sends itself. Check later */
   if (method==1) {
      char *ssys=(char *)SUMA_calloc(strlen(scom)+100, sizeof(char));
      SUMA_LH("Clumsy system call");
      sprintf(ssys,"\\DriveSuma %s &", scom);
      if (system(ssys)) {
         SUMA_S_Err("Failed to execute %s ssys\n", ssys);
         SUMA_ifree(ssys);
         SUMA_RETURN(NOPE);
      }
      SUMA_RETURN(YUP);
   }
      
   margv = SUMA_com2argv(scom, &margc);

   if (!(Opt = SUMA_DriveSuma_ParseInput (margv, margc, NULL))) {
      goto CLEANOUT;
   }
   
   for (i=0; i<Opt->N_com; ++i) {
      if (LocalHead) {
         SUMA_LH("Have the following commands");
         fprintf(SUMA_STDERR,"Command %d: %s\n", i, Opt->com[i]);
      }
      if (!(exflag = SUMA_ProcessCommand(Opt->com[i], NULL, Opt->s))) {
         SUMA_S_Errv("Failed in processing command\n%s\n", Opt->com[i]); 
         goto CLEANOUT;
      }   
      if (exflag == -1) { /*gone daddy gone */ 
         SUMA_S_Note("There's no more reason to exist.\n"
                     "Farewell dear friends.\n");
         goto CLEANOUT;
      }
   }
   
   res = YUP;
   
   CLEANOUT:
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   margv = SUMA_free_com_argv(margv, &margc);
   
   SUMA_RETURN(res);
}   
                                          


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
   static char FuncName[]={"SUMA_Engine"};
   char tmpstr[128], sfield[100], sdestination[100];
   const char *NextCom;
   int NextComCode, ii, i, id, ND, ip, NP, itmp=-1;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_ALL_DO *ado = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_OVERLAYS *curColPlane=NULL;
   float delta_t, ftmp = -1.0;
   struct  timeval tt;
   int it, Wait_tot, nn=0, N_SOlist,
            SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS], iv200[200];
   float ft, **fm, fv15[15];
   double dv15[15];
   XtPointer elvis=NULL;
   NI_element *nel;
   char *cbuf=NULL;
   SUMA_Boolean Found;
   SUMA_SurfaceViewer *svi;
   SUMA_SurfaceViewer *sv = NULL;
   static char Command[]={"OBSOLETE-since:Thu Jan 23 16:55:03 EST 2003"};
   SUMA_EngineData *EngineData=NULL, *ED = NULL; 
      /* EngineData is what get passed from a list element, 
         ED is what gets added to the list inside SUMA_Engine */ 
   DListElmt *NextElem_CANT_TOUCH_THIS, *LocElm=NULL;
   DList *list= NULL;
   SUMA_CREATE_TEXT_SHELL_STRUCT *TextShell = NULL, *LogShell=NULL;
   SUMA_PARSED_NAME *fn = NULL;
   static int NI_TALK_MODE = -1;      /* Choose from:
                                             NI_TEXT_MODE or NI_BINARY_MODE
                                             Note cross-hair communication
                                             is now always in NI_TEXT_MODE,
                                             verify that AFNI handles either well
                                             THIS handling here is TEMPORARY */
   SUMA_Boolean LocalHead = NOPE; 
      
   
   SUMA_ENTRY;
   
   if (NI_TALK_MODE < 0) {
      if (AFNI_yesenv("SUMA_NI_TEXT_TALK_MODE")) {
         SUMA_S_Note("Talking in text mode");
         NI_TALK_MODE = NI_TEXT_MODE;
      } else NI_TALK_MODE = NI_BINARY_MODE;
   }
   
   list = *listp;    /* listp is now passed instead of list so that I can 
                        set list to NULL from within this function */
   
   if (!list) {
      fprintf (SUMA_STDERR, "Error %s: Nothing to do.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   SUMA_LH("Cycling through %d list elements", list->size);
   while (list->size) {/* cycle through NextComs */
      SUMA_LH("Fetching next element\n");
     /* get the next command from the head of the list */
      NextElem_CANT_TOUCH_THIS = dlist_head(list);
      EngineData = (SUMA_EngineData *)NextElem_CANT_TOUCH_THIS->data;
      
      /* decide on what Srcp might be. Currently only sv is passed 
         when the source is Suma*/
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
            break;
      } 
      
      NextComCode = EngineData->CommandCode;
      if (!NextComCode) {
         fprintf (stderr, "%s Error: Bad next element code\n", FuncName);
         SUMA_RETURN (NOPE);
      } 
      NextCom = SUMA_CommandString (NextComCode);
      SUMA_LH("->%s<-\t", NextCom);
      switch (NextComCode) {/* switch NextComCode */
         case SE_SendColorMapToAfni:
            /* expects in i the code of one of SUMA's standard colormaps */
            {
               SUMA_COLOR_MAP *cmap;
               NI_element *nel=NULL;
               int i;
               char sbuf[50], *stmp=NULL;
               
               if (EngineData->i_Dest != NextComCode ) {
                  fprintf (
                     SUMA_STDERR,
                     "Error %s: Data not destined correctly for %s (%d).\n", \
                   FuncName, NextCom, NextComCode);
                  break;
               }
               
               /* get the CMAP */
               if (!(cmap = SUMA_FindCodedColMap(EngineData->i))) {
                  SUMA_SLP_Err("Failed to get colormap");
                  break;
               }
               
               if (cmap->N_M[0] > 256) {
                  SUMA_SLP_Err(  "Cannot send more\n"
                                 "than 256 colors to\n"
                                 "AFNI.");
                  cmap = NULL;
                  break;
               }
               
               /* send AFNI the color map */
               nel = NI_new_data_element("ni_do", 0);
               NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
               stmp = SUMA_append_string("DEFINE_COLORSCALE ", cmap->Name);
               /* SEND COLORMAP In REVERSE ORDER TO AFNI,
               C'est la vie */
               for (i=cmap->N_M[0]-1; i >= 0; --i) {
                  sprintf(sbuf,"rgbi:%f/%f/%f", 
                           cmap->M[i][0], cmap->M[i][1], cmap->M[i][2]);
                  stmp = SUMA_append_replace_string(stmp, sbuf, " ", 1);
               }
               SUMA_LH("%s",stmp);
               NI_set_attribute ( nel, "ni_object", stmp);
               
               /* SUMA_ShowNel((void*)nel); */
               
               if (NI_write_element(   SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , 
                                       nel, NI_TALK_MODE ) < 0) {
                  SUMA_SLP_Err("Failed to send CMAP to afni");
                  NI_free_element(nel) ; nel = NULL;
                  if (stmp) SUMA_free(stmp); stmp = NULL;
                  cmap = NULL;
                  break;
               }
               
               NI_free_element(nel) ; nel = NULL;
               if (stmp) SUMA_free(stmp); stmp = NULL;
               
               /* Now set the colormap in AFNI */
               nel = NI_new_data_element("ni_do", 0);
               NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
               stmp = SUMA_append_string("SET_PBAR_ALL ", "A.+99");
               sprintf(sbuf, " 1 %s", cmap->Name);
               stmp = SUMA_append_replace_string(stmp, sbuf, "", 1);
               NI_set_attribute ( nel, "ni_object", stmp);
               
               /* SUMA_ShowNel((void*)nel); */
               
               if (NI_write_element( SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , 
                                     nel, NI_TALK_MODE ) < 0) {
                  SUMA_SLP_Err("Failed to send CMAP to afni");
                  NI_free_element(nel) ; nel = NULL;
                  if (stmp) SUMA_free(stmp); stmp = NULL;
                  cmap = NULL;
                  break;
               }
               
               NI_free_element(nel) ; nel = NULL;
               if (stmp) SUMA_free(stmp); stmp = NULL;
              
               /* set the autorange off */
               nel = NI_new_data_element("ni_do", 0);
               NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
               NI_set_attribute ( nel, "ni_object", "SET_FUNC_AUTORANGE A.-");
               if (NI_write_element( SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , 
                                     nel, NI_TALK_MODE ) < 0) {
                  SUMA_SLP_Err("Failed to send CMAP to afni");
                  NI_free_element(nel) ; nel = NULL;
                  cmap = NULL;
                  break;
               }
               
               NI_free_element(nel) ; nel = NULL;
               
               /* set the range of the colorbar */
               nel = NI_new_data_element("ni_do", 0);
               NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
               sprintf(sbuf," %d", cmap->N_M[0]);
               stmp = SUMA_append_string("SET_FUNC_RANGE A.", sbuf);
               NI_set_attribute ( nel, "ni_object", stmp);
               if (NI_write_element( SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , 
                                     nel, NI_TALK_MODE ) < 0) {
                  SUMA_SLP_Err("Failed to send CMAP to afni");
                  NI_free_element(nel) ; nel = NULL;
                  if (stmp) SUMA_free(stmp); stmp = NULL;
                  cmap = NULL;
                  break;
               }
               NI_free_element(nel) ; nel = NULL;
               if (stmp) SUMA_free(stmp); stmp = NULL;
               
               cmap = NULL;
            }
            break;
         case SE_OpenDrawnROIFileSelection:
            /* opens the open ROI file selection window. 
            Expects sv in vp, NULL is OK, and a position 
            reference widget typecast to ip, the latter can also be null.*/
            {
               char sbuf[128];
               if (  EngineData->vp_Dest != NextComCode || 
                     EngineData->ip_Dest != NextComCode ) {
                  fprintf (SUMA_STDERR,
                           "Error %s: "
                           "Data not destined correctly for %s (%d).\n", 
                           FuncName, NextCom, NextComCode);
                  break;
               }
               /* open the ROI file */
               if (!sv) sv = SUMA_LAST_VIEWER;
               /* wildcard selection 
                 for a surface, just use the one in focus
                 But it is only to select the wildcard
                 SO assignment to an ROI is done in the ROI
                 reading function*/
               SO = SUMA_SV_Focus_SO(sv);
               if (SO && !SUMA_WildcardChoice(2, SO, sbuf)) {
                  sprintf(sbuf, "*.roi");
               } else sprintf(sbuf, "*.roi");
               if (!EngineData->vp) EngineData->vp = sv;
               if (!EngineData->ip) {
                  SUMAg_CF->X->FileSelectDlg = 
                     SUMA_CreateFileSelectionDialogStruct (
                        sv->X->TOPLEVEL, SUMA_FILE_OPEN, YUP,
                        SUMA_OpenDrawnROI, (void *)EngineData->vp,
                        NULL, NULL,
                        sbuf,
                        SUMAg_CF->X->FileSelectDlg);
               } else {
                  SUMAg_CF->X->FileSelectDlg = 
                     SUMA_CreateFileSelectionDialogStruct (
                        (Widget) EngineData->ip, SUMA_FILE_OPEN, YUP,
                        SUMA_OpenDrawnROI, (void *)EngineData->vp,
                        NULL, NULL,
                        sbuf,
                        SUMAg_CF->X->FileSelectDlg);
               }
               if (SO && SO->Side == SUMA_RIGHT) {
                  sprintf(sbuf,"Select RH ROI File to Open");
               } else if (SO && SO->Side == SUMA_LEFT) {
                  sprintf(sbuf,"Select LH ROI File to Open");
               } else sprintf(sbuf,"Select ROI File to Open");
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialog ( sbuf, 
                                                   &SUMAg_CF->X->FileSelectDlg);
            }
            break;
            
         case SE_OpenXformOrtFileFileSelection:
            /* opens the open ort file selection window. 
            Expects SUMA_XFORM in vp (to be used later and a position 
            reference widget typecast to ip, the latter can be null.*/
            if (  EngineData->vp_Dest != NextComCode || 
                  EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n", 
                        FuncName, NextCom, NextComCode);
               break;
            }
            if (!EngineData->ip) {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                     sv->X->TOPLEVEL, SUMA_FILE_OPEN, YUP,
                     SUMA_OpenXformOrtFile, (void *)EngineData->vp,
                     NULL, NULL,
                     "*.1D",
                     SUMAg_CF->X->FileSelectDlg);
            } else {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                     (Widget) EngineData->ip, SUMA_FILE_OPEN, YUP,
                     SUMA_OpenXformOrtFile, (void *)EngineData->vp,
                     NULL, NULL,
                     "*.1D",
                     SUMAg_CF->X->FileSelectDlg);
            }
            SUMAg_CF->X->FileSelectDlg = 
               SUMA_CreateFileSelectionDialog ( "Select Ort File to Open", 
                                                &SUMAg_CF->X->FileSelectDlg);
            break;
            
         case SE_SaveDrawnROIFileSelection:
            /* opens the save roi  file selection window. 
            Expects NULL in vp (to be used later and a position reference 
            widget typecast to ip, the latter can be null.*/
            if (  EngineData->vp_Dest != NextComCode || 
                  EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n", 
                        FuncName, NextCom, NextComCode);
               break;
            }
            
            /* save ROI to file */
            if (!sv) sv = &(SUMAg_SVv[0]);
            if (!EngineData->ip) {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct ( sv->X->TOPLEVEL,      
                                                   SUMA_FILE_SAVE, YUP,
                                                   SUMA_SaveDrawnROI, 
                                                   (void *)EngineData->vp,
                                                   NULL, NULL,
                                                   "*.roi",
                                                   SUMAg_CF->X->FileSelectDlg);
            } else {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct ((Widget) EngineData->ip, 
                                                   SUMA_FILE_SAVE, YUP,
                                                   SUMA_SaveDrawnROI, 
                                                   (void *)EngineData->vp,
                                                   NULL, NULL,
                                                   "*.roi",
                                                   SUMAg_CF->X->FileSelectDlg);
            }
            
            SUMAg_CF->X->FileSelectDlg = 
               SUMA_CreateFileSelectionDialog ( "Select ROI Filename", 
                                                &SUMAg_CF->X->FileSelectDlg);
            
            break;
         case SE_SaveXformOptsFileSelection:
            /* opens the save Xform Opts  file selection window. 
            Expects SUMA_XFORM* in vp  and a position reference 
            widget typecast to ip, the latter can be null.*/
            if (  EngineData->vp_Dest != NextComCode || 
                  EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n", 
                        FuncName, NextCom, NextComCode);
               break;
            }
            
            /* save Xform opts to file */
            if (!sv) sv = &(SUMAg_SVv[0]);
            if (!EngineData->ip) {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct ( sv->X->TOPLEVEL,      
                                                   SUMA_FILE_SAVE, YUP,
                                                   SUMA_SaveXformOpts, 
                                                   (void *)EngineData->vp,
                                                   NULL, NULL,
                                                   "*.xfopts",
                                                   SUMAg_CF->X->FileSelectDlg);
            } else {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct ((Widget) EngineData->ip, 
                                                   SUMA_FILE_SAVE, YUP,
                                                   SUMA_SaveXformOpts, 
                                                   (void *)EngineData->vp,
                                                   NULL, NULL,
                                                   "*.xfopts",
                                                   SUMAg_CF->X->FileSelectDlg);
            }
            
            SUMAg_CF->X->FileSelectDlg = 
               SUMA_CreateFileSelectionDialog ( "Select Opts Filename", 
                                                &SUMAg_CF->X->FileSelectDlg);
            
            break;

         case SE_SaveSOFileSelection:
            /* saves a surface and its node colors to ascii files */
            /* expects SO in vp and a position reference widget typecast 
               to ip, the latter can be null.*/
            if (EngineData->vp_Dest != NextComCode || 
                  EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n", 
                        FuncName, NextCom, NextComCode);
               break;
            }
            if (!sv) sv = &(SUMAg_SVv[0]);
            
            {
               SUMA_SAVESO_STRUCT *SaveSO_data = NULL;
               
               SaveSO_data = (SUMA_SAVESO_STRUCT *)
                              SUMA_calloc(1,sizeof(SUMA_SAVESO_STRUCT)); 
                              /* DO NOT FREE THIS POINTER,
                                 It is freed by the function 
                                 SUMA_SaveSOascii */
               SaveSO_data->SO = (SUMA_SurfaceObject *)EngineData->vp;
               SaveSO_data->sv = sv;
               
               if (!EngineData->ip) {
                  SUMAg_CF->X->FileSelectDlg = 
                     SUMA_CreateFileSelectionDialogStruct (
                        sv->X->TOPLEVEL, SUMA_FILE_SAVE, YUP,
                        SUMA_SaveSOascii, (void *)SaveSO_data,
                        NULL, NULL,
                        "*.1D.xyz",
                        SUMAg_CF->X->FileSelectDlg);
               } else {
                  SUMAg_CF->X->FileSelectDlg = 
                     SUMA_CreateFileSelectionDialogStruct (
                        (Widget) EngineData->ip, SUMA_FILE_SAVE, YUP,
                        SUMA_SaveSOascii, (void *)SaveSO_data,
                        NULL, NULL, "*.1D.xyz",                                                           SUMAg_CF->X->FileSelectDlg);
               }

               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialog (
                     "Select SO file prefix.", &SUMAg_CF->X->FileSelectDlg);
            }
            break;
            
         case SE_LoadSegDO:
            if (EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            }
            if (!sv) sv = &(SUMAg_SVv[0]);
            if (!EngineData->ip) {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                     sv->X->TOPLEVEL, SUMA_FILE_OPEN, YUP,
                     SUMA_LoadSegDO, (void *)sv,
                     NULL, NULL,
                     "*.*",
                     SUMAg_CF->X->FileSelectDlg);
            } else {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                     (Widget) EngineData->ip, SUMA_FILE_OPEN, YUP,
                     SUMA_LoadSegDO, (void *)sv,
                     NULL, NULL,
                     "*.*",
                     SUMAg_CF->X->FileSelectDlg);
            }
            SUMAg_CF->X->FileSelectDlg = 
               SUMA_CreateFileSelectionDialog (
                  "Select Displayable Objects File", 
                  &SUMAg_CF->X->FileSelectDlg);
            break;
            
         case SE_LoadViewFileSelection:
            /* opens the view file selection window.
            Expects a position  reference widget typecast to ip, 
            the latter can be null.*/
            
            if (EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n", 
                        FuncName, NextCom, NextComCode);
               break;
            }
            if (!sv) sv = &(SUMAg_SVv[0]);
            if (!EngineData->ip) {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                     sv->X->TOPLEVEL, SUMA_FILE_OPEN, YUP,
                     SUMA_LoadVisualState, (void *)sv,
                     NULL, NULL,
                     "*.vvs",
                     SUMAg_CF->X->FileSelectDlg);
            } else {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                     (Widget) EngineData->ip, SUMA_FILE_OPEN, YUP,
                     SUMA_LoadVisualState, (void *)sv,
                     NULL, NULL,
                     "*.vvs",
                     SUMAg_CF->X->FileSelectDlg);
            }
            SUMAg_CF->X->FileSelectDlg = 
               SUMA_CreateFileSelectionDialog (
                  "Select Viewer Settings File", &SUMAg_CF->X->FileSelectDlg);
            break;
            
         case SE_SaveViewFileSelection:
            /* opens the view file selection window.
            Expects a position  reference widget typecast to ip, 
            the latter can be null.*/
            
            if (EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n", 
                        FuncName, NextCom, NextComCode);
               break;
            }
            if (!sv) sv = &(SUMAg_SVv[0]);
            if (!EngineData->ip) {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                       sv->X->TOPLEVEL, SUMA_FILE_SAVE, YUP,
                       SUMA_SaveVisualState, (void *)sv,
                       NULL, NULL,
                       "*.vvs",
                       SUMAg_CF->X->FileSelectDlg);
            } else {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                       (Widget) EngineData->ip, SUMA_FILE_SAVE, YUP,
                       SUMA_SaveVisualState, (void *)sv,
                       NULL, NULL,
                       "*.vvs",
                       SUMAg_CF->X->FileSelectDlg);
            }
            SUMAg_CF->X->FileSelectDlg = 
               SUMA_CreateFileSelectionDialog ("Select Viewer Settings File", 
                                                &SUMAg_CF->X->FileSelectDlg);
            break;
            
         case SE_OpenSurfCont:
            /* opens the surface controller 
            Expects SO in vp */
            if (EngineData->vp_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n", 
                        FuncName, NextCom, NextComCode);
               break;
            }
            if (!sv) sv = &(SUMAg_SVv[0]);
            ado = (SUMA_ALL_DO *)EngineData->vp;
            if (!SUMA_viewSurfaceCont(NULL, ado, sv)) {
               SUMA_S_Err("Failed open surfcont");
               break;  
            }
            break;
         
         case SE_OneOnly:
            if (EngineData->vp_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                  "Error %s: Data not destined correctly for %s (%d).\n", 
                  FuncName, NextCom, NextComCode);
               break;
            }
            if (!sv) sv = &(SUMAg_SVv[0]);
            ado = (SUMA_ALL_DO *)EngineData->vp;
            if (!SUMA_ColPlaneShowOneFore_Set (ado, YUP, 
                                 EngineData->Src == SES_SumaWidget)) {
               SUMA_S_Err("Failed to set one only");
               break;  
            }
            break;
            
         case SE_OpenDsetFileSelection:
             {
               char sbuf[128];  
               /* opens the dataset file selection window. 
                  Expects SO in vp and a position reference 
                  widget typecast to ip, the latter can be null.*/

               if (  EngineData->vp_Dest != NextComCode || 
                     EngineData->ip_Dest != NextComCode ) {
                  fprintf (SUMA_STDERR,
                     "Error %s: Data not destined correctly for %s (%d).\n", 
                     FuncName, NextCom, NextComCode);
                  break;
               }

               /* wildcard selection */
               SO = (SUMA_SurfaceObject *)EngineData->vp;
               if (!SUMA_WildcardChoice(1, SO, sbuf)) {
                  sprintf(sbuf, "*.dset");
               }

               /*Load data from file */
               if (!sv) sv = &(SUMAg_SVv[0]);
               if (!EngineData->ip) {
                  SUMAg_CF->X->FileSelectDlg = 
                     SUMA_CreateFileSelectionDialogStruct ( 
                        sv->X->TOPLEVEL,
                        SUMA_FILE_OPEN, YUP,
                        SUMA_LoadDsetOntoSO,
                        (void *)EngineData->vp,
                        NULL, NULL,
                        sbuf,
                        SUMAg_CF->X->FileSelectDlg);
               } else {
                  SUMAg_CF->X->FileSelectDlg = 
                     SUMA_CreateFileSelectionDialogStruct (
                        (Widget) EngineData->ip, 
                        SUMA_FILE_OPEN, YUP,
                        SUMA_LoadDsetOntoSO, 
                        (void *)EngineData->vp,
                        NULL, NULL,
                        sbuf,
                        SUMAg_CF->X->FileSelectDlg);
               }
               if (SO && SO->Side == SUMA_RIGHT) {
                  sprintf(sbuf,"Select RH Dset File");
               } else if (SO && SO->Side == SUMA_LEFT) {
                  sprintf(sbuf,"Select LH Dset File");
               } else sprintf(sbuf,"Select Dset File");

               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialog (
                     sbuf, 
                     &SUMAg_CF->X->FileSelectDlg);
            }
            break;
            
         case SE_OpenDsetFile:
            /* opens the dataset file, Expects SO in vp and a name in cp*/
            if (  EngineData->vp_Dest != NextComCode || 
                  EngineData->cp_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n", 
                        FuncName, NextCom, NextComCode);
               break;
            }
            SUMA_LoadDsetOntoSO(EngineData->cp, EngineData->vp);
            break;

         case SE_SaveMaskFileSelection:
             {
               char sbuf[128];  
               /* save the mask file selection window. 
                  Expects NULL in vp for now. Kept vp here
                  in case I need it in the future.
                  Also needs a position reference 
                  widget typecast to ip, the latter can be null.*/

               if (  EngineData->vp_Dest != NextComCode || 
                     EngineData->ip_Dest != NextComCode ) {
                  fprintf (SUMA_STDERR,
                     "Error %s: Data not destined correctly for %s (%d).\n", 
                     FuncName, NextCom, NextComCode);
                  break;
               }

               /* wildcard selection */
               sprintf(sbuf, "*.*.mo");

               /*Load data from file */
               if (!sv) sv = &(SUMAg_SVv[0]);
               if (!EngineData->ip) {
                  SUMAg_CF->X->FileSelectDlg = 
                     SUMA_CreateFileSelectionDialogStruct ( 
                        sv->X->TOPLEVEL,
                        SUMA_FILE_SAVE, YUP,
                        SUMA_SaveMultiMasks,
                        (void *)EngineData->vp,
                        NULL, NULL,
                        sbuf,
                        SUMAg_CF->X->FileSelectDlg);
               } else {
                  SUMAg_CF->X->FileSelectDlg = 
                     SUMA_CreateFileSelectionDialogStruct (
                        (Widget) EngineData->ip, 
                        SUMA_FILE_SAVE, YUP,
                        SUMA_SaveMultiMasks, 
                        (void *)EngineData->vp,
                        NULL, NULL,
                        sbuf,
                        SUMAg_CF->X->FileSelectDlg);
               }
               
               sprintf(sbuf,"Enter Masks Filename");
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialog (
                     sbuf, 
                     &SUMAg_CF->X->FileSelectDlg);
            }
            break;

         case SE_OpenMaskFileSelection:
             {
               char sbuf[128];  
               /* opens the mask file selection window. 
                  Expects NULL in vp for now. Kept vp here
                  in case I need it in the future.
                  Also needs a position reference 
                  widget typecast to ip, the latter can be null.*/

               if (  EngineData->vp_Dest != NextComCode || 
                     EngineData->ip_Dest != NextComCode ) {
                  fprintf (SUMA_STDERR,
                     "Error %s: Data not destined correctly for %s (%d).\n", 
                     FuncName, NextCom, NextComCode);
                  break;
               }

               /* wildcard selection */
               sprintf(sbuf, "*.*.mo");

               /*Load data from file */
               if (!sv) sv = &(SUMAg_SVv[0]);
               if (!EngineData->ip) {
                  SUMAg_CF->X->FileSelectDlg = 
                     SUMA_CreateFileSelectionDialogStruct ( 
                        sv->X->TOPLEVEL,
                        SUMA_FILE_OPEN, YUP,
                        SUMA_LoadMultiMasks,
                        (void *)EngineData->vp,
                        NULL, NULL,
                        sbuf,
                        SUMAg_CF->X->FileSelectDlg);
               } else {
                  SUMAg_CF->X->FileSelectDlg = 
                     SUMA_CreateFileSelectionDialogStruct (
                        (Widget) EngineData->ip, 
                        SUMA_FILE_OPEN, YUP,
                        SUMA_LoadMultiMasks, 
                        (void *)EngineData->vp,
                        NULL, NULL,
                        sbuf,
                        SUMAg_CF->X->FileSelectDlg);
               }
               
               sprintf(sbuf,"Select Masks File");
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialog (
                     sbuf, 
                     &SUMAg_CF->X->FileSelectDlg);
            }
            break;
            
         case SE_OpenMaskFile:
            /* opens the dataset file, Expects nothing in vp and a name in cp*/
            if (  EngineData->vp_Dest != NextComCode || 
                  EngineData->cp_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n", 
                        FuncName, NextCom, NextComCode);
               break;
            }
            SUMA_LoadMultiMasks(EngineData->cp, EngineData->vp);
            break;

         case SE_OpenColFile:
            /* opens the color file, Expects SO in vp and a name in cp*/
            if (  EngineData->vp_Dest != NextComCode || 
                  EngineData->cp_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n", 
                        FuncName, NextCom, NextComCode);
               break;
            }
            SUMA_LoadColorPlaneFile(EngineData->cp, EngineData->vp);
            break;
            
         case SE_OpenCmapFileSelection:
            /* opens the Cmap file selection window. 
            Expects ADO in vp and a position reference widget 
            typecast to ip, the latter can be null.*/
            
            if (  EngineData->vp_Dest != NextComCode || 
                  EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            }
            
            /*Load colors from file */
            if (!sv) sv = &(SUMAg_SVv[0]);
            if (!EngineData->ip) {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                     sv->X->TOPLEVEL, SUMA_FILE_OPEN, YUP,
                     SUMA_LoadCmapFile, (void *)EngineData->vp,
                     NULL, NULL,
                     "*.cmap",
                     SUMAg_CF->X->FileSelectDlg);
            } else {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                     (Widget) EngineData->ip, SUMA_FILE_OPEN, YUP,
                     SUMA_LoadCmapFile, (void *)EngineData->vp,
                     NULL, NULL,
                     "*.cmap",
                     SUMAg_CF->X->FileSelectDlg);
            }
            
            SUMAg_CF->X->FileSelectDlg = 
               SUMA_CreateFileSelectionDialog (
                  "Select Cmap File", &SUMAg_CF->X->FileSelectDlg);
            
            break;
         case SE_OpenColFileSelection:
            /* opens the color file selection window. 
            Expects SO in vp and a position reference widget typecast to ip, 
            the latter can be null.*/
            
            if (EngineData->vp_Dest != NextComCode || 
                  EngineData->ip_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n", 
                        FuncName, NextCom, NextComCode);
               break;
            }
            
            /*Load colors from file */
            if (!sv) sv = &(SUMAg_SVv[0]);
            if (!EngineData->ip) {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                     sv->X->TOPLEVEL, SUMA_FILE_OPEN, YUP,
                     SUMA_LoadColorPlaneFile, (void *)EngineData->vp,
                     NULL, NULL,
                     "*.col",
                     SUMAg_CF->X->FileSelectDlg);
            } else {
               SUMAg_CF->X->FileSelectDlg = 
                  SUMA_CreateFileSelectionDialogStruct (
                     (Widget) EngineData->ip, SUMA_FILE_OPEN, YUP,
                     SUMA_LoadColorPlaneFile, (void *)EngineData->vp,
                     NULL, NULL,
                     "*.col",
                     SUMAg_CF->X->FileSelectDlg);
            }
            
            SUMAg_CF->X->FileSelectDlg = 
               SUMA_CreateFileSelectionDialog (
                  "Select Node Color File", &SUMAg_CF->X->FileSelectDlg);
            
            break;
         
            
         case SE_OpenDrawROI:
            /* opens the DrawROI window, expects a surface viewer 
               pointer in EngineData->Srcp*/
            {
               SUMA_DRAWN_ROI *DrawnROI=NULL;
               
               if (!sv) {
                  fprintf (SUMA_STDERR, "Error %s: Null sv.\n", FuncName);
                  SUMA_RETURN(NOPE);
               }   
               
               /* determine if there are ROIs being drawn on surfaces 
                  displayed here */
               DrawnROI = NULL;
               /* start with the Focus_SO */
               if ((SO = SUMA_SV_Focus_SO(sv))) {
                  DrawnROI = SUMA_FetchROI_InCreation (SO, SUMAg_DOv,  
                                                       SUMAg_N_DOv); 
               }
               if (!DrawnROI) { /* none found on focus surface, check 
                                 other surfaces in this viewer */
                  N_SOlist = SUMA_RegisteredSOs(sv, SUMAg_DOv, SOlist);
                  if (N_SOlist) {
                     it = 0;
                     do {
                        DrawnROI = SUMA_FetchROI_InCreation (SO, SUMAg_DOv,  
                                                             SUMAg_N_DOv);
                        ++it;
                     } while (!DrawnROI && it < N_SOlist);
                  }
               }
               
               /* call function to create ROI window */
               if (!SUMA_OpenDrawROIWindow (DrawnROI)) {
                  SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                        "Failed to open Draw ROI window", 
                                        FuncName, 
                                        SMT_Error, SMA_LogAndPopup);

               }
               break;
            
            }
         case SE_SetRenderMode:
            { /* sets the rendering mode of a surface, 
               expects SO in vp and rendering mode in i*/
               SO = (SUMA_SurfaceObject *)EngineData->vp;
               SUMA_SET_SO_POLYMODE(SO,EngineData->i);
            }  
            break;
            
         case SE_SetTransMode:
            { /* sets the transparency value of a surface, 
               expects SO in vp and TransMode in i*/
               SO = (SUMA_SurfaceObject *)EngineData->vp;
               SUMA_Set_ADO_TransMode((SUMA_ALL_DO *)SO,EngineData->i, 0, 0);
            }  
            break;
            
         case SE_SetATransMode:
            { /* sets the transparency value of a surface, 
               expects SO in vp and TransMode in i*/
               ado = (SUMA_ALL_DO *)EngineData->vp;
               SUMA_Set_ADO_TransMode(ado,EngineData->i, 0, 0);
            }  
            break;
            
         case SE_SetDsetViewMode:
            { /* sets the viewing mode of a dset, 
               expects ADO in vp and rendering mode in i*/
               SUMA_COLOR_MAP *cmp=NULL;
               static int nwarn=0, nwarn2=0;

               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(curColPlane = SUMA_ADO_CurColPlane(ado)) ||
                   !(SurfCont = SUMA_ADO_Cont(ado))) {
                  SUMA_S_Err("No cur plane");
                  break;
               }               
               it = SUMA_ABS(curColPlane->ShowMode);
               if (EngineData->i == SW_SurfCont_DsetViewXXX) {
                  curColPlane->ShowMode = 
                     -SUMA_ABS(curColPlane->ShowMode);
               } else {
                  curColPlane->ShowMode =  EngineData->i ;
               }
               if (strcmp(curColPlane->cmapname,"explicit")) {
                  /* Can we do contours? */
                  cmp = SUMA_FindNamedColMap(
                                 curColPlane->cmapname);
                  if (!cmp) { SUMA_S_Err("Unexpected null colormap"); break;}
                  if (SUMA_NeedsLinearizing(cmp)) {
                     if (EngineData->i == SW_SurfCont_DsetViewCon   ||
                         EngineData->i == SW_SurfCont_DsetViewCaC ) {
                        if (!nwarn) {
                           SUMA_SLP_Note("Cannot do contouring with colormaps\n"
                                         "that panes of unequal sizes.\n"
                                         "Contouring turned off.\n"
                                         "Notice shown once per session.");
                           ++nwarn;
                        }
                        curColPlane->ShowMode = it; /* get back */ 
                        SUMA_Set_Menu_Widget( SurfCont->DsetViewModeMenu,
                           SUMA_ShowMode2ShowModeMenuItem(it));
                        /* kill current contours, if any */
                        SUMA_KillOverlayContours(curColPlane);
                     }
                  }
                  /* if new mode require contours, better regenerate them */
                  if ( (it != SW_SurfCont_DsetViewCon &&
                        it != SW_SurfCont_DsetViewCaC ) &&
                       (curColPlane->ShowMode == 
                                 SW_SurfCont_DsetViewCon   ||
                        curColPlane->ShowMode == 
                                 SW_SurfCont_DsetViewCaC) ) {
                     if (!SUMA_ColorizePlane(curColPlane)) {
                        SUMA_S_Err( "Police at the station - "
                                    "and they don't look friendly.");
                     }
                  }
               } else {
                  SUMA_LH("lam");
                  /* explicit colormap no need for all the complications above*/
                     if (EngineData->i == SW_SurfCont_DsetViewCon   ||
                         EngineData->i == SW_SurfCont_DsetViewCaC ) {
                        if (!nwarn2) {
                           SUMA_SLP_Note("Cannot do contouring with explicitly\n"
                                         "colored datasets. \n"
                                         "Notice shown once per session.");
                           ++nwarn2;
                        }
                        curColPlane->ShowMode = it; /* get back */ 
                        SUMA_Set_Menu_Widget( SurfCont->DsetViewModeMenu,
                           SUMA_ShowMode2ShowModeMenuItem(it));
                        /* kill current contours, if any . There should be none
                           here but there is no harm */
                        SUMA_KillOverlayContours(curColPlane);
                     }
               }  
               if (!SUMA_Remixedisplay (ado)) {
                  SUMA_S_Err("Dunno what happened here");
               }
            }  
            break;
            
         case SE_SetDsetFont:
            { /* sets the Font for nodes of a (graph) dset, 
               expects ADO in vp and rendering mode in i*/
               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(curColPlane = SUMA_ADO_CurColPlane(ado)) ||
                   !(SurfCont = SUMA_ADO_Cont(ado))) {
                  SUMA_S_Err("No cur plane");
                  break;
               }
                              
               if (EngineData->i == SW_SurfCont_DsetFontXXX) {
                  curColPlane->Font = 
                     -SUMA_ABS(curColPlane->Font);
               } else {
                  curColPlane->Font = EngineData->i;
               }
               SUMA_ADO_Flush_Pick_Buffer(ado, sv);
               if (!SUMA_Remixedisplay (ado)) {
                  SUMA_S_Err("Dunno what happened here");
               }
            }  
            break;

         case SE_SetDsetThrough:
            { /* sets the sphere radius for nodes of a (graph) dset, 
               expects ADO in vp and rendering mode in i*/
               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(curColPlane = SUMA_ADO_CurColPlane(ado)) ||
                   !(SurfCont = SUMA_ADO_Cont(ado))) {
                  SUMA_S_Err("No cur plane");
                  break;
               }
                              
               if (EngineData->i == SW_SurfCont_DsetThroughXXX) {
                  curColPlane->Through = 
                     -SUMA_ABS(curColPlane->Through);
               } else {
                  curColPlane->Through = EngineData->i;
               }
               SUMA_ADO_Flush_Pick_Buffer(ado, sv);
               if (!SUMA_Remixedisplay (ado)) {
                  SUMA_S_Err("Dunno what happened here");
               }
            }  
            break;
            
         case SE_SetDsetNodeRad:
            { /* sets the sphere radius for nodes of a (graph) dset, 
               expects ADO in vp and rendering mode in i*/
               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(curColPlane = SUMA_ADO_CurColPlane(ado)) ||
                   !(SurfCont = SUMA_ADO_Cont(ado))) {
                  SUMA_S_Err("No cur plane");
                  break;
               }
                              
               if (EngineData->i == SW_SurfCont_DsetNodeRadXXX) {
                  curColPlane->NodeRad = 
                     -SUMA_ABS(curColPlane->NodeRad);
               } else {
                  curColPlane->NodeRad = EngineData->i;
               }
               SUMA_ADO_Flush_Pick_Buffer(ado, sv);
               if (!SUMA_Remixedisplay (ado)) {
                  SUMA_S_Err("Dunno what happened here");
               }
            }  
            break;
            
         case SE_SetDsetEdgeThick:
            { /* sets the thickness of a (graph's) edges, 
               expects ADO in vp and rendering mode in i*/
               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(curColPlane = SUMA_ADO_CurColPlane(ado)) ||
                   !(SurfCont = SUMA_ADO_Cont(ado))) {
                  SUMA_S_Err("No cur plane");
                  break;
               }
                              
               curColPlane->EdgeThick = EngineData->i;
               SUMA_ADO_Flush_Pick_Buffer(ado, sv);
               if (!SUMA_Remixedisplay (ado)) {
                  SUMA_S_Err("Dunno what happened here");
               }
            }  
            break;

         case SE_SetDsetEdgeStip:
            { /* sets the stippling of a (graph's) edges, 
               expects ADO in vp and rendering mode in i*/
               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(curColPlane = SUMA_ADO_CurColPlane(ado)) ||
                   !(SurfCont = SUMA_ADO_Cont(ado))) {
                  SUMA_S_Err("No cur plane");
                  break;
               }
                              
               if (EngineData->i == SW_SurfCont_DsetEdgeStipXXX) {
                  curColPlane->EdgeStip = 
                     -SUMA_ABS(curColPlane->EdgeStip);
               } else {
                  curColPlane->EdgeStip = EngineData->i;
               }

               if (!SUMA_Remixedisplay (ado)) {
                  SUMA_S_Err("Dunno what happened here");
               }
            }
            break;

         case SE_SetTractStyle:
            { /* sets the stippling of a (graph's) edges, 
               expects ADO in vp and rendering mode in i*/
               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(curColPlane = SUMA_ADO_CurColPlane(ado)) ||
                   !(SurfCont = SUMA_ADO_Cont(ado))) {
                  SUMA_S_Err("No cur plane");
                  break;
               }
                              
               if (EngineData->i == SW_SurfCont_TractStyleSOLID) {
                  curColPlane->EdgeStip = 
                     -SUMA_ABS(curColPlane->EdgeStip);
               } else {
                  curColPlane->EdgeStip = EngineData->i;
               }

               if (!SUMA_Remixedisplay (ado)) {
                  SUMA_S_Err("Dunno what happened here");
               }
            }
            break;

         case SE_SetDsetAlphaVal:
            { /* sets the stippling of a (graph's) edges, 
               expects ADO in vp and rendering mode in i*/
               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(curColPlane = SUMA_ADO_CurColPlane(ado)) ||
                   !(SurfCont = SUMA_ADO_Cont(ado))) {
                  SUMA_S_Err("No cur plane");
                  break;
               }
               
               SUMA_LH("Setting AlphaVal = %d", EngineData->i);           
               if (curColPlane->AlphaVal != EngineData->i) {
                  curColPlane->AlphaVal = EngineData->i;
                  /* This requires recoloring because the alpha values may
                     depend on the data */
                  if (!SUMA_ColorizePlane (curColPlane)) {
                     SUMA_SLP_Err("Failed to colorize plane.\n");
                     SUMA_RETURN(0);
                  }

                  if (!SUMA_Remixedisplay (ado)) {
                     SUMA_S_Err("Dunno what happened here");
                  }
               }
            }
            break;

         case SE_SetDsetNodeCol:
            { /* sets the node coloring mode of a (graph) dset, 
               expects ADO in vp and rendering mode in i*/
               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(curColPlane = SUMA_ADO_CurColPlane(ado)) ||
                   !(SurfCont = SUMA_ADO_Cont(ado))) {
                  SUMA_S_Err("No cur plane");
                  break;
               }
                              
               curColPlane->NodeCol = EngineData->i;
               if (!SUMA_Remixedisplay (ado)) {
                  SUMA_S_Err("Dunno what happened here");
               }
            }  
            break;
            
         case SE_SetDsetTxtShad:
            { /* sets the text shading for nodes of a (graph) dset, 
               expects ADO in vp and rendering mode in i*/
               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(curColPlane = SUMA_ADO_CurColPlane(ado)) ||
                   !(SurfCont = SUMA_ADO_Cont(ado))) {
                  SUMA_S_Err("No cur plane");
                  break;
               }
                              
               curColPlane->TxtShad = EngineData->i;
               if (!SUMA_Remixedisplay (ado)) {
                  SUMA_S_Err("Dunno what happened here");
               }
            }  
            break;
            
         case SE_SetDsetGmatBord:
            { /* sets the border width of a (graph) dset matrix, 
               expects ADO in vp and rendering mode in i*/
               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(curColPlane = SUMA_ADO_CurColPlane(ado)) ||
                   !(SurfCont = SUMA_ADO_Cont(ado))) {
                  SUMA_S_Err("No cur plane");
                  break;
               }
                
               /* Get rid of matrix */
               curColPlane->BordFrac = EngineData->i;
               SUMA_GDSET_refresh_matrix_nido(SUMA_ADO_Dset(ado), 1);
               /* Update all viewers showing ado */
               SUMA_UpdateViewPoint_RegisteredADO(ado, 1);
               if (!SUMA_Remixedisplay (ado)) {
                  SUMA_S_Err("Dunno what happened here");
               }
            }  
            break;

         case SE_SetTractMask:
            { /* sets the masking mode for tracts, 
               expects ADO in vp and masking mode in i*/
               SUMA_TRACT_SAUX *TSaux;
               ado = (SUMA_ALL_DO *)EngineData->vp;
               if (!(TSaux = SUMA_ADO_TSaux(ado))) {
                  SUMA_S_Err("No valid pointers");
                  break;
               }
                              
               TSaux->TractMask = EngineData->i;
               
               SUMA_ADO_Flush_Pick_Buffer(ado, sv);
               if (!SUMA_Remixedisplay (ado)) {
                  SUMA_S_Err("Dunno what happened here");
               }
            }  
            break;

         case SE_UpdateLog:
            /* Updates the Log window if it is open */
            {
               if (SUMAg_CF->X->Log_TextShell) {
                  char *s = NULL;
                  s = SUMA_BuildMessageLog (SUMAg_CF->MessageList);
                  SUMAg_CF->X->Log_TextShell->CursorAtBottom = YUP;
                  (void) SUMA_CreateTextShell (s, 
                              "Message Log", SUMAg_CF->X->Log_TextShell);
                  XRaiseWindow(SUMAg_CF->X->DPY_controller1, 
                                 XtWindow(SUMAg_CF->X->Log_TextShell->toplevel));
                  if (s) SUMA_free(s);
               }
            }
            break;
            
         case SE_Log:
            /* opens log window, needs nothing for the moment*/
            {
               char *s = NULL;
               if (SUMAg_CF->X->Log_TextShell) { /* just raise it */
                  XRaiseWindow(  SUMAg_CF->X->DPY_controller1, 
                                 XtWindow(SUMAg_CF->X->Log_TextShell->toplevel));
                  break;
               }else { /* create it */
                  s = SUMA_BuildMessageLog (SUMAg_CF->MessageList);
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,
                              "%s: Message string:\n%s\n", FuncName, s);
                  LogShell =  SUMA_CreateTextShellStruct (
                                  SUMA_Message_open, NULL, NULL, 
                                  SUMA_Message_destroyed, NULL, NULL);
                  if (!LogShell) {
                     fprintf (SUMA_STDERR, 
                            "Error %s: Failed in SUMA_CreateTextShellStruct.\n", 
                            FuncName);
                     break;
                  }
                  SUMAg_CF->X->Log_TextShell = 
                     SUMA_CreateTextShell(s, "SUMA log", LogShell);
                  SUMA_free(s);
               }
            }
            break;
            
         case SE_Help:
            /* opens help window, needs nothing for the moment*/
            {
               char *s = NULL;
               if (SUMAg_CF->X->Help_TextShell) { /* just raise it */
                     XRaiseWindow(SUMAg_CF->X->DPY_controller1, 
                              XtWindow(SUMAg_CF->X->Help_TextShell->toplevel));
               } else {
                  SUMAg_CF->X->Help_TextShell =  SUMA_CreateTextShellStruct (
                                    SUMA_Help_open, NULL, NULL,
                                    SUMA_Help_destroyed, NULL,
                        "https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/"
                                    "SUMA/Viewer.html#mouse-keyboard");
                  if (!SUMAg_CF->X->Help_TextShell) {
                     fprintf (SUMA_STDERR, 
                           "Error %s: Failed in SUMA_CreateTextShellStruct.\n", 
                           FuncName);
                     break;
                  }
               }
                  
               s = SUMA_help_message_Info(TXT);
               if (!s) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed in SUMA_help_message_Info.\n", 
                           FuncName);
                  break;
               }else {
                  SUMAg_CF->X->Help_TextShell = 
                        SUMA_CreateTextShell(
                              s, "SUMA help", SUMAg_CF->X->Help_TextShell);
                  SUMA_free(s);   
               }
            }
            break;
            
         case SE_Help_Xform: /* use same window as SUMA's help */
            /* opens help window, needs xform struct in vp */
            {
               SUMA_XFORM *xf=NULL;
               char *s = NULL;
               if (EngineData->vp_Dest != NextComCode) {
                  fprintf (SUMA_STDERR,
                           "Error %s: "
                           "Data not destined correctly for %s (%d).\n", 
                     FuncName, NextCom, NextComCode);
                  break;
               } 
               if (!(xf = (SUMA_XFORM *)EngineData->vp)) {
                  SUMA_S_Err("NULL input");
                  break;
               }
               if (SUMAg_CF->X->Help_TextShell) { /* just raise it */
                     XRaiseWindow(SUMAg_CF->X->DPY_controller1, 
                              XtWindow(SUMAg_CF->X->Help_TextShell->toplevel));
               } else { /* make one */
                  SUMAg_CF->X->Help_TextShell =  SUMA_CreateTextShellStruct (
                                    SUMA_Help_open, NULL, NULL,
                                    SUMA_Help_destroyed, NULL, NULL);
                  if (!SUMAg_CF->X->Help_TextShell) {
                     fprintf (SUMA_STDERR, 
                           "Error %s: Failed in SUMA_CreateTextShellStruct.\n", 
                           FuncName);
                     break;
                  }
               }
               
               if (!strcmp(xf->name,"Dot")) {
                  s = SUMA_help_xform_dot_message_Info();
               } else {
                  s = SUMA_copy_string("aint no help for this xform honey");
               }
                  
               if (!s) {
                  fprintf (SUMA_STDERR, 
                        "Error %s: Failed somehow.\n", 
                        FuncName);
                  break;
               }else {
                  SUMAg_CF->X->Help_TextShell = 
                        SUMA_CreateTextShell(
                              s, "SUMA Xform help", 
                              SUMAg_CF->X->Help_TextShell);
                  SUMA_free(s);   
               }
            }
            break;
         
         case SE_Help_Cmap:
            /* opens Cmap help window, needs Cmap in vp*/
            {
               char *s = NULL;
               SUMA_COLOR_MAP *Cmp;
               if (EngineData->vp_Dest != NextComCode) {
                  fprintf (SUMA_STDERR,
                           "Error %s: "
                           "Data not destined correctly for %s (%d).\n", 
                     FuncName, NextCom, NextComCode);
                  break;
               } 
               Cmp = (SUMA_COLOR_MAP *)EngineData->vp;
               if (SUMAg_CF->X->Help_Cmap_TextShell) { /* just raise it */
                     XRaiseWindow(
                        SUMAg_CF->X->DPY_controller1, 
                        XtWindow(SUMAg_CF->X->Help_Cmap_TextShell->toplevel));
                     break;
               }
                  
               s = SUMA_help_Cmap_message_Info(Cmp, 0);
               if (!s) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed in SUMA_help_Cmap_message_Info.\n",                            FuncName);
                  break;
               }else {
                  TextShell =  
                     SUMA_CreateTextShellStruct (  SUMA_Help_Cmap_open, 
                                                   NULL, NULL,
                                                   SUMA_Help_Cmap_destroyed,
                                                   NULL,
                              "https://afni.nimh.nih.gov/pub/dist/doc/htmldoc/"
                              "SUMA/Viewer.html#colormap-keyboard-controls");
                  if (!TextShell) {
                     fprintf (SUMA_STDERR, 
                              "Error %s: "
                              "Failed in SUMA_CreateTextShellStruct.\n", 
                              FuncName);
                     break;
                  }
                  SUMAg_CF->X->Help_Cmap_TextShell = 
                     SUMA_CreateTextShell(s, "SUMA Colormap help", TextShell);
                  SUMA_free(s);   
               }
            }
            break;
         case SE_Help_Plot:
            /* opens Plot help window, needs nothing in vp*/
            {
               char *s = NULL;
               
               if (SUMAg_CF->X->Help_Plot_TextShell) { /* just raise it */
                     XRaiseWindow(
                        SUMAg_CF->X->DPY_controller1, 
                        XtWindow(SUMAg_CF->X->Help_Plot_TextShell->toplevel));
                     break;
               }
                  
               s = SUMA_help_Plot_message_Info();
               if (!s) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed in SUMA_help_Plot_message_Info.\n",                            FuncName);
                  break;
               }else {
                  TextShell =  
                     SUMA_CreateTextShellStruct (  SUMA_Help_Plot_open, 
                                                   NULL, NULL,
                                                   SUMA_Help_Plot_destroyed,
                                                   NULL, NULL);
                  if (!TextShell) {
                     fprintf (SUMA_STDERR, 
                              "Error %s: "
                              "Failed in SUMA_CreateTextShellStruct.\n", 
                              FuncName);
                     break;
                  }
                  SUMAg_CF->X->Help_Plot_TextShell = 
                     SUMA_CreateTextShell(s, "SUMA Plot help", TextShell);
                  SUMA_free(s);   
               }
            }
            break;
         
         case SE_Whereami:
            /* opens wheremi text window, 
               Expects SO in vp, and a string for the whereami data
               In the future, it should expect a list of whereami
               data and it should build the report on its own.
               Each whereami datum should contain atlas info, space, coordinate,
               label, etc. */
            {
               char *s = NULL;
               
               if (EngineData->vp_Dest != NextComCode ||
                   EngineData->s_Dest != NextComCode) {
                  fprintf (SUMA_STDERR,
                          "Error %s: Data not destined correctly for %s (%d).\n",
                           FuncName, NextCom, NextComCode);
                  break;
               }
               if (!sv) sv = &(SUMAg_SVv[0]);
               if (!(SO = (SUMA_SurfaceObject *)EngineData->vp)) break;
               if (!(s = (char*)EngineData->s)) break;
               
               if (!SUMAg_CF->X->Whereami_TextShell) {
                  if (!(SUMAg_CF->X->Whereami_TextShell = 
                           SUMA_CreateTextShellStruct (  SUMA_Whereami_open, 
                                                   NULL, NULL,
                                                   SUMA_Whereami_destroyed,
                                                   NULL, NULL))) {
                     SUMA_S_Err("Failed to create TextShellStruct.");
                     break;
                  }
               } 
               
               if (SUMAg_CF->X->Whereami_TextShell) { /* update */
                     SUMAg_CF->X->Whereami_TextShell->CursorAtBottom = YUP;
                     (void) SUMA_CreateTextShell (s, 
                              "Where Thou Layeth", 
                              SUMAg_CF->X->Whereami_TextShell);
                     XRaiseWindow(
                        SUMAg_CF->X->DPY_controller1, 
                        XtWindow(SUMAg_CF->X->Whereami_TextShell->toplevel));
                     
               } 
            }
            break;
         case SE_Load_Group:
            /* Does not need a sv 
               expects  a pointer to .spec filename in cp, 
                        if cp is NULL then it will look for a spec structure 
                        pointer in ip (sorry, ran out of places...) 
                        it will also determine if surfaces in a spec structure 
                        pointer have already been loaded from f (really, ran out 
                        of places...) a VolumeParent name in vp,
                        the indices of the viewers to register the surfaces with 
                        in iv15. 
                        and the number of viewers specified in iv15 in i.
                        Surfaces are registered with all i viewers in iv15*/
            
            if (  EngineData->cp_Dest != NextComCode || 
                  EngineData->vp_Dest != NextComCode 
               || EngineData->iv15_Dest != NextComCode || 
                  EngineData->i_Dest != NextComCode
               || EngineData->ip_Dest != NextComCode || 
                  EngineData->f_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n"
                        "%d %d %d %d %d %d\n", 
                        FuncName, NextCom, NextComCode, EngineData->cp_Dest, 
                        EngineData->vp_Dest, 
                        EngineData->iv15_Dest, EngineData->i_Dest , 
                        EngineData->ip_Dest, EngineData->f_Dest);
               break;
            } 
            {
     		      SUMA_SurfSpecFile Spec;   
               char *VolParName = NULL, *specfilename = NULL;

               VolParName = (char *)EngineData->vp;
               specfilename = EngineData->cp;
               if (specfilename) {
                  /* Load The spec file */
		            if (LocalHead) 
                     fprintf (SUMA_STDERR, 
                              "%s: Reading Spec File %s...\n", 
                              FuncName, specfilename);
                  if (!SUMA_AllocSpecFields(&Spec)) { 
                     SUMA_S_Err("Failed to initialize spec fields."); 
                     exit(1); 
                  }
                  if (!SUMA_Read_SpecFile (specfilename, &Spec)) {
			            fprintf( SUMA_STDERR,
                              "Error %s: Error in SUMA_Read_SpecFile.\n", 
                              FuncName);
			            exit(1);
		            }	
               } else {
                  if (!EngineData->ip) {
                     fprintf( SUMA_STDERR,
                              "Error %s: Nothing in ip, nothing to do !\n", 
                              FuncName); 
                     exit(1);
                  }
                  Spec = *((SUMA_SurfSpecFile *)EngineData->ip); 
               }
               
               /* make sure only one group was read in */
		         if (Spec.N_Groups != 1) {
			         if (Spec.N_Groups > 1) {
                     fprintf( SUMA_STDERR,
                           "Error %s: "
                           "One and only one group of surfaces is allowed "
                           "at the moment (%d found).\n", 
                           FuncName, Spec.N_Groups);
			            exit(1);
                  }
                  /* We hope some DOs are visible, proceed */
		         } else {
		            if (!EngineData->f) {
                     /* load one by one surfs specified in the specs file */
		               if (LocalHead) 
                        fprintf (SUMA_STDERR, 
                                 "%s: Loading Surfaces in Spec File ...\n", 
                                 FuncName);
		               if (!SUMA_LoadSpec_eng (&Spec, SUMAg_DOv, &SUMAg_N_DOv, 
                                             VolParName, 0, SUMAg_CF->DsetList)){
			               SUMA_LH("Failed in SUMA_LoadSpec.");
			               exit(1);
		               }
                  }


                  /* register the new group with SUMA */
                  if (!SUMA_RegisterSpecGroup(SUMAg_CF, &Spec)) {
                     SUMA_SL_Err("Failed to register group");
                     break;
                  }

               }
	            /* Register surfaces in Spec file or other DOs with the 
                     surface viewer and perform setups */
               for (ii = 0; ii < EngineData->i; ++ii) {
	               SUMA_LHv("%s: Registering surfaces with surface viewer "
                           "%d/%d ...\n", FuncName, ii, EngineData->i);
                  if (!SUMA_SetupSVforDOs (&Spec, SUMAg_DOv, SUMAg_N_DOv, 
                                       &(SUMAg_SVv[EngineData->iv15[ii]]), 0)) {
			            fprintf (SUMA_STDERR, 
                              "Error %s: "
                              "Failed in SUMA_SetupSVforDOs function.\n", 
                              FuncName);
			            exit(1);
		            }
	            }

               if (LocalHead) 
                  fprintf (SUMA_STDERR, 
                           "%s: Adding call to Home and Redisplay \n", FuncName);
               /* add a call to Home and a redisplay */
               if (!list) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Should not be inside "
                           "SUMA_Engine: ZSS Feb 02 05.\n", FuncName);
                  /* list = SUMA_CreateList();*/
                  break;
               }else {
                  SUMA_LH("Appending to list ");
               }
               ED = SUMA_InitializeEngineListData (SE_Home_AllVisible);
               if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                      SEF_Empty, NULL, 
                                                      SES_Afni, NULL, NOPE, 
                                                      SEI_Tail, NULL )) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Failed to register command\n", FuncName);
                  break;
               }
               ED = SUMA_InitializeEngineListData (SE_Redisplay_AllVisible);
               if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                      SEF_Empty, NULL, 
                                                      SES_Afni, NULL, NOPE, 
                                                      SEI_Tail, NULL )) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Failed to register command\n", FuncName);
                  break;
               }
               
               if (specfilename) {
                  /* locally created spec, free contents */
                  if (!SUMA_FreeSpecFields(&Spec)) { 
                     SUMA_S_Err("Failed to free spec fields"); 
                     break;
                  }
                  
               }

   
            }
            if (LocalHead) 
               fprintf (SUMA_STDERR, "%s: Done in SE_Load_Spec.\n", FuncName);
            break;
            
         case SE_SetClip:
            {
               int iplane = -1, Delete = 0;
               /* expects a clipping plane name in EngineData->s, 
                  equation in fv15 and type in i*/
               if (EngineData->fv15_Dest != NextComCode || 
                  EngineData->s_Dest != NextComCode  || 
                  EngineData->i_Dest != NextComCode ) {
                  SUMA_S_Errv("Data not destined correctly for %s (%d).\n"
                              , NextCom, NextComCode);
                  break;
               }

               /* find plane in question */
               iplane = -1;
               for (ii=0; ii<SUMAg_CF->N_ClipPlanes; ++ii) {
                  if (  strcmp(SUMAg_CF->ClipPlanesLabels[ii], 
                               EngineData->s) == 0  
                     && SUMAg_CF->ClipPlaneType[ii] == 
                        (SUMA_CLIP_PLANE_TYPES)EngineData->i) {
                     iplane = ii; break;
                  }
               }

               /* stick equation where it belongs */
               if (EngineData->fv15[0] == 0.0 && EngineData->fv15[1] == 0.0 && 
                   EngineData->fv15[2] == 0.0 && EngineData->fv15[3] == 0.0) {
                  Delete = 1; /* no more clipping */
               } else {
                  Delete = 0; /* a plane to add, modify */
               }

               /* what to do? */
               if (Delete && iplane >=0) {
                  /* delete */
                  SUMAg_CF->ClipPlaneType[iplane] = SUMA_NO_CLIP_PLANE_TYPE;
                  SUMAg_CF->ClipPlanesLabels[iplane][0]='\0';
                  SUMAg_CF->ClipPlanes[4*iplane] = 
                     SUMAg_CF->ClipPlanes[4*iplane+1] = 
                     SUMAg_CF->ClipPlanes[4*iplane+2] = 
                     SUMAg_CF->ClipPlanes[4*iplane+3]= 0.0;
                  --SUMAg_CF->N_ClipPlanes;
                  glDisable(SUMA_index_to_clip_plane(iplane));
               } else if (Delete) {
                  /* delete what ? */
                  SUMA_SL_Warn("No plane to delete");
                  break;
               } else if (!Delete && iplane < 0) { 
                  /* add a new one */
                  if (SUMAg_CF->N_ClipPlanes == SUMA_MAX_N_CLIP_PLANES) {
                     SUMA_SLP_Err("No more clipping planes available.");
                     break;   
                  }
                  SUMAg_CF->ClipPlaneType[SUMAg_CF->N_ClipPlanes] = 
                                    (SUMA_CLIP_PLANE_TYPES)EngineData->i;
                  snprintf(SUMAg_CF->ClipPlanesLabels[SUMAg_CF->N_ClipPlanes], 
                                          8*sizeof(char), "%s", EngineData->s);
                  SUMAg_CF->ClipPlanes[4*SUMAg_CF->N_ClipPlanes  ] = 
                                                   (GLdouble)EngineData->fv15[0];
                  SUMAg_CF->ClipPlanes[4*SUMAg_CF->N_ClipPlanes+1] = 
                                                   (GLdouble)EngineData->fv15[1];
                  SUMAg_CF->ClipPlanes[4*SUMAg_CF->N_ClipPlanes+2] = 
                                                   (GLdouble)EngineData->fv15[2];
                  SUMAg_CF->ClipPlanes[4*SUMAg_CF->N_ClipPlanes+3] = 
                                                   (GLdouble)EngineData->fv15[3];
                  ++SUMAg_CF->N_ClipPlanes;
               } else {
                  /* Replace an existing one */
                  SUMAg_CF->ClipPlaneType[iplane] = 
                                          (SUMA_CLIP_PLANE_TYPES)EngineData->i;
                  snprintf(SUMAg_CF->ClipPlanesLabels[iplane], 
                                          8*sizeof(char), "%s", EngineData->s);
                  SUMAg_CF->ClipPlanes[4*iplane  ] = 
                                          (GLdouble)EngineData->fv15[0];
                  SUMAg_CF->ClipPlanes[4*iplane+1] = 
                                          (GLdouble)EngineData->fv15[1];
                  SUMAg_CF->ClipPlanes[4*iplane+2] = 
                                          (GLdouble)EngineData->fv15[2];
                  SUMAg_CF->ClipPlanes[4*iplane+3] = 
                                          (GLdouble)EngineData->fv15[3];
               }
               ED = SUMA_InitializeEngineListData (SE_Redisplay_AllVisible);
               if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                      SEF_Empty, NULL, 
                                                      SES_Afni, NULL, NOPE, 
                                                      SEI_Tail, NULL )) {
                  SUMA_S_Err("Failed to register command");
                  break;
               }
               
               /* Show the clip planes */
               SUMA_Show_Clip_Planes(SUMAg_CF, NULL);
            }
            break;
            
         case SE_SetLookAt:
            /* expects a center XYZ in EngineData->fv3[0 .. 2] */
            if (EngineData->fv3_Dest != NextComCode) {
               SUMA_S_Err("Data not destined correctly for %s (%d).\n", 
                           NextCom, NextComCode);
               break;
            }
            /* calculate the transform required to bring the new look at 
               location to the current one */
            {
               float ulook_old[3], ulook_new[3];
               int Step = 10, iStep;
               float fracUp, fracDown;
              
               ulook_old[0] = sv->GVS[sv->StdView].ViewFrom[0] - 
                              sv->GVS[sv->StdView].ViewCenter[0];
               ulook_old[1] = sv->GVS[sv->StdView].ViewFrom[1] - 
                              sv->GVS[sv->StdView].ViewCenter[1];
               ulook_old[2] = sv->GVS[sv->StdView].ViewFrom[2] - 
                              sv->GVS[sv->StdView].ViewCenter[2];
               ulook_new[0] = ulook_new[1] = ulook_new[2] = 0.0;
               fm = (float **)SUMA_allocate2D(4,4,sizeof(float));
               
               for (iStep = Step; iStep >= 1; --iStep) {
                  fracUp = (float)(iStep)/(float)Step;
                  fracDown = (float)(Step - iStep)/(float)Step;
                  SUMA_LH("%d, fracUp %f, fracDown %f, fv3[%f %f %f]\n", 
                          iStep, fracUp, fracDown, EngineData->fv3[0], 
                                 EngineData->fv3[1], EngineData->fv3[2]);
                  ulook_new[0] = (EngineData->fv3[0] * fracUp + 
                           sv->GVS[sv->StdView].ViewFrom[0] * fracDown)
                                 - sv->GVS[sv->StdView].ViewCenter[0];
                  ulook_new[1] = (EngineData->fv3[1] * fracUp + 
                           sv->GVS[sv->StdView].ViewFrom[1] * fracDown)
                                 - sv->GVS[sv->StdView].ViewCenter[1];
                  ulook_new[2] = (EngineData->fv3[2] * fracUp +
                           sv->GVS[sv->StdView].ViewFrom[2] * fracDown)
                                 - sv->GVS[sv->StdView].ViewCenter[2];
                  if (fm == NULL) {
                     SUMA_S_Err("Failed to allocate fm.");
                     break;
                  }
                  if (!SUMA_FromToRotation (ulook_new, ulook_old, fm)) {
                     SUMA_S_Err("Failed in SUMA_FromToRotation.");
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
                     SUMA_S_Err("Failed to register command");
                     break;
                  }
                                    
                  /* add a redisplay call */
                  ED = SUMA_InitializeEngineListData (SE_RedisplayNow);
                  if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                    SEF_Empty, NULL, 
                                    EngineData->Src, EngineData->Srcp, NOPE, 
                                    SEI_After, LocElm )) {
                     SUMA_S_Err("Failed to register command");
                     break;
                  }
                  
               }
               /* fm was copied into engine data, free it */
               SUMA_free2D((char **)fm, 4);
            }
            break;
         
         case SE_StartListening:
            /* expects nothing in EngineData */
            if (!SUMAg_CF->Listening) {
               SUMAg_CF->Listening = !SUMAg_CF->Listening;
               fprintf(SUMA_STDERR,"%s: Starting to listen ...\n", FuncName);
               /* Make sure special NIML rowtypes are defined before */
               get_NI_tract_type();
               /* start the listening WorkProcess */
               if (!SUMAg_CF->niml_work_on) {
                  SUMA_LH("registering SUMA_niml_workproc...");
                  SUMA_register_workproc(SUMA_niml_workproc, (XtPointer)sv);
               } else {
                  SUMA_LH("SUMA_niml_workproc Already on.");
               }
            } else {
               /* if already on, just close streams */
               /* closing the streams */
               SUMA_S_Text("Closing streams, but still listening ...");
               /* kill the streams */
               for (ii=0; ii< SUMA_MAX_STREAMS; ++ii) {
                  if (ii != SUMA_AFNI_STREAM_INDEX) {
                        /* leave AFNI connection separate */
                     NI_stream_close( SUMAg_CF->ns_v[ii] ) ;
                     SUMAg_CF->ns_v[ii] = NULL ;
                     SUMAg_CF->ns_flags_v[ii] = 0;
                     SUMAg_CF->TrackingId_v[ii] = 0;
                  }
               }
            } 
            break;
            
         case SE_ToggleConnected:
            /* expects nothing in EngineData */
            if (!SUMA_CanTalkToAfni (SUMAg_DOv, SUMAg_N_DOv)) {
               fprintf(SUMA_STDOUT,
                        "%s: Cannot connect to AFNI.\n"
                        "\tNot one of the surfaces is mappable "
                        "and has a Surface Volume.\n"
                        "\tDid you use the -sv option when launching SUMA ?\n", 
                        FuncName);
               break;
            }
               
            SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] = 
                              !SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX];
            if (SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX]) {
               if (!SUMA_niml_call (SUMAg_CF, SUMA_AFNI_STREAM_INDEX, YUP)) {
                  /* conection flag is reset in SUMA_niml_call */
                  break;
               }
                  
               /* start the listening WorkProcess */
               if (!SUMAg_CF->niml_work_on) {
                  SUMA_LH("registering SUMA_niml_workproc...");
                  SUMA_register_workproc(SUMA_niml_workproc, (XtPointer)sv); 
               } else {
                  SUMA_LH("SUMA_niml_workproc Already on.");
               }

               /* register a call for sending the surface to afni (SetAfniSurf)*/
               if (LocalHead) 
                  fprintf(SUMA_STDERR,"Notifying Afni of New surface...\n");
               ED = SUMA_InitializeEngineListData (SE_SetAfniSurf);
               SUMA_RegisterEngineListCommand (list, ED, 
                                                SEF_Empty, NULL,
                                                SES_Suma, (void *)sv, NOPE,
                                                SEI_Head, NULL); 
               break;
            } else {
               fprintf(SUMA_STDOUT,"%s: Disconnecting from afni.\n", FuncName);

               if (!SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX]) {
                  /* It looks like the stream was closed, do the clean up */
                  SUMA_S_Warn("sv->ns is null, stream must have gotten closed.\n"
                              "Cleaning up ...\n");
                  ED = SUMA_InitializeEngineListData (SE_CloseStream4All);
                  ii = SUMA_AFNI_STREAM_INDEX;
                  SUMA_RegisterEngineListCommand (list, ED, 
                                                SEF_i, (void*)&ii,
                                                SES_Suma, (void *)sv, NOPE,
                                                SEI_Head, NULL); 

                  break;
               }


               /* Close the stream if nobody else wants it. 
               This is not a great condition, one should be able to leave the 
               stream open 
               even if no viewer, for the moment, does not want to talk to AFNI.
               Perhaps in the future. */
               if (SUMAg_N_SVv == 1) {
                  SUMA_S_Note("Nobody wants to talk to AFNI anymore\n"
                              "closing stream ...\n");
                  NI_stream_close(SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX]);
                  SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] = NULL;
                  SUMAg_CF->ns_flags_v[SUMA_AFNI_STREAM_INDEX] = 0;
                  SUMAg_CF->TrackingId_v[SUMA_AFNI_STREAM_INDEX] = 0;
               }
               break;
            }
   
         case SE_CloseStream4All:
            /* expects the stream index in i in EngineData */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            }   
            /* odds are communicating program died or closed stream, 
               mark all surfaces as unsent */
            if (EngineData->i == SUMA_AFNI_STREAM_INDEX) {
               for (ii=0; ii<SUMAg_N_DOv; ++ii) {
                  if (SUMA_isSO(SUMAg_DOv[ii])) {
                     SO = (SUMA_SurfaceObject *)(SUMAg_DOv[ii].OP);
                     if (SO->SentToAfni) SO->SentToAfni = NOPE;
                  }
               }
            }
            
            /* same for parent fields */
            /* check first if stream in SUMAg_CF still good by any chance */
            nn = NI_stream_goodcheck(SUMAg_CF->ns_v[EngineData->i] , 1 ) ;
            
            if( nn >= 0 ){ 
               SUMA_S_Err("Stream still alive, this should not be. "
                          "Closing anyway."); 
               NI_stream_close(SUMAg_CF->ns_v[EngineData->i]); 
            }
            
            /* clean up and get out of here*/         
            SUMAg_CF->ns_v[EngineData->i] = NULL;
            SUMAg_CF->ns_flags_v[EngineData->i] = 0;
            SUMAg_CF->TrackingId_v[EngineData->i] = 0;
            if (EngineData->i != SUMA_AFNI_STREAM_INDEX) {  
               /* Connected for AFNI line handled elsewhere */
               SUMAg_CF->Connected_v[EngineData->i] = NOPE;
            }
            break;
            
         case SE_SetForceAfniSurf:
            /* expects nothing in EngineData */
            /* send to afni surfaces that can be sent even if they 
               have been sent already */
            #if 0 /* pre Oct 26 */
            for (ii=0; ii<sv->N_DO; ++ii) {
               if (SUMA_isSO(SUMAg_DOv[sv->RegisteredDO[ii]])) {
                  SO = (SUMA_SurfaceObject *)
                           (SUMAg_DOv[sv->RegisteredDO[ii]].OP);
                  if (SO->SentToAfni) SO->SentToAfni = NOPE;
               }
            }
            #else
               /* send all geometrically correct surfaces */
               for (ii=0; ii<SUMAg_N_DOv; ++ii) {
                  if (SUMA_isSO(SUMAg_DOv[ii])) {
                     SO = (SUMA_SurfaceObject *)(SUMAg_DOv[ii].OP);
                     if (SO->AnatCorrect && SO->SentToAfni) 
                        SO->SentToAfni = NOPE;
                  }
               }
            #endif
            /* proceed to SE_SetAfniSurf: */
            ED = SUMA_InitializeEngineListData (SE_SetAfniSurf);
            SUMA_RegisterEngineListCommand (list, ED, 
                                            SEF_Empty, NULL,
                                            SES_Suma, (void *)sv, NOPE,
                                            SEI_Head, NULL); 
            break;
            
         case SE_SetAfniMask:
            if (SUMAg_CF->Dev) { /* Send a mask surface to AFNI */
               int nels_sent=0;
               SUMA_ALL_DO *ado = NULL;
               SUMA_MaskDO *mdo = NULL;
               if ( EngineData->s_Dest != NextComCode ||
                    EngineData->fv3_Dest != NextComCode) {
                  fprintf (SUMA_STDERR,
                     "Error %s: Data not destined correctly for %s (%d).\n",
                     FuncName, NextCom, NextComCode);
                  break;
               }
               ado = SUMA_whichADOg(EngineData->s);
               if (!ado || ado->do_type != MASK_type) {
                  SUMA_S_Warn("Mask not found or ado of wrong type");
                  break;
               }
               mdo = (SUMA_MaskDO*)ado;
               if (!(SO = mdo->SO)) {
                  SUMA_S_Warn("No mask SO");
                  break;
               }
               SUMA_LH("Sending mask %s's surface, SO = %p...", 
                           ADO_LABEL(ado), SO);
               if (!SO->SentToAfni) {
                  SUMA_LH("Sending the whole thing, SO = %p", SO);
                  nel = NI_new_data_element("SUMA_mask", 0);
                  NI_set_attribute(nel, "idcode", ADO_ID(ado));
                  NI_SET_FLOATv(nel, "init_cen", mdo->init_cen, 3);
                  NI_SET_FLOATv(nel, "new_cen", EngineData->fv3, 3);
                  if ((nn = NI_write_element( 
                              SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , nel , 
                                             NI_TALK_MODE ))<0) {
                          SUMA_S_Err("NI_write_element failed\n");
                  }
                  if (LocalHead) SUMA_ShowNel(nel);
                  NI_free_element(nel);
                  nel = NULL;
                  ++nels_sent;
                  if (1) {
                     float delta[3] = {0, 0, 0};
                     delta[0] = mdo->init_cen[0] - EngineData->fv3[0];
                     delta[1] = mdo->init_cen[1] - EngineData->fv3[1];
                     delta[2] = mdo->init_cen[2] - EngineData->fv3[2];
                     nel = SUMA_makeNI_SurfIXYZ (SO);
                     /* Undo the offset in the surface coords ... */
                     SUMA_offset_NI_SurfIXYZ(nel, delta);
                     /* label this object as being the surface of a mask */
                     if (!nel) {
                        fprintf(SUMA_STDERR,
                                 "Error %s: SUMA_makeNI_SurfIXYZ failed\n", 
                                 FuncName);
                        break;
                     }
                     NI_set_attribute(nel,"parent_type", "SUMA_mask");
                     NI_set_attribute(nel,"parent_idcode", ADO_ID(ado));
                     /* set up some defaults color 
                        (see matlab's rgbdectohex for visual aid) 
                        info for AFNI interface */
                           NI_set_attribute(nel,
                              "afni_surface_controls_toggle", "on");
                           NI_set_attribute(nel,
                              "afni_surface_controls_nodes","none");
                           NI_set_attribute(nel,
                              "afni_surface_controls_lines",
                                 SUMA_RGB_to_hex(mdo->init_col, NULL));
                           NI_set_attribute(nel,
                              "afni_surface_controls_plusminus","none");
                           NI_SET_INT(nel,
                                 "afni_surface_controls_linewidth", 10 );

                     /* send surface nel */
                     if (LocalHead) 
                        fprintf(SUMA_STDERR,"%s: Sending SURF_iXYZ nel...\n ",                                                FuncName) ;
                     nn = NI_write_element( 
                              SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , nel , 
                                             NI_TALK_MODE ) ;

                     if( nn < 0 ){
                          SUMA_S_Err("NI_write_element failed\n");
                     }

                     if (LocalHead) SUMA_ShowNel(nel);
                     NI_free_element(nel);
                     nel = NULL;
                     ++nels_sent;
                  }
                  if (1) {
                     /* send node normals       ZSS Oct 05 04 */
                     nel = SUMA_makeNI_SurfINORM (SO);
                     if (!nel) {
                        SUMA_S_Err("SUMA_makeNI_SurfINORM failed");
                        break;
                     }
                     NI_set_attribute(nel,"parent_type", "SUMA_mask");
                     NI_set_attribute(nel,"parent_idcode", ADO_ID(ado));
                     /* send surface nel */
                     if (LocalHead)    
                        fprintf(SUMA_STDERR,
                                "%s: Sending SURF_NORM nel ...\n", FuncName) ;
                     nn = NI_write_element( 
                              SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , nel , 
                                             NI_TALK_MODE ) ;
                     if( nn < 0 ){
                          SUMA_S_Err("NI_write_element failed");
                     }
                     NI_free_element(nel);
                     nel = NULL;
                     ++nels_sent;
                  }

                  if (1) {
                     /* send triangles */
                     nel = SUMA_makeNI_SurfIJK (SO);
                     if (!nel) {
                        SUMA_S_Err("SUMA_makeNI_SurfIJK failed");
                        break;
                     }
                     NI_set_attribute(nel,"parent_type", "SUMA_mask");
                     NI_set_attribute(nel,"parent_idcode", ADO_ID(ado));
                     /* send surface nel */
                     if (LocalHead)    
                        fprintf(SUMA_STDERR,"%s: Sending SURF_IJK nel ...\n", 
                                            FuncName) ;
                     nn = NI_write_element( 
                              SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , nel , 
                                             NI_TALK_MODE ) ;

                     if( nn < 0 ){
                        SUMA_S_Err("NI_write_element failed");
                     }
                     NI_free_element(nel);
                     nel = NULL;
                     ++nels_sent;
                  }
                  if (nels_sent) {
                     /* mark surface as sent to afni */
                     SO->SentToAfni = YUP;
                  } else {
                     SUMA_SL_Warn("Nothing sent dude, what's happening?");
                  }
               }
               
               if (1) {
                  SUMA_LH("Sending new center, SO = %p", SO);
                  nel = NI_new_data_element("SUMA_mask", 0);
                  NI_set_attribute(nel, "idcode", ADO_ID(ado));
                  NI_SET_FLOATv(nel, "new_cen", EngineData->fv3, 3);
                  if ((nn = NI_write_element( 
                              SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , nel , 
                                             NI_TALK_MODE ))<0) {
                          SUMA_S_Err("NI_write_element failed\n");
                  }
                  if (LocalHead) SUMA_ShowNel(nel);
                  NI_free_element(nel);
                  nel = NULL;
                  ++nels_sent;
               }
            
            } break;
            
         case SE_SetAfniSurfList:
            /* expects ivec in EngineData and a string in s saying what is to be 
               sent, a flag in i 1=report transmission, 0 = be quiet*/
            { int nels_sent, N_Send, *SendList;
              char *stmp = NULL; 
              static char LastPrefix[THD_MAX_PREFIX+1] = {"nuda"};
               if (EngineData->ivec_Dest != NextComCode || 
                  EngineData->s_Dest != NextComCode || 
                  EngineData->i_Dest != NextComCode) {
                  fprintf (SUMA_STDERR,
                     "Error %s: Data not destined correctly for %s (%d).\n",
                     FuncName, NextCom, NextComCode);
                  break;
               }
               N_Send = EngineData->ivec->n;
               SendList = EngineData->ivec->v; 
               /* send to afni the list of surfaces in SendList*/
               if (N_Send) {
                  for (ii=0; ii<N_Send; ++ii) {
                     nels_sent = 0;
                     SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SendList[ii]].OP);
                     if (EngineData->i && SO->Label) 
                        fprintf(SUMA_STDERR,"%s: Sending surface %s (%s)...\n", 
                                             FuncName, SO->Label, EngineData->s);
                     if (SUMA_iswordin(EngineData->s,"NodeList") == 1) {
                        nel = SUMA_makeNI_SurfIXYZ (SO);
                        if (!nel) {
                           fprintf(SUMA_STDERR,
                                    "Error %s: SUMA_makeNI_SurfIXYZ failed\n", 
                                    FuncName);
                           break;
                        }
                        /* set up some defaults color 
                           (see matlab's rgbdectohex for visual aid) 
                           info for AFNI interface */
                        switch(ii) {
                           case 0: /* typically wm left*/
                              NI_set_attribute(nel,
                                 "afni_surface_controls_toggle", "on");
                              NI_set_attribute(nel,
                                 "afni_surface_controls_nodes","none");
                              NI_set_attribute(nel,
                                 "afni_surface_controls_lines","#00ff00"); 
                                                            /* green 0 255 0 */
                              NI_set_attribute(nel,
                                 "afni_surface_controls_plusminus","none");
                              break;
                           case 1: /* typically pial left */
                              NI_set_attribute(nel,
                                 "afni_surface_controls_toggle", "on");
                              NI_set_attribute(nel,
                                 "afni_surface_controls_nodes","none");
                              NI_set_attribute(nel,
                                 "afni_surface_controls_lines","#0000ff");  
                                                         /* 0 0  225  blue */  
                              NI_set_attribute(nel,
                                 "afni_surface_controls_plusminus","none");
                              break;
                           case 2: /* typically wm right */
                              NI_set_attribute(nel,
                                 "afni_surface_controls_toggle", "on");
                              NI_set_attribute(nel,
                                 "afni_surface_controls_nodes","none");
                              NI_set_attribute(nel,
                                 "afni_surface_controls_lines","#ffff00");   
                                                            /* green 0 255 0*/ 
                              NI_set_attribute(nel,
                                 "afni_surface_controls_plusminus","none");
                              break;
                           case 4: /* typically pial right */
                              NI_set_attribute(nel,
                                 "afni_surface_controls_toggle", "on");
                              NI_set_attribute(nel,
                                 "afni_surface_controls_nodes","none");
                              NI_set_attribute(nel,
                                 "afni_surface_controls_lines","#ff0000");  
                                                            /* red 255 0 0 */
                              NI_set_attribute(nel,
                                 "afni_surface_controls_plusminus","none");
                              break;
                           default: /* hot pink */
                              NI_set_attribute(nel,
                                 "afni_surface_controls_toggle", "on");
                              NI_set_attribute(nel,
                                 "afni_surface_controls_nodes","none");
                              NI_set_attribute(nel,
                                 "afni_surface_controls_lines","#ff69b4");  
                                                   /* hot pink 255 105 180 */
                              NI_set_attribute(nel,
                                 "afni_surface_controls_plusminus","none");
                              break;
                        }
                        /* send surface nel */
                        if (LocalHead) 
                           fprintf(SUMA_STDERR,"%s: Sending SURF_iXYZ nel...\n ",                                                FuncName) ;
                        nn = NI_write_element( 
                                 SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , nel , 
                                                NI_TALK_MODE ) ;

                        if( nn < 0 ){
                             SUMA_S_Err("NI_write_element failed\n");
                        }

                        #if 0
                           {
                              NI_stream nstdout;
                              nstdout = NI_stream_open( "fd:1","w");
                              if( nstdout == NULL ){ 
                                 fprintf(SUMA_STDERR,"Can't open fd:1\n"); 
                                 break; 
                              }
                              NI_write_element( nstdout , nel , NI_TEXT_MODE ) ;
                              NI_stream_close(nstdout);
                           }
                        #endif

                        NI_free_element(nel);
                        nel = NULL;
                        ++nels_sent;
                     }
                     if (SUMA_iswordin(EngineData->s,"NodeNormList") == 1) {
                        /* send node normals       ZSS Oct 05 04 */
                        nel = SUMA_makeNI_SurfINORM (SO);
                        if (!nel) {
                           SUMA_S_Err("SUMA_makeNI_SurfINORM failed");
                           break;
                        }
                        /* send surface nel */
                        if (LocalHead)    
                           fprintf(SUMA_STDERR,
                                   "%s: Sending SURF_NORM nel ...\n", FuncName) ;
                        nn = NI_write_element( 
                                 SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , nel , 
                                                NI_TALK_MODE ) ;
                        if( nn < 0 ){
                             SUMA_S_Err("NI_write_element failed");
                        }
                        NI_free_element(nel);
                        nel = NULL;
                        ++nels_sent;
                     }
                     
                     if (SUMA_iswordin(EngineData->s,"FaceSetList") == 1) {
                        /* send triangles */
                        nel = SUMA_makeNI_SurfIJK (SO);
                        if (!nel) {
                           SUMA_S_Err("SUMA_makeNI_SurfIJK failed");
                           break;
                        }
                        /* send surface nel */
                        if (LocalHead)    
                           fprintf(SUMA_STDERR,"%s: Sending SURF_IJK nel ...\n", 
                                               FuncName) ;
                        nn = NI_write_element( 
                                 SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , nel , 
                                                NI_TALK_MODE ) ;

                        if( nn < 0 ){
                           SUMA_S_Err("NI_write_element failed");
                        }
                        NI_free_element(nel);
                        nel = NULL;
                        ++nels_sent;
                     }
                     if (nels_sent) {
                        /* mark surface as sent to afni */
                        SO->SentToAfni = YUP;
                     } else {
                        SUMA_SL_Warn("Nothing sent dude, what's happening?");
                     }
                  }
                  /* Now make afni switch to the surface volume of the 
                     last surface 
                  ZSS Sept 28 06: To avoid the jamming of
                  AFNI and SUMA's write buffers. It used to be
                  That AFNI switched automatically at the first surface
                  and started writing colored data while suma was still
                  writing surfaces. As a result, both programs got stuck
                  waiting for the write operation to finish and it never
                  did because buffers got full and no one was busy listening
                  Kudos to Rick R. for having figured the Ladies' bug source */
                  SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SendList[N_Send-1]].OP);
                  if (SO->VolPar && SO->VolPar->filecode) {
                     if (strcmp(LastPrefix, SO->VolPar->prefix)) {
                        nel = NI_new_data_element("ni_do", 0);
                        NI_set_attribute ( nel, "ni_verb", "DRIVE_AFNI");
                        stmp = SUMA_append_string("SWITCH_UNDERLAY A.", 
                                                  SO->VolPar->prefix);
                        stmp = SUMA_append_replace_string(stmp, "0", " ", 1);
                        NI_set_attribute ( nel, "ni_object", stmp);
                        fprintf(SUMA_STDERR,
                              "%s: Sending switch underlay command to (%s)...\n",                               FuncName, stmp);
                        if (NI_write_element( 
                                 SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , nel, 
                                                NI_TALK_MODE ) < 0) {
                           SUMA_SLP_Err("Failed to send SWITCH_ANATOMY to afni");
                        }
                        NI_free_element(nel) ; nel = NULL;
                        if (stmp) SUMA_free(stmp); stmp = NULL;
                        snprintf(LastPrefix, THD_MAX_PREFIX, "%s", 
                                 SO->VolPar->prefix);
                     }
                  } 
                  
               }

               break;
            }
            
         case SE_SetAfniSurf:
            /* expects nothing in EngineData */
            {  int N_Send, *SendList, ti=1;
               SUMA_IVEC ivec;
               /* send to afni the list of anatomically correct surfaces 
                  and with a surface volume*/
               /* No surfaces are sent twice because there should not be 
                  duplicate  local domain parent surfaces in SUMAg_DOv */
               /* prior to Wed Nov  6 17:47:20 EST 2002, only mappable surfaces 
                  that are related to the ones shown in the viewer
                  were being sent to AFNI. Now all mappable surfaces loaded are 
                  sent regardless of what is shown */
               /* Jan. 08 04: 
                  All anatomically correct surfaces are now sent to AFNI */
               SendList = 
                  SUMA_FormSOListToSendToAFNI(SUMAg_DOv , SUMAg_N_DOv, &N_Send);
               if (N_Send > 0) {
                  ivec.v = SendList;
                  ivec.n = N_Send;
                  ED = SUMA_InitializeEngineListData (SE_SetAfniSurfList);
                  if (!(LocElm = SUMA_RegisterEngineListCommand (  list, ED,
                                                  SEF_ivec, (void *)(&ivec), 
                                                  SES_Suma, (void *)sv, 
                                                  NOPE, 
                                                  SEI_Tail, NULL))) {
                     fprintf(SUMA_STDERR,
                             "Error %s: Failed to register element\n", FuncName);
                     break;
                  }
                  if (!(LocElm = SUMA_RegisterEngineListCommand (  
                        list, ED,
                        SEF_s, (void *)("NodeList, FaceSetList, NodeNormList"), 
                        SES_Suma, (void *)sv, NOPE, 
                        SEI_In, LocElm))) {
                     fprintf(SUMA_STDERR,
                             "Error %s: Failed to register element\n", FuncName);
                     break;
                  }
                  if (!(LocElm = SUMA_RegisterEngineListCommand (  list, ED,
                                                SEF_i, (void *)&ti, 
                                                SES_Suma, (void *)sv, NOPE, 
                                                SEI_In, LocElm))) {
                     fprintf(SUMA_STDERR,
                             "Error %s: Failed to register element\n", FuncName);
                     break;
                  }
                  if (SendList) SUMA_free(SendList); SendList = NULL;   
               }else {
                  if (N_Send < 0 && 
                      !SUMA_Anatomical_DOs(SUMAg_DOv , SUMAg_N_DOv, NULL)) {
                     SUMA_SLP_Warn(
                        "None of the surfaces were marked as 'Anatomical'\n"
                        "So none were sent to AFNI. You can label a surface\n"
                        "as Anatomical by adding 'Anatomical = Y' where the \n"
                        "surface is declared in the .spec file.");
                  }
               }

               break;
            }
         
         case SE_SetAfniThisSurf:
            /* expects an idcode_str in EngineData->cp and what needs to be sent 
               in EngineData->s for surface to be sent to AFNI */
            {
               SUMA_IVEC ivec;
               if (EngineData->s_Dest != NextComCode || 
                  EngineData->cp_Dest != NextComCode || 
                  EngineData->i_Dest != NextComCode) {
                  fprintf (SUMA_STDERR,
                           "Error %s: Data not destined correctly for %s \n"
                           "((%d %d %d) %d).\n",
                     FuncName, NextCom,  
                     EngineData->s_Dest, EngineData->cp_Dest, 
                     EngineData->i_Dest, NextComCode);
                  break;
               }
               i = SUMA_findSO_inDOv(EngineData->cp, SUMAg_DOv, SUMAg_N_DOv);
               if (i<0) {
                  SUMA_SL_Err("Surface Not Found!");
                  break;
               }
               ivec.n = 1;
               ivec.v = (int*)SUMA_malloc(ivec.n*sizeof(int));
               ivec.v[0] = i;
               ED = SUMA_InitializeEngineListData (SE_SetAfniSurfList);
               if (!(LocElm = SUMA_RegisterEngineListCommand (  list, ED,
                                                      SEF_ivec, (void *)(&ivec), 
                                                      SES_Suma, (void *)sv, NOPE,
                                                      SEI_Tail, NULL))) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Failed to register element\n", FuncName);
                  break;
               }
               if (!(LocElm = SUMA_RegisterEngineListCommand (  list, ED,
                                                SEF_s, (void *)(EngineData->s), 
                                                SES_Suma, (void *)sv, NOPE, 
                                                SEI_In, LocElm))) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Failed to register element\n", FuncName);
                  break;
               }
               if (!(LocElm = SUMA_RegisterEngineListCommand (  list, ED,
                                                SEF_i, (void *)&(EngineData->i), 
                                                SES_Suma, (void *)sv, NOPE, 
                                                SEI_In, LocElm))) {
                  fprintf(SUMA_STDERR,
                          "Error %s: Failed to register element\n", FuncName);
                  break;
               }
               SUMA_free(ivec.v);
               break;
            }
               
         case SE_ToggleShowSelectedNode:
            /* expects nothing in EngineData */
            {
               int st = SUMA_SV_GetShowSelectedDatum(sv);
               SUMA_SV_SetShowSelectedDatum(sv, !st,NOPE);
            }
            break;

         case SE_SetSelectedNode: {
            SUMA_ALL_DO *adodat=NULL;
            char *idcode=NULL, *eee=NULL;
            NI_element *nel=NULL;

            /* expects a node (datum) index in i and maybe a ngr in ngr
            ngr is only being used when AFNI is the src of the call*/
            if (EngineData->i_Dest   != NextComCode ||
                EngineData->ngr_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            } 
            if (!(ado = SUMA_SV_Focus_ADO(sv))) {
               /* No so in focus */
               SUMA_S_Err("No SO/DO in focus");
               break;
            }
            
            if ( EngineData->ngr && 
                 (nel = SUMA_FindNgrNamedElement(EngineData->ngr, 
                                                 "SUMA_crosshair_xyz"))) {
                  if ((idcode = NI_get_attribute(nel, "surface_idcode")) &&
                      (adodat = iDO_ADO(SUMA_whichDOg(idcode)))) {
                /* Make sure datum can be related to the DO in Focus 
                   For now, all one gets is surfaces, but volume id will
                   come next. Then you'll need to check for grid match perhaps
                   before proceeding*/
                      if (!SUMA_isRelated(adodat, ado,2)) {
                        SUMA_LH("No relative of mine");
                        break;
                      }
                  }
            }
            
            if (EngineData->i >= 0 && 
                  EngineData->i <= SUMA_ADO_Max_Datum_Index(ado)) {
               switch (ado->do_type) {
                  case VO_type:
                     if ( EngineData->iv15_Dest != NextComCode ||
                          EngineData->fv15_Dest != NextComCode ) {
                        SUMA_S_Err(
                           "Data not destined correctly for VOtype %s (%d).\n",
                            NextCom, NextComCode);
                        break;
                     } 
                     SUMA_ADO_Set_SelectedDatum(ado, EngineData->i, 
                                    (void *)EngineData->iv15, 
                                    (void *)EngineData->fv15);
                     break;
                  case TRACT_type:
                     if ( EngineData->iv15_Dest != NextComCode ) {
                        SUMA_S_Err(
                           "Data not destined correctly for VOtype %s (%d).\n",
                            NextCom, NextComCode);
                        break;
                     }  
                     SUMA_ADO_Set_SelectedDatum(ado, 
                                EngineData->i, (void *)EngineData->iv15, NULL);
                     break;
                  default:
                     SUMA_ADO_Set_SelectedDatum(ado, EngineData->i, NULL, NULL);
                     break;
               }
            } else {
               /* ignore -1, used in initializations */
               if (EngineData->i != -1) { 
                  SUMA_SLP_Err(
                        "Datum index (%d) < 0 || >= Number of data nodes (%d)",
                        EngineData->i, SUMA_ADO_Max_Datum_Index(ado)+1);
                  SUMA_DUMP_TRACE("Whence the bad index");
               }
               break;
            }
            /* Nick's options. Jump to the sub-brick index corresponding 
               to node */
            if (!(curColPlane = SUMA_ADO_CurColPlane(ado))) {
               SUMA_S_Err("No cur plane");
               break;
            }
            SUMA_LHv("Have %d, %d and %d\n",
               SUMAg_CF->YokeIntToNode,
               SDSET_VECNUM(curColPlane->dset_link),
               SUMA_ADO_N_Datum(ado));
            if (SUMAg_CF->YokeIntToNode   &&
                !(SDSET_VECNUM(curColPlane->dset_link) % 
                                             SUMA_ADO_N_Datum(ado))) {
               itmp = SDSET_VECNUM(curColPlane->dset_link) / 
                                             SUMA_ADO_N_Datum(ado);
               if (!SUMA_SwitchColPlaneIntensity(ado, curColPlane,
                                                 EngineData->i*itmp, 1)) {
                  SUMA_S_Err("Failed to yoke intensity to node index");
               }
            }
            SUMAg_CF->YokeIntToNode = 0;
            
            /* NB: I find it strange that there is this block here
               followed by a similar one in SUMA_UpdateNodeField
               Check for possible duplications ... */
            if (SUMAg_CF->callbacks && !SUMAg_CF->HoldClickCallbacks) {
               SUMA_LH("Activating callbacks");
               if (!SUMA_Selected_Node_Activate_Callbacks (
                        ado, curColPlane,
                        EngineData->Src, EngineData->ngr)) {
                  SUMA_S_Err("Failed to activate callbacks");
               }
            }
            
            /* get the controller up if it is not, and the mood is right*/
            SUMA_LH("Get controller up");
            SUMA_OpenSurfCont_if_other(sv->X->TOPLEVEL, ado, sv);
            
            SUMA_LH("Update node field");
            SUMA_UpdateNodeField(ado);
            SUMA_LH("Done with SE_SetSelectedNode");
            
            /* Might need to update nodemask -- experimental, need much love 
               and cleanup 
               Limitations: 1- constant cmask operation 
                            2- reloads all data for mask expression!
                            3- will not work without controller 
                            4- Works only with integers */
            if (SUMAg_CF->Dev && ado->do_type == SO_type &&
                (eee = SUMA_EnvVal("SUMA_TEMP_NODE_CMASK_EXPR"))){
               SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado; 
               SUMA_DRAW_MASKS *DW = SO->DW;
               char *etmp=NULL, stmp[32];
               int nxyz;
               float val;
               
               /* 
                  Toy example for expression:
  setenv SUMA_TEMP_NODE_CMASK_EXPR '-a labels.niml.dset -expr equals(a,$SEL)' 
                  Or better yet, for the TT atlas isosurfaces
  setenv SUMA_TEMP_NODE_CMASK_EXPR '-a labels.niml.dset -expr equals(a,mod($SEL,200))+equals(a,mod($SEL,200)+200)'
              */
               TLH(1);
               DW->user_exp = 
                  SUMA_copy_string(eee);
               DW->cmask_exp = SUMA_copy_string(DW->user_exp);
               if (SUMA_GetValuesAtSelection(ado, 1, &val, NULL, NULL)) {
                  sprintf(stmp,"%d", (int)val);
                  SUMA_Swap_String(&(DW->cmask_exp), "$SEL", stmp);
                  if (!DW->last_cmask_exp || 
                       strcmp(DW->last_cmask_exp, DW->cmask_exp)) {
                     /* This is a brute force regen, just for testing */
                     SUMA_LH("Evaluating %s", DW->cmask_exp);
                     SUMA_ifree(DW->nodemask);
                     etmp = SUMA_copy_string(DW->cmask_exp); /* EDT_calcmask
                                                         modifies the string */
                     DW->nodemask =  EDT_calcmask(etmp, &nxyz, SO->N_Node );
                     SUMA_ifree(etmp);
                     if (!DW->nodemask) {
                        SUMA_S_Err("Failed to setup mask with %s", 
                                   DW->cmask_exp);
                     } else {
                        if (nxyz != SO->N_Node) {
                           SUMA_S_Err("Bigus calamitous!\n"
                                   "length of mask (%d) != SO->N_Node (%d).\n",
                                      nxyz, SO->N_Node);
                           SUMA_ifree(DW->nodemask);
                        } else {
                           DW->last_cmask_exp = SUMA_copy_string(DW->cmask_exp);
                           for (i=0, DW->N_nz_nodemask=0; i<SO->N_Node; ++i) 
                              if (DW->nodemask[i]) ++DW->N_nz_nodemask;
                           ++DW->PatchRegenID;
                           SUMA_LH("OK, %d non zeros, regenID %d", 
                                  DW->N_nz_nodemask, DW->PatchRegenID);
                        }
                     }

                  } else {
                     SUMA_LH("No re-eval last_cmask_exp: %s", 
                             DW->last_cmask_exp);
                  }
               } else {
                  SUMA_LH("Failed to get selection!");
               }
               TLH(0);
            }

            break; }
         case SE_ToggleShowSelectedFaceSet:
            /* expects nothing ! */
            { 
               int st = SUMA_SV_GetShowSelectedFaceSet(sv);
               SUMA_SV_SetShowSelectedFaceSet(sv,!st,NOPE);  
            }          
            break;
         
         case SE_SetSelectedFaceSet:
            /* expects the index for the selected FaceSet */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            } 
            if (!(ado = SUMA_SV_Focus_ADO(sv))) {
               SUMA_S_Err("No SO/ADO in focus");
               break;
            }
            switch(ado->do_type) {
               case SO_type: {
                  SUMA_SurfaceObject *SO=(SUMA_SurfaceObject *)ado;
                  SUMA_VisX_Pointers4Display(SO, 1);/*coordinates as displayed*/
                  if (EngineData->i < 0 || EngineData->i >= SO->N_FaceSet) {
                     if (EngineData->i != -1){
                        SUMA_SLP_Err("Node index < 0 || "
                                     "> Number of FaceSets in surface");
                     } 
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
                  SUMA_VisX_Pointers4Display(SO, 0);/* revert to surf cooords. */
                  SUMA_UpdateTriField(SO);
                  break; }
               case GDSET_type:
                  SUMA_S_Err("ambigous display method without variant");
                  break;
               case CDOM_type:
                  SUMA_S_Err("Help me please");
                  break;
               case GRAPH_LINK_type: {
                  SUMA_DSET *dset = SUMA_find_GLDO_Dset((SUMA_GraphLinkDO*)ado);
                  SUMA_GRAPH_SAUX *Saux = SDSET_GSAUX(dset);
                  if (EngineData->i < dset->Aux->range_node_index[0] || 
                      EngineData->i > dset->Aux->range_node_index[1]) {
                     if (EngineData->i != -1){
                        SUMA_SLP_Err("Node index < 0 || "
                                     "> Number of nodes in graph dataset");
                        SUMA_S_Errv("Index is %d, current range is: %ld %ld\n", 
                                     EngineData->i,
                                     dset->Aux->range_node_index[0], 
                                     dset->Aux->range_node_index[1]);
                        SUMA_DUMP_TRACE("WEIRD 3");
                     } 
                     break;
                  }
                  Saux->PR->iAltSel[SUMA_ENODE_0] = EngineData->i;
                  Saux->PR->datum_index = -1;
                  SUMA_UpdatePointField((SUMA_ALL_DO*)ado);
                  break;}
               case TRACT_type: {
                  SUMA_TractDO *tdo = (SUMA_TractDO *)ado;
                  SUMA_TRACT_SAUX *Saux = TDO_TSAUX(tdo);
                  SUMA_LH("No alternate selection for tracts");
                  break;}
               case MASK_type: {
                  SUMA_S_Err("What would that be for a mask?");
                  break; }
               case VO_type: {
                  SUMA_S_Err("Nothing done for volumes here yet");
                  break; }
               default:
                  SUMA_S_Errv("Not ready for %s\n", ADO_TNAME(ado));
                  break;
            }
            break;  
         case SE_ToggleCrossHair:
            /* expects nothing in EngineData */
            sv->ShowCrossHair = !sv->ShowCrossHair;
            XmToggleButtonSetState (sv->X->ViewMenu->mw[SW_ViewCrossHair], 
               sv->ShowCrossHair, NOPE);            
            break;
            
         case SE_SetCrossHair:
            /* Expects Cross Hair coordinates in fv3, and ADO in vp */
            if (EngineData->fv3_Dest != NextComCode ||
                EngineData->vp_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            }
            ado = (SUMA_ALL_DO *)EngineData->vp;
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: Setting cross hair at %f %f %f\n", 
                                    FuncName, EngineData->fv3[0], 
                                    EngineData->fv3[1],
                                    EngineData-> fv3[2]);
            sv->Ch->c[0]= EngineData->fv3[0]; 
            sv->Ch->c[1]= EngineData->fv3[1]; 
            sv->Ch->c[2]= EngineData->fv3[2];

            /* are we in VisX mode? */
            if (ado && ado->do_type == SO_type &&
                (SO = (SUMA_SurfaceObject *)ado) && SO->VisX.Applied) { 
                                                         /* undo the VisX */
               SUMA_Apply_VisX_Chain(EngineData->fv3, 1, SO->VisX.Xchain, 1);
               sv->Ch->c_noVisX[0]= EngineData->fv3[0]; 
               sv->Ch->c_noVisX[1]= EngineData->fv3[1]; 
               sv->Ch->c_noVisX[2]= EngineData->fv3[2];
            } else {
               sv->Ch->c_noVisX[0]=sv->Ch->c[0];
               sv->Ch->c_noVisX[1]=sv->Ch->c[1];
               sv->Ch->c_noVisX[2]=sv->Ch->c[2];
            }

            /* Make slices go to same location */
            SUMA_VO_set_slices_XYZ(NULL, sv->Ch->c_noVisX);
      
            /* Attempt to update crosshair corrdinates 
               in open surface controllers */
            SUMA_UpdateXhairField(sv); 
            break;
         
         case SE_BindCrossHair:
            /* expects adoID to bind cross hair to*/
            if (EngineData->iv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            }
            sv->Ch->adoID = EngineData->iv3[0];
            sv->Ch->datumID = EngineData->iv3[1];
            sv->Ch->secID = EngineData->iv3[2];
            SUMA_UpdateCrossHairNodeLabelField(sv);
            break;
         
         case SE_SetSOinFocus:
            /* expects surface ID in i */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            }
            if (sv->Focus_DO_ID != EngineData->i) {
               /* a new one, update */
               sv->Focus_DO_ID = EngineData->i;
               SUMA_UpdateViewerTitle(sv);
            }
            break;
            
         case SE_ToggleLockView:
            /* expects index of viewer in i to toggle its lock view */
            /* toggles the lock view button */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                  "Error %s: Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            }
            SUMAg_CF->ViewLocked[EngineData->i] = 
                        !SUMAg_CF->ViewLocked[EngineData->i];
            /* update button if needed*/
            if (EngineData->Src != SES_SumaWidget) {
               XmToggleButtonSetState (
                        SUMAg_CF->X->SumaCont->LockView_tbg[EngineData->i], 
                        SUMAg_CF->ViewLocked[EngineData->i], NOPE);
            }
            
            /* call function to update the AllLock button */
            SUMA_set_LockView_atb ();
            
            break;
         
         case SE_ToggleLockAllViews:
            /* expects nothing, toggles all locked view buttons */
            
            /* get the current value of the button */
            {
               SUMA_Boolean CurState;
               CurState = 
                  XmToggleButtonGetState (SUMAg_CF->X->SumaCont->LockAllView_tb);
               for (ii=0; ii< SUMA_MAX_SURF_VIEWERS; ++ii) { 
                  /* set all buttons accrodingly */
                  XmToggleButtonSetState (
                     SUMAg_CF->X->SumaCont->LockView_tbg[ii], CurState, NOPE);
                  SUMAg_CF->ViewLocked[ii] = CurState;
               }
            }
            break;
         
         case SE_ToggleLockAllCrossHair:
            /* expects nothing, toggles cross hair lock for all viewers */
            {
               char LockName[100];
               SUMA_LockEnum_LockType (SUMAg_CF->Locked[0], LockName);
               fprintf (SUMA_STDERR,
                        "%s: Switching Locktype from %s", FuncName, LockName);
               /* change the locking type of viewer 0 */
               SUMAg_CF->Locked[0] = (int)fmod(SUMAg_CF->Locked[0]+1, 
                                          SUMA_N_Lock_Types);
               SUMA_LockEnum_LockType (SUMAg_CF->Locked[0], LockName);
               fprintf (SUMA_STDERR," %s\n", LockName);
               /* update the widget*/
               SUMA_set_Lock_rb (SUMAg_CF->X->SumaCont->Lock_rbg, 0, 
                                 SUMAg_CF->Locked[0]);
               /* Change the locking type of all remaining viewers, 
                  including unopen ones */
               for (ii=1; ii< SUMA_MAX_SURF_VIEWERS; ++ii) {
                  SUMAg_CF->Locked[ii] = SUMAg_CF->Locked[0];                  
                  SUMA_set_Lock_rb (SUMAg_CF->X->SumaCont->Lock_rbg, ii, 
                                    SUMAg_CF->Locked[ii]);
               }

               /* now update the all lock keys */
               SUMA_set_Lock_arb (SUMAg_CF->X->SumaCont->Lock_rbg);

            }
            break;
         
         case SE_SetLockAllCrossHair:
            /* expects a Lock value in i , sets the lock of all viewers */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                  "Error %s: Data not destined correctly for %s (%d).\n",
                  FuncName, NextCom, NextComCode);
               break;
            }
            {
               
               /* Change the locking type of all remaining viewers, 
                     including unopen ones */
               for (ii=0; ii< SUMA_MAX_SURF_VIEWERS; ++ii) {
                  SUMAg_CF->Locked[ii] = EngineData->i;                  
                  SUMA_set_Lock_rb (SUMAg_CF->X->SumaCont->Lock_rbg, ii, 
                                    SUMAg_CF->Locked[ii]);
               }

               /* now update the all lock keys */
               SUMA_set_Lock_arb (SUMAg_CF->X->SumaCont->Lock_rbg);
            }
            break;
            
         case SE_LockCrossHair:
            /* expects nothing in EngineData */
            /* calls other viewers and determine if the cross hair 
               needs to be locked to the calling sv */

            /* check to see if other viewers need to share the fate */
            ii = SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv);
            if (ii < 0) {
               fprintf (SUMA_STDERR,
                        "Error %s: Failed to find index of sv.\n", FuncName);
               break;
            }
            if (SUMAg_CF->Locked[ii]) { /* This one's locked, find out which 
                                       other viewers are locked to this one */
               SUMA_LH("SV %d locked to something, maybe", ii);
               for (i=0; i < SUMAg_N_SVv; ++i) {
                  svi = &SUMAg_SVv[i];
                  if (i != ii) {
                     SUMA_LH("Tiz locked %d to sv %d", SUMAg_CF->Locked[i], i);
                     switch (SUMAg_CF->Locked[i]) { 
                        case SUMA_No_Lock:
                           if (LocalHead) 
                              fprintf (SUMA_STDERR, 
                                       "%s: No lock for viewer %d.\n", 
                                       FuncName, i);
                           break;
                        case SUMA_XYZ_Lock:
                           if (LocalHead) 
                              fprintf (SUMA_STDERR, 
                                       "%s: Try to XYZ lock viewer %d.\n", 
                                       FuncName, i);
                           /* just set the XYZ, and free the binding 
                              to the surfaces */
                           svi->Ch->c[0] = sv->Ch->c[0];
                           svi->Ch->c[1] = sv->Ch->c[1];
                           svi->Ch->c[2] = sv->Ch->c[2];
                           svi->Ch->datumID = -1;
                           svi->Ch->adoID = -1;
                           /* FORCE a redisplay */
                           svi->ResetGLStateVariables = YUP;
                           SUMA_handleRedisplay((XtPointer)svi->X->GLXAREA);                           
                           break;
                        case SUMA_I_Lock: 
                           Found = NOPE;
                           if (iDO_isSO(sv->Ch->adoID)) { /* Surface click */
                              SUMA_SurfaceObject *SO1 = NULL, *SO2 = NULL;

                              SUMA_LHv("Try to I lock viewer %d to node"
                                       " %d.\n", i, sv->Ch->datumID);

                              /* determine the list of shown surfaces */
                              N_SOlist = SUMA_RegisteredSOs(svi, SUMAg_DOv, 
                                                            SOlist);

                              /* first find the surface that the cross hair 
                                 is bound to */
                              if (sv->Ch->adoID < 0) {
                                 fprintf (SUMA_STDERR, 
                                          "%s: Cannot link from this viewer's "
                                          "cross hair. No bound surface.\n", 
                                          FuncName);
                                 break;
                              }
                              if (sv->Ch->datumID < 0) {
                                 SUMA_S_Errv("Cannot link from this viewer's" 
                                       " cross hair. No datumID for DO %s\n",
                                       iDO_label(sv->Ch->adoID));
                                 break;
                              }
                              
                              SO1 = (SUMA_SurfaceObject *)
                                             SUMAg_DOv[sv->Ch->adoID].OP;
                              Found = NOPE;
                              it = 0;
                              while (it < N_SOlist && !Found) {
                                 SO2 = (SUMA_SurfaceObject *)
                                             SUMAg_DOv[SOlist[it]].OP;
                                 if (SUMA_isRelated_SO(SO1, SO2, 2)) { 
                                       /* high level relationship is allowed,
                                          but with same-side enforcement.
                                          A level 3 returns a match based on
                                          the number of nodes only, but that
                                          can cause trouble between left and 
                                          right hemisphere surfaces where the 
                                          same node index does not refer 
                                          necessarily to the same anatomical 
                                          areas.   */
                                    svi->Ch->adoID = SOlist[it];
                                    if (sv->Ch->datumID > SO2->N_Node) {
                                       SUMA_S_Err("datumID is larger than"
                                             " N_Node. Setting datumID to 0.\n");
                                       svi->Ch->datumID = 0;
                                    }else{
                                       svi->Ch->datumID = sv->Ch->datumID;
                                    }
                                    /* set the XYZ */
                                    svi->Ch->c_noVisX[0] = 
                                               SO2->NodeList[SO2->NodeDim*
                                                               svi->Ch->datumID];
                                    svi->Ch->c_noVisX[1] = 
                                               SO2->NodeList[SO2->NodeDim*
                                                             svi->Ch->datumID+1];
                                    svi->Ch->c_noVisX[2] = 
                                               SO2->NodeList[SO2->NodeDim*
                                                             svi->Ch->datumID+2];
                                    svi->Ch->c[0]=svi->Ch->c_noVisX[0];
                                    svi->Ch->c[1]=svi->Ch->c_noVisX[1];
                                    svi->Ch->c[2]=svi->Ch->c_noVisX[2];
                                    if (SO2->VisX.Applied) { /* Apply the VisX */
                                       SUMA_Apply_VisX_Chain(svi->Ch->c, 1, 
                                                          SO2->VisX.Xchain, 0);
                                    }
                                    if (LocalHead)
                                       fprintf (SUMA_STDERR,
                                                "%s: new XYZ %f %f %f\n", 
                                                FuncName, 
                                                svi->Ch->c[0], svi->Ch->c[1], 
                                                svi->Ch->c[2]); 
                                    Found = YUP;
                                 }
                                 ++it;
                              }
                           } else if (iDO_isGLDO(sv->Ch->adoID)){
                              SUMA_GraphLinkDO *gldo1, *gldo2=NULL;
                              SUMA_DSET *dset=NULL;
                              char *variant=NULL;
                              int ido2;
                              float fv6[6];
                              SUMA_LHv("Try to I lock viewer %d to edge"
                                       " %d.\n", i, sv->Ch->datumID);

                              if (sv->Ch->adoID < 0) {
                                 fprintf (SUMA_STDERR, 
                                          "%s: Cannot link from this viewer's "
                                          "cross hair. No bound graph.\n", 
                                          FuncName);
                                 break;
                              }
                              if (sv->Ch->datumID < 0) {
                                 SUMA_LHv("No datumID for DO %s\n",
                                       iDO_label(sv->Ch->adoID));
                                 /* Don't exit here, because sometimes users
                                 select and entire set of edes, when they
                                 click on the nodes of a graph, you still
                                 want to refresh the other representations
                                 of that graph*/
                              }
                              gldo1 = (SUMA_GraphLinkDO *)
                                          SUMAg_DOv[sv->Ch->adoID].OP;
                              if (!(dset = SUMA_find_GLDO_Dset(gldo1))) {
                                 SUMA_S_Err("Seal Attack!");
                                 break;
                              }
                              Found = NOPE;
                              it = 0;
                              while (it < svi->N_DO && !Found) {
                                    ido2 = svi->RegistDO[it].dov_ind;
                                    if (iDO_isGLDO(ido2)) {
                                 gldo2 = (SUMA_GraphLinkDO *)SUMAg_DOv[ido2].OP;
                                 variant = iDO_variant(ido2);   
                                    } else {
                                 gldo2 = NULL;
                                 variant = NULL;
                                    }
                                 if ((SUMA_find_GLDO_Dset(gldo2) == dset)&&
                                     SUMA_IS_REAL_VARIANT(variant)) { 
                                    svi->Ch->adoID = ido2;
                                    svi->Ch->datumID = sv->Ch->datumID;

                                    /* set the XYZ */
                                    if (svi->Ch->datumID >= 0 &&
                                        (SUMA_GDSET_EdgeXYZ_eng(dset, 
                                                svi->Ch->datumID,
                                           iDO_variant(ido2), fv6))){
                                       svi->Ch->c[0] = (fv6[0]+fv6[3])/2.0;
                                       svi->Ch->c[1] = (fv6[1]+fv6[4])/2.0;
                                       svi->Ch->c[2] = (fv6[2]+fv6[5])/2.0;
                                       if (LocalHead)
                                          fprintf (SUMA_STDERR,
                                                   "%s: new XYZ %f %f %f\n", 
                                                   FuncName, 
                                                   svi->Ch->c[0], svi->Ch->c[1], 
                                                   svi->Ch->c[2]); 
                                       Found = YUP;
                                    } else {
                                       /* Still declare it found, even if coords
                                          are not set because datum index is < 0
                                          This is for updating when you select
                                          and edge's point/node */
                                       Found = YUP;
                                    }  
                                 }
                                 ++it;
                              }
                              
                           } else {
                              SUMA_LHv(
                                 "No linkage for clicks on objects like %s\n",
                                 iDO_label(sv->Ch->adoID));
                           }
                           if (!Found) {
                              if (LocalHead) 
                                 fprintf (SUMA_STDERR,
                                          "%s: No related DOs found in"
                                          " viewer, cross hair will not be"
                                          "touched .\n", FuncName);
                              break;
                           } else {
                              /* FORCE a redisplay */
                              svi->ResetGLStateVariables = YUP;
                              SUMA_handleRedisplay(
                                       (XtPointer)svi->X->GLXAREA);
                           }
                           break;
                        default:
                           fprintf(SUMA_STDERR,
                                   "Error %s: Lock type (%d) undefined.\n", 
                                   FuncName, SUMAg_CF->Locked[ii]);
                           break;
                     }
                  }
               }
            }else{
               /* not locked to anything */
               SUMA_LH("SV %d not locked to anything", ii);
            }
            break;
                        
         case SE_SetAfniCrossHair:
            /* expects a Do_icor flag in i  */
            if (EngineData->i_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                  "Error %s: Data not destined correctly for %s (%d).\n",
                  FuncName, NextCom, NextComCode);
               break;
            }
            if (  SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] && 
                  sv->LinkAfniCrossHair) {
               SUMA_LH("Sending cross hair nel: SUMA_crosshair_xyz") ;
               if (!(nel = SUMA_makeNI_CrossHair (sv))) {
                  SUMA_S_Err("SUMA_makeNI_CrossHair failed");
                  break;
               }
               if (EngineData->i) NI_set_attribute(nel,"Do_icor", "y");
               if ( (nn = NI_write_element( 
                        SUMAg_CF->ns_v[SUMA_AFNI_STREAM_INDEX] , 
                                 nel , NI_TEXT_MODE)) < 0) {
                  SUMA_S_Err("NI_write_element failed");   
               }
               NI_free_element(nel); nel = NULL;
            }
            
            if ( SUMAg_CF->Connected_v[SUMA_HALLO_SUMA_LINE] ) {
               SUMA_LH("Sending cross hair nel: SUMA_crosshair_xyz") ;
               if (!(nel = SUMA_makeNI_CrossHair (sv))) {
                  SUMA_S_Err("SUMA_makeNI_CrossHair failed");
                  break;
               }
               if ( (nn = NI_write_element( 
                        SUMAg_CF->ns_v[SUMA_HALLO_SUMA_LINE] , 
                                 nel , NI_TEXT_MODE)) < 0) {
                  SUMA_S_Err("NI_write_element failed");   
               }
               NI_free_element(nel); nel = NULL;
            }
            
            if ( SUMAg_CF->Connected_v[SUMA_INSTA_TRACT_LINE] ) {
               NI_group *ngr=NULL;
               if (!(ngr = SUMA_makeNI_InstaTract_Query (sv))) {
                  SUMA_LH("SUMA_makeNI_InstaTract_Query failed, or "
                          "nothing found");
                  break;
               }
               if ( (nn = NI_write_element( 
                        SUMAg_CF->ns_v[SUMA_INSTA_TRACT_LINE] , 
                                 ngr , NI_BINARY_MODE)) < 0) {
                  SUMA_S_Err("NI_write_element failed");   
               }
               if (LocalHead) SUMA_ShowNel(ngr);
               NI_free_element(ngr); ngr = NULL;
            }
            break;
         
         case SE_SetGICORnode:
            /* expects nothing in EngineData */
            /* sends the current node to Group Icor */
            if (SUMAg_CF->giset && SUMAg_CF->Connected_v[SUMA_GICORR_LINE] 
                && !SUMAg_CF->HoldClickCallbacks &&
                (SO = SUMA_SV_Focus_SO(sv))) {
               SUMA_LHv("Sending notice to GICOR, SO_ID: %d\n",
                              sv->Focus_DO_ID);
               if (SUMA_AFNI_gicor_setref(SO, SO->SelectedNode) < 0) {
                  SUMA_S_Err("Failed in SUMA_AFNI_gicor_setref");
               } 
            }

            break;
         
         case SE_SetLookAtNode:
            /* expects a center XYZ in EngineData->fv15[0 .. 2]
            expects a normal vector in EngineData->fv15[3 .. 5] */
            if (EngineData->fv15_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                  "Error %s: Data not destined correctly for %s (%d).\n",
                  FuncName, NextCom, NextComCode);
               break;
            } 
            
            { float CurrentDistance;
              float fm2_3[2][3]={ {0.0, 0.0, 0.0}, {0.0, 0.0, 0.0} }, *dir;
              
            /* modify the ViewFrom Value such that the viewing distance 
               remains the same */
            CurrentDistance = sqrt((sv->GVS[sv->StdView].ViewFrom[0] - 
                                    sv->GVS[sv->StdView].ViewCenter[0]) *
                                   (sv->GVS[sv->StdView].ViewFrom[0] - 
                                    sv->GVS[sv->StdView].ViewCenter[0]) +
                                   (sv->GVS[sv->StdView].ViewFrom[1] - 
                                    sv->GVS[sv->StdView].ViewCenter[1]) * 
                                   (sv->GVS[sv->StdView].ViewFrom[1] - 
                                    sv->GVS[sv->StdView].ViewCenter[1]) +
                                   (sv->GVS[sv->StdView].ViewFrom[2] - 
                                    sv->GVS[sv->StdView].ViewCenter[2]) * 
                                   (sv->GVS[sv->StdView].ViewFrom[2] - 
                                    sv->GVS[sv->StdView].ViewCenter[2]));
            
            /* set the ViewCenter Value to that of the node's XYZ*/
            sv->GVS[sv->StdView].ViewCenter[0] = EngineData->fv15[0];
            sv->GVS[sv->StdView].ViewCenter[1] = EngineData->fv15[1]; 
            sv->GVS[sv->StdView].ViewCenter[2] = EngineData->fv15[2];
            
            /* obtain the LookFrom point based on CurrentDistance and 
               the normal vector */
            dir = &(EngineData->fv15[3]);
            SUMA_POINT_AT_DISTANCE(dir, 
                  sv->GVS[sv->StdView].ViewCenter, CurrentDistance, fm2_3);
            
            fprintf(SUMA_STDOUT,"\nPoints: %f %f %f\n%f %f %f\n", 
               fm2_3[0][0], fm2_3[0][1], fm2_3[0][2], 
               fm2_3[1][0], fm2_3[1][1], fm2_3[1][2]);
            
            sv->GVS[sv->StdView].ViewFrom[0] = fm2_3[0][0]; 
            sv->GVS[sv->StdView].ViewFrom[1] = fm2_3[0][1]; 
            sv->GVS[sv->StdView].ViewFrom[2] = fm2_3[0][2]; 
                        
            gluLookAt ( sv->GVS[sv->StdView].ViewFrom[0], 
                        sv->GVS[sv->StdView].ViewFrom[1], 
                        sv->GVS[sv->StdView].ViewFrom[2], 
                        sv->GVS[sv->StdView].ViewCenter[0], 
                        sv->GVS[sv->StdView].ViewCenter[1], 
                        sv->GVS[sv->StdView].ViewCenter[2], 
                        sv->GVS[sv->StdView].ViewCamUp[0], 
                        sv->GVS[sv->StdView].ViewCamUp[1], 
                        sv->GVS[sv->StdView].ViewCamUp[2]);
            }
            
            break;
         case SE_SetLookFrom:
            /* expects a center XYZ in EngineData->fv3[0 .. 2] */
            if (EngineData->fv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s:\n"
                        " Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            } 
            /* set the LookFrom option */
            sv->GVS[sv->StdView].ViewFrom[0] = EngineData->fv3[0];
            sv->GVS[sv->StdView].ViewFrom[1] = EngineData->fv3[1]; 
            sv->GVS[sv->StdView].ViewFrom[2] = EngineData->fv3[2];
            gluLookAt ( sv->GVS[sv->StdView].ViewFrom[0], 
                        sv->GVS[sv->StdView].ViewFrom[1], 
                        sv->GVS[sv->StdView].ViewFrom[2], 
                        sv->GVS[sv->StdView].ViewCenter[0], 
                        sv->GVS[sv->StdView].ViewCenter[1], 
                        sv->GVS[sv->StdView].ViewCenter[2], 
                        sv->GVS[sv->StdView].ViewCamUp[0], 
                        sv->GVS[sv->StdView].ViewCamUp[1], 
                        sv->GVS[sv->StdView].ViewCamUp[2]);
            break;

         case SE_Redisplay_AllVisible:
            /* expects nothing in EngineData */
            /* post a redisplay to all visible viewers,
               Do the one where pointer is last */
            {
               int viewerorder[SUMA_MAX_SURF_VIEWERS], np = 0;
               /* set viewer order so that the one that 
               last had the pointer gets displayed last */
               for (ii=0; ii<SUMAg_N_SVv; ++ii) {
                  if (ii != SUMAg_CF->PointerLastInViewer) {
                     viewerorder[np] = ii; ++np;     
                  }
               }
               if (np < SUMAg_N_SVv) {
                  viewerorder[np] = SUMAg_CF->PointerLastInViewer;
                  ++np;
               }
               if (np != SUMAg_N_SVv) {
                  SUMA_S_Err("WTH?");
               }
               for (np=0; np<SUMAg_N_SVv; ++np) {
                  ii = viewerorder[np];
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,
                              "%s: Checking viewer %d.\n", FuncName, ii);
                  if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
                     /* you must check for both conditions because by default 
                     all viewers are initialized to isShaded = NOPE, even before 
                     they are ever opened */
                     if (LocalHead) 
                        fprintf (SUMA_STDERR,
                                 "%s: Redisplaying viewer %d.\n", FuncName, ii);
                     SUMAg_SVv[ii].ResetGLStateVariables = YUP;
                     SUMA_postRedisplay(SUMAg_SVv[ii].X->GLXAREA, NULL, NULL);
                  }
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
            if (LocalHead) 
               fprintf (SUMA_STDOUT,"%s: Redisplaying NOW ...", FuncName);
            sv->ResetGLStateVariables = YUP;
            SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
            if (LocalHead) fprintf (SUMA_STDOUT," Done\n");
            break;
            
         case SE_RedisplayNow_AllVisible:
            /* expects nothing in EngineData */
            /* causes  an immediate redisplay to all visible viewers 
               The viewer that had the pointer last gets 
               displayed last. This is similar in concept
               to SE_RedisplayNow_AllOtherVisible */
            {
               int viewerorder[SUMA_MAX_SURF_VIEWERS], np = 0;
               /* set viewer order so that the one that 
               last had the pointer gets displayed last */
               for (ii=0; ii<SUMAg_N_SVv; ++ii) {
                  if (ii != SUMAg_CF->PointerLastInViewer) {
                     viewerorder[np] = ii; ++np;     
                  }
               }
               if (np < SUMAg_N_SVv) {
                  viewerorder[np] = SUMAg_CF->PointerLastInViewer;
                  ++np;
               }
               if (np != SUMAg_N_SVv) {
                  SUMA_S_Err("WTH?");
               }
               for (np=0; np<SUMAg_N_SVv; ++np) {
                  ii = viewerorder[np];
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,
                              "%s: Checking viewer %d.\n", FuncName, ii);
                  if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
                     /* you must check for both conditions because by default 
                     all viewers are initialized to isShaded = NOPE, even before 
                     they are ever opened */
                     if (LocalHead) 
                        fprintf (SUMA_STDERR,
                                 "%s: Redisplaying viewer %d.\n", FuncName, ii);
                     SUMAg_SVv[ii].ResetGLStateVariables = YUP;
                     SUMA_handleRedisplay((XtPointer)SUMAg_SVv[ii].X->GLXAREA);
                     SUMA_LHv("Returned redisplaying viewer %d.\n", ii);
                  }
               }
            }
            break;
         
         case SE_RedisplayNow_AllOtherVisible:
            /* expects nothing in EngineData, expects sv in srcp*/
            /* causes an immediate redisplay to all visible viewers 
               other than sv*/
            for (ii=0; ii<SUMAg_N_SVv; ++ii) {
               if (LocalHead) 
                  fprintf (SUMA_STDERR,"%s: Checking viewer %d.\n", 
                           FuncName, ii);
               if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL && 
                     &(SUMAg_SVv[ii]) != sv) {
                  /* you must check for both conditions because by default 
                  all viewers are initialized to isShaded = NOPE, even before 
                  they are ever opened */
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,"%s: Redisplaying viewer %d.\n", 
                              FuncName, ii);
                  SUMAg_SVv[ii].ResetGLStateVariables = YUP;
                  SUMA_handleRedisplay((XtPointer)SUMAg_SVv[ii].X->GLXAREA);
               }
            }
            break;

         case SE_ResetOpenGLState:
            /* reset OPEN GL's state variables */
            /* expects the surface viewer pointer in vp */
            if (EngineData->vp_Dest != NextComCode) {
               SUMA_S_Err("Data not destined correctly for %s (%d).\n",
                          NextCom, NextComCode);
               break;
            } 
            SUMA_LH("Resetting OpenGL state variables.\n");
            
            /* No need to call SUMA_OpenGLStateReset, 
               that is now done in SUMA_display */
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
               fprintf (SUMA_STDERR,
                        "Error %s: Failed in SUMA_SetShownLocalRemixFlag.\n", 
                        FuncName);
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
               fprintf (SUMA_STDERR,
                        "Error %s: Failed in SUMA_SetShownLocalRemixFlag.\n", 
                        FuncName);
               break;
            }
            break;
                     
         case SE_Home:
            /* expects nothing in EngineData, needs sv */
            SUMA_SetGLHome(sv);
            break;
         
         case SE_Home_AllVisible:
            /* expects nothing in EngineData, needs no sv */
            {
               for (ii=0; ii<SUMAg_N_SVv; ++ii) {
                  SUMA_LHv("Checking viewer %d.\n", ii);
                  if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
                     /* you must check for both conditions because by default 
                     all viewers are initialized to isShaded = NOPE, 
                     even before they are ever opened */
                     SUMA_LHv("Home call viewer %d.\n", ii);
                     if (!list) {
                        SUMA_S_Err(
                           "Should not be inside SUMA_Engine: ZSS Feb 02 05.\n");
                        /* list = SUMA_CreateList();*/
                        break;
                     }
                     SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Home, 
                                                      SES_Suma, &SUMAg_SVv[ii]);
                  }
               }
            }
            break;
         
         case SE_FOVreset:
            /* expects nothing in EngineData */
            sv->FOV[sv->iState] = SUMA_sv_auto_fov(sv);   
                        /* reset the zooming */
            /* Now update the zoom compensation variable */
            if (sv->ZoomCompensate) {
               sv->ZoomCompensate = sv->FOV[sv->iState] / 
                                    SUMA_sv_auto_fov(sv);
               if (sv->ZoomCompensate > 1) sv->ZoomCompensate = 1.0; 
                  /* weird stuff at zc_fac higher that 1.5 */
               else if (sv->ZoomCompensate < 0.005) sv->ZoomCompensate = 0.005; 
            }

            break;
            
         case SE_SetNodeColor:
            /* expects a four-columned fm in EngineData->fm[0 .. N][0..3] 
            [Node Index] [R] [G] [B] RGB between 0 and 1*/
            if (EngineData->fm_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            }
            
            if ((SO = SUMA_SV_Focus_SO(sv))) {
               GLfloat *glar_ColorList;
               glar_ColorList = SUMA_GetColorList(sv, SO->idcode_str);
               if (!glar_ColorList) {
                  SUMA_S_Err("NULL color list array. Trouble.");
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
            sv->light0_position[0] *= -1;
            sv->light0_position[1] *= -1;
            sv->light0_position[2] *= -1;
            sv->lit_for *= -1;
            glLightfv(GL_LIGHT0, GL_POSITION, sv->light0_position);
            break;
         
         case SE_SetLight0Pos:
            /* expects light XYZ position in fv[3] */
            if (EngineData->fv3_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                  "Error %s: Data not destined correctly for %s (%d).\n",
                     FuncName, NextCom, NextComCode);
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
               SUMA_S_Errv("Data not destined correctly for %s (%d).\n",
                           NextCom, NextComCode);
               break;
            }
            if ((SO = SUMA_SV_Focus_SO(sv))) {
               SUMA_ISINBOX IB;
               
               ND = SO->NodeDim;
            
               SUMA_etime (&tt, 0);
               IB = SUMA_isinbox (SO->NodeList, SO->N_Node, 
                           &(EngineData->fv15[0]), &(EngineData->fv15[3]),  YUP);
               delta_t = SUMA_etime (&tt, 1);
               fprintf (SUMA_STDOUT,"Elapsed time for isinbox operation: %f\n", 
                                    delta_t);
               fprintf (SUMA_STDOUT,"\t%d nodes (out of %d) found in box\n",
                                    IB.nIsIn, SO->N_Node);
               
               if (IB.nIsIn) { /* found some, find the closest node */
                  /* locate the closest node and store it's id in EngineData*/
                  SUMA_MIN_LOC_VEC (IB.d, IB.nIsIn, ft, it);
                  
                  /* XYZ and normal of the closest to the center */
                  /* Color the nodes*/
                  fm = (float **)SUMA_allocate2D(IB.nIsIn, 4, sizeof(float));
                  if (fm == NULL) {
                     fprintf(SUMA_STDERR,
                             "Error %s: Could not allocate for fm.\n", FuncName);
                     break;
                  }
                  for (i=0; i < IB.nIsIn; ++i) {
                      /* id = ND * IB.IsIn[i]; */
                      /*fprintf (SUMA_STDOUT,"\t[%d] %f %f %f\n", IB.IsIn[i] ,\
                                   SO->NodeList[id], SO->NodeList[id+1], 
                                   SO->NodeList[id+2]);*/
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
                                                         SES_Suma, (void *)sv, 
                                                         NOPE,
                                                         SEI_Head, NULL)) {
                     fprintf(SUMA_STDERR,
                             "Error %s: Failed to register element\n", FuncName);
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
                     fprintf( SUMA_STDERR,
                              "Error %s: Failed to free IB\n", FuncName);
                  }
               } else { /* no node is close enough */
                  /* Do nothing yet */
                  fprintf (SUMA_STDOUT,
                           "\nNo nodes found inside the specified box.\n");
               }
            }
            break;

         case SE_GetNearestNode:
            /* lookfor nodes inside the search box */
            /* expects Node XYZ in EngineData->fv15[0..2]
            Box dimensions in EngineData->fv15[3..5] */
            if (EngineData->fv15_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            }
            if ((SO = SUMA_SV_Focus_SO(sv))){
               SUMA_ISINBOX IB;
               
               ND = SO->NodeDim;
               SUMA_etime (&tt, 0);
               IB = SUMA_isinbox (SO->NodeList, SO->N_Node, 
                                 &(EngineData->fv15[0]), &(EngineData->fv15[3]),
                                 YUP);
               delta_t = SUMA_etime (&tt, 1);
               fprintf (SUMA_STDOUT,
                        "Elapsed time for isinbox operation: %f\n", delta_t);
               fprintf (SUMA_STDOUT,
                        "\t%d nodes (out of %d) found in box\n",
                        IB.nIsIn, SO->N_Node);
               
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
                                                         SES_Suma, (void *)sv, 
                                                         NOPE, 
                                                         SEI_Head, NULL)) {
                     fprintf( SUMA_STDERR,
                              "Error %s: Failed to register element\n", 
                              FuncName);
                     break;
                  }
                  
                  /* get ridd of IB's vectors */
                  if (!SUMA_Free_IsInBox (&IB)) {
                     fprintf( SUMA_STDERR,
                              "Error %s: Failed to free IB\n", FuncName);
                  }
               } else { /* no node is close enough */
                  /* Do nothing yet */
               }
            }
            break;
            
         case SE_SetRotMatrix:
            /* expects a rotation matrix in fm, 4x4 */
            /* takes the rotation matrix 3x3 with 0 in 4th row and column 
            and 1.0 at 4,4 
            makes a quaternion from it and sets csv->currentQuat 
            and posts redisplay */
            if (EngineData->fm_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: "
                        "Data not destined correctly for %s (%d).\n",
                        FuncName, NextCom, NextComCode);
               break;
            }
            if (EngineData->N_rows != 4 || EngineData->N_cols != 4) {
               fprintf( SUMA_STDERR,
                        "Error %s: "
                        "fm must have 4 cols and 4 rows in SetRotMatrix\n", 
                        FuncName);
               break;
            }
            if (!SUMA_mattoquat (EngineData->fm, 
                                 sv->GVS[sv->StdView].currentQuat))
               {
                  fprintf( SUMA_STDERR,
                           "Error %s: Failed in SUMA_mattoquat\n", FuncName);
                  break;
               }
            break;
            
         case SE_SetObjectCont:
         case SE_SetSurfCont:
            /* expects a ngr and ADO in vp */
            if (  EngineData->ngr_Dest != NextComCode || 
                  EngineData->vp_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n"
                        "Have %d and %d\n",
                        FuncName, NextCom, NextComCode, 
                        EngineData->ngr_Dest, EngineData->vp_Dest);
               break;
            }
            ado = (SUMA_ALL_DO *)EngineData->vp;
            if (!(SurfCont = SUMA_ADO_Cont(ado))) {
               SUMA_S_Err("Need SurfCont for %s", ADO_LABEL(ado));
               break;
            }
            
            if (NI_get_attribute(EngineData->ngr, "switch_surf")) {
               int is;
               
               if (SUMA_iswordsame(ADO_GROUP(ado), sv->CurGroupName) != 1) {
                  SUMA_S_Errv(
                     "ADO %s is of group %s while viewer is of group %s.\n"
                     "Need to switch group before switch_surf\n", 
                     ADO_LABEL(ado), ADO_GROUP(ado), sv->CurGroupName);
                  break;
               }
               is = SUMA_WhichState(ADO_STATE(ado), sv, sv->CurGroupName);
               if (is < 0) {
                  SUMA_S_Errv("Surface %s of group %s, viewer in group %s\n"
                              "No surface of state %s found.\n",
                               ADO_LABEL(ado), ADO_GROUP(ado), sv->CurGroupName , 
                               ADO_STATE(ado));
                  break;
               }
               if (!SUMA_SwitchState ( SUMAg_DOv, SUMAg_N_DOv, sv, 
                                       is, sv->CurGroupName)) {
                  SUMA_S_Err("Failed to switch state"); break;
               } else {
                  sv->Focus_DO_ID = ADO_iDO(ado);
                  sv->NewGeom = YUP;   /* sv->ResetGLStateVariables 
                                          was not enough */
                  /* remove this attribute and call engine again 
                     for redisplay */
                  NI_set_attribute(EngineData->ngr, "switch_surf", NULL);
                  {
                     DList *llist = SUMA_CreateList();
                     SUMA_REGISTER_HEAD_COMMAND_NO_DATA(llist, SE_Redisplay, 
                                                         SES_SumaFromAny, sv);
                     if (!SUMA_Engine (&llist)) {
                        fprintf( stderr, 
                                 "Error %s: SUMA_Engine call failed.\n",
                                 FuncName);
                     }
                     /* update titles */
                     SUMA_UpdateViewerTitle(sv);
                  }
               }
            }
            
            if (NI_get_attribute(EngineData->ngr, "switch_dset")) {
               SUMA_OVERLAYS *ColPlane = SUMA_Fetch_OverlayPointer(
                  ado, 
                  NI_get_attribute(EngineData->ngr, "switch_dset"), 
                  &itmp);
               
               if (!ColPlane) {
                  SUMA_S_Errv("Failed to find dset %s\n", 
                              NI_get_attribute(EngineData->ngr, "switch_dset")); 
                  break;
               } else {
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,
                              "%s: Retrieved ColPlane named %s\n", 
                              FuncName, ColPlane->Name);
                  SUMA_InitializeColPlaneShell(ado, ColPlane);
                  SUMA_UpdateColPlaneShellAsNeeded(ado); 
                                 /* update other open ColPlaneShells */
                  /* If you're viewing one plane at a time, do a remix */
                  if (SurfCont->ShowCurForeOnly) 
                           SUMA_Remixedisplay(ado);
               }
            }
            
            if (NI_get_attribute(EngineData->ngr, "switch_cmap")) {
               /* find the colormap */
               if ((itmp = SUMA_Find_ColorMap(
                              NI_get_attribute( EngineData->ngr, "switch_cmap"), 
                                                SUMAg_CF->scm->CMv, 
                                                SUMAg_CF->scm->N_maps, -2 )) < 0)
               {
                  SUMA_S_Err("Failed to find color map"); break;
               } else {
                  SUMA_COLOR_MAP *ColMap = SUMAg_CF->scm->CMv[itmp];

                  #if 0
                  /* Set the menu button to the current choice */
                  if (!SUMA_SetCmapMenuChoice(ado, ColMap->Name)) {
                     SUMA_SL_Err("Failed in SUMA_SetCmapMenuChoice");
                  }

                  /* switch to the recently loaded  cmap */
                  if (!SUMA_SwitchColPlaneCmap(ado, ColMap)) {
                     SUMA_SL_Err("Failed in SUMA_SwitchColPlaneCmap");
                  }

                  /* update Lbl fields */
                  SUMA_UpdateNodeLblField(ado);
                  #else
                  SUMA_SwitchCmap(ado, ColMap, 1);
                  #endif
               }
            } 
           
            if (NI_get_attribute(EngineData->ngr, "switch_cmode")) {
               /* Set the menu button to the current choice */
               if (!SUMA_SetCmodeMenuChoice (ado, 
                        NI_get_attribute(EngineData->ngr, "switch_cmode"))) {
                  SUMA_SL_Err("Failed in SUMA_SetCmodeMenuChoice");
               }
               
               /* update Lbl fields */
               SUMA_UpdateNodeLblField(ado);
            }
            
            if (NI_get_attribute(EngineData->ngr, "load_cmap")) {
               SUMA_LoadCmapFile (NI_get_attribute(EngineData->ngr, "load_cmap"),                                   (void *)ado);
            }
            
            if (NI_get_attribute(EngineData->ngr, "I_sb")) {
               NI_GET_INT(EngineData->ngr, "I_sb", itmp);
               /* inefficient implementation, but avoids duplicate code... */
               if (!SUMA_SwitchColPlaneIntensity(ado, 
                        SurfCont->curColPlane, itmp, 1)) { 
                  SUMA_S_Err("Failed in I_sb"); break; 
               }
            }
            if ((cbuf = NI_get_attribute(EngineData->ngr, "Dsp"))) {
               
               /* inefficient implementation (causes redisplay, 
                  but avoids duplicating code... */
               if (!SUMA_SetDsetViewMode(ado, 
                     SUMA_ShowModeStr2ShowModeMenuItem(cbuf), 1)) { 
                  SUMA_S_Err("Failed in Dsp"); break; 
               }
            }
            
            /* Should you decide to fix/improve this block,
            You should call on function SUMA_SetRangeNew
            to set the values, that function will handle
            contralateral parallelization, but it does not
            seem to use the ShowMode variable so might want
            that added to SUMA_SetRangeNew_one ... 
            Same for B_range and B_scale       ZSS July 2012 */
            if (NI_get_attribute(EngineData->ngr, "I_range")) {
               char *stmp = NULL;

               NI_GET_STR_CP(EngineData->ngr, "I_range", stmp);
               if (!stmp) { 
                  SUMA_S_Err("Bad I_range"); 
               } else {
                  nn = SUMA_StringToNum(stmp, (void*)dv15, 3,2);
                  if (nn < 1 || nn > 2) {
                     SUMA_S_Err("Bad range string.");
                  }else {
                     if (nn == 1) { 
                        dv15[0] = -SUMA_ABS(dv15[0]); dv15[1] = -dv15[0]; }
                     else if (dv15[0] > dv15[1]) { 
                        ftmp = dv15[0]; dv15[0] = dv15[1]; dv15[1] = ftmp; }
                     /* have range, set it please */
                     SUMA_LHv("Have range of %f, %f\n", dv15[0], dv15[1]);  
                     SurfCont->curColPlane->OptScl->IntRange[0] = dv15[0];
                     SurfCont->curColPlane->OptScl->IntRange[1] = dv15[1];
                     SUMA_INSERT_CELL_VALUE(SurfCont->SetRangeTable, 1, 1, 
                                 SurfCont->curColPlane->OptScl->IntRange[0]);
                     SUMA_INSERT_CELL_VALUE(SurfCont->SetRangeTable, 1, 2, 
                                 SurfCont->curColPlane->OptScl->IntRange[1]);
                     if (SurfCont->curColPlane->ShowMode > 0 &&
                         SurfCont->curColPlane->ShowMode < 
                                             SW_SurfCont_DsetViewXXX ) {
                        if (!SUMA_ColorizePlane (SurfCont->curColPlane)) {
                           SUMA_SLP_Err("Failed to colorize plane.\n"); 
                        } else {
                           SUMA_Remixedisplay(ado);
                           SUMA_UpdateNodeValField(ado);
                           SUMA_UpdateNodeLblField(ado);
                        }
                     } 
                  }
                  SUMA_free(stmp); stmp = NULL;
               }
            }
            
            if (NI_get_attribute(EngineData->ngr, "Clst")) {
               char *stmp = NULL;
               int an;
               float reset;
               NI_GET_STR_CP(EngineData->ngr, "Clst", stmp);
               if (!stmp) { 
                  SUMA_S_Err("Bad Clst"); 
               } else {
                  nn = SUMA_StringToNum(stmp, (void*)dv15, 3,2);
                  if (nn != 2) {
                     SUMA_S_Err("Bad Clst string.");
                  } else {
                     /* set radius */
                     an = SUMA_SetClustValue(ado, SurfCont->curColPlane, 1, 1,
                          dv15[0], 0.0,
                          1, 0, &reset); /* don't redisp. yet */
                     if (an < 0) {
                        SUMA_S_Err("An error occurred setting radius");
                     } else {
                        /* set area */
                        an = SUMA_SetClustValue(ado, SurfCont->curColPlane, 1, 2,
                             dv15[1], 0.0,
                             1, 1, &reset); /* Now redisp. */
                        if (an < 0) {
                           SUMA_S_Err("An error occurred setting area");
                        }
                     }
                  }
               }
            }
            
            if (NI_get_attribute(EngineData->ngr, "UseClst")) {
               int tog = 0;
               if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, "UseClst", "y")) {
                  if (!SurfCont->curColPlane->OptScl->Clusterize) tog = 1;
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, "UseClst", "n")){
                  if (SurfCont->curColPlane->OptScl->Clusterize) tog = 1; 
               } else { 
                  SUMA_S_Errv("Bad value of %s for UseClst. Nothing done.\n", 
                              NI_get_attribute(EngineData->ngr, "UseClst"));
               } 
               if (tog) SUMA_SetClustTableTit(ado, SurfCont->curColPlane,
                                              1, 0, Button1);
            }
            
            if (NI_get_attribute(EngineData->ngr, "shw_0")) {
               if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, "shw_0", "y"))
                  SurfCont->curColPlane->OptScl->MaskZero = NOPE; 
               else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, "shw_0", "n"))
                  SurfCont->curColPlane->OptScl->MaskZero = YUP;
               else { 
                  SUMA_S_Errv("Bad value of %s for shw_0, setting to 'y'\n", 
                              NI_get_attribute(EngineData->ngr, "shw_0"));
                  SurfCont->curColPlane->OptScl->MaskZero = NOPE;
               } 
               XmToggleButtonSetState ( SurfCont->ShowZero_tb,
                              SurfCont->curColPlane->OptScl->MaskZero, YUP);
            }
            
            if (NI_get_attribute(EngineData->ngr, "B_sb")) {
               NI_GET_INT(EngineData->ngr, "B_sb", itmp);
               /* inefficient implementation, but avoids duplicate code... */
               if (!SUMA_SwitchColPlaneBrightness(ado, 
                        SurfCont->curColPlane, itmp, 1)) { 
                  SUMA_S_Err("Failed in T_sb"); break; 
               }
            }
            
            /* See comment for "I_range" above before making further
               changes here */
            if (NI_get_attribute(EngineData->ngr, "B_range")) {
               char *stmp = NULL;
               NI_GET_STR_CP(EngineData->ngr, "B_range", stmp);
               if (!stmp) { 
                  SUMA_S_Err("Bad B_range"); 
               } else {
                  nn = SUMA_StringToNum(stmp, (void*)dv15, 3,2);
                  if (nn < 1 || nn > 2) {
                     SUMA_S_Err("Bad range string.");
                  }else {
                     if (nn == 1) { 
                        dv15[0] = -SUMA_ABS(dv15[0]); dv15[1] = -dv15[0]; }
                     else if (dv15[0] > dv15[1]) { 
                        ftmp = dv15[0]; dv15[0] = dv15[1]; dv15[1] = ftmp; }
                     /* have range, set it please */
                     SUMA_LHv("Have range of %f, %f\n", dv15[0], dv15[1]);  
                     SurfCont->curColPlane->OptScl->BrightRange[0] = dv15[0];
                     SurfCont->curColPlane->OptScl->BrightRange[1] = dv15[1];
                     SUMA_INSERT_CELL_VALUE(SurfCont->SetRangeTable, 2, 1, 
                              SurfCont->curColPlane->OptScl->BrightRange[0]);
                     SUMA_INSERT_CELL_VALUE(SurfCont->SetRangeTable, 2, 2, 
                              SurfCont->curColPlane->OptScl->BrightRange[1]);
                     if (SurfCont->curColPlane->ShowMode > 0 &&
                         SurfCont->curColPlane->ShowMode  < 
                                             SW_SurfCont_DsetViewXXX) {
                        if (!SUMA_ColorizePlane (SurfCont->curColPlane)) {
                           SUMA_SLP_Err("Failed to colorize plane.\n"); 
                        } else {
                           SUMA_Remixedisplay(ado);
                           SUMA_UpdateNodeValField(ado);
                           SUMA_UpdateNodeLblField(ado);
                        }
                     } 
                  }
                  SUMA_free(stmp); stmp = NULL;
               }
            }
            /* See comment for "I_range" above before making further
               changes here */
            if (NI_get_attribute(EngineData->ngr, "B_scale")) {
               char *stmp = NULL;
               NI_GET_STR_CP(EngineData->ngr, "B_scale", stmp);
               if (!stmp) { 
                  SUMA_S_Err("Bad B_scale"); 
               } else {
                  nn = SUMA_StringToNum(stmp, (void*)dv15, 3,2);
                  if (nn != 2) {
                     SUMA_S_Err("Bad scale string.");
                  }else {
                     if (nn == 1) { 
                        dv15[0] = -SUMA_ABS(dv15[0]); dv15[1] = -dv15[0]; }
                     else if (dv15[0] > dv15[1]) { 
                        ftmp = dv15[0]; dv15[0] = dv15[1]; dv15[1] = ftmp; }
                     /* have range, set it please */
                     SUMA_LHv("Have scale range of %f, %f\n",dv15[0], dv15[1]);  
                     SurfCont->curColPlane->OptScl->BrightMap[0] = dv15[0];
                     SurfCont->curColPlane->OptScl->BrightMap[1] = dv15[1];
                     SUMA_INSERT_CELL_VALUE(SurfCont->SetRangeTable, 3, 1, 
                              SurfCont->curColPlane->OptScl->BrightMap[0]);
                     SUMA_INSERT_CELL_VALUE(SurfCont->SetRangeTable, 3, 2, 
                              SurfCont->curColPlane->OptScl->BrightMap[1]);
                     if (SurfCont->curColPlane->ShowMode > 0 &&
                         SurfCont->curColPlane->ShowMode < 
                                             SW_SurfCont_DsetViewXXX) {
                        if (!SUMA_ColorizePlane (SurfCont->curColPlane)) {
                           SUMA_SLP_Err("Failed to colorize plane.\n"); 
                        } else {
                           SUMA_Remixedisplay(ado);
                           SUMA_UpdateNodeValField(ado);
                           SUMA_UpdateNodeLblField(ado);
                        }
                     } 
                  }
                  SUMA_free(stmp); stmp = NULL;
               }
            }
            
            if (NI_get_attribute(EngineData->ngr, "T_sb")) {
               NI_GET_INT(EngineData->ngr, "T_sb", itmp);
               /* inefficient implementation, but avoids duplicate code... */
               if (!SUMA_SwitchColPlaneThreshold(ado, 
                            SurfCont->curColPlane, itmp, 1)) { 
                  SUMA_S_Err("Failed in T_sb"); break; 
               }
            }
            if (NI_get_attribute(EngineData->ngr, "T_val")) {
               NI_GET_FLOAT(EngineData->ngr, "T_val", ftmp);
               SUMA_MODIFY_CELL_VALUE(SurfCont->SetThrScaleTable, 0,0, ftmp);
               /* inefficient implementation, but avoids duplicate code... */
               SUMA_cb_SetScaleThr(EngineData->vp); 
            }
            if (NI_get_attribute(EngineData->ngr, "Dim")) {
               char stmp[50];
               NI_GET_FLOAT(EngineData->ngr, "Dim", ftmp);
               SUMA_ColPlane_NewDimFact(ado, NULL, ftmp, 0);
            }
            if (NI_get_attribute(EngineData->ngr, "Alf")) {
               char stmp[50];
               NI_GET_FLOAT(EngineData->ngr, "Alf", ftmp);
               SUMA_ColPlane_NewAlphaThresh(ado, NULL, ftmp, 0);
            }
            if (NI_get_attribute(EngineData->ngr, "Opa")) {
               char stmp[50];
               NI_GET_FLOAT(EngineData->ngr, "Opa", ftmp);
               
               #if 0 /* should be handled in new call format below */
               if (SurfCont && SurfCont->ColPlaneOpacity) {
                  SurfCont->ColPlaneOpacity->value = ftmp;
                  sprintf(stmp,"%.1f", ftmp);
                  SUMA_SET_TEXT_FIELD( SurfCont->ColPlaneOpacity->textfield, 
                                       stmp); 
               }
               #endif
               /* inefficient implementation, but avoids duplicate code... */
               SUMA_ColPlane_NewOpacity(ado, NULL, ftmp, 0);
            }
            if (NI_get_attribute(EngineData->ngr, "view_dset")) {
               if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, "view_dset", "y")) {
                  if (SurfCont->curColPlane->ShowMode < 0)
                     SurfCont->curColPlane->ShowMode = 
                        -SurfCont->curColPlane->ShowMode;
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                                "view_dset", "n")) {
                  if (SurfCont->curColPlane->ShowMode > 0)
                     SurfCont->curColPlane->ShowMode = 
                                    -SurfCont->curColPlane->ShowMode;
               } else { 
                  SUMA_S_Errv("Bad value of %s for view_dset, setting to 'y'\n", 
                              NI_get_attribute(EngineData->ngr, "view_dset"));
                  SurfCont->curColPlane->ShowMode = SW_SurfCont_DsetViewCol;
               }
               SUMA_Set_Menu_Widget( SurfCont->DsetViewModeMenu,
                              SUMA_ShowMode2ShowModeMenuItem(
                                 SurfCont->curColPlane->ShowMode));
            }
            if (NI_get_attribute(EngineData->ngr, "do_draw_mask")) {
               if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "do_draw_mask", "All")) {
                  sv->DO_DrawMask = SDODM_All;
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "do_draw_mask", "CrossHair") ||
                          NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "do_draw_mask", "n0CrossHair") ) {
                  sv->DO_DrawMask = SDODM_n0CrossHair;
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "do_draw_mask", "n1CrossHair") ) {
                  sv->DO_DrawMask = SDODM_n1CrossHair;
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "do_draw_mask", "n2CrossHair") ) {
                  sv->DO_DrawMask = SDODM_n2CrossHair;
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "do_draw_mask", "n3CrossHair") ) {
                  sv->DO_DrawMask = SDODM_n3CrossHair;
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "do_draw_mask", "Hide")) {
                  sv->DO_DrawMask = SDODM_Hide;
               } 
               /* redisplay */
               SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
            }
            if (NI_get_attribute(EngineData->ngr, "view_surf")) {
               switch (ado->do_type) {
                  case SO_type:
                     SO = (SUMA_SurfaceObject *)ado;
               if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, "view_surf", "y")) {
                  SO->Show = YUP;
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr,"view_surf","n")){
                  SO->Show = NOPE;
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "view_surf", "Viewer")) {
                  SUMA_SET_SO_POLYMODE(SO,SRM_ViewerDefault);
                  SUMA_Set_Menu_Widget( SurfCont->RenderModeMenu,
                         SUMA_RenderMode2RenderModeMenuItem(SO->PolyMode+1));
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "view_surf", "Fill")) {
                  SUMA_SET_SO_POLYMODE(SO,SRM_Fill);
                  SUMA_Set_Menu_Widget( SurfCont->RenderModeMenu,
                         SUMA_RenderMode2RenderModeMenuItem(SO->PolyMode+1));
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "view_surf", "Line")) {
                  SUMA_SET_SO_POLYMODE( SO, SRM_Line );
                  SUMA_Set_Menu_Widget( SurfCont->RenderModeMenu,
                         SUMA_RenderMode2RenderModeMenuItem(SO->PolyMode+1));
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "view_surf", "Points")) {
                  SUMA_SET_SO_POLYMODE(SO,SRM_Points);
                  SUMA_Set_Menu_Widget( SurfCont->RenderModeMenu,
                         SUMA_RenderMode2RenderModeMenuItem(SO->PolyMode+1));
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                          "view_surf", "Hide")) {
                  SUMA_SET_SO_POLYMODE(SO,SRM_Hide);
                  SUMA_Set_Menu_Widget( SurfCont->RenderModeMenu,
                         SUMA_RenderMode2RenderModeMenuItem(SO->PolyMode+1));
               } else { 
                  SUMA_S_Errv("Bad value of %s for view_surf, setting to 'y'\n", 
                              NI_get_attribute(EngineData->ngr, "view_surf"));
                  SO->Show = YUP; SUMA_SET_SO_POLYMODE(SO,SRM_Fill);
               }
               /* redisplay */
               SUMA_SiSi_I_Insist();   /* did not think that was necessary...
                                          But DriveSuma's -view_surf failed
                                          to redisplay properly unless you
                                          called the command twice or
                                          move the cursor into the GLXAREA.
                                          This line appears to fix the 
                                          problem... */
               SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
                     break;
                  default:
                     SUMA_S_Err("Not ready with view_surf for type %s",
                              ADO_TNAME(ado));
                     break;
               }
            }
            if (NI_get_attribute(EngineData->ngr, "trans_surf")) {
               switch (ado->do_type) {
                  case SO_type:
                     SO = (SUMA_SurfaceObject *)ado;
                     if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                                "trans_surf", "Viewer")) {
                        SUMA_Set_ADO_TransMode(ado,STM_ViewerDefault, 
                                               0, 1);
                     } else {
                        int N=0;
                        NI_GET_INT(EngineData->ngr, "trans_surf", N);
                        if (N < 0 || N > 16) {
                           SUMA_S_Errv("Bad value for trans_surf of %s\n",
                              NI_get_attribute(EngineData->ngr, "trans_surf"));
                        } else {
                           SUMA_Set_ADO_TransMode(ado,(N+STM_0), 0, 1);
                        }
                     }
                     /* redisplay */
                     SUMA_SiSi_I_Insist(); /* did not think that was necessary...
                                                But DriveSuma's -view_surf failed
                                                to redisplay properly unless you
                                                called the command twice or
                                                move the cursor into the GLXAREA.
                                                This line appears to fix the 
                                                problem... */
                     SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
                     break;
                     #if 0 /* UNTESTED, don't uncomment until you test*/
                  case VolumeObject_type:{
                     SUMA_VolumeObject *VO = (SUMA_VolumeObject *)ado;
                     SUMA_VOL_SAUX *VSaux;
                     if (!(VSaux = ADO_VSaux(ado))) {
                        SUMA_S_Warn("No VSaux"); break;
                     }
                     if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                                "trans_surf", "Viewer")) {
                        SUMA_Set_ADO_TransMode(ado,SATM_ViewerDefault, 
                                               0, 1);
                     } else {
                        int N=0;
                        NI_GET_INT(EngineData->ngr, "trans_surf", N);
                        if (N < 0 || N > 16) {
                           SUMA_S_Errv("Bad value for trans_surf of %s\n",
                              NI_get_attribute(EngineData->ngr, "trans_surf"));
                        } else {
                           SUMA_Set_ADO_TransMode(ado,(N+SATM_0), 0, 1);
                        }
                     }
                     /* redisplay */
                     SUMA_SiSi_I_Insist(); /* did not think that was necessary...
                                                But DriveSuma's -view_surf failed
                                                to redisplay properly unless you
                                                called the command twice or
                                                move the cursor into the GLXAREA.
                                                This line appears to fix the 
                                                problem... */
                     SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
                     break; }
                     #endif
                  default:
                     SUMA_S_Err("Not ready with trans_surf for type %s",
                              ADO_TNAME(ado));
                     break;
               }
            }
            
            if (NI_get_attribute(EngineData->ngr, "1_only")) {
               if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, "1_only", "y")) {
                  itmp = YUP;
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, "1_only", "n")) {
                  itmp = NOPE;
               } else {
                  SUMA_S_Errv("Bad value of %s for 1_only, setting to 'y'\n", 
                              NI_get_attribute(EngineData->ngr, "1_only"));
                  itmp = YUP;
               }
               if (!SUMA_ColPlaneShowOneFore_Set(ado, itmp, 0)) {
                  SUMA_S_Err("Failed to set one only");
                  break;  
               }
            }
            
            if (NI_get_attribute(EngineData->ngr, "View_Surf_Cont")) {
               if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                        "View_Surf_Cont", "y")) {
                  if (!SUMA_viewSurfaceCont(NULL, ado, sv)) {
                     SUMA_S_Err("Failed open surfcont");
                     break;  
                  }
               } else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                               "View_Surf_Cont", "n")) {
                  SUMA_LH("Closing surface controller");
                  SUMA_cb_closeSurfaceCont(NULL, (XtPointer)ado, NULL);
               }
            }
            
            if (NI_get_attribute(EngineData->ngr,"Masks")) {
               SUMA_cb_Mask(NULL, ado, sv);
            }
            
            if (NI_get_attribute(EngineData->ngr,"2xMasks")) {
               SUMA_cb_Mask(NULL, ado, sv);
               SUMA_cb_Mask(NULL, ado, sv);
            }
            
            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Surf_Cont_Help"))) {
               if (!SUMA_WriteCont_Help(SO_type, TXT, cbuf)) {
                  SUMA_S_Err("Failed to write SurfCont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Snap_Surf_Cont_Widgets"))) {
               if (!SUMA_Snap_AllCont(SO_type, cbuf)) {
                  SUMA_S_Err("Failed to write SurfCont widgets to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Surf_Cont_Sphinx_Help"))) {
               if (!SUMA_WriteCont_Help(SO_type, SPX, cbuf)) {
                  SUMA_S_Err("Failed to write SurfCont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Suma_Cont_Help"))) {
               if (!SUMA_WriteCont_Help(not_DO_type, TXT, cbuf)) {
                  SUMA_S_Err("Failed to write SumaCont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Snap_Suma_Cont_Widgets"))) {
               if (!SUMA_Snap_AllCont(not_DO_type, cbuf)) {
                  SUMA_S_Err("Failed to write SumaCont widgets to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Suma_Cont_Sphinx_Help"))) {
               if (!SUMA_WriteCont_Help(not_DO_type, SPX, cbuf)) {
                  SUMA_S_Err("Failed to write SumaCont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Tract_Cont_Help"))) {
               if (!SUMA_WriteCont_Help(TRACT_type, TXT, cbuf)) {
                  SUMA_S_Err("Failed to write TractCont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Snap_Tract_Cont_Widgets"))) {
               if (!SUMA_Snap_AllCont(TRACT_type, cbuf)) {
                  SUMA_S_Err("Failed to write TractCont widgets to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Tract_Cont_Sphinx_Help"))) {
               if (!SUMA_WriteCont_Help(TRACT_type, SPX, cbuf)) {
                  SUMA_S_Err("Failed to write TractCont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Mask_Cont_Help"))) {
               if (!SUMA_WriteCont_Help(MASK_type, TXT, cbuf)) {
                  SUMA_S_Err("Failed to write MaskCont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Snap_Mask_Cont_Widgets"))) {
               if (!SUMA_Snap_AllCont(MASK_type, cbuf)) {
                  SUMA_S_Err("Failed to write MaskCont widgets to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Mask_Cont_Sphinx_Help"))) {
               if (!SUMA_WriteCont_Help(MASK_type, SPX, cbuf)) {
                  SUMA_S_Err("Failed to write MaskCont help to %s", cbuf);
               }
            }

            
            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Vol_Cont_Help"))) {
               if (!SUMA_WriteCont_Help(VO_type, TXT, cbuf)) {
                  SUMA_S_Err("Failed to write TractCont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Snap_Vol_Cont_Widgets"))) {
               if (!SUMA_Snap_AllCont(VO_type, cbuf)) {
                  SUMA_S_Err("Failed to write VolCont widgets to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Vol_Cont_Sphinx_Help"))) {
               if (!SUMA_WriteCont_Help(VO_type, SPX, cbuf)) {
                  SUMA_S_Err("Failed to write TractCont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Graph_Cont_Help"))) {
               if (!SUMA_WriteCont_Help(GRAPH_LINK_type, TXT, cbuf)) {
                  SUMA_S_Err("Failed to write GraphCont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Snap_Graph_Cont_Widgets"))) {
               if (!SUMA_Snap_AllCont(GRAPH_LINK_type, cbuf)) {
                  SUMA_S_Err("Failed to write GraphCont widgets to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Graph_Cont_Sphinx_Help"))) {
               if (!SUMA_WriteCont_Help(GRAPH_LINK_type, SPX, cbuf)) {
                  SUMA_S_Err("Failed to write GraphCont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_ROI_Cont_Help"))) {
               if (!SUMA_WriteCont_Help(ROIdO_type, TXT, cbuf)) {
                  SUMA_S_Err("Failed to write ROICont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Snap_ROI_Cont_Widgets"))) {
               if (!SUMA_Snap_AllCont(ROIdO_type, cbuf)) {
                  SUMA_S_Err("Failed to write ROICont widgets to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_ROI_Cont_Sphinx_Help"))) {
               if (!SUMA_WriteCont_Help(ROIdO_type, SPX, cbuf)) {
                  SUMA_S_Err("Failed to write ROICont help to %s", cbuf);
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Mouse_Keyb_Help"))) {
               FILE *fout = fopen(cbuf,"w");
               if (!fout) {
                  SUMA_S_Err("Failed to open %s for writing", cbuf);
               } else {
                  SUMA_help_message(fout, TXT);
                  fclose(fout); fout = NULL;
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                          "Write_Mouse_Keyb_Sphinx_Help"))) {
               FILE *fout = fopen(cbuf,"w");
               if (!fout) {
                  SUMA_S_Err("Failed to open %s for writing", cbuf);
               } else {
                  SUMA_help_message(fout, SPX);
                  fclose(fout); fout = NULL;
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                         "Write_Mouse_Cmap_Keyb_Help"))) {
               FILE *fout = fopen(cbuf,"w");
               if (!fout) {
                  SUMA_S_Err("Failed to open %s for writing", cbuf);
               } else {
                  SUMA_cmap_help_message(fout, TXT);
                  fclose(fout); fout = NULL;
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                         "Write_Mouse_Cmap_Keyb_Sphinx_Help"))) {
               FILE *fout = fopen(cbuf,"w");
               if (!fout) {
                  SUMA_S_Err("Failed to open %s for writing", cbuf);
               } else {
                  SUMA_cmap_help_message(fout, SPX);
                  fclose(fout); fout = NULL;
               }
            }

            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                         "Delete_All_Masks"))) {
               SUMA_DeleteAllMasks(NULL, NULL, 0);
            }
            
            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                         "Load_Masks"))) {
               SUMA_LoadMultiMasks(cbuf, NULL);
            }
            
            if ((cbuf = NI_get_attribute(EngineData->ngr, 
                                         "Save_Masks"))) {
               SUMA_SaveMultiMasks(cbuf, NULL);
            }
            
            break;
 
         case SE_SetViewerCont:
            /* expects a ngr and ADO in vp */
            if (  EngineData->ngr_Dest != NextComCode || 
                  EngineData->vp_Dest != NextComCode) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n"
                        "Have %d and %d\n",
                        FuncName, NextCom, NextComCode, 
                        EngineData->ngr_Dest, EngineData->vp_Dest);
               break;
            }
            ado = (SUMA_ALL_DO *)EngineData->vp; 
            if (NI_get_attribute(EngineData->ngr, "inout_notify")) {
               if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, "inout_notify", "y"))
                  SUMA_setIO_notify(1); 
               else if (NI_IS_STR_ATTR_EQUAL(EngineData->ngr, 
                                             "inout_notify", "n"))
                  SUMA_setIO_notify(0);
               else { 
                  SUMA_S_Errv("Bad value of %s for inout_notify, "
                              "setting to 'n'\n", 
                              NI_get_attribute(EngineData->ngr, "inout_notify"));
                  SUMA_setIO_notify(0);
               } 

            }

            if (NI_get_attribute(EngineData->ngr, "bkg_col")) {
               char *stmp = NULL;
               NI_GET_STR_CP(EngineData->ngr, "bkg_col", stmp);
               if (!stmp) { 
                  SUMA_S_Err("Bad bkg_col"); 
               } else {
                  nn = SUMA_StringToNum(stmp, (void*)dv15, 4,4);
                  if (nn != 4) {
                     SUMA_S_Err("Bad bkg_col string.");
                  } else {
                     /* have bkg_col, set it please */
                     SUMA_LHv("Have bkg_col of %f, %f, %f, %f\n", 
                              dv15[0], dv15[1], dv15[2], dv15[3]);  
                     sv->clear_color[0] = dv15[0];
                     sv->clear_color[1] = dv15[1];
                     sv->clear_color[2] = dv15[2];
                     sv->clear_color[3] = dv15[3];
                     {
                        DList *llist = SUMA_CreateList();
                        SUMA_REGISTER_HEAD_COMMAND_NO_DATA(llist, SE_Redisplay, 
                                                            SES_SumaFromAny, sv);
                        if (!SUMA_Engine (&llist)) {
                           fprintf( stderr, 
                                    "Error %s: SUMA_Engine call failed.\n",
                                    FuncName);
                        }
                     }
                  }
                  SUMA_free(stmp); stmp = NULL;
               }
            }
            /* autorecord prefix? */
            if (NI_get_attribute(EngineData->ngr, "autorecord")) {
               if (SUMAg_CF->autorecord) SUMA_free(SUMAg_CF->autorecord);
               SUMAg_CF->autorecord = SUMA_SetAutoRecord(
                  NI_get_attribute(EngineData->ngr, "autorecord"));
            }
        
            /* search for the keys */
            if (NI_get_attribute(EngineData->ngr,"N_Key")) {
               char *stmp=NULL, nc, *vbuf=NULL, *strgval=NULL;
               int k, rep,  N_rep = 1, redisp=0;
               float pauz=0.0, delta_t=0.0;
               struct timeval tt;
               DList *llist = NULL;
               LocalHead = NOPE;
               NI_GET_INT(EngineData->ngr,"N_Key", itmp);
               for (ii=0; ii<itmp; ++ii) {
                  sprintf(tmpstr, "Key_%d", ii);
                  NI_GET_STR_CP(EngineData->ngr,tmpstr, stmp);
                  sprintf(tmpstr, "Key_rep_%d", ii);
                  NI_GET_INT(EngineData->ngr,tmpstr, N_rep);
                  sprintf(tmpstr, "Key_pause_%d", ii);
                  NI_GET_FLOAT(EngineData->ngr,tmpstr, pauz);
                  sprintf(tmpstr, "Key_redis_%d", ii);
                  NI_GET_INT(EngineData->ngr,tmpstr, redisp);
                  sprintf(tmpstr, "Key_strval_%d", ii);
                  strgval = NI_get_attribute(EngineData->ngr, tmpstr);
                  SUMA_LHv("Rep=%d, Pause=%f, Redis=%d\n, Strgval=%s", 
                           N_rep, pauz, redisp, CHECK_NULL_STR(strgval));
                  if (stmp && (nc = strlen(stmp))) {
                     k = SUMA_KeyPress(stmp, NULL);
                     for (rep=0; rep<N_rep; ++rep) {
                        SUMA_LHv(".............rep %d\n", rep);
                        SUMA_etime(&tt, 0);
                        switch (k) {
                           case XK_bracketleft:
                              if (!SUMA_bracketleft_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_bracketright:
                              if (!SUMA_bracketright_Key(sv, stmp,"drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break; 
                           case XK_a:
                           case XK_A:
                              if (!SUMA_A_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break; 
                           case XK_b:
                           case XK_B:
                              if (!SUMA_B_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break; 
                           case XK_d:
                           case XK_D:
                              if (!SUMA_D_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break; 
                           case XK_g:
                           case XK_G:
                              if (!SUMA_G_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break; 
                           case XK_j:
                           case XK_J:
                              if (!SUMA_J_Key(sv, stmp, "drivesuma", strgval)) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break; 
                           case XK_l:
                           case XK_L:
                              if (!SUMA_L_Key(sv, stmp, "drivesuma", strgval)) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;  
                           case XK_m:
                           case XK_M:
                              if (!SUMA_M_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_n:
                           case XK_N:
                              if (!SUMA_N_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;   
                           case XK_o:
                           case XK_O:
                              if (!SUMA_O_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_p:
                           case XK_P:
                              if (!SUMA_P_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_r:
                           case XK_R:
                              if (!SUMA_R_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_t:
                           case XK_T:
                              if (!SUMA_T_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_u:
                           case XK_U:
                              if (!SUMA_U_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_w:
                           case XK_W:
                              if (!SUMA_W_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_z:
                           case XK_Z:
                              if (!SUMA_Z_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_Up:
                              if (!SUMA_Up_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_Down:
                              if (!SUMA_Down_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_Left:
                              if (!SUMA_Left_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_Right:
                              if (!SUMA_Right_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_space:
                              if (!SUMA_space_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_period:
                              if (!SUMA_period_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_comma:
                              if (!SUMA_comma_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F1:
                              if (!SUMA_F1_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F2:
                              if (!SUMA_F2_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F3:
                              if (!SUMA_F3_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F4:
                              if (!SUMA_F4_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F5:
                              if (!SUMA_F5_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F6:
                              if (!SUMA_F6_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F7:
                              if (!SUMA_F7_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F8:
                              if (!SUMA_F8_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F9:
                              if (!SUMA_F9_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F10:
                              /* strgval can be used to set the value directly
                                 in the future. But it is not implemented
                                 in SUMA_F10_Key yet ... */
                              if (!SUMA_F10_Key(sv,stmp, "drivesuma", strgval)) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F11:
                              if (!SUMA_F11_Key(sv, stmp,"drivesuma", strgval)) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_F12:
                              if (!SUMA_F12_Key(sv, stmp, "drivesuma")) {
                                 SUMA_S_Err("Failed in Key function.");
                              }
                              break;
                           case XK_VoidSymbol:
                              SUMA_S_Errv("No good key for %s\n", stmp);
                              break;   
                           default:
                              SUMA_S_Errv("Don't know how to deal with %s "
                                          "in drive mode\n", stmp);
                              break;   
                        } /* end switch k */
                        /* do we need a call for redisplay now? */
                        if ((redisp || pauz != 0.0f )) {
                           /* a redisplay is already pending 
                           (from the key functions), kill it or it will get 
                           executed later*/
                           SUMA_remove_workproc2(  SUMA_handleRedisplay, 
                                                   sv->X->GLXAREA );
                           llist = SUMA_CreateList ();
                           SUMA_REGISTER_TAIL_COMMAND_NO_DATA(llist, 
                                       SE_RedisplayNow, SES_SumaFromAny, sv);
                           SUMA_LH("Forcing redisplay");
                           SUMA_Engine (&llist);
                        }

                        /* check on delay */
                        if (pauz < 0) {   
                           char buf[100];
                           sprintf( buf, 
                                 "Pausing DriveSuma at Key %s, rep=%d, N_rep=%d",
                                 stmp, rep, N_rep);
                           SUMA_LH("Calling user pause...");
                           #if 1
                              SUMA_PauseForUser(sv->X->TOPLEVEL, buf, 
                                                SWP_POINTER_OFF, NULL, 0, -1.0); 
                           #else /* kept here to illustrate bug */
         /* very slow when called repeatedly except when you open the 
         'Close All Viewers' thing by hand with SHFTESC then press No.
         In that case, repeated calls to SUMA_ForceUser_YesNo are quite fast.
         The problem in the XtManage call once the dialog is created. It takes
         forever to return. One solution, implemented in PauseForUser, is
         to destroy the widget each time and recreate it anew. Not a big deal.*/
                              SUMA_ForceUser_YesNo(sv->X->TOPLEVEL, 
                                                   "Close All Viewers?",
                                                   SUMA_YES, SWP_DONT_CARE);
                           #endif
                        } else if (pauz > 0.0f) {
                           SUMA_LHv("Sleeping for %dms\n", 
                                    (int) ((pauz-delta_t)*1000));
                           delta_t = SUMA_etime(&tt, 1);
                           if (delta_t < pauz) {
                              NI_sleep((int) ((pauz-delta_t)*1000));
                           }
                        }
                     } /* end of rep */
                     SUMA_free(stmp); stmp = NULL;
                  }  /* end have iith key */
               }  /* end loop all keys */
            }  
            break;
            
         case SE_SetRecorderCont:
            /* expects a ngr */
            if (EngineData->ngr_Dest != NextComCode ) {
               fprintf (SUMA_STDERR,
                        "Error %s: Data not destined correctly for %s (%d).\n"
                        "Have %d \n",
                        FuncName, NextCom, NextComCode, EngineData->ngr_Dest);
               break;
            }
            {
               char *stmp=NULL, *sname=NULL;
               int ifrom = -1, ito = -1, NoTsEt = -999999;
               if (NI_get_attribute(EngineData->ngr, "Anim_Dup")) {
                  NI_GET_INT(EngineData->ngr,"Anim_Dup", itmp);
                  if (!NI_GOT || itmp < 0) {
                     itmp = 0;
                  }
                  SUMA_LHv("Going for %d\n", itmp);
                  ISQ_set_anim_dup(itmp);
               }
               if (NI_get_attribute(EngineData->ngr, "Save_As")) {

                  #if 1
                  ifrom = ito = NoTsEt;
                  NI_GET_INT(EngineData->ngr, "Save_From", itmp);
                  if (!NI_GOT) {
                     itmp = -1; 
                  } else {
                     ifrom = itmp;
                  }
                  NI_GET_INT(EngineData->ngr, "Save_To", itmp);
                  if (!NI_GOT) { 
                     itmp = -1; 
                  }else {
                     ito = itmp;
                  }
                  
                  NI_GET_STR_CP(EngineData->ngr, "Save_As", stmp);
                  if (!stmp) {
                     SUMA_S_Err("Empty Save_As");
                     goto CLEAN_RECORDER_CONT;
                  }
                  fn = SUMA_ParseFname(stmp, 
                                       NI_get_attribute(EngineData->ngr, 
                                       "Caller_Working_Dir"));
                  if (!(sname = SUMA_copy_string(fn->FileName_NoExt))) {
                     sname = SUMA_copy_string("no_name");
                  }
                  sname = SUMA_append_replace_string(fn->AbsPath, sname, "", 2);
                  if (ito == NoTsEt && ifrom == NoTsEt) {
                     if (SUMA_IMG_EXT(fn->Ext)) {
                        ifrom = -1; ito = 0;/* nothing set, save last one */
                     } else if (SUMA_ANIM_EXT(fn->Ext)) {
                        ifrom = 0; ito = 0;
                           /* nothing set, save all in animation */
                     } else {
                        SUMA_S_Errv("No support for extension %s\n", fn->Ext);
                        goto CLEAN_RECORDER_CONT;
                     }
                  }
                  if (ito == NoTsEt || ifrom == NoTsEt) {
                     SUMA_S_Errv("Erreur! Horreur! ito=%d, ifrom=%d"
                                 " (NotSet=%d)\n", ito, ifrom , NoTsEt);
                     goto CLEAN_RECORDER_CONT;
                  }
                  #else
                  NI_GET_INT(EngineData->ngr, "Save_One", itmp);
                  if (NI_GOT) { 
                     if (itmp == -1) { ifrom = -1; ito = 0; }
                     else if (itmp >= 0) { ifrom = itmp; ito = ifrom; }
                     else {
                        SUMA_S_Errv("Bad value for -save_one (%d)\n", itmp);
                        break;
                     }
                  }
                  
                  NI_GET_STR_CP(EngineData->ngr, "Save_As", stmp);
                  if (!stmp) {
                     SUMA_S_Err("Empty Save_As");
                     goto CLEAN_RECORDER_CONT;
                  }
                  fn = SUMA_ParseFname(stmp, 
                                       NI_get_attribute( EngineData->ngr, 
                                                         "Caller_Working_Dir"));
                  if (!(sname = SUMA_copy_string(fn->FileName_NoExt))) {
                     sname = SUMA_copy_string("no_name");
                  }
                  sname = SUMA_append_replace_string(fn->AbsPath, sname, "", 2);
                  
                  /* more checking */
                  if (ito < 0 && ifrom < 0) {   
                     if (SUMA_IMG_EXT(fn->Ext)) {
                        ifrom = -1; ito = 0;/* nothing set, save last one */
                     } else if (SUMA_ANIM_EXT(fn->Ext)) {
                        ifrom = 0; ito = 0;/* nothing set, save all in animation */
                     } else {
                        SUMA_S_Errv("No support for extension %s\n", fn->Ext);
                        goto CLEAN_RECORDER_CONT;
                     }
                  }
                  #endif
                  if (ifrom > ito && ito > 0) { 
                     /* note that negative indices are OK, see ISQ_save_anim */
                     SUMA_S_Errv("Error: ifrom=%d > ito=%d\n", ifrom, ito);
                     goto CLEAN_RECORDER_CONT;
                  }
                  if (  SUMA_iswordsame_ci(fn->Ext,".agif") || 
                        SUMA_iswordsame_ci(fn->Ext,".gif")) {
                     ISQ_snap_agif_rng(sname, ifrom, ito);
                  } else if ( SUMA_iswordsame_ci(fn->Ext,".mpeg") || 
                              SUMA_iswordsame_ci(fn->Ext,".mpg")) {
                     ISQ_snap_mpeg_rng(sname, ifrom, ito);
                  } else if ( SUMA_iswordsame_ci(fn->Ext,".jpeg") || 
                              SUMA_iswordsame_ci(fn->Ext,".jpg")) {
                     ISQ_snap_jpeg_rng(sname, ifrom, ito);
                  } else if (SUMA_iswordsame_ci(fn->Ext,".png") ) {
                     ISQ_snap_png_rng(sname, ifrom, ito);
                  } else {
                     SUMA_S_Errv("Not ready to deal with format %s\n", tmpstr);
                     goto CLEAN_RECORDER_CONT;
                  }
               }
               CLEAN_RECORDER_CONT:
               if (stmp) SUMA_free(stmp); stmp = NULL;
               if (sname) SUMA_free(sname); sname = NULL;
               if (fn) fn = SUMA_Free_Parsed_Name(fn);
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

void *SUMA_nimlEngine2Engine(NI_group *ngr)
{
   static char FuncName[]={"SUMA_nimlEngine2Engine"};
   DList *list = NULL;
   int  isv, itmp;
   SUMA_NI_COMMAND_CODE cc;
   SUMA_EngineData *ED = NULL; 
   DListElmt *el=NULL;
   void *Ret = NULL;
   char *adoid=NULL, *svid=NULL, *name=NULL, *adolabel=NULL, 
        ename[32], lhs[64], rhs[256], *enveqn=NULL, *attr=NULL;
   SUMA_ALL_DO *ado = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   SUMA_PARSED_NAME *fn = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ngr) { SUMA_S_Err("NULL input"); SUMA_RETURN(Ret); }
   if (LocalHead) SUMA_ShowNel(ngr);
   if (strcmp(ngr->name, "EngineCommand")) {
      fprintf (SUMA_STDERR,
               "Error %s: group name (%s) is not (EngineCommand)\n", 
               FuncName, ngr->name);
      SUMA_RETURN(Ret); 
   }
   
   /* Is this a valid command? */
   cc = SUMA_niCommandCode(NI_get_attribute(ngr,"Command"));
   if (cc == SE_Empty || cc == SE_BadCode) {
      SUMA_S_Errv("Bad command code %s", 
                  SUMA_CHECK_NULL_STR(NI_get_attribute(ngr,"Command")));
      SUMA_RETURN(Ret); 
   }
   
   /* do we have the classics? */
   itmp = 0;
   sprintf(ename,"ENV.%d", itmp);
   while ((attr=NI_get_attribute(ngr, ename))) {
      SUMA_LHv("Have %s:>%s<\n", ename, attr);
      lhs[0] = '\0'; rhs[0]='\0';
      if (SUMA_ParseLHS_RHS (attr, lhs, rhs)) {
         enveqn = (char *) malloc(strlen(lhs)+strlen(rhs)+4) ;
         sprintf(enveqn,"%s=%s", lhs, rhs);
         putenv(enveqn);
         SUMA_LHv("Got %s\n", enveqn);
      } else {
         SUMA_S_Errv("Failed to parse %s: %s\n",
               ename, attr);
      }
      sprintf(ename,"ENV.%d", ++itmp);
   }
   
   sv = NULL;
   svid = NI_get_attribute(ngr,"SV_id");
   if (svid) {
      isv = SUMA_TO_LOWER_C(svid[0])-'a';
      if (isv < 0 || isv > SUMAg_N_SVv) {
         /* try in case it was an int */
         isv = atoi(svid);
      }
      if (isv < 0 || isv > SUMAg_N_SVv) {
         SUMA_S_Errv("Bad SV_id of %s\n", svid);
         SUMA_RETURN(Ret);  
      }
      sv = &(SUMAg_SVv[isv]);
      if (!sv->X->TOPLEVEL) {
         SUMA_S_Errv("Viewer %s must first be created "
                     "with a separate -com command.\n", svid);
         SUMA_RETURN(Ret);  
      }
   } 
   if (!sv) {
      sv = &(SUMAg_SVv[0]);
   }
   SUMA_LH("Viewer id %s, isv=%d sv=%p",svid?svid:"NULL", isv, sv);
   
   ado = NULL;
   if (!(adoid = NI_get_attribute(ngr,"SO_idcode")) &&
       !(adoid = NI_get_attribute(ngr,"DO_idcode"))) 
      adoid = NI_get_attribute(ngr,"ADO_idcode");
      
   if (adoid) {
      ado = SUMA_whichADO(adoid, SUMAg_DOv, SUMAg_N_DOv);
      if (!ado) {
         SUMA_S_Errv("DO with id %s not found.\n", adoid);
         SUMA_RETURN(Ret);  
      }
   }
   if (!(adolabel = NI_get_attribute(ngr,"SO_label")) &&
       !(adolabel = NI_get_attribute(ngr,"DO_label"))) 
      adolabel = NI_get_attribute(ngr,"ADO_label");;
   if (adolabel) {
      if (ado && strcmp(adolabel, ADO_LABEL(ado))) {
         SUMA_S_Errv("Conflict between id %s (%s) and label (%s)", 
                     ADO_ID(ado), ADO_LABEL(ado), adolabel);
         SUMA_RETURN(Ret); 
      }
      if (!ado) { /* find ado based on label! */
         if ((adoid = SUMA_find_ADOidcode_from_label(adolabel, 
                                          SUMAg_DOv, SUMAg_N_DOv))) {
            ado = SUMA_whichADO(adoid, SUMAg_DOv, SUMAg_N_DOv);
            if (!ado) {
               SUMA_S_Errv("ADO with id %s not found.\n", adoid);
               SUMA_RETURN(Ret);  
            }
         }else {
            SUMA_S_Errv("ADO id from label %s not found.\n", adolabel);
            SUMA_RETURN(Ret);  
         }
      }
   }
   if (!ado) ado = SUMA_SV_Focus_ADO(sv);
   if (!ado) {
      ado = SUMA_findanyFocusable_ADO(NULL);/* last resort */
   }
   if (!ado) {
      SUMA_S_Err("Have no ADOs to work with at all.\n");
      SUMA_RETURN(Ret);  
   } else {
      if (LocalHead) {
         SUMA_LHv("Have ADO %s to work with\n", ADO_LABEL(ado));
      }
   }
   /* Create da list */
   if (!list) list = SUMA_CreateList ();

   
   
   SurfCont = SUMA_ADO_Cont(ado);
   /* OK, now, switch on that command and create the Engine structure */
   switch (cc) {
       case SE_niSetObjectCont:
       case SE_niSetSurfCont:
         if (!SurfCont) {
            SUMA_S_Err( "Unexpected NULL SurfCont\n"
                        "Please report error to author.");
            SUMA_RETURN(Ret);
         }
         if (!SUMA_isADO_Cont_Realized(ado)){ 
                                       /* better have a controller 
                                                before going crazy */
            if (0) { /* this option or the next behave in the same way */
               if (!SUMA_viewSurfaceCont(NULL, ado, sv)) {
                        SUMA_S_Err("Failed open surfcont");
                        break;  
               }
            } else {
               ED = SUMA_InitializeEngineListData (SE_OpenSurfCont);
               if (!SUMA_RegisterEngineListCommand (  
                           list, ED,
                           SEF_vp, (void *)ado,
                           SES_SumaFromAny, (void *)sv, 
                           NOPE,
                           SEI_Head, NULL)) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed to register command.\n", FuncName);
               }
            }
            
            /* make sure that business is closed if user does not control it */
            /* IF you close the window quickly, the colormap does not render
            and you can get into trouble then, this also happens if
            Drive SUMA orders an immediate surface controller closing after
            a dset load ... */
            
            if (!NI_get_attribute(ngr, "View_Surf_Cont"))  
               NI_set_attribute(ngr, "View_Surf_Cont", "n");      
         }
         
         if ((name = NI_get_attribute(ngr,"Dset_FileName"))) {
            /* Have a dset to load */
            ED = SUMA_InitializeEngineListData (SE_OpenDsetFile);
            if (!(el = SUMA_RegisterEngineListCommand (  list, ED,
                                                      SEF_vp, (void *)ado,
                                                      SES_SumaFromAny, 
                                                      (void *)sv, NOPE,
                                                      SEI_Tail, NULL))) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed to register command.\n", FuncName);
            }
            if (!SUMA_RegisterEngineListCommand (  list, ED,
                                                      SEF_cp, (void *)name,
                                                      SES_SumaFromAny, 
                                                      (void *)sv, NOPE,
                                                      SEI_In, el)) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed to register command.\n", FuncName);
            }
         }
         
         if ((name = NI_get_attribute(ngr,"Mask_FileName"))) {
            /* Have a Mask to load */
            ED = SUMA_InitializeEngineListData (SE_OpenMaskFile);
            if (!(el = SUMA_RegisterEngineListCommand (  list, ED,
                                                      SEF_vp, (void *)ado,
                                                      SES_SumaFromAny, 
                                                      (void *)sv, NOPE,
                                                      SEI_Tail, NULL))) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed to register command.\n", FuncName);
            }
            if (!SUMA_RegisterEngineListCommand (  list, ED,
                                                      SEF_cp, (void *)name,
                                                      SES_SumaFromAny, 
                                                      (void *)sv, NOPE,
                                                      SEI_In, el)) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed to register command.\n", FuncName);
            }
         }
         
         if ((name = NI_get_attribute(ngr,"Col_FileName"))) {
            /* Have a color file to load */
            ED = SUMA_InitializeEngineListData (SE_OpenColFile);
            if (!(el = SUMA_RegisterEngineListCommand (  list, ED,
                                                      SEF_vp, (void *)ado,
                                                      SES_SumaFromAny, 
                                                      (void *)sv, NOPE,
                                                      SEI_Tail, NULL))) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed to register command.\n", FuncName);
            }
            if (!SUMA_RegisterEngineListCommand (  list, ED,
                                                      SEF_cp, (void *)name,
                                                      SES_SumaFromAny, 
                                                      (void *)sv, NOPE,
                                                      SEI_In, el)) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed to register command.\n", FuncName);
            }
         }
         /* all the rest can be handled in one engine call */
         ED = SUMA_InitializeEngineListData (SE_SetSurfCont);
         if (!(el = SUMA_RegisterEngineListCommand (  list, ED,
                                                   SEF_ngr, (void *)ngr,
                                                   SES_SumaFromAny, (void *)sv, 
                                                   NOPE,
                                                   SEI_Tail, NULL))) {
               fprintf (SUMA_STDERR, 
                        "Error %s: Failed to register command.\n", FuncName);
         }
         if (!SUMA_RegisterEngineListCommand (  list, ED,
                                                   SEF_vp, (void *)ado,
                                                   SES_SumaFromAny, (void *)sv, 
                                                   NOPE,
                                                   SEI_In, el)) {
               fprintf (SUMA_STDERR, 
                        "Error %s: Failed to register command.\n", FuncName);
         }
         break; 
      case SE_niSetViewerCont:
         if ((name = NI_get_attribute(ngr,"N_foreg_smooth"))) {
            SUMA_SetNumForeSmoothing(name, (void *)sv);
         }
         if ((name = NI_get_attribute(ngr,"N_final_smooth"))) {
            SUMA_SetNumFinalSmoothing(name, (void *)sv);
         }
         if ((name = NI_get_attribute(ngr,"VVS_FileName"))) {
            /* Have a vvs to load, do it straight up, 
               no need to call some SE_SetViewerCont, a la SE_SetSurfCont yet */
            if ((fn = SUMA_ParseFname(name, 
                                 NI_get_attribute(ngr, 
                                       "Caller_Working_Dir")))) {
               SUMA_LoadVisualState(fn->FullName, (void *)sv);
            }
            if (fn) fn = SUMA_Free_Parsed_Name(fn);
         }
         if ((name = NI_get_attribute(ngr,"DO_FileName"))) {
            /* Have a DO to load, straight up too*/
            if ((fn = SUMA_ParseFname(name, 
                                 NI_get_attribute(ngr, 
                                       "Caller_Working_Dir")))) {
               SUMA_LoadSegDO(fn->FullName, (void *)sv);
            }
            if (fn) fn = SUMA_Free_Parsed_Name(fn);
         }
         if ((name = NI_get_attribute(ngr,"DoViewerSetup"))) {
            /* have something to do with viewer setup */
            if (!SUMA_ApplyVisualState((NI_element *)ngr, sv)) {
               SUMA_S_Err("Failed to apply state");
               SUMA_RETURN(Ret);
            }
         }
         /* all the rest can be handled in one engine call */
         ED = SUMA_InitializeEngineListData (SE_SetViewerCont);
         if (!(el = SUMA_RegisterEngineListCommand (  list, ED,
                                                   SEF_ngr, (void *)ngr,
                                                   SES_SumaFromAny, (void *)sv, 
                                                   NOPE,
                                                   SEI_Tail, NULL))) {
               fprintf (SUMA_STDERR, 
                        "Error %s: Failed to register command.\n", FuncName);
         }
         if (!SUMA_RegisterEngineListCommand (  list, ED,
                                                   SEF_vp, (void *)ado,
                                                   SES_SumaFromAny, (void *)sv, 
                                                   NOPE,
                                                   SEI_In, el)) {
               fprintf (SUMA_STDERR, 
                        "Error %s: Failed to register command.\n", FuncName);
         }
         break;    
      case SE_niSetRecorderCont:
         ED = SUMA_InitializeEngineListData (SE_SetRecorderCont);
         if (!(el = SUMA_RegisterEngineListCommand (  list, ED,
                                                   SEF_ngr, (void *)ngr,
                                                   SES_SumaFromAny, (void *)sv, 
                                                   NOPE,
                                                   SEI_Tail, NULL))) {
               fprintf (SUMA_STDERR, 
                        "Error %s: Failed to register command.\n", FuncName);
         }
         
         break;
      case SE_niKillSuma:
         XtCloseDisplay( SUMAg_CF->X->DPY_controller1 ) ;
         selenium_close();/* close selenium opened browser windows if open */
         exit(0);
         break;
      case SE_GetLabel:
           {
               /* show the current surface label */
               SUMA_SurfaceObject *SO=NULL;
               char *lbls;
//              lbls = ADO_LABEL(ado);
               SO = (SUMA_SurfaceObject *)ado;
               lbls = SUMA_GetLabelsAtSelection(ado,
                      SO->SelectedNode, -1);
               sumaout = SUMA_drive_get_outstream();
               fprintf(sumaout,"%s\n",lbls);
               fflush(sumaout);
            }
            break;           

      default:
         SUMA_S_Errv("Cannot deal with command %s yet.\n", 
                     NI_get_attribute(ngr,"command"));
         SUMA_RETURN(Ret); 
         break;
            
   }
   
   if (!SUMA_Engine(&list)) {
      SUMA_SLP_Err("Failed to execute command.");
      SUMA_RETURN(Ret);
   }
   
 
   SUMA_RETURN(Ret);
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
   
   SUMA_ENTRY;

   for (i=0; i< sv->N_DO; ++i) {
      if (SUMA_isSO_G(dov[sv->RegistDO[i].dov_ind], sv->CurGroupName)) {
         if (SO_IDs != NULL) SO_IDs[k] = sv->RegistDO[i].dov_ind;
         ++k;
      }
   }

   SUMA_RETURN (k);
}
/*!
   ans = SUMA_VisibleSOs (sv, dov, SO_IDs);
   gets the IDs (indices into dov) and number of the Surface Objects 
         registered with sv and with the SO->Side matching sv->ShowRight/
         sv->ShowLeft and with SO->Show set to YUP
   \param sv (SUMA_SurfaceViewer *) the surface viewer structure
   \param dov (SUMA_DO *) the Displayable Objects vector (accessible to sv)
   \param SO_IDs (int *) pre-allocated integer vector that will contain the 
         IDs of the SO shown in sv
         send NULL if you do not care for it and all you'll get is ans
   \param forpicking (int) 1 --> surface rendered as points will be 
                                 considered invisible 
                                 
   \ret ans (int) the number of SOs shown in SV
   Still confused ? read the code for the function, it is shorter 
   than the documentation.
   
   \sa SUMA_isVisibleSO, SUMA_Selectable_ADOs
*/

int SUMA_VisibleSOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *SO_IDs, 
                     int forpicking)
{
   static char FuncName[]={"SUMA_VisibleSOs"};
   SUMA_SurfaceObject *SO=NULL;
   int i, k = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   for (i=0; i< sv->N_DO; ++i) {
      if (SUMA_isSO_G(dov[sv->RegistDO[i].dov_ind], sv->CurGroupName)) {
         SO = (SUMA_SurfaceObject *)dov[sv->RegistDO[i].dov_ind].OP;
         if (SO_SHOWING(SO, sv) && (!forpicking || SO->PolyMode != SRM_Points)) {
            if (  SO->Side == SUMA_NO_SIDE || 
                  SO->Side == SUMA_SIDE_ERROR  || 
                  SO->Side == SUMA_LR) {
               if (SO_IDs) {
                  SO_IDs[k] = sv->RegistDO[i].dov_ind;
               }
               ++k;
            } else if (  (SO->Side == SUMA_RIGHT && sv->ShowRight) || 
                         (SO->Side == SUMA_LEFT && sv->ShowLeft) ) {
               if (SO_IDs) {
                  SO_IDs[k] = sv->RegistDO[i].dov_ind;
               }
               ++k;
            }
         }
      } else {
      	 switch (dov[sv->RegistDO[i].dov_ind].ObjectType) {
	    case CDOM_type:
	       SUMA_LH("So how do we handle the visibility of surfaces "
	               "within a CIFTI object? Is this the place for it?");
	       break;
	 }
      }
   }
   
   SUMA_RETURN (k);
}

int SUMA_VisibleMDOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *MDO_IDs)
{
   static char FuncName[]={"SUMA_VisibleMDOs"};
   SUMA_MaskDO *MDO=NULL;
   int i, k = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   for (i=0; i< sv->N_DO; ++i) {
      if (dov[sv->RegistDO[i].dov_ind].ObjectType != MASK_type) continue;
      MDO = (SUMA_MaskDO *)dov[sv->RegistDO[i].dov_ind].OP;
      if (!MDO_IS_SHADOW(MDO) && MDO_SHOWING(MDO, sv)) {
         if (  MDO->SO->Side == SUMA_NO_SIDE || 
               MDO->SO->Side == SUMA_SIDE_ERROR  || 
               MDO->SO->Side == SUMA_LR) {
            if (MDO_IDs) {
               MDO_IDs[k] = sv->RegistDO[i].dov_ind;
            }
            ++k;
         } else if (  (MDO->SO->Side == SUMA_RIGHT && sv->ShowRight) || 
                      (MDO->SO->Side == SUMA_LEFT && sv->ShowLeft) ) {
            if (MDO_IDs) {
               MDO_IDs[k] = sv->RegistDO[i].dov_ind;
            }
            ++k;
         }
      }
   }
   
   SUMA_RETURN (k);
}

/*
   Is this the kind of DO that can be selected in SUMA
   In other terms, can it be loaded and viewed with nothing
   else?
*/
int SUMA_is_iDO_Selectable(int dov_id)
{
   static char FuncName[]={"SUMA_is_iDO_Selectable"};
   
   switch (iDO_type(dov_id)){
      case SO_type:
      case TRACT_type:
      case GRAPH_LINK_type:
      case MASK_type:
      case VO_type:
      case CDOM_type:
         return(1);
      default:
         return(0);
   }
}

int SUMA_Selectable_ADOs (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int *SO_IDs)
{
   static char FuncName[]={"SUMA_Selectable_ADOs"};
   SUMA_SurfaceObject *SO=NULL;
   int i, k = 0;
   SUMA_NIDO *SDO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   for (i=0; i< sv->N_DO; ++i) {
      SUMA_LHv("Checking on %s\n", iDO_label(sv->RegistDO[i].dov_ind));
      if (SUMA_isSO_G(dov[sv->RegistDO[i].dov_ind], sv->CurGroupName)) {
         SO = (SUMA_SurfaceObject *)dov[sv->RegistDO[i].dov_ind].OP;
         if (SO_SHOWING(SO, sv)) {
            if (  SO->Side == SUMA_NO_SIDE || 
                  SO->Side == SUMA_SIDE_ERROR  || 
                  SO->Side == SUMA_LR) {
               if (SO_IDs) {
                  SO_IDs[k] = sv->RegistDO[i].dov_ind;
               }
               ++k;
            } else if (  (SO->Side == SUMA_RIGHT && sv->ShowRight) || 
                         (SO->Side == SUMA_LEFT && sv->ShowLeft) ) {
               if (SO_IDs) {
                  SO_IDs[k] = sv->RegistDO[i].dov_ind;
               }
               ++k;
            }
         }
      } else {
         switch (dov[sv->RegistDO[i].dov_ind].ObjectType) {
            case SO_type:
               /* ignore, escaped from above isSO_G */
               break;
            case GRAPH_LINK_type:
               /* avoid the shadow ... */
               if (!SUMA_IS_GOOD_STATE(
                     iDO_state(sv->RegistDO[i].dov_ind))) break;
               /* OK, keep */
               if (SO_IDs) {
                  SO_IDs[k] = sv->RegistDO[i].dov_ind;
               }
               ++k;
               break;
            case TRACT_type:
            case MASK_type:
            case CDOM_type:
            case VO_type:
               if (SO_IDs) {
                  SO_IDs[k] = sv->RegistDO[i].dov_ind;
               }
               ++k;
               break;
	    default:
               SUMA_LHv("Ignoring %s\n", iDO_label(sv->RegistDO[i].dov_ind));
               break;
         }
      }
   }
   
   SUMA_RETURN (k);
}

int SUMA_ADOs_WithSurfCont (SUMA_DO *dov, int N_dov, int *dov_IDs)
{
   static char FuncName[]={"SUMA_ADOs_WithSurfCont"};
   SUMA_SurfaceObject *SO=NULL;
   int i, k = 0;
   SUMA_NIDO *SDO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   for (i=0; i< N_dov; ++i) {
      if (SUMA_ADO_Cont((SUMA_ALL_DO*)SUMAg_DOv[i].OP)) {
                  dov_IDs[k] = i;
               ++k;
      }
   }
   
   SUMA_RETURN (k);
}


/*!
   \brief YUP if surface is visible in a viewer
   \sa SUMA_VisibleSOs 
*/
SUMA_Boolean SUMA_isVisibleDO (SUMA_SurfaceViewer *sv, 
                               SUMA_DO *dov, SUMA_ALL_DO *ado)
{
   static char FuncName[]={"SUMA_isVisibleDO"};
   SUMA_SurfaceObject *SO=NULL;
   SUMA_SurfaceObject *curSO=NULL;
   int i, k = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   switch(ado->do_type) {
      case SO_type:
         curSO = (SUMA_SurfaceObject *)ado;
         for (i=0; i< sv->N_DO; ++i) {
            if (SUMA_isSO_G(dov[sv->RegistDO[i].dov_ind], sv->CurGroupName)) {
               SO = (SUMA_SurfaceObject *)dov[sv->RegistDO[i].dov_ind].OP;
               if (curSO == SO) {
                  if (SO_SHOWING(SO, sv)) {
                     if ( SO->Side == SUMA_NO_SIDE || 
                          SO->Side == SUMA_SIDE_ERROR ) {
                        SUMA_RETURN(YUP);
                        ++k;
                     } else if (  (SO->Side == SUMA_RIGHT && sv->ShowRight) || 
                                  (SO->Side == SUMA_LEFT && sv->ShowLeft) ) {
                        SUMA_RETURN(YUP);
                        ++k;
                     }
                  }
               }
            }
         }
         break;
      case GDSET_type:
         SUMA_S_Err("Can't judge this without variant");
         SUMA_RETURN(NOPE);
         break;
      case CDOM_type:
      case VO_type:
      case MASK_type:
      case TRACT_type:
      case GRAPH_LINK_type:
         for (i=0; i< sv->N_DO; ++i) {
            if (dov[sv->RegistDO[i].dov_ind].OP == ado)  SUMA_RETURN(YUP);
         }
         SUMA_RETURN(YUP);
         break;
      default:
         SUMA_S_Errv("Nothing to do with %s\n",
               SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(NOPE);
   
}
SUMA_Boolean SUMA_isRegisteredSO (SUMA_SurfaceViewer *sv, 
                                  SUMA_DO *dov, SUMA_SurfaceObject *curSO)
{
   static char FuncName[]={"SUMA_isRegisteredSO"};
   SUMA_SurfaceObject *SO=NULL;
   int i, k = 0;
   
   SUMA_ENTRY;
   
   for (i=0; i< sv->N_DO; ++i) {
      if (1) {
         SO = (SUMA_SurfaceObject *)dov[sv->RegistDO[i].dov_ind].OP;
         if (curSO == SO) {
            SUMA_RETURN(YUP);
         }
      }
   }
   
   SUMA_RETURN(NOPE);
}

SUMA_Boolean SUMA_ADO_isRegistered(SUMA_SurfaceViewer *sv, SUMA_ALL_DO *ado) 
{
   static char FuncName[]={"SUMA_ADO_isRegistered"};
   return(SUMA_isRegisteredDO(sv, SUMAg_DOv, ado));
}

SUMA_Boolean SUMA_isRegisteredDO (SUMA_SurfaceViewer *sv, 
                                  SUMA_DO *dov, SUMA_ALL_DO *curDO)
{
   static char FuncName[]={"SUMA_isRegisteredDO"};
   SUMA_ALL_DO *DO=NULL;
   int i, k = 0;
   
   SUMA_ENTRY;
   
   for (i=0; i< sv->N_DO; ++i) {
      if (1) {
         DO = (SUMA_ALL_DO *)dov[sv->RegistDO[i].dov_ind].OP;
         if (curDO == DO) {
            SUMA_RETURN(YUP);
         }
      }
   }
   
   SUMA_RETURN(NOPE);
}


/*!
   Return 1st decent, preferably anat correct state(i.e. not shadown state)
*/
int SUMA_FirstGoodState(SUMA_SurfaceViewer *sv)
{
   static char FuncName[] = {"SUMA_FirstGoodState"};
   int inxt, iok;
   
   SUMA_ENTRY;
   
   inxt = 0; iok = -1;
   while (inxt < sv->N_VSv) {
      if (SUMA_IS_GOOD_STATE(sv->VSv[inxt].Name)) {
         if (sv->VSv[inxt].AnatCorrect) {
            SUMA_RETURN(inxt);
         } else {
            if (iok < 0) iok = inxt;
         }
      }
      ++inxt;
   }
   
   SUMA_RETURN(iok);
}
int SUMA_FirstGoodAnatCorrState(SUMA_SurfaceViewer *sv)
{
   static char FuncName[] = {"SUMA_FirstGoodAnatCorrState"};
   int inxt, iok;
   
   SUMA_ENTRY;
   
   inxt = 0; iok = -1;
   while (inxt < sv->N_VSv) {
      if (SUMA_IS_GOOD_STATE(sv->VSv[inxt].Name)) {
         if (sv->VSv[inxt].AnatCorrect) {
            SUMA_RETURN(inxt);
         } 
      }
      ++inxt;
   }
   
   SUMA_RETURN(iok);
}


/*! 
   nxtState = SUMA_NextState(sv);

   get the next Viewing State available in sv
   \param sv (SUMA_SurfaceViewer *) pointer to surface viewer structure 
   \ret nxtState (int) the index into sv->VSv of the next state
      -1 if there is trouble
      icur is returned if there is no next state of the group sv->CurGroupName
   \sa SUMA_PrevState 
*/
int SUMA_NextState(SUMA_SurfaceViewer *sv)
{
   static char FuncName[] = {"SUMA_NextState"};
   int inxt, icur;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   icur = SUMA_WhichState (sv->State, sv, sv->CurGroupName);
   if (icur < 0) {
      SUMA_S_Err("SUMA_WhichState failed.");
      SUMA_RETURN (-1);
   } else {
      inxt = (icur + 1) % sv->N_VSv;
      do {
         /* Now see if the upcoming one is of the same group */
         if (inxt == icur) {
            /* back where we started */
            SUMA_RETURN(inxt);
         } else if (strncmp(sv->VSv[inxt].Name,"TheShadow",9)) {
            if (!strcmp(sv->VSv[inxt].Group, sv->CurGroupName) ||
                (!strcmp(sv->VSv[inxt].Group, "ANY"))) { 
               /* group match, good, go back */
               SUMA_RETURN(inxt);
            }
         } else {
            /* Skip the shadow state */
         }
         inxt = (inxt + 1) % sv->N_VSv;
      } while (1);
   }
   
   /* should not get here */
   SUMA_SL_Err("Flow error");
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
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   icur = SUMA_WhichState (sv->State, sv, sv->CurGroupName);   
   if (icur < 0) {
      fprintf(SUMA_STDERR,"Error %s: SUMA_WhichState failed.\n", FuncName);
      SUMA_RETURN (-1);
   } else {
      inxt = icur -1; if (inxt < 0) inxt = sv->N_VSv + inxt;
      do {
         /* Now see if the upcoming one is of the same group */
         if (inxt == icur) {
            /* back where we started */
            SUMA_RETURN(inxt);
         } else if (strncmp(sv->VSv[inxt].Name,"TheShadow",9)) {
            if (!strcmp(sv->VSv[inxt].Group, sv->CurGroupName) ||
                (!strcmp(sv->VSv[inxt].Group, "ANY"))) { 
               /* group match, good, go back */
               SUMA_RETURN(inxt);
            }
         } else {
            /* Skip the shadow state */
         }
         inxt = inxt -1; if (inxt < 0) inxt = sv->N_VSv + inxt;
      } while (1);
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

   SUMA_ENTRY;

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
SUMA_Boolean SUMA_SwitchSO (SUMA_DO *dov, int N_dov, 
                            int SOcurID, int SOnxtID, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_SwitchSO"};
   SUMA_Axis *EyeAxis;
   int EyeAxis_ID;
   char CommString[100];
   SUMA_EngineData ED;
   DList *list = NULL;
   
   SUMA_ENTRY;

   /* unregister the current surface from RegisteredDO */
   /*fprintf(SUMA_STDERR,"%s: Unregistering DOv[%d]...\n", FuncName, SOcurID);*/
   if (!SUMA_UnRegisterDO(SOcurID, sv)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to UnRegisterDO.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   /* set the focus ID to the current surface */
   sv->Focus_DO_ID = SOnxtID;

   /* register the new surface in RegisteredDO */
   /*fprintf(SUMA_STDERR,"%s: Registering DOv[%d]...\n", 
             FuncName, sv->Focus_DO_ID); */
   if (!SUMA_RegisterDO(sv->Focus_DO_ID, sv)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to RegisterDO.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   /* modify the rotation center */
   if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,
               "Error %s: Failed to update center of rotation", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* set the viewing points */
   if (!SUMA_UpdateViewPoint(sv, dov, N_dov, 0)) {
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
   
   /* do the light business */
   if (dov[sv->Focus_DO_ID].ObjectType == SO_type &&
       !SUMA_SetViewerLightsForSO(sv, 
                  (SUMA_SurfaceObject *)(dov[sv->Focus_DO_ID].OP))) {
      SUMA_S_Warn("Failed to set viewer lights.\n"
                  "Use 'F' key to flip lights in SUMA\n"
                  "if necessary.");
   }
   
   /* do the axis setup */
   SUMA_WorldAxisStandard (sv->WAx, sv);


   /* Home call baby */
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Home, SES_Suma, sv);

   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
   }

   
   /* take care of the cross hair's XYZ */

   /* to do elsewhere */
   /* when a cross hair needs to be communicated, you must use 
      the LocalDomainParentID surface and not the Focus_Surface */
   SUMA_RETURN (YUP);
}

/*!
   \brief gets overlays from parent surface SO_prec to child surface SO_nxt
   
*/
SUMA_Boolean SUMA_GetOverlaysFromParent(SUMA_SurfaceObject *SO_nxt, 
                                        SUMA_SurfaceObject *SO_prec) 
{
   static char FuncName[]={"SUMA_GetOverlaysFromParent"};
   int j, OverInd=-1;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!SO_nxt || !SO_prec) {
      SUMA_SL_Err("Null input");
      SUMA_RETURN(NOPE);
   }
   if (!SUMA_isRelated_SO(SO_prec, SO_nxt, 1)) {
      SUMA_SL_Err("Surfaces are not level 1 related");
      SUMA_RETURN(NOPE);
   }

   /* Create a link to each overlay plane in the precursor unless 
      such a plane exists already  */
   for (j=0; j < SO_prec->N_Overlays; ++j) {
      if (!SUMA_Fetch_OverlayPointer ((SUMA_ALL_DO *)SO_nxt,
                           SO_prec->Overlays[j]->Name, &OverInd)) {
         /* plane not found, create a link to it */
         SUMA_LHv("Overlay plane %s not found, creating the link.\n", 
                  SO_prec->Overlays[j]->Name);
         SO_nxt->Overlays[SO_nxt->N_Overlays] = 
               (SUMA_OVERLAYS *)SUMA_LinkToPointer((void*)SO_prec->Overlays[j]);
         /* it happens at times, that an overlay carries coordinate bias with it. 
         When that happens, the bias should be added immediately */
         if (SO_nxt->Overlays[SO_nxt->N_Overlays]->OptScl) {
            if (SO_nxt->Overlays[SO_nxt->N_Overlays]->OptScl->BiasVect) {
               SUMA_LH("Adding coordbias");
               SUMA_AddVisX_CoordBias(  SO_nxt,
                     SO_nxt->Overlays[SO_nxt->N_Overlays],
                     SO_nxt->Overlays[SO_nxt->N_Overlays]->OptScl->DoBias,
                     SO_nxt->Overlays[SO_nxt->N_Overlays]->OptScl->BiasVect);
               /* Apply the business */
               if (!SUMA_ApplyVisXform(SO_nxt, "VisX", 
                                       FORWARD_XFORM, 1)) {
                  SUMA_S_Warn("Failed to apply VisX transform");
               }
            }
         }
         /*increment the number of overlay planes */
         ++SO_nxt->N_Overlays;
      } else {
         /* plane found, do nothing */
         SUMA_LHv("Overlay plane %s found. Index#%d\n.", 
                  SO_prec->Overlays[j]->Name, OverInd);
      }
   }   

   SUMA_RETURN(YUP);
}

/*! 
   ans = SUMA_SwitchState (dov, N_dov, sv, nxtstateID, nxtgroup);
   
   Replaces one viewing state with another

*/
SUMA_Boolean SUMA_SwitchState (  SUMA_DO *dov, int N_dov, 
                                 SUMA_SurfaceViewer *sv, 
                                 int nxtstateID, char *nxtgroup)
{
   static char FuncName[]={"SUMA_SwitchState"};
   SUMA_Axis *EyeAxis;
   int EyeAxis_ID, I_C, ND, id;
   char CommString[100];
   SUMA_EngineData ED;
   int curstateID, i, j, jmax, prec_ID, *MembSOs=NULL, N_MembSOs=0;
   int RegSO[SUMA_MAX_DISPLAYABLE_OBJECTS], N_RegSO;
   SUMA_SurfaceObject *SO_nxt, *SO_prec, *SO1, *SO2, *SOtmp=NULL;
   SUMA_DO_Types ttv[10]={SO_type, GRAPH_LINK_type, NOT_SET_type};
   float *XYZ, *XYZmap, zfac = 0.0;
   DList *list = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   XYZ = NULL;
   XYZmap = NULL;
   
   curstateID = SUMA_WhichState(sv->State, sv, sv->CurGroupName);
   zfac = sv->FOV[curstateID]/SUMA_sv_auto_fov(sv);
   if (zfac > 10) zfac = 10.0;
   if (zfac < 0.005) zfac = 0.005;
   
   /* unregister all the surfaces for the current view */
   if (LocalHead) 
         fprintf( SUMA_STDERR,
                  "%s: Request to change to id %d\n"
                  "Unregistering state %d (%s), sv(%p)->State = %s\n", 
                  FuncName, nxtstateID, curstateID, sv->VSv[curstateID].Name, 
                  sv, sv->State);
   for (i=0; i<sv->VSv[curstateID].N_MembDO; ++i) {
      if (!SUMA_UnRegisterDO(sv->VSv[curstateID].MembDO[i].dov_ind, sv)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to UnRegisterDO.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
   }
   
   /* adopt the new group */
   if (strcmp(sv->CurGroupName, nxtgroup)) { 
      SUMA_LH("Changing group...");
      if (!SUMA_AdoptGroup(sv, nxtgroup)) {
         SUMA_SLP_Err("Failed to adopt new group");
         SUMA_RETURN(NOPE);
      }
   } else {
      SUMA_LH("No group change...");
   }
   
   /* register all the surfaces/DOs from the next view */

   /* Need to set the state before you go into Registering DOs    May 2013 */
   sv->State =  sv->VSv[nxtstateID].Name;
   sv->iState = nxtstateID;

   SUMA_LHv("Registering %d DOs of state %d (%s)...\n", 
            sv->VSv[nxtstateID].N_MembDO, nxtstateID, 
            sv->VSv[nxtstateID].Name);
   for (i=0; i<sv->VSv[nxtstateID].N_MembDO; ++i) {
      if (!SUMA_RegisterDO(sv->VSv[nxtstateID].MembDO[i].dov_ind, sv)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to RegisterDO.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
   }
   
   /* if have two surfaces, keep them from overlapping */
   N_RegSO = SUMA_RegisteredSOs (sv, dov, RegSO);
   if (N_RegSO > 2) {
      SUMA_LH( "Will not attempt\n"
                     "to separate more than\n"
                     "2 surfaces.\n");
   } else if (N_RegSO == 2) {
      SO1 = (SUMA_SurfaceObject *)dov[RegSO[0]].OP;
      SO2 = (SUMA_SurfaceObject *)dov[RegSO[1]].OP;
      SUMA_LHv("Computing separation for %s and %s\n",
         SO1->Label, SO2->Label);
      if (!SO1->VisX0.Applied || !SO2->VisX0.Applied) {
         if (!SUMA_ComputeVisX(SO1, SO2, sv, "VisX0",1)) {
            SUMA_S_Err("Failed to compute or apply overlap avoidance xform");
            SUMA_RETURN(NOPE);
         }
      }
   }
   
   SUMA_LH("Setting remix flag");
   /*set the Color Remix flag */
   if (!SUMA_SetShownLocalRemixFlag (sv)) {
      fprintf (SUMA_STDERR,
               "Error %s: Failed in SUMA_SetShownLocalRemixFlag.\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   /* Get those DOs that are surfaces or graphs*/
   MembSOs = SUMA_ViewState_Membs(&(sv->VSv[nxtstateID]), ttv, &N_MembSOs);
   
   if (!MembSOs || N_MembSOs==0) {
      SUMA_S_Errv("No members found for state %s, what gives?\n",
                 sv->VSv[nxtstateID].Name);
      SUMA_RETURN(NOPE);
   }
   /* if no coloroverlay exists, link to MapReference surface, if possible */
   SUMA_LHv("Have %d memb[SD]Os\n", N_MembSOs);
   for (i=0; i<N_MembSOs; ++i) {
      switch (dov[MembSOs[i]].ObjectType) {
         case SO_type:
      /* next surface being checked */
      SO_nxt = (SUMA_SurfaceObject *)(dov[MembSOs[i]].OP);

      /* Get the Mapping Reference surface, that's the precursor*/
      if (!SO_nxt->LocalDomainParentID) {
         prec_ID = -1;
      }else {
         prec_ID = SUMA_findSO_inDOv(  SO_nxt->LocalDomainParentID, 
                                       SUMAg_DOv, SUMAg_N_DOv);
      }
      if (prec_ID < 0) {
         /* no precursors found, notify user */
         fprintf(SUMA_STDERR, 
                  "\n\aWarning %s: "
                  "No precursors found for surface %d.\n"
                  "Colors, selected nodes and facesets will "
                  "not reflect those in previous state.\n.",
                  FuncName, MembSOs[i]);
         continue;
      }

      SO_prec = (SUMA_SurfaceObject *)(dov[prec_ID].OP);

      /* check for risk of node inconsistencies */

      if (SO_prec->N_Node >= SO_nxt->N_Node ) {/* > or equal number of nodes*/
         /* matching number of nodes */
         if (!SUMA_GetOverlaysFromParent(SO_nxt,SO_prec)) {
            SUMA_SL_Err("Failed to get overlays from parents");
            SUMA_RETURN(NOPE);
         }

         if (SO_prec->N_Node > SO_nxt->N_Node) {/* More in prec */
            /* just warn */
            fprintf(SUMA_STDERR, 
                     "Warning %s: More nodes (%d) in precursor surface. \n"
                     "Assuming upcoming surface is a subset of precursor.\n",
                     FuncName, SO_prec->N_Node - SO_nxt->N_Node);
         }/* More in prec */ 

         /* link the selected nodes and facesets, if possible */
         /*fprintf(SUMA_STDERR, 
                  "%s: Linking selected nodes  ...\n", 
                  FuncName);*/
         /* check for risk of node inconsistencies */
         if (SO_prec->N_Node == SO_nxt->N_Node) {
            SO_nxt->SelectedNode = SO_prec->SelectedNode;
         } else { /*more nodes in precursor, make sure selected node is OK */
         if (SO_prec->SelectedNode < SO_nxt->N_Node) {
            SO_nxt->SelectedNode = SO_prec->SelectedNode;
         } else { /* this node does not exist in the upcoming thing */
            fprintf(SUMA_STDERR, 
                     "\n\aWarning %s: "
                     "Slected node in precursor state does not exist "
                     "in current state.\n"
                     "Selected Node is left at previous setting in "
                     "this view state.\n", FuncName);
            }
         }

      } /* > or equal number of nodes */ else { /* less in prec */
         fprintf(SUMA_STDERR, 
                  "\n\aWarning %s: More nodes (%d) in upcoming surface. "
                  "Colors, selected nodes and facesets are not carried "
                  "through from precursor.\n", 
                  FuncName, SO_nxt->N_Node - SO_prec->N_Node);
      }

            break;
         case GRAPH_LINK_type:
            /* nothing needed */
            break;
         default:
            SUMA_S_Err("Should not be here");
            break;
      }
   
   }
   
   
   /* Bind the cross hair to a reasonable surface, if possible */
   if (iDO_isSO(sv->Ch->adoID)) {      
      if (LocalHead) 
         fprintf( SUMA_STDERR, 
                  "Local Debug %s: Linking Cross Hair via adoID...\n",
                   FuncName);
      j = SUMA_MapRefRelative (sv->Ch->adoID, 
                               MembSOs,
                               N_MembSOs, dov);
      if (LocalHead) fprintf( SUMA_STDERR, 
                              "Local Debug %s: "
                              "Cross Hair's  New adoID = %d\n", 
                              FuncName, j );
      
      /* set the XYZ of the cross hair based on the 
         coordinates of the upcoming surface, if possible */
      if (j >= 0) {
         SO_nxt = (SUMA_SurfaceObject *)(dov[j].OP);
         ND = SO_nxt->NodeDim;
         id = ND * sv->Ch->datumID;
         if (sv->Ch->datumID >= 0) {
            if (LocalHead) fprintf( SUMA_STDERR, 
                                    "Local Debug %s: Using datumID for link.\n",
                                    FuncName);
            sv->Ch->c[0] = SO_nxt->NodeList[id];
            sv->Ch->c[1] = SO_nxt->NodeList[id+1];
            sv->Ch->c[2] = SO_nxt->NodeList[id+2];
         } else {
            /* no node associated with cross hair, use XYZ */
            if (LocalHead) fprintf( SUMA_STDERR, 
                                    "Local Debug %s: Using XYZ for link.\n", 
                                    FuncName);
            SO_prec = (SUMA_SurfaceObject *)(dov[sv->Ch->adoID].OP);
            /* go from XYZ to XYZmap on current surface 
               then from XYZmap to XYZ on new surface */
            I_C = -1;
            XYZmap = SUMA_XYZ_XYZmap (sv->Ch->c, SO_prec, dov, N_dov, &I_C, 1);
            if (XYZmap == NULL) {
               fprintf( SUMA_STDERR, 
                        "Error %s: Failed in SUMA_XYZ_XYZmap\n", 
                        FuncName); 
            }else {
               XYZ = SUMA_XYZmap_XYZ (XYZmap, SO_nxt, dov, N_dov, &I_C, 1);
               if (XYZ == NULL) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: Failed in SUMA_XYZmap_XYZ\n", 
                           FuncName); 
               } else {
                  sv->Ch->c[0] = XYZ[0];
                  sv->Ch->c[1] = XYZ[1];
                  sv->Ch->c[2] = XYZ[2];
               }
               
            }
            if (XYZ) SUMA_free(XYZ);
            if (XYZmap) SUMA_free(XYZmap);
         }
         
         /* if the surface controller is open, update it */
         if (SUMA_isADO_Cont_Realized((SUMA_ALL_DO *)SO_nxt))   { 
            SUMA_Init_SurfCont_SurfParam((SUMA_ALL_DO *)SO_nxt);
         }

      } else {
         fprintf( SUMA_STDERR, 
                  "%s: No relatives between states. "
                  "CrossHair location will not correspond between states\n",
                  FuncName); 
      }
      sv->Ch->adoID = j;
      if (LocalHead) 
         fprintf(SUMA_STDERR, 
                  "Local Debug %s: Linking Cross Hair Via datumID Done.\n",
                  FuncName);
   }
   


   if (sv->FOV[sv->iState] == sv->FOV_original || sv->FOV[sv->iState] < 0) { 
      sv->FOV[sv->iState] = SUMA_sv_auto_fov(sv);
   }
   if (sv->FreezeZoomXstates) 
      sv->FOV[sv->iState] = SUMA_sv_auto_fov(sv)*zfac; 

   /* set the focus ID to the first surface/object in the next view */
   sv->Focus_DO_ID = MembSOs[0];
   
   SUMA_ifree(MembSOs); 
   
   /* Now update the cross hair info if needed for the surface in focus */
   if (sv->Ch->adoID >= 0 && (SOtmp = SUMA_SV_Focus_SO(sv)))   { 
      if (SUMA_isADO_Cont_Realized((SUMA_ALL_DO *)SOtmp)) {
         SUMA_Init_SurfCont_CrossHair((SUMA_ALL_DO *)SOtmp);
      }
   }

   if ((SOtmp = SUMA_SV_Focus_SO(sv)) && LocalHead) {
      fprintf( SUMA_STDERR,
               "%s: Setting new Focus ID to surface %s\n", 
               FuncName, SOtmp->Label);
   }
   
   /* decide what the best state is */
   sv->StdView = SUMA_BestStandardView (sv,dov, N_dov);
   if (LocalHead) fprintf( SUMA_STDOUT,
                           "%s: Standard View Now %d\n", 
                           FuncName, sv->StdView);
   if (sv->StdView == SUMA_N_STANDARD_VIEWS) {
      fprintf( SUMA_STDERR,
               "Error %s: Could not determine the best standard view. "
               "Choosing default SUMA_3D\n", 
               FuncName);
      sv->StdView = SUMA_3D;
   }

   /* modify the rotation center */
   if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,
               "Error %s: Failed to update center of rotation", 
               FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* set the viewing points */
   if (!SUMA_UpdateViewPoint(sv, dov, N_dov, 0)) {
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

   /* do the axis setup */
   SUMA_WorldAxisStandard (sv->WAx, sv);

   /* do the light business */
   if (SOtmp && !SUMA_SetViewerLightsForSO( sv, SOtmp )) {
      SUMA_S_Warn("Failed to set viewer lights.\n"
                  "Use 'F' key to flip lights in SUMA if necessary.");
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
   Call this function whenever you have new geometry in the viewer
   This happens when you switch states or when you modify the coordinates 
   of a surface
   The succession of function calls is replicated in parts elsewhere in the code
   like in the SwitchState function
*/
SUMA_Boolean SUMA_NewGeometryInViewer (SUMA_DO *dov, int N_dov, 
                                       SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_NewGeometryInViewer"};
   SUMA_Axis *EyeAxis;
   int EyeAxis_ID, I_C, OverInd, ND, id;
   char CommString[100];
   SUMA_EngineData ED;
   int  i, j, jmax, prec_ID;
   SUMA_SurfaceObject *SO_nxt, *SO_prec;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;   
   
   /* decide what the best std view is */
   sv->StdView = SUMA_BestStandardView (sv,dov, N_dov);
   if (LocalHead) 
      fprintf(SUMA_STDOUT,"%s: Standard View Now %d\n", FuncName, sv->StdView);
   if (sv->StdView == SUMA_N_STANDARD_VIEWS) {
      fprintf(SUMA_STDERR,
               "Error %s: Could not determine the best standard view."
               " Choosing default SUMA_3D\n", FuncName);
      sv->StdView = SUMA_3D;
   }
   
   /* modify the rotation center */
   if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
      SUMA_S_Err("Failed to update center of rotation");
      SUMA_RETURN (NOPE);
   }
   
   /* set the viewing points */
   if (!SUMA_UpdateViewPoint(sv, dov, N_dov, 0)) {
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
   
   /* Need to update where you're looking at*/
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   gluLookAt ( sv->GVS[sv->StdView].ViewFrom[0], 
               sv->GVS[sv->StdView].ViewFrom[1], 
               sv->GVS[sv->StdView].ViewFrom[2], 
               sv->GVS[sv->StdView].ViewCenter[0], 
               sv->GVS[sv->StdView].ViewCenter[1], 
               sv->GVS[sv->StdView].ViewCenter[2], 
               sv->GVS[sv->StdView].ViewCamUp[0], 
               sv->GVS[sv->StdView].ViewCamUp[1], 
               sv->GVS[sv->StdView].ViewCamUp[2]);
   
   /* do the axis setup */
   SUMA_WorldAxisStandard (sv->WAx, sv);

   /* You still need to call SUMA_display via SUMA_postRedisplay but that is done after this function returns */ 

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

Note that SUMA_postRedisplay may not be good enough because it posts a display request which may not execute immediately. SUMA_handleRedisplay would be the harsher, but more effective method.

Do not try executing all the commands in SUMA_display that affect the modelview 
matrix and the projection matrix without calling for a display the changes will not take effect.

\param dov (SUMA_DO *) Pointer to vector of displayable objects, typically SUMAg_DOv
\param N_dov (int) number of elements in dov, typically SUMAg_N_DOv
\param sv (SUMA_SurfaceViewer *) viewer making the request.
\return YUP/NOPE Good/Bad
\sa SUMA_NewGeometryInViewer

*/
SUMA_Boolean SUMA_OpenGLStateReset (SUMA_DO *dov, int N_dov, 
                                    SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_OpenGLStateReset"};
   SUMA_Axis *EyeAxis;
   int EyeAxis_ID, I_C, OverInd, ND, id;
   char CommString[100];
   SUMA_EngineData ED;
   int  i, j, jmax, prec_ID;
   SUMA_SurfaceObject *SO_nxt, *SO_prec;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;   
   
   #if 0
   /* modify the rotation center */
   if (!SUMA_UpdateRotaCenter(sv, dov, N_dov)) {
      fprintf (SUMA_STDERR,
               "Error %s: Failed to update center of rotation", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* set the viewing points */
   if (!SUMA_UpdateViewPoint(sv, dov, N_dov, 0)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update view point", FuncName);
      SUMA_RETURN (NOPE);
   }
   #endif
   
   /* This is all that is needed, the others above do not need to 
      be updated at this stage*/
   
   /* Change the defaults of the eye axis to fit standard EyeAxis */
   EyeAxis_ID = SUMA_GetEyeAxis (sv, dov);

   if (EyeAxis_ID < 0) {
      /* This happens when only loading objects that are not surfaces.
         the eye axis for such monsters is created later, so return 
         quietly */
      SUMA_LH("No Eye Axis. dealing with non SOs I take it?");
   } else {
      EyeAxis = (SUMA_Axis *)(dov[EyeAxis_ID].OP);
      SUMA_EyeAxisStandard (EyeAxis, sv);
   }
   

   #if 0
   /* force an axis drawing to set the projection matrix correctly */
   SUMA_SET_GL_PROJECTION(sv, sv->ortho);   
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
   
   SUMA_ENTRY;

   for (i=0; i< sv->N_DO; ++i) {
      if (dov[sv->RegistDO[i].dov_ind].ObjectType == AO_type) {
         AO = (SUMA_Axis *)(dov[sv->RegistDO[i].dov_ind].OP);
         if (strcmp(AO->Label, "Eye Axis") == 0) {
            k = sv->RegistDO[i].dov_ind;
            ++cnt;
         }
      }
   }
   if (cnt > 1) {
      fprintf (SUMA_STDERR,
               "Error %s: Found more than one Eye Axis. \n", FuncName);
      SUMA_RETURN (-1);
   }
   
   SUMA_RETURN (k);
}

/*! 
   transform current XYZ to XYZmap 
   The XYZ on an auxilliary surface are of no relevance to the volume. 
   They must be transformed to mappable XYZ (in mm, RAI, in alignment 
   with the Parent Volume)   
   XYZmap = SUMA_XYZ_XYZmap (XYZ, SO, dov, N_dov, I_C, LDP_only);

   \param XYZ (float *) XYZ triplet in SO's native coordinate space
   \param SO (SUMA_SurfaceObject *SO) obvious, ain't it
   \param dov (SUMA_DO*) vector containing all displayable objects
   \param N_dov (int) number of elements in dov
   \param I_C (int *) (pre allocated) pointer to the index of the closest 
               (or representative) node in SO to the XYZ location. 
               If you do not have it, make sure *I_C = -1. 
               If you do so, the function will search for nodes contained in 
               a box mm wide and centered on XYZ. 
               If nodes are found in the box the I_C is set to the
               index of the closest node and XYZmap contains the coordinates 
               of I_C in the SO->LocalDomainParentID surface.
   \param LDP_only (int) if 1, then XYZmap is only the same as XYZ when SO
                               is itself the LDP. 
                         if 0, XYZmap = XYZ is SO is LDP, or if SO is 
                               anatomically correct.
   \ret XYZmap (float *) Mappable XYZ coordinates. NULL in case of trouble.

*/

float * SUMA_XYZ_XYZmap (float *XYZ, SUMA_SurfaceObject *SO, 
                         SUMA_DO* dov, int N_dov, int *I_C, int LDP_only)
{/* SUMA_XYZ_XYZmap */
   static char FuncName[]={"SUMA_XYZ_XYZmap"};
   float *XYZmap;
   int iclosest, id, ND;
   SUMA_SurfaceObject *SOmap;
   int SOmapID;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /* allocate for return */
   XYZmap = (float *)SUMA_calloc (3, sizeof(float));
   if (XYZmap == NULL) {
      fprintf(SUMA_STDERR,
               "Error %s: Could not allocate for XYZmap.\n", FuncName);
      SUMA_RETURN (NULL);
   } 
   
   /* if surface is a local domain parent, do the obivious */
   if (SUMA_isLocalDomainParent(SO)){
      /*SUMA_LH("Surface is a local domain parent. XYZmap = XYZ.\n"); */
      SUMA_COPY_VEC (XYZ, XYZmap, 3, float, float);
      SUMA_RETURN (XYZmap);   
   }
   
   if (!LDP_only && SO->AnatCorrect){ /* Aug. 22 2012 */
      /*SUMA_LH("Surface is anatomocally correct. XYZmap = XYZ.\n"); */
      SUMA_COPY_VEC (XYZ, XYZmap, 3, float, float);
      SUMA_RETURN (XYZmap);   
   }
   
   /* if surface is not a local domain parent and not anatomically correct, 
   do the deed */
   if (!SUMA_ismappable(SO)){
      SUMA_S_Warn("Surface is NOT mappable, returning NULL.");
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
            SUMA_S_Notev("%d nodes (out of %d) found in box\n",
                         IB.nIsIn, SO->N_Node);

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
               SUMA_S_Errv("No node was close enough to XYZ=[%f %f %f],"
                           " no linkage possible\n",
                           XYZ[0], XYZ[1], XYZ[2]);
               SUMA_free(XYZmap);
               SUMA_RETURN (NULL);
            }
            /* store iclosest for lazy user */
            *I_C = iclosest;
         }
   } else { 
      iclosest = *I_C;
   }
   
   SUMA_LHv("Node identified for linking purposes is %d\n", *I_C);
   /* find the SO that is the Mappable cahuna */
   SOmapID = SUMA_findSO_inDOv(SO->LocalDomainParentID, dov, N_dov);
   if (SOmapID < 0) {
      SUMA_S_Err("Failed in SUMA_findSO_inDOv This should not happen.");
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
   
   XYZ = SUMA_XYZmap_XYZ (XYZmap, SO, dov, N_dov, I_C, LDP_only);

   \param XYZmap (float *) XYZmap triplet in SO's MapRef coordinate space
   \param SO (SUMA_SurfaceObject *SO) obvious, ain't it
   \param dov (SUMA_DO*) vector containing all displayable objects
   \param N_dov (int) number of elements in dov
   \param I_C (int *) (pre allocated) pointer to the index of the closest 
                  (or representative) node in SO's MapRef to the XYZmap location. 
                  If you do not have it, make sure *I_C = -1. 
                  If you do so, the function will search for nodes contained in 
                  a box mm wide and centered on XYZmap. 
                  If nodes are found in the box the I_C is set to the index of 
                  the closest node and XYZ contains the coordinates of I_C in the
                  SO surface.
   \param LDP_only (int) if 1, XYZmap is set to XYZ only if SO is the LDP
                            0, XYZmap is set to XYZ is SO is LDP or AnatCorrect 
   \ret XYZ (float *) Equivalent of XYZmap on the auxilliary surface SO. 
                      NULL in case of trouble.

   \sa SUMA_XYZ_XYZmap
*/

float * SUMA_XYZmap_XYZ (float *XYZmap, SUMA_SurfaceObject *SO, SUMA_DO* dov, 
                         int N_dov, int *I_C, int LDP_only)
{/* SUMA_XYZmap_XYZ */
   static char FuncName[]={"SUMA_XYZmap_XYZ"};
   float *XYZ;
   int iclosest, id, ND;
   SUMA_SurfaceObject *SOmap;
   int SOmapID;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

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

   /* if surface is a local domain parent, do the obivious */
   if (SUMA_isLocalDomainParent(SO)){
      SUMA_LH("Surface is a local domain parent. XYZ = XYZmap.\n");
      SUMA_COPY_VEC (XYZmap, XYZ, 3, float, float);
      SOmap = SO;
      /* do not return yet, must fix the node id too */
   } else if (!LDP_only && SO->AnatCorrect) {
      /* surface is anatomicall correct, use it */
      SUMA_LH("Surface is anatomically correct. XYZ = XYZmap.\n");
      SUMA_COPY_VEC (XYZmap, XYZ, 3, float, float);
      SOmap = SO;
      /* do not return yet, must fix the node id too */
   } else {
      /* surface is mappable, things will get more complicated */
      /* find the SO that is the Mappable cahuna */
      SOmapID = SUMA_findSO_inDOv(SO->LocalDomainParentID, dov, N_dov);
      if (SOmapID < 0) {
         SUMA_S_Err("Failed in SUMA_findSO_inDOv This should not happen.");
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
            SUMA_LHv("%d nodes (out of %d) found in box\n",
                     IB.nIsIn, SOmap->N_Node);

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
               if (SO->AnatCorrect == NOPE && SO != SOmap) { 
                              /* used to be if (SO != SOmap) only */
                  SUMA_SL_Warn(  "No node was close enough\n"
                                 "to XYZmap, no linkage possible."   );
                  SUMA_free(XYZ);
                  SUMA_RETURN (NULL);
               } else {
                  /* comes from inherrently mappable stuff, makes sense to 
                     leave XYZ */
                  SUMA_SL_Warn(  "No node was close enough\n"
                                 "to XYZmap, linking by coordinate."   );
                  SUMA_COPY_VEC (XYZmap, XYZ, 3, float, float);
                  SUMA_RETURN (XYZ);
               }
            }
            /* store iclosest for lazy user */
            *I_C = iclosest;
         }
   } else { 
      iclosest = *I_C;
   }
   SUMA_LHv("Node identified for linking purposes is %d\n", *I_C);
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
   Returns the ID (index into dov) of the surface object in Prec_List that 
   is related (via MapRef) to the surface object Cur_ID.
   This means that SOcur.LocalDomainParentID = SOprec.MapRef_icode_str or SOprec.idcode_str

   \param Cur_ID (int) index into dov of the current surface object
   \param Prec_List (int *) indices into dov of the precursor surface objects 
   \param N_Prec_List (int) number of indices in Prec_List
   \param dov (SUMA_DO *) the vector of Displayable Object Structures
   \ret Prec_ID (int) index into dov of the surface object that is related 
         to Cur_ID

*/
int SUMA_MapRefRelative (int cur_id, int *prec_list, int N_prec_list, 
                         SUMA_DO *dov) 
{
   int i, rel_id = -1;
   static char FuncName[]={"SUMA_MapRefRelative"};
   SUMA_SurfaceObject *SOcur, *SO_prec;

   SUMA_ENTRY;

   if (!iDO_isSO(cur_id)) { /* not a surface, return */
      SUMA_RETURN(-1);
   }
   
   SOcur = (SUMA_SurfaceObject *)(dov[cur_id].OP);
   /* if surface has no MapRef then it cannot receive colors from precursors */
   if (!SUMA_ismappable(SOcur)) {
      SUMA_RETURN (-1);
   }
    
   for (i=0; i<N_prec_list; ++i) {
      if (iDO_isSO(prec_list[i])) {
         SO_prec = (SUMA_SurfaceObject *)(dov[prec_list[i]].OP);

         if (  SO_prec == SOcur ||
               strcmp(SOcur->idcode_str, SO_prec->idcode_str) == 0 ) {
            if (N_prec_list == 1) {
               /* if all you have is one surface in one state in SUMA 
                  then you need not worry about the rest */
            } else {
               /* this can happen if you have multiple surfaces with each being 
                  their own mappable surfaces., so it is OK too */
               /*
                  fprintf(SUMA_STDERR, "\nError %s: Flow problem.\n"
                                    "Did not expect identical surfaces \n"
                                    "in this condition (N_prec_list = %d)\n", 
                                    FuncName, N_prec_list);
               SUMA_BEEP;
               */ 
            }
            /* 
            I changed the next condition: 
            if (  strcmp(SOcur->LocalDomainParentID, 
                         SO_prec->LocalDomainParentID) == 0 || 
               strcmp(SOcur->LocalDomainParentID, SO_prec->idcode_str) == 0 )
            to
            if ( SUMA_isRelated_SO(SOcur, SO_prec, 1) )
            The two are the same except for the condition when the two surfaces 
            are identical. So I put in a error message when that would happen and
            I'll deal with it then.
            ZSS Jan 08 04
            */

         }

         if ( SUMA_isRelated_SO(SOcur, SO_prec, 1) ) { 
            /* Change made Jan 08 04, see note above */
            /* there's some relationship here, save it for return */
            if (rel_id < 0) {
               rel_id = prec_list[i];
            } else {
               fprintf (SUMA_STDERR,
                     "Error %s: I did not think that would occur!"
                     " Ignoring other relatives for now.\n", FuncName); 
            }

         }
      }
   }

   SUMA_RETURN (rel_id);

}

/*!
   \brief creates a list of surfaces that are to be sent to AFNI
*/
int *SUMA_FormSOListToSendToAFNI(SUMA_DO *dov, int N_dov, int *N_Send) 
{
   static char FuncName[]={"SUMA_FormSOListToSendToAFNI"};
   int *SendList = NULL, ii, j, s, *is_listed=NULL;
   SUMA_SurfaceObject *SO=NULL;
   int no_need = 0;
   SUMA_SO_SIDE side=SUMA_NO_SIDE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   *N_Send = 0;
   SendList = (int *)SUMA_malloc(N_dov * sizeof(int));
   is_listed = (int *)SUMA_calloc(N_dov,  sizeof(int));
   if (!SendList || !is_listed) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(SendList);
   }
   
   
   for (s=0;s<5; ++s) {
      for (ii=0; ii<N_dov; ++ii) {
         if (SUMA_isSO(dov[ii])) {
            SO = (SUMA_SurfaceObject *)(dov[ii].OP);      
            if (SO->SentToAfni) { no_need = 1; }
            if (  SO->AnatCorrect && !SO->SentToAfni 
                  && SO->VolPar ) {
               if (!SUMA_ExcludeFromSendToAfni(SO)) {
                  switch (s) {
                     case 0:
                        if (  !is_listed[ii] && 
                              SO->Side == SUMA_LEFT && 
                              SUMA_isTypicalSOforVolSurf(SO) ==  -1) { 
                                 SendList[*N_Send] = ii; 
                                 *N_Send = *N_Send + 1; 
                                 is_listed[ii] = 1;}
                        break;
                     case 1:
                        if (  !is_listed[ii] && 
                              SO->Side == SUMA_LEFT && 
                              SUMA_isTypicalSOforVolSurf(SO) ==   1) { 
                                 SendList[*N_Send] = ii; 
                                 *N_Send = *N_Send + 1; 
                                 is_listed[ii] = 1;}
                        break;
                     case 2:
                        if (  !is_listed[ii] && 
                              SO->Side == SUMA_RIGHT && 
                              SUMA_isTypicalSOforVolSurf(SO) == -1) { 
                                 SendList[*N_Send] = ii; 
                                 *N_Send = *N_Send + 1; 
                                 is_listed[ii] = 1;}
                        break;
                     case 3:
                        if (  !is_listed[ii] && 
                              SO->Side == SUMA_RIGHT && 
                              SUMA_isTypicalSOforVolSurf(SO) ==  1) { 
                                 SendList[*N_Send] = ii; 
                                 *N_Send = *N_Send + 1; 
                                 is_listed[ii] = 1;}
                        break;
                     default:
                        if (  !is_listed[ii]) { 
                           SendList[*N_Send] = ii; 
                           *N_Send = *N_Send + 1; 
                           is_listed[ii] = 1;}
                        break;
                  }
               } else {
                  if (s == 0) { 
                     SUMA_S_Notev(
                     "State %s not sent to AFNI per exclusion in env %s\n",
                     SO->State, "SUMA_DoNotSendStates");
                  }
               }
            }
         }
      }
   }
   
   if (*N_Send == 0 && !no_need) {
      /* an indication that nothing can be sent,
         as opposed things have been sent already
         and need not be resent */
      *N_Send = -1;
   }
   SUMA_RETURN(SendList);

}



/* get the output file for plugout info */
static FILE *
SUMA_drive_get_outstream()
{
   char *suma_outfile;

   /* first time in set the output stream to stdout or environment variable */
   if(sumaout==NULL){
      /* get from environment variable if set */
      suma_outfile = my_getenv("SUMA_OUTPLUG");
      if(suma_outfile!=NULL) {
         SUMA_drive_set_outstream(suma_outfile);
      }
   }

   /* if still NULL output stream, set to default of stdout */
   if(sumaout==NULL) sumaout = stdout;

   return(sumaout);
}

/* set the output stream to a file rather than stdout*/
static int
SUMA_drive_set_outstream(char *outfile)
{
   /* if just passed a NULL, reset output to stdout */
   if(outfile==NULL){
      sumaout = stdout;
      return(-1);
   }

   /* check if resetting to stdout by string from plugout command */
   if(strcmp(outfile, "stdout")==0) {
      sumaout = stdout;
      return 0;
   }

    /* make sure this file name is a good one, and open it for append */
   if( THD_filename_ok(outfile) )
      sumaout = fopen(outfile, "a");

   /* something went wrong, so tell user and reset to stdout */
   if(sumaout==NULL){
      fprintf(stderr, "**** couldn't open outfile, resetting to stdout\n");
      sumaout = stdout;
      return(-1);
   }
   else {
    return 0;
   }
}


