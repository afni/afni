#include "SUMA_suma.h"

/* extern SUMA_SurfaceViewer *SUMAg_cSV; */ /* no longer in use Tue Aug 13 15:55:29 EDT 2002 */
extern int SUMAg_N_DOv; 
extern SUMA_DO *SUMAg_DOv;
extern SUMA_CommonFields *SUMAg_CF; 
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv;

/*!
   Return the code for the key that is specified in keyin
*/
int SUMA_KeyPress(char *keyin, char *keynameback)
{
   static char FuncName[]={"SUMA_KeyPress"};
   int nk=0,i=0,nc=0;
   char keyname[100];
   char *key=NULL, c='\0';
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (keynameback) keynameback[0]='\0';
   keyname[0]='\0';
   
   if (!keyin) SUMA_RETURN(XK_VoidSymbol);
   nc = strlen(keyin);
   if (nc<=0) SUMA_RETURN(XK_VoidSymbol);
   
   key = SUMA_append_string("+",keyin);  /* add a + to simplify parsing */   
   
   nc = strlen(key);
   SUMA_LHv("Key now '%s'\n", key);
   
   /* find the last + */
   i = nc-2; /* skip last char, might itself be + */
   while (i >= 0 && key[i] != '+') { --i; }  ++i; /* reposition past last + */
   
   /* copy the rest into keyname */
   nk=0;
   while (i<nc && nk < 10) {
      keyname[nk] = key[i];
      ++i;   
      ++nk;
   }
   keyname[nk] = '\0';
   if (nk > 10 || nk == 0) {
      SUMA_S_Errv("What kind of key is %s!!!\n", key);
      SUMA_RETURN(XK_VoidSymbol);
   }
   SUMA_LHv("Keyname now '%s'\n", keyname);
   if (keynameback) sprintf(keynameback,"%s", keyname);
   
   if (nk == 1) { /* the simple case, add them as needed */
      c = keyname[0];
      SUMA_LHv("c now '%c'\n", c);
      switch(c) {
         case 'b':
            SUMA_RETURN(XK_b);
         case 'B':
            SUMA_RETURN(XK_B);
         case 'g':
            SUMA_RETURN(XK_g);
         case 'G':
            SUMA_RETURN(XK_G);
         case 'm':
            SUMA_RETURN(XK_m);
         case 'M':
            SUMA_RETURN(XK_M);
         case 'n':
            SUMA_RETURN(XK_n);
         case 'N':
            SUMA_RETURN(XK_N);
         case 'p':
            SUMA_RETURN(XK_p);
         case 'P':
            SUMA_RETURN(XK_P);
         case 'r':
            SUMA_RETURN(XK_r);
         case 'R':
            SUMA_RETURN(XK_R);
         case 't':
            SUMA_RETURN(XK_t);
         case 'T':
            SUMA_RETURN(XK_T);
         case 'z':
            SUMA_RETURN(XK_z);
         case 'Z':
            SUMA_RETURN(XK_Z);
         default:
            SUMA_S_Errv("Key '%c' not yet supported, complain to author.\n", c);
            SUMA_RETURN(XK_VoidSymbol);
      }
   } else {
      if (SUMA_iswordsame_ci(keyname,"up") == 1) SUMA_RETURN(XK_Up);
      if (SUMA_iswordsame_ci(keyname,"down") == 1) SUMA_RETURN(XK_Down);
      if (SUMA_iswordsame_ci(keyname,"left") == 1) SUMA_RETURN(XK_Left);
      if (SUMA_iswordsame_ci(keyname,"right") == 1) SUMA_RETURN(XK_Right);
      if (SUMA_iswordsame_ci(keyname,"f1") == 1) SUMA_RETURN(XK_F1);
      if (SUMA_iswordsame_ci(keyname,"f2") == 1) SUMA_RETURN(XK_F2);
      if (SUMA_iswordsame_ci(keyname,"f3") == 1) SUMA_RETURN(XK_F3);
      if (SUMA_iswordsame_ci(keyname,"f4") == 1) SUMA_RETURN(XK_F4);
      if (SUMA_iswordsame_ci(keyname,"f5") == 1) SUMA_RETURN(XK_F5);
      if (SUMA_iswordsame_ci(keyname,"f6") == 1) SUMA_RETURN(XK_F6);
      if (SUMA_iswordsame_ci(keyname,"f7") == 1) SUMA_RETURN(XK_F7);
      if (SUMA_iswordsame_ci(keyname,"f8") == 1) SUMA_RETURN(XK_F8);
      
      SUMA_S_Errv("Key '%s' not yet supported, complain to author.\n", keyname);
      SUMA_RETURN(XK_VoidSymbol);
   }
   SUMA_RETURN(XK_VoidSymbol);
}

#define SUMA_KEY_COMMON {  \
   if (!sv || !key) {   \
      SUMA_S_Err("Null input");  \
      SUMA_RETURN(0);  \
   }  \
   if (!(nc = strlen(key))) {    \
      SUMA_S_Err("Empty key");   \
      SUMA_RETURN(0);   \
   }  \
      \
   SUMA_LHv("Have %s, nc=%d\n", key, nc); \
   if ((k = SUMA_KeyPress(key, keyname)) == XK_VoidSymbol) {   \
      SUMA_S_Errv("KeyPress for %s could not be obtained.\n", key);  \
      SUMA_RETURN(0);  \
   }  \
   SUMA_LHv("Have keyname = %s\n", keyname); \
   if (SUMA_iswordsame_ci(keyname,tk) != 1) {   \
      SUMA_S_Errv("Expecting %s (or lower case version), got %s\n", tk, keyname );  \
      SUMA_RETURN(0);   \
   }  \
}   

#if 0 /* a template to use for various keys , replace CHAR by upper case char and cHaR by lower case*/
int SUMA_CHAR_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_CHAR_Key"};
   char tk[]={"CHAR"}, keyname[100];
   int k, nc;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_CHAR:
         break;
      case XK_cHaR:
         break;
      default:
         SUMA_S_Err("Il ne faut pas ci dessous");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}
#endif

int SUMA_F1_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_F1_Key"};
   char tk[]={"F1"}, keyname[100];
   int k, nc;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F1:
         sv->ShowEyeAxis = !sv->ShowEyeAxis;
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre la");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}
int SUMA_F2_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_F2_Key"};
   char tk[]={"F2"}, keyname[100];
   int k, nc;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F2:
         {
            int *do_id, n_do_id;
            ++sv->ShowWorldAxis; sv->ShowWorldAxis = sv->ShowWorldAxis % SUMA_N_WAX_OPTIONS; 
            sv->ShowMeshAxis = 0; /* used to be = !sv->ShowMeshAxis; ,  Turned off Oct 15 04 , in favor or WorldAxis */
            do_id = SUMA_GetDO_Type(SUMAg_DOv, SUMAg_N_DOv, SO_type, &n_do_id);
            if (n_do_id) {
               while (n_do_id) {
                 ((SUMA_SurfaceObject *)SUMAg_DOv[do_id[n_do_id-1]].OP)->ShowMeshAxis = sv->ShowMeshAxis;
                  --n_do_id;
               }
               SUMA_free(do_id);
            }
         }
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre la haut");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_F3_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_F3_Key"};
   char tk[]={"F3"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F3:
         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleCrossHair, SES_Suma, sv);
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
         if (!SUMA_Engine (&list)) {
               fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
         }
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre over here");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_F4_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_F4_Key"};
   char tk[]={"F4"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F4:
         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleShowSelectedNode, SES_Suma, sv);
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
         if (!SUMA_Engine (&list)) {
               fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
         }
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre over dort");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_F5_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_F5_Key"};
   char tk[]={"F5"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F5:
         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleShowSelectedFaceSet, SES_Suma, sv);
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
         if (!SUMA_Engine (&list)) {
               fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
         }
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre over dort");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_F6_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_F6_Key"};
   char tk[]={"F6"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F6:
         sv->clear_color[0] = 1 - sv->clear_color[0];
         sv->clear_color[1] = 1 - sv->clear_color[1];
         sv->clear_color[2] = 1 - sv->clear_color[2];

         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
         if (!SUMA_Engine (&list)) {
               fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
         }
         break; 
      default:
         SUMA_S_Err("Il ne faut pas etre over dere");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}
int SUMA_F7_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_F7_Key"};
   char tk[]={"F7"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F7:
         ++SUMAg_CF->ColMixMode;
         if (SUMAg_CF->ColMixMode >= SUMA_MAX_MODES) {
            SUMAg_CF->ColMixMode = SUMA_ORIG_MIX_MODE;
         }
         {
            char stmp[200];
            sprintf(stmp,"Using %s color mixing mode.", SUMA_ColMixModeString(SUMAg_CF->ColMixMode)); 
            if (callmode && strcmp(callmode, "interactive") == 0) { SUMA_SLP_Note(stmp); }
            else { SUMA_S_Note(stmp); }
         }

         SUMA_SetAllRemixFlag (SUMAg_SVv, SUMAg_N_SVv);

         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_Suma, NULL);
         if (!SUMA_Engine (&list)) {
               fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
         }
         break; 
      default:
         SUMA_S_Err("Il ne faut pas etre over yonder");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_F8_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_F8_Key"};
   char tk[]={"F8"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F8:
         sv->ortho = !sv->ortho;
         {
            char stmp[200];
            if (sv->ortho) {
               sprintf(stmp,"Using orthographic projection viewing");
               sv->FOV[sv->iState] = sv->FOV[sv->iState] / 2.0;
            } else {
               sprintf(stmp,"Using perspective viewing");
               sv->FOV[sv->iState] = sv->FOV[sv->iState] * 2.0;
            }
            if (callmode && strcmp(callmode, "interactive") == 0) { SUMA_SLP_Note(stmp); }
            else { SUMA_S_Note(stmp); }
         }

         SUMA_SET_GL_PROJECTION(sv, sv->ortho);
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         break; 
      default:
         SUMA_S_Err("Il ne faut pas etre over yonder");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_B_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_B_Key"};
   char tk[]={"B"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_B:
         if ((SUMA_CTRL_KEY(key))){ 
            if (SUMAg_CF->Dev ) {
               sv->Blend_Mode = (sv->Blend_Mode+1)%SUMA_N_BLEND_MODES;
               switch (sv->Blend_Mode) {
                  case SUMA_NO_BLEND:
                     glDisable(GL_BLEND);
                     if (callmode && strcmp(callmode, "interactive") == 0) { 
                           SUMA_SLP_Note ("Blending  disabled."); }
                     else { SUMA_S_Note ("Blending  disabled."); }
                     break;
                  case SUMA_BLEND1:
                     glEnable (GL_BLEND);
                     glBlendFunc(GL_ONE,GL_SRC_ALPHA);
                     if (callmode && strcmp(callmode, "interactive") == 0) { 
                              SUMA_SLP_Note ("Blending mode1."); }
                     else { SUMA_S_Note ("Blending  mode1."); }
                     break;
                  case SUMA_BLEND2:
                     glEnable (GL_BLEND);
                     glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
                     if (callmode && strcmp(callmode, "interactive") == 0) { 
                           SUMA_SLP_Note ("Blending mode2.");}
                     else { SUMA_S_Note ("Blending  mode2."); }
                     break;
                  default:
                     SUMA_SL_Err ("Should not be here");
                     break;
               }
               SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
            }
         } else {
            sv->BF_Cull = (sv->BF_Cull+1)%3;
            if (callmode && strcmp(callmode, "interactive") == 0) { 
                  SUMA_CullOption(sv, "Apply");}
            else { SUMA_CullOption(sv, "Restore");}
            SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
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
            fprintf (SUMA_STDERR, 
                  "Error %s: Failed to register command.\n", FuncName);
         }

         ED = SUMA_InitializeEngineListData (SE_Redisplay);
         if (!SUMA_RegisterEngineListCommand (  list, ED,
                                                SEF_Empty, NULL,
                                                SES_Suma, (void *)sv, NOPE,
                                                SEI_Head, NULL)) {
            fprintf (SUMA_STDERR, 
                  "Error %s: Failed to register command.\n", FuncName);
         }

         if (!SUMA_Engine (&list)) {
            fprintf(SUMA_STDERR, "Error SUMA_input: SUMA_Engine call failed.\n");
         }
         break;            
      default:
         SUMA_S_Err("Il ne faut pas ci dessous");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_G_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_G_Key"};
   char tk[]={"G"}, keyname[100];
   int k=0, nc=-1;
   int inode = -1;
   SUMA_DSET *Dset = NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_OVERLAYS *Sover=NULL;
   char stmp[100]={"\0"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   if (sv->Focus_SO_ID < 0) {
      if (callmode && strcmp(callmode, "interactive") == 0) {
         SUMA_SLP_Err("No surface in focus.\nCannot graph.");
      } else {
         SUMA_S_Err("No surface in focus.\nCannot graph.");
      }
      SUMA_RETURN(0);
   }
   SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
   if (  !SO || !SO->SurfCont || 
         !SO->SurfCont->curColPlane || 
         !SO->SurfCont->curColPlane->dset_link) {
      SUMA_SL_Err("Nothing to graph");
      SUMA_RETURN(0);
   }
   Sover = SO->SurfCont->curColPlane;
   Dset = SO->SurfCont->curColPlane->dset_link;
   inode = SO->SelectedNode;
   if (inode < 0) {
      if (callmode && strcmp(callmode, "interactive") == 0) {
         SUMA_SLP_Warn("No selected node.\nNothing to graph.");
      }else{
         SUMA_S_Warn("No selected node.\nNothing to graph.");
      }
      SUMA_RETURN(1);
   }
   if (SDSET_VECNUM(Dset) < 2) {
      if (callmode && strcmp(callmode, "interactive") == 0) {
         SUMA_SLP_Warn("One or no value per node.\nNothing to graph.");
      }else{
         SUMA_S_Warn("One or no value per node.\nNothing to graph.");
      }
      SUMA_RETURN(1);
   }
   /* do the work */
   switch (k) {
      case XK_g:
         if ((SUMA_CTRL_KEY(key))) {
         } else {
            SUMA_OverlayGraphAtNode(Sover, SO, inode); 
         }
         break;
      case XK_G:
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre ici");
         SUMA_RETURN(0);
         break;
   }
   SUMA_RETURN(1);
}

int SUMA_M_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_M_Key"};
   char tk[]={"M"}, keyname[100];
   int k, nc;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_m:
         if ((SUMA_CTRL_KEY(key))) {
            SUMA_SurfaceObject *SO;
            float fv3[3];
            int it;
            fprintf(SUMA_STDOUT, "%s: Enter shift in mm [RAI] "
                                 "to apply to all mappable surfaces in DOv.\n",
                                 FuncName);
            it = SUMA_ReadNumStdin (fv3, 3);
            if (it > 0 && it < 3) {
               fprintf(SUMA_STDERR,"Error %s: read %d values, expected 3.\n", 
                                    FuncName, it);
               SUMA_RETURN(0);
            }else if (it < 0) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ReadNumStdin.\n", 
                                    FuncName);
               SUMA_RETURN(0);
            }else if (it == 0) {
               fprintf(SUMA_STDERR,"%s: Nothing read.\n", FuncName);
               SUMA_RETURN(0);
            }

            for (it = 0; it < SUMAg_N_DOv; ++it) {
               if (SUMA_isSO_G (SUMAg_DOv[it], sv->CurGroupName)) {
                  SO = (SUMA_SurfaceObject *)SUMAg_DOv[it].OP;
                  if (SUMA_isLocalDomainParent(SO)) {
                     int imax, ii;
                     /* add the shift */
                     fprintf (SUMA_STDERR,
                              "%s: Shifting %s by %f %f %f mm RAI.\n", 
                              FuncName, SO->Label, fv3[0], fv3[1], fv3[2]);
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

            SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         } else {
            sv->GVS[sv->StdView].ApplyMomentum = 
                                 !sv->GVS[sv->StdView].ApplyMomentum;
            SUMA_UpdateViewerTitle(sv);
            if (sv->GVS[sv->StdView].ApplyMomentum) {
                sv->X->MOMENTUMID = XtAppAddTimeOut(  
                                       SUMAg_CF->X->App, 1,   
                                       SUMA_momentum, 
                                       (XtPointer) sv->X->GLXAREA);
                /* wait till user initiates turning */
               sv->GVS[sv->StdView].spinDeltaX = 0; 
               sv->GVS[sv->StdView].spinDeltaY = 0;
               sv->GVS[sv->StdView].translateDeltaX = 0; 
               sv->GVS[sv->StdView].translateDeltaY = 0;
            } else {
               if (sv->X->MOMENTUMID)  {
                  XtRemoveTimeOut(sv->X->MOMENTUMID);
                  sv->X->MOMENTUMID = 0;
               }
            }
         }
         break;
      case XK_M:
         if (SUMA_CTRL_KEY(key) && (SUMA_ALT_KEY(key) || SUMA_APPLE_KEY(key)) ) {            
            #ifndef DONT_USE_MCW_MALLOC
            /* write memtrace results to disk */
            if (!mcw_malloc_enabled()) {
               if (callmode && strcmp(callmode, "interactive") == 0) {
                  SUMA_SLP_Warn("Memory tracing\n"
                               "is not enabled.\n"
                               "Use Help-->MemTrace.");
               } else {
                  SUMA_S_Warn("Memory tracing\n"
                               "is not enabled.\n"
                               "Use Help-->MemTrace.");
               }
               SUMA_RETURN(0);
            } else {
               if (callmode && strcmp(callmode, "interactive") == 0) {
                  SUMA_SLP_Note("Dumping memory tracing\n"
                               "to latest ./malldump.???\n"
                               "file (if possible).");
               } else {
                  SUMA_S_Note("Dumping memory tracing\n"
                               "to latest ./malldump.???\n"
                               "file (if possible).");
               }
               mcw_malloc_dump();
            }
            #else
               if (callmode && strcmp(callmode, "interactive") == 0) {
                  SUMA_SLP_Warn("Sorry, memory tracing\n"
                             "was not enabled at compile.\n"
                             "time. You are out of luck\n"
                             "if using SUN.");
               } else {
                  SUMA_S_Warn("Sorry, memory tracing\n"
                             "was not enabled at compile.\n"
                             "time. You are out of luck\n"
                             "if using SUN.");
               }
               SUMA_RETURNe;
            #endif
         }
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre ici");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}
int SUMA_N_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_N_Key"};
   char tk[]={"N"}, keyname[100];
   int k, nc, it;
   float fv15[15];
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_N:
         break;
      case XK_n:
         if (SUMA_CTRL_KEY(key)) {   
            if (LocalHead) fprintf(SUMA_STDOUT, "%s: Opening a new controller...\n", FuncName);
            /* open a new controller */
            if (!SUMA_X_SurfaceViewer_Create ()) {
               fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_X_SurfaceViewer_Create.\n", FuncName);
               SUMA_RETURN(0);
            }
         } else {
            fprintf(stdout,"BAD IDEA Enter XYZ of center followed by size of Box (enter nothing to cancel):\n");
            it = SUMA_ReadNumStdin (fv15, 6);
            if (it > 0 && it < 6) {
               fprintf(SUMA_STDERR,"Error %s: read %d values, expected 6.\n", FuncName, it);
               SUMA_RETURN(0);
            }else if (it < 0) {
               fprintf(SUMA_STDERR,"Error %s: Error in SUMA_ReadNumStdin.\n", FuncName);
               SUMA_RETURN(0);
            }else if (it == 0) {
               SUMA_RETURN(0);
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
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre ici");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}


/*!
   Execute commands when P or p is pressed
*/
int SUMA_P_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_P_Key"};
   char tk[]={"P"}, keyname[100];
   int k, nc;
   int N_SOlist, SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS];
   SUMA_SurfaceObject *SO = NULL;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;

   /* do the work */
   switch (k) {
      case XK_P:
         sv->PolyMode = SRM_Fill;
         N_SOlist = SUMA_RegisteredSOs(sv, SUMAg_DOv, SOlist);
         for (k=0; k<N_SOlist; ++k) {
            SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SOlist[k]].OP);   
            SO->PolyMode = SRM_ViewerDefault;
            SO->Show = YUP;
         }
         SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         SUMA_SLP_Note("All surfaces displayed as solids");
         break;
      case XK_p:
         sv->PolyMode = ((sv->PolyMode+1) % SRM_N_RenderModes);
         if (sv->PolyMode <= SRM_ViewerDefault) sv->PolyMode = SRM_Fill;

         SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre ici");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

/*!
   Execute commands when R or r is pressed
*/
int SUMA_R_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_R_Key"};
   char tk[]={"R"}, keyname[100], msg[100];
   int k, nc, ii, jj, mm;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;

   /* do the work */
   switch (k) {
      case XK_r:
         if ((SUMA_APPLE_KEY(key) || SUMA_ALT_KEY(key))) {
            sv->X->SetRot_prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, "Center of Rotation X,Y,Z:", 
                                                   "0,0,0",
                                                   sv->X->TOPLEVEL, YUP,
                                                   SUMA_APPLY_BUTTON,
                                                   SUMA_SetRotCenter, (void *)sv,
                                                   NULL, NULL,
                                                   NULL, NULL,
                                                   NULL, NULL,  
                                                   sv->X->SetRot_prmpt);

            sv->X->SetRot_prmpt = SUMA_CreatePromptDialog(sv->X->Title, sv->X->SetRot_prmpt);

         } else if (SUMA_CTRL_KEY(key)) {
            SUMAg_CF->SUMA_SnapshotOverSampling = (SUMAg_CF->SUMA_SnapshotOverSampling +1)%5;
            if (SUMAg_CF->SUMA_SnapshotOverSampling == 0) SUMAg_CF->SUMA_SnapshotOverSampling = 1;
            { 
               sprintf(msg,"Oversampling now set to %d", SUMAg_CF->SUMA_SnapshotOverSampling);
               if (callmode && strcmp(callmode, "interactive") == 0) { SUMA_SLP_Note (msg); }
               else { SUMA_S_Note (msg); }
            }
         } else {
            GLvoid *pixels;
            double rat;
            int oh=-1,ow=-1;
            /* Control for GL_MAX_VIEWPORT_DIMS */
            if (SUMAg_CF->SUMA_SnapshotOverSampling > 1) {
               glGetIntegerv(GL_MAX_VIEWPORT_DIMS,&k);
               mm = SUMA_MAX_PAIR( SUMAg_CF->SUMA_SnapshotOverSampling*sv->X->HEIGHT,
                                   SUMAg_CF->SUMA_SnapshotOverSampling*sv->X->WIDTH);
               if (mm > k) { /* too big, find best new dimesnions */
                  rat = (double)mm/(double)k; /*window shrinking factor to allow for stitching*/
                  SUMA_S_Notev(  "%d/%d (H/W) Too big for oversampling\n"
                                 " reducing resolution by %f.\n", sv->X->HEIGHT, sv->X->WIDTH, rat);
                  /* store original size */
                  ow = sv->X->WIDTH; oh = sv->X->HEIGHT;
                  sv->WindHeight = sv->X->HEIGHT = (int)((double)sv->X->HEIGHT/rat)-1;
                  sv->WindWidth = sv->X->WIDTH = (int)((double)sv->X->WIDTH/rat)-1;
                  SUMA_WidgetResize (sv->X->TOPLEVEL , sv->X->WIDTH, sv->X->HEIGHT);
                  sv->rdc = SUMA_RDC_X_RESIZE;
                  glViewport( 0, 0, 
                                 sv->X->WIDTH, sv->X->HEIGHT);  
                  SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA); 
               } else {
                  SUMA_S_Note("Size OK");
               }
            }
            /* turn off checking for duplicates */
            for (jj=0; jj<SUMAg_CF->SUMA_SnapshotOverSampling; ++jj) {
               for (ii=0; ii<SUMAg_CF->SUMA_SnapshotOverSampling; ++ii) { 
                  if (SUMAg_CF->SUMA_SnapshotOverSampling > 1) {
                     glGetIntegerv(GL_MAX_VIEWPORT_DIMS,&k);
                     if (ii==0 && jj == 0) {
                        SUMA_S_Notev(  "Resampling factor of %d\n"
                                    "If using this feature, the\n"
                                    " sequence of %d images is saved\n"
                                    " temporarily to disk and 'imcat'\n"
                                    " is then used to put the images together.\n"
                                    "(Have ViewPort GL_MAX_VIEWPORT_DIMS of %d\n"
                                    " and max dims needed of %d.)\n",
                                    SUMAg_CF->SUMA_SnapshotOverSampling, 
                                    SUMAg_CF->SUMA_SnapshotOverSampling*SUMAg_CF->SUMA_SnapshotOverSampling,
                                    k,
                                    SUMA_MAX_PAIR( SUMAg_CF->SUMA_SnapshotOverSampling*sv->X->HEIGHT,
                                                   SUMAg_CF->SUMA_SnapshotOverSampling*sv->X->WIDTH)  );
                     } else {
                        /* sometimes you have repeated black areas when oversampling, allow that after very first 'tant' */
                        SNAP_OkDuplicates();
                     }
                     /* start from top left, move to right then go down one row (Row Major, starting on top left ) */
                     glViewport(-ii*sv->X->WIDTH, -(SUMAg_CF->SUMA_SnapshotOverSampling - jj - 1)*sv->X->HEIGHT, 
                                 SUMAg_CF->SUMA_SnapshotOverSampling*sv->X->WIDTH, SUMAg_CF->SUMA_SnapshotOverSampling*sv->X->HEIGHT);
                     SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
                  }
                  pixels = SUMA_grabPixels(1, sv->X->WIDTH, sv->X->HEIGHT);
                  if (pixels) {
                    ISQ_snapsave (sv->X->WIDTH, -sv->X->HEIGHT, (unsigned char *)pixels, sv->X->GLXAREA ); 
                    SUMA_free(pixels);
                  }else {
                     if (callmode && strcmp(callmode, "interactive") == 0) {SUMA_SLP_Err("Failed to record image.");}
                     else { SUMA_S_Err("Failed to record image.");}
                  }
               }
            }
            if (SUMAg_CF->SUMA_SnapshotOverSampling > 1) {  /* Now return the window to its previous size */
               if (ow > 0) {
                  sv->WindHeight = sv->X->HEIGHT = oh;
                  sv->WindWidth = sv->X->WIDTH = ow;
                  SUMA_WidgetResize (sv->X->TOPLEVEL , ow, oh);   
               }
               sv->rdc = SUMA_RDC_X_RESIZE;
               glViewport( 0, 0, 
                           sv->X->WIDTH, sv->X->HEIGHT);
               SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
            }
            if (SUMAg_CF->NoDuplicatesInRecorder) SNAP_NoDuplicates();
            else SNAP_OkDuplicates();
            if (SUMAg_CF->SUMA_SnapshotOverSampling > 1) {
               /* record the image to make life easy on user */
               sprintf(msg,"Writing resultant image\n to HighRes_Suma_tmp.ppm ...");
               if (callmode && strcmp(callmode, "interactive") == 0) { SUMA_SLP_Note (msg); }
               else { SUMA_S_Note (msg); }
               ISQ_snap_png_rng("HighRes_Photo___tmp", 
                              -(SUMAg_CF->SUMA_SnapshotOverSampling*SUMAg_CF->SUMA_SnapshotOverSampling),
                              0);
               system(  "rm -f HighRes_Suma_tmp* >& /dev/null ; "
                        "imcat -prefix HighRes_Suma_tmp HighRes_Photo___tmp* ;"
                        "rm -f HighRes_Photo___tmp* >& /dev/null");
            }
         }
         break;
      case XK_R:
         sv->Record = !sv->Record;
         if (sv->Record) { 
            if (callmode && strcmp(callmode, "interactive") == 0) { SUMA_SLP_Note ("Recording ON"); }
            else { SUMA_S_Note ("Recording ON"); }
         } else { 
            if (callmode && strcmp(callmode, "interactive") == 0) { SUMA_SLP_Note ("Recording OFF"); }
            else { SUMA_S_Note ("Recording OFF");} 
         }
         SUMA_UpdateViewerTitle(sv);
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre ici");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_T_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_T_Key"};
   char tk[]={"T"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_T:
         if (!list) list = SUMA_CreateList();
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_StartListening, SES_Suma, sv);

         if (!SUMA_Engine (&list)) {
               fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
         } 
         break;
      case XK_t:
         if ((SUMA_CTRL_KEY(key))){
               if (callmode && strcmp(callmode, "interactive") == 0) { SUMA_SLP_Note("Forcing a resend of Surfaces to Afni...");}
               else { SUMA_S_Note("Forcing a resend of Surfaces to Afni..."); }
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
      default:
         SUMA_S_Err("Il ne faut pas ci dessous");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

/*!
   Execute commands when Z or z is pressed
*/
int SUMA_Z_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_Z_Key"};
   char tk[]={"Z"}, keyname[100], msg[100];
   int k, nc, ii, jj, mm;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;

   /* do the work */
   switch (k) {
      case XK_Z:
         sv->FOV[sv->iState] /= (1+sv->KeyZoomGain); 
         if (sv->FOV[sv->iState] < FOV_MIN) { 
            SUMA_BEEP; sv->FOV[sv->iState] = FOV_MIN; 
         }
         /*fprintf(stderr,"Zoom in %f\n", sv->FOV[sv->iState]);*/
         /* Now update the zoom compensation variable */
         if (sv->ZoomCompensate) {
            sv->ZoomCompensate = sv->FOV[sv->iState] / SUMA_sv_fov_original(sv);
            if (sv->ZoomCompensate > 1) sv->ZoomCompensate = 1.0; /* weird stuff at zc_fac higher that 1.5 */
            else if (sv->ZoomCompensate < 0.005) sv->ZoomCompensate = 0.005; 
         }
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         break;

      case XK_z:
         sv->FOV[sv->iState] /= (1-sv->KeyZoomGain); 
         if (sv->ortho) { 
            if (sv->FOV[sv->iState] > FOV_MAX/2.0) { SUMA_BEEP; sv->FOV[sv->iState] = FOV_MAX/2.0; }
         } else {
            if (sv->FOV[sv->iState] > FOV_MAX) { SUMA_BEEP; sv->FOV[sv->iState] = FOV_MAX; }
         }
         /*fprintf(stderr,"Zoom out %f\n", sv->FOV[sv->iState]);*/
         /* Now update the zoom compensation variable */
         if (sv->ZoomCompensate) {
            sv->ZoomCompensate = sv->FOV[sv->iState] / SUMA_sv_fov_original(sv);
            if (sv->ZoomCompensate > 1) sv->ZoomCompensate = 1.0; /* weird stuff at zc_fac higher that 1.5 */
            else if (sv->ZoomCompensate < 0.005) sv->ZoomCompensate = 0.005; /* weird stuff cause by integer spin variables! Proper way to handle all this is with float position storage and no recalculation of zc_fac except at zooming.*/ 
         }
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         break;

      default:
         SUMA_S_Err("Il ne faut pas etre la");
         SUMA_RETURN(0);
         break;
   }
   SUMA_RETURN(1);
}

int SUMA_Up_Key(SUMA_SurfaceViewer *sv, char *key, char *caller)
{
   static char FuncName[]={"SUMA_Up_Key"};
   char tk[]={"Up"}, keyname[100];
   int k, nc, ii;
   float ArrowDeltaRot = 0.05; /* The larger the value, the bigger the rotation increment */
   Widget w;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   w = sv->X->GLXAREA;
   /* do the work */
   switch (k) {
      case XK_Up:
            if ((SUMA_CTRL_KEY(key) && SUMA_SHIFT_KEY(key))) {
               float a[3];
               /* Posterior view ctrl+shift+up*/
               /* From top view, rotate by 90 degrees about x axis */
               a[0] = 1.0; a[1] = 0.0; a[2] = 0.0;
               axis_to_quat(a, SUMA_PI/2, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, NULL, NULL);
            }else if (SUMA_SHIFT_KEY(key)) {
               /*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
               sv->GVS[sv->StdView].translateVec[1] +=  (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaY/(float)sv->WindHeight*sv->GVS[sv->StdView].TranslateGain;
               SUMA_postRedisplay(w, NULL, NULL);
            }else if (SUMA_CTRL_KEY(key)){
               /*fprintf (SUMA_STDERR,"%s: Control down\n", FuncName);*/
               /* Top view ctrl+up*/
               float a[3];
               /* Default top view, rotate by nothing */
               a[0] = 1.0; a[1] = 0.0; a[2] = 0.0;
               axis_to_quat(a, 0, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, NULL, NULL);
            }else if (SUMA_AALT_KEY(key)) {
               /*fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
            }else {
               if (LocalHead) fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);
               trackball_Phi(sv->GVS[sv->StdView].deltaQuat, 
                  0.0, -ArrowDeltaRot, /* first point */
                  0.0, ArrowDeltaRot, /* ending x,y */
                  sv->ArrowRotationAngle);
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
               sv->GVS[sv->StdView].spinDeltaY = 2.0*ArrowDeltaRot*sv->WindHeight;
               SUMA_postRedisplay(w, NULL, NULL);
                  
            }
            
            break;
         
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre ici");
         SUMA_RETURN(0);
         break;
   }   
   
   SUMA_RETURN(1);
}
int SUMA_Down_Key(SUMA_SurfaceViewer *sv, char *key, char *caller)
{
   static char FuncName[]={"SUMA_Down_Key"};
   char tk[]={"Down"}, keyname[100];
   int k, nc, ii;
   float ArrowDeltaRot = 0.05; /* The larger the value, the bigger the rotation increment */
   Widget w;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
      
   w = sv->X->GLXAREA;
   /* do the work */
   switch (k) {
      case XK_Down:
            if ((SUMA_CTRL_KEY(key) && SUMA_SHIFT_KEY(key))) {
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
               SUMA_postRedisplay(w, NULL, NULL);
            }else if (SUMA_SHIFT_KEY(key)) {
               /*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
               /*sv->GVS[sv->StdView].translateVec[0] += 0;*/
               sv->GVS[sv->StdView].translateVec[1] -=  (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaY/(float)sv->WindHeight*sv->GVS[sv->StdView].TranslateGain;
               SUMA_postRedisplay(w, NULL, NULL);
            }else if (SUMA_CTRL_KEY(key)){
               /*fprintf (SUMA_STDERR,"%s: Control down\n", FuncName);*/
               /* Inferior view ctrl+down*/
               float a[3];
               /* From top view, rotate by 180 degrees about y axis */
               a[0] = 0.0; a[1] = 1.0; a[2] = 0.0;
               axis_to_quat(a, SUMA_PI, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, NULL, NULL);
            }else if (SUMA_AALT_KEY(key)) {
               /*fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
            }else {
               /*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
               trackball_Phi(sv->GVS[sv->StdView].deltaQuat, 
                  0.0, ArrowDeltaRot, /* first point */
                  0.0, -ArrowDeltaRot, /* ending x,y */
                  sv->ArrowRotationAngle);
               /*fprintf(stdout,"\ncurrentQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", sv->GVS[sv->StdView].currentQuat[i]);} fprintf(stdout,"\n");
               fprintf(stdout,"\ndeltaQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", sv->GVS[sv->StdView].deltaQuat[i]);} fprintf(stdout,"\n");*/
               add_quats (sv->GVS[sv->StdView].deltaQuat, sv->GVS[sv->StdView].currentQuat, sv->GVS[sv->StdView].currentQuat);
               /*fprintf(stdout,"\nnewQuat\n");for (i=0; i<4; ++i) { fprintf(stdout,"%f\t", sv->GVS[sv->StdView].currentQuat[i]);} fprintf(stdout,"\n");*/
               sv->GVS[sv->StdView].spinDeltaX = 0;
               sv->GVS[sv->StdView].spinDeltaY = -2.0*ArrowDeltaRot*sv->WindHeight;
               SUMA_postRedisplay(w, NULL, NULL);
            }
            
            break;
         
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre ici");
         SUMA_RETURN(0);
         break;
   }   
   

   SUMA_RETURN(1);
}
int SUMA_Left_Key(SUMA_SurfaceViewer *sv, char *key, char *caller)
{
   static char FuncName[]={"SUMA_Left_Key"};
   char tk[]={"Left"}, keyname[100];
   int k, nc, ii;
   float ArrowDeltaRot = 0.05; /* The larger the value, the bigger the rotation increment */
   Widget w;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
         
   w = sv->X->GLXAREA;
   /* do the work */
   switch (k) {
      case XK_Left:
            if ((SUMA_CTRL_KEY(key) && SUMA_SHIFT_KEY(key))) {
               /* do nothing about ctrl+shift+this key */
            }else if (SUMA_SHIFT_KEY(key)) {
               /*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
               sv->GVS[sv->StdView].translateVec[0] -= (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaX/(float)sv->WindWidth*sv->GVS[sv->StdView].TranslateGain;
               /*sv->GVS[sv->StdView].translateVec[1] -= 0;*/
               SUMA_postRedisplay(w, NULL, NULL);
            }else if (SUMA_CTRL_KEY(key)){
               float a[3], cQ[4], dQ[4];
               /* From top view, rotate about x 90 degrees.*/ 
               a[0] = 1.0; a[1] = 0.0; a[2] = 0.0;
               axis_to_quat(a, SUMA_PI/2.0, cQ);
               /* then rotate about y 90 degrees */
               a[0] = 0.0; a[1] = 1.0; a[2] = 0.0;
               axis_to_quat(a, SUMA_PI/2.0, dQ);
               /*add and apply rotation*/
               add_quats (dQ, cQ, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, NULL, NULL);
            }else if (SUMA_AALT_KEY(key)) {
               /*fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
            }else {
               /*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
               trackball_Phi(sv->GVS[sv->StdView].deltaQuat, 
                  ArrowDeltaRot, 0.0, /* first point */
                  -ArrowDeltaRot, 0.0, /* ending x,y */
                  sv->ArrowRotationAngle);
               add_quats (sv->GVS[sv->StdView].deltaQuat, sv->GVS[sv->StdView].currentQuat, sv->GVS[sv->StdView].currentQuat);
               sv->GVS[sv->StdView].spinDeltaX = -2.0*ArrowDeltaRot*sv->WindWidth;
               sv->GVS[sv->StdView].spinDeltaY = 0;
               SUMA_postRedisplay(w, NULL, NULL);
            }
            
            break;
         
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre ici");
         SUMA_RETURN(0);
         break;
   }   
   

   SUMA_RETURN(1);
}
int SUMA_Right_Key(SUMA_SurfaceViewer *sv, char *key, char *caller)
{
   static char FuncName[]={"SUMA_Right_Key"};
   char tk[]={"Right"}, keyname[100];
   int k, nc, ii;
   float ArrowDeltaRot = 0.05; /* The larger the value, the bigger the rotation increment */
   Widget w;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
         
   w = sv->X->GLXAREA;
   /* do the work */
   switch (k) {
      case XK_Right:
            if ((SUMA_CTRL_KEY(key) && SUMA_SHIFT_KEY(key))) {
               /* do nothing about ctrl+shift+this key */
            }else if (SUMA_SHIFT_KEY(key)) {
               /*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
               sv->GVS[sv->StdView].translateVec[0] += (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaX/(float)sv->WindWidth*sv->GVS[sv->StdView].TranslateGain;
               /*sv->GVS[sv->StdView].translateVec[1] -= 0;*/
               SUMA_postRedisplay(w,  NULL, NULL);
            }else if (SUMA_CTRL_KEY(key)){
               float a[3], cQ[4], dQ[4];
               /* From top view, rotate about x 90 degrees */ 
               a[0] = 1.0; a[1] = 0.0; a[2] = 0.0;
               axis_to_quat(a, SUMA_PI/2.0, cQ);
               /* then rotate about y -90 degrees */
               a[0] = 0.0; a[1] = 1.0;
               axis_to_quat(a, -SUMA_PI/2.0, dQ);
               /*add and apply rotation*/
               add_quats (dQ, cQ, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, NULL, NULL);
            }else if (SUMA_AALT_KEY(key)) {
               /*fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);*/
            }else {
               /*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
               trackball_Phi(sv->GVS[sv->StdView].deltaQuat, 
                  -ArrowDeltaRot, 0.0, /* first point */
                  ArrowDeltaRot, 0.0, /* ending x,y */
                  sv->ArrowRotationAngle);
               add_quats (sv->GVS[sv->StdView].deltaQuat, sv->GVS[sv->StdView].currentQuat, sv->GVS[sv->StdView].currentQuat);
               sv->GVS[sv->StdView].spinDeltaX = 2.0*ArrowDeltaRot*sv->WindWidth;
               sv->GVS[sv->StdView].spinDeltaY = 0;
               SUMA_postRedisplay(w,  NULL, NULL);
            }
            
            break;
         
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre ici");
         SUMA_RETURN(0);
         break;
   }   
   

   SUMA_RETURN(1);
}
/*! Mouse and Keyboard input handler function for SUMA's viewer*/

void SUMA_input(Widget w, XtPointer clientData, XtPointer callData)
{
   static char FuncName[]= {"SUMA_input"};
   GLwDrawingAreaCallbackStruct *cd;
   char buffer[10], cbuf = '\0', cbuf2='\0';
   KeySym keysym;
   int xls, ntot, id = 0, ND, ip, NP;
   float ArrowDeltaRot = 0.05; /* The larger the value, the bigger the rotation increment */
   SUMA_EngineData *ED = NULL; 
   char CommString[SUMA_MAX_COMMAND_LENGTH];
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
   float bevx, bevy, mevx, mevy, wwid, whei, zc_fac;
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL; /* Use this only to create prompt that are not to be preserved */
   SUMA_Boolean LocalHead = NOPE; /* local debugging messages */

   /*float ft;
   int **im, iv15[15];*/ /* keep unused variables undeclared to quite compiler */

   SUMA_ENTRY;
   
   /* get the callData pointer */
   cd = (GLwDrawingAreaCallbackStruct *) callData;
   
   /* find out who's calling, only GLXAREA calls this function */
   SUMA_GLXAREA_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, "Error %s: Failed in macro SUMA_GLXAREA_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }
   if (LocalHead) fprintf (SUMA_STDERR,"%s: A call from SUMA_SurfaceViewer[%d], Pointer %p\n", FuncName, isv, sv);
   

   Kev = *(XKeyEvent *) &cd->event->xkey; /* RickR's suggestion to comply with ANSI C, no type casting of structures  July 04*/
   Bev = *(XButtonEvent *) &cd->event->xbutton;
   Mev = *(XMotionEvent *) &cd->event->xmotion;
   
   /* a sample keypresses */
   #if 0
      if (Kev.state & ShiftMask) {
         fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);
      }
      if (Kev.state & ControlMask){
         fprintf (SUMA_STDERR,"%s: Control down\n", FuncName);
      }
      if (Kev.state & Mod1Mask){
         fprintf (SUMA_STDERR,"%s: alt down\n", FuncName);
      }
      if (Kev.state & Mod2Mask){
         fprintf (SUMA_STDERR,"%s: Mod2 down (apple on mac)\n", FuncName);
      }
      if (Kev.state & Mod3Mask){
         fprintf (SUMA_STDERR,"%s: Mod3 down\n", FuncName);
      }
      if (Kev.state & Mod4Mask){
         fprintf (SUMA_STDERR,"%s: Mod4 down\n", FuncName);
      }
      if (Kev.state & Mod5Mask){
         fprintf (SUMA_STDERR,"%s: Mod5 down\n", FuncName);
      }
      fprintf (SUMA_STDERR,"\n\n");
   #endif
   
  switch (Kev.type) { /* switch event type */
  case KeyPress:
      xls = XLookupString((XKeyEvent *) cd->event, buffer, 8, &keysym, NULL);
      
      /* XK_* are found in keysymdef.h */ 
      switch (keysym) { /* keysym */
         case XK_bracketleft: /* The left bracket */
            /* toggle showing left hemispheres */
            {
               #if 0
               int Registered_IDs[SUMAg_N_DOv], N_RegisteredSOs, k;
               SUMA_SurfaceObject *SO = NULL;

                  N_RegisteredSOs = SUMA_RegisteredSOs (sv, SUMAg_DOv, Registered_IDs);
                  for (k=0; k< N_RegisteredSOs; ++k) {
                     SO = (SUMA_SurfaceObject *)SUMAg_DOv[Registered_IDs[k]].OP;
                     if (SO->Side == SUMA_LEFT) SO->Show = !SO->Show;
                  }
               #endif
               sv->ShowLeft = !sv->ShowLeft;
            }
            /* do the axis setup */
            SUMA_WorldAxisStandard (sv->WAx, sv);
            SUMA_UpdateViewerTitle(sv);   
            SUMA_postRedisplay(w, clientData, callData);
            break;
         
         case XK_bracketright: /* The left bracket */
            /* toggle showing left hemispheres */
            {
               sv->ShowRight = !sv->ShowRight;
            }
            /* do the axis setup */
            SUMA_WorldAxisStandard (sv->WAx, sv);            
            SUMA_UpdateViewerTitle(sv);   
            SUMA_postRedisplay(w, clientData, callData);
            break;
                    
         case XK_space:   /* The spacebar. */
            /* toggle between state containing mapping reference of SO in focus and other view */
            {
               SUMA_SurfaceObject *SO = NULL, *SOmap = NULL;
               int curstateID = -1, nxtstateID = -1, dov_ID = -1;

               /* make sure switching is OK */
               curstateID = SUMA_WhichState(sv->State, sv, sv->CurGroupName);
               SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
               if (SUMA_isLocalDomainParent (SO)) {
                  /* get the last non mappable state in SV */
                  if (sv->LastNonMapStateID < 0) { /* not recorded, complain and quit */
                     fprintf(SUMA_STDERR,"Warning %s: Nothing defined to toggle with yet.\n", FuncName); 
                     break;
                  }
                  
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,"%s: surface is inherrently mappable, switching to last non mappable state %d.\n", \
                        FuncName, sv->LastNonMapStateID);
                        
                  if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, sv, sv->LastNonMapStateID, sv->CurGroupName)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SwitchState.\n", FuncName);
                     break;
                  }

               } else {/* that's a non mappable, go to state containing reference */
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,"%s: surface is not inherrently mappable, searching for mapping reference and its state.\n", \
                        FuncName);
                        
                  /* find SO that is mappable reference & get corresponding state ID*/
                  dov_ID = SUMA_findSO_inDOv(SO->LocalDomainParentID, SUMAg_DOv, SUMAg_N_DOv);
                  SOmap = (SUMA_SurfaceObject *)SUMAg_DOv[dov_ID].OP;
                  nxtstateID = SUMA_WhichState(SOmap->State, sv, sv->CurGroupName);
                  
                  if (nxtstateID < 0) {
                     fprintf (SUMA_STDERR,"%s: Failed in SUMA_findSO_inDOv This should not happen.\n", FuncName);
                     break;
                  }
                  
                  if (LocalHead) 
                     fprintf (SUMA_STDERR,"%s: Found mapping reference in viewer state %d.\n", FuncName, nxtstateID);
                     
                  /* store this location */
                  sv->LastNonMapStateID = curstateID;

                  /* go there */
                  if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, sv, nxtstateID, sv->CurGroupName)) {
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
               if( SUMAg_CF->X->WarnClose) {
                  if (SUMA_ForceUser_YesNo(sv->X->TOPLEVEL, "Close All Viewers?", SUMA_YES, SWP_DONT_CARE) != SUMA_YES) {
                     break;   
                  }
               } 
               XtCloseDisplay( SUMAg_CF->X->DPY_controller1 ) ;
               exit(0);
            }else { 
               if( SUMAg_CF->X->WarnClose) {
                  #ifdef DARWIN
                     if (SUMA_ForceUser_YesNo(sv->X->TOPLEVEL, 
                                             "Close This Viewer?\n"
                                             "OS-X users: If answering YES,\n"
                                             "this prompt should not lie \n"
                                             "over viewer to be closed.\n"
                                             "Blame Bill Gates for this bug.",
                                              SUMA_YES, SWP_TOP_RIGHT) != SUMA_YES) {
                        break;   
                     }
                  #else
                     if (SUMA_ForceUser_YesNo(sv->X->TOPLEVEL, "Close This Viewer?", SUMA_YES, SWP_DONT_CARE) != SUMA_YES) {
                        break;   
                     }
                  #endif
               }
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
                  if (SUMA_isSO_G(SUMAg_DOv[sv->RegisteredDO[ii]], sv->Group)) {
                     SO = (SUMA_SurfaceObject*)SUMAg_DOv[sv->RegisteredDO[ii]].OP;
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
               if (( Kev.state & ControlMask)){ 
                  if (SUMAg_CF->Dev ) {
                     if (!SUMA_B_Key(sv, "ctrl+B", "interactive")) {
                        SUMA_S_Err("Failed in key func.");
                     }
                  }
               } else {
                  if (!SUMA_B_Key(sv, "B", "interactive")) {
                     SUMA_S_Err("Failed in key func.");
                  }
               }
            break;

         case XK_b:
            if (!SUMA_B_Key(sv, "b", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;            

         case XK_C:
            if (SUMAg_CF->Dev && (Kev.state & Mod1Mask || Kev.state & Mod2Mask)){
               SUMAg_CF->X->ClipObj_prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, "Enter object clip plane parameters (a,b,c,d)", 
                                                      "A: 0,0,1,0",
                                                      sv->X->TOPLEVEL, YUP,
                                                      SUMA_APPLY_BUTTON,
                                                      SUMA_SetObjectClip, (void *)sv,
                                                      NULL, NULL,
                                                      NULL, NULL,
                                                      NULL, NULL,  
                                                      SUMAg_CF->X->ClipObj_prmpt);
               
               SUMAg_CF->X->ClipObj_prmpt = SUMA_CreatePromptDialog("Enter object clip plane parameters (a,b,c,d)", SUMAg_CF->X->ClipObj_prmpt);
            } else if (SUMAg_CF->Dev && (Kev.state & ControlMask)){
               SUMAg_CF->X->Clip_prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, "Enter screen clip plane parameters (a,b,c,d)", 
                                                      "A: 0,0,1,0",
                                                      sv->X->TOPLEVEL, YUP,
                                                      SUMA_APPLY_BUTTON,
                                                      SUMA_SetScreenClip, (void *)sv,
                                                      NULL, NULL,
                                                      NULL, NULL,
                                                      NULL, NULL,  
                                                      SUMAg_CF->X->Clip_prmpt);
               
               SUMAg_CF->X->Clip_prmpt = SUMA_CreatePromptDialog("Enter screen clip plane parameters (a,b,c,d)", SUMAg_CF->X->Clip_prmpt);
            }
            break; 
         case XK_c:
            {   
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
               if (Kev.state & Mod1Mask || Kev.state & Mod2Mask){ /*  Mod1Mask is alt in linux, Mod2Mask is the apple on mac*/
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
               sv->X->HighlightBox_prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, 
                                                      "Enter XYZ of box's center\n"
                                                      "followed by it's size (6 values)", 
                                                      "",
                                                      sv->X->TOPLEVEL, YUP,
                                                      SUMA_APPLY_BUTTON,
                                                      SUMA_HighlightBox, (void *)sv,
                                                      NULL, NULL,
                                                      NULL, NULL,
                                                      SUMA_CleanNumString, (void*)6,  
                                                      sv->X->HighlightBox_prmpt);
               
               sv->X->HighlightBox_prmpt = SUMA_CreatePromptDialog(sv->X->Title, sv->X->HighlightBox_prmpt);
               
            break;
         case XK_g:
            if (Kev.state & ControlMask){
                  if (SUMAg_CF->Dev) {
                     if (!SUMA_G_Key(sv, "ctrl+g", "interactive")) {
                        SUMA_S_Err("Failed in key func.");
                     }
                  }
               } else {
                  if (!SUMA_G_Key(sv, "g", "interactive")) {
                     SUMA_S_Err("Failed in key func.");
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
                  SUMA_SLP_Note("Please use ctrl+h for help.\nh alone will be reassigned\nin future versions.");
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
               if (Kev.state & ControlMask){     
                 sv->X->JumpXYZ_prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, 
                                                      "Enter XYZ to send the cross hair to:", 
                                                      "",
                                                      sv->X->TOPLEVEL, YUP,
                                                      SUMA_APPLY_BUTTON,
                                                      SUMA_JumpXYZ, (void *)sv,
                                                      NULL, NULL,
                                                      NULL, NULL,
                                                      SUMA_CleanNumString, (void*)3,  
                                                      sv->X->JumpXYZ_prmpt);
               
                  sv->X->JumpXYZ_prmpt = SUMA_CreatePromptDialog(sv->X->Title, sv->X->JumpXYZ_prmpt);  

               } else if (Kev.state & Mod1Mask || Kev.state & Mod2Mask){     
                  sv->X->JumpFocusNode_prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, 
                                                      "Enter index of focus node\nCross hair's XYZ will not be affected:", 
                                                      "",
                                                      sv->X->TOPLEVEL, YUP,
                                                      SUMA_APPLY_BUTTON,
                                                      SUMA_JumpFocusNode, (void *)sv,
                                                      NULL, NULL,
                                                      NULL, NULL,
                                                      SUMA_CleanNumString, (void*)1,  
                                                      sv->X->JumpFocusNode_prmpt);
               
                  sv->X->JumpFocusNode_prmpt = SUMA_CreatePromptDialog(sv->X->Title, sv->X->JumpFocusNode_prmpt);
                  
               } else {
                  sv->X->JumpIndex_prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, 
                                                      "Enter index of node \nto send the cross hair to:", 
                                                      "",
                                                      sv->X->TOPLEVEL, YUP,
                                                      SUMA_APPLY_BUTTON,
                                                      SUMA_JumpIndex, (void *)sv,
                                                      NULL, NULL,
                                                      NULL, NULL,
                                                      SUMA_CleanNumString, (void*)1,  
                                                      sv->X->JumpIndex_prmpt);
               
                  sv->X->JumpIndex_prmpt = SUMA_CreatePromptDialog(sv->X->Title, sv->X->JumpIndex_prmpt);
               }

            break;
         
         case XK_J:
               sv->X->JumpFocusFace_prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, 
                                                   "Enter index of FaceSet\nto highlight (this viewer only):", 
                                                   "",
                                                   sv->X->TOPLEVEL, YUP,
                                                   SUMA_APPLY_BUTTON,
                                                   SUMA_JumpFocusFace, (void *)sv,
                                                   NULL, NULL,
                                                   NULL, NULL,
                                                   SUMA_CleanNumString, (void*)1,  
                                                   sv->X->JumpFocusFace_prmpt);

               sv->X->JumpFocusFace_prmpt = SUMA_CreatePromptDialog(sv->X->Title, sv->X->JumpFocusFace_prmpt);
               
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
            } if (Kev.state & Mod1Mask  || Kev.state & Mod2Mask){ /* alt + l */
               /* register cross hair XYZ with ED */
               if (!list) list = SUMA_CreateList();
               ED = SUMA_InitializeEngineListData (SE_SetLookAt);
               if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                      SEF_fv3, (void *)sv->Ch->c, 
                                                      SES_Suma, (void *)sv, NOPE, 
                                                      SEI_Head, NULL )) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
                  SUMA_RETURNe;
               }
               if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
               }   
            } else {
               sv->X->LookAt_prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, "X,Y,Z coordinates to look at:", 
                                                      "0,0,0",
                                                      sv->X->TOPLEVEL, YUP,
                                                      SUMA_APPLY_BUTTON,
                                                      SUMA_LookAtCoordinates, (void *)sv,
                                                      NULL, NULL,
                                                      NULL, NULL,
                                                      SUMA_CleanNumString, (void*)3,  
                                                      sv->X->LookAt_prmpt);
               
               sv->X->LookAt_prmpt = SUMA_CreatePromptDialog(sv->X->Title, sv->X->LookAt_prmpt);
               
            }
            break;

         case XK_L:
               if ((Kev.state & ControlMask)){
                  if (SUMAg_CF->Dev) {
                     GLfloat light0_color[] = { SUMA_LIGHT0_COLOR_INIT};
                     sv->dim_spe = sv->dim_spe * 0.8; if (sv->dim_spe < 0.1) sv->dim_spe = 1.0;
                     sv->dim_dif = sv->dim_dif * 0.8; if (sv->dim_dif < 0.1) sv->dim_dif = 1.0;
                     sv->dim_amb = sv->dim_amb * 0.8; if (sv->dim_amb < 0.1) sv->dim_amb = 1.0;
                     sv->dim_emi = sv->dim_emi * 0.8; if (sv->dim_emi < 0.1) sv->dim_emi = 1.0;
                     /* dim the lights */
                     fprintf(SUMA_STDERR,"%s:  light dim factor now %.3f\n", FuncName, sv->dim_spe);
                     /*fprintf(SUMA_STDERR,"%s:  light dim factor now %.3f\n%f %f %f %f\n", FuncName, sv->dim_spe,
                                                         sv->light0_color[0], sv->light0_color[1], sv->light0_color[2], sv->light0_color[3]);
                                                         */
                     light0_color[0] = sv->light0_color[0]*sv->dim_spe;
                     light0_color[1] = sv->light0_color[1]*sv->dim_spe;
                     light0_color[2] = sv->light0_color[2]*sv->dim_spe;
                     light0_color[3] = sv->light0_color[3]*sv->dim_spe;
                     glLightfv(GL_LIGHT0, GL_SPECULAR, light0_color);
                     light0_color[0] = sv->light0_color[0]*sv->dim_dif;
                     light0_color[1] = sv->light0_color[1]*sv->dim_dif;
                     light0_color[2] = sv->light0_color[2]*sv->dim_dif;
                     light0_color[3] = sv->light0_color[3]*sv->dim_dif;
                     glLightfv(GL_LIGHT0, GL_DIFFUSE, light0_color);
                     light0_color[0] = sv->lmodel_ambient[0]*sv->dim_amb;
                     light0_color[1] = sv->lmodel_ambient[1]*sv->dim_amb;
                     light0_color[2] = sv->lmodel_ambient[2]*sv->dim_amb;
                     light0_color[3] = sv->lmodel_ambient[3]*sv->dim_amb;
                     glLightModelfv(GL_LIGHT_MODEL_AMBIENT, sv->lmodel_ambient);
                     if (!list) list = SUMA_CreateList(); 
                     SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);

                     if (!SUMA_Engine (&list)) {
                        fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
                     }
                  }
               } else {
                  prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, "X,Y,Z coordinates of light0:", 
                                                         "",
                                                         sv->X->TOPLEVEL, NOPE,
                                                         SUMA_APPLY_BUTTON,
                                                         SUMA_SetLight0, (void *)sv,
                                                         NULL, NULL,
                                                         NULL, NULL,
                                                         SUMA_CleanNumString, (void*)3,  
                                                         NULL);

                  prmpt = SUMA_CreatePromptDialog(sv->X->Title, prmpt);
               }
               
            break;
         
         case XK_M:
            if ((Kev.state & Mod1Mask || Kev.state & Mod2Mask) && (Kev.state & ControlMask) ){
                  if (!SUMA_M_Key(sv, "alt+ctrl+M", "interactive")) {
                     SUMA_S_Err("Failed in key func.");
                  }
            }
            break;
            
         case XK_m:
               if (Kev.state & ControlMask){
                  if (SUMAg_CF->Dev) {
                     if (!SUMA_M_Key(sv, "ctrl+m", "interactive")) {
                        SUMA_S_Err("Failed in key func.");
                     }
                  }
               } else {
                  if (!SUMA_M_Key(sv, "m", "interactive")) {
                     SUMA_S_Err("Failed in key func.");
                  }
               }
             break;

         case XK_n:
               if (Kev.state & ControlMask){
                  if (!SUMA_N_Key(sv, "ctrl+n", "interactive")) {
                     SUMA_S_Err("Failed in key func.");
                  }
               }else {
                  if (SUMAg_CF->Dev) {
                     if (!SUMA_N_Key(sv, "n", "interactive")) {
                        SUMA_S_Err("Failed in key func.");
                     }
                  }
               }
            break;

         case XK_p:
            if (!SUMA_P_Key(sv, "p", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;

         case XK_P:
            if (!SUMA_P_Key(sv, "P", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
         
         case XK_r:
            if (SUMAg_CF->Dev && (Kev.state & Mod1Mask || Kev.state & Mod2Mask)) {
               if (!SUMA_R_Key(sv, "alt+r", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            } if (Kev.state & ControlMask){
               if (!SUMA_R_Key(sv, "ctrl+r", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            } else {
               if (!SUMA_R_Key(sv, "r", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            }
            break;

         case XK_R:
            if (!SUMA_R_Key(sv, "R", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
            }
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
            if ((Kev.state & Mod1Mask || Kev.state & Mod2Mask) && (Kev.state & ControlMask) ){
               if (!list) list = SUMA_CreateList();
               ED = SUMA_InitializeEngineListData (SE_LoadSegDO);
               if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, sv->X->TOPLEVEL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL)) {
                  fprintf (SUMA_STDERR, "Error %s: Failed to register command.\n", FuncName);
               }
               if (!SUMA_Engine (&list)) {
                     fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
               }               
   
            } else if (Kev.state & Mod1Mask || Kev.state & Mod2Mask){
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
                  if (SUMA_isSO(SUMAg_DOv[sv->RegisteredDO[ii]])) 
                     SUMA_Print_Surface_Object((SUMA_SurfaceObject*)SUMAg_DOv[sv->RegisteredDO[ii]].OP, stdout);
               }
               #endif
            }
            break;

         case XK_t:
            if ((Kev.state & ControlMask)){
               if (!SUMA_T_Key(sv, "ctrl+t", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               } 
            } else {
               if (!SUMA_T_Key(sv, "t", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               } 
            }
            break;
         
         case XK_T:
            if (!SUMA_T_Key(sv, "T", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            } 
            break;
            
         case XK_v:
            #if 0
            /*** No longer in use, Jan 03 2004 */
            if (SUMAg_CF->Dev) {
               SUMA_Show_SurfaceViewer_Struct (sv, stdout, 0);
            }
            #endif
            break;

         case XK_W:
            {
               SUMA_SurfaceObject *SO;
               
               SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
               if (SO) {
                  if (!list) list = SUMA_CreateList();
                  ED = SUMA_InitializeEngineListData (SE_SaveSOFileSelection);
                  if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                                   SEF_vp, (void *)SO,
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
               }
            }
            break;

         case XK_w:
            SUMA_SLP_Warn("Option 'w' no longer supported.\nUse 'R' or 'r' instead.");
            #if 0
               fprintf(SUMA_STDOUT,"%s: Began rendering to file. Please wait ...\n", FuncName);
               if (!SUMA_RenderToPixMap (sv, SUMAg_DOv)) {
                  fprintf(SUMA_STDERR, "Error %s: Failed to write image.\n", FuncName);
               } 
            #endif
            break;

         case XK_Z:
            if (!SUMA_Z_Key(sv, "Z", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;

         case XK_z:
            if (!SUMA_Z_Key(sv, "z", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;

         case XK_3:
            sv->Do_3Drender =  !sv->Do_3Drender;
            SUMA_S_Notev("wtf!!!!!!!!!!!!!!%d!!!!!!!\n", sv->Do_3Drender);
            break;
      
         case XK_8:
            {
               char stmp[100];
               sprintf(stmp, "%d", SUMAg_CF->X->NumForeSmoothing);
               SUMAg_CF->X->N_ForeSmooth_prmpt = SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, "Foreground smoothing iterations", 
                                                         stmp,
                                                         sv->X->TOPLEVEL, YUP,
                                                         SUMA_APPLY_BUTTON,
                                                         SUMA_SetNumForeSmoothing, (void *)sv,
                                                         NULL, NULL,
                                                         NULL, NULL,
                                                         SUMA_CleanNumString, (void*)1,  
                                                         SUMAg_CF->X->N_ForeSmooth_prmpt);

               SUMAg_CF->X->N_ForeSmooth_prmpt = SUMA_CreatePromptDialog("Foreground smoothing iterations", SUMAg_CF->X->N_ForeSmooth_prmpt);
            }
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

                  attr_sm = SUMA_SmoothAttr_Neighb (attrbuf, SO->N_Node, NULL, SO->FN, 1, NULL, 1); 
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
                  SO->SC = SUMA_Surface_Curvature (SO->NodeList, SO->N_Node, SO->NodeNormList, SO->PolyArea, 
                                                   SO->N_FaceSet, SO->FN, SO->EL, "Curvs_c.txt", 1);
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
               int MapType;
               SUMA_COLOR_SCALED_VECT * SV;
               float IntRange[2], *Vsort;
               float * attr_sm;
               float *Cx = NULL;

               fprintf(SUMA_STDOUT, "%s: Calculating convexity ...\n", FuncName);
               SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;   
               Cx = (float *)SUMA_GetCx(SO->idcode_str, SUMAg_CF->DsetList, 0);
               if (Cx) {
                  fprintf(stderr,"Error %s: Cx must be null prior to new assignment\n", FuncName);
                  break;
               }
               Cx = SUMA_Convexity   (SO->NodeList, SO->N_Node, SO->NodeNormList, SO->FN);   
               if (Cx == NULL) {
                     fprintf(stderr,"Error %s: Failed in SUMA_Convexity\n", FuncName);
                     break;
               }   
               /* smooth estimate twice */
               attr_sm = SUMA_SmoothAttr_Neighb (Cx, SO->N_Node, NULL, SO->FN, 1, NULL, 1);
               if (attr_sm == NULL) {
                     fprintf(stderr,"Error %s: Failed in SUMA_SmoothAttr_Neighb\n", FuncName);
                     break;
               }   
               Cx = SUMA_SmoothAttr_Neighb (attr_sm, SO->N_Node, Cx, SO->FN, 1, NULL, 1);
               if (attr_sm) SUMA_free(attr_sm);

               fprintf(SUMA_STDOUT, "%s: Use SUMA_ScaleToMap to colorize Conv.txt and display it on surface.\n", FuncName);
               CM = SUMA_FindNamedColMap ("ngray20");
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
               IntRange[0] = 5; IntRange[1] = 95; /* percentile clipping range*/ 
               Vsort = SUMA_PercRange (Cx, NULL, SO->N_Node, IntRange, IntRange, NULL); 
               OptScl->IntRange[0] = IntRange[0]; OptScl->IntRange[1] = IntRange[1];

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
                  if (!SUMA_ScaleToMap (Cx, SO->N_Node, Vsort[0], Vsort[SO->N_Node-1], CM, OptScl, SV)) {
                     fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_ScaleToMap.\n", FuncName);
                     exit(1);
                  }

                  /* Now place SV in the color array */
                  glar_ColorList = SUMA_GetColorList (sv, SO->idcode_str);
                  if (!glar_ColorList) {
                     fprintf (SUMA_STDERR,"Error %s: NULL glar_ColorList. BAD.\n", FuncName);
                     break;
                  }  
                  SUMA_RGBvec_2_GLCOLAR4(SV->cV, glar_ColorList, SO->N_Node);

                  /* free */
                  if (Vsort) SUMA_free(Vsort);
                   if (OptScl) SUMA_free(OptScl);
                  if (SV) SUMA_Free_ColorScaledVect (SV);
                  if (Cx) {
                     SUMA_free(Cx);
                     Cx = NULL;
                  }

               fprintf(SUMA_STDOUT, "%s: Convexity mapping done ...\n", FuncName);
               SUMA_postRedisplay(w, clientData, callData);   
            }
            break;
         case XK_comma:
            {
               /* switch state, back one */
               int nxtstateID = -1, curstateID = -1;
               int origState = sv->iState;
               char *note=NULL;
               
               if (sv->N_VSv < 2) break;

               curstateID = SUMA_WhichState (sv->State, sv, sv->CurGroupName);
               if (curstateID < 0) {
                  SUMA_SL_Err("Current State not found.\n"
                              "Should not happen here.");
                  SUMA_RETURNe;
               }
               
               if (SUMAg_N_SVv > 1) {
                  ii = SUMA_WhichViewerInMomentum (SUMAg_SVv, SUMAg_N_SVv, sv);
                  if (ii >= 0) {
                     sprintf (s, "You cannot switch states while other viewers\n"
                                 "(like %c) are in momentum mode.\n", ii+65);
                     SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                           s, FuncName, SMT_Error, SMA_LogAndPopup);
                     SUMA_RETURNe;
                  }
               }
                  
               do {
                  if (nxtstateID > -1) {
                     note = SUMA_append_string("Skipping state ",sv->State);
                     note = SUMA_append_replace_string(note, ".\nNo surfaces visible.", "", 1);
                     SUMA_SLP_Note(note);
                     SUMA_free(note);   note = NULL;
                  }

                  /*fprintf(SUMA_STDERR,"%s: Current viewing state is %s ...\n", FuncName, sv->State);*/
                  /* toggle to the next view state */
                  nxtstateID = SUMA_PrevState(sv);
                  if (nxtstateID == curstateID) break;
                  if (nxtstateID < 0) {
                     fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_PrevState.\n", FuncName);
                     break;
                  }
                  fprintf(SUMA_STDERR,"%s: Switching from %s to %s viewing state.\n", \
                     FuncName, sv->State, sv->VSv[nxtstateID].Name);

                  if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, sv, nxtstateID, sv->CurGroupName)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SwitchState.\n", FuncName);
                     break;
                  }
                  
                  /* find out if there are any surfaces that will be rendered */
                  
               } while (!SUMA_VisibleSOs (sv, SUMAg_DOv, NULL) && sv->iState != origState);
               
               /* register a call to redisplay (you also need to copy the color data, in case the next surface is of the same family*/
               if (!list) list = SUMA_CreateList();
               SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
               if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
               }
               
               /* update titles */
               SUMA_UpdateViewerTitle(sv);
            }
            break;

         case XK_period:
            {
               /* switch state, forward one */
               int nxtstateID=-1, curstateID = -1;
               int origState = sv->iState;
               char *note=NULL;
               
               if (sv->N_VSv < 2) break;
               
               curstateID = SUMA_WhichState (sv->State, sv, sv->CurGroupName);
               if (curstateID < 0) {
                  SUMA_SL_Err("Current State not found.\n"
                              "Should not happen here.");
                  SUMA_RETURNe;
               }
               
               if (SUMAg_N_SVv > 1) {
                  ii = SUMA_WhichViewerInMomentum (SUMAg_SVv, SUMAg_N_SVv, sv);
                  if (ii >= 0) {
                     sprintf (s, "You cannot switch states while other viewers\n"
                                 "(like %c) are in momentum mode.\n", ii+65);
                     SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                           s, FuncName, SMT_Error, SMA_LogAndPopup);
                     SUMA_RETURNe;
                  }
               }
               
               do {
                  if (nxtstateID > -1) {
                     note = SUMA_append_string("Skipping state ",sv->State);
                     note = SUMA_append_replace_string(note, ".\nNo surfaces visible.", "", 1);
                     SUMA_SLP_Note(note);
                     SUMA_free(note);   note = NULL;
                  }
                  
                  if (LocalHead) fprintf(SUMA_STDERR,"%s: Current viewing state is %s ...\n", FuncName, sv->State);
                  
                  /* toggle to the next view state */
                  nxtstateID = SUMA_NextState(sv);
                  if (nxtstateID == curstateID) break;
                  if (nxtstateID < 0) {
                     fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_NextState.\n", FuncName);
                     break;
                  }
                  fprintf(SUMA_STDERR,"%s: Switching from %s to %s viewing state.\n", FuncName, sv->State, sv->VSv[nxtstateID].Name);

                  if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, sv, nxtstateID, sv->CurGroupName)) {
                     fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_SwitchState.\n", FuncName);
                     break;
                  }

               } while (!SUMA_VisibleSOs (sv, SUMAg_DOv, NULL) && sv->iState != origState);
               /* register a call to redisplay 
               (you also need to copy the color data, in case the next surface is of the same family*/
               if (!list) list = SUMA_CreateList();
               SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
               if (!SUMA_Engine (&list)) {
                  fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
               }

               /* update titles */
               SUMA_UpdateViewerTitle(sv);
            }
            break;

         case XK_F1: /* F1 */
            /*printf("F1\n");*/
            if (!SUMA_F1_Key(sv, "F1", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;

         case XK_F2:
            /*printf("F2\n");*/
            if (!SUMA_F2_Key(sv, "F2", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;

         case XK_F3: /* F3 */
            if (!SUMA_F3_Key(sv, "F3", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;

         case XK_F4: /* F4 */
            if (!SUMA_F4_Key(sv, "F4", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
            
         case XK_F5: /* F5 */
            if (!SUMA_F5_Key(sv, "F5", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;

         case XK_F6: /*F6 */
            if (!SUMA_F6_Key(sv, "F6", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
        
         case XK_F7: /*F7 */
            if (!SUMA_F7_Key(sv, "F7", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
              
         case XK_F8: /*F8 */
            if (!SUMA_F8_Key(sv, "F8", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
         
         case XK_F12: /* F12 */
            /* time display speed */
            {
               int i, nd = 20, N_vis, *Vis_IDs=NULL, NodeTot, FaceTot;
               GLfloat buf; 
               float delta_t;
               SUMA_SurfaceObject *SO=NULL;
               struct  timeval tti;
               char stmp[500];
               SUMA_STRING *SS = NULL;
               
               SS = SUMA_StringAppend (NULL, NULL);

               buf = sv->light0_position[2];
               SUMA_SLP_Note ("Timing Display speed\n"
                              "(20 displays): \n"); 
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
               fprintf (SUMA_STDOUT,"\n");
               delta_t = SUMA_etime (&tti, 1);
               sv->light0_position[2] = buf;
               glLightfv(GL_LIGHT0, GL_POSITION, sv->light0_position);
               SUMA_postRedisplay(w, clientData, callData);
               sprintf (stmp,"Elapsed time: %f seconds.\n%.2f displays/second.\n", delta_t, nd/delta_t);
               SS = SUMA_StringAppend (SS, stmp);
               
               /* Estimate how many nodes and triangles were rendered */
               Vis_IDs = (int *)SUMA_malloc(sizeof(int)*SUMAg_N_DOv);
               N_vis = SUMA_VisibleSOs (sv, SUMAg_DOv, Vis_IDs);
               NodeTot = 0;
               FaceTot = 0;
               for (i=0; i<N_vis;++i) {
                  SO = (SUMA_SurfaceObject *)SUMAg_DOv[Vis_IDs[i]].OP;
                  FaceTot += SO->N_FaceSet;
                  NodeTot += SO->N_Node;   
               }
               if (N_vis) {
                  sprintf (stmp,"In Polymode %d, rendered \n%.2f Ktri/sec %.2f Kpnt/sec.\n",
                     sv->PolyMode,
                     (float)FaceTot / 1000.0 / delta_t  , 
                     (float)NodeTot / 1000.0 / delta_t );
                  SS = SUMA_StringAppend (SS, stmp);
               }
               
               SUMA_SLP_Note(SS->s);
               
               if (Vis_IDs) SUMA_free(Vis_IDs);
               SUMA_free(SS->s);
               SUMA_free(SS);
               
            } 
            break;
         case XK_F13:
            if (SUMAg_CF->Dev) {
               DList *striplist=NULL;
               float Eq[4];
               int *Vis_IDs, N_vis;
               SUMA_SurfaceObject *SO=NULL;
               Vis_IDs = (int *)SUMA_malloc(sizeof(int)*SUMAg_N_DOv);
               N_vis = SUMA_VisibleSOs (sv, SUMAg_DOv, Vis_IDs);
               if (N_vis) {
                  SO = (SUMA_SurfaceObject *)SUMAg_DOv[Vis_IDs[0]].OP;
                  /* Axial plane */
                  Eq[0] = Eq[1] = 0.0; Eq[2] = 1.0; Eq[3] = -SO->Center[2];
                  SUMA_S_Warnv("Kill me!\nEq:[%f %f %f %f], step: %f\n", Eq[0], Eq[1], Eq[2], Eq[3], SO->EL->AvgLe);
                  striplist = SUMA_SliceAlongPlane(SO, Eq, SO->EL->AvgLe);
                  SUMA_display_edge_striplist(striplist, &(SUMAg_SVv[0]), SO, 
                                             "ShowConnectedPoints");
                  SUMA_FREE_DLIST(striplist);
               }
               if (Vis_IDs) SUMA_free(Vis_IDs);
            }
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
               if (!SUMA_Left_Key(sv, "ctrl+shift+left", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               } 
            }else if (Kev.state & ShiftMask) {
               if (!SUMA_Left_Key(sv, "shift+left", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               } 
            }else if (Kev.state & ControlMask){
               if (!SUMA_Left_Key(sv, "ctrl+left", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }   
            }else if (Kev.state & Mod1Mask  || Kev.state & Mod2Mask) {
               if (!SUMA_Left_Key(sv, "alt+left", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }
            }else {
               if (!SUMA_Left_Key(sv, "left", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }
            }
               
            break;

         case XK_Right:   /*KEY_RIGHT: */
            /*printf("Right Key\n");*/
            if ((Kev.state & ControlMask) && (Kev.state & ShiftMask)) {
               if (!SUMA_Right_Key(sv, "ctrl+shift+right", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }   
            }else if (Kev.state & ShiftMask) {
               if (!SUMA_Right_Key(sv, "shift+right", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }  
            }else if (Kev.state & ControlMask){
               if (!SUMA_Right_Key(sv, "ctrl+right", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }  
            }else if (Kev.state & Mod1Mask || Kev.state & Mod2Mask) {
               if (!SUMA_Right_Key(sv, "alt+right", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }  
            }else {
               if (!SUMA_Right_Key(sv, "right", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }  
            }
            break;

         case XK_Down:   /*KEY_DOWN*/
            /*printf("Down Key\n");*/
            if ((Kev.state & ControlMask) && (Kev.state & ShiftMask)) {
               if (!SUMA_Down_Key(sv, "ctrl+shift+down", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }  
            }else if (Kev.state & ShiftMask) {
               if (!SUMA_Down_Key(sv, "shift+down", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }  
            }else if (Kev.state & ControlMask){
               if (!SUMA_Down_Key(sv, "ctrl+down", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               } 
            }else if (Kev.state & Mod1Mask || Kev.state & Mod2Mask) {
               if (!SUMA_Down_Key(sv, "alt+down", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }  
            }else {
               if (!SUMA_Down_Key(sv, "down", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               } 
            }
            
            break;

         case XK_Up: /*KEY_UP*/
            /*printf("Up Key\n");*/
            if ((Kev.state & ControlMask) && (Kev.state & ShiftMask)) {
               if (!SUMA_Up_Key(sv, "ctrl+shift+up", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }
            }else if (Kev.state & ShiftMask) {
               if (!SUMA_Up_Key(sv, "shift+up", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }
            }else if (Kev.state & ControlMask){
               if (!SUMA_Up_Key(sv, "ctrl+up", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }
            }else if (Kev.state & Mod1Mask || Kev.state & Mod2Mask) {
               if (!SUMA_Up_Key(sv, "alt+up", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }
            }else {
               if (!SUMA_Up_Key(sv, "up", "interactive")) {
                  SUMA_S_Err("Error in key func.");
                  break;
               }
            }
            break;
         default:
            break;

      } /* keysym */
   break;
   
   case ButtonPress:
      pButton = Bev.button;
      if (LocalHead) fprintf(stdout,"In ButtonPress Button %d\n", pButton);      
      if (SUMAg_CF->SwapButtons_1_3 || (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
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
               bevx=(float)Bev.x; bevy = (float)Bev.y;
               /*fprintf(SUMA_STDERR,"%s: Button 1 down. New\n", FuncName);*/
               /* setup initial spinning conditions */
               sv->GVS[sv->StdView].spinBeginX = bevx;
               sv->GVS[sv->StdView].spinBeginY = bevy;
               sv->GVS[sv->StdView].spinDeltaX = 0;
               sv->GVS[sv->StdView].spinDeltaY = 0;   
               /* check to see if other viewers need to be notified */
               ii = SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv);
               if (SUMAg_CF->ViewLocked[ii]) {
                  for (it=0; it < SUMAg_N_SVv; ++it) {
                     svi = &SUMAg_SVv[it];
                     if (it != ii && SUMAg_CF->ViewLocked[it]) {
                        svi->GVS[svi->StdView].spinBeginX = bevx;
                        svi->GVS[svi->StdView].spinBeginY = bevy;
                        svi->GVS[svi->StdView].spinDeltaX = 0;
                        svi->GVS[svi->StdView].spinDeltaY = 0; 
                     }  
                  }
               }
            }
            break;
         case Button4:
            {
               if (!SUMA_Z_Key(sv, "z", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            }
            break;
         case Button5:
            {
               if (!SUMA_Z_Key(sv, "Z", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
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
               bevx = (float)Bev.x; bevy = (float)Bev.y;
               sv->GVS[sv->StdView].translateBeginX = bevx;;
               sv->GVS[sv->StdView].translateBeginY = bevy;
               sv->GVS[sv->StdView].translateDeltaX = 0.0;
               sv->GVS[sv->StdView].translateDeltaY = 0.0;
            }
            break;
            
         case Button3:
               if (LocalHead) fprintf(SUMA_STDERR,"%s: Button 3 downplain jane, viewer #%d : X=%f, Y = %f\n", \
                  FuncName, SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv), (float)Bev.x, (float)Bev.y);
               
               #if 0
               /* are we in ROI drawing mode ? */
               if (Bev.state & ShiftMask && SUMAg_CF->ROI_mode && sv->Focus_SO_ID >= 0) {
                  /* ROI drawing mode */
                  ROI_mode = YUP;     
               }else {
                  ROI_mode = NOPE;
               }
               #endif
               /* are we in ROI drawing mode ? */
               if (SUMAg_CF->ROI_mode && sv->Focus_SO_ID >= 0 && !(Bev.state & ShiftMask)) {
                  /* ROI drawing mode */
                  ROI_mode = YUP;     
               }else {
                  ROI_mode = NOPE;
               }
               
               if (!DoubleClick) {
               /* you do not want to waist time doing double calculations if the user clicks twice by mistake */
                  /* make sure no viewer, other than the one clicked in is in momentum mode */
                  if (SUMAg_N_SVv > 1) {
                     ii = SUMA_WhichViewerInMomentum (SUMAg_SVv, SUMAg_N_SVv, NULL);
                     if (ii >= 0) {
                        sprintf (s, "You cannot select or draw while viewers\n"
                                    "(like %c) are in momentum mode.\n", ii+65);
                        SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                              s, FuncName, SMT_Error, SMA_LogAndPopup);
                        SUMA_RETURNe;
                     }
                  }  
                  
                  
                  ii = SUMA_RegisteredSOs(sv, SUMAg_DOv, NULL);
                  if (ii == 0) { /* no surfaces, break */
                     break;
                  }


                  if (!SUMA_GetSelectionLine (sv, (int)Bev.x, (int)Bev.y, sv->Pick0, sv->Pick1, 0, NULL, NULL, NULL)) {
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
      rButton = Bev.button;
      if (LocalHead) fprintf(SUMA_STDERR,"%s: In ButtonRelease Button %d\n", FuncName, rButton); 
      if (SUMAg_CF->SwapButtons_1_3 || (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
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
      if (LocalHead) {
          fprintf(stdout,"In MotionNotify\n"); 
              if (Mev.state & Button1MotionMask) fprintf(stdout,"   B1 mot\n");
         else if (Mev.state & Button2MotionMask) fprintf(stdout,"   B2 mot\n");
         else if (Mev.state & Button3MotionMask) fprintf(stdout,"   B3 mot\n");
         else if (Mev.state & Button4MotionMask) fprintf(stdout,"   B4 mot\n");
         else if (Mev.state & Button5MotionMask) fprintf(stdout,"   B5 mot\n");
      }
      if (SUMAg_CF->SwapButtons_1_3 || (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
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
            /* Now update the zoom compensation variable */
            if (sv->ZoomCompensate) {
               sv->ZoomCompensate = sv->FOV[sv->iState] / SUMA_sv_fov_original(sv);
               if (sv->ZoomCompensate > 1) sv->ZoomCompensate = 1.0; /* no need to compensate at low zooms */
               else if (sv->ZoomCompensate < 0.005) sv->ZoomCompensate = 0.005; /* no need to go lower */ 
            }
            ii = SUMA_WhichSV (sv, SUMAg_SVv, SUMAg_N_SVv);
            SUMA_postRedisplay(w, clientData, callData);    
            break;
            
         case SUMA_Button_1_Motion:     
            /*fprintf(SUMA_STDERR,"%s: In motion, Butt1 \n", FuncName); */
            mevx = (float)Mev.x;
            mevy = (float)Mev.y;
            wwid = (float)sv->WindWidth;
            whei = (float)sv->WindHeight;
            /* spinning mode */
            if (sv->ZoomCompensate) {
               zc_fac = sv->ZoomCompensate;
            }else {
               zc_fac = 1.0;
            }            
            sv->GVS[sv->StdView].spinDeltaX = (mevx - sv->GVS[sv->StdView].spinBeginX);
            sv->GVS[sv->StdView].spinDeltaY = (mevy - sv->GVS[sv->StdView].spinBeginY);
            
            /* fprintf(stdout,"\n"
                           "spinBeginX %f \n"
                           "spinBeginY %f \n"
                           "spinDeltaX %f \n"
                           "spinDeltaY %f \n"
                           "WindWidth %d  \n"
                           "WindHeight %d\n"
                           "ZoomCompensate %f\n", 
                        sv->GVS[sv->StdView].spinBeginX, sv->GVS[sv->StdView].spinBeginY, 
                        sv->GVS[sv->StdView].spinDeltaX, sv->GVS[sv->StdView].spinDeltaY, 
                        sv->WindWidth, sv->WindHeight, sv->ZoomCompensate); */
            if (sv->GVS[sv->StdView].spinDeltaX || sv->GVS[sv->StdView].spinDeltaY){
               trackball(  sv->GVS[sv->StdView].deltaQuat, 
                           (2*sv->GVS[sv->StdView].spinBeginX - wwid)/wwid*zc_fac, 
                           (whei - 2*sv->GVS[sv->StdView].spinBeginY)/whei*zc_fac,
                           (2*mevx - wwid)/wwid*zc_fac, 
                           (whei - 2*mevy)/whei*zc_fac); /* comput the increment Quat */
               sv->GVS[sv->StdView].spinBeginX = mevx;
               sv->GVS[sv->StdView].spinBeginY = mevy;
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
                  SUMA_STANDARD_VIEWS ed_sv, ed_svi;
                  /* redisplay current viewer immediately */
                  list = SUMA_CreateList ();
                  ED = SUMA_InitializeEngineListData (SE_RedisplayNow);
                  SUMA_RegisterEngineListCommand (list, ED,
                                                   SEF_Empty, NULL,
                                                   SES_Suma, (void *)sv, NOPE,
                                                   SEI_Head, NULL);
                  ed_sv = SUMA_BestStandardView(sv, SUMAg_DOv, SUMAg_N_DOv);
                  for (it=0; it < SUMAg_N_SVv; ++it) {
                     svi = &SUMAg_SVv[it];
                     ed_svi = SUMA_BestStandardView(svi, SUMAg_DOv, SUMAg_N_DOv);
                     if (it != ii && SUMAg_CF->ViewLocked[it] && ed_svi == ed_sv) {
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
            mevx = (float)Mev.x; mevy = (float)Mev.y;
            if (sv->ZoomCompensate) {
               zc_fac = sv->ZoomCompensate;
            }else {
               zc_fac = 1.0;
            }
            sv->GVS[sv->StdView].translateDeltaX =  (mevx - sv->GVS[sv->StdView].translateBeginX)/(float)sv->WindWidth*sv->GVS[sv->StdView].TranslateGain;
            sv->GVS[sv->StdView].translateDeltaY = -(mevy - sv->GVS[sv->StdView].translateBeginY)/(float)sv->WindHeight*sv->GVS[sv->StdView].TranslateGain;
            
            if (sv->GVS[sv->StdView].translateDeltaX || sv->GVS[sv->StdView].translateDeltaY){
               sv->GVS[sv->StdView].translateVec[0] += (GLfloat)sv->GVS[sv->StdView].translateDeltaX * zc_fac;
               sv->GVS[sv->StdView].translateVec[1] += (GLfloat)sv->GVS[sv->StdView].translateDeltaY * zc_fac;
               sv->GVS[sv->StdView].translateBeginX = mevx;
               sv->GVS[sv->StdView].translateBeginY = mevy;
               SUMA_postRedisplay(w, clientData, callData);
            }  
            break;
         
         case SUMA_Button_3_Motion:
            if (LocalHead) fprintf(SUMA_STDERR,"%s: In motion, Butt3 \n", FuncName); 
            
            if (SUMAg_CF->ROI_mode && sv->Focus_SO_ID >= 0 && sv->BS) {
               /* ROI drawing mode */
               ii = SUMA_RegisteredSOs(sv, SUMAg_DOv, NULL);
               if (ii == 0) { /* no surfaces, break */
                  break;
               }


               if (!SUMA_GetSelectionLine (sv, (int)Mev.x, (int)Mev.y, sv->Pick0, sv->Pick1, 0, NULL, NULL, NULL)) {
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
   
   SUMA_ENTRY;
   
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
         /*fprintf(stdout,"SUMA_momentum:  spinDeltaX %f spinDeltaY %f\n",  sv->GVS[sv->StdView].spinDeltaX, sv->GVS[sv->StdView].spinDeltaY);*/
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
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;


   P0f[0] = sv->Pick0[0];
   P0f[1] = sv->Pick0[1];
   P0f[2] = sv->Pick0[2];
   P1f[0] = sv->Pick1[0];
   P1f[1] = sv->Pick1[1];
   P1f[2] = sv->Pick1[2];
   
   N_SOlist = SUMA_VisibleSOs(sv, dov, SOlist);
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
      SUMA_UpdateViewerTitle(sv);
      SO = (SUMA_SurfaceObject *)dov[imin].OP;
      NP = SO->FaceSetDim;
      ip = NP * MTI->ifacemin;

      /* if the surface controller is open, update it */
      if (SO->SurfCont->TopLevelShell)   SUMA_Init_SurfCont_SurfParam(SO);

      /* print nodes about the closets faceset*/
      fprintf(SUMA_STDOUT, "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
      fprintf(SUMA_STDOUT, "Selected surface %s (Focus_SO_ID # %d).\nFaceSet %d, Closest Node %d\n", 
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
      if (SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] && sv->LinkAfniCrossHair) {
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
   
   SUMA_ENTRY;
   
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
   
   SUMA_ENTRY;
   
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
   
   SUMA_ENTRY;
      
   /* New Version */
   if (sv->BS) {  /* bad news, this should be NULL to begin with */
      SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                            "Brush Stroke not NULL.", FuncName, 
                            SMT_Critical, SMA_LogAndPopup);
      SUMA_RETURN(NOPE); 
      
   }
   sv->BS = (DList *)SUMA_calloc(1,sizeof(DList));
   dlist_init(sv->BS, SUMA_FreeBSDatum);  
   
   SUMA_RETURN (YUP);
}

SUMA_BRUSH_STROKE_DATUM * SUMA_CreateBSDatum(void)
{
   static char FuncName[]={"SUMA_CreateBSDatum"};
   SUMA_BRUSH_STROKE_DATUM *bsd = NULL;
   
   SUMA_ENTRY;
   
   bsd = (SUMA_BRUSH_STROKE_DATUM *)
            SUMA_calloc(1,sizeof(SUMA_BRUSH_STROKE_DATUM));
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
   
   SUMA_ENTRY;
   
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
   
   SUMA_ENTRY;
   
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
   
   SUMA_ENTRY;
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
   
   SUMA_ENTRY;
   
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
         
         SUMA_DrawWindowLine( sv, (int)bsd->x, (int)bsd->y, 
                              (int)bsdn->x, (int)bsdn->y, 1);
      } while (NEn != dlist_tail(sv->BS));
      
   } else {
      NEn = dlist_tail(sv->BS);
      NE = NEn->prev;
      
      bsd = (SUMA_BRUSH_STROKE_DATUM *)NE->data;
      bsdn = (SUMA_BRUSH_STROKE_DATUM *)NEn->data;
      
      SUMA_DrawWindowLine( sv, 
                           (int)bsd->x, (int)bsd->y, 
                           (int)bsdn->x, (int)bsdn->y, 1 );
          
   }
   SUMA_RETURNe;

}

/*!
   \brief Processes the brushstroke sent from a viewer
   
*/
SUMA_DRAWN_ROI * SUMA_ProcessBrushStroke 
                  (SUMA_SurfaceViewer *sv, SUMA_BRUSH_STROKE_ACTION BsA)
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
   
   SUMA_ENTRY;

   if (!SO) {
      fprintf (SUMA_STDERR, 
               "%s: No surface object in focus, nothing to do.\n", FuncName); 
      SUMA_RETURN (DrawnROI);
   }
   
   if (!sv->BS) {
      fprintf (SUMA_STDERR, 
               "%s: No Brushstroke (BS), nothing to do.\n", FuncName); 
      SUMA_RETURN (DrawnROI);
   }
   
   if (!SUMAg_CF->ROI_mode) {
      fprintf (SUMA_STDERR, "%s: Not in ROI mode, nothing to do.\n", FuncName); 
      SUMA_RETURN (DrawnROI);
   }
   
   /* We are in ROI mode, 
      is there an ROI in curDrawnROI that works with the current surface ? */
   if (SUMAg_CF->X->DrawROI->curDrawnROI) {
      if (  SUMA_isdROIrelated(SUMAg_CF->X->DrawROI->curDrawnROI, SO) && 
            SUMAg_CF->X->DrawROI->curDrawnROI->DrawStatus != SUMA_ROI_Finished){
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s: using currDrawnROI.\n", FuncName);
         DrawnROI = SUMAg_CF->X->DrawROI->curDrawnROI;
      }else {
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: No match between currDrawnROI and SO.\n", FuncName);
         DrawnROI = NULL;
      }
   }
   if (!DrawnROI) { /* try some more */
      if ((DrawnROI = SUMA_FetchROI_InCreation (SO, SUMAg_DOv, SUMAg_N_DOv))){
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s: using ROI in creation.\n", FuncName);
         /* There is an ROI being created on this surface, 
            initialize DrawROI window*/
         SUMA_InitializeDrawROIWindow(DrawnROI);
      } else {
         /* wait till later */
         if (LocalHead) 
            fprintf (SUMA_STDERR,"%s: will create a new ROI.\n", FuncName);
      }
   }
   
   if (!DrawnROI && BsA == SUMA_BSA_JoinEnds) {
      SUMA_SLP_Err ("NO ROI to close.");
      SUMA_RETURN (DrawnROI);
   }
   
   if (!DrawnROI) { /* No ROI found, create one */
      if (LocalHead) 
         fprintf (SUMA_STDERR, 
                  "%s: No ROI found, creating a new one.\n", FuncName);
      SUMA_GET_TEXT_FIELD(SUMAg_CF->X->DrawROI->ROIlbl->textfield, sbuf);
      DrawnROI = SUMA_AllocateDrawnROI (SO->idcode_str, SUMA_ROI_InCreation,  
                                        SUMA_ROI_OpenPath, 
                                        sbuf, 
                                        SUMAg_CF->X->DrawROI->ROIval->value);
      if (!DrawnROI) {
         SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                               "Failed to allocate for DrawnROI.", FuncName, 
                               SMT_Critical, SMA_LogAndPopup);
         SUMA_RETURN (NULL);
      }

      /* Although ROIs are stored as DOs, 
         they are dependent on the surfaces they are related to 
         ROIs at this stage are node indices only (and perhaps the mesh) but the          coordinates of the indices
         come from the surface onto which they are displayed. So when you are 
         drawing a surface, using CreateMesh,
         you will search DOv for ROIs related to the surface displayed and 
         overlay them accordingly */
      /* Add the ROI to DO */
      if (!SUMA_AddDO ( SUMAg_DOv, &SUMAg_N_DOv, 
                        (void *)DrawnROI, ROIdO_type, SUMA_WORLD)) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_AddDO.\n", FuncName);
      }

      /* is the Switch ROI window open ? */
      SUMA_IS_DRAW_ROI_SWITCH_ROI_SHADED(Shaded);
      if (!Shaded) {
         SUMA_cb_DrawROI_SwitchROI (
               NULL, 
               (XtPointer) SUMAg_CF->X->DrawROI->SwitchROIlst, 
               NULL);
      }
   

   } else {
      if (LocalHead) 
         fprintf( SUMA_STDOUT,
                  "%s: ROI %p fetched. Status %d.\n", 
                  FuncName, DrawnROI, DrawnROI->DrawStatus); 
   } 

   if (BsA == SUMA_BSA_AppendStrokeOrFill) {
      if (  DrawnROI->Type == SUMA_ROI_ClosedPath || 
            DrawnROI->Type == SUMA_ROI_FilledArea) 
         BsA = SUMA_BSA_FillArea;
      else if (DrawnROI->Type == SUMA_ROI_OpenPath) 
         BsA = SUMA_BSA_AppendStroke;
   }
   if (  DrawnROI->Type == SUMA_ROI_ClosedPath && 
         BsA != SUMA_BSA_FillArea) {
      SUMA_SLP_Err ( "You can only fill a closed path.\n"
                     "You cannot append more paths to it.");
      SUMA_RETURN (DrawnROI);
   }
   if (  DrawnROI->Type == SUMA_ROI_FilledArea && 
         BsA != SUMA_BSA_FillArea) {
      SUMA_SLP_Err ("You cannot add paths to a filled ROI.");
      SUMA_RETURN (DrawnROI);
   }
   
   /* Good, now initialize the DrawROI widget, if needed */
   if (SUMAg_CF->X->DrawROI->curDrawnROI != DrawnROI) {
      if (!SUMA_InitializeDrawROIWindow (DrawnROI)) {
         SUMA_SL_Err("Failed to initialize DrawWindow.");
      }
   }

   /* Now you must transform the brushstroke to a series of nodes 
      (not necessarily connected)*/
   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: Turning BrushStroke to NodeStroke ...\n", FuncName);
   if (!SUMA_BrushStrokeToNodeStroke (sv)) {
      SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                         "Failed in SUMA_BrushStrokeToNodeStroke.", FuncName, 
                         SMT_Error, SMA_LogAndPopup);
      SUMA_RETURN(NULL);
   }
      
   switch (BsA) {
      case SUMA_BSA_AppendStroke:
         /* Turn the brush stroke into a series of connected nodes */
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: Turning NodeStroke to ROIStroke ...\n", FuncName);
         if (!(ROIstroke = SUMA_NodeStrokeToConnectedNodes (sv))) {
            SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                  "Failed in SUMA_NodeStrokeToConnectedNodes.",
                                  FuncName, 
                                  SMT_Critical, SMA_LogAndPopup);
            if (ROIlink) SUMA_FreeROIDatum((void *)ROIlink); 
            ROIlink = NULL;
            if (ROIstroke) SUMA_FreeROIDatum((void *)ROIstroke); 
            ROIstroke = NULL;
            SUMA_RETURN(NULL);
         }

         if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: Turning NodeStroke to ROIStroke . DONE.\n", 
                     FuncName);
         /* if this is the first element of ROI, 
            create the first ROIdatum and get out */
         if (dlist_size(DrawnROI->ROIstrokelist)) {
            /* Not the beginning of an ROI */
            if (LocalHead) 
               fprintf (SUMA_STDERR, 
                        "%s: Adding ROIstroke to previous ones ...\n", 
                        FuncName);
            /* make sure new brushstroke is not just one node that is 
               the tail of the ROI*/
            SUMA_DRAWN_ROI_TAIL_NODE(DrawnROI,TailNode);

            SUMA_BS_FIRST_SURF_NODE(sv->BS, FirstSurfNode, ft, El);
            SUMA_BS_COUNT_SURF_NODES(sv->BS, N_SurfNode);
            if (FirstSurfNode == TailNode && N_SurfNode == 1) {
               /* nothing to do here */
               fprintf (SUMA_STDERR, 
                        "%s: New stroke has one node that is \n"
                        "identical to tail node. Dumping element.\n", FuncName);
               SUMA_RETURN(DrawnROI);
            }

            /* Connect this chunk to the last open Node in ROI */
            if (FirstSurfNode != TailNode) {
               if (LocalHead) 
                  fprintf (SUMA_STDERR, 
                           "%s: linking Tail Node to New stroke.\n", FuncName);

               ROIlink = SUMA_LinkTailNodeToNodeStroke (sv, DrawnROI);
               if (!ROIlink) {
                  SUMA_SL_Err("Failed to connect Tail node to Node stroke\n"
                              ", try again.");
                  SUMA_RETURN(NULL);
               }
               if (LocalHead) {
                  fprintf (SUMA_STDERR, 
                           "%s: RIOlink, before prepending:\n", FuncName);
                  SUMA_ShowDrawnROIDatum (ROIlink, NULL, NOPE);
               }

               /* connect the ROIlink with the ROIstroke */
               if (LocalHead) {
                  fprintf (SUMA_STDERR, 
                           "%s: RIOstroke, before prepending:\n", FuncName);
                  SUMA_ShowDrawnROIDatum (ROIstroke, NULL, NOPE);
               }
               if (!SUMA_PrependToROIdatum (ROIlink, ROIstroke)) {
                  SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                     "Failed to merge ROIs.", FuncName,
                                     SMT_Critical, SMA_LogAndPopup);
                  if (ROIlink) SUMA_FreeROIDatum((void *)ROIlink); 
                  ROIlink = NULL;
                  if (ROIstroke) SUMA_FreeROIDatum((void *)ROIstroke); 
                  ROIstroke = NULL;
                  SUMA_RETURN(NULL);   
               }

               if (LocalHead) {
                  fprintf (SUMA_STDERR, 
                           "%s: RIOstroke, after prepending:\n", FuncName);
                  SUMA_ShowDrawnROIDatum (ROIstroke, NULL, NOPE);
               }
              /* now free ROIlink, not needed anymore */
               if (ROIlink) SUMA_FreeROIDatum ((void *)ROIlink); ROIlink = NULL;
            }
         }else{
            if (LocalHead) 
               fprintf (SUMA_STDERR, "%s: First ROIStroke of ROI.\n", FuncName);
         }
         break;
      case SUMA_BSA_JoinEnds:
         /* Join ends here */
         if (DrawnROI) { /*   close ROI */
            SUMA_DRAWN_ROI_HEAD_NODE(DrawnROI,HeadNode);         
            SUMA_BS_FIRST_SURF_NODE(sv->BS, FirstSurfNode, ft, El);
            bsd = (SUMA_BRUSH_STROKE_DATUM *)El->data;
            if (LocalHead) 
               fprintf( SUMA_STDERR, 
                        "%s: Trying to join node %d to node %d.\n", 
                        FuncName, FirstSurfNode, HeadNode);
            /* Now compute the intersection of the surface with the plane */
            ROIstroke = SUMA_Surf_Plane_Intersect_ROI (  SO, FirstSurfNode, 
                                                         HeadNode, bsd->NP);

            if (!ROIstroke) {
               SUMA_SL_Err ("Failed to close path. Repeat new stroke.");
               SUMA_RETURN(DrawnROI);
            }
            /* what is the last node of ROIstroke ? 
            It is possible that the returned ROIstroke 
            was not a successful closure (a partial success), investigate*/
            if (LocalHead) 
               fprintf( SUMA_STDERR, 
                        "%s: Last node of ROIstroke is %d\n", 
                        FuncName, ROIstroke->nPath[ROIstroke->N_n-1]); 
            if (ROIstroke->nPath[ROIstroke->N_n-1] != HeadNode) {
               /* pretend this is not a JoinEnds exercice */
               BsA = SUMA_BSA_AppendStroke;
               SUMA_SL_Err ("Failed to close path. Continue with stroke.");
               SUMA_RETURN(DrawnROI);
            }else {
               /* Do not remove the last point from ROIstroke, 
                  otherwise it will make drawing a closed ROI painful */
            } 
         } else {
            /* tremors, nothing to do */
         }
         break;
      case SUMA_BSA_FillArea:
         SUMA_BS_FIRST_SURF_NODE(sv->BS, FirstSurfNode, ft, El);
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: Should be filling from node %d\n", 
                     FuncName, FirstSurfNode);
         
         /* create the mask from ROIs on this surface */
         switch (SUMAg_CF->ROI_FillMode) {
            case SUMA_ROI_FILL_TO_ALLROI:
               ROI_Mask = SUMA_Build_Mask_AllROI ( SUMAg_DOv, SUMAg_N_DOv, 
                                                   SO, NULL, &N_ROI_Mask);
               break;
            case SUMA_ROI_FILL_TO_THISROI:
               ROI_Mask = (int *)SUMA_calloc (SO->N_Node, sizeof(int));
               if (!ROI_Mask) {
                  SUMA_SLP_Crit("Failed to allocate");
                  SUMA_RETURN(DrawnROI);
               }
               SUMA_Build_Mask_DrawnROI (DrawnROI, ROI_Mask);
               break;
            default:
               SUMA_SLP_Err("No such mode.");
               SUMA_RETURN(DrawnROI);
               break;
         }
               
         /* Now fill it up */
         ROIfill = SUMA_FillToMask (SO, ROI_Mask, FirstSurfNode);
         if (ROI_Mask) SUMA_free(ROI_Mask); ROI_Mask = NULL;
         if (!ROIfill) {
            SUMA_SLP_Err(  "Failed to fill area:\n"
                           "Perhaps seed on edge\nor nothing to fill.");
            SUMA_RETURN(DrawnROI);
         }
         
         break;
      default:
         fprintf (SUMA_STDERR, 
                  "Error %s: Why are you doing this to me ?.\n", FuncName);
         break;
   }
        
   
   /* Another switch on BsA, 
      it is possible that its value changed within this function */
   
   switch (BsA) {
      case SUMA_BSA_AppendStroke:
         /* store the action */
         ROIstroke->action = SUMA_BSA_AppendStroke;
         /*now add the ROIdatum to the list of ROIs */
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: Adding ROIStroke to DrawnROI->ROIstrokelist\n",  
                     FuncName);
         ROIA = (SUMA_ROI_ACTION_STRUCT *)
                   SUMA_calloc(1,sizeof(SUMA_ROI_ACTION_STRUCT)); 
                   /* this structure is freed in SUMA_DestroyROIActionData */
         ROIA->DrawnROI = DrawnROI;
         ROIA->ROId = ROIstroke;
         tmpStackPos = SUMA_PushActionStack (
                           DrawnROI->ActionStack, 
                           DrawnROI->StackPos, SUMA_AddToTailROIDatum, 
                           (void *)ROIA, SUMA_DestroyROIActionData);
         if (tmpStackPos) DrawnROI->StackPos = tmpStackPos;
         else {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed in SUMA_PushActionStack.\n", FuncName);
            SUMA_RETURN (DrawnROI);
         }
         if (  SUMAg_CF->X->DrawROI->WhatDist == SW_DrawROI_WhatDistTrace ||  
               SUMAg_CF->X->DrawROI->WhatDist == SW_DrawROI_WhatDistAll) 
            SUMA_ReportDrawnROIDatumLength(  SO, ROIA->ROId, NULL, 
                                             SUMAg_CF->X->DrawROI->WhatDist);
         break;
      case SUMA_BSA_JoinEnds:
         /* store the action */
         ROIstroke->action = SUMA_BSA_JoinEnds;
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Closing path.\n", FuncName);
         ROIA = (SUMA_ROI_ACTION_STRUCT *)
                   SUMA_calloc(1,sizeof(SUMA_ROI_ACTION_STRUCT)); 
                     /* this structure is freed in SUMA_DestroyROIActionData */
         ROIA->DrawnROI = DrawnROI;
         ROIA->ROId = ROIstroke;
         tmpStackPos = SUMA_PushActionStack (
                           DrawnROI->ActionStack, DrawnROI->StackPos, 
                           SUMA_AddToTailJunctionROIDatum, 
                           (void *)ROIA, SUMA_DestroyROIActionData);
         if (tmpStackPos) DrawnROI->StackPos = tmpStackPos;
         else {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed in SUMA_PushActionStack.\n", FuncName);
            SUMA_RETURN (DrawnROI);
         }
         if (  SUMAg_CF->X->DrawROI->WhatDist == SW_DrawROI_WhatDistTrace ||  
               SUMAg_CF->X->DrawROI->WhatDist == SW_DrawROI_WhatDistAll) 
            SUMA_ReportDrawnROIDatumLength(  SO, ROIA->ROId, NULL, 
                                             SUMAg_CF->X->DrawROI->WhatDist);
         break;
      case SUMA_BSA_FillArea:
         /* store the action */
         ROIfill->action = SUMA_BSA_FillArea;
         /* Now add ROIdatum to stack */
         ROIA = (SUMA_ROI_ACTION_STRUCT *)
                   SUMA_calloc(1,sizeof(SUMA_ROI_ACTION_STRUCT)); 
                     /* this structure is freed in SUMA_DestroyROIActionData */
         ROIA->DrawnROI = DrawnROI;
         ROIA->ROId = ROIfill;
         tmpStackPos = SUMA_PushActionStack (
                           DrawnROI->ActionStack, DrawnROI->StackPos, 
                           SUMA_AddFillROIDatum, 
                           (void *)ROIA, SUMA_DestroyROIActionData);
         if (tmpStackPos) DrawnROI->StackPos = tmpStackPos;
         else {
            fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_PushActionStack.\n", FuncName);
            SUMA_RETURN (DrawnROI);
         } 
         break;
      default:
         fprintf (SUMA_STDERR, 
            "Error %s: Why are you doing this to me ??.\n", FuncName);
         break; 
   }      
   
   /* Now update the Paint job on the ROI plane */
   if (!SUMA_Paint_SO_ROIplanes_w (SO, SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SLP_Err("Failed in SUMA_Paint_SO_ROIplanes_w.");
      SUMA_RETURN(DrawnROI);
   }
   if (BsA == SUMA_BSA_FillArea) {   /*  ZSS Sept 29 06 */
      SUMA_OVERLAYS *Overlay=NULL;
      int junk;
      /* If you're drawing, and have just filled an area, 
         better pop the dset to the top so it is visible */
      if (!(Overlay = SUMA_Fetch_OverlayPointer(SO->Overlays, 
                                                SO->N_Overlays, 
                                                DrawnROI->ColPlaneName, 
                                                &junk))) {
         SUMA_S_Err("Unexpected! Could not find overlay pointer");
      } else {
         /* if the current col plane is not the same as this one, 
            do the switching please */
         SUMA_InitializeColPlaneShell(SO, Overlay);
         SUMA_UpdateColPlaneShellAsNeeded(SO); /* update other open 
                                                   ColPlaneShells */
      }
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
   
   SUMA_ENTRY;
   
   
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
      int DeciReentry=0, UsedNode[3]={ 0 , 0, 0 };
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
                     fprintf (SUMA_STDERR, " %d: [%d %d %d] Tri %d\n", i, n1, n2, n3, SUMA_whichTri(SO->EL, n1, n2, n3, 1));
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

                  ti = SUMA_whichTri(SO->EL, n1, n2, n3, 1);
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
   
   SUMA_ENTRY;
   
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
   
   SUMA_ENTRY;
   
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
   
   SUMA_ENTRY;
   
   
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
   
   SUMA_ENTRY;

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
   AS_data = (SUMA_ACTION_STACK_DATA *)
                  SUMA_calloc(1,sizeof(SUMA_ACTION_STACK_DATA));
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
   
   SUMA_ENTRY;
   
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
   
   SUMA_ENTRY;
   
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
   SUMA_SurfaceObject *SOparent=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   ROIA = (SUMA_ROI_ACTION_STRUCT *)data;
   
   switch (Pol) {
      case SAP_Do:
      case SAP_Redo:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Marking as finished...\n", FuncName);
         /* set the drawing status */
         ROIA->DrawnROI->DrawStatus = SUMA_ROI_Finished;
         
         SOparent = SUMA_findSOp_inDOv(ROIA->DrawnROI->Parent_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
         if (!SOparent) {
            SUMA_SLP_Warn( "Parent surface\n"
                           "not found for ROI\n"
                           "No contour will\n"
                           "be determined." );
            SUMA_RETURN(SAR_Succeed);
         }else {
               
            /* calculate the contours */
            if (!ROIA->DrawnROI->CE) { /* must create contour */
               int *Nodes, N_Nodes;
               SUMA_Boolean Unique = NOPE;

               SUMA_LH("Getting Contour ");
               N_Nodes = 0;
               Unique = YUP; /* Set to YUP if you have node indices listed more than once. */
               Nodes = SUMA_NodesInROI (ROIA->DrawnROI, &N_Nodes, Unique);
               if (Nodes) {
                  ROIA->DrawnROI->CE = SUMA_GetContour (
                                 SOparent, 
                                 Nodes, N_Nodes, &(ROIA->DrawnROI->N_CE), 0, NULL);
                  if (!ROIA->DrawnROI->CE) { SUMA_LH("Null DrawnROI->CE"); }
                  else { SUMA_LH("Good DrawnROI->CE"); }
                  SUMA_free(Nodes);
               }
            }else {
               SUMA_SLP_Err("Unexpected Contour");
               SUMA_RETURN(SAR_Fail);
            }
         }

         break;
      case SAP_Undo:
         if (LocalHead) fprintf (SUMA_STDERR, "%s: Marking as InCreation...\n", FuncName);
         ROIA->DrawnROI->DrawStatus = SUMA_ROI_InCreation;
         /* remove any contour if present */
         if (ROIA->DrawnROI->CE) SUMA_free(ROIA->DrawnROI->CE); ROIA->DrawnROI->CE = NULL;
         ROIA->DrawnROI->N_CE = -1;
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
   
   SUMA_ENTRY;

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
   
   SUMA_ENTRY;

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
   
   SUMA_ENTRY;

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
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ROI_ACTION_STRUCT *ROIA=NULL;
   
   SUMA_ENTRY;
   
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
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

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
   \brief sets the number of smoothing operations to perform on foreground colors
   before display
*/
void SUMA_SetNumForeSmoothing (char *s, void *data)
{
   static char FuncName[]={"SUMA_SetNumForeSmoothing"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   float fv3[3];
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, fv3, 1) != 1) { /* problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }

   /* set sv */
  
   if ((int)fv3[0] < 0) {
      SUMA_SLP_Err("Only positive integer\nvalues are valid.\n"); 
      SUMA_RETURNe;
   } 
   SUMAg_CF->X->NumForeSmoothing = (int)fv3[0];
   
   /* flag surfaces for remix */
   SUMA_SetAllRemixFlag(SUMAg_SVv, SUMAg_N_SVv);
   
   /* register a redisplay for sv*/
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_Suma, sv);
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }

   SUMA_RETURNe;
   
}

/*!
   \brief Sets the screen clipping plane 
   
   \param s (char *) a strng containing a,b,c,d parameters of plane equation 
   \param data (void *) a typecast of the pointer to the surface viewer to be affected

*/
void SUMA_SetScreenClip (char *s, void *data)
{
   SUMA_SetClip(s,(SUMA_SurfaceViewer *)data, SUMA_SCREEN_CLIP);
}
void SUMA_SetObjectClip (char *s, void *data)
{
   SUMA_SetClip(s,(SUMA_SurfaceViewer *)data, SUMA_ALL_OBJECT_CLIP);
}

void SUMA_SetClip (char *s, SUMA_SurfaceViewer *sv, SUMA_CLIP_PLANE_TYPES tp)
{
   static char FuncName[]={"SUMA_SetScreenClip"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   float fv15[15];
   int npar=0;
   char *sn;
   char namebuf[24];
   int itmp, ii, it=0;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) {
      SUMA_Show_Clip_Planes(SUMAg_CF, NULL);
      SUMA_RETURNe;
   }
   
   /* Get the name, if any */
   if ((sn = strstr(s,":"))) {/* found name */
      if (sn - s > 7) {
         SUMA_SLP_Err("Plane label too long!");
         SUMA_RETURNe;
      }
      ii=0; 
      while (s[ii] != ':') { namebuf[ii] = s[ii]; ++ii; }/* copy name */
      namebuf[ii] = '\0';
      itmp = 0; ++ii;
      while (s[ii] != '\0') { s[itmp] = s[ii]; ++ii; ++itmp; } /* copy rest */
      s[itmp] = '\0';
      /* get the equation */
      npar = SUMA_StringToNum (s, fv15, 4);
      if (npar != 4 && npar != 2  && npar != 0) { /* problem, beep and ignore */
         XBell (XtDisplay (sv->X->TOPLEVEL), 50);
         SUMA_RETURNe;
      }
      if (npar == 2) { /* the z game */
         fv15[2] = fv15[0];
         fv15[3] = fv15[1];
         fv15[0] = 0.0;
         fv15[1] = 0.0;
      } else if (npar == 0) { /* the nothing */
         fv15[0] = 0.0;
         fv15[1] = 0.0;
         fv15[2] = 0.0;
         fv15[3] = 0.0;
      }
   }else {
      SUMA_SLP_Err("Must provide plane label!");
      SUMA_RETURNe;
   }
   

   /* register fv15 with ED */
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_SetClip);
   if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv15, (void *)fv15, 
                                          SES_Suma, (void *)sv, NOPE, 
                                          SEI_Head, NULL ))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      SUMA_RETURNe;
   }
   /* register type expected */
   it = (int)tp;
   if (!(SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)(&it), 
                                          SES_Suma, (void *)sv, NOPE, 
                                          SEI_In, NextElm ))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      SUMA_RETURNe;
   }
   /* register name of plane */
   if (!(SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_s, (void*)(namebuf), 
                                          SES_Suma, (void *)sv, NOPE, 
                                          SEI_In, NextElm ))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
      SUMA_RETURNe;
   }
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }

   SUMA_RETURNe;
}

/*!
   \brief Sets the rotation center 

   \param s (char *) a strng containing X, Y, Z coordinates
   \param data (void *) a typecast of the pointer to the surface viewer to be affected
   
   NOTE: This is a bit ugly because there is a jump in surface location with the change in
   center of rotation. Something also needs updating but I am not sure what it is.  
*/

void SUMA_SetRotCenter (char *s, void *data)
{
   static char FuncName[]={"SUMA_SetRotCenter"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   float fv3[3];
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;
   
   sv = (SUMA_SurfaceViewer *)data;
   if (!sv) {
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }
   
   if (!s) {
      if (!SUMA_UpdateRotaCenter(sv, SUMAg_DOv, SUMAg_N_DOv)) {
         fprintf (SUMA_STDERR,"Error %s: Failed to update center of rotation", FuncName);
         XBell (XtDisplay (sv->X->TOPLEVEL), 50);
         SUMA_RETURNe;
      }
      SUMA_RETURNe;
   }
   
   /* parse s */
   if (SUMA_StringToNum (s, fv3, 3) != 3) { /* problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }
   
   sv->GVS[sv->StdView].RotaCenter[0] = fv3[0];
   sv->GVS[sv->StdView].RotaCenter[1] = fv3[1];
   sv->GVS[sv->StdView].RotaCenter[2] = fv3[2];
      
   
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
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

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

/*!
   \brief sends the cross hair to a certain node index
   \param s (char *) a string containing node index
   \param data (void *) a typecast of the pointer to the surface viewer to be affected

*/
void SUMA_JumpIndex (char *s, void *data)
{
   static char FuncName[]={"SUMA_JumpIndex"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   SUMA_SurfaceObject *SO= NULL;
   float fv3[3];
   int it, iv3[3];
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, fv3, 1) != 1) { /* problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
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
      SUMA_RETURNe;                                      
   } 


   /* Now set the cross hair position at the selected node*/
   SO = (SUMA_SurfaceObject *)SUMAg_DOv[sv->Focus_SO_ID].OP;
   ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3, (void*)&(SO->NodeList[3*it]),
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                          
   } 

   /* attach the cross hair to the selected surface */
   iv3[0] = SUMA_findSO_inDOv(SO->idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   iv3[1] = it;
   ED = SUMA_InitializeEngineListData (SE_BindCrossHair);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_iv3, (void*)iv3,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;
   }   
   
   /* check to see if AFNI needs to be notified */
   if (SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] && sv->LinkAfniCrossHair) {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Notifying Afni of CrossHair XYZ\n", FuncName);
      /* register a call to SetAfniCrossHair */
      SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_SetAfniCrossHair, SES_Suma, sv);
   }

   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
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
      SUMA_RETURNe;
   }

   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
   }

   /* redisplay curent only*/
   sv->ResetGLStateVariables = YUP;
   SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA); 
   
   SUMA_RETURNe;

}  

/*!
   \brief sends the cross hair to a certain XYZ location. 
   
   \param s (char *) a string containing XYZ coordinates
   \param data (void *) a typecast of the pointer to the surface viewer to be affected

   - Update to AFNI is done if linked
   - Update to other viewers is performed IF they are XYZ locked (that can get confusing)
*/
void SUMA_JumpXYZ (char *s, void *data)
{
   static char FuncName[]={"SUMA_JumpXYZ"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   float fv3[3];
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, fv3, 3) != 3) { /* problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
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
      SUMA_RETURNe;                                      
   }
   
   /* check to see if AFNI needs to be notified */
   if (SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] && sv->LinkAfniCrossHair) {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Notifying Afni of CrossHair XYZ\n", FuncName);
      /* register a call to SetAfniCrossHair */
      SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_SetAfniCrossHair, SES_Suma, sv);
   }

   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
   }

   /* now put in a request for locking cross hair but you must do this after the node selection has been executed 
   NOTE: You do not always have SetNodeElem because the list might get emptied in the call to AFNI notification.
   You should just put the next call at the end of the list.*/
   /* NOTE2: Only viewers that are XYZ locked will be affected */
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_LockCrossHair);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_Empty, NULL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Tail, NULL)) {
      SUMA_SLP_Err("Failed to register element");
      SUMA_RETURNe;
   }

   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
   }

   
   /* redisplay curent only*/
   sv->ResetGLStateVariables = YUP;
   SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);   
   
   SUMA_RETURNe;  
}

/*!
   \brief Changes the focus node without moving the cross hair
   \param s (char *) a string containing node index
   \param data (void *) a typecast of the pointer to the surface viewer to be affected

   -actions of this function are limited to the viewer that launched it
*/

void SUMA_JumpFocusNode (char *s, void *data)
{
   static char FuncName[]={"SUMA_JumpFocusNode"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   float fv3[3];
   int it;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, fv3, 1) != 1) { /* problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
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
      SUMA_SLP_Err("Failed to register element");
      SUMA_RETURNe;                                      
   } 
   
   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
   }
   
   /* redisplay curent only*/
   sv->ResetGLStateVariables = YUP;
   SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);   
   
   SUMA_RETURNe;  

}

/*!
   \brief changes the selected faceset (Focus FaceSet)
   \param s (char *) a string containing FaceSet index
   \param data (void *) a typecast of the pointer to the surface viewer to be affected

*/
void SUMA_JumpFocusFace (char *s, void *data)
{
   static char FuncName[]={"SUMA_JumpFocusFace"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   float fv3[3];
   int it;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, fv3, 1) != 1) { /* problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }
   
   
   
   /* Set the Faceselection  */
   it = (int) fv3[0];
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetSelectedFaceSet);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&it,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                      
   }
               
   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
   }
   
   /* redisplay curent only*/
   sv->ResetGLStateVariables = YUP;
   SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);   
   
   SUMA_RETURNe;  

}

/*!
   \brief Highlight a set of nodes within a box
   \param s (char *) a string containing box center followed by box size (6 values)
   \param data (void *) a typecast of the pointer to the surface viewer to be affected
   
   - operates by coloring nodes inside box.
   - coloring is not permanent and modifies other colors already present
   - operates on current viewer only
*/
void SUMA_HighlightBox (char *s, void *data)
{
   static char FuncName[]={"SUMA_HighlightBox"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   float fv15[15];
   int it;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, fv15, 6) != 6) { /* problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }
   
   /* register fv15 with ED */
   if (!list) list = SUMA_CreateList(); 
   ED = SUMA_InitializeEngineListData (SE_HighlightNodes);
   if (!SUMA_RegisterEngineListCommand (     list, ED, 
                                             SEF_fv15, (void*)fv15,
                                             SES_Suma, (void *)sv, NOPE,
                                             SEI_Head, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURNe;                                      
   }

   SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);

   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }

   
   SUMA_RETURNe;  

}
