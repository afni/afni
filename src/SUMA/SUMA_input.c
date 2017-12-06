#include "SUMA_suma.h"
#include "SUMA_plot.h"

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
         case 'a':
            SUMA_RETURN(XK_a);
         case 'A':
            SUMA_RETURN(XK_A);
         case 'b':
            SUMA_RETURN(XK_b);
         case 'B':
            SUMA_RETURN(XK_B);
         case 'd':
            SUMA_RETURN(XK_d);
         case 'D':
            SUMA_RETURN(XK_D);
         case 'g':
            SUMA_RETURN(XK_g);
         case 'G':
            SUMA_RETURN(XK_G);
         case 'j':
            SUMA_RETURN(XK_j);
         case 'J':
            SUMA_RETURN(XK_J);
         case 'l':
            SUMA_RETURN(XK_l);
         case 'L':
            SUMA_RETURN(XK_L);
         case 'm':
            SUMA_RETURN(XK_m);
         case 'M':
            SUMA_RETURN(XK_M);
         case 'n':
            SUMA_RETURN(XK_n);
         case 'N':
            SUMA_RETURN(XK_N);
         case 'o':
            SUMA_RETURN(XK_o);
         case 'O':
            SUMA_RETURN(XK_O);
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
         case 'u':
            SUMA_RETURN(XK_u);
         case 'U':
            SUMA_RETURN(XK_U);
         case 'w':
            SUMA_RETURN(XK_w);
         case 'W':
            SUMA_RETURN(XK_W);
         case 'z':
            SUMA_RETURN(XK_z);
         case 'Z':
            SUMA_RETURN(XK_Z);
         case '[':
            SUMA_RETURN(XK_bracketleft);
         case ']':
            SUMA_RETURN(XK_bracketright);
         case '.':
            if (keynameback) sprintf(keynameback,"period");
            SUMA_RETURN(XK_period);
         case ' ':
            if (keynameback) sprintf(keynameback,"space");
            SUMA_RETURN(XK_space);
         case ',':
            if (keynameback) sprintf(keynameback,"comma");
            SUMA_RETURN(XK_comma);
         default:
            SUMA_S_Errv("Key '%c' not yet supported, complain to author.\n", c);
            SUMA_RETURN(XK_VoidSymbol);
      }
   } else {
      if (SUMA_iswordsame_ci(keyname,"up") == 1) SUMA_RETURN(XK_Up);
      if (SUMA_iswordsame_ci(keyname,"down") == 1) SUMA_RETURN(XK_Down);
      if (SUMA_iswordsame_ci(keyname,"left") == 1) SUMA_RETURN(XK_Left);
      if (SUMA_iswordsame_ci(keyname,"right") == 1) SUMA_RETURN(XK_Right);
      if (SUMA_iswordsame_ci(keyname,"space") == 1) SUMA_RETURN(XK_space);
      if (SUMA_iswordsame_ci(keyname,"period") == 1) SUMA_RETURN(XK_period);
      if (SUMA_iswordsame_ci(keyname,"comma") == 1) SUMA_RETURN(XK_comma);
      if (SUMA_iswordsame_ci(keyname,"f1") == 1) SUMA_RETURN(XK_F1);
      if (SUMA_iswordsame_ci(keyname,"f2") == 1) SUMA_RETURN(XK_F2);
      if (SUMA_iswordsame_ci(keyname,"f3") == 1) SUMA_RETURN(XK_F3);
      if (SUMA_iswordsame_ci(keyname,"f4") == 1) SUMA_RETURN(XK_F4);
      if (SUMA_iswordsame_ci(keyname,"f5") == 1) SUMA_RETURN(XK_F5);
      if (SUMA_iswordsame_ci(keyname,"f6") == 1) SUMA_RETURN(XK_F6);
      if (SUMA_iswordsame_ci(keyname,"f7") == 1) SUMA_RETURN(XK_F7);
      if (SUMA_iswordsame_ci(keyname,"f8") == 1) SUMA_RETURN(XK_F8);
      if (SUMA_iswordsame_ci(keyname,"f9") == 1) SUMA_RETURN(XK_F9);
      if (SUMA_iswordsame_ci(keyname,"f10") == 1) SUMA_RETURN(XK_F10);
      if (SUMA_iswordsame_ci(keyname,"f11") == 1) SUMA_RETURN(XK_F11);
      if (SUMA_iswordsame_ci(keyname,"f12") == 1) SUMA_RETURN(XK_F12);

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
      SUMA_S_Errv("Expecting %s (or lower case version), got %s\n", \
                  tk, keyname );  \
      SUMA_RETURN(0);   \
   }  \
}

#define SUMA_KEY_SWITCH {  \
   /* Check for key switching */ \
   if (sv->State && strstr(sv->State, "GMATRIX")==sv->State) {  \
      if (SUMA_SHIFT_KEY(key)) { \
         strncpy(skeyi, key, 63); skeyi[64]='\0';  \
         SUMA_wordswap_ci(skeyi, "shift", "", skey);  \
      } else   \
         snprintf(skey,63,"shift%s",key); \
      key = skey; \
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

static int Nwarn_bracket = 0;

int SUMA_bracketleft_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode) 
{
   static char FuncName[]={"SUMA_bracketleft_Key"};
   char tk[]={"["}, keyname[100];
   int k, nc;
   char stmp[200];   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_bracketleft:
         /* getting rid of some warnings that happen with normal use */
         Nwarn_bracket = 1; /* warning always in terminal, no message to window */
         /* toggle showing left hemispheres */
         sv->ShowLeft = !sv->ShowLeft;
         /* do the axis setup */
         SUMA_WorldAxisStandard (sv->WAx, sv);
         SUMA_UpdateViewerTitle(sv);   
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         if (sv->ShowLeft) {
            sprintf(stmp,"Showing Left side%s",
               Nwarn_bracket  ? 
      "":"\nFurther notices for '[' or ']' keys will be echoed in the shell"); 
         } else {
            sprintf(stmp,"Hiding Left side%s",
               Nwarn_bracket > 1 ? 
      "":"\nFurther notices for '[' or ']' keys will be echoed in the shell");
         }
         if (!Nwarn_bracket && callmode && 
               strcmp(callmode, "interactive") == 0) { 
            SUMA_SLP_Note("%s",stmp); 
         } else { SUMA_S_Note("%s",stmp); } 
/*         ++Nwarn_bracket;*/
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre la");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}  
 
int SUMA_bracketright_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode) 
{
   static char FuncName[]={"SUMA_bracketright_Key"};
   char tk[]={"]"}, keyname[100];
   int k, nc;
   char stmp[200];   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_bracketright:
         /* getting rid of some warnings that happen with normal use */
         Nwarn_bracket = 1; /* warning always in terminal, no message to window */
         /* toggle showing left hemispheres */
         sv->ShowRight = !sv->ShowRight;
         /* do the axis setup */
         SUMA_WorldAxisStandard (sv->WAx, sv);
         SUMA_UpdateViewerTitle(sv);   
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         if (sv->ShowRight) {
            sprintf(stmp,"Showing Right side%s",
               Nwarn_bracket ? 
            "":"\nFurther notices for '[' or ']' key will be in the shell"); 
         } else {
            sprintf(stmp,"Hiding right side%s",
               Nwarn_bracket  ? 
            "":"\nFurther notices for '[' or ']' key will be in the shell");
         }
         if (!Nwarn_bracket && callmode && 
               strcmp(callmode, "interactive") == 0) { 
            SUMA_SLP_Note("%s",stmp); 
         } else { SUMA_S_Note("%s",stmp); } 
/*         ++Nwarn_bracket;*/
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre la");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}   

int SUMA_space_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_space_Key"};
   char tk[]={"SPACE"}, keyname[100];
   int k, nc;
   int nxtstateID=-1, curstateID = -1;
   int origState = -1, dov_ID = -1;
   SUMA_SurfaceObject *SO = NULL, *SOmap = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   
   SUMA_KEY_COMMON;
   
   origState = sv->iState;
   
   /* do the work */
   switch (k) {
      case XK_space:
         /* toggle between state containing mapping reference 
            of SO in focus and other view */

         /* make sure switching is OK */
         curstateID = SUMA_WhichState(sv->State, sv, sv->CurGroupName);
         if ((SO = SUMA_SV_Focus_SO(sv))) { /* have something to work with */
            if (SUMA_isLocalDomainParent (SO)) {
               /* get the last non mappable state in SV */
               if (sv->LastNonMapStateID < 0) { 
                                       /* not recorded, complain and quit */
                  fprintf(SUMA_STDERR,
                          "Warning %s: Nothing defined to toggle with yet.\n", 
                          FuncName); 
                  break;
               }

               if (LocalHead) 
                  fprintf (SUMA_STDERR,
                           "%s: surface is inherrently mappable, "
                           "switching to last non mappable state %d.\n", 
                           FuncName, sv->LastNonMapStateID);

               if (!SUMA_SwitchState ( SUMAg_DOv, SUMAg_N_DOv, sv, 
                                      sv->LastNonMapStateID, sv->CurGroupName)) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Failed in SUMA_SwitchState.\n", FuncName);
                  break;
               }

            } else {/* that's a non mappable, go to state containing reference */
               if (LocalHead) 
                  fprintf (SUMA_STDERR,
                           "%s: surface is not inherrently mappable, "
                           "searching for mapping reference and its state.\n", 
                           FuncName);

               /* find SO that is mappable reference & get its state ID*/
               dov_ID = SUMA_findSO_inDOv(SO->LocalDomainParentID, SUMAg_DOv, 
                                          SUMAg_N_DOv);
               SOmap = (SUMA_SurfaceObject *)SUMAg_DOv[dov_ID].OP;
               nxtstateID = SUMA_WhichState(SOmap->State, sv, sv->CurGroupName);

               if (nxtstateID < 0) {
                  fprintf (SUMA_STDERR,
                           "%s: Failed in SUMA_findSO_inDOv "
                           "This should not happen.\n", FuncName);
                  break;
               }

               if (LocalHead) 
                  fprintf (SUMA_STDERR,
                           "%s: Found mapping reference in viewer state %d.\n", 
                           FuncName, nxtstateID);

               /* store this location */
               sv->LastNonMapStateID = curstateID;

               /* go there */
               if (!SUMA_SwitchState ( SUMAg_DOv, SUMAg_N_DOv, sv, 
                                       nxtstateID, sv->CurGroupName)) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Failed in SUMA_SwitchState.\n", 
                           FuncName);
                  break;
               }
            }

            SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         } else {
            SUMA_LH("No surfaces in this state, space bar ignored");
         }
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre la");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}   

int SUMA_comma_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_comma_Key"};
   char tk[]={"COMMA"}, keyname[100], stmp[256];
   int k, nc, ii;
   int nxtstateID=-1, curstateID = -1;
   int origState = -1;
   char *note=NULL;
   DList *list=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   origState = sv->iState;
   
   /* do the work */
   switch (k) {
      case XK_comma:
         /* switch state, back one */
         if (sv->N_VSv < 2) break;

         curstateID = SUMA_WhichState (sv->State, sv, sv->CurGroupName);
         if (curstateID < 0) {
            SUMA_SL_Err("Current State not found.\n"
                        "Should not happen here.");
            SUMA_RETURN(0);
         }

         if (SUMAg_N_SVv > 1) {
            ii = SUMA_WhichViewerInMomentum (SUMAg_SVv, SUMAg_N_SVv, sv);
            if (ii >= 0) {
               sprintf (stmp, "You cannot switch states while other viewers\n"
                              "(like viewer %c) in momentum mode.\n", ii+65);
               SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                     stmp, FuncName, SMT_Error, SMA_LogAndPopup);
               SUMA_RETURN(0);
            }
         }

         do {
            if (LocalHead && nxtstateID > -1) {
               note = SUMA_append_string("Skipping state ",sv->State);
               note = SUMA_append_replace_string(note, 
                                             ".\nNo surfaces visible.", "", 1);
               SUMA_SLP_Note("%s",note);
               SUMA_free(note);   note = NULL;
            }

            /*fprintf(SUMA_STDERR,"%s: Current viewing state is %s ...\n", 
                      FuncName, sv->State);*/
            /* toggle to the next view state */
            nxtstateID = SUMA_PrevState(sv);
            if (nxtstateID == curstateID) break;
            if (nxtstateID < 0) {
               fprintf(SUMA_STDERR,
                       "Error %s: Failed in SUMA_PrevState.\n", FuncName);
               break;
            }
            fprintf( SUMA_STDERR,"%s: Switching from %s to %s viewing state.\n", 
                     FuncName, sv->State, sv->VSv[nxtstateID].Name);

            if (!SUMA_SwitchState ( SUMAg_DOv, SUMAg_N_DOv, sv, 
                                    nxtstateID, sv->CurGroupName)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed in SUMA_SwitchState.\n", FuncName);
               break;
            }
                  
            /* find out if there are any surfaces that will be rendered */

         } while (!SUMA_Selectable_ADOs (sv, SUMAg_DOv, NULL) && 
                   sv->iState != origState);

         /* register a call to redisplay 
            (you also need to copy the color data, 
             in case the next surface is of the same family)*/
         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);
         if (!SUMA_Engine (&list)) {
            fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
         }

         /* update titles */
         SUMA_UpdateViewerTitle(sv);
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre la");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}
   
int SUMA_period_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_period_Key"};
   char tk[]={"PERIOD"}, keyname[100], stmp[256];
   int k, nc, ii;
   int nxtstateID=-1, curstateID = -1;
   int origState = -1;
   char *note=NULL;
   DList *list=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   origState = sv->iState;
   
   /* do the work */
   switch (k) {
      case XK_period:
         /* switch state, forward one */
         if (sv->N_VSv < 2) break;

         curstateID = SUMA_WhichState (sv->State, sv, sv->CurGroupName);
         if (curstateID < 0) {
            SUMA_SL_Err("Current State not found.\n"
                        "Should not happen here.");
            SUMA_RETURN(0);
         }

         if (SUMAg_N_SVv > 1) {
            ii = SUMA_WhichViewerInMomentum (SUMAg_SVv, SUMAg_N_SVv, sv);
            if (ii >= 0) {
               sprintf (stmp, "You cannot switch states while other viewers\n"
                              "(like viewer %c) are in momentum mode.\n", ii+65);
               SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                     stmp, FuncName, SMT_Error, SMA_LogAndPopup);
               SUMA_RETURN(0);
            }
         }

         do {
            if (LocalHead && nxtstateID > -1) {
               note = SUMA_append_string("Skipping state ",sv->State);
               note = SUMA_append_replace_string(note, 
                                          ".\nNo surfaces visible.", "", 1);
               SUMA_SLP_Note("%s",note);
               SUMA_free(note);   note = NULL;
            }

            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: Current viewing state is %s ...\n", 
                                    FuncName, sv->State);

            /* toggle to the next view state */
            nxtstateID = SUMA_NextState(sv);
            if (nxtstateID == curstateID) break;
            if (nxtstateID < 0) {
               fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_NextState.\n", 
                                    FuncName);
               break;
            }

            fprintf( SUMA_STDERR,
                     "%s: Switching from %s to %s viewing state.\n", 
                     FuncName, sv->State, sv->VSv[nxtstateID].Name);

            if (!SUMA_SwitchState ( SUMAg_DOv, SUMAg_N_DOv, sv, 
                                    nxtstateID, sv->CurGroupName)) {
               fprintf( SUMA_STDERR,
                        "Error %s: Failed in SUMA_SwitchState.\n", FuncName);
               break;
            }
            SUMA_SET_AS_NEEDED_2D_VIEW_ANGLE(sv);

         } while (!SUMA_Selectable_ADOs(sv, SUMAg_DOv, NULL) && 
                   sv->iState != origState);
         /* register a call to redisplay 
         (you also need to copy the color data, in case the next surface 
          is of the same family)*/
         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, SE_Redisplay, 
                                             SES_Suma, sv);
         if (!SUMA_Engine (&list)) {
            fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", 
                             FuncName);
         }

         /* update titles */
         SUMA_UpdateViewerTitle(sv);
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre la");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}
      
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
            ++sv->ShowWorldAxis; 
            sv->ShowWorldAxis = sv->ShowWorldAxis % SUMA_N_WAX_OPTIONS; 
            sv->ShowMeshAxis = 0; 
               /* used to be = !sv->ShowMeshAxis; ,  
                  Turned off Oct 15 04 , in favor or WorldAxis */
            do_id = SUMA_GetDO_Type(SUMAg_DOv, SUMAg_N_DOv, SO_type, &n_do_id);
            if (n_do_id) {
               while (n_do_id) {
                 ((SUMA_SurfaceObject *)
                     SUMAg_DOv[do_id[n_do_id-1]].OP)->ShowMeshAxis = 
                           sv->ShowMeshAxis;
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
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F3:
         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, SE_ToggleCrossHair, 
                                             SES_Suma, sv);
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
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleShowSelectedNode, 
                                             SES_Suma, sv);
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
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, SE_ToggleShowSelectedFaceSet, 
                                             SES_Suma, sv);
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
         
         SUMA_UpdateCrossHairNodeLabelField(sv);
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
            sprintf(stmp,"Using %s color mixing mode.", 
                     SUMA_ColMixModeString(SUMAg_CF->ColMixMode)); 
            if (callmode && strcmp(callmode, "interactive") == 0) { 
                  SUMA_SLP_Note("%s",stmp); }
            else { SUMA_S_Note("%s",stmp); }
         }

         SUMA_SetAllRemixFlag (SUMAg_SVv, SUMAg_N_SVv);

         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, 
                                            SES_Suma, NULL);
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
            static int inote = 0;
            char stmp[200];
            if (sv->ortho) {
               sprintf(stmp,"Using orthographic projection viewing");
               sv->FOV[sv->iState] = sv->FOV[sv->iState] / 2.0;
            } else {
               sprintf(stmp,"Using perspective viewing");
               sv->FOV[sv->iState] = sv->FOV[sv->iState] * 2.0;
            }
            ++inote;
            if (callmode && strcmp(callmode, "interactive") == 0 && inote < 3) { 
                  SUMA_SLP_Note("%s",stmp); }
            else { SUMA_S_Note("%s",stmp); }
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

int SUMA_F9_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_F9_Key"};
   char tk[]={"F9"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   static int inote = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F9:
         sv->ShowLabelAtXhair = !sv->ShowLabelAtXhair;
         SUMA_UpdateCrossHairNodeLabelField(sv);
         {
            char stmp[200];
            if (sv->ShowLabelAtXhair) {
               sprintf(stmp,"Showing Label At Xhair");
            } else {
               sprintf(stmp,"Hiding Label At Xhair");
            }
            if (callmode && strcmp(callmode, "interactive") == 0 && inote < 2) { 
               SUMA_SLP_Note("%s",stmp); ++inote;}
            else { SUMA_S_Note("%s",stmp); }
         }
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         break; 
      default:
         SUMA_S_Err("Il ne faut pas etre hawn");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_F10_Key(SUMA_SurfaceViewer *sv,char *key, char *callmode, char *strgval)
{
   static char FuncName[]={"SUMA_F10_Key"};
   char tk[]={"F10"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   static int inote = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F10:
         if (sv->PryAx == 2) sv->PryAx = 3;
         else if (sv->PryAx == 3) sv->PryAx = 2;
         else { 
            SUMA_S_Err("Bad PryAx of %d. Reverting to 3", sv->PryAx);
            sv->PryAx = 3;
         }
         {
            char stmp[200];
            if (sv->PryAx == 3) {
               sprintf(stmp,"Prying about Z axis");
            } else {
               sprintf(stmp,"Prying about Y axis");
            }
            if (callmode && strcmp(callmode, "interactive") == 0 && inote < 2) { 
               SUMA_SLP_Note("%s",stmp); ++inote;}
            else { SUMA_S_Note("%s",stmp); }
         }
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         break; 
      default:
         SUMA_S_Err("Il ne faut pas etre la dessous");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_F11_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode, 
                 char *strgval)
{
   static char FuncName[]={"SUMA_F11_Key"};
   char tk[]={"F11"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   static int inote = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F11: {
         if ( (callmode && strcmp(callmode, "interactive") == 0) ||
             !strgval /* why not? this way the 
                        Driver can pop the interactive window */) {     
            sv->X->SetRenderOrder_prmpt = SUMA_CreatePromptDialogStruct(
                                 SUMA_OK_APPLY_CLEAR_CANCEL, 
                            "Set Object Display Order:\n"
                            "(e.g. VSG for: Volume, Surface, Graph)):", 
                                 "VSG",
                                 sv->X->TOPLEVEL, YUP,
                                 SUMA_APPLY_BUTTON,
                                 SUMA_SV_SetRenderOrder, (void *)sv,
                                 NULL, NULL,
                                 NULL, NULL,
                                 SUMA_VerifyRenderOrder, NULL,  
                                 sv->X->SetRenderOrder_prmpt);

            sv->X->SetRenderOrder_prmpt = SUMA_CreatePromptDialog(
                              sv->X->Title, sv->X->SetRenderOrder_prmpt);
         } else {
            if (!strgval) {
               SUMA_S_Err("Have NULL string");
               SUMA_RETURN(0);
            }
            SUMA_SV_SetRenderOrder(strgval, (void *)sv);
         }   
         break; } 
      default:
         SUMA_S_Err("Il ne faut pas etre hawn");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_F12_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_F12_Key"};
   char tk[]={"F12"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   static int inote = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_F12: 
         {
            /* time display speed */
            int i, nd = 20, N_vis, *Vis_IDs=NULL, NodeTot, FaceTot;
            GLfloat buf; 
            float delta_t;
            SUMA_SurfaceObject *SO=NULL;
            struct  timeval tti;
            char stmp[500], fnameout[]={"__SUMA.speedtest.txt"};
            SUMA_STRING *SS = NULL;
            FILE *fout=NULL;

            if (callmode && !strcmp(callmode, "drivesuma")) {
               fout = fopen(fnameout,"w");
            }
            SS = SUMA_StringAppend (NULL, NULL);

            buf = sv->light0_position[2];
            if (callmode && strcmp(callmode, "interactive") == 0) {
               SUMA_SLP_Note ("Timing Display speed\n"
                              "(20 displays): \n"); 
            } else {
               SUMA_S_Note("Timing Display speed\n"
                           "(20 displays): \n"); 
            }
            if (fout) fprintf(fout, "Timing Display speed (20 displays):\n"); 
            
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
            SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
            sprintf (stmp,
                     "Elapsed time: %f seconds.\n%.2f displays/second.\n", 
                     delta_t, nd/delta_t);
            SS = SUMA_StringAppend (SS, stmp);

            /* Estimate how many nodes and triangles were rendered */
            Vis_IDs = (int *)SUMA_malloc(sizeof(int)*SUMAg_N_DOv);
            N_vis = SUMA_VisibleSOs (sv, SUMAg_DOv, Vis_IDs, 0);
            NodeTot = 0;
            FaceTot = 0;
            for (i=0; i<N_vis;++i) {
               SO = (SUMA_SurfaceObject *)SUMAg_DOv[Vis_IDs[i]].OP;
               FaceTot += SO->N_FaceSet;
               NodeTot += SO->N_Node;   
            }
            if (N_vis) {
               sprintf (stmp, "In Polymode %d, rendered \n"
                              "%.2f Ktri/sec %.2f Kpnt/sec.\n",
                  sv->PolyMode,
                  (float)FaceTot / 1000.0 / delta_t  , 
                  (float)NodeTot / 1000.0 / delta_t );
               SS = SUMA_StringAppend (SS, stmp);
            }

            if (callmode && strcmp(callmode, "interactive") == 0) {
               SUMA_SLP_Note("%s",SS->s);
            } else {
               SUMA_S_Note("%s",SS->s); 
            }
            
            if (fout) {
               fprintf(fout, "%s\n", SS->s);
               SUMA_S_Note("Timing results written to file: %s", fnameout);
               fclose(fout);
            }
            
            if (Vis_IDs) SUMA_free(Vis_IDs);
            SUMA_free(SS->s);
            SUMA_free(SS);
         } 
         break; 
      default:
         SUMA_S_Err("Il ne faut pas etre hawn");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}

int SUMA_A_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_A_Key"};
   char tk[]={"A"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_A:
         if ((SUMA_CTRL_KEY(key))){ 
            
         } else {
            
         }
         break;
      case XK_a:
         /* toggle background attenuation */
         if (sv->Back_Modfact) {
            fprintf (SUMA_STDOUT,
                     "%s: Modulation by background intensity OFF.\n", FuncName);
            sv->Back_Modfact = 0;
         } else {
            fprintf (SUMA_STDOUT,
                     "%s: Modulation by background intensity ON.\n", FuncName);
            sv->Back_Modfact = SUMA_BACKGROUND_MODULATION_FACTOR;
         }

         /* set the remix flag */
         if (!SUMA_SetShownLocalRemixFlag (sv)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_SetShownLocalRemixFlag.\n", 
                     FuncName);
            break;
         }

         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         break;            
      default:
         SUMA_S_Err("Il ne faut pas ci dessous");
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

               

int SUMA_D_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_D_Key"};
   SUMA_LIST_WIDGET *LW=NULL;
   char tk[]={"D"}, keyname[100];
   int k, nc, inode = 0, N_ts = 0, ChildOverInd=-1,loc[2], ii = 0;
   float ftop = 0.1, fbot = 0.01, fs=0.0, fstep=0.0, *fv=NULL;
   int normalize = 1, polort = 2;
   SUMA_EngineData *ED = NULL; 
   SUMA_DSET *dot=NULL;
   SUMA_DSET *in_dset = NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_OVERLAYS *Sover=NULL, *child=NULL;
   SUMA_DSET *inc_dset=NULL;
   SUMA_SurfaceObject *SOC=NULL;
   DList *list = NULL;
   DListElmt *el= NULL;
   double *ts = NULL, TR=0.0;
   NI_group *ngr = NULL;
   NI_element *nel=NULL, *dotopts=NULL;
   SUMA_XFORM *xf=NULL;
   SUMA_CALLBACK *cb=NULL;
   char stmp[SUMA_MAX_NAME_LENGTH]={""};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_D:
         if ((SUMA_CTRL_KEY(key))){ 
         } else {
            SUMA_LH("The dotthing");
            /* DO the dot thing */
            if (!(SO = SUMA_SV_Focus_SO(sv))) {
               if (callmode && strcmp(callmode, "interactive") == 0) {
                  SUMA_SLP_Err("No surface in focus.\nCannot dot.");
               } else {
                  SUMA_S_Err("No surface in focus.\nCannot dot.");
               }
               SUMA_RETURN(0);
            }
            if (  !SO || !SO->SurfCont || 
                  !SO->SurfCont->curColPlane || 
                  !SO->SurfCont->curColPlane->dset_link) {
               SUMA_SL_Err("Nothing to dot");
               SUMA_RETURN(0);
            }
            Sover = SO->SurfCont->curColPlane;
            in_dset = SO->SurfCont->curColPlane->dset_link;
            inode = SO->SelectedNode;
            if (inode < 0) {
               if (callmode && strcmp(callmode, "interactive") == 0) {
                  SUMA_SLP_Warn("No selected node.\nNothing to dot.");
               }else{
                  SUMA_S_Warn("No selected node.\nNothing to dot.");
               }
               SUMA_RETURN(1);
            }
            if (SDSET_VECNUM(in_dset) < 3) {
               if (callmode && strcmp(callmode, "interactive") == 0) {
                  SUMA_SLP_Warn( "Need more than 3 values per node.\n"
                                 "Nothing to dot.");
               }else{
                  SUMA_S_Warn("Need more than 3 values per node.\n"
                              "Nothing to dot.");
               }     
               SUMA_RETURN(1);
            }
            
            
            if ((xf = SUMA_Find_XformByParent("Dot", SDSET_ID(in_dset), NULL))) {
               /* xform exists already, just flip toggle of all callbacks 
                  that claim it as a creator*/
               SUMA_SetXformActive(xf, -1*xf->active, 0);

               if (callmode && strcmp(callmode, "interactive") == 0) {
                  if (xf->active > 0) {
                     SUMA_SLP_Note("Dot product callback active");
                  } else {
                     SUMA_SLP_Note("Dot product callback inactive");
                  }
               }else {
                  if (xf->active > 0) {
                     SUMA_S_Note("Dot product callback active");
                  } else {
                     SUMA_S_Note("Dot product callback inactive");
                  }
               }
               
               SUMA_RETURN(1);
            } else { /* New Xform */
               /* We need to add a dot product transform to 
                  the dataset */
               SUMA_LH("Afresh: Adding Dot Xform");
               xf = SUMA_NewXform("Dot", 
                                  SDSET_ID(in_dset), SO->LocalDomainParentID);
               
               /* Any matching contralateral dset and a matching surface ?*/
               inc_dset = SUMA_Contralateral_dset(in_dset, SO, &SOC);
               if (inc_dset) {
                  SUMA_LHv("Found contralateral dset %s (%s)\n", 
                               SDSET_LABEL(inc_dset), SDSET_FILENAME(inc_dset));
                  if (!(SUMA_AddXformParent(xf, 
                              SDSET_ID(inc_dset), SOC->LocalDomainParentID))) {
                     SUMA_S_Err("Failed to add parent");
                     SUMA_RETURN(0);
                  }                  
               }  else {
                  SUMA_S_Note("No contralateral dset ");
               }
               /* Initialize dot product options. You'll have to redo this
                  unfortunately below... */
               if (!(SUMA_is_TimeSeries_dset(in_dset, &TR))) {
                  TR = 0.0;
               }
               SUMA_SPECT_AXIS(TR, SDSET_VECNUM(in_dset), fs, ftop, fstep);
               dotopts = SUMA_set_dotopts(NULL, SDSET_VECNUM(in_dset),
                                           0.1, fbot, 
                                           normalize, 1, 
                                           polort, NULL);
                    
                     
               SUMA_LH("Get dot product time series");   
               
               if (!(fv = (float*)SUMA_GetDsetAllNodeValsInCols2(in_dset, 
                                       NULL, 0, 
                                       inode, SO->N_Node-1, 
                                       &N_ts,
                                       SUMA_float))) { 
                  SUMA_S_Err("Failed to extract time series.");
                  SUMA_RETURN(0);
               }
               if (!(ts = SUMA_DotPreProcessTimeSeries(fv,  N_ts, 
                                             (float)TR, dotopts))) {
                  SUMA_S_Err("Failed to preprocess time series.");
                  SUMA_RETURN(0);                     
               }
               SUMA_free(fv); fv=NULL;
               
               for (ii=0; ii<xf->N_parents; ++ii) {
                  dot = NULL; /* You'll need a new dset with each pass */
                  if (!SUMA_is_ID_4_DSET(xf->parents[ii], &in_dset)) {
                           /* This is a convoluted way to get in_dset, 
                              since in_dset is known from a few lines above.
                              But it is meant to demo how to work with
                              multiple parents in xf in general */
                     SUMA_S_Err("You've really done it this time!");
                     SUMA_RETURN(0);
                  }
                  if (!SUMA_is_ID_4_SO(xf->parents_domain[ii], &SO)) {
                           /* This is a convoluted way to get SO...*/
                     SUMA_S_Err("You've really really done it this time!");
                     SUMA_RETURN(0);
                  }
                  SUMA_LHv("Creating dot for %d/%d %s\n", 
                           ii, xf->N_parents, SDSET_FILENAME(in_dset));
                  if (!(SUMA_dot_product(in_dset, ts,
                                 &dot,
                                 dotopts))) {
                     SUMA_S_Err("Failed to create dot product");
                     SUMA_RETURN(0);              
                  }
                  
                  /* Assign domain parent fields  */
                  NI_set_attribute(dot->ngr,"domain_parent_idcode",
                                   NI_get_attribute(in_dset->ngr,
                                                    "domain_parent_idcode"));
                  NI_set_attribute(dot->ngr,"geometry_parent_idcode", 
                                   NI_get_attribute(in_dset->ngr,
                                                    "geometry_parent_idcode"));
                  
                  SUMA_LHv( "Insert (%s/%s)in DsetList\n", 
                            SDSET_LABEL(dot), SDSET_ID(dot) ); 
                                                   /* no replacement allowed 
                                                   I don't think it's necessary*/
                  if (!SUMA_InsertDsetPointer (&dot, SUMAg_CF->DsetList, 0)) {
                     SUMA_S_Errv("Failed to insert pointer for %s\n", 
                                 SDSET_LABEL(dot));
                     SUMA_RETURN(0);
                  }
                  
                  /* Add child to Xform */
                  if (!SUMA_AddXformChild(xf,SDSET_ID(dot))) {
                     SUMA_S_Err("Failed to add child");
                     SUMA_RETURN(0);
                  }
                    
                  SUMA_LH("Create its overlay (child)");
                  if (!(SDSET_LABEL(dot))) SUMA_LabelDset(dot,NULL);
                  sprintf(stmp, "overlay.%s", SDSET_ID(dot));
                  child = SUMA_CreateOverlayPointer (
                                          stmp, dot, SO->idcode_str, NULL);
                  if (!child) {
                     fprintf (SUMA_STDERR, 
                              "Error %s: Failed in CreateOverlayPointer.\n" ,
                              FuncName);
                     SUMA_RETURN (0);
                  }
                  SUMA_LH("Add overlay to SO");
                  /* Add this plane to SO->Overlays */
                  if (!SUMA_AddNewPlane ( (SUMA_ALL_DO *)SO, child, 
                                          SUMAg_DOv, SUMAg_N_DOv, 0)) {
                     SUMA_SL_Crit("Failed in SUMA_AddNewPlane");
                     SUMA_FreeOverlayPointer(child);
                     SUMA_RETURN (0);
                  }
                  
                  ChildOverInd = SO->N_Overlays-1;
                  /* set the opacity, index column and the range */
                  child->GlobalOpacity = YUP;
                  child->ShowMode = SW_SurfCont_DsetViewCol;
                  child->OptScl->BrightFact = 0.8;

                  child->OptScl->find = 0;
                  child->OptScl->tind = 0;
                  child->OptScl->bind = 0;
                  child->OptScl->UseThr = 1; /* turn on threshold use */
                  child->SymIrange = 1;   /* Use symmetric range */
                  child->OptScl->AutoIntRange = 0; /* Do not update range */
                  
                  /* Leave it to the -0.5, 0.5 range below, it works better 
                     SUMA_GetDsetColRange(dot, 0, child->OptScl->IntRange, loc); 
                      */

                  SO->SurfCont->curColPlane = child;
                  
                  /* update the Dset frame */
                  if (ChildOverInd >= 0)        
                     SUMA_InitializeColPlaneShell((SUMA_ALL_DO *)SO,
                                       SO->Overlays[ChildOverInd]);
                  SUMA_UPDATE_ALL_NODE_GUI_FIELDS((SUMA_ALL_DO *)SO);
                  child->OptScl->IntRange[0] = -0.5;  /* set the range */
                  child->OptScl->IntRange[1] = 0.5;
                  SUMA_INSERT_CELL_VALUE(SO->SurfCont->SetRangeTable, 1, 1, 
                                          child->OptScl->IntRange[0]);
                  SUMA_INSERT_CELL_VALUE(SO->SurfCont->SetRangeTable, 1, 2, 
                                          child->OptScl->IntRange[1]);
                  
                  SUMA_LH("Colorize plane");
                  SUMA_ColorizePlane(child);

                  /* remix-redisplay  for surface */
                  if (!SUMA_Remixedisplay ((SUMA_ALL_DO*)SO)) {
                     SUMA_RETURN(0);
                  }

                  SUMA_LH("Refreshing Dset list");            
                  /*update the list widget if open */
                  LW = SO->SurfCont->SwitchDsetlst;
                  if (LW) {
                     if (!LW->isShaded) SUMA_RefreshDsetList ((SUMA_ALL_DO *)SO);
                  }  

                  if (LocalHead) 
                     fprintf (SUMA_STDERR,
                              "%s: Updating Dset frame, OverInd=%d\n", 
                              FuncName, ChildOverInd);
                   
               } /* For each parent */   
               
               /* done with ts */
               SUMA_free(ts); ts = NULL;
               
               /* add the dotoptions to the transform options group */
               NI_add_to_group(xf->XformOpts, dotopts);

               /* activate xform */
               SUMA_SetXformActive(xf, 1, 0);
               
               
               SUMA_LH("Add Callback ");
               /* generic parts */
               cb = SUMA_NewCallback(  "SUMA_dot_product_CB", 
                                       SUMA_NEW_NODE_ACTIVATE_EVENT,
                                       SUMA_dot_product_CB,
                                       xf->children[0], 
                                       xf->parents_domain[0],
                                       xf->idcode_str);
               

               /* add extra children */
               for (ii=1; ii<xf->N_children; ++ii) {
                  if (!SUMA_AddCallbackParent(cb, xf->children[ii],
                                              xf->parents_domain[ii])) {
                     SUMA_S_Err("Failed to add parent");
                     SUMA_RETURN(0);
                  }
               }
               
               /* fill the callback function's input parameters */
               /* The time series datasets */
               nel = NI_new_data_element("AFNI_atr", 1);
               NI_set_attribute(nel, "atr_name", "ts_dsets_idcode");
               NI_add_column_stride(nel, NI_STRING, NULL,1);
               for (ii=0; ii<xf->N_parents; ++ii) {
                  if (!SUMA_AddColAtt_CompString( nel, ii, 
                                             xf->parents[ii], SUMA_NI_CSS, 0)){
                     SUMA_S_Err("Failed to add ts_dsets_idcode");
                     SUMA_RETURN(0);
                  }
               }
               NI_add_to_group(cb->FunctionInput, nel);
               
               /* the output datasets */
               nel = NI_new_data_element("AFNI_atr", 1);
               NI_set_attribute(nel, "atr_name", "dot_dsets_idcode");
               NI_add_column_stride(nel, NI_STRING, NULL,1);
               for (ii=0; ii<xf->N_children; ++ii) {
                  if (!SUMA_AddColAtt_CompString( nel, ii, 
                                             xf->children[ii], SUMA_NI_CSS, 0)){
                     SUMA_S_Err("Failed to add dot_dsets_idcode");
                     SUMA_RETURN(0);
                  }
               }
               NI_add_to_group(cb->FunctionInput, nel);
               
               /* Node is from surface SO, but there is no point
               in setting this now because the computations 
               have been done already. Set all sources for
               getting the time series to nothing, for the 
               record.*/
               /* ts can be specified via node selection events and parents */
               SUMA_FlushCallbackEventParameters (cb);
               
               /* or ts can be explicitly set 
                  again, the existence of ts_vec is not necessary,
                  but to illustrate options ...*/
               nel = NI_new_data_element("callback.data",0);
               NI_set_attribute(nel, "data_type", "ts_vec");
               NI_add_to_group(cb->FunctionInput, nel);
                  
               /* set callback to be active, but not pending */
               cb->active = 1;
               SUMA_SetCallbackPending(cb, 0, SES_Empty);
               if (callmode && strcmp(callmode, "interactive") == 0) {
                  SUMA_SLP_Note("Dot product callback active");                                  } else {
                  SUMA_S_Note("Dot product callback active");               
               }  
                    
               if (LocalHead) {
                  SUMA_Show_Xforms(SUMAg_CF->xforms, SUMA_STDERR, 1);
                  SUMA_Show_Callbacks(SUMAg_CF->callbacks, SUMA_STDERR, 1);
               }
               
               /* initialize interface (no callbacks willget triggered)*/
               SUMA_InitializeXformInterface (xf);

            } /* New Xform */
            
         }
         break;
      case XK_d:
         if ((SUMA_CTRL_KEY(key))){ 
            /* Interactively it is handled by the mnemonic
              But if it gets here, the Driver may be driving it */
            /* opens draw ROI controller */
            if (!SUMA_OpenDrawROIController(NULL)) {
               SUMA_S_Err("Failed to open controller");
            }  
         } else {
            if (SUMAg_CF->Dev ) {
               SUMA_Show_DOv(SUMAg_DOv, SUMAg_N_DOv, stdout);
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

int SUMA_G_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_G_Key"};
   char tk[]={"G"}, keyname[100];
   int k=0, nc=-1;
   int inode = -1;
   SUMA_DSET *Dset = NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   char stmp[100]={"\0"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   if (!(ado = SUMA_SV_Focus_ADO(sv))) {
      if (callmode && strcmp(callmode, "interactive") == 0) {
         SUMA_SLP_Err("No surface in focus.\nCannot graph.");
      } else {
         SUMA_S_Err("No surface in focus.\nCannot graph.");
      }
      SUMA_RETURN(0);
   }
   if (  !ado || !(SurfCont = SUMA_ADO_Cont(ado)) || 
         !(Sover = SUMA_ADO_CurColPlane(ado)) || 
         !(Dset = Sover->dset_link)) {
      SUMA_SL_Err("Nothing to graph");
      SUMA_RETURN(0);
   }
   inode = SUMA_ADO_SelectedDatum(ado, NULL, NULL);
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
            SUMA_OverlayGraphAtNode(Sover, ado, inode); 
         }
         break;
      case XK_G:
         if (SUMAg_CF->Dev ) {
            #ifdef SUMA_USE_AFNI_GRAPH
               /* an attempt at using AFNI's graphing interface */
               SUMA_Afni_Graph(Sover, SO);
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

int SUMA_J_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode, char *strgval)
{
   static char FuncName[]={"SUMA_J_Key"};
   char tk[]={"J"}, keyname[100];
   int k=0, nc=-1;
   int inode = -1;
   SUMA_DSET *Dset = NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_OVERLAYS *Sover=NULL;
   char stmp[100]={"\0"};
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   

   if (!(ado = SUMA_SV_Focus_ADO(sv))) {
      if (callmode && strcmp(callmode, "interactive") == 0) {
         SUMA_SLP_Err("No object in focus.\nCannot Jump.");
      } else {
         SUMA_S_Err("No object in focus.\nCannot Jump.");
      }
      SUMA_RETURN(0);
   }
   /* do the work */
   switch (k) {
      case XK_j:
        if ( (callmode && strcmp(callmode, "interactive") == 0) ||
             !strgval /* why not? this way the 
                        Driver can pop the interactive window */) {     
            if (SUMA_CTRL_KEY(key)){
                 sv->X->JumpXYZ_prmpt = SUMA_CreatePromptDialogStruct( 
                                 SUMA_OK_APPLY_CLEAR_CANCEL, 
                                 "Enter XYZ to send the cross hair to:", 
                                 "",
                                 sv->X->TOPLEVEL, YUP,
                                 SUMA_APPLY_BUTTON,
                                 SUMA_JumpXYZ, (void *)sv,
                                 NULL, NULL,
                                 NULL, NULL,
                                 SUMA_CleanNumString, (void*)3,  
                                 sv->X->JumpXYZ_prmpt);
                  sv->X->JumpXYZ_prmpt = SUMA_CreatePromptDialog(
                                    sv->X->Title, sv->X->JumpXYZ_prmpt);  

             } else if (SUMA_AALT_KEY(key)){     
                  sv->X->JumpFocusNode_prmpt = SUMA_CreatePromptDialogStruct( 
                                 SUMA_OK_APPLY_CLEAR_CANCEL, 
                     "Enter index of focus node\n"
                     "Prepend/append L/R for hemiisphere selection\n"
                     "Cross hair's XYZ will not be affected:", 
                                 "",
                                 sv->X->TOPLEVEL, YUP,
                                 SUMA_APPLY_BUTTON,
                                 SUMA_JumpFocusNode, (void *)sv,
                                 NULL, NULL,
                                 NULL, NULL,
                                 SUMA_CleanNumStringSide, (void*)1,                                                    sv->X->JumpFocusNode_prmpt);

                  sv->X->JumpFocusNode_prmpt = SUMA_CreatePromptDialog(
                                    sv->X->Title, sv->X->JumpFocusNode_prmpt);

             } else {
                  sv->X->JumpIndex_prmpt = SUMA_CreatePromptDialogStruct(
                                 SUMA_OK_APPLY_CLEAR_CANCEL, 
                            "Enter index of node to send the cross hair to:\n"
                            "(prepend/append L/R for specifying hemisphere):", 
                                 "",
                                 sv->X->TOPLEVEL, YUP,
                                 SUMA_APPLY_BUTTON,
                                 SUMA_JumpIndex, (void *)sv,
                                 NULL, NULL,
                                 NULL, NULL,
                                 SUMA_CleanNumStringSide, (void*)1,  
                                 sv->X->JumpIndex_prmpt);

                  sv->X->JumpIndex_prmpt = SUMA_CreatePromptDialog(
                                    sv->X->Title, sv->X->JumpIndex_prmpt);
             }
         } else {
            if (!strgval) {
               SUMA_S_Err("Have NULL string");
               SUMA_RETURN(0);
            }
            if (SUMA_CTRL_KEY(key)){
               SUMA_JumpXYZ(strgval, (void *)sv);
            } else if (SUMA_AALT_KEY(key)){    
               SUMA_JumpFocusNode(strgval, (void *)sv);
            } else {
               SUMA_JumpIndex(strgval, (void *)sv);
            }
         }
         
         break;
      case XK_J:
         if ( (callmode && strcmp(callmode, "interactive") == 0) ||
               !strgval) {
            sv->X->JumpFocusFace_prmpt = SUMA_CreatePromptDialogStruct (
                     SUMA_OK_APPLY_CLEAR_CANCEL, 
                     "Enter index of FaceSet\nto highlight (this viewer only):", 
                     "",
                     sv->X->TOPLEVEL, YUP,
                     SUMA_APPLY_BUTTON,
                     SUMA_JumpFocusFace, (void *)sv,
                     NULL, NULL,
                     NULL, NULL,
                     SUMA_CleanNumString, (void*)1,  
                     sv->X->JumpFocusFace_prmpt);

            sv->X->JumpFocusFace_prmpt = SUMA_CreatePromptDialog(
                     sv->X->Title, sv->X->JumpFocusFace_prmpt);
         } else {
            if (!strgval) {
               SUMA_S_Err("Have NULL string");
               SUMA_RETURN(0);
            }
            SUMA_JumpFocusFace( strgval, (void *)sv);
         }
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre ici");
         SUMA_RETURN(0);
         break;
   }
   SUMA_RETURN(1);
}

int SUMA_L_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode, char *strgval)
{
   static char FuncName[]={"SUMA_L_Key"};
   char tk[]={"L"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   if (strgval) {
      SUMA_S_Warn("strgval (%s) not implemented for L_Key yet. \n"
                  "Complain to author if you want it.\n", strgval);
   }
   /* do the work */
   switch (k) {
      case XK_l:
            if ((SUMA_CTRL_KEY(key))){
               #if 0 /* Not of much use */
               if (SUMAg_CF->Dev) {
                  if (!list) list = SUMA_CreateList();
                  ED = SUMA_InitializeEngineListData (SE_ToggleLockAllCrossHair);
                  if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                   SEF_Empty, NULL, 
                                                  SES_Suma, (void *)sv, NOPE, 
                                                  SEI_Head, NULL )) {
                     fprintf( SUMA_STDERR,
                              "Error %s: Failed to register command\n", 
                              FuncName);
                     break;
                  }
                  if (!SUMA_Engine (&list)) {
                     fprintf( stderr, 
                              "Error %s: SUMA_Engine call failed.\n", FuncName);
                  }
               }
               #else
               GLfloat light0_color[] = { SUMA_LIGHT0_COLOR_INIT};
                  /* dim the lights */
                  sv->dim_spe = sv->dim_spe * 0.8; 
                     if (sv->dim_spe < 0.1) sv->dim_spe = 1.0;
                  sv->dim_dif = sv->dim_dif * 0.8; 
                     if (sv->dim_dif < 0.1) sv->dim_dif = 1.0;
                  sv->dim_amb = sv->dim_amb * 0.8; 
                     if (sv->dim_amb < 0.1) sv->dim_amb = 1.0;
                  sv->dim_emi = sv->dim_emi * 0.8; 
                     if (sv->dim_emi < 0.1) sv->dim_emi = 1.0;
                  fprintf(SUMA_STDERR,
                           "%s:  light dim factor now %.3f\n", 
                           FuncName, sv->dim_spe);
                  /*fprintf(SUMA_STDERR,"%s:  light dim factor now %.3f\n"
                                        "%f %f %f %f\n", 
                                        FuncName, sv->dim_spe,
                           sv->light0_color[0], sv->light0_color[1], 
                           sv->light0_color[2], sv->light0_color[3]);
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
                  SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, SE_Redisplay, 
                                                      SES_Suma, sv);

                  if (!SUMA_Engine (&list)) {
                     fprintf(stderr, 
                             "Error SUMA_input: SUMA_Engine call failed.\n");
                  }
               #endif
            } else if ((SUMA_AALT_KEY(key))){ /* alt + l */
               /* register cross hair XYZ with ED */
               if (!list) list = SUMA_CreateList();
               ED = SUMA_InitializeEngineListData (SE_SetLookAt);
               if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                                SEF_fv3, (void *)sv->Ch->c, 
                                                SES_Suma, (void *)sv, NOPE, 
                                                SEI_Head, NULL )) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Failed to register command\n", FuncName);
                  SUMA_RETURN(0);
               }
               if (!SUMA_Engine (&list)) {
                  fprintf(stderr, 
                           "Error %s: SUMA_Engine call failed.\n", FuncName);
               }   
            } else {
               sv->X->LookAt_prmpt = SUMA_CreatePromptDialogStruct(
                                          SUMA_OK_APPLY_CLEAR_CANCEL, 
                                          "X,Y,Z coordinates to look at:", 
                                          "0,0,0",
                                          sv->X->TOPLEVEL, YUP,
                                          SUMA_APPLY_BUTTON,
                                          SUMA_LookAtCoordinates, (void *)sv,
                                          NULL, NULL,
                                          NULL, NULL,
                                          SUMA_CleanNumString, (void*)3,  
                                          sv->X->LookAt_prmpt);
               
               sv->X->LookAt_prmpt = SUMA_CreatePromptDialog(sv->X->Title, 
                                                         sv->X->LookAt_prmpt);
               
            }
         break;
      case XK_L:
               if ((SUMA_CTRL_KEY(key))){
                  GLfloat light0_color[] = { SUMA_LIGHT0_COLOR_INIT};
                  /* brighten the lights */
                  sv->dim_spe = sv->dim_spe / 0.8; 
                  if (sv->dim_spe > 1) sv->dim_spe = 0.1;
                  sv->dim_dif = sv->dim_dif / 0.8; 
                     if (sv->dim_dif > 1) sv->dim_dif = 0.1;
                  sv->dim_amb = sv->dim_amb / 0.8; 
                     if (sv->dim_amb > 1) sv->dim_amb = 0.1;
                  sv->dim_emi = sv->dim_emi / 0.8; 
                     if (sv->dim_emi > 1) sv->dim_emi = 0.1;
                     fprintf(SUMA_STDERR,
                              "%s:  light dim factor now %.3f\n", 
                              FuncName, sv->dim_spe);
                     /*fprintf(SUMA_STDERR,"%s:  light dim factor now %.3f\n"
                                           "%f %f %f %f\n", 
                                           FuncName, sv->dim_spe,
                              sv->light0_color[0], sv->light0_color[1], 
                              sv->light0_color[2], sv->light0_color[3]);
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
                     SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, SE_Redisplay, 
                                                         SES_Suma, sv);

                     if (!SUMA_Engine (&list)) {
                        fprintf(stderr, 
                                "Error SUMA_input: SUMA_Engine call failed.\n");
                     }
               } else if ((SUMA_AALT_KEY(key))){
               
               } else {
                  SUMA_PROMPT_DIALOG_STRUCT *prmpt;
                  prmpt = SUMA_CreatePromptDialogStruct (
                                 SUMA_OK_APPLY_CLEAR_CANCEL, 
                                 "X,Y,Z coordinates of light0:", 
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
               mcw_malloc_dump_sort(1); 
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
               SUMA_RETURN(0);
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
            SUMA_LH("Opening a new controller...");
            /* open a new controller */
            if (!SUMA_X_SurfaceViewer_Create ()) {
               SUMA_S_Err("Failed in SUMA_X_SurfaceViewer_Create.");
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
   Execute commands when O or o is pressed
*/
int SUMA_O_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_O_Key"};
   char tk[]={"O"}, keyname[100];
   int k, nc;
   int N_SOlist, SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS];
   SUMA_ALL_DO *ado=NULL;
   SUMA_SurfaceObject *SO = NULL;
   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;

   /* do the work */
   switch (k) {
      case XK_O:
         if ((SUMA_APPLE_KEY(key) || SUMA_ALT_KEY(key))) {
            
         } else if (SUMA_CTRL_KEY(key)) {
            if ((ado = SUMA_SV_Focus_ADO(sv))) {
               SUMA_Set_ADO_TransMode(ado, sv->TransMode, 
                                      SUMAg_CF->TransModeStep, 1);
               SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
            }
         } else {   
            sv->TransMode = ((sv->TransMode-SUMAg_CF->TransModeStep) % 
                                                      (STM_N_TransModes-2));
            if (sv->TransMode <= STM_ViewerDefault) sv->TransMode = STM_16;
           
            SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         }
         break;
      case XK_o:
         if ((SUMA_APPLE_KEY(key) || SUMA_ALT_KEY(key))) {
           sv->X->SetRot_prmpt = SUMA_CreatePromptDialogStruct (
                  SUMA_OK_APPLY_CLEAR_CANCEL, "Center of Rotation X,Y,Z:", 
                  "0,0,0",
                  sv->X->TOPLEVEL, YUP,
                  SUMA_APPLY_BUTTON,
                  SUMA_SetRotCenter, (void *)sv,
                  NULL, NULL,
                  NULL, NULL,
                  NULL, NULL,  
                  sv->X->SetRot_prmpt);

            sv->X->SetRot_prmpt = SUMA_CreatePromptDialog(sv->X->Title, 
                                                          sv->X->SetRot_prmpt);
         } else if (SUMA_CTRL_KEY(key)) {
            if ((ado = SUMA_SV_Focus_ADO(sv))) {
               SUMA_Set_ADO_TransMode(ado, sv->TransMode, 
                                      -SUMAg_CF->TransModeStep, 1);
               SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
            }
         } else {   
            sv->TransMode = ((sv->TransMode+SUMAg_CF->TransModeStep) % 
                                                      (STM_N_TransModes-2));
            if (sv->TransMode <= STM_ViewerDefault) sv->TransMode = STM_0;

            SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
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
   char tk[]={"P"}, keyname[100], msg[100];
   int k, nc;
   int N_SOlist, SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS];
   SUMA_SurfaceObject *SO = NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;

   /* do the work */
   switch (k) {
      case XK_P:
         if ((SUMA_APPLE_KEY(key) || SUMA_ALT_KEY(key))) {
         } else if (SUMA_CTRL_KEY(key)) {
         
         } else {
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
         }
         break;
      case XK_p:
         if ((SUMA_APPLE_KEY(key) || SUMA_ALT_KEY(key))) {
            sv->DO_DrawMask = ((sv->DO_DrawMask+1) % SDODM_N_DO_DrawMasks);
            snprintf(msg,100*sizeof(char),"DO DrawMask now set to: %s", 
                        SUMA_DO_DrawMaskCode2Name_human(sv->DO_DrawMask));
            if (callmode && strcmp(callmode, "interactive") == 0) { 
                  SUMA_SLP_Note ("%s",msg); 
            } else { SUMA_S_Note ("%s",msg); }
         } else if (SUMA_CTRL_KEY(key)) {
            if ((ado = SUMA_SV_Focus_ADO(sv))) {
               SUMA_Set_ADO_RenderMode(ado, sv->PolyMode, -1, 1);
            }
         } else {
            sv->PolyMode = ((sv->PolyMode+1) % SRM_N_RenderModes);
            if (sv->PolyMode <= SRM_ViewerDefault) sv->PolyMode = SRM_Fill;

            SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
         }
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
            SUMAg_CF->SUMA_SnapshotOverSampling = 
                  (SUMAg_CF->SUMA_SnapshotOverSampling +1)%5;
            if (SUMAg_CF->SUMA_SnapshotOverSampling == 0) 
                     SUMAg_CF->SUMA_SnapshotOverSampling = 1;
            { 
               sprintf(msg,"Oversampling now set to %d", 
                           SUMAg_CF->SUMA_SnapshotOverSampling);
               if (callmode && strcmp(callmode, "interactive") == 0) { 
                  SUMA_SLP_Note ("%s",msg); 
               } else { SUMA_S_Note ("%s",msg); }
            }
         } else if (SUMA_CTRL_KEY(key)) {
            #if 0
            sv->X->SetRot_prmpt = SUMA_CreatePromptDialogStruct (
                  SUMA_OK_APPLY_CLEAR_CANCEL, "Center of Rotation X,Y,Z:", 
                  "0,0,0",
                  sv->X->TOPLEVEL, YUP,
                  SUMA_APPLY_BUTTON,
                  SUMA_SetRotCenter, (void *)sv,
                  NULL, NULL,
                  NULL, NULL,
                  NULL, NULL,  
                  sv->X->SetRot_prmpt);

            sv->X->SetRot_prmpt = SUMA_CreatePromptDialog(sv->X->Title, 
                                                          sv->X->SetRot_prmpt);
            #else
            /* save image to disk */
            SUMA_SnapToDisk(sv,1, 0);
            #endif
         } else {
            GLvoid *pixels=NULL;
            double rat;
            int oareah=-1,oareaw=-1, owindh=-1, owindw=-1;
            /* Control for GL_MAX_VIEWPORT_DIMS */
            if (SUMAg_CF->SUMA_SnapshotOverSampling > 1) {
               glGetIntegerv(GL_MAX_VIEWPORT_DIMS,&k);
               mm = SUMA_MAX_PAIR(
                     SUMAg_CF->SUMA_SnapshotOverSampling*sv->X->aHEIGHT, 
                     SUMAg_CF->SUMA_SnapshotOverSampling*sv->X->aWIDTH);
               if (mm > k) { /* too big, find best new dimesnions */
                  rat = (double)mm/(double)k; 
                     /*window shrinking factor to allow for stitching*/
                  SUMA_S_Notev(  "%d/%d (H/W) Too big for oversampling\n"
                                 " reducing resolution by %f.\n", 
                                 sv->X->aHEIGHT, sv->X->aWIDTH, rat);
                  /* store original size */
                  oareaw = sv->X->aWIDTH; oareah = sv->X->aHEIGHT;
                  owindw = sv->wWindWidth; owindh = sv->wWindHeight;  
                  sv->X->aHEIGHT = 
                     (int)((double)sv->X->aHEIGHT/rat)-1;
                  sv->X->aWIDTH = 
                     (int)((double)sv->X->aWIDTH/rat)-1;
                  SUMA_SV_WindDims_From_DrawAreaDims(sv);
                  SUMA_WidgetResize (sv->X->TOPLEVEL , 
                                     sv->wWindWidth, sv->wWindHeight);
                  sv->rdc = SUMA_RDC_X_RESIZE;
                  glViewport( 0, 0, 
                                 sv->X->aWIDTH, sv->X->aHEIGHT);  
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
                           SUMAg_CF->SUMA_SnapshotOverSampling * 
                              SUMAg_CF->SUMA_SnapshotOverSampling,
                           k,
                           SUMA_MAX_PAIR( SUMAg_CF->SUMA_SnapshotOverSampling * 
                              sv->X->aHEIGHT,
                           SUMAg_CF->SUMA_SnapshotOverSampling*sv->X->aWIDTH)  );
                     } else {
                        /* sometimes you have repeated black areas when 
                        oversampling, allow that after very first 'tant' */
                        SNAP_OkDuplicates();
                     }
                     /* start from top left, move to right then go down 
                        one row (Row Major, starting on top left ) */
                     glViewport(-ii*sv->X->aWIDTH,  
                                -(SUMAg_CF->SUMA_SnapshotOverSampling - jj - 1) *
                                  sv->X->aHEIGHT, 
                               SUMAg_CF->SUMA_SnapshotOverSampling*sv->X->aWIDTH,
                                SUMAg_CF->SUMA_SnapshotOverSampling * 
                                 sv->X->aHEIGHT);
                     SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
                     #if 0 /* problem should be fixed by SUMA_grabRenderedPixels
                              Throw section out if no new problems arise.
                              Search for KILL_DOUBLE_RENDERING to locate
                              other chunks for removal 
                                       ZSS Feb 2012 */
                        if (1) {
                           /* seems to fix an problem with snapping the older 
                           image... at least on mac
                           None of glFlush(); glFinish();glXWaitGL();glXWaitX(); 
                           or NI_sleep did the trick               
                           Perhaps the wrong buffer is being grabbed? 
                           Check SUMA_grabPixels ...
                                 ZSS Feb 2012. 
                           Yes it was,  SUMA_grabRenderedPixels does the trick.
                                 ZSS Feb the next morning 2012 */
                           SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
                        }
                     #endif
                  } else {
                     #if 0 /* Search for KILL_DOUBLE_RENDERING to locate
                              other chunks for removal 
                                       ZSS Feb 2012 */
                  /* ZSS   Nov 20 2009 
                     If you do not redisplay here, you could strange cases of
                     snapping the previous frame as reported by Colm Connolly.

                  1. suma -spec N27_both_tlrc.spec -sv TT_N27+tlrc. &
                  2. press F2 five times to cycle through the various axes 
                     from none to all and back to none.
                  3. press r to record

                  The first image recorded has axes present even though none 
                  are present in the viewer. Pressing r again produces an 
                  image with no axes as expected.

                  Actually, it seems this happens in many other cases, F1, F6,
                  change state, etc. 

                  This seems to be the same problem reported by Chunmao W. 
                  a while back. 
                  Same happens with R option. 

                  Problem only happens under DARWIN it seems.

                  I do not know why the call to SUMA_handleRedisplay does the 
                  trick. Perhaps it is a buffer reading problem in double 
                  buffer rendering. The fix is ugly, especially in continuous
                  record mode (see SUMA_display function in 'if(csv->record)'
                  block), but it works.
                  */
                        #ifdef DARWIN
                        SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
                        #endif
                     #endif
                  }
                  pixels = SUMA_grabRenderedPixels(sv, 3, 
                                       sv->X->aWIDTH, sv->X->aHEIGHT, 0);
                  if (pixels) {
                    ISQ_snapsave (sv->X->aWIDTH, -sv->X->aHEIGHT, 
                                  (unsigned char *)pixels, sv->X->GLXAREA ); 
                    SUMA_free(pixels);
                  }else {
                     if (callmode && strcmp(callmode, "interactive") == 0) {
                        SUMA_SLP_Err("Failed to record image.");
                     } else { SUMA_S_Err("Failed to record image.");}
                  }
               }
            }
            if (SUMAg_CF->SUMA_SnapshotOverSampling > 1) {  
                        /* Now return the window to its previous size */
               if (owindw > 0) {
                  sv->wWindHeight = owindh;
                  sv->X->aHEIGHT = oareah;
                  sv->wWindWidth = owindw;
                  sv->X->aWIDTH = oareaw;
                  SUMA_WidgetResize (sv->X->TOPLEVEL , owindw, owindh);   
               }
               sv->rdc = SUMA_RDC_X_RESIZE;
               glViewport( 0, 0, 
                           sv->X->aWIDTH, sv->X->aHEIGHT);
               SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
            }
            if (SUMAg_CF->NoDuplicatesInRecorder) SNAP_NoDuplicates();
            else SNAP_OkDuplicates();
            if (SUMAg_CF->SUMA_SnapshotOverSampling > 1) {
               /* record the image to make life easy on user */
               sprintf(msg,"Writing resultant image\n"
                           " to HighRes_Suma_tmp.ppm ...");
               if (callmode && strcmp(callmode, "interactive") == 0) { 
                  SUMA_SLP_Note ("%s",msg); 
               } else { SUMA_S_Note ("%s",msg); }
               ISQ_snap_png_rng("HighRes_Photo___tmp",
                                -(SUMAg_CF->SUMA_SnapshotOverSampling * 
                                  SUMAg_CF->SUMA_SnapshotOverSampling),
                                0);
               /* use explicit tcsh to avoid sh syntax  25 Apr 2017 [rickr] */
               system(  "tcsh -c 'rm -f HighRes_Suma_tmp* >& /dev/null' ; "
                        "imcat -prefix HighRes_Suma_tmp HighRes_Photo___tmp* ; "
                        "rm -f HighRes_Photo___tmp*");
            }
         }
         break;
      case XK_R:
         if (SUMA_CTRL_KEY(key)) {
            char sbuf[256];
            sv->Record = !sv->Record;
            if (sv->Record) sv->Record = 2;
            if (sv->Record) {
               SUMA_VALIDATE_RECORD_PATH(SUMAg_CF->autorecord);
               snprintf(sbuf,256*sizeof(char), 
                        "Disk Recording ON to: %s%s*",
                           SUMAg_CF->autorecord->Path,
                           SUMAg_CF->autorecord->FileName_NoExt);
               if (callmode && strcmp(callmode, "interactive") == 0) { 
                  SUMA_SLP_Note ("%s",sbuf); }
               else { SUMA_S_Note ("%s",sbuf); }
            } else { 
               snprintf(sbuf,256*sizeof(char), 
                        "Disk Recording OFF. Results in: %s%s*",
                           SUMAg_CF->autorecord->Path,
                           SUMAg_CF->autorecord->FileName_NoExt);
               if (callmode && strcmp(callmode, "interactive") == 0) { 
                  SUMA_SLP_Note ("%s",sbuf); }
               else { SUMA_S_Note ("%s",sbuf);} 
            }
            SUMA_UpdateViewerTitle(sv);
         } else {
            sv->Record = !sv->Record;
            if (sv->Record) { 
               if (callmode && strcmp(callmode, "interactive") == 0) { 
                  SUMA_SLP_Note ("Recording ON"); }
               else { SUMA_S_Note ("Recording ON"); }
            } else { 
               if (callmode && strcmp(callmode, "interactive") == 0) { 
                  SUMA_SLP_Note ("Recording OFF"); }
               else { SUMA_S_Note ("Recording OFF");} 
            }
            SUMA_UpdateViewerTitle(sv);
         }
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
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, 
                                    SE_StartListening, SES_Suma, sv);

         if (!SUMA_Engine (&list)) {
               fprintf(SUMA_STDERR, 
                        "Error %s: SUMA_Engine call failed.\n", FuncName);
         } 
         break;
      case XK_t:
         if ((SUMA_CTRL_KEY(key))){
               if (callmode && strcmp(callmode, "interactive") == 0) {
                     SUMA_SLP_Note("Forcing a resend of Surfaces to Afni...");}
               else { SUMA_S_Note("Forcing a resend of Surfaces to Afni..."); }
               if (!list) list = SUMA_CreateList();
               SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, 
                        SE_SetForceAfniSurf, SES_Suma, sv);

               if (!SUMA_Engine (&list)) {
                  fprintf(SUMA_STDERR, 
                           "Error %s: SUMA_Engine call failed.\n", FuncName);
               }
         } else {
            if (!list) list = SUMA_CreateList();
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, 
                           SE_ToggleConnected, SES_Suma, sv);

            if (!SUMA_Engine (&list)) {
                  fprintf(SUMA_STDERR, 
                           "Error %s: SUMA_Engine call failed.\n", FuncName);
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
   Execute commands when U or u is pressed
*/
int SUMA_U_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_U_Key"};
   char tk[]={"U"}, keyname[100], msg[100];
   int k, nc, ii, jj, mm;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   SUMA_KEY_COMMON;

   /* do the work */
   switch (k) {
      case XK_u:
         if ((SUMA_APPLE_KEY(key) || SUMA_ALT_KEY(key))) {
            SUMA_LH("Nothing here");   
         } else if (SUMA_CTRL_KEY(key)) {
            SUMA_viewSumaCont(1);
         } else {
            SUMA_LH("Keeping it real");
         }
         break;
      case XK_U:
         if (SUMA_CTRL_KEY(key)) {
            
         } else {
            
         }
         break;
      default:
         SUMA_S_Err("Il ne faut pas etre ici non plus");
         SUMA_RETURN(0);
         break;
   }

   SUMA_RETURN(1);
}


void SUMA_free_Save_List_El(void *selu) {
   SUMA_SAVE_LIST_EL *sel=(SUMA_SAVE_LIST_EL *)selu;
   if (sel) {
      if (sel->identifier) SUMA_free(sel->identifier);
      if (sel->prefix) SUMA_free(sel->prefix);
      if (sel->type) SUMA_free(sel->type);
      SUMA_free(sel);
   }
   return;
}

int SUMA_Add_to_SaveList(DList **SLp, char *type, 
                         char *identifier, char *prefix) 
{
   static char FuncName[]={"SUMA_Add_to_SaveList"};
   DList *SL=NULL;
   DListElmt *el= NULL;
   SUMA_SAVE_LIST_EL *sel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SLp || !type || !identifier || !prefix) SUMA_RETURN(0);
   SL = *SLp;
   if (!SL) {
      SL = (DList*)SUMA_malloc(sizeof(DList));
      dlist_init(SL, SUMA_free_Save_List_El);
   }
   SUMA_LH("Searching for possible identifier >%s<", 
            identifier?identifier:"NULL");
   /* first make sure identifier is not there already */
   el = dlist_head(SL);
   while (el && identifier) {
      if ((sel = (SUMA_SAVE_LIST_EL *)el->data)) {
         if (sel->identifier &&
             !strcmp(sel->identifier, identifier)) {
            /* found, replace */
            SUMA_free(sel->identifier); 
            sel->identifier = SUMA_copy_string(identifier);
                  identifier = NULL;
            SUMA_free(sel->prefix);
            sel->prefix = SUMA_copy_string(prefix);
                  prefix = NULL;  
            SUMA_free(sel->type);
            sel->type = SUMA_copy_string(type);
                  type = NULL;  
         }
      }
      el = dlist_next(el);
   }
   if (identifier) { /* a new one, should add it */
      sel = (SUMA_SAVE_LIST_EL *)SUMA_calloc(1,sizeof(SUMA_SAVE_LIST_EL));
      sel->identifier = SUMA_copy_string(identifier);
      sel->prefix = SUMA_copy_string(prefix);
      sel->type =  SUMA_copy_string(type);
      dlist_ins_next(SL, dlist_tail(SL), (void *)sel);
   }
   
   if (LocalHead) {
      SUMA_Show_SaveList(SL, "SaveList now:\n");
   }
     
   *SLp = SL;
   SUMA_RETURN(1);
}

void SUMA_Show_SaveList(DList *SL, char *head) 
{
   static char FuncName[]={"SUMA_Show_SaveList"};
   FILE *out=NULL;
   DListElmt *el= NULL;
   SUMA_SAVE_LIST_EL *sel=NULL;
   int cnt = 0;
   
   SUMA_ENTRY;
      
   if (!out) out = stderr;
   if (head) { fprintf(out, "%s", head); }
   if (!SL) { fprintf(out,"NULL SaveList\n"); SUMA_RETURNe; }
      
   el = dlist_head(SL);
   cnt = 0;
   while (el) {
      if ((sel = (SUMA_SAVE_LIST_EL *)el->data)) {
         fprintf(out,"   %d:     id>%s<, prefix>%s<, type>%s<\n", 
                        cnt, sel->identifier, sel->prefix, sel->type);
      } else {
         fprintf(out,"   %d:     NULL sel\n", cnt);
      }
      el = dlist_next(el);
      fprintf(out,"\n");
   }

   SUMA_RETURNe;
}

int SUMA_SaveSaveListElement(SUMA_SAVE_LIST_EL *sel) 
{
   static char FuncName[]={"SUMA_SaveSaveListElement"};
   SUMA_DSET *dset=NULL;
   char *oname=NULL, *idtype=NULL;
   int nid=0;
   SUMA_ENTRY;
   
   if (!sel || !sel->identifier || !sel->prefix || !sel->type) SUMA_RETURN(0);
   
   if (!strcmp(sel->type,"sdset")) {
      idtype="label:"; nid = strlen(idtype);
      if (!strncmp(idtype, sel->identifier, nid)) {
         if (!(dset = SUMA_FindDset2_s(sel->identifier+nid, 
                                 SUMAg_CF->DsetList, "label"))) {
            SUMA_S_Errv("Failed to find dset labeled %s\n", sel->identifier+nid);
            SUMA_RETURN(0);
         }
         goto SAVEDSET;
      } 
      idtype="filename:"; nid = strlen(idtype);
      if (!strncmp(idtype, sel->identifier, nid)) {
         if (!(dset = SUMA_FindDset2_s(sel->identifier+nid, 
                                 SUMAg_CF->DsetList, "filename"))) {
            SUMA_S_Errv("Failed to find dset with filename %s\n", 
                        sel->identifier+nid);
            SUMA_RETURN(0);
         }
         goto SAVEDSET;
      }
      idtype="self_idcode:"; nid = strlen(idtype);
      if (!strncmp(idtype, sel->identifier, nid)) {
         if (!(dset = SUMA_FindDset2_s(sel->identifier+nid, 
                                 SUMAg_CF->DsetList, "self_idcode"))) {
            SUMA_S_Errv("Failed to find dset with idcode %s\n", 
                        sel->identifier+nid);
            SUMA_RETURN(0);
         }
         goto SAVEDSET;
      }
      /* last hurrah */
      if (!(dset = SUMA_FindDset2_s(sel->identifier, 
                                 SUMAg_CF->DsetList, NULL))) {
         SUMA_S_Errv("Failed to find dset with identifier %s\n", 
                     sel->identifier);
         SUMA_RETURN(0);
      }
      goto SAVEDSET;
      
      SAVEDSET:
      if (!dset) SUMA_RETURN(0);
      oname = SUMA_WriteDset_PreserveID(sel->prefix, dset,
                                        SUMA_NO_DSET_FORMAT, 1,0);
      SUMA_S_Notev("Wrote: %s\n", oname);
      if (oname) SUMA_free(oname);
   } else {
      SUMA_S_Errv("Not setup for type %s yet\n", sel->type);
      SUMA_RETURN(0);
   }
   SUMA_RETURN(1);
}

int SUMA_W_Key(SUMA_SurfaceViewer *sv, char *key, char *callmode)
{
   static char FuncName[]={"SUMA_W_Key"};
   char tk[]={"W"}, keyname[100];
   int k, nc;
   SUMA_EngineData *ED = NULL; 
   SUMA_SurfaceObject *SO;
   DList *list = NULL;
   DListElmt *el= NULL;
   char *lbls=NULL;
   SUMA_SAVE_LIST_EL *sel=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   /* do the work */
   switch (k) {
      case XK_W:
         if ((SUMA_CTRL_KEY(key))){
            if (!SUMAg_CF->SaveList || !dlist_size(SUMAg_CF->SaveList)) {
               SUMA_S_Note("Nothing in SaveList");
               SUMA_RETURN(1);
            }
            while((el = dlist_head(SUMAg_CF->SaveList))) {
               sel = (SUMA_SAVE_LIST_EL *)el->data;
               if (sel->identifier) {
                  if (!(SUMA_SaveSaveListElement(sel))) {
                     SUMA_S_Warnv("Failed to save %s %s\n", 
                                  sel->identifier, sel->prefix)
                  }
               }
               dlist_remove(SUMAg_CF->SaveList, el, (void *)(&sel));
            }
         } else {
            if ((SO=SUMA_SV_Focus_SO(sv))) {
               if (!list) list = SUMA_CreateList();
               ED = SUMA_InitializeEngineListData (SE_SaveSOFileSelection);
               if (!(el = SUMA_RegisterEngineListCommand (  list, ED,
                                                SEF_vp, (void *)SO,
                                                SES_Suma, (void *)sv, NOPE,
                                                SEI_Head, NULL))) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed to register command.\n", 
                           FuncName);
               }

               if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, sv->X->TOPLEVEL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, el)) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed to register command.\n", 
                           FuncName);
               }  

               if (!SUMA_Engine (&list)) {
                  fprintf( SUMA_STDERR, 
                           "Error %s: SUMA_Engine call failed.\n", FuncName);
               }
            }
         }
         break;
         
      case XK_w:
           if (SUMAg_CF->Dev) {
               if ((SO=SUMA_SV_Focus_SO(sv))) {
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
                  /* call the function to form labels and notify window */
                  lbls = SUMA_GetLabelsAtSelection((SUMA_ALL_DO *)SO,
                                               SO->SelectedNode, -1);
                  if (lbls) SUMA_free(lbls); lbls = NULL;
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
            sv->ZoomCompensate = sv->FOV[sv->iState] / SUMA_sv_auto_fov(sv);
            if (sv->ZoomCompensate > 1) sv->ZoomCompensate = 1.0; 
               /* weird stuff at zc_fac higher that 1.5 */
            else if (sv->ZoomCompensate < 0.005) sv->ZoomCompensate = 0.005; 
         }
         SUMA_postRedisplay(sv->X->GLXAREA, NULL, NULL);
         break;

      case XK_z:
         sv->FOV[sv->iState] /= (1-sv->KeyZoomGain); 
         if (sv->ortho) { 
            if (sv->FOV[sv->iState] > FOV_MAX/2.0) { 
               SUMA_BEEP; sv->FOV[sv->iState] = FOV_MAX/2.0; }
         } else {
            if (sv->FOV[sv->iState] > FOV_MAX) { 
               SUMA_BEEP; sv->FOV[sv->iState] = FOV_MAX; }
         }
         /*fprintf(stderr,"Zoom out %f\n", sv->FOV[sv->iState]);*/
         /* Now update the zoom compensation variable */
         if (sv->ZoomCompensate) {
            sv->ZoomCompensate = sv->FOV[sv->iState] / SUMA_sv_auto_fov(sv);
            if (sv->ZoomCompensate > 1) sv->ZoomCompensate = 1.0; 
               /* weird stuff at zc_fac higher that 1.5 */
            else if (sv->ZoomCompensate < 0.005) sv->ZoomCompensate = 0.005; 
               /* weird stuff cause by integer spin variables! 
                  Proper way to handle all this is with float position 
                  storage and no recalculation of zc_fac except at zooming.*/ 
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
   char tk[]={"Up"}, keyname[100], skey[65], skeyi[65];
   int k, nc, ii, inode = -1;
   float ArrowDeltaRot = 0.05; 
      /* The larger the value, the bigger the rotation increment */
   Widget w;
   double dd[3] = {0.0, -1.0, 0.0}; /* up */
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   SUMA_KEY_SWITCH;

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
               /*fprintf (SUMA_STDERR,"%s: Shift up\n", FuncName);*/
               sv->GVS[sv->StdView].translateVec[1] += 
                (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaY / 
                (float)sv->X->aHEIGHT*sv->GVS[sv->StdView].TranslateGain;
               SUMA_postRedisplay(w, NULL, NULL);
            }else if (SUMA_CTRL_KEY(key)){
               /*fprintf (SUMA_STDERR,"%s: Control Up\n", FuncName);*/
               /* Top view ctrl+up*/
               float a[3];
               /* Default top view, rotate by nothing */
               a[0] = 1.0; a[1] = 0.0; a[2] = 0.0;
               axis_to_quat(a, 0, sv->GVS[sv->StdView].currentQuat);
               SUMA_postRedisplay(w, NULL, NULL);
            }else if (SUMA_AALT_KEY(key)) {
               SUMA_LH("alt down");
               if ((SO = SUMA_SV_Focus_SO(sv))) {
                  if (SO->SelectedNode < 0 ||
                      !SO->FN) SUMA_RETURN(1); /* nothing to do */
                  inode = SO->SelectedNode;
                  {
                     inode = 
                        SUMA_NodeNeighborAlongScreenDirection(sv, SO, inode, dd);
                     if (inode == -2) {
                        SUMA_S_Err("Failed in"
                                   " SUMA_NodeNeighborAlongScreenDirection");
                        SUMA_RETURN(0);
                     } else if (inode == -1) {
                        SUMA_LH("No good direction, get out");
                        SUMA_BEEP;
                        break;
                     } else {
                        SUMA_LHv("Next node should be %d\n", inode);
                     }                           
                  }  
                  /* Now set the new selected node */
                  if (inode >= 0 && inode != SO->SelectedNode) {
                     char stmp[64]; /* use Jump callback, 
                                       the easy route */
                     sprintf(stmp,"%d", inode); 
                     SUMA_JumpIndex (stmp, (void *)sv);
                  }
               }
            } else {
               if (LocalHead) 
                  fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);
               trackball_Phi(sv->GVS[sv->StdView].deltaQuat, 
                  0.0, -ArrowDeltaRot, /* first point */
                  0.0, ArrowDeltaRot, /* ending x,y */
                  sv->ArrowRotationAngle);
               if (LocalHead) {
                  fprintf(stdout,"\ncurrentQuat\n");
                  for (ii=0; ii<4; ++ii) { 
                     fprintf( stdout,"%f\t", 
                              sv->GVS[sv->StdView].currentQuat[ii]);
                  } 
                  fprintf(stdout,"\n");
                  fprintf(stdout,"\ndeltaQuat\n");
                  for (ii=0; ii<4; ++ii) { 
                     fprintf(stdout,"%f\t", sv->GVS[sv->StdView].deltaQuat[ii]);
                  } 
                  fprintf(stdout,"\n");
               }
               add_quats ( sv->GVS[sv->StdView].deltaQuat, 
                           sv->GVS[sv->StdView].currentQuat, 
                           sv->GVS[sv->StdView].currentQuat);
               if (LocalHead) {
                  fprintf(stdout,"\nnewQuat\n");
                  for (ii=0; ii<4; ++ii) { 
                     fprintf( stdout,"%f\t", 
                              sv->GVS[sv->StdView].currentQuat[ii]);
                  } 
                  fprintf(stdout,"\n");
               }
               sv->GVS[sv->StdView].spinDeltaX = 0;
               sv->GVS[sv->StdView].spinDeltaY = 
                                    2.0*ArrowDeltaRot*sv->X->aHEIGHT;
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
   char tk[]={"Down"}, keyname[100], skey[65], skeyi[65];
   int k, nc, ii, inode=-1;
   float ArrowDeltaRot = 0.05; 
         /* The larger the value, the bigger the rotation increment */
   Widget w;
   double dd[3] = {0.0, 1.0, 0.0}; /* down */
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   SUMA_KEY_SWITCH;
      
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
               sv->GVS[sv->StdView].translateVec[1] -=  
                  (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaY /           
                  (float)sv->X->aHEIGHT*sv->GVS[sv->StdView].TranslateGain;
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
               SUMA_LH("alt down");
               if ((SO = SUMA_SV_Focus_SO(sv))) {
                  if (SO->SelectedNode < 0 ||
                      !SO->FN) SUMA_RETURN(1); /* nothing to do */
                  inode = SO->SelectedNode;
                  {
                     inode = 
                        SUMA_NodeNeighborAlongScreenDirection(sv, SO, inode, dd);
                     if (inode == -2) {
                        SUMA_S_Err("Failed in"
                                   " SUMA_NodeNeighborAlongScreenDirection");
                        SUMA_RETURN(0);
                     } else if (inode == -1) {
                        SUMA_LH("No good direction, get out");
                        SUMA_BEEP;
                        break;
                     } else {
                        SUMA_LHv("Next node should be %d\n", inode);
                     }                           
                  }  
                  /* Now set the new selected node */
                  if (inode >= 0 && inode != SO->SelectedNode) {
                     char stmp[64]; /* use Jump callback, 
                                       the easy route */
                     sprintf(stmp,"%d", inode); 
                     SUMA_JumpIndex (stmp, (void *)sv);
                  }
               }
            }else {
               /*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
               trackball_Phi(sv->GVS[sv->StdView].deltaQuat, 
                  0.0, ArrowDeltaRot, /* first point */
                  0.0, -ArrowDeltaRot, /* ending x,y */
                  sv->ArrowRotationAngle);
               /*fprintf(stdout,"\ncurrentQuat\n");
                 for (i=0; i<4; ++i) { 
                  fprintf(stdout,"%f\t", sv->GVS[sv->StdView].currentQuat[i]);}
                 fprintf(stdout,"\n");
                 fprintf(stdout,"\ndeltaQuat\n");for (i=0; i<4; ++i) { 
                  fprintf(stdout,"%f\t", sv->GVS[sv->StdView].deltaQuat[i]);} 
                 fprintf(stdout,"\n"); */
               add_quats (sv->GVS[sv->StdView].deltaQuat, 
                          sv->GVS[sv->StdView].currentQuat, 
                          sv->GVS[sv->StdView].currentQuat);
               /*fprintf(stdout,"\nnewQuat\n");
                 for (i=0; i<4; ++i) { 
                  fprintf(stdout,"%f\t", sv->GVS[sv->StdView].currentQuat[i]);} 
                 fprintf(stdout,"\n");*/
               sv->GVS[sv->StdView].spinDeltaX = 0;
               sv->GVS[sv->StdView].spinDeltaY = 
                           -2.0*ArrowDeltaRot*sv->X->aHEIGHT;
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
   char tk[]={"Left"}, keyname[100], skey[65], skeyi[65];
   int k, nc, ii, jj, inode = -1, bkey = 0;
   float ArrowDeltaRot = 0.05; 
      /* The larger the value, the bigger the rotation increment */
   Widget w;
   double dd[3] = {-1.0, 0.0, 0.0}; /* left */
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   SUMA_KEY_SWITCH;
   
   w = sv->X->GLXAREA;
   /* do the work */
   switch (k) {
      case XK_Left:
            if ((SUMA_CTRL_KEY(key) && SUMA_SHIFT_KEY(key))) {
               float a[3], cQ[4];
               /* rotate about Z axis CCW  */
               a[0] = 0.0; a[1] = 0.0; a[2] = 1.0;
               axis_to_quat(a, -sv->ArrowRotationAngle, cQ);
               /*add rotation */
               add_quats ( cQ, 
                           sv->GVS[sv->StdView].currentQuat, 
                           sv->GVS[sv->StdView].currentQuat);
               sv->GVS[sv->StdView].spinDeltaX = 0;
               sv->GVS[sv->StdView].spinDeltaY = 0;
               SUMA_postRedisplay(w, NULL, NULL); 
            }else if (SUMA_SHIFT_KEY(key)) {
               /*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
               sv->GVS[sv->StdView].translateVec[0] -=
                  (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaX       /     
                  (float)sv->X->aWIDTH*sv->GVS[sv->StdView].TranslateGain;
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
               SUMA_LH("alt down");
               if ((SO = SUMA_SV_Focus_SO(sv))) {
                  if (SO->SelectedNode < 0 ||
                      !SO->FN) SUMA_RETURN(1); /* nothing to do */
                  inode = SO->SelectedNode;
                  {
                     inode = 
                        SUMA_NodeNeighborAlongScreenDirection(sv, SO, inode, dd);
                     if (inode == -2) {
                        SUMA_S_Err("Failed in"
                                   " SUMA_NodeNeighborAlongScreenDirection");
                        SUMA_RETURN(0);
                     } else if (inode == -1) {
                        SUMA_LH("No good direction, get out");
                        SUMA_BEEP;
                        break;
                     } else {
                        SUMA_LHv("Next node should be %d\n", inode);
                     }                           
                  }  
                  /* Now set the new selected node */
                  if (inode >= 0 && inode != SO->SelectedNode) {
                     char stmp[64]; /* use Jump callback, 
                                       the easy route */
                     sprintf(stmp,"%d", inode); 
                     SUMA_JumpIndex (stmp, (void *)sv);
                  }
               }
            }else {
               /*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
               trackball_Phi(sv->GVS[sv->StdView].deltaQuat, 
                  ArrowDeltaRot, 0.0, /* first point */
                  -ArrowDeltaRot, 0.0, /* ending x,y */
                  sv->ArrowRotationAngle);
               add_quats ( sv->GVS[sv->StdView].deltaQuat, 
                           sv->GVS[sv->StdView].currentQuat, 
                           sv->GVS[sv->StdView].currentQuat);
               sv->GVS[sv->StdView].spinDeltaX = 
                              -2.0*ArrowDeltaRot*sv->X->aWIDTH;
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
   char tk[]={"Right"}, keyname[100], skey[65], skeyi[65];
   int k, nc, ii, inode=-1;
   float ArrowDeltaRot = 0.05; 
         /* The larger the value, the bigger the rotation increment */
   Widget w;
   double dd[3] = {1.0, 0.0, 0.0}; /* right */
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_KEY_COMMON;
   
   SUMA_KEY_SWITCH;
         
   w = sv->X->GLXAREA;
   /* do the work */
   switch (k) {
      case XK_Right:
            if ((SUMA_CTRL_KEY(key) && SUMA_SHIFT_KEY(key))) {
               float a[3], cQ[4];
               /* rotate about Z axis CCW  */
               a[0] = 0.0; a[1] = 0.0; a[2] = 1.0;
               axis_to_quat(a, sv->ArrowRotationAngle, cQ);
               /*add rotation */
               add_quats ( cQ, 
                           sv->GVS[sv->StdView].currentQuat, 
                           sv->GVS[sv->StdView].currentQuat);
               sv->GVS[sv->StdView].spinDeltaX = 0;
               sv->GVS[sv->StdView].spinDeltaY = 0;
               SUMA_postRedisplay(w, NULL, NULL); 
            }else if (SUMA_SHIFT_KEY(key)) {
               /*fprintf (SUMA_STDERR,"%s: Shift down\n", FuncName);*/
               sv->GVS[sv->StdView].translateVec[0] += 
                  (GLfloat)sv->GVS[sv->StdView].ArrowtranslateDeltaX /
                  (float)sv->X->aWIDTH*sv->GVS[sv->StdView].TranslateGain;
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
               SUMA_LH("alt down");
               if ((SO = SUMA_SV_Focus_SO(sv))) {
                  if (SO->SelectedNode < 0 ||
                      !SO->FN) SUMA_RETURN(1); /* nothing to do */
                  inode = SO->SelectedNode;
                  {
                     inode = 
                        SUMA_NodeNeighborAlongScreenDirection(sv, SO, inode, dd);
                     if (inode == -2) {
                        SUMA_S_Err("Failed in"
                                   " SUMA_NodeNeighborAlongScreenDirection");
                        SUMA_RETURN(0);
                     } else if (inode == -1) {
                        SUMA_LH("No good direction, get out");
                        SUMA_BEEP;
                        break;
                     } else {
                        SUMA_LHv("Next node should be %d\n", inode);
                     }                           
                  }  
                  /* Now set the new selected node */
                  if (inode >= 0 && inode != SO->SelectedNode) {
                     char stmp[64]; /* use Jump callback, 
                                       the easy route */
                     sprintf(stmp,"%d", inode); 
                     SUMA_JumpIndex (stmp, (void *)sv);
                  }
               }
            }else {
               /*fprintf (SUMA_STDERR,"%s: Vanilla kind.\n", FuncName);*/
               trackball_Phi(sv->GVS[sv->StdView].deltaQuat, 
                  -ArrowDeltaRot, 0.0, /* first point */
                  ArrowDeltaRot, 0.0, /* ending x,y */
                  sv->ArrowRotationAngle);
               add_quats (sv->GVS[sv->StdView].deltaQuat, 
                          sv->GVS[sv->StdView].currentQuat, 
                          sv->GVS[sv->StdView].currentQuat);
               sv->GVS[sv->StdView].spinDeltaX = 2.0*ArrowDeltaRot*sv->X->aWIDTH;
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

#define SUMA_ALTHELL ( (Kev.state & Mod1Mask) || \
                       (Kev.state & Mod2Mask) ||  \
                       (Kev.state & SUMA_APPLE_AltOptMask) )

char *SUMA_Butts2String(SUMA_EVENT *ev)
{
   static char ccs[10][32];
   static int icall=0;
   char *cc;
   int nb=0, mot;
   
   ++icall; if (icall>9) icall=0;
   cc = (char *)ccs[icall]; cc[0]='\0';
   
   if (ev->b1) {cc[nb++]='1'; cc[nb++]='&'; mot = 0;}
   if (ev->b2) {cc[nb++]='2'; cc[nb++]='&'; mot = 0;}
   if (ev->b3) {cc[nb++]='3'; cc[nb++]='&'; mot = 0;}
   if (ev->b4) {cc[nb++]='4'; cc[nb++]='&'; mot = 0;}
   if (ev->b5) {cc[nb++]='5'; cc[nb++]='&'; mot = 0;}
   if (ev->b6) {cc[nb++]='6'; cc[nb++]='&'; mot = 0;}
   if (ev->b7) {cc[nb++]='6'; cc[nb++]='&'; mot = 0;}
   if (ev->b1m) {cc[nb++]='1'; cc[nb++]='&'; mot = 1;}
   if (ev->b2m) {cc[nb++]='2'; cc[nb++]='&'; mot = 1;}
   if (ev->b3m) {cc[nb++]='3'; cc[nb++]='&'; mot = 1;}
   if (ev->b4m) {cc[nb++]='4'; cc[nb++]='&'; mot = 1;}
   if (ev->b5m) {cc[nb++]='5'; cc[nb++]='&'; mot = 1;}
   
   if (nb>1) nb = nb-1; /* Get rid of last & */
   cc[nb] = '\0';
   return(cc);
}

char *SUMA_KeyType2String(int kt) 
{
   switch(kt) {
      case KeyPress:
         return("key");
      case ButtonRelease:
         return("release");
      case ButtonPress:
         return("press");
      case MotionNotify:
         return("motion");
      default:
         return("UNKNOWN");
   }
}

void SUMA_ShowEvent(SUMA_EVENT *ev, int opt, char *pre) 
{
   static char FuncName[]={"SUMA_ShowEvent"};
   static int icall=0;
   char *s = NULL;
   SUMA_STRING *SS = NULL;
   FILE *out = stderr;
   
   SUMA_ENTRY;

   SS = SUMA_StringAppend(NULL, NULL);

   if (pre) SUMA_StringAppend(SS,pre);
   if (!ev) {
      SUMA_StringAppend(SS,"NULL ev\n"); goto OUT;
   }
   ++icall;
   if (!opt) {
      SUMA_StringAppend_va(SS,"Event Struct (set %d, callid %d)\n"
                        "   ktype %d kstate %d transl %s\n"
                        "   keysym %d mtype %d mstate %d\n"
                        "   bButton %d mButton %d\n"
                        "   bTime %ld  mTime %ld\n"
                        "   mX %d mY %d bX %d bY %d\n"
                        "   mDelta %d, mDeltaX %d, mDeltaY %d\n"
               "   shift %d control %d mod1 %d mod2 %d mod3 %d mod4 %d mod5 %d\n"
                        "   ApplAltOpt %d DoubleClick %d\n"
                        "   b1 %d b2 %d b3 %d b4 %d b5 %d b6 %d b7 %d\n"
                        "   b1m %d b2m %d b3m %d b4m %d b5m %d\n",
                        ev->set, icall,
                        ev->ktype, ev->kstate, ev->transl,
                        ev->keysym, ev->mtype, ev->mstate,
                        ev->bButton, ev->mButton,
                        ev->bTime, ev->mTime,
                        ev->mX, ev->mY, ev->bX, ev->bY,
                        ev->mDelta, ev->mDeltaX, ev->mDeltaY,
      ev->Shift, ev->Control, ev->Mod1, ev->Mod2, ev->Mod3, ev->Mod4, ev->Mod5,
                        ev->AppleAltOpt, ev->DoubleClick,
                        ev->b1, ev->b2, ev->b3, ev->b4, ev->b5, ev->b6, ev->b7,
                        ev->b1m, ev->b2m, ev->b3m, ev->b4m, ev->b5m);
   } else {
      /* More readable mode */
      SUMA_StringAppend_va(SS,"Input Event %d: %s   \n", 
            icall, ev->set ? "":"WARNING Event Struct Not Set!" );
      if (ev->ktype == KeyPress) {
         SUMA_StringAppend_va(SS,"%s: char>>%s<< sym>>%d<< ", 
                           SUMA_KeyType2String(ev->ktype),
                           ev->transl, (int)ev->keysym);
      } else {
         SUMA_StringAppend_va(SS,"Mouse %s %s%s: ", 
            SUMA_Butts2String(ev),
            SUMA_KeyType2String(ev->ktype), 
            ev->DoubleClick ? " double click":"");
         if (ev->b1) SUMA_StringAppend_va(SS,"b1 ",ev->b1);
         if (ev->b2) SUMA_StringAppend_va(SS,"b2 ",ev->b2);
         if (ev->b3) SUMA_StringAppend_va(SS,"b3 ",ev->b3);
         if (ev->b4) SUMA_StringAppend_va(SS,"b4 ",ev->b4);
         if (ev->b5) SUMA_StringAppend_va(SS,"b5 ",ev->b5);
         if (ev->b6) SUMA_StringAppend_va(SS,"b6 ",ev->b6);
         if (ev->b7) SUMA_StringAppend_va(SS,"b7 ",ev->b7);
         if (ev->b1m) SUMA_StringAppend_va(SS,"m1 ",ev->b1m);
         if (ev->b2m) SUMA_StringAppend_va(SS,"m2 ",ev->b2m);
         if (ev->b3m) SUMA_StringAppend_va(SS,"m3 ",ev->b3m);
         if (ev->b4m) SUMA_StringAppend_va(SS,"m4 ",ev->b4m);
         if (ev->b5m) SUMA_StringAppend_va(SS,"m5 ",ev->b5m);
      }
      if (ev->Shift) {
         SUMA_StringAppend_va(SS,"Shift ");
      }
      if (ev->Control){
         SUMA_StringAppend_va(SS,"Control ");
      }
      if (ev->Mod1){
         SUMA_StringAppend_va(SS,"alt ");
      }
      if (ev->Mod2){
         SUMA_StringAppend_va(SS,"Mod2 (command on mac) ");
      }
      if (ev->Mod3){
         SUMA_StringAppend_va(SS,"Mod3 ");
      }
      if (ev->Mod4){
         SUMA_StringAppend_va(SS,"Mod4 ");
      }
      if (ev->Mod5){
         SUMA_StringAppend_va(SS,"Mod5 ");
      }
      if (ev->Mod2){
         SUMA_StringAppend_va(SS,"Mod2 ");
      }
      if (ev->AppleAltOpt){
        SUMA_StringAppend_va(SS,"Apple Alt/Opt ");
      }
      SUMA_StringAppend_va(SS,"k/mstate [%d/%d]\n\n", ev->kstate, ev->mstate);
   }
   OUT:
   SUMA_SS2S(SS,s);
   
   fprintf(out,"%s",s);
   
   SUMA_ifree(s);
   
   SUMA_RETURNe;
}

#ifdef DARWIN
#define evALT ((ev->Mod1 || ev->Mod2 || ev->AppleAltOpt))
#else
#define evALT ((ev->Mod1))
#endif
int SUMA_ShftCont_Event(SUMA_EVENT *ev) 
{
   if (!ev) ev = SUMAg_CF->lev;
   if (!ev || !ev->set) return(0);
   if (ev->Shift && ev->Control && !evALT) return(1);
   return(0);
}
int SUMA_Cont_Event(SUMA_EVENT *ev) 
{
   if (!ev) ev = SUMAg_CF->lev;
   if (!ev || !ev->set) return(0);
   if (!ev->Shift && ev->Control && !evALT) return(1);
   return(0);
}
int SUMA_Shft_Event(SUMA_EVENT *ev) 
{
   if (!ev) ev = SUMAg_CF->lev;
   if (!ev || !ev->set) return(0);
   if (ev->Shift && !ev->Control && !evALT) return(1);
   return(0);
}
int SUMA_ShftContAlt_Event(SUMA_EVENT *ev) 
{
   if (!ev) ev = SUMAg_CF->lev;
   if (!ev || !ev->set) return(0);
   if (ev->Shift && ev->Control && evALT ) return(1);
   return(0);
}
int SUMA_ContAlt_Event(SUMA_EVENT *ev) 
{
   if (!ev) ev = SUMAg_CF->lev;
   if (!ev || !ev->set) return(0);
   if (!ev->Shift && ev->Control && evALT ) return(1);
   return(0);
}
int SUMA_ShftAlt_Event(SUMA_EVENT *ev) 
{
   if (!ev) ev = SUMAg_CF->lev;
   if (!ev || !ev->set) return(0);
   if (ev->Shift && !ev->Control && evALT ) return(1);
   return(0);
}
int SUMA_Alt_Event(SUMA_EVENT *ev) 
{
   if (!ev) ev = SUMAg_CF->lev;
   if (!ev || !ev->set) return(0);
   if (!ev->Shift && !ev->Control && evALT) return(1);
   return(0);
}
int SUMA_Plain_Event(SUMA_EVENT *ev) 
{
   if (!ev) ev = SUMAg_CF->lev;
   if (!ev || !ev->set) return(0);
   if (!ev->Shift && !ev->Control && !evALT) return(1);
   return(0);
}

/*! Get a record of events.
    Contents based on logic used in SUMA_input()
    Ideally, all should be done here and SUMA_input()
    should use the results of this call. But we're not
    there yet....
    Once I start using this one call, then the logic
    can be revisited at varied points in there.
*/                       
SUMA_EVENT *SUMA_RecordEvent( XEvent *event, 
                              SUMA_EVENT *ev)
{
   static char FuncName[]={"SUMA_RecordEvent"};
   XKeyEvent Kev;
   XButtonEvent Bev;
   XMotionEvent Mev;
   static SUMA_EVENT lev;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!event) {
      SUMA_S_Err("Null event");
      if (ev) memset(ev, 0, sizeof(SUMA_EVENT));
      SUMA_RETURN(ev);
   }
   
   if (!ev) {
      if (!(ev = SUMAg_CF->lev)) {
         memset(&lev, 0, sizeof(SUMA_EVENT));
         SUMAg_CF->lev = (SUMA_EVENT *)SUMA_malloc(1*sizeof(SUMA_EVENT));   
      }
      ev = SUMAg_CF->lev;
   }
   if (!ev) SUMA_RETURN(NULL);
   memset(ev, 0, sizeof(SUMA_EVENT));
   
   Kev = *(XKeyEvent *) &event->xkey; /* RickR's suggestion to comply with 
                                 ANSI C, no type casting of structures  July 04*/
   Bev = *(XButtonEvent *) &event->xbutton;
   Mev = *(XMotionEvent *) &event->xmotion;

   ev->set = 1;
   /* The copied parameters */
   ev->ktype = Kev.type;
   ev->kstate = Kev.state;
   ev->mtype = Mev.type;
   ev->mstate = Mev.state;
   ev->bTime = Bev.time;
   ev->mTime = Mev.time;
   ev->bButton = Bev.button;
   ev->mButton = 0; /* Not sure this one was all that necessary */
   ev->mX = Mev.x;
   ev->mY = Mev.y;
   ev->bX = Bev.x;
   ev->bY = Bev.y;
   
   /* The inferred parameters */
   if ((Kev.state & ShiftMask) || (Bev.state & ShiftMask)) ev->Shift = 1;
   if ((Kev.state & ControlMask) || (Bev.state & ControlMask)) ev->Control = 1;
   if (Kev.state & Mod1Mask) ev->Mod1 = 1;
   if (Kev.state & Mod2Mask) ev->Mod2 = 1;
   if (Kev.state & Mod3Mask) ev->Mod3 = 1;
   if (Kev.state & Mod4Mask) ev->Mod4 = 1;
   if (Kev.state & Mod5Mask) ev->Mod5 = 1;
   if (Kev.state & SUMA_APPLE_AltOptMask) ev->AppleAltOpt = 1;
   
   switch(ev->ktype) {
      case KeyPress:
         ev->transl[0] = ev->transl[15] = '\0';
         XLookupString( (XKeyEvent *)event, ev->transl, 14, 
                              &ev->keysym, NULL);
         break;
         break;
      case ButtonPress:
         if (Bev.state & Button1Mask) ev->b1 = 1;
         if (Bev.state & Button2Mask) ev->b2 = 1;
         if (Bev.state & Button3Mask) ev->b3 = 1;
         if (Bev.state & Button4Mask) ev->b4 = 1;
         if (Bev.state & Button5Mask) ev->b5 = 1;
         if (ev->bButton == 6) ev->b6 = 1; /* on macs Button6 not in X.h */
         if (ev->bButton == 7) ev->b7 = 1; /* on macs Button7 not in X.h */
         
         switch (ev->bButton) {
            case Button1:
            case Button2:
            case Button3:
            case Button4:
            case Button5:
            case 6: 
            case 7:
            default:
               SUMA_LH("ButtonPress %d not known", ev->bButton);
               break;
         }
         
         /* trap for double click */
         if (Bev.time - lev.bTime < SUMA_DOUBLE_CLICK_MAX_DELAY) {
            ev->DoubleClick = YUP;
         } else {
            ev->DoubleClick = NOPE;
         }
         
         if (  SUMAg_CF->SwapButtons_1_3 || 
               (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
            int kk=ev->b1;
            ev->b1 = ev->b3;
            ev->b3 = kk;
         }

         break;
      case ButtonRelease:
         ev->mTime = 0;
         if (Bev.state & Button1Mask) ev->b1 = 1;
         if (Bev.state & Button2Mask) ev->b2 = 1;
         if (Bev.state & Button3Mask) ev->b3 = 1;
         if (Bev.state & Button4Mask) ev->b4 = 1;
         if (Bev.state & Button5Mask) ev->b5 = 1;
         if (ev->bButton == 6) ev->b6 = 1; /* on macs Button6 not in X.h */
         if (ev->bButton == 7) ev->b7 = 1; /* on macs Button7 not in X.h */
         
         if (SUMAg_CF->SwapButtons_1_3 || 
             (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
            int kk=ev->b1;
            ev->b1 = ev->b3;
            ev->b3 = kk;
         }
         break;
         
      case MotionNotify:
         if (Mev.state & Button1MotionMask) ev->b1m = 1;
         if (Mev.state & Button2MotionMask) ev->b2m = 1;
         if (Mev.state & Button3MotionMask) ev->b3m = 1;
         if (Mev.state & Button4MotionMask) ev->b4m = 1;
         if (Mev.state & Button5MotionMask) ev->b5m = 1;
         if (Mev.state) { SUMA_LH("   Something mot\n"); }
         
         /* The conditions and new assignments of mButton
         below is stupid. But I won't touch it until I 
         have to. Reassignments should be to b1m, b2m, etc.
         mButton should not be touched. 
         Also, there should be no need for these numerous
         conditions. If swapping is needed, b1m and b3m 
         values should be swaped. Things like SUMA_Button_12_Motion
         should be made into functions that return an answer
         based on ev's contents */
         if (  SUMAg_CF->SwapButtons_1_3 || 
               (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
           if (((Mev.state & Button3MotionMask) && 
                                             (Mev.state & Button2MotionMask)) 
            || ((Mev.state & Button2MotionMask) && (Mev.state & ShiftMask))) {
               int kk=ev->b1m;
               ev->b1m = ev->b3m;
               ev->b3m = kk;
               ev->mButton = SUMA_Button_12_Motion;
            } else if(Mev.state & Button3MotionMask) {
               ev->mButton = SUMA_Button_1_Motion;
               int kk=ev->b1m;
               ev->b1m = ev->b3m;
               ev->b3m = kk;
            }else if(Mev.state & Button2MotionMask) { 
               ev->mButton = SUMA_Button_2_Motion;
            }else if(Mev.state & Button1MotionMask) {
                
               ev->mButton = SUMA_Button_3_Motion;
            }else {
               break;
            } 
         } else {
            if (((Mev.state & Button1MotionMask) && 
                                                (Mev.state & Button2MotionMask))
             || ((Mev.state & Button2MotionMask) && (Mev.state & ShiftMask))) {
               ev->mButton = SUMA_Button_12_Motion;
            } else if(Mev.state & Button1MotionMask) {
               ev->mButton = SUMA_Button_1_Motion;
            }else if(Mev.state & Button2MotionMask) { 
               ev->mButton = SUMA_Button_2_Motion;
            } else if(Mev.state & Button3MotionMask) { 
               ev->mButton = SUMA_Button_3_Motion;
            }else {
               break;
            }
         }
         switch (ev->mButton) {
            case SUMA_Button_12_Motion:
            case SUMA_Button_2_Shift_Motion:
               if (ev->mTime) {
                  ev->mDelta = Mev.time - lev.mTime;
                  ev->mDeltaX = Mev.x - lev.mX;
                  ev->mDeltaY = Mev.y - lev.mY;
               } else {
                  ev->mDelta  = 0;
                  ev->mDeltaX = 0;
                  ev->mDeltaY = 0;
               }
               break;
            case SUMA_Button_2_Motion:
               break;
            case SUMA_Button_3_Motion:
               break;
      }
               
   }
   if (LocalHead) {
      SUMA_ShowEvent(ev, 0, "EventRecord:\n");
      SUMA_ShowEvent(ev, 1, "EventRecord:\n");
   } else if (SUMAg_CF->Echo_KeyPress) {
      SUMA_ShowEvent(ev, 1, "EventRecord:\n");
   }
   
   /* keep local copy of last event */
   memcpy(&lev, ev, sizeof(SUMA_EVENT));
   
   SUMA_RETURN(ev);   
}

/*! Mouse and Keyboard input handler function for SUMA's viewer 
    START shifting to SUMA_RecordEvent() and its helper functions
    like SUMA_Alt_Event(), etc. 
    You should also split SUMA_input() into SUMA_input_Xevent()
    and SUMA_input_eng(). Where SUMA_input_Xevent() just sets
    SUMAg_CF->lev and SUMA_input_eng() works entirely off of
    SUMAg_CF->lev . This way you would be able to completely
    drive SUMA_input_eng() without any mouse movement/X structs
    etc. All you need is to manipulate the content of lev.
*/
void SUMA_input(Widget w, XtPointer clientData, XtPointer callData)
{
   static char FuncName[]= {"SUMA_input"};
   GLwDrawingAreaCallbackStruct *cd;
   char buffer[10], cbuf = '\0', cbuf2='\0';
   KeySym keysym;
   int xls=-1, ntot, id = 0, ND, ip, NP;
   SUMA_EngineData *ED = NULL; 
   char CommString[SUMA_MAX_COMMAND_LENGTH];
   char s[SUMA_MAX_STRING_LENGTH], sfield[100], sdestination[100];
   static char ssource[]={"suma"};
   int it, ii, iv3[3], hit = 0, SwasHit = 0;
   float **fm, fv3[3], fv15[15];
   XKeyEvent Kev;
   XButtonEvent Bev;
   XMotionEvent Mev;
   int isv;
   SUMA_SurfaceViewer *sv, *svi = NULL;
   GLfloat *glar_ColorList = NULL;
   static Time B1time = 0, M1time=0, M1delta=0;
   static int pButton, mButton, rButton;
   SUMA_Boolean ROI_mode; 
   static SUMA_Boolean DoubleClick = NOPE;
   DList *list = NULL;
   DListElmt *NextElm= NULL;
   float bevx, bevy, mevx, mevy, wwid, whei, zc_fac, mvx_fac, mvy_fac;
   static int mvxlast, mvylast, mvdeltax, mvdeltay;
   SUMA_PROMPT_DIALOG_STRUCT *prmpt=NULL; /* Use this only to create prompt 
                                             that are not to be preserved */
   SUMA_Boolean LocalHead = NOPE; /* local debugging messages */

   SUMA_ENTRY;
   
   /* get the callData pointer */
   cd = (GLwDrawingAreaCallbackStruct *) callData;
   
   /* find out who's calling, only GLXAREA calls this function */
   SUMA_GLXAREA_WIDGET2SV(w, sv, isv);
   if (isv < 0) {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in macro SUMA_GLXAREA_WIDGET2SV.\n", FuncName);
      SUMA_RETURNe;
   }
   SUMA_LH("A call from SUMA_SurfaceViewer[%d], Pointer %p\n", isv, sv);
   
   /* ******** ABOUT EVENT HANDLING ************** */
   /* Eventually you should use the structure
      created by RecordEvent to decide on what was clicked
      and how. While a little less efficient than 
      what is done below, RecordEvent will eventually
      be considerably more flexible, and easier to debug.
      But for now, so that visible progress can be made,
      I will leave button processing and queries below
      as is, and try to make the switch gradually */
   
   SUMAg_CF->lev = SUMA_RecordEvent( cd->event, SUMAg_CF->lev);
                              
   Kev = *(XKeyEvent *) &cd->event->xkey; /* RickR's suggestion to comply with 
                                 ANSI C, no type casting of structures  July 04*/
   Bev = *(XButtonEvent *) &cd->event->xbutton;
   Mev = *(XMotionEvent *) &cd->event->xmotion;
   
   /* a sample keypresses */
   if (SUMAg_CF->Echo_KeyPress) {
      if (Kev.type == KeyPress) {
         buffer[0] = '\0';
         xls = XLookupString((XKeyEvent *) cd->event, buffer, 8, &keysym, NULL);
         fprintf (SUMA_STDERR,"%s KeyPress char>>%s<< sym>>%d<< ", 
                  FuncName, buffer, (int)keysym);
      } else {
         fprintf (SUMA_STDERR,"%s Mouse Action: ", FuncName);
      }
      if (Kev.state & ShiftMask) {
         fprintf (SUMA_STDERR,"Shift ");
      }
      if (Kev.state & ControlMask){
         fprintf (SUMA_STDERR,"Control ");
      }
      if (Kev.state & Mod1Mask){
         fprintf (SUMA_STDERR,"alt ");
      }
      if (Kev.state & Mod2Mask){
         fprintf (SUMA_STDERR,"Mod2 (command on mac) ");
      }
      if (Kev.state & Mod3Mask){
         fprintf (SUMA_STDERR,"Mod3 ");
      }
      if (Kev.state & Mod4Mask){
         fprintf (SUMA_STDERR,"Mod4 ");
      }
      if (Kev.state & Mod5Mask){
         fprintf (SUMA_STDERR,"Mod5 ");
      }
      if (Kev.state & SUMA_APPLE_AltOptMask){
         fprintf (SUMA_STDERR,"Apple Alt/Opt ");
      }
      fprintf (SUMA_STDERR,"State %d\n\n", Kev.state);
  }
   
  switch (Kev.type) { /* switch event type */
  case KeyPress:
      if (xls < 0) { 
         /* avoid double call in case SUMAg_CF->Echo_KeyPress called already */
         xls = XLookupString((XKeyEvent *) cd->event, buffer, 8, &keysym, NULL);
      }
      /* XK_* are found in keysymdef.h */ 
      switch (keysym) { /* keysym */
         case XK_bracketleft: /* The left bracket */
            if (!SUMA_bracketleft_Key(sv, "[", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
         
         case XK_bracketright: /* The right bracket */
            if (!SUMA_bracketright_Key(sv, "]", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
                    
         case XK_space:   /* The spacebar. */
            if (!SUMA_space_Key(sv, "SPACE", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
            /* toggle between state containing mapping reference 
               of SO in focus and other view */
            {
               SUMA_SurfaceObject *SO = NULL, *SOmap = NULL;
               int curstateID = -1, nxtstateID = -1, dov_ID = -1;

               /* make sure switching is OK */
               curstateID = SUMA_WhichState(sv->State, sv, sv->CurGroupName);
               if ((SO = SUMA_SV_Focus_SO(sv))) {
                  if (SUMA_isLocalDomainParent (SO)) {
                     /* get the last non mappable state in SV */
                     if (sv->LastNonMapStateID < 0) { 
                              /* not recorded, complain and quit */
                        SUMA_S_Warn("Nothing defined to toggle with yet."); 
                        break;
                     }

                     SUMA_LHv("surface is inherrently mappable, "
                              "switching to last non mappable state %d.\n", 
                              sv->LastNonMapStateID);

                     if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, sv, 
                              sv->LastNonMapStateID, sv->CurGroupName)) {
                        SUMA_S_Err("Failed in SUMA_SwitchState.");
                        break;
                     }

                  } else {/* non mappable, go to state containing reference */
                     SUMA_LH("surface is not inherrently mappable, "
                             "searching for mapping reference and its state.");

                     /* find SO that is mappable reference & 
                        get corresponding state ID */
                     dov_ID = SUMA_findSO_inDOv(SO->LocalDomainParentID, 
                                                SUMAg_DOv, SUMAg_N_DOv);
                     SOmap = (SUMA_SurfaceObject *)SUMAg_DOv[dov_ID].OP;
                     nxtstateID = SUMA_WhichState(SOmap->State, sv,
                                                  sv->CurGroupName);

                     if (nxtstateID < 0) {
                        SUMA_S_Err("Failed in SUMA_findSO_inDOv."
                                   "This should not happen.");
                        break;
                     }

                     SUMA_LHv("Found mapping reference in viewer state %d.\n",
                              nxtstateID);

                     /* store this location */
                     sv->LastNonMapStateID = curstateID;

                     /* go there */
                     if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, sv, 
                                            nxtstateID, sv->CurGroupName)) {
                        SUMA_S_Err("Failed in SUMA_SwitchState.");
                        break;
                     }
                  }
               }
            }
            SUMA_postRedisplay(w, clientData, callData);
            break;

         case XK_Escape: /* there's more:  
                  XK_BackSpace XK_Tab XK_Linefeed XK_Return XK_Delete */
            /* control mask and escape is grabbed by gnome window manager .... */
            if (Kev.state & ShiftMask){/* kill all */
               if( SUMAg_CF->X->WarnClose) {
                  if (SUMA_ForceUser_YesNo(sv->X->TOPLEVEL, 
                           "Close All Viewers?", SUMA_YES, 
                           SWP_DONT_CARE) != SUMA_YES) {
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
                     if (SUMA_ForceUser_YesNo(sv->X->TOPLEVEL, 
                                 "Close This Viewer?", SUMA_YES, 
                                 SWP_DONT_CARE) != SUMA_YES) {
                        break;   
                     }
                  #endif
               }
               SUMA_ButtClose_pushed (w, clientData, callData);
            }
            break;

         case XK_a:
            if (!SUMA_A_Key(sv, "a", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
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
            if (SUMAg_CF->Dev && (SUMA_ALTHELL)){
               SUMAg_CF->X->ClipObj_prmpt = 
                  SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, 
                              "Enter object clip plane parameters (a,b,c,d)", 
                              "A: 0,0,1,0",
                              sv->X->TOPLEVEL, YUP,
                              SUMA_APPLY_BUTTON,
                              SUMA_SetObjectClip, (void *)sv,
                              NULL, NULL,
                              NULL, NULL,
                              NULL, NULL,  
                              SUMAg_CF->X->ClipObj_prmpt);
               
               SUMAg_CF->X->ClipObj_prmpt = 
                  SUMA_CreatePromptDialog(
                     "Enter object clip plane parameters (a,b,c,d)", 
                     SUMAg_CF->X->ClipObj_prmpt);
            } else if (SUMAg_CF->Dev && (Kev.state & ControlMask)){
               SUMAg_CF->X->Clip_prmpt = 
                  SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, 
                     "Enter screen clip plane parameters (a,b,c,d)", 
                     "A: 0,0,1,0",
                     sv->X->TOPLEVEL, YUP,
                     SUMA_APPLY_BUTTON,
                     SUMA_SetScreenClip, (void *)sv,
                     NULL, NULL,
                     NULL, NULL,
                     NULL, NULL,  
                     SUMAg_CF->X->Clip_prmpt);

               SUMAg_CF->X->Clip_prmpt = 
                  SUMA_CreatePromptDialog(
                     "Enter screen clip plane parameters (a,b,c,d)", 
                     SUMAg_CF->X->Clip_prmpt);
            }
            break; 
         case XK_c:
            {   
               SUMA_SurfaceObject *SO=NULL;
               if ((SO = SUMA_SV_Focus_SO(sv))) {
                  if (!list) list = SUMA_CreateList();
                  ED = SUMA_InitializeEngineListData (SE_OpenColFileSelection);
                  if (!(NextElm = SUMA_RegisterEngineListCommand (  list, ED,
                                    SEF_vp, (void *)SO,
                                    SES_Suma, (void *)sv, NOPE,
                                    SEI_Head, NULL))) {
                     SUMA_S_Err("Failed to register command.");
                  }

                  if (!SUMA_RegisterEngineListCommand (  list, ED,
                                                SEF_ip, sv->X->TOPLEVEL,
                                                SES_Suma, (void *)sv, NOPE,
                                                SEI_In, NextElm)) {
                     SUMA_S_Err("Failed to register command.");
                  }  

                  if (!SUMA_Engine (&list)) {
                     fprintf(SUMA_STDERR, 
                              "Error %s: SUMA_Engine call failed.\n", FuncName);
                  }
               }
            }
            
            break;
             
            #if 0
            /* THE OLD WAY (part of it) FOR SETTING NODE COLORS DIRECTLY, 
               Left here for documentation */
            /* allocate space */
            fm = (float **)SUMA_allocate2D (ntot/4, 4, sizeof(float));
            if (fm == NULL) {
               fprintf(stderr,
                        "Error SUMA_input: Failed to allocate space for fm\n");
               SUMA_RETURNe;
            }

            if (SUMA_Read_2Dfile (s, fm, 4, ntot/4) != ntot/4 ) {
               fprintf(stderr,
                        "SUMA_input Error: Failed to read full matrix from %s\n",
                        s);
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
               fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", 
                        FuncName);
               break;                                      
            } 

            
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);

            if (!SUMA_Engine (&list)) {
               fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", 
                        FuncName);
            }

            /* free fm since it was registered by pointer and is not 
               automatically freed after the call to SUMA_Engine */
            if (fm) SUMA_free2D ((char **)fm, ntot/4);
            break;
            #endif 
            
         case XK_D:
            if (1) {
               if (SUMA_ALTHELL){ 
                  /*  Mod1Mask is alt in linux, Mod2Mask is the apple on mac*/
               } else {
                  if (!SUMA_D_Key(sv,"D", "interactive")) {
                     SUMA_S_Err("Failed in key func.");
                  }
               }
            }
            break;
            
         case XK_d:
            if (SUMAg_CF->Dev) {
               if (SUMA_ALTHELL){ 
                  /*  Mod1Mask is alt in linux, Mod2Mask is the apple on mac*/
               } else {
                  if (!SUMA_D_Key(sv,"d", "interactive")) {
                     SUMA_S_Err("Failed in key func.");
                  }
               }
            }
            break;
            
         case XK_e:
         case XK_dead_acute:  /* that is alt/option+e on macs 
                               XK_dead_acute is 0xfe51 or decimal 65105 */
            if (SUMAg_CF->Dev) {
               if (SUMA_ALTHELL){ /* Mod1Mask is alt in linux, 
                                     Mod2Mask is the apple on mac*/
                  SUMA_GL_ERRS;
               }
            }
            break;
            
         case XK_F:
            /* flip light position */
            if (!list) list = SUMA_CreateList(); 
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_FlipLight0Pos, 
                                                SES_Suma, sv);
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);

            if (!SUMA_Engine (&list)) {
               fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
            }
            break;

         case XK_f:
            /* Show/hide the foreground */
            if (!list) list = SUMA_CreateList(); 
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_ToggleForeground, 
                                                SES_Suma, sv);
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);

            if (!SUMA_Engine (&list)) {
               fprintf(stderr, "Error SUMA_input: SUMA_Engine call failed.\n");
            }
            break;            

         case XK_H:
               sv->X->HighlightBox_prmpt = 
                  SUMA_CreatePromptDialogStruct(SUMA_OK_APPLY_CLEAR_CANCEL, 
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
               
               sv->X->HighlightBox_prmpt = SUMA_CreatePromptDialog(sv->X->Title, 
                                                      sv->X->HighlightBox_prmpt);
               
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
                  fprintf( stderr, 
                           "Error %s: SUMA_Engine call failed.\n", FuncName);
              }    
            }else{
               if (SUMAg_CF->Dev) {
                  SUMA_SLP_Note("Please use ctrl+h for help.\n"
                                "h alone will be reassigned\n"
                                "in future versions.");
               }
            }
            break;
         
         case XK_j:
               if (Kev.state & ControlMask){
                  if (!SUMA_J_Key(sv, "ctrl+j", "interactive", NULL)) {
                     SUMA_S_Err("Failed in key func.");
                  }
               } else if (SUMA_ALTHELL){
                  if (!SUMA_J_Key(sv, "alt+j", "interactive", NULL)) {
                     SUMA_S_Err("Failed in key func.");
                  }
               } else {
                  if (!SUMA_J_Key(sv, "j", "interactive", NULL)) {
                     SUMA_S_Err("Failed in key func.");
                  }
               }   
            break;
         
         case XK_J:
               if (!SUMA_J_Key(sv, "J", "interactive", NULL)) {
                     SUMA_S_Err("Failed in key func.");
               }
            break; 
              
         case XK_l:
               if (Kev.state & ControlMask){
                  if (!SUMA_L_Key(sv, "ctrl+l", "interactive", NULL)) {
                        SUMA_S_Err("Failed in key func.");
                  }
               } else if (SUMA_ALTHELL){
                  if (!SUMA_L_Key(sv, "alt+l", "interactive", NULL)) {
                     SUMA_S_Err("Failed in key func.");
                  }
               } else {
                  if (!SUMA_L_Key(sv, "l", "interactive", NULL)) {
                        SUMA_S_Err("Failed in key func.");
                  }
               }
            break;

         case XK_L:
               if (Kev.state & ControlMask){
                  if (!SUMA_L_Key(sv, "ctrl+L", "interactive", NULL)) {
                        SUMA_S_Err("Failed in key func.");
                  }
               } else if (SUMA_ALTHELL){
                  if (!SUMA_L_Key(sv, "alt+L", "interactive", NULL)) {
                     SUMA_S_Err("Failed in key func.");
                  }
               } else {
                  if (!SUMA_L_Key(sv, "L", "interactive", NULL)) {
                        SUMA_S_Err("Failed in key func.");
                  }
               }
            break;
         
         case XK_M:
            if ((SUMA_ALTHELL) && (Kev.state & ControlMask) ){
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
                  SUMA_LH("Going to N_Key");
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
         case XK_o:
            if (SUMA_ALTHELL) {
               if (!SUMA_O_Key(sv, "alt+o", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            } else if (Kev.state & ControlMask){
               if (!SUMA_O_Key(sv, "ctrl+o", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            } else {
               if (!SUMA_O_Key(sv, "o", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            }
            break;
         case XK_O:
            if (SUMA_ALTHELL) {
               if (!SUMA_O_Key(sv, "alt+O", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            } else if (Kev.state & ControlMask){
               if (!SUMA_O_Key(sv, "ctrl+O", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            } else {
               if (!SUMA_O_Key(sv, "O", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            }
            break;
         case XK_p:
            if (Kev.state & ControlMask){
               if (!SUMA_P_Key(sv, "ctrl+p", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            } else {
               if (!SUMA_P_Key(sv, "p", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            }
            break;

         case XK_P:
            if (!SUMA_P_Key(sv, "P", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
         
         case XK_r:
            if (SUMA_ALTHELL) {
               if (!SUMA_R_Key(sv, "alt+r", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            } else if (Kev.state & ControlMask){
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
            if (Kev.state & ControlMask){
               if (!SUMA_R_Key(sv, "ctrl+R", "interactive")) {
                     SUMA_S_Err("Failed in key func.");
               }
            } else {
               if (!SUMA_R_Key(sv, "R", "interactive")) {
                     SUMA_S_Err("Failed in key func.");
               }
            }
            break;
            
         case XK_S:
            if (SUMAg_CF->Dev) {
               int *do_id, n_do_id;
               do_id = SUMA_GetDO_Type(SUMAg_DOv, SUMAg_N_DOv, 
                                       SO_type, &n_do_id);
               if (n_do_id) {
                  while (n_do_id) {
                     SUMA_Print_Surface_Object(
                        (SUMA_SurfaceObject *)SUMAg_DOv[do_id[n_do_id-1]].OP,
                        stdout);
                     --n_do_id;
                  }
                  SUMA_free(do_id);
               }
               break;
            }
            
         case XK_s:
            if ((SUMA_ALTHELL) && (Kev.state & ControlMask) ){
               if (!list) list = SUMA_CreateList();
               ED = SUMA_InitializeEngineListData (SE_LoadSegDO);
               if (!SUMA_RegisterEngineListCommand (  list, ED,
                                          SEF_ip, sv->X->TOPLEVEL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL)) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed to register command.\n", FuncName);
               }
               if (!SUMA_Engine (&list)) {
                     fprintf( SUMA_STDERR, 
                              "Error %s: SUMA_Engine call failed.\n", 
                              FuncName);
               }               
   
            } else if (SUMA_ALTHELL){
               /* swap buttons 1 and 3 */
               SUMAg_CF->SwapButtons_1_3 = !SUMAg_CF->SwapButtons_1_3;
               if (SUMAg_CF->SwapButtons_1_3) {
                  fprintf (SUMA_STDOUT,
                           "%s: Buttons 1 and 3 are swapped.\n", FuncName);
               } else {
                  fprintf (SUMA_STDOUT,
                           "%s: Default functions for buttons 1 and 3.\n", 
                           FuncName);
               }               
            } else if (SUMAg_CF->Dev) {
               #if 0
               /** Feb 03/03 No longer in use.*/
               for (ii=0; ii< sv->N_DO; ++ii) {
                  if (SUMA_isSO(SUMAg_DOv[sv->RegisteredDO[ii]]))
                     SUMA_Print_Surface_Object(
                        (SUMA_SurfaceObject*)SUMAg_DOv[sv->RegisteredDO[ii]].OP,
                        stdout);
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
         
         case XK_u:
            if (!SUMA_U_Key(sv, "u", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
         case XK_U:
            if (!SUMA_U_Key(sv, "U", "interactive")) {
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
            if ((Kev.state & ControlMask)){
               if (!SUMA_W_Key(sv, "ctrl+W", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            } else {
               if (!SUMA_W_Key(sv, "W", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            } 
            break;
            
         case XK_w:
            if (!SUMA_W_Key(sv, "w", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            } 
            break;
             
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

         #if 0
         case XK_3:
            sv->Do_3Drender =  !sv->Do_3Drender;
            SUMA_S_Notev("!!!!!!!!!!!!!!%d!!!!!!!\n", sv->Do_3Drender);
            break;
         #endif
         case XK_8:
            {
               char stmp[100];
               sprintf(stmp, "%d", SUMAg_CF->X->NumForeSmoothing);
               SUMAg_CF->X->N_ForeSmooth_prmpt = 
                  SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, 
                                       "Foreground smoothing iterations", 
                                       stmp,
                                       sv->X->TOPLEVEL, YUP,
                                       SUMA_APPLY_BUTTON,
                                       SUMA_SetNumForeSmoothing, (void *)sv,
                                       NULL, NULL,
                                       NULL, NULL,
                                       SUMA_CleanNumString, (void*)1,
                                       SUMAg_CF->X->N_ForeSmooth_prmpt);

               SUMAg_CF->X->N_ForeSmooth_prmpt = 
                     SUMA_CreatePromptDialog("Foreground smoothing iterations", 
                                             SUMAg_CF->X->N_ForeSmooth_prmpt);
            }
            break;
            
         case XK_asterisk:
            {
               char stmp[100];
               sprintf(stmp, "%d", SUMAg_CF->X->NumFinalSmoothing);
               SUMAg_CF->X->N_FinalSmooth_prmpt = 
                  SUMA_CreatePromptDialogStruct (SUMA_OK_APPLY_CLEAR_CANCEL, 
                                    "Final color smoothing iterations", 
                                    stmp,
                                    sv->X->TOPLEVEL, YUP,
                                    SUMA_APPLY_BUTTON,
                                    SUMA_SetNumFinalSmoothing, (void *)sv,
                                    NULL, NULL,
                                    NULL, NULL,
                                    SUMA_CleanNumString, (void*)1,                                                    SUMAg_CF->X->N_FinalSmooth_prmpt);

               SUMAg_CF->X->N_FinalSmooth_prmpt = 
                     SUMA_CreatePromptDialog("Final color smoothing iterations", 
                                             SUMAg_CF->X->N_FinalSmooth_prmpt);
            }
            break;

          case XK_at:
            if (SUMAg_CF->Dev) {
               SUMA_SurfaceObject *SO=NULL;
               /* calculate the curvature */
               fprintf(SUMA_STDOUT, 
                  "%s: Calculating surface curvature ...\n", FuncName);
               if ((SO = SUMA_SV_Focus_SO(sv))){
                  if (!SO->PolyArea) {
                     fprintf(SUMA_STDOUT, 
                              "%s: Computing required mesh area.\n", FuncName);
                     if (!SUMA_SurfaceMetrics (SO, "PolyArea", NULL)) {
                        fprintf (SUMA_STDERR,
                                 "Error %s: Failed in SUMA_SurfaceMetrics.\n", 
                                 FuncName);
                        break;
                     }
                  }
                  SO->SC = SUMA_Surface_Curvature (SO->NodeList, SO->N_Node, 
                              SO->NodeNormList, SO->PolyArea, 
                              SO->N_FaceSet, SO->FN, SO->EL, "Curvs_c.txt", 1);
                  if (SO->SC == NULL) {
                        fprintf( stderr,
                                 "Error %s: Failed in SUMA_Surface_Curvature\n", 
                                 FuncName);
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
               if ((SO = SUMA_SV_Focus_SO(sv))) {
                  Cx = (float *)SUMA_GetCx(SO->idcode_str, 
                                           SUMAg_CF->DsetList, 0);
                  if (Cx) {
                     SUMA_S_Err("Cx must be null prior to new assignment");
                     break;
                  }
                  Cx = SUMA_Convexity   ( SO->NodeList, SO->N_Node, 
                                          SO->NodeNormList, SO->FN, NULL);   
                  if (Cx == NULL) {
                        fprintf(stderr,"Error %s: Failed in SUMA_Convexity\n", 
                                       FuncName);
                        break;
                  }   
                  /* smooth estimate twice */
                  attr_sm = SUMA_SmoothAttr_Neighb (Cx, SO->N_Node, NULL, 
                                                    SO->FN, 1, NULL, 1);
                  if (attr_sm == NULL) {
                        fprintf(stderr,
                                "Error %s: Failed in SUMA_SmoothAttr_Neighb\n", 
                                FuncName);
                        break;
                  }   
                  Cx = SUMA_SmoothAttr_Neighb (attr_sm, SO->N_Node, Cx, 
                                               SO->FN, 1, NULL, 1);
                  if (attr_sm) SUMA_free(attr_sm);

                  fprintf( SUMA_STDOUT, 
                           "%s: Use SUMA_ScaleToMap to colorize Conv.txt "
                           "and display it on surface.\n", FuncName);
                  CM = SUMA_FindNamedColMap ("ngray20");
                  if (CM == NULL) {
                     fprintf (SUMA_STDERR,
                              "Error %s: Could not get standard colormap.\n", 
                              FuncName);
                     exit (1); 
                  }

                  /* get the options for creating the scaled color mapping */
                  OptScl = SUMA_ScaleToMapOptInit();
                  if (!OptScl) {
                     SUMA_S_Err("Could not get scaling option structure.");
                     exit (1); 
                  }

                  /* work the options a bit */
                  OptScl->ApplyClip = YUP;
                  IntRange[0] = 5; IntRange[1] = 95; /* percentile clip range*/ 
                  Vsort = SUMA_PercRange (Cx, NULL, SO->N_Node, 
                                          IntRange, IntRange, NULL); 
                  OptScl->IntRange[0] = IntRange[0]; 
                  OptScl->IntRange[1] = IntRange[1];

                  OptScl->BrightFact = 0.4;

                  /* map the values in Cx to the colormap */
                     /* allocate space for the result */
                     SV = SUMA_Create_ColorScaledVect(SO->N_Node, 0);
                     if (!SV) {
                        fprintf (SUMA_STDERR,
                                 "Error %s: Could not allocate for SV.\n", 
                                 FuncName);
                        exit(1);
                     }

                     /* finally ! */
                     /*fprintf ( SUMA_STDERR,"%s: 1st color in map %f %f %f\n", 
                                 FuncName, 
                                 CM->M[0][0], CM->M[0][1],CM->M[0][2]);*/
                     if (!SUMA_ScaleToMap (Cx, SO->N_Node, Vsort[0], 
                                          Vsort[SO->N_Node-1], CM, OptScl, SV)) {
                        fprintf (SUMA_STDERR,
                                 "Error %s: Failed in SUMA_ScaleToMap.\n", 
                                 FuncName);
                        exit(1);
                     }

                     /* Now place SV in the color array */
                     glar_ColorList = SUMA_GetColorList (sv, SO->idcode_str);
                     if (!glar_ColorList) {
                        SUMA_S_Err("NULL glar_ColorList. BAD.");
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

                  fprintf(SUMA_STDOUT, "%s: Convexity mapping done ...\n", 
                                       FuncName);
                  SUMA_postRedisplay(w, clientData, callData); 
               }  
            }
            break;
            
         case XK_comma:
            if (!SUMA_comma_Key(sv, "comma", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;

         case XK_period:
            if (!SUMA_period_Key(sv, "period", "interactive")) {
               SUMA_S_Err("Failed in key func.");
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
         
         case XK_F9: /*F9 */
            if (!SUMA_F9_Key(sv, "F9", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
            
         case XK_F10: /*F10 */
            if (!SUMA_F10_Key(sv, "F10", "interactive", NULL)) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
         case XK_F11: /* F11 */
            if (!SUMA_F11_Key(sv, "F11", "interactive", NULL)) {
               SUMA_S_Err("Failed in key func.");
            }
            break;   
         case XK_F12: /* F12 */
            if (!SUMA_F12_Key(sv, "F12", "interactive")) {
               SUMA_S_Err("Failed in key func.");
            }
            break;
         case XK_F13:
            if (SUMAg_CF->Dev) {
               DList *striplist=NULL;
               float Eq[4];
               int *Vis_IDs, N_vis;
               SUMA_SurfaceObject *SO=NULL;
               Vis_IDs = (int *)SUMA_malloc(sizeof(int)*SUMAg_N_DOv);
               N_vis = SUMA_VisibleSOs (sv, SUMAg_DOv, Vis_IDs, 0);
               if (N_vis) {
                  SO = (SUMA_SurfaceObject *)SUMAg_DOv[Vis_IDs[0]].OP;
                  /* Axial plane */
                  Eq[0] = Eq[1] = 0.0; Eq[2] = 1.0; Eq[3] = -SO->Center[2];
                  SUMA_S_Warnv("Kill me!\nEq:[%f %f %f %f], step: %f\n", 
                                 Eq[0], Eq[1], Eq[2], Eq[3], SO->EL->AvgLe);
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
            }else if (SUMA_ALTHELL) {
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
            }else if (SUMA_ALTHELL) {
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
            }else if (SUMA_ALTHELL) {
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
            }else if (SUMA_ALTHELL) {
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
      SUMAg_CF->X->ButtonDown=1;
      pButton = Bev.button;
      SUMA_LHv("In ButtonPress Button %d, %d @ x,y=%d,%d\n", 
                              pButton, SUMAg_CF->X->ButtonDown, Bev.x, Bev.y);  
      if (  SUMAg_CF->SwapButtons_1_3 || 
            (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
         if (pButton == Button1) pButton = Button3;
         else if (pButton == Button3) pButton = Button1;
      }
      if (SUMAg_CF->Echo_KeyPress) {
         fprintf (SUMA_STDERR,"Button Press: %d (%d,%d,%d,%d,%d)\n"
                              , pButton, Button1, Button2, Button3, Button4,
                              Button5);
      }
      
     /* trap for double click */
      if (Bev.time - B1time < SUMA_DOUBLE_CLICK_MAX_DELAY) {
         if (LocalHead) fprintf(SUMA_STDERR, "%s: Double click.\n", FuncName);
         DoubleClick = YUP;
      } else {
         DoubleClick = NOPE;
      }
      

      if (strstr(sv->State, "GMATRIX")==sv->State) {
         /* For graph in matrix representation swap buttons 1 and 2 */
         switch (pButton) {
            case Button1:
               pButton = Button2;
               break;
            case Button2:
               pButton = Button1;
               break;
         }
      }
      

      B1time = Bev.time; 
      M1time = 0;      
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
               if (DoubleClick) {
                  if (Kev.state & ControlMask) SUMA_ResetPrying(sv);
                  else {
                     SUMA_HOME_QUAT(sv->StdView, 
                                    sv->GVS[sv->StdView].currentQuat);
                     SUMA_postRedisplay(w, NULL, NULL);
                  }
               }
            }
            break;
         case Button4:
         case 6:  /* This is shift and wheel on mac, Button6 is not in X.h ! */
            if (pButton==6 || Bev.state & ShiftMask) {
               SUMA_ALL_DO *ado=NULL;
               char variant[2];
               #if 0
               int ii;
               SUMA_VolumeObject *VO=NULL;
               for (ii=0; ii<SUMAg_N_DOv; ++ii) {
                  if (SUMA_isVO(SUMAg_DOv[ii])) {
                     VO = (SUMA_VolumeObject *)(SUMAg_DOv[ii].OP);
                     if (VO->SelectedCutPlane >= 0) {
                        SUMA_LHv("Moving cut plane %d\n", 
                                       VO->SelectedCutPlane);
                        if (!SUMA_MoveCutplane(VO, VO->SelectedCutPlane, 1.0)) {
                           SUMA_SLP_Err("Bad");
                        }
                     }
		     /* JB: only allow cutplane from 1st volume object, 
                  otherwise remove 'break' */
		     break;
                  }
               }
               SUMA_postRedisplay(w, NULL, NULL);
               #else
               if ((ado = SUMA_SV_Focus_ADO(sv))) {
                  switch (ado->do_type) {
                     case VO_type: {
                        float incr = 1.0;
                        SUMA_VolumeObject *vo=(SUMA_VolumeObject *)ado;
                        SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
                        if (VSaux && VSaux->PR) {
                           if (SUMA_dset_gui_slice_from_tex_slice_d(vo->VE, 0, 
                                          VSaux->PR->dAltSel+SUMA_VOL_SLC_EQ0,
                                          0, variant, NULL)   >=0 ){
                              SUMA_set_slice(ado, variant, &incr, 
                                             "increment", 0);
                           }
                        }
                        SUMA_postRedisplay(w, NULL, NULL);
                        break; }
                     default:
                        SUMA_LH("Nothing here for types %s\n",
                              ADO_TNAME(ado));
                        break;
                  }
               } 
               #endif
            } else if (pButton==6 || Bev.state & ControlMask) {
               SUMA_ALL_DO *ado=NULL;
               SUMA_X_SurfCont *SurfCont;
               if (MASK_MANIP_MODE(sv)) {
                  ado = SUMA_whichADOg(sv->MouseMode_ado_idcode_str);
                  if (ado && ado->do_type == MASK_type) {
                     SUMA_MaskDO *mdo = (SUMA_MaskDO *)ado;
                     float fv[3];
                     int irow=-1;
                     {
                        fv[0] = mdo->hdim[0]-(0.2*mdo->init_hdim[0]); 
                        fv[1] = mdo->hdim[1]-(0.2*mdo->init_hdim[1]); 
                        fv[2] = mdo->hdim[2]-(0.2*mdo->init_hdim[2]);
                        if (fv[0] < 0 || fv[1] < 0 || fv[2] < 0) {
                           SUMA_BEEP;
                           break;
                        }
                        SUMA_MDO_New_Dim(mdo, fv);
                     }
                     if ((SurfCont=SUMA_ADO_Cont(ado))) {
                        irow = SUMA_ObjectID_Row(SurfCont->MaskTable, 
                                                 ADO_ID(ado));
                        if (irow >= 0) {
                           SUMA_InitMasksTable_row(SurfCont,mdo, irow);
                        }
                     }
                     SUMA_NEW_MASKSTATE();
                     /* enough for now */
                     goto REDISP;
                  }
               } else if ((ado = SUMA_SV_Focus_ADO(sv)) && 
                           ado->do_type == GRAPH_LINK_type &&
                           !strcmp(SUMA_ADO_variant(ado),"GMATRIX")) {
                  SUMA_OVERLAYS *Sover = NULL;
                  SUMA_LH("Going forward one sub-brick");
                  Sover = SUMA_ADO_CurColPlane(ado);
                  SUMA_SwitchColPlaneIntensity( ado, Sover, 
                                                SUMA_FORWARD_ONE_SUBBRICK, 1);   
                  /* redisplay done in function above ... */
                  SUMA_RETURNe;
               }
            } else {
               if (!SUMA_Z_Key(sv, "z", "interactive")) {
                  SUMA_S_Err("Failed in key func.");
               }
            }
            break;
         case Button5:
         case 7: /* This is shift and wheel on mac, Button7 is not in X.h ! */
            if (pButton==7 || Bev.state & ShiftMask) {
               SUMA_ALL_DO *ado=NULL;
               char variant[2];
               #if 0
               int ii;
               SUMA_VolumeObject *VO=NULL;
               for (ii=0; ii<SUMAg_N_DOv; ++ii) {
                  if (SUMA_isVO(SUMAg_DOv[ii])) {
                     VO = (SUMA_VolumeObject *)(SUMAg_DOv[ii].OP);
                     if (VO->SelectedCutPlane >= 0) {
                        SUMA_LHv("Moving cut plane %d\n", 
                                       VO->SelectedCutPlane);
                        if (!SUMA_MoveCutplane(VO, VO->SelectedCutPlane, -1.0)) {
                           SUMA_SLP_Err("Bad");
                        }
                     }
		     /* JB: only allow cutplane from 1st volume object, 
                  otherwise remove 'break' */
		     break;
                  }
               }
               SUMA_postRedisplay(w, NULL, NULL);
               #else
               if ((ado = SUMA_SV_Focus_ADO(sv))) {
                  switch (ado->do_type) {
                     case VO_type: {
                        float incr = -1.0;
                        SUMA_VolumeObject *vo=(SUMA_VolumeObject *)ado;
                        SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
                        if (VSaux && VSaux->PR) {
                           if (SUMA_dset_gui_slice_from_tex_slice_d(vo->VE, 0, 
                                          VSaux->PR->dAltSel+SUMA_VOL_SLC_EQ0,
                                          0, variant, NULL)   >=0 ){
                              SUMA_set_slice(ado, variant, &incr, 
                                             "increment", 0);
                           }
                        }
                        SUMA_postRedisplay(w, NULL, NULL);
                        break; }
                     default:
                        SUMA_LH("Nothing here for types %s\n",
                              ADO_TNAME(ado));
                        break;
                  }
               }
               #endif
            } else if (pButton==7 || Bev.state & ControlMask) {
               SUMA_ALL_DO *ado=NULL;
               SUMA_X_SurfCont *SurfCont;
               if (MASK_MANIP_MODE(sv)) {
                  ado = SUMA_whichADOg(sv->MouseMode_ado_idcode_str);
                  if (ado && ado->do_type == MASK_type) {
                     SUMA_MaskDO *mdo = (SUMA_MaskDO *)ado;
                     float fv[3];
                     int irow=-1;
                     {
                        fv[0] = mdo->hdim[0]+(0.2*mdo->init_hdim[0]); 
                        fv[1] = mdo->hdim[1]+(0.2*mdo->init_hdim[1]); 
                        fv[2] = mdo->hdim[2]+(0.2*mdo->init_hdim[2]);
                        SUMA_MDO_New_Dim(mdo, fv);
                     }
                     if ((SurfCont=SUMA_ADO_Cont(ado))) {
                        irow = SUMA_ObjectID_Row(SurfCont->MaskTable, 
                                                 ADO_ID(ado));
                        if (irow >= 0) {
                           SUMA_InitMasksTable_row(SurfCont,mdo, irow);
                        }
                     }
                     SUMA_NEW_MASKSTATE();
                     /* enough for now */
                     goto REDISP;
                  }
               } else if ((ado = SUMA_SV_Focus_ADO(sv)) && 
                           ado->do_type == GRAPH_LINK_type &&
                           !strcmp(SUMA_ADO_variant(ado),"GMATRIX")) {
                  SUMA_OVERLAYS *Sover = NULL;
                  SUMA_LH("Switching overlay back one");
                  Sover = SUMA_ADO_CurColPlane(ado);
                  SUMA_SwitchColPlaneIntensity( ado, Sover, 
                                                SUMA_BACK_ONE_SUBBRICK, 1);   
                  /* redisplay done in function above ... */
                  SUMA_RETURNe;
               }
            } else {
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
            
         case Button3: {
               SUMA_LHv("Button 3 down plain jane, "
                            "viewer #%d : X=%f, Y = %f\n", 
                            SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv),
                              (float)Bev.x, (float)Bev.y);
                             
               /* Clear any pre-existing selection */
               if (!SUMA_Add_To_PickResult_List(sv, NULL, "TERSUM", NULL)) {
                  SUMA_S_Err("Failed to clear selections");
                  break;
               }
               
               /* Bev.state does work in the line below, 
                  unlike Mev.state further down.
                  Using Kev.state anyway because it works in both cases */
               if ((Kev.state & ShiftMask) && (Kev.state & ControlMask)) {
                  SUMA_LH("Allowing callbacks");
                  SUMAg_CF->HoldClickCallbacks = 0;
               } else {
                  SUMA_LH("Holding back callbacks");
                  SUMAg_CF->HoldClickCallbacks = 1;
               }
               
               /* are we in ROI drawing mode ? */
               if (  SUMAg_CF->ROI_mode 
                     && SUMA_SV_Focus_SO(sv) && !(Bev.state & ShiftMask)) {
                  /* ROI drawing mode */
                  ROI_mode = YUP;     
               } else {
                  ROI_mode = NOPE;
                  
               }

               if (!(Kev.state & ShiftMask) && (Kev.state & ControlMask)) {
                  SUMA_LH("Yoking intensity to node selection");
                  SUMAg_CF->YokeIntToNode = 1;
               } else {
                  SUMA_LH("Holding back yoking");
                  SUMAg_CF->YokeIntToNode = 0;
               }

               SUMA_LH("Get the selection line, bitte");
               if (!SUMA_GetSelectionLine (  sv, (int)Bev.x, (int)Bev.y, 
                                             sv->Pick0, sv->Pick1, 0, 
                                             NULL, NULL, NULL)) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed in SUMA_GetSelectionLine.\n", 
                           FuncName);
                  break;
               }
               
               if (DoubleClick && !ROI_mode){/*See if you are selecting masks */
                  SUMA_ALL_DO *mado=NULL, *ado=NULL;
                  SUMA_GRAPH_SAUX *GSaux=NULL;
                  /* you do not want to waist time doing double calculations if 
                     the user clicks twice by mistake */
                  /* make sure no viewer, other than the one clicked in is in 
                     momentum mode */
                  if (SUMAg_N_SVv > 1) {
                     ii = SUMA_WhichViewerInMomentum (SUMAg_SVv, 
                                                      SUMAg_N_SVv, NULL);
                     if (ii >= 0) {
                        sprintf (s, "You cannot select or draw while viewers\n"
                                    "(like viewer %c) are in momentum mode.\n", 
                                    ii+65);
                        SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                              s, FuncName, SMT_Error, 
                                              SMA_LogAndPopup);
                        SUMA_RETURNe;
                     }
                  }
                  
                  /* Any hits for masks? */
                  hit = SUMA_ComputeLineMaskIntersect (sv, SUMAg_DOv, 0, &mado);
                  if (hit < 0) {
                    SUMA_S_Err("Failed in SUMA_ComputeLineMaskIntersect.");
                  } else if (hit > 0) {
                     SUMA_LH("Mask was double clicked");
                     if (!MASK_MANIP_MODE(sv)) {
                        SUMA_S_Note("Turning on mask manip mode");
                        if (!SUMA_SetMouseMode(sv,SUMA_MASK_MANIP_MMODE,
                                                  (void *)(ADO_ID(mado)))) {
                           SUMA_S_Warn("Mask manip mode could not be set");
                        }
                     } else {
                        SUMA_S_Note("Turning off mask manip mode");
                        if (!SUMA_SetMouseMode(sv,SUMA_MASK_MANIP_MMODE,NULL)) {
                           SUMA_S_Warn("Mask manip mode could not be set");
                        }
                     }
                     /* Not much else to do */
                     goto REDISP;
                  } else if (MASK_MANIP_MODE(sv)) { /* turn it off */
                     SUMA_S_Note("Turning off mask manip mode");
                     if (!SUMA_SetMouseMode(sv,SUMA_MASK_MANIP_MMODE,NULL)) {
                        SUMA_S_Warn("Mask manip mode could not be set");
                     }
                     /* Not much else to do */
                     goto REDISP;
                  } else if ((ado = SUMA_SV_Focus_ADO(sv)) && 
                              ado->do_type == GRAPH_LINK_type &&
                             (GSaux = SUMA_ADO_GSaux(ado)) && 
                             GSaux->PR && GSaux->PR->datum_index == -1 &&
                             GSaux->PR->iAltSel[SUMA_ENODE_0] != -1 &&
                             GSaux->IgnoreSelection == 0) { 
                           /* turn off node selection to allow all connections
                              to be visible. Execute only if we have a graph
                              in focus (in 3D drawing) and a point is selected
                              and selection is not being ignored*/
                        SUMA_LH("Ignoring selection for graph display");
                        GSaux->IgnoreSelection = 1;
                        SUMA_FlushPickBufferForDO(ado);
                        goto REDISP;   
                  }
                  SUMA_LH("No mask hit, and no selection needs ignoring");
               }
               
               if (!DoubleClick) {
                  /* you do not want to waist time doing double calculations if 
                     the user clicks twice by mistake */
                  /* make sure no viewer, other than the one clicked in is in 
                     momentum mode */
                  if (SUMAg_N_SVv > 1) {
                     ii = SUMA_WhichViewerInMomentum (SUMAg_SVv, 
                                                      SUMAg_N_SVv, NULL);
                     if (ii >= 0) {
                        sprintf (s, "You cannot select or draw while viewers\n"
                                    "(like viewer %c) are in momentum mode.\n", 
                                    ii+65);
                        SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                              s, FuncName, SMT_Error, 
                                              SMA_LogAndPopup);
                        SUMA_RETURNe;
                     }
                  }  
                  
                  #if 0
                  /* Try this if you are having OpenGLStateReset problems at
                     node selection time. It is inefficient, but helps point
                     to the problem. 
                     Look at recent updates to SE_Redisplay*All* for the more
                     appropriate fix */
                  SUMA_S_Note("Blunt fix:");
                  SUMA_OpenGLStateReset(SUMAg_DOv, SUMAg_N_DOv, sv);
                  SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
                  #endif

                  /* perform the intersection calcluation and mark the surface */
                  SUMA_LHv("Finding hit: %d %d,\n"
                           "Pick0: %f %f %f, Pick1: %f %f %f\n",
                           (int)Bev.x, (int)Bev.y, 
                           sv->Pick0[0], sv->Pick0[1], sv->Pick0[2], 
                           sv->Pick1[0], sv->Pick1[1], sv->Pick1[2]);
                  
                  
                  if (1) {
                     hit = SUMA_ComputeLineDOsIntersect (sv, SUMAg_DOv, 0, NULL);
                     if ( (Kev.state & ShiftMask) && 
                         !(Kev.state & ControlMask) &&
                         !SUMA_ALTHELL && !SUMAg_CF->ROI_mode){ 
                         /* Show me the click buffer */
                        SUMA_MarkPickInBuffer4(sv, 1, NULL);
                     }
                     if (hit < 0) {
                        SUMA_S_Err("Failed in SUMA_ComputeLineDOsIntersect.");
                     }
                  }
               
                  if (1) {
                      SUMA_LH("Trying for volume intersections");
                      hit =  SUMA_ComputeLineVOslicesIntersect(sv, SUMAg_DOv, 
                                                               0, NULL);
                      if (hit < 0) {
                         fprintf( SUMA_STDERR,
                            "Error %s: "
                            "Failed in SUMA_MarkLineVOslicesIntersect.\n",
                                  FuncName);
                      }
                  }
                  
                  if (1) {
                      SUMA_LH("Trying for volume VR intersections");
                      hit =  SUMA_ComputeLineVOvrIntersect(sv, SUMAg_DOv, 
                                                               0, NULL);
                      if (hit < 0) {
                         fprintf( SUMA_STDERR,
                            "Error %s: "
                            "Failed in SUMA_MarkLineVOvrIntersect.\n",
                                  FuncName);
                      }
                  }
                  #if 0
                  if (SUMA_ALTHELL || 
                      SUMA_VisibleSOs(sv, SUMAg_DOv, NULL, 0) == 0) {
                      SUMA_LH("Trying for cutplanes");
                      hit = SUMA_MarkLineCutplaneIntersect (sv, SUMAg_DOv, 0);
                      if (hit < 0) {
                         fprintf( SUMA_STDERR,
                            "Error %s: "
                            "Failed in SUMA_MarkLineCutplaneIntersect.\n",
                                  FuncName);
                      }
                  }
                  #endif
                     
                  SUMA_LH("Checking on registered surfaces");
                  SwasHit = 0;
                  ii = SUMA_RegisteredSOs(sv, SUMAg_DOv, NULL);
                  if (ii == 0) { /* no surfaces, break */
                     SUMA_LH("No registrants");
                  } else {
                     /* have surfaces, find hits */
                     hit = SUMA_ComputeLineSurfaceIntersect (sv, SUMAg_DOv, 
                                                             0, NULL);
                     if (hit < 0) {
                       SUMA_S_Err("Failed in SUMA_ComputeLineSurfaceIntersect.");
                     } else if (hit > 0) SwasHit = 1;
                     
                  }
                  
               }
               
               
               if (ROI_mode && (SwasHit || DoubleClick)) {
                  /* keep track of mouse motion in window */
                  if (!SUMA_CreateBrushStroke (sv)) {
                     SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                           "Failed to create BrushStroke.", 
                                           FuncName, 
                                           SMT_Error, SMA_LogAndPopup);
                     SUMA_RETURNe;

                  }

                  SUMA_AddToBrushStroke (sv, (int)Bev.x, (int)Bev.y, sv->Pick0,
                     sv->Pick1, YUP); 
               }
               
               ASSESS:
               SUMA_LH("Assessment %d", dlist_size(sv->SelAdo));
               if (dlist_size(sv->SelAdo)) {
                  if (!SUMA_Process_Selected_ADO(sv,SUMA_ALTHELL)) {
                     SUMA_S_Err("Failed to process selected ados");
                     goto OUT;
                  }
                  
                  SUMA_LH("Calling redisplay");
                  /* redisplay */
                  sv->ResetGLStateVariables = YUP;
                  SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);            
               }
               goto OUT;
               
               REDISP:
               SUMA_LH("Calling redisplay without assessment");
               sv->ResetGLStateVariables = YUP;
               SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);        
               goto OUT;
   
               OUT:
               /* reset hold on xforms */
               SUMAg_CF->HoldClickCallbacks = 0;
            break; }
      } /* switch type of button Press */
      break;
      
   case ButtonRelease:
      SUMAg_CF->X->ButtonDown=0;
      M1time = 0;
      rButton = Bev.button;
      SUMA_LHv("In ButtonRelease Button %d %d @ x,y=%d,%d\n", 
                   rButton, SUMAg_CF->X->ButtonDown, Bev.x, Bev.y); 
      if (SUMAg_CF->Echo_KeyPress) {
         fprintf (SUMA_STDERR,"Button Release: %d (%d,%d,%d,%d,%d)\n"
                              , rButton, Button1, Button2, Button3, Button4,
                              Button5);
      }

      if (SUMAg_CF->SwapButtons_1_3 || 
          (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
         if (rButton == Button1) rButton = Button3;
         else if (rButton == Button3) rButton = Button1;
      }
      
      if (strstr(sv->State, "GMATRIX")==sv->State) {
         /* For graph in matrix representation swap buttons 1 and 2 */
         switch (rButton) {
            case Button1:
               rButton = Button2;
               break;
            case Button2:
               rButton = Button1;
               break;
         }
      }
      

      switch (rButton) { /* switch type of button Press */
         case Button3:
            if (LocalHead) 
               fprintf(SUMA_STDERR,"%s: In ButtonRelease3\n", FuncName); 
                        
            if (SUMAg_CF->ROI_mode) {
               SUMA_DRAWN_ROI *DrawnROI = NULL;
               SUMA_BRUSH_STROKE_ACTION BsA=SUMA_BSA_Undefined;
               
               if (sv->BS) { 
                  /* Process the brush stroke*/
                  if (DoubleClick) BsA = SUMA_BSA_JoinEnds;
                  else BsA = SUMA_BSA_AppendStrokeOrFill;
                  if (!(DrawnROI = SUMA_ProcessBrushStroke (sv, BsA))) {
                     if (LocalHead) 
                        fprintf (SUMA_STDERR, 
                                 "%s: NULL DrawnROI returned.\n", FuncName);
                     SUMA_ClearBrushStroke (sv);
                     break;
                  }

                  /* Showme the DrawnROI */
                  if (LocalHead) SUMA_ShowDrawnROI (DrawnROI, NULL, NOPE);

                  /* do smething with the BrushStroke, then wipe it clean, 
                     OK to show even if empty*/
                  if (LocalHead) SUMA_ShowBrushStroke (sv, NULL);

                  /* SUMA_DrawBrushStroke (sv, YUP); */
                  SUMA_ClearBrushStroke (sv);

                  /* redisplay all others */
                  if (!list) list = SUMA_CreateList ();
                  SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, 
                           SE_RedisplayNow_AllOtherVisible, SES_SumaWidget, sv);
                  SUMA_Engine (&list);
               
                  /* redisplay . 
                     DO NOT REDISPLAY WITH SE_Redisplay_AllVisible or 
                     you will have GL state synchronization problems */
                  sv->ResetGLStateVariables = YUP;
                  SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);

               }/* if sv->BS */
            } /* if SUMAg_CF->ROImode */
            
            if (sv->Focus_DO_ID > 0){/* Get surface controller page in sync */
               if (!SUMA_IS_CONTPAGE_ON_TOP(SUMAg_CF->X->AllMaskCont)) { 
                  /* Switch only if 'sticky' mask controller page 
                     is not on top */
                  SUMA_ALL_DO *ado = SUMA_SV_Focus_ADO(sv);
                  if (ado) {
                     SUMA_LHv("Will call Init for %s if %d\n", 
                        SUMA_ADO_Label(ado), 
                        SUMA_isADO_Cont_Realized(ado));
                     if (SUMA_isADO_Cont_Realized(ado))
                        SUMA_Init_SurfCont_SurfParam(ado);
                  }
               } else {
                  if (!SUMA_InitMasksTable(SUMAg_CF->X->AllMaskCont)) {
                     SUMA_S_Err("Failed to initialize mask table");
                  }
               }  
            }
         break;
         case Button1:
            SUMA_LH("In Button 1 release\n");
            if (sv->GVS[sv->StdView].vLHpry0[0] != 
                                          sv->GVS[sv->StdView].vLHpry[0] ||
                sv->GVS[sv->StdView].vLHpry0[1] != 
                                          sv->GVS[sv->StdView].vLHpry[1] ||
                sv->GVS[sv->StdView].vLHpry0[2] != 
                                          sv->GVS[sv->StdView].vLHpry[2] ){
               /* update the normals just now, this would not be needed if
                 SUMA_ApplyPrying has set recompute_normals = 1*/
               SUMA_RecomputeNormsPrying(sv);
               SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
            }
            sv->GVS[sv->StdView].vLHpry0[0] = sv->GVS[sv->StdView].vLHpry[0];
            sv->GVS[sv->StdView].vLHpry0[1] = sv->GVS[sv->StdView].vLHpry[1];
            sv->GVS[sv->StdView].vLHpry0[2] = sv->GVS[sv->StdView].vLHpry[2];
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
         else fprintf(stdout,"   Something mot, button %d\n", Bev.button);
      }
      if (SUMAg_CF->Echo_KeyPress) {
         if (Mev.state & Button1MotionMask) 
            fprintf(SUMA_STDERR,"   B1 mot\n");
         else if (Mev.state & Button2MotionMask) 
            fprintf(SUMA_STDERR,"   B2 mot\n");
         else if (Mev.state & Button3MotionMask) 
            fprintf(SUMA_STDERR,"   B3 mot\n");
         else if (Mev.state & Button4MotionMask) 
            fprintf(SUMA_STDERR,"   B4 mot\n");
         else if (Mev.state & Button5MotionMask) 
            fprintf(SUMA_STDERR,"   B5 mot\n");
         else if (Mev.state) fprintf(SUMA_STDERR,
                              "   Something mot, button %d\n", Bev.button);
      }

      if (  SUMAg_CF->SwapButtons_1_3 || 
            (SUMAg_CF->ROI_mode && SUMAg_CF->Pen_mode)) {
        if (((Mev.state & Button3MotionMask) && (Mev.state & Button2MotionMask)) 
         || ((Mev.state & Button2MotionMask) && (Mev.state & ShiftMask))) {
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
         if (((Mev.state & Button1MotionMask) && (Mev.state & Button2MotionMask))
          || ((Mev.state & Button2MotionMask) && (Mev.state & ShiftMask))) {
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
      
      if (strstr(sv->State, "GMATRIX")==sv->State) {
         /* For graph in matrix representation swap buttons 1 and 2 */
         SUMA_LH("In graph matrix, swapping 1/2 motion");
         switch (mButton) {
            case SUMA_Button_1_Motion:
               mButton = SUMA_Button_2_Motion;
               break;
            case SUMA_Button_2_Motion:
               mButton = SUMA_Button_1_Motion;
               break;
         }
      }
      
      switch (mButton) {
         case SUMA_Button_12_Motion:
         case SUMA_Button_2_Shift_Motion:
            /*fprintf(SUMA_STDERR,"%s: In motion, Butt1 & Butt2\n", FuncName);*/
            sv->GVS[sv->StdView].zoomDelta = 1.0 + 
                                             (float)((int)Mev.y - 
                                 sv->GVS[sv->StdView].zoomBegin)/MOUSE_ZOOM_FACT;
            if (sv->GVS[sv->StdView].zoomDelta > 2.0) 
               sv->GVS[sv->StdView].zoomDelta = 2.0;
            else if (sv->GVS[sv->StdView].zoomDelta < 0.5) 
               sv->GVS[sv->StdView].zoomDelta = 0.5;
            sv->FOV[sv->iState] /= sv->GVS[sv->StdView].zoomDelta;
            if (sv->FOV[sv->iState] < FOV_MIN) sv->FOV[sv->iState] = FOV_MIN;
            else if (sv->FOV[sv->iState] > FOV_MAX) 
               sv->FOV[sv->iState] = FOV_MAX;
            sv->GVS[sv->StdView].zoomBegin = (float)(int)Mev.y;
            /*fprintf(stdout, "FOV zoom Delta = %f=n", 
                              sv->GVS[sv->StdView].zoomDelta);*/
            /* Now update the zoom compensation variable */
            if (sv->ZoomCompensate) {
               sv->ZoomCompensate = sqrt(sv->FOV[sv->iState] / 
                                    SUMA_sv_auto_fov(sv));
                     /* slow down compensation, with sqrt*/
               if (sv->ZoomCompensate > 1) 
                  sv->ZoomCompensate = 1.0; 
                        /* no need to compensate at low zooms */
               else if (sv->ZoomCompensate < 0.05) 
                  sv->ZoomCompensate = 0.05; /* no need to go lower */ 
            }
            ii = SUMA_WhichSV (sv, SUMAg_SVv, SUMAg_N_SVv);
            SUMA_postRedisplay(w, clientData, callData);    
            break;
            
         case SUMA_Button_1_Motion:     
            /* fprintf(SUMA_STDERR,"%s: In motion, Butt1 \n", FuncName); */
            mevx = (float)Mev.x;
            mevy = (float)Mev.y;
            wwid = (float)sv->X->aWIDTH;
            whei = (float)sv->X->aHEIGHT;
            /* spinning mode */
            if (sv->ZoomCompensate) {
               zc_fac = sv->ZoomCompensate;
            }else {
               zc_fac = 1.0;
            }            
            sv->GVS[sv->StdView].spinDeltaX = 
               (mevx - sv->GVS[sv->StdView].spinBeginX);
            sv->GVS[sv->StdView].spinDeltaY = 
               (mevy - sv->GVS[sv->StdView].spinBeginY);
            if (M1time) {
               M1delta = Mev.time - M1time;
               mvdeltax = Mev.x - mvxlast;
               mvdeltay = Mev.y - mvylast;
            } else {
               M1delta = 0;
               mvdeltax = 0;
               mvdeltay = 0;
            }
            M1time = Mev.time;
            mvxlast= Mev.x;
            mvylast= Mev.y;
            
            /* compute move rate in pixels per milliseconds,
            You could use this to add a gain on the amount of rotation,
            but before you could use those factors, you'll need to scale
            the results to something appropriate */
            mvx_fac = ((SUMA_ABS((float)mvdeltax)/(M1delta+0.01))); 
            mvy_fac = ((SUMA_ABS((float)mvdeltay)/(M1delta+0.01))); 
            
            #if 0
            fprintf(stdout,"\n"
                           "spinBeginX %f \n"
                           "spinBeginY %f \n"
                           "spinDeltaX %f \n"
                           "spinDeltaY %f \n"
                           "X->aWIDTH %d  \n"
                           "X->aHEIGHT %d\n"
                           "ZoomCompensate %f\n"
                           "mv[xy]last [%d %d]\n"
                           "mvdelta[xy]last [%d %d]\n"
                           "MoveRatePixelsPerms [%f %f]\n"
                           , 
                        sv->GVS[sv->StdView].spinBeginX, 
                        sv->GVS[sv->StdView].spinBeginY, 
                        sv->GVS[sv->StdView].spinDeltaX, 
                        sv->GVS[sv->StdView].spinDeltaY, 
                        sv->X->aWIDTH, sv->X->aHEIGHT, sv->ZoomCompensate,
                        mvxlast, mvylast,
                        mvdeltax, mvdeltay,
                        mvx_fac, mvy_fac
                        );
            #else
            /* fprintf(stdout,"%f %f; ", mvx_fac, mvy_fac); */
            #endif
            /* do not use movement rate before you scale it properly */
            mvx_fac = mvy_fac = 1.0;
            if (sv->GVS[sv->StdView].spinDeltaX || 
                sv->GVS[sv->StdView].spinDeltaY){
               if (SUMA_Shft_Event(SUMAg_CF->lev)) {
                  float a[3], cQ[4];
                  /* rotate about Z axis   */
                  a[0] = 0.0; a[1] = 0.0; a[2] = 1.0;
                  axis_to_quat(a, 
                              -SUMA_SIGN(mvdeltax)*sqrt(SUMA_ABS(mvdeltax))*
                                 sv->ArrowRotationAngle, 
                              cQ);
                  /*add rotation */
                  add_quats ( cQ, 
                              sv->GVS[sv->StdView].currentQuat, 
                              sv->GVS[sv->StdView].currentQuat);
               } else if (SUMA_Cont_Event(SUMAg_CF->lev)) {
                  float val[3];
                  val[0] = sv->GVS[sv->StdView].spinDeltaX;
                  val[1] = sv->GVS[sv->StdView].spinDeltaY;
                  val[2] = 0.0;
                  SUMA_ApplyPrying(sv, val, "mouse", 0); 
                                       /* update normals at release */
               } else if (SUMA_Plain_Event(SUMAg_CF->lev)){
                  trackball(  sv->GVS[sv->StdView].deltaQuat, 
                              (2*sv->GVS[sv->StdView].spinBeginX - wwid) /
                              wwid*zc_fac*mvx_fac, 
                              (whei - 2*sv->GVS[sv->StdView].spinBeginY) / 
                              whei*zc_fac*mvy_fac,
                              (2*mevx - wwid)/wwid*zc_fac*mvx_fac, 
                              (whei - 2*mevy)/whei*zc_fac*mvy_fac); 
                                    /* comput the increment Quat */
                  sv->GVS[sv->StdView].spinBeginX = mevx;
                  sv->GVS[sv->StdView].spinBeginY = mevy;
                  add_quats ( sv->GVS[sv->StdView].deltaQuat, 
                              sv->GVS[sv->StdView].currentQuat, 
                              sv->GVS[sv->StdView].currentQuat);
               } else {
                  SUMA_LH("Not ready for event flavor");
                  break;
               }
               ii = SUMA_WhichSV(sv, SUMAg_SVv, SUMAg_N_SVv);
               if (ii < 0) {
                  fprintf (SUMA_STDERR,
                           "Error %s: Failed to find index of sv.\n", FuncName);
                  break;
               }
               if (!SUMAg_CF->ViewLocked[ii]) { /* No locking, 
                                                just redisplay current viewer */
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
                     if (  it != ii && 
                           SUMAg_CF->ViewLocked[it] && ed_svi == ed_sv) {
                        /* copy quaternions */
                        svi->GVS[svi->StdView].spinBeginX = 
                           sv->GVS[sv->StdView].spinBeginX;
                        svi->GVS[svi->StdView].spinBeginY = 
                           sv->GVS[sv->StdView].spinBeginY;
                        SUMA_COPY_VEC( sv->GVS[sv->StdView].deltaQuat, 
                                       svi->GVS[svi->StdView].deltaQuat, 
                                       4, float, float);
                        SUMA_COPY_VEC( sv->GVS[sv->StdView].currentQuat, 
                                       svi->GVS[svi->StdView].currentQuat, 
                                       4, float, float);
                       
                        /* add a redisplay now */
                        ED = SUMA_InitializeEngineListData (SE_RedisplayNow);
                        SUMA_RegisterEngineListCommand ( list, ED,
                                                SEF_Empty, NULL,
                                                SES_Suma, (void *)svi, NOPE,
                                                SEI_Head, NULL); 
                     }
                  }
                  if (!SUMA_Engine (&list)) {
                     fprintf (SUMA_STDERR, 
                              "Error %s: Failed calling SUMA_Engine.\n", 
                              FuncName);
                     break;
                  }
               }
            }

            break;
            
         case SUMA_Button_2_Motion:
            mevx = (float)Mev.x; mevy = (float)Mev.y;
            SUMA_LHv("In motion, Butt2 %f, %f\n", mevx , mevy);
            if (sv->ZoomCompensate) {
               zc_fac = sv->ZoomCompensate;
            }else {
               zc_fac = 1.0;
            }
            if ((Kev.state & ShiftMask)){
               
            } else {
               sv->GVS[sv->StdView].translateDeltaX =  
                  (mevx - sv->GVS[sv->StdView].translateBeginX) /
                  (float)sv->X->aWIDTH*sv->GVS[sv->StdView].TranslateGain;
               sv->GVS[sv->StdView].translateDeltaY = 
                  -(mevy - sv->GVS[sv->StdView].translateBeginY) /
                   (float)sv->X->aHEIGHT*sv->GVS[sv->StdView].TranslateGain;

               if (  sv->GVS[sv->StdView].translateDeltaX || 
                     sv->GVS[sv->StdView].translateDeltaY){
                  sv->GVS[sv->StdView].translateVec[0] += 
                     (GLfloat)sv->GVS[sv->StdView].translateDeltaX * zc_fac;
                  sv->GVS[sv->StdView].translateVec[1] += 
                     (GLfloat)sv->GVS[sv->StdView].translateDeltaY * zc_fac;
                  sv->GVS[sv->StdView].translateBeginX = mevx;
                  sv->GVS[sv->StdView].translateBeginY = mevy;
                  SUMA_postRedisplay(w, clientData, callData);
               }
            }  
            break;
         
         case SUMA_Button_3_Motion: {
            SUMA_ALL_DO *lado=NULL;
            SUMA_DO_Types lado_type=NOT_SET_type;
            
            if (sv->LastSel_ado_idcode_str) {
               lado = SUMA_whichADOg(sv->LastSel_ado_idcode_str);
               if (lado) lado_type = lado->do_type;
            }  
            
            SUMA_LH("In button 3 motion, lado=%s", ADO_LABEL(lado));
            
            /* Clear any pre-existing selection */
            if (!SUMA_Add_To_PickResult_List(sv, NULL, "TERSUM", NULL)) {
               SUMA_S_Err("Failed to clear selections");
               break;
            }

            if (SUMAg_CF->ROI_mode && SUMA_SV_Focus_SO(sv) && sv->BS) {
               /* ROI drawing mode */
               ii = SUMA_RegisteredSOs(sv, SUMAg_DOv, NULL);
               if (ii == 0) { /* no surfaces, break */
                  break;
               }


               if (!SUMA_GetSelectionLine(sv, 
                     (int)Mev.x, (int)Mev.y, sv->Pick0, sv->Pick1, 
                     0, NULL, NULL, NULL)) {
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
            } else {
               /* Mev.state does not work in the line below */
               if ((Kev.state & ShiftMask) && (Kev.state & ControlMask) ) {
                  SUMA_LH("Allowing callbacks");
                  SUMAg_CF->HoldClickCallbacks = 0;
               } else {
                  SUMA_LH("Holding back callbacks");
                  SUMAg_CF->HoldClickCallbacks = 1;
               }

               if (SUMAg_N_SVv > 1) {
                  ii = SUMA_WhichViewerInMomentum (SUMAg_SVv, 
                                                   SUMAg_N_SVv, NULL);
                  if (ii >= 0) {
                     sprintf (s, "You cannot select or draw while viewers\n"
                                 "(like viewer %c) are in momentum mode.\n", 
                                 ii+65);
                     SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                           s, FuncName, SMT_Error, 
                                           SMA_LogAndPopup);
                     SUMA_RETURNe;
                  }
               }  
               
               if (!(Kev.state & ShiftMask) && (Kev.state & ControlMask) ) {
                  SUMA_LH("Yoking intensity to node selection");
                  SUMAg_CF->YokeIntToNode = 1;
               } else {
                  SUMA_LH("Holding back callbacks");
                  SUMAg_CF->YokeIntToNode = 0;
               }   
                  
                  #if 0
                  /* Try this if you are having OpenGLStateReset problems at
                     node selection time. It is inefficient, but helps point
                     to the problem. 
                     Look at recent updates to SE_Redisplay*All* for the more
                     appropriate fix */
                  SUMA_S_Note("Blunt fix:");
                  SUMA_OpenGLStateReset(SUMAg_DOv, SUMAg_N_DOv, sv);
                  SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
                  #endif
               
               if (!SUMA_GetSelectionLine (  sv, (int)Mev.x, (int)Mev.y, 
                                             sv->Pick0, sv->Pick1, 0, 
                                             NULL, NULL, NULL)) {
                  fprintf (SUMA_STDERR, 
                           "Error %s: Failed in SUMA_GetSelectionLine.\n", 
                           FuncName);
                  break;
               } 

                  if (lado_type == NOT_SET_type ||
                      lado_type == MASK_type) {
               if (!MASK_MANIP_MODE(sv)) { /* You don't want these if 
                                              you're moving them! */
                  SUMA_LH("Mask picking");
                  hit = SUMA_ComputeLineMaskIntersect (sv, SUMAg_DOv, 0, NULL);
                  if (hit < 0) {
                     SUMA_S_Err("Failed in SUMA_ComputeLineMaskIntersect.");
                  }
               }
                  }
               
                  if (lado_type == NOT_SET_type ||
                      lado_type == TRACT_type || lado_type == GRAPH_LINK_type) {
               if (1) {
                  SUMA_LH("Tract picking");
                  hit = SUMA_ComputeLineDOsIntersect (sv, SUMAg_DOv, 0, NULL);
                  if (hit < 0) {
                     SUMA_S_Err("Failed in SUMA_ComputeLineDOsIntersect.");
                  }
               }
                  }
               
                  if (lado_type == NOT_SET_type ||
                      lado_type == VO_type) {
               if (1) {
                   SUMA_LH("Trying for volume intersections");
                   hit =  SUMA_ComputeLineVOslicesIntersect(sv, SUMAg_DOv, 
                                                            0, NULL);
                   if (hit < 0) {
                      fprintf( SUMA_STDERR,
                         "Error %s: "
                         "Failed in SUMA_MarkLineVOslicesIntersect.\n",
                               FuncName);
                   }
               }
               if (1) {
                   SUMA_LH("Trying for volume rendering intersections");
                   hit =  SUMA_ComputeLineVOvrIntersect(sv, SUMAg_DOv, 
                                                            0, NULL);
                   if (hit < 0) {
                      fprintf( SUMA_STDERR,
                         "Error %s: "
                         "Failed in SUMA_MarkLineVOvrIntersect.\n",
                               FuncName);
                   }
               }
                  }
               
                  if ((lado_type == NOT_SET_type ||
                       lado_type == SO_type)) {
               ii = SUMA_RegisteredSOs(sv, SUMAg_DOv, NULL);
               if (ii > 0) { /* some surfaces, try */
                  /* perform the intersection calcluation and mark the surface */
                  hit = SUMA_ComputeLineSurfaceIntersect (sv, SUMAg_DOv, 
                                                          1, NULL);
                  if (hit < 0) {
                     fprintf( SUMA_STDERR,
                              "Error %s: "
                              "Failed in SUMA_ComputeLineSurfaceIntersect.\n",
                              FuncName);
                     break;
                  }
               }
                  }

               ASSESS_MOTION:
               SUMA_LH("Assessment");
               if (dlist_size(sv->SelAdo)) {
                  if (!SUMA_Process_Selected_ADO(sv, SUMA_ALTHELL)) {
                     SUMA_S_Err("Failed to process selected ado");
                     SUMA_RETURNe;
                  }

                  /* redisplay */
                  sv->ResetGLStateVariables = YUP;
                  SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);            
               }               
               OUT_MOTION:
               /* reset hold on xforms */
               SUMAg_CF->HoldClickCallbacks = 0;
            }
                        
            break; }
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
      SUMA_S_Err("Failed in macro SUMA_ANY_WIDGET2SV.");
      SUMA_RETURNe;
   }

   
   ReDisp = 0;
   if ( ((sv->GVS[sv->StdView].spinDeltaX*sv->GVS[sv->StdView].spinDeltaX) > 
                                          sv->GVS[sv->StdView].MinIdleDelta ) ||
        ((sv->GVS[sv->StdView].spinDeltaY*sv->GVS[sv->StdView].spinDeltaY) >
                                          sv->GVS[sv->StdView].MinIdleDelta ) ) 
      { /* rotate if SUMA_momentum is enabled and spinDeltaX or spinDeltaY 
           are larger than the minimum set */ 
         /*fprintf(stdout,"SUMA_momentum:  spinDeltaX %f spinDeltaY %f\n",  
                  sv->GVS[sv->StdView].spinDeltaX, 
                  sv->GVS[sv->StdView].spinDeltaY);*/
         add_quats ( sv->GVS[sv->StdView].deltaQuat, 
                     sv->GVS[sv->StdView].currentQuat, 
                     sv->GVS[sv->StdView].currentQuat);
         ReDisp = 1;
      }
   if ( ((sv->GVS[sv->StdView].translateDeltaX*
          sv->GVS[sv->StdView].translateDeltaX) >
                                          sv->GVS[sv->StdView].MinIdleDelta ) ||
        ((sv->GVS[sv->StdView].translateDeltaY*
          sv->GVS[sv->StdView].translateDeltaY) >
                                          sv->GVS[sv->StdView].MinIdleDelta ) )
      { /* translate */
         sv->GVS[sv->StdView].translateVec[0] += 
                        (GLfloat)sv->GVS[sv->StdView].translateDeltaX;
         sv->GVS[sv->StdView].translateVec[1] += 
                        (GLfloat)sv->GVS[sv->StdView].translateDeltaY;
         ReDisp = 1;
      }
   if (ReDisp) {
      /*fprintf(stdout,"Momentum Redisplay\n");*/
      SUMA_postRedisplay(w, NULL, NULL);
   }
    sv->X->MOMENTUMID = XtAppAddTimeOut(SUMAg_CF->X->App, 1, 
                                          SUMA_momentum, (XtPointer) w); 

  SUMA_RETURNe;         
}
      
/* 
   Show the buffer used to pick displayable objects (DOs) with color id 
   The function adds a cross hair (4 white pixels) around the selection
   point and displays the image in AFNI's viewer (InViewer == 1) and/or
   save the image to an image file if OnDisk is not NULL. 
   Set OnDisk to something like PickBuff.ppm so that you get the exact
   pixels rendered in the output. Consecutive images are numbered with
   a time stamp.
    
   Note that the 4th chanel (alpha) is not shown or written to disk at 
   the moment.
*/
SUMA_Boolean SUMA_MarkPickInBuffer4(SUMA_SurfaceViewer *sv, int InViewer, 
                                    char *OnDisk)
{
   static char FuncName[]={"SUMA_MarkPickInBuffer4"};
   int n4, n3, n, p0[5],p1[5],p2[5],p3[5], i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!InViewer && !OnDisk) {
      SUMA_S_Err("Nothing to do here");
      SUMA_RETURN(NOPE);
   }
   if (!sv->pickrenpix4) {
      SUMA_S_Err("Empty buffer array");
      SUMA_RETURN(NOPE);
   }
   p0[0] = -1; /* to hold index of pixel being marked */
   p1[0] = -1;
   p2[0] = -1;
   p3[0] = -1;
            
   n4 = sv->PickPix[1]*sv->X->aWIDTH + sv->PickPix[0];
   n4 = 4*n4;
   SUMA_LHv("User pixel selection from whole buffer:"
                  " at %d %d is: %d %d %d %d\n", 
                  sv->PickPix[0], sv->PickPix[1],
                  sv->pickrenpix4[n4] , sv->pickrenpix4[n4+1],
                  sv->pickrenpix4[n4+2] , sv->pickrenpix4[n4+3]);
   if (sv->PickPix[1] > 0) {
      n4 = (sv->PickPix[1]-1)*sv->X->aWIDTH + sv->PickPix[0];
      n4 = 4*n4;
      p0[0] = n4; p0[1] = sv->pickrenpix4[n4  ]; p0[2] = sv->pickrenpix4[n4+1]; 
                  p0[3] = sv->pickrenpix4[n4+2]; p0[4] = sv->pickrenpix4[n4+3];
      sv->pickrenpix4[n4] = sv->pickrenpix4[n4+1] =
         sv->pickrenpix4[n4+2] = sv->pickrenpix4[n4+3] = 255;    
   }
   if (sv->PickPix[0] > 0) {
      n4 = sv->PickPix[1]*sv->X->aWIDTH + sv->PickPix[0]-1;
      n4 = 4*n4;
      p1[0] = n4; p1[1] = sv->pickrenpix4[n4  ]; p1[2] = sv->pickrenpix4[n4+1]; 
                  p1[3] = sv->pickrenpix4[n4+2]; p1[4] = sv->pickrenpix4[n4+3];
      sv->pickrenpix4[n4] = sv->pickrenpix4[n4+1] =
         sv->pickrenpix4[n4+2] = sv->pickrenpix4[n4+3] = 255;    
   }
   if (sv->PickPix[1] < (sv->X->aHEIGHT-1)) {
      n4 = (sv->PickPix[1]+1)*sv->X->aWIDTH + sv->PickPix[0];
      n4 = 4*n4;
      p2[0] = n4; p2[1] = sv->pickrenpix4[n4  ]; p2[2] = sv->pickrenpix4[n4+1]; 
                  p2[3] = sv->pickrenpix4[n4+2]; p2[4] = sv->pickrenpix4[n4+3];
      sv->pickrenpix4[n4] = sv->pickrenpix4[n4+1] =
         sv->pickrenpix4[n4+2] = sv->pickrenpix4[n4+3] = 255;    
   }
   if (sv->PickPix[0] < (sv->X->aWIDTH-1)) {
      n4 = sv->PickPix[1]*sv->X->aWIDTH + sv->PickPix[0]+1;
      n4 = 4*n4;
      p3[0] = n4; p3[1] = sv->pickrenpix4[n4  ]; p3[2] = sv->pickrenpix4[n4+1]; 
                  p3[3] = sv->pickrenpix4[n4+2]; p3[4] = sv->pickrenpix4[n4+3];
      sv->pickrenpix4[n4] = sv->pickrenpix4[n4+1] =
         sv->pickrenpix4[n4+2] = sv->pickrenpix4[n4+3] = 255;    
   }
   if (InViewer) { /* show me the money in the interactive viewer */
      GLubyte *pp3 = (GLubyte *)SUMA_calloc(sv->X->aWIDTH*sv->X->aHEIGHT*3, 
                                            sizeof(GLubyte));
      for (n3=0,n=0; n<sv->X->aWIDTH*sv->X->aHEIGHT; ++n) {
         n4=4*n;
         pp3[n3++]= sv->pickrenpix4[n4++];
         pp3[n3++]= sv->pickrenpix4[n4++];
         pp3[n3++]= sv->pickrenpix4[n4++]; n4++;
      }
      ISQ_snapsave(sv->X->aWIDTH, -sv->X->aHEIGHT,
                    (unsigned char *)pp3, sv->X->GLXAREA );
      SUMA_ifree(pp3);
   }
   if (OnDisk) {
      if (!SUMA_PixelsToDisk(sv, sv->X->aWIDTH, -sv->X->aHEIGHT,
                          (GLvoid *)sv->pickrenpix4, 4, 1, OnDisk, 1, 0)) {
         SUMA_S_Err("Failed to write pix to disk");
      }
   }
   /* now put things back where you found them */
   if (p0[0] >= 0) {
      for (i=0; i<4; ++i) sv->pickrenpix4[p0[0]+i] = p0[1+i];
   }
   if (p1[0] >= 0) {
      for (i=0; i<4; ++i) sv->pickrenpix4[p1[0]+i] = p1[1+i];
   }
   if (p2[0] >= 0) {
      for (i=0; i<4; ++i) sv->pickrenpix4[p2[0]+i] = p2[1+i];
   }
   if (p3[0] >= 0) {
      for (i=0; i<4; ++i) sv->pickrenpix4[p3[0]+i] = p3[1+i];
   }
   
   SUMA_RETURN(YUP);
}

/* 
   Function to return the color in the stored rendering buffer.
   checking is done at the pick location first, then in progressively
   larger neighborhoods 
   
   When first called, the search begins at pixel  *i, *j. If nothing
   is found, the search proceeds within the layer limit and the first
   non-blank find is returned in pixhit. *i and *j are set to the newly
   picked location
   
   i is along the width, j along the height
*/
SUMA_Boolean SUMA_GetColidInPickBuffer4(GLubyte *pix, int Ni, int Nj,
                                        int *ii, int *ji, 
                                        int maxlay, GLubyte *colid)
{
   static char FuncName[]={"SUMA_GetColidInPickBuffer4"};
   int i0, j0, i, j, n4, k;
   int poff[(1+2*2)*(1+2*2)][2] = { 
                      {0,0}, {-1,0}, {-1,-1}, {0,-1}, {-1,-1}, {1,1},/*A..F*/
                      {-2,0}, {-2,1}, {-2,-2}, {-1,-2}, {0,-2}, {-2,1},/*G..L*/
                      {1,-1}, {0,1}, {1,0}, {-2,2}, {1,-2}, {-1,2},/*M..R*/
                      {2,-2}, {2,-1},{0,2}, {2,0},{1,2},{2,1},{2,2} }/*S..Y*/;
   
   SUMA_ENTRY;
   
   if (!pix || !ii || !ji || *ii <0 || *ii >= Ni || *ji<0 || *ji>Nj) {
      SUMA_S_Err("Bad input");
      SUMA_RETURN(NOPE);
   }
   if (maxlay < 0) maxlay = 0;
   if (maxlay > 2) {
      SUMA_S_Warn("Not ready for more than two layers");
      maxlay = 2;
   }
   
   i = *ii; j = *ji;
   n4 = 4*(i+j*Ni);
   if (pix[n4] || pix[n4+1] || pix[n4+2] ||  pix[n4+3]) {
      memcpy(colid, pix+n4, 4*sizeof(GLubyte));
      SUMA_RETURN(YUP);
   }
   
   if (maxlay == 0) SUMA_RETURN(NOPE);
   
   /* search in order shown on page 235 of labbook NIH-6
      The idea is to follow a search pattern based on the shape
      of the cursor pointing up and slightly left 
      Actually maxlay == 1 is not quite honored here */
   i0 = *ii; j0 = *ji;
   k=1;
   while (k<25) {
      if ((i=i0+poff[k][0]) >=0 && i<Ni &&
          (j=j0+poff[k][0]) >=0 && j<Nj) {
         n4 = 4*(i+j*Ni);
         if (pix[n4] || pix[n4+1] || pix[n4+2] ||  pix[n4+3]) {
            memcpy(colid, pix+n4, 4*sizeof(GLubyte));
            *ii=i; *ji=j;
            SUMA_RETURN(YUP);
         }    
      }
      ++k; 
   }
   /* if nothing is found, and users want more layers, start searching in
      squarish patterns from layer 3 onwards, someday */
      
   SUMA_RETURN(NOPE);
}

/* if action == 0: Free saved DO picking buffer in pickrenpix4
                1: Recreate pickrenpix4 regardless of whether or
                   not the new pickrenpix4 is expected to change
                2: Recreate pickrenpix4 only if deemed necessary
*/
SUMA_Boolean SUMA_PickBuffer(SUMA_SurfaceViewer *sv, int action, SUMA_DO *dov) 
{
   static char FuncName[]={"SUMA_PickBuffer"};
   int Flush = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv) {
      SUMA_S_Err("Null sv!");
      SUMA_RETURN(NOPE);
   }
   
   if ( action == 0 || /* flush only */
        action == 1 /* Recreate regardless */ ) { 
         /* flush needed */
         SUMA_LH("Flushing pickrenpix4");
         Flush = 1;
   } else if (action == 2) { /* recreate if needed */
      if (  MASK_MANIP_MODE(sv) || 
            SUMA_DiffGeomViewStruct(sv->GVS[sv->StdView], 
                                  sv->GVS_last_PickMode[sv->StdView], 1) ||
            sv->FOV[sv->iState] != sv->FOV_last_PickMode[sv->iState]) {
         SUMA_LH("Have a changed GVS or FOV, flushing pickrenpix4");
         Flush = 1;
      } else {
         SUMA_LH("GVS and FOV still the same");
      }     
   } else {
      SUMA_S_Errv("Bad action value %d\n", action);
      SUMA_RETURN(NOPE);
   }

   if (Flush) {
      SUMA_LHv("Flushing, action %d\n", action);
      if (sv->pickrenpix4) SUMA_free(sv->pickrenpix4);
      sv->pickrenpix4 = NULL;
   }

   if (action == 0) SUMA_RETURN(YUP);

   /* recreate if no pickrenpix4 */
   if (!sv->pickrenpix4) {
      /* record GVS and FOV states*/
      SUMA_CopyGeomViewStruct(&(sv->GVS[sv->StdView]),
                              &(sv->GVS_last_PickMode[sv->StdView]));
      sv->FOV_last_PickMode[sv->iState] = sv->FOV[sv->iState];
      
      sv->DO_PickMode = 1;
      SUMA_display(sv, dov);
      if (!(sv->pickrenpix4 = 
               SUMA_grabPixels(4, sv->X->aWIDTH, sv->X->aHEIGHT))) {
         SUMA_S_Err("Failed to grab pixels");
      }
      sv->DO_PickMode = 0;
   }
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_ADO_Flush_Pick_Buffer(SUMA_ALL_DO *ado, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_ADO_Flush_Pick_Buffer"};
   int ii;
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURN(NOPE);
   if (sv) {
      if (SUMA_ADO_isRegistered(sv, ado)) {
         SUMA_PickBuffer(sv, 0, NULL);
      }
   } else { /* Do it for all */
      for (ii=0; ii<SUMAg_N_SVv; ++ii) {
         sv = &(SUMAg_SVv[ii]);
         if (SUMA_ADO_isRegistered(sv, ado)) {
            SUMA_PickBuffer(sv, 0, NULL);
         }
      }
   }
   
   SUMA_RETURN(YUP);
}

/* What was picked on a frame SO ?
   See SUMA_Surface_Of_NIDO_Matrix() for how surface coordinates
   translate to matrix pixels and eventually matrix cells.
   See also SUMA_GDSET_edgeij_to_GMATRIX_XYZ() and SUMA_DrawGraphDO_GMATRIX()
*/
SUMA_PICK_RESULT *SUMA_WhatWasPicked_FrameSO(SUMA_SurfaceViewer *sv, int ido)
{
   static char FuncName[]={"SUMA_WhatWasPicked_FrameSO"};
   float P0[3], P1[3];
   int N[3], IIm[3], ii, jj, i, *ui=NULL, *uj=NULL, si=-1, G[3], B[3], M[3];
   SUMA_ALL_DO *ado=NULL;
   SUMA_DSET *dset=NULL;
   SUMA_PICK_RESULT *PR = NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_GRAPH_SAUX *GSaux = NULL;
   SUMA_MT_INTERSECT_TRIANGLE *MTI = NULL;
   double Aff[4][4], I[3], V[12], X[3], GB[3];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv ) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(PR);
   }
   
   if (!iDO_isGLDO(ido) || !iDO_is_variant(ido,"GMATRIX")) { 
      /* No frame SOs in this DO, not an error just keep looking */
      SUMA_LH("Not a DO with a FrameSO, go on");
      SUMA_RETURN(PR);
   }
   
   SUMA_LHv("Pick Query for FrameSO on sv %p, ido %d\n", sv, ido);
   if (iDO_is_variant(ido,"GMATRIX") && 
       (GSaux = iDO_GSaux(ido))) {
       SO=GSaux->FrameSO; 
   }
   if (!SO) {
      SUMA_S_Errv("No FrameSO on %s\n", iDO_label(ido));
      SUMA_RETURN(PR);
   }
   NI_GET_DOUBLEv(GSaux->nido->ngr, "dicom_real_to_ijk", V, 12, LocalHead);
   if (!NI_GOT) {
      SUMA_S_Err("No dicom_real_to_ijk");
      SUMA_RETURN(PR);
   }   
   V12_TO_AFF44(Aff, V);
   
   NI_GET_INTv(GSaux->nido->ngr, "Nijk", N, 3, LocalHead);
   if (!NI_GOT) {
      SUMA_S_Err("No Nijk");
      SUMA_RETURN(PR);
   } 
   
   /* total num of pixels per value */
   NI_GET_INTv(GSaux->nido->ngr, "PixCount", M, 3, LocalHead);
   NI_GET_INTv(GSaux->nido->ngr, "PixPerVal", G, 3, LocalHead);
   NI_GET_INTv(GSaux->nido->ngr, "BorWid", B, 3, LocalHead);
   GB[0] = G[0]+B[0];
   GB[1] = G[1]+B[1];
   GB[2] = G[2]+B[2];
   
   SUMA_COPY_VEC(sv->Pick0, P0, 3, GLdouble, float);
   SUMA_COPY_VEC(sv->Pick1, P1, 3, GLdouble, float);
   if (!(MTI = SUMA_MT_intersect_triangle(P0, P1,
                           SO->NodeList, SO->N_Node, 
                           SO->FaceSetList, SO->N_FaceSet, NULL, 0))){
      SUMA_S_Err("SUMA_MT_intersect_triangle failed.");
      SUMA_RETURN(PR);
   }
         
   if (MTI->N_hits < 1) { 
      SUMA_LH("No hits");
      SUMA_RETURN(PR);
   }
   
   ui = uj = NULL;
   if (iDO_isGLDO(ido)) {
      if (!(dset = SUMA_find_GLDO_Dset((SUMA_GraphLinkDO*)SUMAg_DOv[ido].OP))) {
         SUMA_S_Err("No dset for GLDO?");
         SUMA_RETURN(PR);
      }
      GSaux->IgnoreSelection = 0; /* Selection being made on matrix 
                                     representation of graph.
                                     turn off IgnoreSelection */
      switch (dset->Aux->matrix_shape) {
         case MAT_FULL:
         case MAT_TRI:
         case MAT_TRI_DIAG:
            SUMA_LH("Direct indexing between edge points and matrix row/col");
            break;
         case MAT_SPARSE:
            if (!dset->inel) {
               SUMA_S_Err("Don't have inel, badly shaped dataset");
               SUMA_RETURN(PR);
            }
            if (  !(ui = SUMA_GetUniqueIndicesVec(dset,1)) || 
                  !(uj = SUMA_GetUniqueIndicesVec(dset,2))    ) {
               SUMA_S_Err("Failed to get unique indices");
               SUMA_RETURN(PR);
            }
            break;
         default:
            SUMA_S_Err("Rats");
            SUMA_RETURN(PR);
            break;
      }
   }


   PR = SUMA_New_Pick_Result(PR);
   PR->ado_idcode_str = SUMA_replace_string(PR->ado_idcode_str,iDO_idcode(ido));
   ado = SUMA_whichADOg(PR->ado_idcode_str);
   PR->primitive = SUMA_replace_string(PR->primitive, "none yet");
   PR->primitive_index = -1;
   PR->PickXYZ[0] = MTI->P[0];
   PR->PickXYZ[1] = MTI->P[1];
   PR->PickXYZ[2] = MTI->P[2];
   sv->Focus_DO_ID = ido;

   PR->datum_index = -1;
   for (i=0; i<SUMA_N_IALTSEL_TYPES; ++i) PR->iAltSel[i] = -1;
   for (i=0; i<SUMA_N_DALTSEL_TYPES; ++i) PR->dAltSel[i] = 0.0;
   AFF44_MULT_I(I, Aff, MTI->P);

   SUMA_LHv("Hit on Frame SO, triangle %d %f %f %f, M=[%d %d]\n"
            "     Pixel hit: %.2f %.2f %.2f\n", 
            MTI->ifacemin, MTI->P[0], MTI->P[1], MTI->P[2], M[0], M[1],
            I[0], I[1], I[2]);
            
   for (i=0; i<3; ++i) {
      /* I[i] = I[i]/GB[i]; Account for per value gain and 
                                    background thickness */
      /* Round I to get matrix pixel coordinates*/
      I[i] = SUMA_ROUND(I[i]);
      if (I[i] < 0) IIm[i] = -1;
      else if (I[i] >= GB[i]*N[i]) IIm[i] = -1;
      else IIm[i] = (int)(I[i]/GB[i]); /* Undo gain to get back to matrix grid */
   }
   
   if (ui) { /* sparse hell */
      if (IIm[0]>=0) ii = ui[IIm[0]]; 
      else ii = -1;
      if (IIm[1]>=0) jj = uj[IIm[1]];
      else jj = -1;
   } else {
      ii = IIm[0]; jj=IIm[1];
   }
   
   SUMA_LHv("In face %d X=[%f %f %f]dicom, GB=[%d %d] \n"
            "     If=[%f %f %f], Im=[%d %d %d], ii,jj=[%d %d]\n", 
            MTI->ifacemin, MTI->P[0], MTI->P[1], MTI->P[2], 
            (int)GB[0], (int)GB[1],
            I[0], I[1], I[2], IIm[0], IIm[1], IIm[2], ii, jj);

   switch (MTI->ifacemin) {
      case 0:
      case 1:
         /* in matrix */
         PR->primitive = SUMA_replace_string(PR->primitive, "segments");
         if (!ui && ii >= 0 && jj >= 0) {
            PR->datum_index = PR->primitive_index = ii+jj*N[0];
         } else { /* sparse */
            if (ii < 0 || jj < 0) {
               PR->datum_index = PR->primitive_index = -1;
            } else if (!SUMA_GDSET_PointsToSegIndex(dset, ii, jj, &si)) {
               SUMA_S_Errv("Failed to find segment for %d %d\n", ii, jj);
               if (LocalHead) SUMA_DUMP_TRACE("Now what?");
               PR->datum_index = PR->primitive_index = -1;
            } else {
               PR->datum_index = PR->primitive_index = si;
            }
         }  
         if (MTI->ifacemin == 1) {
            PR->iAltSel[SUMA_ENODE_0] = ii;
            PR->iAltSel[SUMA_ENODE_1] = jj;
         } else {
            PR->iAltSel[SUMA_ENODE_0] = jj;
            PR->iAltSel[SUMA_ENODE_1] = ii;
         }
         break;
      case 2:
      case 3:
         /* top edge*/
         PR->primitive = SUMA_replace_string(PR->primitive, "balls");
         PR->iAltSel[SUMA_ENODE_0] = jj;
         PR->iAltSel[SUMA_ENODE_1] = -1;
         break;
      case 4:
      case 5:
         /* right edge*/
         PR->primitive = SUMA_replace_string(PR->primitive, "balls");
         PR->iAltSel[SUMA_ENODE_0] = ii;
         PR->iAltSel[SUMA_ENODE_1] = -1;
         break;
      case 6:
      case 7:
         /* bottom edge*/
         PR->primitive = SUMA_replace_string(PR->primitive, "balls");
         PR->iAltSel[SUMA_ENODE_0] = jj;
         PR->iAltSel[SUMA_ENODE_1] = -1;
         break;
      case 8:
      case 9:
         /* left edge*/
         PR->primitive = SUMA_replace_string(PR->primitive, "balls");
         PR->iAltSel[SUMA_ENODE_0] = ii;
         PR->iAltSel[SUMA_ENODE_1] = -1;
         break;
      default:
         SUMA_S_Errv("Faceset %d???\n", MTI->ifacemin);
         SUMA_RETURN(PR);
   }
   
   MTI = SUMA_Free_MT_intersect_triangle(MTI); 
   
   SUMA_RETURN(PR);
}

/*!
   \brief find the closest location on a bundle to a point
   on the screen.
   
   p (void *) One bundle
   ptype (char): Type of bundle. 'N' --> p is a NI_element *
                                 'B' --> p is a TAYLOR_BUNDLE *
   Tmask (int): if not < 0 then only consider intersections in
                tract Tmask of the bundle.
   sv (SUMA_SurfaceViewer *) The viewer
   scpx (float *) The pixel location in screen coordinates.
                  If NULL, form it from sv->PickPix;
   crude (int) if (0) then use crude search, return closest point
                      not bothering with where along the segment
                      between the two closest points you are
                  1   a fine search, locating where between
                      the two closest points one clicked
   tmin (int *) Will contain the index of the tract that was
                hit within the bundle (-1 for no cigar)
   pmin (int *) Will contain the index of the closest point
                to the hit in tract tmin
   fmin (float *) Will contain the fraction between pmin and
                  pmin+1 where intersection occurred
   mindist (float *) Distance from closest location on tract
   \ret YUP all good, NOPE nothing found or bad input
*/
   
SUMA_Boolean SUMA_Bundle_Pick_Intersect(void *p, char ptype, int Tmask,
                     SUMA_SurfaceViewer *sv, float *scpxu, int crude,
                     int *tmin, int *pmin, float *frmin, float *mindistu)
{
   static char FuncName[]={"SUMA_Bundle_Pick_Intersect"};
   int nn, nnmin, mmmin, mm, nn_max, nn_min;
   float *scrxyz = NULL, dx, dy, dxy2=0.0, fmin, scpx[3];
   float  mindist2, mindist, *A, *B;
   double P[3], f = 0.0;
   NI_element *nelitp=NULL;
   TAYLOR_BUNDLE *tb = NULL;
   TAYLOR_TRACT *ttn=NULL;

   SUMA_ENTRY;
   
   if (tmin) *tmin = -1;
   if (pmin) *pmin = -1;
   if (frmin) *frmin = -1.0;
   if (mindistu) *mindistu = -1.0;
   
   if (!p || !sv) SUMA_RETURN(NOPE);
   
   nn_min = 0;
   if (ptype == 'N') { /* NI_element */
      nelitp = (NI_element *)p;
      nn_max = nelitp->vec_len;
   } else if (ptype == 'B') { /* Taylor Bundle */
      tb = (TAYLOR_BUNDLE *)p;
      if (Tmask < 0) {
         nn_max = tb->N_tracts;
      } else {
         if (Tmask < tb->N_tracts) {
            nn_min = Tmask;
            nn_max = nn_min+1;
         } else {
            SUMA_S_Err("Tmask (%d) exceeds number of tracts (%d) in bundle", 
                       Tmask, tb->N_tracts);
            SUMA_RETURN(NOPE);
         }
      }
   } else {
      SUMA_RETURN(NOPE);
   }
   
   nnmin=-1; mmmin=-1; mindist=mindist2=1000; fmin=1.0;
   if (scpxu) {
      scpx[0] = scpxu[0]; scpx[1] = scpxu[1]; scpx[2] = scpxu[2];
   } else {
      GLint viewport[4];
      glGetIntegerv(GL_VIEWPORT, viewport);
      /* screen pick coordinate in screen coords */
      scpx[0] = sv->PickPix[0]; 
      scpx[1] = viewport[3]-sv->PickPix[1]-1;
      scpx[2] = 0.0;
   }
   
   for (nn=nn_min; nn<nn_max; ++nn) {
      if (nelitp) {
         ttn = (TAYLOR_TRACT *)(nelitp->vec[0])+nn;
      } else {
         ttn = tb->tracts+nn; 
      }
      scrxyz = (float *)SUMA_calloc(ttn->N_pts3, sizeof(float));
      memcpy(scrxyz, ttn->pts, (ttn->N_pts3)*sizeof(float));
      /* tranform bundle points to screen space*/
      if (!SUMA_World2ScreenCoordsF(sv, ttn->N_pts3/3, 
                                   ttn->pts, scrxyz, NULL, 
                                   YUP, YUP)) {
         SUMA_S_Err("Failed to get screen coords");
         SUMA_RETURN(NOPE);
      }
      /* Now search for the closest point of bundle to the click 
         The depth is ignored, in the hope that a simple closest
         search is enough. Otherwise we need to add a heuristic
         for the cost of depth */
      if (crude) {
            /* For finer tracing, you should project scpx onto the 
            segment between the 1st and 2 points forming a segment,
            much like what is done in SUMA_PROJECT_C_ONTO_AB below 
            The finer tracing is needed for crass paths. However  
            for real bundles, on high-res data this might be 
            overkill */
         mm=0;                                                       
         while (mm < ttn->N_pts3) {
            if ( ((dx = SUMA_ABS(scrxyz[mm  ]-scpx[0])) < mindist) &&
                 ((dy = SUMA_ABS(scrxyz[mm+1]-scpx[1])) < mindist) ){                            if ((dxy2 = dx*dx+dy*dy) < mindist2) {
                  mindist2 = dxy2; 
                  mindist  = sqrtf(mindist2);
                  nnmin=nn; mmmin=mm/3;  fmin = 0.0;
               }
            }
            mm += 3;
         }
      } else {
         mm=0;
         while (mm < ttn->N_pts3-3) {
            A = scrxyz+mm;
            B = scrxyz+mm+3;
            SUMA_PROJECT_C_ONTO_AB(scpx, A, B, P, f);
            if ( (f > -0.2 && f < 1.2) &&
                 ((dx = SUMA_ABS(P[0]-scpx[0])) < mindist) &&
                 ((dy = SUMA_ABS(P[1]-scpx[1])) < mindist) ){                                    if ((dxy2 = dx*dx+dy*dy) < mindist2) {
                  mindist2 = dxy2; 
                  mindist  = sqrtf(mindist2);
                  nnmin=nn; mmmin=mm/3; fmin=f;
               }
            }
            mm += 3;
         }                     
      }
      SUMA_ifree(scrxyz);
   }

   if (tmin) *tmin=nnmin;
   if (pmin) *pmin=mmmin;
   if (frmin) *frmin=fmin;
   if (mindistu) *mindistu = mindist;
   
   SUMA_RETURN(YUP);
}

/* Find the object that was picked, pointer copy to found object is 
   returned in ucodf and should not be freed by calling function*/
SUMA_PICK_RESULT *SUMA_WhatWasPicked(SUMA_SurfaceViewer *sv, GLubyte *colid, 
                                SUMA_COLID_OFFSET_DATUM **ucodf, 
                                int ipick, int jpick,
                                SUMA_PICK_RESULT *PR)
{
   static char FuncName[]={"SUMA_WhatWasPicked"};
   DListElmt *el=NULL;
   SUMA_COLID_OFFSET_DATUM *cod=NULL, *codf=NULL;
   long int n4;
   int iii;
   double f = 0.0;
   SUMA_ALL_DO *ado=NULL;
   SUMA_DUMB_DO DDO;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (ucodf) *ucodf=codf;
   PR = SUMA_New_Pick_Result(PR);
   if (!sv || !colid) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(PR);
   }
   if (!sv->pick_colid_list || !dlist_size(sv->pick_colid_list)) {
      SUMA_S_Err("NULL or Empty pick_colid_list");
      SUMA_RETURN(PR);
   }
   if (LocalHead) SUMA_Show_Pick_Colid_List(sv->pick_colid_list, SUMA_STDERR);
   SUMA_COLID_RGBA2N(colid[0], colid[1], colid[2], colid[3], n4);
   do {
      if (!el) el = dlist_head(sv->pick_colid_list);
      else el = dlist_next(el);
      cod = (SUMA_COLID_OFFSET_DATUM *)el->data;
      if (n4>= cod->i0 && n4<= cod->i1) { /* we're in this object */
         codf = cod;
      }
   } while (!codf && el != dlist_tail(sv->pick_colid_list));
   
   if (!codf) SUMA_RETURN(PR);
   if (ucodf) *ucodf=codf;
   
   /* Compute get more info about intersection  */
   if (codf) {
      NI_element *nelitp=NULL;
      float *fv=NULL;
      double xyzw[9], scl[9], U[3], P[3], C[3], *dv=NULL;
      int i0, i1, ir, datum_index;
      SUMA_ALL_DO *ado=NULL;
      SUMA_DSET *dset=NULL;
      GLint viewport[4];
      SUMA_GRAPH_SAUX *GSaux = NULL;
      
      glGetIntegerv(GL_VIEWPORT, viewport);
      PR->ado_idcode_str = SUMA_replace_string(PR->ado_idcode_str,
                                               codf->ref_idcode_str);
      PR->primitive = SUMA_replace_string(PR->primitive, codf->primitive);
      PR->primitive_index = (n4-codf->i0);
      for (i0=0; i0<SUMA_N_IALTSEL_TYPES; ++i0) PR->iAltSel[i0] = -1;
      for (i0=0; i0<SUMA_N_DALTSEL_TYPES; ++i0) PR->dAltSel[i0] = 0.0;
      PR->datum_index = -1;
      ado = SUMA_whichADOg(PR->ado_idcode_str);
      switch (codf->ref_do_type) {
         case GRAPH_LINK_type: {
            dset = SUMA_find_GLDO_Dset((SUMA_GraphLinkDO*)ado);
            DDO.err = 1; SUMA_Load_Dumb_DO(ado, &DDO);
            if (DDO.err) {
               if (DDO.err==1) {
                  SUMA_SL_Err("Object's parent graph set not found.");
               } else if (DDO.err==2) {
                  SUMA_SL_Err("Could not fill DDO");
               } else {
                  SUMA_SL_Err("Weird error.");
               } 
               SUMA_RETURN (PR);
            }
            if (!DDO.NodeList) {
               SUMA_S_Err("SDO is node based but could not get a NodeList");
               SUMA_RETURN (PR);
            }
            if (!(GSaux = SDSET_GSAUX(dset))) {
               SUMA_S_Err("No GSaux");
               SUMA_RETURN(PR);
            }
            GSaux->IgnoreSelection = 0; /*reset ignore selection flag */
            if (SUMA_is_ADO_Datum_Primitive(ado, codf)) { 
               datum_index = SUMA_GDSET_EdgeRow_To_Index(dset, 
                                                         PR->primitive_index);
               if (!(SUMA_GDSET_SegIndexToPoints(dset, datum_index, 
                                                 &i0, &i1, NULL))) {
                  SUMA_RETURN(PR);
               }
               nelitp = NULL;
               if (GSaux->ShowBundles &&
                   (nelitp = SUMA_GDSET_Edge_Bundle(dset, GSaux, 
                                                    datum_index, -1))) {
               #if 1 /* more unified version, older, valid one below */
                  int nn=0, mm=0, nnmin=-1, mmmin=-1, mmmin3=-1;
                  float scpx[3], *A, *B;
                  float mindist, dist, distatmin, seglen;
                  float fmin;
                  TAYLOR_TRACT *ttn=NULL;
                  
                  SUMA_LHv("Clicked on a bundle representation of edge %d.\n",
                           datum_index);
                  /* screen pick coordinate in screen coords */
                  scpx[0] = sv->PickPix[0]; 
                  scpx[1] = viewport[3]-sv->PickPix[1]-1;
                  scpx[2] = 0.0;
                  /* find the closest point to where we clicked on the bundle */
                  if (!SUMA_Bundle_Pick_Intersect(nelitp, 'N', -1, sv, scpx, 0, 
                                                  &nnmin, &mmmin, 
                                                  &fmin, &mindist)) {
                     SUMA_LH("No bunlde intersection");
                  }
                  mmmin3 = 3*mmmin;
                  if (nnmin > -1) {
                     ttn = (TAYLOR_TRACT *)(nelitp->vec[0])+nnmin;
                     if (fmin != 0.0f) {
                        PR->PickXYZ[0]=ttn->pts[mmmin3+0]+
                                    fmin*(ttn->pts[mmmin3+3]-ttn->pts[mmmin3]);
                        PR->PickXYZ[1]=ttn->pts[mmmin3+1]+
                                    fmin*(ttn->pts[mmmin3+4]-ttn->pts[mmmin3+1]);
                        PR->PickXYZ[2]=ttn->pts[mmmin3+2]+
                                    fmin*(ttn->pts[mmmin3+5]-ttn->pts[mmmin3+2]);
                        /* where along the tract? */
                        mm=0; dist=0.0; distatmin=19999.9;
                        while (mm < ttn->N_pts3-3) {
                           A = ttn->pts+mm;
                           B = ttn->pts+mm+3;
                           seglen =  sqrtf((B[0]-A[0])*(B[0]-A[0])+
                                           (B[1]-A[1])*(B[1]-A[1])+
                                           (B[2]-A[2])*(B[2]-A[2]));
                           if (mm==mmmin3) {
                              distatmin = dist + fmin*seglen;
                           }
                           dist +=seglen;
                           mm += 3;
                        }
                        /* Which is the closest node? */
                        f = distatmin/dist;
                        if (f <= 0.5) {
                           PR->iAltSel[SUMA_ENODE_0] = i0;
                           PR->iAltSel[SUMA_ENODE_0] = i1;
                           if (SUMA_ABS(f)> 0.01) {/* not too close to edge */
                              PR->datum_index = datum_index;
                           } else PR->datum_index = -1;
                        } else {
                           SUMA_LHv("++half way, look for opp. edge [%d %d]\n",
                                    i1, i0);
                           PR->iAltSel[SUMA_ENODE_0] = i1;
                           PR->iAltSel[SUMA_ENODE_1] = i0;
                           
                           if (SUMA_ABS(1.0-f) > 0.01){/*not too close to edge*/
                              /* does edge [i1, i0] exist? If so take it*/
                              if (SUMA_GDSET_PointsToSegIndex(dset,i1,i0,&ir)) {
                                 SUMA_LHv("Switching primitve to edge %d\n", ir);
                                 PR->datum_index = ir;
                              } else { /* leave old hit, no opposite edge */
                                 PR->datum_index = datum_index;
                              }
                           } else PR->datum_index = -1;
                        }
                     } else { /* from the quick search, no fmin used*/
                        PR->PickXYZ[0]=ttn->pts[mmmin3+0];
                        PR->PickXYZ[1]=ttn->pts[mmmin3+1];
                        PR->PickXYZ[2]=ttn->pts[mmmin3+2];
                     }
                  } else {
                     PR->PickXYZ[0] = PR->PickXYZ[1] = PR->PickXYZ[2]=0.0;
                  }
                  SUMA_LHv("Closest distance of %f, tract %d, point %d, f=%f\n"
                           "at world [%f %f %f]\n",
                           mindist, nnmin, mmmin, fmin,
                           PR->PickXYZ[0], PR->PickXYZ[1], PR->PickXYZ[2]);
               #else
                  int nn=0, mm=0, nnmin=-1, mmmin=-1;
                  float *scrxyz = NULL, scpx[3], *A, *B;
                  float mindist, mindist2, dist, distatmin, seglen;
                  float dx, dy, dxy2=0.0, fmin;
                  TAYLOR_TRACT *ttn=NULL;
                  
                  SUMA_LHv("Clicked on a bundle representation of edge %d.\n",
                           datum_index);
                  /* screen pick coordinate in screen coords */
                  scpx[0] = sv->PickPix[0]; 
                  scpx[1] = viewport[3]-sv->PickPix[1]-1;
                  scpx[2] = 0.0;
                  /* find the closest point to where we clicked on the bundle */
                  nnmin=-1; mmmin=-1; mindist=mindist2=1000; fmin=1.0;
                  for (nn=0; nn<nelitp->vec_len; ++nn) {
                     ttn = (TAYLOR_TRACT *)(nelitp->vec[0])+nn;
                     scrxyz = (float *)SUMA_calloc(ttn->N_pts3, sizeof(float));
                     memcpy(scrxyz, ttn->pts, (ttn->N_pts3)*sizeof(float));
                     /* tranform bundle points to screen space*/
                     if (!SUMA_World2ScreenCoordsF(sv, ttn->N_pts3/3, 
                                                  ttn->pts, scrxyz, NULL, 
                                                  YUP, YUP)) {
                        SUMA_S_Err("Failed to get screen coords");
                        SUMA_RETURN(PR);
                     }
                     /* Now search for the closest point of bundle to the click 
                        The depth is ignored, in the hope that a simple closest
                        search is enough. Otherwise we need to add a heuristic
                        for the cost of depth */
                     #ifdef CRUDE_SEARCH
                        /* For finer tracing, you should project scpx onto the 
                        segment between the 1st and 2 points forming a segment,
                        much like what is done in SUMA_PROJECT_C_ONTO_AB below 
                        The finer tracing is needed for crass paths. However  
                        for real bundles, on high-res data this might be 
                        overkill */
                     mm=0;                                                       
                     while (mm < ttn->N_pts3) {
                        if ( ((dx = SUMA_ABS(scrxyz[mm  ]-scpx[0])) < mindist) &&
                             ((dy = SUMA_ABS(scrxyz[mm+1]-scpx[1])) < mindist) ){                            if ((dxy2 = dx*dx+dy*dy) < mindist2) {
                              mindist2 = dxy2; 
                              mindist  = sqrtf(mindist2);
                              nnmin=nn; mmmin=mm;  fmin = 0.0;
                           }
                        }
                        mm += 3;
                     }
                     #else
                     mm=0;
                     while (mm < ttn->N_pts3-3) {
                        A = scrxyz+mm;
                        B = scrxyz+mm+3;
                        SUMA_PROJECT_C_ONTO_AB(scpx, A, B, P, f);
                        if ( ((dx = SUMA_ABS(P[0]-scpx[0])) < mindist) &&
                             ((dy = SUMA_ABS(P[1]-scpx[1])) < mindist) ){                                    if ((dxy2 = dx*dx+dy*dy) < mindist2) {
                              mindist2 = dxy2; 
                              mindist  = sqrtf(mindist2);
                              nnmin=nn; mmmin=mm; fmin=f;
                           }
                        }
                        mm += 3;
                     }                     
                     #endif
                     SUMA_ifree(scrxyz);
                  }
                  if (nnmin > -1) {
                     ttn = (TAYLOR_TRACT *)(nelitp->vec[0])+nnmin;
                     if (fmin != 0.0f) {
                        PR->PickXYZ[0]=ttn->pts[mmmin+0]+
                                    fmin*(ttn->pts[mmmin+3]-ttn->pts[mmmin]);
                        PR->PickXYZ[1]=ttn->pts[mmmin+1]+
                                    fmin*(ttn->pts[mmmin+4]-ttn->pts[mmmin+1]);
                        PR->PickXYZ[2]=ttn->pts[mmmin+2]+
                                    fmin*(ttn->pts[mmmin+5]-ttn->pts[mmmin+2]);
                        /* where along the tract? */
                        mm=0; dist=0.0; distatmin=19999.9;
                        while (mm < ttn->N_pts3-3) {
                           A = ttn->pts+mm;
                           B = ttn->pts+mm+3;
                           seglen =  sqrtf((B[0]-A[0])*(B[0]-A[0])+
                                           (B[1]-A[1])*(B[1]-A[1])+
                                           (B[2]-A[2])*(B[2]-A[2]));
                           if (mm==mmmin) {
                              distatmin = dist + fmin*seglen;
                           }
                           dist +=seglen;
                           mm += 3;
                        }
                        /* Which is the closest node? */
                        f = distatmin/dist;
                        if (f <= 0.5) {
                           PR->iAltSel[SUMA_ENODE_0] = i0;
                           PR->iAltSel[SUMA_ENODE_1] = i1;
                           
                           if (SUMA_ABS(f)> 0.01) {/* not too close to edge */
                              PR->datum_index = datum_index;
                           } else PR->datum_index = -1;
                        } else {
                           SUMA_LHv("++half way, look for opp. edge [%d %d]\n",
                                    i1, i0);
                           PR->iAltSel[SUMA_ENODE_0] = i1;
                           PR->iAltSel[SUMA_ENODE_1] = i0;
                           if (SUMA_ABS(1.0-f) > 0.01){/*not too close to edge*/
                              /* does edge [i1, i0] exist? If so take it*/
                              if (SUMA_GDSET_PointsToSegIndex(dset,i1,i0,&ir)) {
                                 SUMA_LHv("Switching primitve to edge %d\n", ir);
                                 PR->datum_index = ir;
                              } else { /* leave old hit, no opposite edge */
                                 PR->datum_index = datum_index;
                              }
                           } else PR->datum_index = -1;
                        }
                     } else { /* from the quick search, no fmin used*/
                        PR->PickXYZ[0]=ttn->pts[mmmin+0];
                        PR->PickXYZ[1]=ttn->pts[mmmin+1];
                        PR->PickXYZ[2]=ttn->pts[mmmin+2];
                     }
                  } else {
                     PR->PickXYZ[0] = PR->PickXYZ[1] = PR->PickXYZ[2]=0.0;
                  }
                  SUMA_LHv("Closest distance of %f, tract %d, point %d, f=%f\n"
                           "at world [%f %f %f]\n",
                           mindist, nnmin, mmmin, fmin,
                           PR->PickXYZ[0], PR->PickXYZ[1], PR->PickXYZ[2]);
                  
               #endif
               } else {
                  SUMA_LHv("Segment %d is formed by points %d and %d\n",
                           datum_index, i0, i1);
                  fv = SUMA_GDSET_NodeXYZ(dset, i0, SUMA_ADO_variant(ado), NULL);
                     xyzw[0] = fv[0]; xyzw[1] = fv[1]; xyzw[2] = fv[2];
                  fv = SUMA_GDSET_NodeXYZ(dset, i1, SUMA_ADO_variant(ado), NULL);
                     xyzw[3] = fv[0]; xyzw[4] = fv[1]; xyzw[5] = fv[2];
                  xyzw[6] = (sv->Pick0[0]+sv->Pick1[0])/2.0;
                  xyzw[7] = (sv->Pick0[1]+sv->Pick1[1])/2.0;
                  xyzw[8] = (sv->Pick0[2]+sv->Pick1[2])/2.0;

                  if (!SUMA_World2ScreenCoords(sv, 3,xyzw,scl, NULL, YUP, YUP)) {
                     SUMA_S_Err("Failed to get screen coords");
                     SUMA_RETURN(PR);
                  }
                  /* Project click point onto line by two nodes */
                  C[0] = sv->PickPix[0]; 
                  C[1] = viewport[3]-sv->PickPix[1]-1; 
                  C[2] = 0; 
                  dv = scl+3; /* point B */
                  SUMA_PROJECT_C_ONTO_AB(C, scl, dv, P, f);
                  /* Project point click onto segment */
                  SUMA_LHv(
                     "User click locations: %d %d Norm(%f %f)\n"
                        "sv PickPix: %d %d (screen/mouse y:%d)\n"
                        "world: near[%f %f %f] far[%f %f %f]\n"
                        "Edge %d formed by nodes %d [%f %f %f], %d [%f %f %f]\n"
                        "Nodes projected to screen: [%f %f %f], [%f %f %f]\n"
                  "Projection of click point screen edge: [%f %f %f], f = %f\n"
                        ,ipick, jpick, 
                           ipick/(double)viewport[2], jpick/(double)viewport[3],
                        sv->PickPix[0], sv->PickPix[1], viewport[3]-jpick-1,
                           sv->Pick0[0], sv->Pick0[1], sv->Pick0[2],
                           sv->Pick1[0], sv->Pick1[1], sv->Pick1[2],
                        datum_index, i0, xyzw[0], xyzw[1], xyzw[2],
                                     i1, xyzw[3], xyzw[4], xyzw[5],
                           scl[0], scl[1], scl[2], scl[3], scl[4], scl[5],
                           P[0], P[1], P[2], f);

                  /* Record the intersection location */ 
                  PR->PickXYZ[0]=xyzw[0]+f*(xyzw[3]-xyzw[0]);
                  PR->PickXYZ[1]=xyzw[1]+f*(xyzw[4]-xyzw[1]);
                  PR->PickXYZ[2]=xyzw[2]+f*(xyzw[5]-xyzw[2]);

                  /* Which is the closest node? */
                  if (f <= 0.5) {
                     PR->iAltSel[SUMA_ENODE_0] = i0;
                     PR->iAltSel[SUMA_ENODE_1] = i1;
                     if (SUMA_ABS(f)> 0.01) {/* not too close to edge */
                        PR->datum_index = datum_index;
                     } else PR->datum_index = -1;
                  } else {
                     SUMA_LHv("++half way, looking for opp. edge [%d %d]\n",
                              i1, i0);
                     PR->iAltSel[SUMA_ENODE_0] = i1;
                     PR->iAltSel[SUMA_ENODE_1] = i0;
                     if (SUMA_ABS(1.0-f) > 0.01) { /* not too close to edge */
                        /* does edge [i1, i0] exist? If so take it*/
                        if (SUMA_GDSET_PointsToSegIndex(dset, i1, i0, &ir)) {
                           SUMA_LHv("Switching primitve to edge %d\n", ir);
                           PR->datum_index = ir;
                        } else { /* leave old hit, no opposite edge */
                           PR->datum_index = datum_index;
                        }
                     } else PR->datum_index = -1;
                  }
               }
            } else { /* picked a node, no data on it*/
               PR->iAltSel[SUMA_ENODE_0] = 
                  SUMA_GDSET_Index_To_NodeIndex(dset, PR->primitive_index);
               PR->iAltSel[SUMA_ENODE_1] = -1;
               PR->datum_index = -1;
               fv = SUMA_GDSET_NodeXYZ(dset, PR->iAltSel[SUMA_ENODE_0], 
                                       SUMA_ADO_variant(ado), NULL);
               PR->PickXYZ[0]=fv[0]; PR->PickXYZ[1]=fv[1]; PR->PickXYZ[2]=fv[2];
            }                        
            break; }
         case GDSET_type:
            SUMA_S_Err("I don't expect graph dsets to be picked directly");
            break;
         case CDOM_type:
            SUMA_S_Err("CIFTI not picked on buffer");
            break;
         case VO_type:
            SUMA_S_Err("VOs not picked on buffer....");
            break;
         case MASK_type:
            SUMA_S_Err("Masks not picked on buffer....");
            break;
         case TRACT_type:
            {
            SUMA_TractDO *tdo=(SUMA_TractDO *)ado;
            int nn=0, mm=0, nnmin=-1, mmmin=-1, mmmin3=-1, oki=0, it=0, ib=0;
            float *scrxyz = NULL;
            float mindist, dist;
            float fmin;
            TAYLOR_TRACT *ttn=NULL;
            TAYLOR_BUNDLE *tb=NULL;

            oki = 0;
            if (!strcmp(PR->primitive,"bundles")) {
               ib = PR->primitive_index;
               SUMA_LHv("Seeking bundle %d/%d\n", 
                           (int)PR->primitive_index, tdo->net->N_tbv);
               tb = TDO_BUNDLE(tdo, PR->primitive_index);
               if (!(oki=SUMA_Bundle_Pick_Intersect(tb, 'B', -1, sv, NULL, 0, 
                                            &nnmin, &mmmin, &fmin, &mindist))) {
                  SUMA_LH("No bunlde intersection");
               }
            } else if (!strcmp(PR->primitive,"tracts")) {
               SUMA_LHv("Seeking tract %d/%d\n", 
                           (int)PR->primitive_index, TDO_N_TRACTS(tdo));
               if (Network_1T_to_TB(tdo->net, 
                           (int)PR->primitive_index, &it, &ib, NULL, NULL) < 0) {
                  SUMA_S_Err("Failed to resolve tract index");
               } else {
                  tb = TDO_BUNDLE(tdo, ib);
                  if (!(oki=SUMA_Bundle_Pick_Intersect(tb, 'B', it, sv, NULL, 0, 
                                            &nnmin, &mmmin, &fmin, &mindist))) {
                     SUMA_LH("No tract intersection");
                  }
               }
            }
            if (oki) {
               if (nnmin > -1) {
                  mmmin3=3*mmmin;
                  ttn = tb->tracts+nnmin;
                  if (fmin != 0.0f) {
                     PR->PickXYZ[0]=ttn->pts[mmmin3+0]+
                                 fmin*(ttn->pts[mmmin3+3]-ttn->pts[mmmin3]);
                     PR->PickXYZ[1]=ttn->pts[mmmin3+1]+
                                 fmin*(ttn->pts[mmmin3+4]-ttn->pts[mmmin3+1]);
                     PR->PickXYZ[2]=ttn->pts[mmmin3+2]+
                                 fmin*(ttn->pts[mmmin3+5]-ttn->pts[mmmin3+2]);
                  } else {
                     /* from the quick search, no fmin used*/
                     PR->PickXYZ[0]=ttn->pts[mmmin3+0];
                     PR->PickXYZ[1]=ttn->pts[mmmin3+1];
                     PR->PickXYZ[2]=ttn->pts[mmmin3+2];   
                  }
                  PR->iAltSel[SUMA_TRC_PNT] = mmmin; 
                  PR->iAltSel[SUMA_BUN_TRC] = nnmin; 
                  PR->iAltSel[SUMA_NET_BUN] = ib;  
                  PR->datum_index = Network_PTB_to_1P(tdo->net,
                                                      PR->iAltSel[SUMA_TRC_PNT], 
                                                      PR->iAltSel[SUMA_BUN_TRC],
                                                      PR->iAltSel[SUMA_NET_BUN]);
                  PR->iAltSel[SUMA_NET_TRC] = Network_TB_to_1T(tdo->net,
                                                      PR->iAltSel[SUMA_BUN_TRC],
                                                      PR->iAltSel[SUMA_NET_BUN]);
                  SUMA_LHv("Bundle %ld intersected, tract %ld, "
                           "point %ld, tract in net %ld. \n"
                           "Point NetID: %ld\n"
                           "XYZ[%.3f %.3f %.3f], fmin=%f\n", 
                           PR->iAltSel[SUMA_NET_BUN], 
                           PR->iAltSel[SUMA_BUN_TRC],
                           PR->iAltSel[SUMA_TRC_PNT], 
                           PR->iAltSel[SUMA_NET_TRC], PR->datum_index, 
                           PR->PickXYZ[0], PR->PickXYZ[1], PR->PickXYZ[2], fmin);
                           
                  #if 0 /* Just to debug reverse lookup */
                  if (Network_1P_to_PTB(tdo->net, PR->datum_index, 
                                         &mmmin, &nnmin, &nn, NULL) < 0) {
                     SUMA_S_Err("Failed in reverse lookup test");
                  } else {
                     SUMA_LHv("Inverse lookup: %ld --> P %d, T %d, B %d\n",
                              PR->datum_index, mmmin, nnmin, nn); 
                  }
                  #endif
               }
            }
            f = fmin;
            break; }
         default:
            SUMA_S_Warnv("Not ready to get location for %s\n",
                      SUMA_ObjectTypeCode2ObjectTypeName(codf->ref_do_type));
            break;
      }
   }
   
   /* report */
   if (codf) {
         SUMA_S_Notev("\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n"
                   "DO pick: colid (%ld=[%d %d %d %d]) \n"
                   "      is in    <%s, %s, %s>, f=%1.3f\n"
                   "   datum = %ld, iAltSel = [",
                   n4, colid[0], colid[1], colid[2], colid[3],
                   cod->Label, cod->variant, cod->primitive, f, 
                   PR->datum_index);
        for (iii=0; iii<SUMA_N_IALTSEL_TYPES; ++iii)
            fprintf(SUMA_STDOUT, "%ld, ", PR->iAltSel[iii]);
        fprintf(SUMA_STDOUT, "]\n   dAltSel = [");
        for (iii=0; iii<SUMA_N_DALTSEL_TYPES; ++iii)
            fprintf(SUMA_STDOUT, "%.3f, ", PR->dAltSel[iii]);
        fprintf(SUMA_STDOUT, "]\n"
                      "\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");
   }
   
   SUMA_RETURN(PR);
}

SUMA_PICK_RESULT *SUMA_New_Pick_Result(SUMA_PICK_RESULT *PR) 
{
   static char FuncName[]={"SUMA_New_Pick_Result"};
   int i;
   if (!PR) {
      PR = (SUMA_PICK_RESULT *)SUMA_calloc(1,sizeof(SUMA_PICK_RESULT));
   }
   PR->primitive_index = -1;
   PR->datum_index = -1;
   for (i=0; i<SUMA_N_IALTSEL_TYPES; ++i) PR->iAltSel[i] = -1;
   for (i=0; i<SUMA_N_DALTSEL_TYPES; ++i) PR->dAltSel[i] = 0.0;
   SUMA_ifree(PR->primitive);
   SUMA_ifree(PR->ado_idcode_str);
   /* Imprint with event structure */
   PR->evr = (SUMA_EVENT *)malloc(sizeof(SUMA_EVENT));
   if (SUMAg_CF->lev) memcpy(PR->evr, SUMAg_CF->lev, sizeof(SUMA_EVENT));
   else memset(PR->evr, 0, sizeof(SUMA_EVENT));
   /* SUMA_ShowEvent(PR->evr, 0, "From New Pick Result\n"); */
   return(PR);
}

SUMA_PICK_RESULT *SUMA_free_PickResult(SUMA_PICK_RESULT *PR)
{
   static char FuncName[]={"SUMA_free_PickResult"};
   SUMA_ENTRY;
   if (!PR) SUMA_RETURN(PR);
   SUMA_ifree(PR->primitive);
   SUMA_ifree(PR->ado_idcode_str);
   SUMA_ifree(PR->dset_idcode_str);
   SUMA_ifree(PR->evr);
   SUMA_free(PR);
   SUMA_RETURN(NULL);
}

SUMA_Boolean SUMA_ADO_StorePickResult(SUMA_ALL_DO *ado, SUMA_PICK_RESULT **PRP)
{
   static char FuncName[]={"SUMA_ADO_StorePickResult"};
   
   SUMA_ENTRY;
   
   if (!PRP || !*PRP) SUMA_RETURN(NOPE);  

   switch (ado->do_type) {
      case SO_type: {
         SUMA_SURF_SAUX *Saux = SUMA_ADO_SSaux(ado);
         SUMA_free_PickResult(Saux->PR);
         Saux->PR = *PRP; *PRP = NULL;
         SUMA_RETURN(YUP);
         break; }
      case CDOM_type: {
         SUMA_CIFTI_SAUX *Saux = SUMA_ADO_CSaux(ado);
         SUMA_free_PickResult(Saux->PR);
         Saux->PR = *PRP; *PRP = NULL;
         SUMA_RETURN(YUP);
         break; }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         SUMA_GRAPH_SAUX *Saux = SDSET_GSAUX(dset);
         /* Is the selection type changed? If so, then
         you need to flush the pick buffer */

         if (Saux->PR) {
            if ( (Saux->PR->datum_index * (*PRP)->datum_index < 0) ||
                 ((*PRP)->datum_index == -1 && 
                  (Saux->PR->iAltSel[SUMA_ENODE_0] != 
                              (*PRP)->iAltSel[SUMA_ENODE_0])) )  {
               /* Going from edge selection to node selection, in general
                  But there is no need to get picky */
              SUMA_FlushPickBufferForDO((SUMA_ALL_DO *)SUMA_find_Dset_GLDO(dset, 
                                                 "G3D",NULL));            
            }
         }
         SUMA_free_PickResult(Saux->PR);
         Saux->PR = *PRP; *PRP = NULL;
         SUMA_RETURN(YUP);
         break; }
      case GRAPH_LINK_type: 
         SUMA_RETURN(SUMA_ADO_StorePickResult(
            (SUMA_ALL_DO*)SUMA_find_GLDO_Dset(
                           (SUMA_GraphLinkDO*)ado),PRP));
         break;
      case TRACT_type: {
         SUMA_TRACT_SAUX *Saux = SUMA_ADO_TSaux(ado);
         SUMA_free_PickResult(Saux->PR);
         Saux->PR = *PRP; *PRP = NULL;
         SUMA_RETURN(YUP);
         break; }
      case MASK_type: {
         SUMA_MASK_SAUX *Saux = SUMA_ADO_MSaux(ado);
         if (Saux) {
            SUMA_free_PickResult(Saux->PR);
            Saux->PR = *PRP; *PRP = NULL;
         } else {
            SUMA_S_Err("NULL Saux!!!, don't let that happen");
            SUMA_RETURN(NOPE);
         }
         SUMA_RETURN(YUP);
         break; }
      case VO_type: {
         SUMA_VOL_SAUX *Saux = SUMA_ADO_VSaux(ado);
         if (!(*PRP)->primitive) {
            SUMA_S_Err("NULL primitve not acceptable for VOs");
            break;
         }
         if (!strcmp((*PRP)->primitive,"voxel")) {
            SUMA_free_PickResult(Saux->PR);
            Saux->PR = *PRP; *PRP = NULL;
            SUMA_RETURN(YUP);
         } else if (!strcmp((*PRP)->primitive,"cutplane")) {
            SUMA_free_PickResult(Saux->PRc);
            Saux->PRc = *PRP; *PRP = NULL;
            SUMA_RETURN(YUP);
         } else {
            SUMA_S_Err("Bad primitive %s for VO", (*PRP)->primitive);
            SUMA_DUMP_TRACE("Who dunit?");
         }
         break; }
      default:
         SUMA_S_Errv("Note ready for type %s\n", ADO_TNAME(ado));
         break; 
   }
   SUMA_RETURN(NOPE);
}

SUMA_PICK_RESULT * SUMA_ADO_GetPickResult(SUMA_ALL_DO *ado, char *primitive)
{
   static char FuncName[]={"SUMA_ADO_GetPickResult"};
   SUMA_PICK_RESULT *PR=NULL;
   
   SUMA_ENTRY;
   
   if (!ado) SUMA_RETURN(NULL);  
   if (!primitive) primitive = "none";
   
   switch (ado->do_type) {
      case SO_type:{
         SUMA_SURF_SAUX *Saux = SUMA_ADO_SSaux(ado);
         SUMA_RETURN(Saux->PR);
         break; }
      case CDOM_type: {
         SUMA_CIFTI_SAUX *Saux = SUMA_ADO_CSaux(ado);
         SUMA_RETURN(Saux->PR); 
         break; }
      case GDSET_type: {
         SUMA_DSET *dset=(SUMA_DSET *)ado;
         SUMA_GRAPH_SAUX *Saux = SDSET_GSAUX(dset);
         SUMA_RETURN(Saux->PR);
         break; }
      case GRAPH_LINK_type: 
         SUMA_RETURN(SUMA_ADO_GetPickResult(
           (SUMA_ALL_DO*)SUMA_find_GLDO_Dset((SUMA_GraphLinkDO*)ado),primitive));
         break;
      case TRACT_type: {
         SUMA_TRACT_SAUX *Saux = SUMA_ADO_TSaux(ado);
         SUMA_RETURN(Saux->PR);
         break; }
      case MASK_type: {
         SUMA_MASK_SAUX *Saux = SUMA_ADO_MSaux(ado);
         SUMA_RETURN(Saux->PR);
         break; }
      case VO_type: {
         SUMA_VOL_SAUX *Saux = SUMA_ADO_VSaux(ado);
         if (!strcmp(primitive,"voxel")) SUMA_RETURN(Saux->PR);
         else if (!strcmp(primitive,"cutplane")) SUMA_RETURN(Saux->PRc);
         else {
            SUMA_S_Err("Bad primitive %s for VO", primitive);
         }
         break; }
      default:
         SUMA_S_Errv("Note ready for type %s\n", ADO_TNAME(ado));
         break; 
   }
   SUMA_RETURN(NOPE);
}


/*!
*/
void SUMA_Show_Pick_Colid_List(DList *pick_colid_list, FILE *fout) 
{
   static char FuncName[]={"SUMA_Show_Pick_Colid_List"};
   char *s = NULL;
   
   SUMA_ENTRY;
   if (!fout) fout = SUMA_STDOUT;
   
   s = SUMA_Pick_Colid_List_Info(pick_colid_list);
   fprintf(fout, "%s", s);
   SUMA_ifree(s);
    
   SUMA_RETURNe;
}

char *SUMA_Pick_Colid_List_Info (DList *pick_colid_list)
{
   static char FuncName[]={"SUMA_Pick_Colid_List_Info"};
   char *s = NULL;
   SUMA_STRING *SS = NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_DO_Types do_type;
   void *vv=NULL;
   SUMA_DSET *dset=NULL;
   DListElmt *el=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_COLID_OFFSET_DATUM *cod=NULL;
   
   SUMA_ENTRY;

   SS = SUMA_StringAppend(NULL, NULL);
      
   if (!pick_colid_list) { 
      SS = SUMA_StringAppend(SS,"NULL pick_colid_list"); goto CLEAN_RETURN; 
   }
   if (!dlist_size(pick_colid_list)) {
      SS = SUMA_StringAppend(SS,"Empty pick_colid_list"); goto CLEAN_RETURN; 
   }
   SS = SUMA_StringAppend_va(SS,"DO Pick List of %d elements\n", 
                              dlist_size(pick_colid_list));
   do {
      if (!el) el = dlist_head(pick_colid_list);
      else el = dlist_next(el);
      if (!el || !el->data) {
         SS = SUMA_StringAppend(SS, "   NULL element!");
      } else {
         cod = (SUMA_COLID_OFFSET_DATUM *)el->data;
         SS = SUMA_StringAppend_va(SS,"   DO %s, Primitive %s, [%ld to %ld]\n",
                                          cod->Label, cod->primitive, 
                                          cod->i0, cod->i1);
         vv = SUMA_Picked_reference_object(cod, &do_type);
         switch (do_type) {
            case MD_DSET_type:
               dset = (SUMA_DSET *)vv;
               SS = SUMA_StringAppend_va(SS,
                        "     Reference object is a %s dataset labeled %s "
                        "(reference type %s)\n",
                        "Multi Domain",
                        SDSET_LABEL(dset),
                        SUMA_ObjectTypeCode2ObjectTypeName(cod->ref_do_type));
               break;
            case ANY_DSET_type:
	    case GDSET_type:
               dset = (SUMA_DSET *)vv;
               SS = SUMA_StringAppend_va(SS,
                        "     Reference object is a %s dataset labeled %s "
                        "(reference type %s)\n",
                        SUMA_isCIFTIDset(dset) ? "CIFTI" : 
                              (SUMA_isGraphDset(dset) ? "Graph":"Surface-based"),
                        SDSET_LABEL(dset),
                        SUMA_ObjectTypeCode2ObjectTypeName(cod->ref_do_type));
               break;
            case SO_type:
               SO = (SUMA_SurfaceObject *)vv;
               SS = SUMA_StringAppend_va(SS,
                        "     Reference object is a surface labeled %s "
                        "(reference type %s)\n",
                        SO->Label,
                        SUMA_ObjectTypeCode2ObjectTypeName(cod->ref_do_type));
               break;
            case GRAPH_LINK_type:
               ado = (SUMA_ALL_DO *)vv;
                  SS = SUMA_StringAppend_va(SS,
                        "     Reference object is a graph link labeled %s "
                        "(reference type %s)\n",
                        SUMA_ADO_Label(ado),
                        SUMA_ObjectTypeCode2ObjectTypeName(cod->ref_do_type));
               break;
            case TRACT_type:
               SS = SUMA_StringAppend_va(SS,
                        "     Reference object is a tract object labeled %s "
                        "(reference type %s)\n",
                        SUMA_ADO_Label(ado),
                        SUMA_ObjectTypeCode2ObjectTypeName(cod->ref_do_type));
               break;
            case CDOM_type:
               SS = SUMA_StringAppend_va(SS,
                        "     Reference object is a CIFTI DO labeled %s "
                        "(reference type %s)\n",
                        SUMA_ADO_Label(ado),
                        SUMA_ObjectTypeCode2ObjectTypeName(cod->ref_do_type));
               break;
            case MASK_type:
               SS = SUMA_StringAppend_va(SS,
                        "     Reference object is a mask object labeled %s "
                        "(reference type %s)\n",
                        SUMA_ADO_Label(ado),
                        SUMA_ObjectTypeCode2ObjectTypeName(cod->ref_do_type));
               break;
            default:
               SS = SUMA_StringAppend_va(SS,
                     "     Parent, not surface or dset.\n");
               break;
         }
      }
   } while (el != dlist_tail(pick_colid_list));
   
   CLEAN_RETURN:
   SUMA_SS2S(SS,s);
   
   SUMA_RETURN(s);
}

int SUMA_MarkLineDOsIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                               int IgnoreSameNode)
{
   static char FuncName[]={"SUMA_MarkLineDOsIntersect"};

   SUMA_PICK_RESULT *PR = NULL;
   SUMA_ALL_DO *ado = NULL;
   int ans;
   
   SUMA_ENTRY;
   SUMA_S_Warn("Do not call me anymore."
               "Go via SUMA_ComputeLineDOsIntersect. "
               "This is left here for testing purposes");
   ans = SUMA_ComputeLineDOsIntersect(sv, dov, IgnoreSameNode, &ado);
   if (ans <= 0) {
      SUMA_RETURN(ans);
   }
   /* just for temporary testing, get PR back from list and apply it */
   PR = SUMA_Get_From_PickResult_List(sv, ado, NULL);
   ans = SUMA_Apply_PR(sv, &PR);
   
   SUMA_RETURN(ans);
}

/*!
   Determines the intersection between pickline and displayable objects
*/
int SUMA_ComputeLineDOsIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                                  int IgnoreSameNode, SUMA_ALL_DO **pado)
{
   static char FuncName[]={"SUMA_ComputeLineDOsIntersect"};
   int i, j, *MembDOs=NULL, N_MembDOs;
   SUMA_DO_Types ttv[12];
   SUMA_PICK_RESULT *hit=NULL;
   GLubyte colans[4];
   SUMA_COLID_OFFSET_DATUM *codf=NULL;
   SUMA_ALL_DO *ado=NULL;
   SUMA_EngineData *ED = NULL;
   DList *list = NULL;
   DListElmt *SetNodeElem = NULL, *Location=NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean NodeIgnored = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv || !dov) SUMA_RETURN(-1);
   
   SUMA_LH("DO pickin");
   
   if (sv->PickPix[0] < 0 || sv->PickPix[1] < 0 ||
       sv->PickPix[0] >= sv->X->aWIDTH ||  sv->PickPix[1] >= sv->X->aHEIGHT) {
      /* This happens when you select and drag outside of the viewing area
      Don't return in error */
      SUMA_LHv("Bad PickPix=[%d %d] for viewport %d %d\n",
                 sv->PickPix[0], sv->PickPix[1], sv->X->aWIDTH, sv->X->aHEIGHT); 
      SUMA_RETURN(0);
   }

   /* Any pickable DO, other than brain surfaces, that does not require
      pick buffer picking? */
   ttv[0] = GRAPH_LINK_type; ttv[1] = NOT_SET_type;
   MembDOs = SUMA_ViewState_Membs(&(sv->VSv[sv->iState]), ttv, &N_MembDOs);
   for (i=0; i<N_MembDOs; ++i) {
      if ((hit = SUMA_WhatWasPicked_FrameSO(sv, MembDOs[i]))) { 
         if ( hit->datum_index < 0 && (
             ( hit->iAltSel[SUMA_ENODE_0]>=0 && 
               hit->iAltSel[SUMA_ENODE_1]>=0)) ) {
            /* You have selected a point pair that has no edge defined.
               This can happen when you click on an empty cell in the matrix */
            hit = SUMA_free_PickResult(hit);
         } else {
            /* got something, leave. 
               Perhaps in the future will need to go through
            all stack (slices) and pick the best one ... */
            ado = iDO_ADO(MembDOs[i]);
            if (pado) *pado = ado;
            break;
         }
      }
   }
   SUMA_ifree(MembDOs);
   
   if (!hit) { /* nothing found above, go for pick buffer */
      if (!SUMA_PickBuffer(sv, 2, dov)) { /* refresh the buffer only if needed */
         SUMA_S_Err("Failed to refresh buffer");
         SUMA_RETURN(-1);
      }

      if (!sv->pickrenpix4) {
         SUMA_S_Err("Could not form pickrenpix4, should this be an error?");
         SUMA_RETURN(-1);
      }
      if (!sv->pick_colid_list || !dlist_size(sv->pick_colid_list)) {
         SUMA_LH("No such pickable objects");
      } else {
         /* get pixel at click */
         i = sv->PickPix[0]; j = sv->PickPix[1];
         if (SUMA_GetColidInPickBuffer4(sv->pickrenpix4, 
                              sv->X->aWIDTH, sv->X->aHEIGHT,
                              &i, &j, 2, colans)) {
            if (LocalHead) { 
               /* Just for debugging, draw the crosshair point */
               SUMA_MarkPickInBuffer4(sv, 1, NULL);
               SUMA_LHv("User pixel selection: \n"
                        "  closest hit to click at %d %d was from %d %d\n"
                        "  colid = %d %d %d %d \n", 
                        sv->PickPix[0], sv->PickPix[1], i, j, 
                        colans[0] , colans[1],
                        colans[2] , colans[3]);
            }
            /* so what was that you touched. Note hit is never
               returned as null. */
            hit = SUMA_WhatWasPicked(sv, colans, &codf, i, j, NULL);
            if (!hit || !hit->ado_idcode_str) {
               SUMA_LH("Not hit found.");
            } else {
               if (!(ado = iDO_ADO(SUMA_Picked_DO_ID(codf)))) {
                  SUMA_S_Err("Could not locate object in codf (%s) /hit (%s)!",
                            codf->ref_idcode_str, hit->ado_idcode_str);
                  SUMA_free_PickResult(hit); hit = NULL;
               } else {
                  if (pado) *pado = ado;
               }
            }
         } else {
            SUMA_LH("There is no there there\n");
            hit = NULL;
         }
      }
   }
   
   if (hit && ado) {
      SUMA_LH("Adding hit to list");
      if (!SUMA_Add_To_PickResult_List(sv, ado, NULL, &hit)) {
         SUMA_S_Err("Failed to add selected ado");
         SUMA_RETURN(-1);
      }
      SUMA_RETURN(1);
   } else SUMA_RETURN(0);
}

int SUMA_Apply_PR_DO(SUMA_SurfaceViewer *sv, SUMA_ALL_DO *ado,
                     SUMA_PICK_RESULT **PRi)
{
   static char FuncName[]={"SUMA_Apply_PR_DO"};
   DList *list = NULL;
   DListElmt *SetNodeElem = NULL, *Location=NULL;
   SUMA_Boolean NodeIgnored = NOPE;
   SUMA_PICK_RESULT *PR;
   int NP=0, ip=0, it=-1, id = 0, iv3[3], iv15[15];
   SUMA_EngineData *ED = NULL;
   SUMA_Boolean LocalHead = NOPE;
  
   SUMA_ENTRY;
   SUMA_LH("Here");
   if (!sv || !ado || !PRi || !*PRi) { SUMA_S_Err("Niente"); SUMA_RETURN(-1); }

   PR = *PRi; /* keep local copy */
   /* Store the PR in ado, hide it from return potential */
   SUMA_ADO_StorePickResult(ado, PRi);
         
   SUMA_LHv("Hit object type %s, label %s\n",
         SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type),
         SUMA_ADO_Label(ado));
         
   sv->Focus_DO_ID = ADO_iDO(ado);
   SUMA_UpdateViewerTitle(sv);

   /* if the surface controller is open, update it */
   if (SUMA_isADO_Cont_Realized(ado))
      SUMA_Init_SurfCont_SurfParam(ado);

   /* print nodes about the pick */
   switch (ado->do_type) {
      case TRACT_type:
   fprintf(SUMA_STDOUT, "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
   fprintf(SUMA_STDOUT, "Selected network %s (Focus_DO_ID # %d).\n"
                        "Point %ld, (Bundle %ld, Tract %ld, Point %ld)\n", 
      ADO_LABEL(ado), sv->Focus_DO_ID, PR->datum_index,
      PR->iAltSel[SUMA_NET_BUN], PR->iAltSel[SUMA_BUN_TRC],
      PR->iAltSel[SUMA_TRC_PNT]);
   fprintf(SUMA_STDOUT, "Seletion coordinates:\n");
   fprintf(SUMA_STDOUT, "%f, %f, %f\n", 
      PR->PickXYZ[0], PR->PickXYZ[1], PR->PickXYZ[2]);
   fprintf(SUMA_STDOUT, "\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");
         break;
      case MASK_type:
   fprintf(SUMA_STDOUT, "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
   fprintf(SUMA_STDOUT, "Selected mask %s (Focus_DO_ID # %d).\n", 
      ADO_LABEL(ado), sv->Focus_DO_ID);
   fprintf(SUMA_STDOUT, "Seletion coordinates:\n");
   fprintf(SUMA_STDOUT, "%f, %f, %f\n", 
      PR->PickXYZ[0], PR->PickXYZ[1], PR->PickXYZ[2]);
   fprintf(SUMA_STDOUT, "\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");
         break;
      case GRAPH_LINK_type:
   fprintf(SUMA_STDOUT, "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
   fprintf(SUMA_STDOUT, "Selected Graph Dset %s (Focus_DO_ID # %d).\n"
                        "Edge %ld, (P0 %ld, P1 %ld)\n", 
      ADO_LABEL(ado), sv->Focus_DO_ID, PR->datum_index,
      PR->iAltSel[SUMA_ENODE_0], PR->iAltSel[SUMA_ENODE_1]);
   fprintf(SUMA_STDOUT, "Seletion coordinates:\n");
   fprintf(SUMA_STDOUT, "%f, %f, %f\n", 
      PR->PickXYZ[0], PR->PickXYZ[1], PR->PickXYZ[2]);
   fprintf(SUMA_STDOUT, "\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");   
         break;
      default:
   SUMA_S_Err("Not yet set to report on %s", ADO_LABEL(ado));
         break;
   }
    
   /* Based on what you selected, update controller */
   /* Set the Nodeselection at the closest node */
   if (PR->datum_index >= 0 ||
       (PR->datum_index == -1 && 
        PR->iAltSel[SUMA_ENODE_0] >= 0 && 
        PR->iAltSel[SUMA_ENODE_1] == -1) ) { 
                     /* 2nd condition is when only edge node is selected.
                        We insist on PR->iAltSel[SUMA_ENODE_1] == -1 otherwise
                        we would have picked a cell in the matrix for which 
                        there is no data, hence we have ENODE_0, and ENODE_1,
                        but datum_index = -1 */
      it = (int)PR->datum_index;
      if (!list) list = SUMA_CreateList();
      if (PR->ignore_same_datum && 
            SUMA_ADO_SelectedDatum(ado, NULL, NULL) == it) {
         SUMA_LHv("Ignoring identical datum selection %d on object %s\n",
                  SUMA_ADO_SelectedDatum(ado, NULL, NULL), SUMA_ADO_Label(ado));
         NodeIgnored = YUP;
      } else {
         ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
         SetNodeElem = SUMA_RegisterEngineListCommand (  list, ED, 
                                                SEF_i, (void*)&it,
                                                SES_Suma, (void *)sv, NOPE,
                                                SEI_Head, NULL);
         if (!SetNodeElem) {
            fprintf( SUMA_STDERR,
                     "Error %s: Failed to register SetNodeElem\n", FuncName);
            SUMA_RETURN (-1);
         } else {
            SUMA_RegisterEngineListCommand (  list, ED, 
                                              SEF_ngr, NULL,
                                              SES_Suma, (void *)sv, NOPE,
                                              SEI_In, SetNodeElem);  
         }
         switch (ado->do_type) {
            case TRACT_type:
               iv15[SUMA_NET_BUN] = (int)PR->iAltSel[SUMA_NET_BUN];
               iv15[SUMA_BUN_TRC] = (int)PR->iAltSel[SUMA_BUN_TRC];
               iv15[SUMA_TRC_PNT] = (int)PR->iAltSel[SUMA_TRC_PNT];
               iv15[SUMA_NET_TRC] = (int)PR->iAltSel[SUMA_NET_TRC];
               SUMA_RegisterEngineListCommand (  list, ED, 
                                                 SEF_iv15, (void *)iv15,
                                                 SES_Suma, (void *)sv, NOPE,
                                                 SEI_In, SetNodeElem);
               break;
            case SO_type:
            case VO_type:
               /* handled in separate function */
               break;
            default: 
               SUMA_LH("No aux set here for type %s of %s\n",
                        ADO_TNAME(ado), SUMA_ADO_Label(ado));
               break;
         }
              
      }
   }
      
      
   /* Set the selected edge node (cunningly in recycled selected face set) */
   it = PR->iAltSel[SUMA_ENODE_0];
   if (!list) list = SUMA_CreateList();
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
   if (!(Location = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3, (void*)PR->PickXYZ,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURN (-1);
   }
   /* and add the ado with this location, needed for VisX business*/
   SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_vp, (void *)ado,
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, Location);
   /* attach the cross hair to the selected ado */
   iv3[0] = SUMA_whichDO(PR->ado_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   iv3[1] = PR->datum_index; 
   iv3[2] = PR->iAltSel[SUMA_ENODE_0];
   ED = SUMA_InitializeEngineListData (SE_BindCrossHair);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_iv3, (void*)iv3,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURN (-1);
   }

   /* check to see if AFNI needs to be notified */
   /* Need to deal with SUMA_TO_MATLAB_STREAM_INDEX too 
      Same for remaining occurrence of SUMA_AFNI_STREAM_INDEX*/
   if (  ( SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] && 
           sv->LinkAfniCrossHair )                             ||
         ( SUMAg_CF->Connected_v[SUMA_HALLO_SUMA_LINE])        ||
         ( SUMAg_CF->Connected_v[SUMA_INSTA_TRACT_LINE])    ) {
      if (LocalHead) 
         fprintf(SUMA_STDERR,
                  "%s: Notifying Afni of CrossHair XYZ\n", FuncName);
      /* register a call to SetAfniCrossHair */
      if (!list) list = SUMA_CreateList();
      it = SUMA_ShftCont_Event(PR->evr);
      ED = SUMA_InitializeEngineListData (SE_SetAfniCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&it,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Tail, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURN (-1);
      }
      if (MASK_MANIP_MODE(sv) && SUMAg_CF->Dev) {
         SUMA_ALL_DO *ado = SUMA_whichADOg(sv->MouseMode_ado_idcode_str);
         DListElmt *Location=NULL;
         SUMA_LH("Might be telling afni about mask...");
         if (ado && ado->do_type == MASK_type) {
            SUMA_MaskDO *mdo = (SUMA_MaskDO *)ado;
            ED = SUMA_InitializeEngineListData (SE_SetAfniMask);
            if (!(Location=SUMA_RegisterEngineListCommand (  list, ED, 
                                                SEF_fv3, (void*)mdo->cen,
                                                SES_Suma, (void *)sv, NOPE,
                                                SEI_Tail, NULL))) {
               SUMA_S_Err("Failed to register element\n");
               SUMA_RETURN (-1);
            }
            SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_s, (void *)(ADO_ID(ado)),
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, Location);
         }
      }
      
      if (!SUMA_Engine (&list)) {
         fprintf( SUMA_STDERR, 
                  "Error %s: SUMA_Engine call failed.\n", FuncName);
         SUMA_RETURN (-1);
      }
   }else {
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: No Notification to AFNI.\n", FuncName);
   }

   /* now put in a request for locking cross hair but you must do 
      this after the node selection has been executed 
      NOTE: You do not always have SetNodeElem because the list might 
      get emptied in the call to AFNI notification.
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

   SUMA_RETURN(1);
}

int SUMA_Apply_PR(SUMA_SurfaceViewer *sv, SUMA_PICK_RESULT **PR)
{
   static char FuncName[]={"SUMA_Apply_PR"};
   SUMA_ALL_DO *ado=NULL, *ado_pick=NULL;
   float *xyz=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv || !PR || !*PR) {
      SUMA_S_Err("NULL input %p %p %p", sv, PR, *PR);
      SUMA_DUMP_TRACE("PR application");
      SUMA_RETURN(-1);
   }
   if (MASK_MANIP_MODE(sv)) {
      ado_pick = SUMA_whichADOg((*PR)->ado_idcode_str);
      SUMA_LH("Mask Manip Mode, moving to %f %f %f per ado %s\n", 
               (*PR)->PickXYZ[0], (*PR)->PickXYZ[1], (*PR)->PickXYZ[2],
               ADO_LABEL(ado_pick));
      SUMA_ifree(sv->LastSel_ado_idcode_str);
      sv->LastSel_ado_idcode_str = SUMA_copy_string((*PR)->ado_idcode_str);
      /* Just move the selected mask */
      if (!(ado = SUMA_whichADOg(sv->MouseMode_ado_idcode_str))) {
         SUMA_S_Err("Yikes, got not ado for MouseMode_ado_idcode_str %s",
              sv->MouseMode_ado_idcode_str?sv->MouseMode_ado_idcode_str:"NULL");
         SUMA_RETURN(-1);
      }
      if (ado->do_type != MASK_type) {
         SUMA_S_Err("Bad ID for mouse mode value");
         SUMA_RETURN(-1);
      }
      /* Set the parent as the ado_pick */
      SUMA_MDO_New_parent((SUMA_MaskDO *)ado, (*PR)->ado_idcode_str,
                          (*PR)->datum_index);
      
      if (ado_pick->do_type == SO_type) {
         SUMA_SurfaceObject *SO = (SUMA_SurfaceObject *)ado_pick;
         /* PickXYZ might be under VisX */
         xyz = SO->NodeList+SO->NodeDim* (*PR)->datum_index;
         SUMA_MDO_New_Doppel((SUMA_MaskDO *)ado, (*PR)->PickXYZ);
      } else {
         xyz = (*PR)->PickXYZ;
         SUMA_MDO_New_Doppel((SUMA_MaskDO *)ado, NULL);
      }
      if (LocalHead) SUMA_DUMP_TRACE("Box motion");
      SUMA_NEW_MASKSTATE();
      SUMA_MDO_New_Cen((SUMA_MaskDO *)ado, xyz);
   }
   
   {
      ado = SUMA_whichADOg((*PR)->ado_idcode_str);
      SUMA_ifree(sv->LastSel_ado_idcode_str);
      sv->LastSel_ado_idcode_str = SUMA_copy_string((*PR)->ado_idcode_str);
      switch (ado->do_type) {
         case SO_type:
            SUMA_RETURN(SUMA_Apply_PR_SO(sv, (SUMA_SurfaceObject *)ado, PR)); 
            break;
         case VO_type:
            SUMA_RETURN(SUMA_Apply_PR_VO(sv, (SUMA_VolumeObject *)ado, PR)); 
            break;
         case GRAPH_LINK_type:
            SUMA_RETURN(SUMA_Apply_PR_DO(sv, ado, PR)); 
            break;
         case TRACT_type:
            SUMA_RETURN(SUMA_Apply_PR_DO(sv, ado, PR)); 
            break;
         case MASK_type:
            SUMA_RETURN(SUMA_Apply_PR_DO(sv, ado, PR)); 
            break;
         default:
            SUMA_S_Err("Not yet implemented for %s", ADO_TNAME(ado));
            SUMA_RETURN(-1);
            break;
      }
   }
   SUMA_RETURN(-1);
}

int SUMA_MarkLineMaskIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                                int IgnoreSameNode)
{/* determine intersection */
   static char FuncName[]={"SUMA_MarkLineMaskIntersect"};
   SUMA_PICK_RESULT *PR = NULL;
   SUMA_ALL_DO *ado = NULL;
   int ans;
   
   SUMA_ENTRY;
   SUMA_S_Warn("Do not call me anymore. Follow the new selection logic");
   ans = SUMA_ComputeLineMaskIntersect(sv, dov, IgnoreSameNode, &ado);
   if (ans <= 0) {
      SUMA_RETURN(ans);
   }
   /* just for temporary testing, get PR back from list and apply it */
   PR = SUMA_Get_From_PickResult_List(sv, ado, NULL);
   ans = SUMA_Apply_PR(sv, &PR);
   SUMA_RETURN(ans);
}

int SUMA_ComputeLineMaskIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                                      int IgnoreSameNode, SUMA_ALL_DO **pado)
{/* determine intersection */
   static char FuncName[]={"SUMA_ComputeLineMaskIntersect"};
   float P0f[3], P1f[3];
   int NP; 
   SUMA_MT_INTERSECT_TRIANGLE *MTI = NULL, *MTIi = NULL;
   float delta_t_tmp, dmin; 
   struct timeval tt_tmp;
   SUMA_ALL_DO *ado=NULL;
   int ip, it, id, ii, N_MDOlist, 
       MDOlist[SUMA_MAX_DISPLAYABLE_OBJECTS], imin;
   char sfield[100], sdestination[100], CommString[SUMA_MAX_COMMAND_LENGTH];
   SUMA_MaskDO *MDO = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   P0f[0] = sv->Pick0[0];
   P0f[1] = sv->Pick0[1];
   P0f[2] = sv->Pick0[2];
   P1f[0] = sv->Pick1[0];
   P1f[1] = sv->Pick1[1];
   P1f[2] = sv->Pick1[2];
   
   N_MDOlist = SUMA_VisibleMDOs(sv, dov, MDOlist);
   if (LocalHead) {
      SUMA_LH("%d visible MDOs", N_MDOlist);
      for (ii=0; ii < N_MDOlist; ++ii) {
         ado = (SUMA_ALL_DO *)dov[MDOlist[ii]].OP;
         fprintf(SUMA_STDERR,"%d: object %d in DOv, label %s, type %s\n", 
                  ii, MDOlist[ii], ADO_LABEL(ado), ADO_TNAME(ado));
      }
   }
   imin = -1;
   dmin = 10000000.0;
   for (ii=0; ii < N_MDOlist; ++ii) { /* find the closest intersection */
      if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: working %d/%d shown masks ...\n", 
                     FuncName, ii, N_MDOlist);
      MDO = (SUMA_MaskDO *)dov[MDOlist[ii]].OP;
      if (!MDO_IS_SURF(MDO) && !MDO_IS_BOX(MDO) && !MDO_IS_SPH(MDO)) {
         fprintf(SUMA_STDERR,
            "Error %s: "
            "Not ready to handle such MDO (%s) intersections on %s\n", 
            FuncName, MDO->mtype, ADO_LABEL((SUMA_ALL_DO *)MDO));
      } if (!MDO->SO) {
         SUMA_S_Err("No SO buster on %s", ADO_LABEL((SUMA_ALL_DO *)MDO));
      } else {
         /* Here we're doing the intersection the lazy way, via the SO,
         although this can be done a lot faster in other ways for box and
         sphere, speed is not an issue here */
         SUMA_etime (&tt_tmp, 0);
         MTIi = SUMA_MT_intersect_triangle(P0f, P1f, MDO->SO->NodeList,
                                 MDO->SO->N_Node, MDO->SO->FaceSetList, 
                                 MDO->SO->N_FaceSet, NULL, 0);

         delta_t_tmp = SUMA_etime (&tt_tmp, 1);
         SUMA_LH("Intersection took %f seconds with %s.\n", 
               delta_t_tmp, 
               ADO_LABEL((SUMA_ALL_DO *)dov[MDOlist[ii]].OP));

         if (MTIi == NULL) {
            fprintf(SUMA_STDERR,
                     "Error %s: SUMA_MT_intersect_triangle failed.\n", FuncName);
            SUMA_RETURN (-1);
         }
         
         if (MTIi->N_hits) { 
            /* decide on the closest surface to the clicking point */
            if (MTIi->t[MTIi->ifacemin] < dmin) {
               if (LocalHead) 
                  fprintf (SUMA_STDERR, "%s: A minimum for surface %d.\n", 
                           FuncName, ii);
               dmin = MTIi->t[MTIi->ifacemin];
               imin = MDOlist[ii];
               MTI = MTIi;
            }else {     
               /* not good, toss it away */
               SUMA_LH("ii=%d not any closer (%f vs %f). freeing MTIi...\n", 
                        ii,MTIi->t[MTIi->ifacemin], dmin);
               MTIi = SUMA_Free_MT_intersect_triangle(MTIi); 
            }
         }else {
            /* not good, toss it away */
           if (LocalHead) 
               fprintf (SUMA_STDERR, 
                        "%s: ii=%d freeing MTIi no hits...\n", FuncName, ii);
           MTIi = SUMA_Free_MT_intersect_triangle(MTIi); 
        }
      }
    } 

   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: Closest surface is indexed %d in DOv.\n", FuncName, imin);
   
   if (imin >= 0) {
      SUMA_PICK_RESULT *PR;
      SUMA_ALL_DO *ado;
      if (!(ado = iDO_ADO(imin))) {
         SUMA_S_Err("NULL ado at this point? imin = %d", imin);
         SUMA_RETURN(-1);
      }
      PR = SUMA_New_Pick_Result(NULL);
      if (pado) *pado = ado; /* user want answer back */
      PR->ado_idcode_str = SUMA_copy_string(ADO_ID(ado));
      PR->datum_index = MTI->inodemin;
      PR->ignore_same_datum = IgnoreSameNode;
      PR->iAltSel[SUMA_SURF_TRI] = MTI->ifacemin;
      SUMA_COPY_VEC(MTI->P, PR->PickXYZ, 3, float, float);
      /* Add selection result to stack */
      if (!SUMA_Add_To_PickResult_List(sv, ado, NULL, &PR)) {
         SUMA_S_Err("Failed to add selected ado");
         SUMA_RETURN(-1);
      }
   } 
   
   /* clear MTI */
   if (MTI) {
      MTI = SUMA_Free_MT_intersect_triangle(MTI);
   }
   
   if (imin >=0) SUMA_RETURN(1);
   else SUMA_RETURN(0);
}


int SUMA_MarkLineSurfaceIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                                    int IgnoreSameNode)
{
   static char FuncName[]={"SUMA_MarkLineSurfaceIntersect"};
   SUMA_PICK_RESULT *PR = NULL;
   SUMA_ALL_DO *ado = NULL;
   int ans;
   
   SUMA_ENTRY;
   SUMA_S_Warn("Do not call me anymore."
               "Go via SUMA_ComputeLineSurfaceIntersect. "
               "This is left here for testing purposes");
   ans = SUMA_ComputeLineSurfaceIntersect(sv, dov, IgnoreSameNode, &ado);
   if (ans <= 0) {
      SUMA_RETURN(ans);
   }
   /* just for temporary testing, get PR back from list and apply it */
   PR = SUMA_Get_From_PickResult_List(sv, ado, NULL);
   ans = SUMA_Apply_PR(sv, &PR);
   
   SUMA_RETURN(ans);
}

/*!
   Determines the intersection between ]sv->Pick0 sv->Pick1[ and SO
   Highlights the intersected faceset, node and updates cross hair location 
   This used to be part of Button3's code in SUMA_input
   ans = SUMA_ComputeLineSurfaceIntersect (sv, dov);
   \param sv (SUMA_SurfaceViewer *) surface viewer pointer
   \param dov (SUMA_DO *) displayable object vector pointer
   \param IgnoreSameNode (int) 1 do nothing if node already selected
                               0 don't care if it was already selected
   \param pado  (SUMA_ALL_DO **) If not NULL, *pado will contain a copy
                                 of an ADO pointer to the intersected
                                 object, if any. 
   \ret ans (int)  -1 error, 0 no hit, hit 
   
   Note that this function will register whatever surfaces get hit in the
   selections list with a call to SUMA_Add_To_PickResult_List()
   
   also requires SUMAg_DOv and SUMAg_N_DOv
*/
int SUMA_ComputeLineSurfaceIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                                      int IgnoreSameNode, SUMA_ALL_DO **pado)
{/* determine intersection */
   static char FuncName[]={"SUMA_ComputeLineSurfaceIntersect"};
   float P0f[3], P1f[3];
   int NP; 
   SUMA_MT_INTERSECT_TRIANGLE *MTI = NULL, *MTIi = NULL;
   float delta_t_tmp, dmin; 
   struct timeval tt_tmp; 
   int ip, it, id, ii, N_SOlist, 
       SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS], imin;
   char sfield[100], sdestination[100], CommString[SUMA_MAX_COMMAND_LENGTH];
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   P0f[0] = sv->Pick0[0];
   P0f[1] = sv->Pick0[1];
   P0f[2] = sv->Pick0[2];
   P1f[0] = sv->Pick1[0];
   P1f[1] = sv->Pick1[1];
   P1f[2] = sv->Pick1[2];
   
   N_SOlist = SUMA_VisibleSOs(sv, dov, SOlist, 0);
   imin = -1;
   dmin = 10000000.0;
   for (ii=0; ii < N_SOlist; ++ii) { /* find the closest intersection */
      if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: working %d/%d shown surfaces ...\n", 
                     FuncName, ii, N_SOlist);
      SO = (SUMA_SurfaceObject *)dov[SOlist[ii]].OP;
      SUMA_VisX_Pointers4Display(SO, 1); /* using coordinates as displayed */
      if (SO->FaceSetDim != 3) {
         fprintf(SUMA_STDERR,
            "Error %s: "
            "SUMA_MT_intersect_triangle only works for triangular meshes.\n", 
            FuncName);
      } else {

         SUMA_etime (&tt_tmp, 0);

         MTIi = SUMA_MT_intersect_triangle(P0f, P1f, SO->NodeList, SO->N_Node, 
                                        SO->FaceSetList, SO->N_FaceSet, NULL, 0);

         delta_t_tmp = SUMA_etime (&tt_tmp, 1);
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
               "Local Debug %s: Intersection took %f seconds.\n", 
               FuncName, delta_t_tmp);

         if (MTIi == NULL) {
            fprintf(SUMA_STDERR,
                     "Error %s: SUMA_MT_intersect_triangle failed.\n", FuncName);
            SUMA_RETURN (-1);
         }
         
         if (MTIi->N_hits) { 
            /* decide on the closest surface to the clicking point */
            if (MTIi->t[MTIi->ifacemin] < dmin) {
               if (LocalHead) 
                  fprintf (SUMA_STDERR, "%s: A minimum for surface %d.\n", 
                           FuncName, ii);
               dmin = MTIi->t[MTIi->ifacemin];
               imin = SOlist[ii];
               MTI = MTIi;
            }else {     
               /* not good, toss it away */
               if (LocalHead) 
                  fprintf (SUMA_STDERR, 
                           "%s: ii=%d freeing MTIi...\n", FuncName, ii);
               MTIi = SUMA_Free_MT_intersect_triangle(MTIi); 
            }
         }else {
            /* not good, toss it away */
           if (LocalHead) 
               fprintf (SUMA_STDERR, 
                        "%s: ii=%d freeing MTIi no hits...\n", FuncName, ii);
           MTIi = SUMA_Free_MT_intersect_triangle(MTIi); 
        }
      }
      SUMA_VisX_Pointers4Display(SO, 0); /* put things back young man */

    } 

   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: Closest surface is indexed %d in DOv.\n", FuncName, imin);
   
   if (imin >= 0) {
      SUMA_PICK_RESULT *PR;
      SUMA_ALL_DO *ado;
      if (!(ado = iDO_ADO(imin))) {
         SUMA_S_Err("NULL ado at this point?");
         SUMA_RETURN(-1);
      }
      PR = SUMA_New_Pick_Result(NULL);
      if (pado) *pado = ado; /* user want answer back */
      PR->ado_idcode_str = SUMA_copy_string(ADO_ID(ado));
      PR->datum_index = MTI->inodemin;
      PR->ignore_same_datum = IgnoreSameNode;
      PR->iAltSel[SUMA_SURF_TRI] = MTI->ifacemin;
      SUMA_COPY_VEC(MTI->P, PR->PickXYZ, 3, float, float);
      /* Add selection result to stack */
      if (!SUMA_Add_To_PickResult_List(sv, ado, NULL, &PR)) {
         SUMA_S_Err("Failed to add selected ado");
         SUMA_RETURN(-1);
      }
   } 
   
   /* clear MTI */
   if (MTI) {
      MTI = SUMA_Free_MT_intersect_triangle(MTI);
   }
   
   if (imin >=0) SUMA_RETURN(1);
   else SUMA_RETURN(0);
}

int SUMA_Apply_PR_SO(SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO, 
                     SUMA_PICK_RESULT **PRi) 
{
   static char FuncName[]={"SUMA_Apply_PR_SO"};
   SUMA_ALL_DO *ado=NULL;
   DList *list = NULL;
   DListElmt *SetNodeElem = NULL, *Location=NULL;
   SUMA_Boolean NodeIgnored = NOPE;
   SUMA_PICK_RESULT *PR;
   int NP=0, ip=0, it=0, id = 0, iv3[3];
   SUMA_EngineData *ED = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!sv || !SO || !PRi || !*PRi) { SUMA_S_Err("Niente"); SUMA_RETURN(-1); }
    
   /* Mark intersection Facsets */
   ado = (SUMA_ALL_DO *)SO;

   PR = *PRi;   /* Keep local copy */
   /* Store the PR in ado, hide it from return potential */
   SUMA_ADO_StorePickResult(ado, PRi);
   
   
   sv->Focus_DO_ID = ADO_iDO(ado);
   SUMA_UpdateViewerTitle(sv);

   NP = SO->FaceSetDim;
   ip = NP * PR->iAltSel[SUMA_SURF_TRI];
      
   
   /* if the surface controller is open, update it */
   if (SUMA_isADO_Cont_Realized(ado))
       SUMA_Init_SurfCont_SurfParam(ado);

   /* print nodes about the closets faceset*/
   fprintf(SUMA_STDOUT, "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
   fprintf(SUMA_STDOUT, "Selected surface %s (Focus_DO_ID # %d).\n"
                        "FaceSet %ld, Closest Node %ld\n", 
      SO->Label, sv->Focus_DO_ID, PR->iAltSel[SUMA_SURF_TRI], 
      PR->datum_index);
   fprintf(SUMA_STDOUT, "Nodes forming closest FaceSet:\n");
   fprintf(SUMA_STDOUT, "%d, %d, %d\n", 
      SO->FaceSetList[ip], SO->FaceSetList[ip+1],SO->FaceSetList[ip+2]);

   fprintf (SUMA_STDOUT,"Coordinates of Nodes forming closest FaceSet:\n");
   for (it=0; it < 3; ++it) { 

      id = SO->NodeDim * SO->FaceSetList[ip+it];
      fprintf(SUMA_STDOUT, "%f, %f, %f\n", SO->NodeList[id],
                                           SO->NodeList[id+1],
                                           SO->NodeList[id+2]);
   }
   fprintf(SUMA_STDOUT, "\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");

   /* Set the Nodeselection at the closest node */
   it = PR->datum_index;
   if (!list) list = SUMA_CreateList();
   if (PR->ignore_same_datum && SO->SelectedNode == PR->datum_index) {
      SUMA_LHv("Ignoring identical node selection %d on surface %s\n",
               SO->SelectedNode, SO->Label);
      NodeIgnored = YUP;
   } else {
      ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
      SetNodeElem = SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_i, (void*)&it,
                                             SES_Suma, (void *)sv, NOPE,
                                             SEI_Head, NULL);
      if (!SetNodeElem) {
         fprintf( SUMA_STDERR,
                  "Error %s: Failed to register SetNodeElem\n", FuncName);
         SUMA_RETURN (-1);
      } else {
         SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_ngr, NULL,
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, SetNodeElem);  
      }
   }
      
      
   /* Set the FaceSetselection */
   it = PR->iAltSel[SUMA_SURF_TRI];
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
   if (!(Location = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3, (void*)PR->PickXYZ,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURN (-1);
   }
   /* and add the SO with this location, needed for VisX business*/
   SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_vp, (void *)SO,
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, Location);

   /* attach the cross hair to the selected surface */
   iv3[0] = SUMA_findSO_inDOv(SO->idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   iv3[1] = PR->datum_index;
   iv3[2] = PR->iAltSel[SUMA_SURF_TRI];
   ED = SUMA_InitializeEngineListData (SE_BindCrossHair);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_iv3, (void*)iv3,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURN (-1);
   }
      
   /* check to see if AFNI needs to be notified */
   /* Need to deal with SUMA_TO_MATLAB_STREAM_INDEX too 
      Same for remaining occurrence of SUMA_AFNI_STREAM_INDEX*/
   if (  ( SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] && 
           sv->LinkAfniCrossHair )                             ||
         ( SUMAg_CF->Connected_v[SUMA_HALLO_SUMA_LINE])        ||
         ( SUMAg_CF->Connected_v[SUMA_INSTA_TRACT_LINE])    ) {
      if (LocalHead) 
         fprintf(SUMA_STDERR,
                  "%s: Notifying Afni of CrossHair XYZ\n", FuncName);
      /* register a call to SetAfniCrossHair */
      if (!list) list = SUMA_CreateList();
      it = SUMA_ShftCont_Event(PR->evr);
      ED = SUMA_InitializeEngineListData (SE_SetAfniCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&it,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Tail, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURN (-1);
      }
      if (MASK_MANIP_MODE(sv) && SUMAg_CF->Dev) {
         SUMA_ALL_DO *ado = SUMA_whichADOg(sv->MouseMode_ado_idcode_str);
         if (ado && ado->do_type == MASK_type) {
            SUMA_MaskDO *mdo = (SUMA_MaskDO *)ado;
            ED = SUMA_InitializeEngineListData (SE_SetAfniMask);
            if (!(Location=SUMA_RegisterEngineListCommand (  list, ED, 
                                                SEF_fv3, (void*)mdo->cen,
                                                SES_Suma, (void *)sv, NOPE,
                                                SEI_Tail, NULL))) {
               SUMA_S_Err("Failed to register element\n");
               SUMA_RETURN (-1);
            }
            SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_s, (void *)(ADO_ID(ado)),
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, Location);
         }
      }
      if (!SUMA_Engine (&list)) {
         fprintf( SUMA_STDERR, 
                  "Error %s: SUMA_Engine call failed.\n", FuncName);
         SUMA_RETURN (-1);
      }
   }else {
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: No Notification to AFNI.\n", FuncName);
   }

   /* put in a request for GICOR if need be */
   if (  !NodeIgnored &&
         SUMAg_CF->Connected_v[SUMA_GICORR_LINE] && 
         SUMAg_CF->giset && !SUMAg_CF->HoldClickCallbacks) {
      if (LocalHead) 
         fprintf(SUMA_STDERR,
                  "%s: Notifying GICOR of node selection\n", FuncName);
      /* register a call to SetGICORnode */
      if (!list) list = SUMA_CreateList();
      SUMA_REGISTER_TAIL_COMMAND_NO_DATA( list, SE_SetGICORnode, 
                                          SES_Suma, sv);
      if (!SUMA_Engine (&list)) {
         fprintf( SUMA_STDERR, 
                  "Error %s: SUMA_Engine call failed.\n", FuncName);
         SUMA_RETURN (-1);
      }
   }else {
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: No Notification to GICOR.\n", FuncName);
   }
   /* now put in a request for locking cross hair but you must do 
      this after the node selection has been executed 
      NOTE: You do not always have SetNodeElem because the list might 
      get emptied in the call to AFNI notification.
      You should just put the next call at the end of the list.*/
   SUMA_LH("Cross hair locking");
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_LockCrossHair);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_iv3, (void*)iv3,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Tail, NULL)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURN (-1);
   }

   SUMA_LH("Cross hair locking Engine call");
   if (!SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURN (-1);
   }
   SUMA_LH("Returning"); 
   SUMA_RETURN (1); /* OK */
}/* determine intersection */

int SUMA_MarkLineCutplaneIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                                    int IgnoreSameNode)
{/* determine intersection */
   static char FuncName[]={"SUMA_MarkLineCutplaneIntersect"};
   float P0f[3], P1f[3];
   int NP; 
   SUMA_MT_INTERSECT_TRIANGLE *MTI = NULL, *MTIi = NULL;
   float delta_t_tmp, dmin; 
   struct timeval tt_tmp; 
   int ip, it, id, iv3[3], ii, N_SOlist, 
       SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS], imin;
   char sfield[100], sdestination[100], CommString[SUMA_MAX_COMMAND_LENGTH];
   SUMA_EngineData *ED = NULL;
   DList *list = NULL;
   DListElmt *SetNodeElem = NULL, *Location=NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_SurfaceObject **SOv = NULL;
   SUMA_VolumeObject *VO=NULL;
   SUMA_Boolean NodeIgnored = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   P0f[0] = sv->Pick0[0];
   P0f[1] = sv->Pick0[1];
   P0f[2] = sv->Pick0[2];
   P1f[0] = sv->Pick1[0];
   P1f[1] = sv->Pick1[1];
   P1f[2] = sv->Pick1[2];
   
   SUMA_LH("Getting array of pointers to clip plane surfaces");
   if (!(SOv = SUMA_TextureClipPlaneSurfaces(&N_SOlist))) {
      SUMA_LH("No clip plane surfaces");
      SUMA_RETURN(0);
   }
   imin = -1;
   dmin = 10000000.0;
   for (ii=0; ii < N_SOlist; ++ii) { /* find the closest intersection */
      if (LocalHead) 
            fprintf (SUMA_STDERR, 
                     "%s: working %d/%d clip plane ...\n", 
                     FuncName, ii, N_SOlist);
      SO = SOv[ii];
      if (SO->FaceSetDim != 3) {
         fprintf(SUMA_STDERR,
            "Error %s: "
            "SUMA_MT_intersect_triangle only works for triangular meshes.\n", 
            FuncName);
      } else {
 
         SUMA_etime (&tt_tmp, 0);
         SUMA_LH("About to call intersection function");
         
         MTIi = SUMA_MT_intersect_triangle(P0f, P1f, SO->NodeList, SO->N_Node, 
                                        SO->FaceSetList, SO->N_FaceSet, NULL, 0);

         delta_t_tmp = SUMA_etime (&tt_tmp, 1);
         if (LocalHead) 
            fprintf (SUMA_STDERR, 
               "Local Debug %s: Intersection took %f seconds.\n", 
               FuncName, delta_t_tmp);

         if (MTIi == NULL) {
            fprintf(SUMA_STDERR,
                     "Error %s: SUMA_MT_intersect_triangle failed.\n", FuncName);
            SUMA_RETURN (-1);
         }
         
         if (MTIi->N_hits) { 
            /* decide on the closest surface to the clicking point */
            if (MTIi->t[MTIi->ifacemin] < dmin) {
               if (LocalHead) 
                  fprintf (SUMA_STDERR, "%s: A minimum for surface %d.\n", 
                           FuncName, ii);
               dmin = MTIi->t[MTIi->ifacemin];
               imin = ii;
               MTI = MTIi;
            }else {     
               /* not good, toss it away */
               if (LocalHead) 
                  fprintf (SUMA_STDERR, 
                           "%s: ii=%d freeing MTIi...\n", FuncName, ii);
               MTIi = SUMA_Free_MT_intersect_triangle(MTIi); 
            }
         }else {
            /* not good, toss it away */
           if (LocalHead) 
               fprintf (SUMA_STDERR, 
                        "%s: ii=%d freeing MTIi no hits...\n", FuncName, ii);
           MTIi = SUMA_Free_MT_intersect_triangle(MTIi); 
        }
      }
    } 

   if (LocalHead) 
      fprintf (SUMA_STDERR, 
               "%s: Closest surface is indexed %d in cutplane surfaces.\n", 
               FuncName, imin);
      
   /* Mark intersection Facsets */
   if (imin >= 0) {
      SUMA_ALL_DO *ado=NULL;
      if (!(VO = SUMA_VolumeObjectOfClipPlaneSurface(SO))) {
         SUMA_S_Err("Failed to find volume object for clipped surface");
         SUMA_RETURN(-1);
      }
      VO->SelectedCutPlane = imin;
      SUMA_S_Warn("NEED TO IMPLEMENT PR THING HERE, THEN PASS IT BELOW");
      ado = (SUMA_ALL_DO *)VO;
      if (!SUMA_Add_To_PickResult_List(sv, ado, "cutplane", NULL)) {
         SUMA_S_Err("Failed to add selected ado");
         SUMA_RETURN(-1);
      }
      /* Now set this volume as the focus DO */
      sv->Focus_DO_ID = 
         SUMA_findVO_inDOv(SUMA_ADO_idcode(ado), SUMAg_DOv, SUMAg_N_DOv);

      /* if the surface controller is open, update it */
      if (SUMA_isADO_Cont_Realized(ado))
         SUMA_Init_SurfCont_SurfParam(ado);

      SUMA_UpdateViewerTitle(sv);
      
      /* if the surface controller is open, update it */
      if (SUMA_isADO_Cont_Realized(ado))
         SUMA_Init_SurfCont_SurfParam(ado);

      
      ip = SO->FaceSetDim * MTI->ifacemin;
      SUMA_S_Note("Have to decide on what to do here,\n"
                  "see equivalent section in SUMA_MarkLineSurfaceIntersect");
      SUMA_S_Warn("Weird, coords all zero");
      /* print nodes about the closets faceset*/
      fprintf(SUMA_STDOUT, "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
      fprintf(SUMA_STDOUT, "Selected cutplane surface %d .\n"
                           "FaceSet %d, Closest Node %d\n", 
         imin, MTI->ifacemin, MTI->inodemin);
      fprintf(SUMA_STDOUT, "Nodes forming closest FaceSet:\n");
      fprintf(SUMA_STDOUT, "%d, %d, %d\n", \
      SO->FaceSetList[ip], SO->FaceSetList[ip+1],SO->FaceSetList[ip+2]);

      fprintf (SUMA_STDOUT,"Coordinates of Nodes forming closest FaceSet:\n"
                           "SO->NodeDim = %d \n", SO->NodeDim);
      for (it=0; it < 3; ++it) { 
         
         id = SO->NodeDim * SO->FaceSetList[ip+it];
         fprintf(SUMA_STDOUT, "%f, %f, %f\n", SO->NodeList[id],
                                                SO->NodeList[id+1],
                                                SO->NodeList[id+2]);
      }
      fprintf(SUMA_STDOUT, "\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");

     
      
      /* Now set the cross hair position at the intersection*/
      if (!list) list = SUMA_CreateList();
      ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
      if (!(Location=SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_fv3, (void*)MTI->P,
                                             SES_Suma, (void *)sv, NOPE,
                                             SEI_Head, NULL))) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURN (-1);
      }
      /* and add the SO with this location, needed for VisX business*/
      SUMA_RegisterEngineListCommand (  list, ED, 
                                        SEF_vp, (void *)SO,
                                        SES_Suma, (void *)sv, NOPE,
                                        SEI_In, Location);

      if (!SUMA_Engine (&list)) {
         fprintf( SUMA_STDERR, 
                  "Error %s: SUMA_Engine call failed.\n", FuncName);
         SUMA_RETURN (-1);
      }
      /* check to see if AFNI needs to be notified */
      /* Need to deal with SUMA_TO_MATLAB_STREAM_INDEX too 
         Same for remaining occurrence of SUMA_AFNI_STREAM_INDEX*/
      if (  SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] && 
            sv->LinkAfniCrossHair) {
         if (LocalHead) 
            fprintf(SUMA_STDERR,
                     "%s: Notifying Afni of CrossHair XYZ\n", FuncName);
         /* register a call to SetAfniCrossHair */
         if (!list) list = SUMA_CreateList();
         it = 0; /* Might want someday: SUMA_ShftCont_Event(PR->evr); */
         ED = SUMA_InitializeEngineListData (SE_SetAfniCrossHair);
         if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_i, (void*)&it,
                                             SES_Suma, (void *)sv, NOPE,
                                             SEI_Tail, NULL)) {
            SUMA_S_Err("Failed to register element\n");
            SUMA_RETURN (-1);
         }
         if (!SUMA_Engine (&list)) {
            fprintf( SUMA_STDERR, 
                     "Error %s: SUMA_Engine call failed.\n", FuncName);
            SUMA_RETURN (-1);
         }
      }else {
         if (LocalHead) 
            fprintf(SUMA_STDERR,"%s: No Notification to AFNI.\n", FuncName);
      }
      
      /* put in a request for GICOR if need be */
      if (  !NodeIgnored &&
            SUMAg_CF->Connected_v[SUMA_GICORR_LINE] && 
            SUMAg_CF->giset && !SUMAg_CF->HoldClickCallbacks) {
         if (LocalHead) 
            fprintf(SUMA_STDERR,
                     "%s: Notifying GICOR of node selection\n", FuncName);
         /* register a call to SetGICORnode */
         if (!list) list = SUMA_CreateList();
         SUMA_REGISTER_TAIL_COMMAND_NO_DATA( list, SE_SetGICORnode, 
                                             SES_Suma, sv);
         if (!SUMA_Engine (&list)) {
            fprintf( SUMA_STDERR, 
                     "Error %s: SUMA_Engine call failed.\n", FuncName);
            SUMA_RETURN (-1);
         }
      }else {
         if (LocalHead) 
            fprintf(SUMA_STDERR,"%s: No Notification to GICOR.\n", FuncName);
      }
      
      

   } 
   /* clear MTI */
   if (MTI) {
      MTI = SUMA_Free_MT_intersect_triangle(MTI);
   }
   
   SUMA_free(SOv); SOv = NULL;
   
   if (imin >= 0) {
      SUMA_RETURN (1); /* hit */
   } else {
      SUMA_RETURN (0); /* no hit */
   }
}/* determine intersection with cutplanes*/

int SUMA_MarkLineVOslicesIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                                    int IgnoreSameNode)
{/* determine intersection */
   static char FuncName[]={"SUMA_MarkLineVOslicesIntersect"};
   SUMA_PICK_RESULT *PR = NULL;
   SUMA_ALL_DO *ado = NULL;
   int ans;
   
   SUMA_ENTRY;
   SUMA_S_Warn("Do not call me anymore. Follow the new selection logic");
   ans = SUMA_ComputeLineVOslicesIntersect(sv, dov, IgnoreSameNode, &ado);
   if (ans <= 0) {
      SUMA_RETURN(ans);
   }
   /* just for temporary testing, get PR back from list and apply it */
   PR = SUMA_Get_From_PickResult_List(sv, ado, NULL);
   ans = SUMA_Apply_PR(sv, &PR);
   SUMA_RETURN(ans);
}

#if 0
/* BEFORE you start using this MACRO everywhere, including with the, 
   other ThrMode values, make sure you write a function version of it 
   which can be used as a sanity check. For now, this seems OK */
#define SUMA_VAL_MEETS_THRESH(val, ThreshRange, ThrMode) (\
      ((ThrMode) == SUMA_LESS_THAN && (val) >= ThreshRange[0])?1: \
     (((ThrMode) == SUMA_ABS_LESS_THAN && ( (val) >=  ThreshRange[0] ||   \
                                            (val) <= -ThreshRange[0]))?1:0) )
#endif
byte SUMA_Val_Meets_Thresh(float val, double *ThreshRange, 
                           SUMA_THRESH_MODE ThrMode)
{
   static char FuncName[]={"SUMA_Val_Meets_Thresh"};
   switch(ThrMode){
      case SUMA_LESS_THAN:
         return((val >= ThreshRange[0])); 
         break;
      case SUMA_ABS_LESS_THAN:
         return((val >=  ThreshRange[0]) || (val <=  -ThreshRange[0]));
         break;
      case SUMA_THRESH_OUTSIDE_RANGE:
         return((val <  ThreshRange[0]) || (val > ThreshRange[1]));
         break;
      case SUMA_THRESH_INSIDE_RANGE:
         return((val >=  ThreshRange[0]) && (val <= ThreshRange[1]));
         break;  
      case SUMA_NO_THRESH:
         return(1);
      default:
         SUMA_S_Warn("Bad thresh mode %d", ThrMode);
         return(1);
         break;
   } 
   SUMA_S_Warn("Should not be here %d", ThrMode);
   return(1);     
}

/* 
   This function is almost identical to SUMA_ComputeLineVOvrIntersect()
   They could be merged quite readily but for some reason this feels 
   cleaner to me. 
   Make sure that any change here is mirrored verbatim (to the degree possible)  
   in SUMA_ComputeLineVOvrIntersect() .
   
   Consider merging SUMA_ComputeLineVOvrIntersect() into 
   SUMA_ComputeLineVOslicesIntersect() in the future if maintenance is a problem 
*/
int SUMA_ComputeLineVOslicesIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                                       int IgnoreSameNode, SUMA_ALL_DO **pado)
{/* determine intersection */
   static char FuncName[]={"SUMA_ComputeLineVOslicesIntersect"};
   float P0f[3], P1f[3], pinter[3], I[3];
   int NP, N_Hit, okinten=0; 
   float delta_t_tmp, dmin, val; 
   struct timeval tt_tmp; 
   int ip, it, id, ii, imin, I1d, Irw, Hit, ive, icolplane, indef=-1;
   int *MembDOs=NULL, N_MembDOs, UseAlphaTresh=1;
   float valpha=0.0;
   SUMA_DO_Types ttv[12];
   char sfield[100], sdestination[100], CommString[SUMA_MAX_COMMAND_LENGTH];
   SUMA_VolumeObject *VO=NULL;
   SUMA_Boolean NodeIgnored = NOPE;
   SUMA_RENDERED_SLICE *rslc;
   SUMA_ALL_DO *ado = NULL;
   SUMA_DSET *dset = NULL;
   SUMA_OVERLAYS *colplane=NULL;
   SUMA_VOL_SAUX *VSaux = NULL;
   SUMA_PICK_RESULT *PR=NULL;
   DListElmt *el=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   P0f[0] = sv->Pick0[0];
   P0f[1] = sv->Pick0[1];
   P0f[2] = sv->Pick0[2];
   P1f[0] = sv->Pick1[0];
   P1f[1] = sv->Pick1[1];
   P1f[2] = sv->Pick1[2];

   ttv[0] = VO_type; ttv[1] = NOT_SET_type;
   MembDOs = SUMA_ViewState_Membs(&(sv->VSv[sv->iState]), ttv, &N_MembDOs);
   SUMA_LHv("Searching for hit: %f %f %f --> %f %f %f\n", 
            P0f[0], P0f[1], P0f[2], P1f[0], P1f[1], P1f[2]);   
   N_Hit = 0;
   for (ii=0; ii<N_MembDOs; ++ii) {
      {
         VO = (SUMA_VolumeObject *)(dov[MembDOs[ii]].OP);
         ado = (SUMA_ALL_DO *)VO;
         if (!(VSaux = SUMA_ADO_VSaux(ado))) continue;
         SUMA_LH("%d slices on %s", dlist_size(VSaux->slcl), ADO_LABEL(ado));
         if (!dlist_size(VSaux->slcl)) continue;
         /* now compute intersection from the top down */
         Hit = 0; 
         el = NULL;
         do {
            if (!el) el = dlist_head(VSaux->slcl);
            else el = dlist_next(el);
            rslc = (SUMA_RENDERED_SLICE *)el->data;
            /* does line intersect this plane? */
            SUMA_SEGMENT_PLANE_INTERSECT(P0f, P1f, rslc->Eq, Hit, pinter);
            if (Hit) {/* is the intersection point in the volume? */
               Hit = 0; /* demote, real hit decided on below */
               ive = 0;
               while (VO->VE && VO->VE[ive]) {
                  AFF44_MULT_I(I, VO->VE[ive]->X2I, pinter);
                  SUMA_LH("On %s: Inter at X=[%f %f %f] --> ijk=[%f %f %f]", 
                           SUMA_VE_Headname(VO->VE, ive),
                           pinter[0], pinter[1], pinter[2], I[0], I[1], I[2]);
                  I[0] = (int)I[0]; I[1] = (int)I[1]; I[2] = (int)I[2];
                  if (I[0] >= 0.0f && I[1] >= 0.0f && I[2] >= 0.0f &&
                      I[0] < VO->VE[ive]->Ni &&  I[1] < VO->VE[ive]->Nj &&
                      I[2] < VO->VE[ive]->Nk) {
                      dset = SUMA_VE_dset(VO->VE, ive);
                      colplane =  SUMA_Fetch_OverlayPointerByDset(
                                          (SUMA_ALL_DO *)VO, dset, &icolplane); 
                      /* here you check on the value at I in the dataset */
                      I1d = I[2]*VO->VE[ive]->Ni*VO->VE[ive]->Nj + 
                            I[1]*VO->VE[ive]->Ni+I[0];
                      Irw = SUMA_GetNodeRow_FromNodeIndex_eng(dset, I1d,-1);
                      if (!colplane->V) {
                        SUMA_S_Err("Need SUMA_GetDsetValInCol to get vals");
                        SUMA_RETURN(NOPE);
                      } else {
                        val = colplane->V[Irw];
                      }
                      SUMA_LH("Have intersection on ive %d inside VE %s\n"
                              "IJK [%d %d %d], I1d=%d, Irw=%d, \n"
                              "val %f, thr [%f %f]\n",
                              ive, SUMA_VE_Headname(VO->VE, ive), 
                              (int)I[0], (int)I[1], (int)I[2], 
                              I1d, Irw, val,
                              colplane->OptScl->ThreshRange[0], 
                              colplane->OptScl->ThreshRange[1]);
                      
                      /* Do we meet intensity thresholds? */
                      okinten = 0;
                      if ( SUMA_Val_Meets_Thresh(val, 
                                    colplane->OptScl->ThreshRange,
                                    colplane->OptScl->ThrMode ) && 
                           (val != 0.0f || !colplane->OptScl->MaskZero)) {
                        okinten = 1;
                      }
                      
                      indef = -1;
                      UseAlphaTresh = 1;/* control from interface someday */
                      valpha = 2.0; /* no masking */
                      if (UseAlphaTresh && okinten && colplane->ColAlpha) {
                        /* Also mask if value is below alpha thresh 
                           This is a slow search... So you may not want
                           to use it all the time. 
                           Problem is finding the row of the voxel in
                           NodeDef, and that's too slow for a big 
                           volume to be run repeatedly...Would
                           be easier if I had a function to recompute
                           a voxel's alpha, rather than search for it
                           in ColAlpha. Oh, well, someday I guess. For
                           now we search*/
                        if ((indef=SUMA_GetSortedNodeOverInd(colplane, I1d))>=0){
                           valpha = colplane->ColAlpha[indef]/255.0;
                        }
                      }
                      
                      if ( okinten && (valpha > colplane->AlphaThresh) ) {
                        SUMA_LH("FOUND IT, on VE %s, IJK [%d %d %d], val %f," 
                               "thresh[%f %f], UseAlphaTresh = %d, "
                               "valpha=%f, ColAlphaThresh=%f\n",
                                   SUMA_VE_Headname(VO->VE, ive), 
                                   (int)I[0], (int)I[1], (int)I[2], val,
                                   colplane->OptScl->ThreshRange[0], 
                                   colplane->OptScl->ThreshRange[1],
                                   UseAlphaTresh,
                                   valpha, colplane->AlphaThresh*255);
                           PR = SUMA_New_Pick_Result(NULL);
                           PR->ado_idcode_str = SUMA_replace_string(
                                           PR->ado_idcode_str, ADO_ID(ado));
                           if (pado) *pado = ado; /* user wants it */
                           PR->primitive = SUMA_replace_string(
                                                         PR->primitive,"voxel");
                           PR->primitive_index = -1;
                           PR->PickXYZ[0] = pinter[0];
                           PR->PickXYZ[1] = pinter[1];
                           PR->PickXYZ[2] = pinter[2];
                           PR->ignore_same_datum = IgnoreSameNode;
                           PR->datum_index = I1d;
                           PR->iAltSel[SUMA_VOL_I] = I[0];
                           PR->iAltSel[SUMA_VOL_J] = I[1];
                           PR->iAltSel[SUMA_VOL_K] = I[2];
                           PR->iAltSel[SUMA_VOL_IJK] = I1d;
                           PR->iAltSel[SUMA_VOL_SLC_NUM] = rslc->slc_num;
                           PR->iAltSel[SUMA_VOL_SLC_VARIANT] = 
                                 (int)SUMA_SlcVariantToCode(rslc->variant);
                           PR->dAltSel[SUMA_VOL_SLC_EQ0] = rslc->Eq[0];
                           PR->dAltSel[SUMA_VOL_SLC_EQ1] = rslc->Eq[1];
                           PR->dAltSel[SUMA_VOL_SLC_EQ2] = rslc->Eq[2];
                           PR->dAltSel[SUMA_VOL_SLC_EQ3] = rslc->Eq[3];
                           PR->dset_idcode_str = SUMA_replace_string(
                                           PR->dset_idcode_str, SDSET_ID(dset));
                           if (!SUMA_Add_To_PickResult_List(sv, ado, 
                                                            "voxel", &PR)) {
                              SUMA_S_Err("Failed to add selected ado");
                              SUMA_RETURN(0);
                           }
                           Hit = 1;
                           ++N_Hit; 
                           /* You could leave at the first hit IF:
                           you only have one direction of slices in the 
                           entire stack, AND if they are properly
                           ordered for rendering.
                           Should speed be an issue you can check for
                           this condition and bolt with the line below */
                           /* goto GOT_IT; */
                      }
                  }
                  ++ive;
               }
            }
            SUMA_LH("el now %p,\n"
                    "tail = %p, N_Hit = %d", 
                    el, dlist_tail(VSaux->slcl), N_Hit);
         } while(el != dlist_tail(VSaux->slcl));
      }
   }


   GOT_IT:
   SUMA_RETURN(N_Hit);
}/* determine intersection with slices of VO*/

/* 
   This function is almost identical to SUMA_ComputeLineVOslicesIntersect()
   They could be merged quite readily but for some reason this feels 
   cleaner to me. 
   Make sure that any change here is mirrored verbatim (to the degree possible)  
   in SUMA_ComputeLineVOslicesIntersect() .
   
   Consider merging SUMA_ComputeLineVOvrIntersect() into 
   SUMA_ComputeLineVOslicesIntersect() in the future if maintenance is a problem 


*/
int SUMA_ComputeLineVOvrIntersect (SUMA_SurfaceViewer *sv, SUMA_DO *dov, 
                                   int IgnoreSameNode, SUMA_ALL_DO **pado)
{/* determine intersection */
   static char FuncName[]={"SUMA_ComputeLineVOvrIntersect"};
   float P0f[3], P1f[3], pinter[3], I[3];
   int NP, N_Hit, okinten=0; 
   float delta_t_tmp, dmin, val; 
   struct timeval tt_tmp; 
   int ip, it, id, ii, imin, I1d, Irw, Hit, ive, icolplane, indef=-1;
   int *MembDOs=NULL, N_MembDOs, UseAlphaTresh=1;
   float valpha=0.0;
   SUMA_DO_Types ttv[12];
   char sfield[100], sdestination[100], CommString[SUMA_MAX_COMMAND_LENGTH];
   SUMA_VolumeObject *VO=NULL;
   SUMA_Boolean NodeIgnored = NOPE;
   SUMA_RENDERED_SLICE *rslc;
   SUMA_ALL_DO *ado = NULL;
   SUMA_DSET *dset = NULL;
   SUMA_OVERLAYS *colplane=NULL;
   SUMA_VOL_SAUX *VSaux = NULL;
   SUMA_PICK_RESULT *PR=NULL;
   DListElmt *el=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   P0f[0] = sv->Pick0[0];
   P0f[1] = sv->Pick0[1];
   P0f[2] = sv->Pick0[2];
   P1f[0] = sv->Pick1[0];
   P1f[1] = sv->Pick1[1];
   P1f[2] = sv->Pick1[2];

   ttv[0] = VO_type; ttv[1] = NOT_SET_type;
   MembDOs = SUMA_ViewState_Membs(&(sv->VSv[sv->iState]), ttv, &N_MembDOs);
   SUMA_LHv("Searching for hit: %f %f %f --> %f %f %f\n", 
            P0f[0], P0f[1], P0f[2], P1f[0], P1f[1], P1f[2]);   
   N_Hit = 0;
   for (ii=0; ii<N_MembDOs; ++ii) {
      {
         VO = (SUMA_VolumeObject *)(dov[MembDOs[ii]].OP);
         ado = (SUMA_ALL_DO *)VO;
         if (!(VSaux = SUMA_ADO_VSaux(ado))) continue;
         SUMA_LH("%d VR slices on %s, show=%d, VrSelect=%d", 
                              dlist_size(VSaux->vrslcl), 
                              ADO_LABEL(ado), VSaux->ShowVrSlc, 
                              VSaux->VrSelect);
         if (!VSaux->ShowVrSlc || !VSaux->VrSelect) continue;
         if (!dlist_size(VSaux->vrslcl)) continue;
         /* now compute intersection from the top down */
         Hit = 0; 
         el = NULL;
         do {
            if (!el) el = dlist_head(VSaux->vrslcl);
            else el = dlist_next(el);
            rslc = (SUMA_RENDERED_SLICE *)el->data;
            /* does line intersect this plane? */
            SUMA_SEGMENT_PLANE_INTERSECT(P0f, P1f, rslc->Eq, Hit, pinter);
            if (Hit) {/* is the intersection point in the volume? */
               Hit = 0; /* demote, real hit decided on below */
               ive = 0;
               while (VO->VE && VO->VE[ive]) {
                  AFF44_MULT_I(I, VO->VE[ive]->X2I, pinter);
                  SUMA_LH("On %s: Inter at X=[%f %f %f] --> ijk=[%f %f %f]", 
                           SUMA_VE_Headname(VO->VE, ive),
                           pinter[0], pinter[1], pinter[2], I[0], I[1], I[2]);
                  I[0] = (int)I[0]; I[1] = (int)I[1]; I[2] = (int)I[2];
                  if (I[0] >= 0.0f && I[1] >= 0.0f && I[2] >= 0.0f &&
                      I[0] < VO->VE[ive]->Ni &&  I[1] < VO->VE[ive]->Nj &&
                      I[2] < VO->VE[ive]->Nk) {
                      dset = SUMA_VE_dset(VO->VE, ive);
                      colplane =  SUMA_Fetch_OverlayPointerByDset(
                                          (SUMA_ALL_DO *)VO, dset, &icolplane); 
                      /* here you check on the value at I in the dataset */
                      I1d = I[2]*VO->VE[ive]->Ni*VO->VE[ive]->Nj + 
                            I[1]*VO->VE[ive]->Ni+I[0];
                      Irw = SUMA_GetNodeRow_FromNodeIndex_eng(dset, I1d,-1);
                      if (!colplane->V) {
                        SUMA_S_Err("Need SUMA_GetDsetValInCol to get vals");
                        SUMA_RETURN(NOPE);
                      } else {
                        val = colplane->V[Irw];
                      }
                      SUMA_LH("Have intersection on ive %d inside VE %s\n"
                              "IJK [%d %d %d], I1d=%d, Irw=%d, \n"
                              "val %f, thr [%f %f]\n",
                              ive, SUMA_VE_Headname(VO->VE, ive), 
                              (int)I[0], (int)I[1], (int)I[2], 
                              I1d, Irw, val,
                              colplane->OptScl->ThreshRange[0], 
                              colplane->OptScl->ThreshRange[1]);
                      
                      /* Do we meet intensity thresholds? */
                      okinten = 0;
                      if ( SUMA_Val_Meets_Thresh(val, 
                                    colplane->OptScl->ThreshRange,
                                    colplane->OptScl->ThrMode ) && 
                           (val != 0.0f || !colplane->OptScl->MaskZero)) {
                        okinten = 1;
                      }
                      
                      indef = -1;
                      UseAlphaTresh = 1;/* control from interface someday */
                      valpha = 2.0; /* no masking */
                      if (UseAlphaTresh && okinten && colplane->ColAlpha) {
                        /* Also mask if value is below alpha thresh 
                           This is a slow search... So you may not want
                           to use it all the time. 
                           Problem is finding the row of the voxel in
                           NodeDef, and that's too slow for a big 
                           volume to be run repeatedly...Would
                           be easier if I had a function to recompute
                           a voxel's alpha, rather than search for it
                           in ColAlpha. Oh, well, someday I guess. For
                           now we search*/
                        if ((indef=SUMA_GetSortedNodeOverInd(colplane, I1d))>=0){
                           valpha = colplane->ColAlpha[indef]/255.0;
                        }
                      }
                      
                      if ( okinten && (valpha > colplane->AlphaThresh) ) {
                        SUMA_LH("FOUND IT, on VE %s, IJK [%d %d %d], val %f," 
                               "thresh[%f %f], UseAlphaTresh = %d, "
                               "valpha=%f, ColAlphaThresh=%f\n",
                                   SUMA_VE_Headname(VO->VE, ive), 
                                   (int)I[0], (int)I[1], (int)I[2], val,
                                   colplane->OptScl->ThreshRange[0], 
                                   colplane->OptScl->ThreshRange[1],
                                   UseAlphaTresh,
                                   valpha, colplane->AlphaThresh*255);
                           PR = SUMA_New_Pick_Result(NULL);
                           PR->ado_idcode_str = SUMA_replace_string(
                                           PR->ado_idcode_str, ADO_ID(ado));
                           if (pado) *pado = ado; /* user wants it */
                           PR->primitive = SUMA_replace_string(
                                                         PR->primitive,"voxel");
                           PR->primitive_index = -1;
                           PR->PickXYZ[0] = pinter[0];
                           PR->PickXYZ[1] = pinter[1];
                           PR->PickXYZ[2] = pinter[2];
                           PR->ignore_same_datum = IgnoreSameNode;
                           PR->datum_index = I1d;
                           PR->iAltSel[SUMA_VOL_I] = I[0];
                           PR->iAltSel[SUMA_VOL_J] = I[1];
                           PR->iAltSel[SUMA_VOL_K] = I[2];
                           PR->iAltSel[SUMA_VOL_IJK] = I1d;
                           PR->iAltSel[SUMA_VOL_SLC_NUM] = rslc->slc_num;
                           PR->iAltSel[SUMA_VOL_SLC_VARIANT] = 
                                 (int)SUMA_SlcVariantToCode(rslc->variant);
                           PR->dAltSel[SUMA_VOL_SLC_EQ0] = rslc->Eq[0];
                           PR->dAltSel[SUMA_VOL_SLC_EQ1] = rslc->Eq[1];
                           PR->dAltSel[SUMA_VOL_SLC_EQ2] = rslc->Eq[2];
                           PR->dAltSel[SUMA_VOL_SLC_EQ3] = rslc->Eq[3];
                           PR->dset_idcode_str = SUMA_replace_string(
                                           PR->dset_idcode_str, SDSET_ID(dset));
                           if (!SUMA_Add_To_PickResult_List(sv, ado, 
                                                            "voxel", &PR)) {
                              SUMA_S_Err("Failed to add selected ado");
                              SUMA_RETURN(0);
                           }
                           Hit = 1;
                           ++N_Hit; 
                           /* You could leave at the first hit IF:
                           you only have one direction of slices in the 
                           entire stack, AND if they are properly
                           ordered for rendering.
                           Should speed be an issue you can check for
                           this condition and bolt with the line below */
                           /* goto GOT_IT; */
                      }
                  }
                  ++ive;
               }
            }
            SUMA_LH("el now %p,\n"
                    "tail = %p, N_Hit = %d", 
                    el, dlist_tail(VSaux->vrslcl), N_Hit);
         } while(el != dlist_tail(VSaux->vrslcl));
      }
   }


   GOT_IT:
   SUMA_RETURN(N_Hit);
}/* determine intersection with 3D rendering of VO*/

int SUMA_Apply_PR_VO(SUMA_SurfaceViewer *sv, SUMA_VolumeObject *VO, 
                     SUMA_PICK_RESULT **PRi) 
{
   static char FuncName[]={"SUMA_Apply_PR_VO"};
   SUMA_ALL_DO *ado=NULL;
   int iv3[3], iv15[15];
   float fv15[15];
   DList *list = NULL;
   SUMA_Boolean NodeIgnored = NOPE;
   SUMA_PICK_RESULT *PR;
   SUMA_EngineData *ED = NULL;
   SUMA_DSET *dset=NULL;
   DListElmt *Location=NULL, *el=NULL, *SetNodeElem = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Here");
   if (!sv || !VO || !PRi || !*PRi) { SUMA_S_Err("Niente"); SUMA_RETURN(-1); }
    
   /* Mark intersection Facsets */
   ado = (SUMA_ALL_DO *)VO;

   PR = *PRi;   /* Keep local copy */
   /* Store the PR in ado, hide it from return potential */
   SUMA_ADO_StorePickResult(ado, PRi);
   
   if (!(dset = SUMA_FindDset_s(PR->dset_idcode_str, SUMAg_CF->DsetList))) {
      SUMA_S_Err("NULL dset?");
      SUMA_RETURN(0);
   }
   
   sv->Focus_DO_ID = ADO_iDO(ado);
   SUMA_UpdateViewerTitle(sv);

   /* if the surface controller is open, update it */
   if (SUMA_isADO_Cont_Realized(ado))
      SUMA_Init_SurfCont_SurfParam(ado);

   fprintf(SUMA_STDOUT, "\nvvvvvvvvvvvvvvvvvvvvvvvvvvvv\n");
   fprintf(SUMA_STDOUT, "Selected voxel RAI [%.3f %.3f %.3f]mm \n"
                        "               IJK [%ld %ld %ld] on volume %s.\n",
      PR->PickXYZ[0], PR->PickXYZ[1], PR->PickXYZ[2],
      PR->iAltSel[SUMA_VOL_I], PR->iAltSel[SUMA_VOL_J], PR->iAltSel[SUMA_VOL_K],
      SDSET_FILENAME(dset));
   fprintf(SUMA_STDOUT, "\n^^^^^^^^^^^^^^^^^^^^^^^^^^^^\n");
   
   
   /* Set the voxel selection */   
   if (!list) list = SUMA_CreateList();
   if (PR->ignore_same_datum && 
        SUMA_ADO_SelectedDatum(ado, NULL, NULL) == PR->datum_index) {
      SUMA_LHv("Ignoring identical voxel selection %d on volume %s\n",
               SUMA_ADO_SelectedDatum(ado, NULL, NULL), SUMA_ADO_Label(ado));
      NodeIgnored = YUP;
   } else {
      ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
      SetNodeElem = SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_i, (void*)&PR->datum_index,
                                             SES_Suma, (void *)sv, NOPE,
                                             SEI_Head, NULL);
      if (!SetNodeElem) {
         fprintf( SUMA_STDERR,
                  "Error %s: Failed to register SetNodeElem\n", FuncName);
         SUMA_RETURN (-1);
      } else {
         SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_ngr, NULL,
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, SetNodeElem);  
      }
      
      iv15[SUMA_VOL_I] = (int)PR->iAltSel[SUMA_VOL_I];
      iv15[SUMA_VOL_J] = (int)PR->iAltSel[SUMA_VOL_J];
      iv15[SUMA_VOL_K] = (int)PR->iAltSel[SUMA_VOL_K];
      iv15[SUMA_VOL_IJK] = (int)PR->iAltSel[SUMA_VOL_IJK];
      
      fv15[SUMA_VOL_SLC_EQ0] = (float)PR->dAltSel[SUMA_VOL_SLC_EQ0];
      fv15[SUMA_VOL_SLC_EQ1] = (float)PR->dAltSel[SUMA_VOL_SLC_EQ1];
      fv15[SUMA_VOL_SLC_EQ2] = (float)PR->dAltSel[SUMA_VOL_SLC_EQ2];
      fv15[SUMA_VOL_SLC_EQ3] = (float)PR->dAltSel[SUMA_VOL_SLC_EQ3];
      
      SUMA_RegisterEngineListCommand (  list, ED, 
                                        SEF_iv15, (void *)iv15,
                                        SES_Suma, (void *)sv, NOPE,
                                        SEI_In, SetNodeElem);
      SUMA_RegisterEngineListCommand (  list, ED, 
                                        SEF_fv15, (void *)fv15,
                                        SES_Suma, (void *)sv, NOPE,
                                        SEI_In, SetNodeElem);
   }
      

   /* Now set the cross hair position at the selected node*/
   if (!list) list = SUMA_CreateList();
   ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
   if (!(Location=SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3, (void*)PR->PickXYZ,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURN(-1);                                          
   } 
   /* and add the object with this location */
   SUMA_RegisterEngineListCommand (  list, ED, 
                                     SEF_vp, (void *)dset,
                                     SES_Suma, (void *)sv, NOPE,
                                     SEI_In, Location);

   /* attach the cross hair to the selected object 
   Note that binding here is to voxel of dset */
   iv3[0] = ADO_iDO(ado);
   iv3[1] = PR->iAltSel[SUMA_VOL_IJK];
   iv3[2] = -1;

   ED = SUMA_InitializeEngineListData (SE_BindCrossHair);
   if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_iv3, (void*)iv3,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL)) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURN(-1);
   }   

   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURN(-1);
   }

   /* check to see if AFNI needs to be notified */
   if (SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] && 
       sv->LinkAfniCrossHair) {
      int it;
      SUMA_LH("Notifying Afni of CrossHair XYZ");
      /* register a call to SetAfniCrossHair */
      if (!list) list = SUMA_CreateList();
      it = SUMA_ShftCont_Event(PR->evr);
      ED = SUMA_InitializeEngineListData (SE_SetAfniCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&it,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Tail, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURN (-1);
      }
      if (MASK_MANIP_MODE(sv) && SUMAg_CF->Dev) {
         SUMA_ALL_DO *ado = SUMA_whichADOg(sv->MouseMode_ado_idcode_str);
         DListElmt *Location=NULL;
         if (ado && ado->do_type == MASK_type) {
            SUMA_MaskDO *mdo = (SUMA_MaskDO *)ado;
            ED = SUMA_InitializeEngineListData (SE_SetAfniMask);
            if (!(Location=SUMA_RegisterEngineListCommand (  list, ED, 
                                                SEF_fv3, (void*)mdo->cen,
                                                SES_Suma, (void *)sv, NOPE,
                                                SEI_Tail, NULL))) {
               SUMA_S_Err("Failed to register element\n");
               SUMA_RETURN (-1);
            }
            SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_s, (void *)(ADO_ID(ado)),
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, Location);
         }
      }
      if (!SUMA_Engine (&list)) {
         fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
         SUMA_RETURN(-1);
      }
   }

   SUMA_RETURN(1);
}

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
	
   #ifdef DARWIN
      SUMA_S_Warn("Calling this function from OS X seems to cause trouble");
   #endif
   
   /* using sv->X->CMAP instead of 
	         DefaultColormapOfScreen(XtScreen(sv->X->GLXAREA))
		is useless */
   if (!XAllocNamedColor (sv->X->DPY, 
               DefaultColormapOfScreen(XtScreen(sv->X->GLXAREA)),
               Color, &col, &unused)) {
      fprintf (SUMA_STDERR, 
            "Error %s: Can't allocate for %s color.\n", FuncName, Color);
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
												 
	- NB: This function used to crash when run on SGI if display is not 
   in TrueColor mode.
	This happens even though the visual chosen by SUMA does not change.
	To put the SGI in true color mode, you need to add to /var/X11/xdm/Xservers
	the following:  -class TrueColor -depth 24
	and then restart X or the system.
   The bug was that the graphics context (sv->X->gc) was created using the 
   Screen's root window and not the GLX visual's window. 
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

   SUMA_ENTRY;
   
   SO = SUMA_SV_Focus_SO(sv);

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
      if (  SUMA_isdROIrelated(SUMAg_CF->X->DrawROI->curDrawnROI, 
                                                   (SUMA_ALL_DO *)SO) && 
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
      if (!(Overlay = SUMA_Fetch_OverlayPointer((SUMA_ALL_DO *)SO, 
                                                DrawnROI->ColPlaneName, 
                                                &junk))) {
         SUMA_S_Err("Unexpected! Could not find overlay pointer");
      } else {
         /* if the current col plane is not the same as this one, 
            do the switching please */
         SUMA_InitializeColPlaneShell((SUMA_ALL_DO *)SO, Overlay);
         SUMA_UpdateColPlaneShellAsNeeded((SUMA_ALL_DO *)SO); 
                                          /* update other open 
                                             ColPlaneShells */
      }
   }

   SUMA_RETURN(DrawnROI);
}

/*!
   Function that turns a brushstroke to a series of nodes on the surface.
   
   No surface paths are created from one node to the next yet.
   
   It is not always the case that BrushStroke->N_SurfNodes is equal 
   to BrushStroke->N
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
   
   if (!(SO = SUMA_SV_Focus_SO(sv))) {
      SUMA_S_Err("No surface in focus");
      SUMA_RETURN(NOPE);
   }
   
   /* ONLY WORK ON FocusSO */
   if (SO->FaceSetDim != 3) {
      SUMA_S_Err("SUMA_MT_intersect_triangle only works for triangular meshes.");
      SUMA_RETURN(NOPE);
   }
   
   N = dlist_size(sv->BS);
   if (!N) {
      fprintf (SUMA_STDERR, "%s: Empty brushstroke, nothing to do.\n", FuncName);
      SUMA_RETURN(NOPE);
   }else SUMA_LHv("%d element(s) in sv->BS.\n", N);
   
   /* the first node of the brushstroke is stored as the cross hair's node id, 
      just copy it */ 
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
                                                MTI,0);

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
      int IncTri[100], N_IncTri=0, n1=-1, n2=-1, n3=-1, ni = -1, 
          ti = -1, N_Neighb=0,DeciLevel = 0, i, j, Removed=0;
      int DeciReentry=0, UsedNode[3]={ 0 , 0, 0 };
      SUMA_BRUSH_STROKE_DATUM *bsdi=NULL, *bsdn=NULL, *bsd_deci=NULL;
      SUMA_Boolean  DoesInters=NOPE; /* flag for Decimation mode */
      SUMA_Boolean  TrackOscillation = YUP; /* flag to tracking 
                                               algorithm oscillation */
      SUMA_Boolean  TryBruteForce = NOPE;
      int *Visited = NULL;
      
      if (TrackOscillation) {
         Visited = (int *)SUMA_calloc(SO->N_Node, sizeof(int)); 
         if (!Visited) {
            SUMA_SLP_Err("Failed to allocate for Visited.\n");
            SUMA_RETURN(NOPE);
         }
      }
      
      Eli = Elmt; /* initialize current element to the 
                     very fist in the brushstroke */
      MTI = NULL;
      TryBruteForce = NOPE;
      do {   
         bsdi = (SUMA_BRUSH_STROKE_DATUM *)Eli->data;
         n1 = bsdi->SurfNode;
         
         Eln = Eli->next; /* get the next element in line */
         bsdn = (SUMA_BRUSH_STROKE_DATUM *)Eln->data;
         
         if (LocalHead) 
            fprintf(SUMA_STDERR,"%s: Working from node %d.\n", FuncName, n1);

         if (!TryBruteForce) { /* try the fast method */
            N_Neighb = SO->FN->N_Neighb[n1];
            if (N_Neighb < 3) {
               /* nothing found */
               SUMA_SLP_Err ("Node has less than 3 neighbors.\n"
                             "This method will not apply.");
               SUMA_RETURN(NOPE);
            }

            /* does the ray formed by Eln's NP and FP hit any of 
               the triangles incident to bsdi->SurfNode (or n1) ? */
            if (LocalHead) 
               fprintf (SUMA_STDERR, 
                        "%s: Searching incident triangles:\n", FuncName);
            i=0;
            DoesInters = NOPE;
            while ((i < N_Neighb ) && (!DoesInters)) { 
               n2 = SO->FN->FirstNeighb[n1][i];
               if ( i+1 == N_Neighb) n3 = SO->FN->FirstNeighb[n1][0];
               else n3 = SO->FN->FirstNeighb[n1][i+1];
               #if 0
                  if (LocalHead) {
                     fprintf (SUMA_STDERR, " %d: [%d %d %d] Tri %d\n", 
                        i, n1, n2, n3, SUMA_whichTri(SO->EL, n1, n2, n3, 1));
                     fprintf (SUMA_STDERR, " %d: [%.2f, %.2f, %.2f]\n", 
                        n1, SO->NodeList[3*n1], 
                            SO->NodeList[3*n1+1], SO->NodeList[3*n1+2]);
                     fprintf (SUMA_STDERR, " %d: [%.2f, %.2f, %.2f]\n", 
                        n2, SO->NodeList[3*n2], 
                            SO->NodeList[3*n2+1], SO->NodeList[3*n2+2]);
                     fprintf (SUMA_STDERR, " %d: [%.2f, %.2f, %.2f]\n", 
                        n3, SO->NodeList[3*n3], 
                            SO->NodeList[3*n3+1], SO->NodeList[3*n3+2]);
                     fprintf (SUMA_STDERR, 
                        " NP: [%.2f, %.2f, %.2f] FP: [%.3f, %.2f, %.2f]\n", 
                              bsdn->NP[0], bsdn->NP[1], bsdn->NP[2], 
                              bsdn->FP[0], bsdn->FP[1], bsdn->FP[2]);
                  }
               #endif
               DoesInters = SUMA_MT_isIntersect_Triangle (bsdn->NP, bsdn->FP, 
                                 &(SO->NodeList[3*n1]),
                                 &(SO->NodeList[3*n2]), 
                                 &(SO->NodeList[3*n3]), ip, d, &ni);
               if (DoesInters) {
                  if (ni == 0) ni = n1;
                  else if (ni == 1) ni = n2;
                  else ni = n3;

                  ti = SUMA_whichTri(SO->EL, n1, n2, n3, 1, 0);
               }

               #if 0
                  if (LocalHead) 
                     fprintf (SUMA_STDERR, 
                              "%s: DoesInters = %d, ni = %d\n", 
                              FuncName, DoesInters, ni);
                  {
                     /* for debuging */                                           
                     MTI = NULL;MTI = 
                        SUMA_MT_intersect_triangle(   bsdn->NP, bsdn->FP, 
                                                      SO->NodeList, SO->N_Node, 
                                                      SO->FaceSetList, 
                                                      SO->N_FaceSet, 
                                                      MTI, 0);
                     fprintf (SUMA_STDERR, 
                        "%s: Intersection would be with triangle %d, node %d\n", 
                              FuncName, MTI->ifacemin, MTI->inodemin);                                 
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
                                    MTI, 0);

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
SUMA_ROI_DATUM *SUMA_LinkThisNodeToNodeInStroke (SUMA_SurfaceViewer *sv, 
                                          int NonSurf, DListElmt *ELinStroke)
{
   static char FuncName[]={"SUMA_LinkThisNodeToNodeInStroke"};
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ROI_DATUM *ROId=NULL;
   SUMA_SurfaceObject *SO=NULL;
   int Nfrom, Nto;
   SUMA_BRUSH_STROKE_DATUM *bsd=NULL;
   
   SUMA_ENTRY;
   
   if (!(SO = SUMA_SV_Focus_SO(sv))) {
      SUMA_S_Err("No SO in focus");
      SUMA_RETURN(NULL);
   }
   
   Nfrom = NonSurf;
   bsd = (SUMA_BRUSH_STROKE_DATUM *)ELinStroke->data;
   Nto = bsd->SurfNode;
   
   /* Now compute the intersection of the surface with the plane */
   ROId = SUMA_Surf_Plane_Intersect_ROI (SO, Nfrom, Nto, bsd->NP);
   
   if (!ROId) {
      SUMA_S_Err("Failed to link tail node to first node in new stroke.\n"
                 "Repeat new stroke.");
      SUMA_RETURN(NULL);
   }
   
   SUMA_RETURN(ROId);
}

/*!
   \brief Function to link a node on the surface to the first node 
   of a NodeStroke
   
   -This function returns an ROI_datum that represents the link between 
   the last node visited and the first node of the Nodestroke
   
   \sa SUMA_LinkThisNodeToNodeInStroke 
*/
SUMA_ROI_DATUM *SUMA_LinkTailNodeToNodeStroke ( SUMA_SurfaceViewer *sv, 
                                                SUMA_DRAWN_ROI *DrawnROI)
{
 
   static char FuncName[]={"SUMA_LinkTailNodeToNodeStroke"};
   SUMA_ROI_DATUM *ROId=NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   int Nfrom=-1, Nto=-1, Trito=-1;
   DListElmt *Elm=NULL;
   SUMA_BRUSH_STROKE_DATUM *bsd=NULL;
   
   SUMA_ENTRY;
   
   if (!(SO = SUMA_SV_Focus_SO(sv))) {
      SUMA_S_Err("No SO in focus");
      SUMA_RETURN(NULL);
   }

   /* get the equation of the plane fromed by TailNode, 
      FirstNodeinBrushStroke and its NearPlanePoint */
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
      SUMA_S_Err("Failed to link tail node to first node in new stroke.\n"
                 "Repeat new stroke.");
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
   
   if (!(SO = SUMA_SV_Focus_SO(sv))) {
      SUMA_S_Err("No SO in focus");
      SUMA_RETURN(NULL);
   }
   
   ROId = SUMA_AllocROIDatum();

   /* fill up node series here */
   ROId->N_n = 1;
   ROId->N_t = 1;
   ROId->nPath = (int *) SUMA_calloc (ROId->N_n, sizeof(int));
   ROId->tPath = (int *) SUMA_calloc (ROId->N_t, sizeof(int));
   
   SUMA_BS_FIRST_SURF_NODE(sv->BS, ROId->nPath[0], ROId->tPath[0], Elmt);
   ROId->Type = SUMA_ROI_NodeSegment;
   
   /* try filling up the rest */
   oElmt = Elmt;
   do {
      /* get the next element with a surfnode */
     SUMA_BS_NEXT_SURF_NODE(sv->BS, oElmt, Elmt);
    
     if (!Elmt) {
      /* perhaps reached end of list without success */
      SUMA_S_Note("Reached EOL without finding Elmt.\n"
                  "Not necessarily a bad thing.");
      SUMA_RETURN(ROId);
     } else {
      if (LocalHead) {
         fprintf (SUMA_STDERR, "%s: Working with element %p.\n", FuncName, Elmt);
      }
     }
     bsd = (SUMA_BRUSH_STROKE_DATUM *)Elmt->data;
     SUMA_LHv("%d %d\nWill look for edge %d %d\n", 
               ROId->N_n, bsd->SurfNode,
               ROId->nPath[ROId->N_n-1], bsd->SurfNode);
     if (SUMA_FindEdge(SO->EL, ROId->nPath[ROId->N_n-1], bsd->SurfNode) < 0) {
         /* Not found, link nodes together*/
         SUMA_LH("Edge not found, linking together.");
         if (!(ROIlink = SUMA_LinkThisNodeToNodeInStroke (sv, 
                                 ROId->nPath[ROId->N_n-1],  Elmt))) {
            SUMA_SLP_Err ("Failed to connect nodes in stroke.");
            SUMA_RETURN (ROId);
         }
         /* merge ROIlink with ROId */
         SUMA_LH("Merging ROIs together.");
         if (!SUMA_AppendToROIdatum (ROIlink, ROId)) {
               SUMA_RegisterMessage (SUMAg_CF->MessageList, 
                                  "Failed to merge ROIs.", FuncName,
                                  SMT_Critical, SMA_LogAndPopup);
               if (ROIlink) SUMA_FreeROIDatum((void *)ROIlink); ROIlink = NULL;
               SUMA_RETURN(ROId);   
         }
         if (ROIlink) SUMA_FreeROIDatum((void *)ROIlink); ROIlink = NULL;
      }else {
         SUMA_LH("Nodes connected.");
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
         if (LocalHead) 
            fprintf (SUMA_STDERR, "%s: Marking as finished...\n", FuncName);
         /* set the drawing status */
         ROIA->DrawnROI->DrawStatus = SUMA_ROI_Finished;
         
         SOparent = SUMA_findSOp_inDOv(ROIA->DrawnROI->Parent_idcode_str, 
                                       SUMAg_DOv, SUMAg_N_DOv);
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
               Unique = YUP; /* Set to YUP if you have node 
                                 indices listed more than once. */
               Nodes = SUMA_NodesInROI (ROIA->DrawnROI, &N_Nodes, Unique);
               if (Nodes) {
                  ROIA->DrawnROI->CE = SUMA_GetContour (
                                 SOparent, 
                                 Nodes, N_Nodes, &(ROIA->DrawnROI->N_CE), 
                                 0, NULL, NULL, 1);
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
         if (LocalHead) 
            fprintf (SUMA_STDERR, "%s: Marking as InCreation...\n", FuncName);
         ROIA->DrawnROI->DrawStatus = SUMA_ROI_InCreation;
         /* remove any contour if present */
         if (ROIA->DrawnROI->CE) SUMA_free(ROIA->DrawnROI->CE); 
         ROIA->DrawnROI->CE = NULL;
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
SUMA_ACTION_RESULT SUMA_AddToTailJunctionROIDatum (void *data, 
                                                   SUMA_ACTION_POLARITY Pol)
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
         if (LocalHead) 
            fprintf (SUMA_STDERR, "%s: Removing from ROIstrokelist...\n", 
                                             FuncName);
         dlist_remove(ROIA->DrawnROI->ROIstrokelist, 
                      dlist_tail(ROIA->DrawnROI->ROIstrokelist), &eldata);
         /* eldata contains the ROIdatum that has been removed from the list. 
            It should not be freed here 
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
   if (SUMA_StringToNum (s, (void*)fv3, 3, 1) != 3) {/*problem,beep and ignore */
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
   if (SUMA_StringToNum (s, (void *)fv3, 1,1) != 1) {/*problem,beep and ignore */
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
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, SE_Redisplay_AllVisible, 
                                       SES_Suma, sv);
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
   }

   SUMA_RETURNe;
   
}

void SUMA_SetNumFinalSmoothing (char *s, void *data)
{
   static char FuncName[]={"SUMA_SetNumFinalSmoothing"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   float fv3[3];
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, (void *)fv3, 1,1) != 1) {/*problem,beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }

   /* set sv */
  
   if ((int)fv3[0] < 0) {
      SUMA_SLP_Err("Only positive integer\nvalues are valid.\n"); 
      SUMA_RETURNe;
   } 
   SUMAg_CF->X->NumFinalSmoothing = (int)fv3[0];
   
   /* flag surfaces for remix */
   SUMA_SetAllRemixFlag(SUMAg_SVv, SUMAg_N_SVv);
   
   /* register a redisplay for sv*/
   if (!list) list = SUMA_CreateList();
   SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, SE_Redisplay_AllVisible, 
                                       SES_Suma, sv);
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
      npar = SUMA_StringToNum (s, (void *)fv15, 4, 1);
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
   \param data (void *) a typecast of the pointer to the surface viewer to 
                        be affected
   
   NOTE: This is a bit ugly because there is a jump in surface location with 
   the change in center of rotation. Something also needs updating to keep this
   from happening ...  
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
         fprintf (SUMA_STDERR,
                  "Error %s: Failed to update center of rotation", FuncName);
         XBell (XtDisplay (sv->X->TOPLEVEL), 50);
         SUMA_RETURNe;
      }
      SUMA_RETURNe;
   }
   
   /* parse s */
   if (SUMA_StringToNum (s, (void*)fv3, 3,1) != 3) {/*problem, beep and ignore */
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
   if (SUMA_StringToNum (s, (void *)fv3, 3,1) != 3) {/*problem,beep and ignore */
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

void SUMA_SV_SetRenderOrder(char *s, void *data)
{
   static char FuncName[]={"SUMA_SV_SetRenderOrder"};
   SUMA_SurfaceViewer *sv = NULL;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;
   
   sv = (SUMA_SurfaceViewer *)data;
   if (!sv) {
      SUMA_S_Err("Null sv");
      SUMA_RETURNe;
   }
   
   sv->N_otseq = SUMA_SetObjectDisplayOrder(s, sv->otseq);
      
   SUMA_RETURNe;
}

void SUMA_JumpIndex (char *s, void *data)
{
   static char FuncName[]={"SUMA_JumpIndex"};
   SUMA_SurfaceViewer *sv = NULL;
   SUMA_ALL_DO *ado=NULL;
   char *variant = NULL;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;
   
   sv = (SUMA_SurfaceViewer *)data;
   if (!(ado = SUMA_SV_Focus_ADO(sv))) {
      SUMA_S_Err("No ado in focus");
      SUMA_RETURNe;
   }
   
   switch (ado->do_type) {
      case SO_type:
         SUMA_JumpIndex_SO (s, sv, (SUMA_SurfaceObject *)ado);
         break;
      case GDSET_type:
         SUMA_JumpIndex_GDSET (s, sv, (SUMA_DSET *)ado, variant);
         break;
      case CDOM_type:
         SUMA_JumpIndex_CO (s, sv, (SUMA_CIFTI_DO *)ado);
         break;
      case GRAPH_LINK_type: {
         SUMA_GraphLinkDO *gldo=(SUMA_GraphLinkDO *)ado;
         SUMA_DSET *dset=NULL;
         if (!(dset=SUMA_find_GLDO_Dset(gldo))) {
            SUMA_S_Errv("Failed to find dset for gldo %s!!!\n",
                        SUMA_ADO_Label(ado));
            break;
         }
         SUMA_JumpIndex_GDSET (s, sv, dset, gldo->variant);
         break; }
      case TRACT_type: {
         SUMA_JumpIndex_TDO (s, sv, (SUMA_TractDO *)ado);
         break; }
      case MASK_type: {
         SUMA_JumpIndex_MDO (s, sv, (SUMA_MaskDO *)ado);
         break; }
      case VO_type: {
         SUMA_JumpIndex_VO (s, sv, (SUMA_VolumeObject *)ado);
         break; }      
      default:
         SUMA_S_Errv("For %s nothing my dear\n",
            SUMA_ObjectTypeCode2ObjectTypeName(ado->do_type));
         break;
   }
   SUMA_RETURNe;
}

/*!
   \brief sends the cross hair to a certain node index
   \param s (char *) a string containing node index
   \param data (void *) a typecast of the pointer to the surface 
                        viewer to be affected

*/
void SUMA_JumpIndex_SO (char *s, SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_JumpIndex_SO"};
   DList *list=NULL;
   SUMA_EngineData *ED = NULL;
   DListElmt *el=NULL, *Location=NULL;
   SUMA_SurfaceObject  *SOc=NULL;
   SUMA_SO_SIDE sd=SUMA_NO_SIDE;
   float fv3[3];
   int it, iv3[3];
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s || !sv || !SO) SUMA_RETURNe;
      
  /* HERE you should check if you have an L or R at the beginning
   or end of s.
   If you do, then first see if the side of SO (the focus surface)
   is the same as the letter. If it is, proceed. If it is not,
   try to get the contralateral surface with SUMA_Contralateral_SO
   then set the contralateral as the focus surface, then proceed
   with setting the focus node. Needs more work 
   */
   /* parse s */
   SUMA_LHv("Parsing %s\n", s);
   if (SUMA_StringToNumSide(s, (void*)fv3, 1,1, &sd) != 1) {
                                    /*problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }
   
   /* do we have side match with Focus node? */
   SUMA_LHv("Side of jump is %d, SO %s side %d\n", sd, SO->Label, SO->Side);
   if (sd == SUMA_RIGHT || sd == SUMA_LEFT) {
      if ((SO->Side == SUMA_RIGHT || SO->Side == SUMA_LEFT) &&
            SO->Side != sd) {
         /* Need to swith sides */
         if ((SOc = SUMA_Contralateral_SO(SO, SUMAg_DOv, SUMAg_N_DOv))) {
            sv->Focus_DO_ID = SUMA_findSO_inDOv(SOc->idcode_str, 
                                             SUMAg_DOv, SUMAg_N_DOv);
            SUMA_LHv("Jumping to %s (contralateral of %s)\n", 
                  SOc->Label, SO->Label);
            SO = SOc;
         } else {
            SUMA_S_Errv("Failed to find contralateral surface to %s\n"
                        "Ignoring jump to node's side marker\n",
                        SO->Label);
         }
      }
   } 


   /* Set the Nodeselection  */
   it = (int) fv3[0];
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
   if (!(el = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)(&it),
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                      
   } else {
      SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_ngr, NULL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, el);
   }


   /* Now set the cross hair position at the selected node*/
   ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
   if (!(Location=SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3, (void*)&(SO->NodeList[3*it]),
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                          
   } 
   /* and add the SO with this location, needed for VisX business*/
   SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_vp, (void *)SO,
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, Location);

   /* attach the cross hair to the selected surface */
   iv3[0] = SUMA_findSO_inDOv(SO->idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   iv3[1] = it;
   iv3[2] = -1;
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
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: Notifying Afni of CrossHair XYZ\n", FuncName);
      /* register a call to SetAfniCrossHair */
      it = 0; /* Set to 1 if you want instacorr notice to AFNI */
      ED = SUMA_InitializeEngineListData (SE_SetAfniCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&it,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Tail, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURNe;
      }

   }

   /* and if GICOR needs some love */
   if (  SUMAg_CF->Connected_v[SUMA_GICORR_LINE] && 
         SUMAg_CF->giset && !SUMAg_CF->HoldClickCallbacks) {
      if (LocalHead) 
         fprintf(SUMA_STDERR,
                  "%s: Notifying GICOR of node selection\n", FuncName);
      /* register a call to SetGICORnode */
      SUMA_REGISTER_TAIL_COMMAND_NO_DATA( list, SE_SetGICORnode, 
                                          SES_Suma, sv);
   }else {
      SUMA_LHv("No Notification to GICOR. %d %p\n", 
                  SUMAg_CF->Connected_v[SUMA_GICORR_LINE], SUMAg_CF->giset);
   }

   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
   }

   /* now put in a request for locking cross hair but you must do this 
   after the node selection has been executed 
   NOTE: You do not always have SetNodeElem because the list might get emptied in
   the call to AFNI notification.
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

/* Jump to a certain edge on a graph dset */
void SUMA_JumpIndex_GDSET (char *s, SUMA_SurfaceViewer *sv, 
                           SUMA_DSET *dset, char *variant)
{
   static char FuncName[]={"SUMA_JumpIndex_GDSET"};
   DList *list=NULL;
   DListElmt *el=NULL, *Location=NULL;
   SUMA_EngineData *ED = NULL;
   float fv3[3];
   int it, iv3[3];
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s || !sv) SUMA_RETURNe;
      
   /* parse s */
   SUMA_LHv("Parsing %s\n", s);
   if (SUMA_StringToNum(s, (void*)fv3, 1,1) != 1) {
                                    /*problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }
   
   /* Set the Nodeselection  */
   it = (int) fv3[0];
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
   if (!(el = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)(&it),
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                      
   } else {
      SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_ngr, NULL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, el);
   }


   /* Now set the cross hair position at the selected node*/
   ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
   if (!(Location=SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3,
                            (void*)(SUMA_GDSET_NodeXYZ(dset, it, variant, NULL)),
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                          
   } 
   /* and add the object with this location */
   SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_vp, (void *)dset,
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, Location);

   /* attach the cross hair to the selected object 
      Note that binding here is to edge of graph and not to a node*/
   SUMA_find_Dset_GLDO(dset,variant, iv3); /* this will set iv3[0] */
   iv3[1] = it;
   iv3[2] = -1;
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
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: Notifying Afni of CrossHair XYZ\n", FuncName);
      it = 0;
      ED = SUMA_InitializeEngineListData (SE_SetAfniCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&it,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Tail, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURNe;
      }

   }

   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
   }

   /* now put in a request for locking cross hair but you must do this 
   after the node selection has been executed 
   NOTE: You do not always have SetNodeElem because the list might get emptied in
   the call to AFNI notification.
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

/* Jump to a certain point on a tract object */
void SUMA_JumpIndex_TDO (char *s, SUMA_SurfaceViewer *sv, 
                           SUMA_TractDO *tdo)
{
   static char FuncName[]={"SUMA_JumpIndex_TDO"};
   DList *list=NULL;
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)tdo;
   DListElmt *el=NULL, *Location=NULL;
   SUMA_EngineData *ED = NULL;
   float fv3[3];
   int it, iv15[15], iv3[3], nv = 0;
   char stmp[64];
   SUMA_Boolean revert_on_err = YUP;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s || !sv || !tdo || !tdo->net) SUMA_RETURNe;
      
   /* parse s */
   if ((nv = SUMA_StringToNum(s, (void*)fv3, 3,1)) != 1 &&
       nv != 3) {
                                    /*problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }
   SUMA_LHv("Parsed %s to %d val(s)\n", s, nv);
   if (nv == 3) {/* bundle, tract, point */
      iv15[SUMA_NET_BUN] = (int)fv3[SUMA_NET_BUN]; 
      iv15[SUMA_BUN_TRC] = (int)fv3[SUMA_BUN_TRC]; 
      iv15[SUMA_TRC_PNT] = (int)fv3[SUMA_TRC_PNT];
      iv15[SUMA_NET_TRC] = 
         Network_TB_to_1T(tdo->net, iv15[SUMA_BUN_TRC], iv15[SUMA_NET_BUN]);
      it = Network_PTB_to_1P(tdo->net, 
               iv15[SUMA_TRC_PNT], iv15[SUMA_BUN_TRC], iv15[SUMA_NET_BUN]);
      if (it < 0) { /* no good */
         XBell (XtDisplay (sv->X->TOPLEVEL), 50);
         SUMA_LH("BTP %d %d %d could not be parsed\n",
                 iv15[SUMA_NET_BUN],   iv15[SUMA_BUN_TRC], iv15[SUMA_TRC_PNT]);
         if (revert_on_err) {
            sprintf(stmp,"%d", SUMA_ADO_SelectedDatum(ado, NULL, NULL));
            SUMA_JumpIndex_TDO(stmp, sv, tdo);
         }
         SUMA_RETURNe;
      }
      SUMA_LHv("Point ID %d from B%d T%d P%d (tract in net %d)\n",
               it, iv15[SUMA_NET_BUN], iv15[SUMA_BUN_TRC], 
               iv15[SUMA_TRC_PNT], iv15[SUMA_NET_TRC]);
   } else {
      /* Set the point selection  */
      it = (int) fv3[0];
      if (!Network_1P_to_PTB(tdo->net, it, 
               iv15+SUMA_TRC_PNT, iv15+SUMA_BUN_TRC, 
               iv15+SUMA_NET_BUN, iv15+SUMA_NET_TRC)) {
         XBell (XtDisplay (sv->X->TOPLEVEL), 50);
         SUMA_RETURNe;
      }
      SUMA_LHv("Point ID %d yielded B%d T%d P%d (tract in net %d) \n"
               "  (which yields back %d)\n",
               it, iv15[SUMA_NET_BUN], iv15[SUMA_BUN_TRC], iv15[SUMA_TRC_PNT],
               iv15[SUMA_NET_TRC],
               Network_PTB_to_1P(tdo->net,
                  iv15[SUMA_TRC_PNT], iv15[SUMA_BUN_TRC], iv15[SUMA_NET_BUN]));
   }
   
   SUMA_TDO_PointXYZ(tdo, it, iv15, fv3);
   SUMA_LH("   Located at %f %f %f\n", fv3[0], fv3[1], fv3[2]);
   
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
   if (!(el = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)(&it),
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                      
   } else {
      SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_ngr, NULL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, el);
      SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_iv15, (void *)iv15,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, el);
   }


   /* Now set the cross hair position at the selected point*/
   ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
   if (!(Location=SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3, (void*)fv3,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                          
   } 
   /* and add the DO with this location, needed for VisX business*/
   SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_vp, (void *)tdo,
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, Location);

   /* attach the cross hair to the selected object 
      Note that binding here is to edge of graph and not to a node*/
   iv3[0] = SUMA_whichDO(SUMA_ADO_idcode(ado), SUMAg_DOv, SUMAg_N_DOv); 
   iv3[1] = it;
   iv3[2] = -1;
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
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: Notifying Afni of CrossHair XYZ\n", FuncName);
      it = 0;
      ED = SUMA_InitializeEngineListData (SE_SetAfniCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&it,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Tail, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURNe;
      }

   }

   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
   }

   /* now put in a request for locking cross hair but you must do this 
   after the node selection has been executed 
   NOTE: You do not always have SetNodeElem because the list might get emptied in
   the call to AFNI notification.
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

/* Jump to a certain datum on a CIFTI object */
void SUMA_JumpIndex_CO (char *s, SUMA_SurfaceViewer *sv, 
                        SUMA_CIFTI_DO *co)
{
   static char FuncName[]={"SUMA_JumpIndex_CO"};
   DList *list=NULL;
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)co;
   DListElmt *el=NULL, *Location=NULL;
   SUMA_EngineData *ED = NULL;
   float fv3[3], fv15[15];
   int it, iv15[15], iv3[3], nv = 0, *dims=NULL;
   char stmp[64];
   SUMA_DSET *dset=NULL;
   SUMA_Boolean revert_on_err = YUP;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   SUMA_S_Err("Not implemented, see SUMA_JumpIndex_VO and SUMA_JumpIndex_SO "
              "for inspiration. You will need to determine the domain of "
              "the index before jumping anyway");
              
   SUMA_RETURNe;
}

/* Jump to a certain point on a volume object */
void SUMA_JumpIndex_VO (char *s, SUMA_SurfaceViewer *sv, 
                        SUMA_VolumeObject *vo)
{
   static char FuncName[]={"SUMA_JumpIndex_VO"};
   DList *list=NULL;
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)vo;
   DListElmt *el=NULL, *Location=NULL;
   SUMA_EngineData *ED = NULL;
   float fv3[3], fv15[15];
   int it, iv15[15], iv3[3], nv = 0, *dims=NULL;
   char stmp[64];
   SUMA_DSET *dset=NULL;
   SUMA_Boolean revert_on_err = YUP;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;
   
   SUMA_LH("Called");
   if (!s || !sv || !vo ||
       !(dset = SUMA_VO_dset(vo)) ||
       !(dims = SUMA_GetDatasetDimensions(dset))) SUMA_RETURNe;
      
   /* parse s */
   if ((nv = SUMA_StringToNum(s, (void*)fv3, 3,1)) != 1 &&
       nv != 3) {
                                    /*problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }
   SUMA_LHv("Parsed %s to %d val(s)\n", s, nv);
   if (nv == 3) {/* i j k */
      iv15[SUMA_VOL_I] = (int)fv3[SUMA_VOL_I]; 
      iv15[SUMA_VOL_J] = (int)fv3[SUMA_VOL_J]; 
      iv15[SUMA_VOL_K] = (int)fv3[SUMA_VOL_K];
      if (iv15[SUMA_VOL_I] < 0 || iv15[SUMA_VOL_I] >= dims[0] ||
          iv15[SUMA_VOL_J] < 0 || iv15[SUMA_VOL_J] >= dims[1] ||
          iv15[SUMA_VOL_K] < 0 || iv15[SUMA_VOL_K] >= dims[2] ) {
         /* no good */
         XBell (XtDisplay (sv->X->TOPLEVEL), 50);
         SUMA_LH("IJK %d %d %d could not be parsed or out of range\n",
                 iv15[SUMA_VOL_I], iv15[SUMA_VOL_J], iv15[SUMA_VOL_K]);
         if (revert_on_err) {
            sprintf(stmp,"%d", SUMA_ADO_SelectedDatum(ado, NULL, NULL));
            SUMA_JumpIndex_VO(stmp, sv, vo);
         }
         SUMA_RETURNe;
      }
      
      iv15[SUMA_VOL_IJK] = 
         SUMA_3D_2_1D_index(iv15[SUMA_VOL_I], iv15[SUMA_VOL_J], iv15[SUMA_VOL_K],
         dims[0], dims[0]*dims[1] );
         
      it = iv15[SUMA_VOL_IJK];

      SUMA_LHv("Voxel ID %d from I%d J%d K%d\n",
               iv15[SUMA_VOL_IJK], iv15[SUMA_VOL_I], iv15[SUMA_VOL_J], 
               iv15[SUMA_VOL_K]);
   } else {
      /* Set the point selection  */
      it = (int) fv3[0];
      if (it < 0 || it >= SDSET_NVOX(dset)) {
         XBell (XtDisplay (sv->X->TOPLEVEL), 50);
         SUMA_RETURNe;
      }
      Vox1D2Vox3D(it, dims[0], dims[0]*dims[1], (iv15+SUMA_VOL_I));
      SUMA_LHv("Point ID %d yielded I%d J%d K%d \n",
               it, iv15[SUMA_VOL_I], iv15[SUMA_VOL_J], iv15[SUMA_VOL_K]);
   }
   
   SUMA_VO_PointXYZ(vo, it, iv15, fv3);
   SUMA_LH("   Located at %f %f %f\n", fv3[0], fv3[1], fv3[2]);
   SUMA_MARK_PLANE_NOT_SET(fv15+SUMA_VOL_SLC_EQ0); /* No cigar */
   
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
   if (!(el = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)(&it),
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                      
   } else {
      SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_ngr, NULL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, el);
      SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_iv15, (void *)iv15,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, el);
      SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv15, (void *)fv15,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, el);
   }


   /* Now set the cross hair position at the selected point*/
   ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
   if (!(Location=SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3, (void*)fv3,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                          
   } 
   /* and add the DO with this location, needed for VisX business*/
   SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_vp, (void *)vo,
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, Location);

   /* attach the cross hair to the selected object 
      Note that binding here is to edge of graph and not to a node*/
   iv3[0] = SUMA_whichDO(SUMA_ADO_idcode(ado), SUMAg_DOv, SUMAg_N_DOv); 
   iv3[1] = it;
   iv3[2] = -1;
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
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: Notifying Afni of CrossHair XYZ\n", FuncName);
      it = 0;
      ED = SUMA_InitializeEngineListData (SE_SetAfniCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&it,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Tail, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURNe;
      }

   }

   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
   }

   /* now put in a request for locking cross hair but you must do this 
   after the node selection has been executed 
   NOTE: You do not always have SetNodeElem because the list might get emptied in
   the call to AFNI notification.
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

/* Jump to a certain point on a mask object */
void SUMA_JumpIndex_MDO (char *s, SUMA_SurfaceViewer *sv, SUMA_MaskDO *mo)
{
   static char FuncName[]={"SUMA_JumpIndex_MDO"};
   DList *list=NULL;
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)mo;
   DListElmt *el=NULL, *Location=NULL;
   SUMA_EngineData *ED = NULL;
   float fv3[3];
   int it, iv15[15], iv3[3], nv = 0, *dims=NULL;
   char stmp[64];
   SUMA_DSET *dset=NULL;
   SUMA_Boolean revert_on_err = YUP;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s || !sv) SUMA_RETURNe;
   
   SUMA_S_Err("Not ready for action");
   SUMA_RETURNe;
      
   /* parse s */
   if ((nv = SUMA_StringToNum(s, (void*)fv3, 3,1)) != 1 &&
       nv != 3) {
                                    /*problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }
   SUMA_LHv("Parsed %s to %d val(s)\n", s, nv);
   if (nv == 3) {/* i j k */
      iv15[SUMA_VOL_I] = (int)fv3[SUMA_VOL_I]; 
      iv15[SUMA_VOL_J] = (int)fv3[SUMA_VOL_J]; 
      iv15[SUMA_VOL_K] = (int)fv3[SUMA_VOL_K];
      if (iv15[SUMA_VOL_I] < 0 || iv15[SUMA_VOL_I] >= dims[0] ||
          iv15[SUMA_VOL_J] < 0 || iv15[SUMA_VOL_J] >= dims[1] ||
          iv15[SUMA_VOL_K] < 0 || iv15[SUMA_VOL_K] >= dims[2] ) {
         /* no good */
         XBell (XtDisplay (sv->X->TOPLEVEL), 50);
         SUMA_LH("IJK %d %d %d could not be parsed or out of range\n",
                 iv15[SUMA_VOL_I], iv15[SUMA_VOL_J], iv15[SUMA_VOL_K]);
         if (revert_on_err) {
            sprintf(stmp,"%d", SUMA_ADO_SelectedDatum(ado, NULL, NULL));
            SUMA_JumpIndex_MDO(stmp, sv, mo);
         }
         SUMA_RETURNe;
      }
      
      iv15[SUMA_VOL_IJK] = 
         SUMA_3D_2_1D_index(iv15[SUMA_VOL_I], iv15[SUMA_VOL_J], iv15[SUMA_VOL_K],
         dims[0], dims[0]*dims[1] );


      SUMA_LHv("Voxel ID %d from I%d J%d K%d\n",
               iv15[SUMA_VOL_IJK], iv15[SUMA_VOL_I], iv15[SUMA_VOL_J], 
               iv15[SUMA_VOL_K]);
   } else {
      /* Set the point selection  */
      it = (int) fv3[0];
      if (it < 0 || it >= SDSET_NVOX(dset)) {
         XBell (XtDisplay (sv->X->TOPLEVEL), 50);
         SUMA_RETURNe;
      }
      Vox1D2Vox3D(it, dims[0], dims[0]*dims[1], (iv15+SUMA_VOL_I));
      SUMA_LHv("Point ID %d yielded I%d J%d K%d \n",
               it, iv15[SUMA_VOL_I], iv15[SUMA_VOL_J], iv15[SUMA_VOL_K]);
   }
   
   SUMA_MDO_PointXYZ(mo, it, iv15, fv3);
   SUMA_LH("   Located at %f %f %f\n", fv3[0], fv3[1], fv3[2]);
   
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
   if (!(el = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)(&it),
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                      
   } else {
      SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_ngr, NULL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, el);
      SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_iv15, (void *)iv15,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, el);
   }


   /* Now set the cross hair position at the selected point*/
   ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
   if (!(Location=SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3, (void*)fv3,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                          
   } 
   /* and add the DO with this location, needed for VisX business*/
   SUMA_RegisterEngineListCommand (  list, ED, 
                                           SEF_vp, (void *)mo,
                                           SES_Suma, (void *)sv, NOPE,
                                           SEI_In, Location);

   /* attach the cross hair to the selected object 
      Note that binding here is to edge of graph and not to a node*/
   iv3[0] = SUMA_whichDO(SUMA_ADO_idcode(ado), SUMAg_DOv, SUMAg_N_DOv); 
   iv3[1] = it;
   iv3[2] = -1;
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
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: Notifying Afni of CrossHair XYZ\n", FuncName);
      it = 0;
      ED = SUMA_InitializeEngineListData (SE_SetAfniCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&it,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Tail, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURNe;
      }

   }

   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
   }

   /* now put in a request for locking cross hair but you must do this 
   after the node selection has been executed 
   NOTE: You do not always have SetNodeElem because the list might get emptied in
   the call to AFNI notification.
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
   - Update to other viewers is performed IF they are XYZ locked 
   (that can get confusing)
*/
void SUMA_JumpXYZ (char *s, void *data)
{
   static char FuncName[]={"SUMA_JumpXYZ"};
   DList *list=NULL;
   DListElmt *Location=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   float fv3[3];
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, (void*)fv3, 3,1) != 3) {/*problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }
   
   /* Now set the cross hair position */
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetCrossHair);
   if (!(Location = SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_fv3, (void*)fv3,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
      SUMA_RETURNe;                                      
   }
   /* and add the SO with this location (not possible here), needed for VisX 
      business*/
   SUMA_RegisterEngineListCommand (  list, ED, 
                                     SEF_vp, NULL,
                                     SES_Suma, (void *)sv, NOPE,
                                     SEI_In, Location);

   /* check to see if AFNI needs to be notified */
   if (SUMAg_CF->Connected_v[SUMA_AFNI_STREAM_INDEX] && sv->LinkAfniCrossHair) {
      int it;
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s: Notifying Afni of CrossHair XYZ\n", FuncName);
      it = 0;
      ED = SUMA_InitializeEngineListData (SE_SetAfniCrossHair);
      if (!SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)&it,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Tail, NULL)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register element\n", FuncName);
         SUMA_RETURNe;
      }

   }

   /* call with the list */
   if (!SUMA_Engine (&list)) {
      fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      SUMA_RETURNe;
   }

   /* now put in a request for locking cross hair but you must 
   do this after the node selection has been executed 
   NOTE: You do not always have SetNodeElem because the list might 
   get emptied in the call to AFNI notification.
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
   DListElmt *el=NULL;
   SUMA_EngineData *ED = NULL;
   SUMA_SurfaceViewer *sv = NULL;
   float fv3[3];
   int it;
   SUMA_SurfaceObject *SO=NULL, *SOc=NULL;
   SUMA_SO_SIDE sd=SUMA_NO_SIDE;
   SUMA_Boolean LocalHead = NOPE; 

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;
   if (!(SO = SUMA_SV_Focus_SO(sv))) {
      SUMA_S_Err("No SO in focus");
      SUMA_RETURNe;
   }
   

   /* HERE you should check if you have an L or R at the beginning
   or end of s.
   If you do, then first see if the side of SO (the focus surface)
   is the same as the letter. If it is, proceed. If it is not,
   try to get the contralateral surface with SUMA_Contralateral_SO
   then set the contralateral as the focus surface, then proceed
   with setting the focus node. Needs more work 
   */
   /* parse s */
   SUMA_LHv("Parsing %s\n", s);
   if (SUMA_StringToNumSide(s, (void*)fv3, 1,1, &sd) != 1) {
                                    /*problem, beep and ignore */
      XBell (XtDisplay (sv->X->TOPLEVEL), 50);
      SUMA_RETURNe;
   }
   
   SUMA_LHv("Side of focus jump is %d\n", sd);
   /* do we have side match with Focus node? */
   if (sd == SUMA_RIGHT || sd == SUMA_LEFT) {
      if ((SO->Side == SUMA_RIGHT || SO->Side == SUMA_LEFT) &&
            SO->Side != sd) {
         /* Need to swith sides */
         if ((SOc = SUMA_Contralateral_SO(SO, SUMAg_DOv, SUMAg_N_DOv))) {
            sv->Focus_DO_ID = SUMA_findSO_inDOv(SOc->idcode_str, 
                                             SUMAg_DOv, SUMAg_N_DOv);
            SUMA_LHv("Jumping focus only to %s (contralateral of %s)\n", 
                  SOc->Label, SO->Label);
            SO = SOc;
         } else {
            SUMA_S_Errv("Failed to find contralateral surface to %s\n"
                        "Ignoring jump to node's side marker\n",
                        SO->Label);
         }
      }
   } 
   /* Set the Nodeselection  */
   it = (int) fv3[0];
   if (!list) list = SUMA_CreateList ();
   ED = SUMA_InitializeEngineListData (SE_SetSelectedNode);
   if (!(el=SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_i, (void*)(&it),
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_Head, NULL))) {
      SUMA_SLP_Err("Failed to register element");
      SUMA_RETURNe;                                      
   } else {
      SUMA_RegisterEngineListCommand (  list, ED, 
                                          SEF_ngr, NULL,
                                          SES_Suma, (void *)sv, NOPE,
                                          SEI_In, el);
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

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, (void*)fv3, 1,1) != 1) {/*problem, beep and ignore */
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

   SUMA_ENTRY;

   if (!s) SUMA_RETURNe;

   sv = (SUMA_SurfaceViewer *)data;

   /* parse s */
   if (SUMA_StringToNum (s, (void*)fv15, 6,1) != 6) {
                              /*problem, beep and ignore */
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
