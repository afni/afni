
#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF;
extern int SUMAg_N_DOv; 
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv;

/*!
   \brief a function that returns the first viewer that is in momentum mode 
   isv = SUMA_WhichViewerInMomentum( SVv,  N_SV,  sv); 
   
   \param SVv (SUMA_SurfaceViewer *) vector of surface viewers
   \param N_SV (int ) number of surface viewer structures in SVv
   \param sv (SUMA_SurfaceViewer *) if !NULL then the function returns the
            index of a surface viewer OTHER THAN sv that is in momentum mode.
            Otherwise the first surface viewer in momentum mode is returned
   \return isv (int) the index (into SVv) of the first viewer that is in
            momentum mode. If sv is not NULL then it is the index of the 
            first viewer OTHER THAN sv that is in momentum mode
            If none are found, ans = -1
   
   -  To turn the index into the viewer's label use: 
      if (isv >= 0) sprintf(slabel,"[%c] SUMA", 65+isv); 
      else sprintf(slabel,"[DOH] SUMA");
*/

int SUMA_WhichViewerInMomentum(SUMA_SurfaceViewer *SVv, int N_SV, SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_WhichViewerInMomentum"};
   int ii = -1;
   
   SUMA_ENTRY;
   
   if (!SVv) SUMA_RETURN(-1);
   
   for (ii=0; ii < SUMAg_N_SVv; ++ii) {
         if (SVv[ii].GVS[SVv[ii].StdView].ApplyMomentum) {
            if (!sv) { /* don't care which one */
               SUMA_RETURN(ii);
            } else if (&(SUMAg_SVv[ii]) != sv) { /* other than sv */
               SUMA_RETURN(ii);
            }
         }
   }
   
   SUMA_RETURN(-1);
   
}
 
/* This is used to hold the functions that manipulate SV, Surface Viewer Structures */
/*! 
\brief returns a string corresponding to the link type

SUMA_Boolean SUMA_LockEnum_LockType (SUMA_LINK_TYPES i, char *Name);
\param i (SUMA_LINK_TYPES) see enum type in SUMA_define.h
\param Name (char *) a preallocated character string (no more than 50 chars)
\return YUP/NOPE OK, error
*/
SUMA_Boolean SUMA_LockEnum_LockType (SUMA_LINK_TYPES i, char *Name)
{
   static char FuncName[]={"SUMA_LockEnum_LockType"};
   SUMA_ENTRY;   
   
   switch (i) {
      case SUMA_No_Lock:
         sprintf (Name, "No Lock");
         break;
      case SUMA_I_Lock:
         sprintf (Name, "Index Lock");
         break;
      case SUMA_XYZ_Lock:
         sprintf (Name, "XYZ Lock");
         break;
      default:
         sprintf (Name, "?");
         SUMA_RETURN (NOPE);
         
   }
   
   SUMA_RETURN (YUP);
}
/*!
Create a SurfaceViewer data structure
*/
SUMA_SurfaceViewer *SUMA_Alloc_SurfaceViewer_Struct (int N)
{
   SUMA_SurfaceViewer *SV, *SVv;
   static char FuncName[]={"SUMA_Alloc_SurfaceViewer_Struct"};
   int i, j, n;
   
   SUMA_ENTRY;

   SVv =  (SUMA_SurfaceViewer *)SUMA_malloc(sizeof(SUMA_SurfaceViewer)*N);
   if (SVv == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed to SUMA_malloc SV\n", FuncName);
      SUMA_RETURN (NULL);
   }
   for (i=0; i < N; ++i) {
      SV = &(SVv[i]);
      
      SV->N_GVS = SUMA_N_STANDARD_VIEWS;
      SV->GVS = (SUMA_GEOMVIEW_STRUCT *)SUMA_malloc(sizeof(SUMA_GEOMVIEW_STRUCT)*SV->N_GVS);
      if (!SV->GVS) {
         fprintf(SUMA_STDERR,"Error %s: Could not allocate for N_GVS.\n", FuncName);
         SUMA_RETURN (NULL);
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
               
               SV->GVS[j].translateVec[0] = 0.0;
               SV->GVS[j].translateVec[1] = 0.0;
               break;
            default:
               fprintf(SUMA_STDERR,"Error %s: Undefined viewing mode.\n", FuncName);
               SUMA_RETURN (NULL);
               
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

      {
         static SUMA_Boolean err = NOPE;
         float fv3[3];
         char *eee = getenv("SUMA_BackgroundColor");
         if (eee && !err) {
            if (SUMA_StringToNum (eee, fv3, 3) != 3) { 
               err = YUP;
               SUMA_SL_Err("Syntax error in environment\n"
                           "variable SUMA_BackgroundColor");
               SV->clear_color[0] = SUMA_CLEAR_COLOR_R;
               SV->clear_color[1] = SUMA_CLEAR_COLOR_G;
               SV->clear_color[2] = SUMA_CLEAR_COLOR_B;
               SV->clear_color[3] = SUMA_CLEAR_COLOR_A;
            }else {
               SV->clear_color[0] = fv3[0];
               SV->clear_color[1] = fv3[1];
               SV->clear_color[2] = fv3[2];
               SV->clear_color[3] = SUMA_CLEAR_COLOR_A;  
            }
         }else {
            SV->clear_color[0] = SUMA_CLEAR_COLOR_R;
            SV->clear_color[1] = SUMA_CLEAR_COLOR_G;
            SV->clear_color[2] = SUMA_CLEAR_COLOR_B;
            SV->clear_color[3] = SUMA_CLEAR_COLOR_A;   
         } 
      }
      
      
      SV->WindWidth = 350;
      SV->WindHeight = 350;
      {
         char *eee = getenv("SUMA_ArrowRotAngle");
         if (eee) {
            float rotval = strtod(eee, NULL);
            if (rotval > 0.0 && rotval < 360.0) SV->ArrowRotationAngle = SUMA_PI * rotval / 180.0;
            else SV->ArrowRotationAngle = SUMA_PI * ARROW_ROTATION_ANGLE_DEG / 180.0;
         } else SV->ArrowRotationAngle = SUMA_PI * ARROW_ROTATION_ANGLE_DEG / 180.0;
      }
      
      SV->Open = NOPE;
      
      SV->RegisteredDO = (int *)SUMA_calloc( SUMA_MAX_DISPLAYABLE_OBJECTS, sizeof(int));
      if (SV->RegisteredDO == NULL) {
         fprintf(stderr,"Error SUMA_Alloc_SurfaceViewer_Struct: Failed to SUMA_malloc SV->RegisteredDO\n");
         SUMA_RETURN (NULL);
      }
      SV->N_DO = 0; /* Nothing is registered with the viewer yet */

      SV->ColList = (SUMA_COLORLIST_STRUCT *) SUMA_malloc( sizeof(SUMA_COLORLIST_STRUCT) * SUMA_MAX_DISPLAYABLE_OBJECTS);
      SV->N_ColList = 0; /* this number reflects the number of surfaces that have colorlist structures in SV */
      /* initialize fields */
      for (j=0; j<SUMA_MAX_DISPLAYABLE_OBJECTS; ++j) {
         SV->ColList[j].idcode_str = NULL;
         SV->ColList[j].glar_ColorList = NULL;
         SV->ColList[j].N_glar_ColorList = 0;
         SV->ColList[j].Remix = NOPE;
      }
      
      
      SV->ShowEyeAxis = 1;
      SV->ShowMeshAxis = 1;
      
      SV->Ch = SUMA_Alloc_CrossHair ();
      if (SV->Ch == NULL) {
         fprintf(stderr,"Error SUMA_Alloc_SurfaceViewer_Struct: Failed in SUMA_Alloc_CrossHair\n");
         SUMA_RETURN (NULL); 
      } else SV->ShowCrossHair = 1;
      
      SV->X = (SUMA_X *)SUMA_malloc(sizeof(SUMA_X));
      if (SV->X == NULL) {
         fprintf(stderr,"Error SUMA_Alloc_SurfaceViewer_Struct: Failed to SUMA_malloc SV->X\n");
         SUMA_RETURN (NULL);
      }

      SV->X->Title = NULL;
      SV->X->LookAt_prmpt = NULL;
      SV->X->JumpIndex_prmpt = NULL;
      SV->X->JumpXYZ_prmpt = NULL;
      SV->X->JumpFocusNode_prmpt = NULL;
      SV->X->JumpFocusFace_prmpt = NULL;
      SV->X->HighlightBox_prmpt = NULL;
      SV->X->TOPLEVEL = NULL;
      SV->X->MOMENTUMID = 0;
      SV->X->REDISPLAYPENDING = 0;
      SV->X->DOUBLEBUFFER = True;
      SV->X->WIDTH = SV->X->HEIGHT = 300; /* if you change this, make sure you do so for fallbackResources in SUMA_display */
      SV->X->ViewCont = SUMA_CreateViewContStruct();
      
      SV->Focus_SO_ID = -1;
      SV->Focus_DO_ID = -1;
      
      SV->VSv = NULL;
      SV->N_VSv = 0;
      SV->LastNonMapStateID = -1;
      
      SV->iCurGroup = -1;
      SV->CurGroupName = NULL;
      
      SV->PolyMode = SRM_Fill;
      
      #if SUMA_BACKFACE_CULL
         SV->BF_Cull = YUP;
      #else
         SV->BF_Cull = NOPE;
      #endif

      SV->ShowForeground = YUP;
      SV->ShowBackground = YUP;
      SV->Back_Modfact = SUMA_BACKGROUND_MODULATION_FACTOR;
      
      SV->isShaded = NOPE; 
      
      SV->LinkAfniCrossHair = YUP;
      
      SV->ResetGLStateVariables = YUP;
      
      SV->BS = NULL;
      
      SV->ShowRight = YUP;
      SV->ShowLeft = YUP;
      SV->Record = NOPE;
      SV->rdc = SUMA_RDC_NOT_SET;
   }
   SUMA_RETURN (SVv);
}

SUMA_Boolean SUMA_Free_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV)
{
   static char FuncName[]={"SUMA_Free_SurfaceViewer_Struct"};
   int i;
   
   SUMA_ENTRY;

   if (SV->Ch) SUMA_Free_CrossHair (SV->Ch);
   if (SV->X->Title) SUMA_free(SV->X->Title);
   if (SV->X->LookAt_prmpt) SUMA_FreePromptDialogStruct (SV->X->LookAt_prmpt);
   if (SV->X->JumpIndex_prmpt) SUMA_FreePromptDialogStruct (SV->X->JumpIndex_prmpt);
   if (SV->X->JumpXYZ_prmpt) SUMA_FreePromptDialogStruct (SV->X->JumpXYZ_prmpt);
   if (SV->X->JumpFocusNode_prmpt) SUMA_FreePromptDialogStruct (SV->X->JumpFocusNode_prmpt);
   if (SV->X->JumpFocusFace_prmpt) SUMA_FreePromptDialogStruct (SV->X->JumpFocusFace_prmpt);
   if (SV->X->HighlightBox_prmpt) SUMA_FreePromptDialogStruct (SV->X->HighlightBox_prmpt);
   if (SV->X->ViewCont) SUMA_FreeViewContStruct(SV->X->ViewCont);
   if (SV->X) SUMA_free(SV->X);
   if (SV->RegisteredDO) SUMA_free(SV->RegisteredDO);
   if (SV->VSv) {
      for (i=0; i < SV->N_VSv; ++i) {
         if (!SUMA_Free_ViewState (&(SV->VSv[i]))) {
            fprintf (SUMA_STDERR,"Error %s: failed in SUMA_Free_ViewState.\n", FuncName);
         }
      }
   }
   
   if (SV->CurGroupName) SUMA_free(SV->CurGroupName); SV->CurGroupName= NULL;
   
   if (SV->GVS) SUMA_free(SV->GVS);
   if (SV->State) SV->State = NULL; /* never free that one */ 
   if (SV->ColList) {
      for (i=0; i < SV->N_ColList; ++i) {
         if (!SUMA_EmptyColorList (SV, NULL)) fprintf (SUMA_STDERR,"Error %s: failed in SUMA_EmptyColorList.\n", FuncName);
      }
      /* done dumping structure contents, now free pointer */
      SUMA_free(SV->ColList); 
      SV->ColList = NULL; 
      SV->N_ColList = 0;
   }
   
   if (SV->BS) {
      SUMA_EmptyDestroyList(SV->BS);
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_Free_SurfaceViewer_Struct_Vect (SUMA_SurfaceViewer *SVv, int N)
{
   static char FuncName[]={"SUMA_Free_SurfaceViewer_Struct_Vect"};
   int i;
   SUMA_Boolean Ret= YUP;
   
   SUMA_ENTRY;

   for (i=0; i < N; ++i)  {
      if (&SVv[i] != NULL) {
         Ret = Ret * SUMA_Free_SurfaceViewer_Struct (&SVv[i]);
      }
   }
   
   if (SVv) SUMA_free(SVv);
   SUMA_RETURN(Ret);
}

/*!
\brief ans = SUMA_FillColorList (sv, so);
Creates a colorlist structure for a certain surface.   

\param sv (SUMA_SurfaceViewer *) pointer to surface viewer
\param so (SUMA_SurfaceObject *) pointer to surface object

Remix flag is set to YUP since this function is called when surfaces are beging registered
with a viewer and a remix is highly likely.

\return ans YUP/NOPE

*/
SUMA_Boolean SUMA_FillColorList (SUMA_SurfaceViewer *sv, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_FillColorList"};
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Filling a color list for surface %s (%s).\n", FuncName, SO->Label, SO->idcode_str);

   if (!SO->idcode_str) {
      fprintf (SUMA_STDERR,"Error %s: SO->idcode_str is NULL.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* make sure SO->idcode_str is not in the list already */
   for (i=0; i<sv->N_ColList; ++i) {
      if (strcmp (SO->idcode_str, sv->ColList[i].idcode_str) == 0) {
         fprintf (SUMA_STDERR,"Error %s: SO->idcode_str is already in sv->ColList.\n", FuncName);
         SUMA_RETURN (NOPE);
      }
   }
   
   /* create the ColList struct */
   if (sv->ColList[sv->N_ColList].glar_ColorList) {
      fprintf (SUMA_STDERR,"Error %s: glar_ColorList is not NULL. Cannot reallocate.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   SUMA_LH("Pre-alloc\n");
   sv->ColList[sv->N_ColList].glar_ColorList = (GLfloat *) SUMA_calloc (SO->N_Node*4, sizeof(GLfloat));
   sv->ColList[sv->N_ColList].idcode_str = (char *)SUMA_malloc((strlen(SO->idcode_str)+1) * sizeof(char));
   SUMA_LH("Post-alloc\n");
   
   if (!sv->ColList[sv->N_ColList].glar_ColorList || !sv->ColList[sv->N_ColList].idcode_str) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for glar_ColorList or idcode_str.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   sv->ColList[sv->N_ColList].idcode_str = strcpy (sv->ColList[sv->N_ColList].idcode_str, SO->idcode_str);
   if (LocalHead) fprintf (SUMA_STDERR, "%s: sv->ColList[%d].idcode_str=%s is about to be filled.\n", \
               FuncName, sv->N_ColList, sv->ColList[sv->N_ColList].idcode_str);

   /* fill up with blanks, may be unecessary ... */
   sv->ColList[sv->N_ColList].N_glar_ColorList = SO->N_Node*4;
   i=0;
   while (i < sv->ColList[sv->N_ColList].N_glar_ColorList) {
      sv->ColList[sv->N_ColList].glar_ColorList[i] = SUMA_GRAY_NODE_COLOR; ++i;
      sv->ColList[sv->N_ColList].glar_ColorList[i] = SUMA_GRAY_NODE_COLOR; ++i;
      sv->ColList[sv->N_ColList].glar_ColorList[i] = SUMA_GRAY_NODE_COLOR; ++i;
      sv->ColList[sv->N_ColList].glar_ColorList[i] = SUMA_NODE_ALPHA; ++i;
   }
   sv->ColList[sv->N_ColList].Remix = YUP; 

   ++sv->N_ColList;
   
   SUMA_RETURN (YUP);

}

/*!
   glar_ColorList = SUMA_GetColorList (sv, DO_idstr);
   returns the pointer to the colorlist of the DO (or SO) with ID string DO_idstr
   
   \param sv (SUMA_SurfaceViewer *) pointer to surface viewer in question
   \param DO_idstr (char *) ID string of DO (usually a Surface Object) 
   \return glar_ColorList (GLfloat *) a pointer to the array containing node colors 
*/
GLfloat * SUMA_GetColorList (SUMA_SurfaceViewer *sv, char *DO_idstr)
{
   static char FuncName[]={"SUMA_GetColorList"};
   int i;
   GLfloat * glar_ColorList = NULL;
   SUMA_Boolean Found = NOPE;
   
   SUMA_ENTRY;
   
   if (!DO_idstr) {
      fprintf (SUMA_STDERR,"Error %s: DO_idstr is NULL, this should not be.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* find the culprit */
   Found = NOPE;
   i = 0;
   while (!Found && i < sv->N_ColList) {
      if (strcmp (DO_idstr, sv->ColList[i].idcode_str) == 0) {
         Found = YUP;
         SUMA_RETURN (sv->ColList[i].glar_ColorList);      
      }
      ++i;
   }
   
   if (!Found) {
      fprintf (SUMA_STDERR,"Error %s: DO_idstr was not found.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* should not get to this point */
   fprintf (SUMA_STDERR,"Error %s: Logic error. Should not get here.\n", FuncName);
   SUMA_RETURN (NULL);

}

/*!

\brief Empty a colorlist structure 

ans = SUMA_EmptyColorList (sv, DO_idstr)

\param sv (SUMA_SurfaceViewer *) pointer to surface viewer in question
\param DO_idstr (char *) ID string of DO (usually a Surface Object) If you want to delete all 
                        color lists, set this pointer to NULL
\return ans (SUMA_Boolean) YUP/NOPE 

*/
SUMA_Boolean SUMA_EmptyColorList (SUMA_SurfaceViewer *sv, char *DO_idstr)
{
   static char FuncName[]={"SUMA_EmptyColorList"};
   int i;
   SUMA_Boolean Found = NOPE;
   
   SUMA_ENTRY;
   
   if (!sv->ColList) {
      fprintf (SUMA_STDERR,"Error %s: sv->ColList is NULL, this should not be.\n", FuncName);
      SUMA_RETURN (NOPE);
   } 
   
   if (!DO_idstr) {
      /* empty them all */
      for (i=0; i < sv->N_ColList; ++i) {
         if (sv->ColList[i].glar_ColorList) SUMA_free(sv->ColList[i].glar_ColorList);
         sv->ColList[i].glar_ColorList = NULL;
         sv->ColList[i].N_glar_ColorList = 0;
         if (sv->ColList[i].idcode_str) SUMA_free(sv->ColList[i].idcode_str); 
         sv->ColList[i].idcode_str = NULL;
         sv->ColList[i].Remix = NOPE;
      }   
   } else { /* just empty one */
      Found = NOPE;
      i = 0;
      while (!Found && i < sv->N_ColList) {
         if (strcmp (DO_idstr, sv->ColList[i].idcode_str) == 0) {
            Found = YUP;
            /* empty the load */
            if (sv->ColList[i].glar_ColorList) SUMA_free(sv->ColList[i].glar_ColorList);
            sv->ColList[i].glar_ColorList = NULL;
            sv->ColList[i].N_glar_ColorList = 0;
            if (sv->ColList[i].idcode_str) SUMA_free(sv->ColList[i].idcode_str); 
            sv->ColList[i].idcode_str = NULL;
            sv->ColList[i].Remix = NOPE;
            /* copy the last in the list here */
            if (i < sv->N_ColList) {
               sv->ColList[i].glar_ColorList = sv->ColList[sv->N_ColList-1].glar_ColorList;
               sv->ColList[i].N_glar_ColorList = sv->ColList[sv->N_ColList-1].N_glar_ColorList;
               sv->ColList[i].idcode_str = sv->ColList[sv->N_ColList-1].idcode_str;
               sv->ColList[i].Remix = sv->ColList[sv->N_ColList-1].Remix;
               
               /* mark the last element as empty */
               sv->ColList[sv->N_ColList-1].glar_ColorList = NULL;
               sv->ColList[sv->N_ColList-1].N_glar_ColorList = 0;
               sv->ColList[sv->N_ColList-1].idcode_str = NULL;
               sv->ColList[sv->N_ColList-1].Remix = NOPE;
               
               /* decrement the number of full elements in ColList */
               --sv->N_ColList;
            }
         } 
         ++i;
      }
      if (!Found) {
         fprintf (SUMA_STDERR,"Error %s: item %s was not found, this should not be.\n", FuncName, DO_idstr);
         SUMA_RETURN (NOPE);
      }
   }
   
   SUMA_RETURN (YUP);
}

/*!
   ans = SUMA_SetShownLocalRemixFlag (SUMA_SurfaceViewer *sv)
   Set Remix flags for all surfaces in sv->RegisteredDO regardless of their relationship.
   This is useful when you change the settings for background color modulation and the like.
   \param sv (SUMA_SurfaceViewer *) pointer to surface viewer 
   \return ans (SUMA_Boolean) YUP/NOPE
   \sa SUMA_SetRemixFlag
   \sa SUMA_SetLocalRemixFlag

*/
SUMA_Boolean SUMA_SetShownLocalRemixFlag (SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_SetShownLocalRemixFlag"};
   int k;
      
   SUMA_ENTRY;
   
   for (k=0; k < sv->N_ColList; ++k) {
      sv->ColList[k].Remix = YUP;
   }
   
   SUMA_RETURN (YUP);
}

/*!
   ans = SUMA_SetLocalRemixFlag (char *idcode_str, SUMA_SurfaceViewer *sv);
   Search RegisteredDO for sv and if a Surface in RegisteredDO is related 
   to DO_idcode_str then its remix flag is set to yes.
   
   \param idcode_str (char *) IDcode of the surface that had its colorplanes modified
   \param sv (SUMA_SurfaceViewer *) pointer to surface viewer 
   \return ans (SUMA_Boolean) YUP/NOPE
   \sa SUMA_SetRemixFlag
   \sa SUMA_SetShownLocalRemixFlag
   
   Will I ever use that one, not common to have related surfaces in one view ... ?
*/
SUMA_Boolean SUMA_SetLocalRemixFlag (char *SO_idcode_str, SUMA_SurfaceViewer *sv)
{  
   static char FuncName[]={"SUMA_SetLocalRemixFlag"};
   SUMA_SurfaceObject *SO1 = NULL, *SO2 = NULL;
   int k, kk, dov_id;   
   SUMA_Boolean Found = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO_idcode_str || !sv) {
      fprintf (SUMA_STDERR,"Error %s: NULL sv or SO_idcode_str. BAD\n", FuncName);
      SUMA_RETURN (NOPE);
   }

   dov_id = SUMA_findSO_inDOv (SO_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   if (dov_id < 0) {
      fprintf (SUMA_STDERR,"Error %s: Failed to find object with idcode %s.\n", FuncName, SO_idcode_str);
      SUMA_RETURN (NOPE);
   }
   SO1 = (SUMA_SurfaceObject *)SUMAg_DOv[dov_id].OP;
   
   /* search for relatives in RegisteredDO */
   for (k=0; k < sv->N_DO; ++k) {
      SO2 = (SUMA_SurfaceObject *)SUMAg_DOv[sv->RegisteredDO[k]].OP;
      if (SUMA_isRelated (SO1, SO2, 1)) { /* only for kinship of da first order */
         /* related, set flag for remixing SO2 */
         kk = 0;
         Found = NOPE;
         while (!Found && kk < sv->N_ColList) {
            if (strcmp (SO2->idcode_str, sv->ColList[kk].idcode_str) == 0) {
               Found = YUP;
               sv->ColList[kk].Remix = YUP;
            }
            ++kk;
         }
         if (!Found) {
            fprintf (SUMA_STDERR,"Error %s: Failed to find surface in ColList structs. BAD.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
      }  
   } 
   
   SUMA_RETURN (YUP);
}



/*!
   ans = SUMA_SetRemixFlag (char *idcode_str, SUMA_SurfaceViewer *SVv, int N_SVv);
   Search RegisteredDO for each Surface Viewer and if a Surface in RegisteredDO is related 
   to DO_idcode_str then its remix flag is set to yes.
   
   \param idcode_str (char *) IDcode of the surface that had its colorplanes modified
   \param SVv (SUMA_SurfaceViewer *) vector of existing surface viewers (typically, that is SUMAg_SVv)
   \param N_SVv (int) number of surface viewers (typically that is N_SUMAg_SVv)
   \return ans (SUMA_Boolean) YUP/NOPE
   \sa SUMA_SetLocalRemixFlag
   \sa SUMA_SetShownLocalRemix

   DO NOT SET THE REMIXFLAG unless you have modified the colorplanes of a certain surface. This function will
   set a remix flags to all related surfaces that are being displayed in all viewers. You want to do this
   when one (or all) of the colorplanes is changed. Alternately, if you make changes that only affect the 
   surface as is it shown in the viewer (change background modulation for example), you want to do the remixing
   for the concerned surface or surfaces only in that viewer and not in all viewers open. Perhaps I should write 
   a function to set the remix flags for surfaces within the viewer only. Something like SUMA_SetLocalRemixFlag or 
   SUMA_SetShownLocalRemix.
   
*/
SUMA_Boolean SUMA_SetRemixFlag (char *SO_idcode_str, SUMA_SurfaceViewer *SVv, int N_SVv)
{
   static char FuncName[]={"SUMA_SetRemixFlag"};
   SUMA_SurfaceViewer *sv;
   SUMA_SurfaceObject *SO1 = NULL, *SO2 = NULL;
   int i, k, kk, dov_id;   
   SUMA_Boolean Found = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SO_idcode_str || !SVv) {
      fprintf (SUMA_STDERR,"Error %s: NULL SVv or SO_idcode_str. BAD\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   dov_id = SUMA_findSO_inDOv (SO_idcode_str, SUMAg_DOv, SUMAg_N_DOv);
   if (dov_id < 0) {
      fprintf (SUMA_STDERR,"Error %s: Failed to find object with idcode %s.\n", FuncName, SO_idcode_str);
      SUMA_RETURN (NOPE);
   }
   SO1 = (SUMA_SurfaceObject *)SUMAg_DOv[dov_id].OP;
   
   /* search all viewers */
   for (i=0; i < N_SVv; ++i) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Searching viewer %d.\n", FuncName, i);
      sv = &(SVv[i]);
      /* search for relatives in RegisteredDO */
      for (k=0; k < sv->N_DO; ++k) {
         if (SUMA_isSO(SUMAg_DOv[sv->RegisteredDO[k]])) {
            SO2 = (SUMA_SurfaceObject *)SUMAg_DOv[sv->RegisteredDO[k]].OP;
            if (SUMA_isRelated (SO1, SO2, 1)) { /* only 1st order kinship allowed */
               /* related, set flag for remixing SO2 */
               kk = 0;
               Found = NOPE;
               while (!Found && kk < sv->N_ColList) {
                  if (strcmp (SO2->idcode_str, sv->ColList[kk].idcode_str) == 0) {
                     Found = YUP;
                     sv->ColList[kk].Remix = YUP;
                  }
                  ++kk;
               }
               if (!Found) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to find surface in ColList structs. BAD.\n", FuncName);
                  SUMA_RETURN (NOPE);
               }
            }
         }  
      } 
   }
   
   SUMA_RETURN (YUP);
}

/*!
   \brief sets remix flags for all color lists in viewers specified in SVv
   Use this function whenever global color changes occur
   
   \sa SUMA_SetRemixFlag for detailed help

*/
SUMA_Boolean SUMA_SetAllRemixFlag (SUMA_SurfaceViewer *SVv, int N_SVv)
{
   static char FuncName[]={"SUMA_SetAllRemixFlag"};
   SUMA_SurfaceViewer *sv;
   int i, kk;   
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SVv) {
      fprintf (SUMA_STDERR,"Error %s: NULL SVv . BAD\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* search all viewers */
   for (i=0; i < N_SVv; ++i) {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Searching viewer %d.\n", FuncName, i);
      sv = &(SVv[i]);
      for (kk = 0; kk < sv->N_ColList; ++kk) sv->ColList[kk].Remix = YUP;
   }
   
   SUMA_RETURN (YUP);
}

/*!
Updates the View Center and view from of SV based on the contents of RegisteredDO
*/

SUMA_Boolean SUMA_UpdateViewPoint (SUMA_SurfaceViewer *SV, SUMA_DO *dov, int N_dov)
{
   int i, do_id, TotWeight;
   float NewCenter[3];
   SUMA_SurfaceObject *so_op;
   static char FuncName[]={"SUMA_UpdateViewPoint"};
      
   SUMA_ENTRY;

   NewCenter[0] = 0.0;
   NewCenter[1] = 0.0;
   NewCenter[2] = 0.0;
   TotWeight = 0;
   
   i = 0;
   while (i < SV->N_DO) {
      do_id = SV->RegisteredDO[i];
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

   SUMA_RETURN (YUP);
   
   
}
/*!
Updates the Rotation Center of SV based on the contents of RegisteredDO
*/
SUMA_Boolean SUMA_UpdateRotaCenter (SUMA_SurfaceViewer *SV, SUMA_DO *dov, int N_dov)
{
   int i, do_id, TotWeight;
   float NewCenter[3];
   SUMA_SurfaceObject *so_op;
   static char FuncName[]={"SUMA_UpdateRotaCenter"};
   
   SUMA_ENTRY;

   NewCenter[0] = 0.0;
   NewCenter[1] = 0.0;
   NewCenter[2] = 0.0;
   TotWeight = 0;
   
   i = 0;
   while (i < SV->N_DO) {
      do_id = SV->RegisteredDO[i];
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
   SUMA_RETURN (YUP);
   
}

/*!
output the state variable contents of the Surface Viewer 
*/
void SUMA_Show_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV, FILE *Out, int detail)
{
   static char FuncName[]={"SUMA_Show_SurfaceViewer_Struct"};
   char *s = NULL;  
   
   SUMA_ENTRY;

   if (Out == NULL) Out = stdout;
   
   s = SUMA_SurfaceViewer_StructInfo (SV, detail);
   
   if (s) {
      fprintf(Out, "%s", s);
      SUMA_free(s); s = NULL;
   }else {
      SUMA_SL_Err("Failed in SUMA_SurfaceViewer_StructInfo");
   }
   
   SUMA_RETURNe;
}

char *SUMA_SurfaceViewer_StructInfo (SUMA_SurfaceViewer *SV, int detail)
{
   static char FuncName[]={"SUMA_SurfaceViewer_StructInfo"};
   SUMA_STRING *SS = NULL;
   char *s=NULL;
   int i;
      
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   if (!SV) {
      SS = SUMA_StringAppend (SS,"NULL SV.\n");
      SS = SUMA_StringAppend (SS, NULL);
      /* copy s pointer and free SS */
      s = SS->s;
      SUMA_free(SS);
      SUMA_RETURN(s);  
   }
   
   SS = SUMA_StringAppend(SS, "\nSV contents:\n");
   SS = SUMA_StringAppend_va(SS, "   verbose = %d\n", SV->verbose); 
   if (SV->ShowLeft) SS = SUMA_StringAppend_va(SS,"   Show Left = YES\n");
   else SS = SUMA_StringAppend_va(SS,"   Show Left = NO\n");
   if (SV->ShowRight) SS = SUMA_StringAppend_va(SS,"   Show Right = YES\n");
   else SS = SUMA_StringAppend_va(SS,"   Show Right = NO\n");
   SS = SUMA_StringAppend_va(SS,"   Aspect = %f\n", SV->Aspect);
   SS = SUMA_StringAppend_va(SS,"   ViewFrom = [%f %f %f]\n", SV->GVS[SV->StdView].ViewFrom[0], SV->GVS[SV->StdView].ViewFrom[1], SV->GVS[SV->StdView].ViewFrom[2]);
   SS = SUMA_StringAppend_va(SS,"   ViewFromOrig = [%f %f %f]\n", SV->GVS[SV->StdView].ViewFromOrig[0], SV->GVS[SV->StdView].ViewFromOrig[1], SV->GVS[SV->StdView].ViewFromOrig[2]);
   SS = SUMA_StringAppend_va(SS,"   ViewCenter = [%f %f %f]\n", SV->GVS[SV->StdView].ViewCenter[0], SV->GVS[SV->StdView].ViewCenter[1], SV->GVS[SV->StdView].ViewCenter[2]);
   SS = SUMA_StringAppend_va(SS,"   ViewCenterOrig = [%f %f %f]\n", SV->GVS[SV->StdView].ViewCenterOrig[0], SV->GVS[SV->StdView].ViewCenterOrig[1], SV->GVS[SV->StdView].ViewCenterOrig[2]);
   SS = SUMA_StringAppend_va(SS,"   ViewCamUp = [%f %f %f]\n", SV->GVS[SV->StdView].ViewCamUp[0], SV->GVS[SV->StdView].ViewCamUp[1], SV->GVS[SV->StdView].ViewCamUp[2]);
   SS = SUMA_StringAppend_va(SS,"   RotaCenter = [%f %f %f]\n", SV->GVS[SV->StdView].RotaCenter[0], SV->GVS[SV->StdView].RotaCenter[1], SV->GVS[SV->StdView].RotaCenter[2]);
   SS = SUMA_StringAppend_va(SS,"   light0_position = [%f %f %f %f]\n", SV->light0_position[0], SV->light0_position[1], SV->light0_position[2], SV->light0_position[3]);
   SS = SUMA_StringAppend_va(SS,"   light1_position = [%f %f %f %f]\n", SV->light1_position[0], SV->light1_position[1], SV->light1_position[2], SV->light1_position[3]);
   SS = SUMA_StringAppend_va(SS,"   WindWidth = %d\n", SV->WindWidth);
   SS = SUMA_StringAppend_va(SS,"   WindHeight = %d\n", SV->WindHeight);
   SS = SUMA_StringAppend_va(SS,"   currentQuat = [%f %f %f %f]\n", SV->GVS[SV->StdView].currentQuat[0], SV->GVS[SV->StdView].currentQuat[1], SV->GVS[SV->StdView].currentQuat[2], SV->GVS[SV->StdView].currentQuat[3]);
   SS = SUMA_StringAppend_va(SS,"   deltaQuat = [%f %f %f %f]\n", SV->GVS[SV->StdView].deltaQuat[0], SV->GVS[SV->StdView].deltaQuat[1], SV->GVS[SV->StdView].deltaQuat[2], SV->GVS[SV->StdView].deltaQuat[3]);
   SS = SUMA_StringAppend_va(SS,"   ApplyMomentum = %d\n", SV->GVS[SV->StdView].ApplyMomentum);
   SS = SUMA_StringAppend_va(SS,"   MinIdleDelta = %d\n", SV->GVS[SV->StdView].MinIdleDelta);
   SS = SUMA_StringAppend_va(SS,"   zoomDelta = %f, zoomBegin = %f\n", SV->GVS[SV->StdView].zoomDelta, SV->GVS[SV->StdView].zoomBegin);
   SS = SUMA_StringAppend_va(SS,"   ArrowRotationAngle=%f rad (%f deg)\n", SV->ArrowRotationAngle, SV->ArrowRotationAngle * 180.0 / SUMA_PI);
   SS = SUMA_StringAppend_va(SS,"   spinDeltaX/Y = %d/%d\n", SV->GVS[SV->StdView].spinDeltaX, SV->GVS[SV->StdView].spinDeltaY);
   SS = SUMA_StringAppend_va(SS,"   spinBeginX/Y = %d/%d\n", SV->GVS[SV->StdView].spinBeginX, SV->GVS[SV->StdView].spinBeginY);   
   SS = SUMA_StringAppend_va(SS,"   TranslateGain = %f\n", SV->GVS[SV->StdView].TranslateGain);
   SS = SUMA_StringAppend_va(SS,"   ArrowtranslateDeltaX/Y = %f/%f\n", SV->GVS[SV->StdView].ArrowtranslateDeltaX, SV->GVS[SV->StdView].ArrowtranslateDeltaY);
   SS = SUMA_StringAppend_va(SS,"   translateBeginX/Y = %d/%d\n", SV->GVS[SV->StdView].translateBeginX, SV->GVS[SV->StdView].translateBeginY);
   SS = SUMA_StringAppend_va(SS,"   translateDeltaX/Y = %f/%f\n", SV->GVS[SV->StdView].translateDeltaX, SV->GVS[SV->StdView].translateDeltaY);
   SS = SUMA_StringAppend_va(SS,"   translateVec = [%f %f 0.0]\n", SV->GVS[SV->StdView].translateVec[0], SV->GVS[SV->StdView].translateVec[1]);
   SS = SUMA_StringAppend_va(SS,"   Show Mesh Axis %d\n", SV->ShowMeshAxis);
   SS = SUMA_StringAppend_va(SS,"   Show Eye Axis %d\n", SV->ShowEyeAxis);
   SS = SUMA_StringAppend_va(SS,"   Show Cross Hair %d\n", SV->ShowCrossHair);
   SS = SUMA_StringAppend_va(SS,"   PolyMode %d\n", SV->PolyMode);
   
   SS = SUMA_StringAppend_va(SS,"   Group Name %s, indexed %d\n", SV->CurGroupName, SV->iCurGroup);
   SS = SUMA_StringAppend_va(SS,"   Current State %s, indexed %d\n", SV->State, SV->iState);
   SS = SUMA_StringAppend_va(SS,"   N_DO = %d\n", SV->N_DO);
   SS = SUMA_StringAppend(SS, "   RegisteredDO = [");

   for (i=0; i< SV->N_DO; ++i) {
      SS = SUMA_StringAppend_va(SS,"%d, ", SV->RegisteredDO[i]); 
   } 
   SS = SUMA_StringAppend(SS,"]\n");
   if (SV->X == NULL) SS = SUMA_StringAppend_va(SS,"   X struct is NULL!\n");
   else {
   SS = SUMA_StringAppend_va(SS,"   X struct defined.\n");
   }
   
   SS = SUMA_StringAppend_va(SS,"   SO in focus %d\n", SV->Focus_SO_ID);
   SS = SUMA_StringAppend_va(SS,"   DO in focus %d\n", SV->Focus_DO_ID);

   /* show some state stuff */
   SS = SUMA_StringAppend(SS, "\nView States:\n");
   for (i=0; i < SV->N_VSv; ++i) {
      SS = SUMA_StringAppend_va(SS,"\nView State %d/%d (FOV = %f):\n", i, SV->N_VSv-1, SV->FOV[i]);
      s = SUMA_ViewStateInfo (&(SV->VSv[i]), 0);
      if (!s) {
         SS = SUMA_StringAppend(SS, "*** Error in SUMA_Show_ViewState ***\n");
      } else {
         SS = SUMA_StringAppend(SS, s);
         SUMA_free(s); s = NULL;
      }
   }
   SS = SUMA_StringAppend_va(SS, "\nStandard viewing mode: %d\n", SV->StdView );
   SS = SUMA_StringAppend_va(SS, "\nBackground Modulation Factor= %f\n", SV->Back_Modfact);
   SS = SUMA_StringAppend_va(SS, "\nLast non mappable visited %d\n", SV->LastNonMapStateID);
   
   SS = SUMA_StringAppend(SS,"\n");
   
   /* trim SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS);
      
   SUMA_RETURN(s);
}

/*! Show the ViewState structure */
SUMA_Boolean SUMA_Show_ViewState(SUMA_ViewState *VS, FILE *Out, int detail) 
{
   static char FuncName[]={"SUMA_Show_ViewState"};
   char *s = NULL;
   
   SUMA_ENTRY;

   if (Out == NULL) Out = stdout;

   s = SUMA_ViewStateInfo(VS,  detail);
   if (!s) {
      SUMA_SL_Err("Failed in SUMA_ViewStateInfo");
      SUMA_RETURN(NOPE);
   }  else {
      fprintf(Out, "%s", s);
      SUMA_free(s); s = NULL;
   }
   
   SUMA_RETURN(YUP);
}

/*! Show the ViewState structure */
char *SUMA_ViewStateInfo(SUMA_ViewState *VS, int detail) 
{
   static char FuncName[]={"SUMA_ViewStateInfo"};
   int i;
   SUMA_STRING *SS = NULL;
   char *s=NULL;   

   SUMA_ENTRY;

   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   if (!VS) {
      SS = SUMA_StringAppend (SS,"NULL VS.\n");
      SS = SUMA_StringAppend (SS, NULL);
      /* copy s pointer and free SS */
      s = SS->s;
      SUMA_free(SS);
      SUMA_RETURN(s);  
   }

   if (VS->Name) SS = SUMA_StringAppend_va(SS, "   Name: %s\n", VS->Name);
   else SS = SUMA_StringAppend_va(SS, "   Name: NULL\n");
   
   if (VS->Group) SS = SUMA_StringAppend_va(SS, "   Group: %s\n", VS->Group);
   else SS = SUMA_StringAppend_va(SS, "   Group: NULL\n");
   
   if (VS->N_MembSOs) {
      SS = SUMA_StringAppend_va(SS, "   %d MembSOs: ", VS->N_MembSOs);
      for (i=0; i < VS->N_MembSOs; ++i) SS = SUMA_StringAppend_va(SS, "%d, ", VS->MembSOs[i]);
      SS = SUMA_StringAppend_va(SS, "\n");
   } else {
      SS = SUMA_StringAppend_va(SS, "   No MembSOs\n");
   }
   
   if (VS->Hist) {
      if (VS->Hist->N_DO) {
         SS = SUMA_StringAppend_va(SS, "   Hist->N_DO = %d\nHist->RegisteredDO: ", VS->Hist->N_DO);
         for (i=0; i < VS->Hist->N_DO; ++i) {
            SS = SUMA_StringAppend_va(SS, "   %d, ", VS->Hist->RegisteredDO[i]);
         }
      }
   } else {
      SS = SUMA_StringAppend_va(SS, "   Hist is NULL\n");
   }
   
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS);
   
   SUMA_RETURN (s);
}

/*!
   Create & free ViewState_Hist structure 
*/
SUMA_ViewState_Hist *SUMA_Alloc_ViewState_Hist (void)
{
   static char FuncName[]={"SUMA_Alloc_ViewState_Hist"};
   SUMA_ViewState_Hist *vsh;
   
   SUMA_ENTRY;

   vsh = (SUMA_ViewState_Hist *)SUMA_malloc(sizeof(SUMA_ViewState_Hist));
   if (vsh == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Could not allocate for vsh.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   vsh->RegisteredDO = NULL;
   vsh->N_DO = 0;
   SUMA_RETURN (vsh);
}   
SUMA_Boolean SUMA_Free_ViewState_Hist (SUMA_ViewState_Hist *vsh)
{
   static char FuncName[]={"SUMA_Free_ViewState_Hist"};
   
   SUMA_ENTRY;

   if (vsh == NULL) SUMA_RETURN (YUP);
   if (vsh->RegisteredDO) SUMA_free(vsh->RegisteredDO);
   if (vsh) SUMA_free(vsh);
   SUMA_RETURN (YUP);
}

/*!
   Add a new SUMA_ViewState structure 
   This is meant to replace SUMA_Alloc_ViewState
   
   - Both csv->VSv and csv->N_VSv are updated here
*/
SUMA_Boolean SUMA_New_ViewState (SUMA_SurfaceViewer *cs)
{
   static char FuncName[]={"SUMA_New_ViewState"};

   SUMA_ENTRY;

   
   if (!cs->VSv) { /* a new baby */
      cs->N_VSv = 1;
      cs->VSv = (SUMA_ViewState *)SUMA_malloc(sizeof(SUMA_ViewState));
   } else { /* realloc */
      ++cs->N_VSv;
      cs->VSv = (SUMA_ViewState *)SUMA_realloc(cs->VSv, cs->N_VSv*sizeof(SUMA_ViewState) );
   }
   
   /* check on allocation */
   if (!cs->VSv) {
      SUMA_SL_Err("Failed to allocate");
      SUMA_RETURN(YUP);
   }
   
   /* initialization of last element */
   cs->VSv[cs->N_VSv-1].Name = NULL;
   cs->VSv[cs->N_VSv-1].Group = NULL;
   cs->VSv[cs->N_VSv-1].MembSOs = NULL;
   cs->VSv[cs->N_VSv-1].N_MembSOs = 0;
   cs->VSv[cs->N_VSv-1].Hist = SUMA_Alloc_ViewState_Hist ();
   if (cs->VSv[cs->N_VSv-1].Hist == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Could not allocate for cs->VSv->Hist.\n", FuncName);
      SUMA_free(cs->VSv);
      SUMA_RETURN (NOPE);
   }
   
   /* Done */
   SUMA_RETURN(YUP);
   
}
 
/*!
   Create & free SUMA_ViewState structure 
*/
SUMA_ViewState *SUMA_Alloc_ViewState (int N)
{
   SUMA_ViewState *vs;
   int i;
   static char FuncName[]={"SUMA_Alloc_ViewState"};
   
   SUMA_ENTRY;

   SUMA_SL_Err("Should not be using this anymore.\n"
               "Start using SUMA_New_ViewState.\n"
               "     ZSS Jan 12 04 \n");
   SUMA_RETURN(NULL);
   vs = (SUMA_ViewState *)SUMA_malloc(sizeof(SUMA_ViewState)*N);
   if (vs == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Could not allocate for vs.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   for (i=0; i< N; ++i) {
      vs[i].Name = NULL;
      vs[i].Group = NULL;
      vs[i].MembSOs = NULL;
      vs[i].N_MembSOs = 0;
      vs[i].Hist = SUMA_Alloc_ViewState_Hist ();
      if (vs[i].Hist == NULL) {
         fprintf(SUMA_STDERR,"Error %s: Could not allocate for vs->Hist.\n", FuncName);
         SUMA_free(vs);
         SUMA_RETURN (NULL);
      }
   }
   SUMA_RETURN (vs);
}   

SUMA_Boolean SUMA_Free_ViewState (SUMA_ViewState *vs)
{
   static char FuncName[]={"SUMA_Free_ViewState"};
   SUMA_ENTRY;

   if (vs == NULL) SUMA_RETURN (YUP);
   if (vs->Name) SUMA_free(vs->Name);
   if (vs->Group) SUMA_free(vs->Group);
   if (vs->MembSOs) SUMA_free(vs->MembSOs);
   if (vs->Hist) SUMA_Free_ViewState_Hist (vs->Hist);
   if (vs) SUMA_free(vs);
   SUMA_RETURN (YUP);
}

/*! 
   locate the index i (into SVv[i]) of sv 
   -1 if not found
*/
int SUMA_WhichSV (SUMA_SurfaceViewer *sv, SUMA_SurfaceViewer *SVv, int N_SVv)
{
   static char FuncName[]={"SUMA_WhichSV"};
   int i = 0;
   
   SUMA_ENTRY;
   
   if (!SVv || !sv) {
      fprintf (SUMA_STDERR, "Error %s: NULL SVv or sv.\n", FuncName);
      SUMA_RETURN (-1);
   }
   
   for (i=0; i<N_SVv; ++i) {
      if (&(SVv[i]) == sv) {
         SUMA_RETURN (i);
      } 
   }
   
   
   SUMA_RETURN (-1);
}

/*! 
   locate the index i (into csv->VSv[i]) of state 
   -1 if not found
*/
int SUMA_WhichState (char *state, SUMA_SurfaceViewer *csv, char *ForceGroup)
{
   static char FuncName[]={"SUMA_WhichState"};
   int i = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!ForceGroup) {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Searching for: %s\n", 
                                 FuncName, state);
      while (i < csv->N_VSv) {
         if (LocalHead) fprintf(SUMA_STDERR,"   %d? %s ...\n", 
                                 i, csv->VSv[i].Name);
                                 
         if (!csv->VSv[i].Name || !state) {
            SUMA_SL_Err("Null Name or State \n");
            SUMA_RETURN (-1);
         }
         if (strcmp(csv->VSv[i].Name, state) == 0) {
            if (LocalHead) fprintf(SUMA_STDERR,"%s: FOUND, i=%d!\n", FuncName, i);
            SUMA_RETURN (i);
         }
         ++i;
      }
   } else {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Searching for: %s, %s...\n", 
                              FuncName, state, ForceGroup);
      while (i < csv->N_VSv) {
         if (LocalHead) fprintf(SUMA_STDERR,"   %d? %s, %s ...\n", 
                                 i, csv->VSv[i].Name, csv->VSv[i].Group);
         if (!csv->VSv[i].Name || !state || !csv->CurGroupName) {
            SUMA_SL_Err("Null Name or State or CurGroupName.\n");
            SUMA_RETURN (-1);
         }
         if (strcmp(csv->VSv[i].Name, state) == 0 && strcmp(csv->VSv[i].Group, ForceGroup) == 0 ) {
            if (LocalHead) fprintf(SUMA_STDERR,"%s: FOUND, i=%d!\n", FuncName, i);
            SUMA_RETURN (i);
         }
         ++i;
      }
   }
   SUMA_RETURN (-1);
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
   static int iwarn=0;
   SUMA_SurfaceObject * SO;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (LocalHead && SUMA_WhichSV(csv, SUMAg_SVv, SUMA_MAX_SURF_VIEWERS) != 0) {
      fprintf(SUMA_STDERR,"%s: Muted for viewer[%c]\n", FuncName, 65+SUMA_WhichSV(csv, SUMAg_SVv, SUMA_MAX_SURF_VIEWERS) );
      /* turn off the LocalHead, too much output*/
      LocalHead = NOPE;
   }
   

   /* allocate for space depending on the number of states present */
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Entering, Spec->Group[0] = %s ...\n", 
      FuncName, Spec->Group[0]);
   
   if (Spec->N_Groups != 1) {
      SUMA_SL_Err("A spec file is to have 1 and only 1 group in it");
      SUMA_RETURN(NOPE);
   }
   
   #if 0
      /* the old way */
      if (!csv->VSv) { /* first pass */
         csv->VSv = SUMA_Alloc_ViewState (Spec->N_States);
         if (csv->VSv == NULL) {
            fprintf(SUMA_STDERR,"Error %s: Failed to allocate for VSv.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         csv->N_VSv = 0;
      } 
   #endif
   
   /* register the various states from each SO in DOv */
   SUMA_LH("Cycling through DOvs...");
   for (i=0; i < N_dov; ++i) {
      if (SUMA_isSO_G(dov[i], Spec->Group[0])) {
         SO = (SUMA_SurfaceObject *)(dov[i].OP);
         is = SUMA_WhichState (SO->State, csv, SO->Group);
         if (is < 0) {
            /* add state if it is a new one */
            /* make room */
            if (LocalHead) {
               fprintf(SUMA_STDERR,"%s: For %s\nState:%s,Group:%s to be added\n", 
                  FuncName, SO->Label, SO->State, SO->Group);
            }
            SUMA_New_ViewState (csv);
            csv->VSv[csv->N_VSv-1].Name = SUMA_copy_string(SO->State);
            csv->VSv[csv->N_VSv-1].Group = SUMA_copy_string(Spec->Group[0]);
            if (!csv->VSv[csv->N_VSv-1].Name || !csv->VSv[csv->N_VSv-1].Group) {
               fprintf(SUMA_STDERR,"Error %s: Failed to allocate for csv->VSv[csv->N_VSv-1].Name or .Group.\n", FuncName);
               SUMA_RETURN (NOPE);
            }   
            csv->VSv[csv->N_VSv-1].N_MembSOs = 1;
         } else { /* old one, count it */
            if (LocalHead) {
               fprintf(SUMA_STDERR,"%s: For %s\n State:%s,Group:%s found\n", 
                  FuncName, SO->Label, SO->State, SO->Group);
            }
            csv->VSv[is].N_MembSOs += 1;
         }
      }
   }
   
   SUMA_LH("Allocating...");   
   
   /* allocate for FOV */
   if (!csv->FOV) {
      csv->FOV = (float *)SUMA_calloc(csv->N_VSv, sizeof(float));
   } else {
      csv->FOV = (float *)SUMA_realloc(csv->FOV, csv->N_VSv * sizeof(float));
   }
   
   /* allocate space for MembSOs counters will be reset for later use counting proceeds
   also initialize FOV*/
   if (!iwarn) {
      SUMA_LH(  "WARNING: This block is resetting FOV\n"
                  "to all surface views regardless\n"
                  "of whether a new addition was made or not.\n"
                  "This message will not be shown again in this session.\n"
                  "Well, looks like it does no bad thing...");
                  ++iwarn;
   }
   for (i=0; i < csv->N_VSv; ++i) {
      csv->FOV[i] = FOV_INITIAL;
      if (!csv->VSv[i].MembSOs) {
         csv->VSv[i].MembSOs = (int *) SUMA_calloc(csv->VSv[i].N_MembSOs, sizeof(int));
      } else {
         csv->VSv[i].MembSOs = (int *) SUMA_realloc(csv->VSv[i].MembSOs, csv->VSv[i].N_MembSOs * sizeof(int));
      }
      if (csv->VSv[i].MembSOs == NULL) {
         fprintf(SUMA_STDERR,"Error %s: Failed to allocate for csv->VSv[i].MembSOs.\n", FuncName);
         SUMA_RETURN (NOPE);
      }   
      csv->VSv[i].N_MembSOs = 0;
   }

   
   /*fprintf(SUMA_STDERR,"%s: placement ...\n", FuncName);*/
   
   /* now place each SO where it belongs, don't worry about the group they're in */
   for (i=0; i < N_dov; ++i) {
      if (SUMA_isSO(dov[i])) {
         SO = (SUMA_SurfaceObject *)(dov[i].OP);
         /* find out which state it goes in */
         
         is = SUMA_WhichState (SO->State, csv, SO->Group);
         if (is < 0) {
            fprintf(SUMA_STDERR,"Error %s: This should not be.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
         if (LocalHead) {
            fprintf (SUMA_STDERR,"%s: Trying to house %s in: State[%d]\n", \
            FuncName, SO->Label, is);
         }
         /* store it where it should be */
         csv->VSv[is].MembSOs[csv->VSv[is].N_MembSOs] = i; /* store it's id as valid member of the state*/
         csv->VSv[is].N_MembSOs += 1; /* count it, again */ 
      }
   }
   
   /*fprintf(SUMA_STDERR,"%s: Leaving ...\n", FuncName);*/

   SUMA_RETURN (YUP);
}

/*! allocate and intialize SUMA_CommonFields 
\sa SUMA_Free_CommonFields
*/
SUMA_CommonFields * SUMA_Create_CommonFields ()
{
   static char FuncName[]={"SUMA_Create_CommonFields"};
   SUMA_CommonFields *cf;
   int i, portn = -1, n;
   char *eee=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   /* This is the function that creates the debugging flags, do not use them here */
   cf = NULL;
   
   /* allocate */
   /* DO NOT USE SUMA_malloc here, too early for that */
   cf = (SUMA_CommonFields *)malloc(sizeof(SUMA_CommonFields));
   
   if (cf == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate.\n", FuncName);
      SUMA_RETURN (cf);
   }
   
   cf->Dev = NOPE;
   cf->InOut_Notify = NOPE;
   cf->InOut_Level = 0;
   cf->MemTrace = NOPE;
   #ifdef USE_SUMA_MALLOC
   SUMA_SL_Err("NO LONGER SUPPORTED");
   return(NULL);
   cf->Mem = SUMA_Create_MemTrace();
   #else
   cf->Mem = NULL;
   #endif
   
   eee = getenv("SUMA_AFNI_TCP_PORT");
   if (eee) {
      portn = atoi(eee);
      if (portn < 1024 ||  portn > 65535) {
         fprintf (SUMA_STDERR, "Warning %s:\n"
                               "Environment variable SUMA_AFNI_TCP_PORT %d is invalid.\n"
                               "port must be between 1025 and 65534.\n"
                               "Using default of %d\n", FuncName, portn, SUMA_TCP_PORT);
         portn = SUMA_TCP_PORT;
      } 
   } else {
      portn = SUMA_TCP_PORT;
   }   
   
   for (i=0; i<SUMA_MAX_STREAMS; ++i) {
      cf->ns_v[i] = NULL;
      cf->ns_flags_v[i] = 0;
      cf->Connected_v[i] = NOPE;
      cf->TrackingId_v[i] = 0;
      cf->NimlStream_v[i][0] = '\0';
      cf->HostName_v[i][0] = '\0';
      cf->TCP_port[i] = portn + i;   /* me hopes those will be OK */
   }
   cf->Listening = NOPE;
   cf->niml_work_on = NOPE;
   
   for (i=0; i<SUMA_MAX_SURF_VIEWERS; ++i) {
      cf->Locked[i] = SUMA_I_Lock;
      cf->ViewLocked[i] = NOPE;
   }
   
   eee = getenv("SUMA_SwapButtons_1_3");
   if (eee) {
      if (strcmp (eee, "YES") == 0) cf->SwapButtons_1_3 = YUP;
      else cf->SwapButtons_1_3 = NOPE;
   } else {
      cf->SwapButtons_1_3 = NOPE;
   }

   cf->X = (SUMA_X_AllView *)malloc(sizeof(SUMA_X_AllView));
   if (!cf->X) {
     fprintf(SUMA_STDERR,"Error %s: Failed to allocate.\n", FuncName);
     return (NULL); 
   }
   cf->X->SumaCont = SUMA_CreateSumaContStruct();
   cf->X->DrawROI = SUMA_CreateDrawROIStruct();
   cf->X->DPY_controller1 = NULL;
   
   eee = getenv("SUMA_ColorPattern");
   if (eee) {
      if (strcmp (eee, "AFNI") == 0) {
         cf->X->X_Resources = SXR_Afni;
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Afni resources\n", FuncName);
      } else if (strcmp (eee, "EURO") == 0) {
         cf->X->X_Resources = SXR_Euro;
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Euro resources\n", FuncName);
      } else if (strcmp (eee, "BONAIRE") == 0) {
         cf->X->X_Resources = SXR_Bonaire;
         if (LocalHead) fprintf(SUMA_STDERR,"%s: Bonaire resources\n", FuncName);
      } else if (strcmp (eee, "DEFAULT") == 0) {
         cf->X->X_Resources = SXR_default;
         if (LocalHead) fprintf(SUMA_STDERR,"%s: default resources\n", FuncName);
      } else {
         cf->X->X_Resources = SXR_Euro;
         fprintf(SUMA_STDERR,"%s:\nUnrecognized option %s for SUMA_ColorPattern.\nUsing default = EURO\n", FuncName, eee);
      }
   } else {
      cf->X->X_Resources = SXR_Euro;
      if (LocalHead) fprintf(SUMA_STDERR,"%s: Undefined environment. Using default\n", FuncName);
   }
   
   cf->X->Help_TextShell = NULL;
   cf->X->Log_TextShell = NULL;
   cf->X->FileSelectDlg = NULL;
   cf->X->N_ForeSmooth_prmpt = NULL;
   {
      char *eee = getenv("SUMA_NumForeSmoothing");
      if (eee) {
         int rotval = (int)strtod(eee, NULL);
         if (rotval >= 0) cf->X->NumForeSmoothing = rotval;
         else {
            fprintf (SUMA_STDERR,   "Warning %s:\n"
                                    "Bad value for environment variable SUMA_NumForeSmoothing\n"
                                    "Assuming default of 0", FuncName);
            cf->X->NumForeSmoothing = 0;
         }
      } else cf->X->NumForeSmoothing = 0;
   }
   
   {
      char *eee = getenv("SUMA_WarnBeforeClose");
      if (eee) {
         if (strcmp(eee,"NO") == 0) cf->X->WarnClose = NOPE;
         else if (strcmp(eee,"YES") == 0) cf->X->WarnClose = YUP;
         else {
            fprintf (SUMA_STDERR,   "Warning %s:\n"
                                    "Bad value for environment variable SUMA_WarnBeforeClose\n"
                                    "Assuming default of YES", FuncName);
            cf->X->WarnClose = YUP;
         }
      } else cf->X->WarnClose = YUP;
   }
   cf->X->SwitchCmapLst = NULL;
   
   cf->MessageList = SUMA_CreateMessageList ();
   #ifdef USE_SUMA_MALLOC
   SUMA_SL_Err("NO LONGER SUPPORTED");
   return(NULL);
   /*SUMA_ShowMemTrace (cf->Mem, NULL);*/
   #endif
   cf->ROI_mode = NOPE;
   cf->Pen_mode = NOPE;
   
   cf->nimlROI_Datum_type = NI_rowtype_define("SUMA_NIML_ROI_DATUM", "int,int,int,int[#3]");
   if (cf->nimlROI_Datum_type < 0) {
      fprintf(SUMA_STDERR,"Error %s: Failed defining niml code.", FuncName);
      return(NULL);
   }
   if (LocalHead) fprintf(SUMA_STDERR, "%s: roi_type code = %d\n", FuncName, cf->nimlROI_Datum_type) ;
   
   cf->ROI_CM = NULL;
   cf->ROI_FillMode = SUMA_ROI_FILL_TO_THISROI;
   cf->ROI2afni = NOPE;
   
   eee = getenv("SUMA_ColorMixingMode");
   if (eee) {
      if (strcmp (eee, "ORIG") == 0) {
         cf->ColMixMode = SUMA_ORIG_MIX_MODE;
      } else if (strcmp (eee, "MOD1") == 0) {
         cf->ColMixMode = SUMA_4AML;
      } else {
         cf->ColMixMode = SUMA_ORIG_MIX_MODE;
         fprintf(SUMA_STDERR,"%s:\nUnrecognized option %s for SUMA_ColorMixingMode.\nUsing default = ORIG\n", FuncName, eee);
      } 
   } else {
      cf->ColMixMode = SUMA_ORIG_MIX_MODE;
   }
   
   cf->GroupList = NULL;
   cf->N_Group = -1;
   
   cf->scm = NULL;
   cf->DsetList = (DList *)SUMA_malloc(sizeof(DList));
   dlist_init (cf->DsetList, SUMA_FreeDset);
   
   return (cf);

}

/*!
\brief creates the structure for storing the radio buttons used to control viewer locking
   Do not use CommonFields structure here.

*/ 
SUMA_rb_group *SUMA_CreateLock_rbg (int N_rb_group, int N_but) 
{
   static char FuncName[]={"SUMA_CreateLock_rbg"};
   SUMA_rb_group *Lock_rb;

   Lock_rb = (SUMA_rb_group *) malloc(sizeof(SUMA_rb_group));
   if (!Lock_rb) { 
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate.\n", FuncName);
      return(NULL);
   }
   Lock_rb->N_rb_group = N_rb_group;
   Lock_rb->N_but = N_but;
   Lock_rb->tb = (Widget *) calloc(N_rb_group*N_but, sizeof(Widget));
   Lock_rb->rb = (Widget *) calloc(N_rb_group, sizeof(Widget));
   Lock_rb->atb = (Widget *) calloc(N_but, sizeof(Widget));
   Lock_rb->arb = NULL;
   if (!Lock_rb->tb || !Lock_rb->rb || !Lock_rb->atb) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate.\n", FuncName);
      return(NULL);
   }
   return(Lock_rb);

}

/*!
   free SUMA_rb_group *
   Do not use CommonFields structure here.
*/
void * SUMA_FreeLock_rbg (SUMA_rb_group *Lock_rb)
{
  static char FuncName[]={"SUMA_FreeLock_rb"};
  
  if (Lock_rb->rb) free(Lock_rb->rb);
  if (Lock_rb->tb) free(Lock_rb->tb);
  if (Lock_rb->atb) free (Lock_rb->atb);
  if (Lock_rb) free(Lock_rb);

  return (NULL);
}

/*!
   \brief DrawROI = SUMA_CreateDrawROIStruct();
   allocates and initializes structure of type 
   
   \return SUMA_X_DrawROI *
*/
SUMA_X_DrawROI *SUMA_CreateDrawROIStruct (void) 
{
   static char FuncName[]={"SUMA_CreateDrawROIStruct"};
   SUMA_X_DrawROI *DrawROI = NULL;
   
   /* do not use commonfields related stuff here for obvious reasons */
   DrawROI = (SUMA_X_DrawROI *)malloc (sizeof(SUMA_X_DrawROI));
   DrawROI->AppShell = NULL;
   DrawROI->ROIval = (SUMA_ARROW_TEXT_FIELD *)malloc(sizeof(SUMA_ARROW_TEXT_FIELD));
   DrawROI->ROIlbl = (SUMA_ARROW_TEXT_FIELD *)malloc(sizeof(SUMA_ARROW_TEXT_FIELD));
   DrawROI->curDrawnROI = NULL;  /* DO NOT FREE THIS POINTER */
   DrawROI->SwitchROIlst = NULL;
   DrawROI->Delete_first = YUP;
   DrawROI->SaveMode = SW_DrawROI_SaveMode1D;
   DrawROI->SaveWhat = SW_DrawROI_SaveWhatThis;
   return (DrawROI);
}

/*!
   \brief SumaCont = SUMA_CreateSumaContStruct();
   allocates and initializes structure of type SUMA_X_SumaCont
   \return SUMA_X_SumaCont *
   
*/
SUMA_X_SumaCont *SUMA_CreateSumaContStruct (void) 
{
   static char FuncName[]={"SUMA_CreateSumaContStruct"};
   SUMA_X_SumaCont *SumaCont = NULL;
   /* do not use commonfields related stuff here for obvious reasons */
   SumaCont = (SUMA_X_SumaCont *)malloc(sizeof(SUMA_X_SumaCont));
   SumaCont->AppShell = NULL;
   SumaCont->quit_pb = NULL;
   SumaCont->quit_first = YUP;
   SumaCont->Lock_rbg = SUMA_CreateLock_rbg (SUMA_MAX_SURF_VIEWERS, 3);
   if (!SumaCont->Lock_rbg) {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateLock_rb.\n", FuncName);
      return (NULL);
   }
   SumaCont->LockView_tbg = (Widget *)calloc (SUMA_MAX_SURF_VIEWERS, sizeof(Widget));
   SumaCont->LockAllView_tb = NULL;
   SumaCont->SumaInfo_TextShell = NULL;
   return (SumaCont);
}

/*!
   \brief frees structure SUMA_X_SumaCont, returns null
   
*/
void *SUMA_FreeSumaContStruct (SUMA_X_SumaCont *SumaCont)
{
   static char FuncName[]={"SUMA_FreeSumaContStruct"};

   /* do not use commonfields related stuff here for obvious reasons */
   if (SumaCont->Lock_rbg) SUMA_FreeLock_rbg (SumaCont->Lock_rbg);
   if (SumaCont->LockView_tbg) free (SumaCont->LockView_tbg);
   if (SumaCont->SumaInfo_TextShell) { SUMA_SL_Warn("SumaCont->SumaInfo_TextShell is not being freed") };
   if (SumaCont) free(SumaCont);
   return (NULL);
}

/*!
   \brief frees structure SUMA_X_DrawROI, returns null
*/
void *SUMA_FreeDrawROIStruct (SUMA_X_DrawROI *DrawROI)
{  
   static char FuncName[]={"SUMA_FreeDrawROIStruct"};
   
   /* do not use commonfields related stuff here for obvious reasons,
   Well, you can, it is no big deal, memory tracing variables are wiped out at the end*/
   if (DrawROI->ROIval) free (DrawROI->ROIval);
   if (DrawROI->ROIlbl) free (DrawROI->ROIlbl);
   if (DrawROI->SwitchROIlst) SUMA_FreeScrolledList (DrawROI->SwitchROIlst);
   if (DrawROI) free(DrawROI);
   
   return (NULL);
}
/*!
   \brief ViewCont = SUMA_CreateViewContStruct();
   allocates and initializes structure of type SUMA_X_ViewCont
   \return SUMA_X_ViewCont *
   
*/
SUMA_X_ViewCont *SUMA_CreateViewContStruct (void) 
{
   static char FuncName[]={"SUMA_CreateViewContStruct"};
   SUMA_X_ViewCont *ViewCont = NULL;
   /* do not use commonfields related stuff here for obvious reasons */
   ViewCont = (SUMA_X_ViewCont *)malloc(sizeof(SUMA_X_ViewCont));
   ViewCont->TopLevelShell = NULL;
   ViewCont->ViewerInfo_TextShell = NULL;
   ViewCont->Info_lb = NULL;
   ViewCont->ViewerInfo_pb = NULL;
   ViewCont->Mainform = NULL;
   ViewCont->SwitchGrouplst = NULL; 
   ViewCont->SwitchStatelst = NULL; 
   ViewCont->ViewerInfo_pb = NULL;
   return (ViewCont);
}

/*!
   \brief frees structure SUMA_X_ViewCont, returns null
   
*/
void *SUMA_FreeViewContStruct (SUMA_X_ViewCont *ViewCont)
{
   static char FuncName[]={"SUMA_FreeViewContStruct"};

   /* do not use commonfields related stuff here for obvious reasons */
   if (ViewCont->TopLevelShell) {
      SUMA_SL_Warn("ViewCont->TopLevelShell is not being freed");
   }
   if (ViewCont->SwitchGrouplst) ViewCont->SwitchGrouplst = SUMA_FreeScrolledList(ViewCont->SwitchGrouplst);
   if (ViewCont->SwitchStatelst) ViewCont->SwitchStatelst = SUMA_FreeScrolledList(ViewCont->SwitchStatelst);
   if (ViewCont) free(ViewCont);
   return (NULL);
}

/*!
   \brief SurfCont = SUMA_CreateSurfContStruct();
   allocates and initializes structure of type SUMA_X_SurfCont
   \return SUMA_X_SurfCont *
   
*/
SUMA_X_SurfCont *SUMA_CreateSurfContStruct (char *idcode_str) 
{
   static char FuncName[]={"SUMA_CreateSurfContStruct"};
   SUMA_X_SurfCont *SurfCont = NULL;
   
   
   /* do not use commonfields related stuff here for obvious reasons */
   SurfCont = (SUMA_X_SurfCont *)malloc(sizeof(SUMA_X_SurfCont));
   
   /* take care of linking fields */
   if (idcode_str) sprintf(SurfCont->owner_id, "%s", idcode_str);
   else SurfCont->owner_id[0] = '\0';
   SurfCont->N_links = 0;
   SurfCont->LinkedPtrType = SUMA_LINKED_SURFCONT_TYPE;
   
   SurfCont->ColPlane_fr = NULL;
   SurfCont->TopLevelShell = NULL;
   SurfCont->SurfInfo_pb = NULL;
   SurfCont->SurfInfo_label = NULL;
   SurfCont->SurfInfo_TextShell = NULL;
   SurfCont->ColPlaneOrder = (SUMA_ARROW_TEXT_FIELD *)malloc(sizeof(SUMA_ARROW_TEXT_FIELD));
   SurfCont->ColPlaneOpacity = (SUMA_ARROW_TEXT_FIELD *)malloc(sizeof(SUMA_ARROW_TEXT_FIELD));
   SurfCont->ColPlaneDimFact = (SUMA_ARROW_TEXT_FIELD *)malloc(sizeof(SUMA_ARROW_TEXT_FIELD));
   SurfCont->SetRangeTable = SUMA_AllocTableField();
   SurfCont->RangeTable = SUMA_AllocTableField();
   SurfCont->ColPlaneShow_tb = NULL;
   SurfCont->SwitchDsetlst = NULL;
   SurfCont->ColPlaneLabel_Parent_lb = NULL;
   SurfCont->curColPlane = NULL;
   SurfCont->PosRef = NULL;
   SurfCont->cmap_wid = NULL;
   SurfCont->cmap_context = NULL;
   SurfCont->thr_sc = NULL;
   SurfCont->brt_sc = NULL;
   SurfCont->thr_lb = NULL;
   SurfCont->thrstat_lb = NULL;
   SurfCont->cmaptit_lb = NULL;
   SurfCont->cmapswtch_pb = NULL;
   SurfCont->SwitchIntMenu = NULL;
   SurfCont->SwitchBrtMenu = NULL;
   SurfCont->SwitchThrMenu = NULL;
   SurfCont->SwitchCmapMenu = NULL;
   SurfCont->N_CmapMenu = -1;
   SurfCont->CoordBiasMenu[SW_CoordBias] = NULL;
   SurfCont->opts_rc = NULL;
   SurfCont->rcvo = NULL;
   SurfCont->rcsw = NULL;
   SurfCont->rcsw_v1 = NULL;
   SurfCont->rcsw_v2 = NULL;
   SurfCont->rcswr = NULL;
   SurfCont->rccm = NULL;
   SurfCont->rccm_swcmap = NULL;
   SurfCont->IntRange_lb = NULL;
   SurfCont->Int_tb = NULL;
   SurfCont->Thr_tb = NULL;
   SurfCont->Brt_tb = NULL;
   SurfCont->AutoIntRange = 1;
   SurfCont->AutoBrtRange = 1;
   /*SurfCont-> = NULL;
   SurfCont-> = NULL;
   SurfCont-> = NULL;*/
  return (SurfCont);
}
 
/*!
   \brief frees structure SUMA_X_SurfCont, returns null
   
*/
void *SUMA_FreeSurfContStruct (SUMA_X_SurfCont *SurfCont)
{
   static char FuncName[]={"SUMA_FreeSurfContStruct"};

   /* do not use commonfields related stuff here for obvious reasons */
   if (!SurfCont) return(NULL);
   
   if (SurfCont->N_links) {
      SurfCont = (SUMA_X_SurfCont*)SUMA_UnlinkFromPointer((void *)SurfCont);
      return (NULL);
   }
   
   /* no more links, go for it */
   if (SurfCont->ColPlaneOrder) free (SurfCont->ColPlaneOrder);
   if (SurfCont->ColPlaneOpacity) free (SurfCont->ColPlaneOpacity);
   if (SurfCont->ColPlaneDimFact) free (SurfCont->ColPlaneDimFact);
   if (SurfCont->SetRangeTable) SUMA_FreeTableField (SurfCont->SetRangeTable);
   if (SurfCont->RangeTable) SUMA_FreeTableField (SurfCont->RangeTable);
   if (SurfCont->SwitchDsetlst) SUMA_FreeScrolledList (SurfCont->SwitchDsetlst);
   if (SurfCont->SurfInfo_TextShell) { SUMA_SL_Warn("SurfCont->SurfInfo_TextShell is not being freed") };
   if (SurfCont->SwitchIntMenu) { XtDestroyWidget(SurfCont->SwitchIntMenu[0]); SUMA_free(SurfCont->SwitchIntMenu); }
   if (SurfCont->SwitchThrMenu) { XtDestroyWidget(SurfCont->SwitchThrMenu[0]); SUMA_free(SurfCont->SwitchThrMenu); }
   if (SurfCont->SwitchBrtMenu) { XtDestroyWidget(SurfCont->SwitchBrtMenu[0]); SUMA_free(SurfCont->SwitchBrtMenu); }
   if (SurfCont->SwitchCmapMenu) { XtDestroyWidget(SurfCont->SwitchCmapMenu[0]); SUMA_free(SurfCont->SwitchCmapMenu); }
   if (SurfCont) free(SurfCont);
   return (NULL);
}

/*! free SUMA_CommonFields 
NOTE: the SUMA_CommonFields * itself is not freed. You'll have to free it manually with free function;
\sa SUMA_Create_CommonFields
*/
SUMA_Boolean SUMA_Free_CommonFields (SUMA_CommonFields *cf)
{
   static char FuncName[]={"SUMA_Free_CommonFields"};
   int i;
   
   /* do not use commonfields related stuff here for obvious reasons */
   if (cf->GroupList) {
      for (i=0; i< cf->N_Group; ++i) if (cf->GroupList[i]) SUMA_free(cf->GroupList[i]);
      SUMA_free(cf->GroupList); cf->GroupList = NULL;
   }
   if (cf->ROI_CM) SUMA_Free_ColorMap(cf->ROI_CM); /* free the colormap */ cf->ROI_CM = NULL;
   if (cf->X->FileSelectDlg) SUMA_FreeFileSelectionDialogStruct(cf->X->FileSelectDlg); cf->X->FileSelectDlg = NULL;
   if (cf->X->SumaCont) SUMA_FreeSumaContStruct (cf->X->SumaCont); cf->X->SumaCont = NULL;
   if (cf->X->DrawROI) SUMA_FreeDrawROIStruct (cf->X->DrawROI); cf->X->DrawROI = NULL;
   if (cf->X->N_ForeSmooth_prmpt) SUMA_FreePromptDialogStruct (cf->X->N_ForeSmooth_prmpt); cf->X->N_ForeSmooth_prmpt = NULL;
   if (cf->X->SwitchCmapLst) SUMA_FreeScrolledList (cf->X->SwitchCmapLst);
   if (cf->X) free(cf->X); cf->X = NULL;
   if (cf->MessageList) SUMA_EmptyDestroyList(cf->MessageList); cf->MessageList = NULL;
   if (cf->scm) cf->scm = SUMA_DestroyAfniColors (cf->scm); cf->scm = NULL;
   if (cf->DsetList) {
      dlist_destroy(cf->DsetList); 
      SUMA_free(cf->DsetList); cf->DsetList = NULL;
   }
   #ifdef USE_SUMA_MALLOC
   SUMA_SL_Err("NO LONGER SUPPORTED");
   return(NOPE);
   if (cf->Mem) SUMA_Free_MemTrace (cf->Mem); cf->Mem = NULL;/* always free this right before the end */
   #endif
   
   /* if (cf) free(cf); */ /* don't free this stupid pointer since it is used
                        when main returns with SUMA_RETURN. 
                        It is not quite a leak since the OS will clean it up
                        after exit Thu Apr  8 2004*/
   
   return (YUP);
}

void SUMA_Show_CommonFields (SUMA_CommonFields *cf, FILE *out)
{
   static char FuncName[]={"SUMA_Show_CommonFields"};
   char *s=NULL;
   
   s = SUMA_CommonFieldsInfo (cf, 1);
   
   if (!out) fprintf(SUMA_STDERR,"%s", s);
   else fprintf(out,"%s", s);
   
   SUMA_RETURNe;
}

char * SUMA_CommonFieldsInfo (SUMA_CommonFields *cf, int detail)
{
   static char FuncName[]={"SUMA_CommonFieldsInfo"};
   int i;
   char *s=NULL;
   SUMA_STRING *SS=NULL;
   
   SUMA_ENTRY;

   SS = SUMA_StringAppend_va(NULL, NULL);
   
   if (cf == NULL) {
      SS = SUMA_StringAppend_va(SS," NULL cf structure.\n", FuncName);
      SS = SUMA_StringAppend_va(SS, NULL);
      s = SS->s; SUMA_free(SS); SS= NULL;
      SUMA_RETURN(s);
   }
   
   for (i=0; i < SUMA_MAX_STREAMS; ++i) {
      SS = SUMA_StringAppend_va(SS,"   HostName: %s\n", cf->HostName_v[i]);
      SS = SUMA_StringAppend_va(SS,"   NimlStream: %s\n", cf->NimlStream_v[i]);
   }
   
   SS = SUMA_StringAppend_va(SS,"   Available Groups: %d\n", cf->N_Group);
   for (i=0; i<cf->N_Group; ++i) {
      SS = SUMA_StringAppend_va(SS,"      Group[%d]: %s\n", i, cf->GroupList[i]);
   }
   
   #ifdef USE_SUMA_MALLOC
      SUMA_SL_Err("NO LONGER SUPPORTED");
      SUMA_RETURN(NULL);
   #else 
      SS = SUMA_StringAppend_va(SS,"   DBG_trace = %d\n", DBG_trace);
      SS = SUMA_StringAppend_va(SS,"   InOut_Notify = %d\n", cf->InOut_Notify);
      SS = SUMA_StringAppend_va(SS,"   MemTrace = %d\n", cf->MemTrace);
   #endif
   
   /* add the colormap info */
   if (cf->scm) {
      SS = SUMA_StringAppend(SS, "   Colormaps:\n");
      SS = SUMA_StringAppend(SS, SUMA_ColorMapVec_Info (cf->scm->CMv, cf->scm->N_maps, detail));
   } else {
      SS = SUMA_StringAppend_va(SS, "   No Colormaps.\n");
   }  
   
   /* clean up */
   SS = SUMA_StringAppend_va(SS, NULL);
   s = SS->s; SUMA_free(SS); SS= NULL;
   
   SUMA_RETURN(s);
}
/*! assign new afni host name 
    SUMA_Assign_HostName (cf, HostName, istream)
   
   Assigns a new HostName for niml communication on a particular stream
   
   \param cf (SUMA_CommonFields *) pointer to Common Fields structure, field AfniHostName will be modified here
   \param HostName (char *) hostname in IP number form, or name form afni.nimh.nih.gov or afni (if in /etc/hosts file)
                                 NULL to set cf->HostName_v[istream] to localhost if i = SUMA_AFNI_STREAM_INDEX
                                                                                 127.0.0.1 otherwise. That's done to keep
                                                                                 Shared Memory communication betwen AFNI 
                                                                                 and SUMA only.
   \param istream (int) if -1 then all streams are set to HostName
                        otherwise, only HostName_v[istream] is set
   \ret ans (SUMA_Boolean) YUP/NOPE
   
   
*/
SUMA_Boolean SUMA_Assign_HostName (SUMA_CommonFields *cf, char *HostName, int istream)
{
   static char FuncName[]={"SUMA_Assign_HostName"};
   int istart = 0, istop = 0, i = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (istream == -1) {
      istart = 0; istop = SUMA_MAX_STREAMS; 
   } else {
      istart = istream; istop = istream + 1;
   }
   
   for (i = istart; i < istop; ++i) {
      if (HostName == NULL)
         if (i == SUMA_AFNI_STREAM_INDEX) {
            sprintf(cf->HostName_v[i], "localhost"); /*  using localhost will allow the use of Shared Memory.
                                                         That is only allowed for SUMA<-->AFNI */
         } else {
            sprintf(cf->HostName_v[i], "127.0.0.1");  /* force TCP for the commoners */
         }  
      else {   
         if (strlen(HostName) > SUMA_MAX_NAME_LENGTH - 20) {
            fprintf(SUMA_STDERR,"Error %s: too long a host name (> %d chars).\n", FuncName, SUMA_MAX_NAME_LENGTH - 20);
            SUMA_RETURN (NOPE);
         }
         sprintf(cf->HostName_v[i],"%s", HostName);
      }

      sprintf(cf->NimlStream_v[i],"tcp:%s:%d", 
            cf->HostName_v[i], cf->TCP_port[i]);

      if (LocalHead) fprintf(SUMA_STDOUT, "%s: Set HostName %d to %s (stream name: %s)\n", 
                     FuncName, i, cf->HostName_v[i], cf->NimlStream_v[i]);
   }
   
   SUMA_RETURN (YUP);
}

/*!
   This function determines the most suitable standard view of a surface viewer
   This is based on the surface objects being displayed and their embedding dimension.
   The highest Embedding dimension of the lot determines what view to use 
   ans = SUMA_BestStandardView (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int N_dov)
   
   \param sv (SUMA_SurfaceViewer *) Surface viewer structure
   \param dov (SUMA_DO *) vector of displayable objects
   \param N_dov (int) number of displayable objects
   \ret ans (SUMA_STANDARD_VIEWS) recommended view
   
*/   
SUMA_STANDARD_VIEWS SUMA_BestStandardView (SUMA_SurfaceViewer *sv, SUMA_DO *dov, int N_dov)
{
   static char FuncName[] = {"SUMA_BestStandardView"};
   SUMA_STANDARD_VIEWS ans;
   int i, maxdim = -1, is;
   SUMA_SurfaceObject *SO = NULL;
   
   SUMA_ENTRY;

   is = sv->iState;
   if (is < 0) {
      fprintf(SUMA_STDERR, "Error %s: sv->iState undefined.\n", FuncName);
      SUMA_RETURN (SUMA_Dunno); 
   }
   
   for (i=0; i<sv->VSv[is].N_MembSOs; ++i) {   
      SO = (SUMA_SurfaceObject *)(dov[sv->VSv[is].MembSOs[i]].OP);
      if (SO == NULL) {
         fprintf(SUMA_STDERR,"Error %s: SO is null ???\n.", FuncName);
         SUMA_RETURN (SUMA_Dunno);
      }
      if (SO->EmbedDim > maxdim) maxdim = SO->EmbedDim;
   }
   
   switch (maxdim) {
      case 2:
         SUMA_RETURN (SUMA_2D_Z0);
      case 3:
         SUMA_RETURN(SUMA_3D);
      default:
         fprintf(SUMA_STDERR,"Error %s: No provision for such a maximum embedding dimension of %d.\n", FuncName, maxdim);
         SUMA_RETURN(SUMA_Dunno);
   }

}

/*!
   \brief Apply the group of a surface to the surface viewer 
*/
SUMA_Boolean SUMA_AdoptSurfGroup(SUMA_SurfaceViewer *csv, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_AdoptSurfGroup"};

   SUMA_ENTRY;

   csv->iCurGroup = SUMA_WhichGroup(SUMAg_CF, SO->Group);
   if (csv->iCurGroup < 0) {
      SUMA_SL_Err("Bad, unexpected error.\nGroup was not found");
      SUMA_RETURN(NOPE);
   }
   if (csv->CurGroupName) SUMA_free(csv->CurGroupName);

   csv->CurGroupName = SUMA_copy_string(SO->Group);
   SUMA_RETURN(YUP);
}

/*!
   \brief Apply a group to the surface viewer 
*/
SUMA_Boolean SUMA_AdoptGroup(SUMA_SurfaceViewer *csv, char *group)
{
   static char FuncName[]={"SUMA_AdoptSurfGroup"};

   SUMA_ENTRY;

   csv->iCurGroup = SUMA_WhichGroup(SUMAg_CF, group);
   if (csv->iCurGroup < 0) {
      SUMA_SL_Err("Bad, unexpected error.\nGroup was not found");
      SUMA_RETURN(NOPE);
   }
   if (csv->CurGroupName) SUMA_free(csv->CurGroupName);

   csv->CurGroupName = SUMA_copy_string(group);
   SUMA_RETURN(YUP);
}

/*!
ans = SUMA_SetupSVforDOs (Spec, DOv, N_DOv, cSV);

This functions registers all surfaces in a spec file with a surface viewer. 
The following steps are performed:
SUMA_RegisterSpecSO (register info on all surfaces loaded)
SUMA_RegisterDO (only Surface Objects)
SUMA_RegisterDO (all non SO objects)
SUMA_BestStandardView (decide on best standard view)
SUMA_UpdateRotaCenter (based on surfaces in first view)
SUMA_UpdateViewPoint (based on surfaces in first view)
SUMA_EyeAxisStandard (based on surfaces in first view)
Set the Current SO pointer to the first surface object 
if surface is SureFit, flip lights
\param Spec (SUMA_SurfSpecFile)
\param DOv (SUMA_DO *) Pointer to vector of displayable objects
\param N_DOv (int) Number of displayable objects in DOv
\param cSV (SUMA_SurfaceViewer *) Surface viewer structure
\ret ans (SUMA_Boolean) YUP/NOPE
*/

SUMA_Boolean SUMA_SetupSVforDOs (SUMA_SurfSpecFile Spec, SUMA_DO *DOv, int N_DOv, SUMA_SurfaceViewer *cSV)
{
   static char FuncName[] = {"SUMA_SetupSVforDOs"};
   int kar;
   SUMA_SurfaceObject *SO;
   SUMA_Axis *EyeAxis;
   int EyeAxis_ID;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   #if 0
   /* adds DOs individually, left for reference purposes */
   /* Register all DOs with SV */
   for (kar=0; kar < N_DOv; ++kar) {
      if (!SUMA_RegisterDO(kar, cSV)) {
         SUMA_error_message (FuncName,"Failed to register DO", 1);
         SUMA_RETURN(NOPE);
      }
   }

   /* register only the first surface and the remaining DOs */
   {
      SUMA_Boolean SurfIn = NOPE;
      for (kar=0; kar < N_DOv; ++kar) {
         if (!SUMA_isSO(DOv[kar]) || !SurfIn)
         { /* register the first surface only and other non SO objects */
            /*fprintf(SUMA_STDERR," to register DOv[%d] ...\n", kar);*/
            if (!SUMA_RegisterDO(kar, cSV)) {
               SUMA_error_message (FuncName,"Failed to register DO", 1);
               SUMA_RETURN(NOPE);
            }
         }
         if (SUMA_isSO(DOv[kar])) { SurfIn = YUP; }
      }
   }   
   #endif 

   #if 1
   /* register all surface specs */
      /* find out what group the viewer will have and assign the current group to be that of the new surfaces */
      if (LocalHead) {
         fprintf (SUMA_STDERR, "%s: Registering SpecSO with viewer [%d]%p, %d\n",
             FuncName, SUMA_WhichSV(cSV, SUMAg_SVv, SUMA_MAX_SURF_VIEWERS), cSV, SUMAg_N_SVv);
      }
      cSV->iCurGroup =  SUMA_WhichGroup(SUMAg_CF, Spec.Group[0]); /* only one group per spec */
      if (cSV->iCurGroup < 0) {
         SUMA_SL_Err("Group not found.\n");
         SUMA_RETURN(NOPE);
      } else {
         cSV->CurGroupName = SUMA_copy_string(SUMAg_CF->GroupList[cSV->iCurGroup]);
      }
      
      
      if (!SUMA_RegisterSpecSO(&Spec, cSV, DOv, N_DOv)) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterSpecSO.\n", FuncName);
         SUMA_RETURN(NOPE);
      } 
      SUMA_LH("Done.\n");

   /* register all SOs of the first state */   
      /*fprintf(SUMA_STDERR,"%s: Registering All SO of the first group ...", FuncName);*/
      cSV->State = cSV->VSv[0].Name;
      cSV->iState = 0;
      for (kar=0; kar < cSV->VSv[0].N_MembSOs; ++ kar) {
         /*fprintf(SUMA_STDERR," About to register DOv[%d] ...\n", cSV->VSv[0].MembSOs[kar]);*/
            if (!SUMA_RegisterDO(cSV->VSv[0].MembSOs[kar], cSV)) {
               SUMA_error_message (FuncName,"Failed to register DO", 1);
               SUMA_RETURN(NOPE);
            }
      }
      
      
   /*   fprintf(SUMA_STDERR,"%s: Done.\n", FuncName);*/

   /* register all non SO objects */
   /*   fprintf(SUMA_STDERR,"%s: Registering All Non SO ...", FuncName);*/
      for (kar=0; kar < N_DOv; ++kar) {
         if (!SUMA_isSO(DOv[kar]))
         { 
            /*fprintf(SUMA_STDERR," About to register DOv[%d] ...\n", kar);*/
            if (!SUMA_RegisterDO(kar, cSV)) {
               SUMA_error_message (FuncName,"Failed to register DO", 1);
               SUMA_RETURN(NOPE);
            }
         }
      }
   /*   fprintf(SUMA_STDERR,"%s: Done.\n", FuncName);*/
   #endif

   /* decide what the best state is */
   cSV->StdView = SUMA_BestStandardView (cSV, DOv, N_DOv);
   /*fprintf(SUMA_STDOUT,"%s: Standard View Now %d\n", FuncName, cSV->StdView);*/
   if (cSV->StdView == SUMA_Dunno) {
      fprintf(SUMA_STDERR,"Error %s: Could not determine the best standard view. Choosing default SUMA_3D\n", FuncName);
      cSV->StdView = SUMA_3D;
   }

   /* Set the Rotation Center */
   if (!SUMA_UpdateRotaCenter(cSV, DOv, N_DOv)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update center of rotation", FuncName);
      SUMA_RETURN(NOPE);
   }

   /* set the viewing points */
   if (!SUMA_UpdateViewPoint(cSV, DOv, N_DOv)) {
      fprintf (SUMA_STDERR,"Error %s: Failed to update view point", FuncName);
      SUMA_RETURN(NOPE);
   }

   /* Change the defaults of the eye axis to fit standard EyeAxis */
   EyeAxis_ID = SUMA_GetEyeAxis (cSV, DOv);
   if (EyeAxis_ID < 0) {
      fprintf (SUMA_STDERR,"Error %s: Failed to get Eye Axis.\n", FuncName);
      SUMA_RETURN(NOPE);
   }
   SUMA_EyeAxisStandard ((SUMA_Axis *)DOv[EyeAxis_ID].OP, cSV);


   /* Set the index Current SO pointer to the first surface object read of the first state, tiz NOT (Fri Jan 31 15:18:49 EST 2003) a surface of course*/
   cSV->Focus_SO_ID = cSV->VSv[0].MembSOs[0];
   /*set the GroupName info of the viewer correctly */
   SO = (SUMA_SurfaceObject *)(DOv[cSV->Focus_SO_ID].OP);
   if (!SUMA_AdoptSurfGroup(cSV,SO)) {
      SUMA_SL_Err("Failed to adopt surface's group");
      SUMA_RETURN(NOPE);
   }
   
   /* if surface is SureFit, flip lights */
   if (SO->FileType == SUMA_SUREFIT) {
      cSV->light0_position[2] *= -1;
   }


   SUMA_RETURN(YUP);
}

/*!
   \brief updates the cursor in all viewers 
*/
void SUMA_UpdateAllViewerCursor()
{
   static char FuncName[]={"SUMA_UpdateAllViewerCursor"};
   int i;
   
   SUMA_ENTRY;
   
   for (i=0; i<SUMAg_N_SVv; ++i) {
      if (SUMAg_SVv[i].X) {
         SUMA_UpdateViewerCursor(&(SUMAg_SVv[i]));
      } 
   }
   
   SUMA_RETURNe;
}

/*!
   \brief updates the cursor in one viewer
*/
void SUMA_UpdateViewerCursor(SUMA_SurfaceViewer *sv)   
{  
   static char FuncName[]={"SUMA_UpdateViewerCursor"};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   if (!sv->X) SUMA_RETURNe;
   if (!sv->X->GLXAREA) SUMA_RETURNe;
   if (SUMAg_CF->ROI_mode) {
      if (SUMAg_CF->Pen_mode) MCW_set_widget_cursor( sv->X->GLXAREA  , -XC_pencil ) ;
      else  MCW_set_widget_cursor( sv->X->GLXAREA  , -XC_target ) ;
   } else {
      MCW_set_widget_cursor( sv->X->GLXAREA  , -XC_top_left_arrow ) ;
   }
   SUMA_RETURNe;
}

/*!
   \brief updates the title string of a viewer window
*/

void SUMA_UpdateViewerTitle_old(SUMA_SurfaceViewer *sv)   
{  
   static char FuncName[]={"SUMA_UpdateViewerTitle_old"};
   int isv, i, N_SOlist, nalloc;  
   char slabel[30], sside[30], srec[10], cl='\0', cr='\0', smoment[30];   
   SUMA_SurfaceObject *SO = NULL;   
   int SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS];   
   SUMA_Boolean LeftSide, RightSide, RightShown, LeftShown;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!sv->X) SUMA_RETURNe;
   if (!sv->X->TOPLEVEL) SUMA_RETURNe;

   isv = SUMA_WhichSV (sv, SUMAg_SVv, SUMAg_N_SVv);   
   
   if (sv->X->Title) SUMA_free(sv->X->Title);
   sv->X->Title = NULL;
      
   if (isv >= 0) sprintf(slabel,"[%c] SUMA", 65+isv); 
   else sprintf(slabel,"[DOH] SUMA"); 
   
   N_SOlist = SUMA_RegisteredSOs(sv, SUMAg_DOv, SOlist);   
   
   i = 0; 
   nalloc = 0;  
   LeftSide = NOPE;
   LeftShown = NOPE;
   RightSide = NOPE;
   RightShown = NOPE;
   while (i < N_SOlist) {   
      SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SOlist[i]].OP);   
      if (SO->Label) { 
         nalloc +=  (strlen(SO->Label)+5);  
      }
      if (SO->Side == SUMA_LEFT) {
         SUMA_LH("Left found");
         LeftSide = YUP;
         if (sv->ShowLeft) LeftShown = YUP;
      } else if (SO->Side == SUMA_RIGHT) {
         SUMA_LH("Right found");
         RightSide = YUP;  
         if (sv->ShowRight) RightShown = YUP; 
      }
      
      ++i;   
   }
   if (LeftSide && LeftShown) cl = 'L';
   else if (LeftSide && !LeftShown) cl = 'h';
   else cl = 'x';
   if (RightSide && RightShown) cr = 'R';
   else if (RightSide && !RightShown) cr = 'h';
   else cr = 'x';
   
   
   sprintf(sside, ":%c%c:", cl, cr);
   
   if (sv->Record) sprintf(srec,":Rec");
   else srec[0] = '\0';
   
   if (sv->GVS[sv->StdView].ApplyMomentum) sprintf(smoment,":M");
   else smoment[0] = '\0';
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Found %d surface models.\n", FuncName, N_SOlist);
   
   i = 0; 
   if (N_SOlist >= 0) {   
      SUMA_LH("title surfaces found");
      sv->X->Title = (char *)SUMA_calloc(nalloc + strlen(slabel)+ 13, sizeof(char));      
      sv->X->Title[0] = '\0';
      while (i < N_SOlist) {   
         SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SOlist[i]].OP);   
         if (LocalHead) fprintf (SUMA_STDERR,"%s: sv->Focus_SO_ID = %d,  SOlist[%d] = %d\n", FuncName, sv->Focus_SO_ID, i, SOlist[i]);
         if (!i)  {
            if (sv->Focus_SO_ID == SOlist[i]) {
               sprintf (sv->X->Title,"%s%s%s%s [%s]", slabel, srec, smoment, sside, SO->Label); 
            } else {
               sprintf (sv->X->Title,"%s%s%s%s %s", slabel, srec, smoment, sside, SO->Label); 
            }
         } else {
            sv->X->Title = strcat (sv->X->Title, " & ");
            if (sv->Focus_SO_ID == SOlist[i]) {
               sv->X->Title = strcat (sv->X->Title, " [");
               sv->X->Title = strcat (sv->X->Title, SO->Label); 
               sv->X->Title = strcat (sv->X->Title, "] "); 
            } else  {
               sv->X->Title = strcat (sv->X->Title, SO->Label); 
            }
         }
         ++i;   
      }  
   } else {   
      SUMA_LH("No title could be made up");
      sv->X->Title = (char *)SUMA_calloc(strlen(slabel)+3, sizeof(char));  
      sprintf (sv->X->Title,"%s:-", slabel);   
   }  
   
   XtVaSetValues(sv->X->TOPLEVEL,  
            XmNtitle, sv->X->Title,  
            NULL);
            
   SUMA_RETURNe;   
}
/*!
   \brief updates the title string of a viewer window
*/

void SUMA_UpdateViewerTitle(SUMA_SurfaceViewer *sv)   
{  
   static char FuncName[]={"SUMA_UpdateViewerTitle"};
   int isv, i, N_SOlist;  
   char cl='\0', cr='\0', *s=NULL;   
   SUMA_SurfaceObject *SO = NULL;   
   int SOlist[SUMA_MAX_DISPLAYABLE_OBJECTS];   
   SUMA_STRING *SS = NULL;
   SUMA_Boolean LeftSide, RightSide, RightShown, LeftShown;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!sv->X) SUMA_RETURNe;
   if (!sv->X->TOPLEVEL) SUMA_RETURNe;

   SUMA_LH("Finding SV");
   isv = SUMA_WhichSV (sv, SUMAg_SVv, SUMAg_N_SVv);   
   
   if (sv->X->Title) SUMA_free(sv->X->Title);
   sv->X->Title = NULL;
   
   SS = SUMA_StringAppend_va(NULL, NULL);
   
   SUMA_LH("Number");
   if (isv >= 0) SS = SUMA_StringAppend_va(SS, "[%c] SUMA", 65+isv); 
   else SS = SUMA_StringAppend_va(SS,"[DOH] SUMA"); 
   
   SUMA_LH("Rec");
   if (sv->Record) SS = SUMA_StringAppend_va(SS,":Rec");
   
   SUMA_LH("Momentum");
   if (sv->GVS[sv->StdView].ApplyMomentum) SS = SUMA_StringAppend_va(SS,":M");
   
   SUMA_LH("Surf List");
   N_SOlist = SUMA_RegisteredSOs(sv, SUMAg_DOv, SOlist);   
   
   i = 0; 
   LeftSide = NOPE;
   LeftShown = NOPE;
   RightSide = NOPE;
   RightShown = NOPE;
   while (i < N_SOlist) {   
      SUMA_LH("   + +");
      SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SOlist[i]].OP);   
      if (SO->Side == SUMA_LEFT) {
         SUMA_LH("Left found");
         LeftSide = YUP;
         if (sv->ShowLeft) LeftShown = YUP;
      } else if (SO->Side == SUMA_RIGHT) {
         SUMA_LH("Right found");
         RightSide = YUP;  
         if (sv->ShowRight) RightShown = YUP; 
      }
      
      ++i;   
   }
   
   if (LeftSide && LeftShown) cl = 'L';
   else if (LeftSide && !LeftShown) cl = 'h';
   else cl = 'x';
   if (RightSide && RightShown) cr = 'R';
   else if (RightSide && !RightShown) cr = 'h';
   else cr = 'x';
   
   SUMA_LH("Sides");
   
   SS = SUMA_StringAppend_va(SS, ":%c%c:", cl, cr);
   
   if (LocalHead) fprintf (SUMA_STDERR, "%s: Found %d surface models.\n", FuncName, N_SOlist);
   
   /* add the group's name */
   if (LocalHead) {
      if (sv->CurGroupName) fprintf (SUMA_STDERR, "%s: Calling with sv->CurGroupName = %p\n", FuncName, sv->CurGroupName);
      else fprintf (SUMA_STDERR, "%s: Calling with NULL sv->CurGroupName\n", FuncName);
   }
      
   if (sv->CurGroupName) SS = SUMA_StringAppend_va(SS," %s:", sv->CurGroupName);
   else SS = SUMA_StringAppend_va(SS," xx:");
   
   i = 0; 
   if (N_SOlist >= 0) {   
      SUMA_LH("title surfaces found");
      while (i < N_SOlist) {   
         SO = (SUMA_SurfaceObject *)(SUMAg_DOv[SOlist[i]].OP);   
         if (LocalHead) fprintf (SUMA_STDERR,"%s: sv->Focus_SO_ID = %d,  SOlist[%d] = %d\n", FuncName, sv->Focus_SO_ID, i, SOlist[i]);
         if (!i)  {
            if (sv->Focus_SO_ID == SOlist[i]) {
               SS = SUMA_StringAppend_va(SS," [%s]",  SO->Label); 
            } else {
               SS = SUMA_StringAppend_va(SS," %s",  SO->Label); 
            }
         } else {
            SS = SUMA_StringAppend_va(SS," & ");
            if (sv->Focus_SO_ID == SOlist[i]) {
               SS = SUMA_StringAppend_va(SS, " [");
               SS = SUMA_StringAppend_va(SS, "%s", SO->Label); 
               SS = SUMA_StringAppend_va(SS, "] "); 
            } else  {
               SS = SUMA_StringAppend_va(SS, "%s", SO->Label); 
            }
         }
         ++i;   
      }  
   } else {   
      SUMA_LH("No title could be made up");
      SS = SUMA_StringAppend_va(SS,":-");   
   }  
   
   /* compact SS */
   SS = SUMA_StringAppend_va(SS, NULL);
   
   sv->X->Title = SS->s;
   
   SUMA_free(SS); SS= NULL;
   
   XtVaSetValues(sv->X->TOPLEVEL,  
            XmNtitle, sv->X->Title,  
            NULL);
            
   SUMA_RETURNe;   
}

/*!
   \brief finds the index into the grouplist of a certain group
*/
int SUMA_WhichGroup (SUMA_CommonFields *cf, char *nm)
{
   static char FuncName[]={"SUMA_WhichGroup"};
   int i = -1;
   
   SUMA_ENTRY;
   
   if (!nm || !cf) {
      SUMA_SL_Err("Null nm or cf");
      SUMA_RETURN(i);
   }
   
   if (cf->N_Group <=0) { SUMA_RETURN(i); }
   
   for (i=0; i<cf->N_Group; ++i) {
      if (!strcmp(cf->GroupList[i], nm)) SUMA_RETURN(i);
   } 
   
   SUMA_RETURN(-1);
}
/*!
   \brief Register a new group with SUMA 
*/
SUMA_Boolean SUMA_RegisterGroup (SUMA_CommonFields *cf, SUMA_SurfSpecFile *spec)
{
   static char FuncName[]={"SUMA_RegisterGroup"};
   int n=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (spec->N_Groups != 1) {
      SUMA_SL_Err("Spec->N_Groups != 1. This is unacceptable.\n");
      SUMA_RETURN(NOPE);
   }
   
   if (!cf->GroupList){
      cf->GroupList = (char **) SUMA_malloc(sizeof(char*)*SUMA_MAX_N_GROUPS);
      for (n=0; n<SUMA_MAX_N_GROUPS; ++n) cf->GroupList[n]=NULL;
      cf->N_Group = 0;
   }
   
   /* does the group exist already ? */
   if (SUMA_WhichGroup (cf, spec->Group[0]) < 0) {
      /* new group */
      SUMA_LH("Adding group");
      if (cf->N_Group >=  SUMA_MAX_N_GROUPS) {
         SUMA_SL_Err("Exceeding maximum number of groups allowed.\n");
         SUMA_RETURN(NOPE);
      }
      cf->GroupList[cf->N_Group] = SUMA_copy_string(spec->Group[0]);
      ++cf->N_Group;
   } else{ 
      /* an old group */
      SUMA_LH("Group exists already");
   }
   SUMA_RETURN(YUP);
   
}

/*!
   \brief Returns a list of the Groups available to a viewer. 
   
   \param sv (SUMA_SurfaceViewer *) pointer to surface viewer
   
   \return clist (SUMA_ASSEMBLE_LIST_STRUCT *) pointer to structure containing results
   
   \sa SUMA_FreeAssembleListStruct
   \sa SUMA_CreateAssembleListStruct
   
*/
SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleGroupList (SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_AssembleGroupList"};
   SUMA_ASSEMBLE_LIST_STRUCT *clist_str = NULL;
   int i=-1, N_clist=-1; 
   char *store=NULL;
   char **clist=NULL;
   void **oplist=NULL;
   DList *list=NULL, *listop = NULL;
   DListElmt *Elm = NULL, *Elmop = NULL;
   SUMA_Boolean Found = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   list = (DList *)SUMA_malloc(sizeof(DList));
   listop = (DList *)SUMA_malloc(sizeof(DList));
   
   clist = NULL;
   N_clist = -1;
   
   dlist_init(list, NULL);
   dlist_init(listop, NULL);
   
   for (i=0; i< SUMAg_CF->N_Group; ++i) {
      store = SUMA_copy_string(SUMAg_CF->GroupList[i]);
      if (!list->size) {
         dlist_ins_next(list, dlist_tail(list), (void*)store);
         dlist_ins_next(listop, dlist_tail(listop), NULL);
      } else { /* must sort first */
         Elm = NULL;
         Elmop = NULL;
         do {
            Found = NOPE;
            if (!Elm) {
               Elm = dlist_head(list);
               Elmop = dlist_head(listop);
            } else {
               Elm = dlist_next(Elm);
               Elmop = dlist_next(Elmop);
            }

            if (strcmp(store, (char*)Elm->data) <= 0) {
               dlist_ins_prev(list, Elm, (void *)store);
               dlist_ins_prev(listop, Elmop, NULL);
               Found = YUP;
            } else if (Elm == dlist_tail(list)) {
               /* reached the end, append */
               dlist_ins_next(list, Elm, (void *)store);
               dlist_ins_next(listop, Elmop, NULL);
               Found = YUP;
            }
         } while (!Found);
      }
   }
   
   if (!list->size) { /* Nothing found */
      N_clist = 0;
      
   }else {
   
      Elm = NULL;
      Elmop = NULL;
      clist = (char **)SUMA_calloc(list->size, sizeof(char *));
      oplist = (void **)SUMA_calloc(list->size, sizeof(void*));
      for (i=0; i< list->size; ++i) {
         if (!Elm) {
            Elm = dlist_head(list);
            Elmop = dlist_head(listop);
         } else {
            Elm = dlist_next(Elm);
            Elmop = dlist_next(Elmop);
         }
         clist[i] = (char*)Elm->data;
         oplist[i] = Elmop->data;
      }

      N_clist = list->size;
      /* destroy list */
      dlist_destroy(list);
      dlist_destroy(listop);
      SUMA_free(list);
      SUMA_free(listop);
   }
   
   clist_str = SUMA_CreateAssembleListStruct();
   clist_str->clist = clist;
   clist_str->oplist = oplist;
   clist_str->N_clist = N_clist;
   
   /* return */
   SUMA_RETURN (clist_str);  
}

/*!
   \brief   Switch viewer between two groups
*/
SUMA_Boolean SUMA_SwitchGroups (SUMA_SurfaceViewer *sv, char *group) 
{
   static char FuncName[]={"SUMA_SwitchGroups"};
   int ig, i, nxtstateID;
   SUMA_SurfaceObject *SO = NULL;
   DList *list = NULL;      
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!group) {
      SUMA_SL_Err("NULL group");
      SUMA_RETURN(NOPE);
   }
   
   if (!strcmp(group, sv->CurGroupName)) {
      SUMA_LH("Same group, nothing to do.");
      SUMA_RETURN(YUP);
   }
   
   if (SUMAg_CF->N_Group == 1) {
      SUMA_LH("One group, nothing to do.");
      SUMA_RETURN(YUP);
   }
   
   /* which group are we going to ? */
   ig = SUMA_WhichGroup (SUMAg_CF, group);
   
   if (ig < 0) {
      SUMA_SL_Err("No such group");
      SUMA_RETURN(NOPE);
   }
   
   /* It does not seem necessary to close surface controllers or ROI controllers*/

   /* find me a surface in that new group  */
   SO = NULL;
   i = 0;
   while (!SUMA_isSO_G(SUMAg_DOv[i], group) && i < SUMAg_N_DOv) {
      ++i;
   } 
   if (i < SUMAg_N_DOv) { /* found a surface */
      SO = (SUMA_SurfaceObject *)SUMAg_DOv[i].OP;
   } else {
      SUMA_SL_Err("No candidate surface");
      SUMA_RETURN(NOPE);
   } 
   
   /* what is the state ID of that surface ? */
   nxtstateID = SUMA_WhichState(SO->State, sv, SO->Group);
   if (nxtstateID < 0) {
      SUMA_SL_Err("Bad! State not found.");
      SUMA_RETURN(NOPE);
   }
   
   if (!SUMA_SwitchState (SUMAg_DOv, SUMAg_N_DOv, sv,  nxtstateID, group)) {
      SUMA_SL_Err("Failed to switch states");
      SUMA_RETURN(NOPE);
   }  

   /* home call */
   
   #if 0
      /* now redisplay (won't work alone with multiple viewers, GL state problems) */
      if (!list) list = SUMA_CreateList();
      /* SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Home, SES_Suma, sv); */
      SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay, SES_Suma, sv);

      if (!SUMA_Engine (&list)) {
         fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
      }
   #elif 0
                  /* redisplay all others (won't work alone with multiple viewers, GL state problems) */
                  if (!list) list = SUMA_CreateList ();
                  SUMA_REGISTER_TAIL_COMMAND_NO_DATA(list, SE_RedisplayNow_AllOtherVisible, SES_SumaWidget, sv);
                  SUMA_Engine (&list);
               
                  /* redisplay . DO NOT REDISPLAY WITH SE_Redisplay_AllVisible or you will have GL state synchronization problems */
                  sv->ResetGLStateVariables = YUP;
                  SUMA_handleRedisplay((XtPointer)sv->X->GLXAREA);
   #elif 1
            
            /* got to do this, in addition to SE_Redisplay_AllVisible
            to get the views to look good. I don't know why that is yet */
            for (i=0; i < SUMAg_N_SVv; ++i) {
               SUMA_SurfaceViewer *svtmp= &(SUMAg_SVv[i]);
               if (!svtmp->isShaded && svtmp->X->TOPLEVEL) {
                  if (!list) list = SUMA_CreateList();
                  SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Home, SES_Suma, svtmp); 
                  SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_RedisplayNow, SES_Suma, svtmp); 
                  if (!SUMA_Engine (&list)) {
                        fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
                  }
               }
            }
            
            if (!list) list = SUMA_CreateList();
            SUMA_REGISTER_HEAD_COMMAND_NO_DATA(list, SE_Redisplay_AllVisible, SES_Suma, sv);
            if (!SUMA_Engine (&list)) {
                           fprintf(stderr, "Error %s: SUMA_Engine call failed.\n", FuncName);
            }
            
   #endif

   /* update titles */
   SUMA_UpdateViewerTitle(sv);
   
   
   SUMA_RETURN(YUP);
}
