
#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF;
extern int SUMAg_N_DOv; 
extern SUMA_DO *SUMAg_DOv;

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
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);   
   
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
   int i, j;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

      SV->clear_color[0] = SUMA_CLEAR_COLOR_R;
      SV->clear_color[1] = SUMA_CLEAR_COLOR_G;
      SV->clear_color[2] = SUMA_CLEAR_COLOR_B;
      SV->clear_color[3] = SUMA_CLEAR_COLOR_A;
      
      SV->WindWidth = 350;
      SV->WindHeight = 350;
      
      SV->Open = NOPE;
      
      SV->ShowDO = (int *)SUMA_calloc( SUMA_MAX_DISPLAYABLE_OBJECTS, sizeof(int));
      if (SV->ShowDO == NULL) {
         fprintf(stderr,"Error SUMA_Alloc_SurfaceViewer_Struct: Failed to SUMA_malloc SV->ShowDO\n");
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
      SV->BrushStroke = NULL;
   }
   SUMA_RETURN (SVv);
}

SUMA_Boolean SUMA_Free_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV)
{
   static char FuncName[]={"SUMA_Free_SurfaceViewer_Struct"};
   int i;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (SV->Ch) SUMA_Free_CrossHair (SV->Ch);
   if (SV->X->ViewCont) SUMA_FreeViewContStruct(SV->X->ViewCont);
   if (SV->X) SUMA_free(SV->X);
   if (SV->ShowDO) SUMA_free(SV->ShowDO);
   if (SV->VSv) {
      for (i=0; i < SV->N_VSv; ++i) {
         if (!SUMA_Free_ViewState (&(SV->VSv[i]))) {
            fprintf (SUMA_STDERR,"Error %s: failed in SUMA_Free_ViewState.\n", FuncName);
         }
      }
   }
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
   
   if (SV->BrushStroke) {
      SUMA_ClearBrushStroke (SV);
   }
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_Free_SurfaceViewer_Struct_Vect (SUMA_SurfaceViewer *SVv, int N)
{
   static char FuncName[]={"SUMA_Free_SurfaceViewer_Struct_Vect"};
   int i;
   SUMA_Boolean Ret= YUP;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   sv->ColList[sv->N_ColList].glar_ColorList = (GLfloat *) SUMA_calloc (SO->N_Node*4, sizeof(GLfloat));
   sv->ColList[sv->N_ColList].idcode_str = (char *)SUMA_malloc((strlen(SO->idcode_str)+1) * sizeof(char));
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   Set Remix flags for all surfaces in sv->ShowDO regardless of their relationship.
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
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   for (k=0; k < sv->N_ColList; ++k) {
      sv->ColList[k].Remix = YUP;
   }
   
   SUMA_RETURN (YUP);
}

/*!
   ans = SUMA_SetLocalRemixFlag (char *idcode_str, SUMA_SurfaceViewer *sv);
   Search ShowDO for sv and if a Surface in ShowDO is related 
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   
   /* search for relatives in ShowDO */
   for (k=0; k < sv->N_DO; ++k) {
      SO2 = (SUMA_SurfaceObject *)SUMAg_DOv[sv->ShowDO[k]].OP;
      if (SUMA_isRelated (SO1, SO2)) {
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
   Search ShowDO for each Surface Viewer and if a Surface in ShowDO is related 
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
   SUMA_Boolean Found = NOPE, LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
      /* search for relatives in ShowDO */
      for (k=0; k < sv->N_DO; ++k) {
         if (SUMA_isSO(SUMAg_DOv[sv->ShowDO[k]])) {
            SO2 = (SUMA_SurfaceObject *)SUMAg_DOv[sv->ShowDO[k]].OP;
            if (SUMA_isRelated (SO1, SO2)) {
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
Updates the View Center and view from of SV based on the contents of ShowDO
*/

SUMA_Boolean SUMA_UpdateViewPoint (SUMA_SurfaceViewer *SV, SUMA_DO *dov, int N_dov)
{
   int i, do_id, TotWeight;
   float NewCenter[3];
   SUMA_SurfaceObject *so_op;
   static char FuncName[]={"SUMA_UpdateViewPoint"};
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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

   SUMA_RETURN (YUP);
   
   
}
/*!
Updates the Rotation Center of SV based on the contents of ShowDO
*/
SUMA_Boolean SUMA_UpdateRotaCenter (SUMA_SurfaceViewer *SV, SUMA_DO *dov, int N_dov)
{
   int i, do_id, TotWeight;
   float NewCenter[3];
   SUMA_SurfaceObject *so_op;
   static char FuncName[]={"SUMA_UpdateRotaCenter"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   SUMA_RETURN (YUP);
   
}

/*!
output the state variable contents of the Surface Viewer 
*/
void SUMA_Show_SurfaceViewer_Struct (SUMA_SurfaceViewer *SV, FILE *Out)
{
   int i;
   static char FuncName[]={"SUMA_Show_SurfaceViewer_Struct"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   SUMA_RETURNe;
}

/*! Show the ViewState structure */
SUMA_Boolean SUMA_Show_ViewState(SUMA_ViewState *VS, FILE *Out) 
{
   static char FuncName[]={"SUMA_Show_ViewState"};
   int i;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (Out == NULL) Out = stdout;

   if (VS == NULL) {
      fprintf(Out,"VS is NULL\n");      
      SUMA_RETURN(NOPE);
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
   
   SUMA_RETURN (YUP);
}

/*!
   Create & free ViewState_Hist structure 
*/
SUMA_ViewState_Hist *SUMA_Alloc_ViewState_Hist (void)
{
   static char FuncName[]={"SUMA_Alloc_ViewState_Hist"};
   SUMA_ViewState_Hist *vsh;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   vsh = (SUMA_ViewState_Hist *)SUMA_malloc(sizeof(SUMA_ViewState_Hist));
   if (vsh == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Could not allocate for vsh.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   vsh->ShowDO = NULL;
   vsh->N_DO = 0;
   SUMA_RETURN (vsh);
}   
SUMA_Boolean SUMA_Free_ViewState_Hist (SUMA_ViewState_Hist *vsh)
{
   static char FuncName[]={"SUMA_Free_ViewState_Hist"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (vsh == NULL) SUMA_RETURN (YUP);
   if (vsh->ShowDO) SUMA_free(vsh->ShowDO);
   if (vsh) SUMA_free(vsh);
   SUMA_RETURN (YUP);
}

/*!
   Create & free SUMA_ViewState structure 
*/
SUMA_ViewState *SUMA_Alloc_ViewState (int N)
{
   SUMA_ViewState *vs;
   int i;
   static char FuncName[]={"SUMA_Alloc_ViewState"};
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   vs = (SUMA_ViewState *)SUMA_malloc(sizeof(SUMA_ViewState)*N);
   if (vs == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Could not allocate for vs.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   for (i=0; i< N; ++i) {
      vs[i].Name = NULL;
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
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (vs == NULL) SUMA_RETURN (YUP);
   if (vs->Name) SUMA_free(vs->Name);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
int SUMA_WhichState (char *state, SUMA_SurfaceViewer *csv)
{
   static char FuncName[]={"SUMA_WhichState"};
   int i = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   while (i < csv->N_VSv) {
      if (LocalHead) fprintf(SUMA_STDERR,"%s: comparing csv->VSv[%d].Name = %s to %s ...\n", FuncName, i, csv->VSv[i].Name, state);
      if (strcmp(csv->VSv[i].Name, state) == 0) {
         if (LocalHead) fprintf(SUMA_STDERR,"%s: FOUND, i=%d!\n", FuncName, i);
         SUMA_RETURN (i);
      }
      ++i;
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
   SUMA_SurfaceObject * SO;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   /* allocate for space depending on the number of states present */
   
   if (LocalHead) fprintf(SUMA_STDERR,"%s: Entering ...\n", FuncName);
   
   csv->VSv = SUMA_Alloc_ViewState (Spec->N_States);
   if (csv->VSv == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed to allocate for VSv.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   csv->N_VSv = 0;
   
   /* register the various states from each SO in DOv */
   for (i=0; i < N_dov; ++i) {
      if (SUMA_isSO(dov[i])) {
         SO = (SUMA_SurfaceObject *)(dov[i].OP);
         if (csv->N_VSv == 0) {
            /* delaware encountered, snag it*/
            csv->VSv[csv->N_VSv].Name = (char *)SUMA_malloc(sizeof(char)*(strlen(SO->State)+1));
            if (csv->VSv[csv->N_VSv].Name == NULL) {
               fprintf(SUMA_STDERR,"Error %s: Failed to allocate for csv->VSv[csv->N_VSv].Name.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            csv->VSv[csv->N_VSv].Name = strcpy (csv->VSv[csv->N_VSv].Name, SO->State);  
            csv->VSv[csv->N_VSv].N_MembSOs = 1;
            csv->N_VSv += 1;
         }else {
            is = SUMA_WhichState (SO->State, csv);
            if (is < 0) {
               /* add state if it is a new one */
               csv->VSv[csv->N_VSv].Name = (char *)SUMA_malloc(sizeof(char)*(strlen(SO->State)+1));
               if (csv->VSv[csv->N_VSv].Name == NULL) {
                  fprintf(SUMA_STDERR,"Error %s: Failed to allocate for csv->VSv[csv->N_VSv].Name.\n", FuncName);
                  SUMA_RETURN (NOPE);
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
   csv->FOV = (float *)SUMA_calloc(csv->N_VSv, sizeof(float));
   
   /* allocate space for MembSOs counters will be reset for later use counting proceeds
   also initialize FOV*/
   for (i=0; i < csv->N_VSv; ++i) {
      csv->FOV[i] = FOV_INITIAL;
      csv->VSv[i].MembSOs = (int *) SUMA_calloc(csv->VSv[i].N_MembSOs, sizeof(int));
      if (csv->VSv[i].MembSOs == NULL) {
         fprintf(SUMA_STDERR,"Error %s: Failed to allocate for csv->VSv[i].MembSOs.\n", FuncName);
         SUMA_RETURN (NOPE);
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
            SUMA_RETURN (NOPE);
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

   SUMA_RETURN (YUP);
}

/*! allocate and intialize SUMA_CommonFields */
SUMA_CommonFields * SUMA_Create_CommonFields ()
{
   static char FuncName[]={"SUMA_Create_CommonFields"};
   SUMA_CommonFields *cf;
   int i;
   
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
   cf->Mem = SUMA_Create_MemTrace();
   
   cf->ns = NULL;
   cf->Connected = NOPE;
   for (i=0; i<SUMA_MAX_SURF_VIEWERS; ++i) {
      cf->Locked[i] = SUMA_I_Lock;
      cf->ViewLocked[i] = NOPE;
   }
   
   cf->SwapButtons_1_3 = SUMA_SWAP_BUTTONS_1_3;
   
   cf->X = (SUMA_X_AllView *)malloc(sizeof(SUMA_X_AllView));
   if (!cf->X) {
     fprintf(SUMA_STDERR,"Error %s: Failed to allocate.\n", FuncName);
     SUMA_RETURN (NULL); 
   }
   cf->X->SumaCont = SUMA_CreateSumaContStruct();
   cf->X->DPY_controller1 = NULL;
   cf->X->X_Resources = SXR_NP;
   cf->X->Help_TextShell = NULL;
   cf->X->Log_TextShell = NULL;
   cf->MessageList = SUMA_CreateMessageList ();
   /*SUMA_ShowMemTrace (cf->Mem, NULL);*/
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
   if (SumaCont) free(SumaCont);
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
   
   return (ViewCont);
}

/*!
   \brief frees structure SUMA_X_ViewCont, returns null
   
*/
void *SUMA_FreeViewContStruct (SUMA_X_ViewCont *ViewCont)
{
   static char FuncName[]={"SUMA_FreeViewContStruct"};

   /* do not use commonfields related stuff here for obvious reasons */
   if (ViewCont) free(ViewCont);
   return (NULL);
}

/*!
   \brief SurfCont = SUMA_CreateSurfContStruct();
   allocates and initializes structure of type SUMA_X_SurfCont
   \return SUMA_X_SurfCont *
   
*/
SUMA_X_SurfCont *SUMA_CreateSurfContStruct (void) 
{
   static char FuncName[]={"SUMA_CreateSurfContStruct"};
   SUMA_X_SurfCont *SurfCont = NULL;
   /* do not use commonfields related stuff here for obvious reasons */
   SurfCont = (SUMA_X_SurfCont *)malloc(sizeof(SUMA_X_SurfCont));
   SurfCont->TopLevelShell = NULL;
   SurfCont->SurfInfo_pb = NULL;
   SurfCont->SurfInfo_TextShell = NULL;
   
   return (SurfCont);
}

/*!
   \brief frees structure SUMA_X_SurfCont, returns null
   
*/
void *SUMA_FreeSurfContStruct (SUMA_X_SurfCont *SurfCont)
{
   static char FuncName[]={"SUMA_FreeSurfContStruct"};

   /* do not use commonfields related stuff here for obvious reasons */
   if (SurfCont) free(SurfCont);
   return (NULL);
}

/*! free SUMA_CommonFields */
SUMA_Boolean SUMA_Free_CommonFields (SUMA_CommonFields *cf)
{
   static char FuncName[]={"SUMA_Free_CommonFields"};
   
   /* do not use commonfields related stuff here for obvious reasons */
   
   if (cf->Mem) SUMA_Free_MemTrace (cf->Mem);
   if (cf->X->SumaCont) SUMA_FreeSumaContStruct (cf->X->SumaCont);
   if (cf->X) free(cf->X);
   if (cf->MessageList) SUMA_DestroyList(cf->MessageList);
   if (cf) free(cf);
   
   return (YUP);
}

void SUMA_Show_CommonFields (SUMA_CommonFields *cf)
{
   static char FuncName[]={"SUMA_Show_CommonFields"};
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (cf == NULL) {
      fprintf (SUMA_STDOUT,"%s: NULL structure.\n", FuncName);
      SUMA_RETURNe;
   }
   fprintf (SUMA_STDOUT,"%s: AfniHostName: %s\n", FuncName, cf->AfniHostName);
   fprintf (SUMA_STDOUT,"%s: NimlAfniStream: %s\n", FuncName, cf->NimlAfniStream);
   SUMA_RETURNe;
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

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (AfniHostName == NULL)
      sprintf(cf->AfniHostName, "localhost");
   else {   
      if (strlen(AfniHostName) > SUMA_MAX_NAME_LENGTH - 20) {
         fprintf(SUMA_STDERR,"Error %s: too long a host name (> %d chars).\n", FuncName, SUMA_MAX_NAME_LENGTH - 20);
         SUMA_RETURN (NOPE);
      }
      sprintf(cf->AfniHostName,"%s", AfniHostName);
   }

   sprintf(cf->NimlAfniStream,"tcp:%s:53211", cf->AfniHostName);

   fprintf(SUMA_STDOUT, "%s: Set AfniHostName to %s (stream name: %s)\n", FuncName, cf->AfniHostName, cf->NimlAfniStream);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
         fprintf(SUMA_STDERR,"Error %s: No provision for such a maximum embedding dimension.\n", FuncName);
         SUMA_RETURN(SUMA_Dunno);
   }

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
      /*fprintf(SUMA_STDERR,"%s: Registering SpecSO ...", FuncName);*/
      if (!SUMA_RegisterSpecSO(&Spec, cSV, DOv, N_DOv)) {
         fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_RegisterSpecSO.\n", FuncName);
         SUMA_RETURN(NOPE);
      } 
      /*fprintf(SUMA_STDERR,"%s: Done.\n", FuncName);*/

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


   /* if surface is SureFit, flip lights */
   SO = (SUMA_SurfaceObject *)(DOv[cSV->Focus_SO_ID].OP);
   if (SO->FileType == SUMA_SUREFIT) {
      cSV->light0_position[2] *= -1;
   }


   SUMA_RETURN(YUP);
}
