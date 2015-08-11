#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  

/* Volume rendering code, based on GLUT-3.7's advanced97/volume.c 
See also SUMA_GLUT_volumedemo.c*/



static GLfloat lightpos[4] = {150., 150., 150., 1.f};


// put the clip geometry (planes etc.) here for picking...

int SUMA_MoveCutplane (SUMA_VolumeObject *VO, int iplane, float d)
{
   static char FuncName[]={"SUMA_MoveCutplane"};
   
   SUMA_ENTRY;
   
   if (iplane < 0 || iplane > 5) {
      SUMA_S_Err("Bad plane index");
      SUMA_RETURN(0);
   }
   
   VO->CutPlane[iplane][3] = VO->CutPlane[iplane][3]+d;
   if (!SUMA_SetTextureClipPlaneSurface(VO, iplane)) {
      SUMA_S_Err("Failed to set cutplane surface");
      SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1);
}

SUMA_DSET *SUMA_adset_to_VE(SUMA_VolumeObject *VO, THD_3dim_dataset **dsetp)
{
   static char FuncName[]={"SUMA_adset_to_VE"};
   THD_3dim_dataset *dset=NULL;
   THD_3dim_dataset *odset=NULL;
   SUMA_DSET *sdset=NULL;
   int n_VE=0, OverInd, OKdup=0, loc[2];
   char orcode[6], *np=NULL, *dsetcmap=NULL, *forcode;
   SUMA_ALL_DO *ado=(SUMA_ALL_DO *)VO;
   SUMA_OVERLAYS *colplane = NULL, *curcolplane=NULL;
   SUMA_Boolean SetupOverlay = YUP, MakeOverlayCurrent = YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   dset = *dsetp;
    
   /* make sure data part of dset is loaded, not just the header          */
   DSET_mallocize(dset); DSET_load(dset);
   orcode[0] = ORIENT_typestr[dset->daxes->xxorient][0] ;
   orcode[1] = ORIENT_typestr[dset->daxes->yyorient][0] ;
   orcode[2] = ORIENT_typestr[dset->daxes->zzorient][0] ;
   orcode[3] = '\0';
   SUMA_LHv("dset orcode is %s\n", orcode);

   if ((forcode = getenv("SUMA_VO_Reorient")) &&
       strcmp(forcode,"NO") && strcmp(forcode,"No") && strcmp(forcode,"no") ) {
       if (!SUMA_ok_orstring(forcode)) {
         SUMA_S_Err("Bad orientation string %s in env SUMA_VO_Reorient\n"
                    "No reorienting done.", forcode);
       } else if (strcmp(orcode,forcode) ) {
         char sss[5];
         SUMA_S_Note("Resampling %s from %s to %s, per user request.\n", 
                     DSET_HEADNAME(dset), orcode, forcode);
         odset = r_new_resam_dset(dset, NULL, 0.0, 0.0, 0.0, 
                                  forcode, MRI_LINEAR, NULL, 1, 1);
         sprintf(sss, ".%s",forcode);
         np = SUMA_append_string(DSET_PREFIX(dset), sss);
         EDIT_dset_items(  odset ,
                         ADN_prefix      , np,
                         ADN_none ) ;
         tross_Copy_History( dset , odset ) ;      
         DSET_delete(dset); dset = odset; odset = NULL;
         if (LocalHead && 0) {
            SUMA_LH("Writing resampled dset");
            DSET_write(dset);
         }
         SUMA_free(np); np = NULL;
         *dsetp = dset;
      }
   }
  
   sdset = SUMA_afnidset2sumadset(&dset, 1, 1, 0); 
   if (dset) {
      SUMA_S_Warn("Leakage! dset should be null by now...");
   }
   
   /* Does this dset have a built in colormap?
      If it does, then loadit into SCM */
   if (!SUMA_Insert_Cmap_of_Dset(sdset)) {
      SUMA_S_Err("Failed to insert Cmap");
      SUMA_FreeDset(sdset); sdset = NULL;
      SUMA_RETURN(NOPE);
   }
   
   if (SetupOverlay) {
      OverInd = -1;
      SUMA_LH("Looking for pre-existing overlay on %s", SDSET_LABEL(sdset));
      if ((colplane = SUMA_Fetch_OverlayPointerByDset (
                              ado, sdset, &OverInd))) {
         SUMA_LH("Col plane already present");
         OKdup = 0;
         if (colplane->OptScl->Clusterize) {
            SUMA_S_Warn("Nothing implemented for volume clustering here,"
                        "just going through the motions...");
            colplane->OptScl->RecomputeClust = 1;
         }
      } else {
         OKdup = 1;
         SUMA_LH("Creating anew");
         OverInd = SUMA_ADO_N_Overlays(ado);
      }
      if (!(colplane = SUMA_CreateOverlayPointer ( ADO_LABEL(ado), 
                                                sdset, ADO_ID(ado), colplane))) {
         SUMA_S_Err("Failed to create overlay");
         SUMA_RETURN(NOPE);
      }
      colplane->isBackGrnd = NOPE;
      
      /* Add this plane to VO's Overlays */
      SUMA_LH("VO has %d overlays", SUMA_ADO_N_Overlays(ado));
      if (!SUMA_AddNewPlane (ado, colplane, SUMAg_DOv, 
                             SUMAg_N_DOv, OKdup)) {
         SUMA_SL_Err("Failed in SUMA_AddNewPlane");
         SUMA_FreeOverlayPointer(colplane);
         if (!SUMA_DeleteDsetPointer(&sdset, SUMAg_CF->DsetList)) {
            SUMA_S_Err("Failed to delete dset pointer");
         }
         SUMA_RETURN(NOPE);
      }
      SUMA_LH("VO now has %d overlays", SUMA_ADO_N_Overlays(ado));
      
      /* Match previous setting? */
      curcolplane = SUMA_ADO_CurColPlane(ado);
      if (SUMA_PreserveOverlaySettings(curcolplane, colplane)) {
                                 /* attempt to preserve current situation */
         SUMA_OVERLAYS *settingPlane = NULL;
         settingPlane = curcolplane;
         colplane->GlobalOpacity = settingPlane->GlobalOpacity;
         colplane->ShowMode = settingPlane->ShowMode;
         colplane->OptScl->BrightFact = settingPlane->OptScl->BrightFact;
         colplane->OptScl->find = settingPlane->OptScl->find;
         colplane->OptScl->tind = settingPlane->OptScl->tind;
         colplane->OptScl->bind = settingPlane->OptScl->bind;
         colplane->OptScl->UseThr = settingPlane->OptScl->UseThr;
         colplane->OptScl->UseBrt = settingPlane->OptScl->UseBrt;
         colplane->OptScl->ThrMode = settingPlane->OptScl->ThrMode;
         colplane->OptScl->ThreshRange[0] = 
                                       settingPlane->OptScl->ThreshRange[0];
         colplane->OptScl->ThreshRange[1] = 
                                       settingPlane->OptScl->ThreshRange[1];
         colplane->OptScl->BrightRange[0] = 
                                       settingPlane->OptScl->BrightRange[0];
         colplane->OptScl->BrightRange[1] = 
                                       settingPlane->OptScl->BrightRange[1];
         colplane->OptScl->BrightMap[0] = 
                                       settingPlane->OptScl->BrightMap[0];
         colplane->OptScl->BrightMap[1] = 
                                       settingPlane->OptScl->BrightMap[1];
         colplane->SymIrange = settingPlane->SymIrange;
         colplane->OptScl->IntRange[0] = settingPlane->OptScl->IntRange[0];
         colplane->OptScl->IntRange[1] = settingPlane->OptScl->IntRange[1];
         dsetcmap = NI_get_attribute(sdset->ngr,"SRT_use_this_cmap");
         if (dsetcmap) {
            SUMA_STRING_REPLACE(colplane->cmapname, dsetcmap);
         } else {
            SUMA_STRING_REPLACE(colplane->cmapname, 
                                settingPlane->cmapname);
         }         
         colplane->OptScl->Clusterize = settingPlane->OptScl->Clusterize;
         colplane->OptScl->ClustOpt->AreaLim = 
            settingPlane->OptScl->ClustOpt->AreaLim;
         colplane->OptScl->ClustOpt->DistLim = 
            settingPlane->OptScl->ClustOpt->DistLim;
      } else {
         /* set the opacity, index column and the range */
         colplane->GlobalOpacity = YUP;
         colplane->ShowMode = SW_SurfCont_DsetViewCol;
         if (!OKdup) {/* only set this if first time creating plane*/
            colplane->OptScl->BrightFact = 0.8;
         }
         colplane->OptScl->find = 0;
         colplane->OptScl->tind = 0;
         colplane->OptScl->bind = 0;
         #if 0
         SUMA_GetDsetColRange(sdset, 0, colplane->OptScl->IntRange, loc);
         #else
         colplane->OptScl->RangeUnits = SUMA_PERC_VALUE_UNITS;
         colplane->OptScl->IntRange[0] = 2;
         colplane->OptScl->IntRange[1] = 98;
         colplane->OptScl->AutoIntRange = 0; /* turn of auto ranging 
                                 Otherwise SurfCont fields won't reflect
                                 what will eventually get put into IntRange[]
                                 in the ScaleToMap functions when the 
                                 SurfCont is first opened */
         #endif
         if (colplane->SymIrange) {
            colplane->OptScl->IntRange[0] = 
               -fabs(SUMA_MAX_PAIR( colplane->OptScl->IntRange[0],
                                    colplane->OptScl->IntRange[1]));
            colplane->OptScl->IntRange[1] = 
               -colplane->OptScl->IntRange[0];
         }

         /* stick a colormap onto that plane ? */
         dsetcmap = NI_get_attribute(sdset->ngr,"SRT_use_this_cmap");
         if (dsetcmap) {
            SUMA_STRING_REPLACE(colplane->cmapname, dsetcmap);
         } else {
            /* don't worry, there's a default one */
         }
      }
      if (colplane->OptScl->Clusterize) 
         colplane->OptScl->RecomputeClust = 1;
      /* colorize the plane */
      SUMA_LH("Colorizing Plane");
      SUMA_ColorizePlane(colplane);

      /* SUMA_Show_ColorOverlayPlanes(&colplane, 1, 1); */

      /* set the new curColPlane to the newly loaded plane */
      if (MakeOverlayCurrent) {
         SUMA_X_SurfCont *SurfCont=SUMA_ADO_Cont(ado);
         if (!SurfCont) {
            SUMA_S_Err("OMG");
         } else {
            SurfCont->curColPlane = colplane; 
         }
      }
   }
   
   /* Add as new volume element */
   n_VE = SUMA_VO_NumVE(VO);
   VO->VE[n_VE] = (SUMA_VolumeElement*)SUMA_calloc(1,
                                       sizeof(SUMA_VolumeElement));
   VO->VE[n_VE]->dset_idcode_str = SUMA_copy_string(SDSET_ID(sdset));
   if (!SUMA_InsertDsetPointer(&sdset, SUMAg_CF->DsetList, 0)) {
      SUMA_S_Err("Failed to inset dset pointer. Replace is not enabled");
      SUMA_RETURN(NOPE);
   }
   
   if (!SUMA_VE_Set_Dims(VO->VE, n_VE)) {
      SUMA_S_Err("Failed to set dims");
      SUMA_RETURN(NOPE);
   }
 
   /* Prep colorplane */
   SUMA_LH("Prep texture");
   if (!(VO->VE[n_VE]->texvec = 
         SUMA_VE_to_tex3d(VO->VE, n_VE, (byte)n_VE))) {
      SUMA_S_Err("Failed in dset to text3d");
      VO = SUMA_FreeVolumeObject(VO);
      SUMA_RETURN(NOPE);
   }
   SUMA_LHv("Have slot %d,%s\n", n_VE, SUMA_VE_Headname(VO->VE, n_VE));
      
   /* Set the box limits */
   SUMA_dset_extreme_corners(sdset, 
                           VO->VE[n_VE]->vo0, VO->VE[n_VE]->voN, 1);
   SUMA_dset_extreme_corners(sdset, 
                           VO->VE[n_VE]->bo0, VO->VE[n_VE]->boN, 0);
                              
                              
   SUMA_RETURN(sdset);
}

GLubyte * SUMA_VE_to_tex3d(SUMA_VolumeElement **VE, int iVE, byte col)
{  
   static char FuncName[]={"SUMA_VE_to_tex3d"};
   char *filename;
   GLubyte *tex3ddata;
   GLint max3dtexdims; /* maximum allowed 3d texture dimension */
   GLint newval;
   SUMA_DSET *sdset=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
    
   if (!(sdset = SUMA_VE_dset(VE, iVE))) {
      SUMA_S_Err("No volume found");
      SUMA_RETURN(NULL); 
   }
   if (!(tex3ddata = 
            (GLubyte *)SUMA_malloc(4*SUMA_VE_Nvox(VE, iVE)*sizeof(GLubyte)))) {
      SUMA_S_Crit("Failed to allocate.");
      SUMA_RETURN(NULL); 
   }
   if (LocalHead) {
      SUMA_LHv("Copying %d intensity in R, G, B, A \n", 
               SUMA_VE_Nvox(VE, iVE)*4);
   }
   #if 0
   if (!SUMA_Colorize_dset_OBSOLETE(sdset, tex3ddata, col)) {
      SUMA_S_Err("Failed to colorize VO");
      SUMA_RETURN(NULL); 
   }
   #endif
   
   SUMA_RETURN(tex3ddata);
}

/* This function here is for illustrative purposes.
   It may be too inefficient to have to allocate and 
   free SV for each colorizing operation. 
   
*/
SUMA_Boolean SUMA_Colorize_dset_OBSOLETE(SUMA_DSET *dset, 
                                byte *tex3ddata, byte colopt)
{
   static char FuncName[]={"SUMA_Colorize_dset_OBSOLETE"};
   static SUMA_SCALE_TO_MAP_OPT *Opt=NULL;  
   static SUMA_COLOR_MAP *ColMap=NULL; 
   SUMA_COLOR_SCALED_VECT * SV= NULL;
   float *floatvol=NULL;
   byte *bytevol=NULL, am=0;
   int i, j, i3;
   int av=0;
   SUMA_Boolean ans = YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* setup some defaults for now. */
   if (!Opt) { /* setup once, this struct should be part of VO, perhaps.
                  Or part of sv structure, perhaps... */
      Opt = SUMA_ScaleToMapOptInit();
      Opt->alaAFNI=YUP;
      if (LocalHead) {
         SUMA_ShowScaleToMapOpt(Opt, NULL, 1);
      }
   }
   
   if (!ColMap) {
      char *eee=getenv("SUMA_VO_ColorMap");
      if (eee) {
         if (!(ColMap = SUMA_FindNamedColMap(eee))) {
            SUMA_S_Errv( "Colormap %s not found.\n"
                         "Using bgyr64 instead.\n", eee);
         }
      } else {
         eee = "bgyr64";
      } 
      ColMap = SUMA_FindNamedColMap(eee);
      if (!ColMap) {
         SUMA_S_Errv("Could not get %s\n", eee);
         SUMA_RETURN(NOPE);
      }
         
   }
   /* No need to colorize as was done in the days of olde.
      Now colorization is handled in SUMA_Overlays_2_GLCOLAR4, 
      SUMA_ColorizePlane, and SUMA_ScaleToMap_Interactive */
      
   SUMA_RETURN(ans);
   
   /* Create temporary holding structure for colorized vectors */
   if (!(SV = SUMA_Create_ColorScaledVect(SDSET_NVOX(dset), 0))) {
      SUMA_S_Err("Failed to create SV");
      ans = NOPE;      goto CLEANUP;
   }
   
            
   /* copy into tex3ddata */
   if (!colopt) {
      bytevol = (byte *)SUMA_calloc(SDSET_NVOX(dset), sizeof(byte));
      if (!bytevol) {
         SUMA_S_Err("Failed to allocate for bytevol");
         ans = NOPE;      goto CLEANUP;
      }
      EDIT_coerce_scale_type( 
                  SDSET_VECLEN(dset) ,
                  SDSET_BRICK_FACTOR(dset,0) ,
                  SDSET_BRICK_TYPE(dset,0), 
                  SDSET_ARRAY(dset, 0) ,   /* input  */
                  MRI_byte               , bytevol  ) ;
      j=0;
      for(i = 0; i < SDSET_NVOX(dset); i++) {
         tex3ddata[j] = bytevol[i]; ++j;
         tex3ddata[j] = bytevol[i]; ++j;
         tex3ddata[j] = bytevol[i]; ++j;
         tex3ddata[j] = bytevol[i]; ++j;
      }
      if (bytevol) SUMA_free(bytevol); bytevol=NULL;
   } else {
      /* put dset values in temporary floatvol float vector */
      floatvol = (float *)SUMA_calloc(SDSET_NVOX(dset), sizeof(float));
      if (!floatvol) {
         SUMA_S_Err("Failed to allocate for floatvol");
         ans = NOPE;      goto CLEANUP;
      }
      EDIT_coerce_scale_type( 
                  SDSET_NVOX(dset) ,
                  SDSET_BRICK_FACTOR(dset,0) ,
                  SDSET_BRICK_TYPE(dset,0), 
                  SDSET_ARRAY(dset, 0) ,   /* input  */
                  MRI_float               , floatvol  ) ;
      if (!SUMA_ScaleToMap_alaAFNI (floatvol, SDSET_NVOX(dset),
                                 0.0, ColMap, Opt, SV)) {
         SUMA_S_Err("Failed to colorize");
         ans = NOPE;      goto CLEANUP;
      }

      j=0;
      for(i = 0; i < SDSET_NVOX(dset); i++) {
         i3 = 3*i; am = 0;
         tex3ddata[j] = (byte)(SV->cV[i3  ] * 255);
                                   am = tex3ddata[j];  ++j;
         tex3ddata[j] = (byte)(SV->cV[i3+1] * 255); 
            if (tex3ddata[j] > am) am = tex3ddata[j];  ++j;
         tex3ddata[j] = (byte)(SV->cV[i3+2] * 255);
            if (tex3ddata[j] > am) am = tex3ddata[j];  ++j;
         if (SV->isMasked[i]) { 
            tex3ddata[j] = 0;
         } else {
            tex3ddata[j] = am;
         }
         ++j;
      }

   }
   CLEANUP:
   if (SV) SUMA_Free_ColorScaledVect(SV); SV = NULL;
   if (bytevol) SUMA_free(bytevol); bytevol = NULL;
   if (floatvol) SUMA_free(floatvol); floatvol = NULL;
 
   SUMA_RETURN(ans);
}

void SUMA_CreateSphereList(void)
{
   static char FuncName[]={"SUMA_CreateSphereList"};

   SUMA_ENTRY;

   SUMA_S_Note("Making sphere display list");
   /* make a display list containing a sphere */
   glNewList(1, GL_COMPILE);
   {
      static GLfloat lightpos[] = {150.f, 150.f, 150.f, 1.f};
      static GLfloat material[] = {1.f, .5f, 1.f, 1.f};
      GLUquadricObj *qobj = gluNewQuadric();
      glPushAttrib(GL_LIGHTING_BIT);
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT2);
      glLightfv(GL_LIGHT2, GL_POSITION, lightpos);
      glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, material);
      gluSphere(qobj, 20.f, 20, 20);
      gluDeleteQuadric(qobj);
      glPopAttrib();
   }
   glEndList();
   SUMA_RETURNe;
}

void SUMA_RecordEnablingState(SUMA_EnablingRecord *SER, char *Label)
{
   static char FuncName[]={"SUMA_RecordEnablingState"};
   
   SUMA_ENTRY;
   
   if (!SER) {
      SUMA_S_Err("NULL SER, how am I to record?");
      SUMA_RETURNe;
   }
   snprintf(SER->Label,255, "%s", Label ? Label:"Unabeled");
   SER->ALPHA_TEST = glIsEnabled(GL_ALPHA_TEST);
   SER->DEPTH_TEST = glIsEnabled(GL_DEPTH_TEST);
   SER->TEXTURE_3D_EXT = glIsEnabled(GL_TEXTURE_3D_EXT);
   SER->TEXTURE_3D = glIsEnabled(GL_TEXTURE_3D);
   SER->TEXTURE_2D = glIsEnabled(GL_TEXTURE_2D);
   SER->TEXTURE_GEN_S = glIsEnabled(GL_TEXTURE_GEN_S);
   SER->TEXTURE_GEN_T = glIsEnabled(GL_TEXTURE_GEN_T);
   SER->TEXTURE_GEN_R = glIsEnabled(GL_TEXTURE_GEN_R);
   SER->COLOR_MATERIAL = glIsEnabled(GL_COLOR_MATERIAL);
   SER->CLIP_PLANE0 = glIsEnabled(GL_CLIP_PLANE0);
   SER->CLIP_PLANE1 = glIsEnabled(GL_CLIP_PLANE1);
   SER->CLIP_PLANE2 = glIsEnabled(GL_CLIP_PLANE2);
   SER->CLIP_PLANE3 = glIsEnabled(GL_CLIP_PLANE3);
   SER->CLIP_PLANE4 = glIsEnabled(GL_CLIP_PLANE4);
   SER->CLIP_PLANE5 = glIsEnabled(GL_CLIP_PLANE5);
   SER->LIGHTING = glIsEnabled(GL_LIGHTING);
   SER->LIGHT0 = glIsEnabled(GL_LIGHT0);
   SER->LIGHT1 = glIsEnabled(GL_LIGHT1);
   SER->LIGHT2 = glIsEnabled(GL_LIGHT2);
   SER->BLEND = glIsEnabled(GL_BLEND);
   SER->LINE_SMOOTH = glIsEnabled(GL_LINE_SMOOTH);
   /* SER-> = glIsEnabled(GL_); */
   glGetFloatv(GL_CURRENT_COLOR, SER->CurCol);
   glGetIntegerv(GL_COLOR_MATERIAL_PARAMETER, &(SER->ColMatParam));
   glGetIntegerv(GL_COLOR_MATERIAL_FACE, &(SER->ColMatFace));
   
   SUMA_RETURNe;
}

void SUMA_RestoreEnablingState(SUMA_EnablingRecord *SER)
{
   static char FuncName[]={"SUMA_RestoreEnablingState"};
   
   SUMA_ENTRY;
   if (!SER) {
      SUMA_S_Err("No pointer amigo");
      SUMA_RETURNe;
   }   
   if (SER->ALPHA_TEST) glEnable(GL_ALPHA_TEST);
   else glDisable(GL_ALPHA_TEST);
   if (SER->DEPTH_TEST) glEnable(GL_DEPTH_TEST);
   else glDisable(GL_DEPTH_TEST);
   if (SER->TEXTURE_3D_EXT) glEnable(GL_TEXTURE_3D_EXT);
   else glDisable(GL_TEXTURE_3D_EXT);
   if (SER->TEXTURE_3D) glEnable(GL_TEXTURE_3D);
   else glDisable(GL_TEXTURE_3D);
   if (SER->TEXTURE_2D) glEnable(GL_TEXTURE_2D);
   else glDisable(GL_TEXTURE_2D);
   if (SER->TEXTURE_GEN_S) glEnable(GL_TEXTURE_GEN_S);
   else glDisable(GL_TEXTURE_GEN_S);
   if (SER->TEXTURE_GEN_T) glEnable(GL_TEXTURE_GEN_T);
   else glDisable(GL_TEXTURE_GEN_T);
   if (SER->TEXTURE_GEN_R) glEnable(GL_TEXTURE_GEN_R);
   else glDisable(GL_TEXTURE_GEN_R);
   if (SER->CLIP_PLANE0) glEnable(GL_CLIP_PLANE0);
   else glDisable(GL_CLIP_PLANE0);
   if (SER->CLIP_PLANE1) glEnable(GL_CLIP_PLANE1);
   else glDisable(GL_CLIP_PLANE1);
   if (SER->CLIP_PLANE2) glEnable(GL_CLIP_PLANE2);
   else glDisable(GL_CLIP_PLANE2);
   if (SER->CLIP_PLANE3) glEnable(GL_CLIP_PLANE3);
   else glDisable(GL_CLIP_PLANE3);
   if (SER->CLIP_PLANE4) glEnable(GL_CLIP_PLANE4);
   else glDisable(GL_CLIP_PLANE4);
   if (SER->CLIP_PLANE5) glEnable(GL_CLIP_PLANE5);
   else glDisable(GL_CLIP_PLANE5);
   if (SER->LIGHTING) glEnable(GL_LIGHTING);
   else glDisable(GL_LIGHTING);
   if (SER->LIGHT0) glEnable(GL_LIGHT0);
   else glDisable(GL_LIGHT0);
   if (SER->LIGHT1) glEnable(GL_LIGHT1);
   else glDisable(GL_LIGHT1);
   if (SER->LIGHT2) glEnable(GL_LIGHT2);
   else glDisable(GL_LIGHT2);
   if (SER->BLEND) glEnable(GL_BLEND);
   else glDisable(GL_BLEND);
   if (SER->LINE_SMOOTH) glEnable(GL_LINE_SMOOTH);
   else glDisable(GL_LINE_SMOOTH);
   if (SER->COLOR_MATERIAL) glEnable(GL_COLOR_MATERIAL);
   else glDisable(GL_COLOR_MATERIAL);
   
   /* For now, do not bother setting colors, etc. */
   
   /* if (SER->) glEnable(); 
      else glDisable() */
   
   SUMA_RETURNe;
}

char *SUMA_EnablingState_Info(SUMA_EnablingRecord *SERu)
{
   static char FuncName[]={"SUMA_EnablingState_Info"};
   char *s=NULL;
   SUMA_EnablingRecord SERl, *SER;
   SUMA_STRING *SS=NULL;
   
   SUMA_ENTRY;
      
   SS = SUMA_StringAppend(NULL, NULL);
   if (!SERu) {
      SUMA_RecordEnablingState(&SERl, FuncName); 
      SER = &SERl;
   }  else {
      SER = SERu;
   }
   
   SUMA_StringAppend_va(SS,"OpenGL State Record for %s\n", SER->Label);
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
                       "GL_ALPHA_TEST", SER->ALPHA_TEST ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
                        "GL_DEPTH_TEST",SER->DEPTH_TEST ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
               "GL_TEXTURE_3D_EXT", SER->TEXTURE_3D_EXT ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_TEXTURE_2D", SER->TEXTURE_2D ? "+++":"---");
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_TEXTURE_3D", SER->TEXTURE_3D ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_TEXTURE_GEN_S", SER->TEXTURE_GEN_S ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_TEXTURE_GEN_T", SER->TEXTURE_GEN_T ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_TEXTURE_GEN_R", SER->TEXTURE_GEN_R ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_CLIP_PLANE0", SER->CLIP_PLANE0 ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_CLIP_PLANE1", SER->CLIP_PLANE1 ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_CLIP_PLANE2", SER->CLIP_PLANE2 ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_CLIP_PLANE3", SER->CLIP_PLANE3 ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_CLIP_PLANE4", SER->CLIP_PLANE4 ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_CLIP_PLANE5", SER->CLIP_PLANE5 ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_LIGHTING", SER->LIGHTING ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_COLOR_MATERIAL", SER->COLOR_MATERIAL ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %d\n", 
           "COLOR_MATERIAL_PARAMETER", SER->ColMatParam); 
   SUMA_StringAppend_va(SS,"% 24s is %d\n", 
           "COLOR_MATERIAL_FACE", SER->ColMatFace);
   SUMA_StringAppend_va(SS,"% 24s is %.3f %.3f %.3f %.3f\n", 
           "CURRENT_COLOR", 
           SER->CurCol[0], SER->CurCol[1], SER->CurCol[2], SER->CurCol[3] ); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_LIGHT0", SER->LIGHT0 ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_LIGHT1", SER->LIGHT1 ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_LIGHT2", SER->LIGHT2 ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_BLEND", SER->BLEND ? "+++":"---"); 
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_LINE_SMOOTH", SER->LINE_SMOOTH ? "+++":"---"); 

/*   
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_ ", SER-> ? "+++":"---"); 
                        */
   SUMA_SS2S(SS,s);
   
   SUMA_RETURN(s);
}

void SUMA_ShowEnablingState(SUMA_EnablingRecord *SER, FILE *out, 
                            char *preamble) {
   static char FuncName[]={"SUMA_ShowEnablingState"};
   char *s=NULL;
   SUMA_ENTRY;
   if (!out) out = SUMA_STDOUT;
   
   s = SUMA_EnablingState_Info(SER);
   
   fprintf(out, "%s%s", preamble ? preamble:"", s);
   
   SUMA_free(s); s = NULL;
   
   SUMA_RETURNe;
}

char *SUMA_DiffEnablingState_Info(SUMA_EnablingRecord *SERnew,
                                  SUMA_EnablingRecord *SERref)
{
   static char FuncName[]={"SUMA_DiffEnablingState_Info"};
   char *s=NULL;
   static SUMA_EnablingRecord SER, *SER_last=NULL;
   SUMA_EnablingRecord now;
   SUMA_STRING *SS=NULL;
   
   SUMA_ENTRY;
   
   if (!SERref) { /* Use last record */
      if (!SER_last) { /* No last record */
         SUMA_RecordEnablingState(&SER, "From Diff");
         SER_last = &SER;
      }
      SERref = SER_last;
   }
   if (!SERnew) { /* Nothing given get current */
      SUMA_RecordEnablingState(&now, "From Diff");
      SERnew = &now;
   }
   
   SS = SUMA_StringAppend(NULL, NULL);
   SUMA_StringAppend_va(SS,"OpenGL State Diff: %s vs. %s\n",
                        SERnew->Label, SERref->Label);
   if (SERnew->ALPHA_TEST != SERref->ALPHA_TEST) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_ALPHA_TEST", 
                              SERnew->ALPHA_TEST , SERref->ALPHA_TEST); 
   }
   if (SERnew->DEPTH_TEST != SERref->DEPTH_TEST) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_DEPTH_TEST", 
                              SERnew->DEPTH_TEST , SERref->DEPTH_TEST); 
   }
   if (SERnew->TEXTURE_3D_EXT != SERref->TEXTURE_3D_EXT) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_TEXTURE_3D_EXT", 
                              SERnew->TEXTURE_3D_EXT , SERref->TEXTURE_3D_EXT); 
   }   
   if (SERnew->TEXTURE_2D != SERref->TEXTURE_2D) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_TEXTURE_2D", 
                              SERnew->TEXTURE_2D , SERref->TEXTURE_2D); 
   }
   if (SERnew->TEXTURE_3D != SERref->TEXTURE_3D) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_TEXTURE_3D", 
                              SERnew->TEXTURE_3D , SERref->TEXTURE_3D); 
   }
   if (SERnew->TEXTURE_GEN_S != SERref->TEXTURE_GEN_S) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_TEXTURE_GEN_S", 
                              SERnew->TEXTURE_GEN_S , SERref->TEXTURE_GEN_S); 
   }
   if (SERnew->TEXTURE_GEN_T != SERref->TEXTURE_GEN_T) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_TEXTURE_GEN_T", 
                              SERnew->TEXTURE_GEN_T , SERref->TEXTURE_GEN_T); 
   }
   if (SERnew->TEXTURE_GEN_R != SERref->TEXTURE_GEN_R) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_TEXTURE_GEN_R", 
                              SERnew->TEXTURE_GEN_R , SERref->TEXTURE_GEN_R); 
   }
   if (SERnew->CLIP_PLANE0 != SERref->CLIP_PLANE0) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_CLIP_PLANE0", 
                              SERnew->CLIP_PLANE0 , SERref->CLIP_PLANE0); 
   }
   if (SERnew->CLIP_PLANE1 != SERref->CLIP_PLANE1) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_CLIP_PLANE1", 
                              SERnew->CLIP_PLANE1 , SERref->CLIP_PLANE1); 
   }
   if (SERnew->CLIP_PLANE2 != SERref->CLIP_PLANE2) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_CLIP_PLANE2", 
                              SERnew->CLIP_PLANE2 , SERref->CLIP_PLANE2); 
   }
   if (SERnew->ALPHA_TEST != SERref->ALPHA_TEST) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_ALPHA_TEST", 
                              SERnew->ALPHA_TEST , SERref->ALPHA_TEST); 
   }
   if (SERnew->CLIP_PLANE4 != SERref->CLIP_PLANE4) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_CLIP_PLANE4", 
                              SERnew->CLIP_PLANE4 , SERref->CLIP_PLANE4); 
   }
   if (SERnew->CLIP_PLANE5 != SERref->CLIP_PLANE5) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_CLIP_PLANE5", 
                              SERnew->CLIP_PLANE5 , SERref->CLIP_PLANE5); 
   }
   if (SERnew->LIGHTING != SERref->LIGHTING) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_LIGHTING", 
                              SERnew->LIGHTING , SERref->LIGHTING); 
   }
   if (SERnew->COLOR_MATERIAL != SERref->COLOR_MATERIAL) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "GL_COLOR_MATERIAL", 
                              SERnew->COLOR_MATERIAL , SERref->COLOR_MATERIAL); 
   }
   if (SERnew->CurCol[0] != SERnew->CurCol[0] ||
       SERnew->CurCol[1] != SERnew->CurCol[1] ||
       SERnew->CurCol[2] != SERnew->CurCol[2] ||
       SERnew->CurCol[3] != SERnew->CurCol[3]) {
       SUMA_StringAppend_va(SS,
                  "% 24s is %.3f %.3f %.3f %.3f vs %.3f %.3f %.3f %.3f\n", 
                  "CURRENT_COL", 
                  SERnew->CurCol[0], SERnew->CurCol[1], 
                  SERnew->CurCol[2], SERnew->CurCol[3],
                  SERref->CurCol[0], SERref->CurCol[1], 
                  SERref->CurCol[2], SERref->CurCol[3] );  
   }
   if (SERnew->ColMatParam != SERref->ColMatParam) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "COLOR_MATERIAL_PARAMETER", 
                              SERnew->ColMatParam , SERref->ColMatParam);
   }
   if (SERnew->ColMatFace != SERref->ColMatFace) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "COLOR_MATERIAL_FACE", 
                              SERnew->ColMatFace , SERref->ColMatFace);
   }
   if (SERnew->LIGHT0 != SERref->LIGHT0) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "LIGHT0", 
                              SERnew->LIGHT0 , SERref->LIGHT0); 
   }
   if (SERnew->LIGHT1 != SERref->LIGHT1) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "LIGHT1", 
                              SERnew->LIGHT1 , SERref->LIGHT1); 
   }
   if (SERnew->LIGHT2 != SERref->LIGHT2) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "LIGHT2", 
                              SERnew->LIGHT2 , SERref->LIGHT2); 
   }
   if (SERnew->BLEND != SERref->BLEND) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "BLEND", 
                              SERnew->BLEND , SERref->BLEND); 
   }
   if (SERnew->LINE_SMOOTH != SERref->LINE_SMOOTH) {
      SUMA_StringAppend_va(SS,"% 24s is %d vs %d\n", 
                              "LINE_SMOOTH", 
                              SERnew->LINE_SMOOTH , SERref->LINE_SMOOTH); 
   }

/*   
   SUMA_StringAppend_va(SS,"% 24s is %s\n", 
           "GL_ ", SER-> ? "+++":"---"); 
                        */
   SUMA_StringAppend_va(SS,"End of Diff.\n\n"); 
   SUMA_SS2S(SS,s);
   
   /* Keep track of last visited */
   SUMA_CopyEnablingState(SER_last, SERnew);
   SUMA_RETURN(s);
}

void SUMA_DiffEnablingState(SUMA_EnablingRecord *SERnew, 
                            SUMA_EnablingRecord *SERref, FILE *out, 
                            char *preamble) {
   static char FuncName[]={"SUMA_DiffEnablingState"};
   char *s=NULL;
   SUMA_ENTRY;
   if (!out) out = SUMA_STDOUT;
   
   s = SUMA_DiffEnablingState_Info(SERnew, SERref);
   
   fprintf(out, "%s%s", preamble ? preamble:"", s);
   
   SUMA_free(s); s = NULL;
   
   SUMA_RETURNe;
}

int SUMA_CopyEnablingState(SUMA_EnablingRecord *SERnew,
                           SUMA_EnablingRecord *SERref)
{
   static char FuncName[]={"SUMA_CopyEnablingState"};
   
   SUMA_ENTRY;
   
   if (!SERnew || !SERref) SUMA_RETURN(NOPE);
   
   strcpy(SERnew->Label,SERref->Label);
   SERnew->ALPHA_TEST = SERref->ALPHA_TEST ;
   SERnew->DEPTH_TEST = SERref->DEPTH_TEST ;
   SERnew->COLOR_MATERIAL = SERref->COLOR_MATERIAL ;
   SERnew->TEXTURE_2D = SERref->TEXTURE_2D ;
   SERnew->TEXTURE_3D = SERref->TEXTURE_3D ;
   SERnew->TEXTURE_3D_EXT = SERref->TEXTURE_3D_EXT ;
   SERnew->TEXTURE_GEN_S = SERref->TEXTURE_GEN_S ;
   SERnew->TEXTURE_GEN_T = SERref->TEXTURE_GEN_T ;
   SERnew->TEXTURE_GEN_R = SERref->TEXTURE_GEN_R ;
   SERnew->CLIP_PLANE0 = SERref->CLIP_PLANE0 ;
   SERnew->CLIP_PLANE1 = SERref->CLIP_PLANE1 ;
   SERnew->CLIP_PLANE2 = SERref->CLIP_PLANE2 ;
   SERnew->CLIP_PLANE3 = SERref->CLIP_PLANE3 ;
   SERnew->CLIP_PLANE4 = SERref->CLIP_PLANE4 ;
   SERnew->CLIP_PLANE5 = SERref->CLIP_PLANE5 ;
   SERnew->LIGHTING = SERref->LIGHTING ;
   SERnew->LIGHT0 = SERref->LIGHT0 ;
   SERnew->LIGHT1 = SERref->LIGHT1 ;
   SERnew->LIGHT2 = SERref->LIGHT2 ;
   SERnew->BLEND = SERref->BLEND ;
   SERnew->LINE_SMOOTH = SERref->LINE_SMOOTH ;
   SUMA_COPY_VEC(SERref->CurCol,SERnew->CurCol,4,GLfloat, GLfloat);
      
   SUMA_RETURN(YUP);   
}

void SUMA_dset_slice_corners( int slc, float *orig, float *del, 
                              int *nvox, float *corners)
{
   static char FuncName[]={"SUMA_dset_slice_corners"};   
   int kk=0;
   SUMA_ENTRY;
   
   corners[kk]  = orig[0] + 0       * del[0]; ++kk;
   corners[kk]  = orig[1] + 0       * del[1]; ++kk;
   corners[kk]  = orig[2] + slc     * del[2]; ++kk;
   
   corners[kk]  = orig[0] + nvox[0] * del[0]; ++kk;
   corners[kk]  = orig[1] + 0       * del[1]; ++kk;
   corners[kk]  = orig[2] + slc     * del[2]; ++kk;
   
   corners[kk]  = orig[0] + nvox[0] * del[0]; ++kk;
   corners[kk]  = orig[1] + nvox[1] * del[1]; ++kk;
   corners[kk]  = orig[2] + slc     * del[2]; ++kk;

   corners[kk]  = orig[0] + 0       * del[0]; ++kk;
   corners[kk]  = orig[1] + nvox[1] * del[1]; ++kk;
   corners[kk]  = orig[2] + slc     * del[2]; ++kk;
   
   SUMA_RETURNe;
}

SUMA_Boolean SUMA_LoadVolDO (char *fname, 
                        SUMA_DO_CoordUnits coord_type, SUMA_VolumeObject **VOp,
			byte PutVOinList)
{
   static char FuncName[]={"SUMA_LoadVolDO"};
   SUMA_VolumeObject *VO=NULL;
   THD_3dim_dataset *dset=NULL;

   SUMA_ENTRY;

   if (!fname) SUMA_RETURN(NOPE);
   if (coord_type != SUMA_NORM_SCREEN_UNIT &&
       coord_type != SUMA_WORLD) coord_type = SUMA_WORLD;
       
   if (!(dset = THD_open_dataset( fname ))) {
      SUMA_S_Errv("Failed to open %s\n", fname);
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }     

   /* Create a DO from dset */
   if (VOp) {
      if (*VOp == NULL) {
         if (!(VO = SUMA_CreateVolumeObject(fname))) {
            SUMA_S_Err("Failed to create volume object");
            if (dset) DSET_delete(dset); 
            SUMA_free(fname); fname = NULL;
            SUMA_RETURN(NOPE);
         }
         *VOp = VO;
      } else {
         VO = *VOp;
      }
   } else {
      if (!(VO = SUMA_CreateVolumeObject(fname))) {
         SUMA_S_Err("Failed to create volume object");
         if (dset) DSET_delete(dset); 
         SUMA_free(fname); fname = NULL;
         SUMA_RETURN(NOPE);
      }
   }
   /* put main dset into VO */
   if (!(SUMA_AddDsetVolumeObject(VO, &dset))) {
      SUMA_S_Err("Failed to add volume");
      if (dset) DSET_delete(dset);
      if (VO_N_VOLS(VO) == 0) {
         VO = SUMA_FreeVolumeObject(VO); 
         if (VOp) *VOp = NULL;
      }
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }

   /* Set some display defaults */
   {
      SUMA_X_SurfCont *SurfCont = NULL;
      SUMA_VOL_SAUX *VSaux=NULL;
      VO->TexEnvMode = GL_REPLACE;
      if ((SurfCont = SUMA_ADO_Cont((SUMA_ALL_DO *)VO)) && 
          (VSaux = SUMA_ADO_VSaux((SUMA_ALL_DO *)VO))) {
         /* Do the defaults, then modify per env */
         VSaux->ShowAxSlc = 1;
         SurfCont->Ax_slc->slice_num = (int)(SUMA_VO_N_Slices(VO, "Ax")/2.0);
         SurfCont->Ax_slc->mont_inc = 1;
         
         VSaux->ShowSaSlc = 1;
         SurfCont->Sa_slc->slice_num = (int)(SUMA_VO_N_Slices(VO, "Sa")/2.0); 
         SurfCont->Sa_slc->mont_num = 2;
         SurfCont->Sa_slc->mont_inc =
                        (int)SUMA_MAX_PAIR(SurfCont->Sa_slc->slice_num/2,1);
         VSaux->ShowCoSlc = 0;
         SurfCont->Co_slc->slice_num = (int)(SUMA_VO_N_Slices(VO, "Co")/2.0); 
         
         VSaux->ShowVrSlc = 0;
         VSaux->VrSelect = 0;
         VSaux->SlicesAtCrosshair = 0;
         /* Maybe params froms the env? */
         SUMA_Set_VO_Slice_Params(SUMA_EnvVal("SUMA_VO_InitSlices"), VO);
         /* And for selectable VR */
         { 
            char *eee = getenv("SUMA_VrSelectable");
            if (eee) {
               if (strcmp(eee,"NO") == 0)  VSaux->VrSelect = NOPE;
               else if (strcmp(eee,"YES") == 0) VSaux->VrSelect = YUP;
               else {
                  fprintf (SUMA_STDERR,   
                           "Warning %s:\n"
                        "Bad value for environment variable SUMA_VrSelectable\n"
                           "Assuming default of YES", FuncName);
                  VSaux->VrSelect = YUP;
               }
            } else VSaux->VrSelect = YUP;
         } 
      } else {
         SUMA_S_Err("Failed to initialize volume display");
      }
   }
   if (PutVOinList) {
      /* Add VO into DO list */
      if (!SUMA_AddDO(SUMAg_DOv, &(SUMAg_N_DOv), (void *)VO,  
                	VO_type, coord_type)) {
	 fprintf(SUMA_STDERR,"Error %s: Error Adding DO\n", FuncName);
	 SUMA_RETURN(NOPE);
      }
   }
   SUMA_RETURN(YUP);
}

int SUMA_Set_VO_Slice_Params(char *params, SUMA_VolumeObject *VO) 
{
   static char FuncName[]={"SUMA_Set_VO_Slice_Params"};
   NI_str_array *sar=NULL, *sub=NULL;
   int err=0, val, kk;
   float fval;
   SUMA_X_SurfCont *SurfCont = NULL;
   SUMA_VOL_SAUX *VSaux=NULL;
            
   SUMA_ENTRY;
   if (!params || params[0]=='\0') SUMA_RETURN(1);
   if (!VO) { SUMA_S_Err("NO VO"); SUMA_RETURN(0); }
   if (!(SurfCont = SUMA_ADO_Cont((SUMA_ALL_DO *)VO)) ||
       !(VSaux = SUMA_ADO_VSaux((SUMA_ALL_DO *)VO))) {
      SUMA_S_Err("Too early for this!");
      SUMA_RETURN(0);
   }
   if (!(sar = SUMA_NI_decode_string_list( params , ",; "))) {
      SUMA_S_Err("Huh?"); SUMA_RETURN(0);
   }
               
   /* Each string should be the form: variant:sn:num:inc */
   err = 0;
   VSaux->ShowAxSlc = 0;
   VSaux->ShowSaSlc = 0;
   VSaux->ShowCoSlc = 0;
   VSaux->ShowVrSlc = 0;
   for (kk=0; kk<sar->num && !err; ++kk) {
      if ((sub = SUMA_NI_decode_string_list( sar->str[kk] , ":")) &&
          (sub->num > 0)) {
               if (!strcasecmp(sub->str[0], "Ax") ||
                   !strcasecmp(sub->str[0], "hAx")) {
                  if (sub->str[0][0] == 'h') VSaux->ShowAxSlc = 0;
                  else VSaux->ShowAxSlc = 1;
                  if (sub->num > 1) {
                     fval = (float)strtod(sub->str[1], NULL);
                     if (fval > 0.0 && fval < 1.0) {
                        val = fval * (SUMA_VO_N_Slices(VO, "Ax")-1);
                     } else val = (int)fval;
                     if (val >= 0 &&
                         val < SUMA_VO_N_Slices(VO, "Ax")) {
                        SurfCont->Ax_slc->slice_num = val;
                     }
                  }
                  if (sub->num > 2) {
                     val = (int)strtod(sub->str[2], NULL);
                     if (val > 0 && 
                         val < SUMA_VO_N_Slices(VO, "Ax")) {
                         SurfCont->Ax_slc->mont_num = val;
                     }
                  }
                  if (sub->num > 3) {
                     fval = (float)strtod(sub->str[3], NULL);
                     if (fval > 0.0 && fval < 1.0) {
                        val = fval * (SUMA_VO_N_Slices(VO, "Ax")-1);
                     } else val = (int)fval;
                     if (val > 0 && 
                         val < SUMA_VO_N_Slices(VO, "Ax")) {
                         SurfCont->Ax_slc->mont_inc = val;
                     }
                  }
         } else if (!strcasecmp(sub->str[0], "Sa") ||
                    !strcasecmp(sub->str[0], "hSa")) {
                  if (sub->str[0][0] == 'h') VSaux->ShowSaSlc = 0;
                  else VSaux->ShowSaSlc = 1;
                  if (sub->num > 1) {
                     fval = (float)strtod(sub->str[1], NULL);
                     if (fval > 0.0 && fval < 1.0) {
                        val = fval * (SUMA_VO_N_Slices(VO, "Sa")-1);
                     } else val = (int)fval;
                     if (val >= 0 &&
                         val < SUMA_VO_N_Slices(VO, "Sa")) {
                        SurfCont->Sa_slc->slice_num = val;
                     }
                  }
                  if (sub->num > 2) {
                     val = (int)strtod(sub->str[2], NULL);
                     if (val > 0 && 
                         val < SUMA_VO_N_Slices(VO, "Sa")) {
                         SurfCont->Sa_slc->mont_num = val;
                     }
                  }
                  if (sub->num > 3) {
                     fval = (float)strtod(sub->str[3], NULL);
                     if (fval > 0.0 && fval < 1.0) {
                        val = fval * (SUMA_VO_N_Slices(VO, "Sa")-1);
                     } else val = (int)fval;
                     if (val > 0 && 
                         val < SUMA_VO_N_Slices(VO, "Sa")) {
                         SurfCont->Sa_slc->mont_inc = val;
                     }
                  }
         } else if (!strcasecmp(sub->str[0], "Co") ||
                    !strcasecmp(sub->str[0], "hCo")) {
                  if (sub->str[0][0] == 'h') VSaux->ShowCoSlc = 0;
                  else VSaux->ShowCoSlc = 1;
                  if (sub->num > 1) {
                     fval = (float)strtod(sub->str[1], NULL);
                     if (fval > 0.0 && fval < 1.0) {
                        val = fval * (SUMA_VO_N_Slices(VO, "Co")-1);
                     } else val = (int)fval;
                     if (val >= 0 &&
                         val < SUMA_VO_N_Slices(VO, "Co")) {
                        SurfCont->Co_slc->slice_num = val;
                     }
                  }
                  if (sub->num > 2) {
                     val = (int)strtod(sub->str[2], NULL);
                     if (val > 0 && 
                         val < SUMA_VO_N_Slices(VO, "Co")) {
                         SurfCont->Co_slc->mont_num = val;
                     }
                  }
                  if (sub->num > 3) {
                     fval = (float)strtod(sub->str[3], NULL);
                     if (fval > 0.0 && fval < 1.0) {
                        val = fval * (SUMA_VO_N_Slices(VO, "Co")-1);
                     } else val = (int)fval;
                     if (val > 0 && 
                         val < SUMA_VO_N_Slices(VO, "Co")) {
                         SurfCont->Co_slc->mont_inc = val;
                     }
                  }
         } else if (!strcasecmp(sub->str[0], "Vr") ||
                    !strcasecmp(sub->str[0], "hVr")) {
                  if (sub->str[0][0] == 'h') VSaux->ShowVrSlc = 0;
                  else VSaux->ShowVrSlc = 1;
         } else {
            SUMA_S_Err(
      "Slice variant %s not recognized for env SUMA_VO_InitSlices.\n"
      "Defaults will prevail.", sub->str[0]);
            err = 1;
            /* Just put the 'show' flags back where they were */
            VSaux->ShowAxSlc = 1;
            VSaux->ShowSaSlc = 1;
            VSaux->ShowCoSlc = 0;
            VSaux->ShowVrSlc = 0;
         }
         
      }
      sub = SUMA_free_NI_str_array(sub);
   }
   sar = SUMA_free_NI_str_array(sar); 
   
   /* You must have something showing for now because otherwise you can't open
   a volume controller! */
   if (!VSaux->ShowAxSlc && !VSaux->ShowSaSlc && 
       !VSaux->ShowCoSlc && !VSaux->ShowVrSlc ) {
      SUMA_S_Note("For now, must force something to show");
      VSaux->ShowAxSlc = 1;
   }
   
   SUMA_RETURN(1);
}

SUMA_Boolean SUMA_Load3DTextureNIDOnel (NI_element *nel, 
                                    SUMA_DO_CoordUnits defaultcoordtype)
{
   static char FuncName[]={"SUMA_Load3DTextureNIDOnel"};
   char *fname=NULL;
   char *atr=NULL, stmp[128];
   int i = 0;
   SUMA_VolumeObject *VO=NULL;
   SUMA_DO_CoordUnits coord_type=SUMA_WORLD;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (  !nel || 
         strcmp(nel->name,"3DTex")   )
      SUMA_RETURN(NOPE);
   
   if (NI_IS_STR_ATTR_EQUAL(nel, "read_status", "read")) SUMA_RETURN(YUP);
    
   NI_set_attribute(nel,"read_status","fail");
   
   if (! (fname = SUMA_copy_string(NI_get_attribute(nel,"filename"))) )
      SUMA_RETURN(NOPE);
   if (!SUMA_search_file(&fname, NULL)) { /* can't find it */ 
      SUMA_LH("Failed to find %s", fname);
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }
   
   /* Is there a particular coord type in nel? */
   if ((atr = NI_get_attribute(nel, "coord_type"))) {
      if ((coord_type = SUMA_CoordType(atr))
           == SUMA_COORD_TYPE_ERROR) {
         SUMA_S_Errv("Bad coord_type %s,"
                     "using default ",
                     atr);
         coord_type = defaultcoordtype;
      }
   }else {
      coord_type = defaultcoordtype;
   }
   
   if (!(SUMA_LoadVolDO(fname, coord_type, &VO,1))) {
      SUMA_S_Err("Failed to read %s", NI_get_attribute(nel,"filename"));
      SUMA_ifree(fname);
      SUMA_RETURN(NOPE);
   }
   SUMA_ifree(fname);

   /* any other dsets? */
   i = 0; sprintf(stmp,"overlay%d",i);
   while ((atr=NI_get_attribute(nel,stmp))) {
      SUMA_LHv("Loading %s\n", atr);
      if (! (fname = SUMA_copy_string(atr)) )
         SUMA_RETURN(NOPE);
      if (!SUMA_search_file(&fname, NULL)) { /* can't find it */ 
         SUMA_S_Errv("Failed to find %s\n", fname); 
         SUMA_free(fname); fname = NULL;
         break;
      }
      
      if (!(SUMA_LoadVolDO( fname, coord_type, &VO,1))) {
         SUMA_S_Errv("Failed to open %s\n", fname);
         SUMA_free(fname); fname = NULL;
         break;
      }
      
      SUMA_LHv("Added %s\n", SUMA_VE_Headname(VO->VE, i+1));
      SUMA_free(fname); fname = NULL;  
      ++i;
      sprintf(stmp,"overlay%d",i);
   } 
      
   
   VO->TexEnvMode = SUMA_NIDO_TexEnvMode(nel, GL_REPLACE); 

   /* store pointer copy of DO in nel */
   NI_SET_PTR(nel, "DO", VO);
   /* store idcode_str of DO in nel */
   NI_SET_STR(nel,"DO_idcode_str", VO->idcode_str);
   
   /* mark nel as read */
   NI_set_attribute(nel,"read_status","read");
    
   SUMA_RETURN(YUP);
}

SUMA_VolumeObject *SUMA_VOof3DTextureNIDOnel(NI_element *nel)
{
   static char FuncName[]={"SUMA_VOof3DTextureNIDOnel"};
   SUMA_VolumeObject *VO = NULL;
   int ii;
   char *idcode_str=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!(idcode_str = NI_get_attribute(nel, "DO_idcode_str"))) {
      SUMA_S_Err("NULL nel DO_idcode_str");
      SUMA_RETURN(NULL);
   }
   
   for (ii=0; ii<SUMAg_N_DOv; ++ii) {
      if (SUMA_isVO(SUMAg_DOv[ii])) {
         VO = (SUMA_VolumeObject *)(SUMAg_DOv[ii].OP);
         if (!strcmp(VO->idcode_str, idcode_str)) SUMA_RETURN(VO);
      }
   }
   
   SUMA_LHv("DO for nel %s, %s, not found\n",
      nel->name, idcode_str);
      
   SUMA_RETURN(NULL);
}

NI_element *SUMA_3DTextureNIDOnelofVO(SUMA_VolumeObject *VO) 
{
   static char FuncName[]={"SUMA_3DTextureNIDOnelofVO"};
   NI_element *nel = NULL;
   
   SUMA_ENTRY;
   
   SUMA_S_Err("Sorry, not implemented yet");
   
   SUMA_RETURN(nel);
}

/*!
   centers of first and last voxels in volume in RAI 
*/
void SUMA_dset_extreme_corners( SUMA_DSET *dset, 
                                float * mincorner, float *maxcorner,
                                int voxcen)
{
   static char FuncName[]={"SUMA_dset_extreme_corners"};   
   float A[4][4], I[3], *v;
   int *dims;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   
   if (mincorner) mincorner[0] = mincorner[1] = mincorner[2] = 0.0;
   if (maxcorner) maxcorner[0] = maxcorner[1] = maxcorner[2] = 0.0;
   
   if (!dset || !(v=SUMA_GetDatasetI2X(dset, A)) || 
       !(dims = SUMA_GetDatasetDimensions(dset)) ) {
      SUMA_S_Err("no valid ijk_to_dicom_real") ;
      SUMA_RETURNe;
   }
   
   if (mincorner) {
      if (voxcen) {
         mincorner[0] = A[0][3];
         mincorner[1] = A[1][3];
         mincorner[2] = A[2][3];
      } else {
         I[0] = I[1] = I[2] = -0.5;
         AFF44_MULT_I(mincorner, A, I);
      }
   }
   if (maxcorner) {
      if (voxcen) {
         I[0] = dims[0]-1; 
         I[1] = dims[1]-1;
         I[2] = dims[2]-1;
         AFF44_MULT_I(maxcorner, A, I);
      } else {
         I[0] = dims[2]-0.5; 
         I[1] = dims[1]-0.5;
         I[2] = dims[2]-0.5;
         AFF44_MULT_I(maxcorner, A, I);
      }
   }
   
   SUMA_RETURNe;
}

/*!
   Corners of box defining volume boundaries
   
   0,    0,    0
   Nx-1, 0,    0
   Nx-1, Ny-1, 0
   0,    Ny-1, 0
   0,    0,    Nz-1
   Nx-1, 0,    Nz-1
   Nx-1, Ny-1, Nz-1
   0,    Ny-1, Nz-1
    
*/
SUMA_Boolean SUMA_dset_box_corners( SUMA_DSET *dset, 
                                    float * corners, int voxcen)
{
   static char FuncName[]={"SUMA_dset_box_corners"};   
   float A[4][4], I[24], *v, *vi;
   int *dims, i;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   if (!dset || !(v=SUMA_GetDatasetI2X(dset, A)) || 
       !(dims = SUMA_GetDatasetDimensions(dset)) ) {
      SUMA_S_Err("no valid ijk_to_dicom_real") ;
      SUMA_RETURN(NOPE);
   }
   if (!corners) {
      SUMA_S_Err("No return vehicle");
      SUMA_RETURN(NOPE);
   }
   
   /* Fill up the IJKs */
   i = 0;
   I[i++] = 0;         I[i++] = 0;         I[i++] = 0;
   I[i++] = dims[0]-1; I[i++] = 0;         I[i++] = 0;
   I[i++] = dims[0]-1; I[i++] = dims[1]-1; I[i++] = 0;
   I[i++] = 0;         I[i++] = dims[1]-1; I[i++] = 0;
   I[i++] = 0;         I[i++] = 0;         I[i++] = dims[2]-1;
   I[i++] = dims[0]-1; I[i++] = 0;         I[i++] = dims[2]-1;
   I[i++] = dims[0]-1; I[i++] = dims[1]-1; I[i++] = dims[2]-1;
   I[i++] = 0;         I[i++] = dims[1]-1; I[i++] = dims[2]-1;
   
   /* offset for non voxel center? */
   if (!voxcen) {
      for (i=0; i<24; ++i) {
         if (I[i] > 0.0f) I[i] += 0.5;
         else I[i] -= 0.5;
      }
   }
   
   for (i=0; i<24; i = i +3) {
      v = corners+i;
      vi = I+i;
      AFF44_MULT_I(v, A, vi); 
   }
   
   SUMA_RETURN(YUP);
}

/* Older version, OK for cardinal axes only 
   Use SUMA_dset_tex_slice_corners() */
void SUMA_dset_tex_slice_corners_card( int slci, THD_3dim_dataset *dset, 
                              GLfloat *tcorners, GLfloat *corners, 
                              int dim, int voxcen)
{
   static char FuncName[]={"SUMA_dset_tex_slice_corners_card"};   
   int kk=0;
   float orig[3] = { 0, 0, 0}, del[3] = { 0, 0, 0};
   int nvox[3] = { 0, 0, 0};
   int slcx, slcy, slcz=0;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   if (!voxcen) {
      SUMA_LH("voxcen == 0 not supported in this old version");
   }
   
   orig[0] = dset->daxes->xxorg ; 
   orig[1] = dset->daxes->yyorg ; 
   orig[2] = dset->daxes->zzorg ; 
   nvox[0] = DSET_NX(dset);
   nvox[1] = DSET_NY(dset);
   nvox[2] = DSET_NZ(dset);
   del[0] = dset->daxes->xxdel ; 
   del[1] = dset->daxes->yydel ; 
   del[2] = dset->daxes->zzdel ; 

   switch (dim) {
      default:
         SUMA_S_Err("Bad dim value");
         SUMA_RETURNe;
      case 2:
         kk = 0;
    corners[kk] = orig[0] + 0       * del[0]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[1] + 0       * del[1]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[2] + slci    * del[2]; 
   tcorners[kk] = ((float)slci+0.5)/(float)nvox[2];++kk;
   
    corners[kk] = orig[0] + (nvox[0]-1) * del[0];  
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[1] + 0       * del[1]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[2] + slci    * del[2]; 
   tcorners[kk] = tcorners[2];                  ++kk;

   
    corners[kk] = orig[0] + (nvox[0]-1) * del[0]; 
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[1] + (nvox[1]-1) * del[1]; 
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[2] + slci    * del[2]; 
   tcorners[kk] = tcorners[2];                  ++kk;

    corners[kk] = orig[0] + 0       * del[0]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[1] + (nvox[1]-1) * del[1]; 
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[2] + slci    * del[2]; 
   tcorners[kk] = tcorners[2];                  ++kk;
         break;
      case 1:
         kk = 0;
    corners[kk] = orig[0] + 0       * del[0]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[1] + slci    * del[1]; 
   tcorners[kk] = ((float)slci+0.5)/(float)nvox[1];++kk;
    corners[kk] = orig[2] + 0       * del[2]; 
   tcorners[kk] = 0;                            ++kk;
           
    corners[kk] = orig[0] + (nvox[0]-1) * del[0];  
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[1] + slci    * del[1]; 
   tcorners[kk] = tcorners[1];                  ++kk;
    corners[kk] = orig[2] + 0       * del[2]; 
   tcorners[kk] = 0;                            ++kk;


    corners[kk] = orig[0] + (nvox[0]-1) * del[0]; 
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[1] + slci    * del[1]; 
   tcorners[kk] = tcorners[1];                  ++kk;
    corners[kk] = orig[2] + (nvox[2]-1) * del[2]; 
   tcorners[kk] = 1;                            ++kk;

    corners[kk] = orig[0] + 0       * del[0]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[1] + slci    * del[1]; 
   tcorners[kk] = tcorners[1];                  ++kk;
    corners[kk] = orig[2] + (nvox[2]-1) * del[2]; 
   tcorners[kk] = 1;                            ++kk;
         break;
      case 0:
         kk = 0;
    corners[kk] = orig[0] + slci    * del[0]; 
   tcorners[kk] = ((float)slci+0.5)/(float)nvox[0];++kk;
    corners[kk] = orig[1] + 0       * del[1]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[2] + 0       * del[2]; 
   tcorners[kk] = 0;                            ++kk;
           
    corners[kk] = orig[0] + slci    * del[0]; 
   tcorners[kk] = tcorners[0];                  ++kk;
    corners[kk] = orig[1] + (nvox[1]-1) * del[1];  
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[2] + 0       * del[2]; 
   tcorners[kk] = 0;                            ++kk;


    corners[kk] = orig[0] + slci    * del[0]; 
   tcorners[kk] = tcorners[0];                  ++kk;
    corners[kk] = orig[1] + (nvox[1]-1) * del[1]; 
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[2] + (nvox[2]-1) * del[2]; 
   tcorners[kk] = 1;                            ++kk;

    corners[kk] = orig[0] + slci    * del[0]; 
   tcorners[kk] = tcorners[0];                  ++kk;
    corners[kk] = orig[1] + 0       * del[1]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[2] + (nvox[2]-1) * del[2]; 
   tcorners[kk] = 1;                            ++kk;
         break;
   }
   
   if (LocalHead) {
      SUMA_LHv("Slice %d, dim %d corners and textures\n", slci, dim);
      for (kk=0; kk<4; ++kk) {
         fprintf(SUMA_STDERR,
                  "c%d: %.3f   %.3f   %.3f\n"
                  "t%d: %.3f   %.3f   %.3f\n", 
            kk, corners[3*kk],corners[3*kk+1],corners[3*kk+2],
            kk, tcorners[3*kk],tcorners[3*kk+1],tcorners[3*kk+2]);
                              
      }
      fprintf(SUMA_STDERR,"\n");
   }
   
   SUMA_RETURNe;
}

/* Get slider value from texture corners, 
   inverse of SUMA_dset_tex_slice_corners_gui*/
int SUMA_dset_gui_slice_from_tex_slice(SUMA_VolumeElement **VE, int ive,
                     float *PlEq, int voxcen,
                     char *variant,int *slider)
{
   static char FuncName[]={"SUMA_dset_gui_slice_from_tex_slice"};   
   char *orcode;
   int dim=0, nslc=0, *dims;
   float I[3]={0.0, 0.0, 0.0}, C[3]={0.0, 0.0, 0.0}, 
         Dir0[3] = {1, 0, 0}, Dir1[3] = {0, 1, 0}, Dir2[3] = {0, 0, 1},
         dd, d0, d1, d2;
   SUMA_DSET *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;  
   
   if (ive < 0) ive = 0;
   if (!(dset = SUMA_VE_dset(VE, ive)) || !PlEq || 
       !(dims = SUMA_GetDatasetDimensions(dset))) {
      SUMA_S_Err("no dset or no variant") ;
      SUMA_RETURN(-1);
   }
   
   if (slider) *slider = -1;
   
   orcode = SUMA_Dset_orcode(dset);
   if (orcode[0] == 'X') { SUMA_S_Err("No orcode"); SUMA_RETURN(-1); }

   /* Take the normal and turn it to IJK land */
   AFF44_MULT_D(I, VE[ive]->X2I, PlEq);
   SUMA_UNITIZE_VEC(I,3);

   /* Find out which dim you're closest to */
   d0 = SUMA_MT_DOT(I, Dir0); dd = d0; dim = 0;
   d1 = SUMA_MT_DOT(I, Dir1);
   d2 = SUMA_MT_DOT(I, Dir2);
   if (SUMA_ABS(d1) > SUMA_ABS(dd)) {
      dim = 1; dd = d1;
   }
   if (SUMA_ABS(d2) > SUMA_ABS(dd)) {
      dim = 2; dd = d2;
   }
   SUMA_LH("PlEq: %f %f %f %f\n"
            "I  : %f %f %f\n"
            "Dots: %f %f %f, orcode %s, dim %d", 
           PlEq[0], PlEq[1], PlEq[2], PlEq[3],
           I[0], I[1], I[2], d0, d1, d2, orcode, dim)
   if (variant) {
           if (orcode[dim] == 'I' || orcode[dim] == 'S') sprintf(variant,"Ax");
      else if (orcode[dim] == 'R' || orcode[dim] == 'L') sprintf(variant,"Sa");
      else if (orcode[dim] == 'A' || orcode[dim] == 'P') sprintf(variant,"Co");
   } 
   
   /* Don't bother which slice number this is, just return the dim */
   SUMA_RETURN(dim); 
}

int SUMA_dset_gui_slice_from_tex_slice_d(SUMA_VolumeElement **VE, int ive,
                     double *PlEq, int voxcen,
                     char *variant,int *slider)
{
   static char FuncName[]={"SUMA_dset_gui_slice_from_tex_slice_d"};
   float fv[4];
   if (!PlEq) return(-1);
   fv[0] = PlEq[0];    
   fv[1] = PlEq[1];    
   fv[2] = PlEq[2];    
   fv[3] = PlEq[3];
   return(SUMA_dset_gui_slice_from_tex_slice(VE, ive, fv, 
                                             voxcen, variant, slider));
}    

/* Get texture corners from slider values
\sa SUMA_dset_gui_slice_from_tex_slice*/
int SUMA_dset_tex_slice_corners_gui(SUMA_VolumeElement **VE, int ive, 
                                             char *variant,int slider, 
                          GLfloat *tcorners, GLfloat *corners, GLfloat *slc_cen,
                          float *PlEq, int voxcen )
{
   static char FuncName[]={"SUMA_dset_tex_slice_corners_gui"};   
   char *orcode;
   int dim=0, nslc=0, *dims;
   float I[3]={0.0, 0.0, 0.0}, C[3]={0.0, 0.0, 0.0};
   SUMA_DSET *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;  
   
   if (ive < 0) ive = 0;
   if (!(dset = SUMA_VE_dset(VE, ive)) || !variant|| 
       !(dims = SUMA_GetDatasetDimensions(dset))) {
      SUMA_S_Err("no dset or no variant") ;
      SUMA_RETURN(-1);
   }
   
   orcode = SUMA_Dset_orcode(dset);
   if (orcode[0] == 'X') { SUMA_S_Err("No orcode"); SUMA_RETURN(-1); }
   
   switch (variant[0]) {
      case 'A': /* axial slicing desired*/
         if (orcode[0] == 'I' || orcode[0] == 'S') { 
            dim = 0; nslc = slider;
            if (orcode[0] == 'S') nslc = VE[ive]->Ni-1-slider;
            if (nslc < 0) nslc = 0;
            if (nslc >= VE[ive]->Ni) nslc = VE[ive]->Ni-1;
         } else if (orcode[1] == 'I' || orcode[1] == 'S') { 
            dim = 1; nslc = slider;
            if (orcode[1] == 'S') nslc = VE[ive]->Nj-1-slider;
            if (nslc < 0) nslc = 0;
            if (nslc >= VE[ive]->Nj) nslc = VE[ive]->Nj-1;
         } else if (orcode[2] == 'I' || orcode[2] == 'S') { 
            dim = 2; nslc = slider;
            if (orcode[2] == 'S') nslc = VE[ive]->Nk-1-slider;
            if (nslc < 0) nslc = 0;
            if (nslc >= VE[ive]->Nk) nslc = VE[ive]->Nk-1;
         }
         break;
      case 'S': /* sagittal slicing desired */
         if (orcode[0] == 'R' || orcode[0] == 'L') { 
            dim = 0; nslc = slider;
            if (orcode[0] == 'L') nslc = VE[ive]->Ni-1-slider;
            if (nslc < 0) nslc = 0;
            if (nslc >= VE[ive]->Ni) nslc = VE[ive]->Ni-1;
         } else if (orcode[1] == 'R' || orcode[1] == 'L') { 
            dim = 1; nslc = slider;
            if (orcode[1] == 'L') nslc = VE[ive]->Nj-1-slider;
            if (nslc < 0) nslc = 0;
            if (nslc >= VE[ive]->Nj) nslc = VE[ive]->Nj-1;
         } else if (orcode[2] == 'R' || orcode[2] == 'L') { 
            dim = 2; nslc = slider;
            if (orcode[2] == 'L') nslc = VE[ive]->Nk-1-slider;
            if (nslc < 0) nslc = 0;
            if (nslc >= VE[ive]->Nk) nslc = VE[ive]->Nk-1;
         } 
         break;
       case 'C': /* coronal slicing desired */
         if (orcode[0] == 'A' || orcode[0] == 'P') { 
            dim = 0; nslc = slider;
            if (orcode[0] == 'P') nslc = VE[ive]->Ni-1-slider;
            if (nslc < 0) nslc = 0;
            if (nslc >= VE[ive]->Ni) nslc = VE[ive]->Ni-1;
         } else if (orcode[1] == 'A' || orcode[1] == 'P') { 
            dim = 1; nslc = slider;
            if (orcode[1] == 'P') nslc = VE[ive]->Nj-1-slider;
            if (nslc < 0) nslc = 0;
            if (nslc >= VE[ive]->Nj) nslc = VE[ive]->Nj-1;
         } else if (orcode[2] == 'A' || orcode[2] == 'P') { 
            dim = 2; nslc = slider;
            if (orcode[2] == 'P') nslc = VE[ive]->Nk-1-slider;
            if (nslc < 0) nslc = 0;
            if (nslc >= VE[ive]->Nk) nslc = VE[ive]->Nk-1;
         } 
         break;
      case 'V': /* Volume rendering, do not do much */
         SUMA_S_Note("What to return here?");
         SUMA_RETURN(0);
         break;
      default:
         SUMA_S_Err("What gives? Variant '%s'", variant);
         SUMA_DUMP_TRACE("Bad variant trace");
         SUMA_RETURN(-1);
   }
   if (PlEq) { /* Equation of plane for slice in question */
      float mid_slc[3];
      /* Compute normal direction and slice center in index space */
      switch (dim) {
         case 0:
            I[0] = 1;            I[1] = 0;            I[2] = 0; 
            C[0] = nslc;         C[1] = dims[1]/2.0;  C[2] = dims[2]/2.0;
            break;
         case 1:
            I[0] = 0;            I[1] = 1;            I[2] = 0; 
            C[0] = dims[0]/2.0;  C[1] = nslc;         C[2] = dims[2]/2.0;
            break;
         case 2:
            I[0] = 0;            I[1] = 0;            I[2] = 1; 
            C[0] = dims[0]/2.0;  C[1] = dims[1]/2.0;  C[2] = nslc;
            break;
      }
      
      AFF44_MULT_D(PlEq, VE[ive]->I2X, I); /* Compute normal in xyz */

      AFF44_MULT_I(mid_slc, VE[ive]->I2X, C); /* center of slice */
      SUMA_SHIFT_PLANE_TO_P(PlEq, mid_slc); /*go through mid of slice*/
   }
   if (tcorners || corners || slc_cen) {
      SUMA_dset_tex_slice_corners(nslc, dset, tcorners, corners, slc_cen, 
                                  dim, voxcen);
   }
   SUMA_RETURN(dim);
}

/* Take an XYZ in RAI and return Ax, Sa, and Co GUI slider positions */
float* SUMA_XYZ_to_gui_slices(SUMA_VolumeElement **VE, int ive, 
                                    float *xyz, float *here)
{
   static char FuncName[]={"SUMA_XYZ_to_gui_slices"};
   static float n[10][3];
   int icall=0;
   char *orcode;
   int dim=0, nslc=0, *dims;
   float I[3]={0.0, 0.0, 0.0}, C[3]={0.0, 0.0, 0.0};
   SUMA_DSET *dset=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;  
   
   if (icall > 9) icall=0;
   else ++icall;
   
   if (!here) here = (float *)(n[icall]);
   here[0] = here[1] = here[2] = -1;
   
   if (ive < 0) ive = 0;
   if (!xyz || !(dset = SUMA_VE_dset(VE, ive)) || 
       !(dims = SUMA_GetDatasetDimensions(dset)) ) {
      SUMA_S_Err("no dset or no variant") ;
      SUMA_RETURN(here);
   }
   
   orcode = SUMA_Dset_orcode(dset);
   if (orcode[0] == 'X') { SUMA_S_Err("No orcode"); SUMA_RETURN(here); }
   
   /* Change XYZ to I */
   AFF44_MULT_I(I, VE[ive]->X2I, xyz);
   
   /* Get the slider values for A, S, C, in this order */
   /* First the Ax slice number */
   dim = 0;
   if (orcode[0] == 'I' || orcode[0] == 'S') { 
      if (orcode[0] == 'S') here[dim] = VE[ive]->Ni-1-SUMA_ROUND(I[0]);
      else here[dim] = SUMA_ROUND(I[0]);
      if (here[dim] < 0) here[dim] = 0;
      if (here[dim] >= VE[ive]->Ni) here[dim] = VE[ive]->Ni-1;
   } else if (orcode[1] == 'I' || orcode[1] == 'S') { 
      if (orcode[1] == 'S') here[dim] = VE[ive]->Nj-1-SUMA_ROUND(I[1]);
      else here[dim] = SUMA_ROUND(I[1]);
      if (here[dim] < 0) here[dim] = 0;
      if (here[dim] >= VE[ive]->Nj) here[dim] = VE[ive]->Nj-1;
   } else if (orcode[2] == 'I' || orcode[2] == 'S') { 
      if (orcode[2] == 'S') here[dim] = VE[ive]->Nk-1-SUMA_ROUND(I[2]);
      else here[dim] = SUMA_ROUND(I[2]);
      if (here[dim] < 0) here[dim] = 0;
      if (here[dim] >= VE[ive]->Nk) here[dim] = VE[ive]->Nk-1;      
   }
   dim = 1;  /* Now look for Sag */
   if (orcode[0] == 'R' || orcode[0] == 'L') { 
      if (orcode[0] == 'L') here[dim] = VE[ive]->Ni-1-SUMA_ROUND(I[0]);
      else here[dim] = SUMA_ROUND(I[0]);
      if (here[dim] < 0) here[dim] = 0;
      if (here[dim] >= VE[ive]->Ni) here[dim] = VE[ive]->Ni-1;
   } else if (orcode[1] == 'R' || orcode[1] == 'L') { 
      if (orcode[1] == 'L') here[dim] = VE[ive]->Nj-1-SUMA_ROUND(I[1]);
      else here[dim] = SUMA_ROUND(I[1]);
      if (here[dim] < 0) here[dim] = 0;
      if (here[dim] >= VE[ive]->Nj) here[dim] = VE[ive]->Nj-1;
   } else if (orcode[2] == 'R' || orcode[2] == 'L') { 
      if (orcode[2] == 'L') here[dim] = VE[ive]->Nk-1-SUMA_ROUND(I[2]);
      else here[dim] = SUMA_ROUND(I[2]);
      if (here[dim] < 0) here[dim] = 0;
      if (here[dim] >= VE[ive]->Nk) here[dim] = VE[ive]->Nk-1;      
   } 
   dim = 2;  /* Now look for Co */
   if (orcode[0] == 'A' || orcode[0] == 'P') { 
      if (orcode[0] == 'P') here[dim] = VE[ive]->Ni-1-SUMA_ROUND(I[0]);
      else here[dim] = SUMA_ROUND(I[0]);
      if (here[dim] < 0) here[dim] = 0;
      if (here[dim] >= VE[ive]->Ni) here[dim] = VE[ive]->Ni-1;
   } else if (orcode[1] == 'A' || orcode[1] == 'P') { 
      if (orcode[1] == 'P') here[dim] = VE[ive]->Nj-1-SUMA_ROUND(I[1]);
      else here[dim] = SUMA_ROUND(I[1]);
      if (here[dim] < 0) here[dim] = 0;
      if (here[dim] >= VE[ive]->Nj) here[dim] = VE[ive]->Nj-1;
   } else if (orcode[2] == 'A' || orcode[2] == 'P') { 
      if (orcode[2] == 'P') here[dim] = VE[ive]->Nk-1-SUMA_ROUND(I[2]);
      else here[dim] = SUMA_ROUND(I[2]);
      if (here[dim] < 0) here[dim] = 0;
      if (here[dim] >= VE[ive]->Nk) here[dim] = VE[ive]->Nk-1;      
   }
   SUMA_RETURN(here); 
}

/* Set slices of VO at location xyz */
SUMA_Boolean SUMA_VO_set_slices_XYZ(SUMA_VolumeObject *VOu, float *xyz)
{
   static char FuncName[]={"SUMA_VO_set_slices_XYZ"};
   float *slices;
   int i;
   SUMA_VOL_SAUX *VSaux=NULL;
   SUMA_VolumeObject *VO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!xyz) SUMA_RETURN(NOPE);

   for (i=0; i<SUMAg_N_DOv; ++i) {
      if (VOu) VO = VOu;
      else if (iDO_type(i) == VO_type) {
         VO = (SUMA_VolumeObject *)iDO_ADO(i);
      }else VO = NULL;
      VSaux = (SUMA_VOL_SAUX *)VDO_VSAUX(VO);
      if ((VO && VSaux->SlicesAtCrosshair) || VOu) { /* Do this if user
                                               supplies volume or
                                               if volumes request it */
         slices = SUMA_XYZ_to_gui_slices(VO->VE, 0, xyz, NULL);
         SUMA_LH("Jumping to slices Ax%f Sa%f Co%f", 
                  slices[0], slices[1], slices[2]);
         SUMA_set_slice((SUMA_ALL_DO *)VO, "Ax", 
                                  slices, "EngXYZ", 0);
         SUMA_set_slice((SUMA_ALL_DO *)VO, "Sa", 
                                  slices+1, "EngXYZ", 0);
         SUMA_set_slice((SUMA_ALL_DO *)VO, "Co", 
                                  slices+2, "EngXYZ", 0);
      }
      if (VOu) break; 
   }
   SUMA_RETURN(YUP);
}

void SUMA_dset_tex_slice_corners( int slci, SUMA_DSET *dset, 
                           GLfloat *tcorners, GLfloat *corners, GLfloat *slc_cen,
                              int dim, int voxcen)
{
   static char FuncName[]={"SUMA_dset_tex_slice_corners"};   
   int kk=0;
   int slcx, slcy, slcz=0, *dims;
   float A[4][4], I[3], T[3];
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
      
   if (tcorners) memset(tcorners, 0, 4*3*sizeof(float));
   if (corners) memset(corners, 0, 4*3*sizeof(float));
   
   if (!dset || !(SUMA_GetDatasetI2X(dset, A)) ||
       !(dims=SUMA_GetDatasetDimensions(dset)) ||
       !((tcorners && corners) || slc_cen) ) {
      SUMA_S_Err("no valid ijk_to_dicom_real or null input") ;
      SUMA_RETURNe;
   }
   
   if (tcorners && corners) {
      switch (dim) {
         default:
            SUMA_S_Err("Bad dim value");
            SUMA_RETURNe;
         case 2:
            kk = 0;
            I[0] = 0; I[1] = 0; I[2] = slci;
            T[0] = 0; T[1] = 0; T[2] = ((float)slci+0.5)/(float)dims[2];
            if (!voxcen) {I[0] -= 0.5; I[1] -= 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);

            kk = 1;
            I[0] = dims[0]-1; I[1] = 0; I[2] = slci;  
            T[0] = 1;               T[1] = 0; T[2] = tcorners[2];
            if (!voxcen) {I[0] += 0.5; I[1] -= 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);

            kk = 2;
            I[0] = dims[0]-1; I[1] = dims[1]-1; I[2] = slci;  
            T[0] = 1;               T[1] = 1;               T[2] = tcorners[2];
            if (!voxcen) {I[0] += 0.5; I[1] += 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);

            kk = 3;
            I[0] = 0; I[1] = dims[1]-1; I[2] = slci;  
            T[0] = 0; T[1] = 1;               T[2] = tcorners[2];
            if (!voxcen) {I[0] -= 0.5; I[1] += 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);
            break;

         case 1:
            kk = 0;
            I[0] = 0; I[1] = slci;                                   I[2] = 0;
            T[0] = 0; T[1] = ((float)slci+0.5)/(float)dims[1]; T[2] = 0;
            if (!voxcen) {I[0] -= 0.5; I[2] -= 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);

            kk = 1;
            I[0] = dims[0]-1; I[1] = slci;        I[2] = 0;
            T[0] = 1;               T[1] = tcorners[1]; T[2] = 0;
            if (!voxcen) {I[0] += 0.5; I[2] -= 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);

            kk = 2;
            I[0] = dims[0]-1; I[1] = slci;        I[2] = dims[2]-1;
            T[0] = 1;               T[1] = tcorners[1]; T[2] = 1;
            if (!voxcen) {I[0] += 0.5; I[2] += 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);

            kk = 3;
            I[0] = 0; I[1] = slci;        I[2] = dims[2]-1;
            T[0] = 0; T[1] = tcorners[1]; T[2] = 1;
            if (!voxcen) {I[0] -= 0.5; I[2] += 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);
            break;

         case 0:
            kk = 0;
            I[0] = slci;                                   I[1] = 0; I[2] = 0;
            T[0] = ((float)slci+0.5)/(float)dims[0]; T[1] = 0; T[2] = 0;
            if (!voxcen) {I[1] -= 0.5; I[2] -= 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);

            kk = 1;
            I[0] = slci;        I[1] = dims[1]-1; I[2] = 0;
            T[0] = tcorners[0]; T[1] = 1;               T[2] = 0;
            if (!voxcen) {I[1] += 0.5; I[2] -= 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);

            kk = 2;
            I[0] = slci;        I[1] = dims[1]-1; I[2] = dims[2]-1;
            T[0] = tcorners[0]; T[1] = 1;               T[2] = 1;
            if (!voxcen) {I[1] += 0.5; I[2] += 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);

            kk = 3;
            I[0] = slci;        I[1] = 0; I[2] = dims[2]-1;
            T[0] = tcorners[0]; T[1] = 0; T[2] = 1;
            if (!voxcen) {I[1] -= 0.5; I[2] += 0.5; }
            AFF44_MULT_I((corners+3*kk), A, I);
            SUMA_COPY_VEC(T, tcorners+3*kk, 3, float, GLfloat);
            break;
      }
   
      if (LocalHead) {
         float cen[3] = {0.0, 0.0, 0.0};
         SUMA_LHv("Slice %d, dim %d %s corners and textures\n", 
                  slci, dim,
                  voxcen ? "Voxel Center":"Slice Edge");
         for (kk=0; kk<4; ++kk) {
            fprintf(SUMA_STDERR,
                     "c%d: %.3f   %.3f   %.3f\n"
                     "t%d: %.3f   %.3f   %.3f\n", 
               kk, corners[3*kk],corners[3*kk+1],corners[3*kk+2],
               kk, tcorners[3*kk],tcorners[3*kk+1],tcorners[3*kk+2]);
           cen[0] += corners[3*kk  ];
           cen[1] += corners[3*kk+1];                      
           cen[2] += corners[3*kk+2];                      
         }
         fprintf(SUMA_STDERR,"\n");
         cen[0] /= 4.0; cen[1] /= 4.0; cen[2] /= 4.0;
         fprintf(SUMA_STDERR, "Slice center RAI coord: %f %f %fmm\n", 
                              cen[0], cen[1], cen[2]);
      }
   }
   if (slc_cen) {
      switch (dim) {
         default:
            SUMA_S_Err("Bad dim value");
            SUMA_RETURNe;
         case 2:
            I[0] = dims[0]/2.0; I[1] = dims[1]/2.0; I[2] = slci;
            if (!voxcen) {I[0] -= 0.5; I[1] -= 0.5; }
            AFF44_MULT_I((slc_cen), A, I);
            break;
         case 1:
            I[0] = dims[0]/2.0; I[1] = slci; I[2] = dims[2]/2.0;
            if (!voxcen) {I[0] -= 0.5; I[2] -= 0.5; }
            AFF44_MULT_I((slc_cen), A, I);
            break;
         case 0:
            I[0] = slci; I[1] = dims[1]/2.0; I[2] = dims[2]/2.0;
            if (!voxcen) {I[1] -= 0.5; I[2] -= 0.5; }
            AFF44_MULT_I((slc_cen), A, I);
            break;
      }
      SUMA_LH( "Slice center at RAI mm: %f %f %f",
               slc_cen[0], slc_cen[1], slc_cen[2]);
   }
   
   SUMA_RETURNe;
}

int SUMA_VO_SelectedSlice(SUMA_VolumeObject *vo, char *variant, float *scorners)
{
   static char FuncName[]={"SUMA_VO_SelectedSlice"};
   SUMA_ALL_DO *ado=(SUMA_ALL_DO *)vo;
   SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
   int nslc = -1, dim, k;
   GLfloat slc_corners[12], slc_tcorners[12];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!( (VSaux = SUMA_ADO_VSaux(ado)) && 
          variant && 
          VSaux->PR && 
          VSaux->PR->iAltSel[SUMA_VOL_I] >= 0 &&
          VSaux->PR->iAltSel[SUMA_VOL_J] >= 0 &&
          VSaux->PR->iAltSel[SUMA_VOL_K] >= 0 ) ) {
      SUMA_RETURN(-1);
   }
   
   #if 0 /* Old way, does not work when in montage mode.
            Keep for the record */
   if ((dim = SUMA_dset_gui_slice_from_tex_slice_d(vo->VE, 0, 
                                 VSaux->PR->dAltSel+SUMA_VOL_SLC_EQ0, 
                                 0, variant,NULL))< 0) {
      SUMA_RETURN(-1);
   }
   
   
   nslc = VSaux->PR->iAltSel[SUMA_VOL_I+dim];
   
   SUMA_LH("Slice variant %s, dim %d. [%ld %ld %ld] -->%d", 
            variant, dim, 
            VSaux->PR->iAltSel[SUMA_VOL_I], 
            VSaux->PR->iAltSel[SUMA_VOL_J],
            VSaux->PR->iAltSel[SUMA_VOL_K],
            nslc);
   if (nslc >= 0 && scorners) {
      SUMA_dset_tex_slice_corners_gui(vo->VE, 0, variant, nslc, 
                          slc_tcorners, slc_corners, 
                          NULL, NULL, 0 );
      for (k=0; k<12; ++k) scorners[k] = slc_corners[k];
   }
   #else
   SUMA_SlcCodeToVariant(VSaux->PR->iAltSel[SUMA_VOL_SLC_VARIANT], 
                         variant);
   nslc = VSaux->PR->iAltSel[SUMA_VOL_SLC_NUM];
   SUMA_LH("Slice variant %s, [%ld %ld %ld] -->%d", 
            variant,  
            VSaux->PR->iAltSel[SUMA_VOL_I], 
            VSaux->PR->iAltSel[SUMA_VOL_J],
            VSaux->PR->iAltSel[SUMA_VOL_K],
            nslc);
   if (nslc >= 0 && scorners && 
       VSaux->PR->iAltSel[SUMA_VOL_SLC_VARIANT] != SUMA_VR_VARIANT) {
      SUMA_dset_tex_slice_corners_gui(vo->VE, 0, variant, nslc, 
                          slc_tcorners, slc_corners, 
                          NULL, NULL, 0 );
      for (k=0; k<12; ++k) scorners[k] = slc_corners[k];
   }
   #endif
   SUMA_RETURN(nslc);
}


SUMA_Boolean SUMA_DrawVolumeDO_OLD(SUMA_VolumeObject *VO, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawVolumeDO_OLD"};
   int i = 0, k = 0, j=0, ive=0;
   float iq[4]={0, 0, 0, 0}, vo0[3], voN[3];
   static int ipass=0, iplane = 0;
   GLfloat tex_corn[12] ;
   GLfloat slc_corn[12] ;
   GLfloat rotationMatrix[4][4], rt[4][4];
   GLboolean gl_dt, gl_bl;
   int ShowUnselected = 1;
   float tz = 0.0;
   static GLfloat init_rotationMatrix[4][4];
   static GLdouble dmatrix[16], init_mv_matrix[16];
   float* nlt; /*JB: temporary node list, because I do not want to type 
                  "VO->SOcut[0]->NodeList" over and over...*/
   SUMA_Boolean LastTextureOnCutPlane=YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!VO) SUMA_RETURN(NOPE);
   if (!sv) sv = &(SUMAg_SVv[0]);
   
   if (sv->DO_PickMode) {
      SUMA_LH("No need to draw volume in DO_PickMode");
      SUMA_RETURN(YUP);
   }
   
   if (!VO->Show) SUMA_RETURN(YUP);
   
   if (sv->PolyMode != SRM_Fill) {
      /* fill it up */
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);   
   }
   
   if (!VO->SOcut || !VO->SOcut[0]) SUMA_VO_InitCutPlanes(VO);
   
   
   if (0) {
      SUMA_LH( "Draw Clipping planes without modelview matrix enabled\n"
            "So planes are fixed in space.\n"
            "We're more used to creating cutting planes that \n"
            "are attached to the object. So should look into that\n"
            "instead. \n"
            "Note that the use of these clipping planes\n"
            "seem necessary when the volume is not padded with zeros.\n"
            "I don't quite know what that is, but I suspect it has to do\n"
            "with alphas being non zero at the tip and the placement of\n"
            "texture quads\n"  );
   }
   glClipPlane(GL_CLIP_PLANE0, VO->CutPlane[0]);
   glClipPlane(GL_CLIP_PLANE1, VO->CutPlane[1]);
   glClipPlane(GL_CLIP_PLANE2, VO->CutPlane[2]);
   glClipPlane(GL_CLIP_PLANE3, VO->CutPlane[3]);
   glClipPlane(GL_CLIP_PLANE4, VO->CutPlane[4]);
   glClipPlane(GL_CLIP_PLANE5, VO->CutPlane[5]); /* play with this 
                                             one to change cuts*/

   
   /* Now we need to draw the sucker */
   SUMA_CHECK_GL_ERROR("OpenGL Error pre texture");
   glEnable(GL_TEXTURE_3D);

   /* enable clipping planes. */
   glEnable(GL_CLIP_PLANE0);
   glEnable(GL_CLIP_PLANE1);
   glEnable(GL_CLIP_PLANE2);
   glEnable(GL_CLIP_PLANE3);
   glEnable(GL_CLIP_PLANE4);
   glEnable(GL_CLIP_PLANE5);

   glTexEnvf(  GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, 
               VO->TexEnvMode   ); /* what happens if 
                              there is color already on a vertex (I would likely
                              not need this for 3D textures...*/
   
   gl_dt = glIsEnabled(GL_DEPTH_TEST);
   gl_bl = glIsEnabled(GL_BLEND);  
   
   ive = 0;   
   while (VO->VE && VO->VE[ive]) {
      if (!VO->VE[ive]->texName) {
         if (!SUMA_CreateGL3DTexture(VO)) {
            SUMA_S_Err("Failed to create texture");
            SUMA_RETURN(NOPE);
         }      
      }

      SUMA_LHv("About to bind texture %d for %s\n", 
            VO->VE[ive]->texName[0], SUMA_VE_Headname(VO->VE, ive));
      glBindTexture(GL_TEXTURE_3D, VO->VE[ive]->texName[0]); 
                                             /* make texName be current */

      /* Now generate the coordinates */
      SUMA_LHv( "About to generate polygons for dset %d (%p)\n", 
               ive, SUMA_VE_dset(VO->VE, ive)); 
      SUMA_LHv( "About to generate polygons for dset %d (%s)\n", 
               ive, SUMA_VE_Headname(VO->VE, ive)); 
      glShadeModel(GL_FLAT);     /* This should be reverted to sv's settings */
      if (!(gl_dt)) glEnable(GL_DEPTH_TEST);      
      if (!(gl_bl)) glEnable(GL_BLEND);
	   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      if (0 && LocalHead) {
         SUMA_GL_MAT_SHOW(GL_TEXTURE_MATRIX,"Tx PreSetup\n");
         SUMA_GL_MAT_SHOW(GL_MODELVIEW_MATRIX,"MV PreSetup\n");
      }
      SUMA_CHECK_GL_ERROR("OpenGL Error pre setup");

      /* The modelview matrix for drawing the quad vertices should remain the 
      same, the texture matrix will reflect object rotations.
      IF you do want to see the 'slices', i.e. a stack of axial slices (for RAI 
      dset), rather than volume rendering image, then allo the quads to be drawn
      with the scene's current modelview matrix and keep the texture matrix
      as the identity matrix. 
      This slice rendering could be useful to show
      a triplet of slices (Ax, Sa, and Co), rather than the entire volume say. 
      But that should
      be done as a separate nel, or perhaps a different way of drawing a dset 
      within SUMA. One should also decide whether that display is better done
      as 3 2D textures, rather than storing an entire 3D texture and just drawing
      a few quads from it */
      if (!ipass) {
         /* store the Modelview_matrix to reuse later 
         Later on, this should be created automatically, when
         dset is loaded. I am not sure if using whatever was there
         when dset was first loaded is ideal in terms of artifact reduction
         The important thing however, is that the quads are drawn with the
         same rotation matrix, translation should be applied normally. 
         It is the texture matrix that is changed
         with mouse movements, thereby allowing the volume to appear to move
         with other objects in the scene */

         if (LocalHead) {
            SUMA_GL_MAT_SHOW(GL_MODELVIEW_MATRIX,"MV PreRecord\n");
         }
         glGetDoublev(GL_MODELVIEW_MATRIX, init_mv_matrix); 
         if (LocalHead) {
            fprintf(stderr,"init_mv_matrix initialized to:\n");
            for (i=0;i<16;++i) fprintf(stderr,"%.3f\t", init_mv_matrix[i]);
            fprintf(stderr,"\n");
         }
         /* also store the rotation matrix at initialization, this is 
         done to account for any prerotations already present on the scene */
         SUMA_build_rotmatrix(init_rotationMatrix, 
                              sv->GVS[sv->StdView].currentQuat); 
         SUMA_S_Note("The use of this ipass variable is silly and would\n"
                     "only work when we are loading one texture only once.\n"
                     "The parameters recorded here, should be part of nel,\n"
                     "at the initialization stage and not stored this way.\n"
                     "The same rant goes for silly clipping planes.\n"  );  
         ++ipass;                            
      } else {
         if (0 && LocalHead) {
            SUMA_LH("init_mv_matrix recorded to be:");
            for (i=0;i<16;++i) fprintf(stderr,"%.3f\t", init_mv_matrix[i]);
            fprintf(stderr,"\n");       
         }
      }

      /* first set the modelview to the one existing when the texture was
         first rendered*/
         glMatrixMode(GL_MODELVIEW); glPushMatrix(); /* get a new one*/
         glLoadIdentity(); /* set to identity*/
         /* Now apply constant Modelview for Quads*/
         glTranslatef ( sv->GVS[sv->StdView].translateVec[0], 
                     sv->GVS[sv->StdView].translateVec[1], 0.0);   
         glMultMatrixd(init_mv_matrix);

      /* now for the texture coordinates, these should be transformed by
         the inverse of what is done to the objects in the scene */
         glMatrixMode(GL_TEXTURE); glPushMatrix(); /* get a new one*/
         glLoadIdentity(); /* set to identity */

         /* Now apply the inverse rotation params to the texture coordinates */
         SUMA_build_rotmatrix(rotationMatrix, sv->GVS[sv->StdView].currentQuat);
         
         if (sv->ortho) {
            SUMA_TRANSP_4MATRIX(rotationMatrix); /* transpose = inverse 
                                                for rotation matrix*/
         } else {
            SUMA_INV_44ROTATIONMATRIX(rotationMatrix);
         }
         glTranslatef ( 0.5, 0.5, 0.5); 
         glMultMatrixf(&rotationMatrix[0][0]);
         glMultMatrixf(&init_rotationMatrix[0][0]);  /* Need to multiply by
                                                        initial rotation applied
                                                        to the scene. (initial=at 
                                                        the time quads were 1st
                                                        drawn*/
         glTranslatef (-0.5, -0.5, -0.5);  


      if (0 && LocalHead) {
         SUMA_GL_MAT_SHOW(GL_TEXTURE_MATRIX,"Tx PreDraw\n");
         SUMA_GL_MAT_SHOW(GL_MODELVIEW_MATRIX,"MV PreDraw\n");
      }

      SUMA_CHECK_GL_ERROR("OpenGL Error pre draw");
      #if 1
      /* slice by slice drawing, doing the deed by hand*/
      for(i = 0; i < VO_NK(VO); i++) {
         glBegin(GL_QUADS);
            SUMA_dset_tex_slice_corners(i, SUMA_VE_dset(VO->VE,ive), tex_corn, 
                                        slc_corn, NULL, 2, 0);
            for (k=0; k<4; ++k) {
               glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tex_corn[3*k+2]);
                     /* this one is affected by the Texture MatrixMode */
               glVertex3f(slc_corn[3*k], slc_corn[3*k+1], slc_corn[3*k+2]); 
                     /* this one is affected by the Modelview matrixMode*/
            }
         glEnd();
      }
      #else /* automatic texture coordinate generation, 
            same artifact, and more complicated splane and rplane stuff, leave it
           just in case I'll need it. */
       glEnable(GL_TEXTURE_GEN_S);
       glEnable(GL_TEXTURE_GEN_T);
       glEnable(GL_TEXTURE_GEN_R);

       glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
       glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
       glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);

       glTexGenfv(GL_S, GL_OBJECT_PLANE, splane);/*This ordering of the planes */
       glTexGenfv(GL_T, GL_OBJECT_PLANE, rplane);  /* resulted in proper axis */
       glTexGenfv(GL_R, GL_OBJECT_PLANE, tplane);  /*  alignment ...*/



       for(i = 0; i < DSET_NZ(VO->VE[ive]->dset); i++) {
         glBegin(GL_QUADS);
            SUMA_dset_tex_slice_corners(i,VO->VE[ive]->dset, 
                                          tex_corn, slc_corn, NULL, 2, 0);
            for (k=0; k<4; ++k) {
               glVertex3f(slc_corn[3*k], slc_corn[3*k+1], slc_corn[3*k+2]);
            }
         glEnd();
      }
      glDisable(GL_TEXTURE_GEN_S);
      glDisable(GL_TEXTURE_GEN_T);
      glDisable(GL_TEXTURE_GEN_R);
      #endif
       SUMA_CHECK_GL_ERROR("OpenGL Error ddd");

      glMatrixMode(GL_TEXTURE); glPopMatrix(); /* pop matrix of GL_TEXTURE */
      glMatrixMode(GL_MODELVIEW);  glPopMatrix();/* and Modelview*/
     
      ++ive;
   }
   
   glFlush();
   
   /* disable the clipping planes */
   glDisable(GL_CLIP_PLANE0);
   glDisable(GL_CLIP_PLANE1);
   glDisable(GL_CLIP_PLANE2);
   glDisable(GL_CLIP_PLANE3);
   glDisable(GL_CLIP_PLANE4);
   glDisable(GL_CLIP_PLANE5);

   
   /* Here we create a texture on the cutplane from the last dset loaded
   At the moment, without this texture, nothing shows 
   of the overlay volume */
   if (LastTextureOnCutPlane) {
      --ive; /* bring ive counter to last dset put into texture*/
      SUMA_dset_tex_slice_corners( 0, SUMA_VE_dset(VO->VE, ive), 
                                   tex_corn, slc_corn, NULL, 2, 0);

      // Joachim says this is just wrong ...
      tz = 0.5+(-VO->CutPlane[0][3])/(float)VO_NK(VO);

      if (!gl_dt) glDisable(GL_DEPTH_TEST);

      /* If it were not for the slice textures shown here, then the overlay 
         texture would not show up at all! */
      #if 0 
      SUMA_LH("Texture on the slice, with triangles");
      glBegin(GL_TRIANGLES);
         k = 0;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]); ++k;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]); ++k;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]);

         k = 0;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]); k+=2;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]); ++k;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]);

      glEnd();
      #else
      SUMA_LH("Texture on the slice, QUADS?");
      glBegin(GL_QUADS);
         for (k=0; k<4; ++k) {
            glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz);
                  /* this one is affected by the Texture MatrixMode */
            glVertex3f(slc_corn[3*k], slc_corn[3*k+1], 
                                          -VO->CutPlane[0][3]); 
                  /* this one is affected by the Modelview matrixMode*/
         }
      glEnd();
      #endif
   }
      glDisable(GL_TEXTURE_3D);
   if (!gl_dt) glDisable(GL_DEPTH_TEST);
   if (!gl_bl) glDisable(GL_BLEND);
   
   glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE); 
   glEnable(GL_COLOR_MATERIAL);
   for (iplane=0; iplane < 6; ++iplane) {
      if (VO->UseCutPlane[iplane]) {
         if (iplane == VO->SelectedCutPlane) glColor3f(1.0, 1.0, 1.0);
         else { 
            if (ShowUnselected) {
               if (iplane==0 || iplane == 1) glColor3f(1.0, 0.0, 0.0); 
               if (iplane==2 || iplane == 3) glColor3f(0.0, 1.0, 0.0); 
               if (iplane==4 || iplane == 5) glColor3f(0.0, 0.0, 1.0); 
            } else {
               continue;
            }
         }
         glBegin(GL_LINE_LOOP);
            nlt = VO->SOcut[iplane]->NodeList; 
            glVertex3f( nlt[0],nlt[1],nlt[2] );
            glVertex3f( nlt[3],nlt[4],nlt[5] );
            glVertex3f( nlt[6],nlt[7],nlt[8] );
            glVertex3f( nlt[9],nlt[10],nlt[11] );
         glEnd();
      }
   }
   glDisable(GL_COLOR_MATERIAL);

   if (sv->PolyMode != SRM_Fill) {/* set fill mode back */
      SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
   }

   
   SUMA_RETURN(YUP);
   
}

SUMA_Boolean SUMA_GET_VR_Slice_Pack(SUMA_VolumeObject *VO,
                                    SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_GET_VR_Slice_Pack"};
   SUMA_VOL_SAUX *VSaux=NULL;
   SUMA_RENDERED_SLICE *rslc=NULL;
   SUMA_X_SurfCont *SurfCont = NULL;
   float *cen = NULL, Eq[4], *PlOff=NULL;
   int nn;
   int N_slc=150;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!VO || !(VSaux = SUMA_ADO_VSaux((SUMA_ALL_DO *)VO)) ||
       !(SurfCont = SUMA_ADO_Cont((SUMA_ALL_DO *)VO))) {
      SUMA_RETURN(NOPE);
   }

   if (  SurfCont->VR_fld->N_slice_num < 0 || 
         SurfCont->VR_fld->N_slice_num > 2000) {
      N_slc = 150;
   } else N_slc = (int) SurfCont->VR_fld->N_slice_num;
   
   cen = SUMA_VO_Grid_Center(VO, NULL);
   SUMA_ScreenPlane_WorldSpace(sv, cen, Eq);
   PlOff = (float *)SUMA_calloc(N_slc, sizeof(float));
   if (!PlOff || (SUMA_PlaneBoxSlice( sv->GVS[sv->StdView].ViewFrom, Eq, 
                                      VO->VE[0]->bcorners, NULL, NULL,
                                      PlOff, N_slc) < 0)) {
      SUMA_S_Err("Failed to allocate or get %d slicing planes", N_slc);
      SUMA_ifree(PlOff);
      SUMA_RETURN(NOPE);                       
   }
   for (nn=0; nn<N_slc; ++nn) {
      rslc = (SUMA_RENDERED_SLICE *) SUMA_malloc(sizeof(SUMA_RENDERED_SLICE));
      rslc->Eq[0] = Eq[0]; rslc->Eq[1] = Eq[1]; rslc->Eq[2] = Eq[2];
      rslc->Eq[3] = PlOff[nn] ; sprintf(rslc->variant,"Vr");
      /* stick plane in list, last one to be rendered goes to top */
      SUMA_LH("Intersecting VR plane %f %f %f %f, on vol %s\n"
              "(origin %f %f %f)",
               rslc->Eq[0], rslc->Eq[1], rslc->Eq[2], rslc->Eq[3],
               SUMA_VE_Headname(VO->VE,0),
               VO->VE[0]->I2X[3][0], 
               VO->VE[0]->I2X[3][1],VO->VE[0]->I2X[3][2]);
      dlist_ins_next(VSaux->vrslcl, dlist_head(VSaux->vrslcl), rslc);
   }
   SUMA_ifree(PlOff);
   SUMA_RETURN(YUP);
}

SUMA_VOL_REN_VARIANTS SUMA_SlcVariantToCode(char *variant)
{
   
   static char FuncName[]={"SUMA_SlcVariantToCode"};
   if (!variant) {
      SUMA_S_Err("NULL variant");
      return(SUMA_ERR_VARIANT);
   }
   if (!strcmp(variant,"Ax")) return(SUMA_AX_VARIANT);
   if (!strcmp(variant,"Sa")) return(SUMA_SA_VARIANT);
   if (!strcmp(variant,"Co")) return(SUMA_CO_VARIANT);
   if (!strcmp(variant,"Vr")) return(SUMA_VR_VARIANT);
   SUMA_S_Err("Variant >%s< not recognized", variant);
   return(SUMA_ERR_VARIANT);
}

void SUMA_SlcCodeToVariant(SUMA_VOL_REN_VARIANTS v, char *variant) 
{
   static char FuncName[]={"SUMA_SlcCodeToVariant"};
   SUMA_Boolean LocalHead = NOPE;
   
   variant[0] = '\0';
   switch (v) {
      default:
         SUMA_S_Err("Variant code %d unrecognized", v);
         SUMA_DUMP_TRACE("From here");
         variant[0] = '\0';
         break;
      case SUMA_ERR_VARIANT: /* Don't complain, happens if you remotely
                                select a voxel when no slice is selected
                                yet */
         SUMA_LH("Variant code %d is error flag", v);
         variant[0] = '\0';
         break;
      case SUMA_AX_VARIANT:
         variant[0] = 'A'; variant[1] = 'x'; variant[2] = '\0';
         break;
      case SUMA_SA_VARIANT:
         variant[0] = 'S'; variant[1] = 'a'; variant[2] = '\0';
         break;
      case SUMA_CO_VARIANT:
         variant[0] = 'C'; variant[1] = 'o'; variant[2] = '\0';
         break;
      case SUMA_VR_VARIANT:
         variant[0] = 'V'; variant[1] = 'r'; variant[2] = '\0';
         break;
   }
   return;
}       


SUMA_Boolean SUMA_Get_Slice_Pack(SUMA_VolumeObject *VO, 
                                 char *variant, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Get_Slice_Pack"};
   SUMA_VOL_SAUX *VSaux=NULL;
   int i, ii1, ii0, iis, dim, *dims=NULL;
   SUMA_RENDERED_SLICE *rslc=NULL;
   SUMA_SLICE_FIELD *slc;
   SUMA_X_SurfCont *SurfCont = NULL;
   GLfloat slc_cen[6], scr_cen[6];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!VO || !(VSaux = SUMA_ADO_VSaux((SUMA_ALL_DO *)VO)) || !variant ||
       !(SurfCont = SUMA_ADO_Cont((SUMA_ALL_DO *)VO))) {
      SUMA_RETURN(NOPE);
   }
   
   switch (variant[0]) {
      case 'A':
         slc = SurfCont->Ax_slc;
         break;
      case 'S':
         slc = SurfCont->Sa_slc;
         break;
      case 'C':
         slc = SurfCont->Co_slc;
         break;
      case 'V': /* Volume Rendering */
         SUMA_RETURN(SUMA_GET_VR_Slice_Pack(VO, sv));
         break;
      default:
         slc = NULL;
         SUMA_S_Err("Bad variant");
         SUMA_RETURN(NOPE);
   }
   
   /* check for proper ordering direction 
      (only needed if montage is in order ) */
   dim = SUMA_dset_tex_slice_corners_gui(VO->VE, 0, variant, 
                              0,  /* most inferior slice */
                              NULL, NULL, slc_cen, NULL, 0); 
   dim = SUMA_dset_tex_slice_corners_gui(VO->VE, 0, variant,
                              SUMA_VO_N_Slices(VO, variant)-1,/*superior++*/
                              NULL, NULL, slc_cen+3, NULL, 0); 
   /* which of the two is closer to my nose? */
   SUMA_World2ScreenCoordsF(sv, 2, slc_cen, scr_cen, NULL, NOPE, NOPE); 
   
   
   if (slc->mont_num > 1) {
      /* Form the slice planes. For now they are setup along the acquisition 
         directions of the 0th VO->VE, but they need not be.
         Texture generation can then be carried out on any of the VEs
         regardless of their acquisition.                                */
      ii0 = slc->slice_num - ((slc->mont_num-1.0)/2.0)*slc->mont_inc;
      if (scr_cen[dim] > scr_cen[3+dim]) {
         iis = slc->mont_inc;
      } else {
         ii0 += (slc->mont_num-1)*slc->mont_inc;
         iis = -slc->mont_inc;    
      }
      if (1) {
         i=0;
         while (i<slc->mont_num) {
            if (ii0 >= 0 && ii0 < SUMA_VO_N_Slices(VO, variant)) {
               SUMA_LH( "Top is closer bottom    %f %f %f --> scr z %f \n"
                        "              top       %f %f %f --> scr z %f \n", 
                        slc_cen[0], slc_cen[1], slc_cen[2], scr_cen[2], 
                        slc_cen[3], slc_cen[4], slc_cen[5], scr_cen[5]);
               rslc = (SUMA_RENDERED_SLICE *)
                              SUMA_malloc(sizeof(SUMA_RENDERED_SLICE));
               SUMA_dset_tex_slice_corners_gui(VO->VE, 0, variant, ii0,
                                               NULL, NULL, NULL, rslc->Eq, 0);
               rslc->slc_num = ii0; snprintf(rslc->variant, 15, "%s", variant);
               /* stick plane in list, last one rendered goes to top */
               SUMA_LH("Intersecting plane %f %f %f %f, on vol %s\n"
                       "(origin %f %f %f), variant %s",
                        rslc->Eq[0], rslc->Eq[1], rslc->Eq[2], rslc->Eq[3],
                        SUMA_VE_Headname(VO->VE,0),
                        VO->VE[0]->I2X[3][0], 
                        VO->VE[0]->I2X[3][1],VO->VE[0]->I2X[3][2],
                        rslc->variant);
               dlist_ins_prev(VSaux->slcl, dlist_head(VSaux->slcl), rslc);
            }
            ++i;
            ii0 += iis;
         }
      } else {
         ii0 = slc->slice_num + slc->mont_num/2*slc->mont_inc;
         i=0;
         while (i<slc->mont_num) {
            if (ii0 >= 0 && ii0 < SUMA_VO_N_Slices(VO, variant)) {
               SUMA_LH( "Bot is closer bottom    %f %f %f --> scr z %f \n"
                        "              top       %f %f %f --> scr z %f \n", 
                        slc_cen[0], slc_cen[1], slc_cen[2], scr_cen[2], 
                        slc_cen[3], slc_cen[4], slc_cen[5], scr_cen[5]);

               rslc = (SUMA_RENDERED_SLICE *)
                              SUMA_malloc(sizeof(SUMA_RENDERED_SLICE));
               SUMA_dset_tex_slice_corners_gui(VO->VE, 0, variant, ii0,
                                               NULL, NULL, NULL, rslc->Eq, 0);
               rslc->slc_num = ii0; snprintf(rslc->variant, 15, "%s", variant);
               dlist_ins_prev(VSaux->slcl, dlist_head(VSaux->slcl), rslc);
            }
            ++i;
            ii0 -= slc->mont_inc;
         }
      }
   } else {
      SUMA_LH("Single slice %f %s", slc->slice_num, variant);
      rslc = (SUMA_RENDERED_SLICE *)
                           SUMA_malloc(sizeof(SUMA_RENDERED_SLICE));
      SUMA_dset_tex_slice_corners_gui(VO->VE, 0, variant, (int)slc->slice_num,
                                      NULL, NULL, NULL, rslc->Eq, 0);
      rslc->slc_num = (int)slc->slice_num; 
      snprintf(rslc->variant, 15, "%s", variant);
      dlist_ins_prev(VSaux->slcl, dlist_head(VSaux->slcl), rslc);
      
   }
   SUMA_RETURN(YUP);
}

int SUMA_Count_All_VO_Textures(void)
{
   static char FuncName[]={"SUMA_Count_All_VO_Textures"};
   int i, j, c = 0;
   SUMA_ALL_DO *ado=NULL;
   
   for (i=0; i<SUMAg_N_DOv; ++i) {
      ado = iDO_ADO(i);
      if (ado->do_type == VO_type) {
         j=0;
         SUMA_VolumeObject *VO = (SUMA_VolumeObject *)ado;
         while (VO->VE && VO->VE[j]) {
            ++c;
            ++j;
         }
      }
   }
   return(c);
}

/*
   Check if texture has been loaded for a particular viewer
   NOTE: N_tex is only set is the texture was NOT loaded */
SUMA_Boolean SUMA_SV_isTextureLoaded(SUMA_SurfaceViewer *sv, 
                                     GLuint texName, int *N_tex)
{
   static char FuncName[]={"SUMA_SV_isTextureLoaded"};
   int i=0;
   
   while (i<SUMA_MAX_DISPLAYABLE_OBJECTS && sv->LoadedTextures[i]!=-1) {
      if (sv->LoadedTextures[i] = (int)texName) return(YUP);
      ++i;
   }
   if (i == SUMA_MAX_DISPLAYABLE_OBJECTS && sv->LoadedTextures[i]!=-1) {
      SUMA_S_Warn("Looks like LoadedTextures is not plugged");
   }
   return(NOPE);
}

SUMA_Boolean SUMA_SV_Mark_Textures_Status(SUMA_SurfaceViewer *sv, char *MarkAs,
                                          SUMA_VolumeObject *VO, int j, 
                                          int loadifneeded)
{
   static char FuncName[]={"SUMA_SV_Mark_Textures_Status"};
   int N_tex = 0, i=0;
   
   SUMA_ENTRY;
   
   if (!sv || !MarkAs) {
      SUMA_RETURN(NOPE);
   }
   if (!strcmp(MarkAs, "unloaded_all")) {
      sv->LoadedTextures[0]=-1;
      SUMA_RETURN(YUP);
   } else if (!strcmp(MarkAs, "loaded_for_VO")) {
      if (!VO) SUMA_RETURN(NOPE);
      j = 0;
      while (VO->VE && VO->VE[j]) {
         if (!SUMA_SV_isTextureLoaded(sv, VO->VE[j]->texName[0], &N_tex)) {
            sv->LoadedTextures[N_tex] = VO->VE[j]->texName[0];
            sv->LoadedTextures[N_tex+1] = -1;
            if (loadifneeded) {
               SUMA_VE_LoadTexture(VO->VE, j);
            }
         }
         ++j;
      }
      SUMA_RETURN(YUP);
   } else if (!strcmp(MarkAs, "loaded_for_VO_one")) {
      if (!VO || j < 0 || !VO->VE || !VO->VE[j]) SUMA_RETURN(NOPE);
      if (!SUMA_SV_isTextureLoaded(sv, VO->VE[j]->texName[0], &N_tex)) {
         sv->LoadedTextures[N_tex] = VO->VE[j]->texName[0];
         sv->LoadedTextures[N_tex+1] = -1;
         if (loadifneeded) {
            SUMA_VE_LoadTexture(VO->VE, j);
         }
      }
      SUMA_RETURN(YUP);
   } else if (!strcmp(MarkAs, "loaded_all")) {
      SUMA_ALL_DO *ado=NULL;
      N_tex = 0;
      sv->LoadedTextures[N_tex]=-1;
      for (i=0; i<SUMAg_N_DOv; ++i) {
         ado = iDO_ADO(i);
         if (ado->do_type == VO_type) {
            VO = (SUMA_VolumeObject *)ado;
            j=0;
            while (VO->VE && VO->VE[j]) {
               sv->LoadedTextures[N_tex] = VO->VE[j]->texName[0]; 
               if (loadifneeded) {
                  SUMA_VE_LoadTexture(VO->VE, j);
               }
               ++N_tex;
               ++j;
            }
         }
      }
      sv->LoadedTextures[N_tex]=-1;
      SUMA_RETURN(YUP);
   } else {
      SUMA_S_Err("MarkAs %s not understood", MarkAs);
      SUMA_RETURN(NOPE);
   }
}

SUMA_Boolean SUMA_Draw_CIFTI_DO(SUMA_CIFTI_DO *CO, 
                               SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Draw_CIFTI_DO"};
   int i, pp=0;
   SUMA_ALL_DO *asdo=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("We are not rendering CIFTI DOs directly anymore.\n"
      	   "Delete once we're sure we won't go down this route.\n"
	   "CIFTI DOs are rendered by rendering their elementary\n"
	   "DOs. And if you do go down that route, safer to refer\n"
	   "to DOs by their IDs");
	          
   SUMA_RETURN(YUP);
   
   for (i=0; i<CO->N_subdoms; ++i) {
      if (CO->subdoms_id[i]) {
      asdo = SUMA_CIFTI_subdom_ado(CO, i);
      switch (asdo->do_type) {
      	 case SO_type:
	    if (!pp) SUMA_DrawMesh((SUMA_SurfaceObject *)asdo, sv);
	    ++pp;
	    break;
	 case VO_type:
	    SUMA_DrawVolumeDO((SUMA_VolumeObject*)asdo, sv);
	    break;
	 default:
	    SUMA_S_Err("Not ready for subdom %d", asdo->do_type);
	    break;      
      }
      }
   }
   
   
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_DrawVolumeDO(SUMA_VolumeObject *VO, 
                               SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawVolumeDO"};
   SUMA_ENTRY;
   if (!SUMA_DrawVolumeDO_slices(VO,sv)) {
      SUMA_S_Err("Failed to draw slices");
      SUMA_RETURN(NOPE);
   }
   if (!SUMA_DrawVolumeDO_3D(VO, sv)) {
      SUMA_S_Err("Failed to render volume");
      SUMA_RETURN(NOPE);
   }
   SUMA_RETURN(YUP);
}


/* Draw Volume Data, in slice mode for now */
SUMA_Boolean SUMA_DrawVolumeDO_slices(SUMA_VolumeObject *VO, 
                                    SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawVolumeDO_slices"};
   int i = 0, k = 0, j=0, ive=0;
   float iq[4]={0, 0, 0, 0}, vo0[3], voN[3];
   static int ipass=0, iplane = 0;
   SUMA_RENDERED_SLICE *rslc=NULL;
   GLfloat tex_corn[18] ;
   GLfloat slc_corn[18] ;
   GLfloat rotationMatrix[4][4], rt[4][4];
   GLboolean gl_dt, gl_bl, gl_at;
   int ShowUnselected = 1, shmodel, nqd, ivelast;
   float tz = 0.0, I[3];
   static GLfloat init_rotationMatrix[4][4];
   static GLdouble dmatrix[16], init_mv_matrix[16];
   DListElmt *el=NULL;
   DList *st=NULL;
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)VO;
   SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
   SUMA_OVERLAYS *colp = NULL;
   SUMA_DSET *dset=NULL;
   float* nlt; /*JB: temporary node list, because I do not want to type 
                  "VO->SOcut[0]->NodeList" over and over...*/
   SUMA_Boolean LastTextureOnCutPlane=YUP;
   SUMA_ATRANS_MODES trmode=SATM_ViewerDefault;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!VO) SUMA_RETURN(NOPE);
   if (!sv) sv = &(SUMAg_SVv[0]);
   if (!(colp = SUMA_ADO_CurColPlane(ado))) {
      SUMA_S_Err("Need colp here");
      SUMA_RETURN(NOPE);
   }
   if (sv->DO_PickMode) {
      SUMA_LH("No need to draw volume in DO_PickMode");
      SUMA_RETURN(YUP);
   }
   
   if (!VO->Show) SUMA_RETURN(YUP);
   
   if (!VO->SOcut || !VO->SOcut[0]) SUMA_VO_InitCutPlanes(VO);
      
   if (!VO->VE || !VO->VE[0]) { 
      SUMA_S_Err("No elements?");
      SUMA_RETURN(NOPE);
   }

   /* setup slices per VO's standards, dictated by the first VE */
   dset = SUMA_VO_dset(VO);
   if (!(dset = SUMA_VO_dset(VO))) {
      SUMA_S_Err("No dset?");
      SUMA_RETURN(NOPE);
   } 

   /* make sure all textures are ready */
   ive = 0;   
   while (VO->VE && VO->VE[ive]) {
      if (!VO->VE[ive]->texName) {
         if (!SUMA_CreateGL3DTexture(VO)) {
            SUMA_S_Err("Failed to create texture");
            SUMA_RETURN(NOPE);
         }
         if (!SUMA_SV_Mark_Textures_Status(sv, "loaded_for_VO_one", VO, ive,0)) {
            SUMA_S_Err("Failed to mark texture as loaded");
            SUMA_RETURN(NOPE);
         }
      } else {
         /* Need to check if textures need to be reloaded.
         The actual loading needs to be redone only when a viewer has been
         closed and then open again. Not sure why that must be done but without
         a new load the textures go away after the viewer is reopened */
         if (!SUMA_SV_Mark_Textures_Status(sv, "loaded_for_VO_one", VO, ive, 1)){
            SUMA_S_Err("Failed to check or reload texture");
            SUMA_RETURN(NOPE);
         }
      }
      ++ive;
   }

   
   if (!SUMA_GLStateTrack( "new", &st, FuncName, NULL, NULL)) {
      SUMA_S_Err("Failed to create tracking list");
      SUMA_RETURN(NOPE); 
   }
   
   if (sv->PolyMode != SRM_Fill) {
      /* fill it up */
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);   
   }
   
   /* Now we need to draw the sucker */
   SUMA_CHECK_GL_ERROR("OpenGL Error pre texture");
   glEnable(GL_TEXTURE_3D);
   if (!(gl_at = glIsEnabled(GL_ALPHA_TEST))) glEnable(GL_ALPHA_TEST);   
   if (colp->AlphaThresh == 0.0f) glAlphaFunc(GL_ALWAYS, colp->AlphaThresh);
   else glAlphaFunc(GL_GREATER, colp->AlphaThresh);
                              /* Thresholded voxels alphas are set to 0 */
   glTexEnvf(  GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, 
               VO->TexEnvMode   ); /* what happens if 
                              there is color already on a vertex (I would likely
                              not need this for 3D textures...*/
   
   gl_dt = glIsEnabled(GL_DEPTH_TEST);
   gl_bl = glIsEnabled(GL_BLEND);  
   
   if (!(gl_dt)) glEnable(GL_DEPTH_TEST);      
   
   glGetIntegerv(GL_SHADE_MODEL, &shmodel);
   if (shmodel != GL_FLAT) 
      glShadeModel(GL_FLAT);
      
   trmode = VSaux->TransMode;
   if (trmode == SATM_ViewerDefault) {
      if ((trmode = SUMA_TransMode2ATransMode(sv->TransMode)) 
            <= SATM_ViewerDefault || trmode >= SATM_N_TransModes) {
         SUMA_S_Warn("Bad trans mode change from %d to %d", 
                      sv->TransMode,trmode);
         trmode =  SATM_0;     
      }
   }
   if (trmode <= SATM_ViewerDefault || trmode > SATM_N_TransModes) {
      SUMA_S_Warn("Bad trmode %d", trmode);
      trmode =  SATM_0;
   }
   if (trmode == SATM_ALPHA) {
      /* Setup blending options */
      /* See note below, but might be OK with few slices */
      if ((!gl_bl)) glEnable(GL_BLEND);
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   } else {
      /* The safe way. Can't blend properly when showing 
         slices, particularly stacks and
         multiple planes. Becomes a royal pain 
         to render all in proper order*/
      if ((gl_bl)) glDisable(GL_BLEND);
      if (trmode > SATM_0) {
         SUMA_LHv("ATrans Mode %d\n", trmode );
         SUMA_SET_GL_TRANS_MODE(SUMA_ATransMode2TransMode(trmode), st, -1);
      }
   }
   /* empty list of rendered slices */
   while ((el = dlist_head(VSaux->slcl))) {
      dlist_remove(VSaux->slcl, el, (void **)&rslc);
      SUMA_Free_SliceListDatum((void *)rslc);
   }

   if (VSaux->ShowSaSlc && !SUMA_Get_Slice_Pack(VO, "Sa", sv)) {
      SUMA_S_Err("Failed to get Ax slice pack");
   }
   
   if (VSaux->ShowAxSlc && !SUMA_Get_Slice_Pack(VO, "Ax", sv)) {
      SUMA_S_Err("Failed to get Ax slice pack");
   }
   
   
   if (VSaux->ShowCoSlc && !SUMA_Get_Slice_Pack(VO, "Co", sv)) {
      SUMA_S_Err("Failed to get Ax slice pack");
   }
   
   SUMA_LH("Have %d slices to render", dlist_size(VSaux->slcl));
   SUMA_CHECK_GL_ERROR("OpenGL Error pre setup");
      
      
   /* Generate the textures for all VEs. Note, no blending of textures
      is done at the moment.                                          
      If you have multiple VEs, you will need to render one slice at a 
      time from each of the volumes. Blend them in the accumulate buffer
      and then return. Textures will need to be bound/unbound for each
      VE, each slice. Clunky but I won't bother with it until it proves
      too slow.*/
   if (SUMA_VO_NumVE(VO) > 1) {
      SUMA_S_Warn("Not ready to deal with multiple textures quite yet");
   }

   SUMA_LH("Have %d slices", dlist_size(VSaux->slcl));
   ivelast=-1;
   if ((el = dlist_tail(VSaux->slcl))) {
      do {
         rslc = (SUMA_RENDERED_SLICE *)el->data;
         
         ive = 0;   
         while (VO->VE && VO->VE[ive]) {
            if (ive != ivelast) {
               glBindTexture(GL_TEXTURE_3D, VO->VE[ive]->texName[0]); 
               ivelast = ive;
            }                                 /* make texName be current */
            /* compute plane intersection with VE[ive] */
            if ((nqd = SUMA_PlaneBoxIntersect( sv->GVS[sv->StdView].ViewFrom, 
                                             rslc->Eq, VO->VE[ive]->bcorners, 
                                             slc_corn)) > 2) {
               SUMA_LH("Have plane %f %f %f %f, %d pts on vol %s",
                           rslc->Eq[0], rslc->Eq[1], rslc->Eq[2], rslc->Eq[3],
                           nqd,  SUMA_VE_Headname(VO->VE,ive));
               glBegin(GL_POLYGON);
                  for (k=0; k<6; ++k) { /* draw all 6 points always, even 
                                           when there are repetitions. 
                                           Don't bother trimming to unique
                                           set unless this causes trouble */
                     /* change mm (edge coordinate to texture coords) */
                     AFF44_MULT_I(tex_corn, VO->VE[ive]->X2I, (slc_corn+3*k));
                     /* offset indices because slc_corn is on edge */
                     if (tex_corn[0] < 0) tex_corn[0] = 0;
                     else if (tex_corn[0] > VO->VE[ive]->Ni-1) 
                        tex_corn[0] = VO->VE[ive]->Ni-1;
                     if (tex_corn[1] < 0) tex_corn[1] = 0;
                     else if (tex_corn[1] > VO->VE[ive]->Nj-1) 
                        tex_corn[1] = VO->VE[ive]->Nj-1;
                     if (tex_corn[2] < 0) tex_corn[2] = 0;
                     else if (tex_corn[2] > VO->VE[ive]->Nk-1) 
                        tex_corn[2] = VO->VE[ive]->Nk-1;
                     tex_corn[0] /= (float)(VO->VE[ive]->Ni-1);
                     tex_corn[1] /= (float)(VO->VE[ive]->Nj-1);
                     tex_corn[2] /= (float)(VO->VE[ive]->Nk-1);
                     glTexCoord3f(tex_corn[0], 
                                  tex_corn[1], tex_corn[2]);
                           /* this one is affected by the Texture MatrixMode */
                     glVertex3f(slc_corn[3*k], 
                                slc_corn[3*k+1], slc_corn[3*k+2]); 
                           /* this one is affected by the Modelview matrixMode*/
                  }
               glEnd();                            
            }
            if (ive > 0) {
               SUMA_S_Warn("Add blending here");
               /* Here is where you blend slice from this VE with the previous 
                  This should work just fine as is actually, no need to blend
                  separately unless doing overlay on top always. In that case,
                  render each VE separately then blend results across VEs
               */
            }
            ++ive;
         }
            
         if (el != dlist_head(VSaux->slcl)) el = dlist_prev(el);
         else el = NULL;
      } while (el);
   }
   SUMA_CHECK_GL_ERROR("OpenGL Error ddd");
   
   glFlush();
   if (!gl_at) glDisable(GL_ALPHA_TEST); 
   
   glDisable(GL_TEXTURE_3D);
   
   if (shmodel != GL_FLAT) glShadeModel(shmodel);
   
   if (sv->PolyMode != SRM_Fill) {/* set fill mode back */
      SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
   }

   if (gl_dt) glEnable(GL_DEPTH_TEST);
   else glDisable(GL_DEPTH_TEST);
   if (gl_bl) glEnable(GL_BLEND);
   else glDisable(GL_BLEND);

   /* Now for the highlight */
   if (SUMA_SV_GetShowSelectedFaceSet(sv)) { 
      int selslice = -1;
      float nlt[12];
      char variant[8]={"notset"};
      GLfloat No_Color[4] = { 0.0, 0.0, 0.0, 0.0 };
      selslice = SUMA_VO_SelectedSlice(VO, variant, nlt);
      if (selslice >= 0 && variant[0] != 'V') {
         SUMA_LH("Drawing %s Slice %d Selection Contour\n"
                 "%f %f %f --> %f %f %f ...\n", 
                 variant, selslice,
                 nlt[0],nlt[1],nlt[2], nlt[3],nlt[4],nlt[5]);    
         glMaterialfv(GL_FRONT, GL_AMBIENT_AND_DIFFUSE, No_Color);             
         glColorMaterial(GL_FRONT, GL_EMISSION); 
         glEnable(GL_COLOR_MATERIAL);
         glColor4f(0.25, 0.25, 0.25, 1.0);
         glBegin(GL_LINE_LOOP);
            glVertex3f( nlt[0],nlt[1],nlt[2] );
            glVertex3f( nlt[3],nlt[4],nlt[5] );
            glVertex3f( nlt[6],nlt[7],nlt[8] );
            glVertex3f( nlt[9],nlt[10],nlt[11] );
         glEnd();
         glColor4f(0, 0, 0, 0); /* Kill emission,        ZSS. March 2014 
                        or risk hurting functions that expect it to be off. */
         glDisable(GL_COLOR_MATERIAL);
      } else {
         SUMA_LH("Either no selection or failed to find slice, "
                 "or VR intersection (variant=%s)", variant);
      }
   } else {
      SUMA_LH("Do not show selected faceset");
   }
   
   SUMA_LH("Undoing state changes, should fold ones above in here someday");
   SUMA_GLStateTrack("r", &st, FuncName, NULL, NULL); 

   SUMA_RETURN(YUP);
}


/* Draw Volume Data, in 3D this time */
SUMA_Boolean SUMA_DrawVolumeDO_3D(SUMA_VolumeObject *VO, 
                                    SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawVolumeDO_3D"};
   int i = 0, k = 0, j=0, ive=0;
   float iq[4]={0, 0, 0, 0}, vo0[3], voN[3];
   static int ipass=0, iplane = 0;
   SUMA_RENDERED_SLICE *rslc=NULL;
   GLfloat tex_corn[18] ;
   GLfloat slc_corn[18] ;
   GLfloat rotationMatrix[4][4], rt[4][4];
   GLboolean gl_dt, gl_bl, gl_at;
   int ShowUnselected = 1, shmodel, nqd, ivelast;
   DListElmt *el=NULL;
   DList *st=NULL;
   float tz = 0.0, I[3];
   static GLfloat init_rotationMatrix[4][4];
   static GLdouble dmatrix[16], init_mv_matrix[16];
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)VO;
   SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
   SUMA_OVERLAYS *colp = NULL;
   SUMA_DSET *dset=NULL;
   float* nlt; /*JB: temporary node list, because I do not want to type 
                  "VO->SOcut[0]->NodeList" over and over...*/
   SUMA_Boolean LastTextureOnCutPlane=YUP;
   SUMA_ATRANS_MODES trmode=SATM_ViewerDefault;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!VO) SUMA_RETURN(NOPE);
   if (!sv) sv = &(SUMAg_SVv[0]);
   if (!(colp = SUMA_ADO_CurColPlane(ado))) {
      SUMA_S_Err("Need colp here");
      SUMA_RETURN(NOPE);
   }
   if (sv->DO_PickMode) {
      SUMA_LH("No need to draw volume in DO_PickMode");
      SUMA_RETURN(YUP);
   }
   
   if (!VO->Show) SUMA_RETURN(YUP);

   if (!VSaux->ShowVrSlc) SUMA_RETURN(YUP);
      
   if (!SUMA_GLStateTrack( "new", &st, FuncName, NULL, NULL)) {
      SUMA_S_Err("Failed to create tracking list");
      SUMA_RETURN(NOPE); 
   }
   

   if (sv->PolyMode != SRM_Fill) {
      /* fill it up */
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);   
   }
   
   if (!VO->SOcut || !VO->SOcut[0]) SUMA_VO_InitCutPlanes(VO);
      
   /* Now we need to draw the sucker */
   SUMA_CHECK_GL_ERROR("OpenGL Error pre texture");
   glEnable(GL_TEXTURE_3D);
   /* You need a separate control for this ALPHA_TEST, perhaps.
      Enabling it, even at 0.1 threshold, causes ugly slice striping
      artifacts. It is possible we might need it for something later on...*/
   if (!(gl_at = glIsEnabled(GL_ALPHA_TEST))) glEnable(GL_ALPHA_TEST);   
   #if 0
   if (colp->AlphaThresh == 0.0f) glAlphaFunc(GL_ALWAYS, colp->AlphaThresh);
   else glAlphaFunc(GL_GREATER, colp->AlphaThresh);
                              /* Thresholded voxels alphas are set to 0 */
   #else
   glAlphaFunc(GL_GREATER, 0.01); /* Might want to add a separate AlphaThresh
                                     for this. Usually you want it very small,
                                     or else you'll get ugly striping artifacts.
                                     But if you use the slice's alpha threshold,
                                     you'll need to reduce it low enough to make 
                                     the volume look nice but that give the 
                                     slices an ugly rim. */
   #endif
   glTexEnvf(  GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, 
               VO->TexEnvMode   ); /* what happens if 
                              there is color already on a vertex (I would likely
                              not need this for 3D textures...*/
   
   gl_dt = glIsEnabled(GL_DEPTH_TEST);
   gl_bl = glIsEnabled(GL_BLEND);  
   
   if (!(gl_dt)) glEnable(GL_DEPTH_TEST);      
   if (!(gl_bl)) glEnable(GL_BLEND);

   { /* Transparency games */
   /* Beware of the trickery of transparency.
   For cheese cloth transp, rendering multiple objects in a transparent
   fashion is tricky. 
   The bitmask autoshifting is not enough when many objects are in use.
   The stricking case is when you have two surfs and a volume, at 50% transp.
   If you shift for each object, then the first surface will get obscured by
   the volume (third object) because the twice shifted pattern will overlap
   with that of the first surf. One solution, now implemented, is to use the same
   mask shifting per class of objects. But then surfs won't be transparent
   between them (not a big deal). The other option is to set the transparency 
   differently for different objects (i.e. not using the viewer-wide 
   transparency). Other options might help under certain cases (see 
   comment for function SUMA_StippleMask_shift().     ZSS March 13 2014    */
   trmode = VSaux->TransMode;
   if (trmode == SATM_ViewerDefault) {
      if ((trmode = SUMA_TransMode2ATransMode(sv->TransMode)) 
            <= SATM_ViewerDefault || trmode >= SATM_N_TransModes) {
         SUMA_S_Warn("Bad trans mode change from %d to %d", 
                      sv->TransMode,trmode);
         trmode =  SATM_0;     
      }
   }
   if (trmode <= SATM_ViewerDefault || trmode > SATM_N_TransModes) {
      SUMA_S_Warn("Bad trmode %d", trmode);
      trmode =  SATM_0;
   }
   if (trmode == SATM_ALPHA) {
      /* Setup blending options, override initial setting above */
      glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   } else {
      /* The safe way. */
      if (trmode > SATM_0) {
         SUMA_LHv("ATrans Mode %d\n", trmode );
         SUMA_SET_GL_TRANS_MODE(SUMA_ATransMode2TransMode(trmode), st, VO_type);
      }
   }
   }

   if (!VO->VE || !VO->VE[0]) { 
      SUMA_S_Err("No elements?");
      SUMA_RETURN(NOPE);
   }

   /* setup slices per VO's standards, dictated by the first VE */
   dset = SUMA_VO_dset(VO);
   if (!(dset = SUMA_VO_dset(VO))) {
      SUMA_S_Err("No dset?");
      SUMA_RETURN(NOPE);
   } 

   /* make sure all textures are ready */
   ive = 0;   
   while (VO->VE && VO->VE[ive]) {
      if (!VO->VE[ive]->texName) {
         if (!SUMA_CreateGL3DTexture(VO)) {
            SUMA_S_Err("Failed to create texture");
            SUMA_RETURN(NOPE);
         }
      }
      ++ive;
   }
   /* empty list of rendered slices */
   while ((el = dlist_head(VSaux->vrslcl))) {
      dlist_remove(VSaux->vrslcl, el, (void **)&rslc);
      SUMA_Free_SliceListDatum((void *)rslc);
   }

   /* Now create list of 3D slices */

   if (VSaux->ShowVrSlc && !SUMA_Get_Slice_Pack(VO, "Vr", sv)) {
      SUMA_S_Err("Failed to get VR slice pack");
   }
   
   
   SUMA_LH("Have %d slices to render", dlist_size(VSaux->vrslcl));
   /* Setup blending options */
      glGetIntegerv(GL_SHADE_MODEL, &shmodel);
      if (shmodel != GL_FLAT) 
         glShadeModel(GL_FLAT);
	   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      SUMA_CHECK_GL_ERROR("OpenGL Error pre setup");
      
      
   /* Generate the textures for all VEs. Note, no blending of textures
      is done at the moment.                                          
      If you have multiple VEs, you will need to render one slice at a 
      time from each of the volumes. Blend them in the accumulate buffer
      and then return. Textures will need to be bound/unbound for each
      VE, each slice. Clunky but I won't bother with it until it proves
      too slow.*/
   if (SUMA_VO_NumVE(VO) > 1) {
      SUMA_S_Warn("Not ready to deal with multiple textures quite yet");
   }

   SUMA_LH("Have %d slices", dlist_size(VSaux->vrslcl));
   ivelast=-1;
   if ((el = dlist_tail(VSaux->vrslcl))) {
      do {
         rslc = (SUMA_RENDERED_SLICE *)el->data;
         
         ive = 0;   
         while (VO->VE && VO->VE[ive]) {
            if (ive != ivelast) {
               glBindTexture(GL_TEXTURE_3D, VO->VE[ive]->texName[0]); 
               ivelast = ive;
            }                                 /* make texName be current */
            /* compute plane intersection with VE[ive] */
            if ((nqd = SUMA_PlaneBoxIntersect( sv->GVS[sv->StdView].ViewFrom, 
                                             rslc->Eq, VO->VE[ive]->bcorners, 
                                             slc_corn)) > 2) {
               SUMA_LH("Have plane %f %f %f %f, %d pts on vol %s",
                           rslc->Eq[0], rslc->Eq[1], rslc->Eq[2], rslc->Eq[3],
                           nqd,  SUMA_VE_Headname(VO->VE,ive));
               glBegin(GL_POLYGON);
                  for (k=0; k<6; ++k) { /* draw all 6 points always, even 
                                           when there are repetitions. 
                                           Don't bother trimming to unique
                                           set unless this causes trouble */
                     /* change mm (edge coordinate to texture coords) */
                     AFF44_MULT_I(tex_corn, VO->VE[ive]->X2I, (slc_corn+3*k));
                     /* offset indices because slc_corn is on edge */
                     if (tex_corn[0] < 0) tex_corn[0] = 0;
                     else if (tex_corn[0] > VO->VE[ive]->Ni-1) 
                        tex_corn[0] = VO->VE[ive]->Ni-1;
                     if (tex_corn[1] < 0) tex_corn[1] = 0;
                     else if (tex_corn[1] > VO->VE[ive]->Nj-1) 
                        tex_corn[1] = VO->VE[ive]->Nj-1;
                     if (tex_corn[2] < 0) tex_corn[2] = 0;
                     else if (tex_corn[2] > VO->VE[ive]->Nk-1) 
                        tex_corn[2] = VO->VE[ive]->Nk-1;
                     tex_corn[0] /= (float)(VO->VE[ive]->Ni-1);
                     tex_corn[1] /= (float)(VO->VE[ive]->Nj-1);
                     tex_corn[2] /= (float)(VO->VE[ive]->Nk-1);
                     glTexCoord3f(tex_corn[0], 
                                  tex_corn[1], tex_corn[2]);
                           /* this one is affected by the Texture MatrixMode */
                     glVertex3f(slc_corn[3*k], 
                                slc_corn[3*k+1], slc_corn[3*k+2]); 
                           /* this one is affected by the Modelview matrixMode*/
                  }
               glEnd();                                 
            }
            if (ive > 0) {
               SUMA_S_Warn("Add blending here");
               /* Here is where you blend slice from this VE with the previous 
                  This should work just fine as is actually, no need to blend
                  separately unless doing overlay on top always. In that case,
                  render each VE separately then blend results across VEs
               */
            }
            ++ive;
         }
            
         if (el != dlist_head(VSaux->vrslcl)) el = dlist_prev(el);
         else el = NULL;
      } while (el);
   }
   SUMA_CHECK_GL_ERROR("OpenGL Error ddd");
   
   glFlush();
   if (!gl_at) glDisable(GL_ALPHA_TEST); 
   
   glDisable(GL_TEXTURE_3D);
   
   if (shmodel != GL_FLAT) glShadeModel(shmodel);
   
   if (sv->PolyMode != SRM_Fill) {/* set fill mode back */
      SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
   }

   if (gl_dt) glEnable(GL_DEPTH_TEST);
   else glDisable(GL_DEPTH_TEST);
   if (gl_bl) glEnable(GL_BLEND);
   else glDisable(GL_BLEND);
   
   /* This method should eventually replace the individual tests done above...*/
   SUMA_LH("Undoing state changes, should fold ones above in here someday");
   SUMA_GLStateTrack("r", &st, FuncName, NULL, NULL); 

   SUMA_RETURN(YUP);
}

/* Draw Volume Data, exp version */
SUMA_Boolean SUMA_DrawVolumeDO_exp(SUMA_VolumeObject *VO, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawVolumeDO_exp"};
   int i = 0, k = 0, j=0, ive=0;
   float iq[4]={0, 0, 0, 0}, vo0[3], voN[3];
   static int ipass=0, iplane = 0;
   SUMA_RENDERED_SLICE *rslc=NULL;
   GLfloat tex_corn[18] ;
   GLfloat slc_corn[18] ;
   GLfloat rotationMatrix[4][4], rt[4][4];
   GLboolean gl_dt, gl_bl;
   int ShowUnselected = 1, shmodel, nqd, ivelast;
   float tz = 0.0, I[3];
   static GLfloat init_rotationMatrix[4][4];
   static GLdouble dmatrix[16], init_mv_matrix[16];
   DListElmt *el=NULL;
   SUMA_ALL_DO *ado = (SUMA_ALL_DO *)VO;
   SUMA_VOL_SAUX *VSaux = SUMA_ADO_VSaux(ado);
   SUMA_DSET *dset=NULL;
   float* nlt; /*JB: temporary node list, because I do not want to type 
                  "VO->SOcut[0]->NodeList" over and over...*/
   SUMA_Boolean LastTextureOnCutPlane=YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   #ifndef GL_VERSION_2_0
      /* GL must be old */
      SUMA_S_Err("Open GL < 2.0, glWindowPos2s() not yet supported (on all machines)");
      SUMA_RETURN(NOPE);
   #else
   if (!VO) SUMA_RETURN(NOPE);
   if (!sv) sv = &(SUMAg_SVv[0]);
   
   if (sv->DO_PickMode) {
      SUMA_LH("No need to draw volume in DO_PickMode");
      SUMA_RETURN(YUP);
   }
   
   if (!VO->Show) SUMA_RETURN(YUP);
   
   if (sv->PolyMode != SRM_Fill) {
      /* fill it up */
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);   
   }
   
   if (!VO->SOcut || !VO->SOcut[0]) SUMA_VO_InitCutPlanes(VO);
      
   /* Now we need to draw the sucker */
   SUMA_CHECK_GL_ERROR("OpenGL Error pre texture");
   glEnable(GL_TEXTURE_3D);

   glTexEnvf(  GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, 
               VO->TexEnvMode   ); /* what happens if 
                              there is color already on a vertex (I would likely
                              not need this for 3D textures...*/
   
   gl_dt = glIsEnabled(GL_DEPTH_TEST);
   gl_bl = glIsEnabled(GL_BLEND);  
   
   if (!(gl_dt)) glEnable(GL_DEPTH_TEST);      
   if (!(gl_bl)) glEnable(GL_BLEND);

   if (!VO->VE || !VO->VE[0]) { 
      SUMA_S_Err("No elements?");
      SUMA_RETURN(NOPE);
   }

   /* setup slices per VO's standards, dictated by the first VE */
   dset = SUMA_VO_dset(VO);
   if (!(dset = SUMA_VO_dset(VO))) {
      SUMA_S_Err("No dset?");
      SUMA_RETURN(NOPE);
   } 

   /* make sure all textures are ready */
   ive = 0;   
   while (VO->VE && VO->VE[ive]) {
      if (!VO->VE[ive]->texName) {
         if (!SUMA_CreateGL3DTexture(VO)) {
            SUMA_S_Err("Failed to create texture");
            SUMA_RETURN(NOPE);
         }
      }
      ++ive;
   }
   /* empty list of rendered slices */
   while ((el = dlist_head(VSaux->slcl))) {
      dlist_remove(VSaux->slcl, el, (void **)&rslc);
      SUMA_Free_SliceListDatum((void *)rslc);
   }

   if (VSaux->ShowSaSlc && !SUMA_Get_Slice_Pack(VO, "Sa", sv)) {
      SUMA_S_Err("Failed to get Ax slice pack");
   }
   
   if (VSaux->ShowAxSlc && !SUMA_Get_Slice_Pack(VO, "Ax", sv)) {
      SUMA_S_Err("Failed to get Ax slice pack");
   }
   
   
   if (VSaux->ShowCoSlc && !SUMA_Get_Slice_Pack(VO, "Co", sv)) {
      SUMA_S_Err("Failed to get Ax slice pack");
   }
   
   SUMA_LH("Have %d slices to render", dlist_size(VSaux->slcl));
   /* Setup blending options */
      glGetIntegerv(GL_SHADE_MODEL, &shmodel);
      if (shmodel != GL_FLAT) 
         glShadeModel(GL_FLAT);
	   glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
      SUMA_CHECK_GL_ERROR("OpenGL Error pre setup");
      
      
   /* Generate the textures for all VEs. Note, no blending of textures
      is done at the moment.                                          
      If you have multiple VEs, you will need to render one slice at a 
      time from each of the volumes. Blend them in the accumulate buffer
      and then return. Textures will need to be bound/unbound for each
      VE, each slice. Clunky but I won't bother with it until it proves
      too slow.*/
   if (SUMA_VO_NumVE(VO) > 1) {
      SUMA_S_Warn("Not ready to deal with multiple textures quite yet");
   }

   SUMA_LH("Have %d slices", dlist_size(VSaux->slcl));
   ivelast=-1;
   if ((el = dlist_tail(VSaux->slcl))) {
      do {
         rslc = (SUMA_RENDERED_SLICE *)el->data;
         
         ive = 0;   
         while (VO->VE && VO->VE[ive]) {
            if (ive != ivelast) {
               glBindTexture(GL_TEXTURE_3D, VO->VE[ive]->texName[0]); 
               ivelast = ive;
            }                                 /* make texName be current */
            /* compute plane intersection with VE[ive] */
            if ((nqd = SUMA_PlaneBoxIntersect( sv->GVS[sv->StdView].ViewFrom, 
                                             rslc->Eq, VO->VE[ive]->bcorners, 
                                             slc_corn)) > 2) {
               SUMA_LH("Have plane %f %f %f %f, %d pts on vol %s",
                           rslc->Eq[0], rslc->Eq[1], rslc->Eq[2], rslc->Eq[3],
                           nqd,  SUMA_VE_Headname(VO->VE,ive));
               glBegin(GL_POLYGON);
                  for (k=0; k<6; ++k) { /* draw all 6 points always, even 
                                           when there are repetitions. 
                                           Don't bother trimming to unique
                                           set unless this causes trouble */
                     /* change mm (edge coordinate to texture coords) */
                     AFF44_MULT_I(tex_corn, VO->VE[ive]->X2I, (slc_corn+3*k));
                     /* offset indices because slc_corn is on edge */
                     if (tex_corn[0] < 0) tex_corn[0] = 0;
                     else if (tex_corn[0] > VO->VE[ive]->Ni-1) 
                        tex_corn[0] = VO->VE[ive]->Ni-1;
                     if (tex_corn[1] < 0) tex_corn[1] = 0;
                     else if (tex_corn[1] > VO->VE[ive]->Nj-1) 
                        tex_corn[1] = VO->VE[ive]->Nj-1;
                     if (tex_corn[2] < 0) tex_corn[2] = 0;
                     else if (tex_corn[2] > VO->VE[ive]->Nk-1) 
                        tex_corn[2] = VO->VE[ive]->Nk-1;
                     tex_corn[0] /= (float)(VO->VE[ive]->Ni-1);
                     tex_corn[1] /= (float)(VO->VE[ive]->Nj-1);
                     tex_corn[2] /= (float)(VO->VE[ive]->Nk-1);
                     glTexCoord3f(tex_corn[0], 
                                  tex_corn[1], tex_corn[2]);
                           /* this one is affected by the Texture MatrixMode */
                     glVertex3f(slc_corn[3*k], 
                                slc_corn[3*k+1], slc_corn[3*k+2]); 
                           /* this one is affected by the Modelview matrixMode*/
                  }
               glEnd();                                 
            }
            #if 0
            {
               GLvoid *dbuf, *dlum;
               int ij; float ijmin, ijmax;
               GLfloat *uidb=NULL, *fff=NULL;
               /* grab depth buffer DOES NOT WORK*/
               dbuf = SUMA_grabPixels(5, sv->X->aWIDTH, sv->X->aHEIGHT);
               fff = (GLfloat *)dbuf;
               uidb = (GLfloat *)
                     SUMA_malloc(sv->X->aWIDTH*sv->X->aHEIGHT*sizeof(GLfloat));
               glReadPixels(0, 0, sv->X->aWIDTH, sv->X->aHEIGHT, 
                            GL_DEPTH_COMPONENT, GL_FLOAT, uidb);
               
               ijmin = ijmax = (float)uidb[0];
               for (ij=0; ij<sv->X->aWIDTH*sv->X->aHEIGHT; ++ij) {
                  if (ijmin > uidb[ij]) {
                     ijmin = (float)uidb[ij];
                     fprintf(stderr,"min %f\n", (float)uidb[ij]);
                  }
                  if (ijmax < uidb[ij]) {
                     ijmax = (float)uidb[ij];
                     fprintf(stderr,"max %f\n", (float)uidb[ij]);
                  }
               }
               fprintf(stderr, "range %f %f\n", ijmin, ijmax);
               fprintf(stderr, "%f versus %f\n", fff[0], uidb[0]);
               /* grab luminance buffer */
               dlum = SUMA_grabPixels(1, sv->X->aWIDTH, sv->X->aHEIGHT);
               /* reset anything dark */
               /* rewrite depth buffer */
               SUMA_PixelsToDisk(sv, sv->X->aWIDTH, sv->X->aHEIGHT, 
                                 uidb, 5, 1, "dbuf.jpg", 1, 1);
               SUMA_PixelsToDisk(sv, sv->X->aWIDTH, sv->X->aHEIGHT, 
                                 dlum, 1, 1, "dlum.jpg", 1, 1);
               /* free buffers */
               SUMA_ifree(dbuf); SUMA_ifree(dlum);
            }
            #endif
            {
               GLfloat *dbuf;
               GLubyte *lbuf;
               int ij; 
               float ijmin, ijmax;
               /* grab depth buffer DOES NOT WORK*/
               dbuf = (GLfloat *)
                     SUMA_malloc(sv->X->aWIDTH*sv->X->aHEIGHT*sizeof(GLfloat));
               glReadPixels(0, 0, sv->X->aWIDTH, sv->X->aHEIGHT, 
                            GL_DEPTH_COMPONENT, GL_FLOAT, dbuf);
               ijmin = ijmax = (float)dbuf[0];
               for (ij=0; ij<sv->X->aWIDTH*sv->X->aHEIGHT; ++ij) {
                  if (ijmin > dbuf[ij]) {
                     ijmin = (float)dbuf[ij];
                     fprintf(stderr,"min %f\n", (float)dbuf[ij]);
                  }
                  if (ijmax < dbuf[ij]) {
                     ijmax = (float)dbuf[ij];
                     fprintf(stderr,"max %f\n", (float)dbuf[ij]);
                  }
               }
               fprintf(stderr, "depth range %f %f\n", ijmin, ijmax);
               /* grab luminance buffer */
               lbuf = (GLubyte *)SUMA_grabPixels(1,sv->X->aWIDTH,sv->X->aHEIGHT);
               ijmin = ijmax = (float)lbuf[0];
               for (ij=0; ij<sv->X->aWIDTH*sv->X->aHEIGHT; ++ij) {
                  if (ijmin > lbuf[ij]) {
                     ijmin = (float)lbuf[ij];
                     fprintf(stderr,"min %f\n", (float)lbuf[ij]);
                  }
                  if (ijmax < lbuf[ij]) {
                     ijmax = (float)lbuf[ij];
                     fprintf(stderr,"max %f\n", (float)lbuf[ij]);
                  }
               }
               fprintf(stderr, "luminance range %f %f\n", ijmin, ijmax);
               /* reset anything dark */
               for (ij=0; ij<sv->X->aWIDTH*sv->X->aHEIGHT; ++ij) {
                  if (lbuf[ij]<20) dbuf[ij]=-1.0; /* max it out */
               }
               /* rewrite depth buffer */
               glWindowPos2s(0,0); /* specify raster position */
               glDrawPixels(sv->X->aWIDTH, sv->X->aHEIGHT, 
                            GL_DEPTH_COMPONENT, GL_FLOAT, dbuf);
               /* render last slice again */
               if ((nqd = SUMA_PlaneBoxIntersect( sv->GVS[sv->StdView].ViewFrom, 
                                             rslc->Eq, VO->VE[ive]->bcorners, 
                                             slc_corn)) > 2) {
               SUMA_LH("Have plane %f %f %f %f, %d pts on vol %s",
                           rslc->Eq[0], rslc->Eq[1], rslc->Eq[2], rslc->Eq[3],
                           nqd,  SUMA_VE_Headname(VO->VE,ive));
               glBegin(GL_POLYGON);
                  for (k=0; k<6; ++k) { /* draw all 6 points always, even 
                                           when there are repetitions. 
                                           Don't bother trimming to unique
                                           set unless this causes trouble */
                     /* change mm (edge coordinate to texture coords) */
                     AFF44_MULT_I(tex_corn, VO->VE[ive]->X2I, (slc_corn+3*k));
                     /* offset indices because slc_corn is on edge */
                     if (tex_corn[0] < 0) tex_corn[0] = 0;
                     else if (tex_corn[0] > VO->VE[ive]->Ni-1) 
                        tex_corn[0] = VO->VE[ive]->Ni-1;
                     if (tex_corn[1] < 0) tex_corn[1] = 0;
                     else if (tex_corn[1] > VO->VE[ive]->Nj-1) 
                        tex_corn[1] = VO->VE[ive]->Nj-1;
                     if (tex_corn[2] < 0) tex_corn[2] = 0;
                     else if (tex_corn[2] > VO->VE[ive]->Nk-1) 
                        tex_corn[2] = VO->VE[ive]->Nk-1;
                     tex_corn[0] /= (float)(VO->VE[ive]->Ni-1);
                     tex_corn[1] /= (float)(VO->VE[ive]->Nj-1);
                     tex_corn[2] /= (float)(VO->VE[ive]->Nk-1);
                     glTexCoord3f(tex_corn[0], 
                                  tex_corn[1], tex_corn[2]);
                           /* this one is affected by the Texture MatrixMode */
                     glVertex3f(slc_corn[3*k], 
                                slc_corn[3*k+1], slc_corn[3*k+2]); 
                           /* this one is affected by the Modelview matrixMode*/
                  }
               glEnd();                                 
               }               
               /* junk it */
               SUMA_PixelsToDisk(sv, sv->X->aWIDTH, sv->X->aHEIGHT, 
                                 dbuf, 5, 1, "dbuf.jpg", 1, 1);
               SUMA_PixelsToDisk(sv, sv->X->aWIDTH, sv->X->aHEIGHT, 
                                 lbuf, 1, 1, "dlum.jpg", 1, 1);
               /* free buffers */
               SUMA_ifree(dbuf); SUMA_ifree(lbuf);
            }
            if (ive > 0) {
               SUMA_S_Warn("Add blending here");
               /* Here is where you blend slice from this VE with the previous 
                  This should work just fine as is actually, no need to blend
                  separately unless doing overlay on top always. In that case,
                  render each VE separately then blend results across VEs
               */
            }
            ++ive;
         }
            
         if (el != dlist_head(VSaux->slcl)) el = dlist_prev(el);
         else el = NULL;
      } while (el);
   }
   SUMA_CHECK_GL_ERROR("OpenGL Error ddd");
   
   glFlush();
   
   
   /* Here we create a texture on the cutplane from the last dset loaded
   At the moment, without this texture, nothing shows 
   of the overlay volume */
   if (0 && LastTextureOnCutPlane) {
      --ive; /* bring ive counter to last dset put into texture*/
      SUMA_dset_tex_slice_corners( 0, SUMA_VE_dset(VO->VE, ive), 
                                   tex_corn, slc_corn, NULL, 2, 0);

      // Joachim says this is just wrong ...
      tz = 0.5+(-VO->CutPlane[0][3])/(float)VO_NK(VO);

      glEnable(GL_DEPTH_TEST);

      /* If it were not for the slice textures shown here, then the overlay 
         texture would not show up at all! */
      #if 0 
      SUMA_LH("Texture on the slice, with triangles");
      glBegin(GL_TRIANGLES);
         k = 0;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]); ++k;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]); ++k;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]);

         k = 0;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]); k+=2;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]); ++k;
         glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz); 
            glVertex3f( slc_corn[3*k], slc_corn[3*k+1], 
                                    -VO->CutPlane[0][3]);

      glEnd();
      #else
      SUMA_LH("Texture on the slice, QUADS?");
      glBegin(GL_QUADS);
         for (k=0; k<4; ++k) {
            glTexCoord3f(tex_corn[3*k], tex_corn[3*k+1], tz);
                  /* this one is affected by the Texture MatrixMode */
            glVertex3f(slc_corn[3*k], slc_corn[3*k+1], 
                                          -VO->CutPlane[0][3]); 
                  /* this one is affected by the Modelview matrixMode*/
         }
      glEnd();
      #endif
      glDisable(GL_DEPTH_TEST);
   }
   glDisable(GL_TEXTURE_3D);
   
   if (!gl_bl) glDisable(GL_BLEND);
   if (shmodel != GL_FLAT) glShadeModel(shmodel);
   #if 0
   glColorMaterial(GL_FRONT, GL_AMBIENT_AND_DIFFUSE); 
   glEnable(GL_COLOR_MATERIAL);
   for (iplane=0; iplane < 6; ++iplane) {
      if (VO->UseCutPlane[iplane]) {
         if (iplane == VO->SelectedCutPlane) glColor3f(1.0, 1.0, 1.0);
         else { 
            if (ShowUnselected) {
               if (iplane==0 || iplane == 1) glColor3f(1.0, 0.0, 0.0); 
               if (iplane==2 || iplane == 3) glColor3f(0.0, 1.0, 0.0); 
               if (iplane==4 || iplane == 5) glColor3f(0.0, 0.0, 1.0); 
            } else {
               continue;
            }
         }
         glBegin(GL_LINE_LOOP);
            nlt = VO->SOcut[iplane]->NodeList; 
            glVertex3f( nlt[0],nlt[1],nlt[2] );
            glVertex3f( nlt[3],nlt[4],nlt[5] );
            glVertex3f( nlt[6],nlt[7],nlt[8] );
            glVertex3f( nlt[9],nlt[10],nlt[11] );
         glEnd();
      }
   }
   glDisable(GL_COLOR_MATERIAL);
   #endif
   
   if (sv->PolyMode != SRM_Fill) {/* set fill mode back */
      SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
   }

   if (gl_dt) glEnable(GL_DEPTH_TEST);
   else glDisable(GL_DEPTH_TEST);
   if (gl_bl) glEnable(GL_BLEND);
   else glDisable(GL_BLEND);
   SUMA_RETURN(YUP);
   #endif
}


SUMA_Boolean SUMA_VE_LoadTexture(SUMA_VolumeElement **VE, int n)
{
   static char FuncName[]={"SUMA_VE_LoadTexture"};
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!VE || n < 0 || !VE[n]) {
      SUMA_S_Err("NULL input %p %d %p", VE, n, (VE && n >=0) ? VE[n]:NULL);
      SUMA_RETURN(NOPE);
   }
   glPixelStorei(GL_UNPACK_ALIGNMENT, 1); /* Have no padding at the 
                                                end of texel rows*/
   if (!VE[n]->texName) { 
      VE[n]->texName = (GLuint *)SUMA_calloc(1,sizeof(GLuint));
      glGenTextures(1, VE[n]->texName); /* I just need 1 for now*/
   }
   if (!VE[n]->texvec) {
      SUMA_S_Err("NULL texvec!");
      SUMA_RETURN(NOPE);
   }
   
   glBindTexture(GL_TEXTURE_3D, VE[n]->texName[0]); 
                        /* make texName be the current one 
                           This will also create texture object
                           since this is the first time it is
                           called*/

   /* Should texture repeat or no ? - This does not need to be set with 
      every draw call, it should be done at init time*/
   glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_S, GL_CLAMP);
   glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_T, GL_CLAMP);
   glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_WRAP_R, GL_CLAMP);

   /* How should magnification and reduction be handled? */
   glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
   glTexParameteri(GL_TEXTURE_3D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

   SUMA_LHv("Storing texture for %s: %d %d %d\n", 
            SUMA_VE_Headname(VE, n), SUMA_VE_Ni(VE, n), 
            SUMA_VE_Nj(VE, n),  SUMA_VE_Nk(VE, n));
   /* And store the image poiner in question */
   glTexImage3D   (  GL_TEXTURE_3D, 
                     0, /* texture level, highest resolution */
                     GL_RGBA, /* RGBA baby*/
                     SUMA_VE_Ni(VE,n), 
                     SUMA_VE_Nj(VE,n),
                     SUMA_VE_Nk(VE,n),
		               0, /* border is 0 wide. Might have to do borders and 
                           split texture into two, if
                           volume is too big to fit into kangaroo's pouch */
                     GL_RGBA, GL_UNSIGNED_BYTE, 
                     VE[n]->texvec);
   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_VO_InitCutPlanes(SUMA_VolumeObject *VO)
{
   static char FuncName[]={"SUMA_VO_InitCutPlanes"};
   int i,n;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!VO) SUMA_RETURN(NOPE);
   
      /* initialize clipping planes, assuming RAI.
   Even numbered planes cut above the coordinate
   Odd numbered planes cut below the coordinate.
   For a plane P, Objects at X,Y,Z where 
      p[0]X+p[1]Y+p[2]Z+p[3]>=0 are displayed
   Those planes are applied in object space.
   */


   SUMA_LH("The cut off points should be interactive\n"
               "so this next assignment should be done \n"
               "with each drawing operation.\n"
               "These default values, based on First and Last Voxel\n"
               "should be saved in nel.\n"
               "This stuff is not used at the moment. Reconsider later");
   
   VO->CutPlane[0][3] = VO->VE[0]->voN[0]; /* Xmore */
   VO->CutPlane[1][3] = -VO->VE[0]->vo0[0]; /* Xless */
   VO->CutPlane[2][3] = VO->VE[0]->voN[1]; /* Ymore */
   VO->CutPlane[3][3] = -VO->VE[0]->vo0[1]; /* Yless */
   VO->CutPlane[4][3] = VO->VE[0]->voN[2]; /* Zmore */
   VO->CutPlane[5][3] = -VO->VE[0]->vo0[2]; /* Zless */
         
   VO->CutPlane[0][0] = 0.0;  /* Z (I/S) plane Clip below plane*/
   VO->CutPlane[0][1] = 0.0; 
   VO->CutPlane[0][2] = 1.0; 
   VO->CutPlane[0][3] = 0.8*VO->VE[0]->voN[2]; 
   
   VO->CutPlane[1][0] = 0.0;  /* Z (I/S) plane, clip above plane*/
   VO->CutPlane[1][1] = 0.0; 
   VO->CutPlane[1][2] = -1.0; 
   VO->CutPlane[1][3] = -0.8*VO->VE[0]->vo0[2];
   
   VO->CutPlane[2][0] = 0.0;  /* Y (A/P) plane, clip anterior plane*/
   VO->CutPlane[2][1] = 1.0; 
   VO->CutPlane[2][2] = 0.0; 
   VO->CutPlane[2][3] = 0.8*VO->VE[0]->voN[1];; 
   
   VO->CutPlane[3][0] = 0.0;  /* Y (A/P) plane, clip posterior plane*/
   VO->CutPlane[3][1] = -1.0; 
   VO->CutPlane[3][2] = 0.0; 
   VO->CutPlane[3][3] = -0.8*VO->VE[0]->vo0[1];
   
   VO->CutPlane[4][0] = 1.0;  /* X (R/L) plane, clip right plane*/
   VO->CutPlane[4][1] = 0.0; 
   VO->CutPlane[4][2] = 0.0; 
   VO->CutPlane[4][3] = 0.8*VO->VE[0]->voN[0]; 
   
   VO->CutPlane[5][0] = -1.0;  /* X (R/L) plane, clip left plane*/
   VO->CutPlane[5][1] = 0.0; 
   VO->CutPlane[5][2] = 0.0; 
   VO->CutPlane[5][3] = -0.8*VO->VE[0]->vo0[0];
   
   for (i=0; i<6; ++i) {
      SUMA_SetTextureClipPlaneSurface(VO, i);
   }

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_CreateGL3DTexture(SUMA_VolumeObject *VO)
{
   static char FuncName[]={"SUMA_CreateGL3DTexture"};
   int i,n;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Initializing and creating texture\n"
           "So if I am handling the mixing, it is the \n"
           "color vector that ends up filling the texture map...");
   glPixelStorei(GL_UNPACK_ALIGNMENT, 1); /* Have no padding at the 
                                                end of texel rows*/
   if (0) {
   SUMA_S_Note(
         "Need to allow user to specify more than one texture.\n"
         "This would happen if each new nel got its own texName\n"
         "But this operation might fail if lots of textures are \n"
         "loaded, so I should guard against that.\n"
         "Regarding multiple textures, the simplest is to have\n"
         "anatomy and function in each texture and then render one\n"
         "after the other.\n"
         "Also, should check how and when textures are to be deleted\n"
         "and whether texture nel (and other NIDOs) are being properly\n"
         "disposed of when a DO is freed\n"
         "Lastly, is the issue of the dset header that I loath to \n"
         "duplicate into nel. I should just keep dset's header around\n"
         "somehow. For the moment, dset is one static variable in this\n"
         "file and that is clearly not appropriate. So have to deal with\n"
         "that problem too.\n");
   }
   n = 0;
   while (VO->VE && VO->VE[n]) {
      if (!VO->VE[n]->texName) { 
         VO->VE[n]->texName = (GLuint *)SUMA_calloc(1,sizeof(GLuint));
         glGenTextures(1, VO->VE[n]->texName); /* I just need 1 for now*/

         if (!SUMA_VE_LoadTexture(VO->VE, n)) {
            SUMA_S_Err("Failed to load texture for %d", n);
            SUMA_RETURN(NOPE);
         }
      } else {
         SUMA_S_Note("Proably done already via SUMA_Overlays_2_GLCOLAR4's\n"
                     "call to SUMA_VE_LoadTexture. Does this function still \n"
                     "have a reason to exist?");
      }
      ++n;
   }   
   
   if (!SUMA_VO_InitCutPlanes(VO)) {
      SUMA_S_Err("Failed to init cutplanes");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_RETURN(YUP);   
}

SUMA_Boolean SUMA_Draw3DTextureNIDOnel (NI_element *nel, 
                                    SUMA_SurfaceObject *SO, 
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, 
                                    void *default_font, int default_node,
                                    SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Draw3DTextureNIDOnel"};
   SUMA_VolumeObject *VO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   
   if (!nel || strcmp(nel->name,"3DTex")) SUMA_RETURN(NOPE);
   
   if (NI_IS_STR_ATTR_EQUAL(nel,"read_status","fail")) {
      /* can't be read */
      SUMA_LH("read_status is fail, giving up");
      SUMA_RETURN(NOPE);
   }
   
   
   if (!NI_IS_STR_ATTR_EQUAL(nel,"read_status","read")) { /* read it */
      if (!SUMA_Load3DTextureNIDOnel(nel, default_coord_type)) {
         SUMA_LH("Failed to load 3d texture");
         SUMA_RETURN(NOPE);
      }
      
      if (!(VO = SUMA_VOof3DTextureNIDOnel(nel))) {
         SUMA_S_Err("Failed to find corresponding VO");
         SUMA_RETURN(NOPE);
      }
      
      if (!SUMA_CreateGL3DTexture(VO)) {
         SUMA_S_Err("Failed to create texture");
         SUMA_RETURN(NOPE);
      }      
   } else {
      if (!(VO = SUMA_VOof3DTextureNIDOnel(nel))) {
         SUMA_S_Err("Failed to find corresponding VO");
         SUMA_RETURN(NOPE);
      }
   }   

   if (!SUMA_DrawVolumeDO(VO, sv)) {
      SUMA_S_Err("Failed to draw volume");
      SUMA_RETURN(NOPE);
   }
      

   
   
   SUMA_RETURN(YUP);     
}

int iPlane2Dim(int iplane) 
{
   if (iplane == 0 || iplane == 1) return(2);
   else if (iplane == 2 || iplane == 3) return(1);
   else if (iplane == 4 || iplane == 5) return(0);
   else return(-1);
}

SUMA_Boolean SUMA_SetTextureClipPlaneSurface(
                        SUMA_VolumeObject *VO, int iplane )
{
   static char FuncName[]={"SUMA_SetTextureClipPlaneSurface"};
   SUMA_SurfaceObject *SO=NULL;
   static int nwarn = 0;
   int k;
   int *FaceSetList=NULL;
   float *NodeList=NULL;
   GLfloat tex_corn[12] ;
   GLfloat slc_corn[12] ;
   SUMA_DSET *dset=NULL;
   SUMA_NEW_SO_OPT *nsoopt=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!VO || !VO->VE || !(dset = SUMA_VE_dset(VO->VE,0)) 
       || iplane < 0 || iplane > 5) {
      SUMA_S_Err("Bad flow");
      SUMA_RETURN(NOPE);
   }
   if (!VO->SOcut[iplane]) { 
      SUMA_LH("Creating surface for first time");

      NodeList = (float *)SUMA_calloc(3*4, sizeof(float)); 
            /* four nodes for a rectangle */
      FaceSetList = (int *)SUMA_calloc(2*3, sizeof(int));  
            /* two triangles   */       
      
      switch (iplane) {
         case 0:/* initialize for plane 0 */
            SUMA_dset_tex_slice_corners( 0, dset, tex_corn, 
                                         slc_corn, NULL, iPlane2Dim(iplane), 0);
            k=0;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = -VO->CutPlane[iplane][3];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = -VO->CutPlane[iplane][3];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = -VO->CutPlane[iplane][3];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = -VO->CutPlane[iplane][3];
            break;
         case 1:
            SUMA_dset_tex_slice_corners( 0, dset,  tex_corn, 
                                         slc_corn, NULL, iPlane2Dim(iplane), 0);
            k=0;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = -VO->CutPlane[iplane][3];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = -VO->CutPlane[iplane][3];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = -VO->CutPlane[iplane][3];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = -VO->CutPlane[iplane][3];
            break;
         case 2:
            SUMA_dset_tex_slice_corners( 0, dset,  tex_corn, 
                                         slc_corn, NULL, iPlane2Dim(iplane), 0);
            k=0;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = -VO->CutPlane[iplane][3];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = -VO->CutPlane[iplane][3];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = -VO->CutPlane[iplane][3];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = -VO->CutPlane[iplane][3];
            NodeList[3*k+2] = slc_corn[3*k+2];
            break;
         case 3:
            SUMA_dset_tex_slice_corners( 0, dset, tex_corn, 
                                         slc_corn, NULL, iPlane2Dim(iplane), 0);
            k=0;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = -VO->CutPlane[iplane][3];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = -VO->CutPlane[iplane][3];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = -VO->CutPlane[iplane][3];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = slc_corn[3*k];
            NodeList[3*k+1] = -VO->CutPlane[iplane][3];
            NodeList[3*k+2] = slc_corn[3*k+2];
            break;
         case 4:
            SUMA_dset_tex_slice_corners( 0, dset, tex_corn, 
                                         slc_corn, NULL, iPlane2Dim(iplane), 0);
            k=0;
            NodeList[3*k] = -VO->CutPlane[iplane][3];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = -VO->CutPlane[iplane][3];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = -VO->CutPlane[iplane][3];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = -VO->CutPlane[iplane][3];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = slc_corn[3*k+2];
            break;
         case 5:
            SUMA_dset_tex_slice_corners( 0, dset, tex_corn, 
                                         slc_corn, NULL, iPlane2Dim(iplane), 0);
            k=0;
            NodeList[3*k] = -VO->CutPlane[iplane][3];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = -VO->CutPlane[iplane][3];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = -VO->CutPlane[iplane][3];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = slc_corn[3*k+2];
            k++;
            NodeList[3*k] = -VO->CutPlane[iplane][3];
            NodeList[3*k+1] = slc_corn[3*k+1];
            NodeList[3*k+2] = slc_corn[3*k+2];
            break;
         default:
            SUMA_S_Errv("Bad iplane %d\n", iplane);
            SUMA_RETURN(NOPE);
      }
      FaceSetList[0] = 0;
      FaceSetList[1] = 1;
      FaceSetList[2] = 2;

      FaceSetList[3] = 0;
      FaceSetList[4] = 2;
      FaceSetList[5] = 3;
      
      nsoopt = SUMA_NewNewSOOpt();
      VO->SOcut[iplane] = SUMA_NewSO( &NodeList, 4, &FaceSetList, 2, nsoopt );
      /* free nsoopt here */
      nsoopt = SUMA_FreeNewSOOpt(nsoopt);
   } 
   
   NodeList = VO->SOcut[iplane]->NodeList;
   FaceSetList = VO->SOcut[iplane]->FaceSetList;


   /* adjust depending on which plane is called for */
   switch (iplane) {
      case 0:
         SUMA_dset_tex_slice_corners( 0, dset, tex_corn, 
                                      slc_corn, NULL, iPlane2Dim(iplane), 0);
         k=0;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = -VO->CutPlane[iplane][3];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = -VO->CutPlane[iplane][3];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = -VO->CutPlane[iplane][3];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = -VO->CutPlane[iplane][3];
	 
         break;
      case 1:
         SUMA_dset_tex_slice_corners( 0, dset, tex_corn, 
                                      slc_corn, NULL, iPlane2Dim(iplane), 0);
         k=0;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = VO->CutPlane[iplane][3];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = VO->CutPlane[iplane][3];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = VO->CutPlane[iplane][3];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = VO->CutPlane[iplane][3];
         break;
      case 2:
         SUMA_dset_tex_slice_corners( 0, dset, tex_corn, 
                                      slc_corn, NULL, iPlane2Dim(iplane), 0);
         k=0;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = -VO->CutPlane[iplane][3];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = -VO->CutPlane[iplane][3];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = -VO->CutPlane[iplane][3];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = -VO->CutPlane[iplane][3];
         NodeList[3*k+2] = slc_corn[3*k+2];
         break;
      case 3:
         SUMA_dset_tex_slice_corners( 0, dset, tex_corn, 
                                      slc_corn, NULL, iPlane2Dim(iplane), 0);
         k=0;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = VO->CutPlane[iplane][3];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = VO->CutPlane[iplane][3];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = VO->CutPlane[iplane][3];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] = slc_corn[3*k];
         NodeList[3*k+1] = VO->CutPlane[iplane][3];
         NodeList[3*k+2] = slc_corn[3*k+2];
         break;
      case 4:
         SUMA_dset_tex_slice_corners( 0, dset, tex_corn, 
                                      slc_corn, NULL, iPlane2Dim(iplane), 0);
         k=0;
         NodeList[3*k] = -VO->CutPlane[iplane][3];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] = -VO->CutPlane[iplane][3];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] = -VO->CutPlane[iplane][3];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] = -VO->CutPlane[iplane][3];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = slc_corn[3*k+2];
         break;
      case 5:
         SUMA_dset_tex_slice_corners( 0, dset, tex_corn, 
                                      slc_corn, NULL, iPlane2Dim(iplane), 0);
         k=0;
         NodeList[3*k] =  VO->CutPlane[iplane][3];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] =  VO->CutPlane[iplane][3];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] =  VO->CutPlane[iplane][3];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = slc_corn[3*k+2];
         k++;
         NodeList[3*k] =  VO->CutPlane[iplane][3];
         NodeList[3*k+1] = slc_corn[3*k+1];
         NodeList[3*k+2] = slc_corn[3*k+2];
         break;
      default:
         SUMA_S_Err("Should not be here");
         break;
   } 

   SUMA_RETURN(YUP);
}

SUMA_SurfaceObject ** SUMA_TextureClipPlaneSurfaces(int *N_SOv)
{
   static char FuncName[]={"SUMA_TextureClipPlaneSurfaces"};
   SUMA_SurfaceObject **SOv=NULL;
   SUMA_VolumeObject *VO=NULL;
   int ii=0, jj, kk;
   SUMA_Boolean LocalHead = NOPE;

   
   SUMA_ENTRY;
   
   *N_SOv=0;
   for (ii=0; ii<SUMAg_N_DOv; ++ii) {
      if (SUMA_isVO(SUMAg_DOv[ii])) *N_SOv = *N_SOv+6;
   }
   
   SOv = (SUMA_SurfaceObject **)SUMA_calloc(*N_SOv, 
                                 sizeof(SUMA_SurfaceObject *));
   kk = 0;
   for (ii=0; ii<SUMAg_N_DOv; ++ii) {
      if (SUMA_isVO(SUMAg_DOv[ii])) {
         VO = (SUMA_VolumeObject *)(SUMAg_DOv[ii].OP);
         for (jj=0; jj<6; ++jj) {
            if (VO->UseCutPlane[jj]) {
               SOv[kk] = VO->SOcut[jj]; ++kk;
            }
         }
      }
   }
   
   *N_SOv = kk;
   
   SUMA_RETURN(SOv);
}

SUMA_VolumeObject *SUMA_VolumeObjectOfClipPlaneSurface(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_VolumeObjectOfClipPlaneSurface"};
   SUMA_VolumeObject *VO=NULL, *VOr = NULL;
   int ii=0, jj, kk;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   VO = NULL; VOr=NULL;
   for (ii=0; ii<SUMAg_N_DOv; ++ii) {
      if (SUMA_isVO(SUMAg_DOv[ii])) {
         VO = (SUMA_VolumeObject *)(SUMAg_DOv[ii].OP);
         for (jj=0; jj<6; ++jj) {
            if (VO->SOcut[jj] == SO) {
               if (!VOr) VOr = VO;
               else {
                  SUMA_S_Err("Found more than one VO for SO");
                  SUMA_RETURN(NULL);
               }
            }
         }
      }
   }
   
   SUMA_RETURN(VOr);
}

int SUMA_VO_SlicesAtCrosshair(SUMA_VolumeObject *VO)
{
   SUMA_VOL_SAUX *VSaux = VDO_VSAUX(VO);
   if (VSaux) return(VSaux->SlicesAtCrosshair);
   return(0);
}   

