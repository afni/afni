#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  

/* Volume rendering code, based on GLUT-3.7's advanced97/volume.c 
See also SUMA_GLUT_volumedemo.c*/


#define SUMA_CHECK_GL_ERROR(str)                                           \
{                                                                  \
    GLenum error;                                                  \
    while((error = glGetError()) != GL_NO_ERROR)                   \
       fprintf(stderr,"**************GL Error: %s (%s)\n", \
         gluErrorString(error), str);  \
}



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

GLubyte * SUMA_dset_to_tex3d(THD_3dim_dataset **dsetp, byte col)
{  
   static char FuncName[]={"SUMA_dset_to_tex3d"};
   char *filename;
   GLubyte *tex3ddata;
   GLint max3dtexdims; /* maximum allowed 3d texture dimension */
   GLint newval;
   THD_3dim_dataset *dset=NULL;
   THD_3dim_dataset *odset=NULL;
   char orcode[6], *np=NULL;
   int NewNx=0, NewNy=0, NewNz=0;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
    
   dset = *dsetp;
    
   /* make sure data part of dset is loaded, not just the header          */
   DSET_load(dset);
   orcode[0] = ORIENT_typestr[dset->daxes->xxorient][0] ;
   orcode[1] = ORIENT_typestr[dset->daxes->yyorient][0] ;
   orcode[2] = ORIENT_typestr[dset->daxes->zzorient][0] ;
   orcode[3] = '\0';
   SUMA_LHv("dset orcode is %s\n", orcode);

   /* texture is to be power of two in each direction, 
   so get new dimensions, this is no longer necessary
   with OpenGL > 1.2, remove restriction in the future */
   NewNx = SUMA_makepow2(DSET_NX(dset));
   NewNy = SUMA_makepow2(DSET_NY(dset));
   NewNz = SUMA_makepow2(DSET_NZ(dset));
   if (LocalHead && 
        (DSET_NX(dset) != NewNx || 
         DSET_NY(dset) != NewNy || 
         DSET_NZ(dset) != NewNz)) {
      SUMA_LHv("Just note that dset %s %d %d %d\n"
               "does not have power of 2 dimensions\n"
               "No resampling to %d %d %d will be done.\n",
               DSET_HEADNAME(dset),
               DSET_NX(dset) , DSET_NY(dset), DSET_NZ(dset),
               NewNx, NewNy, NewNz );
           
   }
   if (  strcmp(orcode,"RAI") ) {
      SUMA_LHv("Resampling %s to RAI\n", DSET_HEADNAME(dset));
      /* resample into RAI, assuming that is needed */
      odset = r_new_resam_dset(dset, NULL, 0.0, 0.0, 0.0, 
                               "RAI", MRI_LINEAR, NULL, 1, 1);
      np = SUMA_append_string(DSET_PREFIX(dset), ".RAI");
      EDIT_dset_items(  odset ,
                      ADN_prefix      , np,
                      ADN_none ) ;
      tross_Copy_History( dset , odset ) ;      
      DSET_delete(dset); dset = odset; odset = NULL;
      if (LocalHead) {
         SUMA_LH("Writing resampled dset");
         DSET_write(dset);
      }
      SUMA_free(np); np = NULL;
      *dsetp = dset;
   }

   if (!(tex3ddata = 
            (GLubyte *)SUMA_malloc(4*DSET_NVOX(dset)*sizeof(GLubyte)))) {
      SUMA_S_Crit("Failed to allocate.");
      SUMA_RETURN(NULL); 
   }
   if (LocalHead) {
      SUMA_LHv("Copying %d intensity in R, G, B, A \n", 
               DSET_NVOX(dset)*4);
   }
   if (!SUMA_Colorize_dset(dset, tex3ddata, col)) {
      SUMA_S_Err("Failed to colorize VO");
      SUMA_RETURN(NULL); 
   }

   
   SUMA_RETURN(tex3ddata);
}

/* This function here is for illustrative purposes.
   It may be too inefficient to have to allocate and 
   free SV for each colorizing operation. 
   We'll see how slow it is and then improve if need be
*/
SUMA_Boolean SUMA_Colorize_dset(THD_3dim_dataset *dset, 
                                 byte *tex3ddata, byte colopt)
{
   static char FuncName[]={"SUMA_Colorize_dset"};
   static SUMA_SCALE_TO_MAP_OPT *Opt=NULL;  
   static SUMA_COLOR_MAP *ColMap=NULL; 
   SUMA_COLOR_SCALED_VECT * SV= NULL;
   float *floatvol=NULL;
   byte *bytevol=NULL, am=0;
   int i, j, i3;
   float av=0.0;
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
   
   /* Create temporary holding structure for colorized vectors */
   if (!(SV = SUMA_Create_ColorScaledVect(DSET_NVOX(dset), 0))) {
      SUMA_S_Err("Failed to create SV");
      ans = NOPE;      goto CLEANUP;
   }
   
            
   /* copy into tex3ddata */
   if (!colopt) {
      bytevol = (byte *)SUMA_calloc(DSET_NVOX(dset), sizeof(byte));
      if (!bytevol) {
         SUMA_S_Err("Failed to allocate for bytevol");
         ans = NOPE;      goto CLEANUP;
      }
      EDIT_coerce_scale_type( 
                  DSET_NVOX(dset) ,
                  DSET_BRICK_FACTOR(dset,0) ,
                  DSET_BRICK_TYPE(dset,0), 
                  DSET_ARRAY(dset, 0) ,   /* input  */
                  MRI_byte               , bytevol  ) ;
      j=0;
      #if 0
      /* Playing with color overlay */
      SUMA_S_Note("TEMPO17");
      for(i = 0; i < DSET_NVOX(dset); i++) {
         if (bytevol[i]!=17) {
            tex3ddata[j] = bytevol[i]; ++j;
            tex3ddata[j] = bytevol[i]; ++j;
            tex3ddata[j] = bytevol[i]; ++j;
            tex3ddata[j] = bytevol[i]/3; ++j;
         } else {
            tex3ddata[j] = 255; ++j;
            tex3ddata[j] = 0; ++j;
            tex3ddata[j] = 0; ++j;
            tex3ddata[j] = 255; ++j;
         }
      }
      #else
      for(i = 0; i < DSET_NVOX(dset); i++) {
         tex3ddata[j] = bytevol[i]; ++j;
         tex3ddata[j] = bytevol[i]; ++j;
         tex3ddata[j] = bytevol[i]; ++j;
         tex3ddata[j] = bytevol[i]; ++j;
      }
      #endif
      if (bytevol) SUMA_free(bytevol); bytevol=NULL;
   } else {
      /* put dset values in temporary floatvol float vector */
      floatvol = (float *)SUMA_calloc(DSET_NVOX(dset), sizeof(float));
      if (!floatvol) {
         SUMA_S_Err("Failed to allocate for floatvol");
         ans = NOPE;      goto CLEANUP;
      }
      EDIT_coerce_scale_type( 
                  DSET_NVOX(dset) ,
                  DSET_BRICK_FACTOR(dset,0) ,
                  DSET_BRICK_TYPE(dset,0), 
                  DSET_ARRAY(dset, 0) ,   /* input  */
                  MRI_float               , floatvol  ) ;
      if (!SUMA_ScaleToMap_alaAFNI (floatvol, DSET_NVOX(dset),
                                 0.0, ColMap, Opt, SV)) {
         SUMA_S_Err("Failed to colorize");
         ans = NOPE;      goto CLEANUP;
      }
      j=0;
      for(i = 0; i < DSET_NVOX(dset); i++) {
         i3 = 3*i; av = 0.0; am = 0;
         tex3ddata[j] = (byte)(SV->cV[i3  ] * 255);
            av += tex3ddata[j]; am = tex3ddata[j];                        ++j;
         tex3ddata[j] = (byte)(SV->cV[i3+1] * 255); 
            av += tex3ddata[j]; if (tex3ddata[j] > am) am = tex3ddata[j]; ++j;
         tex3ddata[j] = (byte)(SV->cV[i3+2] * 255);
            av += tex3ddata[j]; if (tex3ddata[j] > am) am = tex3ddata[j]; ++j;
         if (0)   tex3ddata[j] = (byte)(av/3.0); 
            else  tex3ddata[j] = am; 
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


SUMA_EnablingRecord SUMA_RecordEnablingState(void)
{
   static char FuncName[]={"SUMA_RecordEnablingState"};
   SUMA_EnablingRecord SER;
   
   SUMA_ENTRY;
   
   SER.DEPTH_TEST = glIsEnabled(GL_DEPTH_TEST);
   SER.TEXTURE_3D_EXT = glIsEnabled(GL_TEXTURE_3D_EXT);
   SER.TEXTURE_3D = glIsEnabled(GL_TEXTURE_3D);
   SER.TEXTURE_GEN_S = glIsEnabled(GL_TEXTURE_GEN_S);
   SER.TEXTURE_GEN_T = glIsEnabled(GL_TEXTURE_GEN_T);
   SER.TEXTURE_GEN_R = glIsEnabled(GL_TEXTURE_GEN_R);
   SER.CLIP_PLANE0 = glIsEnabled(GL_CLIP_PLANE0);
   SER.CLIP_PLANE1 = glIsEnabled(GL_CLIP_PLANE1);
   SER.CLIP_PLANE2 = glIsEnabled(GL_CLIP_PLANE2);
   SER.CLIP_PLANE3 = glIsEnabled(GL_CLIP_PLANE3);
   SER.CLIP_PLANE4 = glIsEnabled(GL_CLIP_PLANE4);
   SER.CLIP_PLANE5 = glIsEnabled(GL_CLIP_PLANE5);
   SER.LIGHTING = glIsEnabled(GL_LIGHTING);
   SER.LIGHT0 = glIsEnabled(GL_LIGHT0);
   SER.LIGHT1 = glIsEnabled(GL_LIGHT1);
   SER.LIGHT2 = glIsEnabled(GL_LIGHT2);
   SER.BLEND = glIsEnabled(GL_BLEND);
   /* SER. = glIsEnabled(GL_); */
   
   SUMA_RETURN(SER);
}

void SUMA_RestoreEnablingState(SUMA_EnablingRecord SER)
{
   static char FuncName[]={"SUMA_RestoreEnablingState"};
   
   SUMA_ENTRY;
      
   if (SER.DEPTH_TEST) glEnable(GL_DEPTH_TEST);
   else glDisable(GL_DEPTH_TEST);
   if (SER.TEXTURE_3D_EXT) glEnable(GL_TEXTURE_3D_EXT);
   else glDisable(GL_TEXTURE_3D_EXT);
   if (SER.TEXTURE_3D) glEnable(GL_TEXTURE_3D);
   else glDisable(GL_TEXTURE_3D);
   if (SER.TEXTURE_GEN_S) glEnable(GL_TEXTURE_GEN_S);
   else glDisable(GL_TEXTURE_GEN_S);
   if (SER.TEXTURE_GEN_T) glEnable(GL_TEXTURE_GEN_T);
   else glDisable(GL_TEXTURE_GEN_T);
   if (SER.TEXTURE_GEN_R) glEnable(GL_TEXTURE_GEN_R);
   else glDisable(GL_TEXTURE_GEN_R);
   if (SER.CLIP_PLANE0) glEnable(GL_CLIP_PLANE0);
   else glDisable(GL_CLIP_PLANE0);
   if (SER.CLIP_PLANE1) glEnable(GL_CLIP_PLANE1);
   else glDisable(GL_CLIP_PLANE1);
   if (SER.CLIP_PLANE2) glEnable(GL_CLIP_PLANE2);
   else glDisable(GL_CLIP_PLANE2);
   if (SER.CLIP_PLANE3) glEnable(GL_CLIP_PLANE3);
   else glDisable(GL_CLIP_PLANE3);
   if (SER.CLIP_PLANE4) glEnable(GL_CLIP_PLANE4);
   else glDisable(GL_CLIP_PLANE4);
   if (SER.CLIP_PLANE5) glEnable(GL_CLIP_PLANE5);
   else glDisable(GL_CLIP_PLANE5);
   if (SER.LIGHTING) glEnable(GL_LIGHTING);
   else glDisable(GL_LIGHTING);
   if (SER.LIGHT0) glEnable(GL_LIGHT0);
   else glDisable(GL_LIGHT0);
   if (SER.LIGHT1) glEnable(GL_LIGHT1);
   else glDisable(GL_LIGHT1);
   if (SER.LIGHT2) glEnable(GL_LIGHT2);
   else glDisable(GL_LIGHT2);
   if (SER.BLEND) glEnable(GL_BLEND);
   else glDisable(GL_BLEND);
   /* if (SER.) glEnable(); */
   
   SUMA_RETURNe;
}

char *SUMA_EnablingState_Info(SUMA_EnablingRecord SER)
{
   static char FuncName[]={"SUMA_EnablingState_Info"};
   char *s=NULL;
   SUMA_STRING *SS=NULL;
   
   SUMA_ENTRY;
      
   SS = SUMA_StringAppend(NULL, NULL);
   SUMA_StringAppend_va(SS,"GL_DEPTH_TEST is %s\n", 
                        SER.DEPTH_TEST ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_TEXTURE_3D_EXT is %s\n", 
                        SER.TEXTURE_3D_EXT ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_TEXTURE_3D is %s\n", 
                        SER.TEXTURE_3D ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_TEXTURE_GEN_S is %s\n", 
                        SER.TEXTURE_GEN_S ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_TEXTURE_GEN_T is %s\n", 
                        SER.TEXTURE_GEN_T ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_TEXTURE_GEN_R is %s\n", 
                        SER.TEXTURE_GEN_R ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_CLIP_PLANE0 is %s\n", 
                        SER.CLIP_PLANE0 ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_CLIP_PLANE1 is %s\n", 
                        SER.CLIP_PLANE1 ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_CLIP_PLANE2 is %s\n", 
                        SER.CLIP_PLANE2 ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_CLIP_PLANE3 is %s\n", 
                        SER.CLIP_PLANE3 ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_CLIP_PLANE4 is %s\n", 
                        SER.CLIP_PLANE4 ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_CLIP_PLANE5 is %s\n", 
                        SER.CLIP_PLANE5 ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_LIGHTING is %s\n", 
                        SER.LIGHTING ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_LIGHT0 is %s\n", 
                        SER.LIGHT0 ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_LIGHT1 is %s\n", 
                        SER.LIGHT1 ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_LIGHT2 is %s\n", 
                        SER.LIGHT2 ? "Enabled":"Disabled"); 
   SUMA_StringAppend_va(SS,"GL_BLEND is %s\n", 
                        SER.BLEND ? "Enabled":"Disabled"); 

/*   
   SUMA_StringAppend_va(SS,"GL_ is %s\n", 
                        SER. ? "Enabled":"Disabled"); 
                        */
   SUMA_SS2S(SS,s);
   
   SUMA_RETURN(s);
}
void SUMA_ShowEnablingState(SUMA_EnablingRecord SER, FILE *out, char *preamble) {
   static char FuncName[]={"SUMA_ShowEnablingState"};
   char *s=NULL;
   SUMA_ENTRY;
   if (!out) out = SUMA_STDOUT;
   
   s = SUMA_EnablingState_Info(SER);
   
   fprintf(out, "%s%s", preamble ? preamble:"", s);
   
   SUMA_free(s); s = NULL;
   
   SUMA_RETURNe;
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


SUMA_Boolean SUMA_Load3DTextureNIDOnel (NI_element *nel, 
                                    SUMA_DO_CoordUnits defaultcoordtype)
{
   static char FuncName[]={"SUMA_Load3DTextureNIDOnel"};
   char *fname=NULL;
   char *atr=NULL, stmp[128];
   int i = 0;
   SUMA_VolumeObject *VO=NULL;
   SUMA_DO_CoordUnits coord_type=SUMA_WORLD;
   THD_3dim_dataset *dset=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   if (  !nel || 
         strcmp(nel->name,"3DTex")   )
      SUMA_RETURN(NOPE);
   
   if (NI_IS_STR_ATTR_EQUAL(nel, "read_status", "read")) SUMA_RETURN(YUP);
    
   NI_set_attribute(nel,"read_status","fail");
   
   if (! (fname = SUMA_copy_string(NI_get_attribute(nel,"filename"))) )
      SUMA_RETURN(NOPE);
   if (!SUMA_search_file(&fname, NULL)) { /* can't find it */ 
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }
   
   if (!(dset = THD_open_dataset( fname ))) {
      SUMA_S_Errv("Failed to open %s\n", fname);
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }     

   /* Create a DO from dset */
   if (!(VO = SUMA_CreateVolumeObject(fname))) {
      SUMA_S_Err("Failed to create volume object");
      if (dset) DSET_delete(dset); 
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }
   /* put main dset into VO */
   if (!(SUMA_AddDsetVolumeObject(VO, &dset))) {
      SUMA_S_Err("Failed to add volume");
      if (dset) DSET_delete(dset);
      VO = SUMA_FreeVolumeObject(VO); 
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }
   SUMA_free(fname); fname = NULL;

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
      if (!(dset = THD_open_dataset( fname ))) {
         SUMA_S_Errv("Failed to open %s\n", fname);
         SUMA_free(fname); fname = NULL;
         break;
      }
      
      /* put overlay dset into VO */
      if (!(SUMA_AddDsetVolumeObject(VO, &dset))) {
         SUMA_S_Err("Failed to add volume");
         if (dset) DSET_delete(dset);
         SUMA_free(fname); fname = NULL;
         break;
      }
      SUMA_LHv("Added %s\n", DSET_HEADNAME(VO->VE[i+1]->dset));
      SUMA_free(fname); fname = NULL;  
      ++i;
      sprintf(stmp,"overlay%d",i);
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
   
   VO->TexEnvMode = SUMA_NIDO_TexEnvMode(nel, GL_REPLACE); 

   /* Add VO into DO list */
   if (!SUMA_AddDO(SUMAg_DOv, &(SUMAg_N_DOv), (void *)VO,  
                     VO_type, coord_type)) {
      fprintf(SUMA_STDERR,"Error %s: Error Adding DO\n", FuncName);
      SUMA_RETURN(NOPE);
   }
   
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
   SUMA_Boolean LocalHead = YUP;
   
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
   centers of first and last voxels in volume */
void SUMA_dset_extreme_corners( THD_3dim_dataset *dset, 
                                float * mincorner, float *maxcorner)
{
   static char FuncName[]={"SUMA_dset_extreme_corners"};   
   int kk=0;
   float orig[3] = { 0, 0, 0}, del[3] = { 0, 0, 0};
   int nvox[3] = { 0, 0, 0};
    
   SUMA_ENTRY;
   
   mincorner[0] = dset->daxes->xxorg ; 
   mincorner[1] = dset->daxes->yyorg ; 
   mincorner[2] = dset->daxes->zzorg ; 
   nvox[0] = DSET_NX(dset);
   nvox[1] = DSET_NY(dset);
   nvox[2] = DSET_NZ(dset);
   del[0] = dset->daxes->xxdel ; 
   del[1] = dset->daxes->yydel ; 
   del[2] = dset->daxes->zzdel ; 

   maxcorner[0] = mincorner[0] +  (nvox[0]-1) * del[0];
   maxcorner[1] = mincorner[1] +  (nvox[1]-1) * del[1];
   maxcorner[2] = mincorner[2] +  (nvox[2]-1) * del[2];
   
   
   SUMA_RETURNe;
}

/*! Note that corners returned correspond to voxel centers */
void SUMA_dset_tex_slice_corners( int slci, THD_3dim_dataset *dset, 
                              GLfloat *tcorners, GLfloat *corners, 
                              int dim)
{
   static char FuncName[]={"SUMA_dset_tex_slice_corners"};   
   int kk=0;
   float orig[3] = { 0, 0, 0}, del[3] = { 0, 0, 0};
   int nvox[3] = { 0, 0, 0};
   int slcx, slcy, slcz=0;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
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

#define SUMA_GL_MAT_SHOW(mm,str) {\
   int i;   \
   glGetDoublev(mm, dmatrix); \
   SUMA_S_Note(str); \
   for (i=0; i<4; ++i) {   \
      fprintf(stderr,"\t");  \
      fprintf(stderr,"%+3.3f\t%+3.3f\t%+3.3f\t%+3.3f\t", \
               dmatrix[i],dmatrix[4+i],dmatrix[8+i],dmatrix[12+i]); \
      fprintf(stderr,"\n");  \
   }\
}


SUMA_Boolean SUMA_DrawVolumeDO(SUMA_VolumeObject *VO, SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_DrawVolumeDO"};
   int i = 0, k = 0, j=0, ive=0;
   float iq[4]={0, 0, 0, 0}, vo0[3], voN[3];
   static int ipass=0, iplane = 0;
   GLfloat tex_corn[12] ;
   GLfloat slc_corn[12] ;
   GLfloat rotationMatrix[4][4], rt[4][4];
   GLboolean gl_dt, gl_bl;
   int ShowUnselected = 0;
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
   
   
   if (!VO->Show) SUMA_RETURN(YUP);
   
   if (sv->PolyMode != SRM_Fill) {
      /* fill it up */
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);   
   }
   
   
   
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
         SUMA_S_Errv("Weird, texture %d should have been created by now (%d)", 
                     VO->VE[ive]->texName[0], 
                     glIsTexture(VO->VE[ive]->texName[0]));
         SUMA_RETURN(NOPE);
      }

      SUMA_LHv("About to bind texture %d for %s\n", 
            VO->VE[ive]->texName[0], DSET_HEADNAME(VO->VE[ive]->dset));
      glBindTexture(GL_TEXTURE_3D, VO->VE[ive]->texName[0]); 
                                             /* make texName be current */

      /* Now generate the coordinates */
      SUMA_LHv( "About to generate polygons for dset %d (%p)\n", 
               ive, VO->VE[ive]->dset); 
      SUMA_LHv( "About to generate polygons for dset %d (%s)\n", 
               ive, DSET_HEADNAME(VO->VE[ive]->dset)); 
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
         glLoadIdentity(); /* set to identity*/

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
         glTranslatef (-0.5, 
                        -0.5, 
                        -0.5);  


      if (0 && LocalHead) {
         SUMA_GL_MAT_SHOW(GL_TEXTURE_MATRIX,"Tx PreDraw\n");
         SUMA_GL_MAT_SHOW(GL_MODELVIEW_MATRIX,"MV PreDraw\n");
      }

      SUMA_CHECK_GL_ERROR("OpenGL Error pre draw");
      #if 1
      /* slice by slice drawing, doing the deed by hand*/
      for(i = 0; i < DSET_NZ(VO->VE[ive]->dset); i++) {
         glBegin(GL_QUADS);
            SUMA_dset_tex_slice_corners(i, VO->VE[ive]->dset, tex_corn, 
                                        slc_corn, 2);
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
                                          tex_corn, slc_corn,2);
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
      SUMA_dset_tex_slice_corners( 0, VO->VE[ive]->dset, tex_corn, slc_corn, 2);

      // Joachim says this is just wrong ...
      tz = 0.5+(-VO->CutPlane[0][3])/(float)DSET_NZ(VO->VE[ive]->dset);

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
         if (iplane == VO->SelectedCutPlane) glColor3f(1.0, 0, 0);
         else { 
            if (ShowUnselected) {
               glColor3f(0.0, 1.0, 0); /* Unselected in green */
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

SUMA_Boolean SUMA_CreateGL3DTexture(SUMA_VolumeObject *VO)
{
   static char FuncName[]={"SUMA_CreateGL3DTexture"};
   int i,n;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;
   
   SUMA_LH("Initializing and creating texture");
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
      if (VO->VE[n]->texName) {
         SUMA_S_Err("Unexpected texName, not ready for such a case");
         SUMA_RETURN(NOPE);
      }

      VO->VE[n]->texName = (GLuint *)SUMA_calloc(1,sizeof(GLuint));
      glGenTextures(1, VO->VE[n]->texName); /* I just need 1 for now*/

      glBindTexture(GL_TEXTURE_3D, VO->VE[n]->texName[0]); 
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
               DSET_HEADNAME(VO->VE[n]->dset),
               DSET_NX(VO->VE[n]->dset), 
               DSET_NY(VO->VE[n]->dset), 
               DSET_NZ(VO->VE[n]->dset));
      /* And store the image poiner in question */
      glTexImage3D   (  GL_TEXTURE_3D, 
                        0, /* texture level, highest resolution */
                        GL_RGBA, /* RGBA baby*/
                        DSET_NX(VO->VE[n]->dset), 
                        DSET_NY(VO->VE[n]->dset), 
                        DSET_NZ(VO->VE[n]->dset),
		                  0, /* border is 0 wide. Might have to do borders and 
                              split texture into two, if
                              volume is too big to fit into kangaroo's pouch */
                        GL_RGBA, GL_UNSIGNED_BYTE, 
                        VO->VE[n]->texvec);
      ++n;
   }   
   /* initialize clipping planes, assuming RAI.
   Even numbered planes cut above the coordinate
   Odd numbered planes cut below the coordinate.
   For a plane P, Objects at X,Y,Z where 
      p[0]X+p[1]Y+p[2]Z+p[3]>=0 are displayed
   Those planes are applied in object space.
   */


   SUMA_S_Note("The cut off points should be interactive\n"
               "so this next assignment should be done \n"
               "with each drawing operation.\n"
               "These default values, based on First and Last Voxel\n"
               "should be saved in nel.\n");
   
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
      SUMA_RETURN(NOPE);
   }
   
   
   if (!NI_IS_STR_ATTR_EQUAL(nel,"read_status","read")) { /* read it */
      if (!SUMA_Load3DTextureNIDOnel(nel, default_coord_type)) {
         SUMA_RETURN(NOPE);
      }
      
      if (!(VO = SUMA_VOof3DTextureNIDOnel(nel))) {
         SUMA_S_Err("Failed to find corresponding VO");
         SUMA_RETURN(NOPE);
      }
      
      if (!SUMA_CreateGL3DTexture(VO)) {
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
   SUMA_NEW_SO_OPT *nsoopt=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!VO || !VO->VE || !VO->VE[0]->dset || iplane < 0 || iplane > 5) {
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
            SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                         tex_corn, slc_corn, iPlane2Dim(iplane));
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
            SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                         tex_corn, slc_corn, iPlane2Dim(iplane));
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
            SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                         tex_corn, slc_corn, iPlane2Dim(iplane));
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
            SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                         tex_corn, slc_corn, iPlane2Dim(iplane));
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
            SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                         tex_corn, slc_corn, iPlane2Dim(iplane));
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
            SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                         tex_corn, slc_corn, iPlane2Dim(iplane));
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
         SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                    tex_corn, slc_corn, iPlane2Dim(iplane));
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
         SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                    tex_corn, slc_corn, iPlane2Dim(iplane));
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
         SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                      tex_corn, slc_corn, iPlane2Dim(iplane));
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
         SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                      tex_corn, slc_corn, iPlane2Dim(iplane));
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
         SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                      tex_corn, slc_corn, iPlane2Dim(iplane));
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
         SUMA_dset_tex_slice_corners( 0, VO->VE[0]->dset, 
                                      tex_corn, slc_corn, iPlane2Dim(iplane));
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
   SUMA_Boolean LocalHead = YUP;

   
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
   SUMA_Boolean LocalHead = YUP;
   
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
