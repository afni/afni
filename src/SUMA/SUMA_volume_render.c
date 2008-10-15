#include "SUMA_suma.h"

/* Volume rendering code, based on GLUT-3.7's advanced97/volume.c 
See also SUMA_GLUT_volumedemo.c*/

/* nonzero if not power of 2 */ 
#define NOTPOW2(num) ((num) & (num - 1))


/* Old restrictions (before OpenGL2.1 required power of 2 textures. This is not the case anymore relax this in the near future. */
int
makepow2(int val)
{
    int power = 0;
    if(!val)
	return 0;

    while(val >>= 1)
	power++;

    return(1 << power);
}

#define CHECK_ERROR(str)                                           \
{                                                                  \
    GLenum error;                                                  \
    if(error = glGetError())                                       \
       fprintf(stderr,"**************GL Error: %s (%s)\n", \
         gluErrorString(error), str);  \
}


THD_3dim_dataset *dset=NULL;

static GLfloat lightpos[4] = {150., 150., 150., 1.f};

/* define a cutting plane */
GLdouble cutplane[] = {0.f, -.5f, -2.f, 50.f};

GLubyte *
dset_to_tex3d(THD_3dim_dataset *dset)
{
    char *filename;
    GLubyte *tex3ddata;
    GLuint *texslice; /* 2D slice of 3D texture */
    GLint max3dtexdims; /* maximum allowed 3d texture dimension */
    GLint newval;
    int i, j;
    byte *bytevol=NULL;

    /* load 3D texture data */
    {
       bytevol = (byte *)malloc(DSET_NVOX(dset)*sizeof(byte));
       tex3ddata = (GLubyte *)malloc(DSET_NVOX(dset) * 
				     4 * sizeof(GLubyte));
       
       EDIT_coerce_scale_type( 
                     DSET_NVOX(dset) ,
                     DSET_BRICK_FACTOR(dset,0) ,
                     DSET_BRICK_TYPE(dset,0), 
                     DSET_ARRAY(dset, 0) ,   /* input  */
                     MRI_byte               , bytevol  ) ;
       j=0;
       for(i = 0; i < DSET_NVOX(dset); i++) {
         tex3ddata[j] = bytevol[i]; ++j;
         tex3ddata[j] = bytevol[i]; ++j;
         tex3ddata[j] = bytevol[i]; ++j;
         tex3ddata[j] = bytevol[i]; ++j;
       }
      free(bytevol); bytevol=NULL;
    }

    return tex3ddata;
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


SUMA_Boolean SUMA_Load3DTextureNIDOnel (NI_element *nel)
{
   static char FuncName[]={"SUMA_Load3DTextureNIDOnel"};
   THD_3dim_dataset *odset=NULL;
   int i, j, newval;
   byte *bytevol=NULL, *bp=NULL;
   char *fname=NULL, orcode[6];
   int Texcomps=1, max3dtexdims=0;
   float fv[12], vo0[3], voN[3];
   int iv[12];
   int NewNx=0, NewNy=0, NewNz=0;
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
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }
   if (!(dset = THD_open_dataset( fname ))) {
      SUMA_S_Errv("Failed to open %s\n", fname);
      SUMA_free(fname); fname = NULL;
      SUMA_RETURN(NOPE);
   }

   /* coerce dset into byte (unsigned in afni) */
   bytevol = (byte *)SUMA_malloc(DSET_NVOX(dset)*sizeof(byte));
   DSET_load(dset);
   orcode[0] = ORIENT_typestr[dset->daxes->xxorient][0] ;
   orcode[1] = ORIENT_typestr[dset->daxes->yyorient][0] ;
   orcode[2] = ORIENT_typestr[dset->daxes->zzorient][0] ;
   orcode[3] = '\0';
   SUMA_LHv("dset orcode is %s\n", orcode);
   
   SUMA_S_Note("Relax power of 2 restriction\n"
               "Decide whether it is worth restricting\n"
               "yourself to RAI...\n");
   /* texture is to be power of two in each direction, 
   so get new dimensions, this is no longer necessary
   with OpenGL > 1.2, remove restriction in the future */
   NewNx = makepow2(DSET_NX(dset));
   NewNy = makepow2(DSET_NY(dset));
   NewNz = makepow2(DSET_NZ(dset));
   if (  strcmp(orcode,"RAI") || 
         DSET_NX(dset) != NewNx || 
         DSET_NY(dset) != NewNy || 
         DSET_NZ(dset) != NewNz) {
      /* resample into RAI, assuming that is needed */
      odset = r_new_resam_dset(dset, NULL, NewNx, NewNy, NewNz, 
                               "RAI", MRI_LINEAR, NULL, 1);
      DSET_delete(dset); dset = odset; odset = NULL;
   }
   
   EDIT_coerce_scale_type( 
               DSET_NVOX(dset) ,
               DSET_BRICK_FACTOR(dset,0) ,
               DSET_BRICK_TYPE(dset,0), 
               DSET_ARRAY(dset, 0) ,   /* input  */
               MRI_byte               , bytevol  ) ;
   /* create texture vector */
   NI_alter_veclen(nel, (int)(DSET_NVOX(dset)*4));
   if (LocalHead) {
      SUMA_LHv("veclen altered to %d\n", DSET_NVOX(dset)*4);
      SUMA_ShowNel(nel);
   }
   NI_add_column (nel, NI_BYTE, NULL); 
   if (!nel->vec[0]) {
      SUMA_S_Crit("Failed to allocate.");
      DSET_delete(dset);
      SUMA_free(fname); fname = NULL;
      SUMA_free(bytevol);
      SUMA_RETURN(NOPE); 
   }
   SUMA_LH("Filling");
   bp = (byte *)nel->vec[0];
   j=0;
   for(i = 0; i < DSET_NVOX(dset); i++) {
      bp[j] = bytevol[i]; ++j;
      bp[j] = bytevol[i]; ++j;
      bp[j] = bytevol[i]; ++j;
      bp[j] = bytevol[i]; ++j;
   }
   SUMA_free(bytevol); bytevol=NULL;
   
   SUMA_free(fname); fname = NULL;

   /* Set the box limits, assuming you are in RAI */
   SUMA_dset_extreme_corners(dset, vo0, voN);
   NI_SET_FLOATv(nel,"FirstVoxel", vo0, 3);
   NI_SET_FLOATv(nel,"LastVoxel", voN, 3);
   
   NI_set_attribute(nel,"read_status","read");
   
   SUMA_S_Note("dset is not freed anywhere here");
      
 

   SUMA_RETURN(YUP);
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
void SUMA_dset_tex_slice_corners( int slc, THD_3dim_dataset *dset, 
                              GLfloat *tcorners, GLfloat *corners)
{
   static char FuncName[]={"SUMA_dset_tex_slice_corners"};   
   int kk=0;
   float orig[3] = { 0, 0, 0}, del[3] = { 0, 0, 0};
   int nvox[3] = { 0, 0, 0};
    
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

   
    corners[kk] = orig[0] + 0       * del[0]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[1] + 0       * del[1]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[2] + slc     * del[2]; 
   tcorners[kk] = ((float)slc+0.5)/(float)nvox[2];++kk;
   
    corners[kk] = orig[0] + (nvox[0]-1) * del[0];  
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[1] + 0       * del[1]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[2] + slc     * del[2]; 
   tcorners[kk] = tcorners[2];                  ++kk;

   
    corners[kk] = orig[0] + (nvox[0]-1) * del[0]; 
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[1] + (nvox[1]-1) * del[1]; 
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[2] + slc     * del[2]; 
   tcorners[kk] = tcorners[2];                  ++kk;

    corners[kk] = orig[0] + 0       * del[0]; 
   tcorners[kk] = 0;                            ++kk;
    corners[kk] = orig[1] + (nvox[1]-1) * del[1]; 
   tcorners[kk] = 1;                            ++kk;
    corners[kk] = orig[2] + slc     * del[2]; 
   tcorners[kk] = tcorners[2];                  ++kk;
   
   SUMA_RETURNe;
}

#define SUMA_GL_MAT_SHOW(mm,str) {\
   int i;   \
   glGetDoublev(mm, dmatrix); \
   fprintf(stderr,"%s", str); \
   for (i=0; i<4; ++i) {   \
      fprintf(stderr,"\t");  \
      fprintf(stderr,"%+3.3f\t%+3.3f\t%+3.3f\t%+3.3f\t", \
               dmatrix[i],dmatrix[4+i],dmatrix[8+i],dmatrix[12+i]); \
      fprintf(stderr,"\n");  \
   }\
}

SUMA_Boolean SUMA_Draw3DTextureNIDOnel (NI_element *nel, 
                                    SUMA_SurfaceObject *SO, 
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, 
                                    void *default_font,
                                    SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Draw3DTextureNIDOnel"};
   static GLuint texName;
   int i = 0, k = 0, j=0;
   float iq[4]={0, 0, 0, 0}, vo0[3], voN[3];
   static int ipass=0;
   GLfloat tex_corn[12] ;
   GLfloat slc_corn[12] ;
   GLfloat rotationMatrix[4][4], rt[4][4];
   GLboolean gl_dt, gl_bl;
   static GLdouble clipplane0[] = {-1.,  0.,  0., 50.}; /* default */
   static GLdouble clipplane1[] = { 1.,  0.,  0., 50.}; /* clipping  */
   static GLdouble clipplane2[] = { 0., -1.,  0., 50.}; /* plane */
   static GLdouble clipplane3[] = { 0.,  1.,  0., 50.}; /* values */
   static GLdouble clipplane4[] = { 0.,  0., -1., 50.}; /* ...... */
   static GLdouble clipplane5[] = { 0.,  0.,  1., 50.}; /* ...... */
   static GLfloat init_rotationMatrix[4][4];
   static GLdouble dmatrix[16], init_mv_matrix[16];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   
   if (!nel || strcmp(nel->name,"3DTex")) SUMA_RETURN(NOPE);
   
   if (NI_IS_STR_ATTR_EQUAL(nel,"read_status","fail")) {
      /* can't be read */
      SUMA_RETURN(NOPE);
   }
   
   /*glPushAttrib(GL_ALL_ATTRIB_BITS);    be safe push all attributes down */
   
   if (!NI_IS_STR_ATTR_EQUAL(nel,"read_status","read")) { /* read it */
      if (!SUMA_Load3DTextureNIDOnel(nel)) {
         SUMA_RETURN(NOPE);
      }
      /* At this moment, dset is static variable, this should later be folded 
      somewhere into SUMA's global structures */
      SUMA_LH("Initializing and creating texture");
      /* initialization */
      glPixelStorei(GL_UNPACK_ALIGNMENT, 1); /* Have no padding at the 
                                                end of texel rows*/
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
      NI_GET_INT(nel,"texName",texName);
      if (!NI_GOT) {
         /* As expected, brand new. Need to generate texture */
         glGenTextures(1, &texName);   /* I just need 1 */
         /* Now store it */
         NI_SET_INT(nel,"texName",texName);
      } else {
         SUMA_S_Err("I don't know how this happened");
         SUMA_RETURN(NOPE);
      }
   
      glBindTexture(GL_TEXTURE_3D, texName); /* make texName be the current one 
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
      
      SUMA_LHv("Storing texture: %d %d %d\n", 
               DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset));
      /* And store the image poiner in question */
      glTexImage3D   (  GL_TEXTURE_3D, 
                        0, /* texture level, highest resolution */
                        GL_RGBA, /* RGBA baby*/
                        DSET_NX(dset), DSET_NY(dset), DSET_NZ(dset),
		                  0, /* border is 0 wide. Might have to do borders and 
                              split texture into two, if
                              volume is too big to fit into kangaroo's pouch */
                        GL_RGBA, GL_UNSIGNED_BYTE, 
                        nel->vec[0]);
      
      /* initialize clipping planes, assuming RAI.
      Even numbered planes cut above the coordinate
      Odd numbered planes cut below the coordinate.
      For a plane P, Objects at X,Y,Z where 
         p[0]X+p[1]Y+p[2]Z+p[3]>=0 are displayed
      Those planes are applied in object space.
      */
      
      NI_GET_FLOATv(nel,"FirstVoxel", vo0,3,1);
      NI_GET_FLOATv(nel,"LastVoxel", voN,3,1);
      
      SUMA_S_Note("The cut off points should be interactive\n"
                  "so this next assignment should be done \n"
                  "with each drawing operation.\n"
                  "These default values, based on First and Last Voxel\n"
                  "should be saved in nel.\n")
      clipplane0[3] = voN[0]; /* Xmore */
      clipplane1[3] = -vo0[0]; /* Xless */
      clipplane2[3] = voN[1]; /* Ymore */
      clipplane3[3] = -vo0[1]; /* Yless */
      clipplane4[3] = voN[2]; /* Zmore */
      clipplane5[3] = -vo0[2]; /* Zless */
      
      clipplane0[0] = -1.0;   /* cut coords > Xmore along x axis */
      clipplane1[0] =  1.0;   /* cut coords < -Xless along x axis */
      clipplane2[1] = -1.0;   /* cut coords > Ymore along y axis */
      clipplane3[1] =  1.0;   /* cut coords < -Yless along y axis */
      clipplane4[2] = -1.0;   /* cut coords > Zmore along z axis */
      clipplane5[2] =  1.0;   /* cut coords < -Zless along z axis */
      
   }
   
   if (sv->PolyMode != SRM_Fill) {
      /* fill it up */
      glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);   
   }
   
   NI_GET_INT(nel,"texName",texName);
   if (!NI_GOT) {
      SUMA_S_Errv("Weird, texture %d should have been created by now (%d)", 
                  texName, glIsTexture(texName));
      SUMA_RETURN(NOPE);
   }
   
   
   SUMA_S_Note( "Draw Clipping planes without modelview matrix enabled\n"
            "So planes are fixed in space.\n"
            "We're more used to creating cutting planes that \n"
            "are attached to the object. So should look into that\n"
            "instead. \n"
            "Note that the use of these clipping planes\n"
            "seem necessary when the volume is not padded with zeros.\n"
            "I don't quite know what that is, but I suspect it has to do\n"
            "with alphas being non zero at the tip and the placement of\n"
            "texture quads\n"  );
   glClipPlane(GL_CLIP_PLANE0, clipplane0);
   glClipPlane(GL_CLIP_PLANE1, clipplane1);
   glClipPlane(GL_CLIP_PLANE2, clipplane2);
   glClipPlane(GL_CLIP_PLANE3, clipplane3);
   glClipPlane(GL_CLIP_PLANE4, clipplane4);
   glClipPlane(GL_CLIP_PLANE5, clipplane5); /* play with this 
                                             one to change cuts*/


   /* Now we need to draw the sucker */
   SUMA_LHv("About to draw texture %d\n", texName);
   CHECK_ERROR("OpenGL Error pre texture");
   glEnable(GL_TEXTURE_3D);
   
   /* enable clipping planes. */
   glEnable(GL_CLIP_PLANE0);
   glEnable(GL_CLIP_PLANE1);
   glEnable(GL_CLIP_PLANE2);
   glEnable(GL_CLIP_PLANE3);
   glEnable(GL_CLIP_PLANE4);
   glEnable(GL_CLIP_PLANE5);

   glTexEnvf(  GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, 
                  SUMA_NIDO_TexEnvMode(nel, GL_REPLACE)); /* what happens if 
                              there is color already on a vertex (I would likely
                              not need this for 3D textures...*/
   glBindTexture(GL_TEXTURE_3D, texName); /* make texName be current */
   /* Now generate the coordinates */
   SUMA_LH( "About to generate polygons"); 
   glShadeModel(GL_FLAT);        /* This should be reverted to sv's settings */
   if (!(gl_dt = glIsEnabled(GL_DEPTH_TEST))) glEnable(GL_DEPTH_TEST);      
   if (!(gl_bl = glIsEnabled(GL_BLEND))) glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   if (LocalHead) {
      SUMA_GL_MAT_SHOW(GL_TEXTURE_MATRIX,"Tx PreSetup\n");
      SUMA_GL_MAT_SHOW(GL_MODELVIEW_MATRIX,"MV PreSetup\n");
   }
   CHECK_ERROR("OpenGL Error pre setup");
   
   /* The modelview matrix for drawing the quad vertices should remain the same,
   the texture matrix will reflect object rotations.
   IF you do want to see the 'slices', i.e. a stack of axial slices (for RAI dset), rather than volume rendering image, then allo the quads to be drawn
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
      if (LocalHead) {
         fprintf(stderr,"init_mv_matrix recorded to be:\n");
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
      SUMA_TRANSP_4MATRIX(rotationMatrix); /* transpose = inverse 
                                             for rotation matrix*/
         
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
      

   if (LocalHead) {
      SUMA_GL_MAT_SHOW(GL_TEXTURE_MATRIX,"Tx PreDraw\n");
      SUMA_GL_MAT_SHOW(GL_MODELVIEW_MATRIX,"MV PreDraw\n");
   }
   
   CHECK_ERROR("OpenGL Error pre draw");
   #if 1
   /* slice by slice drawing, doing the deed by hand*/
   for(i = 0; i < DSET_NZ(dset); i++) {
      glBegin(GL_QUADS);
         SUMA_dset_tex_slice_corners( i, dset, tex_corn, slc_corn)     ;
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

    glTexGenfv(GL_S, GL_OBJECT_PLANE, splane);  /* THis ordering of the planes */
    glTexGenfv(GL_T, GL_OBJECT_PLANE, rplane);  /* resulted in proper axis */
    glTexGenfv(GL_R, GL_OBJECT_PLANE, tplane);  /*  alignment ...*/
   
      
      
    for(i = 0; i < DSET_NZ(dset); i++) {
      glBegin(GL_QUADS);
         SUMA_dset_tex_slice_corners( i, dset, tex_corn, slc_corn)     ;
         for (k=0; k<4; ++k) {
            glVertex3f(slc_corn[3*k], slc_corn[3*k+1], slc_corn[3*k+2]);
         }
      glEnd();
   }
   glDisable(GL_TEXTURE_GEN_S);
   glDisable(GL_TEXTURE_GEN_T);
   glDisable(GL_TEXTURE_GEN_R);
   #endif
    CHECK_ERROR("OpenGL Error ddd");

   glMatrixMode(GL_TEXTURE); glPopMatrix(); /* pop matrix of GL_TEXTURE */
   glMatrixMode(GL_MODELVIEW);  glPopMatrix();/* and Modelview*/

   glFlush();
   
   /* glPopAttrib(); put all attributes back where they were */
   /* disable the clipping planes */
   glDisable(GL_CLIP_PLANE0);
   glDisable(GL_CLIP_PLANE1);
   glDisable(GL_CLIP_PLANE2);
   glDisable(GL_CLIP_PLANE3);
   glDisable(GL_CLIP_PLANE4);
   glDisable(GL_CLIP_PLANE5);

   glDisable(GL_TEXTURE_3D);
   if (!gl_dt) glDisable(GL_DEPTH_TEST);
   if (!gl_bl) glDisable(GL_BLEND);
   if (sv->PolyMode != SRM_Fill) {/* set fill mode back */
      SUMA_SET_GL_RENDER_MODE(sv->PolyMode);
   }
   
               
   
   SUMA_RETURN(YUP);     
}

