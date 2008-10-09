#include "SUMA_suma.h"

 SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
 int SUMAg_N_SVv; /*!< Number of SVs realized by X  */
/* extern SUMA_SurfaceViewer *SUMAg_cSV; */ /* This variable is no longer used in this file Tue Aug 13 15:27:53 EDT 2002*/ 
 int SUMAg_N_DOv; 
 SUMA_DO *SUMAg_DOv;
 SUMA_CommonFields *SUMAg_CF; 

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
       printf("GL Error: %s (%s)\n", gluErrorString(error), str);  \
}

enum {X, Y, Z, W};
enum {R, G, B, A};
enum {OVER, ATTENUATE, NONE, LASTOP}; /* blend modes */
/* mouse modes */
enum {OBJ_ANGLE, SLICES, CUTTING, GEOMXY, GEOMZ, MINBOOST, BOOSTWID, BOOST}; 
enum {NOLIST, SPHERE}; /* display list */

/* window dimensions */
GLboolean polmode = GL_TRUE;
int winWidth = 512;
int winHeight = 512;
int active;
int operator = OVER;
GLboolean texture = GL_TRUE;
GLboolean dblbuf = GL_TRUE;
GLboolean cut = GL_FALSE;
GLboolean geom = GL_FALSE;
GLboolean map = GL_FALSE;
GLint cutbias = 50;
int hasBlendColor = 0;

GLfloat objangle[2] = {0.f, 0.f};
GLfloat objpos[3] = {0.f, 0.f, 0.f};
GLfloat dyxz[3] = { 1.f, 1.f, 1.f};
GLfloat ori[3] = { -127.f, -112.f, -117.f};

GLfloat minboost = 0.f, boostwid = .03f, boost = 3.f; /* transfer function */

THD_3dim_dataset *dset=NULL;

GLdouble clipplane0[] = {-1.,  0.,  0., 100.}; /* x < 100 out */
GLdouble clipplane1[] = { 1.,  0.,  0., 100.}; /* x > 100 out */
GLdouble clipplane2[] = { 0., -1.,  0., 100.}; /* y < 100 out */
GLdouble clipplane3[] = { 0.,  1.,  0., 100.}; /* y > 100 out */
GLdouble clipplane4[] = { 0.,  0., -1., 100.}; /* z < 100 out */
GLdouble clipplane5[] = { 0.,  0.,  1., 100.}; /* z > 100 out */

static GLfloat splane[4] = {1.f/200.f, 0.f, 0.f, .5f};
static GLfloat rplane[4] = {0, 1.f/200.f, 0, .5f};
static GLfloat tplane[4] = {0, 0, 1.f/200.f, .5f};
static GLfloat lightpos[4] = {150., 150., 150., 1.f};

/* define a cutting plane */
GLdouble cutplane[] = {0.f, -.5f, -2.f, 50.f};
/* initialization for 128x128x128 volume */
GLfloat vox_orig[3] = { -100.f, -100.f, -100.f};
GLint   vox_num[3]  = {128, 128, 128};
GLfloat vol_center[3] = { 100.f, 100.f, 100.f};
GLint   tex_num[3] = {128, 128, 128}; /* this would be texwid, texht, textdepth*/
GLfloat vox_del[3] = { 1.0, 1.0, 1.0}; 
GLubyte *tex3ddata; /* pointer to 3D texture data */
int slices;


GLfloat *lighttex = 0;
GLboolean lightchanged[2] = {GL_TRUE, GL_TRUE};

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

void SUMA_3dTexParameter_setup(THD_3dim_dataset *dset) {
   static char FuncName[]={"SUMA_3dTexParameter_setup"};
   GLint max3dtexdims; /* maximum allowed 3d texture dimension */
   GLint newval;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   tex_num[0] = DSET_NX(dset);   
   tex_num[1] = DSET_NY(dset);
   tex_num[2] = DSET_NZ(dset);
   vox_orig[0] = dset->daxes->xxorg ; 
   vox_orig[1] = dset->daxes->yyorg ; 
   vox_orig[2] = dset->daxes->zzorg ; 
   vox_num[0] = DSET_NX(dset);
   vox_num[1] = DSET_NY(dset);
   vox_num[2] = DSET_NZ(dset);
   vox_del[0] = dset->daxes->xxdel ; 
   vox_del[1] = dset->daxes->yydel ; 
   vox_del[2] = dset->daxes->zzdel ; 
   
   vol_center[0] = vox_orig[0] + ((float)(vox_num[0]-1)*vox_del[0])/2.0f;
   vol_center[1] = vox_orig[1] + ((float)(vox_num[1]-1)*vox_del[1])/2.0f;
   vol_center[2] = vox_orig[2] + ((float)(vox_num[2]-1)*vox_del[2])/2.0f;
      
   slices = tex_num[2];

   SUMA_LHv("nel dset:\n"
            "vox_orig: %.2f %.2f %.2f\n"
            "vox_num : %d %d %d\n"
            "vox_del: %.2f %.2f %.2f\n"
            "vox_center:%.2f %.2f %.2f\n"
            "init tex_num:%2d %2d %2d\n"
            "init slices: %d\n",
            vox_orig[0], vox_orig[1], vox_orig[2],
            vox_num[0], vox_num[1], vox_num[2],
            vox_del[0], vox_del[1], vox_del[2],
            vol_center[0], vol_center[1], vol_center[2],
            tex_num[0], tex_num[1], tex_num[2],
            slices);

/* setting up OpenGL */
   /* adjust *plane */
   splane[0] = 1.f/(float)vox_num[0];
   splane[1] = 0.f;
   splane[2] = 0.f;
   splane[3] = vox_del[0]/2.0;
   rplane[0] = 0.f;
   rplane[1] = 1.f/(float)vox_num[1];
   rplane[2] = 0.f;
   rplane[3] = vox_del[1]/2.0;
   tplane[0] = 0.f;
   tplane[1] = 0.f;
   tplane[2] = 1.f/(float)vox_num[2];
   tplane[3] = vox_del[2]/2.0;
   
   SUMA_LH("Adjusting texture dimensions");
   max3dtexdims = 0;

#ifdef GL_EXT_texture3D
    glGetIntegerv(GL_MAX_3D_TEXTURE_SIZE_EXT, &max3dtexdims);
#endif
   SUMA_LHv("Max texdims = %d\n", max3dtexdims);

    /* adjust width */
    newval = tex_num[0];
    if(tex_num[0] > max3dtexdims)
	newval = max3dtexdims;
    if(NOTPOW2(tex_num[0]))
        newval = makepow2(tex_num[0]);
    if(newval != tex_num[0])
    {
	glPixelStorei(GL_UNPACK_ROW_LENGTH, tex_num[0]);
	glPixelStorei(GL_UNPACK_SKIP_PIXELS, (tex_num[0] - newval)/2);
	tex_num[0] = newval;
    }

    /* adjust height */
    newval = tex_num[1];
    if(tex_num[1] > max3dtexdims)
	newval = max3dtexdims;
    if(NOTPOW2(tex_num[1]))
        newval = makepow2(tex_num[1]);
    if(tex_num[1] > newval)
    {
#ifdef GL_EXT_texture3D
	glPixelStorei(GL_UNPACK_IMAGE_HEIGHT_EXT, tex_num[1]);
#endif
	glPixelStorei(GL_UNPACK_SKIP_ROWS, (tex_num[1] - newval)/2);
	tex_num[1] = newval;
    }

    /* adjust depth */
    newval = tex_num[2];
    if(tex_num[2] > max3dtexdims)
	newval = max3dtexdims;
    if(NOTPOW2(tex_num[2]))
        newval = makepow2(tex_num[2]);
    if(tex_num[2] > newval)
    {
	tex_num[2] = newval;
    }
  
   slices = tex_num[2];
   
   SUMA_LHv("Adjusted tex_num: %2d %2d %2d\nslices = %d\n"
            "check on what happens to slices around here in original code.\n",
            tex_num[0], tex_num[1], tex_num[2],
            slices);

   SUMA_RETURNe;

}




/* use pixel path to remap 3D texture data */
void
remaptex(void)
{
    static char FuncName[]={"remaptex"};
    int i, size;
    GLfloat *map;

   fprintf(stderr,"remaptex called.\n");
    glPixelTransferi(GL_MAP_COLOR, GL_TRUE);

    glGetIntegerv(GL_MAX_PIXEL_MAP_TABLE, &size);

    map = (GLfloat *)malloc(sizeof(GLfloat) * size);
    for(i = 0; i < size;i++)
    {
	map[i] = (GLfloat)i/(size - 1);
	   if(((GLfloat)i/size > minboost) &&
	      ((GLfloat)i/size < minboost + boostwid))
	   {
	       map[i] *= boost;
	   }
	   else
	       map[i] /= boost;
          
    }
      
    glPixelMapfv(GL_PIXEL_MAP_R_TO_R, size, map);
    glPixelMapfv(GL_PIXEL_MAP_G_TO_G, size, map);
    glPixelMapfv(GL_PIXEL_MAP_B_TO_B, size, map);
    glPixelMapfv(GL_PIXEL_MAP_A_TO_A, size, map);

#ifdef GL_EXT_texture3D
    SUMA_S_Note("Running GL_EXT_texture3D...\n");
    glTexImage3DEXT(GL_TEXTURE_3D_EXT, 0, GL_LUMINANCE_ALPHA,
		    tex_num[0], tex_num[1], tex_num[2],
		    0,
		    GL_RGBA, GL_UNSIGNED_BYTE, tex3ddata);
#endif

    glPixelTransferi(GL_MAP_COLOR, GL_FALSE);
    free(map);

    CHECK_ERROR("OpenGL Error in remaptex()");
}

void SUMA_SetTexImage(void) 
{
   static char FuncName[]={"SUMA_SetTexImage"};
   SUMA_ENTRY;
   #ifdef GL_EXT_texture3D
       SUMA_S_Note("Setting GL_EXT_texture3D...\n");

       glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
       glTexImage3DEXT(GL_TEXTURE_3D_EXT, 0, GL_LUMINANCE_ALPHA,
		       tex_num[0], tex_num[1], tex_num[2],
		       0,
		       GL_RGBA, GL_UNSIGNED_BYTE, tex3ddata);
   #endif
   SUMA_RETURNe;
}      

void SUMA_CreateSphereList(void)
{
   static char FuncName[]={"SUMA_CreateSphereList"};

   SUMA_ENTRY;

   SUMA_S_Note("Making sphere display list");
   /* make a display list containing a sphere */
   glNewList(SPHERE, GL_COMPILE);
   {
      static GLfloat lightpos[] = {150.f, 150.f, 150.f, 1.f};
      static GLfloat material[] = {1.f, .5f, 1.f, 1.f};
      GLUquadricObj *qobj = gluNewQuadric();
      glPushAttrib(GL_LIGHTING_BIT);
      glEnable(GL_LIGHTING);
      glEnable(GL_LIGHT0);
      glLightfv(GL_LIGHT0, GL_POSITION, lightpos);
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
void SUMA_Enable3DRendering(void) 
{
   static char FuncName[]={"SUMA_Enable3DRendering"};
   SUMA_EnablingRecord SER;
   
   SUMA_ENTRY;
   
   glEnable(GL_DEPTH_TEST);
   #ifdef GL_EXT_texture3D
   glEnable(GL_TEXTURE_3D_EXT);
   #endif

   glEnable(GL_TEXTURE_GEN_S);
   glEnable(GL_TEXTURE_GEN_T);
   glEnable(GL_TEXTURE_GEN_R);

   glTexGeni(GL_S, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
   glTexGeni(GL_T, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);
   glTexGeni(GL_R, GL_TEXTURE_GEN_MODE, GL_OBJECT_LINEAR);

   glTexGenfv(GL_S, GL_OBJECT_PLANE, splane);
   glTexGenfv(GL_T, GL_OBJECT_PLANE, tplane);
   glTexGenfv(GL_R, GL_OBJECT_PLANE, rplane);

   #ifdef GL_EXT_texture3D
   /* to avoid boundary problems */
   glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_S, GL_CLAMP);
   glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_T, GL_CLAMP);
   glTexParameteri(GL_TEXTURE_3D_EXT, GL_TEXTURE_WRAP_R_EXT, GL_CLAMP);
   #endif

   glEnable(GL_CLIP_PLANE0);
   glEnable(GL_CLIP_PLANE1);
   glEnable(GL_CLIP_PLANE2);
   glEnable(GL_CLIP_PLANE3);
   glEnable(GL_CLIP_PLANE4);
   glEnable(GL_CLIP_PLANE5);

   glDisable(GL_LIGHT0);
   glLightfv(GL_LIGHT0, GL_POSITION, lightpos);

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

/* draw the object unlit without surface texture */
void SUMA_3DTex_redraw(void)
{
   static char FuncName[]={"SUMA_3DTex_redraw"};
   SUMA_Boolean LocalHead = NOPE;
   int i, k;
   GLfloat slc_corn[12];
   
   GLfloat offS, offT, offR; /* mapping texture to planes */
    
   SUMA_ENTRY;
   
   SUMA_LH("Calculating offsets");
   
   offS = vox_del[0];
   offT = vox_del[1];
   offR = vox_del[2];

   clipplane0[W] = -vox_orig[0] - offS;
   clipplane1[W] = -vox_orig[0] - offS;
   clipplane2[W] = -vox_orig[1] - offT;
   clipplane3[W] = -vox_orig[1] - offT;
   clipplane4[W] = -vox_orig[2] - offR;
   clipplane5[W] = -vox_orig[2] - offR;
   SUMA_LHv("off: [%f %f %f]\n"
            "clp: [%f %f \t%f %f \t%f %f]\n"
            "tex_num: [%d %d %d]\n"
            "slices: %d\n",
            offS, offT, offR,
            clipplane0[W], clipplane1[W], 
            clipplane2[W], clipplane3[W], 
            clipplane4[W], clipplane5[W],
            tex_num[0], tex_num[1], tex_num[2],
            slices );
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

   if(map) {
      SUMA_LH("Remaping texture");
	   remaptex();
   }
   
   /* GL_MODELVIEW */
   if(cut) {
      SUMA_LH("Cutting planes");
      cutplane[W] = cutbias;
      glClipPlane(GL_CLIP_PLANE5, cutplane);
   }

   SUMA_LH("Draw Clipping");
   glPushMatrix(); /* identity */
   glRotatef(objangle[X], 0.f, 1.f, 0.f);
   glRotatef(objangle[Y], 1.f, 0.f, 0.f);
   glClipPlane(GL_CLIP_PLANE0, clipplane0);
   glClipPlane(GL_CLIP_PLANE1, clipplane1);
   glClipPlane(GL_CLIP_PLANE2, clipplane2);
   glClipPlane(GL_CLIP_PLANE3, clipplane3);
   glClipPlane(GL_CLIP_PLANE4, clipplane4);
   if(!cut)
      glClipPlane(GL_CLIP_PLANE5, clipplane5);
   glPopMatrix(); /* back to identity */
   SUMA_LHv("Clipping angles: [%f %f]\n", 
            objangle[X], objangle[Y]);
   
   SUMA_LH("Draw opaque geometry");
   /* draw opaque geometry here */
   glDisable(GL_CLIP_PLANE0);
   glDisable(GL_CLIP_PLANE1);
   glDisable(GL_CLIP_PLANE2);
   glDisable(GL_CLIP_PLANE3);
   glDisable(GL_CLIP_PLANE4);
   if(geom) {
      SUMA_LH("Drawing geom");
	   if(!cut)
	       glDisable(GL_CLIP_PLANE5);
	   glPushMatrix();
	   glTranslatef(objpos[X], objpos[Y], objpos[Z]);
	   glCallList(SPHERE);
	   glPopMatrix();
   }
   
   SUMA_LH("Draw texture");
   glMatrixMode(GL_TEXTURE);
   glEnable(GL_CLIP_PLANE0);
   glEnable(GL_CLIP_PLANE1);
   glEnable(GL_CLIP_PLANE2);
   glEnable(GL_CLIP_PLANE3);
   glEnable(GL_CLIP_PLANE4);
   glEnable(GL_CLIP_PLANE5);

   glMatrixMode(GL_TEXTURE);
   glPushMatrix(); /* identity */
   glTranslatef( .5f,  .5f, .5f);
   glRotatef(objangle[Y], 1.f, 0.f, 0.f);
   glRotatef(objangle[X], 0.f, 0.f, 1.f);
   glTranslatef( -.5f,  -.5f, -.5f);

   SUMA_LHv("Switching blending operator, slices = %d, blend %d\n", 
            slices, operator);
    switch(operator)
    {
    case OVER:
	glEnable(GL_BLEND);
	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
	break;
    case ATTENUATE:
#ifdef GL_EXT_blend_color
        if (hasBlendColor){
	    glEnable(GL_BLEND);
	    glBlendFunc(GL_CONSTANT_ALPHA_EXT, GL_ONE);
	    glBlendColorEXT(1.f, 1.f, 1.f, 1.f/slices);
        } else
#endif
        {
            fprintf(stderr, "volume: attenuate not supported!\n");
        }
        break;
    case NONE:
	/* don't blend */
	break;
    }

    
    if(texture) {
#ifdef GL_EXT_texture3D
       glEnable(GL_TEXTURE_3D_EXT);
#endif
    } else {
#ifdef GL_EXT_texture3D
       glDisable(GL_TEXTURE_3D_EXT);
#endif
       glEnable(GL_LIGHTING);
       glEnable(GL_LIGHT0);
    }

    
   SUMA_LH("Drawing Quads");
   for(i = 0; i < slices; i++) {
         SUMA_dset_slice_corners( i, vox_orig, vox_del, 
                                  vox_num, slc_corn)     ;
         glBegin(GL_QUADS);
         for (k=0; k<4; ++k) {
            glVertex3f(slc_corn[3*k], slc_corn[3*k+1], slc_corn[3*k+2]);
         }
         glEnd();
   }

   #ifdef GL_EXT_texture3D
   glDisable(GL_TEXTURE_3D_EXT);
   #endif
   if(!texture) {
    glDisable(GL_LIGHTING);
   }
   glDisable(GL_BLEND);

   SUMA_LH("Attenuating");
   glPopMatrix(); /* back to identity */
   glMatrixMode(GL_MODELVIEW);

   if(operator == ATTENUATE) {
      glPixelTransferf(GL_RED_SCALE, 3.f); /* brighten image */
      glPixelTransferf(GL_GREEN_SCALE, 3.f);
      glPixelTransferf(GL_BLUE_SCALE, 3.f);
      glCopyPixels(0, 0, winWidth, winHeight, GL_COLOR);
   }
    
   SUMA_LH("Checking error"); 
    CHECK_ERROR("OpenGL Error in redraw()");
   
   #ifdef DO_VOLUME_MAIN
   /* only if using program in stand-alone mode */
   SUMA_LH("Swap or flush"); 
   if(dblbuf)
	   glutSwapBuffers(); 
   else
	   glFlush(); 
   #endif
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
   float fv[12];
   int iv[12];
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

   /* coerce dset into byte (unsigned in afni) */
   bytevol = (byte *)SUMA_malloc(DSET_NVOX(dset)*sizeof(byte));
   DSET_load(dset);
   orcode[0] = ORIENT_typestr[dset->daxes->xxorient][0] ;
   orcode[1] = ORIENT_typestr[dset->daxes->yyorient][0] ;
   orcode[2] = ORIENT_typestr[dset->daxes->zzorient][0] ;
   orcode[3] = '\0';
   SUMA_LHv("dset orcode is %s\n", orcode);
   #if 0
   if (  strcmp(orcode,"RAI") || 
         DSET_NX(dset) != 128 || 
         DSET_NY(dset) != 128 || 
         DSET_NZ(dset) != 128) {
      /* resample into RAI, assuming that is needed */
      odset = r_new_resam_dset(dset, NULL, 128, 128, 128, 
                               "RAI", MRI_LINEAR, NULL, 1);
      DSET_delete(dset); dset = odset; odset = NULL;
   }
   #endif
   
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
      SUMA_SL_Crit("Failed to allocate.");
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

   NI_set_attribute(nel,"read_status","read");
   
   SUMA_S_Note("dset is not freed anywhere here");
      
 

   SUMA_RETURN(YUP);
}
void SUMA_SetRenderModelView(void)
{
   static char FuncName[]={"SUMA_SetRenderModelView"};

   SUMA_ENTRY;
   /* set the perspective */
   glMatrixMode(GL_PROJECTION);
   glLoadIdentity ();   
   gluPerspective(30.0, 1.0, 1., 900.);
   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   gluLookAt ( vol_center[0], vol_center[1], vol_center[2]+450., 
               vol_center[0], vol_center[1], vol_center[2],
               0., 1., 0.);      
   SUMA_RETURNe;
}

void SUMA_Init3DTextureDSET (char *dsetname)
{ 
   static char FuncName[]={"SUMA_Init3DTextureDSET"};
   
   SUMA_ENTRY;
         
   dset = THD_open_dataset( dsetname );

   DSET_load(dset);

   /* setup global values for dset */
   SUMA_3dTexParameter_setup(dset);

   /* Initialize OpenGL State */
   SUMA_SetRenderModelView();

   SUMA_Enable3DRendering();

   tex3ddata = dset_to_tex3d(dset);

   SUMA_SetTexImage();

   SUMA_CreateSphereList();

   SUMA_RETURNe;
}

SUMA_Boolean SUMA_Init3DTextureNIDOnel (NI_element *nel, 
                                    SUMA_SurfaceObject *SO, 
                                    SUMA_DO_CoordUnits default_coord_type,
                                    float *default_txcol, 
                                    void *default_font,
                                    SUMA_SurfaceViewer *sv)
{
   static char FuncName[]={"SUMA_Init3DTextureNIDOnel"};
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_ENTRY;

   
   if (!nel || strcmp(nel->name,"3DTex")) SUMA_RETURN(NOPE);
   
   if (NI_IS_STR_ATTR_EQUAL(nel,"read_status","fail")) {
      /* can't be read */
      SUMA_RETURN(NOPE);
   }
   
   if (LocalHead) {
      SUMA_S_Note("for debugging force geom to be displayed...");
      geom = 1;
   }
   
   if (!NI_IS_STR_ATTR_EQUAL(nel,"read_status","read")) { /* read it */
      if (!SUMA_Load3DTextureNIDOnel(nel)) {
         SUMA_RETURN(NOPE);
      }
      
       /* setup global values for dset */
       SUMA_3dTexParameter_setup(dset);

       if (sv) {
         sv->SER = SUMA_RecordEnablingState();
         if (LocalHead) SUMA_ShowEnablingState(sv->SER,stderr,
                                 "In SUMA_Init3DTextureNIDOnel "
                                 "before any rendering\n");
       
       } else { 
         #ifdef DO_VOLUME_MAIN
            SUMA_SetRenderModelView();
         #endif
       }
       SUMA_Enable3DRendering(); /* This needs to be done just
                                    once (when dset is first loaded)
                                    if you are just rendering in a window
                                    However, you will need to disable 
                                    (or revert to previous settings) the 
                                    various parameters that interfer with
                                    surface rendering in the same viewer... */
                                 

      tex3ddata = (byte *)nel->vec[0];

      SUMA_SetTexImage();

      SUMA_CreateSphereList();
   } 
   
  
  if (sv) {
      /* set the rendering flag, the redraw function 
      is called from within SUMA_display */
      sv->Do_3Drender = 1; 
   }
  SUMA_RETURN(YUP);
}


#ifdef DO_VOLUME_MAIN
void
reshape(int wid, int ht)
{
    winWidth = wid;
    winHeight = ht;
    glViewport(0, 0, wid, ht);
}


void
motion(int x, int y)
{
    switch(active)
    {
    case OBJ_ANGLE:
	objangle[X] = (x - winWidth/2) * 360./winWidth;
	objangle[Y] = (y - winHeight/2) * 360./winHeight;
	glutPostRedisplay();
	break;
    case SLICES:
	slices = x * tex_num[0]/winWidth;
	glutPostRedisplay();
	break;
    case CUTTING:
	cutbias = (x - winWidth/2) * 300/winWidth;
	glutPostRedisplay();
	break;
    case GEOMXY:
	objpos[X] = (x - winWidth/2) * 300/winWidth;
	objpos[Y] = (winHeight/2 - y) * 300/winHeight;
	glutPostRedisplay();
	break;
    case GEOMZ:
	objpos[Z] = (x - winWidth/2) * 300/winWidth;
	glutPostRedisplay();
	break;
    case MINBOOST:
	minboost = x * .25f/winWidth;
	glutPostRedisplay();
	break;
    case BOOSTWID:
	boostwid = x * .5f/winWidth;
	glutPostRedisplay();
	break;
    case BOOST:
	boost = x * 20.f/winWidth;
	glutPostRedisplay();
	break;
    }
}

void
mouse(int button, int state, int x, int y)
{
    if(state == GLUT_DOWN)
	switch(button)
	{
	case GLUT_LEFT_BUTTON: /* rotate the data volume */
	    if(map)
		active = MINBOOST;
	    else
		active = OBJ_ANGLE;
	    motion(x, y);
	    break;
	case GLUT_MIDDLE_BUTTON:
	    if(map)
		active = BOOSTWID;
	    else
		if(cut)
		    active = CUTTING; /* move cutting plane */
		else
		    active = GEOMXY; /* move geometry */
	    motion(x, y);
	    break;
	case GLUT_RIGHT_BUTTON: /* move the polygon */
	    if(map)
		active = BOOST;
	    else
		if(geom)
		    active = GEOMZ;
		else
		    active = SLICES;
	    motion(x, y);
	    break;
	}
}
/* ARGSUSED1 */
void key(unsigned char key, int x, int y)
{
    switch(key)
    {
    case 'm': /* remap texture values */
	if(map)
	{
	    fprintf(stderr, "remapping off\n");
	    map = GL_FALSE;
	}
	else
	{
	    fprintf(stderr, "remapping on:\n"
		            "left mouse moves emphasize value\n"
		            "middle mouse moves emphasize width\n"
		            "right mouse adjusts gain\n");
	    map = GL_TRUE;
	}

	remaptex();
	glutPostRedisplay();
	break;
   case 'p':
      polmode = !polmode;  /* NO RENDERING without GL_FILL */
      if (polmode) glPolygonMode(GL_FRONT_AND_BACK, GL_FILL);
      else glPolygonMode(GL_FRONT_AND_BACK, GL_LINE);
      glutPostRedisplay();
	   break;
    case 'o':
	operator++;
	if(operator == LASTOP)
	    operator = OVER;
	glutPostRedisplay();
	break;
    case 't':
	if(texture)
	    texture = GL_FALSE;
	else
	    texture = GL_TRUE;
	glutPostRedisplay();
	break;
    case 'c':
	if(cut)
	{
	    fprintf(stderr, "cutting plane off\n");
	    cut = GL_FALSE;
	}
	else
	{
	    fprintf(stderr, 
		    "Cutting plane on: "
		    "middle mouse (horizontal) moves cutting plane\n");
	    cut = GL_TRUE;
	}
	glutPostRedisplay();
	break;
    case 'g': /* toggle geometry */
	if(geom)
	    geom = GL_FALSE;
	else
	    geom = GL_TRUE;
	glutPostRedisplay();
	break;
    case '\033':
	exit(0);
	break;
    case '?':
    case 'h':
    default:
	fprintf(stderr, 
		"Keyboard Commands\n"
		"m - toggle transfer function (remapping)\n"
		"o - toggle operator\n"
		"t - toggle 3D texturing\n"
		"c - toggle cutting plane\n"
		"g - toggle geometry\n");
	break;
    }
}



void SUMA_Standalone_GLUT_Window_Init(void)
{
   static char FuncName[]={"SUMA_Standalone_GLUT_Window_Init"};
   
   SUMA_ENTRY;
   
   if(dblbuf)
      glutInitDisplayMode(GLUT_RGBA|GLUT_DEPTH|GLUT_DOUBLE);
   else
      glutInitDisplayMode(GLUT_RGBA|GLUT_DEPTH);

   (void)glutCreateWindow("volume rendering demo");
   glutDisplayFunc(SUMA_3DTex_redraw);
   glutReshapeFunc(reshape);
   glutMouseFunc(mouse);
   glutMotionFunc(motion);
   glutKeyboardFunc(key);

   SUMA_RETURNe;
}

main(int argc, char *argv[])
{
   static char FuncName[]={"volume"};
   int ip;
   static GLfloat splane[4] = {1.f/200.f, 0.f, 0.f, .5f};
   static GLfloat rplane[4] = {0, 1.f/200.f, 0, .5f};
   static GLfloat tplane[4] = {0, 0, 1.f/200.f, .5f};
   static GLfloat lightpos[4] = {150., 150., 150., 1.f};
   SUMA_NIDO *SDO = NULL;
   NI_element *nel=NULL;
   SUMA_Boolean LocalHead = YUP;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);

   #ifndef GL_EXT_texture3D
      SUMA_S_Err("Program needs GL_EXT_texture3D");
      exit(1);
   #endif
    glutInit(&argc, argv);
    glutInitWindowSize(winWidth, winHeight);
   
   nel = NULL;
   if (SUMA_GuessFormatFromExtension_core(argv[1]) == SUMA_NIML) {
      SDO = SUMA_ReadNIDO (argv[1], NULL);
      /* loop through the elements to find 1st 3DTex if any */
      nel = NULL;
      for( ip=0 ; ip < SDO->ngr->part_num && !nel; ip++ ){ 
         switch( SDO->ngr->part_typ[ip] ){
            /*-- a sub-group ==> recursion! --*/
            case NI_GROUP_TYPE:
               break ;
            case NI_ELEMENT_TYPE:
               nel = (NI_element *)SDO->ngr->part[ip] ;
               if (strcmp(nel->name,"3DTex")) nel = NULL;
               break;
            default:
               SUMA_SL_Err(
                  "Don't know what to make of this group element, ignoring.");
               break;
         }
      }
   } else {
      /* brick will be loaded below */
   }

   /* setup rendering vindow and callbacks */
   
   SUMA_Standalone_GLUT_Window_Init();
    if (!nel) {
      SUMA_Init3DTextureDSET (argv[1]);
    } else {
      SUMA_Init3DTextureNIDOnel (nel, 
                                 NULL, 
                                 0,
                                 NULL, 
                                 NULL,
                                 NULL);
    }
    key('?', 0, 0); /* print usage message */

    CHECK_ERROR("end of main");

    
    if(!glutExtensionSupported("GL_EXT_texture3d")) {
      /* this one gives a false warning. texture 3d is in newer OpenGL */      
      /* fprintf(stderr,
        "volume: requires OpenGL texture 3D extension to operate correctly.\n");          */
    }
    hasBlendColor = glutExtensionSupported("GL_EXT_blend_color");
    if(!hasBlendColor) {
      fprintf(stderr,
        "volume: needs OpenGL blend color extension to attenuate.\n");
    }

    glutMainLoop();
    return 0;             /* ANSI C requires main to return int. */
}

#endif
