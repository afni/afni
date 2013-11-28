#include "SUMA_suma.h"

static char names[12][8]={"Dx", "Dy", "Dz", 
                          "Rx", "Ry", "Rz", 
                          "Kx", "Ky", "Kz", 
                          "Sx", "Sy", "Sz"};

typedef struct {
   SUMA_SurfaceObject *SOr;
   float *xyz;
   int N_xyz;
   byte *cmask;
   double *aff;
   byte   *affm;
   int method;
   float *dist;
   float *XYZ;
   int N_XYZ;
   double cost;
   byte city;
} CMO_UD; /* user data for SUMA_CoordMatch_OptimCost */

static CMO_UD cmoud;
static int debug = 1;

void SUMA_free_cmoud() {
   if (cmoud.dist) SUMA_free(cmoud.dist); cmoud.dist = NULL;
   if (cmoud.XYZ) SUMA_free(cmoud.XYZ); cmoud.XYZ = NULL;
   return;
}

void SUMA_set_cmoud(SUMA_SurfaceObject *SOr,
                    float *xyz, int N_xyz,
                    byte *cmask, 
                    double *aff, byte *affm,
                    int method, byte city) {
   static char FuncName[]={"SUMA_set_cmoud"};
   
   SUMA_ENTRY;

   
   SUMA_free_cmoud();
   cmoud.SOr = SOr;
   cmoud.xyz = xyz;
   cmoud.N_xyz = N_xyz;
   cmoud.cmask = cmask;
   cmoud.aff = aff;
   cmoud.affm = affm;
   cmoud.method = method;
   cmoud.dist = NULL;
   cmoud.XYZ = NULL;
   cmoud.N_XYZ=-1;
   cmoud.cost = 0.0;
   cmoud.city=city;
   
   if (debug) {
      fprintf(stderr,"Have surface %s and %d points in xyz vector\n",
                     SOr->Label, N_xyz);
   }
   SUMA_RETURNe;
}


double SUMA_CoordMatchEnergy(SUMA_SurfaceObject *SOr,
                       float *xyz, int N_xyz,
                       double *aff, byte *maff,
                       int method, float *dist, byte city)
{
   static char FuncName[]={"SUMA_CoordMatchEnergy"};
   double ener=0.0;
   int pp=0;
   
   SUMA_ENTRY;
   
   if (!SUMA_Shortest_Point_To_Triangles_Distance(
            xyz, N_xyz, 
            SOr->NodeList, SOr->FaceSetList, SOr->N_FaceSet,
            SOr->FaceNormList, &dist, NULL, NULL, city )) {
         SUMA_S_Err("Failed to get shortys");
         SUMA_RETURN(ener);     
   }
   
   ener=0.0;
   for (pp=0; pp<N_xyz; ++pp) {
      if (dist[pp]<0) {
         SUMA_S_Warnv("Dist should not be negative, %f at node %d\n",
                        dist[pp], pp);
         dist[pp]=SUMA_ABS(dist[pp]);
      }
      if (city) ener += (dist[pp]); 
      else ener += sqrt(dist[pp]);
      dist[pp] = -1.0; /* reinitialize for next call */
   }
   #if 1
      fprintf(stderr,"ener = %f/%d\n", ener, N_xyz);
   #endif
   ener/=(double)N_xyz;
   
   SUMA_RETURN(ener);   
} 

#define ADD_SHIFT(par, mat) { \
   mat[0][3]+=par[0];   \
   mat[1][3]+=par[1];   \
   mat[2][3]+=par[2];   \
}
#define ADD_ROTX(par, mat) {  \
   static double m_dd[3][3],m_m[3][3];   \
   m_dd[0][0]=1.0;          m_dd[0][1]=0.0;            m_dd[0][2]=0.0;  \
   m_dd[1][0]=0.0;          m_dd[1][1]=cos(par[0]);    m_dd[1][2]=-sin(par[0]);\
   m_dd[2][0]=0.0;          m_dd[2][1]=sin(par[0]);    m_dd[2][2]= cos(par[0]);\
   SUMA_MULT_MAT(mat,m_dd, m_m,3,3,3,double,double,double); \
   COPY_MAT33(mat,m_m); \
}   
#define ADD_ROTY(par, mat) {  \
   static double m_dd[3][3],  m_m[3][3];   \
   m_dd[0][0]=1.0;          m_dd[0][1]=0.0;            m_dd[0][2]=0.0;  \
   m_dd[1][0]=0.0;          m_dd[1][1]=cos(par[0]);    m_dd[1][2]=-sin(par[0]);\
   m_dd[2][0]=0.0;          m_dd[2][1]=sin(par[0]);    m_dd[2][2]= cos(par[0]);\
   SUMA_MULT_MAT(mat,m_dd, m_m,3,3,3,double,double,double);  \
   COPY_MAT33(mat,m_m); \
}   
#define ADD_ROTZ(par, mat) {  \
   static double m_dd[3][3], m_m[3][3];   \
   m_dd[0][0]=cos(par[0]);  m_dd[0][1]=-sin(par[0]);   m_dd[0][2]=0.0;  \
   m_dd[1][0]=sin(par[0]);  m_dd[1][1]= cos(par[0]);   m_dd[1][2]=0.0;\
   m_dd[2][0]=0.0;          m_dd[2][1]= 0.0;           m_dd[2][2]=1.0;\
   SUMA_MULT_MAT(mat,m_dd, m_m,3,3,3,double,double,double);  \
   COPY_MAT33(mat,m_m); \
}

/* mat = Rx*Ry*Rz*mat if repl = 0
   mat = Rx*Ry*Rz     if repl = 1*/
#define ADD_ROTXYZ(par, mat, repl) {   \
   static double m_dd[3][3], m_m[3][3], cx, cy, cz, sx, sy, sz;   \
   cx=cos(par[0]); sx=sin(par[0]);   \
   cy=cos(par[1]); sy=sin(par[1]);   \
   cz=cos(par[2]); sz=sin(par[2]);   \
   /* RxRyRz */   \
   m_dd[0][0]=cy*cz;  m_dd[0][1]=cy*sz;  m_dd[0][2]=sy; \
   m_dd[1][0]=sx*sy*cz+cx*sz; m_dd[1][1]=-sx*sy*sz+cx*cz; m_dd[1][2]=-sx*cy;  \
   m_dd[2][0]=-cx*sy*cz+sx*sz; m_dd[2][1]=cx*sy*sz+sx*cz; m_dd[2][2]=cx*cy;   \
   if (repl) { \
      mat[0][0]=m_dd[0][0]; mat[0][1]=m_dd[0][1]; mat[0][2]=m_dd[0][2];\
      mat[1][0]=m_dd[1][0]; mat[1][1]=m_dd[1][1]; mat[1][2]=m_dd[1][2];\
      mat[2][0]=m_dd[2][0]; mat[2][1]=m_dd[2][1]; mat[2][2]=m_dd[2][2];\
   } else {  \
      SUMA_MULT_MAT(mat,m_dd, m_m,3,3,3,double,double,double);  \
      COPY_MAT33(mat,m_m); \
   }  \
}
/* mat = Rz*Ry*Rx*mat if repl = 0
   mat = Rz*Ry*Rx     if repl = 1*/
#define ADD_ROTZYX(par, mat, repl) {   \
   static double m_dd[3][3], m_m[3][3], cx, cy, cz, sx, sy, sz;   \
   cx=cos(par[0]); sx=sin(par[0]);   \
   cy=cos(par[1]); sy=sin(par[1]);   \
   cz=cos(par[2]); sz=sin(par[2]);   \
   /* RzRyRx */   \
   m_dd[0][0]=cz*cy; m_dd[0][1]=cz*sy*sx-sz*cx;  m_dd[0][2]=cz*sy*cx+sz*sx; \
   m_dd[1][0]=sz*cy; m_dd[1][1]=sz*sy*sx+cz*cx;  m_dd[1][2]=sz*sy*cx-cz*sx;  \
   m_dd[2][0]=-sy; m_dd[2][1]=cy*sx; m_dd[2][2]=cy*cx;   \
   if (repl) { \
      mat[0][0]=m_dd[0][0]; mat[0][1]=m_dd[0][1]; mat[0][2]=m_dd[0][2];\
      mat[1][0]=m_dd[1][0]; mat[1][1]=m_dd[1][1]; mat[1][2]=m_dd[1][2];\
      mat[2][0]=m_dd[2][0]; mat[2][1]=m_dd[2][1]; mat[2][2]=m_dd[2][2];\
   } else {  \
      SUMA_MULT_MAT(mat,m_dd, m_m,3,3,3,double,double,double);  \
      COPY_MAT33(mat,m_m); \
   }  \
}

#define ADD_SCALE(par, mat, repl) {   \
   if (repl) { \
      mat[0][0]=par[0]; mat[0][1]=0.0;    mat[0][2]=0.0;\
      mat[1][0]=0.0;    mat[1][1]=par[1]; mat[1][2]=0.0;\
      mat[2][0]=0.0;    mat[2][1]=0.0;    mat[2][2]=par[2];\
   } else {  \
      mat[0][0] *= par[0]; mat[0][1] *= par[0]; mat[0][2] *= par[0]; \
      mat[1][0] *= par[1]; mat[1][1] *= par[1]; mat[1][2] *= par[1]; \
      mat[2][0] *= par[2]; mat[2][1] *= par[2]; mat[2][2] *= par[2]; \
   }  \
}

#define ADD_SHEAR(par, mat, repl) {   \
   static double m_dd[3][3], m_m[3][3];   \
   m_dd[0][0]=1.0;    m_dd[0][1]=par[0];  m_dd[0][2]=par[0]; \
   m_dd[1][0]=par[1]; m_dd[1][1]=1.0;     m_dd[1][2]=par[1];  \
   m_dd[2][0]=par[2]; m_dd[2][1]=par[2];  m_dd[2][2]=1.0;   \
   if (repl) { \
      mat[0][0]=m_dd[0][0]; mat[0][1]=m_dd[0][1]; mat[0][2]=m_dd[0][2];\
      mat[1][0]=m_dd[1][0]; mat[1][1]=m_dd[1][1]; mat[1][2]=m_dd[1][2];\
      mat[2][0]=m_dd[2][0]; mat[2][1]=m_dd[2][1]; mat[2][2]=m_dd[2][2];\
   } else {  \
      SUMA_MULT_MAT(mat,m_dd, m_m,3,3,3,double,double,double);  \
      COPY_MAT33(mat,m_m); \
   }  \
}

#define INIT_MAT(mat) { \
   mat[0][0]=1.0; mat[0][1]=0.0; mat[0][2]=0.0; mat[0][3]=0.0; \
   mat[1][0]=0.0; mat[1][1]=1.0; mat[1][2]=0.0; mat[1][3]=0.0; \
   mat[2][0]=0.0; mat[2][1]=0.0; mat[2][2]=1.0; mat[2][3]=0.0; \
   mat[3][0]=0.0; mat[3][1]=0.0; mat[3][2]=0.0; mat[3][3]=1.0; \
}

#define COPY_MAT44(m1,m2) { \
   m1[0][0]=m2[0][0]; m1[0][1]=m2[0][0]; m1[0][2]=m2[0][0]; m1[0][3]=m2[0][0]; \
   m1[1][0]=m2[1][0]; m1[1][1]=m2[1][0]; m1[1][2]=m2[1][0]; m1[1][3]=m2[1][0]; \
   m1[2][0]=m2[2][0]; m1[2][1]=m2[2][0]; m1[2][2]=m2[2][0]; m1[2][3]=m2[2][0]; \
   m1[3][0]=m2[3][0]; m1[3][1]=m2[3][0]; m1[3][2]=m2[3][0]; m1[3][3]=m2[3][0]; \
}

#define COPY_MAT33(m1,m2) { \
   m1[0][0]=m2[0][0]; m1[0][1]=m2[0][0]; m1[0][2]=m2[0][0]; \
   m1[1][0]=m2[1][0]; m1[1][1]=m2[1][0]; m1[1][2]=m2[1][0]; \
   m1[2][0]=m2[2][0]; m1[2][1]=m2[2][0]; m1[2][2]=m2[2][0]; \
}

#define SHOW_MAT AFF44_SHOW

int SUMA_par2mat(double *par12, double mat[4][4])
{
   static char FuncName[]={"SUMA_par2mat"};
   
   SUMA_ENTRY;
   
   /* all masked parameters should be left to 0 */
   INIT_MAT(mat);

   /* load the three rotations */
   ADD_ROTZYX((par12+3), mat, 1);

   /* append the scale */
   ADD_SCALE((par12+6), mat, 0);

   #if 0 /* Causing crash, see why */
   /* append the shear */
   ADD_SHEAR((par12+9), mat, 0);
   #endif
   /* load the shift */
   ADD_SHIFT(par12,mat);
      
   if (debug > 1) {
      SHOW_MAT(mat, "par2mat");
   }
   SUMA_RETURN(1);
}

double SUMA_CoordMatch_OptimCost(int n, double *par) 
{
   static char FuncName[]={"SUMA_CoordMatch_OptimCost"};
   static int iter;
   int i, k;
   double x, y, z;
   double mat[4][4];
   char *s=NULL;
   
   SUMA_ENTRY;
   
   /* put parameters into cs */
   for (i=0, k=0; i<12; ++i) {
      if (cmoud.affm[i]) cmoud.aff[i] = par[k++];
      else if (i>=6 && i<9) cmoud.aff[i]=1.0;
      else cmoud.aff[i] = 0.0;
      if (debug > 2) {
         fprintf(stderr,"affm[%d]=%d, aff[%d]=%f\n",
                     i, cmoud.affm[i], i, cmoud.aff[i]);
      }
   }
   
   /* form the affine matrix from 12 parameters*/
   SUMA_par2mat(cmoud.aff, mat);
   
   if (!cmoud.XYZ) 
      cmoud.XYZ = (float *)SUMA_malloc(3*cmoud.N_xyz* sizeof(float)); 
   
   /* transform the coordinates */
   i=0; k=0; cmoud.N_XYZ=0;
   while(i<3*cmoud.N_xyz) {
      if (!cmoud.cmask || cmoud.cmask[i/3]) {
         x = cmoud.xyz[i++]; y = cmoud.xyz[i++]; z = cmoud.xyz[i++];
         cmoud.XYZ[k++] = (float) (  mat[0][0] * x + 
                                     mat[0][1] * y + 
                                     mat[0][2] * z +
                                     mat[0][3] );
         cmoud.XYZ[k++] = (float) (  mat[1][0] * x + 
                                     mat[1][1] * y + 
                                     mat[1][2] * z +
                                     mat[1][3] );
         cmoud.XYZ[k++] = (float) (  mat[2][0] * x + 
                                     mat[2][1] * y + 
                                     mat[2][2] * z +
                                     mat[2][3] );
         ++cmoud.N_XYZ;
      } else {
         i += 3;
      }
   }
   
   if (debug > 2) {
      s = SUMA_ShowMeSome(cmoud.XYZ, SUMA_float, cmoud.N_XYZ, 10, NULL);
      SUMA_S_Notev("iter %d, xformed coords: %s\n", iter, s);
      SUMA_free(s); s = NULL;
   }
   cmoud.cost =  SUMA_CoordMatchEnergy(cmoud.SOr,
                       cmoud.XYZ, cmoud.N_XYZ,
                       cmoud.aff, cmoud.affm,
                       cmoud.method, cmoud.dist, cmoud.city);
   
   if (debug==1) {
      fprintf(SUMA_STDERR,"%cMethod %d. iter %d, %d points, Coord Cost %f%c", 
            0xd, cmoud.method, iter, cmoud.N_XYZ, cmoud.cost, iter?'\0':'\n'); 
   } else if (debug > 1) {
      fprintf(SUMA_STDERR,"%cMethod %d. iter %d, %d points, Coord Cost %f%c", 
            '\n', cmoud.method, iter, cmoud.N_XYZ, cmoud.cost, iter?'\0':'\n');
         fprintf(SUMA_STDERR,"   Params: ");
      for(i=0; i<n; ++i) {
         fprintf(SUMA_STDERR,"%f   ",par[i]);
      } 
         fprintf(SUMA_STDERR,"\n");
   }
   
   ++iter; 
   SUMA_RETURN(cmoud.cost);
}

double SUMA_AlignCoords(float *xyz, int N_xyz, byte *cmask, int method, 
                        SUMA_SurfaceObject *SOr, char *opt)
{
   static char FuncName[]={"SUMA_AlignCoords"};
   int ncalls = 0, npar, i, k, nrand, ntry, nkeep, maxcall, nparmax=12;
   float x,y,z,*xr, *yr,*zr;
   double par[nparmax], bot[nparmax], top[nparmax], costf,
          gap[nparmax], rstart, rend, aff[12], mat[4][4];
   byte affm[nparmax];
   static int icall = 0;
   SUMA_Boolean LocalHead = NOPE;   
   
   SUMA_ENTRY;
   
   
   npar = 0;        
   memset(affm, 0, nparmax*sizeof(byte));
   if (strstr(opt,"shft")) {
      npar += 3;
      affm[0]=1; affm[1]=1; affm[2]=1;
   }
   if (strstr(opt,"rot")) {
      npar += 3;
      affm[3]=1; affm[4]=1; affm[5]=1;
   } 
   if (strstr(opt,"scl")) {
      npar += 3;
      affm[6]=1; affm[7]=1; affm[8]=1;
   }
   if (strstr(opt,"shr")) {
      npar += 3;
      affm[9]=1; affm[10]=1; affm[11]=1;
   }

   /* load user data */
   SUMA_set_cmoud(SOr, xyz, N_xyz, cmask, aff, affm, method, 
                  strstr(opt, "City")?1:0);

   
   /* load parameters into par, bot, top */
   for (i=0, k=0; i<12; ++i) {
      if (affm[i]) {
         par[k] = 0.0;
         if (i>=0 && i<3) {
            bot[k] = -40;
            top[k] = +40;
         }  else if (i>=3 && i<6) {
            bot[k] = -15*SUMA_PI/180.0;
            top[k] = 15*SUMA_PI/180.0;
         } else if (i>=6 && i<9) {
            bot[k] = 0.5;
            top[k] = 1.5;
         } else if (i>=9 && i<12) {
            bot[k] = -0.1;
            top[k] = 0.1;
         }
         ++k;
      }
   }
   
   
   if (debug) {
      for (i=0, k=0; i<12; ++i) {
         if (affm[i]) {
            if (!k) fprintf(SUMA_STDERR, "Pre Optimization:\n");
            fprintf(SUMA_STDERR, 
         "%s [%.3f <- %.3f -> %.3f]\n",
            names[i], bot[k  ], par[k  ], top[k  ]);
            ++k;
         }
      }
   }

   
   nrand = 0; nkeep = 0; ntry = 2;
   rstart = 0.2; rend = 0.05;
   maxcall = 500;
   #if 0
   if ( (ncalls = powell_newuoa_constrained (npar, par, &costf,
                                             bot, top, 
                                             nrand, nkeep, 2,
                                             rstart, rend,
                                             maxcall,
                                    SUMA_CoordMatch_OptimCost)) < 0) {
      SUMA_S_Err("Failed in optimization");
      SUMA_RETURN(0);
   }
   #else
   if (debug) { 
      fprintf(SUMA_STDERR,"About to being optimization\n");
   }
   if ( (ncalls = powell_newuoa_con (npar, par,
                                       bot, top, 
                                       nrand, 
                                       rstart, rend,
                                       maxcall,
                                    SUMA_CoordMatch_OptimCost)) < 0) {
      SUMA_S_Err("Failed in optimization");
      SUMA_RETURN(0);
   }
   if (debug) { 
      fprintf(SUMA_STDERR,"Done with optimization\n");
   }
   costf = cmoud.cost;   
   #endif
   if (debug) fprintf(SUMA_STDERR,"\n");
   
   if (debug) {
      for (i=0, k=0; i<12; ++i) {
         if (affm[i]) {
            fprintf(SUMA_STDERR,
               "Post Optimization:\n"
         "%s  [%.3f <- %.3f -> %.3f]\n",
            names[i],       
            bot[k  ], par[k  ], top[k  ]);
            ++k;
         }
      }
      fprintf(SUMA_STDERR,"   Final cost %f\n", 
              SUMA_CoordMatch_OptimCost(npar, par));
   }
   
   /* Now apply the final transform */
   SUMA_par2mat(cmoud.aff, mat);
   i=0; k=0; 
   while(i<3*N_xyz) {
      xr = xyz+i; x = xyz[i++]; 
      yr = xyz+i; y = xyz[i++]; 
      zr = xyz+i; z = xyz[i++];
      *xr = (float) (   mat[0][0] * x + 
                        mat[0][1] * y + 
                        mat[0][2] * z +
                        mat[0][3] );
      *yr = (float) (   mat[1][0] * x + 
                        mat[1][1] * y + 
                        mat[1][2] * z +
                        mat[1][3] );
      *zr = (float) (   mat[2][0] * x + 
                        mat[2][1] * y + 
                        mat[2][2] * z +
                        mat[2][3] );
   }

   SUMA_free_cmoud();
   
   ++icall;
   SUMA_RETURN(costf);
}
