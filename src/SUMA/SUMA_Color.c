#include "SUMA_suma.h"
#define MAIN     /* need this to read in color info from afni.h */
#include "../afni.h"
#include "../pbardefs.h"
#undef MAIN
  

extern SUMA_CommonFields *SUMAg_CF;
extern SUMA_DO *SUMAg_DOv;
extern SUMA_SurfaceViewer *SUMAg_SVv;
extern int SUMAg_N_SVv; 
extern int SUMAg_N_DOv;  

/*! The set of functions deals with node colors
*/



/*! 
This function creates an RGB colormap containing Ncols that vary linearily 
   from the first color in Fiducials to the last.
   
   SM = SUMA_MakeColorMap (Fiducials, N , isRGBA, N_cols, SkipLast, Name)
   
   \param Fiducials (float **) N x 3, or 4 matrix containing RGB or RGBA 
               values (range 0..1) 
          of fiducial colours which will be equally spaced on the color map
   \param N (int) number of fiducial colors in Fiducials
   \param isRGBA if 1 then Fiducials are RGBA else RGB
   \param Ncols (int) total number of colors in the map.
         You are somewhat restricted in the total number of 
         colours you choose. You must choose a number that
         allows you to have the same number of colours between
         successive fiducials. The function will complain if that's not the case
   \param SkipLast (SUMA_Boolean) if set to NOPE (0), then the last color specified in
          Fiducials is the last color in M. If set to 1, the last 
          color in M represents the color that would come right 
          before the last one in Fifucials. This last option is
          usefull when you're crating cyclical color maps where
          the last color in Fiduciasl is like the first. 
   \param Name (char *) name of colormap
   \ret SM (SUMA_COLOR_MAP *) see help for SUMA_COLOR_MAP for more info
   
   \sa based on my matlab function MakeColorMap.m
   \sa SUMA_Free_ColorMap
   \sa SUMA_MakeColorMap_v2
   
*/
SUMA_COLOR_MAP* SUMA_MakeColorMap ( float **Fiducials, int Nfid, byte isRGBA, 
                                    int Ncols, 
                                    SUMA_Boolean SkipLast, char *Name)
{
   static char FuncName[]={"SUMA_MakeColorMap"};
   float **M, dFid[4];
   int i, j, Ninter, Ngap, im, Ncolsgood, Npergap;
   int Nrows;
   SUMA_COLOR_MAP * SM;
   
   SUMA_ENTRY;
   
   if (isRGBA == 1) Nrows = 4;
   else if (isRGBA == 0)  Nrows = 3;
   else {
      SUMA_S_Errv("Not too good there %d\n", (int)isRGBA);
      SUMA_RETURN (NULL);
   }
   /* check for bad input */
   for (i=0; i < Nfid; ++i) {
      for (j=0; j < Nrows; ++j) {
         if (Fiducials[i][j] < 0 || Fiducials[i][j] > 1) {
            fprintf (SUMA_STDERR,
                     "Error %s: Fiducial colors must be between 0 & 1 "
                     "(found %f)\n", FuncName, Fiducials[i][j]);
            SUMA_RETURN (NULL);
         }
      }
   }
   /* determine the number of intermediate colors */
   if (SkipLast) Ninter = Ncols - (Nfid - 1);
   else Ninter = Ncols - Nfid;
   
   Ngap = Nfid - 1;
   
   /* you must have an equal number of colours in each gap */ 
   if (Ninter % Ngap) {
      /* bad, sugeest a better number */
      if (SkipLast) 
         Ncolsgood = (int)(rint((float)Ninter/Ngap) * Ngap + Nfid + 1);
      else Ncolsgood = (int)(rint((float)Ninter/Ngap) * Ngap + Nfid);
      
      fprintf (SUMA_STDERR,
               "Error %s: The choice of Ncols does not work with the number\n"
               "of fiducial colours.\nTry Ncols = %d\n", 
               FuncName, Ncolsgood); 
      SUMA_RETURN (NULL);
   }
   
   /* allocate for M */
   M = (float **)SUMA_allocate2D (Ncols, 4, sizeof(float));
   if (M == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for M.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* start forming M */
   im = 0;
   Npergap = Ninter / Ngap;
   
   for (i=0; i < Ngap; ++i) {
      dFid[0] = (Fiducials[i+1][0] - Fiducials[i][0])/(Npergap+1);
      dFid[1] = (Fiducials[i+1][1] - Fiducials[i][1])/(Npergap+1);
      dFid[2] = (Fiducials[i+1][2] - Fiducials[i][2])/(Npergap+1);
      if (Nrows == 4) {
      dFid[3] = (Fiducials[i+1][3] - Fiducials[i][3])/(Npergap+1);
      } else dFid[3] = 1.0;
      /*fprintf (SUMA_STDERR,"%s:  dFid = %f %f %f %f\n", 
         FuncName, dFid[0], dFid[1] , dFid[2], dFid[3]);*/
      
      for (j=0; j < Npergap+1; ++ j) {

         if (im < Ncols) {
            M[im][0] = Fiducials[i][0] + j*dFid[0];
            M[im][1] = Fiducials[i][1] + j*dFid[1];
            M[im][2] = Fiducials[i][2] + j*dFid[2];
            if (Nrows == 4) {
            M[im][3] = Fiducials[i][3] + j*dFid[3];
            } else M[im][3] = 1.0;
            /*fprintf (SUMA_STDERR,"%s: M[%d][:] = %f %f %f %f\n", 
                        FuncName, im, M[im][0], M[im][1], M[im][2], M[im][3]); */
         }
               
         ++im;
      }
   }
   if (!SkipLast) {
      if (im >= Ncols) {
         SUMA_S_Crit("Unexpected logic error");
      } else {
         M[im][0] = Fiducials[Ngap][0];
         M[im][1] = Fiducials[Ngap][1];
         M[im][2] = Fiducials[Ngap][2];
         if (Nrows == 4) {
         M[im][3] = Fiducials[Ngap][3];
         } else M[im][3] = 1.0;
      }
   }

   /* package the resutls */
   SM = (SUMA_COLOR_MAP *)SUMA_calloc(1,sizeof(SUMA_COLOR_MAP));
   if (SM == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for SM.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   memset(SM, 0, sizeof(SUMA_COLOR_MAP));
   SM->chd = NULL;
   SM->idvec = NULL;
   SM->top_frac = 0.0f;
   SM->Name = (char *)SUMA_calloc(strlen(Name)+1, sizeof(char));
   if (SM->Name == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for SM->Name.\n", 
               FuncName);
      SUMA_RETURN (NULL);
   }
   sprintf(SM->Name, "%s",Name); 
   SM->M = M;
   for (j=0; j< 4; ++j) SM->M0[j] = SM->M[0][j];  
   SM->N_M[0] = Ncols;
   SM->N_M[1] = 4;
   SM->frac = NULL; /* a linear map */
   SM->cname = NULL;
   SM->Sgn = 0; /* setup for linear maps with no signing, 
                  mapping a la old ScaleToMap*/
   SM->SO = NULL; /* created when needed */

   SUMA_RETURN (SM);
}

/*! 
This function creates an RGB colormap containing Ncols that vary linearily 
   from the first color in Fiducials to the last.
   
   SM = SUMA_MakeColorMap_v2 (Fiducials, NFid, isRGBA, Nin , SkipLast, Name)
   
   \param Fiducials (float **) NFid x 3 (or 4) matrix containing RGB 
               (or RGBA) values (range 0..1) 
          of fiducial colours which will be equally spaced on the color map
   \param NFid (int) number of fiducial colors
   \param isRGBA if 1 then Fiducials are RGBA else RGB
   \param Nin (int*) NFid x 1 vector indicating the number of interpolations 
          to perform
           between successive colors e.g.:
              Fiducials   Nin
            1 0 0         2
            0 1 0         5
            0 0 1         0
            The map will start with 1 0 0 then place 2 colors interpolated 
            between 1 0 0 and 0 1 0. The last color of this map is 0 0 1.
            If the last entry in Nin was 4 then 4 colors are added after 0 0 1 wich 
            are interpolated between the last and 1st color. This is good
            for cyclical color maps
   \param SkipLast (SUMA_Boolean) YUP/NOPE keep last color in Fiducials out of the final map
   \param Name (char *) name of colormap
   \ret SM (SUMA_COLOR_MAP *) see help for SUMA_COLOR_MAP for more info
   
   \sa SUMA_Free_ColorMap
   \sa SUMA_MakeColorMap
*/

SUMA_COLOR_MAP* SUMA_MakeColorMap_v2 (float **Fiducials, int Nfid, byte isRGBA, 
                                      int *Nint, SUMA_Boolean SkipLast, 
                                      char *Name)
{
   static char FuncName[]={"SUMA_MakeColorMap_v2"};
   float **M, dFid[4]={1.0, 1.0, 1.0, 1.0};
   int i, j, im, Ncols, Nrows;
   SUMA_COLOR_MAP * SM;
   
   SUMA_ENTRY;
   
   if (isRGBA == 1) Nrows = 4;
   else if (isRGBA == 0)  Nrows = 3;
   else {
      SUMA_S_Errv("Not too good there %d\n", (int)isRGBA);
      SUMA_RETURN (NULL);
   }
   
   /* check for bad input and calculate the total number of colors*/
   if (Nint[0]) {
      fprintf (SUMA_STDERR,
         "Error %s: The index of the first color (%d) must be 0, \n"
         "indexing starts at 0 not 1.\n", FuncName, Nint[0]);
      SUMA_RETURN (NULL);
   }
   for (i=0; i < Nfid; ++i) {
      for (j=0; j < Nrows; ++j) {
         if (Fiducials[i][j] < 0 || Fiducials[i][j] > 1) {
            fprintf (SUMA_STDERR,
               "Error %s: Fiducial colors must be between 0 & 1 (found %f)\n", 
                  FuncName, Fiducials[i][j]);
            SUMA_RETURN (NULL);
         }
      }
   }
   
   Ncols = Nint[Nfid-1]+1;
   
   if (SkipLast) Ncols = Ncols - 1;
      
   /* allocate for M */
   M = (float **)SUMA_allocate2D (Ncols, 4, sizeof(float));
   if (M == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for M.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* start forming M */
   im = 0;   
   for (i=0; i < Nfid-1; ++i) {
         dFid[0] = (Fiducials[i+1][0] - Fiducials[i][0])/(Nint[i+1]-Nint[i]);
         dFid[1] = (Fiducials[i+1][1] - Fiducials[i][1])/(Nint[i+1]-Nint[i]);
         dFid[2] = (Fiducials[i+1][2] - Fiducials[i][2])/(Nint[i+1]-Nint[i]);
         if (Nrows==4) {
         dFid[2] = (Fiducials[i+1][3] - Fiducials[i][3])/(Nint[i+1]-Nint[i]);
         } else dFid[3] = 1.0;
         /*fprintf (SUMA_STDERR,"%s:  dFid = %f %f %f %f\n", 
                     FuncName, dFid[0], dFid[1] , dFid[2], dFid[3]);*/

         for (j=0; j < (Nint[i+1]- Nint[i]); ++ j) {
               M[im][0] = Fiducials[i][0] + j*dFid[0];
               M[im][1] = Fiducials[i][1] + j*dFid[1];
               M[im][2] = Fiducials[i][2] + j*dFid[2];
               if (Nrows==4) {
               M[im][3] = Fiducials[i][3] + j*dFid[3];
               } else M[im][3] = 1.0;
               /*fprintf (SUMA_STDERR,"%s: M[%d][:] = %f %f %f %f\n", 
                       FuncName, im, M[im][0], M[im][1], M[im][2], M[im][3]); */
            ++im;
         }
   }
   
   if (!SkipLast){
      M[im][0] = Fiducials[Nfid-1][0];
      M[im][1] = Fiducials[Nfid-1][1];
      M[im][2] = Fiducials[Nfid-1][2];
      if (Nrows==4) {
      M[im][3] = Fiducials[Nfid-1][3];
      } else M[im][3] = 1.0;
   }
   
   /* package the resutls */
   SM = (SUMA_COLOR_MAP *)SUMA_calloc(1,sizeof(SUMA_COLOR_MAP));
   if (SM == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for SM.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   memset(SM, 0, sizeof(SUMA_COLOR_MAP));
   SM->idvec = NULL;
   SM->chd = NULL;
   SM->top_frac = 0.0f;
   SM->Name = (char *)SUMA_calloc(strlen(Name)+1, sizeof(char));
   if (SM->Name == NULL) {
      fprintf (SUMA_STDERR,
               "Error %s: Failed to allocate for SM->Name.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   sprintf(SM->Name, "%s",Name); 
   SM->M = M;
   for (j=0; j< 4; ++j) SM->M0[j] = SM->M[0][j]; 
   SM->N_M[0] = Ncols;
   SM->N_M[1] = 4;
   
   SM->frac = NULL; /* a linear map */
   SM->cname = NULL;
   SM->Sgn = 0; /* setup for linear maps with no signing, 
                     mapping a la old ScaleToMap*/
   SM->SO = NULL; /* created when needed */
   SUMA_RETURN (SM);
}

/*! function to free memory allocated for SUMA_COLOR_MAP */
void SUMA_Free_ColorMap (SUMA_COLOR_MAP* SM)
{
   static char FuncName[]={"SUMA_Free_ColorMap"};
   int i = 0;
   
   SUMA_ENTRY;

   if (SM->Name) SUMA_free(SM->Name);
   if (SM->M) SUMA_free2D((char **)SM->M, SM->N_M[0]);
   if (SM->cname) {
      for (i=0; i<SM->N_M[0]; ++i) { if (SM->cname[i]) SUMA_free(SM->cname[i]); }
      SUMA_free(SM->cname);
   }
   if (SM->frac) SUMA_free(SM->frac);
   if (SM->SO) SUMA_Free_Surface_Object(SM->SO); 
   if (SM->idvec) SUMA_free(SM->idvec);
   if (SM->chd) SUMA_DestroyCmapHash(SM);
   if (SM) SUMA_free(SM);

   SUMA_RETURNe;
}

/* based on AFNI's PBAR_define_bigmap */
SUMA_COLOR_MAP * SUMA_pbardef_to_CM(char *cmd)
{
   static char FuncName[]={"SUMA_pbardef_to_CM"};
   SUMA_COLOR_MAP *CM=NULL;
   int ii , neq=0 , nonum=0, N_Col;
   float  val[NPANE_BIG+1],rgb[3]={0.0, 0.0, 0.0}, **M=NULL;
   char name[NSBUF]="\0", eqn[NSBUF]="\0" , rhs[NSBUF]="\0" ;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   CM = (SUMA_COLOR_MAP *)SUMA_calloc(1,sizeof(SUMA_COLOR_MAP));
   if (CM == NULL ) {
      SUMA_SL_Crit ("Failed to allocate for CM");
      SUMA_RETURN(NULL);
   }
   memset(CM, 0, sizeof(SUMA_COLOR_MAP));
   CM->idvec = NULL;
   CM->chd = NULL;
   CM->top_frac = 0.0f;
   CM->SO = NULL; 
   CM->cname = NULL;
   CM->N_M[0] = NPANE_BIG; 
   CM->N_M[1] = 4;
   CM->Sgn = 0;
      
   CM->frac = NULL;
   CM->M = (float**)SUMA_allocate2D (CM->N_M[0], CM->N_M[1], sizeof(float));
   if (  CM->M == NULL  ) {
      SUMA_SL_Crit ("Failed to allocate for fields of CM.");
      SUMA_RETURN (NULL);
   }
   
   /* most of it from the PBAR_define_bigmap chunk */
   if (strncmp(cmd,"init_bigmaps", 12) == 0) {
      static char **bmn = NULL;
      static rgbyte **bm = NULL;
      char sname[100], snamemax[100];
      int iii, kkk, found;
      SUMA_LHv("one big map %s\n", cmd);
      if (!bm) {
         SUMA_LH("Going to initialize bigmaps");
         if (NJ_bigmaps_init(NBIGMAP_INIT, &bmn, &bm)) {
            SUMA_S_Err("That Acela Sure Sucks!");
            SUMA_RETURN(NULL);
         }
      }
      sprintf(snamemax, "init_bigmaps_%d", NBIGMAP_INIT-1);
      found = 0;
      for (iii=0; iii<NBIGMAP_INIT; ++iii) {
         SUMA_LHv("testing %d/(%d)...\n",iii, NBIGMAP_INIT);
         sprintf(sname, "init_bigmaps_%d", iii);
         if ( strcmp(cmd,sname) == 0 ) {
            SUMA_LHv("found one %d\n", iii);
            CM->Name = SUMA_copy_string(BIGMAP_NAMES[iii]);
            for (kkk=0; kkk<NPANE_BIG; ++kkk) {
                  SUMA_LHv("%d [%d %d %d]\n", 
                        kkk,
                        bm[iii][kkk].r, bm[iii][kkk].g, bm[iii][kkk].b);
               CM->M[NPANE_BIG-(kkk+1)][0] = bm[iii][kkk].r / 255.0f ; 
               CM->M[NPANE_BIG-(kkk+1)][1] = bm[iii][kkk].g / 255.0f ; 
               CM->M[NPANE_BIG-(kkk+1)][2] = bm[iii][kkk].b / 255.0f ;
               CM->M[NPANE_BIG-(kkk+1)][3] = 1.0;
            }
            found = 1;
         }
      }
      if (strcmp(cmd, snamemax)==0) {
         SUMA_LHv("Freeing bigmaps (at %s)\n", snamemax);
         for (iii=0; iii<NBIGMAP_INIT; ++iii) {
            if (bmn[iii]) free(bmn[iii]);
            if (bm[iii]) free(bm[iii]);
         }
         free(bmn); free(bm);  
         bmn = NULL;
         bm= NULL;
      }
      if (found) {
         CM->M0[0] = CM->M[0][0]; 
         CM->M0[1] = CM->M[0][1]; 
         CM->M0[2] = CM->M[0][2];
         CM->M0[3] = CM->M[0][3]; 
         SUMA_RETURN(CM);
      } else{
         SUMA_S_Err("Bad deal. No bigmap constructed.");
         SUMA_RETURN(NULL);
      }
   } else {
      name[0] = '\0' ; ii = 0 ;
      sscanf(cmd,"%127s%n",name,&ii) ;
      SUMA_LHv("name = %s %d\n",name,ii);
      CM->Name = SUMA_copy_string(name);

      if( *name == '\0' || ii == 0 ) RETURN(NULL) ;
      cmd += ii ;
      /* get lines of form "value=colordef" */

      while( neq < NPANE_BIG ){
         eqn[0] = '\0' ; ii = 0 ;
         sscanf(cmd,"%127s%n",eqn,&ii) ;
         SUMA_LHv("%s %d\n",eqn,ii);
         if( *eqn == '\0' || ii == 0 ) break ;   /* exit loop */
         cmd += ii ;
         if( neq == 0 && (isalpha(eqn[0]) || eqn[0]=='#') ) nonum = 1 ;
         rhs[0] = '\0' ; ii = 0 ;
         if( !nonum ) sscanf(eqn,"%f=%s%n",val+neq,rhs,&ii) ;
         else         sscanf(eqn,"%s%n"           ,rhs,&ii) ;
         if( *rhs == '\0' || ii == 0 ) RETURN(NULL);               /* bad */
         SUMA_LHv("eqn=%s\n rhs=%s\n", eqn, rhs);
         ii = !SUMA_Interpret_AFNIColor(rhs, rgb );
         if (ii) {
            SUMA_S_Errv("Failed to interpret AFNIColor %s\n", rhs);
            RETURN(NULL);       
         }
         SUMA_LHv("%s %f %f %f %d\n",rhs, rgb[0],rgb[1],rgb[2], ii);
         CM->M[neq][0] = rgb[0];
         CM->M[neq][1] = rgb[1];
         CM->M[neq][2] = rgb[2];
         CM->M[neq][3] = 1.0;
         neq++;
      }
      SUMA_LHv("Map %s, neq = %d\n", name, neq);

      /* in AFNI, all of these maps get interpolated to NPANE_BIG
         but that is not needed in SUMA. Only some maps need the 
         interpolation to look good */
      if (neq <= 20) { /* an arbitrary number, really */
         SUMA_COLOR_MAP *CMn=NULL;   
         /* now do the interpolation to NPANE_BIG */
         CMn = SUMA_MakeColorMap(CM->M, neq, 0, NPANE_BIG+1, 0, CM->Name);
         SUMA_Free_ColorMap(CM); CM=CMn; CMn=NULL;
      } else { /* leave it like it is */
         SUMA_LHv("Leaving map %s at %d colors\n", CM->Name, neq);
         /* realloc */
         N_Col = neq;
         M = (float**)SUMA_allocate2D (N_Col, 4, sizeof(float));
         for (ii=0; ii<N_Col; ++ii) { 
            M[ii][0] = CM->M[ii][0];
            M[ii][1] = CM->M[ii][1];
            M[ii][2] = CM->M[ii][2];
            M[ii][3] = CM->M[ii][3];
         }
         SUMA_free2D((char**)CM->M, CM->N_M[0]); CM->M = M; M = NULL;
         CM->N_M[0] = N_Col; CM->N_M[1] = 4;
      }
   }
   SUMA_RETURN(CM);
}
/*!
      \brief Builds the SUMA_AFNI_COLORS structure which
      contains the Named colors and colormaps defined in AFNI
      The colors and colormaps loaded are the ones that 
      are found in AFNI when AFNI is first started.
      
      - Continuous colormaps are NOT loaded.
      - Colors and colormaps defined in the .afnirc are loaded
      
\sa   SUMA_DestroyAfniColors (for deleting returned structure)
*/
#define SUMA_PBARDEF_ADD(mdmm) {  \
   SUMA_COLOR_MAP *m_CM=NULL; \
   m_CM = SUMA_pbardef_to_CM( mdmm ); \
   if (!(CMv = SUMA_Add_ColorMap (m_CM, CMv, &N_maps))) { \
      SUMA_SL_Crit("Failed in SUMA_Add_ColorMap"); \
      SUMA_RETURN(NULL);   \
   }  \
}
SUMA_AFNI_COLORS *SUMA_Get_AFNI_Default_Color_Maps ()
{
   static char FuncName[]={"SUMA_Get_AFNI_Default_Color_Maps"};
   float rgb[3]={0.0, 0.0, 0.0};
   SUMA_RGB_NAME *Cv = NULL;
   SUMA_COLOR_MAP **CMv=NULL;
   SUMA_COLOR_MAP *CMp=NULL, *CMn=NULL;
   SUMA_AFNI_COLORS *SAC=NULL;
   int i=-1, j=-1, icol=-1;
   int N_maps=-1, N_cols=-1;
   int       ival=-1 , ii=-1,jj=-1 ;
   float     fval =0.0;
   float     pthr[NPANE_MAX+1] ;
   int       pov[NPANE_MAX+1] ;
   char *homeenv=NULL, *sumarc=NULL;
   struct stat stbuf;
   SUMA_Boolean LocalHead_Detail = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   /* initilialize*/
   N_maps = -1;
   N_cols = -1;
   for (i=0;i<NPANE_MAX+1;++i) { pthr[i] = 0.0; pov[i] = 0;}
   
   /* add the none color first */
   Cv = SUMA_Add_Color ("none", 
                        -1.0, -1.0, -1.0, 1.0, 
                        Cv, &N_cols);
                        
   /* get the rgb, floats of each color defined DEFAULT_NCOLOVR */
   for (i=0; i<DEFAULT_NCOLOVR; ++i) {
      if (!SUMA_Interpret_AFNIColor (INIT_def_colovr[i], rgb)) {
         fprintf(SUMA_STDERR,
                  "Error %s: Failed to interpret color %s : %s\n", 
                  FuncName, INIT_def_labovr[i], INIT_def_colovr[i]);
      } else {
         if (LocalHead_Detail) 
            fprintf(SUMA_STDERR,"%s: Adding color %s : %s [%.3f %.3f %.3f]\n", 
                    FuncName, INIT_def_labovr[i], INIT_def_colovr[i], 
                    rgb[0], rgb[1], rgb[2]); 
         Cv = SUMA_Add_Color (INIT_def_labovr[i], 
                           rgb[0], rgb[1], rgb[2], 1.0, 
                           Cv, &N_cols);
     } 
   }
   #if 0
      /* causes crash on Fedora Core 7 and core 6, not worth it */
      SUMA_Interpret_AFNIColor (NULL, rgb);
   #endif
   /* Now create the afni color maps with more than 10 
      panes (excerpts from afni.c)*/
   
   /* start with positive panes */
   for( ii=NPANE_INIT+1 ; ii <= NPANE_MAX ; ii++ ){
      fval     = 1.0 / ii ;
      pthr[0]  = 1.0 ;
      pthr[ii] = 0.0 ;
      for( jj=1 ; jj < ii ; jj++ ) pthr[jj] = fval * (ii-jj) ;
      for( jj=0 ; jj < ii ; jj++ ) pov[jj]  = (jj % INIT_ncolovr) + 1 ;

      for( jj=0 ; jj <= ii ; jj++ ) INIT_pval_pos[ii][jj] = pthr[jj] ;
      for( jj=0 ; jj <  ii ; jj++ ) INIT_ovin_pos[ii][jj] = pov[jj] ;
   }

   /** initialize signed pbar panes **/
   for( ii=NPANE_INIT+1 ; ii <= NPANE_MAX ; ii++ ){
      fval     =  1.0 / ii ;
      pthr[0]  =  1.0 ;
      pthr[ii] = -1.0 ;
      for( jj=1 ; jj < ii ; jj++ ) pthr[jj] = fval * (ii-2*jj) ;
      for( jj=0 ; jj < ii ; jj++ ) pov[jj]  = (jj % INIT_ncolovr) + 1 ;

      for( jj=0 ; jj <= ii ; jj++ ) INIT_pval_sgn[ii][jj] = pthr[jj] ;
      for( jj=0 ; jj <  ii ; jj++ ) INIT_ovin_sgn[ii][jj] = pov[jj] ;
   }
   
#if defined(RGBCYC_COUNT) && RGBCYC_COUNT <= NPANE_MAX
   ii = RGBCYC_COUNT ;
   for( jj=0 ; jj < ii ; jj++ ) INIT_ovin_pos[ii][jj] = RGBCYC_FIRST+jj+1 ;
   for( jj=0 ; jj < ii ; jj++ ) INIT_ovin_sgn[ii][jj] = RGBCYC_FIRST+jj+1 ;
#endif

   
   /* now create AFNI's colormaps */
   for (i = NPANE_MIN; i<= NPANE_MAX; ++i) {
      CMp = (SUMA_COLOR_MAP *)SUMA_calloc(1,sizeof(SUMA_COLOR_MAP));
      CMn = (SUMA_COLOR_MAP *)SUMA_calloc(1, sizeof(SUMA_COLOR_MAP));
      if (CMp == NULL || CMn == NULL) {
         SUMA_SL_Crit ("Failed to allocate for CMp &/| CMn.");
         SUMA_RETURN(NULL);
      }
      memset(CMp, 0, sizeof(SUMA_COLOR_MAP));
      memset(CMn, 0, sizeof(SUMA_COLOR_MAP));
      CMp->idvec = NULL;
      CMn->idvec = NULL;
      CMp->chd = NULL;
      CMn->chd = NULL;
      CMp->top_frac = 0.0f;
      CMn->top_frac = 0.0f;
      CMp->SO = NULL; 
      CMn->SO = NULL; 
      CMp->cname = NULL;
      CMn->cname = NULL;
      CMp->N_M[0] = i; CMp->N_M[1] = 4;
      CMn->N_M[0] = i; CMn->N_M[1] = 4;
      CMp->Sgn = 1;
      CMn->Sgn = -1;
      
      CMp->Name = (char *)SUMA_calloc(25, sizeof(char));
      CMn->Name = (char *)SUMA_calloc(25, sizeof(char));
      CMp->frac = (float *)SUMA_calloc(i, sizeof(float));
      CMn->frac = (float *)SUMA_calloc(i, sizeof(float));
      CMp->M = (float**)SUMA_allocate2D (CMp->N_M[0],CMp->N_M[1],sizeof(float));
      CMn->M = (float**)SUMA_allocate2D (CMn->N_M[0],CMn->N_M[1],sizeof(float));
      if (  CMp->frac == NULL || CMn->frac == NULL 
         || CMp->M == NULL || CMn->M == NULL 
         || CMp->Name == NULL || CMn->Name == NULL ) {
         SUMA_SL_Crit ("Failed to allocate for fields of CMp &/| CMn.");
         SUMA_RETURN (NULL);
      }
      
      sprintf(CMp->Name,   "afni_p%d",i); 
      sprintf(CMn->Name, "afni_n%d",i);
      
      if (LocalHead_Detail) 
         fprintf (SUMA_STDERR,"%s: Building colormap POS #%d (%s)\n", 
                  FuncName, i, CMp->Name);
      
      for ( j = 0; j < i; ++j) {
         if (!INIT_ovin_pos[i][j]) {
            if (LocalHead_Detail) {
               fprintf (SUMA_STDERR,"\t[i%d] NoColor\t%f\n", 
                              INIT_ovin_pos[i][j], 
                              INIT_pval_pos[i][j]);   
            }
            CMp->M[i - j - 1][0] = CMp->M[i - j - 1][1] = 
            CMp->M[i - j - 1][2] = -1.0; 
            CMp->M[i - j - 1][3] = 0.0;
         } else {
            if (LocalHead_Detail) {
               fprintf (SUMA_STDERR,"\t[i%d] %s\t%f\n", 
                              INIT_ovin_pos[i][j], 
                              INIT_def_labovr[INIT_ovin_pos[i][j]-1], 
                              INIT_pval_pos[i][j]);
            }
            /* find out which color this is */
            icol = SUMA_Find_Color (INIT_def_labovr[INIT_ovin_pos[i][j]-1], 
                                    Cv, N_cols);
            if (icol < 0) {
               fprintf (SUMA_STDERR,
                        "Error%s: Failed to find color %s\n"
                        "Using no-color in its place\n", 
                         FuncName, INIT_def_labovr[INIT_ovin_pos[i][j]-1]);
               CMp->M[i - j - 1][0] = CMp->M[i - j - 1][1] = 
               CMp->M[i - j - 1][2] = -1.0; 
               CMp->M[i - j - 1][3] = 0.0;
            } else {
               CMp->M[i - j - 1][0] = Cv[icol].r;
               CMp->M[i - j - 1][1] = Cv[icol].g;
               CMp->M[i - j - 1][2] = Cv[icol].b;
               CMp->M[i - j - 1][3] = 1.0;
            }
         }
         CMp->frac[i - j - 1] = INIT_pval_pos[i][j];
      }
      if (CMp->frac[CMp->N_M[0]-1] != 1.0f) {
         SUMA_LH("top_frac not 1 for positive map");
         CMp->top_frac = CMp->frac[CMp->N_M[0]-1];
      }
      CMp->M0[0] = CMp->M[0][0]; 
      CMp->M0[1] = CMp->M[0][1]; 
      CMp->M0[2] = CMp->M[0][2];
      CMp->M0[3] = CMp->M[0][3]; 

      /* add the positive map to the list */
      CMv = SUMA_Add_ColorMap (CMp, CMv, &N_maps);
      if (!CMv) {
         SUMA_SL_Crit("Failed in SUMA_Add_ColorMap");
         SUMA_RETURN(NULL);
      }
      
      if (LocalHead_Detail) 
         fprintf (SUMA_STDERR,"%s: Building colormap SGN #%d (%s)\n", 
                  FuncName, i, CMn->Name);
      
      for ( j = 0; j < i; ++j) {
         if (!INIT_ovin_sgn[i][j]) {
            if (LocalHead_Detail) {
               fprintf (SUMA_STDERR,"\t[i%d] NoColor\t%f\n", 
                              INIT_ovin_sgn[i][j], 
                              INIT_pval_sgn[i][j]);
            }
            CMn->M[i - j - 1][0] = CMn->M[i - j - 1][1] = 
            CMn->M[i - j - 1][2] = -1.0;
            CMn->M[i - j - 1][3] = 0.0;
         } else {
            if (LocalHead_Detail) {
               fprintf (SUMA_STDERR,"\t[i%d] %s\t%f\n", 
                              INIT_ovin_sgn[i][j], 
                              INIT_def_labovr[INIT_ovin_sgn[i][j]-1], 
                              INIT_pval_sgn[i][j]);
            }
            icol = SUMA_Find_Color (INIT_def_labovr[INIT_ovin_sgn[i][j]-1], 
                                    Cv, N_cols);
            if (icol < 0) {
               fprintf (SUMA_STDERR,
                        "Error%s: Failed to find color %s\n"
                        "Using no-color in its place", 
                        FuncName, INIT_def_labovr[INIT_ovin_sgn[i][j]-1]);
               CMn->M[i - j - 1][0] = CMn->M[i - j - 1][1] = 
               CMn->M[i - j - 1][2] = -1.0;
               CMn->M[i - j - 1][3] = 0.0;
            } else {
               CMn->M[i - j - 1][0] = Cv[icol].r;
               CMn->M[i - j - 1][1] = Cv[icol].g;
               CMn->M[i - j - 1][2] = Cv[icol].b;
               CMn->M[i - j - 1][3] = 1.0;
            }
         }
         CMn->frac[i - j - 1] = INIT_pval_sgn[i][j];
      }
      if (CMn->frac[CMn->N_M[0]-1] != 1.0f) {
         SUMA_LH("top_frac not 1 for negative map");
         CMn->top_frac = CMn->frac[CMp->N_M[0]-1];
      }
      CMn->M0[0] = CMn->M[0][0]; 
      CMn->M0[1] = CMn->M[0][1]; 
      CMn->M0[2] = CMn->M[0][2]; 
      CMn->M0[3] = CMn->M[0][3]; 
      /* add the negative map to the list */
      CMv = SUMA_Add_ColorMap (CMn, CMv, &N_maps);
      if (!CMv) {
         SUMA_SL_Crit("Failed in SUMA_Add_ColorMap");
         SUMA_RETURN(NULL);
      }
   }
   
   
   /* perhaps someday include the continuous color maps from AFNI too. 
      (see pbar.c file, search for colorscale) 
      That day has come Dec 18 07*/
   SUMA_PBARDEF_ADD("init_bigmaps_6");
   SUMA_PBARDEF_ADD("init_bigmaps_5");
   SUMA_PBARDEF_ADD("init_bigmaps_4");
   SUMA_PBARDEF_ADD("init_bigmaps_3");
   SUMA_PBARDEF_ADD("init_bigmaps_2");
   SUMA_PBARDEF_ADD("init_bigmaps_1");
   SUMA_PBARDEF_ADD("init_bigmaps_0");
   SUMA_PBARDEF_ADD(CB_CS_35); 
   SUMA_PBARDEF_ADD(CB_CS);
   SUMA_PBARDEF_ADD(CYTOARCH_ROI_256_CMD);
   SUMA_PBARDEF_ADD(CYTOARCH_ROI_256_GAP_CMD);
   SUMA_PBARDEF_ADD(FREESURFER_SEG_255_CMD);
   SUMA_PBARDEF_ADD(GRAY_CS);
   SUMA_PBARDEF_ADD(GRAY_CIRCLE_CS);
   SUMA_PBARDEF_ADD(GRAY_INV_CIRCLE_CS);
   SUMA_PBARDEF_ADD(AMBER_CS);
   SUMA_PBARDEF_ADD(AMBER_CIRCLE_CS);
   SUMA_PBARDEF_ADD(AMBER_INV_CIRCLE_CS);
   SUMA_PBARDEF_ADD(GREEN_CS);
   SUMA_PBARDEF_ADD(RED_CS);
   SUMA_PBARDEF_ADD(BLUE_CS);
   SUMA_PBARDEF_ADD(ROI_32_CMD);
   SUMA_PBARDEF_ADD(ROI_64_CMD);
   SUMA_PBARDEF_ADD(ROI_128_CMD);
   SUMA_PBARDEF_ADD(ROI_256_CMD);

   /* now wrap it up */   
   SAC = (SUMA_AFNI_COLORS *) SUMA_calloc(1,sizeof(SUMA_AFNI_COLORS));
   SAC->CMv = CMv;
   SAC->N_maps = N_maps;
   SAC->Cv = Cv;
   SAC->N_cols = N_cols;

   /* load whatever's lurking in the .afnirc files */
   /* passing NULL for dc (the last parameter) because I
   am not using the continuous color maps yet */
   SUMA_LH("Calling Process setup");
   homeenv = getenv("HOME");   
   
   if (!homeenv) sumarc = SUMA_copy_string(".afnirc");
   else sumarc = SUMA_append_string (homeenv, "/.afnirc");
   if (stat(sumarc, &stbuf) != -1) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: Loading %s ...\n", FuncName, sumarc);
      if (SUMA_AFNI_Extract_Colors ( sumarc, SAC ) < 0) {
         fprintf(SUMA_STDERR,
                  "Error %s: Failed scanning .afnirc for colors and colormaps.\n"
                  "Proceeding ...\n", FuncName);
      } 
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: No rc files found.\n", FuncName);
   }
   if (sumarc) SUMA_free(sumarc);
   
   /* Show me the results: */
   if (LocalHead_Detail) {
      SUMA_Show_ColorVec (SAC->Cv, SAC->N_cols, NULL);
   }
   
   if (LocalHead_Detail) {
      SUMA_Show_ColorMapVec (SAC->CMv, SAC->N_maps, NULL, 1);
   }   
   
   SUMA_LH("Returning");
   
   SUMA_RETURN(SAC);
}

/*!
   \brief creates the colormaps available for SUMA
*/
SUMA_AFNI_COLORS *SUMA_Build_Color_maps(void)
{
   static char FuncName[]={"SUMA_Build_Color_maps"};
   SUMA_RGB_NAME *Cv = NULL;
   int i;
   SUMA_COLOR_MAP **CMv=NULL;
   SUMA_COLOR_MAP *CM=NULL;
   SUMA_AFNI_COLORS *SAC=NULL;
   char *name;
   SUMA_ENTRY;
   
   SAC = SUMA_Get_AFNI_Default_Color_Maps();
  
   /* Now add SUMA's colormaps */
   i = 0;
   name = SUMA_COLOR_MAP_NAMES[0];
   while (name[0]) {
      CM = SUMA_MakeStandardMap (name);
      if (!CM) {
         SUMA_SL_Crit("Failed to create standard maps");
         SUMA_RETURN(NULL);
      }
      SAC->CMv = SUMA_Add_ColorMap (CM, SAC->CMv, &(SAC->N_maps));
      if (!SAC->CMv) {
         SUMA_SL_Crit("Failed in SUMA_Add_ColorMap");
         SUMA_RETURN(NULL);
      }
      ++i;
      name = SUMA_COLOR_MAP_NAMES[i];
   } 
   
   SUMA_RETURN(SAC);
}
   

/*! \brief A function to add a new color to the color vector 

   \param Name (char *) name of new color  to be added
   \param r (float) r color ( 0 <= r <= 1)
   \param g (float) g color ( 0 <= g <= 1)
   \param b (float) b color ( 0 <= b <= 1)
   \param a (float) a color ( 0 <= a <= 1)
   - You are allowed to pass -1.0, -1.0, -1.0 for r g b to define the No-Color
   \param oCv (SUMA_RGB_NAME*) old vector containing color structs
   \param N_cols (int *) number of colors in oCv
               When the function returns, this number will include the added
               color
   \return NewCv (SUMA_RGB_NAME *) you know what.
                  If the color being added exists (by Name), it will replace 
                  the preexisting one
-  New space is allocated for each new color (realloc). 
-  This process is not terribly efficient by not many colors
are loaded in this manner.
-  You can send NULL for oCv if this vector has yet to be created

*/
SUMA_RGB_NAME * SUMA_Add_Color (char *Name, float r, float g, float b, float a, SUMA_RGB_NAME *oCv, int *N_cols)
{
   static char FuncName[]={"SUMA_Add_Color"};
   SUMA_RGB_NAME *NewCv = NULL;
   int iadd;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!(r == -1.0 && g == -1.0 && b == -1.0)) {
      if (  r < 0 || r > 1 ||
            g < 0 || g > 1 ||
            b < 0 || b > 1 ||
            a < 0 || a > 1 ) {
         SUMA_S_Err("Bad r, g, b and/or a values.\nMust be between 0 and 1.Nothing done.");
         SUMA_RETURN(oCv);
      }
   }else {
      SUMA_LH("No color");
   }
   
   if (strlen(Name) > SUMA_MAX_COLOR_NAME -1 ) {
      SUMA_S_Err("Too long a color name\n(> SUMA_MAX_COLOR_NAME)\nNothing done.");
      SUMA_RETURN(oCv);
   }
   
   if (!oCv) {
      SUMA_LH("New color vector, allocating");
      NewCv = (SUMA_RGB_NAME *) SUMA_calloc(1, sizeof(SUMA_RGB_NAME));
      *N_cols = 1;
      NewCv[*N_cols-1].r = r;
      NewCv[*N_cols-1].g = g;
      NewCv[*N_cols-1].b = b;
      NewCv[*N_cols-1].a = a;
      sprintf(NewCv[*N_cols-1].Name, "%s", Name);
      SUMA_RETURN(NewCv);
   }
   
   /* not new, check to see if color exists already */
   iadd = SUMA_Find_Color (Name, oCv, *N_cols);
   
   if (iadd >= 0) {
      SUMA_LH("Replacing old color");
      NewCv = oCv;
      NewCv[iadd].r = r;
      NewCv[iadd].g = g;
      NewCv[iadd].b = b;
      NewCv[iadd].a = a;
      SUMA_RETURN(oCv);
   }
   
   /* a new map altogether */
   SUMA_LH("Adding new color");
   *N_cols += 1;
   NewCv = (SUMA_RGB_NAME *) SUMA_realloc(oCv, *N_cols * sizeof(SUMA_RGB_NAME));
   NewCv[*N_cols-1].r = r;
   NewCv[*N_cols-1].g = g;
   NewCv[*N_cols-1].b = b;
   NewCv[*N_cols-1].a = a;
   sprintf(NewCv[*N_cols-1].Name, "%s", Name);

   SUMA_RETURN(NewCv);
}

/*! \brief A function to add a new colormap to the colormap vector 

   \param CM (SUMA_COLOR_MAP *) new color map to be added
   \param OldCMv (SUMA_COLOR_MAP **) old vector containing pointers to colormaps
   \param N_map (int *) number of maps in OldCMv
               When the function returns, this number will include the added
               map
   \return NewCMv (SUMA_COLOR_MAP **) you know what.
                  If the map being added exists (by Name), it will replace 
                  the preexisting one
-  New space is allocated for each new map (realloc). 
-  This process is not terribly efficient by not many maps
are loaded in this manner.
-  You can send NULL for OldCMv if this vector has yet to be created
-  Color map structure is copied by reference. Do not free it.

*/
SUMA_COLOR_MAP ** SUMA_Add_ColorMap (SUMA_COLOR_MAP *CM, 
                                     SUMA_COLOR_MAP **OldCMv, int *N_maps) 
{
   static char FuncName[]={"SUMA_Add_ColorMap"};
   SUMA_COLOR_MAP ** NewCMv = NULL;
   int iadd;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!CM) {
      SUMA_S_Warn("Null CM, nothing to do");
      SUMA_RETURN(OldCMv);
   }
   if (!OldCMv) {
      SUMA_LH("New color vector, allocating");
      NewCMv = (SUMA_COLOR_MAP **) SUMA_calloc(1, sizeof(SUMA_COLOR_MAP *));
      *N_maps = 1;
      NewCMv[0] = CM;
      SUMA_RETURN(NewCMv);
   }
   
   /* not new, check to see if map exists already */
   iadd = SUMA_Find_ColorMap (CM->Name, OldCMv, *N_maps, CM->Sgn);
   if (iadd >= 0) {
      SUMA_LH("Replacing old colormap");
      SUMA_Free_ColorMap (OldCMv[iadd]);
      OldCMv[iadd] = CM;
      SUMA_RETURN(OldCMv);
   }
   
   /* a new map altogether */
   SUMA_LH("Adding new color map");
   *N_maps += 1;
   NewCMv = (SUMA_COLOR_MAP **) SUMA_realloc( OldCMv, *N_maps * 
                                              sizeof(SUMA_COLOR_MAP *));
   NewCMv[*N_maps - 1] = CM;
   
   SUMA_RETURN(NewCMv);
}

/*!
   \brief Cleans up and destroys the contents of the SUMA_AFNI_COLORS 
   \sa SUMA_Get_AFNI_Default_Color_Maps
*/
SUMA_AFNI_COLORS *SUMA_DestroyAfniColors (SUMA_AFNI_COLORS *SAC)
{
   static char FuncName[]={"SUMA_DestroyAfniColors"};
   int i;
   
   SUMA_ENTRY;
   
   if (!SAC) SUMA_RETURN(NULL);
   
   /* Now clean the colormap vector */
   for (i=0; i < SAC->N_maps; ++i) {
      if (SAC->CMv[i]) SUMA_Free_ColorMap(SAC->CMv[i]);
   }
   SUMA_free(SAC->CMv);
   SAC->N_maps = -1;
   
   /* Now clean the color vector */
   SUMA_free(SAC->Cv);
   SAC->N_cols = -1;
   
   /* Now destroy SAC */
   SUMA_free(SAC);
   
   SUMA_RETURN(NULL);
}

/*!
   \brief List the colors in the vector
   \sa char *SUMA_Show_ColorVec
*/
char *SUMA_ColorVec_Info (SUMA_RGB_NAME *Cv, int N_cols) 
{
   static char FuncName[]={"SUMA_ColorVec_Info"};
   int i;
   char stmp[100], *s = NULL;
   SUMA_STRING *SS = NULL;  
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   if (Cv) {
      for (i=0; i < N_cols; ++i) {
         if (Cv[i].r == -1) {
            sprintf (stmp, "%d/%d: color(%d) No Color(%s): [%f %f %f %f]\n", 
                           i+1, N_cols, i, 
                           Cv[i].Name, Cv[i].r, Cv[i].g, Cv[i].b, Cv[i].a);
            SS = SUMA_StringAppend (SS, stmp);
         } else {
            sprintf (stmp, "%d/%d: color(%d) %s: [%f %f %f %f]\n", 
                           i+1, N_cols, i, 
                           Cv[i].Name, Cv[i].r, Cv[i].g, Cv[i].b, Cv[i].a);
            SS = SUMA_StringAppend (SS, stmp);
         }
      }
   } else {
      sprintf (stmp, "NULL Cv.\n");
      SS = SUMA_StringAppend (SS, stmp);
   }   
   
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
      
   SUMA_RETURN(s);
}

/*!
   return a color in acol (4 floats) from colormap name color index i % Ncolors
   if name is NULL a random color is returned
*/
    
int SUMA_a_good_col(char *name, int i, float *acol)
{
   static char FuncName[]={"SUMA_a_good_col"};
   int ic, icmap=-1, dorand;
   SUMA_COLOR_MAP *CM;

   SUMA_ENTRY;

   dorand = 0;
   if (i<0 || !acol) {
      SUMA_S_Err("Give me a break!");
      SUMA_RETURN(0);
   }

   if (name && !(SUMAg_CF && SUMAg_CF->scm && SUMAg_CF->scm->CMv && 
                  SUMAg_CF->scm->N_maps)) {
      /* try building colormaps */
      if (!SUMAg_CF->scm) SUMAg_CF->scm = SUMA_Build_Color_maps();
   }
   
   if (!name || !(SUMAg_CF && SUMAg_CF->scm && SUMAg_CF->scm->CMv && 
                  SUMAg_CF->scm->N_maps)) {
      dorand = 1;   
   } else {
      /* have colormaps, get me something decent */
      icmap = SUMA_Find_ColorMap(name, SUMAg_CF->scm->CMv, 
                                 SUMAg_CF->scm->N_maps,-2);
      if (icmap < 0) {
         int d;
         char *endp;
         /* try again, maybe we just have an maximum number */
         d = (int)strtod(name, &endp);
         if (endp == name && d == 0) { /* no good */
         } else {
            if (d < 32) {
               icmap = SUMA_Find_ColorMap("ROI_i32", SUMAg_CF->scm->CMv, 
                                 SUMAg_CF->scm->N_maps,-2);
            }else if (d < 64) {
               icmap = SUMA_Find_ColorMap("ROI_i64", SUMAg_CF->scm->CMv, 
                                 SUMAg_CF->scm->N_maps,-2);
            }else if (d < 128) {
               icmap = SUMA_Find_ColorMap("ROI_i128", SUMAg_CF->scm->CMv, 
                                 SUMAg_CF->scm->N_maps,-2);
            }else if (d < 256) {
               icmap = SUMA_Find_ColorMap("ROI_i256", SUMAg_CF->scm->CMv, 
                                 SUMAg_CF->scm->N_maps,-2);
            }
         }
      }
      
   }

   if (name && icmap < 0) {
            SUMA_S_Warnv("No colormap named %s was found, "
                         "returning random colors.\n", name);
            dorand = 1;
   }
   
   if (dorand) {
      /* GIMME SOME RANDOM */
      srand(i);
      acol[0] = (float) (double)rand()/(double)RAND_MAX;
      acol[1] = (float) (double)rand()/(double)RAND_MAX;
      acol[2] = (float) (double)rand()/(double)RAND_MAX;
      acol[3] = 1.0;
      SUMA_RETURN(1);
   } else {
      CM = SUMAg_CF->scm->CMv[icmap];
      ic = i % CM->N_M[0];
      acol[0] = CM->M[ic][0];
      acol[1] = CM->M[ic][1];
      acol[2] = CM->M[ic][2];
      if (CM->N_M[1]==4) acol[3] = CM->M[ic][3];
      else acol[3] = 1.0;
   }

   SUMA_RETURN(1);
}

/*!
   \brief List the colormaps in the vector
   
   \param CMv (SUMA_COLOR_MAP **) an array of pointers to colormap structures
   \param N_maps (int) the number of colormaps in CMv
   \param detail (int)  0, just the name of the colormaps
                        1, up to 5 of the colors listed
                        2, complete colormap listed
                        
   \sa char *SUMA_Show_ColorMapVec
   
*/
char *SUMA_ColorMapVec_Info (SUMA_COLOR_MAP **CMv, int N_maps, int detail) 
{
   static char FuncName[]={"SUMA_ColorMapVec_Info"};
   int i, j, jmax;
   char stmp[256], *s = NULL;
   SUMA_STRING *SS = NULL;  
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   SUMA_LH("Entered");
   SS = SUMA_StringAppend (NULL, NULL);
   
   if (CMv) {
      for (i=0; i < N_maps; ++i) {
         if (!CMv[i]) {
            sprintf (stmp, "%d/%d: cmap(%d) NULL\n", i+1, N_maps, i);
            SS = SUMA_StringAppend (SS, stmp);
         } else {
            switch (CMv[i]->Sgn) {
               case 0:
                  sprintf (stmp, "%d/%d: cmap(%d) %s(.), %d (%d)cols.", 
                           i+1, N_maps, i, CMv[i]->Name,  
                           CMv[i]->N_M[0], CMv[i]->N_M[1]);
                  break;
               case 1:
                  sprintf (stmp, "%d/%d: cmap(%d) %s(+), %d (%d)cols.", 
                           i+1, N_maps, i, CMv[i]->Name,  
                           CMv[i]->N_M[0], CMv[i]->N_M[1]);
                  break;
               case -1:
                  sprintf (stmp, "%d/%d: cmap(%d) %s(-), %d (%d)cols.", 
                           i+1, N_maps, i, CMv[i]->Name,  
                           CMv[i]->N_M[0], CMv[i]->N_M[1]);
                  break;
               default:   
                  sprintf (stmp, 
                           "%d/%d: cmap(%d) %s(?), %d (%d)cols.\n"
                           "\tSgn field of colormap is not acceptable (%d)\n", 
                     i+1, N_maps, i, CMv[i]->Name,  
                     CMv[i]->N_M[0], CMv[i]->N_M[1], CMv[i]->Sgn);
                  break;
            }
            SS = SUMA_StringAppend (SS, stmp);
            if (CMv[i]->frac) {
               SS = SUMA_StringAppend (SS, "   Possibly non-linear.");
            } else {
               SS = SUMA_StringAppend (SS, "   Linear.");
            }
            if (CMv[i]->idvec) {
               SS = SUMA_StringAppend (SS, "   Has idvec.");
            } else {
               SS = SUMA_StringAppend (SS, "   NULL idvec.");
            }
            if (CMv[i]->chd) {
               SS = SUMA_StringAppend (SS, "   Has chd.");
            } else {
               SS = SUMA_StringAppend (SS, "   NULL chd.");
            }
            
            SS = SUMA_StringAppend (SS, "\n");
            switch (detail) {
               case 0:
                  jmax = 0;
                  break;
               case 1:
                  if (CMv[i]->N_M[0] < 5) jmax = CMv[i]->N_M[0];
                  else jmax = 5;
                  break;
               case 2:
                  jmax = CMv[i]->N_M[0];
                  break;
               default:
                  SUMA_SL_Err("Bad detail value\nUsing detail = 2");
                  jmax = CMv[i]->N_M[0];
                  break;
            }
            
            if (jmax) {
               for (j=jmax-1; j >= 0; --j) {
                  if (CMv[i]->frac) {
                     if (j == jmax -1) {
                        if (CMv[i]->N_M[1] == 3) 
                           sprintf (stmp, 
                                    "rank (i):\tid\t"
                                    "[R    \tG    \tB    \t\tf]\tName\n");
                        else
                           sprintf (stmp, 
                                    "rank (i):\tid\t"
                                    "[R    \tG    \tB    \tA\t\tf]\tName\n");
                        SS = SUMA_StringAppend (SS,stmp);    
                     }
                     if (CMv[i]->N_M[1] == 4) 
                        sprintf (stmp, 
                    "%03d:\t%6d\t [% .3f\t% .3f\t% .3f\t% .3f\t\t% .3f]\t%s\n",
                                 j, CMv[i]->idvec ? CMv[i]->idvec[j] : -1,
                                    CMv[i]->M[j][0], CMv[i]->M[j][1], 
                                    CMv[i]->M[j][2], CMv[i]->M[j][3], 
                                    CMv[i]->frac[j], 
                                 CMv[i]->cname ? CMv[i]->cname[j] : "anonimo");
                     else
                        sprintf (stmp, 
                             "%03d:\t%6d\t [% .3f\t% .3f\t% .3f\t\t% .3f]\t%s\n",
                                 j, CMv[i]->idvec ? CMv[i]->idvec[j] : -1,
                                    CMv[i]->M[j][0], CMv[i]->M[j][1], 
                                    CMv[i]->M[j][2], CMv[i]->frac[j],
                                 CMv[i]->cname ? CMv[i]->cname[j] : "anonimo");
                  } else {
                     if (j == jmax - 1) {
                        if (CMv[i]->N_M[1] == 3) 
                           sprintf (stmp, 
                                    "rank (i):\tid\t[R    \tG    \tB    ]\n");
                        else
                           sprintf (stmp, 
                                    "rank (i):\tid\t"
                                    "[R    \tG    \tB    \tA    ]\n");
                        SS = SUMA_StringAppend (SS,stmp);    
                     }
                     if (CMv[i]->N_M[1] == 4) 
                        sprintf (stmp, 
                           "%03d:\t%6d\t [% .3f\t% .3f\t% .3f\t% .3f]\t%s\n", 
                                 j, CMv[i]->idvec ? CMv[i]->idvec[j] : -1,
                                    CMv[i]->M[j][0], CMv[i]->M[j][1], 
                                    CMv[i]->M[j][2], CMv[i]->M[j][3],
                                 CMv[i]->cname ? CMv[i]->cname[j] : "anonimo");
                     else
                        sprintf (stmp, 
                           "%03d:\t%6d\t [% .3f\t% .3f\t% .3f]\t%s\n", 
                                 j, CMv[i]->idvec ? CMv[i]->idvec[j] : -1,
                                    CMv[i]->M[j][0], CMv[i]->M[j][1], 
                                    CMv[i]->M[j][2],
                                 CMv[i]->cname ? CMv[i]->cname[j] : "anonimo");
                  }
                  SS = SUMA_StringAppend (SS,stmp); 
               }
               if (jmax < CMv[i]->N_M[0] - 1) { 
                  if (CMv[i]->frac) 
                     SS = SUMA_StringAppend (SS,
                                       "..:\t [.....\t....\t....\t\t....]\n");
                  else SS = SUMA_StringAppend (SS,"..:\t [.....\t....\t....]\n");
               }
               if (jmax < CMv[i]->N_M[0]) { 
                  j = CMv[i]->N_M[0] - 1;
                  if (CMv[i]->frac) {
                     if (CMv[i]->N_M[1] == 4) 
                        sprintf (stmp, 
                     "%03d:\t%6d\t [% .3f\t% .3f\t% .3f\t\t% .3f\t\t% .3f]%s\n",
                              j, CMv[i]->idvec ? CMv[i]->idvec[j] : -1,
                              CMv[i]->M[j][0], CMv[i]->M[j][1],
                              CMv[i]->M[j][2], CMv[i]->M[j][3], CMv[i]->frac[j],
                              CMv[i]->cname ? CMv[i]->cname[j] : "anonimo");
                     else
                        sprintf (stmp, 
                           "%03d:\t%6d\t [% .3f\t% .3f\t% .3f\t\t% .3f]\t%s\n", 
                              j, CMv[i]->idvec ? CMv[i]->idvec[j] : -1,
                              CMv[i]->M[j][0], CMv[i]->M[j][1],
                              CMv[i]->M[j][2], CMv[i]->frac[j], 
                              CMv[i]->cname ? CMv[i]->cname[j] : "anonimo");
                  } else {
                     if (CMv[i]->N_M[1] == 4) 
                        sprintf (stmp, 
                           "%03d:\t%6d\t [% .3f\t% .3f\t% .3f\t% .3f]\t%s\n", 
                                 j, CMv[i]->idvec ? CMv[i]->idvec[j] : -1,
                              CMv[i]->M[j][0], CMv[i]->M[j][1],  
                              CMv[i]->M[j][2], CMv[i]->M[j][3],
                              CMv[i]->cname ? CMv[i]->cname[j] : "anonimo");
                     else
                        sprintf (stmp, 
                           "%03d:\t%6d\t [% .3f\t% .3f\t% .3f]\t%s\n", 
                                 j, CMv[i]->idvec ? CMv[i]->idvec[j] : -1,
                              CMv[i]->M[j][0], CMv[i]->M[j][1],  
                              CMv[i]->M[j][2], 
                              CMv[i]->cname ? CMv[i]->cname[j] : "anonimo");
                  }
                  SS = SUMA_StringAppend (SS,stmp); 
               }
            }
         }
      }
   } else {
      sprintf (stmp, "NULL CMv.\n");
      SS = SUMA_StringAppend (SS, stmp);
   }   
   
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
  
   SUMA_LH("Left");
      
   SUMA_RETURN(s);
}

/*!
   \brief Shows the contents of the color vector
   \sa SUMA_ColorVec_Info
*/
void SUMA_Show_ColorVec (SUMA_RGB_NAME *CMv, int N_maps, FILE *Out) 
{
   static char FuncName[]={"SUMA_Show_ColorVec"};
   char *s;
   
   SUMA_ENTRY;

   if (Out == NULL) Out = stdout;
      
   s =  SUMA_ColorVec_Info(CMv, N_maps);
   
   if (s) {
      fprintf (Out, "%s", s);
      SUMA_free(s);
   }else {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_ColorVec_Info.\n", FuncName);
   }   
   
   SUMA_RETURNe;
}

/*!
   \brief Shows the contents of the colormaps vector
   \sa SUMA_ColorMapVec_Info
*/
void SUMA_Show_ColorMapVec (SUMA_COLOR_MAP **CMv, int N_maps, 
                            FILE *Out, int detail) 
{
   static char FuncName[]={"SUMA_Show_ColorMapVec"};
   char *s;
   SUMA_Boolean LocalHead  = NOPE;
   
   SUMA_ENTRY;

   
   if (Out == NULL) Out = stdout;
      
   s =  SUMA_ColorMapVec_Info(CMv, N_maps, detail);
   
   if (s) {
      fprintf (Out, "%s", s);
      SUMA_free(s);
   }else {
      fprintf (SUMA_STDERR, 
               "Error %s: Failed in SUMA_ColorMapVec_Info.\n", FuncName);
   }   
   

   SUMA_RETURNe;
}

/*!
   \brief Locate a color in the color vector
   \param Name (char *) Name ('\0' terminated) name of color to be found
   \param Cv (SUMA_RGB_NAME *) vector of color structs to be searched
   \param N_cols (int) number of colors in Cv
   \return icol (int) the index into Cv where the color named Name was found
                     (-1) if no such color was found
*/
int SUMA_Find_Color ( char *Name, SUMA_RGB_NAME *Cv, int N_cols) 
{
   static char FuncName[]={"SUMA_Find_Color"};
   int icol = -1, i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!Cv) {
      SUMA_S_Warn("Nothing to do. NULL Cv");
      SUMA_RETURN(icol);
   }
   
   for (i=0; i < N_cols; ++i) {
      if (strcmp(Cv[i].Name, Name) == 0) {
         SUMA_LH("Found Color");
         icol = i;
         SUMA_RETURN(icol);
      }
   }
   
   SUMA_LH("Failed to find color");
   SUMA_RETURN(icol);   
}

/*!
   \brief Locate a colormap in the colormap vector
   \param Name (char *) Name ('\0' terminated) name of colormap to be found
   \param CMv (SUMA_COLOR_MAP **) vector of pointers to colormaps to be searched
   \param N_maps (int) number of maps in CMv
   \param sgn (int) the sign of the map (-1, 0, +1)
                     if sign = -2 then the sign is completely ignored and 
                     only Name is used for a match
   \return imap (int) the index into CMv where the colormap named Name was found
                     (-1) if no such map was found
*/
int SUMA_Find_ColorMap ( char *Name, SUMA_COLOR_MAP **CMv, int N_maps, int sgn) 
{
   static char FuncName[]={"SUMA_Find_ColorMap"};
   int imap = -1, i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!CMv) {
      SUMA_S_Warn("Nothing to do. NULL CMv");
      SUMA_RETURN(imap);
   }
   
   
   for (i=0; i < N_maps; ++i) {
      if (CMv[i]) {
         if (sgn != -2) {
            if (strcmp(CMv[i]->Name, Name) == 0 && CMv[i]->Sgn == sgn) {
               SUMA_LH("Found Map");
               imap = i;
               SUMA_RETURN(imap);
            }
         } else {
            /* don't care about sign */
            if (strcmp(CMv[i]->Name, Name) == 0 ) {
               SUMA_LH("Found Map");
               imap = i;
               SUMA_RETURN(imap);
            }
         }
      }
   }
   
   SUMA_LH("Failed to find map");
   SUMA_RETURN(imap);   
}
SUMA_COLOR_MAP *SUMA_FindNamedColMap(char *Name) 
{
   static char FuncName[]={"SUMA_FindNamedColMap"};
   int imap;
   SUMA_COLOR_MAP *CM = NULL;
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->scm) SUMAg_CF->scm = SUMA_Build_Color_maps();
   if (!SUMAg_CF->scm || !SUMAg_CF->scm->CMv) SUMA_RETURN(NULL);
   if ((imap = SUMA_Find_ColorMap(Name, SUMAg_CF->scm->CMv, 
                           SUMAg_CF->scm->N_maps, -2)) >= 0) { 
      SUMA_RETURN(SUMAg_CF->scm->CMv[imap]);
   } else SUMA_RETURN(NULL);
}

SUMA_COLOR_MAP *SUMA_FindCodedColMap(int imap) 
{
   static char FuncName[]={"SUMA_FindCodedColMap"};
   SUMA_COLOR_MAP *CM = NULL;
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->scm || !SUMAg_CF->scm->CMv) SUMA_RETURN(NULL);
   if (imap < 0 || imap > SUMAg_CF->scm->N_maps-1) SUMA_RETURN(NULL);
   SUMA_RETURN(SUMAg_CF->scm->CMv[imap]);
}
/*!
   function that reads in a 1D format color map file 
   1D format contains 3 or 4 columns:
   3 columns: R G B
   4 columns: R G B frac 
   R G B are float values between 0 and 1.0 specifying the R G B colors
   frac is the fraction of the color map assigned to each color. These 
        are the same numbers shown to the right of the colormap in AFNI
      Sept 22 04: you can specify integer values between 0 and 255 for 
      the RGB values if you wish 
   
   The colormap files are specified in the order adopted in AFNI's palette files
   The last row in the file is the first color (bottom color) in the map. 
   If you specify frac then the lowest fraction must be at the bottom row
*/
SUMA_COLOR_MAP *SUMA_Read_Color_Map_1D (char *Name)
{
   static char FuncName[]={"SUMA_Read_Color_Map_1D"};
   MRI_IMAGE *im = NULL;
   float *far=NULL;
   float ColSum;
   int i=0;
   SUMA_COLOR_MAP* SM = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!Name) {
      SUMA_S_Err("NULL file name");
      SUMA_RETURN(SM);
   }
   
   /* read the colormap 1D file */
   im = mri_read_1D (Name);
   if (!im) {
      SUMA_S_Err("Failed to read file");
      SUMA_RETURN(NULL);
   }

   /* check for sizes */
   if (im->ny != 3 && im->ny != 4) {
      SUMA_S_Err("File must contain 3 or 4 columns only");
      SUMA_RETURN(NULL);
      
   }
   
   
   /* allocate for SM */
   SM = (SUMA_COLOR_MAP*) SUMA_calloc(1,sizeof(SUMA_COLOR_MAP));
   SM->idvec = NULL;
   SM->chd = NULL;
   SM->top_frac = 0.0f;
   SM->SO = NULL; 
   SM->cname = NULL;
   SM->N_M[0] = im->nx;
   SM->N_M[1] = 4;
   SM->Name = (char *)SUMA_calloc((strlen(Name)+1),sizeof(char));
   sprintf(SM->Name, "%s", Name);
   if (im->ny == 4) {
      SM->frac = (float*) SUMA_calloc(im->nx, sizeof(float));
   } else {
      SM->frac = NULL;
   }
   
   SM->M = (float**)SUMA_allocate2D (SM->N_M[0], 4, sizeof(float));
   
   
   far = MRI_FLOAT_PTR(im);
   
   ColSum = 0;
   if (im->ny == 4) {
      SM->Sgn = 1;
      for (i=0; i < im->nx; ++i) {
         SM->M[SM->N_M[0] - i - 1][0] = far[i]; 
            ColSum += far[i];
         SM->M[SM->N_M[0] - i - 1][1] = far[i+im->nx]; 
            ColSum += far[i+im->nx];
         SM->M[SM->N_M[0] - i - 1][2] = far[i+2*im->nx]; 
            ColSum += far[i+2*im->nx];
         SM->M[SM->N_M[0] - i - 1][3] = 1.0;
         SM->frac[SM->N_M[0] - i - 1] = far[i+3*im->nx];
         if (SM->frac[SM->N_M[0] - i - 1] < 0.0) SM->Sgn = -1;
      }
   } else {
      SM->Sgn = 0;
      for (i=0; i < im->nx; ++i) {
         SM->M[SM->N_M[0] - i - 1][0] = far[i]; 
            ColSum += far[i];
         SM->M[SM->N_M[0] - i - 1][1] = far[i+im->nx]; 
            ColSum += far[i+im->nx];
         SM->M[SM->N_M[0] - i - 1][2] = far[i+2*im->nx]; 
            ColSum += far[i+2*im->nx];
         SM->M[SM->N_M[0] - i - 1][3] = 1.0;
      }
   }
   
   ColSum = ColSum / (3.0 *  SM->N_M[0]);
   if (ColSum > 1) {
      /* looks like colormap values are between 0 and 255 */
      for (i=0; i < SM->N_M[0]; ++i) {
         SM->M[i][0] /= 255.0; SM->M[i][1] /= 255.0;  SM->M[i][2] /= 255.0;
      }
   }
   
   /* check on craziness in frac */
   if (SM->frac && SM->N_M[0] > 1) {
      for (i=0; i < im->nx-1; ++i) {
         if (SM->frac[i] > SM->frac[i+1]) {
            SUMA_S_Err( "Fractions must be specified in monotonic\n"
                   " descending order from the top to the bottom of the column");
            SUMA_Free_ColorMap (SM); mri_free(im);
            SUMA_RETURN(NULL);
         }
      }
   }
   
   mri_free(im); im = NULL; 
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: Colormap read:\n", FuncName);
      if (SM->frac) {
         for (i=0; i < SM->N_M[0]; ++i) {
            fprintf (SUMA_STDOUT,"%f\t%f\t%f\t%f\t%f\n", 
               SM->M[i][0], SM->M[i][1], SM->M[i][2], SM->M[i][3], SM->frac[i]);
         }
      } else SUMA_disp_mat (SM->M, SM->N_M[0], SM->N_M[1], 1);
   }
   
   SM->M0[0] = SM->M[0][0]; 
   SM->M0[1] = SM->M[0][1]; 
   SM->M0[2] = SM->M[0][2]; 
   SM->M0[3] = SM->M[0][3]; 

   SUMA_RETURN (SM);
}

SUMA_COLOR_MAP *SUMA_Read_Color_Map_NIML (char *Name)
{
   static char FuncName[]={"SUMA_Read_Color_Map_NIML"};
   SUMA_COLOR_MAP* SM = NULL;
   char *FullName = NULL, *niname = NULL;
   NI_stream ns = NULL;
   void *nini=NULL;
   SUMA_DSET *dset=NULL;
   int tt;
   SUMA_Boolean iselement = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   
   if (!Name) { SUMA_SL_Err("Null Name"); SUMA_RETURN(SM); }
   
   /* work the name */
   if (!SUMA_filexists(Name)) {
      /* try the extension game */
      FullName = SUMA_Extension(Name, ".niml.cmap", NOPE);
      if (!SUMA_filexists(FullName)) {
         SUMA_S_Errv("Failed to find cmap file %s or %s", Name, FullName); 
         if (FullName) SUMA_free(FullName); FullName = NULL;
         SUMA_RETURN(SM);
      }
   }else {
      FullName = SUMA_copy_string(Name);
   }
   
   /* got the name, now load it */
   niname = SUMA_append_string("file:", FullName);
   
   ns = NI_stream_open(niname, "r");
   if (!ns) {
      SUMA_SL_Crit("Failed to open NI stream for reading.");
      if (FullName) SUMA_free(FullName); FullName = NULL;
      SUMA_RETURN(SM);
   }
   SUMA_free(niname); niname = NULL;
   
   nini = NI_read_element(ns, 1) ; 
   NI_stream_close( ns ) ; ns = NULL;
   tt = NI_element_type(nini);
    
   SUMA_LH("Checking on nini type");
   /* check if group or element */
   if(tt == NI_GROUP_TYPE) {
      iselement = NOPE; 
      SUMA_LH("Dealing with group");
   } else if (tt == NI_ELEMENT_TYPE) { 
      iselement = YUP; 
      SUMA_S_Err("Bad format");
      if (FullName) SUMA_free(FullName); FullName = NULL;
      NI_free(nini); SUMA_RETURN(SM);
   } else {
      fprintf(SUMA_STDERR, "Note %s: %s has no element and no group. \n",
                            FuncName, Name);
      NI_free(nini); SUMA_RETURN(SM);
      if (FullName) SUMA_free(FullName); FullName = NULL;
      SUMA_RETURN(NULL);
   }
   
         
   /* change to cmap */
   SM = SUMA_NICmapToCmap((NI_group *)nini);

   /* frees */
   NI_free(nini); 
   if (FullName) SUMA_free(FullName); FullName = NULL;
   
   SUMA_RETURN (SM);
}

/*! function that turns a non-linear color map into a linear one
The larger the number of colors in the linear color map, the close the
approximation to the non-linear map. 
   \param SM (SUMA_COLOR_MAP*) a non-linear colormap (i.e. SM->frac != NULL)
   \param N_Lin (int) number of colors to use in the linear version of the map
                      if N_Lin = -1, N_Lin is set to 1024
   \return LSM (SUMA_COLOR_MAP*) linear version of the colormap
                                 NULL if SM is already linear 
                                 NULL if SM is NULL   

*/
SUMA_COLOR_MAP *SUMA_Linearize_Color_Map (SUMA_COLOR_MAP* SM, int N_lin)
{
   static char FuncName[]={"SUMA_Linearize_Color_Map"};
   SUMA_COLOR_MAP* LSM = NULL;
   int ilin = 0, i = 0, ilin_stp = -1; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!SM) {
      SUMA_S_Err("NULL color map");
      SUMA_RETURN(LSM);
   }
   
   if (!SM->frac) {
      SUMA_S_Err("NULL SM->frac!\nMap is linear");
      SUMA_RETURN(LSM);
   }
   
   if (SM->chd || SM->idvec) {
      SUMA_S_Err("No hash, and no idvecs please.");
      SUMA_RETURN(LSM);
   }
   if (N_lin < 0) N_lin = 2048;     /* 2048 set default linear map length */
   
   if (!N_lin) {
      SUMA_S_Err("N_lin = 0");
      SUMA_RETURN(LSM);
   }
   
   /* allocate for new map */
   SUMA_LH("Allocating for new map");
   LSM = (SUMA_COLOR_MAP *)SUMA_calloc(1,sizeof(SUMA_COLOR_MAP));
   if (LSM == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for LSM.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   LSM->top_frac = 0.0f;
   LSM->SO = NULL;
   
   LSM->Name = (char *)SUMA_calloc(strlen(SM->Name)+10, sizeof(char));
   if (LSM->Name == NULL) {
      fprintf (SUMA_STDERR,
               "Error %s: Failed to allocate for LSM->Name.\n", FuncName);
      SUMA_free(LSM);
      SUMA_RETURN (NULL);
   }
   sprintf(LSM->Name, "%s_lin",SM->Name); 
   LSM->N_M[0] = N_lin; LSM->N_M[1] = SM->N_M[1];
   LSM->frac = NULL;
   LSM->cname = NULL;
   LSM->Sgn = SM->Sgn;
                                       
   SUMA_LHv("Allocating for %d (%d)cols\n", LSM->N_M[0], LSM->N_M[1]);
   LSM->M = (float **)SUMA_allocate2D (LSM->N_M[0], LSM->N_M[1], sizeof(float));
   if (LSM->M == NULL) {
      fprintf (SUMA_STDERR,
               "Error %s: Failed to allocate for LSM->M.\n", FuncName);
      SUMA_free(LSM->Name);
      SUMA_free(LSM);
      SUMA_RETURN (NULL);
   }
   
   if (SM->frac[SM->N_M[0]-1] != 1.0f) {
      LSM->top_frac = SM->frac[SM->N_M[0]-1];
   }
   
   ilin = 0;
   for (i=0; i < SM->N_M[0]; ++i) {
      if (SM->N_M[1] == 4) {   
         SUMA_LHv("SM->frac[%d]=%f, tf = %f , [%f %f %f %f]", 
                  i, SM->frac[i], SM->frac[SM->N_M[0]-1], SM->M[i][0], 
                  SM->M[i][1], SM->M[i][2], SM->M[i][3]);
      } else {
         SUMA_LHv("SM->frac[%d]=%f, tf = %f , [%f %f %f]", 
                  i, SM->frac[i], SM->frac[SM->N_M[0]-1], SM->M[i][0], 
                  SM->M[i][1], SM->M[i][2]);
      }
      if (LSM->Sgn >= 0) {
         ilin_stp = (int)( ceil((double)(SM->frac[i]/SM->frac[SM->N_M[0]-1]) * 
                                (double)LSM->N_M[0]) ) - 1;
      } else {
         ilin_stp = (int)(ceil((1.0 + 
                                (double)(SM->frac[i]/SM->frac[SM->N_M[0]-1])) * 
                                (double)LSM->N_M[0]/2) ) - 1;
      }
      while (ilin < ilin_stp  && ilin < LSM->N_M[0]) {
         LSM->M[ilin][0] = SM->M[i][0];
         LSM->M[ilin][1] = SM->M[i][1];
         LSM->M[ilin][2] = SM->M[i][2];
         if (LSM->N_M[1] == 4) {
            LSM->M[ilin][3] = SM->M[i][3];
         } 
         ++ilin; 
      }
   }
   
   /* copy last value */
   LSM->M[LSM->N_M[0]-1][0] = SM->M[SM->N_M[0]-1][0];
   LSM->M[LSM->N_M[0]-1][1] = SM->M[SM->N_M[0]-1][1];
   LSM->M[LSM->N_M[0]-1][2] = SM->M[SM->N_M[0]-1][2];
   if (LSM->N_M[1] == 4) {
   LSM->M[LSM->N_M[0]-1][3] = SM->M[SM->N_M[0]-1][3];
   }
   LSM->M0[0] = LSM->M[0][0]; 
   LSM->M0[1] = LSM->M[0][1]; 
   LSM->M0[2] = LSM->M[0][2]; 
   if (LSM->N_M[1] == 4) 
   LSM->M0[3] = LSM->M[0][3]; 
   else
   LSM->M0[3] = 1.0;
   
   if (LocalHead) {
      for (i=0; i < LSM->N_M[0]; ++i) {
         if (LSM->N_M[1] == 4) 
            fprintf (SUMA_STDOUT,
                     "%d:\t%f\t%f\t%f\t%f\n", 
                     i, LSM->M[i][0], LSM->M[i][1], LSM->M[i][2], LSM->M[i][3]); 
         else
            fprintf (SUMA_STDOUT,
                     "%d:\t%f\t%f\t%f\n", 
                     i, LSM->M[i][0], LSM->M[i][1], LSM->M[i][2]); 
      }
      fprintf (SUMA_STDOUT,"%s: ilin_stp = %d\n", FuncName, ilin_stp); 
   }
   
   SUMA_RETURN(LSM);
}

/*!
   \brief Find the colormap associated with a colorplane
   \return SUMA_COLOR_MAP * NULL if error or if cmap is explicit 
*/
SUMA_COLOR_MAP *SUMA_CmapOfPlane (SUMA_OVERLAYS *Sover )
{
   static char FuncName[]={"SUMA_CmapOfPlane"};
   SUMA_COLOR_MAP *ColMap = NULL;
   int icmap;
   
   SUMA_ENTRY;
   
   if (!Sover) { SUMA_SL_Err("NULL Sover"); SUMA_RETURN(ColMap); }
   if (!Sover->cmapname) { 
      SUMA_SL_Err("NULL Colormap name"); 
      SUMA_RETURN(ColMap); 
   }
   
   if (strcmp(Sover->cmapname, "explicit") == 0) {
      SUMA_RETURN(NULL);
   }

   if (!SUMAg_CF->scm) { 
      SUMAg_CF->scm = SUMA_Build_Color_maps();
      if (!SUMAg_CF->scm) { 
         SUMA_SL_Err("Can't build color maps"); 
         SUMA_RETURN(ColMap); 
      }
   }   
   icmap = SUMA_Find_ColorMap ( Sover->cmapname, SUMAg_CF->scm->CMv, 
                                SUMAg_CF->scm->N_maps, -2 );
   if (icmap < 0) { SUMA_SL_Err("Failed to find ColMap"); SUMA_RETURN(ColMap); }
   ColMap = SUMAg_CF->scm->CMv[icmap];
   
   SUMA_RETURN(ColMap);
   
}

/* Removes pre-existing bias
   DOES NOT free bias vector 
   DOES NOT SET OptScl->DoBias to SW_CoordBias_None
   The last two operations should be carried out once the bias is removed from all surfaces
   to which they'd been applied. In this case it will be in SUMA_RemoveCoordBias
   This function should not be called directly, only from SUMA_RemoveCoordBias
*/
SUMA_Boolean SUMA_RemoveSO_CoordBias(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *ovr)
{
   static char FuncName[]={"SUMA_RemoveSO_CoordBias"};
   int i, i3, x_i3;
      
   SUMA_ENTRY;
   
   if (!SO || !ovr) {
      SUMA_SL_Err("Dim dim diM");
      SUMA_RETURN(NOPE);    
   }
   x_i3 = 3*SO->N_Node;
   if (ovr->OptScl->BiasVect) { /* something to be removed */
      switch (ovr->OptScl->DoBias) {
         case SW_CoordBias_X:
            /* Remove X bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i];
               if (i3 < x_i3) SO->NodeList[i3] -= ovr->OptScl->BiasVect[i];
            }
            break;
         case SW_CoordBias_Y:
            /* Remove Y bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i]+1;
               if (i3 < x_i3) SO->NodeList[i3] -= ovr->OptScl->BiasVect[i];
            }
            break;
         case SW_CoordBias_Z:
            /* Remove Z bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i]+2;
               if (i3 < x_i3) SO->NodeList[i3] -= ovr->OptScl->BiasVect[i];
            }
            break;
         case SW_CoordBias_N:
            /* Remove Normal bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i];
               if (i3 < x_i3) {
                  SO->NodeList[i3] -=  ovr->OptScl->BiasVect[i] * 
                                       SO->NodeNormList[i3]; ++i3; 
                  SO->NodeList[i3] -=  ovr->OptScl->BiasVect[i] * 
                                       SO->NodeNormList[i3]; ++i3; 
                  SO->NodeList[i3] -=  ovr->OptScl->BiasVect[i] * 
                                       SO->NodeNormList[i3];  
               }
            }
            break;
         default:
            SUMA_SL_Err("This should not be");
            SUMA_RETURN(NOPE);
      }
   } else {
      SUMA_SL_Err("DO not call me with no bias!");
      SUMA_RETURN(NOPE);
   }

   /* Update surface geometry properties */
   SUMA_NewSurfaceGeometry(SO);

   SUMA_RETURN(YUP);
}

/* Removes pre-existing bias to old dimension 
   Add same bias to BiasDim dimension
   sets OptScl->DoBias to BiasDim
*/
SUMA_Boolean SUMA_TransferCoordBias(SUMA_OVERLAYS *ovr, 
                                    SUMA_WIDGET_INDEX_COORDBIAS BiasDim)
{
   static char FuncName[]={"SUMA_TransferCoordBias"};
   SUMA_SurfaceObject *SO=NULL;
   int iso;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   if (!ovr) SUMA_RETURN(YUP);
      
   for (iso=0; iso<SUMAg_N_DOv; ++iso) {
      if (SUMA_isSO(SUMAg_DOv[iso])) {
         SO = (SUMA_SurfaceObject *)SUMAg_DOv[iso].OP;
         if (SUMA_isOverlayOfSO(SO, ovr)) {
            SUMA_TransferSO_CoordBias(SO, ovr, BiasDim);   
         } 
      }
   }

   SUMA_LH("Setting bias flag");
   ovr->OptScl->DoBias = BiasDim;

   SUMA_RETURN(YUP);

}

/*!
   Single surface version of SUMA_TransferCoordBias DO NOT CALL THIS FUNCTION OUTSIDE OF SUMA_TransferCoordBias
*/
SUMA_Boolean SUMA_TransferSO_CoordBias(SUMA_SurfaceObject *SO, 
                     SUMA_OVERLAYS *ovr, SUMA_WIDGET_INDEX_COORDBIAS BiasDim)
{
   static char FuncName[]={"SUMA_TransferSO_CoordBias"};
   SUMA_Boolean LocalHead = NOPE;
   int i, i3;
      
   SUMA_ENTRY;
   
   SUMA_LH("Called");
   
   
   if (ovr->OptScl->BiasVect) {
      SUMA_LH("Removing old bias"); 
      /* Remove old bias */
      switch (ovr->OptScl->DoBias) {
         case SW_CoordBias_X:
            /* Remove X bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i];
               SO->NodeList[i3] -= ovr->OptScl->BiasVect[i];
            }
            break;
         case SW_CoordBias_Y:
            /* Remove Y bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i]+1;
               SO->NodeList[i3] -= ovr->OptScl->BiasVect[i];
            }
            break;
         case SW_CoordBias_Z:
            /* Remove Z bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i]+2;
               SO->NodeList[i3] -= ovr->OptScl->BiasVect[i];
            }
            break;
         case SW_CoordBias_N:
            /* Remove Normal bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i];
               SO->NodeList[i3] -=  ovr->OptScl->BiasVect[i] * 
                                    SO->NodeNormList[i3]; ++i3; 
               SO->NodeList[i3] -=  ovr->OptScl->BiasVect[i] * 
                                    SO->NodeNormList[i3]; ++i3; 
               SO->NodeList[i3] -=  ovr->OptScl->BiasVect[i] * 
                                    SO->NodeNormList[i3];  
            }
            break;
         default:
            SUMA_SL_Err("This should not be");
            SUMA_RETURN(NOPE);
      }
      SUMA_LH("Adding new bias");
      
      #if 0
      /* Add same bias to other direction */
      switch (BiasDim) {
         case SW_CoordBias_X:
            /* Add X bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i];
               SO->NodeList[i3] += ovr->OptScl->BiasVect[i];
            }
            break;
         case SW_CoordBias_Y:
            /* Add Y bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i]+1;
               SO->NodeList[i3] += ovr->OptScl->BiasVect[i];
            }
            break;
         case SW_CoordBias_Z:
            /* Add Z bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i]+2;
               SO->NodeList[i3] += ovr->OptScl->BiasVect[i];
            }
            break;
         case SW_CoordBias_N:
            /* Add Normal bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i];
               SO->NodeList[i3] +=  ovr->OptScl->BiasVect[i] * 
                                    SO->NodeNormList[i3]; ++i3; 
               SO->NodeList[i3] +=  ovr->OptScl->BiasVect[i] * 
                                    SO->NodeNormList[i3]; ++i3; 
               SO->NodeList[i3] +=  ovr->OptScl->BiasVect[i] *    
                                    SO->NodeNormList[i3];  
            }
            break;
         default:
            SUMA_SL_Err("This should not be.\nWhy, oh why ?");
            SUMA_RETURN(NOPE);
      }
      #else 
      /* Add same bias to other direction */
      SUMA_ADD_COORD_BIAS_VECT(SO, ovr, BiasDim, ovr->OptScl->BiasVect);
      #endif
   }
   
   /* Update surface geometry properties */
   SUMA_NewSurfaceGeometry(SO);
   SUMA_RETURN(YUP);
}

/*!
   Function called when a surface's geometry is changed (currently due to a change in the CoordBias)
*/
SUMA_Boolean SUMA_NewSurfaceGeometry(SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_NewSurfaceGeometry"};
   int ii, i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* first recompute the bounding box of the surface */
   /* Calculate Min, Max, Mean */
   
   SUMA_MIN_MAX_SUM_VECMAT_COL ( SO->NodeList, SO->N_Node, SO->NodeDim, 
                                 SO->MinDims, SO->MaxDims, SO->Center);
     
   SO->Center[0] /= SO->N_Node;
   SO->Center[1] /= SO->N_Node;
   SO->Center[2] /= SO->N_Node;

   SUMA_MIN_VEC (SO->MinDims, 3, SO->aMinDims );
   SUMA_MAX_VEC (SO->MaxDims, 3, SO->aMaxDims);
   
   /* find out what viewers this surface is registered with 
      and which viewers show it */
   for (ii=0; ii<SUMAg_N_SVv; ++ii) {
      if (!SUMAg_SVv[ii].isShaded && SUMAg_SVv[ii].X->TOPLEVEL) {
         for (i=0; i< SUMAg_SVv[ii].N_DO; ++i) {
            if (SUMA_isSO_G(  SUMAg_DOv[SUMAg_SVv[ii].RegisteredDO[i]], 
                              SUMAg_SVv[ii].CurGroupName)) {
               /* is this surface the same as SO ? */
               if (SUMA_findSO_inDOv(SO->idcode_str, SUMAg_DOv, SUMAg_N_DOv) == 
                   SUMAg_SVv[ii].RegisteredDO[i]) {
                  /* This surface is visible in this viewer, mark that viewer  */
                  SUMA_LH("Marking Viewer ");
                  SUMAg_SVv[ii].NewGeom = YUP;
               }
            }
         }
      }
   }
   
   SUMA_RETURN(YUP);
}

  

/* 
   -  adds new bias 
   -  DOES NOT copy NewBias to OptScl->BiasVect 
   -  DOES NOT set OptScl->DoBias to BiasDim
   The previous 2 operations are to be done in SUMA_SetCoordBias which call this function for all
   surfaces using this overlay plane 
   
*/
SUMA_Boolean SUMA_SetSO_CoordBias(  SUMA_SurfaceObject *SO, SUMA_OVERLAYS *ovr,                                       float *NewBias, 
                                    SUMA_WIDGET_INDEX_COORDBIAS BiasDim)
{
   static char FuncName[]={"SUMA_SetSO_CoordBias"};
   int i, i3;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ovr) { 
      SUMA_SL_Err("NULL ovr");
      SUMA_RETURN(NOPE);
   }
   if (!ovr->NodeDef) {
      SUMA_SL_Err("NULL ovr->NodeDef");
      SUMA_RETURN(NOPE);
   }
   /* Now add the new one */
   if (NewBias) {
      #if 0
      switch (BiasDim) {
         case SW_CoordBias_X:
            /* Add X bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i];
               SO->NodeList[i3] += NewBias[i];
            }
            break;
         case SW_CoordBias_Y:
            /* Add Y bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i]+1;
               SO->NodeList[i3] += NewBias[i];
            }
            break;
         case SW_CoordBias_Z:
            /* Add Z bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i]+2;
               SO->NodeList[i3] += NewBias[i];
            }
            break;
         case SW_CoordBias_N:
            /* Add Normal bias */
            for (i=0; i < ovr->N_NodeDef; ++i) {
               i3 = 3*ovr->NodeDef[i];
               SO->NodeList[i3] += NewBias[i] * SO->NodeNormList[i3]; ++i3; 
               SO->NodeList[i3] += NewBias[i] * SO->NodeNormList[i3]; ++i3; 
               SO->NodeList[i3] += NewBias[i] * SO->NodeNormList[i3];  
            }
            break;
         case SW_CoordBias_None:
            /* That should not be if NewBias is not NULL */
            SUMA_SL_Err("Why are you calling me with SW_CoordBias_None and a non-null NewBias?");
            SUMA_RETURN(NOPE);
            break;
         default:
            SUMA_SL_Err("This should not be.\nNot at all.");
            SUMA_RETURN(NOPE); 
      }
      #else
      /* Add bias to  direction */
      SUMA_ADD_COORD_BIAS_VECT(SO, ovr, BiasDim, NewBias);
      #endif  

   } else {/* nothing to add (0 bias)*/

   }

   /* Update surface geometry properties */
   SUMA_NewSurfaceGeometry(SO);
   SUMA_RETURN(YUP);   
}

/*!
   Sets the coordinate bias for surfaces using a particular overlay plane
   Do NOT free NewBias upon return since this pointer will replace OptScl->BiasVect
   DO NOT CALL THIS FUNCTION IF ovr->NodeDef has been modified, otherwise  SUMA_RemoveSO_CoordBias will fail
*/

SUMA_Boolean SUMA_SetCoordBias(SUMA_OVERLAYS *ovr, float *NewBias, SUMA_WIDGET_INDEX_COORDBIAS BiasDim)
{
   static char FuncName[]={"SUMA_SetCoordBias"};
   int i, i3, iso;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   if (!ovr) SUMA_RETURN(YUP);
   
   if (ovr->OptScl->BiasVect ) {
         SUMA_SL_Err("Can't have Non NULL bias here");
         SUMA_Show_ColorOverlayPlanes(&ovr,1,1);
         SUMA_RETURN(NOPE);
   }
   
   for (iso=0; iso<SUMAg_N_DOv; ++iso) {
      if (SUMA_isSO(SUMAg_DOv[iso])) {
         SO = (SUMA_SurfaceObject *)SUMAg_DOv[iso].OP;
         if (SUMA_isOverlayOfSO(SO, ovr)) {
            SUMA_LH(SO->Label);
            SUMA_SetSO_CoordBias(SO, ovr, NewBias, BiasDim);   
         } 
      }
   }

   ovr->OptScl->BiasVect = NewBias;   
   ovr->OptScl->DoBias = BiasDim;

   SUMA_RETURN(YUP);
}

SUMA_Boolean SUMA_RemoveCoordBias(SUMA_OVERLAYS *ovr) 
{
   static char FuncName[]={"SUMA_RemoveCoordBias"};
   int i, i3, iso;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   if (!ovr) SUMA_RETURN(YUP);
   if (ovr->OptScl->BiasVect) { /* something to be removed */
      for (iso=0; iso<SUMAg_N_DOv; ++iso) {
         if (SUMA_isSO(SUMAg_DOv[iso])) {
            SO = (SUMA_SurfaceObject *)SUMAg_DOv[iso].OP;
            if (SUMA_isOverlayOfSO(SO, ovr)) {
               SUMA_LH(SO->Label);
               SUMA_RemoveSO_CoordBias(SO, ovr);     
            } 
         }
      }
      /* Now free BiasVect */
      SUMA_free(ovr->OptScl->BiasVect); 
   }
   ovr->OptScl->BiasVect = NULL;
   ovr->OptScl->DoBias = SW_CoordBias_None;

   SUMA_RETURN(YUP);
} 

/*!
   
   -  Some of the fields of SUMA_SCALE_TO_MAP_OPT are ignored here.
   1- Find the colormap structure 
   2- Linearize the colormap if need be
   3- Apply a global brightness coefficient (if any) on the 
      colors in the colormap   
   4- Loop accross each node in the intensity column
      if its threshold meets the cut-off,
      colorize it
      
*/
SUMA_Boolean SUMA_ScaleToMap_Interactive (   SUMA_OVERLAYS *Sover )
{
   static char FuncName[]={"SUMA_ScaleToMap_Interactive"};
   float *V=NULL, *T=NULL, *B=NULL;
   int i, icmap, i3, cnt, cnt3, loc[2], *nd=NULL;
   double Range[2];
   double minB, maxB, fact=0.0, floc = 0.0;
   SUMA_COLOR_MAP *ColMap = NULL;
   SUMA_SCALE_TO_MAP_OPT *Opt = NULL;
   SUMA_COLOR_SCALED_VECT * SV = NULL;
   float *junk;
   static int nwarn=0;
   SUMA_WIDGET_INDEX_COORDBIAS HoldBiasOpt;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   
   if (!Sover) { SUMA_SL_Err("NULL Sover"); SUMA_RETURN(NOPE); }
   if (!Sover->cmapname) { 
      SUMA_SL_Err("NULL Colormap name"); SUMA_RETURN(NOPE); 
   }
   if (!SUMAg_CF->scm) { 
      SUMAg_CF->scm = SUMA_Build_Color_maps();
      if (!SUMAg_CF->scm) { 
         SUMA_SL_Err("Can't build color maps"); 
         SUMA_RETURN(NOPE);  
      }
   }
   SUMA_LH("Finding ColorMap");   
   icmap = SUMA_Find_ColorMap (  Sover->cmapname, 
                                 SUMAg_CF->scm->CMv, SUMAg_CF->scm->N_maps, -2 );
   if (icmap < 0) { 
      SUMA_SL_Err("Failed to find ColMap");
      SUMA_S_Errv("Missing ColMap called %s\n", Sover->cmapname); 
      SUMA_RETURN(NOPE); 
   }
   ColMap = SUMAg_CF->scm->CMv[icmap];
   
   Opt = Sover->OptScl;
   
   SUMA_LH("Creating a scaled color vect");   
   if (Sover->ShowMode == SW_SurfCont_DsetViewCon ||
       Sover->ShowMode == SW_SurfCont_DsetViewCaC ) {
      if (SUMA_NeedsLinearizing(ColMap)) {
         if (!nwarn) {
            SUMA_SLP_Note("Cannot do contouring with colormaps\n"
                          "that panes of unequal sizes.\n"
                          "Notice shown once per session.");
            ++nwarn;
         }
         Opt->ColsContMode = 0;
      } else {
         Opt->ColsContMode = 1;
      }
   } else {
      Opt->ColsContMode = 0;
   }
   SV = SUMA_Create_ColorScaledVect(SDSET_VECFILLED(Sover->dset_link), 
                                    Opt->ColsContMode);
   
   SUMA_LH("Fetching vetors from dset");
   T = NULL; V = NULL; B = NULL;
   /* Thresholding ? */
   if (Opt->tind >= 0 && Opt->UseThr) { 
      SUMA_LH("Fetching Threshold column");
      /* got to copy values into float vectors 'cause of possible types */
      T = SUMA_DsetCol2Float (Sover->dset_link, Opt->tind, 0);
      if (!T) { SUMA_SL_Err("Failed to get T"); SUMA_RETURN(NOPE); }
      switch (Opt->ThrMode) {
         case SUMA_LESS_THAN:
            for (i=0; i<SDSET_VECFILLED(Sover->dset_link); ++i) {
               if (T[i] < Opt->ThreshRange[0]) {
                  SV->isMasked[i] = YUP; /* Mask */
               }
            }
            break;
         case SUMA_ABS_LESS_THAN:
            for (i=0; i<SDSET_VECFILLED(Sover->dset_link); ++i) {
               if (T[i] < Opt->ThreshRange[0] && T[i] > -Opt->ThreshRange[0]) {
                  SV->isMasked[i] = YUP; /* Mask */
               }
            }
            break;
         case SUMA_THRESH_OUTSIDE_RANGE:
            for (i=0; i<SDSET_VECFILLED(Sover->dset_link); ++i) {
               if (T[i] < Opt->ThreshRange[0] || T[i] > Opt->ThreshRange[1]) {
                  SV->isMasked[i] = YUP; /* Mask */
               }
            }
            break;
         case SUMA_THRESH_INSIDE_RANGE:
            for (i=0; i<SDSET_VECFILLED(Sover->dset_link); ++i) {
               if (T[i] > Opt->ThreshRange[0] && T[i] < Opt->ThreshRange[1]) {
                  SV->isMasked[i] = YUP; /* Mask */
               }
            }
            break;       
         default:
            SUMA_SL_Err("Wrond threshold mode");
            SUMA_RETURN(NOPE);
            break;
      }
      /* T is no longer needed */
      if (T) SUMA_free(T); T = NULL;
   } 
   
   SUMA_LH("Fetching Intensity column");
   if (Opt->find < 0) { SUMA_SL_Crit("Bad column index.\n"); SUMA_RETURN(NOPE); }
   else { 
      /* got to copy values into float vectors 'cause of possible types */
      V = SUMA_DsetCol2Float (Sover->dset_link, Opt->find, 0);
      if (!V) { SUMA_SL_Err("Failed to get V"); SUMA_RETURN(NOPE); }
   }
   
   /* colorizing */
   if ( (Opt->interpmode == SUMA_DIRECT)&& 
        (SUMA_is_Label_dset(Sover->dset_link,NULL)) ) {
      SUMA_LH("Scaling a la HASH");
      if (!SUMA_ScaleToMap_alaHASH (V, SDSET_VECFILLED(Sover->dset_link), 
                                    ColMap, Opt,
                                    SV) ) {
         SUMA_SL_Err("Failed in SUMA_ScaleToMap_alaHASH");
         SUMA_RETURN(NOPE);   
      }     
   } else if (Opt->alaAFNI) {
      /* a la AFNI */
      SUMA_LH("Scaling a la AFNI");
      if (!SUMA_ScaleToMap_alaAFNI (V, SDSET_VECFILLED(Sover->dset_link),
                              SUMA_LARG_ABS(Opt->IntRange[0], Opt->IntRange[1]), 
                                    ColMap, Opt,
                                    SV) ) {
         SUMA_SL_Err("Failed in SUMA_ScaleToMap_alaAFNI");
         SUMA_RETURN(NOPE);   
      }
   } else { 
      /* a la SUMA */
      SUMA_LH("Scaling a la SUMA");
      if (!SUMA_ScaleToMap( V, SDSET_VECFILLED(Sover->dset_link),
                            Opt->IntRange[0], Opt->IntRange[1], 
                            ColMap, Opt,
                            SV) ){
         SUMA_SL_Err("Failed in  SUMA_ScaleToMap");
         SUMA_RETURN(NOPE);                    
      }
   }                              
   
   
   if (Opt->bind >= 0 && Opt->UseBrt) {
      SUMA_LH("Brightness modulation needed"); 
      /* got to copy values into float vectors 'cause of possible types */
      B = SUMA_DsetCol2Float (Sover->dset_link, Opt->bind, 0);
      if (!B) { SUMA_SL_Err("Failed to get B"); SUMA_RETURN(NOPE); }
      /* go over B and clip it, if needed */
      if (Opt->BrightRange[0] && Opt->BrightRange[1]) {
         /* need to clip B */
         minB = Opt->BrightRange[0]; maxB = Opt->BrightRange[1];
         for (i=0; i<SDSET_VECFILLED(Sover->dset_link); ++i) {
            if (!SV->isMasked[i]) {
               /* worry only about unmasked colors */
               if (B[i] < Opt->BrightRange[0]) B[i] = Opt->BrightRange[0];
               else if (B[i] > Opt->BrightRange[1]) B[i] = Opt->BrightRange[1];
            }
         }
      } else {
         if (!SUMA_GetDsetColRange(Sover->dset_link, Opt->bind, Range, loc)) { 
            SUMA_SL_Err("Failed to get ColRange!"); 
            SUMA_RETURN(NOPE); 
         }
         minB = Range[0]; maxB = Range[1];
      }
      /* Now scale B and modulate colors in SV*/
      SUMA_LH("Scaling by B");
      fact = (Opt->BrightMap[1] - Opt->BrightMap[0]) / (maxB - minB);
      for (i=0; i<SDSET_VECFILLED(Sover->dset_link); ++i) {
         if (!SV->isMasked[i]) {
            /* B[i] = (B[i] - minB) * fact; SV->cV[i] = SV->cV[i] * B[i]; */
            i3 = 3*i;
            floc = (B[i] - minB) * fact + Opt->BrightMap[0];
            SV->cV[i3   ] = SV->cV[i3   ] * floc;
            SV->cV[i3 +1] = SV->cV[i3 +1] * floc;
            SV->cV[i3 +2] = SV->cV[i3 +2] * floc;
         } 
      }
   }
   
   /* remove any bias before NodeDef is modified */
   HoldBiasOpt = Opt->DoBias; 
      /* This field gets wiped (by SUMA_RemoveCoordBias) from Opt 
         (which is inside Sover) */
   if (  Opt->DoBias == SW_CoordBias_X || 
         Opt->DoBias == SW_CoordBias_Y || 
         Opt->DoBias == SW_CoordBias_Z || 
         Opt->DoBias == SW_CoordBias_N ) {
      SUMA_RemoveCoordBias(Sover); 
   }
   
   /* finally copy results */
   SUMA_LH("Copying into NodeDef");
   nd = SUMA_GetNodeDef(Sover->dset_link);
   if (nd) {
      cnt = 0;
      for (i=0; i<SDSET_VECFILLED(Sover->dset_link); ++i) {
         if (!SV->isMasked[i]) {
            cnt3 = 3*cnt; i3 = 3*i;
            Sover->NodeDef[cnt] = nd[i];
            Sover->ColVec[cnt3   ] = SV->cV[i3   ];
            Sover->ColVec[cnt3 +1] = SV->cV[i3 +1];
            Sover->ColVec[cnt3 +2] = SV->cV[i3 +2]; 
            if (SV->BiasCoordVec) 
               SV->BiasCoordVec[cnt] = SV->BiasCoordVec[i]; 
                  /* a compression of BiasCoordVec vector to match NodeDef */ 
            ++cnt;
         }
      }
   } else {
      cnt = 0;
      for (i=0; i<SDSET_VECFILLED(Sover->dset_link); ++i) {
         if (!SV->isMasked[i]) {
            cnt3 = 3*cnt; i3 = 3*i;
            Sover->NodeDef[cnt] = i;
            Sover->ColVec[cnt3   ] = SV->cV[i3   ];
            Sover->ColVec[cnt3 +1] = SV->cV[i3 +1];
            Sover->ColVec[cnt3 +2] = SV->cV[i3 +2];
            if (SV->BiasCoordVec) 
               SV->BiasCoordVec[cnt] = SV->BiasCoordVec[i]; 
                  /* a compression of BiasCoordVec vector to match NodeDef */ 
            ++cnt;
         }
      }
   }
   Sover->N_NodeDef = cnt;
   Sover->FullList = NOPE;
   
   
   /* Add any coord bias ? */
   switch (HoldBiasOpt) {
      case SW_CoordBias_X:
         SUMA_LH("Bias X");
         SUMA_SetCoordBias(Sover, SV->BiasCoordVec, SW_CoordBias_X);
         SV->BiasCoordVec = NULL; 
            /* set SV->BiasCoordVec to NULL to keep it from being freed below */
         break;
      case SW_CoordBias_Y:
         SUMA_LH("Bias Y");
         SUMA_SetCoordBias(Sover, SV->BiasCoordVec, SW_CoordBias_Y);
         SV->BiasCoordVec = NULL; 
            /* set SV->BiasCoordVec to NULL to keep it from being freed below */
         break;
      case SW_CoordBias_Z:
         SUMA_LH("Bias Z");
         SUMA_SetCoordBias(Sover, SV->BiasCoordVec, SW_CoordBias_Z);
         SV->BiasCoordVec = NULL; 
            /* set SV->BiasCoordVec to NULL to keep it from being freed below */
         break;
      case SW_CoordBias_N:
         SUMA_LH("Bias N");
         SUMA_SetCoordBias(Sover, SV->BiasCoordVec, SW_CoordBias_N);
         SV->BiasCoordVec = NULL; 
            /* set SV->BiasCoordVec to NULL to keep it from being freed below */
         break;
      default:
         SUMA_LH("Bias None");
         break;   
   }
   
   /* Do we need to create contours */
   if (Opt->ColsContMode) {
      if (SUMA_is_Label_dset(Sover->dset_link,NULL))
         SUMA_ContourateDsetOverlay(Sover, NULL);
      else
         SUMA_ContourateDsetOverlay(Sover, SV);
   }
   
   if (LocalHead) {
      SUMA_LH("In Scale_Interactive\n**********************");
      /* SUMA_Show_ColorOverlayPlanes (&Sover, 1, 1);  */
   }

   
   /* clean up */
   SUMA_LH("Cleanup, cleanup, everybody cleanup");
   if (T) SUMA_free(T); T = NULL;
   if (V) SUMA_free(V); V = NULL;
   if (B) SUMA_free(B); B = NULL;
   if (SV)  SUMA_Free_ColorScaledVect (SV); SV = NULL;
   SUMA_RETURN(YUP);
   
}

SUMA_Boolean SUMA_ScaleToMap_alaAFNI ( float *V, int N_V, 
                                       float range, 
                                       SUMA_COLOR_MAP *ColMap, 
                                       SUMA_SCALE_TO_MAP_OPT *Opt, 
                                       SUMA_COLOR_SCALED_VECT * SV)
{
   static char FuncName[]={"SUMA_ScaleToMap_alaAFNI"};
   int i,j, i0, i1, i3, mxColindex;
   float Vmin, Vmax, MinCol, MaxCol, Vrange, Vscl, r, **Mbuf= NULL;
   SUMA_Boolean NewMap = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   /* Autorange ?*/
   
   if (range == 0.0) {
      /* autorange, find min-max values */
      Vmin = V[0]; Vmax = V[0];
      for (i=0; i < N_V; ++i) {
         if (V[i] > Vmax) Vmax = V[i];
         else if (V[i] < Vmin) Vmin = V[i];
      }
   } else {
      if (ColMap->Sgn >= 0) {
         Vmin = 0.0;
         Vmax = range;
      }else {
         Vmin = -range;
         Vmax = range;
      }
   }
   
   if (ColMap->top_frac > 0.0f) {
      SUMA_LH("Using top_frac, not commonly used. ");
      Vmax *= ColMap->top_frac;
      Vmin *= ColMap->top_frac;
   } 
   
   /* find the values to be masked out */
   if (Opt->ApplyMask){
      if (Opt->MaskZero) {
         /* mask zeros and values in range */
         for (i=0; i < N_V; ++i) {
            if (!V[i] || ( V[i] >= Opt->MaskRange[0] && 
                           V[i] <= Opt->MaskRange[1]) ) SV->isMasked[i] = YUP;
         } 
      } else {
         /* don't mask zeros, just range */
         for (i=0; i < N_V; ++i) {
            if (V[i] >= Opt->MaskRange[0] && V[i] <= Opt->MaskRange[1]) 
                              SV->isMasked[i] = YUP;
         } 
      }
   } else {
      if (Opt->MaskZero) {
         /* mask zeros */
         for (i=0; i < N_V; ++i) {
            if (!V[i]) SV->isMasked[i] = YUP;
         }
      }
   }
   
   /* go through and clip values in V to those specified in the range */
   if (Opt->ApplyClip) {
      SUMA_S_Warn("Option Opt->ApplyClip not applicable here.\nOption ignored.");
   }
   for (i=0; i < N_V; ++i) {
      if (!SV->isMasked[i]) { /* don't waist time on masked stuff */
         if (V[i] > Vmin) { 
            /* that's cool */
         } else {
            V[i] = Vmin;
         }

         if (V[i] < Vmax) { 
            /* that's cool */
         } else {
            V[i] = Vmax;
         }
      } 
   }
   
   /* is the colormap non-linear ? */
   if (SUMA_NeedsLinearizing(ColMap)) {
      /* linearize color map */
      SUMA_LH("Linearizing colormap ...");
      NewMap = YUP;
      if (ColMap->frac[0] > 0 && ColMap->Sgn == -1) {
         SUMA_S_Warn ("Color map fractions are positive with Sgn flag = -1");
      }
      if (ColMap->frac[0] < 0 && ColMap->Sgn == 1) {
         SUMA_S_Warn ("Color map fractions are negative with Sgn flag = 1");
      }
      ColMap = SUMA_Linearize_Color_Map (ColMap, -1);
      if (LocalHead) {
         FILE *lincmap=NULL;
         int ii = 0;
         lincmap=fopen("./lincmap.1D", "w");
         if (lincmap) {
            SUMA_LH("Linearized map written to ./lincmap.1D");
            /* use simple format to allow for easy read in matlab */
            /* SUMA_Show_ColorMapVec (&ColMap, 1, lincmap, 2); */
            for (ii=ColMap->N_M[0]-1; ii >=0; --ii) {
               if (ColMap->N_M[1] == 4) 
                  fprintf (lincmap, 
                     "%d\t%f\t%f\t%f\t%f\n", 
                     ii, ColMap->M[ii][0], ColMap->M[ii][1],
                         ColMap->M[ii][2], ColMap->M[ii][3]);
               else
                  fprintf (lincmap, 
                     "%d\t%f\t%f\t%f\n", 
                     ii, ColMap->M[ii][0], ColMap->M[ii][1],ColMap->M[ii][2]);
            }
            fclose (lincmap); lincmap = NULL;
         }else {
            SUMA_SL_Err("Failed to write linearized colormap to file.\n"
                        "Proceeding...");
         }
      }
      
   }else {
      SUMA_LH("NO Linearizing of colormap deemed necessary...");
      NewMap = NOPE;
   }
   
   /* if brightness factor is given, apply it to color map and mask color */
   Mbuf = NULL;
   if (Opt->BrightFact <= 0 || Opt->BrightFact > 1) {
      fprintf (SUMA_STDERR,
               "Error %s: Opt->BrightFact must be between ]0 1]\n", FuncName);
      SUMA_RETURN (NOPE);
   }else {
      if (Opt->BrightFact != 1) { 
         Mbuf = ColMap->M; /* save pointer */
         ColMap->M = (float **)SUMA_allocate2D( ColMap->N_M[0], 
                                                ColMap->N_M[1], sizeof(float));
         for (i=0; i < ColMap->N_M[0]; ++i) {
            ColMap->M[i][0] = Mbuf[i][0] * Opt->BrightFact;
            ColMap->M[i][1] = Mbuf[i][1] * Opt->BrightFact;
            ColMap->M[i][2] = Mbuf[i][2] * Opt->BrightFact;
            if (ColMap->N_M[1] == 4) {
            ColMap->M[i][3] = Mbuf[i][3] * Opt->BrightFact;
            }
         }
         /* now for the mask color */
         Opt->MaskColor[0] *= Opt->BrightFact;
         Opt->MaskColor[1] *= Opt->BrightFact;
         Opt->MaskColor[2] *= Opt->BrightFact;
         if (ColMap->N_M[1] == 4) {
         Opt->MaskColor[3] *= Opt->BrightFact;
         }
      }
   }
   
   if (  Opt->interpmode != SUMA_DIRECT && 
         Opt->interpmode != SUMA_NO_INTERP && Opt->interpmode != SUMA_INTERP) {
      fprintf (SUMA_STDERR,
               "Error %s: Opt->interpmode is incorrectly specifed (%d).\n", 
               FuncName, Opt->interpmode);
      SUMA_RETURN(NOPE);
   }
   
   if (Opt->interpmode == SUMA_INTERP || Opt->interpmode == SUMA_NO_INTERP) {
      /* Now go through values and interpolate onto index of colormap */
      MinCol = 0.0; MaxCol = (float)ColMap->N_M[0]; 
      Vrange = Vmax - Vmin; 
      if (LocalHead) 
         fprintf(SUMA_STDERR,
                 "%s: [Vrange, Vmax, Vmin] = [%f, %f, %f]\nInterpMode=%d\n", 
                 FuncName, Vrange, Vmax, Vmin, Opt->interpmode);
      if (Vrange < 0) {
         fprintf (SUMA_STDERR,
                  "Error %s: Vmax (%f)< Vmin(%f).\n", FuncName, Vmax, Vmin);
         SUMA_RETURN (NOPE);
      }

      if (Vrange > 0) {
         mxColindex = ColMap->N_M[0] -1; 
         if (Opt->interpmode == SUMA_NO_INTERP) {
               SUMA_LH("No_Interp mode");
               for (i=0; i < N_V; ++i) {
                  i3 = 3*i;
                  if (!SV->isMasked[i]) {
                     Vscl = (V[i] - Vmin) / Vrange * ColMap->N_M[0]; 
                        /* used mxColindex instead of N_M[0] (wrong!) 
                           prior to Oct 22, 03 */
                     if (Vscl < 0) Vscl = 0; 
                     if (Vscl > ColMap->N_M[0]) Vscl = ColMap->N_M[0]; 
                        /* This happens when your Min--Max are within the 
                           boundaries of the data's (V[i]) min to max */
                     i0 = (int)(Vscl); 
                     if (i0 > mxColindex) i0 = mxColindex;  
                        /* No need, Vscl's clipping takes care of that: 
                           if (i0 < 0) i0 = 0; */
                     if (LocalHead) {
                        fprintf(SUMA_STDERR,
                                "%s: %f-->%f: Colmap index is %d\n", 
                                FuncName, V[i], Vscl, i0);
                     }
                     if (ColMap->M[i0][0] >= 0) { /* good color */
                        SV->cV[i3  ] = ColMap->M[i0][0];
                        SV->cV[i3+1] = ColMap->M[i0][1];
                        SV->cV[i3+2] = ColMap->M[i0][2];
                     } else { /* mask color */
                        SV->isMasked[i] = YUP;
                        SV->cV[i3  ] = Opt->MaskColor[0]; 
                        SV->cV[i3+1] = Opt->MaskColor[1]; 
                        SV->cV[i3+2] = Opt->MaskColor[2];
                     }
                  } else {
                     SV->cV[i3  ] = Opt->MaskColor[0]; 
                     SV->cV[i3+1] = Opt->MaskColor[1]; 
                     SV->cV[i3+2] = Opt->MaskColor[2]; 
                  }
                  if (LocalHead) {
                        fprintf(SUMA_STDERR,
                               "%s: %f-->[%f %f %f]\n", FuncName, 
                               V[i], SV->cV[i3  ], SV->cV[i3+1], SV->cV[i3+2]);
                  }
               }
            } else { 
               SUMA_LH("Interp mode");
               for (i=0; i < N_V; ++i) {
                  i3 = 3*i;
                  if (!SV->isMasked[i]) {
                     Vscl = (V[i] - Vmin) / Vrange * ColMap->N_M[0]; 
                           /* used mxColindex instead of N_M[0] (wrong!) 
                              prior to Oct 22, 03 */ 
                     if (Vscl < 0) Vscl = 0; 
                     if (Vscl > ColMap->N_M[0]) Vscl = ColMap->N_M[0]; 
                           /* This happens when your Min--Max are within the 
                              boundaries of the data's (V[i]) min to max */
                     /*now linearly interpolate between the two closest colors 
                     in the color map */
                     i0 = (int)(Vscl); 
                     if (i0 > mxColindex) i0 = mxColindex;  
                           /* No need, Vscl's clipping takes care of that: 
                              if (i0 < 0) i0 = 0; */
                     i1=i0+1;

                     if (ColMap->M[i0][0] >= 0) { /* good color */
                        if (i1 < ColMap->N_M[0]) {
                           r = Vscl - i0; 
                           /*fprintf (SUMA_STDERR,
                     "i0 = %d, i1 = %d, Vscl = %f, r= %f Col[i0] = %f %f %f\n", 
                                       i0, i1, Vscl, r, ColMap->M[i0][0], 
                                       ColMap->M[i0][1], ColMap->M[i0][2]);*/

                           SV->cV[i3  ] = ColMap->M[i0][0] + r * 
                                          (ColMap->M[i1][0] - ColMap->M[i0][0]);
                           SV->cV[i3+1] = ColMap->M[i0][1] + r * 
                                          (ColMap->M[i1][1] - ColMap->M[i0][1]);
                           SV->cV[i3+2] = ColMap->M[i0][2] + r * 
                                          (ColMap->M[i1][2] - ColMap->M[i0][2]);
                        } else {
                           SV->cV[i3  ] = ColMap->M[i0][0];
                           SV->cV[i3+1] = ColMap->M[i0][1];
                           SV->cV[i3+2] = ColMap->M[i0][2];
                        }
                     } else { /* mask color */
                        SV->isMasked[i] = YUP;
                        SV->cV[i3  ] = Opt->MaskColor[0]; 
                        SV->cV[i3+1] = Opt->MaskColor[1]; 
                        SV->cV[i3+2] = Opt->MaskColor[2];
                     }
                  } else {
                     SV->cV[i3  ] = Opt->MaskColor[0]; 
                     SV->cV[i3+1] = Opt->MaskColor[1]; 
                     SV->cV[i3+2] = Opt->MaskColor[2]; 
                  }
               }
            }
      } else { /* all values are equal, use the middle color in the colormap */
         if (LocalHead) 
            fprintf (SUMA_STDOUT,
                     "Warning %s: Node value range is 0,"
                     " using middle color in colormap.\n", FuncName);
         i0 = (ColMap->N_M[0] - 1)/2;
         for (i=0; i < N_V; ++i) {
            i3 = 3*i;
            if (!SV->isMasked[i]) {
               if (ColMap->M[i0][0] >= 0) {
                  SV->cV[i3  ] = ColMap->M[i0][0];
                  SV->cV[i3+1] = ColMap->M[i0][1];
                  SV->cV[i3+2] = ColMap->M[i0][2];
               } else {
                  SV->isMasked[i] = YUP;
                  SV->cV[i3  ] = Opt->MaskColor[0]; 
                  SV->cV[i3+1] = Opt->MaskColor[1]; 
                  SV->cV[i3+2] = Opt->MaskColor[2]; 
               }
            } else {
               SV->cV[i3  ] = Opt->MaskColor[0]; 
               SV->cV[i3+1] = Opt->MaskColor[1]; 
               SV->cV[i3+2] = Opt->MaskColor[2]; 
            }
         }
      }
   } else {
      /* direct color mapping */
      SUMA_LH("Direct colormapping");
      if (Opt->interpmode != SUMA_DIRECT) {
         fprintf (SUMA_STDOUT,
                  "Error %s: Logic error, should never get here with"
                  " Opt->interpmode != SUMA_DIRECT\n", FuncName);
         SUMA_RETURN(NOPE);
      }
      for (i=0; i < N_V; ++i) {
         i3 = 3*i;
         if (!SV->isMasked[i]) {
            i0 = (int)V[i]; 
            if (i0 < 0) i0 = 0;
            else if (i0 >= ColMap->N_M[0]) i0 = ColMap->N_M[0] -1;
            if (ColMap->M[i0][0] >= 0) {
               SV->cV[i3  ] = ColMap->M[i0][0];
               SV->cV[i3+1] = ColMap->M[i0][1];
               SV->cV[i3+2] = ColMap->M[i0][2];
            } else {
               SV->isMasked[i] = YUP;
               SV->cV[i3  ] = Opt->MaskColor[0]; 
               SV->cV[i3+1] = Opt->MaskColor[1]; 
               SV->cV[i3+2] = Opt->MaskColor[2];
            }
         } else {
            SV->cV[i3  ] = Opt->MaskColor[0]; 
            SV->cV[i3+1] = Opt->MaskColor[1]; 
            SV->cV[i3+2] = Opt->MaskColor[2]; 
         }

      }
   }
   if (Mbuf) {
      /* free what is in ColMap->M */
      SUMA_free2D((char **)ColMap->M, ColMap->N_M[0]);
      ColMap->M = Mbuf; Mbuf = NULL;
   }
   if (NewMap) {
      SUMA_LH("Freeing linearized colormap.");
      SUMA_Free_ColorMap (ColMap); 
   }
   
   SUMA_RETURN (YUP);
   
}

/* Change a colormap to NIML format */
NI_group *SUMA_CmapToNICmap(SUMA_COLOR_MAP *CM)
{
   static char FuncName[]={"SUMA_CmapToNICmap"};
   NI_group *ngr=NULL;
   NI_element *nel = NULL;
   SUMA_DSET *dset=NULL;
   float *fbuf=NULL;
   int *ibuf = NULL;
   int i;
   SUMA_PARSED_NAME *sname = NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;
   
   if (!CM) SUMA_RETURN(ngr);
   
   /* bufer space */
   fbuf = (float *)SUMA_calloc(CM->N_M[0],sizeof(float));
   ibuf = (int *)SUMA_calloc(CM->N_M[0],sizeof(int));
   
   /* create group and name it */
   sname = SUMA_ParseFname(CM->Name, NULL);
   dset = SUMA_CreateDsetPointer(sname->FileName, SUMA_LABEL_TABLE_OBJECT, 
                                 NULL, NULL, CM->N_M[0]);
   
   /* Go for it */
   for (i=0; i<CM->N_M[0]; ++i) fbuf[i] = CM->M[i][0];
   if (!SUMA_AddDsetNelCol(dset, "R", SUMA_NODE_R, (void *)fbuf, NULL, 1)) {
      SUMA_S_Err("Failed to add R");
      SUMA_FreeDset(dset); dset = NULL;
      goto CLEANUP;
   }
   
   for (i=0; i<CM->N_M[0]; ++i) fbuf[i] = CM->M[i][1];
   if (!SUMA_AddDsetNelCol(dset, "G", SUMA_NODE_G, (void *)fbuf, NULL, 1)) {
      SUMA_S_Err("Failed to add G");
      SUMA_FreeDset(dset); dset = NULL;
      goto CLEANUP;
   }
   
   for (i=0; i<CM->N_M[0]; ++i) fbuf[i] = CM->M[i][2];
   if (!SUMA_AddDsetNelCol(dset, "B", SUMA_NODE_B, (void *)fbuf, NULL, 1)) {
      SUMA_S_Err("Failed to add B");
      SUMA_FreeDset(dset); dset = NULL;
      goto CLEANUP;
   }

   if (CM->N_M[1] == 4) {
      for (i=0; i<CM->N_M[0]; ++i) fbuf[i] = CM->M[i][3];
      if (!SUMA_AddDsetNelCol(dset, "A", SUMA_NODE_A, (void *)fbuf, NULL, 1)) {
         SUMA_S_Err("Failed to add A");
         SUMA_FreeDset(dset); dset = NULL;
         goto CLEANUP;
      }
   }
   
   if (CM->idvec) {
      if (!SUMA_AddDsetNelCol(dset, "key", SUMA_NODE_ILABEL, 
                              (void *)CM->idvec, NULL, 1)) {
         SUMA_S_Err("Failed to add idvec");
         SUMA_FreeDset(dset); dset = NULL;
         goto CLEANUP;
      }
   }
   
   if (CM->cname) {
      if (!SUMA_AddDsetNelCol(dset, "name", SUMA_NODE_SLABEL, 
                              (void *)CM->cname, NULL, 1)) {
         SUMA_S_Err("Failed to add cname");
         SUMA_FreeDset(dset); dset = NULL;
         goto CLEANUP;
      }
   }
   
   if (CM->frac) {
      if (!SUMA_AddDsetNelCol(dset, "fraction", SUMA_NODE_FLOAT, 
                              (void *)CM->frac, NULL, 1)) {
         SUMA_S_Err("Failed to add frac");
         SUMA_FreeDset(dset); dset = NULL;
         goto CLEANUP;
      }
   }   
   
   /* the little people */
   NI_SET_INT(dset->ngr, "flipped", (int)CM->flipped);
   NI_SET_INT(dset->ngr, "Sgn", CM->Sgn);
   NI_SET_FLOAT(dset->ngr, "top_frac", CM->top_frac);
   NI_SET_FLOATv(dset->ngr, "M0", CM->M0, CM->N_M[1]);
   NI_set_attribute(dset->ngr, "Name", sname->FileName_NoExt);
   
   /* So this is not really a dset, but it was nice to make use of
   dset utility functions. Now cleanup a little */
   /* remove ugly inel */
   NI_remove_from_group(dset->ngr, dset->inel);
   
   /* grab ngr from dset, it is all we need */
   ngr = dset->ngr; dset->ngr = NULL; 
   /* change name from AFNI_dataset to AFNI_labeltable */
   NI_rename_group(ngr, "AFNI_labeltable");
   /* get rid of dset */
   dset->dnel = NULL; SUMA_FreeDset(dset); dset=NULL;
   
   
   CLEANUP:
   if (fbuf) SUMA_free(fbuf);
   if (ibuf) SUMA_free(ibuf);
   if (sname) sname = SUMA_Free_Parsed_Name(sname);
   
   SUMA_RETURN(ngr);
}   

/*!   \brief Create a colormap for a label dset if one does not
             exist already
   dset is a legitimate Label Dset (is_Label_dset = 1)
   ThisCmap: If Not NULL, then use this colormap for sure
             If NULL, then pick a default one. 
   
   The function returns the colormap in NI format. 
   But it is already stuck in dset.
*/
NI_group * SUMA_CreateCmapForLabelDset(SUMA_DSET *dset, 
                                       SUMA_COLOR_MAP *ThisCmap) 
{
   static char FuncName[]={"SUMA_CreateCmapForLabelDset"};
   int *idvec_hold=NULL;
   char **cname_hold=NULL, stmp[256];
   int cnt=0, i, N_unq, *unq=NULL;
   NI_group *ngr = NULL, *NIcmap=NULL;
   SUMA_COLOR_MAP *cmap=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if ((ngr = SUMA_NI_Cmap_of_Dset(dset))) { 
      SUMA_RETURN(ngr); 
   }
   
   if (!ThisCmap) {
      SUMA_LH("Producing uniques");
      unq = SUMA_UniqueValuesInLabelDset(dset, &N_unq);
      if (!(cmap = SUMA_FindNamedColMap("ROI_i256"))) {
         SUMA_S_Errv("Found no colmap %s in %p\n", "ROI_i256", SUMAg_CF->scm);
         SUMA_Show_ColorMapVec (SUMAg_CF->scm->CMv, SUMAg_CF->scm->N_maps, 
                                NULL, 1);
         SUMA_RETURN(NULL);
      }
      if (!unq || N_unq > cmap->N_M[0]) {
         SUMA_S_Errv("Either no unique values or too many of them:\n"
                     "  %p, %d (%d)\n", unq, N_unq, cmap->N_M[0]); 
         SUMA_RETURN(NULL);
      }
   
      /* add a new key, and names. But first preserve the past. */
      SUMA_LH("Doing the key stuff");
      idvec_hold = cmap->idvec;
      cname_hold = cmap->cname;
      /* Now get a new key */
      cmap->idvec = (int *)SUMA_calloc(cmap->N_M[0], sizeof(int));
      cmap->cname = (char **)SUMA_calloc(cmap->N_M[0], sizeof(char *));
      cnt = 0;
      for (i=0; i<N_unq; ++i) {
         if (unq[i]) { 
            cmap->idvec[cnt] = unq[i]; 
            sprintf(stmp, "ROI_%04d", cmap->idvec[cnt]);
            cmap->cname[cnt] = SUMA_copy_string(stmp);
            ++ cnt; 
         }
      }
      /* fill the rest */
      SUMA_LH("padding");
      for (i=cnt; i<cmap->N_M[0]; ++i) {
         cmap->idvec[i] = cmap->idvec[i-1]+1; 
         sprintf(stmp, "ROI_%04d", cmap->idvec[i]);
         cmap->cname[i] = SUMA_copy_string(stmp);
      }
      
      /* change to NI form */
      NIcmap =  SUMA_CmapToNICmap (cmap);
      
      /* and put things back in place in cmap */
      SUMA_LH("tidy up");
      SUMA_free(cmap->idvec); cmap->idvec = idvec_hold; idvec_hold=NULL;
      for (i=0; i<cmap->N_M[0]; ++i) {
         if (cmap->cname[i]) SUMA_free(cmap->cname[i]); 
      }
      SUMA_free(cmap->cname); cmap->cname = cname_hold; cname_hold=NULL;
      
   } else {
      cmap = ThisCmap;
      SUMA_LH("Got cmap, checking it out");
      /* check */
      if (!SUMA_IsCmapOKForLabelDset(dset, cmap)) {
         SUMA_S_Err("Provided cmap is no good ");
         SUMA_RETURN(NULL);
      } 
      
      
      NIcmap =  SUMA_CmapToNICmap (cmap);
   }
      
   if (!NIcmap) {
      SUMA_S_Err("Have no cmap!");
      SUMA_RETURN(NULL);
   }
      
   /* Now stick in the colormap */
   NI_add_to_group(dset->ngr, (void*)NIcmap);
   
   SUMA_RETURN(NIcmap);
}

SUMA_Boolean SUMA_IsCmapOKForLabelDset(SUMA_DSET *dset, SUMA_COLOR_MAP *cmap) 
{
   static char FuncName[]={"SUMA_IsCmapOKForLabelDset"};
   int i, *unq=NULL, N_unq=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!cmap) {
      SUMA_LH("NULL cmap");
      SUMA_RETURN(NOPE);   
   }
   
   if (!cmap->idvec || !cmap->cname) {
      SUMA_LH("Submitted cmap has no cnames or idvec ");
      SUMA_RETURN(NOPE);
   }
   
   if (!SUMA_is_Label_dset(dset, NULL)) {
      SUMA_LH("Not a label dset");
      SUMA_RETURN(NOPE);
   }

   /* do not free unq */
   unq = SUMA_UniqueValuesInLabelDset(dset, &N_unq);
   
   if (!unq) {
      SUMA_S_Err("Cannot get unique set ");
      SUMA_RETURN(NOPE);
   }
   
   if (N_unq > cmap->N_M[0]) {
      SUMA_S_Errv( "Have %d unique values, \n"
                  "have no colormap big enough for this\n", N_unq);
      SUMA_RETURN(NOPE);
   }
   if (!cmap->idvec || !cmap->cname) {
      SUMA_S_Err("Submitted cmap has no cnames or idvec ");
      SUMA_RETURN(NOPE);
   }
   /* check if all unique entries are covered .... */
   if (N_unq > cmap->N_M[0]) {
      SUMA_S_Errv( "Have %d unique values, \n"
                  "Submitted colormap is bad\n", N_unq);
      SUMA_RETURN(NOPE);
   }
   
   for (i=0; i<N_unq; ++i) {
      if (SUMA_ColMapKeyIndex(unq[i], cmap) < 0) {
         SUMA_S_Errv("Key %d has no entry in cmap %s\n", unq[i], cmap->Name);
         SUMA_RETURN(NOPE);
      }
   }
   
   SUMA_RETURN(YUP);
}

/* Change a dataset with one integer valued column to a 
  Label type dset. Note that dset itself is modified.
*/
int SUMA_dset_to_Label_dset_cmap(SUMA_DSET *dset, SUMA_COLOR_MAP *cmap) 
{
   static char FuncName[]={"SUMA_dset_to_Label_dset_cmap"};
   int ctp, vtp, i, *unq=NULL, N_unq=0;
   NI_group *NIcmap=NULL, *ngr=NULL;
   char stmp[256], *lbli=NULL, *attname=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SUMA_dset_to_Label_dset(dset)) { SUMA_RETURN(0); }
         
   /* Prep the colormap. */
   {
      if (!(ngr = SUMA_CreateCmapForLabelDset(dset, cmap))) {
         SUMA_S_Err("Failed to add colormap");
         SUMA_RETURN(0);
      }
   }
   
   SUMA_RETURN(1);
}

SUMA_COLOR_MAP *SUMA_NICmapToCmap(NI_group *ngr)
{
   static char FuncName[]={"SUMA_NICmapToCmap"};
   int i, *id=NULL;
   float *r=NULL, *g=NULL, *b=NULL, *a=NULL;
   char **s=NULL;
   SUMA_COLOR_MAP *CM=NULL;
   SUMA_DSET dset;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!ngr) SUMA_RETURN(CM);
   
   /* shoehorn into dset struct */
   dset.ngr = ngr;
   dset.inel = dset.dnel = NULL;
   dset.dnel = SUMA_FindDsetDataElement(&dset);
   
   if (SDSET_TYPE((&dset)) != SUMA_LABEL_TABLE_OBJECT) {
      SUMA_S_Errv("Not a colormap object (%d,%d)\n", 
                  SDSET_TYPE((&dset)), SUMA_LABEL_TABLE_OBJECT);
      SUMA_RETURN(CM);
   }
   SUMA_LH("Alloc");
   CM = (SUMA_COLOR_MAP *)SUMA_calloc(1,sizeof(SUMA_COLOR_MAP));
   CM->N_M[0] = dset.dnel->vec_len;
   CM->N_M[1] = 0; /* don't know yet */
   SUMA_LH("locate");
   for (i=0; i<dset.dnel->vec_num; ++i) {
      switch (SUMA_TypeOfDsetColNumb(&dset, i)) {
         case SUMA_NODE_R:
            r = (float*)dset.dnel->vec[i];
            break;
         case SUMA_NODE_G:
            g = (float*)dset.dnel->vec[i];
            break;
         case SUMA_NODE_B:
            b = (float*)dset.dnel->vec[i];
            break;
         case SUMA_NODE_A:
            a = (float*)dset.dnel->vec[i];
            break;
         case SUMA_NODE_ILABEL:
            id = (int *)dset.dnel->vec[i];
            break;
         case SUMA_NODE_SLABEL:
            s = (char **)dset.dnel->vec[i];
            break;
         default:
            SUMA_S_Errv("Bad column (#%d) type %d for colormap\n", 
                        i, SUMA_TypeOfDsetColNumb(&dset, i));
            SUMA_Free_ColorMap(CM); CM = NULL;
            SUMA_RETURN(CM);
      }
   }
   if (a) CM->N_M[1] = 4;  /*rgba*/
   else CM->N_M[1] = 3;    /*rgb*/
   if (!r || !g || !b) {
      SUMA_S_Err("Missing columns");
      SUMA_Free_ColorMap(CM); CM = NULL;
      SUMA_RETURN(CM);
   }
   
   SUMA_LH("Fill");
   CM->M = (float **)SUMA_allocate2D(CM->N_M[0], CM->N_M[1], sizeof(float));
   for (i=0; i<CM->N_M[0]; ++i) {
      CM->M[i][0] = r[i];
      CM->M[i][1] = g[i];
      CM->M[i][2] = b[i];
   }
   if (a) {
      for (i=0; i<CM->N_M[0]; ++i) CM->M[i][3] = a[i];
   }
   if (s) {
      CM->cname = (char **)SUMA_calloc(CM->N_M[0], sizeof(char *));
      for (i=0; i<CM->N_M[0]; ++i) CM->cname[i] = SUMA_copy_string(s[i]);
   }
   if (id) {
      CM->idvec = (int *)SUMA_calloc(CM->N_M[0], sizeof(int));
      for (i=0; i<CM->N_M[0]; ++i) CM->idvec[i] = id[i];
   }
   
   NI_GET_INT(ngr, "flipped", i); CM->flipped = (byte)i;
   NI_GET_INT(ngr, "Sgn", CM->Sgn);
   NI_GET_FLOAT(ngr, "top_frac", CM->top_frac);
   NI_GET_FLOATv(ngr, "M0", CM->M0, CM->N_M[1], 1);
   
   CM->Name = SUMA_copy_string(NI_get_attribute(ngr,"Name"));
   
   SUMA_RETURN(CM);
}
/*!
   \brief Return the index into a colormap's color array
   given a color's key (or id).
   This function will be called a trillion times, do not
   put fancy stuff in it, or just use macro
   \sa faster macro SUMA_COLMAPKEYTOINDEX
   */
int SUMA_ColMapKeyIndex(int key, SUMA_COLOR_MAP *CM)
{
   static char FuncName[]={"SUMA_ColMapKeyIndex"};
   SUMA_COLOR_MAP_HASH_DATUM *hd=NULL;
   SUMA_ENTRY;
   
   if (!CM || !CM->chd) SUMA_RETURN(-1);
   
   SUMA_COLMAPKEYTOINDEX(key, CM->chd, hd);
   
   SUMA_RETURN(key);
}
/* destroy the hash table of the colormap */
SUMA_Boolean SUMA_DestroyCmapHash(SUMA_COLOR_MAP *SM)
{
   static char FuncName[]={"SUMA_DestroyCmapHash"};
   SUMA_COLOR_MAP_HASH_DATUM *hd=NULL;
   
   SUMA_ENTRY;
   
   if (!SM || !SM->chd) SUMA_RETURN(YUP);
   
   /* destroy all of the hash table */
   while (SM->chd) {
      hd = SM->chd;  /* will delete the head of the hash table list */
      HASH_DEL( SM->chd, hd); /* remove the head from the list, after
                                 this macro, SM->chd points to the next
                                 item in the list; the new head */ 
      SUMA_free(hd); hd = NULL; /* free hd, no longer needed */
   }
      
   SUMA_RETURN(YUP);
}
/* create a new or recreate a colormap's hashtable */  
SUMA_Boolean SUMA_CreateCmapHash(SUMA_COLOR_MAP *SM)
{
   static char FuncName[]={"SUMA_CreateCmapHash"};
   SUMA_COLOR_MAP_HASH_DATUM *hd=NULL;
   int ism = 0;
   
   SUMA_ENTRY;
   
   if (!SM || !SM->idvec) {
      SUMA_S_Err("Null colormap or no id vector");
      SUMA_DUMP_TRACE(FuncName);
      SUMA_RETURN(NOPE);
   }
   
   /* destroy old hash */
   SUMA_DestroyCmapHash(SM);
   
   /* create new hash table */
   for (ism=0; ism < SM->N_M[0]; ++ism) {
         hd = (SUMA_COLOR_MAP_HASH_DATUM *)
                  SUMA_calloc(1, sizeof(SUMA_COLOR_MAP_HASH_DATUM));
         hd->id = SM->idvec[ism];
         hd->colmapindex = ism;
         HASH_ADD_INT(SM->chd, id, hd); 
   }
   
   SUMA_RETURN(YUP);
}  

SUMA_Boolean SUMA_ScaleToMap_alaHASH ( float *V, int N_V, 
                                       SUMA_COLOR_MAP *ColMap, 
                                       SUMA_SCALE_TO_MAP_OPT *Opt, 
                                       SUMA_COLOR_SCALED_VECT * SV)
{
   static char FuncName[]={"SUMA_ScaleToMap_alaHASH"};
   int i,j, i0, i1, i3, mxColindex;
   float Vmin, Vmax, MinCol, MaxCol, Vrange, Vscl, r, **Mbuf= NULL;
   SUMA_Boolean NewMap = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   
   SUMA_RETURN (SUMA_ScaleToMap(V, N_V, -1.0, -1.0, ColMap, Opt, SV));
   
}
/* Does a colormap need linearization?*/
SUMA_Boolean SUMA_NeedsLinearizing(SUMA_COLOR_MAP *ColMap)
{
   static char FuncName[]={"SUMA_NeedsLinearizing"};
   float dfr = 0.0;
   int i = 0;
   
   SUMA_ENTRY;
   
   /*    SUMA_Show_ColorMapVec(&ColMap, 1, NULL, 2); */
   
   if (!ColMap->frac) SUMA_RETURN(NOPE);
   
   if (ColMap->N_M[0]<2) SUMA_RETURN(NOPE);
   
   dfr = ColMap->frac[0]-ColMap->frac[1];
   for (i=2; i<ColMap->N_M[0]; ++i) {
      if (SUMA_ABS((ColMap->frac[i-1]-ColMap->frac[i]) - dfr) > 0.0001) {
         SUMA_RETURN(YUP); 
      }
   }
   
   SUMA_RETURN(NOPE);
}

/*!
   This function maps the values in a vector to colors on a color map
   Res = SUMA_ScaleToMap (V, N_V, Vmin, Vmax, ColMap, Opt, SV); 
   \param V (float *) N_V x1  vector containing the values to be colorized by the map in ColMap. 
   \param N_V (int) number of elements in V
   \param Vmin (float) minimum value in V
   \param Vmax (float) maximum value in V 
          If both Vmin and Vmax are set to -1, and interpmode = SUMA_DIRECT then
          HashMode is turned on (see below)
   \param ColMap (SUMA_COLOR_MAP *) pointer to the color map
   \param Opt (SUMA_SCALE_TO_MAP_OPT *) is a structure containing the options for SUMA_ScaleToMap
      see help on SUMA_SCALE_TO_MAP_OPT for info on various options 
   \param SV (SUMA_COLOR_SCALED_VECT *) is a pre-allocated structure that will contain the 
          color mapped version of V. See the definition of SUMA_COLOR_SCALED_VECT for more info.
   \ret Res (SUMA_Boolean) good/bad
          
   \sa SUMA_Create_ColorScaledVect and SUMA_Free_ColorScaledVect
   \sa SUMA_ScaleToMapOptInit
   \sa SUMA_MakeColorMap
   
   NOTES: 
      +The brightness factor Opt->BrightFact is applied to the colormap in ColMap and the MaskColor in Opt
      +How Clipping is done: 
         if (V[i] < Clip[0]) V[i] = Clip[0];
         if (V[i] > Clip[1]) V[i] = Clip[1];
      +Values in Mask Range are applied BEFORE the clipping is done 
         IF (Mask[0] <= V[i] <= Mask[1]) V[i] is masked 
*/
SUMA_Boolean SUMA_ScaleToMap (float *V, int N_V, 
                              float Vmin, float Vmax, 
                              SUMA_COLOR_MAP *ColMap, 
                              SUMA_SCALE_TO_MAP_OPT *Opt, 
                              SUMA_COLOR_SCALED_VECT * SV)
{
   static char FuncName[]={"SUMA_ScaleToMap"};
   int i,j, i0, i1, mxColindex, i3=0, HashMode = 0, FillVCont = 0;
   float MinCol, MaxCol, Vrange, Vscl, r, **Mbuf= NULL, top_frac;
   static int nwarn = 0, nwarnvcont = 0;
   SUMA_COLOR_MAP_HASH_DATUM *hdbuf=NULL;
   SUMA_Boolean NewMap = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!ColMap) {
      SUMA_SL_Err("NULL ColMap");
      SUMA_RETURN(NOPE);
   }
   if (!Opt) {
      SUMA_SL_Err("NULL Opt");
      SUMA_RETURN(NOPE);
   }
   if (!SV) {
      SUMA_SL_Err("NULL SV");
      SUMA_RETURN(NOPE);
   }
   if (!V) {
      SUMA_SL_Err("NULL V");
      SUMA_RETURN(NOPE);
   }
   
   /* No negative colormaps here */
   if (ColMap->Sgn < 0) {
      /* proceed, in SUMA options were given to the user to make 
         the range symmetric about 0.
         They can shoot themselves in the foot if they want to */
      SUMA_LH( "Colormap is split into positive and negative.\n "
               "Make sure your range is from -a to + a to have "
               "the mapping resemble AFNI's");
   }
   
   /* find the values to be masked out */
   if (Opt->ApplyMask){
      SUMA_LH("Applying Mask");
      if (Opt->MaskZero) {
         /* mask zeros and values in range */
         for (i=0; i < N_V; ++i) {
            if (  !V[i] || (V[i] >= Opt->MaskRange[0] && 
                  V[i] <= Opt->MaskRange[1]))  SV->isMasked[i] = YUP;
         }
      } else {
         /* don't mask zeros, just range */
         for (i=0; i < N_V; ++i) {
            if (V[i] >= Opt->MaskRange[0] && V[i] <= Opt->MaskRange[1]) 
               SV->isMasked[i] = YUP;
         }
      }
   }else {
      if (Opt->MaskZero) {
         SUMA_LH("Masking Zeros");
        /* mask zeros */
         for (i=0; i < N_V; ++i) {
            if (!V[i]) SV->isMasked[i] = YUP;
         }
      }
   }
   
   /* refuse to deal with top_frac here, SUMA does not allow for 
      colormaps to have a top != 1 
      First pass at implementing top != 1 does exist below but is 
      now turned off. I don't feel
      like testing it... */
   top_frac = ColMap->top_frac;
   if (top_frac > 0.0f) {
      if (!(nwarn % 25)) {
         SUMA_SL_Warn(  "\n"                                                                 /* don't want to deal with this because */
                        "Colormaps for SUMA usage must have 1 for top value.\n"              /* suma colormap interface does not yet */
                        "Any scaling should be done using the range options.\n"              /* have a way to show a top value for the */
                        "The top value for this colormap will be forced to 1.0\n"            /* colormap and it is assumed that the top */
                        "This warning will be shown intermittently when using.\n"            /* is 1. Allowing maps where the top is not */
                        "SUMA's GUI.\n");                                                    /* 1 is dealt with below but is cause for */
                                                                                             /* confusion. You can remove this restriction */      
      }                                                                                      /* the colormap. */
      ++nwarn;                                                                            /* when SUMA clearly shows the top value of */
      top_frac = 0.0f;  
   }
   
   /* go through and clip values in V to those specified in the range */
   if (Opt->ApplyClip) {
      SUMA_LH( "Applying Clip \n"
               "(This one's not used in\n"
               " interactive mode anymore \n"
               " because clipping is handled \n"
               " in the colormapping part)");
      if (top_frac > 0.0f) {
         SUMA_S_Note("Using top_frac, not fully tested.");
         Opt->IntRange[0] *= top_frac;
         Opt->IntRange[1] *= top_frac;
      } 

      for (i=0; i < N_V; ++i) {
         if (!SV->isMasked[i]) { /* don't waist time on masked stuff */
            if (V[i] > Opt->IntRange[0]) { 
               /* that's cool */
            } else {
               V[i] = Opt->IntRange[0];
            }

            if (V[i] < Opt->IntRange[1]) { 
               /* that's cool */
            } else {
               V[i] = Opt->IntRange[1];
            }
         } 
      }
      Vmin = Opt->IntRange[0];
      Vmax = Opt->IntRange[1];
      
      if (top_frac > 0.0f) {
         /* SUMA_S_Note("Using top_frac, not fully tested."); */
         Opt->IntRange[0] /= top_frac;
         Opt->IntRange[1] /= top_frac;
      } 
   } else {
      if (top_frac > 0.0f) {
         SUMA_S_Note("Using top_frac, not fully tested.");
         Vmin *= top_frac;
         Vmax *= top_frac;
      }
      SUMA_LHv("No Clip, Opt->IntRange ignored.\nVmin..Vmax= %f..%f", 
               Vmin, Vmax);
   }
   
   /* Add any coord bias ? */
   if (  Opt->DoBias == SW_CoordBias_X || 
         Opt->DoBias == SW_CoordBias_Y || 
         Opt->DoBias == SW_CoordBias_Z || 
         Opt->DoBias == SW_CoordBias_N) {
      SUMA_LH("Coord Bias requested");
      if (!SV->BiasCoordVec) {
         SUMA_LH("Allocating for BiasCoordVec");
         SV->BiasCoordVec = (float *)SUMA_calloc(N_V, sizeof(float));
         if (!SV->BiasCoordVec) { 
            SUMA_SL_Crit("Failed to allocate"); SUMA_RETURN(NOPE); }
      }else {
         SUMA_SL_Err("Do not expect this not to be null here ... ");
         SUMA_RETURN(NOPE);
      }
   } else {
      SUMA_LH("No Coord Bias requested");
      if (SV->BiasCoordVec) { 
         SUMA_SL_Err("Do NOT expect this not to be null here ... ");
         SUMA_RETURN(NOPE);
      }
   }
   
   /* is the colormap non-linear ? */
   FillVCont = Opt->ColsContMode; /* the default behaviour, but that does not
                                    work if colormaps are linearized because
                                    indices into the colormap do not match 
                                    indices into non-linearized maps that are
                                    stored in memory. */
   if (SUMA_NeedsLinearizing(ColMap)) {
      if (Opt->interpmode == SUMA_NO_INTERP || Opt->interpmode == SUMA_INTERP) {
         /* linearize color map */
         SUMA_LH("Linearizing colormap ...");
         NewMap = YUP;
         if (ColMap->frac[0] > 0 && ColMap->Sgn == -1) {
            SUMA_S_Warn ("Color map fractions are positive with Sgn flag = -1");
         }
         if (ColMap->frac[0] < 0 && ColMap->Sgn == 1) {
            SUMA_S_Warn ("Color map fractions are negative with Sgn flag = 1");
         }
         ColMap = SUMA_Linearize_Color_Map (ColMap, -1);
         if (FillVCont && !nwarnvcont) {
            SUMA_SLP_Warn( "No contouring for colormaps that \n"
                           "do not have equal size panes. \n"
                           "This warning will be shown once this session.");
            ++nwarnvcont;
         }
         FillVCont = 0;
      } else {
         if (Opt->interpmode != SUMA_DIRECT) {
            SUMA_SL_Err("Not expected interpmode.");
            /* Do nothing to the colormap, direct mapping mode in gear */
            NewMap = NOPE;
         }
      }
   }else {
      SUMA_LH("NO Linearizing of colormap deemed necessary...");
      NewMap = NOPE;
   }
   
   /* if brightness factor is given, apply it to color map and mask color */
   Mbuf = NULL;
   if (Opt->BrightFact <= 0 || Opt->BrightFact > 1) {
      fprintf (SUMA_STDERR,
               "Error %s: Opt->BrightFact must be between ]0 1]\n", FuncName);
      SUMA_RETURN (NOPE);
   }else {
      if (Opt->BrightFact != 1) {
         SUMA_LH("Modulating brightness of map");
         Mbuf = ColMap->M; /* save pointer */
         ColMap->M = (float **)
               SUMA_allocate2D(ColMap->N_M[0], ColMap->N_M[1], sizeof(float));
         for (i=0; i < ColMap->N_M[0]; ++i) {
            ColMap->M[i][0] = Mbuf[i][0] * Opt->BrightFact;
            ColMap->M[i][1] = Mbuf[i][1] * Opt->BrightFact;
            ColMap->M[i][2] = Mbuf[i][2] * Opt->BrightFact;
            if (ColMap->N_M[1] == 4) {
            ColMap->M[i][3] = Mbuf[i][3] * Opt->BrightFact;
            }
         }
         /* now for the mask color */
         Opt->MaskColor[0] *= Opt->BrightFact;
         Opt->MaskColor[1] *= Opt->BrightFact;
         Opt->MaskColor[2] *= Opt->BrightFact;
      }
   }
   
   
   if (  Opt->interpmode != SUMA_DIRECT && 
         Opt->interpmode != SUMA_NO_INTERP && 
         Opt->interpmode != SUMA_INTERP) {
      fprintf (SUMA_STDERR,
               "Error %s: Opt->interpmode is incorrectly specifed (%d).\n", 
               FuncName, Opt->interpmode);
      SUMA_RETURN(NOPE);
   }
   
   if (Opt->interpmode == SUMA_NO_INTERP || Opt->interpmode == SUMA_INTERP) {
      /* Now go through values and interpolate onto index of colormap */
      MinCol = 0.0; MaxCol = (float)ColMap->N_M[0]; 
      Vrange = Vmax - Vmin; 
      if (Vrange < 0) {
         fprintf (SUMA_STDERR,
                  "Error %s: Vmax (%f)< Vmin(%f).\n", FuncName, Vmax, Vmin);
         SUMA_RETURN (NOPE);
      }

      if (Vrange > 0) {
         mxColindex = ColMap->N_M[0] -1; 
         if (Opt->interpmode == SUMA_NO_INTERP) { 
            /* no interpolation between colours */
            SUMA_LH("No Interp Mode");
            SV->N_VCont = 0;
            for (i=0; i < N_V; ++i) {
               i3 = 3*i;
               if (!SV->isMasked[i]) {
                  Vscl = (V[i] - Vmin) / Vrange * ColMap->N_M[0]; 
                        /* used mxColindex instead of N_M[0] (wrong!) 
                           prior to Oct 22, 03 */
                  if (Vscl < 0) Vscl = 0; 
                  if (Vscl > ColMap->N_M[0]) Vscl = ColMap->N_M[0]; 
                        /* This happens when your Min--Max are within the 
                           boundaries of the data's (V[i]) min to max */
                  i0 = (int)(Vscl); 
                  if (i0 > mxColindex) i0 = mxColindex; 
                        /* No need, Vscl's clipping takes care of that: 
                           if (i0 < 0) i0 = 0; */
                  if (SV->BiasCoordVec) SV->BiasCoordVec[i] = Vscl;
                  if (ColMap->M[i0][0] >= 0) { /* good color */
                     if (FillVCont) {
                        SV->VCont[SV->N_VCont] = i0;
                        ++SV->N_VCont; 
                     }
                     SV->cV[i3  ] = ColMap->M[i0][0];
                     SV->cV[i3+1] = ColMap->M[i0][1];
                     SV->cV[i3+2] = ColMap->M[i0][2];
                  } else { /* mask color */
                     SV->isMasked[i] = YUP;
                     SV->cV[i3  ] = Opt->MaskColor[0]; 
                     SV->cV[i3+1] = Opt->MaskColor[1]; 
                     SV->cV[i3+2] = Opt->MaskColor[2];
                  }
               } else {
                  SV->cV[i3  ] = Opt->MaskColor[0]; 
                  SV->cV[i3+1] = Opt->MaskColor[1]; 
                  SV->cV[i3+2] = Opt->MaskColor[2]; 
               }
            }
         } else { /* interpolation mode */
            SUMA_LH("Interp Mode");
            SV->N_VCont = 0;
            for (i=0; i < N_V; ++i) {
               i3 = 3*i;
               if (!SV->isMasked[i]) {
                  Vscl = (V[i] - Vmin) / Vrange * ColMap->N_M[0]; 
                     /* used mxColindex instead of N_M[0] (wrong!) 
                        prior to Oct 22, 03 */ 
                  if (Vscl < 0) Vscl = 0; 
                  if (Vscl > ColMap->N_M[0]) Vscl = ColMap->N_M[0]; 
                     /* This happens when your Min--Max are within the 
                        boundaries of the data's (V[i]) min to max */
                  /*now linearly interpolate between the two closest 
                     colors in the color map */
                  i0 = (int)(Vscl); 
                  if (i0 > mxColindex) i0 = mxColindex; 
                     /* No need, Vscl's clipping takes care of that: 
                        if (i0 < 0) i0 = 0; */
                  i1=i0+1;
                  if (SV->BiasCoordVec) { 
                     SV->BiasCoordVec[i] = Vscl; 
                  }
                  if (ColMap->M[i0][0] >= 0) { /* good color */
                     if (FillVCont) {
                        /* fprintf(SUMA_STDERR,"i0=%d (on %s)\t", 
                                                i0, ColMap->Name); */
                        SV->VCont[SV->N_VCont] = i0;
                        ++SV->N_VCont; 
                     }
                     if (i1 < ColMap->N_M[0]) {
                        r = Vscl - i0; 
                        SV->cV[i3  ] = ColMap->M[i0][0] + r * 
                                       (ColMap->M[i1][0] - ColMap->M[i0][0]);
                        SV->cV[i3+1] = ColMap->M[i0][1] + r * 
                                       (ColMap->M[i1][1] - ColMap->M[i0][1]);
                        SV->cV[i3+2] = ColMap->M[i0][2] + r *
                                       (ColMap->M[i1][2] - ColMap->M[i0][2]);
                     } else {
                        SV->cV[i3  ] = ColMap->M[i0][0];
                        SV->cV[i3+1] = ColMap->M[i0][1];
                        SV->cV[i3+2] = ColMap->M[i0][2];
                     }
                  } else { /* mask color */
                     SV->isMasked[i] = YUP;
                     SV->cV[i3  ] = Opt->MaskColor[0]; 
                     SV->cV[i3+1] = Opt->MaskColor[1]; 
                     SV->cV[i3+2] = Opt->MaskColor[2];
                  }
               } else {
                  SV->cV[i3  ] = Opt->MaskColor[0]; 
                  SV->cV[i3+1] = Opt->MaskColor[1]; 
                  SV->cV[i3+2] = Opt->MaskColor[2]; 
               }
            }
         }
      }else { /* all values are equal, use the middle color in the colormap */
         if (LocalHead) 
            fprintf (SUMA_STDOUT,
                     "Warning %s: Node value range is 0, using middle color "
                     "in colormap.\n", FuncName);
         i0 = (ColMap->N_M[0] - 1)/2;
         SV->N_VCont = 0;
         for (i=0; i < N_V; ++i) {
            i3 = 3 * i;
            if (!SV->isMasked[i]) {
               if (SV->BiasCoordVec) SV->BiasCoordVec[i] = i0;
               SV->cV[i3  ] = ColMap->M[i0][0];
               SV->cV[i3+1] = ColMap->M[i0][1];
               SV->cV[i3+2] = ColMap->M[i0][2];
            } else {
               SV->cV[i3  ] = Opt->MaskColor[0]; 
               SV->cV[i3+1] = Opt->MaskColor[1]; 
               SV->cV[i3+2] = Opt->MaskColor[2]; 
            }
         }
      }
   } else {
      /* direct color mapping */
      if ((int) Vmin == (int) Vmax && Vmin == -1) {
         HashMode = 1;   
      } else HashMode = 0;
      SUMA_LHv( "Direct colormapping, HashMode = %d.\n"
               "Opt->IntRange values are useless, and\n"
               "Vmin, Vmax are used as To Hash or not To Hash flags\n", 
               HashMode );
      if (Opt->interpmode != SUMA_DIRECT) {
         fprintf (SUMA_STDOUT,
                  "Error %s: Logic error, should never get here with"
                  " Opt->interpmode != SUMA_DIRECT\n", FuncName);
         SUMA_RETURN(NOPE);
      }
      if (HashMode && !ColMap->chd) {
         if (!SUMA_CreateCmapHash(ColMap)) {
            SUMA_SLP_Err("Cannot create hash. HashMode turned off.");
            HashMode = 0;
         }
      }
      SV->N_VCont = 0;
      for (i=0; i < N_V; ++i) {
         i3 = 3*i;
         if (!SV->isMasked[i]) {
            SUMA_COLMAP_INDEX_FROM_ID(V[i], ColMap, i0, HashMode); 
            if (i0 >=0 && ColMap->M[i0][0] >= 0) {
               if (FillVCont) { 
                  SV->VCont[SV->N_VCont] = i0;
                  ++SV->N_VCont; 
               }
               SV->cV[i3  ] = ColMap->M[i0][0];
               SV->cV[i3+1] = ColMap->M[i0][1];
               SV->cV[i3+2] = ColMap->M[i0][2];
               if (SV->BiasCoordVec) SV->BiasCoordVec[i] = i0;
            } else {
               SV->isMasked[i] = YUP;
               SV->cV[i3  ] = Opt->MaskColor[0]; 
               SV->cV[i3+1] = Opt->MaskColor[1]; 
               SV->cV[i3+2] = Opt->MaskColor[2];
            }
         } else {
            SV->cV[i3  ] = Opt->MaskColor[0]; 
            SV->cV[i3+1] = Opt->MaskColor[1]; 
            SV->cV[i3+2] = Opt->MaskColor[2]; 
         }

      }
   
   }
   
   /* change range for coord bias */
   Vrange = (Opt->CoordBiasRange[1] - Opt->CoordBiasRange[0]) / ColMap->N_M[0];
   if (SV->BiasCoordVec) {
      SUMA_LH("Adding the CoordBias");
      for (i=0; i < N_V; ++i) {
            if (!SV->isMasked[i]) {
               SV->BiasCoordVec[i] = Opt->CoordBiasRange[0] + 
                                     SV->BiasCoordVec[i] * Vrange;
               /* if (LocalHead) 
                     fprintf( SUMA_STDERR,
                              "%s: %f\n", FuncName, SV->BiasCoordVec[i]); */
            } else SV->BiasCoordVec[i] = 0.0;
      }
   }
   
   if (Mbuf) {
      /* free what is in ColMap->M */
      SUMA_free2D((char **)ColMap->M, ColMap->N_M[0]);
      ColMap->M = Mbuf; Mbuf = NULL;
   }
   if (NewMap) {
      SUMA_LH("Freeing linearized colormap.");
      SUMA_Free_ColorMap (ColMap); 
   }
   
   SUMA_RETURN (YUP);
} 

/*! function to create a linear colormap out of a non-linear one 

*/

/*! function to allocate and initialize a structure of the type SUMA_COLOR_SCALED_VECT
   S = SUMA_Create_ColorScaledVect();
   \param N_Node (int) number of nodes for which colors will be assigned
   \param ColsContMode (int) flag to indicate if contouring is to be done.
   \ret S (SUMA_COLOR_SCALED_VECT * ) pointer to structure that will contain the color map of N_Node nodes
*/
SUMA_COLOR_SCALED_VECT * SUMA_Create_ColorScaledVect(int N_Node, 
                                                     int ColsContMode)
{
   static char FuncName[]={"SUMA_Create_ColorScaledVect"};
   SUMA_COLOR_SCALED_VECT * S;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (LocalHead) 
      fprintf (SUMA_STDERR,
               "%s:\n Allocate for %d nodes ...\n", FuncName, N_Node);
   S = (SUMA_COLOR_SCALED_VECT *)SUMA_calloc(1,sizeof(SUMA_COLOR_SCALED_VECT));
   if (S == NULL) {
      fprintf(SUMA_STDERR, "Error %s: Failed to allocate for S.\n", FuncName);
      SUMA_RETURN (S);
   }
   
   /* S->cM = (float **) SUMA_allocate2D(N_Node, 3, sizeof(float));  */
   S->cV = (float *) SUMA_calloc(N_Node * 3, sizeof(float));
   S->isMasked = (SUMA_Boolean *)SUMA_calloc(N_Node, sizeof(SUMA_Boolean));
   S->BiasCoordVec = NULL; /* That is created in the scaleToMap functions 
                              if needed */
   S->N_VCont = 0;
   S->VCont = NULL;
   if (ColsContMode) {
      S->VCont = (int *)SUMA_calloc(N_Node, sizeof(int));
   }
   if (!S->cV || !S->isMasked) {
      fprintf( SUMA_STDERR, 
               "Error %s: Failed to allocate for S->cV or S->isMasked.\n", 
               FuncName);
      SUMA_free(S); S = NULL;
      SUMA_RETURN (S);
   }
   
   S->N_Node = N_Node;
   
   
   SUMA_RETURN(S);
}

/*! function to free structures of the type SUMA_COLOR_SCALED_VECT
    SUMA_Free_ColorScaledVect (S)
   \param S (SUMA_COLOR_SCALED_VECT * ) pointer to structure being deleted
   \ret void
   
*/
void SUMA_Free_ColorScaledVect (SUMA_COLOR_SCALED_VECT * S)
{
   static char FuncName[]={"SUMA_Free_ColorScaledVect"};
   
   SUMA_ENTRY;

   if (S->cV) SUMA_free(S->cV);
   if (S->isMasked) SUMA_free(S->isMasked);
   if (S->BiasCoordVec) SUMA_free(S->BiasCoordVec);
   if (S->VCont) SUMA_free(S->VCont);
   if (S) SUMA_free(S);
   SUMA_RETURNe;
}

/*! 
   This function allocates for and initializes the Options structure for the function SUMA_ScaleToMap
   
   Opt = SUMA_ScaleToMapOptInit();
   
   \ret Opt (SUMA_SCALE_TO_MAP_OPT *) options structure with its fields initialized to the following:
      ApplyMask = NOPE;
      MaskRange[0] = MaskRange[1] = 0.0;
      MaskColor[0] = MaskColor[1] = MaskColor[2] = 0.0; 
      Range[0] = Range[1] = 0.0;
      BrightFact = 1;
      interpmode = SUMA_INTERP;
      NULL is returned in the case of failure
   
   You can free Opt with the free function
*/
SUMA_SCALE_TO_MAP_OPT * SUMA_ScaleToMapOptInit(void)
{
   SUMA_SCALE_TO_MAP_OPT * Opt;
   static char FuncName[]={"SUMA_ScaleToMapOptInit"};
   
   SUMA_ENTRY;

   Opt = (SUMA_SCALE_TO_MAP_OPT *)SUMA_malloc(sizeof(SUMA_SCALE_TO_MAP_OPT));
   memset (Opt, 0, sizeof(SUMA_SCALE_TO_MAP_OPT));
   if (Opt == NULL) {
      fprintf (SUMA_STDERR, "Error %s: Could not allocate for Opt.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   Opt->ApplyMask = NOPE;
   Opt->MaskRange[0] = Opt->MaskRange[1] = 0.0;
   Opt->MaskColor[0] = Opt->MaskColor[1] = Opt->MaskColor[2] = 0.0; 
   Opt->ApplyClip = NOPE;
   Opt->IntRange[0] = Opt->IntRange[1] = 0.0;
   Opt->ThreshRange[0] = Opt->ThreshRange[1] = 0.0;
   Opt->ThreshStats[0] = Opt->ThreshStats[1] = -1.0;
   Opt->BrightRange[0] = 0.0; Opt->BrightRange[1] = 0.0; 
   Opt->BrightMap[0] = 0.3; Opt->BrightMap[1] = 0.8; 
   Opt->BrightFact = 1.0;
   Opt->interpmode = SUMA_INTERP;
   Opt->alaAFNI = NOPE;
   Opt->AutoIntRange = YUP;
   Opt->AutoBrtRange = YUP;
   Opt->ColsContMode = 0;
   {
      char *eee = getenv("SUMA_MaskZero");
      if (eee) {
         if (strcmp(eee,"NO") == 0) Opt->MaskZero = NOPE;
         else if (strcmp(eee,"YES") == 0) Opt->MaskZero = YUP;
         else {
            fprintf (SUMA_STDERR,   
                     "Warning %s:\n"
                     "Bad value for environment variable SUMA_MaskZero\n"
                     "Assuming default of YES", FuncName);
            Opt->MaskZero = YUP;
         }
      } else Opt->MaskZero = YUP;
   }
   
   Opt->find = 0;
   Opt->tind = 0;
   Opt->bind = 0;
   Opt->UseThr = NOPE;
   {
      char *eee = getenv("SUMA_AbsThreshold");
      if (eee) {
         if (strcmp(eee,"NO") == 0) Opt->ThrMode = SUMA_LESS_THAN;
         else if (strcmp(eee,"YES") == 0) Opt->ThrMode = SUMA_ABS_LESS_THAN;
         else {
            fprintf (SUMA_STDERR,   
                     "Warning %s:\n"
                     "Bad value for environment variable SUMA_AbsThresh_tbold\n"
                     "Assuming default of YES", FuncName);
            Opt->ThrMode = SUMA_ABS_LESS_THAN;
         }
      } else Opt->ThrMode = SUMA_ABS_LESS_THAN;
   }
   
   Opt->UseBrt = NOPE;
   Opt->DoBias = SW_CoordBias_None;
   Opt->BiasVect = NULL;
   Opt->CoordBiasRange[0] = 0.0; Opt->CoordBiasRange[1] = 10.0; 
   SUMA_RETURN (Opt);

}

/*!
   \brief the interpmode value to a string.
*/ 
char *SUMA_CmapModeName (SUMA_COLORMAP_INTERP_MODE mapmode)
{
   static char FuncName[]={"SUMA_CmapModeName"};
   
   SUMA_ENTRY;
   
   switch (mapmode) {
         case SUMA_UNDEFINED_MODE:
            SUMA_RETURN("Undefined");
            break;
         case SUMA_DIRECT:
            SUMA_RETURN("Direct");
            break;
         case SUMA_NO_INTERP:
            SUMA_RETURN("NearestNeighbor");
            break;
         case SUMA_INTERP:
            SUMA_RETURN("Interpolate");
            break;
         default:
            SUMA_RETURN("Unexpected business");
            break;
   }
}


/*!
   \brief Returns the ascii name of a Suma standard map.
   
   \param mapcode (int)
   \param N_col (int *) to contain the number of colors in the map
         -1 if no map was found
   \return ans (char *) ascii version of mapcode
   
   \sa SUMA_StandardMapCode, StandardMapIndex
*/ 
#if 0
char *SUMA_StandardMapName (SUMA_STANDARD_CMAP mapcode, int *N_col)
{
   static char FuncName[]={"SUMA_StandardMapName"};
   
   SUMA_ENTRY;
   
   *N_col = -1;
   switch (mapcode) {
      case SUMA_CMAP_ERROR:
         SUMA_RETURN("Error");
         break;
      case SUMA_CMAP_UNDEFINED:
         SUMA_RETURN("Undefined");
         break;
      case SUMA_CMAP_RGYBR20:
         *N_col = 20;
         SUMA_RETURN("rgybr20");
         break;
      case SUMA_CMAP_nGRAY20:
         *N_col = 20;
         SUMA_RETURN("ngray20");
         break;
      case SUMA_CMAP_GRAY02:
         *N_col = 02;
         SUMA_RETURN("gray02");
         break;
      case SUMA_CMAP_flpGRAY02:
         *N_col = 02;
         SUMA_RETURN("gray_i02");
         break;
      case SUMA_CMAP_GRAY20:
         *N_col = 20;
         SUMA_RETURN("gray20");
         break;
      case SUMA_CMAP_BW20:
         *N_col = 20;
         SUMA_RETURN("bw20");
         break;
      case SUMA_CMAP_BGYR19:
         *N_col = 19;
         SUMA_RETURN("bgyr19");
         break;
      case SUMA_CMAP_MATLAB_DEF_BYR64:
         *N_col = 64;
         SUMA_RETURN("byr64");
         break;
      case SUMA_CMAP_BGYR64:
         *N_col = 64;
         SUMA_RETURN("bgyr64");
         break;
      case SUMA_CMAP_ROI256:
         *N_col = 256;
         SUMA_RETURN("roi256");
         break;
      case SUMA_CMAP_ROI128:
         *N_col = 128;
         SUMA_RETURN("roi128");
         break;
      case SUMA_CMAP_ROI64:
         *N_col = 64;
         SUMA_RETURN("roi64");
         break;
      default:
         SUMA_RETURN("Cowabonga-x321");
         break;
   }
}
#else
char *SUMA_StandardMapName (int mapindex, int *N_col)
{
   static char FuncName[]={"SUMA_StandardMapName"};
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->scm) SUMA_RETURN(NULL);  
   if (mapindex < 0 || mapindex >SUMAg_CF->scm->N_maps-1) SUMA_RETURN(NULL);
   *N_col = SUMAg_CF->scm->CMv[mapindex]->N_M[0];
   SUMA_RETURN(SUMAg_CF->scm->CMv[mapindex]->Name);
}
#endif
/*!
   \brief Returns the code corresponding to a colormap name
   
   \sa SUMA_StandardMapName
*/
#if 0
SUMA_STANDARD_CMAP SUMA_StandardMapCode (char *Name)
{
   static char FuncName[]={"SUMA_StandardMapCode"};
   
   SUMA_ENTRY;
   
   if (!Name) SUMA_RETURN(SUMA_CMAP_ERROR);
   SUMA_TO_LOWER(Name);
   if (!strcmp(Name, "undefined")) SUMA_RETURN(SUMA_CMAP_UNDEFINED);
   if (!strcmp(Name, "rgybr20")) SUMA_RETURN(SUMA_CMAP_RGYBR20);
   if (!strcmp(Name, "ngray20")) SUMA_RETURN(SUMA_CMAP_nGRAY20);
   if (!strcmp(Name, "gray20")) SUMA_RETURN(SUMA_CMAP_GRAY20);
   if (!strcmp(Name, "gray02")) SUMA_RETURN(SUMA_CMAP_GRAY02);
   if (!strcmp(Name, "gray_i02")) SUMA_RETURN(SUMA_CMAP_flpGRAY02);
   if (!strcmp(Name, "bw20")) SUMA_RETURN(SUMA_CMAP_BW20);
   if (!strcmp(Name, "bgyr19")) SUMA_RETURN(SUMA_CMAP_BGYR19);
   if (!strcmp(Name, "matlab_default_byr64")) 
               SUMA_RETURN(SUMA_CMAP_MATLAB_DEF_BYR64);
   if (!strcmp(Name, "byr64")) SUMA_RETURN(SUMA_CMAP_MATLAB_DEF_BYR64);
   if (!strcmp(Name, "bgyr64")) SUMA_RETURN(SUMA_CMAP_BGYR64);
   if (!strcmp(Name, "ygbrp64")) SUMA_RETURN(SUMA_CMAP_ROI64);
   if (!strcmp(Name, "roi64")) SUMA_RETURN(SUMA_CMAP_ROI64);
   if (!strcmp(Name, "ygbrp128")) SUMA_RETURN(SUMA_CMAP_ROI128);
   if (!strcmp(Name, "ygbrp256")) SUMA_RETURN(SUMA_CMAP_ROI256);
   if (!strcmp(Name, "roi128")) SUMA_RETURN(SUMA_CMAP_ROI128);
   if (!strcmp(Name, "roi256")) SUMA_RETURN(SUMA_CMAP_ROI256);
   /* if (!strcmp(Name, "")) SUMA_RETURN(); */
   SUMA_RETURN(SUMA_CMAP_ERROR);
}
#else
int SUMA_StandardMapIndex (char *Name)
{
   static char FuncName[]={"SUMA_StandardMapIndex"};
   
   SUMA_ENTRY;
   
   if (!Name) SUMA_RETURN(-1);
   
   /* Kill these three lines once you start producing legitimate
   ROI colormaps */
   if (!strcmp(Name,"roi128")) Name = "ygbrp128"; /* Don't ask */
   else if (!strcmp(Name,"roi256")) Name = "ygbrp256"; /* Don't ask */
   else if (!strcmp(Name,"roi64")) Name = "ygbrp64"; /* Don't ask */
   
   SUMA_RETURN(SUMA_Find_ColorMap(Name,  
                                  SUMAg_CF->scm->CMv, 
                                  SUMAg_CF->scm->N_maps, -2 ));
}
#endif

/*! 
   Returns one of a bunch of standard SUMA colormaps
   CM = SUMA_MakeStandardMap (mapname);
   
   \param mapname (int) type of color map, choose from  
      SUMA_CMAP_RGYBR20
      SUMA_CMAP_BGYR19
      SUMA_CMAP_GRAY20
      SUMA_CMAP_GRAY02
      SUMA_CMAP_nGRAY20
      SUMA_CMAP_BW20
      SUMA_CMAP_MATLAB_DEF_BYR64
   \return CM (SUMA_COLOR_MAP*) color map structure (NULL in case of error)
*/

SUMA_COLOR_MAP * SUMA_MakeStandardMap (char *mapname)
{     static char FuncName[]={"SUMA_MakeStandardMap"};
      float **Fiducials;
      int k, nc;
      int *Nind;
      int Ncols, NFid;
      SUMA_COLOR_MAP * CM;
         
      SUMA_ENTRY;
      if (!strcmp(mapname,"rgybr20")) {
         Fiducials = (float **)SUMA_allocate2D(5, 3, sizeof(float));
         if (!Fiducials) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to allocate for Fiducials.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; 
            ++k;/* Red */
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; 
            ++k;/* Green */
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 1.0; 
            ++k;/* Blue */
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; 
            ++k;/* Yellow */
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; 
            ++k;/* Red */

         /* generate 20 colors colormap */
         CM = SUMA_MakeColorMap (Fiducials, k, 0, 20, YUP, mapname);
         /* free Fiducials */
         SUMA_free2D((char **)Fiducials, k);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else if (!strcmp(mapname,"bgyr19")) {
         Fiducials = (float **)SUMA_allocate2D(4, 3, sizeof(float));
         if (!Fiducials) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to allocate for Fiducials.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 1.0; 
            ++k;/* Blue */
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; 
            ++k;/* Green */
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; 
            ++k;/* Yellow */
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; 
            ++k;/* Red */

         /* generate 20 colors colormap */
         CM = SUMA_MakeColorMap (Fiducials, k, 0, 19, NOPE, mapname);
         
         /* free Fiducials */
         SUMA_free2D((char **)Fiducials, k);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else if (!strcmp(mapname,"gray02")) {
         Fiducials = (float **)SUMA_allocate2D(2, 3, sizeof(float));
         if (!Fiducials) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to allocate for Fiducials.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.4; 
            ++k;/* 0.4 gray */
         Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.7; 
            ++k;/* 0.8 gray */

         /* generate 2 colors colormap */
         CM = SUMA_MakeColorMap (Fiducials, k, 0, 2, NOPE, mapname);
         /* free Fiducials */
         SUMA_free2D((char **)Fiducials, k);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else if (!strcmp(mapname,"gray_i02")) {   
         Fiducials = (float **)SUMA_allocate2D(2, 3, sizeof(float));
         if (!Fiducials) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to allocate for Fiducials.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.7; ++k;
         Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.4; ++k;

         /* generate 2 colors colormap */
         CM = SUMA_MakeColorMap (Fiducials, k, 0, 2, NOPE, mapname);
         /* free Fiducials */
         SUMA_free2D((char **)Fiducials, k);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else if (!strcmp(mapname,"gray20")) {  
         Fiducials = (float **)SUMA_allocate2D(2, 3, sizeof(float));
         if (!Fiducials) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to allocate for Fiducials.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.3; 
            ++k;/* 0.3 gray */
         Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.8; 
            ++k;/* 0.8 gray */

         /* generate 20 colors colormap */
         CM = SUMA_MakeColorMap (Fiducials, k, 0, 20, NOPE, mapname);
         /* free Fiducials */
         SUMA_free2D((char **)Fiducials, k);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else if (!strcmp(mapname,"ngray20")) {
         Fiducials = (float **)SUMA_allocate2D(2, 3, sizeof(float));
         if (!Fiducials) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to allocate for Fiducials.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.3; 
            ++k;/* 0.3 gray */
         Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.8; 
            ++k;/* 0.8 gray */

         /* generate 20 colors colormap */
         CM = SUMA_MakeColorMap (Fiducials, k, 0, 20, NOPE, mapname);
         /* free Fiducials */
         SUMA_free2D((char **)Fiducials, k);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else if (!strcmp(mapname,"bw20")) {
         Fiducials = (float **)SUMA_allocate2D(2, 3, sizeof(float));
         if (!Fiducials) {
            fprintf (SUMA_STDERR,
               "Error %s: Failed to allocate for Fiducials.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.0; 
            ++k;/* black  */
         Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 1.0; 
            ++k;/* white */

         /* generate 20 colors colormap */
         CM = SUMA_MakeColorMap (Fiducials, k, 0, 20, NOPE, mapname);
         /* free Fiducials */
         SUMA_free2D((char **)Fiducials, k);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else if (!strcmp(mapname,"matlab_default_byr64") ||
                !strcmp(mapname,"byr64")) {
         /* default matlab color map */
         Ncols = 64;
         NFid = 10;

         Fiducials = (float **)SUMA_allocate2D(NFid, 3, sizeof(float));
         Nind = (int *) SUMA_calloc (NFid, sizeof (int));

         if (!Fiducials || !Nind) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to allocate for Fiducials or Nind.\n", 
                     FuncName);
            SUMA_RETURN (NULL);
         }

         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.5625; 
            Nind[k] = 0; ++k; 
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 1.0; 
            Nind[k] = 7; ++k;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.5; Fiducials[k][2] = 1.0; 
            Nind[k] = 15; ++k;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 1.0; 
            Nind[k] = 23; ++k;
         Fiducials[k][0] = 0.5; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.5625; 
            Nind[k] = 31; ++k;
         Fiducials[k][0] = 0.5625; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.5; 
            Nind[k] = 32; ++k;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; 
            Nind[k] = 40; ++k;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.5; Fiducials[k][2] = 0.0; 
            Nind[k] = 48; ++k;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0;    
            Nind[k] = 56; ++k;
         Fiducials[k][0] = 0.5625; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0;
            Nind[k] = 63; ++k;

         /* generate 20 colors colormap */
         CM = SUMA_MakeColorMap_v2 (Fiducials, k, 0, Nind, NOPE, mapname);

         /* free Fiducials & Nind*/
         SUMA_free2D((char **)Fiducials, k);
         SUMA_free(Nind);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else if (!strcmp(mapname,"bgyr64")) {
         /* default matlab color map */
         Ncols = 64;
         NFid = 10;

         Fiducials = (float **)SUMA_allocate2D(NFid, 3, sizeof(float));
         Nind = (int *) SUMA_calloc (NFid, sizeof (int));

         if (!Fiducials || !Nind) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to allocate for Fiducials or Nind.\n", 
                     FuncName);
            SUMA_RETURN (NULL);
         }

         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.5625; 
            Nind[k] = 0; ++k; 
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 1.0; 
            Nind[k] = 7; ++k;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.5; Fiducials[k][2] = 1.0; 
            Nind[k] = 15; ++k;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 1.0; 
            Nind[k] = 18; ++k;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.5; Fiducials[k][2] = 0.0; 
            Nind[k] = 24; ++k;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; 
            Nind[k] = 32; ++k;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; 
            Nind[k] = 43; ++k;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.5; Fiducials[k][2] = 0.0; 
            Nind[k] = 48; ++k;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; 
            Nind[k] = 56; ++k;
         Fiducials[k][0] = 0.5625; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; 
            Nind[k] = 63; ++k;

         /* generate 64 colors colormap */
         CM = SUMA_MakeColorMap_v2 (Fiducials, k, 0, Nind, NOPE, mapname);

         /* free Fiducials & Nind*/
         SUMA_free2D((char **)Fiducials, k);
         SUMA_free(Nind);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else if (!strcmp(mapname,"oldroi256") || !strcmp(mapname, "ygbrp256")) {
         /* a large colormap for lots of ROI drawing */
         Ncols = 256;
         NFid = 6;

         Fiducials = (float **)SUMA_allocate2D(NFid, 3, sizeof(float));
         Nind = (int *) SUMA_calloc (NFid, sizeof (int));

         if (!Fiducials || !Nind) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to allocate for Fiducials or Nind.\n", 
                     FuncName);
            SUMA_RETURN (NULL);
         }

         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0; 
            Nind[k] = 0; ++k; 
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0; 
            Nind[k] = 50; ++k;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0; Fiducials[k][2] = 1.0; 
            Nind[k] = 100; ++k;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; 
            Nind[k] = 150; ++k;
         Fiducials[k][0] = 0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 1; 
            Nind[k] = 200; ++k;
         Fiducials[k][0] = 1; Fiducials[k][1] = 0; Fiducials[k][2] = 1; 
            Nind[k] = 255; ++k;


         /* generate colormap */
         CM = SUMA_MakeColorMap_v2 (Fiducials, k, 0, Nind, NOPE, mapname);

         /* free Fiducials & Nind*/
         SUMA_free2D((char **)Fiducials, k);
         SUMA_free(Nind);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else if (!strcmp(mapname,"oldroi128") || !strcmp(mapname,"ygbrp128")) {
         /* a large colormap for lots of ROI drawing */
         Ncols = 128;
         NFid = 6;

         Fiducials = (float **)SUMA_allocate2D(NFid, 3, sizeof(float));
         Nind = (int *) SUMA_calloc (NFid, sizeof (int));

         if (!Fiducials || !Nind) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to allocate for Fiducials or Nind.\n", 
                     FuncName);
            SUMA_RETURN (NULL);
         }

         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0; 
            Nind[k] = 0; ++k; 
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0; 
            Nind[k] = 25; ++k;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0; Fiducials[k][2] = 1.0; 
            Nind[k] = 50; ++k;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; 
            Nind[k] = 75; ++k;
         Fiducials[k][0] = 0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 1; 
            Nind[k] = 100; ++k;
         Fiducials[k][0] = 1; Fiducials[k][1] = 0; Fiducials[k][2] = 1; 
            Nind[k] = 127; ++k;


         /* generate colormap */
         CM = SUMA_MakeColorMap_v2 (Fiducials, k, 0, Nind, NOPE, mapname);

         /* free Fiducials & Nind*/
         SUMA_free2D((char **)Fiducials, k);
         SUMA_free(Nind);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else if (!strcmp(mapname,"oldroi64") || !strcmp(mapname,"ygbrp64")) {
         /* a large colormap for lots of ROI drawing */
         Ncols = 64;
         NFid = 6;

         Fiducials = (float **)SUMA_allocate2D(NFid, 3, sizeof(float));
         Nind = (int *) SUMA_calloc (NFid, sizeof (int));

         if (!Fiducials || !Nind) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed to allocate for Fiducials or Nind.\n", 
                     FuncName);
            SUMA_RETURN (NULL);
         }

         /* create the fiducial colors */
         k = 0;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0; 
            Nind[k] = 0; ++k; 
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0; 
            Nind[k] = 12; ++k;
         Fiducials[k][0] = 0.0; Fiducials[k][1] = 0; Fiducials[k][2] = 1.0; 
            Nind[k] = 25; ++k;
         Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; 
            Nind[k] = 33; ++k;
         Fiducials[k][0] = 0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 1; 
            Nind[k] = 50; ++k;
         Fiducials[k][0] = 1; Fiducials[k][1] = 0; Fiducials[k][2] = 1; 
            Nind[k] = 63; ++k;


         /* generate colormap */
         CM = SUMA_MakeColorMap_v2 (Fiducials, k, 0, Nind, NOPE, mapname);

         /* free Fiducials & Nind*/
         SUMA_free2D((char **)Fiducials, k);
         SUMA_free(Nind);

         if (!CM) {
            fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
            SUMA_RETURN (NULL);   
         }
      }else {
         fprintf (SUMA_STDERR,
                  "Error %s: Unrecognized color map name.\n", FuncName);
         SUMA_RETURN (NULL);
      
      }
      
      SUMA_RETURN (CM);
   }


/*!
   \brief flips a color map upside down 
   
   \param CM (SUMA_COLOR_MAP *) to be flipped
*/
void SUMA_Flip_Color_Map (SUMA_COLOR_MAP *CM)
{
   static char FuncName[] = {"SUMA_Flip_Color_Map"};
   float orig[3]={ SUMA_CMAP_ORIGIN };
   float topright[3] = { SUMA_CMAP_TOPLEFT };
   int lim, i, j, c;
   float t;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!CM) SUMA_RETURNe;
   
   if (!CM->M) SUMA_RETURNe;
   
   lim = CM->N_M[0]/2;
   for (j=0; j < CM->N_M[1]; ++j) {  /* loop accross R,G,B columns */
      for (i=0; i < lim; ++i) {
         t = CM->M[i][j];           /* store color at i to be flipped */
         c = CM->N_M[0] - i - 1;     /* index of color to replace one at i */
         CM->M[i][j] = CM->M[c][j]; /* replace color at i */
         CM->M[c][j] = t;           /* put old color of i ar c */
      } 
   }
      
   if (CM->SO) { /* Free it, and recreate it */
      SUMA_Free_Surface_Object(CM->SO);
      CM->SO = SUMA_Cmap_To_SO(CM, orig, topright, 0);; 
   }
   
   #if 0 /* not sure you want to do that. If you flip
            after rotation, you'll be lost */
   for (j=0; j < CM->N_M[1]; ++j)
      CM->M0[j] = CM->M[CM->N_M[0]-1][j]; 
   #endif
   
   CM->flipped = !CM->flipped;
   SUMA_RETURNe;
}

/*!
   \brief rotates a color map by a fraction of its
   length 
   frac = 0, ET go home
   frac < 0 rotate down
   frac > 0 rotate up
   abs(frac) = 1, go one color at a time 
   \param CM (SUMA_COLOR_MAP *) to be flipped
*/
int SUMA_Rotate_Color_Map (SUMA_COLOR_MAP *CM, float frac)
{
   static char FuncName[] = {"SUMA_Rotate_Color_Map"};
   float orig[3]={ SUMA_CMAP_ORIGIN };
   float topright[3] = { SUMA_CMAP_TOPLEFT };
   float **orig_cols=NULL, tdist=0.0, tdistmin=0.0;
   int i, j, di, ic, dmin;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!CM) SUMA_RETURN(0);
   
   if (!CM->M) SUMA_RETURN(0);
   
   /* make copy */
   orig_cols = (float **)SUMA_allocate2D (CM->N_M[0], CM->N_M[1], sizeof(float));
   for (j=0; j < CM->N_M[1]; ++j) { 
      for (i=0; i < CM->N_M[0]; ++i) {
         orig_cols[i][j] = CM->M[i][j];  
      }
   }
   
   /* shift */
   if (frac == 0.0f) {  /* come back baby */
      tdistmin = 1000;
      dmin = 900;
      for (i=0; i < CM->N_M[0]; ++i) {
         tdist = 0.0;
         for (j=0; j< CM->N_M[1]; ++j) {
            tdist += SUMA_POW2(CM->M[i][j]-CM->M0[j]); 
         }
         if (tdist <= tdistmin) {
            tdistmin = tdist;
            dmin = i;
         }
      }
     if (!CM->flipped) di = dmin;
     else di = dmin+1;
   } else if (SUMA_ABS(frac) == 1.0f) {   /* one color at a time */
      di = 1;
   } else {
      di = SUMA_ROUND(SUMA_ABS(frac*CM->N_M[0]));
      if (di > CM->N_M[0]/2) di = CM->N_M[0]/2;
      if (di < 1) di = 1;
   }
   SUMA_LHv("A shift of %d colors (frac %f, N_M[0,1] [%d, %d])...\n",
            di, frac, CM->N_M[0], CM->N_M[1]); 
   if (frac > 0) {
      for (i=0; i < CM->N_M[0]; ++i)  {
         ic = (i+di) % CM->N_M[0];
         for (j=0; j < CM->N_M[1]; ++j) {
            CM->M[ic][j] = orig_cols[i][j];
         }
      }
   } else {
      for (i=0; i < CM->N_M[0]; ++i)  {
         ic = (i+di) % CM->N_M[0];
         for (j=0; j < CM->N_M[1]; ++j) {
            CM->M[i][j] = orig_cols[ic][j];
         }
      }
   }
   
   SUMA_free2D((char **)orig_cols, CM->N_M[0]); orig_cols = NULL;
      
   if (CM->SO) { /* Free it, and recreate it here or suffer
                  from asynchronous display related problems if you
                  wait for that to happen when DrawCmap is
                  doing it. */
      SUMA_Free_Surface_Object(CM->SO);
      CM->SO = SUMA_Cmap_To_SO(CM, orig, topright, 0);; 
   }
      
   SUMA_RETURN(di);
}

/*! 
   A function to compute the percentile range.
   
   Vsort = SUMA_PercRange (V, Vsort, N_V, PercRange, PercRangeVal, iPercRangeVal)
   
   \param V (float *) pointer to vector containing N_V values
   \param Vsort (float *) pointer to sorted version of V. 
      NOTE: If you want the function to sort V for you then pass NULL here and 
       expect the pointer to Vsort to be returned
   \param N_V (int) number of values in V
   \param PercRange (float *) 2x1 vector with percentile range desired (values between 0 and 100)
   \param PercRangeVal (float *) 2x1 vector with values in V corresponding the percentile range
   \param iPercRangeVal (int *) 2 x 1 vector containing indices into Vsort of PercRangeVal.
                                i.e.   PercRangeVal[0] = Vsort[iPercRangeVal[0]];
                                       PercRangeVal[1] = Vsort[iPercRangeVal[1]];
                                pass NULL if you do not care for it.                            
   \ret Vsort, pointer to the sorted version of V. NULL in case of error.
      NOTE: Before a NULL is returned, Vsort is freed.
   
   This function only allocates space for Vsort if a null is passed for Vsort in the function call
   
   \sa SUMA_dPercRange
*/
float * SUMA_PercRange (float *V, float *Vsort, int N_V, float *PercRange, float *PercRangeVal, int *iPercRangeVal)
{
   static char FuncName[] = {"SUMA_PercRange"};
   int *isort, il, ih;
   
   SUMA_ENTRY;

   if (PercRange[0] < 0 || PercRange[0] > 100 || PercRange[1] < 0 || PercRange[1] > 100) {
      fprintf (SUMA_STDERR, "Error %s: Values in PercRange must be between 0 and 100.\nVsort will be freed.\n", FuncName);
      if (Vsort) SUMA_free(Vsort);
      SUMA_RETURN (NULL);
   }
    
   if (!Vsort) {
      /* need to create my own sorted version */
        Vsort = (float *)SUMA_calloc (N_V, sizeof(float));
      if (!Vsort) {
         fprintf (SUMA_STDERR, "Error %s: Failed to allocate for Vsort.\n", FuncName);
         SUMA_RETURN (NULL);
      }
      /* copy V to Vsort */
      SUMA_COPY_VEC (V, Vsort, N_V, float, float);
      
      /* sort Vsort */
      isort = SUMA_z_qsort (Vsort  , N_V ); SUMA_free(isort);
   } 
   
   /* choose the index for the lower range */
   il = (int)rint((N_V-1)*PercRange[0]/100.0);
   ih = (int)rint((N_V-1)*PercRange[1]/100.0);
   PercRangeVal[0] = Vsort[il];
   PercRangeVal[1] = Vsort[ih];
   if (iPercRangeVal) { 
      iPercRangeVal[0] = il; iPercRangeVal[1] = ih;
   }
   SUMA_RETURN (Vsort);
}
/*!
   Vsort = SUMA_dPercRange (V, Vsort, N_V, PercRange, PercRangeVal, iPercRangeVal)
   the double version of SUMA_PercRange, working with double instead of float data
   \sa SUMA_PercRange
*/
double * SUMA_dPercRange (double *V, double *Vsort, int N_V, double *PercRange, double *PercRangeVal, int *iPercRangeVal)
{
   static char FuncName[] = {"SUMA_dPercRange"};
   int *isort, il, ih;
   
   SUMA_ENTRY;

   if (PercRange[0] < 0 || PercRange[0] > 100 || PercRange[1] < 0 || PercRange[1] > 100) {
      fprintf (SUMA_STDERR, "Error %s: Values in PercRange must be between 0 and 100.\nVsort will be freed.\n", FuncName);
      if (Vsort) SUMA_free(Vsort);
      SUMA_RETURN (NULL);
   }
    
   if (!Vsort) {
      /* need to create my own sorted version */
        Vsort = (double *)SUMA_calloc (N_V, sizeof(double));
      if (!Vsort) {
         fprintf (SUMA_STDERR, "Error %s: Failed to allocate for Vsort.\n", FuncName);
         SUMA_RETURN (NULL);
      }
      /* copy V to Vsort */
      SUMA_COPY_VEC (V, Vsort, N_V, double, double);
      
      /* sort Vsort */
      isort = SUMA_z_doubqsort (Vsort  , N_V ); SUMA_free(isort);
   } 
   
   /* choose the index for the lower range */
   il = (int)rint((N_V-1)*PercRange[0]/100.0);
   ih = (int)rint((N_V-1)*PercRange[1]/100.0);
   PercRangeVal[0] = Vsort[il];
   PercRangeVal[1] = Vsort[ih];
   if (iPercRangeVal) { 
      iPercRangeVal[0] = il; iPercRangeVal[1] = ih;
   }
   SUMA_RETURN (Vsort);
}

void SUMA_freeXformDatum (void *dd) {
   if (dd) NI_free_element(dd); return;
}

void SUMA_freeCallbackDatum(void *dd) {
   if (dd) NI_free_element(dd); return;
}

/*!
   Function to allocate and initialize an Overlays pointer
   
   ans = SUMA_CreateOverlayPointer (Name, SUMA_DSET *dset, char *ownerid, SUMA_OVERLAYS *recycle);
   
   \param Name (char *): A character string containing the name of the color overlay
   \param dset (SUMA_DSET *): Pointer to data set structure that this plane gets its 
                              data from. 
   \param ownerid (char *) idcode of owner of colorplane. Can set it to NULL if you
                           don't care.
   \param recycle (SUMA_OVERLAYS *): Reuse this overlays pointer (used to replace current overlay pointer.
   \ret ans (SUMA_OVERLAYS *): a pointer to the structure containing the color overlay
      NULL is returned in case of trouble.
      
   The following fields are set to these defaults:
   Show = NOPE;
   GlobalOpacity = -1.0;
   LocalOpacity vector is all zeros except the first value is -1.0
   FullList = 1
   PlaneOrder = -1; i.e. not set 
   isBackGrnd = 0 ; i.e. none
   SymIrange = 0;
   \sa SUMA_FreeOverlayPointer 
    
*/
SUMA_OVERLAYS * SUMA_CreateOverlayPointerIdentifiers(
                        int N_Nodes, const char *Name, 
                        SUMA_DSET *dset, char *ownerid)
{
   static char FuncName[]={"SUMA_CreateOverlayPointerIdentifiers"};
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_FileName sfn;
   int N_Alloc = -1, i=0;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
    
   Sover = (SUMA_OVERLAYS *)SUMA_calloc(1, sizeof(SUMA_OVERLAYS));
   if (!Sover) {
      fprintf (SUMA_STDERR,
               "Error %s: Could not allocate for Sover.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   Sover->N_links = 0;
   if (ownerid) sprintf(Sover->owner_id, "%s", ownerid);
   else Sover->owner_id[0] = '\0';
   Sover->LinkedPtrType = SUMA_LINKED_OVERLAY_TYPE;
   
   /* make a link to dset */
   SUMA_LH("Linking to Dset");
   Sover->dset_link = (SUMA_DSET *)SUMA_LinkToPointer ((void *)dset);
   /* N_Nodes is no longer used, use it for sanity check only */
   if (Sover->dset_link->dnel) {
      if (N_Nodes != SDSET_VECLEN(Sover->dset_link)) {
         SUMA_S_Errv("N_Nodes (%d) not equal to vec_len (%d)",
                     N_Nodes , SDSET_VECLEN(Sover->dset_link));
         SUMA_RETURN(NULL);
      }
  } else { SUMA_SL_Err ("No nel yet !"); SUMA_RETURN(NULL);}
   
   /* copy the name */
   Sover->Name = (char *)SUMA_calloc (strlen(Name)+1, sizeof(char));
   Sover->Name = strcpy(Sover->Name, Name);
   
   /* create a label */
   sfn = SUMA_StripPath((char *)Name);
   Sover->Label = sfn.FileName;
   if (sfn.Path) SUMA_free(sfn.Path); /* get rid of path */
   
   Sover->rowgraph_mtd=NULL;
   Sover->rowgraph_num=0;
      
   Sover->N_Contours = 0;
   Sover->Contours = NULL;
      
   SUMA_RETURN(Sover);   
}

SUMA_OVERLAYS * SUMA_CreateOverlayPointer (
                     const char *Name, 
                     SUMA_DSET *dset, char *ownerid, 
                     SUMA_OVERLAYS *Recycle)
{
   static char FuncName[]={"SUMA_CreateOverlayPointer"};
   SUMA_OVERLAYS *Sover=NULL;
   NI_group *ncmap=NULL;
   SUMA_FileName sfn;
   int N_Alloc = -1, i=0;
   int N_Nodes = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!dset) {
      SUMA_SL_Err("Need dset");
      SUMA_RETURN(NULL);
   }
   
   N_Nodes = SDSET_VECLEN(dset);    /* Mar. 2009 */ 
   
   if (!Recycle) { /* a new puppy */
      if (!(Sover = SUMA_CreateOverlayPointerIdentifiers(N_Nodes, Name, 
                                                         dset, ownerid))) {
         SUMA_S_Err("Failed to create overlay pointer identifiers.");
         SUMA_RETURN(NULL);
      }
   } else {
      Sover = Recycle;
      /* cleanup things to be replaced */
      if (!SUMA_FreeOverlayPointerRecyclables (Sover)) {
         SUMA_SL_Err("Recycling failed!\nEarth is doomed.");
         SUMA_FreeOverlayPointer(Sover);
         SUMA_RETURN(NULL);
      }
   }
   
   SUMA_LH("Allocating for vectors");
   N_Alloc = COLP_N_ALLOC(Sover);
   
   if (N_Alloc != N_Nodes) {
      SUMA_SL_Err("This is not supposed to be.");
      SUMA_RETURN(NULL);
   }
   Sover->N_NodeDef = N_Nodes;
   Sover->NodeDef = (int *) SUMA_calloc(N_Alloc, sizeof(int));
   for (i=0; i < Sover->N_NodeDef; ++i) Sover->NodeDef[i] = i;
   Sover->FullList = 1;
   

   Sover->ColVec = (float *)SUMA_calloc(N_Alloc*3, sizeof(float));
   Sover->LocalOpacity = (float *)SUMA_calloc(N_Alloc, sizeof(float));
   Sover->LocalOpacity[0] = -1.0; /* flag indicating local facts 
                                       have not been initialized */
   
   if (!Sover->ColVec || !Sover->LocalOpacity || !Sover->NodeDef) {
      fprintf (SUMA_STDERR,
               "Error %s: Could not allocate for Sover fields.\n", FuncName);
      SUMA_FreeOverlayPointer(Sover);
      SUMA_RETURN (NULL);   
   }
   
   if (!Recycle) {
      Sover->GlobalOpacity = -1.0; /* no factor applied */
      Sover->ShowMode = -SW_SurfCont_DsetViewCol;
      Sover->PlaneOrder = -1; /* No order is specified */
      Sover->isBackGrnd = 0; /* no brightness modulation effects */
      Sover->DimFact = 0.3;
      Sover->ForceIntRange[0] = 0.0; 
      Sover->ForceIntRange[1] = 0.0; /* force nothing */

      /* new, from Feb 20 */
      /* default, choose something */
      SUMA_LH("SCM stuff");
      if (!SUMAg_CF->scm) {  /* try building it */ 
         SUMAg_CF->scm = SUMA_Build_Color_maps();
      }
      if (!SUMAg_CF->scm) {
         SUMA_LH("SUMA color maps not set up.");
         Sover->cmapname = NULL;
         Sover->OptScl = NULL;
      } else {
         if (!SUMA_is_Label_dset(dset, &ncmap)) {
            char *eee=getenv("SUMA_DsetColorMap");
            if (eee) {
               if (!SUMA_FindNamedColMap(eee)) {
                  Sover->cmapname = SUMA_copy_string("Spectrum:red_to_blue");
                  SUMA_S_Errv( "Colormap %s not found.\n"
                              "Using Spectrum:red_to_blue instead.\n", eee);
               } else Sover->cmapname = SUMA_copy_string(eee);
            } else {
               Sover->cmapname = SUMA_copy_string("Spectrum:red_to_blue");
            }
         } else {
            /* this is a label dset */
            if (!ncmap) { /*  have no colormap, throw one in */
               if (!(ncmap = SUMA_CreateCmapForLabelDset(dset, NULL))) {
                  SUMA_S_Err("Failed to create new label dset cmap");
               }
               /* stick color map in database */
               if (!SUMA_Insert_Cmap_of_Dset(dset)) {
                  SUMA_S_Err("Failed to insert Cmap");
                  SUMA_FreeDset(dset); dset = NULL;
                  SUMA_RETURN(NOPE);
               }
               Sover->cmapname = SUMA_copy_string(
                                       NI_get_attribute(ncmap,"Name"));
            }
         }
         Sover->OptScl = SUMA_ScaleToMapOptInit();
         if (!Sover->OptScl) {
            fprintf (SUMA_STDERR,
                     "Error %s: Could not get scaling option structure.\n",
                     FuncName);
            SUMA_RETURN (NOPE); 
         }
         if (SUMA_is_Label_dset(dset, NULL)) {
            Sover->OptScl->interpmode = SUMA_DIRECT; /* default for such dsets */
         }
      }

      Sover->SymIrange = 0;
   }
   
   SUMA_RETURN (Sover);
}

/*!
   \brief releases an overlay pointer (decrement its inode count and 
                                       free it if necessary)
*/
SUMA_Boolean SUMA_ReleaseOverlay (SUMA_OVERLAYS * Overlays, 
                                  SUMA_INODE *Overlays_Inode)
{
   static char FuncName[]={"SUMA_ReleaseOverlay"};
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (Overlays_Inode || Overlays) { /* there should be no case where only one of             two is null but if such a case existed, you'll get notified below. */
      if (SUMA_ReleaseLink(Overlays_Inode)) { 
         /* some links are left, do not free memory */
      } else {
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Overlay plane %s is free of links, "
                     "freeing allocated memory ...\n", FuncName, Overlays->Name);
         if (Overlays) SUMA_FreeOverlayPointer (Overlays);
         if (Overlays_Inode) SUMA_free(Overlays_Inode); 
      }
   }   
   SUMA_RETURN(YUP);
}
/*! 
   Function to free an overlay structure 
   ans = SUMA_FreeOverlayPointer (Sover); 
   \param Sover (SUMA_OVERLAYS * ) 
   \ret ans (SUMA_Boolean) (YUP/NOPE)
   
   -WARNING, If YOU CREATED AN INODE FOR THIS POINTER, YOU NEED TO RELASE IT BEFORE YOU FREE Sover
   Perhaps you should use SUMA_FreeOverlay (SUMA_OVERLAYS * Sover, SUMA_INODE *Sover_Inode);
   
   If you free one overlay structure at a time, take care to make sure the plane orders still make sense
*/
SUMA_Boolean SUMA_FreeOverlayPointerRecyclables (SUMA_OVERLAYS * Sover)
{
   static char FuncName[]={"SUMA_FreeOverlayPointerRecyclables"};
   
   SUMA_ENTRY;

   if (Sover == NULL) {
      fprintf (SUMA_STDERR,
               "Error %s: Sover is NULL, nothing to do. Returning OK flag.\n", 
               FuncName);
      SUMA_RETURN (YUP);
   }
   
   if (Sover->NodeDef) SUMA_free(Sover->NodeDef); Sover->NodeDef = NULL;
   Sover->N_NodeDef = -1;
   Sover->FullList = -1;
   /* if (Sover->ColMat) SUMA_free2D ((char **)Sover->ColMat, Sover->N_Alloc); 
      Sover->ColMat = NULL*/
   if (Sover->ColVec)  SUMA_free(Sover->ColVec); 
   Sover->ColVec = NULL;
   if (Sover->LocalOpacity) SUMA_free(Sover->LocalOpacity); 
   Sover->LocalOpacity = NULL;
   
   
   SUMA_RETURN (YUP);
}

void SUMA_KillOverlayContours(SUMA_OVERLAYS * Sover) 
{

   int kkk=0;
   
   if (Sover) {
      if (Sover->Contours) {
         for (kkk=0; kkk<Sover->N_Contours; ++kkk) 
            if (Sover->Contours[kkk]) SUMA_freeDrawnROI(Sover->Contours[kkk]); 
         SUMA_free(Sover->Contours); 
      }
      Sover->Contours = NULL;
      Sover->N_Contours = 0;
   }
   
}

SUMA_Boolean SUMA_FreeOverlayPointer (SUMA_OVERLAYS * Sover) 
{
   static char FuncName[]={"SUMA_FreeOverlayPointer"};
   int kkk=0;
   
   SUMA_ENTRY;

   if (Sover == NULL) {
      fprintf (SUMA_STDERR,
               "Error %s: Sover is NULL, nothing to do. Returning OK flag.\n", 
               FuncName);
      SUMA_RETURN (YUP);
   }
   /* is this pointer used by others ? */
   if (Sover->N_links) {
      Sover = (SUMA_OVERLAYS*)SUMA_UnlinkFromPointer((void *)Sover);
      SUMA_RETURN (YUP);
   }
   
   /* No more links, go for it */
   if (Sover->dset_link) Sover->dset_link = 
      (SUMA_DSET *)SUMA_UnlinkFromPointer((void *)Sover->dset_link);
   if (Sover->Label) SUMA_free(Sover->Label);
   if (Sover->Name) SUMA_free(Sover->Name);
   if (Sover->cmapname) SUMA_free(Sover->cmapname);
   if (Sover->OptScl) SUMA_free(Sover->OptScl);
   if (Sover->rowgraph_mtd) Sover->rowgraph_mtd = NULL; /* that struct is killed
                                                           with delete_memplot
                                                           which is called with
                                                           donebut_CB in
                                                           plot_motif.c*/
   SUMA_KillOverlayContours(Sover);
   
   SUMA_free(Sover); Sover = NULL;
   
   SUMA_RETURN (YUP);
}

/*! 
   function to fetch a certain overlay pointer 
   ptr = SUMA_Fetch_OverlayPointer (Overlays, N_Overlays, Name, OverInd);
   
   if an overlay pointer of the name Name exists in SO then it is returned in ptr
   else ptr is null
   \param Overlays (SUMA_OVERLAYS **) vector of overlay plane pointers
   \param N_Overlays (int) number of overlay planes
   \param Name (const char *) name of overlay plane to fetch
   \param OverInd (int *) index of overlay plane in Overlays that is fetched (-1 if plane not found)
   \ret ptr (SUMA_OVERLAYS *)  pointer of overlay plane (= Overlays[*OverInd]) NULL if Name is not found
*/

SUMA_OVERLAYS * SUMA_Fetch_OverlayPointer (SUMA_OVERLAYS **Overlays, int N_Overlays, const char * Name, int * OverInd)
{
   static char FuncName[]={"SUMA_Fetch_OverlayPointer"};
   int i, ifound =-1, nfound = 0;
   SUMA_OVERLAYS *ptr= NULL;
   SUMA_PARSED_NAME *pn=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!Name) SUMA_RETURN(NULL);
   
   for (i=0; i < N_Overlays; ++i) {
      SUMA_LHv("Comparing\n>%s<\nto>%s<\n", Overlays[i]->Name, Name);
      if (!strcmp(Overlays[i]->Name, Name)) {
         *OverInd = i;
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Found overlay plane %s, indexed %d.\n", FuncName, Name, i);
         SUMA_RETURN (Overlays[i]);
      }
   }
   
   /* failed to find name, perhaps name is missing the path which would be OK as long as there is no conflict */
   nfound = 0;
   for (i=0; i < N_Overlays; ++i) {
      if (strlen(Overlays[i]->Name) > strlen(Name)) {
         if((pn = SUMA_ParseFname(Overlays[i]->Name, NULL))) {
            if (!strcmp(pn->FileName, Name)) {
               ifound = i; ++nfound;
            }
            SUMA_Free_Parsed_Name(pn); pn = NULL; 
         }
      }
   }
   if (nfound == 1) {
      i = ifound;
      *OverInd = i;
      if (LocalHead) fprintf (SUMA_STDOUT,"%s: Found overlay plane %s in secondary search, indexed %d.\n", FuncName, Name, i);
      SUMA_RETURN (Overlays[i]);
   } else if (nfound > 1) {
      SUMA_LHv("Found %d possible matches for %s\n", nfound, Name);
   } 
   
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Overlay plane %s was not found.\n", FuncName, Name);
   
   *OverInd = -1;

   SUMA_RETURN (NULL);
} 

/*!
   Look for overlay pointers for a particular dset 
*/
SUMA_OVERLAYS * SUMA_Fetch_OverlayPointerByDset (SUMA_OVERLAYS **Overlays, 
                                 int N_Overlays, SUMA_DSET *dset, int * OverInd)
{
   static char FuncName[]={"SUMA_Fetch_OverlayPointerByDset"};
   int i, nfound;
   SUMA_OVERLAYS *ptr= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!dset) {
      SUMA_SL_Warn("NULL dset");
      SUMA_RETURN(NULL);
   }
   
   if (LocalHead) {
      fprintf( SUMA_STDERR,
               "%s: Seaching from %d overlays for dset_link = %p\n", 
               FuncName, N_Overlays, dset);    
   }
   nfound = 0;
   *OverInd = -1;
   for (i=0; i < N_Overlays; ++i) {
      if (LocalHead) 
         fprintf( SUMA_STDERR,"%s: %p ?= %p\n", 
                  FuncName, Overlays[i]->dset_link, dset); 
      if (Overlays[i]->dset_link == dset) {
         *OverInd = i;
         ++nfound;
         if (LocalHead) 
            fprintf (SUMA_STDOUT,"%s: Found overlay plane %s, indexed %d.\n", 
                     FuncName, Overlays[i]->Name, i);
      }
   }
   
   if (*OverInd >= 0) {
      if (nfound == 1) {
         SUMA_RETURN (Overlays[*OverInd]);
      } else {
         SUMA_SL_Err("Multiple overlays found for dset!");
         *OverInd = -1;
         SUMA_RETURN(NULL);  
      }
   } 

   if (LocalHead) 
      fprintf (SUMA_STDOUT,"%s: Overlay plane for dset %p (%s) not found.\n", 
               FuncName, dset,SDSET_LABEL(dset));
   
   *OverInd = -1;

   SUMA_RETURN (NULL);
} 
 
/*!

   function to turn color overlay planes into GL color array
   
   --- usage prior to Fri Nov  7 15:58:57 EST 2003 ---
   ans = SUMA_Overlays_2_GLCOLAR4(Overlays, N_Overlays, glar_ColorList, N_Node, Back_Modfact, ShowBackground, ShowForeground);
   
   
   \param Overlays (SUMA_OVERLAYS **) a pointer to the vector of overlay planes structure pointers
   \param N_Overlays (int) number of overlay plane structures
   \param glar_ColorList (GLfloat *) pointer to vector (4*SO->N_Node long) that contains the node colors 
   \param N_Node (int) total number of nodes in Surface NOT in color overlay plane
   \param Back_Modfact (float) Background brightness modulation factor typically SAFNIg_cSV->Back_Modfact
   \param ShowBackground (SUMA_Boolean) flag for showing/hiding background (brightness modulating) colors
   \param ShowForeground (SUMA_Boolean) flag for showing/hiding foreground colors
   \ret YUP/NOPE
   \sa SUMA_MixOverlays

   --- current usage ---
   ans = SUMA_Overlays_2_GLCOLAR4(SO, SV, glar_ColorList)
   \param SO (SUMA_SurfaceObject *) surface structure with all the parameters listed in the old usage needed, and more
   \param SV (SUMA_SurfaceViewer *) surface viewer structure with all the parameters listed in the old usage needed, and more
   \param glar_ColorList (GLfloat *) pointer to vector (4*SO->N_Node long) that contains the node colors 
*/

SUMA_Boolean SUMA_Overlays_2_GLCOLAR4(SUMA_SurfaceObject *SO, 
                                 SUMA_SurfaceViewer *SV, GLfloat *glcolar)
{
   static char FuncName[]={"SUMA_Overlays_2_GLCOLAR4"};
   int ShowOverLays[SUMA_MAX_OVERLAYS], ShowOverLays_Back[SUMA_MAX_OVERLAYS]; 
   int   ShowOverLays_sort[SUMA_MAX_OVERLAYS], 
         ShowOverLays_Back_sort[SUMA_MAX_OVERLAYS], iloc[SUMA_MAX_OVERLAYS];
   int OverlayOrder_Back[SUMA_MAX_OVERLAYS], OverlayOrder[SUMA_MAX_OVERLAYS];
   int i, j, NshowOverlays, NshowOverlays_Back, *isort, i4, i4_0, i4_1, i4_2;
   SUMA_Boolean *isColored, *isColored_Fore, *isColored_Back;
   GLfloat *glcolar_Fore , *glcolar_Fore_tmp, *glcolar_Back;
   float avg_Back, avgfact;
   SUMA_OVERLAYS ** Overlays;
   int N_Overlays;  
   int N_Node; 
   float Back_Modfact;
   SUMA_Boolean ShowBackground; 
   SUMA_Boolean ShowForeground;
   SUMA_Boolean LocalHead = NOPE; /* local headline debugging messages */   
   
   SUMA_ENTRY;

   if (!SO || !SV || !glcolar) {
      SUMA_SL_Err("Null input to SUMA_Overlays_2_GLCOLAR4!");
      SUMA_RETURN(NOPE);
   }
   
   /* old variable names */
   Overlays = SO->Overlays;
   N_Overlays = SO->N_Overlays; 
   N_Node = SO->N_Node; 
   Back_Modfact = SV->Back_Modfact;
   ShowBackground = SV->ShowBackground; 
   ShowForeground = SV->ShowForeground;
   
   if (LocalHead)   { 
      SUMA_LHv("Showing all overlay planes.\n"
               "SurfCont->curColPlane=%s\n",
               SO->SurfCont->curColPlane ? 
                        SO->SurfCont->curColPlane->Label:"NULL");
      if (0) {
         SUMA_Print_Surface_Object(SO, NULL);
         SUMA_Show_ColorOverlayPlanes (Overlays, N_Overlays, 0); 
      }
   } 
   

   /* get the indices into the color structure vector of overlays to be shown */
   NshowOverlays = 0;
   NshowOverlays_Back = 0;
   for (j=0; j < N_Overlays; ++j) {
      if ( (Overlays[j]->ShowMode == SW_SurfCont_DsetViewCol ||
            Overlays[j]->ShowMode == SW_SurfCont_DsetViewCaC ) &&
           Overlays[j]->GlobalOpacity != 0) {
         if (Overlays[j]->isBackGrnd) {
            if (0) { /* There is no 
                        SO->SurfCont->ShowCurBackOnly and I am not
                        sure how useful it would be.
                        Also, it makes little sense to use ShowCurForeOnly,
                        because background colors are mixed separately,
                        and attenuate the resultant foreground mixture
                        even when just 'one' of the foreground is
                        shown... */
               if (SO->SurfCont->curColPlane == Overlays[j]) {
                  SUMA_LHv("Le ShowCurForeOnly %s in bg action\n",
                           Overlays[j]->Label);
                  ShowOverLays_Back[NshowOverlays_Back] = j; 
                  OverlayOrder_Back[NshowOverlays_Back] = 
                                                   Overlays[j]->PlaneOrder;
                  ++ NshowOverlays_Back;
               }
            } else {
               ShowOverLays_Back[NshowOverlays_Back] = j; 
               OverlayOrder_Back[NshowOverlays_Back] = Overlays[j]->PlaneOrder;
               ++ NshowOverlays_Back;
            }
         }else {
            if (SO->SurfCont->ShowCurForeOnly) {
               if (SO->SurfCont->curColPlane == Overlays[j]) {
                  SUMA_LHv("Le ShowCurForeOnly %s in action\n",
                          Overlays[j]->Label);
                  ShowOverLays[NshowOverlays] = j; 
                  OverlayOrder[NshowOverlays] = Overlays[j]->PlaneOrder;
                  ++ NshowOverlays;
               }
            } else {
               ShowOverLays[NshowOverlays] = j; 
               OverlayOrder[NshowOverlays] = Overlays[j]->PlaneOrder;
               ++ NshowOverlays;
            }
         }
      }
   }

   SUMA_LHv("Found %d Mix overlays and %d Mix-Brightmod overlays.\n", 
            NshowOverlays, NshowOverlays_Back);
   
   /* vvvvvvvvvvvvvvvvvvvvvvvvv allocate space ------------------------------*/
   
   isColored = (SUMA_Boolean *) SUMA_calloc (N_Node, sizeof(SUMA_Boolean));
                        /* allocate for flag indicating the a node is colored */
   if (!isColored) {
      fprintf (SUMA_STDERR,
               "Error %s: Failed to allocate for isColored.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   glcolar_Back = NULL;
   isColored_Back = NULL;
   if (ShowBackground) {
      if (NshowOverlays_Back) {
         glcolar_Back = (GLfloat *) SUMA_calloc (4*N_Node, sizeof(GLfloat));
         isColored_Back = (SUMA_Boolean *) 
                                 SUMA_calloc (N_Node, sizeof(SUMA_Boolean));

         if (!isColored_Back || !glcolar_Back) {
            SUMA_S_Err("Failed to allocate for isColored_Back || glcolar_Back.");
            SUMA_RETURN (NOPE);
         }
      }
   }
   
   isColored_Fore = NULL;
   glcolar_Fore = NULL;
   if (ShowForeground) {
      if (NshowOverlays) {
         glcolar_Fore = (GLfloat *) SUMA_calloc (4*N_Node, sizeof(GLfloat));
         isColored_Fore = (SUMA_Boolean *) 
                                 SUMA_calloc (N_Node, sizeof(SUMA_Boolean));

         if (!isColored_Fore || !glcolar_Fore) {
            SUMA_S_Err("Failed to allocate for isColored_Fore || glcolar_Fore");
            SUMA_RETURN (NOPE);
         }
      }      
   }
   /* ^^^^^^^^^^^^^^^^^^^^^^^^^^^ allocate space ------------------------------*/
   
   /* vvvvvvvvvvvvvvvvvvvvvvvvv Background colors -----------------------------*/
   
   if (ShowBackground) {
      /* arrange Background color planes by plane order in preparation for 
         mixing them */
         /* sort plane order */
         if (NshowOverlays_Back > 1) {
            isort = SUMA_z_dqsort (OverlayOrder_Back, NshowOverlays_Back );
            /* use sorting by plane order to reorder ShowOverlays */
            for (j=0; j < NshowOverlays_Back; ++j) {
               ShowOverLays_Back_sort[j] = ShowOverLays_Back[isort[j]];
            }
            /* done with isort, free it */
            SUMA_free(isort);
         } 
         if (NshowOverlays_Back == 1) {
               ShowOverLays_Back_sort[0] = ShowOverLays_Back[0];
         }


      /* mix the colors that will constitute background*/
      if (NshowOverlays_Back) {
         if (LocalHead)   
            fprintf (SUMA_STDERR,"%s: Mixing Background colors ...\n", FuncName);

         if (!SUMA_MixOverlays ( Overlays, N_Overlays, ShowOverLays_Back_sort, 
                                 NshowOverlays_Back, glcolar_Back, N_Node, 
                                 isColored_Back, NOPE)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_MixOverlays.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
      } else {
         ShowBackground = NOPE;
      } 
   } else {
      NshowOverlays_Back = 0;
   }
   /* ^^^^^^^^^^^^^^^^^^^^^^^^^^  Background colors --------------------------*/
   
  
   /* vvvvvvvvvvvvvvvvvvvvvvvvv Foreground  colors ----------------------------*/
   if (ShowForeground) {
      /* arrange foreground color planes by plane order */
         /* sort plane order */
         if (NshowOverlays > 1) {
            isort = SUMA_z_dqsort (OverlayOrder, NshowOverlays );
            /* use sorting by plane order to reorder ShowOverlays */
            for (j=0; j < NshowOverlays; ++j) {
               ShowOverLays_sort[j] = ShowOverLays[isort[j]];
            }
            /* done with isort, free it */
            SUMA_free(isort);
         } 
         if (NshowOverlays  == 1) {
            ShowOverLays_sort[0] = ShowOverLays[0];   
         }
   

      /* Now mix the foreground colors */
      if (NshowOverlays) {
            if (LocalHead)   
               fprintf (SUMA_STDERR,
                        "%s: Mixing Foreground colors ....\n", FuncName);
            if (!SUMA_MixOverlays ( Overlays, N_Overlays, ShowOverLays_sort, 
                                    NshowOverlays, glcolar_Fore, N_Node, 
                                    isColored_Fore, NOPE)) {
               fprintf (SUMA_STDERR,
                        "Error %s: Failed in SUMA_MixOverlays.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
            if (SUMAg_CF->X->NumForeSmoothing > 0) { 
               glcolar_Fore_tmp = NULL;
               glcolar_Fore_tmp = SUMA_SmoothAttr_Neighb_Rec (glcolar_Fore, 
                                       4*SO->N_Node, NULL, SO->FN, 4, 
                                       SUMAg_CF->X->NumForeSmoothing); 
               if (!glcolar_Fore_tmp) {
                  SUMA_SL_Err("Smoothing failed.\n");
               } else {
                  SUMA_free(glcolar_Fore); 
                  glcolar_Fore = glcolar_Fore_tmp; glcolar_Fore_tmp = NULL;
               }
            }
      } else {
         ShowForeground = NOPE;
      }
   } else {
      NshowOverlays = 0;
   }
   /* ^^^^^^^^^^^^^^^^^^^^^^^^^^^  Foreground colors -------------------------*/

   /* time to modulate the mixed colors with the average brightness */
   if (NshowOverlays && NshowOverlays_Back) {
      if (LocalHead)   
         fprintf (SUMA_STDERR,
                  "%s: Modulating Brightness of Foreground colors ...\n", 
                  FuncName);

      for (i=0; i < N_Node; ++i) {
         avgfact = Back_Modfact / 3.0;
         if (isColored_Fore[i] && isColored_Back[i]) { 
                     /* colors from both sides, adjust brightness */
            i4_0 = 4 * i; i4_1 = i4_0 + 1; i4_2 = i4_0 + 2; 
            if (!Back_Modfact) {
               glcolar[i4_0] = glcolar_Fore[i4_0];
               glcolar[i4_1] = glcolar_Fore[i4_1];
               glcolar[i4_2] = glcolar_Fore[i4_2];
            } else {
               avg_Back = (glcolar_Back[i4_0] + glcolar_Back[i4_1] + 
                           glcolar_Back[i4_2]) * avgfact ;   
               glcolar[i4_0] = avg_Back * glcolar_Fore[i4_0];
               glcolar[i4_1] = avg_Back * glcolar_Fore[i4_1];
               glcolar[i4_2] = avg_Back * glcolar_Fore[i4_2];
            }
               isColored[i] = YUP;
               continue;
         }
         if (isColored_Fore[i]) {
            i4 = 4 * i;
            glcolar[i4] = glcolar_Fore[i4]; ++i4;
            glcolar[i4] = glcolar_Fore[i4]; ++i4;
            glcolar[i4] = glcolar_Fore[i4]; ++i4;
            isColored[i] = YUP;
            continue;
         }
         if (isColored_Back[i]) {
            i4 = 4 * i;
            glcolar[i4] = glcolar_Back[i4]; ++i4;
            glcolar[i4] = glcolar_Back[i4]; ++i4;
            glcolar[i4] = glcolar_Back[i4]; ++i4;
            isColored[i] = YUP;
            continue;
         } else {
            /* has never been colored, put defaults */
            i4 = 4 * i;
            glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
            glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
            glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
            isColored[i] = NOPE;
         }
      }
      
      if (LocalHead)   
         fprintf (SUMA_STDERR,
                  "%s: Done Modulating Brightness of overlay colors.\n", 
                  FuncName);
   } 
   if (NshowOverlays && !NshowOverlays_Back) {
      if (LocalHead)   
         fprintf (SUMA_STDERR,"%s: Only Foreground colors.\n", FuncName);
         for (i=0; i < N_Node; ++i) {
            if (isColored_Fore[i]) {
               i4 = 4 * i;
               glcolar[i4] = glcolar_Fore[i4]; ++i4;
               glcolar[i4] = glcolar_Fore[i4]; ++i4;
               glcolar[i4] = glcolar_Fore[i4]; ++i4;
               isColored[i] = YUP;
               continue;
            } else {
               i4 = 4 * i;
               glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
               glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
               glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
               isColored[i] = NOPE;
            }
         }
   }
   
   if (!NshowOverlays && NshowOverlays_Back) {
      if (LocalHead)   
         fprintf (SUMA_STDERR,"%s: Only Background colors.\n", FuncName);
         for (i=0; i < N_Node; ++i) {
            if (isColored_Back[i]) {
               i4 = 4 * i;
               glcolar[i4] = glcolar_Back[i4]; ++i4;
               glcolar[i4] = glcolar_Back[i4]; ++i4;
               glcolar[i4] = glcolar_Back[i4]; ++i4;
               isColored[i] = YUP;
               continue;
            } else {
               i4 = 4 * i;
               glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
               glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
               glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
               isColored[i] = NOPE;
            }
         }
   }
   
   if (!(ShowBackground) && !ShowForeground) {
      for (i=0; i < N_Node; ++i) {
         i4 = 4 * i;
         glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
         glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
         glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
      }
   } else {
      /* any final airbrushing ? */
      if (SUMAg_CF->X->NumFinalSmoothing > 0) { 
         glcolar_Fore_tmp = NULL;
         glcolar_Fore_tmp = SUMA_SmoothAttr_Neighb_Rec (glcolar, 
                                 4*SO->N_Node, NULL, SO->FN, 4, 
                                 SUMAg_CF->X->NumFinalSmoothing); 
         if (!glcolar_Fore_tmp) {
            SUMA_SL_Err("Smoothing failed.\n");
         } else {
            memcpy(glcolar,glcolar_Fore_tmp, 4*SO->N_Node*sizeof(GLfloat));
            SUMA_free(glcolar_Fore_tmp); 
         }
      }
   }
   
   /* free this mess and get out */   
   if (isColored) SUMA_free(isColored);
   if (isColored_Back) SUMA_free(isColored_Back);
   if (glcolar_Back) SUMA_free(glcolar_Back);
   if (isColored_Fore) SUMA_free(isColored_Fore);
   if (glcolar_Fore) SUMA_free(glcolar_Fore);
   
   SUMA_RETURN (YUP);
}
/*! 
   function to mix overlay plane colors 

   ans = SUMA_MixOverlays (SUMA_OVERLAYS ** Overlays, int N_Overlays, int *ShowOvelays, int N_ShowOverlays, GLfloat *glcolar, int N_Node, SUMA_Boolean *isColored, SUMA_Boolean FILL)
   
   Overlays (SUMA_OVERLAYS **) a pointer to the vector of overlay planes structure pointers
   N_Overlays (int) number of overlay plane structures
   ShowOvelays (int *) vector of Overlay plane indices to be used. The plane indices must be sorted by plane order. 
   N_ShowOverlays (int) number of ovrlay planes used in the mixing
   glar_ColorList (GLfloat *) pointer to vector (4*SO->N_Node long) that contains the node colors 
   N_Node (int) total number of nodes in Surface NOT in color overlay plane
   \param isColored (SUMA_Boolean *) N_Node x1 vector containing flags indicating if a node received a color or not
   \param FILL (SUMA_Boolean) top off nodes that received no color with default gray
   
   \sa SUMA_Overlays_2_GLCOLAR4
*/
SUMA_Boolean SUMA_MixOverlays (  SUMA_OVERLAYS ** Overlays, int N_Overlays, 
                                 int *ShowOverlays, int NshowOverlays, 
                                 GLfloat *glcolar, int N_Node, 
                                 SUMA_Boolean *isColored, SUMA_Boolean FILL)
{    
   static char FuncName[] = {"SUMA_MixOverlays"};
   int i, j;
   int *NodeDef, N_NodeDef = -1;
   SUMA_Boolean Full, Fill, Locl, Glob;
   SUMA_Boolean LocalHead = NOPE; /* local headline debugging messages */   
   
   SUMA_ENTRY;

   if (!Overlays) {
      SUMA_SL_Err("Null Overlays!");
      SUMA_RETURN(NOPE);
   }
   if (!glcolar) {
      SUMA_SL_Err("Null glcolar!");
      SUMA_RETURN(NOPE);
   }
   if (!isColored) {
      fprintf (SUMA_STDERR, "Error %s: isColored is NULL.\n", FuncName); 
      SUMA_RETURN (NOPE);
   }
   if (!ShowOverlays) {
      SUMA_SL_Err("NULL ShowOverlays");
      SUMA_RETURN (NOPE);
   }
   if (!NshowOverlays) { /* nothing to see here */
      fprintf (SUMA_STDERR, "Warning %s: Nothing to do.\n", FuncName); 
      if (FILL) {
         fprintf (SUMA_STDERR, 
                  "Warning %s: Filling with blank default color\n", FuncName); 
         SUMA_FillBlanks_GLCOLAR4(isColored, N_Node, SUMA_GRAY_NODE_COLOR, 
                                  SUMA_GRAY_NODE_COLOR, SUMA_GRAY_NODE_COLOR, 
                                  glcolar);
      }
      SUMA_RETURN (YUP);
   }
   
   /* start building the node colors */
   Full = YUP;
   Glob = YUP;
   Locl = YUP;
   Fill = YUP; 
   for (j=0; j<NshowOverlays; ++j) {
      Full = YUP;
      Glob = YUP;
      Locl = YUP;
      Fill = YUP; 
      
      i = ShowOverlays[j];
      if (!Overlays[i]) {
         fprintf(SUMA_STDERR,"Error %s:\nNULL ShowOverlays[%d]\n", FuncName, i);
         SUMA_RETURN (NOPE);
      }  
     
      /* is this a full listing */
      SUMA_LHv("Full listing flag: %d\n", Overlays[i]->FullList);
      if (Overlays[i]->FullList) {         
         Fill = NOPE;
              /* Full list, no need to fill up unvisited nodes at the end */
      } else {         
         Full = NOPE; /* Not a full list */      
      }
      
      if (j > 0) { /* opacity plays a role when you are overlaying 
                      one plane on top of the other */
         /* is this a Global Factor */
         if (Overlays[i]->GlobalOpacity < 0.0) {         Glob = NOPE;      }

         /* is this a Local Factor */
         if (!Overlays[i]->LocalOpacity) {
            SUMA_S_Errv("NULL Overlays[%d]->LocalOpacity\n", i);
            SUMA_RETURN (NOPE);
         }
         if (Overlays[i]->LocalOpacity[0] < 0) {         Locl = NOPE;      }
      } else {
         Glob = NOPE; Locl = NOPE;
      }
      
      NodeDef = COLP_NODEDEF(Overlays[i]);
      N_NodeDef = COLP_N_NODEDEF(Overlays[i]);
   

      if (LocalHead) 
         fprintf (SUMA_STDOUT,"%s: Building color layer %d Overlay #%d: %s ...\nFull=%d, Glob=%d (Globopacity %f), Locl=%d,Fill=%d\n", \
         FuncName, j, i, Overlays[i]->Name, (int)Full, (int)Glob, Overlays[i]->GlobalOpacity, (int)Locl, (int)Fill);
      
         
      /* call the appropriate macro to add the overlay */
      if (Full && Glob && Locl) {
         if (SUMAg_CF->ColMixMode == SUMA_ORIG_MIX_MODE) {
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBv_FGL_AR4op ...\n", FuncName);
            /* This macro used to be called: SUMA_RGBmat_FullGlobLoc2_GLCOLAR4_opacity
            but name was too long for some compilers */
            SUMA_RGBv_FGL_AR4op(\
            Overlays[i]->ColVec, glcolar, N_Node, Overlays[i]->GlobalOpacity, Overlays[i]->LocalOpacity, isColored);         
         } else if (SUMAg_CF->ColMixMode == SUMA_4AML) {
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBv_FGL_AR4op2 ...\n", FuncName);
            SUMA_RGBv_FGL_AR4op2(\
            Overlays[i]->ColVec, glcolar, N_Node, Overlays[i]->GlobalOpacity, Overlays[i]->LocalOpacity, isColored); 
         }
      }
      
      if (!Full && Glob && Locl) {
         if (SUMAg_CF->ColMixMode == SUMA_ORIG_MIX_MODE) {
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBv_PGL_AR4op ...\n", FuncName);
            /* This macro used to be called: SUMA_RGBmat_PartGlobLoc2_GLCOLAR4_opacity */
            SUMA_RGBv_PGL_AR4op(\
            Overlays[i]->ColVec, NodeDef, glcolar, N_NodeDef, isColored, Overlays[i]->GlobalOpacity, Overlays[i]->LocalOpacity,  N_Node);
          } else if (SUMAg_CF->ColMixMode == SUMA_4AML) {  
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBv_PGL_AR4op2 ...\n", FuncName);
            SUMA_RGBv_PGL_AR4op2(\
            Overlays[i]->ColVec, NodeDef, glcolar, N_NodeDef, isColored, Overlays[i]->GlobalOpacity, Overlays[i]->LocalOpacity,  N_Node);
         }
      }
      
      if (Full && !Glob && Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling  SUMA_RGBv_FnGL_AR4op...\n", FuncName);
         /* This macro used to be called: SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4_opacity */
         SUMA_RGBv_FnGL_AR4op(\
         Overlays[i]->ColVec, glcolar, N_Node, Overlays[i]->LocalOpacity, isColored);         
      }
      
      if (!Full && !Glob && Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBv_PnGL_AR4op ...\n", FuncName);
         /* This macro used to be called: SUMA_RGBmat_PartNoGlobLoc2_GLCOLAR4_opacity*/
         SUMA_RGBv_PnGL_AR4op(\
         Overlays[i]->ColVec, NodeDef, glcolar, N_NodeDef, isColored, Overlays[i]->LocalOpacity, N_Node);
      }
      
      if (Full && !Glob && !Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBv_FnGnL_AR4op ...\n", FuncName);
         /* This macro used to be called: SUMA_RGBmat_FullNoGlobNoLoc2_GLCOLAR4_opacity*/
         SUMA_RGBv_FnGnL_AR4op(\
         Overlays[i]->ColVec, glcolar, N_Node, isColored);         
      }
      
      if (!Full && !Glob && !Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBv_PnGnL_AR4op ...\n", FuncName);  
         /* This macro used to be called: SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4_opacity */
         SUMA_RGBv_PnGnL_AR4op(\
         Overlays[i]->ColVec, NodeDef, glcolar, N_NodeDef, isColored, N_Node);
      }
      
      if (Full && Glob && !Locl) {
         if (SUMAg_CF->ColMixMode == SUMA_ORIG_MIX_MODE) {
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling  SUMA_RGBv_FGnL_AR4op...\n", FuncName);
            /* This macro used to be called: SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4_opacity*/
            SUMA_RGBv_FGnL_AR4op(\
            Overlays[i]->ColVec, glcolar, N_Node, Overlays[i]->GlobalOpacity, isColored);
         } else if (SUMAg_CF->ColMixMode == SUMA_4AML){
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling  SUMA_RGBv_FGnL_AR4op2...\n", FuncName);
            SUMA_RGBv_FGnL_AR4op2(\
            Overlays[i]->ColVec, glcolar, N_Node, Overlays[i]->GlobalOpacity, isColored);
         }
         
      }
       
      if (!Full && Glob && !Locl) {
         if (SUMAg_CF->ColMixMode == SUMA_ORIG_MIX_MODE) {
            if (LocalHead) {
               fprintf (SUMA_STDOUT,"%s: Calling  SUMA_RGBv_PGnL_AR4op...\n"
                                    "    N_NodeDef = %d, N_Node = %d\n", FuncName, N_NodeDef, N_Node);
            
            }
            /* This macro used to be called: SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4_opacity*/
            SUMA_RGBv_PGnL_AR4op(\
            Overlays[i]->ColVec, NodeDef, glcolar, N_NodeDef, isColored, Overlays[i]->GlobalOpacity, N_Node);
         } else if (SUMAg_CF->ColMixMode == SUMA_4AML){
            if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling  SUMA_RGBv_PGnL_AR4op2...\n", FuncName);
            SUMA_RGBv_PGnL_AR4op2(\
            Overlays[i]->ColVec, NodeDef, glcolar, N_NodeDef, isColored, Overlays[i]->GlobalOpacity, N_Node);
         }
      }
         
   }
   
   if (FILL && Fill) { /* nothing to see here */
      if (LocalHead) fprintf (SUMA_STDOUT,"%s: Some nodes received no colors from any of the overplanes, filling them with background color ...\n", FuncName);
      SUMA_FillBlanks_GLCOLAR4(isColored, N_Node, SUMA_GRAY_NODE_COLOR, SUMA_GRAY_NODE_COLOR, SUMA_GRAY_NODE_COLOR, glcolar);
      SUMA_RETURN (YUP);
   }
   
   SUMA_RETURN (YUP);
}

/*! 
   Function that shows the contents of overlay planes 
   ans = SUMA_Show_ColorOverlayPlanes (SUMA_OVERLAYS **Overlays, int N_Overlays) ;
   
   \param Overlays (SUMA_OVERLAYS **) vector of  pointers to overlay structures
   \param N_Overlays (int) number of overlay structures
   \ret ans (SUMA_Boolean)
   
   
*/
SUMA_Boolean SUMA_Show_ColorOverlayPlanes (
         SUMA_OVERLAYS **Overlays, int N_Overlays, int detail) 
{
   static char FuncName[]={"SUMA_Show_ColorOverlayPlanes"};
   char *s;
   
   SUMA_ENTRY;

   s = SUMA_ColorOverlayPlane_Info (Overlays, N_Overlays, detail);
   if (s) {
      fprintf (SUMA_STDERR,"%s\n", s);
      SUMA_free(s);
   }
   
   SUMA_RETURN (YUP);
}

/*!
   \brief Shows the contents of the color overlay planes
   \sa SUMA_Show_ColorOverlayPlanes (for backward compat.)
*/
char *SUMA_ColorOverlayPlane_Info (SUMA_OVERLAYS **Overlays, 
                                    int N_Overlays, int detail) 
{
   static char FuncName[]={"SUMA_ColorOverlayPlane_Info"};
   char stmp[1000], *s = NULL, *s2 = NULL;
   int i, j, ShowN, icmap;
   SUMA_COLOR_MAP *ColMap=NULL;
   int N_Alloc = -1, *NodeDef=NULL, N_NodeDef = -1;
   DListElmt *el=NULL;
   NI_element *nel = NULL;
   
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY; 
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   sprintf (stmp, "Info on %d color overlay planes:\n"
                  "---------------------------------\n", N_Overlays);
   SS = SUMA_StringAppend (SS,stmp);
   for (i=0; i < N_Overlays; ++i) {
      if (Overlays[i]) {
         sprintf (stmp, 
            "\n---> Overlay plane %s:\n"
            "pointer %p, dset_link %p\n"
            "order %d, indexed %d\n"
            "DimFact %f, global opacity %f, isBackGrnd (isBackground) %d.\n"
            "ForceIntRange %f, %f.\n"
            "SymIrange = %d\n", 
            Overlays[i]->Name, 
            Overlays[i], Overlays[i]->dset_link, 
            Overlays[i]->PlaneOrder, i, 
            Overlays[i]->DimFact, Overlays[i]->GlobalOpacity, 
                  Overlays[i]->isBackGrnd, 
            Overlays[i]->ForceIntRange[0], Overlays[i]->ForceIntRange[1], 
            Overlays[i]->SymIrange);
         SS = SUMA_StringAppend (SS,stmp);
         SS = SUMA_StringAppend_va (SS, "N_links = %d\n", Overlays[i]->N_links);
         SS = SUMA_StringAppend_va (SS, "LinkedPtrType = %d\n", 
                                    Overlays[i]->LinkedPtrType);
         SS = SUMA_StringAppend_va (SS, "owner_id = %s\n",  
                                    Overlays[i]->owner_id);
         NodeDef = COLP_NODEDEF(Overlays[i]);
         N_NodeDef = COLP_N_NODEDEF(Overlays[i]);
         N_Alloc = COLP_N_ALLOC(Overlays[i]);
         sprintf (stmp,"ShowMode=%d, N_Alloc=%d, N_NodeDef=%d\n", 
                        (int)Overlays[i]->ShowMode, N_Alloc, N_NodeDef);
         SS = SUMA_StringAppend (SS,stmp);
         if (detail > 1) {
            ShowN = N_NodeDef;
         } else { 
            if (N_NodeDef > 5) ShowN = 5;
            else ShowN = N_NodeDef;
         }
         SS = SUMA_StringAppend (SS,"\n");
         sprintf (stmp,"\tindex\tR\tG\tB\tLocOp\n");
         SS = SUMA_StringAppend (SS,stmp);
         for (j=0; j < ShowN; ++j) {
            SS = SUMA_StringAppend_va (SS, "\t%d\t%.3f\t%.3f\t%.3f\t%.3f\n", 
                     NodeDef[j], Overlays[i]->ColVec[3*j], 
                     Overlays[i]->ColVec[3*j+1], Overlays[i]->ColVec[3*j+2],
                     Overlays[i]->LocalOpacity[j]);
         }
         SS = SUMA_StringAppend (SS,"\n");
         
         if (!Overlays[i]->cmapname) 
            SS = SUMA_StringAppend (SS,"cmapname = NULL\n");
         else SS = SUMA_StringAppend_va (SS,"cmapname = %s\n", 
                                             Overlays[i]->cmapname);
         /* get the color map */
         if (!SUMAg_CF->scm) { /* try creating since it is no longer 
                                    created at initialization */
            static int try_once=0;
            if (!try_once) { 
               SUMAg_CF->scm = SUMA_Build_Color_maps(); ++ try_once; 
            }
         }
         if (Overlays[i]->Contours) {
            SS = SUMA_StringAppend_va (SS, "%d contours, pointer %p\n",
                                       Overlays[i]->N_Contours,
                                       Overlays[i]->Contours);
         }else {
            SS = SUMA_StringAppend_va (SS, "%d contours, NULL pointer\n",
                                       Overlays[i]->N_Contours,
                                       Overlays[i]->Contours);
         }
         if (SUMAg_CF->scm) {
            icmap = SUMA_Find_ColorMap (  Overlays[i]->cmapname, 
                                          SUMAg_CF->scm->CMv, 
                                          SUMAg_CF->scm->N_maps, -2 );
            if (icmap < 0) { SS = SUMA_StringAppend (SS,"cmap not found.\n"); }
            else {
               ColMap = SUMAg_CF->scm->CMv[icmap];
               s2 = SUMA_ColorMapVec_Info(&ColMap, 1, detail);
               SS = SUMA_StringAppend (SS, s2); SUMA_free(s2); s2 = NULL;
            }   
            s2 = SUMA_ScaleToMapOpt_Info (Overlays[i]->OptScl, 0);
            SS = SUMA_StringAppend (SS, s2); SUMA_free(s2); s2 = NULL;
         } else {
            SS = SUMA_StringAppend (SS,"\tNULL SUMA color maps.\n");
         }
         
         
      } else {
         SS = SUMA_StringAppend (SS,"\tNULL overlay plane.\n");
      }
   }
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN(s);
}

SUMA_Boolean SUMA_ShowScaleToMapOpt(SUMA_SCALE_TO_MAP_OPT *OptScl, FILE *Out, int detail)
{
   static char FuncName[]={"SUMA_ShowScaleToMapOpt"};
   char *s=NULL;
   
   SUMA_ENTRY;
   
   if (!Out) Out = stdout;
   
   s = SUMA_ScaleToMapOpt_Info(OptScl, detail);
   
   fprintf (Out, "%s\n", s);
   
   if (s) SUMA_free(s); s = NULL;
   
   SUMA_RETURN(YUP);
}

char *SUMA_ScaleToMapOpt_Info (SUMA_SCALE_TO_MAP_OPT *OptScl, int detail)
{
   static char FuncName[]={"SUMA_ScaleToMapOpt_Info"};
   char *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY; 
   SS = SUMA_StringAppend (NULL, NULL);
   
   if (!OptScl) { SS = SUMA_StringAppend (SS, "NULL ScaleToMap options\n"); }
   else {
      SS = SUMA_StringAppend (SS, "ScaleToMap options:\n");
      SS = SUMA_StringAppend_va (SS, "ApplyMask = %d\n", OptScl->ApplyMask);
      SS = SUMA_StringAppend_va (SS, "MaskRange = %f %f\n", 
         OptScl->MaskRange[0],  OptScl->MaskRange[1]);
      SS = SUMA_StringAppend_va (SS, "MaskColor = %f %f %f\n", 
         OptScl->MaskColor[0],  OptScl->MaskColor[1], OptScl->MaskColor[2]);
      SS = SUMA_StringAppend_va (SS, "ApplyClip = %d\n", OptScl->ApplyClip);
      SS = SUMA_StringAppend_va (SS, "BrightFact = %f\n", OptScl->BrightFact);
      SS = SUMA_StringAppend_va (SS, "MaskZero = %d\n", OptScl->MaskZero);
      SS = SUMA_StringAppend_va (SS, "find = %d\n", OptScl->find);
      SS = SUMA_StringAppend_va (SS, "IntRange = %f %f\n", 
         OptScl->IntRange[0], OptScl->IntRange[1]);
      SS = SUMA_StringAppend_va (SS, "AutoIntRange = %d\n", 
                                 OptScl->AutoIntRange);
      SS = SUMA_StringAppend_va (SS, "tind = %d (use:%d). Mode %d\n", 
                                 OptScl->tind, OptScl->UseThr, 
                                 OptScl->ThrMode);
      SS = SUMA_StringAppend_va (SS, "ThreshRange = %f %f\n", 
         OptScl->ThreshRange[0], OptScl->ThreshRange[1]);
      SS = SUMA_StringAppend_va (SS, "ThreshStats = %f %f\n", 
         OptScl->ThreshStats[0], OptScl->ThreshStats[1]);
      SS = SUMA_StringAppend_va (SS, "bind = %d (use:%d)\n", 
                                 OptScl->bind, OptScl->UseBrt);
      SS = SUMA_StringAppend_va (SS, "BrightRange = %f %f\n", 
         OptScl->BrightRange[0], OptScl->BrightRange[1]);
      SS = SUMA_StringAppend_va (SS, "BrightMap = %f %f\n", 
         OptScl->BrightMap[0], OptScl->BrightMap[1]);
      SS = SUMA_StringAppend_va (SS, "AutoBrtRange = %d\n", 
                                 OptScl->AutoBrtRange);
      SS = SUMA_StringAppend_va (SS, "alaAFNI = %d\n", OptScl->alaAFNI);
      SS = SUMA_StringAppend_va (SS, "interpmode = %d (%s)\n", 
                                 OptScl->interpmode, 
                                 SUMA_CmapModeName(OptScl->interpmode));
      SS = SUMA_StringAppend_va (SS, "ColsContMode = %d \n", 
                                 OptScl->ColsContMode);
      SS = SUMA_StringAppend_va (SS, "BiasMode = %d, Range=%f, %f \n", 
                                 OptScl->DoBias, OptScl->CoordBiasRange[0],
                                 OptScl->CoordBiasRange[1]);
      if (OptScl->BiasVect) 
         SS = SUMA_StringAppend_va (SS, "BiasVect is NOT NULL\n");
      else SS = SUMA_StringAppend_va (SS, "BiasVect is NULL\n");
   }        
   SUMA_SS2S(SS, s);
   SUMA_RETURN(s);
}

/*!
   \brief Frees SUMA_OVERLAY_LIST_DATUM * used in the linked list
*/ 
void SUMA_FreeOverlayListDatum (void *OLDv)
{
   static char FuncName[]={"SUMA_FreeOverlayListDatum"};
   SUMA_Boolean LocalHead = NOPE; 
   
   SUMA_ENTRY; 

   if (OLDv) SUMA_free(OLDv); 
   
   SUMA_RETURNe;
}

/*!
   \brief Create an ordered list of the colorplanes in Overlays
   the sorting of the list is done based on BackMod followed by the order
   The function makes sure colorplane orders span 0 to N_Overlays-1 
   
   \param SO (SUMA_SurfaceObject *)
   \param Opt (int) -1 for background plane list only
                     1 for foreground plane list only
                     0 for both background followed by foreground
   \return list (DList *) a doubly linked list of the ordered color planes.
         NULL is returned in case of error.
        to free this list when you no longer need it, do:   
         dlist_destroy(list); SUMA_free(list);
    
                  

*/
DList * SUMA_OverlaysToOrderedList (SUMA_SurfaceObject *SO, int Opt)
{
   static char FuncName[]={"SUMA_OverlaysToOrderedList"}; 
   DList *listop = NULL;
   DListElmt *Elmop=NULL;
   SUMA_OVERLAY_LIST_DATUM *OvD = NULL, *oOvD = NULL;
   int i, Shift, ShftPlaneOrder, oShftPlaneOrder;
   SUMA_OVERLAYS *oPlane=NULL;
   SUMA_Boolean Found, LocalHead = NOPE;  
   
   SUMA_ENTRY; 
   
   listop = (DList *)SUMA_calloc(1,sizeof(DList));
   
   dlist_init(listop, SUMA_FreeOverlayListDatum);
   SUMA_LH("Considering loop");
   for (i=0; i < SO->N_Overlays; ++i) {
      SUMA_LH("In Loop");
         OvD = (SUMA_OVERLAY_LIST_DATUM *)
                  SUMA_calloc(1,sizeof(SUMA_OVERLAY_LIST_DATUM));
         OvD->Overlay = SO->Overlays[i];
         if (!OvD->Overlay) {
            SUMA_LH("NULL Overlay");
         }
            SUMA_LH("Here");
         if (OvD->Overlay->isBackGrnd && Opt == 1) continue;   
            /* that was an unwanted background */
         if (!OvD->Overlay->isBackGrnd && Opt == -1) continue; 
            /* that was an unwanted foreground */
         if (!listop->size) {
            SUMA_LH("Very first");
            dlist_ins_next(listop, dlist_tail(listop), (void*)OvD);
         }else { /* must sort first */
            Elmop = NULL;
            do {
               SUMA_LH("Searching");
               Found = NOPE;
               if (!Elmop) {
                  Elmop = dlist_head(listop);
               } else {
                  Elmop = dlist_next(Elmop);
               }
               
               oOvD = (SUMA_OVERLAY_LIST_DATUM *)Elmop->data;
               
               /* transform PlaneOrder so that is 
                  reflects the Background modulation */
               Shift = SO->N_Overlays;
               
               if (OvD->Overlay->isBackGrnd) 
                  ShftPlaneOrder = OvD->Overlay->PlaneOrder - Shift;
               else ShftPlaneOrder = OvD->Overlay->PlaneOrder;
               
               if (oOvD->Overlay->isBackGrnd) 
                  oShftPlaneOrder = oOvD->Overlay->PlaneOrder - Shift;
               else oShftPlaneOrder = oOvD->Overlay->PlaneOrder; 
               
               if (ShftPlaneOrder <= oShftPlaneOrder) {
                  SUMA_LH ("Ins Prev");
                  dlist_ins_prev(listop, Elmop, (void *)OvD);
                  Found = YUP;
               } else if (Elmop == dlist_tail(listop)) {
                  SUMA_LH ("Ins Next");
                  /* reached the end, append */
                  dlist_ins_next(listop, Elmop, (void *)OvD);
                  Found = YUP;
               }
            } while (!Found);
         }
   }
    

   /* Now the list is sorted  
   Go through the planes and make sure that the orders 
   span 0 to N_Overlays -1 */
   SUMA_LH("Changing list order to plane order");
   SUMA_ListOrderToPlaneOrder (listop);
   
   SUMA_RETURN(listop);
}

/*!
   \brief sets the values of PlaneOrder to reflect the location of the color planes in list
*/
SUMA_Boolean SUMA_ListOrderToPlaneOrder (DList *listop) 
{
   static char FuncName[]={"SUMA_ListOrderToPlaneOrder"};
   SUMA_OVERLAY_LIST_DATUM *OvD = NULL;
   int i, fg_shift = 0;
   DListElmt *Elmop=NULL;

   SUMA_ENTRY;

   /* First pass, do background */
   if (listop->size) {
      Elmop = NULL;
      i = 0;
      do {
         if (!Elmop) Elmop = dlist_head(listop);
         else Elmop = Elmop->next;
         OvD = (SUMA_OVERLAY_LIST_DATUM *)Elmop->data;
         if (OvD->Overlay->isBackGrnd) {
            OvD->Overlay->PlaneOrder = i;
            ++i;
         }
      } while (!dlist_is_tail(Elmop));
   }
   
   /* second pass, do foreground */
   if (listop->size) {
      Elmop = NULL;
      i = 0;
      do {
         if (!Elmop) Elmop = dlist_head(listop);
         else Elmop = Elmop->next;
         OvD = (SUMA_OVERLAY_LIST_DATUM *)Elmop->data;
         if (!OvD->Overlay->isBackGrnd) {
            OvD->Overlay->PlaneOrder = i;
            ++i;
         }
      } while (!dlist_is_tail(Elmop));
   }
   

   SUMA_RETURN(YUP);
}

/*!
   \brief returns the largest background order in the list of verlay planes
*/
int SUMA_GetLargestBackroundOrder (DList *listop)
{
   static char FuncName[]={"SUMA_GetLargestBackroundOrder"};
   int Order, i;
   DListElmt *Elmop=NULL;
   SUMA_OVERLAY_LIST_DATUM *OvD = NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   Order = 0;
   Elmop = NULL;
   do {
      if (!Elmop) Elmop = dlist_head(listop);
      else Elmop = Elmop->next;
      OvD = (SUMA_OVERLAY_LIST_DATUM *)Elmop->data;
      if (OvD->Overlay->isBackGrnd) {
         if (OvD->Overlay->PlaneOrder > Order) Order = OvD->Overlay->PlaneOrder;
      }
      ++i;
   } while (!dlist_is_tail(Elmop));
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: Highest background order found is %d\n", FuncName, Order);
   }
   
   SUMA_RETURN(Order);
}

/*!
   \brief returns the lowest foreground plane order in the list of overlay planes
*/
int SUMA_GetSmallestForegroundOrder (DList *listop)
{
   static char FuncName[]={"SUMA_GetSmallestForegroundOrder"};
   int Order, i;
   DListElmt *Elmop=NULL;
   SUMA_OVERLAY_LIST_DATUM *OvD = NULL, *oOvD = NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   Order = listop->size -1 ;
   Elmop = NULL;
   do {
      if (!Elmop) Elmop = dlist_head(listop);
      else Elmop = Elmop->next;
      OvD = (SUMA_OVERLAY_LIST_DATUM *)Elmop->data;
      if (!OvD->Overlay->isBackGrnd) {
         if (OvD->Overlay->PlaneOrder < Order) Order = OvD->Overlay->PlaneOrder;
      }
      ++i;
   } while (!dlist_is_tail(Elmop));
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: Lowest foreground order found is %d\n", FuncName, Order);
   }
   
   SUMA_RETURN(Order);
}

/*!
   Is a color overlay plane registered in SO
*/
SUMA_Boolean SUMA_isOverlayOfSO (SUMA_SurfaceObject *SO, SUMA_OVERLAYS *Plane)
{
   static char FuncName[]={"SUMA_isOverlayOfSO"};
   int i;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY; 

   for (i=0; i< SO->N_Overlays; ++i) if (SO->Overlays[i] == Plane) { SUMA_LHv("Found plane at ind: %d\n", i); SUMA_RETURN(YUP); }
   
   SUMA_LH("Plane not found");
   SUMA_RETURN(NOPE);
}

void SUMA_Print_PlaneOrder (SUMA_SurfaceObject *SO, FILE *Out)
{   
   static char FuncName[]={"SUMA_Print_PlaneOrder"};
   char *s;
   
   SUMA_ENTRY;

   if (Out == NULL) Out = stdout;
      
   s =  SUMA_PlaneOrder_Info(SO);
   
   if (s) {
      fprintf (Out, "%s", s);
      SUMA_free(s);
   }else {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_PlaneOrder_Info.\n", FuncName);
   }   
   
   SUMA_RETURNe;
}   

/*!
   \brief Shows the overlay plane order 
*/
char * SUMA_PlaneOrder_Info (SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_PlaneOrder_Info"};
   char stmp[1000], *s = NULL;
   SUMA_STRING *SS = NULL;
   DList *list=NULL;
   DListElmt *Elm=NULL;
   SUMA_OVERLAY_LIST_DATUM *OvD=NULL;
   
   SUMA_ENTRY;

   /* get the background and foreground lists */
   SS = SUMA_StringAppend (NULL, NULL);
      
   if (!(list = SUMA_OverlaysToOrderedList (SO, -1))) {
      SS = SUMA_StringAppend (SS,"NULL Background list\n");
   }else if (!list->size) {
      SS = SUMA_StringAppend (SS,"Empty Background list\n");
   } else {
      Elm=NULL;
      do {
         if (!Elm) Elm = dlist_head(list);
         else Elm = Elm->next;
         OvD = (SUMA_OVERLAY_LIST_DATUM *)Elm->data;
         sprintf (stmp,"BK: %s o%d (%s)\n", OvD->Overlay->Label, OvD->Overlay->PlaneOrder, OvD->Overlay->Name );
         SS = SUMA_StringAppend (SS,stmp);
      } while (Elm != dlist_tail(list));
   }
   
   if (!(list = SUMA_OverlaysToOrderedList (SO, 1))) {
      SS = SUMA_StringAppend (SS,"NULL Foreground list\n");
   }else if (!list->size) {
      SS = SUMA_StringAppend (SS,"Empty Foreground list\n");
   } else {
      Elm=NULL;
      do {
         if (!Elm) Elm = dlist_head(list);
         else Elm = Elm->next;
         OvD = (SUMA_OVERLAY_LIST_DATUM *)Elm->data;
         sprintf (stmp,"FG: %s o%d (%s)\n", OvD->Overlay->Label, OvD->Overlay->PlaneOrder, OvD->Overlay->Name );
         SS = SUMA_StringAppend (SS,stmp);
      } while (Elm != dlist_tail(list));
   }
   
   s = SS->s;
   SUMA_free(SS);
   
   SUMA_RETURN (s);
}

/*!
   \brief Moves a plane up one order
*/
SUMA_Boolean SUMA_MovePlaneUp (SUMA_SurfaceObject *SO, char *Name)
{
   static char FuncName[]={"SUMA_MovePlaneUp"};
   SUMA_OVERLAYS *Overlay=NULL;
   SUMA_OVERLAY_LIST_DATUM *OvD=NULL;
   DList *list=NULL;
   DListElmt *Elm = NULL;
   int junk=0;
   SUMA_Boolean Found = NOPE, LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   /* search for the plane by name */
   SUMA_LH("Searching for plane");
   if (!(Overlay = SUMA_Fetch_OverlayPointer(SO->Overlays, SO->N_Overlays, Name, &junk))) {
      SUMA_S_Err("Plane does not exist in SO->Overlays. (identified by name)");
      SUMA_RETURN (NOPE);
   }
   
   /* get the list of planes */
   SUMA_LH("Creating list");
   if (Overlay->isBackGrnd) list = SUMA_OverlaysToOrderedList (SO, -1);
   else list = SUMA_OverlaysToOrderedList (SO, 1);
   if (!list) {
      SUMA_S_Err("NULL list");
      SUMA_RETURN (NOPE);
   }
   
   /* Now search through the list until you find Overlay */
   SUMA_LH("Searching for plane in list");
   Found = NOPE;
   Elm = NULL;
   do {
      if (!Elm) Elm = dlist_head(list);
      else Elm = Elm->next;
      OvD = (SUMA_OVERLAY_LIST_DATUM *) Elm->data;
      if (OvD->Overlay == Overlay) Found = YUP;
   }while (Elm != dlist_tail(list) && !Found);
   
   if (!Found) {
      SUMA_S_Err("Strange, real strange.");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_LH("Found element, inserting at new position");
   if (Elm != dlist_tail(list)) { /* not on top, can move up */
      /* add Elm's data  ahead of Elm->next */
      dlist_ins_next (list, Elm->next, Elm->data);
      /* remove Elm BUT NOT ITS DATA STRUCTURE!*/
      dlist_remove (list, Elm, (void *)(&OvD));
   } else {
      SUMA_LH("Reached the top");
   }
   
   SUMA_LH("Compacting");
   /* now compact the order just for good measure */
   SUMA_ListOrderToPlaneOrder (list);

   
   SUMA_LH("Clean up");
   dlist_destroy(list); SUMA_free(list);
   SUMA_RETURN(YUP);   
}

/*!
   \brief Moves a plane up one order
*/
SUMA_Boolean SUMA_MovePlaneDown (SUMA_SurfaceObject *SO, char *Name)
{
   static char FuncName[]={"SUMA_MovePlaneDown"};
   SUMA_OVERLAYS *Overlay=NULL;
   SUMA_OVERLAY_LIST_DATUM *OvD=NULL;
   DList *list=NULL;
   DListElmt *Elm = NULL;
   int junk=0;
   SUMA_Boolean Found = NOPE, LocalHead = NOPE;
   
   SUMA_ENTRY; 
   
   /* search for the plane by name */
   SUMA_LH("Searching for plane");
   if (!(Overlay = SUMA_Fetch_OverlayPointer(SO->Overlays, SO->N_Overlays, Name, &junk))) {
      SUMA_S_Err("Plane does not exist in SO->Overlays. (identified by name)");
      SUMA_RETURN (NOPE);
   }
   
   /* get the list of planes */
   SUMA_LH("Creating list");
   if (Overlay->isBackGrnd) list = SUMA_OverlaysToOrderedList (SO, -1);
   else list = SUMA_OverlaysToOrderedList (SO, 1);
   if (!list) {
      SUMA_S_Err("NULL list");
      SUMA_RETURN (NOPE);
   }
   
   /* Now search through the list until you find Overlay */
   SUMA_LH("Searching for plane in list");
   Found = NOPE;
   Elm = NULL;
   do {
      if (!Elm) Elm = dlist_head(list);
      else Elm = Elm->next;
      OvD = (SUMA_OVERLAY_LIST_DATUM *) Elm->data;
      if (OvD->Overlay == Overlay) Found = YUP;
   }while (Elm != dlist_tail(list) && !Found);
   
   if (!Found) {
      SUMA_S_Err("Strange, real strange.");
      SUMA_RETURN(NOPE);
   }
   
   SUMA_LH("Found element, inserting at new position");
   if (Elm != dlist_head(list)) { /* not on bottom, can move down */
      /* add Elm's data  before of Elm->prev */
      dlist_ins_prev (list, Elm->prev, Elm->data);
      /* remove Elm BUT NOT ITS DATA STRUCTURE!*/
      dlist_remove (list, Elm, (void *)(&OvD));
   } else {
      SUMA_LH("Reached the bottom");
   }
   
   SUMA_LH("Compacting");
   /* now compact the order just for good measure */
   SUMA_ListOrderToPlaneOrder (list);

   
   SUMA_LH("Clean up");
   dlist_destroy(list); SUMA_free(list);
   SUMA_RETURN(YUP);   
}


SUMA_OVERLAYS * SUMA_NewPlaneSearch(SUMA_SurfaceObject *SO, SUMA_OVERLAYS *Overlay)
{
   static char FuncName[]={"SUMA_NewPlaneSearch"};
   int junk = 0;
   
   SUMA_ENTRY;
   
   if (!Overlay || !SO) {
      SUMA_S_Err("You sent me NULLS!");
      SUMA_RETURN (NULL);
   }
   
   if (SUMA_isOverlayOfSO(SO, Overlay)) {
      SUMA_RETURN(Overlay);
   }
   
   /* also try looking for plane by name */
   SUMA_RETURN(SUMA_Fetch_OverlayPointer( SO->Overlays, SO->N_Overlays, 
                                          Overlay->Name, &junk));
}

/*!
   \brief Adds a new plane to SO->Overlays. 
   If plane exists, you get an error message
   Adds plane to related surfaces if dov is not NULL
   
   DuplicateFlag == 0 return with error if plane already exists
                    1 return with warning if plane already exists
*/
SUMA_Boolean SUMA_AddNewPlane (SUMA_SurfaceObject *SO, SUMA_OVERLAYS *Overlay, 
                                 SUMA_DO *dov, int N_dov, int DuplicateFlag)
{
   static char FuncName[]={"SUMA_AddNewPlane"};
   DList *ForeList=NULL, *BackList = NULL;
   SUMA_OVERLAY_LIST_DATUM *OvD=NULL;
   int junk=0, i, OverInd, kin = 1;
   SUMA_SurfaceObject *SO2 = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY_LH; 
   
   if (!Overlay) {
      SUMA_S_Err("You sent me NULLS!");
      SUMA_RETURN (NOPE);
   }
   
   if (SUMA_NewPlaneSearch(SO, Overlay)) {
      if (DuplicateFlag == 0) {
         SUMA_S_Errv(
            "Plane exists in SO->Overlays either \n"
            "by pointer %p or by name (%s).\n"
            "Both of these must be unique because DuplicateFlag=%d\n", 
            Overlay, Overlay->Name, DuplicateFlag);
         SUMA_DUMP_TRACE("Allora");
         SUMA_RETURN (NOPE);
      } else {
         SUMA_S_Note("Plane exists in SO->Overlays. Preserving old one.");
         SUMA_RETURN (YUP);
      }
   }
   
   /* make sure that overlay plane does not have bias in it */
   if (Overlay->OptScl) {
      if (Overlay->OptScl->BiasVect) {
         SUMA_SL_Err("New overlay plane cannot have coordinate bias.\n"
                     "Not yet at least.\n");
         /* If you want to support this feature, 
            you'll have to call SUMA_ADD_COORD_BIAS_VECT
            on any surface the plane gets assigned to. 
            That means SO and SO2 below.
            Search for macro SUMA_ADD_COORD_BIAS_VECT in 
            SUMA_SwitchState in file SUMA_Engine.c
            for the example */
         SUMA_RETURN(NOPE);
      }
   }
   /* make sure there's enough room for the new plane */
   if (SO->N_Overlays+1 >= SUMA_MAX_OVERLAYS) {
      SUMA_SL_Crit("Too many color overlays.");
      SUMA_RETURN (NOPE);
   }
   
   /* Now add the plane where it belongs */
   if (!(ForeList = SUMA_OverlaysToOrderedList (SO, 1))) {
      SUMA_S_Err("NULL ForeList");
      SUMA_RETURN (NOPE);
   }
   if (!(BackList = SUMA_OverlaysToOrderedList (SO, -1))) {
      SUMA_S_Err("NULL BackList");
      SUMA_RETURN (NOPE);
   }
   
   SUMA_LH("Adding to list...");
   OvD = (SUMA_OVERLAY_LIST_DATUM *)
            SUMA_calloc(1,sizeof(SUMA_OVERLAY_LIST_DATUM));
   OvD->Overlay = Overlay;
   
   if (Overlay->isBackGrnd) {
      SUMA_LH("Back dude...");
      dlist_ins_next(BackList, dlist_tail(BackList), (void *)OvD);
      Overlay->PlaneOrder = BackList->size - 1;
   } else {
      SUMA_LH("Front dude...");
      dlist_ins_next(ForeList, dlist_tail(ForeList), (void *)OvD);
      Overlay->PlaneOrder = ForeList->size - 1;
   }
   
   SUMA_LH("Out dude...");
   /* place the Overlay plane and its inode in SO */
   SO->Overlays[SO->N_Overlays] = Overlay;
   /* Now increment the number of overlays to be in SO */
   ++SO->N_Overlays;
   
   
   SUMA_LH("Destruction...");
   dlist_destroy(ForeList); SUMA_free(ForeList);
   dlist_destroy(BackList); SUMA_free(BackList);

   /* Now register plane with surfaces deserving it*/
   if (AFNI_yesenv("SUMA_ShareGrandChildrenOverlays")) {
      SUMA_S_Warn("Option needs polishing before\n"
                  "public usage. Nothing is done\n"
                  "to reflect changes to overlay coloring\n"
                  "from one group of LocalDom. to another\n"
                  "that share the same DomainGrandParentID\n");
      kin = 2;
   } else kin = 1; 
   if (dov) {
      SUMA_LH("Registering plane with surfaces deserving it");
      /* Now that you have the color overlay plane set, 
         go about all the surfaces, searching for ones related to SO 
         and make sure they have this colorplane, 
         otherwise, create a link to it. */   
      for (i=0; i < N_dov; ++i) {
         if (SUMA_isSO(dov[i])) { 
            SO2 = (SUMA_SurfaceObject *)dov[i].OP;
            if (SUMA_isRelated(SO, SO2, kin) && SO != SO2) { 
               /* only 1st order kinship allowed */
               /* surfaces related and not identical, check on colorplanes */
               if (!SUMA_Fetch_OverlayPointer ( SO2->Overlays, SO2->N_Overlays, 
                                                Overlay->Name, &OverInd)) {
                  /* color plane not found, link to that of SO */
                  SO2->Overlays[SO2->N_Overlays] = 
                     (SUMA_OVERLAYS*)SUMA_LinkToPointer(
                                       (void*)SO->Overlays[SO->N_Overlays-1]);
                  /*increment the number of overlay planes */
                  ++SO2->N_Overlays;
               } else {
                  /* colorplane found OK */
               }
            }
         }
      }
   }
   SUMA_RETURN (YUP);
}

/*!
   \brief ans = SUMA_MixColors (sv);
   this functions mixes the colors for surface objects that ask for it 
   \param sv (SUMA_SurfaceViewer *)
   \return YUP/NOPE
*/
SUMA_Boolean SUMA_MixColors (SUMA_SurfaceViewer *sv) 
{
   static char FuncName[]={"SUMA_MixColors"};
   int i, dov_id;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   for (i=0; i<sv->N_ColList; ++i) {
      if (sv->ColList[i].Remix) {
         if (LocalHead) 
            fprintf( SUMA_STDERR, 
                     "%s: Mixing colors (%s)...\n", 
                     FuncName, sv->ColList[i].idcode_str);
         dov_id = SUMA_findSO_inDOv (  sv->ColList[i].idcode_str, 
                                       SUMAg_DOv, SUMAg_N_DOv);
         if (dov_id < 0) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_findSO_inDOv.\n", FuncName);
            SUMA_RETURN(NOPE);
         }
         SO = (SUMA_SurfaceObject *)SUMAg_DOv[dov_id].OP;
         if (!SUMA_Overlays_2_GLCOLAR4(SO, sv, sv->ColList[i].glar_ColorList)) {
            fprintf (SUMA_STDERR,
                     "Error %s: Failed in SUMA_Overlays_2_GLCOLAR4.\n", 
                     FuncName);
            SUMA_RETURN(NOPE);
         }
         sv->ColList[i].Remix = NOPE;
      }
   }   
   
   SUMA_RETURN (YUP);

}

/*!
   \brief A function that looks up an overlay plane by its name.
   If the plane is found its index is returned, otherwise
   a new one is created. The function also sets up pointers to 
   that plane for all related surfaces in dov and places node colors
   in that plane.
   
   \param SO (SUMA_SurfaceObject *) Surface Object
   \param Name (char *) Name of color plane
   \param sopd (SUMA_OVERLAY_PLANE_DATA *) Data to put in overlay plane 
   \param PlaneInd (int *) index of created or found color plane
                        (that's set by the function )
   \param dov (SUMA_DO *) vector of displayable objects
   \param N_dov (int ) number of displayable objects 
   
*/ 
SUMA_Boolean SUMA_iRGB_to_OverlayPointer (SUMA_SurfaceObject *SO, 
                                 char *Name, SUMA_OVERLAY_PLANE_DATA *sopd, 
                                 int *PlaneInd, SUMA_DO *dov, int N_dov,
                                 DList *DsetList) 
{
   static char FuncName[]={"SUMA_iRGB_to_OverlayPointer"}, stmp[500];
   char *DsetName_tmp=NULL;
   int i, OverInd = -1, i_max, wrn_cnt = 0, i3 = 0, 
         N_NodeDef = -1, *NodeDef = NULL;
   SUMA_SurfaceObject *SO2 = NULL;
   SUMA_OVERLAYS *Overlay=NULL;
   SUMA_DSET *dset = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

      SUMA_LH("Fetching Overlay Pointer");
      /* if plane exists use it, else create a new one on the mappable surface */
      if (!SUMA_Fetch_OverlayPointer (SO->Overlays, SO->N_Overlays, 
                                       Name, &OverInd)) {
         SUMA_LH("pointer not found");
         /* overlay plane not found, create a new one on the mappable surface*/
         if (!SUMA_isLocalDomainParent(SO)) {
            if (sopd->Source == SES_Afni) {
               /* unexpected, surfaces coming from AFNI with a map 
               should be a local domain parent */
               fprintf( SUMA_STDERR,
                        "Error %s: Surface %s (ID: %s) received from AFNI is "
                        "not a local domain parent.\n", 
                        FuncName, SO->Label, SO->idcode_str);
               SUMA_RETURN(NOPE);
            } else {
               SUMA_SL_Warn ( "Placing colors on surface \n"
                              "not a local domain parent.\nCase not tested.");
            }
         } 
         
         DsetName_tmp = SUMA_append_string(Name, SO->LocalDomainParentID);
         dset = SUMA_CreateDsetPointer (DsetName_tmp, 
                                        SUMA_NODE_RGB,
                                        NULL,
                                        SO->idcode_str,
                                        SO->N_Node);   /* first create a dataset 
                                          that will go with that colorplane */   
         SUMA_free(DsetName_tmp); DsetName_tmp = NULL;
         /* insert that element into DaList */
         if (!SUMA_InsertDsetPointer(&dset, DsetList, 0)) {
            SUMA_SL_Err("Failed to insert dset into list");
            SUMA_RETURN(NOPE);
         }
         /* We'll be using NodeDef here so begin by allocating space 
            for the various entries */
         SUMA_AddDsetNelCol (dset, "node index", SUMA_NODE_INDEX, NULL, NULL, 1);
         SUMA_AddDsetNelCol (dset, "red", SUMA_NODE_R, NULL, NULL, 1);
         SUMA_AddDsetNelCol (dset, "green", SUMA_NODE_G, NULL, NULL, 1);
         SUMA_AddDsetNelCol (dset, "blue", SUMA_NODE_B, NULL, NULL, 1);

         Overlay = SUMA_CreateOverlayPointer (Name, dset, SO->idcode_str, NULL);
         if (!Overlay) {
            SUMA_SL_Err("Failed in SUMA_CreateOverlayPointer.\n");
            SUMA_RETURN(NOPE);
         } 
         
         /* set up some defaults for the overlap plane */
         if (sopd->Show) Overlay->ShowMode = SW_SurfCont_DsetViewCol;
         else Overlay->ShowMode = -SW_SurfCont_DsetViewCol;
         Overlay->GlobalOpacity = sopd->GlobalOpacity;
         Overlay->isBackGrnd = sopd->isBackGrnd;
         Overlay->OptScl->BrightFact = sopd->DimFact;
         OverInd = SO->N_Overlays; 

         /* Add this plane to SO->Overlays */
         if (!SUMA_AddNewPlane (SO, Overlay, dov, N_dov, 0)) {
            SUMA_SL_Crit("Failed in SUMA_AddNewPlane");
            SUMA_FreeOverlayPointer(Overlay);
            SUMA_RETURN (NOPE);
         }

         
      }else {
         SUMA_LH("Pointer found");
      }
      
      if (LocalHead) 
         fprintf (SUMA_STDERR, 
                  "%s: OverInd = %d, Loading colors to Overlay Plane...\n", 
                  FuncName, OverInd);
      if (sopd->N > SO->N_Node) {
         sprintf(stmp,"Number of nodes in colorplane (%d)\n" \
                      "is larger than number of nodes in surface (%d)\n" \
                      "Proceed if you know what you're doing.\n" \
                      "Data from high node indices will be ignored.",  
                  sopd->N, SO->N_Node);
         SUMA_SLP_Warn(stmp);
         i_max = SO->N_Node;
      } else {
         i_max = sopd->N;
      }
      
      SO->Overlays[OverInd]->DimFact = sopd->DimFact;
      SO->Overlays[OverInd]->cmapname = SUMA_copy_string("explicit"); 
                                 /* explict color definition in data */
      SO->Overlays[OverInd]->FullList = 0; /*!< This type of planes is not 
               usually a full list because it has the nodes defined*/
      
      /* Now put the colors in the overlay plane */
      if (LocalHead) fprintf (SUMA_STDERR,
                              "%s: %d node colors are to be inserted.\n",
                              FuncName, i_max);
                                    
      COLP_N_NODEDEF(SO->Overlays[OverInd]) = i_max; /* set the number of nodes 
                                       filled IN THE OVERLAY PLANE*/
      SDSET_VECFILLED(SO->Overlays[OverInd]->dset_link) = i_max; /* set the 
                     number of nodes filled IN THE DSET, For this
                     type of dsets, the N_NodeDef is the same for 
                     both OVERLAY and for DSET*/
      if (COLP_N_NODEDEF(SO->Overlays[OverInd])) {
         int *iv, N_i,*Nv; 
         float *Rv, *Gv, *Bv; 
         SUMA_DSET *dset;
         dset = SO->Overlays[OverInd]->dset_link;
         /* find the columns you need to fill. 
          You can't use SUMA_FillNelCol directly because
          columns (vectors) are of different types */
         if (!(Nv = SUMA_GetNodeDef(dset))) { 
            SUMA_SL_Err("Failed to find node indices."); SUMA_RETURN(NOPE); }
         if (LocalHead) SUMA_ShowDset(dset, 0, NULL);
         iv = SUMA_GetDsetColIndex (dset, SUMA_NODE_R, &N_i);
         if (N_i != 1) { 
            SUMA_SL_Err("Failed to find one column."); 
            SUMA_free(iv); SUMA_RETURN(NOPE); }
         Rv = (float *)dset->dnel->vec[iv[0]];SUMA_free(iv); iv = NULL; 
         iv = SUMA_GetDsetColIndex (dset, SUMA_NODE_G, &N_i);
         if (N_i != 1) { 
            SUMA_SL_Err("Failed to find one column."); 
            SUMA_free(iv); SUMA_RETURN(NOPE); }
         Gv = (float *)dset->dnel->vec[iv[0]];SUMA_free(iv); iv = NULL; 
         iv = SUMA_GetDsetColIndex (dset, SUMA_NODE_B, &N_i);
         if (N_i != 1) { 
            SUMA_SL_Err("Failed to find one column."); 
            SUMA_free(iv); SUMA_RETURN(NOPE); }
         Bv = (float *)dset->dnel->vec[iv[0]];SUMA_free(iv); iv = NULL; 
         /* Now store these colors into the dataset */
         switch (sopd->Type) {
            case SOPT_ibbb:
               {
                  int *inel=NULL;
                  byte *r=NULL, *g=NULL, *b=NULL, *a=NULL;
                  
                  inel = (int *)sopd->i;
                  r = (byte *)sopd->r;
                  g = (byte *)sopd->g;
                  b = (byte *)sopd->b;
                  for (i=0; i < i_max; ++i) {
                     /*fprintf(SUMA_STDERR,
                              "Node %d: r%d, g%d, b%d\n", 
                              inel[i], r[i], g[i], b[i]);*/
                     if (SO->N_Node > inel[i]) {
                        Nv[i] = inel[i];
                        Rv[i] = (float)(r[i]) / 255.0;
                        Gv[i] = (float)(g[i]) / 255.0;
                        Bv[i] = (float)(b[i]) / 255.0;
                     } else {
                        if (!wrn_cnt) {
                           sprintf(stmp, 
                           "Color plane includes node indices (%d at %d)\n"   
                           "that are >= number of nodes in surface (%d).\n"
                           "Other similar warnings will be muted.", 
                           inel[i], i, SO->N_Node); 
                           SUMA_SLP_Warn(stmp);
                        }
                        ++wrn_cnt;  
                     }
                  }
               }
               break;
            case SOPT_ifff:
               {
                  int *inel=NULL;
                  float *r=NULL, *g=NULL, *b=NULL, *a=NULL;
                  
                  inel = (int *)sopd->i;
                  r = (float *)sopd->r;
                  g = (float *)sopd->g;
                  b = (float *)sopd->b;
                  for (i=0; i < i_max; ++i) {
                     /*fprintf(SUMA_STDERR,
                        "Node %d: r%f, g%f, b%f\n", inel[i], r[i], g[i], b[i]);*/
                     if (SO->N_Node > inel[i]) {
                        Nv[i] = inel[i];
                        Rv[i] = (float)(r[i]) ;
                        Gv[i] = (float)(g[i]) ;
                        Bv[i] = (float)(b[i]) ;
                     } else {
                        if (!wrn_cnt) {
                           SUMA_SLP_Warn("Color plane includes node indices\n"   
                                   "that are >= number of nodes in surface.\n");
                        }
                        ++wrn_cnt;  
                     }
                  }
               }
               break;
            default:
               SUMA_SLP_Err("Unknown color plane type");
               SUMA_RETURN(NOPE);
               break;
         }
      }
      
      /* Now you want to create the colors of that plane based on 
         the data in dset */
      if (!SUMA_ColorizePlane (SO->Overlays[OverInd])) {
         SUMA_SLP_Err("Failed to colorize plane.\n");
         SUMA_RETURN(NOPE);
      }
      
      /* store overlay plane index here, OverInd will get mango-ed 
         further down */
      if (LocalHead) 
         fprintf (SUMA_STDERR, 
                  "%s: OverInd = %d. Returning.\n", FuncName, OverInd);
      *PlaneInd = OverInd;

   SUMA_RETURN (YUP);

}

SUMA_DRAWN_ROI * SUMA_is_NamedColPlane_ForROI(char *PlaneName) 
{
   static char FuncName[]={"SUMA_is_NamedColPlane_ForROI"};
   int i;
   SUMA_DRAWN_ROI *D_ROI=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!PlaneName) SUMA_RETURN(NULL);
   
   /* search all dov for ROIs that use the same plane */
   for (i=0; i < SUMAg_N_DOv && !D_ROI; ++i) {
      switch (SUMAg_DOv[i].ObjectType) { /* case Object Type */
         case ROIdO_type:
            if ((D_ROI = (SUMA_DRAWN_ROI *)SUMAg_DOv[i].OP)) {
               if (  D_ROI->ColPlaneName &&
                     !strcmp(D_ROI->ColPlaneName, PlaneName) ) {
                  SUMA_RETURN(D_ROI);
               } else {
                  D_ROI=NULL;
               }
            }
            break;
         default:
            D_ROI = NULL;
            break;
      }
   }
   SUMA_RETURN(NULL);   
}

/*!
   \brief SUMA_FlushPlaneNotInUse (char *PlaneName, SUMA_SurfaceObject *SO, SUMA_DO *dov, int N_dov)
   Searches all DrawnROIs in dov. 
   If no ROI related to SO has PlaneName for a color plane
   then that colorplane is flushed (ie no node colors are left in it, not deleted)
*/
SUMA_Boolean SUMA_FlushPlaneNotInUse (char *PlaneName, SUMA_SurfaceObject *SO, SUMA_DO *dov, int N_dov)
{
   static char FuncName[]={"SUMA_FlushPlaneNotInUse"};
   SUMA_DRAWN_ROI *D_ROI = NULL;
   int i, OverInd;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!PlaneName) SUMA_RETURN(YUP);
   
   /* search all dov for ROIs that use the same plane */
   for (i=0; i < N_dov; ++i) {
      switch (dov[i].ObjectType) { /* case Object Type */
         case ROIdO_type:
            D_ROI = (SUMA_DRAWN_ROI *)dov[i].OP;
            break;
         default:
            D_ROI = NULL;
            break;
      }
      if (D_ROI && SUMA_isdROIrelated (D_ROI, SO)) {
         if (strcmp(PlaneName, D_ROI->ColPlaneName) == 0) {
            /* found one, do nothing and return */
            SUMA_RETURN (YUP);
         }
      }
   }   
   
   /* looks like no other ROIs use that plane, flush it */
   if (!SUMA_Fetch_OverlayPointer (SO->Overlays, SO->N_Overlays, PlaneName, &OverInd)) {
      SUMA_SLP_Warn("No Overlay Plane Found!");
      SUMA_RETURN (YUP);
   }
   
   SUMA_LH("Flushing meadows");
   #if 0
      SO->Overlays[OverInd]->N_NodeDef = 0; /* Flushed */
   #else
      COLP_N_NODEDEF(SO->Overlays[OverInd]) = 0;
   #endif
   SUMA_RETURN (YUP);
}

/*!
   \brief refreshes a colorplane list. 
   A combo of SUMA_AssembleColorPlaneList and SUMA_CreateScrolledList.
   
*/
void SUMA_RefreshDsetList (SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_RefreshDsetList"};
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   LW = SO->SurfCont->SwitchDsetlst;
   
   if (!LW) SUMA_RETURNe;
   
   if (LW->ALS) {
      /* free that old hag */
      if (LocalHead) SUMA_S_Err("Freeing the hag.");
      LW->ALS = SUMA_FreeAssembleListStruct(LW->ALS);
   }
   
   
   /* assemble the ColorPlane list */
   LW->ALS = SUMA_AssembleColorPlaneList (SO);
  
   if (!LW->ALS) {
      SUMA_SLP_Err("Error assembling list.");
      SUMA_RETURNe;
   }
   
   if (LW->ALS->N_clist < 0) {
      SUMA_SL_Err("Failed in SUMA_AssembleColorPlaneList");
      SUMA_RETURNe;
   }
   
   if (!LW->ALS->N_clist) {
      SUMA_SLP_Note ("No Color planes to choose from.");
      SUMA_RETURNe;
   }
   
   if (LocalHead) {
      int i;
      for (i=0; i < LW->ALS->N_clist; ++i) 
         fprintf (SUMA_STDERR,"%s: %s\n", FuncName, LW->ALS->clist[i]);
   }
   SUMA_CreateScrolledList ( LW->ALS->clist, LW->ALS->N_clist, NOPE,
                             LW);
   
   SUMA_RETURNe;
}  

/*!
   \brief Returns a list of the Colorplanes belonging to a certain surface. 
   
   \param SO (SUMA_SurfaceObject *) pointer to surface object
   
   \return clist (SUMA_ASSEMBLE_LIST_STRUCT *) pointer to structure containing results
   
   \sa SUMA_FreeAssembleListStruct
   \sa SUMA_CreateAssembleListStruct
   
*/
SUMA_ASSEMBLE_LIST_STRUCT * SUMA_AssembleColorPlaneList (SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_AssembleColorPlaneList"};
   int i=-1, N_clist=-1; 
   DList *list=NULL, *listop = NULL, *OverlayPlanelist = NULL;
   DListElmt *Elm = NULL, *Elmop = NULL, *Elm_OverlayPlanelist = NULL;
   char Label[SUMA_MAX_NAME_LENGTH], *store=NULL;
   char **clist=NULL;
   void **oplist=NULL;
   SUMA_ASSEMBLE_LIST_STRUCT *clist_str = NULL;
   SUMA_OVERLAY_LIST_DATUM *OvD=NULL, *oOvD=NULL;
   SUMA_Boolean SortByOrder = YUP;
   SUMA_Boolean Found = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   /* get list of all Overlay planes */
   OverlayPlanelist = SUMA_OverlaysToOrderedList (SO, 0);

   /* need a list to store new names */
   list = (DList *)SUMA_calloc(1,sizeof(DList));
   dlist_init(list, NULL); /* you don't want to free the strings */
   /* need a list to store the pointers, it is useless when SortByOrder is used, but I leave it in to keep the code simple */
   listop = (DList *)SUMA_calloc(1,sizeof(DList)); 
   dlist_init(listop, NULL); /* you don't want to free the data as it is copied from  OverlayPlanelist*/
         
   clist = NULL;
   N_clist = -1;
   Elm_OverlayPlanelist = NULL;
   do {
      if (!Elm_OverlayPlanelist) Elm_OverlayPlanelist = dlist_head(OverlayPlanelist);
      else Elm_OverlayPlanelist = Elm_OverlayPlanelist->next;
      
      OvD = (SUMA_OVERLAY_LIST_DATUM *) Elm_OverlayPlanelist->data;

      if (!OvD->Overlay->Label) sprintf (Label,"NULL");
      else sprintf (Label,"%s", OvD->Overlay->Label);

      SUMA_LH(Label);
      /* Now allocate space for that label */
      store = (char *)SUMA_calloc(strlen(Label)+10, sizeof(char));
      if (OvD->Overlay->isBackGrnd) {
         sprintf(store,"bk:%s", Label);
      } else {
         sprintf(store,"fg:%s", Label);
      }

      if (SortByOrder) {
         SUMA_LH("Sorting by order");
         /* list is already sorted, just copy the string and object structure pointers to lists */
         dlist_ins_next(list, dlist_tail(list), (void*)store);
         /* this line is redundant with SortByOrder but it don't hoyt */
         dlist_ins_next(listop, dlist_tail(listop), (void*)OvD);
      } else {   /* sort the list by aplhpabetical order */
         SUMA_LH("Sorting by name");
         if (!list->size) {
            dlist_ins_next(list, dlist_tail(list), (void*)store);
            dlist_ins_next(listop, dlist_tail(listop), (void*)OvD);
         }else { /* must sort first */
            Elm = NULL;
            Elmop = NULL;
            do {
               Found = NOPE;
               if (!Elm) {
                  Elm = dlist_head(list);
                  Elmop = dlist_head(listop);
               } else {
                  Elm = Elm->next;
                  Elmop = Elmop->next;
               }

               if (strcmp(store, (char*)Elm->data) <= 0) {
                  dlist_ins_prev(list, Elm, (void *)store);
                  dlist_ins_prev(listop, Elmop, (void *)OvD);
                  Found = YUP;
               } else if (Elm == dlist_tail(list)) {
                  /* reached the end, append */
                  dlist_ins_next(list, Elm, (void *)store);
                  dlist_ins_next(listop, Elmop, (void *)OvD);
                  Found = YUP;
               }
            } while (!Found);
         }

      }
   } while (Elm_OverlayPlanelist != dlist_tail(OverlayPlanelist));
   
   SUMA_LH("saving list.");
   if (!list->size) { /* Nothing found */
      SUMA_LH("Empty list");
      N_clist = 0;
   }else {
      Elm = NULL;
      Elmop = NULL;
      clist =  (char **)SUMA_calloc(list->size, sizeof(char *));
      oplist = (void **)SUMA_calloc(list->size, sizeof(void *));
      for (i=0; i< list->size; ++i) {
         if (!Elm) {
            Elm = dlist_head(list);
            Elmop = dlist_head(listop);
         } else {
            Elm = dlist_next(Elm);
            Elmop = dlist_next(Elmop);
         }
         clist[i] = (char*)Elm->data;
         OvD = (SUMA_OVERLAY_LIST_DATUM *) Elmop->data;
         oplist[i] = (void *)OvD->Overlay;
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Inserting %s with %s (%s).\n", 
            FuncName, clist[i], OvD->Overlay->Label, OvD->Overlay->Name);
      }

      N_clist = list->size;
      /* destroy list */
      dlist_destroy(list);SUMA_free(list);
      dlist_destroy(listop);SUMA_free(listop);
      dlist_destroy(OverlayPlanelist);SUMA_free(OverlayPlanelist);
      
      
      
   }
   
   clist_str = SUMA_CreateAssembleListStruct();
   clist_str->clist = clist;
   clist_str->oplist = oplist;
   clist_str->N_clist = N_clist;
   
   /* return */
   SUMA_RETURN (clist_str);  
}

/*!
   \brief determines if a Dset is related to a surface 
*/
SUMA_Boolean  SUMA_isDsetRelated(SUMA_DSET *dset, SUMA_SurfaceObject *SO)
{
   static char FuncName[]={"SUMA_isDsetRelated"};
   char *mp = NULL;
   int lmp = 0;
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(NOPE);
   if (!SO) SUMA_RETURN(NOPE);
   
   mp = SDSET_IDMDOM(dset); if (mp) lmp = strlen(mp); else lmp = 0;
   
   if (mp && lmp) {
      if (strcmp(mp, SO->idcode_str) == 0) SUMA_RETURN(YUP);
      if (SO->DomainGrandParentID) {
         if (strcmp(mp, SO->DomainGrandParentID) == 0) SUMA_RETURN(YUP); 
      }   
      if (SO->LocalDomainParentID) {
         if (strcmp(mp, SO->LocalDomainParentID) == 0) SUMA_RETURN(YUP);
      }
   }
   
   SUMA_RETURN(NOPE);
}

SUMA_Boolean SUMA_isDsetColumn_inferred(SUMA_DSET *dset, int icol)
{
   static char FuncName[]={"SUMA_isDsetColumn_inferred"};
   char *lblcp=NULL;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   lblcp = SUMA_DsetColLabelCopy(dset, icol, 0);
   
   SUMA_LH(lblcp);
   if (lblcp) {
      if (strstr(lblcp, "(inferred)")) SUMA_RETURN(YUP);   
   }
   SUMA_free(lblcp);
   SUMA_RETURN(NOPE);
}


/*!
   \brief determines if a Dset can be assigned to a surface object 
*/   
SUMA_Boolean SUMA_OKassign(SUMA_DSET *dset, SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_OKassign"};
   double range[2];
   int loc[2], *ind = NULL, lnp = 0;
   char *np = NULL, stmp[201];
   SUMA_Boolean LocalHead = NOPE;
       
   SUMA_ENTRY;
   
   if (!dset) SUMA_RETURN(NOPE);
   if (!SO) SUMA_RETURN(NOPE);

   /* does dset have a mesh parent ? */
   np = SDSET_IDMDOM(dset); if (np) lnp = strlen(np); else lnp = 0; 
   if (np && lnp) { 
      SUMA_LH("Has IDMDOM");
      /* has parent, verify against SO*/
      if (SUMA_isDsetRelated(dset, SO)) { 
         SUMA_LH("Is Related"); SUMA_RETURN(YUP); 
         }
      else { SUMA_LH("Is NOT Related"); SUMA_RETURN(NOPE); }
   }
   
   SUMA_LH("Has no parent, trying adoption");
   /* has no parent, check if you can adopt it*/
   ind = SUMA_GetNodeDef(dset);
   if (!ind) {
      SUMA_LH("No node index column");
      /* No node index. Make sure vec_len <= SO->N_Node */
      if (SDSET_VECLEN(dset) <= SO->N_Node) { 
         SUMA_LH("Number of values per column\n"
                      "is less than the number \n"
                      "of nodes in the surface.\n");
         if (SDSET_VECFILLED(dset) != SO->N_Node) {
            SUMA_LH("Need to attach a node index column, if possible");
            /* attempt to assign a node index column */
            if (!SUMA_AddNodeIndexColumn(dset, SO->N_Node)) {
                SUMA_LH(" Failed to add a node index column");
                SUMA_RETURN(NOPE);
            } else {           
               SUMA_LH("Added Index Column");
               SUMA_RETURN(YUP);
            }
         }else {
            SUMA_LH( 
               "Looks like a full list of values\n"
               "Techincally, there's no need for explicit node column.\n"
               "But at times, the data are not ordered by ascending node \n"
               "index which causes trouble.\nSo now I add a node index column"
               " always which would help point to the problem if it arises");
            /* Sept 21 04, call SUMA_AddNodeIndexColumn, it is good for you. 
               Might add an unecessary index column when none exit but makes 
               things clear*/
            if (!SUMA_AddNodeIndexColumn(dset, SO->N_Node)) {
                SUMA_LH(" Failed to add a node index column");
                SUMA_RETURN(NOPE);
            }        
            SUMA_LH("Added Index Column");
            SUMA_RETURN(YUP);
         }
      } else {
         snprintf(stmp, 200*sizeof(char), 
                        "Number of values per column (%d)\n"
                        "is larger than the number \n"
                        "of nodes (%d) in the surface.", 
                        SDSET_VECLEN(dset), SO->N_Node);
         SUMA_SLP_Warn(stmp);
         SUMA_RETURN(NOPE);
      }
   } else {
      SUMA_LH("Node index column found");
      /* there is a node index column, see if the range is OK */
      if (!SUMA_GetDsetNodeIndexColRange(dset, range, loc, 1)) {
         SUMA_SLP_Err("Unexpect error in SUMA_GetDsetColRange");
         SUMA_RETURN(NOPE);
      }
      if (range[0] < 0 || range[1] > SO->N_Node) {
         SUMA_SLP_Err("Node index range outside\n"
                      "0 and SO->N_Node");
         SUMA_RETURN(NOPE);
      }
      /* Now we're OK to return on a positive note */
      SUMA_RETURN(YUP);      
   } 
      
   SUMA_SL_Err("Should not get here");   
   SUMA_RETURN(NOPE);
}

/*!
   \brief Loads a Dset file and adds it to the list of datasets
   
   \param dlg (SUMA_SELECTION_DIALOG_STRUCT *) struture from selection dialogue
*/
void SUMA_LoadDsetOntoSO (char *filename, void *data)
{
   static char FuncName[]={"SUMA_LoadDsetOntoSO"};
   SUMA_SurfaceObject *SO = NULL;

   SUMA_ENTRY;
   if (!data || !filename) {
      SUMA_SLP_Err("Null data"); 
      SUMA_RETURNe;
   }
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (!SUMA_LoadDsetOntoSO_eng(filename, SO, 1, 1, 1, NULL)) {
      SUMA_SLP_Err("Failed loading, and colorizing dset"); 
      SUMA_RETURNe;
   }
   
   SUMA_RETURNe;
}

SUMA_Boolean SUMA_LoadDsetOntoSO_eng (char *filename, SUMA_SurfaceObject *SO,
                              int SetupOverlay, int MakeOverlayCurrent,
                              int LaunchDisplay, SUMA_OVERLAYS **used_over)
{
   static char FuncName[]={"SUMA_LoadDsetOntoSO_eng"};
   SUMA_IRGB *irgb=NULL;
   int OverInd = -1, lnp=-1, loc[2], OKdup = 0;
   char *np=NULL, *dsetcmap=NULL;
   SUMA_DSET_FORMAT form;
   DList *list=NULL;
   SUMA_LIST_WIDGET *LW=NULL;
   SUMA_DSET *dset = NULL, *dsetpre = NULL;
   SUMA_OVERLAYS *NewColPlane = NULL,  *colplanepre = NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   if (!filename || !SO) {
      SUMA_S_Err("Null data"); 
      SUMA_RETURN(NOPE);
   }
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,
               "%s: Received request to load %s for surface %s.\n", 
               FuncName, filename, SO->Label);
   }

   /* find out if file exists and how many values it contains */
   if (!SUMA_filexists(filename)) {
      SUMA_SLP_Err("File not found");
      SUMA_RETURN(NOPE);
   }

   /* take a stab at the format */
   form = SUMA_GuessFormatFromExtension(filename, NULL);
   
   /* load the dude */
   /* first, set the parent ID of the dset to be loaded,
   This parent ID is only used when generating an ID for those dsets
   that have no ID attached, like the 1D ones */
   if (SO->LocalDomainParentID) 
      SUMA_SetParent_DsetToLoad(SO->LocalDomainParentID);
   else if (SO->idcode_str) 
      SUMA_SetParent_DsetToLoad(SO->idcode_str); 
   else SUMA_SetParent_DsetToLoad(NULL);  

   dset = SUMA_LoadDset_s (filename, &form, 0); 
   SUMA_LHv("Dset as loaded is %p\n", dset);
   
   if (!dset) { SUMA_SLP_Err(  "Failed to load dataset.\n"
                                 "Make sure file exists\n"
                                 "and is of a supported\n"
                                 "format. See command line\n"
                                 "for hints in other error\n"
                                 "messages."); SUMA_RETURN(NOPE); }
   SUMA_SetParent_DsetToLoad(NULL);  /* reset the parent surface flag */
   
   if (LocalHead) {
      char *si = NULL;
      si = SUMA_DsetInfo(dset, 0);
      fprintf( SUMA_STDERR,
               "%s:\n----------------dset loaded ----------\n%s\n",
               FuncName, si);
      SUMA_free(si); si = NULL;
   }
   
   /* Check if the domain order is SO or not .
   If not specified, assign it */
   np = SDSET_IDMDOM(dset); if (np) lnp = strlen(np) ; else lnp = 0;
   
   if (np && lnp) {
      SUMA_SL_Note("dset has a mesh parent, Checking relationship");
      if (!SUMA_isDsetRelated(dset, SO)) {
         if (SUMA_isEnv("SUMA_AlwaysAssignSurface","Y")) {
            SUMA_S_Note("Setting domain_parent_idcode to NULL!");
            NI_set_attribute(dset->ngr, "domain_parent_idcode", NULL);
            np = NULL; lnp = 0; 
         } else {
            SUMA_SLP_Err("Dset not related to SO");
            SUMA_FreeDset(dset); dset=NULL;
            SUMA_RETURN(NOPE);
         }
      }
      
   }

   if (!np || lnp == 0) { 
      SUMA_LH("dset has no mesh parent, assigning SO");
      if (!SUMA_OKassign(dset, SO)) {
         SUMA_SurfaceObject *SOldp = SUMA_findSOp_inDOv(
                           SO->LocalDomainParentID,SUMAg_DOv, SUMAg_N_DOv);
         if (SOldp) {
            SUMA_SLP_Note( "Could not assign dset to SO.\n"
                           "Trying to assign to domain parent.");
            if (!SUMA_OKassign(dset, SOldp)) {
               SUMA_SLP_Err(  "Cannot assign dset to SO \n"
                              "or its local domain parent");
               SUMA_FreeDset(dset); dset=NULL;
               SUMA_RETURN(NOPE);
            }
            /* from that point on, treat dset as if being loaded onto SOpar*/
            SO = SOldp;
         } else {
            SUMA_SLP_Err("Cannot assign dset to SO.");
            SUMA_FreeDset(dset); dset=NULL;
            SUMA_RETURN(NOPE);
         }
      }
      NI_set_attribute(dset->ngr,"domain_parent_idcode", SO->idcode_str);
      NI_set_attribute(dset->ngr,"geometry_parent_idcode", SO->idcode_str);
      if (LocalHead) SUMA_ShowDset(dset, 0, NULL);
   } else {
      
   }
   
   /* add the dset to the list SUMAg_CF->DsetList*/
   dsetpre = dset;
   if (LocalHead) {
      fprintf( SUMA_STDERR,
               "%s: New dset (%s) has pointer %p\n", 
               FuncName, SDSET_LABEL(dset), dset); 
   }
   if (!SUMA_InsertDsetPointer(  &dset, SUMAg_CF->DsetList, 
                                 SUMAg_CF->Allow_Dset_Replace)) {
      SUMA_SLP_Err("Failed to add new dset to list");
      /* is there not a function to replace a dset yet? */
      SUMA_FreeDset(dset); dset = NULL;
      SUMA_RETURN(NOPE);
   }
   if (LocalHead) {
      fprintf( SUMA_STDERR,
               "%s: Now dset (%s) is  pointer %p\n", 
               FuncName, SDSET_LABEL(dset), dset); 
   }
   
   /* Does this dset have a built in colormap?
      If it does, then loadit into SCM */
   if (!SUMA_Insert_Cmap_of_Dset(dset)) {
      SUMA_S_Err("Failed to insert Cmap");
      SUMA_FreeDset(dset); dset = NULL;
      SUMA_RETURN(NOPE);
   }

   if (SetupOverlay) {
      OverInd = -1;
      {
         if (dset != dsetpre) { /* dset was pre-existing in the list */
            if (LocalHead) {
               fprintf( SUMA_STDERR,
                        "%s: Dset %s (%p) pre-existing, "
                        "finding its pre-existing overlays.\n", 
                        FuncName, SDSET_LABEL(dset), dset); 
            }
            if (!(colplanepre = SUMA_Fetch_OverlayPointerByDset (
                           SO->Overlays, SO->N_Overlays, dset, &OverInd))) {
               SUMA_SLP_Err("Failed to fetch existing dset's overlay pointer");
               /* is there not a function to replace a dset yet? */
               /* SUMA_FreeDset(dset); dset = NULL; do not free existing dset */
               SUMA_RETURN(NOPE);
            }
            /* have bias? REMOVE IT! */
            if (!SUMA_RemoveCoordBias(colplanepre)) {
               SUMA_SLP_Err("Failed to remove coord bias");
               SUMA_RETURN(NOPE);
            }
            OKdup = 1;
         } else { /* dset is considered new */
            colplanepre = NULL;
            /* The overlay index for that plane is SO->N_Overlays */
            OverInd = SO->N_Overlays;
            OKdup = 0;
         }
         /* set up the colormap for this dset */
         NewColPlane = SUMA_CreateOverlayPointer ( filename, 
                                                   dset, SO->idcode_str, 
                                                   colplanepre);
         if (SetupOverlay < 0) NewColPlane->isBackGrnd = YUP;
         else NewColPlane->isBackGrnd = NOPE;
         
         if (!NewColPlane) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed in SUMA_CreateOverlayPointer.\n", 
                     FuncName);
            SUMA_RETURN(NOPE);
         }

         /* Add this plane to SO->Overlays */
         if (!SUMA_AddNewPlane (SO, NewColPlane, SUMAg_DOv, 
                                SUMAg_N_DOv, OKdup)) {
            SUMA_SL_Err("Failed in SUMA_AddNewPlane");
            SUMA_FreeOverlayPointer(NewColPlane);
            if (!SUMA_DeleteDsetPointer(&dset, SUMAg_CF->DsetList)) {
               SUMA_S_Err("Failed to delete dset pointer");
            }

            SUMA_RETURN(NOPE);
         }
      } 


      /* set the opacity, index column and the range */
      NewColPlane->GlobalOpacity = YUP;
      NewColPlane->ShowMode = SW_SurfCont_DsetViewCol;
      if (!colplanepre) { /* only set this default if first time creating plane*/
         NewColPlane->OptScl->BrightFact = 0.8;
      }
      NewColPlane->OptScl->find = 0;
      NewColPlane->OptScl->tind = 0;
      NewColPlane->OptScl->bind = 0;
      SUMA_GetDsetColRange(dset, 0, NewColPlane->OptScl->IntRange, loc);


      /* stick a colormap onto that plane ? */
      dsetcmap = NI_get_attribute(dset->ngr,"SRT_use_this_cmap");
      if (dsetcmap) {
         SUMA_STRING_REPLACE(NewColPlane->cmapname, dsetcmap);
      } else {
         /* don't worry, there's a default one */
      }

      /* colorize the plane */
      SUMA_ColorizePlane(NewColPlane);

      /* SUMA_Show_ColorOverlayPlanes(&NewColPlane, 1, 1); */

      /* set the new curColPlane to the newly loaded plane,
      you need to do this before you remix the colors in case
      you are only showing the curColPlane.
      curColPlane is normally set in  SUMA_InitializeColPlaneShell
      but when SO->SurfCont->ShowCurForeOnly = YUP, curColPlane
      is used in the RemixRedisplay function.
      NOTE: You can't call SUMA_InitializeColPlaneShell
      before remixing because colors are reported in Lbl block
       June 28 04*/
      if (MakeOverlayCurrent) 
         SO->SurfCont->curColPlane = SO->Overlays[OverInd]; 
   }
   
   if (LaunchDisplay) {
      /* remix-redisplay  for surface */
      if (!SUMA_RemixRedisplay (SO)) {
         SUMA_RETURN(NOPE);
      }

      SUMA_LH("Refreshing Dset list");            
      /*update the list widget if open */
      LW = SO->SurfCont->SwitchDsetlst;
      if (LW) {
         if (!LW->isShaded) SUMA_RefreshDsetList (SO);  
      }  

      if (LocalHead) 
         fprintf (SUMA_STDERR,
                  "%s: Updating Dset frame, OverInd=%d\n", 
                  FuncName, OverInd);
      /* update the Dset frame */
      if (OverInd >= 0)        
         SUMA_InitializeColPlaneShell(SO, SO->Overlays[OverInd]);

   }
   
   if (used_over) *used_over = SO->Overlays[OverInd];
   SUMA_RETURN(YUP);
}

/*!
   \brief Loads a color plane file and adds it to a surface's list of colorplanes
   
   \param dlg (SUMA_SELECTION_DIALOG_STRUCT *) struture from selection dialogue
*/
void SUMA_LoadColorPlaneFile (char *filename, void *data)
{
   static char FuncName[]={"SUMA_LoadColorPlaneFile"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_OVERLAY_PLANE_DATA sopd;
   SUMA_IRGB *irgb=NULL;
   int OverInd = -1;
   DList *list=NULL;
   SUMA_LIST_WIDGET *LW=NULL;
   SUMA_Boolean LocalHead = NOPE;
      
   SUMA_ENTRY;

   if (!data) {
      SUMA_SLP_Err("Null data"); 
      SUMA_RETURNe;
   }
   
   SO = (SUMA_SurfaceObject *)data;
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,
               "%s: Received request to load %s for surface %s.\n", 
               FuncName, filename, SO->Label);
   }

   /* find out if file exists and how many values it contains */
   if (!SUMA_filexists(filename)) {
      SUMA_SLP_Err("File not found");
      SUMA_RETURNe;
   }

   irgb = SUMA_Read_IRGB_file(filename);
   if (!irgb) {
      SUMA_SLP_Err("Failed to read file.");
      SUMA_RETURNe;
   }

   sopd.N = irgb->N;
   sopd.Type = SOPT_ifff;
   sopd.Source = SES_Suma;
   sopd.GlobalOpacity = 0.3;
   sopd.isBackGrnd = NOPE;
   sopd.Show = YUP;
   /* dim colors from maximum intensity to preserve surface shape 
      highlights, division by 255 is to scale color values between 1 and 0 */
   sopd.DimFact = 0.5;
   sopd.i = (void *)irgb->i;
   sopd.r = (void *)irgb->r;
   sopd.g = (void *)irgb->g;
   sopd.b = (void *)irgb->b;
   sopd.a = NULL;

   if (!SUMA_iRGB_to_OverlayPointer (  SO, filename, &sopd, &OverInd, 
                                       SUMAg_DOv, SUMAg_N_DOv, 
                                       SUMAg_CF->DsetList)) {
      SUMA_SLP_Err("Failed to fetch or create overlay pointer.");
      SUMA_RETURNe;
   }

   /* values were copied, dump structure */
   irgb = SUMA_Free_IRGB(irgb);  

   /* See note before similar line in SUMA_LoadDsetOntoSO */
   SO->SurfCont->curColPlane = SO->Overlays[OverInd]; 

   if (!SUMA_RemixRedisplay (SO)) {
      SUMA_RETURNe;
   }
  
   SUMA_LH("Refreshing color plane list");            
   /*update the list widget if open */
   LW = SO->SurfCont->SwitchDsetlst;
   if (LW) {
      if (!LW->isShaded) SUMA_RefreshDsetList (SO);  
   }  
   
   if (LocalHead) 
      fprintf (SUMA_STDERR,
               "%s: Updating color plane frame, OverInd=%d\n", 
               FuncName, OverInd);
   /* update the color plane frame */
   if (OverInd >= 0)        
      SUMA_InitializeColPlaneShell(SO, SO->Overlays[OverInd]);

   SUMA_RETURNe;
}


/*** AFNI setup functions taken and trimmed from afni_setup.c
Reason for duplicating the functions is the complicated dependencies 
of some functions in afni_setup */
/*-----------------------------------------------------------------
   Process an AFNI setup file.
-------------------------------------------------------------*/

#define SUMA_ISTARRED(s) ( (s)[0]=='*' && (s)[1]=='*' && (s)[2]=='*' )

#define SUMA_EOLSKIP                                                          \
  do{ for( ; fptr[0] != '\n' && fptr[0] != '\0' ; fptr++ ) ; /* nada */  \
      if( fptr[0] == '\0' ){ if (fbuf) free(fbuf) ; fbuf = NULL; goto donesection; }                   \
      fptr++ ; } while(0)

#define SUMA_GETSSS                                                            \
  do{ int nu=0,qq;                                                        \
      if( fptr-fbuf >= nbuf || fptr[0] == '\0' ){ if (fbuf) free(fbuf); fbuf = NULL; goto donesection; } \
      str[0]='\0'; qq=sscanf(fptr,"%127s%n",str,&nu); nused+=nu;fptr+=nu; \
      if( str[0]=='\0' || qq==0 || nu==0 ){ if (fbuf) free(fbuf); fbuf = NULL; goto donesection; }       \
    } while(0)

#define SUMA_GETSTR                                                             \
  do{ SUMA_GETSSS ;                                                             \
      while(str[0]=='!' || (str[0]=='/' && str[1]=='/')){SUMA_EOLSKIP; SUMA_GETSSS;} \
    } while(0)

#define SUMA_GETEQN                                         \
  do{ SUMA_GETSTR ; if(SUMA_ISTARRED(str)) goto SkipSection ;    \
      strcpy(left,str) ;                               \
      SUMA_GETSTR ; if(SUMA_ISTARRED(str)) goto SkipSection ;    \
      strcpy(middle,str) ;                             \
      SUMA_GETSTR ; if(SUMA_ISTARRED(str)) goto SkipSection ;    \
      strcpy(right,str) ; } while(0)

#define SUMA_NSBUF 256

/*!
   \brief Extracts colors and colormaps from AFNI formatted .pal files
   Also used to extract such info from .afnirc file
   
   \param fname (char *) name of file containing colors.
                        either .afnirc or a .pal file
   \param SAC (SUMA_AFNI_COLORS *) pointer to structure that will contain 
                        the default afni colors and colormaps.
                        This structure must have been previously initialized 
                        in SUMA_Get_AFNI_Default_Color_Maps which also 
                        automatically scans ~/.afnirc then ./afnirc if the former
                        one was not found. 
   \return  N_palsRead (int) number of palettes found and read
                        -1 if trouble reading at least one of the palettes
                        
*/
int SUMA_AFNI_Extract_Colors ( char *fname, SUMA_AFNI_COLORS *SAC )
{
   static char FuncName[]={"SUMA_AFNI_Extract_Colors"};
   int    nbuf , nused , ii, ngood = -1;
   float rgb[3]={0.0, 0.0, 0.0};
   char * fbuf , * fptr ;
   char str[SUMA_NSBUF]="\0" , left[SUMA_NSBUF]="\0" , 
         middle[SUMA_NSBUF]="\0" , right[SUMA_NSBUF]="\0" ;
   SUMA_STRING *SS = NULL;
   SUMA_COLOR_MAP *CM=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   fbuf = AFNI_suck_file( fname ) ; if( fbuf == NULL ) { 
                                       SUMA_SL_Warn("File could not be read");
                                       SUMA_RETURN(-1) ; }
   nbuf = strlen(fbuf) ;            if( nbuf == 0    ) { 
                                       SUMA_SL_Warn("Empty  file");
                                       SUMA_RETURN(-1) ; } 

   fptr = fbuf ; nused = 0 ;

   /** scan for section strings, which start with "***" **/
   ngood = 0; /* assume none will be found */
   
   str[0] = '\0' ;  /* initialize string */

   if(LocalHead) { 
      fprintf(SUMA_STDERR,"Reading AFNI setup file = %s (%d bytes)",fname,nbuf) ;
   }

   while( nused < nbuf ){

      /**----------------------------------**/
      /**-- skip ahead to next section keyword --**/

      SkipSection: while( ! SUMA_ISTARRED(str) ){ SUMA_GETSTR; SUMA_LH(str);}

      /*- 04 Jun 1999 -*/

      if( strcmp(str, "***COLORS") != 0 && strcmp(str, "***PALETTES") != 0){
         SUMA_GETSTR ;
         goto SkipSection ;
      }

      
      SS = SUMA_StringAppend (NULL, NULL);
      
      /**--------------------**/
      /**-- COLORS section --**/

      if( strcmp(str,"***COLORS") == 0 ){
         char label[SUMA_NSBUF]="\0" , defn[SUMA_NSBUF]="\0" ;

         if (LocalHead) fprintf (SUMA_STDERR,"%s: Found ***COLORS\n", FuncName);
         while(1){                      /* loop, looking for 'label = color' */
            SUMA_GETEQN ;
            
            if (LocalHead) {
               fprintf (SUMA_STDERR,"\t %s%s%s\n", left, middle, right);
            }
            
            
            
            if( !THD_filename_pure(left) ) continue ;
            /* don't allow 'none' to be redefined! */
            if( strcmp(left,"none")==0 || strcmp(right,"none")==0 ) {
               sprintf(left,"none");
               rgb[0] = rgb[1] = rgb[2] =-1.0;
            } else {
               if (!SUMA_Interpret_AFNIColor (right, rgb)) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Failed to interpret color %s\n", 
                           FuncName, right);
                  SUMA_RETURN(-1);
               }
            }
            SAC->Cv = SUMA_Add_Color (left, 
                           rgb[0], rgb[1], rgb[2], 1.0, 
                           SAC->Cv, &(SAC->N_cols));
         }
         continue ;  /* skip to end of outer while loop */   
      } /* end of COLORS */
      #if 0
         /* causes crash on Fedora Core 7 and core 6, not worth it */
         SUMA_Interpret_AFNIColor (NULL, rgb);
      #endif

      /**----------------------**/
      /**-- PALETTES section --**/

      if( strcmp(str,"***PALETTES") == 0 ){  /* loop, looking for palettes */
         char label[SUMA_NSBUF] = "NoThing" , ccc , * cpt ;
         PBAR_palette_array * ppar=NULL ;
         PBAR_palette ** ppp ;
         PBAR_palette  * ppnew ;
         int npane , pmode , icol=0 , jj ;
         float val ;

         if (LocalHead) fprintf (SUMA_STDERR,"enter ***PALETTES\n");
         
         /* loop, looking for palettes */

         while(1){
            SUMA_GETSTR ; if( SUMA_ISTARRED(str) ) goto SkipSection ;
            if( fptr-fbuf >= nbuf ){ 
               if (fbuf) free(fbuf) ; fbuf = NULL; SUMA_RETURN(-1) ; 
            }

            if( str[0] != '[' ){                     /* found a palette label */
               strcpy(label,str) ;
               if( !THD_filename_ok(label) ){
                  fprintf( SUMA_STDERR,
                           "Error %s: In setup file %s, bad palette label: %s.\n"
                           " Ignoring palette.\n",
                          FuncName, fname,label) ;
                  if (fbuf) free(fbuf) ; fbuf = NULL; 
                  SUMA_RETURN(-1) ;
               }
               
               if (LocalHead) {
                  fprintf (SUMA_STDERR,
                           "%s: found palette label=%s. [len=%d label[0]=%d]\n"
                           "nbuf=%d fptr-fbuf=%ld\n", 
                     FuncName, label,(int)strlen(label),(int)label[0],
                     nbuf, (long int)(fptr-fbuf));
               }
               


               SUMA_GETSTR ; if( SUMA_ISTARRED(str) ) goto SkipSection ;
            }


            if( str[0] != '[' ){                    /* bad news! */
               fprintf(SUMA_STDERR,
                        "Error %s: In setup file %s, expected palette "
                        "'[n]' here: %s.\n",
                        FuncName, fname , str ) ;
               SUMA_RETURN(-1) ;
            }

            /* decide how big the new palette is to be, and what mode  */
            ii = sscanf( str , "[%d%c" , &npane , &ccc ) ;
            if( ii < 2 ){
               fprintf( SUMA_STDERR,
                        "%s: In setup file %s, can't interpret palette %s\n",
                        FuncName, fname , str ) ;
               SUMA_RETURN(-1) ;
            } else if( npane < NPANE_MIN || npane > NPANE_MAX ){
               fprintf(SUMA_STDERR,
                        "%s: In setup file %s, illegal palette count %s.\n",
                        FuncName, fname , str ) ;
               SUMA_RETURN(-1) ;
            }

            /* at this point, now loop to read parameters for new palette */
            if (LocalHead) {
               fprintf( SUMA_STDERR,
                        "%s: About to read %d panes.\n", 
                        FuncName, npane);
            }
            
            /* prepare the colormap */
            CM = (SUMA_COLOR_MAP *)SUMA_calloc(1,sizeof(SUMA_COLOR_MAP));
            if (CM == NULL) {
               SUMA_SL_Crit ("Failed to allocate for CM");
               SUMA_RETURN(-1);
            }
            CM->idvec = NULL;
            CM->chd = NULL;
            CM->top_frac = 0.0f;
            CM->SO = NULL;
            CM->N_M[0] = npane; CM->N_M[1] = 4;
            CM->cname = NULL;
 
            if (ccc == '+') CM->Sgn = 1;
            else CM->Sgn = -1;
            
            
            CM->Name = (char *)SUMA_calloc(strlen(label)+10, sizeof(char));
            CM->frac = (float *)SUMA_calloc(CM->N_M[0], sizeof(float));
            CM->M = (float**)
                        SUMA_allocate2D (CM->N_M[0], CM->N_M[1], sizeof(float));
            if (  CM->frac == NULL || CM->M == NULL || CM->Name == NULL ) {
               SUMA_SL_Crit ("Failed to allocate for fields of CM.");
               SUMA_RETURN (-1);
            }
            if (CM->Sgn == 1) sprintf(CM->Name, "%s_p%d", label, CM->N_M[0]);
            else sprintf(CM->Name, "%s_n%d", label, CM->N_M[0]);
            
            for( ii=0 ; ii < npane ; ii++ ){
               SUMA_GETEQN ;

               if (LocalHead) {
                  fprintf(SUMA_STDERR,
                           "%s: SUMA_GETEQN: %s %s %s\n",
                           FuncName, left,middle,right) ;
               }
               
               /* find that color */
               icol = SUMA_Find_Color (right, SAC->Cv, SAC->N_cols);
               if (icol < 0) {
                  fprintf( SUMA_STDERR,
                           "Error %s: Color %s not found in dbase.\n"
                           "Using no-color in its place\n", FuncName, right);
                  CM->M[npane - ii - 1][0] = 
                  CM->M[npane - ii - 1][1] = 
                  CM->M[npane - ii - 1][2] = -1.0;
                  CM->M[npane - ii - 1][3] = 0.0; 
               } else {
                  CM->M[npane - ii - 1][0] = SAC->Cv[icol].r;
                  CM->M[npane - ii - 1][1] = SAC->Cv[icol].g;
                  CM->M[npane - ii - 1][2] = SAC->Cv[icol].b;
                  CM->M[npane - ii - 1][3] = 1.0;
               }
               CM->frac[npane - ii - 1] = atof(left);
            }
            if (CM->frac[CM->N_M[0]-1] != 1.0f) {
               CM->top_frac = CM->frac[CM->N_M[0]-1];
            }
            
            CM->M0[0] = CM->M[0][0]; 
            CM->M0[1] = CM->M[0][1]; 
            CM->M0[2] = CM->M[0][2]; 
            CM->M0[3] = CM->M[0][3]; 

            /* add the map to the list */
            SAC->CMv = SUMA_Add_ColorMap (CM, SAC->CMv, &(SAC->N_maps));  
            if (SAC->CMv) ++ngood;
            continue ;  /* to end of outer while */
         }
      } /* end of PALETTES */
      SUMA_GETSTR ; goto SkipSection ;             /* find another section */

   }  /* end of while loop */
   
   donesection:

   if (LocalHead) fprintf(SUMA_STDERR,"%s: Returning\n", FuncName);
   if (fbuf) free(fbuf) ; 
   SUMA_RETURN (ngood);

}


/*!
   \brief turns a color defined by it's Xname or AFNI Hex formulation
   into good old RGB
   
   - The function is not particularly efficient for parsing numerous
   Xnamed colors...
   */
SUMA_Boolean SUMA_Interpret_AFNIColor (char *Name, float RGB[3])
{
   static char FuncName[]={"SUMA_Interpret_AFNIColor"};
   char *vargv[1]={ "SCALE_TO_MAP" };
   int cargc = 1;
   int r=0, g=0, b=0;
   char stmp[10]="\0";   
   XVisualInfo *vislist=NULL;
   static XtAppContext *app=NULL; 
   static Widget tl=NULL;
   static Display *dpy=NULL;
   XColor *color_exact=NULL;
   static Colormap cmap;
   static int iwarn = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!app) {
      app = (XtAppContext *)XtCalloc(1, sizeof(XtAppContext));
      memset(app, 0, sizeof(XtAppContext));
   }
   color_exact = (XColor*)XtCalloc(1, sizeof(XColor));
   memset(color_exact, 0, sizeof(XColor));
   
   if (!Name) {
      /* cleanup */
      if (tl && dpy) {
         SUMA_LH("Cleanup");
         XFreeColormap(dpy, cmap);
         /* These 2 lines cause a crash on Fedora Core 4, 
            but Core 4 crashes at XmCreateMainWindow anyway so we're doomed.*/
         XtDestroyWidget(tl); 
         XtDestroyApplicationContext(*app);
         tl = NULL;
         dpy = NULL;
      }
      SUMA_RETURN (YUP);
   }
   
      if (Name[0] == '#') { /* explicitly defined */
         SUMA_LHv("Explicit %s\n", Name);
         sprintf(stmp,"0x%c%c", Name[1], Name[2]);
         sscanf (stmp, "%x", &r);
         sprintf(stmp,"0x%c%c", Name[3], Name[4]);
         sscanf (stmp, "%x", &g);
         sprintf(stmp,"0x%c%c", Name[5], Name[6]);
         sscanf (stmp, "%x", &b);
         RGB[0] = (float)r/255.0;
         RGB[1] = (float)g/255.0;
         RGB[2] = (float)b/255.0;
         
      } else { /* named */
         /* XtAppInitialize (at least on mac osx) forces the application 
         to quit if display cannot be opened
         So you must decide ahead of time whether to call it or not! */
         if (SUMAg_CF->isGraphical) {
            SUMA_LHv("Graphical, named %s\n", Name);
            if (!tl) {
               SUMA_LH("tl init\n");
               /* tl = XtAppInitialize(app, "ScaleToMap", NULL, 0, &cargc, vargv,
                     SUMA_get_fallbackResources(), NULL, 0); 
                     Superseded by XtOpenApplication */

               tl = XtOpenApplication( app, "ScaleToMap", NULL, 
                                       0, &cargc, vargv,
                                       SUMA_get_fallbackResources(),
                                       topLevelShellWidgetClass,  
                                       NULL, 0);

               dpy = XtDisplay(tl);
               cmap = DefaultColormap(dpy, DefaultScreen(dpy));
            } 


            XParseColor(dpy, cmap, Name, color_exact);

            /* You need to divide by color_exact.red ,green and blue by 257
            to bring the numbers in the 0..255 range as listed in the 
            rgb.txt file */
            RGB[0] = (float)color_exact->red/255.0/257.0;
            RGB[1] = (float)color_exact->green/255.0/257.0;
            RGB[2] = (float)color_exact->blue/255.0/257.0;
         } else {
            SUMA_LH("Not graphical");
            if (0 && (LocalHead || !(iwarn % 10))) {
               fprintf(SUMA_STDERR,
                        "%s: \n"
                        "Xcolor %s cannot be resolved without \n"
                        "trying to open X display.\n"
                        "Returning color of %f %f %f.\n", 
                        FuncName, Name, 
                        SUMA_DUNNO_GRAY, SUMA_DUNNO_GRAY, SUMA_DUNNO_GRAY);
               iwarn = 0;
            }   
            ++iwarn;
            RGB[0] = RGB[1] = RGB[2] = SUMA_DUNNO_GRAY;
         }
      }
   
   if (color_exact) XtFree((char *)color_exact); color_exact=NULL;

   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: %s = %f %f %f\n", 
                        FuncName,  Name, RGB[0], RGB[1], RGB[2]);
   }
   
   SUMA_RETURN (YUP);
}

SUMA_Boolean SUMA_ContourateDsetOverlay(SUMA_OVERLAYS *cp, 
                                        SUMA_COLOR_SCALED_VECT *SV)
{
   static char FuncName[]={"SUMA_ContourateDsetOverlay"};
   int kkk=0, *ind=NULL, *key=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!cp) SUMA_RETURN(NOPE);
   if (!cp->dset_link) SUMA_RETURN(NOPE);
   
   if (!SV) {
      if (SUMA_is_Label_dset(cp->dset_link,NULL)) {
         SUMA_LHv("Creating countours for %s\n",SDSET_LABEL(cp->dset_link));
         if (cp->Contours) {
            /* this should only happen when users reload a label dset.
            or maybe if it regenerated by a draw ROI move in the future.
            Not it is worth noting this event, to be sure it is not
            being done excessively */
            SUMA_S_Notev("Wiping out existing contours for label dset %s\n",
                         SDSET_LABEL(cp->dset_link));
            SUMA_KillOverlayContours(cp);
         }
         
         ind = SDSET_NODE_INDEX_COL(cp->dset_link);
         key = SDSET_VEC(cp->dset_link, 0);
         cp->Contours = 
            SUMA_MultiColumnsToDrawnROI( SDSET_VECLEN(cp->dset_link),
                  (void *)ind, SUMA_int,
                  (void *)key, SUMA_int,
                  NULL, SUMA_notypeset,
                  NULL, SUMA_notypeset,
                  NULL, SUMA_notypeset,
                  SUMA_FindNamedColMap (cp->cmapname), 1,
                  cp->Label, SDSET_IDMDOM(cp->dset_link),
                  &(cp->N_Contours), 1, 0);
         if (LocalHead) SUMA_Show_ColorOverlayPlanes(&cp, 1, 0);
      } else {
         SUMA_S_Err("Cannot create contours non-label dset types without SV");
         SUMA_RETURN(NOPE);
      }
   } else {
      if (!SV->VCont || !SV->N_VCont) {
         SUMA_RETURN(NOPE);
      } else {
         SUMA_LHv("Creating countours for %s\n",SDSET_LABEL(cp->dset_link));
         if (cp->Contours) {
            SUMA_LHv("Wiping out existing contours for label dset %s\n",
                         SDSET_LABEL(cp->dset_link));
            SUMA_KillOverlayContours(cp);
         }
         if (SV->N_VCont != cp->N_NodeDef) {
            SUMA_S_Warn("I expected N_VCont and N_NodeDef to match!\n"
                        "Bad things might happen.");
         }
         ind = cp->NodeDef;
         key = SV->VCont;
         /*for (kkk=0; kkk<cp->N_NodeDef; ++kkk)
            fprintf(SUMA_STDERR,"%d-->%d\t", ind[kkk], key[kkk]);*/
         cp->Contours = 
            SUMA_MultiColumnsToDrawnROI( cp->N_NodeDef,
                  (void *)ind, SUMA_int,
                  (void *)key, SUMA_int,
                  NULL, SUMA_notypeset,
                  NULL, SUMA_notypeset,
                  NULL, SUMA_notypeset,
                  SUMA_FindNamedColMap (cp->cmapname), 1,
                  cp->Label, SDSET_IDMDOM(cp->dset_link),
                  &(cp->N_Contours), 1, 1);
         if (LocalHead) SUMA_Show_ColorOverlayPlanes(&cp, 1, 0);
      }
   }
   
   SUMA_RETURN(YUP);
}

/*!
   \brief colorize the data of the colorplane
*/
int SUMA_ColorizePlane (SUMA_OVERLAYS *cp)
{  
   static char FuncName[]={"SUMA_ColorizePlane"};
   int i, i3, N_i, *iv, *Nv;
   float *Rv, *Bv, *Gv;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (LocalHead)  {
      SUMA_LH("Color Plane Pre Colorizing");
      SUMA_Show_ColorOverlayPlanes ( &cp, 1, 0);
   }
   if (!cp) { SUMA_SL_Err("NULL cp"); SUMA_RETURN(NOPE); }
   if (!cp->dset_link) { 
      SUMA_SL_Err("Where's your dset_link?"); 
      SUMA_RETURN(NOPE); 
   }
   if (!cp->cmapname) { 
      SUMA_SL_Err("Where's your cmapname?"); 
      SUMA_RETURN(NOPE); 
   }
   if (!cp->ColVec) { SUMA_SL_Err("NULL cV"); SUMA_RETURN(NOPE); }
   
   /* is the coloring direct ? */
   if (strcmp(cp->cmapname, "explicit") == 0) {
      SUMA_LH("Explicit color specification");
      /* make sure dataset is of type NODE_RGB */
      if (SDSET_TYPE(cp->dset_link) != SUMA_NODE_RGB) {
         SUMA_SL_Err("Direct mapping is only supported for SUMA_NODE_RGB types");
         SUMA_RETURN(NOPE);
      }
      if (!(Nv = SUMA_GetNodeDef(cp->dset_link))) { 
         SUMA_SL_Err("Failed to find index column."); 
         SUMA_RETURN(NOPE); 
      }
      iv = SUMA_GetDsetColIndex (cp->dset_link, SUMA_NODE_R, &N_i);
      if (N_i != 1) { 
         SUMA_SL_Err("Failed to find red column."); 
         SUMA_free(iv); 
         SUMA_RETURN(NOPE); 
      }
      Rv = (float *)cp->dset_link->dnel->vec[iv[0]];SUMA_free(iv); iv = NULL;
      iv = SUMA_GetDsetColIndex (cp->dset_link, SUMA_NODE_G, &N_i);
      if (N_i != 1) { 
         SUMA_SL_Err("Failed to find green column."); 
         SUMA_free(iv); 
         SUMA_RETURN(NOPE); 
      }
      Gv = (float *)cp->dset_link->dnel->vec[iv[0]];SUMA_free(iv); iv = NULL;
      iv = SUMA_GetDsetColIndex (cp->dset_link, SUMA_NODE_B, &N_i);
      if (N_i != 1) { 
         SUMA_SL_Err("Failed to find blue column."); 
         SUMA_free(iv); 
         SUMA_RETURN(NOPE); 
      }
      Bv = (float *)cp->dset_link->dnel->vec[iv[0]];SUMA_free(iv); iv = NULL;
      /* go ahead and populate cV */
      
      if (LocalHead) {
         char *s = NULL;
         s = SUMA_DsetInfo(cp->dset_link, 0);
         SUMA_S_Note(s);
         SUMA_free(s);
      }
      if (cp->DimFact == 1.0) {
         for (i=0; i < SDSET_VECFILLED(cp->dset_link); ++i) {
            i3 = 3 * i;
            cp->NodeDef[i] = Nv[i];
            cp->ColVec[i3] = Rv[i]; ++i3;
            cp->ColVec[i3] = Gv[i]; ++i3;
            cp->ColVec[i3] = Bv[i]; 
         } 
      } else {
         for (i=0; i < SDSET_VECFILLED(cp->dset_link); ++i) {   
            i3 = 3 * i;
            cp->NodeDef[i] = Nv[i];
            cp->ColVec[i3] = Rv[i] * cp->DimFact; ++i3;
            cp->ColVec[i3] = Gv[i] * cp->DimFact; ++i3;
            cp->ColVec[i3] = Bv[i] * cp->DimFact; 
         }
      }
      cp->N_NodeDef = SDSET_VECFILLED(cp->dset_link);
   } else {
      /* indirect mapping */
      if (!SUMA_ScaleToMap_Interactive (cp)) {
         SUMA_SL_Err("Failed in SUMA_ScaleToMap_Interactive.");
         SUMA_RETURN(0); 
      }
      /* cp->N_NodeDef is taken care of inside SUMA_ScaleToMap_Interactive */
   }   
   
   
   
   if (LocalHead)  {
      SUMA_LH("Color Plane Post Colorizing");
      SUMA_Show_ColorOverlayPlanes ( &cp, 1, 0);
   }
   SUMA_RETURN(1);
}

/*!
   \brief Sets up the defaults for the convexity plane.
   
   \param SO (SUMA_SurfaceObject *) 
            pointer to surface object that the convexity dataset
                                    is attributed to. 
   \param ConvPlane (SUMA_OVERLAYS *) 
*/
SUMA_Boolean SUMA_SetConvexityPlaneDefaults(SUMA_SurfaceObject *SO, 
                                            DList *DsetList) 
{
   static char FuncName[]={"SUMA_SetConvexityPlaneDefaults"};
   float IntRange[2], *Vsort = NULL;
   float *Cx=NULL;
   int junk;
   char *eee = NULL;
   int icmap;
   SUMA_OVERLAYS *ConvPlane;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   if (!SUMAg_CF->scm) { /* colors not setup, go back */
      SUMAg_CF->scm = SUMA_Build_Color_maps();
      if (!SUMAg_CF->scm) {
         SUMA_SL_Warn("No color maps set up.\n");
         SUMA_RETURN(YUP);
      }
   }
   
   if (!(ConvPlane = 
            SUMA_Fetch_OverlayPointer(SO->Overlays, SO->N_Overlays, 
                                      "Convexity", &junk))) {
      SUMA_SL_Err("Failed to find overlay plane 'Convexity'");
      SUMA_RETURN(NOPE);
   }
   
   if (SO->Group && strcmp(SO->Group,SUMA_DEF_TOY_GROUP_NAME) == 0) {
      SUMA_LH("Adriano");
      /* fun times, gimme a random colormap */
      if (1) {
         icmap = rand()%SUMAg_CF->scm->N_maps; 
         SUMA_STRING_REPLACE( ConvPlane->cmapname, 
                              SUMAg_CF->scm->CMv[icmap]->Name);
         /* SUMA_S_Notev("Cmap %s\n", ConvPlane->cmapname); */
      } else { /* Used here to force crash on linux, bug should now be fixed
                  See comment in SUMA_GimmeSomeSOs (also other 20 maps...)*/
         SUMA_STRING_REPLACE(ConvPlane->cmapname, "rgybr20");
      }
   } else {
      /* decide on the color map */
      eee = getenv("SUMA_ConvColorMap");
      if (eee) {
         icmap = SUMA_Find_ColorMap ( eee, SUMAg_CF->scm->CMv, 
                                      SUMAg_CF->scm->N_maps, -2 );
         if (icmap < 0) {
            SUMA_SL_Err("Colormap specified in\n"
                        "environment variable SUMA_ConvColorMap\n"
                        "was not found. Using ngray20.\n");
            SUMA_STRING_REPLACE(ConvPlane->cmapname, "ngray20");
        } else {
            SUMA_STRING_REPLACE(ConvPlane->cmapname, eee);
        }
      } else {
         SUMA_STRING_REPLACE(ConvPlane->cmapname, "ngray20");
      }
   } 

   SUMA_LH("Deciding on brightness factor");   
   /* decide on the convexity brightness factor */
   eee = getenv("SUMA_ConvBrightFactor");
   if (eee) {
      ConvPlane->OptScl->BrightFact = (float)strtod(eee, NULL);
      if (ConvPlane->OptScl->BrightFact < 0) {
         SUMA_SL_Err("Brightness factor specified in\n"
                     "environment variable SUMA_ConvColorMap\n"
                     "is negative. Using default.\n");
         ConvPlane->OptScl->BrightFact = SUMA_DIM_CONVEXITY_COLOR_FACTOR;
     } 
   } else {
      ConvPlane->OptScl->BrightFact = SUMA_DIM_CONVEXITY_COLOR_FACTOR;
   } 
   
   /* Now place the color map in the Coloroverlay structure */
   ConvPlane->GlobalOpacity = SUMA_floatEnv("SUMA_ConvexityDsetOpacity", 
                                             SUMA_CONVEXITY_COLORPLANE_OPACITY); 
   ConvPlane->ShowMode = SW_SurfCont_DsetViewCol;
   ConvPlane->isBackGrnd = YUP;

   SUMA_LH("Smoothing Cx");   
   /* work the options for creating the scaled color mapping a bit */
   ConvPlane->OptScl->interpmode = SUMA_NO_INTERP;
   ConvPlane->OptScl->ApplyClip = YUP;
   
   IntRange[0] = 5; IntRange[1] = 95; /* percentile clipping range*/ 
   Cx = (float *)SUMA_GetCx(SO->idcode_str, DsetList, 0);
   if (!Cx) { SUMA_SL_Err("Failed to find Cx\n"); SUMA_RETURN (NOPE);  }
   Vsort = SUMA_PercRange (Cx, NULL, SO->N_Node, IntRange, IntRange, NULL); 
   if (Vsort[0] < 0 && Vsort[SO->N_Node -1] > 0 ) {
      /* the new method */
      if (fabs(IntRange[0]) > IntRange[1]) {
         IntRange[1] = -IntRange[0];
      } else {
         IntRange[0] = -IntRange[1];
      }
   } else { 
     /* The old method, do nothing here */ 
   }
   if (Vsort) SUMA_free(Vsort); Vsort = NULL;
   
   ConvPlane->OptScl->find = 0; /* the intensity column */
   ConvPlane->OptScl->tind = 0; /* the threshold column */
   ConvPlane->OptScl->bind = 0; /* the brightness modulation column */
   ConvPlane->OptScl->IntRange[0] = IntRange[0]; 
   ConvPlane->OptScl->IntRange[1] = IntRange[1];
   /* Force Auto Range to these percentile values */
   ConvPlane->ForceIntRange[0] = IntRange[0]; 
   ConvPlane->ForceIntRange[1] = IntRange[1];

   SUMA_RETURN(YUP);
}

/*!
   \brief j = SUMA_GetNodeOverInd(Sover, node);
   returns the index, into Sover->NodeDef such
   that Sover->NodeDef[j] = node;
   \sa SUMA_GetNodeRow_FromNodeIndex
*/
int SUMA_GetNodeOverInd (SUMA_OVERLAYS *Sover, int node)
{
   static char FuncName[]={"SUMA_GetNodeOverInd"};
   int Found, i;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;

   /* Now look for the node's location in the color overlay plane.
   Nodes that are not colored will be absent ... */
   if (node < 0) SUMA_RETURN(-1);
   
   Found = -1;
   if (SDSET_VECFILLED(Sover->dset_link) > node) { /* try the straight shot */
      if (Sover->NodeDef[node] == node) {
         SUMA_LH("Good, found it easily");
         /* make sure node is not outside number of defined nodes */
         if (node >= Sover->N_NodeDef) {
            /* this one's masked but it was left over from the previous pass 
            Must go search below to make sure whether it is truly masked or not*/
            SUMA_LH("Can't tell for sure");
         } else {
            SUMA_RETURN(node);
         }
      }
   }
   if (Found < 0) {
      SUMA_LH("The hard way");
      i=0; 
      while (Found <0 && i<Sover->N_NodeDef) {
         if (Sover->NodeDef[i] == node) Found = i;
         ++i;
      }      
   }   

   SUMA_RETURN(Found);
}

/*-----------------------------------------------------------*/
/* 
   Activate callbacks pertinent to SO->SelectedNode and Sover
   This function should be called after SO->SelectedNode has been set 
   \param SO
   \param Sover
   \param src
   \param ngr: a NI_group* containing crosshair data from AFNI
*/
SUMA_Boolean SUMA_Selected_Node_Activate_Callbacks (
      SUMA_SurfaceObject *SO, SUMA_OVERLAYS *Sover,
      SUMA_ENGINE_SOURCE Src, NI_group *ngr)
{
   static char FuncName[]={"SUMA_Selected_Node_Activate_Callbacks"};
   NI_element*nelts = NULL, *nelts_s2v=NULL;
   char *ts_dset_id = NULL, *cbuf=NULL;
   SUMA_DSET *in_dset=NULL;
   float *fv3=NULL;
   DListElmt *el=NULL;
   NI_element *nel=NULL;
   SUMA_CALLBACK *cb=NULL;
   NI_element *nelpars=NULL;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   if (!SO || !SO->SurfCont || !Sover) {
      /* this can happen in normal cases where nothing is loaded or selected. */
      SUMA_LH("No Surface or Surface Controller, or other important stuff!");
      SUMA_RETURN(YUP);
   } 
   
   /* setup callback, if needed */
   if (SUMAg_CF->callbacks) {
      el = dlist_head(SUMAg_CF->callbacks);
      while (el) {
         cb = (SUMA_CALLBACK *)el->data;
         if (  cb->event == SUMA_NEW_NODE_ACTIVATE_EVENT && 
               cb->active > 0 && 
               (Sover && Sover->dset_link ) ) {
            SUMA_LHv("Looking for callback parents involved \n"
                     "with dset %s\n",
                     SDSET_ID(Sover->dset_link ));
            /* Is any of the parents involved here? */
            if (SUMA_is_CallbackParent(cb, 
                                       SDSET_ID(Sover->dset_link ),
                                       NULL)){
               SUMA_SetCallbackPending(cb, 1, Src);
               /* setup event parameters
                  YOU SHOULD NOT SET ANYTHING that THIS event
                  call does not normally receive */
               if (!(nelpars = SUMA_FindNgrNamedElement(
                                 cb->FunctionInput, "event_parameters"))) {
                  SUMA_S_Err("Failed to find parameters element!");                                 SUMA_RETURN(NOPE);
               }  
               NI_SET_INT(nelpars,     
                          "event.new_node", SO->SelectedNode);
               NI_set_attribute( nelpars, 
                                 "event.SO_idcode", SO->idcode_str);
               NI_set_attribute(nelpars, 
                                "event.overlay_name", 
                                Sover->Name);
               if ((Src == SES_Afni || 
                    Src == SES_SumaFromAfni) && 
                   ngr) { 
                     /* See if there is a time series */
                  nelts = 
                        SUMA_FindNgrNamedElement(ngr,
                                                 "underlay_array");
                  if ((nelts_s2v = SUMA_FindNgrNamedElement(ngr,
                                                 "v2s_node_array"))) {
                     if (AFNI_yesenv("SUMA_USE_V2S_TS")) {
                        SUMA_LH("Using ts from v2s");
                        nelts = nelts_s2v;
                     }else {
                        SUMA_LH("Ignoring ts from v2s");
                        nelts_s2v = NULL;
                     }
                  }
                     
                  if ( (nelts)){
                     SUMA_LH("Have underlay time "
                                 "series from AFNI");
                     /* check if length of time series matches dsets
                     in question */
                     ts_dset_id = 
                        SUMA_GetNgrColStringAttr( cb->FunctionInput, 
                                             0, "ts_dsets_idcode");
                     if (!(SUMA_is_ID_4_DSET(ts_dset_id, 
                                             &in_dset))) {
                        SUMA_S_Err("Could not find ts dset");
                        SUMA_RETURN(NOPE);
                     }

                     if (nelts->vec_len && 
                         nelts->vec_len == SDSET_VECNUM(in_dset) &&
                         NI_YES_ATTR(nelts,"has_taxis")) {
                        nel = NI_new_data_element(
                                 "callback.data", nelts->vec_len);
                        NI_set_attribute(nel, 
                                         "atr_name", "ts_vec");
                        NI_add_column(nel, NI_FLOAT, nelts->vec[0]);
                        NI_add_to_group(cb->FunctionInput, nel);
                        if (LocalHead) {
                           char stmp[1000], *cbuf=NULL;
                           float fv3[3], *fv=NULL;
                           cbuf = NI_get_attribute(nelts, "vox_ijk");
                           SUMA_StringToNum(cbuf, (void *)fv3,3,1);
                           if (!nelts_s2v) {
                              sprintf(stmp,"underlay_array.%d.%d.%d.1D",
                                           (int)fv3[0], (int)fv3[1],(int)fv3[2]);
                           } else {
                              sprintf(stmp,"v2s_array.%d.1D",
                                           SO->SelectedNode);
                           }
                           fv = (float *)nelts->vec[0];
                           SUMA_LHv("Writing %s\n", stmp);
                           SUMA_WRITE_ARRAY_1D( fv, 
                                                nelts->vec_len, 
                                                1, stmp); 
                        }
                     } else {
                        SUMA_LHv(
                           "vec_len = %d\n"
                           "SDSET_VECNUM = %d\n"
                           "has_taxis = %s\n",
                           nelts->vec_len,
                           SDSET_VECNUM(in_dset),
                           NI_get_attribute(nelts,"has_taxis") );
                     }
                  } else {
                     SUMA_LH("No underlay time "
                                 "series from AFNI");
                     if (LocalHead) SUMA_ShowNel(ngr);
                  }
               }
            } else {
               SUMA_LH("No involved parents found");
            }
         } else {
            SUMA_LHv("Skipping callback for %s...\n", 
                     cb->FunctionName);
         }
         el = dlist_next(el);
      }
   }
   SUMA_RETURN(YUP);
}  

