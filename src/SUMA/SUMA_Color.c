#include "SUMA_suma.h"
#define MAIN     /* need this to read in color info from afni.h */
#include "../afni.h"
#undef MAIN

#ifdef SUMA_MakeColorMap_STAND_ALONE
   /* need to define these global variables because function calls are made to functions in files that declare these variables as extern */
   SUMA_CommonFields *SUMAg_CF;
   SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
   SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
   int SUMAg_N_SVv; /*!< Number of SVs stored in SVv */
   SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object structure vector*/
   int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
   
#elif defined SUMA_ScaleToMap_STAND_ALONE
   /* need to define these global variables because function calls are made to functions in files that declare these variables as extern */
   SUMA_CommonFields *SUMAg_CF;
   SUMA_SurfaceViewer *SUMAg_cSV; /*!< Global pointer to current Surface Viewer structure*/
   SUMA_SurfaceViewer *SUMAg_SVv; /*!< Global pointer to the vector containing the various Surface Viewer Structures */
   int SUMAg_N_SVv; /*!< Number of SVs stored in SVv */
   SUMA_DO *SUMAg_DOv;   /*!< Global pointer to Displayable Object structure vector*/
   int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
   
#else
   extern SUMA_CommonFields *SUMAg_CF;
   extern int SUMAg_N_DOv; 
   extern SUMA_DO *SUMAg_DOv;
   
#endif

/*! The set of functions deals with node colors
*/



/*! 
This function creates an RGB colormap containing Ncols that vary linearily 
   from the first color in Fiducials to the last.
   
   SM = SUMA_MakeColorMap (Fiducials, N , N_cols, SkipLast, Name)
   
   \param Fiducials (float **) N x 3 matrix containing RGB values (range 0..1) 
          of fiducial colours which will be equally spaced on the color map
   \param N (int) number of fiducial colors in Fiducials
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
SUMA_COLOR_MAP* SUMA_MakeColorMap (float **Fiducials, int Nfid, int Ncols, SUMA_Boolean SkipLast, char *Name)
{
   static char FuncName[]={"SUMA_MakeColorMap"};
   float **M, dFid[3];
   int i, j, Ninter, Ngap, im, Ncolsgood, Npergap;
   SUMA_COLOR_MAP * SM;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }
   
   /* check for bad input */
   for (i=0; i < Nfid; ++i) {
      for (j=0; j < 3; ++j) {
         if (Fiducials[i][j] < 0 || Fiducials[i][j] > 1) {
            fprintf (SUMA_STDERR,"Error %s: Fiducial colors must be between 0 & 1 (found %f)\n", FuncName, Fiducials[i][j]);
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
      if (SkipLast) Ncolsgood = (int)(rint((float)Ninter/Ngap) * Ngap + Nfid + 1);
      else Ncolsgood = (int)(rint((float)Ninter/Ngap) * Ngap + Nfid);
      
      fprintf (SUMA_STDERR,"Error %s: The choice of Ncols does not work with the number\nof fiducial colours.\nTry Ncols = %d\n", \
      FuncName, Ncolsgood); 
      SUMA_RETURN (NULL);
   }
   
   /* allocate for M */
   M = (float **)SUMA_allocate2D (Ncols, 3, sizeof(float));
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
      /*fprintf (SUMA_STDERR,"%s:  dFid = %f %f %f\n", FuncName, dFid[0], dFid[1] , dFid[2]);*/
      
      for (j=0; j < Npergap+1; ++ j) {

         if (im < Ncols) {
            M[im][0] = Fiducials[i][0] + j*dFid[0];
            M[im][1] = Fiducials[i][1] + j*dFid[1];
            M[im][2] = Fiducials[i][2] + j*dFid[2];
            /*fprintf (SUMA_STDERR,"%s: M[%d][:] = %f %f %f\n", FuncName, im, M[im][0], M[im][1], M[im][2]); */
         }
               
         ++im;
      }
   }
   if (!SkipLast) {
      M[im][0] = Fiducials[Ngap][0];
      M[im][1] = Fiducials[Ngap][1];
      M[im][2] = Fiducials[Ngap][2];
   }
   
   /* package the resutls */
   SM = (SUMA_COLOR_MAP *)SUMA_malloc(sizeof(SUMA_COLOR_MAP));
   if (SM == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for SM.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   SM->Name = (char *)SUMA_calloc(strlen(Name)+1, sizeof(char));
   if (SM->Name == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for SM->Name.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   sprintf(SM->Name, "%s",Name); 
   SM->M = M;
   SM->N_Col = Ncols;

   SM->frac = NULL; /* a linear map */
   SM->Sgn = 0; /* setup for linear maps with no signing, mapping a la old ScaleToMap*/
   
   SUMA_RETURN (SM);
}

/*! 
This function creates an RGB colormap containing Ncols that vary linearily 
   from the first color in Fiducials to the last.
   
   SM = SUMA_MakeColorMap_v2 (Fiducials, NFid, Nin , SkipLast, Name)
   
   \param Fiducials (float **) NFid x 3 matrix containing RGB values (range 0..1) 
          of fiducial colours which will be equally spaced on the color map
   \param NFid (int) number of fiducial colors
   \param Nin (int*) NFid x 1 vector indicating the number of interpolations to perform
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

SUMA_COLOR_MAP* SUMA_MakeColorMap_v2 (float **Fiducials, int Nfid, int *Nint, SUMA_Boolean SkipLast, char *Name)
{
   static char FuncName[]={"SUMA_MakeColorMap_v2"};
   float **M, dFid[3];
   int i, j, im, Ncols;
   SUMA_COLOR_MAP * SM;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   /* check for bad input and calculate the total number of colors*/
   if (Nint[0]) {
      fprintf (SUMA_STDERR,"Error %s: The index of the first color (%d) must be 0, indexing starts at 0 not 1.\n", FuncName, Nint[0]);
      SUMA_RETURN (NULL);
   }
   for (i=0; i < Nfid; ++i) {
      for (j=0; j < 3; ++j) {
         if (Fiducials[i][j] < 0 || Fiducials[i][j] > 1) {
            fprintf (SUMA_STDERR,"Error %s: Fiducial colors must be between 0 & 1 (found %f)\n", FuncName, Fiducials[i][j]);
            SUMA_RETURN (NULL);
         }
      }
   }
   
   Ncols = Nint[Nfid-1]+1;
   
   if (SkipLast) Ncols = Ncols - 1;
      
   /* allocate for M */
   M = (float **)SUMA_allocate2D (Ncols, 3, sizeof(float));
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
         /*fprintf (SUMA_STDERR,"%s:  dFid = %f %f %f\n", FuncName, dFid[0], dFid[1] , dFid[2]);*/

         for (j=0; j < (Nint[i+1]- Nint[i]); ++ j) {

               M[im][0] = Fiducials[i][0] + j*dFid[0];
               M[im][1] = Fiducials[i][1] + j*dFid[1];
               M[im][2] = Fiducials[i][2] + j*dFid[2];
               /*fprintf (SUMA_STDERR,"%s: M[%d][:] = %f %f %f\n", FuncName, im, M[im][0], M[im][1], M[im][2]); */

            ++im;
         }
   }
   
   if (!SkipLast){
      M[im][0] = Fiducials[Nfid-1][0];
      M[im][1] = Fiducials[Nfid-1][1];
      M[im][2] = Fiducials[Nfid-1][2];
   }
   
   /* package the resutls */
   SM = (SUMA_COLOR_MAP *)SUMA_malloc(sizeof(SUMA_COLOR_MAP));
   if (SM == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for SM.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   SM->Name = (char *)SUMA_calloc(strlen(Name)+1, sizeof(char));
   if (SM->Name == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for SM->Name.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   sprintf(SM->Name, "%s",Name); 
   SM->M = M;
   SM->N_Col = Ncols;
   
   SM->frac = NULL; /* a linear map */
   SM->Sgn = 0; /* setup for linear maps with no signing, mapping a la old ScaleToMap*/
   
   SUMA_RETURN (SM);
}

/*! function to free memory allocated for SUMA_COLOR_MAP */
void SUMA_Free_ColorMap (SUMA_COLOR_MAP* SM)
{
   static char FuncName[]={"SUMA_Free_ColorMap"};
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   if (SM->Name) SUMA_free(SM->Name);
   if (SM->M) SUMA_free2D((char **)SM->M, SM->N_Col);
   if (SM->frac) SUMA_free(SM->frac);
   if (SM) SUMA_free(SM);

   SUMA_RETURNe;
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
SUMA_AFNI_COLORS *SUMA_Get_AFNI_Default_Color_Maps ()
{
   static char FuncName[]={"SUMA_Get_AFNI_Default_Color_Maps"};
   float rgb[3];
   SUMA_RGB_NAME *Cv = NULL;
   SUMA_COLOR_MAP **CMv=NULL;
   SUMA_COLOR_MAP *CMp=NULL, *CMn=NULL;
   SUMA_AFNI_COLORS *SAC=NULL;
   int i, j, icol;
   int N_maps, N_cols;
   int       ival , ii,jj ;
   float     fval ;
   float     pthr[NPANE_MAX+1] ;
   int       pov[NPANE_MAX+1] ;
   char *homeenv=NULL, *sumarc=NULL;
   struct stat stbuf;
   SUMA_Boolean LocalHead_Detail = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }
   
   
   /* initilialize*/
   N_maps = -1;
   N_cols = -1;

   /* add the none color first */
   Cv = SUMA_Add_Color ("none", 
                        -1.0, -1.0, -1.0, 1.0, 
                        Cv, &N_cols);
                        
   /* get the rgb, floats of each color defined DEFAULT_NCOLOVR */
   for (i=0; i<DEFAULT_NCOLOVR; ++i) {
      if (!SUMA_Interpret_AFNIColor (INIT_def_colovr[i], rgb)) {
         fprintf(SUMA_STDERR,"Error %s: Failed to interpret color %s : %s\n", FuncName, INIT_def_labovr[i], INIT_def_colovr[i]);
      } else {
         if (LocalHead_Detail) fprintf(SUMA_STDERR,"%s: Adding color...", FuncName); 
         Cv = SUMA_Add_Color (INIT_def_labovr[i], 
                           rgb[0], rgb[1], rgb[2], 1.0, 
                           Cv, &N_cols);
     } 
   }
   
   /* Now create the afni color maps with more than 10 panes (excerpts from afni.c)*/
   
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
      CMp = (SUMA_COLOR_MAP *)SUMA_malloc(sizeof(SUMA_COLOR_MAP));
      CMn = (SUMA_COLOR_MAP *)SUMA_malloc(sizeof(SUMA_COLOR_MAP));
      if (CMp == NULL || CMn == NULL) {
         SUMA_SL_Crit ("Failed to allocate for CMp &/| CMn.");
         SUMA_RETURN(NULL);
      }
      CMp->N_Col = i; 
      CMn->N_Col = i;
      CMp->Sgn = 1;
      CMn->Sgn = -1;
      
      CMp->Name = (char *)SUMA_calloc(25, sizeof(char));
      CMn->Name = (char *)SUMA_calloc(25, sizeof(char));
      CMp->frac = (float *)SUMA_calloc(i, sizeof(float));
      CMn->frac = (float *)SUMA_calloc(i, sizeof(float));
      CMp->M = (float**)SUMA_allocate2D (CMp->N_Col, 3, sizeof(float));
      CMn->M = (float**)SUMA_allocate2D (CMn->N_Col, 3, sizeof(float));
      if (  CMp->frac == NULL || CMn->frac == NULL 
         || CMp->M == NULL || CMn->M == NULL 
         || CMp->Name == NULL || CMn->Name == NULL ) {
         SUMA_SL_Crit ("Failed to allocate for fields of CMp &/| CMn.");
         SUMA_RETURN (NULL);
      }
      
      sprintf(CMp->Name,   "afni_p%d",i); 
      sprintf(CMn->Name, "afni_n%d",i);
      
      if (LocalHead_Detail) fprintf (SUMA_STDERR,"%s: Building colormap POS #%d (%s)\n", FuncName, i, CMp->Name);
      
      for ( j = 0; j < i; ++j) {
         if (!INIT_ovin_pos[i][j]) {
            if (LocalHead_Detail) {
               fprintf (SUMA_STDERR,"\t[i%d] NoColor\t%f\n", 
                              INIT_ovin_pos[i][j], 
                              INIT_pval_pos[i][j]);   
            }
            CMp->M[i - j - 1][0] = CMp->M[i - j - 1][1] = CMp->M[i - j - 1][2] = -1.0; 
         } else {
            if (LocalHead_Detail) {
               fprintf (SUMA_STDERR,"\t[i%d] %s\t%f\n", 
                              INIT_ovin_pos[i][j], 
                              INIT_def_labovr[INIT_ovin_pos[i][j]-1], INIT_pval_pos[i][j]);
            }
            /* find out which color this is */
            icol = SUMA_Find_Color (INIT_def_labovr[INIT_ovin_pos[i][j]-1], Cv, N_cols);
            if (icol < 0) {
               fprintf (SUMA_STDERR,"Error%s: Failed to find color %s\nUsing no-color in its place\n", 
                                       FuncName, INIT_def_labovr[INIT_ovin_pos[i][j]-1]);
               CMp->M[i - j - 1][0] = CMp->M[i - j - 1][1] = CMp->M[i - j - 1][2] = -1.0; 
            } else {
               CMp->M[i - j - 1][0] = Cv[icol].r;
               CMp->M[i - j - 1][1] = Cv[icol].g;
               CMp->M[i - j - 1][2] = Cv[icol].b;
            }
         }
         CMp->frac[i - j - 1] = INIT_pval_pos[i][j];
      }
      
      /* add the positive map to the list */
      CMv = SUMA_Add_ColorMap (CMp, CMv, &N_maps);
      if (!CMv) {
         SUMA_SL_Crit("Failed in SUMA_Add_ColorMap");
         SUMA_RETURN(NULL);
      }
      
      if (LocalHead_Detail) fprintf (SUMA_STDERR,"%s: Building colormap SGN #%d (%s)\n", FuncName, i, CMn->Name);
      
      for ( j = 0; j < i; ++j) {
         if (!INIT_ovin_sgn[i][j]) {
            if (LocalHead_Detail) {
               fprintf (SUMA_STDERR,"\t[i%d] NoColor\t%f\n", 
                              INIT_ovin_sgn[i][j], 
                              INIT_pval_sgn[i][j]);
            }
            CMn->M[i - j - 1][0] = CMn->M[i - j - 1][1] = CMn->M[i - j - 1][2] = -1.0;
         } else {
            if (LocalHead_Detail) {
               fprintf (SUMA_STDERR,"\t[i%d] %s\t%f\n", 
                              INIT_ovin_sgn[i][j], 
                              INIT_def_labovr[INIT_ovin_sgn[i][j]-1], INIT_pval_sgn[i][j]);
            }
            icol = SUMA_Find_Color (INIT_def_labovr[INIT_ovin_sgn[i][j]-1], Cv, N_cols);
            if (icol < 0) {
               fprintf (SUMA_STDERR,"Error%s: Failed to find color %s\nUsing no-color in its place", 
                                    FuncName, INIT_def_labovr[INIT_ovin_sgn[i][j]-1]);
               CMn->M[i - j - 1][0] = CMn->M[i - j - 1][1] = CMn->M[i - j - 1][2] = -1.0;
            } else {
               CMn->M[i - j - 1][0] = Cv[icol].r;
               CMn->M[i - j - 1][1] = Cv[icol].g;
               CMn->M[i - j - 1][2] = Cv[icol].b;
            }
         }
         CMn->frac[i - j - 1] = INIT_pval_sgn[i][j];
      }
      
      /* add the negative map to the list */
      CMv = SUMA_Add_ColorMap (CMn, CMv, &N_maps);
      if (!CMv) {
         SUMA_SL_Crit("Failed in SUMA_Add_ColorMap");
         SUMA_RETURN(NULL);
      }
   }
   
   
   /* perhaps someday include the continuous color maps from AFNI too. 
      (see pbar.c file, search for colorscale) */
   
      
   SAC = (SUMA_AFNI_COLORS *) SUMA_malloc(sizeof(SUMA_AFNI_COLORS));
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
      if (LocalHead) fprintf (SUMA_STDERR,"%s: Loading %s ...\n", FuncName, sumarc);
      if (SUMA_AFNI_Extract_Colors ( sumarc, SAC ) < 0) {
         fprintf(SUMA_STDERR,"Error %s: Failed scanning .afnirc for colors and colormaps.\nProceeding ...\n", FuncName);
      } 
   } else {
      if (LocalHead) fprintf (SUMA_STDERR,"%s: No rc files found.\n", FuncName);
   }
   if (sumarc) free(sumarc);
   
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
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }
   
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

*/
SUMA_COLOR_MAP ** SUMA_Add_ColorMap (SUMA_COLOR_MAP *CM, SUMA_COLOR_MAP **OldCMv, int *N_maps) 
{
   static char FuncName[]={"SUMA_Add_ColorMap"};
   SUMA_COLOR_MAP ** NewCMv = NULL;
   int iadd;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

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
   NewCMv = (SUMA_COLOR_MAP **) SUMA_realloc(OldCMv, *N_maps * sizeof(SUMA_COLOR_MAP *));
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
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   if (Cv) {
      for (i=0; i < N_cols; ++i) {
         if (Cv[i].r == -1) {
            sprintf (stmp, "%d/%d: color(%d) No Color(%s): [%f %f %f %f]\n", 
                           i+1, N_cols, i, Cv[i].Name, Cv[i].r, Cv[i].g, Cv[i].b, Cv[i].a);
            SS = SUMA_StringAppend (SS, stmp);
         } else {
            sprintf (stmp, "%d/%d: color(%d) %s: [%f %f %f %f]\n", 
                           i+1, N_cols, i, Cv[i].Name, Cv[i].r, Cv[i].g, Cv[i].b, Cv[i].a);
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
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }
   
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
                  sprintf (stmp, "%d/%d: cmap(%d) %s(.), %d cols.", i+1, N_maps, i, CMv[i]->Name,  CMv[i]->N_Col);
                  break;
               case 1:
                  sprintf (stmp, "%d/%d: cmap(%d) %s(+), %d cols.", i+1, N_maps, i, CMv[i]->Name,  CMv[i]->N_Col);
                  break;
               case -1:
                  sprintf (stmp, "%d/%d: cmap(%d) %s(-), %d cols.", i+1, N_maps, i, CMv[i]->Name,  CMv[i]->N_Col);
                  break;
               default:   
                  sprintf (stmp, "%d/%d: cmap(%d) %s(?), %d cols.\n\tSgn field of colormap is not acceptable (%d)\n", 
                     i+1, N_maps, i, CMv[i]->Name,  CMv[i]->N_Col, CMv[i]->Sgn);
                  break;
            }
            SS = SUMA_StringAppend (SS, stmp);
            if (CMv[i]->frac) {
               SS = SUMA_StringAppend (SS, "   Possibly non-linear\n");
            } else {
               SS = SUMA_StringAppend (SS, "   Linear\n");
            }
            switch (detail) {
               case 0:
                  jmax = 0;
                  break;
               case 1:
                  if (CMv[i]->N_Col < 5) jmax = CMv[i]->N_Col;
                  else jmax = 5;
                  break;
               case 2:
                  jmax = CMv[i]->N_Col;
                  break;
               default:
                  SUMA_SL_Err("Bad detail value\nUsing detail = 2");
                  jmax = CMv[i]->N_Col;
                  break;
            }
            
            if (jmax) {
               for (j=jmax-1; j >= 0; --j) {
                  if (CMv[i]->frac) {
                     if (j == jmax -1) {
                        sprintf (stmp, "rank (i)[R    \tG    \tB    \t\tf]\n");
                        SS = SUMA_StringAppend (SS,stmp);    
                     }
                     sprintf (stmp, "%03d:\t[% .3f\t% .3f\t% .3f\t\t% .3f]\n", 
                                    j, CMv[i]->M[j][0], CMv[i]->M[j][1],  CMv[i]->M[j][2], CMv[i]->frac[j]);
                  } else {
                     if (j == jmax - 1) {
                        sprintf (stmp, "rank (i):[R    \tG    \tB    ]\n");
                        SS = SUMA_StringAppend (SS,stmp);    
                     }
                     sprintf (stmp, "%03d:\t[% .3f\t% .3f\t% .3f]\n", 
                                    j, CMv[i]->M[j][0], CMv[i]->M[j][1],  CMv[i]->M[j][2]);
                  }
                  SS = SUMA_StringAppend (SS,stmp); 
               }
               if (jmax < CMv[i]->N_Col - 1) { 
                  if (CMv[i]->frac) SS = SUMA_StringAppend (SS,"..:\t [.....\t....\t....\t\t....]\n");
                  else SS = SUMA_StringAppend (SS,"..:\t [.....\t....\t....]\n");
               }
               if (jmax < CMv[i]->N_Col) { 
                  j = CMv[i]->N_Col - 1;
                  if (CMv[i]->frac) {
                     sprintf (stmp, "%03d:\t [% .3f\t% .3f\t% .3f\t\t% .3f]\n", 
                                    j, CMv[i]->M[j][0], CMv[i]->M[j][1],  CMv[i]->M[j][2], CMv[i]->frac[j]);
                  } else {
                     sprintf (stmp, "%03d:\t [% .3f\t% .3f\t% .3f]\n", 
                                    j, CMv[i]->M[j][0], CMv[i]->M[j][1],  CMv[i]->M[j][2]);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
void SUMA_Show_ColorMapVec (SUMA_COLOR_MAP **CMv, int N_maps, FILE *Out, int detail) 
{
   static char FuncName[]={"SUMA_Show_ColorMapVec"};
   char *s;
   SUMA_Boolean LocalHead  = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   
   if (Out == NULL) Out = stdout;
      
   s =  SUMA_ColorMapVec_Info(CMv, N_maps, detail);
   
   if (s) {
      fprintf (Out, "%s", s);
      SUMA_free(s);
   }else {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_ColorMapVec_Info.\n", FuncName);
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
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }
   
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
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }
   
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

/*!
   function that reads in a 1D format color map file 
   1D format contains 3 or 4 columns:
   3 columns: R G B
   4 columns: R G B frac 
   R G B are float values between 0 and 1.0 specifying the R G B colors
   frac is the fraction of the color map assigned to each color. These 
        are the same numbers shown to the right of the colormap in AFNI
   
   The colormap files are specified in the order adopted in AFNI's palette files
   The last row in the file is the first color (bottom color) in the map. 
   If you specify frac then the lowest fraction must be at the bottom row
*/
SUMA_COLOR_MAP *SUMA_Read_Color_Map_1D (char *Name)
{
   static char FuncName[]={"SUMA_Read_Color_Map_1D"};
   MRI_IMAGE *im = NULL;
   float *far=NULL;
   int i=0;
   SUMA_COLOR_MAP* SM = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

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
   SM = (SUMA_COLOR_MAP*) SUMA_malloc(sizeof(SUMA_COLOR_MAP));
   SM->N_Col = im->nx;
   SM->Name = (char *)SUMA_malloc(sizeof(char)*(strlen(Name)+1));
   sprintf(SM->Name, "%s", Name);
   if (im->ny == 4) {
      SM->frac = (float*) SUMA_calloc(im->nx, sizeof(float));
   } else {
      SM->frac = NULL;
   }
   
   SM->M = (float**)SUMA_allocate2D (SM->N_Col, 3, sizeof(float));
   
   
   far = MRI_FLOAT_PTR(im);

   if (im->ny == 4) {
      SM->Sgn = 1;
      for (i=0; i < im->nx; ++i) {
         SM->M[SM->N_Col - i - 1][0] = far[i];
         SM->M[SM->N_Col - i - 1][1] = far[i+im->nx];
         SM->M[SM->N_Col - i - 1][2] = far[i+2*im->nx];
         SM->frac[SM->N_Col - i - 1] = far[i+3*im->nx];
         if (SM->frac[SM->N_Col - i - 1] < 0.0) SM->Sgn = -1;
      }
   } else {
      SM->Sgn = 0;
      for (i=0; i < im->nx; ++i) {
         SM->M[SM->N_Col - i - 1][0] = far[i];
         SM->M[SM->N_Col - i - 1][1] = far[i+im->nx];
         SM->M[SM->N_Col - i - 1][2] = far[i+2*im->nx];
      }
   }

   /* check on craziness in frac */
   if (SM->frac && SM->N_Col > 1) {
      for (i=0; i < im->nx-1; ++i) {
         if (SM->frac[i] > SM->frac[i+1]) {
            SUMA_S_Err("Fractions must be specified in monotonic\n descending order from the top to the bottom of the column");
            SUMA_Free_ColorMap (SM); mri_free(im);
            SUMA_RETURN(NULL);
         }
      }
   }
   
   mri_free(im); im = NULL; 
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: Colormap read:\n", FuncName);
      if (SM->frac) {
         for (i=0; i < SM->N_Col; ++i) {
            fprintf (SUMA_STDOUT,"%f\t%f\t%f\t%f\n", SM->M[i][0], SM->M[i][1], SM->M[i][2], SM->frac[i]);
         }
      } else SUMA_disp_mat (SM->M, SM->N_Col, 3, 1);
   }
   
   
   SUMA_RETURN (SM);
}

/*! function that turns a non-linear color map into a linear one
The larger the number of colors in the linear color map, the close the
approximation to the non-linear map. 
   \param SM (SUMA_COLOR_MAP*) a non-linear colormap (i.e. SM->frac != NULL)
   \param N_Lin (int) number of colors to use in the linear version of the map
                      if N_Lin = -1, N_Lin is set to 1000
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
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   if (!SM) {
      SUMA_S_Err("NULL color map");
      SUMA_RETURN(LSM);
   }
   
   if (!SM->frac) {
      SUMA_S_Err("NULL SM->frac!\nMap is linear");
      SUMA_RETURN(LSM);
   }
   
   if (N_lin < 0) N_lin = 20;     /* set default linear map length */
   
   if (!N_lin) {
      SUMA_S_Err("N_lin = 0");
      SUMA_RETURN(LSM);
   }
   
   /* allocate for new map */
   LSM = (SUMA_COLOR_MAP *)SUMA_malloc(sizeof(SUMA_COLOR_MAP));
   if (LSM == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for LSM.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   LSM->Name = (char *)SUMA_calloc(strlen(SM->Name)+10, sizeof(char));
   if (LSM->Name == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for LSM->Name.\n", FuncName);
      SUMA_free(LSM);
      SUMA_RETURN (NULL);
   }
   sprintf(LSM->Name, "%s_lin",SM->Name); 
   LSM->N_Col = N_lin;
   LSM->frac = NULL;
   LSM->Sgn = SM->Sgn;
                                       
   LSM->M = (float **)SUMA_allocate2D (LSM->N_Col, 3, sizeof(float));
   if (LSM->M == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for LSM->M.\n", FuncName);
      SUMA_free(LSM->Name);
      SUMA_free(LSM);
      SUMA_RETURN (NULL);
   }
   
   ilin = 0;
   for (i=0; i < SM->N_Col; ++i) {
      if (LSM->Sgn >= 0) {
         ilin_stp = (int)(ceil((double)SM->frac[i] * (double)LSM->N_Col)) - 1;
      } else {
         ilin_stp = (int)(ceil( (1.0 + (double)SM->frac[i]) * (double)LSM->N_Col/2 ) ) - 1;
      }
      while (ilin < ilin_stp) {
         LSM->M[ilin][0] = SM->M[i][0];
         LSM->M[ilin][1] = SM->M[i][1];
         LSM->M[ilin][2] = SM->M[i][2];
         ++ilin; 
      }
   }
   
   /* copy last value */
   LSM->M[LSM->N_Col-1][0] = SM->M[SM->N_Col-1][0];
   LSM->M[LSM->N_Col-1][1] = SM->M[SM->N_Col-1][1];
   LSM->M[LSM->N_Col-1][2] = SM->M[SM->N_Col-1][2];
   
   if (LocalHead) {
      for (i=0; i < LSM->N_Col; ++i) {
         fprintf (SUMA_STDOUT,"%d:\t%f\t%f\t%f\n", i, LSM->M[i][0], LSM->M[i][1], LSM->M[i][2]); 
      }
      fprintf (SUMA_STDOUT,"%s: ilin_stp = %d\n", FuncName, ilin_stp); 
   }
   
   SUMA_RETURN(LSM);
}

/*!----------------------------------------------------------------------
 * sprintf_long_to_hex    - write hex chars to a string
 *
 * return number of hex pairs written (should equal bytes)
 *----------------------------------------------------------------------
*/
int r_sprintf_long_to_hex
    (
   char          * dest,           /* location of output string     */
   unsigned long   lsrc,      /* number to translate           */
   int             bytes,       /* total bytes (hex pairs)       */
   int      pad      /* pad the result?               */
    )
{
    static char hexstring[] = "0123456789ABCDEF";

    unsigned char   ub;
    char          * cp = dest;
    int             posn, size, ret;

    if ( (bytes <= 0) || (bytes > 4) )
    {
   *cp = '\0';
   return 0;
    }

    size = r_ulong_size( lsrc );

    if ( (size < bytes) && !pad )
   ret = size;
    else
   ret = bytes;

    for ( posn = ret-1; posn >= 0; posn-- )
    {
   /* write one hex pair for this byte */
   ub = ( lsrc >> (posn << 3) ) & 0xff;      /* current ubyte */
   *cp++ = hexstring[(ub>>4) & 0xf];      /* upper nibble  */
   *cp++ = hexstring[ ub     & 0xf];      /* lower nibble  */
    }

    *cp = '\0';

    return ret;
}

/* return number of bytes needed to represent a long - return at least 1 */
int r_ulong_size ( unsigned long l )
{
    if ( l & 0xff000000 )
   return 4;

    if ( l & 0xff0000 )
   return 3;

    if ( l & 0xff00 )
   return 2;

    return 1;
} 

#ifdef SUMA_MakeColorMap_STAND_ALONE
void SUMA_MakeColorMap_usage ()
   {
      fprintf (SUMA_STDOUT,   "\n\33[1mUsage1: \33[0m\n");
      fprintf (SUMA_STDOUT, "MakeColorMap <-fn Fiducials_Ncol> [-pos] [-ah prefix] [-h/-help]\n");
      fprintf (SUMA_STDOUT, "\t Creates a colormap of N colors that contains the fiducial colors.\n");
      fprintf (SUMA_STDOUT, "\t -fn Fiducials_Ncol: Fiducial colors and their indices in the color\n"
                            "\t                     map are listed in file Fiducials_Ncol.\n");
      fprintf (SUMA_STDOUT, "\t\t Each row contains 4 tab delimited values:\n");
      fprintf (SUMA_STDOUT, "\t\t R G B i\n");
      fprintf (SUMA_STDOUT, "\t\t R G B values are between 0 and 1 and represent the \n"
                            "\t\t i-th color in the colormap. i should be between 0 and\n");
      fprintf (SUMA_STDOUT, "\t\t N-1, N being the total number of colors in the colormap.\n");
      fprintf (SUMA_STDOUT, "\n\33[1mUsage2: \33[0m\n");
      fprintf (SUMA_STDOUT, "MakeColorMap <-f Fiducials> <-nc N> [-sl] [-ah prefix] [-h/-help]\n");
      fprintf (SUMA_STDOUT, "\t Creates a colormap of N colors that contains the fiducial colors.\n");
      fprintf (SUMA_STDOUT, "\t -f Fiducials:  Fiducial colors are listed in an ascii file Fiducials. \n");
      fprintf (SUMA_STDOUT, "\t\t Each row contains 3 tab delimited R G B values between 0 and 1.\n");
      fprintf (SUMA_STDOUT, "\t -nc N: Total number of colors in the color map.\n");
      fprintf (SUMA_STDOUT, "\t -sl: (optional, default is NO) if used, the last color in the Fiducial \n");
      fprintf (SUMA_STDOUT, "\t\t list is omitted. This is useful in creating cyclical color maps.\n");
      fprintf (SUMA_STDOUT, "\n\33[1mUsage3: \33[0m\n");
      fprintf (SUMA_STDOUT, "MakeColorMap <-std MapName>\n");
      fprintf (SUMA_STDOUT, "\t Returns one of SUMA's standard colormaps. Choose from:\n");
      fprintf (SUMA_STDOUT, "\t rgybr20, ngray20, gray20, bw20, bgyr19, \n");
      fprintf (SUMA_STDOUT, "\t matlab_default_byr64, roi128, roi64\n");
      fprintf (SUMA_STDOUT, "\n\33[1mCommon options to all usages:\33[0m\n");
      fprintf (SUMA_STDOUT, "\t -ah prefix: (optional, Afni Hex format.\n");
      fprintf (SUMA_STDOUT, "\t              default is RGB values in decimal form)\n");
      fprintf (SUMA_STDOUT,   "\t\t use this option if you want a color map formatted to fit \n"
                              "\t\t in AFNI's .afnirc file. The colormap is written out as \n");
      fprintf (SUMA_STDOUT,   "\t\tprefix_01 = #xxxxxxx \n\t\tprefix_02 = #xxxxxxx\n\t\t etc...\n"); 
      /* that's not a useful option, both versions will be written out */
      /*fprintf (SUMA_STDOUT, "\t -pos: (optional, default is NO) create a positive only color map. \n");
      fprintf (SUMA_STDOUT, "\t\t This option is meaningful when combined with -ah option.\n"); */
      fprintf (SUMA_STDOUT, "\t -h or -help: displays this help message.\n");
      fprintf (SUMA_STDOUT, "\n");
      fprintf (SUMA_STDOUT,   "Example Usage 1: Creating a colormap of 20 colors that goes from \n"
                              "Red to Green to Blue to Yellow to Red.\n");
      fprintf (SUMA_STDOUT,   "\n\tThe file FidCol_Nind contains the following:\n");
      fprintf (SUMA_STDOUT,   "\t1 0 0 0\n\t0 1 0 5\n\t0 0 1 10\n\t1 1 0 15\n\t1 0 0 19\n\n");
      fprintf (SUMA_STDOUT,   "\tThe following command will generate the RGB colormap in decimal form:\n");
      fprintf (SUMA_STDOUT,   "\tMakeColorMap -fn FidCol_Nind \n\n");
      fprintf (SUMA_STDOUT,   "\tThe following command will generate the colormap and write it as \n"
                              "\tan AFNI color palette file:\n");
      fprintf (SUMA_STDOUT,   "\tMakeColorMap -fn FidCol_Nind -ah TestPalette > TestPalette.pal\n\n");
      fprintf (SUMA_STDOUT,   "Example Usage 2: Creating a cyclical version of the colormap in usage 1:\n");
      fprintf (SUMA_STDOUT,   "\n\tThe file FidCol contains the following:\n");
      fprintf (SUMA_STDOUT,   "\t1 0 0\n\t0 1 0\n\t0 0 1\n\t1 1 0\n\t1 0 0\n\n");
      fprintf (SUMA_STDOUT,   "\tThe following command will generate the RGB colormap in decimal form:\n");
      fprintf (SUMA_STDOUT,   "\tMakeColorMap -f FidCol -sl -nc 20 \n\n");
      fprintf (SUMA_STDOUT,   "Example Usage 3: MakeColorMap -std ngray20 \n\n");
      fprintf (SUMA_STDOUT,   "To read in a new colormap into AFNI, either paste the contents of TestPalette.pal\n");
      fprintf (SUMA_STDOUT,   "in your .afnirc file or read the .pal file using AFNI as follows:\n");
      fprintf (SUMA_STDOUT,   "1- run afni\n2- Define Function --> right click on Inten (over colorbar) \n"
                              "\t--> Read in palette (choose TestPalette.pal)\n");
      fprintf (SUMA_STDOUT,   "3- set the #colors chooser (below colorbar) to 20 (the number of colors in TestPalette.pal).\n");
      /*fprintf (SUMA_STDOUT, "\t To Compile:\ngcc  -DSUMA_MakeColorMap_STAND_ALONE -Wall -Wno-unused-variable -o SUMA_MakeColorMap SUMA_Color.c SUMA_lib.a libmri.a  -I/usr/X11R6/include -I./ -L/usr/lib -L/usr/X11R6/lib -lMesaGLwM -lMesaGLw -lGLU -lGL -lXmu -lXm -lXt -lXext -lX11 -lm\n");*/
      fprintf (SUMA_STDOUT, "\t\t Ziad S. Saad & Rick R. Reynolds SSCC/NIMH/NIH ziad@nih.gov \tTue Apr 23 14:14:48 EDT 2002\n\n");
   }
 
int main (int argc,char *argv[])
{/* Main */
   char  FuncName[]={"MakeColorMap"}, 
         *FidName = NULL, *Prfx = NULL, h[9], *StdType=NULL; 
   int Ncols = 0, N_Fid = 0, kar, i, ifact, *Nind = NULL;
   float **Fid=NULL, **M=NULL;
   MRI_IMAGE *im = NULL;
   float *far=NULL;
   SUMA_Boolean   brk, SkipLast, AfniHex, PosMap, 
                  Usage1, Usage2, Usage3, LocalHead = NOPE;
   SUMA_COLOR_MAP *SM=NULL;
      
   /* allocate space for CommonFields structure and initialize debug*/
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   SUMAg_CF->InOut_Notify = NOPE;
   
   if (argc < 2) {
      SUMA_MakeColorMap_usage();
      exit (1);
   }
   
   kar = 1;
   brk = NOPE;
   SkipLast = NOPE;
   AfniHex = NOPE;
   PosMap = NOPE;
   Usage1 = NOPE;
   Usage2 = NOPE;
   Usage3 = NOPE;
   while (kar < argc) { /* loop accross command ine options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
          SUMA_MakeColorMap_usage();
         exit (1);
      }
      if (!brk && (strcmp(argv[kar], "-v") == 0))
      {
         LocalHead = NOPE;
         brk = YUP;
      }
      if (!brk && (strcmp(argv[kar], "-f") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -f ");
            exit (1);
         }
         FidName = argv[kar];
         Usage1 = YUP;
         brk = YUP;
      }      

      if (!brk && (strcmp(argv[kar], "-fn") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -fn ");
            exit (1);
         }
         FidName = argv[kar];
         Usage2 = YUP;
         brk = YUP;
      }      
      
      if (!brk && (strcmp(argv[kar], "-nc") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -nc ");
            exit (1);
         }
         Ncols = atoi(argv[kar]);
         Usage1 = YUP;
         brk = YUP;
      }      
   
      if (!brk && (strcmp(argv[kar], "-ah") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -ah ");
            exit (1);
         }
         Prfx = argv[kar];
         AfniHex = YUP; 
         brk = YUP;
      }      
      
      if (!brk && (strcmp(argv[kar], "-std") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -std ");
            exit (1);
         }
         StdType = argv[kar];
         Usage3 = YUP; 
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-sl") == 0))
      {
         SkipLast = YUP;         
         brk = YUP;
      }      
      
      if (!brk && (strcmp(argv[kar], "-pos") == 0))
      {
         /* obsolete */
         PosMap = YUP;
         
         brk = YUP;
      }      
   
      if (!brk) {
         fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
         exit (1);
      } else {   
         brk = NOPE;
         kar ++;
      }
      
   }/* loop accross command ine options */
   
   /* check input */
   if ( (Usage1 && Usage2) || (Usage1 && Usage3) || (Usage2 && Usage3)) {
      fprintf (SUMA_STDERR,"Error %s: Mixing options from multiple usage modes.\n", FuncName);
      exit(1);
   }
   
   if (!Usage1 && !Usage2 && !Usage3) {
      fprintf (SUMA_STDERR,"Error %s: One of these options must be used:\n-f -fn or -std.\n", FuncName);
      exit(1);
   }
   
   if (Usage1 || Usage2) {
      if (!SUMA_filexists (FidName)) {
         fprintf (SUMA_STDERR,"Error %s: File %s could not be found.\n", FuncName, FidName);
         exit(1);
      }
      
      /* read the fiducials file */
      im = mri_read_1D (FidName);
      if (!im) {
         SUMA_S_Err("Failed to read file");
         exit(1);
      }

      far = MRI_FLOAT_PTR(im);
      N_Fid = im->nx * im->ny;
   }

   if (PosMap) {
      fprintf (SUMA_STDERR,"\nWarning %s: -pos option is obsolete.\n", FuncName);
   }
   
   
   /* allocate for fiducials */
   if (Usage1) {
      if (N_Fid % 3) {
         fprintf (SUMA_STDERR,"Error %s: Not all rows in %s appear to have RGB triplets.\n", FuncName, FidName);
         exit (1);
      }

      Fid = (float **) SUMA_allocate2D (N_Fid / 3, 3, sizeof(float));
      if (Fid == NULL) {
         fprintf (SUMA_STDERR,"Error %s: Could not allocate for Fid.\n", FuncName);
         exit(1);
      }

      for (i=0; i < im->nx; ++i) {
         Fid[i][0] = far[i];
         Fid[i][1] = far[i+im->nx];
         Fid[i][2] = far[i+2*im->nx];
      }
      
      mri_free(im); im = NULL; 
      /* now create the color map */
      SM = SUMA_MakeColorMap (Fid, N_Fid/3, Ncols, SkipLast, FuncName);
      if (SM == NULL) {
         fprintf (SUMA_STDERR,"Error %s: Error in SUMA_MakeColorMap.\n", FuncName);
         exit(1);
      }
   } 
   if (Usage2) { /* second usage */
      if (N_Fid % 4) {
         fprintf (SUMA_STDERR,"Error %s: Not all rows in %s appear to have RGB N quadruplets.\n", FuncName, FidName);
         exit (1);
      }

      Fid = (float **) SUMA_allocate2D (N_Fid / 4, 3, sizeof(float));
      Nind = (int *) SUMA_calloc (N_Fid/4, sizeof(int));
      if (Fid == NULL || !Nind) {
         fprintf (SUMA_STDERR,"Error %s: Could not allocate for Fid or Nind.\n", FuncName);
         exit(1);
      }
      
      for (i=0; i < im->nx; ++i) {
         Fid[i][0] = far[i];
         Fid[i][1] = far[i+im->nx];
         Fid[i][2] = far[i+2*im->nx];
         Nind[i] = (int)far[i+3*im->nx];
      }
      
      mri_free(im); im = NULL; 
      
      /* now create the color map */
      SM = SUMA_MakeColorMap_v2 (Fid, N_Fid/4, Nind, SkipLast, FuncName); 
      if (SM == NULL) {
         fprintf (SUMA_STDERR,"Error %s: Error in SUMA_MakeColorMap.\n", FuncName);
         exit(1);
      }
      Ncols = SM->N_Col;
   }
   
   if (Usage3) { /* third usage */
      SM = SUMA_GetStandardMap (SUMA_StandardMapCode(StdType));
      if (SM == NULL) {
         fprintf (SUMA_STDERR,"Error %s: Error in SUMA_MakeColorMap.\n", FuncName);
         exit(1);
      }
      Ncols = SM->N_Col;
   }
   
   M = SM->M;

   if (AfniHex && Ncols > 200) {
         fprintf (SUMA_STDERR,"Error %s: Cannot write a colormap of more than 200 colors in Afni's hex format.\n", FuncName);
         exit(1);
      }

   
   if (!AfniHex) 
         SUMA_disp_mat (M, Ncols, 3, 1);
   else {
         fprintf (stdout, "\n***COLORS\n");
         
         for (i=0; i < Ncols; ++i) {
            /* Now create the hex form */
            r_sprintf_long_to_hex (h, (unsigned long)rint((M[i][0]*255)), 1, 0);
            if (i<10) fprintf (stdout, "%s_0%d = #%s", Prfx, i, h);
               else fprintf (stdout, "%s_%d = #%s", Prfx, i, h); 

            r_sprintf_long_to_hex (h, (unsigned long)rint((M[i][1]*255)), 1, 0);
            fprintf (stdout, "%s", h);

            r_sprintf_long_to_hex (h, (unsigned long)rint((M[i][2]*255)), 1, 0);
            fprintf (stdout, "%s\n", h);
         }
         
         /* color map */
         
         fprintf (stdout, "\n***PALETTES %s [%d]\n//1 to -1 range\n", Prfx, Ncols);
         ifact = 2;
         for (i=0; i < Ncols; ++i) {
            fprintf (stdout, "%f -> ", 1.0 - (float)(ifact*i)/Ncols);
            if (i<10) fprintf (stdout, "%s_0%d\n", Prfx, i);
               else fprintf (stdout, "%s_%d\n", Prfx, i); 
         }
         fprintf (stdout, "\n***PALETTES %s [%d+]\n//1 to 0 range\n", Prfx, Ncols);
         ifact = 1;
         for (i=0; i < Ncols; ++i) {
            fprintf (stdout, "%f -> ", 1.0 - (float)(ifact*i)/Ncols);
            if (i<10) fprintf (stdout, "%s_0%d\n", Prfx, i);
               else fprintf (stdout, "%s_%d\n", Prfx, i); 
         }
   }
   
   /* free allocated space */
   if (Usage1)  {
      if (Fid) SUMA_free2D((char **)Fid, N_Fid / 3);
   } else {
      if (Fid) SUMA_free2D((char **)Fid, N_Fid / 4);
      if (Nind) SUMA_free(Nind);
   }
   if (SM) SUMA_Free_ColorMap(SM);
   
   exit (0);
}   
#endif

#ifdef STOPPED_HERE

SEE afni_setup.c file for reading in colormaps from files:
color definitions are read in starting at line 93 
      if( strcmp(str,"***COLORS") == 0 ){
palette definitions are read in starting at line 146
      if( strcmp(str,"***PALETTES") == 0 ){  /* loop, looking for palettes */

#endif

SUMA_Boolean SUMA_ScaleToMap_alaAFNI (float *V, int N_V, float range, SUMA_COLOR_MAP *ColMap, SUMA_SCALE_TO_MAP_OPT *Opt, SUMA_COLOR_SCALED_VECT * SV)
{
   static char FuncName[]={"SUMA_ScaleToMap_alaAFNI"};
   int i,j, i0, i1, mxColindex;
   float Vmin, Vmax, MinCol, MaxCol, Vrange, Vscl, r;
   SUMA_Boolean NewMap = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

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
   
   /* find the values to be masked out */
   if (Opt->ApplyMask){
      for (i=0; i < N_V; ++i) {
         if (V[i] >= Opt->MaskRange[0] && V[i] <= Opt->MaskRange[1]) {
            SV->isMasked[i] = YUP;
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
   if (ColMap->frac) {
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
   }else {
      SUMA_LH("NO Linearizing of colormap deemed necessary...");
      NewMap = NOPE;
   }
   
   /* if brightness factor is given, apply it to color map and mask color */
   if (Opt->BrightFact <= 0 || Opt->BrightFact > 1) {
      fprintf (SUMA_STDERR,"Error %s: Opt->BrightFact must be between ]0 1]\n", FuncName);
      SUMA_RETURN (NOPE);
   }else {
      if (Opt->BrightFact != 1) {
         for (i=0; i < ColMap->N_Col; ++i) {
            ColMap->M[i][0] *= Opt->BrightFact;
            ColMap->M[i][1] *= Opt->BrightFact;
            ColMap->M[i][2] *= Opt->BrightFact;
         }
         /* now for the mask color */
         Opt->MaskColor[0] *= Opt->BrightFact;
         Opt->MaskColor[1] *= Opt->BrightFact;
         Opt->MaskColor[2] *= Opt->BrightFact;
      }
   }
   
   if (Opt->interpmode != SUMA_DIRECT && Opt->interpmode != SUMA_NO_INTERP && Opt->interpmode != SUMA_INTERP) {
      fprintf (SUMA_STDERR,"Error %s: Opt->interpmode is incorrectly specifed (%d).\n", FuncName, Opt->interpmode);
      SUMA_RETURN(NOPE);
   }
   
   if (Opt->interpmode == SUMA_INTERP || Opt->interpmode == SUMA_NO_INTERP) {
      /* Now go through values and interpolate onto index of colormap */
      MinCol = 0.0; MaxCol = (float)ColMap->N_Col; 
      Vrange = Vmax - Vmin; 
      if (LocalHead) fprintf(SUMA_STDERR,"%s: [Vrange, Vmax, Vmin] = [%f, %f, %f]\nInterpMode=%d\n", 
                                          FuncName, Vrange, Vmax, Vmin, Opt->interpmode);
      if (Vrange < 0) {
         fprintf (SUMA_STDERR,"Error %s: Vmax < Vmin.\n", FuncName);
         SUMA_RETURN (NOPE);
      }

      if (Vrange > 0) {
         mxColindex = ColMap->N_Col -1; 
         if (Opt->interpmode == SUMA_NO_INTERP) {
               SUMA_LH("No_Interp mode");
               for (i=0; i < N_V; ++i) {
                  if (!SV->isMasked[i]) {
                     Vscl = (V[i] - Vmin) / Vrange * ColMap->N_Col; /* used mxColindex instead of N_Col (wrong!) prior to Oct 22, 03 */
                     i0 = (int)(Vscl); 
                     if (i0 > mxColindex) i0 = mxColindex; 

                     if (ColMap->M[i0][0] >= 0) { /* good color */
                        SV->cM[i][0] = ColMap->M[i0][0];
                        SV->cM[i][1] = ColMap->M[i0][1];
                        SV->cM[i][2] = ColMap->M[i0][2];
                     } else { /* mask color */
                        SV->isMasked[i] = YUP;
                        SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2];
                     }
                  } else {
                     SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2]; 
                  }
               }
            } else { 
               SUMA_LH("Interp mode");
               for (i=0; i < N_V; ++i) {
                  if (!SV->isMasked[i]) {
                     Vscl = (V[i] - Vmin) / Vrange * ColMap->N_Col; /* used mxColindex instead of N_Col (wrong!) prior to Oct 22, 03 */ 
                     /*now linearly interpolate between the two closest colors in the color map */
                     i0 = (int)(Vscl); 
                     if (i0 > mxColindex) i0 = mxColindex; 
                     i1=i0+1;

                     if (ColMap->M[i0][0] >= 0) { /* good color */
                        if (i1 < ColMap->N_Col) {
                           r = Vscl - i0; 
                           /*fprintf (SUMA_STDERR,"i0 = %d, i1 = %d, Vscl = %f, r= %f Col[i0] = %f %f %f\n", \
                              i0, i1, Vscl, r, ColMap->M[i0][0], ColMap->M[i0][1], ColMap->M[i0][2]);*/

                           SV->cM[i][0] = ColMap->M[i0][0] + r * (ColMap->M[i1][0] - ColMap->M[i0][0]);
                           SV->cM[i][1] = ColMap->M[i0][1] + r * (ColMap->M[i1][1] - ColMap->M[i0][1]);
                           SV->cM[i][2] = ColMap->M[i0][2] + r * (ColMap->M[i1][2] - ColMap->M[i0][2]);
                        } else {
                           SV->cM[i][0] = ColMap->M[i0][0];
                           SV->cM[i][1] = ColMap->M[i0][1];
                           SV->cM[i][2] = ColMap->M[i0][2];
                        }
                     } else { /* mask color */
                        SV->isMasked[i] = YUP;
                        SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2];
                     }
                  } else {
                     SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2]; 
                  }
               }
            }
      } else { /* all values are equal, use the middle color in the colormap */
         fprintf (SUMA_STDOUT,"Warning %s: Node value range is 0, using middle color in colormap.\n", FuncName);
         i0 = (ColMap->N_Col - 1)/2;
         for (i=0; i < N_V; ++i) {
            if (!SV->isMasked[i]) {
               if (ColMap->M[i0][0] >= 0) {
                  SV->cM[i][0] = ColMap->M[i0][0];
                  SV->cM[i][1] = ColMap->M[i0][1];
                  SV->cM[i][2] = ColMap->M[i0][2];
               } else {
                  SV->isMasked[i] = YUP;
                  SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2]; 
               }
            } else {
               SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2]; 
            }
         }
      }
   } else {
      /* direct color mapping */
      SUMA_LH("Direct colormapping");
      if (Opt->interpmode != SUMA_DIRECT) {
         fprintf (SUMA_STDOUT,"Error %s: Logic error, should never get here with Opt->interpmode != SUMA_DIRECT\n", FuncName);
         SUMA_RETURN(NOPE);
      }
      for (i=0; i < N_V; ++i) {
         if (!SV->isMasked[i]) {
            i0 = (int)V[i]; 
            if (i0 < 0) i0 = 0;
            else if (i0 >= ColMap->N_Col) i0 = ColMap->N_Col -1;
            if (ColMap->M[i0][0] >= 0) {
               SV->cM[i][0] = ColMap->M[i0][0];
               SV->cM[i][1] = ColMap->M[i0][1];
               SV->cM[i][2] = ColMap->M[i0][2];
            } else {
               SV->isMasked[i] = YUP;
               SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2];
            }
         } else {
            SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2]; 
         }

      }
   }
   if (NewMap) {
      SUMA_LH("Freeing linearized colormap.");
      SUMA_Free_ColorMap (ColMap); 
   }
   
   SUMA_RETURN (YUP);
   
}

/*!
   This function maps the values in a vector to colors on a color map
   Res = SUMA_ScaleToMap (V, N_V, Vmin, Vmax, ColMap, Opt, SV); 
   \param V (float *) N_V x1  vector containing the values to be colorized by the map in ColMap. 
   \param N_V (int) number of elements in V
   \param Vmin (float) minimum value in V
   \param Vmax (float) maximum value in V
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
SUMA_Boolean SUMA_ScaleToMap (float *V, int N_V, float Vmin, float Vmax, SUMA_COLOR_MAP *ColMap, SUMA_SCALE_TO_MAP_OPT *Opt, SUMA_COLOR_SCALED_VECT * SV)
{
   static char FuncName[]={"SUMA_ScaleToMap"};
   int i,j, i0, i1, mxColindex;
   float MinCol, MaxCol, Vrange, Vscl, r;
   SUMA_Boolean NewMap = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   
   /* No negative colormaps here */
   if (ColMap->Sgn < 0) {
      SUMA_S_Err("Colormap is split into positive and negative.\n No support for this feature in SUMA_ScaleToMap.\nTry SUMA_ScaleToMap_alaAFNI");
      SUMA_RETURN(NOPE);
   }
   
   /* find the values to be masked out */
   if (Opt->ApplyMask){
      for (i=0; i < N_V; ++i) {
         if (V[i] >= Opt->MaskRange[0] && V[i] <= Opt->MaskRange[1]) {
            SV->isMasked[i] = YUP;
         } 
      }
   }

   /* go through and clip values in V to those specified in the range */
   if (Opt->ApplyClip) {
      for (i=0; i < N_V; ++i) {
         if (!SV->isMasked[i]) { /* don't waist time on masked stuff */
            if (V[i] > Opt->ClipRange[0]) { 
               /* that's cool */
            } else {
               V[i] = Opt->ClipRange[0];
            }

            if (V[i] < Opt->ClipRange[1]) { 
               /* that's cool */
            } else {
               V[i] = Opt->ClipRange[1];
            }
         } 
      }
      Vmin = Opt->ClipRange[0];
      Vmax = Opt->ClipRange[1];
   }
   
   /* is the colormap non-linear ? */
   if (ColMap->frac) {
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
   }else {
      SUMA_LH("NO Linearizing of colormap deemed necessary...");
      NewMap = NOPE;
   }
   
   /* if brightness factor is given, apply it to color map and mask color */
   if (Opt->BrightFact <= 0 || Opt->BrightFact > 1) {
      fprintf (SUMA_STDERR,"Error %s: Opt->BrightFact must be between ]0 1]\n", FuncName);
      SUMA_RETURN (NOPE);
   }else {
      if (Opt->BrightFact != 1) {
         for (i=0; i < ColMap->N_Col; ++i) {
            ColMap->M[i][0] *= Opt->BrightFact;
            ColMap->M[i][1] *= Opt->BrightFact;
            ColMap->M[i][2] *= Opt->BrightFact;
         }
         /* now for the mask color */
         Opt->MaskColor[0] *= Opt->BrightFact;
         Opt->MaskColor[1] *= Opt->BrightFact;
         Opt->MaskColor[2] *= Opt->BrightFact;
      }
   }
   
   
   if (Opt->interpmode != SUMA_DIRECT && Opt->interpmode != SUMA_NO_INTERP && Opt->interpmode != SUMA_INTERP) {
      fprintf (SUMA_STDERR,"Error %s: Opt->interpmode is incorrectly specifed (%d).\n", FuncName, Opt->interpmode);
      SUMA_RETURN(NOPE);
   }
   
   if (Opt->interpmode == SUMA_NO_INTERP || Opt->interpmode == SUMA_INTERP) {
      /* Now go through values and interpolate onto index of colormap */
      MinCol = 0.0; MaxCol = (float)ColMap->N_Col; 
      Vrange = Vmax - Vmin; 
      if (Vrange < 0) {
         fprintf (SUMA_STDERR,"Error %s: Vmax < Vmin.\n", FuncName);
         SUMA_RETURN (NOPE);
      }

      if (Vrange > 0) {
         mxColindex = ColMap->N_Col -1; 
         if (Opt->interpmode == SUMA_NO_INTERP) { /* no interpolation between colours */
            SUMA_LH("No Interp Mode");
            for (i=0; i < N_V; ++i) {
               if (!SV->isMasked[i]) {
                  Vscl = (V[i] - Vmin) / Vrange * ColMap->N_Col; /* used mxColindex instead of N_Col (wrong!) prior to Oct 22, 03 */
                  i0 = (int)(Vscl); 
                  if (i0 > mxColindex) i0 = mxColindex; 

                  if (ColMap->M[i0][0] >= 0) { /* good color */
                     SV->cM[i][0] = ColMap->M[i0][0];
                     SV->cM[i][1] = ColMap->M[i0][1];
                     SV->cM[i][2] = ColMap->M[i0][2];
                  } else { /* mask color */
                     SV->isMasked[i] = YUP;
                     SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2];
                  }
               } else {
                  SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2]; 
               }
            }
         } else { /* interpolation mode */
            SUMA_LH("Interp Mode");
            for (i=0; i < N_V; ++i) {
               if (!SV->isMasked[i]) {
                  Vscl = (V[i] - Vmin) / Vrange * ColMap->N_Col; /* used mxColindex instead of N_Col (wrong!) prior to Oct 22, 03 */ 
                  /*now linearly interpolate between the two closest colors in the color map */
                  i0 = (int)(Vscl); 
                  if (i0 > mxColindex) i0 = mxColindex; 
                  i1=i0+1;

                  if (ColMap->M[i0][0] >= 0) { /* good color */
                     if (i1 < ColMap->N_Col) {
                        r = Vscl - i0; 
                        /*fprintf (SUMA_STDERR,"i0 = %d, i1 = %d, Vscl = %f, r= %f Col[i0] = %f %f %f\n", \
                           i0, i1, Vscl, r, ColMap->M[i0][0], ColMap->M[i0][1], ColMap->M[i0][2]);*/

                        SV->cM[i][0] = ColMap->M[i0][0] + r * (ColMap->M[i1][0] - ColMap->M[i0][0]);
                        SV->cM[i][1] = ColMap->M[i0][1] + r * (ColMap->M[i1][1] - ColMap->M[i0][1]);
                        SV->cM[i][2] = ColMap->M[i0][2] + r * (ColMap->M[i1][2] - ColMap->M[i0][2]);
                     } else {
                        SV->cM[i][0] = ColMap->M[i0][0];
                        SV->cM[i][1] = ColMap->M[i0][1];
                        SV->cM[i][2] = ColMap->M[i0][2];
                     }
                  } else { /* mask color */
                     SV->isMasked[i] = YUP;
                     SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2];
                  }
               } else {
                  SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2]; 
               }
            }
         }
      }else { /* all values are equal, use the middle color in the colormap */
         fprintf (SUMA_STDOUT,"Warning %s: Node value range is 0, using middle color in colormap.\n", FuncName);
         i0 = (ColMap->N_Col - 1)/2;
         for (i=0; i < N_V; ++i) {
            if (!SV->isMasked[i]) {
               SV->cM[i][0] = ColMap->M[i0][0];
               SV->cM[i][1] = ColMap->M[i0][1];
               SV->cM[i][2] = ColMap->M[i0][2];
            } else {
               SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2]; 
            }
         }
      }
   } else {
      /* direct color mapping */
      SUMA_LH("Direct colormapping");
      if (Opt->interpmode != SUMA_DIRECT) {
         fprintf (SUMA_STDOUT,"Error %s: Logic error, should never get here with Opt->interpmode != SUMA_DIRECT\n", FuncName);
         SUMA_RETURN(NOPE);
      }
      for (i=0; i < N_V; ++i) {
         if (!SV->isMasked[i]) {
            i0 = (int)V[i]; 
            if (i0 < 0) i0 = 0;
            else if (i0 >= ColMap->N_Col) i0 = ColMap->N_Col -1;
            if (ColMap->M[i0][0] >= 0) {
               SV->cM[i][0] = ColMap->M[i0][0];
               SV->cM[i][1] = ColMap->M[i0][1];
               SV->cM[i][2] = ColMap->M[i0][2];
            } else {
               SV->isMasked[i] = YUP;
               SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2];
            }
         } else {
            SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2]; 
         }

      }
   
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
   \ret S (SUMA_COLOR_SCALED_VECT * ) pointer to structure that will contain the color map of N_Node nodes
*/
SUMA_COLOR_SCALED_VECT * SUMA_Create_ColorScaledVect(int N_Node)
{
   static char FuncName[]={"SUMA_Create_ColorScaledVect"};
   SUMA_COLOR_SCALED_VECT * S;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   S = (SUMA_COLOR_SCALED_VECT *)SUMA_malloc(sizeof(SUMA_COLOR_SCALED_VECT));
   if (S == NULL) {
      fprintf(SUMA_STDERR, "Error %s: Failed to allocate for S.\n", FuncName);
      SUMA_RETURN (S);
   }
   
   S->cM = (float **) SUMA_allocate2D(N_Node, 3, sizeof(float));
   S->isMasked = (SUMA_Boolean *)SUMA_calloc(N_Node, sizeof(SUMA_Boolean));
   
   if (!S->cM || !S->isMasked) {
      fprintf(SUMA_STDERR, "Error %s: Failed to allocate for S->cM or S->isMasked.\n", FuncName);
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
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   if (S->cM) SUMA_free2D((char **)S->cM, S->N_Node);
   if (S->isMasked) SUMA_free(S->isMasked);
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
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   Opt = (SUMA_SCALE_TO_MAP_OPT *)SUMA_malloc(sizeof(SUMA_SCALE_TO_MAP_OPT));
   
   if (Opt == NULL) {
      fprintf (SUMA_STDERR, "Error %s: Could not allocate for Opt.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   Opt->ApplyMask = NOPE;
   Opt->MaskRange[0] = Opt->MaskRange[1] = 0.0;
   Opt->MaskColor[0] = Opt->MaskColor[1] = Opt->MaskColor[2] = 0.0; 
   Opt->ApplyClip = NOPE;
   Opt->ClipRange[0] = Opt->ClipRange[1] = 0.0;
   Opt->BrightFact = 1.0;
   Opt->interpmode = SUMA_INTERP;
   
   SUMA_RETURN (Opt);

}

/*!
   \brief Returns the ascii name of a Suma standard map.
   
   \param mapcode (SUMA_STANDARD_CMAP)
   \param N_col (int *) to contain the number of colors in the map
         -1 if no map was found
   \return ans (char *) ascii version of mapcode
   
   \sa SUMA_StandardMapCode
*/ 
char *SUMA_StandardMapName (SUMA_STANDARD_CMAP mapcode, int *N_col)
{
   static char FuncName[]={"SUMA_StandardMapName"};
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }
   
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
         SUMA_RETURN("matlab_default_byr64");
         break;
      case SUMA_CMAP_BGYR64:
         *N_col = 64;
         SUMA_RETURN("bgyr64");
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
         SUMA_RETURN("Cowabonga");
         break;
   }
}

/*!
   \brief Returns the code corresponding to a colormap name
   
   \sa SUMA_StandardMapName
*/
SUMA_STANDARD_CMAP SUMA_StandardMapCode (char *Name)
{
   static char FuncName[]={"SUMA_StandardMapCode"};
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }
   
   if (!Name) SUMA_RETURN(SUMA_CMAP_ERROR);
   if (!strcmp(Name, "Undefined")) SUMA_RETURN(SUMA_CMAP_UNDEFINED);
   if (!strcmp(Name, "rgybr20")) SUMA_RETURN(SUMA_CMAP_RGYBR20);
   if (!strcmp(Name, "ngray20")) SUMA_RETURN(SUMA_CMAP_nGRAY20);
   if (!strcmp(Name, "gray20")) SUMA_RETURN(SUMA_CMAP_GRAY20);
   if (!strcmp(Name, "bw20")) SUMA_RETURN(SUMA_CMAP_BW20);
   if (!strcmp(Name, "bgyr19")) SUMA_RETURN(SUMA_CMAP_BGYR19);
   if (!strcmp(Name, "matlab_default_byr64")) SUMA_RETURN(SUMA_CMAP_MATLAB_DEF_BYR64);
   if (!strcmp(Name, "bgyr64")) SUMA_RETURN(SUMA_CMAP_BGYR64);
   if (!strcmp(Name, "roi64")) SUMA_RETURN(SUMA_CMAP_ROI64);
   if (!strcmp(Name, "roi128")) SUMA_RETURN(SUMA_CMAP_ROI128);
   /* if (!strcmp(Name, "")) SUMA_RETURN(); */
   SUMA_RETURN(SUMA_CMAP_ERROR);
}

/*! 
   Returns one of a bunch of standard SUMA colormaps
   CM = SUMA_GetStandardMap (mapname);
   
   \param mapname (SUMA_STANDARD_CMAP) type of color map, choose from  
      SUMA_CMAP_RGYBR20
      SUMA_CMAP_BGYR19
      SUMA_CMAP_GRAY20
      SUMA_CMAP_nGRAY20
      SUMA_CMAP_BW20
      SUMA_CMAP_MATLAB_DEF_BYR64
   \return CM (SUMA_COLOR_MAP*) color map structure (NULL in case of error)
*/

SUMA_COLOR_MAP * SUMA_GetStandardMap (SUMA_STANDARD_CMAP mapcode)
{     static char FuncName[]={"SUMA_GetStandardMap"};
      float **Fiducials;
      int k, nc;
      int *Nind;
      int Ncols, NFid;
      SUMA_COLOR_MAP * CM;
      
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

      switch (mapcode) {
         case SUMA_CMAP_RGYBR20:
            {               
               Fiducials = (float **)SUMA_allocate2D(5, 3, sizeof(float));
               if (!Fiducials) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to allocate for Fiducials.\n", FuncName);
                  SUMA_RETURN (NULL);
               }
               /* create the fiducial colors */
               k = 0;
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; ++k;/* Red */
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; ++k;/* Green */
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 1.0; ++k;/* Blue */
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; ++k;/* Yellow */
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; ++k;/* Red */

               /* generate 20 colors colormap */
               CM = SUMA_MakeColorMap (Fiducials, k, 20, YUP, SUMA_StandardMapName(mapcode,&nc));
               /* free Fiducials */
               SUMA_free2D((char **)Fiducials, k);
               
               if (!CM) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
                  SUMA_RETURN (NULL);   
               }
               break;
            }
         case SUMA_CMAP_BGYR19:
            {
               Fiducials = (float **)SUMA_allocate2D(4, 3, sizeof(float));
               if (!Fiducials) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to allocate for Fiducials.\n", FuncName);
                  SUMA_RETURN (NULL);
               }
               /* create the fiducial colors */
               k = 0;
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 1.0; ++k;/* Blue */
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; ++k;/* Green */
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; ++k;/* Yellow */
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; ++k;/* Red */

               /* generate 20 colors colormap */
               CM = SUMA_MakeColorMap (Fiducials, k, 19, NOPE, SUMA_StandardMapName(mapcode,&nc));
               /* free Fiducials */
               SUMA_free2D((char **)Fiducials, k);
               
               if (!CM) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
                  SUMA_RETURN (NULL);   
               }
               break;
            }
                     
         case SUMA_CMAP_GRAY20:
            {
               Fiducials = (float **)SUMA_allocate2D(2, 3, sizeof(float));
               if (!Fiducials) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to allocate for Fiducials.\n", FuncName);
                  SUMA_RETURN (NULL);
               }
               /* create the fiducial colors */
               k = 0;
               Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.3; ++k;/* 0.3 gray */
               Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.8; ++k;/* 0.8 gray */

               /* generate 20 colors colormap */
               CM = SUMA_MakeColorMap (Fiducials, k, 20, NOPE, SUMA_StandardMapName(mapcode,&nc));
               /* free Fiducials */
               SUMA_free2D((char **)Fiducials, k);
               
               if (!CM) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
                  SUMA_RETURN (NULL);   
               }
               break;
            }
            
         case SUMA_CMAP_nGRAY20:
            {
               Fiducials = (float **)SUMA_allocate2D(2, 3, sizeof(float));
               if (!Fiducials) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to allocate for Fiducials.\n", FuncName);
                  SUMA_RETURN (NULL);
               }
               /* create the fiducial colors */
               k = 0;
               Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.8; ++k;/* 0.8 gray */
               Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.3; ++k;/* 0.3 gray */

               /* generate 20 colors colormap */
               CM = SUMA_MakeColorMap (Fiducials, k, 20, NOPE, SUMA_StandardMapName(mapcode,&nc));
               /* free Fiducials */
               SUMA_free2D((char **)Fiducials, k);
               
               if (!CM) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
                  SUMA_RETURN (NULL);   
               }
               break;
            }
            
         case SUMA_CMAP_BW20:
            {
               Fiducials = (float **)SUMA_allocate2D(2, 3, sizeof(float));
               if (!Fiducials) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to allocate for Fiducials.\n", FuncName);
                  SUMA_RETURN (NULL);
               }
               /* create the fiducial colors */
               k = 0;
               Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 0.0; ++k;/* black  */
               Fiducials[k][0] = Fiducials[k][1] = Fiducials[k][2] = 1.0; ++k;/* white */

               /* generate 20 colors colormap */
               CM = SUMA_MakeColorMap (Fiducials, k, 20, NOPE, SUMA_StandardMapName(mapcode,&nc));
               /* free Fiducials */
               SUMA_free2D((char **)Fiducials, k);
               
               if (!CM) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
                  SUMA_RETURN (NULL);   
               }
               break;
            }
            case SUMA_CMAP_MATLAB_DEF_BYR64:
            {
               /* default matlab color map */
               Ncols = 64;
               NFid = 10;
               
               Fiducials = (float **)SUMA_allocate2D(NFid, 3, sizeof(float));
               Nind = (int *) SUMA_calloc (NFid, sizeof (int));
               
               if (!Fiducials || !Nind) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to allocate for Fiducials or Nind.\n", FuncName);
                  SUMA_RETURN (NULL);
               }
               
               /* create the fiducial colors */
               k = 0;
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.5625; Nind[k] = 0; ++k; 
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 1.0; Nind[k] = 7; ++k;
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.5; Fiducials[k][2] = 1.0; Nind[k] = 15; ++k;
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 1.0; Nind[k] = 23; ++k;
               Fiducials[k][0] = 0.5; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.5625; Nind[k] = 31; ++k;
               Fiducials[k][0] = 0.5625; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.5; Nind[k] = 32; ++k;
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; Nind[k] = 40; ++k;
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.5; Fiducials[k][2] = 0.0; Nind[k] = 48; ++k;
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; Nind[k] = 56; ++k;
               Fiducials[k][0] = 0.5625; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; Nind[k] = 63; ++k;
               
               /* generate 20 colors colormap */
               CM = SUMA_MakeColorMap_v2 (Fiducials, k, Nind, NOPE, SUMA_StandardMapName(mapcode,&nc));
               
               /* free Fiducials & Nind*/
               SUMA_free2D((char **)Fiducials, k);
               SUMA_free(Nind);
               
               if (!CM) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
                  SUMA_RETURN (NULL);   
               }
               break;
            
            }
         
            case SUMA_CMAP_BGYR64:
            {
               /* default matlab color map */
               Ncols = 64;
               NFid = 10;
               
               Fiducials = (float **)SUMA_allocate2D(NFid, 3, sizeof(float));
               Nind = (int *) SUMA_calloc (NFid, sizeof (int));
               
               if (!Fiducials || !Nind) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to allocate for Fiducials or Nind.\n", FuncName);
                  SUMA_RETURN (NULL);
               }
               
               /* create the fiducial colors */
               k = 0;
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.5625; Nind[k] = 0; ++k; 
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 1.0; Nind[k] = 7; ++k;
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.5; Fiducials[k][2] = 1.0; Nind[k] = 15; ++k;
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 1.0; Nind[k] = 18; ++k;
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 0.5; Fiducials[k][2] = 0.0; Nind[k] = 24; ++k;
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; Nind[k] = 32; ++k;
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0.0; Nind[k] = 43; ++k;
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.5; Fiducials[k][2] = 0.0; Nind[k] = 48; ++k;
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; Nind[k] = 56; ++k;
               Fiducials[k][0] = 0.5625; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; Nind[k] = 63; ++k;
               
               /* generate 64 colors colormap */
               CM = SUMA_MakeColorMap_v2 (Fiducials, k, Nind, NOPE, SUMA_StandardMapName(mapcode,&nc));
               
               /* free Fiducials & Nind*/
               SUMA_free2D((char **)Fiducials, k);
               SUMA_free(Nind);
               
               if (!CM) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
                  SUMA_RETURN (NULL);   
               }
               break;
            
            }

         case SUMA_CMAP_ROI128:
            {
               /* a large colormap for lots of ROI drawing */
               Ncols = 128;
               NFid = 6;
               
               Fiducials = (float **)SUMA_allocate2D(NFid, 3, sizeof(float));
               Nind = (int *) SUMA_calloc (NFid, sizeof (int));
               
               if (!Fiducials || !Nind) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to allocate for Fiducials or Nind.\n", FuncName);
                  SUMA_RETURN (NULL);
               }
               
               /* create the fiducial colors */
               k = 0;
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0; Nind[k] = 0; ++k; 
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0; Nind[k] = 25; ++k;
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 0; Fiducials[k][2] = 1.0; Nind[k] = 50; ++k;
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; Nind[k] = 75; ++k;
               Fiducials[k][0] = 0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 1; Nind[k] = 100; ++k;
               Fiducials[k][0] = 1; Fiducials[k][1] = 0; Fiducials[k][2] = 1; Nind[k] = 127; ++k;
               
               
               /* generate colormap */
               CM = SUMA_MakeColorMap_v2 (Fiducials, k, Nind, NOPE, SUMA_StandardMapName(mapcode,&nc));
               
               /* free Fiducials & Nind*/
               SUMA_free2D((char **)Fiducials, k);
               SUMA_free(Nind);
               
               if (!CM) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
                  SUMA_RETURN (NULL);   
               }
               break;
            
            }
         
         case SUMA_CMAP_ROI64:
            {
               /* a large colormap for lots of ROI drawing */
               Ncols = 64;
               NFid = 6;
               
               Fiducials = (float **)SUMA_allocate2D(NFid, 3, sizeof(float));
               Nind = (int *) SUMA_calloc (NFid, sizeof (int));
               
               if (!Fiducials || !Nind) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to allocate for Fiducials or Nind.\n", FuncName);
                  SUMA_RETURN (NULL);
               }
               
               /* create the fiducial colors */
               k = 0;
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0; Nind[k] = 0; ++k; 
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 0; Nind[k] = 12; ++k;
               Fiducials[k][0] = 0.0; Fiducials[k][1] = 0; Fiducials[k][2] = 1.0; Nind[k] = 25; ++k;
               Fiducials[k][0] = 1.0; Fiducials[k][1] = 0.0; Fiducials[k][2] = 0.0; Nind[k] = 33; ++k;
               Fiducials[k][0] = 0; Fiducials[k][1] = 1.0; Fiducials[k][2] = 1; Nind[k] = 50; ++k;
               Fiducials[k][0] = 1; Fiducials[k][1] = 0; Fiducials[k][2] = 1; Nind[k] = 63; ++k;
               
               
               /* generate colormap */
               CM = SUMA_MakeColorMap_v2 (Fiducials, k, Nind, NOPE, SUMA_StandardMapName(mapcode,&nc));
               
               /* free Fiducials & Nind*/
               SUMA_free2D((char **)Fiducials, k);
               SUMA_free(Nind);
               
               if (!CM) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
                  SUMA_RETURN (NULL);   
               }
               break;
            
            }
            
         default:
            fprintf (SUMA_STDERR,"Error %s: Unrecognized color map name.\n", FuncName);
            SUMA_RETURN (NULL);
      
      }
      
      SUMA_RETURN (CM);
   }

#ifdef SUMA_ScaleToMap_STAND_ALONE
   void SUMA_ScaleToMap_usage ()
   {
      fprintf (SUMA_STDOUT,   "\n\33[1mUsage: \33[0m ScaleToMap <-input IntFile icol vcol>  \n"
                              "\t [-cmap MapType] [-cmapfile Mapfile] [-cmapdb Palfile] [-frf] \n"
                              "\t [-clp/-perc_clp clp0 clp1] [-apr/-anr range]\n"
                              "\t [-interp/-nointerp/-direct] [-msk msk0 msk1] [-nomsk_col]\n"
                              "\t [-msk_col R G B] [-br BrightFact]\n"
                              "\t [-h/-help] [-verb] [-showmap] [-showdb]\n\n");
      fprintf (SUMA_STDOUT,   "\t -input IntFile icol vcol: input data.\n"
                              "\t    Infile: 1D formatted ascii file containing node values\n"
                              "\t    icol: index of node index column \n"
                              "\t    (-1 if the node index is implicit)\n"
                              "\t    vcol: index of node value column.\n"
                              "\t    Example: -input ValOnly.1D -1 0 \n"
                              "\t    for a 1D file containing node values\n"
                              "\t    in the first column and no node indices.\n"
                              "\t    Example: -input NodeVal.1D 1 3\n"
                              "\t    for a 1D file containing node indices in\n"
                              "\t    the SECOND column and node values in the \n"
                              "\t    FOURTH column (index counting begins at 0)\n");
      fprintf (SUMA_STDOUT,   "\t -v and -iv options are now obsolete.\n"
                              "\t    Use -input option instead.\n");
      fprintf (SUMA_STDOUT,   "\t -cmap MapName: (optional, default RGYBR20) \n"
                              "\t    choose one of the standard colormaps available with SUMA:\n"
                              "\t    RGYBR20, BGYR19, BW20, GRAY20, MATLAB_DEF_BYR64, \n"
                              "\t    ROI64, ROI128\n"
                              "\t    You can also use AFNI's default paned color maps:\n"
                              "\t    The maps are labeled according to the number of \n"
                              "\t    panes and their sign. Example: afni_p10\n"
                              "\t    uses the positive 10-pane afni colormap.\n"
                              "\t    afni_n10 is the negative counterpart.\n"
                              "\t    These maps are meant to be used with\n"
                              "\t    the options -apr and -anr listed below.\n"
                              "\t    You can also load non-default AFNI colormaps\n"
                              "\t    from .pal files (AFNI's colormap format); see option\n"
                              "\t    -cmapdb below.\n");
      fprintf (SUMA_STDOUT,   "\t -cmapdb Palfile: read color maps from AFNI .pal file\n"
                              "\t    In addition to the default paned AFNI colormaps, you\n"
                              "\t    can load colormaps from a .pal file.\n");                   
      fprintf (SUMA_STDOUT,   "\t -cmapfile Mapfile: read color map from Mapfile.\n"
                              "\t    Mapfile:1D formatted ascii file containing colormap.\n"
                              "\t            each row defines a color in one of two ways:\n"
                              "\t            R  G  B        or\n"
                              "\t            R  G  B  f     \n"
                              "\t    where R, G, B specify the red, green and blue values, \n"
                              "\t    between 0 and 1 and f specifies the fraction of the range\n"
                              "\t    reached at this color. THINK values of right of AFNI colorbar.\n"
                              "\t    The use of fractions (it is optional) would allow you to create\n"
                              "\t    non-linear color maps where colors cover differing fractions of \n"
                              "\t    the data range.\n"
                              "\t    Sample colormap with positive range only (a la AFNI):\n"
                              "\t            0  0  1  1.0\n"
                              "\t            0  1  0  0.8\n"
                              "\t            1  0  0  0.6\n"
                              "\t            1  1  0  0.4\n"
                              "\t            0  1  1  0.2\n"
                              "\t    Note the order in which the colors and fractions are specified.\n"
                              "\t    The bottom color of the +ve colormap should be at the bottom of the\n"
                              "\t    file and have the lowest +ve fraction. The fractions here define a\n"
                              "\t    a linear map so they are not necessary but they illustrate the format\n"
                              "\t    of the colormaps.\n"
                              "\t    Comparable colormap with negative range included:\n"
                              "\t            0  0  1   1.0\n"
                              "\t            0  1  0   0.6\n"
                              "\t            1  0  0   0.2\n"
                              "\t            1  1  0  -0.2\n"
                              "\t            0  1  1  -0.6\n"
                              "\t    The bottom color of the -ve colormap should have the \n"
                              "\t    lowest -ve fraction. \n"
                              "\t    You can use -1 -1 -1 for a color to indicate a no color\n"
                              "\t    (like the 'none' color in AFNI). Values mapped to this\n"
                              "\t    'no color' will be masked as with the -msk option.\n"
                              "\t    If your 1D color file has more than three or 4 columns,\n"
                              "\t    you can use the [] convention adopted by AFNI programs\n"
                              "\t    to select the columns you need.\n");
      fprintf (SUMA_STDOUT,   "\t -frf: (optional) first row in file is the first color.\n"
                              "\t    As explained in the -cmapfile option above, the first \n"
                              "\t    or bottom (indexed 0 )color of the colormap should be \n"
                              "\t    at the bottom of the file. If the opposite is true, use\n"
                              "\t    the -frf option to signal that.\n"
                              "\t    This option is only useful with -cmapfile.\n");
      fprintf (SUMA_STDOUT,   "\t -clp/-perc_clp clp0 clp1: (optional, default no clipping)\n"
                              "\t    clips values in IntVect. if -clp is used then values in vcol\n"
                              "\t    < clp0 are clipped to clp0 and > clp1 are clipped to clp1\n");
      fprintf (SUMA_STDOUT,   "\t    if -perc_clp is used them vcol is clipped to the values \n"
                              "\t    corresponding to clp0 and clp1 percentile.\n"
                              "\t    The -clp/-prec_clp options are mutually exclusive with -apr/-anr.\n");
      fprintf (SUMA_STDOUT,   "\t -apr range: (optional) clips the values in IntVect to [0 range].\n"
                              "\t    This option allows range of colormap to be set as in AFNI, \n"
                              "\t    with Positive colorbar (Pos selected).\n"
                              "\t    This option is mutually exclusive with -clp/-perc_clp).\n"
                              "\t    set range = 0 for autoranging.\n"
                              "\t    If you use -apr and your colormap contains fractions, you\n"
                              "\t    must use a positive range colormap.\n");
      fprintf (SUMA_STDOUT,   "\t -anr range: (optional) clips the values in IntVect to [-range range].\n"
                              "\t    This option allows range of colormap to be set as in AFNI, \n"
                              "\t    with Negative colorbar (Pos NOT selected).\n"
                              "\t    This option is mutually exclusive with -clp/-perc_clp).\n"
                              "\t    set range = 0 for autoranging.\n"
                              "\t    If you use -anr and your colormap contains fractions, you\n"
                              "\t    must use a negative range colormap.\n");
      fprintf (SUMA_STDOUT,   "\t -interp: (default) use color interpolation between colors in colormap\n"
                              "\t    If a value is assigned between two colors on the colorbar,\n"
                              "\t    it receives a color that is an interpolation between those two colors.\n"
                              "\t    This is the default behaviour in SUMA and AFNI when using the continuous\n"
                              "\t    colorscale. Mutually exclusive with -nointerp and -direct options.\n");
      fprintf (SUMA_STDOUT,   "\t -nointerp: (optional) turns off color interpolation within the colormap\n"
                              "\t    Color assigniment is done a la AFNI when the paned colormaps are used.\n"
                              "\t    Mutually exclusive with -interp and -direct options.\n");
      fprintf (SUMA_STDOUT,   "\t -direct: (optional) values (typecast to integers) are mapped directly\n"
                              "\t    to index of color in color maps. Example: value 4 is assigned\n" 
                              "\t    to the 5th (index 4) color in the color map (same for values\n"
                              "\t    4.2 and 4.7). This mapping scheme is useful for ROI indexed type\n"
                              "\t    data. Negative data values are set to 0 and values >= N_col \n"
                              "\t    (the number of colors in the colormap) are set to N_col -1\n");
      fprintf (SUMA_STDOUT,   "\t -msk msk0 msk1: (optinal, default is no masking) \n"
                              "\t    Values in vcol (BEFORE clipping is performed) \n");
      fprintf (SUMA_STDOUT,   "\t    between [msk0 msk1] are masked by the masking color.\n");
      fprintf (SUMA_STDOUT,   "\t -msk_col R G B: (optional, default is 0.3 0.3 0.3) \n"
                              "\t    Sets the color of masked voxels.\n");
      fprintf (SUMA_STDOUT,   "\t -nomsk_col: do not output nodes that got masked.\n"
                              "\t    It does not make sense to use this option with\n"
                              "\t    -msk_col.\n");
      fprintf (SUMA_STDOUT,   "\t -br BrightFact: (optional, default is 1) \n"
                              "\t    Applies a brightness factor to the colors \n"
                              "\t    of the colormap and the mask color.\n");
      fprintf (SUMA_STDOUT,   "\t -h or -help: displays this help message.\n");
      fprintf (SUMA_STDOUT,   "\n");
      fprintf (SUMA_STDOUT,   "\tThe following options are for debugging and sanity checks.\n");
      fprintf (SUMA_STDOUT,   "\t -verb: (optional) verbose mode.\n");
      fprintf (SUMA_STDOUT,   "\t -showmap: (optional) print the colormap to the screen and quit.\n"
                              "\t    This option is for debugging and sanity checks.\n");
      fprintf (SUMA_STDOUT,   "\t -showdb: (optional) print the colors and colormaps of AFNI\n"
                              "\t    along with any loaded from the file Palfile.\n\n");
      fprintf (SUMA_STDOUT,   "\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \n"
                              "\t   July 31/02 Last Modified Nov 03 03\n\n");
   }

int main (int argc,char *argv[])
{/* Main */
   char FuncName[]={"ScaleToMap"}, *IntName = NULL, *Prfx, h[9], *CmapFileName = NULL, *dbfile = NULL, *MapName=NULL; 
   MRI_IMAGE *im = NULL;
   float *far=NULL;
   int N_V, N_Int, kar, k, ii, i, icol=-1, vcol=-1, Sgn, interpmode;
   int Vminloc, Vmaxloc, *iV = NULL;
   float Vmin, Vmax, brfact;
   float *V = NULL, *Vsort = NULL;
   float ClipRange[2], MaskColor[3], MaskRange[2], arange;
   SUMA_Boolean ApplyClip, ApplyMask, setMaskCol, ApplyPercClip, Vopt;
   SUMA_Boolean iVopt, inopt, NoMaskCol, MapSpecified, alaAFNI;
   SUMA_Boolean brk, frf, ShowMap, ShowMapdb;
   SUMA_COLOR_MAP *CM;
   SUMA_SCALE_TO_MAP_OPT * OptScl;
   SUMA_STANDARD_CMAP MapType;
   SUMA_COLOR_SCALED_VECT * SV;
   SUMA_AFNI_COLORS *SAC=NULL;
   SUMA_Boolean FromAFNI = NOPE;
   int imap, isPmap, isNmap;
   SUMA_Boolean LocalHead = NOPE;
   
   /* allocate space for CommonFields structure and initialize debug*/
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   
   SUMAg_CF->InOut_Notify = NOPE;

   
   /* this is placed down here to */
   /* 
   if (argc < 3) {
      SUMA_ScaleToMap_usage();
      exit (1);
   }
   */
   
   kar = 1;
   brfact = 1; /* the brightness factor */
   MaskColor[0] = MaskColor[1] = MaskColor[2] = 0.3;
   ApplyClip = NOPE;
   ApplyPercClip = NOPE;
   ApplyMask = NOPE;
   NoMaskCol = NOPE;
   setMaskCol = NOPE;
   Vopt = NOPE;
   iVopt = NOPE;
   inopt = NOPE;
   MapType = SUMA_CMAP_RGYBR20;
   brk = NOPE;
   MapSpecified = NOPE;
   CmapFileName = NULL;
   interpmode = SUMA_UNDEFINED_MODE;
   ShowMap = NOPE;
   alaAFNI = NOPE;   /* applying the alaAFNI mapping */
   frf = NOPE;
   arange  = -1.0; /* afni range specified */
   Sgn = 0;
   ShowMapdb = NOPE;
   while (kar < argc) { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
          SUMA_ScaleToMap_usage();
         exit (1);
      }
      
      if (strcmp(argv[kar], "-verb") == 0) {
         LocalHead = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
         if (kar+2 >= argc)  {
            fprintf (SUMA_STDERR, "need 3 arguments after -input \n");
            exit (1);
         }
         IntName = argv[kar]; kar ++;
         icol = atoi(argv[kar]); kar ++;
         vcol = atoi(argv[kar]); 
         inopt = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-apr") == 0)) {
         if (arange >= 0) {
            fprintf (SUMA_STDERR, "range has already been specified.\n");
            exit (1);
         }
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -apr \n");
            exit (1);
         }
         arange = atof(argv[kar]);
         if (arange < 0) {
            fprintf (SUMA_STDERR, "range must be positive.\n");
            exit (1);
         }
         Sgn = 1;
         alaAFNI = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-anr") == 0)) {
         if (arange >= 0) {
            fprintf (SUMA_STDERR, "range has already been specified.\n");
            exit (1);
         }
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -anr \n");
            exit (1);
         }
         arange = atof(argv[kar]);
         if (arange < 0) {
            fprintf (SUMA_STDERR, "range must be positive.\n");
            exit (1);
         }
         
         Sgn = -1;
         alaAFNI = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-v") == 0)) {
         fprintf (SUMA_STDERR, "\n -v option is now obsolete.\nUse -input option instead.\n");
         exit (1);
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -v \n");
            exit (1);
         }
         IntName = argv[kar];
         Vopt = YUP;
         brk = YUP;
      }      
      
      if (!brk && (strcmp(argv[kar], "-iv") == 0)) {
         fprintf (SUMA_STDERR, "\n -iv option is now obsolete.\nUse -input option instead.\n");
         exit (1);
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -iv \n");
            exit (1);
         }
         IntName = argv[kar];
         iVopt = YUP;
         brk = YUP;
      }   
      
      if (!brk && (strcmp(argv[kar], "-br") == 0)) {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -br \n");
            exit (1);
         }
         brfact = atof(argv[kar]);

         brk = YUP;
      }   
      
      if (!brk && (strcmp(argv[kar], "-frf") == 0)) {
         frf = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-showmap") == 0)) {
         ShowMap = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-showdb") == 0)) {
         ShowMapdb = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-nointerp") == 0)) {
         if (interpmode != SUMA_UNDEFINED_MODE) {
            fprintf (SUMA_STDERR, "Color interpolation mode already set.\n");
         }
         interpmode = SUMA_NO_INTERP;
         brk = YUP;
      } 
      
      if (!brk && (strcmp(argv[kar], "-direct") == 0)) {
         if (interpmode != SUMA_UNDEFINED_MODE) {
            fprintf (SUMA_STDERR, "Color interpolation mode already set.\n");
         }
         interpmode = SUMA_DIRECT;
         brk = YUP;
      } 
      
      if (!brk && (strcmp(argv[kar], "-interp") == 0)) {
         if (interpmode != SUMA_UNDEFINED_MODE) {
            fprintf (SUMA_STDERR, "Color interpolation mode already set.\n(-nointerp, -direct and -interp are mutually exclusive.\n");
         }
         interpmode = SUMA_INTERP;
         brk = YUP;
      } 
        
      if (!brk && (strcmp(argv[kar], "-clp") == 0)) {
         kar ++;
         if (kar+1 >= argc)  {
              fprintf (SUMA_STDERR, "need 2 arguments after -clp \n");
            exit (1);
         }
         ApplyClip = YUP;
         ClipRange[0] = atof(argv[kar]); kar ++;
         ClipRange[1] = atof(argv[kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-perc_clp") == 0)) {
         kar ++;
         if (kar+1 >= argc)  {
              fprintf (SUMA_STDERR, "need 2 arguments after -perc_clp ");
            exit (1);
         }
         ApplyPercClip = YUP;
         ClipRange[0] = atof(argv[kar]); kar ++;
         ClipRange[1] = atof(argv[kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-msk") == 0)) {
         kar ++;
         if (kar+1 >= argc)  {
              fprintf (SUMA_STDERR, "need 2 arguments after -msk ");
            exit (1);
         }
         ApplyMask = YUP;
         MaskRange[0] = atof(argv[kar]); kar ++;
         MaskRange[1] = atof(argv[kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-nomsk_col") == 0)) {
         kar ++;
         NoMaskCol = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-msk_col") == 0)) {
         kar ++;
         if (kar+2 >= argc)  {
              fprintf (SUMA_STDERR, "need 3 arguments after -msk_col ");
            exit (1);
         }
         setMaskCol = YUP;
         MaskColor[0] = atof(argv[kar]); kar ++;
         MaskColor[1] = atof(argv[kar]); kar ++;
         MaskColor[2] = atof(argv[kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-cmapfile") ==0)) {
         if (MapSpecified) {
            fprintf (SUMA_STDERR, "Color map already specified.\n-cmap and -cmapfile are mutually exclusive\n");
            exit (1);
         }
         MapSpecified = YUP;
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need 1 arguments after -cmapfile ");
            exit (1);
         }
         
         CmapFileName = argv[kar];
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-cmapdb") ==0)) {
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need 1 arguments after -cmapdb ");
            exit (1);
         }
         
         dbfile = argv[kar];
         brk = YUP;
      }
      
      
      if (!brk && (strcmp(argv[kar], "-cmap") ==0)) {
         if (MapSpecified) {
            fprintf (SUMA_STDERR, "Color map already specified.\n-cmap and -cmapfile are mutually exclusive\n");
            exit (1);
         }
         MapSpecified = YUP;
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need 1 arguments after -cmap ");
            exit (1);
         }
         MapName = argv[kar];
         MapType = SUMA_CMAP_UNDEFINED;
         if (strcmp(argv[kar], "RYGBR20") == 0)    MapType = SUMA_CMAP_RGYBR20;
         if (strcmp(argv[kar], "BW20") == 0)    MapType = SUMA_CMAP_BW20;
         if (strcmp(argv[kar], "GRAY20") == 0)    MapType = SUMA_CMAP_GRAY20;
         if (strcmp(argv[kar], "BGYR19") == 0)    MapType = SUMA_CMAP_BGYR19;
         if (strcmp(argv[kar], "MATLAB_DEF_BYR64") == 0)    MapType = SUMA_CMAP_MATLAB_DEF_BYR64;
         if (strcmp(argv[kar], "BGYR64") == 0)    MapType = SUMA_CMAP_BGYR64;
         if (strcmp(argv[kar], "ROI64") == 0)    MapType = SUMA_CMAP_ROI64;
         if (strcmp(argv[kar], "ROI128") == 0)    MapType = SUMA_CMAP_ROI128;
   
         if (MapType == SUMA_CMAP_UNDEFINED) {
            /* hold till later, could be a map from SAC */
            /*
            fprintf (SUMA_STDERR, "Color map type not recognized.\n");
            exit (1);*/
         }      
         brk = YUP;
      }
      
      if (!brk) {
         fprintf (SUMA_STDERR,"Error %s: Option %s not understood. Try -help for usage\n", FuncName, argv[kar]);
         exit (1);
      } else {   
         brk = NOPE;
         kar ++;
      }
      
   }/* loop accross command ine options */
   
   if (!IntName) {
      fprintf (SUMA_STDERR,"Error %s: No input file specified.\n", FuncName);
      exit(1);
   }
   
   /* default interpolation mode */
   if (interpmode == SUMA_UNDEFINED_MODE) interpmode = SUMA_INTERP; 
   
   /* check input */
   if (!SUMA_filexists (IntName)) {
      fprintf (SUMA_STDERR,"Error %s: File %s could not be found.\n", FuncName, IntName);
      exit(1);
   }
   
   if (frf && !CmapFileName) {
      fprintf (SUMA_STDERR,"Error %s: -frf option is only valid with -cmapfile.\n", FuncName);
      exit(1);
   }
   
   #if 0
   /* will not work with [ choosers ] */
   if (CmapFileName) {
      if (!SUMA_filexists (CmapFileName)) {
         fprintf (SUMA_STDERR,"Error %s: File %s could not be found.\n", FuncName, CmapFileName);
         exit(1);
      }
   } 
   #endif
   
   if (ApplyPercClip && ApplyClip) {
      fprintf (SUMA_STDERR,"Error %s: Simultaneous use of -clp and -perc_clp. You should be punished.\n", FuncName);
      exit(1);
   }
   
   if ((ApplyPercClip || ApplyClip) && arange >= 0.0) {
      fprintf (SUMA_STDERR,"Error %s: Simultaneous use of -clp/-perc_clp and -apr/anr.\n Read the help.\n", FuncName);
      exit(1);
   }
   
   if (iVopt || Vopt) {
      fprintf (SUMA_STDERR,"Error %s: -v and -iv are obsolete.\n Use -input option instead.\n", FuncName);
      exit(1);
   }
   
   if (!inopt) {
      fprintf (SUMA_STDERR,"Error %s: -input option must be specified.\n", FuncName);
      exit(1);
   }
   
   im = mri_read_1D (IntName);
   
   if (!im) {
      SUMA_S_Err("Failed to read file");
      exit (1);
   }
   
   if (vcol < 0) {
      fprintf (SUMA_STDERR,"Error %s: vcol must be > 0\n", FuncName);
      exit(1);
   }
   
   far = MRI_FLOAT_PTR(im);
   if (icol < 0 && icol != -1) {
      fprintf (SUMA_STDERR,"Error %s: icol(%d) can only have -1 for a negative value\n", FuncName, icol);
      exit(1);
   }
   
   if (icol >= im->ny || vcol >= im->ny) {
      fprintf (SUMA_STDERR,"Error %s: icol(%d) and vcol(%d) must be < %d\nwhich is the number of columns in %s\n",
          FuncName, icol, vcol, im->ny, IntName);
      exit(1);
   }
   
   
   if (brfact <=0 || brfact > 1) {
      fprintf (SUMA_STDERR,"Error %s: BrightFact must be > 0 and <= 1.\n", FuncName);
      exit (1);
   }
   
   if (MaskColor[0] < 0 || MaskColor[0] > 1 || MaskColor[1] < 0 || MaskColor[1] > 1 || MaskColor[2] < 0 || MaskColor[2] > 1) {
      fprintf (SUMA_STDERR,"Error %s: MaskColor values must be >=0 <=1.\n", FuncName);
      exit(1);
   }
     
   
   N_V = im->nx;
   V = (float *) SUMA_calloc (N_V, sizeof(float));
   iV = (int *) SUMA_calloc (N_V, sizeof(int));
   if (!V || !iV) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for V or iV.\n", FuncName);
      exit(1);
   }
   
   if (icol < 0) {
     for (ii=0; ii < N_V; ++ii) {
         iV[ii] = ii; 
         V[ii] = far[vcol*N_V+ii]; 
     } 
   } else {
      for (ii=0; ii < N_V; ++ii) {
         iV[ii] = (int)far[icol*N_V+ii]; 
         V[ii] = far[vcol*N_V+ii]; 
      }
   }
   
   mri_free(im); im = NULL;

   /* read values per node */
   /* SUMA_disp_vect (V, 3);  */
   
   /* find the min/max of V */
   SUMA_MIN_MAX_VEC(V, N_V, Vmin, Vmax, Vminloc, Vmaxloc)
   /* fprintf (SUMA_STDERR,"%s: Vmin=%f, Vmax = %f\n", FuncName, Vmin, Vmax);*/ 
   
   if (arange == 0.0) {
      if (fabsf(Vmin) > fabsf(Vmax)) arange = fabsf(Vmin);
      else arange = fabsf(Vmax);
   }
   /* figure out the range if PercRange is used */
   if (ApplyPercClip) {
      
      fprintf (SUMA_STDERR,"%s: Percentile range [%f..%f] is equivalent to ", FuncName, ClipRange[0], ClipRange[1]);
      Vsort = SUMA_PercRange (V, NULL, N_V, ClipRange, ClipRange);
      fprintf (SUMA_STDERR,"[%f..%f]\n", ClipRange[0], ClipRange[1]);
      ApplyClip = YUP;
      
      if (Vsort) SUMA_free(Vsort);
      else {
         fprintf (SUMA_STDERR,"Error %s: Error in SUMA_PercRange.\n", FuncName);
         exit(1);
      }
   }
   
   /* Load AFNI default color maps */
   SAC = SUMA_Get_AFNI_Default_Color_Maps ();
   if (!SAC) {
      fprintf (SUMA_STDERR,"Error %s: Failed to obtain AFNI's standard colors.\n", FuncName);
      exit(1);
   } else {
      /* are there database files to read */
      if (dbfile) {
         SUMA_LH("Now trying to read db file");
         if (SUMA_AFNI_Extract_Colors ( dbfile, SAC ) < 0) {
            fprintf (SUMA_STDERR,"Error %s: Failed to read %s colormap file.\n", FuncName, dbfile);
            exit(1);
         }
      }
   }
   
   
   FromAFNI = NOPE; /* assume colormap is not coming from SAC (the colormap database structure) */
   if (CmapFileName) { 
      /* load the color map */
      CM = SUMA_Read_Color_Map_1D (CmapFileName);
      if (CM == NULL) {
         fprintf (SUMA_STDERR,"Error %s: Could not load colormap.\n", FuncName);
         exit (1); 
      }
      if (frf) {
         SUMA_LH("Flipping colormap");
         SUMA_Flip_Color_Map (CM);
      }   

      if (!CM->Sgn) CM->Sgn = Sgn; 
   }else{
      /* dunno what kind of map yet. Try default first */
      if (MapType != SUMA_CMAP_UNDEFINED) {
         CM = SUMA_GetStandardMap (MapType);
         if (CM) {
            /* good, sign it and out you go */   
            CM->Sgn = Sgn;
         } else {
            fprintf (SUMA_STDERR,"Error %s: Could not get standard colormap.\n", FuncName);
            exit (1); 
         }
      } else {
         SUMA_LH("An AFNI color map ");
         /* a color from AFNI's maps */
         FromAFNI = YUP;
         imap = SUMA_Find_ColorMap ( MapName, SAC->CMv, SAC->N_maps, -2);
         if (imap < 0) {
            fprintf (SUMA_STDERR,"Error %s: Could not find colormap %s.\n", FuncName, MapName);
            exit (1); 
         }
         CM = SAC->CMv[imap];
      }
   }
   
   
   /* show the colromap on STDERR */
   if (ShowMap) {
      fprintf (SUMA_STDERR, "%s: Colormap used:\n", FuncName);
      SUMA_Show_ColorMapVec (&CM, 1, NULL, 2);
      exit(0);
   }
   
   /* show all the colors and colormaps in SAC on STDERR */
   if (ShowMapdb) {
      fprintf (SUMA_STDERR, "%s: AFNI colormaps found in db:\n", FuncName);
      SUMA_Show_ColorVec (SAC->Cv, SAC->N_cols, NULL);
      SUMA_Show_ColorMapVec (SAC->CMv, SAC->N_maps, NULL, 2);
      exit(0);
   }
   
   /* get the options for creating the scaled color mapping */
   OptScl = SUMA_ScaleToMapOptInit();
   if (!OptScl) {
      fprintf (SUMA_STDERR,"Error %s: Could not get scaling option structure.\n", FuncName);
      exit (1); 
   }
   
   /* work the options a bit */
   if (ApplyMask) {
      OptScl->ApplyMask = ApplyMask;
      OptScl->MaskRange[0] = MaskRange[0]; OptScl->MaskRange[1] = MaskRange[1]; 
      OptScl->MaskColor[0] = MaskColor[0]; OptScl->MaskColor[1] = MaskColor[1]; OptScl->MaskColor[2] = MaskColor[2];
   }
   
   if (ApplyClip) {
      OptScl->ApplyClip = YUP;
      OptScl->ClipRange[0] = ClipRange[0]; OptScl->ClipRange[1] = ClipRange[1];
   }

   OptScl->interpmode = interpmode;
   
   OptScl->BrightFact = brfact;
      
   /* map the values in V to the colormap */
      /* allocate space for the result */
      SV = SUMA_Create_ColorScaledVect(N_V);
      if (!SV) {
         fprintf (SUMA_STDERR,"Error %s: Could not allocate for SV.\n", FuncName);
         exit(1);
      }
      
      /* finally ! */
      if (alaAFNI) {
         if (LocalHead) {
            fprintf (SUMA_STDERR,"%s: Calling SUMA_ScaleToMap_alaAFNI\n", FuncName);
            fprintf (SUMA_STDERR,"%s: arange = %f\n",  FuncName, arange);
         }
         if (CM->frac) {
            if (CM->frac[0] > 0 && CM->Sgn == -1) {
               SUMA_S_Err ("Color map fractions positive with -anr option");
               exit(1);
            }
            if (CM->frac[0] < 0 && CM->Sgn == 1) {
               SUMA_S_Err ("Color map fractions negative with -apr option");
               exit(1);
            }
         }
      
         if (Sgn) {
            if (Sgn != CM->Sgn) {
               SUMA_S_Warn ("Mixing positive maps (all fractions > 0) with -anr option\nor vice versa. That is allowed but know what you're doing.\n");
            }
         }
         if (!SUMA_ScaleToMap_alaAFNI (V, N_V, arange, CM, OptScl, SV)) {
            fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_ScaleToMap_alaAFNI.\n", FuncName);
            exit(1);
         }
      } else {
         if (LocalHead) fprintf (SUMA_STDERR,"%s: Calling SUMA_ScaleToMap\n", FuncName);
         if (!SUMA_ScaleToMap (V, N_V, Vmin, Vmax, CM, OptScl, SV)) {
            fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_ScaleToMap.\n", FuncName);
            exit(1);
         }
      }
   
   /* Now write the colored vector back to disk */
   if (NoMaskCol) {
      for (k=0; k < N_V; ++k) {
         if (!SV->isMasked[k]) fprintf (SUMA_STDOUT, "%d %f %f %f\n", iV[k], SV->cM[k][0], SV->cM[k][1], SV->cM[k][2]);
      }
   } else {
      for (k=0; k < N_V; ++k) {
         fprintf (SUMA_STDOUT, "%d %f %f %f\n", iV[k], SV->cM[k][0], SV->cM[k][1], SV->cM[k][2]);
      }
   }
   
   /* freeing time */
   if (V) SUMA_free(V);
   if (iV) SUMA_free(iV);
   if (!FromAFNI) if (CM) SUMA_Free_ColorMap (CM); /* only free CM if it was a pointer copy from a map in SAC */
   if (OptScl) SUMA_free(OptScl);
   if (SV) SUMA_Free_ColorScaledVect (SV);
   if (SAC) SAC = SUMA_DestroyAfniColors(SAC); /* destroy SAC */
   

   
   exit (0);
}   

#endif

/*!
   \brief flips a color map upside down 
   
   \param CM (SUMA_COLOR_MAP *) to be flipped
*/
void SUMA_Flip_Color_Map (SUMA_COLOR_MAP *CM)
{
   static char FuncName[] = {"SUMA_Flip_Color_Map"};
   int lim, i, j, c;
   float t;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }
   
   if (!CM) SUMA_RETURNe;
   
   if (!CM->M) SUMA_RETURNe;
   
   lim = CM->N_Col/2;
   for (j=0; j < 3; ++j) {  /* loop accross R,G,B columns */
      for (i=0; i < lim; ++i) {
         t = CM->M[i][j];           /* store color at i to be flipped */
         c = CM->N_Col - i - 1;     /* index of color to replace one at i */
         CM->M[i][j] = CM->M[c][j]; /* replace color at i */
         CM->M[c][j] = t;           /* put old color of i ar c */
      } 
   }
   
   if (CM->frac) { /* got to flip fractions */
      for (i=0; i < lim; ++i) {
         t = CM->frac[i];           /* store fraction at i to be flipped */
         c = CM->N_Col - i - 1;     /* index of fraction to replace one at i */
         CM->frac[i] = CM->frac[c]; /* replace fraction at i */
         CM->frac[c] = t;           /* put old fraction of i at c */
      } 
   }
   
   SUMA_RETURNe;
}
/*! 
   A function to compute the percentile range.
   
   Vsort = SUMA_PercRange (V, Vsort, N_V, PercRange, PercRangeVal)
   
   \param V (float *) pointer to vector containing N_V values
   \param Vsort (float *) pointer to sorted version of V. 
      NOTE: If you want the function to sort V for you then pass NULL here and 
       expect the pointer to Vsort to be returned
   \param N_V (int) number of values in V
   \param PercRange (float *) 2x1 vector with percentile range desired (values between 0 and 100)
   \param PercRangeVal (float *) 2x1 vector with values in V corresponding the percentile range
   \ret Vsort, pointer to the sorted version of V. NULL in case of error.
      NOTE: Before a NULL is returned, Vsort is freed.
   
   This function only allocates space for Vsort if a null is passed for Vsort in the function call   
*/
float * SUMA_PercRange (float *V, float *Vsort, int N_V, float *PercRange, float *PercRangeVal)
{
   static char FuncName[] = {"SUMA_PercRange"};
   int *isort, il, ih;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

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
   
   SUMA_RETURN (Vsort);
}

/*!
   Function to allocate and initialize an Overlays pointer
   
   ans = SUMA_CreateOverlayPointer (N_Nodes, Name);
   
   \param N_Nodes (int): The number of nodes for which color is assigned
   \param Name (char *): A character string containing the name of the color overlay
   \ret ans (SUMA_OVERLAYS *): a pointer to the structure containing the color overlay
      NULL is returned in case of trouble.
      
   The following fields are set to these defaults:
   Show = NOPE;
   GlobalOpacity = -1.0;
   LocalOpacity vector is all zeros except the first value is -1.0
   NodeDef vector is all zeros except the first value is -1
   PlaneOrder = -1; i.e. not set 
   BrightMod = 0 ; i.e. none
   \sa SUMA_FreeOverlayPointer 
    
*/

SUMA_OVERLAYS * SUMA_CreateOverlayPointer (int N_Nodes, const char *Name)
{
   static char FuncName[]={"SUMA_CreateOverlayPointer"};
   SUMA_OVERLAYS *Sover=NULL;
   SUMA_FileName sfn;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   Sover = (SUMA_OVERLAYS *)SUMA_malloc(sizeof(SUMA_OVERLAYS));
   if (!Sover) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for Sover.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   
   /* copy the name */
   Sover->Name = (char *)SUMA_calloc (strlen(Name)+1, sizeof(char));
   Sover->Name = strcpy(Sover->Name, Name);
   
   /* create a label */
   sfn = SUMA_StripPath((char *)Name);
   Sover->Label = sfn.FileName;
   if (sfn.Path) SUMA_free(sfn.Path); /* get rid of path */
   
   Sover->N_Alloc = N_Nodes;
   
   Sover->NodeDef = (int *) SUMA_calloc(Sover->N_Alloc, sizeof(int));
   Sover->ColMat = (float **) SUMA_allocate2D(Sover->N_Alloc, 3, sizeof(float));
   Sover->LocalOpacity = (float *)SUMA_calloc(Sover->N_Alloc, sizeof(float));
   
   if (!Sover->NodeDef || !Sover->ColMat || !Sover->LocalOpacity) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for Sover fields.\n", FuncName);
      SUMA_free(Sover);
      SUMA_RETURN (NULL);   
   }
   
   Sover->GlobalOpacity = -1.0; /* no factor applied */
   Sover->LocalOpacity[0] = -1.0; /* flag indicating local facts have not been initialized */
   Sover->NodeDef[0] = -1;
   Sover->N_NodeDef = -1;
   Sover->Show = NOPE;
   Sover->PlaneOrder = -1; /* No order is specified */
   Sover->BrightMod = 0; /* no brightness modulation effects */
   
   SUMA_RETURN (Sover);
}

/*!
   \brief releases an overlay pointer (decrement its inode count and free it if necessary)
*/
SUMA_Boolean SUMA_ReleaseOverlay (SUMA_OVERLAYS * Overlays, SUMA_INODE *Overlays_Inode)
{
   static char FuncName[]={"SUMA_FreeOverlay"};
   SUMA_Boolean LocalHead = NOPE;

   if (Overlays_Inode || Overlays) { /* there should be no case where only one of two is null but if such a case existed, you'll get notified below. */
      if (SUMA_ReleaseLink(Overlays_Inode)) { 
         /* some links are left, do not free memory */
      } else {
         fprintf (SUMA_STDERR,"%s: Overlay plane %s is free of links, freeing allocated memory ...\n", FuncName, Overlays->Name);
         if (Overlays) SUMA_FreeOverlayPointer (Overlays);
         SUMA_free(Overlays_Inode); 
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

SUMA_Boolean SUMA_FreeOverlayPointer (SUMA_OVERLAYS * Sover) 
{
   static char FuncName[]={"SUMA_FreeOverlayPointer"};
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   if (Sover == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Sover is NULL, nothing to do. Returning OK flag.\n", FuncName);
      SUMA_RETURN (YUP);
   }
   
   if (Sover->NodeDef) SUMA_free(Sover->NodeDef);
   if (Sover->ColMat) SUMA_free2D ((char **)Sover->ColMat, Sover->N_Alloc);
   if (Sover->LocalOpacity) SUMA_free(Sover->LocalOpacity);
   if (Sover->Label) SUMA_free(Sover->Label);
   if (Sover->Name) SUMA_free(Sover->Name);
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
   int i;
   SUMA_OVERLAYS *ptr= NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   for (i=0; i < N_Overlays; ++i) {
      if (!strcmp(Overlays[i]->Name, Name)) {
         *OverInd = i;
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Found overlay plane %s, indexed %d.\n", FuncName, Name, i);
         SUMA_RETURN (Overlays[i]);
      }
   }
   
   if (LocalHead) fprintf (SUMA_STDOUT,"%s: Overlay plane %s was not found.\n", FuncName, Name);
   
   *OverInd = -1;

   SUMA_RETURN (NULL);
} 
 
/*!

   function to turn color overlay planes into GL color array
   
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
*/

SUMA_Boolean SUMA_Overlays_2_GLCOLAR4(SUMA_OVERLAYS ** Overlays, int N_Overlays, GLfloat *glcolar, int N_Node, float Back_Modfact, SUMA_Boolean ShowBackground, SUMA_Boolean ShowForeground)
{
   static char FuncName[]={"SUMA_Overlays_2_GLCOLAR4"};
   int ShowOverLays[SUMA_MAX_OVERLAYS], ShowOverLays_Back[SUMA_MAX_OVERLAYS]; 
   int ShowOverLays_sort[SUMA_MAX_OVERLAYS], ShowOverLays_Back_sort[SUMA_MAX_OVERLAYS], iloc[SUMA_MAX_OVERLAYS];
   int OverlayOrder_Back[SUMA_MAX_OVERLAYS], OverlayOrder[SUMA_MAX_OVERLAYS];
   int i, j, NshowOverlays, NshowOverlays_Back, *isort, i4, i4_0, i4_1, i4_2;
   SUMA_Boolean *isColored, *isColored_Fore, *isColored_Back;
   GLfloat *glcolar_Fore , *glcolar_Back;
   float avg_Back, avgfact;
   SUMA_Boolean LocalHead = NOPE; /* local headline debugging messages */   

   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   if (LocalHead)   { 
      fprintf (SUMA_STDOUT, "%s: Showing all overlay planes.\n", FuncName);
      SUMA_Show_ColorOverlayPlanes (Overlays, N_Overlays); } 
   

   /* get the indices into the color structure vector of overlays to be shown */
   NshowOverlays = 0;
   NshowOverlays_Back = 0;
   for (j=0; j < N_Overlays; ++j) {
      if (Overlays[j]->Show && Overlays[j]->GlobalOpacity != 0) {
         if (Overlays[j]->BrightMod) {
            ShowOverLays_Back[NshowOverlays_Back] = j; 
            OverlayOrder_Back[NshowOverlays_Back] = Overlays[j]->PlaneOrder;
            ++ NshowOverlays_Back;
         }else {
            ShowOverLays[NshowOverlays] = j; 
            OverlayOrder[NshowOverlays] = Overlays[j]->PlaneOrder;
            ++ NshowOverlays;
         }
      }
   }

   if (LocalHead)   fprintf (SUMA_STDERR,"%s: Found %d Mix overlays and %d Mix-Brightmod overlays.\n", FuncName, NshowOverlays, NshowOverlays_Back);
   
   /* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv allocate space ------------------------------------*/
   
   isColored = (SUMA_Boolean *) SUMA_calloc (N_Node, sizeof(SUMA_Boolean));/* allocate for flag indicating the a node is colored */
   if (!isColored) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for isColored.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   glcolar_Back = NULL;
   isColored_Back = NULL;
   if (ShowBackground) {
      if (NshowOverlays_Back) {
         glcolar_Back = (GLfloat *) SUMA_calloc (4*N_Node, sizeof(GLfloat));
         isColored_Back = (SUMA_Boolean *) SUMA_calloc (N_Node, sizeof(SUMA_Boolean));

         if (!isColored_Back || !glcolar_Back) {
            fprintf (SUMA_STDERR,"Error %s: Failed to allocate for isColored_Back || glcolar_Back.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
      }
   }
   
   isColored_Fore = NULL;
   glcolar_Fore = NULL;
   if (ShowForeground) {
      if (NshowOverlays) {
         glcolar_Fore = (GLfloat *) SUMA_calloc (4*N_Node, sizeof(GLfloat));
         isColored_Fore = (SUMA_Boolean *) SUMA_calloc (N_Node, sizeof(SUMA_Boolean));

         if (!isColored_Fore || !glcolar_Fore) {
            fprintf (SUMA_STDERR,"Error %s: Failed to allocate for isColored_Fore || glcolar_Fore.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
      }      
   }
   /* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^ allocate space ------------------------------------*/
   
   /* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv Background colors ------------------------------*/
   
   if (ShowBackground) {
      /* arrange Background color planes by plane order in preparation for mixing them */
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
         if (LocalHead)   fprintf (SUMA_STDERR,"%s: Mixing Background colors ...\n", FuncName);

         if (!SUMA_MixOverlays (Overlays, N_Overlays, ShowOverLays_Back_sort, NshowOverlays_Back, glcolar_Back, N_Node, isColored_Back, NOPE)) {
            fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_MixOverlays.\n", FuncName);
            SUMA_RETURN (NOPE);
         }
      } else {
         ShowBackground = NOPE;
      } 
   } else {
      NshowOverlays_Back = 0;
   }
   /* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  Background colors ------------------------------*/
   
   
   /* vvvvvvvvvvvvvvvvvvvvvvvvvvvvvv Foreground  colors ------------------------------*/
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
            if (LocalHead)   fprintf (SUMA_STDERR,"%s: Mixing Foreground colors ....\n", FuncName);
            if (!SUMA_MixOverlays (Overlays, N_Overlays, ShowOverLays_sort, NshowOverlays, glcolar_Fore, N_Node, isColored_Fore, NOPE)) {
               fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_MixOverlays.\n", FuncName);
               SUMA_RETURN (NOPE);
            }
      } else {
         ShowForeground = NOPE;
      }
   } else {
      NshowOverlays = 0;
   }
   /* ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^  Foreground colors ------------------------------*/

   /* time to modulate the mixed colors with the average brightness */
   if (NshowOverlays && NshowOverlays_Back) {
      if (LocalHead)   fprintf (SUMA_STDERR,"%s: Modulating Brightness of Foreground colors ...\n", FuncName);

      for (i=0; i < N_Node; ++i) {
         avgfact = Back_Modfact / 3.0;
         if (isColored_Fore[i] && isColored_Back[i]) { /* colors from both sides, adjust brightness */
            i4_0 = 4 * i; i4_1 = i4_0 + 1; i4_2 = i4_0 + 2; 
            if (!Back_Modfact) {
               glcolar[i4_0] = glcolar_Fore[i4_0];
               glcolar[i4_1] = glcolar_Fore[i4_1];
               glcolar[i4_2] = glcolar_Fore[i4_2];
            } else {
               avg_Back = (glcolar_Back[i4_0] + glcolar_Back[i4_1] + glcolar_Back[i4_2]) * avgfact ;   
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
            fprintf (SUMA_STDERR,"%s: Never been colored.\n", FuncName);
            /* has never been colored, put defaults */
            i4 = 4 * i;
            glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
            glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
            glcolar[i4] = SUMA_GRAY_NODE_COLOR; ++i4;
            isColored[i] = NOPE;
         }
      }
      
      if (LocalHead)   fprintf (SUMA_STDERR,"%s: Done Modulating Brightness of overlay colors.\n", FuncName);
   } 
   if (NshowOverlays && !NshowOverlays_Back) {
      if (LocalHead)   fprintf (SUMA_STDERR,"%s: Only Foreground colors.\n", FuncName);
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
      if (LocalHead)   fprintf (SUMA_STDERR,"%s: Only Background colors.\n", FuncName);
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
SUMA_Boolean SUMA_MixOverlays (SUMA_OVERLAYS ** Overlays, int N_Overlays, int *ShowOverlays, int NshowOverlays, GLfloat *glcolar, int N_Node, SUMA_Boolean *isColored, SUMA_Boolean FILL)
{    
   static char FuncName[] = {"SUMA_MixOverlays"};
   int i, j;
   SUMA_Boolean Full, Fill, Locl, Glob;
   SUMA_Boolean LocalHead = NOPE; /* local headline debugging messages */   
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   if (!isColored) {
      fprintf (SUMA_STDERR, "Error %s: isColored is NULL.\n", FuncName); 
      SUMA_RETURN (NOPE);
   }
   if (!NshowOverlays) { /* nothing to see here */
      fprintf (SUMA_STDERR, "Warning %s: Nothing to do.\n", FuncName); 
      if (FILL) {
         fprintf (SUMA_STDERR, "Warning %s: Filling with blank default color\n", FuncName); 
         SUMA_FillBlanks_GLCOLAR4(isColored, N_Node, SUMA_GRAY_NODE_COLOR, SUMA_GRAY_NODE_COLOR, SUMA_GRAY_NODE_COLOR, glcolar);
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
      
      /* is this a full listing */
      if (LocalHead) fprintf (SUMA_STDOUT, "%s: Full listing flag: %d\n", FuncName, Overlays[i]->NodeDef[0]);
      if (Overlays[i]->NodeDef[0] < 0) {         Fill = NOPE;   /* Full list, no need to fill up unvisited nodes at the end */   } 
      else {         Full = NOPE; /* Not a full list */      }
      
      if (j > 0) { /* opacity plays a role when you are overlaying one plane on top of the other */
         /* is this a Global Factor */
         if (Overlays[i]->GlobalOpacity < 0.0) {         Glob = NOPE;      }

         /* is this a Local Factor */
         if (Overlays[i]->LocalOpacity[0] < 0) {         Locl = NOPE;      }
      } else {
         Glob = NOPE; Locl = NOPE;
      }
      
      if (LocalHead) 
         fprintf (SUMA_STDOUT,"%s: Building color layer %d Overlay #%d: %s ...\nFull=%d, Glob=%d (Globopacity %f), Locl=%d,Fill=%d\n", \
         FuncName, j, i, Overlays[i]->Name, (int)Full, (int)Glob, Overlays[i]->GlobalOpacity, (int)Locl, (int)Fill);
      
         
      /* call the appropriate macro to add the overlay */
      if (Full && Glob && Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGB_FGL_AR4op ...\n", FuncName);
         
         /* This macro used to be called: SUMA_RGBmat_FullGlobLoc2_GLCOLAR4_opacity
         but name was too long for some compilers */
         SUMA_RGB_FGL_AR4op(\
         Overlays[i]->ColMat, glcolar, N_Node, Overlays[i]->GlobalOpacity, Overlays[i]->LocalOpacity, isColored);         
      }
      
      if (!Full && Glob && Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGB_PGL_AR4op ...\n", FuncName);
         /* This macro used to be called: SUMA_RGBmat_PartGlobLoc2_GLCOLAR4_opacity */
         SUMA_RGB_PGL_AR4op(\
         Overlays[i]->ColMat, Overlays[i]->NodeDef, glcolar, Overlays[i]->N_NodeDef, isColored, Overlays[i]->GlobalOpacity, Overlays[i]->LocalOpacity,  N_Node);
      }
      
      if (Full && !Glob && Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling  SUMA_RGB_FnGL_AR4op...\n", FuncName);
         /* This macro used to be called: SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4_opacity */
         SUMA_RGB_FnGL_AR4op(\
         Overlays[i]->ColMat, glcolar, N_Node, Overlays[i]->LocalOpacity, isColored);         
      }
      
      if (!Full && !Glob && Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGB_PnGL_AR4op ...\n", FuncName);
         /* This macro used to be called: SUMA_RGBmat_PartNoGlobLoc2_GLCOLAR4_opacity*/
         SUMA_RGB_PnGL_AR4op(\
         Overlays[i]->ColMat, Overlays[i]->NodeDef, glcolar, Overlays[i]->N_NodeDef, isColored, Overlays[i]->LocalOpacity, N_Node);
      }
      
      if (Full && !Glob && !Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGB_FnGnL_AR4op ...\n", FuncName);
         /* This macro used to be called: SUMA_RGBmat_FullNoGlobNoLoc2_GLCOLAR4_opacity*/
         SUMA_RGB_FnGnL_AR4op(\
         Overlays[i]->ColMat, glcolar, N_Node, isColored);         
      }
      
      if (!Full && !Glob && !Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGB_PnGnL_AR4op ...\n", FuncName);
         /* This macro used to be called: SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4_opacity */
         SUMA_RGB_PnGnL_AR4op(\
         Overlays[i]->ColMat, Overlays[i]->NodeDef, glcolar, Overlays[i]->N_NodeDef, isColored, N_Node);
      }
      
      if (Full && Glob && !Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling  SUMA_RGB_FGnL_AR4op...\n", FuncName);
         /* This macro used to be called: SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4_opacity*/
         SUMA_RGB_FGnL_AR4op(\
         Overlays[i]->ColMat, glcolar, N_Node, Overlays[i]->GlobalOpacity, isColored);
      }
       
      if (!Full && Glob && !Locl) {
         if (LocalHead) fprintf (SUMA_STDOUT,"%s: Calling  SUMA_RGB_PGnL_AR4op...\n", FuncName);
         /* This macro used to be called: SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4_opacity*/
         SUMA_RGB_PGnL_AR4op(\
         Overlays[i]->ColMat, Overlays[i]->NodeDef, glcolar, Overlays[i]->N_NodeDef, isColored, Overlays[i]->GlobalOpacity, N_Node);
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
SUMA_Boolean SUMA_Show_ColorOverlayPlanes (SUMA_OVERLAYS **Overlays, int N_Overlays) 
{
   static char FuncName[]={"SUMA_Show_ColorOverlayPlanes"};
   char *s;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   s = SUMA_ColorOverlayPlane_Info (Overlays, N_Overlays);
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
char *SUMA_ColorOverlayPlane_Info (SUMA_OVERLAYS **Overlays, int N_Overlays) 
{
   static char FuncName[]={"SUMA_ColorOverlayPlane_Info"};
   char stmp[1000], *s = NULL;
   int i, j, ShowN;
   SUMA_STRING *SS = NULL;
   
   if (SUMAg_CF->InOut_Notify)  SUMA_DBG_IN_NOTIFY(FuncName); 
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   sprintf (stmp,"Info on %d color overlay planes:\n---------------------------------\n", N_Overlays);
   SS = SUMA_StringAppend (SS,stmp);
   for (i=0; i < N_Overlays; ++i) {
      if (Overlays[i]) {
         sprintf (stmp,"Overlay plane %s: order %d, indexed %d, global opacity %f, BrightMod (isBackground) %d.\n", 
            Overlays[i]->Name, Overlays[i]->PlaneOrder, i, Overlays[i]->GlobalOpacity, Overlays[i]->BrightMod);
         SS = SUMA_StringAppend (SS,stmp);
         sprintf (stmp,"Show=%d, N_Alloc=%d, N_NodeDef=%d\n", (int)Overlays[i]->Show, Overlays[i]->N_Alloc, Overlays[i]->N_NodeDef);
         SS = SUMA_StringAppend (SS,stmp);
         if (Overlays[i]->N_NodeDef > 5) ShowN = 5;
         else ShowN = Overlays[i]->N_NodeDef;
         SS = SUMA_StringAppend (SS,"\n");
         sprintf (stmp,"\tindex\tR\tG\tB\tLocOp\n");
         SS = SUMA_StringAppend (SS,stmp);
         stmp[0] = '\0';
         for (j=0; j < ShowN; ++j) {
            sprintf (stmp,"%s\t%d\t%.3f\t%.3f\t%.3f\t%.3f\n", 
                     stmp, Overlays[i]->NodeDef[j], Overlays[i]->ColMat[j][0], 
                     Overlays[i]->ColMat[j][1], Overlays[i]->ColMat[j][2],
                     Overlays[i]->LocalOpacity[j]);
         }
         SS = SUMA_StringAppend (SS,stmp);        
         SS = SUMA_StringAppend (SS,"\n");

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

/*!
   \brief Frees SUMA_OVERLAY_LIST_DATUM * used in the linked list
*/ 
void SUMA_FreeOverlayListDatum (void *OLDv)
{
   static char FuncName[]={"SUMA_FreeOverlayListDatum"};
   SUMA_Boolean LocalHead = NOPE; 
   
   if (SUMAg_CF->InOut_Notify)  SUMA_DBG_IN_NOTIFY(FuncName); 

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
   
   if (SUMAg_CF->InOut_Notify)  SUMA_DBG_IN_NOTIFY(FuncName); 
   
   listop = (DList *)SUMA_malloc(sizeof(DList));
   
   dlist_init(listop, SUMA_FreeOverlayListDatum);
   SUMA_LH("Considering loop");
   for (i=0; i < SO->N_Overlays; ++i) {
      SUMA_LH("In Loop");
         OvD = (SUMA_OVERLAY_LIST_DATUM *)SUMA_malloc(sizeof(SUMA_OVERLAY_LIST_DATUM));
         OvD->Overlay = SO->Overlays[i];
         OvD->Overlay_Inode = SO->Overlays_Inode[i];
         if (!OvD->Overlay) {
            SUMA_LH("NULL Overlay");
         }
            SUMA_LH("Here");
         if (OvD->Overlay->BrightMod && Opt == 1) continue;   /* that was an unwanted background */
         if (!OvD->Overlay->BrightMod && Opt == -1) continue; /* that was an unwanted foreground */
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
               
               /* transform PlaneOrder so that is reflects the Background modulation */
               Shift = SO->N_Overlays;
               
               if (OvD->Overlay->BrightMod) ShftPlaneOrder = OvD->Overlay->PlaneOrder - Shift;
               else ShftPlaneOrder = OvD->Overlay->PlaneOrder;
               
               if (oOvD->Overlay->BrightMod) oShftPlaneOrder = oOvD->Overlay->PlaneOrder - Shift;
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

   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   /* First pass, do background */
   if (listop->size) {
      Elmop = NULL;
      i = 0;
      do {
         if (!Elmop) Elmop = dlist_head(listop);
         else Elmop = Elmop->next;
         OvD = (SUMA_OVERLAY_LIST_DATUM *)Elmop->data;
         if (OvD->Overlay->BrightMod) {
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
         if (!OvD->Overlay->BrightMod) {
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
      
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   Order = 0;
   Elmop = NULL;
   do {
      if (!Elmop) Elmop = dlist_head(listop);
      else Elmop = Elmop->next;
      OvD = (SUMA_OVERLAY_LIST_DATUM *)Elmop->data;
      if (OvD->Overlay->BrightMod) {
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
      
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   Order = listop->size -1 ;
   Elmop = NULL;
   do {
      if (!Elmop) Elmop = dlist_head(listop);
      else Elmop = Elmop->next;
      OvD = (SUMA_OVERLAY_LIST_DATUM *)Elmop->data;
      if (!OvD->Overlay->BrightMod) {
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
   
   if (SUMAg_CF->InOut_Notify)  SUMA_DBG_IN_NOTIFY(FuncName); 

   for (i=0; i< SO->N_Overlays; ++i) if (SO->Overlays[i] == Plane) SUMA_RETURN(YUP);
   
   SUMA_RETURN(NOPE);
}

void SUMA_Print_PlaneOrder (SUMA_SurfaceObject *SO, FILE *Out)
{   
   static char FuncName[]={"SUMA_Print_PlaneOrder"};
   char *s;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   if (Out == NULL) Out = stdout;
      
   s =  SUMA_PlaneOrder_Info(SO);
   
   if (s) {
      fprintf (Out, "%s", s);
      SUMA_free(s);
   }else {
      fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_SurfaceObject_Info.\n", FuncName);
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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

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
   
   if (SUMAg_CF->InOut_Notify)  SUMA_DBG_IN_NOTIFY(FuncName); 
   
   /* search for the plane by name */
   SUMA_LH("Searching for plane");
   if (!(Overlay = SUMA_Fetch_OverlayPointer(SO->Overlays, SO->N_Overlays, Name, &junk))) {
      SUMA_S_Err("Plane does not exist in SO->Overlays. (identified by name)");
      SUMA_RETURN (NOPE);
   }
   
   /* get the list of planes */
   SUMA_LH("Creating list");
   if (Overlay->BrightMod) list = SUMA_OverlaysToOrderedList (SO, -1);
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
   
   if (SUMAg_CF->InOut_Notify)  SUMA_DBG_IN_NOTIFY(FuncName); 
   
   /* search for the plane by name */
   SUMA_LH("Searching for plane");
   if (!(Overlay = SUMA_Fetch_OverlayPointer(SO->Overlays, SO->N_Overlays, Name, &junk))) {
      SUMA_S_Err("Plane does not exist in SO->Overlays. (identified by name)");
      SUMA_RETURN (NOPE);
   }
   
   /* get the list of planes */
   SUMA_LH("Creating list");
   if (Overlay->BrightMod) list = SUMA_OverlaysToOrderedList (SO, -1);
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

/*!
   \brief Adds a new plane to SO->Overlays. 
   If plane exists, you get an error message
*/
SUMA_Boolean SUMA_AddNewPlane (SUMA_SurfaceObject *SO, SUMA_OVERLAYS *Overlay, SUMA_INODE *Overlay_Inode)
{
   static char FuncName[]={"SUMA_AddNewPlane"};
   DList *ForeList=NULL, *BackList = NULL;
   SUMA_OVERLAY_LIST_DATUM *OvD=NULL;
   int junk=0;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify)  SUMA_DBG_IN_NOTIFY(FuncName); 
   
   if (!Overlay || !Overlay_Inode) {
      SUMA_S_Err("You sent me NULLS!");
      SUMA_RETURN (NOPE);
   }
   
   if (SUMA_isOverlayOfSO(SO, Overlay)) {
      SUMA_S_Err("Plane exists in SO->Overlays.");
      SUMA_RETURN (NOPE);
   }
   
   /* also try looking for plane by name */
   if (SUMA_Fetch_OverlayPointer(SO->Overlays, SO->N_Overlays, Overlay->Name, &junk)) {
      SUMA_S_Err("Plane exists in SO->Overlays. (identified by name)");
      SUMA_RETURN (NOPE);
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
   OvD = (SUMA_OVERLAY_LIST_DATUM *) SUMA_malloc(sizeof(SUMA_OVERLAY_LIST_DATUM));
   OvD->Overlay = Overlay;
   OvD->Overlay_Inode = Overlay_Inode;
   if (Overlay->BrightMod) {
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
   SO->Overlays_Inode[SO->N_Overlays] = Overlay_Inode;
   
   /* Now increment the number of overlays to be in SO */
   ++SO->N_Overlays;
   
   
   SUMA_LH("Destruction...");
   dlist_destroy(ForeList); SUMA_free(ForeList);
   dlist_destroy(BackList); SUMA_free(BackList);

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
   SUMA_Boolean LocalHead = NOPE;
   SUMA_SurfaceObject *SO = NULL;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   for (i=0; i<sv->N_ColList; ++i) {
      if (sv->ColList[i].Remix) {
         if (LocalHead) fprintf(SUMA_STDERR, "%s: Mixing colors (%s)...\n", FuncName, sv->ColList[i].idcode_str);
         dov_id = SUMA_findSO_inDOv (sv->ColList[i].idcode_str, SUMAg_DOv, SUMAg_N_DOv);
         if (dov_id < 0) {
            fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_findSO_inDOv.\n", FuncName);
            SUMA_RETURN(NOPE);
         }
         SO = (SUMA_SurfaceObject *)SUMAg_DOv[dov_id].OP;
         if (!SUMA_Overlays_2_GLCOLAR4(SO->Overlays, SO->N_Overlays, sv->ColList[i].glar_ColorList, SO->N_Node, \
            sv->Back_Modfact, sv->ShowBackground, sv->ShowForeground)) {
            fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_Overlays_2_GLCOLAR4.\n", FuncName);
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
                                 int *PlaneInd, SUMA_DO *dov, int N_dov) 
{
   static char FuncName[]={"SUMA_iRGB_to_OverlayPointer"}, stmp[500];
   int i, OverInd = -1, i_max, wrn_cnt = 0;
   SUMA_SurfaceObject *SO2 = NULL;
   SUMA_OVERLAYS *Overlay=NULL;
   SUMA_INODE *Overlay_Inode = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

      SUMA_LH("Fetching Overlay Pointer");
      /* if plane exists use it, else create a new one on the mappable surface */
      if (!SUMA_Fetch_OverlayPointer (SO->Overlays, SO->N_Overlays, Name, &OverInd)) {
         SUMA_LH("pointer not found");
         /* overlay plane not found, create a new one on the mappable surface*/
         if (!SUMA_isINHmappable(SO)) {
            if (sopd->Source == SES_Afni) {
               /* unexpected, surfaces coming from AFNI with a map should be inherrently mappable */
               fprintf(SUMA_STDERR,"Error %s: Surface %s (ID: %s) received from AFNI is not Inherrently mappable.\n", FuncName, SO->Label, SO->idcode_str);
               SUMA_RETURN(NOPE);
            } else {
               SUMA_SL_Warn ("Placing colors on surface \nnot inherently mappable.\nCase not tested.");
            }
         } 

         Overlay = SUMA_CreateOverlayPointer (SO->N_Node, Name);
         if (!Overlay) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateOverlayPointer.\n", FuncName);
            SUMA_RETURN(NOPE);
         } 

         /* make an Inode for the overlay */
         Overlay_Inode = SUMA_CreateInode ((void *)Overlay, SO->idcode_str);
         if (!Overlay_Inode) {
            fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInode\n", FuncName);
            SUMA_RETURN(NOPE);
         }

         /* set up some defaults for the overlap plane */
         Overlay->Show = sopd->Show;
         Overlay->GlobalOpacity = sopd->GlobalOpacity;
         Overlay->BrightMod = sopd->BrightMod;
         
         OverInd = SO->N_Overlays; 

         /* Add this plane to SO->Overlays */
         if (!SUMA_AddNewPlane (SO, Overlay, Overlay_Inode)) {
            SUMA_SL_Crit("Failed in SUMA_AddNewPlane");
            SUMA_FreeOverlayPointer(Overlay);
            SUMA_RETURN (NOPE);
         }

         
      }else {
         SUMA_LH("Pointer found");
      }
      
      if (LocalHead) fprintf (SUMA_STDERR, "%s: OverInd = %d, Loading colors to Overlay Plane...\n", FuncName, OverInd);
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
      
      /* Now put the colors in the overlay plane */
      if (LocalHead) fprintf (SUMA_STDERR,
                              "%s: %d node colors are to be inserted.\n",
                              FuncName, i_max);
                              
      SO->Overlays[OverInd]->N_NodeDef = i_max;
      if (SO->Overlays[OverInd]->N_NodeDef) {
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
                     /*fprintf(SUMA_STDERR,"Node %d: r%d, g%d, b%d\n", inel[i], r[i], g[i], b[i]);*/
                     if (SO->N_Node > inel[i]) {
                        SO->Overlays[OverInd]->NodeDef[i] = inel[i];
                        SO->Overlays[OverInd]->ColMat[i][0] = (float)(r[i]) * sopd->DimFact;
                        SO->Overlays[OverInd]->ColMat[i][1] = (float)(g[i]) * sopd->DimFact;
                        SO->Overlays[OverInd]->ColMat[i][2] = (float)(b[i]) * sopd->DimFact;
                     } else {
                        if (!wrn_cnt) {
                           SUMA_SLP_Warn("Color plane includes node indices\n"   \
                                         "that are >= number of nodes in surface.\n");
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
                     /*fprintf(SUMA_STDERR,"Node %d: r%d, g%d, b%d\n", inel[i], r[i], g[i], b[i]);*/
                     if (SO->N_Node > inel[i]) {
                        SO->Overlays[OverInd]->NodeDef[i] = inel[i];
                        SO->Overlays[OverInd]->ColMat[i][0] = (float)(r[i]) * sopd->DimFact;
                        SO->Overlays[OverInd]->ColMat[i][1] = (float)(g[i]) * sopd->DimFact;
                        SO->Overlays[OverInd]->ColMat[i][2] = (float)(b[i]) * sopd->DimFact;
                     } else {
                        if (!wrn_cnt) {
                           SUMA_SLP_Warn("Color plane includes node indices\n"   \
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
      
      /* store overlay plane index here, OverInd will get mango-ed further down */
      if (LocalHead) fprintf (SUMA_STDERR, "%s: OverInd = %d. Returning.\n", FuncName, OverInd);
      *PlaneInd = OverInd;

      SUMA_LH("Registering plane with surfaces deserving it");
      /* Now that you have the color overlay plane set, go about all the surfaces, searching for ones related to SO 
      and make sure they have this colorplane, otherwise, create a link to it. */   
      for (i=0; i < N_dov; ++i) {
         if (SUMA_isSO(dov[i])) {
            SO2 = (SUMA_SurfaceObject *)dov[i].OP;
            if (SUMA_isRelated(SO, SO2) && SO != SO2) {
               /* surfaces related and not identical, check on colorplanes */
               if (!SUMA_Fetch_OverlayPointer (SO2->Overlays, SO2->N_Overlays, Name, &OverInd)) {
                  /* color plane not found, link to that of SO */
                  SO2->Overlays_Inode[SO2->N_Overlays] = SUMA_CreateInodeLink (SO2->Overlays_Inode[SO2->N_Overlays],\
                         SO->Overlays_Inode[SO->N_Overlays-1]);
                  if (!SO2->Overlays_Inode[SO2->N_Overlays]) {
                     fprintf (SUMA_STDERR, "Error %s: Failed in SUMA_CreateInodeLink\n", FuncName);
                     SUMA_RETURN(NOPE);
                  }
                  /* now copy the actual overlay plane pointer */
                  SO2->Overlays[SO2->N_Overlays] = SO->Overlays[SO->N_Overlays-1];

                  /*setup the defaults */
                  SO2->Overlays[SO2->N_Overlays]->Show = YUP;
                  SO2->Overlays[SO2->N_Overlays]->GlobalOpacity = SUMA_AFNI_COLORPLANE_OPACITY;
                  SO2->Overlays[SO2->N_Overlays]->BrightMod = NOPE;

                  /*increment the number of overlay planes */
                  ++SO2->N_Overlays;
               } else {
                  /* colorplane found OK */
               }
            }
         }
      }

   SUMA_RETURN (YUP);

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
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
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
   SO->Overlays[OverInd]->N_NodeDef = 0; /* Flushed */
   SUMA_RETURN (YUP);
}

/*!
   \brief refreshes a colorplane list. 
   A combo of SUMA_AssembleColorPlaneList and SUMA_CreateScrolledList.
   
*/
void SUMA_RefreshColorPlaneList (SUMA_SurfaceObject *SO) 
{
   static char FuncName[]={"SUMA_RefreshColorPlaneList"};
   SUMA_LIST_WIDGET *LW = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   LW = SO->SurfCont->SwitchColPlanelst;
   
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
      for (i=0; i < LW->ALS->N_clist; ++i) fprintf (SUMA_STDERR,"%s: %s\n", FuncName, LW->ALS->clist[i]);
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
   SUMA_Boolean Found = NOPE, LocalHead = NOPE;

   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);
   
   /* get list of all Overlay planes */
   OverlayPlanelist = SUMA_OverlaysToOrderedList (SO, 0);

   /* need a list to store new names */
   list = (DList *)SUMA_malloc(sizeof(DList));
   dlist_init(list, NULL); /* you don't want to free the strings */
   /* need a list to store the pointers, it is useless when SortByOrder is used, but I leave it in to keep the code simple */
   listop = (DList *)SUMA_malloc(sizeof(DList)); 
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
      if (OvD->Overlay->BrightMod) {
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
      dlist_destroy(list);
      dlist_destroy(listop);
      dlist_destroy(OverlayPlanelist);
      SUMA_free(list);
      SUMA_free(listop);
      SUMA_free(OverlayPlanelist);
   }
   
   clist_str = SUMA_CreateAssembleListStruct();
   clist_str->clist = clist;
   clist_str->oplist = oplist;
   clist_str->N_clist = N_clist;
   
   /* return */
   SUMA_RETURN (clist_str);  
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
      
   if (SUMAg_CF->InOut_Notify) SUMA_DBG_IN_NOTIFY(FuncName);

   SO = (SUMA_SurfaceObject *)data;
   
   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: Received request to load %s for surface %s.\n", FuncName, filename, SO->Label);
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
   sopd.BrightMod = NOPE;
   sopd.Show = YUP;
   /* dim colors from maximum intensity to preserve surface shape highlights, division by 255 is to scale color values between 1 and 0 */
   sopd.DimFact = 0.5;
   sopd.i = (void *)irgb->i;
   sopd.r = (void *)irgb->r;
   sopd.g = (void *)irgb->g;
   sopd.b = (void *)irgb->b;
   sopd.a = NULL;

   if (!SUMA_iRGB_to_OverlayPointer (SO, filename, &sopd, &OverInd, SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SLP_Err("Failed to fetch or create overlay pointer.");
      SUMA_RETURNe;
   }

   /* values were copied, dump structure */
   irgb = SUMA_Free_IRGB(irgb);  

   if (!SUMA_RemixRedisplay (SO)) {
      SUMA_RETURNe;
   }
  
   SUMA_LH("Refreshing color plane list");            
   /*update the list widget if open */
   LW = SO->SurfCont->SwitchColPlanelst;
   if (LW) {
      if (!LW->isShaded) SUMA_RefreshColorPlaneList (SO);  
   }  
   
   if (LocalHead) fprintf (SUMA_STDERR,"%s: Updating color plane frame, OverInd=%d\n", FuncName, OverInd);
   /* update the color plane frame */
   if (OverInd >= 0)        
      SUMA_InitializeColPlaneShell(SO, SO->Overlays[OverInd]);

   SUMA_RETURNe;
}


/*** AFNI setup functions taken and trimmed from afni_setup.c
Reason for duplicating the functions is the complicated dependencies 
of some functions in afni_setup */
/*-----------------------------------------------------------------------
   Process an AFNI setup file.
-------------------------------------------------------------------------*/

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
   float rgb[3];
   char * fbuf , * fptr ;
   char str[SUMA_NSBUF] , left[SUMA_NSBUF] , middle[SUMA_NSBUF] , right[SUMA_NSBUF] ;
   SUMA_STRING *SS = NULL;
   SUMA_COLOR_MAP *CM=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
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

      /**----------------------------------------**/
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
         char label[SUMA_NSBUF] , defn[SUMA_NSBUF] ;

         if (LocalHead) fprintf (SUMA_STDERR,"%s: Found ***COLORS\n", FuncName);
         while(1){                          /* loop, looking for 'label = color' */
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
                  fprintf(SUMA_STDERR,"Error %s: Failed to interpret color %s\n", FuncName, right);
                  SUMA_RETURN(-1);
               }
            }
            SAC->Cv = SUMA_Add_Color (left, 
                           rgb[0], rgb[1], rgb[2], 1.0, 
                           SAC->Cv, &(SAC->N_cols));
         }
         continue ;  /* skip to end of outer while loop */   
      } /* end of COLORS */

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
            if( fptr-fbuf >= nbuf ){ if (fbuf) free(fbuf) ; fbuf = NULL; SUMA_RETURN(-1) ; }

            if( str[0] != '[' ){                     /* found a palette label */
               strcpy(label,str) ;
               if( !THD_filename_ok(label) ){
                  fprintf(SUMA_STDERR,"Error %s: In setup file %s, bad palette label: %s.\n Ignoring palette.\n",
                          FuncName, fname,label) ;
                  if (fbuf) free(fbuf) ; fbuf = NULL; 
                  SUMA_RETURN(-1) ;
               }
               
               if (LocalHead) {
                  fprintf (SUMA_STDERR,"%s: found palette label=%s. [len=%d label[0]=%d]\nnbuf=%d fptr-fbuf=%d\n", 
                     FuncName, label,(int)strlen(label),(int)label[0],
                     nbuf,fptr-fbuf);
               }
               


               SUMA_GETSTR ; if( SUMA_ISTARRED(str) ) goto SkipSection ;
            }


            if( str[0] != '[' ){                    /* bad news! */
               fprintf(SUMA_STDERR,"Error %s: In setup file %s, expected palette '[n]' here: %s.\n",
                              FuncName, fname , str ) ;
               SUMA_RETURN(-1) ;
            }

            /* decide how big the new palette is to be, and what mode  */
            ii = sscanf( str , "[%d%c" , &npane , &ccc ) ;
            if( ii < 2 ){
               fprintf(SUMA_STDERR,"%s: In setup file %s, can't interpret palette %s\n",
                              FuncName, fname , str ) ;
               SUMA_RETURN(-1) ;
            } else if( npane < NPANE_MIN || npane > NPANE_MAX ){
               fprintf(SUMA_STDERR,"%s: In setup file %s, illegal palette count %s.\n",
                              FuncName, fname , str ) ;
               SUMA_RETURN(-1) ;
            }

            /* at this point, now loop to read parameters for new palette */
            if (LocalHead) {
               fprintf(SUMA_STDERR,"%s: About to read %d panes.\n", FuncName, npane);
            }
            
            /* prepare the colormap */
            CM = (SUMA_COLOR_MAP *)SUMA_malloc(sizeof(SUMA_COLOR_MAP));
            if (CM == NULL) {
               SUMA_SL_Crit ("Failed to allocate for CM");
               SUMA_RETURN(-1);
            }
            CM->N_Col = npane; 
            if (ccc == '+') CM->Sgn = 1;
            else CM->Sgn = -1;
            
            
            CM->Name = (char *)SUMA_calloc(strlen(label)+10, sizeof(char));
            CM->frac = (float *)SUMA_calloc(CM->N_Col, sizeof(float));
            CM->M = (float**)SUMA_allocate2D (CM->N_Col, 3, sizeof(float));
            if (  CM->frac == NULL || CM->M == NULL || CM->Name == NULL ) {
               SUMA_SL_Crit ("Failed to allocate for fields of CM.");
               SUMA_RETURN (-1);
            }
            if (CM->Sgn == 1) sprintf(CM->Name, "%s_p%d", label, CM->N_Col);
            else sprintf(CM->Name, "%s_n%d", label, CM->N_Col);
            
            for( ii=0 ; ii < npane ; ii++ ){
               SUMA_GETEQN ;

               if (LocalHead) {
                  fprintf(SUMA_STDERR,"%s: SUMA_GETEQN: %s %s %s\n",FuncName, left,middle,right) ;
               }
               
               /* find that color */
               icol = SUMA_Find_Color (right, SAC->Cv, SAC->N_cols);
               if (icol < 0) {
                  fprintf(SUMA_STDERR,"Error %s: Color %s not found in dbase.\nUsing no-color in its place\n", FuncName, right);
                  CM->M[npane - ii - 1][0] = CM->M[npane - ii - 1][1] = CM->M[npane - ii - 1][2] = -1.0; 
               } else {
                  CM->M[npane - ii - 1][0] = SAC->Cv[icol].r;
                  CM->M[npane - ii - 1][1] = SAC->Cv[icol].g;
                  CM->M[npane - ii - 1][2] = SAC->Cv[icol].b;
               }
               CM->frac[npane - ii - 1] = atof(left);
            }
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
   int r, g, b;
   char stmp[10];   
   XVisualInfo vtmp, *vislist;
   XtAppContext app; 
   Widget tl;
   Display *dpy=NULL;
   XColor color_closest, color_exact;
   Colormap cmap;
   SUMA_Boolean LocalHead = NOPE;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }
   
      if (Name[0] == '#') { /* explicitly defined */
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
         tl = XtAppInitialize(&app, "ScaleToMap", NULL, 0, &cargc, vargv,
               SUMA_get_fallbackResources(), NULL, 0);
         dpy = XtDisplay(tl);
         cmap = DefaultColormap(dpy, DefaultScreen(dpy));
         
         XParseColor(dpy, cmap, Name, &color_exact);
         
         /* You need to divide by color_exact.red ,green and blue by 257
         to bring the numbers in the 0..255 range as listed in the rgb.txt file */
         RGB[0] = (float)color_exact.red/255.0/257.0;
         RGB[1] = (float)color_exact.green/255.0/257.0;
         RGB[2] = (float)color_exact.blue/255.0/257.0;
         
         XFreeColormap(dpy, cmap);
         XtDestroyWidget(tl);
      }
   

   if (LocalHead) {
      fprintf (SUMA_STDERR,"%s: %s = %f %f %f\n", 
                        FuncName,  Name, RGB[0], RGB[1], RGB[2]);
   }
   
   SUMA_RETURN (YUP);
}
/*-----------------------------------------------------------------*/

