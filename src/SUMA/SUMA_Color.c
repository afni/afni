#include "SUMA_suma.h"

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
   
   SUMA_RETURN (SM);
}

/*! function to free memory allocated for SUMA_COLOR_MAP */
void SUMA_Free_ColorMap (SUMA_COLOR_MAP* SM)
{
   static char FuncName[]={"SUMA_Free_ColorMap"};
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   if (SM->Name) SUMA_free(SM->Name);
   if (SM->M) SUMA_free2D((char **)SM->M, SM->N_Col);
   if (SM) SUMA_free(SM);

   SUMA_RETURNe;
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
      fprintf (SUMA_STDOUT, "\n\33[1mUsage1: \33[0m MakeColorMap <-fn Fiducials_Ncol> [-pos] [-ah prefix] [-h/-help]\n");
      fprintf (SUMA_STDOUT, "\t Creates a colormap of N colors that contains the fiducial colors.\n");
      fprintf (SUMA_STDOUT, "\t -fn Fiducials_Ncol: Fiducial colors and their indices in the color map are listed in file Fiducials_Ncol.\n");
      fprintf (SUMA_STDOUT, "\t\t Each row contains 4 tab delimited values:\n");
      fprintf (SUMA_STDOUT, "\t\t R G B i\n");
      fprintf (SUMA_STDOUT, "\t\t R G B values are between 0 and 1 and represent the i-th color in the colormap.\n");
      fprintf (SUMA_STDOUT, "\t\t i should be between 0 and N-1, N being the total number of colors in the colormap.\n");
      fprintf (SUMA_STDOUT, "\n\33[1mUsage2: \33[0m MakeColorMap <-f Fiducials> <-nc N> [-sl] [-ah prefix] [-h/-help]\n");
      fprintf (SUMA_STDOUT, "\t Creates a colormap of N colors that contains the fiducial colors.\n");
      fprintf (SUMA_STDOUT, "\t -f Fiducials:  Fiducial colors are listed in an ascii file Fiducials. \n");
      fprintf (SUMA_STDOUT, "\t\t Each row contains 3 tab delimited R G B values between 0 and 1.\n");
      fprintf (SUMA_STDOUT, "\t -nc N: Total number of colors in the color map.\n");
      fprintf (SUMA_STDOUT, "\t -sl: (optional, default is NO) if used, the last color in the Fiducial list is omitted.\n");
      fprintf (SUMA_STDOUT, "\t\t This is useful in creating cyclical color maps.\n");
      fprintf (SUMA_STDOUT, "\n\33[1mCommon options to both usages:\33[0m\n");
      fprintf (SUMA_STDOUT, "\t -ah prefix: (optional, default is RGB values in decimal form)\n");
      fprintf (SUMA_STDOUT, "\t\t use this option if you want a color map formatted to fit in AFNI's .afnirc file.\n");
      fprintf (SUMA_STDOUT, "\t\t the colormap is written out as \n\t\tprefix_01 = #xxxxxxx \n\t\tprefix_02 = #xxxxxxx\n\t\t etc...\n"); 
      /* that's not a useful option, both versions will be written out */
      /*fprintf (SUMA_STDOUT, "\t -pos: (optional, default is NO) create a positive only color map. \n");
      fprintf (SUMA_STDOUT, "\t\t This option is meaningful when combined with -ah option.\n"); */
      fprintf (SUMA_STDOUT, "\t -h or -help: displays this help message.\n");
      fprintf (SUMA_STDOUT, "\n");
      fprintf (SUMA_STDOUT, "Example Usage 1: Creating a colormap of 20 colors that goes from Red to Green to Blue to Yellow to Red.\n");
      fprintf (SUMA_STDOUT, "\n\tThe file FidCol_Nind contains the following:\n");
      fprintf (SUMA_STDOUT, "\t1 0 0 0\n\t0 1 0 5\n\t0 0 1 10\n\t1 1 0 15\n\t1 0 0 19\n\n");
      fprintf (SUMA_STDOUT, "\tThe following command will generate the RGB colormap in decimal form:\n");
      fprintf (SUMA_STDOUT, "\tMakeColorMap -fn FidCol_Nind \n\n");
      fprintf (SUMA_STDOUT, "\tThe following command will generate the colormap and write it as an AFNI color palette file:\n");
      fprintf (SUMA_STDOUT, "\tMakeColorMap -fn FidCol_Nind -ah TestPalette > TestPalette.pal\n\n");
      fprintf (SUMA_STDOUT, "Example Usage 2: Creating a cyclical version of the colormap in usage 1:\n");
      fprintf (SUMA_STDOUT, "\n\tThe file FidCol contains the following:\n");
      fprintf (SUMA_STDOUT, "\t1 0 0\n\t0 1 0\n\t0 0 1\n\t1 1 0\n\t1 0 0\n\n");
      fprintf (SUMA_STDOUT, "\tThe following command will generate the RGB colormap in decimal form:\n");
      fprintf (SUMA_STDOUT, "\tMakeColorMap -f FidCol -sl -nc 20 \n\n");
      fprintf (SUMA_STDOUT, "To read in a new colormap into AFNI, either paste the contents of TestPalette.pal\n");
      fprintf (SUMA_STDOUT, "in your .afnirc file or read the .pal file using AFNI as follows:\n");
      fprintf (SUMA_STDOUT, "1- run afni\n2- Define Function --> right click on Inten (over colorbar) --> Read in palette (choose TestPalette.pal)\n");
      fprintf (SUMA_STDOUT, "3- set the #colors chooser (below colorbar) to 20 (the number of colors in TestPalette.pal).\n");
      /*fprintf (SUMA_STDOUT, "\t To Compile:\ngcc  -DSUMA_MakeColorMap_STAND_ALONE -Wall -Wno-unused-variable -o SUMA_MakeColorMap SUMA_Color.c SUMA_lib.a libmri.a  -I/usr/X11R6/include -I./ -L/usr/lib -L/usr/X11R6/lib -lMesaGLwM -lMesaGLw -lGLU -lGL -lXmu -lXm -lXt -lXext -lX11 -lm\n");*/
      fprintf (SUMA_STDOUT, "\t\t Ziad S. Saad & Rick R. Reynolds SSCC/NIMH/NIH ziad@nih.gov \tTue Apr 23 14:14:48 EDT 2002\n\n");
   }
 
int main (int argc,char *argv[])
{/* Main */
   char FuncName[]={"SUMA_MakeColorMap-main"}, *FidName = NULL, *Prfx = NULL, h[9]; 
   int Ncols = 0, N_Fid, kar, i, ifact, *Nind = NULL;
   float **Fid, **M;
   SUMA_Boolean brk, SkipLast, AfniHex, PosMap, Usage1, Usage2, LocalHead = NOPE;
   SUMA_COLOR_MAP *SM;
      
   /* allocate space for CommonFields structure and initialize debug*/
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   SUMAg_CF->InOut_Notify = NOPE;
   
   if (argc < 3) {
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
   while (kar < argc) { /* loop accross command ine options */
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
          SUMA_MakeColorMap_usage();
         exit (1);
      }
      if (!brk && (strcmp(argv[kar], "-v") == 0))
      {
         LocalHead = YUP;
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
   if (Usage1 && Usage2) {
      fprintf (SUMA_STDERR,"Error %s: Mixing options from both usage modes.\n", FuncName);
      exit(1);
   }
      
   if (!SUMA_filexists (FidName)) {
      fprintf (SUMA_STDERR,"Error %s: File %s could not be found.\n", FuncName, FidName);
      exit(1);
   }

   if (PosMap) {
      fprintf (SUMA_STDERR,"\nWarning %s: -pos option is obsolete.\n", FuncName);
   }
   
   /* read the fiducials file */
   N_Fid = SUMA_float_file_size(FidName);
   if (N_Fid < 1) {
      fprintf (SUMA_STDERR,"Error %s: File %s could not be read or is badly formatted.\n", FuncName, FidName);
      exit (1);
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

      SUMA_Read_2Dfile (FidName, Fid, 3, N_Fid / 3);
      /*   SUMA_disp_mat (Fid, N_Fid / 3, 3,1);*/
   
      /* now create the color map */
      SM = SUMA_MakeColorMap (Fid, N_Fid/3, Ncols, SkipLast, FuncName);
      if (SM == NULL) {
         fprintf (SUMA_STDERR,"Error %s: Error in SUMA_MakeColorMap.\n", FuncName);
         exit(1);
      }
   } else { /* second usage */
      if (N_Fid % 4) {
         fprintf (SUMA_STDERR,"Error %s: Not all rows in %s appear to have RGB N quadruplets.\n", FuncName, FidName);
         exit (1);
      }

      Fid = (float **) SUMA_allocate2D (N_Fid / 4, 4, sizeof(float));
      Nind = (int *) SUMA_calloc (N_Fid/4, sizeof(int));
      if (Fid == NULL || !Nind) {
         fprintf (SUMA_STDERR,"Error %s: Could not allocate for Fid or Nind.\n", FuncName);
         exit(1);
      }
      
      SUMA_Read_2Dfile (FidName, Fid, 4, N_Fid / 4);
      /*   SUMA_disp_mat (Fid, N_Fid / 4, 4,1);*/
      
      /* copy fourth column to Nind */
      for (i=0; i < N_Fid/4; ++i) {
         Nind[i] = (int)Fid[i][3];
      }
   
      /* now create the color map */
      SM = SUMA_MakeColorMap_v2 (Fid, N_Fid/4, Nind, SkipLast, FuncName); 
      if (SM == NULL) {
         fprintf (SUMA_STDERR,"Error %s: Error in SUMA_MakeColorMap.\n", FuncName);
         exit(1);
      }
      Ncols = SM->N_Col;
   }
   
   M = SM->M;

   if (AfniHex && Ncols > 100) {
         fprintf (SUMA_STDERR,"Error %s: Cannot write a colormap of more than 100 colors in Afni's hex format.\n", FuncName);
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
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

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
   
   /* Now go through values and interpolate onto index of colormap */
   MinCol = 0.0; MaxCol = (float)ColMap->N_Col; 
   Vrange = Vmax - Vmin; 
   if (Vrange < 0) {
      fprintf (SUMA_STDERR,"Error %s: Vmax < Vmin.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (Vrange > 0) {
      mxColindex = ColMap->N_Col -1;
      for (i=0; i < N_V; ++i) {
         if (!SV->isMasked[i]) {
            Vscl = (V[i] - Vmin) / Vrange * mxColindex; 
            /*now linearly interpolate between the two closest colors in the color map */
            i0 = (int)(Vscl); 
            i1=i0+1;
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

         } else {
            SV->cM[i][0] = Opt->MaskColor[0]; SV->cM[i][1] = Opt->MaskColor[1]; SV->cM[i][2] = Opt->MaskColor[2]; 
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
   SUMA_RETURN (YUP);
} 

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
   
   SUMA_RETURN (Opt);

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
      SUMA_CMAP_MATLAB_DEF_BGYR64
   \ret CM (SUMA_COLOR_MAP*) color map structure (NULL in case of error)
*/

SUMA_COLOR_MAP * SUMA_GetStandardMap (SUMA_STANDARD_CMAP mapname)
   {   static char FuncName[]={"SUMA_GetStandardMap"};
      float **Fiducials;
      int k;
      int *Nind;
      int Ncols, NFid;
      SUMA_COLOR_MAP * CM;
      
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

      switch (mapname) {
         case SUMA_CMAP_RGYBR20:
            {
               char Name[]={"RGBYR20"};
               
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
               CM = SUMA_MakeColorMap (Fiducials, k, 20, YUP, Name);
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
               char Name[]={"BGYR19"};
               
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
               CM = SUMA_MakeColorMap (Fiducials, k, 19, NOPE, Name);
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
               char Name[]={"GRAY20"};
               
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
               CM = SUMA_MakeColorMap (Fiducials, k, 20, NOPE, Name);
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
               char Name[]={"nGRAY20"};
               
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
               CM = SUMA_MakeColorMap (Fiducials, k, 20, NOPE, Name);
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
               char Name[]={"BW20"};
               
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
               CM = SUMA_MakeColorMap (Fiducials, k, 20, NOPE, Name);
               /* free Fiducials */
               SUMA_free2D((char **)Fiducials, k);
               
               if (!CM) {
                  fprintf (SUMA_STDERR,"Error %s: Failed to create CM.\n", FuncName);
                  SUMA_RETURN (NULL);   
               }
               break;
            }
            case SUMA_CMAP_MATLAB_DEF_BGYR64:
            {
               /* default matlab color map */
               char Name[]={"MATLAB_DEF_BGYR64"};
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
               CM = SUMA_MakeColorMap_v2 (Fiducials, k, Nind, NOPE, Name);
               
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
      fprintf (SUMA_STDOUT, "\n\33[1mUsage: \33[0m SUMA_ScaleToMap <-v/-iv IntFile>  [-cmap MapType] [-clp/-perc_clp clp0 clp1] [-msk msk0 msk1] [-msk_col R G B] [-br BrightFact] [-h/-help]\n");
      fprintf (SUMA_STDOUT, "\t -v IntFile:  Node value vector in ascii file, one node value per line. \n");
      fprintf (SUMA_STDOUT, "\t -iv IntFile:  Node index and value Matrix in ascii file, node index and one node value per line. \n");
      fprintf (SUMA_STDOUT, "\t -v and -iv are mutually exclusive.\n");
      fprintf (SUMA_STDOUT, "\t -cmap MapType: (optional, default RGYBR20) choose one of the standard colormaps available with SUMA.\n");
      fprintf (SUMA_STDOUT, "\t\t RGYBR20, BGYR19, BW20, GRAY20, MATLAB_DEF_BGYR64\n");
      fprintf (SUMA_STDOUT, "\t -clp/-perc_clp clp0 clp1: (optional, default no clipping) clips values in IntVect.\n");
      fprintf (SUMA_STDOUT, "\t\t if -clp is used them IntVect is clipped to clp0 if IntVect < clp0 and clp1 if IntVect > clp1\n");
      fprintf (SUMA_STDOUT, "\t\t if -perc_clp is used them IntVect is clipped to the values corresponding to clp0 and clp1 percentile.\n");
      fprintf (SUMA_STDOUT, "\t\t Please don't use -clp and -perc_clp simultaneously.\n");
      fprintf (SUMA_STDOUT, "\t -msk msk0 msk1: (optinal, default is no masking) Values in IntVect (BEFORE clipping is performed) \n");
      fprintf (SUMA_STDOUT, "\t\t between [msk0 msk1] are masked by the masking color.\n");
      fprintf (SUMA_STDOUT, "\t -msk_col R G B: (optional, default is 0.3 0.3 0.3) Sets the color of masked voxels.\n");
      fprintf (SUMA_STDOUT, "\t -br BrightFact: (optional, default is 1) Sets the brightness factor applied to the colors \n");
      fprintf (SUMA_STDOUT, "\t\t of the colormap and the mask color.\n");
      fprintf (SUMA_STDOUT, "\t -h or -help: displays this help message.\n");
      fprintf (SUMA_STDOUT, "\n");
      /*fprintf (SUMA_STDOUT, "\t To Compile:\n gcc  -DSUMA_ScaleToMap_STAND_ALONE -Wall -Wno-unused-variable -o SUMA_ScaleToMap SUMA_Color.c SUMA_lib.a libmri.a  -I/usr/X11R6/include -I./ -lm\n");*/
      fprintf (SUMA_STDOUT, "\t\t Ziad S. Saad SSCC/NIMH/NIH ziad@nih.gov \tJuly 31/02\n\n");
   }

int main (int argc,char *argv[])
{/* Main */
   char FuncName[]={"SUMA_ScaleToMap-main"}, *IntName = NULL, *Prfx, h[9]; 
   int N_V, N_Va, N_Int, kar, k, ii;
   int Vminloc, Vmaxloc, *iV = NULL;
   float Vmin, Vmax, brfact;
   float *V = NULL, *Vsort = NULL,  *Va = NULL;
   float ClipRange[2], MaskColor[3], MaskRange[2];
   SUMA_Boolean ApplyClip, ApplyMask, setMaskCol, ApplyPercClip, Vopt, iVopt;
   SUMA_Boolean brk;
   SUMA_COLOR_MAP *CM;
   SUMA_SCALE_TO_MAP_OPT * OptScl;
   SUMA_STANDARD_CMAP MapType;
   SUMA_COLOR_SCALED_VECT * SV;
   
   if (argc < 3) {
      SUMA_ScaleToMap_usage();
      exit (1);
   }
   
   /* allocate space for CommonFields structure and initialize debug*/
   SUMAg_CF = SUMA_Create_CommonFields ();
   if (SUMAg_CF == NULL) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Create_CommonFields\n", FuncName);
      exit(1);
   }
   
   SUMAg_CF->InOut_Notify = NOPE;

   kar = 1;
   brfact = 1; /* the brightness factor */
   MaskColor[0] = MaskColor[1] = MaskColor[2] = 0.3;
   ApplyClip = NOPE;
   ApplyPercClip = NOPE;
   ApplyMask = NOPE;
   setMaskCol = NOPE;
   Vopt = NOPE;
   iVopt = NOPE;
   MapType = SUMA_CMAP_RGYBR20;
   brk = NOPE;
   while (kar < argc) { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
          SUMA_ScaleToMap_usage();
         exit (1);
      }
      
      if (!brk && (strcmp(argv[kar], "-v") == 0)) {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -v ");
            exit (1);
         }
         IntName = argv[kar];
         Vopt = YUP;
         brk = YUP;
      }      
      
      if (!brk && (strcmp(argv[kar], "-iv") == 0)) {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -iv ");
            exit (1);
         }
         IntName = argv[kar];
         iVopt = YUP;
         brk = YUP;
      }   
      
      if (!brk && (strcmp(argv[kar], "-br") == 0)) {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -br ");
            exit (1);
         }
         brfact = atof(argv[kar]);

         brk = YUP;
      }   
      
      if (!brk && (strcmp(argv[kar], "-clp") == 0)) {
         kar ++;
         if (kar+1 >= argc)  {
              fprintf (SUMA_STDERR, "need 2 arguments after -clp ");
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
      
      if (!brk && (strcmp(argv[kar], "-cmap") ==0)) {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need 1 arguments after -cmap ");
            exit (1);
         }
         MapType = SUMA_CMAP_UNDEFINED;
         if (strcmp(argv[kar], "RYGBR20") == 0)    MapType = SUMA_CMAP_RGYBR20;
         if (strcmp(argv[kar], "BW20") == 0)    MapType = SUMA_CMAP_BW20;
         if (strcmp(argv[kar], "GRAY20") == 0)    MapType = SUMA_CMAP_GRAY20;
         if (strcmp(argv[kar], "BGYR19") == 0)    MapType = SUMA_CMAP_BGYR19;
         if (strcmp(argv[kar], "MATLAB_DEF_BGYR64") == 0)    MapType = SUMA_CMAP_MATLAB_DEF_BGYR64;
   
         if (MapType == SUMA_CMAP_UNDEFINED) {
            fprintf (SUMA_STDERR, "Color map type not recognized.\n");
            exit (1);
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
   
   /* check input */
   if (!SUMA_filexists (IntName)) {
      fprintf (SUMA_STDERR,"Error %s: File %s could not be found.\n", FuncName, IntName);
      exit(1);
   }
   
   if (ApplyPercClip && ApplyClip) {
      fprintf (SUMA_STDERR,"Error %s: Simultaneous use of -clp and -perc_clp. You should be punished.\n", FuncName);
      exit(1);
   }
   
   if (iVopt && Vopt) {
      fprintf (SUMA_STDERR,"Error %s: Simultaneous use of -v and -iv. You should be ashamed of yourself.\n", FuncName);
      exit(1);
   }
   
   N_Va =  SUMA_float_file_size(IntName);
   if (N_Va < 1) {
      fprintf (SUMA_STDERR,"Error %s: File %s could not be read or is badly formatted.\n", FuncName, IntName);
      exit (1);
   }
   
   if (brfact <=0 || brfact > 1) {
      fprintf (SUMA_STDERR,"Error %s: BrightFact must be > 0 and <= 1.\n", FuncName);
      exit (1);
   }
   
   if (MaskColor[0] < 0 || MaskColor[0] > 1 || MaskColor[1] < 0 || MaskColor[1] > 1 || MaskColor[2] < 0 || MaskColor[2] > 1) {
      fprintf (SUMA_STDERR,"Error %s: MaskColor values must be >=0 <=1.\n", FuncName);
      exit(1);
   }
   
   /* if you decide to have two values per column */
   if (iVopt && (N_Va % 2)) {
         fprintf (SUMA_STDERR,"Error %s: Number of values in %s not divisible by 2.\n", FuncName, IntName);
         exit (1);
      }
   
   /* allocate for Nodes */
   Va = (float *) SUMA_calloc (N_Va, sizeof(float));
   if (!Va) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for Va.\n", FuncName);
      exit(1);
   }
   SUMA_Read_file (Va, IntName, N_Va);
   
   if (Vopt) {
      V = Va;
      N_V = N_Va;
   } else {
      N_V = N_Va/2;
      V = (float *) SUMA_calloc (N_V, sizeof(float));
      iV = (int *) SUMA_calloc (N_V, sizeof(int));
      if (!V || !iV) {
         fprintf (SUMA_STDERR,"Error %s: Could not allocate for V or iV.\n", FuncName);
         exit(1);
      }
      k = 0;
      for (ii=0; ii < N_V; ++ii) {
         iV[ii] = (int)Va[k]; ++k;
         V[ii] = Va[k]; ++k;
      }
   }
   
   /* read values per node */
   /* SUMA_disp_vect (V, 10); */
   
   /* find the min/max of V */
   SUMA_MIN_MAX_VEC(V, N_V, Vmin, Vmax, Vminloc, Vmaxloc)
   /* fprintf (SUMA_STDERR,"%s: Vmin=%f, Vmax = %f\n", FuncName, Vmin, Vmax);*/ 
   
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
   /* get the color map */
   CM = SUMA_GetStandardMap (MapType);
   if (CM == NULL) {
      fprintf (SUMA_STDERR,"Error %s: Could not get standard colormap.\n", FuncName);
      exit (1); 
   }
   /* show the colromap on STDERR */
   /*fprintf (SUMA_STDERR, "%s: Colormap used:\n", FuncName);
   for (k=0; k < CM->N_Col; ++k) fprintf (SUMA_STDERR, "%f %f %f\n", CM->M[k][0], CM->M[k][1], CM->M[k][2]);*/

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

   OptScl->BrightFact = brfact;
      
   /* map the values in V to the colormap */
      /* allocate space for the result */
      SV = SUMA_Create_ColorScaledVect(N_V);
      if (!SV) {
         fprintf (SUMA_STDERR,"Error %s: Could not allocate for SV.\n", FuncName);
         exit(1);
      }
      
      /* finally ! */
      /*fprintf (SUMA_STDERR,"%s: 1st color in map %f %f %f\n", FuncName, CM->M[0][0], CM->M[0][1],CM->M[0][2]);*/
      if (!SUMA_ScaleToMap (V, N_V, Vmin, Vmax, CM, OptScl, SV)) {
         fprintf (SUMA_STDERR,"Error %s: Failed in SUMA_ScaleToMap.\n", FuncName);
         exit(1);
      }

   /* Now write the colored vector back to disk */
   
   for (k=0; k < N_V; ++k) {
      if (Vopt) {
         fprintf (SUMA_STDOUT, "%d %f %f %f\n", k, SV->cM[k][0], SV->cM[k][1], SV->cM[k][2]);
      } else {
         fprintf (SUMA_STDOUT, "%d %f %f %f\n", iV[k], SV->cM[k][0], SV->cM[k][1], SV->cM[k][2]);
      }
   }
   
   /* freeing time */
   if (Va) SUMA_free(Va);
   if (iVopt){
      if (V) SUMA_free(V);
      if (iV) SUMA_free(iV);
   }
   if (CM) SUMA_Free_ColorMap (CM);
    if (OptScl) SUMA_free(OptScl);
   if (SV) SUMA_Free_ColorScaledVect (SV);

   
   exit (0);
}   

#endif

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
   \param Name (char *): A character string no longer than SUMA_MAX_LABEL_LENGTH-1 containing the name of the color overlay
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
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   Sover = (SUMA_OVERLAYS *)SUMA_malloc(sizeof(SUMA_OVERLAYS));
   if (!Sover) {
      fprintf (SUMA_STDERR,"Error %s: Could not allocate for Sover.\n", FuncName);
      SUMA_RETURN (NULL);
   }
   if (strlen(Name) - 1 > SUMA_MAX_LABEL_LENGTH) {
      fprintf (SUMA_STDERR,"Error %s: Name must be less than %d characters in length.\n", FuncName, SUMA_MAX_LABEL_LENGTH-1);
      SUMA_free(Sover);
      SUMA_RETURN (NULL);
   } else {
      strcpy (Sover->Name, Name);
   }
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
   Function to free an overlay structure 
   ans = SUMA_FreeOverlayPointer (Sover); 
   \param Sover (SUMA_OVERLAYS * ) 
   \ret ans (SUMA_Boolean) (YUP/NOPE)
   
   \sa SUMA_SetPlaneOrder
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
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   for (i=0; i < N_Overlays; ++i) {
      if (!strcmp(Overlays[i]->Name, Name)) {
         *OverInd = i;
         /*fprintf (SUMA_STDOUT,"%s: Found overlay plane %s, indexed %d.\n", FuncName, Name, i);*/
         SUMA_RETURN (Overlays[i]);
      }
   }
   
   /*fprintf (SUMA_STDOUT,"%s: Overlay plane %s was not found.\n", FuncName, Name);*/
   
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
      if (Overlays[j]->Show) {
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
      
      i = ShowOverlays[j];
      
      /* is this a full listing */
      if (LocalHead) fprintf (SUMA_STDOUT, "%s: Full listing flag: %d\n", FuncName, Overlays[i]->NodeDef[0]);
      if (Overlays[i]->NodeDef[0] < 0) {         Fill = NOPE;   /* Full list, no need to fill up unvisited nodes at the end */   } 
      else {         Full = NOPE; /* Not a full list */      }
      
      if (j > 0) {
         /* is this a Global Factor */
         if (Overlays[i]->GlobalOpacity < 0) {         Glob = NOPE;      }

         /* is this a Local Factor */
         if (Overlays[i]->LocalOpacity[0] < 0) {         Locl = NOPE;      }
      } else {
         Glob = NOPE; Locl = NOPE;
      }
      
      if (LocalHead) 
         fprintf (SUMA_STDOUT,"%s: Building color layer %d Overlay #%d: %s ...\nFull=%d, Glob=%d, Locl=%d,Fill=%d\n", \
         FuncName, j, i, Overlays[i]->Name, (int)Full, (int)Glob, (int)Locl, (int)Fill);
      
         
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
   This is the first incarnation: NOW OBSOLETE
   
   function to turn color overlay planes into GL color array
   
   ans = SUMA_Overlays_2_GLCOLAR4_old(Overlays, N_Overlays, glar_ColorList, N_Node);
   
   Overlays (SUMA_OVERLAYS **) a pointer to the vector of overlay planes structure pointers
   N_Overlays (int) number of overlay plane structures
   glar_ColorList (GLfloat *) pointer to vector (4*SO->N_Node long) that contains the node colors 
   N_Node (int) total number of nodes in Surface NOT in color overlay plane
   
*/
SUMA_Boolean SUMA_Overlays_2_GLCOLAR4_old(SUMA_OVERLAYS ** Overlays, int N_Overlays, GLfloat *glcolar, int N_Node)
{
   static char FuncName[]={"SUMA_Overlays_2_GLCOLAR4_old"};
   int ShowOverLays[SUMA_MAX_OVERLAYS], iloc[SUMA_MAX_OVERLAYS];
   int i, j, NshowOverlays;
   SUMA_Boolean Fill, Full, Glob, Locl, add;
   SUMA_Boolean *isColored;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   fprintf (SUMA_STDOUT, "%s: Showing all overlay planes.\n", FuncName);
   SUMA_Show_ColorOverlayPlanes (Overlays, N_Overlays); 
   

   /* get the indices into the color structure vector of overlays to be shown */
   NshowOverlays = 0;
   for (j=0; j < N_Overlays; ++j) {
      if (Overlays[j]->Show) {
         ShowOverLays[NshowOverlays] = j; ++ NshowOverlays;
      }
   }

   isColored = (SUMA_Boolean *) SUMA_calloc (N_Node, sizeof(SUMA_Boolean));
   if (!isColored) {
      fprintf (SUMA_STDERR,"Error %s: Failed to allocate for isColored.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (!NshowOverlays) { /* nothing to see here */
      SUMA_FillBlanks_GLCOLAR4(isColored, N_Node, SUMA_GRAY_NODE_COLOR, SUMA_GRAY_NODE_COLOR, SUMA_GRAY_NODE_COLOR, glcolar);
      SUMA_free(isColored);
      SUMA_RETURN (YUP);
   }
   
   /* start building the node colors */
   Full = YUP;
   Glob = YUP;
   Locl = YUP;
   Fill = YUP; 
   for (j=0; j<NshowOverlays; ++j) {
      if (j==0) { add = NOPE; }
      else { add = YUP; }
      
      i = ShowOverLays[j];
      
      /* is this a full listing */
      fprintf (SUMA_STDOUT, "%s: %d\n", FuncName, Overlays[i]->NodeDef[0]);
      if (Overlays[i]->NodeDef[0] < 0) {         Fill = NOPE;   /* Full list, no need to fill up unvisited nodes at the end */   } 
      else {         Full = NOPE; /* Not a full list */      }
      
      /* is this a Global Factor */
      if (Overlays[i]->GlobalOpacity < 0) {         Glob = NOPE;      }
      
      /* is this a Local Factor */
      if (Overlays[i]->LocalOpacity[0] < 0) {         Locl = NOPE;      }
      
      /*
      fprintf (SUMA_STDOUT,"%s: Building color layer %d Overlay #%d: %s ...\nFull=%d, Glob=%d, Locl=%d, add=%d, Fill=%d\n", \
         FuncName, j, i, Overlays[i]->Name, (int)Full, (int)Glob, (int)Locl, (int)add, (int)Fill);
       */
         
      /* call the appropriate macro to add the overlay */
      if (Full && Glob && Locl) {
         /*fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBmat_FullGlobLoc2_GLCOLAR4 ...\n", FuncName);*/
         SUMA_RGBmat_FullGlobLoc2_GLCOLAR4(Overlays[i]->ColMat, glcolar, N_Node, Overlays[i]->GlobalOpacity, Overlays[i]->LocalOpacity, add);         
      }
      
      if (!Full && Glob && Locl) {
         /*fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBmat_PartGlobLoc2_GLCOLAR4 ...\n", FuncName);*/
         SUMA_RGBmat_PartGlobLoc2_GLCOLAR4(Overlays[i]->ColMat, Overlays[i]->NodeDef, glcolar, Overlays[i]->N_NodeDef, isColored, Overlays[i]->GlobalOpacity, Overlays[i]->LocalOpacity, add, N_Node);
      }
      
      if (Full && !Glob && Locl) {
         /*fprintf (SUMA_STDOUT,"%s: Calling  SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4...\n", FuncName);*/
         SUMA_RGBmat_FullNoGlobLoc2_GLCOLAR4(Overlays[i]->ColMat, glcolar, N_Node, Overlays[i]->LocalOpacity, add);         
      }
      
      if (!Full && !Glob && Locl) {
         /*fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBmat_PartNoGlobLoc2_GLCOLAR4 ...\n", FuncName);*/
         SUMA_RGBmat_PartNoGlobLoc2_GLCOLAR4(Overlays[i]->ColMat, Overlays[i]->NodeDef, glcolar, Overlays[i]->N_NodeDef, isColored, Overlays[i]->LocalOpacity, add, N_Node);
      }
      
      if (Full && !Glob && !Locl) {
         /*fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBmat_FullNoGlobNoLoc2_GLCOLAR4 ...\n", FuncName);*/
         SUMA_RGBmat_FullNoGlobNoLoc2_GLCOLAR4(Overlays[i]->ColMat, glcolar, N_Node, add);         
      }
      
      if (!Full && !Glob && !Locl) {
         /*fprintf (SUMA_STDOUT,"%s: Calling SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4 ...\n", FuncName); */
         SUMA_RGBmat_PartNoGlobNoLoc2_GLCOLAR4(Overlays[i]->ColMat, Overlays[i]->NodeDef, glcolar, Overlays[i]->N_NodeDef, isColored, add, N_Node);
      }
      
      if (Full && Glob && !Locl) {
         /*fprintf (SUMA_STDOUT,"%s: Calling  SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4...\n", FuncName);*/
         SUMA_RGBmat_FullGlobNoLoc2_GLCOLAR4(Overlays[i]->ColMat, glcolar, N_Node, Overlays[i]->GlobalOpacity,  add);
      }
       
      if (!Full && Glob && !Locl) {
         /*fprintf (SUMA_STDOUT,"%s: Calling  SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4...\n", FuncName);*/
         SUMA_RGBmat_PartGlobNoLoc2_GLCOLAR4(Overlays[i]->ColMat, Overlays[i]->NodeDef, glcolar, Overlays[i]->N_NodeDef, isColored, Overlays[i]->GlobalOpacity,  add, N_Node);
      }
   }
   
   if (Fill) { /* nothing to see here */
      /*fprintf (SUMA_STDOUT,"%s: Some nodes received no colors from any of the overplanes, filling them with background color ...\n", FuncName);*/
      SUMA_FillBlanks_GLCOLAR4(isColored, N_Node, SUMA_GRAY_NODE_COLOR, SUMA_GRAY_NODE_COLOR, SUMA_GRAY_NODE_COLOR, glcolar);
      SUMA_RETURN (YUP);
   }

   SUMA_free(isColored);

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
   int i;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   for (i=0; i < N_Overlays; ++i) {
      fprintf (SUMA_STDOUT, "%s: Overlay plane %s, order %d, indexed %d.\n", FuncName, Overlays[i]->Name, Overlays[i]->PlaneOrder, i);
      if (!Overlays[i]) {
         fprintf (SUMA_STDOUT, "is NULL\n");
         continue;
      }
      fprintf (SUMA_STDOUT, "%s, Show=%d, N_Alloc=%d\n", Overlays[i]->Name, (int)Overlays[i]->Show, Overlays[i]->N_Alloc);
      fprintf (SUMA_STDOUT, "\tNodeDef: %d values, [%d %d %d %d %d...]\n", \
         Overlays[i]->N_NodeDef, Overlays[i]->NodeDef[0], Overlays[i]->NodeDef[1], Overlays[i]->NodeDef[2], \
         Overlays[i]->NodeDef[3], Overlays[i]->NodeDef[4]);
      fprintf (SUMA_STDOUT, "\tColMat:[%f %f %f; %f %f %f; %f %f %f ....]\n", \
         Overlays[i]->ColMat[0][0], Overlays[i]->ColMat[0][1],Overlays[i]->ColMat[0][2],\
         Overlays[i]->ColMat[1][0], Overlays[i]->ColMat[1][1],Overlays[i]->ColMat[1][2],\
         Overlays[i]->ColMat[2][0], Overlays[i]->ColMat[2][1],Overlays[i]->ColMat[2][2]);
      fprintf (SUMA_STDOUT, "\tGlobal Fact: %f\n\tLocalOpacity: [%f %f %f %f %f...]", \
         Overlays[i]->GlobalOpacity, Overlays[i]->LocalOpacity[0], Overlays[i]->LocalOpacity[1],\
         Overlays[i]->LocalOpacity[2], Overlays[i]->LocalOpacity[3], Overlays[i]->LocalOpacity[4]);
      fprintf (SUMA_STDOUT, "\n");
   }

   SUMA_RETURN (YUP);
}
   
/*! 

   ans = SUMA_SetPlaneOrder (Overlays, N_Overlays, Name, NewOrder);
   \param Overlays (SUMA_OVERLAYS **) vector of  pointers to overlay structures
   \param N_Overlays (int) Number of overlay structure pointers 
   \param Name (const char *) Name of plane to be assigned the NewOrder
   \param NewOrder (int) layer order of Ovelay plane
   \ret ans (SUMA_Boolean) YUP/NOPE
   
   To put a plane on the top, use NewOrder = N_Ovelays -1 

   IF YOU EVER START TO REMOVE SINGLE OVERLAY PLANES AT A TIME FROM SO, You will need to repack the orders
   \sa SUMA_FreeOverlayPointer
*/

SUMA_Boolean SUMA_SetPlaneOrder (SUMA_OVERLAYS **Overlays, int N_Overlays, const char* Name, int NewOrder)
{
   static char FuncName[]={"SUMA_SetPlaneOrder"};
   int i, PlaneIndex = -1;
   SUMA_Boolean Found;
   
   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_IN_NOTIFY(FuncName); }

   if (!SUMA_Fetch_OverlayPointer (Overlays, N_Overlays, Name, &PlaneIndex)) {
      fprintf (SUMA_STDERR,"Error %s: Plane %s not found.\n", FuncName, Name);
      SUMA_RETURN (NOPE);
   }

   if (PlaneIndex < 0 || PlaneIndex >= N_Overlays) {
      fprintf (SUMA_STDERR,"Error %s: PlaneIndex <0 || >= N_Overlays.\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   if (NewOrder < 0 || NewOrder >= N_Overlays) {
      fprintf (SUMA_STDERR,"Error %s: NewOrder <0 || NewOrder >= .\n", FuncName);
      SUMA_RETURN (NOPE);
   }
   
   /* find out if NewOrder alreay exists */
   i = 0; 
   Found = NOPE;
   while (i < N_Overlays && !Found) {
      if (Overlays[i]->PlaneOrder == NewOrder) {
         Found = YUP;
      }else ++i;
   }
   
   /* if a new order, assign it and leave */
   if (!Found) {
      Overlays[PlaneIndex]->PlaneOrder = NewOrder;
      SUMA_RETURN (YUP);
   }
   
   /* not a new order, some shifting required */
   for (i=0; i < N_Overlays; ++i) {
      if (i != PlaneIndex) {
         if (Overlays[i]->PlaneOrder >= NewOrder) ++Overlays[i]->PlaneOrder;
      }
   }
   Overlays[PlaneIndex]->PlaneOrder = NewOrder;
   
   /* dbg 
   for (i=0; i < N_Overlays; ++i) {
      fprintf (SUMA_STDERR,"%s: Overlay Plane %s, order %d.\n", FuncName, Overlays[i]->Name, Overlays[i]->PlaneOrder);
   }
   */

   if (SUMAg_CF->InOut_Notify) { SUMA_DBG_OUT_NOTIFY(FuncName); }
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
