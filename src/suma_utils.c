/* 
   Assortment of generic functions needed to support I/O functions
   in suma_datasets.c 
*/

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <string.h>
#include <sys/time.h>
#include <math.h>
#include "mrilib.h"
#include "niml.h"
#include "../niml/niml_private.h"
#include "xutil.h"


#include "suma_suma.h"

#if defined SUMA_COMPILED
   extern SUMA_CommonFields *SUMAg_CF;
   extern int SUMAg_N_DOv; 
   extern SUMA_DO *SUMAg_DOv;
#endif 

/*! ********** Begin Multiplexed Vectors Functions ************* */
/*!
   \brief A function to create a multiplexed vector structure
   \param tp (SUMA_VARTYPE) type of data in array. Only SUMA_double for now.
   \param N_dims (int) Number of array dimensions
   \param *dims (int) Size of each dimension
   \param first_dim_first (byte) 1 = first dimension first 
                                 (column major, for a matrix) 
                                 only 1 is accepted at the moment
   \return mxv (SUMA_MX_VEC *) the structure with enough memory allocated to 
                               the pointer of interest. 
                               (mxv->dv for SUMA_double).
   - Use SUMA_FreeMxVec to free mxv
   - see macros mxvd* to handle data in mxv
*/
SUMA_MX_VEC *SUMA_NewMxNullVec(SUMA_VARTYPE tp, int N_dims, int *dims, byte first_dim_first)
{
   static char FuncName[]={"SUMA_NewMxNullVec"};
   SUMA_MX_VEC *mxv=NULL;
   int n_vals=0, i = 0;
   
   SUMA_ENTRY;
   
   if (first_dim_first != 1) {
      SUMA_SL_Err("first_dim_first must be 1 for now");
      SUMA_RETURN(NULL);
   }
   
   if (N_dims < 1) {
      SUMA_SL_Err("N_dims < 1");
      SUMA_RETURN(NULL);
   } else if (N_dims > SUMA_MX_VEC_MAX_DIMS-1) {
      SUMA_SL_Err("N_dims > 49");
      SUMA_RETURN(NULL);
   }
   if (!dims) {
      SUMA_SL_Err("NULL dims");
      SUMA_RETURN(NULL);
   }
   mxv = (SUMA_MX_VEC *)SUMA_calloc(1,sizeof(SUMA_MX_VEC));
   mxv->fdf = 1;
   mxv->bv = NULL;
   mxv->sv = NULL;
   mxv->iv = NULL;
   mxv->fv = NULL;
   mxv->dv = NULL;
   mxv->cv = NULL;
   mxv->v = NULL;
   mxv->m = NULL;
   mxv->N_dims = N_dims;
   mxv->N_vals = dims[0]; 
   mxv->dims[0] = dims[0];
   for (i=1; i<N_dims; ++i) {
      mxv->N_vals = mxv->N_vals * dims[i]; 
      mxv->dims[i] = dims[i];
   }
   for (i=N_dims; i< SUMA_MX_VEC_MAX_DIMS; ++i) mxv->dims[i] = 1;
   if (mxv->N_vals <= 0) {
      SUMA_SL_Err("Negative dims");
      SUMA_free(mxv); SUMA_RETURN(NULL);
   }
   
   mxv->tp = tp;
   
   /* mutlipliers for first_dim_first */
   mxv->fdfm[0] = mxv->dims[0];
   for (i=1; i<N_dims-1; ++i) {
      mxv->fdfm[i] = mxv->fdfm[i-1]*mxv->dims[i];
   }
   
   SUMA_RETURN(mxv);
   
}

int SUMA_MxVecInit(SUMA_MX_VEC *mxv, void *val)
{
   static char FuncName[]={"SUMA_MxVecInit"};
   int i, ii;
   byte bb;
   short ss;
   float ff;
   complex cc;
   double dd;
   
   SUMA_ENTRY;
   
   if (!mxv->v) {
      SUMA_S_Err("null vector pointer");
      SUMA_RETURN(0);
   }
   switch (mxv->tp) {
      case SUMA_byte:
         bb = *((byte *)val);
         mxv->bv = (byte *)mxv->v; 
         for (i=0;i<mxv->N_vals;++i) mxv->bv[i] = bb;
         break;
      case SUMA_short:
         ss = *((short *)val);
         mxv->sv = (short *)mxv->v;         
         for (i=0;i<mxv->N_vals;++i) mxv->sv[i] = ss;
         break;
      case SUMA_int:
         ii = *((int *)val);
         mxv->iv = (int *)mxv->v;         
         for (i=0;i<mxv->N_vals;++i) mxv->iv[i] = ii;
         break;
      case SUMA_float:
         ff = *((float *)val);
         mxv->fv = (float *)mxv->v;         
         for (i=0;i<mxv->N_vals;++i) mxv->fv[i] = ff;
         break;
      case SUMA_double:
         dd = *((double *)val);
         mxv->dv = (double *)mxv->v;         
         for (i=0;i<mxv->N_vals;++i)  mxv->dv[i] = 1.0; 
         break;
      case SUMA_complex:
         cc = *((complex *)val);
         mxv->cv = (complex *)mxv->v;         
         for (i=0; i<mxv->N_vals; ++i) 
            { mxv->cv[i].r = cc.r; mxv->cv[i].i = cc.i; } 
         break;
      default:
         SUMA_SL_Err("Bad type");
         SUMA_RETURN(0);
   }
   
   SUMA_RETURN(1); 
}
int SUMA_NewMxAllocVec(SUMA_MX_VEC *mxv) 
{
   static char FuncName[]={"SUMA_NewMxAllocVec"};
   int i, ip;
   
   SUMA_ENTRY;
   
   if (mxv->v || mxv->bv || mxv->sv || mxv->iv || mxv->fv || mxv->dv || mxv->cv || mxv->m) {
      SUMA_S_Err("Non null vector pointers");
      SUMA_RETURN(0);
   }
   
   switch (mxv->tp) {
      case SUMA_byte:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(byte));
         mxv->bv = (byte *)mxv->v; 
         break;
      case SUMA_short:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(short));
         mxv->sv = (short *)mxv->v;         
         break;
      case SUMA_int:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(int));
         mxv->iv = (int *)mxv->v;         
         break;
      case SUMA_float:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(float));
         mxv->fv = (float *)mxv->v;         
         break;
      case SUMA_double:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(double));
         mxv->dv = (double *)mxv->v;         
         break;
      case SUMA_complex:
         mxv->v = SUMA_calloc(mxv->N_vals, sizeof(complex));
         mxv->cv = (complex *)mxv->v; 
         if (mxv->v) {
            for (i=0; i<mxv->N_vals; ++i) { 
               mxv->cv[i].r = 0.0; mxv->cv[i].i = 0.0; 
            }
         }
         break;
      default:
         SUMA_SL_Err("Bad type");
         SUMA_RETURN(0);
   }
   
   if (!mxv->v) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_RETURN(0);
   }
   mxv->m = NULL; /* just to be sure */
   SUMA_RETURN(1);
}

SUMA_MX_VEC *SUMA_NewMxVec(SUMA_VARTYPE tp, int N_dims, int *dims, 
                           byte first_dim_first)
{
   static char FuncName[]={"SUMA_NewMxVec"};
   SUMA_MX_VEC *mxv=NULL;
   int n_vals=0, i = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* blank holder */
   mxv = SUMA_NewMxNullVec(tp, N_dims,  dims,  first_dim_first);
   if (LocalHead) SUMA_ShowMxVec(mxv, 1, NULL, "\nmxv in NewMx\n");
   /* allocator */
   if (!SUMA_NewMxAllocVec(mxv)) {
      SUMA_SL_Crit("Failed to allocate");
      SUMA_free(mxv); SUMA_RETURN(NULL);
   }
   
   SUMA_RETURN(mxv);
} 

SUMA_MX_VEC *SUMA_VecToMxVec(SUMA_VARTYPE tp, int N_dims, int *dims, 
                             byte first_dim_first, void *vec)
{
   static char FuncName[]={"SUMA_VecToMxVec"};
   SUMA_MX_VEC *mxv=NULL;
   int n_vals=0, i = 0;
   
   SUMA_ENTRY;
   
   /* blank holder */
   mxv = SUMA_NewMxNullVec(tp, N_dims,  dims,  first_dim_first);
   
   if (!vec) SUMA_RETURN(mxv);
   
   mxv->v = vec;
   switch (mxv->tp) {
      case SUMA_byte:
         mxv->bv = (byte *)mxv->v; 
         break;
      case SUMA_short:
         mxv->sv = (short *)mxv->v;         
         break;
      case SUMA_int:
         mxv->iv = (int *)mxv->v;         
         break;
      case SUMA_float:
         mxv->fv = (float *)mxv->v;         
         break;
      case SUMA_double:
         mxv->dv = (double *)mxv->v;         
         break;
      case SUMA_complex:
         mxv->cv = (complex *)mxv->v;         
         break;
      default:
         SUMA_SL_Err("Bad type");
         SUMA_free(mxv); SUMA_RETURN(NULL);
   }
      
   SUMA_RETURN(mxv);
} 


SUMA_MX_VEC *SUMA_FreeMxVec(SUMA_MX_VEC *mxv)
{
   static char FuncName[]={"SUMA_FreeMxVec"};
   int i;
   SUMA_ENTRY;
   
   if (mxv) {
      if (mxv->v) SUMA_free(mxv->v);
      if (mxv->m) {
         if (mxv->m->elts) {
            for (i=0; i<mxv->m->rows; ++i) 
               if (mxv->m->elts[i]) SUMA_free(mxv->m->elts[i]);
            SUMA_free(mxv->m->elts);
         }
         SUMA_free(mxv->m); 
      }
      mxv->m = NULL;
      SUMA_free(mxv);
   }
   
   SUMA_RETURN(NULL);
}

void SUMA_ShowMxVec (SUMA_MX_VEC *mxv, int detail, FILE *out, char *title)
{
   static char FuncName[]={"SUMA_ShowMxVec"};
   char *si = NULL;
   
   SUMA_ENTRY;
   
   if (!out) out = SUMA_STDERR;
   
   si = SUMA_MxVec_Info(mxv, detail, title);
   
   fprintf(out,"%s\n", si);
   
   if (si) SUMA_free(si); si = NULL;
   
   SUMA_RETURNe;
   
}

/*!
   \brief Function to return info on SUMA_DSET
   
   - You must free the returned string on your own
   \sa SUMA_ShowDset
*/
char *SUMA_MxVec_Info (SUMA_MX_VEC *mxv, int detail, char *title)
{
   static char FuncName[]={"SUMA_MxVec_Info"};
   int i, imx = 5, j;
   SUMA_COL_TYPE ctp;
   char *s=NULL, stmp[200];
   SUMA_STRING *SS=NULL;
   SUMA_Boolean LocalHead = NOPE;
    
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   if (mxv) {
      SUMA_LH("Have mxv");
      if (title) SS = SUMA_StringAppend_va(SS, "%s", title); 
      SS = SUMA_StringAppend_va(SS, "mxv: %p\n"
                                    "data type: %d (%s)\n"
                                    "fdf: %d\n"
                                    "N_dims: %d\n"
                                    "N_vals: %d\n"
                                    , mxv, mxv->tp, 
                                    SUMA_VarType2CTypeName(mxv->tp), mxv->fdf, 
                                    mxv->N_dims, mxv->N_vals);
      if (mxv->m) {
         SUMA_LH("Working m");
         SS = SUMA_StringAppend_va(SS, 
            "m is setup (rows: %d, cols: %d)\n", mxv->m->rows, mxv->m->cols);
         SUMA_LH("Working m");
         for (i=0; i < mxv->m->rows && i < imx; ++i) {
            for (j=0; j < mxv->m->cols && j < imx; ++j) {
               SUMA_LH("elts[][]\n");
               SS = SUMA_StringAppend_va(SS,"%g   ", mxv->m->elts[i][j]);
            }
            if (mxv->m->cols > imx) SS = SUMA_StringAppend(SS,"...\n");
            else SS = SUMA_StringAppend(SS,"\n");
         }
         if (mxv->m->rows > imx) 
            SS = SUMA_StringAppend(SS,"...  ...   ...   ...   ...\n");
         else SS = SUMA_StringAppend(SS,"\n");
         SUMA_LH("Done with m");
      } else {
         SUMA_LH("NULL m");
         SS = SUMA_StringAppend(SS, "m is NULL\n");
      }
      SUMA_LH("dims time");
      SS = SUMA_StringAppend_va(SS, "dims: ");
      for (i=0; i<mxv->N_dims; ++i) {
         SS = SUMA_StringAppend_va(SS, "%d ", mxv->dims[i]);
      }
      SS = SUMA_StringAppend_va(SS, "\n");
      
      if (mxv->v) {
         if (detail < 0) {
            imx = mxv->N_vals;
         } else {
            imx = 5*detail;
         }
         s = SUMA_ShowMeSome( mxv->v, 
                              mxv->tp,
                              mxv->N_vals, imx, NULL);
         SS = SUMA_StringAppend_va(SS, "         %s\n", s); 
         SUMA_free(s); s = NULL;
      } else SS = SUMA_StringAppend_va(SS, "         NULL\n");
   } else {
      SS = SUMA_StringAppend(SS, "NULL mxv.");
   }
   
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}

/****************** Command line and Env parsing functions. ************** */
/****************** Help strings. Filename parsing.         ************** */

static int no_suma_rc_found;

int NoSumaRcFound (void) { return (no_suma_rc_found);}

/*!
   Return number of Icosahedron
   depth (int) depth of subdivision (value of -ld or -rd in  CreateIcosahedron)
   bin (byte) 1 --> equiv to -rd in CreateIcosahedron
              0 -->          -ld in CreateIcosahedron
   what (char) 'v' or 'n' :return number of nodes
               't': return number of triangles
               'e': return number of edges
*/
int SUMA_IcoNums(int depth, byte bin, char what) {
   int dd=-1;
   if (depth < 0) return (dd);
   if (bin) { /* binary subdivisions */
      switch (what){
         case 'v':
         case 'n':
            dd = (int)(pow(2, (2*depth)))*10 + 2;
            break;
         case 't':
            dd = (int)(pow(2, (2*depth)))*20;
            break;
         case 'e':
            dd = (int)(pow(2, (2*depth)))*30;
            break;
      }
   } else { /* linear subdivisions */
      switch (what){
         case 'v':
         case 'n':
            dd = 2 + (10 * depth * depth);
            break;
         case 't':
            dd = 20 * depth * depth;
            break;
         case 'e':
            dd = 30 * depth * depth;
            break;
      }
   }
   return(dd);
}

/*!
   \brief load the environment variables first from 
   $HOME/.sumarc and $HOME/.afnirc
   if HOME is not defined then try .afnirc and .sumarc
   Shameless wrapper for AFNI_process_environ

   No fancies here, this function is called before CommonFields
*/
void SUMA_process_environ(void)
{
   static char FuncName[]={"SUMA_process_environ"};
   struct stat stbuf;
   char *sumarc = NULL, *homeenv=NULL;
   SUMA_Boolean LocalHead = NOPE;

   no_suma_rc_found = 0;
   
   if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: Entering SUMA_process_environ\n", FuncName);
   
   sumarc = (char *)malloc( sizeof(char)*
                           (SUMA_MAX_NAME_LENGTH+SUMA_MAX_DIR_LENGTH+1));

   /* load the environment variables from .sumarc and .afnirc*/
   homeenv = getenv("HOME");

   if (!homeenv) sprintf(sumarc, ".sumarc");
   else sprintf(sumarc,"%s/.sumarc", homeenv);
   if (stat(sumarc, &stbuf) != -1) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: Loading %s ...\n", FuncName, sumarc);
      AFNI_process_environ(sumarc); 
   } else {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: No sumarc file found.\n", FuncName);
         no_suma_rc_found = 1;
   }

   if (!homeenv) sprintf(sumarc, ".afnirc");
   else sprintf(sumarc,"%s/.afnirc", homeenv);
   if (stat(sumarc, &stbuf) != -1) {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: Loading %s ...\n", FuncName, sumarc);
      AFNI_process_environ(sumarc); 
   } else {
      if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: No afnirc file found.\n", FuncName);
   }   

   if (sumarc) free(sumarc); sumarc = NULL; /* allocated before CommonFields */
   
   AFNI_mark_environ_done(); /* flag environment rc files as read */
   
   if (LocalHead) 
         fprintf (SUMA_STDERR,"%s: Exiting SUMA_process_environ\n", FuncName);
         
   return;
}

char *SUMA_help_basics()
{
   SUMA_STRING *SS = NULL;
   char *s=NULL;
   static char FuncName[]={"SUMA_help_basics"};
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   SS = SUMA_StringAppend_va(SS,
      "   [-novolreg]: Ignore any Rotate, Volreg, Tagalign, \n"
      "                or WarpDrive transformations present in \n"
      "                the Surface Volume.\n"
      "   [-noxform]: Same as -novolreg\n"
      "   [-setenv \"'ENVname=ENVvalue'\"]: Set environment variable ENVname\n"
      "                to be ENVvalue. Quotes are necessary.\n"
      "             Example: suma -setenv \"'SUMA_BackgroundColor = 1 0 1'\"\n"
      "                See also options -update_env, -environment, etc\n"
      "                in the output of 'suma -help'\n" 
      "  Common Debugging Options:\n"
      "   [-trace]: Turns on In/Out debug and Memory tracing.\n"
      "             For speeding up the tracing log, I recommend \n"
      "             you redirect stdout to a file when using this option.\n"
      "             For example, if you were running suma you would use:\n"
      "             suma -spec lh.spec -sv ... > TraceFile\n"
      "             This option replaces the old -iodbg and -memdbg.\n"
      "   [-TRACE]: Turns on extreme tracing.\n"
      "   [-nomall]: Turn off memory tracing.\n"
      "   [-yesmall]: Turn on memory tracing (default).\n"
      "  NOTE: For programs that output results to stdout\n"
      "    (that is to your shell/screen), the debugging info\n"
      "    might get mixed up with your results.\n" 
      "\n"
      "\n"
      "Global Options (available to all AFNI/SUMA programs)\n"
      "%s\n",
      SUMA_Offset_SLines(get_help_help(),2),     get_gopt_help() );
   SUMA_SS2S(SS,s);               
   SUMA_RETURN(s);
}


char *SUMA_help_talk()
{
   SUMA_STRING *SS = NULL;
   char *s=NULL;
   static char FuncName[]={"SUMA_help_talk"};
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   SS = SUMA_StringAppend_va(SS,
"  SUMA communication options:\n"
"      -talk_suma: Send progress with each iteration to SUMA.\n"
"      -refresh_rate rps: Maximum number of updates to SUMA per second.\n"
"                         The default is the maximum speed.\n"
"      -send_kth kth: Send the kth element to SUMA (default is 1).\n"
"                     This allows you to cut down on the number of elements\n"
"                     being sent to SUMA.\n" 
"      -sh <SumaHost>: Name (or IP address) of the computer running SUMA.\n"
"                      This parameter is optional, the default is 127.0.0.1 \n"
"      -ni_text: Use NI_TEXT_MODE for data transmission.\n"
"      -ni_binary: Use NI_BINARY_MODE for data transmission.\n"
"                  (default is ni_binary).\n"
"      -feed_afni: Send updates to AFNI via SUMA's talk.\n"
"%s"
"\n", get_np_help());
   SUMA_SS2S(SS,s);               
   SUMA_RETURN(s);
}

char *SUMA_help_dset()
{
   SUMA_STRING *SS = NULL;
   char *s=NULL;
   static char FuncName[]={"SUMA_help_dset"};
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   SS = SUMA_StringAppend(SS,
      "  SUMA dataset input options:\n"
      "      -input DSET: Read DSET1 as input.\n"
      "                   In programs accepting multiple input datasets\n"
      "                   you can use -input DSET1 -input DSET2 or \n"
      "                   input DSET1 DSET2 ...\n"
      "       NOTE: Selecting subsets of a dataset:\n"
      "             Much like in AFNI, you can select subsets of a dataset\n"
      "             by adding qualifiers to DSET.\n"
      "           Append #SEL# to select certain nodes.\n"
      "           Append [SEL] to select certain columns.\n"
      "           Append {SEL} to select certain rows.\n"
      "           The format of SEL is the same as in AFNI, see section:\n"
      "           'INPUT DATASET NAMES' in 3dcalc -help for details.\n"
      "           Append [i] to get the node index column from\n"
      "                      a niml formatted dataset.\n"
      "           *  SUMA does not preserve the selection order \n"
      "              for any of the selectors.\n"
      "              For example:\n"
      "              dset[44,10..20] is the same as dset[10..20,44]\n"
      "              Also, duplicate values are not supported.\n"
      "              so dset[13, 13] is the same as dset[13].\n"
      "              I am not proud of these limitations, someday I'll get\n"
      "              around to fixing them.\n" 
      "\n");
   SUMA_SS2S(SS,s);               
   SUMA_RETURN(s);
}

char *SUMA_help_cmap()
{
   SUMA_STRING *SS = NULL;
   char *s=NULL;
   static char FuncName[]={"SUMA_help_mask"};
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   SS = SUMA_StringAppend(SS,
" Selecting Colormaps: \n"
"    -cmap MapName:\n"
"       choose one of the standard colormaps available with SUMA:\n"
"       RGYBR20, BGYR19, BW20, GRAY20, MATLAB_DEF_BYR64, \n"
"       ROI64, ROI128. See Suma's colormap chooser for a list of names.\n"
"    -cmapdb Palfile: read color maps from AFNI .pal file\n"
"       In addition to the default paned AFNI colormaps, you\n"
"       can load colormaps from a .pal file.\n"
"       To access maps in the Palfile you must use the -cmap option\n"
"       with the label formed by the name of the palette, its sign\n"
"       and the number of panes. For example, to following palette:\n"
"       ***PALETTES deco [13]\n"
"       should be accessed with -cmap deco_n13\n"
"       ***PALETTES deco [13+]\n"
"       should be accessed with -cmap deco_p13\n"  
"    -cmapfile Mapfile: read color map from Mapfile.\n"
"       Mapfile:1D formatted ascii file containing colormap.\n"
"               each row defines a color in one of two ways:\n"
"               R  G  B        or\n"
"               R  G  B  f     \n"
"       where R, G, B specify the red, green and blue values, \n"
"       between 0 and 1 and f specifies the fraction of the range\n"
"       reached at this color. THINK values of right of AFNI colorbar.\n"
"       The use of fractions (it is optional) would allow you to create\n"
"       non-linear color maps where colors cover differing fractions of \n"
"       the data range.\n"
"       Sample colormap with positive range only (a la AFNI):\n"
"               0  0  1  1.0\n"
"               0  1  0  0.8\n"
"               1  0  0  0.6\n"
"               1  1  0  0.4\n"
"               0  1  1  0.2\n"
"       Note the order in which the colors and fractions are specified.\n"
"       The bottom color of the +ve colormap should be at the bottom of the\n"
"       file and have the lowest +ve fraction. The fractions here define a\n"
"       a linear map so they are not necessary but they illustrate the format\n"
"       of the colormaps.\n"
"       Comparable colormap with negative range included:\n"
"               0  0  1   1.0\n"
"               0  1  0   0.6\n"
"               1  0  0   0.2\n"
"               1  1  0  -0.2\n"
"               0  1  1  -0.6\n"
"       The bottom color of the -ve colormap should have the \n"
"       lowest -ve fraction. \n"
"       You can use -1 -1 -1 for a color to indicate a no color\n"
"       (like the 'none' color in AFNI). Values mapped to this\n"
"       'no color' will be masked as with the -msk option.\n"
"       If your 1D color file has more than three or 4 columns,\n"
"       you can use the [] convention adopted by AFNI programs\n"
"       to select the columns you need.\n"   
      );

   SUMA_SS2S(SS,s);               
   SUMA_RETURN(s);
}

char *SUMA_help_mask()
{
   SUMA_STRING *SS = NULL;
   char *s=NULL;
   static char FuncName[]={"SUMA_help_mask"};
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   SS = SUMA_StringAppend(SS,
" SUMA mask options:\n"
"      -n_mask INDEXMASK: Apply operations to nodes listed in\n"
"                            INDEXMASK  only. INDEXMASK is a 1D file.\n"
"      -b_mask BINARYMASK: Similar to -n_mask, except that the BINARYMASK\n"
"                          1D file contains 1 for nodes to filter and\n"
"                          0 for nodes to be ignored.\n"
"                          The number of rows in filter_binary_mask must be\n"
"                          equal to the number of nodes forming the\n"
"                          surface.\n"
"      -c_mask EXPR: Masking based on the result of EXPR. \n"
"                    Use like afni's -cmask options. \n"
"                    See explanation in 3dmaskdump -help \n"
"                    and examples in output of 3dVol2Surf -help\n"
"      NOTE: Unless stated otherwise, if n_mask, b_mask and c_mask \n"
"            are used simultaneously, the resultant mask is the intersection\n"
"            (AND operation) of all masks.\n"
"\n");
   SUMA_SS2S(SS,s);     
   SUMA_RETURN(s);
}

/*!
   \brief parse command line arguments for input/output debugging and
   memory debugging. Use no fancies in this function!
   
   This function is to be called after SUMAg_CF has been created,
   if #ifdef SUMA_COMPILED 
   
   Default for iotrace = 0
               memtrace = 1 
   Those defaults are common to all apps 
   
*/
static int Domemtrace=0, Doiotrace=0, IgnoreXforms=0;
int get_Domemtrace(void) {
   return (Domemtrace);
}
void set_Domemtrace(int s) {
   Domemtrace = s;
   return;
}
int get_Doiotrace(void) {
   return (Doiotrace);
}
void set_Doiotrace(int s) {
   Doiotrace = s;
   return;
}
int get_IgnoreXforms(void) {
   return (IgnoreXforms);
}
void set_IgnoreXforms(int s) {
   IgnoreXforms = s;
   return;
}


/*!
   No fancy macros and allocation here please
*/
int SUMA_ParseInput_basics_eng (char *argv[], int argc) 
{

   static char FuncName[]={"SUMA_ParseInput_basics_eng"};
   int brk = 0;
   int kar;

   if (!argv) return (0);
   set_Domemtrace(1);
   set_Doiotrace(0);

   /* if (argc < 2) return (0); why insist on two parameters? */

   kar = 1;
   brk = 0;
   while (kar < argc) { /* loop accross tracing and debugging 
                           command line options */
		if ((strcmp(argv[kar], "-memdbg") == 0) ||
          (strcmp(argv[kar], "-yesmall") == 0) ) {
			fprintf(SUMA_STDOUT,"Warning %s:  running in memory trace mode.\n", 
                  FuncName);
			set_Domemtrace(1);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-nomall") == 0)) {
			fprintf(SUMA_STDOUT,"Warning %s:  turning off memory trace mode.\n", 
                  FuncName);
			set_Domemtrace(0);
			brk = 1;
		}

      if (!brk && ( (strcmp(argv[kar], "-trace") == 0) ||
                   (strcmp(argv[kar], "-iodbg") == 0)) ){
			fprintf( SUMA_STDERR,
                  "Warning %s: SUMA running in I/O trace mode.\n", FuncName);
			set_Doiotrace(1);
         brk = 1;
		}
      
      if (!brk && (strcmp(argv[kar], "-TRACE") == 0)) {
			fprintf( SUMA_STDERR,
                  "Warning %s: SUMA running in detailed I/O trace mode.\n", 
                  FuncName);
			set_Doiotrace(2);
         brk = 1;
		}
      
      if (!brk && 
            (  strcmp(argv[kar], "-novolreg") == 0 || 
               strcmp(argv[kar], "-noxform") == 0)) {
			set_IgnoreXforms(1);
         brk = 1;
		}
      
      brk = 0;
      kar ++;
   }
       
   return (1);
}

/*!
   No fancy macros and allocation here please
*/
void SUMA_ParseInput_basics_ns(char *argv[], int argc) /* for non-suma programs */
{
   static char FuncName[]={"SUMA_ParseInput_basics_ns"};
   
   if (!argv) return;
   /*if (argc < 2) return; why insist on two parameters? */
   
   if (!SUMA_ParseInput_basics_eng (argv, argc)) return;
   
   if (get_Doiotrace()) { SUMA_INOUT_NOTIFY_ON; } 
   if (get_Domemtrace()) { SUMA_MEMTRACE_ON; } 

   /* some more special ones */
   #ifdef USE_TRACING
      if (get_Doiotrace() == 2) { DBG_trace = 2; } 
   #endif
   
   return;

}

/*!
   A function version of macro SUMA_TO_LOWER
   what gets returned is actually the same pointer
   s but s now points to a lowercase string.
*/
char * SUMA_to_lower(char *s) { 
   int i, d; 
   if (s) { 
      d = 'a' - 'A';  
      for (i=0; i < strlen(s); ++i) { 
         if (s[i] >= 'A' && s[i] <= 'Z') s[i] = s[i] + d;  
      }   
   } 
   return(s); 
}  

/*!**
   
Purpose : 
   
   splits a path/filename into its path and filename components
   
Usage : 
		Ans = SUMA_StripPath (Name)
   
   
Input parameters : 
\param   Name (char *) something like /hello/something
   
Returns : 
\return   ans (SUMA_FileName) .Path (char *) and .FileName (char *)
   
Support : 
\sa  SUMA_define.h 

NOTE: SUMA_ParseFname() is better than this function	
   
To Compile as stand alone:
gcc -DSUMA_StripPath_STAND_ALONE -Wall -o $1 $1.c -SUMA_lib.a -I/usr/X11R6/include -I./
***/
SUMA_FileName SUMA_StripPath (char *FileName)
{/*SUMA_StripPath*/
   static char FuncName[] = {"SUMA_StripPath"},  PathDelimiter[]={"/"}; 
   int i, j, NotFound=1, N_FileName;
	SUMA_FileName NewName;
	
	N_FileName = strlen(FileName);
	if (N_FileName ){
		i = N_FileName -1;
		while (i > -1 && NotFound) {
			if (FileName[i] == PathDelimiter[0]) NotFound = 0;
			--i;
		}
		if (!NotFound && i > -1) {
			NewName.Path = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
			NewName.FileName = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
			if (NewName.Path == NULL || NewName.FileName == NULL) {
				SUMA_SL_Err("Failed to allocate");
            return (NewName);
			}
			for (j=0; j<=i+1; ++j) {
				NewName.Path[j] = FileName[j];
			}
         NewName.Path[j] = '\0';
         
			/*fprintf(stdout,"jbegin=%d/%d\n", i+2, N_FileName);*/
			for (j=i+2; j < N_FileName; ++j) NewName.FileName[j-i-2] = FileName[j];
         NewName.FileName[j-i-2] = '\0';
         
			/* fprintf(stdout,"All Path (%d chars)/%d: %s\n", 
                     (i+2),  strlen(NewName.Path), NewName.Path);
			fprintf(stdout,"All FileName (%d chars)/%d: %s\n", 
                     (N_FileName-i-2), strlen(NewName.FileName), 
                     NewName.FileName); */
		}
		else {
			NewName.Path = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
			NewName.FileName = (char *)SUMA_malloc(sizeof(char)*(N_FileName+1));
			if (NewName.Path == NULL || NewName.FileName == NULL) {
				SUMA_SL_Err("Failed to allocate");
            return (NewName);
			}
			sprintf(NewName.Path,"./");		
			sprintf(NewName.FileName,"%s", FileName);
		}
	}
	else {
		NewName.Path = NULL;
		NewName.FileName = NULL;
	}
	return (NewName);
}/*SUMA_StripPath*/

SUMA_Boolean SUMA_ShowParsedFname(SUMA_PARSED_NAME *pn, FILE *out)
{
   static char FuncName[]={"SUMA_ShowParsedFname"};
   char *s=NULL;
   SUMA_STRING *SS=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!out) out = SUMA_STDOUT;
   
   SS = SUMA_StringAppend(NULL, NULL);
   if (!pn) {
      SS = SUMA_StringAppend_va(SS, "NULL parsed name");
   } else {
      SS = SUMA_StringAppend_va(SS, "AbsPath       :%s\n", pn->AbsPath);
      SS = SUMA_StringAppend_va(SS, "RelDir        :%s\n", pn->RelDir);
      SS = SUMA_StringAppend_va(SS, "RelPath       :%s\n", pn->RelPath);
      SS = SUMA_StringAppend_va(SS, "Path          :%s\n", pn->Path);
      SS = SUMA_StringAppend_va(SS, "FileName      :%s\n", pn->FileName);
      SS = SUMA_StringAppend_va(SS, "Prefix        :%s\n", pn->Prefix);
      SS = SUMA_StringAppend_va(SS, "View          :%s\n", pn->View);
      SS = SUMA_StringAppend_va(SS, "Ext           :%s\n", pn->Ext);
      SS = SUMA_StringAppend_va(SS, "TypeExt       :%s\n", pn->TypeExt);
      SS = SUMA_StringAppend_va(SS, "StorageMode   :%d\n", pn->StorageMode);
      SS = SUMA_StringAppend_va(SS, "StorageModeNm.:%s\n", pn->StorageModeName);
      SS = SUMA_StringAppend_va(SS, "FileName_NoExt:%s\n", pn->FileName_NoExt);
      SS = SUMA_StringAppend_va(SS, "FNameNoAfniExt:%s\n", \
                                 without_afni_filename_extension(pn->FileName));
      SS = SUMA_StringAppend_va(SS, "FNameLabel    :%s\n", \
                                 without_afni_filename_extension(pn->Prefix));
      SS = SUMA_StringAppend_va(SS, "Col. Selector :%s\n", pn->ColSelect);
      SS = SUMA_StringAppend_va(SS, "Node Selector :%s\n", pn->NodeSelect);
      SS = SUMA_StringAppend_va(SS, "Row Selector  :%s\n", pn->RowSelect);
      SS = SUMA_StringAppend_va(SS, "Range Selector:%s\n", pn->RangeSelect);
      SS = SUMA_StringAppend_va(SS, "Only index col:%d\n", pn->only_index);
      SS = SUMA_StringAppend_va(SS, "FullName      :%s\n", pn->FullName);
      SS = SUMA_StringAppend_va(SS, "FullName_NoSel:%s\n", pn->FullName_NoSel);
      SS = SUMA_StringAppend_va(SS, "RelName       :%s%s\n", 
                                                    pn->RelPath,pn->FileName);
      SS = SUMA_StringAppend_va(SS, "HeadName      :%s\n", pn->HeadName);
      SS = SUMA_StringAppend_va(SS, "BrikName      :%s\n", pn->BrikName);
      SS = SUMA_StringAppend_va(SS, "OnDisk        :%d\n", pn->OnDisk);
      SS = SUMA_StringAppend_va(SS, "ExistsAs      :%s\n", 
                                                pn->ExistsAs?pn->ExistsAs:"");
      SS = SUMA_StringAppend_va(SS, "Size          :%d\n", pn->Size);
      SS = SUMA_StringAppend_va(SS, "NameAsParsed  :%s\n", pn->NameAsParsed);
      SS = SUMA_StringAppend_va(SS, "cwdAsParsed   :%s\n", pn->cwdAsParsed);
      
   }

   SUMA_SS2S(SS,s);
   
   fprintf(out, "%s", s); SUMA_free(s); s= NULL;
   fflush(out);
   
   SUMA_RETURN(YUP);
}

char *SUMA_getcwd(void) 
{
   static char FuncName[]={"SUMA_getcwd"};
   char *cwd = NULL;
   
   SUMA_ENTRY;
   
   cwd = (char *)SUMA_malloc(sizeof(char)*(SUMA_MAX_DIR_LENGTH+1));
   getcwd(cwd, SUMA_MAX_DIR_LENGTH);
   
   SUMA_RETURN(cwd);
}

SUMA_PARSED_NAME * SUMA_ParseFname (char *FileName, char *ucwd)
{
   return(SUMA_ParseFname_eng(FileName, ucwd, 1));  
}

/*!
   \brief ans = SUMA_ParseFname (FileName, cwd, diskcheck);
   parses a file name into its elements
   \param FileName (char *) obvious ...
   \param ucwd (char *) if not null, this is the user supplied current work. dir.
   \param diskcheck (int) if not 0 check for file's size and existence on disk
   \return ans (SUMA_PARSED_NAME *) pointer to structure with following fields:
      .FileName (char *) containing filename without path and without selectors (see below). 
                        if empty .FileName[0] = '\0'
      .Path (char *) containing path including last slash.
                     If no path exists, Path is "./" 
      .AbsPath (char *) containing absolute path, which is 
                        the same as .Path if .Path starts with '/'
                        or cwd/.Path if .Path does not start with '/'
                        AND cwd is specified. 
                        If cwd is null then it is determined inside the function.
      .Ext (char *) containing extension including the dot.
                    If no extension exists, Ext[0] = '\0'
      .FileName_NoExt (char *) filename without extension.
      .NodeSelect (char *) Node selector part; between ## ; The () were already taken
      .ColSelect (char *) Column selector part; between []
      .RowSelect (char *) Row selector part; between {}
      .RangeSelect (char *) Range selector part; between <> 
      
      \sa SUMA_Free_Parsed_Name, SUMA_ShowParsedFname
*/
SUMA_PARSED_NAME * SUMA_ParseFname_eng (char *FileName, char *ucwd, 
                                        int diskcheck)
{/*SUMA_ParseFname*/
   static char FuncName[]={"SUMA_ParseFname_eng"};
   char PathDelimiter='/';
   char *cwd=NULL; 
   int   i, j, iExt , iFile, iPath, iColSel, iRowSel, iNodeSel, 
         iRangeSel, N_FileName, iFirstSel, nc,
         iAtSel, only_index;
	SUMA_PARSED_NAME *NewName = NULL;
   SUMA_Boolean   FoundPath = NOPE, 
                  FoundExt, FoundFile, FoundRowSel , 
                  FoundNodeSel, FoundColSel, FoundRangeSel;
	SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   
   if (!FileName) {
      SUMA_S_Err("Null input");
      SUMA_RETURN(NULL);
   }
   
   /* work cwd */
   if (ucwd) {
      if (ucwd[0] != '/') {
         SUMA_S_Err("Current working directory must start with '/'");
         SUMA_RETURN(NULL);
      }
      cwd = SUMA_copy_string(ucwd);
   } else {
      cwd = SUMA_getcwd();
      if (cwd[0] != '/') {
         SUMA_S_Err("STRANGE! Current working directory must start with '/'");
         SUMA_free(cwd); cwd = NULL;
         SUMA_RETURN(NULL);
      }
   }
   nc = strlen(cwd);
   if (cwd[nc-1] == PathDelimiter) { cwd[nc-1] = '\0'; --nc; } 
   
   SUMA_LH("Checking chars");
	N_FileName = strlen(FileName);
   iExt = N_FileName;
   iPath = -1;
   iFile = 0;
   iColSel = -1;
   iRowSel = -1;
   iNodeSel = -1;
   iRangeSel = -1;
   iAtSel = -1;
   only_index = 0;
   iFirstSel = N_FileName;
   FoundPath = NOPE;
   FoundExt = NOPE;
   FoundRowSel = NOPE;
   FoundNodeSel = NOPE;
   FoundColSel = NOPE;
   FoundRangeSel = NOPE;
   
   NewName = (SUMA_PARSED_NAME *) SUMA_calloc(1,sizeof(SUMA_PARSED_NAME));
	NewName->NameAsParsed = SUMA_copy_string(FileName);
   NewName->cwdAsParsed = SUMA_copy_string(ucwd);
   
   if (N_FileName ){
      i = N_FileName -1;
		while (i > -1 && !FoundPath) {
			if (FileName[i] == '.' && !FoundExt && 
             !(FoundColSel && iColSel < 0) && /* Not whilst column selection */
             !(FoundRowSel && iRowSel < 0) && /* Not whilst  row selection */
             !(FoundNodeSel && iNodeSel < 0) && /* Not whilst node selection */
             !(FoundRangeSel && iRangeSel < 0) /* Not whilst  range selection */          ) {
            iExt = i;
            FoundExt = YUP;
         } else if (FileName[i] == PathDelimiter) {
            FoundPath = YUP;
            iPath = i;
            iFile = i+1;
         } else if (FileName[i] == ']') {
            FoundColSel = YUP;
         } else if (FileName[i] == '[' && FoundColSel) {
            iColSel = i; if (iColSel < iFirstSel) iFirstSel = iColSel; 
         } else if (FileName[i] == '}') {
            FoundRowSel = YUP;
         } else if (FileName[i] == '{' && FoundRowSel) {
            iRowSel = i; if (iRowSel < iFirstSel) iFirstSel = iRowSel;
         } else if (FileName[i] == '#' && !FoundNodeSel) {
            FoundNodeSel = YUP;
         } else if (FileName[i] == '#' && FoundNodeSel) {
            iNodeSel = i; if (iNodeSel < iFirstSel) iFirstSel = iNodeSel;
         } else if (FileName[i] == '>') {
            FoundRangeSel = YUP;
         } else if (FileName[i] == '<' && FoundRangeSel) {
            iRangeSel = i; if (iRangeSel < iFirstSel) iFirstSel = iRangeSel;
         } else if ( 0 &&        /* Don't need that, using [i] instead */
                     FileName[i] == '@' &&            /* Not whilst reading */
                     !FoundExt &&                     /* pre extension*/
                     !(FoundColSel && iColSel < 0) && /* column selection   */
                     !(FoundRowSel && iRowSel < 0) && /*  row selection */
                     !(FoundNodeSel && iNodeSel < 0) && /* node selection */
                     !(FoundRangeSel && iRangeSel < 0) )  {/* range selection */
            iAtSel = i; if (iAtSel < iFirstSel) iFirstSel = iAtSel;
            only_index = 1;
			}
         --i;
		}
      
      if (FoundColSel && iColSel < 0) {
         FoundColSel = NOPE; /* need both of [ ] */
      }
      if (FoundRowSel && iRowSel < 0) {
         FoundRowSel = NOPE; /* need both of { } */
      }
      if (FoundNodeSel && iNodeSel < 0) {
         FoundNodeSel = NOPE; /* need both of # # */
      }
      if (FoundRangeSel && iRangeSel < 0) {
         FoundRangeSel = NOPE; /* need both of < > */
      }
      
      if (iFile == iExt) {
         /* .file, not an extension */
         FoundExt = NOPE;
      }
      
      if (iFile ==  N_FileName) FoundFile = NOPE;
      else FoundFile = YUP;
      
      SUMA_LH("Storing stuff");
      
      if (FoundPath) {
         NewName->Path = (char *)SUMA_malloc(sizeof(char)*(iPath+2));
         for (i=0; i<= iPath; ++i) NewName->Path[i] = FileName[i];
         NewName->Path[i] = '\0';
      }else {
         NewName->Path = (char *)SUMA_malloc(sizeof(char)*(3));
         sprintf(NewName->Path, "./");
      }
      if (NewName->Path[0] == '/') {
         NewName->AbsPath = SUMA_copy_string(NewName->Path);
      } else {
         char *ptmp = NewName->Path;
         if (ptmp[0] == '.') {
            /* just searching for ./ here?                  15 Aug 2011 [rickr]
             * ... problem noted by Ryan from Princeton
             * if (strstr(NewName->Path,"./") && ptmp[1] == '/') ptmp = ptmp+2;
             * else ptmp = ptmp+1;                                           */

            if ( ptmp[1] == '/' ) ptmp = ptmp+2;
            else if ( ptmp[1] == '\0' ) ptmp = ptmp+1;
         } 
         NewName->AbsPath = SUMA_append_replace_string(cwd, ptmp, "/", 0);
         ptmp = NULL;
      }
      /* store the current directory, and put back the slash */
      NewName->RelDir = SUMA_append_string(cwd, "/");
      /* get the relative path (assumes both end with / )*/
      {
         int nback=0, imatch=0;
         i=0; /* go as far as the directories match */
         while (i < strlen(NewName->RelDir) &&
                i < strlen(NewName->AbsPath) &&
                NewName->RelDir[i] == NewName->AbsPath[i] ) ++i;
         /* backup i until you hit the last '/' */
         while (i>=0 && NewName->RelDir[i] != '/') --i;
         if (NewName->RelDir[i] == '/') ++i; /* and back up one */
         
         /* how many extra directories do we have left in NewName->RelDir ?*/
         imatch = i;
         while (i < strlen(NewName->RelDir)) {
            if (NewName->RelDir[i] == '/') ++nback;
            ++i;
         }
         NewName->RelPath = SUMA_calloc(sizeof(char), 
                                  strlen(NewName->AbsPath)-imatch+nback*3+10);
         NewName->RelPath[0]= '\0';
         for (i=0; i<nback; ++i) {
            strcat(NewName->RelPath, "../");
         }
         strcat(NewName->RelPath,NewName->AbsPath+imatch);
         if (!strlen(NewName->RelPath)) strcat(NewName->RelPath, "./");
      } 
      if (FoundFile) {
         NewName->FileName = 
            (char *)SUMA_malloc(sizeof(char)*(N_FileName - iFile + 2));
         for (i=iFile; i< iFirstSel; ++i) 
            NewName->FileName[i-iFile] = FileName[i];
         NewName->FileName[i-iFile] = '\0';
      }else {
         NewName->FileName = (char *)SUMA_malloc(sizeof(char));
         NewName->FileName[0] = '\0';
      }      
		
      if (FoundExt) {
		   NewName->FileName_NoExt = 
            (char *)SUMA_malloc(sizeof(char)*(N_FileName - iFile +2));
         NewName->Ext = 
            (char *)SUMA_malloc(sizeof(char)*(N_FileName - iExt+2));
         for (i=iFile; i< iExt; ++i) 
            NewName->FileName_NoExt[i-iFile] = FileName[i];
         NewName->FileName_NoExt[i-iFile] = '\0';
         for (i=iExt; i < iFirstSel; ++i) 
            NewName->Ext[i-iExt] = FileName[i];
         NewName->Ext[i-iExt] = '\0';
      } else {
         NewName->FileName_NoExt = SUMA_copy_string(NewName->FileName);
         NewName->Ext = (char *)SUMA_malloc(sizeof(char));
         NewName->Ext[0] = '\0';
      }
      
      if (FoundNodeSel) {
         NewName->NodeSelect = (char *)SUMA_malloc(sizeof(char)*
                                                   (N_FileName - iNodeSel + 2));
         for (i=iNodeSel; i< N_FileName; ++i) { 
            NewName->NodeSelect[i-iNodeSel] = FileName[i];
            if (FileName[i] == '#' && i >iNodeSel) { ++i; break; }
         }
         NewName->NodeSelect[i-iNodeSel] = '\0';
      }else {
         NewName->NodeSelect = (char *)SUMA_malloc(sizeof(char));
         NewName->NodeSelect[0] = '\0';
      }      
		
      if (FoundRowSel) {
         NewName->RowSelect = (char *)SUMA_malloc( sizeof(char)*
                                                   (N_FileName - iRowSel + 2));
         for (i=iRowSel; i< N_FileName; ++i) {
            NewName->RowSelect[i-iRowSel] = FileName[i];
            if (FileName[i] == '}') { ++i; break; }
         }
         NewName->RowSelect[i-iRowSel] = '\0';
      }else {
         NewName->RowSelect = (char *)SUMA_malloc(sizeof(char));
         NewName->RowSelect[0] = '\0';
      }
      
      if (FoundColSel) {
         NewName->ColSelect = 
            (char *)SUMA_malloc(sizeof(char)*(N_FileName - iColSel + 2));
         for (i=iColSel; i< N_FileName; ++i) {
            NewName->ColSelect[i-iColSel] = FileName[i];
            if (FileName[i] == ']') { ++i; break; }
         }
         NewName->ColSelect[i-iColSel] = '\0';
         if (NewName->ColSelect[1] == 'i') {
            only_index = 1;
            NewName->ColSelect[0] = '\0';
         }
      }else {
         NewName->ColSelect = (char *)SUMA_malloc(sizeof(char));
         NewName->ColSelect[0] = '\0';
      }
      
      if (FoundRangeSel) {
         NewName->RangeSelect = 
            (char *)SUMA_malloc(  sizeof(char)* (N_FileName - iRangeSel + 2));
         for (i=iRangeSel; i< N_FileName; ++i) {
            NewName->RangeSelect[i-iRangeSel] = FileName[i];
            if (FileName[i] == '>') { ++i; break; }
         }
         NewName->RangeSelect[i-iRangeSel] = '\0';
      }else {
         NewName->RangeSelect = (char *)SUMA_malloc(sizeof(char));
         NewName->RangeSelect[0] = '\0';
      }
      
      NewName->only_index = only_index;
      
      NewName->FullName_NoSel=NULL;
      NewName->FullName_NoSel=
         SUMA_append_replace_string(NewName->FullName_NoSel, 
                                    NewName->AbsPath, "", 1);
      NewName->FullName_NoSel=
         SUMA_append_replace_string(NewName->FullName_NoSel, 
                                    NewName->FileName, "", 1);
      
      NewName->StorageMode = storage_mode_from_prefix(NewName->FullName_NoSel);
      NewName->StorageModeName = storage_mode_name(NewName->StorageMode);
      
      NewName->FullName=NULL;
      NewName->FullName=
         SUMA_append_replace_string(NewName->FullName, 
                                    NewName->AbsPath, "", 1);
      NewName->FullName=
         SUMA_append_replace_string(NewName->FullName, 
                                    NewName->FileName, "", 1);
      NewName->FullName=
         SUMA_append_replace_string(NewName->FullName,  
                                    NewName->NodeSelect, "", 1);
      NewName->FullName=
         SUMA_append_replace_string(NewName->FullName,  
                                    NewName->RowSelect, "", 1);
      NewName->FullName=
         SUMA_append_replace_string(NewName->FullName, 
                                    NewName->ColSelect, "", 1);
      
      if (!(NewName->TypeExt = SUMA_copy_string(
                     find_filename_extension(NewName->FileName)))) {
         NewName->TypeExt = SUMA_copy_string("");              
      }
      
      if (NewName->StorageMode == STORAGE_BY_BRICK) {
         int dotted = 0;
         NewName->Prefix = SUMA_copy_string(NewName->FileName);
         NewName->Prefix[strlen(NewName->FileName)-
                         strlen(NewName->TypeExt)]='\0';
         if (NewName->Prefix[strlen(NewName->Prefix)-1] == '.') {
            dotted = 1;
         }
         if (dotted) {
            if (STRING_HAS_SUFFIX(NewName->Prefix, "+orig.") ){
               NewName->View = SUMA_copy_string("+orig");
            } else if (STRING_HAS_SUFFIX(NewName->Prefix, "+acpc.") ) {
               NewName->View = SUMA_copy_string("+acpc");
            } else if (STRING_HAS_SUFFIX(NewName->Prefix, "+tlrc.") ){
               NewName->View = SUMA_copy_string("+tlrc");
            }
            NewName->Prefix[strlen(NewName->Prefix)-6]='\0'; 
         } else {
            if (STRING_HAS_SUFFIX(NewName->Prefix, "+orig") ) {
               NewName->View = SUMA_copy_string("+orig");
            } else if (STRING_HAS_SUFFIX(NewName->Prefix, "+acpc") ) {
               NewName->View = SUMA_copy_string("+acpc");
            } else if (STRING_HAS_SUFFIX(NewName->Prefix, "+tlrc") ) {
               NewName->View = SUMA_copy_string("+tlrc");
            }
            NewName->Prefix[strlen(NewName->Prefix)-5]='\0'; 
         }   
	   } else {
         NewName->Prefix = SUMA_copy_string(NewName->FileName);
         NewName->View = SUMA_copy_string("");
      }
   }
   
   
   
   if (NewName->StorageMode == STORAGE_BY_BRICK) {
      if (NewName->View[0] != '\0') {
         NewName->HeadName = SUMA_append_string(NewName->Path,NewName->Prefix);
         NewName->HeadName = SUMA_append_replace_string(NewName->HeadName,
                                                NewName->View, "",1);
         NewName->BrikName = SUMA_append_string(NewName->HeadName, ".BRIK");
         NewName->HeadName = SUMA_append_string(NewName->HeadName, ".HEAD");
      } else {
         NewName->BrikName = SUMA_copy_string("");
         NewName->HeadName = SUMA_copy_string("");
      }
   } else {
      NewName->HeadName = SUMA_append_string(NewName->Path,NewName->FileName);
      NewName->BrikName = SUMA_append_string(NewName->Path,NewName->FileName);
   }
   NewName->OnDisk = -1;
   NewName->Size = -1;
   if (diskcheck) {
      SUMA_LH("Setting OnDisk for %s...", NewName->HeadName);
      NewName->OnDisk = THD_is_file(NewName->HeadName);
      if (NewName->OnDisk) {
         SUMA_LH("Setting filesize for %s...",  NewName->HeadName);
         NewName->Size = THD_filesize(NewName->HeadName);
      }
      if (!NewName->OnDisk) {
         NewName->ExistsAs = NULL;
         if (NewName->View[0] == '\0') { 
            char *sss = NULL;
                                 /* See if anything is there with 
                                 +orig, +acpc, +tlrc anything */
            if (!NewName->ExistsAs) {
               sss = SUMA_append_replace_string(NewName->Path,"+orig.HEAD",
                                                     NewName->Prefix, 0);
               if (THD_is_file(sss)) {
                  NewName->ExistsAs = sss; sss = NULL;
               } else {
                  SUMA_ifree(sss); sss = NULL;
               }
            }
            if (!NewName->ExistsAs) {
               sss = SUMA_append_replace_string(NewName->Path,"+acpc.HEAD",
                                                     NewName->Prefix, 0);
               if (THD_is_file(sss)) {
                  NewName->ExistsAs = sss; sss = NULL;
               } else {
                  SUMA_ifree(sss); sss = NULL;
               }
            }
            if (!NewName->ExistsAs) {
               sss = SUMA_append_replace_string(NewName->Path,"+tlrc.HEAD",
                                                     NewName->Prefix, 0);
               if (THD_is_file(sss)) {
                  NewName->ExistsAs = sss; sss = NULL;
               } else {
                  SUMA_ifree(sss); sss = NULL;
               }
            }
         }        
      } else {
         NewName->ExistsAs = SUMA_copy_string(NewName->HeadName);
      }
      if (!NewName->ExistsAs) { /* no nulls please */
         NewName->ExistsAs = (char*)SUMA_malloc(1*sizeof(char));
         NewName->ExistsAs[0] = '\0';
      }
   }
   if (LocalHead) {
      SUMA_ShowParsedFname(NewName, NULL);
   }
   if (cwd) SUMA_free(cwd);
   
	SUMA_RETURN (NewName);
}/*SUMA_ParseFname_eng*/

/*!
   \brief Lazy function calls to get at various parts of a file name without the
          pains of freeing and allocating. Do NOT free the returned string 
          
   Valid options for sel:
      "pa": path
      "Pa": Absolute path
      "e": Extension
      "fne": filename without path and without extension
      "f": filename without path
      "F": filename with path
      
   Note that the function can leak ONE allocated and filed SUMA_PARSED_NAME in
   a program's life cycle, unless one calls  SUMA_FnameGet(NULL, NULL) at the end
   
   WARNING: You can't use this function more than MAX_NUM_STR_FG times as  
            argument to a *printf command. Otherwise, you'll end up with the  
            repeated strings for call numbers that exceed MAX_NUM_STR_FG!
*/
#define MAX_NUM_STR_FG 10
char *SUMA_FnameGet(char *Fname, char *sel, char *cccwd)
{
   static char FuncName[]={"SUMA_FnameGet"};
   static char str[MAX_NUM_STR_FG]
                  [SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH+20]={""};
   static char lastid[SUMA_IDCODE_LENGTH]={""};
   char *currid=NULL;
   static int istr=-1;
   static SUMA_PARSED_NAME *ParsedFname=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   istr = (istr+1) % 10;
   str[istr][0] = '\0';
    
   if (!Fname) {
      /* cleanup */
      if (ParsedFname) SUMA_Free_Parsed_Name(ParsedFname); ParsedFname = NULL;
      SUMA_RETURN(str[istr]);
   }
   if (!sel) {
      SUMA_S_Err("no selection"); 
      SUMA_RETURN(str[istr]);
   }   
   

   /* is this a new name?*/
   if (!lastid[0]) { /* a fresh start, ParsedFname should be NULL */
      if (ParsedFname) {
         SUMA_S_Err("Oh boy oh boy, that's not good!"); 
         SUMA_RETURN(str[istr]);
      }
      if (!(ParsedFname = SUMA_ParseFname(Fname, cccwd))) 
         SUMA_RETURN(str[istr]);
      currid = UNIQ_hashcode(Fname);
      strcpy (lastid, currid);   /* store id */
      free(currid); currid = NULL;
   } else {
      currid = UNIQ_hashcode(Fname);
      if (strcmp(currid,lastid)) { /* different name */
         if (ParsedFname) SUMA_Free_Parsed_Name(ParsedFname);  /* free the old */
         if (!(ParsedFname = SUMA_ParseFname(Fname, cccwd))) 
            SUMA_RETURN(str[istr]);
         strcpy (lastid, currid);   /* store id */
      } else { /* same name, reuse old stuff */
         free(currid); currid = NULL;
      }
   }
   /* Now that you have the parsed name, return what user wants */
   if       (sel[0] == 'p' && sel[1] == 'a') 
      strcpy (str[istr], ParsedFname->Path);
   else if  (sel[0] == 'P' && sel[1] == 'a') 
      strcpy (str[istr], ParsedFname->AbsPath); 
   else if  (sel[0] == 'f' && sel[1] == '\0')
      strcpy (str[istr], ParsedFname->FileName);
   else if  (sel[0] == 'F' && sel[1] == '\0')
      strcpy (str[istr], ParsedFname->FullName);
   else if  (sel[0] == 'e' && sel[1] == '\0')
      strcpy (str[istr], ParsedFname->Ext); 
   else if  (sel[0] == 'f' && sel[1] == 'n' && sel[2] == 'e' )
      strcpy (str[istr], ParsedFname->FileName_NoExt); 
   else if  (sel[0] == 'l') {
      strcpy (str[istr], without_afni_filename_extension(ParsedFname->Prefix));
   } else {
      SUMA_S_Err("Selection not understood");
   }

   if (LocalHead) {
      SUMA_ShowParsedFname(ParsedFname, NULL);
      fprintf(SUMA_STDERR,
            "++   %s\n"
            "for >>%s<<\n"
            "sel >>%s<<\n"
            "ret.>>%s<<\n",
            FuncName, Fname, sel, str[istr]);
   }
   
   SUMA_RETURN(str[istr]);
}

SUMA_PARSED_NAME * SUMA_ParseModifyName(char *Fname, char *what, char *val, 
                                        char *cwd)
{
   SUMA_PARSED_NAME *pn=NULL, *pno=NULL;
   if (!Fname || !what) return(NULL);
   pn = SUMA_ParseFname(Fname, cwd);
   if (!pn) return(NULL);
   pno = SUMA_ModifyParsedName (pn, what, val);
   SUMA_Free_Parsed_Name(pn); 
   return(pno);
}

char * SUMA_ModifyName(char *Fname, char *what, char *val, char *cwd)
{
   char *oname=NULL;
   SUMA_PARSED_NAME *pn=NULL, *pno=NULL;
   if (!Fname || !what) return(NULL);
   pn = SUMA_ParseFname(Fname, cwd);
   if (!pn) return(NULL);
   pno = SUMA_ModifyParsedName (pn, what, val);
   SUMA_Free_Parsed_Name(pn); 
   if (pno) {
      oname = SUMA_append_replace_string(pno->Path,pno->FileName,"",0);
      oname = SUMA_append_replace_string(oname, pno->NodeSelect,"",1);
      oname = SUMA_append_replace_string(oname, pno->RowSelect,"",1);
      oname = SUMA_append_replace_string(oname, pno->ColSelect,"",1);
      SUMA_Free_Parsed_Name(pno);
   } 
   return(oname);
}

SUMA_PARSED_NAME * SUMA_DuplicateParsedName(SUMA_PARSED_NAME *pn) {
   
   if (pn && pn->NameAsParsed) {
      return(SUMA_ParseFname(pn->NameAsParsed, pn->cwdAsParsed));
   }
   return(NULL);
}

SUMA_PARSED_NAME * SUMA_ModifyParsedName (SUMA_PARSED_NAME *pn, 
                                          char *what, char *val)
{
   static char FuncName[]={"SUMA_ModifyParsedName"};
   char *fullname=NULL;
   SUMA_PARSED_NAME *pno=NULL;
   
   SUMA_ENTRY;
   
   if (!what || !pn) SUMA_RETURN(NULL);
   
   if (!strcmp(what,"prepend")) {
      if (!val) SUMA_RETURN(NULL);
      if (pn->StorageMode ==  STORAGE_BY_BRICK) {
         fullname=NULL;
         if(strstr(pn->NameAsParsed,"/")) 
            fullname = SUMA_append_replace_string(fullname, 
                                                pn->Path, "", 1);
         fullname = SUMA_append_replace_string(fullname,
                                                val, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                             pn->Prefix, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                             pn->View, "",1);
         fullname = SUMA_append_replace_string(fullname, 
                                             pn->TypeExt, "",1);
         fullname = SUMA_append_replace_string(fullname,  
                                    pn->NodeSelect, "", 1);
         fullname = SUMA_append_replace_string(fullname,  
                                    pn->RowSelect, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                    pn->ColSelect, "", 1);
         pno=SUMA_ParseFname(fullname, pn->RelDir);
         SUMA_free(fullname); fullname=NULL;
      } else {
         fullname=NULL;
         if(strstr(pn->NameAsParsed,"/")) 
            fullname = SUMA_append_replace_string(fullname, 
                                                pn->Path, "", 1);
         fullname = SUMA_append_replace_string(fullname,
                                                val, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                             pn->Prefix, "", 1);
         fullname = SUMA_append_replace_string(fullname,  
                                    pn->NodeSelect, "", 1);
         fullname = SUMA_append_replace_string(fullname,  
                                    pn->RowSelect, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                    pn->ColSelect, "", 1);
         pno=SUMA_ParseFname(fullname, pn->RelDir);
         SUMA_free(fullname); fullname=NULL;
      }
   } else if (!strcmp(what,"append")) {
      if (!val) SUMA_RETURN(NULL);
      if (pn->StorageMode ==  STORAGE_BY_BRICK) {
         fullname=NULL;
         if(strstr(pn->NameAsParsed,"/")) 
            fullname = SUMA_append_replace_string(fullname, 
                                                pn->Path, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                             pn->Prefix, "", 1);
         fullname = SUMA_append_replace_string(fullname,
                                                val, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                             pn->View, "",1);
         fullname = SUMA_append_replace_string(fullname, 
                                             pn->TypeExt, "",1);
         fullname = SUMA_append_replace_string(fullname,  
                                    pn->NodeSelect, "", 1);
         fullname = SUMA_append_replace_string(fullname,  
                                    pn->RowSelect, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                    pn->ColSelect, "", 1);
         pno=SUMA_ParseFname(fullname, pn->RelDir);
         SUMA_free(fullname); fullname=NULL;
      } else {
         fullname=NULL;
         if(strstr(pn->NameAsParsed,"/")) 
            fullname = SUMA_append_replace_string(fullname, 
                                                pn->Path, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                             pn->Prefix, "", 1);
         fullname[strlen(fullname)-strlen(pn->TypeExt)]='\0';
         fullname = SUMA_append_replace_string(fullname,
                                                val, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                             pn->TypeExt, "",1);
         fullname = SUMA_append_replace_string(fullname,  
                                    pn->NodeSelect, "", 1);
         fullname = SUMA_append_replace_string(fullname,  
                                    pn->RowSelect, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                    pn->ColSelect, "", 1);
         pno=SUMA_ParseFname(fullname, pn->RelDir);
         SUMA_free(fullname); fullname=NULL;
      }
   } else if (!strcmp(what,"view")) {
      char vval[6]={""};
      if (!val) SUMA_RETURN(NULL);
      if (val[0] != '+') sprintf(vval,"+%c%c%c%c",val[0],val[1],val[2],val[3]);
      else sprintf(vval,"%c%c%c%c%c",val[0],val[1],val[2],val[3], val[4]);
      if (pn->StorageMode ==  STORAGE_BY_BRICK ||
          pn->StorageMode ==  STORAGE_UNDEFINED) {
         fullname=NULL;
         if(strstr(pn->NameAsParsed,"/")) 
            fullname = SUMA_append_replace_string(fullname, 
                                                pn->Path, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                             pn->Prefix, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                              vval,"",1);
         fullname = SUMA_append_replace_string(fullname, 
                                             pn->TypeExt, "",1);
         fullname = SUMA_append_replace_string(fullname,  
                                    pn->NodeSelect, "", 1);
         fullname = SUMA_append_replace_string(fullname,  
                                    pn->RowSelect, "", 1);
         fullname = SUMA_append_replace_string(fullname, 
                                    pn->ColSelect, "", 1);
         pno=SUMA_ParseFname(fullname, pn->RelDir);
         SUMA_free(fullname); fullname=NULL;
      } else {
         pno = SUMA_DuplicateParsedName(pn);
      }
   }
   
   SUMA_RETURN(pno);
}
/*!
   \brief ans = SUMA_isExtension(filename, ext);
      YUP if filename has the extension ext
*/
SUMA_Boolean SUMA_isExtension(char *filename, char *ext)
{
   static char FuncName[]={"SUMA_isExtension"}; 
   int cnt, N_ext, N_filename;
      
   SUMA_ENTRY;

   if (!filename) SUMA_RETURN(NOPE);
   if (!ext) SUMA_RETURN(NOPE);
   N_ext = strlen(ext);
   N_filename = strlen(filename);
   if (N_ext > N_filename) SUMA_RETURN(NOPE);

   cnt = 1;
   while (cnt <= N_ext) {
      if (filename[N_filename-cnt] != ext[N_ext-cnt]) SUMA_RETURN(NOPE);
      ++cnt; 
   } 
   
   SUMA_RETURN(YUP);
}

char * SUMA_CropExtension(char *filename, char *ext)
{
   static char FuncName[]={"SUMA_CropExtension"}; 
   int cnt, N_ext, N_filename;
      
   SUMA_ENTRY;

   if (!filename) SUMA_RETURN(filename);
   if (!ext) SUMA_RETURN(filename);
   N_ext = strlen(ext);
   N_filename = strlen(filename);
   if (N_ext > N_filename) SUMA_RETURN(filename);

   cnt = 1;
   while (cnt <= N_ext) {
      if (filename[N_filename-cnt] != ext[N_ext-cnt]) SUMA_RETURN(filename);
      ++cnt; 
   } 
   filename[N_filename-N_ext] = '\0';
   
   SUMA_RETURN(filename);
}

/*!
   \brief ans = SUMA_Extension(filename, ext, Remove);
      removes or enforces an arbitrary extension from/to a filename
   
   \param filename(char *) input filename
   \param ext (char *) extension
   \param Remove (SUMA_Boolean) YUP = Remove extension if found
                                      Do nothing if it is not there already 
                                NOPE = Add extension if not there
                                       Do nothing if it is there already    
   \returns ans (char*) containing modified filename 
  
   - You must free ans on your own
   Examples:
      {
      char *ans=NULL;
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roi", YUP);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roi", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roxi", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", ".niml.roxi", YUP);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("Junk.niml.roi", "", YUP);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension(".roi", "Junk.niml.roi", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      ans = SUMA_Extension("", "", NOPE);
      SUMA_LH(ans); SUMA_free(ans);
      
      exit(1);
    }

*/

char *SUMA_Extension(char *filename, char *ext, SUMA_Boolean Remove)
{
   static char FuncName[]={"SUMA_Extension"}; 
   char *ans = NULL;
   int i, next, nfilename, ifile;
   SUMA_Boolean NoMatch = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;

   if (!filename) SUMA_RETURN(NULL);
   nfilename = strlen(filename);
   
   if (!ext) {
      ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
      ans = strcpy(ans,filename);
      SUMA_RETURN(ans);
   }
   next = strlen(ext);
   
   if (next > nfilename && Remove) {
      ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
      ans = strcpy(ans,filename);
      SUMA_RETURN(ans);
   }
   #if 0
   if (nfilename < next || next < 1 || nfilename < 1) {
      ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
      ans = strcpy(ans,filename);
      SUMA_RETURN(ans);
   }
   #endif
   
   
   ifile = nfilename - next;
   if (ifile > 0) {
      NoMatch = NOPE;
      i = 0;
      do {
         if (LocalHead) 
            fprintf (SUMA_STDERR,
                     "%s: Comparing %c %c\n", 
                     FuncName, filename[ifile+i], ext[i]);
         if (filename[ifile+i] != ext[i]) NoMatch = YUP;
         ++i;
      }  while (ifile < nfilename && i < next && !NoMatch);
   } else {
      NoMatch = YUP;
   }
   
   if (NoMatch) {
      if (Remove) { /* nothing to do */
         SUMA_LH("NoMatch, nothing to do");
         ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
         ans = strcpy(ans,filename);
         SUMA_RETURN(ans);
      } else { /* add extension */
         SUMA_LH("NoMatch, adding extension");
         ans = SUMA_append_extension(filename, ext);
         SUMA_RETURN(ans);
      }
   }else {
      if (Remove) { /* remove it */
         SUMA_LH("Match, removing extension");
         ans = (char *)SUMA_malloc((nfilename - next+2)*sizeof(char));
         for (i=0; i< nfilename - next; ++i)  ans[i] = filename[i];
         ans[nfilename - next] = '\0'; /* for good measure */
      } else { /* nothing to do */
         SUMA_LH("Match, nothing to do");
         ans = (char *)SUMA_malloc((nfilename+1)*sizeof(char));
         ans = strcpy(ans,filename);
         SUMA_RETURN(ans);
      }
   }
   
   SUMA_RETURN (ans);

}
   
void *SUMA_Free_Parsed_Name(SUMA_PARSED_NAME *Test) 
{
   static char FuncName[]={"SUMA_Free_Parsed_Name"}; 

   SUMA_ENTRY;

   if (!Test) SUMA_RETURN (NULL);
   if (Test->AbsPath) SUMA_free(Test->AbsPath);
   if (Test->RelPath) SUMA_free(Test->RelPath);
   if (Test->RelDir) SUMA_free(Test->RelDir);
   if (Test->Path) SUMA_free(Test->Path);
   if (Test->FileName) SUMA_free(Test->FileName);
   if (Test->FullName) SUMA_free(Test->FullName);
   if (Test->Ext) SUMA_free(Test->Ext);
   if (Test->FileName_NoExt) SUMA_free(Test->FileName_NoExt);
   if (Test->RowSelect) SUMA_free(Test->RowSelect);
   if (Test->ColSelect) SUMA_free(Test->ColSelect);
   if (Test->NodeSelect) SUMA_free(Test->NodeSelect);
   if (Test->RangeSelect) SUMA_free(Test->RangeSelect);
   if (Test->NameAsParsed) SUMA_free(Test->NameAsParsed);
   if (Test->cwdAsParsed) SUMA_free(Test->cwdAsParsed);
   if (Test->ExistsAs) SUMA_free(Test->ExistsAs);
   
   SUMA_free(Test);
   
   SUMA_RETURN (NULL);
}



/*! Taken from filexists 
returns 1 if file can be read/found
*/
int SUMA_filexists (char *f_name)
{/*SUMA_filexists*/
    FILE *outfile;
    static char FuncName[]={"SUMA_filexists"};
   
   SUMA_ENTRY;

   outfile = fopen (f_name,"r");
   /*fprintf(stderr,"%s %p\n", f_name, outfile);*/
   if (outfile == NULL) {
       SUMA_RETURN(0); 
   } else {
       fclose (outfile); 
   }
    
   SUMA_RETURN(1);
       
}/*SUMA_filexists*/

/*! \brief A function that attempts to find a file that is readable.
   If *fname has a full path, then returns 1 if readable, 0 otherwise
   If *fname has no path,
      Try finding file under SUMAg_cwd/ if found -> 1, else, continue
      If a search path is given, then that path is searched
         if file is found -> 1 else -> 0
      If no search path is given, then path from user's environment is searched.
   If file is found, old name is freed and new one put in its place.
*/
int SUMA_search_file(char **fnamep, char *epath) 
{
   static char FuncName[]={"SUMA_search_file"};
   SUMA_PARSED_NAME *pn = NULL;
   char dname[THD_MAX_NAME], ename[THD_MAX_NAME], *elocal=NULL, *af=NULL;
   int epos=0, ll=0, ii=0, id = 0, imode=1;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* does it exist? */
   if ( SUMA_filexists(*fnamep) ) {
      SUMA_LH("Found file %s as named", *fnamep);
      SUMA_RETURN(1); /* all is well */
   }
   
   SUMA_LH("Searching for variations on %s", *fnamep);
   #if defined SUMA_COMPILED
   /* else, the hard work, first check with cwd options 
   for suma programs*/
   pn = SUMA_ParseFname(*fnamep, SUMAg_CF->cwd);
   if ( SUMA_filexists(pn->FullName) ) {
      SUMA_free(*fnamep); 
      *fnamep = SUMA_copy_string(pn->FullName);
      pn = SUMA_Free_Parsed_Name(pn);
      SUMA_RETURN(1); /* all is well */
   }
   pn = SUMA_Free_Parsed_Name(pn);
   /* try again perhaps for compressed data */
   elocal = SUMA_append_string(*fnamep, ".gz");
   pn = SUMA_ParseFname(elocal, SUMAg_CF->cwd);
   if ( SUMA_filexists(pn->FullName) ) {
      SUMA_free(*fnamep); 
      *fnamep = SUMA_copy_string(pn->FullName);
      pn = SUMA_Free_Parsed_Name(pn);
      SUMA_RETURN(2); /* all is well */
   }
   pn = SUMA_Free_Parsed_Name(pn);
   #endif
   
   /* Now work the path (based on code form get_atlas function */
   if (!epath) {
      #if 0 /* overkill, as Yaroslav Halchenko pointed out*/
         epath = getenv("PATH") ;
         if( epath == NULL ) SUMA_RETURN(NOPE) ; /* nothing left to do */
      #else
         /* Search in AFNI's standard locations */
         af = find_afni_file(*fnamep, 0, NULL);
         if (af[0] != '\0') {
            SUMA_free(*fnamep); 
            *fnamep = SUMA_copy_string(af);
            SUMA_RETURN(1);
         } 
      #endif
      SUMA_RETURN(NOPE); /* miserable pathless failure */
   }
   
   /*----- copy path list into local memory -----*/

   ll = strlen(epath) ;
   elocal = (char *)SUMA_calloc(ll+2, sizeof(char));

   /*----- put a blank at the end -----*/
   strcpy( elocal , epath ) ; elocal[ll] = ' ' ; elocal[ll+1] = '\0' ;

   /*----- replace colons with blanks -----*/
   for( ii=0 ; ii < ll ; ii++ )
     if( elocal[ii] == ':' ) elocal[ii] = ' ' ;

   /*----- extract blank delimited strings;
           use as directory names to look for atlas -----*/
   imode = 1;
   while (imode < 3) {
      epos = 0 ;
      do{
         ii = sscanf( elocal+epos , "%s%n" , ename , &id ); /* next substring */
         if( ii < 1 ) break ;                               /* none -> done   */

         epos += id ;                               /* char after last scanned */

         ii = strlen(ename) ;                         /* make sure name has   */
         if( ename[ii-1] != '/' ){                    /* a trailing '/' on it */
             ename[ii]  = '/' ; ename[ii+1] = '\0' ;
         }
         strcpy(dname,ename) ;
         SUMA_strncat(dname,*fnamep, THD_MAX_NAME-1) ;     /* add dataset name */
         if (imode == 2) {
            SUMA_strncat(dname,".gz", THD_MAX_NAME-1); /* add compression flag */
         }
         if ( SUMA_filexists(dname) ) {
            SUMA_free(*fnamep); *fnamep = SUMA_copy_string(dname);
            SUMA_free(elocal); elocal=NULL;
            SUMA_RETURN(imode); /* all is well */
         }

      } while( epos < ll ) ;  /* scan until 'epos' is after end of epath */
      ++imode;
   } 
   /* nothing */
   SUMA_free(elocal); elocal=NULL;
   
   SUMA_RETURN(0); /* bummer */
}

/*!
   \brief function that tests whether a string contains N numbers
   
   WARNING: This function will deform s ! 
   
   \param str (char *) null terminated string
   \param N (void *) This is an integer in disguise
   \return 1: If str is NULL or N numbers were found in str
   
   \sa SUMA_isNumString
*/
int SUMA_CleanNumString (char *s, void *p)
{
   static char FuncName[]={"SUMA_CleanNumString"};
   char *endp, *strtp;
   int nd, N;
   int eos, FoundTip;
   double d;
   int LocalHead = 0;
   
   SUMA_ENTRY;
   
   if (!s) SUMA_RETURN(1);
   
   #if INT_MAX < LONG_MAX
      N = (int)(long int)p;
   #else 
      N = (int)p;
   #endif
   
   /* clean s by removing trailing junk then replacing non characters by space*/
   if (LocalHead) fprintf (stderr, "%s: string begins:%s:\n", FuncName, s);
   FoundTip = 0;
   for (nd=strlen(s)-1; nd >=0; --nd) {
      if (!isdigit(s[nd]) && s[nd] != '.'  && s[nd] != '-' && s[nd] != '+') {
         if (!FoundTip) {
            s[nd]= '\0'; /* remove */
         } else {
            s[nd] = ' '; /* blank */
         }
      }else {
         FoundTip = 1;
      }
   }
   
   if (LocalHead) fprintf (stderr, "%s: string now:%s:\n", FuncName, s);
   if (strlen(s) == 1 && (s[0] == '+' || s[0] == '-' || s[0] == '.')) {
      SUMA_RETURN(0);
   }
   
   /* parse s */
   strtp = s;
   endp = NULL;
   nd = 0;
   eos = 0;
   while (!eos) {
      errno=0;
      d = strtod(strtp, &endp);
      /* See SUMA_strtod() for an example on exception handling for strtod */
      SUMA_LHv("value %f, ERANGE: %d, EDOM %d, errno %d\n", 
               d, ERANGE, EDOM, errno); 
      

      if (endp == strtp && *endp=='\0') { 
         eos = 1;
      } else {
         strtp = endp;
         ++nd;
         if (nd > N && nd > 1000) {
            SUMA_SL_Err("Fishy fish");
            fprintf (stderr, "%s: >>>%s<<<", FuncName, s);
            SUMA_RETURN(0);
         }
      }
   }
   
   if (LocalHead) fprintf (stderr,"%s: Read %d/%d values.\n", FuncName, nd,N);
   if (N != nd) {
      SUMA_RETURN(0);
   } else {
      SUMA_RETURN(1);
   }
   
}

int SUMA_CleanNumStringSide (char *s, void *p)
{   
   static char FuncName[]={"SUMA_CleanNumStringSide"};
   char *s2=NULL, c ='\0';
   int nn=0;
   
   SUMA_ENTRY;
   
   if (!s) SUMA_RETURN(SUMA_CleanNumString(s,p));
   deblank_name(s);
   
   nn = strlen(s);
   if (s[0]=='r' || s[0]=='R') {
      c = 'R';
      s2 = SUMA_copy_string(s+1); 
   } else if (s[nn-1]=='r' || s[nn-1]=='R') {
      c = 'R';
      s[nn-1]='\0'; s2 = SUMA_copy_string(s); 
   } else if (s[0]=='l' || s[0]=='L') {
      c = 'L';
      s2 = SUMA_copy_string(s+1);          
   } else if (s[nn-1]=='l' || s[nn-1]=='L') {
      c = 'L';
      s[nn-1]='\0'; s2 = SUMA_copy_string(s); 
   } else {
      /* nothing to do */
      SUMA_RETURN(SUMA_CleanNumString(s,p));
   }
   
   /* Now clean s2 */
   s2 = SUMA_copy_string(s); 
   nn = SUMA_CleanNumString(s2,p);
   
   /* Put side back in string */
   sprintf(s,"%c%s",c,s2);
   SUMA_free(s2); s2=NULL;
   
   SUMA_RETURN(nn);
}

/*
   Much like SUMA_CleanNumString, but leaves s untouched
*/
int SUMA_isNumString (char *s, void *p) 
{
   static char FuncName[]={"SUMA_isNumString"};
   int ans;
   char *sc;
   
   SUMA_ENTRY;
   
   sc = SUMA_copy_string(s);
   ans = SUMA_CleanNumString(sc,p);
   if(sc) SUMA_free(sc); sc = NULL;
   SUMA_RETURN(ans);
}


int SUMA_NumStringUnits (char *s, int marktip) 
{
   static char FuncName[]={"SUMA_NumStringUnits"};
   int unt = SUMA_NO_NUM_UNITS;
   int FoundTip = 0, nd = 0, ndm=0;
   int LocalHead = 0;
   
   SUMA_ENTRY;
   
   if (!s) SUMA_RETURN(unt);
   
   /* go back until you hit the tip of the number */
   FoundTip = 0;
   ndm = strlen(s);
   nd=ndm-1;
   while ( nd >=0 && !FoundTip) {
      if (isdigit(s[nd]) || s[nd] == '.' || s[nd] == '-' || s[nd] == '+') {
         FoundTip = 1;
      } else {
         --nd;
      }
   }
   if (!FoundTip) SUMA_RETURN(unt);
   
   if (marktip) s[nd] = '\0';
   
   
   /* now move forward, skipping blanks, commas, parenthesis */
   SUMA_LH("Got tip, goind forward");
   ++nd;
   FoundTip = 0;
   while (nd < ndm && !FoundTip) {
      if (  isspace(s[nd]) || s[nd] == ',' || 
            s[nd] == '[' || s[nd] == '(' || s[nd] == '{') {
         ++nd;
      } else {
         FoundTip = 1;
      }
   }
 
   /* now look for unit string */
   SUMA_LH("%s",(s+nd));
   unt = SUMA_NO_NUM_UNITS;
   if (0) ; /* order of following else ifs matters */
   else if (!strncmp((s+nd), "mm", 2)) 
                              SUMA_RETURN(SUMA_MM_UNITS);
   else if (!strncmp((s+nd), "p", 1)) 
                              SUMA_RETURN(SUMA_P_VALUE_UNITS);
   else if (!strncmp((s+nd), "q",1)) 
                              SUMA_RETURN(SUMA_Q_VALUE_UNITS);
   else if (!strncmp((s+nd), "%",1)) 
                              SUMA_RETURN(SUMA_PERC_VALUE_UNITS);
   SUMA_RETURN(unt);
}

int SUMA_strtod(char *n, double *valp)
{
   static char FuncName[]={"SUMA_strtod"};
   char *stp=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   if (!n || !valp) SUMA_RETURN(0);
   
   errno = 0;
   *valp = strtod (n, &stp);
   
   #if 0
      /* Not all constants below are standard */
   SUMA_LHv("s=%s, stp=%s, val=%f, \n"
            "errno=%d, HUGE_VAL=%g,%Lg,%g, EINVAL=%d,ERANGE=%d\n",
            (char *)n, CHECK_NULL_STR(stp), *valp, errno,
            HUGE_VAL, HUGE_VALL, HUGE_VALF, EINVAL, ERANGE);
   #endif
   
   if ((errno == ERANGE &&(*valp == LONG_MAX || *valp == LONG_MIN))
         || (errno != 0 && *valp == 0) ||
         stp == n /* nothing numeric was read */) {
      SUMA_RETURN(0);
   }
   
   /* all is well */
   SUMA_RETURN(1);
}

/*!
   \brief function that parses a string of numbers into a float vector
   
   \param str (char *) null terminated string
   \param vv (void*) vector where values will be stored
   \param N (int) This is the number of values desired
   \param prec (int) 1=float 2=double
   \return int: This is the number of values read. 
      The function will not register in fv more than N values 
      (to keep from running over preallocated space), but it
      will return the full number of values found.
      
      -1 in case of error
   \sa SUMA_CleanNumString
   \sa SUMA_strtol_vec
   \sa SUMA_AdvancePastNumbers
   \sa SUMA_NumStringUnits
*/

int SUMA_StringToNum (char *s, void *vv, int N, int prec)
{
   static char FuncName[]={"SUMA_StringToNum"};
   char *endp, *strtp;
   int nd;
   int eos, FoundTip;
   double d;
   float *fv=NULL;
   double *dv=NULL;
   int LocalHead = 0;
   
   SUMA_ENTRY;
   
   if (!s || prec < 1) SUMA_RETURN(0); 
     
   if (LocalHead) fprintf (stderr, "%s: string was:%s:\n", FuncName, s);
   /* clean s by removing trailing junk then replacing non characters by space*/
   FoundTip = 0;
   for (nd=strlen(s)-1; nd >=0; --nd) {
      if (!SUMA_IS_NUM_CHAR(s,nd)) {
         if (!FoundTip) {
            s[nd]= '\0'; /* remove */
         } else {
            s[nd] = ' '; /* blank */
         }
      }else {
         FoundTip = 1;
      }
   }
   
   if (LocalHead) fprintf (stderr, "%s: string now:%s:\n", FuncName, s);
   
   if (prec > 1) dv = (double *)vv;
   else fv = (float *)vv;
   
   /* parse s */
   strtp = s;
   endp = NULL;
   nd = 0;
   eos = 0;
   while (!eos) {
      errno = 0;
      d = strtod(strtp, &endp);
      /* See SUMA_strtod() for an example on exception handling for strtod */
    
      if (endp == strtp && *endp=='\0') { 
         eos = 1;
      } else {
         if (nd < N) {
            if (prec > 1) dv[nd] = d;
            else fv[nd] = (float)d;
         }
         strtp = endp;
         ++nd;
         if (nd > N && nd >1000) {
            SUMA_SL_Err("Something's fishy");
            fprintf (stderr, "s = >>>%s<<<\nnd = %d\n", s, nd);
            SUMA_RETURN(-1);
         }
      }
   }
   
   if (LocalHead) fprintf (stderr,"%s: Read %d/%d values.\n", FuncName, nd, N);
   
   SUMA_RETURN(nd);
   
}   

/* Like SUMA_StringToNum but looks for side flags in beginning or end
Those would be L or R at the very beginning or very end.
Function also deblanks s 
*/
int SUMA_StringToNumSide(char *s, void *vv, int N, int prec, int *Side)
{
   static char FuncName[]={"SUMA_StringToNumSide"};
   int nn = 0;
   
   SUMA_ENTRY;
   
   *Side = SUMA_NO_SIDE;
   if (!s) SUMA_RETURN(SUMA_StringToNum(s,vv,N,prec));
   
   deblank_name(s);
   /* Could get something like 'v"55R"' from DriveSuma. clean a little */
   if (s[0] == 'v') {
      ++s;
      dequote_name(s, '\0');
   }
   nn = strlen(s);
   if (s[0]=='r' || s[0]=='R') {
      *Side = SUMA_RIGHT;
      ++s;
   } else if (s[nn-1]=='r' || s[nn-1]=='R') {
      *Side = SUMA_RIGHT;
      s[nn-1]='\0';
   } else if (s[0]=='l' || s[0]=='L') {
      *Side = SUMA_LEFT;
      ++s;         
   } else if (s[nn-1]=='l' || s[nn-1]=='L') {
      *Side = SUMA_LEFT;
      s[nn-1]='\0';
   }

   SUMA_RETURN(SUMA_StringToNum(s,vv,N,prec));         
}


/*!
   \brief forces a string to be of a certain length.
   If truncation is necessary, ... are inserted at 
   the end of the string.
   
   You need to free the returned pointer
*/
char *SUMA_set_string_length(char *buf, char cp, int n)
{
   static char FuncName[]={"SUMA_set_string_length"};
   char *lbl=NULL, *lbl30=NULL;
   
   SUMA_ENTRY;
   
   if (!buf) SUMA_RETURN(NULL);
   
   lbl = SUMA_truncate_string (buf, n);
   if (!lbl) {
      SUMA_SL_Err("Failed to truncate");
      SUMA_RETURN(NULL);
   }
         
   if (strlen(lbl) != n) {
      lbl30 = SUMA_pad_string(lbl, cp, n, 1); 
      SUMA_free(lbl); lbl = NULL;
   } else {
      lbl30 = lbl; lbl = NULL;
   }
   
   SUMA_RETURN(lbl30);
}

/*!
   \brief padds a string to a certain length.
   You can use this function to crop a string to 
   the specified number of characters n
   Padding is done with character cp
   The original string is not modified.
    
   s_tr = SUMA_pad_string(s1, cp, n, add2end);
   
   \sa SUMA_pad_str
   \sa SUMA_truncate_string
   - free returned pointer with: if(s_tr) SUMA_free(s_tr);
*/
char *SUMA_pad_string(char *buf, char cp, int n, int add2end)
{
   static char FuncName[]={"SUMA_pad_string"};
   char *atr = NULL;
   int i, ib, nb;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!buf) SUMA_RETURN(NULL);
   
   atr = (char *) SUMA_calloc(n+2, sizeof(char));
   nb = strlen(buf);
   
   if (add2end) { /* add to end */
      i=0;
      while (i < n) {
         if (i<nb) atr[i] = buf[i];
         else atr[i] = cp;
         ++i;
      }
      atr[i] = '\0';
   } else {
      atr[n] = '\0';
      i = n -1; 
      ib = nb - 1;
      while (i >= 0) {
         if (ib >=0) atr[i] = buf[ib];
         else atr[i] = cp;
         --i; --ib;
      }
      
   }
   
   if (LocalHead) {
      fprintf(SUMA_STDERR,"%s:\nin\t:%s:\nout\t:%s:\n", FuncName, buf, atr);
   }
   SUMA_RETURN(atr);  
}

/*!
   \brief truncates a string to a certain length.
   Adds ... as the last characters of the string
   The original string is not modified.
    
   s_tr = SUMA_truncate_string(s1, n);
   
   - free returned pointer with: if(s_tr) SUMA_free(s_tr);
*/
char *SUMA_truncate_string(char *buf, int n)
{
   static char FuncName[]={"SUMA_truncate_string"};
   char *atr = NULL;
   int i;

   SUMA_ENTRY;
   
   if (!buf) SUMA_RETURN(NULL);
   
   if (n < 5) {
      fprintf(stderr,"Error %s:\nNot worth the effort. N < 5.", FuncName);
      SUMA_RETURN(NULL);
   }
   
   if (strlen(buf) <= n) {
      atr = (char *) SUMA_calloc(strlen(buf)+2, sizeof(char));
      sprintf(atr, "%s", buf);
      SUMA_RETURN (atr);
   }else {
      atr = (char *) SUMA_calloc(n+3, sizeof(char));
      i=0;
      while (i < n - 3) {
         atr[i] = buf[i];
         ++i;
      }
      atr[i] = atr[i+1] = atr[i+2] = '.';
      atr[i+3] = '\0';
   }
   
   SUMA_RETURN(atr);  
}

/*!
   \brief returns a copy of a null terminated string . 
   s_cp = SUMA_copy_string(s1);
   
   - free returned pointer with: if(s_cp) SUMA_free(s_cp);
*/
char *SUMA_copy_string(char *buf)
{
   static char FuncName[]={"SUMA_copy_string"};
   char *atr = NULL;
   int i;
   
   SUMA_ENTRY;
   
   if (!buf) SUMA_RETURN(NULL);
   
   atr = (char *) SUMA_calloc(strlen(buf)+2, sizeof(char));
   
   i=0;
   while (buf[i]) {
      atr[i] = buf[i];
      ++i;
   }
   atr[i] = '\0';
   
   SUMA_RETURN(atr);  
}

/*!
   brief Return a copy of string between quotes q1 and q2
   s (char*) input string
   eop (char *) if != NULL, stop when s reaches eop, else keep going
                till end of s if necessary
   q1 (char) opening quote. If '\0', take opening quote as s[0]
                           after you deblank s
   q2 (char) closing quote. If '\0', q2 = q1
   is_closed (int *) on return, set to 1 if found opening and closing quotes
                            0 otherwise
   deblank (int) remove blanks after q1 and before q2
   withqotes (int)  if 1 then put quotes back on output                          
   
   returns qs (char *) the quoted string. Free with SUMA_free(qs);
*/   
char *SUMA_copy_quoted( char *s, char *eop, 
                        char q1, char q2,
                        int deblank, int withquotes,
                        int *is_closed ) {
   static char FuncName[]={"SUMA_copy_quoted"};
   char *strn=NULL;
   char *op=s, *op2=NULL;  
   
   SUMA_ENTRY;
   
   if (!s) SUMA_RETURN(strn);
   SUMA_SKIP_BLANK(s,eop); 
   
   op=s;
   if (q1 == '\0') { q1=*op;}   
   if (q2 == '\0') { q2=q1; } 
     
   SUMA_SKIP_TO_NEXT_CHAR(op, eop, q1);   
   
   op2=op+1;  
   SUMA_SKIP_TO_NEXT_CHAR(op2, eop, q2);   
   
   /* decide on closure, op and op2 are at the quotes*/
   if (is_closed) {
      if (*op == q1 && *op2 == q2) *is_closed = 1;
      else *is_closed = 0;  
   }
   /* deblanking */
   if (deblank) {
      /* move up from q1 and skip blanks */
      ++op;
      while (SUMA_IS_BLANK(*op) && op < op2) { ++op; }
      --op; *op=q1;/* go back one and put q1 back */
      
      /* move down from q2 and skip blanls */
      --op2;
      while (SUMA_IS_BLANK(*op2) && op2 > op) { --op2; }
      ++op2; *op2=q2;/* go forward one and put q2 back */
   }
   
   if (withquotes) { ++op2; SUMA_COPY_TO_STRING(op,op2,strn); }
   else { ++op; SUMA_COPY_TO_STRING(op,op2,strn);}
   
   SUMA_RETURN(strn);
}

/*! 
   Put all arguments between opening and closing quotes into one string
   The function starts by looking for argv[*kar] that begins with opq
   If opq is found, it continues looking until it finds argv[K] with ends
   with cloq. If a closing quote is found, a catenation of all the argvs
   is returned. Also *kar is updated to indicate the last used argument.
*/
char *args_in_quotes(char **argv, int *kar, int N_argv, 
                     char *opq, char *cloq, int clearused)
{
   static char FuncName[]={"args_in_quotes"};
   char *aq=NULL;
   int n, closed, n2;
   SUMA_Boolean LocalHead=NOPE;
   
   SUMA_ENTRY;
   
   if (!argv || !N_argv || !kar || *kar >= N_argv || !opq) RETURN(aq);
   
   n = *kar;
   if (begins_with(argv[n], opq,1)) {
      aq = SUMA_copy_string(argv[n]);
   } else {
      SUMA_RETURN(NULL);
   }  
   SUMA_LHv("Begin aq %s, n=%d, argv[n]=%s, N=%d\n", aq, n, argv[n], N_argv);
   closed = 0;
   while (!(closed=ends_with(argv[n],cloq,1)) && n<N_argv-1) {
      aq = SUMA_append_replace_string(aq,argv[++n]," ",1);
      SUMA_LHv("added aq %s, n=%d, argv[n]=%s, N=%d\n", aq, n, argv[n], N_argv);
   }
   if (!closed) {
      SUMA_LHv("Could not find closing %s\n",cloq);
      SUMA_free(aq); 
      aq = NULL;
   } else {
      if (clearused) {
         n2 = *kar;
         while (n2 < n) {
            argv[n2][0] = '\0'; ++n2;
         }
      }
      *kar = n; /* the last argument to be used */
   }
   
   SUMA_RETURN(aq);
}      

char *args_in_niml_quotes(char **argv, int *kar, int N_argv, int clearused) 
{
   char *aq=NULL;
   
   if ((aq=args_in_quotes(argv, kar, N_argv,"<","/>", clearused))) {
      return(aq);
   } else if ((aq=args_in_quotes(argv, kar, N_argv,"'<","/>'", clearused))) {
      return(aq);
   } else if ((aq=args_in_quotes(argv, kar, N_argv,"\"<","/>\"", clearused))) {
      return(aq);
   } 
   return(NULL);
}

char *args_in_simple_quotes(char **argv, int *kar, int N_argv, int clearused) 
{
   char *aq=NULL;
   
   if ((aq=args_in_quotes(argv, kar, N_argv,"'","'", clearused))) {
      return(aq);
   } else if ((aq=args_in_quotes(argv, kar, N_argv,"\"","\"", clearused))) {
      return(aq);
   }  
   return(NULL);
}

char * SUMA_append_extension(char *s1, char *s2)
{
   static char FuncName[]={"SUMA_append_extension"};
   char *s1c = NULL;
   int ns1c=0;
   
   SUMA_ENTRY;
   
   /* remove last dot */
   if (s1) {
      s1c = SUMA_copy_string(s1);
      ns1c = strlen(s1);
      if (s1c[ns1c-1]=='.') s1c[ns1c-1]='\0';
   }
   
   /* remove first dot */
   if (s2 && s2[0] == '.') ++s2;
   
   /* put them together */
   SUMA_RETURN(SUMA_append_replace_string(s1c, s2, ".", 1));
}

/*!
   \brief appends two null terminated strings.
   
   s_ap = SUMA_append_string(s1, s2);
  
   - s1 and s2 are copied into a new string
   -free returned pointer with:  if(s_ap) SUMA_free(s_ap);
   - None of the strings passed to the function
   are freed.
   
   \sa SUMA_append_replace_string
*/
char * SUMA_append_string(char *s1, char *s2)
{
   static char FuncName[]={"SUMA_append_string"};
   char *atr = NULL;
   int i,cnt, N_s2, N_s1;

   
   SUMA_ENTRY;
   
   if (!s1 && !s2) SUMA_RETURN(NULL);
   if (!s1) N_s1 = 0;
   else N_s1 = strlen(s1);
   
   if (!s2) N_s2 = 0;
   else N_s2 = strlen(s2);
   
   atr = (char *) SUMA_calloc(N_s1+N_s2+2, sizeof(char));
   
   /* copy first string */
   cnt = 0;
   if (N_s1){
      i=0;
      while (s1[i]) {
         atr[cnt] = s1[i];
         ++i;
         ++cnt;
      }
   }   
   if (N_s2) {
      i=0;
      while (s2[i]) {   
         atr[cnt] = s2[i];
         ++i;
         ++cnt;
      }
   }
   atr[cnt] = '\0';
   
   SUMA_RETURN(atr);  
}    


/*!
   \brief appends two null terminated strings.
   
   s_ap = SUMA_append_replace_string(s1, s2, spc, whichTofree);
  
  \param s1 (char *) string 1
  \param s2 (char *) string 2
  \param spc (char *) spacing string
  \param whichTofree (int) 0 free none, 
                           1 free s1
                           2 free s2
                           3 free s1 and s2
   \return s_ap (char *) a string formed by "%s%s%s", s1, spc, s2
   
   - s1 and s2 are copied into a new string with spc in between
   - s1 (but not s2 or spc ) IS FREED inside this function
   -free returned pointer with:  if(s_ap) SUMA_free(s_ap);
   
   \sa SUMA_append_string, SUMA_ar_string, SUMA_append_replace_string_eng
*/
char * SUMA_append_replace_string(char *s1, char *s2, char *Spc, int whichTofree)
{
   return(SUMA_append_replace_string_eng(s1, s2, Spc, whichTofree, 0));
}
char * SUMA_ar_string(char *s1, char *s2, char *Spc, int whichTofree)
{
   return(SUMA_append_replace_string_eng(s1, s2, Spc, whichTofree, 1));
}
char * SUMA_append_replace_string_eng(char *s1, char *s2, char *Spc, 
                                      int whichTofree, int cleanstart)
{
   static char FuncName[]={"SUMA_append_replace_string_eng"};
   char *atr = NULL;
   int i,cnt, N_s2, N_s1, N_Spc=0;

   
   SUMA_ENTRY;
   
   if (!s1 && !s2) SUMA_RETURN(NULL);
   
   if (!s1) N_s1 = 0;
   else N_s1 = strlen(s1);
   
   if (!s2) N_s2 = 0;
   else N_s2 = strlen(s2);
   
   if (!Spc) N_Spc = 0;
   else N_Spc = strlen(Spc);
   
   atr = (char *) SUMA_calloc(N_s1+N_s2+N_Spc+2, sizeof(char));
   
   /* copy first string */
   i=0;
   cnt = 0;
   if (s1) {
      while (s1[i]) {
         atr[cnt] = s1[i];
         ++i;
         ++cnt;
      }
   }
     
   i=0;
   if (Spc && (N_s1 || !cleanstart)) {
      while (Spc[i]) {
         atr[cnt] = Spc[i];
         ++i;
         ++cnt;
      }
   }
   
   i=0;
   if (s2) {
      while (s2[i]) {   
         atr[cnt] = s2[i];
         ++i;
         ++cnt;
      }
   }
   atr[cnt] = '\0';
   
   switch (whichTofree) {
      case 0:
         break;
      case 1:
         if (s1) free(s1);
         break;
      case 2:
         if (s2) free(s2);
         break;
      case 3:
         if (s1) free(s1);
         if (s2) free(s2);
         break;
      default: 
         fprintf(stderr, "Error %s:\nBad freeing parameter\n"
                         "No variables were freed.\n",
                         FuncName);
         break;
   }  

   SUMA_RETURN(atr);  
}   

char * SUMA_replace_string(char *s1, char *s2)
{   
   if (s1) SUMA_free(s1);
   return(SUMA_copy_string(s2));
}

char * SUMA_append_replace_num(char *s1, char *form, double num, 
                               SUMA_VARTYPE tp, int whichTofree)
{
   static char FuncName[]={"SUMA_append_replace_num"};
   char *atr = NULL, sbuf[500];
   int i,cnt, N_s2, N_s1, N_Spc=0;

   
   SUMA_ENTRY;
   
   if (!form) SUMA_RETURN(NULL);
   if (!s1 && !form) SUMA_RETURN(NULL);
   if (whichTofree > 1) {
      SUMA_S_Err("Can only free s1");
      SUMA_RETURN(NULL);
   }
   if (!s1) N_s1 = 0;
   else N_s1 = strlen(s1);
   
   switch(tp) {
      case SUMA_short:
      case SUMA_int:
         snprintf(sbuf, 450, form, (int)num);
         break;
      case SUMA_float:
      case SUMA_double:
         snprintf(sbuf, 450, form, (double)num);
         break;
      default: 
         snprintf(sbuf, 450, "NUM_FORMAT_ERROR");
         break; 
   }  
         
   /* fprintf(SUMA_STDERR,"%s: Have %lf num, form:>%s<, sbuf>%s<\n", 
      FuncName, num, form, sbuf); */
      
   atr = SUMA_append_replace_string(s1, sbuf, "", whichTofree);
   

   SUMA_RETURN(atr);  
}    

/*!
   \brief Appends newstring to string in SS->s while taking care of resizing space allocated for s
   
   \param SS (SUMA_STRING *) pointer to string structure
   \param newstring (char *) pointer to string to add to SS
   \return SS (SUMA_STRING *) pointer to string structure 
                              with SS->s now containing newstring
   - When SS is null, 1000 characters are allocated for s (initialization) 
                                    and s[0] = '\0';
   - When newstring is NULL, space allocated for SS->s is resized to the 
     correct dimension and  a null character is placed at the end.
   \sa SUMA_SS2S
*/
SUMA_STRING * SUMA_StringAppend (SUMA_STRING *SS, char *newstring)
{
   static char FuncName[]={"SUMA_StringAppend"};
   int N_inc = 0, N_cur = 0;
   int N_chunk = 1000;
   int i=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SS) {
      if (LocalHead) fprintf (SUMA_STDERR, "%s: Allocating for SS.\n", FuncName);
      SS = (SUMA_STRING *) SUMA_malloc (sizeof(SUMA_STRING));
      SS->s = (char *) SUMA_calloc (N_chunk, sizeof(char));
      SS->s[0] = '\0';
      SS->N_alloc = N_chunk;
      SUMA_RETURN (SS);
   }
   
   if (newstring) {
      if (LocalHead) 
         fprintf (SUMA_STDERR, "%s: Appending to SS->s.\n", FuncName);
      N_inc = strlen (newstring);
      N_cur = strlen (SS->s);
      if (SS->N_alloc < N_cur+N_inc+1) { /* must reallocate */
         if (LocalHead) 
            fprintf (SUMA_STDERR, "%s: Must reallocate for SS->s.\n", FuncName);
         SS->N_alloc = N_cur+N_inc+N_chunk+1;
         SS->s = (char *)SUMA_realloc (SS->s, sizeof(char)*SS->N_alloc);
         if (!SS->s) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to reallocate for s.\n", FuncName);
            SUMA_RETURN (NULL);
         }
      }
      /* append */
      for (i=N_cur;i<N_cur+N_inc; ++i)
         SS->s[i] = newstring[i-N_cur];
      SS->s[N_cur+N_inc] = '\0';   
   }else {
      /* shrink SS->s to small size */
      N_cur = strlen (SS->s);
      if (SS->N_alloc > N_cur+1) {
         if (LocalHead) 
            fprintf (SUMA_STDERR, "%s: Shrink realloc for SS->s.\n", FuncName);
         SS->N_alloc = N_cur+1;
         SS->s = (char *)SUMA_realloc (SS->s, sizeof(char)*SS->N_alloc);
         if (!SS->s) {
            fprintf (SUMA_STDERR, 
                     "Error %s: Failed to reallocate for s.\n", FuncName);
            SUMA_RETURN (NULL);
         }
         /*put a null at the end */
         SS->s[SS->N_alloc-1] = '\0';
      }
   }
   
   SUMA_RETURN (SS);

}

/*!
   \brief Appends newstring to string in SS->s while taking care of resizing space allocated for s
   A variable argument version of SUMA_StringAppend
   
   \param SS (SUMA_STRING *) pointer to string structure
   \param newstring (char *) pointer to string to add to SS
   \param ..... the remaining parameters a la printf manner
   \return SS (SUMA_STRING *) pointer to string structure with SS->s now containing newstring
   - When SS is null, 1000 characters are allocated for s (initialization) and s[0] = '\0';
   - When newstring is NULL, space allocated for SS->s is resized to the correct dimension and 
   a null character is placed at the end.
   
   - For this function, the formatted length of newstring should not be > than MAX_APPEND-1 
   If that occurs, the string will be trunctated and no one should get hurt
   
   NOTE: DO NOT SEND NULL pointers in the variable argument parts or crashes will occur on SUN
   Such NULL pointers do not result in null vararg_ptr and cause a seg fault in vsnprintf
   
   \sa SUMA_StringAppend
   \sa SUMA_SS2S
*/

#define MAX_APPEND 30000


SUMA_STRING * SUMA_StringAppend_va (SUMA_STRING *SS, char *newstring, ... )
{
   static char FuncName[]={"SUMA_StringAppend_va"};
   char sbuf[MAX_APPEND+2];
   int nout;
   va_list vararg_ptr ;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!SS) {
      SUMA_LH("NULL SS");
      /* let the other one handle this */
      SUMA_RETURN (SUMA_StringAppend(SS,newstring));
   }
   
   if (newstring) {
      SUMA_LH("newstring %s...", newstring);
      /* form the newstring and send it to the olde SUMA_StringAppend */
      va_start( vararg_ptr ,  newstring) ;
      if (strlen(newstring) >= MAX_APPEND -1 ) {
         SUMA_SL_Err("newstring too long.\nCannot use SUMA_StringAppend_va");
         SUMA_RETURN(SUMA_StringAppend(SS,"Error SUMA_StringAppend_va: "
                                          "***string too long to add ***"));
      }
      if (LocalHead) {
         SUMA_LH("Calling vsnprintf");
         if (vararg_ptr) {
            SUMA_LH("Non NULL vararg_ptr");
         } else {
            SUMA_LH("NULL vararg_ptr");
         }
      }
      nout = vsnprintf (sbuf, MAX_APPEND * sizeof(char), newstring, vararg_ptr); 
      if (LocalHead) 
         fprintf(SUMA_STDERR,"%s:\n Calling va_end, nout = %d\n", 
                  FuncName, nout);
      va_end(vararg_ptr);  /* cleanup */
      
      if (nout < 0) {
         SUMA_SL_Err("Error reported by  vsnprintf");
         SUMA_RETURN(SUMA_StringAppend(SS,
                                       "Error SUMA_StringAppend_va:"
                                       " ***Error reported by  vsnprintf"));
      }
      if (nout >= MAX_APPEND) {
         SUMA_LH("String trunctated by vsnprintf");
         SUMA_StringAppend(SS,sbuf);
         SUMA_RETURN(SUMA_StringAppend(SS,
                                       "\nWARNING: "
                                       "***Previous string trunctated because "
                                       "of its length. ***\n"));
      }
      SUMA_LH("Calling StringAppend on %s", sbuf);
      SUMA_RETURN (SUMA_StringAppend(SS,sbuf));
   }else {
      SUMA_LH("NULL newstring");
      /* let the other one handle this */
      SUMA_RETURN (SUMA_StringAppend(SS,newstring));
   }
   
   /* should not be here */
   SUMA_RETURN (NULL);

}

/* ***************** Environment value access begin **********************/
static ENV_SPEC envlist[] = {
   {  "Incremental arrow rotation angle in degrees",
      "SUMA_ArrowRotAngle",
      "5" } ,
   {  "Color pattern (AFNI, EURO, PRINT, DEFAULT)",
      "SUMA_ColorPattern",
      "EURO" },
   {  "Swap mouse buttons 1 and 3",
      "SUMA_SwapButtons_1_3",
      "NO" },
   {  "Background color r g b. No space between values",
      "SUMA_BackgroundColor",
      "0.0,0.0,0.0" },
   {  "ROI color map (bgyr64, roi64, roi128, roi256)",
      "SUMA_ROIColorMap",
      "ROI_i256" },
   {  "Number of smoothing operations to run on convexity data",
      "SUMA_NumConvSmooth",
      "5" },
   {  "Colormap for convexity (gray02, gray_i02, ngray20, bgyr64, etc.)",
      "SUMA_ConvColorMap",
      "gray02" },
   {  "Brightness factor for convexity ",
      "SUMA_ConvBrightFactor",
      "0.5" },
   {  "Number of smoothing operations to run on mixed foregroung color plane\n"
      " before mixing with background",
      "SUMA_NumForeSmoothing",
      "0" },
   {  "Number of smoothing operations to run on final set of mixed colors.\n"
      " This would be the mixed foreground and background colors",
      "SUMA_NumFinalSmoothing",
      "0" },
   {  "Setup the color mixing mode (ORIG, MOD1) ",
      "SUMA_ColorMixingMode",
      "ORIG" },
   {  "** OBSOLETE: Port for communicating with AFNI\n"
      "              Listening ports are derived from SUMA_AFNI_TCP_PORT\n"
      "              Listening port i\n"
      "              SUMA_AFNI_TCP_PORT + i (i > 0)",
      "SUMA_AFNI_TCP_PORT",
      "0" /* used to be 53211 */},
   {  "Warn before closing with the Escape key (YES/NO)",
      "SUMA_WarnBeforeClose",
      "YES" },
   {  "Mask node values\n"
      " 0 ? YES/NO",
      "SUMA_MaskZero",
      "YES" },
   {  "Threshold if Val < thr (NO) or | Val | < | Thr | (YES)",
      "SUMA_AbsThreshold",
      "YES" },
   {  "Threshold scale precision. 2 is the minimum allowed. \n"
      " This value might be overriden in SUMA.",
      "SUMA_ThresholdScalePower",
      "2" },
   {  "Center of Rotation is based on nodes used in the mesh and not \n"
      " on all the nodes in NodeList",
      "SUMA_CenterOnPatch",
      "NO" },
   {  "Use cross ticks on axis ?",
      "SUMA_UseCrossTicks",
      "NO" },
   {  "Warn if 1D file looks like it needs a transpose",
      "SUMA_1D_Transpose_Warn",
      "YES" },
   {  "Adjust roation and translation factor of mouse with changes \n"
      " in zoom levels ",
      "SUMA_AdjustMouseMotionWithZoom",
      "YES" },
   {  "Use orthographic projection ",
      "SUMA_ViewOrthographicProjection",
      "NO" },
   {  "Percent gain for zooming in and out with the 'z' and 'Z' keys. \n"
      " Typical range from 0 to 50",
      "SUMA_KeyZoomGain",
      "5" },
   {  "Original FOV. Set between 1.0 and 100.0 \n"
      " Default is 30.0, -1 == auto",
      "SUMA_FOV_Original",
      "-1" },
   {  "Original windows size and width in pixels \n"
      " :SPX:Allowed values are:\n\n"
      "    'TopLeft'\n\n"
      "    'RightOffset'\n\n"
      "    'X Y' Sets only the position to top left corner\n\n"
      "    'X Y Xwidth Ywidth' Set also width of window\n\n"
      " :DEF:Allowed values are: 'TopLeft'\n"
      "                     'RightOffset' \n"
      "                     'X Y' Sets only the position to top left corner\n"
      "                     'X Y Xwidth Ywidth' Set also width of window\n"
      " :SPX:",
      "SUMA_Position_Original",
      "TopLeft" },
   {  "light0 color",
      "SUMA_Light0Color",
      "1.0,1.0,1.0" },
   {  "Ambient light ",
      "SUMA_AmbientLight",
      "1.0,1.0,1.0" },
   {  "Allow for replacement of pre-loaded dsets",
      "SUMA_AllowDsetReplacement",
      "YES" },
   {  "Allow a dataset to be assigned to a surface, even if\n"
      "domain of dset is specified and different for the surface.\n",
      "SUMA_AlwaysAssignSurface",
      "YES" },
   {  "Allow for surfaces with same DomainGrandParentID to share overlays",
      "SUMA_ShareGrandChildrenOverlays",
      "NO" },
   {  "Increase the resolution of images recorded with 'r' button.\n"
      " Increase is done by taking multiple shots that once stitched  \n"
      " together form a high-resolution image.\n"
      " The maximum resolution is set by the GL_MAX_VIEWPORT_DIMS of your\n" 
      " graphics card. I have 4096 pixels.\n"
      " If you exceed this number, SUMA will make adjustments automatically.\n"
      " Assemble images with program imcat.",
      "SUMA_SnapshotOverSampling",
      "1" },
   {  "Ignore consecutive duplicate images in recorder",
      "SUMA_NoDuplicatesInRecorder",
      "YES" },
   {  "start NIML (can't do this for more than one suma at a time!)",
      "SUMA_START_NIML",
      "YES" },
   {  "Allow (YES) datasets with the same filename but differing ID \n"
      " to be considered the same.\n"
      " This is only useful with SUMA_AllowDsetReplacement",
      "SUMA_AllowFilenameDsetMatch",
      "YES" },
   {  "Freeze zoom across states",
      "SUMA_FreezeFOVAcrossStates",
      "NO" },
   {  "Dset color map",
      "SUMA_DsetColorMap",
      "Spectrum:red_to_blue" },
   {  "Show only selected dset in suma's surface controller.",
      "SUMA_ShowOneOnly",
      "YES" },
   {  "Update graphs, even SUMA_ShowOneOnly (or suma's '1 Only') is turned on.",
      "SUMA_GraphHidden",
      "YES" },
   {  "Fraction of colormap to rotate with up/down arrow keys.",
      "SUMA_ColorMapRotationFraction",
      "0.05"},
   {  "Size of surface controller font. \n"
      " Values are SMALL, BIG (old style).",
      "SUMA_SurfContFontSize",
      "SMALL"},
   {  "Where to position SUMA window when first opened.\n"
      " Values are POINTER (at the mouse pointer's location)\n"
      "            DEFAULT (let the window manager decide)\n",
      "SUMA_StartUpLocation",
      "DEFAULT"},
   {  "Numer of nodes to jump with the 'alt+arrow' keys. \n"
      " Valid range from 1 to 10",
      "SUMA_KeyNodeJump",
      "1" },
   {  "Numer of seconds to wait for SUMA to respond to DriveSuma. \n"
      " Valid range from 0 to 60000, see also env SUMA_DriveSumaMaxCloseWait",
      "SUMA_DriveSumaMaxWait",
      "300.0" },
   {  "String to use in creating left hemisphere dataset wildcards.",
      "SUMA_LEFT_FILE_DSET_IDENTIFIER",
      "*lh*.dset" },
   {  "String to use in creating left hemisphere dataset wildcards.",
      "SUMA_RIGHT_FILE_DSET_IDENTIFIER",
      "*rh*.dset" },
   {  "String to use in creating left hemisphere roi wildcards.",
      "SUMA_LEFT_FILE_ROI_IDENTIFIER",
      "*lh*.roi" },
   {  "String to use in creating right hemisphere roi wildcards.",
      "SUMA_RIGHT_FILE_ROI_IDENTIFIER",
      "*rh*.roi" },
   {  "String to use in creating left hemisphere roi wildcards.",
      "SUMA_LEFT_FILE_OTHER_IDENTIFIER",
      "*lh*" },
   {  "String to use in creating right hemisphere roi wildcards.",
      "SUMA_RIGHT_FILE_OTHER_IDENTIFIER",
      "*rh*" },
   {  "Initial Convexity Datasest opacity.",
      "SUMA_ConvexityDsetOpacity",
      "0.85" },
   {  "Display mode of  Label Datasest specified in spec file at startup.\n"
      ":SPX:"
      "\n"
      "  'YES' or 'Col': Shows it in color\n\n"
      "  'Con': Shows only contours (see also env SUMA_ContourThickness).\n\n"
      "  'C&C': Shows both colors and contours \n\n"
      "  'XXX'or 'No': Does not show it.\n\n"
      ":DEF:"
      "  'YES' or 'Col': Shows it in color\n"
      "  'Con': Shows only contours (see also env SUMA_ContourThickness).\n"
      "  'C&C': Shows both colors and contours \n"
      "  'XXX'or 'No': Does not show it.\n"
      ":SPX:",
      "SUMA_ShowLabelDsetAtStartup",
      "XXX" },
   {  "Show label at cross hair in viewer\n"
      "You can toggle the display at such labels with F9\n",
      "SUMA_ShowLabelsAtCrossHair",
      "YES" },
   {  "Initial Label Datasest opacity.",
      "SUMA_LabelDsetOpacity",
      "0.2" },
   {  "Attempt to recover from AFNI <--> SUMA disconnection bug.\n",
      "SUMA_AttemptTalkRecover",
      "Yes" }, 
   {  "Name of directory containing user's own SUMA color maps"
      " (:SPX:`*`:DEF:*:SPX:.cmap)\n",
      "SUMA_CmapsDir",
      "None" },
   {  "Name of color map for datasets of retinotopy angles.\n"
      "These would be produced by 3dRetinoPhase\n",
      "SUMA_RetinoAngle_DsetColorMap",
      "rgybr20" },
   {  "Name of color map for VFR datasets produced by SurfRetinoMap\n",
      "SUMA_VFR_DsetColorMap",
      "afni_n2" },
   {  "Coordinate units of surface nodes. Choose from 'mm' or 'cm'\n"
      "A bad choice can make the surfaces render with many artifacts.\n",
      "SUMA_NodeCoordsUnits",
      "mm" }, 
   {  "Which anatomically correct surf. states should not NOT be sent to AFNI?\n"
      "This is mostly for deciding whether one of 'white' or 'smoothwm'\n"
      "FreeSurfer states should not be sent to AFNI.\n"
      "The default is to let them all go.\n"
      "You can specify multiple states with a , delimited list (no spaces!). \n"
      "By default nothing is excluded.\n",
      "SUMA_DoNotSendStates",
      "N/A" },
   {  "Prefix for autorecord (suma's Ctrl+R) files. \n"
      "FreeSurfer states should not be sent to AFNI.\n"
      "Add a path if you want the files to endup in a particular directory.\n"
      "You can also add an extension to prefix to specify the output type.\n"
      "Choose from .jpg, .ppm, or .1D . The fallback type is .jpg\n",
      "SUMA_AutoRecordPrefix",
      "./SUMA_Recordings/autorecord.jpg" }, 
   {  "Font for cross hair label in SUMA viewer\n"
      "Choose one of: f8 f9 tr10 tr24 he10 he12 he18\n",
      "SUMA_CrossHairLabelFont",
      "f9" }, 
   {  "Linking mode of I and T sub-brick selectors\n"
      "Choose one of: None, Same, Stat\n",
      "SUMA_IxT_LinkMode",
      "Stat" }, 
   {  "Minimum Number of sub-bricks to trigger use of arrow field for \n"
      "sub-brick selectors.\n",
      "SUMA_ArrowFieldSelectorTrigger",
      "200" }, 
   {  "Use symmetric Intensity range at startup? Valid options are:\n"
      " YES or NO: For your preference if no decision is made by the software\n"
      " FYES or FNO: To force your preference and keep software from deciding\n",
      "SUMA_Sym_I_Range",
      "YES" }, 
   {  "Set auto Intensity range by default (YES or NO)\n",
      "SUMA_Auto_I_Range",
      "NO" }, 
   {  "Set auto Brightness range by default (YES or NO)\n",
      "SUMA_Auto_B_Range",
      "NO" }, 
   {  "Set thickness of dataset contours\n",
      "SUMA_ContourThickness",
      "1.0" }, 
   {  "Merge separated left/right states for inflated/spherical/etc. surfaces\n"
      "Choose from YES or NO",
      "SUMA_LHunify",
      "YES" }, 
   {  "Put surface controllers in same window\n"
      "Choose from YES or NO",
      "SUMA_SameSurfCont",
      "YES" },
   {  "Adjust offset of Surface Viewers as they are first open\n"
      "Choose from AUTO or provide two X Y offsets.",
      "SUMA_WindowOffset",
      "Auto" },
   {  "Lock views across viewers\n"
      "Choose from YES or NO.",
      "SUMA_LockViewers",
      "YES" },
   {  "Set colormap for volumes, choose any of the standard list",
      "SUMA_VO_ColorMap",
      "bw20" },
   {  "Force reorienting of read volume.\nTo force reorientation,\n"
      "Choose from RAI, LPI, RAS. etc...\n"
      "Use NO to avoid reorientation. This env. is for debugging purposes.\n",
      "SUMA_VO_Reorient",
      "NO" },
   {  "Set maximum waiting time for proper detection of closed stream\n"
      "This is to avoid DriveSuma's: Failed to detect closed stream ...\n"
      "complaint which results in a forced stream closing. Time unit is\n"
      "in seconds. See also env SUMA_DriveSumaMaxWait\n",
      "SUMA_DriveSumaMaxCloseWait",
      "5" },
   {  "Set order in which object types are rendered. This order will affect\n"
      "the resultant image in the few instances where alpha transparency is\n"
      "used. The order can be specified for only three types of objects for \n"
      "now: graphs, surfaces, and volumes. If you want to render graphs first,\n"
      "followed by volumes then surfaces then set SUMA_ObjectDisplayOrder to\n"
      "something like: 'graph,vol,surf'. Do not include spaces between the\n"
      "type names.",
      "SUMA_ObjectDisplayOrder",
      "vol,surf,graph" },
   {  "Font for datasets in SUMA viewer\n"
      "Choose one of: f8 f9 tr10 tr24 he10 he12 he18\n",
      "SUMA_Dset_Font",
      "f9" }, 
   {  "Method for representing connections to a certain node in a graph"
      " dataset.\n"
      "Choose one of: Edge, Color, Radius, C&R, XXX\n",
      "SUMA_Dset_NodeConnections",
      "Edge" },
   {  "Set which slices should be shown when a volume is first loaded.\n"
      "You can set parameters for each of the Ax, Sa, and Co planes, and\n"
      "the volume rendering.\n"
      "Each plane gets its own string formatted as such: PL:SL:MON:INC\n"
      "where:\n"
      ":SPX:\n"
      "   PL is the plane (Ax, Co, Sa, or Vr)\n\n"
      "   SL is the slice number, you can also set the number as \n"
      "     a fraction of the number of slices in the volume.\n\n"
      "   MON is the number of montage slices\n\n"
      "   INC is the increment between montage slices. You can use \n"
      "       fractions for this parameter also.\n\n"
      ":DEF:"
      "      PL is the plane (Ax, Co, Sa, or Vr)\n"
      "      SL is the slice number, you can also set the number as \n"
      "         a fraction of the number of slices in the volume.\n"
      "      MON is the number of montage slices\n"
      "      INC is the increment between montage slices. You can use \n"
      "          fractions for this parameter also.\n"
      ":SPX:"
      "If you want to set parameters for a certain plane, but do not\n"
      "want to see it, prepend the plane name with 'h' (for hide) as in 'hAx'\n"
      "Note that for Vr, there are no SL, MON, and INC qualifiers\n"
      "Also, SUMA will force the display of at least one plane because\n"
      "otherwise you have no way of opening a volume controller\n"
      "Example: 'Ax:0.5:3:10,Co:123:2:50,Vr'",
      "SUMA_VO_InitSlices",
      "Ax:0.5,Sa:0.5:2:0.5,hCo:0.5" },
   {  "Allow selection of voxels on 3D rendering.\n"
      "Choose one of: YES or NO\n",
      "SUMA_VrSelectable",
      "YES"  },
   {  "Perform 'Home' call in SUMA after each prying.\n"
      "If YES, objects are repositioned to stay in the middle of the viewer\n"
      "as you pry the surfaces apart. This behavior is desired in general, \n"
      "unless you don't like the initial positioning in the first place.\n"
      "Choose from YES or NO",
      "SUMA_HomeAfterPrying",
      "YES" },
   {  "Assume surface in TESSCON units if range is extreme\n"
      "If YES, surfaces with a big difference between max and min dims are\n"
      "scaled by 319.7. Don't set this env to YES unless this jibber jabber \n"
      "means.\n"
      "Choose from YES or NO",
      "SUMA_SUMA_TESSCON_AutoScale",
      "NO" },
   {  "Turn on verbose mode for function count_procs() that checks for \n"
      "recursive calls to a program. Do not keep this env set to YES unless\n"
      "you are debugging.\n",
      "SUMA_CountProcs_Verb",
      "NO" },
   {  "Number of transparency levels to jump with each 'o' key press\n"
      "Choose one of 1, 2, 4, or 8\n",
      "SUMA_Transparency_Step",
      "4" },
   {  "If YES, then automatically load datasets with names matching those \n"
      "the surface just read.\n"
      "For example, if you load a surface named PATH/TOY.gii, for instance,\n"
      "and there exists a file called PATH/TOY.niml.dset then that file\n"
      "is automatically loaded onto surface TOY.gii. This would work for\n"
      "all surface types (e.g. TOY.ply) and dataset types (e.g. TOY.1D.dset)\n"
      "Choose from YES or NO\n",
      "SUMA_AutoLoad_Matching_Dset",
      "YES" },
   {  "Colorize labeled datasets without attempting to make colors match\n"
      "what would be displayed in AFNI (YES or NO). Set to YES to match\n"
      "old style colorization preceding the addition of this variable\n",
      "SUMA_Classic_Label_Colors",
      "NO" }, 
   {  "Multiplier for range of thresholding scale.\n",
      "SUMA_Range_Multiplier",
      "1.0" },
   {  "Default p value to adopt when switching to a new sub-brick.\n"
      "Negative values mean leave the threshold alone when switching.\n",
      "SUMA_pval_at_switch",
      "-1.0" },
   {  "If YES, then reduce messages to only errors while driving suma\n"
      "Choose from YES or NO",
      "SUMA_DriveSumaQuiet",
      "NO" },
   {  "If YES, then show popup message windows in suma\n"
      "Choose from YES or NO",
      "SUMA_SHOWPOPUPS",
      "NO" },

   {  NULL, NULL, NULL  }
};
      
ENV_SPEC SUMA_envlistelement(int i) {
   ENV_SPEC se = envlist[i];
   return(se);
}

char * SUMA_EnvVal(char *env)
{
   static char FuncName[]={"SUMA_EnvVal"};
   char *eee=NULL;
   int i=0;
   
   SUMA_ENTRY;
   
   if (!env) SUMA_RETURN(NULL);
   if ((eee = getenv(env))) { SUMA_RETURN(eee); }
   
   /* search defaults*/
   i = 0;
   while (envlist[i].envhelp) {
      if ( envlist[i].envname &&
          !strcmp(envlist[i].envname, env) ) {
         SUMA_RETURN(envlist[i].envval);
      }
      ++i;
   }
   SUMA_RETURN(NULL);
}

/* Returns non 0 if the env variable matches sval
   
   The Function can handle an env that returns 
   some character separated list if sep is not NULL
   For example, say env = {"The, olde, fox"}
   SUMA_EnvEquals(env,"olde", 0,NULL) returns 0
   but 
   SUMA_EnvEquals(env,"olde", 0,",") returns 2
   because olde in the second word in env
*/
int SUMA_EnvEquals(char *env, char *sval, byte ci, char *sep)
{
   static char FuncName[]={"SUMA_EnvEquals"};
   char *eee=NULL;
   NI_str_array *sar=NULL;
   int i=0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!env) SUMA_RETURN(0);
   
   if (!(eee = getenv(env))) { 
      /* search defaults*/
      i = 0;
      while (envlist[i].envhelp && !eee) {
         if ( envlist[i].envname &&
             !strcmp(envlist[i].envname, env) ) {
            eee = envlist[i].envval;
         }
         ++i;
      }
   }
   
   if (eee==NULL) {
      if (sval==NULL) SUMA_RETURN(1);
      else SUMA_RETURN(0);
   } 
   
   /* have env value of some sort */
   if (sval == NULL) SUMA_RETURN(0);
   if (LocalHead) 
      fprintf(SUMA_STDERR,
           "%s: eee %s, sval %s, sep %s\n", FuncName, eee, sval, sep?sep:"NULL");
   if (!sep) {
      if (ci) SUMA_RETURN(!(strcasecmp(eee,sval)));
      else SUMA_RETURN(!(strcmp(eee,sval)));
   }
   
   /* need to breakup into subsets */
   if (!(sar = SUMA_NI_decode_string_list( eee , sep ))) SUMA_RETURN(0);
   if (LocalHead) 
      fprintf(SUMA_STDERR,
              "%s: Have %d vals in %s\n", FuncName, sar->num, eee);
   for (i=0; i<sar->num; ++i) {
         if (LocalHead) 
      fprintf(SUMA_STDERR,
              "%s: Comapring %s to %s\n", FuncName, sval, sar->str[i]);
      if ( (ci && !(strcasecmp(sval,sar->str[i]))) ||
           !strcmp(sval, sar->str[i]) ) {
         sar = SUMA_free_NI_str_array(sar);
         SUMA_RETURN(i+1);  
      }
   }
   sar = SUMA_free_NI_str_array(sar);
   SUMA_RETURN(0);
}


char * SUMA_env_list_help(int DEFAULT_values, TFORM targ){
   static char FuncName[]={"SUMA_env_list_help"};
   int i=0;
   char *sli=NULL;
   SUMA_STRING *SS=NULL;
   char *s=NULL, *eee=NULL, *userval=NULL;
   ENV_SPEC se;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend(NULL, NULL);
   
   se = SUMA_envlistelement(i);
   while (se.envhelp) {
      if (!DEFAULT_values) {
         /* find the user's setting */
         eee = getenv(se.envname);
      }
      if (userval) 
         SUMA_free(userval); 
      userval=NULL;
      if (!eee) userval = SUMA_copy_string(se.envval);
      else userval = SUMA_copy_string(eee);
      switch (targ) {
         default:
         case TXT: /* default */
            sli = SUMA_ReplaceChars(se.envhelp, "\n","\n//      ");
            sli = SUMA_Sphinx_String_Edit(&sli, targ, 0);
            SS = SUMA_StringAppend_va(SS,
                           "// %03d-%s:\n"
                           "//     %s\n"
                           "//     default:   %s = %s\n"
                           "   %s = %s\n",
                           i, se.envname,
                           sli,
                           se.envname,
                           se.envval,
                           se.envname,
                           userval);
            SUMA_free(sli); sli = NULL;
            break;
         case ASPX:
         case SPX: /* Sphinxy */
            sli = SUMA_copy_string(se.envhelp);
            sli = SUMA_Sphinx_String_Edit(&sli, targ, 0);
            SS = SUMA_StringAppend_va(SS,
                           ".. _%s:\n\n"
                           ":ref:`%s (env)<%s>`: %s\n\n"
                           "  default value:   %s = %s\n\n",
                           se.envname,
                           se.envname, se.envname, sli,
                           se.envname, se.envval);
            SUMA_free(sli); sli = NULL;
            break;
      }
      ++i;
      se = SUMA_envlistelement(i);
   }
   SUMA_SS2S(SS,s);
   
   SUMA_RETURN(s);
}



/*!
   If !env return NULL
   if !sval and env is an environment variable, its value is returned
   if sval and env then env's value is compared (case insensitive) and 
       NULL is returned if there is no match, else the environment 
       variable value is returned
       Partial match is allowed
   DO NOT free the returned pointer
*/
char *SUMA_isEnv(char *env, char *sval) {
   static char FuncName[]={"SUMA_isEnv"};
   char *eee = NULL, svalv[256]={""}, eeev[256]={""};
   int i=0;
   
   SUMA_ENTRY;
   
   if (!env) SUMA_RETURN(NULL);
   
   eee = SUMA_EnvVal(env);
   
   if (!sval || !eee) SUMA_RETURN(eee);
   /* have eee and sval, compare them */
   strncpy(svalv,sval, 255);
   strncpy(eeev, eee, 255);
   SUMA_TO_LOWER(eeev); SUMA_TO_LOWER(svalv);
   
   if (!strlen(eee)) {
      /* set but no value. Does user seek "" ?*/
      if (!strlen(sval)) SUMA_RETURN(eee);
      else SUMA_RETURN(NULL);
   } 
   for (i=0; i<strlen(svalv) && i<strlen(eeev); ++i) 
      if (svalv[i] != eeev[i]) SUMA_RETURN(NULL);

   SUMA_RETURN(eee);   
}

float SUMA_floatEnv(char *env, float defval)
{
   static char FuncName[]={"SUMA_floatEnv"};
   float fv = defval;
   char *eee=NULL, *eend=NULL;
   SUMA_ENTRY;
   
   if ((eee = SUMA_EnvVal(env)))  {
      fv = (float)strtod(eee, &eend);
      if (eee == eend) { /* failed */
         fv = defval;
      }
   }
   
   SUMA_RETURN(fv);
}

/* ***************** Environment value access end **********************/

/*! ********** Searching/Sorting Functions ************* */

/* Find the array index k where NodeIndex[n] == k 
Function assumes NodeIndex is monotonic ascending */
int SUMA_NodeIndex_To_Index(int *NodeIndex, int N_Node, int n)
{
   static char FuncName[]={"SUMA_NodeIndex_To_Index"};
   if (!NodeIndex || n < 0) return(n);
   if (n < N_Node && NodeIndex[n]==n) return(n);
   else { /* have to search */
      return(SUMA_ibinFind(NodeIndex, N_Node, n));
   }
}

/*!
  SUMA_binSearch( nodeList, target, seg, ematch);

  This function performs a binary search.  The indices of the elements in nodeList surrounding target will be stored in (overwrite) seg; thus seg[0]=seg[1]=i implies that an exact match was found at index i.
  \param nodeList (float *) vector of sorted values
  \param target (float) value seeking
  \param seg (int *) contains begin and end point of segment being searched
  \param ematch (byte) 1: Exact match enforced. 0: Closest
  \return found (SUMA_Boolean) YUP if all passed correctly and target within segment, NOPE otherwise

  Written by Brenna Argall
*/
SUMA_Boolean SUMA_binSearch( float *nodeList, float target, int *seg, 
                             byte ematch) 
{
   static char FuncName[]={"SUMA_binSearch"};
   int mid=0;
   int beg = seg[0], end = seg[1];
   SUMA_Boolean found=YUP;
   SUMA_Boolean LocalHead = NOPE;
   
      
   SUMA_LHv("%f < %f < %f\n", nodeList[beg], target, nodeList[end]);
   if ( end<beg) {
      SUMA_S_Errv("Segment must be passed with seg[0]=%d <= seg[1]=%d.\n",
                  seg[0], seg[1]);
      return (found = NOPE);
   }
   if ( nodeList[end]<nodeList[beg] ) {
      SUMA_S_Errv("Nodelist must be passed sorted and in ascending order.\n"
                  "nodeList[%d]=%f<nodeList[%d]=%f\n",
                  end, nodeList[end], beg, nodeList[beg]);
      return (found = NOPE);
   }
   if ( (nodeList[beg]>target) || (nodeList[end]<target) ) {
      SUMA_LHv("Don't bother, target (%f) does not lie within segment ]%f, %f[\n"
                  , target, nodeList[beg], nodeList[end]);
      return (found = NOPE);
   }

   if (beg!=end) {
      mid =(end-beg)/2 + beg;
      /**no exact match, but elements above and below found*/
      if (beg+1==end) {
         if (nodeList[end]==target) {
            seg[0] = end;
            seg[1] = end;
         } else if (nodeList[beg]==target) {
            seg[0] = beg;
            seg[1] = beg;
         } else { 
            if (!ematch) {
               seg[0] = beg;
               seg[1] = end;
            } else {
               return(found = NOPE);
            }
         }
      }
      else if (target==nodeList[mid]) {
         seg[0] = mid;
         seg[1] = mid;
      }
      /**keep searching*/
      else if ( target  < nodeList[mid]) {
         seg[0] = beg;  seg[1] = mid;
         found = SUMA_binSearch( nodeList, target, seg, ematch);
      }
      else if ( target > nodeList[mid]) {
         seg[0] = mid;  seg[1] = end;
         found = SUMA_binSearch( nodeList, target, seg, ematch);
      }
   }
   /**exact match; beg==end or target==nodeList[ indexList[mid] ]*/
   else {
      seg[0] = mid;
      seg[1] = mid;
   }
  
   return(found);
}

int SUMA_binFind( float *indexList, int N_node, float target, byte ematch) {
   int seg[2]={0, N_node -1};
   if (SUMA_binSearch(indexList, target, seg, ematch)) return(seg[0]);
   else return(-1);
}

/*!
  SUMA_ibinSearch( nodeList, target, seg);

  This function performs a binary search.  See SUMA_binSearch for comments
  \param nodeList (float *) vector of sorted values
  \param target (float) value seeking
  \param seg (int *) contains begin and end point of segment being searched
  \return found (SUMA_Boolean) YUP if all passed correctly and target within segment, NOPE otherwise

*/

SUMA_Boolean SUMA_ibinSearch( int *indexList, int target, int *seg) 
{
   static char FuncName[]={"SUMA_ibinSearch"};
   int mid=0;
   int beg = seg[0], end = seg[1];
   SUMA_Boolean found=YUP;
   SUMA_Boolean LocalHead = NOPE;
   
   if ( end<beg) {
      SUMA_S_Errv("Segment must be passed with seg[0]=%d <= seg[1]=%d.\n",
                  seg[0], seg[1]);
      return (found = NOPE);
   }
   if  (indexList[end]<indexList[beg] ) {
      SUMA_S_Errv("indexList must be passed sorted and in ascending order.\n"
                  "indexList[%d]=%d<indexList[%d]=%d\n",
                  end, indexList[end], beg, indexList[beg]);
      return (found = NOPE);
   }
   if ( (indexList[beg]>target) || (indexList[end]<target) ) {
      SUMA_LHv("Don't bother, target (%d) does not lie within segment ]%d, %d[\n"
                  , target, indexList[beg], indexList[end]);
      return (found = NOPE);
   }

   if (beg!=end) {
      mid =(end-beg)/2 + beg;
      /**no exact match, but elements above and below found*/
      if (beg+1==end) {
         if (indexList[end]==target) {
            seg[0] = end;
            seg[1] = end;
         } else if (indexList[beg]==target) {
            seg[0] = beg;
            seg[1] = beg;
         } else {
            return (found = NOPE);
         }
      }
      else if (target==indexList[mid]) {
         seg[0] = mid;
         seg[1] = mid;
      }
      /**keep searching*/
      else if ( target  < indexList[mid]) {
         seg[0] = beg;  seg[1] = mid;
         found = SUMA_ibinSearch( indexList, target, seg);
      }
      else if ( target > indexList[mid]) {
         seg[0] = mid;  seg[1] = end;
         found = SUMA_ibinSearch( indexList, target, seg);
      }
   }
   /**exact match; beg==end or target==indexList[ indexList[mid] ]*/
   else {
      seg[0] = mid;
      seg[1] = mid;
   }
  
   return(found);
} 

int SUMA_ibinFind( int *indexList, int N_node, int target) {
   static char FuncName[]={"SUMA_ibinFind"};
   int seg[2]={0, N_node -1};
   if (SUMA_ibinSearch(indexList, target, seg)) return(seg[0]);
   else return(-1);
}

/*!
   \brief creates a reordered version of a vector 
   yr = SUMA_reorder(y, isort, N_isort);
   
   \param y (int *) vector, if y is NULL what 
                    you get back in yr is a copy
                    of isort. 
   \param isort (int *) vector containing sorting order
   \param N_isort (int ) number of elements in isort
   \return yr (int *) reordered version of y where:
                     yr[i] = y[isort[i]];
                     
   - you should free yr with SUMA_free(yr) when done with it
   - obviously it's your business to ensure that
            isort[i] cannot be larger than then number
            of elements in y 
*/
int *SUMA_reorder(int *y, int *isort, int N_isort)
{
   static char FuncName[]={"SUMA_reorder"};
   int i = 0, *yr = NULL;
   
   SUMA_ENTRY;
   
   if (!isort || N_isort <= 0) SUMA_RETURN(yr);
   
   yr = (int *)SUMA_calloc( N_isort, sizeof(int));
   if (!yr) SUMA_RETURN(yr);
   
   if (!y) for (i=0; i<N_isort; ++i) yr[i] = isort[i];
   else for (i=0; i<N_isort; ++i) yr[i] = y[isort[i]];
   
   SUMA_RETURN(yr);
}

/* Careful to only free returned pointer after 
calling function is done with it. Do not free
individual strings in returned pointer */
char **SUMA_sreorder(char **y, int *isort, int N_isort)
{
   static char FuncName[]={"SUMA_sreorder"};
   int i = 0;
   char **yr = NULL;
   
   SUMA_ENTRY;
   
   if (!y || !isort || N_isort <= 0) SUMA_RETURN(yr);
   
   yr = (char  **)SUMA_calloc( N_isort, sizeof(char*));
   if (!yr) SUMA_RETURN(yr);
   
   for (i=0; i<N_isort; ++i) yr[i] = y[isort[i]];
   
   SUMA_RETURN(yr);
}

byte *SUMA_breorder(byte *y, int *isort, int N_isort)
{
   static char FuncName[]={"SUMA_breorder"};
   int i = 0;
   byte *yr = NULL;
   
   SUMA_ENTRY;
   
   if (!y || !isort || N_isort <= 0) SUMA_RETURN(yr);
   
   yr = (byte *)SUMA_calloc( N_isort, sizeof(byte));
   if (!yr) SUMA_RETURN(yr);
   
   for (i=0; i<N_isort; ++i) yr[i] = y[isort[i]];
   
   SUMA_RETURN(yr);
}


float *SUMA_freorder(float *y, int *isort, int N_isort)
{
   static char FuncName[]={"SUMA_freorder"};
   int i = 0;
   float *yr = NULL;
   
   SUMA_ENTRY;
   
   if (!y || !isort || N_isort <= 0) SUMA_RETURN(yr);
   
   yr = (float *)SUMA_calloc( N_isort, sizeof(float));
   if (!yr) SUMA_RETURN(yr);
   
   for (i=0; i<N_isort; ++i) yr[i] = y[isort[i]];
   
   SUMA_RETURN(yr);
}

float *SUMA_freorder_triplets(float *y, int *isort, int N_isort)
{
   static char FuncName[]={"SUMA_freorder_triplets"};
   int i = 0;
   float *yr = NULL;
   
   SUMA_ENTRY;
   
   if (!y || !isort || N_isort <= 0) SUMA_RETURN(yr);
   
   yr = (float *)SUMA_calloc( N_isort*3, sizeof(float));
   if (!yr) SUMA_RETURN(yr);
   
   for (i=0; i<N_isort; ++i) {
      yr[3*i] = y[3*isort[i]];
      yr[3*i+1] = y[3*isort[i]+1];
      yr[3*i+2] = y[3*isort[i]+2];
   }
   
   SUMA_RETURN(yr);
}

double *SUMA_dreorder(double *y, int *isort, int N_isort)
{
   static char FuncName[]={"SUMA_dreorder"};
   int i = 0;
   double *yr = NULL;
   
   SUMA_ENTRY;
   
   if (!y || !isort || N_isort <= 0) SUMA_RETURN(yr);
   
   yr = (double *)SUMA_calloc( N_isort, sizeof(double));
   if (!yr) SUMA_RETURN(yr);
   
   for (i=0; i<N_isort; ++i) yr[i] = y[isort[i]];
   
   SUMA_RETURN(yr);
}


float *SUMA_string_to_RGBA(char *s, float *here, float scl, int *Err) 
{
   static char FuncName[]={"SUMA_string_to_RGBA"};
   
   static int icall=0;
   static float fv[10][4];
   char *sc=NULL, i, err, one, twofif, N;
   float all[12];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (float *)(&fv[icall]);
   }
   here[0] = here[1] = here[2] =  here[3] = 1.0;
   if (Err) *Err=1; /* initialize with problem */
   
   if (!s) SUMA_RETURN(here);
   
   sc = SUMA_copy_string(s);
   
   /* deblank borders */
   sc = deblank_name(sc);
   if (!strcasecmp(sc,"red") || !strcasecmp(sc,"r")) {
      here[0] = 1.0; here[1] = 0.0; here[2] = 0.0; here[3] = 1.0;
      if (Err) *Err=0;
   } else if (!strcasecmp(sc,"green") || !strcasecmp(sc,"g")) {
      here[0] = 0.0; here[1] = 1.0; here[2] = 0.0; here[3] = 1.0;
      if (Err) *Err=0;
   } else if (!strcasecmp(sc,"blue") || !strcasecmp(sc,"b")) {
      here[0] = 0.0; here[1] = 0.0; here[2] = 1.0; here[3] = 1.0;  
      if (Err) *Err=0;
   } else if (!strcasecmp(sc,"yellow") || !strcasecmp(sc,"y")) {
      here[0] = 1.0; here[1] = 1.0; here[2] = 0.0; here[3] = 1.0;  
      if (Err) *Err=0;
   } else if (!strcasecmp(sc,"cyan") || !strcasecmp(sc,"c")) {
      here[0] = 0.0; here[1] = 1.0; here[2] = 1.0; here[3] = 1.0;  
      if (Err) *Err=0;
   } else if (!strcasecmp(sc,"purple") || !strcasecmp(sc,"p")) {
      here[0] = 1.0; here[1] = 0.0; here[2] = 1.0; here[3] = 1.0;  
      if (Err) *Err=0;
   } else if (!strcasecmp(sc,"white") || !strcasecmp(sc,"w")) {
      here[0] = 1.0; here[1] = 1.0; here[2] = 1.0; here[3] = 1.0;  
      if (Err) *Err=0;
   } else if (SUMA_CleanNumString(sc,(void *)4)) {
      SUMA_LH("Have RGBA");
      N=4;
      SUMA_StringToNum(sc, (void*)all, N, 1);
      one = 0; twofif = 0; err = 0;
      if (scl == 0.0f) {
         for (i=0; i<N; ++i) {
            if (all[i] >= 0.0 && all[i] <= 1.0) {
               ++one;
            } else if (all[i] >= 0.0 && all[i] <= 255.0) {
               ++twofif;
            } else {
               SUMA_S_Err("Bad col param %d in %s", i, sc);
               ++err;
            }
         }
         if (err) {
            SUMA_ifree(sc); SUMA_RETURN(here);
         }
         if (twofif == 0) scl = 1.0;
         else scl = 1.0/255.0;
      } 
      for (i=0; i<N; ++i) { here[i] = all[i]*scl; }
      if (Err) *Err=0;
   } else if (SUMA_CleanNumString(sc,(void *)3)) {
      SUMA_LH("Have RGB");
      N=3;
      SUMA_StringToNum(sc, (void*)all, N, 1);
      one = 0; twofif = 0; err = 0;
      if (scl == 0.0f) {
         for (i=0; i<N; ++i) {
            if (all[i] >= 0.0 && all[i] <= 1.0) {
               ++one;
            } else if (all[i] >= 0.0 && all[i] <= 255.0) {
               ++twofif;
            } else {
               SUMA_S_Err("Bad col param %d in %s", i, sc);
               ++err;
            }
         }
         if (err) {
            SUMA_ifree(sc); SUMA_RETURN(here);
         }
         if (twofif == 0) scl = 1.0;
         else scl = 1.0/255.0;
      } 
      for (i=0; i<N; ++i) { here[i] = all[i]*scl; }
      here[3] = 1.0*scl; 
      if (Err) *Err=0;
   } else if (SUMA_CleanNumString(sc,(void *)1)) {
      SUMA_LH("Have  1 color, going gray scale");
      N=1;
      SUMA_StringToNum(sc, (void*)all, N, 1);
      one = 0; twofif = 0; err = 0;
      if (scl == 0.0f) {
         for (i=0; i<N; ++i) {
            if (all[i] >= 0.0 && all[i] <= 1.0) {
               ++one;
            } else if (all[i] >= 0.0 && all[i] <= 255.0) {
               ++twofif;
            } else {
               SUMA_S_Err("Bad col param %d in %s", i, sc);
               ++err;
            }
         }
         if (err) {
            SUMA_ifree(sc); SUMA_RETURN(here);
         }
         if (twofif == 0) scl = 1.0;
         else scl = 1.0/255.0;
      }
      for (i=0; i<4; ++i) { here[i] = all[0]*scl; }
      if (Err) *Err=0;
   } else if (SUMA_CleanNumString(sc,(void *)0)) {
      SUMA_LH("Have no numbers");
   }
   SUMA_ifree(sc);
   SUMA_RETURN(here);
}

char *SUMA_floats_to_string(float *rgba, int N, float scl, char *here, int *Err,
                            char *sep, int MVf) 
{
   static char FuncName[]={"SUMA_floats_to_string"};
   static int icall=0;
   static char fv[10][64];
   char *sc=NULL, i, err, one, twofif;
   float all[12];
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!here) {
      ++icall; if (icall > 9) icall = 0;
      here = (char *)(&fv[icall]);
   }
   here[0] = '\0';
   if (Err) *Err=1; /* initialize with problem */
   
   if (!rgba) SUMA_RETURN(here);
   if (!sep) sep = ",";
   if (scl == 0.0) scl = 1.0;

   if (N == 4) {
      if (MVf > 0) {
         snprintf(here, 63, "%s%s%s%s%s%s%s", 
                  MV_format_fval2(rgba[0]*scl, MVf), sep, 
                  MV_format_fval2(rgba[1]*scl, MVf), sep, 
                  MV_format_fval2(rgba[2]*scl, MVf), sep, 
                  MV_format_fval2(rgba[3]*scl, MVf));
      } else if (MVf == 0) {
         snprintf(here, 63, "%f%s%f%s%f%s%f", 
                  rgba[0]*scl, sep, rgba[1]*scl, sep, 
                  rgba[2]*scl, sep, rgba[3]*scl);
      } else if (MVf == -1) {
         snprintf(here, 63, "%.1f%s%.1f%s%.1f%s%.1f", 
                  rgba[0]*scl, sep, rgba[1]*scl, sep, 
                  rgba[2]*scl, sep, rgba[3]*scl);
      } else if (MVf == -2) {
         snprintf(here, 63, "%.2f%s%.2f%s%.2f%s%.2f", 
                  rgba[0]*scl, sep, rgba[1]*scl, sep, 
                  rgba[2]*scl, sep, rgba[3]*scl);
      } else if (MVf == -3) {
         snprintf(here, 63, "%.3f%s%.3f%s%.3f%s%.3f", 
                  rgba[0]*scl, sep, rgba[1]*scl, sep, 
                  rgba[2]*scl, sep, rgba[3]*scl);
      } 
   } else if (N == 3) {
      if (MVf > 0) {
         snprintf(here, 63, "%s%s%s%s%s", 
                  MV_format_fval2(rgba[0]*scl, MVf), sep, 
                  MV_format_fval2(rgba[1]*scl, MVf), sep, 
                  MV_format_fval2(rgba[2]*scl, MVf));
      } else if (MVf == 0) {
         snprintf(here, 63, "%f%s%f%s%f", 
                  rgba[0]*scl, sep, rgba[1]*scl, sep, 
                  rgba[2]*scl);
      } else if (MVf == -1) {
         snprintf(here, 63, "%.1f%s%.1f%s%.1f", 
                  rgba[0]*scl, sep, rgba[1]*scl, sep, 
                  rgba[2]*scl);
      } else if (MVf == -2) {
         snprintf(here, 63, "%.2f%s%.2f%s%.2f", 
                  rgba[0]*scl, sep, rgba[1]*scl, sep, 
                  rgba[2]*scl);
      } else if (MVf == -3) {
         snprintf(here, 63, "%.3f%s%.3f%s%.3f", 
                  rgba[0]*scl, sep, rgba[1]*scl, sep, 
                  rgba[2]*scl);
      }
   } else if (N == 1) {
      if (MVf > 0) {
         snprintf(here, 63, "%s", 
                  MV_format_fval2(rgba[0]*scl, MVf));
      } else if (MVf == 0) {
         snprintf(here, 63, "%f", 
                  rgba[0]*scl);
      } else if (MVf == -1) {
         snprintf(here, 63, "%.1f", 
                  rgba[0]*scl);
      } else if (MVf == -2) {
         snprintf(here, 63, "%.2f", 
                  rgba[0]*scl);
      } else if (MVf == -3) {
         snprintf(here, 63, "%.3f", 
                  rgba[0]*scl);
      }
   } 
   SUMA_RETURN(here);
}
