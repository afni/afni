/*! File to contain routines for creating isosurfaces.

NOTE: MarchingCube code was translated from Thomas Lewiner's C++
implementation of the paper:
Efficient Implementation of Marching Cubes' Cases with Topological Guarantees
by Thomas Lewiner, Helio Lopes, Antonio Wilson Vieira and Geovan Tavares 
in Journal of Graphics Tools. 
http://www-sop.inria.fr/prisme/personnel/Thomas.Lewiner/JGT.pdf
*/

#include "SUMA_suma.h"
#include "MarchingCubes/MarchingCubes.h"
#include "SUMA_gts.h"


#if 1

static char SUMA_ISO_Obj_Types[][10] = { {"Cushin"}, {"Sphere"}, {"Plane"}, {"Cassini"}, {"Blooby"}, {"Chair"}, {"Cyclide"}, {"2 Torus"}, {"mc case"}, {"Drip"} };

void usage_SUMA_IsoSurface (SUMA_GENERIC_ARGV_PARSE *ps)
   {
      static char FuncName[]={"usage_SUMA_IsoSurface"};
      char * s = NULL, *sio=NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( 
      "\n"
"Usage: A program to perform isosurface extraction from a volume.\n"
"  Based on code by Thomas Lewiner (see below).\n"
"\n"
"  IsoSurface  < -input VOL | -shape S GR >\n"
"              < -isoval V | -isorange V0 V1 | -isocmask MASK_COM >\n"
"              [< -o_TYPE PREFIX>] [-Tsmooth KPB NITER]\n"
"              [< -debug DBG >]\n"  
"\n"
"  Mandatory parameters:\n"
"     You must use one of the following two options:\n"
"     -input VOL: Input volume.\n"
"     -shape S GR: Built in shape.\n"
"                  where S is the shape number, \n"
"                  between 0 and 9 (see below). \n"
"                  and GR is the grid size (like 64).\n"
"                  If you use -debug 1 with this option\n"
"                  a .1D volume called mc_shape*.1D is\n"
"                  written to disk. Watch the debug output\n"
"                  for a command suggesting how to turn\n"
"                  this 1D file into a BRIK volume for viewing\n"
"                  in AFNI.\n"
"     You must use one of the following iso* options:\n"
"     -isorois: Create isosurface for each unique value in the input volume\n"
"               This outputs multiple surfaces that are automatically named\n"
"               using PREFIX and the label and key corresponding to each of\n"
"               the ROIs to the extent that they are available.\n"
"               Example:\n"
"           IsoSurface -isorois -input TTatlas+tlrc'[0]' -o_gii auto.all\n"
"           suma -onestate -i auto.all.k*.gii\n"
"               You can also follow -isorois with the only ROI keys you want\n"
"               considered as in:\n"
"           IsoSurface -isorois 276 277 54 input TTatlas+tlrc'[0]'-o_gii auto\n"
"     -isorois+dsets: Same as -isorois, but also write out a labeled dataset \n"
"                     for each surface."
"               Example:\n"
"           IsoSurface -isorois+dsets -input TTatlas+tlrc'[0]' -o_gii auto.all\n"
"           suma -onestate -i auto.all.k*.gii\n"
"     -mergerois [LAB_OUT]: Combine all surfaces from isorois into one surface\n"
"                 If you specify LAB_OUT then a dataset with ROIs labels is \n"
"                 also written out and named LAB_OUT\n"
"                Example:\n"
"           IsoSurface -isorois -mergerois auto.all.niml.dset \\\n"
"                      -input TTatlas+tlrc'[0]' -o_gii auto.all\n"
"           suma -i auto.all.gii\n"
"     -mergerois+dset: Same as -mergerois PREFIX, where PREFIX is the\n"
"                      prefix of the name you will use for the output surface.\n"
"                      The reason for this convenience function is that\n"
"                      suma now automatically loads a dataset that has the \n"
"                      same prefix as the loaded surface and this option saves\n"
"                      you from having to manually match the names.\n"
"\n"
"     -isoval V: Create isosurface where volume = V\n"
"     -isorange V0 V1: Create isosurface where V0 <= volume < V1\n"
"     -isocmask MASK_COM: Create isosurface where MASK_COM != 0\n"
"        For example: -isocmask '-a VOL+orig -expr (1-bool(a-V))' \n"
"        is equivalent to using -isoval V. \n"
"     NOTE: -isorange and -isocmask are only allowed with -xform mask\n"
"            See -xform below for details.\n"
"     NOTE NOTE: Sometimes you can get the equivalent of the negative space\n"
"                of what you're expecting. If that is the case, try padding\n"
"                your volume with zeros on all sides and try again.\n"
"                You can do the padding with something like:\n"
"                3dZeropad -I 2 -S 2 -R 2 -L 2 -A 2 -P 2 -prefix ... \n"
"\n"
"     -Tsmooth KPB NITER: Smooth resultant surface using the Taubin smoothing\n"
"                         approach in SurfSmooth and with parameters KPB and \n"
"                         NITER. If unsure, try -Tsmooth 0.1 100 for a start.\n"
"  Optional Parameters:\n"
"     -autocrop: Crop input volume (and any internally computed volume) before\n"
"                doing IsoSurface extraction. When you're running the program\n"
"                on largely empty datasets or with -isorois* then using \n"
"                -autocrop will make the program run faster. Either way \n"
"                the output should not change however.\n"
"     -remesh EDGE_FRACTION: Remesh the surface(s) to result in a surface with\n"
"                            N_edges_new = N_edges_old x EDGE_FRACTION\n"
"                            EDGE_FRACTION should be between 0.0 and 1.0\n"
"     -xform XFORM:  Transform to apply to volume values\n"
"                    before searching for sign change\n"
"                    boundary. XFORM can be one of:\n"
"            mask: values that meet the iso* conditions\n"
"                  are set to 1. All other values are set\n"
"                  to -1. This is the default XFORM.\n"
"            shift: subtract V from the dataset and then \n"
"                   search for 0 isosurface. This has the\n"
"                   effect of constructing the V isosurface\n"
"                   if your dataset has a continuum of values.\n"
"                   This option can only be used with -isoval V.\n"
"            none: apply no transforms. This assumes that\n"
"                  your volume has a continuum of values \n"
"                  from negative to positive and that you\n"
"                  are seeking to 0 isosurface.\n"
"                  This option can only be used with -isoval 0.\n"
"     -o_TYPE PREFIX: prefix of output surface.\n"
"        where TYPE specifies the format of the surface\n"
"        and PREFIX is, well, the prefix.\n"
"        TYPE is one of: fs, 1d (or vec), sf, ply.\n"
"        Default is: -o_ply \n"
"\n"
"%s\n"
"\n"
"     -debug DBG: debug levels of 0 (default), 1, 2, 3.\n"
"        This is no Rick Reynolds debug, which is oft nicer\n"
"        than the results, but it will do.\n"
"\n"
"%s"
   , ps->hverb > 1 ? sio:"   Use -help for more detail.\n"
   , ps->hverb ? "  Built In Shapes:\n":""); 
     if (ps->hverb) {
         for (i=0; i<10;++i) {
   printf(  "     %d: %s\n", i, SUMA_ISO_Obj_Types[i]);   
         }
      }
      printf (    "\n" 
"  NOTE:\n"
"  The code for the heart of this program is a translation of:\n"
"  Thomas Lewiner's C++ implementation of the algorithm in:\n"
"  Efficient Implementation of Marching Cubes' Cases with Topological Guarantees\n"
"  by Thomas Lewiner, Helio Lopes, Antonio Wilson Vieira and Geovan Tavares \n"
"  in Journal of Graphics Tools. \n"
"  http://www-sop.inria.fr/prisme/personnel/Thomas.Lewiner/JGT.pdf\n"
"\n"
"%s"
"\n", ps->hverb > 1 ? s:"   Use -help for more detail.");
       SUMA_free(s); s = NULL;  SUMA_free(sio); sio = NULL;          
       printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
       return;
   }
   
/*!
   \brief parse the arguments for SurfSmooth program
   
   \param argv (char *)
   \param argc (int)
   \return Opt (SUMA_GENERIC_PROG_OPTIONS_STRUCT *) options structure.
               To free it, use 
               SUMA_free(Opt->out_name); 
               SUMA_free(Opt);
*/
SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_IsoSurface_ParseInput (char *argv[], 
                                       int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_IsoSurface_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar, i, ind;
   char *outname;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = (SUMA_GENERIC_PROG_OPTIONS_STRUCT *)
            SUMA_calloc(1, sizeof(SUMA_GENERIC_PROG_OPTIONS_STRUCT));
   
   kar = 1;
   Opt->spec_file = NULL;
   Opt->out_prefix = NULL;
   Opt->sv_name = NULL;
   Opt->N_surf = -1;
   Opt->in_name = NULL;
   Opt->cmask = NULL;
   Opt->MaskMode = SUMA_ISO_UNDEFINED;
   for (i=0; i<SUMA_GENERIC_PROG_MAX_SURF; ++i) { Opt->surf_names[i] = NULL; }
   outname = NULL;
   Opt->in_vol = NULL;
   Opt->nvox = -1;
   Opt->ninmask = -1;
   Opt->mcfv = NULL;
   Opt->debug = 0;
   Opt->v0 = 0.0;
   Opt->v1 = -1.0;
   Opt->dvec = NULL;
   Opt->SurfFileType = SUMA_PLY;
   Opt->SurfFileFormat = SUMA_ASCII;
   Opt->xform = SUMA_ISO_XFORM_MASK;
   Opt->obj_type = -1;
   Opt->obj_type_res = -1;
   Opt->n_ivec = -1;
   Opt->XYZ = NULL;
   Opt->in_1D = NULL;
   Opt->N_XYZ = 0;
   Opt->ivec = NULL;
   Opt->N_it = 0;
   Opt->Zt = 0.1;
   Opt->s = NULL;
   Opt->b1 = 0;
   Opt->b2 = 0;
   Opt->efrac = 0;
   brk = NOPE;
   while (kar < argc) { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
          ps->hverb = strlen(argv[kar])>3?2:1;
          usage_SUMA_IsoSurface(ps);
          exit (0);
      }
      
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-xform") == 0)) {
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -xform \n");
            exit (1);
         }
         if (!strcmp(argv[kar], "mask")) {
            Opt->xform = SUMA_ISO_XFORM_MASK;
         } else if (!strcmp(argv[kar], "none")) {
            Opt->xform = SUMA_ISO_XFORM_NONE;
         } else if (!strcmp(argv[kar], "shift")) {
            Opt->xform = SUMA_ISO_XFORM_SHIFT;
         }else {
            fprintf (SUMA_STDERR, 
                     "%s is a bad parameter for -xform option. \n", argv[kar]);
            exit (1);
         }
         brk = YUP;
      }
            
      if (!brk && (strcmp(argv[kar], "-Tsmooth") == 0)) {
         kar ++;
         if (kar+1 >= argc)  {
            fprintf (SUMA_STDERR, "need 2 arguments after -Tsmooth \n");
            exit (1);
         }
         Opt->Zt = atof(argv[kar++]); 
         Opt->N_it = atoi(argv[kar]);
         brk = YUP;
      }
            
    
      if (!brk && (strcmp(argv[kar], "-debug") == 0)) {
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -debug \n");
            exit (1);
         }
         Opt->debug = atoi(argv[kar]);
         if (Opt->debug > 2) { LocalHead = YUP; }
         brk = YUP;
      }
            
      if (!brk && (strcmp(argv[kar], "-isocmask") == 0)) {
         if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
            fprintf (SUMA_STDERR, "only one masking mode (-iso*) allowed.\n");
         }
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -isocmask \n");
            exit (1);
         }
         Opt->cmask = argv[kar];
         Opt->MaskMode = SUMA_ISO_CMASK;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-remesh") == 0)) {
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -remesh \n");
            exit (1);
         }
         Opt->efrac = atof(argv[kar]);
         if (Opt->efrac < 0.0 || Opt->efrac > 1.0) {
            SUMA_S_Err("-remesh value should be between 0 and 1.0, have %f"
                        , Opt->efrac);
            exit(1);
         }
         brk = YUP;
      }
      
      if (!brk && ((strcmp(argv[kar], "-isorois") == 0) ||
                   (strcmp(argv[kar], "-isorois+dsets") == 0))) {
         static int nalloc=0;
         if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
            fprintf (SUMA_STDERR, "only one masking mode (-iso*) allowed.\n");
         }
         while (kar+1<argc && argv[kar+1][0] != '-') {
            kar ++;
            if (!nalloc || nalloc <= Opt->n_ivec) {
               if (Opt->n_ivec < 0) Opt->n_ivec = 0;
               nalloc += 1000;
               Opt->ivec = (int *)SUMA_realloc(Opt->ivec, nalloc*sizeof(int));
            }
            Opt->ivec[Opt->n_ivec++] = atoi(argv[kar]);
         }
         Opt->MaskMode = SUMA_ISO_ROIS;
         if ((strcmp(argv[kar], "-isorois+dsets") == 0)) Opt->b1 = 1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-mergerois") == 0)) {
         SUMA_ifree(Opt->s);
         while (kar+1<argc && argv[kar+1][0] != '-') {
            kar ++;
            Opt->s = SUMA_copy_string(argv[kar]);
         }
         if (!Opt->s) Opt->s = SUMA_copy_string("_Don't_Do_No'in");
         brk = YUP;
      }
     
      if (!brk && ((strcmp(argv[kar], "-mergerois+dset") == 0) ||
                   (strcmp(argv[kar], "-mergerois+dsets") == 0))) {
         SUMA_ifree(Opt->s);
         Opt->s = SUMA_copy_string("_from_surf_yall");
         brk = YUP;
      }
     
      if (!brk && (strcmp(argv[kar], "-autocrop") == 0)) {
         Opt->b2 = 1;
         brk = YUP;
      }
     
      if (!brk && (strcmp(argv[kar], "-isoval") == 0)) {
         if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
            fprintf (SUMA_STDERR, "only one masking mode (-iso*) allowed.\n");
         }
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -isoval \n");
            exit (1);
         }
         Opt->v0 = atof(argv[kar]);
         Opt->MaskMode = SUMA_ISO_VAL;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-isorange") == 0)) {
         if (Opt->MaskMode != SUMA_ISO_UNDEFINED) {
            fprintf (SUMA_STDERR, "only one masking mode (-iso*) allowed.\n");
         }
         kar ++;
         if (kar+1 >= argc)  {
            fprintf (SUMA_STDERR, "need 2 arguments after -isorange \n");
            exit (1);
         }
         Opt->v0 = atof(argv[kar]);kar ++;
         Opt->v1 = atof(argv[kar]);
         Opt->MaskMode = SUMA_ISO_RANGE;
         if (Opt->v1 < Opt->v0) {
            fprintf (SUMA_STDERR, 
                  "range values wrong. Must have %f <= %f \n", Opt->v0, Opt->v1);
            exit (1);
         }
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-input") == 0)) {
         kar ++;
         if (kar >= argc)  {
            fprintf (SUMA_STDERR, "need argument after -input \n");
            exit (1);
         }
         Opt->in_name = SUMA_copy_string(argv[kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-shape") == 0)) {
         kar ++;
         if (kar+1 >= argc)  {
            fprintf (SUMA_STDERR, "need 2 arguments after -shape \n");
            exit (1);
         }
         Opt->obj_type = atoi(argv[kar]); kar ++;
         Opt->obj_type_res = atoi(argv[kar]);
         if (Opt->obj_type < 0 || Opt->obj_type > 9) {
            SUMA_S_Errv("Shape number (S) must be between 0 and 9. I have %d\n", 
                        Opt->obj_type);
            exit (1);
         }
         if (Opt->obj_type_res < 0) {
            SUMA_S_Errv("Shape grid resolution (GR) must be > 0 . I have %d\n", 
                        Opt->obj_type_res);
            exit (1);
         }
         brk = YUP;
      }
      
      if (!brk && !ps->arg_checked[kar]) {
         SUMA_S_Errv("Option %s not understood. Try -help for usage\n", 
                     argv[kar]);
         suggest_best_prog_option(argv[0], argv[kar]);
         exit (1);
      } else { 
         brk = NOPE;
         kar ++;
      }
   }

fprintf(stderr,"== MaskMode %g, v0 %g, v1 %d\n",Opt->v0,Opt->v1,Opt->MaskMode);

   /* transfer some options to Opt from ps.  Clunky retrofitting */
   if (ps->o_N_surfnames) {
      Opt->out_prefix = SUMA_copy_string(ps->o_surfnames[0]);
      Opt->SurfFileType = ps->o_FT[0];
      Opt->SurfFileFormat = ps->o_FF[0];
   }
   
   if (Opt->in_name && Opt->obj_type >= 0) {
      SUMA_S_Err("Options -input and -shape are mutually exclusive.\n");
      exit(1);
   }
   if (!Opt->in_name && Opt->obj_type < 0) {
      SUMA_S_Err("Either -input or -shape options must be used.\n");
      exit(1);
   }
   if (!Opt->out_prefix) Opt->out_prefix = SUMA_copy_string("isosurface_out");
   if (Opt->xform == SUMA_ISO_XFORM_NONE) {
      if (Opt->v0 != 0) {
         SUMA_S_Errv("Bad %f isovalue\n"
               "With -xform none you can only extract the 0 isosurface.\n"
               "(i.e. -isoval 0)\n", Opt->v0);
         exit(1);
      }
      if (Opt->MaskMode != SUMA_ISO_VAL) {
         SUMA_S_Err("With -xform none you can only use -isoval 0\n");
         exit(1);
      }
   }
   if (Opt->xform == SUMA_ISO_XFORM_SHIFT) {
      if (Opt->MaskMode != SUMA_ISO_VAL) {
         SUMA_S_Err("With -xform shift you can only use -isoval val\n");
         exit(1);
      }
   }
   
   SUMA_RETURN(Opt);
}

/* A convenience wrapper for SUMA_THD_ROI_IsoSurfaces
to also perform remeshing. The remeshing should be
part of SUMA_THD_ROI_IsoSurfaces but I don't want 
to make all of libSUMA.a depend on libgts */
SUMA_SurfaceObject **SUMA_THD_ROI_IsoSurfaces_ef(
                           THD_3dim_dataset *in_volu, int isb,
                            int *valmask, int *N_valmask,
                            byte *cmask, byte cropit, int debug, float efrac)
{
   static char FuncName[]={"SUMA_THD_ROI_IsoSurfaces_ef"};
   SUMA_SurfaceObject **SOv=NULL, *SOr=NULL;
   int i=0;
   SUMA_ENTRY;
   
   SOv=SUMA_THD_ROI_IsoSurfaces(in_volu, isb, valmask, N_valmask, 
                                cmask, cropit, debug);
   if (!SOv) {
      SUMA_RETURN(SOv);
   }
   if (efrac > 0.0 && efrac < 1.0) {
      i = 0;
      while (SOv[i]) {
         if (debug) {
            SUMA_S_Note("Reducing mesh for surface %s", SOv[i]->Label);
         }
         if (!(SOr = SUMA_Mesh_Resample(SOv[i], efrac))) {
            SUMA_S_Err("Failed to resample surface %d", i);
         } else {
            SUMA_ifree(SOr->Label);
            SOr->Label = SUMA_copy_string(SOv[i]->Label);
            SUMA_Free_Surface_Object(SOv[i]);
            SOv[i] = SOr; SOr = NULL;
         }
         ++i;
      }
   }
   SUMA_RETURN(SOv);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"IsoSurface"}; 
   int i;
   void *SO_name=NULL;
   SUMA_SurfaceObject *SO = NULL;
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   char  stmp[200];
   SUMA_Boolean exists = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;

   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   
   /* Allocate space for DO structure */
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-o;");
   
   Opt = SUMA_IsoSurface_ParseInput (argv, argc, ps);
   if (argc < 2) {
      SUMA_S_Err("Too few options");
      exit (1);
   }
   
   if (Opt->debug > 1) set_suma_debug(Opt->debug-1);
   
   SO_name = SUMA_Prefix2SurfaceName(  Opt->out_prefix, NULL, NULL, 
                                       Opt->SurfFileType, &exists);
   if (exists && !THD_ok_overwrite()) {
      SUMA_S_Errv("Output file(s) %s* on disk.\nWill not overwrite.\n", 
                  Opt->out_prefix);
      exit(1);
   }
  
   if (Opt->MaskMode != SUMA_ISO_ROIS) {
      if (Opt->obj_type < 0) {
         if (Opt->debug) {
            SUMA_S_Note("Creating mask...");
         }
         if (!SUMA_Get_isosurface_datasets (Opt)) {
            SUMA_SL_Err("Failed to get data.");
            exit(1);
         }

         if (Opt->debug > 1) {
            if (Opt->debug == 2) {
               FILE *fout=fopen("inmaskvec.1D","w");
               SUMA_S_Note("Writing masked values...\n");
               if (!fout) {
                  SUMA_SL_Err("Failed to write maskvec");
                  exit(1);
               }
               fprintf(fout,"#Col. 0 Voxel Index\n"
                            "#Col. 1 Is a mask (all values here should be 1)\n");
               for (i=0; i<Opt->nvox; ++i) {
                  if (Opt->mcfv[i]) {
                     fprintf(fout,"%d %.2f\n", i, Opt->mcfv[i]);
                  }
               }
               fclose(fout); fout = NULL;
            } else {
               FILE *fout=fopen("maskvec.1D","w");
               SUMA_S_Note("Writing all mask values...\n");
               if (!fout) {
                  SUMA_S_Err("Failed to write maskvec");
                  exit(1);
               }
               fprintf(fout,  "#Col. 0 Voxel Index\n"
                              "#Col. 1 Is in mask ?\n" );
               for (i=0; i<Opt->nvox; ++i) {
                  fprintf(fout,"%d %.2f\n", i, Opt->mcfv[i]);
               }
               fclose(fout); fout = NULL;
            }
         }
      } else {
         if (Opt->debug) {
            SUMA_S_Note("Using built in types...");
         }
      }
      /* Now call Marching Cube functions */
      if (!(SO = SUMA_MarchingCubesSurface(Opt))) {
         SUMA_S_Err("Failed to create surface.\n");
         exit(1);
      }
      
      if (Opt->N_it) {
         if (!SUMA_Taubin_Smooth_SO( SO, SUMA_EQUAL, 
                                     Opt->Zt, NULL, 0, Opt->N_it)) {
            SUMA_S_Err("Failed to smooth surface");
         }
      }
      
      if (Opt->efrac > 0.0 && Opt->efrac < 1.0) {
         SUMA_SurfaceObject *SOr=NULL;
         if (!(SOr = SUMA_Mesh_Resample(SO, Opt->efrac))) {
            SUMA_S_Err("Failed to resample surface. Continuing...");
         } else {
            SUMA_ifree(SOr->Label);
            SOr->Label = SUMA_copy_string(SO->Label);
            SUMA_Free_Surface_Object(SO);
            SO = SOr; SOr = NULL;
         }
      }
      
      /* write the surface to disk */
      if (!SUMA_Save_Surface_Object (SO_name, SO, Opt->SurfFileType, 
                                     Opt->SurfFileFormat, NULL)) {
         SUMA_S_Err("Failed to write surface object.\n");
         exit (1);
      }
   } else {
      SUMA_SurfaceObject **SOv=NULL;
      char *ppo=NULL, astr[1024]={""}, *labcp=NULL;
      int key=-1, *keys = NULL, *labs=NULL, ii=0, kk=0;
      char **labstr = NULL,  stmp[64], *oname=NULL;
      SUMA_DSET *dset=NULL;
      SUMA_COLOR_MAP *SM=NULL;
      set_atlas_name_code(0);  /* only deal with short names for now */
      if (!SUMA_Get_isosurface_datasets (Opt)) {
           SUMA_SL_Err("Failed to get data.");
           exit(1);
      }
      if (!(SOv = SUMA_THD_ROI_IsoSurfaces_ef(Opt->in_vol, 0,
                                     Opt->ivec, 
                                     &Opt->n_ivec, NULL, Opt->b2, Opt->debug,
                                     Opt->efrac))) {
         SUMA_S_Err("No surfaces generated");
         exit(1);
      } else {
         /* Prepare for writing datasets, potentially */
         if (!(labstr=(char **)SUMA_calloc(Opt->n_ivec, sizeof(char*))) 
                                       ||
             !(keys = (int *)SUMA_calloc(Opt->n_ivec, sizeof(int)))) {
            SUMA_S_Crit("Failed to allocate for %d ints and strings!", 
                        Opt->n_ivec);
            exit(1);   
         } 
         i=0;
         while (SOv[i]) {
            labstr[i] = SUMA_Decode_ROI_IsoSurfacesLabels(SOv[i]->Label,
                                                          keys+i);
            if (keys[i] < 0) keys[i] = i;
            if (!labstr[i]) { 
               sprintf(stmp,"roi%03d", keys[i]); 
               labstr[i] = SUMA_copy_string(stmp);
            }
            ++i;
         }
         if (!(SM = SUMA_LabelsKeys2Cmap(labstr, Opt->n_ivec, keys, 
                                   NULL, 0, DSET_PREFIX(Opt->in_vol)))) {
            SUMA_S_Err("Failed to create color map!");                     
         }               
         
         if (Opt->s) { /* Merged mode */
            SUMA_SurfaceObject *SOm=NULL;
            SUMA_S_Note("Merging %d surfaces...", Opt->n_ivec);
            if (!(SOm = SUMA_MergeSurfs(SOv, Opt->n_ivec))) {
               SUMA_S_Err("Failed to merge");
               exit(1);
            }
            SO_name = SUMA_Prefix2SurfaceName( Opt->out_prefix, NULL, NULL, 
                                               Opt->SurfFileType, &exists);
            if (exists && !THD_ok_overwrite()) {
               SUMA_S_Errv("Output file(s) %s* on disk.\n"
                           "Will not overwrite.\n", 
                           Opt->out_prefix);
               exit(1);
            }

            if (Opt->N_it) {
               if (!SUMA_Taubin_Smooth_SO( SOm, SUMA_EQUAL, 
                                           Opt->Zt, NULL, 0, Opt->N_it)) {
                  SUMA_S_Err("Failed to smooth surface %s", Opt->out_prefix);
               }
            }

            if (!SUMA_Save_Surface_Object (SO_name, SOm, Opt->SurfFileType, 
                                     Opt->SurfFileFormat, NULL)) {
                     SUMA_S_Err("Failed to write surface object.\n");
                  exit (1);
            }
            
            if (!strcmp(Opt->s, "_from_surf_yall")) {
               SUMA_ifree(Opt->s);
               Opt->s = SUMA_RemoveSurfNameExtension (SO_name, SOm->FileType);
            }
            
            if (strcmp(Opt->s, "_Don't_Do_No'in")) { /* write out labels dset */
               if (!(labs = (int *)SUMA_malloc(SOm->N_Node*sizeof(int)))) {
                  SUMA_S_Crit("Failed to allocate for %d ints!", SOm->N_Node);
                  exit(1);
               }
               i=0; kk = 0;
               while (SOv[i]) {
                  labstr[i] = SUMA_Decode_ROI_IsoSurfacesLabels(SOv[i]->Label, 
                                                                keys+i);
                  if (keys[i] < 0) keys[i] = i;
                  if (!labstr[i]) { 
                     sprintf(stmp,"roi%03d", keys[i]); 
                     labstr[i] = SUMA_copy_string(stmp);
                  }
                  for (ii=0; ii<SOv[i]->N_Node; ++ii) {
                     labs[kk++] = keys[i];
                  }
                  ++i;
               }
               
               /* Now form dset and add labeltable */
               dset = SUMA_iar2dset_ns(Opt->s, NULL, SOm->idcode_str, 
                                       &labs, SOm->N_Node, 1, 0);
                                       
               if (!(SM = SUMA_LabelsKeys2Cmap(labstr, Opt->n_ivec, keys, 
                                         NULL, 0, DSET_PREFIX(Opt->in_vol)))) {
                  SUMA_S_Err("Failed to create color map!");                     
               }
               
               if (!SUMA_AddNodeIndexColumn(dset, SOm->N_Node)) {
                  SUMA_S_Err(" Failed to add a node index column");
               }               
               
               if (!SUMA_dset_to_Label_dset_cmap(dset, SM)) {
                  SUMA_S_Err("Failed to turn dset to label dset");
               }
               
               oname = SUMA_WriteDset_s (Opt->s, dset, SUMA_NO_DSET_FORMAT, 
                                         1, 1);
               SUMA_ifree(oname);
               SUMA_FreeDset(dset);
               SUMA_ifree(labs);
            }
            
            SUMA_Free_Surface_Object(SOm); SOm = NULL;
            
            
            i=0;
            while (SOv[i]) {
               SUMA_Free_Surface_Object(SOv[i]); SOv[i]=NULL;
               if (labstr) SUMA_ifree(labstr[i]);
               ++i;
            }
         } else { /* Loose change */
            i=0;
            while (SOv[i]) {
               labcp = SUMA_Decode_ROI_IsoSurfacesLabels(SOv[i]->Label, &key);
               labcp = cdeblank_allname(labcp,'_');
               labcp = deslash_allname(labcp,'-');
               if (labcp && key > 0) {
                  snprintf(astr,100,".%s.k%d",labcp, key);
               } else if (labcp) {
                  snprintf(astr,100,".%s",labcp);
               } else if (key > 0) {
                  snprintf(astr,100,".k%d",key);
               } else {
                  snprintf(astr,100,".i%d", i);
               }
               SUMA_ifree(labcp);
               ppo = SUMA_ModifyName(Opt->out_prefix, "append", astr, NULL);
               SO_name = SUMA_Prefix2SurfaceName(  ppo, NULL, NULL, 
                                          Opt->SurfFileType, &exists);
               if (exists && !THD_ok_overwrite()) {
                  SUMA_S_Errv("Output file(s) %s* on disk.\n"
                              "Will not overwrite.\n", 
                              Opt->out_prefix);
                  exit(1);
               }

               if (Opt->N_it) {
                  if (!SUMA_Taubin_Smooth_SO( SOv[i], SUMA_EQUAL, 
                                              Opt->Zt, NULL, 0, Opt->N_it)) {
                     SUMA_S_Err("Failed to smooth surface %d: %s", i, ppo);
                  }
               }

               if (!SUMA_Save_Surface_Object (SO_name, SOv[i], Opt->SurfFileType, 
                                        Opt->SurfFileFormat, NULL)) {
                        SUMA_S_Err("Failed to write surface object %s.\n", ppo);
                     exit (1);
               }
               
               if (Opt->b1) { /* make baby datasets */
                  if (!(labs = (int *)SUMA_calloc(SOv[i]->N_Node, sizeof(int)))){
                     SUMA_S_Err("Failed to allocate for %d ints", 
                                SOv[i]->N_Node);
                     exit(1);
                  }
                  for (ii=0; ii<SOv[i]->N_Node; ++ii) {
                     labs[ii] = keys[i];
                  }

                  /* Now form dset and add labeltable */
                  SUMA_ifree(ppo);
                  ppo = SUMA_ModifyName(Opt->out_prefix, "append", astr, NULL);
                  ppo = SUMA_append_replace_string(ppo,".niml.dset", "",1);
                  dset = SUMA_iar2dset_ns(ppo, NULL, SOv[i]->idcode_str, 
                                          &labs, SOv[i]->N_Node, 1, 0);

                  

                  if (!SUMA_AddNodeIndexColumn(dset, SOv[i]->N_Node)) {
                     SUMA_S_Err(" Failed to add a node index column");
                  }               

                  if (!SUMA_dset_to_Label_dset_cmap(dset, SM)) {
                     SUMA_S_Err("Failed to turn dset to label dset");
                  }

                  oname = SUMA_WriteDset_s (ppo, dset, SUMA_NO_DSET_FORMAT, 
                                            1, 1);
                  SUMA_ifree(oname);
                  SUMA_FreeDset(dset);
                  SUMA_ifree(labs);   
               }
               
               if (SO_name) SUMA_free(SO_name); SO_name = NULL;
               SUMA_ifree(ppo);
               SUMA_Free_Surface_Object(SOv[i]); SOv[i]=NULL;
               ++i;
            }
         }
         SUMA_ifree(SOv); SUMA_ifree(labstr); SUMA_ifree(keys);
         SUMA_Free_ColorMap(SM); SM = NULL;
      }
   }
   
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt->dvec) SUMA_free(Opt->dvec); Opt->dvec = NULL;
   if (Opt->mcfv) {SUMA_free(Opt->mcfv); Opt->mcfv = NULL;} 
   if (Opt->in_vol) { DSET_delete( Opt->in_vol); Opt->in_vol = NULL;} 
   if (Opt->out_prefix) SUMA_free(Opt->out_prefix); Opt->out_prefix = NULL;
   SUMA_ifree(Opt->ivec);
   if (Opt) SUMA_free(Opt);
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) {
      SUMA_SL_Err("DO Cleanup Failed!");
   }
   if (SO_name) SUMA_free(SO_name); SO_name = NULL;
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   exit(0);
}
#endif      
