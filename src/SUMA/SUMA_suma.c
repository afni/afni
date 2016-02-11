#define DEBUG_1
#ifdef DEBUG_1
   #define DEBUG_2
   #define DEBUG_3
#endif
   
/* Header FILES */
   
#include "SUMA_suma.h"
#include "../afni.h"

/* CODE */
#ifdef SUMA_DISASTER
/*!
   a function to test debugging 
*/
int * SUMA_disaster(void)
{
   static char FuncName[]={"SUMA_disaster"};
   int *iv1=NULL, *iv2 = NULL, *iv3 = NULL;
   int N_iv1, N_iv2;
   int i;
   double v[7] = {-1.6, -1.5, -1.4, 0, 1.4, 1.5, 1.6};
   
   SUMA_ENTRY;
   
   
   
   for (i=0; i<7; ++i) {
      fprintf (stderr,"%f  : r %d, c %d\n", 
               v[i], SUMA_ROUND(v[i]), SUMA_CEIL(v[i]));
   }
   
   SUMA_S_Notev("Domemtrace %d\n", get_Domemtrace());
   N_iv1 = 5;
   N_iv2 = 5;
   iv1 = (int*) SUMA_calloc(N_iv1, sizeof(int));
   iv2 = (int*) SUMA_calloc(N_iv2, sizeof(int));
   
   /* overwrite iv1 */
   iv1[N_iv1] = 3;
   
   /* overwrite iv2 */
   iv2[N_iv2] = 7;
   
   /* MEMCHECK should give a warning */
   SUMA_S_Note("Memcheck output");
   MCHECK ; fflush(stdout) ; /* ZSS */

   /* free iv1 (that should give a warning)*/
   SUMA_S_Note("Now freeing iv1");
   SUMA_free(iv1); /* without the -trace option, 
                      you'll get a warning of this corruption here */

   /* try to free iv3 although it was not allocated for */
   /* AFNI's functions do not check for this ...*/
   /* SUMA_free(iv3);*/
         
   /* don't free iv2, that should only give a warning when you
      exit with -trace option turned on */
   SUMA_S_Note("Now dumping malloc table");
   mcw_malloc_dump();
   
   /* if you use -trace, you'll get a warning at the return for iv2 
   All allocated memory will be checked, at the return, not just iv2*/   
   SUMA_S_Note("Now returning");
   SUMA_RETURN(iv2); 
}

#endif

/*!
   A toy function to identify windows on the root window.
   This function can potentially be used to identify where
   AFNI windows, for example, are taking screen space and 
   have SUMA avoid overlapping with them.
   The function needs much work. It is not terribly selective
   about what it chases down. 
   See command line  xwininfo -tree -root 
   for full listing from command line, or xwininfo for
   interactive queries.
   For now, we let this rest.
*/
int SUMA_WindowsOnRootDisplay(Display *dd, Window ww, int all)
{
   static char FuncName[]={"SUMA_WindowsOnRootDisplay"};
   Window rr, pr, *cr, jj;
   XWindowAttributes wa;
   unsigned int ncr, wr, hr, bwr, bhr;
   int ii, xr, yr, showgeom=0;
   char *wname=NULL, *sout=NULL;

   /* tell me what windows are displayed and where */
   if (XQueryTree(dd, 
                   ww,
                   &rr, &pr, &cr, &ncr)) {

      if (all) fprintf(stderr,"Have %d childern in query\n", ncr);
      for (ii=0; ii<ncr; ++ii) {
          showgeom = 0;
          XGetWindowAttributes(dd, cr[ii], &wa);
          XTranslateCoordinates (dd, cr[ii], wa.root, 
                  -wa.border_width,
                  -wa.border_width,
                  &xr, &yr, &jj);
         if (XFetchName(dd, cr[ii], &wname)) { 
            /* XGetWMName usage From source of xwininfo.c,
               Not getting advantage over XFetchName for 
               lots more hassle ....*/
            if (wname) {
               sout = SUMA_copy_string(wname);
               XFree(wname);
            } else sout = SUMA_copy_string("no name but fetch OK!");
            if (strcasestr(sout,"afni") ||
                strcasestr(sout,"suma") ) {
                fprintf(stderr,"   brethren %d: %s\n", ii, sout);
                showgeom=1;
            } else {
               if (all) {
                  fprintf(stderr,"   furner %d: %s\n", ii, sout);
                  showgeom=1;
               }
            }
         } else {
            if (all) {
               fprintf(stderr,"   %d: (No name!)\n", ii);
               showgeom = 1;
            }
         }
         if (showgeom) {
            fprintf(stderr,
               "      abs Upper Left X Y %d %d, (rel UL %d %d) W H%d %d, \n", 
                                    xr, yr, wa.x, wa.y, wa.width, wa.height);
             /* 
               Under alternative below, xr and yr are relative UL vals */ 
             #if 0
             if (XGetGeometry(dd, cr[ii], &rr,
                              &xr, &yr, &wr, &hr, &bwr, &bhr)) {
                fprintf(stderr,"       %d %d, %dx%d, %d %d\n", 
                               xr, yr, wr, hr, bwr, bhr);     

             }
             #endif
         }
         if (sout) SUMA_free(sout); sout = NULL;
         SUMA_WindowsOnRootDisplay(dd, cr[ii],all);
      } 
   }
}


void SUMA_usage (SUMA_GENERIC_ARGV_PARSE *ps, int detail)
   
  {/*Usage*/
          char *sb = NULL, *sio = NULL, *ssym=NULL;
          ssym = SUMA_help_SPEC_symbolic();
          sb = SUMA_help_basics();
          sio  = SUMA_help_IO_Args(ps);
          printf (
"\nUsage:  \n"
" Mode 0: Just type suma to see some toy surface and play\n"
"         with the interface. Some surfaces are generated\n"
"         using T. Lewiner's MarchingCubes library. \n"
"         Use '.' and ',' keys to cycle through surfaces.\n"
"\n"
" Mode 1: Using a spec file to specify surfaces\n"
"                suma -spec <Spec file> \n"
"                     [-sv <SurfVol>] [-ah AfniHost]\n"
"\n"
" Mode 2: Just show me the money\n"
"                suma <-i SomeSurface> \n"
"                     [-sv <SurfVol>] [-ah AfniHost]\n"
"\n"
"\n%s", detail ? "":"use -h or -help for more help detail.\n");
   if (detail) {
      printf ( 
" Mode 1:\n"
"   -spec <Spec file>: File containing surface specification. \n"     
"                      This file is typically generated by \n"     
"                      @SUMA_Make_Spec_FS (for FreeSurfer surfaces) or \n"
"                      @SUMA_Make_Spec_SF (for SureFit surfaces). \n"
"                      The Spec file should be located in the directory \n"
"                      containing the surfaces.\n"
"%s\n"
"   [-sv <SurfVol>]: Anatomical volume used in creating the surface \n"     
"                    and registerd to the current experiment's anatomical \n"
"                    volume (using @SUMA_AlignToExperiment). \n"
"                    This parameter is optional, but linking to AFNI is \n"
"                    not possible without it.If you find the need for it \n"
"                    (as some have), you can specify the SurfVol in the \n"
"                    specfile. You can do so by adding the field \n"
"                    SurfaceVolume to each surface in the spec file. \n"
"                    In this manner, you can have different surfaces using\n"
"                    different surface volumes.\n"     
"   [-ah AfniHost]: Name (or IP address) of the computer running AFNI. \n"     
"                     This parameter is optional, the default is localhost.\n"
"                     When both AFNI and SUMA are on the same computer, \n"
"                     communication is through shared memory. \n"
"                     You can turn that off by explicitly setting AfniHost\n"
"                     to 127.0.0.1\n"
"   [-niml]: Start listening for communications with NIML-formatted elements.\n"
"            Environment variable SUMA_START_NIML can also be used to start\n"
"            listening.\n"
"   [-noniml]: Do not start listening for communications with NIML-formatted\n"
"              elements, even if env. SUMA_START_NIML is set to YES\n"    
"\n"
" Mode 2: Using -t_TYPE or -t* options to specify surfaces on command line.\n"
"         -sv, -ah, -niml and -dev are still applicable here. This mode \n"
"         is meant to simplify the quick viewing of a surface model.\n"
"                suma [-i_TYPE surface] [-t* surface] \n"
"         Surfaces specified on command line are place in a group\n"
"         called 'DefGroup'.\n"
"         If you specify nothing on command line, you will have a random\n"
"         surface created for you. Some of these surfaces are generated\n"
"         using Thomas Lewiner's sample volumes for creating isosurfaces.\n"
"         See suma -sources for a complete reference.\n"
"\n"
"%s"
"\n"
" Modes 1 & 2: You can mix the two modes for loading surfaces but the -sv\n"
"              option may not be properly applied.\n"    
"              If you mix these modes, you will have two groups of\n"
"              surfaces loaded into SUMA. You can switch between them\n"
"              using the 'Switch Group' button in the viewer controller.\n" 
"\n"
"%s"
/*"   [-iodbg] Trun on the In/Out debug info from the start.\n"
"   [-memdbg] Turn on the memory tracing from the start.\n" */    
"   [-visuals] Shows the available glxvisuals and exits.\n"
"   [-brethren_windows] For Testing Only. Show a listing of windows possibly \n"
"                       related to AFNI and SUMA.\n" 
"   [-version] Shows the current version number.\n"
"   [-environment] Shows a list of all environment variables, \n"
"                  their default setting and your current setting.\n"
"                  The output can be used as a new .sumarc file.\n"
"                  Since it takes into consideration your own settings\n"
"                  this command can be used to update your .sumarc \n"
"                  regularly with a csh command like this:\n"
"\n"
"           suma -environment > ~/sumarc && \\\n"
"             cp ~/.sumarc ~/.sumarc-bak ; \\\n"
"             mv ~/sumarc ~/.sumarc\n" 
"\n"
"\n"
"   [-drive_com DRIVE_SUMA_COM]: Drive suma with command DRIVE_SUMA_COM,\n"
"            which has the same syntax that you would use for DriveSuma.\n"
"            For instance:\n"
"\n"
"            suma -i ld120 -drive_com '-com surf_cont -view_surf_cont y'\n"      
"            or \n"
"            suma -drive_com '-com viewer_cont -key 'F12' -com kill_suma'\n"
"\n"
"            You can use repeated instances of -drive_com to have a series\n"
"            of commands that get executed in the order in which they appear\n"
"            on the command line.\n"
"\n"
"\n"
"%s", 
       (detail > 1) ? ssym:"     use -help for more detail on loading template surfaces with symbolic notation\n" ,
       (detail > 1) ? sio:"     use -help for more detail on input options\n" , 
       (detail > 1) ? sb:"     use -help for more detail on basic options\n", 
       (detail > 1) ? get_np_help():
                  "     use -help for more detail on communication ports\n");
   
   if (detail > 1) { printf(
"-help_interactive: Write the help for interactive usage into file\n"
"                   Mouse_Keyboard_Controls.txt\n"
"-help_sphinx_interactive HOUT: Write the help for interactive usage into \n"
"                   SPHINX formatted file HOUT"
"See DriveSuma's -write_*_help options for more\n"
"-test_help_string_edit: Show example of help string editing and quit\n"
"-test_help_string_edit_web: Like its prefix, but nicer for webpage.\n");
   }
   
   if (detail > 1) { printf(
"   [-list_ports]  List all port assignments and quit\n"
"   [-port_number PORT_NAME]: Give port number for PORT_NAME and quit\n"
"   [-port_number_quiet PORT_NAME]: Same as -port_number but writes out \n"
"                                    number only\n"
"   [-dev]: Allow access to options that are not well polished for\n"
"           mass consuption.\n"   
"   [-fake_cmap]: Use X11 to render cmap. This is only needed to get colorbar\n"
"                 to appear when the frame is automatically captured by SUMA\n"
"                 for making documentation. This option has no other use.\n"
"   [-update_env] Performs the set operations detailed under -environment\n"
"   [-default_env] Output hard coded default environment values, ignoring\n"
"                  user settings.\n"
"   [-latest_news] Shows the latest news for the current \n"
"                  version of the entire SUMA package.\n"
"   [-all_latest_news] Shows the history of latest news.\n"
"   [-progs] Lists all the programs in the SUMA package.\n"
"   [-motif_ver] Displays the linked version of Motif.\n"
"   [-sources] Lists code sources used in parts of SUMA.\n"
"   [-help_nido] Help message for displayable objects of type NIDO\n"
"\n"
"   For help on interacting with SUMA, press 'ctrl+h' with the mouse \n"
"   pointer inside SUMA's window.\n"     
"   For more help: https://afni.nimh.nih.gov/pub/dist/edu/latest/suma/suma.pdf\n"
"\n"     
"   If you can't get help here, please get help somewhere.\n"
      ); } 
   }
   SUMA_free(sb); SUMA_free(sio); SUMA_free(ssym);
   if (detail) {
      SUMA_Version(NULL);
      printf ("\n" 
            "\n    Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov \n\n");
   }
          return;
  }/*Usage*/
     


/*!
    a function to return some surface objects for SUMA to work with 
    
    surfaces are added to SUMAg_DOv so let them be freed there....
*/
SUMA_SurfaceObject **SUMA_GimmeSomeSOs(int *N_SOv) 
{
   static char FuncName[]={"SUMA_GimmeSomeSOs"};
   SUMA_SurfaceObject **SOv=NULL, *SO=NULL;
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;
   char sid[100];
   int i, N_k, k, *ilist=NULL, nhjs;
   float *vlist=NULL; 
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   Opt = (SUMA_GENERIC_PROG_OPTIONS_STRUCT *)
            SUMA_calloc(1,sizeof(SUMA_GENERIC_PROG_OPTIONS_STRUCT));

   N_k = 13; /* Think of this number as the number of states, 
               rather than individual surfaces
               10 from isosurface (actually 9, number 6 is removed), 
               1 from HJS collection
               1 from head collection
            */ 
   vlist = (float*)SUMA_calloc(N_k, sizeof(float));
   srand((unsigned int)time(NULL));
   for (i=0; i<N_k; ++i) {
      vlist[i] = rand();
   }
   ilist = SUMA_z_qsort(vlist, N_k);
   
   /* remove six from ilist, bad surface ... */
   for (i=0; i<N_k; ++i) if (ilist[i] == 6) ilist[i] = ilist[N_k-1];
   N_k = N_k - 1; /* remove last one since it replaced 6 */
   
   nhjs=0;
   *N_SOv = 0; 
   i=-1;
   /*       Sequence below, coupled with the use of rygbr20 colormap was 
            necessary in reproducing a crash 
            Crash was likely cause by uninitialized mcb->_case in MarchingCubes.c               ZSS: Oct 06 
      ilist[0]=1;    ilist[1]=4;    ilist[2]=8;    ilist[3]=2;    ilist[4]=10;
       ilist[5]=5;    ilist[6]=0;    ilist[7]=9;    ilist[8]=3;    ilist[9]=7; */
   for (k=0; k<N_k; ++k) {
      if (LocalHead) fprintf(SUMA_STDERR,"ilist[%d]=%d    ", k, ilist[k]);
      if (ilist[k] <= 9) { /* 0 to 9 is code for MarchingCubesSurfaces */
         Opt->obj_type = ilist[k];
         Opt->obj_type_res = 64;
         Opt->debug =0;
         Opt->in_vol =0;
         Opt->mcfv= NULL;
         if ((SO = SUMA_MarchingCubesSurface(Opt))) {
            ++*N_SOv; 
            SOv = (SUMA_SurfaceObject **) 
                     SUMA_realloc(SOv, (*N_SOv)*sizeof(SUMA_SurfaceObject *));
            SOv[*N_SOv-1]=SO;
            /* assign its Group and State and Side and few other things, must 
               look like surfaces loaded with SUMA_Load_Spec_Surf*/
            SOv[*N_SOv-1]->Group = SUMA_copy_string(SUMA_DEF_TOY_GROUP_NAME); 
                  /* change this in sync with string in macro 
                     SUMA_BLANK_NEW_SPEC_SURF*/
            sprintf(sid, "%s_%d", SUMA_DEF_STATE_NAME, Opt->obj_type);
            SOv[*N_SOv-1]->State = SUMA_copy_string(sid);
            sprintf(sid, "surf_%d", Opt->obj_type);
            SOv[*N_SOv-1]->Label = SUMA_copy_string(sid);
            SOv[*N_SOv-1]->EmbedDim = 3;
            SOv[*N_SOv-1]->AnatCorrect = YUP;
            /* make this surface friendly for suma */
            if (!SUMA_PrepSO_GeomProp_GL(SOv[*N_SOv-1])) {
               SUMA_S_Err("Failed in SUMA_PrepSO_GeomProp_GL");
               SUMA_RETURN(NULL);
            }
            /* Add this surface to SUMA's displayable objects */
            if (!SUMA_PrepAddmappableSO(SOv[*N_SOv-1], SUMAg_DOv, &(SUMAg_N_DOv),                                         0, SUMAg_CF->DsetList)) {
               SUMA_S_Err("Failed to add mappable SOs ");
               SUMA_RETURN(NULL);
            }
         }
      } else if (ilist[k] == 10) {  /* 10 is code for HJS */
         /* HJS's turn */
         for (nhjs=0; nhjs < 19; ++nhjs) { 
            ++*N_SOv; 
            SOv = (SUMA_SurfaceObject **) 
                        SUMA_realloc(SOv, (*N_SOv)*sizeof(SUMA_SurfaceObject *));
            SOv[*N_SOv-1] = SUMA_HJS_Surface(nhjs);
            /* assign its Group and State and Side and few other things, must 
               look like surfaces loaded with SUMA_Load_Spec_Surf*/
            SOv[*N_SOv-1]->Group = SUMA_copy_string(SUMA_DEF_TOY_GROUP_NAME); 
                        /* change this in sync with string in macro 
                           SUMA_BLANK_NEW_SPEC_SURF*/
            sprintf(sid, "H.J.S.");
            SOv[*N_SOv-1]->State = SUMA_copy_string(sid);
            sprintf(sid, "H.J.S._%d", nhjs);
            SOv[*N_SOv-1]->Label = SUMA_copy_string(sid);
            SOv[*N_SOv-1]->EmbedDim = 3;
            SOv[*N_SOv-1]->AnatCorrect = YUP;
            /* make this surface friendly for suma */
            if (!SUMA_PrepSO_GeomProp_GL(SOv[*N_SOv-1])) {
               SUMA_S_Err("Failed in SUMA_PrepSO_GeomProp_GL");
               SUMA_RETURN(NULL);
            }
            /* Add this surface to SUMA's displayable objects */
            if (!SUMA_PrepAddmappableSO(SOv[*N_SOv-1], SUMAg_DOv, &(SUMAg_N_DOv),
                                        0, SUMAg_CF->DsetList)) {
               SUMA_S_Err("Failed to add mappable SOs ");
               SUMA_RETURN(NULL);
            }
         }
      } else if (ilist[k] == 11) {  /* 11 is code for head */
         if ((SO = SUMA_head_01_surface())) {
            ++*N_SOv; 
            SOv = (SUMA_SurfaceObject **) 
                     SUMA_realloc(SOv, (*N_SOv)*sizeof(SUMA_SurfaceObject *));
            SOv[*N_SOv-1]=SO;
            /* assign its Group and State and Side and few other things, must 
               look like surfaces loaded with SUMA_Load_Spec_Surf*/
            SOv[*N_SOv-1]->Group = SUMA_copy_string(SUMA_DEF_TOY_GROUP_NAME); 
                  /* change this in sync with string in macro 
                     SUMA_BLANK_NEW_SPEC_SURF*/
            SOv[*N_SOv-1]->State = SUMA_copy_string("head_01");
            SOv[*N_SOv-1]->Label = SUMA_copy_string("La_Tete");
            SOv[*N_SOv-1]->EmbedDim = 3;
            SOv[*N_SOv-1]->AnatCorrect = YUP;
            /* make this surface friendly for suma */
            if (!SUMA_PrepSO_GeomProp_GL(SOv[*N_SOv-1])) {
               SUMA_S_Err("Failed in SUMA_PrepSO_GeomProp_GL");
               SUMA_RETURN(NULL);
            }
            /* Add this surface to SUMA's displayable objects */
            if (!SUMA_PrepAddmappableSO(SOv[*N_SOv-1], SUMAg_DOv, &(SUMAg_N_DOv),                                         0, SUMAg_CF->DsetList)) {
               SUMA_S_Err("Failed to add mappable SOs ");
               SUMA_RETURN(NULL);
            }
         }
      } else if (ilist[k] == 12) { /* 12 is code for cube */
         if ((SO = SUMA_cube_surface(100, NULL))) {
            ++*N_SOv; 
            SOv = (SUMA_SurfaceObject **) 
                     SUMA_realloc(SOv, (*N_SOv)*sizeof(SUMA_SurfaceObject *));
            SOv[*N_SOv-1]=SO;
            /* assign its Group and State and Side and few other things, must 
               look like surfaces loaded with SUMA_Load_Spec_Surf*/
            SOv[*N_SOv-1]->Group = SUMA_copy_string(SUMA_DEF_TOY_GROUP_NAME); 
                  /* change this in sync with string in macro 
                     SUMA_BLANK_NEW_SPEC_SURF*/
            SOv[*N_SOv-1]->State = SUMA_copy_string("Cube100");
            SOv[*N_SOv-1]->Label = SUMA_copy_string("Le_Cube");
            SOv[*N_SOv-1]->EmbedDim = 3;
            SOv[*N_SOv-1]->AnatCorrect = YUP;
            /* make this surface friendly for suma */
            if (!SUMA_PrepSO_GeomProp_GL(SOv[*N_SOv-1])) {
               SUMA_S_Err("Failed in SUMA_PrepSO_GeomProp_GL");
               SUMA_RETURN(NULL);
            }
            /* Add this surface to SUMA's displayable objects */
            if (!SUMA_PrepAddmappableSO(SOv[*N_SOv-1], SUMAg_DOv, &(SUMAg_N_DOv),                                         0, SUMAg_CF->DsetList)) {
               SUMA_S_Err("Failed to add mappable SOs ");
               SUMA_RETURN(NULL);
            }
         }
      } else {
         SUMA_S_Errv("Bad ilist number: ilist[%d]=%d\n", k, ilist[k]);
         break;
      }
      if (LocalHead) SUMA_Print_Surface_Object(SOv[*N_SOv-1], stderr);
   }
  
   if (Opt) SUMA_free(Opt);
   if (ilist) SUMA_free(ilist);
   if (vlist) SUMA_free(vlist);
   
   SUMA_RETURN(SOv);
}

/*!\**
File : SUMA.c
\author : Ziad Saad
Date : Thu Dec 27 16:21:01 EST 2001
   
Purpose : 
   
   
   
Input paramters : 
\param   
\param   
   
Usage : 
      SUMA ( )
   
   
Returns : 
\return   
\return   
   
Support : 
\sa   OpenGL prog. Guide 3rd edition
\sa   varray.c from book's sample code
   
Side effects : 
   
   
   
***/
int main (int argc,char *argv[])
{/* Main */
   static char FuncName[]={"suma"}; 
   int kar, i;
   SUMA_SFname *SF_name;
   SUMA_Boolean brk, SurfIn;
   char  *NameParam, *AfniHostName = NULL, *s = NULL, *pdspec=NULL, *pdsv=NULL;
   char *specfilename[SUMA_MAX_N_GROUPS], *VolParName[SUMA_MAX_N_GROUPS];
   byte InMem[SUMA_MAX_N_GROUPS];
   SUMA_SurfSpecFile *Specp[SUMA_MAX_N_GROUPS];   
   SUMA_Axis *EyeAxis;    
   SUMA_EngineData *ED= NULL;
   DList *list = NULL;
   DListElmt *Element= NULL;
   int iv15[15], N_iv15, ispec, nspec;
   struct stat stbuf;
   float fff=0.0;
   int Start_niml = 0;
   SUMA_Boolean  Domemtrace = YUP;
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
    
   SUMA_STANDALONE_INIT;
   SUMA_mainENTRY;
   
   
   SUMAg_CF->isGraphical = YUP;
   
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-dset;-do;");

   /* initialize Volume Parent and AfniHostName to nothing */
   for (ispec=0; ispec < SUMA_MAX_N_GROUPS; ++ispec) {
      specfilename[ispec] = NULL;
      VolParName[ispec] = NULL;
      Specp[ispec] = NULL;
      InMem[ispec] = 0;
   }
   AfniHostName = NULL; 
   
      
   /* Allocate space for DO structure */
   SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   
   /* call the function to parse the other surface mode inputs */
   ispec = 0;
   if (LocalHead) SUMA_Show_IO_args(ps);
   if (ps->i_N_surfnames || ps->t_N_surfnames || ps->N_DO) {
      SUMA_LH("-i and/or -t surfaces on command line!");
      Specp[ispec] = SUMA_IO_args_2_spec (ps, &nspec); 
      if (Specp[ispec]) {
         ++ispec;
         if (nspec != 1) {
            SUMA_S_Errv("-spec is being parsed separately here, "
                        "expecting one spec only from SUMA_IO_args_2_spec, \n"
                        "got %d\n", nspec);
            exit (1);
         }
      } else {
         SUMA_S_Err("Failed to load -i/-t surfaces");
         exit(1);
      }
      
   }
   /* Work the options */
   kar = 1;
   brk = NOPE;
   SurfIn = NOPE;
   Domemtrace = YUP; 
   while (kar < argc) { /* loop accross command ine options */
      /*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      
      if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
         SUMA_usage (ps, strlen(argv[kar]) > 3 ? 2:1);
          exit (0); /* return a good status on -help   12 Jul 2013 [rickr] */
      }
      
      /* -list_ports list and quit */
      if( strncmp(argv[kar],"-list_ports", 8) == 0) {
         show_ports_list(); exit(0);
      }
      
      /* -port_number and quit */
      if( strncmp(argv[kar],"-port_number", 8) == 0) {
         int pp = 0;
         if( ++kar >= argc ) 
            ERROR_exit("need an argument after -port_number!"); 
         pp = get_port_named(argv[kar]);
         if (strcmp(argv[kar-1], "-port_number_quiet")) { 
            fprintf(stdout, "\nPort %s: %d\n", argv[kar], pp); 
         } else {
            fprintf(stdout, "%d\n", pp); 
         }
         if (pp < 1) exit(1);
         else exit(0);
      }
      
      if (strcmp(argv[kar], "-visuals") == 0) {
          SUMA_ShowAllVisuals ();
          exit (0);
      }
      
      if (strcmp(argv[kar], "-brethren_windows") == 0) {
          Display *dd=NULL; Window ww;
          if (!(dd = XOpenDisplay(NULL))) {
            SUMA_S_Err("No display "); exit(1);
          }
          ww = XDefaultRootWindow(dd);
          
          SUMA_WindowsOnRootDisplay(dd, ww , 0);
          exit (0);
      }
      
      if (strcmp(argv[kar], "-version") == 0) {
          s = SUMA_New_Additions (0.0, 1);
          fprintf (SUMA_STDOUT,"%s\n", s); 
          SUMA_free(s); s = NULL;
          exit (0);
      }
      
      if (strcmp(argv[kar], "-sources") == 0) {
          s = SUMA_sources_Info();
          fprintf (SUMA_STDOUT,"%s\n", s); 
          SUMA_free(s); s = NULL;
          exit (0);
      }
      
      if (strcmp(argv[kar], "-help_nido") == 0) {
         s = SUMA_NIDO_Info();
         fprintf (SUMA_STDOUT,"%s\n", s); 
         SUMA_free(s); s = NULL;
         exit (0);
      }
      
      if (strcmp(argv[kar], "-all_latest_news") == 0) {
          s = SUMA_New_Additions (-1.0, 0);
          fprintf (SUMA_STDOUT,"%s\n", s); 
          SUMA_free(s); s = NULL;
          exit (0);
      }
      
      if (strcmp(argv[kar], "-help_sphinx_interactive") == 0) {
         FILE *fout = NULL;
         if( ++kar >= argc ) 
            ERROR_exit("need a file name after -help_sphinx_interactive!");       
          fout = fopen(argv[kar],"w");
          if (!fout) {
            SUMA_S_Err("Failed to open %s for writing", argv[kar]);
            exit(1);
          }
          SUMA_help_message(fout,SPX);
          fclose(fout); fout = NULL;
          exit (0);
      }
      
      if (strcmp(argv[kar], "-help_interactive") == 0) {
          FILE *fout = fopen("Mouse_Keyboard_Controls.txt","w");
          if (!fout) {
            SUMA_S_Err("Failed to open Mouse_Keyboard_Controls.txt for writing");
            exit(1);
          }
          SUMA_help_message(fout,TXT);
          fclose(fout); fout = NULL;
          exit (0);
      }
      
      if (strcmp(argv[kar], "-test_help_string_edit") == 0) {
         SUMA_Sphinx_String_Edit_Help(SUMA_STDOUT, 0);
         exit(0);
      }
      if (strcmp(argv[kar], "-test_help_string_edit_web") == 0) {
         SUMA_Sphinx_String_Edit_Help(SUMA_STDOUT, 1);
         exit(0);
      }
      
      if (strcmp(argv[kar], "-environment") == 0) {
          s = SUMA_env_list_help (0, TXT);
          fprintf (SUMA_STDOUT,  
            "#SUMA ENVIRONMENT \n"
            "# If you do not have a ~/.sumarc file, cannot find a SUMA\n"
            "# environment variable that's been mentioned in documentation,\n"
            "# or fervently desire to update your current ~/.sumarc with  \n"
            "# all the latest variables that SUMA uses, you should run: \n"
            "# \n"
            "#    suma -update_env\n"
            "# \n"
            "# Unless you have setup SUMA environment variables outside of\n"
            "# your ~/.sumarc file, updating your ~/.sumarc file with \n"
            "# 'suma -update_env' WILL NOT ALTER changes you have already\n"
            "# made to the variables in your current ~/.sumarc. \n"
            "# For this reason consider running the update command after each \n"
            "# upgrade of your AFNI/SUMA binaries.\n" 
            "***ENVIRONMENT\n"
                  "%s\n", s); 
          SUMA_free(s); s = NULL;
          exit (0);
      }
      
      if (strcmp(argv[kar], "-default_env") == 0) {
          s = SUMA_env_list_help (1, NO_FORMAT);
          fprintf (SUMA_STDOUT,  
                  "#SUMA DEFAULT ENVIRONMENT (user settings ignored)\n"
                  "# see also suma -udate_env or suma -environment\n"
                  "# \n"
                  "***ENVIRONMENT\n"
                  "%s\n", s); 
          SUMA_free(s); s = NULL;
          exit (0);
      }
      
      if (strcmp(argv[kar], "-update_env") == 0) {
          if (system("suma -environment > ___sumarc")) {
            SUMA_S_Err("Failed to create env file.");
            exit(1);
          }
          if (SUMA_filexists("~/.sumarc")) {
            if (system("\\cp -f ~/.sumarc ~/.sumarc-bak")) {
               SUMA_S_Err("Failed to backup ~/.sumarc to ~/.sumarc-bak.");
               exit(1);
            }
          }
          if (system("\\mv ___sumarc ~/.sumarc")) {
            SUMA_S_Err("Failed to copy newrc (___sumarc) to ~/.sumarc");
            exit(1); 
          }
          SUMA_S_Note("Environment update done.");
          exit(0);
      }
      
      if (strcmp(argv[kar], "-latest_news") == 0) {
          s = SUMA_New_Additions (0.0, 0);
          fprintf (SUMA_STDOUT,"%s\n", s); 
          SUMA_free(s); s = NULL;
          exit (0);
      }
      
      if (strcmp(argv[kar], "-progs") == 0) {
          s = SUMA_All_Programs();
          fprintf (SUMA_STDOUT,"%s\n", s); 
          SUMA_free(s); s = NULL;
          exit (0);
      }
      
      if (strcmp(argv[kar], "-motif_ver") == 0) {  /* 9 Mar 2009 [rickr] */
         show_motif_version_string();
         exit (0);
      }
      
      if (!brk && (strcmp(argv[kar], "-iodbg") == 0)) {
         fprintf(SUMA_STDERR,"Error %s: Obsolete, use -trace\n", FuncName);
         exit (0);
         /*
         fprintf(SUMA_STDOUT,
                 "Warning %s: SUMA running in in/out debug mode.\n", FuncName);
         SUMA_INOUT_NOTIFY_ON; 
         brk = YUP;
         */
      }
      
      
      
      SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      #if SUMA_MEMTRACE_FLAG
         if (!brk && (strcmp(argv[kar], "-memdbg") == 0)) {
            fprintf(SUMA_STDOUT,"Error %s: -memdbg is obsolete, use -trace\n", 
                                FuncName);
            exit (0);
            fprintf( SUMA_STDOUT,
                     "Warning %s: SUMA running in memory trace mode.\n", 
                     FuncName);
            SUMAg_CF->MemTrace = YUP;
            #ifdef USING_MCW_MALLOC
            #endif
            brk = YUP;
         }
      #endif
      
      if (!brk && (strcmp(argv[kar], "-dev") == 0)) {
         fprintf(SUMA_STDOUT,
                  "Warning %s: SUMA running in developer mode, "
                  "some options may malfunction.\n", FuncName);
         SUMAg_CF->Dev = YUP;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-fake_cmap") == 0)) {
         SUMA_S_Warn("-fake_cmap is for automatic selfies of the widgets.\n"
                     "You should not use this option for any other reason\n");
         SUMAg_CF->Fake_Cmap = YUP;
         brk = YUP;
      }
      
      if (!brk && SUMAg_CF->Dev && (strcmp(argv[kar], "-truth_table") == 0)) {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need expression after -truth_table \n");
            exit (1);
         }
         SUMA_bool_eval_truth_table(argv[kar], 0);  exit(0);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-niml") == 0)) {
         Start_niml = 1;
         brk = YUP;
      }

      if (!brk && (strcmp(argv[kar], "-noniml") == 0)) {
         Start_niml = -1;
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-vp") == 0 || 
                   strcmp(argv[kar], "-sa") == 0 || 
                   strcmp(argv[kar], "-sv") == 0))
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -vp|-sa|-sv \n");
            exit (1);
         }
         if (ispec < 1) {
            fprintf (SUMA_STDERR, 
                     "a -spec option must precede the first -sv option\n");
            exit (1);
         }
         if (!specfilename[ispec-1] && !Specp[ispec-1]) {
            fprintf (SUMA_STDERR, 
                     "a -spec option must precede each -sv option\n");
            exit (1);
         }
         VolParName[ispec-1] = argv[kar]; 
         if (LocalHead) {
            fprintf(SUMA_STDOUT, "Found: %s\n", VolParName[ispec]);
         }
         
         brk = YUP;
      }      
      
      if (!brk && strcmp(argv[kar], "-drive_com") == 0)
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -drive_com\n");
            exit (1);
         }
         SUMAg_CF->dcom = (char **)SUMA_realloc(SUMAg_CF->dcom,
                                          (SUMAg_CF->N_dcom+1)*sizeof(char *));
         SUMAg_CF->dcom[SUMAg_CF->N_dcom] = SUMA_copy_string(argv[kar]);
         ++SUMAg_CF->N_dcom;
         brk = YUP;
      }
      
      if (!brk && strcmp(argv[kar], "-ah") == 0)
      {
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -ah\n");
            exit (1);
         }
         if (strcmp(argv[kar],"localhost") != 0) {
            AfniHostName = argv[kar];
         }else {
           fprintf (SUMA_STDERR, 
                    "localhost is the default for -ah\n"
                    "No need to specify it.\n");
         }
         /*fprintf(SUMA_STDOUT, "Found: %s\n", AfniHostName);*/

         brk = YUP;
      }   
      
      if (!brk && strcmp(argv[kar], "-spec") == 0)
      { 
         kar ++;
         if (kar >= argc)  {
              fprintf (SUMA_STDERR, "need argument after -spec \n");
            exit (1);
         }
         
         if (ispec >= SUMA_MAX_N_GROUPS) {
            fprintf (SUMA_STDERR, 
                     "Cannot accept more than %d spec files.\n",     
                     SUMA_MAX_N_GROUPS);
            exit(1);
         }
         
         if (SUMA_is_predefined_SO_name(argv[kar], NULL, 
                                        &pdspec, &pdsv, NULL) == 3) {
            specfilename[ispec] = pdspec; pdspec = NULL; /* Memory leak! */
            VolParName[ispec] = pdsv; pdsv = NULL; /* Memory leak! */
         } else {
            specfilename[ispec] = argv[kar]; 
         }
         if (LocalHead) {
            fprintf(SUMA_STDOUT, "Found: %s\n", specfilename[ispec]);
         }
         ++ispec;
         brk = YUP;
      } 
      
      
      if (!brk && !ps->arg_checked[kar]) {
         if (  !strcmp(argv[kar], "-i") ||
               !strncmp(argv[kar], "-i_",3) ) {
            fprintf (SUMA_STDERR,
      "Error %s: Option %s not understood. \n"
      "  Make sure parameter after -i or -i_ is the full name of a surface.\n"
      "%s",
      FuncName, argv[kar], 
      strlen(argv[kar])==2 ? 
         "For -i to work, SUMA needs to guess at the surface type from\n"
         "  the filename extensions. If SUMA fails try the full -i_* option"
         " instead.\n" : ""
      );
         } else {
            fprintf (SUMA_STDERR,
                  "Error %s: Option %s not understood. Try -help for usage\n", 
                  FuncName, argv[kar]);
            suggest_best_prog_option(argv[0], argv[kar]);
         }
         exit (1);
      } else {   
         brk = NOPE;
         kar ++;
      }
      
   }/* loop accross command ine options */
   /* -ah option now checked for in ps */
   if (ps->cs->afni_host_name && !AfniHostName) {
      AfniHostName = SUMA_copy_string(ps->cs->afni_host_name);
   }
   
   #if 0
   SUMA_S_Note("KILL ME");
   { 
      int i,j, nl; 
      SUMA_TextBoxSize("Hello", &i,&j,&nl,NULL); 
      SUMA_TextBoxSize("", 
                        &i,&j,&nl,GLUT_BITMAP_8_BY_13); 
      SUMA_TextBoxSize("O", 
                        &i,&j,&nl,GLUT_BITMAP_8_BY_13); 
                        SUMA_TextBoxSize(NULL, 
                        &i,&j,&nl,GLUT_BITMAP_8_BY_13); 
   }
   SUMA_ReadNIDO("/Users/ziad/SUMA_test_dirs/DO/TextDO/sample.niml.do", NULL);   
   exit(1);
      
   #endif
      
   /* Make surface loading pacifying */
   SetLoadPacify(1);
   
   #if 0
   if (ps->N_DO) { /* Have DOs on command line */
      if (Specp[0]) { /* Add to Specp[0] */
         if (ps->N_DO + Specp[0]->N_DO > SUMA_MAX_DO_SPEC) {
            SUMA_S_Warn("Too many DOs, increase static limit..");
                                       /* ignore extras for now */
            ps->N_DO = SUMA_MAX_DO_SPEC - Specp[0]->N_DO;
         }
         for (i=0; i<ps->N_DO; ++i) {
            strcpy(Specp[0]->DO_name[Specp[0]->N_DO], ps->DO_name[i]);
            Specp[0]->DO_type[Specp[0]->N_DO] = ps->DO_type[i];
            ++Specp[0]->N_DO;
         }
      } else {
         Specp[0]
      }
   }
   #endif
      
   /* any Specp to be found ?*/
   if (specfilename[0] == NULL && Specp[0] == NULL) {
      SUMA_SurfaceObject **SOv=NULL;
      int N_SOv = 0;
      fprintf (SUMA_STDERR,
               "\n"
               "%s: \n"
               "     No input specified, loading some toy surfaces...\n"
               "     Use '.' and ',' to cycle between them.\n"
               "     See suma -help for assistance.\n"
               "\n", FuncName);
      /* create your own surface and put it in a spec file */
      SOv = SUMA_GimmeSomeSOs(&N_SOv);
      Specp[ispec] = SUMA_SOGroup_2_Spec (SOv, N_SOv);
      SUMA_free(SOv); SOv = NULL;
      InMem[ispec] = 1;
      ++ispec;
   }

   if(!SUMA_Assign_HostName (SUMAg_CF, AfniHostName, -1)) {
      fprintf (SUMA_STDERR, 
         "Error %s: Failed in SUMA_Assign_HostName\n", FuncName);
      exit (1);
   }
   
   #ifdef SUMA_DISASTER
   /* a function to test Memtracing */
   {
      int *jnk;
      jnk = SUMA_disaster();
      SUMA_free(jnk); /* without the -trace, you'll get a 
                           warning here if jnk is corrupted */
   }
   #endif
   
   /* create an Eye Axis DO */
   EyeAxis = SUMA_Alloc_Axis ("Eye Axis", AO_type);
   if (EyeAxis == NULL) {
      SUMA_error_message (FuncName,"Error Creating Eye Axis",1);
      exit(1);
   }

   /* Store it into SUMAg_DOv */
   if (!SUMA_AddDO(  SUMAg_DOv, &SUMAg_N_DOv, 
                     (void *)EyeAxis,  AO_type, SUMA_SCREEN)) {
      SUMA_error_message (FuncName,"Error Adding DO", 1);
      exit(1);
   }
   /*fprintf (SUMA_STDERR, "SUMAg_N_DOv = %d created\n", SUMAg_N_DOv);
   SUMA_Show_DOv(SUMAg_DOv, SUMAg_N_DOv, NULL);*/

   /* Allocate space (and initialize) Surface Viewer Structure */
   SUMAg_SVv = SUMA_Alloc_SurfaceViewer_Struct (SUMA_MAX_SURF_VIEWERS);
   
   /* SUMAg_N_SVv gets updated in SUMA_X_SurfaceViewer_Create
   and reflects not the number of elements in SUMAg_SVv which is
   SUMA_MAX_SURF_VIEWERS, but the number of viewers that were realized
   by X */
   
   /* Check on initialization */
   /*SUMA_Show_SurfaceViewer_Struct (SUMAg_cSV, stdout);*/

   /* Create the Surface Viewer Window */
   if (!SUMA_X_SurfaceViewer_Create ()) {
      fprintf(stderr,"Error in SUMA_X_SurfaceViewer_Create. Exiting\n");
      return 1;
   }
   
   for (i=0; i<ispec; ++i) {
      if (!list) list = SUMA_CreateList();
      ED = SUMA_InitializeEngineListData (SE_Load_Group);
      if (!( Element = SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_cp, (void *)specfilename[i], 
                                             SES_Suma, NULL, NOPE, 
                                             SEI_Head, NULL ))) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
         exit (1);
      }
      if (!( Element = SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_ip, (void *)Specp[i], 
                                             SES_Suma, NULL, NOPE, 
                                             SEI_In, Element ))) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
         exit (1);
      }
      fff = (float) InMem[i];
      if (!( Element = SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_f, (void *)&fff, 
                                             SES_Suma, NULL, NOPE, 
                                             SEI_In, Element ))) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
         exit (1);
      }
      if (!( Element = SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_vp, (void *)VolParName[i], 
                                             SES_Suma, NULL, NOPE, 
                                             SEI_In, Element ))) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
         exit (1);
      }

      N_iv15 = SUMA_MAX_SURF_VIEWERS;
      if (N_iv15 > 15) {
         fprintf( SUMA_STDERR,
                  "Error %s: trying to register more than 15 viewers!\n", 
                  FuncName);
         exit(1);
      }
      for (kar=0; kar<N_iv15; ++kar) iv15[kar] = kar;
      if (!( Element = SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_iv15, (void *)iv15, 
                                             SES_Suma, NULL, NOPE, 
                                             SEI_In, Element ))) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
         exit (1);
      }

      if (!( Element = SUMA_RegisterEngineListCommand (  list, ED, 
                                             SEF_i, (void *)&N_iv15, 
                                             SES_Suma, NULL, NOPE, 
                                             SEI_In, Element ))) {
         fprintf(SUMA_STDERR,"Error %s: Failed to register command\n", FuncName);
         exit (1);
      }
   }
   
   if (ispec > 0 && !SUMA_Engine (&list)) {
      fprintf(SUMA_STDERR,"Error %s: Failed in SUMA_Engine\n", FuncName);
      exit (1);
   }
   
   /* For some reason, I had to add the glLightfv line below
   to force the lightflipping done in SUMA_SetupSVforDOs to take place
   in the A viewer when first opened. I don't know why that is, especially
   since other controllers would show up lit correctly without this 
   glLightfv line below.
      To make matters worse, the A controller's light0_position is correctly 
   flipped.
      It is just that the shading is done as if the position was never flipped. 
   Actually, without the line below, the first time you hit the F key (to 
   manually flip the light), nothing changes, that's because the light's position    is unflipped, which is supposed to show the incorrect lighting. 
   You'll have to hit F again to have the lighting correctly flipped 
   and the shading reflecting it.... ZSS, Aug. 05 04 */
   glLightfv(GL_LIGHT0, GL_POSITION, SUMAg_SVv[0].light0_position); 

   if (Start_niml != -1 && (Start_niml == 1|| AFNI_yesenv("SUMA_START_NIML"))) {
      if (!list) list = SUMA_CreateList();
      SUMA_REGISTER_HEAD_COMMAND_NO_DATA( list, SE_StartListening, 
                                          SES_Suma, NULL);

      if (!SUMA_Engine (&list)) {
         fprintf(SUMA_STDERR, "Error %s: SUMA_Engine call failed.\n", FuncName);
         exit (1);   
      }
   }
   
   /* load the datasets onto the first SO, if any, else hope that dset 
      is some form of DO  */
   if (ps->N_dsetname>0) {
      SUMA_SurfaceObject *SO = SUMA_findanySOp_inDOv(SUMAg_DOv, 
                                                     SUMAg_N_DOv, NULL);
      if (!SO) {
         SUMA_LH("Could not find any SO, here is hoping dset is a DO");
      }
      for (i=0; i<ps->N_dsetname; ++i) {
         if (!(SUMA_LoadDsetOntoSO_eng(ps->dsetname[i], SO, 1, 1, 1, NULL))) {
            SUMA_S_Errv("Failed to load %s onto %s\n", 
                        ps->dsetname[i], SO?SO->Label:"NULL");
         }
      }
   }

   SUMA_FreeGenericArgParse(ps); ps = NULL;
 
   /* A Warning about no sumarc */
   if (NoSumaRcFound()) { 
         SUMA_S_Warn(
"\n"
" No sumarc file found. You should create one by running the following:\n"
"\n"
"              suma -update_env\n"
"\n"
" I also recommend you run 'suma -update_env' whenever you update AFNI.\n" 
"\n"
" See details for -environment and -update_env options in suma -help's output.\n"
"\n");
   }
   
   /*Main loop */
   XtAppMainLoop(SUMAg_CF->X->App);

   
   /* Done, clean up time */
   if (ispec) {
      int k=0; 
      for (k=0; k<ispec; ++k) {
         if (!SUMA_FreeSpecFields((Specp[k]))) { 
            SUMA_S_Err("Failed to free spec fields"); 
         } 
         Specp[k] = NULL;
      }
   } ispec = 0;
  
   if (!SUMA_Free_Displayable_Object_Vect (SUMAg_DOv, SUMAg_N_DOv)) 
      SUMA_error_message(FuncName,"DO Cleanup Failed!",1);
   if (!SUMA_Free_SurfaceViewer_Struct_Vect (SUMAg_SVv, SUMA_MAX_SURF_VIEWERS)) 
      SUMA_error_message(FuncName,"SUMAg_SVv Cleanup Failed!",1);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) 
      SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
  SUMA_RETURN(0);             /* ANSI C requires main to return int. */
}/* Main */ 


