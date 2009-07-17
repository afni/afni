/*USE This sample to start writing standalone programs.
Change DriveSuma to the program name of your choosing.
*/
#include "SUMA_suma.h"

SUMA_SurfaceViewer *SUMAg_cSV = NULL; /*!< Global pointer to current Surface Viewer structure*/
SUMA_SurfaceViewer *SUMAg_SVv = NULL; /*!< Global pointer to the vector containing the various Surface Viewer Structures 
                                    SUMAg_SVv contains SUMA_MAX_SURF_VIEWERS structures */
int SUMAg_N_SVv = 0; /*!< Number of SVs realized by X */
SUMA_DO *SUMAg_DOv = NULL;   /*!< Global pointer to Displayable Object structure vector*/
int SUMAg_N_DOv = 0; /*!< Number of DOs stored in DOv */
SUMA_CommonFields *SUMAg_CF = NULL; /*!< Global pointer to structure containing info common to all viewers */

/* NICE THINGS TO ADD 
   + support -surf_group and -switch_group
   + support view_surf (for hiding say L/R hemis)
   + DONE: create syntax for series of repeated key strokes with delay between strokes and perhaps a forced redisplay
   + DONE: make recorder save pictures
   + add passing of DOs as is done in Julia's program
   + DONE: support for quit action
   + DONE: support control of intensity range
*/

static char uDS_show_surf[]={
               "        CreateIcosahedron -rd 4\n"
               "        suma -niml &\n"
               "        echo 'Wait until suma is ready then proceed.'\n"
               "        DriveSuma -com show_surf -label icoco \\\n"
               "                       -i_fs CreateIco_surf.asc\n"
};
static char uDS_node_xyz[]={
"        ConvertSurface -i_fs CreateIco_surf.asc \\\n"
"                       -o_1D radcoord radcoord \\\n"
"                       -radial_to_sphere 100\n"
"        DriveSuma -com node_xyz -label icoco \\\n"
"                       -xyz_1D radcoord.1D.coord'[0,1,2]'\n"
"        1deval -a radcoord.1D.coord'[0]' -expr 'sin(a)*100' \\\n"
"            > xmess.1D ;1dcat xmess.1D radcoord.1D.coord'[1,2]' \\\n"
"            > somecoord.1D.coord ; rm xmess.1D\n"
"        DriveSuma -com node_xyz -label icoco \\\n"
"                       -xyz_1D somecoord.1D.coord\n"
};
static char uDS_viewer_cont[]={
"       DriveSuma -com  viewer_cont -key R -key ctrl+right\n"
"       DriveSuma -com  viewer_cont -key:r3:s0.3 up  \\\n"
"                       -key:r2:p left -key:r5:d right \\\n"
"                       -key:r3 z   -key:r5 left -key F6\n"
"       DriveSuma -com  viewer_cont -key m -key down \\\n"
"                 -com  sleep 2s -com viewer_cont -key m \\\n"
"                       -key:r4 Z   -key ctrl+right\n"
"       DriveSuma -com  viewer_cont -key m -key right \\\n"
"                 -com  pause press enter to stop this misery \\\n"
"                 -com  viewer_cont -key m \n"
};
static char uDS_recorder_cont[]={
"       DriveSuma -com  recorder_cont -save_as allanimgif.agif \\\n"
"                 -com  recorder_cont -save_as lastone.jpg \\\n"
"                 -com  recorder_cont -save_as three.jpg -save_index 3\\\n"
"                 -com  recorder_cont -save_as some.png -save_range 3 6\n"
};
static char uDS_surf_cont[]={
               /*"       quickspec -spec radcoord.spec \\\n"
               "                 -tn 1d radcoord.1D.coord radcoord.1D.topo \\\n"
               "       SurfaceMetrics -curv -spec radcoord.spec \\\n"
               "                      -surf_A radcoord -prefix radcoord      \n"*/
"       echo 1 0 0 > bbr.1D.cmap; echo 1 1 1 >> bbr.1D.cmap; \\\n"
"       echo 0 0  1 >> bbr.1D.cmap\n"
"       IsoSurface -shape 4 128 -o_ply blooby.ply\n"
"       quickspec -spec blooby.spec -tn ply blooby.ply\n"
"       SurfaceMetrics -curv -spec blooby.spec \\\n"
"                      -surf_A blooby -prefix blooby      \n"
"       DriveSuma -com show_surf -surf_label blooby \\\n"
"                      -i_ply blooby.ply -surf_winding cw \\\n"
"                      -surf_state la_blooby\n"
"       DriveSuma -com surf_cont -load_dset blooby.curv.1D.dset \\\n"
"                      -surf_label blooby -view_surf_cont y\n"
"       DriveSuma -com surf_cont -I_sb 7 -T_sb 8 -T_val 0.0\n"
"       DriveSuma -com surf_cont -I_range 0.05 -T_sb -1\n"
"       DriveSuma -com surf_cont -I_sb 8 -I_range -0.1 0.1 \\\n"
"                      -T_val 0.02 -Dim 0.4\n"
"       DriveSuma -com surf_cont -B_sb 7 -B_range 0.5 -B_scale 0.1 0.9\n"
"       DriveSuma -com surf_cont -switch_dset Convexity -1_only y\n"
"       DriveSuma -com surf_cont -switch_cmap roi64 -1_only n\n"
"       DriveSuma -com surf_cont -view_dset n\n"
"       DriveSuma -com surf_cont -switch_dset blooby.curv.1D.dset \\\n"
"                      -view_surf_cont n -I_range -0.05 0.14\n"
"       DriveSuma -com surf_cont -load_cmap bbr.1D.cmap\n"
};
static char uDS_kill_suma[]={
               "       DriveSuma -com kill_suma\n"
};
void usage_DriveSuma (SUMA_GENERIC_ARGV_PARSE *ps)
{
      static char FuncName[]={"usage_DriveSuma"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL;
      int i;
      s = SUMA_help_basics();
      sio  = SUMA_help_IO_Args(ps);
      printf ( "\n"
"Usage: A program to drive suma from command line.\n"
"       DriveSuma [options] -com COM1 -com COM2 ...\n"
"Mandatory parameters:\n"
"---------------------\n"
"   -com COM: Command to be sent to SUMA.\n"
"             At least one command must be used\n"
"             and various commands can follow in\n"
"             succession.\n"
"        COM is the command string and consists\n"
"            of at least an action ACT. Some actions\n"
"            require additional parameters to follow\n"
"            ACT. \n"
" Actions (ACT) and their parameters:\n"
" -----------------------------------\n"
" o pause [MSG]: Pauses DriveSuma and awaits\n"
"                an 'Enter' to proceed with\n"
"                other commands. \n"
"                MSG is an optional collection of\n"
"                strings that can be displayed as\n"
"                a prompt to the user. See usage\n"
"                in examples below.\n"
"\n"
" o sleep DUR: Put DriveSuma to sleep for a duration DUR.\n"
"              DUR is the duration, specified with something\n"
"              like 2s (or 2) or 150ms\n"
"              See usage in examples below.\n"
"\n"
" o show_surf: Send surface to SUMA.\n"
"     + Mandatory parameters for show_surf action:\n"
"        -surf_label LABEL: A label (identifier) to assign to the\n"
"                           surface\n"
"        -i_TYPE SURF: Name of surface file, see surface I/O \n"
"                      options below for details.\n"
"     + Optional parameters for show_surf action:\n"
/*"        -surf_group GROUP:\n"*/
"          -surf_state STATE: Name the state of that surface\n"
"          -surf_winding WIND: Winding of triangles. Choose \n"
"                              from ccw or cw (normals on sphere\n"
"                              pointing in). This option affects\n"
"                              the lighting of the surface.\n"
"     + Example show_surf: \n"
"        1- Create some surface\n"
"        2- Start SUMA\n"
"        3- Send new surface to SUMA\n"
"        ---------------------------\n"
"%s"
"\n"
" o node_xyz: Assign new coordinates to surface in SUMA\n"
"     + Mandatory parameters for action node_xyz:\n"
"        -surf_label LABEL: A label to identify the target \n"
"                           surface\n"
"        -xyz_1D COORDS.1D: A 1D formatted file containing a new \n"
"                           coordinate for each of the nodes \n"
"                           forming the surface. COORDS.1D must \n"
"                           have three columns.\n"
"                           Column selectors can be used here as \n"
"                           they are in AFNI.\n"
"     + Example node_xyz (needs surface from 'Example show_surf')\n"
"        1- Create some variation on the coords of the surface\n"
"        2- Send new coordinates to SUMA\n"
"        3- Manipulate the x coordinate now\n"
"        4- Send new coordinates again to SUMA\n"
"        -------------------------------------\n"
"%s"
"\n"
" o viewer_cont: Apply settings to viewer or viewer controller\n"
"     + Optional parameters for action viewer_cont:\n"
"       (Parameter names reflect GUI labels or key strokes.)\n"
"        -load_view VIEW_FILE: Load a previously\n"
"                              saved view file (.vvs).\n"
"                              Same as 'File-->Load View'\n"
"        -load_do   DO_FILE: Load a displayable object file\n"
"                            For detailed information on DO_FILE's format,\n"
"                            see the section under suma's  help (ctrl+h)\n"
"                            where the function of Ctrl+Alt+s is detailed.\n"
"        -key KEY_STRING: Act as if the key press KEY_STRING\n"
"                         was applied in the viewer.\n"
"                         ~ Not all key presses from interactive\n"
"                         mode are allowed here.\n"
"                         ~ Available keys and their variants are:\n"
"                         [, ], comma (or ','), period (or '.'), space,\n"
"                         a, b, d, G, j, m, n, p, r, t, z, \n"
"                         up, down, left, right, and F1 to F8.\n"
"                         ~ Key variants are specified this way:\n"
"                         ctrl+Up or ctrl+alt+Down etc.\n"
"                         ~ For help on key actions consult SUMA's\n"
"                         GUI help.\n"
"                         ~ Using multiple keys in the same command\n"
"                         might not result in the serial display of\n"
"                         the effect of each key, unless 'd' modifier\n"
"                         is used as shown further below. For example,\n"
"                         -key right -key right would most likely\n"
"                         produce one image rotated twice rather than\n"
"                         two images, each turned right once.\n"
"           The -key string can be followed by modifiers:\n"
"              For example, -key:r5:s0.2 has two modifiers,\n"
"              r5 and s0.2. All modifiers are separated by ':'.\n"
"              'r' Repeat parameter, so r5 would repeat the \n"
"                  same key 5 times.\n"
"              's' Sleep parameter, so s0.2 would sleep for 0.2\n"
"                  seconds between repeated keys.\n"
"              'd' Immediate redisplay flag. That is useful\n"
"                  when you are performing a succession of keys and\n"
"                  want to ensure each individual one gets displayed\n"
"                  and recorded (most likely). Otherwise, successive\n"
"                  keys may only display their resultant. 'd' is used\n"
"                  automatically with 's' modifier.\n"
"              'p' Pause flag. Requires user intervention to proceed.\n"
"              'v' Value string. The string is passed to the function\n"
"                  that processes this key, as if you'd entered that string\n"
"                  in the GUI directly. To avoid parsing headaches, you\n"
"                  should use quotes with this qualifier. For example, say\n"
"                  you want to pass 0.0 0.0 0.0 to the 'ctrl+j' key press.\n"
"                  At the shell you would enter:\n"
"                  DriveSuma -com viewer_cont '-key:v\"0.8 0 10.3\"' ctrl+j\n"
"        -viewer VIEWER: Specify which viewer should be acted \n"
"                        upon. Default is viewer 'A'. Viewers\n"
"                        must be created first (ctrl+n) before\n"
"                        they can be acted upon.\n"
"                        You can also refer to viewers with integers\n"
"                        0 for A, 1 for B, etc.\n"
"        -viewer_width or (-width) WIDTH: Set the width in pixels of\n"
"                                     the current viewer.\n"
"        -viewer_height or (-height) HEIGHT: Set the height in pixels of\n"
"                                     the current viewer.\n"
"        -viewer_size WIDTH HEIGHT : Convenient combo of -viewer_width \n"
"                                    and -viewer_height\n"
"        -viewer_position X Y: Set position on the screen\n"
"     + Example viewer_cont (assumes all previous examples have\n"
"       been executed and suma is still running).\n"
"        - a series of commands that should be obvious.\n"
"       -------------------------------------\n"
"%s"
"\n"
" o recorder_cont: Apply commands to recorder window\n"
"     + Optional parameters for action recorder_cont:\n"
"       -anim_dup DUP: Save DUP copies of each frame into movie\n"
"                      This has the effect of slowing movies down\n"
"                      at the expense of file size, of course.\n"
"                      DUP's default is set by the value of AFNI_ANIM_DUP\n"
"                      environment variable. \n"
"                      To set DUP back to its default value, use -anim_dup 0.\n" 
"       -save_as PREFIX.EXT: Save image(s) in recorder\n"
"                             in the format determined by\n"
"                             extension EXT.\n"
"                             Allowed extensions are:\n"
"                             agif or gif: Animated GIF (movie)\n"
"                             mpeg or mpg: MPEG (movie)\n"
"                             jpeg or jpg: JPEG (stills)\n"
"                             png: PNG (stills)\n"
"       -save_index IND: Save one image indexed IND (start at 0)\n"
"       -save_range FROM TO: Save images from FROM to TO \n"
"       -save_last: Save last image (default for still formats)\n"
"       -save_last_n N: Save last N images\n"
"       -save_all: Save all images (default for movie formats)\n"
"       -cwd ABSPATH: Set ABSPATH as SUMA's working directory. \n"
"                     This path is used for storing output files\n"
"                     or loading dsets.\n"
"     + Example recorder_cont (assumes there is a recorder window)\n"
"       currently open from SUMA.\n"
"       -------------------------------------\n"
"%s"
"\n"                            
" o surf_cont: Apply settings to surface controller.\n"
"     + Optional parameters for action surf_cont:\n"
"       (Parameter names reflect GUI labels.)\n"  
"       -surf_label LABEL: A label to identify the target surface\n"
"       -load_dset DSET: Load a dataset\n"
"           ! NOTE: When using -load_dset you can follow it\n"
"                   with -surf_label in order to attach\n"
"                   the dataset to a particular target surface.\n"
"       -view_surf y/n: Show or hide surface LABEL\n"
"       -load_col COL: Load a colorfile named COL.\n"
"                      Similar to what one loads under\n"
"                      SUMA-->ctrl+s-->Load Col\n"
"                      COL contains 4 columns, of\n"
"                      the following format:\n"
"                      n r g b\n"
"                      where n is the node index and \n"
"                      r g b are thre flooat values between 0 and 1\n"
"                      specifying the color of each node.\n"
"       -view_surf_cont y/n: View surface controller\n"
"       -switch_surf LABEL: switch state to that of surface \n"
"                           labeled LABEL and make that surface \n"
"                           be in focus.\n"
"       -switch_dset DSET: switch dataset to DSET\n"
"       -view_dset y/n: Set view toggle button of DSET\n"
"       -1_only y/n: Set 1_only toggle button of DSET\n"
"       -switch_cmap CMAP: switch colormap to CMAP\n"
"       -load_cmap CMAP.1D.cmap: load and switch colormap in \n"
"                                file CMAP.1D.cmap\n"
"       -I_sb ISB: Switch intensity to ISBth column (sub-brick)\n"
"       -I_range IR0 IR1: set intensity range from IR0 to IR1.\n"
"                         If only one number is given, the range\n"
"                         is symmetric from -|IR0| to |IR0|.\n"
"       -shw_0 y/n      or \n" 
"       -show_0 y/n: Set shw 0 toggle button of DSET.\n"
"       -T_sb TSB: Switch threshold to TSBth column (sub-brick)\n"
"                  Set TSB to -1 to turn off thresholding.\n"
"       -T_val THR: Set threshold to THR\n"
"       -B_sb BSB: Switch brightness modulation to BSBth column (sub-brick)\n"
"       -B_range BR0 BR1: set brightness clamping range from BR0 to BR1.\n"
"                         If only one number is given, the range\n"
"                         is symmetric from -|BR0| to |BR0|.\n"
"       -B_scale BS0 BS1: Modulate brightness by BS0 factor for BR0 or lower\n"
"                         by BS1 factor for BR1 or higher, and linearly \n"
"                         interpolate scaling for BR0 < values < BR1\n" 
"       -Dim DIM: Set the dimming factor.\n"
"     + Example surf_cont (assumes all previous examples have\n"
"       been executed and suma is still running).\n"
"       - Obvious chicaneries to follow:\n"
"       --------------------------------\n"
"%s"
"\n"
" o kill_suma: Close suma and quit.\n"
"\n"
"Advice:\n"
"-------\n"
"   If you get a colormap in your recorded image, it is\n"
"   because the last thing you drew was the surface controller\n"
"   which has an openGL surface for a colormap. In such cases,\n"
"   Force a redisplay of the viewer with something like:\n"
"      -key:r2:d m \n"
"                  where the m key is pressed twice (nothing)\n"
"                  changes in the setup but the surface is \n"
"                  redisplayed nonetheless because of the 'd'\n"
"                  key option.\n"
"Options:\n"
"--------\n"
"   -examples: Show all the sample commands and exit\n"
"   -C_demo: execute a preset number of commands\n"
"            which are meant to illustrate how one\n"
"            can communicate with SUMA from one's \n"
"            own C code. Naturally, you'll need to\n"
"            look at the source code file SUMA_DriveSuma.c\n"
"      Example:\n"
"      suma -niml &\n"
"      DriveSuma -C_demo\n"
"\n"
"%s"
"%s"
"\n"
               , uDS_show_surf, uDS_node_xyz, uDS_viewer_cont, uDS_recorder_cont, uDS_surf_cont, sio,  s);
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      s = SUMA_New_Additions(0, 1); printf("%s\n", s);SUMA_free(s); s = NULL;
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}

SUMA_Boolean SUMA_ParseKeyModifiers(char *keyopt, int *Key_mult, 
                                    float *Key_pause, int *Key_redis, 
                                    char **strgvalp)
{
   static char FuncName[]={"SUMA_ParseKeyModifiers"};
   char *cccp=NULL, *op=NULL;
   int Found, v;
   double dv;
   SUMA_Boolean LocalHead = NOPE;
   SUMA_ENTRY;
   
   *Key_mult = 1;
   *Key_pause = 0.0;
   *Key_redis = 0;
   if (!strgvalp || *strgvalp) {
      SUMA_S_Err("strgvalp is NULL or point to not NULL");
   }  
   if (!keyopt || strncmp(keyopt,"-key", 4)) {
      SUMA_S_Errv("NULL or bad keyopt %s", SUMA_CHECK_NULL_STR(keyopt));
      SUMA_RETURN(NOPE);
   }
   Found = 1; 
   SUMA_LHv("keyopt=%s\n", keyopt);
   cccp = keyopt;
   do {
      if ((cccp = strstr(cccp,":"))) {/* found mods */
         SUMA_LHv("Now at =%s\n", cccp);
         /* what is it? */
         ++cccp;
         switch (cccp[0]) {
            case 'r':
               SUMA_ADVANCE_PAST_INT(cccp, v, Found);
               if (!Found) {
                  fprintf (SUMA_STDERR, 
                           "Failed to parse number after :r in %s\n", 
                           keyopt);
                  SUMA_RETURN(NOPE);
               }
               *Key_mult = v;
               break;
            case 'p':
               *Key_pause = -1; Found = 1;
               SUMA_LH("Will pause for each rep\n");
               break;
            case 's':
               ++cccp;  /* touchy for floats*/
               SUMA_ADVANCE_PAST_NUM(cccp, dv, Found);
               if (!Found) {
                  fprintf (SUMA_STDERR, 
                           "Failed to parse number after :s in %s\n", 
                           keyopt);
                  SUMA_RETURN(NOPE);
               }
               *Key_pause = (float)dv;
               SUMA_LHv("Will pause for %f secs\n", *Key_pause);
               break;
            case 'd':
               *Key_redis = 1;
               SUMA_LH("Will redisplay for each rep\n");
               break;
            case 'v':
               op = cccp;
               SUMA_SKIP_TO_NEXT_CHAR(cccp, NULL, ':');
               SUMA_COPY_TO_STRING(op, cccp, (*strgvalp));
               break;
            default:
               SUMA_S_Errv("Failed to parse content of %s\n", keyopt);
               Found = 0;
               break;
         }
      }
   } while (cccp && cccp[0] && Found);

   SUMA_RETURN(YUP);   
}
/*
A function for parsing command command options.
Words recognized here are flagged with a null char at the beginning of the identified strings
*/
int SUMA_DriveSuma_ParseCommon(NI_group *ngr, int argtc, char ** argt)
{
   static char FuncName[]={"SUMA_DriveSuma_ParseCommon"};
   int kar, N, nv, nums;
   double dv3[3], tmpd;
   char *stmp=NULL;
   SUMA_PARSED_NAME *fn;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   /* parse 'em */
   kar = 1;
   brk = NOPE;
	if (LocalHead) {
      fprintf(SUMA_STDERR, 
               "%s verbose: Parsing command line...\n"
               "Have %d entries with argt[0]='%s'", 
               FuncName, argtc, argt[0]);
	}
   while (kar < argtc) { /* loop accross command ine options */
      SUMA_LHv("Now processing argt[%d]=%s\n", 
               kar, SUMA_CHECK_NULL_STR(argt[kar]));
      if (!argt[kar]) {
         SUMA_S_Errv("Null entry!: argt[%d]=%s\n", 
                     kar, SUMA_CHECK_NULL_STR(argt[kar]));
         SUMA_RETURN(NOPE);
      }
      if (!brk && (  (strcmp(argt[kar], "-label") == 0) || 
                     (strcmp(argt[kar], "-surf_label") == 0) || 
                     (strcmp(argt[kar], "-so_label") == 0)))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a label after -surf_label \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         if (!NI_get_attribute(ngr, "SO_label")) 
            NI_set_attribute(ngr, "SO_label", argt[++kar]);
         else if (strcmp(NI_get_attribute(ngr, "SO_label"), argt[++kar])) { 
            SUMA_S_Err("Two options setting different  surface labels"); 
            SUMA_RETURN(0); 
         }
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-switch_surf") == 0) ))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a surf label after -switch_surf \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         if (!NI_get_attribute(ngr, "SO_label")) 
            NI_set_attribute(ngr, "SO_label", argt[++kar]);
         else if (strcmp(NI_get_attribute(ngr, "SO_label"), argt[++kar])) { 
            SUMA_S_Err("Two options setting different  surface labels"); 
            SUMA_RETURN(0); 
         }
         NI_set_attribute(ngr, "switch_surf", argt[kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-switch_cmap") == 0) ))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a cmap name after -switch_cmap \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "switch_cmap", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-load_cmap") == 0) ))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a cmap name after -load_cmap \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "load_cmap", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-dset_label") == 0) ))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a label after -dset_label \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "dset_label", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-switch_dset") == 0) ))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a dset label after -switch_dset \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "dset_label", argt[++kar]);
         NI_set_attribute(ngr, "switch_dset", argt[kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-load_dset") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a dset file after -load_dset \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         fn = SUMA_ParseFname(argt[++kar], SUMAg_CF->cwd);
         /* SUMA_ShowParsedFname(fn, NULL); */
         NI_set_attribute(ngr, "Dset_FileName", fn->FullName);
         fn = SUMA_Free_Parsed_Name(fn);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-load_col") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a color file after -load_col \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         fn = SUMA_ParseFname(argt[++kar], SUMAg_CF->cwd);
         /* SUMA_ShowParsedFname(fn, NULL); */
         NI_set_attribute(ngr, "Col_FileName", fn->FullName);
         fn = SUMA_Free_Parsed_Name(fn);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-I_sb") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need an index after -I_sb \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "I_sb", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-I_range") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need at least one value after -I_range \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         ++kar; N = 1; stmp = NULL; nums = 0;
         while (  kar < argtc && 
                  argt[kar] && 
                  SUMA_isNumString(argt[kar],(void *)((long int)N))) {
            stmp = SUMA_append_replace_string(stmp, argt[kar], " ", 1); ++nums;
            argt[kar][0] = '\0'; ++kar;
         } --kar;
         if (!stmp || nums < 1 || nums > 2) {
            SUMA_S_Err( "Bad format for -I_range option values;\n"
                        " 1 or 2 values allowed.");
            SUMA_RETURN(0);
         }
         nv = SUMA_StringToNum(stmp, (void *)dv3, 3,2);
         if (nv < 1 || nv > 2) {
            SUMA_S_Err("Bad range string.");
            SUMA_RETURN(0);
         }else {
            if (nv == 1) { dv3[0] = -SUMA_ABS(dv3[0]); dv3[1] = -dv3[0]; }
            else if (dv3[0] > dv3[1]) { 
               tmpd = dv3[0]; dv3[0] = dv3[1]; dv3[1] = tmpd; 
            }
            /* have range, set it please */
            SUMA_free(stmp); stmp = NULL; 
            stmp = (char *)SUMA_malloc(sizeof(char)*nv*50);
            sprintf(stmp,"%f , %f", dv3[0], dv3[1]);
            NI_set_attribute(ngr, "I_range", stmp);
            SUMA_LHv("Range of %s\n", stmp);
            SUMA_free(stmp); stmp = NULL;
         }
         brk = YUP;
      }
      if (!brk && ( (strcmp(argt[kar], "-B_sb") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need an index after -B_sb \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "B_sb", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-B_range") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need at least one value after -B_range \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         ++kar; N = 1; stmp = NULL; nums = 0;
         while (  kar < argtc && 
                  argt[kar] && 
                  SUMA_isNumString(argt[kar],(void *)((long int)N))) {
            stmp = SUMA_append_replace_string(stmp, argt[kar], " ", 1); ++nums;
            argt[kar][0] = '\0'; ++kar;
         } --kar;
         if (!stmp || nums < 1 || nums > 2) {
            SUMA_S_Err( "Bad format for -B_range option values;\n"
                        " 1 or 2 values allowed.");
            SUMA_RETURN(0);
         }
         nv = SUMA_StringToNum(stmp, (void *)dv3, 3,2);
         if (nv < 1 || nv > 2) {
            SUMA_S_Err("Bad range string.");
            SUMA_RETURN(0);
         }else {
            if (nv == 1) { dv3[0] = -SUMA_ABS(dv3[0]); dv3[1] = -dv3[0]; }
            else if (dv3[0] > dv3[1]) { 
               tmpd = dv3[0]; dv3[0] = dv3[1]; dv3[1] = tmpd; 
            }
            /* have range, set it please */
            SUMA_free(stmp); stmp = NULL; 
            stmp = (char *)SUMA_malloc(sizeof(char)*nv*50);
            sprintf(stmp,"%f , %f", dv3[0], dv3[1]);
            NI_set_attribute(ngr, "B_range", stmp);
            SUMA_LHv("Range of %s\n", stmp);
            SUMA_free(stmp); stmp = NULL;
         }
         brk = YUP;
      }
      if (!brk && ( (strcmp(argt[kar], "-B_scale") == 0) ) )
      {
         if (kar+2 >= argtc)
         {
            fprintf (SUMA_STDERR, "need two values after -B_scale \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         ++kar; N = 1; stmp = NULL; nums = 0;
         while (  kar < argtc && 
                  argt[kar] && 
                  SUMA_isNumString(argt[kar],(void *)((long int)N))) {
            stmp = SUMA_append_replace_string(stmp, argt[kar], " ", 1); ++nums;
            argt[kar][0] = '\0'; ++kar;
         } --kar;
         if (!stmp || nums != 2) {
            SUMA_S_Err( "Bad format for -B_scale option values;\n"
                        " 2 values needed.");
            SUMA_RETURN(0);
         }
         nv = SUMA_StringToNum(stmp, (void *)dv3, 3,2);
         if ( nv != 2) {
            SUMA_S_Err("Bad scale string.");
            SUMA_RETURN(0);
         }else {
            if (dv3[0] > dv3[1]) { 
               tmpd = dv3[0]; dv3[0] = dv3[1]; dv3[1] = tmpd; 
            }
            /* have scale, set it please */
            SUMA_free(stmp); stmp = NULL; 
            stmp = (char *)SUMA_malloc(sizeof(char)*nv*50);
            sprintf(stmp,"%f , %f", dv3[0], dv3[1]);
            NI_set_attribute(ngr, "B_scale", stmp);
            SUMA_LHv("Scale of %s\n", stmp);
            SUMA_free(stmp); stmp = NULL;
         }
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-T_sb") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need an index after -T_sb \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "T_sb", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-T_val") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a value after -T_val \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "T_val", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-Dim") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a value after -Dim \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "Dim", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-viewer") == 0))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, 
                     "need a viewer (A-F) or (0-5) after -viewer \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "SV_id", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-1_only") == 0))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a 'y/n' after -1_only \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         if (argt[kar][0] == 'y' || argt[kar][0] == 'Y')  
            NI_set_attribute(ngr, "1_only", "y");
         else if (argt[kar][0] == 'n' || argt[kar][0] == 'N')  
            NI_set_attribute(ngr, "1_only", "n");
         else {
            fprintf (SUMA_STDERR, "need a 'y/n' after -1_only \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         brk = YUP;
      }

      if (!brk && (  (strcmp(argt[kar], "-shw_0") == 0) ||
                     (strcmp(argt[kar], "-show_0") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a 'y/n' after -show_0 \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         if (argt[kar][0] == 'y' || argt[kar][0] == 'Y')  
            NI_set_attribute(ngr, "shw_0", "y");
         else if (argt[kar][0] == 'n' || argt[kar][0] == 'N')  
            NI_set_attribute(ngr, "shw_0", "n");
         else {
            fprintf (SUMA_STDERR, "need a 'y/n' after -show_0 \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      
      if (!brk && (strcmp(argt[kar], "-view_dset") == 0))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a 'y/n' after -view_dset \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         if (argt[kar][0] == 'y' || argt[kar][0] == 'Y')  
            NI_set_attribute(ngr, "view_dset", "y");
         else if (argt[kar][0] == 'n' || argt[kar][0] == 'N')  
            NI_set_attribute(ngr, "view_dset", "n");
         else {
            fprintf (SUMA_STDERR, "need a 'y/n' after -view_dset \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-view_surf") == 0))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a 'y/n' after -view_surf \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         if (argt[kar][0] == 'y' || argt[kar][0] == 'Y')  
            NI_set_attribute(ngr, "view_surf", "y");
         else if (argt[kar][0] == 'n' || argt[kar][0] == 'N')  
            NI_set_attribute(ngr, "view_surf", "n");
         else {
            fprintf (SUMA_STDERR, "need a 'y/n' after -view_surf \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-view_surf_cont") == 0))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a 'y/n' after -view_surf_cont \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         if (argt[kar][0] == 'y' || argt[kar][0] == 'Y')  
            NI_set_attribute(ngr, "View_Surf_Cont", "y");
         else if (argt[kar][0] == 'n' || argt[kar][0] == 'N')  
            NI_set_attribute(ngr, "View_Surf_Cont", "n");
         else {
            fprintf (SUMA_STDERR, "need a 'y/n' after -view_surf_cont \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strncmp(argt[kar], "-key", 4) == 0))
      {
         int N_Key = 0, Key_mult = 1, Key_redis= 0;
         char *Key_strval=NULL;
         char stmp[100];
         float Key_pause = 0;
         
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a key after -key \n");
            SUMA_RETURN(0);
         }
         
         if (!SUMA_ParseKeyModifiers(argt[kar], 
                                    &Key_mult, 
                                    &Key_pause, 
                                    &Key_redis, 
                                    &Key_strval)) {
            SUMA_S_Errv("Failed in parsing %s\n", argt[kar]);
            SUMA_RETURN(0);
         } 

         
         argt[kar][0] = '\0';
         ++kar;
         if (!NI_get_attribute(ngr,"N_Key")) NI_SET_INT(ngr,"N_Key", 0);
         NI_GET_INT(ngr, "N_Key", N_Key); 
         sprintf(stmp, "Key_%d", N_Key);
         NI_SET_STR(ngr, stmp, argt[kar]);
         sprintf(stmp, "Key_rep_%d", N_Key);
         NI_SET_INT(ngr, stmp, Key_mult);
         sprintf(stmp, "Key_pause_%d", N_Key);
         NI_SET_FLOAT(ngr, stmp, Key_pause);
         sprintf(stmp, "Key_redis_%d", N_Key);
         NI_SET_INT(ngr, stmp, Key_redis);
         sprintf(stmp, "Key_strval_%d", N_Key);
         if (Key_strval) {
            NI_set_attribute(ngr, stmp, Key_strval);
            SUMA_free(Key_strval);
         }
         argt[kar][0] = '\0';
         ++N_Key;
         NI_SET_INT(ngr,"N_Key", N_Key);
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-load_view") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a .vvs file after -load_view \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "VVS_FileName", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-load_do") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a .do file after -load_do \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "DO_FileName", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      if (!brk && ( (strcmp(argt[kar], "-anim_dup") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a positive integer after -anim_dup \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';++kar;
         
         if (atoi(argt[kar]) < 0) {
            fprintf (SUMA_STDERR, "need a positive integer after -anim_dup \n");
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "Anim_Dup", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-save_as") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a PREFIX.EXT  after -save_as \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';++kar;
         
         NI_set_attribute(ngr, "Save_As", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      if (!brk && ( (strcmp(argt[kar], "-save_range") == 0) ) )
      {
         if (kar+2 >= argtc)
         {
            fprintf (SUMA_STDERR, "need 2 numbers after -save_from \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';++kar;
         NI_SET_INT(ngr, "Save_From", atoi(argt[kar]));
         argt[kar][0] = '\0';++kar;
         NI_SET_INT(ngr, "Save_To", atoi(argt[kar]));
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-save_last") == 0) ) )
      {
         
         NI_SET_INT(ngr, "Save_From", -1);
         NI_SET_INT(ngr, "Save_To", 0);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-save_index") == 0) ) )
      {
         
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a number after -save_index \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';++kar;
         NI_SET_INT(ngr, "Save_From", atoi(argt[kar]));
         NI_SET_INT(ngr, "Save_To", atoi(argt[kar]));
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      if (!brk && ( (strcmp(argt[kar], "-save_last_n") == 0) ) )
      {
         
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a number after -save_last_n \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';++kar;
         if (atoi(argt[kar]) <= 0) {
            fprintf (SUMA_STDERR, "need a number > 0 after -save_last_n\n");
            SUMA_RETURN(0);
         }
         NI_SET_INT(ngr, "Save_From", -atoi(argt[kar]));
         NI_SET_INT(ngr, "Save_To", 0);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      if (!brk && ( (strcmp(argt[kar], "-save_all") == 0) ) )
      {
         
         NI_SET_INT(ngr, "Save_From", 0);
         NI_SET_INT(ngr, "Save_To", 0);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      if (  !brk && 
            (  (strcmp(argt[kar], "-caller_working_dir") == 0) || 
               (strcmp(argt[kar], "-cwd") == 0)) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a path after -caller_working_dir \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';++kar;
         
         NI_set_attribute(ngr, "Caller_Working_Dir", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (  !brk && 
            (  (strcmp(argt[kar], "-viewer_width") == 0) ||
               (strcmp(argt[kar], "-width") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need 1 number after %s \n", argt[kar]);
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';++kar;
         if (atoi(argt[kar]) < 10 || atoi(argt[kar]) > 2000) {
            fprintf (SUMA_STDERR, 
               "Have %d for width in pixels! \n", atoi(argt[kar]));
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "WindWidth", argt[kar]);
         NI_set_attribute(ngr, "DoViewerSetup","y"); /* flag indicating 
                                                      need to setup viewer, 
                                                      a la vvs */
        
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (  !brk && 
            (  (strcmp(argt[kar], "-viewer_height") == 0) ||
               (strcmp(argt[kar], "-height") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need 1 number after %s \n", argt[kar]);
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';++kar;
         if (atoi(argt[kar]) < 10 || atoi(argt[kar]) > 2000) {
            fprintf (SUMA_STDERR, 
               "Have %d for height in pixels! \n", atoi(argt[kar]));
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "WindHeight", argt[kar]);
         NI_set_attribute(ngr, "DoViewerSetup","y"); /* flag indicating 
                                                      need to setup viewer, 
                                                      a la vvs */
         argt[kar][0] = '\0';
         brk = YUP;
      }
      if (  !brk && 
            (  (strcmp(argt[kar], "-viewer_size") == 0)  ) )
      {
         if (kar+2 >= argtc)
         {
            fprintf (SUMA_STDERR, "need 2 numbers after %s \n", argt[kar]);
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';++kar;
         if (atoi(argt[kar]) < 10 || atoi(argt[kar]) > 2000) {
            fprintf (SUMA_STDERR, 
               "Have %d for width in pixels! \n", atoi(argt[kar]));
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "WindWidth", argt[kar]);
         argt[kar][0] = '\0';++kar;
         if (atoi(argt[kar]) < 10 || atoi(argt[kar]) > 2000) {
            fprintf (SUMA_STDERR, 
               "Have %d for height in pixels! \n", atoi(argt[kar]));
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "WindHeight", argt[kar]);
         NI_set_attribute(ngr, "DoViewerSetup","y"); /* flag indicating 
                                                      need to setup viewer, 
                                                      a la vvs */
         argt[kar][0] = '\0';
         brk = YUP;
      }
      if (  !brk && 
            (  (strcmp(argt[kar], "-viewer_position") == 0)  ) )
      {
         if (kar+2 >= argtc)
         {
            fprintf (SUMA_STDERR, "need 2 numbers after %s \n", argt[kar]);
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';++kar;
         if (atoi(argt[kar]) < 10 || atoi(argt[kar]) > 4000) {
            fprintf (SUMA_STDERR, 
               "Have %d for X in pixels! \n", atoi(argt[kar]));
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "WindX", argt[kar]);
         argt[kar][0] = '\0';++kar;
         if (atoi(argt[kar]) < 10 || atoi(argt[kar]) > 4000) {
            fprintf (SUMA_STDERR, 
               "Have %d for Y in pixels! \n", atoi(argt[kar]));
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "WindY", argt[kar]);
         NI_set_attribute(ngr, "DoViewerSetup","y"); /* flag indicating 
                                                      need to setup viewer, 
                                                      a la vvs */
         argt[kar][0] = '\0';
         brk = YUP;
      }
      if (0 && !brk) { /* do not enforce this here */
			fprintf (SUMA_STDERR,
                  "Error %s:\n"
                  "Option %s not understood. Try -help for usage\n",
               FuncName, argt[kar]);
			SUMA_RETURN(0);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }

   if (!NI_get_attribute(ngr, "Caller_Working_Dir")) {
      NI_set_attribute(ngr, "Caller_Working_Dir", SUMAg_CF->cwd);
   }
   SUMA_RETURN(YUP);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_DriveSuma_ParseInput(char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_DriveSuma_ParseInput"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt=NULL;
   int kar;
   SUMA_Boolean brk;
   SUMA_Boolean LocalHead = NOPE;

   SUMA_ENTRY;
   
   Opt = SUMA_Alloc_Generic_Prog_Options_Struct();
   Opt->com = NULL;
   Opt->N_com = 0;
   Opt->b1 = 0;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
		if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			 usage_DriveSuma(ps);
          exit (0);
		}
		
		SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      
      if (!brk && (strcmp(argv[kar], "-debug") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -debug \n");
            exit (1);
         }
         
         Opt->debug = atoi(argv[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argv[kar], "-C_demo") == 0))
      {
         Opt->b1 = 1;
         brk = YUP;  
      }
      
      if (!brk && ( (strcmp(argv[kar], "-examples") == 0) ) ) {
         fprintf(SUMA_STDOUT,"#Example commands for running DriveSuma\n\n");
         fprintf(SUMA_STDOUT,"#show_surf action\n%s\n", uDS_show_surf);
         fprintf(SUMA_STDOUT,"#node_xyz action\n%s\n", uDS_node_xyz);
         fprintf(SUMA_STDOUT,"#viewer_cont action\n%s\n", uDS_viewer_cont);
         fprintf(SUMA_STDOUT,"#recorder_cont action\n%s\n", uDS_recorder_cont);
         fprintf(SUMA_STDOUT,"#surf_cont action\n%s\n", uDS_surf_cont);
         fprintf(SUMA_STDOUT,"#Adieu\n%s\n", uDS_kill_suma);
         exit(0);          
      }

      if (!brk && (strcmp(argv[kar], "-com") == 0))
      {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a number after -debug \n");
            exit (1);
         }
         
         Opt->com = (char **)SUMA_realloc(Opt->com, sizeof(char *)*(Opt->N_com+1));
         Opt->com[Opt->N_com] = NULL;
         ++kar;
         do { 
            Opt->com[Opt->N_com] = SUMA_append_replace_string (Opt->com[Opt->N_com], argv[kar], " ", 1);
            ++kar;
            brk = NOPE;
            if ( kar >= argc ) brk = YUP;
            else if (strcmp(argv[kar], "-com") == 0) {
               --kar; brk = YUP;
            }
         } while (!brk);
         ++Opt->N_com;
         brk = YUP;
      }
      
      
      if (!brk && !ps->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n", FuncName, argv[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   SUMA_RETURN(Opt);
}

char ** SUMA_free_com_argv(char **argt, int *argtc)
{
   static char FuncName[]={"SUMA_free_com_argv"};
   int i;
   
   SUMA_ENTRY;
   
   if (argt) {
      for (i=0; i<*argtc; ++i) if (argt[i]) SUMA_free(argt[i]); 
      SUMA_free(argt); argt = NULL;
   }
   
   *argtc = -1;
   SUMA_RETURN(NULL);
}

/*!
   \brief char ** SUMA_com2argv(char *com, int *argtcp)
   Turn a command into an argv, argc duo
   Free argv with SUMA_free_com_argv
*/
char ** SUMA_com2argv(char *com, int *argtcp) 
{
   static char FuncName[]={"SUMA_com2argv"};
   char **argt=NULL, *pos, *tp=NULL;
   int argtc = 0;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   *argtcp = -1;
   
   /* change com to a bunch of arguments */
   /* get the type */
   SUMA_GET_BETWEEN_BLANKS(com, NULL, pos);
   tp = NULL; SUMA_COPY_TO_STRING(com, pos, tp); com = pos;
   if (!tp) { /* nothing to see here */
      *argtcp = 0;
      SUMA_RETURN(NULL);
   }
   SUMA_LHv("Adding >>>%s<<<\n", tp);
   argt = (char **)SUMA_realloc(argt, sizeof(char *)*(argtc+2)); {
      argt[argtc] = SUMA_copy_string("drivesumacom"); ++argtc; 
      argt[argtc] = tp; tp = NULL; ++argtc;
   }
   /* get whatever else follows */
   while (com[0]) {
      SUMA_GET_BETWEEN_BLANKS(com, NULL, pos);
      tp=NULL;SUMA_COPY_TO_STRING(com, pos, tp); com = pos;
      SUMA_LHv("Adding other >>>%s<<<\n", tp);
      argt = (char **)SUMA_realloc(argt, sizeof(char *)*(argtc+1)); 
      argt[argtc] = tp; tp = NULL; 
      ++argtc;
   }
   
   *argtcp = argtc;
   SUMA_RETURN(argt);
}

SUMA_SurfaceObject *SUMA_ShowSurfComToSO(char *com)
{
   static char FuncName[]={"SUMA_ShowSurfComToSO"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_GENERIC_ARGV_PARSE *pst=NULL;
   char **argt=NULL, *pos, *tp=NULL;
   int argtc = 0;
   SUMA_SurfSpecFile *Spec = NULL;
   int *isin=NULL;
   int  i = -1, ii, jj, kk, il, N_Spec=0, kar=0;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* change com to a bunch of arguments */
   argt = SUMA_com2argv(com, &argtc); 

   /* now parse these fake options */
   pst = SUMA_Parse_IO_Args(argtc, argt, "-i;-t;-spec;-sv;");
   if (LocalHead) SUMA_Show_IO_args(pst);
   
   
   if (pst->s_N_surfnames + pst->i_N_surfnames + pst->t_N_surfnames != 1) {
      SUMA_S_Err("Multiple surface specifications used. Only one surface allowed.");
      exit(1);
   }

   Spec = SUMA_IO_args_2_spec(pst, &N_Spec);
   if (N_Spec == 0) {
      SUMA_S_Err("No surfaces found.");
      exit(1);
   }
   if (N_Spec != 1) {
      SUMA_S_Err("Multiple spec at input.");
      exit(1);
   }

   /* read in one surface for now */
   SO = SUMA_Load_Spec_Surf(Spec, 0, pst->sv[0], 0);
   if (!SO) {
         fprintf (SUMA_STDERR,"Error %s:\n"
                              "Failed to find surface\n"
                              "in spec file. \n",
                              FuncName );
         exit(1);
      
   }

   /* now search for some extra options */
   kar = 1;
   brk = NOPE;
	while (kar < argtc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (!brk && ( (strcmp(argt[kar], "-label") == 0) || (strcmp(argt[kar], "-surf_label") == 0) || (strcmp(argt[kar], "-so_label") == 0)))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a label after -surf_label \n");
            exit (1);
         }
         
         if (SO->Label) SUMA_free(SO->Label);
         SO->Label = SUMA_copy_string(argt[++kar]);
         brk = YUP;
      }
      
      if (!brk && (  (strcmp(argt[kar], "-group") == 0) || 
                     (strcmp(argt[kar], "-surf_group") == 0) || 
                     (strcmp(argt[kar], "-so_group") == 0)))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a label after -surf_group \n");
            exit (1);
         }
         
         if (SO->Group) SUMA_free(SO->Group);
         SO->Group = SUMA_copy_string(argt[++kar]);
         brk = YUP;
      }
      
      if (!brk && (  (strcmp(argt[kar], "-state") == 0) || 
                     (strcmp(argt[kar], "-surf_state") == 0) || 
                     (strcmp(argt[kar], "-so_state") == 0)))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a label after -surf_state \n");
            exit (1);
         }
         
         if (SO->State) SUMA_free(SO->State);
         SO->State = SUMA_copy_string(argt[++kar]);
         brk = YUP;
      }
      
      if (!brk && (  (strcmp(argt[kar], "-norm_dir") == 0) || 
                     (strcmp(argt[kar], "-surf_winding") == 0) )) 
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, 
                     "need a direction (cw or ccw) after -surf_winding \n");
            exit (1);
         }
         ++kar;
         if (  SUMA_iswordsame_ci("cw", argt[kar]) == 1 ||
               SUMA_iswordsame_ci("in", argt[kar]) == 1 || 
               SUMA_iswordsame_ci("-1", argt[kar]) == 1 ) {
            SO->normdir = -1;
         } else if ( SUMA_iswordsame_ci("ccw", argt[kar]) == 1 || 
                     SUMA_iswordsame_ci("out", argt[kar]) == 1 || 
                     SUMA_iswordsame_ci("1", argt[kar]) == 1) {
            SO->normdir = 1;
         } else {
            fprintf (SUMA_STDERR,
                     "Error %s:\n"
                     "value %s not valid with -surf_winding "
                     "(cw or ccw only acceptable)\n", 
                                    FuncName, argt[kar]);
            exit(1);
         }
         brk = YUP;
      }
      
      if (!brk && !pst->arg_checked[kar]) {
			fprintf (SUMA_STDERR,
                  "Error %s:\n"
                  "Option %s not understood. Try -help for usage\n",
               FuncName, argt[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }

   /* fix the trimmings */
   if (!SO->State) {SO->State = SUMA_copy_string("DC"); }
   if (!SO->Group) {SO->Group = SUMA_copy_string("DS"); }
   if (!SO->Label) {SO->Label = SUMA_copy_string("Benedictus"); }
   if (SO->Label) { 
      if (SO->idcode_str) SUMA_free(SO->idcode_str); 
      SO->idcode_str = NULL; SUMA_NEW_ID(SO->idcode_str, SO->Label); }

   if (LocalHead) {
      SUMA_Print_Surface_Object(SO, NULL);
   }
   /* clean up */
   argt = SUMA_free_com_argv(argt, &argtc);
   
   if (pst) SUMA_FreeGenericArgParse(pst); pst = NULL;
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   
   SUMA_RETURN(SO);
}


SUMA_SurfaceObject *SUMA_NodeXYZComToSO(char *com)
{
   static char FuncName[]={"SUMA_NodeXYZComToSO"};
   SUMA_SurfaceObject *SO = NULL;
   SUMA_GENERIC_ARGV_PARSE *pst=NULL;
   char **argt=NULL, *pos, *tp=NULL;
   int argtc = 0;
   SUMA_SurfSpecFile *Spec = NULL;
   int *isin=NULL;
   int  i = -1, ii, jj, kk, il, N_Spec=0, kar=0;
   SUMA_Boolean brk = NOPE;
   char *f1d = NULL;
   float *far = NULL;
   int ncol, nrow;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   /* change com to a bunch of arguments */
   argt = SUMA_com2argv(com, &argtc); 
   
   /* now parse these fake options (have to do it, in case you need it later)*/
   pst = SUMA_Parse_IO_Args(argtc, argt, "-i;-t;-spec;-sv;");
   if (LocalHead) SUMA_Show_IO_args(pst);

   /* a necessary receptacle */
   SO = SUMA_Alloc_SurfObject_Struct(1);  
   
   /* parse 'em */
   kar = 1;
   brk = NOPE;
	while (kar < argtc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (!brk && ( (strcmp(argt[kar], "-label") == 0) || (strcmp(argt[kar], "-surf_label") == 0) || (strcmp(argt[kar], "-so_label") == 0)) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a label after -label \n");
            exit (1);
         }
         
         if (SO->Label) SUMA_free(SO->Label);
         SO->Label = SUMA_copy_string(argt[++kar]);
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-xyz_1D") == 0))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a 1D file after -xyz_1D \n");
            exit (1);
         }
         
         far=SUMA_Load1D_s(argt[++kar], &ncol, &nrow, 1, 0);
         SO->N_Node = nrow;
         SO->NodeDim = ncol;
         SO->NodeList = (float *)SUMA_calloc(nrow*ncol, sizeof(float));
         memcpy((void *)SO->NodeList, (void *)far, nrow*ncol * sizeof(float));
         free(far); far = NULL;
         brk = YUP;
      }
      
      if (!brk && !pst->arg_checked[kar]) {
			fprintf (SUMA_STDERR,"Error %s:\nOption %s not understood. Try -help for usage\n",
               FuncName, argt[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }

   /* fix the trimmings */
   if (!SO->State) {SO->State = SUMA_copy_string("DC"); }
   if (!SO->Group) {SO->Group = SUMA_copy_string("DS"); }
   if (!SO->Label) {SO->Label = SUMA_copy_string("Benedictus"); }
   if (SO->Label) { 
      if (SO->idcode_str) SUMA_free(SO->idcode_str); 
      SO->idcode_str = NULL; SUMA_NEW_ID(SO->idcode_str, SO->Label); 
   }

   if (LocalHead) {
      SUMA_Print_Surface_Object(SO, NULL);
   }
   /* clean up */
   argt = SUMA_free_com_argv(argt, &argtc);
   if (pst) SUMA_FreeGenericArgParse(pst); pst = NULL;
   if (N_Spec) {
      int k=0; 
      for (k=0; k<N_Spec; ++k) {
         if (!SUMA_FreeSpecFields(&(Spec[k]))) { SUMA_S_Err("Failed to free spec fields"); } 
      }
      SUMA_free(Spec); Spec = NULL; N_Spec = 0;
   }
   
   SUMA_RETURN(SO);
}


NI_group *SUMA_ComToNgr(char *com, char *command)
{
   static char FuncName[]={"SUMA_ComToNgr"};
   NI_group *ngr = NULL;   
   char **argt=NULL, *pos, *tp=NULL;
   int argtc = 0, kar = 0;
   SUMA_Boolean brk = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!com || !command) {
      SUMA_S_Err("NULL input");
      SUMA_RETURN(NULL);
   }
   SUMA_LHv("Called with >%s<\n", com);
   /* change com to a bunch of arguments */
   argt = SUMA_com2argv(com, &argtc); 
   
   ngr = NI_new_group_element();
   NI_rename_group(ngr, "EngineCommand");
   NI_set_attribute(ngr, "Command", command);
   
   if (argtc > 0) {
      if (!SUMA_DriveSuma_ParseCommon(ngr, argtc, argt)) {
         SUMA_S_Err("Failed to parse common options.\n");
         NI_free(ngr); ngr = NULL;
         SUMA_RETURN(ngr);
      }
   }
   
   /* parse left overs */
   kar = 1;
   brk = NOPE;
	while (kar < argtc) { /* loop accross command ine options */
		/*fprintf(stdout, "%s verbose: Parsing command line...\n", FuncName);*/
      if (argt[kar][0] == '\0') { brk = YUP; } /* been take care of */
      
      if (!brk) {
			fprintf (SUMA_STDERR,
                  "Error %s:\n"
                  "Option %s not understood or not valid for command %s.\n"
                  " Try -help for usage\n",
               FuncName, argt[kar], NI_get_attribute(ngr, "Command"));
			NI_free(ngr); ngr = NULL;
         SUMA_RETURN(ngr);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }

   if (LocalHead) {
      if (LocalHead) {
         int suc;
         SUMA_SL_Warn("writing NI group to DISK!");
         NEL_WRITE_TX(ngr, "stderr:", suc);
      }
   }
   
   /* clean up */
   argt = SUMA_free_com_argv(argt, &argtc);
   

   SUMA_RETURN(ngr);
}


  
int SUMA_ProcessCommand(char *com, SUMA_GENERIC_ARGV_PARSE *ps)
{
   static char FuncName[]={"SUMA_ProcessCommand"};
   int i;
   float *far=NULL;
   char *act, *pos, *stp;
   NI_group *ngr = NULL;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_SO_File_Type tp = SUMA_FT_NOT_SPECIFIED;
   SUMA_Boolean ans = NOPE;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!com) { SUMA_S_Err("NULL command"); SUMA_RETURN(NOPE); }
   SUMA_LHv("Called with %s\n", com);
   SUMA_GET_BETWEEN_BLANKS(com, NULL, pos);
   act = NULL;
   SUMA_COPY_TO_STRING(com, pos, act); com = pos;
   if (!act) { SUMA_S_Err("No action found"); SUMA_RETURN(NOPE); }
   
   ans = YUP;
   SUMA_TO_LOWER(act);
   if (strcmp((act), "show_surf") == 0) {
      SO = SUMA_ShowSurfComToSO(com);
      SUMA_LHv("Sending Surface %s\n", SO->Label); /* send the surface */
      SUMA_SendSumaNewSurface(SO, ps->cs);
   } else if (strcmp((act), "node_xyz") == 0) {
      SO = SUMA_NodeXYZComToSO(com);
      SUMA_LHv("Sending XYZ to %s", SO->Label);
      if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
   } else if (strcmp((act), "load_dset") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending LoadDset to suma");
      if (!SUMA_SendToSuma (SO, ps->cs, (void *)ngr, SUMA_ENGINE_INSTRUCTION, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      NI_free_element(ngr); ngr = NULL;
   } else if (strcmp((act), "load_col") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending LoadCol to suma");
      if (!SUMA_SendToSuma (SO, ps->cs, (void *)ngr, SUMA_ENGINE_INSTRUCTION, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      NI_free_element(ngr); ngr = NULL;
   }else if (strcmp((act), "surf_cont") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending SetSurfCont to suma");
      if (!SUMA_SendToSuma (SO, ps->cs, (void *)ngr, SUMA_ENGINE_INSTRUCTION, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      NI_free_element(ngr); ngr = NULL; 
   } else if (strcmp((act), "viewer_cont") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending SetViewerCont to suma");
      if (!SUMA_SendToSuma (SO, ps->cs, (void *)ngr, SUMA_ENGINE_INSTRUCTION, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      NI_free_element(ngr); ngr = NULL;
   } else if (strcmp((act), "recorder_cont") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending SetRecorderCont to suma");
      if (!SUMA_SendToSuma (SO, ps->cs, (void *)ngr, SUMA_ENGINE_INSTRUCTION, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      NI_free_element(ngr); ngr = NULL;
   } else if (strcmp((act), "sleep") == 0) {
      double slp;
      char **argt=NULL;
      int argtc = 0;

      /* change com to a bunch of arguments */
      argt = SUMA_com2argv(com, &argtc); 

      if (argtc != 2) {
         SUMA_S_Errv("Expecting one value after sleep, have %d\n%s\n", 
                        argtc-1, argt[1]);
         ans = NOPE;
      }
      slp = SUMA_ParseTime(argt[1]);
      SUMA_S_Notev("Sleeping for %.3lf seconds\n", slp/1000.0);
      NI_sleep((int)slp);
      argt = SUMA_free_com_argv(argt, &argtc);
   } else if (strcmp((act), "pause") == 0) {
      char **argt=NULL, *msg=NULL;
      int argtc = 0;
      
      /* change com to a bunch of arguments */
      argt = SUMA_com2argv(com, &argtc); 
      if (argtc < 2) {
         SUMA_PAUSE_PROMPT("Pausing DriveSuma.\nDo something to proceed.\n");
      } else {
        for (i=1; i<argtc; ++i) msg = SUMA_append_replace_string(msg, argt[i], " ", 1);
        SUMA_PAUSE_PROMPT(msg);
      }
      if (msg) SUMA_free(msg); msg = NULL;
      argt = SUMA_free_com_argv(argt, &argtc);
   } else if (strcmp((act), "kill_suma") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending kill_suma to suma");
      if (!SUMA_SendToSuma (SO, ps->cs, (void *)ngr, SUMA_ENGINE_INSTRUCTION, 1)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      SUMA_Wait_Till_Stream_Goes_Bad(ps->cs, 100, 1000, 0);
      NI_free_element(ngr); ngr = NULL;
      ans = -1; 
   } else {
      fprintf(SUMA_STDERR, "Error %s: Action '%s' not supported.\n", FuncName, act);
      ans = NOPE;
   }
   
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   if (act) SUMA_free(act);
   SUMA_RETURN(ans);
}

int main (int argc,char *argv[])
{/* Main */    
   static char FuncName[]={"DriveSuma"}; 
   SUMA_GENERIC_PROG_OPTIONS_STRUCT *Opt;  
   SUMA_GENERIC_ARGV_PARSE *ps=NULL;
   float ctr[3] = { 0.0, 0.0, 0.0};
   int cnt=0, i=0, exflag=0;
   SUMA_SurfaceObject *SO=NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_STANDALONE_INIT;
	SUMA_mainENTRY;

   /* Allocate space for DO structure */
	SUMAg_DOv = SUMA_Alloc_DisplayObject_Struct (SUMA_MAX_DISPLAYABLE_OBJECTS);
   ps = SUMA_Parse_IO_Args(argc, argv, "-i;-t;-spec;");
   /* force talk option whether users specify it or not */
   ps->cs->talk_suma = 1;
   if (ps->cs->rps > 0) { ps->cs->nelps = (float)ps->cs->talk_suma * ps->cs->rps; }
   else { ps->cs->nelps = (float) ps->cs->talk_suma * -1.0; }

   if (argc < 0) {
      usage_DriveSuma(ps);
      exit (1);
   }
   
   Opt = SUMA_DriveSuma_ParseInput (argv, argc, ps);

   if (Opt->debug > 2) LocalHead = YUP;
   
   /* open communication */
   SUMA_LH("Talking to suma");
   ps->cs->istream = SUMA_DRIVESUMA_LINE;
   ps->cs->afni_istream = SUMA_AFNI_STREAM_INDEX2; /* not used yet */
   ps->cs->kth = 1; /* make sure all surfaces get sent */
   if (!SUMA_SendToSuma (NULL, ps->cs, NULL, SUMA_NO_DSET_TYPE, 0)) {
      SUMA_SL_Err("Failed to initialize SUMA_SendToSuma");
      ps->cs->Send = NOPE;
      ps->cs->afni_Send = NOPE;
      ps->cs->talk_suma = NOPE;
   } 

   
   if (Opt->b1) { /* sample code for Ben Singer */
      /* create a surface of sorts, set up a few attributes */
      SO = SUMA_CreateIcosahedron (50.0, 12, ctr, "n", 1);
      if (!SO) { SUMA_S_Err("Failed to create Icosahedron"); exit(1); }
      if (!SO->State) {SO->State = SUMA_copy_string("DC"); }
      if (!SO->Group) {SO->Group = SUMA_copy_string("DS"); }
      if (!SO->Label) {SO->Label = SUMA_copy_string("IcoSurf"); }
      if (SO->Label) { 
         if (SO->idcode_str) SUMA_free(SO->idcode_str); 
         SO->idcode_str = NULL; SUMA_NEW_ID(SO->idcode_str, SO->Label); 
      }
      SO->normdir = 1;
      if (ps->cs->talk_suma) {   /* strcutre setup during program default options parsing */
            SUMA_LH("Sending Ico"); /* send the surface */
            SUMA_SendSumaNewSurface(SO, ps->cs);
      }
   
      SUMA_LH("An example for modifying mesh and redisplaying");
      cnt = 0;
      while (cnt < 20) {
         /* Do some mesh action */
         for (i=0; i<SO->N_Node*SO->NodeDim; ++i) SO->NodeList[i] *= 0.9;
            /* recalculate surface normals */
            SUMA_RECOMPUTE_NORMALS(SO); 
            if (ps->cs->Send) {
               if (!SUMA_SendToSuma (SO, ps->cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)) {
                  SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
               }
            }
         ++cnt;
      }
   } else {
      /* interpret command line commands */
      for (i=0; i<Opt->N_com; ++i) {
         if (LocalHead) {
            SUMA_LH("Have the following commands");
            fprintf(SUMA_STDERR,"Command %d: %s\n", i, Opt->com[i]);
         }
         if (!(exflag = SUMA_ProcessCommand(Opt->com[i], ps))) {
            fprintf(SUMA_STDERR,"Error %s: Failed in processing command\n%s\n", FuncName, Opt->com[i]); 
            exit(1);
         }   
         if (exflag == -1) { /*gone daddy gone */ 
            fprintf(SUMA_STDERR,"There's no more reason to exist.\nFarewell dear friends.\n");
            exit(0);
         }
      }
   }
   
   SUMA_LH("Freedom");
   /* you don't want to exit rapidly because the SUMA might not be done processing the last elements*/
   if (ps->cs->Send && !ps->cs->GoneBad) {
      /* cleanup and close connections */
      if (!SUMA_SendToSuma (SO, ps->cs, NULL, SUMA_NODE_XYZ, 2)) {
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCleanup failed");
      }
   }   
   if (ps) SUMA_FreeGenericArgParse(ps); ps = NULL;
   if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
   if (!SUMA_Free_CommonFields(SUMAg_CF)) SUMA_error_message(FuncName,"SUMAg_CF Cleanup Failed!",1);
   
   exit(0);
   
} 
