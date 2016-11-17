#include "SUMA_suma.h"
static int SUMA_drive_set_outstream(char *outfile);
static FILE *SUMA_drive_get_outstream(void);

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
"                 -com  recorder_cont -save_last lastone.jpg \\\n"
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
"       DriveSuma -com surf_cont -switch_cmode Dir \n"
"       DriveSuma -com surf_cont -view_dset n\n"
"       DriveSuma -com surf_cont -switch_dset blooby.curv.1D.dset \\\n"
"                      -view_surf_cont n -I_range -0.05 0.14\n"
"       DriveSuma -com surf_cont -load_cmap bbr.1D.cmap\n"
};

static char uDS_tract_cont[]={
"       #This uses one of the tract files output by FATCAT's demo.\n"
"       #and some tracts mask called triplets.niml.do\n"
"\n"
"       suma -tract DTI/o.NETS_OR_000.niml.tract &\n"
"       DriveSuma -com object_cont -view_object_cont y          \\\n"
"                 -com object_cont -2xmasks                     \\\n"
"                 -com object_cont -delete_all_masks            \\\n"
"                 -com object_cont -load_masks triplets.niml.mo   \n"
};
static char uDS_kill_suma[]={
               "       DriveSuma -com kill_suma\n"
};

static FILE *sumaout = NULL;             /* no default output stream */

void usage_DriveSuma (SUMA_GENERIC_ARGV_PARSE *ps, int detail)
{
      static char FuncName[]={"usage_DriveSuma"};
      char * s = NULL, *sio=NULL, *st = NULL, *sts = NULL, *snido=NULL;
      int i;
      s = SUMA_help_basics();
      snido = SUMA_NIDO_Info();
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
"        -surf_label S_LABEL: A label (identifier) to assign to the\n"
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
"        -surf_label S_LABEL: A label to identify the target \n"
"                           surface\n"
"        -xyz_1D COORDS.1D: A 1D formatted file containing a new \n"
"                           coordinate for each of the nodes \n"
"                           forming the surface. COORDS.1D must \n"
"                           have three columns.\n"
"                           Column selectors can be used here as \n"
"                           they are in AFNI.\n"
"        If you do not have the coordinates handy in a 1D file\n"
"        and would prefer to get them directly from a surface,\n"
"        you can substitute -xyz_1D COORDS.1D with any valid suma \n"
"        surface input option. For example, if you want to send\n"
"        the coords of surface surf.gii, you can just use -i surf.gii,\n"
"        in lieu of -node_xyz COORDS.1D\n"
"     + Example node_xyz (needs surface from 'Example show_surf')\n"
"        1- Create some variation on the coords of the surface\n"
"        2- Send new coordinates to SUMA\n"
"        3- Manipulate the x coordinate now\n"
"        4- Send new coordinates again to SUMA\n"
"        -------------------------------------\n"
" o get_label: have current label associated with current node printed\n"
" o set_outplug filename: redirect output to file instead of stdout\n"
"%s"
"\n"
" o viewer_cont: Apply settings to viewer or viewer controller\n"
"     + Optional parameters for action viewer_cont:\n"
"       (Parameter names reflect GUI labels or key strokes.)\n"
"        -autorecord RECORD_PREFIX: Set the autorecord prefix\n"
"                        See 'Ctrl+r' in suma's interactive help for\n"
"                        details.\n"
"                    You can can use this option to make different snapshots\n"
"                    go to different directories or filenames. For example:\n"
"           ... \n"
"               -com viewer_cont -autorecord left/Javier.ppm \\\n"
"                                -key 'ctrl+left' -key 'ctrl+r' \\\n"
"               -com viewer_cont -autorecord right/Javier.ppm \\\n"
"                                -key 'ctrl+right' -key 'ctrl+r' \\\n"
"           ...\n"
"        -bkg_col R G B: Set the color of the background to R G B triplet.\n"
"                        R G B values must be between 0 and 1\n"
"        -load_view VIEW_FILE: Load a previously\n"
"                              saved view file (.vvs).\n"
"                              Same as 'File-->Load View'\n"
"        -load_do   DO_FILE: Load a displayable object file\n"
"                            For detailed information on DO_FILE's format,\n"
"                            see the section under suma's  help (ctrl+h)\n"
"                            where the function of Ctrl+Alt+s is detailed.\n"
"        -do_draw_mask MASKMODE: Restrict where DO node-based objects are\n"
"                                displayed. MASKMODE is one of:\n"
"                          All: No restrictions\n"
"                          n3Crosshair: Crosshair node + 3 neighboring layers\n"
"                          n2Crosshair: Crosshair node + 2 neighboring layers\n"
"                          n1Crosshair: Crosshair node only\n"
"                          None: Show nothing.\n"
"                      See also Ctrl+p option in SUMA.\n"
"        -fixed_do NIML_DO_STRING: Load a fixed coordinate type NIML DO that \n"
"                     is defined by the string NIML_DO_STRING.\n"
"                     This is more convenient than specifying\n"
"                     a simple DO in a file. For example:\n"
"                  DriveSuma -com viewer_cont \\\n"
"                              -fixed_do \"<T text='Hi' coord='0.5 0.2 0'/>\"\n"
"               or the simpler:\n"
"                  DriveSuma -com viewer_cont \\\n"
"                              -fixed_do \"<T text='Up here' p=tlf/>\"\n"
"                  DriveSuma -com viewer_cont \\\n"
"                              -fixed_do \"<T text='Down there' p=bcf/>\"\n"
"\n"
"                     Repeated calls to -fixed_do would replace the previous\n"
"                     object with the new one. You could specify multiple DOs\n"
"                     by adding a qualifier string to the option -fixed_do.\n"
"                     For example:\n"
"                  DriveSuma -com viewer_cont \\\n"
"                          -fixed_do1 \"<T text='Tango' coord='0.5 0.2 0'/>\"\n"
"                  DriveSuma -com viewer_cont \\\n"
"                          -fixed_do2 \"<T text='ognaT' coord='0.2 0.2 0'/>\"\n"
"                  DriveSuma -com viewer_cont \\\n"
"                          -fixed_do1 \"<T text='-X-' coord='0.5 0.2 0'/>\"\n"
"                  DriveSuma -com viewer_cont \\\n"
"                          -fixed_do3 \"<Tex target='FRAME' \\\n"
"                                  filename='funstuff/face_afniman.jpg'/>\"\n"
"               or for a more useful example for how you can add a logo on \n"
"               the bottom right side and way back in the viewer:\n"
"                  DriveSuma -com viewer_cont \\\n"
"                          -fixed_do3 \"<I target='FRAME' \\\n"
"                               coord   = '1 0 1' \\\n"
"                               h_align = 'right'  \\\n"
"                               v_align = 'bot'    \\\n"
"                               filename='funstuff/face_afniman.jpg'/>\"\n"
"\n"
"               For more information about DOs, see NIDO section below \n"
"               (visible with -help option) and demo script @DO.examples.\n"
"\n"
"        -Fixed_do NIML_DO_STRING: Same as -fixed_do, but spits out some \n"
"                     debugging info.\n"
"        -mobile_do NIML_DO_STRING: Mobile version of -fixed_do\n"
"        -Mobile_do NIML_DO_STRING: Mobile version of -Fixed_do\n"
   , uDS_show_surf, uDS_node_xyz );
if (detail > 1) { 
   printf(
"\n"
" ---------------------------------------------\n"
" Details for %s"
" ---------------------------------------------\n"
"\n"
      , snido);
}
   printf(
"        -key KEY_STRING: Act as if the key press KEY_STRING\n"
"                         was applied in the viewer.\n"
"                         ~ Not all key presses from interactive\n"
"                         mode are allowed here.\n"
"                         ~ Available keys and their variants are:\n"
"                         [, ], comma (or ','), period (or '.'), space,\n"
"                         a, b, d, G, j, m, n, p, r, t, z, \n"
"                         up, down, left, right, and F1 to F12.\n"
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
"                    DriveSuma -com viewer_cont '-key:v\"0.8 0 10.3\"' ctrl+j\n"
"                  In another example, say you want to jump to node 54 on the\n"
"                  right hemisphere (hence the 'R' in '54R'), then you would\n"
"                  execute:\n"
"                    DriveSuma -com viewer_cont '-key:v54R' j\n"
"        -viewer VIEWER: Specify which viewer should be acted \n"
"                        upon. Default is viewer 'A'. Viewers\n"
"                        must be created first (ctrl+n) before\n"
"                        they can be acted upon.\n"
"                        You can also refer to viewers with integers\n"
"                        0 for A, 1 for B, etc.\n"
"                        For -viewer to take effect it must be in the\n"
"                        same -com viewer_cont ... commands. For example:\n"
"               ... -com viewer_cont -viewer B -viewer_size 600 900 ...\n"
"        -viewer_width or (-width) WIDTH: Set the width in pixels of\n"
"                                     the current viewer.\n"
"        -viewer_height or (-height) HEIGHT: Set the height in pixels of\n"
"                                     the current viewer.\n"
"        -viewer_size WIDTH HEIGHT : Convenient combo of -viewer_width \n"
"                                    and -viewer_height\n"
"        -viewer_position X Y: Set position on the screen\n"
"        -controller_position X Y: Set position of the object (surface)\n"
"                                  controller on the screen\n"
"        -inout_notify y/n: Turn on or off function call tracing\n"
"        -N_foreg_smooth n: Number of foreground smoothing iterations\n"
"                           Same as suma's interactive '8' key or what\n"
"                           you'd set with env: SUMA_NumForeSmoothing\n"
"        -N_final_smooth n: Number of final color smoothing iterations\n"
"                           Same as suma's interactive '*' key or what\n"
"                           you'd set with env: SUMA_NumForeSmoothing\n" 
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
" o object_cont: Apply settings to object controller.\n"
" o surf_cont: Apply settings to surface controller.\n"
"     Note that for most cases, the use of object_cont and surf_cont is\n"
"     interchangeable.\n"
"     + Optional parameters for action surf_cont:\n"
"       (Parameter names reflect GUI labels.)\n"  
"       -surf_label S_LABEL: A label to identify the target surface\n"
"       -load_dset DSET: Load a dataset\n"
"           ! NOTE: When using -load_dset you can follow it\n"
"                   with -surf_label in order to attach\n"
"                   the dataset to a particular target surface.\n"
"       -view_surf y/n: Show or hide surface S_LABEL\n"
"       -RenderMode V/F/L/P/H: Set the render mode for surface S_LABEL.\n"
"       -TransMode V/0/../16: Set the transparency mode for surface S_LABEL.\n"
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
"       -view_object_cont y/n: View object controller\n"
"       -masks: Equivalent of pressing 'Masks' in tract controller\n"
"       -2xmasks: Equivalent of pressing 'Masks' twice in tract controller\n"
"       -delete_all_masks: Well, delete all the masks.\n"
"       -load_masks: Equivalent of pressing 'Load Masks' in masks controller\n"
"       -save_masks: Equivalent of pressing 'Save Masks' in masks controller\n"
"       -switch_surf S_LABEL: switch state to that of surface \n"
"                           labeled S_LABEL and make that surface \n"
"                           be in focus.\n"
"       -switch_dset DSET: switch dataset to DSET\n"
"       -view_dset y/n: Set view toggle button of DSET\n"
"       -1_only y/n: Set 1_only toggle button of DSET\n"
"       -switch_cmap CMAP: switch colormap to CMAP\n"
"       -switch_cmode CMODE: switch color mapping mode to CMODE\n"
"       -load_cmap CMAP.1D.cmap: load and switch colormap in \n"
"                                file CMAP.1D.cmap\n"
"       -I_sb ISB: Switch intensity to ISBth column (sub-brick)\n"
"       -I_range IR0 IR1: set intensity range from IR0 to IR1.\n"
"                         If only one number is given, the range\n"
"                         is symmetric from -|IR0| to |IR0|.\n"
"       -shw_0 y/n      or \n" 
"       -show_0 y/n: Set shw 0 toggle button of DSET.\n"
"       -Dsp MODE: Set the viewing mode of the current DSET.\n"
"                  MODE is one of XXX, Con, Col, or 'C&C' \n"
"                      (single quotes necessary for 'C&C' MODE).\n"
"                  This is equivalent to setting the 'Dsp' menu button\n"
"                  in the surface controller. The option is applied\n"
"                  to the current DSET on the selected surface.\n"
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
"       -Opa OPA: Set the opacity factor.\n"
"       -Clst RAD AREA: Set the clustering parameters\n"
"       -UseClst y/n: Turn on/off clustering\n"
"       -setSUMAenv \"'ENVname=ENVvalue'\": Set an ENV in SUMA. Note that\n"
"                      most SUMA env need to be set at SUMA's launch time. \n"
"                      Setting the env from DriveSuma may not achieve what \n" 
"                      you want, so consider using suma's -setenv instead.\n"
"       -write_surf_cont_help FILE.txt: Write help output for surface \n"
"                      controller uses into file FILE.txt (in append mode)\n"
"                      Make sure the surface controller is open before you\n"
"                      use this command.\n"
"       -write_surf_cont_sphinx_help FILE.rst: Same as -write_surf_cont_help,\n"
"                      but write SPHINX formatted RST file.\n"
"       -snap_surf_cont_widgets FROOT: Takes snapshots of various widget \n"
"                                      groupings and save them under FROOT*\n"
"       Also, in the same vein as -write_surf_cont_help, \n"
"       -write_surf_cont_sphinx_help, and -snap_surf_cont_widgets you have:\n"
"       -write_vol_cont_help\n"
"       -write_vol_cont_sphinx_help \n"
"       -snap_vol_cont_widgets\n"
"       -write_tract_cont_help\n"
"       -write_tract_cont_sphinx_help \n"
"       -snap_tract_cont_widgets\n"
"       -write_mask_cont_help\n"
"       -write_mask_cont_sphinx_help \n"
"       -snap_mask_cont_widgets\n"
"       -write_graph_cont_help\n"
"       -write_graph_cont_sphinx_help \n"
"       -snap_graph_cont_widgets\n"
"       -write_roi_cont_help\n"
"       -write_roi_cont_sphinx_help \n"
"       -snap_roi_cont_widgets\n"
"       -write_suma_cont_help\n"
"       -write_suma_cont_sphinx_help \n"
"       -snap_suma_cont_widgets\n"
"       -write_mouse_keyb_help FILE.txt: Write help output for mouse and \n"
"                      keyboard shortcuts.\n"
"       -write_mouse_keyb_sphinx_help FILE.rst: Same as -write_mouse_keyb_help\n"
"                      , but write SPHINX formatted RST file.\n"
"       -write_mouse_cmap_keyb_help FILE.txt: Write help output for mouse and \n"
"                      keyboard shortcuts.\n"
"       -write_mouse_cmap_keyb_sphinx_help FILE.rst: Same\n"
"                      as -write_mouse_cmap_keyb_help, but write SPHINX \n"
"                      formatted RST file.\n"
"\n"
"     + Example surf_cont (assumes all previous examples have\n"
"       been executed and suma is still running).\n"
"       - Obvious chicaneries to follow:\n"
"       --------------------------------\n"
"%s"
"\n"
"     + Example for loading masks onto tracts\n"
"       -------------------------------------\n"
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
"   Crashes: It is possible for SUMA to crash under certain combinations\n"
"            of commands that involve opening X windows followed by\n"
"            some command. For example, suma might crash with:\n"
"         DriveSuma   -com viewer_cont  -viewer_size 600 600 -key 'ctrl+n'\n"
"            Splitting such a command into two DriveSuma instances gets\n"
"            around the problem:\n"
"         DriveSuma   -com viewer_cont  -viewer_size 600 600 \n"
"         DriveSuma   -com viewer_cont  -key 'ctrl+n'\n"
"\n"
"Options:\n"
"--------\n"
"   -echo_edu: Echos the entire command line (without -echo_edu)\n"
"              for edification purposes\n"
"   -echo_nel_stdout: Spit out the NIML object being sent to SUMA for \n"
"   -echo_nel_stderr: edification purposes. These two options are meant\n"
"                     to help motivate the example in HalloSuma.\n"
"                     You need to have SUMA up and listening for this option\n"
"                     to take effect.\n"
"            Example: DriveSuma -echo_nel_stdout -com viewer_cont '-key:v28' j\n"
"   -echo_nel FILE: Write the elements to FILE.\n"
"                   You can also use stdout or stderr for FILE.\n"
"   -examples: Show all the sample commands and exit\n"
"   -help: All the help, in detail.\n"
"       ** NOTE: You should also take a look at scripts @DO.examples and \n"
"          @DriveSuma for examples. Suma's interactive help (ctrl+h) for\n"
"          the kinds of controls you can have with -key option.\n"
"   -h: -help, with slightly less detail\n"
"   -help_nido: Show the help for NIML Displayable Objects and exit.\n"
"               Same as suma -help_nido\n"
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
               , uDS_viewer_cont, uDS_recorder_cont, uDS_surf_cont, 
                 uDS_tract_cont,
      (detail> 1) ? sio:"use -help for I/O detail\n",  
      (detail> 1) ? s:"use -help for misc. help basics\n");
      SUMA_free(s); s = NULL; SUMA_free(st); st = NULL; SUMA_free(sio); sio = NULL;       
      if (snido) SUMA_free(snido); snido=NULL;
      /* s = SUMA_New_Additions(0, 1); 
         printf("%s\n", s);SUMA_free(s); s = NULL; */
      printf("       Ziad S. Saad SSCC/NIMH/NIH saadz@mail.nih.gov     \n");
      exit(0);
}


int SUMA_ProcessCommand(char *com, SUMA_COMM_STRUCT *cs, char *EchoNel)
{
   static char FuncName[]={"SUMA_ProcessCommand"};
   int i, suc;
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
      SUMA_SendSumaNewSurface(SO, cs);
      if (EchoNel) 
         SUMA_S_Warn("Sorry, no echo for show_surf. Complain to author.");
   } else if (strcmp((act), "node_xyz") == 0) {
      SO = SUMA_NodeXYZComToSO(com);
      SUMA_LHv("Sending XYZ to %s", SO->Label);
      if (!SUMA_SendToSuma (SO, cs, (void *)SO->NodeList, SUMA_NODE_XYZ, 1)){
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      if (EchoNel) 
         SUMA_S_Warn("Sorry, no echo for node_xyz. Complain to author.");
   } else if (strcmp((act), "load_dset") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending LoadDset to suma");
      if (!SUMA_SendToSuma (SO, cs, (void *)ngr,SUMA_ENGINE_INSTRUCTION, 1)){
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      if (EchoNel) NEL_WRITE_TX(ngr, EchoNel, suc);
      NI_free_element(ngr); ngr = NULL;
   } else if (strcmp((act), "load_col") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending LoadCol to suma");
      if (!SUMA_SendToSuma (SO, cs, (void *)ngr,SUMA_ENGINE_INSTRUCTION, 1)){
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      if (EchoNel) NEL_WRITE_TX(ngr, EchoNel, suc);
      NI_free_element(ngr); ngr = NULL;
   } else if (strcmp((act), "surf_cont") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending SetSurfCont to suma");
      if (!SUMA_SendToSuma (SO, cs, (void *)ngr,SUMA_ENGINE_INSTRUCTION, 1)){
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      if (EchoNel) NEL_WRITE_TX(ngr, EchoNel, suc);
      NI_free_element(ngr); ngr = NULL; 
   } else if (strcmp((act), "object_cont") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending SetObjectCont to suma");
      if (!SUMA_SendToSuma (SO, cs, (void *)ngr,SUMA_ENGINE_INSTRUCTION, 1)){
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      if (EchoNel) NEL_WRITE_TX(ngr, EchoNel, suc);
      NI_free_element(ngr); ngr = NULL; 
   } else if (strcmp((act), "viewer_cont") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending SetViewerCont to suma");
      if (!SUMA_SendToSuma (SO, cs, (void *)ngr,SUMA_ENGINE_INSTRUCTION, 1)){
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      if (EchoNel) NEL_WRITE_TX(ngr, EchoNel, suc);      
      NI_free_element(ngr); ngr = NULL;
   } else if (strcmp((act), "recorder_cont") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending SetRecorderCont to suma");
      if (!SUMA_SendToSuma (SO, cs, (void *)ngr,SUMA_ENGINE_INSTRUCTION, 1)){
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      if (EchoNel) NEL_WRITE_TX(ngr, EchoNel, suc);
      NI_free_element(ngr); ngr = NULL;
   } else if (strcmp((act), "set_outplug") == 0) {
      char **argt=NULL;
      char *outplug=NULL;
      int argtc = 0;

      /* change com to a bunch of arguments */
      argt = SUMA_com2argv(com, &argtc); 

      if (argtc != 2) {
         SUMA_S_Errv("Expecting one value after sleep, have %d\n%s\n", 
                        argtc-1, argt[1]);
         ans = NOPE;
      }
      outplug = argt[1];
      SUMA_drive_set_outstream(outplug);
   } else if (strcmp((act), "get_label") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending get_label to suma");
      if (!SUMA_SendToSuma (SO, cs, (void *)ngr,SUMA_ENGINE_INSTRUCTION, 1)){
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      if (EchoNel) NEL_WRITE_TX(ngr, EchoNel, suc);      
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
        for (i=1; i<argtc; ++i) 
            msg = SUMA_append_replace_string(msg, argt[i], " ", 1);
        SUMA_PAUSE_PROMPT(msg);
      }
      if (msg) SUMA_free(msg); msg = NULL;
      argt = SUMA_free_com_argv(argt, &argtc);
   } else if (strcmp((act), "kill_suma") == 0) {
      if (!(ngr = SUMA_ComToNgr(com, act))) {
         SUMA_S_Err("Failed to process command."); SUMA_RETURN(NOPE); 
      }
      SUMA_LH("Sending kill_suma to suma");
      if (!SUMA_SendToSuma (SO, cs, (void *)ngr,SUMA_ENGINE_INSTRUCTION, 1)){
         SUMA_SL_Warn("Failed in SUMA_SendToSuma\nCommunication halted.");
      }
      if (EchoNel) NEL_WRITE_TX(ngr, EchoNel, suc);
      
      if (cs) {
         if (1) {
            /* go bad anyway without waiting for stream to go bad 
            because suma will be dying. No point in waiting if
            if there was a communication error. 
                  Added per Yaroslav Halchenko's request     Sept 2013 */
            cs->GoneBad = YUP;
         } else {
            SUMA_Wait_Till_Stream_Goes_Bad(cs, 100, 1000, 0);
         }
      }
      NI_free_element(ngr); ngr = NULL;
      ans = -1; 
   } else {
      fprintf(SUMA_STDERR, 
               "Error %s: Action '%s' not supported.\n", FuncName, act);
      ans = NOPE;
   }
   
   if (SO) SUMA_Free_Surface_Object(SO); SO = NULL;
   if (act) SUMA_free(act);
   SUMA_RETURN(ans);
}

SUMA_GENERIC_PROG_OPTIONS_STRUCT *SUMA_DriveSuma_ParseInput(
                  char *argv[], int argc, SUMA_GENERIC_ARGV_PARSE *ps)
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
   Opt->s = NULL;
   kar = 1;
   brk = NOPE;
	while (kar < argc) { /* loop across command line options */
		SUMA_LH("Parsing command line at %d/%d: %s...\n", kar, argc, argv[kar]);
		if (ps) {
         if (strcmp(argv[kar], "-h") == 0 || strcmp(argv[kar], "-help") == 0) {
			    usage_DriveSuma(ps, strlen(argv[kar]) > 3 ? 2:1);
             exit (0);
		   }

		   SUMA_SKIP_COMMON_OPTIONS(brk, kar);
      }
      if (strcmp(argv[kar], "-echo_nel_stdout") == 0) {
         Opt->s = SUMA_copy_string("stdout:");
         brk = YUP;
      }

      if (strcmp(argv[kar], "-echo_nel_stderr") == 0) {
         Opt->s = SUMA_copy_string("stderr:");
         brk = YUP;
      }
      if (strcmp(argv[kar], "-echo_nel") == 0) {
         if (kar+1 >= argc)
         {
            fprintf (SUMA_STDERR, "need a parameter after -echo_nel \n");
            exit (1);
         }
         ++kar;
         if (strcmp(argv[kar], "stdout")==0) {
            Opt->s = SUMA_copy_string("stdout:");
         } else if (strcmp(argv[kar], "stderr")==0) {
            Opt->s = SUMA_copy_string("stderr:");
         } else {
            Opt->s = SUMA_append_replace_string("file:",argv[kar],"",0);
         }         
         brk = YUP;
      }
      
      if (strcmp(argv[kar], "-help_nido") == 0) {
         char *s = SUMA_NIDO_Info();
         fprintf (SUMA_STDOUT,"%s\n", s); 
         SUMA_free(s); s = NULL;
         exit (0);
      }

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
            fprintf (SUMA_STDERR, "need love after -com \n");
            exit (1);
         }
         
         Opt->com = (char **)SUMA_realloc(Opt->com, 
                                 sizeof(char *)*(Opt->N_com+1));
         Opt->com[Opt->N_com] = NULL;
         ++kar;
         do { 
            SUMA_LH("Now getting %d/%d: %s", kar, argc, argv[kar]);
            Opt->com[Opt->N_com] = 
               SUMA_append_replace_string (Opt->com[Opt->N_com], 
                                           argv[kar], " ", 1);
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
      
      
      if (!brk && (!ps || !ps->arg_checked[kar])) {
			SUMA_S_Errv("Option %s not valid, or requires preceding -com option\n"
                     "Try -help for usage\n", argv[kar]);
         suggest_best_prog_option(argv[0], argv[kar]);
			if (Opt) Opt = SUMA_Free_Generic_Prog_Options_Struct(Opt);
         SUMA_RETURN(NULL); 
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   SUMA_RETURN(Opt);
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
   double dv3[3], tmpd, dv12[12];
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
      
      if (!brk && (  (strcmp(argt[kar], "-setSUMAenv") == 0) ))
      {
         int ienv = 0, closed = 0;
         char attr[32]={""}, *aval=NULL;
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, 
"need a string with 'NAME = VALUE' after -setSUMAenv (obey quotes and spaces) %d %d\n", kar, argtc);
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ienv = -1;
         do {
            ++ienv;
            sprintf(attr,"ENV.%d", ienv);
         } while (NI_get_attribute(ngr,attr));
         /* Now search for a quote before the name */
         ++kar;
         if (argt[kar][0] != '\'' && argt[kar][0] != '\"') {
            SUMA_S_Errv("You must enclose env expression with ' or \" quotes\n"
                        "Have open %s\n", argt[kar]);
            SUMA_RETURN(0);
         } 
         
         aval = SUMA_copy_quoted(argt[kar],NULL,'\0','\0', 1, 0, &closed);
         
         if (!aval) {
            SUMA_S_Err("Failed to get env value");
            SUMA_RETURN(0);
         }
         SUMA_LHv("Adding >>%s<< %d \n", aval, closed); 
         if (!closed) {
            SUMA_S_Errv("You must enclose env expression with ' or \" quotes\n"
                        "Have unterminated %s\n", aval);
            SUMA_RETURN(0);
         } 
         NI_set_attribute(ngr, attr, aval);
         SUMA_free(aval); aval=NULL;
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
      
      if (!brk && ( (strcmp(argt[kar], "-switch_cmode") == 0) ))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, 
                     "need a color mapping mode after -switch_cmode \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         if (strcasecmp(argt[kar],"nn") &&
             strcasecmp(argt[kar],"dir") &&
             strcasecmp(argt[kar],"int")) {
            fprintf(SUMA_STDERR,
                  "CMODE %s not allowed. Choose from 'NN', 'Dir', or 'Int'\n", 
                  argt[kar]);   
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "switch_cmode", argt[kar]);
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
      
      if (!brk && ( (strcmp(argt[kar], "-bkg_col") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need at least 3 values after -bkg_col \n");
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
         if (!stmp || nums < 3 || nums > 4) {
            SUMA_S_Err( "Bad format for -bkg_col option values;\n"
                        " 3 or 4 values allowed.");
            SUMA_RETURN(0);
         }
         if (nums == 3) {
            stmp = SUMA_append_replace_string(stmp, "1.0", " ", 1); ++nums;
         }
         nv = SUMA_StringToNum(stmp, (void *)dv12, 12,nums);
         if (nv < 3 || nv > 4) {
            SUMA_S_Err("Bad range string.");
            SUMA_RETURN(0);
         }else {
            /* have range, set it please */
            SUMA_free(stmp); stmp = NULL; 
            stmp = (char *)SUMA_malloc(sizeof(char)*nv*50);
            sprintf(stmp,"%f , %f, %f, %f", dv12[0], dv12[1], dv12[2], dv12[3]);
            NI_set_attribute(ngr, "bkg_col", stmp);
            SUMA_LHv("bkg_col of %s\n", stmp);
            SUMA_free(stmp); stmp = NULL;
         }
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-autorecord") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a prefix after -autorecord\n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "autorecord", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }

      if (!brk && ( (strcmp(argt[kar], "-Dsp") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need XXX, Col, Con, or C&C after -Dsp \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "Dsp", argt[++kar]);
         argt[kar][0] = '\0';
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
      
      if (!brk && ( (strcmp(argt[kar], "-Opa") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a value after -Opa \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "Opa", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-Clst") == 0) ||
                    (strcmp(argt[kar], "-Clust") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need two values after -Clust \n");
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
            SUMA_S_Err( "Bad format for -Clst option values;\n"
                        " 2 and only 2 values allowed.");
            SUMA_RETURN(0);
         }
         nv = SUMA_StringToNum(stmp, (void *)dv3, 3,2);
         if (nv < 1 || nv > 2) {
            SUMA_S_Err("Bad Clst string.");
            SUMA_RETURN(0);
         }else {
            /* have range, set it please */
            SUMA_free(stmp); stmp = NULL; 
            stmp = (char *)SUMA_malloc(sizeof(char)*nv*50);
            sprintf(stmp,"%f , %f", dv3[0], dv3[1]);
            NI_set_attribute(ngr, "Clst", stmp);
            SUMA_LHv("Clst of %s\n", stmp);
            SUMA_free(stmp); stmp = NULL;
         }
         brk = YUP;
      }
      
      if (!brk && (  (strcmp(argt[kar], "-UseClst") == 0) ||
                     (strcmp(argt[kar], "-UseClust") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a 'y/n' after -UseClust \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         if (argt[kar][0] == 'y' || argt[kar][0] == 'Y')  
            NI_set_attribute(ngr, "UseClst", "y");
         else if (argt[kar][0] == 'n' || argt[kar][0] == 'N')  
            NI_set_attribute(ngr, "UseClst", "n");
         else {
            fprintf (SUMA_STDERR, "need a 'y/n' after -UseClust \n");
            SUMA_RETURN(0);
         }
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
      
      if (!brk && (strcmp(argt[kar], "-RenderMode") == 0))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a valid string after -RenderMode \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         if (argt[kar][0] == 'V' || argt[kar][0] == 'v')  
            NI_set_attribute(ngr, "view_surf", "Viewer");
         else if (argt[kar][0] == 'F' || argt[kar][0] == 'f')  
            NI_set_attribute(ngr, "view_surf", "Fill");
         else if (argt[kar][0] == 'L' || argt[kar][0] == 'l')  
            NI_set_attribute(ngr, "view_surf", "Line");
         else if (argt[kar][0] == 'P' || argt[kar][0] == 'p')  
            NI_set_attribute(ngr, "view_surf", "Points");
         else if (argt[kar][0] == 'H' || argt[kar][0] == 'h')  
            NI_set_attribute(ngr, "view_surf", "Hide");
         else {
            fprintf (SUMA_STDERR, "need a valid string after -view_surf \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-TransMode") == 0))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, 
                     "need a valid string/value after -TransMode \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         if (argt[kar][0] == 'V' || argt[kar][0] == 'v')  
            NI_set_attribute(ngr, "trans_surf", "Viewer");
         else if (strstr(argt[kar],"%")) {
            char stmp[32]={""};
            for (N=0; (N<strlen(argt[kar]) && argt[kar][N]!='%' && N<16); ++N) {
               stmp[N] = argt[kar][N];
            }
            stmp[N]='\0';
            N = (int)strtol(stmp, NULL,10);
            if (N < 0 || N > 100) {
               fprintf (SUMA_STDERR, 
                        "Tansparency percentage should be between 0 and 100\n"
                        "have %d from %s\n", N, argt[kar]);
               SUMA_RETURN(0);
            }
            N = (int)(N*16.0/100);
            NI_SET_INT(ngr, "trans_surf", N);
         } else { /* read an int, should be between 0 and 16 */
            N = (int)strtol(argt[kar], NULL,10);
            if (N < 0 || N > 100) {
               fprintf (SUMA_STDERR, 
                        "Tansparency index should be between 0 and 16\n"
                        "have %d from %s\n", N, argt[kar]);
               SUMA_RETURN(0);
            }
            NI_SET_INT(ngr, "trans_surf", N);
         } 
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-masks") == 0))
      {
         NI_set_attribute(ngr, "Masks", "Click!");
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-2xmasks") == 0))
      {
         NI_set_attribute(ngr, "2xMasks", "Click!");
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-delete_all_masks") == 0))
      {
         NI_set_attribute(ngr, "Delete_All_Masks", "Click!");
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-load_masks") == 0))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR,
                     "need a tract masks filename with -Load_Masks \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "Load_Masks", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-save_masks") == 0))
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR,
                     "need a tract masks filename with -Save_Masks \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         NI_set_attribute(ngr, "Save_Masks", argt[++kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ((strcmp(argt[kar], "-view_surf_cont") == 0) ||
                   (strcmp(argt[kar], "-view_object_cont") == 0)) )
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
      
      if (!brk && (strcmp(argt[kar], "-write_suma_cont_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_suma_cont_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Suma_Cont_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-snap_suma_cont_widgets") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -snap_suma_cont_widgets \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Snap_Suma_Cont_Widgets", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_suma_cont_sphinx_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_suma_cont_sphinx_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Suma_Cont_Sphinx_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      if (!brk && (strcmp(argt[kar], "-write_surf_cont_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_surf_cont_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Surf_Cont_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-snap_surf_cont_widgets") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -snap_surf_cont_widgets \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Snap_Surf_Cont_Widgets", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_surf_cont_sphinx_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_surf_cont_sphinx_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Surf_Cont_Sphinx_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_tract_cont_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_tract_cont_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Tract_Cont_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-snap_tract_cont_widgets") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -snap_tract_cont_widgets \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Snap_Tract_Cont_Widgets", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_tract_cont_sphinx_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_tract_cont_sphinx_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Tract_Cont_Sphinx_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_mask_cont_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_mask_cont_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Mask_Cont_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-snap_mask_cont_widgets") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -snap_mask_cont_widgets \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Snap_Mask_Cont_Widgets", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_mask_cont_sphinx_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_mask_cont_sphinx_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Mask_Cont_Sphinx_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_vol_cont_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_vol_cont_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Vol_Cont_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-snap_vol_cont_widgets") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -snap_vol_cont_widgets \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Snap_Vol_Cont_Widgets", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_vol_cont_sphinx_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_vol_cont_sphinx_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Vol_Cont_Sphinx_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_graph_cont_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_graph_cont_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Graph_Cont_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-snap_graph_cont_widgets") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -snap_graph_cont_widgets \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Snap_Graph_Cont_Widgets", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_graph_cont_sphinx_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_graph_cont_sphinx_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Graph_Cont_Sphinx_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_roi_cont_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_roi_cont_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_ROI_Cont_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-snap_roi_cont_widgets") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -snap_roi_cont_widgets \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Snap_ROI_Cont_Widgets", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_roi_cont_sphinx_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_roi_cont_sphinx_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_ROI_Cont_Sphinx_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      
      if (!brk && (strcmp(argt[kar], "-write_mouse_keyb_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_mouse_keyb_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Mouse_Keyb_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_mouse_keyb_sphinx_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_mouse_keyb_sphinx_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Mouse_Keyb_Sphinx_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_mouse_cmap_keyb_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need a filename after -write_mouse_cmap_keyb_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Mouse_Cmap_Keyb_Help", argt[kar]);
         
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && (strcmp(argt[kar], "-write_mouse_cmap_keyb_sphinx_help") == 0))
      {
         if (kar+1 >= argtc)
         {
            SUMA_S_Err("need filename w/ -write_mouse_cmap_keyb_sphinx_help \n");
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';
         ++kar;
         NI_set_attribute(ngr, "Write_Mouse_Cmap_Keyb_Sphinx_Help", argt[kar]);
         
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
      
      if (!brk && ( (strcmp(argt[kar], "-N_foreg_smooth") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need an integer after -N_foreg_smooth \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         ++kar; nums = (int)strtol(argt[kar], NULL,10);
         if (nums < 0 || nums > 500) {
            SUMA_S_Errv("Bad integer for option -N_foreg_smooth %s\n",
                     argt[kar]);
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "N_foreg_smooth", argt[kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-N_final_smooth") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need an integer after -N_final_smooth \n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         ++kar; nums = (int)strtol(argt[kar], NULL,10);
         if (nums < 0 || nums > 500) {
            SUMA_S_Errv("Bad integer for option -N_final_smooth %s\n",
                     argt[kar]);
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "N_final_smooth", argt[kar]);
         argt[kar][0] = '\0';
         brk = YUP;
      }
      
      if (!brk && ( (strcmp(argt[kar], "-inout_notify") == 0) ) )
      {
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, "need a value after -inout_notify\n");
            SUMA_RETURN(0);
         }
         
         argt[kar][0] = '\0';
         ++kar;
         if (argt[kar][0] == 'y' || argt[kar][0] == 'Y')  
            NI_set_attribute(ngr, "inout_notify", "y");
         else if (argt[kar][0] == 'n' || argt[kar][0] == 'N')  
            NI_set_attribute(ngr, "inout_notify", "n");
         else {
            fprintf (SUMA_STDERR, "need a 'y/n' after -view_surf \n");
            SUMA_RETURN(0);
         }
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


      if (!brk && ( (strncmp(argt[kar], "-fixed_do",9) == 0) ||
                    (strncmp(argt[kar], "-Fixed_do",9) == 0) ||
                    (strncmp(argt[kar], "-mobile_do",10) == 0) ||
                    (strncmp(argt[kar], "-Mobile_do",10) == 0) ) )
      {
         char *sbuf=NULL, *qar=NULL;
         NI_element *nel=NULL;
         int showit=0;
         if (argt[kar][1] == 'F' || argt[kar][1] == 'M') showit=1;
         if (kar+1 >= argtc)
         {
            fprintf (SUMA_STDERR, 
                     "need a string after -fixed_do (or -mobile_do)\n");
            SUMA_RETURN(0);
         }
         qar = UNIQ_hashcode(argt[kar]);
         if (strstr(argt[kar],"ixed_do")) {
            sbuf = SUMA_copy_string("<nido_head coord_type = 'fixed'\n"
                                 "default_color = '1.0 1.0 1.0'\n"
                                 "default_font = 'he18'\n"
                                 "idcode_str = ");
         } else {
            sbuf = SUMA_copy_string("<nido_head coord_type = 'mobile'\n"
                                 "default_SO_label = 'CURRENT'\n"
                                 "bond = 'surface'\n"
                                 "idcode_str = ");
         }
         sbuf = SUMA_append_replace_string(sbuf,qar,"",1);
         argt[kar][0] = '\0';
         free(qar); qar=NULL;
         sbuf = SUMA_append_replace_string(sbuf,"/>\n","\n",1);
         ++kar;
         if (!(qar = args_in_niml_quotes(argt, &kar, argtc, 1))) {
            SUMA_S_Errv("Could not find niml element starting at %s\n",
                        argt[kar]);
         } else {
            /* check that the new element is OK, that function reads 
               just one element*/
            if (!(nel=NI_read_element_fromstring(qar))) {
               SUMA_S_Errv("Could not parse -fixed_do %s\n"
                  "Try experimenting with niccc -s to get the syntax right.\n",
                        argt[kar]);
               exit(1);
            }
            if (showit) SUMA_ShowNel(nel);
            if (nel) NI_free_element(nel); nel=NULL;   
            sbuf = SUMA_append_replace_string(sbuf, qar,"",1);
            SUMA_free(qar); qar=NULL;
         }
         
         NI_set_attribute(ngr, "DO_FileName", sbuf);
         SUMA_free(sbuf); sbuf=NULL;
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
         if (atoi(argt[kar]) < 0 || atoi(argt[kar]) > 4000) {
            fprintf (SUMA_STDERR, 
               "Have %d for X in pixels. Range [0 4000].! \n", atoi(argt[kar]));
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "WindX", argt[kar]);
         argt[kar][0] = '\0';++kar;
         if (atoi(argt[kar]) < 0 || atoi(argt[kar]) > 4000) {
            fprintf (SUMA_STDERR, 
               "Have %d for Y in pixels!  Range [0 4000].\n", atoi(argt[kar]));
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "WindY", argt[kar]);
         NI_set_attribute(ngr, "DoViewerSetup","y"); /* flag indicating 
                                                      need to setup viewer, 
                                                      a la vvs */
         argt[kar][0] = '\0';
         brk = YUP;
      }
      if (  !brk && 
            (  (strcmp(argt[kar], "-controller_position") == 0)  ) )
      {
         if (kar+2 >= argtc)
         {
            fprintf (SUMA_STDERR, "need 2 numbers after %s \n", argt[kar]);
            SUMA_RETURN(0);
         }
         argt[kar][0] = '\0';++kar;
         if (atoi(argt[kar]) < 0 || atoi(argt[kar]) > 4000) {
            fprintf (SUMA_STDERR, 
               "Have %d for X in pixels. Range [0 4000].! \n", atoi(argt[kar]));
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "ContX", argt[kar]);
         argt[kar][0] = '\0';++kar;
         if (atoi(argt[kar]) < 0 || atoi(argt[kar]) > 4000) {
            fprintf (SUMA_STDERR, 
               "Have %d for Y in pixels!  Range [0 4000].\n", atoi(argt[kar]));
            SUMA_RETURN(0);
         }
         NI_set_attribute(ngr, "ContY", argt[kar]);
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
      if (tp) {
         argt = (char **)SUMA_realloc(argt, sizeof(char *)*(argtc+1)); 
         argt[argtc] = tp; tp = NULL; 
         ++argtc;
      }
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
      if (!brk && (  (strcmp(argt[kar], "-label") == 0) || 
                     (strcmp(argt[kar], "-surf_label") == 0) || 
                     (strcmp(argt[kar], "-so_label") == 0)) )
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
			SUMA_S_Err("Option %s not understood. Try -help for usage\n",
                    argt[kar]);
			exit (1);
		} else {	
			brk = NOPE;
			kar ++;
		}
   }
   
   if (!SO->NodeList) {
      SUMA_SurfaceObject *SOu=NULL;

      /* Perhaps user tried to go the -i route */
      Spec = SUMA_IO_args_2_spec(pst, &N_Spec);
      if (N_Spec == 0) {
         SUMA_S_Err("No -xyz_1D or input surface found.");
         exit(1);
      }
      if (N_Spec != 1) {
         SUMA_S_Err("Multiple spec at input.");
         exit(1);
      }

      SOu = SUMA_Load_Spec_Surf(Spec, 0, pst->sv[0], 0);
      if (!SOu) {
            fprintf (SUMA_STDERR,"Error %s:\n"
                                 "Failed to find surface\n"
                                 "in spec file. \n",
                                 FuncName );
            exit(1);

      }
      SO->N_Node = SOu->N_Node;
      SO->NodeDim = SOu->NodeDim;
      SO->NodeList = SOu->NodeList; SOu->NodeList = NULL;
      SUMA_Free_Surface_Object(SOu); SOu = NULL;
   }
   
   if (!SO->NodeList) {
      SUMA_S_Err("Have no XYZ coords! Did you use -node_xyz or -i, -spec, etc.");
      exit (1);
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
         NI_free_element(ngr); ngr = NULL;
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
			NI_free_element(ngr); ngr = NULL;
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

/* get the output file for plugout info */
static FILE *
SUMA_drive_get_outstream()
{
   char *suma_outfile;

   /* first time in set the output stream to stdout or environment variable */
   if(sumaout==NULL){
      /* get from environment variable if set */
      suma_outfile = my_getenv("SUMA_OUTPLUG");
      if(suma_outfile!=NULL) {
         SUMA_drive_set_outstream(suma_outfile);
      }
   }

   /* if still NULL output stream, set to default of stdout */
   if(sumaout==NULL) sumaout = stdout;

   return(sumaout);
}

/* set the output stream to a file rather than stdout*/
static int
SUMA_drive_set_outstream(char *outfile)
{
   /* if just passed a NULL, reset output to stdout */
   if(outfile==NULL){
      sumaout = stdout;
      return(-1);
   }

   /* check if resetting to stdout by string from plugout command */
   if(strcmp(outfile, "stdout")==0) {
      sumaout = stdout;
      return 0;
   }

    /* make sure this file name is a good one, and open it for append */
   if( THD_filename_ok(outfile) )
      sumaout = fopen(outfile, "a");

   /* something went wrong, so tell user and reset to stdout */
   if(sumaout==NULL){
      fprintf(stderr, "**** couldn't open outfile, resetting to stdout\n");
      sumaout = stdout;
      return(-1);
   }
   else {
    return 0;
   }
}



