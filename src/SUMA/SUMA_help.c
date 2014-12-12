#include "SUMA_suma.h"

extern SUMA_CommonFields *SUMAg_CF; 

static char s_ver[100];

static char * SUMA_ver2date(int ver)
{
   int yy, mm, dd;
   yy = ver/10000;
   mm = (ver % 10000) / 100;
   dd = ver % 100;
   sprintf(s_ver,"%d_%d_%d", yy, mm, dd);
   return(s_ver);
} 


/*!
   \brief Returns a string with the new additions and version information
   
   \param ver (float) v (v > 0) for info on version v alone 
                      0.0 just the latest version info
                      -1.0 for all versions
   \param StampOnly (SUMA_Boolean) Want version number and time stamp only ?
                      
   \return s (char *) the string, yall have to free it with SUMA_free
   \sa SUMA_New_Additions_perver
   
   - To add a new version, you must add a case statement in SUMA_New_Additions_perver
     AND add the version number in the beginning of SUMA_VERSION_VECTOR  in SUMA_DataSets.h
*/

static int verv[] = { SUMA_VERSION_VECTOR }; 

char * SUMA_New_Additions (int ver, SUMA_Boolean StampOnly)
{
   static char FuncName[]={"SUMA_New_Additions"};
   char *s = NULL;
   int i;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;

   SS = SUMA_StringAppend (NULL, NULL);
   
   #if 0 /* Stopped maintaining this list for a long time now. */
   if (ver == 0) { /* just the latest */
      s = SUMA_New_Additions_perver( verv[0], StampOnly);
      if (s) {
         SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;
      }
   } else if (ver < 0) {
      /* all history */
      SS = SUMA_StringAppend (SS, "All Version Info:\n"); 
      i = 0;
      while (verv[i] > 0) {
         s = SUMA_New_Additions_perver( verv[i], StampOnly);
         if (s) {
         SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;
         SS = SUMA_StringAppend (SS, "\n");
         }
         ++i;
      }
      
   } else {
      /* just for ver */
      s = SUMA_New_Additions_perver( ver, StampOnly);
      if (s) {
         SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;
      }
   }
   /* add the CVS tag            ZSS: Looks like nobody likes tags. 
                                      Compile Date is enough*/
   SS = SUMA_StringAppend_va (SS, "\nCVS tag:\n   %s\n", SUMA_VERSION_LABEL);
   #endif
   
   /* add the compile date */
   SS = SUMA_StringAppend_va (SS, "\nCompile Date:\n   %s\n",__DATE__);
   
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN(s);      
   
}

 
/*!
   \brief Returns a string with version information
   \param ver (float) Version number
   \param StampOnly (SUMA_Boolean) if YUP 
                     then return the time stamp of the version only)
   \return s (char *) the string, yall have to free it with SUMA_free
   \sa SUMA_New_Additions
   
   - To add a new version, you must add a case statement in SUMA_New_Additions_perver
     AND add the version number in the beginning of SUMA_VERSION_VECTOR  in SUMA_DataSets.h
*/
char * SUMA_New_Additions_perver (int ver, SUMA_Boolean StampOnly)
{
   static char FuncName[]={"SUMA_New_Additions_perver"};
   char *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   
   switch (ver) {
      /* Must modify SUMA_VERSION_VECTOR  in SUMA_DataSets.h when add a new case  */
      /*
      case XX:
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %s\n", SUMA_ver2date(ver)); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
            "New Programs:\n"
            "  + \n"
            "Modifications:\n"
            "  + \n");
         break; 
      */
      case 20060703:
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %s\n", SUMA_ver2date(ver)); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
         "New Programs:\n"
         "  + SurfDsetInfo: Program to display surface dataset information.\n"
         "  + AnalyzeTrace: Program to analyze the output of -trace option.\n"
         "  + DriveSuma: Program to control SUMA from the command line\n"
         "  + imcat: Program to catenate images.\n"
         "  + Surf2VolCoord: Surface-node to voxel correspondence.\n"
         "  + SurfDist: Program to calculate internodal distances.\n"
         "  + SpharmDeco: Spherical harmonics decomposition.\n"
         "  + SpharmReco: Spherical harmonics reconstruction.\n"
         "Modifications:\n"
         "  + SUMA:\n"
         "    o Addition of new Displayable Objects (DO)(ctrl+Alt+s)\n"
         "    o Allow replacement of pre-loaded DO and Dsets\n"
         "    o Support for .niml.dset as format for surface-based anlysis\n"
         "    o High resolution image saving with ctrl+r\n"
         "    o Bug fixes for support of niml dset format\n"
         "    o Use of '[i]' to select node index from surface dset\n"
         "    o Scroll lists for I T and B selectors in SUMA\n"
         "    o Graphing of dset content with 'g'\n"
         "    o Display of text and images, see suma -help_nido \n"
         "  + SurfDist:\n"
         "    o Output of node path along with shortest distance.\n"  
         "  + ConvertDset:\n"
         "    o Output of full dsets if needed\n"
         "  + ROIgrow:\n"
         "    o Grows regions separately, depending on labels.\n"
         "  + ROI2dataset:\n"
         "    o outputs full datasets if needed.\n"
         "  + SurfSmooth:\n"
         "    o Improved HEAT_05 method.\n"
         "    o New 'blurring to' a FWHM with HEAT_07 method.\n"
         "  + SurfFWHM:\n"
         "    o Estimating FWHM on the surface.\n" 
         "  + MapIcosahedron:\n"
         "    o Better handling of surface centers. \n"
            );
         break; 
      case 20041229:
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %s\n", SUMA_ver2date(ver)); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
            "New Programs:\n"
            "  + SurfClust: Program to find clusters of activation\n"
            "               on the surface.\n"
            "  + IsoSurface: Program to create isosurfaces from AFNI volumes.\n"
            "  + ConvexHull: Program to create the convex hull of a set of\n"
            "                points.\n"
            "  + 3dSkullStrip: Program to remove the skull from anatomical \n"
            "                  volumes.\n"
            "  + 3dCRUISEtoAFNI: Program to convert CRUISE volumes to AFNI\n"
            "  + 3dBRAIN_VOYAGERtoAFNI: Program to convert BrainVoyager .vmr\n"
            "                           volumes to AFNI\n"
            "  + SurfMesh: Program to increase or decrease a mesh's density.\n"
            "  + SurfMask: Program to find the volume enclosed by a surface.\n"
            "  + SurfToSurf: Program to interpolate between non-isotopic surfaces.\n"
            "  + ROIgrow: Program to expand an ROI on the surface.\n"
            "Modifications:\n"
            "  + SUMA:\n"
            "    o Slight modification to threshold scale.\n"
            "    o Added environment variable SUMA_ThresholdScalePower.\n"
            "    o Fixed a few kinks in the surface controller.\n"
            "    o Fixed ROI drawing trace on OSX.\n"
            "    o Added geodesic distance measurements in ROI drawing\n"
            "    controller.\n"
            "    o Suma can read surfaces specified on command line.\n"
            "    o Fixed bug reading AFNI generated niml files.\n"
            "    o Useful axis displayed with F2 key.\n"
            "    o Fixed bug with recursive function used to fill ROIs.\n"
            "    o Support for reading CRUISE surfaces in OpenDX format\n"
            "    o Support for reading BrainVoyager surfaces (.srf) format\n"
            "    o Mouse motion effect is modulated with Zoom level\n"
            "    o F8 toggles between orthographic and perspective viewing\n"
            "    o Fixed bug causing crash in SUMA_MixColorOverlays\n"
            "  + ConvertSurface:\n"
            "    o Option -make_consistent added to make the winding\n"
            "    of the mesh consistent.  \n"
            "    o Option to project surface nodes to sphere\n" 
            "  + SurfQual:\n"
            "    o Checks and warns about mesh's winding inconsistency.\n"
            "  + SurfSmooth:\n"
            "    o Added NN_geom, nearest neighbor interpolation option.\n"
            "    o Combined with -match_vol or -match_area, this geometry\n"
            "    smoothing mode can be used to inflate surfaces.\n"
            "    o New weighting for Taubin geometry filtering.\n"
            "    o Option for masking nodes to be filtered.\n"
            "    o New HEAT method for filtering data.\n"
            "  + SurfaceMetrics:\n"
            "    o Option -vol calculates the volume of the closed surface.\n"
            "  + SurfPatch:\n"
            "    o Option -vol to calculate the volume between two isotopic\n"
            "    surface patches.\n"
            "  + ROI2dataset:\n"
            "    o Option -pad_to_node and -pad_label to output datasets\n"
            "    containing full node listings.\n"
            "  + ConvertDset:\n"
            "    o Option -o_1dp was added to write 1D file data only,\n"
            "    without additional comments.\n"
            "  + SurfaceMetrics:\n"
            "    o Options for finding surface boundaries.\n"
            "  + 3dSkullStrip:\n"
            "    o Fixed bug on 64 bit machines.\n"
            );
         break; 
         
      case 20040610:   /* used to be 25000 */
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %s (used to be 2.500)\n", SUMA_ver2date(ver)); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
            "Modifications:\n"
            "  + SUMA's surface controller 'ctrl+s' has been\n"
            "    vastly improved. \n"
            "    Of note are the following features:\n"
            "     - interactive color mapping\n"
            "     - thresholding controls \n"
            "     - brightness modulation\n"
            "     - choice of colormaps\n"
            "     - coordinate bias (tres tres cool)\n"
            "     - info on current cross hair location\n"
            "    Use Bhelp button in the controller for detailed help.\n"
            "  + 3dVol2Surf can output NIML formatted datasets.\n"
            "    Options -first_node and -last_node can be used\n"
            "    to restrict the mapping to a subset of the nodes.\n"
            "    That is useful if your output file size exceeds 2GB.\n"
            "Bug Fix:\n"
            "  + Fixed bug on Mac OS-X that cause all viewers to close\n"
            "    after pressing 'Yes' on the 'Close this viewer' prompt.\n"  
            );
         break;
         
      case 20040116:    /* used to be 24800 */
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %s (used to be 2.480)\n", SUMA_ver2date(ver)); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
            "New Programs:\n"
            "  + FS_readannot: Program to read FreeSurfer's\n"
            "                  annotation files.\n"
            "  + SurfPatch: Program to create surface patches\n"
            "               from a set of nodes.\n"
            "  + SurfQual: Program to report defects in surfaces.\n"
            "              For the moment, works on spherical \n"
            "              surfaces only.\n"
            "Modifications:\n"
            "  + Added affine transforms to ConvertSurface.\n"
            "  + Added datasets into SUMA's code (no interface).\n"
            "  + Added saving/loading of viewer settings.\n"
            "  + Beginning of multiple group support in SUMA.\n"
            "  + Redisplays of Surface Viewers due to X events\n"
            "    are no longer passed to the image recorder.\n" );
         break; 
         
      case 20040106:    /* used to be 24500 */
         SS = SUMA_StringAppend_va(SS, 
            "++ SUMA version %s (used to be 2.450)\n", SUMA_ver2date(ver)); if (StampOnly) break;
         SS = SUMA_StringAppend(SS, 
            "New Programs:\n"
            "  + inspec: Shows the contents of a spec file\n"
            "  + quickspec: Creates a minimal spec file for one\n"
            "               or a bunch of surfaces.\n"
            "  + SurfSmooth: Smoothes surface data or geometry\n"
            "  + SurfMeasures: Outputs various surface attributes  \n"
            "                  and measurements such as:\n"
            "                  Thickness, Area, Volume, etc.\n"
            "Modifications:\n"
            "  + Foreground color smoothing option (SUMA keyb. 8)\n"
            "  + No more MappingRef field in Spec files.\n"
            "    The field is broken up into a set of other\n"
            "    fields for more flexibility.\n"
            "  + Surface input to command-line programs is \n"
            "    now done via -spec files too.\n"
            "  + One-way communication with SUMA via niml.\n"
            "    Only available with SurfSmooth for the moment.\n"
            "  + Began, in good faith, to update the new version \n"
            "    information.\n"); 
         break;
      
      default:
         SS = SUMA_StringAppend_va(SS, "++ %d? No such version, fool!\n", ver);
         break;
   }
   
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN(s);
}

/*!
   \brief function called when help window is open
*/
void SUMA_Help_open (void *p)
{
   static char FuncName[]={"SUMA_Help_open"};

   SUMA_ENTRY;
   /* nothing to do here */
   
   SUMA_RETURNe;
}

/*!
   \brief function called when help window is destroyed
*/
void SUMA_Help_destroyed (void *p)
{
   static char FuncName[]={"SUMA_Help_destroyed"};
   
   SUMA_ENTRY;

   SUMAg_CF->X->Help_TextShell = NULL;
   
   SUMA_RETURNe;
}
/*!
   \brief function called when help window is open
*/
void SUMA_Help_Cmap_open (void *p)
{
   static char FuncName[]={"SUMA_Help_Cmap_open"};

   SUMA_ENTRY;
   /* nothing to do here */
   
   SUMA_RETURNe;
}
void SUMA_Help_Plot_open (void *p)
{
   static char FuncName[]={"SUMA_Help_Plot_open"};

   SUMA_ENTRY;
   /* nothing to do here */
   
   SUMA_RETURNe;
}

/*!
   \brief function called when help window is destroyed
*/
void SUMA_Help_Cmap_destroyed (void *p)
{
   static char FuncName[]={"SUMA_Help_Cmap_destroyed"};
   
   SUMA_ENTRY;

   SUMAg_CF->X->Help_Cmap_TextShell = NULL;
   
   SUMA_RETURNe;
}
/*!
   \brief function called when help window is destroyed
*/
void SUMA_Help_Plot_destroyed (void *p)
{
   static char FuncName[]={"SUMA_Help_Plot_destroyed"};
   
   SUMA_ENTRY;

   SUMAg_CF->X->Help_Plot_TextShell = NULL;
   
   SUMA_RETURNe;
}

/*!
   \brief function called when whereami window is destroyed
*/
void SUMA_Whereami_destroyed (void *p)
{
   static char FuncName[]={"SUMA_Whereami_destroyed"};
   
   SUMA_ENTRY;

   SUMAg_CF->X->Whereami_TextShell = NULL;
   
   SUMA_RETURNe;
}

/*!
   \brief function called when whereami window is destroyed
*/
void SUMA_Whereami_open (void *p)
{
   static char FuncName[]={"SUMA_Whereami_open"};
   
   SUMA_ENTRY;
   
   SUMA_RETURNe;
}

/*!
   \brief function called when Message window is open
*/
void SUMA_Message_open (void *p)
{
   static char FuncName[]={"SUMA_Message_open"};

   SUMA_ENTRY;
   /* nothing to do here */
   
   SUMA_RETURNe;
}

/*!
   \brief function called when Message window is destroyed
*/
void SUMA_Message_destroyed (void *p)
{
   static char FuncName[]={"SUMA_Message_destroyed"};
   
   SUMA_ENTRY;

   SUMAg_CF->X->Log_TextShell = NULL;
   
   SUMA_RETURNe;
}

char * SUMA_NIDO_Info(void)
{
   static char FuncName[]={"SUMA_NIDO_Info"};
   char *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   SS = SUMA_StringAppend(SS,
"Displayble objects in NIML format (NIDO).\n"
"A NIDO is a collection of displayable objects specified in an ascii file.\n"
"NIDO is a collection of elements with the first element named 'nido_head'\n"
"That first element can contain attributes that describe the entire NIDO \n"
"and default attributes for the remaining elements.\n"
"The following example shows a nido_head element with possible attributes.\n"
"You do not need to set them all if you don't care to do so. Note that all\n "
"attributes are strings and should be enclosed in single or double quotes.\n"
"\n"
"<nido_head\n"
"coord_type = 'fixed'\n"
"default_color = '1.0 0.2 0.6'\n"
"default_font = 'tr24'\n"
"bond = ''\n"
"/>\n"
"\n"
"  coord_type attribute:\n"
"     Describes the coordinate type of all elements in NIDO.\n"
"     * If 'fixed' then that means then the elements do not move with\n"
"     suma's surfaces, and the coordinate units are assumed to be in the\n"
"     range [0,1] with '0 0 0' being the lower left corner of the screen\n"
"     and closest to you. The z coordinate is useful for assigning elements\n"
"     to either the background (1) or the foreground (0) of the scene. \n"
"     Elements in the foreground would always be visible, while those in the\n"
"     background may be obscured by the rendered surface.\n"
"     * If 'mobile' then the elements will move along with your object.\n"
"     In that case, the corrdinates you specify are in the same space \n"
"     as your rendered objects. Also, with 'mobile' NIDO, you can specify\n"
"     location by specifying a 'node' attribute as illustrated below.\n"
"     * Default NIDO coordinate type is: 'mobile'\n"
"  default_color atribute:\n"
"     3 (R G B) , or 4 (R G B A) color values between [0, 1]\n"
"     Elements that do not have their own 'col' attribute set, will use \n"
"     default_color instead. At the moment however, A is not being used.\n"
"     Default default_color is '1.0 1.0 1.0'\n" 
"  default_font attribute:\n"
"     String specifying font. All fonts are from the GLUT library. \n"
"     Elements that do not have their own 'font' attribute set, will use \n"
"     default_font instead.\n" 
"     Default default_font is 'f9'\n"
"        Allowed fonts are:\n"
"           'f8', or 'font8': Constant width 8 size font\n"
"           'f9', or 'font9': Constant width 9 size font\n"
"           'tr10', or 'times_roman10'\n"
"           'tr24', or 'times_roman24'\n"
"           'he10', or 'helvetica10'\n"
"           'he12', or 'helvetica12'\n"
"           'he18', or 'helvetica18'\n"
"  default_SO_label:\n"
"     Label identifying surface from which elements get their node based \n"
"     parameters extracted.\n"
"     This is mostly useful when the coordinate system's type is 'mobile'\n"
"     The default is the currently selected surface in SUMA. If no surface\n"
"     is currently selected, some random surface is picked.\n"
"  default_node:\n"
"     One integer which specifies the index of the node to which all elements\n"
"     belong. This value essentially specfies the 'node' attribute of\n"
"     individual elements should the 'node' attribute be missing.\n"
"     A missing default_node, or a value of -1 indicate there is no default\n"
"     node.\n"
"  bond:\n"
"     If set to 'surface' then NIDO is attached to a particular surface.\n"
"     This means that if a surface is not displayed, none of the elements in\n"
"     this NIDO would be displayed. Default is 'none'\n" 
"\n"
"After 'nido_head' comes a list of elements of various types.\n"
"Text element example:\n"
"<T\n"
"font = 'he12'\n"
"coord = '0.5 0.5 0'\n"
"col = '0.21 0.9 0.61'\n"
"text = "
"'The Middle\n"
"----------'\n"
"h_align = 'center'\n"
"v_align = 'center'\n"
"/>\n"
"  text attribute:\n"
"     Put the text you want to display between single or double quotes.\n"
"     You can do multi-line text.\n"
"  coord attribute:\n"
"     XYZ coordinates whose units are determined by nido_head's coord_type.\n" 
"     See also p attribute\n"
"  p attribute:\n"
"     A convenience positioning attribute for placing text in fixed screen\n"
"     coordinates. If present, it will override coord, h_align, and v_align\n"
"     attributes. Its value is two to 3 characters long.\n"
"     1st char: t for top, c for center or m for middle, b for bottom\n"
"     2nd char: l for left, c for center or m for middle, r for right\n"
"     3rd char: f for front, r for rear (optional)\n"
"     h_align and v_align are set in a manner that makes sense for these \n"
"     special position flags.\n"
"  font attribute:\n"
"     Sets the font for the text element. If not specified, font is set per \n"
"     default_font.\n"
"  col attribute:\n"
"     Sets the color for the text element. If not specified, col is set per \n"
"     default_color.\n"
"  h_align:\n"
"     Sets the horizontal alignment. Choose from 'l' (default) for left,\n"
"    'c' for center, or 'r' for right.\n"
"  v_align:\n"
"     Sets the horizontal alignment. Choose from 'b' (default) for bottom, \n"
"     'c' for center, or 't' for top.\n"
"  node:\n"
"     Places the object at a node's location in the surface object defined by\n"
"     SO_label attribute. Note that this option overrides coord and might \n"
"     confuse you if NIDO's coord_type is 'fixed'. In such a case, the \n"
"     location would be that of the node, before you moved the surface.\n"
"  SO_label:\n"
"     Label of Surface Object from which the element gets its node based\n"
"     parameters extracted. Default is NIDO's default_SO_label\n"
"Sphere element example (only new attributes are detailed):\n"
"<S\n"
"node = '0'\n"
"col = '0.9 0.1 0.61'\n"
"rad = '35'\n"
"line_width = '1.5'\n"
"style = 'silhouette'\n"
"stacks = '20'\n"
"slices = '20'\n"
"/>\n"
"  rad attribute:\n"
"     Radius of the sphere (default 10).\n"
"  rad.ef attribute:\n"
"     In lieu of rad, this parameter would\n"
"     make the radius be a fraction of the average edge length\n"
"     for the surface related to this sphere.\n"
"  line_width attribute:\n"
"     Width of line (segments) of sphere's mesh\n"
"  stacks attribute:\n"
"     Number of longitudes (default 10).\n"
"  slices attribute:\n"
"     Number of lattitudes (default 10).\n"
"  style attribute:\n"
"     Style of sphere rendering. Choose from:\n"
"     fill (default), line, silhouette, or point\n"
"     See OpenGL's gluQuadricStyle function for details.\n"
"  Other acceptable attributes:\n"
"  node, coord, and SO_label\n" 
"Image element example (only new attributes are detailed):\n"
"<I\n"
"coord = '0.4 0.5 1'\n"
"filename = 'face_alexmartin2.jpg'\n"
"h_align = 'center'\n"
"v_align = 'bot'\n"
"/>\n"
"  filename attribute:\n"
"     Specifies the filename of the image. If the filename has no path, SUMA\n"
"     will search your path for a match before failing.\n"
"  Other acceptable attributes:\n"
"  h_align, v_align, coord, node, and SO_label.\n"
"\n"
"Texture element example:\n"
"<Tex\n"
"filename = 'face_afniman.jpg'\n"
"target = 'FRAME'\n"
"frame_coords = '\n"
"0.0 0.0 1\n"
"0.0 1.0 1\n"
"1.0 1.0 1\n"
"1.0 0.0 1 '\n"
"mix_mode = 'blend'\n"
"coord_gen = 'sphere'\n"
"/>\n"
"  filename attribute:\n"
"     Specifies the filename of the texture image.\n"
"  target attribute:\n"
"     Specifies the target of the texture. \n"
"     If target is 'FRAME' then the texture is bound to a quadrilateral whose\n"
"     coordinates are defined by the frame_coords attribute. This is useful\n"
"     for putting a background image in SUMA for example, when NIDO is of\n"
"     a 'fixed' coord_type. Alternately, target can be the label of a \n"
"     surface, or a bunch of surfaces sharing the label string.\n"
"     The default is 'ALL_SURFS' which targets all surfaces being displayed\n"
"  frame_coords attribute:\n"
"     Specify the coordinate of the quadrilateral onto which the texture\n"
"     is bound. This is of use when target is set to 'FRAME'. The default\n"
"     coordinates are set to:\n"
"        0.0 0.0 1\n"
"        0.0 1.0 1\n"
"        1.0 1.0 1\n"
"        1.0 0.0 1 '\n"
"     For 'fixed' coord_type, this defaut sets up a rectangle that fills up \n"
"     the suma viewer in the background of the scene. \n"
"     BUG: If you reduce z in 'fixed' coord_type, the texture map be\n"
"     positioned closer to the foreground, and should obscure objects behind  \n"
"     it. But for some reason, no surfaces get rendered in that case, no \n"
"     matter where they lie relative to the texture frame.\n"
"     For 'mobile' coord_type, the texture frame coordinates are in the same\n"
"     units as those for the rendered objects. \n"
"     Showing textures in frames is like displaying an image except that:\n"
"     - Textures will scale with changes in viewer size for 'fixed' coord_type\n"
"     and zoom factor for 'mobile' coord_type. While image size only depends\n"
"     on its number of pixels. \n"
"     - Frame orientation is arbitrary for textures. For images, the frame is\n"
"     always aligned with the pixel arrays (always facing you). With images, \n"
"     you can only control where its center is located.\n"
"  mix_mode attribute:\n"
"     Specifies the way texture mixes with node colors.\n"
"     Choose from: 'decal', 'blend', 'replace', and 'modulate'. \n"
"     Default is 'replace' when target is 'frame' and 'modulate' for \n"
"     other target values. These parallel OpenGL's GL_DECAL, GL_BLEND, etc.\n"
"  coord_gen attribute:\n"
"     Specifies how texture coordinate generation is done, when target is not\n"
"     'FRAME'. Choose from: 'sphere', 'object', 'eye'. Default is 'sphere'\n"
"     For detail, see OpenGL's GL_SPHERE_MAP, GL_OBJECT_LINEAR, etc.\n"
"\n");
   if (SUMAg_CF->Dev) {
      SS = SUMA_StringAppend(SS,
"3DTexture element example:\n"
"<3DTex\n"
"filename = 'cube256+tlrc.BRIK'\n"
"/>\n"
"  filename attribute:\n"
"     Specifies the filename of the 3D Texture (volume) image.\n"
"     This image is anything that AFNI can read as a dataset.\n"
"     For the moment, it is only tested on single brick dsets.\n"
"  For testing, try the following from ~/Work/VolumeRender:\n"
"     suma -i surf.ply -niml -dev & ; \\\n"
"     sleep 3 && \\\n"
"     DriveSuma -com viewer_cont -load_do mobile.niml.do ; \\\n"
"     set l = $< ; DriveSuma -com kill_suma\n"
"\n");
   }
   SS = SUMA_StringAppend(SS,
"  Try the script '@DO.examples' for concrete examples on displayable \n"
"  objects.\n"  
"\n"
); 

   SUMA_SS2S(SS,s);
   
   SUMA_RETURN (s);

}

char * SUMA_OptList_string(HELP_OPT *hol)
{
   static char FuncName[]={"SUMA_OptList_string"};
   char *s = NULL;
   int i=0;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   while (hol[i].name) {
      SS = SUMA_StringAppend_va(SS,"   %s\n", hol[i].help);
      if (hol[i].val) 
         SS = SUMA_StringAppend_va(SS,"     default: %s\n", hol[i].val);
      ++i;
   }
   
   SUMA_SS2S(SS,s);
   
   SUMA_RETURN (s);
}

char *SUMA_OptList_get(HELP_OPT *hol, char *opname, char *what) 
{
   static char FuncName[]={"SUMA_OptList_default"};
   int i = 0;
   
   SUMA_ENTRY;
   
   while (hol[i].name) {
      if (!strcasecmp(hol[i].name, opname)) {
         if (what[0] == 'd' || what[0] == 'D') {
            SUMA_RETURN(hol[i].val);
         } else if (what[0] == 'v' || what[0] == 'V') {
            SUMA_RETURN(hol[i].val);
         } else if (what[0] == 'h' || what[0] == 'H') {
            SUMA_RETURN(hol[i].help);
         } else {
            SUMA_RETURN(NULL);
         }
      }
      ++i;
   }
   SUMA_RETURN(NULL);  
}

char * SUMA_sources_Info(void)
{
   static char FuncName[]={"SUMA_sources_Info"};
   char *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   SS = SUMA_StringAppend(SS,
"Parts of SUMA use functions from various sources.\n"
"Library sources:\n"
"----------------\n"
"* Openmotif\n"
"   --------------------------------------------------------------\n"
"\n"
"The enhancements made by Integrated Computer Solutions, Inc. (ICS) to\n"
"create Open Motif 2.2.2, Open Motif 2.2.3, Open Motif 2.2.4 and\n"
"Open Motif 2.3.0 are made available under The Open Group Public\n"
"License that is included below.\n"
"\n"
"   --------------------------------------------------------------\n"
"\n"
"$TOG: COPYRIGHT.MOTIF /main/5 2000/04/10 12:00:00 $\n"
"\n"
"                            MOTIF 2.1.30\n"
"                             Source Code\n"
"                        MASTER COPYRIGHT NOTICE\n"
"\n"
"(c) Copyright 1989 - 1994, 1996 - 1999 The Open Group\n"
"(c) Copyright 1987 - 1999 Hewlett-Packard Company\n"
"(c) Copyright 1987 - 1999 Digital Equipment Corporation, Maynard, Mass.\n"
"(c) Copyright 1988 Massachusetts Institute of Technology\n"
"(c) Copyright 1988 Microsoft Corporation\n"
"(c) Copyright 1990 Motorola Inc.\n"
"(c) Copyright 1989 - 1994 Groupe Bull\n"
"(c) Copyright 1991 Joseph Friedman\n"
"(c) Copyright 1995 - 1999 International Business Machines Corp.\n"
"(c) Copyright 1995 - 1999 Sun Microsystems, Inc.\n"
"(c) Copyright 1995 - 1999 Santa Cruz Organization, Inc.\n"
"(c) Copyright 1995, 1996 Fujitsu Limited\n"
"(c) Copyright 1995, 1996 Hitachi, Ltd.\n"
"\n"
"\n"
"ALL RIGHTS RESERVED\n"
"\n"
"This software is furnished under a license and may be used\n"
"and copied only in accordance with the terms of such license and\n"
"with the inclusion of this copyright notice. No title to and ownership\n"
"of the software is hereby transferred.\n"
"\n"
"This software is subject to an open license. It may only be\n"
"used on, with or for operating systems which are themselves open\n"
"source systems. You must contact The Open Group for a license allowing\n"
"distribution and sublicensing of this software on, with, or for\n"
"operating systems which are not Open Source programs.\n"
"\n"
"See http://www.opengroup.org/openmotif/license for full\n"
"details of the license agreement. Any use, reproduction, or\n"
"distribution of the program constitutes recipient's acceptance of\n"
"this agreement.\n"
"\n"
"THE OPEN GROUP AND ITS THIRD PARTY SUPPLIERS, ASSUME NO RESPONSIBILITY\n"
"FOR THE USE OR INABILITY TO USE ANY OF ITS SOFTWARE.\n"
"\n"
"EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, THE PROGRAM IS\n"
"PROVIDED ON AN \"AS IS\" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY\n"
"KIND, EITHER EXPRESS OR IMPLIED INCLUDING, WITHOUT LIMITATION, ANY\n"
"WARRANTIES OR CONDITIONS OF TITLE, NON-INFRINGEMENT, MERCHANTABILITY\n"
"OR FITNESS FOR A PARTICULAR PURPOSE.\n"
"\n"
"EXCEPT AS EXPRESSLY SET FORTH IN THIS AGREEMENT, NEITHER RECIPIENT\n"
"NOR ANY CONTRIBUTORS SHALL HAVE ANY LIABILITY FOR ANY DIRECT,\n"
"INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL\n"
"DAMAGES (INCLUDING WITHOUT LIMITATION LOST PROFITS), HOWEVER CAUSED\n"
"AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT\n"
"LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN\n"
"ANY WAY OUT OF THE USE OR DISTRIBUTION OF THE PROGRAM OR THE\n"
"EXERCISE OF ANY RIGHTS GRANTED HEREUNDER, EVEN IF ADVISED OF THE\n"
"POSSIBILITY OF SUCH DAMAGES.\n"
"\n"
"The information in this software is subject to change without\n"
"notice and should not be construed as a commitment by The Open Group\n"
"or its third party suppliers.\n"
"\n"
"Notice: Notwithstanding any other lease or license that may pertain to,\n"
"or accompany the delivery of, this computer software, the rights of the\n"
"Government regarding its use, reproduction and disclosure are as set\n"
"forth in Section 52.227-19 of the FARS Computer Software-Restricted\n"
"Rights clause.\n"
"\n"
"(c) Copyright 1990, 1991, 1992, 1993, 1994, 1996 Open Software\n"
"Foundation, Inc.\n"
"(c) Copyright 1996, 1997, 1998, 1999, 2000 The Open Group.\n"
"Unpublished - all rights reserved under the Copyright laws of the United\n"
"States.\n"
"\n"
"RESTRICTED RIGHTS NOTICE: Use, duplication, or disclosure by the\n"
"Government is subject to the restrictions as set forth in subparagraph\n"
"(c)(1)(ii) of the Rights in Technical Data and Computer Software clause\n"
"at DFARS 52.227-7013.\n"
"\n"
"The Open Group LLC\n"
"Apex Plaza, Forbury Road\n"
"Reading, Berkshire,\n"
"RG1 1AX, UK.\n"
"\n"
"RESTRICTED RIGHTS LEGEND: This computer software is submitted with\n"
"\"restricted rights.\" Use, duplication or disclosure is subject to the\n"
"restrictions as set forth in NASA FAR SUP 18-52.227-79 (April 1985)\n"
"\"Commercial Computer Software- Restricted Rights (April 1985).\" The\n"
"Open Group, Apex Plaza, Forbury Road, Reading, Berkshire, RG1 1AX, UK.\n"
"If the contract contains the Clause at 18-52.227-74 \"Rights in Data\n"
"General\" then the \"Alternate III\" clause applies.\n"
"\n"
"(c) Copyright 1990, 1991, 1992, 1993, 1994, 1996 Open Software\n"
"Foundation,Inc. ALL RIGHTS RESERVED\n"
"(c) Copyright 1996, 1997, 1998, 1999, 2000 The Open Group. ALL RIGHTS\n"
"RESERVED\n"
"\n"
"The Open Group, Open Software Foundation, OSF, OSF/Motif, and Motif are\n"
"Trademarks of The Open Group\n"
"DEC and DIGITAL are registered trademarks of Digital Equipment\n"
"Corporation\n"
"HP is a trademark of Hewlett-Packard Company\n"
"X Window System is a trademark of the Massachusetts Institute of\n"
"Technology\n"
"\n"
"* MarchingCubes:\n"
"  The code for the heart of the program IsoSurface is a translation of:\n"
"  Thomas Lewiner's C++ implementation of the algorithm in:\n"
"  Efficient Implementation of Marching Cubes� Cases with Topological Guarantees\n"
"  by Thomas Lewiner, H�lio Lopes, Ant�nio Wilson Vieira and Geovan Tavares \n"
"  in Journal of Graphics Tools. \n"
"  http://www-sop.inria.fr/prisme/personnel/Thomas.Lewiner/JGT.pdf\n"
"  Permission to use this translation in other programs must be obtained \n"
"  from Mr. Lewiner.\n"
"\n"
"* 3d Edge Detection:\n"
"  The code for 3dEdge detection is from the library 3DEdge \n"
"  by Gregoire Malandain (gregoire.malandain@sophia.inria.fr)\n"
"  References for the algorithms:\n"
"  -  Optimal edge detection using recursive filtering\n"
"     R. Deriche, International Journal of Computer Vision,\n"
"     pp 167-187, 1987.\n"
"  -  Recursive filtering and edge tracking: two primary tools\n"
"     for 3-D edge detection, O. Monga, R. Deriche, G. Malandain\n"
"     and J.-P. Cocquerez, Image and Vision Computing 4:9, \n"
"     pp 203-214, August 1991.\n"
"\n"
"* QHull:\n"
"                       Qhull, Copyright (c) 1993-2001 \n"
"  The National Science and Technology Research Center for \n"
"          Computation and Visualization of Geometric Structures \n"
"                          (The Geometry Center) \n"
"                         University of Minnesota \n"
"                              400 Lind Hall \n"
"                          207 Church Street S.E. \n"
"                         Minneapolis, MN 55455  USA \n"
"   \n"
"                         email: qhull@geom.umn.edu \n"
"   \n"
"  This software includes Qhull from The Geometry Center.  Qhull is  \n"
"  copyrighted as noted above.  Qhull is free software and may be obtained  \n"
"  via anonymous ftp from geom.umn.edu.  It may be freely copied, modified,  \n"
"  and redistributed under the following conditions: \n"
"   \n"
"  1. All copyright notices must remain intact in all files. \n"
"   \n"
"  2. A copy of this text file must be distributed along with any copies  \n"
"     of Qhull that you redistribute; this includes copies that you have  \n"
"     modified, or copies of programs or other software products that  \n"
"     include Qhull. \n"
"   \n"
"  3. If you modify Qhull, you must include a notice giving the \n"
"    name of the person performing the modification, the date of \n"
"     modification, and the reason for such modification. \n"
"   \n"
"  4. When distributing modified versions of Qhull, or other software \n"
"     products that include Qhull, you must provide notice that the original \n"
"     source code may be obtained as noted above.\n"
"  \n"
"  5. There is no warranty or other guarantee of fitness for Qhull, it is \n"
"     provided solely ""as is"".  Bug reports or fixes may be sent to \n"
"     qhull_bug@geom.umn.edu; the authors may or may not act on them as \n"
"    they desire.\n"
"  \n"
"\n"
"* GLUT: Mark. J. Kilgard's code from the book 'Programming OpenGL for \n"
"        the X Window System' (ISBN:0-201-48359-9) published by \n"
"        Addison-Wesley.\n"
"  NOTICE:  This source code distribution contains source code contained\n"
"  in the book 'Programming OpenGL for the X Window System' (ISBN:\n"
"  0-201-48359-9) published by Addison-Wesley.  The programs and\n"
"  associated files contained in the distribution were developed by Mark\n"
"  J. Kilgard and are Copyright 1994, 1995, 1996 by Mark J.  Kilgard\n"
"  (unless otherwise noted).  The programs are not in the public domain,\n"
"  but they are freely distributable without licensing fees.  These\n"
"  programs are provided without guarantee or warrantee expressed or\n"
"  implied.\n"
"\n"
"* PLY: A set of functions for reading/writing PLY polygon files by\n"
"       Greg Turk.\n"
"  Copyright (c) 1994 The Board of Trustees of The Leland Stanford\n"
"  Junior University.  All rights reserved.   \n"
"   \n"
"  Permission to use, copy, modify and distribute this software and its  \n" 
"  documentation for any purpose is hereby granted without fee, provided \n"  
"  that the above copyright notice and this permission notice appear in  \n" 
"  all copies of this software and that you do not sell the software.   \n"
"    \n"
"  THE SOFTWARE IS PROVIDED 'AS IS' AND WITHOUT WARRANTY OF ANY KIND,   \n"
"  EXPRESS, IMPLIED OR OTHERWISE, INCLUDING WITHOUT LIMITATION, ANY   \n"
"  WARRANTY OF MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE.  \n" 
"  \n"
"* SimpEnvelopes: A set of functions for simplifying triangular meshes by\n"
"                 Jonathan Cohen (cohenj@cs.unc.edu) and Amitabh Varshney\n"
"                 (varshney@cs.sunysb.edu).\n"
"  Copyright 1995 The University of North Carolina at Chapel Hill.\n"
"  All Rights Reserved.\n"
"\n"
"  Permission to use, copy, modify and distribute this software and its\n"
"  documentation for educational, research and non-profit purposes,\n"
"  without fee, and without a written agreement is hereby granted,\n"
"  provided that the above copyright notice and the following three\n"
"  paragraphs appear in all copies.\n"
"\n"
"  IN NO EVENT SHALL THE UNIVERSITY OF NORTH CAROLINA AT CHAPEL HILL BE\n"
"  LIABLE TO ANY PARTY FOR DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR\n"
"  CONSEQUENTIAL DAMAGES, INCLUDING LOST PROFITS, ARISING OUT OF THE\n"
"  USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY\n"
"  OF NORTH CAROLINA HAVE BEEN ADVISED OF THE POSSIBILITY OF SUCH\n"
"  DAMAGES.\n"
"\n"
"\n"
"  Permission to use, copy, modify and distribute this software and its\n"
"  documentation for educational, research and non-profit purposes,\n"
"  without fee, and without a written agreement is hereby granted,\n"
"  provided that the above copyright notice and the following three\n"
"  paragraphs appear in all copies.\n"
"\n"
"  THE UNIVERSITY OF NORTH CAROLINA SPECIFICALLY DISCLAIM ANY\n"
"  WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF\n"
"  MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE\n"
"  PROVIDED HEREUNDER IS ON AN 'AS IS' BASIS, AND THE UNIVERSITY OF\n"
"  NORTH CAROLINA HAS NO OBLIGATION TO PROVIDE MAINTENANCE, SUPPORT,\n"
"  UPDATES, ENHANCEMENTS, OR MODIFICATIONS.\n"
"\n"
"  The authors may be contacted via:\n"
"\n"
"  US Mail:  Jonathan Cohen                      Amitabh Varshney\n"
"            Department of Computer Science      Department of Computer Science \n"
"            Sitterson Hall, CB #3175            State University of New York\n"
"            University of N. Carolina           Stony Brook, NY 11794-4400, USA \n"
"            Chapel Hill, NC 27599-3175\n"
"	    \n"
"  Phone:    (919)962-1749                       Phone: (516)632-8446 \n"
"	    \n"
"  EMail:    cohenj@cs.unc.edu                   varshney@cs.sunysb.edu\n"
"\n"
"* UTHASH:\n"
"Copyright (c) 2005-2009, Troy D. Hanson    http://uthash.sourceforge.net\n"
"All rights reserved.\n"
"\n"
"Redistribution and use in source and binary forms, with or without\n"
"modification, are permitted provided that the following conditions are met:\n"
"\n"
"    * Redistributions of source code must retain the above copyright\n"
"      notice, this list of conditions and the following disclaimer.\n"
"\n"
"THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS ""AS\n"
"IS"" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED\n"
"TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A\n"
"PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER\n"
"OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,\n"
"EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,\n"
"PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR\n"
"PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF\n"
"LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING\n"
"NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS\n"
"SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.\n"
"\n"
"Sample code sources:\n"
"--------------------\n"
"* 'Mastering Algorithms with C': a book by Kyle Loudon,  \n"
"                                 published by O'Reilly & Associates. \n"
"   This code is under copyright and cannot be included in any other book, publication,\n"
"   or  educational product  without  permission  from  O'Reilly & Associates.  No\n"
"   warranty is attached; we cannot take responsibility for errors or  fitness for\n"
"   use.\n"
"\n"
"* 'C Language Algorithms for Digital Signal Processing': a book by \n"
"                Bruce Kimball, Paul Embree and Bruce Kimble\n"
"                published by Prentice Hall, 1991.\n"
"\n"
"* 'Motif Programming Manual': a book by Dan Heller and Paula Ferguson\n"  
"                              published by O'Reilly & Associates, Inc. 1994\n"  
"   * Written by Dan Heller and Paula Ferguson.\n"  
"   * Copyright 1994, O'Reilly & Associates, Inc.\n" 
"   * Permission to use, copy, and modify this program without\n" 
"   * restriction is hereby granted, as long as this copyright\n" 
"   * notice appears in each copy of the program source code.\n" 
"   * This program is freely distributable without licensing fees and\n" 
"   * is provided without guarantee or warrantee expressed or implied.\n" 
"   * This program is -not- in the public domain.\n"
"\n"
"* 'http://astronomy.swin.edu.au/~pbourke': Paul Bourke's personal pages\n"
"                                           on geometry and rendering .\n"
"   Copyright notice on webpage: Copyright Paul Bourke or a third party \n"
"   contributer where indicated. You may view this site and its contents \n"
"   using your web browser. You may print or save an electronic copy of \n"
"   parts of this web site for your own personal use. \n"
"   Permission must be sought for any other use. \n"
"\n"
" The C clustering library.\n"
"   Copyright (C) 2002 Michiel Jan Laurens de Hoon.\n"
"  \n"
"   This library was written at the Laboratory of DNA Information Analysis,\n"
"   Human Genome Center, Institute of Medical Science, University of Tokyo,\n"
"   4-6-1 Shirokanedai, Minato-ku, Tokyo 108-8639, Japan.\n"
"   Contact: mdehoon 'AT' gsc.riken.jp\n"
"   \n"
"   Permission to use, copy, modify, and distribute this software and its\n"
"   documentation with or without modifications and for any purpose and\n"
"   without fee is hereby granted, provided that any copyright notices\n"
"   appear in all copies and that both those copyright notices and this\n"
"   permission notice appear in supporting documentation, and that the\n"
"   names of the contributors or copyright holders not be used in\n"
"   advertising or publicity pertaining to distribution of the software\n"
"   without specific prior permission.\n"
"   \n"
"   THE CONTRIBUTORS AND COPYRIGHT HOLDERS OF THIS SOFTWARE DISCLAIM ALL\n"
"   WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING ALL IMPLIED\n"
"   WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL THE\n"
"   CONTRIBUTORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY SPECIAL, INDIRECT\n"
"   OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS\n"
"   OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE\n"
"   OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE\n"
"   OR PERFORMANCE OF THIS SOFTWARE.\n"
"   \n"
 ); 

   SUMA_SS2S(SS,s);
   
   SUMA_RETURN (s);
}

char * SUMA_help_Cmap_message_Info(SUMA_COLOR_MAP * ColMap, TFORM targ)
{
   static char FuncName[]={"SUMA_help_Cmap_message_Info"};
   char stmp[1000], *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   if (targ != TXT && targ != SPX) targ = TXT;

   SS = SUMA_StringAppend (NULL, NULL);
   
   if (targ == TXT) {
      SS = SUMA_StringAppend (SS,
            "\nColormap Keyboard Controls:\n");
   } else if (targ == SPX) {
      SS = SUMA_StringAppend (SS,
            ".. _Colormap_Keyboard_Controls:\n\n"
            "Colormap Keyboard Controls\n"
            "--------------------------\n\n");
   }
   #if 0
   s = SUMA_New_Additions (0, 1);
   SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;
   #endif
   
   SS = SUMA_StringAppend_va (SS, 
"With the cursor over the colormap, the following keyboard initiated actions\n"
"are available.\n\n"
"     %s: flip color map\n"
"        See also Up/Down keys.\n"
"\n", SUMA_hkcf("f", targ));

   SS = SUMA_StringAppend_va (SS, 
"     %s: this help message\n"
"\n", SUMA_hkcf("Ctrl+h", targ));

   SS = SUMA_StringAppend_va (SS, 
"     %s: record image of colormap.\n"
"\n", SUMA_hkcf("r", targ));

   SS = SUMA_StringAppend_va (SS, 
"     %s: write colormap to ascii file.\n"
"\n", SUMA_hkcf("w", targ));

   SS = SUMA_StringAppend_va (SS, 
"     %s: Zoom in.\n"
"        Maximum zoom in shows 2 colors in the map\n"
"     %s: Zoom out.\n"
"        Minimum zoom in shows all colors in the map\n"
"\n", SUMA_hkcf("Z", targ), SUMA_hkcf("z", targ));

   SS = SUMA_StringAppend_va (SS, 
"     %s arrows: rotate colormap up/down by fraction of\n"
"                     number of colors in color map. Fraction\n"
"                     a number between 0 and 0.5 and set via\n"
"                     the environment variable\n"
"                     SUMA_ColorMapRotationFraction.\n"
"                     See suma -environment for complete list\n"
"                     of variables.\n\n",
    SUMA_hkcf("U-D arrows", targ));

   SS = SUMA_StringAppend_va (SS, 
"     %s: rotate colormap up/down by one color\n"
"     %s: move colormap up/down\n"
"\n", SUMA_hkcf("Ctrl+U-D arrows", targ), SUMA_hkcf("Shift+U-D arrows", targ));

   SS = SUMA_StringAppend_va (SS, 
"     %s: Reset zoom, translation and rotation parameters\n"
"\n" , SUMA_hkcf("HOME", targ));
      
   if (ColMap) {
      SS = SUMA_StringAppend (SS, 
         "\nCmap properties\n");
      s = SUMA_ColorMapVec_Info(&ColMap, 1, 1);
      SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;

      /* Add help for all controller options */
      s = SUMA_Help_AllSurfCont(targ);
      SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;
   }
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN (SUMA_Sphinx_String_Edit(&s, targ, 0));

}

char *SUMA_help_SPEC_symbolic(void)
{
   static char FuncName[]={"SUMA_help_SPEC_symbolic"};
   char stmp[1000], *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);

   
   SS = SUMA_StringAppend_va (SS, 
"     As with option -i, you can load template\n"
"     spec files with symbolic notation trickery as in:\n"
"                    suma -spec MNI_N27 \n"
"     which will load the all the surfaces from template MNI_N27\n"
"     at the original FreeSurfer mesh density.\n"
"  The string following -spec is formatted in the following manner:\n"
"     HEMI:TEMPLATE:DENSITY where:\n"
"     HEMI specifies a hemisphere. Choose from 'l', 'r', 'lh', 'rh', 'lr', or\n"
"          'both' which is the default if you do not specify a hemisphere.\n"
"     TEMPLATE: Specify the template name. For now, choose from MNI_N27 if\n"
"               you want surfaces from the MNI_N27 volume, or TT_N27\n"
"               for the Talairach version.\n"
"               Those templates must be installed under this directory:\n"
"                 %s\n"
"               If you have no surface templates there, download\n"
"                 http:afni.nimh.nih.gov:/pub/dist/tgz/suma_MNI_N27.tgz\n"
"               and/or\n"
"                 http:afni.nimh.nih.gov:/pub/dist/tgz/suma_TT_N27.tgz\n"
"               and untar them under directory %s\n"
"     DENSITY: Use if you want to load standard-mesh versions of the template\n"
"              surfaces. Note that only ld20, ld60, ld120, and ld141 are in\n"
"              the current distributed templates. You can create other \n"
"              densities if you wish with MapIcosahedron, but follow the\n"
"              same naming convention to enable SUMA to find them.\n"
"              This parameter is optional.\n"
"     The order in which you specify HEMI, TEMPLATE, and DENSITY, does\n"
"     not matter.\n"
"     For template surfaces, the -sv option is provided automatically, so you\n"
"     can have SUMA talking to AFNI with something like:\n"
"             suma -spec MNI_N27:ld60 &\n"
"             afni -niml %s/suma_MNI_N27 \n"
, THD_datadir(1), THD_datadir(1), THD_datadir(0)
);
   
   SS = SUMA_StringAppend (SS, NULL);   
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN (s);
}

static char PlotCommonHelp[]={
      "        Open a graphing window for the dataset\n"
      "        currently selected. The graphing window\n"
      "        updates with each new node selection.\n"
      "        A graphing window can be opened for each\n"
      "        dataset, and all graphs will update unless\n"
      "        '1 Only' is set in Surface Controller.\n"
      "        For complex data its magnitude is plotted instead.\n"
      "        Use 'ctrl+h' in graph window for more help.\n" };

char * SUMA_help_Plot_message_Info(void)
{
   static char FuncName[]={"SUMA_help_Plot_message_Info"};
   char stmp[1000], *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);

   
   SS = SUMA_StringAppend_va (SS, 
      "What's in it for me?\n"
      "%s"
      "\nButtons:\n"
      "  Save:   Write graph image to file\n"
      "  Freeze: Detach graph from SUMA.\n"
      "        Further clicks will not update\n"
      "        graph.\n"
      "  Done: Close graph forever.\n"
      "\nKeyboard Controls\n"
      "     Ctrl+h: this help message\n"
      "\n"
      "     q/Q: Quit\n"
      "     w: Write time series to 1D file.\n"
      "\n"
      "\n", PlotCommonHelp);
     
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN (s);

}

/* Format help key string */
char * SUMA_hkf_eng(char *keyi, TFORM target, char *cm)
{
   static char FuncName[]={"SUMA_hkf_eng"};
   static char ss[20][256];
   char key1[256], key2[256], *direc="kbd";
   static int c;
   char *s;
   int ichar=-1;
   
   if (!cm) cm = "";
   
   ++c;
   if (c > 19) c = 0;
   s = (char *)ss[c]; s[0] = s[255] = '\0';
   if (!keyi) return(s);
   switch (target) {
      default:
      case TXT: /* SUMA */
         /* Actually COMMA, PERIOD, STAR mods are not needed, leave 
         for posterity.*/
         if (strstr(keyi,"COMMA")) {
            snprintf(key1, 255, ",");
         } else if (strstr(keyi,"PERIOD")) {
            snprintf(key1, 255, ".");
         } else if (strstr(keyi,"STAR")) {
            snprintf(key1, 255, "*");
         } else {
            snprintf(key1, 255, "%s", keyi);
         }
            snprintf(s, 255, "  %s", key1);
         return(s);
         break;
      case SPX: /* Sphinx */
         if (strstr(keyi,"->") == keyi) {
            /* Won't work if you pass key with blanks before '->'
               But why do such a thing? */ 
            snprintf(key1, 255, "%s", keyi+2);
            snprintf(key2, 255, "%s", keyi+2);
            direc = "menuselection";
         } else {
            snprintf(key1, 255, "%s", keyi);
            snprintf(key2, 255, "%s", keyi);
            direc = "kbd";
         }
         
         if (key1[1] == '\0') {
            ichar = 0;
         } else if (key1[strlen(key1)-2] == '+'){
            ichar = strlen(key1)-1;
         } else ichar = -1;
         
         if (ichar > -1) { 
            if (SUMA_IS_UPPER_C(key1[ichar])) {
               snprintf(s, 255, "\n.. _%sUC_%s:\n\n:%s:`%s`"
                  , cm, deblank_allname(key1,'_'), direc, deblank_name(key2));
            } else { 
               snprintf(s, 255, "\n.. _%sLC_%s:\n\n:%s:`%s`"
                  , cm, deblank_allname(key1,'_'), direc, deblank_name(key2));
            }
         } else {
            snprintf(s, 255, "\n.. _%s%s:\n\n:%s:`%s`"
                  , cm, deblank_allname(key1,'_'), direc, deblank_name(key2));
         }
         return(s);
         break;
   }
   return(s);
}

char * SUMA_hkf(char *keyi, TFORM target) {
   /* for main suma area keys */
   return(SUMA_hkf_eng(keyi,target,""));
}

char * SUMA_hkcf(char *keyi, TFORM target) {
   /* for colormap area keys */
   return(SUMA_hkf_eng(keyi,target,"CM_"));
}


/* Format GUI section */
char * SUMA_gsf(char *wname, TFORM target, char **hintout, char **helpout)
{
   static char FuncName[]={"SUMA_gsf"};
   static char ss[20][256], wnameclp[256];
   char key1[256], key2[256], *direc="kbd", *lnm=NULL;
   static int c;
   char *s=NULL, *su=NULL, *shh=NULL, *sii=NULL;
   int ichar=-1, i;
   GUI_WIDGET_HELP *gwh=NULL;
      
   ++c;
   if (c > 19) c = 0;
   s = (char *)ss[c]; s[0] = s[255] = '\0';
   
   if ((helpout && *helpout) || (hintout && *hintout)) {
      SUMA_S_Err("string init error");
      return(s);
   }

   
   if (!wname) return(s);
   
   switch (target) {
      default:
      case TXT: /* SUMA */
         snprintf(s, 255, "  %s", wname);
         return(s);
         break;
      case SPX: /* Sphinx */
         if (!(gwh = SUMA_Get_GUI_Help(wname, target, &shh, &sii,3))) {
            SUMA_S_Err("No help for %s\n", wname);
            SUMA_suggest_GUI_Name_Match(wname, 8, NULL);
            shh = SUMA_copy_string(wname);
            sii = SUMA_copy_string(wname);
         }
         
         if (!sii) sii = SUMA_copy_string("No Hint");
         if (helpout) *helpout = shh;
         if (hintout) *hintout = sii;
         
         if (!gwh) { return(s); }
         
         su = (char *)SUMA_calloc(strlen(sii)+2, sizeof(char));
         
         lnm = gwh->name[gwh->name_lvl-1];
         snprintf(wnameclp, 255, "%s", lnm);
         if (strstr(wnameclp,".r00")) { /* get rid of .r00 */
            wnameclp[strlen(lnm)-4]='\0';
         }
         
         switch (gwh->type) {
            case 0: /* container only */
               if (gwh->name_lvl == 1) {
                  for (i=0; i<strlen(sii); ++i) {su[i] = '-';} su[i] = '\0';
                  snprintf(s, 255, "\n"
                                   ".. _%s:\n"
                                   "\n"
                                   "%s\n"
                                   "%s\n",
                              wname, sii, su);
               } else if (gwh->name_lvl == 2) {
                  for (i=0; i<strlen(sii); ++i) {su[i] = '^';} su[i] = '\0';
                  snprintf(s, 255, "\n"
                                   ".. _%s:\n"
                                   "\n"
                                   "%s\n"
                                   "%s\n",
                              wname, sii, su);
               } else if (gwh->name_lvl == 3) {
                  for (i=0; i<strlen(sii); ++i) {su[i] = '"';} su[i] = '\0';
                  snprintf(s, 255, "\n"
                                   ".. _%s:\n"
                                   "\n"
                                   "%s\n"
                                   "%s\n",
                              wname, sii, su);
               } else if (gwh->name_lvl == 4) {
                  for (i=0; i<strlen(sii); ++i) {su[i] = '.';} su[i] = '\0';
                  snprintf(s, 255, "\n"
                                   ".. _%s:\n"
                                   "\n"
                                   "%s\n"
                                   "%s\n",
                              wname, sii, su);
               } else {
                  snprintf(s, 255, "\n"
                                   "   .. _%s:\n"
                                   "\n"
                                   "**%s**: %s\n"
                                   "\n",
                              wname, wnameclp,sii);
               }
               break;
            case 1: /* actual widget */
               snprintf(s, 255, "\n"
                                "   .. _%s:\n"
                                "\n"
                                "**%s**: %s\n"
                                "\n",
                              wname, wnameclp,sii);
               break;
            default:
               SUMA_S_Err("Bad type %d", gwh->type);
               break;
         }
         
         if (!hintout) SUMA_ifree(sii); 
         if (!helpout) SUMA_ifree(shh); 
         SUMA_ifree(su);
         
         return(s);
         break;
   }
   
   return(s);
}

char * SUMA_help_message_Info(TFORM targ)
{
   static char FuncName[]={"SUMA_help_message_Info"};
   char stmp[1000], *s = NULL;
   SUMA_STRING *SS = NULL;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (targ != TXT && targ != SPX) targ = TXT;
   
   SS = SUMA_StringAppend (NULL, NULL);

   #if 0 /* not maintained any more */
   s = SUMA_New_Additions (0, 1);
   SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;
   #endif
   if (targ == TXT) {
      SS = SUMA_StringAppend (SS,
            "Keyboard Controls:\n");
   } else if (targ == SPX) {
      SS = SUMA_StringAppend (SS,
            ".. _KeyboardControls:\n\n"
            "Keyboard Controls\n"
            "-----------------\n\n");
   }
   SS = SUMA_StringAppend (SS, 
      "*On MACs*, Alt is the Apple/Command key.\n"
      "   If it is commandeered by the OS, and you can't get it back, then\n"
      "   try the alt/option key instead.\n\n"
      "*On Linux*, Turn NumLock OFF, otherwise certainly mouse or \n"
      "   keyboard combinations do not work as intended.\n\n");
   SS = SUMA_StringAppend_va (SS, 
      "   %s: attenuation by background, toggle. "
      "See also :ref:`Plane Layering<Plane_Layering>`\n\n", 
      SUMA_hkf("a", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Backface/Frontface/Noface culling, toggle.\n", 
      SUMA_hkf("B", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: background color, toggle. "
      "See also :ref:`Plane Layering <Plane_Layering>`\n\n", 
      SUMA_hkf("b", targ));
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend_va (SS, 
      "   %s: Set screen-coordinate-based clipping planes\n"
      "   %s: Set object-coordinate-based clipping planes\n"
      "           o Planes are defined by a string of the format:\n"
      "             NAME: a, b, c, d\n"
      "             Where NAME is a user-given short name,\n"
      "             a, b, c, and d define the plane's equation\n"
      "             aX + bY + cZ + d = 0\n"
      "                Example: A: 0, 0, 1, 0\n"
      "           o To delete a plane, just enter its name followed\n"
      "             by the ':' (e.g. 'A:')\n"
      "           o If you enter only two parameters after the name, \n"
      "             they are assumed to be the c and d parameters,\n"
      "             a and b are set to 0.\n"
      "           o You are allowed a maximum of %d planes\n"
      "\n", SUMA_hkf("Ctrl+C", targ), SUMA_hkf(" Alt+C", targ),
      SUMA_MAX_N_CLIP_PLANES);
   SS = SUMA_StringAppend_va (SS, 
      "   %s: load a node color file.\n\n", SUMA_hkf("c", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Open draw :ref:`ROI controller<drawing_rois>`, and put cursor "\
      "in drawing mode.\n\n", 
      SUMA_hkf("Ctrl+d", targ));
   SS = SUMA_StringAppend_va (SS, 
   "   %s: Attch to the current dataset 'parent' a dot product\n"
   "        transform. The 'child' (transformed) dataset\n"
   "        is created by calculating the dot product between\n"
   "        each node time series and the time series of the current\n"
   "        node. Each time you 'shift+ctrl+click (drag too if you like)'\n"
   "        on the surface, with the child dataset in view, the dot product\n"
   "        is recalculated. \n"
   "        You can save the resultant datasets with 'ctrl+W' key (see below).\n"
   "        Dset names are automatically formed.\n"
   "        To stop the interactive dot product computations,\n"
   "        switch back to the parent dset and press 'D' again.\n"
   "        If the parent dataset is properly detrended and each \n"
   "        time series is normalized so that its stdev is 1.0\n"
   "        then the dot product is the cross correlation coefficient.\n"
   "        Detrending and normalization can be carried out with:\n\n"
   "           3dDetrend -polort 4 -normalize \\\n"
   "                     -prefix dt.TS.niml.dset \\\n"
   "                     v2s.TS.niml.dset\n\n"
   "        You can get a good feel for what this 'D' does by running\n\n"
   "               @Install_InstaCorr_Demo\n\n"
   "        That script will download and setup demo data for resting-state\n"
   "        correlations. In particular, script @RunSingleSurfInstaCorr of the\n"
   "        demo illustrates the 'D' feature.\n"
      "\n\n", SUMA_hkf("D", targ));
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend_va (SS, 
      "   %s: Show all DO objects in DOv.\n\n", SUMA_hkf("d", targ));
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend_va (SS, 
      "   %s: Look for OpenGL errors.\n\n", SUMA_hkf("Ctrl+r", targ)); 
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Flip light position between +z and -z.\n", SUMA_hkf("F", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: functional overlay, toggle.\n\n", SUMA_hkf("f", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: graph data.\n"
      "%s\n", SUMA_hkf("g", targ), PlotCommonHelp); 
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Highlight nodes inside a specified box.\n"
      "        Does not update other viewers\n"
      "        Paints into existing colors\n"
      "        Highlight is wiped out with new\n"
      "        colors.\n\n", SUMA_hkf("H", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: help message\n\n", SUMA_hkf("Ctrl+h", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Set the selected FaceSet on Surface Object\n"
      "        in Focus. Does not update in other viewers\n"
      "        or in AFNI.\n", SUMA_hkf("J", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Set the cross hair to a certain node on SO in Focus.\n"
      "        Append/prepend L or R to switch hemispheres.\n"
      "        Does update in other viewers if linked by index\n"
      "        and AFNI if connected\n", SUMA_hkf("j", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Set the cross hair's XYZ location. \n"
      "        Does update in other viewers\n"
      "        if linked by XYZ"
      "        and AFNI if connected\n", SUMA_hkf("Ctrl+j", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Set the Focus node. \n"
      "        Cross hair's XYZ remain unchanged.\n"
      "        Does not update in other viewers\n"
      "        or in AFNI\n\n", SUMA_hkf("Alt+j", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Light's XYZ coordinates.\n"
      "        Default setting is 0.0 0.0 %.1f \n", SUMA_hkf("L", targ), 
      1.0 * SUMA_INTITIAL_LIGHT0_SWITCH);
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend_va (SS, 
      "   %s: Dim all lights and colors by a factor of 0.8\n", 
            SUMA_hkf("Ctrl+L", targ) );
   SS = SUMA_StringAppend_va (SS, 
      "   %s: look at point\n", SUMA_hkf("l", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: look at cross hair\n", SUMA_hkf("Alt+l", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Switch locking mode for all viewers \n"
      "             between: No Lock, Index Lock and \n"
      "             XYZ Lock. The switching is order is \n"
      "             based on the lock of the first viewer.\n\n", 
      SUMA_hkf("Ctrl+l", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Dumps memory trace to file \n"
      "                 called malldump.NNN where NNN\n"
      "                 is the smallest number between\n"
      "                 001 and 999 that has not been used.\n", 
      SUMA_hkf("Alt+Ctrl+M", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: momentum, toggle\n\n", SUMA_hkf("m", targ));
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend_va (SS, 
      "   %s: bring a node to direct view (does not work AT ALL)\n", 
      SUMA_hkf("n", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Increase opacity of all surfaces in viewer by 4 levels.\n"
      "       Transparency levels accessible are: \n"
      "       0 (opaque), 25%%, 50%%, 75%%, 100%% (invisible)\n"
      "\n", SUMA_hkf("O", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Decrease opacity of all surfaces in viewer by 4 levels.\n"
      "\n", SUMA_hkf("o", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Set new center of rotation.\n"
      "       Enter nothing to go back to default.\n"
      "\n", SUMA_hkf("Ctrl+o", targ));
   SS = SUMA_StringAppend_va (SS, 
      "  %s: Open a new surface viewer window.\n\n",
      SUMA_hkf("Ctrl+n", targ));
      
   SS = SUMA_StringAppend_va (SS, 
"   %s: Viewer rendering mode  \n"
"        (Fill, Line, Points, Hide), switch.\n\n"
"   %s: Cycle between restrictions of where DO node-based \n"
"         objects are displayed. Available modes are:\n"
"           All: No restrictions\n"
"           n3Crosshair: Crosshair node + 3 neighboring layers\n"
"           n2Crosshair: Crosshair node + 2 neighboring layers\n" 
"           n1Crosshair: Crosshair node only\n"
"           None: Show nothing.\n\n"
"              See also -do_draw_mask option in DriveSuma\n\n"
"        ** DO stands for displayable objects, see 'Ctrl+Alt+s'\n"
"           below.\n"
"        ** For the moment, 'Ctrl+p' only applies to segment \n"
"           and sphere DOs  that are node based. \n"
"           If you need it applied to other DOs, let me know.\n"
      , SUMA_hkf("p", targ), SUMA_hkf("Ctrl+p", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Reset viewer and all surfaces to Fill  \n"
      "        rendering mode.\n\n", SUMA_hkf("P", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: record current image\n"
      "       in an a la AFNI image viewer.\n"
      "       Identical images are rejected.\n"
      "       If you just save one image, the recording\n"
      "       window has no visible controls for saving\n"
      "       the image. Either take another picture, or\n"
      "       use 'Shift+right click' to get a menu.\n\n", SUMA_hkf("r", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Record current image directly to disk.\n"
   "        Images are saved with a date stamp of the\n"
   "        format PREFIX.X.yymmdd_hhmmss.MMM.jpg where:\n\n"
   "        PREFIX controlled with SUMA_AutoRecordPrefix.\n"
   "           See environment variable SUMA_AutoRecordPrefix for\n"
   "           controlling prefix and output image type (suma -update_env).\n"
   "        X  The character indicating which viewer is recording (you can\n"
   "           record from multiple viewers at once.\n"
   "        yy, mm, dd, hh, mm, ss for year, month, day, hours, minutes,\n"
   "           and seconds, respectively. MMM is a millisecond marker to\n"
   "           avoid overwriting files. Unlike the other recording mode \n"
   "           (with the 'R' key), there is no rejection of identical images\n"
   "\n"
   "        This option is useful for saving a large number of images\n"
   "        without running out of memory in the recorder GUI. \n"
   "\n"
   "        Your current PREFIX is: %s%s\n"
   "\n", SUMA_hkf("Ctrl+r", targ), 
   SUMAg_CF->autorecord->Path, SUMAg_CF->autorecord->FileName);
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Increase the image oversampling factor.\n"
      "             By increasing this factor, you can create\n"
      "             images at a resolution higher than that \n"
      "             of the SUMA window. This is done by subdividing \n"
      "             the scene into NxN sections and rendering each\n"
      "             section separately. The NxN renderings are\n"
      "             saved in the image recorder. After you \n"
      "             save the images to disk, you can stitch them\n"
      "             using imcat (a la AFNI montage). \n\n"
      "        Note that each section is still rendered at\n"
      "             the resolution of the SUMA window. So the bigger\n"
      "             the window the more resolution per section.\n"
      "             However, you cannot exceed a certain limit\n"
      "             on the number of pixels in the final image.\n"
      "             This limitation is due to the graphics card\n"
      "             on your system. SUMA will take care not to exceed\n"
      "             this limit.\n", SUMA_hkf("Alt+r", targ));
   SS = SUMA_StringAppend_va(SS, 
   "   %s: Toggle continuous jpeg saving to disk.\n"
   "             Naming of output images is automatic, same as in Ctrl+r.\n"
   "             See help for Ctrl+r above for more info.\n"
   "   %s: Toggle continuous recording \n"
   "        to an a la AFNI image viewer.\n"
   "        Identical images are rejected.\n\n"
   , SUMA_hkf("Ctrl+R", targ), SUMA_hkf("R", targ));

   SS = SUMA_StringAppend_va (SS, 
      "   %s: Open :SPX::ref:`controller <SurfCont>`:DEF:controller:SPX: for \n"
      ":           :surface in Focus.\n", SUMA_hkf("Ctrl+s", targ));
   SS = SUMA_StringAppend_va (SS, 
"   %s: Input filename containing displayable objects.\n"
"                 Files are of 1D format with a necessary comment\n"
"                 at the top to indicate the type of objects in \n"
"                 the file.\n"
"                 Note 1: Repeatedly loading files with the same \n"
"                 name will replace currently loaded versions.\n"
"                 Note 2: Node-based (Types 3 and 4) objects\n"
"                 will follow a node when its coordinates change.\n"
"                 Note 3: See also 'Ctrl+p' for restricting which \n"
"                 node-based objects get displayed.\n\n"
"          Type 1:Segments between (x0,y0,z0) and (x1,y1,z1) \n"
"                 1st line must be '#segments' (without quotes),\n"
"                 or '#oriented_segments' (slower to render).\n"
"                 One can also use #node-based_segments or \n"
"                 #node-based_oriented_segments and use a node index\n"
"                 in place of (x,y,z) triplets.\n"
"                 Remainder of file is N rows, each defining a \n"
"                 segment (or a vector) between two points.\n"
"                 Column content depends on the number of columns\n"
"                 in the file:\n\n"
"              For node-based:\n"
"                 2  cols: n0 n1\n"
"                 3  cols: n0 n1 th\n\n"
"                          with th being line thickness\n\n"
"                 6  cols: n0 n1 c0 c1 c2 c3\n\n"
"                          with c0..3 being the RGBA values\n"
"                          between 0 and 1.0\n\n"
"                 7  cols: n0 n1 c0 c1 c2 c3 th\n"
"                 8  cols: n0 n1 c0 c1 c2 c3 th st\n\n"
"                          with st being a stippling, or dashing for some,\n"
"                          flag. Use integers between 1 and 5 for a variety\n"
"                          of syles.\n\n"
"              For coordinate-based\n"
"                 6  cols: x0 y0 z0 x1 y1 z1\n"
"                 7  cols: x0 y0 z0 x1 y1 z1 th\n\n"
"                          with th being line thickness\n\n"
"                 10 cols: x0 y0 z0 x1 y1 z1 c0 c1 c2 c3\n\n"
"                          with c0..3 being the RGBA values\n"
"                          between 0 and 1.0\n\n"
"                 11 cols: x0 y0 z0 x1 y1 z1 c0 c1 c2 c3 th\n"
"                 12 cols: x0 y0 z0 x1 y1 z1 c0 c1 c2 c3 th st\n\n"
"                          with st defined above.\n\n"
"          Type 2: Directions, a variant of segments and oriented segments.\n"
"                 1st line must be '#directions' (without quotes).\n"
"                 Remainder of file is N rows, each defining a \n"
"                 direction.\n"
"                 Column content depends on the number of columns\n"
"                 in the file:\n\n"
"                 3  cols: dx dy dz\n\n"
"                          with dx dy dz defining the direction. The\n"
"                          triplet need not be of unti norm though that\n"
"                          would affect the default coloring scheme detailed\n"
"                          below. The segment drawn has origin 0, 0, 0\n\n"
"                 4  cols: dx dy dz mag\n\n"
"                          with mag being a scaling factor for the direction.\n"
"                          mag is 1 by default.\n\n"
"                 5  cols: dx dy dz mag th\n\n"
"                          with th being the thickness of the line.\n"
"                          Default is 1\n\n"
"                 6  cols: ox oy oz dx dy dz\n\n"
"                          Specify the origin of the segment in o1, o2, o3.\n"
"                          Default is origin 0, 0, 0 for all\n\n"
"                 7  cols: o1 o2 o3 dx dy dz mag\n\n"
"                          Add individual scaling factors to case above\n"
"                          Segment is from origin to origin+mag*direction\n\n"
"                 8  cols: o1 o2 o3 dx dy dz mag th\n\n"
"                          Add thickness to case with 7 columns\n\n"
"                 9  cols: dx dy dz mag th c0 c1 c2 c3\n\n"
"                          Add colors for each segment, with origin at 0,0,0\n\n"
"                 11 cols: ox oy oz dx dy dz mag c0 c1 c2 c3\n"
"                 12 cols: ox oy oz dx dy dz mag th c0 c1 c2 c3\n\n"
"          Type 3:Spheres centered at (ox, oy, oz) \n"
"                 1st line must be '#spheres' (without quotes).\n"
"                 Remainder of file is N rows, each defining a \n"
"                 sphere.\n"
"                 Column content depends on the number of columns\n"
"                 in the file:\n"
"                 3  cols: ox oy oz\n"
"                 4  cols: ox oy oz rd\n\n"
"                          with rd being the radius of the sphere\n\n"
"                 5  cols: ox oy oz rd st\n\n"
"                          with st being the style of the sphere's\n"
"                          rendering. Choose from:\n\n"
"                             0: points\n"
"                             1: Lines\n"
"                             2: Filled\n\n"
"                 7  cols: ox oy oz c0 c1 c2 c3 \n\n"
"                          with c0..3 being the RGBA values\n"
"                          between 0 and 1.0\n\n"
"                 8  cols: ox oy oz c0 c1 c2 c3 rd\n"
"                 9  cols: ox oy oz c0 c1 c2 c3 rd st\n\n"
"          Type 4:Points at (ox, oy, oz) \n"
"                 1st line must be '#points' (without quotes).\n"
"                 Remainder of file is N rows, each defining a \n"
"                 point.\n"
"                 Column content depends on the number of columns\n"
"                 in the file:\n"
"                 3  cols: ox oy oz\n"
"                 4  cols: ox oy oz sz\n\n"
"                          with sz being the size of the point\n\n"
"                 7  cols: ox oy oz c0 c1 c2 c3 \n\n"
"                          with c0..3 being the RGBA values\n"
"                          between 0 and 1.0\n\n"
"                 8  cols: ox oy oz c0 c1 c2 c3 sz\n"
"          Type 5:Vectors (vx, vy, vz) at surface nodes \n"
"                 1st line must be '#node-based_vectors' (without quotes)\n"
"                 or '#node-based_ball-vectors' (slower to render).\n"
"                 Remainder of file is N rows, each defining a \n"
"                 a vector at a particular node of the current surface.\n"
"                 Column content depends on the number of columns\n"
"                 in the file:\n"
"                 3  cols: vx, vy, vz \n\n"
"                          node index 'n' is implicit equal to row index.\n"
"                          Vector 'v' is from coordinates of node 'n' to \n"
"                          coordinates of node 'n' + 'v'\n\n"
"                 4  cols: n, vx, vy, vz \n\n"
"                          Here the node index 'n' is explicit. You can\n"
"                          have multiple vectors per node, one on \n"
"                          each row.\n\n"
"                 5  cols: n, vx, vy, vz, gn\n\n"
"                          with gn being a vector gain factor\n\n"
"                 8  cols: n, vx, vy, vz, c0 c1 c2 c3\n\n"
"                          with with c0..3 being the RGBA values\n"
"                          between 0 and 1.0\n\n"
"                 9  cols: n, vx, vy, vz, c0 c1 c2 c3 gn\n\n"   
"          Type 6:Spheres centered at nodes n of the current surface\n"
"                 1st line must be '#node-based_spheres' (without quotes).\n"
"                 Remainder of file is N rows, each defining a \n"
"                 sphere.\n"
"                 Column content depends on the number of columns\n"
"                 in the file, see Type 2 for more details:\n"
"                 1  cols: n\n"
"                 2  cols: n rd\n"
"                 3  cols: n rd st\n"
"                 5  cols: n c0 c1 c2 c3 \n"
"                 6  cols: n c0 c1 c2 c3 rd\n"
"                 7  cols: n c0 c1 c2 c3 rd st\n\n"
"          Type 7:Planes defined with: ax + by + cz + d = 0.\n"
"                 1st line must be '#planes' (without quotes).\n"
"                 Remainder of file is N rows, each defining a \n"
"                 plane.\n"
"                 Column content depends on the number of columns\n"
"                 in the file:\n"
"                 7  cols: a b c d cx cy cz\n\n"
"                          with the plane's equation being:\n"
"                          ax + by + cz + d = 0\n"
"                          cx,cy,cz is the center of the plane's\n"
"                          representation. \n"
"                          Yes, d is not of much use here.\n\n"
"                 There are no node-based planes at the moment.\n"
"                 They are a little inefficient to reproduce with\n"
"                 each redraw. Complain if you need them.\n\n"
"         Type 8: Another class of displayble objects is described in\n"
"                 the output of suma -help_nido and the demonstration\n"
"                 script @DO.examples. This new class allows for displaying \n"
"                 text and figures in both screen and world space.\n"
      , SUMA_hkf("Ctrl+Alt+s", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Switch mouse buttons 1 and 3.\n\n", SUMA_hkf("Alt+s", targ));
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend_va (SS, 
      "   %s: Show all surface objects registered in DOv.\n\n", 
      SUMA_hkf("S", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: talk to AFNI, toggle.\n", SUMA_hkf("t", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Force a resend of \n"
      "            surfaces to AFNI.\n\n", SUMA_hkf("Ctrl+t", targ));

   SS = SUMA_StringAppend_va (SS, 
      "   %s: Start listening for niml connections\n\n", SUMA_hkf("T", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Open SUMA controller.\n\n", SUMA_hkf("Ctrl+u", targ));   
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Whereami window of little use at the moment.\n"
      "\n"        , SUMA_hkf("w", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Write items stowed in SUMA's save list.\n"
      "             This is used to write temporary dsets that\n"
      "             are created on the fly in SUMA. Such sets include\n"
      "             those created via the 'D' option above,\n"
      "             or the results sent by 3dGroupInCorr\n"
      "\n", SUMA_hkf("Ctrl+W", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Write ascii files containing the NodeList,\n"
      "        the FaceSetList and the nodecolors of the \n"
      "        surface in focus.\n\n", SUMA_hkf("W", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Zoom in\n", SUMA_hkf("Z", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Zoom out\n", SUMA_hkf("z", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Show/Hide left hemisphere.\n"
      "   %s: Show/Hide right hemisphere.\n"
      "        Window title shows which \n"
      "        hemispheres are shown :LR:\n"
      "        :-R: :L-: or :--:\n\n", SUMA_hkf("[", targ), SUMA_hkf("]", targ));
   SS = SUMA_StringAppend_va (SS, 
      "  %s: Set the number of smoothing iterations\n"
      "     to be applied to the foreground colors.\n"
      "     This setting will be applied to all subsequent\n"
      "     color sets.\n", SUMA_hkf("8", targ));
   SS = SUMA_StringAppend_va (SS, 
      "  %s: Smooth node colors by averaging with neighbors.\n"
      "     The smoothing is only applied to the current colors,\n"
      "     and will be not be applied to new color sets.\n\n", 
      SUMA_hkf("*", targ));
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend_va (SS, 
      "   %s: Compute curvatures along principal directions \n"
      "        on the surface, results written to disk.\n\n", 
      SUMA_hkf("@", targ));
   if (SUMAg_CF->Dev) SS = SUMA_StringAppend_va (SS, 
      "   %s: Compute convexity of surface, \n"
      "        results written to disk.\n\n", SUMA_hkf("(", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   :SPX:%s\n%s:DEF:%s/%s:SPX: (think </>): Switch to next/previous "
      "view state.\n"
      "     Viewing angle is reset only when switching to\n"
      "     a state with flat surfaces.:LR:\n"
      "     See :term:`state` for more on the meaning of states for different "
      "object types.\n\n",
      SUMA_hkf(",", targ), SUMA_hkf(".", targ),
      SUMA_hkf(",", targ), SUMA_hkf(".", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Toggle between Mapping Reference and\n"
      ":          :Current view state.\n"
      ":          :Viewing angle is reset only when switching to\n"
      ":          :a state with flat surfaces.\n\n"
      , SUMA_hkf("SPACE", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: rotate about screen's Y axis\n"
      , SUMA_hkf("L-R arrows", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: rotate about screen's X axis\n"
      , SUMA_hkf("U-D arrows", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: translate along screen's \n"
      "                       X axis\n", SUMA_hkf("Shift+L-R arrows", targ));
   SS = SUMA_StringAppend_va (SS, 
      "     %s arrows: translate along screen's \n"
      "                       Y axis\n", SUMA_hkf("Shift+U-D arrows", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: LR cardinal views\n"
      , SUMA_hkf("Ctrl+L-R arrows", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: IS cardinal views\n"
      , SUMA_hkf("Ctrl+U-D arrows", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: AP cardinal views\n\n"
      , SUMA_hkf("Ctrl+Shift+U-D arrows", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: rotate CCW and CW about Z screen axis\n\n"
      , SUMA_hkf("Ctrl+Shift+L-R arrows", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Move selected node to neighboring nodes\n"
      "                     in the direction of the screen's \n"
      "                     X axis. The default is to move one\n"
      "                     node at a time. You can alter this\n"
      "                     setting with the environment variable:\n"
      "                     SUMA_KeyNodeJump in your ~/.sumarc file.\n"
      , SUMA_hkf("Alt+L-R arrows", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Same as Alt+L-R but in the direction \n"
      "                     of the screen's Y axis\n\n"
      , SUMA_hkf("Alt+U-D arrows", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: screen axis (X-Red, Y-Green), toggle. \n"
      , SUMA_hkf("F1", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: surface axis (X-Red, Y-Green, Z-Blue), \n"
      "         switch. \n", SUMA_hkf("F2", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: cross hair, toggle. \n", SUMA_hkf("F3", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: node/voxel/edge/cell/tract/tie selection highlight, toggle. \n", 
                              SUMA_hkf("F4", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: FaceSet/Slice selection highlight, toggle.\n", 
         SUMA_hkf("F5", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Viewer background color, toggle.\n", SUMA_hkf("F6", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Switch between color mixing modes.\n"
      "         ORIG: Col = ( 1 - opacity ) * OldCol + opacity * NewCol \n"
      "         MOD1: Col = ( 1 - opacity ) * OldCol +           NewCol \n"
      , SUMA_hkf("F7", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Viewing mode (Perspective or Orthographic Projection), toggle.\n"
      , SUMA_hkf("F8", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Labels at cross hair, toggle.\n"
      , SUMA_hkf("F9", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Toggle prying axis between surfaces' Z and Y axes.\n"
      , SUMA_hkf("F10", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Change object rendering order.\n"
      "          This order will affect the resultant image in\n"
      "          the few instances where alpha transparency is\n"
      "          used. The order can be specified for only three types of \n"
      "          objects for now: graphs, surfaces, and volumes. \n"
      "          If you want to render graphs first, followed by volumes then\n"
      "          surfaces then set SUMA_ObjectDisplayOrder to something like:\n"
      "          'graph,vol,surf', or 'GVS'\n"
      , SUMA_hkf("F11", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Time 20 scene renderings.\n\n", SUMA_hkf("F12", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: reset zoom and recenter surfaces.\n"
      "           rest view angle for flat surfaces only.\n\n"
      , SUMA_hkf("HOME", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: close the surface viewer window.\n", 
      SUMA_hkf("ESCAPE", targ));
   SS = SUMA_StringAppend_va (SS, 
      "   %s: close all surface viewer windows.\n\n"
      , SUMA_hkf("Shift+ESCAPE", targ));
   
   if (targ == TXT) {
      SS = SUMA_StringAppend (SS,
            "Mouse Controls:\n");
   } else if (targ == SPX) {
      SS = SUMA_StringAppend (SS,
            ".. _MouseControls:\n\n"
            "Mouse Controls\n"
            "--------------\n\n");
   }

   SS = SUMA_StringAppend (SS, 
      "*On MACs*, Alt is the Apple/Command key.\n"
      "   If it is commandeered by the OS, and you can't get it back, then\n"
      "   try the alt/option key instead.\n\n"
      "*On Linux*, Turn NumLock OFF, otherwise certainly mouse or \n"
      "   keyboard combinations do not work as intended.\n\n");
      
   SS = SUMA_StringAppend_va (SS, 
      "   %s: For 3D scenes, rotation as if you were \n"
      "       using a trackball.\n"
      "       For matrix displays, Button 1-Motion causes translation because\n"
      "       rotations are of little use in matrix display mode.\n\n"
      "       Pure vertical motion is equivalent to using \n"
      "       the up/down arrow keys.\n\n"
      "       Pure horizontal motion is equivalent to using \n"
      "       the left/right arrow keys.\n\n"
      "       Of course, the advantage to using the mouse is \n"
      "       a continuous range of rotation angles and \n"
      "       simultaneous rotations about the screen's \n"
      "       X & Y axis.\n"
      , SUMA_hkf("Button 1-Motion", targ));
   SS = SUMA_StringAppend_va (SS,    
      "   %s: Rotate surfaces about the Z\n"
      "       axis. This is useful at times for reorienting flat\n"
      "       maps.\n", SUMA_hkf("Shift+Button 1-Motion", targ));
   SS = SUMA_StringAppend_va (SS,    
      "   %s: Undo Z rotation\n", 
      SUMA_hkf("Shift+Button 1-DoubleClick", targ));
   SS = SUMA_StringAppend_va (SS,       
      "   %s: Pry open two hemispheres\n"
      "       so that you can see medial or lateral walls better\n"
      "       from one angle. Prying is disabled for flat surfaces\n"
      "       and with spheres the effect is to rotate each sphere\n"
      "       about the Y axis passing through its center. Also, prying\n"
      "       is only enabled when the state you are viewing contains two \n"
      "       surfaces, one on the left and one on the right.\n"
      "       in 3D views, left right mouse movement cause a rotation about\n"
      "       the front or rear I/S axis.\n"
      "       Up down movements cause a shift along the left/right direction.\n"
      "       You can select nodes (Button 3) on pried surfaces and still\n"
      "       have AFNI jump to the proper location, and vice versa. However\n"
      "       for the moment, you cannot draw in pried mode. If you attempt\n"
      "       to draw, the surfaces are put back together.\n"
      "       To make best use of this option, you want to have env. variable\n"
      "       SUMA_LHunify = YES (see your ~/.sumarc for help)\n",
         SUMA_hkf("Ctrl+Button 1-Motion", targ)); 
   SS = SUMA_StringAppend_va (SS,        
      "  %s: Reset to Home vieweing angle, zooming is\n"
      "       left unchanged. See also 'HOME' key\n",
         SUMA_hkf("Button 1-DoubleClick", targ));
   SS = SUMA_StringAppend_va (SS,       
      "  %s: Undo surface prying.\n",
         SUMA_hkf("Ctrl+Button 1-DoubleClick", targ));
   SS = SUMA_StringAppend_va (SS, 
      "  %s: Translation for 3D scenes. Rotation for\n"
      "                      matrix displays.\n",
         SUMA_hkf("Button 2-Motion", targ)); 
   
   SS = SUMA_StringAppend_va (SS, 
      "  %s"
      ":SPX:"
      " or :kbd:`Button 1+2-Motion`: \n"
      ":DEF:"
      ":    OR \n"
      "  Button 1+2-Motion: \n"
      ":SPX:"
      "          Zoom in/out\n",
         SUMA_hkf("Shift+Button2-Motion", targ));
         
   SS = SUMA_StringAppend_va (SS, 
"  %s: Node/Voxel/Edge/etc. picking.\n"
"                     In ROI mode, initiates a path to new node in DrawROI mode.\n"
"                     Picking of graph edges/nodes can get difficut \n"
"                     when surfaces are also displayed. To help with that,\n"
"                     see Alt+Button 3-Press next.:LR:\n"
"                Also see :ref:`Selecting Objects<Selecting_Objects>` below.\n",
         SUMA_hkf("Button 3-Press", targ));
   SS = SUMA_StringAppend_va (SS,    
      "  %s: Graph edge/node picking in the presence \n"
      "                         of surfaces. Intersections with surfaces\n"
      "                         are ignored.\n",
         SUMA_hkf("Alt+Button 3-Press", targ));
   SS = SUMA_StringAppend_va (SS,    
      "  %s: Same as Alt+Button 3, but also display\n"
      "                         pick buffer. This is mostly for debugging or \n"
      "                         for understanding why selection is behaving\n"
      "                         strangely.\n",
         SUMA_hkf("Shift+Alt+Button 3-Press", targ));
   SS = SUMA_StringAppend_va (SS,    
      "  %s: Shows an image of the selection buffer for debugging purposes. \n"
      ":    :In :ref:`Draw ROI Mode <Draw_ROI_Mode>`, the selection buffer is\n"
      ":    :not displayed and the effect of the click is to select a node, \n"
      ":    :but not to include it in the ROI.\n",
         SUMA_hkf("Shift+Button 3-Press", targ));
   SS = SUMA_StringAppend_va (SS, 
      "  %s: Yoke intensity selection to index of \n"
      "                          selected node*K. This is only possible if\n"
      "                          the currently visualized dataset has K times \n"
      "                          many sub-bricks as the surface has nodes. \n"
      "                          K is an integer.\n",
         SUMA_hkf("Ctrl+Button 3-Press", targ));
   SS = SUMA_StringAppend_va (SS, 
      "  %s: Pick and initiate call in Dot xform\n"
      "                               mode, or to GroupInCorr\n",
         SUMA_hkf("Shift+Ctrl+Button 3-Press", targ));
   SS = SUMA_StringAppend_va (SS,     
      "  %s: If double clicking on a tract mask, select\n"
      "                           the tract mask and turn the viewer into Mask\n"
      "                           Manipulation Mode. In this mode, the mask is\n"
      "                           shown as a wiremesh, and selections on any \n"
      "                           object will move the mask to that location.\n"
      "                           New tract/masks intersections are computed\n"
      "                           at the new location.\n"
      "                           To leave Mask Manipulation Mode, double \n"
      "                           click with button 3 either on the mask or \n"
      "                           in an area void of any objects.\n" 
      "                           If double clicking with a graph object in \n"
      "                           focus and only connections from one node  \n"
      "                           are shown, then revert to showing all graph\n"
      "                           connections. Without this, you can loose all\n"
      "                           other clickables if a certain node is not \n"
      "                           connected to anything.\n",
         SUMA_hkf("Button 3-DoubleClick", targ));
   SS = SUMA_StringAppend_va (SS, 
      "  %s: continuous picking whenever surface are present.\n"
      "                      No calls for dot product (InstaCorr)\n"
      "                      or GroupInCorr, while dragging.\n"
      "                      Continuous picking of graph edges/nodes if no\n"
      "                      surfaces are displayed.:LR:\n"
      "          See :ref:`Continuous Selection<Continuous_Selection>` below\n",
         SUMA_hkf("Button 3-Motion", targ));
   SS = SUMA_StringAppend_va (SS, 
      "  %s: Continuous picking of graph edges/nodes. \n"
      "                      Intersections with surfaces are ignored.\n" ,
         SUMA_hkf("Alt+Button 3-Motion", targ));
   SS = SUMA_StringAppend_va (SS, 
      "  %s: continous yoking of intensity selection to\n"
      "                           selected node*K.\n",
         SUMA_hkf("Ctrl+Button 3-Motion", targ)); 
   SS = SUMA_StringAppend_va (SS, 
      "  %s: Continuous picking and calls \n"
      "                               for dot product (InstaCorr)\n"
      "                               or GroupInCorr, while dragging.\n",
         SUMA_hkf("Shift+Ctrl+Button 3-Motion", targ));
   SS = SUMA_StringAppend_va(SS,
      "  %s or Wheel: Zoom in/out\n",
         SUMA_hkf("Scroll", targ));
   SS = SUMA_StringAppend_va(SS,
      "  %s or Shift+Wheel: change selected slice if current selected\n"
      "                         object is a volume.\n",
         SUMA_hkf("Shift+Scroll", targ));
   SS = SUMA_StringAppend_va(SS,
      "  %s or Ctrl+Wheel: change the size of the currently selected \n"
      "                        tract mask. This only works when you're in\n"
      "                        mask manipulation mode.\n",
         SUMA_hkf("Ctrl+Scroll", targ));
   SS = SUMA_StringAppend (SS, 
      "    \n");
   
   if (targ == TXT) {
      SS = SUMA_StringAppend (SS,
            "Selecting Objects:\n");
   } else if (targ == SPX) {
      SS = SUMA_StringAppend (SS,
                  "\n\n.. _Selecting_Objects:\n\n"
                  "Selecting Objects\n"
                  "-----------------\n\n");
   }
   
   SS = SUMA_StringAppend (SS,
"Selections are done with the :ref:`right-mouse click <Button_3-Press>` "
"unless you request :ref:`otherwise <SUMA_SwapButtons_1_3>`.\n\n"
"The selection of an object triggers a multitude of actions:\n\n"
"* When :ref:`talking<LC_t>` to AFNI, a selection prompts AFNI to also jump "
"to the corresponding location. SUMA can also talk to other programs such as"
"HalloSuma\n\n"
"* The controller for that object is popped to the top of the stack in the "
"controllers notebook, and the crosshair information in the controller gets "
"updated.\n\n"
"* Other open SUMA controllers are made to jump to the corresponding "
"locations. Use the SUMA controller (:ref:`Ctrl+u<LC_Ctrl+u>`) to setup "
"how different controllers are locked together. \n\n"
"* When in :ref:`drawing ROIs mode<Draw_ROI_Mode>` a selection adds "
"to the ROI being drawn. See :ref:`drawing_ROIs` for details, assuming it "
"is written by now!\n\n"
"* If you have 'click callbacks' initiated, a selection combined with "
"the proper keyboard modifiers initiates a callback. An example of this "
"would be the :ref:`surface-based instacorr<Shift+Ctrl+Button_3-Press>` "
"or the variety of instacorr features in AFNI and/or 3dGroupInCorr. "
"The following command can download and install demo material for "
"InstaCorr excitement::\n\n"
"  @Install_InstaCorr_Demo -mini\n\n"
"* If you are in :ref:`Mask Manipulation Mode<Mask_Manipulation_Mode>` "
"Selections will make the tract mask jump to the selection location.\n\n" 
"Picking behavior depends on the object being selected as follows:\n\n"
"1- Node picking on surfaces: Selection of a node on the surface involves "
"finding intersected triangles, identifying the closest intersected triangle, "
"and then indentifying the closest node within it. The crosshair is centered "
"at the location of intersection and marked with a yellow sphere. The closest "
"node in the triangle is marked with a small (tiny some say) blue sphere, and "
"the triangle is highlighted with a gray line contour. Highlighting can be "
"toggled with :ref:`F3 for crosshair<F3>`, :ref:`F4 for selected node<F4>`, "
"and :ref:`F5 for the triangle<F5>`\n\n"
":SPX:"
".. figure:: media/surface_selection.jpg\n"
"    :figwidth: 30%\n"
"    :align: center\n\n"
":SPX:"
"2- Voxel picking in volumes: You can select voxels on rendered slices as "
"long as the voxels are not thresholded out of view. They maybe too dark to "
"see but still be selectable if their value exceeds that of the threshold.\n\n"
"Selecting a voxel also highlights the slice. You can turn off the highlight "
"rectangle with :ref:`F5 <F5>`.\n\n"
"3- Edge/cell selection in graphs: Right click on an edge, matrix cell, "
"or bundle reprenting the edge and the connection is rendered white. Because "
"the graphs can be bidirectional, clicking on an edge between [n1, n2] with the "
"click location closest to n1 would select edge n1-->n2, while clicking closer "
"to n2 gets you edge n2-->n1. This also happens when you click on a bundle "
"representation of the edge. The selected connection is highlighted in white "
"and the highlighting can be toggled with the :ref:`F4 <F4>`\n\n"
"  Selecting an edge on the 3D graph is reflected on the dual representation "
"in matrix form by highlighting the equivalent cell, and vice versa.\n\n"
"  Selecting a node on the 3D graph, by clicking on the ball representing the "
"node, or the node's name highlights only the connections to that node. The "
"same type of selection can be made by clicking on a row or column's label "
"in the matrix representation form.\n\n"
"  When graphs are represented along with volumes and surfaces, picking "
"an edge can get tricky. In that case, use :ref:`Alt+Button 3"
"<Alt+Button_3-press>` instead.\n\n"
"4- Tract selection: Right click on a tract - the hairline - for selecting "
"a location along the tract. What's more to say ?\n\n");

   SS = SUMA_StringAppend (SS,
                  ":SPX:"
                  "\n\n.. _Continuous_Selection:\n\n"
                  "Continuous Selection\n"
                  "^^^^^^^^^^^^^^^^^^^^\n\n"
                  ":DEF:"
                  "Continuous Selection:\n"
                  ":SPX:"
"You can select and drag and sweep through numerous locations. The main thing "
"to keep in mind is that when you have a multitute of object types, such as "
"tracts, voxels, surfaces, etc. SUMA locks the selection to the object type "
"selected at the beginning of the sweep. So, if you begin the selection on a "
"surface and drag, then the selections during the sweep are restricted to "
"surfaces only.\n\n");
   
   if (targ == TXT) {
      SS = SUMA_StringAppend (SS,
            "Viewer Menus:\n");
   } else if (targ == SPX) {
      SS = SUMA_StringAppend (SS,
            ".. _Viewer_Menus:\n\n"
            "Viewer Menus\n"
            "------------\n\n");
   }
   
   if (targ == TXT) {
      SS = SUMA_StringAppend (SS,
            "File:\n");
   } else if (targ == SPX) {
      SS = SUMA_StringAppend (SS,
            ".. _File_Menu:\n\n"
            "File\n"
            "^^^^\n\n");
   }
   
   SS = SUMA_StringAppend_va (SS, 
      "  %s: Save viewer's display settings.\n"
      "  %s: Load and apply display settings.\n"
      "  %s: Close this viewer.\n"
      "           Exit SUMA if this is the only viewer.\n\n",
         SUMA_hkf("->Save View", targ),
         SUMA_hkf("->Load View", targ),
         SUMA_hkf("->Close", targ));
   
   if (targ == TXT) {
      SS = SUMA_StringAppend (SS,
            "View:\n");
   } else if (targ == SPX) {
      SS = SUMA_StringAppend (SS,
            ".. _View_Menu:\n\n"
            "View\n"
            "^^^^\n\n");
   }

   SS = SUMA_StringAppend_va (SS, 
      "  %s: Open SUMA controller interface.\n"
      "  %s: Open selected surface's \n"
      "                          controller interface.\n"
      "  %s: Open viewer's controller interface.\n"
      "  \n"
      "  %s: Toggle cross hair display.\n"
      "  %s: Toggle highlight of selected node.\n"
      "  %s: Toggle highlight of selected faceset.\n\n",
         SUMA_hkf("->SUMA Controller", targ),
         SUMA_hkf("->Surface Controller", targ),
         SUMA_hkf("->Viewer Controller", targ),
         SUMA_hkf("->Cross Hair", targ),
         SUMA_hkf("->Node in Focus", targ),
         SUMA_hkf("->Selected Faceset", targ));
   
   if (targ == TXT) {
      SS = SUMA_StringAppend (SS,
            "Tools:\n");
   } else if (targ == SPX) {
      SS = SUMA_StringAppend (SS,
            ".. _Tools_Menu:\n\n"
            "Tools\n"
            "^^^^^\n\n");
   }      
   SS = SUMA_StringAppend_va (SS, 
      "  %s: Open :ref:`Draw ROI controller<drawing_rois>`, also with :ref:`Ctrl+d<LC_Ctrl+d>`.\n\n",
      SUMA_hkf("->Draw ROI", targ));
   
   if (targ == TXT) {
      SS = SUMA_StringAppend (SS,
            "Help:\n");
   } else if (targ == SPX) {
      SS = SUMA_StringAppend (SS,
            ".. _Help_Menu:\n\n"
            "Help\n"
            "^^^^\n\n");
   }     
   SS = SUMA_StringAppend_va (SS, 
      "   %s: Opens window with this message.\n"
      "   %s: Opens window that will \n"
      "                   contain errors and warnings\n"
      "                   typically output to screen.\n"
      "   \n"
      "   %s: Output debugging information\n"
      "                   about some of SUMA's global \n"
      "                   structure's variables.\n"
      "   %s: Output debugging info on \n"
      "                     a viewer's structure.\n"
      "   %s: Output debugging info on\n"
      "                      the selected surface's struct.\n"
      "   \n"
      "   %s: Turn on/off function in/out tracing.\n"
      "   %s: Turn on memory tracing.\n"
      "                Once turned on, this can't be turned off.\n\n"
      "\n",
         SUMA_hkf("->Usage", targ),
         SUMA_hkf("->Message Log", targ),
         SUMA_hkf("->SUMA Global", targ),
         SUMA_hkf("->Viewer Struct", targ),
         SUMA_hkf("->Surface Struct", targ),
         SUMA_hkf("->InOut Notify", targ),
         SUMA_hkf("->MemTrace", targ) );
   
      
   /* Environment variables */
   if (targ == TXT) {
      SS = SUMA_StringAppend (SS,
            "SUMA's environment vars:\n");
   } else if (targ == SPX) {
      SS = SUMA_StringAppend (SS,
            ".. _ENV_List:\n\n"
            "SUMA's env.\n"
            "-----------\n\n"
            "Below is a list of all of SUMA's environment variables and"
            " their default values.\n"
            "You can query the value of a variable as SUMA sees it with:\n\n"
            "     :command:`suma -Vname=` \n\n"
            "with 'name' replaced by the environment variable's name.\n\n"
            "     e.g: :command:`suma -VSUMA_ArrowRotAngle=`\n\n"
            "*Always* update your environment variable list with:\n\n"
            "     :command:`suma -update_env`\n\n"
            "List of Variables\n"
            "^^^^^^^^^^^^^^^^^\n"  );
   }  
   s = SUMA_env_list_help(0, targ);
   SS = SUMA_StringAppend( SS, s); SUMA_free(s); s = NULL;
   SS = SUMA_StringAppend( SS, "\n");
   
   if (targ == TXT) {
      SS = SUMA_StringAppend (SS, 
         "    More help at \n"
         "    http://afni.nimh.nih.gov/pub/dist/edu/latest/suma/suma.pdf\n");
      SS = SUMA_StringAppend (SS, 
         "\n");
   }
   
   /* add latest additions */
   SS = SUMA_StringAppend (SS, "Current Version Info:\n");
   s = SUMA_New_Additions (0, 0);
   SS = SUMA_StringAppend (SS, s); SUMA_free(s); s = NULL;

   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN (SUMA_Sphinx_String_Edit(&s, targ, 0));

}

char * SUMA_help_xform_dot_message_Info(void)
{
   static char FuncName[]={"SUMA_help_xform_dot_message_Info"};
   char stmp[1000], *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);

   SS = SUMA_StringAppend (SS, 
"Interface for InstaCorr on the surface.\n"
"Picking a node will initiate the computation\n"
"of the cross correlation between the time series at that node\n"
"and those on other nodes. The time series come from the dataset\n"
"on which the xform is applied (see TS Parents below).\n"
"\n"
"This transform is applied to time series datasets. It is initiated\n"
"by pressing 'D' on a 'parent' time series dataset. \n"
"The result of the transform is one (or more) 'child' dataset that\n"
"will initiate a new transform call with every new node selected.\n"
"\n"
"You can save the resultant datasets by pressing 'ctrl+W' in the SUMA\n"
"window. The datasets are automatically named to reflect the seed location\n"
"and the dataset whose voxels time series are dotted by the seed's.\n"
"\n"  
   ); 
   
   SS = SUMA_StringAppend (SS, 
"xform block:\n"
"------------\n"
"Active: Activates/Deactivates transform. When active, selecting a node\n"
"        (right click) on the child dataset, recomputes the correlation \n"
"        using a reference time series from the parent timeseries dataset\n"
"        at the selected node.\n"
"        When toggled off, selecting a node does not cause a recalculation\n"
"        Another way to select a node without initiating a new calculation is\n"
"        to use 'Shift+Right Click\n"
"\n"
   );


   SS = SUMA_StringAppend (SS, 
"datasets block:\n"
"------------\n"
"TS Parents: Names of parent datasets. These would be the datasets providing\n"
"            the time series for correlation calculations\n"
"Preprocessed Dsets: \n"
"  Save: Save parent datasets after they were preprocessed per the options \n"
"        set in the 'options' block below. \n"
"        Preprocessed data is maintained in memory, but it is accessible \n"
"        to the user from the 'Switch Dset' button. By default, this dataset\n"
"        is not displayed and is positioned below its parent timeseries.\n"
"        You can change all these settings as you would for any datasets, and\n"
"        you can graph the preprocessed time series with the 'g' key.\n"
"\n"
   );
   
   SS = SUMA_StringAppend (SS, 
"options block:\n"
"--------------\n"
"Band Pass Range: Set the range of bandpass filtering.\n"
"  LF: Low frequency in Hz. Default is 0.01 Hz\n"
"      Use a value of 0.0 to have a low pass filter.\n"
"  HF: High frequency in Hz. Default is the lowest of \n"
"      0.1Hz and the Nyquist frequency. \n"
"      Use a value of 1.0 to have a high pass filter.\n"
"  Note that even if LF = 0.0 and HF = 1.0, the 0th and Nyquist\n"
"      frequencies are always filtered. \n"
"  polort: Always set to 2 for now, and always applied.\n"
"  OrtFile: Load an extra set of regressors of no interest.\n"
"  Options:\n"
"     Save: Save options structure.\n"
"     Apply: Apply changes to options structure.\n"
"            Changes do not take effect unless 'Apply' is pressed.\n"             "Disp. Cont. block:\n"
"------------------\n"
"   Close: Close xform window. Settings are preserved.\n"
"   BHelp: Try it.\n"
"   Help: You're reading it.\n"
   );
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN (s);

}

/*!
Controls help message
*/
void SUMA_help_message(FILE *Out, TFORM targ)
{
	char *s=NULL;
   static char FuncName[]={"SUMA_help_message"};
   
   SUMA_ENTRY;

   if (Out == NULL) {
		Out = stdout;
	}
   
   s = SUMA_help_message_Info(targ);
   if (!s) {
      SUMA_S_Err("Failed in SUMA_help_message_Info.\n");
   }else {
      fprintf (Out, "%s\n", s);
      SUMA_free(s);
   }
	
   SUMA_RETURNe;
}

void SUMA_cmap_help_message(FILE *Out, TFORM targ)
{
	char *s=NULL;
   static char FuncName[]={"SUMA_cmap_help_message"};
   
   SUMA_ENTRY;

   if (Out == NULL) {
		Out = stdout;
	}
   
   s = SUMA_help_Cmap_message_Info(NULL, targ);
   if (!s) {
      SUMA_S_Err("Failed in SUMA_help_Cmap_message_Info.\n");
   }else {
      fprintf (Out, "%s\n", s);
      SUMA_free(s);
   }
	
   SUMA_RETURNe;
}

char *SUMA_All_Programs(void )
{
   char *s=NULL;
   static char FuncName[]={"SUMA_All_Programs"};
   SUMA_STRING  *SS = NULL; 
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   SS = SUMA_StringAppend ( SS,
         "+ List of programs in the SUMA package:\n"
         "  3dCRUISEtoAFNI\n"
         "  3dBRAIN_VOYAGERtoAFNI\n"
         "  3dSkullStrip\n"
         "  3dSurf2Vol\n"
         "  3dSurfMask\n"
         "  3dVol2Surf\n"
         "  AnalyzeTrace\n"
         "  CompareSurfaces\n"
         "  ConvertSurface\n"
         "  ConvexHull\n"
         "  CreateIcosahedron\n"
         "  DriveSuma\n"
         "  FSread_annot\n"
         "  inspec\n"
         "  IsoSurface\n"
         "  MakeColorMap\n"
         "  MapIcosahedron\n"
         "  quickspec\n"
         "  ROI2dataset\n"
         "  ROIgrow\n"
         "  ScaleToMap\n"
         "  SUMA_glxdino\n"
         "  SurfaceMetrics\n"
         "  SurfClust\n"
         "  SurfDist\n"
         "  SurfDsetInfo\n"
         "  SurfInfo\n"
         "  SurfMeasures\n"
         "  SurfMesh\n"
         "  SurfPatch\n"
         "  SurfQual\n"
         "  SurfSmooth\n"
         "  SurfToSurf\n"
     );
   
   /* clean SS */
   SS = SUMA_StringAppend (SS, NULL);
   /* copy s pointer and free SS */
   s = SS->s;
   SUMA_free(SS); 
   
   SUMA_RETURN(s);      
   
}

/*!
SUMA version 
*/

void SUMA_Version (FILE *Out)
{
   static char FuncName[]={"SUMA_Version"};
   char *s = NULL;
   

   if (Out == NULL) {
		Out = stdout;
	}
   
   s = SUMA_New_Additions (0, 0);
	if (s) {
      fprintf (Out, "\n   %s\n", s);
      SUMA_free(s);
   } else {
      fprintf (Out, "\n");
   }
   
	return;
}

/*!
Surface .. Volume relationships
*/
void SUMA_VolSurf_help (FILE *Out)
{
	if (Out == NULL) {
		Out = stdout;
	}
    fprintf (Out, "SUMA_VolSurf_help: This function is obsolete.\n");
	 return;
	 fprintf (Out, "\nVolume <--> Surface jeremiad:\n");
	 fprintf (Out, "-----------------------------\n");
	 fprintf (Out, "\tTo transform surface node coordinates to voxel coordinates a few parameters are required.\n");
	 fprintf (Out, "\tThose paramters vary depending on the type of surfaces used. Currently, SUMA supports \n");
	 fprintf (Out, "\tFreeSurfer and SureFit surfaces.\n");
	 fprintf (Out, "\nParent Volume (VolPar):\n");
	 fprintf (Out, "\tThe surface model is created from a high-resolution anatomical scan\n"); 
	 fprintf (Out, "\treferred to as Parent Volume (VolPar).\n"); 
	 fprintf (Out, "\tTo align the surface with data from a particular experiment, VolPar must\n"); 
	 fprintf (Out, "\tbe brought to alignment with the experiemt's data.\n"); 
	 fprintf (Out, "\tFor example, VolPar is aligned with data from experiment Day1 using:\n"); 
	 fprintf (Out, "\t3dvolreg -clipit -twopass -twodup -zpad 8 -rotcom -verbose \n"); 
	 fprintf (Out, "\t-base SPGR_Day1 -prefix VolPar_Day1 VolMast >>& VolParAlignLog\n");
	 fprintf (Out, "\twhere SPGR_Day1 is the high-resolution anatomical scan obtained in\n"); 
	 fprintf (Out, "\texperiment Day1 and VolPar_Day1 is VolPar aligned to SPGR_Day1.\n"); 
	 fprintf (Out, "\nSurface segmentation programs typically require the resolution of VolPar to\n"); 
	 fprintf (Out, "\tbe 1x1x1mm. Such volumes, especially for FreeSurfer are quite large and\n"); 
	 fprintf (Out, "\t3dvolreg might run out of memory. If that happens, you could resample \n"); 
	 fprintf (Out, "\tVolPar to a lower resolution such as 1.2x1.2x1.2mm, prior to registration. \n"); 
	 fprintf (Out, "\tNote that SPGR_Day1 must have the same resolution and number of slices as VolPar.\n"); 
	 fprintf (Out, "\n\t+FreeSurfer Parent Volume:\n"); 
	 fprintf (Out, "\tConstruct VolPar from the .COR images used to create the surface using:\n"); 
	 fprintf (Out, "\tto3d -prefix CW-cSurfParent-SPGR -xSLAB 127.5L-R -ySLAB 127.5S-I -zSLAB 127.5P-A COR-???\n");
	 fprintf (Out, "\tExample command line for a FreeSurfer suface with VolPar aligned to experiment ARzs:\n"); 
	 fprintf (Out, "\t./suma -vp CW-cSurfParent-SPGR_Reg2_ARzsspgrax_1mm_256pad_cor_RSP_down12+orig\\\n");
	 fprintf (Out, "\t -spec CW-FreeSurfer.SumaSpec\n");
	 fprintf (Out, "\n\t+SureFit Parent Volume:\n"); 
	 fprintf (Out, "\tVolPar is the anatomical 1x1x1 mm volume in the correct orientation (LPI) \n"); 
	 fprintf (Out, "\tthat is used by SureFit to create the surface. Typically, this volume has \n"); 
	 fprintf (Out, "\tthe .Orient string in its name unless it was in LPI orientation from the \n"); 
	 fprintf (Out, "\tstart. Because SureFit crops the volume before segmentation, it is also \n"); 
	 fprintf (Out, "\tnecessary to supply the .params file along with VolPar. The .params file is \n"); 
	 fprintf (Out, "\ttypically named something like: (anything here).L.full.sMRI.params for the \n"); 
	 fprintf (Out, "\tleft full hemisphere.  Example command line for a SureFit surface with VolPar:\n"); 
	 fprintf (Out, "\t./suma -vp colin_short_Orient+orig. colin_short+orig.L.full.sMRI.params\\\n");
	 fprintf (Out, "\t -s_s colin.fiducial.coord colin.topo");
	 fprintf (Out, "\nor:\n");
	 fprintf (Out, "\t./suma -vp CW-cSurfParent-SPGR-AX_LPI+orig. -spec CW-SureFit.SumaSpec\n");
	 fprintf (Out, "\t\n"); 
	 return;
}



char * SUMA_Help_AllSurfCont_old ()
{
   static char FuncName[]={"SUMA_Help_AllSurfCont_old"};
   char *s = NULL;
   SUMA_STRING *SS = NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   SS = SUMA_StringAppend(SS, 
         "\n"
         "\n"
         "----------------------------\n"
         "Help for Surface Controller:\n"
         "----------------------------\n"
         "The surface controller is for \n"
         "controlling properties pertinent\n"
         "to the surface selected (in focus).\n"
         "The Surface Controller is launched\n"
         "with 'ctrl+s' or \n"
         "      View-->Object Controller .\n"
         "\n"
         );
   SS = SUMA_StringAppend_va(SS, 
         "+ Surface Properties Block:\n"
         "\n"
         "++ more:\n%s\n"
         "\n"
         "++ Drw (DrawingMode, RenderMode):\n%s\n"
         "\n"
         "++ Trn (Transparency):\n%s\n"
         "\n"
         "++ Dsets:\n%s\n"
         "\n", 
         SUMA_SurfContHelp_more, SUMA_SurfContHelp_RenderMode,
         SUMA_SurfContHelp_TransMode, SUMA_SurfContHelp_Dsets);
   SS = SUMA_StringAppend_va(SS, 
         "+ Xhair Info Block:\n"
         "\n"
         "++ Xhr:\n%s\n"
         "\n"
         "++ Node:\n%s\n"
         "\n"
         "++ Tri:\n%s\n"
         "\n"
         "++ Node Values Table: %s\n"
         "+++ Col. Intens\n%s\n"
         "+++ Col. Thresh\n%s\n"
         "+++ Col. Bright:\n%s\n"
         "+++ Row  Val:\n%s\n"
         "\n"
         "++ Node Label Table:\n"
         "+++ Row  Lbl:\n%s\n"
         "\n",
         SUMA_SurfContHelp_Xhr, SUMA_SurfContHelp_Node, SUMA_SurfContHelp_Tri, 
         SUMA_SurfContHelp_NodeValTblr0, SUMA_SurfContHelp_NodeValTblc1,  
         SUMA_SurfContHelp_NodeValTblc2, SUMA_SurfContHelp_NodeValTblc3, 
         SUMA_SurfContHelp_NodeValTblr0,
         SUMA_SurfContHelp_NodeLabelTblr0);
   
   SS = SUMA_StringAppend_va(SS, 
         "+ Dset Controls Block:\n"
         "\n"
         "++ Dset Info Table: \n"
         "+++ Row  Lbl:\n%s\n"
         "+++ Row  Par:\n%s\n"
         "\n"
         "++ Ord:\n%s\n"
         "\n"
         "++ Opa:\n%s\n"
         "\n"
         "++ Dim:\n%s\n"
         "\n"
         "++ view:\n%s\n"
         "\n"
         "++ Switch Dset:\n%s\n"
         "\n", 
         SUMA_SurfContHelp_DsetLblTblr0, SUMA_SurfContHelp_DsetLblTblr1, 
         SUMA_SurfContHelp_DsetOrd, SUMA_SurfContHelp_DsetOpa, 
         SUMA_SurfContHelp_DsetDim,
         SUMA_SurfContHelp_DsetView, SUMA_SurfContHelp_DsetSwitch);
   
   SS = SUMA_StringAppend_va(SS,       
         "++ Load Dset:\n%s\n"
         "\n"
         "++ Load Col:\n%s\n"
         "\n", 
         SUMA_SurfContHelp_DsetLoad,
         SUMA_SurfContHelp_DsetLoadCol);
         
   SS = SUMA_StringAppend_va(SS, 
         "+ Dset Mapping Block:\n"
         "\n"
         "++ Mapping Data: \n"
         "\n"
         "+++ I,T Link:\n%s\n"
         "+++ I\n%s\n"
         "++++ v:\n%s\n"
         "+++ T\n%s\n"
         "++++ v\n%s\n"
         "+++ B\n%s\n"
         "++++ v\n%s\n"
         "\n"
         "     Note: %s\n"
         "\n", 
         SUMA_SurfContHelp_Link, 
         SUMA_SurfContHelp_SelInt, SUMA_SurfContHelp_SelIntTgl,
         SUMA_SurfContHelp_SelThr, SUMA_SurfContHelp_SelThrTgl, 
         SUMA_SurfContHelp_SelBrt, SUMA_SurfContHelp_SelBrtTgl,
         SUMA_SurfContHelp_ArrowFieldMenu );
         
   SS = SUMA_StringAppend_va(SS, 
         "++ Mapping Parameters Table:\n%s\n"
         "+++ Col. Min\n%s\n"      
         "+++ Col. Max\n%s\n"
         "+++ Row  I\n%s\n"
         "+++ Row  B1\n%s\n"
         "+++ Row  B2\n%s\n"
         "+++ Row  C\n%s\n"
         "\n", 
         SUMA_SurfContHelp_SetRngTbl_r0,
         SUMA_SurfContHelp_SetRngTbl_c1, SUMA_SurfContHelp_SetRngTbl_c2,
         SUMA_SurfContHelp_SetRngTbl_r1, SUMA_SurfContHelp_SetRngTbl_r2,
         SUMA_SurfContHelp_SetRngTbl_r3, SUMA_SurfContHelp_SetRngTbl_r4);
   
   SS = SUMA_StringAppend_va(SS, 
         "++ Col\n%s\n"
         "\n"
         "++ Bias\n%s\n"
         "\n"
         "Cmp\n%s\n"
         "\n"
         "New\n%s\n"
         "\n"
         "|T|\n%s\n"
         "\n"
         "sym I\n%s\n"
         "\n"      
         "shw 0\n%s\n"
         "\n",      
         SUMA_SurfContHelp_Col, SUMA_SurfContHelp_Bias, SUMA_SurfContHelp_Cmp,
         SUMA_SurfContHelp_CmpNew, SUMA_SurfContHelp_AbsThr, 
         SUMA_SurfContHelp_Isym,
         SUMA_SurfContHelp_Shw0);

   SS = SUMA_StringAppend_va(SS, 
         "++ Data Range Table:\n%s\n"
         "\n"
         "+++ Col Min\n%s\n"
         "+++ Col Node\n%s\n"
         "+++ Col Max\n%s\n"
         "+++ Col Node\n%s\n"
         "+++ Row I\n%s\n"
         "+++ Row T\n%s\n"
         "+++ Row B\n%s\n", 
         SUMA_SurfContHelp_RangeTbl_c0,
         SUMA_SurfContHelp_RangeTbl_c1, SUMA_SurfContHelp_RangeTbl_c2,
         SUMA_SurfContHelp_RangeTbl_c3, SUMA_SurfContHelp_RangeTbl_c4,
         SUMA_SurfContHelp_RangeTbl_r1, SUMA_SurfContHelp_RangeTbl_r2, 
         SUMA_SurfContHelp_RangeTbl_r3);
               
            
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(s);
}

static DList *All_GUI_Help = NULL;

void SUMA_Free_Widget_Help(void *data)
{
   static char FuncName[]={"SUMA_Free_Widget_Help"};
   GUI_WIDGET_HELP *gwh = (GUI_WIDGET_HELP *)data;
   
   SUMA_ENTRY;
   if (data) SUMA_free(data);
   SUMA_RETURNe;
}

int SUMA_Register_GUI_Help(char *which, char *hint, char *help, int type)
{
   static char FuncName[]={"SUMA_Register_GUI_Help"};
   GUI_WIDGET_HELP *gwh=NULL, *gwhc=NULL;
   char *sstmp = NULL, *s=NULL;
   DListElmt *el=NULL;
   int nn;
   SUMA_Boolean LocalHead = NOPE;
   
   SUMA_ENTRY;
   
   if (!hint && !which) {
      SUMA_S_Err("No hint, no which");
      SUMA_RETURN(NOPE);
   }
   
   if (!which) { /* get from hint if it has "which" string between
                   ":which:" directives.
                   ":which:SurfCont->more:which:hint goes here"*/
      if ( !(strstr(hint,":which:") == hint) || 
           !(sstmp = strstr(hint+strlen(":which:"),":which:"))  ) {
         SUMA_S_Err("No which and no :which: in hint. No good");
         SUMA_RETURN(NOPE);        
      }
      which = hint+strlen(":which:");
      hint = sstmp+strlen(":which:");
   }
   
   gwh = (GUI_WIDGET_HELP *)SUMA_calloc(1,sizeof(GUI_WIDGET_HELP));
   
   gwh->type = type;
   
   /* parse which: SurfCont->more */
   sstmp = which;
   gwh->name_lvl = 0;
   while( (s = strstr(sstmp, "->")) && gwh->name_lvl < 9 ) {
      if (s == sstmp) {
         SUMA_S_Err("Empty child in %s\n", which);
         SUMA_free(gwh);
         SUMA_RETURN(NOPE);
      }
      nn = s - sstmp;
      if (nn > 63) {
         SUMA_S_Err("Too wordy for me.");
         SUMA_free(gwh);
         SUMA_RETURN(NOPE);
      }
      strncpy(gwh->name[gwh->name_lvl], sstmp, nn);
                     gwh->name[gwh->name_lvl][nn+1] = '\0';
      sstmp = s+2; /* skip -> */
      ++gwh->name_lvl;
   }
   /* copy last one */
   strncpy(gwh->name[gwh->name_lvl], sstmp, 63); 
                     gwh->name[gwh->name_lvl][64] = '\0'; 
   ++gwh->name_lvl;
   
   /* store the hint */
   if (hint) {
      if (strlen(hint)>255) {
         SUMA_S_Err("Hint too long");
         SUMA_free(gwh);
         SUMA_RETURN(NOPE);
      }
      strncpy(gwh->hint, hint, 255); gwh->hint[255] = '\0';
   } 
   
   /* store the help */   
   gwh->help = help;
   
   /* Put it all in */
   if (!All_GUI_Help) {
      All_GUI_Help = (DList *)SUMA_calloc(1, sizeof(DList));
      dlist_init(All_GUI_Help, SUMA_Free_Widget_Help);
   }
   
   /* insert in list */
   if (!dlist_size(All_GUI_Help)) {
      dlist_ins_next(All_GUI_Help, dlist_head(All_GUI_Help), (void *)gwh);
      SUMA_RETURN(YUP);
   }
   
   SUMA_LH("Inserting '%s' with  %s %s", 
               SUMA_Name_GUI_Help(gwh), gwh->hint, gwh->help);
   /* Insert in alphabetical order */
   el = dlist_head(All_GUI_Help);
   do {
      gwhc = (GUI_WIDGET_HELP *)el->data;
      if ((nn = strcmp(SUMA_Name_GUI_Help(gwhc), 
                       SUMA_Name_GUI_Help(gwh))) == 0) {
         if (1 || LocalHead) {
            SUMA_S_Note("GUI Name %s already in use. No special help entry.",
                        SUMA_Name_GUI_Help(gwh));
            SUMA_DUMP_TRACE("Trace at duplicate GUI name");
            SUMA_free(gwh);
         }
         SUMA_RETURN(YUP);
      } else if (nn < 0) {
         dlist_ins_next(All_GUI_Help, el, (void *)gwh);
         SUMA_RETURN(YUP);
      } else {
         el = dlist_next(el);
      }
   } while (el && el != dlist_tail(All_GUI_Help));
   
   /* Reached bottom without going over, put on the top */
   dlist_ins_prev(All_GUI_Help, dlist_head(All_GUI_Help), (void *)gwh);
   
   /* A debug for when you get to the bottom condition */
   if (LocalHead) {
      SUMA_Show_All_GUI_Help(All_GUI_Help, NULL, 0, 0);
   }
   
   SUMA_RETURN(YUP);
}

char *SUMA_Name_GUI_Help(GUI_WIDGET_HELP *gwh)
{
   static char FuncName[]={"SUMA_Name_GUI_Help"};
   static char sa[10][641], *s=NULL;
   static int nc=0;
   int k;
   
   SUMA_ENTRY;
   
   ++nc; if (nc > 9) nc = 0;
   s = (char *)sa[nc]; s[0] = '\0';
   
   if (!gwh) SUMA_RETURN(s);
   
   for (k=0; k<gwh->name_lvl; ++k) {
      strncat(s,gwh->name[k], 640);
      if (k<gwh->name_lvl-1) strncat(s,"->", 640);
   }
   
   SUMA_RETURN(s);
}

char *SUMA_All_GUI_Help_Info(DList *dl, int detail, int format)
{
   static char FuncName[]={"SUMA_All_GUI_Help_Info"};
   SUMA_STRING *SS=NULL;
   DListElmt *el=NULL;
   char *s=NULL;
   GUI_WIDGET_HELP *gwh=NULL;
   
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);

   if (!dl) {
      SS = SUMA_StringAppend(SS,"NULL dl");  
   } else {
      SS = SUMA_StringAppend_va(SS,
                                "Help for %d widgets. Detail %d, Format %d\n"
                                "--------------------------------------------\n",
                                dlist_size(dl), detail, format);
      el = dlist_head(dl);
      do {
         gwh = (GUI_WIDGET_HELP *)el->data;
         if (!gwh) SUMA_StringAppend(SS,"NULL widget data!");
         else {
               SUMA_StringAppend_va(SS,"Widget: %s\n", SUMA_Name_GUI_Help(gwh));
            if (detail > 0)
               SUMA_StringAppend_va(SS,"  hint: %s\n", gwh->hint);
            if (detail > 1) {
               s = SUMA_copy_string(gwh->help);
               switch (format) {
                  case 0:
                     SUMA_Sphinx_String_Edit(&s, TXT, 0);
                     SUMA_StringAppend_va(SS,"  help: %s\n", s);
                     SUMA_ifree(s);
                     break;
                  default:
                  case 1:
                     SUMA_Sphinx_String_Edit(&s, SPX, 0);
                     SUMA_StringAppend_va(SS,"  help: %s\n", s);
                     SUMA_ifree(s);
                     break;
               }
            }
            SUMA_StringAppend_va(SS,"\n");
         }
         el = dlist_next(el);   
      } while (el);
   }
   
   SUMA_StringAppend_va(SS,"\n");
   
   SUMA_SS2S(SS, s);
   SUMA_RETURN(s);
}

/*!
   Return the help and hint strings stored in the GUI help list.
   
   \param gname (char *)Name of widget
   \param format (int) 0: Default
                       1: Sphinx format
   \param helpout (char **): If Not NULL, this will contain
                             the pointer to a copy of the help string
                             formatted per format.
                             Note that if helpout, *helpout 
                             must be NULL at the function call.
                             You must also free *helpout when 
                             done with it.
   \param hintout (char **): If Not NULL, this will contain
                             the pointer to a copy of the hint string
                             formatted per format. 
                             if (hintout), *hintout must be NULL at  
                             function call. You must also free *hintout 
                             when done with it.
   \param whelp_off (int ): If non-zero, number of blank charcters by which
                            to offset the lines of a widget's help string 
   \return gwh (GUI_WIDGET_HELP *)  A pointer to the structure containing 
                                    the widget help. 
                                    NULL if nothing was found.
*/ 
GUI_WIDGET_HELP *SUMA_Get_GUI_Help( char *gname, TFORM format, 
                                    char **helpout, char **hintout,
                                    int whelp_off)
{
   static char FuncName[]={"SUMA_Get_GUI_Help"};
   char *s = NULL;
   DListElmt *el = NULL;
   int nn;
   GUI_WIDGET_HELP *gwhc=NULL;
   
   SUMA_ENTRY;
   
   if (!gname) { SUMA_S_Err("NULL name"); SUMA_RETURN(gwhc);}
   
   if (!All_GUI_Help || !dlist_size(All_GUI_Help)) {
      SUMA_S_Err("No help list");
      SUMA_RETURN(gwhc);
   }
   if ((helpout && *helpout) || (hintout && *hintout)) {
      SUMA_S_Err("string init error");
      SUMA_RETURN(gwhc);
   }
   
   /* Seek name in list */
   
   /* Find name in list. 
      Note that no attempt at fast search is done here even though the 
      list is alphabetical... */
   gwhc = NULL;
   do {
      if (!el) el = dlist_head(All_GUI_Help);
      else el = dlist_next(el);
      gwhc = (GUI_WIDGET_HELP *)el->data;
      if ((nn = strcmp(SUMA_Name_GUI_Help(gwhc), 
                       gname)) == 0) {
         el = NULL;
      } else {
         gwhc = NULL;
      }
   } while (el && el != dlist_tail(All_GUI_Help));
   
   if (gwhc) {
      if (helpout) {
         *helpout = SUMA_copy_string(gwhc->help);
         if (gwhc->type == 1 && whelp_off) {/* widget and offset requested */
            SUMA_Sphinx_String_Edit(helpout, format, whelp_off);
         } else {
            SUMA_Sphinx_String_Edit(helpout, format, 0);
         }
      }
      if (hintout) {
         *hintout = SUMA_copy_string(gwhc->hint);
         SUMA_Sphinx_String_Edit(hintout, format, 0);
      }
   }
   
   SUMA_RETURN(gwhc);
}

void SUMA_Show_All_GUI_Help(DList *dl, FILE *fout, int detail, int format)
{
   static char FuncName[]={"SUMA_Show_All_GUI_Help"};
   char *s=NULL;
   
   SUMA_ENTRY;
   
   if (!fout) fout = stdout;
   
   s = SUMA_All_GUI_Help_Info(dl, detail, format);
   
   fprintf(fout, "%s", s);
   
   SUMA_ifree(s);
   
   SUMA_RETURNe;
}

char *SUMA_do_type_2_contwname(SUMA_DO_Types do_type)
{
   static char FuncName[]={"SUMA_do_type_2_contwname"};
   static char s[10][64], *ss;
   static int nc=0;

   SUMA_ENTRY;

   ++nc; if (nc > 9) nc=0; ss=s[nc]; ss[0]='\0';

   switch (do_type) {
      case ROIdO_type:
         snprintf(ss, 63,"ROICont");
         break;
      case SO_type:
         snprintf(ss, 63,"SurfCont");
         break;
      case VO_type:
         snprintf(ss, 63,"VolCont");
         break;
      case MASK_type:
         snprintf(ss, 63,"MaskCont");
         break;
      case GRAPH_LINK_type:
         snprintf(ss, 63,"GraphCont");
         break;
      case TRACT_type:
         snprintf(ss, 63,"TractCont");
         break;
      case SDSET_type:
         snprintf(ss, 63,"NoCont");
         break;
      default:
         snprintf(ss, 63,"NOT_SET_FIX_ME");
         SUMA_S_Warn("Not ready for tp %d (%s)",
            do_type, SUMA_ObjectTypeCode2ObjectTypeName(do_type));
         SUMA_DUMP_TRACE("Who rang?");
         break;
   }
   
   SUMA_RETURN(ss);
}

void SUMA_suggest_GUI_Name_Match(char *wname, int nmx, DList *dl)
{
   static char FuncName[]={"SUMA_suggest_GUI_Name_Match"};
   int i, nlot;
   char **lot=NULL, **slot=NULL;
   DListElmt *el=NULL;
   GUI_WIDGET_HELP *gwhc=NULL;
   
   SUMA_ENTRY;
   
   if (!dl) dl = All_GUI_Help;
   
   if (!dl || !dlist_size(dl)) {
      SUMA_S_Err("No list to be had");
      SUMA_RETURNe;
   }
   lot = (char **)SUMA_calloc(dlist_size(dl), sizeof(char *));
   nlot = 0; i = 0;
   gwhc = NULL;
   do {
      if (!el) el = dlist_head(dl);
      else el = dlist_next(el);
      gwhc = (GUI_WIDGET_HELP *)el->data;
      lot[i] = SUMA_copy_string(SUMA_Name_GUI_Help(gwhc));
      ++i;
   } while (el && el != dlist_tail(dl));
   nlot = i;
  
   slot = approx_str_sort(lot, nlot, wname, 0, NULL, 0, NULL, NULL);
   
   if (nmx < 0) nmx = nlot;
   fprintf(SUMA_STDERR,
               "Suggestions for %s\n"
               "---------------\n", wname);
   for (i=0; i < nlot && i < nmx; ++i) {
      fprintf(SUMA_STDERR,
               "                %s\n", slot[i]);
   }
   
   for (i=0; i < nlot; ++i) {
      SUMA_ifree(lot[i]);
      SUMA_ifree(slot[i]);
   }
   SUMA_ifree(lot);
   SUMA_ifree(slot);
   SUMA_RETURNe;
}

char * SUMA_Help_AllSurfCont (TFORM targ)
{
   static char FuncName[]={"SUMA_Help_AllSurfCont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_STRING *SS = NULL;
   char *worder[] = {
                     "SurfCont",
                     "SurfCont->Surface_Properties",
                     "SurfCont->Surface_Properties->more",
                     "SurfCont->Surface_Properties->Drw",
                     "SurfCont->Surface_Properties->Trn",
                     "SurfCont->Surface_Properties->Dsets",
                     "SurfCont->Xhair_Info",
                     "SurfCont->Xhair_Info->Xhr.r00",
                     "SurfCont->Xhair_Info->Node.r00",
                   /*"SurfCont->Xhair_Info->Node[1]",   Hints/help on headings */
                   /*"SurfCont->Xhair_Info->Node[2]",   Hints/help on headings */
                     "SurfCont->Xhair_Info->Tri.r00",
                   /*"SurfCont->Xhair_Info->Tri[1]",   Hints/help on headings */
                   /*"SurfCont->Xhair_Info->Tri[2]",   Hints/help on headings */
                     "SurfCont->Xhair_Info->Val.c00",
                     "SurfCont->Xhair_Info->Lbl.r00",
                     "SurfCont->Dset_Controls",
                     "SurfCont->Dset_Controls->Lbl+Par.r00",
                     "SurfCont->Dset_Controls->Ord",
                     "SurfCont->Dset_Controls->Opa",
                     "SurfCont->Dset_Controls->Dim",
                     "SurfCont->Dset_Controls->Dsp",
                     "SurfCont->Dset_Controls->1",
                     "SurfCont->Dset_Controls->Switch_Dset",
                     "SurfCont->Dset_Controls->Load_Dset",
                     "SurfCont->Dset_Controls->Load_Col",
                     "SurfCont->Dset_Mapping",
                     "SurfCont->Dset_Mapping->IxT",
                     "SurfCont->Dset_Mapping->I",
                     "SurfCont->Dset_Mapping->I->v",
                     "SurfCont->Dset_Mapping->T",
                     "SurfCont->Dset_Mapping->T->v",
                     "SurfCont->Dset_Mapping->B",
                     "SurfCont->Dset_Mapping->B->v",
                     "SurfCont->Dset_Mapping->ThrVal[0]",
                     "SurfCont->Dset_Mapping->Cmap->bar",
                     "SurfCont->Dset_Mapping->Cmap->scale",
                     "SurfCont->Dset_Mapping->Cmap->pval",
                     "SurfCont->Dset_Mapping->SetRangeTable.c00",
                     "SurfCont->Dset_Mapping->SetRangeTable.r01",
                     "SurfCont->Dset_Mapping->SetRangeTable.r02",
                     "SurfCont->Dset_Mapping->SetRangeTable.r03",
                     "SurfCont->Dset_Mapping->Col",
                     "SurfCont->Dset_Mapping->Bias",
                     "SurfCont->Dset_Mapping->Cmp",
                     "SurfCont->Dset_Mapping->Cmp->New",
                     "SurfCont->Dset_Mapping->abs_T",
                     "SurfCont->Dset_Mapping->sym_I",
                     "SurfCont->Dset_Mapping->shw_0",
                     "SurfCont->Dset_Mapping->Clst.c00",
                     "SurfCont->Dset_Mapping->Clst.c01",
                     "SurfCont->Dset_Mapping->RangeTable.c00",
                     "SurfCont->Dset_Mapping->RangeTable.r01",
                     "SurfCont->Dset_Mapping->RangeTable.r02",
                     "SurfCont->Dset_Mapping->RangeTable.r03",
                     "SurfCont->Dset_Mapping->RangeTable.c01",
                     "SurfCont->Dset_Mapping->RangeTable.c02",
                     "SurfCont->Dset_Mapping->RangeTable.c03",
                     "SurfCont->Dset_Mapping->RangeTable.c04",
                     NULL };
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   k = 0;
   while (worder[k]) {
         s = SUMA_gsf(worder[k], targ, &sii, &shh);
         if (!shh || strstr(sii, shh)) {/* help same as hint */
            SS = SUMA_StringAppend_va(SS, "%s\n", s);
         } else {
            SS = SUMA_StringAppend_va(SS, "%s\n%s\n", 
                                   s, shh?shh:"");
         }
         SUMA_ifree(sii); SUMA_ifree(shh);
      ++k;
   }
          
   SUMA_SS2S(SS, s);
      
   SUMA_RETURN(SUMA_Sphinx_String_Edit(&s, targ, 0));
}

void SUMA_Snap_AllSurfCont (char *froot)
{
   static char FuncName[]={"SUMA_Snap_AllSurfCont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   
   SUMA_ENTRY;
   
   ado = SUMA_findany_ADO_WithSurfContWidget(NULL, SO_type);
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURNe;
   if (!SUMA_viewSurfaceCont(NULL, ado, NULL)) {
      SUMA_S_Err("No viewer could be opened for %s", ADO_LABEL(ado));
      SUMA_RETURNe;
   }
   if (!froot) froot = "SurfCont";
   
   s = SUMA_append_replace_string(froot, "ALL.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->Mainform,  s); SUMA_ifree(s);

   s = SUMA_append_replace_string(froot, "Disp_Cont.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->DispFrame,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Surface_Properties.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->SurfFrame,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Xhair_Info.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->Xhair_fr,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Dset_Controls.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->ColPlane_fr,  s); SUMA_ifree(s);

   s = SUMA_append_replace_string(froot, "Dset_Mapping.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->DsetMap_fr,  s); SUMA_ifree(s);

   SUMA_RETURNe;
}

char * SUMA_Help_AllGraphCont (TFORM targ)
{
   static char FuncName[]={"SUMA_Help_AllGraphCont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_STRING *SS = NULL;
   char *worder[] = {
                     "GraphCont",
                     "GraphCont->Graph_Dset_Properties",
                     "GraphCont->Graph_Dset_Properties->more",
                     "GraphCont->Xhair_Info",
                     "GraphCont->Xhair_Info->Xhr.r00",
                     "GraphCont->Xhair_Info->Edge.r00",
                     "GraphCont->Xhair_Info->Node.r00",
                     "GraphCont->Xhair_Info->Val.c00",
                     "GraphCont->Xhair_Info->Lbl.r00",
                     "GraphCont->GDset_Controls",
                     "GraphCont->GDset_Controls->Dim",
                     "GraphCont->GDset_Controls->Bundles",
                     "GraphCont->GDset_Controls->CN",
                     "GraphCont->GDset_Controls->Rd",
                     "GraphCont->GDset_Controls->Rd->Gn",
                     "GraphCont->GDset_Controls->Br",
                     "GraphCont->GDset_Controls->Fo",
                     "GraphCont->GDset_Controls->Cl",
                     "GraphCont->GDset_Controls->Sh",
                     "GraphCont->GDset_Controls->U",
                     "GraphCont->GDset_Controls->Th",
                     "GraphCont->GDset_Controls->Th->Gn",
                     "GraphCont->GDset_Controls->St",
                     "GraphCont->GDset_Mapping",
                     "GraphCont->GDset_Mapping->IxT",
                     "GraphCont->GDset_Mapping->I",
                     "GraphCont->GDset_Mapping->I->v",
                     "GraphCont->GDset_Mapping->T",
                     "GraphCont->GDset_Mapping->T->v",
                     "GraphCont->GDset_Mapping->B",
                     "GraphCont->GDset_Mapping->B->v",
                     "GraphCont->GDset_Mapping->ThrVal[0]",
                     "GraphCont->GDset_Mapping->Cmap->bar",
                     "GraphCont->GDset_Mapping->Cmap->scale",
                     "GraphCont->GDset_Mapping->Cmap->pval",
                     "GraphCont->GDset_Mapping->SetRangeTable.c00",
                     "GraphCont->GDset_Mapping->SetRangeTable.r01",
                     "GraphCont->GDset_Mapping->SetRangeTable.r02",
                     "GraphCont->GDset_Mapping->SetRangeTable.r03",
                     "GraphCont->GDset_Mapping->Col",
                     "GraphCont->GDset_Mapping->Cmp",
                     "GraphCont->GDset_Mapping->Cmp->New",
                     "GraphCont->GDset_Mapping->abs_T",
                     "GraphCont->GDset_Mapping->sym_I",
                     "GraphCont->GDset_Mapping->shw_0",
                     "GraphCont->GDset_Mapping->RangeTable.c00",
                     "GraphCont->GDset_Mapping->RangeTable.r01",
                     "GraphCont->GDset_Mapping->RangeTable.r02",
                     "GraphCont->GDset_Mapping->RangeTable.r03",
                     "GraphCont->GDset_Mapping->RangeTable.c01",
                     "GraphCont->GDset_Mapping->RangeTable.c02",
                     "GraphCont->GDset_Mapping->RangeTable.c03",
                     "GraphCont->GDset_Mapping->RangeTable.c04",
                     NULL };
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   k = 0;
   while (worder[k]) {
         s = SUMA_gsf(worder[k], targ, &sii, &shh);
         if (!shh || strstr(sii, shh)) {/* help same as hint */
            SS = SUMA_StringAppend_va(SS, "%s\n", s);
         } else {
            SS = SUMA_StringAppend_va(SS, "%s\n%s\n", 
                                   s, shh?shh:"");
         }
         SUMA_ifree(sii); SUMA_ifree(shh);
      ++k;
   }
          
   SUMA_SS2S(SS, s);
      
   SUMA_RETURN(SUMA_Sphinx_String_Edit(&s, targ, 0));
}

void SUMA_Snap_AllGraphCont (char *froot)
{
   static char FuncName[]={"SUMA_Snap_AllGraphCont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   
   SUMA_ENTRY;
   
   ado = SUMA_findany_ADO_WithSurfContWidget(NULL, GRAPH_LINK_type);
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURNe;
   if (!SUMA_viewSurfaceCont(NULL, ado, NULL)) {
      SUMA_S_Err("No viewer could be opened for %s", ADO_LABEL(ado));
      SUMA_RETURNe;
   }
   
   if (!froot) froot = "GraphCont";
   
   s = SUMA_append_replace_string(froot, "ALL.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->Mainform,  s); SUMA_ifree(s);

   s = SUMA_append_replace_string(froot, "Disp_Cont.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->DispFrame,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Graph_Dset_Properties.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->SurfFrame,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Xhair_Info.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->Xhair_fr,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "GDset_Controls.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->ColPlane_fr,  s); SUMA_ifree(s);

   s = SUMA_append_replace_string(froot, "GDset_Mapping.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->DsetMap_fr,  s); SUMA_ifree(s);

   SUMA_RETURNe;
}

char * SUMA_Help_AllROICont (TFORM targ)
{
   static char FuncName[]={"SUMA_Help_AllROICont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_STRING *SS = NULL;
   char *worder[] = {
                     "ROICont",
                     "ROICont->ROI",
                     "ROICont->ROI->Draw",
                     "ROICont->ROI->Cont.",
                     "ROICont->ROI->Pen",
                     "ROICont->ROI->Afni",
                     "ROICont->ROI->Dist",
                     "ROICont->ROI->Label",
                     "ROICont->ROI->Value",
                     "ROICont->ROI->Undo",
                     "ROICont->ROI->Redo",
                     "ROICont->ROI->Join",
                     "ROICont->ROI->Finish",
                     "ROICont->ROI->Switch_ROI",
                     "ROICont->ROI->Load",
                     "ROICont->ROI->delete_ROI",
                     "ROICont->ROI->Save",
                     NULL };
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   k = 0;
   while (worder[k]) {
         s = SUMA_gsf(worder[k], targ, &sii, &shh);
         if (!shh || strstr(sii, shh)) {/* help same as hint */
            SS = SUMA_StringAppend_va(SS, "%s\n", s);
         } else {
            SS = SUMA_StringAppend_va(SS, "%s\n%s\n", 
                                   s, shh?shh:"");
         }
         SUMA_ifree(sii); SUMA_ifree(shh);
      ++k;
   }
          
   SUMA_SS2S(SS, s);
      
   SUMA_RETURN(SUMA_Sphinx_String_Edit(&s, targ, 0));
}

void SUMA_Snap_AllROICont (char *froot)
{
   static char FuncName[]={"SUMA_Snap_AllROICont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   
   SUMA_ENTRY;
   
   if (!SUMAg_CF->X->DrawROI) SUMA_RETURNe;
   if (!SUMA_OpenDrawROIController(NULL)) {
      SUMA_S_Err("DrawROI controller could not be open");
      SUMA_RETURNe;
   }
   if (!SUMA_wait_till_visible(SUMAg_CF->X->DrawROI->AppShell, 5000)) {
      SUMA_S_Err("Widget not visible after long wait");
      SUMA_RETURNe;
   }
   if (!froot) froot = "ROICont";

   s = SUMA_append_replace_string(froot, "ALL.jpg",".", 0);   
   ISQ_snapfile2 ( SUMAg_CF->X->DrawROI->form,  s); SUMA_ifree(s);

   s = SUMA_append_replace_string(froot, "ROI.jpg",".", 0);   
   ISQ_snapfile2 ( SUMAg_CF->X->DrawROI->frame,  s); SUMA_ifree(s);
   
   SUMA_RETURNe;
}


char * SUMA_Help_AllVolCont (TFORM targ)
{
   static char FuncName[]={"SUMA_Help_AllVolCont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_STRING *SS = NULL;
   char *worder[] = {
                     "VolCont",
                     "VolCont->Volume_Properties",
                     "VolCont->Volume_Properties->more",
                     "VolCont->Xhair_Info",
                     "VolCont->Xhair_Info->Xhr.r00",
                     "VolCont->Xhair_Info->Ind.r00",
                     "VolCont->Xhair_Info->IJK.r00",
                     "VolCont->Xhair_Info->Val.c00",
                     "VolCont->Xhair_Info->Lbl.r00",
                     "VolCont->Slice_Controls",
                     "VolCont->Ax_slc->Ax",
                     "VolCont->Sa_slc->Sa",
                     "VolCont->Co_slc->Co",
                     "VolCont->Slice_Controls->Trn",
                     "VolCont->Slice_Controls->Slices_At_+",
                     "VolCont->Volume_Rendering_Controls",
                     "VolCont->VR->Ns",
                     "VolCont->VR->Ns->v",
                     "VolCont->Dset_Controls",
                     "VolCont->Dset_Controls->Lbl.r00",
                     "VolCont->Dset_Controls->Dim",
                     "VolCont->Dset_Controls->Avl",
                     "VolCont->Dset_Controls->Ath",
                     "VolCont->Dset_Mapping",
                     "VolCont->Dset_Mapping->IxT",
                     "VolCont->Dset_Mapping->I",
                     "VolCont->Dset_Mapping->I->v",
                     "VolCont->Dset_Mapping->T",
                     "VolCont->Dset_Mapping->T->v",
                     "VolCont->Dset_Mapping->B",
                     "VolCont->Dset_Mapping->B->v",
                     "VolCont->Dset_Mapping->ThrVal[0]",
                     "VolCont->Dset_Mapping->Cmap->bar",
                     "VolCont->Dset_Mapping->Cmap->scale",
                     "VolCont->Dset_Mapping->Cmap->pval",
                     "VolCont->Dset_Mapping->SetRangeTable.c00",
                     "VolCont->Dset_Mapping->SetRangeTable.r01",
                     "VolCont->Dset_Mapping->SetRangeTable.r02",
                     "VolCont->Dset_Mapping->SetRangeTable.r03",
                     "VolCont->Dset_Mapping->Col",
                     "VolCont->Dset_Mapping->Cmp",
                     "VolCont->Dset_Mapping->Cmp->New",
                     "VolCont->Dset_Mapping->abs_T",
                     "VolCont->Dset_Mapping->sym_I",
                     "VolCont->Dset_Mapping->shw_0",
                     "VolCont->Dset_Mapping->RangeTable.c00",
                     "VolCont->Dset_Mapping->RangeTable.r01",
                     "VolCont->Dset_Mapping->RangeTable.r02",
                     "VolCont->Dset_Mapping->RangeTable.r03",
                     "VolCont->Dset_Mapping->RangeTable.c01",
                     "VolCont->Dset_Mapping->RangeTable.c02",
                     "VolCont->Dset_Mapping->RangeTable.c03",
                     "VolCont->Dset_Mapping->RangeTable.c04",
                     NULL };
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   k = 0;
   while (worder[k]) {
         s = SUMA_gsf(worder[k], targ, &sii, &shh);
         if (!shh || strstr(sii, shh)) {/* help same as hint */
            SS = SUMA_StringAppend_va(SS, "%s\n", s);
         } else {
            SS = SUMA_StringAppend_va(SS, "%s\n%s\n", 
                                   s, shh?shh:"");
         }
         SUMA_ifree(sii); SUMA_ifree(shh);
      ++k;
   }
          
   SUMA_SS2S(SS, s);
      
   SUMA_RETURN(SUMA_Sphinx_String_Edit(&s, targ, 0));
}

void SUMA_Snap_AllVolCont (char *froot)
{
   static char FuncName[]={"SUMA_Snap_AllVolCont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   
   SUMA_ENTRY;
   
   ado = SUMA_findany_ADO_WithSurfContWidget(NULL, VO_type);
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURNe;
   if (!SUMA_viewSurfaceCont(NULL, ado, NULL)) {
      SUMA_S_Err("No viewer could be opened for %s", ADO_LABEL(ado));
      SUMA_RETURNe;
   }
   
   if (!froot) froot = "VolCont";
   
   s = SUMA_append_replace_string(froot, "ALL.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->Mainform,  s); SUMA_ifree(s);

   s = SUMA_append_replace_string(froot, "Disp_Cont.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->DispFrame,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Volume_Properties.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->SurfFrame,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Xhair_Info.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->Xhair_fr,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Slice_Controls.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->Slice_fr,  s); SUMA_ifree(s);

   s = SUMA_append_replace_string(froot, "Volume_Rendering_Controls.jpg",".", 0);
   ISQ_snapfile2 ( SurfCont->VR_fr,  s); SUMA_ifree(s);

   s = SUMA_append_replace_string(froot, "Dset_Controls.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->ColPlane_fr,  s); SUMA_ifree(s);

   s = SUMA_append_replace_string(froot, "Dset_Mapping.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->DsetMap_fr,  s); SUMA_ifree(s);

   SUMA_RETURNe;
}


char * SUMA_Help_AllMaskCont (TFORM targ)
{
   static char FuncName[]={"SUMA_Help_AllMaskCont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_STRING *SS = NULL;
   char *worder[] = {"MaskCont",
                     "MaskCont->Masks",
                     "MaskCont->Masks->Mask_Eval.r00",
                     "MaskCont->Masks->Mask_Eval->v",
                     "MaskCont->Masks->Tract_Length.r00",
                     "MaskCont->Masks->Tract_Length->v",
                     "MaskCont->Masks->Table.c00",
                     "MaskCont->Masks->Table.c01",
                     "MaskCont->Masks->Table.c02",
                     "MaskCont->Masks->Table.c03",
                     "MaskCont->Masks->Table.c04",
                     "MaskCont->Masks->Table.c05",
                     "MaskCont->Masks->Table.c06",
                     "MaskCont->Masks->Table.c07",
                     "MaskCont->Masks->Table.c08",
                     "MaskCont->Masks->Table.c09",
                     "MaskCont->Masks->Load_Masks",
                     "MaskCont->Masks->Save_Masks",
                     NULL };
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   k = 0;
   while (worder[k]) {
         s = SUMA_gsf(worder[k], targ, &sii, &shh);
         if (!shh || strstr(sii, shh)) {/* help same as hint */
            SS = SUMA_StringAppend_va(SS, "%s\n", s);
         } else {
            SS = SUMA_StringAppend_va(SS, "%s\n%s\n", 
                                   s, shh?shh:"");
         }
         SUMA_ifree(sii); SUMA_ifree(shh);
      ++k;
   }
          
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(SUMA_Sphinx_String_Edit(&s, targ, 0));
}


void SUMA_Snap_AllMaskCont (char *froot)
{
   static char FuncName[]={"SUMA_Snap_AllMaskCont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   
   SUMA_ENTRY;
   
   ado = SUMA_findany_ADO_WithSurfContWidget(NULL, MASK_type);
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURNe;
   if (!SUMA_viewSurfaceCont(NULL, ado, NULL)) {
      SUMA_S_Err("No viewer could be opened for %s", ADO_LABEL(ado));
      SUMA_RETURNe;
   }
   
   if (!froot) froot = "TractCont";
   
   s = SUMA_append_replace_string(froot, "ALL.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->Mainform,  s); SUMA_ifree(s);

   s = SUMA_append_replace_string(froot, "Disp_Cont.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->DispFrame,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Masks.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->SurfFrame,  s); SUMA_ifree(s);
   
   SUMA_RETURNe;
}

char * SUMA_Help_AllTractCont (TFORM targ)
{
   static char FuncName[]={"SUMA_Help_AllTractCont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_STRING *SS = NULL;
   char *worder[] = {"TractCont",
                     "TractCont->Tract_Properties",
                     "TractCont->Tract_Properties->more",
                     "TractCont->Xhair_Info",
                     "TractCont->Xhair_Info->Xhr.r00",
                     "TractCont->Xhair_Info->Ind.r00",
                     "TractCont->Xhair_Info->BTP.r00",
                     "TractCont->Xhair_Info->Val.c00",
                     "TractCont->Xhair_Info->Lbl.r00",
                     "TractCont->Coloring_Controls",
                     "TractCont->Coloring_Controls->Lbl.r00",
                     "TractCont->Coloring_Controls->Dim",
                     "TractCont->Coloring_Controls->Ord",
                     "TractCont->Coloring_Controls->1",
                     "TractCont->Coloring_Controls->Opa",
                     "TractCont->Coloring_Controls->Ln",
                     "TractCont->Coloring_Controls->Masks",
                     "TractCont->Coloring_Controls->Hde",
                     "TractCont->Coloring_Controls->Gry",
                     "TractCont->Coloring_Controls->Switch_Dset",
                     "Mask_Manipulation_Mode",
                     NULL };
   SUMA_ENTRY;
   
   SS = SUMA_StringAppend (NULL, NULL);
   
   k = 0;
   while (worder[k]) {
         s = SUMA_gsf(worder[k], targ, &sii, &shh);
         if (!shh || strstr(sii, shh)) {/* help same as hint */
            SS = SUMA_StringAppend_va(SS, "%s\n", s);
         } else {
            SS = SUMA_StringAppend_va(SS, "%s\n%s\n", 
                                   s, shh?shh:"");
         }
         SUMA_ifree(sii); SUMA_ifree(shh);
      ++k;
   }
          
   SUMA_SS2S(SS, s);
   
   SUMA_RETURN(SUMA_Sphinx_String_Edit(&s, targ, 0));
}

void SUMA_Snap_AllTractCont (char *froot)
{
   static char FuncName[]={"SUMA_Snap_AllTractCont"};
   char *s = NULL, *shh=NULL, *sii=NULL;
   int k=0;
   SUMA_ALL_DO *ado=NULL;
   SUMA_X_SurfCont *SurfCont=NULL;
   
   SUMA_ENTRY;
   
   ado = SUMA_findany_ADO_WithSurfContWidget(NULL, TRACT_type);
   if (!ado || !(SurfCont=SUMA_ADO_Cont(ado))) SUMA_RETURNe;
   if (!SUMA_viewSurfaceCont(NULL, ado, NULL)) {
      SUMA_S_Err("No viewer could be opened for %s", ADO_LABEL(ado));
      SUMA_RETURNe;
   }
   
   if (!froot) froot = "TractCont";
   
   s = SUMA_append_replace_string(froot, "ALL.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->Mainform,  s); SUMA_ifree(s);

   s = SUMA_append_replace_string(froot, "Disp_Cont.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->DispFrame,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Tract_Properties.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->SurfFrame,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Xhair_Info.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->Xhair_fr,  s); SUMA_ifree(s);
   
   s = SUMA_append_replace_string(froot, "Coloring_Controls.jpg",".", 0);   
   ISQ_snapfile2 ( SurfCont->ColPlane_fr,  s); SUMA_ifree(s);

   SUMA_RETURNe;
}

