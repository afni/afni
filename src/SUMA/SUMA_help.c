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
"  Try the script :ref:`@DO.examples<@DO.examples>` for concrete examples on  \n"
"  displayable objects.\n"
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
