#ifndef SUMA_DEFINE_INCLUDED
#define SUMA_DEFINE_INCLUDED

#define ARRAY 1
#define STRAIGHT 2
#define TRIANGLES 1
#define POINTS 2

#define DRAW_METHOD ARRAY
#define RENDER_METHOD TRIANGLES
#define DO_MESH
#define DO_MESH_AXIS
/*#define ZERO_CENTER*/

#define SUMA_SWAP_BUTTONS_1_3 0 /*!< 1/0 flag for swapping functions of buttons 1 and 3, also controlled by alt+s */ 
#define SUMA_DOUBLE_CLICK_MAX_DELAY 250 /*!< Maximum delay in ms to consider a double click */

#define NODE_COLOR_R 0.35
#define NODE_COLOR_G 0.35
#define NODE_COLOR_B 0.35
#define SUMA_GRAY_NODE_COLOR 0.30
#define SUMA_DIM_AFNI_COLOR_FACTOR 0.5 /*!< 0.4 works well, use higher factors for flashiness scaling factor (0..1) applied to afni's rgb colors, lower values help retain surface shape info */
#define SUMA_AFNI_COLORPLANE_OPACITY 1
#define SUMA_DIM_CONVEXITY_COLOR_FACTOR 0.5
#define SUMA_CONVEXITY_COLORPLANE_OPACITY 1
#define SUMA_BACKGROUND_MODULATION_FACTOR 3   /*!< 0 background does not modulate foreground, 
                                                   Color = Fore * avg_Bright * AttenFactor (0 <= avg_Bright <=1)
                                                   a good setting is such that SUMA_BACKGROUND_ATTENUATION_FACTOR * SUMA_DIM_AFNI_COLOR_FACTOR = 1
                                                    Watch for saturation effects!*/

#define SUMA_MAT_SHININESS_INIT 0 /*!< Surface object shininess, 0 20, 50 .. 128*/
#define SUMA_MAT_SPECULAR_INIT    0.0, 0.0, 0.0, 1.0 /*<! The specular color of the material, keep this and the exponent (that's MAT_SHININESS) 0 to keep shininess down*/
#define SUMA_MAT_AMBIENT_INIT    0.2, 0.2, 0.2, 1.0 /*<! Ambient light has an undetermined direction and is scattered equally in all directions */
#define SUMA_MAT_DIFFUSE_INIT    0.8, 0.8, 0.8, 1.0 /*<! Diffuse light comes from one direction, but is scattered equally in all directions and appears equally bright no matter where the eye is located*/
#define SUMA_MAT_EMISSION_INIT    0.0, 0.0, 0.0, 1.0 /*<! Emissive color is emanated from the object and is unaffected by light sources. It adds no light to other objects in the scene */

#define SUMA_LMODEL_AMBIENT       1.0, 1.0, 1.0, 1.0 /*<! keep the ambient light high */

#define SUMA_CLEAR_COLOR_R         0.0 /*!< clear color (viewer background) Red */
#define SUMA_CLEAR_COLOR_G         0.0 /*!< clear color (viewer background) Green */
#define SUMA_CLEAR_COLOR_B         0.0 /*!< clear color (viewer background) Blue */
#define SUMA_CLEAR_COLOR_A         0.0 /*!< clear color (viewer background) Alpha */


#define SUMA_BACKFACE_CULL 0 /*<! 1/0 flag for culling backface facesets */
#define SUMA_CHECK_WINDING 0 /*<! 1/0 flag for checking triangle winding */

#define SUMA_LIGHT0_COLOR_INIT    1.0, 1.0, 1.0,  1.0 /*<! add some local light for shading */
#define SUMA_INTITIAL_LIGHT0_SWITCH 1 /*!< -1 works well for SureFit Surfaces, 1 works well for iv and FreeSurfer surfaces */
#define SUMA_STDERR stderr
#define SUMA_STDOUT stdout

#define SUMA_CROSS_HAIR_LINE_WIDTH 1.5
#define SUMA_CROSS_HAIR_RADIUS 6
#define SUMA_CROSS_HAIR_GAP 2
#define SUMA_CROSS_HAIR_SPHERE_RADIUS 0.5
#define SUMA_SELECTED_NODE_SPHERE_RADIUS 0.25

#define SUMA_XYZ_XFORM_BOXDIM_MM 5 /*!< search box width (in mm) used to change XYZ to the closest node index. Keep this one small, 5 mm works for me. Otherwise you may get thrown way off of where you should be. It is no guarantee that the closest node is part of the faceset you are looking at*/
#define SUMA_SELECTED_FACESET_LINE_WIDTH 2 /*!< Line Width of highlighting triangles */
#define SUMA_SELECTED_FACESET_OFFSET_FACTOR 0.01 /*!< highlighting is done by drawing two triangles at a fractional distance of the normal vector */
#define SUMA_SELECTED_FACESET_LINE_INTENSITY 0.75 /*!< line gray color intensity */
#define SUMA_NODE_ALPHA 1 /*!< Node Color Intensity 1, max intensity 0 min intensity*/
#define FOV_INITIAL 30
#define FOV_MIN 0.01
#define FOV_MAX 140
#define FOV_IN_FACT 1.05
#define FOV_OUT_FACT 0.95
#define MOUSE_ZOOM_FACT 30 /*!< The larger, the slower the gain on mouse movement */
#define TRANSLATE_GAIN 50 /*!< between 40 and 80 */
#define ARROW_TRANSLATE_DELTAX 30
#define ARROW_TRANSLATE_DELTAY 30
#define SUMA_MAX_MESSAGES 100 /*!< Maximum number of messages stored in list */
#define SUMA_MAX_MEMBER_FACE_SETS 60 /*!< Maximum number of facesets a node can be part of */
#define SUMA_MAX_FACESET_EDGE_NEIGHB 3 /*!< Maximum number of adjoining FaceSets a triangular faceset can have.*/
#define SUMA_MAX_DISPLAYABLE_OBJECTS 1000 /*!< Maximum number of displayable Objects */
#define SUMA_MAX_SURF_VIEWERS 6 /*!< Maximum number of surface viewers allowed */
#define SUMA_N_STANDARD_VIEWS 2/*!< Maximum number of standard views, see SUMA_STANDARD_VIEWS*/
#define SUMA_DEFAULT_VIEW_FROM 300 /*!< default view from location on Z axis */
#define SUMA_MAX_NAME_LENGTH 500   /*!< Maximum number of characters in a filename */
#define SUMA_MAX_DIR_LENGTH 2000    /*!< Maximum number of characters in a directory name */
#define SUMA_MAX_COMMAND_LENGTH      2000/*!< Maximum number of characters in a command string */
#define SUMA_MAX_LABEL_LENGTH 100 /*!< Maximum number of characters for labeling and naming suma fields and objects */
#define SUMA_IDCODE_LENGTH 50   /*!< Max. length of idcode_str of all suma objects */
#define SUMA_MAX_STRING_LENGTH 1000 /*!< Maximum number of characters in a string */ 
#define SUMA_MAX_NUMBER_NODE_NEIGHB   50 /*!< Maximum number of neighbors any one node can have */
#define SUMA_MAX_OVERLAYS 50 /*!< Maximum number of color overlay planes allowed */
#define SUMA_COMMAND_DELIMITER '|'
#define SUMA_COMMAND_TERMINATOR '~'
#define SUMA_PERSPECTIVE_NEAR   1.0   /*!< Z Near, distance from the viewer to the near clipping plane (for gluPerspective)*/
#define SUMA_PERSPECTIVE_FAR      900 /*!< Z Far, distance from the viewer to the far clipping plane (for gluPerspective)*/
#define SUMA_TESSCON_TO_MM       319.7 /*!< The mysterious Tesscon units */
#define SUMA_TESSCON_DIFF_FLAG    1000   /*!< If aMaxDim - aMinDim > SUMA_TESSCON_DIFF_FLAG in a .iv file, scaling by SUMA_TESSCON_TO_MM is applied */

#define SUMA_WriteCheckWait 400 /*!< Milliseconds to wait for each stream_writecheck call */ 
#define SUMA_WriteCheckWaitMax 2000 /*!< Milliseconds to try and establish a good WriteCheck */

#define SUMA_MAX_N_SURFACE_SPEC 20/*!< Maximum number of surfaces allowed in a spec file */

#define SUMA_MEMTRACE_BLOCK 10000 /*!< Number of elements to allocate for when keeping track of allocated memory. If needed more space is reallocated with SUMA_MEMTRACE_BLOCK increments. */
#define SUMA_MEMTRACE_FLAG 1    /*!< Flag to turn on(1) or off (0) the memory tracing capability */
#define SUMA_PI 3.14159 
#define SUMA_EPSILON 0.000001
/*!
   Debugging flags
*/
#define SUMA_NIML_WORKPROC_IO_NOTIFY 0  /*!< If set to 1 then SUMA_niml_workprocess will send a notification when InOut_Notify is ON
                                          You should keep it off unless you suspect a problem in that function. Otherwise
                                         you'll get many reports from the function making it difficult to see other messages. */
#define SUMA_WORKPROC_IO_NOTIFY 0  /*!< Same as above but for SUMA_workprocess */
                                    
typedef enum  { SUMA_FT_NOT_SPECIFIED, SUMA_FREE_SURFER, SUMA_SUREFIT, SUMA_INVENTOR_GENERIC, SUMA_PLY, SUMA_VEC } SUMA_SO_File_Type;
typedef enum { SUMA_FF_NOT_SPECIFIED, SUMA_ASCII, SUMA_BINARY, SUMA_BINARY_BE, SUMA_BINARY_LE } SUMA_SO_File_Format;
typedef enum { NOPE, YUP} SUMA_Boolean;
typedef enum {SO_type, AO_type, ROIdO_type, ROIO_type, GO_type, LS_type} SUMA_DO_Types;   /*!< Displayable Object Types 
                                                                                    S: surface, A: axis, G: grid, 
                                                                                    ROId: Region of interest drawn type,
                                                                                    LS_type: segment*/
typedef enum {SUMA_SCREEN, SUMA_LOCAL} SUMA_DO_CoordType; /*!< Coordinate system that Displayable object is attached to
                                                                  SCREEN is for a fixed system, LOCAL is for a mobile system,
                                                                  ie one that is rotated by the mouse movements */
typedef enum {SUMA_SOLID_LINE, SUMA_DASHED_LINE} SUMA_STIPPLE;

typedef enum {SUMA_Button_12_Motion, SUMA_Button_2_Shift_Motion, SUMA_Button_1_Motion, SUMA_Button_2_Motion} SUMA_MOTION_TYPES; /*!< Types of mouse motion */

typedef enum { SE_Empty, 
               SE_SetLookAt, SE_SetLookFrom, SE_Redisplay, SE_Home, SE_SetNodeColor, 
               SE_FlipLight0Pos, SE_GetNearestNode, SE_SetLookAtNode, SE_HighlightNodes, SE_SetRotMatrix, 
               SE_SetCrossHair, SE_ToggleCrossHair, SE_SetSelectedNode, SE_ToggleShowSelectedNode, SE_SetSelectedFaceSet,
               SE_ToggleShowSelectedFaceSet, SE_ToggleConnected, SE_SetAfniCrossHair, SE_SetAfniSurf, SE_SetForceAfniSurf, 
               SE_BindCrossHair, SE_ToggleForeground, SE_ToggleBackground, SE_FOVreset, SE_CloseStream4All, 
               SE_Redisplay_AllVisible, SE_RedisplayNow, SE_ResetOpenGLState, SE_LockCrossHair,
               SE_ToggleLockAllCrossHair, SE_SetLockAllCrossHair, SE_ToggleLockView, SE_ToggleLockAllViews, 
               SE_Load_Group, SE_Home_AllVisible, SE_Help, SE_Log, SE_UpdateLog, SE_SetRenderMode,
               SE_BadCode} SUMA_ENGINE_CODE; /* DO not forget to modify SUMA_CommandCode */
               
typedef enum { SEF_Empty, 
               SEF_fm, SEF_im, SEF_fv3, SEF_iv3, SEF_fv15, 
               SEF_iv15, SEF_i, SEF_f, SEF_s, SEF_vp, 
               SEF_cp, SEF_fp, SEF_ip, 
               SEF_BadCode} SUMA_ENGINE_FIELD_CODE; 
               
typedef enum { SES_Empty,
               SES_Afni,  /*!< command from Afni directly which practically means that Srcp in EngineData is not SUMA_SurfaceViewer * . In the future, some Afni related pointer might get passed here. */
               SES_Suma,  /*!< command from Suma, which means that Srcp is a SUMA_SurfaceViewer * to the viewer making the command. */
               SES_SumaWidget,  /*!< command from a widget in Suma. Usually means, do not try to update widget ... */
               SES_SumaFromAfni,   /*!< command from Suma in response to a request from Afni. Srcp is still a SUMA_SurfaceViewer * but Afni, havin initiated the command should not receive the command back from Suma. Think cyclical cross hair setting... */
               SES_SumaFromAny,  /*!< Same concept as SES_SumaFromAfni but from generic program. */
               SES_Unknown} SUMA_ENGINE_SOURCE;
               
typedef enum { SEI_WTSDS,  
               SEI_Head, SEI_Tail, SEI_Before, SEI_After, SEI_In,
               SEI_BadLoc } SUMA_ENGINE_INSERT_LOCATION;
               
typedef enum { SUMA_int, SUMA_float } SUMA_VARTYPE;

typedef enum { SUMA_CMAP_UNDEFINED, SUMA_CMAP_RGYBR20,  SUMA_CMAP_nGRAY20,
               SUMA_CMAP_GRAY20, SUMA_CMAP_BW20, SUMA_CMAP_BGYR19, 
               SUMA_CMAP_MATLAB_DEF_BGYR64} SUMA_STANDARD_CMAP; /*!< Names of standard colormaps. RGYBR20 reads Red, Green, Yellow, Blue, Red, 20 colors total */

typedef enum { SUMA_ROI_InCreation, SUMA_ROI_Finished, SUMA_ROI_InEdit} SUMA_ROI_DRAWING_STATUS;

typedef enum { SUMA_ROI_ClosedPath} SUMA_ROI_DRAWING_TYPE;  /* an ROI created by drawing */

typedef enum { SUMA_ROI_NodeGroup, SUMA_ROI_EdgeGroup, SUMA_ROI_FaceGroup } SUMA_ROI_TYPE; /* a generic ROI */

typedef enum { SXR_default, SXR_NP, SXR_Afni , SXR_Bonaire} SUMA_XRESOURCES;   /* flags for different X resources */

typedef enum { SRM_ViewerDefault, SRM_Fill, SRM_Line, SRM_Points , SRM_N_RenderModes} SUMA_RENDER_MODES; /*!< flags for various rendering modes */

#define SUMA_N_STANDARD_VIEWS  2 /*!< number of useful views enumerated in SUMA_STANDARD_VIEWS */
typedef enum {   SUMA_2D_Z0, SUMA_3D, SUMA_Dunno} SUMA_STANDARD_VIEWS; /*!< Standard viewing modes. These are used to decide what viewing parameters to carry on when switching states 
                                                                  SUMA_2D_Z0 2D views, with Z = 0 good for flat surfaces
                                                                  SUMA_3D standard 3D view
                                                                  SUMA_Dunno used to flag errors leave this at the end 
                                                                  Keep in sync with SUMA_N_STANDARD_VIEWS*/
typedef enum {   SUMA_No_Lock, SUMA_I_Lock, SUMA_XYZ_Lock, SUMA_N_Lock_Types}  SUMA_LINK_TYPES; /*!< types of viewer linking. Keep SUMA_N_Lock_Types at the end, it is used to keep track of the number of types*/
                                                                 
/*! structure to keep track of allocate memory */
typedef struct {
   void **Pointers; /*!< vector of pointers for which memory was allocated */
   int *Size; /*!< vector of sizes of allocated memory blocks. Pointers[i] has Size[i] bytes allocated for it */
   int N_alloc; /*!< number of meaningful entries in Pointers and Size */
   int N_MaxPointers; /*!< Maximum number of elements allocated for in Pointers and Size */
} SUMA_MEMTRACE_STRUCT;

/*! structure containing a data block information */
typedef struct {
   void *data;   /*!< pointer to data location */
   int N_link; /*!< number of links to data location */
   char ParentIDcode[SUMA_IDCODE_LENGTH]; /* IDcode of the creator of data */
} SUMA_INODE;

/*! Structure containing a color map */
typedef struct {
   float ** M; /*!< N_Col x 3 matrix of R G B values (0..1) */
   int N_Col; /*!< number of colors in the color map */
   char *Name; /*!< Name of colormap */
} SUMA_COLOR_MAP;

/*! Structure containing one color overlay */
typedef struct {
   SUMA_Boolean Show; /*!< if YUP then this overlay enters the composite color map */
   char Name[SUMA_MAX_LABEL_LENGTH]; /*!< name of ovelay, CONVEXITY or Functional or areal boundaries perhaps*/
   int *NodeDef; /*!< nodes overwhich the colors are defined*/
   int N_NodeDef; /*!< total number of nodes specified in NodeDef*/
   int N_Alloc; /*!< You'd think this should be equal to NodeDef, but in instances where you may be receiving
             varying numbers of colors to the same plane, it's a pane to have to free and realloc space.
             So, while the juice is only up to N_NodeDef, the allocation is for N_Alloc */
   float **ColMat; /*!< N_NodeDef x 3 matrix containing colors of nodes specified in NodeDef */
   float GlobalOpacity; /*!< Opacity factor between 0 and 1 to apply to all values in ColMat */
   float *LocalOpacity; /*!< Opacity factor vector between 0 and 1 to apply to each individual node color */
   int PlaneOrder; /*!< Order of the overlay plane, 1st plane is 0 and is farthest away from the top */  
   SUMA_Boolean BrightMod; /*!< if YUP then colors overlaid on top of this plane have their brightness modulated by the average intensity of the colors in that plane see the function SUMA_Overlays_2_GLCOLAR4 for details*/  
} SUMA_OVERLAYS;

/*! a structure holding the options for the function SUMA_ScaleToMap 
\sa SUMA_ScaleToMapOptInit to allocate and initialize such a structure 
to free this structure use the free function
*/
typedef struct {
   SUMA_Boolean ApplyMask; /*!< if YUP then values that fall in MaskRange are assigned the color in MaskColor */
   float MaskRange[2]; /*!< values between MaskRange[0] and MaskRange[1] (inclusive) are assigned MaskColor */
   float MaskColor[3]; /*!< color to assign to masked nodes */
   SUMA_Boolean ApplyClip; /*!< if YUP then values that range clipping using Range is applied */
   float ClipRange[2]; /*!< nodes with values <= Range[0] are given the first color in the color map, values >= Range[1] get the last color in the map */
   float BrightFact; /*!< a brightness factor to apply to the color map. This factor is applied to the colors in the colormap and the mask colors*/
} SUMA_SCALE_TO_MAP_OPT;

/*! structure containing the color mapping of a vector */
typedef struct {
   float **cM; /*!< N_Node x 3 matrix containing the colors at each node*/
   int N_Node; /*!< obvious */
   SUMA_Boolean *isMasked; /*!< if isMasked[i] then node i has a mask color associated with it */ 
} SUMA_COLOR_SCALED_VECT;

/*! 
Stucture to hold the contents of the specs file 
*/
typedef struct {
   char SurfaceType[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_LABEL_LENGTH];
   char SurfaceFormat[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_LABEL_LENGTH]; 
   char SureFitTopo[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_NAME_LENGTH];
   char SureFitCoord[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_NAME_LENGTH];
   char MappingRef[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_NAME_LENGTH];
   char SureFitVolParam[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_NAME_LENGTH];
   char FreeSurferSurface[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_NAME_LENGTH];
   char InventorSurface[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_NAME_LENGTH];
   char VolParName[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_NAME_LENGTH]; 
   char *IDcode[SUMA_MAX_N_SURFACE_SPEC];
   char State[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_LABEL_LENGTH];
   char Group[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_NAME_LENGTH];
   char SurfaceLabel[SUMA_MAX_N_SURFACE_SPEC][SUMA_MAX_NAME_LENGTH];
   int EmbedDim[SUMA_MAX_N_SURFACE_SPEC];
   int N_Surfs;
   int N_States;
   int N_Groups;
   char StateList[SUMA_MAX_N_SURFACE_SPEC*100];
   char SpecFilePath[SUMA_MAX_DIR_LENGTH];
} SUMA_SurfSpecFile;

/*! structure that containing node's first order neighbors */
typedef struct {
   int N_Node; /*!< Number of nodes whose neighbors are listed in this structure */
   int *NodeId; /*!< Id of each node whose neighbors are listed in this structure */
   int **FirstNeighb; /*!< N_Node x N_Neighb_max matrix with each row specifying the indices of neighboring nodes.
                        After Tue Jan  7 18:13:44 EST 2003: The nodes are now ordered to form a path on the surface.
                        Note: There is no guarantee that the path is closed. */
   int *N_Neighb; /*!< maximum number of neighbors for a particular node */
   int N_Neighb_max; /*!< maximum number of neighbors of all nodes */
} SUMA_NODE_FIRST_NEIGHB;

/*! structure that contains faceset's first order neighbors */
typedef struct {
   int N_FaceSet; /*!< Number of nodes whos neighbors are listed in this structure */
   int **FirstNeighb; /*!< N_Node x N_Neighb_max matrix with each row specifying the indices of neighboring facesets */
   int *N_Neighb; /*!< maximum number of neighbors for a particular faceset */
   int N_Neighb_max; /*!< maximum number of neighbors of all facesets */
   int N_Neighb_min; /*!< minimum number of neighbors of all facesets */
} SUMA_FACESET_FIRST_EDGE_NEIGHB;

/*!
   structure containing surface curvature parameters
*/
typedef struct {
   int N_Node; /*!< Number of nodes in the surface */
   float **T1; /*!< N_Node x 3 matrix with each row specifying the 1st principal direction of the surface */
   float **T2; /*!< N_Node x 3 matrix with each row specifying the 2nd principal direction of the surface */
   float *Kp1; /*!< N_Node x 1 vector with each row specifying the curvature along the 1st principal direction */
   float *Kp2; /*!< N_Node x 1 vector with each row specifying the curvature along the 2nd principal direction */
   int N_SkipNode; /*!< number of nodes for which the curvature could not be computed */
} SUMA_SURFACE_CURVATURE;


/*! 
   structure containing the edges that make up a triangular faceset list

*/
typedef struct {
   int ** EL; /*!< pointer to where the Edge List ( N_EL x 2 ) will be placed
                        each row is an edge, i1 i2 where i1 is always <= i2
                        EL is sorted by row */
   int ** ELps; /*!< pointer to where the Edge List Property matrix ( N_EL x 2 )will be placed 
                        1st column, row i = 1 means edge i: i1,i2 was encountered as i2->i1 in the triangle J (so it was flipped when stored in EL)
                                          = -1 means edge i: i1,i2 was encountered as i1->i2 in the triangle J (so no flipping was done to store it in EL)
                        2nd column, row i = J is the triangle ( FL[J] ) that the segment belongs to. 
                        3rd column, row i = Numer of triangles that contain this edge. This number is positive for the first occurence
                        of the edge in EL, it is -1 afterwards. A decent edge has 2 hosting triangles, an edge edge
                        has 1 hosting triangle. Bad edges come in all other colors*/
                        
   int *ELloc; /*!< k x 1 vector that stores where each node's listing begins. 
                     ELloc is used to quickly find a certain edge in EL
                     to find the edge formed by nodes na-nb
                     find the minimum of na and nb (say it's nb)
                     the first reference of an edge containing nb starts at EL(ELloc(nb),:)
                     NOTE: ELloc contains an entry for each node in FaceSetList, except the 
                     largest node index since that's never in the 
                     first column of EL */
                     
   int N_EL; /*!< Number of segments = 3 * N_Facesets */
   int max_N_Hosts; /*!< Maximum number of triangle hosts any one edge has (max ( ELps(:,2) != -1 ) )*/
   int  min_N_Hosts; /*!< Minimum version of max_N_Hosts */
   
   int **Tri_limb; /*!< each row j of Tri_limb contains the indices into EL (and ELps) of the edges that make it up */
   float *Le; /*!< Vector N_EL elements long containing the length of each edge in EL */
   
} SUMA_EDGE_LIST;

/*! structure that contains array pointers from function SUMA_isinbox */
#define SUMA_isinbox_struct
typedef struct {
   int *IsIn; /*!< Indices of nodes inside the box */
   int nIsIn; /*!< Number of nodes inside the box */
   float *d; /*!< Distance of each node to the center of the box */
   float **dXYZ; /*!< Not implemented */
} SUMA_ISINBOX;

/*! structure that contains array pointers from function isinsphere */
#define SUMA_isinsphere_struct
typedef struct {
   int *IsIn;  /*!< Indices of nodes inside the sphere */
   int nIsIn; /*!< Number of nodes inside the sphere */
   float *d;  /*!< Not implemented Distance of each node to the center of the shpere */
   float **dXYZ; /*!< Not implemented */
} SUMA_ISINSPHERE;

/*! Displayable Object Type */
typedef struct {
   void *OP;   /*<! Object Pointer */
   SUMA_DO_Types ObjectType; /*!< Type of displayable object */
   SUMA_DO_CoordType CoordType; /*!< Type of coordinate system that the object is attached to
                                    This is used to determine whether the object is drawn before or 
                                    or after the shift and rotation matrices are applied */
} SUMA_DO;

/*! string stucture 
*/
typedef struct {
   int N_alloc;  /*!< space allocated for s */
   char *s; /*!< string s */
} SUMA_STRING;

/*! structure containing widgets for surface viewer controllers ViewCont */
typedef struct {
   Widget TopLevelShell;/*!< Top level shell for a viewer's controller */
}SUMA_X_ViewCont;

typedef struct {
   Widget toplevel;  /*!< toplevel widget of the text display window */
   Widget text_w; /*!<  text widget containing string to be displayed */
   Widget search_w;  /*!< widget of string search field */
   Widget text_output;  /*!< widget of search result field */
   SUMA_Boolean case_sensitive;  /*!< Case sensitive widget search */
   SUMA_Boolean allow_edit; /*!< allow editing of text displayed*/
   void (*OpenCallBack)(void *data); /*!< call back performed when SUMA_CreateTextShell is entered */
   void * OpenData;  /*!< data sent along with OpenCallBack */
   void (*DestroyCallBack)(void *data);   /*!< call back performed when SUMA_DestroyTextShell is entered */
   void * DestroyData; /*!< data sent along with DestroyCallBack */
} SUMA_CREATE_TEXT_SHELL_STRUCT; /*!< structure containing options and widgets for the text shell window */


/*! structure containing widgets for surface  controllers SurfCont */
typedef struct {
   Widget TopLevelShell;/*!< Top level shell for a Surface's controller */
   Widget SurfInfo_pb; /*!< More info push button */
   SUMA_CREATE_TEXT_SHELL_STRUCT * SurfInfo_TextShell; /*!< structure containing widgets and options of the surface info text shell */
   Widget RenderModeMenu[SRM_N_RenderModes]; /*!< vector of widgets controlling the rendering mode menu */
}SUMA_X_SurfCont;

typedef struct {
   int N_rb_group; /*!< number of radio buttons in group */
   int N_but; /*!< number of buttons per radio button group */
   Widget *tb; /*!< vector of N_rb_group * N_but toggle button widgets */
   Widget *rb; /*!< vetor of N_rb_group radio box widget */
   Widget arb; /*!< widget of radiobox for all lock buttons */
   Widget *atb; /*!< widget of toggle buttons in arb */
}SUMA_rb_group;

/*! structure containing widgets for Suma's controller SumaCont */
typedef struct {
   Widget AppShell; /*!< AppShell widget for Suma's controller */
   Widget quit_pb; /*!< quit push button */
   SUMA_Boolean quit_first;   /*!< flag indicating first press of done button */
   SUMA_rb_group *Lock_rbg; /*!< pointer to structure contining N radio button groups */
   Widget *LockView_tbg;   /*!< vector of toggleview buttons */
   Widget LockAllView_tb;  /*!< widget of toggleAllview button */
}SUMA_X_SumaCont;

typedef enum { SW_File, 
               SW_FileOpen, SW_FileOpenSpec, SW_FileOpenSurf, SW_FileClose, 
               SW_N_File } SUMA_WIDGET_INDEX_FILE; /*!< Indices to widgets under File menu. 
                                                      Make sure you begin with SW_File and end
                                                      with SW_N_File */
typedef enum { SW_View, 
               SW_ViewSumaCont, SW_ViewSurfCont, SW_ViewViewCont, 
               SW_ViewSep1,
               SW_ViewCrossHair, SW_ViewNodeInFocus, SW_ViewSelectedFaceset,
               SW_N_View } SUMA_WIDGET_INDEX_VIEW; /*!< Indices to widgets under View menu. 
                                                      Make sure you begin with SW_View and end
                                                      with SW_N_View */
typedef enum { SW_Help, 
               SW_HelpViewer,  SW_HelpMessageLog, SW_HelpSep1, SW_HelpIONotify,
               SW_HelpMemTrace,  
               SW_N_Help } SUMA_WIDGET_INDEX_HELP; /*!< Indices to widgets under Help menu.
                                                         Make sure you begin with SW_View and end
                                                         with SW_N_View */                                                   
typedef enum { SW_SurfCont_Render,
               SW_SurfCont_RenderViewerDefault, SW_SurfCont_RenderFill, SW_SurfCont_RenderLine, SW_SurfCont_RenderPoints, 
               SW_N_SurfCont_Render } SUMA_WIDGET_INDEX_SURFCONT_RENDER; /*!< Indices to widgets in SurfaceController under
                                                                           RenderMode */
               
               
/*! structure containg X vars for surface viewers*/
typedef struct {
   Display *DPY; /*!< display of toplevel widget */
   Widget TOPLEVEL, FORM, FRAME, GLXAREA;
   XVisualInfo *VISINFO;
   GLXContext GLXCONTEXT;
   Colormap CMAP;
   Bool DOUBLEBUFFER;
   int REDISPLAYPENDING;
   int WIDTH, HEIGHT;
   XtWorkProcId REDISPLAYID, MOMENTUMID;
   
   SUMA_X_ViewCont *ViewCont; /*!< pointer to structure containing viewer controller widget structure */
   Widget ToggleCrossHair_View_tglbtn; /*!< OBSOLETE Toggle button in View-> menu */
   Widget FileMenu[SW_N_File]; /*!< Vector of widgets under File Menu */       
   Widget ViewMenu[SW_N_View]; /*!< Vector of widgets under View Menu */
   Widget HelpMenu[SW_N_Help]; /*!< Vector of widgets under Help Menu */
}SUMA_X;

/*! structure containg X vars common to all viewers */
typedef struct {
   SUMA_X_SumaCont *SumaCont; /*!< structure containing widgets for Suma's controller */
   XtAppContext App; /*!< Application Context for SUMA */
   Display *DPY_controller1; /*!< Display of 1st controller's top level shell */
   SUMA_XRESOURCES X_Resources; /*!< flag specifying the types of resources to use */
   SUMA_CREATE_TEXT_SHELL_STRUCT *Help_TextShell; /*!< structure containing widgets and options of SUMA_help window */
   SUMA_CREATE_TEXT_SHELL_STRUCT *Log_TextShell; /*!<  structure containing widgets and options of SUMA_log window */
}SUMA_X_AllView;

/*! filename and path */
typedef struct {
   char *Path;
   char *FileName;
}SUMA_FileName;

/*! filename, extension and path */
typedef struct {
   char *Path;
   char *FileName;
   char *FileName_NoExt;
   char *Ext;
}SUMA_PARSED_NAME;


/*! structure defining a cross hair */
typedef struct {   
   GLfloat XaxisColor[4] ;
   GLfloat YaxisColor[4] ;
   GLfloat ZaxisColor[4] ;
   
   GLfloat LineWidth;
   SUMA_STIPPLE Stipple; /*!< dashed or solid line */

   GLfloat c[3]; /*!< Cross Hair center */
   GLfloat r; /*!< Cross Hair radius */
   GLfloat g; /*!< 1/2 of gap between center and ray (should be less than radius/2) */
   
   SUMA_Boolean ShowSphere; /*!< YUP/NOPE, starting to regret this. */
   GLUquadricObj *sphobj; /*!< quadric object, representing central sphere */
   GLfloat sphcol[4]; /*!< Sphere color */
   GLdouble sphrad; /*!< Sphere radius */
   GLint slices; /*!< think pizza */
   GLint stacks; /*!< think lattitudes */
   
   int SurfaceID; /*!< If the cross hair is tied to a surface, SurfaceID contains the index into SUMAg_DOv of that surface. -1 if that cross hair is wild and loose */
   int NodeID; /*!< a node from SurfaceID can be associated with the cross hair (-1 for nothing) */
}SUMA_CrossHair;   

typedef struct {      
   GLUquadricObj *sphobj; /*!< quadric object, representing central sphere */
   GLfloat sphcol[4]; /*!< Sphere color */
   GLdouble sphrad; /*!< Sphere radius */
   GLint slices; /*!< think pizza */
   GLint stacks; /*!< think lattitudes */
   GLfloat c[3]; /*!< center of Sphere Marker */
}SUMA_SphereMarker;   

typedef struct {
   GLfloat n0[3]; /*!< Node 1 XYZ*/
   GLfloat n1[3]; /*!< Node 2 XYZ*/
   GLfloat n2[3]; /*!< Node 3 XYZ*/
   GLfloat LineWidth; /*!< LineWidth of Edge*/
   GLfloat LineCol[4]; /*!< LineColor of Edge*/
   GLfloat NormVect[3]; /*!< normal vector of faceset, two triangles are drawn at a small distance from the selected FaceSet */
}SUMA_FaceSetMarker;

/*!
   Structure containg a bunch of segments defined between n0 and n1
*/
typedef struct {
   char *idcode_str;    /*!< unique idcode for DO */
   char *Label; /*!< ascii label for DO */ 

   GLfloat *n0; /*!< vector containing XYZ of nodes 1 (3*N_n elements long)*/
   GLfloat *n1; /*!< vector containing XYZ of nodes 2 (3*N_n elements long)*/
   int N_n; /*!< Number of elements in n0 and n1 */
   GLfloat LineWidth; /*!< LineWidth of all segment*/
   GLfloat LineCol[4]; /*!< LineColor of all segments*/
   SUMA_STIPPLE Stipple; /*!< dashed or solid line */
}SUMA_SegmentDO;

/*! Structure containing the communication info and status with AFNI */
typedef struct {
   SUMA_Boolean Connected;   /*!< flag indicating connection state */
   int ConSock;
   
} SUMA_AfniCom;

/* structure defining the former state of a surface viewer window */
typedef struct {
   int N_DO;      /*!< Total number of surface objects registered with the viewer */
   int *ShowDO;    /*!< ShowSO[i] (i=0..N_DO) contains Object indices into DOv for DOs visible in the surface viewer*/
   float ViewFrom[3]; /*!< Location of observer's eyes */
   float ViewFromOrig[3]; /*!< Original Location of observer's eyes */
   float ViewCenter[3];   /*!< Center of observer's gaze */
   float ViewCenterOrig[3];   /*!< Original Center of observer's gaze */
   float ViewCamUp[3];   /*!< Camera Up direction vector */
   float ViewDistance; /*!< Viewing distance */
   float FOV; /*!< Field of View (affects zoom level)*/
   float Aspect;   /*!< Aspect ratio of the viewer*/
} SUMA_ViewState_Hist;


/*! structure defining the viewing state of the viewer window */
typedef struct {
   char *Name; /*!< The name of the viewing state, fiducial, inflated, etc .. */
   int *MembSOs; /*!< Indices into DOv of SOs that are members of the viewing state */
   int N_MembSOs; /*!< Number of members in MembSOs. Only SOs that are in MembSOs can
                     be placed into ShowDO of the viewer in a particular viewing state.*/                  
   SUMA_ViewState_Hist *Hist; /*!< Pointer to structure containing various parameter settings for that viewing state */            
} SUMA_ViewState;

/*! structure containing the geometric settings for viewing the surface */
typedef struct {
   float ViewFrom[3]; /*!< Location of observer's eyes */
   float ViewFromOrig[3]; /*!< Original Location of observer's eyes */
   float ViewCenter[3];   /*!< Center of observer's gaze */
   float ViewCenterOrig[3];   /*!< Original Center of observer's gaze */
   float ViewCamUp[3];   /*!< Camera Up direction vector */
   float ViewDistance; /*!< Viewing distance */
   
   int translateBeginX; /*!< User Input (mouse) X axis current position for translation */
   int translateBeginY; /*!< User Input (mouse) Y axis current position for translation */
   float translateDeltaX;   /*!< User Input (mouse) X axis position increment for translation */
   float translateDeltaY;   /*!< User Input (mouse) Y axis position increment for translation */
   float TranslateGain;   /*!< gain applied to mouse movement */
   float ArrowtranslateDeltaX;   /*!< User Input (Keyboard) X axis position increment for translation */
   float ArrowtranslateDeltaY;   /*!< User Input (Keyboard) X axis position increment for translation */
   GLfloat translateVec[2];      /*!< translation vector, in screen coordinates, equal to [translateDeltaX translateDeltaY]. The third translation (Z dimension) is 0.0*/
   GLfloat RotaCenter[3];   /*!<Center of Rotation */
   float zoomDelta;       /*!< Zoom increment */
   float zoomBegin;    /*!< Current zoom level*/
   int spinDeltaX;            /*!< User Input (mouse) X axis position increment for spinning*/
   int spinDeltaY;            /*!< User Input (mouse) Y axis position increment for spinning*/
   int spinBeginX;            /*!< User Input (mouse) X axis current position for spinning */
   int spinBeginY;            /*!< User Input (mouse) Y axis current position for spinning */
   int MinIdleDelta;       /*!< minimum spinDeltaX or spinDeltaY to initiate momentum rotation */
   float deltaQuat[4];   /*!< Quaternion increment */
   float currentQuat[4]; /*!< Current quaternion */
   Boolean ApplyMomentum;   /*<! Turn momentum ON/OFF */
} SUMA_GEOMVIEW_STRUCT;

/*! structure holding the pointer the node color assignment and a bit more */
typedef struct {
   GLfloat *glar_ColorList; /*!< pointer to the 1D ColorList array */
   int N_glar_ColorList; /*!< Number of elements in glar_ColorList 4xNumber of nodes in the surface */
   char *idcode_str; /*!< string containing the idcode of the surface to which glar_ColorList belongs*/
   SUMA_Boolean Remix; /*!< flag indicating that colors need to be remixed */ 
} SUMA_COLORLIST_STRUCT;

/*! structure defining the state of a viewer window */
typedef struct {
   int N_DO;      /*!< Total number of surface objects registered with the viewer */
   int *ShowDO;    /*!< ShowDO[i] (i=0..N_DO) contains Object indices into DOv for DOs visible in the surface viewer*/
   
   SUMA_COLORLIST_STRUCT *ColList; /*!< pointer to structures containing NodeColorLists for surfaces listed in ShowDO */
   int N_ColList; /*!< Number of structures in ColList */
   
   SUMA_STANDARD_VIEWS StdView; /*!< viewing mode, for 2D or 3D */
   SUMA_GEOMVIEW_STRUCT *GVS; /*! pointer to structures containing geometric viewing settings */
   int N_GVS; /*!< Number of different geometric viewing structures */

   short verbose;   /*!< Verbosity of viewer */

   SUMA_X *X; /*!< structure containing X widget midgets */

   float Aspect;   /*!< Aspect ratio of the viewer*/
   int WindWidth;   /*!< Width of window */
   int WindHeight;   /*!< Height of window */
   float *FOV; /*!< Field of View (affects zoom level, there is a separate FOV for each ViewState)*/
   
   SUMA_Boolean BF_Cull; /*!< flag for backface culling */
   SUMA_RENDER_MODES PolyMode; /*!< polygon viewing mode, SRM_Fill, SRM_Line, SRM_Points
                                    There is a similar field for each surface object to 
                                    allow independent control for each surface. If the rendering mode
                                    is specified for a certain surface, it takes precedence over the
                                    one specified here*/

   float Back_Modfact; /*!< Factor to apply when modulating foreground color with background intensity
                           background does not modulate foreground, 
                           Color = Fore * avg_Bright * AttenFactor; (w/ 0 <= avg_Bright <=1)
                           a good setting is such that SUMA_BACKGROUND_ATTENUATION_FACTOR * SUMA_DIM_AFNI_COLOR_FACTOR = 1
                            Watch for saturation effects!  */

   GLfloat light0_position[4]; /*!< Light 0 position: 1st 3 vals --> direction of light . Last value is 0 -->  directional light*/
   GLfloat light1_position[4]; /*!< Light 1 position: 1st 3 vals --> direction of light. Last value is 0 -->  directional light*/
   
   GLfloat clear_color[4]; /*!< viewer background color */
      
   SUMA_Boolean Open; /*! Viewer visible to the human eye */
   int ShowEyeAxis ; /*!< ShowEyeAxis */
   int ShowMeshAxis; /*!< ShowEyeAxis */
   int ShowCrossHair; /*!< ShowCrossHair */
   SUMA_Boolean ShowForeground;    /*!< Flag for showing/not showing foreground colors */
   SUMA_Boolean ShowBackground; /*!< Flag for showing/not showing background colors */   
   
   
   int Focus_SO_ID; /*!< index into SUMAg_DOv of the surface currently in focus, -1 for nothing*/
   int Focus_DO_ID; /*!< index into SUMAg_DOv of the Displayabl Object currently in focus -1 for nothing*/
   
   GLdouble Pick0[3];   /*!< Click location in World coordinates, at z = 0 (near clip plane)*/
   GLdouble Pick1[3];   /*!< Click location in World coordinates, at z = 1.0 (far clip plane)*/
   
   SUMA_CrossHair *Ch; /*!< Pointer to Cross Hair structure */
      
   SUMA_ViewState *VSv; /*!< Vector of Viewing State Structures */
   int N_VSv; /*!< Number of Viewing State structures */
   char *State; /*!< The current state of the viewer. This variable should no be freed since it points to locations within VSv*/
   int iState; /*!< index into VSv corresponding to State */
   int LastNonMapStateID; /*!< Index into the state in VSv from which a toggle to the mappable state was initiated */ 
   
   SUMA_Boolean isShaded; /*!< YUP if the window is minimized or shaded, NOPE if you can see its contents */

   SUMA_Boolean LinkAfniCrossHair; /*!< YUP if the cross hair location is to be sent (and accepted from AFNI, when the stream is open) */
   SUMA_Boolean ResetGLStateVariables; /*!< YUP if you need to run the function that resets the Eye Axis before display. 
                                          see functions SUMA_display and SUMA_OpenGLStateReset for more info */
}SUMA_SurfaceViewer;

/*! structure defining an EngineData structure */
typedef struct {
   SUMA_ENGINE_CODE CommandCode; /*!< Code of command to be executed by SUMA_Engine function, 
                                    this is the same as the _Dest fields for each variable type.
                                    However, the _Dest fields are left as a way to make sure that
                                    the user has correctly initialized EngineData for a certain command.*/
   
   void *Srcp; /*!< Pointer to data structure of the calling source, typically, a typecast version of SUMA_SurfaceViewer * */
   SUMA_ENGINE_SOURCE Src; /*!< Source of command. This replaces the _Source fields in the older version of the structure */
   
   float fv3[3]; /*!< Float vector, 3 values */
   SUMA_ENGINE_CODE fv3_Dest; /*!<  float3 vector destination */
   SUMA_ENGINE_SOURCE fv3_Source; /*!< OBSOLETE float3 vector source */
   
   int iv3[3];      /*!< Integer vector, 3 values */
   SUMA_ENGINE_CODE iv3_Dest;  /*!<  Integer3 vector destination */
   SUMA_ENGINE_SOURCE iv3_Source;  /*!<OBSOLETE  Integer3 vector source */
   
   float fv15[15]; /*!< Float vector, 15 values */
   SUMA_ENGINE_CODE fv15_Dest; /*!<  float15 vector destination */
   SUMA_ENGINE_SOURCE fv15_Source; /*!< OBSOLETE float15 vector source */
   
   int iv15[15];/*!< Integer vector, 15 values */
   SUMA_ENGINE_CODE iv15_Dest;/*!<  Integer15 vector destination */
   SUMA_ENGINE_SOURCE iv15_Source; /*!< OBSOLETE Integer15 vector source */
   
   int i;      /*!< integer */
   SUMA_ENGINE_CODE i_Dest;   /*!<  integer destination */
   SUMA_ENGINE_SOURCE i_Source; /*!< OBSOLETE integer source */
   
   float f; /*!< float, ingenious ain't it! */
   SUMA_ENGINE_CODE f_Dest; /*!<  float destination */
   SUMA_ENGINE_SOURCE f_Source; /*!< OBSOLETE float source */
   
   char s[SUMA_MAX_STRING_LENGTH]; /*!< string */
   SUMA_ENGINE_CODE s_Dest; /*!<  string destination */
   SUMA_ENGINE_SOURCE s_Source; /*!< OBSOLETE string source */
   
   int *ip; /*!< integer pointer */
   SUMA_ENGINE_CODE ip_Dest; /*!<  integer pointer destination */
   
   float *fp; /*!< float pointer */
   SUMA_ENGINE_CODE fp_Dest; /*!< float pointer destination */
   
   char *cp; /*!< char pointer */
   SUMA_ENGINE_CODE cp_Dest; /*!< character pointer destination */
   
   float **fm; /*!< float matrix pointer */
   SUMA_Boolean fm_LocalAlloc; /*!< Locally allocated matrix pointer ? (if it is then it is freed in SUMA_ReleaseEngineData ) */
   SUMA_ENGINE_CODE fm_Dest; /*!<  destination of fm */
   SUMA_ENGINE_SOURCE fm_Source; /*!< OBSOLETE source of fm*/
   
   int **im; /*!< Same dance as fm but for integers */
   SUMA_Boolean im_LocalAlloc;
   SUMA_ENGINE_CODE im_Dest; /*!<  destination of im */
   SUMA_ENGINE_SOURCE im_Source; /*!< OBSOLETE source of im */
   
   void *vp; /*!< pointer to void */
   SUMA_ENGINE_CODE vp_Dest; /*!<  destination of fm */
   SUMA_ENGINE_SOURCE vp_Source; /*!< OBSOLETE source of fm*/
   
   int N_rows; /*!< Number of rows in fm or im */
   int N_cols; /*!< Number of colums in fm or im */
   
} SUMA_EngineData;

 
/*! structure defining an axis object */
typedef struct {
   GLfloat XaxisColor[4] ;
   GLfloat YaxisColor[4] ;
   GLfloat ZaxisColor[4] ;
   
   GLfloat LineWidth;
   SUMA_STIPPLE Stipple; /*!< dashed or solid line */
   
   GLfloat XYZspan[3]; /*!< the axis will span +/- span[i] in the three dimensions */
   GLfloat Center[3]; /*!< origin of axis */
   char *Name; /*!< name of axis */
   char *idcode_str; /*! idcode of axis */
}SUMA_Axis;


/*! structure that contains the output of SurfNorm function */
#define SUMA_SurfNorm_struct
typedef struct {
   int N_Node; /*!< Number of nodes, 1st dim of NodeNormList*/
   int N_Face;/*!< Number of facesets, 1st dim of FaceNormList*/
   float *FaceNormList ; /*!< N_Face x 3 vector (was matrix prior to SUMA 1.2) containing normalized normal vectors for each triangular faceset*/ 
   float *NodeNormList ; /*!< N_Node x 3 vector (was matrix prior to SUMA 1.2) containing normalized normal vectors for each node*/
} SUMA_SURF_NORM; /*!< structure that contains the output of SurfNorm function */

/*! structure that contains the output of SUMA_MemberFaceSets function */
#define SUMA_MemberFaceSets_struct
typedef struct {
   int N_Memb_max;/*!< Maximum number of Facesets any node belonged to*/
   int Nnode; /*! Total number of nodes examined (0..Nnode-1) */
   int **NodeMemberOfFaceSet ; /*!< Nnode x N_Memb_max matrix containing for each row i, the indices of the facesets containing node i*/ 
   int *N_Memb ; /*!< Nnode x 1 vetor containing for each node i, the number of facesets that contain it*/
} SUMA_MEMBER_FACE_SETS; /*!< structure that contains the output of SurfNorm function */


/*! structure containing results of intersection of a ray with triangles */
typedef struct {
   int N_el; /*!< Number of elements in each vector */
   SUMA_Boolean *isHit;   /*!< Is the triangle hit ? */
   float *t;   /*!< SIGNED Distance from ray source to triangle */
   float *u;   /*!< location of intersection in triangle in Barycentric coordinates, V0P = u V0V1 + vV0V2*/
   float *v;   /*!< location of intersection in triangle */
   int ifacemin; /*!< index of the faceset closest (NOT SIGNED, abs(t)) to the ray's origin */
   int ifacemax; /*!< index of the faceset farthest (NOT SIGNED, abs(t)) from the ray's origin */
   int N_hits; /*!< Number of intersections between ray and surface */
   float P[3]; /*!< XYZ of intersection with ifacemin */
   float d; /*!< Distance from the closest node in ifacemin to P */
   int inodemin; /*!< node index (into NodeList)that is closest to P  */
   int inodeminlocal; /*!< node in FaceSet[ifacemin] that is closest to P , 
                  inodemin = FaceSet[ifacemin][inodeminlocal]*/
} SUMA_MT_INTERSECT_TRIANGLE;

/*! Structure defining the surface's volume parent info */
typedef struct {
   int isanat; /*!< 1 if parent volume is of type anat */
   int nx, ny, nz; /*!< number of voxels in the three dimensions */
   float dx, dy, dz; /*!< delta x, y, z in mm */
   float xorg, yorg, zorg; /*!< voxel origin in three dimensions */
   char *prefix; /*!< parent volume prefix */
   char *filecode; /*!< parent volume prefix + view */
   char *dirname; /*!< parent volume directory name */
   char *idcode_str; /*!< idcode string*/
   char *idcode_date; /*!< idcode date */
   int xxorient, yyorient, zzorient; /*!< orientation of three dimensions*/ 
   float *VOLREG_CENTER_OLD; /*!< pointer to the named attribute (3x1) in the .HEAD file of the experiment-aligned Parent Volume */
   float *VOLREG_CENTER_BASE; /*!< pointer to the named attribute (3x1) in the .HEAD file of the experiment-aligned Parent Volume */
   float *VOLREG_MATVEC; /*!< pointer to the named attribute (12x1) in the .HEAD file of the experiment-aligned Parent Volume */
} SUMA_VOLPAR;

#define SUMA_MAX_ROI_CTRL_NODES 100 /*!< Maximum number of control nodes in an ROI */
#define SUMA_MAX_ROI_CTRL_NODES3 300 
#define SUMA_MAX_ROI_ON_SURFACE 100 /*!< Maximum number of ROIs Drawn on a surface */
/*! structure to hold the drawing of an ROI */
typedef struct {   
   SUMA_ROI_DRAWING_TYPE Type;   /*!< The type of ROI drawn, that would be closed path, etc, etc, */

   char *idcode_str;    /*!< unique idcode for ROI */
   char *Parent_idcode_str; /*!< idcode of parent surface */
   char *Label; /*!< ascii label for ROI */ 
   
   float Pick0v[SUMA_MAX_ROI_CTRL_NODES3]; /*!< a vector of XYZ of picked points (equivalent of Pick0)------------ MAKE DO WITHOUT, SOON -------------*/
   float Pick1v[SUMA_MAX_ROI_CTRL_NODES3]; /*!< a vector of XYZ of picked points (equivalent of Pick1)------------ MAKE DO WITHOUT, SOON -------------*/
   
   int CtrlNodev[SUMA_MAX_ROI_CTRL_NODES]; /*!< indices of control nodes */
   int N_CtrlNodev; /*!< number of control nodes */
   
   SUMA_ROI_DRAWING_STATUS DrawStatus; /*!< Status of the ROI being drawn, finished, being drawn, being edited, etc. */

} SUMA_DRAWN_ROI;

/*! structure to hold an ROI */
typedef struct { 
   SUMA_ROI_TYPE Type;   /*!< The type of ROI */
   
   char *idcode_str;    /*!< unique idcode for ROI */
   char *Parent_idcode_str; /*!< idcode of parent surface */
   char *Label; /*!< ascii label for ROI */

   int *ElInd; /*!< pointer to vector containing indices into the parent surface (SO has Parent_idcode_str) of ROI elements.
                           If Type is SUMA_ROI_NodeGroup then ElementIndex contains indices to SO->NodeList .
                           If Type is SUMA_ROI_FaceGroup then ElementIndex contains indices to SO->FaceList.
                           If Type is SUMA_ROI_EdgeGroup then ElementIndex contains indices to SO->EL->EL. */
   int N_ElInd; /*!< Number of elements in ElementIndex */ 
} SUMA_ROI;



/*! structure defining a Surface Object */
typedef struct {
   SUMA_SO_File_Type FileType; /*!< Type of Surface file */
   SUMA_SO_File_Format FileFormat; /*!< Format of Surface file ascii or binary*/
   
   SUMA_FileName Name; /*!< Directory and Name of surface object file (SO) */
   SUMA_FileName Name_coord; /*!< Directory and Name of surface coordinate file (for SureFit files) */
   SUMA_FileName Name_topo; /*!< Directory and Name of surface topology file  (for SureFit files)*/
   char *idcode_str; /*!< string containing the idcode of the surface */
   char *Label; /*!< string containing a label for the surface. Used for window titles and saved image names */
   
   SUMA_Boolean SUMA_VolPar_Aligned; /*!< Surface aligned to Parent Volume data sets ?*/
   SUMA_Boolean VOLREG_APPLIED; /*!< YUP if VP->VOLREG_CENTER_BASE, VP->VOLREG_CENTER_OLD, VP->VOLREG_MATVEC were successfully applied*/
   SUMA_Boolean SentToAfni; /*!< YUP if the surface has been niml-sent to AFNI */

   SUMA_RENDER_MODES PolyMode; /*!< polygon viewing mode, SRM_Fill, SRM_Line, SRM_Points */
   
   int N_Node; /*!< Number of nodes in the SO */
   int NodeDim; /*!< Dimension of Node coordinates 3 for 3D only 3 is used for now, with flat surfaces having z = 0*/
   int EmbedDim; /*!< Embedding dimension of the surface, 2 for flat surfaces 3 for ones with non zero curvature other. */ 
   float *NodeList; /*!< N_Node x 3 vector containing the XYZ node coordinates. 
                        If NodeDim is 2 then the third column is all zeros
                        Prior to SUMA  1.2 this used to be a 2D matrix (a vector of vectors) */
   char *MapRef_idcode_str; /*!< if NULL, then it is not known whether surface is mappable or not
                                 if equal to idcode_str then surface surface is Mappable, 
                                 otherwise it specifies the idcode of the Mapping reference surface */
   
   int N_FaceSet; /*!< Number of polygons defining the surface  */
   int FaceSetDim; /*!< Number of sides on the polygon */
   int *FaceSetList; /*!< N_FaceSetList x FaceSetDim vector describing the polygon set that makes up the SO.
                     Each row contains the indices (into NodeList) of the nodes that make up a polygon 
                     Prior to SUMA  1.2 this used to be a 2D matrix (a vector of vectors) */
   
   float *NodeNormList ; /*!< N_Node x 3 vector (used to be matrix prior to SUMA 1.2) containing normalized normal vectors for each node*/
   float *FaceNormList ; /*!< N_FaceSet x 3 vector (used to be matrix prior to SUMA 1.2) containing normalized normal vectors for each polygon*/ 
   
   float Center[3];       /*!< The centroid of the surface */
   float MaxDims[3];      /*!< The maximum along each of the XYZ dimensions */
   float MinDims[3];      /*!< The minimum along each of the XYZ dimensions */
   float aMinDims;      /*!< The maximum across all dimensions*/
   float aMaxDims;      /*!< The minimum across all dimensions*/
   
   int RotationWeight; /*!< Contribution to center of rotation calculation. 
                           set to 0 if not contributing.
                            set to N_Node to have the number of nodes weigh into the center's location, center of mass effect
                           set to 1 to give each object equal weight */
   int ViewCenterWeight; /*!< Contribution to center of gaze and viewfrom location */
   
   char *Name_NodeParent; /*!< Node parent of the SO.   Node Indices of SO are into NodeList matrix of the NodeParent SO*/               

   GLfloat *glar_NodeList;         /*!< pointer to the 1D NodeList array - DO NOT FREE IT, it is a pointer copy of NodeList*/
   GLint  *glar_FaceSetList;      /*!< pointer to the 1D FaceSetList array - DO NOT FREE IT, it is a pointer copy of FaceSetList*/
   GLfloat *glar_FaceNormList;    /*!< pointer to the 1D FaceNormList array - DO NOT FREE IT, it is a pointer copy of NodeNormList*/
   #if 0
   /* This pointer is now a part of the surface viewer structure. Wed Nov  6 10:23:05 EST 2002
   Node color assignment is not a property of the surface alone, it also depends on the settings of the viewer. */
   GLfloat *glar_ColorList;       /*!< pointer to the 1D ColorList array*/
   #endif
   GLfloat *glar_NodeNormList;    /*!< pointer to the 1D NodeNormList array - DO NOT FREE IT, it is a pointer copy of NodeNormList*/
   
   SUMA_Boolean ShowMeshAxis; /*!< flag to show Mesh Axis if it is created */
   SUMA_Axis *MeshAxis;   /*!< pointer to XYZ axis centered on the surface's centroid */
   
   SUMA_MEMBER_FACE_SETS *MF; /*!< structure containing the facesets containing each node */
   SUMA_NODE_FIRST_NEIGHB *FN; /*!< structure containing the first order neighbors of each node */
   SUMA_INODE *FN_Inode; /*!< Inode structure for FN */
   SUMA_EDGE_LIST *EL; /*!< structure containing the edge list */
   SUMA_INODE *EL_Inode; /*!< Inode structure for EL */
   
   float *PolyArea; /*!< N_FaceSet x 1 vector containing the area of each polygon in FaceSetList */
   SUMA_SURFACE_CURVATURE *SC; /*!< Structure containing the surface curvature info */
   
   float *Cx; /*!< vector containing surface convexity at each node */
   SUMA_INODE *Cx_Inode; /*!< Inode structure for Cx */
   
   /* selection stuff */
   SUMA_Boolean ShowSelectedNode; /*!< flag for an obvious reason */
   int SelectedNode; /*!< index of one selected node, -1 if no node is selected */
   SUMA_SphereMarker *NodeMarker; /*!< Node Marker object structure*/
   
   SUMA_Boolean ShowSelectedFaceSet; /*!< you know what I mean */
   int SelectedFaceSet; /*!< index of one selected faceset, -1 if no faceset is selected */
   SUMA_FaceSetMarker *FaceSetMarker; /*!< Aha, I hear ya */
   
   SUMA_VOLPAR *VolPar; /*!< Parent Volume structure */
   
   char *Group;   /*!< Group the surface belongs to, like Simpsons H. */
   char *State; /*!< State of SO (like inflated, bloated, exploded) */
   
   SUMA_OVERLAYS **Overlays; /*!< vector of pointers to color overlay structures */
   SUMA_INODE **Overlays_Inode; /*!< vector of pointers to Inodes corresponding to each Overlays struct */
   int N_Overlays; /*!< number of pointers to overlay structures */
   
   SUMA_X_SurfCont *SurfCont;/*!< pointer to structure containing surface controller widget structure */

}SUMA_SurfaceObject; /*!< \sa Alloc_SurfObject_Struct in SUMA_DOmanip.c
                     \sa SUMA_Free_Surface_Object in SUMA_Load_Surface_Object.c
                     \sa SUMA_Print_Surface_Object in SUMA_Load_Surface_Object.c
                     \sa SUMA_Load_Surface_Object in SUMA_Load_Surface_Object.c
               */      


/*! structure containing a mapping of one surface to another*/
typedef struct {
   float *NewNodeList; /*!< N_Node x 3 vector containing new mapping of node coordinates */
   int N_Node; /*!< Number of nodes in NodeList */
   float *NodeVal; 
   float *NodeCol;
   float *NodeDisp;
} SUMA_SO_map;

/*! structure containing SureFit Surface*/
typedef struct {
   /* coord files */
   char name_coord[SUMA_MAX_NAME_LENGTH];
   int N_Node; /*!< Number of nodes */
   float *NodeList; /*!< N_Node x 3 vector containing node coordinates */
   int *NodeId; /*!< Node ID, that's normaly from 0..N_Nodes-1 but since it's in .coord file, I keep it anyway */
   char encoding_coord[100];
   char configuration_id[100];
   char coordframe_id[100];
   /* Topo files */
   char name_topo[SUMA_MAX_NAME_LENGTH];
   char encoding_topo[100];
   char date[100];
   char perimeter_id[100];
   int N_Node_Specs; /*!< Number of nodes with listed node specs */
   int **Specs_mat; /*!< Node Specs matrix. Columns appear to be arraged as such NodeId #Neighbors ? ? NodeId ? */
   SUMA_NODE_FIRST_NEIGHB FN; /*!< First order neighbor structure */
   int N_FaceSet; /*!< Number of polygons making up surface */
   int *FaceSetList; /*!< definition of polygons. Became a vector in SUMA 1.2*/
   /* Param Files */
   char name_param[SUMA_MAX_NAME_LENGTH];
   float AC_WholeVolume[3]; /*!< XYZ (from .Orient.params file) of Anterior Comissure of whole volume */
   float AC[3]; /*!< XYZ of Anterior Comissure of cropped volume */
} SUMA_SureFit_struct;

/* structure containing FreeSurfer Surface */
typedef struct {
   char name[SUMA_MAX_NAME_LENGTH];
   int N_Node; /*!< Number of nodes */
   int *NodeId; /*!< Node ID, that's normaly from 0..N_Nodes-1 unless the surface is a patch of another surface see FaceSetIndexInParent*/
   float *NodeList; /*!< N_Node x 3 vector containing node coordinates */
   int N_FaceSet; /*!< Number of polygons making up surface */
   int *FaceSetList; /*!< definition of polygons. For a complete surface, these are indices into NodeList's rows
                           For a patch, these are indices into NodeList of the parent surface.
                        Became a vector in SUMA 1.2*/
   char comment[SUMA_MAX_STRING_LENGTH]; /*!< comment at beginning of patch or surface */
   SUMA_Boolean isPatch; /*!< is the surface a patch of another ? */
   int *FaceSetIndexInParent; /*!< for a FaceSet in patch, this represents its index in FaceSetList of the parent surface.
                                    This is the FaceSet equivalent of NodeId*/ 
} SUMA_FreeSurfer_struct;

/* structure containing SureFit name*/
typedef struct {
   char name_coord[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
   char name_topo[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH]; 
   char name_param[SUMA_MAX_DIR_LENGTH+SUMA_MAX_NAME_LENGTH];
} SUMA_SFname;

typedef enum {    SMT_Nothing, 
                  SMT_Notice, SMT_Warning, SMT_Error, SMT_Critical,  
                  SMT_N }  SUMA_MESSAGE_TYPES; /*!< different types of messages */

typedef enum {    SMA_Nothing, 
                  SMA_Log, SMA_LogAndPopup,  
                  SMA_N }  SUMA_MESSAGE_ACTION; /*!< different actions to perform with messages */

 
                                 
/*! structure containing a SUMA Message structure */
typedef struct {
   SUMA_MESSAGE_TYPES Type;   /*!< type of message */
   SUMA_MESSAGE_ACTION  Action; /*!< what to do with message*/
   char *Message; /*!< null terminated message string */
   char *Source;  /*!< source of message, usually calling function */
}  SUMA_MessageData;

/*! structure containing information global to all surface viewers */
typedef struct {
   char AfniHostName[SUMA_MAX_NAME_LENGTH]; /*!< name or ipaddress of afni host maximum allowed name is 20 chars less than allocated for, see SUMA_Assign_AfniHostName*/ 
   char NimlAfniStream[SUMA_MAX_NAME_LENGTH]; /*!< niml stream name for communicating with afni */
   SUMA_Boolean Dev; /*!< Flag for developer option (allows the use of confusing or kludge options) */
   SUMA_Boolean InOut_Notify; /*!< prints to STDERR a notice when a function is entered or exited */ 
   int InOut_Level; /*!< level of nested function calls */
   
   int N_OpenSV; /*!< Number of open (visible) surface viewers.
                     Do not confuse this with the number of surface viewers
                     created (SUMAg_N_SVv)*/
   
   SUMA_MEMTRACE_STRUCT *Mem; /*!< structure used to keep track of memory usage */
   SUMA_Boolean MemTrace; /*!< Flag for keeping track of memory usage (must also set SUMA_MEMTRACE_FLAG ) */

   NI_stream ns; /*!< Stream used to communicate with AFNI. 
                     It is null when no communication stream is open. 
                     The stream can be open with Connected set to NOPE.
                     In that case no communication between the two programs
                     but resuming communication is easy since surfaces need
                     not be sent to AFNI again as would be the case if the stream 
                     was completely closed */
   SUMA_Boolean Connected; /*!< YUP/NOPE, if SUMA is sending (or accepting) communication from AFNI */
   SUMA_LINK_TYPES Locked[SUMA_MAX_SURF_VIEWERS]; /*!< All viewers i such that Locked[i] != SUMA_No_Lock have their cross hair locked together */   
   SUMA_Boolean ViewLocked[SUMA_MAX_SURF_VIEWERS]; /*!< All viewers i such that ViewLocked[i] = YUP have their view point locked together */    
   SUMA_Boolean SwapButtons_1_3; /*!< YUP/NOPE, if functions of mouse buttons 1 and 3 are swapped */
   SUMA_X_AllView *X; /*!< structure containing widgets and other X related variables that are common to all viewers */ 
   DList *MessageList; /*!< a doubly linked list with data elements containing notices, warnings and error messages*/
} SUMA_CommonFields;

/*! structure containing a surface patch */
typedef struct {
   int N_FaceSet; /*!< Number of Facesets forming patch */
   int *FaceSetList; /*!< vector (was a matrix prior to SUMA 1.2) (N_FaceSet x 3) containing indices of nodes forming triangles making up the patch */
   int *FaceSetIndex; /*!< vector (N_FaceSet x 1) containing indices of triangles in FaceSetList in the FaceSetList of the surface that the patch was taken from */
} SUMA_PATCH;

/*! structure containing ClientData 
This remains to be used somewhere ... */
typedef struct {
   SUMA_SurfaceViewer *sv; /*!< pointer to surface viewer from which the callback was made */
   int svi; /*!< index of sv into SUMAg_SVv */
}SUMA_CLIENT_DATA;

/*! Maximum nuber of branches that can be found in the intersection 
   of a plane with the surface model */
#define SUMA_BRANCHMAX 500   

/*! Maximum nuber of nodes that can form one branch */
#define SUMA_NODEINLISTMAX 500

/*!
\brief Structure forming a branch 

A branch is a protruded part of a tree often resulting in chainsaw accidents.
It is used in determining the intersection of a plane with a surface
*/
typedef struct {
   int begin, start;   /*!< first node of the branch */
   int last;   /*!< last node of the branch */
   int closed; /*!< flag. 0--> open, 1--> closed */
   int list[SUMA_NODEINLISTMAX]; /*!< list of nodes per branch */
   int listsz; /*!< Number of nodes in list*/
} SUMA_BRANCH;

/*!
\brief Structure forming a triangle branch 

A Tiangle branch represents a strip of connected triangles.

*/
typedef struct {
   int begin, start;   /*!< first node of the branch */
   int last;   /*!< last node of the branch */
   int iBranch; /*!< index identifying branch */
   SUMA_Boolean closed; /*!< flag. 0--> open, 1--> closed */
   int * list; /*!< list of nodes per branch */
   int N_list; /*!< Number of nodes in list*/
} SUMA_TRI_BRANCH;

/*!
\brief Structure defining the intersection of a surface with a plane 
*/
typedef struct {
   int N_IntersEdges; /*!< Number of edges intersected by the plane */
   int *IntersEdges;  /*!< Vector containing indices of edges intersected by the plane. The indices
                        are into SO->EL->EL matrix. The space allocate for this vector is SO->EL->N_EL
                        But that is way more than ususally needed. For this vector and others in
                        this structure, reallocation is not done to save time. Useful values for IntersEdges
                        are between IntersEdges[0]  and IntersEdges[N_IntersEdges-1]*/
   SUMA_Boolean *isEdgeInters; /*!< Vector specifying if an edge i (index into SO->EL->EL) was intersected. */
   #if 0
   /* old way, less memory usage, slower access - pre Wed Dec  4 16:57:03 EST 2002*/
   float *IntersNodes;  /*!< Vector containing XYZ coordinates of the intersection point on each 
                           intersected segment. Allocated space is for 3*SO->EL->N_EL, useful space is 
                           between IntersNodes[0] and IntersNodes[3*(N_IntersEdges-1) + 2]. Intersection point 
                           of edge IntersEdges[k] has  X = IntersNodes[3*k], Y = IntersNodes[3*k+1] and
                           Z = IntersNodes[3*k+2] */
   #endif
   float *IntersNodes;  /*!< Vector containing XYZ coordinates of the intersection point on each 
                           intersected segment. Allocated space is for 3*SO->EL->N_EL, meaningful values are
                           for intersected segments only. Intersection point 
                           of edge SO->EL->EL[k][:] has  X = IntersNodes[3*k], Y = IntersNodes[3*k+1] and
                           Z = IntersNodes[3*k+2] */
   int *IntersTri; /*!< Vector containing indices of triangles intersected by the plane (i.e. containing
                        and edge that was intersected. Allocation is done for SO->N_FaceSet. 
                        But meaningful values are between IntersETri[0] and IntersTri[N_IntersTri-1]. */
   int N_IntersTri; /*!< Number of intersected triangles. */
   SUMA_Boolean *isNodeInMesh;   /*!< Vector of SO->N_Node elements indicating whether a certain node
                                    belongs to an intersected seqment or not */
   int N_NodesInMesh;  /*!< Total number of nodes belonging to intersected segments */
   
   SUMA_Boolean *isTriHit; /*!< Vector of SO->N_FaceSet elements indicating whether a triangle was intersected by the plane.
                     if isTriHit[j] then triangle SO->FaceSetList[3*j], [3*j+1], [3*j+2] was intersected. You should
                     have a total of N_IntersTri YUP values in this vector*/
                        
} SUMA_SURF_PLANE_INTERSECT;

/*! Structure to contain the path between one node and the next. The path is defined in terms of the previous one, plus an edge from
the previous to the current */
typedef struct {
   int node; /*!< Index of current node*/ 
   float le;   /*!< Length of edge between current node and previous one. 0 for starting node. */ 
   int order; /*!< Path order to node. A path order of i means i segments are needed to reach node from the starting node. 0 for starting node*/
   void *Previous; /*!< pointer to path leading up to the previous node. NULL for starting node. This pointer is to be typecast to SUMA_DIJKSTRA_PATH_CHAIN **/
} SUMA_DIJKSTRA_PATH_CHAIN;

#endif
